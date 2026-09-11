/*
 * Copyright 2018 - 2026 guenchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Saving, which is one `set` at a time and never two.
 *
 * THE ANSWERS ARE SORTED BY WHAT THEY SAY ABOUT THE STORE, not by
 * whether they were called errors. Three groups, and the middle one is
 * the reason this file exists:
 *
 *   the request ran      -- ok, or a replay of the same request. Drop
 *                           the entry and carry the cursor forward.
 *   nobody can say       -- `(error unknown <why>)`, a timeout, a core
 *                           that printed nothing. KEEP the entry. The
 *                           store may hold the record; asking again with
 *                           the same id is the only way to find out, and
 *                           dropping it here is how an edit disappears.
 *   the request did not  -- every other refusal. Drop the entry and say
 *                           so, because retrying cannot change the
 *                           answer and leaving it queued would suggest
 *                           it might.
 *
 * THE VERDICT IS THE EXIT CODE. A core that printed `(ok ...)` and then
 * exited non-zero did not finish; reading the head symbol would confirm
 * a save that the core itself refuses to call a success.
 *
 * ONE AT A TIME, AND THE HEAD OF THE QUEUE BLOCKS. A second save sent
 * before the first is answered would carry a cursor that the first is
 * about to move, and the store would refuse it -- so the queue is
 * strictly serial, and an entry nobody can resolve stops the ones behind
 * it rather than being stepped over.
 */

import { randomUUID } from 'crypto';
import { Client } from './client';
import { Outbox, OutboxEntry } from './outbox';
import { TransportError } from './transport';
import { eventFromWrite, firstCursorFromCheck, isReplay } from './cursor';
import { Datum, clauseValue, formatCursor, headName, isSym } from './wire';

export type SaveStatus = 'saved' | 'replayed' | 'pending' | 'refused' | 'blocked';

export interface SaveOutcome {
  status: SaveStatus;
  req: string;
  id: string;
  message: string;
  answer: Datum | null;
}

export interface SaverOptions {
  newRequestId?: () => string;
  now?: () => number;
}

/*
 * `unknown` IS THE ONLY REFUSAL THAT MEANS "ASK AGAIN". The core's
 * README is explicit that it never guesses: every other named refusal is
 * a determination, and `unknown` is the absence of one.
 */
function saysNobodyKnows(datum: Datum): boolean {
  return headName(datum) === 'error' && Array.isArray(datum) && datum.length >= 2 && isSym(datum[1], 'unknown');
}

function saysAnOperatorSettledIt(datum: Datum): boolean {
  return (
    headName(datum) === 'error' &&
    Array.isArray(datum) &&
    datum.length >= 2 &&
    isSym(datum[1], 'resolved-executed')
  );
}

function describeRefusal(datum: Datum): string {
  if (!Array.isArray(datum) || datum.length < 2) {
    return 'the core refused the write';
  }
  const name = isSym(datum[1]) ? (datum[1] as { name: string }).name : 'refused';
  const current = clauseValue(datum, 'current');
  if (name === 'changed' && current !== undefined) {
    return 'the block changed in the store since it was opened';
  }
  return `the core refused the write: ${name}`;
}

export class Saver {
  private readonly client: Client;
  private readonly outbox: Outbox;
  private readonly newRequestId: () => string;
  private readonly now: () => number;
  private running: Promise<void> = Promise.resolve();
  private bootstrapProblem: string | null = null;

  constructor(client: Client, outbox: Outbox, options: SaverOptions = {}) {
    this.client = client;
    this.outbox = outbox;
    this.newRequestId = options.newRequestId ?? (() => randomUUID());
    this.now = options.now ?? (() => Date.now());
  }

  public get pendingCount(): number {
    return this.outbox.pendingCount;
  }

  public get cursor(): string | null {
    return this.outbox.cursor;
  }

  public get blockedBecause(): string | null {
    return this.bootstrapProblem;
  }

  /*
   * THE ENTRY IS WRITTEN DOWN, THEN THE QUEUE RUNS. Both steps are
   * inside the same serialisation, so two saves arriving together are
   * enqueued in the order they arrived and sent in that order.
   */
  public save(id: string, field: string, payload: string): Promise<SaveOutcome> {
    return this.serialise(async () => {
      const cursor = await this.ensureCursor();
      if (cursor === null) {
        return {
          status: 'blocked' as const,
          req: '',
          id,
          message: this.bootstrapProblem ?? 'this store has no cursor to write against',
          answer: null
        };
      }
      const entry: OutboxEntry = {
        req: this.newRequestId(),
        cursor,
        id,
        field,
        payload,
        state: 'queued',
        createdAt: this.now(),
        lastError: null
      };
      this.outbox.enqueue(entry);
      const outcomes = await this.drain();
      const mine = outcomes.find((o) => o.req === entry.req);
      return (
        mine ?? {
          status: 'pending' as const,
          req: entry.req,
          id,
          message: 'the save is queued behind an earlier one whose outcome is unknown',
          answer: null
        }
      );
    });
  }

  public retry(): Promise<SaveOutcome[]> {
    return this.serialise(() => this.drain());
  }

  /*
   * ONE PROMISE CHAIN IS THE WHOLE SERIALISATION. A boolean flag plus a
   * queue of callbacks is the same thing with more places to get the
   * bookkeeping wrong, and the failure mode -- two `set` requests in
   * flight -- is one the store answers with a refusal that looks like a
   * bug in the store.
   */
  private serialise<T>(work: () => Promise<T>): Promise<T> {
    const next = this.running.then(work, work);
    this.running = next.then(
      () => undefined,
      () => undefined
    );
    return next;
  }

  private async ensureCursor(): Promise<string | null> {
    const known = this.outbox.cursor;
    if (known !== null) {
      return known;
    }
    const answer = await this.client.request('check', []);
    if (!answer.ok || answer.answers.length === 0) {
      this.bootstrapProblem = 'the store did not answer `check`, so this client has no cursor to write against';
      return null;
    }
    const first = firstCursorFromCheck(answer.answers[0]);
    if (!first.ok) {
      this.bootstrapProblem =
        first.reason === 'many-writers'
          ? `this store has ${first.writers.length} writers and the core does not yet say which is local; ` +
            'writing from here is not supported in this batch'
          : 'this store reports no writer, so there is nothing to write against';
      return null;
    }
    this.bootstrapProblem = null;
    this.outbox.setCursor(first.cursor);
    return first.cursor;
  }

  /*
   * The queue is walked from the front and stops at the first entry
   * whose outcome is unknown. Everything behind it was composed against
   * a cursor that entry is about to move.
   */
  private async drain(): Promise<SaveOutcome[]> {
    const outcomes: SaveOutcome[] = [];
    for (;;) {
      const entries = this.outbox.entries;
      if (entries.length === 0) {
        return outcomes;
      }
      const entry = entries[0];
      const cursor = this.outbox.cursor;
      if (cursor !== null) {
        this.outbox.retarget(entry.req, cursor);
      }
      const current = this.outbox.find(entry.req) ?? entry;
      const outcome = await this.send(current);
      outcomes.push(outcome);
      if (outcome.status === 'pending') {
        return outcomes;
      }
    }
  }

  private async send(entry: OutboxEntry): Promise<SaveOutcome> {
    const args = [
      entry.id,
      entry.field,
      entry.payload,
      '--req',
      entry.req,
      '--cursor',
      entry.cursor
    ];
    let answer;
    try {
      answer = await this.client.request('set', args);
    } catch (e) {
      if (e instanceof TransportError) {
        this.outbox.markPending(entry.req, e.message);
        return {
          status: 'pending',
          req: entry.req,
          id: entry.id,
          message: `${e.message}; the save is kept and can be retried`,
          answer: null
        };
      }
      throw e;
    }

    const datum = answer.answers.length > 0 ? answer.answers[0] : null;

    if (answer.ok) {
      const event = eventFromWrite(answer);
      this.outbox.resolve(entry.req, event === null ? null : formatCursor(event));
      return {
        status: isReplay(answer) ? 'replayed' : 'saved',
        req: entry.req,
        id: entry.id,
        message: isReplay(answer)
          ? 'the store had already applied this request'
          : 'saved',
        answer: datum
      };
    }

    if (datum === null) {
      this.outbox.markPending(entry.req, `the core exited ${answer.rc} without saying why`);
      return {
        status: 'pending',
        req: entry.req,
        id: entry.id,
        message: `the core exited ${answer.rc} without an answer; the save is kept and can be retried`,
        answer: null
      };
    }

    if (saysNobodyKnows(datum)) {
      this.outbox.markPending(entry.req, 'the store cannot say whether the request ran');
      return {
        status: 'pending',
        req: entry.req,
        id: entry.id,
        message: 'the store cannot say whether this save ran; it is kept and can be retried',
        answer: datum
      };
    }

    this.outbox.resolve(entry.req, null);
    if (saysAnOperatorSettledIt(datum)) {
      return {
        status: 'replayed',
        req: entry.req,
        id: entry.id,
        message: 'an operator recorded that this request had already been carried out',
        answer: datum
      };
    }
    return {
      status: 'refused',
      req: entry.req,
      id: entry.id,
      message: describeRefusal(datum),
      answer: datum
    };
  }
}
