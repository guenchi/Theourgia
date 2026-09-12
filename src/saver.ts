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
import * as path from 'path';
import { Client } from './client';
import { Outbox, OutboxEntry } from './outbox';
import { ImportTarget } from './sessions';
import { TransportError } from './transport';
import { eventFromWrite, firstCursorFromCheck, isReplay, isWellFormedCursor } from './cursor';
import { Datum, clauseValue, formatCursor, headName, isSym } from './wire';

/*
 * WHAT THE CORE SAID ON THE OTHER STREAM, short enough to put in a
 * sentence. Empty when it said nothing, so the sentence reads normally
 * in the ordinary case.
 */
export function aside(stderr: string): string {
  const text = stderr.trim();
  if (text.length === 0) {
    return '';
  }
  const first = text.split('\n').slice(0, 3).join(' / ');
  return `. It said: ${first.length > 300 ? `${first.slice(0, 300)}...` : first}`;
}

export type SaveStatus = 'saved' | 'replayed' | 'pending' | 'refused' | 'blocked';

/*
 * What to do with an entry the store has answered. It is handed in
 * rather than assumed, so that the one place which decides the order --
 * record first, remove second -- is the one the caller names.
 *
 * ⚠️ IT MAKES THE CHOICE EXPLICIT; IT DOES NOT ENFORCE THE ORDER. Any
 * caller can pass `(req, cursor) => outbox.resolve(req, cursor)` and
 * remove the entry with nothing recorded -- the cells in this file do
 * exactly that on purpose, because what they are about is the sending
 * and not the recording. Having no default means nobody gets that
 * behaviour without writing it down; a guarantee that the record is
 * always written first would have to live somewhere this signature
 * cannot reach.
 */
export type Settle = (req: string, cursor: string | null) => void;

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
  /*
   * A USAGE LINE IS THE CORE SAYING IT DID NOT UNDERSTAND THE REQUEST,
   * and it is the answer a core that predates request tracking gives to
   * a tracked write: `(usage (set <id> <field> <value>))`, with no
   * mention of --req or --cursor. Rendering it through the branch below
   * produced "the core refused the write: refused", which says nothing
   * at all -- and the thing it was failing to say is that the core is
   * the wrong version, which takes a while to work out by hand.
   */
  if (headName(datum) === 'usage') {
    return (
      'the core rejected this request with its usage line. Check the version at ' +
      'theourgia.corePath and the arguments sent: a core without request tracking answers ' +
      'this way to a save carrying --req and --cursor, and so does a core that got an ' +
      'argument it did not expect'
    );
  }
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

/*
 * One chain per outbox file, for the whole process. Keyed by the
 * resolved path so that two spellings of one file do not get two chains.
 */
const queues = new Map<string, Promise<void>>();

export class Saver {
  private readonly client: Client;
  private readonly outbox: Outbox;
  private readonly settle: Settle;
  private readonly newRequestId: () => string;
  private readonly now: () => number;
  private bootstrapProblem: string | null = null;

  constructor(client: Client, outbox: Outbox, settle: Settle, options: SaverOptions = {}) {
    this.client = client;
    this.outbox = outbox;
    this.settle = settle;
    this.newRequestId = options.newRequestId ?? (() => randomUUID());
    this.now = options.now ?? (() => Date.now());
  }

  public get pendingCount(): number | null {
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
        lastError: null,
        importedBy: null
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
   * ⚠️ A TAKEOVER'S ENTRIES ARRIVE THROUGH THE SAME LOCK AS A SAVE.
   *
   * The import used to be handed a bare `Outbox` and told the comment
   * "this is held by the Saver's serial chain". It was not: a save in
   * flight answers, writes ITS copy of the queue back, and the imported
   * entries are gone -- while the source is already marked as having
   * handed them over, so no later takeover offers them again. The bytes
   * survive in the dead window's file and automatic recovery stops
   * seeing them. Reproduced in review.
   *
   * Going through `serialise` also reloads this copy from disk first,
   * which is the other half: importing into a stale copy writes the
   * stale one back.
   *
   * IT IS THE SAME `serialise` THAT `save` AND `retry` USE, keyed by the
   * queue's path, so an import and a save over one file wait for each
   * other whichever objects they were reached through.
   */
  public adopt<T>(work: (into: ImportTarget) => T): Promise<T> {
    return this.serialise(async () =>
      work({
        has: (req: string) => this.outbox.find(req) !== undefined,
        adopt: (entry: OutboxEntry) => this.outbox.enqueue(entry)
      })
    );
  }

  /*
   * THE SERIALISATION BELONGS TO THE QUEUE, NOT TO THIS OBJECT. A chain
   * held in an instance field serialises that instance and nothing else,
   * and this instance is not the only one: the extension builds a new
   * Saver over the SAME outbox file every time a setting changes, and
   * the old one may still have a request in flight. Two Savers, one
   * file, two requests on the wire -- and the second carries a cursor
   * the first is about to move.
   *
   * So the chain is looked up by the outbox's path. Every Saver over one
   * file waits behind the same promise, whatever object made it.
   */
  private serialise<T>(work: () => Promise<T>): Promise<T> {
    const key = path.resolve(this.outbox.path);
    const before = queues.get(key) ?? Promise.resolve();
    /*
     * THE FILE IS THE QUEUE; THIS OBJECT IS A COPY OF IT. Waiting for
     * the previous holder of the lock is not enough -- what that holder
     * wrote is on disk, and this Saver's Outbox still holds whatever it
     * read when it was built. The extension makes a new Saver, over a
     * new Outbox, on every settings change, and the old one may add a
     * pending entry after the new one loaded: writing this copy back
     * then ERASES it, which is the one thing the outbox exists to stop.
     *
     * So the copy is refreshed from the file after taking the lock and
     * before doing anything with it.
     */
    const reloaded = () => {
      this.outbox.load();
      return work();
    };
    const next = before.then(reloaded, reloaded);
    queues.set(
      key,
      next.then(
        () => undefined,
        () => undefined
      )
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
    /*
     * A CURSOR THE CORE WOULD NOT PARSE IS NOT SENT. `--cursor` is
     * parsed by shape and anything else is `malformed-cursor`; composing
     * one out of a writer name with a colon in it would produce a save
     * refused for a reason that has nothing to do with the save.
     */
    if (!isWellFormedCursor(first.cursor)) {
      this.bootstrapProblem = `this store reports a writer this client cannot spell a cursor for: ${first.cursor}`;
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
      /*
       * THE ENTRY IS MARKED AS GOING OUT BEFORE IT GOES OUT, for the
       * same reason it was written down before it was sent: after this
       * line the store may have seen it, and nothing may change what it
       * says.
       */
      this.outbox.aboutToSend(entry.req, this.outbox.cursor);
      const current = this.outbox.find(entry.req) ?? entry;
      const outcome = await this.send(current);
      /*
       * ⚠️ AND IT STOPS IF THE ENTRY IS STILL HERE.
       *
       * This loop had exactly one way out: an answer of `pending`. It
       * took `entries[0]`, sent it, and went round again on the
       * assumption that the entry was gone by then -- and NOTHING
       * CHECKED THAT IT WENT. The settler is allowed to decline: when
       * the record beside the file cannot be written, `recordAnswer`
       * KEEPS the entry so the request stays retryable rather than being
       * lost, which is the safe direction and is deliberate. But then
       * the store has answered, the outcome is not `pending`, and the
       * same entry is at the front of the queue again -- so the same
       * request goes to the store on every turn, for ever, inside a
       * command the user is awaiting.
       *
       * MEASURED, NOT SUPPOSED: with a settler that records nothing, a
       * single `save` sent the same request five times and stopped only
       * because the scripted core ran out of `ok` answers. A real store
       * does not run out.
       *
       * THE ENTRY IS MARKED PENDING AND THE OUTCOME SAYS SO, because
       * "saved" would be a report that the work is done about a queue
       * that still holds it -- the count in the status bar and the
       * sentence after a retry would disagree with each other.
       */
      outcomes.push(outcome);
      /*
       * AN ANSWER OF `pending` IS ALREADY A REASON TO STOP, AND IT HAS
       * ITS OWN SENTENCE. `send` keeps the entry in that case too, so
       * the check below would be true here as well -- and would replace
       * a message naming what went wrong ("the core exited 255 without
       * saying why") with a general one. Three cells caught that.
       */
      if (outcome.status === 'pending') {
        return outcomes;
      }
      if (this.outbox.find(entry.req) !== undefined) {
        const why =
          'the store answered and the answer could not be recorded beside the file; the request ' +
          'is kept';
        this.outbox.markPending(entry.req, why);
        outcomes[outcomes.length - 1] = { ...outcome, status: 'pending', message: why };
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
        const why = aside(e.detail);
        this.outbox.markPending(entry.req, `${e.message}${why}`);
        return {
          status: 'pending',
          req: entry.req,
          id: entry.id,
          message: `${e.message}; the save is kept and can be retried${why}`,
          answer: null
        };
      }
      throw e;
    }

    const datum = answer.answers.length > 0 ? answer.answers[0] : null;

    if (answer.ok) {
      const event = eventFromWrite(answer);
      /*
       * AN `ok` THAT NAMES NO RECORD IS NOT A SAVE THIS CLIENT CAN ACT
       * ON. Every write the core accepts answers with the event it
       * appended -- `(cursor ("w" . 6))` fresh, `(event ("w" . 6))` on a
       * replay -- and an answer with neither leaves the next request
       * with no cursor to be composed against. Dropping the entry would
       * be calling it saved on the strength of the word `ok`, and the
       * next save would then go out against a cursor from before this
       * one, which the store refuses. So the entry stays and this is
       * reported as the defect it is.
       */
      if (event === null) {
        this.outbox.markPending(entry.req, 'the core answered ok without naming a record');
        return {
          status: 'pending',
          req: entry.req,
          id: entry.id,
          message:
            'the core accepted the save but did not say which record it wrote, so this client ' +
            'cannot carry the cursor forward; the save is kept and can be retried',
          answer: datum
        };
      }
      const moved = formatCursor(event);
      if (!isWellFormedCursor(moved)) {
        this.outbox.markPending(entry.req, `the core named a record this client cannot spell: ${moved}`);
        return {
          status: 'pending',
          req: entry.req,
          id: entry.id,
          message: `the core named the record ${moved}, which is not a cursor the core itself would parse`,
          answer: datum
        };
      }
      this.settle(entry.req, moved);
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
      /*
       * THE OTHER STREAM IS THE ONLY EVIDENCE THERE IS. A core that
       * exits without printing an answer has usually said why on stderr,
       * and a message that drops it leaves whoever reads the report with
       * an exit code and nothing to act on. This happened once in a full
       * run and could not be reproduced; the reason it could not be
       * looked into afterwards is that this sentence did not carry it.
       */
      const why = aside(answer.stderr);
      this.outbox.markPending(entry.req, `the core exited ${answer.rc} without saying why${why}`);
      return {
        status: 'pending',
        req: entry.req,
        id: entry.id,
        message:
          `the core exited ${answer.rc} without an answer; the save is kept and can be retried${why}`,
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

    this.settle(entry.req, null);
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
