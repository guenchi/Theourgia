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
 * Where the store stood when this client decided to send.
 *
 * A CURSOR NAMES A WRITER AND A SEQUENCE, AND THE WRITER IS THE STORE'S,
 * NOT OURS. `--actor` is a name recorded in a record; the writer is the
 * append target this copy of the store owns, and the same actor working
 * in two stores writes under two different writers. Deriving the cursor
 * from the actor would produce a name the store has never heard of.
 *
 * AFTER THE FIRST ANSWER THE CURSOR COMES FROM THE ANSWER. A write that
 * landed answers `(cursor ("w" . 6))` and a replay answers `(event ("w"
 * . 6))`; both name the record the store is standing behind, and either
 * is the next request's cursor.
 *
 * BEFORE THE FIRST ANSWER THERE IS NOTHING TO CARRY FORWARD, so it is
 * asked for: `check` reports every writer with the sequence it ends at.
 * What `check` does NOT report is which of them is local -- that is
 * decided by an owner file inside the store, which a client has no
 * business reading. With one writer there is no question; with more than
 * one there is, and this refuses rather than picking. A store with
 * several writers is one that was adopted or copied, and guessing wrong
 * produces `cursor-unreachable` at best and a cursor that means someone
 * else's history at worst.
 */

import { Answer } from './client';
import {
  Datum,
  Event,
  answerOf,
  asInteger,
  formatCursor,
  isList,
  readEvent,
  recordValue
} from './wire';

export interface WriterEnd {
  writer: string;
  end: number;
}

export function writersFromCheck(answer: Datum): WriterEnd[] | null {
  /*
   * ⛔ AND THE FORM HAS TO BE A `check`. `(garbage (writers (("local"
   * (end 7)))))` answered a cursor, and `(error ...)` carrying the same
   * clause did too -- so a refusal shaped like an answer would have this
   * client choose a writer and start writing. Measured in a twelfth
   * review round. The caller checks the exit code and that there is an
   * answer at all; what the answer IS belongs here.
   */
  const check = answerOf(answer, 'check');
  if (check === null) {
    return null;
  }
  /*
   * ⛔ EXACTLY ONE VALUE. `(check (writers ((...)) ((...))))` carries
   * two, and taking the first silently made a malformed listing supply
   * a cursor -- measured in a fourteenth review round, which got
   * `local:7` out of a clause naming two separate listings. A clause
   * with more values than this build knows how to read is a `check`
   * this build cannot account for, and the refusal it already has for
   * an unreadable listing is the right answer.
   */
  /*
   * ⚠️ BOTH REASONS REFUSE HERE. A `check` with no `writers` clause
   * and a `check` carrying two are equally unusable for the one thing
   * this reader does, which is to name the single log writer.
   */
  const writers = check.clause('writers');
  if (!writers.read || writers.items.length !== 1 || !isList(writers.items[0])) {
    return null;
  }
  /*
   * ⛔ AN ENTRY THAT CANNOT BE READ IS NOT AN ENTRY THAT IS NOT THERE,
   * and here the count is the whole decision.
   *
   * Skipping the unreadable ones made `(check (writers (("local" (end 7))
   * ("other" (end "bad")))))` count ONE writer, and one writer is what
   * lets this client pick a cursor and write. The refusal that stands
   * behind a multi-writer store -- the core does not say which writer is
   * local, so do not guess -- was defeated by an entry nobody could
   * parse. Measured in an eleventh review round. An unreadable listing
   * refuses, and the caller reports that it could not be read.
   */
  const out: WriterEnd[] = [];
  for (const entry of writers.items[0]) {
    if (!isList(entry) || entry.length < 1 || typeof entry[0] !== 'string') {
      return null;
    }
    /*
     * ⚠️ A WRITER ENTRY HAS NO HEAD EITHER -- it reads
     * `("local" (end 7))`, whose first element is the writer's name. It
     * comes out of a form whose head has been checked, which is what
     * makes reading it safe. See `clauseOfRecord` in wire.ts.
     */
    const end = asInteger(recordValue(entry, 'end'));
    if (end === null) {
      return null;
    }
    out.push({ writer: entry[0], end });
  }
  return out;
}

export type FirstCursor =
  | { ok: true; cursor: string }
  | { ok: false; reason: 'no-writer' | 'many-writers' | 'unreadable'; writers: WriterEnd[] };

export function firstCursorFromCheck(answer: Datum): FirstCursor {
  const writers = writersFromCheck(answer);
  if (writers === null) {
    return { ok: false, reason: 'unreadable', writers: [] };
  }
  if (writers.length === 1) {
    return { ok: true, cursor: formatCursor({ writer: writers[0].writer, seq: writers[0].end }) };
  }
  if (writers.length === 0) {
    return { ok: false, reason: 'no-writer', writers };
  }
  return { ok: false, reason: 'many-writers', writers };
}

/*
 * THE EVENT IS LOOKED FOR UNDER BOTH NAMES, in the order the core uses
 * them: a fresh write carries `cursor`, and only a replay carries
 * `event`. A client that read `cursor` alone would stop advancing the
 * moment a retry was answered with a replay -- which is exactly the
 * moment it most needs to advance.
 */
export function eventFromWrite(answer: Answer): Event | null {
  if (!answer.ok || answer.answers.length === 0) {
    return null;
  }
  /*
   * ⛔ THE ENVELOPE IS UNWRAPPED ONCE, BY THE CLIENT, AND NOT AGAIN
   * HERE.
   *
   * This reached into an `items` clause by SHAPE, after `interpret` had
   * already removed the envelope the request asked for. Measured in an
   * eleventh review round with a commit answering
   * `(ok (items (ok (items (ok (cursor ("wrong" . 99)) (replay #t))) (cursor ("right" . 7)) (replay #f))))`:
   * this returned `wrong:99` -- a cursor from a form nested inside the
   * answer, chosen over the one the answer states. Whether an answer was
   * wrapped is a fact about the REQUEST, and the client is where that is
   * known.
   */
  /*
   * ⛔ AND THE FORM HAS TO HAVE SAID `ok`.
   *
   * This is the worst place in this extension for the unchecked-form
   * shape, and it was the last found. Measured in a twelfth review
   * round: a successful exit carrying
   * `(garbage (cursor ("w" . 7)) (replay #t))` gave a cursor, a Saver
   * settled the entry `confirmed`, and the request left the queue --
   * a save reported as landed, and its record removed, on the strength
   * of a form nobody can parse. The exit code was being trusted for a
   * question it cannot answer.
   */
  const form = answerOf(answer.answers[0], 'ok');
  if (form === null) {
    return null;
  }
  for (const name of ['cursor', 'event']) {
    const found = form.value(name);
    if (found !== undefined) {
      const event = readEvent(found);
      if (event !== null) {
        return event;
      }
    }
  }
  return null;
}

export function isReplay(answer: Answer): boolean {
  if (!answer.ok || answer.answers.length === 0) {
    return false;
  }
  /*
   * ⛔ AS ABOVE -- no second unwrapping. The same crafted answer made
   * this report a replay where the answer says it is not one, which
   * would have a save reported as "the store had already applied this
   * request" when the store had just applied it for the first time.
   */
  const form = answerOf(answer.answers[0], 'ok');
  return form !== null && form.value('replay') === true;
}

export const CURSOR_SHAPE = /^[^:\s]+:(0|[1-9][0-9]*)$/;

export function isWellFormedCursor(cursor: string): boolean {
  return CURSOR_SHAPE.test(cursor);
}

/*
 * WHOSE LOG A POSITION IS IN, AND WHERE IN IT. (section 13.3)
 *
 * ⚠️ TWO WRITERS' NUMBERS ARE NOT COMPARABLE. A cursor is `writer:n`,
 * and `n` counts within that writer's log. Reading a different
 * writer's position as "later" because its number is bigger is reading
 * two rulers as one.
 */
export function writerOf(cursor: string): string {
  const at = cursor.lastIndexOf(':');
  return at < 0 ? cursor : cursor.slice(0, at);
}

export function sameWriter(a: string, b: string): boolean {
  return writerOf(a) === writerOf(b);
}
