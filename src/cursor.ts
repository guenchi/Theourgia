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

/*
 * A WRITER IN THE LISTING, WITH ITS END IF IT COULD BE READ. `end` is null
 * when the entry does not state one this build can read -- the core writes
 * `(end unreadable)` for a writer whose directory it could not read (F77),
 * and a shape nobody here knows is no better. The NAME is never optional:
 * an entry whose name cannot be read might be the local writer's.
 */
export interface ListedWriter {
  writer: string;
  end: number | null;
}

/*
 * THE LISTING, ENTRY BY ENTRY. Null when there is no listing to read at all:
 * not a `check`, no `writers` clause or two of them, or an entry with no
 * name. Which ends may be missing is the caller's question, not this one's.
 */
function listingFromCheck(answer: Datum): ListedWriter[] | null {
  /*
   * NEVER: AND THE FORM HAS TO BE A `check`. `(garbage (writers (("local"
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
   * NEVER: EXACTLY ONE VALUE. `(check (writers ((...)) ((...))))` carries
   * two, and taking the first silently made a malformed listing supply
   * a cursor -- measured in a fourteenth review round, which got
   * `local:7` out of a clause naming two separate listings. A clause
   * with more values than this build knows how to read is a `check`
   * this build cannot account for, and the refusal it already has for
   * an unreadable listing is the right answer.
   */
  /*
   * NOTE: BOTH REASONS REFUSE HERE. A `check` with no `writers` clause
   * and a `check` carrying two are equally unusable for the one thing
   * this reader does, which is to name the single log writer.
   */
  const writers = check.clause('writers');
  if (!writers.read || writers.items.length !== 1 || !isList(writers.items[0])) {
    return null;
  }
  const out: ListedWriter[] = [];
  for (const entry of writers.items[0]) {
    if (!isList(entry) || entry.length < 1 || typeof entry[0] !== 'string') {
      return null;
    }
    /*
     * NOTE: A WRITER ENTRY HAS NO HEAD EITHER -- it reads
     * `("local" (end 7))`, whose first element is the writer's name. It
     * comes out of a form whose head has been checked, which is what
     * makes reading it safe. See `clauseOfRecord` in wire.ts.
     */
    /*
     * NOTE: AND `end` IS ASKED FOR THROUGH THE COUNTED READER, so a writer
     * entry naming its end twice has no end this build can read, rather
     * than supplying the first of them -- and what that costs is decided
     * by the caller, as for `(end unreadable)`.
     */
    const stated = recordValue(entry, 'end');
    out.push({ writer: entry[0], end: stated.read ? asInteger(stated.value) : null });
  }
  return out;
}

export function writersFromCheck(answer: Datum): WriterEnd[] | null {
  const listing = listingFromCheck(answer);
  /*
   * NEVER: AN ENTRY THAT CANNOT BE READ IS NOT AN ENTRY THAT IS NOT THERE,
   * and where the count is the whole decision, one unreadable end refuses
   * the listing.
   *
   * Skipping the unreadable ones made `(check (writers (("local" (end 7))
   * ("other" (end "bad")))))` count ONE writer, and one writer is what
   * lets this client pick a cursor and write. The refusal that stands
   * behind a multi-writer store -- the core does not say which writer is
   * local, so do not guess -- was defeated by an entry nobody could
   * parse. Measured in an eleventh review round. An unreadable listing
   * refuses, and the caller reports that it could not be read.
   */
  if (listing === null || listing.some((w) => w.end === null)) {
    return null;
  }
  return listing as WriterEnd[];
}

export type FirstCursor =
  | { ok: true; cursor: string }
  | {
      ok: false;
      reason: 'no-writer' | 'many-writers' | 'unreadable' | 'local-writer-not-listed' | 'local-writer-twice';
      writers: ListedWriter[];
      local?: string;
      /*
       * WHICH PART COULD NOT BE READ, when the reason is `unreadable`. The
       * listing and the `local-writer` clause are two different failures,
       * and a sentence about one told of the other says something false:
       * review r1 of item 2 measured "how many writers it has is not
       * known" about a listing that read perfectly well.
       */
      unreadable?: 'listing' | 'local-writer' | 'local-end';
    };

export function firstCursorFromCheck(answer: Datum): FirstCursor {
  const writers = listingFromCheck(answer);
  if (writers === null) {
    return { ok: false, reason: 'unreadable', writers: [], unreadable: 'listing' };
  }
  /*
   * WHICH WRITER IS THIS STORE'S OWN, WHEN THE CORE SAYS. (queue item 2, the
   * core's F45)
   *
   * A `check` from a core that knows names it: `(local-writer "<id>")`,
   * a clause of its own beside `writers`, taken by name (its position is
   * not promised). When it is there, the cursor is that writer's end,
   * however many writers the store has -- which is what a store with a
   * second writer, adopted elsewhere and published back, was waiting for.
   *
   * NOTE: THREE REFUSALS, AND THEY STAY THREE. A `local-writer` clause
   * that is there twice, or is not a string, is an answer this build
   * cannot read -- the same refusal as an unreadable listing. One that
   * names a writer the listing does not hold is the answer contradicting
   * itself, and it is refused with that reason rather than by picking
   * either half: two suppliers of one fact disagreeing is not a thing to
   * guess about. (The core has a cell pinning that this cannot happen;
   * this one pins that nothing is guessed if it does anyway.) And the
   * clause being absent is the third case, below.
   */
  const check = answerOf(answer, 'check');
  const local = check === null ? null : check.value('local-writer');
  if (local !== null && !(!local.read && local.because === 'absent')) {
    if (!local.read || typeof local.value !== 'string') {
      return { ok: false, reason: 'unreadable', writers, unreadable: 'local-writer' };
    }
    const named = writers.filter((w) => w.writer === local.value);
    if (named.length === 0) {
      return { ok: false, reason: 'local-writer-not-listed', writers, local: local.value };
    }
    /*
     * NEVER: THE LOCAL WRITER LISTED TWICE. Two entries for one writer are
     * two answers to "where does it end", and taking the first is choosing
     * between them (review r1 of item 15: `(("w1" (end 7)) ("w1" (end
     * unreadable)))` gave `w1:7`, where the whole-listing rule before this
     * item refused). Refused, whatever the second entry says.
     */
    if (named.length > 1) {
      return { ok: false, reason: 'local-writer-twice', writers, local: local.value };
    }
    const own = named[0];
    /*
     * KEY: WHEN THE CORE NAMES THE LOCAL WRITER, ONLY THAT WRITER'S END HAS
     * TO BE READ. (queue item 15, ruled 2026-09-25) The cursor is that
     * writer's end and nothing else in the listing enters it, so another
     * writer the core could not read -- `(end unreadable)`, F77 -- does not
     * stop it. The whole-listing rule below stays for the absent clause,
     * where the COUNT decides and an unread entry would change it.
     */
    if (own.end === null) {
      return { ok: false, reason: 'unreadable', writers, local: own.writer, unreadable: 'local-end' };
    }
    return { ok: true, cursor: formatCursor({ writer: own.writer, seq: own.end }) };
  }
  if (writers.some((w) => w.end === null)) {
    return { ok: false, reason: 'unreadable', writers, unreadable: 'listing' };
  }
  if (writers.length === 1) {
    return { ok: true, cursor: formatCursor({ writer: writers[0].writer, seq: writers[0].end as number }) };
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
   * NEVER: THE ENVELOPE IS UNWRAPPED ONCE, BY THE CLIENT, AND NOT AGAIN
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
   * NEVER: AND THE FORM HAS TO HAVE SAID `ok`.
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
  /*
   * NEVER: A NAME THAT IS THERE AND CANNOT BE READ STOPS THE SEARCH.
   *
   * This asked `value` for each name in turn and moved on when it got
   * `undefined` -- which the decoder gave for "there is no such clause"
   * and for "there are two of them" alike. Measured in a seventeenth
   * review round:
   * `(ok (cursor ("a" . 1)) (cursor ("b" . 2)) (event ("wrong" . 99))
   * (replay #f))` answered `{writer: "wrong", seq: 99}` -- the two
   * cursors cancelled out and the cursor came from the `event` clause,
   * and a Saver settles a save `confirmed` on it.
   *
   * Absence still falls through to the next name, because `cursor` and
   * `event` are two spellings of one thing and an answer carries one of
   * them. Anything else refuses.
   */
  for (const name of ['cursor', 'event']) {
    const found = form.value(name);
    if (!found.read) {
      if (found.because === 'absent') {
        continue;
      }
      return null;
    }
    /*
     * NEVER: AND A CLAUSE THAT IS THERE AND WILL NOT DECODE IS NOT AN
     * ABSENT ONE EITHER.
     *
     * The decoder answers whether the CLAUSE was found; whether its
     * value is an event is this reader's own question, and it used to
     * answer that question by moving on. Measured in an eighteenth
     * review round: `(ok (cursor bad) (event ("wrong" . 99)))` gave
     * `{writer: "wrong", seq: 99}` -- the answer names a cursor, this
     * client cannot read it, and the save settles `confirmed` on a
     * cursor taken from somewhere else. One clause is present, so the
     * search is over; what remains is whether it can be read.
     */
    const event = readEvent(found.value);
    return event;
  }
  return null;
}

export function isReplay(answer: Answer): boolean {
  if (!answer.ok || answer.answers.length === 0) {
    return false;
  }
  /*
   * NEVER: AS ABOVE -- no second unwrapping. The same crafted answer made
   * this report a replay where the answer says it is not one, which
   * would have a save reported as "the store had already applied this
   * request" when the store had just applied it for the first time.
   */
  /*
   * NOTE: AND A `replay` CLAUSE THIS BUILD CANNOT READ ANSWERS THE SAME AS
   * AN ABSENT ONE, which is the one place in this file where the two are
   * deliberately not told apart. The reason is reachability: by the time
   * this is asked, the save has been settled on a cursor -- and a cursor
   * taken from an answer with two of them is refused above. What is left
   * for this to decide is the WORD shown to the user, `replayed` or
   * `saved`, and `saved` is the answer that claims nothing about which
   * it was. A third state here would need a third sentence, which is a
   * question for whoever writes that sentence and not for this reader.
   */
  const form = answerOf(answer.answers[0], 'ok');
  if (form === null) {
    return false;
  }
  const replay = form.value('replay');
  return replay.read && replay.value === true;
}

export const CURSOR_SHAPE = /^[^:\s]+:(0|[1-9][0-9]*)$/;

export function isWellFormedCursor(cursor: string): boolean {
  return CURSOR_SHAPE.test(cursor);
}

/*
 * WHOSE LOG A POSITION IS IN, AND WHERE IN IT. (section 13.3)
 *
 * NOTE: TWO WRITERS' NUMBERS ARE NOT COMPARABLE. A cursor is `writer:n`,
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
