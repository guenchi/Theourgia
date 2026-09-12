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
import { Datum, Event, asInteger, clause, clauseValue, formatCursor, isList, readEvent } from './wire';

export interface WriterEnd {
  writer: string;
  end: number;
}

export function writersFromCheck(answer: Datum): WriterEnd[] {
  const writers = clause(answer, 'writers');
  if (writers === null || writers.length < 2 || !isList(writers[1])) {
    return [];
  }
  const out: WriterEnd[] = [];
  for (const entry of writers[1]) {
    if (!isList(entry) || entry.length < 1 || typeof entry[0] !== 'string') {
      continue;
    }
    const end = asInteger(clauseValue(entry, 'end'));
    if (end !== null) {
      out.push({ writer: entry[0], end });
    }
  }
  return out;
}

export type FirstCursor =
  | { ok: true; cursor: string }
  | { ok: false; reason: 'no-writer' | 'many-writers'; writers: WriterEnd[] };

export function firstCursorFromCheck(answer: Datum): FirstCursor {
  const writers = writersFromCheck(answer);
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
  const datum = answer.answers[0];
  for (const name of ['cursor', 'event']) {
    const found = clauseValue(datum, name);
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
  return clauseValue(answer.answers[0], 'replay') === true;
}

export const CURSOR_SHAPE = /^[^:\s]+:(0|[1-9][0-9]*)$/;

export function isWellFormedCursor(cursor: string): boolean {
  return CURSOR_SHAPE.test(cursor);
}

/*
 * WHOSE LOG A POSITION IS IN, AND WHERE IN IT. (§13.3)
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
