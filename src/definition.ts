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
 * WHERE A NAME IS DEFINED, AS THE STORE ANSWERS IT.
 *
 * The core's `whereis <name>` answers an items list of two kinds of
 * record: `(def <id> (library <lib>) (name <sym>) (kind code))` for a code
 * block that defines the name, and `(export <lib-id> (library <lib>) (name
 * <sym>))` for a library block that exports it. A name nobody defines or
 * exports is a REFUSAL, `(error unknown-name <name> (nearest <sym> ...))`,
 * never an empty list. Neither record names a line: the store knows which
 * block, and the line is found here, in the text the person is looking at.
 *
 * Nothing in this file waits on the editor; the command and the editor's
 * own Go to Definition, which do, are wired in extension.ts.
 */

import { Client, Note } from './client';
import { TransportError } from './transport';
import { Datum, Form, answerOf, asInteger, isList, isSym, wire } from './wire';

export interface DefinitionRecord {
  kind: 'def' | 'export';
  id: string;
  library: string;
  name: string;
  /*
   * WHAT A DEFINITION IS, as the store says it (`(kind code)`); an export
   * has none.
   */
  of: string | null;
}

export type DefinitionAnswer =
  | { found: DefinitionRecord[]; notes: Note[] | null }
  | { none: true; name: string; nearest: string[]; leftOut: LeftOut | null; notes: Note[] | null };

/*
 * THE BLOCKS A LOOKUP LEFT OUT because they are superseded or refuted, as
 * the store counted them in its `(excluded (blocks (superseded <n>)
 * (refuted <n>)))` clause.
 */
export interface LeftOut {
  superseded: number;
  refuted: number;
}

/*
 * THE NAME UNDER THE CURSOR. A Scheme identifier runs between delimiters
 * -- whitespace, brackets, quotes, `;` -- so `foo-bar`, `foo?` and
 * `set-car!` are one name each. Inside a `[[key]]` reference the key is the
 * name. Null when the cursor is on a delimiter.
 */
const DELIMITER = /[\s()[\]{}"';`,]/;

export function identifierAt(text: string, line: number, character: number): string | null {
  const lines = text.split('\n');
  if (line < 0 || line >= lines.length) {
    return null;
  }
  const row = lines[line];
  const open = row.lastIndexOf('[[', character);
  const close = row.indexOf(']]', open + 2);
  if (open >= 0 && close >= 0 && open + 2 <= character && character <= close) {
    const key = row.slice(open + 2, close).split('|')[0].trim();
    if (key.length > 0) {
      return key;
    }
  }
  let start = Math.min(character, row.length);
  while (start > 0 && !DELIMITER.test(row[start - 1])) {
    start -= 1;
  }
  let end = Math.min(character, row.length);
  while (end < row.length && !DELIMITER.test(row[end])) {
    end += 1;
  }
  return end > start ? row.slice(start, end) : null;
}

/*
 * THE TEXT WITH ITS COMMENTS AND STRING CONTENTS BLANKED, every character
 * kept in its place and every newline kept, so an offset into the result is
 * an offset into the text. Line comments (`;` to the end of the line) and
 * block comments (`#| |#`, which nest) are blanked; so is what a string
 * holds, between its quotes, so that neither a `;` nor a `(define` quoted in
 * a string is read. Datum comments (`#;`) are not read: the form after one
 * stays visible, a limit stated here rather than a scanner that pretends to
 * know where the next datum ends.
 */
export function blankComments(text: string): string {
  const out = text.split('');
  let at = 0;
  let depth = 0;
  let inString = false;
  while (at < text.length) {
    const c = text[at];
    if (depth > 0) {
      if (c === '|' && text[at + 1] === '#') {
        out[at] = ' ';
        out[at + 1] = ' ';
        depth -= 1;
        at += 2;
        continue;
      }
      if (c === '#' && text[at + 1] === '|') {
        out[at] = ' ';
        out[at + 1] = ' ';
        depth += 1;
        at += 2;
        continue;
      }
      if (c !== '\n') {
        out[at] = ' ';
      }
      at += 1;
      continue;
    }
    if (inString) {
      if (c === '\\') {
        out[at] = ' ';
        if (at + 1 < text.length && text[at + 1] !== '\n') {
          out[at + 1] = ' ';
        }
        at += 2;
        continue;
      }
      if (c === '"') {
        inString = false;
      } else if (c !== '\n') {
        out[at] = ' ';
      }
      at += 1;
      continue;
    }
    if (c === '"') {
      inString = true;
      at += 1;
      continue;
    }
    if (c === '#' && text[at + 1] === '|') {
      out[at] = ' ';
      out[at + 1] = ' ';
      depth = 1;
      at += 2;
      continue;
    }
    if (c === ';') {
      while (at < text.length && text[at] !== '\n') {
        out[at] = ' ';
        at += 1;
      }
      continue;
    }
    at += 1;
  }
  return out.join('');
}

const DEFINING = /\((?:define-record-type|define-syntax|define)\s+\(?/g;
const AFTER_A_NAME = /[\s()[\]"';]/;

/*
 * THE LINE, IN THE DISPLAYED DOCUMENT, WHERE `name` IS DEFINED.
 *
 * The scan runs over the document's own text from `from` on -- the prefix
 * the editor shows before the source (front matter and heading) is skipped
 * by its length -- so a draft with lines of its own resolves to the line it
 * shows, not the one the store holds. The first `(define`, `(define-syntax`
 * or `(define-record-type` whose name is exactly `name`, outside comments
 * and across a line break, wins; `(define (name args ...) ...)` counts. A
 * name that is only a prefix of the defined one (`foo-bar` in
 * `foo-bar-baz`) does not. With none, the first line after the prefix.
 *
 * Answered as a zero-based line of the whole document.
 */
export function definitionLine(text: string, from: number, name: string): number {
  const start = Math.max(0, Math.min(from, text.length));
  const blanked = blankComments(text.slice(start));
  DEFINING.lastIndex = 0;
  for (let m = DEFINING.exec(blanked); m !== null; m = DEFINING.exec(blanked)) {
    const at = m.index + m[0].length;
    const after = blanked[at + name.length];
    if (blanked.startsWith(name, at) && (after === undefined || AFTER_A_NAME.test(after))) {
      return lineOf(text, start + m.index);
    }
  }
  return lineOf(text, start);
}

/*
 * WHERE A RECORD OPENS IN THE DISPLAYED DOCUMENT: a definition at its
 * definition's line; an export at the first line after the prefix, without
 * a scan -- an export names a library and no line in it, and a scan of the
 * library's text could land on a definition of the same name that is not
 * the one exported.
 */
export function targetLine(record: DefinitionRecord, text: string, prefixLength: number): number {
  return record.kind === 'def' ? definitionLine(text, prefixLength, record.name) : firstLineAfter(text, prefixLength);
}

/*
 * THE FIRST LINE AFTER THE PREFIX.
 */
export function firstLineAfter(text: string, from: number): number {
  return lineOf(text, Math.max(0, Math.min(from, text.length)));
}

function lineOf(text: string, offset: number): number {
  let line = 0;
  for (let i = 0; i < offset; i += 1) {
    if (text[i] === '\n') {
      line += 1;
    }
  }
  return line;
}

function symbolName(value: Datum): string {
  return isSym(value) ? value.name : typeof value === 'string' ? value : wire().write(value);
}

function recordOf(item: Datum): DefinitionRecord | null {
  for (const kind of ['def', 'export'] as const) {
    const form = answerOf(item, kind);
    if (form === null) {
      continue;
    }
    if (!isList(item) || typeof item[1] !== 'string') {
      return null;
    }
    const library = form.value('library');
    const name = form.value('name');
    if (!library.read || !name.read) {
      return null;
    }
    const of = kind === 'def' ? form.value('kind') : null;
    return {
      kind,
      id: item[1],
      library: library.value === false ? 'no library' : wire().write(library.value),
      name: symbolName(name.value),
      of: of !== null && of.read ? symbolName(of.value) : null
    };
  }
  return null;
}

/*
 * ASK THE STORE. A refusal that is not `unknown-name` goes back to the
 * caller as the failure it is; an answer holding something that is neither
 * record is not read as fewer records.
 */
export async function definitionsOf(client: Client, name: string): Promise<DefinitionAnswer> {
  const answer = await client.request('whereis', [name, '--wire']);
  const notes = answer.notes ?? null;
  if (!answer.ok) {
    const said = answer.answers.length === 1 ? answer.answers[0] : null;
    const refusal = said === null ? null : answerOf(said, 'error', { at: 1, is: 'unknown-name' });
    if (refusal !== null) {
      const nearest = refusal.clause('nearest');
      return { none: true, name, nearest: nearest.read ? nearest.items.map(symbolName) : [], leftOut: null, notes };
    }
    throw new TransportError(
      'unreadable',
      `the store would not look up ${name}: ${answer.text.trim() || answer.stderr.trim()}`,
      answer.text
    );
  }
  const found: DefinitionRecord[] = [];
  for (const item of answer.answers) {
    const record = recordOf(item);
    if (record === null) {
      throw new TransportError(
        'unreadable',
        `the store answered the lookup of ${name} with something that is neither a definition nor an export`,
        answer.text
      );
    }
    found.push(record);
  }
  /*
   * A NAME WHOSE EVERY RECORD WAS LEFT OUT. From theourgia 06a348b
   * `whereis` leaves out the records in superseded and refuted blocks, and
   * a name all of whose records were left out answers no items with an
   * `excluded` clause, not `unknown-name` (README, "Class and validity";
   * lifecycle.sc `whereis-split`). Read by name for this one sentence and
   * for nothing else: the name has records, in blocks that are not in force.
   */
  if (found.length === 0) {
    return { none: true, name, nearest: [], leftOut: leftOutOf(answer.envelope), notes };
  }
  return { found, notes };
}

/*
 * THE COUNTS IN AN ANSWER'S `excluded` CLAUSE, or null when there is no such
 * clause, it cannot be read, or it counts nothing.
 */
function leftOutOf(envelope: Datum | null): LeftOut | null {
  const form = envelope === null ? null : answerOf(envelope, 'ok');
  const excluded = form === null ? null : form.value('excluded');
  const blocks = excluded === null || !excluded.read ? null : answerOf(excluded.value, 'blocks');
  if (blocks === null) {
    return null;
  }
  const superseded = countIn(blocks, 'superseded');
  const refuted = countIn(blocks, 'refuted');
  if (superseded === null || refuted === null || superseded + refuted === 0) {
    return null;
  }
  return { superseded, refuted };
}

function countIn(form: Form, which: string): number | null {
  const said = form.value(which);
  const n = said.read ? asInteger(said.value) : null;
  return n === null || n < 0 ? null : n;
}

/*
 * WHAT IS SAID WHEN NOTHING DEFINES THE NAME, with the names the store
 * found nearest to it -- or, when the store left out every block that
 * holds a record of it, that it is found only in blocks that are not in
 * force. "Found", not "defined": a record may be an export, and a name a
 * library only exports is not defined there.
 */
export function noDefinitionNotice(name: string, nearest: string[], leftOut: LeftOut | null = null): string {
  if (leftOut !== null) {
    const which =
      leftOut.superseded > 0 && leftOut.refuted > 0 ? 'superseded or refuted' : leftOut.superseded > 0 ? 'superseded' : 'refuted';
    const blocks = leftOut.superseded + leftOut.refuted === 1 ? 'a block that is' : 'blocks that are';
    return `no definition of ${name} in force: it is found only in ${blocks} ${which}.`;
  }
  return nearest.length === 0
    ? `no definition of ${name}.`
    : `no definition of ${name}; nearest: ${nearest.join(', ')}.`;
}

/*
 * HOW A RECORD IS LISTED WHEN THERE ARE SEVERAL: its library, then what it
 * is -- the definition's kind, or "export".
 */
export function recordLabel(record: DefinitionRecord): string {
  return `${record.library} ${record.kind === 'export' ? 'export' : record.of ?? 'definition'}`;
}
