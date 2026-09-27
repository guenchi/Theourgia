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
 * What a block says, and what a buffer holding one contains.
 *
 * THE HEADING IS A FIELD, NOT THE FIRST LINE. `read <id> --md` hands
 * back the heading the block was written with followed by its body, and
 * splitting that text at the first newline gets the answer right only
 * for blocks that HAVE a heading -- a block made by `insert` has no
 * `heading-src` at all, and for one of those the first line of the text
 * is the first line of the BODY. Reading the two fields separately is
 * the only split that is correct for both, so the buffer is composed
 * here from `heading-src` and `src` and taken apart by the same rule.
 */

import { TransportError } from './transport';
import { Datum, asInteger, assocTail, cdrOf, clauseOfRecord, isDotted, isList, isSym } from './wire';

/*
 * WHERE A BLOCK SITS HAS THREE ANSWERS, NOT TWO. `(position root . 0)`
 * is the top level and `(position "<id>" . <n>)` is under a block -- and
 * `(position conflict <n>)` is neither: the core writes that when a
 * block has more than one position candidate, which is what two
 * concurrent moves leave behind. Reading it as a missing parent, which
 * is what a two-valued answer forces, says "top level" about a block
 * whose place nobody knows -- so a block that has been moved twice would
 * be listed among the roots on the strength of an answer that means the
 * opposite.
 */
export type Placement =
  | { kind: 'root' }
  | { kind: 'under'; parent: string }
  | { kind: 'unsettled'; candidates: number | null };

export interface Block {
  id: string;
  deleted: boolean;
  fields: Map<string, Datum>;
  placement: Placement;
  ord: number | null;
}

export interface FieldConflict {
  candidates: Datum[];
}

export function isBlockDatum(value: Datum): boolean {
  return isList(value) && readBlock(value) !== null;
}

/*
 * ENOUGH OF AN UNREADABLE DATUM TO RECOGNISE IT, and no more: the value
 * may be a whole document body, and an error message is not the place
 * for one.
 */
function describe(value: Datum): string {
  const text = String(value);
  return text.length > 200 ? `${text.slice(0, 200)}...` : text;
}

/*
 * A BLOCK IS AN ASSOCIATION LIST AND IS READ BY NAME. Its entries are
 * written in a fixed order today -- id, deleted, fields, position, edges
 * -- and reading them by position would be a client that breaks the
 * first time the core adds a sixth.
 */
export function readBlock(value: Datum): Block | null {
  if (!isList(value)) {
    return null;
  }
  /*
   * NEVER: A HEADLESS READER MUST REFUSE A FORM THAT HAS A HEAD.
   *
   * This checked only that the value was a list, so
   * `(garbage (id . "a.1") (fields (src . "forged")) ...)` read as a
   * block -- a record with a head on the front, taken apart by the one
   * reader that exists because records have none. Measured in a
   * fourteenth review round; the forged body reached a caller. The door
   * held open for headless values has to be narrow enough to hold only
   * them.
   *
   * A record's first element is a PAIR: `(id . "a.1")`. A form's is a
   * name -- or anything else at all. The first version of this asked
   * whether the first element was a SYMBOL, which is the same mistake
   * one step in: `("garbage" (id . "a.1") ...)` and the same beginning
   * with a number both still read as blocks. Measured in a fifteenth
   * review round. What a record IS, is a list whose first element is a
   * pair; everything else is refused.
   */
  if (value.length === 0 || !isDotted(value[0])) {
    return null;
  }
  /*
   * NEVER: AND A RECORD THAT NAMES TWO IDENTITIES IS NOT A RECORD WITH
   * ONE. `assocTail` used to take the first of several -- measured in a
   * seventeenth review round, where `((id . "a.1") (id . "b.1") ...)`
   * read as block a.1. It answers `duplicated` now, and that is a
   * refusal here for the same reason `deleted` and `position` are: this
   * reader cannot choose between two answers the store gave.
   */
  const named = assocTail(value, 'id');
  const id = named.read ? named.value : undefined;
  if (typeof id !== 'string') {
    return null;
  }
  const marked = assocTail(value, 'deleted');
  if (!marked.read && marked.because !== 'absent') {
    return null;
  }
  const deleted = marked.read ? marked.value : undefined;
  /*
   * NOTE: A BLOCK RECORD HAS NO HEAD. Its first element is
   * `(id . "a.1")` -- a pair, not a name -- so there is nothing for
   * `answerOf` to verify here, and the record has already come out of a
   * form whose head was checked. See `clauseOfRecord` in wire.ts.
   */
  /*
   * NEVER: TWO `fields` CLAUSES IS NOT A RECORD WITH NO FIELDS.
   *
   * The decoder began refusing a duplicated clause in a fifteenth review
   * round, and refused it by answering null -- the same answer as "this
   * record has no fields at all". A sixteenth round measured what
   * happened here:
   * `((id . "a.1") (fields (src . "body")) (fields (title . "A")) ...)`
   * read as block a.1 with NO fields, and `documentFor` served it with
   * an empty prefix, an empty src and empty text. A record whose fields
   * this build cannot resolve is unreadable, which is the refusal
   * already written below for a field of an unknown shape.
   */
  const fieldList = clauseOfRecord(value, 'fields');
  if (!fieldList.read && fieldList.because !== 'absent') {
    throw new TransportError(
      'unreadable',
      `the core answered with two field lists for block ${id}, and which one holds its text ` +
        'is not something this client may choose',
      describe(value)
    );
  }
  const fields = new Map<string, Datum>();
  if (fieldList.read) {
    for (const entry of fieldList.items) {
      /*
       * A FIELD WHOSE VALUE IS A LIST LOSES ITS DOT ON THE WIRE.
       * `(title . (conflict (...)))` and `(title conflict (...))` are one
       * datum, so a field entry that is a proper list carries everything
       * after the name as its value -- which is how a conflicted field
       * arrives.
       */
      /*
       * NEVER: AND A NAME THAT APPEARS TWICE IS NOT THE SECOND ONE.
       *
       * The repair above protects which LIST is read; this protects what
       * is in it. Measured in a seventeenth review round:
       * `(fields (src . "one") (src . "two"))` gave `src = "two"`, and
       * `src` is the document body -- a block served with one of two
       * bodies the store named, chosen by the order they were written
       * in. `set` on a Map is silent about it, which is why nothing said
       * so.
       */
      const named = isDotted(entry) && entry.items.length >= 1 && isSym(entry.items[0])
        ? (entry.items[0] as { name: string }).name
        : isList(entry) && entry.length >= 1 && isSym(entry[0])
          ? (entry[0] as { name: string }).name
          : null;
      if (named !== null && fields.has(named)) {
        throw new TransportError(
          'unreadable',
          `the core answered with two fields called ${named} for block ${id}, and which one is ` +
            'the block is not something this client may choose',
          describe(entry)
        );
      }
      if (isDotted(entry) && entry.items.length >= 1 && isSym(entry.items[0])) {
        fields.set((entry.items[0] as { name: string }).name, cdrOf(entry));
        continue;
      }
      if (isList(entry) && entry.length >= 1 && isSym(entry[0])) {
        fields.set((entry[0] as { name: string }).name, entry.slice(1));
        continue;
      }
      /*
       * A FIELD THIS CANNOT READ STOPS THE WHOLE BLOCK. Skipping it
       * would produce a block that is missing a field, which is
       * indistinguishable from a block that never had one -- so a title
       * in a shape this client has not met would show as an untitled
       * block rather than as something to look at. The same refusal the
       * client makes for an unregistered verb.
       */
      throw new TransportError(
        'unreadable',
        `the core answered with a field this client cannot read, in block ${id}`,
        describe(entry)
      );
    }
  }
  const placed = assocTail(value, 'position');
  if (!placed.read && placed.because !== 'absent') {
    return null;
  }
  const position = placed.read ? placed.value : undefined;
  const placement = placementOf(position);
  return {
    id,
    deleted: deleted === true,
    fields,
    placement,
    /*
     * A BLOCK WHOSE PLACE IS UNSETTLED HAS NO SIBLING INDEX. The datum
     * for one is `(position conflict <n>)`, where n counts the CANDIDATE
     * POSITIONS -- and reading the second element of a position as an
     * order, which is right for every other shape, turns that count into
     * an index among siblings it does not have.
     */
    ord: placement.kind === 'unsettled' ? null : ordOf(position)
  };
}

export function parentOf(block: Block): string | null {
  return block.placement.kind === 'under' ? block.placement.parent : null;
}

export function isTopLevel(block: Block): boolean {
  return block.placement.kind === 'root';
}

/*
 * A TOP-LEVEL BLOCK NAMES ITS PARENT WITH THE SYMBOL `root` AND EVERY
 * OTHER BLOCK NAMES IT WITH A STRING. Two spellings because they are two
 * different things -- `root` is not a block and cannot be read. The
 * third shape, `(position conflict <n>)`, is a block whose place is
 * unsettled, and it is kept apart from both: it is not the top level and
 * there is no parent to name.
 */
function placementOf(position: Datum): Placement {
  if (isDotted(position)) {
    const first = position.items[0];
    if (typeof first === 'string') {
      return { kind: 'under', parent: first };
    }
    if (isSym(first, 'root')) {
      return { kind: 'root' };
    }
    return { kind: 'unsettled', candidates: null };
  }
  if (isList(position) && position.length >= 1) {
    const first = position[0];
    if (typeof first === 'string') {
      return { kind: 'under', parent: first };
    }
    if (isSym(first, 'root')) {
      return { kind: 'root' };
    }
    if (isSym(first, 'conflict')) {
      return { kind: 'unsettled', candidates: position.length >= 2 ? asInteger(position[1]) : null };
    }
  }
  return { kind: 'unsettled', candidates: null };
}

function ordOf(position: Datum): number | null {
  if (isDotted(position)) {
    return asInteger(position.tail);
  }
  if (isList(position) && position.length >= 2) {
    return asInteger(position[1]);
  }
  return null;
}

/*
 * A FIELD WHOSE VALUE IS `(conflict (...))` HAS CANDIDATES AND NO VALUE.
 * Reading one as though it were the value would show a caller the word
 * "conflict" where a title belongs.
 */
export function fieldConflict(value: Datum): FieldConflict | null {
  if (isList(value) && value.length >= 1 && isSym(value[0], 'conflict')) {
    const rest = value[1];
    return { candidates: isList(rest) ? rest : value.slice(1) };
  }
  return null;
}

/*
 * A FIELD THAT HOLDS BYTES WHICH ARE NOT UTF-8 TEXT, refused by name: which
 * field, of which block, and the offset of the first byte that does not
 * decode. Never read as an empty field and never decoded with replacement
 * characters: either would show a document that is not what the store
 * holds.
 */
export class TextNotUtf8 extends Error {
  public readonly field: string;
  public readonly id: string;
  public readonly offset: number;

  constructor(field: string, id: string, offset: number) {
    super(`text-not-utf8: the ${field} of ${id} is not UTF-8 text (the byte at offset ${offset} does not decode)`);
    this.name = 'TextNotUtf8';
    this.field = field;
    this.id = id;
    this.offset = offset;
  }
}

/*
 * A FIELD AS TEXT, THE ONE DECODER FOR EVERY FIELD READER.
 *
 * A string is itself. Bytes -- how the core stores a text-mode code block's
 * source and the doc derived from it -- decode as UTF-8, and a decode
 * failure is `TextNotUtf8`. Anything else (absent, a conflict, a datum) is
 * answered as it is, so that each reader keeps its own rule for those: a
 * field reader answers "", the document view refuses a field that is there
 * and is not text.
 */
export function textOf(block: Block, name: string): Datum {
  const value = block.fields.get(name);
  if (!(value instanceof Uint8Array)) {
    return value;
  }
  const bad = firstInvalidUtf8(value);
  if (bad >= 0) {
    throw new TextNotUtf8(name, block.id, bad);
  }
  return Buffer.from(value).toString('utf8');
}

/*
 * THE OFFSET OF THE FIRST BYTE THAT DOES NOT BEGIN, OR CONTINUE, A VALID
 * UTF-8 SEQUENCE; -1 when every byte does. Overlong forms, surrogates and
 * code points past U+10FFFF are invalid, as a fatal decoder counts them.
 */
export function firstInvalidUtf8(bytes: Uint8Array): number {
  let at = 0;
  while (at < bytes.length) {
    const b = bytes[at];
    let need = 0;
    let min = 0;
    let code = 0;
    if (b < 0x80) {
      at += 1;
      continue;
    } else if (b >= 0xc2 && b <= 0xdf) {
      need = 1;
      min = 0x80;
      code = b & 0x1f;
    } else if (b >= 0xe0 && b <= 0xef) {
      need = 2;
      min = 0x800;
      code = b & 0x0f;
    } else if (b >= 0xf0 && b <= 0xf4) {
      need = 3;
      min = 0x10000;
      code = b & 0x07;
    } else {
      return at;
    }
    for (let k = 1; k <= need; k += 1) {
      const next = bytes[at + k];
      if (next === undefined || (next & 0xc0) !== 0x80) {
        return at;
      }
      code = (code << 6) | (next & 0x3f);
    }
    if (code < min || code > 0x10ffff || (code >= 0xd800 && code <= 0xdfff)) {
      return at;
    }
    at += need + 1;
  }
  return -1;
}

export function stringField(block: Block, name: string): string {
  const value = textOf(block, name);
  if (typeof value === 'string') {
    return value;
  }
  return '';
}

export function titleOf(block: Block): string {
  const value = textOf(block, 'title');
  if (typeof value === 'string') {
    return value;
  }
  const path = textOf(block, 'path');
  if (typeof path === 'string') {
    return path;
  }
  return '';
}

export function hasFieldConflict(block: Block): boolean {
  for (const value of block.fields.values()) {
    if (fieldConflict(value) !== null) {
      return true;
    }
  }
  return false;
}

/*
 * A BUFFER REMEMBERS WHICH STORE IT CAME OUT OF. The file it lives in is
 * named under that store, but the client that would save it is whichever
 * one the settings name NOW -- so a buffer still open from a store the
 * user has since switched away from would be written into the new one,
 * under an id that means something else there, or nothing. Carrying the
 * store here makes that a question the save path can ask.
 */
export interface BlockDocument {
  id: string;
  store: string;
  /*
   * EVERYTHING BEFORE THE BODY, AND NONE OF IT EDITABLE HERE. The core
   * composes a block's own bytes as `front + heading-src + src`: front
   * matter, then the heading the block was written with, then the body.
   * A file-level block is where front matter lives, and composing
   * without it showed a document as an almost empty buffer -- inviting
   * the user to paste the front matter back in, which would then exist
   * twice, once in the field and once in the body.
   */
  prefix: string;
  headingSrc: string;
  front: string;
  src: string;
  text: string;
}

/*
 * THE BUFFER IS THE HEADING FOLLOWED BY THE BODY, which is byte for byte
 * what `read <id> --md` prints. Composing it here rather than asking for
 * it means the two halves are still separate when the buffer is saved,
 * and it is that separation -- not a line count -- that decides whether
 * the title was edited.
 */
export function documentFor(block: Block, store: string): BlockDocument {
  const front = stringField(block, 'front');
  const headingSrc = stringField(block, 'heading-src');
  const src = stringField(block, 'src');
  const prefix = front + headingSrc;
  return { id: block.id, store, prefix, front, headingSrc, src, text: prefix + src };
}

/*
 * THE EDITOR'S LANGUAGE MODE FOR A BLOCK: a code block's own `lang`, a prose
 * block markdown. A code block with no lang is plain text.
 */
export function languageModeOf(block: Block): string {
  const kind = block.fields.get('kind');
  const lang = block.fields.get('lang');
  const code = (isSym(kind) && kind.name === 'code') || kind === 'code';
  if (!code) {
    return 'markdown';
  }
  return isSym(lang) ? lang.name : typeof lang === 'string' && lang.length > 0 ? lang : 'plaintext';
}

/*
 * THE PREFIX ALONE -- front matter and heading -- for a caller that does not
 * show the source from this record. Opening a block takes its text from the
 * core's working read, which decodes the source itself and refuses a
 * source that is not UTF-8 by name; decoding it here first would answer
 * that refusal with this client's instead.
 */
export function prefixOf(block: Block): string {
  return stringField(block, 'front') + stringField(block, 'heading-src');
}

export type SplitResult =
  | { ok: true; src: string; normalised: boolean }
  | { ok: false; reason: 'heading-changed' };

/*
 * WHAT CHANGED IS DECIDED BY THE PREFIX'S BYTES, NOT BY A LINE NUMBER.
 * The prefix is the front matter and the heading together -- everything
 * the core puts before the body -- because neither is edited by writing
 * to `src`, and a buffer that no longer starts with both is a buffer
 * whose front matter or title was changed.
 * Everything after the heading is the new body, and a buffer that no
 * longer starts with the heading it was given is a buffer whose title
 * was edited -- which is a different verb (`set <id> title`) and is not
 * in this batch, so it is refused rather than half-applied.
 *
 * A BLOCK WITH NO HEADING HAS NO PREFIX TO CHECK, and the whole buffer
 * is its body. That is the case a first-line split gets wrong.
 *
 * CRLF IS THE EDITOR'S, NOT THE BLOCK'S. An editor configured to write
 * CRLF turns the heading we handed it into one we would no longer
 * recognise, and the user would be told they had edited a title they
 * never touched. So a buffer whose block held no carriage return at all
 * is read back with its line endings normalised -- and the normalisation
 * is reported rather than done quietly, because it is a change to the
 * bytes that get written.
 */
export function splitDocument(document: BlockDocument, buffer: string): SplitResult {
  let text = buffer;
  let normalised = false;
  if (!document.text.includes('\r') && text.includes('\r\n')) {
    text = text.replace(/\r\n/g, '\n');
    normalised = true;
  }
  if (document.prefix.length === 0) {
    return { ok: true, src: text, normalised };
  }
  if (!text.startsWith(document.prefix)) {
    return { ok: false, reason: 'heading-changed' };
  }
  return { ok: true, src: text.slice(document.prefix.length), normalised };
}
