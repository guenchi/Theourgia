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

import { Datum, asInteger, assocTail, cdrOf, clauseRest, isDotted, isList, isSym } from './wire';

export interface Block {
  id: string;
  deleted: boolean;
  fields: Map<string, Datum>;
  parent: string | null;
  ord: number | null;
}

export interface FieldConflict {
  candidates: Datum[];
}

export function isBlockDatum(value: Datum): boolean {
  return isList(value) && readBlock(value) !== null;
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
  const id = assocTail(value, 'id');
  if (typeof id !== 'string') {
    return null;
  }
  const deleted = assocTail(value, 'deleted');
  const fieldList = clauseRest(value, 'fields');
  const fields = new Map<string, Datum>();
  if (fieldList !== null) {
    for (const entry of fieldList) {
      /*
       * A FIELD WHOSE VALUE IS A LIST LOSES ITS DOT ON THE WIRE.
       * `(title . (conflict (...)))` and `(title conflict (...))` are one
       * datum, so a field entry that is a proper list carries everything
       * after the name as its value -- which is how a conflicted field
       * arrives.
       */
      if (isDotted(entry) && entry.items.length >= 1 && isSym(entry.items[0])) {
        fields.set((entry.items[0] as { name: string }).name, cdrOf(entry));
      } else if (isList(entry) && entry.length >= 1 && isSym(entry[0])) {
        fields.set((entry[0] as { name: string }).name, entry.slice(1));
      }
    }
  }
  const position = assocTail(value, 'position');
  return {
    id,
    deleted: deleted === true,
    fields,
    parent: parentOf(position),
    ord: ordOf(position)
  };
}

/*
 * A TOP-LEVEL BLOCK NAMES ITS PARENT WITH THE SYMBOL `root` AND EVERY
 * OTHER BLOCK NAMES IT WITH A STRING. Two spellings because they are two
 * different things -- `root` is not a block and cannot be read -- so
 * null here means "directly under the store" and never "unknown".
 */
function parentOf(position: Datum): string | null {
  if (isDotted(position)) {
    const first = position.items[0];
    if (typeof first === 'string') {
      return first;
    }
    return null;
  }
  if (isList(position) && position.length >= 1) {
    const first = position[0];
    return typeof first === 'string' ? first : null;
  }
  return null;
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

export function stringField(block: Block, name: string): string {
  const value = block.fields.get(name);
  if (typeof value === 'string') {
    return value;
  }
  return '';
}

export function titleOf(block: Block): string {
  const value = block.fields.get('title');
  if (typeof value === 'string') {
    return value;
  }
  const path = block.fields.get('path');
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

export interface BlockDocument {
  id: string;
  headingSrc: string;
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
export function documentFor(block: Block): BlockDocument {
  const headingSrc = stringField(block, 'heading-src');
  const src = stringField(block, 'src');
  return { id: block.id, headingSrc, src, text: headingSrc + src };
}

export type SplitResult =
  | { ok: true; src: string; normalised: boolean }
  | { ok: false; reason: 'heading-changed' };

/*
 * WHAT CHANGED IS DECIDED BY THE HEADING'S BYTES, NOT BY A LINE NUMBER.
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
  if (document.headingSrc.length === 0) {
    return { ok: true, src: text, normalised };
  }
  if (!text.startsWith(document.headingSrc)) {
    return { ok: false, reason: 'heading-changed' };
  }
  return { ok: true, src: text.slice(document.headingSrc.length), normalised };
}
