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
 * WHERE EACH BLOCK SITS IN A FILE `export-code` WROTE.
 *
 * The core's code projection writes a text-mode file as its blocks' bytes
 * between marker lines, each wrapped in the language's comment syntax:
 *
 *   <prefix of the first block: a byte-order mark, a shebang, ...>
 *   <prefix>@file <hex of the header><suffix>
 *   <prefix>@block <id> pad <0|1><suffix>
 *   <the block's own bytes, a marker-like line escaped with one more @>
 *   <prefix>@block <id> pad <0|1><suffix>
 *   ...
 *
 * `pad 1` on a marker says the block before it did not end with a line
 * feed, so one was written before the marker; that line feed is the
 * marker's, not the block's. The header's last element says the same of
 * the first block's prefix and the `@file` line. (The core's own layout is
 * `projection-encode-map` in code-markers.sc, and the marker grammar is
 * markers.sc.)
 *
 * NOTE: THE COMMENT WRAPPING IS READ OFF THE FILE'S OWN HEADER LINE. It comes
 * from the core's language table, which no verb answers; the `@file` line
 * is written with it, and a header line is only taken when its hex decodes
 * to a code-projection header naming this very file. The same wrapping then
 * recognises every `@block` line.
 *
 * NOTE: THIS READER IS THE FIRST IMPLEMENTATION OF THE MAP ON THIS SIDE. The
 * core checks a diagnostic's block against the range it gives, but not a
 * signature's or a call's; when the core answers the map itself, a cell
 * compares the two, and this stays as the second implementation.
 */

import { coreRegexCompile, coreRegexMatches } from './core-regex';
import { answerOf, asInteger, isDotted, isList, parseAnswers } from './wire';

/*
 * ONE STRETCH OF A PROJECTED FILE, in bytes, half-open, laid out as the
 * core's `projection-encode-map` lays it out. A `source` piece is a block's
 * own bytes, and names that block. A `control` piece is a marker line with
 * the pad line feed written before it, if any, and belongs to no block; it
 * names the block whose source follows it, as the core's map does.
 */
export interface Piece {
  kind: 'source' | 'control';
  id: string | null;
  start: number;
  end: number;
}

export interface Projection {
  file: string;
  fileId: string;
  length: number;
  pieces: Piece[];
  blocks: string[];
}

export type ProjectionReading = { ok: true; projection: Projection } | { ok: false; file: string; why: string };

/*
 * THE LINES OF A FILE AS THE CORE SPLITS THEM (markers.sc, `byte-lines`):
 * each is [start, end of its content, end of the line], the content without
 * its `\n` or `\r\n`; a last line with no line feed ends at the end.
 */
function byteLines(bytes: Uint8Array): Array<[number, number, number]> {
  const out: Array<[number, number, number]> = [];
  let start = 0;
  for (let i = 0; i < bytes.length; i += 1) {
    if (bytes[i] === 10) {
      const contentEnd = i > start && bytes[i - 1] === 13 ? i - 1 : i;
      out.push([start, contentEnd, i + 1]);
      start = i + 1;
    }
  }
  if (start < bytes.length) {
    out.push([start, bytes.length, bytes.length]);
  }
  return out;
}

function utf8(bytes: Uint8Array): string | null {
  try {
    return new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch {
    return null;
  }
}

/*
 * A CODING LINE, as the core recognises one: text-code.sc's pattern, run by
 * a transcription of the core's own engine (src/core-regex.ts), whose limits
 * -- 4096 characters, 30000 steps -- text-code.sc takes as "no".
 */
const CODING = coreRegexCompile('^[ \\t]*#.*coding[:=][ \\t]*[-_.A-Za-z0-9]+');

function isCodingLine(text: string): boolean {
  return coreRegexMatches(CODING, text);
}

/*
 * THE FIRST BLOCK'S PREFIX, in bytes, measured as the core's
 * `source-prefix-size` (text-code.sc) measures it: a byte-order mark, then
 * at most two lines, the first a `#!` line or a coding line, the second a
 * coding line; an ordinary `#` comment on the first line is taken only
 * when a coding line follows it. A header line is none of these (its hex
 * holds no `coding:`), and neither is a marker line (an id holds no `:` or
 * `=`), so measured over the projected file it ends where the header's
 * line, or its pad line feed, begins.
 */
export function sourcePrefixSize(bytes: Uint8Array): number {
  const bom = bytes.length >= 3 && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf ? 3 : 0;
  const body = bytes.subarray(bom);
  const lines = byteLines(body);
  let end = bom;
  let i = 0;
  let line = 0;
  while (i < lines.length && line < 2) {
    const [start, contentEnd, lineEnd] = lines[i];
    const text = utf8(body.subarray(start, contentEnd));
    const directive = text !== null && line === 0 && text.startsWith('#!');
    const coding = text !== null && isCodingLine(text);
    if (directive || coding) {
      end = bom + lineEnd;
      i += 1;
      line += 1;
    } else if (line === 0 && i + 1 < lines.length && text !== null && text.replace(/^[ \t\f]+/, '').startsWith('#')) {
      i += 1;
      line = 1;
    } else {
      break;
    }
  }
  return end;
}

/*
 * A HEADER LINE, WHEN THIS LINE COULD BE ONE: the comment's opening (ending
 * in a space, with no `@` in it), exactly one `@`, `file `, the hex, and the
 * comment's closing (nothing, or a space and a token).
 */
const HEADER_LINE = /^([^@]* )@file ([0-9a-f]+)((?: \S+)?)$/;

/*
 * THE HEADER'S DATUM FROM ITS HEX: `(code-projection 1 <store-id>
 * <file-id> ((<writer> . <seq>) ...) text <pad>)`, checked as the core's
 * `header-read` (markers.sc) checks it, text mode only. Null when the hex is
 * not such a header.
 *
 * NOTE: THE FOURTH ELEMENT IS THE FILE'S BLOCK ID, NOT ITS PATH
 * (code-project.sc, `export-code-view`). Nothing in the file says which
 * path it was written at, so the header is not matched against one.
 */
function headerOf(hex: string): { pad: number; fileId: string } | null {
  if (hex.length % 2 !== 0) {
    return null;
  }
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < bytes.length; i += 1) {
    bytes[i] = parseInt(hex.slice(2 * i, 2 * i + 2), 16);
  }
  const text = utf8(bytes);
  if (text === null) {
    return null;
  }
  let data;
  try {
    data = parseAnswers(`${text}\n`);
  } catch {
    return null;
  }
  const head = data.length === 1 ? data[0] : null;
  if (
    head === null ||
    answerOf(head, 'code-projection', { at: 5, is: 'text' }) === null ||
    !isList(head) ||
    head.length !== 7
  ) {
    return null;
  }
  const version = asInteger(head[1]);
  const pad = asInteger(head[6]);
  const fileId = head[3];
  const cut = head[4];
  /*
   * A CUT'S SEQUENCE IS ANY NON-NEGATIVE EXACT INTEGER, as `header-read`
   * takes it: the reader's integers are BigInts, and `asInteger` answers
   * null past 2^53, so it is not the test here.
   */
  const cutReads =
    isList(cut) &&
    cut.every(
      (entry) =>
        isDotted(entry) &&
        entry.items.length === 1 &&
        typeof entry.items[0] === 'string' &&
        typeof entry.tail === 'bigint' &&
        entry.tail >= BigInt(0)
    );
  if (version !== 1 || typeof head[2] !== 'string' || typeof fileId !== 'string' || !cutReads || (pad !== 0 && pad !== 1)) {
    return null;
  }
  return { pad, fileId };
}

/*
 * A MARKER LINE'S PAYLOAD UNDER THE WRAPPING: how many `@` it starts with and
 * what follows them, or null when the line is not in the marker family
 * (markers.sc, `family`). One `@` is a control line; more is escaped source.
 */
function family(content: string, prefix: string, suffix: string): { ats: number; rest: string } | null {
  if (!content.startsWith(prefix) || !content.endsWith(suffix) || content.length < prefix.length + suffix.length) {
    return null;
  }
  const inner = content.slice(prefix.length, content.length - suffix.length);
  let ats = 0;
  while (ats < inner.length && inner[ats] === '@') {
    ats += 1;
  }
  const rest = inner.slice(ats);
  if (ats === 0 || !(rest.startsWith('block ') || rest.startsWith('file '))) {
    return null;
  }
  return { ats, rest };
}

/*
 * A BLOCK MARKER'S ID AND PAD (markers.sc, `block-read`): an id of lower-case
 * letters, digits, `.`, `-` and `_`, then optionally ` pad 0` or ` pad 1`.
 */
const BLOCK_MARKER = /^block ([a-z0-9._-]{1,256})(?: pad ([01]))?$/;

/*
 * THE PIECES OF ONE PROJECTED FILE, `file` being its path as the export
 * wrote it (relative, `/` between parts), which names it in a refusal.
 */
export function readProjection(bytes: Uint8Array, file: string): ProjectionReading {
  const lines = byteLines(bytes);
  const contents = lines.map(([start, end]) => utf8(bytes.subarray(start, end)));
  /*
   * THE HEADER LINE IS WHERE THE CORE PUT IT: right after the first block's
   * prefix, or after the one pad line feed that follows a prefix not ending
   * in one. The prefix is measured here as the core measures it.
   *
   * NEVER: A HEADER CHOSEN BY WHAT A LINE LOOKS LIKE. A prefix line is kept
   * above the header whatever it holds, and a shebang or coding line can
   * hold a header followed by a marker in its own wrapping, as a whole
   * self-consistent file of another language. Two rounds of review each
   * found a layout that defeated a rule of that kind; the core's own
   * measurement has no such layout.
   */
  const prefixSize = sourcePrefixSize(bytes);
  let headerAt = lines.findIndex(([start]) => start === prefixSize);
  const blank = headerAt >= 0 && lines[headerAt][1] === lines[headerAt][0];
  if ((headerAt < 0 || blank) && bytes[prefixSize] === 10) {
    headerAt = lines.findIndex(([start]) => start === prefixSize + 1);
  }
  const headerText = headerAt >= 0 ? contents[headerAt] : null;
  const headerLine = headerText === null ? null : HEADER_LINE.exec(headerText);
  const header = headerLine === null ? null : headerOf(headerLine[2]);
  if (headerLine === null || header === null) {
    return { ok: false, file, why: 'no code-projection header line where the prefix ends' };
  }
  const prefix = headerLine[1];
  const suffix = headerLine[3];
  const prefixPad = header.pad;
  const fileId = header.fileId;
  const markers: Array<{ line: number; id: string; pad: number }> = [];
  for (let i = headerAt + 1; i < lines.length; i += 1) {
    const content = contents[i];
    const f = content === null ? null : family(content, prefix, suffix);
    if (f === null || f.ats !== 1) {
      continue;
    }
    if (f.rest.startsWith('file ')) {
      return { ok: false, file, why: `a second header line at line ${i + 1}` };
    }
    const m = BLOCK_MARKER.exec(f.rest);
    if (m === null || m[1] === '.' || m[1] === '..') {
      return { ok: false, file, why: `a block marker that does not read at line ${i + 1}` };
    }
    markers.push({ line: i, id: m[1], pad: m[2] === '1' ? 1 : 0 });
  }
  const pieces: Piece[] = [];
  const headerStart = lines[headerAt][0];
  const firstId = markers.length > 0 ? markers[0].id : null;
  /*
   * A PAD IS A LINE FEED BEFORE ITS MARKER LINE. Every line after the first
   * starts after one, so only a header on the file's first line can claim a
   * pad that is not there (the core's `unpad`, invalid-padding).
   */
  if (prefixPad === 1 && headerStart === 0) {
    return { ok: false, file, why: 'a header claiming a pad at the start of the file' };
  }
  const prefixEnd = headerStart - prefixPad;
  if (prefixEnd > 0) {
    pieces.push({ kind: 'source', id: firstId, start: 0, end: prefixEnd });
  }
  pieces.push({ kind: 'control', id: firstId, start: prefixEnd, end: lines[headerAt][2] });
  const afterHeader = lines[headerAt][2];
  const firstMarkerStart = markers.length > 0 ? lines[markers[0].line][0] : bytes.length;
  if (firstMarkerStart > afterHeader) {
    pieces.push({ kind: 'source', id: null, start: afterHeader, end: firstMarkerStart });
  }
  for (let k = 0; k < markers.length; k += 1) {
    const marker = markers[k];
    const [markerStart, , markerEnd] = lines[marker.line];
    const framing = k === 0 ? 0 : marker.pad;
    pieces.push({ kind: 'control', id: marker.id, start: markerStart - framing, end: markerEnd });
    const next = markers[k + 1];
    const end = next === undefined ? bytes.length : lines[next.line][0] - next.pad;
    pieces.push({ kind: 'source', id: marker.id, start: markerEnd, end: Math.max(markerEnd, end) });
  }
  const blocks: string[] = [];
  for (const m of markers) {
    if (!blocks.includes(m.id)) {
      blocks.push(m.id);
    }
  }
  return { ok: true, projection: { file, fileId, length: bytes.length, pieces, blocks } };
}

/*
 * THE BLOCK A BYTE OF THE FILE BELONGS TO: the block whose own bytes hold
 * it; the last block for the end of the file; null inside a marker line or
 * a pad line feed, where no block's source is.
 */
export function blockAt(projection: Projection, byte: number): string | null {
  for (const piece of projection.pieces) {
    if (piece.kind === 'source' && piece.start <= byte && byte < piece.end) {
      return piece.id;
    }
  }
  if (byte === projection.length) {
    for (let i = projection.pieces.length - 1; i >= 0; i -= 1) {
      const piece = projection.pieces[i];
      if (piece.kind === 'source' && piece.id !== null) {
        return piece.id;
      }
    }
  }
  return null;
}
