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
 * A SPLIT OF A SOURCE FILE, CUT WHERE THE EDITOR'S SYMBOLS START.
 *
 * The core's `split-suggest <file>` proposes where a file could be divided
 * into blocks and writes the proposal as a review file; it records nothing.
 * With `--symbols <file>` the question "does a definition start on this
 * line" is answered by a list of the file's top-level symbols instead of
 * the language's definition patterns. The editor has that list: its symbol
 * providers answer for the document. This file turns their answer into the
 * core's symbols file and reads the core's answer back.
 *
 * The symbols file, one datum per line:
 *
 *   (symbols (digest "<sha256 of the file>") (source (vscode "<version>" "<languageId>")) (top-level #t))
 *   (symbol <start> <end> <kind> "<name>")
 *
 * `<start>` and `<end>` are BYTE offsets into the file exactly as it is on
 * disk, and `<start>` is the first byte of its line. The digest is of those
 * same bytes, so the core refuses a list made from anything else
 * (`symbols-stale`). The editor's positions are lines and UTF-16 code units
 * in ITS text, which is not the file: it has no byte-order mark and may
 * hold unsaved edits. So everything is computed on the bytes read back
 * from disk after saving, and the editor is asked, after collecting, whether
 * its text is still exactly those bytes.
 *
 * Nothing in this file waits on the editor; `runSplit` is handed what it
 * needs, and the command in extension.ts hands it the editor's.
 */

import { createHash } from 'crypto';
import { Answer, Client } from './client';
import { SETTINGS_REFUSALS } from './saver';
import { TransportError } from './transport';
import { Datum, answerOf, asInteger, isList, isSym, wire } from './wire';

/*
 * VS CODE'S SymbolKind NAMES, BY THEIR VALUE, as the core spells them:
 * lower-cased, one word. The enum runs from File = 0 to TypeParameter = 25;
 * the core refuses any other name as a malformed line.
 */
export const SYMBOL_KIND_NAMES: readonly string[] = [
  'file',
  'module',
  'namespace',
  'package',
  'class',
  'method',
  'property',
  'field',
  'constructor',
  'enum',
  'interface',
  'function',
  'variable',
  'constant',
  'string',
  'number',
  'boolean',
  'array',
  'object',
  'key',
  'null',
  'enummember',
  'struct',
  'event',
  'operator',
  'typeparameter'
];

export interface PositionLike {
  line: number;
  character: number;
}

export interface RangeLike {
  start: PositionLike;
  end: PositionLike;
}

/*
 * ONE TOP-LEVEL SYMBOL AS A PROVIDER GAVE IT, in the editor's coordinates.
 */
export interface ProvidedSymbol {
  name: string;
  kind: number;
  range: RangeLike;
}

/*
 * ONE LINE OF THE SYMBOLS FILE: byte offsets into the file on disk.
 */
export interface FileSymbol {
  start: number;
  end: number;
  kind: string;
  name: string;
}

/*
 * THE FILE AS THE EDITOR SEES IT, from the bytes on disk: the text without
 * a leading byte-order mark (the editor does not show one), and how many
 * bytes that mark took. Null for bytes that are not UTF-8, which no
 * position the editor gives can be converted against.
 */
export interface SavedText {
  text: string;
  bom: number;
}

export function savedText(bytes: Uint8Array): SavedText | null {
  let text: string;
  try {
    text = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch {
    return null;
  }
  if (text.startsWith('\ufeff')) {
    return { text: text.slice(1), bom: 3 };
  }
  return { text, bom: 0 };
}

/*
 * WHERE EACH LINE STARTS, as the editor numbers lines: a line ends at
 * `\r\n`, at `\n`, or at a `\r` alone. Offsets are UTF-16 code units into
 * the text.
 */
export function lineStarts(text: string): number[] {
  const starts = [0];
  for (let i = 0; i < text.length; i += 1) {
    const c = text.charCodeAt(i);
    if (c === 13) {
      if (text.charCodeAt(i + 1) === 10) {
        i += 1;
      }
      starts.push(i + 1);
    } else if (c === 10) {
      starts.push(i + 1);
    }
  }
  return starts;
}

/*
 * THE BYTE OFFSET IN THE FILE OF AN EDITOR POSITION: the mark's bytes, then
 * the UTF-8 length of the text before the position. A position past the end
 * of its line is taken at the line's end, as the editor itself validates
 * one; a line past the last is the end of the text.
 *
 * NEVER: THE START OF THE FIRST LINE IS NOT AFTER THE MARK. The mark belongs
 * to line 0, so that line starts at byte 0 of the file; answering the
 * mark's length there sent `(symbol 3 ...)`, which the core refuses as not
 * a line start (the byte before it is the mark's, not a line feed). Every
 * other position on line 0 is after the mark.
 */
export function byteOffset(saved: SavedText, starts: number[], at: PositionLike): number {
  let unit: number;
  if (at.line >= starts.length) {
    unit = saved.text.length;
  } else {
    const lineStart = starts[at.line];
    const next = at.line + 1 < starts.length ? starts[at.line + 1] : saved.text.length;
    let lineEnd = next;
    while (lineEnd > lineStart && (saved.text.charCodeAt(lineEnd - 1) === 10 || saved.text.charCodeAt(lineEnd - 1) === 13)) {
      lineEnd -= 1;
    }
    unit = Math.min(lineStart + Math.max(0, at.character), lineEnd);
  }
  /*
   * NEVER: A POSITION BETWEEN THE TWO HALVES OF A SURROGATE PAIR. One UTF-16
   * unit of a character outside the basic plane has no UTF-8 bytes of its
   * own: counting it gave an offset inside the character, which the core
   * refuses as not a boundary. Such a position is taken after the pair.
   */
  const at16 = (i: number): number => saved.text.charCodeAt(i);
  if (unit > 0 && unit < saved.text.length && at16(unit - 1) >= 0xd800 && at16(unit - 1) <= 0xdbff && at16(unit) >= 0xdc00 && at16(unit) <= 0xdfff) {
    unit += 1;
  }
  if (unit === 0) {
    return 0;
  }
  return saved.bom + Buffer.byteLength(saved.text.slice(0, unit), 'utf8');
}

/*
 * THE TOP LEVEL OF A PROVIDER'S ANSWER, in either of its two forms.
 *
 * `DocumentSymbol[]` is a tree: its roots are the top level. The flat
 * `SymbolInformation[]` says a symbol's parent by `containerName`, and its
 * location can name another file: the top level is the entries with no
 * container whose location is this document. The editor merges every
 * provider's answer, so both forms can arrive in one list.
 */
export function topLevelSymbols(answer: unknown, documentUri: string): ProvidedSymbol[] {
  if (!Array.isArray(answer)) {
    return [];
  }
  const out: ProvidedSymbol[] = [];
  for (const item of answer) {
    if (typeof item !== 'object' || item === null) {
      continue;
    }
    const s = item as {
      name?: unknown;
      kind?: unknown;
      range?: RangeLike;
      containerName?: unknown;
      location?: { uri?: { toString(): string }; range?: RangeLike };
    };
    if (typeof s.name !== 'string' || typeof s.kind !== 'number') {
      continue;
    }
    if (s.location !== undefined) {
      const container = typeof s.containerName === 'string' ? s.containerName : '';
      const here = s.location.uri !== undefined && s.location.uri.toString() === documentUri;
      if (container === '' && here && s.location.range !== undefined) {
        out.push({ name: s.name, kind: s.kind, range: s.location.range });
      }
    } else if (s.range !== undefined) {
      out.push({ name: s.name, kind: s.kind, range: s.range });
    }
  }
  return out;
}

/*
 * THE LINES OF THE SYMBOLS FILE, from the top-level symbols and the file
 * read back from disk.
 *
 * Each symbol is sent at the first byte of its LINE: a provider's range
 * starts at the keyword's column, and the core cuts at lines. Two symbols
 * that start on one line (`const a = 1, b = 2;`) are one start, and the
 * core refuses a start given twice, so they are sent as one line: the
 * first one's kind, both names. A symbol's end is where its range ends,
 * but not past the next symbol's start, since the core refuses ranges that
 * overlap and a range that ends on the line the next one starts is common.
 * A symbol whose kind has no name the core knows, or whose range is empty
 * at its line start, is left out and counted, so the message can say so.
 */
export function fileSymbols(
  saved: SavedText,
  provided: ProvidedSymbol[]
): { symbols: FileSymbol[]; left: number } {
  const starts = lineStarts(saved.text);
  const converted: FileSymbol[] = [];
  let left = 0;
  for (const p of provided) {
    const kind = SYMBOL_KIND_NAMES[p.kind];
    if (kind === undefined) {
      left += 1;
      continue;
    }
    const start = byteOffset(saved, starts, { line: p.range.start.line, character: 0 });
    const end = byteOffset(saved, starts, p.range.end);
    if (end <= start) {
      left += 1;
      continue;
    }
    converted.push({ start, end, kind, name: p.name });
  }
  converted.sort((a, b) => a.start - b.start || a.end - b.end);
  const merged: FileSymbol[] = [];
  for (const s of converted) {
    const last = merged[merged.length - 1];
    if (last !== undefined && last.start === s.start) {
      last.end = Math.max(last.end, s.end);
      last.name = `${last.name}, ${s.name}`;
    } else {
      merged.push({ ...s });
    }
  }
  for (let i = 0; i + 1 < merged.length; i += 1) {
    merged[i].end = Math.min(merged[i].end, merged[i + 1].start);
  }
  return { symbols: merged, left };
}

export function digestOf(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

/*
 * THE SYMBOLS FILE'S TEXT, printed by the wire's own writer so that a name
 * holding a quote or a backslash reads back as itself.
 *
 * NEVER: THE OFFSETS AS JS NUMBERS. The writer prints a number as an
 * inexact float (`#f8"..."`), and the core refuses a line whose offsets are
 * not exact integers (`symbols-malformed`) -- measured on the pinned core.
 * They go as BigInt, which it prints as integers.
 */
export function symbolsFileText(digest: string, version: string, languageId: string, symbols: FileSymbol[]): string {
  const w = wire();
  const header = w.write([
    w.sym('symbols'),
    [w.sym('digest'), digest],
    [w.sym('source'), [w.sym('vscode'), version, languageId]],
    [w.sym('top-level'), true]
  ]);
  return [header, ...symbols.map(symbolLine)].map((l) => `${l}\n`).join('');
}

/*
 * ONE SYMBOL LINE, `(symbol <start> <end> <kind> "<name>")`, for this file's
 * symbols file and for an import's (src/import-symbols.ts), which has the
 * same lines under a header per file.
 */
export function symbolLine(s: FileSymbol): string {
  const w = wire();
  return w.write([w.sym('symbol'), BigInt(s.start), BigInt(s.end), w.sym(s.kind), s.name]);
}

/*
 * THE CORE'S ANSWER TO A SPLIT: the review file, the byte offsets where
 * the blocks start, the warnings clause as it came, and which supplier
 * chose the cuts. A refusal is carried as its datum, to be shown as the
 * core wrote it.
 */
export type SplitAnswer =
  | { ok: true; review: string; boundaries: number[]; warnings: Datum; cutsFrom: Datum }
  | { ok: false; refusal: Datum };

export function readSplitAnswer(answer: Answer): SplitAnswer {
  const datum = answer.answers.length > 0 ? answer.answers[0] : null;
  if (!answer.ok) {
    if (datum !== null && answerOf(datum, 'error') !== null) {
      return { ok: false, refusal: datum };
    }
    throw new TransportError('unreadable', 'the core refused the split without saying why', answer.text);
  }
  const form = datum === null ? null : answerOf(datum, 'ok');
  if (form === null) {
    throw new TransportError('unreadable', 'the core answered the split with something that is not an ok form', answer.text);
  }
  const review = form.value('working-path');
  const boundaries = form.value('boundaries');
  const warnings = form.value('warnings');
  const cutsFrom = form.value('cuts-from');
  if (!review.read || typeof review.value !== 'string') {
    throw new TransportError('unreadable', 'the core answered the split without the review file it wrote', answer.text);
  }
  /*
   * The reader answers an integer as a BigInt; `asInteger` is the one
   * conversion, and answers null for anything that is not an integer.
   */
  const offsets = boundaries.read && isList(boundaries.value) ? boundaries.value.map((n) => asInteger(n)) : null;
  if (offsets === null || offsets.some((n) => n === null)) {
    throw new TransportError('unreadable', 'the core answered the split with boundaries this client cannot read', answer.text);
  }
  if (!warnings.read || !isList(warnings.value) || !cutsFrom.read) {
    throw new TransportError('unreadable', 'the core answered the split without its warnings or its cuts-from', answer.text);
  }
  return {
    ok: true,
    review: review.value,
    boundaries: offsets as number[],
    warnings: warnings.value,
    cutsFrom: cutsFrom.value
  };
}

/*
 * WHAT IS SAID BESIDE THE OPENED REVIEW: the number of blocks, who chose the
 * cuts, the whole warnings clause, and the symbols this client left out.
 */
export function splitNotice(split: { boundaries: number[]; warnings: Datum; cutsFrom: Datum }, left: number): string {
  const w = wire();
  const from = isSym(split.cutsFrom) ? split.cutsFrom.name : w.write(split.cutsFrom);
  const warnings = isList(split.warnings) && split.warnings.length === 0 ? 'no warnings' : `warnings ${w.write(split.warnings)}`;
  const leftOut = left === 0 ? '' : `; ${left} symbol${left === 1 ? '' : 's'} the editor gave had no line to start at and were left out`;
  return `${split.boundaries.length} block${split.boundaries.length === 1 ? '' : 's'} proposed, cuts from ${from}; ${warnings}${leftOut}`;
}

/*
 * A REFUSED SPLIT, AS THE CORE WROTE IT, with the sentence its kind has in
 * the refusal table when it has one. Nothing is retried: the refusal says
 * the input was wrong, and the same input gets the same answer.
 */
export function splitRefusalNotice(refusal: Datum): string {
  const said = wire().write(refusal);
  const kind = isList(refusal) && refusal.length > 1 && isSym(refusal[1]) ? refusal[1].name : null;
  const sentence = kind === null ? undefined : SETTINGS_REFUSALS[kind];
  return sentence === undefined ? `the core refused the split: ${said}` : `the core refused the split: ${said}. ${sentence}`;
}

/*
 * WHAT THE COMMAND IS HANDED: the document, and the ways to reach the
 * editor, the disk and the core. The command in extension.ts supplies the
 * real ones; the cells supply stand-ins.
 */
export interface SplitDocument {
  uri: string;
  fsPath: string;
  languageId: string;
  isDirty(): boolean;
  version(): number;
  text(): string;
  save(): Promise<boolean>;
}

/*
 * `symbolsPath` names the file before anything is written to it, so that a
 * write that fails after creating it is still removed: the removal covers
 * the write as well as the request. `removeSymbols` is given that path
 * whether or not the file came to exist.
 */
export interface SplitDeps {
  client: Client;
  editorVersion: string;
  readFile(fsPath: string): Uint8Array;
  symbols(): Promise<unknown>;
  symbolsPath(): string;
  writeSymbols(path: string, text: string): void;
  removeSymbols(path: string): void;
}

export type SplitOutcome =
  | { done: 'split'; review: string; boundaries: number[]; notice: string; argv: string[] }
  | { done: 'refused'; refusal: Datum; argv: string[] }
  | { done: 'stopped'; why: string };

export const COULD_NOT_SAVE = 'could not save the file, so there is nothing on disk the symbols can be counted against';
export const CHANGED_WHILE_COLLECTING = 'the file changed while its symbols were being collected; nothing was sent';

/*
 * THE COMMAND'S STEPS.
 *
 * Save a dirty document first, and stop if the save did not happen. Read
 * the saved file back from disk, collect the symbols, convert them on
 * what was read back; then ask the editor again whether the document is
 * still that text, unchanged and unsaved-free, and stop if not: an external
 * write that reached the disk and not yet the editor would otherwise pair
 * positions from one text with bytes from another. With no symbols the
 * split runs without `--symbols`, and the answer says the cuts came from
 * the language's patterns. The symbols file is removed however the request
 * ends.
 */
export async function runSplit(doc: SplitDocument, deps: SplitDeps): Promise<SplitOutcome> {
  if (doc.isDirty()) {
    const saved = await doc.save();
    if (!saved || doc.isDirty()) {
      return { done: 'stopped', why: COULD_NOT_SAVE };
    }
  }
  const version = doc.version();
  const bytes = deps.readFile(doc.fsPath);
  const saved = savedText(bytes);
  if (saved === null) {
    return { done: 'stopped', why: 'the file on disk is not UTF-8, so the editor\'s positions cannot be counted in it' };
  }
  const provided = topLevelSymbols(await deps.symbols(), doc.uri);
  const { symbols, left } = fileSymbols(saved, provided);
  if (doc.isDirty() || doc.version() !== version || doc.text() !== saved.text) {
    return { done: 'stopped', why: CHANGED_WHILE_COLLECTING };
  }
  let args = [doc.fsPath];
  let written: string | null = null;
  let answer: Answer;
  try {
    if (symbols.length > 0) {
      written = deps.symbolsPath();
      deps.writeSymbols(written, symbolsFileText(digestOf(bytes), deps.editorVersion, doc.languageId, symbols));
      args = [doc.fsPath, '--symbols', written];
    }
    answer = await deps.client.request('split-suggest', args);
  } finally {
    if (written !== null) {
      deps.removeSymbols(written);
    }
  }
  const read = readSplitAnswer(answer);
  if (!read.ok) {
    return { done: 'refused', refusal: read.refusal, argv: ['split-suggest', ...args] };
  }
  return {
    done: 'split',
    review: read.review,
    boundaries: read.boundaries,
    notice: splitNotice(read, left),
    argv: ['split-suggest', ...args]
  };
}
