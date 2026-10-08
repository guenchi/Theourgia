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
 * AN IMPORT SPLIT WHERE THE EDITOR'S SYMBOLS ARE.
 *
 * The core's `import-code <dir> --symbols <file>` splits the first import of
 * each file the symbols file names at its symbols, into the blocks a marked
 * import of the same cuts makes, and imports every other file as before. A
 * file that already carries markers follows them, and is listed as
 * `symbols-ignored`; a file whose cuts the core refuses is not imported, and
 * is listed with its refusal as `symbols-refused`. The editor has the
 * symbols: its providers answer for each document. This file collects them
 * for every file under the directory, writes the core's symbols file, runs
 * the import, and reads back where each new block lies in its file.
 *
 * The symbols file is split-suggest's, one section per file:
 *
 *   (symbols (path "<path under dir>") (digest "<sha256 of the file>") (source (vscode "<version>" "<languageId>")) (top-level #t))
 *   (symbol <start> <end> <kind> "<name>")
 *
 * The positions are counted as src/split-symbols.ts counts them, on the
 * bytes read back from disk: a start at the first byte of its line, an end
 * where the range ends, not past the next start (the core takes an end to
 * the start of the line after it). The digest is of those same bytes.
 *
 * NEVER: A FILE WITH UNSAVED EDITS IS NOT IMPORTED. The core reads the disk,
 * and the editor's symbols describe its own text; with unsaved edits the
 * two are different files. Such a file stops the whole command, and the
 * message names it; nothing is sent.
 *
 * Nothing in this file waits on the editor; `runImport` is handed what it
 * needs, and the command in extension.ts hands it the editor's.
 */

import { Answer, Client, Note } from './client';
import { readBlock } from './blocks';
import { readProjection } from './markers';
import {
  CHANGED_WHILE_COLLECTING,
  FileSymbol,
  PositionLike,
  SavedText,
  digestOf,
  fileSymbols,
  lineStarts,
  savedText,
  symbolLine,
  topLevelSymbols
} from './split-symbols';
import { TransportError } from './transport';
import { Datum, answerOf, asInteger, isList, isSym, readEvent, wire } from './wire';

/*
 * ONE FILE UNDER THE DIRECTORY: its path as the core's walk spells it (`/`
 * between parts, relative to the directory), the file the core will read,
 * and the address of the editor's document for it. For a folder the two
 * are one file; for a single file imported alone they are the copy the
 * command made and the original the editor shows.
 */
export interface ImportSource {
  rel: string;
  bytesPath: string;
  uri: string;
}

/*
 * THE EDITOR'S DOCUMENT FOR A FILE, opened when it was not.
 */
export interface ImportDocument {
  languageId: string;
  isDirty(): boolean;
  version(): number;
  text(): string;
}

/*
 * THE DISK A RUN USES FOR THE EXPORT THAT PLACES THE NEW BLOCKS: a fresh
 * directory, a file's bytes in it, and its removal (fsops' `scratchUnder`).
 */
export interface ImportScratch {
  make(): string;
  read(directory: string, file: string): Uint8Array;
  remove(directory: string): void;
}

/*
 * `symbolsPath` names the file before anything is written to it, so that a
 * write that fails after creating it is still removed, as for a split.
 */
export interface ImportDeps {
  client: Client;
  editorVersion: string;
  directory: string;
  sources: ImportSource[];
  readFile(fsPath: string): Uint8Array;
  document(source: ImportSource): Promise<ImportDocument | null>;
  symbols(source: ImportSource): Promise<unknown>;
  symbolsPath(): string;
  writeSymbols(path: string, text: string): void;
  removeSymbols(path: string): void;
  scratch: ImportScratch;
}

/*
 * ONE FILE'S SECTION, and what it was counted against: the file read back
 * from disk, so that a byte the core names can be turned back into a line.
 */
interface Section {
  rel: string;
  bytes: Uint8Array;
  digest: string;
  languageId: string;
  symbols: FileSymbol[];
  saved: SavedText;
  left: number;
}

export interface EditorRange {
  start: PositionLike;
  end: PositionLike;
}

/*
 * A FILE THE IMPORT SPLIT: its file block, and each of its blocks with the
 * range of the file it holds. `placed` is null with the reason when the
 * blocks could not be placed; the import itself stands.
 */
export interface SplitFile {
  rel: string;
  placed: { fileId: string; blocks: Array<{ id: string; range: EditorRange }> } | { why: string };
}

/*
 * A FILE THE CORE REFUSED TO SPLIT, with its refusal as the core wrote it
 * and, when the refusal names a byte (`(at <n>)`), where that byte is in the
 * file as the editor shows it.
 */
export interface RefusedFile {
  rel: string;
  refusal: Datum;
  at: PositionLike | null;
}

export type ImportOutcome =
  | {
      done: 'imported';
      split: SplitFile[];
      ignored: string[];
      refused: RefusedFile[];
      skipped: string[];
      left: number;
      argv: string[];
    }
  | { done: 'refused'; refusal: Datum; notes: Note[] | null; argv: string[] }
  | { done: 'stopped'; why: string };

export const UNSAVED = 'has unsaved edits, and the core imports the file on disk: save it first; nothing was imported';
export const NO_SYMBOLS = 'the editor gave no top-level symbols for any file there, so nothing would be split; nothing was imported';

/*
 * THE SYMBOLS FILE'S TEXT: a header per file, naming it by its path, then
 * its symbol lines (split-symbols.ts, symbolLine).
 */
export function importSymbolsText(version: string, sections: Array<{ rel: string; digest: string; languageId: string; symbols: FileSymbol[] }>): string {
  const w = wire();
  const lines: string[] = [];
  for (const s of sections) {
    lines.push(
      w.write([
        w.sym('symbols'),
        [w.sym('path'), s.rel],
        [w.sym('digest'), s.digest],
        [w.sym('source'), [w.sym('vscode'), version, s.languageId]],
        [w.sym('top-level'), true]
      ])
    );
    for (const symbol of s.symbols) {
      lines.push(symbolLine(symbol));
    }
  }
  return lines.map((l) => `${l}\n`).join('');
}

/*
 * THE EDITOR POSITION OF A BYTE OF THE FILE ON DISK: the inverse of
 * split-symbols' `byteOffset`. A byte inside the byte-order mark is the
 * start of the file; a byte inside a character is that character's start;
 * a byte past the end is the end.
 */
export function positionAtByte(saved: SavedText, starts: number[], byte: number): PositionLike {
  const target = byte - saved.bom;
  let unit = 0;
  let bytes = 0;
  while (unit < saved.text.length && bytes < target) {
    const code = saved.text.codePointAt(unit) as number;
    const width = code < 0x80 ? 1 : code < 0x800 ? 2 : code < 0x10000 ? 3 : 4;
    if (bytes + width > target) {
      break;
    }
    bytes += width;
    unit += code >= 0x10000 ? 2 : 1;
  }
  let line = 0;
  while (line + 1 < starts.length && starts[line + 1] <= unit) {
    line += 1;
  }
  /*
   * NEVER: A POSITION INSIDE A LINE'S END. Both bytes of a CRLF, as a lone
   * CR or LF, are the end of their line: the LF of "a\r\nb" is not column 2
   * of a line whose text ends at column 1.
   */
  let end = line + 1 < starts.length ? starts[line + 1] : saved.text.length;
  while (end > starts[line] && (saved.text.charCodeAt(end - 1) === 10 || saved.text.charCodeAt(end - 1) === 13)) {
    end -= 1;
  }
  return { line, character: Math.min(unit, end) - starts[line] };
}

/*
 * A CLAUSE OF THE IMPORT'S ANSWER THAT IS A LIST, or the empty list when it
 * is absent: the core writes `skipped`, `symbols-ignored` and
 * `symbols-refused` only when they list something.
 */
function listedClause(envelope: Datum, name: string, text: string): Datum[] {
  const form = answerOf(envelope, 'ok');
  if (form === null) {
    throw new TransportError('unreadable', 'the core answered the import with something that is not an ok form', text);
  }
  const value = form.value(name);
  if (!value.read) {
    if (value.because === 'absent') {
      return [];
    }
    throw new TransportError('unreadable', `the core answered the import with a ${name} clause this client cannot read`, text);
  }
  if (!isList(value.value)) {
    throw new TransportError('unreadable', `the core answered the import with a ${name} clause that is not a list`, text);
  }
  return value.value;
}

function names(items: Datum[], clause: string, text: string): string[] {
  if (!items.every((p) => typeof p === 'string')) {
    throw new TransportError('unreadable', `the core answered the import with a ${clause} clause that is not a list of paths`, text);
  }
  return items as string[];
}

/*
 * THE BLOCKS THE IMPORT WROTE, by id: each item of its answer is the answer
 * of one record, `(ok (events ((<writer> . <seq>))) (state ((<id> . <hash>)
 * ...)) (cursor (<writer> . <seq>)) (replay #f))` as the core's write path
 * gives it for each intent of the import's plan (store.sc, run-intents!),
 * and a block made by an insert has the id `<writer>.<seq in base 36>` of
 * its record, as the core itself names it there and a save names the block
 * it made. `(event ...)` is read as well, the shape of a single record.
 *
 * NOTE: ONLY CREATES ARE ASKED ABOUT. A set, move or delete of a block the
 * store holds has an event that is no block's id, so it adds nothing a file
 * is looked up by. That is enough: the only files placed are ones the
 * symbols split, and the core splits a file only on its first import, one
 * with no marker line, whose file block and blocks are all inserted. A file
 * that carries markers -- an export, a re-import -- is updated in place and
 * listed as symbols-ignored, and is never placed.
 */
export function writtenIds(items: Datum[]): Set<string> {
  const ids = new Set<string>();
  const add = (value: Datum): void => {
    const event = readEvent(value);
    if (event !== null) {
      ids.add(`${event.writer}.${event.seq.toString(36)}`);
    }
  };
  for (const item of items) {
    const form = answerOf(item, 'ok');
    if (form === null) {
      continue;
    }
    const one = form.value('event');
    if (one.read) {
      add(one.value);
    }
    const many = form.value('events');
    if (many.read && isList(many.value)) {
      many.value.forEach(add);
    }
  }
  return ids;
}

/*
 * WHERE EACH BLOCK OF A SPLIT FILE LIES. The export names the file's block
 * and its blocks in order (markers.ts, readProjection); a recursive read of
 * the file block gives each block's src; the srcs run together to the file,
 * so each block's range is the sum of the lengths before it.
 *
 * NEVER: BLOCKS THIS IMPORT DID NOT WRITE. The export shows the store as it
 * is, so a file block the import did not make -- one already there under
 * the path -- would be placed as though it had just been split; a file
 * whose block, or any of whose blocks, is not among the ids the import's
 * own records made is said to be already present. And the blocks' bytes,
 * run together, are compared with the file's bytes, not only counted: the
 * same length with other content would place wrong ranges.
 */
async function place(
  client: Client,
  exported: string,
  scratch: ImportScratch,
  section: Section,
  written: Set<string>
): Promise<SplitFile['placed']> {
  let bytes: Uint8Array;
  try {
    bytes = scratch.read(exported, section.rel);
  } catch (e) {
    return { why: `the export wrote no ${section.rel}: ${String(e)}` };
  }
  const reading = readProjection(bytes, section.rel, 'text');
  if (!reading.ok) {
    return { why: `the export of ${section.rel} does not read: ${reading.why}` };
  }
  const { fileId, blocks } = reading.projection;
  if (![fileId, ...blocks].every((id) => written.has(id))) {
    return { why: `the store already held ${fileId} for ${section.rel}, and this import wrote none of its blocks` };
  }
  const answer: Answer = await client.request('read', [fileId, '--recursive', '--wire']);
  if (!answer.ok) {
    return { why: `the store would not read ${fileId}: ${answer.text.trim()}` };
  }
  const srcs = new Map<string, Uint8Array>();
  for (const item of answer.answers) {
    const block = readBlock(item);
    if (block === null) {
      continue;
    }
    const src = block.fields.get('src');
    if (src instanceof Uint8Array) {
      srcs.set(block.id, src);
    } else if (typeof src === 'string') {
      srcs.set(block.id, Buffer.from(src, 'utf8'));
    }
  }
  const starts = lineStarts(section.saved.text);
  const placed: Array<{ id: string; range: EditorRange }> = [];
  const held: Uint8Array[] = [];
  let at = 0;
  for (const id of blocks) {
    const src = srcs.get(id);
    if (src === undefined) {
      return { why: `the read of ${fileId} did not give the src of ${id}` };
    }
    placed.push({
      id,
      range: { start: positionAtByte(section.saved, starts, at), end: positionAtByte(section.saved, starts, at + src.length) }
    });
    held.push(src);
    at += src.length;
  }
  if (!Buffer.concat(held).equals(Buffer.from(section.bytes))) {
    return { why: `the blocks of ${fileId}, run together, are not the bytes of ${section.rel}` };
  }
  return { fileId, blocks: placed };
}

/*
 * THE COMMAND'S STEPS.
 *
 * Every document first: one with unsaved edits stops the command. Then, for
 * each file that is UTF-8 text, its bytes are read back from disk, its
 * document is asked whether it is still exactly those bytes, its top-level
 * symbols are collected and converted, and the document is asked again; a
 * change between stops the command. With no symbols for any file nothing is
 * sent. The symbols file is removed however the request ends. A refusal of
 * the whole import is carried as the core wrote it; otherwise the answer's
 * lists are read, and each split file's blocks are placed by an export to a
 * scratch directory, which is removed however that ends.
 */
export async function runImport(deps: ImportDeps): Promise<ImportOutcome> {
  const documents: Array<{ source: ImportSource; bytes: Uint8Array; saved: SavedText; document: ImportDocument }> = [];
  const dirty: string[] = [];
  for (const source of deps.sources) {
    const bytes = deps.readFile(source.bytesPath);
    const saved = savedText(bytes);
    if (saved === null) {
      continue;
    }
    const document = await deps.document(source);
    if (document === null) {
      continue;
    }
    if (document.isDirty()) {
      dirty.push(source.rel);
      continue;
    }
    documents.push({ source, bytes, saved, document });
  }
  if (dirty.length > 0) {
    return { done: 'stopped', why: `${dirty.join(', ')} ${UNSAVED}` };
  }
  const sections: Section[] = [];
  for (const { source, bytes, saved, document } of documents) {
    const version = document.version();
    if (document.text() !== saved.text) {
      return { done: 'stopped', why: CHANGED_WHILE_COLLECTING };
    }
    const provided = topLevelSymbols(await deps.symbols(source), source.uri);
    const { symbols, left } = fileSymbols(saved, provided);
    if (document.isDirty() || document.version() !== version || document.text() !== saved.text) {
      return { done: 'stopped', why: CHANGED_WHILE_COLLECTING };
    }
    if (symbols.length > 0) {
      sections.push({ rel: source.rel, bytes, digest: digestOf(bytes), languageId: document.languageId, symbols, saved, left });
    }
  }
  if (sections.length === 0) {
    return { done: 'stopped', why: NO_SYMBOLS };
  }
  const written = deps.symbolsPath();
  const args = [deps.directory, '--symbols', written, '--wire'];
  let answer: Answer;
  try {
    deps.writeSymbols(written, importSymbolsText(deps.editorVersion, sections));
    answer = await deps.client.request('import-code', args);
  } finally {
    deps.removeSymbols(written);
  }
  const argv = ['import-code', ...args];
  if (!answer.ok) {
    const datum = answer.answers.length > 0 ? answer.answers[0] : null;
    if (datum !== null && answerOf(datum, 'error') !== null) {
      return { done: 'refused', refusal: datum, notes: answer.notes ?? null, argv };
    }
    throw new TransportError('unreadable', 'the core refused the import without saying why', answer.text);
  }
  if (answer.envelope === null) {
    throw new TransportError('unreadable', 'the core answered the import without its envelope', answer.text);
  }
  const skipped = names(listedClause(answer.envelope, 'skipped', answer.text), 'skipped', answer.text);
  const ignored = names(listedClause(answer.envelope, 'symbols-ignored', answer.text), 'symbols-ignored', answer.text);
  const refused: RefusedFile[] = [];
  for (const entry of listedClause(answer.envelope, 'symbols-refused', answer.text)) {
    if (!isList(entry) || entry.length !== 2 || typeof entry[0] !== 'string' || answerOf(entry[1], 'error') === null) {
      throw new TransportError('unreadable', 'the core answered the import with a refusal this client cannot read', answer.text);
    }
    const rel = entry[0] as string;
    const named = answerOf(entry[1], 'error')?.value('at');
    const byte = named !== undefined && named.read ? asInteger(named.value) : null;
    const section = sections.find((s) => s.rel === rel);
    refused.push({
      rel,
      refusal: entry[1],
      at: byte === null || section === undefined ? null : positionAtByte(section.saved, lineStarts(section.saved.text), byte)
    });
  }
  /*
   * A FILE THE CORE SKIPPED IS NOT PLACED: one holding a NUL byte is UTF-8
   * to this client and not text to the core.
   */
  const splitSections = sections.filter(
    (s) => !ignored.includes(s.rel) && !skipped.includes(s.rel) && !refused.some((r) => r.rel === s.rel)
  );
  const made = writtenIds(answer.answers);
  const split: SplitFile[] = [];
  if (splitSections.length > 0) {
    const exported = deps.scratch.make();
    try {
      const projection = await deps.client.request('export-code', [exported]);
      for (const section of splitSections) {
        split.push({
          rel: section.rel,
          placed: projection.ok
            ? await place(deps.client, exported, deps.scratch, section, made)
            : { why: `the store would not write its export: ${projection.text.trim()}` }
        });
      }
    } finally {
      deps.scratch.remove(exported);
    }
  }
  return {
    done: 'imported',
    split,
    ignored,
    refused,
    skipped,
    left: sections.reduce((n, s) => n + s.left, 0),
    argv
  };
}

/*
 * LINE AND COLUMN AS A PERSON READS THEM: both counted from 1, the column in
 * the editor's characters.
 */
function where(at: PositionLike): string {
  return `line ${at.line + 1}, column ${at.character + 1}`;
}

/*
 * WHAT THE COMMAND SAYS, one line each: every split file with each block's
 * id and the lines it holds, the files that followed their markers, the
 * refusals by file with the place a byte names, and the files the core
 * skipped as not text.
 */
export function importReport(outcome: Extract<ImportOutcome, { done: 'imported' }>): string[] {
  const w = wire();
  const lines: string[] = [];
  for (const file of outcome.split) {
    if ('why' in file.placed) {
      lines.push(`${file.rel}: imported, split at its symbols; its blocks could not be placed: ${file.placed.why}`);
      continue;
    }
    lines.push(`${file.rel}: imported as ${file.placed.fileId}, ${file.placed.blocks.length} block${file.placed.blocks.length === 1 ? '' : 's'}`);
    for (const block of file.placed.blocks) {
      lines.push(`  ${block.id}: from ${where(block.range.start)} to ${where(block.range.end)}`);
    }
  }
  for (const rel of outcome.ignored) {
    lines.push(`${rel}: carries markers, so it followed them; its symbols were not used (symbols-ignored)`);
  }
  for (const refused of outcome.refused) {
    const kind = isList(refused.refusal) && refused.refusal.length > 1 && isSym(refused.refusal[1]) ? refused.refusal[1].name : 'refused';
    const at = refused.at === null ? '' : ` at ${where(refused.at)}`;
    lines.push(`${refused.rel}: not imported, the core refused its symbols (${kind})${at}: ${w.write(refused.refusal)}`);
  }
  for (const rel of outcome.skipped) {
    lines.push(`${rel}: skipped by the core, not UTF-8 text or holding a NUL byte`);
  }
  if (outcome.left > 0) {
    lines.push(`${outcome.left} symbol${outcome.left === 1 ? '' : 's'} the editor gave had no line to start at and were left out`);
  }
  return lines;
}
