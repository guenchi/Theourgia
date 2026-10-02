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
 * FACTS THE EDITOR COMPUTES, SUPPLIED TO THE STORE.
 *
 * The core's `supply <kind> <file> [--for <writer>]` keeps facts an editor
 * derived from a projection of the store beside the store, never in its log:
 * a declaration's signature and the words it declares, which function calls
 * which, a compiler's diagnostics. The core re-projects the view and refuses
 * the whole file when a listed file is not the bytes it projects
 * (`supply-stale`) or a line is not a fact it takes (`supply-malformed`), and
 * judges each fact stale later by the blocks it depends on. (README of the
 * core, "Derived data from an editor".)
 *
 * This file holds the steps: project with `export-code`, read the markers
 * (src/markers.ts) to know which block a position is in, ask the editor's
 * providers, convert their positions to bytes of the file on disk, write the
 * supply file, send it. Nothing here waits on the editor itself: `runSupply`
 * is handed what it needs, and the commands in extension.ts hand it the
 * editor's.
 *
 * NOTE: THE PROJECTION IS OUTSIDE THE WORKSPACE, in this extension's own
 * storage. A language server may analyse such a file with less context than
 * it has for a workspace folder (an inferred project); the README says so.
 */

import { Answer, Client } from './client';
import { Projection, blockAt, readProjection } from './markers';
import {
  PositionLike,
  RangeLike,
  SYMBOL_KIND_NAMES,
  SavedText,
  byteOffset,
  digestOf,
  lineStarts,
  savedText
} from './split-symbols';
import { TransportError } from './transport';
import { Datum, answerOf, asInteger, isSym, wire } from './wire';

/*
 * HOW LONG THE EDITOR'S DIAGNOSTICS HAVE TO STAY UNCHANGED BEFORE THEY ARE
 * TAKEN AS SETTLED, AND THE MOST THIS WAITS FOR THAT. The editor gives no
 * "analysis finished" signal; a supply taken at the cap says the analysis
 * may be incomplete. Both are here and nowhere else.
 */
export const DIAGNOSTICS_QUIET_MS = 1000;
export const DIAGNOSTICS_CAP_MS = 10000;

export type SupplyKind = 'signatures' | 'calls' | 'diagnostics';

export type Severity = 'error' | 'warning' | 'information' | 'hint';

/*
 * VS CODE'S DiagnosticSeverity, BY ITS VALUE, as the core names it.
 */
const SEVERITIES: readonly Severity[] = ['error', 'warning', 'information', 'hint'];

export type Fact =
  | { kind: 'signature'; id: string; text: string; symbolKind: string; depends: string[] }
  | { kind: 'keywords'; id: string; words: string[]; depends: string[] }
  | { kind: 'calls'; from: string; to: string; depends: string[] }
  | { kind: 'diagnostic'; id: string; severity: Severity; message: string; start: number; end: number; depends: string[] };

/*
 * ONE FILE THE EXPORT WROTE, as the collectors read it: its bytes and digest
 * (every file is listed), and, for a file whose markers read and whose bytes
 * are text, the text the editor's positions count in and where each block
 * sits.
 */
export interface ProjectedFile {
  path: string;
  fsPath: string;
  uri: string;
  digest: string;
  saved: SavedText | null;
  starts: number[];
  projection: Projection | null;
}

/*
 * A FACT'S DEPENDENCIES: its own block first, then every other block of its
 * file -- the coarse set a provider's answer can safely be said to rest on.
 */
export function fileDepends(file: ProjectedFile, subject: string): string[] {
  const others = file.projection === null ? [] : file.projection.blocks.filter((id) => id !== subject);
  return [subject, ...others];
}

/*
 * THE BYTE OF THE FILE ON DISK AN EDITOR POSITION NAMES.
 *
 * NOTE: THE START OF THE TEXT IS AFTER THE BYTE-ORDER MARK. `byteOffset`
 * answers 0 for it, which is what a split wants (a cut at the start keeps
 * the mark with the file); a range of a projected file is counted in its
 * bytes, and the editor's first character is the one after the mark.
 */
export function projectedByte(saved: SavedText, starts: number[], at: PositionLike): number {
  const byte = byteOffset(saved, starts, at);
  return byte === 0 ? saved.bom : byte;
}

/*
 * THE BLOCK AN EDITOR POSITION OF `file` IS IN, or null.
 */
export function blockOfPosition(file: ProjectedFile, at: PositionLike): string | null {
  if (file.saved === null || file.projection === null) {
    return null;
  }
  return blockAt(file.projection, projectedByte(file.saved, file.starts, at));
}

/*
 * ONE DOCUMENT SYMBOL AS THIS FILE USES IT: the name and kind, the detail a
 * provider gave (the signature, when there is one), where its name is, and
 * the names of its direct children.
 */
export interface DeclaredSymbol {
  name: string;
  kind: number;
  detail: string;
  selection: RangeLike;
  children: string[];
}

function isPosition(value: unknown): value is PositionLike {
  const p = value as { line?: unknown; character?: unknown } | null;
  return typeof p === 'object' && p !== null && typeof p.line === 'number' && typeof p.character === 'number';
}

function isRange(value: unknown): value is RangeLike {
  const r = value as { start?: { line?: unknown; character?: unknown }; end?: unknown } | null;
  return (
    typeof r === 'object' &&
    r !== null &&
    typeof r.start === 'object' &&
    r.start !== null &&
    typeof r.start.line === 'number' &&
    typeof r.start.character === 'number' &&
    typeof r.end === 'object' &&
    r.end !== null
  );
}

/*
 * THE TOP LEVEL OF A SYMBOL PROVIDER'S ANSWER, in either form. A
 * `DocumentSymbol` carries its detail, its selection range and its
 * children; a flat `SymbolInformation` of this document with no container
 * has neither detail nor selection, so its range's start stands for both,
 * and its children are the entries naming it as their container.
 */
export function declaredSymbols(answer: unknown, documentUri: string): DeclaredSymbol[] {
  if (!Array.isArray(answer)) {
    return [];
  }
  const out: DeclaredSymbol[] = [];
  const flat: Array<{ name: string; container: string }> = [];
  for (const item of answer) {
    const s = item as {
      name?: unknown;
      kind?: unknown;
      detail?: unknown;
      range?: unknown;
      selectionRange?: unknown;
      children?: unknown;
      containerName?: unknown;
      location?: { uri?: { toString(): string } | null; range?: unknown } | null;
    } | null;
    if (typeof s !== 'object' || s === null || typeof s.name !== 'string' || typeof s.kind !== 'number') {
      continue;
    }
    if (s.location !== undefined) {
      if (typeof s.location !== 'object' || s.location === null) {
        continue;
      }
      const container = typeof s.containerName === 'string' ? s.containerName : '';
      const here = typeof s.location.uri === 'object' && s.location.uri !== null && s.location.uri.toString() === documentUri;
      if (here) {
        flat.push({ name: s.name, container });
      }
      if (container === '' && here && isRange(s.location.range)) {
        out.push({ name: s.name, kind: s.kind, detail: '', selection: s.location.range, children: [] });
      }
    } else if (isRange(s.selectionRange)) {
      const children = Array.isArray(s.children)
        ? s.children
            .map((c) => (c as { name?: unknown } | null)?.name)
            .filter((n): n is string => typeof n === 'string')
        : [];
      out.push({
        name: s.name,
        kind: s.kind,
        detail: typeof s.detail === 'string' ? s.detail : '',
        selection: s.selectionRange,
        children
      });
    }
  }
  const byContainer = new Map<string, string[]>();
  for (const f of flat) {
    byContainer.set(f.container, [...(byContainer.get(f.container) ?? []), f.name]);
  }
  for (const symbol of out) {
    if (symbol.children.length === 0) {
      symbol.children = byContainer.get(symbol.name) ?? [];
    }
  }
  return out;
}

/*
 * THE SIGNATURE IN A HOVER'S TEXT, one hover at a time, the first that has
 * one: the first line of code in it -- inside its first code fence, or a
 * part a server marks with its language -- else its first line that is not
 * blank, not a fence and not a rule, with a heading's marks and code spans'
 * backticks taken off. Null when no hover says anything.
 *
 * NOTE: A HEADING IS NOT A SIGNATURE. A C/C++ server's hover begins
 * "### variable `_server`" and gives the declaration in a fence below it;
 * the first line alone was recorded as the signature, Markdown and all
 * (measured in the user's supply test on a C store).
 *
 * NOTE: CODE WINS OVER PROSE EVEN WHEN THE PROSE COMES FIRST. The same
 * server writes "Parameters:" and the return type above its declaration
 * fence, so prose above a fence is no sign that the fence is an example. A
 * hover that gives its signature as a plain line and then an example in a
 * fence is described by the example; that is the cost of this choice.
 *
 * NOTE: A FENCE ENDS WITH ITS PART. Hovers and parts are read one by one, so
 * a fence that never closes cannot take a line from the next hover.
 */
export function hoverFirstLine(answer: unknown): string | null {
  if (!Array.isArray(answer)) {
    return null;
  }
  for (const hover of answer) {
    const contents = (hover as { contents?: unknown } | null)?.contents;
    const parts: Array<{ lines: string[]; code: boolean }> = [];
    for (const part of Array.isArray(contents) ? contents : [contents]) {
      const value = typeof part === 'string' ? part : (part as { value?: unknown } | null)?.value;
      if (typeof value === 'string') {
        const language = typeof part === 'string' ? undefined : (part as { language?: unknown }).language;
        parts.push({ lines: value.split(/\r?\n/), code: typeof language === 'string' });
      }
    }
    const code = parts.map(codeLineOf).find((line) => line !== null);
    if (code !== undefined && code !== null) {
      return code;
    }
    for (const line of parts.flatMap((p) => p.lines)) {
      const trimmed = line.trim();
      if (trimmed.length === 0 || trimmed.startsWith('```') || /^(-{3,}|\*{3,}|_{3,})$/.test(trimmed)) {
        continue;
      }
      const plain = trimmed.replace(/^#{1,6}\s+/, '').replace(/`/g, '').trim();
      if (plain.length > 0) {
        return plain;
      }
    }
  }
  return null;
}

/*
 * THE FIRST LINE OF CODE IN ONE PART OF A HOVER: the first non-blank line of
 * a part marked with its language, or the first inside the part's first
 * fence, up to the fence's end or the part's. Null when there is none.
 */
function codeLineOf(part: { lines: string[]; code: boolean }): string | null {
  const fence = part.code ? -1 : part.lines.findIndex((line) => line.trim().startsWith('```'));
  if (!part.code && fence < 0) {
    return null;
  }
  for (const line of part.lines.slice(fence + 1)) {
    const trimmed = line.trim();
    if (!part.code && trimmed.startsWith('```')) {
      return null;
    }
    if (trimmed.length > 0) {
      return trimmed;
    }
  }
  return null;
}

/*
 * THE WORDS OF A NAME, composed first (NFC), so that an accent written as a
 * separate mark and one written as part of its letter cut the same way: split
 * at every character that is not a letter, a combining mark or a decimal
 * digit, between a lower-case letter or a digit and an upper-case one, before
 * the last capital of a run of capitals followed by a lower-case letter, and
 * between letters and digits; lower-cased. A combining mark belongs to the
 * character before it. Letters and digits of any script: a name in another
 * alphabet is words too, and an accented letter is part of its word.
 */
export function wordsOf(name: string): string[] {
  return name
    .normalize('NFC')
    .replace(/([\p{Ll}\p{Nd}]\p{M}*)(\p{Lu})/gu, '$1 $2')
    .replace(/((?:\p{Lu}\p{M}*)+)(\p{Lu}\p{M}*\p{Ll})/gu, '$1 $2')
    .replace(/(\p{L}\p{M}*)(\p{Nd})/gu, '$1 $2')
    .replace(/(\p{Nd}\p{M}*)(\p{L})/gu, '$1 $2')
    .split(/[^\p{L}\p{M}\p{Nd}]+/u)
    .filter((w) => w.length > 0)
    .map((w) => w.toLowerCase());
}

/*
 * THE WORDS A BLOCK DECLARES: its symbol's name and its direct children's,
 * each split, in order, each once. Never a name it only uses.
 */
export function declaredWords(symbol: DeclaredSymbol): string[] {
  const out: string[] = [];
  for (const name of [symbol.name, ...symbol.children]) {
    for (const w of wordsOf(name)) {
      if (!out.includes(w)) {
        out.push(w);
      }
    }
  }
  return out;
}

/*
 * HOW REPRESENTATIVE A SYMBOL IS OF ITS BLOCK, lower first: something that is
 * called (a function, a method, a constructor), then a type (a class, a
 * struct, an interface, an enum), then anything else (a variable, a constant,
 * a field). The signature of a block is its most representative symbol's.
 */
function representative(kind: number): number {
  const name = SYMBOL_KIND_NAMES[kind];
  if (name === 'function' || name === 'method' || name === 'constructor') {
    return 0;
  }
  if (name === 'class' || name === 'struct' || name === 'interface' || name === 'enum') {
    return 1;
  }
  return 2;
}

/*
 * EACH BLOCK AND THE TOP-LEVEL SYMBOLS IT DECLARES: a symbol belongs to the
 * block whose own bytes hold the start of its name. `symbol` is the one a
 * signature is taken from (one signature per block): the most representative
 * of them, the first by position among equals -- a file block that opens
 * with a variable is described by its first function, not by the variable
 * (the user's supply test on a C store). `declared` is all of them, in
 * order, the words and calls of a block being every name it declares.
 */
export function subjects(
  file: ProjectedFile,
  symbols: DeclaredSymbol[]
): Array<{ id: string; symbol: DeclaredSymbol; declared: DeclaredSymbol[] }> {
  const ordered = [...symbols].sort(
    (a, b) => a.selection.start.line - b.selection.start.line || a.selection.start.character - b.selection.start.character
  );
  const out: Array<{ id: string; symbol: DeclaredSymbol; declared: DeclaredSymbol[] }> = [];
  for (const symbol of ordered) {
    const id = blockOfPosition(file, symbol.selection.start);
    if (id === null) {
      continue;
    }
    const held = out.find((s) => s.id === id);
    if (held === undefined) {
      out.push({ id, symbol, declared: [symbol] });
    } else {
      held.declared.push(symbol);
      if (representative(symbol.kind) < representative(held.symbol.kind)) {
        held.symbol = symbol;
      }
    }
  }
  return out;
}

/*
 * A BLOCK'S SIGNATURE: the provider's detail when it gave one, else the
 * hover's first line, else none.
 */
export function signatureText(symbol: DeclaredSymbol, hover: unknown): string | null {
  if (symbol.detail.trim().length > 0) {
    return symbol.detail.trim();
  }
  return hoverFirstLine(hover);
}

export async function signatureFacts(
  file: ProjectedFile,
  symbols: DeclaredSymbol[],
  hover: (at: PositionLike) => Promise<unknown>
): Promise<Fact[]> {
  const out: Fact[] = [];
  for (const { id, symbol, declared } of subjects(file, symbols)) {
    const depends = fileDepends(file, id);
    const text = signatureText(symbol, symbol.detail.trim().length > 0 ? null : await hover(symbol.selection.start));
    if (text !== null) {
      out.push({ kind: 'signature', id, text, symbolKind: SYMBOL_KIND_NAMES[symbol.kind] ?? 'unknown', depends });
    }
    const words: string[] = [];
    for (const w of declared.flatMap(declaredWords)) {
      if (!words.includes(w)) {
        words.push(w);
      }
    }
    if (words.length > 0) {
      out.push({ kind: 'keywords', id, words, depends });
    }
  }
  return out;
}

/*
 * ONE CALL HIERARCHY ITEM'S FILE AND NAME POSITION, from an outgoing call's
 * `to`.
 */
function callTarget(call: unknown): { uri: string; at: PositionLike } | null {
  const to = (call as { to?: { uri?: { toString(): string }; selectionRange?: unknown } | null } | null)?.to;
  if (typeof to !== 'object' || to === null || typeof to.uri !== 'object' || to.uri === null || !isRange(to.selectionRange)) {
    return null;
  }
  return { uri: to.uri.toString(), at: to.selectionRange.start };
}

/*
 * THE CALLS A BLOCK MAKES, from the call hierarchy of every top-level symbol
 * it declares: an edge for every outgoing call whose target is a block of
 * this projection, each edge once. A call into anything else -- a library,
 * another store -- gives nothing. Its dependencies are the subject's file's
 * blocks, the subject first, then the target file's.
 */
export async function callFacts(
  file: ProjectedFile,
  symbols: DeclaredSymbol[],
  files: ProjectedFile[],
  prepare: (at: PositionLike) => Promise<unknown>,
  outgoing: (item: unknown) => Promise<unknown>
): Promise<Fact[]> {
  const out: Fact[] = [];
  for (const { id, declared } of subjects(file, symbols)) {
    const items: unknown[] = [];
    for (const symbol of declared) {
      const prepared = await prepare(symbol.selection.start);
      if (Array.isArray(prepared)) {
        items.push(...prepared);
      }
    }
    for (const item of items) {
      const calls = await outgoing(item);
      if (!Array.isArray(calls)) {
        continue;
      }
      for (const call of calls) {
        const target = callTarget(call);
        const into = target === null ? undefined : files.find((f) => f.uri === target.uri);
        const to = into === undefined || target === null ? null : blockOfPosition(into, target.at);
        /*
         * NEVER: A BLOCK CALLING ITSELF. A call between two functions of one
         * block (a text file is one block) is no edge between blocks; it was
         * recorded as one (membuf.h to itself, in the user's supply test).
         */
        if (into === undefined || to === null || to === id || out.some((f) => f.kind === 'calls' && f.from === id && f.to === to)) {
          continue;
        }
        const depends = fileDepends(file, id);
        for (const other of fileDepends(into, to)) {
          if (!depends.includes(other)) {
            depends.push(other);
          }
        }
        out.push({ kind: 'calls', from: id, to, depends });
      }
    }
  }
  return out;
}

/*
 * THE DIAGNOSTICS OF ONE FILE, as the core takes them: a byte range of the
 * file on disk, on the block that holds its start. One that starts in a
 * marker line gives nothing, and so does one whose end is before its start
 * (the core refuses the line); the core maps the range again and refuses a
 * block other than its own.
 */
export function diagnosticFacts(file: ProjectedFile, diagnostics: unknown): Fact[] {
  const out: Fact[] = [];
  if (!Array.isArray(diagnostics) || file.saved === null) {
    return out;
  }
  for (const d of diagnostics) {
    const diagnostic = d as { range?: unknown; severity?: unknown; message?: unknown } | null;
    if (typeof diagnostic !== 'object' || diagnostic === null || !isRange(diagnostic.range) || typeof diagnostic.message !== 'string') {
      continue;
    }
    const severity = typeof diagnostic.severity === 'number' ? SEVERITIES[diagnostic.severity] : undefined;
    if (severity === undefined) {
      continue;
    }
    const range = diagnostic.range;
    const id = blockOfPosition(file, range.start);
    const start = projectedByte(file.saved, file.starts, range.start);
    const end = isPosition(range.end) ? projectedByte(file.saved, file.starts, range.end) : -1;
    if (id === null || end < start) {
      continue;
    }
    out.push({ kind: 'diagnostic', id, severity, message: diagnostic.message, start, end, depends: fileDepends(file, id) });
  }
  return out;
}

/*
 * THE SUPPLY FILE'S TEXT, printed by the wire's own writer.
 *
 * NEVER: THE OFFSETS AS JS NUMBERS. The writer prints a number as an inexact
 * float, and the core takes exact integers only; they go as BigInt.
 *
 * NEVER: A LINE FEED INSIDE A LINE. The file is read one datum per line,
 * and the writer passes a line feed inside a string through raw, so a
 * compiler message of two lines would cut its fact in half and the core
 * would refuse the whole file. Line feeds and carriage returns are escaped
 * over the whole printed line, as the core's own record writer does
 * (wire.sc): the writer emits them nowhere but inside strings, and the
 * core's reader takes `\n` and `\r` there.
 */
export function supplyFileText(
  kind: SupplyKind,
  writer: string,
  language: string,
  editorVersion: string,
  listed: Array<{ path: string; digest: string }>,
  replaces: string[],
  facts: Fact[]
): string {
  const w = wire();
  const header = w.write([
    w.sym('supply'),
    w.sym(kind),
    [w.sym('writer'), writer],
    [w.sym('language'), language],
    [w.sym('source'), [w.sym('vscode'), editorVersion]],
    [w.sym('files'), listed.map((f) => [f.path, f.digest])],
    [w.sym('replaces'), replaces]
  ]);
  const lines = facts.map((f) => {
    const depends = [w.sym('depends'), f.depends];
    switch (f.kind) {
      case 'signature':
        return w.write([w.sym('signature'), f.id, f.text, [w.sym('kind'), w.sym(f.symbolKind)], depends]);
      case 'keywords':
        return w.write([w.sym('keywords'), f.id, f.words, depends]);
      case 'calls':
        return w.write([w.sym('calls'), f.from, f.to, depends]);
      case 'diagnostic':
        return w.write([
          w.sym('diagnostic'),
          f.id,
          w.sym(f.severity),
          f.message,
          [w.sym('range'), BigInt(f.start), BigInt(f.end)],
          depends
        ]);
    }
  });
  return [header, ...lines].map((l) => `${l.replace(/\n/g, '\\n').replace(/\r/g, '\\r')}\n`).join('');
}

/*
 * THE CORE'S ANSWER TO A SUPPLY: how many facts it kept and how many files
 * the supply replaced; or the refusal, carried as its datum.
 */
export type SupplyAnswer = { ok: true; facts: number; files: number } | { ok: false; refusal: Datum };

export function readSupplyAnswer(answer: Answer): SupplyAnswer {
  const datum = answer.answers.length > 0 ? answer.answers[0] : null;
  if (!answer.ok) {
    if (datum !== null && answerOf(datum, 'error') !== null) {
      return { ok: false, refusal: datum };
    }
    throw new TransportError('unreadable', 'the core refused the supply without saying why', answer.text);
  }
  const form = datum === null ? null : answerOf(datum, 'ok');
  const clause = form === null ? null : form.whole('supplied');
  const counts = clause !== null && clause.read ? answerOf(clause.items, 'supplied') : null;
  const facts = counts === null ? null : counts.value('facts');
  const files = counts === null ? null : counts.value('files');
  const nFacts = facts !== null && facts.read ? asInteger(facts.value) : null;
  const nFiles = files !== null && files.read ? asInteger(files.value) : null;
  if (nFacts === null || nFiles === null) {
    throw new TransportError('unreadable', 'the core answered the supply with something this client cannot read', answer.text);
  }
  return { ok: true, facts: nFacts, files: nFiles };
}

/*
 * HOW MANY FILES `export-code` WROTE: its answer `(ok (files <n>))`
 * (code-project.sc, `export-code-view`).
 */
export function readExportedCount(answer: Answer): number {
  const datum = answer.answers.length > 0 ? answer.answers[0] : null;
  const form = datum === null ? null : answerOf(datum, 'ok');
  const files = form === null ? null : form.value('files');
  const n = files !== null && files.read ? asInteger(files.value) : null;
  if (n === null || n < 0) {
    throw new TransportError('unreadable', 'the core answered the projection with something this client cannot read', answer.text);
  }
  return n;
}

/*
 * WHAT A REFUSED SUPPLY SAYS. A stale file asks for the command again; a
 * malformed supply is this extension's defect, and its file is kept for a
 * person; anything else is shown as the core wrote it.
 */
export function supplyRefusalNotice(refusal: Datum, kept: string | null): string {
  const w = wire();
  const stale = answerOf(refusal, 'error', { at: 1, is: 'supply-stale' });
  if (stale !== null) {
    const file = stale.value('file');
    const named = file.read && typeof file.value === 'string' ? file.value : w.write(refusal);
    return `${named} changed after it was projected; run the command again`;
  }
  const malformed = answerOf(refusal, 'error', { at: 1, is: 'supply-malformed' });
  if (malformed !== null) {
    const line = malformed.value('line');
    const reason = malformed.value('reason');
    const at = line.read ? asInteger(line.value) : null;
    const why = reason.read && isSym(reason.value) ? reason.value.name : w.write(refusal);
    const where = kept === null ? '' : `; the file is kept at ${kept}`;
    return `this extension wrote a supply the core refused (line ${at ?? '?'}: ${why})${where}`;
  }
  return `the core refused the supply: ${w.write(refusal)}`;
}

/*
 * A DOCUMENT OF THE PROJECTION AS THE EDITOR HOLDS IT.
 */
export interface OpenedDocument {
  uri: string;
  languageId: string;
  version(): number;
  text(): string;
  isDirty(): boolean;
}

/*
 * THE EDITOR'S DIAGNOSTICS SETTLING: a count that grows with every change to
 * the projection's diagnostics, and a clock.
 */
export interface Settling {
  changes(): number;
  now(): number;
  sleep(ms: number): Promise<void>;
}

/*
 * WAIT UNTIL THE DIAGNOSTICS HAVE NOT CHANGED FOR DIAGNOSTICS_QUIET_MS, or
 * DIAGNOSTICS_CAP_MS has passed; true when they settled, false at the cap.
 */
export async function settle(watch: Settling): Promise<boolean> {
  const start = watch.now();
  let lastCount = watch.changes();
  let lastChange = start;
  for (;;) {
    const now = watch.now();
    if (now - lastChange >= DIAGNOSTICS_QUIET_MS) {
      return true;
    }
    if (now - start >= DIAGNOSTICS_CAP_MS) {
      return false;
    }
    await watch.sleep(Math.max(1, Math.min(100, DIAGNOSTICS_QUIET_MS - (now - lastChange), DIAGNOSTICS_CAP_MS - (now - start))));
    const count = watch.changes();
    if (count !== lastCount) {
      lastCount = count;
      lastChange = watch.now();
    }
  }
}

/*
 * WHAT THE COMMAND IS HANDED: the core, the directory the projection goes
 * into, the disk, and the editor's providers. `supplyPath` names the supply
 * file before anything is written to it, so that a write that fails after
 * creating it is still removed.
 */
export interface SupplyDeps {
  client: Client;
  editorVersion: string;
  directory: string;
  emptyDirectory(directory: string): void;
  filesUnder(directory: string): string[];
  readFile(fsPath: string): Uint8Array;
  fsPathOf(directory: string, path: string): string;
  open(fsPath: string): Promise<OpenedDocument>;
  symbols(uri: string): Promise<unknown>;
  hover(uri: string, at: PositionLike): Promise<unknown>;
  prepareCalls(uri: string, at: PositionLike): Promise<unknown>;
  outgoingCalls(item: unknown): Promise<unknown>;
  settling: Settling;
  diagnostics(uri: string): unknown;
  stillCurrent(): boolean;
  supplyPath(): string;
  writeSupply(path: string, text: string): void;
  /*
   * Throws when a file that exists could not be removed; a file that never
   * came to exist -- the write failed before creating it -- is nothing to
   * remove.
   */
  removeSupply(path: string): void;
}

export type SupplyResult =
  | { language: string; done: 'supplied'; facts: number; files: number }
  | { language: string; done: 'refused'; refusal: Datum; notice: string };

export type SupplyOutcome =
  | { done: 'ran'; results: SupplyResult[]; unread: string[]; incomplete: boolean; notRemoved: string[] }
  | { done: 'refused'; refusal: Datum; notice: string }
  | { done: 'stopped'; why: string };

export const CHANGED_WHILE_SUPPLYING = 'changed while the facts were being collected';

export const STORE_CHANGED_WHILE_SUPPLYING = 'the store changed while the facts were being collected';

/*
 * WHAT A RUN HAD ALREADY DONE WHEN IT STOPPED OR FAILED: each language's
 * supply that was answered, a refused one with its own sentence (which says
 * where a malformed file is kept); each one sent whose answer could not be
 * read, which the core may have kept; every supply file it could not
 * remove; and, once anything was sent, the files it could not read and
 * whether the diagnostics were taken at the cap. A stop between two
 * languages' supplies leaves the first one kept; saying "nothing was sent"
 * then would be false.
 */
export interface History {
  results: SupplyResult[];
  unanswered: string[];
  notRemoved: string[];
  unread: string[];
  incomplete: boolean;
}

export function nothingDone(): History {
  return { results: [], unanswered: [], notRemoved: [], unread: [], incomplete: false };
}

export function historyOf(h: History): string {
  const parts: string[] = [];
  if (h.results.length > 0) {
    parts.push(
      `sent before that: ${h.results
        .map((r) =>
          r.done === 'supplied' ? `${r.language} (${r.facts} fact${r.facts === 1 ? '' : 's'})` : `${r.language} (${r.notice})`
        )
        .join(', ')}`
    );
  }
  if (h.unanswered.length > 0) {
    parts.push(`sent, with an answer this client could not read: ${h.unanswered.join(', ')}`);
  }
  const sent = parts.length > 0;
  if (!sent) {
    parts.push('nothing was sent');
  }
  if (sent && h.unread.length > 0) {
    parts.push(`not read: ${h.unread.join(', ')}`);
  }
  if (sent && h.incomplete) {
    parts.push(`the analysis may be incomplete: the diagnostics were still changing after ${DIAGNOSTICS_CAP_MS / 1000} s`);
  }
  if (h.notRemoved.length > 0) {
    parts.push(`the supply file could not be removed: ${h.notRemoved.join(', ')}`);
  }
  return parts.join('; ');
}

export function stoppedWhy(what: string, h: History): string {
  return `${what}; ${historyOf(h)}. Run the command again`;
}

/*
 * THE SAME TEXT BUT FOR ITS LINE ENDS. The editor's text model makes a
 * file's line ends one kind when they are mixed, and leaves every line's
 * characters as they were; positions, which are all the collectors use,
 * count lines and characters within a line, so they mean the same bytes.
 */
export function sameLines(a: string, b: string): boolean {
  return a.split(/\r\n|\r|\n/).join('\n') === b.split(/\r\n|\r|\n/).join('\n');
}

/*
 * THE COMMAND'S STEPS, for one kind. `writer` is null for the committed
 * store (the header says "-") and the writer's name for a working view.
 *
 * The projection's directory is emptied first, so no file an earlier
 * projection left is listed. Every file the export wrote is listed with its
 * digest; the files whose markers and text read are collected, one supply
 * per language, each naming in `replaces` every collected file of its
 * language, those with no fact included. After collecting, every document is
 * asked again whether it is still the text read from disk, and every file is
 * digested again, before EACH language's supply, since the one before it
 * was a wait: a change anywhere stops the run, since a fact may rest on
 * another file, and the sentence says what had already been sent. The
 * supply file is removed after its answer, except when the core found it
 * malformed, when it is kept for a person.
 *
 * NEVER: A SUPPLY SENT AFTER THE STORE CHANGED. The providers take seconds;
 * a window switched to another store meanwhile has closed this client, and
 * the facts belong to a store it no longer shows. `stillCurrent` is asked
 * before each `supply`, after every wait before it.
 */
export async function runSupply(kind: SupplyKind, writer: string | null, deps: SupplyDeps): Promise<SupplyOutcome> {
  deps.emptyDirectory(deps.directory);
  const exportArgs = writer === null ? [deps.directory] : [deps.directory, '--working', '--writer', writer];
  const exported = await deps.client.request('export-code', exportArgs);
  if (!exported.ok) {
    const datum = exported.answers.length > 0 ? exported.answers[0] : null;
    if (datum !== null && answerOf(datum, 'error') !== null) {
      return { done: 'refused', refusal: datum, notice: `the core refused the projection: ${wire().write(datum)}` };
    }
    throw new TransportError('unreadable', 'the core refused the projection without saying why', exported.text);
  }
  /*
   * THE EXPORT SAYS HOW MANY FILES IT WROTE, and every one of them is
   * listed: a file missing from the directory would be left out of the
   * supply's `files`, and the core does not ask for all of them.
   */
  const written = readExportedCount(exported);
  const found = deps.filesUnder(deps.directory);
  if (written !== found.length) {
    return {
      done: 'stopped',
      why: stoppedWhy(`the projection wrote ${written} file${written === 1 ? '' : 's'} and ${found.length} are there`, nothingDone())
    };
  }
  const files: ProjectedFile[] = [];
  const documents = new Map<string, { document: OpenedDocument; version: number; text: string }>();
  const unread: string[] = [];
  for (const path of found) {
    const fsPath = deps.fsPathOf(deps.directory, path);
    const bytes = deps.readFile(fsPath);
    const saved = savedText(bytes);
    const reading = readProjection(bytes, path);
    const projection = reading.ok ? reading.projection : null;
    if (!reading.ok || saved === null) {
      unread.push(path);
    }
    let uri = fsPath;
    if (projection !== null && saved !== null) {
      const document = await deps.open(fsPath);
      uri = document.uri;
      documents.set(path, { document, version: document.version(), text: document.text() });
      if (!sameLines(document.text(), saved.text)) {
        return { done: 'stopped', why: stoppedWhy(`${path} ${CHANGED_WHILE_SUPPLYING}`, nothingDone()) };
      }
    }
    files.push({ path, fsPath, uri, digest: digestOf(bytes), saved, starts: saved === null ? [] : lineStarts(saved.text), projection });
  }
  const collected = files.filter((f) => documents.has(f.path));
  let incomplete = false;
  if (kind === 'diagnostics' && collected.length > 0) {
    incomplete = !(await settle(deps.settling));
  }
  const byLanguage = new Map<string, { files: ProjectedFile[]; facts: Fact[] }>();
  for (const file of collected) {
    const language = (documents.get(file.path) as { document: OpenedDocument }).document.languageId;
    const group = byLanguage.get(language) ?? { files: [], facts: [] };
    byLanguage.set(language, group);
    group.files.push(file);
    if (kind === 'diagnostics') {
      group.facts.push(...diagnosticFacts(file, deps.diagnostics(file.uri)));
      continue;
    }
    const symbols = declaredSymbols(await deps.symbols(file.uri), file.uri);
    if (kind === 'signatures') {
      group.facts.push(...(await signatureFacts(file, symbols, (at) => deps.hover(file.uri, at))));
    } else {
      group.facts.push(
        ...(await callFacts(file, symbols, collected, (at) => deps.prepareCalls(file.uri, at), (item) => deps.outgoingCalls(item)))
      );
    }
  }
  /*
   * THE FIRST FILE THAT IS NO LONGER WHAT WAS READ, or null: its bytes on
   * disk, or the editor's document of it.
   */
  const moved = (): string | null => {
    for (const file of files) {
      const held = documents.get(file.path);
      const changed =
        digestOf(deps.readFile(file.fsPath)) !== file.digest ||
        (held !== undefined &&
          (held.document.isDirty() || held.document.version() !== held.version || held.document.text() !== held.text));
      if (changed) {
        return file.path;
      }
    }
    return null;
  };
  const listed = files.map((f) => ({ path: f.path, digest: f.digest }));
  const results: SupplyResult[] = [];
  const notRemoved: string[] = [];
  const history: History = { results, unanswered: [], notRemoved, unread, incomplete };
  /*
   * AN ERROR THAT ENDS THE RUN CARRIES WHAT THE RUN HAD DONE: a supply the
   * core had already kept is not undone by a later failure, and a file left
   * behind is said. The error keeps its own kind; only a thrown value that
   * is not an Error becomes one.
   */
  const withHistory = (e: unknown): unknown => {
    if (results.length === 0 && history.unanswered.length === 0 && notRemoved.length === 0) {
      return e;
    }
    const error = e instanceof Error ? e : new Error(String(e));
    error.message = `${error.message}; ${historyOf(history)}`;
    return error;
  };
  for (const [language, group] of byLanguage) {
    let path: string | null = null;
    let keep = false;
    let asked = false;
    let answered = false;
    let failure: { error: unknown } | null = null;
    try {
      if (!deps.stillCurrent()) {
        return { done: 'stopped', why: stoppedWhy(STORE_CHANGED_WHILE_SUPPLYING, history) };
      }
      const changed = moved();
      if (changed !== null) {
        return { done: 'stopped', why: stoppedWhy(`${changed} ${CHANGED_WHILE_SUPPLYING}`, history) };
      }
      path = deps.supplyPath();
      deps.writeSupply(
        path,
        supplyFileText(kind, writer ?? '-', language, deps.editorVersion, listed, group.files.map((f) => f.path), group.facts)
      );
      asked = true;
      const answer: Answer = await deps.client.request('supply', writer === null ? [kind, path] : [kind, path, '--for', writer]);
      const read = readSupplyAnswer(answer);
      if (read.ok) {
        results.push({ language, done: 'supplied', facts: read.facts, files: read.files });
      } else {
        keep = answerOf(read.refusal, 'error', { at: 1, is: 'supply-malformed' }) !== null;
        results.push({ language, done: 'refused', refusal: read.refusal, notice: supplyRefusalNotice(read.refusal, keep ? path : null) });
      }
      answered = true;
    } catch (e) {
      failure = { error: e };
      /*
       * A SUPPLY SENT WITHOUT AN ANSWER THAT READS MAY HAVE BEEN KEPT: the
       * core writes before it answers, so it is said as sent, not as not.
       * Except where the client could not start the core at all
       * (`spawn-failed`, transport.ts): then the verb never ran.
       */
      const neverRan = e instanceof TransportError && e.failure === 'spawn-failed';
      if (asked && !answered && !neverRan) {
        history.unanswered.push(language);
      }
    }
    /*
     * THE REMOVAL COVERS THE WRITE AS WELL AS THE REQUEST, and a removal
     * that fails is said whichever way the send went: with the error that
     * stopped the send, or in the outcome.
     */
    if (path !== null && !keep) {
      try {
        deps.removeSupply(path);
      } catch (e) {
        notRemoved.push(`${path} (${e instanceof Error ? e.message : String(e)})`);
      }
    }
    if (failure !== null) {
      throw withHistory(failure.error);
    }
  }
  return { done: 'ran', results, unread, incomplete, notRemoved };
}

/*
 * WHAT THE COMMAND SAYS WHEN IT RAN: one sentence per language, the files it
 * could not read, a supply file it could not remove, and, for diagnostics
 * taken at the cap, that the analysis may be incomplete.
 */
export function supplyNotice(
  kind: SupplyKind,
  outcome: { results: SupplyResult[]; unread: string[]; incomplete: boolean; notRemoved: string[] }
): string {
  const parts = outcome.results.map((r) =>
    r.done === 'supplied'
      ? `supplied ${r.facts} ${kind === 'signatures' ? 'signature and keyword' : kind === 'calls' ? 'call' : 'diagnostic'} fact${r.facts === 1 ? '' : 's'} for ${r.files} ${r.language} file${r.files === 1 ? '' : 's'}`
      : `${r.language}: ${r.notice}`
  );
  if (parts.length === 0) {
    parts.push('the projection holds no file the editor could read, so nothing was supplied');
  }
  if (outcome.unread.length > 0) {
    parts.push(`not read: ${outcome.unread.join(', ')}`);
  }
  if (outcome.notRemoved.length > 0) {
    parts.push(`the supply file could not be removed: ${outcome.notRemoved.join(', ')}`);
  }
  if (outcome.incomplete) {
    parts.push(`the analysis may be incomplete: the diagnostics were still changing after ${DIAGNOSTICS_CAP_MS / 1000} s`);
  }
  return parts.join('; ');
}
