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
 * WHAT THE STORE HOLDS ABOUT THE NAME UNDER THE POINTER, said in the hover.
 *
 * KEY: THE INVARIANT, before any route. The hover reads only, and answers
 * about THE STORE THE DOCUMENT SAYS IT CAME FROM: a projection file's record
 * names its store, a composed document's address names its store. It never
 * answers from the configured store under an id that came from another one.
 * Anything it cannot place answers undefined.
 *
 * What it answers about: T1, the block the pointer's position belongs to (by
 * the kind of document, D1), and T2, the block `whereis` says defines the name
 * (none when the store answers unknown-name, which is not a failure). What it
 * shows, in order: edges someone wrote to them (`refs`, via link), references
 * in text (`refs`, via md; an editor-supplied `calls` row is code, not a
 * document), and mentions of the name as a whole word in prose blocks (`grep`,
 * filtered here: the core matches literally). At most five lines; the rest is
 * one "N more" line that lists them all.
 *
 * NOTHING HERE IMPORTS VS CODE, so that what the hover says is decided where a
 * cell can ask; extension.ts registers the provider and passes the document.
 */

import { Client } from './client';
import { HOVER_MORE, OPEN_BLOCK } from './commands';
import { Block, readBlock, textOf } from './blocks';
import { definitionsOf, identifierAt } from './definition';
import { readProjection } from './markers';
import { TransportError } from './transport';
import { Datum, answerOf, isList, wire } from './wire';

/*
 * D1. WHERE A HOVER IS ASKED, read from the document's record or address,
 * never guessed from its text: a block's own projection file, the subtree view
 * composed from `read --recursive`, or the read-only datum view. `store` is the
 * store the document says it came from.
 */
export type HoverPlace =
  | { kind: 'block'; store: string; blockId: string; scheme: boolean }
  | { kind: 'subtree'; store: string }
  | { kind: 'datum'; store: string };

/*
 * THE SCHEME DIALECTS, by the lang a block carries. D2: the name reader is
 * chosen by this and by the kind of document, not by the editor's language id,
 * which falls back to plain text for a lang the editor does not know.
 */
const SCHEME_LANGS = new Set(['scheme', 'chez', 'chezscheme', 'r6rs', 'r7rs']);

export function isSchemeLang(lang: string | null | undefined): boolean {
  return typeof lang === 'string' && SCHEME_LANGS.has(lang.toLowerCase());
}

/*
 * D2. WHETHER THE NAME UNDER THE POINTER IS READ AS A SCHEME IDENTIFIER: in a
 * datum view always (only Scheme is projected as a datum), in a block's file
 * when its block's lang is a Scheme dialect, and nowhere else.
 */
export function readsSchemeNames(place: HoverPlace): boolean {
  return place.kind === 'datum' || (place.kind === 'block' && place.scheme);
}

/*
 * D2. THE NAME UNDER THE POINTER: the Scheme identifier reader where the place
 * reads Scheme names, the editor's word range (given, as the editor computes
 * it for the document) everywhere else.
 */
export function nameUnder(
  place: HoverPlace,
  text: string,
  line: number,
  character: number,
  wordRange: () => string | null
): string | null {
  const name = readsSchemeNames(place) ? identifierAt(text, line, character) : wordRange();
  return name === null || name.length === 0 ? null : name;
}

const SUBTREE_MARKER = /^<!-- theourgia block (\S+) depth \d+ -->$/;

/*
 * D1. T1, THE BLOCK AT AN OFFSET (in characters) of the document's text:
 * - a block's own file: the record's block;
 * - the subtree view: the innermost block, the last marker above the
 *   position; a marker line itself is in no block;
 * - the datum view: the projection's pieces -- a definition's own source is
 *   its block, the library's own lines (between the header and the first
 *   definition) are the library, the header's file id; a marker line, the
 *   header line and the lines above the header are in no block.
 * Null for a position in no block.
 */
export function blockAt(place: HoverPlace, text: string, offset: number): string | null {
  if (place.kind === 'block') {
    return place.blockId;
  }
  if (place.kind === 'subtree') {
    const lines = text.split('\n');
    let at = 0;
    let line = 0;
    while (line < lines.length - 1 && at + lines[line].length < offset) {
      at += lines[line].length + 1;
      line += 1;
    }
    if (SUBTREE_MARKER.test(lines[line] ?? '')) {
      return null;
    }
    for (let i = line - 1; i >= 0; i -= 1) {
      const marker = SUBTREE_MARKER.exec(lines[i]);
      if (marker !== null) {
        return marker[1];
      }
    }
    return null;
  }
  const bytes = new Uint8Array(Buffer.from(text, 'utf8'));
  const reading = readProjection(bytes, 'datum view', 'datum');
  if (!reading.ok) {
    return null;
  }
  const byte = Buffer.byteLength(text.slice(0, Math.max(0, offset)), 'utf8');
  const pieces = reading.projection.pieces;
  const header = pieces.findIndex((p) => p.kind === 'control');
  for (let i = 0; i < pieces.length; i += 1) {
    const piece = pieces[i];
    const last = i === pieces.length - 1;
    if (!(piece.start <= byte && (byte < piece.end || (last && byte === piece.end)))) {
      continue;
    }
    if (piece.kind === 'control' || i < header) {
      return null;
    }
    return piece.id ?? reading.projection.fileId;
  }
  return null;
}

/*
 * WHAT A LINE OF THE HOVER IS ABOUT: a block that refers to T1 or T2, by an
 * edge someone wrote (`edge`, with its relation), by a reference in its text
 * (`text`), or by holding the name as a whole word (`mention`).
 */
export interface HoverCandidate {
  section: 'edge' | 'text' | 'mention';
  id: string;
  rel: string | null;
}

export interface HoverLine extends HoverCandidate {
  title: string;
  kind: string;
  sentence: string;
}

/*
 * `more` is exact: edges and text references past the five read (their kind
 * needs no read), and any referrer that could not be read. `unverified` is the
 * mentions past the five: whether each is prose is not known until it is
 * read, so the hover says "up to" that many, and the list shows the exact set.
 */
export interface HoverAnswer {
  lines: HoverLine[];
  candidates: HoverCandidate[];
  more: number;
  unverified: number;
}

/*
 * D3. THE KINDS A MENTION IS LISTED FOR: prose, by a positive list. A library
 * block and a file block are code-side, and so is a code block.
 */
export const PROSE_KINDS = ['doc', 'section', 'decision', 'task'];

export const MAX_LINES = 5;
const SENTENCE_LIMIT = 200;

/*
 * A BLOCK'S FIRST SENTENCE: its text up to the first line break, then up to
 * the first sentence end, at most 200 characters.
 */
export function firstSentence(text: string): string {
  const line = text.replace(/^\s+/, '').split('\n')[0] ?? '';
  const end = /[.!?](?=\s|$)/.exec(line);
  const sentence = end === null ? line : line.slice(0, end.index + 1);
  return sentence.length > SENTENCE_LIMIT ? sentence.slice(0, SENTENCE_LIMIT) : sentence;
}

/*
 * WHETHER A LINE HOLDS THE NAME AS A WHOLE WORD, by the identifier rule of the
 * document's language on both sides: a Scheme identifier runs between
 * delimiters; any other runs over letters, digits and `_`.
 */
const SCHEME_DELIMITER = /[\s()[\]{}"';`,]/;
const WORD = /[A-Za-z0-9_]/;

export function holdsWord(line: string, name: string, scheme: boolean): boolean {
  if (name.length === 0) {
    return false;
  }
  const inside = (c: string | undefined): boolean =>
    c !== undefined && (scheme ? !SCHEME_DELIMITER.test(c) : WORD.test(c));
  for (let at = line.indexOf(name); at >= 0; at = line.indexOf(name, at + 1)) {
    if (!inside(line[at - 1]) && !inside(line[at + name.length])) {
      return true;
    }
  }
  return false;
}

/*
 * THE HOVER WAS LEFT (its CancellationToken asked) while an answer was on its
 * way: nothing is shown and nothing is kept.
 */
export class HoverCancelled extends Error {}

interface RefRow {
  from: string;
  rel: string;
  via: string;
}

/*
 * `refs <id>` answers one `(ref (from <id>) (rel <rel>) (via link|md|...))`
 * per line. A refusal is a failure of the hover, said by the caller.
 */
async function refsOf(client: Client, id: string): Promise<RefRow[]> {
  const answer = await client.request('refs', [id]);
  if (!answer.ok) {
    throw new TransportError('unreadable', `the store would not list what refers to ${id}: ${answer.text.trim()}`, answer.text);
  }
  const rows: RefRow[] = [];
  for (const item of answer.answers) {
    const form = answerOf(item, 'ref');
    const from = form === null ? null : form.value('from');
    const rel = form === null ? null : form.value('rel');
    const via = form === null ? null : form.value('via');
    if (from === null || rel === null || via === null || !from.read || !rel.read || !via.read || typeof from.value !== 'string') {
      throw new TransportError('unreadable', `the store answered the references to ${id} with a row this client cannot read`, answer.text);
    }
    rows.push({ from: from.value, rel: wire().write(rel.value), via: wire().write(via.value) });
  }
  return rows;
}

interface MatchRow {
  id: string;
  text: string;
}

/*
 * `grep <name>` answers one `(match <id> <line> "<text>")` per item, literally
 * and without case.
 *
 * NEVER: ASKED WITH `--wire`. On the human route, when every match was in a
 * superseded or refuted block, the pinned core (5c28e42) prints nothing, and
 * a newer core prints its `(excluded ...)` line, which is not a match and
 * would fail the hover where there is nothing to mention. The wire answer's
 * items are the matches on either core; what was left out is not mentioned.
 */
async function matchesOf(client: Client, name: string): Promise<MatchRow[]> {
  const answer = await client.request('grep', [name, '--wire']);
  if (!answer.ok) {
    throw new TransportError('unreadable', `the store would not search the text for ${name}: ${answer.text.trim()}`, answer.text);
  }
  const rows: MatchRow[] = [];
  for (const item of answer.answers) {
    if (answerOf(item, 'match') === null || !isList(item) || typeof item[1] !== 'string' || typeof item[3] !== 'string') {
      throw new TransportError('unreadable', `the store answered the search for ${name} with a row this client cannot read`, answer.text);
    }
    rows.push({ id: item[1], text: item[3] });
  }
  return rows;
}

/*
 * A BLOCK'S TITLE, KIND AND FIRST SENTENCE, read from the store; null for a
 * block that could not be read, which is counted under "N more" and never
 * shown as a bare id.
 */
async function describe(client: Client, id: string): Promise<{ title: string; kind: string; sentence: string } | null> {
  try {
    const answer = await client.request('read', [id]);
    const datum: Datum = answer.ok && answer.answers.length > 0 ? answer.answers[0] : null;
    const block: Block | null = datum !== null && isList(datum) && datum.length >= 2 && answerOf(datum, 'ok') !== null ? readBlock(datum[1]) : null;
    if (block === null || block.id !== id) {
      return null;
    }
    const kind = block.fields.get('kind');
    const title = block.fields.get('title');
    const src = textOf(block, 'src');
    return {
      title: typeof title === 'string' && title.length > 0 ? title : id,
      kind: kind === undefined ? 'block' : typeof kind === 'string' ? kind : wire().write(kind),
      sentence: typeof src === 'string' ? firstSentence(src) : ''
    };
  } catch {
    return null;
  }
}

/*
 * THE WHOLE ANSWER for T1 and a name, in the order the brief sets: `whereis`,
 * `refs T1` and `grep` together; then `refs T2` when there is one and it is not
 * T1 (D6); then the reads for the lines to be shown, at most five, together.
 * `cancelled` is asked after every wait.
 *
 * NOTE: A MENTION'S KIND IS KNOWN ONLY BY READING IT. The reads go to the
 * first five candidates; a mention among them whose kind is not prose is
 * dropped. After them, edges and text references are counted exactly, and
 * mentions as "up to" -- the list command reads them all and shows the exact
 * set.
 */
export async function gatherContext(
  client: Client,
  t1: string,
  name: string,
  scheme: boolean,
  cancelled: () => boolean
): Promise<HoverAnswer> {
  const check = (): void => {
    if (cancelled()) {
      throw new HoverCancelled();
    }
  };
  const [defined, refs1, matches] = await Promise.all([definitionsOf(client, name), refsOf(client, t1), matchesOf(client, name)]);
  check();
  const t2 = 'found' in defined ? defined.found.find((r) => r.kind === 'def')?.id ?? null : null;
  const refs2 = t2 !== null && t2 !== t1 ? await refsOf(client, t2) : [];
  check();
  const candidates: HoverCandidate[] = [];
  const seen = new Set<string>();
  const add = (candidate: HoverCandidate): void => {
    const key = JSON.stringify([candidate.section, candidate.id, candidate.rel]);
    if (!seen.has(key)) {
      seen.add(key);
      candidates.push(candidate);
    }
  };
  for (const row of [...refs1, ...refs2]) {
    if (row.via === 'link') {
      add({ section: 'edge', id: row.from, rel: row.rel });
    }
  }
  for (const row of [...refs1, ...refs2]) {
    if (row.via === 'md' && row.rel !== 'calls') {
      add({ section: 'text', id: row.from, rel: null });
    }
  }
  const named = new Set(candidates.map((c) => c.id));
  for (const row of matches) {
    if (row.id !== t1 && row.id !== t2 && !named.has(row.id) && holdsWord(row.text, name, scheme)) {
      named.add(row.id);
      add({ section: 'mention', id: row.id, rel: null });
    }
  }
  const first = candidates.slice(0, MAX_LINES);
  const described = await Promise.all(first.map((c) => describe(client, c.id)));
  check();
  const lines: HoverLine[] = [];
  let dropped = 0;
  first.forEach((candidate, i) => {
    const about = described[i];
    if (about === null) {
      return;
    }
    if (candidate.section === 'mention' && !PROSE_KINDS.includes(about.kind)) {
      dropped += 1;
      return;
    }
    lines.push({ ...candidate, ...about });
  });
  const kept = candidates.filter((c, i) => !(i < MAX_LINES && c.section === 'mention' && described[i] !== null && !PROSE_KINDS.includes((described[i] as { kind: string }).kind)));
  const unread = candidates.slice(MAX_LINES);
  const unverified = unread.filter((c) => c.section === 'mention').length;
  const unreadable = first.length - lines.length - dropped;
  return { lines, candidates: kept, more: unreadable + unread.length - unverified, unverified };
}

/*
 * THE TWO COMMANDS A HOVER'S LINKS MAY RUN, and no other: opening a block,
 * and listing everything the hover found.
 */
export const HOVER_COMMANDS: readonly string[] = [OPEN_BLOCK.id, HOVER_MORE.id];

/*
 * EVERYTHING A HOVER FOUND, for its "N more" list: each candidate read, in the
 * hover's order. A mention whose block is not prose is left out, as in the
 * hover; a block that could not be read is listed by its id and says so.
 */
export async function listEntries(
  client: Client,
  candidates: unknown[]
): Promise<Array<{ id: string; label: string; description: string }>> {
  const wanted = candidates.filter(
    (c): c is HoverCandidate =>
      typeof c === 'object' && c !== null && typeof (c as HoverCandidate).id === 'string' &&
      ['edge', 'text', 'mention'].includes((c as HoverCandidate).section)
  );
  const described = await Promise.all(wanted.map((c) => describe(client, c.id)));
  const out: Array<{ id: string; label: string; description: string }> = [];
  wanted.forEach((c, i) => {
    const about = described[i];
    if (about === null) {
      out.push({ id: c.id, label: c.id, description: 'could not be read' });
      return;
    }
    if (c.section === 'mention' && !PROSE_KINDS.includes(about.kind)) {
      return;
    }
    const how = c.section === 'edge' ? c.rel ?? 'edge' : c.section === 'text' ? 'refers in its text' : 'mentions';
    out.push({ id: c.id, label: about.title, description: `${how} -- ${about.kind}` });
  });
  return out;
}

/*
 * TEXT A HOVER SHOWS, escaped: markdown and HTML characters are shown as
 * themselves.
 */
export function escapeHover(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/([\\`*_{}[\]()#+\-.!|~])/g, '\\$1');
}

/*
 * A COMMAND LINK: the command's id and its arguments, as a command URI.
 */
export function commandLink(command: string, args: unknown[]): string {
  return `command:${command}?${encodeURIComponent(JSON.stringify(args))}`;
}

/*
 * THE HOVER'S MARKDOWN. Each line: the relation (edges), the title as a link
 * that opens the block in the store it came from, its kind, and its first
 * sentence. More than five: the exact "N more" for edges, text references and
 * unreadable referrers, and "up to N more mentions" for the mentions not read;
 * both list everything.
 */
export function hoverMarkdown(answer: HoverAnswer, store: string, open: string, list: string, name: string): string {
  const out: string[] = ['**Theourgia**', ''];
  for (const line of answer.lines) {
    const link = `[${escapeHover(line.title)}](${commandLink(open, [line.id, store])})`;
    const rel = line.section === 'edge' && line.rel !== null ? `*${escapeHover(line.rel)}* ` : line.section === 'mention' ? '*mentions* ' : '';
    const sentence = line.sentence.length > 0 ? ` -- ${escapeHover(line.sentence)}` : '';
    out.push(`- ${rel}${link} (${escapeHover(line.kind)})${sentence}`);
  }
  const all = commandLink(list, [store, name, answer.candidates]);
  if (answer.more > 0) {
    out.push(`- [${answer.more} more](${all})`);
  }
  if (answer.unverified > 0) {
    out.push(`- [up to ${answer.unverified} more mentions](${all})`);
  }
  return out.join('\n');
}

/*
 * THE CACHE, THE DEADLINE AND THE TOKEN. An answer is kept under (store,
 * T1, name) for 15 seconds -- another writer's commit is not observed -- and
 * every entry is dropped on what this window does observe (the extension
 * calls `drop`). An answer that began before a drop is not kept after it.
 * Past the deadline the hover answers undefined; the answer still fills the
 * cache when it arrives, unless the hover was left. The token is asked
 * before the cache is read, after every wait, and before the cache is written.
 */
export interface HoverDeps {
  clientFor(store: string): Client | null;
  log(line: string): void;
  now(): number;
}

export interface Token {
  isCancellationRequested: boolean;
}

export const HOVER_DEADLINE_MS = 800;

function shows(answer: HoverAnswer): boolean {
  return answer.lines.length > 0 || answer.more > 0 || answer.unverified > 0;
}
export const HOVER_TTL_MS = 15000;

export class HoverService {
  private readonly cache = new Map<string, { answer: HoverAnswer; at: number }>();
  private drops = 0;

  constructor(
    private readonly deps: HoverDeps,
    private readonly deadlineMs: number = HOVER_DEADLINE_MS,
    private readonly ttlMs: number = HOVER_TTL_MS
  ) {}

  public drop(): void {
    this.cache.clear();
    this.drops += 1;
  }

  public async answer(store: string, t1: string, name: string, scheme: boolean, token: Token): Promise<HoverAnswer | undefined> {
    /*
     * NEVER: A STORE THE DOCUMENT DOES NOT NAME. An empty identity resolves to
     * the working directory, which is no store the document said.
     */
    if (token.isCancellationRequested || store.length === 0 || t1.length === 0) {
      return undefined;
    }
    const key = JSON.stringify([store, t1, name]);
    const kept = this.cache.get(key);
    if (kept !== undefined && this.deps.now() - kept.at < this.ttlMs) {
      return shows(kept.answer) ? kept.answer : undefined;
    }
    const client = this.deps.clientFor(store);
    if (client === null) {
      return undefined;
    }
    const startedAt = this.drops;
    const work = gatherContext(client, t1, name, scheme, () => token.isCancellationRequested).then(
      (answer) => {
        if (!token.isCancellationRequested && this.drops === startedAt) {
          this.cache.set(key, { answer, at: this.deps.now() });
        }
        return answer;
      },
      (error: unknown) => {
        if (!(error instanceof HoverCancelled)) {
          this.deps.log(`hover: what the store holds about ${name} could not be read: ${error instanceof Error ? error.message : String(error)}`);
        }
        return null;
      }
    );
    let timer: ReturnType<typeof setTimeout> | undefined;
    const late = new Promise<'late'>((resolve) => {
      timer = setTimeout(() => resolve('late'), this.deadlineMs);
    });
    const outcome = await Promise.race([work, late]);
    if (timer !== undefined) {
      clearTimeout(timer);
    }
    if (outcome === 'late') {
      this.deps.log(`hover: no answer about ${name} within ${this.deadlineMs} ms; the next hover is answered when it arrives`);
      return undefined;
    }
    if (token.isCancellationRequested || outcome === null) {
      return undefined;
    }
    return shows(outcome) ? outcome : undefined;
  }
}
