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
 * THE STORE AS FILES: the directory tree export would write, derived from
 * the `path` field of the blocks export writes as files.
 *
 * The tree is never a second hierarchy in the store. A directory is only a
 * prefix of some file's path: it appears with its first file and goes with
 * its last. A file here is exactly what export writes as one:
 *
 *   - a document (`kind doc`) at the root of the outline, with a path;
 *   - a text-mode file block (`kind file`, `mode text`) with a path,
 *     wherever it sits -- export-code writes every one;
 *   - a datum-mode library (`kind library`, `mode datum`) with a path.
 *
 * A block of another kind that carries a path is not a file, and neither
 * is a document below the root (a nested document: export writes its
 * content into the enclosing document, not a file of its own).
 *
 * Nothing in this file asks the store anything; it is given the blocks.
 */

import { Block, TextNotUtf8, fieldConflict, parentOf, textOf } from './blocks';
import { Datum, answerOf, isSym, wire } from './wire';

/*
 * A PATH AS THE EXPORTER TAKES ONE, the same rule as the core's
 * `relative-safe?` (code-project.sc): not empty, not beginning with `/`, no
 * NUL and no backslash, and no component that is empty, `.` or `..`. A path
 * the exporter refuses is not tidied here into one it would take: its
 * block is not exported, and is shown as such.
 */
export function exportablePath(path: string): boolean {
  if (path.length === 0 || path.startsWith('/') || path.includes('\u0000') || path.includes('\\')) {
    return false;
  }
  return path.split('/').every((part) => part !== '' && part !== '.' && part !== '..');
}

export type FileKind = 'doc' | 'file' | 'library';

/*
 * ONE BLOCK AS THIS VIEW SEES IT: what it is, where it sits in the outline,
 * and its path as stored.
 */
export interface Candidate {
  id: string;
  title: string;
  kind: string | null;
  mode: string | null;
  /*
   * The path when it is stored as a string, the only form the exporters read
   * as a path (project.sc's text-field answers "" for anything else); null
   * otherwise.
   */
  path: string | null;
  /*
   * Why a path field that is there is not a string path: `conflict` when it
   * holds more than one candidate value, `not-text` when it is stored as
   * something else (bytes included). Null when there is a string path or no
   * path field at all. `raw` is the stored value as text, when there is
   * text to show.
   */
  pathProblem: 'conflict' | 'not-text' | null;
  raw: string | null;
  /*
   * True for a block listed at the root of the outline: top level, or
   * promoted there by a structural mark (cycle, unplaced, orphan).
   */
  atRoot: boolean;
  /*
   * True for a block export takes as a root: top level, or a cycle or an
   * unplaced block, which the core's outline puts at the root. An orphan is
   * listed at the root and is NOT one: it still names the parent that was
   * deleted, and export-md skips it.
   */
  exportRoot: boolean;
  /*
   * The block's parent in the outline (null at the root or when its place
   * is not one parent), and whether its `src` is stored as bytes. The
   * exporters judge a file by its children, and these say what they are.
   */
  parent: string | null;
  srcBytes: boolean;
  /*
   * A tombstone. A subtree read can answer one (a block deleted while the
   * listing was being read); the exporters take live blocks only, so it is
   * in no file and no family.
   */
  deleted: boolean;
}

/*
 * A FIELD'S SYMBOL, or null. The exporters compare kind and mode as
 * symbols, so a string that spells `doc` is not a document.
 */
function symbolText(value: Datum): string | null {
  return isSym(value) ? value.name : null;
}

/*
 * A BLOCK'S CANDIDATE ENTRY. A path in conflict is not a path, and neither is
 * one stored as bytes: the exporters read a string or nothing. Such a path
 * is shown as it reads, decoded when its bytes are UTF-8.
 */
export function candidateOf(block: Block, title: string, atRoot: boolean, exportRoot: boolean): Candidate {
  const stored = block.fields.get('path');
  let path: string | null = null;
  let pathProblem: Candidate['pathProblem'] = null;
  let raw: string | null = null;
  if (stored === undefined) {
    pathProblem = null;
  } else if (fieldConflict(stored as Datum) !== null) {
    pathProblem = 'conflict';
  } else if (typeof stored === 'string') {
    path = stored;
    raw = stored;
  } else {
    pathProblem = 'not-text';
    try {
      const value = textOf(block, 'path');
      raw = typeof value === 'string' ? value : null;
    } catch (e) {
      if (!(e instanceof TextNotUtf8)) {
        throw e;
      }
      raw = null;
    }
  }
  return {
    id: block.id,
    title,
    kind: symbolText(block.fields.get('kind') as Datum),
    mode: symbolText(block.fields.get('mode') as Datum),
    path,
    pathProblem,
    raw,
    atRoot,
    exportRoot,
    parent: parentOf(block),
    srcBytes: block.fields.get('src') instanceof Uint8Array,
    deleted: block.deleted
  };
}

/*
 * WHETHER EXPORT WRITES THIS BLOCK AS A FILE, and as which kind. The path is
 * judged separately: a file kind with a path export refuses is still not
 * exported.
 */
export function fileKindOf(c: Candidate): FileKind | null {
  if (c.path === null) {
    return null;
  }
  if (c.kind === 'doc' && c.exportRoot) {
    return 'doc';
  }
  if (c.kind === 'file' && c.mode === 'text') {
    return 'file';
  }
  if (c.kind === 'library' && c.mode === 'datum') {
    return 'library';
  }
  return null;
}

/*
 * A FILE NODE: the block, its label (the last segment of its path -- export
 * names files by path, and the title stays in the tooltip), and what export
 * does with it when it is not simply written.
 */
export interface FileEntry {
  id: string;
  path: string;
  label: string;
  title: string;
  kind: FileKind;
  /*
   * Null when export writes the file. Otherwise the reason it does not, in
   * the exporter's own terms.
   */
  note: string | null;
}

/*
 * A DIRECTORY NODE: a grouping only -- no block, no id of the store's, no
 * mark, nothing to open. `files` counts every file below it, at any depth.
 */
export interface DirectoryEntry {
  prefix: string;
  name: string;
  directories: DirectoryEntry[];
  files: FileEntry[];
  count: number;
}

/*
 * A ROOT BLOCK THAT IS IN NO FILE: no path, a path that is not a file's, a
 * path export refuses, or a path in conflict. Never hidden.
 */
export interface PathlessEntry {
  id: string;
  title: string;
  /*
   * Why the block is here when it carries a path; null when it has none.
   */
  note: string | null;
  /*
   * The path as stored, when there is one to show.
   */
  raw: string | null;
}

export interface FilesTree {
  root: DirectoryEntry;
  pathless: PathlessEntry[];
}

export const NOT_EXPORTABLE = 'not exportable';
export const PATH_TAKEN = 'not exported: path taken';
export const DUPLICATE_REFUSED = 'export refused: duplicate path';
export const PATH_IN_CONFLICT = 'path in conflict';
export const PATH_NOT_A_FILE = 'a path on a block export does not write as a file';

function byName(a: { name: string }, b: { name: string }): number {
  return a.name < b.name ? -1 : a.name > b.name ? 1 : 0;
}

function byLabel(a: FileEntry, b: FileEntry): number {
  return a.label < b.label ? -1 : a.label > b.label ? 1 : a.id < b.id ? -1 : a.id > b.id ? 1 : 0;
}

/*
 * WHETHER EXPORT WRITES A WHOLE FAMILY OF FILES OR NONE OF THEM.
 *
 * export-code builds every text file's output before it writes one, and
 * refuses the whole export if any text-mode file block -- wherever it sits
 * -- has no safe path, shares its path with another, or has a child that is
 * not a text-mode code block with its source as bytes (code-project.sc,
 * export-code-view). export-code --datum does the same for datum-mode
 * libraries, whose children must be datum-mode code blocks
 * (datum-project.sc, export-datum-view). Documents are not a family:
 * export-md skips a document it cannot write and writes the rest.
 *
 * The answer is the reason, in the exporter's terms, or null when the
 * family is written.
 *
 * NOTE: NOT MODELLED, BY CONTRACT: the datum exporter also refuses a library
 * whose child's doc holds a projection marker (marker-in-doc) or is not
 * whole-line comments (invalid-doc). Those are export-time answers; this
 * view shows such a library as written.
 */
export type Family = 'file' | 'library';

/*
 * Each member is judged as the exporter judges it, in the exporter's order,
 * and the first reason met is the one given: its children, then its path,
 * then a path an earlier member already has. A child is a live block whose
 * parent is the member and that the core's outline places there: a block
 * the outline promotes to the root (a cycle's, or an unplaced one) is no
 * member's child.
 */
export function familyRefusal(candidates: Candidate[], family: Family): string | null {
  const mode = family === 'file' ? 'text' : 'datum';
  const members = candidates.filter((c) => !c.deleted && c.kind === family && c.mode === mode);
  const seen = new Set<string>();
  for (const m of members) {
    const wrong = candidates.some(
      (c) =>
        !c.deleted &&
        !c.atRoot &&
        c.parent === m.id &&
        !(c.kind === 'code' && c.mode === mode && (family === 'library' || c.srcBytes))
    );
    if (wrong) {
      return family === 'file'
        ? 'export refused: a block under a text file that is not a text-mode code block'
        : 'export refused: a block under a library that is not a datum code block';
    }
    if (m.path === null || !exportablePath(m.path)) {
      return `export refused: a ${family === 'file' ? 'text file' : 'library'} with no path or one export refuses`;
    }
    if (seen.has(m.path)) {
      return DUPLICATE_REFUSED;
    }
    seen.add(m.path);
  }
  return null;
}

/*
 * THE TREE, from every block the listing read.
 *
 * Duplicates follow the exporter, by kind: of two documents with one path
 * export-md writes the first by id and not the second. Text files and
 * libraries are written as a family or not at all (`familyRefusal`): when
 * their exporter would refuse, every one of them says why. The pathless
 * group holds the root blocks that are not files.
 */
export function filesTree(candidates: Candidate[]): FilesTree {
  const files: FileEntry[] = [];
  const pathless: PathlessEntry[] = [];
  for (const c of candidates) {
    if (c.deleted) {
      continue;
    }
    const kind = fileKindOf(c);
    if (kind !== null && c.path !== null && exportablePath(c.path)) {
      const path = c.path;
      files.push({ id: c.id, path, label: path.split('/').pop() as string, title: c.title, kind, note: null });
      continue;
    }
    if (!c.atRoot) {
      continue;
    }
    if (c.pathProblem === 'conflict') {
      pathless.push({ id: c.id, title: c.title, note: PATH_IN_CONFLICT, raw: null });
    } else if (c.pathProblem === 'not-text') {
      pathless.push({ id: c.id, title: c.title, note: NOT_EXPORTABLE, raw: c.raw });
    } else if (c.path === null) {
      pathless.push({ id: c.id, title: c.title, note: null, raw: null });
    } else if (kind !== null) {
      pathless.push({ id: c.id, title: c.title, note: NOT_EXPORTABLE, raw: c.path });
    } else {
      pathless.push({ id: c.id, title: c.title, note: PATH_NOT_A_FILE, raw: c.path });
    }
  }
  const refused: Record<Family, string | null> = {
    file: familyRefusal(candidates, 'file'),
    library: familyRefusal(candidates, 'library')
  };
  for (const f of files) {
    if (f.kind !== 'doc') {
      f.note = refused[f.kind];
    }
  }
  const seen = new Map<string, FileEntry[]>();
  for (const f of files) {
    const key = `${f.kind === 'doc' ? 'doc' : f.kind}\u0000${f.path}`;
    seen.set(key, [...(seen.get(key) ?? []), f]);
  }
  for (const group of seen.values()) {
    if (group.length < 2) {
      continue;
    }
    if (group[0].kind === 'doc') {
      const first = [...group].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))[0];
      for (const f of group) {
        f.note = f === first ? null : PATH_TAKEN;
      }
    }
  }
  const root: DirectoryEntry = { prefix: '', name: '', directories: [], files: [], count: 0 };
  for (const f of files) {
    const parts = f.path.split('/');
    let at = root;
    for (let i = 0; i < parts.length - 1; i += 1) {
      const prefix = parts.slice(0, i + 1).join('/');
      let next = at.directories.find((d) => d.prefix === prefix);
      if (next === undefined) {
        next = { prefix, name: parts[i], directories: [], files: [], count: 0 };
        at.directories.push(next);
      }
      at = next;
    }
    at.files.push(f);
  }
  const settle = (d: DirectoryEntry): number => {
    d.directories.sort(byName);
    d.files.sort(byLabel);
    d.count = d.files.length + d.directories.reduce((n, sub) => n + settle(sub), 0);
    return d.count;
  };
  settle(root);
  return { root, pathless };
}

/*
 * WHETHER A STORE SHOULD OPEN IN FILES MODE: when any block at the root of
 * its outline carries a path. A store an agent wrote has none, and an empty
 * directory tree would hide it.
 */
export function filesByDefault(candidates: Candidate[]): boolean {
  return candidates.some((c) => c.atRoot && c.path !== null);
}

/*
 * THE INTENT THAT CREATES A DOCUMENT AT A PATH: one insert at the root
 * carrying the kind, the path and the title, as the core's `batch` takes it.
 * Written by the reader's own printer, so the path and title are quoted as
 * the core will read them back.
 */
export function newDocumentIntent(path: string, title: string): string {
  const w = wire();
  const field = (name: string, value: Datum) => w.dotted([w.sym(name)], value);
  return w.write([
    w.sym('insert'),
    w.sym('root'),
    false,
    [field('kind', w.sym('doc')), field('path', path), field('title', title)]
  ]);
}

/*
 * THE PATH A NEW DOCUMENT GETS: `<name>.md` in the directory it was asked
 * for, `.md` added when the name has none. Null for a name that would not
 * make one exportable path segment.
 */
export function newDocumentPath(directory: string, name: string): string | null {
  const trimmed = name.trim();
  const file = trimmed.endsWith('.md') ? trimmed : `${trimmed}.md`;
  if (trimmed.length === 0 || file.includes('/')) {
    return null;
  }
  const path = directory === '' ? file : `${directory}/${file}`;
  return exportablePath(path) ? path : null;
}

/*
 * A FILE'S PATH MOVED TO ANOTHER DIRECTORY, OR RENAMED IN ITS OWN. Null
 * when the result is not a path export would take.
 */
export function movedPath(path: string, directory: string): string | null {
  const name = path.split('/').pop() as string;
  const next = directory.trim().replace(/^\/+|\/+$/g, '');
  const moved = next === '' ? name : `${next}/${name}`;
  return exportablePath(moved) ? moved : null;
}

export function renamedPath(path: string, name: string): string | null {
  const trimmed = name.trim();
  if (trimmed.length === 0 || trimmed.includes('/')) {
    return null;
  }
  const parts = path.split('/');
  parts[parts.length - 1] = trimmed;
  const renamed = parts.join('/');
  return exportablePath(renamed) ? renamed : null;
}

/*
 * THE GROUP THAT HOLDS THE ROOT BLOCKS IN NO FILE, by the label it is shown
 * with.
 */
export const NOT_IN_ANY_FILE = 'not in any file';

/*
 * WHICH OF THE TWO WAYS THE TREE VIEW SHOWS A STORE. Files is the tree
 * export would write; Outline is the store's own parent and order.
 */
export type ViewMode = 'files' | 'outline';

export function isViewMode(value: unknown): value is ViewMode {
  return value === 'files' || value === 'outline';
}

/*
 * WHAT A PATH ACTION SAYS WHEN THE STORE REFUSED IT AS STALE: the block is
 * no longer at the version the action read, so the action was about a state
 * that has gone. The view is refreshed with it.
 */
export const CHANGED_SINCE_LISTED = 'this item changed since it was listed; the view is refreshed';

/*
 * WHETHER AN OUTCOME IS THE STORE'S `changed`: a write sent with
 * `--if-unchanged <version>` for a block that has moved on.
 */
export function changedSinceRead(outcome: { status: string; answer?: Datum | null }): boolean {
  return (
    outcome.status === 'refused' &&
    outcome.answer !== undefined &&
    outcome.answer !== null &&
    answerOf(outcome.answer, 'error', { at: 1, is: 'changed' }) !== null
  );
}

/*
 * WHAT A PATH ACTION'S OUTCOME SAYS, naming the path it was about. A
 * refusal says nothing changed; an outcome nobody knows yet says the
 * request is kept, as the Saver's own message does; a write refused as
 * stale says so, without the path it would have written.
 */
export function pathActionNotice(
  done: string,
  outcome: { status: string; message: string; answer?: Datum | null },
  path: string
): { level: 'information' | 'warning' | 'error'; text: string } {
  if (changedSinceRead(outcome)) {
    return { level: 'warning', text: CHANGED_SINCE_LISTED };
  }
  if (outcome.status === 'saved' || outcome.status === 'replayed') {
    return { level: 'information', text: `${done} ${path}` };
  }
  if (outcome.status === 'pending') {
    return { level: 'warning', text: `${path}: ${outcome.message}` };
  }
  return { level: 'error', text: `${path}: ${outcome.message}. Nothing was changed in the store.` };
}

/*
 * ONE LEVEL OF THE FILES VIEW, IN ITS ORDER: the directories, then the
 * files, each already sorted by name; at the root, the group of blocks in
 * no file after them, when there is any.
 */
export type FilesRow = { directory: DirectoryEntry } | { file: FileEntry } | { pathless: PathlessEntry[] };

export function rowsOf(directory: DirectoryEntry, pathless: PathlessEntry[] = []): FilesRow[] {
  const rows: FilesRow[] = [
    ...directory.directories.map((d) => ({ directory: d })),
    ...directory.files.map((f) => ({ file: f }))
  ];
  if (pathless.length > 0) {
    rows.push({ pathless });
  }
  return rows;
}
