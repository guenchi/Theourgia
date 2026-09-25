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
 * WHAT A BLOCK'S PROJECTION FILE IS CALLED, AND HOW IT IS FOUND AGAIN.
 * (queue item 5, designed by the main session 2026-09-25)
 *
 * Every block's projection used to be `current.md`, so every editor tab
 * read "current.md". A new projection is now named `<slug>-<blockId>.md`
 * -- the slug first, because a tab truncates its tail and the title is
 * what a person reads; the id after it, so two blocks with one title stay
 * apart -- and `<blockId>.md` when the title leaves no slug.
 *
 * KEY: THE NAME IS CHOSEN ONCE, AT THE FIRST PUBLICATION, AND NEVER AGAIN.
 * A title that changes later does not rename the file: renaming would have
 * to move a file the editor may have open, its sidecar and whatever owner
 * records name it. And nothing reads the name back: the block id is in the
 * directory's name and in the sidecar, and a directory's projection is
 * FOUND, by `projectionFileIn`, not spelled. So the format below can change
 * -- it is a user-visible choice and may be revisited -- by changing
 * `slugOf` alone; old names stay readable, `current.md` included.
 */

import * as path from 'path';
import { FileOps } from './fsops';

const SLUG_LIMIT = 40;

/*
 * THE SLUG OF A TITLE, a pure function.
 *
 * NFC first, so one title typed two ways gets one slug. Unicode letters and
 * digits are kept as they are -- a Chinese title keeps its characters, it is
 * not transliterated -- and so are combining marks, without which a word in a
 * script that writes vowels as marks (Devanagari, for one) would be cut into
 * pieces; ASCII letters are lower-cased, other letters are left alone.
 * "Digits" are decimal digits (`\p{Nd}`), in any script; a number that is not
 * a digit -- a fraction, a circled number, a Roman numeral (U+00BD, U+2460,
 * U+216B) -- is not kept (review r1 of item 5 found `\p{N}` here, which kept
 * them).
 * Everything else becomes `-`: white space, the characters a path cannot hold
 * on some system (`/ \ : * ? " < > |`), control characters -- and, reading
 * the design's "a title of symbols alone is empty" as meaning it, all other
 * punctuation too; runs of `-` become one; a leading or trailing `-`
 * is dropped, which also removes a leading `.`. At most 40 code points, then
 * a trailing `-` is dropped again. A title with nothing left has an empty
 * slug.
 */
export function slugOf(title: string): string {
  const kept = Array.from(title.normalize('NFC'))
    .map((ch) => (/[\p{L}\p{M}\p{Nd}]/u.test(ch) ? (/[A-Z]/.test(ch) ? ch.toLowerCase() : ch) : '-'))
    .join('')
    .replace(/-+/g, '-')
    .replace(/^-+|-+$/g, '');
  return Array.from(kept).slice(0, SLUG_LIMIT).join('').replace(/-+$/, '');
}

/*
 * THE TITLE A PROJECTION IS NAMED AFTER: the first line of the block's
 * prefix, without its `#` marks. A block whose prefix is not a heading has
 * no title here, and its projection is `<blockId>.md`.
 */
export function titleOfPrefix(prefix: string): string {
  const first = prefix.split('\n', 1)[0] ?? '';
  const heading = /^#{1,6}\s+(.*)$/.exec(first);
  return heading === null ? '' : heading[1].trim();
}

export function projectionNameFor(title: string, blockId: string): string {
  const slug = slugOf(title);
  return slug.length > 0 ? `${slug}-${blockId}.md` : `${blockId}.md`;
}

/*
 * A FILE THAT IS A PROJECTION OR ITS SIDECAR, by its name. `temporaryFor`
 * asks this to stage such a file OUTSIDE its block directory, so that a
 * temporary never sits in the directory `projectionFileIn` reads. It used to
 * ask whether the name began with `current.md`.
 */
export function isProjectionPath(file: string): boolean {
  return /\.md(\.meta)?$/.test(path.basename(file));
}

/*
 * NUMBERED VERSIONS -- `1.md`, `2.md.meta` -- are the layout before
 * `current.md`. They are migration's to move; they are never this
 * directory's projection.
 */
function isLegacyVersion(name: string): boolean {
  return /^\d+\.md(\.meta)?$/.test(name);
}

export type ProjectionLookup =
  | { found: 'none' }
  | { found: 'one'; file: string }
  | { found: 'unknown'; names: string[] };

/*
 * THE PROJECTION IN A BLOCK DIRECTORY: the one `.md` that has a sidecar.
 * It is found by its sidecar, because a publication interrupted after the
 * sidecar and before the file leaves the sidecar alone, and that directory
 * still has a projection -- one to recover, not one to make afresh.
 *
 * `none` is a directory with no `.md` at all (numbered legacy versions
 * aside): the block has not been published here, and a first publication
 * names its file. `unknown` is anything else that is not exactly one --
 * several `.md` with sidecars, or `.md` files and none of them with a
 * sidecar -- and the caller refuses by name, listing what it saw, rather
 * than choosing, or writing a second file beside one it does not know.
 */
export function projectionFileIn(files: FileOps, directory: string): ProjectionLookup {
  const listed = files.list(directory).filter((name) => !isLegacyVersion(name));
  const withSidecar = listed
    .filter((name) => name.endsWith('.md.meta'))
    .map((name) => name.slice(0, -'.meta'.length))
    .sort();
  if (withSidecar.length === 1) {
    return { found: 'one', file: path.join(directory, withSidecar[0]) };
  }
  if (withSidecar.length > 1) {
    return { found: 'unknown', names: withSidecar };
  }
  const bare = listed.filter((name) => name.endsWith('.md')).sort();
  return bare.length === 0 ? { found: 'none' } : { found: 'unknown', names: bare };
}
