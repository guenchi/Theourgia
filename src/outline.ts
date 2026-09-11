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
 * Reading the outline the core drew.
 *
 * THE OUTLINE IS TEXT AND THE CORE RENDERS IT. rpc.ss says why: a caller
 * given the rows and left to draw them would be a second opinion about
 * what an outline looks like, and the two would differ first at the rows
 * that are hardest to draw. So this file parses a rendering rather than
 * producing one, and every rule here is read off outline-text:
 *
 *   two spaces per level, then "- ", then the id, then TWO spaces, then
 *   the title, and then -- only when the row is in a structural conflict
 *   -- two more spaces and the mark, which is `conflict` or `unplaced`.
 *
 * THE TITLE IS SPLIT AT THE FIRST DOUBLE SPACE AND NOT AT EVERY ONE. A
 * title may contain two spaces of its own; the separator is the first
 * one after the id, because the id cannot contain a space at all.
 *
 * THE `orphans:` LINE IS A SECTION AND NOT A ROW. Blocks whose parent
 * was deleted are printed under it at depth zero; they are still blocks
 * and are still worth showing, but they are not children of anything.
 */

import { TransportError } from './transport';

export type OutlineMark = 'conflict' | 'unplaced';

export const OUTLINE_MARKS: OutlineMark[] = ['conflict', 'unplaced'];

export interface OutlineRow {
  id: string;
  title: string;
  depth: number;
  mark: OutlineMark | null;
  orphan: boolean;
}

const ROW = /^( *)- (\S+)( {2})?(.*)$/;

export function parseOutline(text: string): OutlineRow[] {
  const rows: OutlineRow[] = [];
  let inOrphans = false;
  const lines = text.split('\n');
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (line.length === 0) {
      continue;
    }
    if (line === 'orphans:') {
      inOrphans = true;
      continue;
    }
    const found = ROW.exec(line);
    if (found === null) {
      /*
       * A LINE THIS CANNOT READ STOPS THE WHOLE OUTLINE. Skipping it
       * would hide a block -- and the rows hardest to draw are exactly
       * the ones in a structural conflict, which is to say the rows a
       * reader most needs to see. A shorter outline reads like a smaller
       * store.
       */
      /*
       * THE LINE NUMBER AND THE LINE ITSELF, because the block this
       * belongs to has to be findable. A title carrying a newline is
       * what produces this, and the row above the reported line is the
       * one to go and look at.
       */
      throw new TransportError(
        'unreadable',
        `the outline could not be read at line ${i + 1}: ${JSON.stringify(line)}. ` +
          (rows.length > 0
            ? `The row before it is ${rows[rows.length - 1].id}, whose title may carry a newline.`
            : 'No row was read before it.'),
        text
      );
    }
    const indent = found[1].length;
    const id = found[2];
    const rest = found[4];
    const { title, mark } = splitTitle(rest);
    rows.push({
      id,
      title,
      depth: inOrphans ? 0 : Math.floor(indent / 2),
      mark,
      orphan: inOrphans
    });
  }
  return rows;
}

/*
 * THE MARK IS TAKEN OFF THE END, NOT FOUND BY SPLITTING. Splitting the
 * remainder at its double spaces would cut a title that has two spaces
 * in it; the mark is one of two known words and sits last, so the only
 * question that can be answered without guessing is whether the text
 * ends with one of them after two spaces.
 */
function splitTitle(rest: string): { title: string; mark: OutlineMark | null } {
  for (const mark of OUTLINE_MARKS) {
    const suffix = `  ${mark}`;
    if (rest.endsWith(suffix)) {
      return { title: rest.slice(0, rest.length - suffix.length), mark };
    }
  }
  return { title: rest, mark: null };
}
