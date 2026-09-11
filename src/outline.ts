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
 * Reading the outline the core drew, for the ONE thing it is the only
 * source of.
 *
 * THE OUTLINE IS A RENDERING, AND ONLY ITS IDS ARE UNAMBIGUOUS. It is
 * text with no escaping in it: a title is written raw, and a title can
 * contain the very things that separate one field from the next. Two
 * real cases, both produced by the core from an ordinary store:
 *
 *     insert --title "Design notes  conflict"
 *       gives "- W.1  Design notes  conflict", and reading the suffix as
 *       a mark shows a sound block as being in a structural conflict
 *
 *     insert --title "second line<newline>- fake.1  invented"
 *       gives "- W.2  second line" and then "- fake.1  invented", and
 *       reading the second of those as a row invents a block
 *
 * An id cannot contain a space, so the id is the one field whose
 * boundary no title can forge. Everything else -- the title, whether the
 * block is in a structural conflict, whether it is an orphan -- is asked
 * for as DATA: `read <id>` for the title, `conflicts` for the marks. The
 * core states that `outline` takes its marks from the same place
 * `conflicts` does, so this is the same authority reached by a channel
 * that cannot be forged, and not a second opinion about what an outline
 * means.
 *
 * WHAT IS LEFT HERE IS THE SHAPE OF A ROW AND ITS DEPTH: two spaces per
 * level, then "- ", then the id. A line that is not that stops the whole
 * outline, because a line this cannot read is a block it would otherwise
 * drop, and a shorter outline reads like a smaller store.
 *
 * THIS FILE GOES AWAY when the core offers the outline as data -- one
 * item per block carrying id, parent, ord, title and mark -- at which
 * point there is nothing here left to get wrong.
 */

import { TransportError } from './transport';

export interface OutlineRow {
  id: string;
  depth: number;
  line: number;
}

/*
 * The id runs to the first space. The core writes two spaces after it
 * and then the title, and neither is read here.
 */
const ROW = /^( *)- (\S+)(?:  .*)?$/;

export function parseOutline(text: string): OutlineRow[] {
  const rows: OutlineRow[] = [];
  const lines = text.split('\n');
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (line.length === 0) {
      continue;
    }
    /*
     * THE ORPHAN SECTION IS A HEADING, NOT A ROW, and it is no longer
     * read for anything else: which blocks are orphans is answered by
     * `conflicts`, so a title whose second line happens to say
     * `orphans:` can no longer change what any row means.
     */
    if (line === 'orphans:') {
      continue;
    }
    const found = ROW.exec(line);
    if (found === null) {
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
    rows.push({ id: found[2], depth: Math.floor(found[1].length / 2), line: i + 1 });
  }
  return rows;
}
