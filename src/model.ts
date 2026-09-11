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
 * What the tree shows, without a tree.
 *
 * NOTHING HERE IMPORTS VS CODE. A view is a rendering of this, and a
 * rendering is the part that cannot be run in a plain process -- so the
 * questions worth pinning (how many requests an expansion costs, whose
 * children a node has, what a row's title is) are asked of this file and
 * answered without an extension host.
 *
 * THE TOP LEVEL COMES FROM `outline --depth 1` AND A SUBTREE COMES FROM
 * `read <id> --recursive`. Not from one unlimited outline: a depth limit
 * stops the core's walk rather than filtering its output, which the core
 * says is the difference between an outline and a dump, and a client
 * that asked for everything at once would pay that on every refresh.
 *
 * A SUBTREE ANSWER IS FILTERED TO THE DIRECT CHILDREN AND KEPT IN THE
 * ORDER IT ARRIVED. The core answers in document order; sorting here
 * would be a second opinion about sibling order, and the core's own
 * `ord` is the first.
 */

import { Client } from './client';
import { Block, readBlock, titleOf, hasFieldConflict } from './blocks';
import { parseOutline } from './outline';
import { TransportError } from './transport';
import { Datum, headName, isList, isSym } from './wire';

export type StructuralMark = 'cycle' | 'unplaced' | 'orphan' | 'nested-document';

export interface Node {
  id: string;
  title: string;
  /*
   * `mark` is what the STORE says is wrong with this block's place in
   * the tree; `fieldConflict` is a block whose own field has more than
   * one candidate value. They are different things with different
   * remedies, and drawing them the same way told a reader one thing when
   * the store had said the other. `marked` is only "draw an icon".
   */
  marked: boolean;
  mark: StructuralMark | null;
  fieldConflict: boolean;
  orphan: boolean;
  /*
   * WHETHER A NODE HAS CHILDREN IS NOT KNOWN UNTIL IT IS OPENED. The
   * outline was cut off at depth one and a block's own record does not
   * carry a child count, so every node is offered as expandable and an
   * expansion that finds nothing collapses again. Claiming to know would
   * mean hiding a subtree whenever the guess was wrong.
   */
  mayHaveChildren: boolean;
}

function nodeFromBlock(block: Block, mark: StructuralMark | null = null): Node {
  const fieldConflict = hasFieldConflict(block);
  return {
    id: block.id,
    title: titleOf(block),
    marked: mark !== null || fieldConflict,
    mark,
    fieldConflict,
    orphan: mark === 'orphan',
    mayHaveChildren: true
  };
}

export class StoreModel {
  private readonly client: Client;

  constructor(client: Client) {
    this.client = client;
  }

  /*
   * THE OUTLINE PROPOSES IDS AND THE STORE CONFIRMS THEM. `read <id>`
   * on every top-level row costs a request each, and buys the one thing
   * the rendering cannot give: an id that came out of a title rather
   * than out of the store answers `unknown-id`, and a block that does
   * not exist is then a refusal instead of a row in the tree.
   *
   * A ROW THAT CANNOT BE CONFIRMED STOPS THE WHOLE LISTING. Showing the
   * others would be showing a store that is smaller than it is, and the
   * reason the listing is wrong -- a title carrying a newline -- makes
   * no promise about which rows survived it.
   */
  public async roots(): Promise<Node[]> {
    const answer = await this.client.request('outline', ['--depth', '1']);
    const rows = parseOutline(answer.text);
    const marks = await this.structuralMarks();
    const out: Node[] = [];
    for (const row of rows) {
      const block = await this.blockOf(row.id);
      if (block === null) {
        throw new TransportError(
          'unreadable',
          `the outline names ${row.id} at line ${row.line}, and the store has no such block. ` +
            'A title carrying a newline can produce a line that looks like a row.',
          answer.text
        );
      }
      const mark = marks.get(row.id) ?? null;
      /*
       * EXISTING IS NOT THE SAME AS BEING AT THE TOP LEVEL. A title can
       * name a block that really does exist -- a child of the very block
       * whose title carries the line -- and `read` then confirms it,
       * putting a child in the root listing and again under its parent.
       * So the block is asked where it sits, and only a block that says
       * `root` belongs here.
       *
       * EXCEPT THE ONES THE STORE HAS ALREADY SAID ARE OUT OF PLACE. An
       * orphan still names the parent that was deleted -- measured, not
       * supposed: a real store answers `(position "<dead id>" . 0)` for
       * a block it reports under `orphan` -- so those are listed on the
       * strength of the mark rather than of their position.
       *
       * AND THIS IS ALSO WHAT CATCHES A TREE THAT NEVER EXISTED. The
       * listing and the block are read by two requests, and a block can
       * move between them; a row whose block now says it is somewhere
       * else is refused rather than drawn at a place it has left.
       */
      if (block.parent !== null && mark === null) {
        throw new TransportError(
          'unreadable',
          `the outline lists ${row.id} at line ${row.line} as a top-level block, and the store ` +
            `says it sits under ${block.parent}. Either a title carries a newline that looks ` +
            'like a row, or the block moved while the outline was being read.',
          answer.text
        );
      }
      out.push(nodeFromBlock(block, mark));
    }
    return out;
  }

  /*
   * WHAT THE STORE HOLDS AND CANNOT SHOW, as data. This is where the
   * marks come from; the outline prints them too, but prints them into
   * text a title can imitate. The core says both are read from one
   * place, so this is that place asked directly.
   *
   * AN ITEM WITH NO BLOCK IN IT IS NOT A MARK. `conflicts` also reports
   * records still waiting for their premises, which name an event and
   * not a block; those belong to whoever is repairing the store, not to
   * a tree.
   */
  public async structuralMarks(): Promise<Map<string, StructuralMark>> {
    const answer = await this.client.request('conflicts', []);
    /*
     * A QUESTION THAT WAS REFUSED IS NOT AN ANSWER OF "NONE". `conflicts`
     * exiting non-zero -- no store at that path, a store being replaced
     * underneath -- would otherwise produce an empty map, and every
     * warning in the tree would quietly go out while the reads that
     * follow still succeed.
     */
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not say what it holds and cannot show: ${answer.text.trim()}`,
        answer.text
      );
    }
    const out = new Map<string, StructuralMark>();
    for (const item of answer.answers) {
      const mark = readMark(item);
      if (mark !== null) {
        out.set(mark.id, mark.mark);
      }
    }
    return out;
  }

  /*
   * ONE REQUEST PER EXPANSION. The answer holds the whole subtree and
   * this keeps only the direct children; asking again for each
   * grandchild when it is opened costs a request that already has its
   * answer, but caching that answer would mean showing a subtree as it
   * was before someone else wrote to it.
   */
  public async childrenOf(id: string): Promise<Node[]> {
    const answer = await this.client.request('read', [id, '--recursive']);
    /*
     * TWO REQUESTS, NOT ONE, AND THE SECOND IS NOT OPTIONAL. A nested
     * document is a structural conflict that sits UNDER another block,
     * so it reaches the tree through here and not through the root
     * listing -- and without asking, it would be drawn as an ordinary
     * child with no warning on it at all. The subtree answer says what a
     * block is; only `conflicts` says what the store cannot show.
     */
    const marks = await this.structuralMarks();
    const out: Node[] = [];
    for (const item of answer.answers) {
      const block = readBlock(item);
      if (block === null || block.id === id || block.parent !== id) {
        continue;
      }
      out.push(nodeFromBlock(block, marks.get(block.id) ?? null));
    }
    return out;
  }

  public async blockOf(id: string): Promise<Block | null> {
    const answer = await this.client.request('read', [id]);
    if (!answer.ok || answer.answers.length === 0) {
      return null;
    }
    const datum = answer.answers[0];
    /*
     * `read <id>` answers `(ok (<block>))`, so the block is inside the
     * answer rather than being it. Reading the answer itself as a block
     * would find no `id` entry and report the block as missing.
     */
    if (Array.isArray(datum) && datum.length >= 2) {
      return readBlock(datum[1]);
    }
    return null;
  }

  /*
   * HOW MANY THINGS THE STORE HOLDS AND CANNOT SHOW. A refusal is not
   * zero of them: answering zero to a question that was refused is the
   * one reading a user would act on, and it would be wrong exactly when
   * something was wrong.
   */
  public async conflictCount(): Promise<number> {
    const answer = await this.client.request('conflicts', []);
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not say what it holds and cannot show: ${answer.text.trim()}`,
        answer.text
      );
    }
    return answer.answers.length;
  }
}

/*
 * `(conflict <id> cycle)`, `(conflict <id> unplaced)`, `(orphan <id>)`
 * and `(nested-document <id>)` are the items that name a block. The
 * heads are read by name; anything else -- a pending record, an item a
 * later core adds -- is left alone rather than guessed at, because a
 * mark this client invented would be worse than one it did not draw.
 */
function readMark(item: Datum): { id: string; mark: StructuralMark } | null {
  if (!isList(item) || item.length < 2 || typeof item[1] !== 'string') {
    return null;
  }
  const id = item[1];
  const head = headName(item);
  if (head === 'orphan') {
    return { id, mark: 'orphan' };
  }
  if (head === 'nested-document') {
    return { id, mark: 'nested-document' };
  }
  if (head === 'conflict' && item.length >= 3) {
    if (isSym(item[2], 'cycle')) {
      return { id, mark: 'cycle' };
    }
    if (isSym(item[2], 'unplaced')) {
      return { id, mark: 'unplaced' };
    }
  }
  return null;
}
