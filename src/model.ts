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
 * EVERY EXPANSION ASKS `conflicts` AGAIN, AND SCANS ALL OF IT. With K
 * expansions and C things the store cannot show that is K by C items
 * looked at, and a leaf pays it too. It is not cached because a cache
 * would show a subtree as it was before someone else wrote to it, and
 * the whole of this arrangement -- ids from a rendering, everything else
 * from data -- goes away when the core offers the outline as data.
 *
 * A SUBTREE ANSWER IS FILTERED TO THE DIRECT CHILDREN AND KEPT IN THE
 * ORDER IT ARRIVED. The core answers in document order; sorting here
 * would be a second opinion about sibling order, and the core's own
 * `ord` is the first.
 */

import { Client } from './client';
import { Block, isTopLevel, parentOf, readBlock, titleOf, hasFieldConflict } from './blocks';
import { parseOutline } from './outline';
import { TransportError } from './transport';
import { Datum, headName, isList, isSym } from './wire';

/*
 * THE MARKS, AS A VALUE AND NOT ONLY AS A TYPE. A union is erased before
 * anything runs, so a cell comparing two hand-written lists proves only
 * that someone wrote the same thing twice -- adding a variant to the
 * union changed neither list and the check still passed. Deriving the
 * type FROM this record turns "a mark with no cell" into a compile
 * error, because a variant that is not here does not exist.
 */
export const STRUCTURAL_MARKS = {
  cycle: true,
  unplaced: true,
  orphan: true,
  'nested-document': true
} as const;

export type StructuralMark = keyof typeof STRUCTURAL_MARKS;

/*
 * WHICH MARKS CAN PUT A BLOCK IN THE ROOT LISTING. The core says of a
 * cycle and an unplaced block that "these appear under root and marked",
 * and it prints orphans in their own section there; a NESTED DOCUMENT is
 * none of those -- it sits under another document, which is exactly what
 * is wrong with it. Accepting any mark let a title name a nested
 * document and have it promoted to the root on the strength of being
 * marked at all.
 */
export const ROOT_MARKS: StructuralMark[] = ['cycle', 'unplaced', 'orphan'];

export interface ChildListing {
  nodes: Node[];
  marksKnown: boolean;
}

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
  /*
   * ALL OF THEM, NOT THE LAST ONE SEEN. A document whose parent was
   * deleted is BOTH an orphan and a nested document; the core computes
   * the two independently and prints orphan first, so a map keyed by id
   * kept whichever came last and the block stopped being an orphan.
   * Independent facts are not alternatives.
   */
  /*
   * NULL IS NOT "NONE". The structure of the tree and the marks on it
   * are two facts from two requests, and losing the second is no reason
   * to lose the first -- so a subtree whose `conflicts` was refused
   * still arrives, with every node saying that its marks are UNKNOWN.
   * Drawing that as "nothing wrong here" would be the reassuring answer
   * given exactly when nothing is known.
   */
  marks: StructuralMark[] | null;
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

function nodeFromBlock(block: Block, marks: StructuralMark[] | null = []): Node {
  const fieldConflict = hasFieldConflict(block);
  return {
    id: block.id,
    title: titleOf(block),
    marked: marks === null || marks.length > 0 || fieldConflict,
    marks,
    fieldConflict,
    orphan: marks !== null && marks.includes('orphan'),
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
      const found = marks.get(row.id) ?? [];
      const promotes = found.some((m) => ROOT_MARKS.includes(m));
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
      if (!isTopLevel(block) && !promotes) {
        const where =
          block.placement.kind === 'under'
            ? `says it sits under ${block.placement.parent}`
            : 'cannot say where it sits: its position has more than one candidate';
        const marked = found.length > 0 ? ` It is marked ${found.join(' and ')}.` : '';
        throw new TransportError(
          'unreadable',
          `the outline lists ${row.id} at line ${row.line} as a top-level block, and the store ` +
            `${where}. Either a title carries a newline that looks like a row, or the block ` +
            `moved while the outline was being read.${marked}`,
          answer.text
        );
      }
      out.push(nodeFromBlock(block, found));
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
  public async structuralMarks(): Promise<Map<string, StructuralMark[]>> {
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
    const out = new Map<string, StructuralMark[]>();
    for (const item of answer.answers) {
      const mark = readMark(item);
      if (mark !== null) {
        const already = out.get(mark.id) ?? [];
        if (!already.includes(mark.mark)) {
          already.push(mark.mark);
        }
        out.set(mark.id, already);
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
  /*
   * THE ANSWER SAYS WHETHER THE MARKS WERE ASKED FOR, SEPARATELY FROM
   * THE CHILDREN. A caller cannot read that off the nodes: an expansion
   * of a block with no children returns an empty list whether the marks
   * were refused or answered, and the status bar beside it would go on
   * reporting a count from a question that has since failed.
   */
  public async childrenOf(id: string): Promise<ChildListing> {
    const answer = await this.client.request('read', [id, '--recursive']);
    /*
     * A REFUSED READ IS NOT A BLOCK WITH NO CHILDREN. The answer to a
     * failed `read --recursive` is an error datum, and an error datum is
     * not a block -- so the loop below skips it and the expansion comes
     * back empty, which is what a leaf looks like. The store refusing to
     * say and the store saying "nothing" are one collapse apart.
     */
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not read the subtree under ${id}: ${answer.text.trim()}`,
        answer.text
      );
    }
    /*
     * TWO REQUESTS, NOT ONE, AND THE SECOND IS NOT OPTIONAL. The subtree
     * answer says what a block IS; only `conflicts` says what the store
     * holds and cannot show, and without asking, a marked block would be
     * drawn as an ordinary child with no warning on it at all.
     *
     * A NESTED DOCUMENT IS NOT WHAT THIS IS FOR, and the comment here
     * used to say it was. The core's walk stops at a doc-kind child, so
     * one never arrives through this path; what does arrive marked is
     * any child the store reports under `conflicts` for another reason.
     */
    /*
     * A REFUSAL HERE COSTS THE MARKS AND NOT THE CHILDREN. Which blocks
     * are under this one is already answered; whether any of them is a
     * nested document is a second question, and a subtree that vanished
     * because the second was refused would hide what the first said.
     *
     * ROOTS ARE NOT THE SAME CASE and do not do this. There the mark is
     * what decides MEMBERSHIP -- a legitimate orphan is listed on the
     * strength of it -- so without the marks there is no way to tell one
     * from a row a title invented, and the listing is refused instead.
     */
    let marks: Map<string, StructuralMark[]> | null;
    try {
      marks = await this.structuralMarks();
    } catch (e) {
      if (!(e instanceof TransportError)) {
        throw e;
      }
      marks = null;
    }
    const nodes: Node[] = [];
    for (const item of answer.answers) {
      const block = readBlock(item);
      if (block === null || block.id === id || parentOf(block) !== id) {
        continue;
      }
      nodes.push(nodeFromBlock(block, marks === null ? null : marks.get(block.id) ?? []));
    }
    return { nodes, marksKnown: marks !== null };
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
