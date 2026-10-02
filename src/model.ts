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

import { Client, Note, mergeNotes } from './client';
import { Block, isTopLevel, parentOf, readBlock, stringField, titleOf, hasFieldConflict } from './blocks';
import { parseOutline } from './outline';
import { TransportError } from './transport';
import { Datum, Form, answerOf, isList, isSym } from './wire';
import { Hit, hitsOf, knownVerbs, rankHits } from './search';

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

/*
 * A STORE'S CONDITION, AS `check` STATES IT, or why it could not be read.
 */
export type StoreVerdict =
  | { known: true; verdict: string; notes?: Note[] | null }
  | { known: false; because: string; notes?: Note[] | null };

/*
 * THE VERDICT OUT OF A `check` ANSWER.
 *
 * NEVER: THE EXIT CODE IS NOT ASKED. The core's `rpc-ok?` makes `check` a
 * failure exactly when its verdict is not `ok`, so a damaged store answers
 * with a non-zero exit AND a complete `(check ... (verdict damaged))` --
 * measured in the pinned core's `rpc.sc` (`rpc-ok?`, rpc.sc:400-410). A reader that required `ok`
 * would read "could not ask" on precisely the occasion it exists for.
 * The form's head is what says whether this is an answer at all.
 *
 * The verdict is one symbol, read through the counted clause reader: no
 * `verdict` clause, two of them, or one that is not a symbol is a named
 * unknown, never a verdict this client made up.
 */
export function verdictOf(answers: Datum[], notes: Note[] | null = null): StoreVerdict {
  const verdict = verdictOfCheck(answers);
  return notes === null ? verdict : { ...verdict, notes };
}

function verdictOfCheck(answers: Datum[]): StoreVerdict {
  if (answers.length !== 1) {
    return { known: false, because: `check answered with ${answers.length} data where one was expected` };
  }
  const form = answerOf(answers[0], 'check');
  if (form === null) {
    return { known: false, because: 'check did not answer with a check form' };
  }
  const stated = form.value('verdict');
  if (!stated.read) {
    return { known: false, because: `the check answer's verdict is ${stated.because}` };
  }
  if (!isSym(stated.value)) {
    return { known: false, because: 'the check answer states its verdict as something other than a name' };
  }
  return { known: true, verdict: (stated.value as { name: string }).name };
}

export interface ChildListing {
  nodes: Node[];
  marksKnown: boolean;
  /*
   * THE WRITERS THIS LISTING COULD NOT SEE, from every answer it was built
   * from (the outline or subtree, the marks, the blocks). Null when every
   * writer was read. The rows are still the store's answer; the notes say
   * whose writing is not wholly in them.
   */
  notes: Note[] | null;
}

/*
 * A BLOCK AS A FILES LISTING READ IT: its record, its node, and whether it
 * sits at the root of the outline (top level, or promoted there by a mark).
 */
export interface ListedBlock {
  block: Block;
  node: Node;
  atRoot: boolean;
  /*
   * Whether export takes it as a root: top level, or a cycle or an unplaced
   * block, which the core's outline puts at the root. An orphan is listed at
   * the root and is not one (it keeps the parent that was deleted).
   */
  exportRoot: boolean;
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
  /*
   * THE BLOCK'S OWN KEYWORDS, TAKEN FROM ITS RECORD.
   *
   * NOTE: NOT FROM `outline --with-keywords`. That option exists and
   * prints them, and reading them out of it would put this back in the
   * business `outline.ts` was written to get out of: the outline is a
   * rendering with no escaping in it, and a title containing two spaces
   * and a bracket can forge any field but the id. The block is already
   * being read here for its title, so its keywords cost nothing more and
   * arrive through a channel a title cannot forge.
   */
  keywords: string;
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
    mayHaveChildren: true,
    keywords: stringField(block, 'keywords')
  };
}

/*
 * NOTE: `namesRefusal` IS GONE. It asked whether a form's head was `error`
 * and its second element a given name -- which is exactly what
 * `answerOf(datum, 'error', {at: 1, is: 'unknown-id'})` asks, in the one
 * place that checks heads. A helper beside the decoder is a second
 * reader of the same shape, and this file has already paid for one of
 * those.
 */
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
  public async roots(): Promise<ChildListing> {
    return (await this.rootsFrom((id) => this.blockReading(id))).listing;
  }

  /*
   * THE ROOT LISTING FOR FILES MODE, and every block below each root.
   *
   * Export writes a text file block wherever it sits, so a file may be below
   * a section nobody has expanded yet. Each root is therefore read with its
   * whole subtree, once per listing, in place of the one-block read the
   * outline listing makes -- the same number of requests, each answering
   * more. What the blocks carry (kind, mode, path) is read from those
   * answers; nothing is fetched again when a directory is opened.
   */
  public async filesReading(): Promise<{ listing: ChildListing; blocks: ListedBlock[] }> {
    const below: Block[] = [];
    const found = await this.rootsFrom(async (id) => {
      const answer = await this.client.request('read', [id, '--recursive', '--wire']);
      if (!answer.ok) {
        throw new TransportError(
          'unreadable',
          `the store would not read the subtree under ${id}: ${answer.text.trim()}`,
          answer.text
        );
      }
      const blocks = answer.answers.map((item) => readBlock(item));
      if (blocks.some((b) => b === null)) {
        throw new TransportError(
          'unreadable',
          `the store answered the subtree under ${id} with something that is not a block`,
          answer.text
        );
      }
      const own = (blocks as Block[]).find((b) => b.id === id) ?? null;
      below.push(...(blocks as Block[]).filter((b) => b.id !== id));
      return { block: own, notes: answer.notes ?? null };
    });
    /*
     * ONE ENTRY PER BLOCK. A block reached from two roots' reads -- a
     * promoted block whose old parent is still being read -- is listed
     * once, as a root when it is one.
     */
    const blocks: ListedBlock[] = found.roots.map((block, i) => ({
      block,
      node: found.listing.nodes[i],
      atRoot: true,
      exportRoot: isTopLevel(block) || (found.marks.get(block.id) ?? []).some((m) => m === 'cycle' || m === 'unplaced')
    }));
    const seen = new Set(blocks.map((b) => b.block.id));
    for (const block of below) {
      if (seen.has(block.id)) {
        continue;
      }
      seen.add(block.id);
      blocks.push({ block, node: nodeFromBlock(block, found.marks.get(block.id) ?? []), atRoot: false, exportRoot: false });
    }
    return { listing: found.listing, blocks };
  }

  private async rootsFrom(
    readRoot: (id: string) => Promise<{ block: Block | null; notes: Note[] | null }>
  ): Promise<{ listing: ChildListing; roots: Block[]; marks: Map<string, StructuralMark[]> }> {
    const answer = await this.client.request('outline', ['--depth', '1', '--wire']);
    /*
     * NEVER: THE EXIT CODE IS READ BEFORE THE TEXT IS.
     *
     * A refused outline has empty text, and empty text parses to an
     * outline with no rows -- a store drawn as having nothing in it at
     * the moment it could not be reached. Found in a second review
     * round; it is the same shape as the search reader had, and this
     * one is older than that reader.
     */
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not give an outline: ${answer.text.trim() || answer.stderr.trim()}`,
        answer.stderr
      );
    }
    const rows = parseOutline(answer.text);
    /*
     * NEVER: AND WHETHER THE MARKS WERE ALL THERE TRAVELS WITH THE
     * LISTING.
     *
     * This took `.marks` and dropped the completeness flag beside it,
     * and `extension.ts` then set `marksKnown` to a literal `true` for
     * the root listing -- so a conflicts answer this build could only
     * partly read drew every top-level block as sound, while the same
     * answer one level down reported that the marks were not known.
     * Measured in a sixteenth review round with a conflicts answer of
     * `(orphan)`: complete came back false and the block was listed
     * unmarked, with nothing saying so.
     *
     * NOTE: IT IS NOT A REFUSAL, and the choice is deliberate. A mark
     * decides membership here, so the listing could be argued to be
     * undrawable -- but a store that answers one unreadable mark would
     * then have no tree at all, and `childrenOf` has answered this same
     * question with `marksKnown` since the round that added it. One
     * vocabulary for one fact: the tree already knows how to say "the
     * marks were not answered" and says it at the top level now too.
     */
    const read = await this.structuralMarks();
    const marks = read.marks;
    let notes = mergeNotes(answer.notes, read.notes);
    const out: Node[] = [];
    const rootBlocks: Block[] = [];
    for (const row of rows) {
      const reading = await readRoot(row.id);
      notes = mergeNotes(notes, reading.notes);
      const block = reading.block;
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
      rootBlocks.push(block);
    }
    return { listing: { nodes: out, marksKnown: read.complete, notes }, roots: rootBlocks, marks };
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
  /*
   * NOTE: `complete` IS THE THIRD STATE'S OWN EVIDENCE.
   *
   * A mark whose head this build knows and which names nobody --
   * `(orphan)` -- is not a mark, and it is not nothing either: it says
   * something is wrong and does not say about what. Walking past it
   * left the caller with a map that looked authoritative, so the tree
   * drew every block as sound with `marksKnown` TRUE. That is the one
   * reading a user acts on, given at the moment the question went
   * unanswered.
   *
   * NEVER: IT DOES NOT REFUSE. A `conflicts` answer carrying a report this
   * build does not recognise must not stop the extension working -- the
   * core is allowed to say new things. So the marks that WERE read are
   * returned, and `complete` says whether any were lost. Ruled by the
   * main session after a review round produced the evidence above.
   */
  public async structuralMarks(): Promise<{
    marks: Map<string, StructuralMark[]>;
    complete: boolean;
    notes: Note[] | null;
  }> {
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
    /*
     * NEVER: A READING THAT COULD NOT SEE A WRITER DOES NOT KNOW THAT
     * WRITER'S MARKS. The marks read here are those of what could be read,
     * so "none" would draw every block as sound on the strength of an
     * answer that says it is not the whole store.
     */
    let complete = (answer.notes ?? null) === null;
    for (const item of answer.answers) {
      const mark = readMark(item);
      /*
       * NEVER: AN ENTRY THIS BUILD CANNOT READ IS NOT A BLOCK WITH NO
       * MARKS.
       *
       * Skipping it turned an incomplete reading into known absence:
       * `(orphan 5)` in a successful answer produced an empty map, and
       * the tree then draws every block as sound -- with `marksKnown`
       * TRUE, so nothing downstream says the question went unanswered.
       * Measured in a twelfth review round. The whole point of the
       * nullable marks is that "I could not ask" is a third state, and
       * this threw the evidence for it away.
       */
      /*
       * A HEAD THIS BUILD KNOWS, NAMING NOBODY: the mark is lost and the
       * reading is no longer complete.
       */
      if (mark === null && namesNothing(item)) {
        complete = false;
        continue;
      }
      if (mark === null && namesNoBlock(item)) {
        throw new TransportError(
          'unreadable',
          'the store answered `conflicts` with a structural mark whose block cannot be read, so ' +
            'which blocks it holds and cannot show is not known',
          answer.text
        );
      }
      if (mark === null) {
        continue;
      }
      {
        const already = out.get(mark.id) ?? [];
        if (!already.includes(mark.mark)) {
          already.push(mark.mark);
        }
        out.set(mark.id, already);
      }
    }
    return { marks: out, complete, notes: answer.notes ?? null };
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
     * A NESTED DOCUMENT ARRIVES THIS WAY TOO. The pinned core
     * (cba98ae) returns it in its parent's recursive read like any other
     * block and reports it under `conflicts`, so it comes out of here as a
     * child carrying `nested-document`, as does any child the store
     * reports under `conflicts` for another reason. (An older core's walk
     * stopped at a doc-kind child and it never arrived here.)
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
    let marksNotes: Note[] | null = null;
    try {
      const read = await this.structuralMarks();
      marksNotes = read.notes;
      /*
       * NOTE: AN INCOMPLETE READING IS REPORTED AS AN UNKNOWN ONE. Half
       * the marks and a `marksKnown` of true would be the map looking
       * authoritative again, one layer down.
       */
      marks = read.complete ? read.marks : null;
    } catch (e) {
      if (!(e instanceof TransportError)) {
        throw e;
      }
      marks = null;
    }
    /*
     * NEVER: AN EMPTY SUCCESSFUL SUBTREE READ IS NOT A BLOCK WITH NO
     * CHILDREN.
     *
     * `read <id> --recursive` includes the block itself -- the core's
     * `subtree-ids` answers the block and everything under it
     * (project.sc:180-184) -- so a success
     * carrying nothing is an answer this build cannot account for, and
     * drawing it as a leaf is the reassuring reading. The repair that
     * made the LOOP refuse an unreadable record left this case outside
     * it, because an empty list does not enter a loop. Found in an
     * eleventh review round.
     */
    if (answer.answers.length === 0) {
      throw new TransportError(
        'unreadable',
        `the store answered the subtree under ${id} with nothing at all, and a subtree always ` +
          'holds at least the block it is under',
        answer.text
      );
    }
    /*
     * NEVER: AND IT HAS TO BE THE SUBTREE THAT WAS ASKED FOR.
     *
     * The core's `read --recursive` includes the block itself
     * (`project.sc:180-184`), so an answer that never mentions it is not an
     * answer about it. Measured in a twelfth review round: a successful
     * response carrying only an unrelated root block gave
     * `{nodes: [], marksKnown: true}` -- a leaf, confidently. Checking
     * only that SOMETHING came back left this open, which is the
     * emptiness guard reaching one case short again.
     */
    /*
     * NEVER: NO EXEMPTION FOR THE PLACEMENT ROOT. There was one, and it
     * existed for a stand-in: the only caller that ever asked this with
     * `root` was a cell whose script answered a request the core then pinned
     * refused (`(error unknown-id "root" (nearest ...))`, measured). The
     * cell was corrected to ask about a real block and the exemption
     * went with it. The product asks this with block ids only -- the top
     * level comes from `outline --depth 1`.
     */
    let sawTheBlock = false;
    const nodes: Node[] = [];
    for (const item of answer.answers) {
      const block = readBlock(item);
      /*
       * NEVER: A RECORD THIS BUILD CANNOT READ IS NOT A CHILD THAT IS NOT
       * THERE.
       *
       * This skipped it, so a subtree containing one unreadable record
       * came back one child short and a subtree of nothing but
       * unreadable records came back empty -- which is what a leaf looks
       * like. It is the same collapse `blockOf` was repaired for, in the
       * other read path, and it survived that repair because the repair
       * was made where the finding pointed rather than wherever the
       * shape occurs. Found in a tenth review round.
       */
      if (block === null) {
        throw new TransportError(
          'unreadable',
          `the store answered the subtree under ${id} with something that is not a block`,
          answer.text
        );
      }
      if (block.id === id) {
        sawTheBlock = true;
        continue;
      }
      if (parentOf(block) !== id) {
        continue;
      }
      nodes.push(nodeFromBlock(block, marks === null ? null : marks.get(block.id) ?? []));
    }
    if (!sawTheBlock) {
      throw new TransportError(
        'unreadable',
        `the store answered the subtree under ${id} without mentioning ${id}, so it is not an ` +
          'answer about that block',
        answer.text
      );
    }
    return { nodes, marksKnown: marks !== null, notes: mergeNotes(answer.notes, marksNotes) };
  }

  /*
   * NULL MEANS THE STORE SAYS THERE IS NO SUCH BLOCK. It does not mean
   * the store could not be asked.
   *
   * NEVER: THE EXIT CODE ALONE CANNOT DECIDE THIS, which is why the name
   * is read. A block that is genuinely not there IS a non-zero exit --
   * `(error unknown-id "a.9" (nearest ...))` -- and so is a daemon that
   * could not be started. This returned null for both, and the caller
   * turns null into "the store has no block ...": told that, a person
   * deletes the reference. "It is not there" is the store's statement
   * and "I could not look" is the road's, and only the first belongs
   * here. Found in a review round, ruled by the main session.
   */
  public async blockOf(id: string): Promise<Block | null> {
    return (await this.blockReading(id)).block;
  }

  /*
   * THE BLOCK AND WHAT ITS READING COULD NOT SEE. What opens a block for a
   * person reads this one, so an editor opened from a store missing a
   * writer carries the notes with it.
   */
  public async blockReading(id: string): Promise<{ block: Block | null; notes: Note[] | null }> {
    const read = await this.readOne(id, [id]);
    return { block: read.block, notes: read.notes };
  }

  /*
   * THE BLOCK AS IT IS NOW, AND ITS VERSION: what an action on a listed row
   * starts from. A row is an id; its path and its version are read here, at
   * the moment of the action, never taken from the listing. The version is
   * what `set ... --if-unchanged <version>` compares against, so a block
   * changed between this read and the write is refused by the store as
   * `changed`.
   *
   * NOTE: THE CLAUSE IS `(version "<block-hash>")` BESIDE THE RECORD in the
   * answer to `read <id> --wire`: `(ok <record> (version "<hash>"))`. An
   * answer without it is one this client cannot act on, and is refused as
   * unreadable rather than written without a precondition.
   *
   * NOTE: `(version unavailable (reason "<text>"))` IS THE STORE SAYING IT
   * CANNOT COMPUTE THIS BLOCK'S VERSION. No guarded write is possible, so the
   * action is refused as well, but with that sentence and the store's reason
   * rather than as an answer this client could not read.
   */
  public async blockWithVersion(id: string): Promise<{ block: Block | null; version: string | null; notes: Note[] | null }> {
    const read = await this.readOne(id, [id, '--wire']);
    if (read.block === null) {
      return { block: null, version: null, notes: read.notes };
    }
    const whole = read.form === null ? null : read.form.whole('version');
    const unavailable = whole !== null && whole.read ? answerOf(whole.items, 'version', { at: 1, is: 'unavailable' }) : null;
    if (unavailable !== null) {
      const reason = unavailable.value('reason');
      const why = reason.read && typeof reason.value === 'string' ? `: ${reason.value}` : '';
      throw new TransportError(
        'unreadable',
        `the store cannot version this block (${id}), so its path cannot be changed safely${why}`,
        ''
      );
    }
    const version = read.form === null ? null : read.form.value('version');
    if (version === null || !version.read || typeof version.value !== 'string') {
      throw new TransportError('unreadable', `the store answered the read of ${id} without its version`, '');
    }
    return { block: read.block, version: version.value, notes: read.notes };
  }

  private async readOne(
    id: string,
    args: string[]
  ): Promise<{ block: Block | null; notes: Note[] | null; form: Form | null }> {
    const answer = await this.client.request('read', args);
    if (!answer.ok) {
      const said = answer.answers.length > 0 ? answer.answers[0] : null;
      /*
       * NEVER: AND THE REFUSAL HAS TO BE ABOUT THE BLOCK THAT WAS ASKED
       * FOR. `blockOf('a.1')` given `(error unknown-id "b.1" ...)`
       * answered null -- "a.1 is not there" -- on the strength of a
       * statement about another block. The success side was repaired for
       * this a round earlier; this side was not, because the repair was
       * made where the finding pointed. Measured in a thirteenth review
       * round.
       */
      if (
        said !== null &&
        answerOf(said, 'error', { at: 1, is: 'unknown-id' }) !== null &&
        isList(said) &&
        said.length >= 3 &&
        said[2] === id
      ) {
        return { block: null, notes: answer.notes ?? null, form: null };
      }
      throw new TransportError(
        'unreadable',
        `the store would not read ${id}: ${answer.text.trim() || answer.stderr.trim()}`,
        answer.stderr
      );
    }
    /*
     * NEVER: AND A SUCCESS THIS BUILD CANNOT READ IS NOT AN ABSENT BLOCK
     * EITHER.
     *
     * Splitting the refusals was only half of it: exit zero with no
     * output, `(ok)`, and `(ok "not a block")` all came back as null,
     * and the caller draws null as "the store has no block ...". The
     * store said yes and then said something unreadable -- which is the
     * road's problem, not the store's statement that the block is gone.
     * Found in a seventh review round.
     */
    const unreadable = (why: string): never => {
      throw new TransportError(
        'unreadable',
        `the store answered the read of ${id} with ${why}`,
        answer.text
      );
    };
    if (answer.answers.length === 0) {
      return unreadable('nothing at all');
    }
    const datum = answer.answers[0];
    /*
     * `read <id>` answers `(ok (<block>))`, so the block is inside the
     * answer rather than being it. Reading the answer itself as a block
     * would find no `id` entry and report the block as missing.
     */
    /*
     * NEVER: AND THE FORM THAT HOLDS IT HAS TO HAVE SAID `ok`. Measured in
     * an eleventh review round: `(garbage ((id . "a.1") ...))` was read
     * as block a.1, because only the length was checked. The same shape
     * as the search and catalogue readers, in a third place.
     */
    if (!Array.isArray(datum) || datum.length < 2 || answerOf(datum, 'ok') === null) {
      return unreadable('a form that holds no block');
    }
    const block = readBlock(datum[1]);
    if (block === null) {
      return unreadable('something that is not a block');
    }
    /*
     * NEVER: AND IT HAS TO BE THE BLOCK THAT WAS ASKED FOR. Measured in a
     * twelfth review round: `blockOf('a.1')` handed a record for `b.1`
     * returned b.1, and the caller then opens another block's body into
     * a buffer named for the one the user clicked. The head check made
     * the record READABLE and said nothing about whose it is.
     */
    if (block.id !== id) {
      return unreadable(`a record for ${block.id}`);
    }
    return { block, notes: answer.notes ?? null, form: answerOf(datum, 'ok') };
  }

  /*
   * HOW MANY THINGS THE STORE HOLDS AND CANNOT SHOW. A refusal is not
   * zero of them: answering zero to a question that was refused is the
   * one reading a user would act on, and it would be wrong exactly when
   * something was wrong.
   */
  /*
   * WHAT THE STORE FOUND, BEST FIRST.
   *
   * NOTE: THE WORDS GO AS ONE ARGUMENT. `search <query>` takes one, and
   * all its words must match; two words passed as two arguments get
   * `(usage (search <query>))` -- a complaint about the command line
   * that a caller could easily draw as "nothing matched". The joining
   * is here so that no caller can arrive at a different rule.
   */
  public async search(query: string): Promise<Hit[]> {
    return (await this.searchReading(query)).hits;
  }

  /*
   * THE HITS AND WHAT THE SEARCH COULD NOT SEE. The search a person runs
   * reads this one: hits from a store missing a writer are still hits,
   * and the notes say whose writing was not searched.
   */
  public async searchReading(query: string): Promise<{ hits: Hit[]; notes: Note[] | null }> {
    const answer = await this.client.request('search', [query]);
    /*
     * NEVER: THE EXIT CODE IS READ BEFORE THE BYTES ARE.
     *
     * Found by an outside review and reproduced: on the human route no
     * hits IS no output, so a refusal whose stdout was empty parsed to
     * the same empty list as a search that matched nothing -- and the
     * user was told "nothing in the store matches", about a search that
     * never happened. An answer beginning with `ok` that came with a
     * non-zero exit is not a result, and neither is an empty one.
     */
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not run the search: ${answer.text.trim() || answer.stderr.trim()}`,
        answer.stderr
      );
    }
    const hits = hitsOf(answer.answers);
    if (hits === null) {
      throw new TransportError(
        'unreadable',
        `the store did not answer the search: ${answer.text.trim()}`,
        answer.text
      );
    }
    return { hits: rankHits(hits), notes: answer.notes ?? null };
  }

  /*
   * WHICH VERBS THIS CORE HAS.
   *
   * NULL IS "I COULD NOT ASK", which is not the same as a core with no
   * verbs, and the difference decides whether an entry is hidden because
   * the core lacks it or hidden because the question failed. A caller
   * that cannot tell them apart would take a refused `describe` for a
   * core that can do nothing.
   */
  public async verbs(): Promise<Set<string> | null> {
    try {
      const answer = await this.client.request('describe', []);
      /*
       * NEVER: AND A REFUSED CATALOGUE IS NOT AN EMPTY ONE. Answering
       * with an empty set would hide every verb the core has, which is
       * the opposite of what a caller asking "can it do this?" needs.
       */
      if (!answer.ok) {
        return null;
      }
      return knownVerbs(answer.answers[0]);
    } catch (e) {
      return null;
    }
  }

  /*
   * WHAT THE STORE SAYS ABOUT ITS OWN CONDITION, asked once when a session
   * starts on it. See `verdictOf` for how the answer is read.
   *
   * NOTE: IT DOES NOT THROW. A `check` that could not be put is a named
   * unknown here, not an exception: the caller's whole policy is "say it
   * once when the store is not sound", and a store whose condition could
   * not be asked is reported by the conflict count beside it, which runs
   * at the same moment and puts the core's own sentence on the status
   * bar. Nothing here draws that unknown as "sound".
   */
  public async storeVerdict(): Promise<StoreVerdict> {
    let answer;
    try {
      answer = await this.client.request('check', []);
    } catch (e) {
      return { known: false, because: e instanceof Error ? e.message : String(e) };
    }
    return verdictOf(answer.answers, answer.notes ?? null);
  }

  public async conflictCount(): Promise<number> {
    return (await this.conflictReading()).count;
  }

  /*
   * THE COUNT AND WHAT IT COULD NOT SEE. The clause is not an item -- the
   * client took it off the answer -- so it is never counted as a conflict;
   * and a count taken without a writer is that writer's conflicts short,
   * which the notes say.
   */
  public async conflictReading(): Promise<{ count: number; notes: Note[] | null }> {
    const answer = await this.client.request('conflicts', []);
    if (!answer.ok) {
      throw new TransportError(
        'unreadable',
        `the store would not say what it holds and cannot show: ${answer.text.trim()}`,
        answer.text
      );
    }
    return { count: answer.answers.length, notes: answer.notes ?? null };
  }
}

/*
 * `(conflict <id> cycle)`, `(conflict <id> unplaced)`, `(orphan <id>)`
 * and `(nested-document <id>)` are the items that name a block. The
 * heads are read by name; anything else -- a pending record, an item a
 * later core adds -- is left alone rather than guessed at, because a
 * mark this client invented would be worse than one it did not draw.
 */
/*
 * AN ENTRY WHOSE HEAD IS A STRUCTURAL MARK AND WHOSE BLOCK CANNOT BE
 * READ.
 *
 * NOTE: THE LINE IS DRAWN AT THE HEAD, and it has to be. `conflicts`
 * carries other kinds of report -- `(pending ...)`, and whatever the
 * core adds next -- and a client that refused every item it did not
 * recognise would stop working the day the core said something new.
 * Those are skipped, as they always were; what is refused is an entry
 * this build DOES recognise and cannot read, such as `(orphan 5)`, which
 * used to be skipped and so turned an incomplete reading into known
 * absence.
 *
 * NOTE: `(orphan)` -- a known head naming nobody -- is still skipped,
 * because a cell has pinned that since before this repair. It is a
 * narrower line than the finding suggested; named in the delivery note
 * for a ruling rather than changed here.
 */
const STRUCTURAL_HEADS = ['orphan', 'nested-document', 'conflict'];

/*
 * NOTE: `(orphan)` -- A HEAD THIS BUILD KNOWS, NAMING NOBODY -- IS STILL
 * SKIPPED, AND THAT IS A RULING RATHER THAN AN OVERSIGHT.
 *
 * A fifteenth review round argued it should refuse: the entry says
 * something is wrong and does not say about what, and skipping it
 * leaves `marksKnown` TRUE, so the store is drawn as sound on the
 * strength of it. That argument is recorded in the delivery note and
 * put back to the main session with the new evidence, because the
 * behaviour is pinned by a cell that predates this batch and the main
 * session has already ruled once that it stays.
 *
 * NEVER: WHAT IS REFUSED IS AN ENTRY WITH A SUBJECT THAT CANNOT BE READ --
 * `(orphan 5)`. That one used to be skipped too, and skipping it turned
 * an incomplete reading into known absence.
 */
/*
 * A HEAD THIS BUILD RECOGNISES, CARRYING NO SUBJECT AT ALL.
 *
 * Told apart from `namesNoBlock` -- an unreadable subject -- because the
 * two get different answers: an unreadable subject is a `conflicts`
 * answer this build cannot account for and refuses; a missing one is a
 * mark that is simply lost, and losing it is reported through
 * `complete`.
 */
function namesNothing(item: Datum): boolean {
  return (
    isList(item) &&
    item.length < 2 &&
    STRUCTURAL_HEADS.some((head) => answerOf(item, head) !== null)
  );
}

function namesNoBlock(item: Datum): boolean {
  if (!isList(item) || item.length < 2) {
    return false;
  }
  return STRUCTURAL_HEADS.some((head) => answerOf(item, head) !== null) &&
    typeof item[1] !== 'string';
}

function readMark(item: Datum): { id: string; mark: StructuralMark } | null {
  if (!isList(item) || item.length < 2 || typeof item[1] !== 'string') {
    return null;
  }
  const id = item[1];
  const head = STRUCTURAL_HEADS.find((h) => answerOf(item, h) !== null) ?? null;
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
