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
 * O1 and O2 of the X1a cell list.
 */

import * as assert from 'assert';
import { Client } from '../../src/client';
import { ROOT_MARKS, STRUCTURAL_MARKS, StoreModel, StructuralMark } from '../../src/model';
import { nodeTooltip } from '../../src/status';
import { parseOutline } from '../../src/outline';
import { CliTransport } from '../../src/transport';
import { TransportError } from '../../src/transport';
import { initWire } from '../../src/wire';
import { FakeCore } from '../support/fake';

/*
 * THE OUTLINE AS THE CORE ANSWERS IT OVER `--wire`, which is how the model
 * asks for it: the text inside one `(ok (text ...))` form. The rows are the
 * same bytes the human route prints.
 */
function outlineOverWire(text: string): string {
  return `(ok (text ${JSON.stringify(text)}))\n`;
}

describe('O1 the outline is read for the one field a title cannot forge', () => {
  it('reads the ids and their depths', () => {
    const rows = parseOutline('- a.1  T one\n  - a.2  Two  three\n- b.1  X  conflict\n');
    assert.deepStrictEqual(
      rows.map((r) => [r.id, r.depth]),
      [
        ['a.1', 0],
        ['a.2', 1],
        ['b.1', 0]
      ]
    );
  });

  /*
   * THE SUFFIX IS NOT READ ANY MORE, and this is the cell that says so.
   * A real store produced "- W.1  Design notes  conflict" from an
   * ordinary one-line title; reading the end of the line as a mark drew
   * a sound block as conflicted. The mark now comes from `conflicts`.
   */
  it('does not read the end of a row as a mark', () => {
    const rows = parseOutline('- a.1  Design notes  conflict\n');
    assert.strictEqual(rows.length, 1);
    assert.strictEqual(rows[0].id, 'a.1');
    assert.deepStrictEqual(Object.keys(rows[0]).sort(), ['depth', 'id', 'line']);
  });

  it('takes the depth from leading spaces', () => {
    const rows = parseOutline('- a  A\n  - b  B\n    - c  C\n      - d  D\n');
    assert.deepStrictEqual(rows.map((r) => r.depth), [0, 1, 2, 3]);
  });

  /*
   * A CELL NAMED "not from tabs" WITH NO TAB IN IT SAYS NOTHING. The
   * core indents with two spaces per level and never with a tab, so a
   * line indented by one is a line this cannot read -- and refusing it
   * is what stops a tab being counted as some number of levels.
   */
  it('refuses a row indented with a tab, rather than guessing its depth', () => {
    assert.throws(
      () => parseOutline('- a  A\n\t- b  B\n'),
      (e: unknown) => e instanceof TransportError && e.failure === 'unreadable'
    );
  });

  /*
   * AND THE ROW THAT MUST STILL BE ACCEPTED. A refusal cell on its own
   * is satisfied by refusing too much: "reject any outline containing a
   * tab" passes the cell above and throws away a perfectly ordinary row
   * whose TITLE holds a tab, which the grammar accepts and the core can
   * produce. The two cells together say where the line is.
   */
  it('reads a row whose title contains a tab, which is not an indent', () => {
    const rows = parseOutline('- a.1  A\tB\n');
    assert.strictEqual(rows.length, 1);
    assert.strictEqual(rows[0].id, 'a.1');
    assert.strictEqual(rows[0].depth, 0);
  });

  it('reads a row whose title is empty, and one with no title at all', () => {
    assert.strictEqual(parseOutline('- a.1  \n')[0].id, 'a.1');
    assert.strictEqual(parseOutline('- a.1\n')[0].id, 'a.1');
  });

  it('reads the orphan section as a heading and not as a row', () => {
    const rows = parseOutline('- a.1  Doc\norphans:\n- b.2  Kid\n');
    assert.deepStrictEqual(rows.map((r) => r.id), ['a.1', 'b.2']);
  });

  it('numbers the lines it read, so a refusal can point at one', () => {
    const rows = parseOutline('- a.1  Doc\norphans:\n- b.2  Kid\n');
    assert.deepStrictEqual(rows.map((r) => r.line), [1, 3]);
  });

  it('reads an empty outline as no rows at all', () => {
    assert.deepStrictEqual(parseOutline(''), []);
  });
});

/*
 * THE CHILDREN AGREE WITH NO SORT AT ALL, in either direction. The
 * first version had them ascending in every field, so any sort passed;
 * the second had them descending in every field, so a descending sort
 * passed. Document order here is a.5, a.9, a.2 -- ids neither up nor
 * down, titles Mike, Alpha, Zulu likewise, ords 2, 0, 1 -- so only
 * keeping the order the core answered in reproduces it.
 */
const SUBTREE = [
  '((id . "a.1") (deleted . #f) (fields (src . "") (title . "Doc")) (position root . 0) (edges))',
  '((id . "a.5") (deleted . #f) (fields (src . "body") (title . "Mike")) (position "a.1" . 2) (edges))',
  '((id . "a.9") (deleted . #f) (fields (src . "b3") (title . "Alpha")) (position "a.1" . 0) (edges))',
  '((id . "a.2") (deleted . #f) (fields (src . "x") (title . "Zulu")) (position "a.1" . 1) (edges))',
  '((id . "a.4") (deleted . #f) (fields (src . "deep") (title . "Deeper")) (position "a.5" . 0) (edges))'
].join('\n');

const BLOCK_OF: { [id: string]: string } = {
  'a.1': '(ok ((id . "a.1") (deleted . #f) (fields (src . "") (title . "Doc")) (position root . 0) (edges)))',
  'b.1': '(ok ((id . "b.1") (deleted . #f) (fields (src . "") (title . "Other")) (position root . 1) (edges)))'
};

describe('O2 a node is expanded when it is opened and not before', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function model(): StoreModel {
    return new StoreModel(new Client(new CliTransport(core.config(), core.env())));
  }

  it('asks for the top level with a depth limit, and confirms each id', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n- b.1  Other\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 },
      { match: ['read', 'b.1'], stdout: `${BLOCK_OF['b.1']}\n`, rc: 0 }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots.map((n) => n.id), ['a.1', 'b.1']);
    assert.deepStrictEqual(
      roots.map((n) => n.title),
      ['Doc', 'Other'],
      'the title came from the outline text instead of from the block'
    );
    assert.deepStrictEqual(core.requests()[0].slice(0, 3), ['outline', '--depth', '1']);
  });

  /*
   * A real store printed exactly this, from `insert --title "second
   * line<newline>- fake.1  invented"`. The second line parses as a row
   * and names a block that does not exist.
   */
  it('refuses a listing naming a block the store does not have', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  second line\n- fake.1  invented\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 },
      { match: ['read', 'fake.1'], stdout: '(error unknown-id "fake.1" (nearest ("a.1")))\n', rc: 1 }
    ]);
    await assert.rejects(
      () => model().roots(),
      (e: unknown) =>
        e instanceof TransportError &&
        e.failure === 'unreadable' &&
        /fake\.1/.test(e.message) &&
        /line 2/.test(e.message)
    );
  });

  /*
   * EVERY STRUCTURAL MARK THE STORE CAN REPORT, one cell each, driven
   * from a table that lists them all. Three of the four were covered by
   * hand and the fourth was not, and a mutation that made
   * `nested-document` never match survived a whole suite: parallel cells
   * are asking one question, and the one variant nobody wrote a cell for
   * is the hole. Adding a variant to StructuralMark without adding a row
   * here now fails the last cell in this block.
   */
  const MARKS: { item: string; mark: StructuralMark; promotesToRoot: boolean }[] = [
    { item: '(conflict "a.1" cycle)', mark: 'cycle', promotesToRoot: true },
    { item: '(conflict "a.1" unplaced)', mark: 'unplaced', promotesToRoot: true },
    { item: '(orphan "a.1")', mark: 'orphan', promotesToRoot: true },
    { item: '(nested-document "a.1")', mark: 'nested-document', promotesToRoot: false }
  ];

  const CHILD_OF_A1 =
    '(ok ((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position "a.9" . 0) (edges)))';

  for (const { item, mark, promotesToRoot } of MARKS) {
    it(`reads ${item} as the ${mark} mark, and says so by name`, async () => {
      core = new FakeCore([
        { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
        { match: ['conflicts'], stdout: `${item}\n`, rc: 0 },
        { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
      ]);
      const roots = (await model().roots()).nodes;
      assert.deepStrictEqual(roots[0].marks, [mark]);
      assert.strictEqual(roots[0].marked, true);
      assert.strictEqual(roots[0].fieldConflict, false, 'a structural mark was reported as a field conflict');
      assert.strictEqual(roots[0].orphan, mark === 'orphan');
      const tooltip = nodeTooltip(roots[0].id, roots[0].marks, roots[0].fieldConflict);
      assert.ok(tooltip !== null, 'a marked row has no tooltip');
      assert.match(tooltip as string, new RegExp(mark), `the tooltip does not name ${mark}`);
      for (const other of MARKS.map((m) => m.mark)) {
        if (other !== mark && !mark.includes(other)) {
          assert.ok(
            !new RegExp(`\\b${other}\\b`).test(tooltip as string),
            `the tooltip for ${mark} also names ${other}`
          );
        }
      }
    });

    it(`${promotesToRoot ? 'lets' : 'does not let'} ${mark} put a block in the root listing`, async () => {
      core = new FakeCore([
        { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
        { match: ['conflicts'], stdout: `${item}\n`, rc: 0 },
        { match: ['read', 'a.1'], stdout: `${CHILD_OF_A1}\n`, rc: 0 }
      ]);
      if (promotesToRoot) {
        const roots = (await model().roots()).nodes;
        assert.deepStrictEqual(roots.map((n) => n.id), ['a.1']);
      } else {
        await assert.rejects(
          () => model().roots(),
          (e: unknown) => e instanceof TransportError && /a\.9/.test(e.message),
          `${mark} promoted a block that sits under another one into the root listing`
        );
      }
    });

    /*
     * `nested-document` HAS THIS CELL TOO (queue item 48). It was skipped
     * while the core's recursive walk stopped at a document below the
     * root; the pinned core (F85 R1-R3: project.sc:67-79, `read
     * --recursive` through `subtree-ids`, rpc.sc:1555) answers such a
     * block under its parent like any other and reports it once under
     * `conflicts`, so it arrives through this path carrying its mark
     * (measured on the pin: its own test one-subtree.sc, rows F85-1 and
     * F85-4).
     */
    it(`carries ${mark} down to a child as well as to a root`, async () => {
      const subtree = [
        '((id . "p.1") (deleted . #f) (fields (title . "Parent")) (position root . 0) (edges))',
        '((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position "p.1" . 0) (edges))'
      ].join('\n');
      core = new FakeCore([
        { match: ['read', 'p.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
        { match: ['conflicts'], stdout: `${item}\n`, rc: 0 }
      ]);
      const children = (await model().childrenOf('p.1')).nodes;
      assert.deepStrictEqual(children.map((n) => n.marks), [[mark]]);
    });
  }

  /*
   * WHAT HAPPENS TO A NESTED DOCUMENT (queue item 48). The write path
   * refuses a document below the root, so one exists only in history made
   * before that rule or elsewhere; the pinned core then treats it as a block
   * like any other -- it is in its parent's recursive read -- and reports it
   * once, as `(nested-document <id>)` (F85 R1-R3). The tree shows it where it
   * is, as a child, with that conflict's mark; it never hides it.
   * `nested-document` is still not a mark that puts a block in the root
   * listing.
   */
  describe('a nested document is shown under its parent, with its mark', () => {
    /*
     * WITH ONE WAY IN, and it was missing from this account. A nested
     * document whose parent is then deleted is reported by the core as
     * BOTH an orphan and a nested document -- the two are computed
     * independently -- and the orphan mark is one that puts a block in
     * the root listing. So it reaches the tree as a marked root too, besides
     * being shown under its parent while the parent is alive.
     */
    it('does reach the root listing once its parent is deleted', async () => {
      core = new FakeCore([
        { match: ['outline'], stdout: outlineOverWire('orphans:\n- n.1  Inner\n'), rc: 0 },
        { match: ['conflicts'], stdout: '(orphan "n.1")\n(nested-document "n.1")\n', rc: 0 },
        {
          match: ['read', 'n.1'],
          stdout:
            '(ok ((id . "n.1") (deleted . #f) (fields (title . "Inner") (kind . doc)) (position "p.1" . 0) (edges)))\n',
          rc: 0
        }
      ]);
      const roots = (await model().roots()).nodes;
      assert.deepStrictEqual(roots.map((n) => n.id), ['n.1']);
      assert.deepStrictEqual((roots[0].marks as string[]).slice().sort(), ['nested-document', 'orphan']);
      assert.strictEqual(roots[0].orphan, true);
    });

    it('is among its parent\'s children, carrying its mark, as the core returns it', async () => {
      const subtree = [
        '((id . "p.1") (deleted . #f) (fields (title . "Parent") (kind . doc)) (position root . 0) (edges))',
        '((id . "n.1") (deleted . #f) (fields (title . "Inner") (kind . doc)) (position "p.1" . 0) (edges))'
      ].join('\n');
      core = new FakeCore([
        { match: ['read', 'p.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
        { match: ['conflicts'], stdout: '(nested-document "n.1")\n', rc: 0 }
      ]);
      const listing = await model().childrenOf('p.1');
      assert.deepStrictEqual(listing.nodes.map((n) => n.id), ['n.1'], 'the nested document is not shown under its parent');
      assert.deepStrictEqual(listing.nodes[0].marks, ['nested-document'], 'it is shown without the mark the core reported');
      assert.strictEqual(listing.marksKnown, true);
    });

    /*
     * AND ITS PARENT CAN BE OPENED (review r1 #1). A parent the tree could
     * not expand would hide the child as surely as a walk that dropped it;
     * no cell looked at `mayHaveChildren` until this one.
     */
    it('leaves its parent openable, and opening it shows the nested document with its mark', async () => {
      const subtree = [
        '((id . "p.1") (deleted . #f) (fields (title . "Parent") (kind . doc)) (position root . 0) (edges))',
        '((id . "n.1") (deleted . #f) (fields (title . "Inner") (kind . doc)) (position "p.1" . 0) (edges))'
      ].join('\n');
      core = new FakeCore([
        { match: ['outline'], stdout: outlineOverWire('- p.1  Parent\n'), rc: 0 },
        { match: ['conflicts'], stdout: '(nested-document "n.1")\n', rc: 0 },
        { match: ['read', 'p.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
        {
          match: ['read', 'p.1'],
          stdout: '(ok ((id . "p.1") (deleted . #f) (fields (title . "Parent") (kind . doc)) (position root . 0) (edges)))\n',
          rc: 0
        }
      ]);
      const roots = (await model().roots()).nodes;
      assert.deepStrictEqual(roots.map((n) => n.id), ['p.1']);
      assert.strictEqual(roots[0].mayHaveChildren, true, 'the parent cannot be opened, so its nested document is never shown');
      const listing = await model().childrenOf('p.1');
      assert.deepStrictEqual(listing.nodes.map((n) => n.id), ['n.1']);
      assert.deepStrictEqual(listing.nodes[0].marks, ['nested-document']);
    });

    it('is not promoted into the root listing by its mark either', async () => {
      core = new FakeCore([
        { match: ['outline'], stdout: outlineOverWire('- n.1  Inner\n'), rc: 0 },
        { match: ['conflicts'], stdout: '(nested-document "n.1")\n', rc: 0 },
        {
          match: ['read', 'n.1'],
          stdout:
            '(ok ((id . "n.1") (deleted . #f) (fields (title . "Inner") (kind . doc)) (position "p.1" . 0) (edges)))\n',
          rc: 0
        }
      ]);
      await assert.rejects(
        () => model().roots(),
        (e: unknown) => e instanceof TransportError && /p\.1/.test(e.message)
      );
    });
  });

  it('has a cell for every mark the model can produce', () => {
    /*
     * `STRUCTURAL_MARKS` is the value the type is derived from, so a
     * variant that is not in it does not exist and a variant added to it
     * and not to MARKS fails here. Two hand-written lists could not do
     * this: adding to the union changed neither of them.
     */
    assert.deepStrictEqual(
      MARKS.map((m) => m.mark).sort(),
      Object.keys(STRUCTURAL_MARKS).sort(),
      'a structural mark has no cell of its own'
    );
    assert.deepStrictEqual(
      MARKS.filter((m) => m.promotesToRoot).map((m) => m.mark).sort(),
      ROOT_MARKS.slice().sort(),
      'the table and the code disagree about which marks belong at the root'
    );
  });

  it('keeps every mark a block carries, not the last one read', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.1")\n(nested-document "a.1")\n', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual((roots[0].marks as string[]).slice().sort(), ['nested-document', 'orphan']);
    assert.strictEqual(roots[0].orphan, true, 'a later mark overwrote the fact that it is an orphan');
    const tooltip = nodeTooltip(roots[0].id, roots[0].marks, roots[0].fieldConflict) as string;
    assert.match(tooltip, /orphan/);
    assert.match(tooltip, /nested-document/);
  });

  it('says both when a block is marked and also has a field with candidates', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.1")\n', rc: 0 },
      {
        match: ['read', 'a.1'],
        stdout:
          '(ok ((id . "a.1") (deleted . #f) (fields (title conflict ("One" "Two"))) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots[0].marks, ['orphan']);
    assert.strictEqual(roots[0].fieldConflict, true);
    const tooltip = nodeTooltip(roots[0].id, roots[0].marks, roots[0].fieldConflict) as string;
    assert.match(tooltip, /orphan/, 'the structural mark went missing');
    assert.match(tooltip, /candidate value/, 'the field conflict went missing');
  });

  it('does not mark a root whose title merely ends in the word', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Design notes  conflict\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      {
        match: ['read', 'a.1'],
        stdout:
          '(ok ((id . "a.1") (deleted . #f) (fields (title . "Design notes  conflict")) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = (await model().roots()).nodes;
    assert.strictEqual(roots[0].title, 'Design notes  conflict', 'the title was truncated');
    assert.deepStrictEqual(roots[0].marks, [], 'a sound block was drawn as being in a conflict');
    assert.strictEqual(roots[0].marked, false);
    assert.strictEqual(nodeTooltip(roots[0].id, roots[0].marks, roots[0].fieldConflict), null);
  });

  it('makes no mark out of an item that names no block, or a head it does not know', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      {
        match: ['conflicts'],
        stdout:
          '(pending (event "w" 6) (missing "v" 2))\n(something-later "a.1" whatever)\n(orphan)\n',
        rc: 0
      },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots[0].marks, [], 'a mark was invented from an item this client cannot read');
    assert.strictEqual(roots[0].marked, false);
    /*
     * KEY: AND THE READING SAYS IT WAS NOT COMPLETE.
     *
     * This cell's own expectation is unchanged: an entry naming nobody
     * still produces no mark. What is added is the other half, which a
     * review round showed was missing -- `(orphan)` says something is
     * wrong and does not say about what, and walking past it used to
     * leave the caller a map that looked authoritative. The tree then
     * drew every block as sound with `marksKnown` true, which is the
     * one reading a user acts on, given at the moment the question went
     * unanswered. Ruled by the main session: keep the mark out, and
     * carry "I could not read all of it" downstream.
     */
    const read = await model().structuralMarks();
    assert.strictEqual(
      read.complete,
      false,
      'a mark this build recognises was lost and the reading still called itself complete'
    );
    /*
     * KEY: AND THE ROOT LISTING CARRIES IT, which is the half a
     * sixteenth review round found missing. `roots` took `.marks` and
     * dropped the flag beside it, and the tree provider then set
     * `marksKnown` to a literal `true` for the root listing -- so this
     * very answer drew every top-level block as sound while the same
     * answer one level down reported the marks unknown. Two levels of
     * one tree, opposite news, from one reading.
     */
    const listing = await model().roots();
    assert.strictEqual(
      listing.marksKnown,
      false,
      'the root listing said the marks were known, for a conflicts answer it could only partly read'
    );
  });

  /*
   * THE TWIN: a conflicts answer this build reads in full still says the
   * marks ARE known, or the notice above would be permanent and mean
   * nothing.
   */
  it('says the marks are known when it could read all of them', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.1")\n', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const listing = await model().roots();
    assert.strictEqual(listing.marksKnown, true);
    assert.deepStrictEqual(listing.nodes[0].marks, ['orphan']);
  });

  it('keeps a field conflict distinct from a structural one', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      {
        match: ['read', 'a.1'],
        stdout:
          '(ok ((id . "a.1") (deleted . #f) (fields (title conflict ("One" "Two"))) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots[0].marks, []);
    assert.strictEqual(roots[0].fieldConflict, true);
    assert.strictEqual(roots[0].marked, true);
    const tooltip = nodeTooltip(roots[0].id, roots[0].marks, roots[0].fieldConflict);
    assert.match(tooltip as string, /candidate value/);
    assert.ok(!/cycle|unplaced|orphan|nested-document/.test(tooltip as string));
  });

  /*
   * TWO REQUESTS PER EXPANSION AND NOT THREE. The subtree says what the
   * blocks are; `conflicts` says which of them the store cannot show,
   * and a nested document reaches the tree only through here. What this
   * pins is that nothing is fetched per child, and that a node nobody
   * opened costs nothing at all.
   */
  it('costs two requests per expansion and none for what is not expanded', async () => {
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    const store = model();
    const children = (await store.childrenOf('a.1')).nodes;
    assert.deepStrictEqual(children.map((n) => n.id), ['a.5', 'a.9', 'a.2']);
    const requests = core.requests();
    assert.strictEqual(requests.length, 2, 'an expansion cost more than the subtree and the conflicts');
    assert.ok(
      !requests.some((r) => r[0] === 'read' && r.length === 2),
      'a child was fetched one at a time'
    );
  });

  it('keeps the order the core answered in and drops grandchildren', async () => {
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    const children = (await model().childrenOf('a.1')).nodes;
    assert.deepStrictEqual(
      children.map((n) => n.id),
      ['a.5', 'a.9', 'a.2'],
      'the children came back in some order other than the one the core answered in'
    );
    assert.deepStrictEqual(children.map((n) => n.title), ['Mike', 'Alpha', 'Zulu']);
  });

  /*
   * KEY: ASKED OF A REAL BLOCK, because `root` is not one.
   *
   * This scripted `read root --recursive` with a success, and the pinned
   * core answers that request `(error unknown-id "root" (nearest ...))`
   * -- measured. A stand-in saying something the core never says is a
   * cell about a situation that cannot arise, and it was holding an
   * exemption open in the product: `childrenOf` had `id === 'root'`
   * written into it for these cells alone. Found in a thirteenth review
   * round; the exemption went with it.
   *
   * What the cell is really about survives unchanged: a block whose
   * `position` is the placement `root` is a child of nothing, and must
   * not be listed under a block whose id happens to be read as `root`.
   */
  it('reads a top-level block as a child of nothing, not of a block called root', async () => {
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    const children = (await model().childrenOf('a.1')).nodes;
    assert.deepStrictEqual(
      children.map((n) => n.id).sort(),
      ['a.2', 'a.5', 'a.9'],
      'a top-level block was listed among the children of a.1'
    );
  });
});

describe('a row in the listing must be a block that is actually at the top level', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function model(): StoreModel {
    return new StoreModel(new Client(new CliTransport(core.config(), core.env())));
  }

  const CHILD =
    '(ok ((id . "a.2") (deleted . #f) (fields (title . "Actual child")) (position "a.1" . 0) (edges)))';

  /*
   * A TITLE CAN NAME A BLOCK THAT REALLY EXISTS. Giving the root a title
   * of "Parent\n- a.2  invented", where a.2 is its own child, makes a
   * row that `read` confirms -- the block is there. Existing is not the
   * same as being at the top level, and without asking where it sits the
   * child appears twice: once at the root and once under its parent.
   */
  it('refuses a listing that names an existing block which is somebody\'s child', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Parent\n- a.2  invented\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 },
      { match: ['read', 'a.2'], stdout: `${CHILD}\n`, rc: 0 }
    ]);
    await assert.rejects(
      () => model().roots(),
      (e: unknown) =>
        e instanceof TransportError && /a\.2/.test(e.message) && /a\.1/.test(e.message),
      'a child was promoted into the root listing'
    );
  });

  /*
   * The same check catches a tree that never existed: the listing and
   * the block are two requests, and a block can move between them.
   */
  it('refuses a row whose block has moved since the listing was taken', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.2  Old root\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['read', 'a.2'], stdout: `${CHILD}\n`, rc: 0 }
    ]);
    await assert.rejects(
      () => model().roots(),
      (e: unknown) => e instanceof TransportError && /moved while the outline/.test(e.message)
    );
  });

  /*
   * An orphan still names the parent that was deleted -- measured on a
   * real store -- so it is listed on the strength of its mark.
   */
  it('lists an orphan although its position still names a block that is gone', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('orphans:\n- a.2  Kid\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.2")\n', rc: 0 },
      { match: ['read', 'a.2'], stdout: `${CHILD}\n`, rc: 0 }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots.map((n) => n.id), ['a.2']);
    assert.deepStrictEqual(roots[0].marks, ['orphan']);
  });

  /*
   * A block with more than one position candidate is not at the top
   * level, and saying it is would put a block whose place nobody knows
   * among the roots. It is listed only if the store has marked it.
   */
  it('refuses a row whose block cannot say where it sits', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.2  Moved twice\n'), rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      {
        match: ['read', 'a.2'],
        stdout:
          '(ok ((id . "a.2") (deleted . #f) (fields (title . "Moved twice")) (position conflict 2) (edges)))\n',
        rc: 0
      }
    ]);
    await assert.rejects(
      () => model().roots(),
      (e: unknown) => e instanceof TransportError && /more than one candidate/.test(e.message)
    );
  });

  it('lists a block with an unsettled position when the store has marked it', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.2  Moved twice\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(conflict "a.2" unplaced)\n', rc: 0 },
      {
        match: ['read', 'a.2'],
        stdout:
          '(ok ((id . "a.2") (deleted . #f) (fields (title . "Moved twice")) (position conflict 2) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = (await model().roots()).nodes;
    assert.deepStrictEqual(roots.map((n) => n.id), ['a.2']);
    assert.deepStrictEqual(roots[0].marks, ['unplaced']);
  });

  it('does not offer a block with an unsettled position as anybody\'s child', async () => {
    const subtree = [
      '((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position root . 0) (edges))',
      '((id . "a.2") (deleted . #f) (fields (title . "Moved")) (position conflict 2) (edges))'
    ].join('\n');
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(conflict "a.2" unplaced)\n', rc: 0 }
    ]);
    const children = (await model().childrenOf('a.1')).nodes;
    assert.deepStrictEqual(
      children.map((n) => n.id),
      [],
      'a block whose place is unsettled was drawn under a parent it may not have'
    );
  });

  /*
   * THE CELL THAT USED TO BE HERE SCRIPTED A NESTED DOCUMENT INTO A
   * SUBTREE ANSWER and asserted that it arrived marked, at a time when the
   * core's walk stopped at a doc-kind child and it never arrived at all;
   * it was retired then. On the pinned core (cba98ae) it does arrive,
   * and what a marked child looks like -- a nested document included -- is
   * covered by the table above, one cell per mark, and by the block about
   * nested documents.
   */

  it('refuses rather than reporting no conflicts when the store would not say', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(error no-store "/tmp/store")\n', rc: 1 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    await assert.rejects(
      () => model().roots(),
      (e: unknown) => e instanceof TransportError && /no-store/.test(e.message),
      'a refused question was read as a store with nothing wrong with it'
    );
    await assert.rejects(() => model().conflictCount(), TransportError);
  });
});

describe('marks that could not be asked for are shown as unknown, not as none', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function model(): StoreModel {
    return new StoreModel(new Client(new CliTransport(core.config(), core.env())));
  }

  const SUBTREE_OF_P1 = [
    '((id . "p.1") (deleted . #f) (fields (title . "Parent")) (position root . 0) (edges))',
    '((id . "a.1") (deleted . #f) (fields (title . "One")) (position "p.1" . 0) (edges))',
    '((id . "a.2") (deleted . #f) (fields (title . "Two")) (position "p.1" . 1) (edges))'
  ].join('\n');

  /*
   * A REFUSED READ AND A BLOCK WITH NO CHILDREN ARE ONE COLLAPSE APART.
   * The answer to a failed `read --recursive` is an error datum, and an
   * error datum is not a block -- so the filter skips it and the
   * expansion comes back empty, which is exactly what a leaf looks like.
   */
  it('refuses rather than reporting an empty subtree when the read was refused', async () => {
    core = new FakeCore([
      { match: ['read', 'p.1', '--recursive'], stdout: '(error unknown-id "p.1" (nearest ()))\n', rc: 1 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    await assert.rejects(
      () => model().childrenOf('p.1'),
      (e: unknown) => e instanceof TransportError && /p\.1/.test(e.message),
      'a store that would not read the subtree was reported as a block with no children'
    );
  });

  it('says the marks were not asked for even when there are no children at all', async () => {
    /*
     * A LEAF EXPANSION RETURNS AN EMPTY LIST EITHER WAY. If the fact
     * travelled with the nodes there would be none to carry it, and the
     * status bar beside the tree would go on reporting a count from the
     * question that had just failed.
     */
    const leaf = '((id . "a.1") (deleted . #f) (fields (title . "Leaf")) (position root . 0) (edges))';
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${leaf}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(error no-store "/tmp/store")\n', rc: 1 }
    ]);
    const listing = await model().childrenOf('a.1');
    assert.deepStrictEqual(listing.nodes, []);
    assert.strictEqual(
      listing.marksKnown,
      false,
      'an expansion with no children lost the fact that the marks were refused'
    );
  });

  it('says the marks were asked for when a leaf expansion succeeds', () => {
    return (async () => {
      const leaf = '((id . "a.1") (deleted . #f) (fields (title . "Leaf")) (position root . 0) (edges))';
      core = new FakeCore([
        { match: ['read', 'a.1', '--recursive'], stdout: `${leaf}\n`, rc: 0 },
        { match: ['conflicts'], stdout: '', rc: 0 }
      ]);
      const listing = await model().childrenOf('a.1');
      assert.deepStrictEqual(listing.nodes, []);
      assert.strictEqual(listing.marksKnown, true);
    })();
  });

  it('still shows the children when the conflicts request is refused', async () => {
    core = new FakeCore([
      { match: ['read', 'p.1', '--recursive'], stdout: `${SUBTREE_OF_P1}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(error no-store "/tmp/store")\n', rc: 1 }
    ]);
    const children = (await model().childrenOf('p.1')).nodes;
    assert.deepStrictEqual(
      children.map((n) => n.id),
      ['a.1', 'a.2'],
      'a subtree that arrived was thrown away because a second question was refused'
    );
    for (const child of children) {
      assert.strictEqual(child.marks, null, 'an unknown mark was drawn as no mark');
      assert.strictEqual(child.marked, true, 'a node whose marks are unknown was drawn as plain');
      assert.strictEqual(child.orphan, false);
      const tooltip = nodeTooltip(child.id, child.marks, child.fieldConflict);
      assert.match(tooltip as string, /structural marks unavailable/);
      assert.match(tooltip as string, /refused/);
    }
  });

  /*
   * The passing twin: when the question IS answered, nothing is unknown.
   * Without this, a model that answered `null` for everything would
   * satisfy the cell above.
   */
  it('leaves nothing unknown when the conflicts request is answered', async () => {
    core = new FakeCore([
      { match: ['read', 'p.1', '--recursive'], stdout: `${SUBTREE_OF_P1}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.2")\n', rc: 0 }
    ]);
    const children = (await model().childrenOf('p.1')).nodes;
    assert.deepStrictEqual(children.map((n) => n.marks), [[], ['orphan']]);
    for (const child of children) {
      assert.notStrictEqual(child.marks, null, 'a mark was reported as unknown although it was answered');
    }
    assert.strictEqual(nodeTooltip('a.1', children[0].marks, children[0].fieldConflict), null);
  });

  it('keeps a field conflict visible even when the marks are unknown', async () => {
    const subtree = [
      '((id . "p.1") (deleted . #f) (fields (title . "Parent")) (position root . 0) (edges))',
      '((id . "a.1") (deleted . #f) (fields (title conflict ("One" "Two"))) (position "p.1" . 0) (edges))'
    ].join('\n');
    core = new FakeCore([
      { match: ['read', 'p.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(error no-store "/tmp/store")\n', rc: 1 }
    ]);
    const children = (await model().childrenOf('p.1')).nodes;
    const tooltip = nodeTooltip(children[0].id, children[0].marks, children[0].fieldConflict) as string;
    assert.match(tooltip, /structural marks unavailable/);
    assert.match(tooltip, /candidate value/, 'the field conflict was lost behind the unknown marks');
  });

  /*
   * ROOTS ARE THE OTHER CASE ON PURPOSE. There a mark is what lets a
   * block be in the listing at all, so a refused `conflicts` leaves no
   * way to tell a real orphan from a row a title invented.
   */
  it('still refuses the root listing when the marks cannot be asked for', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: outlineOverWire('- a.1  Doc\n'), rc: 0 },
      { match: ['conflicts'], stdout: '(error no-store "/tmp/store")\n', rc: 1 },
      {
        match: ['read', 'a.1'],
        stdout: '(ok ((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    await assert.rejects(() => model().roots(), TransportError);
  });
});
