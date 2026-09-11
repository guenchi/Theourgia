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
import { StoreModel, StructuralMark } from '../../src/model';
import { nodeTooltip } from '../../src/status';
import { parseOutline } from '../../src/outline';
import { CliTransport } from '../../src/transport';
import { TransportError } from '../../src/transport';
import { initWire } from '../../src/wire';
import { FakeCore } from '../support/fake';

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

  it('takes the depth from leading spaces and not from tabs', () => {
    const rows = parseOutline('- a  A\n  - b  B\n    - c  C\n      - d  D\n');
    assert.deepStrictEqual(rows.map((r) => r.depth), [0, 1, 2, 3]);
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

const SUBTREE = [
  '((id . "a.1") (deleted . #f) (fields (src . "") (title . "Doc")) (position root . 0) (edges))',
  '((id . "a.2") (deleted . #f) (fields (src . "body") (title . "Two")) (position "a.1" . 0) (edges))',
  '((id . "a.3") (deleted . #f) (fields (src . "b3") (title . "Three")) (position "a.1" . 1) (edges))',
  '((id . "a.4") (deleted . #f) (fields (src . "deep") (title . "Deeper")) (position "a.2" . 0) (edges))'
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
      { match: ['outline'], stdout: '- a.1  Doc\n- b.1  Other\n', rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 },
      { match: ['read', 'b.1'], stdout: `${BLOCK_OF['b.1']}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
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
      { match: ['outline'], stdout: '- a.1  second line\n- fake.1  invented\n', rc: 0 },
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
  const MARKS: { item: string; mark: StructuralMark }[] = [
    { item: '(conflict "a.1" cycle)', mark: 'cycle' },
    { item: '(conflict "a.1" unplaced)', mark: 'unplaced' },
    { item: '(orphan "a.1")', mark: 'orphan' },
    { item: '(nested-document "a.1")', mark: 'nested-document' }
  ];

  for (const { item, mark } of MARKS) {
    it(`reads ${item} as the ${mark} mark, and says so by name`, async () => {
      core = new FakeCore([
        { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
        { match: ['conflicts'], stdout: `${item}\n`, rc: 0 },
        { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
      ]);
      const roots = await model().roots();
      assert.strictEqual(roots[0].mark, mark);
      assert.strictEqual(roots[0].marked, true);
      assert.strictEqual(roots[0].fieldConflict, false, 'a structural mark was reported as a field conflict');
      assert.strictEqual(roots[0].orphan, mark === 'orphan');
      const tooltip = nodeTooltip(roots[0].id, roots[0].mark, roots[0].fieldConflict);
      assert.ok(tooltip !== null, 'a marked row has no tooltip');
      assert.match(tooltip as string, new RegExp(mark), `the tooltip does not name ${mark}`);
      for (const other of MARKS.map((m) => m.mark)) {
        if (other !== mark) {
          assert.ok(
            !new RegExp(`\\b${other}\\b`).test(tooltip as string),
            `the tooltip for ${mark} also names ${other}`
          );
        }
      }
    });
  }

  it('has a cell for every mark the model can produce', () => {
    /*
     * The list this checks against is written out by hand on purpose: a
     * variant added to StructuralMark and not to MARKS has no cell, and
     * this is what shouts about it. It cannot be derived from the type,
     * which is erased before it runs.
     */
    const known: StructuralMark[] = ['cycle', 'unplaced', 'orphan', 'nested-document'];
    assert.deepStrictEqual(
      MARKS.map((m) => m.mark).sort(),
      known.slice().sort(),
      'a structural mark has no cell of its own'
    );
  });

  it('does not mark a root whose title merely ends in the word', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Design notes  conflict\n', rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      {
        match: ['read', 'a.1'],
        stdout:
          '(ok ((id . "a.1") (deleted . #f) (fields (title . "Design notes  conflict")) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].title, 'Design notes  conflict', 'the title was truncated');
    assert.strictEqual(roots[0].mark, null, 'a sound block was drawn as being in a conflict');
    assert.strictEqual(roots[0].marked, false);
    assert.strictEqual(nodeTooltip(roots[0].id, roots[0].mark, roots[0].fieldConflict), null);
  });

  it('makes no mark out of an item that names no block, or a head it does not know', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
      {
        match: ['conflicts'],
        stdout:
          '(pending (event "w" 6) (missing "v" 2))\n(something-later "a.1" whatever)\n(orphan)\n',
        rc: 0
      },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].mark, null, 'a mark was invented from an item this client cannot read');
    assert.strictEqual(roots[0].marked, false);
  });

  it('keeps a field conflict distinct from a structural one', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      {
        match: ['read', 'a.1'],
        stdout:
          '(ok ((id . "a.1") (deleted . #f) (fields (title conflict ("One" "Two"))) (position root . 0) (edges)))\n',
        rc: 0
      }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].mark, null);
    assert.strictEqual(roots[0].fieldConflict, true);
    assert.strictEqual(roots[0].marked, true);
    const tooltip = nodeTooltip(roots[0].id, roots[0].mark, roots[0].fieldConflict);
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
    const children = await store.childrenOf('a.1');
    assert.deepStrictEqual(children.map((n) => n.id), ['a.2', 'a.3']);
    const requests = core.requests();
    assert.strictEqual(requests.length, 2, 'an expansion cost more than the subtree and the conflicts');
    assert.ok(
      !requests.some((r) => r[0] === 'read' && r[1] === 'a.2'),
      'a child was fetched one at a time'
    );
  });

  it('keeps the order the core answered in and drops grandchildren', async () => {
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    const children = await model().childrenOf('a.1');
    assert.deepStrictEqual(children.map((n) => n.id), ['a.2', 'a.3']);
    assert.deepStrictEqual(children.map((n) => n.title), ['Two', 'Three']);
  });

  it('reads a top-level block as a child of nothing, not of a block called root', async () => {
    core = new FakeCore([
      { match: ['read', 'root', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '', rc: 0 }
    ]);
    const children = await model().childrenOf('root');
    assert.deepStrictEqual(children, []);
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
      { match: ['outline'], stdout: '- a.1  Parent\n- a.2  invented\n', rc: 0 },
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
      { match: ['outline'], stdout: '- a.2  Old root\n', rc: 0 },
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
      { match: ['outline'], stdout: 'orphans:\n- a.2  Kid\n', rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.2")\n', rc: 0 },
      { match: ['read', 'a.2'], stdout: `${CHILD}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
    assert.deepStrictEqual(roots.map((n) => n.id), ['a.2']);
    assert.strictEqual(roots[0].mark, 'orphan');
  });

  it('marks a child the store reports as a nested document', async () => {
    const subtree = [
      '((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position root . 0) (edges))',
      '((id . "a.2") (deleted . #f) (fields (title . "Inner")) (position "a.1" . 0) (edges))'
    ].join('\n');
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${subtree}\n`, rc: 0 },
      { match: ['conflicts'], stdout: '(nested-document "a.2")\n', rc: 0 }
    ]);
    const children = await model().childrenOf('a.1');
    assert.strictEqual(children.length, 1);
    assert.strictEqual(
      children[0].mark,
      'nested-document',
      'a structural conflict that sits under another block reached the tree unmarked'
    );
  });

  it('refuses rather than reporting no conflicts when the store would not say', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
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
