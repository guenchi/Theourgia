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
import { StoreModel } from '../../src/model';
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

  it('marks a root the store reports as being in a structural conflict', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].mark, 'cycle');
    assert.strictEqual(roots[0].marked, true);
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
  });

  it('reads an orphan as an orphan, from the data and not from the section heading', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: 'orphans:\n- a.1  Doc\n', rc: 0 },
      { match: ['conflicts'], stdout: '(orphan "a.1")\n', rc: 0 },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].orphan, true);
    assert.strictEqual(roots[0].mark, 'orphan');
  });

  it('ignores a conflicts item that names no block', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n', rc: 0 },
      {
        match: ['conflicts'],
        stdout: '(pending (event "w" 6) (missing "v" 2))\n(conflict "a.1" unplaced)\n',
        rc: 0
      },
      { match: ['read', 'a.1'], stdout: `${BLOCK_OF['a.1']}\n`, rc: 0 }
    ]);
    const roots = await model().roots();
    assert.strictEqual(roots[0].mark, 'unplaced');
  });

  it('costs one request per expansion and none for what is not expanded', async () => {
    core = new FakeCore([
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 }
    ]);
    const store = model();
    const children = await store.childrenOf('a.1');
    assert.deepStrictEqual(children.map((n) => n.id), ['a.2', 'a.3']);
    const requests = core.requests();
    assert.strictEqual(requests.length, 1, 'an expansion cost more than one request');
  });

  it('keeps the order the core answered in and drops grandchildren', async () => {
    core = new FakeCore([{ match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 }]);
    const children = await model().childrenOf('a.1');
    assert.deepStrictEqual(children.map((n) => n.id), ['a.2', 'a.3']);
    assert.deepStrictEqual(children.map((n) => n.title), ['Two', 'Three']);
  });

  it('reads a top-level block as a child of nothing, not of a block called root', async () => {
    core = new FakeCore([{ match: ['read', 'root', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 }]);
    const children = await model().childrenOf('root');
    assert.deepStrictEqual(children, []);
  });
});
