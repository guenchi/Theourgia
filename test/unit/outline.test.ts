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
import { initWire } from '../../src/wire';
import { FakeCore } from '../support/fake';

describe('O1 the outline is read as the core drew it', () => {
  it('reads three rows, their depths and a mark', () => {
    const rows = parseOutline('- a.1  T one\n  - a.2  Two  three\n- b.1  X  conflict\n');
    assert.strictEqual(rows.length, 3);
    assert.deepStrictEqual(
      rows.map((r) => [r.id, r.title, r.depth, r.mark]),
      [
        ['a.1', 'T one', 0, null],
        ['a.2', 'Two  three', 1, null],
        ['b.1', 'X', 0, 'conflict']
      ]
    );
  });

  it('splits at the first double space only, so a title may contain one', () => {
    const rows = parseOutline('- a.2  Two  three  four\n');
    assert.strictEqual(rows[0].title, 'Two  three  four');
  });

  it('takes the depth from leading spaces and not from tabs', () => {
    const rows = parseOutline('- a  A\n  - b  B\n    - c  C\n      - d  D\n');
    assert.deepStrictEqual(rows.map((r) => r.depth), [0, 1, 2, 3]);
  });

  it('reads the unplaced mark as well as the conflict one', () => {
    const rows = parseOutline('- a.1  One  unplaced\n');
    assert.strictEqual(rows[0].mark, 'unplaced');
    assert.strictEqual(rows[0].title, 'One');
  });

  it('reads a row whose title is empty', () => {
    const rows = parseOutline('- a.1  \n');
    assert.strictEqual(rows[0].id, 'a.1');
    assert.strictEqual(rows[0].title, '');
  });

  it('reads the orphans section as rows that are nobody\'s children', () => {
    const rows = parseOutline('- a.1  Doc\norphans:\n- b.2  Kid\n');
    assert.strictEqual(rows.length, 2);
    assert.strictEqual(rows[1].id, 'b.2');
    assert.strictEqual(rows[1].orphan, true);
    assert.strictEqual(rows[0].orphan, false);
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

describe('O2 a node is expanded when it is opened and not before', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function model(): StoreModel {
    return new StoreModel(new Client(new CliTransport(core.config(), core.env())));
  }

  it('asks for the top level with a depth limit', async () => {
    core = new FakeCore([{ match: ['outline'], stdout: '- a.1  Doc\n- b.1  Other\n', rc: 0 }]);
    const roots = await model().roots();
    assert.deepStrictEqual(roots.map((n) => n.id), ['a.1', 'b.1']);
    assert.deepStrictEqual(core.requests()[0].slice(0, 3), ['outline', '--depth', '1']);
  });

  it('costs one request per expansion and none for what is not expanded', async () => {
    core = new FakeCore([
      { match: ['outline'], stdout: '- a.1  Doc\n- b.1  Other\n', rc: 0 },
      { match: ['read', 'a.1', '--recursive'], stdout: `${SUBTREE}\n`, rc: 0 }
    ]);
    const store = model();
    await store.roots();
    const children = await store.childrenOf('a.1');
    assert.deepStrictEqual(children.map((n) => n.id), ['a.2', 'a.3']);
    const requests = core.requests();
    assert.strictEqual(requests.length, 2, 'an expansion cost more than one request');
    assert.ok(
      !requests.some((r) => r[1] === 'b.1'),
      'a node nobody opened was fetched anyway'
    );
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
