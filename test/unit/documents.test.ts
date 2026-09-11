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
 * The file a block is edited in, and how it says whether it holds work
 * the store has not got.
 *
 * THE EDITOR'S DIRTY FLAG CANNOT ANSWER THIS. The save handler runs
 * after the file has been written, so a save the core refused leaves a
 * CLEAN buffer holding text that never reached the store. And a second
 * editor window computes the same path for the same block while being
 * invisible to this one. Both are cases where "not dirty" and "nothing
 * to lose" are different things.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { BlockDocument } from '../../src/blocks';
import { documentPathFor, fileNameFor, hasUncommittedWork, markCommitted, writeDocument } from '../../src/documents';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-doc-'));
}

function document(id: string, text: string): BlockDocument {
  return { id, store: '/stores/one', headingSrc: '## Two\n', src: text, text: `## Two\n${text}` };
}

describe('a file that is clean may still hold work the store has not got', () => {
  it('says a freshly written file holds nothing uncommitted', () => {
    const file = path.join(scratch(), 'a.md');
    writeDocument(file, document('a.2', 'body\n'));
    assert.strictEqual(hasUncommittedWork(file), false);
  });

  it('says a file nobody has written holds nothing', () => {
    assert.strictEqual(hasUncommittedWork(path.join(scratch(), 'never.md')), false);
  });

  it('notices an edit written to the file after it was taken from the store', () => {
    const file = path.join(scratch(), 'a.md');
    writeDocument(file, document('a.2', 'body\n'));
    fs.writeFileSync(file, '## Two\nedited and refused\n', 'utf8');
    assert.strictEqual(
      hasUncommittedWork(file),
      true,
      'a save the core refused would be overwritten the next time the block was opened'
    );
  });

  it('forgets the edit once the store has confirmed it', () => {
    const file = path.join(scratch(), 'a.md');
    writeDocument(file, document('a.2', 'body\n'));
    fs.writeFileSync(file, '## Two\nedited and accepted\n', 'utf8');
    assert.strictEqual(hasUncommittedWork(file), true);
    markCommitted(file);
    assert.strictEqual(hasUncommittedWork(file), false);
  });

  it('treats a file it has no record of as holding work', () => {
    const file = path.join(scratch(), 'a.md');
    fs.writeFileSync(file, 'written by something else\n', 'utf8');
    assert.strictEqual(
      hasUncommittedWork(file),
      true,
      'a file from another window or an older version would be overwritten'
    );
  });

  it('keeps one block per store and one file per block', () => {
    const storage = scratch();
    const one = documentPathFor(storage, '/stores/one', 'a.2');
    const two = documentPathFor(storage, '/stores/two', 'a.2');
    const other = documentPathFor(storage, '/stores/one', 'a.3');
    assert.notStrictEqual(one, two, 'two stores share a file for the same id');
    assert.notStrictEqual(one, other);
    assert.strictEqual(one, documentPathFor(storage, '/stores/one', 'a.2'));
  });

  it('makes a file name out of an id that is not a file name', () => {
    assert.notStrictEqual(fileNameFor('a/../b'), fileNameFor('a_.._b'));
    assert.ok(!fileNameFor('a/../b').includes('/'));
    assert.notStrictEqual(fileNameFor('Abc'), fileNameFor('abc'));
  });
});
