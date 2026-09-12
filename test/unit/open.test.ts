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
 * Which reading of a block is the baseline when two opens overlap.
 *
 * THIS IS HERE BECAUSE THE EDITOR CANNOT BE MADE TO DO IT. Opening a
 * block is three VS Code waits long, nothing can widen them, and the
 * interleaving that matters -- an older open finishing after a newer one
 * -- is therefore not reachable from an editor-hosted cell. The rule was
 * lifted out of `activate` so that it could be driven directly, which is
 * the only way it is guarded at all.
 *
 * WHAT IT COSTS WHEN IT IS WRONG is not a refusal. The save path splits
 * the buffer against the baseline's prefix; a baseline older than the
 * buffer has a prefix the buffer no longer starts with, and the heading
 * is then taken for body and written into the block.
 */

import * as assert from 'assert';
import { OpenBuffers } from '../../src/open';

describe('the newest reading of a block is the one a save is measured against', () => {
  it('refuses a registration from an open that started earlier and finished later', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    const newer = open.claim();
    assert.strictEqual(open.register('/f', 'newer', newer), true);
    assert.strictEqual(
      open.register('/f', 'older', older),
      false,
      'an open that started first was allowed to overwrite a later reading'
    );
    assert.strictEqual(open.get('/f'), 'newer', 'the stale reading became the baseline');
  });

  it('takes a registration from an open that started later', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    const newer = open.claim();
    assert.strictEqual(open.register('/f', 'older', older), true);
    assert.strictEqual(open.register('/f', 'newer', newer), true);
    assert.strictEqual(open.get('/f'), 'newer', 'a newer reading was refused');
  });

  /*
   * THE ORDER IS PER FILE. Two blocks opened together must not order
   * each other: the second block's ticket is higher than the first's, so
   * a rule that compared tickets globally would refuse the first block's
   * registration and leave it with no baseline at all.
   */
  it('orders each file on its own', () => {
    const open = new OpenBuffers<string>();
    const first = open.claim();
    const second = open.claim();
    assert.strictEqual(open.register('/b', 'b', second), true);
    assert.strictEqual(open.register('/a', 'a', first), true);
    assert.strictEqual(open.get('/a'), 'a');
    assert.strictEqual(open.get('/b'), 'b');
  });

  it('re-registers the same ticket, which is one open finishing once', () => {
    const open = new OpenBuffers<string>();
    const ticket = open.claim();
    assert.strictEqual(open.register('/f', 'one', ticket), true);
    assert.strictEqual(open.register('/f', 'two', ticket), true, 'one open could not revise itself');
  });

  /*
   * A SAVE REVISES; IT DOES NOT REOPEN. Giving a save a fresh ticket
   * would promote whatever reading it revised above every open still in
   * flight, which is the stale-baseline fault arriving by another door.
   */
  it('keeps a save from promoting the reading it revised', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    const newer = open.claim();
    open.register('/f', 'older', older);
    open.revise('/f', 'older, saved');
    assert.strictEqual(
      open.register('/f', 'newer', newer),
      true,
      'a save let a stale reading outrank an open that started later'
    );
    assert.strictEqual(open.get('/f'), 'newer');
  });

  it('has no baseline for a file nobody opened', () => {
    assert.strictEqual(new OpenBuffers<string>().get('/f'), undefined);
  });
});
