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
 * C4 and C18: one queue per block file.
 *
 * WHY THIS IS THE CELL THAT MATTERS. S1 through S6 were all one shape --
 * two critical sections for one file interleaving at an await. The
 * design's answer is a chain keyed by the resolved path, and these cells
 * are the only thing holding a build to it: the interleavings cannot be
 * produced through the editor's API, so an editor-hosted cell could not
 * reach them.
 *
 * ORDER, NOT FINAL STATE. C18 says so explicitly, and the reason is that
 * an implementation which DROPPED one of the operations also leaves a
 * consistent final state. Each cell below records what ran, in sequence,
 * and asserts the sequence.
 */

import * as assert from 'assert';
import * as path from 'path';
import { PathChain } from '../../src/chain';

/*
 * A critical section that announces when it starts and when it ends, and
 * whose middle can be held open by the cell. Two of these overlapping is
 * the whole hazard.
 */
function section(
  log: string[],
  name: string
): { work: () => Promise<void>; release: () => void; started: Promise<void> } {
  let release = (): void => undefined;
  let announce = (): void => undefined;
  const held = new Promise<void>((r) => {
    release = r;
  });
  const started = new Promise<void>((r) => {
    announce = r;
  });
  return {
    release: () => release(),
    started,
    work: async () => {
      log.push(`${name}:start`);
      announce();
      await held;
      log.push(`${name}:end`);
    }
  };
}

describe('C4 two critical sections for one file do not interleave', () => {
  it('runs the second only after the first has finished', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const first = section(log, 'first');
    const second = section(log, 'second');

    const a = chain.run('/s/a.md', first.work);
    await first.started;
    const b = chain.run('/s/a.md', second.work);

    /*
     * THE SECOND MUST NOT HAVE STARTED. Asserting only the end order
     * would pass for an implementation that ran both at once and merely
     * finished them in order.
     */
    await new Promise((r) => setTimeout(r, 20));
    assert.deepStrictEqual(log, ['first:start'], 'the second section began while the first was running');

    first.release();
    await a;
    second.release();
    await b;
    assert.deepStrictEqual(log, ['first:start', 'first:end', 'second:start', 'second:end']);
  });

  it('lets sections for different files run together', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const one = section(log, 'one');
    const two = section(log, 'two');

    const a = chain.run('/s/a.md', one.work);
    const b = chain.run('/s/b.md', two.work);
    await Promise.all([one.started, two.started]);
    assert.deepStrictEqual(
      log,
      ['one:start', 'two:start'],
      'two different files were serialised against each other, which would deadlock a refresh'
    );
    one.release();
    two.release();
    await Promise.all([a, b]);
  });

  it('runs what was queued behind a section that failed', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const failing = chain.run('/s/a.md', async () => {
      log.push('failing');
      throw new Error('the store refused');
    });
    const after = chain.run('/s/a.md', async () => {
      log.push('after');
    });
    await assert.rejects(() => failing, /the store refused/);
    await after;
    assert.deepStrictEqual(log, ['failing', 'after'], 'one failure stopped the queue for that file');
  });

  it('reports how deep the queue for a file is', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const held = section(log, 'held');
    const running = chain.run('/s/a.md', held.work);
    await held.started;
    const queued = chain.run('/s/a.md', async () => undefined);
    assert.strictEqual(chain.depth('/s/a.md'), 2, 'the queue does not report what is waiting on it');
    assert.strictEqual(chain.depth('/s/other.md'), 0);
    held.release();
    await Promise.all([running, queued]);
  });
});

describe('C18 the queue is keyed by the resolved path', () => {
  /*
   * THE IMPLEMENTATION THIS KILLS, named in C18: a chain keyed by the
   * string it was handed. Every cell above passes for it, because every
   * cell above spells the path one way.
   */
  it('serialises two spellings of one file', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const first = section(log, 'first');
    const second = section(log, 'second');
    /*
     * THE SPELLINGS ARE WRITTEN OUT, NOT BUILT WITH `path.join`.
     *
     * The first version of this cell made the second spelling with
     * `path.join('/s', 'dir', 'sub', '..', 'a.md')` -- which normalises
     * `sub/..` away as it builds, so both arguments were the SAME
     * STRING and the cell said nothing at all about resolution. Removing
     * every `path.resolve` from the chain left all six of these passing;
     * that was measured, after a reviewer pointed at it.
     *
     * The two assertions below are the fixture's own guard: the strings
     * must differ and must name one file. Without them this cell can
     * quietly go back to testing nothing.
     */
    const plain = '/s/dir/a.md';
    const roundabout = '/s/dir/sub/../a.md';
    assert.notStrictEqual(plain, roundabout, 'the two spellings are the same string, so nothing is being tested');
    assert.strictEqual(path.resolve(plain), path.resolve(roundabout), 'the two spellings are not one file');

    const a = chain.run(plain, first.work);
    await first.started;
    const b = chain.run(roundabout, second.work);
    await new Promise((r) => setTimeout(r, 20));
    assert.deepStrictEqual(
      log,
      ['first:start'],
      'the same file under two spellings ran two critical sections at once'
    );
    first.release();
    await a;
    second.release();
    await b;
  });

  /*
   * AND THE TWIN THAT MUST STILL PASS: one block id under two stores is
   * two files. A chain keyed by the id would serialise them and this
   * would hang.
   */
  it('does not serialise one block id across two stores', async () => {
    const chain = new PathChain();
    const log: string[] = [];
    const one = section(log, 'one');
    const two = section(log, 'two');
    const a = chain.run('/s/storeA/a.2.md', one.work);
    const b = chain.run('/s/storeB/a.2.md', two.work);
    await Promise.all([one.started, two.started]);
    assert.deepStrictEqual(log, ['one:start', 'two:start'], 'one id under two stores was treated as one file');
    one.release();
    two.release();
    await Promise.all([a, b]);
  });
});
