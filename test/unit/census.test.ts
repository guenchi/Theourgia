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
 * How many cells each suite file holds, written down.
 *
 * ⚠️ THIS EXISTS BECAUSE THREE CELLS WERE DELETED AND NOTHING NOTICED.
 * Rewriting a block of `two-hosts.test.ts` dropped the three that kill a
 * real process inside a publication -- the crash coverage -- and the
 * suite went green, because a suite reports what it ran and has no
 * opinion about what it used to run. It was found by counting.
 *
 * A SHORTER FILE LOOKS TIDIER. That is the whole danger: nothing about
 * a missing guard announces itself, and the run that lost it is the run
 * that reports success.
 *
 * SUBTRACTION IS ALLOWED, BUT IT HAS TO BE DELIBERATE. Lowering a number
 * here is a decision someone made; the comment beside it has to say
 * which cell went and what covers its ground now. A cell whose ground
 * nobody takes over is not being removed, it is being lost.
 *
 * GROWING IS FREE -- the check is one-sided on purpose. A guard that had
 * to be edited for every new cell would be edited without being read.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';

/*
 * Counted 2026-09-12, after X1c's cells and implementation.
 *
 * REMOVED, WITH WHO TOOK OVER THE GROUND:
 *
 *   open.test.ts (12) and placing.test.ts (4) went when `src/open.ts`
 *   and `src/placing.ts` did: X1c's wiring calls neither, and code that
 *   nothing calls but everything tests reads as maintained. What they
 *   covered is carried by `src/chain.ts` (ordering the critical
 *   sections), by immutable publication (a reading writes its own
 *   version rather than replacing another's baseline), by the sidecar
 *   (a save is recorded against the version it was split from), and by
 *   `publishInto` (one door, which asks `isOpen` before writing). The
 *   sequences themselves are written out at the top of
 *   sequences.test.ts -- that note is the reason this subtraction is a
 *   decision and not a loss.
 */
const AT_LEAST: Array<[string, number]> = [
  ['answering.test.ts', 6],
  ['census.test.ts', 3],
  ['blocks.test.ts', 20],
  ['chain.test.ts', 6],
  ['cursor.test.ts', 8],
  ['dependency-sexpr.test.ts', 15],
  ['documents.test.ts', 8],
  ['durability.test.ts', 18],
  ['fsops.test.ts', 7],
  ['host.test.ts', 8],
  ['outline.test.ts', 40],
  ['publication.test.ts', 28],
  ['real-core.test.ts', 13],
  ['saver.test.ts', 37],
  ['saving.test.ts', 23],
  ['sequences.test.ts', 15],
  ['sessions.test.ts', 29],
  ['shapes.test.ts', 34],
  ['transport.test.ts', 20],
  ['two-hosts.test.ts', 8],
  ['wire.test.ts', 9],
  ['extension.test.ts', 14],
  ['generation.test.ts', 3]
];

function sourceOf(name: string): string | null {
  for (const where of ['unit', 'integration']) {
    const file = path.join(__dirname, '..', '..', '..', 'test', where, name);
    if (fs.existsSync(file)) {
      return fs.readFileSync(file, 'utf8');
    }
  }
  return null;
}

function cellsIn(text: string): number {
  return text.split('\n').filter((line) => /^\s*it\(/.test(line)).length;
}

describe('no suite quietly gets shorter', () => {
  for (const [name, expected] of AT_LEAST) {
    it(`${name} still holds at least ${expected} cells`, () => {
      const text = sourceOf(name);
      assert.ok(text !== null, `${name} is gone; if that was meant, say here what covers its ground`);
      const found = cellsIn(text as string);
      assert.ok(
        found >= expected,
        `${name} has ${found} cells and had ${expected}. If cells were removed on purpose, lower the ` +
          'number here and write which cell went and what covers what it covered.'
      );
    });
  }

  /*
   * AND THE COUNTER CAN ACTUALLY SEE A CELL. A scanner that matched
   * nothing would report zero everywhere and every comparison would
   * fail -- which is loud -- but one that matched too much would report
   * a large number and hide a real loss, which is not.
   */
  it('counts cells and not other things that look like them', () => {
    assert.strictEqual(cellsIn("  it('a', () => {});\n  it('b', () => {});\n"), 2);
    assert.strictEqual(cellsIn("  // it('commented out', () => {});\n"), 0, 'a commented-out cell was counted');
    assert.strictEqual(cellsIn("  describe('x', () => {});\n"), 0, 'a describe was counted as a cell');
    assert.strictEqual(cellsIn("  const admitted = it;\n"), 0);
  });

  /*
   * AND EVERY SUITE FILE IS IN THE LIST. A new file that nobody listed
   * is a file whose cells can all vanish without this noticing -- the
   * same hole one level up.
   */
  it('has a number for every suite file that exists', () => {
    const listed = new Set(AT_LEAST.map(([name]) => name));
    const missing: string[] = [];
    for (const where of ['unit', 'integration']) {
      const dir = path.join(__dirname, '..', '..', '..', 'test', where);
      for (const name of fs.readdirSync(dir)) {
        if (name.endsWith('.test.ts') && !listed.has(name)) {
          missing.push(name);
        }
      }
    }
    assert.deepStrictEqual(missing, [], 'these suites are not counted, so losing their cells would be silent');
  });
});
