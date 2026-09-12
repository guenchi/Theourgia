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
 * How many cells each suite actually registers, written down.
 *
 * ⚠️ THIS GUARD HAD THE DISEASE IT WAS BUILT TO CATCH. Its first version
 * counted `it(` lines with a regular expression. A reviewer commented
 * out a whole suite -- the three cells that kill a real process inside a
 * publication -- with a block comment: the cells mocha would run fell
 * from eight to five, and all twenty-seven checks passed. A regular
 * expression reads text; what matters is what runs.
 *
 * IT ALSO COULD NOT SEE TABLE-DRIVEN CELLS. Several suites build their
 * cells from a list of rows, so removing a row removes a cell while the
 * source keeps exactly as many `it(` lines as before. Measured: the
 * counts below differ from the line counts by 30 cells across the tree.
 *
 * SO IT ASKS MOCHA. `--dry-run` loads the suites and reports every test
 * it WOULD run, attributed to its file, without running one. A commented
 * cell is not there; a cell made by a loop is.
 *
 * IN A SEPARATE PROCESS, ON PURPOSE. Walking the current run's suite
 * tree would report whatever this invocation happened to load, so
 * running one file would make the guard vacuous. The dry run always
 * loads the whole unit tree, however this run was started.
 *
 * THE EDITOR-HOSTED SUITES ARE COUNTED WHERE THEY LIVE. They import
 * `vscode`, which exists only inside the extension host, so a plain node
 * process cannot load them at all; their count is asserted by a cell in
 * that suite instead.
 *
 * SUBTRACTION IS ALLOWED AND HAS TO BE DELIBERATE: lower the number and
 * say here which cell went and what covers its ground. Growth is free,
 * so that a guard needing an edit per new cell is not edited unread.
 */

import * as assert from 'assert';
import { execFileSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { countRegistered, EDITOR_CELLS, shortfalls } from '../integration/census';

/*
 * Counted 2026-09-12 by dry run, after X1c's wiring.
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
 *   sequences.test.ts -- that note is why this subtraction is a
 *   decision and not a loss.
 */
const AT_LEAST: Array<[string, number]> = [
  ['activation.test.ts', 4],
  ['answering.test.ts', 27],
  ['blocks.test.ts', 20],
  /*
   * ⚠️ INCLUDING ITSELF. This file was exempt from the inventory check
   * and had no floor, so deleting its own cells was the one shortening
   * nothing here would have said a word about.
   */
  ['census.test.ts', 17],
  ['chain.test.ts', 6],
  ['commands.test.ts', 5],
  ['cursor.test.ts', 8],
  ['dependency-sexpr.test.ts', 15],
  ['documents.test.ts', 8],
  ['durability.test.ts', 31],
  ['fsops.test.ts', 7],
  ['host.test.ts', 8],
  ['outline.test.ts', 48],
  ['publication.test.ts', 43],
  ['recovery.test.ts', 17],
  ['real-core.test.ts', 14],
  ['saver.test.ts', 41],
  ['saving.test.ts', 34],
  ['sequences.test.ts', 15],
  ['sessions.test.ts', 52],
  ['shapes.test.ts', 53],
  ['transport.test.ts', 20],
  ['two-hosts.test.ts', 8],
  ['wire.test.ts', 9]
];

const root = path.join(__dirname, '..', '..', '..');

function dryRun(target: string): Array<{ file?: string }> {
  const out = execFileSync('npx', ['mocha', '--dry-run', '--reporter', 'json', target], {
    cwd: root,
    encoding: 'utf8',
    maxBuffer: 64 * 1024 * 1024,
    stdio: ['ignore', 'pipe', 'ignore']
  });
  return (JSON.parse(out) as { tests?: Array<{ file?: string }> }).tests ?? [];
}

function registeredPerFile(): Map<string, number> {
  const counts = new Map<string, number>();
  for (const test of dryRun('out/test/unit/**/*.test.js')) {
    if (test.file === undefined) {
      continue;
    }
    const name = path.basename(test.file).replace(/\.js$/, '.ts');
    counts.set(name, (counts.get(name) ?? 0) + 1);
  }
  return counts;
}

describe('no suite quietly gets shorter', function () {
  this.timeout(180000);
  let counted: Map<string, number>;

  before(() => {
    counted = registeredPerFile();
  });

  /*
   * THE COUNTER HAS TO HAVE SEEN SOMETHING. A dry run that returned
   * nothing would make every comparison below fail, which is loud and
   * therefore safe -- but this says so directly rather than leaving it
   * to be inferred from twenty identical failures.
   */
  it('registers cells in every suite it was asked about', () => {
    assert.ok(
      counted.size >= AT_LEAST.length,
      `only ${counted.size} suites registered any cells; the dry run saw ${[...counted.keys()].join(', ')}`
    );
  });

  for (const [name, expected] of AT_LEAST) {
    it(`${name} still registers at least ${expected} cells`, () => {
      const found = counted.get(name);
      assert.ok(
        found !== undefined,
        `${name} registers no cells at all; if that was meant, say here what covers its ground`
      );
      assert.ok(
        (found as number) >= expected,
        `${name} registers ${found} cells and registered ${expected}. If cells were removed on ` +
          'purpose, lower the number here and write which cell went and what covers what it covered.'
      );
    });
  }

  /*
   * AND A COMMENTED-OUT SUITE IS GONE, which is the whole reason this
   * stopped reading the source. The sample below holds four `it(` lines
   * and one cell.
   */
  it('does not count cells that are commented out', () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-census-'));
    const file = path.join(scratch, 'sample.test.js');
    fs.writeFileSync(
      file,
      [
        "describe('sample', () => {",
        "  it('runs', () => {});",
        '  /*',
        "  it('commented with a block', () => {});",
        "  it('also commented', () => {});",
        '  */',
        "  // it('commented with a line', () => {});",
        '});',
        ''
      ].join('\n'),
      'utf8'
    );
    assert.strictEqual(
      dryRun(file).length,
      1,
      'the counter sees cells mocha would not run, so commenting a suite out would pass it'
    );
  });

  /*
   * AND A CELL MADE BY A LOOP IS COUNTED. Removing a row from a table
   * removes a cell while the source keeps every `it(` line it had --
   * which the old counter could not see at all.
   */
  it('counts cells a loop produces', () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-census-'));
    const file = path.join(scratch, 'table.test.js');
    fs.writeFileSync(
      file,
      [
        "describe('table', () => {",
        '  for (const row of [1, 2, 3]) {',
        '    it(`row ${row}`, () => {});',
        '  }',
        '});',
        ''
      ].join('\n'),
      'utf8'
    );
    assert.strictEqual(dryRun(file).length, 3, 'a table of three rows was counted as one cell');
  });

  /*
   * AND THE EDITOR-HOSTED SUITES ARE COUNTED BY THEIR OWN HARNESS,
   * WHICH IS EXERCISED HERE.
   *
   * Those files `require('vscode')`, so the dry run above cannot load
   * them and they had no guard at all -- a whole editor suite could go
   * and the run would report no failures. The count is therefore taken
   * inside the host, by the harness, before it runs anything.
   *
   * ⚠️ WHAT IS CHECKED HERE IS THE COUNTING, NOT THAT THE HARNESS CALLS
   * IT. Removing the `shortfalls` call from test/integration/index.ts
   * leaves every cell below green; only a run inside an editor would
   * notice. These cells exist because the counting cannot otherwise be
   * exercised at all, not because they cover the wiring.
   */
  it('counts an editor-hosted suite by the cells it registers, not the ones it writes', () => {
    const tree = {
      tests: [],
      suites: [
        {
          tests: [{ file: '/x/out/test/integration/extension.test.js' }],
          suites: [
            {
              tests: [
                { file: '/x/out/test/integration/extension.test.js' },
                { file: '/x/out/test/integration/extension.test.js' }
              ],
              suites: []
            }
          ]
        }
      ]
    };
    assert.strictEqual(countRegistered(tree).get('extension.test.js'), 3, 'nested cells were missed');
  });

  it('says so when an editor-hosted suite got shorter', () => {
    const tree = { tests: [{ file: '/x/extension.test.js' }], suites: [] };
    const said = shortfalls(tree, ['extension.test.js'], [['extension.test.js', 14]]);
    assert.strictEqual(said.length, 1, JSON.stringify(said));
    assert.ok(said[0].includes('registers 1 cells and registered 14'), said[0]);
  });

  it('says so when an editor-hosted suite is loaded and counted by nothing', () => {
    const tree = { tests: [{ file: '/x/new.test.js' }], suites: [] };
    const said = shortfalls(tree, ['new.test.js'], [['extension.test.js', 0]]);
    assert.strictEqual(said.length, 1, JSON.stringify(said));
    assert.ok(said[0].includes('has no number'), said[0]);
  });

  /*
   * THE GREEN TWIN. Without it every cell above is satisfied by a
   * version that complains about everything, which would be removed
   * within a day -- and then nothing would be counted at all.
   */
  /*
   * ⚠️ A LISTED SUITE THAT REGISTERS NOTHING AT ALL. The negative cell
   * above exercises 1 against 14; nothing exercised 0 against 14, and a
   * guard written `found > 0 && found < expected` therefore passed every
   * cell here while silently accepting a file whose every cell was
   * deleted -- which is the one thing this exists to catch. Found in
   * review as a surviving mutation.
   */
  it('says so when a listed editor-hosted suite registers nothing at all', () => {
    const tree = { tests: [], suites: [] };
    const said = shortfalls(tree, [], [['extension.test.js', 14]]);
    assert.strictEqual(said.length, 1, JSON.stringify(said));
    assert.ok(said[0].includes('registers 0 cells'), said[0]);
  });

  /*
   * AND GROWTH IS NOT A SHORTFALL. A guard written with `!==` passes
   * every other cell here and turns adding a cell into a failure, which
   * is how a guard gets removed.
   */
  it('says nothing when a suite grew past its floor', () => {
    const tree = {
      tests: [{ file: '/x/extension.test.js' }, { file: '/x/extension.test.js' }],
      suites: []
    };
    assert.deepStrictEqual(shortfalls(tree, ['extension.test.js'], [['extension.test.js', 1]]), []);
  });

  /*
   * AND THE FILE NAME IS TAKEN THE SAME WAY ON EITHER SEPARATOR. Mocha
   * reports whatever path the platform gave it; a basename that only
   * knows `/` counts every Windows file under its full path and every
   * floor then reads as zero.
   */
  it('takes the file name from a Windows path too', () => {
    const tree = { tests: [{ file: 'C:\\x\\out\\extension.test.js' }], suites: [] };
    assert.strictEqual(countRegistered(tree).get('extension.test.js'), 1);
  });

  it('says nothing when every loaded suite is there and counted', () => {
    const tree = {
      tests: [{ file: '/x/extension.test.js' }, { file: '/x/extension.test.js' }],
      suites: [{ tests: [{ file: '/x/generation.test.js' }], suites: [] }]
    };
    assert.deepStrictEqual(
      shortfalls(tree, ['extension.test.js', 'generation.test.js'], [
        ['extension.test.js', 2],
        ['generation.test.js', 1]
      ]),
      []
    );
  });

  /*
   * AND THE NUMBERS THE HARNESS ACTUALLY USES NAME THE FILES IT
   * ACTUALLY LOADS. The cells above check the counting; this checks the
   * table, which is the part that goes stale.
   */
  /*
   * ⚠️ IN ONE DIRECTION ONLY: every source file has a number. It does
   * not reject a number for a file that no longer exists, and it
   * compares source names rather than the compiled files the harness
   * actually loads.
   */
  it('has a number for every editor-hosted suite that exists', () => {
    const listed = new Set(EDITOR_CELLS.map(([name]) => name.replace(/\.js$/, '.ts')));
    const missing = fs
      .readdirSync(path.join(root, 'test', 'integration'))
      .filter((name) => name.endsWith('.test.ts') && !listed.has(name));
    assert.deepStrictEqual(missing, [], 'these editor-hosted suites are not counted');
  });

  it('has a number for every unit suite that exists', () => {
    const listed = new Set(AT_LEAST.map(([name]) => name));
    const missing = fs
      .readdirSync(path.join(root, 'test', 'unit'))
      .filter((name) => name.endsWith('.test.ts') && !listed.has(name));
    assert.deepStrictEqual(missing, [], 'these suites are not counted, so losing their cells would be silent');
  });
});
