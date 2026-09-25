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
 * NOTE: THIS GUARD HAD THE DISEASE IT WAS BUILT TO CATCH. Its first version
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
  ['brief-acceptance.test.ts', 9],
  ['brief-current-crash.test.ts', 1],
  ['brief-current.test.ts', 6],
  ['brief-lease.test.ts', 8],
  ['brief-migration.test.ts', 7],
  ['brief-outline.test.ts', 8],
  ['brief-reconcile.test.ts', 6],
  ['brief-shared-state.test.ts', 4],
  ['brief-temporary.test.ts', 7],
  ['brief-working.test.ts', 2],
  ['brief-write-boundaries.test.ts', 12],
  ['activation.test.ts', 4],
  ['answering.test.ts', 27],
  /*
   * ADDED in round 35: the AST census over every wait in the extension
   * host, after a review traced a takeover into the queue the window had
   * just stopped using. RAISED in plugin-r3 to the ten it registers: the
   * cell that turns a generation guard around, and the two that read how
   * the integrity watch is wired. And in item 7, 10 -> 11: the one that
   * reads that the retry command calls `retryParked`. And in item 16,
   * 11 -> 12: a guard that compares the copy plus one is not a guard; and
   * 12 -> 13 in its r2: the shapes written around a guard.
   */
  ['awaiting.test.ts', 13],
  ['blocks.test.ts', 20],
  /*
   * NOTE: INCLUDING ITSELF. This file was exempt from the inventory check
   * and had no floor, so deleting its own cells was the one shortening
   * nothing here would have said a word about.
   */
  ['census.test.ts', 17],
  ['chain.test.ts', 6],
  ['commands.test.ts', 8],
  /*
   * RAISED in plugin-r3 item 2 to the fourteen it registers: the five that
   * read the core's `local-writer`; 14 -> 15 in its review r1, the named
   * writer first and in the middle of the listing.
   */
  ['cursor.test.ts', 15],
  /*
   * ADDED in plugin-r2: the decoder's own cells, and the census over the
   * two readers that have no head to name. The other half of that
   * census is the compiler: `clause`, `clauseRest`, `clauseValue` and
   * `headName` are no longer exported from wire.ts.
   */
  /*
   * ADDED in plugin-r3 item 19: src/ quotes the core by the names its
   * files have.
   */
  ['core-quotes.test.ts', 1],
  /*
   * ADDED in plugin-r3 item 15: a damaged store's `check` is an answer, and
   * another writer the core could not read does not stop the local cursor;
   * three more in its r2 (the local writer listed twice, two forms).
   */
  ['damaged-check.test.ts', 12],
  ['decoding.test.ts', 12],
  ['dependency-sexpr.test.ts', 15],
  /*
   * ADDED in plugin-r3 item 6: a subtree composed as one read-only
   * document -- fifteen cells on the composition, five on the text behind
   * a document's address, four against a real store.
   */
  ['document-view.test.ts', 24],
  ['documents.test.ts', 8],
  ['durability.test.ts', 33],
  ['fsops.test.ts', 11],
  ['host.test.ts', 8],
  ['mutators.test.ts', 12],
  ['outline.test.ts', 48],
  ['ownership.test.ts', 18],
  ['publication.test.ts', 67],
  /*
   * ADDED in plugin-r3 item 5: the slug of a title, and how a block's
   * projection is named once and found again by its sidecar.
   */
  ['projection-name.test.ts', 12],
  /*
   * LOWERED in plugin-r3 item 1, 34 -> 33: the cell "imports when
   * theourgia.transport changed under it and the store did not" went with
   * the setting. Its cells are one per setting in package.json, so the
   * row goes when the setting goes; there is nothing left for it to cover.
   */
  ['recovery.test.ts', 33],
  /*
   * ADDED in plugin-r3 item 3: the four source censuses of the enqueue
   * receipt -- the brand, where it is issued, and the two call-site
   * censuses, one per interface change.
   */
  ['receipts.test.ts', 4],
  ['real-core.test.ts', 19],
  /*
   * ADDED in round 39: the refusal kinds the core makes, read from the
   * core, against the table that sorts them.
   */
  ['refusals.test.ts', 3],
  /*
   * ADDED in plugin-r2: the gate that keeps comments in ASCII, and the
   * cell that proves the gate can tell a comment from a string.
   */
  /*
   * ADDED in plugin-r2: the census that stops an inability being
   * answered as an absence -- the supplier for a shape thirteen review
   * rounds found in twelve places.
   */
  /*
   * RAISED in plugin-r3 item 35, 4 -> 7: an anchor of its own for every
   * catch; an exemption kept through moved lines and changed whitespace;
   * and one lost to a rewrite of the statement it guards.
   */
  ['absence.test.ts', 7],
  ['ascii-comments.test.ts', 3],
  /*
   * ADDED in plugin-r2: the gate that watches the user's own
   * `~/.theourgia/run` across the whole suite, and the two cells that
   * keep the gate honest.
   */
  ['run-root.test.ts', 9],
  /*
   * ADDED in plugin-r3: what `check` says about a store's condition, read
   * whatever its exit code, and the one sentence said about it; then the
   * eight that drive `IntegrityWatch`, when to ask and when to tell. RAISED
   * in item 17, 13 -> 15: marks kept past two other stores, and an `ask`
   * that throws at once. And in item 18, 15 -> 17: an output channel that
   * throws too, and nothing written for a warning that was shown; in its r2,
   * 17 -> 19: an Error named by what it says, and a value that cannot be
   * named still written down.
   */
  ['integrity.test.ts', 19],
  /*
   * ADDED in plugin-r2: reading a search answer and a verb catalogue,
   * what a search does, and the envelope --wire puts round a commit.
   */
  ['search.test.ts', 70],
  /*
   * RAISED in plugin-r3 item 1, 54 -> 55: a bare `unreadable` answer to a
   * save is kept pending under its own id, like `transport-unknown`. And
   * in item 7, 55 -> 61: a refusal's detail and remedy reach the
   * sentence, an instance mismatch is kept and parked, and the retry
   * command's way back, both halves; 61 -> 63 in its review r1: a remedy
   * that cannot be said is printed, and `changed` keeps its clauses. And
   * in item 2, 63 -> 65: a store with two writers saved against the one
   * the core names, and a contradicting answer refused with nothing sent;
   * 65 -> 67 in its review r1, one true sentence for each thing that could
   * not be read.
   */
  ['saver.test.ts', 67],
  ['saving.test.ts', 34],
  /*
   * RAISED in plugin-r3 item 3 to the thirty-six it registers: the five
   * runtime cells of the enqueue receipt (design cells 3, 4, 5a, 5b, 6);
   * 36 -> 37 in its review r1, 5c, through the shipping file operations.
   */
  ['sending.test.ts', 37],
  ['sequences.test.ts', 15],
  /*
   * ADDED in round 36: the settler moved out of `extension.ts` into
   * `src/settling.ts` so that "which queue, which store" could be
   * driven by a cell at all.
   */
  ['settling.test.ts', 23],
  /*
   * RAISED in plugin-r3 item 3 to the hundred and five it registers, with
   * the addressed-receipt cell on the import path.
   */
  ['sessions.test.ts', 105],
  ['shapes.test.ts', 68],
  ['tombstones.test.ts', 11],
  /*
   * LOWERED in plugin-r2, from 27, deliberately: the cell for the socket
   * transport went with the class. It asserted that an unimplemented
   * adapter refused rather than falling back, which was worth asserting
   * while a setting could select it; when `transport` stopped offering
   * `socket` the class became unreachable and the cell measured nothing.
   * A number that goes down is meant to be argued for, which is what
   * this comment is.
   */
  ['transport.test.ts', 26],
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
   * NOTE: WHAT IS CHECKED HERE IS THE COUNTING, NOT THAT THE HARNESS CALLS
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
   * NOTE: A LISTED SUITE THAT REGISTERS NOTHING AT ALL. The negative cell
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
   * NOTE: IN ONE DIRECTION ONLY: every source file has a number. It does
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
