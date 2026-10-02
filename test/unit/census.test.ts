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
  ['brief-outline.test.ts', 9],
  ['brief-reconcile.test.ts', 17],
  ['brief-shared-state.test.ts', 6],
  ['brief-temporary.test.ts', 7],
  ['brief-working.test.ts', 2],
  ['bytes.test.ts', 14],
  ['brief-write-boundaries.test.ts', 13],
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
   * 12 -> 13 in its r2: the shapes written around a guard. And in item 24,
   * 13 -> 15: that the open reads and publishes on the save chain. And in
   * item 22, 15 -> 16: that every command is registered through the wrapper
   * that shows the durability sink.
   */
  ['awaiting.test.ts', 16],
  ['blocks.test.ts', 20],
  /*
   * NOTE: INCLUDING ITSELF. This file was exempt from the inventory check
   * and had no floor, so deleting its own cells was the one shortening
   * nothing here would have said a word about.
   */
  ['census.test.ts', 17],
  ['chain.test.ts', 6],
  /*
   * RAISED 8 -> 9 with the removal of theourgia.writer: the manifest declares
   * no draft-space setting.
   */
  ['commands.test.ts', 9],
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
  ['core-quotes.test.ts', 2],
  /*
   * ADDED in plugin-r3 item 15: a damaged store's `check` is an answer, and
   * another writer the core could not read does not stop the local cursor;
   * three more in its r2 (the local writer listed twice, two forms).
   */
  ['damaged-check.test.ts', 12],
  /*
   * ADDED with the read-only datum view: thirteen cells on the view, its
   * address and a restored tab, two on the open itself, one against a
   * stand-in core and one against a real store, and five on the mode a
   * write goes by (a file this version made, a file with no mode on a text
   * block and on a datum block, a record with no projection, and a
   * reconciliation), the census of the places that can record text, and on a
   * real core the behind notice of a save that reads the block first and of
   * one that does not; and six on a read that does not show text (a failed
   * open, reconciliation and gate; a queued write and its control; two
   * opens racing and its control).
   */
  ['datum-view.test.ts', 29],
  ['decoding.test.ts', 12],
  ['definition.test.ts', 14],
  ['dependency-sexpr.test.ts', 15],
  /*
   * ADDED with the files view: the tree export would write, from the
   * blocks' paths -- the grouping and the exporter's rule (6), the
   * enumeration (1), the default mode (1), the action paths (2), the batch
   * through the Saver (5), its queue entry (5), the menus (1), the window in
   * the harness (6), and on a real core the agreement with export and a new
   * document made through the window (5). RAISED 32 -> 45 by its review: kind
   * and mode as symbols (1), a family written whole or not at all (6), an
   * orphan document (1), the receipt, the insert-only batch and the queue's
   * version (3), a stale row and a mode chosen during a listing (2). RAISED
   * 45 -> 50 by its second review: a cycle's blocks and a deleted block in a
   * family, a receipt whose event is not one, a mode spelled as a string and
   * an orphan read by the model (5). RAISED 50 -> 62 when a row became an id
   * and a write of the path became conditional: the version read and a
   * version missing (1), a queued write refused as stale and one kept through
   * a restart (2), a version that is not a string (1), and in the harness a
   * rename from a row kept from before a move, a move from an old directory,
   * a write that raced the prompt, a settings change that kept the store,
   * rows of another store, a store switched during the read and during the
   * prompt, and Files chosen during an Outline listing, net of the two rows
   * they replaced (6 + 2 + 1), and on a real core the conditional rename (1).
   * RAISED 62 -> 66 by its review: a write of the path never queued without
   * its version (1), a stale change drained by the retry command (1), a mode
   * chosen while the listing remembered its own (1), and Outline nodes of
   * another store (1). RAISED 66 -> 72 by its next review: a stale refusal
   * of a kept write handed on from any drain, not the asking write's own
   * (1), and when the drain then throws (1); a queued record writing the
   * path and a queued batch of other intents refused (1); submit refusing a
   * record writing the path (1); in the window, a kept move drained in
   * front of a new document (1) and across a settings change during the
   * retry (1).
   */
  ['directory-view.test.ts', 72],
  /*
   * ADDED in plugin-r3 item 6: a subtree composed as one read-only
   * document -- fifteen cells on the composition, five on the text behind
   * a document's address, four against a real store.
   */
  ['document-view.test.ts', 26],
  ['documents.test.ts', 8],
  /*
   * RAISED in queue item 22, 33 -> 65: D1 for each of eight mutators under a
   * flush that returns and one that throws (16), D2 for each (8), and D4c,
   * the sink keeping one sentence per queue file (2), and D4/D4b, the
   * window showing them (2); 61 -> 63 in its delivery review r2: D4b through
   * the startup retry, and D4d, a text whose show throws not taking the others;
   * 63 -> 64 in review r3, D4e, a show that rejects later; 64 -> 65 folded from
   * the closing review, D4f, the awaited save's own warning shown. 65 -> 67 in
   * queue item 46: E1 and its control; 67 -> 68 folded from its review, E7.
   */
  ['durability.test.ts', 68],
  ['fsops.test.ts', 11],
  ['host.test.ts', 8],
  /*
   * ADDED with the hover: twenty-four cells on what it shows, its requests,
   * cache, deadline, name reader and the store it may ask, against a scripted
   * client; four in the
   * window (its three kinds of document, marker lines, the name in a restored
   * tab, the cache across a mode invalidation and a save); two on a real store.
   */
  ['hover.test.ts', 30],
  /*
   * ADDED with the supply commands: the reader of the projection's marker
   * lines, one cell per layout the core writes.
   */
  ['markers.test.ts', 15],
  ['mutators.test.ts', 12],
  /*
   * ADDED in queue item 39: an information notice and an error notice reach
   * the editor at their levels.
   */
  ['notices.test.ts', 2],
  ['outline.test.ts', 51],
  ['ownership.test.ts', 18],
  ['publication.test.ts', 101],
  /*
   * ADDED in plugin-r3 item 5: the slug of a title, and how a block's
   * projection is named once and found again by its sidecar.
   */
  ['projection-name.test.ts', 18],
  /*
   * ADDED with the core's read receipts (59e69f3): each reader of an answer
   * that now ends with a receipt reads it as before.
   */
  ['read-receipt.test.ts', 5],
  /*
   * LOWERED in plugin-r3 item 1, 34 -> 33: the cell "imports when
   * theourgia.transport changed under it and the store did not" went with
   * the setting. Its cells are one per setting in package.json, so the
   * row goes when the setting goes; there is nothing left for it to cover.
   */
  /*
   * 33 -> 34 in queue item 22: D6, a takeover's durability warning in its
   * report and not in the sink.
   */
  /*
   * LOWERED 34 -> 33 with the removal of theourgia.writer: the cell
   * "imports when theourgia.writer changed under it and the store did not"
   * went with the setting, as the transport one did above; there is
   * nothing left for it to cover.
   */
  ['recovery.test.ts', 33],
  /*
   * ADDED in plugin-r3 item 3: the four source censuses of the enqueue
   * receipt -- the brand, where it is issued, and the two call-site
   * censuses, one per interface change.
   */
  ['receipts.test.ts', 4],
  /*
   * ADDED in plugin-r3 item 43: the source census that every write of a
   * record goes through writeSidecar, and its probe.
   */
  ['record-door.test.ts', 6],
  ['real-core.test.ts', 20],
  /*
   * ADDED in round 39: the refusal kinds the core makes, read from the
   * core, against the table that sorts them. RAISED 8 -> 14 when the
   * census learned the consed and the log-condition constructors: the one
   * unreadable kind pinned (1), the new routes read (1), and the kinds they
   * found each landing in its family through a real Saver (4). RAISED 14 ->
   * 16 by its review: no kind excused as a returned record is raised (1),
   * `adopt` is sent nowhere (1), and a constructor matched only at a list's
   * head (1).
   */
  /*
   * RAISED at the re-pin to f34d84f, 17 -> 18: `template` and `init` are sent
   * nowhere (the project template's refusals answer only them). RAISED by the
   * citation audit, 18 -> 19: every row of NOT_A_WRITES_ANSWER that cites the
   * core cites a line holding its kind.
   */
  ['refusals.test.ts', 19],
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
   * named still written down. And in item 33, 19 -> 55: what `show` and
   * `record` return (C1-C7, C9-C36; C6 is two cells).
   */
  /*
   * RAISED with the cut note, 24 -> 28: a cut read and said, an unreadable
   * writer beside a cut, notes merged by whether and where they are cut, and
   * an unknown note kind still refused.
   */
  ['incomplete.test.ts', 29],
  ['integrity.test.ts', 55],
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
  /*
   * RAISED 72 -> 94 when the cell that looped over every retryable and
   * settings refusal name became one cell per name (22 names, plus a cell
   * that the two families are not empty).
   */
  /*
   * RAISED 94 -> 96 with the re-pin to theourgia 3aad6fd: a completion
   * that stopped `unknown` is kept, and one refused `stale-baseline` is a
   * refusal, both read by name with their `completion` clause.
   */
  ['saver.test.ts', 96],
  ['saving.test.ts', 34],
  /*
   * RAISED in plugin-r3 item 3 to the thirty-six it registers: the five
   * runtime cells of the enqueue receipt (design cells 3, 4, 5a, 5b, 6);
   * 36 -> 37 in its review r1, 5c, through the shipping file operations.
   * 37 -> 42 in item 22: D3 (two: the mark alone, and the enqueue and the
   * mark on one file keeping the latest), D3b, D3c and D5, where a drain's
   * durability warnings go; 42 -> 44 in its delivery review r1: D3c through a
   * rejecting send (the settle version kept) and D3d; 44 -> 45 in review r2,
   * D3e; 45 -> 46 in review r3, D3e through save(); 46 -> 47 folded from the
   * closing review, D3f. 47 -> 48 in queue item 46: E2; 48 -> 53 folded from
   * its review, E3, E4, E5 and E6 through submit and save.
   */
  /*
   * RAISED 53 -> 54 with the re-pin to theourgia 3aad6fd: a commit whose
   * completion stopped `unknown`, answered as a batch, is kept.
   */
  ['sending.test.ts', 54],
  /*
   * UNCHANGED at fifteen by queue item 25, one in and one out: the cell that
   * lands a save while a publication is being prepared, and C12 retired (a
   * chain key reaches no argument of `publish`).
   */
  ['sequences.test.ts', 15],
  /*
   * ADDED in round 36: the settler moved out of `extension.ts` into
   * `src/settling.ts` so that "which queue, which store" could be
   * driven by a cell at all. 23 -> 24 in queue item 22: D5b, a settle's
   * durability warning in the sink; 24 -> 27 folded from its closing review:
   * D5b for the refusal, the recorded answer and an operator's determination.
   */
  ['settling.test.ts', 27],
  /*
   * RAISED in plugin-r3 item 3 to the hundred and five it registers, with
   * the addressed-receipt cell on the import path; 105 -> 107 in item 25,
   * the two cells that read a non-string mark on a complete entry; 107 -> 108
   * in item 22's delivery review r1, D6b; 108 -> 109 in its review r2, D6c.
   */
  ['sessions.test.ts', 109],
  ['shapes.test.ts', 68],
  /*
   * ADDED with the split by the editor's symbols: the conversion to byte
   * offsets and the symbols file (6), the command's steps (6), the empty
   * list (1), the refusals and their census (5), and a real core (3: the
   * third a first-line symbol in a file with a byte-order mark).
   */
  ['split-symbols.test.ts', 21],
  /*
   * ADDED with the supply commands: collecting (13), the supply file (4), the
   * answers (5), the command's steps (40), and on a real core (3).
   */
  /*
   * RAISED with the supply test's findings, 65 -> 70: a hover's declaration
   * in its fence, how a hover is read, the ranking of a block's symbols, a
   * block described by its first function, no call from a block to itself.
   */
  /*
   * RAISED 70 -> 71: Supply Diagnostics reads the working view this window
   * writes its drafts into.
   */
  /*
   * RAISED 71 -> 72: a draft space named in the environment decides nothing.
   */
  ['supply.test.ts', 72],
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
  /*
   * RAISED at the re-pin that brought the core's platform numbers, 26 -> 27:
   * a platform the core has not measured is said as the core failing to
   * start, with the core's remedy.
   */
  /*
   * RAISED with the supply test's findings, 27 -> 28: the real core found
   * through the directory holding corePath.
   */
  ['transport.test.ts', 28],
  ['tripwire.test.ts', 7],
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
