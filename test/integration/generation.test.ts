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
 * What happens to a request that is in flight when the settings change.
 *
 * THE STAND-IN CORE IS USED HERE AND NOT THE REAL ONE, because the thing
 * being arranged is a moment: a read that is still running while the
 * store setting is replaced. A real core answers when it answers; a
 * stand-in can be told to take a second, which is what makes the window
 * wide enough to step into.
 *
 * THE ENVIRONMENT IS SET ON THIS PROCESS because the extension spawns
 * the core with `process.env` as its base, and it reads that when the
 * client is built -- which is on every settings change, so a variable
 * set before the change reaches the child.
 */

import * as assert from 'assert';
import { createHash } from 'crypto';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as vscode from 'vscode';
import { wroteAnswer } from '../support/answers';
import { FakeCore } from '../support/fake';
import { StatusFacts } from '../../src/status';
import { settle, until } from '../support/until';

const BLOCK =
  '(ok ((id . "a.2") (deleted . #f) (fields (heading-src . "## Two\\n") (src . "body\\n") (title . "Two")) (position root . 0) (edges)))';

/*
 * NEVER: THESE CELLS USED TO ESTABLISH A RACE WITH A STOPWATCH.
 *
 * Each one starts a request the stand-in holds open for 2500 ms, waits
 * `settle(400)` in the hope that it has begun, and then changes the
 * store under it. 400 ms is not an observation of anything: on a loaded
 * machine the request has not started and the cell measures a change
 * made before the race it is about, and nothing says so -- the
 * assertions below would pass for the wrong reason. The other
 * editor-hosted file was repaired for exactly this in rounds three to
 * five and this one was named in the delivery note for three rounds
 * after that.
 *
 * What is waited for now is the stand-in's own record that it has TAKEN
 * the request, which it writes before its delay. `until` and `settle`
 * come from `test/support/until.ts`, the same ones the other file uses.
 */
/*
 * WHERE A HELD REQUEST WAITS, AND WHO LETS IT GO.
 *
 * The stand-in holds a request until its hold file appears. The path has
 * to be known before the stand-in is built, so it lives here rather than
 * on the FakeCore.
 *
 * NEVER: AND EVERY ONE IS REMOVED BEFORE EACH CELL. A marker whose name
 * can be reused is a wait that returns instantly -- the release left by
 * the previous cell would let the next cell's request straight through,
 * and the race it meant to establish would never exist. The removal is
 * in `beforeEach` with the rest of the setup, not beside the release.
 */
const HOLD_ROOT = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-hold-'));
const HOLD = {
  read: path.join(HOLD_ROOT, 'read'),
  conflicts: path.join(HOLD_ROOT, 'conflicts'),
  commit: path.join(HOLD_ROOT, 'commit')
};

function release(name: keyof typeof HOLD): void {
  fs.writeFileSync(HOLD[name], '', 'utf8');
}

function holdAgain(): void {
  for (const file of Object.values(HOLD)) {
    fs.rmSync(file, { force: true });
  }
}

/*
 * NEVER: AND `argv.includes(verb)` IS NOT THE REQUEST THIS CELL STARTED.
 *
 * Measured in an eighteenth review round: a wait for `commit` was
 * satisfied by `['set', 'a.2', 'src', 'commit']`, a wait for `read` by a
 * read of another id in another store, and both by a request that had
 * finished long before -- because two of the three call sites passed a
 * literal zero for "how many there were before" instead of reading it.
 *
 * What is waited for now is a request that is IN FLIGHT -- taken and not
 * answered -- whose first argument is the verb. The stand-in holds it
 * until this cell releases it, so "in flight" stops being a claim about
 * the clock.
 *
 * KEY: AND THE PRECISE MATCH IS HARDENING, WHICH IS SAID RATHER THAN
 * CLAIMED. With the hold in place, reverting `argv[0] === verb` to
 * `argv.includes(verb)` changes nothing that any cell can see: the log
 * is this cell's own, the held request is the only one outstanding, and
 * no request in this tree carries another verb's name in its arguments.
 * The reviewer's example -- a wait for `commit` satisfied by
 * `['set', 'a.2', 'src', 'commit']` -- is a shape the product does not
 * currently produce. It is kept because it costs nothing and because
 * what the wait MEANS is "this request", not "a request with that word
 * in it"; it is not kept on the strength of a mutation, and the
 * mutation run that failed to kill it is in the delivery note.
 */
function flying(core: FakeCore, verb: string): string[][] {
  return core.inFlight().filter((argv) => argv[0] === verb);
}

async function requestInFlight(core: FakeCore, verb: string): Promise<void> {
  await until(
    () =>
      `a ${verb} request was taken and not answered (in flight: ${JSON.stringify(
        core.inFlight().map((argv) => argv[0])
      )})`,
    () => flying(core, verb).length > 0
  );
}

async function requestAnswered(core: FakeCore, verb: string, from: number): Promise<void> {
  const answered = (): number =>
    core.calls().filter((c) => c.event === 'answer' && c.coreArgv[0] === verb).length;
  await until(
    () => `the stand-in answered a ${verb} (it has answered ${answered()}, and ${from} before this)`,
    () => answered() > from
  );
}

async function useStore(core: FakeCore, store: string): Promise<void> {
  const settings = vscode.workspace.getConfiguration('theourgia');
  process.env.FAKE_CORE_SCRIPT = core.scriptFile;
  process.env.FAKE_CORE_LOG = core.logFile;
  await settings.update('scheme', core.config().scheme, vscode.ConfigurationTarget.Global);
  await settings.update('corePath', core.corePath, vscode.ConfigurationTarget.Global);
  await settings.update('libDirs', [], vscode.ConfigurationTarget.Global);
  await settings.update('store', store, vscode.ConfigurationTarget.Global);
  await settings.update('actor', 'generation-cell', vscode.ConfigurationTarget.Global);
}

/*
 * THE DIRECTORY A STORE'S QUEUE LIVES IN.
 *
 * NOTE: THIS IS A SECOND COPY OF THE PRODUCT'S RULE, and it is here rather
 * than imported because the rule lives inside `activate`, which a cell
 * cannot reach. What makes the duplicate acceptable is the direction it
 * fails in: if the extension ever names the directory differently, this
 * finds NO queue for the store and the cell says so by name -- it cannot
 * quietly read the wrong directory, because there is no other directory
 * with this name to read.
 */
function namespaceOf(store: string): string {
  return createHash('sha256').update(store, 'utf8').digest('hex').slice(0, 16);
}

interface Queue {
  store: string;
  cursor: string | null;
  entries: unknown[];
}

/*
 * EVERY QUEUE THIS WINDOW HAS WRITTEN, READ OFF THE DISK.
 *
 * The path comes from the launcher through the environment rather than
 * being rebuilt here: a cell that worked out for itself where the
 * extension writes would be a second opinion about it, and the reading
 * that is wrong is the reassuring one -- an empty directory looks
 * exactly like a queue with nothing in it.
 */
function queueFiles(): Queue[] {
  const storage = process.env.THEOURGIA_TEST_STORAGE;
  assert.ok(
    storage !== undefined && storage.length > 0,
    'THEOURGIA_TEST_STORAGE is not set, so this cell cannot read the queues it is about'
  );
  const sessions = path.join(storage as string, 'sessions');
  if (!fs.existsSync(sessions)) {
    return [];
  }
  const out: Queue[] = [];
  for (const session of fs.readdirSync(sessions)) {
    const directory = path.join(sessions, session);
    if (!fs.statSync(directory).isDirectory()) {
      continue;
    }
    for (const store of fs.readdirSync(directory)) {
      const file = path.join(directory, store, 'outbox.json');
      if (!fs.existsSync(file)) {
        continue;
      }
      const held = JSON.parse(fs.readFileSync(file, 'utf8')) as {
        cursor: string | null;
        entries: unknown[];
      };
      out.push({ store, cursor: held.cursor ?? null, entries: held.entries ?? [] });
    }
  }
  return out;
}

async function currentFacts(): Promise<StatusFacts> {
  return (await vscode.commands.executeCommand('theourgia.showStatus', { ask: false })) as StatusFacts;
}

describe('a setting that changes while a request is in flight', function () {
  this.timeout(180000);
  let core: FakeCore;

  before(async () => {
    const extension = vscode.extensions.getExtension('theourgia.theourgia');
    assert.ok(extension !== undefined);
    await extension?.activate();
  });

  beforeEach(() => {
    /*
     * EVERY HOLD IS PUT BACK BEFORE THE CELL THAT USES IT. See the note
     * at HOLD: a release left by the previous cell would let this cell's
     * request straight through.
     */
    holdAgain();
  });

  afterEach(async () => {
    core?.dispose();
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    delete process.env.FAKE_CORE_SCRIPT;
    delete process.env.FAKE_CORE_LOG;
    await settle();
  });

  it('C1 does not open a block into the store that replaced the one it was read from', async () => {
    core = new FakeCore([
      { match: ['read', 'a.2'], stdout: `${BLOCK}\n`, rc: 0, holdFile: HOLD.read },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['check'], stdout: '(check (writers (("w" (end 1) (torn #f) (integrity ())))) (verdict ok))\n', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    const before = vscode.workspace.textDocuments.map((d) => d.uri.toString()).sort();
    const opening = vscode.commands.executeCommand('theourgia.openBlock', 'a.2');
    await requestInFlight(core, 'read');

    /*
     * THE READ IS HELD -- taken and not answered, and it stays that way
     * until this cell releases it. The store setting is replaced under
     * it, which is the state this cell is about.
     */
    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    release('read');
    await opening;
    await requestAnswered(core, 'read', 0);

    const after = vscode.workspace.textDocuments.map((d) => d.uri.toString()).sort();
    assert.deepStrictEqual(
      after,
      before,
      'a block read from one store was opened after the settings named another'
    );
    const facts = await currentFacts();
    assert.strictEqual(facts.store, `${core.store}-B`, 'the settings did not actually change');
  });

  it('C5 does not carry one store\'s conflict count over to another', async () => {
    core = new FakeCore([
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n(orphan "a.9")\n', rc: 0 },
      { match: ['read'], stdout: `${BLOCK}\n`, rc: 0 },
      { match: ['outline'], stdout: '', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    await vscode.commands.executeCommand('theourgia.refreshOutline');
    let seen: unknown = null;
    await until(
      () => `the status reported store A's conflicts (it says ${JSON.stringify(seen)})`,
      async () => {
        seen = (await currentFacts()).conflicts;
        return seen !== null;
      }
    );
    const asked = await currentFacts();
    assert.strictEqual(asked.conflicts, 2, `the count for store A was ${asked.conflicts}`);
    let named: unknown = null;

    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    /*
     * WAIT FOR THE STATUS TO SAY IT IS ABOUT THE NEW STORE, which is the
     * event this cell is about; the count beside it is then read from a
     * status that has caught up, rather than from one that may not have.
     */
    await until(
      () => `the status named store B (it names ${JSON.stringify(named)})`,
      async () => {
        named = (await currentFacts()).store;
        return named === `${core.store}-B`;
      }
    );

    const carried = await currentFacts();
    assert.strictEqual(carried.store, `${core.store}-B`);
    assert.strictEqual(
      carried.conflicts,
      null,
      'a store nobody has asked was shown with the previous store\'s conflict count'
    );
  });

  /*
   * C6 THE COUNT THAT WAS STILL IN FLIGHT. C5 above changes the settings
   * when nothing is outstanding, so it proves that `rebuild` forgets the
   * old count -- and nothing more. The check that matters is the one
   * AFTER the wait: an answer that arrives for the store the user has
   * just left must be dropped, not painted. Without it the sequence is
   * `rebuild` clears the count, then the late answer writes store A's
   * number under store B's name, which is worse than never clearing it
   * because it looks freshly fetched.
   *
   * THE WAIT IS A CORE REQUEST, so the stand-in can hold it open for as
   * long as the cell needs. That is what makes this one guardable at all
   * -- the waits inside `openBlock` are editor calls and are not.
   */
  it('C6 does not paint a conflict count that arrived for the store the user left', async () => {
    core = new FakeCore([
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n(orphan "a.9")\n', rc: 0, holdFile: HOLD.conflicts },
      { match: ['read'], stdout: `${BLOCK}\n`, rc: 0 },
      { match: ['outline'], stdout: '', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    const running = vscode.commands.executeCommand('theourgia.refreshOutline') as Promise<unknown>;
    await requestInFlight(core, 'conflicts');
    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    release('conflicts');
    await running;
    await requestAnswered(core, 'conflicts', 0);

    /*
     * THE REQUEST HAS TO HAVE SUCCEEDED. `refreshConflicts` answers null
     * when the count could not be fetched at all, so a cell that only
     * checks for null passes whether the guard works or the request
     * simply failed -- and a stand-in whose delayed answer never arrived
     * would look exactly like a guard doing its job. The call is
     * required to have been made and to have been answered.
     */
    const conflictCalls = core.calls().filter((c) => c.coreArgv.includes('conflicts'));
    assert.ok(conflictCalls.length > 0, 'the conflicts request was never made');
    assert.ok(
      conflictCalls.some((c) => c.event === 'answer'),
      `the delayed request did not answer, so null proves nothing: ${JSON.stringify(
        conflictCalls.map((c) => c.event)
      )}`
    );

    const facts = await currentFacts();
    assert.strictEqual(facts.store, `${core.store}-B`, 'the settings did not actually change');
    assert.strictEqual(
      facts.conflicts,
      null,
      'a count fetched for the previous store was painted under the new one'
    );
  });

  /*
   * C7 A SAVE THAT ANSWERS AFTER THE USER HAS MOVED ON -- A READING,
   * TAKEN ON PURPOSE, NOT A GUARD.
   *
   * NOTE: WHY THIS CELL EXISTS AND WHAT IT IS NOT. A census over every wait
   * in `extension.ts` (test/unit/awaiting.test.ts) found one that does
   * not check the generation: `onSaved` issues the save to the Saver it
   * read before the wait, and reports the outcome after it. The main
   * session ruled that this belongs to the NEXT batch -- it is the save
   * surface, this batch did not make it worse, and the batch's scope was
   * closed at kickoff -- and that this round takes a MEASUREMENT rather
   * than a repair. So this cell asserts what the extension does today,
   * and will go red when that changes, which is exactly what the next
   * batch needs from it.
   *
   * NOTE: AND IT ANSWERS WHY C6 ABOVE IS GREEN, which is a different
   * question from whether this path is safe. C6's stimulus is
   * `refreshOutline`, whose wait is inside `refreshConflicts` -- and
   * `refreshConflicts` takes the generation and drops a late answer.
   * `paint` itself checks nothing: it reads the CURRENT facts every
   * time. So C6 passes because of the guard in the fetcher, not because
   * of anything in the painter, and nothing it does touches `onSaved`.
   * Measured here rather than reasoned about.
   *
   * NOTE: WHAT THIS CELL CANNOT SEE. `show` calls
   * `vscode.window.showInformationMessage`, and the suite has no way to
   * read what was shown. The sentence about store A's save is therefore
   * beyond this reading, and that -- not the status bar -- is where the
   * exposure is. Said here so that a green run is not mistaken for "the
   * whole question was put".
   */
  it('C7 paints the new store’s facts when a save for the old one answers late', async () => {
    core = new FakeCore([
      { match: ['check'], stdout: '(check (writers (("w" (end 1) (torn #f) (integrity ())))) (verdict ok))\n', rc: 0 },
      { match: ['read'], stdout: `${BLOCK}\n`, rc: 0 },
      /*
       * NOTE: THE SHAPE A REAL CORE ANSWERS WITH, AND NOT ONE WRITTEN HERE.
       * This cell used to spell its own `(ok ((cursor . "w:2")))`, which
       * `eventFromWrite` reads as an ok that names no record: the entry
       * is marked pending and THE SETTLER IS NEVER CALLED. So the first
       * version measured a save that never settled while its comment
       * said what a settled one does, and the delivery note repeated it.
       * The shapes now live in `test/support/answers.ts` and are put to
       * the product's own reader by a cell in `fsops.test.ts`.
       */
      { match: ['commit'], stdout: `(ok (items ${wroteAnswer(2).trim()}))\n`, rc: 0, holdFile: HOLD.commit },
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n', rc: 0 },
      { match: ['outline'], stdout: '', rc: 0 }
    ],{prefix:'## Two\n',body:'body\n'});
    await useStore(core, `${core.store}-A`);
    await settle();

    await vscode.commands.executeCommand('theourgia.openBlock', 'a.2');
    /*
     * THE BLOCK IS OPEN WHEN AN EDITOR SAYS SO. `openBlock` resolves
     * before the editor is active, and 500 ms was a guess about how long
     * after.
     */
    await until('a block opened in an editor', () => vscode.window.activeTextEditor !== undefined);
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined, 'the block did not open, so nothing below is about a save');
    const document = (editor as vscode.TextEditor).document;
    await (editor as vscode.TextEditor).edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Two\nedited while the store was about to change\n'
      );
    });
    const saving = document.save();
    await requestInFlight(core, 'commit');

    /*
     * THE SAVE IS IN FLIGHT -- the stand-in has taken the commit and is
     * holding it until this cell lets go. The store is replaced under
     * it, which is the state the census named.
     */
    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    release('commit');
    await saving;
    await requestAnswered(core, 'commit', 0);
    /*
     * NEVER: AND THE STAND-IN'S ANSWER IS NOT THE EXTENSION'S SETTLEMENT.
     *
     * `requestAnswered` reads the stand-in's log, and the stand-in writes
     * that line BEFORE it writes the answer to stdout -- so the reading
     * below could be taken while the save handler had not yet settled
     * the entry or moved the cursor, and the assertions would then be
     * racing correct behaviour rather than measuring it. Measured in an
     * eighteenth review round.
     *
     * What this cell is about is store A's queue: its entry gone and its
     * cursor moved. So it waits for THAT, and the assertions that follow
     * say what the settled queue must look like. A wait for the thing
     * being asserted is not the assertion: the wait can end for any
     * reading at all, and the assertions are still what decide.
     */
    await until(
      () => {
        const at = queueFiles().find((q) => q.store === namespaceOf(`${core.store}-A`));
        return `store A's queue settled (it holds ${
          at === undefined ? 'no file' : `${at.entries.length} entries, cursor ${String(at.cursor)}`
        })`;
      },
      () => {
        const at = queueFiles().find((q) => q.store === namespaceOf(`${core.store}-A`));
        return at !== undefined && at.entries.length === 0 && at.cursor !== null;
      }
    );

    const sets = core.calls().filter((c) => c.coreArgv.includes('commit'));
    assert.ok(sets.length > 0, 'no save was ever sent, so this cell measured nothing');
    assert.ok(
      sets.some((c) => c.event === 'answer'),
      `the delayed save never answered, so the reading is about a request that did not finish: ${
        JSON.stringify(sets.map((c) => c.event))
      }`
    );

    const facts = await currentFacts();
    assert.strictEqual(facts.store, `${core.store}-B`, 'the settings did not actually change');
    /*
     * THE READING. `paint` recomputes from the current store, the
     * current queue and the current conflict count, so the numbers on
     * the status bar are store B's even though the answer that triggered
     * the painting was store A's. The cursor is B's too -- which is to
     * say null, because nothing has asked B anything.
     */
    assert.strictEqual(
      facts.conflicts,
      null,
      'store A’s conflict count was painted under store B'
    );
    assert.strictEqual(facts.cursor, null, 'store A’s cursor was painted under store B');

    /*
     * NOTE: AND THE FILES, WHICH IS WHERE THE DEFECT WAS.
     *
     * The status bar is recomputed from whatever is configured now, so
     * it is the wrong instrument for this: it showed nothing wrong while
     * store A's cursor sat committed in store B's `outbox.json` and
     * store A's request stayed queued, answered and unsettled, in a
     * queue nobody was looking at. Two wrong bytes on disk, no wrong
     * number on screen.
     *
     * So both queues are read. A's must have settled -- its entry gone
     * and its cursor moved to what the answer established -- and B's
     * must be exactly as it was, which is to say absent: nothing has
     * ever written to it.
     */
    const queues = queueFiles();
    const a = queues.find((q) => q.store === namespaceOf(`${core.store}-A`));
    assert.ok(a !== undefined, `store A has no queue file; the queues found were ${
      JSON.stringify(queues.map((q) => q.store))
    }`);
    assert.deepStrictEqual(
      (a as Queue).entries,
      [],
      'the answer settled nothing: store A’s request is still queued, in a queue this window has ' +
        'stopped looking at'
    );
    assert.strictEqual(
      (a as Queue).cursor,
      'w:2',
      'store A’s queue did not take the cursor its own answer established'
    );

    const b = queues.find((q) => q.store === namespaceOf(`${core.store}-B`));
    if (b !== undefined) {
      assert.strictEqual(
        b.cursor,
        null,
        'store B’s queue holds a cursor, and nothing has ever asked store B anything'
      );
    }
  });
});
