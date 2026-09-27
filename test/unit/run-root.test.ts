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
 * THE WHOLE SUITE IS WATCHED, NOT EACH FIXTURE.
 *
 * NOTE: WHY THIS FILE EXISTS. The shipping transport starts a daemon, and a
 * daemon's socket goes under `THEOURGIA_RUN` -- which, unset, is the
 * user's own `~/.theourgia/run`. When these cells were first turned onto
 * that transport, one run of this suite left THIRTEEN daemons running
 * and thirteen directories in it. The suite was green. It was cleaned up
 * by hand.
 *
 * NEVER: AND FIXING THE FIXTURE WAS NOT ENOUGH, which is the reason this is a
 * gate over everything rather than an assertion inside `RealStore`.
 * Several cells build their own transport around the store's config --
 * `new CliTransport(store.config)` -- and that spelling takes
 * `process.env`, which names no run root. With the fixture's own client
 * corrected and nothing else, a full run still left three daemons and
 * three directories behind. Six call sites had to change, and the next
 * cell somebody writes will be a seventh; a rule that depends on
 * remembering is not what should be holding this.
 *
 * THE MEASURE IS GROWTH, NOT THE TOTAL. Whatever is in that directory
 * when the suite starts belongs to whoever put it there -- another
 * session's daemon, an editor the user has open -- and a gate that
 * demanded it be empty would be red on a working machine and would be
 * switched off within a day.
 */

import * as assert from 'assert';
import { execFileSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as ts from 'typescript';
import {cannotFinishQuietly, namesOfTheFailure, onlyWhenAbsent, returnsIn} from '../support/catches';
import { additionsTo, entriesIn, usersRunRoot } from '../support/run-root';
import { FakeCore } from '../support/fake';

/*
 * KEY: AND EVERY PROCESS THIS RUN STARTED, WHEREVER ITS SOCKET WENT.
 *
 * The directory count above answers "did anything land in the user's
 * run root". It does NOT answer "is anything of mine still running": a
 * fixture that redirected its socket correctly and then failed to stop
 * the daemon leaves the user's directory untouched and a process
 * behind, and the core's line found one of this line's daemons that way
 * -- alive for hours, in nobody's gate.
 *
 * The scope is this process's own fixtures, which is what makes the
 * assertion sound: `RealStore` names its run root `tvr-<pid>-<n>` with
 * THIS pid, so nothing another session owns can match, and NEVER: nothing
 * here ever kills by a pattern like `scheme`.
 */
function ourDaemons(): string[] {
  const mark = `tvr-${process.pid}-`;
  /*
   * NEVER: NOT WRAPPED IN A `catch` THAT ANSWERS "none". A `ps` that could
   * not be run would then make this gate announce that nothing leaked --
   * the one answer it must never give. It is allowed to throw; a hook
   * that cannot look is a failing hook, which is the truth.
   */
  const listing = execFileSync('ps', ['-ax', '-o', 'pid=,command='], { encoding: 'utf8' });
  return listing
    .split('\n')
    .filter((line) => line.includes('--socket') && line.includes(mark))
    .map((line) => line.trim());
}

let atStart: string[] = [];

before(function () {
  atStart = entriesIn(usersRunRoot());
});

after(function () {
  const left = ourDaemons();
  assert.deepStrictEqual(
    left,
    [],
    `this suite left ${left.length} daemon(s) of its own running after every fixture had been ` +
      'disposed:\n' +
      left.join('\n') +
      '\nA signal that was sent is not a process that went. Stop them by pid.'
  );
  const added = additionsTo(usersRunRoot(), atStart);
  assert.deepStrictEqual(
    added,
    [],
    `this suite put ${added.length} directory/directories into ${usersRunRoot()}, which belongs ` +
      'to whoever is logged in: ' +
      added.join(', ') +
      '. Some cell started a daemon without telling it where to live. Every transport onto a ' +
      'real core must be built with the fixture\'s environment -- `store.transport()`, not ' +
      '`new CliTransport(store.config)` -- and there are almost certainly processes still ' +
      'running: `ps -ax -o pid=,command= | grep -- --socket`.'
  );
});

/*
 * THE GATE'S OWN TWO PROPERTIES. A hook that runs once and reports
 * nothing is checked by nothing; these ask the two questions whose wrong
 * answers would make the hook above pass for ever.
 */
describe('plugin-r2 T6 the run-root gate reads the user\'s directory', function () {
  it('does not follow THEOURGIA_RUN, which is what the fixtures redirect', function () {
    const was = process.env.THEOURGIA_RUN;
    process.env.THEOURGIA_RUN = path.join(os.tmpdir(), 'somewhere-a-fixture-owns');
    try {
      assert.strictEqual(
        usersRunRoot(),
        path.join(process.env.HOME ?? '/tmp', '.theourgia', 'run'),
        'the gate reads the same variable the fixtures set, so it is comparing a scratch ' +
          'directory with itself and can never fail'
      );
    } finally {
      if (was === undefined) {
        delete process.env.THEOURGIA_RUN;
      } else {
        process.env.THEOURGIA_RUN = was;
      }
    }
  });

  it('names what was added and ignores what was already there', function () {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'tv-gate-'));
    try {
      fs.mkdirSync(path.join(dir, 'was-here'));
      const before = entriesIn(dir);
      fs.mkdirSync(path.join(dir, 'arrived'));
      /*
       * THE FUNCTION THE HOOK CALLS, not a copy of what it does.
       */
      assert.deepStrictEqual(additionsTo(dir, before), ['arrived']);
      assert.deepStrictEqual(additionsTo(dir, entriesIn(dir)), []);
    } finally {
      fs.rmSync(dir, { recursive: true, force: true });
    }
  });
});

/*
 * plugin-r2 T6: a setup that fails, and why there is no cell here.
 *
 * An outside review found that `RealStore.make` sends `init` and throws
 * the refusal before handing the store back, so nothing in the suite
 * held a reference to stop a daemon that request had started -- every
 * teardown here is written `store?.dispose()` against a variable that
 * was never assigned. The cleanup was added: `make` disposes and
 * re-raises.
 *
 * KEY: BUT THERE IS NO CELL FOR IT, because the failure it describes
 * cannot be produced today, and a cell that passes either way is worse
 * than none. `init` is the one verb the core routes LOCALLY -- `describe`
 * answers `(init (usage (init)) ... (route local))` -- so it never
 * reaches a daemon and never starts one. Measured two ways against a
 * core pinned before F100b (877f0da or earlier), a store path under a regular file and a store directory
 * with no write permission: both refuse, and `ps` shows no daemon for
 * either. A cell written on that premise was drafted, passed, and passed
 * again with the cleanup mutated away; it was deleted rather than kept.
 *
 * What would make it writable: `make` sending anything the core routes
 * to a daemon before it can fail. Named here rather than left for
 * somebody to rediscover, and named in the delivery note.
 */

/*
 * plugin-r2: the hygiene readers refuse a look that failed.
 *
 * KEY: THESE GUARDS HAD NO CELLS, and an eleventh review round said so.
 * Every one of them was added because the reader had answered "nothing
 * is there" for a look that could not be taken -- the reassuring answer,
 * from the instruments the whole T6 section rests on. The repairs were
 * measured by injecting failures in a reviewer's memory, and nothing in
 * this tree would have noticed them being undone.
 *
 * NOTE: WHAT IS MEASURED IS A REAL FAILURE, not a stubbed one: a path
 * inside a directory with no execute permission cannot be read, and
 * `readdirSync` raises EACCES on it. The directory is made and removed
 * here.
 */
describe('plugin-r2 a look that failed is not a look that found nothing', function () {
  let locked: string | null = null;

  afterEach(() => {
    if (locked !== null) {
      try {
        fs.chmodSync(locked, 0o700);
        fs.rmSync(locked, { recursive: true, force: true });
      } catch (e) {
        /* the cell has already reported what matters */
      }
      locked = null;
    }
  });

  /*
   * A DIRECTORY WHOSE CONTENTS CANNOT BE LISTED. Running as root defeats
   * permissions, so the cell says it could not set the situation up
   * rather than passing on a reading it did not take.
   */
  const unreadable = (): string => {
    const root = fs.mkdtempSync(path.join(os.tmpdir(), 'tv-unreadable-'));
    locked = root;
    fs.mkdirSync(path.join(root, 'inside'));
    fs.chmodSync(root, 0o000);
    return path.join(root, 'inside');
  };

  it('refuses a directory it cannot read, rather than calling it empty', function () {
    const where = unreadable();
    let raised: unknown = null;
    try {
      entriesIn(where);
    } catch (e) {
      raised = e;
    }
    if (raised === null) {
      /*
       * NOTE: NOT A PASS. If the directory could be read after all -- as
       * root, or on a filesystem that ignores the mode -- then this cell
       * did not put the reader in the situation it is about, and saying
       * so is the only honest outcome.
       */
      assert.fail(
        `${where} could be listed although its parent has no permissions, so this machine cannot ` +
          'produce the failure this cell is about'
      );
    }
    assert.match(
      (raised as Error).message,
      /could not read/,
      `the refusal does not say what went wrong: ${(raised as Error).message}`
    );
  });

  /*
   * THE TWIN: a directory that is NOT THERE really is empty, and that
   * case is ordinary -- the user may never have run the core. Without
   * this, the cell above would pass on a reader that refuses everything
   * and the gate would be red on every machine with no run root.
   */
  it('still calls an absent directory empty', function () {
    assert.deepStrictEqual(entriesIn(path.join(os.tmpdir(), 'tv-not-there-at-all')), []);
    assert.deepStrictEqual(additionsTo(path.join(os.tmpdir(), 'tv-not-there-at-all'), []), []);
  });

  it('refuses a stand-in call log it cannot read', function () {
    const where = unreadable();
    const core = new FakeCore([]);
    try {
      (core as unknown as { logFile: string }).logFile = path.join(where, 'calls.jsonl');
      let raised: unknown = null;
      try {
        core.calls();
      } catch (e) {
        raised = e;
      }
      if (raised === null) {
        assert.fail(`${where} could be read although its parent has no permissions`);
      }
      assert.match((raised as Error).message, /could not read the stand-in's call log/);
    } finally {
      core.dispose();
    }
  });

  /*
   * THE TWIN, again: a stand-in that has written no line yet has no
   * calls, and that is how every cell in this suite starts.
   */
  it('still reports no calls for a stand-in that has written none', function () {
    const core = new FakeCore([]);
    try {
      assert.deepStrictEqual(core.calls(), []);
      assert.deepStrictEqual(core.pidsSeen(), []);
    } finally {
      core.dispose();
    }
  });
});

/*
 * plugin-r2: the rest of the hygiene readers' failure paths.
 *
 * KEY: THE FIRST SECTION COVERED TWO OF THEM AND A TWELFTH REVIEW ROUND
 * NAMED THE REST. `countRealRunRoot` keeps its own catch in the editor
 * runner, and `ps` is run in four places. Listing them here, with the
 * one that cannot be reached from this process named as such, is what
 * makes a fifth an obvious omission rather than a discovery.
 */
describe('plugin-r2 every hygiene reader refuses a look that failed', function () {
  /*
   * WHAT RUNS `ps`, AND WHICH OF THEM THIS PROCESS CAN DRIVE.
   *
   * `daemonsMatching` and `socketPathOf` in test/support/real-core.ts and
   * `ourDaemons` above all call `execFileSync('ps', ...)` with no catch,
   * so a failure raises. The fourth -- `schemeProcesses` in
   * test/integration/run.ts -- is in the launcher, a separate process
   * this suite does not load; it is read as source below instead, which
   * is weaker and is said to be.
   */
  it('runs ps without swallowing its failure, in every reader this process loads', function () {
    const root = path.join(__dirname, '..', '..', '..');
    const files = [
      path.join(root, 'test', 'support', 'real-core.ts'),
      path.join(root, 'test', 'unit', 'run-root.test.ts'),
      path.join(root, 'test', 'integration', 'run.ts')
    ];
    const swallowed: string[] = [];
    for (const file of files) {
      const text = fs.readFileSync(file, 'utf8');
      const parsed = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
      const visit = (node: ts.Node): void => {
        if (ts.isTryStatement(node) && /execFileSync\(\s*'ps'/.test(node.tryBlock.getText(parsed))) {
          /*
           * NEVER: THE WORD `throw` IN THE TEXT IS NOT A `throw` STATEMENT.
           *
           * This searched the catch's source for /throw/, and the
           * comment beside one of these readers says "with execFileSync
           * throwing" -- so replacing the real `throw` with `return []`
           * left the census green. Measured in a thirteenth review
           * round. It was my own census, and it was reading prose.
           *
           * The walk asks the parser for a `throw` STATEMENT instead,
           * which a comment cannot be.
           */
          /*
           * KEY: EVERY PATH OUT OF THE CATCH MUST THROW, not one of them.
           *
           * Measured in a fourteenth review round: adding
           * `if (code === 'EACCES') return [];` in front of the existing
           * `throw` left this census green, and the reader then answered
           * "nothing is running" for exactly the failure it was repaired
           * for. One throwing path is not proof about the others; a
           * `return` beside it is the defect coming back.
           */
          /*
           * KEY: AND "EVERY PATH" INCLUDES FALLING OFF THE END.
           *
           * Measured in a fifteenth review round: making the catch read
           * `if (code !== 'EACCES') { throw e; }` and putting
           * `listing ??= ''` after the try left this census green. There
           * is a throw statement in that catch and there is no return in
           * it, and EACCES still reaches the caller as an empty process
           * listing -- through the one exit neither question asked
           * about. What is asked now is that the block CANNOT FINISH,
           * which is a property of its last statement.
           */
          const block = node.catchClause?.block;
          const answers = block === undefined ? [] : returnsIn(block);
          const refuses = block !== undefined && cannotFinishQuietly(block);
          if (!refuses || answers.length > 0) {
            swallowed.push(
              `${path.basename(file)}:${parsed.getLineAndCharacterOfPosition(node.getStart(parsed)).line + 1}`
            );
          }
        }
        node.forEachChild(visit);
      };
      visit(parsed);
    }
    assert.deepStrictEqual(
      swallowed,
      [],
      'these readers answer "nothing is running" for a process listing they could not take, ' +
        'which is the one answer a leak gate must never give'
    );
  });

  /*
   * THE CENSUS'S OWN FIRST READING: the files it is about really do run
   * `ps`. A walk that found none would pass for ever while looking at
   * nothing.
   */
  it('finds the readers it is about', function () {
    const root = path.join(__dirname, '..', '..', '..');
    const found = [
      path.join(root, 'test', 'support', 'real-core.ts'),
      path.join(root, 'test', 'unit', 'run-root.test.ts'),
      path.join(root, 'test', 'integration', 'run.ts')
    ].filter((file) => /execFileSync\(\s*'ps'/.test(fs.readFileSync(file, 'utf8')));
    assert.deepStrictEqual(found.length, 3, 'the process readers have moved, so nothing is checked');
  });

  it('counts the real run root without swallowing a failure to read it', function () {
    const file = path.join(__dirname, '..', '..', '..', 'test', 'integration', 'run.ts');
    const text = fs.readFileSync(file, 'utf8');
    const parsed = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
    let checked = false;
    const visit = (node: ts.Node): void => {
      if (
        ts.isFunctionDeclaration(node) &&
        node.name?.getText(parsed) === 'countRealRunRoot' &&
        node.body !== undefined
      ) {
        checked = true;
        /*
         * NEVER: TOKEN PRESENCE IS NOT BEHAVIOUR. This asked whether the
         * words ENOENT and throw appear in the function, which a
         * condition of `if (true || code === 'ENOENT')` satisfies while
         * returning zero for every failure -- measured in a thirteenth
         * review round. What is asked now is that the catch re-raises
         * on some path and that the only early answer is guarded by a
         * comparison against ENOENT.
         */
        /*
         * KEY: AND THE QUESTION IS ASKED OF EACH ANSWER, not of the
         * function.
         *
         * Measured in a fifteenth review round, twice over. Adding
         * `if (code === 'EACCES') return 0;` IN FRONT of the real guard
         * left this green -- the old guard was still there for the
         * census to find, and the new one answered zero for a run root
         * this process may not read. Writing the real guard as
         * `!(code === 'ENOENT')` left it green too, because those eight
         * characters were all it was looking for.
         *
         * So: the catch must not be able to finish quietly, and EVERY
         * return in it must sit in the then-branch of a test that means
         * absence and nothing else. `testsForAbsence` puts that to the
         * parser rather than to a regular expression.
         */
        const caught = node.body.statements.find(ts.isTryStatement)?.catchClause;
        assert.ok(caught !== undefined, 'countRealRunRoot no longer catches anything, so this cell reads nothing');
        const failure = caught.block;
        assert.ok(
          cannotFinishQuietly(failure),
          'countRealRunRoot answers for some failure, so a run root it could not read reads as ' +
            'empty at both ends and the growth gate compares nothing with nothing'
        );
        const carried = namesOfTheFailure(caught, parsed);
        const unguarded = returnsIn(failure)
          .filter((answer) => !onlyWhenAbsent(answer, failure, parsed, carried))
          .map((answer) => answer.getText(parsed));
        assert.deepStrictEqual(
          unguarded,
          [],
          'these answers are given for failures that are not an absence, so a run root this ' +
            'process could not read counts as empty'
        );
      }
      node.forEachChild(visit);
    };
    visit(parsed);
    assert.ok(checked, 'countRealRunRoot is gone, so this cell reads nothing');
  });
});
