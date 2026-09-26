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
 * plugin-r3 item 38: the unit suite fails when a test leaves an unhandled
 * rejection. Each cell runs Mocha in a child process on a small fixture file,
 * with the tripwire loaded the way the suite loads it, and reads the exit
 * code and the output. Nothing here starts the core.
 */

import * as assert from 'assert';
import { spawnSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

const TRIPWIRE = path.join(__dirname, '..', 'support', 'unhandled-tripwire.js');
const ROOT = path.join(__dirname, '..', '..', '..');
const MOCHA = path.join(ROOT, 'node_modules', 'mocha', 'bin', 'mocha.js');

function runFixture(body: string): { status: number | null; output: string } {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-tripwire-'));
  try {
    const file = path.join(dir, 'fixture.test.js');
    fs.writeFileSync(file, body, 'utf8');
    const run = spawnSync(process.execPath, [MOCHA, '--reporter', 'dot', '--require', TRIPWIRE, file], {
      encoding: 'utf8',
      timeout: 20000
    });
    return { status: run.status, output: `${run.stdout}${run.stderr}` };
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

describe('plugin-r3 38 a suite that leaves an unhandled rejection is red', function () {
  this.timeout(30000);

  /*
   * T1: A TEST THAT LEAVES A REJECTION FAILS THE RUN, and the report names
   * the test and the reason -- in Mocha's own summary, as a failure. A run
   * that fails only by its exit code, with a report that reads green, is the
   * shape this tripwire is for (X1, the root afterAll no longer throwing,
   * measured: the exit check alone kept the exit code at 1).
   */
  it('T1 fails the run and names the test and the reason', () => {
    const run = runFixture(`
      it('leaves one behind', () => {
        Promise.reject(new Error('left behind'));
        return new Promise((resolve) => setImmediate(resolve));
      });
    `);
    assert.notStrictEqual(run.status, 0, run.output);
    assert.ok(run.output.includes('left behind'), run.output);
    assert.ok(run.output.includes('leaves one behind'), run.output);
    assert.ok(/\b1 failing\b/.test(run.output), `Mocha's own summary does not count the failure: ${run.output}`);
  });

  /*
   * T2: T1'S TWIN -- a rejection that is handled leaves the run green.
   */
  it('T2 leaves a run green when every rejection is handled', () => {
    const run = runFixture(`
      it('handles its own', () => {
        Promise.reject(new Error('handled')).catch(() => undefined);
        return new Promise((resolve) => setImmediate(resolve));
      });
    `);
    assert.strictEqual(run.status, 0, run.output);
  });

  /*
   * T3: ONE PROMISE IS ONE REJECTION. Mocha re-emits the event on `process`,
   * so the listener hears it twice; the report says one.
   */
  it('T3 counts a rejection that Mocha re-emits once', () => {
    const run = runFixture(`
      it('leaves exactly one', () => {
        Promise.reject(new Error('only one'));
        return new Promise((resolve) => setImmediate(resolve));
      });
    `);
    assert.notStrictEqual(run.status, 0, run.output);
    assert.ok(run.output.includes('unhandled-tripwire: 1 unhandled rejection(s)'), run.output);
    assert.ok(!run.output.includes('unhandled-tripwire: 2 unhandled rejection(s)'), run.output);
  });

  /*
   * T4: A REJECTION THAT SURFACES AFTER THE LAST TEST -- a timer the test
   * left running -- fails the run at exit.
   */
  it('T4 fails the run for a rejection that surfaces after the last test', () => {
    const run = runFixture(`
      it('leaves a timer running', () => {
        setTimeout(() => {
          Promise.reject(new Error('after the last test'));
        }, 50);
      });
    `);
    assert.notStrictEqual(run.status, 0, run.output);
    assert.ok(run.output.includes('after the last test'), run.output);
  });

  /*
   * T5: A TEST THAT REMOVES THE TRIPWIRE'S LISTENERS FAILS, named -- both
   * listeners, each on its own (codex, item 38 r1, W1 and W2: removing
   * either blinded the tripwire and the run stayed green).
   */
  it('T5 fails a test that removes either of its listeners, and names it', () => {
    const rejections = runFixture(`
      it('removes the rejection listener', () => {
        process.removeAllListeners('unhandledRejection');
        process.on('unhandledRejection', () => undefined);
        Promise.reject(new Error('unseen'));
        return new Promise((resolve) => setImmediate(resolve));
      });
    `);
    assert.notStrictEqual(rejections.status, 0, rejections.output);
    assert.ok(rejections.output.includes("removes the rejection listener removed its 'unhandledRejection' listener"), rejections.output);
    const exits = runFixture(`
      it('removes the exit listener', () => {
        process.removeAllListeners('exit');
      });
    `);
    assert.notStrictEqual(exits.status, 0, exits.output);
    assert.ok(exits.output.includes("removes the exit listener removed its 'exit' listener"), exits.output);
  });

  /*
   * T6: A RUN ENDED BY `process.exit` FAILS. The unit script does not pass
   * `--exit`, so only test code calls it; a rejection made just before it
   * is never delivered (codex, item 38 r1, W3), and the tests after it never
   * run.
   */
  it('T6 fails a run that a test ends with process.exit', () => {
    const run = runFixture(`
      it('ends the run', () => {
        setTimeout(() => {
          Promise.reject(new Error('never delivered'));
          process.exit(0);
        }, 50);
      });
    `);
    assert.notStrictEqual(run.status, 0, run.output);
    assert.ok(run.output.includes('the run was ended by process.exit(0)'), run.output);
  });

  /*
   * THE SUITE LOADS IT. A tripwire that is not loaded is silent, and its
   * cells above stay green: they load it themselves. So the unit script is
   * read: it must pass `--require` with this file -- and not `--exit`, under
   * which Mocha ends the process before a rejection made in a root `after`
   * hook is delivered (codex, item 38 r1, measured).
   */
  it('is loaded by the unit script', () => {
    const scripts = JSON.parse(fs.readFileSync(path.join(ROOT, 'package.json'), 'utf8')).scripts as Record<string, string>;
    assert.ok(
      scripts['test:unit'].includes('--require out/test/support/unhandled-tripwire.js'),
      `test:unit does not load the tripwire: ${scripts['test:unit']}`
    );
    assert.ok(!/(^|\s)--exit(\s|$)/.test(scripts['test:unit']), `test:unit passes --exit: ${scripts['test:unit']}`);
  });
});
