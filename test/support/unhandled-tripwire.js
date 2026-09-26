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

'use strict';

/*
 * A SUITE THAT LEAVES AN UNHANDLED REJECTION IS RED (queue item 38).
 *
 * Loaded by the unit suite with `mocha --require`. Mocha 10 re-emits a
 * rejection that is not its own on `process` and nothing else, so with no
 * listener it vanished and the suite stayed green (measured, item 33).
 * This listener counts every one and says which test was running.
 *
 * KEY: ONE PROMISE IS ONE REJECTION. Node calls the listener, and Mocha's
 * own listener re-emits the same event on `process`, which calls it again
 * (measured: two events for one rejection). Counting events would report
 * every rejection twice.
 *
 * KEY: TWO VERDICTS. The root `afterAll` fails the run for every rejection
 * seen by the end of the last test. A rejection that surfaces after that --
 * a timer a test left running -- is caught at `exit`, which prints it and
 * exits with a failing code; Mocha has already printed its summary by then.
 */

const counted = new WeakSet();
const seen = [];
let reported = 0;
let current = '(before any test)';

function nameOf(reason) {
  try {
    return reason instanceof Error ? `${reason.name}: ${reason.message}` : String(reason);
  } catch {
    return '(a reason that could not be named)';
  }
}

const originalExit = process.exit;
let exitCalled = null;
/*
 * A TEST THAT CALLS `process.exit` ENDS THE RUN FROM UNDER THE TRIPWIRE. The
 * unit script does not pass `--exit`, so nothing in a run calls it but test
 * code; a rejection made just before it is never delivered (codex, item 38
 * r1, W3, measured), and the tests after it never run. The call is recorded
 * and the run fails at `exit`.
 */
process.exit = function exit(code) {
  if (exitCalled === null) {
    exitCalled = `process.exit(${code === undefined ? '' : String(code)}) during: ${current}`;
  }
  return originalExit.call(process, code);
};

function onRejection(reason, promise) {
  if (promise !== null && typeof promise === 'object') {
    if (counted.has(promise)) {
      return;
    }
    counted.add(promise);
  }
  seen.push(`${nameOf(reason)} -- during: ${current}`);
}
process.on('unhandledRejection', onRejection);

function listing(from) {
  return seen.slice(from).map((line) => `  ${line}`).join('\n');
}

function onExit() {
  if (exitCalled !== null) {
    process.stderr.write(`unhandled-tripwire: the run was ended by ${exitCalled}\n`);
    originalExit.call(process, Math.max(Number(process.exitCode) || 0, 1));
  }
  if (seen.length > reported) {
    process.stderr.write(
      `unhandled-tripwire: ${seen.length - reported} unhandled rejection(s) after the last test\n${listing(reported)}\n`
    );
    /*
     * NOTE: EXIT HERE, NOT `process.exitCode = 1`. Mocha 10 registers its
     * own `exit` listener when the run ends (`exitMochaLater` in
     * lib/cli/run-helpers.js: `process.exitCode = clampedCode`), after this
     * one, so an exit code set here was overwritten with the failure count --
     * measured: the report printed and the run exited 0.
     */
    originalExit.call(process, Math.max(Number(process.exitCode) || 0, 1));
  }
}
process.on('exit', onExit);

/*
 * A TEST THAT REMOVES THE TRIPWIRE'S LISTENERS BLINDS IT (codex, item 38 r1,
 * W1 and W2, measured). After each test both are looked for, and a test that
 * removed either fails, named.
 */
function stillListening() {
  const missing = [];
  if (!process.listeners('unhandledRejection').includes(onRejection)) {
    missing.push("its 'unhandledRejection' listener");
  }
  if (!process.listeners('exit').includes(onExit)) {
    missing.push("its 'exit' listener");
  }
  if (missing.length > 0) {
    throw new Error(`unhandled-tripwire: ${current} removed ${missing.join(' and ')}`);
  }
}

exports.mochaHooks = {
  beforeEach() {
    current = this.currentTest ? this.currentTest.fullTitle() : '(unknown test)';
  },
  afterEach() {
    stillListening();
  },
  afterAll() {
    current = '(after the last test)';
    if (seen.length > 0) {
      reported = seen.length;
      throw new Error(`unhandled-tripwire: ${seen.length} unhandled rejection(s)\n${listing(0)}`);
    }
  }
};
