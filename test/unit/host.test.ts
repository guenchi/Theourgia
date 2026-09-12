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
 * The second host, before anything is measured with it.
 *
 * EIGHT OF THE X1c CELLS ARE ABOUT TWO PROCESSES AND FOUR ARE ABOUT
 * CRASHES. If the harness quietly ran one process, or its "crash" were
 * an exception the child recovered from, those twelve would report on
 * something else entirely -- and they would report success, because the
 * hazards they look for are hazards of concurrency and interruption.
 * So: it is made to prove it started a different process, and that a
 * crash step really ends the child with steps left undone.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { idleProcess, runHost } from '../support/host';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-host-'));
}

describe('the second host is a second process', function () {
  this.timeout(60000);

  it('runs in a different process from the suite', async () => {
    const result = await runHost({ storage: scratch(), steps: [{ pid: true }] });
    const reported = result.steps.find((s) => s.step === 'pid');
    assert.ok(reported !== undefined, `no pid was reported: ${JSON.stringify(result)}`);
    assert.notStrictEqual(
      reported.value,
      process.pid,
      'the "second host" is this process, so nothing here is about two windows'
    );
    assert.strictEqual(result.code, 0);
  });

  it('runs two hosts that are different processes from each other', async () => {
    const storage = scratch();
    const [a, b] = await Promise.all([
      runHost({ storage, steps: [{ pid: true }] }),
      runHost({ storage, steps: [{ pid: true }] })
    ]);
    const pidOf = (r: typeof a): unknown => r.steps.find((s) => s.step === 'pid')?.value;
    assert.notStrictEqual(pidOf(a), pidOf(b), 'both hosts were one process');
  });

  it('reports every step it ran, in order', async () => {
    const result = await runHost({
      storage: scratch(),
      steps: [{ note: 'first' }, { note: 'second' }, { note: 'third' }]
    });
    assert.deepStrictEqual(
      result.steps.map((s) => s.value ?? s.step),
      ['first', 'second', 'third', 'done'],
      'the report is not the sequence that ran'
    );
  });

  it('really writes to the storage directory the cell gave it', async () => {
    const storage = scratch();
    await runHost({ storage, steps: [{ writeFile: { name: 'a/b.md', text: 'from the other host\n' } }] });
    assert.strictEqual(
      fs.readFileSync(path.join(storage, 'a', 'b.md'), 'utf8'),
      'from the other host\n',
      'the other host did not reach the shared storage directory'
    );
  });
});

describe('a crash step ends the child with work undone', function () {
  this.timeout(60000);

  it('leaves the steps after it unreported and exits non-zero', async () => {
    const result = await runHost({
      storage: scratch(),
      steps: [{ note: 'before' }, { crash: 'here' }, { note: 'after' }]
    });
    const names = result.steps.map((s) => s.value ?? s.step);
    assert.ok(names.includes('before'), 'the step before the crash did not run');
    assert.ok(!names.includes('after'), 'a step after the crash ran, so this is not a crash');
    assert.ok(!names.includes('done'), 'the child finished normally');
    assert.strictEqual(result.code, 9, `the child exited ${result.code}, not by the crash`);
  });

  /*
   * AND THE WORK BEFORE THE CRASH IS ON DISK. A crash that also lost the
   * writes preceding it would make every recovery cell vacuous: there
   * would be nothing to recover.
   */
  it('leaves what it wrote before the crash on disk', async () => {
    const storage = scratch();
    await runHost({
      storage,
      steps: [{ writeFile: { name: 'kept.md', text: 'written before the crash\n' } }, { crash: 'after-write' }]
    });
    assert.strictEqual(
      fs.readFileSync(path.join(storage, 'kept.md'), 'utf8'),
      'written before the crash\n',
      'the crash discarded the write it was supposed to follow'
    );
  });
});

describe('the harness has a deadline of its own', function () {
  this.timeout(60000);

  it('kills a child that will not finish and says so, keeping what it reported', async () => {
    const result = await runHost({
      storage: scratch(),
      steps: [{ note: 'got here' }, { sleep: 30000 }, { note: 'never' }],
      timeoutMs: 1500
    });
    assert.strictEqual(result.timedOut, true, 'a hanging child was not reported as a timeout');
    assert.ok(
      result.steps.some((s) => s.value === 'got here'),
      'the steps that did complete were discarded with the child'
    );
    assert.ok(!result.steps.some((s) => s.value === 'never'), 'the child ran past the deadline');
  });
});

describe('a live pid for the liveness cells', function () {
  this.timeout(60000);

  it('is alive while it runs and dead after it is stopped', async () => {
    const idle = idleProcess();
    try {
      assert.doesNotThrow(() => process.kill(idle.pid, 0), 'the idle process was not alive');
    } finally {
      idle.stop();
    }
    await new Promise((r) => setTimeout(r, 300));
    assert.throws(() => process.kill(idle.pid, 0), /ESRCH/, 'the stopped process still reads as alive');
  });
});
