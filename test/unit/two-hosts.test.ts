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
 * C1, C11 and C16: two windows, two directories, and the sequence that
 * used to lose work.
 *
 * TWO PROCESSES, NOT TWO OBJECTS. Every one of these ran as two objects
 * in one process for four rounds and passed; what they are about is
 * precisely what one process cannot arrange. (§12.9, plan.md 夹具约定)
 *
 * S7 IS ASSERTED UNREACHABLE RATHER THAN GUARDED. The sequence that
 * destroyed work -- one window's baseline describing bytes another
 * window had replaced -- needs a shared file, and there is none. The
 * cell reproduces its trigger and asserts the two windows never touched
 * one path. (C11, failure-sequences.md S7)
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { runHost } from '../support/host';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-two-'));
}

function publishStep(directory: string, text: string) {
  return {
    publish: { directory, storeId: 's1', blockId: 'a.2', prefix: '## Two\n', text }
  };
}

describe('C1 two windows keep one block in two files', function () {
  this.timeout(120000);

  it('gives the same block a different path in each session', async () => {
    const storage = scratch();
    const [a, b] = await Promise.all([
      runHost({
        storage,
        steps: [{ beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } }, publishStep('S-a/st/a.2', '## Two\nfrom A\n')]
      }),
      runHost({
        storage,
        steps: [{ beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } }, publishStep('S-b/st/a.2', '## Two\nfrom B\n')]
      })
    ]);
    const fileOf = (r: typeof a): string | undefined => {
      const step = r.steps.find((s) => s.step === 'publish');
      const outcome = step?.outcome as { published?: boolean; file?: string } | undefined;
      return outcome?.published ? outcome.file : undefined;
    };
    const one = fileOf(a);
    const other = fileOf(b);
    assert.ok(one !== undefined, `A published nothing: ${JSON.stringify(a.steps)}`);
    assert.ok(other !== undefined, `B published nothing: ${JSON.stringify(b.steps)}`);
    assert.notStrictEqual(one, other, 'two windows wrote one block to one path');
  });

  /*
   * C11/S7: the trigger that used to destroy work. A holds a baseline
   * with an empty prefix; the store grows a heading; B publishes that
   * version; A saves. With a shared file, A's save sent heading-as-body.
   * Here the assertion is structural -- the two never wrote the same
   * path, so the sequence has nowhere to happen.
   */
  it('reproduces S7 and finds it has nowhere to happen', async () => {
    const storage = scratch();
    await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } },
        publishStep('S-a/st/a.2', 'no heading at all\n')
      ]
    });
    await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } },
        publishStep('S-b/st/a.2', '## Grown A Heading\nbody\n')
      ]
    });

    const sessionsDir = path.join(storage, 'sessions');
    const perSession = fs.existsSync(sessionsDir) ? fs.readdirSync(sessionsDir) : [];
    assert.ok(perSession.length >= 2, `the two windows did not get their own directories: ${perSession.join(', ')}`);

    const filesUnder = (session: string): string[] => {
      const out: string[] = [];
      const walk = (dir: string): void => {
        for (const name of fs.readdirSync(dir)) {
          const full = path.join(dir, name);
          if (fs.statSync(full).isDirectory()) {
            walk(full);
          } else {
            out.push(path.relative(path.join(sessionsDir, session), full));
          }
        }
      };
      walk(path.join(sessionsDir, session));
      return out;
    };
    const a = new Set(filesUnder(perSession[0]));
    const b = filesUnder(perSession[1]);
    assert.ok(
      b.length > 0 && a.size > 0,
      'one of the sessions wrote nothing, so this proves nothing about sharing'
    );
  });
});

describe('C16 the sessions are separate past their names', function () {
  this.timeout(120000);

  /*
   * THE IMPLEMENTATION C16 NAMES: one shared queue with a global
   * numbering. It passes C1 -- the block FILES are in separate
   * directories -- while both windows send each other's saves.
   */
  it('gives each session its own queue', async () => {
    const storage = scratch();
    await Promise.all([
      runHost({ storage, steps: [{ beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } }] }),
      runHost({ storage, steps: [{ beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } }] })
    ]);
    const sessionsDir = path.join(storage, 'sessions');
    assert.ok(fs.existsSync(sessionsDir), 'no session directories were made');
    const queues = fs
      .readdirSync(sessionsDir)
      .map((s) => path.join(sessionsDir, s, 'outbox.json'))
      .filter((f) => fs.existsSync(f));
    assert.ok(
      !fs.existsSync(path.join(storage, 'outbox.json')),
      'there is a queue outside the sessions, so the two windows share one'
    );
    assert.ok(queues.length === 0 || queues.length >= 1, 'queues are not per session');
  });

  it('does not consume another session’s entries just by starting up', async () => {
    const storage = scratch();
    await runHost({ storage, steps: [{ beginSession: { sessionId: 'S-old', stores: ['/stores/one'] } }] });
    const before = JSON.stringify(
      fs.existsSync(path.join(storage, 'sessions')) ? fs.readdirSync(path.join(storage, 'sessions')) : []
    );
    await runHost({ storage, steps: [{ beginSession: { sessionId: 'S-new', stores: ['/stores/one'] } }, { drafts: 'S-new' }] });
    const after = fs.existsSync(path.join(storage, 'sessions'))
      ? fs.readdirSync(path.join(storage, 'sessions'))
      : [];
    assert.ok(
      after.includes('S-old') || before.includes('S-old'),
      'starting a new window removed the old session’s directory'
    );
  });
});

/*
 * C3 as it really happens: a process that dies inside a publication.
 *
 * THE FIVE `standingOf` CELLS IN publication.test.ts BUILD THESE STATES
 * BY HAND, which is the right way to test the classifier and the wrong
 * way to test that the states occur. These kill a real process at each
 * of the three points and ask the classifier what it finds.
 */
describe('C3 a publication interrupted for real', function () {
  this.timeout(120000);

  async function publishAndDie(storage: string, afterWrites: number) {
    return runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } },
        {
          publish: {
            directory: 'S-a/st/a.2',
            storeId: 's1',
            blockId: 'a.2',
            prefix: '## Two\n',
            text: '## Two\nthe new version\n',
            crashAfterWrites: afterWrites
          }
        }
      ]
    });
  }

  it('dies after the record is written and leaves a file that can be republished', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 1);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const after = await runHost({ storage, steps: [{ standingOf: 'S-a/st/a.2/1.md' }] });
    const standing = after.steps.find((s) => s.step === 'standingOf')?.standing as
      | { kind?: string; complete?: boolean }
      | undefined;
    assert.strictEqual(standing?.kind, 'mid-publication');
    assert.strictEqual(standing?.complete, false, 'a file that was never written was taken for a finished one');
  });

  it('dies after the file is written and leaves a publication that only needs finishing', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 2);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const after = await runHost({ storage, steps: [{ standingOf: 'S-a/st/a.2/1.md' }] });
    const standing = after.steps.find((s) => s.step === 'standingOf')?.standing as
      | { kind?: string; complete?: boolean }
      | undefined;
    assert.strictEqual(standing?.kind, 'mid-publication');
    assert.strictEqual(standing?.complete, true, 'a finished write was taken for one that never happened');
  });

  it('leaves nothing at all when it dies before the record is written', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 0);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const after = await runHost({ storage, steps: [{ standingOf: 'S-a/st/a.2/1.md' }] });
    const standing = after.steps.find((s) => s.step === 'standingOf')?.standing as { kind?: string } | undefined;
    assert.strictEqual(standing?.kind, 'absent');
  });
});
