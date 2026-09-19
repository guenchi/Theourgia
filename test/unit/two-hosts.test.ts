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
 * precisely what one process cannot arrange. (section 12.9, and the
 * fixture conventions in plan.md)
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

function publishStep(sessionId: string, text: string, prefix = '## Two\n') {
  return {
    publish: { sessionId, storeId: 's1', blockId: 'a.2', prefix, text }
  };
}

describe('C1 two windows keep one block in two files', function () {
  this.timeout(120000);

  it('gives the same block a different path in each session', async () => {
    const storage = scratch();
    const [a, b] = await Promise.all([
      runHost({
        storage,
        steps: [{ beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } }, publishStep('S-a', '## Two\nfrom A\n')]
      }),
      runHost({
        storage,
        steps: [{ beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } }, publishStep('S-b', '## Two\nfrom B\n')]
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
   * C11/S7: the trigger that used to destroy work, run for real.
   *
   * NOTE: THE FIRST VERSION NEVER SAVED. It published into paths it made up
   * outside the sessions tree and then asserted that two directories
   * existed -- which they did, holding nothing but `session.json`. The
   * sequence S7 names is: A holds an empty-prefix baseline, the store
   * grows a heading, B publishes that, and A SAVES. Without the save
   * there is no sequence, only two directories.
   */
  it('reproduces S7 and finds it has nowhere to happen', async () => {
    const storage = scratch();
    const a = await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } },
        publishStep('S-a', 'no heading at all\n', ''),
        { save: { sessionId: 'S-a', blockId: 'a.2', name: 'current.md', text: 'no heading at all\n' } }
      ]
    });
    const b = await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } },
        publishStep('S-b', '## Grown A Heading\nbody\n', '## Grown A Heading\n'),
        { save: { sessionId: 'S-b', blockId: 'a.2', name: 'current.md', text: '## Grown A Heading\nbody\n' } }
      ]
    });

    const decisionOf = (r: typeof a): { send?: boolean; src?: string } =>
      (r.steps.find((s) => s.step === 'save')?.decision ?? {}) as { send?: boolean; src?: string };
    const fromA = decisionOf(a);
    const fromB = decisionOf(b);

    assert.strictEqual(fromA.send, true, `A sent nothing: ${JSON.stringify(a.steps)}`);
    assert.strictEqual(fromB.send, true, `B sent nothing: ${JSON.stringify(b.steps)}`);
    /*
     * THE PAYLOAD IS THE WHOLE POINT. In the shape that lost work, A's
     * save was split against a baseline B had replaced, so A sent the
     * heading as part of the body. Each window here measures against its
     * own record, so A sends its own body and B sends its own.
     */
    assert.strictEqual(fromA.src, 'no heading at all\n', 'A sent bytes measured against another window');
    assert.strictEqual(fromB.src, 'body\n', 'B sent its heading as part of the body');

    const sessionsDir = path.join(storage, 'sessions');
    const blockFiles = (session: string): string[] => {
      const out: string[] = [];
      const walk = (dir: string): void => {
        for (const name of fs.readdirSync(dir)) {
          const full = path.join(dir, name);
          if (fs.statSync(full).isDirectory()) {
            walk(full);
          } else if (name.endsWith('.md')) {
            out.push(path.relative(path.join(sessionsDir, session), full));
          }
        }
      };
      walk(path.join(sessionsDir, session));
      return out;
    };
    const mine = blockFiles('S-a');
    const theirs = blockFiles('S-b');
    assert.ok(mine.length > 0 && theirs.length > 0, 'one of the windows wrote no block file');
    /*
     * AND THE TWO NEVER TOUCHED ONE PATH -- which is why the sequence has
     * nowhere to happen rather than being guarded against.
     */
    const absolute = (session: string, rel: string): string => path.join(sessionsDir, session, rel);
    for (const rel of mine) {
      assert.ok(
        !theirs.map((t) => absolute('S-b', t)).includes(absolute('S-a', rel)),
        'the two windows wrote the same path'
      );
    }
  });
});

describe('C16 the sessions are separate past their names', function () {
  this.timeout(120000);

  /*
   * THE IMPLEMENTATION C16 NAMES: one shared queue with a global
   * numbering. It passes C1 -- the block FILES are in separate
   * directories -- while both windows send each other's saves.
   *
   * NOTE: THE FIRST VERSION OF THIS CELL ENQUEUED NOTHING and asserted
   * `queues.length === 0 || queues.length >= 1`, which is true of every
   * number. Each window now puts a request in its own queue and the
   * other window's count is required not to move.
   */
  it('gives each session its own queue, and neither sees the other’s entries', async () => {
    const storage = scratch();
    await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-a', stores: ['/stores/one'] } },
        { enqueue: { sessionId: 'S-a', req: 'aaaaaaaa-0000-0000-0000-000000000001', payload: 'from A\n' } },
        { countQueue: 'S-a' }
      ]
    });
    const b = await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-b', stores: ['/stores/one'] } },
        { enqueue: { sessionId: 'S-b', req: 'bbbbbbbb-0000-0000-0000-000000000002', payload: 'from B\n' } },
        { countQueue: 'S-b' },
        { countQueue: 'S-a' }
      ]
    });

    const counts = b.steps.filter((s) => s.step === 'countQueue') as Array<{ sessionId: string; count: number }>;
    const mine = counts.find((c) => c.sessionId === 'S-b');
    const theirs = counts.find((c) => c.sessionId === 'S-a');
    assert.strictEqual(mine?.count, 1, `B's own queue holds ${mine?.count}: ${JSON.stringify(b.steps)}`);
    assert.strictEqual(
      theirs?.count,
      1,
      'B changed the other window’s queue, so the two share one'
    );
    assert.ok(
      !fs.existsSync(path.join(storage, 'outbox.json')),
      'there is a queue outside the sessions, so the two windows share one'
    );
  });

  /*
   * AND STARTING UP CONSUMES NOTHING. C13 asks for this and the first
   * version of the cell only checked that the old directory still
   * existed -- which it did before startup too.
   */
  it('does not consume another session’s entries just by starting up', async () => {
    const storage = scratch();
    await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-old', stores: ['/stores/one'] } },
        { enqueue: { sessionId: 'S-old', req: 'cccccccc-0000-0000-0000-000000000003', payload: 'unsent\n' } }
      ]
    });
    const fresh = await runHost({
      storage,
      steps: [
        { beginSession: { sessionId: 'S-new', stores: ['/stores/one'] } },
        { drafts: 'S-new' },
        { countQueue: 'S-old' }
      ]
    });
    const left = fresh.steps.find((s) => s.step === 'countQueue') as { count: number } | undefined;
    assert.strictEqual(
      left?.count,
      1,
      `starting a window changed the dead session's queue: ${JSON.stringify(fresh.steps)}`
    );
  });
});

/*
 * C8 contention, across two processes.
 *
 * NOTE: THE FIRST VERSION RAN BOTH CLAIMS IN ONE PROCESS, where the scan
 * and the link are one synchronous stretch and never overlap -- so a
 * read-then-write publication with a window between them would have
 * passed. These two hosts wait at a rendezvous until both are ready and
 * then race.
 */
describe('C8 two windows racing for one claim', function () {
  this.timeout(120000);

  it('lets exactly one of two processes take the token', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'S-old'), { recursive: true });
    fs.writeFileSync(
      path.join(storage, 'sessions', 'S-old', 'session.json'),
      JSON.stringify({ sessionId: 'S-old', pid: 999999, startedAt: 1000, nonce: 'n', stores: [] }),
      'utf8'
    );

    const [a, b] = await Promise.all([
      runHost({ storage, steps: [{ claim: { dead: 'S-old', as: 'S-a', waitFor: 'gate' } }] }),
      runHost({ storage, steps: [{ claim: { dead: 'S-old', as: 'S-b', waitFor: 'gate' } }] })
    ]);
    const outcomeOf = (r: typeof a): { claimed?: boolean } =>
      (r.steps.find((s) => s.step === 'claim')?.outcome ?? {}) as { claimed?: boolean };
    const won = [outcomeOf(a), outcomeOf(b)].filter((o) => o.claimed === true).length;
    assert.strictEqual(
      won,
      1,
      `${won} of two processes took the same claim: ${JSON.stringify([a.steps, b.steps])}`
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
 *
 * NOTE: THESE THREE WERE ONCE DELETED BY A REWRITE OF THIS FILE and the
 * suite stayed green -- a shorter file looks tidier and the missing
 * guard does not announce itself. Counting the cells is what found it.
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
            sessionId: 'S-a',
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

  async function standingAfter(storage: string): Promise<{ kind?: string; complete?: boolean }> {
    const after = await runHost({
      storage,
      steps: [{ standingOf: { sessionId: 'S-a', blockId: 'a.2', name: 'current.md' } }]
    });
    return (after.steps.find((s) => s.step === 'standingOf')?.standing ?? {}) as {
      kind?: string;
      complete?: boolean;
    };
  }

  it('dies after the record is written and leaves a publication that can be repeated', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 1);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const standing = await standingAfter(storage);
    assert.strictEqual(standing.kind, 'mid-publication');
    assert.strictEqual(standing.complete, false, 'a write that never happened was taken for a finished one');
  });

  it('dies after the file is written and leaves a publication that only needs finishing', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 2);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const standing = await standingAfter(storage);
    assert.strictEqual(standing.kind, 'mid-publication');
    assert.strictEqual(standing.complete, true, 'a finished write was taken for one that never happened');
  });

  it('leaves nothing at all when it dies before the record is written', async () => {
    const storage = scratch();
    const result = await publishAndDie(storage, 0);
    assert.strictEqual(result.code, 9, `the host did not die inside the publication: ${JSON.stringify(result.steps)}`);
    const standing = await standingAfter(storage);
    assert.strictEqual(standing.kind, 'absent');
  });
});
