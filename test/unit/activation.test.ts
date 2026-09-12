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
 * What activation must leave behind.
 *
 * ⚠️ NONE OF THIS WAS CHECKED, AND IT WAS ALL MISSING. The extension
 * created a `Sessions` and a session id and never called `begin`, so no
 * `session.json` was ever written by the real thing -- every takeover,
 * every discard, every "other sessions" listing was blind to the
 * directories it made. The two-process cells passed because the harness
 * called `begin` on its own behalf.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { activateCore } from '../../src/activate';
import { nodeFileOps } from '../../src/fsops';
import { Sessions } from '../../src/sessions';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-activate-'));
}

function core(globalStorage: string, sessionId = 'S-mine', stores = ['/stores/one']) {
  return activateCore({
    files: nodeFileOps,
    globalStorage,
    documents: { isOpen: () => false },
    stores,
    sessionId
  });
}

describe('activation leaves a session another window can see', () => {
  it('writes session.json naming this process', () => {
    const storage = scratch();
    core(storage);
    const file = path.join(storage, 'sessions', 'S-mine', 'session.json');
    assert.ok(fs.existsSync(file), 'activation left no record of this session');
    const written = JSON.parse(fs.readFileSync(file, 'utf8')) as { pid: number; startedAt: number | null };
    assert.strictEqual(written.pid, process.pid, 'the record names some other process');
    assert.ok(
      typeof written.startedAt === 'number',
      'the record carries no start time, so a reused pid could never be told from this session'
    );
  });

  /*
   * AND ANOTHER WINDOW CAN SEE IT. That is the whole purpose: a session
   * nobody else can enumerate cannot be taken over or discarded, and its
   * unsent work is unreachable.
   */
  it('is listed by a second window looking at the same storage', async () => {
    const storage = scratch();
    core(storage, 'S-first');
    const second = new Sessions(nodeFileOps, storage);
    second.begin('S-second', ['/stores/one']);
    const others = await second.others();
    assert.deepStrictEqual(
      others.map((o) => o.identity.sessionId),
      ['S-first'],
      'a second window could not see the first, so nothing can ever be recovered from it'
    );
  });

  /*
   * ⚠️ THE QUEUES ONLY. The block directories are named by
   * `Sessions.directoryFor`, which this does not call; the cell's old
   * name promised both.
   */
  it('gives each store its own queue', () => {
    const storage = scratch();
    const made = core(storage, 'S-mine', ['/stores/one', '/stores/two']);
    const one = made.outboxPath('/stores/one');
    const two = made.outboxPath('/stores/two');
    assert.notStrictEqual(one, two, 'two stores share one queue, so their cursors cross');
    assert.ok(one.includes(path.join('sessions', 'S-mine')), 'the queue is not inside the session');
  });

  /*
   * THE QUEUE PATH HAS ONE SUPPLIER. Recovery reads it and saving writes
   * it; when the writer moved and the readers did not, a real queue
   * became invisible to the listing -- pending counted zero and an
   * import carried nothing.
   */
  it('hands out the same queue path the session listing reads', () => {
    const storage = scratch();
    const made = core(storage);
    assert.strictEqual(
      made.outboxPath('/stores/one'),
      made.sessions.outboxPathFor('S-mine', made.storeHash('/stores/one')),
      'the wiring composes a queue path of its own instead of asking for one'
    );
  });
});
