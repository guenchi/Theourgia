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
 * WHICH STORE AN ANSWER BELONGS TO, WHEN THE WINDOW HAS MOVED ON.
 *
 * A settings change replaces the queue and the store this window writes
 * to. An answer already in flight belongs to neither of the new ones,
 * and the settler used to read both live: one store's cursor was
 * committed into another store's `outbox.json`, and the request that had
 * actually been answered stayed queued where nothing would look at it
 * again.
 *
 * ⚠️ AND THE SECOND HALF OF THAT DEFECT HAD NO CELL UNTIL THIS FILE.
 * `recovered` -- the path that works out which file an answer is about
 * when this process has no memory of sending it, which is every retry
 * after a restart -- looked the block up under the LIVE store. The
 * editor-hosted suite cannot stage a restart inside one window, so the
 * mutation that reads the live store there survived, measured, with the
 * repair held up by nothing but the reasoning that produced it. The
 * decision moved into `src/settling.ts` so that a cell could drive it,
 * and "a restart" here is what it actually is on disk: a queue file that
 * was there before this process started, holding a request nothing in
 * memory knows about.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Outbox } from '../../src/outbox';
import { Publisher } from '../../src/publication';
import { Saving } from '../../src/saving';
import { Sessions } from '../../src/sessions';
import { SaveContext, settlerFor } from '../../src/settling';
import { Notice } from '../../src/status';
import { nodeFileOps } from '../../src/fsops';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-settling-'));
}

const A = 'store-a-hash';
const B = 'store-b-hash';

interface Rig {
  storage: string;
  sessions: Sessions;
  publisher: Publisher;
  saving: Saving;
  queueOf(store: string): Outbox;
  said: Notice[];
}

function rig(): Rig {
  const storage = scratch();
  const sessions = new Sessions(nodeFileOps, storage);
  sessions.begin('S-mine', []);
  const said: Notice[] = [];
  return {
    storage,
    sessions,
    /*
     * NOTHING IS OPEN IN THIS WINDOW: the settler never asks, and a
     * stand-in that answered otherwise would be describing a state this
     * cell is not about.
     */
    publisher: new Publisher(nodeFileOps, { isOpen: () => false }),
    saving: new Saving(nodeFileOps),
    queueOf(store: string): Outbox {
      const queue = new Outbox(sessions.outboxPathFor('S-mine', store), nodeFileOps);
      queue.load();
      return queue;
    },
    said
  };
}

/*
 * A REQUEST THAT WAS ON DISK BEFORE THIS PROCESS EXISTED. `pendingSaves`
 * is empty for it -- that is what makes it the `recovered` path -- and
 * the block's newest version holds exactly the bytes it sent, which is
 * what makes that path able to answer at all.
 */
async function queuedBeforeWeStarted(
  r: Rig,
  store: string,
  blockId: string,
  body: string
): Promise<{ req: string; file: string }> {
  const directory = r.sessions.directoryFor('S-mine', store, blockId);
  /*
   * ⚠️ PUBLISHED BY THE PRODUCT, NOT WRITTEN BY HAND. A version is a
   * file AND the record beside it, and `recognise` -- the thing under
   * test here -- answers "cannot say" when the record is missing. A
   * fixture that wrote the markdown itself would therefore measure the
   * absence of its own sidecar rather than anything about stores, and it
   * would be inventing the record's shape for the second time in one
   * batch.
   */
  const outcome = await r.publisher.publish({
    directory,
    storeId: store,
    blockId,
    prefix: '## Two\n',
    /*
     * THE WHOLE FILE, PREFIX INCLUDED. `publish` writes `text` verbatim
     * and records `prefix` beside it; the split that `recognise` makes
     * is the file's text minus that prefix. Passing the body alone
     * produced a version whose bytes do not start with its own recorded
     * prefix -- a state nothing in the product creates -- and
     * `recognise` answered "cannot say", which is exactly the answer a
     * fixture should never be manufacturing by accident.
     */
    text: `## Two\n${body}`
  });
  assert.ok(outcome.published, `the fixture could not publish a version: ${JSON.stringify(outcome)}`);
  const file = (outcome as { file: string }).file;
  const queue = r.queueOf(store);
  const req = 'req-from-before';
  queue.enqueue({
    req,
    cursor: 'w:1',
    id: blockId,
    field: 'src',
    payload: body,
    state: 'sent',
    createdAt: 0,
    lastError: null,
    importedBy: null
  });
  return { req, file };
}

describe('U-settle an answer is settled against the queue and store it was sent for', () => {
  /*
   * ⚠️ THE CELL THE EDITOR SUITE COULD NOT WRITE. The window has moved
   * to store B; the answer is for a request store A's queue has been
   * holding since before this process started. Everything the settler
   * touches has to be A's.
   */
  it('recovers the block from the store the request was sent for, not the one configured now', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const queueA = r.queueOf(A);
    const pendingSaves = new Map<string, SaveContext>();

    const settle = settlerFor({
      queue: queueA,
      storeHash: A,
      sessionId: 'S-mine',
      pendingSaves,
      sessions: r.sessions,
      publisher: r.publisher,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, 'w:2');

    /*
     * THE RECORD WAS WRITTEN BESIDE STORE A'S FILE. That is what the
     * whole path exists to do, and reading the file rather than a return
     * value is the point: the defect this replaces wrote to the wrong
     * place and reported nothing.
     */
    /*
     * ⚠️ WHAT THE RECORD SAYS, ASKED OF THE PRODUCT. The sidecar has no
     * field naming a request -- what a settled answer leaves behind is
     * the cursor it established and the bytes the store acknowledged --
     * so a cell that searched the file for the request id was looking
     * for something that is never written, and would have stayed red
     * however right the code was. Reading it through `sidecarOf` also
     * keeps this cell from being a second opinion about the record's
     * shape, which is the mistake this file's own fixture made an hour
     * earlier.
     */
    const record = r.publisher.sidecarOf(file);
    assert.ok(record !== null, `nothing was published beside ${file}`);
    assert.strictEqual(
      record?.cursor,
      'w:2',
      'the record beside store A’s file did not take the cursor the answer established'
    );
    assert.ok(
      record?.acknowledgedRaw !== null,
      'the record does not say the store acknowledged these bytes'
    );

    const after = r.queueOf(A);
    assert.strictEqual(after.find(req), undefined, 'store A’s request was not settled');
    assert.strictEqual(after.cursor, 'w:2', 'store A’s queue did not take its own cursor');

    /*
     * AND STORE B HAS NOTHING. Not an empty queue with a cursor in it --
     * nothing at all: no directory, no file, no block.
     */
    const bQueue = r.sessions.outboxPathFor('S-mine', B);
    assert.ok(!fs.existsSync(bQueue), `store B has a queue file at ${bQueue} and never had one`);
    const bBlocks = r.sessions.directoryFor('S-mine', B, 'a.2');
    assert.ok(!fs.existsSync(bBlocks), `store B has a directory for a block it has never seen`);
  });

  /*
   * ⚠️ THE TWIN: the same settler, told it belongs to store B, must NOT
   * find store A's block. Without it, a build that ignores the store
   * hash entirely -- looking wherever the block happens to be -- passes
   * the cell above, and that is exactly the build under repair.
   */
  it('does not find a block filed under another store', async () => {
    const r = rig();
    const { req } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    /*
     * The queue is still A's -- the request has to be findable, or this
     * would be measuring an empty queue -- and only the store is wrong,
     * which is the one thing under test.
     */
    const queueA = r.queueOf(A);
    const settle = settlerFor({
      queue: queueA,
      storeHash: B,
      sessionId: 'S-mine',
      pendingSaves: new Map<string, SaveContext>(),
      sessions: r.sessions,
      publisher: r.publisher,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, 'w:2');

    /*
     * ⚠️ THE SIDECAR IS ALREADY THERE -- the fixture published a version
     * through the product, and a version is a file and its record. So
     * what separates "recorded this answer" from "did not" is whether
     * the record NAMES THE REQUEST, not whether the file exists. The
     * first version of this assertion looked for the file and was green
     * for a reason that had nothing to do with stores.
     */
    /*
     * ⚠️ THE SIDECAR IS ALREADY THERE -- the fixture published a version
     * through the product, and a version is a file and its record. So
     * what separates "recorded this answer" from "did not" is the
     * CURSOR, which a settlement moves and nothing else does.
     */
    const file = path.join(r.sessions.directoryFor('S-mine', A, 'a.2'), '1.md');
    const record = r.publisher.sidecarOf(file);
    assert.ok(record !== null);
    assert.notStrictEqual(
      record?.cursor,
      'w:2',
      'a settler filed under store B recorded its answer beside store A’s file'
    );
    /*
     * AND THE ENTRY IS RELEASED ANYWAY, because the store HAS answered
     * it: this is the documented "nothing says which version this is
     * about" outcome, and it is what makes the cell above discriminating
     * rather than a check that anything at all happened.
     */
    const after = r.queueOf(A);
    assert.strictEqual(after.find(req), undefined, 'the answered request was not released');
  });
});
