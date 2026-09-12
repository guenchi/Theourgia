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
import { Publisher, digestOfBytes } from '../../src/publication';
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
   * ⚠️ AND THE CONTEXT IN MEMORY MUST BE ABOUT THIS REQUEST'S BYTES.
   *
   * `pendingSaves` lives for the life of the window, survives every
   * rebuild, and is keyed by BLOCK ID -- while the question an answer
   * asks is "which send was this". The two are the same thing only while
   * one store has one save of that block in flight.
   *
   * The sequence a review reproduced: save `a.2` in store A; change the
   * store; save `a.2` in store B before A answers, which REPLACES the
   * entry under that key; then A's answer arrives. A's settler finds its
   * own request in its own queue, takes store B's context, and writes
   * store A's cursor and acknowledgement beside STORE B's file using
   * store B's digests -- then deletes the context, leaving B's own
   * request with nothing to settle it. Queue and store were already
   * bound correctly; this is the third thing the answer has to own.
   */
  it('ignores a context left by a different save of the same block', async () => {
    const r = rig();
    const { req } = await queuedBeforeWeStarted(r, A, 'a.2', 'body from A\n');
    const theirs = await queuedBeforeWeStarted(r, B, 'a.2', 'body from B\n');

    /*
     * WHAT THE OTHER STORE'S SAVE LEFT UNDER THE SHARED KEY: its file,
     * its digests, its block -- the same block id, which is the whole
     * reason it collides.
     */
    const pendingSaves = new Map<string, SaveContext>();
    const bFile = path.join(r.sessions.directoryFor('S-mine', B, 'a.2'), '1.md');
    const bRecord = r.publisher.sidecarOf(bFile);
    assert.ok(bRecord !== null, 'the fixture did not publish store B’s version');
    pendingSaves.set('a.2', {
      blockId: 'a.2',
      storeHash: B,
      file: bFile,
      rawDigest: 'digest-of-b',
      sentDigest: 'digest-of-b-body'
    });

    const settle = settlerFor({
      queue: r.queueOf(A),
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
     * STORE B'S FILE IS UNTOUCHED: no cursor from another store's
     * answer, no acknowledgement it never received.
     */
    const after = r.publisher.sidecarOf(bFile);
    assert.strictEqual(
      after?.cursor,
      null,
      'store A’s answer put its cursor on store B’s file'
    );
    assert.strictEqual(
      after?.acknowledgedRaw,
      null,
      'store A’s answer acknowledged bytes store B sent'
    );

    /*
     * AND STORE B'S CONTEXT IS STILL THERE, because nothing has answered
     * it. Deleting it is how the defect left B's own request with
     * nothing able to settle it.
     */
    assert.ok(
      pendingSaves.has('a.2'),
      'the other store’s save lost the record of what it had sent'
    );
    assert.ok(theirs.req.length > 0);
  });

  /*
   * ⚠️ THE INPUT THE DIGEST IS BLIND TO: THE SAME BYTES, IN ANOTHER
   * STORE.
   *
   * The first repair compared the bytes a send carried, which separates
   * two sends of one block that differ in their text -- and says nothing
   * at all about two sends that carry the SAME text to different stores.
   * Open the block in store B, save it unchanged, and the digests agree
   * while the contexts do not: store A's answer would take store B's
   * context again, and write A's cursor and acknowledgement beside B's
   * file. A review named this while the digest repair was being frozen.
   *
   * So the store is compared too -- a different mechanism asking a
   * different question -- and this row is the one that only the store
   * comparison can pass.
   */
  it('ignores a context left by the same bytes saved into another store', async () => {
    const r = rig();
    const same = 'identical body\n';
    const { req } = await queuedBeforeWeStarted(r, A, 'a.2', same);
    await queuedBeforeWeStarted(r, B, 'a.2', same);

    const bFile = path.join(r.sessions.directoryFor('S-mine', B, 'a.2'), '1.md');
    const pendingSaves = new Map<string, SaveContext>();
    /*
     * STORE B'S CONTEXT, AND ITS DIGEST IS THE RIGHT ONE FOR STORE A'S
     * REQUEST TOO -- that is the whole point: the bytes are identical,
     * so the digest cannot tell these two sends apart.
     */
    pendingSaves.set('a.2', {
      blockId: 'a.2',
      storeHash: B,
      file: bFile,
      rawDigest: digestOfBytes(Buffer.from(`## Two\n${same}`, 'utf8')),
      sentDigest: digestOfBytes(Buffer.from(same, 'utf8'))
    });

    const settle = settlerFor({
      queue: r.queueOf(A),
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

    const after = r.publisher.sidecarOf(bFile);
    assert.strictEqual(after?.cursor, null, 'store A’s answer put its cursor on store B’s file');
    assert.strictEqual(
      after?.acknowledgedRaw,
      null,
      'store A’s answer acknowledged bytes store B sent'
    );
    assert.ok(
      pendingSaves.has('a.2'),
      'store B’s save lost the record of what it had sent'
    );
  });

  /*
   * ⚠️ THE TWIN THAT RULES OUT THE CHEAPER REPAIR: ONE STORE, ONE BLOCK,
   * TWO SENDS IN FLIGHT.
   *
   * Keying the memory by store and block together would make the cell
   * above pass, and this one would still fail: nothing about the store
   * separates two saves of the same block in one queue. The fact that
   * does is on the entry -- the bytes it sent -- so that is what is
   * compared, and this row is why.
   *
   * The second save's context is the one under the key; when the FIRST
   * answer comes back it must not be spent on it, and the second save
   * must still have it when its own answer arrives.
   */
  it('leaves the later save’s context alone when the earlier answer arrives', async () => {
    const r = rig();
    const first = await queuedBeforeWeStarted(r, A, 'a.2', 'first body\n');
    const queue = r.queueOf(A);
    /*
     * A SECOND SEND OF THE SAME BLOCK, into the same queue, still
     * unanswered -- and the context in memory is ITS one, because it
     * wrote under the shared key last.
     */
    queue.enqueue({
      req: 'req-second',
      cursor: 'w:1',
      id: 'a.2',
      field: 'src',
      payload: 'second body\n',
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    const pendingSaves = new Map<string, SaveContext>();
    pendingSaves.set('a.2', {
      blockId: 'a.2',
      storeHash: A,
      file: first.file,
      rawDigest: 'digest-of-the-second-raw',
      sentDigest: digestOfBytes(Buffer.from('second body\n', 'utf8'))
    });

    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      pendingSaves,
      sessions: r.sessions,
      publisher: r.publisher,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(first.req, 'w:2');

    assert.ok(
      pendingSaves.has('a.2'),
      'the earlier answer spent the later save’s record of what it had sent'
    );
    assert.strictEqual(
      pendingSaves.get('a.2')?.sentDigest,
      digestOfBytes(Buffer.from('second body\n', 'utf8')),
      'the memory under that key is no longer the later save’s'
    );
    const after = r.queueOf(A);
    assert.strictEqual(after.find(first.req), undefined, 'the answered request was not settled');
    assert.ok(
      after.find('req-second') !== undefined,
      'the unanswered second send was removed by the first answer'
    );
  });

  /*
   * ⚠️ AND A QUEUE THAT IS NOT THIS STORE'S IS REFUSED AT CONSTRUCTION.
   *
   * The move that made this module drivable also made it callable with
   * any pair: told store B while holding store A's queue, a settler
   * acknowledges B's matching file and removes A's request -- the very
   * defect the module exists to repair, through its own front door. A
   * review raised it about the move itself, so the pairing is checked
   * where it can be: the queue's path is what the session builds for
   * that store, or this is not that queue.
   */
  it('refuses a queue that is not the one this session keeps for that store', async () => {
    const r = rig();
    await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const queueA = r.queueOf(A);
    assert.throws(
      () =>
        settlerFor({
          queue: queueA,
          storeHash: B,
          sessionId: 'S-mine',
          pendingSaves: new Map<string, SaveContext>(),
          sessions: r.sessions,
          publisher: r.publisher,
          saving: r.saving,
          report: (notice) => r.said.push(notice),
          unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
        }),
      /is not the queue this session keeps for that store/,
      'a settler was built over another store’s queue'
    );
  });

  /*
   * AND THE PAIRING THAT IS RIGHT IS ACCEPTED -- without this the check
   * above passes for a build that refuses everything, which would stop
   * every save in the extension.
   */
  it('accepts the queue this session keeps for that store', async () => {
    const r = rig();
    await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    assert.doesNotThrow(() =>
      settlerFor({
        queue: r.queueOf(A),
        storeHash: A,
        sessionId: 'S-mine',
        pendingSaves: new Map<string, SaveContext>(),
        sessions: r.sessions,
        publisher: r.publisher,
        saving: r.saving,
        report: (notice) => r.said.push(notice),
        unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
      })
    );
  });

  /*
   * ⚠️ THE TWIN FOR THE STORE THE BLOCKS ARE FILED UNDER. The pairing
   * check above forbids the inconsistent combination at construction, so
   * this drives the same question through a queue that IS store B's:
   * a settler for store B must not find store A's block.
   */
  it('does not find a block filed under another store', async () => {
    const r = rig();
    const { req } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    /*
     * Store B's own queue, holding a request for the same block id and
     * the same bytes. Only the store differs, which is the one thing
     * under test.
     */
    const queueB = r.queueOf(B);
    queueB.enqueue({
      req,
      cursor: 'w:1',
      id: 'a.2',
      field: 'src',
      payload: 'body\n',
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    const settle = settlerFor({
      queue: r.queueOf(B),
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
    /*
     * AND THE ENTRY IS RELEASED FROM THE QUEUE THAT HELD IT -- store
     * B's -- because the store HAS answered it. That is the documented
     * "nothing says which version this is about" outcome, and asserting
     * it is what keeps the cell above from passing for a build that
     * simply does nothing.
     */
    const afterB = r.queueOf(B);
    assert.strictEqual(afterB.find(req), undefined, 'the answered request was not released');
  });
});
