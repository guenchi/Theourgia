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
import { Client } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { Saver } from '../../src/saver';
import { CliTransport } from '../../src/transport';
import { Publisher, Sidecar, cleanliness, digestOfBytes, writeSidecar } from '../../src/publication';
import { Saving } from '../../src/saving';
import { Sessions } from '../../src/sessions';
import { settlerFor } from '../../src/settling';
import { recordFor } from '../../src/record';
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
 * A REQUEST THAT WAS ON DISK BEFORE THIS PROCESS EXISTED, carrying the
 * record it was accepted with.
 *
 * ⚠️ THAT IS THE WHOLE OF WHAT MAKES IT SETTLEABLE. There used to be
 * two ways to find out what an answer was about -- a map in memory
 * while the window was up, and a reconstruction from the block's newest
 * version after a restart -- and they disagreed. The entry is the
 * record now, so an answer arriving after a restart takes the same road
 * as one arriving while the window is up.
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
    text: `## Two\n${body}`,
    cursor: null
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
    importedBy: null,
    record: recordFor({
      req,
      store,
      storeHash: store,
      blockId,
      file,
      projectionId:r.publisher.sidecarOf(file)?.projection?.id,
      rawDigest: digestOfBytes(`## Two\n${body}`),
      sentDigest: digestOfBytes(body),
      prefixDigest: digestOfBytes('## Two\n'),
      seq: 1,
      intent: { verb: 'set', field: 'src', expectation: null, body }
    })
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

    const settle = settlerFor({
      queue: queueA,
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, { verdict: 'confirmed', cursor: 'w:2' });

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
   * ⚠️ A REFUSED SAVE IS STILL A DRAFT, AND THAT IS THE OBSERVABLE HALF
   * OF "RECORDS NOTHING".
   *
   * The settler used to turn every answer into an acknowledgement: an
   * empty cursor, `mismatch: false`, `recordAnswer`. A save the store
   * had REJECTED came out of that as settled -- `draft: false`, nothing
   * pending, `unresolved: false` -- so the user's rejected text stopped
   * being reported as work the store has not got, anywhere. Reproduced
   * end to end in review.
   *
   * The repair records nothing, and the fact worth asserting is not a
   * field of the sidecar but what the product says about the block
   * afterwards: it is a draft. That is what a scan for unsent work
   * looks at, and it is what the user sees.
   */
  it('leaves a refused save as a draft, with the request released', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    /*
     * ⚠️ THE FILE HAS BEEN EDITED SINCE IT WAS PUBLISHED, because that
     * is what a save IS. The first version of this cell settled a
     * refusal over an untouched file and asked whether it was a draft:
     * it was not, and correctly so -- nothing had changed. The premise
     * had to be the one the product is about.
     */
    fs.writeFileSync(file, '## Two\nbody the store refused\n', 'utf8');
    const before = r.publisher.standingOf(file);
    assert.strictEqual(
      (before as { draft?: boolean }).draft,
      true,
      `the edited file is not a draft even before the answer: ${JSON.stringify(before)}`
    );

    settle(req, { verdict: 'refused' });

    const standing = r.publisher.standingOf(file);
    assert.strictEqual(
      (standing as { draft?: boolean }).draft,
      true,
      `a refused save is not reported as a draft: ${JSON.stringify(standing)}`
    );
    const record = r.publisher.sidecarOf(file);
    assert.strictEqual(record?.cursor, null, 'a refusal moved the cursor');
    assert.strictEqual(record?.acknowledgedRaw, null, 'a refusal was recorded as acknowledged');
    assert.strictEqual(record?.unresolved, false, 'a plain refusal was marked for a person');

    /*
     * AND THE REQUEST IS RELEASED: the store has answered it, so leaving
     * it queued would make the count of saves still waiting say
     * something untrue -- and would stop the queue with nothing in the
     * extension able to clear it.
     */
    assert.strictEqual(r.queueOf(A).find(req), undefined, 'the answered request is still queued');
  });

  /*
   * ⚠️ AND THE ONE REFUSAL THAT IS KEPT. `req-mismatch` says the store
   * holds a different request under this id: no retry settles that, so
   * the entry stays for a person and the record is marked. Without this
   * row, a build that released everything passes the cell above.
   */
  it('keeps a req-mismatch for a person, and marks the record', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, { verdict: 'req-mismatch' });

    const record = r.publisher.sidecarOf(file);
    assert.strictEqual(record?.unresolved, true, 'the record was not marked for a person');
    assert.strictEqual(record?.cursor, null, 'a refusal moved the cursor');
    assert.ok(
      r.queueOf(A).find(req) !== undefined,
      'the request nobody can retry was released anyway'
    );
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
    const bFile = path.join(r.sessions.directoryFor('S-mine', B, 'a.2'), 'current.md');
    const bRecord = r.publisher.sidecarOf(bFile);
    assert.ok(bRecord !== null, 'the fixture did not publish store B’s version');

    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, { verdict: 'confirmed', cursor: 'w:2' });

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
     * ⚠️ AND STORE B'S OWN SEND IS UNTOUCHED, WITH ITS RECORD.
     *
     * This assertion used to be about a map in memory: store B's
     * context had to survive store A's answer, because deleting it left
     * B's request with nothing able to settle it. The map is gone and
     * the same question has a better place to be asked -- B's entry is
     * on disk and carries the record it was accepted with, so a build
     * that spent A's answer on B's send would show up here as a missing
     * entry or a record that is not B's.
     */
    const bQueue = r.queueOf(B);
    const theirEntry = bQueue.find(theirs.req);
    assert.ok(theirEntry !== undefined, 'store B’s request was settled by store A’s answer');
    assert.strictEqual(
      theirEntry.record?.file,
      theirs.file,
      'store B’s entry no longer carries the record of what it had sent'
    );
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

    const bFile = path.join(r.sessions.directoryFor('S-mine', B, 'a.2'), 'current.md');
    /*
     * STORE B'S CONTEXT, AND ITS DIGEST IS THE RIGHT ONE FOR STORE A'S
     * REQUEST TOO -- that is the whole point: the bytes are identical,
     * so the digest cannot tell these two sends apart.
     */

    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, { verdict: 'confirmed', cursor: 'w:2' });

    const after = r.publisher.sidecarOf(bFile);
    assert.strictEqual(after?.cursor, null, 'store A’s answer put its cursor on store B’s file');
    assert.strictEqual(
      after?.acknowledgedRaw,
      null,
      'store A’s answer acknowledged bytes store B sent'
    );
    /*
     * AND STORE B'S SEND IS STILL WAITING, WITH ITS OWN RECORD. The
     * bytes are identical, so nothing about what was sent separates
     * these two; what does is the record each entry carries.
     */
    const bQueue = r.queueOf(B);
    const theirEntry = bQueue.find('req-from-before');
    assert.ok(theirEntry !== undefined, 'store B’s request was settled by store A’s answer');
    assert.strictEqual(
      theirEntry.record?.store,
      B,
      'store B’s entry no longer carries the record of what it had sent'
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
      importedBy: null,
      record: recordFor({
        req: 'req-second',
        store: A,
        storeHash: A,
        blockId: 'a.2',
        file: first.file,
        projectionId:r.publisher.sidecarOf(first.file)?.projection?.id,
        rawDigest: digestOfBytes('## Two\nsecond body\n'),
        sentDigest: digestOfBytes('second body\n'),
        prefixDigest: digestOfBytes('## Two\n'),
        seq: 2,
        intent: { verb: 'set', field: 'src', expectation: null, body: 'second body\n' }
      })
    });

    const settle = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(first.req, { verdict: 'confirmed', cursor: 'w:2' });

    /*
     * ⚠️ THE LATER SEND STILL HAS ITS OWN RECORD. This used to be a
     * question about a map keyed by block id, where the second save
     * overwrote the first; now each send carries its own, and the
     * assertion is that the earlier answer did not take the later
     * send's.
     */
    const second = r.queueOf(A).find('req-second');
    assert.ok(second !== undefined, 'the unanswered second send was removed by the first answer');
    assert.strictEqual(
      second.record?.sentDigest,
      digestOfBytes('second body\n'),
      'the later send’s record is no longer its own'
    );
    const after = r.queueOf(A);
    assert.strictEqual(after.find(first.req), undefined, 'the answered request was not settled');
    assert.ok(
      after.find('req-second') !== undefined,
      'the unanswered second send was removed by the first answer'
    );
  });

  /*
   * ⚠️ AND THE SAVER IT IS HANDED TO MUST BE OVER THE SAME QUEUE OBJECT.
   *
   * The pairing checked in `settlerFor` is settler-to-store. `Saver`
   * takes its outbox and its settlement callback as two arguments, so a
   * correctly built settler for one store could still be attached to a
   * Saver over another -- and a review reproduced what follows: with the
   * same request id in both queues, one store's answer removed the
   * other's entry and wrote its cursor beside the other's file.
   *
   * It is object identity rather than path equality: two Outbox objects
   * over one file are two copies of it, and writing one back erases what
   * the other holds.
   */
  it('cannot be handed to a saver over a different queue', async () => {
    const r = rig();
    await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    await queuedBeforeWeStarted(r, B, 'a.2', 'body\n');
    const settlerForA = settlerFor({
      queue: r.queueOf(A),
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
    const client = new Client(new CliTransport({ scheme: 'scheme', corePath: 'nowhere', libDirs: [], store: '/tmp/s', actor: 'x', timeoutMs: 1000, transport: 'cli' }, {}));

    assert.throws(
      () => new Saver(client, r.queueOf(B), settlerForA),
      /was given a settler built for/,
      'a saver took a settler belonging to another queue'
    );

    /*
     * AND A SETTLER OVER THE SAVER'S OWN QUEUE IS ACCEPTED -- the
     * control that stops "refuse everything" from passing the row above,
     * which would mean no saves at all.
     */
    const queue = r.queueOf(A);
    const settlerOverThatQueue = settlerFor({
      queue,
      storeHash: A,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
    assert.doesNotThrow(() => new Saver(client, queue, settlerOverThatQueue));
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
              sessions: r.sessions,
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
          sessions: r.sessions,
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
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });

    settle(req, { verdict: 'confirmed', cursor: 'w:2' });

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
    const file = path.join(r.sessions.directoryFor('S-mine', A, 'a.2'), 'current.md');
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

/*
 * WHAT EACH VERDICT LEAVES BEHIND. (R4, R5; §13.2, §13.3)
 *
 * The record beside a file keeps two things apart: WHICH SEND the store
 * confirmed, and what the file now holds. Every verdict has to say
 * something about the first -- ⚠️ "every verdict writes the record or
 * none of them does" is not a choice: a refusal that wrote nothing
 * would leave its number in `outstanding` for ever, and the block would
 * be a draft it can never stop being.
 */
describe('R5 each verdict leaves its own mark on the record', () => {
  function settlerOver(r: Rig, store: string) {
    return settlerFor({
      queue: r.queueOf(store),
      storeHash: store,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
  }

  it('records which send the store confirmed, and takes its number out', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    /*
     * THE SEND IS RECORDED AS OUT BEFORE IT IS ANSWERED, which is what
     * the acceptance does on the real path. A fixture that skipped it
     * would be asking whether the number is removed from a set it was
     * never in.
     */
    const numbered = r.publisher.takeSequence(file, req);
    assert.ok(numbered.taken, 'the fixture could not take a sequence number');

    settlerOver(r, A)(req, { verdict: 'confirmed', cursor: 'w:2' });

    const after = r.publisher.sidecarOf(file);
    assert.strictEqual(after?.confirmed?.by, 'store', 'the record does not say the store confirmed a send');
    assert.strictEqual((after?.confirmed as { req: string }).req, req);
    assert.strictEqual((after?.confirmed as { cursor: string }).cursor, 'w:2');
    assert.strictEqual(after?.highWater, 1, 'the high-water mark did not move to the send that was confirmed');
    assert.deepStrictEqual(
      after?.outstanding,
      [],
      'the confirmed send is still counted as out, so the block stays a draft for ever'
    );
  });

  /*
   * ⚠️ A REFUSAL WRITES ONE THING AND ONLY ONE THING. The store said
   * no: nothing about the file changed, so no baseline is written --
   * and the number has to come out, because the send is over. A build
   * that wrote nothing at all would leave the block permanently
   * unsettled, which is the same defect the other way round.
   */
  it('takes only the number out when the store refuses', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const numbered = r.publisher.takeSequence(file, req);
    assert.ok(numbered.taken);
    const before = r.publisher.sidecarOf(file);

    settlerOver(r, A)(req, { verdict: 'refused' });

    const after = r.publisher.sidecarOf(file);
    assert.deepStrictEqual(after?.outstanding, [], 'the refused send is still counted as out');
    assert.deepStrictEqual(
      after?.confirmed,
      before?.confirmed,
      'a refusal wrote a baseline; the store did not take these bytes'
    );
    assert.strictEqual(after?.highWater, before?.highWater, 'a refusal moved the high-water mark');
    assert.strictEqual(r.queueOf(A).find(req), undefined, 'the refused request is still queued');
  });

  /*
   * ⚠️ AN OPERATOR'S DETERMINATION IS RECORDED WITHOUT A POSITION, and
   * the queue's own position is dropped. The core says the
   * determination does not recover the original execution's event, so
   * there is no place to record -- and the number this queue was
   * holding can no longer be vouched for either.
   */
  it('records an operator’s determination, with no position anywhere', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const numbered = r.publisher.takeSequence(file, req);
    assert.ok(numbered.taken);
    const queue = r.queueOf(A);
    queue.setCursor('w:9');

    settlerOver(r, A)(req, { verdict: 'executed-by-operator', note: 'an operator said so' });

    const after = r.publisher.sidecarOf(file);
    assert.strictEqual(after?.confirmed?.by, 'operator');
    assert.strictEqual(
      (after?.confirmed as { cursor: string | null }).cursor,
      null,
      'a position was recorded for a determination that does not recover one'
    );
    assert.deepStrictEqual(after?.outstanding, [], 'the send is still counted as out');
    const reloaded = r.queueOf(A);
    assert.strictEqual(
      reloaded.cursor,
      null,
      'the queue kept a position nobody can vouch for, so the next send composes against it'
    );
  });

  /*
   * ⚠️ AND A MISMATCH KEEPS ITS NUMBER. The store is saying it cannot
   * say what happened to this send; removing the number would make the
   * block read as though nothing were in flight, which is the one thing
   * that is certainly untrue.
   */
  it('keeps the number out when the store cannot say what happened', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const numbered = r.publisher.takeSequence(file, req);
    assert.ok(numbered.taken);

    settlerOver(r, A)(req, { verdict: 'req-mismatch' });

    const after = r.publisher.sidecarOf(file);
    assert.deepStrictEqual(
      after?.outstanding.map((o) => o.seq),
      [1],
      'a send nobody can account for stopped being counted as out'
    );
    assert.strictEqual(after?.unresolved, true, 'the record was not marked for a person');
    assert.ok(r.queueOf(A).find(req) !== undefined, 'the entry a person has to look at was released');
  });
});

/*
 * R4 THE NUMBER IS THE FILE'S AXIS, AND THE CURSOR IS THE STORE'S.
 *
 * A cursor belongs to one store's log, so two sends to different stores
 * carry positions that cannot be compared at all. The send number
 * belongs to the file, and two sends of one file always can be. §13
 * moves the ordering guard onto the number for exactly that reason --
 * and these cells are about what that costs when the two disagree.
 */
describe('R4 which send becomes the baseline', () => {
  function settlerOver(r: Rig, store: string) {
    return settlerFor({
      queue: r.queueOf(store),
      storeHash: store,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
  }

  /*
   * ⚠️ AN ANSWER FOR AN OLDER SEND STILL SETTLES. It leaves the queue
   * and its number leaves `outstanding` -- it simply does not become
   * the baseline. A build that ignored it entirely would keep the block
   * a draft over a send the store has answered; a build that let it
   * through would replace a newer baseline with an older one.
   */
  it('lets an older send settle without replacing a newer baseline', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const older = r.publisher.takeSequence(file, req);
    assert.ok(older.taken);
    /*
     * A LATER SEND HAS ALREADY BEEN CONFIRMED. The high-water mark is
     * above the send whose answer is about to arrive, which is the only
     * thing that separates these two.
     */
    const held = r.publisher.sidecarOf(file) as Sidecar;
    writeSidecar(nodeFileOps, file, {
      ...held,
      highWater: 5,
      confirmed: {
        by: 'store',
        req: 'a-later-send',
        seq: 5,
        sentDigest: 'later-sent',
        rawDigest: digestOfBytes('## Two\nbody\n'),
        prefixDigest: 'later-prefix',
        cursor: 'w:50'
      }
    });

    settlerOver(r, A)(req, { verdict: 'confirmed', cursor: 'w:2' });

    const after = r.publisher.sidecarOf(file);
    assert.strictEqual(
      (after?.confirmed as { req: string }).req,
      'a-later-send',
      'an answer for an older send replaced the baseline a newer one established'
    );
    assert.strictEqual(after?.highWater, 5, 'the high-water mark went backwards');
    assert.deepStrictEqual(
      after?.outstanding.map((o) => o.seq),
      [],
      'the older send is still counted as out, so the block stays a draft over an answered send'
    );
    assert.strictEqual(r.queueOf(A).find(req), undefined, 'the answered request is still queued');
  });

  /*
   * ⚠️ AND THE TWIN ALONG THE ONE AXIS UNDER TEST: the same story with
   * the high-water mark BELOW the arriving send must replace. Without
   * it, a build that never wrote a baseline at all passes the cell
   * above.
   */
  it('replaces the baseline when the arriving send is the later one', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const taken = r.publisher.takeSequence(file, req);
    assert.ok(taken.taken);

    settlerOver(r, A)(req, { verdict: 'confirmed', cursor: 'w:2' });

    const after = r.publisher.sidecarOf(file);
    assert.strictEqual((after?.confirmed as { req: string }).req, req);
    assert.strictEqual(after?.highWater, taken.seq);
  });

  /*
   * ⚠️ TWO SENDS WEARING ONE NUMBER IS NOT A THING TO DECIDE QUIETLY.
   *
   * The number is taken from `nextSeq` and written down before anything
   * is sent, so within one window it cannot repeat. It can still arrive
   * repeated: a takeover carries entries that were numbered against a
   * record somebody else has since replaced, and a stop between taking
   * the number and writing it down leaves the next start free to hand
   * it out again.
   *
   * The baseline says which send it holds. If an answer arrives for the
   * same number under a DIFFERENT request, the two disagree about what
   * that number means -- and no rule here can say which is right. §13
   * says so out loud: mark the record for a person and keep the entry.
   * Settling it quietly would make one of the two sends disappear.
   */
  it('keeps a send whose number the baseline already holds for another request', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const taken = r.publisher.takeSequence(file, req);
    assert.ok(taken.taken);
    const held = r.publisher.sidecarOf(file) as Sidecar;
    writeSidecar(nodeFileOps, file, {
      ...held,
      highWater: taken.seq,
      confirmed: {
        by: 'store',
        req: 'a-different-request',
        seq: taken.seq,
        sentDigest: 'somebody-else-sent',
        rawDigest: digestOfBytes('## Two\nbody\n'),
        prefixDigest: 'somebody-else-split',
        cursor: 'w:40'
      }
    });

    settlerOver(r, A)(req, { verdict: 'confirmed', cursor: 'w:2' });

    const after = r.publisher.sidecarOf(file);
    assert.strictEqual(
      after?.unresolved,
      true,
      'two sends claimed one number and the record was not marked for a person'
    );
    assert.ok(
      r.queueOf(A).find(req) !== undefined,
      'the entry a person has to look at was settled quietly'
    );
    assert.deepStrictEqual(
      after?.outstanding.map((o) => o.seq),
      [taken.seq],
      'a send nobody can account for stopped being counted as out'
    );
  });

  /*
   * ⚠️ THE SAME ANSWER TWICE IS NOT TWO SENDS. A replay, a retry after a
   * restart, or a drain that runs while one is already in flight can
   * deliver the same request's answer again; the second one has no
   * entry to find and must change nothing.
   */
  it('takes the same answer twice without counting it twice', async () => {
    const r = rig();
    const { req, file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const taken = r.publisher.takeSequence(file, req);
    assert.ok(taken.taken);
    const settle = settlerOver(r, A);

    settle(req, { verdict: 'confirmed', cursor: 'w:2' });
    const once = r.publisher.sidecarOf(file);
    settle(req, { verdict: 'confirmed', cursor: 'w:2' });
    const twice = r.publisher.sidecarOf(file);

    assert.deepStrictEqual(twice, once, 'settling the same answer again changed the record');
  });
});

/*
 * R14 AN ENTRY FROM BEFORE THE RECORD. (§13.1)
 *
 * A queue written by the older build carries a request and bytes and
 * nothing else: no file, no digests, no number. It is sent and dequeued
 * exactly as before -- ⚠️ and its answer may not write anything beside
 * a file, because the provenance a baseline needs was never recorded.
 * Writing one would mean deciding, after the fact, which version those
 * bytes went from.
 */
describe('R14 an entry written before the record', () => {
  function legacyEntry(r: Rig, store: string, blockId: string, body: string): string {
    const queue = r.queueOf(store);
    const req = 'req-without-a-record';
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
    return req;
  }

  function settlerOver(r: Rig, store: string) {
    return settlerFor({
      queue: r.queueOf(store),
      storeHash: store,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
  }

  it('settles it in the queue and writes nothing beside any file', async () => {
    const r = rig();
    const { file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const before = r.publisher.sidecarOf(file);
    const req = legacyEntry(r, A, 'a.2', 'body\n');

    settlerOver(r, A)(req, { verdict: 'confirmed', cursor: 'w:2' });

    assert.strictEqual(r.queueOf(A).find(req), undefined, 'the answered request is still queued');
    assert.strictEqual(r.queueOf(A).cursor, 'w:2', 'the queue did not take the position the answer established');
    assert.deepStrictEqual(
      r.publisher.sidecarOf(file),
      before,
      'an entry with no record wrote a baseline beside a file it never named'
    );
  });

  /*
   * ⚠️ AND A REFUSAL OF ONE HAS NO NUMBER TO RELEASE. It never took one.
   * A build that reached for `record.seq` here would be reading a field
   * that is not there, and the shape that catches it is this cell.
   */
  it('releases a refused one without reaching for a number it never had', async () => {
    const r = rig();
    const { file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const before = r.publisher.sidecarOf(file);
    const req = legacyEntry(r, A, 'a.2', 'body\n');

    settlerOver(r, A)(req, { verdict: 'refused' });

    assert.strictEqual(r.queueOf(A).find(req), undefined, 'the refused request is still queued');
    assert.deepStrictEqual(r.publisher.sidecarOf(file), before, 'a refusal of a record-less entry wrote something');
  });

  /*
   * ⚠️ AND A MISMATCH KEEPS IT, with nothing marked. There is no file to
   * mark -- the entry does not name one -- and the entry itself is what
   * a person will look at.
   */
  it('keeps a mismatched one for a person, with no file to mark', async () => {
    const r = rig();
    const { file } = await queuedBeforeWeStarted(r, A, 'a.2', 'body\n');
    const before = r.publisher.sidecarOf(file);
    const req = legacyEntry(r, A, 'a.2', 'body\n');

    settlerOver(r, A)(req, { verdict: 'req-mismatch' });

    assert.ok(r.queueOf(A).find(req) !== undefined, 'the entry a person has to look at was released');
    assert.deepStrictEqual(r.publisher.sidecarOf(file), before, 'a file was marked for an entry that names none');
  });
});

/*
 * R6 A WITHDRAWAL FOLLOWED BY A REFUSAL. (§13.3, 第三十八封②)
 *
 * The sequence a review found, and the reason the ordering guard moved
 * onto the send number: publish X, save Y and get no answer, put X back
 * and save again, and then Y's confirmation arrives. The store now
 * holds Y; the file holds X; and the send meant to restore X is
 * refused. A build that read "the file equals something that was once
 * acknowledged" as settled would call that clean -- with the user's
 * visible text and the store's contents different.
 */
describe('R6 what a withdrawal leaves behind', () => {
  function settlerOver(r: Rig, store: string) {
    return settlerFor({
      queue: r.queueOf(store),
      storeHash: store,
      sessionId: 'S-mine',
      sessions: r.sessions,
      saving: r.saving,
      report: (notice) => r.said.push(notice),
      unrecorded: (f, because) => ({ level: 'warning', text: `${f}:${because}` })
    });
  }

  async function publishedX(r: Rig): Promise<string> {
    const directory = r.sessions.directoryFor('S-mine', A, 'a.2');
    const outcome = await r.publisher.publish({
      directory,
      storeId: A,
      blockId: 'a.2',
      prefix: '## Two\n',
      text: '## Two\nX\n',
      cursor: 'w:1'
    });
    assert.ok(outcome.published);
    return (outcome as { file: string }).file;
  }

  function sent(r: Rig, file: string, req: string, body: string): number {
    const queue = r.queueOf(A);
    const numbered = r.publisher.takeSequence(file, req);
    assert.ok(numbered.taken, 'the fixture could not take a sequence number');
    queue.enqueue({
      req,
      cursor: 'w:1',
      id: 'a.2',
      field: 'src',
      payload: body,
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null,
      record: recordFor({
        req,
        store: A,
        storeHash: A,
        blockId: 'a.2',
        file,
        projectionId:r.publisher.sidecarOf(file)?.projection?.id,
        rawDigest: digestOfBytes(`## Two\n${body}`),
        sentDigest: digestOfBytes(body),
        prefixDigest: digestOfBytes('## Two\n'),
        seq: numbered.seq,
        intent: { verb: 'set', field: 'src', expectation: null, body }
      })
    });
    return numbered.seq;
  }

  it('calls the block a draft when the store holds Y and the file holds X', async () => {
    const r = rig();
    const file = await publishedX(r);
    /*
     * Y IS SAVED AND CONFIRMED. The file has to hold Y at that moment,
     * because that is what a save is -- the bytes are on disk before
     * the answer comes back.
     */
    fs.writeFileSync(file, '## Two\nY\n', 'utf8');
    sent(r, file, 'req-Y', 'Y\n');
    settlerOver(r, A)('req-Y', { verdict: 'confirmed', cursor: 'w:2' });

    /*
     * AND THEN THE USER PUTS X BACK AND SAVES, AND THAT SEND IS
     * REFUSED. The store still holds Y.
     */
    fs.writeFileSync(file, '## Two\nX\n', 'utf8');
    sent(r, file, 'req-X-again', 'X\n');
    settlerOver(r, A)('req-X-again', { verdict: 'refused' });

    const sidecar = r.publisher.sidecarOf(file) as Sidecar;
    assert.strictEqual((sidecar.confirmed as { req: string }).req, 'req-Y', 'the refused send became the baseline');
    assert.deepStrictEqual(sidecar.outstanding, [], 'the refused send is still counted as out');
    assert.deepStrictEqual(
      cleanliness(fs.readFileSync(file), sidecar, { unsettled: [] }),
      { clean: false, because: 'bytes-moved' },
      'the file holds X while the store holds Y, and the block was called settled'
    );
  });

  /*
   * ⚠️ THE FALSE EXAMPLE, so that "always a draft" cannot pass. Same
   * shape, one difference: the send that was refused is the one that
   * would have CHANGED the block away from what the store confirmed.
   * The file is back at what the baseline says, so it is clean -- and a
   * build that called every refusal a draft fails here.
   */
  it('calls it clean when the refused send is the one that would have moved it', async () => {
    const r = rig();
    const file = await publishedX(r);
    fs.writeFileSync(file, '## Two\nX\n', 'utf8');
    sent(r, file, 'req-X', 'X\n');
    settlerOver(r, A)('req-X', { verdict: 'confirmed', cursor: 'w:2' });

    fs.writeFileSync(file, '## Two\nZ\n', 'utf8');
    sent(r, file, 'req-Z', 'Z\n');
    settlerOver(r, A)('req-Z', { verdict: 'refused' });
    /*
     * AND THE USER PUTS BACK WHAT THE STORE HAS.
     */
    fs.writeFileSync(file, '## Two\nX\n', 'utf8');

    const sidecar = r.publisher.sidecarOf(file) as Sidecar;
    assert.deepStrictEqual(
      cleanliness(fs.readFileSync(file), sidecar, { unsettled: [] }),
      { clean: true },
      'the file holds exactly what the store confirmed and the block was called a draft'
    );
  });
});
