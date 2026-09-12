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
 * O3, S7 and S14 of the X1a cell list, against the core itself.
 *
 * THESE ARE THE CELLS THE STAND-IN CANNOT ANSWER. Everything else in
 * this suite is measured against a script this repository wrote, and a
 * script agrees with whatever it was written to agree with. What a real
 * `outline` prints, what a real `set` does to a real log, and which
 * bytes survive a round trip are facts about the core.
 */

import * as assert from 'assert';
import { assertRuledRefusal } from '../support/refusal-shape';

/*
 * SETTLING, FOR CELLS THAT ARE NOT ABOUT THE ORDER.
 *
 * The Saver no longer removes an answered entry itself: whoever builds
 * one has to say what settling means, because the order -- write the
 * record beside the file, THEN remove the request -- has to live in one
 * named place and the Saver is not it. These cells are about sending and
 * retrying rather than about that order, so they settle the plain way;
 * the cells that ARE about it are in answering.test.ts, where settling
 * goes through `Saving.recordAnswer`.
 */
function settling(outbox: Outbox): (req: string, cursor: string | null) => void {
  return (req, cursor) => outbox.resolve(req, cursor);
}
import * as path from 'path';
import { documentFor, readBlock, splitDocument, titleOf } from '../../src/blocks';
import { StoreModel } from '../../src/model';
import { createHash } from 'crypto';
import { nodeFileOps } from '../../src/fsops';
import { Sessions } from '../../src/sessions';
import { Outbox } from '../../src/outbox';
import { parseOutline } from '../../src/outline';
import { Client } from '../../src/client';
import { CliTransport, TransportError } from '../../src/transport';
import { Counting } from '../support/counting';
import { SpawnSpy } from '../support/spawn-spy';
import { Saver } from '../../src/saver';
import { LosesTheAnswer } from '../support/lossy';
import { clause, clauseValue, initWire, readEvent } from '../../src/wire';
import { CorePin, RealStore, checkCorePin, pinCore } from '../support/real-core';

const DOC = '# Doc One\n\nintro\n\n## Two\nbody\n\n## Three  spaced\nb3\n';

/*
 * TWO DIRECT CHILDREN AND A GRANDCHILD. A request count taken over a
 * single child cannot tell "one request for the subtree" from "one
 * request per child", because with one child those are the same number.
 */
const DEEP_DOC =
  '# Doc One\n\nintro\n\n## Two\nbody\n\n### Deeper\ndeep\n\n## Three  spaced\nb3\n';

/*
 * THE TITLE COMES FROM THE BLOCK, not from the outline's rendering of it
 * -- the same rule the model follows, for the same reason.
 */
async function idOfSection(store: RealStore, title: string): Promise<string> {
  const model = new StoreModel(store.client);
  const outline = parseOutline((await store.client.request('outline', [])).text);
  for (const row of outline) {
    const block = await model.blockOf(row.id);
    if (block !== null && titleOf(block) === title) {
      return row.id;
    }
  }
  throw new Error(`no block titled ${title} in this store`);
}

describe('O3 the tree the model builds is the tree the core printed', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    await initWire();
    store = await RealStore.make();
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => {
    /*
     * THE PIN IS CHECKED WHATEVER HAPPENED TO THE CLEANUP. A dispose
     * that throws would otherwise take the provenance check with it, and
     * the readings in this file would go unqualified.
     */
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  it('shows every id the outline holds, and no others', async () => {
    const outline = await store.client.request('outline', []);
    assert.strictEqual(outline.ok, true, outline.stderr);
    const printed = parseOutline(outline.text);
    assert.ok(printed.length >= 4, `the store printed ${printed.length} rows: ${outline.text}`);

    const model = new StoreModel(store.client);
    const seen: string[] = [];

    const walk = async (id: string): Promise<void> => {
      for (const child of (await model.childrenOf(id)).nodes) {
        seen.push(child.id);
        await walk(child.id);
      }
    };
    for (const root of await model.roots()) {
      seen.push(root.id);
      await walk(root.id);
    }
    assert.deepStrictEqual(
      seen.slice().sort(),
      printed.map((r) => r.id).sort(),
      'the tree and the outline disagree about which blocks exist'
    );
  });

  it('gives each block the parent and the title the core gave it', async () => {
    const model = new StoreModel(store.client);
    const roots = await model.roots();
    assert.strictEqual(roots.length, 1, 'one imported file is one top-level block');
    const outline = parseOutline((await store.client.request('outline', [])).text);
    assert.strictEqual(roots[0].id, outline[0].id);

    const documents = (await model.childrenOf(roots[0].id)).nodes;
    assert.deepStrictEqual(
      documents.map((c) => c.title),
      ['Doc One']
    );
    const sections = (await model.childrenOf(documents[0].id)).nodes;
    assert.deepStrictEqual(
      sections.map((c) => c.title),
      ['Two', 'Three  spaced']
    );
  });
});

describe('S7 a save reaches the store and shows up in its log', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    await initWire();
    store = await RealStore.make('vscode-cell');
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => {
    /*
     * THE PIN IS CHECKED WHATEVER HAPPENED TO THE CLEANUP. A dispose
     * that throws would otherwise take the provenance check with it, and
     * the readings in this file would go unqualified.
     */
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  it('sends one set, and the store reads back what was written', async () => {
    const id = await idOfSection(store, 'Two');
    const model = new StoreModel(store.client);
    const before = await model.blockOf(id);
    assert.ok(before !== null);
    const document = documentFor(before as NonNullable<typeof before>, store.store);
    assert.strictEqual(document.headingSrc, '## Two\n', 'an imported block carries its heading');

    const logBefore = await store.client.request('log', [id]);
    const outbox = new Outbox(path.join(store.root, 'outbox.json'));
    outbox.load();
    /*
     * COUNTING WHAT WENT OUT, NOT WHAT WAS RECORDED. A tracked write
     * sent twice appends ONE record -- that is what the request id is
     * for -- so the log cannot tell a client that sent it once from one
     * that sent it twice. Only the wire can.
     */
    /*
     * COUNTED WHERE THE PROCESS IS STARTED. A wrapper round the
     * transport counts calls INTO it; if the transport itself launched
     * the core twice and returned the first answer, the wrapper would
     * see one call and the store would record one event, and the
     * duplicate would be invisible from both ends.
     */
    const counting = new Counting(new CliTransport(store.config));
    const spy = new SpawnSpy();
    const saver = new Saver(new Client(counting), outbox, settling(outbox));

    const split = splitDocument(document, `${document.headingSrc}body2\n`);
    assert.strictEqual(split.ok, true);
    spy.install();
    let outcome;
    try {
      outcome = await saver.save(id, 'src', (split as { src: string }).src);
    } finally {
      spy.remove();
    }
    assert.strictEqual(outcome.status, 'saved', outcome.message);
    assert.strictEqual(outbox.pendingCount, 0, 'a confirmed save left an entry behind');

    /*
     * THE EXACT CURSOR, not merely a cursor. The bootstrap already put
     * one there, so "not null" is satisfied by a save that resolved
     * nothing; what has to be true is that it names the record this
     * write appended.
     */
    const written = await store.client.request('log', [id]);
    const last = written.answers[written.answers.length - 1];
    /*
     * `(event "w" 3)` is a clause with TWO values in it, not a clause
     * holding a pair -- the write answers use the pair and the log uses
     * this. Taking element one gives the writer and loses the sequence.
     */
    const event = readEvent((clause(last, 'event') as unknown[]).slice(1));
    assert.ok(event !== null, `could not read the event out of ${written.text}`);
    assert.strictEqual(
      outbox.cursor,
      `${event?.writer}:${event?.seq}`,
      'the cursor does not name the record the store just appended'
    );

    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(
      readBack.text,
      document.prefix + 'body2\n',
      'the bytes the core calls this block\'s own are not what the buffer was composed from'
    );
    assert.strictEqual(readBack.text, '## Two\nbody2\n');

    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length + 1,
      'the store did not record exactly one new record'
    );
    assert.strictEqual(
      counting.countOf('set'),
      1,
      `the client sent ${counting.countOf('set')} writes for one save: ${JSON.stringify(counting.sent)}`
    );
    assert.strictEqual(
      spy.carrying('set').length,
      1,
      `one save started ${spy.carrying('set').length} core processes carrying a write`
    );
  });

  it('answers a repeat of the same request with a replay and appends nothing', async () => {
    const id = await idOfSection(store, 'Three  spaced');
    const outbox = new Outbox(path.join(store.root, 'outbox-replay.json'));
    outbox.load();
    const saver = new Saver(store.client, outbox, settling(outbox));

    /*
     * The cursor the first request will carry has to be read before it
     * is sent: the answer moves it, and a repeat sent against the moved
     * cursor is a different request wearing the first one's id.
     */
    const before = await store.client.request('check', []);
    assert.strictEqual(before.ok, true);
    const first = await saver.save(id, 'src', 'changed\n');
    assert.strictEqual(first.status, 'saved', first.message);
    const sentCursor = firstCursorOf(before.text);

    const logBefore = await store.client.request('log', [id]);
    const repeat = await store.client.request('set', [
      id,
      'src',
      'changed\n',
      '--req',
      first.req,
      '--cursor',
      sentCursor
    ]);
    assert.strictEqual(repeat.ok, true, `the store refused the repeat: ${repeat.text}`);
    /*
     * READ, NOT MATCHED. `/replay/` also matches `(replay #f)`, which is
     * what a FRESH write answers -- so the pattern was satisfied by the
     * opposite of what this cell is about.
     */
    assert.strictEqual(
      clauseValue(repeat.answers[0], 'replay'),
      true,
      `the repeat was not answered as a replay: ${repeat.text}`
    );

    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length,
      'the repeat appended a second record'
    );
  });
});

/*
 * Read out of the printed `check` rather than through the client's own
 * reader, so that this cell's expectation does not come from the code it
 * is checking.
 */
function firstCursorOf(checkText: string): string {
  const found = /\("([^"]+)" \(end (\d+)\)/.exec(checkText);
  assert.ok(found !== null, `no writer in ${checkText}`);
  return `${(found as RegExpExecArray)[1]}:${(found as RegExpExecArray)[2]}`;
}

describe('S14 the bytes a store accepts are the bytes this client can read back', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    await initWire();
    store = await RealStore.make();
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => {
    /*
     * THE PIN IS CHECKED WHATEVER HAPPENED TO THE CLEANUP. A dispose
     * that throws would otherwise take the provenance check with it, and
     * the readings in this file would go unqualified.
     */
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * WHAT THIS COST AND WHAT IT BOUGHT. The core takes a form feed in a
   * body, stores it, and prints it back as `\f`; the vendored reader
   * refused that escape, so an ordinary body a user could type made the
   * block one this extension could never open again. The cell was red
   * for as long as that was true -- it was never skipped, and never
   * quietly reclassified -- and went green when goeteia's reader was
   * widened to the escapes a conforming R6RS writer emits (5b45908).
   * It stays as the guard against that gap reopening.
   */
  it('reads back a body holding a form feed and a vertical tab', async () => {
    const id = await idOfSection(store, 'Two');
    const FORM_FEED = '\u000c';
    const VERTICAL_TAB = '\u000b';
    const body = `line1${FORM_FEED}form feed above${VERTICAL_TAB} vertical tab\n`;

    const written = await store.client.request('set', [id, 'src', body]);
    assert.strictEqual(written.ok, true, `the core refused the write: ${written.text}`);

    const answer = await store.client.request('read', [id]);
    assert.strictEqual(answer.ok, true);
    const block = readBlock((answer.answers[0] as unknown[])[1]);
    assert.ok(block !== null);
    assert.strictEqual(documentFor(block as NonNullable<typeof block>, store.store).src, body);
  });
});

describe('S13 a save whose answer is lost, retried against the real store', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    store = await RealStore.make('vscode-lossy');
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => {
    /*
     * THE PIN IS CHECKED WHATEVER HAPPENED TO THE CLEANUP. A dispose
     * that throws would otherwise take the provenance check with it, and
     * the readings in this file would go unqualified.
     */
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * THE RECORD REACHES THE STORE AND THE ANSWER DOES NOT. Refusing to
   * send would prove nothing: the retry would find nothing to replay and
   * would land as a first write, which is the case that already works.
   * What has to be shown is that the store recognises the request it has
   * already applied, and that the second attempt leaves the log the
   * length it was.
   */
  it('the retry is answered as a replay and appends no second record', async () => {
    const id = await idOfSection(store, 'Two');
    const outbox = new Outbox(path.join(store.root, 'outbox-lossy.json'));
    outbox.load();

    const losing = new LosesTheAnswer(
      new CliTransport(store.config),
      (verb) => verb === 'set'
    );
    const blind = new Saver(new Client(losing), outbox, settling(outbox));
    const lost = await blind.save(id, 'src', 'written but unheard\n');
    assert.strictEqual(lost.status, 'pending', lost.message);
    assert.strictEqual(outbox.pendingCount, 1, 'the unheard save was dropped');
    assert.deepStrictEqual(losing.delivered[0].slice(0, 3), ['set', id, 'src']);

    /*
     * The store has the record even though this client was not told so.
     */
    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(readBack.text, '## Two\nwritten but unheard\n');
    const logBefore = await store.client.request('log', [id]);

    const reloaded = new Outbox(outbox.path);
    reloaded.load();
    assert.strictEqual(reloaded.pendingCount, 1);
    assert.strictEqual(reloaded.entries[0].payload, 'written but unheard\n');
    /*
     * C14: THE SAME REQUEST, NOT AN EQUIVALENT ONE. A build that minted
     * a fresh id and cursor for the retry would reach the store with a
     * request it has never seen, and the store would apply the body a
     * SECOND time -- the log length below would still be checked, but
     * only after the damage. What makes the retry safe is that the
     * identity survived the restart, so it is asserted here rather than
     * inferred from the answer.
     */
    assert.strictEqual(
      reloaded.entries[0].req,
      outbox.entries[0].req,
      'the retry carries a different request id than the one the store applied'
    );
    assert.strictEqual(
      reloaded.entries[0].cursor,
      outbox.entries[0].cursor,
      'the retry carries a different cursor, so it is a different request wearing the same name'
    );

    const saver = new Saver(store.client, reloaded, settling(reloaded));
    const outcomes = await saver.retry();
    assert.strictEqual(outcomes.length, 1);
    assert.strictEqual(
      outcomes[0].status,
      'replayed',
      `the store did not recognise the request it had applied: ${outcomes[0].message}`
    );
    assert.strictEqual(reloaded.pendingCount, 0);

    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length,
      'the retry appended a second record'
    );
    const stillThere = await store.client.request('read', [id, '--md']);
    assert.strictEqual(stillThere.text, '## Two\nwritten but unheard\n');

    /*
     * AND THE REPLAY MOVED THE CURSOR. Every assertion above is
     * satisfied by a client that drops a replayed entry without carrying
     * its event forward -- the status is right, the queue is empty, the
     * log did not grow and the body is in the store. What that client
     * gets wrong only shows on the NEXT save, whose cursor would then be
     * from before the record the store is standing behind.
     */
    /*
     * THE EXPECTATION COMES FROM THE STORE'S ANSWER, NOT FROM THE
     * CLIENT'S OWN RECORD. Reading `reloaded.cursor` and then checking
     * that the next save carried it compares the client with itself: a
     * client that failed to advance on a replay stores the old cursor
     * and then faithfully sends the old cursor, and the two agree. The
     * replay answer names the record the store is standing behind, and
     * that is the outside source.
     */
    const replayed = outcomes[0].answer;
    const named = readEvent(clauseValue(replayed, 'event') as unknown);
    /*
     * THE MESSAGE MUST SURVIVE THE VALUE IT DESCRIBES. Every integer the
     * reader produces is a BigInt, and JSON.stringify throws on one --
     * so the first version of this line replaced a readable assertion
     * failure with a TypeError about the message.
     */
    assert.ok(named !== null, `the replay answer named no record: ${String(replayed)}`);
    const after = `${named?.writer}:${named?.seq}`;
    assert.strictEqual(
      reloaded.cursor,
      after,
      'the client did not carry the record the replay named into its cursor'
    );
    const counting = new Counting(new CliTransport(store.config));
    const next = new Saver(new Client(counting), reloaded, settling(reloaded));
    const second = await next.save(id, 'src', 'and then this\n');
    assert.strictEqual(second.status, 'saved', second.message);
    const sent = counting.sent.find((c) => c[0] === 'set');
    assert.ok(sent !== undefined);
    assert.strictEqual(
      (sent as string[])[(sent as string[]).indexOf('--cursor') + 1],
      after,
      'the save after a replay did not carry the cursor the replay established'
    );
  });

  /*
   * THE SAME THING WITHOUT BUILDING A NEW SAVER. The cell above makes a
   * fresh Saver between the replay and the next save, so a cursor held
   * inside the replaying instance -- stale, and never consulted again --
   * would escape it. Here one instance does both, which is what the
   * extension does.
   */
  it('carries the replay\'s cursor forward within one saver', async () => {
    const id = await idOfSection(store, 'Three  spaced');
    const outbox = new Outbox(path.join(store.root, 'outbox-same-saver.json'));
    outbox.load();

    const losing = new LosesTheAnswer(new CliTransport(store.config), (verb) => verb === 'set');
    const blind = new Saver(new Client(losing), outbox, settling(outbox));
    const lost = await blind.save(id, 'src', 'unheard again\n');
    assert.strictEqual(lost.status, 'pending', lost.message);

    const counting = new Counting(new CliTransport(store.config));
    const saver = new Saver(new Client(counting), outbox, settling(outbox));
    const retried = await saver.retry();
    assert.strictEqual(retried[0].status, 'replayed', retried[0].message);

    const named = readEvent(clauseValue(retried[0].answer, 'event') as unknown);
    assert.ok(named !== null, `the replay named no record: ${String(retried[0].answer)}`);
    const expected = `${named?.writer}:${named?.seq}`;

    counting.forget();
    const second = await saver.save(id, 'src', 'and then this\n');
    assert.strictEqual(second.status, 'saved', second.message);
    const sent = counting.sent.find((c) => c[0] === 'set');
    assert.ok(sent !== undefined);
    assert.strictEqual(
      (sent as string[])[(sent as string[]).indexOf('--cursor') + 1],
      expected,
      'the same saver did not carry the cursor its own replay established'
    );
  });
});

describe('the marks the model draws are the marks a real store reports', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    store = await RealStore.make('vscode-marks');
  });

  after(() => {
    /*
     * THE PIN IS CHECKED WHATEVER HAPPENED TO THE CLEANUP. A dispose
     * that throws would otherwise take the provenance check with it, and
     * the readings in this file would go unqualified.
     */
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * THE WHOLE MARK PIPELINE WAS CHECKED ONLY AGAINST A STAND-IN, and the
   * stand-in is scripted from shapes this repository measured. If the
   * measurement were wrong the stand-in and the model would agree with
   * each other and both be wrong about the store -- one implementation
   * wearing two hats. So a real store is put into a real structural
   * conflict here: a parent is deleted and its child is left behind.
   */
  it('reads an orphan the store really has', async () => {
    const made = await store.client.request('insert', ['--under', 'root', '--title', 'Parent']);
    assert.strictEqual(made.ok, true, made.text);
    const parent = parseOutline((await store.client.request('outline', [])).text)[0].id;
    const kid = await store.client.request('insert', ['--under', parent, '--title', 'Kid']);
    assert.strictEqual(kid.ok, true, kid.text);
    const removed = await store.client.request('del', [parent]);
    assert.strictEqual(removed.ok, true, removed.text);

    const model = new StoreModel(store.client);
    const marks = await model.structuralMarks();
    assert.strictEqual(marks.size, 1, `the store reported ${marks.size} marked blocks`);
    const [id, found] = [...marks.entries()][0];
    assert.deepStrictEqual(found, ['orphan'], `the store marked ${id} as ${found.join(',')}`);

    /*
     * And the listing shows it: an orphan's position still names the
     * parent that is gone, so it is in the root listing on the strength
     * of the mark and nothing else. This is the branch the stand-in
     * cells exercise with a scripted `(orphan "a.2")`.
     */
    const roots = await model.roots();
    assert.deepStrictEqual(roots.map((n) => n.id), [id]);
    assert.deepStrictEqual(roots[0].marks, ['orphan']);
    assert.strictEqual(roots[0].orphan, true);
    assert.strictEqual(
      roots[0].title,
      'Kid',
      'the title came from somewhere other than the block'
    );

    const block = await model.blockOf(id);
    assert.ok(block !== null);
    assert.strictEqual(
      block?.placement.kind,
      'under',
      'a real orphan no longer names the parent it lost, so the exception that lists it is unnecessary'
    );
  });

  it('reports no marks at all for a store with nothing wrong with it', async () => {
    const clean = await RealStore.make('vscode-clean');
    try {
      await clean.importMarkdown('doc.md', DOC);
      const model = new StoreModel(clean.client);
      assert.strictEqual((await model.structuralMarks()).size, 0);
      assert.strictEqual(await model.conflictCount(), 0);
      const roots = await model.roots();
      /*
       * A LOOP OVER NOTHING ASSERTS NOTHING. Without this line a model
       * that returned no roots at all would satisfy every claim below.
       */
      assert.ok(roots.length > 0, 'the store reported no roots, so the loop below checked nothing');
      for (const root of roots) {
        assert.deepStrictEqual(root.marks, [], `${root.id} was marked in a sound store`);
      }
    } finally {
      clean.dispose();
    }
  });
});

describe('an expansion against the real core costs what the stand-in says it costs', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    store = await RealStore.make('vscode-count');
    await store.importMarkdown('doc.md', DEEP_DOC);
  });

  after(() => {
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * THE STAND-IN CELLS PIN THE REQUEST COUNTS AND THE REAL-CORE ONES DID
   * NOT, so a change that started fetching one block at a time would
   * have failed only the scripted half. Counting is done by wrapping the
   * client rather than by reading the core's own log, because what is
   * being counted is what this extension SENDS.
   */
  it('sends the subtree and the conflicts, and nothing per child', async () => {
    /*
     * THE FIXTURE HAS TWO DIRECT CHILDREN AND A GRANDCHILD, because a
     * count taken over one child cannot see a client that fetches one
     * request per child -- with a single child the two are the same
     * number. And EVERY verb is counted, not three named ones: an extra
     * `check` slipped past a count that only looked for the three it
     * expected.
     */
    const counting = new Counting(new CliTransport(store.config));
    const model = new StoreModel(new Client(counting));

    const roots = await model.roots();
    assert.strictEqual(roots.length, 1);

    /*
     * THE FIRST EXPANSION IS MEASURED TOO. Discarding its traffic and
     * counting the second would miss a client that pays something extra
     * only once -- a lazily built cache, a one-time probe -- which is
     * exactly the shape of a regression that hides from a second
     * measurement.
     */
    counting.forget();
    const documents = (await model.childrenOf(roots[0].id)).nodes;
    assert.strictEqual(documents.length, 1, 'the fixture should have one document under the file');
    assert.strictEqual(
      counting.sent.length,
      2,
      `the first expansion sent ${counting.sent.length} requests: ${JSON.stringify(counting.sent)}`
    );

    counting.forget();
    const listing = await model.childrenOf(documents[0].id);
    /*
     * BY ID, NOT BY TITLE. A title comes from a field and two blocks can
     * share one; the id is what the listing is actually about, and it is
     * what the stand-in twin compares.
     */
    const outline = parseOutline((await store.client.request('outline', [])).text);
    const expected = outline.filter((r) => r.depth === 2).map((r) => r.id);
    assert.strictEqual(expected.length, 2, 'the fixture no longer has two direct children');
    assert.deepStrictEqual(
      listing.nodes.map((n) => n.id),
      expected,
      'the expansion does not hold the ids the outline puts under this block'
    );
    assert.strictEqual(listing.marksKnown, true);

    assert.strictEqual(
      counting.sent.length,
      2,
      `an expansion sent ${counting.sent.length} requests: ${JSON.stringify(counting.sent)}`
    );
    assert.strictEqual(counting.countOf('read'), 1, 'the subtree was read more than once');
    assert.strictEqual(counting.countOf('conflicts'), 1, 'the conflicts were asked for more than once');
    assert.deepStrictEqual(counting.sent[0].slice(0, 3), ['read', documents[0].id, '--recursive']);

    /*
     * And the grandchild is reachable, which is what makes the two
     * direct children a real filter rather than an accident of a flat
     * fixture.
     */
    counting.forget();
    const deeper = await model.childrenOf(listing.nodes[0].id);
    assert.deepStrictEqual(deeper.nodes.map((n) => n.title), ['Deeper']);
    assert.strictEqual(counting.sent.length, 2, 'expanding a node with one child cost more than two');
  });

  /*
   * A NESTED DOCUMENT CANNOT BE MADE THROUGH ANY VERB THIS CORE HAS.
   * That is why the case is pinned against a stand-in and not here: the
   * write path refuses both ways in, and one can only arrive by import
   * from elsewhere or from history that predates the rule.
   */
  it('refuses to make a nested document, which is why one cannot be built here', async () => {
    const roots = await new StoreModel(store.client).roots();
    const file = roots[0].id;
    /*
     * THE PARENT IS A DIFFERENT BLOCK. Moving the document under ITSELF
     * is refused by any implementation that turns away self-moves, which
     * is not the rule this cell is about; the rule is that a document
     * cannot sit under anything. A distinct parent asks that question
     * and the self-move does not.
     */
    const parent = await store.client.request('insert', [
      '--under',
      'root',
      '--title',
      'Somewhere else',
      '--text',
      'x'
    ]);
    assert.strictEqual(parent.ok, true, parent.text);
    /*
     * THE PARENT IS THE BLOCK THAT WAS JUST INSERTED, AND IT IS AT THE
     * TOP LEVEL. Taking "the first row that is not the document" can
     * pick one of the document's own descendants, and moving a block
     * under its own descendant is refused by a different rule -- so the
     * cell would pass without the document-placement rule existing.
     */
    const rows = parseOutline((await store.client.request('outline', [])).text);
    const topLevel = rows.filter((r) => r.depth === 0).map((r) => r.id);
    const under = topLevel.find((candidate) => candidate !== file);
    assert.ok(under !== undefined, `no second top-level block among ${topLevel.join(', ')}`);
    const underBlock = await store.client.request('read', [under as string]);
    assert.strictEqual(underBlock.ok, true, 'the chosen parent could not be read');
    const moved = await store.client.request('move', [file, under as string]);
    assert.strictEqual(moved.ok, false, `moving a document under ${under} was accepted`);
    assert.match(moved.text, /doc-must-be-top-level/, `move answered ${moved.text}`);
  });
});

describe('S14 the bytes a store accepts are the bytes this client can read back, continued', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    store = await RealStore.make('vscode-symbols');
  });

  after(() => {
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * ⚠️ THESE TWO CELLS ARE RED UNTIL THE CORE LANDS U8. That is
   * deliberate and it is not a defect in this extension.
   *
   * WHAT WAS MEASURED against pin 9ecbd88e on 2026-09-12: the core now
   * refuses a relation name the wire cannot carry, and nothing lands --
   *
   *   $ theourgia link <a> "has part" <b>
   *   (error malformed-intent (symbol-not-wire-safe (link "<a>" has\x20;part "<b>")))
   *   $ theourgia refs <b>
   *   (ref (from "<a>") (rel has-part) (via link))     <- only the ordinary one
   *
   * -- but the refusal echoes the offending symbol back USING THE VERY
   * ESCAPE IT IS REFUSING, so this client cannot read the answer that is
   * about it: what reaches the user is "the core's answer to link could
   * not be read at line 1: bad token @65". A refusal nobody can read is
   * a refusal nobody can act on, and it is unreadable precisely because
   * its reason is "this cannot be read".
   *
   * THE RULING (U8, theourgia main session, Q2): the data of any error
   * answer is wire-safe BY CONSTRUCTION. A refusal does not echo the raw
   * intent; it describes it -- `(symbol-not-wire-safe (field rel)
   * (spelling "1"))`, with the offending name given as a STRING. The
   * whole `malformed-intent` family becomes position-plus-string.
   *
   * SO THE EXPECTATION HERE IS THE RULED SHAPE, NOT TODAY'S. These cells
   * assert that the refusal comes back readable, as `ok: false`, naming
   * the field and carrying the spelling. Until the core lands it they
   * FAIL, and they say in their own failure text that they are waiting
   * on the core rather than reporting a regression. They are not skipped
   * and not special-cased: a cell that tolerated both shapes would go on
   * passing after the change and would stop being evidence of anything.
   *
   * ⚠️ SO THIS SUITE IS NOT ALL-GREEN BY DESIGN. "0 failing" is not the
   * gate for this delivery; "exactly these two, by these names" is.
   *
   * READING A STORE THAT ALREADY HOLDS SUCH AN EDGE is covered
   * separately, by the byte-level twin in shapes.test.ts -- a store
   * written before this rule, or imported from elsewhere, can still
   * carry one, and this client must report it by name rather than as a
   * block with no edges. That coverage does not depend on U8.
   */

  /*
   * THE REFUSAL, AS U8 RULES IT. Split out because both cells ask the
   * same question of a different unspellable name, and a second copy of
   * these assertions would be a second place for them to drift from the
   * ruling.
   */
  async function refusalOf(
    where: RealStore,
    from: string,
    name: string,
    to: string
  ): Promise<void> {
    /*
     * THE STATE IS READ AROUND THIS REQUEST AND NO OTHER. An earlier
     * version checked the state first and then made a SECOND write whose
     * effect nothing looked at, so "nothing landed" was established
     * about a different request than the one whose refusal is asserted.
     */
    const before = [
      (await where.client.request('read', [from])).text,
      (await where.client.request('refs', [to])).text
    ];
    let answer;
    let failure: unknown = null;
    try {
      answer = await where.client.request('link', [from, name, to]);
    } catch (e) {
      failure = e;
    }
    /*
     * NOTHING LANDED, WHOLE AND UNCHANGED, not "the escape is absent".
     * A wrongly stored but readable relation passes an absence check and
     * fails this one.
     */
    const after = [
      (await where.client.request('read', [from])).text,
      (await where.client.request('refs', [to])).text
    ];
    assert.deepStrictEqual(after, before, 'the refused write changed the store');

    if (failure !== null) {
      /*
       * ⚠️ ONLY ONE FAILURE MEANS "WAITING ON THE CORE". Diagnosing
       * every exception this way would label a timeout, a missing
       * library or any other transport regression as an upstream wait --
       * and a gate that accepts these two cells by name would then be
       * hiding a new defect behind a sentence saying there is none.
       */
      const unreadable =
        failure instanceof TransportError && (failure as TransportError).failure === 'unreadable';
      if (!unreadable) {
        throw failure;
      }
      assert.fail(
        `WAITING ON THE CORE (U8), not a regression here: the refusal of the relation name ` +
          `${JSON.stringify(name)} could not be read by this client -- ` +
          `${(failure as Error).message}. The core this ran against still echoes the offending ` +
          'symbol using the escape it is refusing. U8 has been ruled and has landed in the core ' +
          'since this baseline was pinned, so the fix for this red is to re-pin, not to change ' +
          'anything here.'
      );
      return;
    }
    assert.strictEqual(
      answer?.ok,
      false,
      `a name the wire cannot carry was accepted: ${answer?.text}`
    );
    /*
     * THE ASSERTIONS LIVE IN test/support/refusal-shape.ts, and a
     * witness in shapes.test.ts runs the same ones against the ruling's
     * written-out example and against nine near misses. A red cell's
     * expectation is checked by nothing: without that witness a wrong
     * pattern would keep this red for ever, including after the core
     * lands the change.
     */
    assertRuledRefusal(answer?.text ?? '', name);
  }
  /*
   * A NAME THE WIRE CANNOT CARRY, FROM THE WRITE SIDE.
   *
   * ⚠️ THIS PARAGRAPH DESCRIBED THE OLD CORE AND IS KEPT AS HISTORY
   * RATHER THAN AS A STATEMENT ABOUT THE ONE UNDER TEST. It used to say
   * that `wire-safe-symbol?` answering false did NOT refuse a write --
   * `storable-encode` stored the wrapped form `("#%sym" "1")` and `read`
   * printed the name back escaped as `\x31;`, so an ordinary `link` was
   * enough to make a block this client could not read -- and that the
   * answer to `link` was therefore not asserted. Both sentences are now
   * false of the core: the write is refused and nothing lands. What is
   * asserted below is the refusal, in the shape U8 rules for it.
   *
   * READING A STORE THAT ALREADY HOLDS SUCH AN EDGE is the behaviour
   * that paragraph was protecting, and it is still needed for stores
   * written before the rule or imported from elsewhere. It lives in the
   * byte-level twin in shapes.test.ts.
   */
  it('reports a block whose edge name the store accepts and this reader refuses', async () => {
    /*
     * ITS OWN STORE. The cell beside this one asks its store for the
     * whole outline and expects the two blocks it made; blocks inserted
     * here would be counted there, and the first version of this cell
     * broke it that way. A cell that adds to a shared store has to own
     * one instead.
     */
    const mine = await RealStore.make('vscode-numeric');
    try {
      await runNumericNameCell(mine);
    } finally {
      mine.dispose();
    }
  });

  async function runNumericNameCell(store: RealStore): Promise<void> {
    const a = await store.client.request('insert', ['--under', 'root', '--title', 'NA', '--text', 'a']);
    assert.strictEqual(a.ok, true, a.text);
    const b = await store.client.request('insert', ['--under', 'root', '--title', 'NB', '--text', 'b']);
    assert.strictEqual(b.ok, true, b.text);
    const rows = parseOutline((await store.client.request('outline', [])).text);
    const [from, to] = rows.map((r) => r.id);

    /*
     * READABLE BEFORE THE LINK, and readable with an ORDINARY edge, so
     * that the refusal below belongs to the numeric name rather than to
     * having any edge at all.
     */
    const spelled = await store.client.request('link', [from, 'has-part', to]);
    assert.strictEqual(spelled.ok, true, `an ordinary link was refused: ${spelled.text}`);
    const before = await store.client.request('read', [from]);
    assert.strictEqual(before.ok, true, 'the block was unreadable before the numeric name');

    /*
     * THE REFUSAL, AND THE STATE AROUND IT. `refusalOf` reads the block
     * and its references before and after the one request it makes, so
     * "nothing landed" is about that request and no other.
     */
    await refusalOf(store, from, '1', to);
  }

  it('reports a block whose edge name the wire cannot carry, and names it', async () => {
    const a = await store.client.request('insert', ['--under', 'root', '--title', 'A', '--text', 'a']);
    assert.strictEqual(a.ok, true, a.text);
    const b = await store.client.request('insert', ['--under', 'root', '--title', 'B', '--text', 'b']);
    assert.strictEqual(b.ok, true, b.text);
    const rows = parseOutline((await store.client.request('outline', [])).text);
    assert.strictEqual(rows.length, 2);
    const [from, to] = rows.map((r) => r.id);

    /*
     * READABLE BEFORE THE LINK. Without this control the cell passes on
     * any store whose blocks were never readable, and the link it is
     * about would have nothing to do with the failure.
     */
    const before = await store.client.request('read', [from]);
    assert.strictEqual(before.ok, true, 'the block was already unreadable before the link');
    /*
     * AND THE REFERENCE QUERY ANSWERED TOO. Without this control an
     * implementation that always reported `refs` as unreadable -- naming
     * its subject as it did so -- would satisfy the assertion below
     * without the link having anything to do with it.
     *
     * THE CONTROL IS NOT ALLOWED TO BE EMPTY. Asked before any link
     * exists, `refs` answers about a block with no references, and an
     * implementation that accepted that and refused every non-empty
     * answer -- or refused every block carrying an edge at all -- would
     * pass. So an ORDINARY link is made first, with a name the wire can
     * spell, and both queries are made to answer about it. What is left
     * between this control and the assertions below is the space in the
     * edge name, which is what the cell is about.
     */
    const spelled = await store.client.request('link', [from, 'has-part', to]);
    assert.strictEqual(spelled.ok, true, `an ordinary link was refused: ${spelled.text}`);
    const refsBefore = await store.client.request('refs', [to]);
    assert.strictEqual(refsBefore.ok, true, 'references were already unreadable before the link');
    assert.ok(
      refsBefore.answers.length > 0,
      'the control asked about a block with no references, so it proves nothing about one that has them'
    );
    const readBefore = await store.client.request('read', [from]);
    assert.strictEqual(readBefore.ok, true, 'a block with an ordinary edge was already unreadable');

    /*
     * THE REFUSAL, AND THE STATE AROUND IT. Both the block and the
     * references are compared whole across the one request, because a
     * write path that refused the read side and kept the reference would
     * satisfy either alone.
     */
    await refusalOf(store, from, 'has part', to);
  });
});

/*
 * ONE SESSION, TWO STORES, AND TWO CURSORS.
 *
 * ⚠️ THIS IS THE DEFECT THE WIRING PRODUCED. The queue moved inside the
 * session -- which is what stops two windows sharing one file -- and a
 * queue carries ONE cursor. A session spans as many stores as the user
 * points it at, so after switching, the saver was offering a position
 * the new store had never issued and every save came back
 * `cursor-unreachable`. Nothing in the modules could show it: each is
 * right on its own, and only a second store makes the queue ambiguous.
 *
 * THE QUEUE IS THEREFORE PER SESSION AND PER STORE. This cell alternates
 * between two real stores through one session and requires every save to
 * land -- with a single queue, the second one fails.
 */
describe('a session that writes to two stores keeps their cursors apart', function () {
  this.timeout(180000);
  let one: RealStore;
  let two: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    one = await RealStore.make('two-stores-a');
    two = await RealStore.make('two-stores-b');
    await one.importMarkdown('a.md', '# Doc A\n\n## Block\nfrom a\n');
    await two.importMarkdown('b.md', '# Doc B\n\n## Block\nfrom b\n');
  });

  after(() => {
    try {
      one?.dispose();
      two?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  it('lands every save when the session alternates between them', async () => {
    const sessions = new Sessions(nodeFileOps, one.root);
    sessions.begin('S-two-stores', [one.store, two.store]);
    const hash = (store: string): string =>
      createHash('sha256').update(store, 'utf8').digest('hex').slice(0, 16);

    const saverFor = async (store: RealStore): Promise<Saver> => {
      const outbox = new Outbox(sessions.outboxPathFor('S-two-stores', hash(store.store)));
      outbox.load();
      return new Saver(store.client, outbox, settling(outbox));
    };

    const idOf = async (store: RealStore): Promise<string> => {
      const outline = await store.client.request('outline', []);
      for (const line of outline.text.split('\n')) {
        const at = line.indexOf('- ');
        if (at > 0) {
          return line.slice(at + 2).split('  ')[0];
        }
      }
      throw new Error(`no child block in ${outline.text}`);
    };

    const a = await idOf(one);
    const b = await idOf(two);

    const first = await (await saverFor(one)).save(a, 'src', 'first into a\n');
    assert.strictEqual(first.status, 'saved', first.message);
    const second = await (await saverFor(two)).save(b, 'src', 'first into b\n');
    assert.strictEqual(
      second.status,
      'saved',
      `the second store refused the save: ${second.message}. With one queue per session the cursor ` +
        'from the first store is offered to the second, which has never issued it.'
    );
    const third = await (await saverFor(one)).save(a, 'src', 'second into a\n');
    assert.strictEqual(third.status, 'saved', third.message);

    assert.strictEqual((await one.client.request('read', [a, '--md'])).text.includes('second into a'), true);
    assert.strictEqual((await two.client.request('read', [b, '--md'])).text.includes('first into b'), true);
  });
});
