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
import * as path from 'path';
import { documentFor, readBlock, splitDocument, titleOf } from '../../src/blocks';
import { StoreModel } from '../../src/model';
import { Outbox } from '../../src/outbox';
import { parseOutline } from '../../src/outline';
import { Client } from '../../src/client';
import { CliTransport } from '../../src/transport';
import { Saver } from '../../src/saver';
import { LosesTheAnswer } from '../support/lossy';
import { clause, initWire, readEvent } from '../../src/wire';
import { RealStore, checkCorePin, pinCore } from '../support/real-core';

const DOC = '# Doc One\n\nintro\n\n## Two\nbody\n\n## Three  spaced\nb3\n';

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
  let pinned: { corePath: string; digest: string };

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
  let pinned: { corePath: string; digest: string };

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
    const saver = new Saver(store.client, outbox);

    const split = splitDocument(document, `${document.headingSrc}body2\n`);
    assert.strictEqual(split.ok, true);
    const outcome = await saver.save(id, 'src', (split as { src: string }).src);
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
  });

  it('answers a repeat of the same request with a replay and appends nothing', async () => {
    const id = await idOfSection(store, 'Three  spaced');
    const outbox = new Outbox(path.join(store.root, 'outbox-replay.json'));
    outbox.load();
    const saver = new Saver(store.client, outbox);

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
    assert.match(repeat.text, /replay/, `the repeat was not answered as a replay: ${repeat.text}`);

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
  let pinned: { corePath: string; digest: string };

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
   * KNOWN RED until goeteia's reader accepts the escapes a Chez writer
   * emits. The core takes a form feed in a body, stores it, and prints
   * it back as an escape the vendored reader refuses, so the block
   * becomes one this extension cannot open again. The fix is upstream
   * and this cell is what will notice the day it lands.
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
  let pinned: { corePath: string; digest: string };

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
    const blind = new Saver(new Client(losing), outbox);
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

    const saver = new Saver(store.client, reloaded);
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
  });
});

describe('the marks the model draws are the marks a real store reports', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: { corePath: string; digest: string };

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
      for (const root of await model.roots()) {
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
  let pinned: { corePath: string; digest: string } | undefined;

  before(async () => {
    pinned = pinCore();
    store = await RealStore.make('vscode-count');
    await store.importMarkdown('doc.md', DOC);
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
    const sent: string[][] = [];
    const counting = {
      transportKind: store.client.transportKind,
      request: (verb: string, args: string[] = []) => {
        sent.push([verb, ...args]);
        return store.client.request(verb, args);
      }
    } as unknown as typeof store.client;

    const model = new StoreModel(counting);
    const roots = await model.roots();
    assert.strictEqual(roots.length, 1);

    const outlineCalls = sent.filter((c) => c[0] === 'outline');
    assert.strictEqual(outlineCalls.length, 1, 'the root listing asked for the outline more than once');
    assert.deepStrictEqual(outlineCalls[0].slice(0, 3), ['outline', '--depth', '1']);
    assert.strictEqual(
      sent.filter((c) => c[0] === 'conflicts').length,
      1,
      'the root listing asked for the conflicts more than once'
    );
    assert.strictEqual(
      sent.filter((c) => c[0] === 'read').length,
      roots.length,
      'the root listing did not ask for exactly one read per row'
    );

    sent.length = 0;
    const listing = await model.childrenOf(roots[0].id);
    assert.ok(listing.nodes.length > 0, 'the imported document has children');
    assert.strictEqual(listing.marksKnown, true, 'the marks were answered and the listing says otherwise');
    assert.strictEqual(sent.length, 2, `an expansion sent ${sent.length} requests: ${JSON.stringify(sent)}`);
    assert.deepStrictEqual(sent[0].slice(0, 3), ['read', roots[0].id, '--recursive']);
    assert.strictEqual(sent[1][0], 'conflicts');
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
    const second = await RealStore.make('vscode-count-2');
    try {
      await second.importMarkdown('other.md', '# Other\n\nbody\n');
      const other = await new StoreModel(second.client).roots();
      assert.strictEqual(other.length, 1);
    } finally {
      second.dispose();
    }
    const moved = await store.client.request('move', [file, file]);
    assert.strictEqual(moved.ok, false);
    assert.match(moved.text, /doc-must-be-top-level/, `move answered ${moved.text}`);
  });
});
