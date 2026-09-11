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
import { initWire } from '../../src/wire';
import { RealStore } from '../support/real-core';

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

  before(async () => {
    await initWire();
    store = await RealStore.make();
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => store?.dispose());

  it('shows every id the outline holds, and no others', async () => {
    const outline = await store.client.request('outline', []);
    assert.strictEqual(outline.ok, true, outline.stderr);
    const printed = parseOutline(outline.text);
    assert.ok(printed.length >= 4, `the store printed ${printed.length} rows: ${outline.text}`);

    const model = new StoreModel(store.client);
    const seen: string[] = [];
    const walk = async (id: string): Promise<void> => {
      for (const child of await model.childrenOf(id)) {
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

    const documents = await model.childrenOf(roots[0].id);
    assert.deepStrictEqual(
      documents.map((c) => c.title),
      ['Doc One']
    );
    const sections = await model.childrenOf(documents[0].id);
    assert.deepStrictEqual(
      sections.map((c) => c.title),
      ['Two', 'Three  spaced']
    );
  });
});

describe('S7 a save reaches the store and shows up in its log', function () {
  this.timeout(120000);
  let store: RealStore;

  before(async () => {
    await initWire();
    store = await RealStore.make('vscode-cell');
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => store?.dispose());

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
    assert.ok(outbox.cursor !== null, 'the store answered without a cursor');

    const readBack = await store.client.request('read', [id, '--md']);
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

  before(async () => {
    await initWire();
    store = await RealStore.make();
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => store?.dispose());

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

  before(async () => {
    store = await RealStore.make('vscode-lossy');
    await store.importMarkdown('doc.md', DOC);
  });

  after(() => store?.dispose());

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
