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
 * plugin-r3 item 15: a damaged store's `check` is an answer, and one writer
 * the core could not read does not stop the local writer's cursor.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Client } from '../../src/client';
import { firstCursorFromCheck, writersFromCheck } from '../../src/cursor';
import { Outbox } from '../../src/outbox';
import { Saver, Settle } from '../../src/saver';
import { RawResult } from '../../src/transport';
import { initWire, isSym, wire } from '../../src/wire';
import { CorePin, RealStore, checkCorePin, pinCore } from '../support/real-core';

/*
 * THE ANSWER F46 GIVES for a store with one writer directory it cannot
 * read (measured: `chmod 000` on a second writer's directory), with the
 * paths shortened. Exit 1, because the verdict is not `ok`.
 */
const DAMAGED =
  '(check (store "pgvsik24") (local-writer "3tfoh9hu") (writers (("3tfoh9hu" (end 1) (torn #f) ' +
  '(integrity ())) ("zzzzzzzz" (end unreadable) (torn unreadable) (integrity ((metadata-unreadable ' +
  '(path "/s/writers/zzzzzzzz") (reason "Permission denied") (errno EACCES))))))) (snapshots ()) ' +
  '(registry outside-store) (notes ()) (verdict damaged) (incomplete (unreadable (writer "zzzzzzzz") ' +
  '(path "/s/writers/zzzzzzzz") (reason "Permission denied"))))';

function read(text: string) {
  return wire().read(text);
}

let counter = 0;

function queue(): Outbox {
  counter += 1;
  const outbox = new Outbox(path.join(os.tmpdir(), `tv-damaged-${process.pid}-${counter}.json`));
  outbox.load();
  return outbox;
}

const settling = (outbox: Outbox): Settle => (req, settlement) => {
  if (settlement.verdict === 'req-mismatch') {
    return;
  }
  outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
};

/*
 * A STAND-IN CORE that answers `check` as told and refuses everything else,
 * recording what it was sent -- so a cell can tell a save that was held
 * back at the cursor from one that went on to the store.
 */
function standIn(check: { stdout: string; rc: number }) {
  const sent: string[] = [];
  const transport = {
    kind: 'stand-in',
    send: async (verb: string): Promise<RawResult> => {
      sent.push(verb);
      if (verb === 'check') {
        return { argv: [], rc: check.rc, stdout: `${check.stdout}\n`, stderr: '' };
      }
      return { argv: [], rc: 1, stdout: '(error stand-in-refuses)\n', stderr: '' };
    }
  };
  return { sent, client: new Client(transport) };
}

describe('plugin-r3 15 the local writer\'s cursor, from a listing with another writer unread', () => {
  before(async () => {
    await initWire();
  });

  it('takes the local writer\'s end when another writer\'s end is unreadable', () => {
    assert.deepStrictEqual(firstCursorFromCheck(read(DAMAGED)), { ok: true, cursor: '3tfoh9hu:1' });
  });

  it('refuses when it is the local writer\'s own end that cannot be read, and names it', () => {
    const own = '(check (local-writer "w1") (writers (("w1" (end unreadable)) ("w2" (end 4)))) (verdict damaged))';
    const first = firstCursorFromCheck(read(own));
    assert.strictEqual(first.ok, false);
    if (!first.ok) {
      assert.strictEqual(first.reason, 'unreadable');
      assert.strictEqual(first.unreadable, 'local-end');
      assert.strictEqual(first.local, 'w1');
    }
  });

  /*
   * KEY: WITHOUT THE CLAUSE THE COUNT DECIDES, and an unread entry would
   * change the count: the whole-listing rule stays.
   */
  it('still refuses the whole listing when the core does not name the local writer', () => {
    const unnamed = '(check (writers (("w1" (end 7)) ("w2" (end unreadable)))) (verdict damaged))';
    const first = firstCursorFromCheck(read(unnamed));
    assert.strictEqual(first.ok, false);
    if (!first.ok) {
      assert.strictEqual(first.reason, 'unreadable');
      assert.strictEqual(first.unreadable, 'listing');
    }
  });

  it('refuses a listing whose entry has no name, even when the local writer is named', () => {
    const nameless = '(check (local-writer "w1") (writers (("w1" (end 7)) (7 (end 2)))) (verdict ok))';
    const first = firstCursorFromCheck(read(nameless));
    assert.strictEqual(first.ok, false);
    if (!first.ok) {
      assert.strictEqual(first.unreadable, 'listing');
    }
  });

  /*
   * KEY: ONE WRITER, ONE END. Review r1 of item 15 measured the local writer
   * listed twice, its second end unreadable, giving `w1:7` -- the first entry
   * chosen. Two entries are two answers; both shapes are refused.
   */
  it('refuses a listing that names the local writer twice, whatever the second entry says', () => {
    for (const second of ['(end unreadable)', '(end 9)']) {
      const twice = `(check (local-writer "w1") (writers (("w1" (end 7)) ("w1" ${second}))) (verdict damaged))`;
      const first = firstCursorFromCheck(read(twice));
      assert.strictEqual(first.ok, false, `${second}: a cursor was taken`);
      if (!first.ok) {
        assert.strictEqual(first.reason, 'local-writer-twice', second);
        assert.strictEqual(first.local, 'w1');
      }
    }
  });

  it('keeps writersFromCheck refusing any listing with an unread end', () => {
    assert.strictEqual(writersFromCheck(read(DAMAGED)), null);
  });
});

describe('plugin-r3 15 a damaged store\'s check is an answer', () => {
  before(async () => {
    await initWire();
  });

  /*
   * KEY: THE CASE THE ITEM IS ABOUT. Exit 1 and a complete `check`: the
   * cursor is taken and the save goes on to the store, which decides.
   */
  it('goes on to the store when check exits non-zero with a complete answer', async () => {
    const core = standIn({ stdout: DAMAGED, rc: 1 });
    const outbox = queue();
    const saver = new Saver(core.client, outbox, settling(outbox));
    const outcome = await saver.save('a.1', 'src', 'body\n');
    assert.strictEqual(saver.blockedBecause, null, `held back: ${saver.blockedBecause}`);
    assert.notStrictEqual(outcome.status, 'blocked', outcome.message);
    assert.ok(core.sent.length > 1 && core.sent[0] === 'check', `sent ${core.sent.join(', ')}`);
  });

  it('still says the store did not answer when it answered with no check at all', async () => {
    for (const [stdout, rc] of [['(error stand-in-busy)', 1], ['', 0]] as Array<[string, number]>) {
      const core = standIn({ stdout, rc });
      const outbox = queue();
      const saver = new Saver(core.client, outbox, settling(outbox));
      const outcome = await saver.save('a.1', 'src', 'body\n');
      assert.strictEqual(outcome.status, 'blocked', `${JSON.stringify(stdout)}: ${outcome.message}`);
      assert.match(outcome.message, /did not answer `check`/);
      assert.deepStrictEqual(core.sent, ['check'], `${JSON.stringify(stdout)}: sent more than check`);
    }
  });

  /*
   * KEY: A NON-ZERO EXIT CARRIES WHATEVER ARRIVED, and the client checks for
   * one datum only on a zero exit. Review r1 of item 15 measured a `check`
   * followed by an `(error ...)` supplying a cursor, and the reverse saying
   * "did not answer". Two forms are refused, in either order, and said as two.
   */
  it('refuses a check answered with two forms, in either order, and says there were two', async () => {
    const check = '(check (local-writer "w1") (writers (("w1" (end 7)))) (verdict ok))';
    for (const stdout of [`${check}\n(error refused)`, `(error refused)\n${check}`]) {
      const core = standIn({ stdout, rc: 1 });
      const outbox = queue();
      const saver = new Saver(core.client, outbox, settling(outbox));
      const outcome = await saver.save('a.1', 'src', 'body\n');
      assert.strictEqual(outcome.status, 'blocked', `${JSON.stringify(stdout)}: ${outcome.message}`);
      assert.match(outcome.message, /answered `check` with 2 forms where one was expected/);
      assert.deepStrictEqual(core.sent, ['check'], `${JSON.stringify(stdout)}: sent more than check`);
    }
  });

  it('tells a person when the local writer is listed twice', async () => {
    const twice = '(check (local-writer "w1") (writers (("w1" (end 7)) ("w1" (end unreadable)))) (verdict damaged))';
    const core = standIn({ stdout: twice, rc: 1 });
    const outbox = queue();
    const saver = new Saver(core.client, outbox, settling(outbox));
    const outcome = await saver.save('a.1', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /lists its local writer \(w1\) more than once/);
    assert.deepStrictEqual(core.sent, ['check']);
  });

  it('tells a person when the local writer\'s own position could not be read', async () => {
    const own = '(check (local-writer "w1") (writers (("w1" (end unreadable)))) (verdict damaged))';
    const core = standIn({ stdout: own, rc: 1 });
    const outbox = queue();
    const saver = new Saver(core.client, outbox, settling(outbox));
    const outcome = await saver.save('a.1', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /naming its local writer \(w1\) without a position/);
    assert.doesNotMatch(outcome.message, /did not answer/);
  });
});

describe('plugin-r3 15 a real store with a writer it cannot read', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;
  let locked: string | null = null;

  before(async () => {
    pinned = pinCore();
    await initWire();
    store = await RealStore.make();
  });

  after(() => {
    try {
      if (locked !== null) {
        fs.chmodSync(locked, 0o755);
      }
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  /*
   * KEY: WHAT THE USER IS TOLD IS WHAT THE STORE SAID. On F46 the core
   * answers the write on such a store with `(error unknown (uncertain-cache
   * unreadable) (incomplete ...))` -- it cannot say whether the write ran --
   * and the save is kept to be retried, as for any answer of that kind
   * (measured). Before this item the save never reached the store: the
   * user was told the store had not answered `check`.
   */
  it('lets the store answer the save, and says what it said', async () => {
    const made = await store.client.request('insert', ['--under', 'root', '--title', 'H', '--text', 'one\n']);
    assert.ok(made.ok, made.text);
    const id = (/\(state \(\("([^"]+)"/.exec(made.text) as RegExpExecArray)[1];
    locked = path.join(store.store, 'writers', 'zzzzzzzz');
    fs.mkdirSync(locked);
    fs.writeFileSync(path.join(locked, '000001.sexp'), '');
    fs.chmodSync(locked, 0o000);

    const check = await store.client.request('check', []);
    assert.strictEqual(check.ok, false, 'the core no longer exits non-zero on a damaged store; re-read this item');
    assert.match(check.text, /\(end unreadable\)/, check.text);

    const outbox = queue();
    const saver = new Saver(store.client, outbox, settling(outbox));
    const outcome = await saver.save(id, 'src', 'two\n');
    assert.strictEqual(saver.blockedBecause, null, `held back: ${saver.blockedBecause}`);
    assert.doesNotMatch(outcome.message, /did not answer `check`/);
    assert.strictEqual(outcome.status, 'pending', outcome.message);
    const said = outcome.answer;
    assert.ok(
      Array.isArray(said) && isSym(said[0], 'error') && isSym(said[1], 'unknown'),
      'the outcome is not the store\'s own `(error unknown ...)`'
    );
    assert.strictEqual(outbox.entries.length, 1, 'a save the store could not settle was not kept');
  });
});
