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
 * THE COMMAND THAT MAKES A TAKEOVER REACHABLE BY A PERSON.
 *
 * `Sessions` could list, take over and discard for this whole batch, and
 * nothing showed any of it: a refusal told the user to take a window
 * over and the only way to do so was to write code. These cells are
 * about the decisions the command makes -- which windows, which actions,
 * what the user is told they cost, and what happens when they walk away.
 *
 * ⚠️ THE CELLS DRIVE THE PRODUCT'S OWN ENTRY POINT. `chooseAndRecover`
 * is what the registered handler calls, in one line, with a `Chooser`
 * backed by the editor; here it is called with a `Chooser` that records.
 * Nothing is re-implemented, which is the mistake this batch has already
 * made once: a harness that did a step the product forgot left
 * twenty-five cells green over missing wiring.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Choice, Chooser, Destination, RecoveryAction, chooseAndRecover } from '../../src/recovery';
import { ImportTarget, SessionIdentity, Sessions, systemStartTime } from '../../src/sessions';
import { Notice } from '../../src/status';
import { OutboxEntry } from '../../src/outbox';
import { RecordingFs } from '../support/recording-fs';
import { Client } from '../../src/client';
import { CliTransport } from '../../src/transport';
import { Outbox } from '../../src/outbox';
import { Saver } from '../../src/saver';
import { FakeCore } from '../support/fake';
import { initWire } from '../../src/wire';

const CHECK =
  '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) ' +
  '(registry outside-store) (verdict ok))\n';
const WROTE =
  '(ok (events (("w" . 8))) (state (("a.2" . "hhh"))) (cursor ("w" . 8)) (replay #f))\n';
import { idleProcess } from '../support/host';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-recovery-'));
}

function makeSession(storage: string, sessionId: string, over: Partial<SessionIdentity> = {}): void {
  const dir = path.join(storage, 'sessions', sessionId);
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, 'session.json'),
    JSON.stringify({ sessionId, pid: 999999, startedAt: 1000, nonce: 'n', stores: [], ...over }),
    'utf8'
  );
}

function withQueue(storage: string, sessionId: string, reqs: string[]): void {
  const dir = path.join(storage, 'sessions', sessionId, 'h');
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, 'outbox.json'),
    JSON.stringify({
      cursor: null,
      entries: reqs.map((req) => ({
        req,
        cursor: 'w:1',
        id: 'a.1',
        field: 'src',
        payload: 'x',
        state: 'queued',
        createdAt: 0,
        lastError: null
      }))
    }),
    'utf8'
  );
}

/*
 * A CHOOSER THAT ANSWERS FROM A SCRIPT AND WRITES DOWN EVERYTHING IT WAS
 * SHOWN. `undefined` in the script is the user walking away, which is
 * the commonest outcome and the one with the strictest requirement:
 * nothing at all may happen.
 */
class Recorder implements Chooser {
  public readonly offered: Array<Array<Choice<unknown>>> = [];
  public readonly placeholders: string[] = [];
  public readonly confirmations: Array<{ text: string; word: string }> = [];
  public readonly said: Notice[] = [];
  public readonly picks: unknown[];
  public readonly agrees: boolean[];

  public constructor(picks: unknown[], agrees: boolean[] = []) {
    this.picks = picks;
    this.agrees = agrees;
  }

  public async pick<T>(items: Array<Choice<T>>, placeHolder: string): Promise<T | undefined> {
    this.offered.push(items as Array<Choice<unknown>>);
    this.placeholders.push(placeHolder);
    const want = this.picks.shift();
    if (want === undefined) {
      return undefined;
    }
    const found = items.find((item) => item.value === want || item.label === want);
    /*
     * A SCRIPT THAT NAMES SOMETHING THE FLOW DID NOT OFFER IS A BROKEN
     * CELL, not a cancelled pick. Returning `undefined` there would make
     * every such mistake read as "the user walked away" and pass.
     */
    assert.ok(
      found !== undefined,
      `the script asked for ${JSON.stringify(want)}, which was not offered: ` +
        JSON.stringify(items.map((i) => i.label))
    );
    return found?.value;
  }

  public async confirm(text: string, confirmation: string): Promise<boolean> {
    this.confirmations.push({ text, word: confirmation });
    return this.agrees.shift() ?? false;
  }

  public say(notice: Notice): void {
    this.said.push(notice);
  }

  /*
   * ⚠️ EVERY SCRIPTED ANSWER HAD TO BE ASKED FOR.
   *
   * Without this, a flow that asked nothing at all and returned
   * `cancelled` passed every cancellation cell here -- four of them --
   * because each only checked the answer. Found in review with that
   * exact replacement. A cell about a user declining has to establish
   * that they were asked.
   */
  public asked(picks: number, confirmations: number): void {
    assert.strictEqual(
      this.offered.length,
      picks,
      `the flow put ${this.offered.length} lists in front of the user and not ${picks}`
    );
    assert.strictEqual(
      this.confirmations.length,
      confirmations,
      `the flow asked ${this.confirmations.length} confirmations and not ${confirmations}`
    );
    assert.strictEqual(this.picks.length, 0, 'the script named choices the flow never offered');
    /*
     * ⚠️ THE CONFIRMATION ANSWERS TOO. `asked` checked only that the
     * picks were consumed, so a script carrying an answer to a question
     * the flow never asked still passed -- while the comment above says
     * every scripted answer had to be asked for. A review said so.
     */
    assert.strictEqual(
      this.agrees.length,
      0,
      'the script answered a confirmation the flow never asked'
    );
  }
}

/*
 * A DESTINATION THAT ACCEPTS EVERYTHING AND REMEMBERS IT, standing in
 * for this window's own queue. `run` is where the lock would be, and
 * calling `work` inside it is exactly what the Saver does.
 */
function destination(storeHash = 'h'): { into: Destination; held: OutboxEntry[]; runs: number } {
  const held: OutboxEntry[] = [];
  const box = {
    held,
    runs: 0,
    into: {
      storeHash,
      run: async <T>(work: (into: ImportTarget) => T): Promise<T> => {
        box.runs += 1;
        return work({
          has: (req: string) => held.some((e) => e.req === req),
          adopt: (entry: OutboxEntry) => {
            held.push(entry);
          }
        });
      }
    }
  };
  return box;
}

function nowhere(): Destination {
  return destination().into;
}

describe('U-recover the command that shows another window’s unsent work', () => {
  before(async () => {
    await initWire();
  });

  it('says so, and does nothing, when there is no other window', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder([]);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'no-other-sessions' });
    assert.strictEqual(chooser.offered.length, 0, 'an empty list was put in front of the user');
    assert.strictEqual(chooser.said.length, 1);
  });

  /*
   * THE ROW CARRIES WHAT THE USER IS DECIDING ON: which window, how much
   * is at stake, and whether it can be judged. A list of bare ids is a
   * list nobody can choose from.
   */
  it('lists each window with what it holds and whether it can be judged', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1', 'r2']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder([]);
    await chooseAndRecover(sessions, chooser, nowhere());
    const rows = chooser.offered[0];
    assert.strictEqual(rows.length, 1);
    assert.strictEqual(rows[0].label, 'S-dead');
    assert.match(rows[0].description ?? '', /2 unsent requests/);
    assert.match(rows[0].detail ?? '', /not running/);
  });

  /*
   * ⚠️ THE EXPENSIVE ACTION IS OFFERED ONLY WHERE IT IS THE ONLY WAY
   * OUT. A window that can be judged dead has the ordinary takeover;
   * putting the forced one beside it would teach the user to reach for
   * the one that may send somebody's requests twice.
   */
  it('does not offer the forced takeover for a window it can judge', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-dead']);
    await chooseAndRecover(sessions, chooser, nowhere());
    const actions = (chooser.offered[1] as Array<Choice<RecoveryAction>>).map((a) => a.value);
    assert.deepStrictEqual(actions.slice().sort(), ['discard', 'take-over']);
  });

  it('offers it for a window that left no record, which nothing can judge', async () => {
    const storage = scratch();
    withQueue(storage, 'S-norecord', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-norecord']);
    await chooseAndRecover(sessions, chooser, nowhere());
    const actions = (chooser.offered[1] as Array<Choice<RecoveryAction>>).map((a) => a.value);
    assert.ok(actions.includes('force-take-over'), JSON.stringify(actions));
    assert.ok(!actions.includes('take-over'), 'the ordinary takeover was offered for a window nothing can judge');
  });

  /*
   * ⚠️ THE CONFIRMATION IS THE CONSTANT, NOT A SENTENCE THE UI WROTE.
   * What makes the forced takeover offerable at all is the statement of
   * what it costs; a second copy of that statement is a second place for
   * it to be wrong, and this is the one question in the extension whose
   * answer sends somebody's requests again.
   */
  it('confirms a forced takeover with exactly the sentence the model supplies', async () => {
    const storage = scratch();
    withQueue(storage, 'S-norecord', ['r1', 'r2', 'r3']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const box = destination();
    const chooser = new Recorder(['S-norecord', 'force-take-over'], [true]);
    const outcome = await chooseAndRecover(sessions, chooser, box.into);
    /*
     * ⚠️ AND IT WENT THROUGH. A cell that only read the dialog's wording
     * passed a flow that asked the right question and then did nothing,
     * which is not a forced takeover.
     */
    assert.strictEqual(outcome.did, 'take-over', JSON.stringify(outcome));
    assert.strictEqual(chooser.confirmations.length, 1, 'a forced takeover was not confirmed');
    const { text, word } = chooser.confirmations[0];
    const expected = require('../../src/status').forceClaimNotice('S-norecord', 3).text;
    assert.strictEqual(text, expected, 'the dialog does not show the sentence that was pinned');
    assert.strictEqual(word, require('../../src/status').FORCE_CLAIM_CONFIRMATION);
  });

  /*
   * AND WALKING AWAY DOES NOTHING AT ALL. Not "nothing visible": no
   * token, no import, no move. This is the outcome a user reaches by
   * pressing escape, which is to say the commonest one.
   */
  it('does nothing when the confirmation is declined', async () => {
    const storage = scratch();
    withQueue(storage, 'S-norecord', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-norecord', 'force-take-over'], [false]);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'cancelled' });
    chooser.asked(2, 1);
    const left = fs
      .readdirSync(path.join(storage, 'sessions'))
      .filter((name) => name.includes('.claim.'));
    assert.deepStrictEqual(left, [], 'a declined takeover left a claim token');
    assert.strictEqual(chooser.said.length, 0, 'a declined takeover reported something');
  });

  it('does nothing when the window is closed at the list', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder([]);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'cancelled' });
    chooser.asked(1, 0);
  });

  it('does nothing when the window is closed at the action', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-dead']);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'cancelled' });
    chooser.asked(2, 0);
  });

  /*
   * AND THE ORDINARY TAKEOVER ACTUALLY MOVES THE WORK. Winning the token
   * is not the takeover: the entries have to arrive in this window's
   * queue or nothing was rescued.
   */
  it('moves the other window’s unsent requests into this one', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1', 'r2']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const box = destination();
    const outcome = await chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), box.into);
    const landed = box.held;
    assert.strictEqual(box.runs, 1, 'the import did not go through the lock the destination owns');
    assert.strictEqual(outcome.did, 'take-over');
    assert.strictEqual(outcome.did === 'take-over' && outcome.ledger.imported, 2);
    assert.strictEqual(outcome.did === 'take-over' && outcome.ledger.skippedDuplicate, 0);
    assert.deepStrictEqual(landed.map((e) => e.req).sort(), ['r1', 'r2']);
  });

  /*
   * AND A REFUSAL IS REPORTED BY NAME. "It did not work" is not
   * something anybody can act on; "that window is still running" is.
   */
  it('says why when the window turns out to be running', async () => {
    const storage = scratch();
    const alive = await idleProcess();
    try {
      makeSession(storage, 'S-alive', { pid: alive.pid, startedAt: systemStartTime(alive.pid) });
      withQueue(storage, 'S-alive', ['r1']);
      const sessions = new Sessions(new RecordingFs(), storage);
      sessions.begin('S-mine', []);
      const chooser = new Recorder(['S-alive', 'discard'], [true]);
      const outcome = await chooseAndRecover(sessions, chooser, nowhere());
      assert.strictEqual(outcome.did, 'refused');
      assert.match(chooser.said[0].text, /still running/);
      assert.ok(
        fs.existsSync(path.join(storage, 'sessions', 'S-alive')),
        'a running window’s files were moved'
      );
    } finally {
      alive.stop();
    }
  });

  /*
   * AND A DISCARD CARRIES ITS TWO FIXED SENTENCES INTO THE
   * CONFIRMATION, not only into the answer afterwards. They are what the
   * user is deciding on: that the listing reflects the disk only, and
   * that requests already sent cannot be recalled from here.
   */
  it('confirms a discard with the two sentences the model fixes', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-dead', 'discard'], [true]);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.strictEqual(outcome.did, 'discard');
    const { DISCARD_BACKUP_NOTE, DISCARD_REACH_NOTE } = require('../../src/sessions');
    assert.ok(chooser.confirmations[0].text.includes(DISCARD_BACKUP_NOTE));
    assert.ok(chooser.confirmations[0].text.includes(DISCARD_REACH_NOTE));
    assert.ok(
      !fs.existsSync(path.join(storage, 'sessions', 'S-dead')),
      'the directory was not moved aside'
    );
    assert.match(chooser.said[0].text, /Nothing was deleted/);
  });

  it('does nothing when the discard is declined', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-dead', 'discard'], [false]);
    const outcome = await chooseAndRecover(sessions, chooser, nowhere());
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'cancelled' });
    chooser.asked(2, 1);
    assert.ok(fs.existsSync(path.join(storage, 'sessions', 'S-dead')), 'a declined discard moved files');
  });

  /*
   * AND WITH NOWHERE TO PUT THEM, THE USER IS TOLD. A takeover into a
   * window with no store configured wins the token and moves nothing;
   * reporting that as a takeover would be reporting a rescue that did
   * not happen.
   */
  it('says so when there is no queue here to move them into', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const chooser = new Recorder(['S-dead', 'take-over']);
    const outcome = await chooseAndRecover(sessions, chooser, null);
    /*
     * ⚠️ AND IT REFUSES BEFORE TAKING A TOKEN. Claiming first and then
     * finding nowhere to put the entries left this window holding a live
     * claim over work it had not moved, and the advice it then gave --
     * configure a store and run this again -- was refused by that very
     * claim. The way out was blocked by the attempt to use it.
     */
    assert.strictEqual(outcome.did, 'refused');
    assert.match(chooser.said[0].text, /no store is configured/);
    const tokens = fs
      .readdirSync(path.join(storage, 'sessions'))
      .filter((name) => name.includes('.claim.'));
    assert.deepStrictEqual(tokens, [], 'a takeover that moved nothing still took a token');
  });
});

/*
 * REVIEW ROUND 22: WHOSE STORE, AND WHOSE LOCK.
 *
 * Two defects with the same shape: a takeover that moves work somewhere
 * it was not meant to go.
 */
describe('review 22 a takeover moves one store’s work, through one lock', () => {
  /*
   * ⚠️ A WINDOW WRITES TO AS MANY STORES AS IT WAS CONFIGURED FOR, and
   * keeps a queue for each -- a queue carries one cursor and a cursor
   * belongs to one store. The import walked every one of them into a
   * single destination, so requests written for one store landed in
   * another window's queue for a DIFFERENT store, and its next drain
   * would have sent them there. Agreeing to rescue one store's unsent
   * work is not agreeing to redirect those writes.
   */
  it('imports only the queue belonging to the destination’s store', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['for-a']);
    /*
     * A SECOND STORE'S QUEUE IN THE SAME SESSION. `withQueue` writes
     * under 'h'; this one is under another store's name.
     */
    const other = path.join(storage, 'sessions', 'S-dead', 'other-store');
    fs.mkdirSync(other, { recursive: true });
    fs.writeFileSync(
      path.join(other, 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'for-b',
            cursor: 'w:1',
            id: 'a.9',
            field: 'src',
            payload: 'x',
            state: 'queued',
            createdAt: 0,
            lastError: null
          }
        ]
      }),
      'utf8'
    );

    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const box = destination('h');
    const outcome = await chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), box.into);
    assert.strictEqual(outcome.did, 'take-over', JSON.stringify(outcome));
    assert.deepStrictEqual(
      box.held.map((e) => e.req),
      ['for-a'],
      'a request written for another store was moved into this one’s queue'
    );
  });

  /*
   * AND THE IMPORT HAPPENS INSIDE THE SECTION THAT HOLDS SAVES OF THIS
   * WINDOW'S OWN QUEUE.
   *
   * ⚠️ THE FIRST VERSION OF THIS CELL SUPPLIED ITS OWN `run` AND WATCHED
   * ITS OWN FLAG. It passed with `Saver.adopt` replaced by an
   * implementation that threw -- it was testing the stand-in, not the
   * thing the stand-in stands for. A review found that. This one builds
   * a real `Saver`, starts a save that cannot answer yet, and requires
   * the import to wait for it: the property is "one at a time over one
   * queue file", and only a real `Saver` has it.
   */
  it('waits for a save already in flight over the same queue', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    withQueue(storage, 'S-dead', ['from-the-dead']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    /*
     * A CORE THAT ANSWERS THE SAVE ONLY WHEN THIS CELL LETS IT. Until
     * then the Saver holds its section, and anything else over that file
     * has to wait.
     */
    const core = new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: WROTE, rc: 0, delayMs: 400 }
    ]);
    try {
      const outbox = new Outbox(core.outboxFile());
      outbox.load();
      const saver = new Saver(
        new Client(new CliTransport(core.config(), core.env())),
        outbox,
        (req, cursor) => outbox.resolve(req, cursor)
      );
      const order: string[] = [];
      const saving = saver.save('a.2', 'src', 'body\n').then(() => {
        order.push('save');
      });
      const recovering = chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), {
        storeHash: 'h',
        run: (work) =>
          saver.adopt((into) => {
            order.push('import');
            return work(into);
          })
      }).then(() => undefined);
      await Promise.all([saving, recovering]);
      assert.deepStrictEqual(
        order,
        ['save', 'import'],
        'the import ran while a save over the same queue was still in flight'
      );
      assert.ok(
        outbox.entries.some((e) => e.req === 'from-the-dead'),
        'the imported request is not in this window’s queue'
      );
    } finally {
      core.dispose();
    }
  });

});

/*
 * REVIEW ROUND 23: ONE TAKEOVER NEED NOT FINISH THE JOB, AND MUST NOT
 * BLOCK THE REST.
 *
 * The token names a dead SESSION; that session keeps a queue per store.
 * Narrowing the import to the destination's store -- which stopped one
 * store's requests being sent to another -- made the first takeover take
 * a session-wide token and the second store's work unreachable behind
 * it, refused to this window by this window. Found in review.
 */
describe('review 23 a takeover that moved one store’s work can come back for the rest', () => {
  function queueAt(storage: string, sessionId: string, storeHash: string, reqs: string[]): void {
    const dir = path.join(storage, 'sessions', sessionId, storeHash);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(
      path.join(dir, 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: reqs.map((req) => ({
          req,
          cursor: 'w:1',
          id: 'a.1',
          field: 'src',
          payload: 'x',
          state: 'queued',
          createdAt: 0,
          lastError: null
        }))
      }),
      'utf8'
    );
  }

  it('takes the other store’s requests on a second run, and says they are waiting on the first', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a']);
    queueAt(storage, 'S-dead', 'store-b', ['for-b1', 'for-b2']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    const a = destination('store-a');
    const said = new Recorder(['S-dead', 'take-over']);
    const first = await chooseAndRecover(sessions, said, a.into);
    assert.strictEqual(first.did, 'take-over', JSON.stringify(first));
    assert.deepStrictEqual(a.held.map((e) => e.req), ['for-a']);
    /*
     * AND IT SAYS WHAT IT LEFT. Reporting only what arrived lets the
     * user believe the rescue was complete; two requests are still in
     * that window's directory.
     */
    assert.match(said.said[0].text, /2 belong to other stores/);

    /*
     * ⚠️ THE SECOND RUN, WITH THE OTHER STORE CONFIGURED. This used to
     * be refused as `already-claimed` -- by this window, to this window,
     * with no way round it.
     */
    const b = destination('store-b');
    const second = await chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), b.into);
    assert.strictEqual(
      second.did,
      'take-over',
      `the other store's work was unreachable behind this window's own claim: ${JSON.stringify(second)}`
    );
    assert.deepStrictEqual(b.held.map((e) => e.req).sort(), ['for-b1', 'for-b2']);
  });

  /*
   * AND THE GREEN TWIN'S OPPOSITE: another window's live claim is still
   * refused. Without it, re-entering is satisfied by a build that let
   * anybody take anybody's claim, which is the double-send the whole
   * token exists to prevent.
   */
  it('still refuses a takeover another running window holds', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a']);
    const holder = new Sessions(new RecordingFs(), storage);
    holder.begin('S-holder', []);
    assert.ok((await holder.claim('S-dead')).claimed);

    const mine = new Sessions(new RecordingFs(), storage);
    mine.begin('S-mine', []);
    const chooser = new Recorder(['S-dead', 'take-over']);
    const outcome = await chooseAndRecover(mine, chooser, destination('store-a').into);
    assert.strictEqual(outcome.did, 'refused', JSON.stringify(outcome));
    assert.match(chooser.said[0].text, /cannot take it from/);
  });
});
