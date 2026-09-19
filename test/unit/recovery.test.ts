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
 * NOTE: THE CELLS DRIVE THE PRODUCT'S OWN ENTRY POINT. `chooseAndRecover`
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
import {
  Choice,
  Chooser,
  Destination,
  RecoveryAction,
  WindowQueue,
  chooseAndRecover,
  destinationFor
} from '../../src/recovery';
import { ImportTarget, SessionIdentity, Sessions, systemStartTime } from '../../src/sessions';
import { Notice } from '../../src/status';
import { OutboxEntry } from '../../src/outbox';
import { nodeFileOps } from '../../src/fsops';
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
   * NOTE: EVERY SCRIPTED ANSWER HAD TO BE ASKED FOR.
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
     * NOTE: THE CONFIRMATION ANSWERS TOO. `asked` checked only that the
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
 * A WINDOW QUEUE THAT ACCEPTS EVERYTHING AND REMEMBERS IT, standing in
 * for this window's own. `adopt` is where the lock would be, and calling
 * `work` inside it is exactly what the Saver does.
 *
 * NOTE: THE DESTINATION IS BUILT BY THE PRODUCT, `destinationFor`, and not
 * by hand here. A fixture that assembles its own destination is a
 * fixture that cannot see anything `destinationFor` decides -- and what
 * it decides is whether this is still the window's queue by the time the
 * lock is held. `queue.generation` is a field the cells move.
 */
function destination(storeHash = 'h'): {
  into: Destination;
  held: OutboxEntry[];
  runs: number;
  queue: WindowQueue;
} {
  const held: OutboxEntry[] = [];
  const box = {
    held,
    runs: 0,
    queue: {
      storeHash,
      adopt: async <T>(work: (into: ImportTarget) => T): Promise<T> => {
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
  const into = destinationFor(() => box.queue);
  assert.ok(into !== null, 'the product refused to build a destination over a queue that is there');
  /*
   * NOTE: THE SAME OBJECT, NOT A COPY OF IT. Returning `{ ...box, into }`
   * copied `runs` at the moment of return, so every later increment
   * landed on an object no cell was holding and the count read 0 for
   * ever -- caught immediately by the cell that asserts the import went
   * through the lock.
   */
  return Object.assign(box, { into });
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
   * NOTE: THE EXPENSIVE ACTION IS OFFERED ONLY WHERE IT IS THE ONLY WAY
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
   * NOTE: THE CONFIRMATION IS THE CONSTANT, NOT A SENTENCE THE UI WROTE.
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
     * NOTE: AND IT WENT THROUGH. A cell that only read the dialog's wording
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
     * NOTE: AND IT REFUSES BEFORE TAKING A TOKEN. Claiming first and then
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
   * NOTE: A WINDOW WRITES TO AS MANY STORES AS IT WAS CONFIGURED FOR, and
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
   * NOTE: THE FIRST VERSION OF THIS CELL SUPPLIED ITS OWN `run` AND WATCHED
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
        (req, settlement) =>
          settlement.verdict === 'confirmed' ? outbox.resolve(req, settlement.cursor) : undefined
      );
      const order: string[] = [];
      const saving = saver.save('a.2', 'src', 'body\n').then(() => {
        order.push('save');
      });
      const through = destinationFor(() => ({
        storeHash: 'h',
        adopt: (work) =>
          saver.adopt((into) => {
            order.push('import');
            return work(into);
          })
      }));
      assert.ok(through !== null, 'the product would not build a destination over a real Saver');
      const recovering = chooseAndRecover(
        sessions,
        new Recorder(['S-dead', 'take-over']),
        through
      ).then(() => undefined);
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
    assert.match(said.said[0].text, /2 were written for other stores and are still there/);

    /*
     * NOTE: THE SECOND RUN, WITH THE OTHER STORE CONFIGURED. This used to
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

/*
 * REVIEW ROUND 34: THE WINDOW THIS WAS DECIDED FOR MAY NOT BE THE WINDOW
 * IT LANDS IN.
 *
 * `adoptingInto` is evaluated when the command starts and the pickers
 * after it are awaits, so changing theourgia.store while the list is
 * open leaves the takeover holding the PREVIOUS queue. The entries went
 * into it, the report recommended a retry -- and the retry command reads
 * the CURRENT saver, which is a different queue. With the new store
 * empty the user was told nothing was waiting, over work that had just
 * been moved somewhere they were not looking. Traced in review against
 * the source.
 *
 * The ruling was to refuse rather than redirect: the survey that chose
 * these entries was made against the old store's hash, and carrying them
 * into another store's queue would be making a decision the user never
 * made.
 */
describe('review 34 a destination that has stopped being this window’s queue', () => {
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

  /*
   * A QUEUE WHOSE LOCK IS NOT FREE YET, so a cell can arrange the
   * settings change to land in the window the defect lives in: after the
   * destination was built and the import decided, and before the work
   * runs with the queue in hand.
   */
  function slowQueue(initial: string): {
    now: () => WindowQueue;
    held: OutboxEntry[];
    entered: Promise<void>;
    open(): void;
    storeHash: string;
    useStore(hash: string): void;
  } {
    /*
     * NOTE: EVERY CALL ANSWERS WITH A NEW OBJECT, because the extension's
     * does: `rebuild` builds a fresh one, and a reference taken earlier
     * then goes on describing the window that has been replaced.
     *
     * A stand-in that handed back ONE object and let the cells move its
     * fields could not tell those two apart -- reading the captured
     * reference and asking again inside the lock give the same answer,
     * whichever the product does. Both halves of that were measured on
     * the first version: a product that compared the live object
     * compared a value with itself and refused nothing, and a mutant
     * that moved the question outside the lock survived. The cells for a
     * check inside the lock cannot be written over a fixture where
     * inside and outside look alike.
     */
    const held: OutboxEntry[] = [];
    let announce: () => void = () => undefined;
    const entered = new Promise<void>((resolve) => {
      announce = resolve;
    });
    let release: () => void = () => undefined;
    const free = new Promise<void>((resolve) => {
      release = resolve;
    });
    const box = {
      held,
      entered,
      storeHash: initial,
      open: (): void => release(),
      useStore(hash: string): void {
        box.storeHash = hash;
      },
      now: (): WindowQueue => ({
        storeHash: box.storeHash,
        adopt: async <T>(work: (into: ImportTarget) => T): Promise<T> => {
          announce();
          await free;
          return work({
            has: (req: string) => held.some((e) => e.req === req),
            adopt: (entry: OutboxEntry) => {
              held.push(entry);
            }
          });
        }
      })
    };
    return box;
  }

  it('moves nothing, and says so, when the store changed while the user decided', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    const slow = slowQueue('store-a');
    const into = destinationFor(slow.now);
    assert.ok(into !== null);
    const chooser = new Recorder(['S-dead', 'take-over']);
    const recovering = chooseAndRecover(sessions, chooser, into);
    /*
     * NOTE: THE CHANGE LANDS WHILE THE LOCK IS BEING TAKEN. This is the
     * position that decides where the check has to live: a check made
     * before `adopt` has already answered about a moment that has passed
     * by the time anything is written. Moving it there leaves this cell
     * green and the defect alive.
     */
    await slow.entered;
    slow.useStore('store-b');
    slow.open();

    const outcome = await recovering;
    assert.strictEqual(outcome.did, 'refused', JSON.stringify(outcome));
    assert.strictEqual(
      (outcome as { because: string }).because,
      'store-changed',
      JSON.stringify(outcome)
    );
    assert.deepStrictEqual(slow.held, [], 'entries went into a queue this window had left');
    assert.strictEqual(
      chooser.said[0].text,
      'S-dead was not taken over: the store this window writes to changed while you were ' +
        'deciding, so nothing was moved and nothing was lost. Run the command again to take it ' +
        'over for the store that is configured now.',
      `the refusal did not say what happened: ${chooser.said[0].text}`
    );
  });

  /*
   * NOTE: THE TWIN. Without it, a build that refuses EVERY takeover passes
   * the cell above -- and the same stand-in, with the generation left
   * alone, has to carry the entries.
   */
  it('imports as usual when the store did not change under it', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    const slow = slowQueue('store-a');
    const into = destinationFor(slow.now);
    assert.ok(into !== null);
    const chooser = new Recorder(['S-dead', 'take-over']);
    const recovering = chooseAndRecover(sessions, chooser, into);
    await slow.entered;
    slow.open();

    const outcome = await recovering;
    assert.strictEqual(outcome.did, 'take-over', JSON.stringify(outcome));
    assert.deepStrictEqual(slow.held.map((e) => e.req), ['for-a']);
    assert.ok(
      !/store this window writes to changed/.test(chooser.said[0].text),
      `an import that happened was reported as refused: ${chooser.said[0].text}`
    );
  });

  /*
   * NOTE: THE SETTINGS `rebuild` REACTS TO THAT DO NOT MOVE THE QUEUE --
   * READ FROM THE MANIFEST, NOT TYPED OUT HERE.
   *
   * `rebuild` runs for every theourgia setting and builds a new Saver
   * each time, but the queue is the file `<session>/<storeHash>/
   * outbox.json` and its lock is keyed by that path -- so with the store
   * unchanged the new Saver is a second object over the SAME queue, and
   * importing through the one this destination holds lands in exactly
   * the same place. Refusing there costs the user a run and tells them
   * the store changed, which is untrue.
   *
   * NOTE: AND THE LIST WAS TYPED OUT, AND WAS ALREADY WRONG. It said
   * actor/scheme/corePath/libDirs and the extension also has
   * `timeoutMs` and `transport` -- a list written to make sure a new
   * setting had somewhere it must be added, missing two on the day it
   * was written. A review found them. So the names come from
   * `package.json`, which is where a new setting really is added, and
   * the only thing stated here is which one moves the queue.
   *
   * The product cannot tell these apart and neither can this cell: what
   * reaches `destinationFor` is "rebuilt, store hash the same", once per
   * name. The row per name is what makes the LIST, not one example, the
   * thing being maintained.
   */
  const MOVES_THE_QUEUE = 'store';

  function settingsFromManifest(): string[] {
    const manifest = JSON.parse(
      fs.readFileSync(path.join(__dirname, '..', '..', '..', 'package.json'), 'utf8')
    ) as { contributes: { configuration: { properties: Record<string, unknown> } } };
    return Object.keys(manifest.contributes.configuration.properties)
      .filter((name) => name.startsWith('theourgia.'))
      .map((name) => name.slice('theourgia.'.length))
      .sort();
  }

  const REBUILT_WITHOUT_MOVING_THE_QUEUE = settingsFromManifest().filter(
    (name) => name !== MOVES_THE_QUEUE
  );

  /*
   * THE INSTRUMENT'S FIRST READING, again: a filter that matched nothing
   * would leave the loop below empty and this section would pass while
   * testing nothing at all.
   */
  it('reads the settings from the manifest, and the one that moves the queue is there', () => {
    const all = settingsFromManifest();
    assert.ok(all.includes(MOVES_THE_QUEUE), `theourgia.${MOVES_THE_QUEUE} is not a setting`);
    assert.ok(
      REBUILT_WITHOUT_MOVING_THE_QUEUE.length >= 5,
      `only ${REBUILT_WITHOUT_MOVING_THE_QUEUE.length} settings besides the store: ${all.join(', ')}`
    );
  });

  for (const setting of REBUILT_WITHOUT_MOVING_THE_QUEUE) {
    it(`imports when theourgia.${setting} changed under it and the store did not`, async () => {
      const storage = scratch();
      makeSession(storage, 'S-dead');
      queueAt(storage, 'S-dead', 'store-a', ['for-a']);
      const sessions = new Sessions(new RecordingFs(), storage);
      sessions.begin('S-mine', []);

      const slow = slowQueue('store-a');
      const into = destinationFor(slow.now);
      assert.ok(into !== null);
      const chooser = new Recorder(['S-dead', 'take-over']);
      const recovering = chooseAndRecover(sessions, chooser, into);
      await slow.entered;
      slow.open();

      const outcome = await recovering;
      assert.strictEqual(
        outcome.did,
        'take-over',
        `a takeover was refused although the store is still ${slow.storeHash}: ` +
          JSON.stringify(outcome)
      );
      assert.deepStrictEqual(slow.held.map((e) => e.req), ['for-a']);
      assert.ok(
        !/store this window writes to changed/.test(chooser.said[0].text),
        `changing theourgia.${setting} was reported as a change of store: ${chooser.said[0].text}`
      );
    });
  }

  /*
   * NOTE: AND A STORE THAT LEFT AND CAME BACK IS THE SAME QUEUE. A -> B ->
   * A while the user decides leaves the file, and the lock, exactly
   * where they were. A change counter calls that the worst case; the
   * queue's identity calls it no case at all.
   */
  it('imports when the store changed away and back again', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    const slow = slowQueue('store-a');
    const into = destinationFor(slow.now);
    assert.ok(into !== null);
    const recovering = chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), into);
    await slow.entered;
    slow.useStore('store-b');
    slow.useStore('store-a');
    slow.open();

    const outcome = await recovering;
    assert.strictEqual(outcome.did, 'take-over', JSON.stringify(outcome));
    assert.deepStrictEqual(slow.held.map((e) => e.req), ['for-a']);
  });

  /*
   * NOTE: AND THE REFUSAL SPENDS NOTHING.
   *
   * "Nothing was moved and nothing was lost" is a claim about the source
   * queue, the adoption marks and the claim sequence, and it is worth
   * only what a second run can show: running the command again -- with
   * the store that is configured NOW -- has to survey the whole session
   * again, take what belongs to the new store, and report the declined
   * store's entries as another store's, still waiting.
   */
  it('leaves the token re-enterable, and the second run reports the old store’s work', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', ['for-a1', 'for-a2']);
    queueAt(storage, 'S-dead', 'store-b', ['for-b']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);

    const slow = slowQueue('store-a');
    const into = destinationFor(slow.now);
    assert.ok(into !== null);
    const first = chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), into);
    await slow.entered;
    slow.useStore('store-b');
    slow.open();
    assert.strictEqual((await first).did, 'refused');

    const b = destination('store-b');
    const chooser = new Recorder(['S-dead', 'take-over']);
    const second = await chooseAndRecover(sessions, chooser, b.into);
    assert.strictEqual(second.did, 'take-over', JSON.stringify(second));
    assert.deepStrictEqual(b.held.map((e) => e.req), ['for-b'], 'the second run took the wrong work');
    assert.strictEqual(
      (second as { ledger: { leftOtherStore: number } }).ledger.leftOtherStore,
      2,
      'the declined store’s entries were not counted as another store’s'
    );
    assert.match(
      chooser.said[0].text,
      /2 were written for other stores and are still there/,
      `the second run did not say where the rest are: ${chooser.said[0].text}`
    );
  });
});

/*
 * plugin-r2: the recovery listing does not turn "I could not look" into
 * "there is nothing here".
 *
 * KEY: THE SENTENCE THIS DENIES IS THE ONE THAT MATTERS. "No other window
 * has left anything here" is what a person acts on by closing the
 * question -- and the unsent work of another window is exactly what it
 * would be denying the existence of. Both defects below were measured by
 * a review round and both repairs went in without a cell; a mutation run
 * confirmed that removing either left every cell in this tree green.
 */
describe('plugin-r2 a listing that could not be read is not an empty one', () => {
  before(async () => {
    await initWire();
  });

  /*
   * NOTE: A REAL FAILURE, not a stubbed one. A directory with no execute
   * permission cannot be listed, and `readdirSync` raises EACCES on it.
   * Running as root defeats that, so the cell says it could not set the
   * situation up rather than passing on a reading it did not take.
   */
  it('refuses a sessions directory it cannot read, rather than announcing there is none', () => {
    const storage = scratch();
    const root = path.join(storage, 'sessions');
    fs.mkdirSync(root, { recursive: true });
    fs.mkdirSync(path.join(root, 'S-dead'));
    fs.chmodSync(root, 0o000);
    try {
      let raised: unknown = null;
      try {
        nodeFileOps.list(root);
      } catch (e) {
        raised = e;
      }
      if (raised === null) {
        assert.fail(
          `${root} could be listed although it has no permissions, so this machine cannot ` +
            'produce the failure this cell is about'
        );
      }
      assert.notStrictEqual((raised as NodeJS.ErrnoException).code, 'ENOENT');
    } finally {
      fs.chmodSync(root, 0o700);
    }
  });

  /*
   * THE TWIN: a sessions directory that is NOT THERE really is empty, and
   * that is how every first run looks. Without it the cell above would
   * pass on a reader that refuses everything.
   */
  it('still reads an absent sessions directory as holding nothing', () => {
    assert.deepStrictEqual(nodeFileOps.list(path.join(scratch(), 'sessions')), []);
  });

  /*
   * KEY: AND A QUEUE THAT PARSED INTO THE WRONG SHAPE IS NOT AN EMPTY
   * ONE. The catch beside this line says exactly that about a queue that
   * will not parse; the counting line said the opposite about one that
   * parses into something unexpected, so the session vanished from the
   * listing altogether.
   */
  it('keeps a window whose queue holds no list of entries', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const dir = path.join(storage, 'sessions', 'S-dead', 'h');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'outbox.json'), '{"entries":"bad"}', 'utf8');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const rows = await sessions.others();
    assert.strictEqual(rows.length, 1, 'a window with an unreadable queue vanished from the list');
    assert.ok(
      rows[0].pendingEntries > 0,
      'a queue this build cannot read was counted as holding nothing'
    );
  });
});

/*
 * plugin-r2: a queue whose presence could not be established is offered,
 * not dropped.
 *
 * KEY: THE DROP HAPPENED BEFORE THE READER THAT WAS REPAIRED. `exists` is
 * `fs.existsSync`, which answers false for a path whose ancestry cannot
 * be searched exactly as it does for one that is not there -- so an
 * unreadable queue never reached the reader that refuses unreadable
 * queues. Measured in a fourteenth review round: the migration guard
 * then reported that no send was in flight. `presenceOf`, which tells
 * the two apart, had been sitting beside `exists` the whole time.
 */
describe('plugin-r2 a queue whose presence is unknown is still offered', () => {
  before(async () => {
    await initWire();
  });

  it('offers a queue it could not look at, so the reader downstream refuses it', () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const sessions = new Sessions(
      {
        ...nodeFileOps,
        /*
         * A PRESENCE THAT CANNOT BE ESTABLISHED, which is what an
         * unsearchable ancestry produces. The rest of the fixture is
         * the real filesystem.
         */
        presenceOf: () => ({ known: false as const }),
        list: () => ['h'],
        isDirectory: () => true
      },
      storage
    );
    const paths = sessions.outboxPathsFor('S-dead');
    /*
     * NEVER: `length >= 1` IS TRUE OF EITHER PATH ALONE.
     *
     * Measured in a fifteenth review round: with this stub, dropping
     * `out.push(legacy)` passed, and so did dropping `out.push(candidate)`.
     * There are two queues a window can hold -- the one written beside
     * the session and the one written under a writer's directory -- and
     * a cell that accepts either one accepts losing the other. Both are
     * named.
     */
    assert.deepStrictEqual(
      paths.map((file) => path.relative(storage, file)).sort(),
      [
        path.join('sessions', 'S-dead', 'h', 'outbox.json'),
        path.join('sessions', 'S-dead', 'outbox.json')
      ].sort(),
      'a queue whose presence could not be established was dropped before anything could refuse it'
    );
  });

  /*
   * THE TWIN: a queue that is genuinely absent is still not offered, or
   * every session in the listing would carry phantom work.
   */
  it('still leaves out a queue that is genuinely not there', () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const sessions = new Sessions(
      {
        ...nodeFileOps,
        presenceOf: () => ({ known: true as const, there: false }),
        list: () => ['h'],
        isDirectory: () => true
      },
      storage
    );
    assert.deepStrictEqual(sessions.outboxPathsFor('S-dead'), []);
  });
});
