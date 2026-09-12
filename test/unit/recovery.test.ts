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
import { Choice, Chooser, RecoveryAction, chooseAndRecover } from '../../src/recovery';
import { ImportTarget, SessionIdentity, Sessions, systemStartTime } from '../../src/sessions';
import { Notice } from '../../src/status';
import { OutboxEntry } from '../../src/outbox';
import { RecordingFs } from '../support/recording-fs';
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
  private readonly picks: unknown[];
  private readonly agrees: boolean[];

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
}

function nowhere(): ImportTarget {
  const held: OutboxEntry[] = [];
  return {
    has: (req) => held.some((e) => e.req === req),
    adopt: (entry) => {
      held.push(entry);
    }
  };
}

describe('U-recover the command that shows another window’s unsent work', () => {
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
    const chooser = new Recorder(['S-norecord', 'force-take-over'], [true]);
    await chooseAndRecover(sessions, chooser, nowhere());
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
    assert.strictEqual(chooser.confirmations.length, 0);
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
    assert.strictEqual(chooser.confirmations.length, 0);
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
    const landed: OutboxEntry[] = [];
    const into: ImportTarget = {
      has: (req) => landed.some((e) => e.req === req),
      adopt: (entry) => {
        landed.push(entry);
      }
    };
    const outcome = await chooseAndRecover(sessions, new Recorder(['S-dead', 'take-over']), into);
    assert.deepStrictEqual(outcome, {
      did: 'take-over',
      sessionId: 'S-dead',
      imported: 2,
      skipped: 0
    });
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
    assert.strictEqual(outcome.did, 'take-over');
    assert.match(chooser.said[0].text, /no store is configured/);
  });
});
