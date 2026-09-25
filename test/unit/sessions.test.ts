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
 * C6, C8, C9, C10, C12, C13, C19: whose files these are, whether that
 * window is still running, and what a user may do about one that is not.
 *
 * THE LIVENESS ANSWER DECIDES ONE THING ONLY -- whether a takeover is
 * OFFERED. It never decides whether anything may be written, which is
 * why erring towards "alive" costs nothing but a takeover that is not
 * offered, and why erring the other way would double-send. (section 12.9)
 *
 * THE ORDER IS ESRCH FIRST, IDENTITY SECOND. A pid that is absent is
 * dead and needs no further question; a pid that is PRESENT may be a
 * reused one, so its start time is compared then. Reversing them makes
 * a dead session read as alive for ever -- which is what a reboot
 * leaves behind, so it is the common case and not the rare one.
 * (section 12.21.4, C19)
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
  SessionIdentity,
  Sessions,
  StartTimeReader,
  ImportTarget,
  TakeoverLedger,
  emptyLedger,
  ledgerTotal,
  systemStartTime
} from '../../src/sessions';
import { idleProcess } from '../support/host';
import { Outbox, OutboxEntry, Receipt } from '../../src/outbox';
import { Publisher } from '../../src/publication';
import { RecordingFs } from '../support/recording-fs';
import { receiptFor, strangersReceipt } from '../support/receipts';

/*
 * A CONTROL CHARACTER, named rather than typed, so that the source of
 * this file stays readable.
 */
/*
 * A DESTINATION THAT BEHAVES LIKE ONE: what it is given, it has.
 *
 * NOTE: THESE FIXTURES USED `{ has: () => false, adopt: () => undefined }`
 * -- a destination that accepts everything and keeps nothing. Under the
 * old code that counted as `imported`, so the fixtures were resting on
 * the very defect this batch then fixed: the import now asks whether the
 * entry is really there before marking the source as having handed it
 * over, and a stand-in that always answers no is not a destination.
 */
function keeping(): ImportTarget {
  const held: string[] = [];
  return {
    has: (req) => held.includes(req),
    adopt: (entry) => {
      held.push(entry.req);
      return receiptFor(entry);
    }
  };
}

const TAB = String.fromCharCode(9);
const NUL = String.fromCharCode(0);

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-sessions-'));
}

/*
 * WHICH WINDOW A CLAIM TOKEN NAMES, read the way `claim` reads it.
 *
 * NOTE: THE TOKEN HAS TWO LINES: the session id, and the nonce of the
 * incarnation that published it. The cells used to `trim()` the whole
 * file, which was the id when there was only one line and is now both.
 * A cell reading a file differently from the code that writes it is a
 * cell about a different file.
 */
function holderOf(token: string): string {
  return fs.readFileSync(token, 'utf8').split('\n')[0];
}

/*
 * A SESSION DIRECTORY THAT EXISTS, with the identity a cell wants it to
 * have. Several cells below are about what happens to ANOTHER session,
 * and a session that was never written to disk is not another session --
 * it is nothing, which is a different case with a different answer.
 */
function makeSession(storage: string, sessionId: string, over: Partial<SessionIdentity> = {}): void {
  const dir = path.join(storage, 'sessions', sessionId);
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, 'session.json'),
    JSON.stringify({ sessionId, pid: 999999, startedAt: 1000, nonce: 'n', stores: [], ...over }),
    'utf8'
  );
}

/*
 * THE REAL START TIME OF A REAL PROCESS.
 *
 * NOTE: These fixtures used `startedAt: null` to mean "this one is alive",
 * which worked only because the comparison answered `true` when nothing
 * was recorded -- the very defect a reviewer found. The cells were
 * therefore agreeing with it rather than catching it. A live session
 * now records the start time the platform actually reports.
 */
function liveIdentity(pid: number): { pid: number; startedAt: number | null } {
  return { pid, startedAt: systemStartTime(pid) };
}

function identity(over: Partial<SessionIdentity> = {}): SessionIdentity {
  return {
    sessionId: 'S-old',
    pid: 999999,
    startedAt: 1000,
    nonce: 'n',
    stores: ['/stores/one'],
    ...over
  };
}

describe('C8 and C19 whether another window is still running', () => {
  it('calls a pid that is not there dead, without needing the platform to say more', async () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    const answer = await sessions.livenessOf(identity({ pid: 999999 }));
    assert.deepStrictEqual(answer, { alive: false, because: 'pid-absent' });
  });

  it('calls a live pid whose start time matches alive', async () => {
    const idle = idleProcess();
    try {
      const sessions = new Sessions(new RecordingFs(), scratch());
      const started = await sessions.livenessOf(identity(liveIdentity(idle.pid)));
      assert.deepStrictEqual(started, { alive: true, because: 'identity-matches' });
    } finally {
      idle.stop();
    }
  });

  /*
   * THE IMPLEMENTATION C19 NAMES, and the one a reboot produces: the pid
   * is present, so `kill` says nothing is wrong -- but it belongs to
   * some unrelated program that started later. Only the start time
   * separates them, and a build that consulted it merely in the ESRCH
   * branch never reaches it here.
   */
  it('calls a live pid whose start time differs dead, because the pid was reused', async () => {
    const idle = idleProcess();
    try {
      const sessions = new Sessions(new RecordingFs(), scratch());
      const answer = await sessions.livenessOf(identity({ pid: idle.pid, startedAt: 1 }));
      assert.deepStrictEqual(
        answer,
        { alive: false, because: 'pid-reused' },
        'a reused pid read as the session that used to hold it'
      );
    } finally {
      idle.stop();
    }
  });

  /*
   * EPERM IS NOT AN ANSWER ON ITS OWN. It says the pid exists and is
   * someone else's -- which a reused pid also is. C19 asks for all three
   * outcomes under EPERM, so the three cells here differ only in what
   * the start time says.
   */
  it('does not take permission-denied as proof the session is alive', async () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    const answer = await sessions.livenessOf(identity({ pid: 1, startedAt: 1 }));
    assert.deepStrictEqual(
      answer,
      { alive: false, because: 'pid-reused' },
      'a process belonging to another user was taken as the session, without checking identity'
    );
  });

  it('calls permission-denied alive when the start time does match', async () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    const real = await sessions.livenessOf(identity(liveIdentity(1)));
    assert.ok('alive' in real && real.alive, 'pid 1 with a matching identity was not called alive');
    assert.strictEqual(
      'alive' in real && real.alive && real.because,
      'identity-matches-permission-denied',
      'the reason does not record that the answer came from another user’s process'
    );
  });

  /*
   * AND WHEN THE PLATFORM WILL NOT SAY. Not alive, not dead: a third
   * answer, which the caller renders as "this platform cannot tell" and
   * which withholds the takeover. Folding it into either of the other
   * two is how a build either stops offering recovery for ever or
   * double-sends. (section 12.21.4)
   */
  /*
   * AND A RECORD WITH NO START TIME IS THE SAME ANSWER. Nothing was
   * compared, so nothing was verified -- reporting `identity-matches`
   * there claims a check that did not happen.
   */
  /*
   * NOTE: AND IT SAYS WHICH OF THE TWO WAYS. A record carrying no start
   * time and a machine that could not read one were the same value; the
   * sentence for that value told the user to try again in a moment,
   * which is advice that cannot work for the first -- no later attempt
   * produces a number the record never had. Found in review.
   */
  it('cannot tell about a session whose record carries no start time, and says which', async () => {
    const idle = idleProcess();
    try {
      const sessions = new Sessions(new RecordingFs(), scratch());
      const answer = await sessions.livenessOf(identity({ pid: idle.pid, startedAt: null }));
      assert.deepStrictEqual(answer, { decidable: false, because: 'start-time-unrecorded' });
    } finally {
      idle.stop();
    }
  });

  it('answers that it cannot tell when the start time is unavailable', async () => {
    /*
     * THE PLATFORM IS THE PARAMETER. On a machine where `ps` works this
     * answer is otherwise unreachable, and an answer no cell can reach
     * is an answer nothing holds the build to -- which matters here
     * because it is the one that must not be folded into either of the
     * other two.
     */
    const noStartTime: StartTimeReader = () => null;
    const sessions = new Sessions(new RecordingFs(), scratch(), noStartTime);
    const answer = await sessions.livenessOf(identity({ pid: 1, startedAt: 1 }));
    assert.deepStrictEqual(answer, { decidable: false, because: 'start-time-unavailable' });
  });
});

describe('C8 taking over a dead session’s queue', () => {
  /*
   * NOTE: `begin` FIRST, AS ACTIVATION DOES. These fixtures used to claim
   * without it, which is a state production never reaches and which the
   * code now refuses by name: a window that never said who it is
   * published a token nobody -- including itself -- could identify, so
   * an attempt to rescue a queue stranded it.
   */
  it('picks the next sequence itself rather than being told one', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-taker', []);
    makeSession(storage, 'S-old');
    const first = await sessions.claim('S-old');
    assert.ok(first.claimed, `nothing was claimed: ${JSON.stringify(first)}`);
    assert.strictEqual(first.claimed && first.sequence, 1);
  });

  /*
   * NOTE: THE SECOND CLAIM IS A SECOND WINDOW'S. The fixture used to take
   * both claims from one `Sessions`, which is not what the name says and
   * is not the case the refusal exists for: `already-claimed` stops a
   * SECOND window sending what a first is still draining. One window
   * re-entering its own claim is the cell below.
   */
  it('refuses when the newest claimant is still running', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const first = new Sessions(new RecordingFs(), storage);
    first.begin('S-taker', []);
    assert.ok((await first.claim('S-old')).claimed);
    const second = new Sessions(new RecordingFs(), storage);
    second.begin('S-other', []);
    assert.deepStrictEqual(await second.claim('S-old'), {
      claimed: false,
      because: 'already-claimed'
    });
  });

  /*
   * NOTE: AND THE WINDOW THAT HOLDS A CLAIM RE-ENTERS IT.
   *
   * The token names a dead SESSION, and that session may have a queue
   * per store -- so one takeover cannot finish the job. Rescuing the
   * first store's requests took the token and the second store's were
   * then refused, by this window, to this window, for ever. The same
   * wall stood in front of every other way a takeover can stop halfway:
   * an import that threw after moving some entries, or one that found
   * the destination unreadable. Found in review, each reproduced.
   */
  it('lets the window that holds a claim take it up again', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-taker', []);
    const first = await sessions.claim('S-old');
    assert.ok(first.claimed, JSON.stringify(first));
    const again = await sessions.claim('S-old');
    assert.ok(again.claimed, `a window could not take up its own claim: ${JSON.stringify(again)}`);
    if (first.claimed && again.claimed) {
      /*
       * THE SAME TOKEN AND THE SAME NUMBER. A new number would say a new
       * generation had taken over, and the entries the first pass marked
       * would then look as though a different claimant had carried them.
       */
      assert.strictEqual(again.sequence, first.sequence);
      assert.strictEqual(again.token, first.token);
    }
    /*
     * COUNTED THE WAY `claim` COUNTS. A `.tmp-` name is the half of a
     * publication that was written before the link, and `claim` skips
     * those when it looks for the newest token -- a cell using a
     * different rule would be measuring something the product does not.
     */
    const tokens = fs
      .readdirSync(path.join(storage, 'sessions'))
      .filter((name) => name.startsWith('S-old.claim.') && !name.includes('.tmp-'));
    assert.strictEqual(tokens.length, 1, `re-entering minted a second token: ${tokens.join(', ')}`);
  });

  it('refuses to offer a takeover of a session that is alive', async () => {
    const idle = idleProcess();
    try {
      const storage = scratch();
      makeSession(storage, 'S-alive', liveIdentity(idle.pid));
      const sessions = new Sessions(new RecordingFs(), storage);
      sessions.begin('S-taker', []);
      const answer = await sessions.claim('S-alive');
      assert.deepStrictEqual(answer, { claimed: false, because: 'session-alive' });
    } finally {
      idle.stop();
    }
  });

  /*
   * DEDUPLICATION IS BY REQUEST, NOT BY CONTENT. C19 names the
   * implementation this kills: two saves of the same text are two
   * legitimate requests and must both be applied, while one request
   * carried across several generations of takeover must be applied once.
   */
  /*
   * WITH ENTRIES THAT ACTUALLY EXIST.
   *
   * NOTE: The first version created NO entries and asserted
   * `imported >= 0`, which is true of every number this can return -- a
   * cell about deduplication with nothing to deduplicate.
   *
   * The dead queue holds three: two carrying the SAME request (one
   * request that was retried) and one carrying a DIFFERENT request with
   * the SAME payload. Deduplication is by request, so two arrive -- an
   * implementation comparing payloads would drop a legitimate save.
   */
  it('skips an entry whose request is already here, and keeps one that only looks alike', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    makeSession(storage, 'S-old');
    sessions.begin('S-mine', []);

    const dead = new Outbox(sessions.outboxPathFor('S-old'));
    dead.load();
    dead.setCursor('w:7');
    const entry = (req: string, payload: string): OutboxEntry => ({
      req,
      cursor: 'w:7',
      id: 'a.2',
      field: 'src',
      payload,
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    dead.enqueue(entry('11111111-1111-1111-1111-111111111111', 'same text\n'));
    dead.enqueue(entry('11111111-1111-1111-1111-111111111111', 'same text\n'));
    dead.enqueue(entry('22222222-2222-2222-2222-222222222222', 'same text\n'));

    const taken: OutboxEntry[] = [];
    const target = {
      has: (req: string) => taken.some((e) => e.req === req),
      adopt: (e: OutboxEntry) => {
        taken.push(e);
        return receiptFor(e);
      }
    };

    const token = await sessions.claim('S-old');
    assert.ok(token.claimed, JSON.stringify(token));
    if (token.claimed) {
      const imported = sessions.importFrom(
        { deadSessionId: 'S-old', sequence: token.sequence, file: token.token },
        target
      );
      assert.strictEqual(imported.imported, 2, 'one request was carried twice, or a different one was dropped');
      assert.strictEqual(imported.skippedDuplicate, 1, 'the repeat of one request was not skipped');
      assert.deepStrictEqual(
        taken.map((e) => e.req).sort(),
        ['11111111-1111-1111-1111-111111111111', '22222222-2222-2222-2222-222222222222'],
        'deduplication was by payload, so a legitimate second save was lost'
      );
    }

    /*
     * AND THE SOURCE RECORDS WHICH TAKEOVER CARRIED THEM, so a later
     * generation can tell what to skip. The entries stay: they are that
     * window's only trace of what it was doing.
     */
    const after = new Outbox(sessions.outboxPathFor('S-old'));
    after.load();
    assert.strictEqual(after.entries.length, 3, 'the source queue was emptied');
    assert.ok(
      after.entries.some((e) => e.importedBy !== null),
      'the source does not record which takeover carried its entries'
    );
  });
});

describe('C9 adopting the documents another session left open', () => {
  it('creates the marker once and keeps the one that is there', () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const first = sessions.adopt('S-old');
    assert.ok(first.adopted, `nothing was adopted: ${JSON.stringify(first)}`);
    const again = sessions.adopt('S-old');
    assert.deepStrictEqual(
      again,
      { adopted: false, because: 'already-adopted-by-this-session' },
      'adopting twice replaced a create-once marker'
    );
  });

  it('says the directory is gone rather than making one', () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    const answer = sessions.adopt('S-never-existed');
    assert.deepStrictEqual(answer, { adopted: false, because: 'directory-gone' });
  });
});

describe('C10 nothing is deleted, and discarding is the user’s decision', () => {
  it('refuses to discard a session that is alive', async () => {
    const idle = idleProcess();
    try {
      const storage = scratch();
      makeSession(storage, 'S-alive', liveIdentity(idle.pid));
      const sessions = new Sessions(new RecordingFs(), storage);
      const answer = await sessions.discard('S-alive');
      assert.strictEqual(answer.discarded, false);
      assert.strictEqual(answer.discarded === false && answer.because, 'session-alive');
    } finally {
      idle.stop();
    }
  });

  it('refuses to discard a session it cannot judge', async () => {
    const storage = scratch();
    makeSession(storage, 'S-undecidable', { pid: 1, startedAt: 1 });
    const sessions = new Sessions(new RecordingFs(), storage, () => null);
    const answer = await sessions.discard('S-undecidable');
    assert.strictEqual(answer.discarded, false);
    assert.strictEqual(answer.discarded === false && answer.because, 'undecidable');
  });

  /*
   * TWO DISCARDS OF ONE SESSION MAKE TWO DIRECTORIES. A destination that
   * could collide is a destination that can overwrite, and the one
   * operation that moves anything must not be the one that loses
   * something. (section 12.23)
   */
  it('moves a dead session to a place that cannot already exist', async () => {
    const storage = scratch();
    const files = new RecordingFs();
    const sessions = new Sessions(files, storage);
    makeSession(storage, 'S-dead');
    const first = await sessions.discard('S-dead');
    makeSession(storage, 'S-dead');
    const second = await sessions.discard('S-dead');
    assert.ok(first.discarded && second.discarded);
    if (first.discarded && second.discarded) {
      assert.notStrictEqual(first.trash, second.trash, 'the second discard could overwrite the first');
    }
    assert.deepStrictEqual(files.touched('unlink'), [], 'discarding deleted something');
  });

  /*
   * WITH AN ADOPTER THAT EXISTS.
   *
   * NOTE: The first version created none and asserted the answer was an
   * array -- true of `[]`, which is what an implementation that never
   * looked would also return. The warning only means something when
   * there is something to warn about.
   */
  it('names the windows that still have documents open, for the confirmation to show', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead-with-adopters');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-still-open', []);
    const adopted = sessions.adopt('S-dead-with-adopters');
    assert.ok(adopted.adopted, `the adopter could not be recorded: ${JSON.stringify(adopted)}`);

    const answer = await sessions.discard('S-dead-with-adopters');
    assert.deepStrictEqual(
      answer.liveAdopters,
      ['S-still-open'],
      'the confirmation does not name the window that still has documents open'
    );
  });

  /*
   * AND A DEAD ADOPTER IS NOT A WARNING. The twin: without it the cell
   * above passes for an implementation that lists every marker it finds,
   * which would warn about windows that closed months ago.
   */
  it('does not warn about an adopter that is no longer running', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead-with-adopters');
    makeSession(storage, 'S-long-gone', { pid: 999999, startedAt: 1000 });
    const sessions = new Sessions(new RecordingFs(), storage);
    fs.writeFileSync(
      path.join(storage, 'sessions', 'S-dead-with-adopters', 'adopted-by.S-long-gone'),
      'S-long-gone',
      'utf8'
    );
    const answer = await sessions.discard('S-dead-with-adopters');
    assert.deepStrictEqual(answer.liveAdopters, [], 'a window that has exited was reported as still open');
  });
});

describe('C6 and C12 what is a draft, decided without the queue', () => {
  /*
   * THE QUEUE IS NOT CONSULTED. A save that completed and was never sent
   * -- the shape a host killed at the wrong moment leaves -- has no
   * outbox entry at all, and a scan that started from the queue would
   * report nothing to recover. (section 12.17.4, C6)
   */
  it('finds a file whose bytes the store never acknowledged', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const directory = sessions.directoryFor('S-mine', 'st', 'a.2');
    const published = await new Publisher(new RecordingFs(), { isOpen: () => false }).publish({
      directory,
      storeId: 's1',
      blockId: 'a.2',
      prefix: '## Two\n',
      text: '## Two\nfrom the store\n',
      cursor: null
    });
    assert.ok(published.published);
    /*
     * A VERSION AS PUBLISHED IS NOT A DRAFT -- it holds what the store
     * gave. Editing it makes one, and that is the state a window killed
     * after a save but before it was sent leaves behind.
     */
    assert.deepStrictEqual(sessions.draftsIn('S-mine'), [], 'a freshly published version was listed as a draft');
    if (published.published) {
      fs.writeFileSync(published.file, '## Two\nedited and never sent\n', 'utf8');
      assert.deepStrictEqual(
        sessions.draftsIn('S-mine'),
        [published.file],
        'an edit the store never saw was not listed'
      );
    }
  });

  /*
   * AND A BASELINE `reconcile` BUILT IS ALWAYS A DRAFT, whatever the
   * digests say: the store has not seen it, because it was established
   * from the file rather than from an answer. (section 12.19.2, C12)
   */
  it('counts a local-only version as a draft even when its digests agree', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const directory = sessions.directoryFor('S-mine', 'st', 'a.2');
    const publisher = new Publisher(new RecordingFs(), { isOpen: () => false });
    const published = await publisher.publish({
      directory,
      storeId: 's1',
      blockId: 'a.2',
      prefix: '',
      text: 'no heading at all\n',
      cursor: null
    });
    assert.ok(published.published);
    if (published.published) {
      /*
       * `reconcile` builds the baseline from the file, so the store has
       * not seen it -- the digests agree with each other and with
       * nothing the store said.
       */
      publisher.reconcile(published.file, '', 'no heading at all\n');
      assert.deepStrictEqual(
        sessions.draftsIn('S-mine'),
        [published.file],
        'a baseline built from the file was treated as one the store had confirmed'
      );
    }
  });
});

describe('C13 a takeover only ever happens because someone asked for one', () => {
  /*
   * THE IMPLEMENTATION C13 NAMES: a startup sweep that imports a dead
   * session's entries on its own. It would pass C8 -- the token works,
   * the entries arrive -- and it would send another window's saves
   * without anybody deciding to.
   */
  /*
   * NOTE: AND THERE IS SOMETHING TO TAKE. (queue item 4) The cell used to
   * start up in an empty directory, so a startup sweep had nothing to sweep
   * and passed it. A dead session with a queued request is laid down first;
   * after startup its queue file must be exactly as it was.
   */
  it('imports nothing and sends nothing when a session merely starts up', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const deadQueue = path.join(storage, 'sessions', 'S-dead', 'store-a', 'outbox.json');
    fs.mkdirSync(path.dirname(deadQueue), { recursive: true });
    fs.writeFileSync(
      deadQueue,
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'r1',
            cursor: 'w:1',
            id: 'a.1',
            field: 'src',
            payload: 'another window\'s unsent work',
            state: 'queued',
            createdAt: 0,
            lastError: null,
            importedBy: null
          }
        ]
      }),
      'utf8'
    );
    const before = fs.readFileSync(deadQueue, 'utf8');
    const files = new RecordingFs();
    const sessions = new Sessions(files, storage);
    sessions.begin('S-new', ['/stores/one']);
    assert.ok(fs.existsSync(deadQueue), 'starting up moved the dead session\'s queue away');
    assert.strictEqual(fs.readFileSync(deadQueue, 'utf8'), before, 'starting up changed the dead session\'s queue');
    const others = await sessions.others();
    assert.ok(Array.isArray(others), 'starting up did not even list the other sessions');
    /*
     * NOTE: NOTHING OF ANYBODY ELSE'S MOVED. This asserted that nothing was
     * renamed at all, which stopped being the question when `begin`
     * started publishing its own record through a temporary file: that
     * rename is this window writing down who it is, and it is the one
     * move a startup is supposed to make. What the cell is about is that
     * startup takes nothing over -- so it names the paths instead of
     * counting the operation.
     */
    for (const moved of files.touched('rename')) {
      assert.ok(
        moved.includes(path.join('sessions', 'S-new')),
        `starting up moved ${moved}, which is not its own record`
      );
    }
    assert.deepStrictEqual(files.touched('link'), [], 'starting up published a claim token');
  });
});

/*
 * C8, continued: the token has to survive the ways a takeover can be
 * interrupted.
 */
describe('C8 the claim token is complete when it is visible', () => {
  /*
   * A TEMPORARY FILE IS NOT A CLAIM. The token is published by writing
   * a temporary file and linking it into place, so a `.tmp` left by a
   * window that died mid-write must read as no claim at all -- otherwise
   * one interrupted takeover strands the queue for ever. (section 12.13.5)
   */
  it('does not read a leftover temporary file as a claim', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions'), { recursive: true });
    makeSession(storage, 'S-old');
    fs.writeFileSync(path.join(storage, 'sessions', 'S-old.claim.1.tmp-abc'), 'S-other', 'utf8');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const answer = await sessions.claim('S-old');
    assert.ok(
      answer.claimed,
      `a half-written token was taken for a claim: ${JSON.stringify(answer)}`
    );
  });

  it('lets exactly one of two windows take the same sequence', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const one = new Sessions(new RecordingFs(), storage);
    one.begin('S-one', []);
    const two = new Sessions(new RecordingFs(), storage);
    two.begin('S-two', []);
    const [a, b] = await Promise.all([one.claim('S-old'), two.claim('S-old')]);
    const won = [a, b].filter((r) => r.claimed).length;
    assert.strictEqual(won, 1, `${won} windows took the same claim`);
  });

  /*
   * AND A TAKEOVER THAT DIED HALFWAY IS NOT THE END OF THE QUEUE. The
   * next window takes the NEXT number and carries on; entries already
   * copied are skipped by request id, so nothing is applied twice and
   * nothing is stranded. (section 12.11.3, C8)
   */
  it('lets the next window carry on with the next sequence', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const a = new Sessions(new RecordingFs(), storage);
    a.begin('S-a', []);
    const first = await a.claim('S-old');
    assert.ok(first.claimed, JSON.stringify(first));
    /*
     * AND THEN THAT WINDOW DIES. `begin` recorded this process, which is
     * very much alive, so the death has to be written down -- otherwise
     * the cell is about a claimant that is still running, which is the
     * case the cell above already covers.
     */
    makeSession(storage, 'S-a', { pid: 999999, startedAt: 1000 });
    const b = new Sessions(new RecordingFs(), storage);
    b.begin('S-b', []);
    const second = await b.claim('S-old');
    assert.ok(
      second.claimed,
      'the queue was stranded because the window that claimed it died'
    );
    if (first.claimed && second.claimed) {
      assert.strictEqual(second.sequence, first.sequence + 1);
    }
  });
});

/*
 * X1c (8): WHAT IS IN THE TOKEN, not only that one exists.
 *
 * The cells above count tokens and compare sequence numbers, and every
 * one of them passes whatever the file contains. But the CONTENT is what
 * the next window reads to decide whether the holder is still running:
 * `claim` reads the token, judges that name's liveness, and refuses with
 * `already-claimed` when it cannot show the holder is gone. A token
 * naming the wrong window -- or nothing -- makes that judgement about
 * somebody else, and the two wrong answers are both bad: a name that
 * reads as dead strands nothing and double-sends, a name that reads as
 * alive strands the queue for ever.
 */
describe('X1c ⑧ the claim token names the window that took it', () => {
  it('writes the claiming session’s own id into the token', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const answer = await sessions.claim('S-old');
    assert.ok(answer.claimed, JSON.stringify(answer));
    if (answer.claimed) {
      assert.strictEqual(
        holderOf(answer.token),
        'S-mine',
        'the token does not say which window holds the claim'
      );
    }
  });

  /*
   * AND THE NAME IS READ BACK BY THE MECHANISM THAT DEPENDS ON IT. This
   * is the half a content check alone cannot give: a second, LIVE window
   * holding the token must make a third window refuse. Reading the file
   * proves what was written; this proves the written name is what the
   * refusal is computed from.
   */
  it('refuses a second takeover because the token’s holder is still running', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const holder = new Sessions(new RecordingFs(), storage);
    holder.begin('S-holder', []);
    const won = await holder.claim('S-old');
    assert.ok(won.claimed);

    const next = new Sessions(new RecordingFs(), storage);
    next.begin('S-next', []);
    assert.deepStrictEqual(
      await next.claim('S-old'),
      { claimed: false, because: 'already-claimed' },
      'a queue whose holder is still running was taken over a second time'
    );
  });

  /*
   * THE GREEN TWIN OF THAT REFUSAL: when the token names a window that
   * is GONE, the next one carries on. Without this the cell above is
   * satisfied by a build that refuses every second claim regardless of
   * what the token says, which is the stranded queue the sequence
   * numbers exist to prevent.
   */
  it('carries on when the token names a window that has died', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const holder = new Sessions(new RecordingFs(), storage);
    holder.begin('S-holder', []);
    assert.ok((await holder.claim('S-old')).claimed);
    makeSession(storage, 'S-holder', { pid: 999999, startedAt: 1000 });

    const next = new Sessions(new RecordingFs(), storage);
    next.begin('S-next', []);
    const second = await next.claim('S-old');
    assert.ok(second.claimed, 'the queue was stranded although its holder is gone');
    if (second.claimed) {
      assert.strictEqual(
        holderOf(second.token),
        'S-next',
        'the new token still names the window that died'
      );
    }
  });
});

describe('C10 what the confirmation says, and what discarding touches', () => {
  it('carries both fixed sentences with the answer', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const sessions = new Sessions(new RecordingFs(), storage);
    const answer = await sessions.discard('S-dead');
    assert.ok(
      answer.notes.some((n) => n.includes('backups')),
      'the confirmation does not mention the editor’s own backups'
    );
    assert.ok(
      answer.notes.some((n) => n.includes('cannot be recalled')),
      'the confirmation does not say that sent requests cannot be taken back'
    );
  });

  /*
   * C10's retained listing/claim/discard scope. Current-body replacement
   * and explicit migration have their own XC operation traces; this cell
   * does not impose the retired global ban on renaming published files.
   */
  it('keeps block files intact during session listing, claims and whole-session discard', async () => {
    const storage = scratch();
    const files = new RecordingFs();
    const sessions = new Sessions(files, storage);
    sessions.begin('S-mine', ['/stores/one']);
    await sessions.others();
    sessions.draftsIn('S-mine');
    sessions.adopt('S-old');
    await sessions.claim('S-old');
    await sessions.discard('S-dead');

    assert.deepStrictEqual(files.touched('unlink'), [], 'something was deleted');
    for (const renamed of files.touched('rename')) {
      assert.ok(
        !renamed.endsWith('.md') && !renamed.endsWith('.meta'),
        `${renamed} was renamed; only whole session directories may move`
      );
    }
  });
});

describe('C20 adopting a directory that is discarded underneath it', () => {
  /*
   * THE IMPLEMENTATION C20 NAMES: one that creates the directory when
   * the marker cannot be linked. Adoption is not a reason for a
   * discarded session's directory to come back -- the user discarded it,
   * and re-creating it would put a marker beside files that are now in
   * the trash. The document is marked "save this somewhere else"
   * instead.
   */
  it('reports the directory gone rather than making it again', () => {
    const storage = scratch();
    const files = new RecordingFs();
    const sessions = new Sessions(files, storage);
    const answer = sessions.adopt('S-discarded-between-look-and-link');
    assert.deepStrictEqual(answer, { adopted: false, because: 'directory-gone' });
    assert.ok(
      !fs.existsSync(path.join(storage, 'sessions', 'S-discarded-between-look-and-link')),
      'adopting re-created a directory the user had discarded'
    );
  });
});

/*
 * P1-4: a record that will not read is not a death certificate.
 *
 * NOTE: THE FIX HAD NO CELL UNTIL A MUTATION SURVIVED. Collapsing
 * "unreadable" back into "no such session" left every existing cell
 * green, because every existing cell wrote a well-formed record. A fix
 * verified only by the reasoning that produced it is not guarded.
 */
/*
 * X1c (5): A DIRECTORY WITH NO RECORD IN IT IS NOT A DEAD SESSION EITHER.
 *
 * `discard` already refused this: a directory with no `session.json` is
 * something to lose with nothing to judge it by. `claim` did not, and
 * the asymmetry ran the wrong way -- a refused discard leaves files on
 * disk, while a claim that should have been refused produces a SECOND
 * sender for requests the first window is still holding.
 *
 * THE GAP IS ON THE PATH EVERY WINDOW TAKES, not an exotic one:
 * `begin` makes the directory and then writes the record, so a window
 * starting up is in exactly this state for as long as that takes.
 */
describe('X1c ⑤ a session directory with no record is not a dead session', () => {
  it('refuses to take over a directory that holds no record at all', async () => {
    const storage = scratch();
    /*
     * A QUEUE IS IN IT, so the refusal cannot be explained away as "there
     * was nothing there anyway" -- this is the shape where claiming
     * costs something.
     */
    fs.mkdirSync(path.join(storage, 'sessions', 'S-starting', 'h'), { recursive: true });
    fs.writeFileSync(
      path.join(storage, 'sessions', 'S-starting', 'h', 'outbox.json'),
      JSON.stringify({ cursor: null, entries: [{ req: 'r1', blockId: 'a.1', field: 'src', text: 'x' }] }),
      'utf8'
    );
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    assert.deepStrictEqual(
      await sessions.claim('S-starting'),
      { claimed: false, because: 'undecidable' },
      'a window that had made its directory and not yet written its record was taken for dead'
    );
  });

  it('writes no token when it refuses, so the next attempt is not told it is claimed', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'S-starting'), { recursive: true });
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    await sessions.claim('S-starting');
    const left = fs
      .readdirSync(path.join(storage, 'sessions'))
      .filter((name) => name.startsWith('S-starting.claim.'));
    assert.deepStrictEqual(left, [], 'a refused claim left a token behind');
  });

  /*
   * AND THE OTHER HALF: a session id nothing on this disk has ever seen
   * is `not-found`, the word `discard` uses for it, rather than a token
   * naming a session that does not exist.
   */
  it('answers not-found for a session that was never here', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    assert.deepStrictEqual(await sessions.claim('S-never'), { claimed: false, because: 'not-found' });
  });

  /*
   * THE GREEN TWIN. A rule that refuses is only worth having if the
   * thing it is meant to allow still goes through: a dead session WITH a
   * record is claimed, on the same disk, by the same code.
   */
  it('still takes over a dead session that did write a record', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const answer = await sessions.claim('S-old');
    assert.strictEqual(answer.claimed, true, 'the refusal took the ordinary takeover with it');
  });
});

describe('a session whose record cannot be read is not treated as gone', () => {
  for (const [what, contents] of [
    ['half-written', '{"sessionId":"S-broken","pid":'],
    ['empty', ''],
    ['without a pid', '{"sessionId":"S-broken"}']
  ] as Array<[string, string]>) {
    it(`refuses to take over a session whose record is ${what}`, async () => {
      const storage = scratch();
      fs.mkdirSync(path.join(storage, 'sessions', 'S-broken'), { recursive: true });
      fs.writeFileSync(path.join(storage, 'sessions', 'S-broken', 'session.json'), contents, 'utf8');
      const sessions = new Sessions(new RecordingFs(), storage);
      sessions.begin('S-mine', []);
      assert.deepStrictEqual(
        await sessions.claim('S-broken'),
        { claimed: false, because: 'undecidable' },
        'a window whose record could not be read was taken for one that never existed'
      );
    });

    it(`refuses to discard a session whose record is ${what}`, async () => {
      const storage = scratch();
      fs.mkdirSync(path.join(storage, 'sessions', 'S-broken'), { recursive: true });
      fs.writeFileSync(path.join(storage, 'sessions', 'S-broken', 'session.json'), contents, 'utf8');
      const sessions = new Sessions(new RecordingFs(), storage);
      const answer = await sessions.discard('S-broken');
      assert.strictEqual(answer.discarded, false);
      assert.strictEqual(answer.discarded === false && answer.because, 'undecidable');
      assert.ok(
        fs.existsSync(path.join(storage, 'sessions', 'S-broken')),
        'a session nobody could judge was moved to the trash'
      );
    });
  }
});

/*
 * U-claim: THE WAY OUT OF THE ONE UNDECIDABLE THAT WAITING CANNOT FIX.
 *
 * A directory with a queue in it and no `session.json` beside it cannot
 * be judged: nothing on disk distinguishes a window that died before it
 * wrote its record from one that is alive and has not written it yet.
 * `claim` refuses -- correctly -- and a review pointed out that it would
 * refuse for ever: no sequence of ordinary claims can change that
 * answer, so that queue is stranded permanently.
 *
 * The ruling: keep the refusal, and add an explicit forced takeover the
 * user asks for, having been told what it costs. It is open ONLY for a
 * missing record. A start time this machine could not obtain is a
 * failure to observe rather than a failure of evidence -- the same
 * question may answer itself a second later, so offering to duplicate a
 * request over it would be paying the cost for nothing.
 */
describe('U-claim taking over a window that left no record', () => {
  /*
   * A DIRECTORY WITH A QUEUE AND NO RECORD. The queue matters: without
   * it the refusal costs nothing and the way out is about nothing.
   */
  function recordless(storage: string, id: string): void {
    fs.mkdirSync(path.join(storage, 'sessions', id, 'h'), { recursive: true });
    fs.writeFileSync(
      path.join(storage, 'sessions', id, 'h', 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: [{ req: 'r1', cursor: 'w:1', id: 'a.1', field: 'src', payload: 'x', state: 'queued', createdAt: 0, lastError: null }]
      }),
      'utf8'
    );
  }

  it('refuses an ordinary claim, as it did before', async () => {
    const storage = scratch();
    recordless(storage, 'S-norecord');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    assert.deepStrictEqual(await sessions.claim('S-norecord'), {
      claimed: false,
      because: 'undecidable'
    });
  });

  it('takes it over when the user asks for it explicitly', async () => {
    const storage = scratch();
    recordless(storage, 'S-norecord');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const forced = await sessions.claim('S-norecord', true);
    assert.strictEqual(forced.claimed, true, JSON.stringify(forced));
    if (forced.claimed) {
      assert.strictEqual(
        holderOf(forced.token),
        'S-mine',
        'the forced token does not say who holds it'
      );
    }
  });

  /*
   * NOTE: AND FORCING IS NOT A WAY PAST THE OTHER REFUSALS. A window that
   * is provably ALIVE is still refused; so is one whose start time this
   * machine could not obtain, which is the case that fixes itself.
   */
  it('still refuses a window that is running, asked for explicitly or not', async () => {
    const storage = scratch();
    const alive = await idleProcess();
    try {
      makeSession(storage, 'S-alive', liveIdentity(alive.pid));
      const sessions = new Sessions(new RecordingFs(), storage);
      sessions.begin('S-mine', []);
      assert.deepStrictEqual(await sessions.claim('S-alive', true), {
        claimed: false,
        because: 'session-alive'
      });
    } finally {
      alive.stop();
    }
  });

  it('still refuses when the start time could not be obtained', async () => {
    const storage = scratch();
    const alive = await idleProcess();
    try {
      makeSession(storage, 'S-unknown', { pid: alive.pid, startedAt: 1000 });
      /*
       * A READER THAT CANNOT ANSWER. The pid is present, so liveness
       * turns on the start time, and this machine cannot supply it.
       */
      const sessions = new Sessions(new RecordingFs(), storage, () => null);
      sessions.begin('S-mine', []);
      assert.deepStrictEqual(await sessions.claim('S-unknown', true), {
        claimed: false,
        because: 'undecidable'
      });
    } finally {
      alive.stop();
    }
  });

  /*
   * AND THE LISTING IS WHERE THE USER FINDS IT. This row used to be
   * SKIPPED -- `others` read the record and moved on -- so the one state
   * being refused was invisible in the only listing there is. A refusal
   * about something nobody can see is not one anybody can act on.
   */
  it('lists the directory it refuses, and says an explicit takeover is open for it', async () => {
    const storage = scratch();
    recordless(storage, 'S-norecord');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const listed = await sessions.others();
    const row = listed.find((o) => o.sessionId === 'S-norecord');
    assert.ok(row !== undefined, 'the directory being refused is not in the listing at all');
    assert.strictEqual(row?.identity, null, 'an identity was invented for a window that left none');
    assert.deepStrictEqual(row?.liveness, { decidable: false, because: 'record-missing' });
    assert.strictEqual(row?.forceable, true);
    assert.strictEqual(row?.pendingEntries, 1, 'the listing does not say what is at stake');
  });

  it('does not offer it for a record that will not parse', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'S-broken'), { recursive: true });
    fs.writeFileSync(path.join(storage, 'sessions', 'S-broken', 'session.json'), '{"pid":', 'utf8');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const row = (await sessions.others()).find((o) => o.sessionId === 'S-broken');
    assert.ok(row !== undefined, 'a record that will not parse is not listed either');
    assert.deepStrictEqual(row?.liveness, { decidable: false, because: 'record-unreadable' });
    assert.strictEqual(row?.forceable, false, 'forcing was offered for a record nobody has read');
    assert.deepStrictEqual(await sessions.claim('S-broken', true), {
      claimed: false,
      because: 'undecidable'
    });
  });

  /*
   * AND A WINDOW THAT IS SIMPLY DEAD IS STILL LISTED AS SUCH, with no
   * forced entry -- the ordinary takeover already works for it, and
   * offering the expensive one beside it would teach the user to press
   * the expensive one.
   */
  it('does not offer it for a window that can be judged dead', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const row = (await sessions.others()).find((o) => o.sessionId === 'S-dead');
    assert.strictEqual(row?.forceable, false);
    assert.strictEqual((await sessions.claim('S-dead')).claimed, true);
  });
});

/*
 * REVIEW ROUND 21: THE EXCEPTION IS FOR EVIDENCE THAT CANNOT BE HAD, NOT
 * FOR A LOOK THAT FAILED.
 *
 * A forced takeover is open only where no amount of waiting produces the
 * record. A directory this process may not search reports every path
 * inside it as absent -- so a window that is ALIVE, with its record
 * sitting right there, read as "left no record" and was offered for
 * takeover. Waiting does fix that one; so does a chmod. Found in review.
 */
describe('review 21 a look that failed is not a missing record', () => {
  /*
   * THE FAILURE IS INJECTED THROUGH THE FILE OPERATIONS rather than by
   * changing permissions on disk, because what is being tested is the
   * decision and not the platform: the shipped adapter turns an
   * unsearchable directory into exactly this pair of answers.
   */
  function unsearchable(directory: string): RecordingFs {
    return new (class extends RecordingFs {
      public exists(file: string): boolean {
        return file.startsWith(directory) && file !== directory ? false : super.exists(file);
      }

      public readDirectory(
        target: string
      ): { read: true; names: string[] } | { read: false; because: 'absent' | 'unreadable' } {
        return target === directory ? { read: false, because: 'unreadable' } : super.readDirectory(target);
      }
    })();
  }

  it('does not offer a takeover for a directory it could not search', async () => {
    const storage = scratch();
    makeSession(storage, 'S-locked');
    const directory = path.join(storage, 'sessions', 'S-locked');
    const sessions = new Sessions(unsearchable(directory), storage);
    sessions.begin('S-mine', []);
    const row = (await sessions.others()).find((o) => o.sessionId === 'S-locked');
    assert.ok(row !== undefined, 'the directory vanished from the listing entirely');
    assert.deepStrictEqual(
      row?.liveness,
      { decidable: false, because: 'record-unreadable' },
      'a directory this process cannot search was read as a window that left no record'
    );
    assert.strictEqual(row?.forceable, false);
    assert.deepStrictEqual(await sessions.claim('S-locked', true), {
      claimed: false,
      because: 'undecidable'
    });
  });

  /*
   * AND THE GREEN TWIN: a directory this process CAN search, with no
   * record in it, is still the case the exception exists for.
   */
  it('still offers it for a directory it can search that holds no record', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'S-norecord', 'h'), { recursive: true });
    fs.writeFileSync(
      path.join(storage, 'sessions', 'S-norecord', 'h', 'outbox.json'),
      JSON.stringify({ cursor: null, entries: [{ req: 'r1', cursor: 'w:1', id: 'a.1', field: 'src', payload: 'x', state: 'queued', createdAt: 0, lastError: null }] }),
      'utf8'
    );
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const row = (await sessions.others()).find((o) => o.sessionId === 'S-norecord');
    assert.strictEqual(row?.forceable, true, 'the case the exception exists for stopped working');
  });

  /*
   * AND A DIRECTORY WITH NOTHING IN IT IS NOT A WINDOW. Every directory
   * under `sessions/` was becoming a row, so anything left there was
   * offered an explicit takeover -- over nothing. Offering the expensive
   * action where there is nothing at stake teaches the user to press it.
   */
  it('does not list an empty directory as a window nobody can judge', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'junk'), { recursive: true });
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    assert.strictEqual(
      (await sessions.others()).find((o) => o.sessionId === 'junk'),
      undefined,
      'a directory holding nothing was offered for takeover'
    );
  });

  /*
   * BUT A RECORD THAT WILL NOT PARSE IS STILL A WINDOW SAYING IT WAS
   * HERE, and that row is the only trace of it.
   */
  it('lists a directory whose record will not parse even when it holds nothing else', async () => {
    const storage = scratch();
    fs.mkdirSync(path.join(storage, 'sessions', 'S-broken2'), { recursive: true });
    fs.writeFileSync(path.join(storage, 'sessions', 'S-broken2', 'session.json'), '{', 'utf8');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const row = (await sessions.others()).find((o) => o.sessionId === 'S-broken2');
    assert.ok(row !== undefined, 'the only trace of that window is not in the listing');
    assert.strictEqual(row?.forceable, false);
  });
});

/*
 * REVIEW ROUND 24: A NAME IS NOT AN IDENTITY.
 *
 * Re-entering one's own claim compared the id written in the token with
 * this window's id, after trimming the file. A window called `"S "`
 * therefore published a token that read back as `"S"`, and a DIFFERENT
 * window called `"S"` re-entered its claim and sent what the first was
 * still draining -- the double-send the token exists to prevent.
 * Reproduced in review. A window that died and was replaced by one with
 * the same id was accepted too, and inherited a generation number that
 * then described two claimants.
 *
 * The token carries the nonce `begin` made, which identifies the
 * incarnation. Sharing a name is no longer sharing a claim.
 */
describe('review 24 re-entering a claim is about the window, not its name', () => {
  it('refuses a window whose id only looks like the holder’s', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    /*
     * THE HOLDER'S ID CARRIES A TRAILING SPACE, which the trimmed
     * comparison erased. It is alive, so a second window must be
     * refused; the point is which refusal it gets and why.
     */
    const holder = new Sessions(new RecordingFs(), storage);
    holder.begin('S-taker ', []);
    assert.ok((await holder.claim('S-old')).claimed);

    const lookalike = new Sessions(new RecordingFs(), storage);
    lookalike.begin('S-taker', []);
    const answer = await lookalike.claim('S-old');
    assert.strictEqual(
      answer.claimed,
      false,
      'a window re-entered a claim that belonged to another window with a similar name'
    );
  });

  /*
   * NOTE: AND A REPLACEMENT WITH THE SAME ID INHERITS NOTHING -- it is
   * REFUSED, and that is the honest answer rather than a convenient one.
   *
   * The expectation here was written as "it takes over by the ordinary
   * route, with the next number", and the code said otherwise: the
   * token's holder is looked up by NAME, and that name now belongs to a
   * window which is alive, so the claim reads as held by a running
   * window. Nothing on disk distinguishes "I am the window that
   * published this" from "I have its name", which is the whole point of
   * the nonce -- and when the nonce does not match, the safe answer is
   * to refuse.
   *
   * IT IS NOT A STRANDING: the name is held by a live window, and when
   * that one stops, the next takes the next number by the ordinary
   * route.
   */
  it('refuses a later window that merely took the same id', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const first = new Sessions(new RecordingFs(), storage);
    first.begin('S-same', []);
    const won = await first.claim('S-old');
    assert.ok(won.claimed);
    /*
     * THE PUBLISHER DIES AND A NEW WINDOW TAKES ITS NAME.
     */
    makeSession(storage, 'S-same', { pid: 999999, startedAt: 1000 });
    const second = new Sessions(new RecordingFs(), storage);
    second.begin('S-same', []);
    const again = await second.claim('S-old');
    assert.deepStrictEqual(
      again,
      { claimed: false, because: 'already-claimed' },
      'a different incarnation was let into a claim on the strength of sharing a name'
    );
  });

  /*
   * AND THE GREEN TWIN: the window that really did publish the token
   * still re-enters it. Without this the two cells above are satisfied
   * by a build that never re-enters, which is the stranding round 23
   * removed.
   */
  it('still lets the very window that published the token take it up again', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-taker', []);
    const first = await sessions.claim('S-old');
    const again = await sessions.claim('S-old');
    assert.ok(again.claimed, JSON.stringify(again));
    if (first.claimed && again.claimed) {
      assert.strictEqual(again.sequence, first.sequence);
    }
  });
});

/*
 * REVIEW ROUND 24: WHAT A TAKEOVER LEFT, COUNTED HONESTLY.
 */
describe('review 24 what a takeover reports as still waiting', () => {
  function queueAt(storage: string, id: string, where: string, entries: unknown[]): void {
    const dir = where === '' ? path.join(storage, 'sessions', id) : path.join(storage, 'sessions', id, where);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'outbox.json'), JSON.stringify({ cursor: null, entries }), 'utf8');
  }

  function entry(req: string, importedBy: string | null = null): unknown {
    return {
      req,
      cursor: 'w:1',
      id: 'a.1',
      field: 'src',
      payload: 'x',
      state: 'queued',
      createdAt: 0,
      lastError: null,
      importedBy
    };
  }

  it('does not count a request an earlier takeover already carried away', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', [entry('gone', 'S-dead.claim.1')]);
    queueAt(storage, 'S-dead', 'store-b', [entry('waiting')]);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed);
    const moved = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          keeping(),
          'store-b'
        )
      : null;
    assert.strictEqual(
      moved?.leftOtherStore,
      0,
      'the user was told to go back for a request an earlier run had already brought across'
    );
    assert.strictEqual(moved?.imported, 1);
  });

  /*
   * NOTE: AND A QUEUE NOBODY CAN READ IS NOT AN EMPTY ONE. Counting it as
   * zero reported somebody's unsent work as nothing left behind.
   */
  it('says a queue could not be read rather than counting it as empty', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', [entry('r1')]);
    fs.mkdirSync(path.join(storage, 'sessions', 'S-dead', 'store-x'), { recursive: true });
    fs.writeFileSync(path.join(storage, 'sessions', 'S-dead', 'store-x', 'outbox.json'), '{ not json', 'utf8');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    const moved = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          keeping(),
          'store-a'
        )
      : null;
    assert.strictEqual(moved?.unreadableQueue, 1, 'an unreadable queue was reported as nothing left');
    assert.strictEqual(moved?.leftOtherStore, 0);
  });

  /*
   * AND A QUEUE FROM BEFORE STORES HAD DIRECTORIES IS COUNTED APART,
   * because no configuration reaches it and "configure that store" is an
   * instruction the user cannot carry out.
   */
  it('counts a queue whose store nothing can establish apart from the rest', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueAt(storage, 'S-dead', 'store-a', [entry('r1')]);
    queueAt(storage, 'S-dead', '', [entry('old1'), entry('old2')]);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    const moved = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          keeping(),
          'store-a'
        )
      : null;
    assert.strictEqual(moved?.leftUnknownStore, 2);
    assert.strictEqual(moved?.leftOtherStore, 0, 'the unroutable queue was counted as another store’s');
  });
});

/*
 * REVIEW ROUND 25: WHAT A QUEUE FILE CAN BE, AND WHAT A NAME CAN FRAME.
 */
describe('review 25 a queue nobody can trust is not an empty queue', () => {
  function badQueue(storage: string, id: string, where: string, text: string): void {
    const dir = path.join(storage, 'sessions', id, where);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'outbox.json'), text, 'utf8');
  }

  function good(storage: string, id: string, where: string): void {
    badQueue(
      storage,
      id,
      where,
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'r1',
            cursor: 'w:1',
            id: 'a.1',
            field: 'src',
            payload: 'x',
            state: 'queued',
            createdAt: 0,
            lastError: null,
            importedBy: null
          }
        ]
      })
    );
  }

  async function takeover(storage: string, store: string) {
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed, JSON.stringify(won));
    return won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          keeping(),
          store
        )
      : null;
  }

  /*
   * NOTE: A FILE HOLDING THE FOUR BYTES `null` PARSES. Asking it for a
   * field then threw -- outside the catch, out of `importFrom`, out of
   * the command -- so the user saw no answer at all where they should
   * have seen a takeover reporting an unreadable queue.
   */
  it('does not fall over on a queue file that parses to null', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    good(storage, 'S-dead', 'store-a');
    badQueue(storage, 'S-dead', 'store-x', 'null');
    const moved = await takeover(storage, 'store-a');
    assert.strictEqual(moved?.unreadableQueue, 1);
    assert.strictEqual(moved?.imported, 1);
  });

  /*
   * AND THE QUEUE IT WAS ASKED TO TAKE CAN FAIL TOO. Only the ones it
   * did not open were counted, so a takeover of the one queue it was
   * told to take reported `imported: 0` and nothing else -- which reads
   * as "there was nothing there" over a file full of unsent work.
   */
  it('says so when the queue it was told to take could not be read', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    badQueue(storage, 'S-dead', 'store-a', '{ not json');
    const moved = await takeover(storage, 'store-a');
    assert.strictEqual(moved?.imported, 0);
    assert.strictEqual(
      moved?.unreadableQueue,
      1,
      'the queue this takeover was about failed in silence'
    );
  });

  for (const [what, text] of [
    ['a version this build does not know', '{"version":99,"entries":[]}'],
    ['an entries field that is not a list', '{"entries":{}}']
  ] as Array<[string, string]>) {
    it(`counts a queue with ${what} as unreadable rather than empty`, async () => {
      const storage = scratch();
      makeSession(storage, 'S-dead');
      good(storage, 'S-dead', 'store-a');
      badQueue(storage, 'S-dead', 'store-x', text);
      const moved = await takeover(storage, 'store-a');
      assert.strictEqual(moved?.unreadableQueue, 1, `${what} was counted as an empty queue`);
      assert.strictEqual(moved?.leftOtherStore, 0);
    });
  }

  /*
   * NOTE: AND AN ELEMENT THAT IS NOT A REQUEST MAKES ITS FILE UNREADABLE --
   * because the loader says so, and the survey now asks the loader.
   *
   * An earlier version of this counted such an element on its own and
   * called the requests beside it "waiting". A review showed that the
   * two parsers disagreed: the loader REFUSES that file, so those
   * requests are not waiting on a configuration, they are waiting on a
   * repair, and the advice that went with "waiting" would have failed.
   * The count now describes what could actually be recovered.
   */
  for (const [what, element] of [
    ['an element that is not an object', 'null'],
    ['an element with no request id', '{"payload":"x"}']
  ] as Array<[string, string]>) {
    it(`counts a file holding ${what} as one this build cannot read`, async () => {
      const storage = scratch();
      makeSession(storage, 'S-dead');
      good(storage, 'S-dead', 'store-a');
      badQueue(
        storage,
        'S-dead',
        'store-x',
        `{"entries":[${element},{"req":"r2","importedBy":null}]}`
      );
      const moved = await takeover(storage, 'store-a');
      assert.strictEqual(moved?.unreadableQueue, 1, `${what} did not make its file unreadable`);
      assert.strictEqual(
        moved?.leftOtherStore,
        0,
        'a request in a file the loader refuses was reported as merely waiting'
      );
    });
  }

  /*
   * NOTE: AND ONLY A STRING IS A MARK. `Outbox` reads anything else as no
   * mark at all and WILL import that entry, so treating a `false` as
   * "already carried away" would leave a waiting request out of the
   * count of what is waiting.
   */
  it('counts a request whose mark is not a string as still waiting', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    good(storage, 'S-dead', 'store-a');
    badQueue(
      storage,
      'S-dead',
      'store-b',
      JSON.stringify({
        cursor: null,
        entries: [{ req: 'r9', cursor: 'w:1', id: 'a.1', field: 'src', payload: 'x', state: 'queued', createdAt: 0, lastError: null, importedBy: false }]
      })
    );
    const moved = await takeover(storage, 'store-a');
    assert.strictEqual(moved?.leftOtherStore, 1, 'a waiting request was counted as already carried away');
  });
});

/*
 * REVIEW ROUND 25: AN ID WITH A LINE BREAK IN IT FRAMES ITS OWN NONCE.
 */
describe('review 25 what a session id may be', () => {
  /*
   * NOTE: THE ATTACK THE REVIEW FOUND, IN ONE CELL. A claim token is the id
   * on one line and the nonce on the next. A window whose id IS
   * `"M\nb"` publishes a token that the window called `M` with nonce `b`
   * reads as its own -- and re-enters a claim it does not hold. Both
   * nonces are the real ones; nothing is forged. Production ids are
   * uuids and cannot do this, and nothing made that a requirement.
   */
  it('refuses an id that would frame a second line', () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    assert.throws(() => sessions.begin('M\nsomething', []), /line break/);
    assert.throws(() => sessions.begin('', []), /empty/);
  });

  /*
   * AND A WINDOW THAT NEVER SAID WHO IT IS CANNOT TAKE A CLAIM. It used
   * to publish a token reading `unnamed` with no nonce, which its own
   * next call could not re-enter and no other window could judge -- so
   * an attempt to rescue a queue stranded it.
   */
  it('refuses to claim before the window has recorded who it is', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    await assert.rejects(() => sessions.claim('S-old'), /before begin/);
    const left = fs.readdirSync(path.join(storage, 'sessions')).filter((n) => n.includes('.claim.'));
    assert.deepStrictEqual(left, [], 'a nameless window left a token behind');
  });

  /*
   * AND AN EMPTY STORE NAME IS NOT A STORE. `path.join(dir, '', 'x')` is
   * `dir/x`, so it silently resolved to the queue from before stores had
   * their own directories -- the one whose store nothing can establish.
   */
  it('refuses an empty store name rather than resolving it to the legacy queue', () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    assert.throws(() => sessions.outboxPathFor('S-old', ''), /may not be empty/);
    assert.ok(sessions.outboxPathFor('S-old').endsWith(path.join('S-old', 'outbox.json')));
  });
});

/*
 * KEY: THE CONSERVATION LAW: EVERYTHING A TAKEOVER SAW IS IN EXACTLY ONE
 * BUCKET.
 *
 * Three times in one function a count answered with a smaller, more
 * comfortable number than the truth -- an unreadable queue as zero, a
 * queue the loader rejects as empty, the selected queue failing in
 * silence. Each was fixed where it was found, and the next would have
 * been fixed the same way, one shape at a time, for ever. NEVER: A cell per
 * shape cannot catch a shape nobody has thought of.
 *
 * So the report is a ledger and this is its law. Anything that goes
 * uncounted from here on is a DIFFERENCE that goes red, rather than a
 * number that quietly shrinks -- including in a shape written after this
 * was, by somebody who never read it.
 */
describe('the takeover ledger accounts for everything it saw', () => {
  function write(storage: string, id: string, where: string, text: string): void {
    const dir = where === '' ? path.join(storage, 'sessions', id) : path.join(storage, 'sessions', id, where);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'outbox.json'), text, 'utf8');
  }

  function request(req: string, importedBy: unknown = null): string {
    return JSON.stringify({
      req,
      cursor: 'w:1',
      id: 'a.1',
      field: 'src',
      payload: 'x',
      state: 'queued',
      createdAt: 0,
      lastError: null,
      importedBy
    });
  }

  /*
   * ONE FIXTURE PER SHAPE, AND ALL OF THEM AT ONCE. Every bucket is
   * non-empty in this store, so the sum is not accidentally right
   * because most of the terms are zero -- which is how a conservation
   * check passes without conserving anything.
   */
  async function ledgerOf(storage: string, refuse = ''): Promise<TakeoverLedger> {
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed, JSON.stringify(won));
    const held: string[] = ['already-here'];
    return won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          {
            has: (req) => held.includes(req),
            adopt: (entry) => {
              /*
               * A DESTINATION THAT REFUSES ONE REQUEST AND KEEPS THE
               * REST. Something has to be able to throw here, or
               * `failedToMove` is a bucket no fixture can fill -- and a
               * conservation law whose sum is right because a term is
               * always zero is not being tested. It must also KEEP what
               * it takes, or the import is right to say nothing arrived.
               */
              if (entry.req === refuse) {
                throw new Error('this window will not take that one');
              }
              held.push(entry.req);
              return receiptFor(entry);
            }
          },
          'store-a'
        )
      : emptyLedger();
  }

  it('adds up, with every bucket carrying something', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    /*
     * THE QUEUE BEING TAKEN: one to import, one already in the
     * destination, one an earlier takeover marked.
     */
    write(
      storage,
      'S-dead',
      'store-a',
      `{"entries":[${request('fresh')},${request('already-here')},${request('carried', 'S-dead.claim.1')},${request('refused')}]}`
    );
    /* ANOTHER STORE'S, WAITING. */
    write(storage, 'S-dead', 'store-b', `{"entries":[${request('other')}]}`);
    /* THE ONE WHOSE STORE NOTHING CAN SAY. */
    write(storage, 'S-dead', '', `{"entries":[${request('old')}]}`);
    /* AND ONE NOBODY CAN READ. */
    write(storage, 'S-dead', 'store-x', 'null');

    const led = await ledgerOf(storage, 'refused');
    assert.strictEqual(
      ledgerTotal(led),
      led.observed,
      `the ledger does not add up: ${JSON.stringify(led)}`
    );
    /*
     * NOTE: AND THE SUM IS OF THE LEDGER'S OWN KEYS, not of a list written
     * here. `ledgerTotal` is a hand-written sum and the compiler will
     * not notice a missing term -- an earlier comment claimed it would,
     * and a review showed otherwise. This is what notices.
     */
    const byKey = Object.entries(led)
      .filter(([name]) => name !== 'observed')
      .reduce((total, [, count]) => total + (count as number), 0);
    assert.strictEqual(
      ledgerTotal(led),
      byKey,
      'ledgerTotal has forgotten a bucket the ledger carries'
    );
    /*
     * AND EVERY BUCKET IS CARRYING SOMETHING, so the equality above is
     * not the sum of mostly zeroes.
     */
    assert.ok(led.imported > 0, JSON.stringify(led));
    assert.ok(led.skippedDuplicate > 1, 'both kinds of "already carried" are not represented');
    assert.ok(led.leftOtherStore > 0, JSON.stringify(led));
    assert.ok(led.leftUnknownStore > 0, JSON.stringify(led));
    assert.ok(led.unreadableQueue > 0, JSON.stringify(led));
    assert.ok(led.unreadableQueue > 0, JSON.stringify(led));
    assert.ok(led.failedToMove > 0, JSON.stringify(led));
  });

  /*
   * AND IT ADDS UP FOR EACH SHAPE ON ITS OWN, so a failure names which
   * one rather than only that the total moved.
   *
   * NOTE: AND THE BUCKETS SAY WHERE EACH THING WENT, not only that they sum.
   * (queue item 4) Every one of these cells compared the total with
   * `observed` and nothing else, so an importer that observed and imported
   * nothing -- `() => emptyLedger()`, measured -- passed all sixteen with
   * 0 equal to 0. Each now states its buckets: the readable store's request
   * is imported, and a queue in a shape this build cannot read is counted as
   * unreadable. One of the sixteen cannot say more than zero and says so
   * below: an empty queue being taken is, correctly, a ledger of zeroes.
   */
  for (const [what, text] of [
    ['a file that will not parse', '{ not json'],
    ['a file that parses to null', 'null'],
    ['a version this build does not know', '{"version":99,"entries":[]}'],
    ['an entries field that is not a list', '{"entries":{}}'],
    ['an element that is not an object', '{"entries":[null]}'],
    ['an element with no request id', '{"entries":[{"payload":"x"}]}'],
    ['a mark that is not a string', `{"entries":[${JSON.stringify({ req: 'r', importedBy: false })}]}`],
    ['an empty queue', '{"entries":[]}']
  ] as Array<[string, string]>) {
    it(`adds up when another store's queue is ${what}`, async () => {
      const storage = scratch();
      makeSession(storage, 'S-dead');
      write(storage, 'S-dead', 'store-a', `{"entries":[${request('fresh')}]}`);
      write(storage, 'S-dead', 'store-b', text);
      const led = await ledgerOf(storage);
      assert.strictEqual(
        ledgerTotal(led),
        led.observed,
        `${what} is not accounted for: ${JSON.stringify(led)}`
      );
      assert.strictEqual(led.imported, 1, `the readable store's request did not move: ${JSON.stringify(led)}`);
      assert.strictEqual(
        led.unreadableQueue,
        what === 'an empty queue' ? 0 : 1,
        `${what} was not counted where it belongs: ${JSON.stringify(led)}`
      );
    });

    it(`adds up when the queue being taken is ${what}`, async () => {
      const storage = scratch();
      makeSession(storage, 'S-dead');
      write(storage, 'S-dead', 'store-a', text);
      const led = await ledgerOf(storage);
      assert.strictEqual(
        ledgerTotal(led),
        led.observed,
        `${what} is not accounted for when it is the one being taken: ${JSON.stringify(led)}`
      );
      /*
       * NOTE: THE EMPTY QUEUE IS THE ONE SHAPE WHOSE RIGHT ANSWER IS ALL
       * ZEROES, and so the one this cell cannot tell from an importer that
       * did nothing. Stated rather than padded with an assertion that could
       * not fail.
       */
      assert.strictEqual(
        led.unreadableQueue,
        what === 'an empty queue' ? 0 : 1,
        `${what} was not counted as unreadable when it is the one being taken: ${JSON.stringify(led)}`
      );
    });
  }
});

/*
 * REVIEW ROUND 26: TWO THINGS THE LEDGER'S LAW DOES NOT CATCH BY ITSELF.
 *
 * Conservation says everything SEEN is in a bucket. It says nothing
 * about something never seen, and nothing about a window that believes
 * it has an identity nothing on disk agrees with.
 */
describe('review 26 what the takeover must still see, and who may take one', () => {
  /*
   * NOTE: A DIRECTORY THIS PROCESS CANNOT LIST STILL HAS THE QUEUE IN IT.
   * `list` answers `[]` for a directory it cannot read, so the selected
   * queue -- the one the takeover was asked about, which plainly exists
   * -- was never visited and never counted: an all-zero ledger over a
   * file full of unsent work. Conservation held perfectly, about
   * nothing.
   */
  it('counts the queue it was asked for even when the directory will not list', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const dir = path.join(storage, 'sessions', 'S-dead', 'store-a');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(
      path.join(dir, 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'r1',
            cursor: 'w:1',
            id: 'a.1',
            field: 'src',
            payload: 'x',
            state: 'queued',
            createdAt: 0,
            lastError: null,
            importedBy: null
          }
        ]
      }),
      'utf8'
    );
    /*
     * NOTE: THE ENUMERATION COMES BACK EMPTY AND THE EXISTENCE CHECK SAYS
     * NO, which is what an ancestry this process cannot search looks
     * like from here: `list` answers `[]` and `exists` answers false for
     * paths inside it. Only the directory itself can be seen, which is
     * what lets the claim happen at all.
     *
     * The first version of this cell left `exists` working, so it did
     * not reach the filter that was also dropping the named queue --
     * measured: the reversion survived it.
     */
    const blind = new (class extends RecordingFs {
      public list(directory: string): string[] {
        return directory.includes('S-dead') ? [] : super.list(directory);
      }

      public exists(file: string): boolean {
        return file.endsWith('outbox.json') && file.includes('S-dead')
          ? false
          : super.exists(file);
      }
    })();
    const sessions = new Sessions(blind, storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed, JSON.stringify(won));
    const led = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          keeping(),
          'store-a'
        )
      : emptyLedger();
    assert.strictEqual(
      led.imported,
      1,
      `the queue this takeover was asked about was never opened: ${JSON.stringify(led)}`
    );
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * NOTE: AND A WINDOW WHOSE IDENTITY WAS NEVER PUBLISHED HAS NOT BEGUN.
   * The two fields used to be set before the write, so a failure to
   * publish left the object believing it had an identity: `claim`
   * proceeded and published a token naming a window whose `session.json`
   * does not exist -- which nobody, including itself, can ever judge.
   */
  it('does not count itself as begun when its identity could not be written', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    /*
     * NOTE: THE OPERATION `begin` ACTUALLY USES. It wrote the record with
     * `writeText` when this cell was written and publishes it through a
     * temporary file now; a stand-in that refuses the operation the code
     * no longer calls refuses nothing, and the cell passes while
     * establishing nothing.
     */
    const refusing = new (class extends RecordingFs {
      public writeDurably(file: string, text: string): void {
        if (file.includes('session.json')) {
          throw new Error('the disk would not take it');
        }
        super.writeDurably(file, text);
      }
    })();
    const sessions = new Sessions(refusing, storage);
    assert.throws(() => sessions.begin('S-mine', []), /would not take it/);
    await assert.rejects(
      () => sessions.claim('S-old'),
      /before begin/,
      'a window whose identity was never published took a claim anyway'
    );
  });
});

/*
 * REVIEW ROUND 27: WHAT A FAILED MOVE ACTUALLY MEANS, AND WHERE A NAME
 * MAY POINT.
 */
describe('review 27 a request that arrived is not a request that did not', () => {
  function queueWith(storage: string, id: string, where: string, reqs: string[]): void {
    const dir = path.join(storage, 'sessions', id, where);
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
          lastError: null,
          importedBy: null
        }))
      }),
      'utf8'
    );
  }

  /*
   * NOTE: THE SOURCE MARK FAILS AFTER THE ENTRY HAS ARRIVED. Both failures
   * were one bucket, and its sentence said the requests "could not be
   * moved and are still in that window's queue" -- the opposite of the
   * truth, about work that was already here. A user acting on it goes
   * looking where it is not.
   */
  it('says a request arrived when only the bookkeeping failed', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueWith(storage, 'S-dead', 'store-a', ['r1']);
    const stubborn = new (class extends RecordingFs {
      public writeDurably(file: string, text: string): void {
        if (file.includes(path.join('S-dead', 'store-a'))) {
          throw new Error('the other window’s file will not take the mark');
        }
        super.writeDurably(file, text);
      }
    })();
    const sessions = new Sessions(stubborn, storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed);
    const landed: string[] = [];
    const led = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          {
            has: (req) => landed.includes(req),
            adopt: (entry) => {
              landed.push(entry.req);
              return receiptFor(entry);
            }
          },
          'store-a'
        )
      : emptyLedger();
    assert.deepStrictEqual(landed, ['r1'], 'the entry did not arrive, so this is the other case');
    assert.strictEqual(
      led.movedButUnmarked,
      1,
      `an entry that arrived was reported as one that did not: ${JSON.stringify(led)}`
    );
    assert.strictEqual(led.failedToMove, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * AND THE OTHER HALF: a destination that refuses the entry really is a
   * request that did not move.
   */
  it('says a request did not move when the destination refused it', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    queueWith(storage, 'S-dead', 'store-a', ['r1']);
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    const led = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          {
            has: () => false,
            adopt: () => {
              throw new Error('this window will not take it');
            }
          },
          'store-a'
        )
      : emptyLedger();
    assert.strictEqual(led.failedToMove, 1, JSON.stringify(led));
    assert.strictEqual(led.movedButUnmarked, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * NOTE: AND A STORE NAME MAY NOT LEAVE THE SESSION'S DIRECTORY. `..` in
   * one addressed a LIVE window's queue, and the takeover imported from
   * it and marked it as carried away under a claim on a different
   * session.
   */
  it('refuses a store name that points outside the session', () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    /*
     * NOTE: A BACKSLASH TOO. `path.basename` on POSIX does not treat it as
     * a separator, so `a\\b` is one component here and a path on
     * Windows -- and these directory names travel between windows. The
     * check asks both platforms' rules.
     */
    for (const bad of ['../live/a', 'a/b', '..', '.', 'a\\b', 'a\\..\\live']) {
      assert.throws(
        () => sessions.outboxPathFor('S-dead', bad),
        /path|empty|single directory/,
        `${bad} was accepted as a store name`
      );
    }
    assert.ok(sessions.outboxPathFor('S-dead', 'store-a').includes('store-a'));
  });

  /*
   * AND A SECOND `begin` CANNOT LEAVE HALF A RECORD BEHIND. It wrote
   * over the file in place, so a failure part-way left `session.json`
   * unreadable while the object kept the identity it already had -- and
   * went on claiming, over a record nobody could read.
   */
  it('leaves the old record intact when a second begin cannot be published', () => {
    const storage=scratch();let refuse=false;
    const files=new(class extends RecordingFs {
      public rename(from:string,to:string):void {
        if(refuse&&to.endsWith('session.json'))throw new Error('the rename would not go through');
        super.rename(from,to);
      }
    })();
    const sessions=new Sessions(files,storage);sessions.begin('S-mine',[]);
    const record=path.join(storage,'sessions','S-mine','session.json'),before=fs.readFileSync(record,'utf8');
    refuse=true;
    assert.throws(()=>sessions.begin('S-mine',['/stores/two']),/would not go through/);
    assert.strictEqual(fs.readFileSync(record,'utf8'),before);
  });
});

/*
 * REVIEW ROUND 28: WHERE AN ENTRY ENDED UP IS ASKED, NOT INFERRED.
 *
 * "It arrived" used to mean "`adopt` returned without throwing", which
 * is a different statement. A destination that stored the entry and then
 * threw was reported as one that refused it; one that returned without
 * storing anything was reported as having it. Both sentences were then
 * false, in opposite directions, about somebody's unsent work.
 */
describe('review 28 the destination is asked where the entry ended up', () => {
  function oneRequest(storage: string, id: string, where: string): void {
    const dir = path.join(storage, 'sessions', id, where);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(
      path.join(dir, 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'r1',
            cursor: 'w:1',
            id: 'a.1',
            field: 'src',
            payload: 'x',
            state: 'queued',
            createdAt: 0,
            lastError: null,
            importedBy: null
          }
        ]
      }),
      'utf8'
    );
  }

  async function ledgerWith(storage: string, into: ImportTarget): Promise<TakeoverLedger> {
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed, JSON.stringify(won));
    return won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          into,
          'store-a'
        )
      : emptyLedger();
  }

  /*
   * KEY: A RECEIPT MUST BE THIS ENTRY'S, NOT MERELY A REAL ONE. (queue item
   * 3, condition 3: the addressing cell lives on the import path, where
   * receipts multiply -- one per entry of the dead window's queue.) This
   * destination stores both entries but answers every `adopt` with the FIRST
   * receipt it ever issued: authentic, and about something else. The second
   * entry must be counted as not moved, and the source must go on offering
   * it rather than be marked as having handed it over.
   */
  it('counts an entry answered with another entry\'s receipt as not moved, and leaves it offered', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    const dir = path.join(storage, 'sessions', 'S-dead', 'store-a');
    fs.mkdirSync(dir, { recursive: true });
    const entry = (req: string): Record<string, unknown> => ({
      req,
      cursor: 'w:1',
      id: 'a.1',
      field: 'src',
      payload: req,
      state: 'queued',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    fs.writeFileSync(
      path.join(dir, 'outbox.json'),
      JSON.stringify({ cursor: null, entries: [entry('r1'), entry('r2')] }),
      'utf8'
    );
    const held: string[] = [];
    let first: Receipt | undefined;
    const led = await ledgerWith(storage, {
      has: (req) => held.includes(req),
      adopt: (taken) => {
        held.push(taken.req);
        const issued = receiptFor(taken);
        first = first ?? issued;
        return first;
      }
    });
    assert.deepStrictEqual(held, ['r1', 'r2'], 'the destination was not offered both entries');
    assert.strictEqual(led.imported, 1, `the stale receipt was believed: ${JSON.stringify(led)}`);
    assert.strictEqual(led.failedToMove, 1, JSON.stringify(led));
    assert.strictEqual(ledgerTotal(led), led.observed);
    const queue = JSON.parse(fs.readFileSync(path.join(dir, 'outbox.json'), 'utf8')) as {
      entries: Array<{ req: string; importedBy: unknown }>;
    };
    const marked = Object.fromEntries(queue.entries.map((e) => [e.req, e.importedBy !== null]));
    assert.deepStrictEqual(marked, { r1: true, r2: false }, 'the source was marked for an entry whose receipt was not its own');
  });

  /*
   * A DESTINATION THAT TOOK IT AND THEN THREW. It has the entry. The old
   * rule called that "could not be moved", and a user acting on it goes
   * looking for work that is already here.
   */
  it('says it arrived when the destination kept it and then failed', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    oneRequest(storage, 'S-dead', 'store-a');
    const held: string[] = [];
    const led = await ledgerWith(storage, {
      has: (req) => held.includes(req),
      adopt: (entry) => {
        held.push(entry.req);
        throw new Error('stored it, then fell over');
      }
    });
    assert.strictEqual(led.movedButUnmarked, 1, JSON.stringify(led));
    assert.strictEqual(led.failedToMove, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * AND ONE THAT RETURNED WITHOUT STORING ANYTHING. It does not have the
   * entry. The old rule called that "arrived here", and a user acting on
   * it may discard the only copy.
   */
  /*
   * NOTE: WHAT THIS ESTABLISHES, AND WHAT IT CANNOT. It is about the
   * classification AFTER something threw: the source mark is made to
   * fail, and the question is whether a destination that kept nothing is
   * then reported as having it. When NOTHING throws, a destination that
   * silently keeps nothing is reported as `imported` -- and no ledger
   * can know otherwise, because the only evidence is the destination's
   * own answer and it was never asked for one. Named in review.
   */
  it('says it did not move when the destination returned and kept nothing', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    oneRequest(storage, 'S-dead', 'store-a');
    /*
     * NOTE: A DESTINATION THAT ACCEPTS AND KEEPS NOTHING -- deliberately,
     * because that is the case. It used to be reported as `imported`,
     * and the source was then marked as having handed the request over:
     * the report wrong AND the other window's copy saying the work was
     * rescued. The import asks before it marks now, so this ends as a
     * request that did not move and the source is left offering it.
     *
     * NOTE: WRITTEN AS A RECEIPT FOR ANOTHER REQUEST since queue item 3.
     * `adopt` returns a receipt and one cannot be forged, so "kept
     * nothing" can no longer be `undefined`; what a destination can still
     * do is answer with a receipt that is not this entry's, and that is
     * what the import now reads.
     */
    const led = await ledgerWith(storage, {
      has: () => false,
      adopt: () => strangersReceipt()
    });
    assert.strictEqual(
      led.failedToMove,
      1,
      `a destination that kept nothing was reported as having it: ${JSON.stringify(led)}`
    );
    assert.strictEqual(led.imported, 0);
    assert.strictEqual(led.movedButUnmarked, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * AND THE SOURCE IS LEFT OFFERING IT. Marking a request as handed over
   * when the destination does not have it is the half that makes the
   * wrong report unrecoverable.
   */
  it('does not mark the source when the destination did not keep it', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    oneRequest(storage, 'S-dead', 'store-a');
    await ledgerWith(storage, { has: () => false, adopt: () => strangersReceipt() });
    const queue = JSON.parse(
      fs.readFileSync(path.join(storage, 'sessions', 'S-dead', 'store-a', 'outbox.json'), 'utf8')
    ) as { entries: Array<{ importedBy: unknown }> };
    assert.strictEqual(
      queue.entries[0].importedBy,
      null,
      'the source was told the request had been carried away by a window that does not have it'
    );
  });

  /*
   * AND WHEN THE DESTINATION CANNOT SAY, NEITHER DOES THIS. An unknown
   * drawn as one of the two comfortable answers is the shape this batch
   * has met more often than any other.
   */
  it('says it does not know when the destination cannot answer', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    oneRequest(storage, 'S-dead', 'store-a');
    let asked = 0;
    const led = await ledgerWith(storage, {
      has: () => {
        asked += 1;
        if (asked > 1) {
          throw new Error('this window cannot say');
        }
        return false;
      },
      adopt: () => {
        throw new Error('and the move fell over');
      }
    });
    assert.strictEqual(led.outcomeUnknown, 1, JSON.stringify(led));
    assert.strictEqual(led.failedToMove, 0);
    assert.strictEqual(led.movedButUnmarked, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });

  /*
   * AND A STORE NAME THE FILESYSTEM CANNOT HOLD IS REFUSED WHERE NAMES
   * ARRIVE. A NUL got as far as the read, where node refuses it, and the
   * takeover counted that as one more queue it could not inspect --
   * reporting a path that was never there.
   */
  it('refuses a store name carrying a control character', () => {
    const sessions = new Sessions(new RecordingFs(), scratch());
    /*
     * NOTE: NUL FIRST, because it is the one this guard was written for --
     * a check that rejected only tab and newline would pass this cell
     * while admitting the character that reached the read. Found in
     * review.
     */
    assert.throws(() => sessions.outboxPathFor('S-dead', `a${NUL}b`), /control character/);
    assert.throws(() => sessions.outboxPathFor('S-dead', `a${TAB}b`), /control character/);
    assert.throws(() => sessions.outboxPathFor('S-dead', 'a\nb'), /control character/);
    assert.ok(sessions.outboxPathFor('S-dead', 'store-a').includes('store-a'));
  });

  /*
   * AND A FAILED PUBLICATION TRIES TO LEAVE NOTHING BESIDE THE RECORD.
   * The temporary file was named after the process alone -- the same
   * name on every call -- and a failed rename left it there for ever.
   *
   * NOTE: "TRIES TO", NOT "DOES". The removal is best effort and can itself
   * fail, and the cells further down are about what is said when it
   * does. This comment said the stronger thing flatly, which is the
   * shape of claim this file exists to catch. Found in review.
   */
  /*
   * NOTE: AND A WRITE THAT THROWS AFTER TOUCHING THE FILE LEAVES NOTHING
   * EITHER. The write was outside the cleanup, so one that had already
   * created bytes left the temporary beside the record -- the same leak
   * the rename's cleanup was added for, a step earlier. The cell only
   * injected a rename failure and passed.
   *
   * NOTE: IT IS NOT A PARTIAL WRITE, and used to say it was. The stand-in
   * below completes `writeDurably` and then throws: what it establishes
   * is a failure AFTER a file exists, which is the state the cleanup has
   * to handle. Nothing here produces a half-written file, and naming one
   * would be describing a fixture that does not exist. Found in review.
   */
  it('clears up after itself when the identity cannot even be written', () => {
    const storage = scratch();
    const refusing = new (class extends RecordingFs {
      public writeDurably(file: string, text: string): void {
        if (file.includes('session.json')) {
          super.writeDurably(file, text);
          throw new Error('the write fell over after touching it');
        }
        super.writeDurably(file, text);
      }
    })();
    const sessions = new Sessions(refusing, storage);
    assert.throws(() => sessions.begin('S-mine', []), /fell over/);
    const left = fs
      .readdirSync(path.join(storage, 'sessions', 'S-mine'))
      .filter((name) => name.includes('.tmp'));
    assert.deepStrictEqual(left, [], `a failed write left ${left.join(', ')} behind`);
  });

  /*
   * AND THE TEMPORARY NAME IS NOT SHARED BY TWO WRITERS. It was
   * `<file>.<pid>.tmp` -- the same name on every call in one process --
   * so two of them would write over each other's half-made record.
   */
  it('gives each publication its own temporary name', () => {
    const storage = scratch();
    const names: string[] = [];
    const watching = new (class extends RecordingFs {
      public writeDurably(file: string, text: string): void {
        if (file.includes('session.json')) {
          names.push(file);
        }
        super.writeDurably(file, text);
      }
    })();
    const sessions = new Sessions(watching, storage);
    sessions.begin('S-mine', []);
    sessions.begin('S-mine', ['/stores/one']);
    assert.strictEqual(names.length, 2, 'the record was not published twice');
    assert.notStrictEqual(names[0], names[1], 'two publications shared one temporary name');
  });

  /*
   * NOTE: AND WHAT THE FAILURE SAYS ABOUT WHAT IT LEFT BEHIND.
   *
   * The publication's `catch` removes the temporary file, and when that
   * removal ALSO fails it adds a sentence naming the path. There was no
   * cell for the path where both fail, and an outside judge showed what
   * that allowed: replacing the whole sentence with an empty string left
   * the suite exactly as green as before. It was the third fix in this
   * batch held up by nothing but the reasoning that produced it.
   *
   * The four cells below are the four states, because the sentence is
   * wrong in three different ways if it is printed whenever `unlink`
   * throws: the commonest reason it throws is that the write failed
   * before creating anything.
   *
   * NOTE: AND THE STAND-IN SAYS WHETHER IT CREATED THE FILE, because that
   * is what decides which branch the cleanup takes. The first version of
   * these cells used one stand-in that threw BEFORE creating anything
   * for all four -- so the cell meant to exercise a SUCCESSFUL removal
   * was taking the `ENOENT` branch beside it, and the two cells were one
   * cell twice. A mutation adding a spurious note after a successful
   * unlink would have survived every one of them. Found in review.
   */
  class FailingWrite extends RecordingFs {
    public readonly temporaries: string[] = [];
    public readonly refusals: NodeJS.ErrnoException[] = [];
    private readonly leavesAFile: boolean;

    constructor(leavesAFile: boolean) {
      super();
      this.leavesAFile = leavesAFile;
    }

    public writeDurably(file: string, text: string): void {
      if (file.includes('session.json')) {
        this.temporaries.push(file);
        if (this.leavesAFile) {
          super.writeDurably(file, text);
        }
        const e = new Error('no room on the device') as NodeJS.ErrnoException;
        e.code = 'ENOSPC';
        this.refusals.push(e);
        throw e;
      }
      super.writeDurably(file, text);
    }

    /*
     * NOTE: THE ERROR OBJECT ITSELF, so a cell can compare identities. A
     * copy of it that kept `code` and the text would satisfy every
     * assertion about its contents, and a copy is exactly what the
     * defect under repair produced.
     */
    public get injected(): NodeJS.ErrnoException {
      assert.strictEqual(this.refusals.length, 1, 'the publication did not fail exactly once');
      return this.refusals[0];
    }

    /*
     * THE ONE PATH THE PUBLICATION WROTE, so the cells can compare the
     * path in the sentence with the path that was written rather than
     * with a pattern any `.tmp` name would satisfy.
     */
    public get temporary(): string {
      assert.strictEqual(this.temporaries.length, 1, 'the publication did not write exactly once');
      return this.temporaries[0];
    }
  }

  /*
   * A PUBLICATION THAT FAILED, AND THE ERROR IT FAILED WITH. Every cell
   * below asks for the same thing, and `assert.fail` rather than a flag
   * makes "it did not fail at all" a red rather than a skipped assertion.
   */
  function publicationFailure(files: FailingWrite, storage: string): NodeJS.ErrnoException {
    const sessions = new Sessions(files, storage);
    try {
      sessions.begin('S-mine', []);
    } catch (e) {
      return e as NodeJS.ErrnoException;
    }
    return assert.fail('the publication did not fail');
  }

  /*
   * WHAT IS TRUE OF EVERY PUBLICATION FAILURE, whichever way the cleanup
   * went. The shape used to depend on that -- the original error when
   * there was nothing to add and a wrapper when there was -- so
   * `err.code` worked or did not according to whether an unlink
   * succeeded. It is asserted in all four cells because a shape that
   * holds in three of them is not a contract.
   */
  function carriesTheReason(thrown: NodeJS.ErrnoException, injected: Error): void {
    assert.strictEqual(thrown.code, 'ENOSPC', 'the failure does not carry the original code');
    assert.strictEqual(
      (thrown as Error & { cause?: unknown }).cause,
      injected,
      'the original error is not the cause of the failure'
    );
    assert.ok(
      thrown.message.startsWith(injected.message),
      `the reason the publication failed was replaced: ${thrown.message}`
    );
  }

  it('names the leftover file when the cleanup could not remove one that is there', () => {
    const storage = scratch();
    /*
     * THE FILE IS REALLY THERE: the stand-in writes it and then throws,
     * and only the removal is refused. Nothing here has to pretend an
     * answer about presence -- the file system gives the true one.
     */
    const stubborn = new (class extends FailingWrite {
      constructor() {
        super(true);
      }

      public unlink(): void {
        throw new Error('the removal was refused');
      }
    })();
    const thrown = publicationFailure(stubborn, storage);
    carriesTheReason(thrown, stubborn.injected);
    assert.strictEqual(
      thrown.message,
      `${stubborn.injected.message} The cleanup probe found an object at ${stubborn.temporary}; ` +
        'verify it before removing it.',
      `the surviving file was not named, or not named exactly: ${thrown.message}`
    );
    assert.ok(
      fs.existsSync(stubborn.temporary),
      'the cell did not leave a file behind, so it is not measuring this case'
    );
  });

  /*
   * NOTE: THE TWIN, AND IT HAS TO REACH THE SUCCESSFUL REMOVAL. Without it,
   * a build whose failure ALWAYS reports a leftover file passes the cell
   * above and sends every reader of every publication failure looking
   * for a file that is not there. The first version of this cell used a
   * stand-in that never created the file, so `unlink` threw ENOENT and
   * this was the cell below it wearing a different name -- and a note
   * added after a SUCCESSFUL unlink would have survived both.
   */
  it('says nothing about a leftover file when the cleanup removed it', () => {
    const storage = scratch();
    const tidy = new FailingWrite(true);
    const thrown = publicationFailure(tidy, storage);
    carriesTheReason(thrown, tidy.injected);
    assert.strictEqual(
      thrown.message,
      tidy.injected.message,
      `a removed file was reported as left behind: ${thrown.message}`
    );
    assert.ok(
      !fs.existsSync(tidy.temporary),
      'the cleanup did not remove the file, so this cell is not measuring a successful removal'
    );
  });

  /*
   * NOTE: AND THE STATE THAT MADE THE SENTENCE WRONG: the removal failed
   * because there was nothing to remove. This is the ORDINARY case -- a
   * write that fails before creating the file leaves no file, and
   * `unlink` answers ENOENT of its own accord here rather than being
   * told to.
   */
  it('does not invent a leftover file when the cleanup failed and none is there', () => {
    const storage = scratch();
    const empty = new FailingWrite(false);
    const thrown = publicationFailure(empty, storage);
    carriesTheReason(thrown, empty.injected);
    assert.strictEqual(
      thrown.message,
      empty.injected.message,
      `a file that was never created was reported as left behind: ${thrown.message}`
    );
    assert.ok(
      !fs.existsSync(empty.temporary),
      'the cell created the file after all, so it is not measuring this case'
    );
  });

  /*
   * NOTE: AND WHEN THE QUESTION ITSELF CANNOT BE PUT, IT SAYS THAT. Neither
   * of the two comfortable answers is chosen on the file system's
   * behalf -- the same rule the takeover ledger is built on.
   *
   * NOTE: THIS IS A STATE PRODUCTION CAN REACH. It could not be, while the
   * question went through `exists`: `existsSync` answers false for a
   * path it may not search, so the "could not be established" branch was
   * something only a stand-in could produce, and a cell for it was
   * measuring a world the product does not have. `presenceOf` reports an
   * unreadable ancestry as unknown, which is what this stand-in returns.
   */
  it('says a leftover file could not be established when it cannot look', () => {
    const storage = scratch();
    const blind = new (class extends FailingWrite {
      constructor() {
        super(true);
      }

      public unlink(): void {
        throw new Error('the removal was refused');
      }

      public presenceOf(file: string): { known: true; there: boolean } | { known: false } {
        return file.includes('.tmp') ? { known: false } : super.presenceOf(file);
      }
    })();
    const thrown = publicationFailure(blind, storage);
    carriesTheReason(thrown, blind.injected);
    assert.strictEqual(
      thrown.message,
      `${blind.injected.message} The cleanup probe could not determine whether ${blind.temporary} exists.`,
      `the unanswerable case did not say so, or did not say it exactly: ${thrown.message}`
    );
  });

  it('clears up after itself when the identity cannot be published', () => {
    const storage = scratch();
    const refusing = new (class extends RecordingFs {
      public rename(from: string, to: string): void {
        if (to.includes('session.json')) {
          throw new Error('the rename would not go through');
        }
        super.rename(from, to);
      }
    })();
    const sessions = new Sessions(refusing, storage);
    assert.throws(() => sessions.begin('S-mine', []), /would not go through/);
    const left = fs
      .readdirSync(path.join(storage, 'sessions', 'S-mine'))
      .filter((name) => name.includes('.tmp'));
    assert.deepStrictEqual(left, [], `a failed publication left ${left.join(', ')} behind`);
  });
});


/*
 * REVIEW ROUND 29: PRESENCE AFTERWARDS DOES NOT SAY WHO PUT IT THERE.
 */
describe('review 29 an entry that was already there is not one that arrived', () => {
  function oneRequest(storage: string, id: string, where: string): void {
    const dir = path.join(storage, 'sessions', id, where);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(
      path.join(dir, 'outbox.json'),
      JSON.stringify({
        cursor: null,
        entries: [
          {
            req: 'r1',
            cursor: 'w:1',
            id: 'a.1',
            field: 'src',
            payload: 'x',
            state: 'queued',
            createdAt: 0,
            lastError: null,
            importedBy: null
          }
        ]
      }),
      'utf8'
    );
  }

  /*
   * NOTE: THE FIRST QUESTION THROWS AND A LATER ONE ANSWERS "YES". Nothing
   * was adopted in this attempt -- the entry was already there. Calling
   * that an arrival told the user their work had just been rescued by a
   * takeover that moved nothing.
   */
  it('calls it a duplicate when the first question threw and it was there all along', async () => {
    const storage = scratch();
    makeSession(storage, 'S-dead');
    oneRequest(storage, 'S-dead', 'store-a');
    let asked = 0;
    let adopted = 0;
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-mine', []);
    const won = await sessions.claim('S-dead');
    assert.ok(won.claimed);
    const led = won.claimed
      ? sessions.importFrom(
          { deadSessionId: 'S-dead', sequence: won.sequence, file: won.token },
          {
            has: () => {
              asked += 1;
              if (asked === 1) {
                throw new Error('this window could not look just then');
              }
              return true;
            },
            adopt: () => {
              adopted += 1;
              return strangersReceipt();
            }
          },
          'store-a'
        )
      : emptyLedger();
    assert.strictEqual(adopted, 0, 'the fixture adopted something, so this is a different case');
    assert.strictEqual(
      led.skippedDuplicate,
      1,
      `an entry nothing moved was reported as having arrived: ${JSON.stringify(led)}`
    );
    assert.strictEqual(led.movedButUnmarked, 0);
    assert.strictEqual(ledgerTotal(led), led.observed);
  });
});
