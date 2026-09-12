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
 * offered, and why erring the other way would double-send. (§12.9)
 *
 * THE ORDER IS ESRCH FIRST, IDENTITY SECOND. A pid that is absent is
 * dead and needs no further question; a pid that is PRESENT may be a
 * reused one, so its start time is compared then. Reversing them makes
 * a dead session read as alive for ever -- which is what a reboot
 * leaves behind, so it is the common case and not the rare one.
 * (§12.21.4, C19)
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { SessionIdentity, Sessions, StartTimeReader, systemStartTime } from '../../src/sessions';
import { idleProcess } from '../support/host';
import { Outbox, OutboxEntry } from '../../src/outbox';
import { Publisher } from '../../src/publication';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-sessions-'));
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
 * ⚠️ These fixtures used `startedAt: null` to mean "this one is alive",
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
   * double-sends. (§12.21.4)
   */
  /*
   * AND A RECORD WITH NO START TIME IS THE SAME ANSWER. Nothing was
   * compared, so nothing was verified -- reporting `identity-matches`
   * there claims a check that did not happen.
   */
  it('cannot tell about a session whose record carries no start time', async () => {
    const idle = idleProcess();
    try {
      const sessions = new Sessions(new RecordingFs(), scratch());
      const answer = await sessions.livenessOf(identity({ pid: idle.pid, startedAt: null }));
      assert.deepStrictEqual(answer, { decidable: false, because: 'start-time-unavailable' });
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
  it('picks the next sequence itself rather than being told one', async () => {
    const storage = scratch();
    const sessions = new Sessions(new RecordingFs(), storage);
    makeSession(storage, 'S-old');
    const first = await sessions.claim('S-old');
    assert.ok(first.claimed, `nothing was claimed: ${JSON.stringify(first)}`);
    assert.strictEqual(first.claimed && first.sequence, 1);
  });

  it('refuses when the newest claimant is still running', async () => {
    const storage = scratch();
    makeSession(storage, 'S-old');
    const sessions = new Sessions(new RecordingFs(), storage);
    sessions.begin('S-taker', []);
    await sessions.claim('S-old');
    const again = await sessions.claim('S-old');
    assert.deepStrictEqual(again, { claimed: false, because: 'already-claimed' });
  });

  it('refuses to offer a takeover of a session that is alive', async () => {
    const idle = idleProcess();
    try {
      const storage = scratch();
      makeSession(storage, 'S-alive', liveIdentity(idle.pid));
      const sessions = new Sessions(new RecordingFs(), storage);
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
   * ⚠️ The first version created NO entries and asserted
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
      assert.strictEqual(imported.skipped, 1, 'the repeat of one request was not skipped');
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
   * something. (§12.23)
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
   * ⚠️ The first version created none and asserted the answer was an
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
   * report nothing to recover. (§12.17.4, C6)
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
      text: '## Two\nfrom the store\n'
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
   * from the file rather than from an answer. (§12.19.2, C12)
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
      text: 'no heading at all\n'
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
  it('imports nothing and sends nothing when a session merely starts up', async () => {
    const storage = scratch();
    const files = new RecordingFs();
    const sessions = new Sessions(files, storage);
    sessions.begin('S-new', ['/stores/one']);
    const others = await sessions.others();
    assert.ok(Array.isArray(others), 'starting up did not even list the other sessions');
    assert.deepStrictEqual(
      files.touched('rename'),
      [],
      'starting up moved something, so it did more than look'
    );
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
   * one interrupted takeover strands the queue for ever. (§12.13.5)
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
   * nothing is stranded. (§12.11.3, C8)
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
   * ACROSS THE WHOLE SET OF OPERATIONS, not one call. C10 asks for the
   * absence over everything `Sessions` does, and an absence is the
   * reading a narrow cell gives away.
   */
  it('never unlinks, and renames only whole directories', async () => {
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
 * ⚠️ THE FIX HAD NO CELL UNTIL A MUTATION SURVIVED. Collapsing
 * "unreadable" back into "no such session" left every existing cell
 * green, because every existing cell wrote a well-formed record. A fix
 * verified only by the reasoning that produced it is not guarded.
 */
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
