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
 * S1 and S3 - S6 of the X1a cell list: the queue, the cursor it carries
 * and what it does with each kind of answer. The editor is not involved;
 * a save here is a call with a body in it.
 */

import * as assert from 'assert';

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
/*
 * NOTE: THE STAND-IN SETTLER, AND WHAT IT DOES WITH EACH VERDICT.
 *
 * These cells are about the sending, not about the recording, so the
 * settler here just releases the entry -- but it has to release it only
 * for the verdicts that release it in the product, or the cells stop
 * describing the same machine. A refusal keeps the entry: the store said
 * no and the bytes are still only in the user's file.
 */
function settling(outbox: Outbox): Settle {
  return (req, settlement) => {
    /*
     * A PLAIN REFUSAL RELEASES THE REQUEST -- the store has answered it,
     * so it is not waiting any more -- and `req-mismatch` does not: the
     * store says this id names a different request, and the entry is
     * what a person will look at. The product records things as well;
     * these cells are about the sending.
     */
    if (settlement.verdict === 'req-mismatch') {
      return;
    }
    outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
  };
}
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Client } from '../../src/client';
import { Outbox, OutboxWriteError } from '../../src/outbox';
import { RETRYABLE_REFUSALS, RETRY_CAP, SETTINGS_REFUSALS, Saver, Settle } from '../../src/saver';
import { CliTransport } from '../../src/transport';
import { initWire } from '../../src/wire';
import { FakeCore, ScriptedCall } from '../support/fake';

const CHECK = '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';

function wrote(seq: number): string {
  return `(ok (events (("w" . ${seq}))) (state (("a.2" . "hhh"))) (cursor ("w" . ${seq})) (replay #f))\n`;
}

interface Rig {
  core: FakeCore;
  outbox: Outbox;
  saver: Saver;
}

function rig(calls: ScriptedCall[], outboxFile?: string): Rig {
  const core = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, ...calls]);
  const outbox = new Outbox(outboxFile ?? core.outboxFile());
  outbox.load();
  const client = new Client(new CliTransport(core.config(), core.env()));
  return { core, outbox, saver: new Saver(client, outbox, settling(outbox)) };
}

function setCalls(core: FakeCore): string[][] {
  return core.requests().filter((r) => r[0] === 'set');
}

describe('S1 a save is one set, carrying a request id and a cursor', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('sends the body, a fresh request id and the cursor, together', async () => {
    const r = rig([{ match: ['set'], stdout: wrote(8), rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'saved');
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 1);
    assert.deepStrictEqual(sent[0].slice(0, 3), ['set', 'a.2', 'src']);
    assert.strictEqual(sent[0][3], 'body2\n');
    assert.strictEqual(sent[0][4], '--req');
    assert.strictEqual(sent[0][6], '--cursor');
    assert.strictEqual(sent[0][7], 'w:7', 'the first cursor comes from check');
    assert.ok(
      /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(sent[0][5]),
      `the request id is not a uuid: ${sent[0][5]}`
    );
  });

  it('never sends the same request id twice', async () => {
    const r = rig([
      { match: ['set'], stdout: wrote(8), rc: 0, once: true },
      { match: ['set'], stdout: wrote(9), rc: 0, once: true }
    ]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'one\n');
    await r.saver.save('a.2', 'src', 'two\n');
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 2);
    assert.notStrictEqual(sent[0][5], sent[1][5]);
  });

  it('sends nothing at all when the entry could not be written down first', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, { match: ['set'], stdout: wrote(8), rc: 0 }]);
    core = core0;
    /*
     * THE DIRECTORY IS READABLE AND NOT WRITABLE. An earlier version put
     * a FILE where the directory belonged, and this comment still said
     * so for a round after the fixture stopped doing it. The cell is
     * about the order of the two steps, and the only way to see an
     * order is to break the second one while the first still works.
     */
    /*
     * THE READ HAS TO SUCCEED AND THE WRITE HAS TO FAIL. Putting a file
     * where the directory belongs made the RELOAD fail instead -- the
     * save was refused before `enqueue` was ever reached, so the cell
     * passed without testing what it names. A directory that can be read
     * and not written separates the two.
     */
    const blocked = path.join(core0.root, 'readonly');
    fs.mkdirSync(blocked, { recursive: true });
    const outbox = new Outbox(path.join(blocked, 'outbox.json'));
    outbox.load();
    assert.strictEqual(outbox.pendingCount, 0, 'the queue was not readable, so this tests the wrong step');
    /*
     * THE CURSOR IS ESTABLISHED WHILE THE DIRECTORY IS STILL WRITABLE.
     * Without this the bootstrap's own `setCursor` is the first write to
     * fail, and the save is refused before `enqueue` is ever reached --
     * so a build that swallowed an enqueue failure still passed. The
     * cell's name is about enqueue, so enqueue has to be the first write
     * that happens.
     */
    outbox.setCursor('w:7');
    fs.chmodSync(blocked, 0o500);
    const saver = new Saver(new Client(new CliTransport(core0.config(), core0.env())), outbox, settling(outbox));
    try {
      await assert.rejects(
        () => saver.save('a.2', 'src', 'body\n'),
        (e: unknown) => e instanceof OutboxWriteError,
        'the save failed for some reason other than not being able to write the entry down'
      );
      assert.deepStrictEqual(setCalls(core0), [], 'a save was sent that had not been written down');
      /*
       * AND THE ENTRY IS NOT SITTING IN MEMORY EITHER. Without this, an
       * `enqueue` that appended to the in-memory list, swallowed its own
       * write failure and returned still passed: the rejection this cell
       * observes would then be raised later, by `aboutToSend`, and both
       * the refusal and the absence of traffic look identical from here.
       * The step this cell is named for is the one that has to fail.
       */
      assert.strictEqual(
        outbox.entries.length,
        0,
        'the entry was kept in memory although writing it down had failed'
      );
    } finally {
      fs.chmodSync(blocked, 0o700);
    }
  });

  it('refuses to write against a store whose local writer is not known', async () => {
    const core0 = new FakeCore([
      {
        match: ['check'],
        stdout:
          '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())) ("v" (end 1) (torn #f) (integrity ())))) (verdict ok))\n',
        rc: 0
      },
      { match: ['set'], stdout: wrote(8), rc: 0 }
    ]);
    core = core0;
    const outbox = new Outbox(core0.outboxFile());
    outbox.load();
    const saver = new Saver(new Client(new CliTransport(core0.config(), core0.env())), outbox, settling(outbox));
    const outcome = await saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /writers/);
    assert.deepStrictEqual(setCalls(core0), []);
  });
});

describe('S3 the cursor moves on an answer and on a replay alike', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('carries the cursor of a write that landed into the next save', async () => {
    const r = rig([
      { match: ['set'], stdout: wrote(9), rc: 0, once: true },
      { match: ['set'], stdout: wrote(10), rc: 0, once: true }
    ]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'one\n');
    await r.saver.save('a.2', 'src', 'two\n');
    const sent = setCalls(core);
    assert.strictEqual(sent[0][7], 'w:7');
    assert.strictEqual(sent[1][7], 'w:9');
  });

  it('carries the cursor of a replay just the same', async () => {
    const r = rig([
      { match: ['set'], stdout: '(ok (replay #t) (event ("w" . 9)))\n', rc: 0, once: true },
      { match: ['set'], stdout: wrote(10), rc: 0, once: true }
    ]);
    core = r.core;
    const first = await r.saver.save('a.2', 'src', 'one\n');
    assert.strictEqual(first.status, 'replayed');
    await r.saver.save('a.2', 'src', 'two\n');
    assert.strictEqual(setCalls(core)[1][7], 'w:9');
  });

  it('keeps the cursor across a restart', async () => {
    const file = path.join(
      fs.mkdtempSync(path.join(require('os').tmpdir(), 'theourgia-cursor-')),
      'outbox.json'
    );
    const r = rig([{ match: ['set'], stdout: wrote(9), rc: 0, once: true }], file);
    core = r.core;
    await r.saver.save('a.2', 'src', 'one\n');
    const again = new Outbox(file);
    again.load();
    assert.strictEqual(again.cursor, 'w:9');
    assert.strictEqual(again.pendingCount, 0, 'a confirmed save left an entry behind');
  });
});

describe('S4 an unanswered save is kept and retried as the same request', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('keeps the entry when the core exits without answering, and survives a restart', async () => {
    const file = path.join(
      fs.mkdtempSync(path.join(require('os').tmpdir(), 'theourgia-outbox-')),
      'outbox.json'
    );
    const r = rig([{ match: ['set'], exitWithoutAnswer: true, once: true }, { match: ['set'], stdout: wrote(9), rc: 0 }], file);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'pending');

    const restarted = new Outbox(file);
    restarted.load();
    assert.strictEqual(restarted.pendingCount, 1);
    const kept = restarted.entries[0];
    assert.strictEqual(kept.id, 'a.2');
    assert.strictEqual(kept.payload, 'body2\n');
    assert.strictEqual(kept.cursor, 'w:7');
    assert.strictEqual(kept.state, 'pending');

    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), restarted, settling(restarted));
    const retried = await saver.retry();
    assert.strictEqual(retried[0].status, 'saved');
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 2);
    assert.deepStrictEqual(sent[0], sent[1], 'the retry did not send the same request');
  });

  it('retries the bytes it recorded, not whatever the buffer now holds', async () => {
    const r = rig([{ match: ['set'], exitWithoutAnswer: true, once: true }, { match: ['set'], stdout: wrote(9), rc: 0 }]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'first version\n');
    await r.saver.retry();
    const sent = setCalls(core);
    assert.strictEqual(sent[1][3], 'first version\n');
    assert.strictEqual(sent[0][5], sent[1][5], 'the retry invented a new request id');
  });

  it('keeps the entry when the request timed out', async () => {
    const r = rig([{ match: ['set'], stdout: wrote(9), rc: 0, delayMs: 3000 }]);
    core = r.core;
    const saver = new Saver(
      new Client(new CliTransport(r.core.config({ timeoutMs: 250 }), r.core.env())),
      r.outbox,
      settling(r.outbox)
    );
    const outcome = await saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'pending');
    assert.strictEqual(r.outbox.pendingCount, 1);
  });
});

describe('S5 saves are sent one at a time', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('does not send the second until the first has answered', async () => {
    const r = rig([
      { name: 'slow', match: ['set'], stdout: wrote(9), rc: 0, delayMs: 600, once: true },
      { name: 'fast', match: ['set'], stdout: wrote(10), rc: 0, once: true }
    ]);
    core = r.core;
    const both = Promise.all([
      r.saver.save('a.2', 'src', 'one\n'),
      r.saver.save('a.2', 'src', 'two\n')
    ]);
    const outcomes = await both;
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['saved', 'saved']);

    const answers = core.calls().filter((c) => c.event === 'answer' && c.coreArgv[0] === 'set');
    assert.strictEqual(answers.length, 2);
    const [first, second] = answers[0].name === 'slow' ? answers : [answers[1], answers[0]];
    assert.ok(
      second.started >= first.at,
      `the second set started at ${second.started} and the first answered at ${first.at}`
    );
    assert.strictEqual(setCalls(core)[1][7], 'w:9', 'the second save carried a stale cursor');
  });

  it('holds everything behind an entry nobody can resolve', async () => {
    const r = rig([
      { match: ['set'], contains: ['one\n'], stdout: '(error unknown (range-overlaps ("w" 3 5)))\n', rc: 1 },
      { match: ['set'], contains: ['two\n'], stdout: wrote(10), rc: 0 }
    ]);
    core = r.core;
    const first = await r.saver.save('a.2', 'src', 'one\n');
    assert.strictEqual(first.status, 'pending');
    const second = await r.saver.save('a.2', 'src', 'two\n');
    assert.strictEqual(second.status, 'pending', 'a save went out past an unresolved one');
    assert.strictEqual(r.outbox.pendingCount, 2);
    const sentBodies = setCalls(core).map((c) => c[3]);
    assert.deepStrictEqual(
      sentBodies.filter((b) => b === 'two\n'),
      [],
      'the second save was sent while the first was unresolved'
    );
  });
});

describe('S6 an answer is sorted by what it says about the store', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  async function outcomeOf(stdout: string, rc: number) {
    const r = rig([{ match: ['set'], stdout, rc }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    return { outcome, outbox: r.outbox };
  }

  it('keeps the entry when the store cannot say whether the request ran', async () => {
    const { outcome, outbox } = await outcomeOf('(error unknown (range-overlaps ("w" 3 5)))\n', 1);
    assert.strictEqual(outcome.status, 'pending');
    assert.strictEqual(outbox.pendingCount, 1);
  });

  /*
   * NOTE: THIS EXPECTATION CHANGED, AND THE CHANGE IS THE POINT.
   *
   * It used to read "drops the entry", asserting `pendingCount === 0`,
   * and it was written against what the code did: every refusal was
   * settled as though it had been acknowledged, so every refusal
   * dequeued. A review showed what that cost -- a rejected edit was
   * recorded as saved, and the `mismatch` path in `Saving` was
   * unreachable -- and the main session ruled the contract: a plain
   * refusal releases the request and records nothing, while
   * `req-mismatch` KEEPS it, because the store is saying this id names a
   * different request and no retry can settle that. The entry is what a
   * person will look at.
   *
   * The other six cells in this section did not change, which is the
   * reading that says the new contract is the one they were already
   * written for.
   */
  it('keeps the entry when the id already names a different request', async () => {
    const { outcome, outbox } = await outcomeOf('(error req-mismatch ("w" . 6))\n', 1);
    assert.strictEqual(outcome.status, 'refused');
    assert.strictEqual(outcome.keptForAPerson, true, 'the refusal did not say it kept the entry');
    assert.strictEqual(outbox.pendingCount, 1, 'the request nobody can retry was dropped');
    assert.match(outcome.message, /req-mismatch/, 'the refusal lost the name the core gave it');
  });

  it('drops the entry when the cursor names an unreachable position', async () => {
    const { outcome, outbox } = await outcomeOf(
      '(error cursor-unreachable (after ("w" . 999)) (writing ("w" . 8)))\n',
      1
    );
    assert.strictEqual(outcome.status, 'refused');
    assert.strictEqual(outbox.pendingCount, 0);
  });

  /*
   * THIS ANSWER IS NOT REACHABLE FROM THIS CLIENT YET, and the cell says
   * so rather than implying otherwise. The core produces `(error
   * changed ...)` only for a write carrying `--if-unchanged`, and this
   * batch does not send one -- so an ordinary save over somebody else's
   * edit is NOT refused, it lands. What is pinned here is that the
   * answer is classified correctly when X1b starts sending the option;
   * it is not evidence that a stale save is protected today.
   */
  it('classifies the stale-write refusal, which X1b will be able to provoke', async () => {
    const { outcome, outbox } = await outcomeOf('(error changed (current "hhh"))\n', 1);
    assert.strictEqual(outcome.status, 'refused');
    assert.match(outcome.message, /changed in the store/);
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('reports a request without a cursor as a defect rather than passing over it', async () => {
    const { outcome, outbox } = await outcomeOf('(error bad-request req-without-cursor)\n', 1);
    assert.strictEqual(outcome.status, 'refused');
    assert.match(outcome.message, /bad-request/);
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('treats an operator\'s determination that the work was done as done', async () => {
    const { outcome, outbox } = await outcomeOf('(error resolved-executed (event ("w" . 6)))\n', 1);
    assert.strictEqual(outcome.status, 'replayed');
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('keeps the entry when the core exits non-zero with nothing to say', async () => {
    const { outcome, outbox } = await outcomeOf('', 1);
    assert.strictEqual(outcome.status, 'pending');
    assert.strictEqual(outbox.pendingCount, 1);
  });

  it('leaves nothing behind once a save is confirmed', async () => {
    const { outcome, outbox } = await outcomeOf(wrote(9), 0);
    assert.strictEqual(outcome.status, 'saved');
    assert.strictEqual(outbox.pendingCount, 0);
    assert.strictEqual(outbox.cursor, 'w:9');
  });
});

describe('S9 the cursor survives a restart and is not asked for twice', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('sends the next save against the cursor the last answer established, without asking check again', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s9-')), 'outbox.json');
    const r = rig(
      [
        { match: ['set'], stdout: wrote(9), rc: 0, once: true },
        { match: ['set'], stdout: wrote(10), rc: 0, once: true }
      ],
      file
    );
    core = r.core;
    await r.saver.save('a.2', 'src', 'one\n');
    assert.strictEqual(core.requests().filter((c) => c[0] === 'check').length, 1);

    const restarted = new Outbox(file);
    restarted.load();
    assert.strictEqual(restarted.cursor, 'w:9');
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), restarted, settling(restarted));
    const outcome = await saver.save('a.2', 'src', 'two\n');
    assert.strictEqual(outcome.status, 'saved', outcome.message);

    const sent = setCalls(core);
    assert.strictEqual(sent[1][7], 'w:9', 'the save after a restart used the wrong cursor');
    assert.strictEqual(
      core.requests().filter((c) => c[0] === 'check').length,
      1,
      'check was asked again although the cursor was on disk'
    );
  });
});

describe('S10 the entry is on disk at the moment the core sees the request', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THE WITNESS IS THE CORE, NOT THE CLIENT. Asking the client whether
   * it wrote before it sent is asking the suspect; the stand-in reads
   * the outbox file the instant the request arrives, which is a moment
   * only the receiving side has.
   */
  it('the core finds the entry already written when the request arrives', async () => {
    const core0 = new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: wrote(9), rc: 0 }
    ]);
    core = core0;
    const outboxFile = core0.outboxFile();
    const outbox = new Outbox(outboxFile);
    outbox.load();
    const saver = new Saver(
      new Client(new CliTransport(core0.config(), core0.env(outboxFile))),
      outbox,
      settling(outbox)
    );
    const outcome = await saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'saved');

    const atSet = core0.calls().find((c) => c.event === 'answer' && c.coreArgv[0] === 'set');
    assert.ok(atSet !== undefined, 'the set never reached the core');
    assert.ok(
      atSet?.watched !== null && atSet?.watched !== undefined,
      'the outbox file did not exist when the request arrived'
    );
    const onDisk = JSON.parse(atSet?.watched as string) as {
      entries: { req: string; payload: string; cursor: string; state: string }[];
    };
    assert.strictEqual(onDisk.entries.length, 1);
    assert.strictEqual(onDisk.entries[0].req, outcome.req);
    assert.strictEqual(onDisk.entries[0].payload, 'body2\n');
    assert.strictEqual(onDisk.entries[0].cursor, 'w:7');
    assert.strictEqual(
      onDisk.entries[0].state,
      'sent',
      'the entry did not record that the request was going out'
    );
  });
});

describe('S11 a host interrupted between the send and the answer', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * WHAT IS ON DISK AFTER AN INTERRUPTION is a cursor that a previous
   * save moved and an entry that nothing has resolved. The entry's own
   * cursor is older than the outbox's, and that is not a mistake to be
   * corrected: the request went out with it.
   */
  it('keeps the entry and does not move its cursor to the one the outbox now holds', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s11-')), 'outbox.json');
    const r = rig(
      [
        { match: ['set'], contains: ['first\n'], stdout: wrote(9), rc: 0 },
        { match: ['set'], contains: ['second\n'], stdout: wrote(10), rc: 0 }
      ],
      file
    );
    core = r.core;
    await r.saver.save('a.2', 'src', 'first\n');
    assert.strictEqual(r.outbox.cursor, 'w:9');

    /*
     * THE ENTRY'S CURSOR AND THE OUTBOX'S ARE DIFFERENT ON PURPOSE. With
     * both at `w:9` a client that rewrote the first from the second
     * would be invisible -- the value it wrote would be the value that
     * was already there. `w:4` is the position this request really went
     * out at, which is what makes the rewrite observable.
     */
    const interrupted = new Outbox(file);
    interrupted.load();
    interrupted.enqueue({
      req: 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee',
      cursor: 'w:4',
      id: 'a.2',
      field: 'src',
      payload: 'second\n',
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    interrupted.setCursor('w:9');

    const reloaded = new Outbox(file);
    reloaded.load();
    assert.strictEqual(reloaded.pendingCount, 1, 'the interrupted entry was not kept');
    assert.strictEqual(reloaded.entries[0].state, 'sent');
    assert.strictEqual(reloaded.cursor, 'w:9', 'the cursor was not persisted');

    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded, settling(reloaded));
    const outcomes = await saver.retry();
    assert.strictEqual(outcomes[0].status, 'saved', outcomes[0].message);
    const sent = setCalls(core);
    assert.strictEqual(sent[1][5], 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', 'the retry changed the request id');
    assert.strictEqual(
      sent[1][7],
      'w:4',
      'the retry sent the outbox cursor instead of the one the request went out at'
    );
  });

  it('does not move the cursor of a sent entry even when the outbox has moved on', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s11b-')), 'outbox.json');
    const outbox = new Outbox(file);
    outbox.load();
    outbox.setCursor('w:3');
    outbox.enqueue({
      req: 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee',
      cursor: 'w:3',
      id: 'a.2',
      field: 'src',
      payload: 'x\n',
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    outbox.setCursor('w:40');
    outbox.aboutToSend('aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', 'w:40');
    assert.strictEqual(outbox.entries[0].cursor, 'w:3', 'a sent request had its identity rewritten');
  });

  it('does correct the cursor of an entry that has not been sent', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s11c-')), 'outbox.json');
    const outbox = new Outbox(file);
    outbox.load();
    outbox.enqueue({
      req: 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee',
      cursor: 'w:3',
      id: 'a.2',
      field: 'src',
      payload: 'x\n',
      state: 'queued',
      createdAt: 0,
      lastError: null,
      importedBy: null
    });
    outbox.aboutToSend('aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', 'w:40');
    assert.strictEqual(outbox.entries[0].cursor, 'w:40');
    assert.strictEqual(outbox.entries[0].state, 'sent');
  });
});

describe('S12 entries found on disk after a restart', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function entriesOnDisk(file: string): Outbox {
    const outbox = new Outbox(file);
    outbox.load();
    outbox.setCursor('w:7');
    /*
     * THE WRITTEN ORDER AGREES WITH NO SORT IN EITHER DIRECTION. Two
     * entries could only be ascending or descending, and the second
     * version of this fixture was descending in both fields -- so a
     * descending sort still reproduced it. Three entries with ids
     * 5,9,1 and payloads mike, alpha, zulu are monotonic in neither
     * field either way, and their creation times descend, so no sort on
     * any of the three reproduces the written order.
     */
    /*
     * AND THE TIMESTAMPS DISAGREE WITH THE ORDER TOO. Every entry
     * carrying `createdAt: 0` left a whole family of implementations
     * alive: a stable sort by creation time reproduces the written order
     * whenever the times are equal, and would reorder a real queue whose
     * times are not. The first attempt at this made them DESCENDING --
     * 300, 200, 100 -- which a descending sort reproduces exactly, so it
     * killed half the family and the comment claimed it killed all of
     * it. They are 300, 100, 200: monotonic in neither direction.
     */
    for (const [req, payload, createdAt] of [
      ['55555555-5555-5555-5555-555555555555', 'mike\n', 300],
      ['99999999-9999-9999-9999-999999999999', 'alpha\n', 100],
      ['11111111-1111-1111-1111-111111111111', 'zulu\n', 200]
    ] as Array<[string, string, number]>) {
      outbox.enqueue({
        req,
        cursor: 'w:7',
        id: 'a.2',
        field: 'src',
        payload,
        state: 'sent',
        createdAt,
        lastError: null,
        importedBy: null
      });
    }
    return outbox;
  }

  it('sends them in the order they were written down', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s12-')), 'outbox.json');
    entriesOnDisk(file);
    const r = rig(
      [
        { match: ['set'], contains: ['mike\n'], stdout: wrote(9), rc: 0 },
        { match: ['set'], contains: ['alpha\n'], stdout: wrote(10), rc: 0 },
        { match: ['set'], contains: ['zulu\n'], stdout: wrote(11), rc: 0 }
      ],
      file
    );
    core = r.core;
    const reloaded = new Outbox(file);
    reloaded.load();
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded, settling(reloaded));
    const outcomes = await saver.retry();
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['saved', 'saved', 'saved']);
    const bodies = setCalls(core).map((c) => c[3]);
    assert.deepStrictEqual(
      bodies,
      ['mike\n', 'alpha\n', 'zulu\n'],
      'the restart sent them in some order other than the one they were written in'
    );
    assert.strictEqual(reloaded.pendingCount, 0);
  });

  it('holds the second back when the first cannot be resolved', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s12b-')), 'outbox.json');
    entriesOnDisk(file);
    const r = rig(
      [
        { match: ['set'], contains: ['mike\n'], stdout: '(error unknown (chain-unreadable))\n', rc: 1 },
        { match: ['set'], contains: ['alpha\n'], stdout: wrote(10), rc: 0 }
      ],
      file
    );
    core = r.core;
    const reloaded = new Outbox(file);
    reloaded.load();
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded, settling(reloaded));
    const outcomes = await saver.retry();
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['pending']);
    assert.deepStrictEqual(
      setCalls(core).map((c) => c[3]),
      ['mike\n'],
      'something went out past an unresolved head'
    );
    assert.strictEqual(reloaded.pendingCount, 3, 'an entry was resolved although the head was not');
  });
});

describe('an ok that names no record is not a save this client can act on', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('keeps the entry when the answer carries neither a cursor nor an event', async () => {
    const r = rig([{ match: ['set'], stdout: '(ok)\n', rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'pending', 'an ok with no record in it was called a save');
    assert.strictEqual(r.outbox.pendingCount, 1);
    assert.strictEqual(r.outbox.cursor, 'w:7', 'the cursor moved on an answer that named no record');
  });

  it('keeps the entry when the answer names a record it cannot spell a cursor for', async () => {
    const r = rig([{ match: ['set'], stdout: '(ok (cursor ("w:x" . 9)))\n', rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'pending');
    assert.strictEqual(r.outbox.cursor, 'w:7');
  });
});

describe('two savers over one queue are still one request at a time', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THE EXTENSION BUILDS A NEW SAVER EVERY TIME A SETTING CHANGES, over
   * the same outbox file, without waiting for the old one. If the
   * serialisation lived in the object, the second Saver would start with
   * an empty chain and send while the first was still in flight -- two
   * requests on the wire, the second carrying a cursor the first is
   * about to move.
   */
  it('holds the second saver behind the first saver\'s request', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-two-savers-')), 'outbox.json');
    const r = rig(
      [
        { name: 'slow', match: ['set'], contains: ['one\n'], stdout: wrote(9), rc: 0, delayMs: 700 },
        { name: 'fast', match: ['set'], contains: ['two\n'], stdout: wrote(10), rc: 0 }
      ],
      file
    );
    core = r.core;

    const second = new Outbox(file);
    second.load();
    const other = new Saver(new Client(new CliTransport(core.config(), core.env())), second, settling(second));

    const first = r.saver.save('a.2', 'src', 'one\n');
    const later = other.save('a.2', 'src', 'two\n');
    const outcomes = await Promise.all([first, later]);
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['saved', 'saved']);

    const answers = core.calls().filter((c) => c.event === 'answer' && c.coreArgv[0] === 'set');
    assert.strictEqual(answers.length, 2);
    const slow = answers.find((a) => a.name === 'slow');
    const fast = answers.find((a) => a.name === 'fast');
    assert.ok(slow !== undefined && fast !== undefined);
    assert.ok(
      (fast?.started as number) >= (slow?.at as number),
      `the second saver started at ${fast?.started} while the first answered at ${slow?.at}`
    );
  });
});

describe('a core that exits without answering says why, and the client passes it on', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THIS CAME OUT OF AN INCIDENT. A full run once reported "the core
   * exited 255 without an answer" for a save against the real core, and
   * it could not be looked into afterwards because the message carried
   * the exit code and nothing else -- the core's own account of what
   * went wrong was on the other stream and was dropped.
   */
  it('carries the core\'s stderr into the message when nothing was printed', async () => {
    const r = rig([
      {
        match: ['set'],
        stdout: '',
        stderr: 'Exception in fasl-read: invalid situation\n',
        rc: 255
      }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'pending');
    assert.match(outcome.message, /exited 255/);
    assert.match(outcome.message, /invalid situation/, 'the core said why and the message dropped it');
    assert.match(
      r.outbox.entries[0].lastError as string,
      /invalid situation/,
      'the reason was not written down with the entry either'
    );
  });

  it('says nothing extra when the core was silent on both streams', async () => {
    const r = rig([{ match: ['set'], stdout: '', stderr: '', rc: 3 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.match(outcome.message, /exited 3 without an answer/);
    assert.ok(!/It said/.test(outcome.message), 'an empty stderr was announced as though it said something');
  });

  it('carries the reason through a transport failure too', async () => {
    const r = rig([
      {
        match: ['set'],
        stdout: '',
        stderr: 'Exception: library (igropyr sexpr) not found\n',
        rc: 1
      }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'pending');
    assert.match(outcome.message, /libDirs/);
  });
});

describe('a core that does not understand the request says which core it is', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * MEASURED. Against theourgia at 842cf46 a tracked write answers
   * `(usage (set <id> <field> <value>))` with exit 1 -- that build has
   * no request tracking at all, and the option this client always sends
   * is not in its usage line. The message used to render as "the core
   * refused the write: refused", which named nothing and cost an hour
   * to work out by hand.
   */
  it('names the version problem when the core answers with a usage line', async () => {
    const r = rig([{ match: ['set'], stdout: '(usage (set <id> <field> <value>))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'refused');
    assert.match(outcome.message, /usage line/);
    assert.match(outcome.message, /--req/);
    assert.match(outcome.message, /corePath/);
    assert.match(
      outcome.message,
      /arguments sent/,
      'the message asserts a version problem where an argument problem is equally possible'
    );
    assert.ok(
      !/refused the write: refused/.test(outcome.message),
      'the message still says nothing about what went wrong'
    );
  });

  it('still names an ordinary refusal by the name the core gave it', async () => {
    const r = rig([{ match: ['set'], stdout: '(error req-mismatch ("w" . 6))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.match(outcome.message, /req-mismatch/);
  });
});

describe('two savers over one queue share the queue, not just the lock', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THE FILE IS THE QUEUE AND EACH SAVER HOLDS A COPY OF IT. Taking the
   * lock in turn is not enough: the second Saver's Outbox was read
   * before the first one recorded anything, and writing that copy back
   * ERASES what the first wrote. The entry lost this way is precisely
   * the one whose outcome nobody knows -- the one the outbox exists for.
   *
   * The earlier two-saver cell scripted only successful answers, so
   * every entry was resolved and removed anyway and the loss was
   * invisible. Here neither answer resolves anything, so an entry that
   * disappears can only have been overwritten.
   */
  it('does not let one saver write over an entry the other recorded', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-shared-')), 'outbox.json');
    const r = rig(
      [
        { match: ['set'], contains: ['one\n'], stdout: '(error unknown (chain-unreadable))\n', rc: 1 },
        { match: ['set'], contains: ['two\n'], stdout: '(error unknown (chain-unreadable))\n', rc: 1 }
      ],
      file
    );
    core = r.core;

    /*
     * Loaded BEFORE the first save records anything, which is what the
     * extension does whenever a setting changes mid-flight.
     */
    const second = new Outbox(file);
    second.load();
    const other = new Saver(new Client(new CliTransport(core.config(), core.env())), second, settling(second));

    /*
     * BOTH ARE STARTED BEFORE EITHER FINISHES. Awaiting the first and
     * then calling the second would leave a reload taken BEFORE the lock
     * indistinguishable from one taken after it -- by then the disk
     * already holds the first entry either way. Started together, only a
     * reload that happens after the lock is granted can see it.
     */
    const [first, later] = await Promise.all([
      r.saver.save('a.2', 'src', 'one\n'),
      other.save('a.2', 'src', 'two\n')
    ]);
    assert.strictEqual(first.status, 'pending', first.message);
    assert.strictEqual(later.status, 'pending', 'the second save went out past an unresolved one');

    const onDisk = new Outbox(file);
    onDisk.load();
    assert.deepStrictEqual(
      onDisk.entries.map((e) => e.payload),
      ['one\n', 'two\n'],
      'a saver holding a stale copy of the queue wrote over an entry nobody had resolved'
    );
  });

  it('picks up an entry the other saver added, rather than starting from its own copy', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-shared2-')), 'outbox.json');
    const r = rig(
      [
        { match: ['set'], contains: ['one\n'], stdout: '(error unknown (chain-unreadable))\n', rc: 1 },
        { match: ['set'], contains: ['two\n'], stdout: wrote(11), rc: 0 }
      ],
      file
    );
    core = r.core;

    const second = new Outbox(file);
    second.load();
    const other = new Saver(new Client(new CliTransport(core.config(), core.env())), second, settling(second));

    await r.saver.save('a.2', 'src', 'one\n');
    /*
     * WHAT THE SECOND OPERATION ADDED, not what the whole run contains.
     * `bodies.includes('one')` was already true because the FIRST save
     * sent it, so the claim "the head was retried" was satisfied without
     * the second saver doing anything at all.
     */
    const beforeSecond = setCalls(core).length;
    const later = await other.save('a.2', 'src', 'two\n');
    assert.strictEqual(
      later.status,
      'pending',
      'the second saver did not see the unresolved entry the first had queued'
    );
    const added = setCalls(core).slice(beforeSecond).map((c) => c[3]);
    assert.deepStrictEqual(
      added,
      ['one\n'],
      'the second operation did not retry the unresolved head, or sent its own body past it'
    );
  });
});

/*
 * X1c: A DRAIN THAT CANNOT END.
 *
 * `drain` walks the queue from the front and stops at the first entry
 * whose outcome is unknown. It has no other way to stop: it takes
 * `entries[0]`, sends it, and loops unless the answer was `pending`. The
 * entry is expected to be gone by then -- the settler removes it -- and
 * NOTHING CHECKS THAT IT WENT.
 *
 * The settler is allowed not to remove it. That is deliberate and it is
 * the safe direction: `recordAnswer` keeps the entry when the record
 * beside the file could not be written, so the request stays retryable
 * rather than being lost. But then the store has answered, the outcome
 * is not `pending`, and the same entry is at the front of the queue
 * again -- for ever, sending the same request to the store on every
 * turn, inside a command the user is awaiting.
 *
 * NOTE: HOW THIS SHOWED UP IS WHY IT IS WORTH SAYING: as `Timeout of
 * 180000ms exceeded` in an editor-hosted cell, three runs out of three,
 * with nothing naming the queue. A hang is the failure a suite reports
 * worst.
 */
describe('X1c a save stops when the answer could not be recorded', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THE SCRIPT ANSWERS `ok` MORE TIMES THAN A CORRECT BUILD NEEDS, and
   * only then something the Saver must treat as `pending`. A correct
   * build sends once; a looping one sends until the `ok` answers run
   * out. Either way the cell ends, and the COUNT is what tells them
   * apart -- a hang proves nothing a reader can act on.
   *
   * NOTE: THE FIRST VERSION OF THIS CELL MEASURED ONLY THE RETRY, after a
   * `save` had already driven the entry into `pending` by exhausting the
   * script. It passed against the looping build. The loop happens inside
   * the FIRST call, so that is the call the count has to be about.
   */
  function okThenUnknown(): FakeCore {
    return new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: wrote(8), rc: 0, once: true },
      { match: ['set'], stdout: wrote(9), rc: 0, once: true },
      { match: ['set'], stdout: wrote(10), rc: 0, once: true },
      { match: ['set'], stdout: wrote(11), rc: 0, once: true },
      { match: ['set'], stdout: wrote(12), rc: 0, once: true },
      { match: ['set'], stdout: '(error unknown "no")\n', rc: 0 }
    ]);
  }

  it('sends the request once and gives up rather than sending it again for ever', async () => {
    core = okThenUnknown();
    const outbox = new Outbox(core.outboxFile());
    outbox.load();
    const client = new Client(new CliTransport(core.config(), core.env()));
    /*
     * A SETTLER THAT RECORDS NOTHING AND REMOVES NOTHING -- which is
     * what `recordAnswer` does when the bytes under the file moved while
     * the answer was in flight. Keeping the entry is deliberate; sending
     * it again for ever is not.
     */
    const saver = new Saver(client, outbox, () => undefined);
    const outcome = await saver.save('a.2', 'src', 'body\n');
    const sent = setCalls(core).length;
    assert.strictEqual(
      sent,
      1,
      `one save sent the same request ${sent} times: the drain does not end when the answer ` +
        'cannot be recorded, so the store is asked again on every turn inside a command the user ' +
        'is awaiting'
    );
    /*
     * AND THE ENTRY IS STILL THERE, which is the whole reason the
     * settler may decline. A build that ended the loop by dropping the
     * request would satisfy the count and lose the save.
     */
    assert.strictEqual(outbox.entries.length, 1, 'the request was dropped to end the loop');
    /*
     * AND IT IS REPORTED AS UNRESOLVED. "Saved" about a queue that still
     * holds the request would make the sentence after a retry disagree
     * with the count in the status bar.
     */
    assert.strictEqual(outcome.status, 'pending', `the save reported ${outcome.status}`);
    assert.strictEqual(outbox.entries[0].state, 'pending', 'the entry does not say it needs attention');
  });

  /*
   * WHAT THIS ONE IS THE ONLY EVIDENCE FOR: that `retry` reaches the
   * same guard. It shares `drain` with the cell above and does NOT add a
   * second red -- measured: with the guard disabled this cell stays
   * green, because by then the first call has already driven the entry
   * into `pending`. It is here because `retry` is a separate entry point
   * and a later build that gave it a loop of its own would be caught
   * here and nowhere else.
   */
  it('retries it once more and stops again, rather than spinning', async () => {
    core = okThenUnknown();
    const outbox = new Outbox(core.outboxFile());
    outbox.load();
    const client = new Client(new CliTransport(core.config(), core.env()));
    const saver = new Saver(client, outbox, () => undefined);
    await saver.save('a.2', 'src', 'body\n');
    const before = setCalls(core).length;
    const outcomes = await saver.retry();
    const sent = setCalls(core).length - before;
    assert.strictEqual(sent, 1, `a retry sent the same request ${sent} times`);
    assert.strictEqual(outcomes.length, 1);
    assert.strictEqual(outcomes[0].status, 'pending');
  });

  /*
   * THE GREEN TWIN. A build that stopped after every send would satisfy
   * both cells above and would never drain a queue.
   *
   * NOTE: AND IT HAS TO BE ONE CALL OVER TWO QUEUED ENTRIES. The first
   * version did two awaited saves, each of which began with an empty
   * queue -- so it passed a drain that returns after every single
   * normally settled request, which is exactly the build it exists to
   * rule out. Found in review. The entries are queued first, against a
   * core that answers nothing, and then one retry has to carry both.
   */
  it('still drains a queue of two entries in one call when the answers are recorded', async () => {
    core = new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: '(error unknown "not yet")\n', rc: 0, once: true },
      { match: ['set'], stdout: '(error unknown "not yet")\n', rc: 0, once: true },
      { match: ['set'], stdout: wrote(8), rc: 0, once: true },
      { match: ['set'], stdout: wrote(9), rc: 0, once: true },
      { match: ['set'], stdout: '(error unknown "no")\n', rc: 0 }
    ]);
    const outbox = new Outbox(core.outboxFile());
    outbox.load();
    const client = new Client(new CliTransport(core.config(), core.env()));
    const saver = new Saver(client, outbox, settling(outbox));
    /*
     * TWO ENTRIES IN THE QUEUE AND NEITHER SETTLED. `unknown` is the one
     * answer that means "ask again", so both saves leave their entry
     * behind without the store having applied anything.
     */
    assert.strictEqual((await saver.save('a.2', 'src', 'one\n')).status, 'pending');
    assert.strictEqual((await saver.save('a.3', 'src', 'two\n')).status, 'pending');
    assert.strictEqual(outbox.entries.length, 2, 'the queue did not hold two entries to drain');

    const before = setCalls(core).length;
    const outcomes = await saver.retry();
    assert.strictEqual(
      setCalls(core).length - before,
      2,
      'one retry did not carry both entries: a drain that returns after every settled request ' +
        'would leave the second waiting for ever'
    );
    assert.strictEqual(outcomes.length, 2);
    assert.strictEqual(outbox.entries.length, 0, 'the queue was not drained');
  });

  /*
   * AND A MIXED QUEUE: the first answer is recorded and its entry goes,
   * the second is not and its entry stays. The drain must carry on past
   * the first and stop at the second, which is the boundary the guard
   * changes and which neither cell above reaches.
   */
  it('carries on past a settled entry and stops at one that was kept', async () => {
    core = new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: '(error unknown "not yet")\n', rc: 0, once: true },
      { match: ['set'], stdout: '(error unknown "not yet")\n', rc: 0, once: true },
      { match: ['set'], stdout: wrote(8), rc: 0, once: true },
      { match: ['set'], stdout: wrote(9), rc: 0, once: true },
      { match: ['set'], stdout: wrote(10), rc: 0, once: true },
      { match: ['set'], stdout: '(error unknown "no")\n', rc: 0 }
    ]);
    const outbox = new Outbox(core.outboxFile());
    outbox.load();
    const client = new Client(new CliTransport(core.config(), core.env()));
    let settled = 0;
    const saver = new Saver(client, outbox, (req, settlement) => {
      /*
       * THE FIRST ANSWER IS RECORDED, THE SECOND IS NOT -- which is what
       * `recordAnswer` does when the record beside the second file
       * cannot be written.
       */
      settled += 1;
      if (settled === 1 && settlement.verdict === 'confirmed') {
        outbox.resolve(req, settlement.cursor);
      }
    });
    await saver.save('a.2', 'src', 'one\n');
    await saver.save('a.3', 'src', 'two\n');
    assert.strictEqual(outbox.entries.length, 2);

    const before = setCalls(core).length;
    const outcomes = await saver.retry();
    assert.strictEqual(setCalls(core).length - before, 2, 'the drain stopped at the settled entry');
    assert.strictEqual(outbox.entries.length, 1, 'the kept entry was removed after all');
    assert.strictEqual(outcomes[outcomes.length - 1].status, 'pending');
  });
});

/*
 * S-notnow AND S-unknown: THE TWO ANSWERS THAT ARE NOT ABOUT THESE BYTES.
 *
 * The core gained both with its batch E, and what separates them is what
 * the store knows. `store-busy` and `draining` say the store did not look
 * at this write; `transport-unknown` says the store may have applied it
 * and cannot say. NEVER: Neither may settle the entry, and they settle it in
 * two DIFFERENT ways: one is retried under a cap, the other stops the
 * queue until somebody learns what happened. (v223, v224)
 */
describe('S-notnow a refusal that is not about these bytes is retried, and capped', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  const BUSY = '(error store-busy (path "/s"))\n';

  it('keeps the entry under the same request id rather than settling it', async () => {
    const r = rig([{ match: ['set'], stdout: BUSY, rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'pending');
    const held = r.outbox.entries;
    assert.strictEqual(held.length, 1, 'the entry must still be in the queue');
    assert.strictEqual(held[0].req, outcome.req);
    /*
     * NEVER: THE POINT OF THE ROW. A settled entry would be gone, and the
     * next save would go out under a NEW id -- which is a different
     * request as far as the store is concerned, and the first one's
     * outcome would never be established.
     */
    assert.strictEqual(setCalls(core)[0][4], '--req');
  });

  /*
   * NEVER: FIVE IN A ROW PARKS IT, AND THE QUEUE STEPS OVER IT. `pending`
   * would stop the queue for ever on a store that is never going to
   * answer differently without a person; `parked` is the state that is
   * stepped over. The two are not interchangeable and this row is the
   * difference.
   */
  it('parks the entry after five in a row, and carries on with another block', async () => {
    const r = rig([
      { match: ['set', 'a.2'], stdout: BUSY, rc: 1 },
      { match: ['set', 'b.3'], stdout: wrote(9), rc: 0 }
    ]);
    core = r.core;
    for (let i = 0; i < RETRY_CAP; i += 1) {
      await r.saver.save('a.2', 'src', `body${i}\n`);
    }
    /*
     * NOTE: THE QUEUE IS WHAT THIS ROW READS, NOT WHAT `save` RETURNED.
     * After the first refusal every later `save` answers about the entry
     * IT just queued -- "queued behind an earlier one whose outcome is
     * unknown" -- while the entry being retried is the first one. A row
     * written against the caller's status would be reading a different
     * request from the one it is about. Measured: it read `pending` five
     * times while the first entry went from attempt 1 to parked.
     */
    const parked = r.outbox.entries.filter((e) => e.state === 'parked');
    assert.strictEqual(parked.length, 1, 'the fifth answer must park the entry being retried');
    assert.ok(
      parked[0].lastError?.includes(String(RETRY_CAP)),
      `the reason must say how many times: ${parked[0].lastError}`
    );

    /*
     * NEVER: AND THE QUEUE CARRIES ON, which is the whole difference between
     * `parked` and `pending`. NOTE: It has to be ANOTHER block: entries for
     * a parked block are held back with it, deliberately, so a same-block
     * save would be stepped over too and this row would pass for the
     * wrong reason.
     */
    const before = setCalls(core).length;
    const other = await r.saver.save('b.3', 'src', 'elsewhere\n');
    assert.strictEqual(other.status, 'saved', 'another block must still go out');
    assert.ok(setCalls(core).length > before, 'the other block must have been sent');
  });

  /*
   * NEVER: TWIN: A RESTART STARTS THE COUNT OVER, ON PURPOSE. The count is
   * in memory; a new Saver over the same queue file is what a restart
   * looks like from here. Persisting it would leave a user with an entry
   * that has used up its allowance and will never go again on its own,
   * which is harder to explain than trying once more -- and both of
   * these conditions are transient by nature. (v224)
   */
  it('TWIN: a restart sends the same entry once more', async () => {
    const r = rig([{ match: ['set'], stdout: BUSY, rc: 1 }]);
    core = r.core;
    for (let i = 0; i < RETRY_CAP - 1; i += 1) {
      await r.saver.save('a.2', 'src', `body${i}\n`);
    }
    assert.strictEqual(
      r.outbox.entries.filter((e) => e.state === 'parked').length,
      0,
      'four in a row must not have parked it yet, or this row proves nothing'
    );
    const before = setCalls(core).length;
    const restarted = new Saver(
      new Client(new CliTransport(core.config(), core.env())),
      r.outbox,
      settling(r.outbox)
    );
    await restarted.save('a.2', 'src', 'again\n');
    assert.ok(setCalls(core).length > before, 'the restarted saver must have sent it again');
    assert.strictEqual(
      r.outbox.entries.filter((e) => e.state === 'parked').length,
      0,
      'and the fifth send after a restart must not park it: the count started over'
    );
  });
});

describe('S-unknown an answer nobody can act on stops the queue and keeps the identity', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  const LOST = '(error transport-unknown (reason store-actor-down))\n';

  /*
   * NEVER: THE REQUEST ID IS THE WHOLE ROW. The store may already have
   * applied this write; sending it again under a new id would ask the
   * store to do it a second time, and nothing would ever establish what
   * the first one did. So the resend carries the SAME id, and the core
   * recognising it -- answering with `(replay #t)` -- is what says the
   * identity survived.
   */
  it('resends under the same request id, and the core answers it as a replay', async () => {
    const r = rig([
      /*
       * NOTE: `once` IS LOAD BEARING. A scripted call without it answers
       * EVERY matching send, so the retry met the same lost answer again
       * and the row read "it was never settled" about a core that had
       * never been asked a second time. Measured: two sends, both
       * answered `transport-unknown`, no settlement -- which is exactly
       * what a broken retry would also look like.
       */
      { match: ['set'], stdout: LOST, rc: 1, once: true },
      { match: ['set'], stdout: '(ok (events (("w" . 9))) (state (("a.2" . "hhh"))) (cursor ("w" . 9)) (replay #t))\n', rc: 0 }
    ]);
    core = r.core;
    const first = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(first.status, 'pending', 'an unknown outcome may not be settled');
    assert.strictEqual(r.outbox.entries.length, 1, 'the entry stays until somebody knows');
    const held = r.outbox.entries[0].req;

    await r.saver.save('a.2', 'src', 'body3\n');
    const sent = setCalls(core);
    const idOf = (call: string[]): string => call[call.indexOf('--req') + 1];
    /*
     * NOTE: THREE SENDS, NOT TWO, AND THE THIRD IS THE POINT OF THE FIRST
     * TWO. Once the held entry is answered it leaves the queue, and the
     * queue carries on with the save that had been waiting behind it.
     * This row first asserted two, which described a queue that stays
     * stuck after the answer arrives -- the opposite of what a settled
     * unknown is for.
     */
    assert.ok(sent.length >= 2, `the held entry must have gone out again: ${sent.length} sends`);
    /*
     * NEVER: THE REQUEST ID IS THE WHOLE ROW. The store may already have
     * applied the first send; going again under a NEW id would ask it to
     * do the work twice and leave the first attempt's outcome permanently
     * unestablished. The core recognising the id -- answering `(replay
     * #t)` -- is what says the identity survived the retry.
     */
    assert.strictEqual(idOf(sent[1]), idOf(sent[0]), 'the retry must wear the first send\'s id');
    assert.strictEqual(idOf(sent[0]), held, 'and it must be the id the queue was holding');
    assert.ok(
      r.outbox.entries.every((e) => e.req !== held),
      'once the core answers the replay, the held entry is settled and leaves the queue'
    );
    /*
     * AND THE QUEUE RESUMED: the save that was waiting behind the
     * unknown one goes out under its OWN id, which is what says the
     * stop was about that one entry and not about the queue.
     */
    assert.ok(sent.length >= 3, 'the save queued behind it must go out once the unknown is settled');
    assert.notStrictEqual(idOf(sent[2]), held, 'and it is a different request, with its own id');
  });

  /*
   * KEY: A BARE `unreadable` DOES NOT SAY WHETHER THE WRITE LANDED. The
   * core's guard makes it of an entry it could not read anywhere in the
   * verb, after the append included, and gives no position (ruled
   * 2026-09-25). Settling it as a refusal would release the send's number
   * and record "the store declined this" about a write that may be in
   * the log.
   */
  it('keeps a save answered unreadable pending under its own id, and settles it by replay', async () => {
    const r = rig([
      {
        match: ['set'],
        stdout: '(error unreadable (path "writers/w/log") (reason "permission denied"))\n',
        rc: 1,
        once: true
      },
      { match: ['set'], stdout: '(ok (events (("w" . 9))) (state (("a.2" . "hhh"))) (cursor ("w" . 9)) (replay #t))\n', rc: 0 }
    ]);
    core = r.core;
    const first = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(first.status, 'pending', `an unreadable answer was settled as ${first.status}`);
    assert.strictEqual(r.outbox.entries.length, 1, 'the entry left the queue on an answer that knows nothing');
    const held = r.outbox.entries[0].req;
    await r.saver.save('a.2', 'src', 'body3\n');
    const sent = setCalls(core);
    const idOf = (call: string[]): string => call[call.indexOf('--req') + 1];
    assert.ok(sent.length >= 2, `the held entry must have gone out again: ${sent.length} sends`);
    assert.strictEqual(idOf(sent[1]), held, 'the retry must wear the id the queue was holding');
    assert.ok(
      r.outbox.entries.every((e) => e.req !== held),
      'once the core answers the replay, the held entry is settled and leaves the queue'
    );
  });
});

/*
 * plugin-r2: THE TRANSPORT'S OWN REFUSALS, AND THE TWO THAT A RETRY
 * CANNOT HELP. (design 7.6.50, ruled 2026-09-19)
 *
 * The thin client answers `not-sent` only when it can prove no byte
 * left, and relays such an answer under its own name. Four of those
 * names join the "not now" family -- the store never saw the bytes, so
 * the same request goes again. Two others are settings: the store
 * directory, and the length of the run-root path the socket is computed
 * under. Neither changes because time passed.
 */
describe('plugin-r2 S-transport a refusal from the transport is not a refusal of the write', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('keeps the entry and retries when nothing was sent', async () => {
    const r = rig([{ match: ['set'], stdout: '(error connect-failed (errno 2) (carried-out no))\n', rc: 75 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'pending');
    assert.strictEqual(r.outbox.entries.length, 1, 'a send that never left settled the entry');
    assert.strictEqual(r.outbox.entries[0].req, outcome.req, 'the retry would go under a new id');
  });

  /*
   * NOTE: PARKED AT ONCE, NOT AFTER FIVE. Five attempts against a store
   * directory that does not exist are five identical failures and a
   * slower arrival at the same place. The message names the setting and
   * what to do with it, because that is the only thing that can change
   * the answer.
   */
  it('parks a refusal that only a setting can answer, with the setting named', async () => {
    const r = rig([{ match: ['set'], stdout: '(error store-not-found (store "/nowhere"))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.keptForAPerson, true);
    const held = r.outbox.entries;
    assert.strictEqual(held.length, 1, 'the save was thrown away rather than kept');
    assert.strictEqual(held[0].state, 'parked');
    assert.match(String(held[0].lastError), /theourgia\.store/);
    assert.match(outcome.message, /theourgia\.store/);
  });

  it('parks a socket path the operating system will not take, and says what to shorten', async () => {
    const r = rig([
      { match: ['set'], stdout: '(error socket-path-too-long (path "/x") (length 126) (max 104))\n', rc: 75 }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(r.outbox.entries[0].state, 'parked');
    assert.match(outcome.message, /THEOURGIA_RUN/);
  });

  /*
   * NOTE: A NAME THIS BUILD HAS NEVER HEARD, FROM THE CLIENT'S OWN EXIT
   * CODE, IS UNKNOWN AND NOT REFUSED.
   *
   * When a daemon fails to start the client answers with the last
   * `(error ...)` in the log it just wrote -- whatever the core happened
   * to write there. The set of names that can arrive that way is not a
   * list this extension can hold. Exit 75 says the client refused on its
   * own account; an unrecognised name at 75 is a transport-side refusal
   * this build has not met, and recording it as `refused` would be
   * recording a determination nobody made.
   */
  it('reads an unknown name at the client’s own exit code as an outcome nobody knows', async () => {
    const r = rig([
      { match: ['set'], stdout: '(error some-name-from-a-newer-core (detail "x"))\n', rc: 75 }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'pending', 'an unknown transport refusal settled the entry');
    assert.strictEqual(r.outbox.entries.length, 1);
    assert.strictEqual(r.outbox.entries[0].req, outcome.req);
  });

  /*
   * NOTE: THE TWIN THAT KEEPS THE EXIT CODE MEANING SOMETHING. The same
   * unknown name at an ordinary exit code is the STORE refusing with a
   * word this build does not know -- an `(error ...)` is the protocol's
   * way of saying the write did not happen -- and that settles.
   */
  it('reads the same unknown name at an ordinary exit code as a refusal', async () => {
    const r = rig([
      { match: ['set'], stdout: '(error some-name-from-a-newer-core (detail "x"))\n', rc: 1 }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(outcome.status, 'refused');
  });

  /*
   * NOTE: THE GUARD BEHIND THE CLASSIFIER'S ANSWER. `classifyRefusal`
   * answers `refused` for both families so that the census over the
   * core's refusals does not read them as unclassified -- and nothing
   * in the running extension may ever reach that answer with one of
   * them. This row drives a real Saver with every name in both tables
   * and asserts the entry survived. Delete an interception in `send`
   * and exactly this reddens.
   */
  it('lets no name in either family settle the entry', async () => {
    for (const kind of [...RETRYABLE_REFUSALS, ...Object.keys(SETTINGS_REFUSALS)]) {
      const r = rig([{ match: ['set'], stdout: `(error ${kind} (detail "x"))\n`, rc: 75 }]);
      core = r.core;
      await r.saver.save('a.2', 'src', 'body2\n');
      assert.strictEqual(
        r.outbox.entries.length,
        1,
        `${kind} settled the entry; it is classified as a refusal and must be intercepted first`
      );
      core.dispose();
    }
  });
});

/*
 * plugin-r2: THE WAY BACK FROM A PARKED SETTING IS THE SETTING.
 *
 * An entry parked because the store directory does not exist is waiting
 * for one thing only: somebody to change the setting. So a configuration
 * change is what releases it -- once. If the setting still does not
 * answer, the same rule parks it again on the same attempt, which is why
 * "once" needs no counter.
 */
describe('plugin-r2 S-settings a configuration change releases what only a setting can release', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  const GONE = '(error store-not-found (store "/nowhere"))\n';

  it('sends a parked entry exactly once more, and parks it again when nothing was fixed', async () => {
    const r = rig([
      { match: ['set'], stdout: GONE, rc: 1, once: true },
      { match: ['set'], stdout: GONE, rc: 1, once: true }
    ]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(r.outbox.entries[0].state, 'parked');
    const beforeRelease = setCalls(core).length;

    const released = r.outbox.unparkAll();
    assert.strictEqual(released, 1, 'the release did not report what it released');
    await r.saver.retry();

    assert.strictEqual(
      setCalls(core).length,
      beforeRelease + 1,
      'a released entry went out a number of times other than once'
    );
    assert.strictEqual(
      r.outbox.entries[0].state,
      'parked',
      'the setting was not fixed, so the entry belongs back where it was'
    );
  });

  /*
   * NOTE: THE TWIN THAT KEEPS THE RELEASE FROM BEING A TIMER. Draining
   * without a release must not touch a parked entry -- that is the whole
   * point of parking it rather than leaving it pending.
   */
  it('does not send a parked entry on an ordinary drain', async () => {
    const r = rig([{ match: ['set'], stdout: GONE, rc: 1, once: true }]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'body2\n');
    const afterPark = setCalls(core).length;
    await r.saver.retry();
    assert.strictEqual(setCalls(core).length, afterPark, 'a parked entry went out without a release');
  });

  it('reports nothing to release when nothing is parked', async () => {
    const r = rig([{ match: ['set'], stdout: wrote(8), rc: 0 }]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'body2\n');
    assert.strictEqual(r.outbox.unparkAll(), 0);
  });
});

/*
 * plugin-r3 items 7, 7b and 7c: what a refusal carries after its name.
 *
 * The answers are quoted, not restated. `(error refused (instance
 * machine))` is 877f0da's answer to a `set` on a store created under
 * another THEOURGIA_HOME, measured by the main session on 2026-09-25;
 * the other two are the shapes queue item 7b quotes from the core's B2a.
 */
describe('plugin-r3 7 a refusal says what the core said after its name', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  const INSTANCE = (what: string): string => `(error refused (instance ${what}))\n`;

  it('carries the clauses after the name into the sentence', async () => {
    const answers: Array<[string, RegExp[]]> = [
      [
        '(error malformed-intent (field-value-not-text (field title) (kind symbol) (allowed (string))))\n',
        [/malformed-intent/, /field-value-not-text/, /\(field title\)/, /\(kind symbol\)/, /\(allowed \(string\)\)/]
      ],
      [
        '(error bad-request kind-not-known (kind "X") (known (a b)))\n',
        [/bad-request/, /kind-not-known/, /\(kind "X"\)/, /\(known \(a b\)\)/]
      ]
    ];
    for (const [answer, says] of answers) {
      const r = rig([{ match: ['set'], stdout: answer, rc: 1 }]);
      core = r.core;
      const outcome = await r.saver.save('a.2', 'src', 'body\n');
      assert.strictEqual(outcome.status, 'refused', `${answer.trim()} was not a refusal`);
      for (const pattern of says) {
        assert.match(outcome.message, pattern, `the sentence lost part of ${answer.trim()}: ${outcome.message}`);
      }
      core.dispose();
    }
  });

  it('says the remedy the core names in words, and does not print it twice', async () => {
    const r = rig([{ match: ['set'], stdout: '(error refused integrity (remedy adopt))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'refused');
    assert.match(outcome.message, /refused integrity/);
    assert.match(outcome.message, /The core names the remedy: adopt/);
    assert.doesNotMatch(outcome.message, /\(remedy adopt\)/, 'the remedy clause was printed as well as said');
    core.dispose();
    /*
     * AND IT SAYS THE REMEDY IT WAS GIVEN, not one it knows: 877f0da's
     * `remedy-for` names another one for a registry inside the store
     * (store.sc:2473).
     */
    const other = rig([
      {
        match: ['set'],
        stdout: '(error refused registry-inside-store (remedy move-the-registry-outside-the-store))\n',
        rc: 1
      }
    ]);
    core = other.core;
    const moved = await other.saver.save('a.2', 'src', 'body\n');
    assert.match(moved.message, /The core names the remedy: move-the-registry-outside-the-store/);
  });

  /*
   * KEY: A REMEDY THIS CLIENT CANNOT PUT INTO WORDS IS PRINTED, NOT DROPPED.
   * No such shape comes from 877f0da; the rule is that nothing after the
   * name disappears, and this is the case where the first version broke it.
   */
  it('prints a remedy it cannot put into words, rather than dropping it', async () => {
    for (const remedy of ['(remedy (adopt))', '(remedy)', '(remedy 0)']) {
      const r = rig([{ match: ['set'], stdout: `(error refused integrity ${remedy})\n`, rc: 1 }]);
      core = r.core;
      const outcome = await r.saver.save('a.2', 'src', 'body\n');
      assert.ok(
        outcome.message.includes(remedy),
        `${remedy} is neither said nor printed: ${outcome.message}`
      );
      assert.doesNotMatch(outcome.message, /The core names the remedy/, `${remedy} was said as though it were a name`);
      core.dispose();
    }
  });

  /*
   * KEY: THE ONE SENTENCE WRITTEN FOR A NAME CARRIES THE REST AS WELL.
   * `(error changed (current ...))` is what store.sc makes of a stale
   * expectation (store.sc:2324 in 877f0da).
   */
  it('carries what follows changed into the sentence written for it', async () => {
    const r = rig([{ match: ['set'], stdout: '(error changed (current "hhh"))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.match(outcome.message, /the block changed in the store since it was opened/);
    assert.match(outcome.message, /\(current "hhh"\)/, `the clause after changed was dropped: ${outcome.message}`);
  });

  /*
   * KEY: AN INSTANCE MISMATCH IS KEPT, NOT SETTLED. A person fixes it by
   * restarting a daemon or adopting the store; a permanent refusal would
   * release the request and leave the text a draft nothing sends again.
   */
  it('keeps a save refused for an instance mismatch, parked, and says how to send it again', async () => {
    for (const what of ['machine', 'device', 'inode', 'nonce']) {
      const r = rig([{ match: ['set'], stdout: INSTANCE(what), rc: 1 }]);
      core = r.core;
      const outcome = await r.saver.save('a.2', 'src', 'body\n');
      assert.strictEqual(outcome.status, 'refused', `(${what}) status`);
      assert.strictEqual(
        (outcome as { keptForAPerson?: true }).keptForAPerson,
        true,
        `(${what}) an instance mismatch was settled as a permanent refusal`
      );
      assert.deepStrictEqual(
        r.outbox.entries.map((e) => e.state),
        ['parked'],
        `(${what}) the save did not stay in the queue, parked`
      );
      assert.match(outcome.message, new RegExp(`\\(instance ${what}\\)`), `(${what}) the sentence lost the clause`);
      assert.match(outcome.message, /theourgia: Retry Pending Saves/, `(${what}) the sentence does not say how to go on`);
      assert.match(outcome.message, /THEOURGIA_HOME/, `(${what}) the sentence does not name the home to run under`);
      assert.match(outcome.message, /`adopt`/, `(${what}) the sentence does not name adopt`);
      core.dispose();
    }
  });

  it('settles a reason at position 2 that is not an instance clause as it always did', async () => {
    /*
     * Three of the other reasons 877f0da puts after `refused`, the first
     * two a symbol that merely begins like the family (log.sc:5332, 5338).
     */
    for (const reason of ['instance-malformed', 'no-instance', 'integrity']) {
      const r = rig([{ match: ['set'], stdout: `(error refused ${reason})\n`, rc: 1 }]);
      core = r.core;
      const outcome = await r.saver.save('a.2', 'src', 'body\n');
      assert.strictEqual(outcome.status, 'refused', `(${reason}) status`);
      assert.notStrictEqual((outcome as { keptForAPerson?: true }).keptForAPerson, true, `(${reason}) kept`);
      assert.strictEqual(r.outbox.entries.length, 0, `(${reason}) a reason nobody ruled into the settings family was kept`);
      assert.match(outcome.message, new RegExp(`refused ${reason}`));
      core.dispose();
    }
  });

  /*
   * THE WAY BACK, BOTH HALVES (ruled 2026-09-25). Once the store's
   * environment is fixed, the command sends the parked save and it
   * settles; while it is not, the command leaves it parked, and a later
   * save of the same block waits behind it rather than being lost or sent
   * around it.
   */
  it('sends a parked instance save once the environment is fixed, and settles it', async () => {
    const r = rig([
      { match: ['set'], stdout: INSTANCE('machine'), rc: 1, once: true },
      { match: ['set'], stdout: wrote(8), rc: 0 }
    ]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'body\n');
    const held = r.outbox.entries[0].req;
    const outcomes = await r.saver.retryParked();
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 2, 'the parked save was not sent again');
    assert.strictEqual(sent[1][sent[1].indexOf('--req') + 1], held, 'it went again under another request id');
    assert.ok(outcomes.some((o) => o.status === 'saved'), `nothing settled: ${JSON.stringify(outcomes.map((o) => o.status))}`);
    assert.strictEqual(r.outbox.entries.length, 0, 'the settled save stayed in the queue');
    /*
     * AND IT SETTLED AS CONFIRMED: the store's record became the cursor the
     * next save is composed against. A save settled any other way leaves
     * the queue just the same, so the next send is what tells them apart.
     */
    await r.saver.save('a.2', 'src', 'after\n');
    const next = setCalls(core);
    assert.strictEqual(next[2][next[2].indexOf('--cursor') + 1], 'w:8', 'the retried save did not move the cursor');
  });

  it('leaves it parked while the environment is not fixed, with the next save of the block waiting', async () => {
    const r = rig([{ match: ['set'], stdout: INSTANCE('machine'), rc: 1 }]);
    core = r.core;
    await r.saver.save('a.2', 'src', 'first\n');
    await r.saver.save('a.2', 'src', 'second\n');
    assert.strictEqual(setCalls(core).length, 1, 'the second save went around the parked first one');
    await r.saver.retryParked();
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 2, `expected the parked save to be tried once more: ${sent.length} sends`);
    assert.strictEqual(sent[1][3], 'first\n', 'the retry sent the later save before the parked one');
    assert.deepStrictEqual(
      r.outbox.entries.map((e) => e.state),
      ['parked', 'queued'],
      'a save was lost, or the block\'s second save was sent around the first'
    );
    assert.deepStrictEqual(
      r.outbox.entries.map((e) => e.payload),
      ['first\n', 'second\n'],
      'the two saves no longer hold their own bodies'
    );
  });
});

/*
 * plugin-r3 item 2, end to end through a Saver: the core's F45 `local-writer`
 * decides the first cursor of a store with more than one writer -- the case
 * this client refused until now -- and an answer that contradicts itself is
 * refused in words, with nothing sent.
 */
describe('plugin-r3 2 a store with several writers, when the core names the local one', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  const TWO =
    '(check (store "s") (local-writer "LOCAL") (writers (("w" (end 7) (torn #f) (integrity ())) ' +
    '("LOCAL" (end 3) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';

  function over(check: string, calls: ScriptedCall[]): Rig {
    const made = new FakeCore([{ match: ['check'], stdout: check, rc: 0 }, ...calls]);
    const outbox = new Outbox(made.outboxFile());
    outbox.load();
    const client = new Client(new CliTransport(made.config(), made.env()));
    return { core: made, outbox, saver: new Saver(client, outbox, settling(outbox)) };
  }

  it('saves against the named local writer of a store with two writers', async () => {
    const r = over(TWO, [
      { match: ['set'], stdout: '(ok (events (("LOCAL" . 4))) (state (("a.2" . "hhh"))) (cursor ("LOCAL" . 4)) (replay #f))\n', rc: 0 }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'saved', outcome.message);
    const sent = setCalls(core);
    assert.strictEqual(sent.length, 1);
    assert.strictEqual(sent[0][sent[0].indexOf('--cursor') + 1], 'LOCAL:3', 'the save was not composed against the local writer');
  });

  it('sends nothing and says why when the named local writer is not among the writers', async () => {
    const r = over(TWO.replace('(local-writer "LOCAL")', '(local-writer "stranger")'), [
      { match: ['set'], stdout: wrote(8), rc: 0 }
    ]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /contradicts itself/);
    assert.match(outcome.message, /stranger/);
    assert.strictEqual(setCalls(core).length, 0, 'a save was sent against an answer that contradicts itself');
  });

  /*
   * KEY: TWO THINGS THAT CANNOT BE READ, TWO SENTENCES, EACH TRUE. Review r1
   * of this item measured an unreadable `local-writer` clause reported as an
   * unreadable writer LISTING with an unknown count -- about a listing that
   * had read, one writer.
   */
  it('says it could not read which writer is local, not that the listing could not be read', async () => {
    const r = over(TWO.replace('(local-writer "LOCAL")', '(local-writer #f)'), [{ match: ['set'], stdout: wrote(8), rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /naming its local writer in a form this build could not read/);
    assert.doesNotMatch(outcome.message, /writer listing/, 'the sentence blamed the listing');
    assert.strictEqual(setCalls(core).length, 0);
  });

  /*
   * AND THE LISTING'S SENTENCE CLAIMS NOTHING IT CANNOT KNOW. Here both
   * writers are named and only LOCAL's end is unreadable: their number is
   * known, so a sentence saying it is not would be false (review r2 of this
   * item).
   */
  it('says the listing could not be read in full when it is the listing, and claims no count', async () => {
    const r = over(TWO.replace('(end 3)', '(end "three")'), [{ match: ['set'], stdout: wrote(8), rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'blocked');
    assert.match(outcome.message, /a writer listing this build could not read in full/);
    assert.doesNotMatch(outcome.message, /how many writers/, 'the sentence claimed the count was unknown');
    assert.doesNotMatch(outcome.message, /naming its local writer/, 'the sentence blamed the clause');
  });
});
