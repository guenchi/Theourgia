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
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Client } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { Saver } from '../../src/saver';
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
  return { core, outbox, saver: new Saver(client, outbox) };
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
     * The outbox's directory is occupied by a FILE, so making it fails
     * and the entry cannot be recorded. The cell is about the order of
     * the two steps, and the only way to see an order is to break the
     * first one.
     */
    const blocked = path.join(core0.root, 'occupied');
    fs.writeFileSync(blocked, 'not a directory\n', 'utf8');
    const outbox = new Outbox(path.join(blocked, 'outbox.json'));
    const saver = new Saver(new Client(new CliTransport(core0.config(), core0.env())), outbox);
    await assert.rejects(() => saver.save('a.2', 'src', 'body\n'));
    assert.deepStrictEqual(setCalls(core0), [], 'a save was sent that had not been written down');
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
    const saver = new Saver(new Client(new CliTransport(core0.config(), core0.env())), outbox);
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

    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), restarted);
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
      r.outbox
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

  it('drops the entry when the id already names a different request', async () => {
    const { outcome, outbox } = await outcomeOf('(error req-mismatch ("w" . 6))\n', 1);
    assert.strictEqual(outcome.status, 'refused');
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('drops the entry when the cursor names an unreachable position', async () => {
    const { outcome, outbox } = await outcomeOf(
      '(error cursor-unreachable (after ("w" . 999)) (writing ("w" . 8)))\n',
      1
    );
    assert.strictEqual(outcome.status, 'refused');
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('drops the entry and says the block moved when the store refuses a stale write', async () => {
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
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), restarted);
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
      outbox
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

    const interrupted = new Outbox(file);
    interrupted.load();
    interrupted.enqueue({
      req: 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee',
      cursor: 'w:9',
      id: 'a.2',
      field: 'src',
      payload: 'second\n',
      state: 'sent',
      createdAt: 0,
      lastError: null
    });
    interrupted.setCursor('w:9');

    const reloaded = new Outbox(file);
    reloaded.load();
    assert.strictEqual(reloaded.pendingCount, 1, 'the interrupted entry was not kept');
    assert.strictEqual(reloaded.entries[0].state, 'sent');
    assert.strictEqual(reloaded.cursor, 'w:9', 'the cursor was not persisted');

    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded);
    const outcomes = await saver.retry();
    assert.strictEqual(outcomes[0].status, 'saved', outcomes[0].message);
    const sent = setCalls(core);
    assert.strictEqual(sent[1][5], 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', 'the retry changed the request id');
    assert.strictEqual(sent[1][7], 'w:9', 'the retry changed the cursor of a request already sent');
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
      lastError: null
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
      lastError: null
    });
    outbox.aboutToSend('aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', 'w:40');
    assert.strictEqual(outbox.entries[0].cursor, 'w:40');
    assert.strictEqual(outbox.entries[0].state, 'sent');
  });
});

describe('S12 two entries found on disk after a restart', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function twoEntries(file: string): Outbox {
    const outbox = new Outbox(file);
    outbox.load();
    outbox.setCursor('w:7');
    for (const [req, payload] of [
      ['11111111-1111-1111-1111-111111111111', 'first\n'],
      ['22222222-2222-2222-2222-222222222222', 'second\n']
    ]) {
      outbox.enqueue({
        req,
        cursor: 'w:7',
        id: 'a.2',
        field: 'src',
        payload,
        state: 'sent',
        createdAt: 0,
        lastError: null
      });
    }
    return outbox;
  }

  it('sends them in the order they were written down', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s12-')), 'outbox.json');
    twoEntries(file);
    const r = rig(
      [
        { match: ['set'], contains: ['first\n'], stdout: wrote(9), rc: 0 },
        { match: ['set'], contains: ['second\n'], stdout: wrote(10), rc: 0 }
      ],
      file
    );
    core = r.core;
    const reloaded = new Outbox(file);
    reloaded.load();
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded);
    const outcomes = await saver.retry();
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['saved', 'saved']);
    const bodies = setCalls(core).map((c) => c[3]);
    assert.deepStrictEqual(bodies, ['first\n', 'second\n'], 'the restart sent them out of order');
    assert.strictEqual(reloaded.pendingCount, 0);
  });

  it('holds the second back when the first cannot be resolved', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-s12b-')), 'outbox.json');
    twoEntries(file);
    const r = rig(
      [
        { match: ['set'], contains: ['first\n'], stdout: '(error unknown (chain-unreadable))\n', rc: 1 },
        { match: ['set'], contains: ['second\n'], stdout: wrote(10), rc: 0 }
      ],
      file
    );
    core = r.core;
    const reloaded = new Outbox(file);
    reloaded.load();
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), reloaded);
    const outcomes = await saver.retry();
    assert.deepStrictEqual(outcomes.map((o) => o.status), ['pending']);
    assert.deepStrictEqual(
      setCalls(core).map((c) => c[3]),
      ['first\n'],
      'the second went out past an unresolved first'
    );
    assert.strictEqual(reloaded.pendingCount, 2);
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
    const other = new Saver(new Client(new CliTransport(core.config(), core.env())), second);

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
