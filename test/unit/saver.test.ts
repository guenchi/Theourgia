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
