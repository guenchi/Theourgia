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
 * WHO, WHAT AND WHICH SEND -- READ ONCE, AT ONE MOMENT. (section 13.1)
 *
 * Four rounds of review found four faces of one defect on the save
 * path, and the shape has a name: WHO (which store, which queue), WHAT
 * (which file, which bytes) and WHICH SEND (which request) were each
 * read at a DIFFERENT moment. The live configuration was read once
 * before a wait and again after it; a per-block map in memory outlived
 * the rebuild that replaced everything around it; the settlement
 * carried a cursor but not a verdict. Every repair pinned one pair and
 * left the third face for the next letter.
 *
 * section 13 answers it by capturing all three at the instant the save is
 * accepted -- inside the chain's critical section, where `decide` says
 * send -- into one immutable record that everything downstream reads
 * and nothing downstream re-derives.
 *
 * This file holds the part of that which is a question about the SHAPE
 * of the source rather than about a run: where the live configuration
 * may be read, and whether anything still remembers a save somewhere
 * other than the queue. Both are read from the syntax tree. A regular
 * expression finds `config.store` in a comment -- there is one in
 * `settling.ts` this very moment, describing the defect that comment
 * was written about -- and cannot say whether a read sits before or
 * after a wait, which is the whole question.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';
import { Client } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { RecordParts, SendRecord, recordFor } from '../../src/record';
import { SaveOutcome, Saver, Settle } from '../../src/saver';
import { CliTransport, TransportError } from '../../src/transport';
import { FileOps, nodeFileOps } from '../../src/fsops';
import { initWire } from '../../src/wire';
import { FakeCore, ScriptedCall } from '../support/fake';
import { wroteAnswer } from '../support/answers';

const SRC = path.join(__dirname, '..', '..', '..', 'src');

function parse(name: string): ts.SourceFile {
  return ts.createSourceFile(
    name,
    fs.readFileSync(path.join(SRC, name), 'utf8'),
    ts.ScriptTarget.ES2022,
    true
  );
}

function within<T extends ts.Node>(root: ts.Node, is: (n: ts.Node) => n is T): T[] {
  const found: T[] = [];
  const walk = (n: ts.Node): void => {
    if (is(n)) {
      found.push(n);
    }
    ts.forEachChild(n, walk);
  };
  walk(root);
  return found;
}

function isFunction(n: ts.Node): boolean {
  return (
    ts.isFunctionDeclaration(n) ||
    ts.isFunctionExpression(n) ||
    ts.isArrowFunction(n) ||
    ts.isMethodDeclaration(n)
  );
}

/*
 * THE FUNCTION IS FOUND BY NAME AND THE NAME IS CHECKED. A census that
 * silently finds nothing is a census that passes for ever; every cell
 * here asserts that its subject was located before it asserts anything
 * about it.
 */
function functionNamed(src: ts.SourceFile, name: string): ts.Node {
  const all: ts.Node[] = [];
  const walk = (n: ts.Node): void => {
    if (isFunction(n)) {
      const named = n as ts.FunctionDeclaration;
      if (named.name !== undefined && named.name.getText(src) === name) {
        all.push(n);
      }
    }
    ts.forEachChild(n, walk);
  };
  walk(src);
  assert.strictEqual(
    all.length,
    1,
    `${src.fileName} holds ${all.length} functions called ${name}; this census reads exactly one`
  );
  return all[0];
}

function firstWaitIn(fn: ts.Node, src: ts.SourceFile): number {
  const awaits: ts.Node[] = within(fn, ts.isAwaitExpression);
  const forAwaits = within(fn, ts.isForOfStatement).filter((n) => n.awaitModifier !== undefined);
  const at = [...awaits, ...forAwaits].map((n) => n.getStart(src)).sort((a, b) => a - b);
  assert.ok(at.length > 0, 'the function this census reads has no wait in it at all');
  return at[0];
}

/*
 * A READ OF THE LIVE CONFIGURATION: `config.<anything>`. The binding is
 * a module-level `let` that `rebuild` reassigns, so reading a property
 * off it is exactly the act of asking what the settings say NOW.
 */
function liveConfigReads(fn: ts.Node): ts.PropertyAccessExpression[] {
  return within(fn, ts.isPropertyAccessExpression).filter(
    (n) => ts.isIdentifier(n.expression) && n.expression.text === 'config'
  );
}

describe('the save path reads who, what and which send at one moment', () => {
  /*
   * NOTE: "NEVER READ THE SETTINGS AFTER A WAIT" IS NOT THE RULE, and
   * writing it that way is how the exception ends up unwritten. There is
   * exactly one place that must read them -- the check that the file's
   * own store is still the one this window is configured for -- and
   * section 13.1 puts it inside the chain callback, in the same critical
   * section as `decide`, because that is the instant the save is
   * accepted. Outside the callback the answer can change between the
   * check and the record.
   *
   * So the rule is a COUNT AND A PLACE, both pinned: one more, one
   * fewer, or the same one moved out of the callback all fail here.
   */
  it('reads the live configuration before W saving and again at acceptance', () => {
    const src = parse('extension.ts');
    const onSaved = functionNamed(src, 'onSaved');
    const after = liveConfigReads(onSaved).filter((n) => n.getStart(src) > firstWaitIn(onSaved, src));
    /*
     * THE EXPECTATION IS THE TEXT, NOT THE LINE. A line number in an
     * expectation turns every edit above it into a failure, and the
     * failure says nothing about what this cell is for. Where the read
     * sits is the next cell's question, and it asks it by position in
     * the syntax tree rather than by counting lines.
     */
    assert.deepStrictEqual(
      after.map((n) => n.getText(src)),
      ['config.store','config.store'],
      'onSaved reads the live configuration after its first wait somewhere other than the one ' +
        'place §13.1 allows -- the store check inside the chain callback. Found at ' +
        after
          .map((n) => `extension.ts:${src.getLineAndCharacterOfPosition(n.getStart(src)).line + 1}`)
          .join(', ')
    );
  });

  it('takes both readings inside the captured chain callback', () => {
    const src = parse('extension.ts');
    const onSaved = functionNamed(src, 'onSaved');
    const read = liveConfigReads(onSaved).filter((n) => n.getStart(src) > firstWaitIn(onSaved, src));
    assert.strictEqual(read.length, 2, 'W preparation and final acceptance each require a store check');

    const runs = within(onSaved, ts.isCallExpression).filter(
      (n) =>
        ts.isPropertyAccessExpression(n.expression) &&
        n.expression.name.getText(src) === 'run' &&
        ts.isIdentifier(n.expression.expression) &&
        n.expression.expression.text === 'chain'
    );
    assert.ok(runs.length > 0, 'onSaved does not call chain.run at all, so this census is not reading the save path');
    const inside = read.every(r=>runs.some(
      call => r.getStart(src) > call.getStart(src) && r.getEnd() < call.getEnd()
    ));
    assert.ok(
      inside,
      'the one live-configuration read in onSaved sits outside chain.run, so the store it checks ' +
        'can change between that check and the record the save is captured into'
    );
  });

  /*
   * NOTE: AND THE SETTLER READS NOTHING LIVE AT ALL. It is handed the
   * record; the record carries the store the FILE says it belongs to.
   * A settler that consults the settings is a settler whose answer
   * depends on what the user did while the store was thinking.
   */
  it('leaves the settler with nothing live to read', () => {
    const src = parse('settling.ts');
    const reads = within(src, ts.isPropertyAccessExpression).filter(
      (n) => ts.isIdentifier(n.expression) && n.expression.text === 'config'
    );
    assert.deepStrictEqual(
      reads.map((n) => `${n.getText(src)} (settling.ts:${src.getLineAndCharacterOfPosition(n.getStart(src)).line + 1})`),
      [],
      'the settler reads the live configuration; §13.1 gives it the record instead'
    );
  });

  /*
   * NOTE: AND NEITHER DOES THE DRAIN. It runs on a schedule of its own --
   * `rebuild` starts one, and a retry command starts another -- so
   * whatever it read before its first wait belongs to a window that may
   * have moved on by the time an answer comes back. Everything it needs
   * is in the entries.
   */
  it('leaves the drain with nothing live to read after it waits', () => {
    const src = parse('extension.ts');
    const scheduled = within(src, ts.isArrowFunction).filter((fn) => {
      const waits = within(fn, ts.isAwaitExpression);
      return waits.length > 0 && /\bdraining\b/.test(fn.getText(src));
    });
    assert.ok(
      scheduled.length > 0,
      'this census found no scheduled drain in extension.ts, so it is not reading the save path'
    );
    const late: string[] = [];
    for (const fn of scheduled) {
      const first = firstWaitIn(fn, src);
      for (const read of liveConfigReads(fn)) {
        if (read.getStart(src) > first) {
          late.push(`${read.getText(src)} (extension.ts:${src.getLineAndCharacterOfPosition(read.getStart(src)).line + 1})`);
        }
      }
    }
    assert.deepStrictEqual(
      late,
      [],
      'the drain reads the live configuration after a wait; everything it needs is in the entries'
    );
  });

  /*
   * NOTE: THE MEMORY IS THE QUEUE, AND THERE IS NO OTHER ONE. (section 13.1)
   *
   * `pendingSaves` was a map from block id to what the save was about,
   * and `recovered()` was a second supplier of the same thing for
   * answers that arrived after a restart. Two suppliers of one fact
   * disagree the first time their conditions are spelled differently --
   * and these two were, which is how an answer settled against the
   * wrong file. section 13 deletes both: the queue entry IS the record, so the
   * in-flight path and the after-a-restart path read the same bytes.
   *
   * This is a symbol census rather than a grep: `pendingSaves` appears
   * in comments describing what was removed, and those must be allowed
   * to stay.
   */
  it('keeps no memory of a save outside the queue', () => {
    const gone = ['pendingSaves', 'recovered'];
    const found: string[] = [];
    for (const name of fs.readdirSync(SRC).filter((f) => f.endsWith('.ts'))) {
      const src = parse(name);
      for (const id of within(src, ts.isIdentifier)) {
        if (gone.includes(id.text)) {
          found.push(`${id.text} (${name}:${src.getLineAndCharacterOfPosition(id.getStart(src)).line + 1})`);
        }
      }
    }
    assert.deepStrictEqual(
      found,
      [],
      'these are the second supplier of what a save was about; §13.1 makes the queue entry the ' +
        'only one'
    );
  });
});

const CHECK =
  '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';

/*
 * A SAVER OVER A FAKE CORE, and the queue it writes to on disk. The
 * settler here releases the entry the way the product's does for the
 * verdicts these cells reach; what they are about is the record, not
 * the recording.
 */
/*
 * NOTE: THE STAND-IN SETTLER CARRIES THE STORE THE REAL ONE CARRIES.
 *
 * `settlerFor` attaches the queue and the store hash it was built for,
 * and the Saver reads them to refuse a record that belongs elsewhere. A
 * bare callback carries neither -- which is a door left open on purpose
 * for the cells about sending -- so a cell about the refusal has to
 * hand over a settler shaped like the shipping one, or it would be
 * measuring the open door.
 */
function settling(outbox: Outbox, storeHash?: string): Settle & { storeHash?: string } {
  const settle: Settle & { storeHash?: string } = (req, settlement) => {
    if (settlement.verdict === 'req-mismatch') {
      return;
    }
    outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
  };
  settle.storeHash = storeHash;
  return settle;
}

interface Rig {
  core: FakeCore;
  outbox: Outbox;
  saver: Saver;
  queuePath: string;
}

function rig(
  calls: ScriptedCall[],
  storeHash: string = parts().storeHash,
  baselineOf?: (file: string) => { highWater: number } | null
): Rig {
  const core = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, ...calls]);
  const queuePath = core.outboxFile();
  const outbox = new Outbox(queuePath);
  outbox.load();
  const client = new Client(new CliTransport(core.config(), core.env()));
  return {
    core,
    outbox,
    queuePath,
    saver: new Saver(client, outbox, settling(outbox, storeHash), { baselineOf })
  };
}

/*
 * THE PARTS OF A RECORD, spelled out once so that a cell changing one
 * of them says which one it changed. Every field is given a value that
 * could not be confused with another field's, because a record whose
 * `rawDigest` and `sentDigest` are the same string cannot tell a build
 * that swapped them from a build that did not.
 */
function parts(over: Partial<RecordParts> = {}): RecordParts {
  return {
    req: '11111111-2222-3333-4444-555555555555',
    store: '/tmp/store-a',
    storeHash: 'hash-of-store-a',
    blockId: 'a.2',
    file: '/tmp/session/a.2/1.md',
    rawDigest: 'raw-on-disk',
    sentDigest: 'sent-body',
    prefixDigest: 'prefix-of-the-file',
    seq: 4,
    intent: { verb: 'set', field: 'src', expectation: null, body: 'body2\n' },
    ...over
  };
}

function sentCalls(core: FakeCore): string[][] {
  return core.requests().filter((r) => r[0] === 'set');
}

describe('R2 the record is the queue entry', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * NOTE: THE ENTRY CARRIES THE RECORD, not a copy of the two fields the
   * sender happens to need. (section 13.1)
   *
   * What a save was about used to live in a map in memory, keyed by
   * block id, filled in by the save and read by the answer. A restart
   * emptied it, so a second supplier -- `recovered()` -- reconstructed
   * an approximation from the entry, and the two spelled their
   * conditions differently. They disagreed, and an answer settled
   * against a file it did not belong to. The queue is on disk, so an
   * entry that IS the record has no second supplier to disagree with.
   */
  it('puts every field of the record into the entry on disk', async () => {
    /*
     * THE CORE DOES NOT ANSWER, so the entry is still there to be read.
     * A send that settles removes its own entry -- which is what should
     * happen, and would leave this cell with nothing to look at.
     *
     * NOTE: AND THE BYTES ARE READ, NOT THE READER'S ANSWER. What the queue
     * hands back after parsing is the next cell's question; this one is
     * whether the fields reached the file at all. A writer that dropped
     * a field and a reader that filled it back in would agree with each
     * other and disagree with every other build.
     */
    const r = rig([{ match: ['set'], stdout: '', rc: 7 }]);
    core = r.core;
    const record = recordFor(parts());
    await r.saver.submit(record);

    const held = JSON.parse(fs.readFileSync(r.queuePath, 'utf8')) as {
      entries: Array<{ req: string; record?: Record<string, unknown> }>;
    };
    const mine = held.entries.find((e) => e.req === record.req);
    assert.ok(mine !== undefined, 'the queue file holds no entry for the record that was submitted');
    assert.deepStrictEqual(
      mine.record,
      JSON.parse(JSON.stringify(record)),
      'the entry written to disk does not carry the record, so an answer arriving after a restart ' +
        'would have to reconstruct what the save was about from something else'
    );
  });

  /*
   * NOTE: IMMUTABILITY HAS TO BE PROVED TO THE VALUE, not to the name.
   *
   * A shallow freeze, a shadow copy under a different name, or a field
   * that is only frozen on the way in all pass a cell that asserts
   * "the symbol is not assigned anywhere". This one goes at the record
   * through every route that exists -- the object the caller still
   * holds, the nested intent that carries the bytes, the entry the
   * queue hands back from `entries` and from `find`, and the same
   * entries after a reload from disk -- and then asks what was sent.
   */
  it('refuses every route to changing the record after it is accepted', async () => {
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }]);
    core = r.core;
    const record = recordFor(parts());
    const body = record.intent.body;

    const shove = (at: SendRecord | undefined): void => {
      if (at === undefined) {
        return;
      }
      /*
       * EACH ASSIGNMENT MAY THROW OR MAY BE IGNORED, and which one
       * depends on whether this file is strict. The cell is about the
       * value afterwards, so both are allowed here and the values are
       * asserted below.
       */
      try {
        (at as { store: string }).store = '/tmp/store-b';
      } catch (e) {
        void e;
      }
      try {
        (at.intent as { body: string }).body = 'not what the user saved\n';
      } catch (e) {
        void e;
      }
    };

    shove(record);
    await r.saver.submit(record);
    const reloaded = new Outbox(r.queuePath);
    reloaded.load();
    shove(reloaded.entries[0]?.record);
    shove(reloaded.find(record.req)?.record);

    assert.strictEqual(record.store, '/tmp/store-a', 'the record the caller holds was changed');
    assert.strictEqual(record.intent.body, body, 'the bytes inside the record were changed');
    const sent = sentCalls(core);
    assert.strictEqual(sent.length, 1, 'the record was sent a number of times other than once');
    assert.strictEqual(sent[0][3], body, 'the bytes that went to the store are not the bytes the record was made with');
  });
});

describe('R3 the request id is made when the save is accepted, not when it is sent', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * NOTE: THE ID USED TO BE MADE INSIDE THE SENDER, which is after the
   * wait. The record names the send; a name given later belongs to a
   * different moment than the thing it names, and there was no way to
   * write down "this file, these bytes, this request" as one fact.
   */
  it('sends the id the record carries rather than making one of its own', async () => {
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }]);
    core = r.core;
    const record = recordFor(parts({ req: '99999999-8888-7777-6666-555555555555' }));
    const outcome = await r.saver.submit(record);

    const sent = sentCalls(core);
    assert.strictEqual(sent.length, 1);
    assert.strictEqual(sent[0][4], '--req');
    assert.strictEqual(
      sent[0][5],
      record.req,
      'the request id on the wire is not the one the record was accepted with'
    );
    assert.strictEqual(outcome.req, record.req, 'the outcome names a request the record does not');
  });

  /*
   * NOTE: TWO SENDS OF ONE BLOCK ARE TWO THINGS, and what used to keep
   * them apart was a map keyed by block id -- so the second overwrote
   * the first, and the first's answer settled against whatever the
   * second was about. The record makes them two values; these cells
   * ask whether anything still collapses them.
   *
   * THE TWO ARE MADE DISTINGUISHABLE ON EVERY AXIS THAT MATTERS. A
   * fixture whose two records share a digest cannot tell an
   * implementation that kept them apart from one that kept only the
   * last one.
   */
  it('keeps two sends of one block apart when their bytes differ', async () => {
    const r = rig([
      { match: ['set'], stdout: wroteAnswer(8), rc: 0, once: true },
      { match: ['set'], stdout: wroteAnswer(9), rc: 0, once: true }
    ]);
    core = r.core;
    const first = recordFor(
      parts({
        req: 'aaaaaaaa-0000-0000-0000-000000000001',
        seq: 4,
        rawDigest: 'raw-one',
        sentDigest: 'sent-one',
        intent: { verb: 'set', field: 'src', expectation: null, body: 'one\n' }
      })
    );
    const second = recordFor(
      parts({
        req: 'bbbbbbbb-0000-0000-0000-000000000002',
        seq: 5,
        rawDigest: 'raw-two',
        sentDigest: 'sent-two',
        intent: { verb: 'set', field: 'src', expectation: null, body: 'two\n' }
      })
    );
    await r.saver.submit(first);
    await r.saver.submit(second);

    const sent = sentCalls(core);
    assert.strictEqual(sent.length, 2, 'the two sends did not both reach the store');
    assert.deepStrictEqual(
      sent.map((call) => [call[5], call[3]]),
      [
        [first.req, 'one\n'],
        [second.req, 'two\n']
      ],
      'a send went out under the other one’s request id or with the other one’s bytes'
    );
  });

  /*
   * THE SAME BYTES TWICE. Identical payloads are the case where every
   * accidental identity holds: a build that told the two apart by
   * comparing what was sent cannot tell these apart at all, and only
   * the request and the sequence number separate them.
   */
  it('keeps two sends of one block apart when their bytes are the same', async () => {
    const r = rig([
      { match: ['set'], stdout: wroteAnswer(8), rc: 0, once: true },
      { match: ['set'], stdout: wroteAnswer(9), rc: 0, once: true }
    ]);
    core = r.core;
    const same = { verb: 'set', field: 'src', expectation: null, body: 'the same words\n' };
    const first = recordFor(parts({ req: 'aaaaaaaa-0000-0000-0000-000000000001', seq: 4, intent: same }));
    const second = recordFor(parts({ req: 'bbbbbbbb-0000-0000-0000-000000000002', seq: 5, intent: same }));
    await r.saver.submit(first);
    await r.saver.submit(second);

    const sent = sentCalls(core);
    assert.strictEqual(sent.length, 2, 'one of two identical sends was swallowed');
    assert.deepStrictEqual(
      sent.map((call) => call[5]),
      [first.req, second.req],
      'two sends with the same bytes were not sent under their own request ids'
    );
  });

  /*
   * NOTE: AND A RECORD FOR ANOTHER STORE IS NOT THIS QUEUE'S BUSINESS.
   * (R7.2, I5)
   *
   * A queue carries one cursor and a cursor belongs to one store. A
   * record whose store is not this Saver's would be written into a
   * queue no Saver holds -- while a window configured for THAT store
   * may be writing it -- and two writers over one file is one of them
   * erased. The refusal is here as well as at the acceptance because a
   * rule kept at one end has no way to notice the second entrance.
   */
  it('refuses a record that belongs to another store', async () => {
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }]);
    core = r.core;
    const elsewhere = recordFor(parts({ store: '/tmp/store-b', storeHash: 'hash-of-store-b' }));
    const outcome = await r.saver.submit(elsewhere);
    assert.strictEqual(outcome.status, 'blocked', `the record was accepted: ${JSON.stringify(outcome)}`);
    assert.strictEqual(sentCalls(core).length, 0, 'a record for another store was sent from this queue');
    const reloaded = new Outbox(r.queuePath);
    reloaded.load();
    assert.strictEqual(reloaded.entries.length, 0, 'a record for another store was written into this queue');
  });

  /*
   * NOTE: THE ANSWER THAT ARRIVES AFTER A RESTART TAKES THE SAME ROAD AS
   * THE ONE THAT ARRIVES WHILE THE WINDOW IS UP. (section 13.1)
   *
   * It did not. In flight, the settler read a map in memory; after a
   * restart that map was empty, so a second supplier reconstructed an
   * approximation from the entry -- and the two spelled their
   * conditions differently. This cell is the pair of roads made into
   * one: a second Saver, built over the same queue file with nothing in
   * memory, retries and settles from the entry alone.
   */
  it('retries and settles from the entry alone after a restart', async () => {
    const first = rig([{ match: ['set'], stdout: '', rc: 7 }]);
    core = first.core;
    const record = recordFor(parts());
    await first.saver.submit(record);
    assert.strictEqual(
      first.outbox.find(record.req) !== undefined,
      true,
      'the unanswered send did not stay in the queue, so there is nothing for the restart to find'
    );

    const reborn = new Outbox(first.queuePath);
    reborn.load();
    const entry = reborn.find(record.req);
    assert.ok(entry !== undefined, 'the queue on disk has no entry for the send');
    assert.deepStrictEqual(
      entry.record,
      record,
      'the entry read back after a restart does not carry the record, so the answer would have ' +
        'to be matched against something reconstructed'
    );
  });
});

/*
 * R8 A SEND A LATER CONFIRMATION HAS OVERTAKEN. (section 13, r3-4)
 *
 * A queue can sit for a long time: a store that was unreachable, a
 * window that was closed, a takeover carrying work in. In that time
 * another window may have saved the same file and had it confirmed.
 * Sending the older bytes then would put them over the newer ones --
 * and the answer would come back carrying a number below the
 * high-water mark, where it settles WITHOUT becoming the baseline. The
 * work would look sent and would not be.
 */
describe('R8 a send the record has already moved past', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('parks it instead of sending it', async () => {
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], parts().storeHash, () => ({
      highWater: 9
    }));
    core = r.core;
    const record = recordFor(parts({ seq: 4 }));
    const outcome = await r.saver.submit(record);

    assert.strictEqual(sentCalls(core).length, 0, 'an overtaken send went to the store anyway');
    const held = new Outbox(r.queuePath);
    held.load();
    const entry = held.find(record.req);
    assert.ok(entry !== undefined, 'the overtaken send was thrown away rather than kept');
    assert.strictEqual(entry.state, 'parked');
    assert.match(String(entry.lastError), /later save of this block has been confirmed/);
    assert.strictEqual(outcome.status, 'pending', `the outcome does not say it is waiting: ${outcome.status}`);
  });

  /*
   * NOTE: THE CONTROL, ALONG THE ONE AXIS: the same story with the
   * high-water mark AT the send's own number rather than above it. A
   * build that parked everything -- or that compared with `>=` -- fails
   * here and passes the cell above.
   */
  it('sends one the record has not moved past', async () => {
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], parts().storeHash, () => ({
      highWater: 4
    }));
    core = r.core;
    await r.saver.submit(recordFor(parts({ seq: 4 })));
    assert.strictEqual(sentCalls(core).length, 1, 'a send that had not been overtaken was parked');
  });

  /*
   * NOTE: AND IT IS READ AGAIN BEFORE EVERY TRANSMISSION, not once when the
   * entry was made. This is the cell that separates "the baseline was
   * consulted" from "the result governs the sending": the queue holds
   * an entry that was fine when it was accepted, the persistent
   * baseline moves, and a retry must not send it.
   */
  it('reads the record again before a retry, not only when the save was accepted', async () => {
    let highWater = 0;
    const r = rig(
      [
        { match: ['set'], stdout: '', rc: 7, once: true },
        { match: ['set'], stdout: wroteAnswer(8), rc: 0, once: true }
      ],
      parts().storeHash,
      () => ({ highWater })
    );
    core = r.core;
    const record = recordFor(parts({ seq: 4 }));
    await r.saver.submit(record);
    assert.strictEqual(sentCalls(core).length, 1, 'the first transmission did not happen');

    /*
     * ANOTHER WINDOW SAVED THIS FILE AND HAD IT CONFIRMED while the
     * entry sat here.
     */
    highWater = 9;
    await r.saver.retry();
    assert.strictEqual(
      sentCalls(core).length,
      1,
      'the retry sent a send that had been overtaken since it was accepted'
    );
    const held = new Outbox(r.queuePath);
    held.load();
    assert.strictEqual(held.find(record.req)?.state, 'parked');
  });

  /*
   * NOTE: A `sent` ENTRY IS NOT AN EXECUTED ONE. It says this client put
   * the request on the wire, not that the store applied it -- which is
   * the whole reason the queue keeps it. A build that skipped the check
   * for an entry it had already sent would retry an overtaken send for
   * ever.
   */
  it('checks a send it has already put on the wire once', async () => {
    /*
     * AN ENTRY LEFT IN `sent` IS WHAT A WINDOW KILLED BETWEEN THE SEND
     * AND THE ANSWER LEAVES BEHIND -- so it is seeded the way a restart
     * finds it, rather than manufactured by a send that would have gone
     * on to mark it something else.
     */
    const r = rig([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], parts().storeHash, () => ({
      highWater: 9
    }));
    core = r.core;
    const record = recordFor(parts({ seq: 4 }));
    r.outbox.enqueue({
      req: record.req,
      cursor: 'w:7',
      id: record.blockId,
      field: record.intent.field,
      payload: record.intent.body,
      state: 'sent',
      createdAt: 0,
      lastError: null,
      importedBy: null,
      record
    });
    const seeded = new Outbox(r.queuePath);
    seeded.load();
    assert.strictEqual(seeded.find(record.req)?.state, 'sent', 'the fixture did not leave a sent entry');

    await r.saver.retry();
    assert.strictEqual(
      sentCalls(core).length,
      0,
      'a sent entry was retried past the record that overtook it; `sent` says this client put it on ' +
        'the wire, not that the store applied it'
    );
    const after = new Outbox(r.queuePath);
    after.load();
    assert.strictEqual(after.find(record.req)?.state, 'parked');
  });
});

/*
 * R9 THE QUEUE STEPS OVER WHAT IT CANNOT SEND, AND CARRIES ON.
 */
describe('R9 a parked entry does not stop the other blocks', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('steps over a parked entry and sends another block’s', async () => {
    const r = rig(
      [{ match: ['set'], stdout: wroteAnswer(8, 'w', 'b.3'), rc: 0 }],
      parts().storeHash,
      (file: string) => (file.includes('a.2') ? { highWater: 9 } : { highWater: 0 })
    );
    core = r.core;
    const overtaken = recordFor(parts({ seq: 4, req: 'aaaaaaaa-0000-0000-0000-000000000001' }));
    const other = recordFor(
      parts({
        seq: 1,
        req: 'bbbbbbbb-0000-0000-0000-000000000002',
        blockId: 'b.3',
        file: '/tmp/session/b.3/1.md'
      })
    );
    await r.saver.submit(overtaken);
    await r.saver.submit(other);

    const sent = sentCalls(core);
    assert.deepStrictEqual(
      sent.map((call) => call[1]),
      ['b.3'],
      'the parked block’s send went out, or the other block’s did not'
    );
    const held = new Outbox(r.queuePath);
    held.load();
    assert.strictEqual(held.find(overtaken.req)?.state, 'parked', 'the overtaken send is not parked');
    assert.strictEqual(held.find(other.req), undefined, 'the other block’s send was not settled');
  });

  /*
   * NOTE: AND WITHIN ONE BLOCK THE ORDER HOLDS. A second save of the same
   * block means "and then this"; sending it past a parked one would
   * apply an edit to a version the store never received.
   */
  it('holds a later send of the same block behind the parked one', async () => {
    const r = rig(
      [{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }],
      parts().storeHash,
      () => ({ highWater: 9 })
    );
    core = r.core;
    await r.saver.submit(recordFor(parts({ seq: 4, req: 'aaaaaaaa-0000-0000-0000-000000000001' })));
    await r.saver.submit(recordFor(parts({ seq: 5, req: 'bbbbbbbb-0000-0000-0000-000000000002' })));
    assert.strictEqual(sentCalls(core).length, 0, 'a send went out past a parked one for the same block');
  });
});

/*
 * R9 A RETIRED REQUEST IS NOT SENT, FROM WHEREVER IT IS FOUND.
 * (section 13, r4-2, r5-3)
 *
 * NOTE: CANCELLING IS A TOMBSTONE, NOT A DEQUEUE. Removing an entry
 * removes it from ONE queue; the same request can sit in a dead
 * session's file that no takeover has reached, and the next takeover
 * carries the cancelled work back in. So the check is made HERE, in
 * front of the transmission, wherever the entry came from.
 *
 * NOTE: AND IT PARKS RATHER THAN DISCARDS. Whoever wrote the tombstone
 * does the bookkeeping that goes with a cancellation -- releasing the
 * send's number beside the file. A copy found later has no business
 * doing that a second time; what it must do is not send.
 */
describe('R9 a request that has been retired', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  function rigWith(
    calls: ScriptedCall[],
    retired: (record: SendRecord) => { known: true; retired: boolean } | { known: false }
  ): Rig {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, ...calls]);
    const queuePath = core0.outboxFile();
    const outbox = new Outbox(queuePath);
    outbox.load();
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    return {
      core: core0,
      outbox,
      queuePath,
      saver: new Saver(client, outbox, settling(outbox, parts().storeHash), { retired })
    };
  }

  it('is not sent, and is kept where it is', async () => {
    const r = rigWith([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], () => ({
      known: true,
      retired: true
    }));
    core = r.core;
    const record = recordFor(parts());
    await r.saver.submit(record);

    assert.strictEqual(sentCalls(core).length, 0, 'a retired request was sent');
    const held = new Outbox(r.queuePath);
    held.load();
    const entry = held.find(record.req);
    assert.ok(entry !== undefined, 'a retired request was thrown away here rather than parked');
    assert.strictEqual(entry.state, 'parked');
  });

  /*
   * NOTE: THE CONTROL. A build that parked everything passes the cell
   * above and fails this one.
   */
  it('sends one that has not been retired', async () => {
    const r = rigWith([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], () => ({
      known: true,
      retired: false
    }));
    core = r.core;
    await r.saver.submit(recordFor(parts()));
    assert.strictEqual(sentCalls(core).length, 1, 'a request nobody retired was not sent');
  });

  /*
   * NOTE: "I COULD NOT LOOK" IS NOT "NOTHING IS RETIRED". The directory
   * may be unreadable; the caller is deciding whether to SEND, and the
   * convenient reading of that is the one that resends cancelled work.
   */
  it('does not send when it could not tell', async () => {
    const r = rigWith([{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], () => ({ known: false }));
    core = r.core;
    const record = recordFor(parts());
    await r.saver.submit(record);

    assert.strictEqual(sentCalls(core).length, 0, 'a request that could not be checked was sent');
    const held = new Outbox(r.queuePath);
    held.load();
    assert.strictEqual(held.find(record.req)?.state, 'parked');
  });
});

/*
 * R4 THE NUMBER IS TAKEN BEFORE THE QUEUE IS WRITTEN, so the queue
 * write is the one that can fail with a number already spent. (section 13.1,
 * I7)
 *
 * NOTE: NOTHING WAS SENT, SO THE NUMBER HAS TO COME BACK. Leaving it in
 * `outstanding` would make the block a draft it can never stop being:
 * the record says a send is out, and there is no entry anywhere that
 * could ever answer for it.
 */
describe('R4 a send whose queue write did not land', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('says it was not queued, so the number can be given back', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }]);
    core = core0;
    /*
     * THE QUEUE IS READABLE AND NOT WRITABLE, and the cursor is
     * established while it still is -- otherwise the bootstrap's own
     * write is the first one to fail and this cell would be about that
     * instead.
     */
    const blocked = path.join(core0.root, 'readonly-queue');
    fs.mkdirSync(blocked, { recursive: true });
    const queuePath = path.join(blocked, 'outbox.json');
    const outbox = new Outbox(queuePath);
    outbox.load();
    outbox.setCursor('w:7');
    fs.chmodSync(blocked, 0o500);

    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, parts().storeHash));
    const outcome = await saver.submit(recordFor(parts()));
    fs.chmodSync(blocked, 0o700);

    assert.strictEqual(outcome.status, 'blocked', `the save was not reported as blocked: ${outcome.status}`);
    assert.strictEqual(
      outcome.notQueued,
      true,
      'the outcome does not say the entry never reached the queue, so the number it took stays out ' +
        'for ever and the block is a draft nothing can settle'
    );
    assert.strictEqual(sentCalls(core0).length, 0, 'something was sent for an entry that was never queued');
  });

  /*
   * KEY: AND SO DO THE TWO REFUSALS THAT COME BEFORE THE QUEUE WRITE.
   *
   * The flag was written on the refusal in front of the reviewer who
   * asked for it -- an unwritable queue -- and on neither of the two
   * above it, which return just as plainly before `enqueue`. A
   * sixteenth review round measured the cursor one:
   * `(check (writers ()))` gave blocked with no `notQueued`, an empty
   * queue, and the sequence still in the sidecar's `outstanding`. The
   * rule is positional, so both are asked here; a third refusal added
   * in front of the enqueue would want a row of its own.
   */
  it('says it was not queued for a store this queue is not for', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }]);
    core = core0;
    const outbox = new Outbox(path.join(core0.root, 'outbox.json'));
    outbox.load();
    outbox.setCursor('w:7');
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, 'hash-of-some-other-store'));
    const outcome = await saver.submit(recordFor(parts()));
    assert.strictEqual(outcome.status, 'blocked');
    assert.strictEqual(
      outcome.notQueued,
      true,
      'a save refused for belonging to another store kept the number it had taken'
    );
    assert.strictEqual(outbox.entries.length, 0, 'the entry reached the queue after all');
  });

  /*
   * KEY: AND AN EXCEPTION IS AN EXIT TOO.
   *
   * The rule was written as "every RETURN before the enqueue carries the
   * flag", and a seventeenth review round left through the door beside
   * it: `ensureCursor` asks the store, a `check` that times out rejects,
   * and `submit` rejected with the number already outstanding. The save
   * handler's catch reports and returns; `releaseSend` is reached only
   * for a resolved outcome.
   */
  it('says it was not queued when asking the store where its log ends failed', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: '', rc: 0 }]);
    core = core0;
    const outbox = new Outbox(path.join(core0.root, 'outbox.json'));
    outbox.load();
    const refusing = {
      kind: 'refusing',
      send: async (): Promise<never> => {
        throw new TransportError('timeout', 'the store did not answer the check in time', '');
      }
    };
    const saver = new Saver(new Client(refusing), outbox, settling(outbox, parts().storeHash));
    const outcome = await saver.submit(recordFor(parts()));
    assert.strictEqual(outcome.status, 'blocked', `the save was not blocked: ${outcome.message}`);
    assert.strictEqual(
      outcome.notQueued,
      true,
      'the bootstrap threw, so the outcome never said the entry had not been queued, and the ' +
        'number it took stays out for ever'
    );
    assert.strictEqual(outbox.entries.length, 0, 'the entry reached the queue after all');
  });

  /*
   * KEY: AND SO DOES A FAILURE THAT HAPPENS BEFORE `submit`'s OWN BODY
   * RUNS.
   *
   * `serialise` reloads the queue from disk after taking the lock and
   * before the callback -- so a queue file that will not read rejects
   * `submit` without anything in the callback having had a chance to
   * say so. Measured in an eighteenth review round with EACCES on the
   * queue: the save handler reported the rejection and returned, and the
   * number stayed in `outstanding`.
   *
   * This is the third round to find an exit of this kind, which is why
   * the answer is no longer positional: `submit` records whether the
   * entry was queued and every failure without that flag is a failure
   * before it.
   */
  /*
   * KEY: AND AN ENQUEUE THAT FAILED AFTER THE RENAME DID QUEUE.
   *
   * `Outbox.write` renames the new file into place and then syncs the
   * directory; a failure at that second step leaves the entry on disk
   * while the catch that reports "not queued" runs. Measured in a
   * nineteenth review round: the caller gave the sequence number back
   * and reloading the queue recovered the request the record then said
   * was not outstanding -- the mirror image of the defect the flag was
   * added for, and reachable through the shipped `syncDirectory`, whose
   * `closeSync` is not caught.
   */
  it('does not say it was not queued when the entry reached the file anyway', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }]);
    core = core0;
    const queuePath = path.join(core0.root, 'outbox.json');
    let syncs = 0;
    let failing = false;
    const failsAfterTheRename = {
      ...nodeFileOps,
      syncDirectory(directory: string): string | null {
        if (!failing) {
          return nodeFileOps.syncDirectory(directory);
        }
        syncs += 1;
        throw new Error('the directory could not be synced after the rename');
      }
    };
    const outbox = new Outbox(queuePath, failsAfterTheRename);
    outbox.load();
    outbox.setCursor('w:7');
    /*
     * THE SETUP WRITES TOO, so the fixture only starts failing once the
     * cursor is in place: otherwise this cell is about the cursor write
     * rather than the enqueue.
     */
    failing = true;
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, parts().storeHash));
    let outcome: SaveOutcome | undefined;
    let rejected: unknown = null;
    try {
      outcome = await saver.submit(recordFor(parts()));
    } catch (e) {
      rejected = e;
    }
    assert.ok(rejected !== null || outcome !== undefined);
    assert.ok(syncs > 0, 'the write never reached the sync, so this cell is about nothing');
    /*
     * THE ENTRY IS ON DISK. A fresh reader is used, because the saver's
     * own copy is not evidence about the file.
     */
    const onDisk = new Outbox(queuePath);
    onDisk.load();
    assert.strictEqual(
      onDisk.entries.length,
      1,
      'the fixture did not leave the entry on disk, so this cell cannot be about the case it names'
    );
    /*
     * AND THE OUTCOME DOES NOT LET THE NUMBER GO. A rejection is the
     * right answer here: the entry will be drained by whoever comes
     * next, the user is told the write failed, and `releaseSend` is
     * reached only for a resolved outcome carrying the flag -- so the
     * send stays outstanding, which is what the file says.
     */
    assert.notStrictEqual(
      outcome === undefined ? undefined : (outcome as { notQueued?: true }).notQueued,
      true,
      'the outcome said the entry was never queued while it is in the file, so the caller gives ' +
        'the sequence number back and the store keeps a request nothing is outstanding for'
    );
  });

  /*
   * KEY: AND A SAVE THAT WENT OUT IS NOT A SAVE THAT WAS NEVER QUEUED.
   *
   * Round nineteen's answer read the file in the catch. Round twenty
   * showed the file answering a different question by then: a save that
   * was queued, SENT and dequeued is not in the file either, so a
   * failure after a successful transmission read as "never queued" and
   * the caller released the number for a send that had gone out. "Was
   * this ever queued" is a fact about a moment, and the moment is the
   * enqueue.
   */
  it('does not say it was not queued after the save has been sent', async () => {
    const core0 = new FakeCore([
      { match: ['check'], stdout: CHECK, rc: 0 },
      { match: ['set'], stdout: `${wroteAnswer(8)}\n`, rc: 0 }
    ]);
    core = core0;
    const queuePath = path.join(core0.root, 'outbox.json');
    let sent = false;
    const failsOnceTheSendHasLanded = {
      ...nodeFileOps,
      syncDirectory(directory: string): string | null {
        if (!sent) {
          return nodeFileOps.syncDirectory(directory);
        }
        throw new Error('the directory could not be synced after the entry was dequeued');
      }
    };
    const outbox = new Outbox(queuePath, failsOnceTheSendHasLanded);
    outbox.load();
    outbox.setCursor('w:7');
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    /*
     * THE FIXTURE STARTS FAILING ONCE THE WIRE HAS SEEN THE SAVE, so the
     * failure this cell is about is the one AFTER the transmission.
     */
    const watching = {
      kind: 'watching',
      send: async (verb: string, args: string[]) => {
        const answer = await new CliTransport(core0.config(), core0.env()).send(verb, args);
        if (verb === 'set') {
          sent = true;
        }
        return answer;
      }
    };
    void client;
    const saver = new Saver(new Client(watching), outbox, settling(outbox, parts().storeHash));
    let outcome: SaveOutcome | undefined;
    try {
      outcome = await saver.submit(recordFor(parts()));
    } catch {
      outcome = undefined;
    }
    assert.ok(sent, 'nothing was ever sent, so this cell is not about the case it names');
    assert.notStrictEqual(
      outcome === undefined ? undefined : (outcome as { notQueued?: true }).notQueued,
      true,
      'a save that reached the store was reported as never queued, so the caller gives back a ' +
        'sequence number the store has already seen'
    );
  });

  /*
   * KEY: AND WHEN THE FILE CANNOT BE READ AT THAT MOMENT, THE ANSWER IS
   * THE SAFE ONE.
   *
   * The enqueue threw and the reload that would say whether it landed
   * threw as well. Nothing here can tell, and cannot-tell is not
   * permission to release the number: the entry may be on disk.
   */
  it('does not release the number when it could not tell whether the entry landed', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }]);
    core = core0;
    const queuePath = path.join(core0.root, 'outbox.json');
    let blind = false;
    let threw = false;
    const failsBothWays = {
      ...nodeFileOps,
      syncDirectory(directory: string): string | null {
        if (!blind) {
          return nodeFileOps.syncDirectory(directory);
        }
        /*
         * THE WRITE FAILS AFTER THE RENAME, and from that moment the
         * file cannot be read either -- which is the state where nothing
         * can say whether the entry landed.
         */
        threw = true;
        throw new Error('the directory could not be synced');
      },
      readText(file: string): string {
        if (threw && file === queuePath) {
          throw new Error('and the queue cannot be read either');
        }
        return nodeFileOps.readText(file);
      }
    };
    const outbox = new Outbox(queuePath, failsBothWays);
    outbox.load();
    outbox.setCursor('w:7');
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, parts().storeHash));
    blind = true;
    let outcome: SaveOutcome | undefined;
    try {
      outcome = await saver.submit(recordFor(parts()));
    } catch {
      outcome = undefined;
    }
    assert.notStrictEqual(
      outcome === undefined ? undefined : (outcome as { notQueued?: true }).notQueued,
      true,
      'the number was released although nothing could say whether the entry is on disk'
    );
  });

  it('says it was not queued when the queue itself could not be read', async () => {
    const core0 = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }]);
    core = core0;
    const queuePath = path.join(core0.root, 'outbox.json');
    const outbox = new Outbox(queuePath);
    outbox.load();
    outbox.setCursor('w:7');
    /*
     * READABLE WHEN THE SAVER IS BUILT AND NOT WHEN IT RELOADS, which is
     * the window `serialise` opens: the copy in memory is fine and the
     * file it is about to be refreshed from is not.
     */
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, parts().storeHash));
    fs.chmodSync(queuePath, 0o000);
    let outcome;
    try {
      outcome = await saver.submit(recordFor(parts()));
    } finally {
      fs.chmodSync(queuePath, 0o600);
    }
    assert.strictEqual(outcome.status, 'blocked', `the save was not blocked: ${outcome.message}`);
    assert.strictEqual(
      outcome.notQueued,
      true,
      'the queue reload failed before anything was queued and the outcome did not say so, so the ' +
        'number the save took stays outstanding for ever'
    );
  });

  it('says it was not queued when the store has no cursor to write against', async () => {
    const core0 = new FakeCore([
      {
        match: ['check'],
        stdout: '(check (store "s") (writers ()) (snapshots ()) (registry outside-store) (verdict ok))\n',
        rc: 0
      }
    ]);
    core = core0;
    const outbox = new Outbox(path.join(core0.root, 'outbox.json'));
    outbox.load();
    const client = new Client(new CliTransport(core0.config(), core0.env()));
    const saver = new Saver(client, outbox, settling(outbox, parts().storeHash));
    const outcome = await saver.submit(recordFor(parts()));
    assert.strictEqual(outcome.status, 'blocked', `the save was not blocked: ${outcome.message}`);
    assert.strictEqual(
      outcome.notQueued,
      true,
      'a save refused for want of a cursor kept the number it had taken, and the block is then a ' +
        'draft nothing can settle'
    );
    assert.strictEqual(outbox.entries.length, 0, 'the entry reached the queue after all');
    assert.strictEqual(sentCalls(core0).length, 0);
  });
});

/*
 * plugin-r3 C: what `submit` records, checked by something that fails.
 *
 * NEVER: AND THE FIRST VERSION OF THIS CENSUS MEASURED NOTHING AT ALL.
 *
 * It asked whether every `return` before the enqueue carried `notQueued`
 * and whether every `await` before it was inside a `try`. Its walk
 * stopped at function-like nodes, and `submit`'s whole body is an async
 * arrow handed to `serialise` -- so the only return it ever inspected
 * was `return this.serialise(...)`, whose SOURCE TEXT contains the
 * entire body including the comments that mention the flag. Measured in
 * an eighteenth review round: renaming every `notQueued` property inside
 * `submit` left it passing, and so did deleting the bootstrap's
 * try/catch outright.
 *
 * KEY: AND THE MUTATIONS I RAN AGAINST IT KILLED SOMETHING ELSE. Both
 * went red, and I recorded them as evidence for these two cells --
 * whereas the rows that failed were the BEHAVIOURAL cells above, which
 * were doing the work. A kill is evidence for a cell only if that cell
 * is in the red rows, and I did not read which rows they were.
 *
 * What is asked now is about the flag `submit` keeps, which is what its
 * correctness actually rests on: it is set in one place, that place is
 * immediately after the enqueue, and the catch consults it. The
 * behavioural cells above cover the four failures anyone has thought of;
 * this covers the fifth.
 */
describe('plugin-r3 submit records whether the entry was queued', () => {
  const root = path.join(__dirname, '..', '..', '..');

  function submitOf(): { node: ts.MethodDeclaration; source: ts.SourceFile } {
    const file = path.join(root, 'src', 'saver.ts');
    const parsed = ts.createSourceFile(
      'saver.ts',
      fs.readFileSync(file, 'utf8'),
      ts.ScriptTarget.Latest,
      true
    );
    let found: ts.MethodDeclaration | undefined;
    const walk = (node: ts.Node): void => {
      if (ts.isMethodDeclaration(node) && node.name.getText(parsed) === 'submit') {
        found = node;
      }
      node.forEachChild(walk);
    };
    walk(parsed);
    assert.ok(found !== undefined, 'submit is gone, so this census reads nothing');
    return { node: found, source: parsed };
  }

  /*
   * EVERY NODE INSIDE, INCLUDING THE ONES INSIDE THE CALLBACK. The walk
   * that stopped at function-like nodes is the defect this census had.
   */
  function every(node: ts.Node, seen: (inner: ts.Node) => void): void {
    const walk = (inner: ts.Node): void => {
      seen(inner);
      inner.forEachChild(walk);
    };
    node.forEachChild(walk);
  }

  /*
   * NEVER: THE CENSUS HAS BEEN REWRITTEN THREE TIMES, AND EACH VERSION
   * ASKED ABOUT THE SHAPE OF THE CODE RATHER THAN ABOUT WHAT DECIDES.
   *
   * r19 counted `return`s before the enqueue and read source text for
   * `.enqueue(`; r20 counted assignments and required adjacency; r21
   * required both facts to appear in the catch. Measured past them, in
   * order: comments containing the word, a flag initialised `true`, an
   * enqueue wrapped in `if (false)`, `if (queued && false)`, and
   * `false && (attempted = true)` -- an expression statement in the
   * right position that does nothing.
   *
   * KEY: AND A SOURCE CENSUS CANNOT ESTABLISH WHAT A VALUE MEANS. This
   * is written down because the previous version of this comment claimed
   * otherwise.
   *
   * A twenty-first review round got four more changes past it: deleting
   * the catch's `load()`, `entries.every(...)` in place of
   * `entries.some(...)`, wrapping the fallback in `if (false)`, and an
   * unconditional `return` placed above the decision so that the
   * decision is unreachable. Each of those leaves the shape intact. No
   * reading of the source can tell them apart from the real thing,
   * because what separates them is what happens when the program runs.
   *
   * So the claim here is narrowed to what this can actually witness:
   * the decision is held in ONE variable, it starts false, it is written
   * in the three places the design has and no more, one of those writes
   * reads the file, none of them is behind an operator that can skip it,
   * and the catch re-raises under the identifier alone. **Every one of
   * the mutations above is killed by a BEHAVIOURAL cell in this file**,
   * and which row dies to which mutation is recorded in the delivery
   * note. This row is the tripwire for a shape nobody has written a
   * behavioural cell for yet; it is not evidence that the program is
   * right.
   */
  /*
   * REWRITTEN FOR THE RECEIPT (queue item 3, approved 2026-09-22). Everything
   * above describes the flag this census used to hold: one variable, three
   * writes, one of them a read of the file. The design that replaced it is
   * that the enqueue answers the question itself -- `Outbox.enqueue` returns a
   * receipt nothing else can make -- so what is read now is that `submit`
   * decides from THAT: one `Receipt | null`, starting null, written once, from
   * the enqueue's return, and the catch re-raises under it alone. The same
   * limit applies as before: this is a tripwire for the shape, and the
   * behavioural cells in test/unit/receipts.test.ts are what say the program
   * is right.
   */
  it('decides from the receipt the enqueue returned, in one place and no other', () => {
    const { node, source } = submitOf();
    const declared: ts.VariableDeclaration[] = [];
    const writes: ts.BinaryExpression[] = [];
    let rereads = 0;
    every(node, (inner) => {
      if (ts.isVariableDeclaration(inner) && inner.name.getText(source) === 'receipt') {
        declared.push(inner);
      }
      if (
        ts.isBinaryExpression(inner) &&
        inner.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
        inner.left.getText(source) === 'receipt'
      ) {
        writes.push(inner);
      }
      if (ts.isCallExpression(inner) && /\.outbox\.load$/.test(inner.expression.getText(source))) {
        rereads += 1;
      }
    });
    assert.strictEqual(declared.length, 1, 'the decision is not held in one variable');
    assert.strictEqual(declared[0].type?.getText(source), 'Receipt | null', 'the decision is not a receipt');
    assert.strictEqual(
      declared[0].initializer?.getText(source),
      'null',
      'it starts as something other than null, so a failure before anything happened reads as a ' +
        'save that was queued and the number is never given back'
    );
    assert.strictEqual(writes.length, 1, `it is written in ${writes.length} places; the design has one`);
    const right = writes[0].right;
    assert.ok(
      ts.isCallExpression(right) && /\.enqueue$/.test(right.expression.getText(source)),
      `the one write is not the enqueue's return: ${right.getText(source)}`
    );
    assert.strictEqual(
      rereads,
      0,
      'submit reads the queue file again, which is the answer this design replaced: the file at a ' +
        'moment when a drain, or another window, may already have changed it'
    );
    /*
     * NEVER: AND THE WRITE IS NOT BEHIND AN EXPRESSION THAT CAN SKIP IT --
     * the same trap as before (`false && (receipt = ...)`).
     */
    let at: ts.Node = writes[0];
    while (at.parent !== undefined && !ts.isBlock(at.parent)) {
      if (
        (ts.isBinaryExpression(at.parent) &&
          (at.parent.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
            at.parent.operatorToken.kind === ts.SyntaxKind.BarBarToken ||
            at.parent.operatorToken.kind === ts.SyntaxKind.QuestionQuestionToken)) ||
        ts.isConditionalExpression(at.parent)
      ) {
        assert.fail(`the receipt's write is behind \`${at.parent.getText(source).slice(0, 60)}\``);
      }
      at = at.parent;
    }
  });

  it('consults the receipt, and nothing else, when a failure reaches the caller', () => {
    const { node, source } = submitOf();
    let decides = false;
    every(node, (inner) => {
      if (!ts.isCallExpression(inner) || !/\.catch$/.test(inner.expression.getText(source))) {
        return;
      }
      /*
       * THE CATCH RE-RAISES UNDER `if (receipt !== null)` AND NOTHING ELSE:
       * the condition IS that comparison, and its consequence throws.
       */
      const walk = (at: ts.Node): void => {
        if (
          ts.isIfStatement(at) &&
          ts.isBinaryExpression(at.expression) &&
          at.expression.getText(source) === 'receipt !== null'
        ) {
          const body = at.thenStatement;
          const throws =
            ts.isThrowStatement(body) ||
            (ts.isBlock(body) && body.statements.some((one) => ts.isThrowStatement(one)));
          if (throws) {
            decides = true;
          }
        }
        at.forEachChild(walk);
      };
      inner.arguments.forEach((argument) => argument.forEachChild(walk));
    });
    assert.ok(
      decides,
      "submit's catch does not re-raise under the receipt alone. Anything else there -- a second " +
        'reading of the file, a condition with another term in it -- decides the release of a ' +
        'sequence number by something other than whether the enqueue issued a receipt.'
    );
  });
});

/*
 * plugin-r3 item 3: THE ENQUEUE RECEIPT DECIDES WHETHER THE NUMBER GOES BACK.
 *
 * `submit` took a sequence number for the save before anything was queued;
 * it must give it back (`notQueued`) exactly when the entry never reached
 * the queue file, and never when it did. Since the approved design
 * (2026-09-22) that is decided by the receipt `Outbox.enqueue` returns --
 * made inside the lock, after the commit, by nothing else. These are the
 * design's runtime cells (3, 4, 5 both ways, 6); cells 1 and 2 and the two
 * call-site censuses are source censuses in test/unit/receipts.test.ts,
 * because where a statement sits is not something a running cell can see.
 *
 * "The number goes back" is read as the outcome carrying `notQueued`,
 * which is what the save handler releases it on; "kept" is a rejection or
 * an outcome without it. "The entry is there" is asked of a FRESH reader
 * of the queue file, not of the Outbox the Saver holds.
 */
describe('plugin-r3 3 the enqueue receipt decides whether the number goes back', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * A FILE SYSTEM THAT FAILS ONE OPERATION ONCE, AT THE ENQUEUE, and behaves
   * otherwise. "The first time it is asked" was the first version, and it
   * measured the wrong thing: `submit` writes the cursor it bootstraps
   * BEFORE it enqueues, so the first directory flush belonged to that write
   * and the save was answered as never queued for a reason that had nothing
   * to do with the enqueue. So the failure is armed by what is being
   * written: the queue file with this request in it, or, for the flush,
   * the directory once the queue file holds the request.
   */
  function failingAtTheEnqueue(operation: 'writeDurably' | 'rename' | 'syncDirectory', req: string, queuePath: () => string): FileOps {
    let failed = false;
    const files: FileOps = { ...nodeFileOps };
    const original = nodeFileOps[operation] as (...args: unknown[]) => void;
    const carries = (args: unknown[]): boolean => {
      if (operation === 'syncDirectory') {
        return fs.existsSync(queuePath()) && fs.readFileSync(queuePath(), 'utf8').includes(req);
      }
      if (operation === 'writeDurably') {
        return typeof args[1] === 'string' && args[1].includes(req);
      }
      const from = String(args[0]);
      return fs.existsSync(from) && fs.readFileSync(from, 'utf8').includes(req);
    };
    (files as unknown as Record<string, (...args: unknown[]) => void>)[operation] = (...args: unknown[]) => {
      if (!failed && carries(args)) {
        failed = true;
        throw new Error(`${operation} failed on purpose`);
      }
      return original.apply(nodeFileOps, args);
    };
    return files;
  }

  function over(
    files: FileOps | ((queuePath: () => string) => FileOps),
    calls: ScriptedCall[],
    settle?: (outbox: Outbox) => Settle
  ): Rig {
    const made = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, ...calls]);
    const queuePath = made.outboxFile();
    const outbox = new Outbox(queuePath, typeof files === 'function' ? files(() => queuePath) : files);
    outbox.load();
    const client = new Client(new CliTransport(made.config(), made.env()));
    const settler = settle === undefined ? settling(outbox, parts().storeHash) : settle(outbox);
    return { core: made, outbox, queuePath, saver: new Saver(client, outbox, settler) };
  }

  function freshlyRead(queuePath: string): string[] {
    const reader = new Outbox(queuePath);
    reader.load();
    return reader.entries.map((e) => e.req);
  }

  const NOTHING_SAID: ScriptedCall = { match: ['set'], stdout: '', rc: 1 };

  it('3: gives the number back when the enqueue failed before anything landed', async () => {
    const r = over((at) => failingAtTheEnqueue('writeDurably', parts().req, at), [NOTHING_SAID]);
    core = r.core;
    const outcome = await r.saver.submit(recordFor(parts()));
    assert.strictEqual(outcome.notQueued, true, `the number was kept for an entry that never landed: ${outcome.message}`);
    assert.match(outcome.message, /could not be written to the queue/, `the failure was not the enqueue's: ${outcome.message}`);
    assert.deepStrictEqual(freshlyRead(r.queuePath), [], 'the queue file holds the entry after all');
    assert.strictEqual(sentCalls(core).length, 0);
  });

  /*
   * KEY: THE CASE THAT DECIDES THE DESIGN, BOTH WAYS (ruled 2026-09-22):
   * the rename is the existence fact, the directory flush is durability.
   * Each cell fails on the other's fixture -- a rename that did not land
   * leaves no entry and must give the number back; one that landed leaves
   * the entry, keeps the number and says what it could not promise.
   */
  it('5a: keeps the number when the rename landed and the flush failed, and says so', async () => {
    const r = over((at) => failingAtTheEnqueue('syncDirectory', parts().req, at), [NOTHING_SAID]);
    core = r.core;
    const record = recordFor(parts());
    const outcome = await r.saver.submit(record);
    assert.notStrictEqual(outcome.notQueued, true, 'the number was given back for an entry that is on disk');
    assert.deepStrictEqual(freshlyRead(r.queuePath), [record.req], 'the entry is not in the queue file');
    assert.ok(
      outcome.durability !== undefined && /could not be flushed/.test(outcome.durability),
      `the durability warning was not carried on the outcome: ${JSON.stringify(outcome.durability)}`
    );
  });

  /*
   * AND THROUGH THE SHIPPING FILE OPERATIONS, NOT A STAND-IN. (review r1 of
   * item 3) 5a injects above `syncDirectory`, and the shipping one used to
   * swallow a failed flush -- so 5a was green while the warning could never
   * fire in production. Here the Outbox is given `nodeFileOps` itself, and
   * the failure is injected BELOW it: Node's own `fs.fsyncSync` is replaced
   * for the length of this cell by one that throws EIO when it is asked to
   * flush a directory while the queue file already holds this request --
   * that is, the flush that follows the enqueue's rename -- and is restored
   * in `finally`. Replacing the module's property works because the
   * compiled product reads `fs.fsyncSync` through the module object at each
   * call.
   */
  it('5c: carries the warning through the shipping file operations when the directory flush fails', async () => {
    const nodeFs = require('fs') as typeof import('fs');
    const original = nodeFs.fsyncSync;
    let injected = 0;
    const r = over({ ...nodeFileOps }, [NOTHING_SAID]);
    core = r.core;
    const record = recordFor(parts());
    nodeFs.fsyncSync = ((handle: number): void => {
      if (
        injected === 0 &&
        nodeFs.fstatSync(handle).isDirectory() &&
        nodeFs.existsSync(r.queuePath) &&
        nodeFs.readFileSync(r.queuePath, 'utf8').includes(record.req)
      ) {
        injected += 1;
        const failure = new Error('EIO: i/o error, fsync') as NodeJS.ErrnoException;
        failure.code = 'EIO';
        throw failure;
      }
      original(handle);
    }) as typeof nodeFs.fsyncSync;
    let outcome: SaveOutcome;
    try {
      outcome = await r.saver.submit(record);
    } finally {
      nodeFs.fsyncSync = original;
    }
    assert.strictEqual(injected, 1, 'the directory flush after the enqueue was never reached, so this cell measured nothing');
    assert.notStrictEqual(outcome.notQueued, true, 'the number was given back for an entry that is on disk');
    assert.deepStrictEqual(freshlyRead(r.queuePath), [record.req], 'the entry is not in the queue file');
    assert.ok(
      outcome.durability !== undefined && /could not be flushed/.test(outcome.durability) && /EIO/.test(outcome.durability),
      `the shipping file operations did not carry the failed flush: ${JSON.stringify(outcome.durability)}`
    );
  });

  it('5b: gives the number back when the rename did not land', async () => {
    const r = over((at) => failingAtTheEnqueue('rename', parts().req, at), [NOTHING_SAID]);
    core = r.core;
    const outcome = await r.saver.submit(recordFor(parts()));
    assert.strictEqual(outcome.notQueued, true, `the number was kept for an entry that never landed: ${outcome.message}`);
    assert.match(outcome.message, /could not be written to the queue/, `the failure was not the enqueue's: ${outcome.message}`);
    assert.deepStrictEqual(freshlyRead(r.queuePath), [], 'the queue file holds the entry after all');
    assert.strictEqual(outcome.durability, undefined, 'a durability warning for an entry that was never written');
  });

  it('4: keeps the number when something fails after the enqueue, and the entry is there', async () => {
    const r = over({ ...nodeFileOps }, [{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], () => () => {
      throw new Error('the settlement fell over');
    });
    core = r.core;
    const record = recordFor(parts());
    await assert.rejects(r.saver.submit(record), /fell over/, 'a failure after the enqueue was answered as not queued');
    assert.deepStrictEqual(freshlyRead(r.queuePath), [record.req], 'the entry is not in the queue file');
  });

  /*
   * KEY: THE CASE THAT DEFEATED THE PREVIOUS ANSWER. The entry is queued,
   * sent, settled and removed -- and THEN something fails. The file no
   * longer holds it, so any answer that reads the file says "never
   * queued" and gives the number back for a send that went out.
   */
  it('6: keeps the number when the entry was settled and removed before the failure', async () => {
    const r = over({ ...nodeFileOps }, [{ match: ['set'], stdout: wroteAnswer(8), rc: 0 }], (outbox) => (req, settlement) => {
      outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
      throw new Error('fell over after settling');
    });
    core = r.core;
    const record = recordFor(parts());
    await assert.rejects(r.saver.submit(record), /fell over after settling/, 'the failure was answered as not queued');
    assert.deepStrictEqual(freshlyRead(r.queuePath), [], 'the fixture did not remove the entry, so this is cell 4');
    assert.strictEqual(sentCalls(core).length, 1);
  });
});
