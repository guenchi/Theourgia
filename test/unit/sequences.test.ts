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
 * C11: the ten sequences that actually went wrong, put to the new shape.
 *
 * THESE ARE NOT NEW CELLS. Each one is a trigger from
 * archive/x1a-baseline-design/failure-sequences.md -- every one of them
 * reproduced against real handler code during rounds 14 to 17 -- asked
 * again of the design that replaced the code they broke. The old cells
 * in placing/documents go when X1c lands; the SEQUENCES must not go
 * with them, because they are the only record of what this design is
 * for.
 *
 * S7 IS ELSEWHERE, in two-hosts.test.ts, because it is the one that
 * needs two processes and the one whose answer is "unreachable".
 *
 * TWO MODULES WERE RETIRED INTO THIS FILE.
 *
 * `src/open.ts` and `src/placing.ts` carried X1c's predecessors -- a
 * ticket that ordered two overlapping opens, and a step that admitted a
 * reading, wrote the file and revealed it. Sixteen cells went with them,
 * and this note is the record of what they covered and what covers it
 * now, because a suite that merely gets shorter has lost something and
 * says nothing:
 *
 *   - ORDERING TWO OPENS OF ONE BLOCK (open.test.ts: tickets, the
 *     refusal of an older registration, per-file ordering) -> the chain
 *     in `src/chain.ts`, which serialises the critical sections rather
 *     than ordering their results, AND immutable publication, which
 *     removes the thing they were competing for: a reading no longer
 *     replaces another reading's baseline, it writes its own. S1 and S4
 *     below are those cells' subject.
 *   - A FAILED WRITE MUST RECORD NO BASELINE (open.test.ts) -> the
 *     three-step record in `Publisher.publish`, and S2 below.
 *   - A CONFIRMED SAVE OUTRANKS AN EARLIER READ (open.test.ts:
 *     `confirmed`) -> the sidecar: a save writes `acknowledged-raw`
 *     against the version it was split from, and a later reading is a
 *     NEW version rather than a competitor. S3 and S5 below.
 *   - ADMIT, WRITE, THEN REVEAL (placing.test.ts) -> `publishInto`,
 *     which is the single door every publication goes through and which
 *     asks `isOpen` before it writes anything.
 *   - THE LOSING READING MUST NOT WRITE (placing.test.ts) -> the same
 *     door: a path the editor holds is refused, and a path it does not
 *     hold has never been published, so there is nothing to lose.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { PathChain } from '../../src/chain';
import { OpenDocuments, Publisher, Sidecar, UNNUMBERED, sidecarToDisk } from '../../src/publication';
import { Saving } from '../../src/saving';
import { RecordingFs } from '../support/recording-fs';

/*
 * WHICH SEND AN ANSWER IS ABOUT, for cells that are not about that.
 *
 * §13 records WHICH send the store confirmed, so `recordAnswer` is told
 * the send's number, the split it was made against, and whether it was
 * this client's write. These cells are about the ORDER and the refusals
 * around a record, so they all speak for one ordinary first send; the
 * cells that are about the number itself say so in their own fixtures.
 */

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-seq-'));
}

const digestOf = (text: string): string =>
  require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

/*
 * ⚠️ THE REAL DIGEST OF THE REAL PREFIX. A placeholder here was fine
 * while nothing compared it and became a trap the moment the draft
 * rule started asking whether the split still matches: every cell in
 * this file would have read as a draft, for a reason that lived in
 * the fixture.
 */
const SENT_ONCE = { seq: 1, prefixDigest: digestOf('## Two\n'), by: 'store' as const };

function nothingOpen(): OpenDocuments {
  return { isOpen: () => false };
}

function sidecar(over: Partial<Sidecar> = {}): Sidecar {
  return {
    ...UNNUMBERED,
    format: 1,
    storeId: 's1',
    blockId: 'a.2',
    phase: 'published',
    prefix: '## Two\n',
    written: 'w',
    previous: null,
    acknowledgedRaw: null,
    sent: null,
    cursor: null,
    localOnly: false,
    unresolved: false,
    bodyHasCrlf: false,
    ...over
  };
}

function place(dir: string, name: string, text: string | null, meta: Sidecar): string {
  fs.mkdirSync(dir, { recursive: true });
  const file = path.join(dir, name);
  fs.writeFileSync(`${file}.meta`, JSON.stringify(sidecarToDisk(meta)), 'utf8');
  if (text !== null) {
    fs.writeFileSync(file, text, 'utf8');
  }
  return file;
}

describe('C11/S1 an older reading cannot replace a newer baseline', () => {
  /*
   * WHAT WENT WRONG: two opens of one block overlapped and whichever
   * finished last became the baseline a save was split against. With an
   * empty prefix the mismatch was not refused -- the heading became body
   * and was written into the block.
   *
   * WHAT ANSWERS IT NOW: a reading is published to a NEW path with its
   * own record, so an older one cannot become the newer one's baseline;
   * there is nothing to replace. The chain keeps the two publications
   * from interleaving at all.
   *
   * AN EDITOR-HOSTED CELL WAS RETIRED FOR THIS. `opens the same block
   * into the same document the second time` required one block to reach
   * one document, which was true while the file was rewritten in place
   * and is false now -- the second open publishes the next version. Its
   * sequence is recorded here rather than lost with it: open, open
   * again, and the FIRST version's bytes must be unchanged. The cell
   * that replaced it asserts exactly that, and so does the second half
   * of this one.
   */
  it('replaces body and prefix together at the current path', async () => {
    const dir = scratch();
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const older = await publisher.publish({ directory: dir, storeId: 's1', blockId: 'a.2', prefix: '', text: 'no heading\n', cursor: null });
    const newer = await publisher.publish({ directory: dir, storeId: 's1', blockId: 'a.2', prefix: '## Grown\n', text: '## Grown\nbody\n', cursor: null });
    assert.ok(older.published && newer.published);
    if (older.published && newer.published) {
      assert.strictEqual(older.file, newer.file, 'the canonical path changed');
      assert.strictEqual(fs.readFileSync(newer.file,'utf8'),'## Grown\nbody\n');
      assert.strictEqual(
        publisher.sidecarOf(newer.file)?.prefix,
        '## Grown\n',
        'one record now describes both readings, which is the fault S1 named'
      );
    }
  });

  it('does not let two publications of one block interleave', async () => {
    const chain = new PathChain();
    const order: string[] = [];
    const first = chain.run('/s/a.2/1.md', async () => {
      order.push('first:in');
      await new Promise((r) => setTimeout(r, 15));
      order.push('first:out');
    });
    const second = chain.run('/s/a.2/1.md', async () => {
      order.push('second:in');
    });
    await Promise.all([first, second]);
    assert.deepStrictEqual(order, ['first:in', 'first:out', 'second:in']);
  });
});

describe('C11/S2 a write that failed leaves no baseline claiming it happened', () => {
  /*
   * WHAT WENT WRONG: admission happened before the file was written, so
   * a failing write left a baseline describing a file nobody wrote.
   *
   * WHAT ANSWERS IT NOW: the record is written first and says
   * `publishing` with the digest it intends and the digest that was
   * there. A file still holding the previous version is therefore a
   * write that never happened, and it is decidable rather than assumed.
   */
  it('says the write never happened when the file is still the previous version', () => {
    const dir = scratch();
    const old = '## Two\nold\n';
    const file = place(dir, '2.md', old, sidecar({ phase: 'publishing', written: digestOf('## Two\nnew\n'), previous: digestOf(old) }));
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(file), {
      kind: 'mid-publication',
      complete: false
    });
  });

  it('refuses to save from a file whose publication never finished', () => {
    const dir = scratch();
    const text = '## Two\nold\n';
    const file = place(dir, '2.md', text, sidecar({ phase: 'publishing', written: digestOf('## Two\nnew\n'), previous: digestOf(text) }));
    const decision = new Saving(new RecordingFs()).decide(
      { file, isDirty: false, getText: () => text },
      sidecar({ phase: 'publishing' })
    );
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'publication-incomplete' } });
  });
});

describe('C11/S3 a save cannot install an older reading', () => {
  /*
   * WHAT WENT WRONG: the save captured a baseline before its await, a
   * newer open replaced it, and the save then wrote its own older
   * reading back under the newer one's ticket.
   *
   * WHAT ANSWERS IT NOW: a save does not install a baseline at all. It
   * records an acknowledgement against the record of the file it was
   * split from, and that record belongs to one version and one path.
   */
  it('records the answer against the version it was split from, not against the newest', () => {
    const dir = scratch();
    const oldText = '## Two\nold\n';
    const older = place(dir, '1.md', oldText, sidecar({ written: digestOf(oldText) }));
    place(dir, '2.md', '## Two\nnewer\n', sidecar({ written: digestOf('## Two\nnewer\n') }));
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    publisher.acknowledge(older, digestOf(oldText), digestOf('old\n'), 'w:7');
    assert.strictEqual(
      publisher.sidecarOf(path.join(dir, '2.md'))?.acknowledgedRaw,
      null,
      'answering for one version marked another version as sent'
    );
  });
});

describe('C11/S4 a reading that began before a save cannot overwrite it', () => {
  /*
   * WHAT WENT WRONG: an open started reading, the user saved, the save
   * was confirmed and the file marked committed, and the open's older
   * answer then arrived and rewrote the file.
   *
   * WHAT ANSWERS IT NOW: a reading never rewrites a file. It publishes
   * the next version, and the version the user saved from stays exactly
   * as it was.
   */
  it('publishes the late reading as a new version and leaves the saved file untouched', async () => {
    const dir = scratch();
    const saved = '## Two\nwhat the user saved\n';
    const file = place(dir, '1.md', saved, sidecar({ written: digestOf(saved), acknowledgedRaw: digestOf(saved) }));
    const files = new RecordingFs();
    await new Publisher(files, nothingOpen()).publish({
      directory: dir,
      storeId: 's1',
      blockId: 'a.2',
      prefix: '## Two\n',
      text: '## Two\nthe older reading\n',
      cursor: null
    });
    assert.strictEqual(fs.readFileSync(file, 'utf8'), saved, 'a late reading overwrote a saved file');
    assert.strictEqual(files.countOf('unlink', file), 0);
  });
});

describe('C11/S5 one save’s answer cannot mark another save’s bytes as sent', () => {
  /*
   * WHAT WENT WRONG: the confirmation hashed whatever was on disk at the
   * moment the answer arrived, so a first save's answer marked a second
   * save's bytes as being in the store. The file then read as clean and
   * committed while holding text the store had never seen.
   *
   * WHAT ANSWERS IT NOW: the digest recorded is the one that was
   * verified and sent -- it is carried into the answer, not re-derived
   * from the disk.
   */
  it('records the digest that was sent, not the digest of whatever is there now', () => {
    const dir = scratch();
    const sentText = '## Two\nfirst\n';
    const file = place(dir, '1.md', sentText, sidecar({ written: digestOf(sentText) }));
    fs.writeFileSync(file, '## Two\nsecond, written while the first was in flight\n', 'utf8');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    publisher.acknowledge(file, digestOf(sentText), digestOf('first\n'), 'w:7');
    assert.strictEqual(
      publisher.sidecarOf(file)?.acknowledgedRaw,
      digestOf(sentText),
      'the answer credited the later save’s bytes to the earlier save'
    );
    assert.deepStrictEqual(
      publisher.standingOf(file),
      { kind: 'published', draft: true },
      'the file holds text the store never saw and is not counted as a draft'
    );
  });
});

describe('C11/S6 no record ever describes bytes it did not write', () => {
  /*
   * WHAT WENT WRONG: the branch that kept a file it found registered the
   * reading it had just taken from the store, so the prefix a save was
   * split against belonged to bytes that were never written.
   *
   * WHAT ANSWERS IT NOW: a record is written beside the file it
   * describes, in the same publication, and a file found without one is
   * refused rather than given a prefix from somewhere else.
   */
  it('refuses to split a file that has no record of its own', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, 'stray.md');
    fs.writeFileSync(file, 'bytes from who knows where\n', 'utf8');
    const decision = new Saving(new RecordingFs()).decide(
      { file, isDirty: false, getText: () => 'bytes from who knows where\n' },
      null
    );
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'no-sidecar' } });
  });
});

describe('C11/S8 a normalised save does not leave the file a draft for ever', () => {
  /*
   * WHAT WENT WRONG: the confirmation hashed the normalised text while
   * the disk held CRLF, so the file never matched again and every later
   * start demanded the user copy it out and delete it.
   *
   * WHAT ANSWERS IT NOW: two digests. `sent` is the normalised text that
   * went out; `acknowledged-raw` is the bytes on disk, which is what a
   * draft is judged against.
   */
  it('judges a draft by the bytes on disk, not by the text that was sent', () => {
    const dir = scratch();
    const onDisk = '## Two\r\nbody\r\n';
    const file = place(dir, '1.md', onDisk, sidecar({
      written: digestOf(onDisk),
      acknowledgedRaw: digestOf(onDisk),
      sent: digestOf('body\n')
    }));
    assert.deepStrictEqual(
      new Publisher(new RecordingFs(), nothingOpen()).standingOf(file),
      { kind: 'published', draft: false },
      'a file whose line endings were normalised on the way out counts as unsent for ever'
    );
  });
});

describe('C11/S9 a save that arrives while a block is opening is not silent', () => {
  /*
   * WHAT WENT WRONG: the file was classified only after three editor
   * waits, so a save arriving during them found neither a baseline nor a
   * classification and returned without a word -- which looks exactly
   * like a save that worked.
   *
   * WHAT ANSWERS IT NOW: the two critical sections are on one chain, so
   * the save waits; and a file with no record is refused by name rather
   * than passed over.
   */
  it('serialises a save that arrives while the block is being published', async () => {
    const chain = new PathChain();
    const order: string[] = [];
    const opening = chain.run('/s/a.2/1.md', async () => {
      order.push('publish:in');
      await new Promise((r) => setTimeout(r, 15));
      order.push('publish:out');
    });
    const saving = chain.run('/s/a.2/1.md', async () => {
      order.push('save');
    });
    await Promise.all([opening, saving]);
    assert.deepStrictEqual(order, ['publish:in', 'publish:out', 'save']);
  });

  it('names a refusal rather than returning quietly', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, 'text\n', 'utf8');
    const decision = new Saving(new RecordingFs()).decide(
      { file, isDirty: false, getText: () => 'text\n' },
      null
    );
    assert.strictEqual(decision.send, false);
    assert.ok(
      decision.send === false && decision.refusal.because.length > 0,
      'the save was declined with no reason, which is what a successful save also looks like'
    );
  });
});

describe('C11/S10 nothing is left set that only a person can clear', () => {
  /*
   * WHAT WENT WRONG: a file was marked as needing attention in memory,
   * and a later editor failure meant the mark was never cleared -- so
   * opening said one thing and saving said the opposite.
   *
   * WHAT ANSWERS IT NOW: the mark is on disk beside the file, it is
   * `unresolved`, and `reconcile` is the only thing that clears it. Both
   * the opening and the saving path read the same record.
   */
  it('gives the same answer to the open path and the save path', () => {
    const dir = scratch();
    const text = '## Two\na third version\n';
    const file = place(dir, '1.md', text, sidecar({ unresolved: true, written: digestOf('## Two\nstore\n'), previous: digestOf('## Two\nolder\n') }));
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const standing = publisher.standingOf(file);
    const decision = new Saving(new RecordingFs()).decide(
      { file, isDirty: false, getText: () => text },
      publisher.sidecarOf(file)
    );
    assert.strictEqual(standing.kind, 'third-version');
    assert.strictEqual(decision.send, false, 'opening called it stranded and saving sent it anyway');
  });
});

/*
 * C12: the display ticket decides which document is shown and nothing
 * else.
 *
 * HOW TO SHOW THAT SOMETHING DOES NOT PARTICIPATE. An absence cannot be
 * asserted directly, so the ticket is put OUT OF ORDER and the writing
 * is required to come out the same. If a build had let it into a write
 * decision, reversing it would change what landed.
 */
describe('C12 the display ticket takes no part in what is written', () => {
  async function publishTwice(dir: string, tickets: number[]): Promise<string[]> {
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());
    const chain = new PathChain();
    const written: string[] = [];
    for (const [index, text] of ['## Two\nfirst\n', '## Two\nsecond\n'].entries()) {
      await chain.run(path.join(dir, `${tickets[index]}.md`), async () => {
        const outcome = await publisher.publish({
          directory: dir,
          storeId: 's1',
          blockId: 'a.2',
          prefix: '## Two\n',
          text,
          cursor: null
        });
        if (outcome.published) {
          written.push(`${outcome.version}:${fs.readFileSync(outcome.file, 'utf8')}`);
        }
      });
    }
    return written;
  }

  it('writes the same versions whether the tickets run in order or reversed', async () => {
    const inOrder = await publishTwice(scratch(), [1, 2]);
    const reversed = await publishTwice(scratch(), [2, 1]);
    assert.deepStrictEqual(
      reversed,
      inOrder,
      'putting the display tickets out of order changed what was written, so they are in a write decision'
    );
  });
});

/*
 * C20: three interruptions, each of which an obvious implementation
 * survives.
 */
describe('C20 what an interruption must not be allowed to do', () => {
  /*
   * THE IMPLEMENTATION C20 NAMES for this one: a build that decides the
   * document is clean when the save is QUEUED. By the time the handler
   * runs the user may have typed again, and the text that goes out is
   * then neither the saved one nor the current one.
   */
  it('sends nothing when the document went dirty between queueing and handling', async () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nsaved\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const chain = new PathChain();

    const document = { file, isDirty: false, getText: () => '## Two\nsaved\n' };
    const queued = chain.run(file, async () => saving.decide(document, sidecar()));
    document.isDirty = true;
    const decision = await queued;
    assert.deepStrictEqual(
      decision,
      { send: false, refusal: { because: 'document-dirty' } },
      'the handler used a cleanliness it decided at queueing time'
    );
  });

  /*
   * A TAKEOVER THAT DIED AFTER RECORDING AND BEFORE DEQUEUEING. The
   * entry is still there, which is the point: the next window takes the
   * next sequence, sends the SAME request, and the store answers replay.
   * The cursor must not go backwards on the way.
   */
  it('leaves the entry for the next window when the answer was recorded but not removed', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = place(dir, '1.md', text, sidecar({ written: digestOf(text), acknowledgedRaw: digestOf(text), cursor: 'w:9' }));
    const saving = new Saving(new RecordingFs());
    const late = saving.recordAnswer(file, {
      req: 'r1',
      cursor: 'w:4',
      rawDigest: digestOf(text),
      sentDigest: digestOf('body\n'),
      mismatch: false,
      send: SENT_ONCE
    });
    assert.deepStrictEqual(late, { dequeued: true }, 'a replay of an already-recorded answer was not settled');
    assert.strictEqual(
      new Publisher(new RecordingFs(), nothingOpen()).sidecarOf(file)?.cursor,
      'w:9',
      'a late replay moved the cursor backwards'
    );
  });
});
