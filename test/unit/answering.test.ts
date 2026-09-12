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
 * C7, C14 and C20: what survives an answer that never arrives, and what
 * the order of two writes has to be.
 *
 * THE RECORD IS WRITTEN BEFORE THE ENTRY IS REMOVED. A build that
 * removed the entry first and then died has destroyed the only means of
 * retrying: the request is gone and the store may or may not hold it.
 * Both orders leave the same state when nothing goes wrong, which is
 * why these cells interrupt. (§12.7.4, C7)
 *
 * C14 NAMES THE IMPLEMENTATION IT KILLS: one that persists the entry
 * only after the answer comes back. It passes C7 -- entries are removed
 * in the right order -- and loses every save whose answer was in flight
 * when the window died, including the ones the store DID apply.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
  digestOfBytes,
  Publisher,
  Sidecar,
  sidecarFromDisk,
  sidecarToDisk
} from '../../src/publication';
import { Saving } from '../../src/saving';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-answer-'));
}

/*
 * THE DIGEST OF THE BODY THAT WENT OUT.
 *
 * ⚠️ THESE FIXTURES USED THE STRING 'sent'. That is not a digest of
 * anything, and it describes a world that cannot happen: a save whose
 * sent body has no relation to the file it came from. The cells passed
 * because nothing compared it with anything -- and the check that the
 * record still splits the file into the body that was sent, which a
 * review found missing, is exactly the comparison a made-up value hides.
 * A fixture has to describe a state the product can actually be in.
 */
function bodyDigest(text: string, prefix = '## Two\n'): string {
  return digestOfBytes(Buffer.from(text.slice(prefix.length), 'utf8'));
}

/*
 * A PUBLISHED FILE WITH ITS RECORD BESIDE IT. The first version of these
 * cells wrote only the file, so every one of them was answered
 * `not-acknowledged` before it reached the branch it names -- red, but
 * for the wrong reason, which is a cell that would go green the day
 * something unrelated changed.
 */
function published(dir: string, text: string, over: Partial<Sidecar> = {}): string {
  fs.mkdirSync(dir, { recursive: true });
  const file = path.join(dir, '1.md');
  fs.writeFileSync(file, text, 'utf8');
  const sidecar: Sidecar = {
    format: 1,
    storeId: 's1',
    blockId: 'a.2',
    phase: 'published',
    prefix: '## Two\n',
    written: digestOfBytes(Buffer.from(text, 'utf8')),
    previous: null,
    acknowledgedRaw: null,
    sent: null,
    cursor: null,
    localOnly: false,
    unresolved: false,
    bodyHasCrlf: false,
    ...over
  };
  fs.writeFileSync(`${file}.meta`, JSON.stringify(sidecarToDisk(sidecar)), 'utf8');
  return file;
}

describe('C7 the answer is recorded before the entry is removed', () => {
  it('keeps the entry when the record could not be written', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    /*
     * NO RECORD BESIDE IT ON PURPOSE: this is the case where there is
     * nothing to write the answer into.
     */
    const saving = new Saving(new RecordingFs());
    const recorded = saving.recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'raw',
      sentDigest: 'sent',
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: false, because: 'not-acknowledged' },
      'the entry was released although nothing recorded the answer'
    );
  });

  /*
   * THE ORDER IS OBSERVED, NOT INFERRED. Both orders end with the entry
   * gone and the record written; only watching which happened first
   * tells them apart, which is why the recorder is here.
   */
  /*
   * THE REMOVAL IS PERFORMED BY THE SAME FUNCTION, so that the order is
   * observable. The first version of this cell looked for a write to
   * `outbox.json` that the code never made -- `queueAt` was -1 and the
   * assertion passed for every implementation, which is the absence-read-
   * as-success shape this batch keeps meeting.
   */
  it('writes the record first and removes the entry second', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text);
    const queue = path.join(dir, 'outbox.json');
    const files = new RecordingFs();
    const saving = new Saving(files);
    const recorded = saving.recordAnswer(
      file,
      {
        req: 'r1',
        cursor: 'w:7',
        rawDigest: digestOfBytes(Buffer.from(text, 'utf8')),
        sentDigest: bodyDigest(text),
        mismatch: false
      },
      () => files.writeText(queue, '{"entries":[]}')
    );
    assert.deepStrictEqual(recorded, { dequeued: true });
    /*
     * X1c ⑥: THE RECORD LANDS AT THE RENAME, NOT AT THE WRITE. This
     * looked for a `writeText` on the `.meta` path -- which found the
     * truncating rewrite this code used to do, and so endorsed it: the
     * cell would have gone red the day the write was made safe. What
     * makes the record visible is the rename of the temporary file onto
     * it, so that is the instant the order is about.
     */
    const sidecarAt = files.entries.findIndex(
      (e) => e.op === 'rename' && e.file.endsWith('.meta') && !e.file.endsWith('.tmp')
    );
    const queueAt = files.entries.findIndex((e) => e.op === 'writeText' && e.file.endsWith('outbox.json'));
    assert.ok(sidecarAt >= 0, 'the answer was never recorded beside the file');
    assert.strictEqual(
      files.entries.filter((e) => e.op === 'writeText' && e.file.endsWith('.meta')).length,
      0,
      'the record was opened for writing directly, which empties it first'
    );
    assert.ok(queueAt >= 0, 'the entry was never removed, so there is no order to observe');
    assert.ok(
      sidecarAt < queueAt,
      'the queue was written before the record, so a crash between them loses the request'
    );
  });

  /*
   * AND NOTHING IS REMOVED WHEN NOTHING WAS RECORDED. The twin of the
   * order above: a build that dequeued regardless would satisfy it.
   */
  it('does not remove the entry when it could not record the answer', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    let dequeued = 0;
    new Saving(new RecordingFs()).recordAnswer(
      file,
      { req: 'r1', cursor: 'w:7', rawDigest: 'raw', sentDigest: 'sent', mismatch: false },
      () => {
        dequeued += 1;
      }
    );
    assert.strictEqual(dequeued, 0, 'the entry was released although the answer was not recorded');
  });

  it('keeps the entry, marked unresolved, when the store reports a different request', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'raw',
      sentDigest: 'sent',
      mismatch: true
    });
    assert.deepStrictEqual(recorded, { dequeued: false, because: 'req-mismatch' });
  });

  /*
   * A LATE REPLAY MUST NOT MOVE THE CURSOR BACKWARDS. The comparison is
   * defined rather than assumed: the same writer's sequence numbers are
   * compared numerically, a different writer is a later generation.
   */
  it('does not take an older event as news', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text);
    const raw = digestOfBytes(Buffer.from(text, 'utf8'));
    const saving = new Saving(new RecordingFs());
    saving.recordAnswer(file, {
      req: 'r2',
      cursor: 'w:9',
      rawDigest: raw,
      sentDigest: bodyDigest(text),
      mismatch: false
    });
    const late = saving.recordAnswer(file, {
      req: 'r1',
      cursor: 'w:4',
      rawDigest: raw,
      sentDigest: 'sent',
      mismatch: false
    });
    assert.deepStrictEqual(
      late,
      { dequeued: true },
      'a late replay was treated as an error rather than as a duplicate'
    );
  });
});

describe('C20 the answer section gives up rather than describing bytes that moved', () => {
  /*
   * THE IMPLEMENTATION C20 NAMES: one that dequeues unconditionally. The
   * editor is not on the chain, so the file can change while the answer
   * is in flight; recording an acknowledgement against bytes that are no
   * longer there would mark a draft as sent.
   */
  it('abandons the write and keeps the entry when the file changed under it', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nwhat the user has now\n');
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'the digest of some older bytes',
      sentDigest: 'sent',
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: false, because: 'file-moved' },
      'an acknowledgement was recorded against bytes that are no longer in the file'
    );
  });
});

/*
 * X1c ⑧: WHAT THE ANSWER PUTS IN THE RECORD, READ BACK OFF THE DISK.
 *
 * The cells above watch what `recordAnswer` RETURNS and when it removes
 * the entry. Nothing read the record afterwards, so every field it
 * writes was unwatched: mutations that dropped `unresolved`, that put
 * the sent digest in the raw digest's field, and that wrote neither,
 * all survived a green suite. The values are the whole point of the
 * call -- the return value only says it happened.
 */
describe('X1c ⑧ the fields the answer writes into the record', () => {
  function recordOf(file: string): Sidecar {
    const read = sidecarFromDisk(fs.readFileSync(`${file}.meta`, 'utf8'));
    assert.ok(read.read, 'the record could not be read back');
    if (!read.read) {
      throw new Error('unreachable');
    }
    return read.sidecar;
  }

  it('marks the file unresolved when the store reports a different request', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'raw',
      sentDigest: 'sent',
      mismatch: true
    });
    const sidecar = recordOf(file);
    assert.strictEqual(sidecar.unresolved, true, 'the file was left looking ordinary');
    /*
     * AND NOTHING ELSE MOVED. A mismatch is not an acknowledgement: a
     * build that wrote the digests as well would mark the bytes as sent
     * on the strength of an answer that says the store has a different
     * request under that name.
     */
    assert.strictEqual(sidecar.acknowledgedRaw, null, 'a mismatch was recorded as an acknowledgement');
    assert.strictEqual(sidecar.sent, null, 'a mismatch recorded what was sent');
    assert.strictEqual(sidecar.cursor, null, 'a mismatch moved the cursor');
  });

  /*
   * THE TWO DIGESTS ARE DIFFERENT THINGS AND GO IN DIFFERENT PLACES.
   * `acknowledged-raw` is the whole file as it was on disk; `sent` is
   * the body that went to the store. Fixtures where the two are equal
   * cannot tell a build that swapped them from one that did not, so
   * they are deliberately different here.
   */
  it('records the file’s digest and the sent body’s digest in their own fields', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text);
    const raw = digestOfBytes(Buffer.from(text, 'utf8'));
    const sent = digestOfBytes(Buffer.from('body\n', 'utf8'));
    assert.notStrictEqual(raw, sent, 'the fixture cannot tell the two fields apart');
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: raw,
      sentDigest: sent,
      mismatch: false
    });
    assert.deepStrictEqual(recorded, { dequeued: true });
    const sidecar = recordOf(file);
    assert.strictEqual(sidecar.acknowledgedRaw, raw, 'the file’s digest is not what was recorded');
    assert.strictEqual(sidecar.sent, sent, 'the sent body’s digest is not what was recorded');
    assert.strictEqual(sidecar.cursor, 'w:7', 'the cursor the store gave was not kept');
    /*
     * AND THE FILE IS NO LONGER A DRAFT. That is what these two fields
     * are for: without them the digests disagree for ever and the block
     * is reported as holding unsent work after the store has taken it.
     */
    const standing = new Publisher(new RecordingFs(), { isOpen: () => false }).standingOf(file);
    assert.strictEqual(standing.kind, 'published');
    assert.strictEqual(
      standing.kind === 'published' && standing.draft,
      false,
      'the block still reads as holding unsent work after the store acknowledged it'
    );
  });

  /*
   * AND THE REST OF THE RECORD SURVIVES THE ANSWER. The answer replaces
   * three fields; a build that wrote a fresh record from the answer
   * alone would lose the prefix a save is split against, which is the
   * one field that cannot be recovered from anywhere else.
   */
  it('leaves every field the answer does not name exactly as it was', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text, { prefix: '## Two\n', bodyHasCrlf: true, previous: 'older' });
    const before = recordOf(file);
    new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: digestOfBytes(Buffer.from(text, 'utf8')),
      sentDigest: 'sent',
      mismatch: false
    });
    const after = recordOf(file);
    assert.strictEqual(after.prefix, before.prefix, 'the prefix a save is split against was lost');
    assert.strictEqual(after.bodyHasCrlf, before.bodyHasCrlf, 'what the block’s own body used was lost');
    assert.strictEqual(after.previous, before.previous);
    assert.strictEqual(after.written, before.written);
    assert.strictEqual(after.blockId, before.blockId);
    assert.strictEqual(after.storeId, before.storeId);
  });
});

/*
 * X1c ⑨: A RECONCILED VERSION STOPS BEING LOCAL-ONLY WHEN THE STORE
 * TAKES IT.
 *
 * `local-only` says the baseline came from the file rather than from an
 * answer, so the store has never seen these bytes -- and it makes the
 * version a draft whatever the digests say. An acknowledgement of those
 * bytes is exactly the refutation of it, and it was not being cleared:
 * the user resolved the conflict, saved, the store took it, and the
 * block went on reporting unsent work for ever with nothing they could
 * do about it.
 */
describe('X1c ⑨ a reconciled version after the store answers', () => {
  it('stops being local-only once the store has acknowledged its bytes', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text, { localOnly: true });
    const raw = digestOfBytes(Buffer.from(text, 'utf8'));
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: raw,
      sentDigest: digestOfBytes(Buffer.from('body\n', 'utf8')),
      mismatch: false
    });
    assert.deepStrictEqual(recorded, { dequeued: true });
    const read = sidecarFromDisk(fs.readFileSync(`${file}.meta`, 'utf8'));
    assert.ok(read.read);
    assert.strictEqual(
      read.read && read.sidecar.localOnly,
      false,
      'the store acknowledged the bytes and the record still says it has never seen them'
    );
    const standing = new Publisher(new RecordingFs(), { isOpen: () => false }).standingOf(file);
    assert.strictEqual(
      standing.kind === 'published' && standing.draft,
      false,
      'a reconciled version reported unsent work after the store took it'
    );
  });

  /*
   * AND IT IS STILL LOCAL-ONLY WHEN NOTHING WAS ACKNOWLEDGED. The twin:
   * a build that cleared the flag on every call would satisfy the cell
   * above and would throw away the one thing that makes a reconciled
   * baseline a draft.
   */
  it('is still local-only when the answer could not be recorded', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text, { localOnly: true });
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'the bytes that were sent, which are not the bytes here',
      sentDigest: 'sent',
      mismatch: false
    });
    assert.deepStrictEqual(recorded, { dequeued: false, because: 'file-moved' });
    const read = sidecarFromDisk(fs.readFileSync(`${file}.meta`, 'utf8'));
    assert.strictEqual(
      read.read && read.sidecar.localOnly,
      true,
      'a version the store never acknowledged was recorded as one it has seen'
    );
  });
});

/*
 * X1c ⑨: AN ANSWER TO A REQUEST THIS WINDOW DID NOT SEND.
 *
 * A retry after a restart names a request the queue remembers and the
 * process does not. Nothing said which file it was about, so nothing was
 * recorded: the store held the bytes and the record beside the file went
 * on saying it did not, so that block reported unsent work for ever --
 * and saving it again would send the store bytes it already has under a
 * new request.
 *
 * `recognise` answers the question the queue can still answer: does the
 * file still hold exactly the body that went out? It does not write --
 * recording stays in `recordAnswer`, so the order lives in one place.
 */
describe('X1c ⑨ matching a retried answer back to the version it was sent from', () => {
  it('recognises the version whose body is the text that was sent', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text);
    const found = new Saving(new RecordingFs()).recognise(file, 'body\n');
    assert.ok(found !== null, 'the file holding exactly what was sent was not recognised');
    assert.strictEqual(found?.rawDigest, digestOfBytes(Buffer.from(text, 'utf8')));
    assert.strictEqual(found?.sentDigest, digestOfBytes(Buffer.from('body\n', 'utf8')));
  });

  /*
   * AND WHAT IT RECOGNISES CAN THEN BE RECORDED, which is the whole
   * point: the block stops reporting work the store already has.
   */
  it('produces digests the answer section accepts', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const file = published(dir, text);
    const saving = new Saving(new RecordingFs());
    const found = saving.recognise(file, 'body\n');
    assert.ok(found !== null);
    const recorded = saving.recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: found?.rawDigest ?? '',
      sentDigest: found?.sentDigest ?? '',
      mismatch: false
    });
    assert.deepStrictEqual(recorded, { dequeued: true });
    const standing = new Publisher(new RecordingFs(), { isOpen: () => false }).standingOf(file);
    assert.strictEqual(
      standing.kind === 'published' && standing.draft,
      false,
      'the block still reported unsent work after the retried answer was recorded'
    );
  });

  /*
   * THE REFUSALS, WHICH ARE WHAT MAKE IT SAFE. Each of these is a file
   * that must NOT be recorded as acknowledged, and a build that answered
   * on the strength of the block id alone would record every one of
   * them -- marking edits the store has never seen as sent.
   */
  it('does not recognise a file the user has edited since', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    fs.writeFileSync(file, '## Two\nbody and more\n', 'utf8');
    assert.strictEqual(
      new Saving(new RecordingFs()).recognise(file, 'body\n'),
      null,
      'an edit the store has never seen was matched to an answer'
    );
  });

  it('does not recognise a file holding a third version', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n', { unresolved: true });
    assert.strictEqual(new Saving(new RecordingFs()).recognise(file, 'body\n'), null);
  });

  it('does not recognise a publication that never finished', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n', { phase: 'publishing' });
    assert.strictEqual(new Saving(new RecordingFs()).recognise(file, 'body\n'), null);
  });

  it('does not recognise a file whose heading is no longer the recorded one', () => {
    const dir = scratch();
    const file = published(dir, '## Three\nbody\n');
    assert.strictEqual(
      new Saving(new RecordingFs()).recognise(file, 'body\n'),
      null,
      'a file that would not split against its own record was matched anyway'
    );
  });

  it('does not recognise a file that is not there', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    fs.unlinkSync(file);
    assert.strictEqual(new Saving(new RecordingFs()).recognise(file, 'body\n'), null);
  });

  /*
   * AND IT WRITES NOTHING. Recognising is a question; a build that
   * recorded while answering it would put the order -- record first,
   * entry second -- in a second place.
   */
  it('writes nothing while it is only being asked', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    const files = new RecordingFs();
    new Saving(files).recognise(file, 'body\n');
    assert.deepStrictEqual(files.touched('writeText'), []);
    assert.deepStrictEqual(files.touched('writeDurably'), []);
    assert.deepStrictEqual(files.touched('rename'), []);
  });
});

/*
 * X1c ⑨: AND IT ANSWERS "CANNOT SAY" RATHER THAN THROWING.
 *
 * `recognise` runs inside the settler, where the store has already
 * answered and the entry has not yet been removed. A throw from it would
 * escape into the Saver's answer handling and turn a question that could
 * not be answered into a failure of the save it was asked about.
 */
describe('X1c ⑨ recognising cannot fail the answer it is asked about', () => {
  it('answers null when the record cannot be read at all', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    const throwing = new (class extends RecordingFs {
      public readText(target: string): string {
        if (target.endsWith('.meta')) {
          throw new Error('the record went away between the look and the read');
        }
        return super.readText(target);
      }
    })();
    assert.strictEqual(new Saving(throwing).recognise(file, 'body\n'), null);
  });

  it('answers null when the file cannot be read at all', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    const throwing = new (class extends RecordingFs {
      public readBytes(target: string): Buffer {
        throw new Error(`${target} went away between the look and the read`);
      }
    })();
    assert.strictEqual(new Saving(throwing).recognise(file, 'body\n'), null);
  });
});

/*
 * REVIEW ROUND 20: THE RECORD MUST STILL SPLIT THE FILE THE WAY THE SEND
 * DID.
 *
 * The digest check in `recordAnswer` says the BYTES have not moved. It
 * says nothing about the PREFIX -- which lives in the same record and
 * can be changed by something else while the answer is in flight.
 * `reconcile` adopting a longer heading does exactly that and leaves the
 * file's bytes untouched.
 *
 * The sequence, from the review, with a reproduction: the file holds
 * `P + X + Y` and the record says the prefix is `P`, so the save sends
 * `X + Y`. While that answer is outstanding the prefix becomes `P + X`.
 * The answer arrives, the whole-file digest still matches, the
 * acknowledgement is recorded and `local-only` cleared -- so the version
 * reads as fully sent while a save under the record as it now stands
 * would send `Y`, which the store has never seen. Work the user can no
 * longer see is pending.
 */
describe('review 20 an answer is recorded only if the record still produces what was sent', () => {
  it('declines when the heading grew while the answer was in flight', () => {
    const dir = scratch();
    const text = '## Two\nX\nY\n';
    const file = published(dir, text, { localOnly: true });
    const raw = digestOfBytes(Buffer.from(text, 'utf8'));
    const sent = digestOfBytes(Buffer.from('X\nY\n', 'utf8'));

    /*
     * THE PREFIX CHANGES AND THE BYTES DO NOT. That is the whole point:
     * every digest of the file is still what it was.
     */
    const before = sidecarFromDisk(fs.readFileSync(`${file}.meta`, 'utf8'));
    assert.ok(before.read);
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(sidecarToDisk({ ...(before.read ? before.sidecar : ({} as Sidecar)), prefix: '## Two\nX\n' })),
      'utf8'
    );
    assert.strictEqual(
      digestOfBytes(fs.readFileSync(file)),
      raw,
      'the fixture moved the bytes, so it is not about the prefix any more'
    );

    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: raw,
      sentDigest: sent,
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: false, because: 'split-changed' },
      'an acknowledgement of X+Y was recorded against a record that now sends only Y'
    );
    /*
     * AND THE RECORD IS UNTOUCHED, so the block still reports the work
     * the store has not got.
     */
    const after = sidecarFromDisk(fs.readFileSync(`${file}.meta`, 'utf8'));
    assert.ok(after.read);
    assert.strictEqual(after.read && after.sidecar.acknowledgedRaw, null);
    assert.strictEqual(after.read && after.sidecar.localOnly, true, 'local-only was cleared anyway');
  });

  /*
   * THE GREEN TWIN. Without it the check above is satisfied by a build
   * that declines every answer, which loses every acknowledgement there
   * is.
   */
  it('records the answer when the record still produces exactly what was sent', () => {
    const dir = scratch();
    const text = '## Two\nX\nY\n';
    const file = published(dir, text);
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: digestOfBytes(Buffer.from(text, 'utf8')),
      sentDigest: digestOfBytes(Buffer.from('X\nY\n', 'utf8')),
      mismatch: false
    });
    assert.deepStrictEqual(recorded, { dequeued: true });
  });

  /*
   * AND THE MISMATCH BRANCH WRITES ATOMICALLY TOO. Restoring the
   * truncating write in that one branch survived every cell: the
   * mismatch cells read the fields the write produced and never looked
   * at how it was made.
   */
  it('replaces the record through a rename when it marks a mismatch', () => {
    const dir = scratch();
    const file = published(dir, '## Two\nbody\n');
    const files = new RecordingFs();
    new Saving(files).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: 'raw',
      sentDigest: 'sent',
      mismatch: true
    });
    assert.strictEqual(
      files.entries.filter((e) => e.op === 'writeText' && e.file.endsWith('.meta')).length,
      0,
      'the record was opened for writing directly, which empties it first'
    );
    assert.ok(
      files.entries.some((e) => e.op === 'rename' && e.file.endsWith('.meta')),
      'the record was not published by a rename'
    );
  });
});

/*
 * REVIEW ROUND 21: THE ACKNOWLEDGEMENT IS ASSEMBLED FROM ONE READING OF
 * THE FILE, AND THE REFUSAL IS NOT ABOUT `local-only`.
 *
 * Three mutations survived the round-20 cells. Each is here.
 */
describe('review 21 what the answer is checked against', () => {
  /*
   * ⚠️ ONE BUFFER, TWO QUESTIONS. The digest and the split were two
   * separate reads, and the writer they are about is another process. A
   * write landing between them let an acknowledgement be assembled out
   * of two different versions: the first satisfied the digest, the
   * second satisfied the split, and neither on its own would have.
   */
  it('does not assemble an answer out of two different versions of the file', () => {
    const dir = scratch();
    const sent = '## Two\nX\nY\n';
    const file = published(dir, sent, { prefix: '## Two\nX\n' });
    const racing = new (class extends RecordingFs {
      public readBytes(target: string): Buffer {
        const bytes = super.readBytes(target);
        if (target === file) {
          /*
           * AFTER THE READ THAT THE DIGEST IS TAKEN FROM, somebody
           * writes a file which the CURRENT record happens to split into
           * exactly the body that was sent.
           */
          fs.writeFileSync(file, '## Two\nX\nX\nY\n', 'utf8');
        }
        return bytes;
      }
    })();
    const recorded = new Saving(racing).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: digestOfBytes(Buffer.from(sent, 'utf8')),
      sentDigest: digestOfBytes(Buffer.from('X\nY\n', 'utf8')),
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: false, because: 'split-changed' },
      'the digest was taken from one version of the file and the body from another'
    );
  });

  /*
   * AND THE SPLIT USES THE SAME RULE THE SEND USED. A build that sliced
   * the text at `prefix.length` instead of asking `bodyOf` passes every
   * LF fixture and refuses ordinary CRLF saves the store did accept.
   */
  it('splits the way the send did, so a normalised save is still recorded', () => {
    const dir = scratch();
    const text = '## Two\r\nbody\r\n';
    const file = published(dir, text, { prefix: '## Two\n' });
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: digestOfBytes(Buffer.from(text, 'utf8')),
      /*
       * WHAT WENT OUT WAS NORMALISED: the block's own body has no CRLF,
       * so the save folded it. The record has to reach the same answer.
       */
      sentDigest: digestOfBytes(Buffer.from('body\n', 'utf8')),
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: true },
      'an ordinary normalised save was refused because the record was split by raw length'
    );
  });

  /*
   * AND THE REFUSAL IS NOT ABOUT `local-only`. A build that only checked
   * the split for reconciled versions passed both fixtures of round 20 --
   * the refusing one had it set, the accepting one did not -- and let
   * every ordinary record be acknowledged wrongly.
   */
  it('refuses a changed split on an ordinary version too', () => {
    const dir = scratch();
    const text = '## Two\nX\nY\n';
    const file = published(dir, text, { prefix: '## Two\nX\n', localOnly: false });
    const recorded = new Saving(new RecordingFs()).recordAnswer(file, {
      req: 'r1',
      cursor: 'w:7',
      rawDigest: digestOfBytes(Buffer.from(text, 'utf8')),
      sentDigest: digestOfBytes(Buffer.from('X\nY\n', 'utf8')),
      mismatch: false
    });
    assert.deepStrictEqual(
      recorded,
      { dequeued: false, because: 'split-changed' },
      'the check only applies to reconciled versions, so ordinary ones are acknowledged wrongly'
    );
  });
});
