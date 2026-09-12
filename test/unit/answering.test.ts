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
import { digestOfBytes, Sidecar, sidecarToDisk } from '../../src/publication';
import { Saving } from '../../src/saving';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-answer-'));
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
        sentDigest: 'sent',
        mismatch: false
      },
      () => files.writeText(queue, '{"entries":[]}')
    );
    assert.deepStrictEqual(recorded, { dequeued: true });
    const sidecarAt = files.entries.findIndex((e) => e.op === 'writeText' && e.file.endsWith('.meta'));
    const queueAt = files.entries.findIndex((e) => e.op === 'writeText' && e.file.endsWith('outbox.json'));
    assert.ok(sidecarAt >= 0, 'the answer was never recorded beside the file');
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
    saving.recordAnswer(file, { req: 'r2', cursor: 'w:9', rawDigest: raw, sentDigest: 'sent', mismatch: false });
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
