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
 * Which reading of a block is the baseline when two opens overlap.
 *
 * THIS IS HERE BECAUSE THE EDITOR CANNOT BE MADE TO DO IT. Opening a
 * block is three VS Code waits long, nothing can widen them, and the
 * interleaving that matters -- an older open finishing after a newer one
 * -- is therefore not reachable from an editor-hosted cell. The rule was
 * lifted out of `activate` so that it could be driven directly, which is
 * the only way it is guarded at all.
 *
 * WHAT IT COSTS WHEN IT IS WRONG is not a refusal. The save path splits
 * the buffer against the baseline's prefix; a baseline older than the
 * buffer has a prefix the buffer no longer starts with, and the heading
 * is then taken for body and written into the block.
 */

import * as assert from 'assert';
import { OpenBuffers } from '../../src/open';

describe('the newest reading of a block is the one a save is measured against', () => {
  it('refuses a registration from an open that started earlier and finished later', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    const newer = open.claim();
    assert.strictEqual(open.register('/f', 'newer', newer), 'taken');
    assert.strictEqual(
      open.register('/f', 'older', older),
      'superseded-by-read',
      'an open that started first was allowed to overwrite a later reading'
    );
    assert.strictEqual(open.get('/f'), 'newer', 'the stale reading became the baseline');
  });

  it('takes a registration from an open that started later', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    const newer = open.claim();
    assert.strictEqual(open.register('/f', 'older', older), 'taken');
    assert.strictEqual(open.register('/f', 'newer', newer), 'taken');
    assert.strictEqual(open.get('/f'), 'newer', 'a newer reading was refused');
  });

  /*
   * THE ORDER IS PER FILE. Two blocks opened together must not order
   * each other: the second block's ticket is higher than the first's, so
   * a rule that compared tickets globally would refuse the first block's
   * registration and leave it with no baseline at all.
   */
  it('orders each file on its own', () => {
    const open = new OpenBuffers<string>();
    const first = open.claim();
    const second = open.claim();
    assert.strictEqual(open.register('/b', 'b', second), 'taken');
    assert.strictEqual(open.register('/a', 'a', first), 'taken');
    assert.strictEqual(open.get('/a'), 'a');
    assert.strictEqual(open.get('/b'), 'b');
  });

  it('re-registers the same ticket, which is one open finishing once', () => {
    const open = new OpenBuffers<string>();
    const ticket = open.claim();
    assert.strictEqual(open.register('/f', 'one', ticket), 'taken');
    assert.strictEqual(open.register('/f', 'two', ticket), 'taken', 'one open could not revise itself');
  });

  /*
   * WHAT THE STORE CONFIRMED OUTRANKS A READ THAT BEGAN BEFORE IT. The
   * first version of this class kept the ticket already present when a
   * save was confirmed, so a read that started earlier and answered
   * later still outranked it -- and that read's answer predates the
   * bytes the store has just accepted.
   */
  it('lets a confirmed save outrank an open that started before it', () => {
    const open = new OpenBuffers<string>();
    const started = open.claim();
    open.register('/f', 'read', started);
    open.confirmed('/f', 'saved');
    assert.strictEqual(
      open.register('/f', 'the older read, arriving late', started),
      'superseded-by-save',
      'a read that began before the save was confirmed overwrote what the store accepted'
    );
    assert.strictEqual(open.get('/f'), 'saved');
  });

  /*
   * INCLUDING A READ THAT STARTED AFTER THE SAVE WAS CAPTURED BUT BEFORE
   * IT WAS ANSWERED. This is the ordering that made the previous rule
   * look sound: the open is newer than the save's own baseline, so
   * keeping the existing ticket seemed to order them correctly -- but
   * the save is answered later still, and it is the store speaking.
   */
  it('outranks an open that started while the save was in flight', () => {
    const open = new OpenBuffers<string>();
    const first = open.claim();
    open.register('/f', 'first', first);
    const during = open.claim();
    open.confirmed('/f', 'saved');
    assert.strictEqual(
      open.register('/f', 'read during the save', during),
      'superseded-by-save',
      'the refusal did not say a save was what outranked it'
    );
    assert.strictEqual(open.get('/f'), 'saved');
  });

  /*
   * AND AN OPEN THAT STARTED AFTERWARDS STILL WINS, or the rule above
   * would freeze the baseline at the last save and no later reading of
   * the store could ever replace it.
   */
  it('still admits an open that started after the save was confirmed', () => {
    const open = new OpenBuffers<string>();
    open.confirmed('/f', 'saved');
    const after = open.claim();
    assert.strictEqual(open.register('/f', 'read afterwards', after), 'taken');
    assert.strictEqual(open.get('/f'), 'read afterwards');
  });

  /*
   * A WRITE THAT FAILED RECORDS NOTHING. Admission has to happen before
   * the file is written -- a losing open must not put its older text
   * there -- but a baseline recorded for a write that then failed
   * describes a file nobody wrote. When its prefix is empty the save
   * path does not refuse the mismatch: it takes the buffer's heading for
   * body and sends it.
   */
  it('keeps the previous baseline when the write it admitted fails', () => {
    const open = new OpenBuffers<string>();
    open.register('/f', 'already there', open.claim());
    const ticket = open.claim();
    assert.throws(
      () =>
        open.register('/f', 'never written', ticket, () => {
          throw new Error('EACCES');
        }),
      /EACCES/
    );
    assert.strictEqual(
      open.get('/f'),
      'already there',
      'a baseline was recorded for a write that failed'
    );
  });

  /*
   * INCLUDING THE FIRST REGISTRATION FOR A FILE. A rollback written as
   * "record, commit, and put the previous entry back if there was one"
   * passes every other cell here and leaves a baseline behind exactly
   * when there was nothing to restore -- which is the case a fresh file
   * is always in.
   */
  it('records nothing when the write for a file nobody had opened fails', () => {
    const open = new OpenBuffers<string>();
    assert.throws(
      () =>
        open.register('/fresh', 'never written', open.claim(), () => {
          throw new Error('EACCES');
        }),
      /EACCES/
    );
    assert.strictEqual(
      open.get('/fresh'),
      undefined,
      'a baseline was recorded for the first write to a file, and that write failed'
    );
  });

  it('records the baseline when the write it admitted succeeds', () => {
    const open = new OpenBuffers<string>();
    let wrote = 0;
    assert.strictEqual(open.register('/f', 'written', open.claim(), () => { wrote += 1; }), 'taken');
    assert.strictEqual(wrote, 1, 'the write was not run');
    assert.strictEqual(open.get('/f'), 'written');
  });

  /*
   * AND A REFUSED REGISTRATION DOES NOT WRITE AT ALL. The write is run
   * between the decision and the record precisely so that a losing open
   * never reaches the file.
   */
  it('does not run the write when the registration is refused', () => {
    const open = new OpenBuffers<string>();
    const older = open.claim();
    open.register('/f', 'newer', open.claim());
    let wrote = 0;
    assert.strictEqual(
      open.register('/f', 'older', older, () => { wrote += 1; }),
      'superseded-by-read'
    );
    assert.strictEqual(wrote, 0, 'a losing open wrote its older text to the file');
  });

  it('has no baseline for a file nobody opened', () => {
    assert.strictEqual(new OpenBuffers<string>().get('/f'), undefined);
  });
});
