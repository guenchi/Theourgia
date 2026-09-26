import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { spawnSync } from 'child_process';
import { Publisher } from '../../src/publication';
import { Owners } from '../../src/ownership';
import { RecordingFs } from '../support/recording-fs';

function schedule(name: string): any {
  const run = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {encoding:'utf8', timeout:20000});
  assert.strictEqual(run.status, 0, run.stdout + run.stderr);
  const rows = run.stdout.trim().split('\n');
  const answer = JSON.parse(rows[rows.length - 1]);
  assert.strictEqual(answer.complete, true);
  return answer.result;
}

describe('XR reconcile keeps the offered source across waits', function () {
  this.timeout(90000);
  for (const name of ['reconcile-switch', 'reconcile-chain-switch', 'reconcile-control']) {
    it(`XR-01 captured target ${name}`, () => {
      const result = schedule(name);
      assert.deepStrictEqual(result.afterB, result.beforeB, 'The other store was modified');
      const contents: string[] = Object.entries(result.afterA).filter(([p])=>p.endsWith('.md')).map(([,b])=>Buffer.from(b as string,'hex').toString());
      assert.ok(contents.includes('# Alpha\noffered text\n'), 'The offered source was not used');
      assert.deepStrictEqual(result.requests.filter((r:any)=>r.verb==='write').map((r:any)=>r.store),['/stores/A'],'W writes retain the captured store across the picker');
    });
  }
  it('XR-07 dirty during confirmation refuses publication visibly', () => {
    const result = schedule('reconcile-dirty');
    assert.deepStrictEqual(result.afterA, result.beforeA);
    assert.strictEqual(result.notice.level, 'warning');
    assert.match(result.notice.text, /unsaved|dirty/);
  });
  it('XR-03 changed offer retains the later file and refuses', () => {
    const result = schedule('reconcile-changed');
    const contents = Object.entries(result.afterA).filter(([p])=>p.endsWith('.md')).map(([,b])=>Buffer.from(b as string,'hex').toString());
    assert.deepStrictEqual(contents, ['later edit\n']);
    assert.strictEqual(result.notice.level, 'warning');
  });
  it('XR-05 immediate ownership denial names the reason', async () => {
    const files = new RecordingFs(), directory = fs.mkdtempSync(path.join(os.tmpdir(), 'xr-owner-'));
    const owners = new Owners(files);
    const publisher = new Publisher(files, {isOpen:()=>false}, {owners,sessionId:'first'});
    const result = await publisher.publish({directory,storeId:'A',blockId:'a.1',prefix:'# A\n',text:'# A\nbody\n',cursor:null,expected:null});
    assert.ok(result.published);
    if (!result.published) return;
    const other = new Publisher(files,{isOpen:()=>false},{owners,sessionId:'second'});
    const before = fs.readFileSync(result.file+'.meta');
    const refusal = other.reconcile(result.file,'# A\n','# A\nbody\n');
    assert.strictEqual((refusal as any).because,'not-ours');
    assert.deepStrictEqual(fs.readFileSync(result.file+'.meta'),before);
  });
});

/*
 * queue item 43: A RECORD THAT MOVES WHILE A READING OR A PICK IS OPEN. (design
 * v4 R3-R8, ruling Q2) What the extension did is read from the trace the
 * harness keeps around the extension's own Publisher -- each guard,
 * reconciliation and publication, with the stage it refused at and why -- not
 * from a notice's wording; a sentence is checked only where the design names
 * it. The harness is test/support/extension-schedules.js.
 */
const MOVED_EARLY = 'changed while you were choosing; pick again.';
const MOVED_LATE = 'its record changed while the reconciliation was being applied';

/*
 * THE SENTENCE REACHED THE EDITOR (review r3 S2): a returned notice that was
 * never shown is a refusal the user does not see. `shown` is cleared by each
 * scenario just before the step under test.
 */
function shownOnce(r: any, sentence: string): void {
  const hits = r.shown.filter((n: any) => n.level === 'warning' && n.text.includes(sentence));
  assert.strictEqual(hits.length, 1, `the warning was not shown exactly once: ${JSON.stringify(r.shown)}`);
}

function parentOf(args: string[]): { writer: string | null; version: string | null } {
  const at = (flag: string) => (args.indexOf(flag) < 0 ? null : args[args.indexOf(flag) + 1]);
  return { writer: at('--working-parent-writer'), version: at('--working-parent') };
}

describe('queue item 43 the record a reading or a pick was taken from', function () {
  this.timeout(90000);

  it('R3 open-moved: an open whose record moved while it read is refused, and the newer write stays', () => {
    const r = schedule('open-moved');
    assert.strictEqual(r.entered, true, 'the second open never reached its reading, so nothing moved under it');
    assert.strictEqual(r.moved, r.before.revision + 1, 'the second publisher did not move the record');
    assert.deepStrictEqual(r.trace, [{ stage: 'publish', because: 'record-moved' }]);
    assert.strictEqual(r.file, '# Alpha\nthe second write\n', 'the open wrote over the second write');
    assert.strictEqual(r.after.revision, r.moved, 'the record moved again after the second write');
    assert.strictEqual(r.after.projection.version, 'v-second');
    assert.ok(
      r.shown.some((n: any) => n.level === 'warning' && n.text.includes('(record-moved)')),
      `the refusal does not name its reason: ${JSON.stringify(r.shown)}`
    );
  });

  it('R4 midpick-save: a save while the pick is open refuses the pick, early, on the record', () => {
    const r = schedule('midpick-save');
    assert.strictEqual(r.choice, 'prepend-prefix');
    assert.strictEqual(r.afterSave.revision > r.beforePick.revision, true, 'the save did not move the record');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: 'record-moved' }]);
    assert.deepStrictEqual(r.afterReconcile, r.afterSave, 'the record changed after the save');
    assert.strictEqual(r.afterReconcile.projection.version, 'v1');
    assert.deepStrictEqual(r.afterReconcile.outstanding.map((o: any) => o.seq), [1]);
    assert.strictEqual(r.file, '# Alpha\nsaved while the pick was open\n');
    assert.strictEqual(r.writes, 1, 'a working write was made for the refused pick');
  });

  /*
   * R5 IS ITEM 47'S CASE: the bytes are back to the offered ones, the record is
   * not. A bytes-only guard applies the pick over the save's record (item 47
   * measured: record v2, two writes).
   */
  it('R5 midpick-aba: bytes put back after a save do not let the pick through', () => {
    const r = schedule('midpick-aba');
    assert.strictEqual(r.choice, 'prepend-prefix');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: 'record-moved' }]);
    assert.deepStrictEqual(r.afterReconcile, r.afterSave, 'the pick was applied over the save\'s record');
    assert.strictEqual(r.afterReconcile.projection.version, 'v1');
    assert.deepStrictEqual(r.afterReconcile.outstanding.map((o: any) => o.seq), [1]);
    assert.strictEqual(r.queue.length, 1);
    assert.strictEqual(JSON.parse(r.queue[0].expectation).version, 'v1', 'the queued save no longer expects its own version');
    assert.strictEqual(r.writes, 1, 'a working write was made for the refused pick');
    assert.strictEqual(r.file, 'offered text\n');
    assert.strictEqual(r.notice.level, 'warning');
    assert.ok(r.notice.text.includes(MOVED_EARLY), r.notice.text);
    shownOnce(r, MOVED_EARLY);
  });

  it('R6 midpick-prechain-save: an offer built after a save that landed before the first wait is applied on that save', () => {
    const r = schedule('midpick-prechain-save');
    assert.ok(r.landed !== null, 'the save never landed before the first wait');
    assert.strictEqual(r.landed.projection.kind, 'working');
    assert.strictEqual(r.choice, 'prepend-prefix', 'the offer\'s route was not taken');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: null }, { stage: 'late', because: null }]);
    assert.strictEqual(r.reconcileWrites.length, 1, `not one working write for the pick: ${JSON.stringify(r.reconcileWrites)}`);
    assert.deepStrictEqual(parentOf(r.reconcileWrites[0]), { writer: r.landed.projection.writer, version: r.landed.projection.version });
    assert.strictEqual(r.file, '# Alpha\noffered text\n');
  });

  it('R6b midpick-afterguard: a record that moves after the guard is written against the offer\'s projection and refused late', () => {
    const r = schedule('midpick-afterguard');
    assert.strictEqual(r.second, r.beforePick.revision + 1, 'the second publisher did not move the record after the guard');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: null }, { stage: 'late', because: 'record-moved' }]);
    assert.strictEqual(r.reconcileWrites.length, 1);
    assert.deepStrictEqual(parentOf(r.reconcileWrites[0]), { writer: r.beforePick.projection.writer, version: r.beforePick.projection.version });
    assert.notStrictEqual(r.beforePick.projection.version, 'v-second');
    assert.strictEqual(r.file, 'offered text\n', 'the file was replaced over a record that moved');
    assert.strictEqual(r.after.projection.version, 'v-second', 'the newer record did not survive');
    assert.ok(r.notice.text.includes(MOVED_LATE), r.notice.text);
    shownOnce(r, MOVED_LATE);
  });

  /*
   * REVIEW R1 #1: THE OFFER AND ITS RECORD ARE READ TOGETHER. A record written
   * by another process right after reconcile's exclusive section ends is newer
   * than the offer; the second wait must refuse it, early.
   */
  it('R1#1 midpick-afterreconcile: a record written after reconcile left its section is newer than the offer, and refused early', () => {
    const r = schedule('midpick-afterreconcile');
    assert.strictEqual(r.second, r.beforePick.revision + 1, 'the second publisher did not move the record after reconcile');
    assert.strictEqual(r.choice, 'prepend-prefix');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: 'record-moved' }]);
    assert.strictEqual(r.file, 'offered text\n', 'the pick was applied over the newer record');
    assert.strictEqual(r.after.projection.version, 'v-second', 'the newer record did not survive');
    assert.ok(r.notice.text.includes(MOVED_EARLY), r.notice.text);
    shownOnce(r, MOVED_EARLY);
  });

  /*
   * REVIEW R1 #5 AND #6: WHAT AN OPEN OR A SAVE NAMES IS READ INSIDE ITS WAIT.
   * A chain callback queued ahead of it moves the record first; read before
   * the wait, the revision is stale and the open or the save is refused.
   */
  it('R1#5 open-queued: an open queued behind a move of its record publishes', () => {
    const r = schedule('open-queued');
    assert.strictEqual(r.entered, true, 'the open never queued behind the move, so nothing was measured');
    assert.deepStrictEqual(r.trace, [{ stage: 'publish', because: null }]);
    assert.ok(r.after.revision > r.before.revision + 1, 'the record did not move ahead of the open and then with it');
  });

  it('R1#6 queued-save: a save queued behind a move of its record is recorded', () => {
    const r = schedule('queued-save');
    assert.strictEqual(r.entered, true, 'the save never queued behind the move, so nothing was measured');
    assert.strictEqual(r.after.projection.kind, 'working');
    assert.notStrictEqual(r.after.projection.version, r.before.projection.version, 'the save was not recorded');
    assert.ok(!r.messages.some((m: string) => m.includes('not confirmed') || m.includes('working note')), JSON.stringify(r.messages));
  });

  /*
   * REVIEW R3 S1: THE AUTOMATIC ROUTE NAMES THE RECORD reconcileLeaving
   * RETURNED. A settlement landing during its working write moves the
   * record; the note is not recorded over it, and the reconciliation does
   * not report success.
   */
  it('R3S1 autoreconcile-settlement: a settlement during the automatic route\'s working write leaves the note unrecorded', () => {
    const r = schedule('autoreconcile-settlement');
    assert.strictEqual(r.commitEntered, true, 'the first request\'s commit was never sent, so nothing was settled');
    assert.strictEqual(r.writeEntered, true, 'the automatic route\'s working write was never sent');
    assert.ok(r.left !== null, 'reconcileLeaving was never called');
    assert.strictEqual(r.advanced, true, 'the settlement did not move the record before the working write was let go');
    assert.strictEqual(r.after.revision, r.left.revision + 1, 'something besides the settlement wrote the record');
    assert.deepStrictEqual(r.after.projection, r.left.projection, 'the note was recorded over a record that moved');
    assert.strictEqual(r.notice.level, 'warning', `the reconciliation reported success: ${JSON.stringify(r.notice)}`);
    /*
     * `show` puts the extension's name in front of the text, so the shown
     * sentence CONTAINS the notice's text.
     */
    assert.ok(r.shown.some((n: any) => n.level === 'warning' && n.text.includes(r.notice.text)), JSON.stringify(r.shown));
  });

  /*
   * R8 (U6, V1): THE ACCEPTED COST, PINNED. A settlement of this window's own
   * drain does not wait on the block chain; one that lands during the pick's
   * working write moves the record, and reconcileBy refuses late.
   */
  it('R8 midpick-settlement: a settlement during the pick\'s working write refuses the reconciliation late', () => {
    const r = schedule('midpick-settlement');
    assert.strictEqual(r.commitEntered, true, 'the first request\'s commit was never sent, so nothing was settled');
    assert.strictEqual(r.writeEntered, true, 'the pick\'s working write was never sent');
    assert.strictEqual(r.advanced, true, 'the settlement did not move the record before the working write was let go');
    assert.deepStrictEqual(r.trace, [{ stage: 'early', because: null }, { stage: 'late', because: 'record-moved' }]);
    assert.strictEqual(r.file, 'offered text\n', 'the file was replaced over a record that moved');
    assert.ok(!r.queueAfter.some((e: any) => e.req === r.pendingBefore[0].req), 'the retired request is still queued');
    assert.ok(r.notice.text.includes(MOVED_LATE), r.notice.text);
    shownOnce(r, MOVED_LATE);
  });

  /*
   * Q2: THE SAME COST ON THE SAVE PATH. recordWorking names the revision read
   * at the start of the save's wait; a settlement landing during the save's
   * working write makes it refuse, and the save is reported unrecorded.
   */
  it('Q2 working-save-settlement: a settlement during a save\'s working write leaves the save unrecorded', () => {
    const r = schedule('working-save-settlement');
    assert.strictEqual(r.commitEntered, true, 'the first request\'s commit was never sent, so nothing was settled');
    assert.strictEqual(r.writeEntered, true, 'the save\'s working write was never sent');
    assert.strictEqual(r.advanced, true, 'the settlement did not move the record before the working write was let go');
    assert.strictEqual(r.after.revision, r.held.revision + 1, 'something besides the settlement wrote the record');
    assert.deepStrictEqual(r.after.projection, r.held.projection, 'the save was recorded over a record that moved');
    assert.deepStrictEqual(
      r.queueAfter.map((e: any) => e.req),
      r.pendingBefore.slice(1).map((e: any) => e.req),
      'the queue is not the second request alone'
    );
    assert.ok(r.messages.some((m: string) => m.includes('not confirmed') || m.includes('working note')), JSON.stringify(r.messages));
  });
});
