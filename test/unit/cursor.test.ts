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
 * Where a cursor comes from. Every wire string in this file was copied
 * from a real run of the core against a scratch store, not composed from
 * the design note: the note says `(ok (event (w . 9)))` and the core
 * prints `(ok (events (("w" . 9))) (state ...) (cursor ("w" . 9))
 * (replay #f))`, and only one of those is what arrives.
 */

import * as assert from 'assert';
import { Answer } from '../../src/client';
import { eventFromWrite, firstCursorFromCheck, isReplay, isWellFormedCursor, writersFromCheck } from '../../src/cursor';
import { formatCursor } from '../../src/wire';
import { initWire, wire } from '../../src/wire';

function answerOf(text: string, rc = 0): Answer {
  return {
    argv: [],
    rc,
    ok: rc === 0,
    kind: 'datum',
    text,
    answers: [wire().read(text)],
    envelope: null,
    stderr: ''
  };
}

const FRESH_WRITE =
  '(ok (events (("fsu7hd1k" . 6))) (state (("fsu7hd1k.3" . "b3bc5100179d830f44f54caeb86f2954782d63eafb60814ef58f87ad5365c951"))) (cursor ("fsu7hd1k" . 6)) (replay #f))';
const REPLAYED_WRITE = '(ok (replay #t) (event ("fsu7hd1k" . 6)))';
const CHECK_ONE_WRITER =
  '(check (store "tasul7cl") (writers (("fsu7hd1k" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))';
const CHECK_TWO_WRITERS =
  '(check (store "tasul7cl") (writers (("fsu7hd1k" (end 7) (torn #f) (integrity ())) ("other123" (end 2) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))';

describe('the cursor a write carries forward', () => {
  before(async () => {
    await initWire();
  });

  it('reads the cursor of a write that landed', () => {
    assert.deepStrictEqual(eventFromWrite(answerOf(FRESH_WRITE)), { writer: 'fsu7hd1k', seq: 6 });
    assert.strictEqual(isReplay(answerOf(FRESH_WRITE)), false);
  });

  /*
   * plugin-r3: an answer that names two cursors names no cursor.
   *
   * This reader asks for `cursor` and then for `event`, and it used to
   * move on from each when the decoder said `undefined` -- which it gave
   * for "there is no such clause" and for "there are two of them" alike.
   * Measured in a seventeenth review round:
   * `(ok (cursor ("a" . 1)) (cursor ("b" . 2)) (event ("wrong" . 99))
   * (replay #f))` answered `{writer: "wrong", seq: 99}`, a cursor the
   * answer never gave, and a Saver settles a save `confirmed` on it.
   */
  it('refuses an answer that names two cursors, rather than falling through to the event', () => {
    assert.strictEqual(
      eventFromWrite(
        answerOf('(ok (cursor ("a" . 1)) (cursor ("b" . 2)) (event ("wrong" . 99)) (replay #f))')
      ),
      null,
      'the cursor came from the event clause, because two cursors cancelled each other out'
    );
    /*
     * THE TWIN, TWICE: `event` is still read when `cursor` is genuinely
     * absent -- the two are spellings of one thing -- and two `event`
     * clauses refuse in their turn.
     */
    assert.deepStrictEqual(eventFromWrite(answerOf(REPLAYED_WRITE)), { writer: 'fsu7hd1k', seq: 6 });
    assert.strictEqual(
      eventFromWrite(answerOf('(ok (replay #t) (event ("a" . 1)) (event ("b" . 2)))')),
      null
    );
    /*
     * KEY: AND A CURSOR THAT IS THERE AND WILL NOT DECODE IS NOT AN
     * ABSENT ONE. Measured in an eighteenth review round, one round
     * after the repair above: `(ok (cursor bad) (event ("wrong" . 99)))`
     * answered `{writer: "wrong", seq: 99}`. The decoder said the clause
     * was found; whether its value is an event is this reader's own
     * question, and it answered that one by moving on to the next name.
     */
    assert.strictEqual(
      eventFromWrite(answerOf('(ok (cursor bad) (event ("wrong" . 99)))')),
      null,
      'a cursor this client cannot read let the event clause supply one instead'
    );
  });

  it('reads the event of a write the store had already applied', () => {
    assert.deepStrictEqual(eventFromWrite(answerOf(REPLAYED_WRITE)), { writer: 'fsu7hd1k', seq: 6 });
    assert.strictEqual(isReplay(answerOf(REPLAYED_WRITE)), true);
  });

  /*
   * KEY: THE REFUSAL HAS TO CARRY A CURSOR, or this cell is about
   * nothing.
   *
   * It used `(error req-mismatch ("fsu7hd1k" . 6))`, which holds no
   * `cursor` and no `event` clause at all -- so removing the exit-code
   * guard from both readers left it green. Measured in a twelfth review
   * round. A failure that carries exactly what a success carries is the
   * only input that can tell the guard is there.
   */
  it('takes nothing from an answer the core called a failure', () => {
    /*
     * KEY: `replay #t`, NOT `#f`. With `#f` the reader answers false
     * whether or not the guard is there, so removing it from `isReplay`
     * alone left this green -- measured in a thirteenth review round.
     * The value asserted has to be the one the guard changes.
     */
    const refusedButShaped = '(ok (cursor ("fsu7hd1k" . 6)) (replay #t))';
    assert.strictEqual(eventFromWrite(answerOf(refusedButShaped, 1)), null);
    assert.strictEqual(isReplay(answerOf(refusedButShaped, 1)), false);
    /*
     * THE TWIN: the same bytes with exit zero ARE read. Without it the
     * cell above would pass on a reader that takes nothing from
     * anything.
     */
    assert.deepStrictEqual(eventFromWrite(answerOf(refusedButShaped, 0)), {
      writer: 'fsu7hd1k',
      seq: 6
    });
    assert.strictEqual(isReplay(answerOf(refusedButShaped, 0)), true);
  });

  it('spells a cursor the way the core parses one', () => {
    const event = eventFromWrite(answerOf(FRESH_WRITE));
    assert.ok(event !== null);
    /*
     * THROUGH THE PRODUCTION SPELLING, not a copy of it written here. A
     * broken formatter would leave an assertion that composes the string
     * itself perfectly green.
     */
    assert.strictEqual(formatCursor(event as NonNullable<typeof event>), 'fsu7hd1k:6');
    assert.ok(isWellFormedCursor('fsu7hd1k:6'));
    assert.ok(!isWellFormedCursor('fsu7hd1k'));
    assert.ok(!isWellFormedCursor('fsu7hd1k:'));
    assert.ok(!isWellFormedCursor('fsu7hd1k:x'));
  });
});

describe('Q3 the first cursor, before any answer has been heard', () => {
  before(async () => {
    await initWire();
  });

  it('reads the writer and the sequence it ends at out of check', () => {
    assert.deepStrictEqual(writersFromCheck(wire().read(CHECK_ONE_WRITER)), [
      { writer: 'fsu7hd1k', end: 7 }
    ]);
  });

  it('uses the one writer a local store has', () => {
    assert.deepStrictEqual(firstCursorFromCheck(wire().read(CHECK_ONE_WRITER)), {
      ok: true,
      cursor: 'fsu7hd1k:7'
    });
  });

  it('refuses to guess which of several writers is this machine\'s', () => {
    const first = firstCursorFromCheck(wire().read(CHECK_TWO_WRITERS));
    assert.strictEqual(first.ok, false);
    assert.strictEqual((first as { reason: string }).reason, 'many-writers');
  });

  it('refuses a store that reports no writer at all', () => {
    const first = firstCursorFromCheck(wire().read('(check (store "s") (writers ()) (verdict ok))'));
    assert.strictEqual(first.ok, false);
    assert.strictEqual((first as { reason: string }).reason, 'no-writer');
  });});

/*
 * plugin-r3 item 2: the core's F45 names the local writer.
 *
 * The shape, quoted from the main session (2026-09-19): `(check (store "...")
 * (local-writer "xihfjyym") (writers (("xihfjyym" ...))) ...)` -- a top-level
 * clause beside `writers`, position not promised, taken by name; absent
 * altogether when there is none. Measured present in 877f0da
 * (store.sc:4828-4829).
 */
describe('plugin-r3 2 the local writer, when the core names it', () => {
  before(async () => {
    await initWire();
  });

  const withLocal = (clause: string, listing: string = CHECK_TWO_WRITERS): string =>
    listing.replace('(check (store "tasul7cl") ', `(check (store "tasul7cl") ${clause} `);

  it('writes as the writer the core names, however many writers the store has', () => {
    assert.deepStrictEqual(firstCursorFromCheck(wire().read(withLocal('(local-writer "other123")'))), {
      ok: true,
      cursor: 'other123:2'
    });
    assert.deepStrictEqual(
      firstCursorFromCheck(wire().read(withLocal('(local-writer "fsu7hd1k")', CHECK_ONE_WRITER))),
      { ok: true, cursor: 'fsu7hd1k:7' }
    );
  });

  /*
   * THE NAMED ONE, NOT A POSITION IN THE LISTING. Review r1 of this item
   * measured that both fixtures above put the named writer last, so taking
   * the last writer passed them. Here it is first, then in the middle.
   */
  it('writes as the named writer wherever it stands in the listing', () => {
    const three =
      '(check (store "tasul7cl") (writers (("aaaa" (end 1) (torn #f) (integrity ())) ' +
      '("bbbb" (end 2) (torn #f) (integrity ())) ("cccc" (end 3) (torn #f) (integrity ())))) ' +
      '(snapshots ()) (registry outside-store) (verdict ok))';
    for (const [named, cursor] of [['aaaa', 'aaaa:1'], ['bbbb', 'bbbb:2']]) {
      assert.deepStrictEqual(
        firstCursorFromCheck(wire().read(withLocal(`(local-writer "${named}")`, three))),
        { ok: true, cursor },
        `named ${named}`
      );
    }
  });

  it('reads the clause wherever it stands', () => {
    const after = CHECK_TWO_WRITERS.replace('(snapshots ())', '(snapshots ()) (local-writer "other123")');
    assert.deepStrictEqual(firstCursorFromCheck(wire().read(after)), { ok: true, cursor: 'other123:2' });
    const last = CHECK_TWO_WRITERS.replace('(verdict ok))', '(verdict ok) (local-writer "fsu7hd1k"))');
    assert.deepStrictEqual(firstCursorFromCheck(wire().read(last)), { ok: true, cursor: 'fsu7hd1k:7' }, 'the clause standing last');
  });

  /*
   * KEY: THE ANSWER CONTRADICTING ITSELF IS REFUSED, NOT RESOLVED. Neither
   * half is picked -- not the named writer, not the only writer listed.
   */
  it('refuses an answer whose local writer is not among its writers, and names it', () => {
    const none = '(check (store "tasul7cl") (writers ()) (snapshots ()) (registry outside-store) (verdict ok))';
    for (const listing of [CHECK_ONE_WRITER, CHECK_TWO_WRITERS, none]) {
      const first = firstCursorFromCheck(wire().read(withLocal('(local-writer "stranger")', listing)));
      assert.strictEqual(first.ok, false, 'a contradicting answer supplied a cursor');
      assert.strictEqual((first as { reason: string }).reason, 'local-writer-not-listed');
      assert.strictEqual((first as { local?: string }).local, 'stranger');
    }
  });

  it('refuses a local-writer clause it cannot read, rather than falling back', () => {
    for (const clause of [
      '(local-writer "fsu7hd1k") (local-writer "other123")',
      '(local-writer fsu7hd1k)',
      '(local-writer)',
      '(local-writer "fsu7hd1k" "other123")',
      '(local-writer #f)'
    ]) {
      const first = firstCursorFromCheck(wire().read(withLocal(clause, CHECK_ONE_WRITER)));
      assert.strictEqual(first.ok, false, `${clause} supplied a cursor`);
      assert.strictEqual((first as { reason: string }).reason, 'unreadable', `${clause}`);
      assert.strictEqual((first as { unreadable?: string }).unreadable, 'local-writer', `${clause} blamed the listing`);
    }
  });

  /*
   * NOTE: AND WITHOUT THE CLAUSE, AS BEFORE (ruled (b), 2026-09-25). A core
   * older than F45 never sends it and a received copy on 877f0da does not
   * either; the two cannot be told apart from the answer. The older rule
   * stands: one writer is used, several are refused. Where that is wrong --
   * a copy whose only writer is not this machine's -- the core refuses the
   * write itself with `(error refused (instance machine))`, measured by the
   * main session on 877f0da, and item 7 parks that save with its way out.
   */
  it('keeps the older rule when the core does not name one', () => {
    assert.deepStrictEqual(firstCursorFromCheck(wire().read(CHECK_ONE_WRITER)), { ok: true, cursor: 'fsu7hd1k:7' });
    const many = firstCursorFromCheck(wire().read(CHECK_TWO_WRITERS));
    assert.strictEqual((many as { reason: string }).reason, 'many-writers');
    const three = CHECK_TWO_WRITERS.replace(
      '("other123" (end 2) (torn #f) (integrity ()))',
      '("other123" (end 2) (torn #f) (integrity ())) ("third456" (end 5) (torn #f) (integrity ()))'
    );
    assert.strictEqual((firstCursorFromCheck(wire().read(three)) as { reason: string }).reason, 'many-writers', 'three writers');
  });
});
