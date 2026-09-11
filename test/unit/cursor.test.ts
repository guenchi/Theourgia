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
import { initWire, wire } from '../../src/wire';

function answerOf(text: string, rc = 0): Answer {
  return {
    argv: [],
    rc,
    ok: rc === 0,
    kind: 'datum',
    text,
    answers: [wire().read(text)],
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

  it('reads the event of a write the store had already applied', () => {
    assert.deepStrictEqual(eventFromWrite(answerOf(REPLAYED_WRITE)), { writer: 'fsu7hd1k', seq: 6 });
    assert.strictEqual(isReplay(answerOf(REPLAYED_WRITE)), true);
  });

  it('takes nothing from an answer the core called a failure', () => {
    assert.strictEqual(eventFromWrite(answerOf('(error req-mismatch ("fsu7hd1k" . 6))', 1)), null);
  });

  it('spells a cursor the way the core parses one', () => {
    const event = eventFromWrite(answerOf(FRESH_WRITE));
    assert.ok(event !== null);
    assert.strictEqual(`${event?.writer}:${event?.seq}`, 'fsu7hd1k:6');
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
  });
});
