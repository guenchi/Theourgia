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
 * An answer in a shape this client has not met is refused, not trimmed.
 *
 * ALL THREE OF THESE USED TO SKIP. An outline line that did not match
 * was dropped, a field entry that did not match was dropped, and a
 * conflict count that could not be fetched was drawn as zero. Each of
 * those turns "the core said something I do not understand" into a
 * smaller store, a block with fewer fields, or a clean bill of health --
 * three ways of reporting good news that nobody checked.
 */

import * as assert from 'assert';
import { documentFor, fieldConflict, readBlock } from '../../src/blocks';
import { parseOutline } from '../../src/outline';
import {
  prefixRefusedNotice,
  retryNotice,
  saveNotice,
  statusLine,
  wrongStoreNotice
} from '../../src/status';
import { TransportError } from '../../src/transport';
import { Client } from '../../src/client';
import { Says } from '../support/says';
import {
  assertRuledRefusal,
  ESCAPED_ECHO,
  RULED_EXAMPLE
} from '../support/refusal-shape';
import { Datum, asInteger, initWire, parseAnswers, wire } from '../../src/wire';

describe('an outline line this client cannot read stops the outline', () => {
  before(async () => {
    await initWire();
  });

  it('refuses rather than showing a store with one block missing', () => {
    let caught: unknown = null;
    try {
      parseOutline('- a.1  One\nwhat is this line\n- a.2  Two\n');
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof TransportError, `threw ${(caught as Error)?.name}`);
    assert.strictEqual((caught as TransportError).failure, 'unreadable');
    assert.match((caught as Error).message, /what is this line/);
  });

  it('still reads the two lines the core does produce beside rows', () => {
    const rows = parseOutline('- a.1  Doc\norphans:\n- b.2  Kid\n');
    assert.deepStrictEqual(rows.map((r) => r.id), ['a.1', 'b.2']);
  });

  it('reads a row whose title would look like a stray line', () => {
    assert.deepStrictEqual(parseOutline('- a.1  orphans:\n').map((r) => r.id), ['a.1']);
  });

  /*
   * A CONTINUATION CAN CARRY TWO SPACES IN IT, and then it looks like a
   * row with its dash missing. Only the dash tells a row from a line of
   * prose, so that is what this pins: without it, `Design notes  here`
   * is read as a block called `Design` and the tree gains a row nobody
   * wrote.
   */
  it('refuses a line that has a row\'s shape but no dash', () => {
    assert.throws(
      () => parseOutline('- a.1  first line\nDesign notes  here\n'),
      (e: unknown) => e instanceof TransportError && e.failure === 'unreadable'
    );
  });

  it('refuses an indented line with no dash', () => {
    assert.throws(
      () => parseOutline('- a.1  Doc\n  b.2  Kid\n'),
      (e: unknown) => e instanceof TransportError && e.failure === 'unreadable'
    );
  });

  it('refuses a row whose id would be empty', () => {
    assert.throws(
      () => parseOutline('-   two spaces and no id\n'),
      (e: unknown) => e instanceof TransportError && e.failure === 'unreadable'
    );
  });

  it('refuses an outline whose row runs onto a second line, and says which line', () => {
    let caught: unknown = null;
    try {
      parseOutline('- a.1  first line\nsecond line\n');
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof TransportError);
    assert.match((caught as Error).message, /line 2/, 'the refusal does not say which line');
    assert.match((caught as Error).message, /second line/, 'the refusal does not quote the line');
    assert.match((caught as Error).message, /a\.1/, 'the refusal does not name the row to go and look at');
  });
});

describe('a field this client cannot read stops the block', () => {
  before(async () => {
    await initWire();
  });

  it('refuses rather than returning a block with the field missing', () => {
    const value = wire().read('((id . "a.2") (deleted . #f) (fields (title . "Two") "loose") (position root . 0) (edges))');
    let caught: unknown = null;
    try {
      readBlock(value);
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof TransportError, `threw ${(caught as Error)?.name}`);
    assert.strictEqual((caught as TransportError).failure, 'unreadable');
    assert.match((caught as Error).message, /a\.2/);
  });

  it('reads a block with no fields at all as a block with no fields', () => {
    const value = wire().read('((id . "a.2") (deleted . #f) (fields) (position root . 0) (edges))');
    const block = readBlock(value);
    assert.ok(block !== null);
    assert.strictEqual(block?.fields.size, 0);
  });

  it('still reads every field shape the core does produce', () => {
    const value = wire().read(
      '((id . "a.2") (deleted . #f) (fields (heading-src . "## Two\\n") (level . 2) (title conflict ("One" "Two"))) (position "a.1" . 0) (edges))'
    );
    const block = readBlock(value);
    assert.ok(block !== null);
    assert.strictEqual(block?.fields.size, 3);
    /*
     * AND WHAT THE THREE HOLD. Counting them accepts a reader that kept
     * every name and replaced every value -- the numeric one with zero,
     * the conflicted one with a string -- which is the whole of what
     * reading a field shape means.
     */
    assert.strictEqual(block?.fields.get('heading-src'), '## Two\n');
    /*
     * AND IT COMES BACK AS A BigInt, which is what the authority's
     * reader makes of every integer. Both halves are asserted: the
     * VALUE through `asInteger`, and the REPRESENTATION directly --
     * `asInteger` answers 2 for the number 2 and for the BigInt 2n
     * alike, so on its own it says nothing about which arrived, and the
     * first version of this cell claimed it did.
     */
    assert.strictEqual(
      typeof block?.fields.get('level'),
      'bigint',
      'the reader no longer returns integers as BigInt; asInteger callers need re-checking'
    );
    assert.strictEqual(
      asInteger(block?.fields.get('level') as Datum),
      2,
      'the numeric field did not come back as its value'
    );
    const conflict = fieldConflict(block?.fields.get('title') as Datum);
    assert.ok(conflict !== null, 'the conflicted field was not read as a conflict');
    assert.deepStrictEqual(conflict?.candidates, ['One', 'Two']);
  });
});

describe('a queue nobody could read is not a queue with nothing in it', () => {
  const facts = {
    store: '/tmp/s',
    actor: 'someone',
    cursor: 'w:7',
    conflicts: 0,
    blocked: null
  };

  /*
   * THE MOST REASSURING SENTENCE AT THE LEAST APPROPRIATE MOMENT. A
   * corrupt queue stops every save; reporting its count as zero put
   * "nothing waiting to be saved" on the screen of a user whose saves
   * had just stopped going out.
   */
  it('says the queue could not be read rather than that nothing is waiting', () => {
    const line = statusLine({ ...facts, pending: null });
    assert.match(line.tooltip, /could not be read/);
    assert.ok(!/nothing waiting to be saved/.test(line.tooltip), 'an unreadable queue read as empty');
    assert.match(line.text, /unreadable/);
    assert.ok(line.warning, 'a stopped queue was drawn as though nothing were wrong');
  });

  it('says so differently from a queue that is genuinely empty', () => {
    const unknown = statusLine({ ...facts, pending: null });
    const empty = statusLine({ ...facts, pending: 0 });
    assert.notStrictEqual(unknown.text, empty.text);
    assert.match(empty.tooltip, /nothing waiting to be saved/);
    assert.strictEqual(empty.warning, false);
  });

  it('still counts the entries when there are some', () => {
    const line = statusLine({ ...facts, pending: 3 });
    assert.match(line.text, /3/);
    assert.ok(line.warning);
  });
});

describe('a conflict count nobody could fetch is not a count of zero', () => {
  const facts = {
    store: '/tmp/s',
    actor: 'someone',
    cursor: 'w:7',
    pending: 0,
    blocked: null
  };

  it('says the question could not be put', () => {
    const line = statusLine({ ...facts, conflicts: null });
    assert.match(line.tooltip, /conflicts: unknown/);
    assert.ok(line.warning, 'not knowing was drawn as though it were fine');
  });

  it('says so differently from a store that has none', () => {
    const unknown = statusLine({ ...facts, conflicts: null });
    const none = statusLine({ ...facts, conflicts: 0 });
    assert.notStrictEqual(unknown.text, none.text);
    assert.notStrictEqual(unknown.tooltip, none.tooltip);
    assert.strictEqual(none.warning, false);
  });

  it('counts the conflicts when there are some', () => {
    const line = statusLine({ ...facts, conflicts: 3 });
    assert.match(line.text, /3/);
    assert.ok(line.warning);
  });
});

describe('S8 what the user is told after a save', () => {
  const saved = { status: 'saved', id: 'a.2', message: 'saved' };

  it('says nothing when the bytes went out as they were typed', () => {
    assert.strictEqual(saveNotice(saved, false).level, 'none');
  });

  it('says so when the line endings were changed on the way out', () => {
    const notice = saveNotice(saved, true);
    assert.strictEqual(notice.level, 'information');
    assert.match(notice.text, /a\.2/);
    assert.match(notice.text, /LF/);
    assert.match(notice.text, /CRLF/);
  });

  it('reports a refusal as an error and an unresolved save as a warning', () => {
    assert.strictEqual(saveNotice({ ...saved, status: 'refused', message: 'no' }, false).level, 'error');
    assert.strictEqual(saveNotice({ ...saved, status: 'blocked', message: 'no' }, false).level, 'error');
    assert.strictEqual(saveNotice({ ...saved, status: 'pending', message: 'wait' }, false).level, 'warning');
  });

  it('does not let a normalisation notice hide a refusal', () => {
    const notice = saveNotice({ ...saved, status: 'refused', message: 'the block changed' }, true);
    assert.strictEqual(notice.level, 'error');
    assert.strictEqual(notice.text, 'the block changed');
  });

  it('says both facts when an edit to the protected prefix is refused', () => {
    const notice = prefixRefusedNotice('a.2', false, true);
    assert.strictEqual(notice.level, 'warning');
    assert.match(notice.text, /nothing.*was sent/);
    assert.match(notice.text, /still holds what you wrote/);
  });

  /*
   * THE SENTENCE NAMES WHAT THE USER ACTUALLY EDITED. A document block
   * has front matter and no heading; telling its editor that "the
   * heading line changed" points at something the block does not have.
   */
  it('names the heading for a block that has one', () => {
    const notice = prefixRefusedNotice('a.2', false, true);
    assert.match(notice.text, /heading line/);
    assert.ok(!/front matter/.test(notice.text));
  });

  it('names the front matter for a document, which has no heading', () => {
    const notice = prefixRefusedNotice('a.1', true, false);
    assert.match(notice.text, /front matter/);
    assert.ok(!/heading line/.test(notice.text), 'a block with no heading was told its heading changed');
  });

  it('names both when a block carries both', () => {
    const notice = prefixRefusedNotice('a.2', true, true);
    assert.match(notice.text, /front matter/);
    assert.match(notice.text, /heading line/);
  });

  it('says something usable for a block that has neither', () => {
    const notice = prefixRefusedNotice('a.3', false, false);
    assert.match(notice.text, /text before the body/);
  });
});

describe('a buffer carries the store it was opened from', () => {
  before(async () => {
    await initWire();
  });

  it('remembers which store a block came out of', () => {
    const value = wire().read(
      '((id . "a.2") (deleted . #f) (fields (src . "body")) (position "a.1" . 0) (edges))'
    );
    const block = readBlock(value);
    assert.ok(block !== null);
    const document = documentFor(block as NonNullable<typeof block>, '/stores/one');
    assert.strictEqual(document.store, '/stores/one');
  });

  it('says nothing was sent when the settings now name another store', () => {
    const notice = wrongStoreNotice('a.2', '/stores/one', '/stores/two');
    assert.strictEqual(notice.level, 'error');
    assert.match(notice.text, /\/stores\/one/);
    assert.match(notice.text, /\/stores\/two/);
    assert.match(notice.text, /nothing was sent/);
  });
});

/*
 * WHAT A RETRY SAYS WHEN IT CANNOT COUNT WHAT IS LEFT.
 *
 * THE WORD "null" REACHED THE USER. The sentence was built at the call
 * to the editor by interpolating `saver.pendingCount`, whose type is
 * `number | null`, so a queue this build could not read produced
 * "1 of 1 resolved; null still waiting." -- a sentence that reads like a
 * count and names a value no user has a word for. It is the same fault
 * the status bar already had, arriving through the one path that had
 * kept composing its own text.
 */
describe('a retry reports a count that may not exist', () => {
  it('does not put the word null in front of the user', () => {
    const notice = retryNotice('/tmp/s', 1, 1, null);
    assert.ok(!/null/.test(notice.text), `the absent count was rendered: ${notice.text}`);
  });

  it('says the number is not known rather than reporting one', () => {
    const notice = retryNotice('/tmp/s', 1, 1, null);
    assert.match(notice.text, /not known/, 'an unreadable queue was not reported as unknown');
    assert.strictEqual(notice.level, 'warning', 'a stopped queue was reported as information');
  });

  it('says that differently from a queue with nothing left in it', () => {
    const unknown = retryNotice('/tmp/s', 1, 1, null);
    const done = retryNotice('/tmp/s', 1, 1, 0);
    assert.notStrictEqual(unknown.text, done.text);
    assert.match(done.text, /0 still waiting/);
    assert.strictEqual(done.level, 'information');
  });

  it('still reports the count when there is one', () => {
    const notice = retryNotice('/tmp/s', 2, 5, 3);
    assert.match(notice.text, /2 of 5 resolved/);
    assert.match(notice.text, /3 still waiting/);
  });

  /*
   * THE COUNT AND THE OUTCOMES MUST BE ABOUT ONE STORE. A sentence with
   * no store in it cannot be caught pairing one store's outcomes with
   * another store's queue, which is exactly what the command did when
   * the settings changed while a retry was in flight.
   */
  it('names the store the numbers belong to', () => {
    assert.match(retryNotice('/stores/one', 1, 1, 0).text, /\/stores\/one/);
  });
});

/*
 * A SYMBOL NAME THIS READER WILL NOT DECODE, AND WHOSE BLOCK IS NAMED.
 *
 * goeteia 1.7.2 refuses a symbol whose escaped name is numeric --
 * `\x31;`, the symbol called `1`. The golden table records that name as
 * readable, which is what the older reader did.
 *
 * THE STORE AT THIS PIN DOES NOT REFUSE IT, AND THAT WAS MEASURED. The
 * first version of this comment said the core's wire-safety predicate
 * kept such names out of stores, which is wrong: when the predicate
 * answers false `storable-encode` does not refuse, it stores the
 * wrapped form `("#%sym" "1")`, and `read` then prints the name back as
 * `\x31;`. An ordinary `link a 1 b` is enough to make one. So this is
 * not a shape only an old or foreign store could hold.
 *
 * What must not happen is the answer coming back as a block with a
 * field quietly missing -- so the refusal has to name the block to go
 * and look at. The real-core cell that builds this state through `link`
 * is in real-core.test.ts; this one pins the same behaviour at the
 * client, on bytes, without paying for a store.
 *
 * THE WRITE PATH IS BEING CHANGED to refuse these names at intent time,
 * which is a decision taken after this was measured. When that lands,
 * the real-core cell becomes one that asks the core to make the name
 * and checks that it will not; this cell stays as it is, because a
 * store written before that change can still hold one.
 */
describe('a symbol name this reader refuses stops the answer and names the block', () => {
  before(async () => {
    await initWire();
  });

  it('reports a transport error naming the block rather than a block with a field missing', async () => {
    const says = new Says(
      '(ok ((id . "a.2") (deleted . #f) (fields (rel . \\x31;)) (position root . 0) (edges)))\n'
    );
    const client = new Client(says);
    let caught: unknown = null;
    try {
      await client.request('read', ['a.2']);
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof TransportError, `threw ${(caught as Error)?.name}`);
    assert.strictEqual((caught as TransportError).failure, 'unreadable');
    assert.match(
      (caught as Error).message,
      /a\.2/,
      'the refusal does not name the block whose answer could not be read'
    );
  });

  /*
   * AND THE ORDINARY ESCAPE STILL ARRIVES. Without this the cell above
   * is satisfied by a client that refuses every escaped symbol name,
   * which would turn `--store` -- a name the core really does escape --
   * into an unreadable answer.
   */
  it('still reads an escaped symbol name the reader does accept', async () => {
    const says = new Says('(error unknown-verb \\x2D;-store (verbs init))\n');
    /*
     * The exit code is the verdict in this protocol, and the stand-in
     * reports zero, so this answer is `ok` whatever datum it carries.
     * What is being asked is only that the escaped name was READ.
     */
    const answer = await new Client(says).request('read', ['a.2']);
    /*
     * THE DECODED NAME, NOT THE RAW LINE. `text` is the bytes the core
     * printed and still holds the escape; the decoding this cell is
     * about is only visible in the datum.
     */
    const datum = answer.answers[0] as { name: string }[];
    assert.strictEqual(datum[2].name, '--store', 'the escaped name did not decode');
  });
});

/*
 * U8: THE WITNESS FOR A RED CELL'S EXPECTATION.
 *
 * Two cells in real-core.test.ts are red on purpose: they assert the
 * shape the main session ruled for a refusal, and the core has not
 * landed it yet. A red cell's expectation is checked by NOTHING -- a
 * stray bracket or a wrong field name keeps it red for ever, and the day
 * the core changes, the cell goes on failing while still looking like it
 * is waiting. That has happened in this tree before.
 *
 * So the same assertions are run here against the ruling's own example,
 * with no core involved. This cell passing is the only evidence that the
 * red over there is about the core and not about a typo in the pattern.
 */
describe('U8 the shape a refusal must come back in', () => {
  before(async () => {
    await initWire();
  });

  it('accepts the answer the ruling describes, read with the wire reader', () => {
    assertRuledRefusal(RULED_EXAMPLE, '1', 'rel');
  });

  /*
   * AND IT IS ABOUT THE DATUM, NOT ABOUT ITS SPACING. The first version
   * of this check matched substrings, so the same answer written with an
   * extra space failed -- which would have kept the red cells red after
   * the core landed the change, for a reason that has nothing to do with
   * the core.
   */
  it('accepts the same datum written with different spacing', () => {
    assertRuledRefusal(
      '(error   malformed-intent\n'.replace('\n', ' ') +
        '(symbol-not-wire-safe  (field  rel)  (spelling  "1")))',
      '1',
      'rel'
    );
  });

  /*
   * AND A STRING IS A STRING HOWEVER IT WAS SPELLED. `"\x31;"` decodes
   * to "1" and carries the spelling as a string, which is what the
   * ruling asks for. The first version banned every hex escape outright
   * and rejected this -- "wire-safe" is a property of the datum, not a
   * prohibition on escape sequences inside strings.
   */
  it('accepts a spelling written with an escape inside the string', () => {
    assertRuledRefusal(
      '(error malformed-intent (symbol-not-wire-safe (field rel) (spelling "\\x31;")))',
      '1',
      'rel'
    );
  });

  /*
   * THE NEGATIVE WITNESSES. Without them the assertions could be
   * vacuous -- a check everything satisfies would pass the cells above
   * and would make the red cells green against the unchanged core.
   */
  it('refuses the escaped echo the core answers today', () => {
    assert.throws(
      () => parseAnswers(`${ESCAPED_ECHO}\n`),
      'the escape this is all about was read without complaint'
    );
    assert.throws(
      () => assertRuledRefusal(ESCAPED_ECHO, '1', 'rel'),
      'the shape check accepts the answer it exists to reject'
    );
  });

  it('refuses a refusal of a different error family', () => {
    assert.throws(() =>
      assertRuledRefusal(
        '(error unrelated-error (symbol-not-wire-safe (field rel) (spelling "1")))',
        '1',
        'rel'
      )
    );
  });

  /*
   * AND THE POSITION AND THE SPELLING HAVE TO BE INSIDE THE REASON.
   * Beside it, they belong to the error and the reason is a bare tag --
   * a different answer, which independent substring matches accepted.
   */
  it('refuses a refusal whose position and spelling sit outside the reason', () => {
    assert.throws(() =>
      assertRuledRefusal(
        '(error malformed-intent (symbol-not-wire-safe) (field rel) (spelling "1"))',
        '1',
        'rel'
      )
    );
  });

  it('refuses a spelling that is not a string', () => {
    assert.throws(
      () =>
        assertRuledRefusal(
          '(error malformed-intent (symbol-not-wire-safe (field rel) (spelling 1)))',
          '1',
          'rel'
        ),
      'a number was accepted where the ruling asks for a string'
    );
    assert.throws(
      () =>
        assertRuledRefusal(
          '(error malformed-intent (symbol-not-wire-safe (field rel) (spelling |1|)))',
          '1',
          'rel'
        ),
      'a symbol was accepted where the ruling asks for a string'
    );
  });

  it('refuses a refusal that names a different spelling', () => {
    assert.throws(() =>
      assertRuledRefusal(
        '(error malformed-intent (symbol-not-wire-safe (field rel) (spelling "2")))',
        '1',
        'rel'
      )
    );
  });

  it('refuses a refusal that does not say which position was wrong', () => {
    assert.throws(() =>
      assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (spelling "1")))', '1', 'rel')
    );
  });

  it('refuses a refusal that names a different position', () => {
    assert.throws(() =>
      assertRuledRefusal(
        '(error malformed-intent (symbol-not-wire-safe (field title) (spelling "1")))',
        '1',
        'rel'
      )
    );
  });
});
