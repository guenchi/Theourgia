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
  FORCE_CLAIM_CONFIRMATION,
  adoptedNotice,
  behindNotice,
  discardedNotice,
  forceClaimNotice,
  prefixRefusedNotice,
  retryNotice,
  saveNotice,
  statusLine,
  undecidableSessionNotice,
  wrongStoreNotice
} from '../../src/status';
import { TransportError } from '../../src/transport';
import { Client } from '../../src/client';
import { Says } from '../support/says';
import { emptyLedger } from '../../src/sessions';
import {
  assertRuledRefusal,
  ESCAPED_ECHO,
  RULED_EXAMPLE
} from '../support/refusal-shape';
import { Datum, answerOf, asInteger, initWire, parseAnswers, wire } from '../../src/wire';

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

/*
 * NOTE: AND EVERY OTHER SENTENCE THAT COUNTS SOMETHING, AT ONE.
 *
 * A review read the eight takeover sentences at a count of one and found
 * all eight ungrammatical. The repair was applied to those eight -- and
 * `conflict(s)`, `save(s)`, `unsent request(s)` and `other window(s)`
 * went on standing in four other places, because the fix had been made
 * where the finding pointed rather than everywhere the shape was. These
 * cells exist so that the sweep has a floor: a sentence that counts
 * something is read here at one AND at more than one, and `(s)` cannot
 * come back without going red.
 */
describe('every sentence that counts something says it in English at one', () => {
  const facts = {
    store: '/tmp/s',
    actor: 'someone',
    cursor: 'w:7',
    conflicts: 0,
    pending: 0,
    blocked: null, unreachable: null
  };

  it('counts one conflict and one unresolved save without a parenthesis', () => {
    const one = statusLine({ ...facts, conflicts: 1, pending: 1 }).tooltip;
    assert.ok(one.includes('1 conflict reported by the store'), one);
    assert.ok(one.includes('1 save whose outcome is unknown'), one);
    const many = statusLine({ ...facts, conflicts: 2, pending: 3 }).tooltip;
    assert.ok(many.includes('2 conflicts reported by the store'), many);
    assert.ok(many.includes('3 saves whose outcome is unknown'), many);
    for (const text of [one, many]) {
      assert.ok(!/\(s\)/.test(text), `a parenthesised plural is back: ${text}`);
    }
  });

  /*
   * AND THE FORCED-TAKEOVER SENTENCE, whose subject is counted twice --
   * once as what is at stake and once as what reaches the store twice.
   * The second is the one a word-level repair would leave behind.
   */
  it('says one request is at stake, and that THAT request goes twice', () => {
    const one = forceClaimNotice('S-norecord', 1).text;
    assert.ok(one.includes('It holds 1 unsent request.'), one);
    assert.ok(one.includes('that request reaches the store twice'), one);
    const many = forceClaimNotice('S-norecord', 4).text;
    assert.ok(many.includes('It holds 4 unsent requests.'), many);
    assert.ok(many.includes('each of those requests reaches the store twice'), many);
    for (const text of [one, many]) {
      assert.ok(!/\(s\)/.test(text), `a parenthesised plural is back: ${text}`);
    }
  });

  it('says one other window HAS documents open, not have', () => {
    const one = discardedNotice('S-dead', '/tmp/trash', ['W-1']).text;
    assert.ok(one.endsWith(' 1 other window still has documents open in it.'), one);
    const many = discardedNotice('S-dead', '/tmp/trash', ['W-1', 'W-2']).text;
    assert.ok(many.endsWith(' 2 other windows still have documents open in it.'), many);
    const none = discardedNotice('S-dead', '/tmp/trash', []).text;
    assert.ok(!/window/.test(none), `a window nobody has open was mentioned: ${none}`);
    for (const text of [one, many, none]) {
      assert.ok(!/\(s\)/.test(text), `a parenthesised plural is back: ${text}`);
    }
  });
});

describe('a queue nobody could read is not a queue with nothing in it', () => {
  const facts = {
    store: '/tmp/s',
    actor: 'someone',
    cursor: 'w:7',
    conflicts: 0,
    blocked: null, unreachable: null
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
    blocked: null, unreachable: null
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
    assert.ok(notice.text.includes('the block changed'), notice.text);
  });

  /*
   * NOTE: AND A REFUSAL SAYS WHICH BLOCK, AND WHAT BECAME OF THE TEXT.
   *
   * It used to be the core's sentence and nothing else. Two things were
   * missing and both matter to somebody with several blocks open: which
   * one this is about, and that what they typed is still in the file and
   * still unsent. The second became urgent when a review found that a
   * refusal was being recorded as an acknowledgement -- the block then
   * stopped being counted as unsent, and this sentence was the only
   * trace of the edit left anywhere.
   */
  it('names the block and says the text is still there, unsent', () => {
    const notice = saveNotice(
      { ...saved, id: 'a.7', status: 'refused', message: 'the core refused the write: req-mismatch' },
      false
    );
    assert.strictEqual(
      notice.text,
      'a.7: the core refused the write: req-mismatch. Your text is still in the file and has ' +
        'not been saved to the store.',
      `the refusal does not say all three things: ${notice.text}`
    );
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
    assertRuledRefusal(RULED_EXAMPLE, '1');
  });

  /*
   * AND IT IS ABOUT THE DATUM, NOT ABOUT ITS SPACING. The first version
   * of this check matched substrings, so the same answer written with an
   * extra space failed -- which would have kept the red cells red after
   * the core landed the change, for a reason that has nothing to do with
   * the core.
   */
  it('accepts the same datum written with different spacing', () => {
    assertRuledRefusal('(error   malformed-intent\n'.replace('\n', ' ') +
        '(symbol-not-wire-safe  (where  relation)  (spelling  "1")))', '1');
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
      '(error malformed-intent (symbol-not-wire-safe (where relation) (spelling "\\x31;")))',
      '1'
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
      () => assertRuledRefusal(ESCAPED_ECHO, '1'),
      'the shape check accepts the answer it exists to reject'
    );
  });

  it('refuses a refusal of a different error family', () => {
    assert.throws(() =>
      assertRuledRefusal('(error unrelated-error (symbol-not-wire-safe (where relation) (spelling "1")))', '1')
    );
  });

  /*
   * AND THE POSITION AND THE SPELLING HAVE TO BE INSIDE THE REASON.
   * Beside it, they belong to the error and the reason is a bare tag --
   * a different answer, which independent substring matches accepted.
   */
  it('refuses a refusal whose position and spelling sit outside the reason', () => {
    assert.throws(() =>
      assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe) (where relation) (spelling "1"))', '1')
    );
  });

  it('refuses a spelling that is not a string', () => {
    assert.throws(
      () =>
        assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (where relation) (spelling 1)))', '1'),
      'a number was accepted where the ruling asks for a string'
    );
    assert.throws(
      () =>
        assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (where relation) (spelling |1|)))', '1'),
      'a symbol was accepted where the ruling asks for a string'
    );
  });

  it('refuses a refusal that names a different spelling', () => {
    assert.throws(() =>
      assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (where relation) (spelling "2")))', '1')
    );
  });

  it('refuses a refusal that does not say which position was wrong', () => {
    assert.throws(() =>
      assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (spelling "1")))', '1')
    );
  });

  it('refuses a refusal that names a different position', () => {
    assert.throws(() =>
      assertRuledRefusal('(error malformed-intent (symbol-not-wire-safe (where verb) (spelling "1")))', '1')
    );
  });
});

/*
 * U-claim: WHAT THE USER IS TOLD BEFORE A FORCED TAKEOVER.
 *
 * The takeover exists because refusing for ever strands a queue. It is
 * only offerable because its cost is bounded, and it is only safe to
 * offer if the sentence says what that cost is. Three things have to be
 * in it: that the other window may still be running, that each request
 * then reaches the store twice, and that the store settles the second by
 * request id rather than doing the work again.
 *
 * NOTE: ALL THREE OR NONE. Without the third it reads like data loss and
 * nobody presses it; without the second it hides that there is a cost.
 * The sentence is built where a cell can read it for exactly that
 * reason.
 */
describe('U-claim the sentence a forced takeover carries', () => {
  it('says the window may be running, that requests go twice, and that the store settles them', () => {
    const text = forceClaimNotice('S-norecord', 3).text;
    assert.match(text, /S-norecord/, 'the sentence does not say which window');
    assert.match(text, /3 unsent request/, 'the sentence does not say what is at stake');
    assert.match(text, /still running/, 'the sentence does not say the window may be alive');
    assert.match(text, /twice/, 'the sentence does not say the request is sent again');
    /*
     * NOTE: BOTH HALVES OF THE REASSURANCE, separately. A build that kept
     * the words "already applied" and dropped the explanation -- that
     * the store recognises the request by its id, and does not do the
     * work again -- passed a single check for either. What makes the
     * cost acceptable is the mechanism, not the phrase.
     */
    assert.match(
      text,
      /request id/i,
      'the sentence does not say HOW the store settles the second one'
    );
    assert.match(
      text,
      /rather than doing the work again|not.{0,20}again/i,
      'the sentence does not say the work is not repeated'
    );
  });

  it('asks for a word the user has to choose, not a yes', () => {
    assert.ok(FORCE_CLAIM_CONFIRMATION.length > 0);
    assert.ok(
      /take/i.test(FORCE_CLAIM_CONFIRMATION),
      `the confirmation does not name the action: ${FORCE_CLAIM_CONFIRMATION}`
    );
  });

  /*
   * AND THE THREE UNDECIDABLE REASONS GET THREE DIFFERENT SENTENCES.
   * They were one value in the model until this batch, and they are not
   * the same news: one of them fixes itself and two of them do not.
   */
  /*
   * NOTE: THE SAME SESSION ID IN ALL THREE. With three different ids the
   * three sentences differ whatever they say, so a build that gave the
   * unreadable case the start-time wording passed -- the ids alone made
   * the strings distinct. Found in review.
   */
  it('tells the user which kind of "cannot tell" this is', () => {
    const waiting = undecidableSessionNotice('S-a', 'start-time-unavailable').text;
    const unobtainable = undecidableSessionNotice('S-a', 'liveness-unobtainable').text;
    const unrecorded = undecidableSessionNotice('S-a', 'start-time-unrecorded').text;
    const missing = undecidableSessionNotice('S-a', 'record-missing').text;
    const unreadable = undecidableSessionNotice('S-a', 'record-unreadable').text;
    assert.strictEqual(
      new Set([waiting, unobtainable, unrecorded, missing, unreadable]).size,
      5,
      'two of the five say the same thing, so the user cannot tell them apart'
    );
    /*
     * NOTE: AND ONLY THE ONES THAT CAN FIX THEMSELVES SAY SO. A record with
     * no start time in it will never acquire one, and it used to be told
     * to try again in a moment.
     */
    assert.ok(
      !/try again|moment/i.test(unrecorded),
      'a record that will never carry a start time was described as something that fixes itself'
    );
    assert.match(unobtainable, /try again|moment/i, 'a failed reading does not say it can be retaken');
    assert.match(waiting, /try again|moment/i, 'the one that fixes itself does not say so');
    assert.match(missing, /no amount of waiting|explicitly/i, 'the permanent one reads as temporary');
    assert.ok(
      !/explicitly/.test(waiting),
      'a takeover was offered for the reading that fixes itself'
    );
    assert.ok(
      !/explicitly/.test(unreadable),
      'a takeover was offered for a record nobody has managed to read'
    );
    assert.ok(
      !/try again|moment/i.test(unreadable),
      'a record that will not parse was described as something that fixes itself'
    );
  });
});

/*
 * U8, THE WHOLE FAMILY: EVERY ERROR ANSWER CAN BE READ BACK.
 *
 * The ruling is not about one message. The data of ANY error answer is
 * wire-safe by construction, because a refusal describes the intent
 * rather than echoing it. `symbol-not-wire-safe` was the one that
 * necessarily broke -- what it refuses is by definition what the writer
 * cannot spell -- but `unknown-verb` had the same defect from the same
 * cause, and it was found by the core session applying the rule rather
 * than by anyone reporting a symptom.
 *
 * NOTE: THIS CELL IS THE CONSUMER SIDE OF THAT CONTRACT, and it is
 * deliberately degenerate with the core's own: the core checks what it
 * writes, this checks that what arrives can be read by the reader this
 * client actually uses. One of them can be wrong without the other
 * being wrong, which is the whole value of having both.
 */
describe('U8 every error answer the core can produce is readable by this client', () => {
  before(async () => {
    await initWire();
  });

  /*
   * THE ANSWERS ARE WRITTEN OUT RATHER THAN PRODUCED. A cell that asked
   * a real core would only cover the refusals that core happens to
   * produce today; these are the shapes the rule is about, including
   * the two that were broken and the argument position that has not
   * been needed yet.
   */
  const ANSWERS: Array<[string, string]> = [
    [
      'a relation name the wire cannot carry',
      '(error malformed-intent (symbol-not-wire-safe (where relation) (spelling "has part")))'
    ],
    [
      'a verb nobody knows, whose name needs escaping',
      '(error unknown-verb (spelling "show me") (verbs init insert set))'
    ],
    [
      'an argument position',
      '(error malformed-intent (symbol-not-wire-safe (where argument) (spelling "1")))'
    ],
    ['a refusal with no detail at all', '(error unknown)'],
    [
      'a refusal carrying a string with an escape in it',
      '(error malformed-intent (symbol-not-wire-safe (where relation) (spelling "a\\x31;b")))'
    ]
  ];

  for (const [what, answer] of ANSWERS) {
    it(`reads back the refusal of ${what}`, () => {
      const read = parseAnswers(`${answer}\n`);
      assert.strictEqual(read.length, 1, `${answer} did not read back as one datum`);
      assert.notStrictEqual(answerOf(read[0], 'error'), null, `${answer} did not read back as an error`);
    });
  }

  /*
   * AND THE NEGATIVE WITNESS: the shape the rule forbids -- a refusal
   * that ECHOES the offending symbol instead of describing it -- is
   * exactly what this client cannot read. Without it the cells above
   * pass against a reader that accepts everything, and would say nothing
   * about the contract.
   */
  it('cannot read a refusal that echoes the symbol instead of describing it', () => {
    assert.throws(
      () => parseAnswers('(error malformed-intent (symbol-not-wire-safe (link "a.1" \\x31; "a.2")))\n'),
      'the echo this whole rule is about was read without complaint'
    );
    assert.throws(
      () => parseAnswers('(error unknown-verb show\\x20;me (verbs init))\n'),
      'the unknown-verb echo was read without complaint'
    );
  });
});

/*
 * KEY: THE OTHER HALF OF THE CONSERVATION LAW: EVERY BUCKET THE LEDGER
 * CARRIES REACHES THE USER.
 *
 * `sessions.test.ts` asserts that everything a takeover saw is in
 * exactly one bucket. That guards the count. It says nothing about the
 * report, and a bucket the report never mentions is work that vanished
 * between the count and the person -- the same defect one step later.
 *
 * NOTE: THE BUCKETS ARE ENUMERATED FROM THE LEDGER ITSELF, not listed here.
 * A list written here would be a second place to forget the new bucket,
 * which is precisely the thing being guarded against.
 */
describe('every bucket a takeover counts is something the user is told', () => {
  /*
   * NOTE: THE WHOLE SENTENCE, NOT A FRAGMENT OF IT.
   *
   * These cells used to look for a phrase and, separately, for the
   * count. An outside judge showed what that lets through: "was not
   * confirmed as handed over BECAUSE MARKING FAILED" passed, because the
   * forbidden words were absent while the claim they forbade was back;
   * so did "0 could not be moved (observed 5)", and so did dropping the
   * "not" out of "does not do the work twice". A fragment is not a
   * proposition, and every one of those changed what the user is told.
   *
   * So each bucket's sentence is written out here in full and compared
   * exactly.
   *
   * NOTE: WHAT THAT BUYS, EXACTLY. The copy here was written by whoever
   * wrote the product's, so it is not an independent statement of what
   * the message ought to say and does not make a wrong sentence go red.
   * What it does is stop a sentence changing WITHOUT ANYONE LOOKING:
   * every edit to the message shows up as a diff here, beside the old
   * wording, for a human to accept or refuse. It is a checkpoint in the
   * review, not a guarantee about the meaning -- and saying otherwise
   * would be this file making the same kind of claim it exists to catch.
   * Found in review.
   */
  /*
   * NOTE: TWO FINISHED SENTENCES PER BUCKET, ONE FOR EACH NUMBER.
   *
   * A single template with `${n}` in it cannot be wrong about agreement,
   * because it says the same thing at every count -- which is how "1
   * unsent request(s) are now in this window's queue" and seven more
   * like it went unremarked here while the cells compared whole
   * sentences. Writing the singular out separately is what makes it
   * possible to be wrong, and therefore what makes the cell able to say
   * anything. Found in review, all eight at once.
   */
  const SENTENCES: Record<string, (n: number) => string> = {
    imported: (n) =>
      n === 1
        ? "1 unsent request is now in this window's queue."
        : `${n} unsent requests are now in this window's queue.`,
    skippedDuplicate: (n) =>
      n === 1
        ? '1 had already been carried across and was left alone.'
        : `${n} had already been carried across and were left alone.`,
    leftOtherStore: (n) =>
      n === 1
        ? '1 was written for another store and is still there; configure that store and run ' +
          'this command again to bring it across.'
        : `${n} were written for other stores and are still there; configure those stores and ` +
          'run this command again to bring them across.',
    leftUnknownStore: (n) =>
      n === 1
        ? '1 is in a queue from an older version of this extension, which does not record which ' +
          'store it was written for; this command will not move it into a store it cannot show ' +
          'it belongs to.'
        : `${n} are in a queue from an older version of this extension, which does not record ` +
          'which store they were written for; this command will not move them into a store it ' +
          'cannot show they belong to.',
    unreadableQueue: (n) =>
      n === 1
        ? 'a usable queue could not be loaded from 1 of its paths, and nothing was taken from it.'
        : `a usable queue could not be loaded from ${n} of its paths, and nothing was taken ` +
          'from them.',
    failedToMove: (n) =>
      n === 1
        ? '1 could not be moved into this window and is still in that window’s queue; nothing ' +
          'was lost, and running this command again will try it.'
        : `${n} could not be moved into this window and are still in that window’s queue; ` +
          'nothing was lost, and running this command again will try them.',
    movedButUnmarked: (n) =>
      n === 1
        ? '1 arrived here, and the other window’s copy was not confirmed as handed over, so a ' +
          'later takeover may carry it again. The store recognises a request it has already ' +
          'applied by its id and does not do the work twice.'
        : `${n} arrived here, and the other window’s copy was not confirmed as handed over, so ` +
          'a later takeover may carry them again. The store recognises a request it has already ' +
          'applied by its id and does not do the work twice.',
    outcomeUnknown: (n) =>
      n === 1
        ? '1 could not be accounted for: this window could not find out whether it arrived. ' +
          'Look at both queues before deciding anything about it.'
        : `${n} could not be accounted for: this window could not find out whether they ` +
          'arrived. Look at both queues before deciding anything about them.'
  };

  /*
   * THE ADVICE IS PART OF THE REPORT, so it is written out here too, and
   * against the buckets it is allowed to follow. It names its subject by
   * PLACE: it said "They", and then "The ones that arrived", and both
   * could be read as the requests the sentence before them is about --
   * the ones nobody could account for, or the ones carried across on an
   * earlier run. What is in this window's queue is not ambiguous.
   *
   * NOTE: AND IT PROMISES AN ATTEMPT, NOT AN OUTCOME. Naming the place let
   * the verb overstate: a save stops at the first request it cannot
   * settle, so "everything ... goes out with the next save" was false
   * for any window that already holds an unresolved one. The behaviour
   * that sentence contradicted has cells of its own in `saver.test.ts`;
   * this is the report agreeing with them.
   */
  const ADVICE =
    ' Run "theourgia: Retry Pending Saves" to try this window\'s queue now. The next save tries ' +
    'it too, but only after reaching the store, so a store that cannot be reached leaves the ' +
    'queue untouched.';
  const ADVICE_AFTER: Record<string, string> = { imported: ADVICE, movedButUnmarked: ADVICE };

  /*
   * A BUCKET WITH NO SENTENCE WRITTEN DOWN FAILS THIS FILE rather than
   * being skipped by it -- otherwise adding a bucket silently removes it
   * from every check below.
   */
  it('has a sentence written down for every bucket the ledger carries', () => {
    /*
     * NOTE: `durabilityWarnings` IS LEFT OUT BY NAME. (queue item 22, K8) It is
     * not a bucket -- outside the conservation law -- and its sentences are
     * held by their own cell.
     */
    const buckets = Object.keys(emptyLedger()).filter(
      (name) => name !== 'observed' && name !== 'durabilityWarnings'
    );
    const missing = buckets.filter((name) => SENTENCES[name] === undefined);
    assert.deepStrictEqual(missing, [], 'these buckets have no expected sentence');
    const extra = Object.keys(SENTENCES).filter((name) => !buckets.includes(name));
    assert.deepStrictEqual(extra, [], 'these sentences are for buckets that no longer exist');
  });

  it('says exactly that sentence when a bucket is the only thing that happened', () => {
    for (const [name, sentence] of Object.entries(SENTENCES)) {
      const ledger = emptyLedger();
      (ledger as unknown as Record<string, number>)[name] = 5;
      ledger.observed = 5;
      const text = adoptedNotice('S-dead', ledger, false).text;
      assert.strictEqual(
        text,
        `S-dead: ${sentence(5)}${ADVICE_AFTER[name] ?? ''}`,
        `the sentence for ${name} is not what it should be`
      );
    }
  });

  /*
   * NOTE: AND AGAIN AT ONE, WHICH IS WHERE THE THRESHOLDS ARE.
   *
   * Every sentence above is gated on `bucket > 0`, and the cell that
   * reads them used 5 for all of them. An outside judge changed one gate
   * to `> 1` -- a single failed request, the smallest thing that can go
   * wrong, stops being mentioned -- and all six cells in this section
   * passed. Nothing here distinguished "more than none" from "more than
   * one", so nothing here was testing the threshold at all.
   *
   * One is also the count that makes English disagree, so the two
   * readings this file was blindest to are the same reading.
   */
  it('says exactly that sentence when a bucket holds a single request', () => {
    for (const [name, sentence] of Object.entries(SENTENCES)) {
      const ledger = emptyLedger();
      (ledger as unknown as Record<string, number>)[name] = 1;
      ledger.observed = 1;
      const text = adoptedNotice('S-dead', ledger, false).text;
      assert.strictEqual(
        text,
        `S-dead: ${sentence(1)}${ADVICE_AFTER[name] ?? ''}`,
        `the sentence for ${name} at a count of one is not what it should be`
      );
    }
  });

  /*
   * NOTE: AND THE ADVICE ONLY WHEN SOMETHING IS HERE TO SEND. It was
   * unconditional, so a report about another store's requests ended by
   * telling the user they would go out with the next save -- of a queue
   * that does not hold them.
   */
  /*
   * NOTE: AND THIS CHECK LOOKED FOR A PHRASE THE PRODUCT NO LONGER SAYS.
   * It searched for "go out with the next save" -- wording that two
   * rounds of review have since replaced -- so it passed for any advice
   * whatsoever, including an advice sentence that should not have been
   * there at all. It compares against the one string the rest of this
   * file compares against now, which cannot go stale without the exact
   * comparisons going red in the same run. Found in review.
   */
  it('does not offer to send what is not in this window', () => {
    for (const name of ['leftOtherStore', 'leftUnknownStore', 'unreadableQueue', 'outcomeUnknown']) {
      const ledger = emptyLedger();
      (ledger as unknown as Record<string, number>)[name] = 2;
      ledger.observed = 2;
      const text = adoptedNotice('S-dead', ledger, false).text;
      assert.ok(
        !text.includes(ADVICE),
        `${name} was offered for sending from a queue that does not hold it: ${text}`
      );
      assert.ok(
        !/Retry Pending Saves/.test(text),
        `${name} was told to retry a queue that does not hold it: ${text}`
      );
    }
  });

  /*
   * NOTE: THE TWO REPORTS WHERE THE ADVICE HAD SOMETHING TO BE MISREAD AS.
   *
   * A review named these: `{imported: 1, outcomeUnknown: 1}` puts the
   * advice directly after the sentence about requests nobody could
   * account for, and `{imported: 1, skippedDuplicate: 1}` puts it
   * directly after the one about requests carried across on an earlier
   * run. Both are reachable, both are one request each, and in both the
   * previous wording offered to send something this window had just said
   * it does not have or did not take. They are written out whole here so
   * that a wording which reintroduces a pronoun cannot pass.
   */
  it('ends with advice that cannot be read as being about the sentence before it', () => {
    const withUnknown = emptyLedger();
    withUnknown.imported = 1;
    withUnknown.outcomeUnknown = 1;
    withUnknown.observed = 2;
    assert.strictEqual(
      adoptedNotice('S-dead', withUnknown, false).text,
      `S-dead: ${SENTENCES.imported(1)} ${SENTENCES.outcomeUnknown(1)}${ADVICE}`,
      'the report after an unaccounted-for request is not what it should be'
    );

    const withDuplicate = emptyLedger();
    withDuplicate.imported = 1;
    withDuplicate.skippedDuplicate = 1;
    withDuplicate.observed = 2;
    assert.strictEqual(
      adoptedNotice('S-dead', withDuplicate, false).text,
      `S-dead: ${SENTENCES.imported(1)} ${SENTENCES.skippedDuplicate(1)}${ADVICE}`,
      'the report after an already-carried request is not what it should be'
    );
  });

  it('says every sentence, and only those, when several buckets carry something', () => {
    const ledger = emptyLedger();
    ledger.imported = 2;
    ledger.leftOtherStore = 3;
    ledger.unreadableQueue = 1;
    ledger.observed = 6;
    const text = adoptedNotice('S-dead', ledger, false).text;
    assert.strictEqual(
      text,
      `S-dead: ${SENTENCES.imported(2)} ${SENTENCES.leftOtherStore(3)} ` +
        `${SENTENCES.unreadableQueue(1)}${ADVICE}`,
      'the report is not exactly the sentences for the buckets that carry something'
    );
  });

  /*
   * AND A TAKEOVER THAT FOUND NOTHING SAYS SO. Every bucket empty is an
   * answer, not an empty sentence with a window's name in front of it.
   */
  it('says so when there was nothing in that window to move', () => {
    const text = adoptedNotice('S-dead', emptyLedger(), false).text;
    assert.strictEqual(text, 'S-dead was taken over and there was nothing in it to move.');
  });

  it('says nothing about a bucket that is empty', () => {
    const ledger = emptyLedger();
    ledger.imported = 3;
    ledger.observed = 3;
    const text = adoptedNotice('S-dead', ledger, false).text;
    for (const [name, sentence] of Object.entries(SENTENCES)) {
      if (name === 'imported') {
        continue;
      }
      assert.ok(
        !text.includes(sentence(0)) && !text.includes(sentence(3)),
        `${name} is empty and was recited anyway: ${text}`
      );
    }
  });
});

/*
 * plugin-r2 T2: WHAT LANDED WHILE THIS SAVE WAS BEING PREPARED.
 * (design 7.5.22 v161/v167)
 *
 * A successful commit can carry `(behind ((<writer> . <seq>) ...))`: the
 * writers whose records reached the store after this save's draft took
 * its baseline. The core is explicit about what it is and is not --
 * informational, never a refusal, never an action; a commit that
 * carries it succeeded. It names OTHER writers only, and a baseline
 * that is not behind carries no clause at all.
 *
 * NOTE: AND A CLAUSE THE CORE COULD NOT BUILD IS SIMPLY ABSENT. The core
 * answers a successful commit with no `behind` rather than an error
 * when it cannot compute one, so a reader that threw on a shape it did
 * not expect would turn a success into a failure at the one moment the
 * user is watching.
 */
describe('plugin-r2 T2 a commit says who landed after its baseline', () => {
  before(async () => {
    await initWire();
  });

  const answerOf = (text: string): Datum => parseAnswers(`${text}\n`)[0];

  /*
   * KEY: THE WRITER AND THE COUNT ARE ASSERTED TOGETHER, in the words
   * they appear in.
   *
   * Measured in a thirteenth review round: with `/other/` and `/3/`
   * checked separately, replacing every `${one.writer}` with a constant
   * left this passing -- the notice named the wrong writer and the
   * number was still somewhere in the sentence. A notice about who
   * landed records that does not name who is a notice about nothing.
   */
  it('names one writer and counts its records', () => {
    const notice = String(
      behindNotice(answerOf('(ok (events (("w" . 6))) (behind (("other" . 3))))'))
    );
    assert.match(notice, /other has 3 records/, notice);
  });

  /*
   * NOTE: ONE RECORD GETS ITS OWN SENTENCE. A template that reads "3
   * records" at three and "1 records" at one is a template that cannot
   * be wrong about the number and is wrong about the language, and this
   * tree has been bitten by exactly that shape.
   */
  it('writes a whole sentence at one record, not a template with a 1 in it', () => {
    const notice = String(behindNotice(answerOf('(ok (events (("w" . 6))) (behind (("other" . 1))))')));
    /*
     * KEY: AND IT STILL NAMES THE WRITER AND THE COUNT. Checking only
     * that "record" appears and "records" does not let the whole
     * rendering be replaced by a constant phrase -- measured in a
     * thirteenth review round with `a record landed`, which satisfies
     * both.
     */
    assert.match(notice, /other has 1 record\b/, notice);
    assert.doesNotMatch(notice, /\brecords\b/, 'the singular reads as a template with a 1 in it');
  });

  /*
   * KEY: EACH WRITER KEEPS ITS OWN COUNT. Measured in a thirteenth review
   * round: rendering every writer with the FIRST one's number showed
   * alice and bob as 2 records each, and a cell that asked only for the
   * two names passed.
   */
  it('names every writer when more than one landed, each with its own count', () => {
    const notice = String(
      behindNotice(answerOf('(ok (events (("w" . 6))) (behind (("alice" . 2) ("bob" . 5))))'))
    );
    assert.match(notice, /alice has 2 records/, notice);
    assert.match(notice, /bob has 5 records/, notice);
  });

  /*
   * THE TWIN. A commit whose baseline was fresh carries no clause, and
   * the user is told nothing -- a notice on every save would be noise
   * with no action behind it.
   */
  it('says nothing when the answer carries no such clause', () => {
    assert.strictEqual(behindNotice(answerOf('(ok (events (("w" . 6))) (replay #f))')), null);
  });

  it('says nothing about a clause it cannot read, rather than failing the save', () => {
    assert.strictEqual(behindNotice(answerOf('(ok (events (("w" . 6))) (behind))')), null);
    assert.strictEqual(behindNotice(answerOf('(ok (events (("w" . 6))) (behind "nonsense"))')), null);
    assert.strictEqual(behindNotice(answerOf('(ok (events (("w" . 6))) (behind (("other" . "x"))))')), null);
  });
});
