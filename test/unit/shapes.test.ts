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
import { Datum, asInteger, initWire, wire } from '../../src/wire';

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
     * reader makes of every integer. Writing this cell is what showed
     * it: the count-only assertion it replaces was compatible with the
     * field holding anything at all, including the number 2 that this
     * one is not.
     */
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
