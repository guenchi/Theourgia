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
import { documentFor, readBlock } from '../../src/blocks';
import { parseOutline } from '../../src/outline';
import { headingRefusedNotice, saveNotice, statusLine, wrongStoreNotice } from '../../src/status';
import { TransportError } from '../../src/transport';
import { initWire, wire } from '../../src/wire';

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
    assert.deepStrictEqual(
      rows.map((r) => [r.id, r.orphan]),
      [
        ['a.1', false],
        ['b.2', true]
      ]
    );
  });

  it('reads a row whose title would look like a stray line', () => {
    const rows = parseOutline('- a.1  orphans:\n');
    assert.strictEqual(rows.length, 1);
    assert.strictEqual(rows[0].title, 'orphans:');
  });

  /*
   * THIS PINS A CONSEQUENCE, NOT A PREFERENCE. `set <id> title` accepts
   * a title with a newline in it and `outline` writes the title raw, so
   * a real store can print a row that runs over two lines -- measured,
   * not supposed: a scratch store built by the core prints
   *
   *     - p9wewqpa.1  first line
   *     second line
   *
   * Refusing is the rule this batch was given, and it is the louder of
   * the two wrong answers: the alternative, skipping, drops a block from
   * the tree without a word. Neither is right. Reading an unmatched line
   * as the rest of the previous row's title would be, and is a change to
   * this one function -- it is not made here because the batch's rule
   * says refuse, and a client that quietly did something else would be
   * the second opinion this whole file exists to avoid.
   */
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

  /*
   * TWO CASES THE REFUSAL DOES NOT CATCH, both reproduced against a real
   * store built by the core. They are pinned here as what this client
   * currently does, not as what it should do -- the ruling on multi-line
   * titles is that the root cause is fixed in the core, and these say
   * what happens until it is.
   *
   *   theourgia insert --under root --title "Design notes  conflict"
   *   theourgia insert --under root --title "second line
   *   - fake.1  invented"
   *
   * printed
   *
   *   - 0zfgvajv.1  Design notes  conflict
   *   - 0zfgvajv.2  second line
   *   - fake.1  invented
   *
   * while `conflicts` reported nothing at all.
   */
  it('cannot tell a title ending in the conflict mark from a conflicted row', () => {
    const rows = parseOutline('- a.1  Design notes  conflict\n');
    assert.strictEqual(rows[0].title, 'Design notes');
    assert.strictEqual(
      rows[0].mark,
      'conflict',
      'a block the store reports as sound is shown as being in a structural conflict'
    );
  });

  it('cannot tell a row from the second line of a title that looks like one', () => {
    const rows = parseOutline('- a.2  second line\n- fake.1  invented\n');
    assert.strictEqual(rows.length, 2);
    assert.strictEqual(
      rows[1].id,
      'fake.1',
      'a block that does not exist is shown in the tree, and no refusal is raised'
    );
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

  it('says both facts when a heading edit is refused', () => {
    const notice = headingRefusedNotice('a.2');
    assert.strictEqual(notice.level, 'warning');
    assert.match(notice.text, /nothing.*was sent/);
    assert.match(notice.text, /still holds what you wrote/);
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
