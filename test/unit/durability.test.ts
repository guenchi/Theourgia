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
 * What happens to the queue when the disk says no.
 *
 * ALL OF THESE CAME OUT OF A REVIEW, and each names a way the queue
 * could disagree with what is on disk. The pattern in every one is the
 * same: a step changed memory and then failed to change the file, and
 * everything afterwards acted on a state nothing had recorded.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { MAX_TIMEOUT_MS, problemsWith } from '../../src/config';
import { Outbox, OutboxEntry, OutboxWriteError } from '../../src/outbox';

function scratch(name: string): string {
  return path.join(fs.mkdtempSync(path.join(os.tmpdir(), `theourgia-${name}-`)), 'outbox.json');
}

function entry(req: string, payload: string): OutboxEntry {
  return {
    req,
    cursor: 'w:7',
    id: 'a.2',
    field: 'src',
    payload,
    state: 'queued',
    createdAt: 0,
    lastError: null
  };
}

describe('a change that did not reach the disk did not happen', () => {
  it('does not hold an entry in memory that it could not write down', () => {
    /*
     * The queue is read while its directory is real -- a missing file is
     * an empty queue -- and only then is the directory replaced by a
     * file, so that the WRITE is what fails and not the read. Breaking
     * it before the read would test the other rule.
     */
    const file = scratch('unwritable');
    const outbox = new Outbox(file);
    outbox.load();
    const directory = path.dirname(file);
    fs.rmSync(directory, { recursive: true, force: true });
    fs.writeFileSync(directory, 'this is a file, not a directory\n', 'utf8');

    assert.throws(() => outbox.enqueue(entry('11111111-1111-1111-1111-111111111111', 'one\n')), OutboxWriteError);
    assert.strictEqual(
      outbox.pendingCount,
      0,
      'the entry stayed in memory after the write failed, so a later retry would send a request nothing recorded'
    );
  });

  it('does not forget an entry it could not record as resolved', () => {
    const file = scratch('resolve');
    const outbox = new Outbox(file);
    outbox.load();
    outbox.enqueue(entry('22222222-2222-2222-2222-222222222222', 'one\n'));
    assert.strictEqual(outbox.pendingCount, 1);

    /*
     * The directory is replaced by a file, so the next write cannot
     * succeed. This is the shape of a disk that filled up or a
     * permission that changed between two saves.
     */
    fs.rmSync(path.dirname(file), { recursive: true, force: true });
    fs.writeFileSync(path.dirname(file), 'in the way\n', 'utf8');

    assert.throws(() => outbox.resolve('22222222-2222-2222-2222-222222222222', 'w:9'), OutboxWriteError);
    assert.strictEqual(
      outbox.pendingCount,
      1,
      'the entry was dropped from memory although the removal was never recorded'
    );
    assert.strictEqual(outbox.cursor, null, 'the cursor moved on a write that failed');
  });

  it('does not keep a cursor it could not record', () => {
    const file = scratch('cursor');
    const outbox = new Outbox(file);
    outbox.load();
    fs.rmSync(path.dirname(file), { recursive: true, force: true });
    fs.writeFileSync(path.dirname(file), 'in the way\n', 'utf8');
    assert.throws(() => outbox.setCursor('w:9'), OutboxWriteError);
    assert.strictEqual(outbox.cursor, null);
  });
});

describe('a queue that could not be read is not an empty queue', () => {
  it('reads a file that is not there as an empty queue', () => {
    const outbox = new Outbox(scratch('missing'));
    outbox.load();
    assert.strictEqual(outbox.pendingCount, 0);
    assert.strictEqual(outbox.cursor, null);
  });

  it('refuses to carry on when the file exists and could not be read', () => {
    const file = scratch('unreadable');
    fs.writeFileSync(file, '{"version":1,"cursor":"w:7","entries":[]}\n', 'utf8');
    /*
     * A directory where the file was: the read fails with something
     * other than "not there", which is every reason that leaves the
     * question of what was recorded open.
     */
    fs.rmSync(file);
    fs.mkdirSync(file);
    const outbox = new Outbox(file);
    assert.throws(() => outbox.load(), OutboxWriteError);
  });

  it('will not write over a file it could not read', () => {
    const file = scratch('no-clobber');
    fs.writeFileSync(file, 'this is not json\n', 'utf8');
    const outbox = new Outbox(file);
    assert.throws(() => outbox.load(), OutboxWriteError);
    assert.throws(
      () => outbox.setCursor('w:9'),
      OutboxWriteError,
      'the unread file was about to be replaced with an empty queue'
    );
    assert.strictEqual(
      fs.readFileSync(file, 'utf8'),
      'this is not json\n',
      'the file that could not be read was overwritten anyway'
    );
  });

  it('will not write to a queue nobody has read', () => {
    const outbox = new Outbox(scratch('unloaded'));
    assert.throws(() => outbox.enqueue(entry('33333333-3333-3333-3333-333333333333', 'x\n')), OutboxWriteError);
  });
});

describe('a timeout the timer cannot honour is refused where it is read', () => {
  const base = {
    scheme: 'scheme',
    corePath: '/core',
    libDirs: [],
    store: '/store',
    actor: 'someone',
    transport: 'cli' as const
  };

  it('accepts the largest delay the timer takes', () => {
    assert.deepStrictEqual(problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS }), []);
  });

  it('refuses one larger, which the timer would turn into a millisecond', () => {
    const problems = problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS + 1 });
    assert.strictEqual(problems.length, 1);
    assert.strictEqual(problems[0].setting, 'theourgia.timeoutMs');
    assert.match(problems[0].message, /millisecond/);
  });

  it('still refuses a timeout of zero or less', () => {
    assert.strictEqual(problemsWith({ ...base, timeoutMs: 0 }).length, 1);
    assert.strictEqual(problemsWith({ ...base, timeoutMs: -1 }).length, 1);
  });
});

describe('a queue in a shape this build cannot read is refused, not repaired', () => {
  /*
   * SILENTLY DROPPING THE UNREADABLE PART WAS THE WORSE FAILURE. A file
   * holding one entry without a cursor came back as a queue with that
   * entry missing, and the next save wrote the repaired queue over the
   * file -- so work nobody could read became work nobody had. Each of
   * these leaves the file alone instead.
   */
  const bad: { name: string; text: string; says: RegExp }[] = [
    { name: 'a queue that is not an object', text: 'null\n', says: /not an object/ },
    { name: 'a queue that is a list', text: '[]\n', says: /not an object/ },
    {
      name: 'entries that are not a list',
      text: '{"version":1,"cursor":null,"entries":{}}\n',
      says: /not a list/
    },
    {
      name: 'an entry with no cursor',
      text: '{"version":1,"cursor":"w:7","entries":[{"req":"r","id":"a.2","field":"src","payload":"x"}]}\n',
      says: /no readable cursor/
    },
    {
      name: 'an entry whose payload is not text',
      text:
        '{"version":1,"cursor":"w:7","entries":[{"req":"r","cursor":"w:7","id":"a.2","field":"src","payload":42}]}\n',
      says: /no readable payload/
    },
    {
      name: 'an entry in a state this build does not know',
      text:
        '{"version":1,"cursor":"w:7","entries":[{"req":"r","cursor":"w:7","id":"a.2","field":"src","payload":"x","state":"halfway"}]}\n',
      says: /state this build does not know/
    },
    {
      name: 'a queue from a later version',
      text: '{"version":2,"cursor":"w:7","entries":[]}\n',
      says: /version 2/
    },
    {
      /*
       * IT BELONGS IN THE MATRIX, not beside it. As its own cell this
       * shape was asked only whether `load` refused; the matrix also
       * asks whether the refusal stops the file being written over,
       * which is the half that matters and the half a build could fail
       * while still throwing.
       */
      name: 'a queue with no entries list at all',
      text: '{"cursor":"w:7"}\n',
      says: /no entries list/
    }
  ];

  for (const row of bad) {
    it(`refuses ${row.name}, and leaves the file alone`, () => {
      const file = scratch('malformed');
      fs.writeFileSync(file, row.text, 'utf8');
      const outbox = new Outbox(file);
      assert.throws(() => outbox.load(), (e: unknown) => e instanceof OutboxWriteError && row.says.test((e as Error).message));
      assert.throws(
        () => outbox.setCursor('w:9'),
        OutboxWriteError,
        'a queue that could not be read was about to be written over'
      );
      assert.strictEqual(fs.readFileSync(file, 'utf8'), row.text, 'the unreadable file was changed');
    });
  }

  /*
   * A REFUSAL THAT LEAVES NO WAY FORWARD IS NOT FINISHED. Once the queue
   * is unreadable nothing will be written to it, so saving stops until a
   * person does something -- and the message is the only place they will
   * learn what. It names the file, says why it is being left alone, and
   * says that moving it aside resumes saving and what that costs.
   */
  function refusalFor(file: string): string {
    const outbox = new Outbox(file);
    let caught: unknown = null;
    try {
      outbox.load();
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof OutboxWriteError, `load did not refuse ${file}`);
    return (caught as Error).message;
  }

  it('tells the user what they can do about a queue whose bytes it cannot read', () => {
    const file = scratch('stuck');
    fs.writeFileSync(file, 'this is not json\n', 'utf8');
    const said = refusalFor(file);
    assert.ok(said.includes(file), 'the message does not name the file');
    assert.match(said, /no further save will be sent/, 'the message does not say saving has stopped');
    assert.match(said, /move it aside/, 'the message does not say what can be done');
    /*
     * AND THAT THE STORE IS NOT WHAT IS BROKEN. An earlier version of
     * this cell checked that line and the rewrite dropped it, which left
     * the sentence a user needs most -- their blocks are fine -- covered
     * by nothing.
     */
    assert.match(
      said,
      /does not touch the store/,
      'the message does not say the store itself is unaffected'
    );
  });

  /*
   * WHAT SETTING THE QUEUE ASIDE ACTUALLY COSTS. The message used to say
   * that "what is lost is only the record of which requests were still
   * unresolved", which is not what the file holds: entries carry whole
   * payloads, and one written down and never sent is work that exists
   * nowhere else. A user told the loss was bookkeeping would move the
   * file and lose saves. The sentence has to say the work will not be
   * retried, and it must not describe the loss as only a record.
   */
  it('does not describe setting the queue aside as losing only a record', () => {
    const file = scratch('stuck-cost');
    fs.writeFileSync(file, 'this is not json\n', 'utf8');
    const said = refusalFor(file);
    assert.match(said, /never sent/, 'the message does not say the queue holds unsent work');
    assert.match(
      said,
      /will not be retried/,
      'the message does not say the recorded work stops being retried'
    );
    assert.ok(
      !/only the record/.test(said),
      'the message still calls the loss a record, which is what made it safe to discard'
    );
  });

  /*
   * ADVICE THAT WOULD NOT HAVE WORKED. A queue that could not be REACHED
   * -- a permission, a device error -- is not repaired by moving the
   * file, and a user who cannot read the file may not be able to move it
   * either. One sentence served both faults, so in this one it named a
   * remedy for a different problem.
   */
  it('does not answer an unreachable queue with the advice for an unreadable one', () => {
    /*
     * A DIRECTORY IN THE QUEUE'S PLACE reaches the same branch as a
     * permission or a device error -- the read fails with something that
     * is not ENOENT -- and it does so without depending on which user is
     * running the suite, which a chmod would.
     */
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-unreachable-')), 'outbox.json');
    fs.mkdirSync(file);
    const said = refusalFor(file);
    assert.match(said, /readable again/, 'the message does not say what would repair this fault');
    assert.ok(
      !/move it aside/.test(said),
      'an unreachable queue was answered with the remedy for an unreadable one'
    );
  });

  it('says the same for a shape it cannot read, not only for bad JSON', () => {
    const file = scratch('stuck2');
    fs.writeFileSync(file, '{"version":1,"cursor":"w:7","entries":[{"req":"r"}]}\n', 'utf8');
    const said = refusalFor(file);
    assert.match(said, /shape this build cannot read/);
    assert.match(said, /move it aside/);
  });

  it('still reads a queue this build wrote', () => {
    const file = scratch('roundtrip');
    const first = new Outbox(file);
    first.load();
    first.setCursor('w:7');
    first.enqueue(entry('11111111-1111-1111-1111-111111111111', 'one\n'));

    const second = new Outbox(file);
    second.load();
    assert.strictEqual(second.cursor, 'w:7');
    assert.deepStrictEqual(
      second.entries.map((e) => [e.req, e.payload, e.state]),
      [['11111111-1111-1111-1111-111111111111', 'one\n', 'queued']]
    );
  });

  /*
   * THE LEGACY CASE HAS TO CARRY WORK IN IT. A cell that loads an EMPTY
   * versionless queue is satisfied by a build that accepts empty ones
   * and drops or refuses populated ones -- which is the migration this
   * change could actually break.
   */
  it('reads a populated queue with no version field, keeping every entry', () => {
    const file = scratch('noversion');
    fs.writeFileSync(
      file,
      JSON.stringify({
        cursor: 'w:7',
        entries: [
          { req: 'aaaaaaaa-1111-1111-1111-111111111111', cursor: 'w:3', id: 'a.2', field: 'src', payload: 'one\n', state: 'pending' },
          /*
           * A SECOND FIELD NAME, so that a reader which hardcodes the
           * one this batch happens to write is caught. Both entries
           * saying 'src' let such a reader reproduce the fixture.
           */
          { req: 'bbbbbbbb-2222-2222-2222-222222222222', cursor: 'w:5', id: 'a.9', field: 'title', payload: 'two\n' }
        ]
      }),
      'utf8'
    );
    const outbox = new Outbox(file);
    outbox.load();
    assert.strictEqual(outbox.cursor, 'w:7');
    assert.deepStrictEqual(
      /*
       * THE FIELD IS PART OF THE ENTRY. Leaving it out of the comparison
       * let a reader that replaced every legacy entry's field pass a
       * cell whose name promises the entries survived -- and the field
       * is what the retry writes.
       */
      outbox.entries.map((e) => [e.req, e.cursor, e.id, e.field, e.payload, e.state]),
      [
        ['aaaaaaaa-1111-1111-1111-111111111111', 'w:3', 'a.2', 'src', 'one\n', 'pending'],
        ['bbbbbbbb-2222-2222-2222-222222222222', 'w:5', 'a.9', 'title', 'two\n', 'queued']
      ],
      'a queue an earlier build wrote did not survive the stricter reading'
    );
  });

  /*
   * THE FLAG HAS TO BE CLEARED, not merely never set. Every malformed
   * cell above starts from a fresh Outbox that was never readable, so
   * none of them would notice a build that kept writing after a
   * successful load was followed by a failed one.
   */
  /*
   * AND FOR EVERY WAY A LOAD CAN FAIL, not only for bad JSON. `load`
   * clears the flag in three separate catches -- the read, the parse and
   * the shape -- and a cell that exercises one of them accepts a build
   * that dropped the other two. The three faults are the three rows.
   */
  const wentBad: Array<{ name: string; spoil: (file: string) => string }> = [
    {
      name: 'bytes that are not JSON',
      spoil: (file) => {
        fs.writeFileSync(file, 'this is not json\n', 'utf8');
        return 'this is not json\n';
      }
    },
    {
      name: 'a shape this build cannot read',
      spoil: (file) => {
        const text = '{"version":1,"cursor":"w:7","entries":[{"req":"r"}]}\n';
        fs.writeFileSync(file, text, 'utf8');
        return text;
      }
    },
    {
      name: 'a file it can no longer reach',
      spoil: (file) => {
        fs.rmSync(file);
        fs.mkdirSync(file);
        return '';
      }
    }
  ];

  for (const row of wentBad) {
    it(`stops writing after a queue it had read becomes ${row.name}`, () => {
      const file = scratch('wentbad');
      const outbox = new Outbox(file);
      outbox.load();
      outbox.setCursor('w:7');
      assert.strictEqual(outbox.pendingCount, 0);

      const after = row.spoil(file);
      assert.throws(() => outbox.load(), OutboxWriteError);
      assert.strictEqual(
        outbox.pendingCount,
        null,
        'the count of an unreadable queue was reported as a number'
      );
      assert.throws(
        () => outbox.setCursor('w:9'),
        OutboxWriteError,
        'a queue that had gone bad was still being written to'
      );
      if (after !== '') {
        assert.strictEqual(fs.readFileSync(file, 'utf8'), after, 'the unreadable file was changed');
      }
    });
  }
});
