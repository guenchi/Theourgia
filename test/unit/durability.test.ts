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
import { spawnSync } from 'child_process';
import { MAX_TIMEOUT_MS, WITNESS_SOURCE, problemsWith } from '../../src/config';
import { Outbox, OutboxEntry, OutboxWriteError } from '../../src/outbox';
import { FileOps, nodeFileOps } from '../../src/fsops';
import { DurabilitySink } from '../../src/durability';

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
    lastError: null,
    importedBy: null
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

  /*
   * RETIRED: `will not write to a queue nobody has read`.
   *
   * It pinned a FLAG. An object whose `load` had never been called had
   * `readable` false, and every mutator refused to write. section 13 makes
   * every mutator re-read the file before it changes it -- so there is
   * no such object any more: the first change reads for itself.
   *
   * NOTE: THE RULE BECAME A STRUCTURE, which is why the flag could go. The
   * property it protected is asserted directly by its successor:
   * `reads the file before its first change, even if nobody loaded it`
   * (mutators.test.ts) -- an object that never loaded, over a file that
   * already holds entries, and both survive.
   *
   * The sibling that guards the other half is untouched and still here:
   * `will not write over a file it could not read`, above.
   *
   * Retired on the main session's ruling, with that successor named.
   */
});

describe('a timeout the timer cannot honour is refused where it is read', () => {
  const base = {
    scheme: 'scheme',
    corePath: '/core',
    libDirs: [],
    store: '/store',
    actor: 'someone',
    writer: ''
  };

  /*
   * A CORE DIRECTORY THAT IS IN ORDER, so that these cells are about the
   * timeout and nothing else. The second argument stopped being optional
   * when it turned out the extension was calling without it and the
   * refusal about a corePath holding neither form could never fire.
   */
  const SOURCES = { has: (name: string): boolean => name === WITNESS_SOURCE };

  it('accepts the largest delay the timer takes', () => {
    assert.deepStrictEqual(problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS }, SOURCES), []);
  });

  it('refuses one larger, which the timer would turn into a millisecond', () => {
    const problems = problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS + 1 }, SOURCES);
    assert.strictEqual(problems.length, 1);
    assert.strictEqual(problems[0].setting, 'theourgia.timeoutMs');
    assert.match(problems[0].message, /millisecond/);
  });

  it('still refuses a timeout of zero or less', () => {
    assert.strictEqual(problemsWith({ ...base, timeoutMs: 0 }, SOURCES).length, 1);
    assert.strictEqual(problemsWith({ ...base, timeoutMs: -1 }, SOURCES).length, 1);
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

/*
 * U-req: A REQUEST ID CANNOT BE CHANGED UNDER THE THING THAT IS SENDING
 * IT.
 *
 * `entries` copied the array and handed out the same objects. The
 * Saver's drain sends the entry at the front and then asks whether the
 * request it sent is still queued -- and stops if it is, because the
 * answer could not be recorded. A caller that renamed `req` in between
 * would make that question miss, and the drain would go on sending under
 * a changed identity. No ordinary settlement does this; nothing stopped
 * one either, which was a review's point: an invariant nothing enforces
 * is a comment.
 *
 * THE ATTEMPT HAS TO THROW RATHER THAN BE LOST. Handing out copies would
 * also stop the drain from missing, and would swallow the write in
 * silence; a frozen object raises a TypeError in strict mode at the line
 * that made the attempt.
 */
/*
 * NOTE: A CURSOR IS WHAT AN ANSWER TO ONE OF THIS QUEUE'S REQUESTS SAYS.
 *
 * `resolve` filtered by request id and then wrote the cursor whatever
 * the filter had done -- so a caller holding the wrong queue could
 * commit another store's position into this file while its own request
 * stayed unsettled elsewhere. That is exactly what happened: the
 * extension's settler read the live queue rather than its own, and the
 * editor suite showed store A's cursor painted under store B.
 *
 * The caller was repaired. This is the second layer, and it states the
 * rule where the writing happens rather than where the calling does.
 */
describe('U-cur the cursor moves only for a request this queue was holding', () => {
  it('moves the cursor when the answer settles an entry that is here', () => {
    const file = scratch('cursor-mine');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('r1', 'one\n'));
    queue.resolve('r1', 'w:7');
    assert.strictEqual(queue.cursor, 'w:7', 'a settled request did not move the cursor');
    assert.strictEqual(queue.find('r1'), undefined, 'the settled entry is still queued');
  });

  it('leaves the cursor alone when the request was never in this queue', () => {
    const file = scratch('cursor-theirs');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('mine', 'one\n'));
    queue.resolve('mine', 'w:3');
    assert.strictEqual(queue.cursor, 'w:3');

    queue.resolve('from-another-store', 'v:99');
    assert.strictEqual(
      queue.cursor,
      'w:3',
      'a cursor arrived for a request this queue never held, and was written anyway'
    );
  });

  /*
   * AND IT SURVIVES A RELOAD, because the damage the caller did was
   * committed to the file: an in-memory check would pass over a build
   * that wrote the foreign cursor to disk and only kept the old one in
   * this object.
   */
  it('writes no foreign cursor to the file either', () => {
    const file = scratch('cursor-disk');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('mine', 'one\n'));
    queue.resolve('mine', 'w:3');
    queue.resolve('from-another-store', 'v:99');

    const reread = new Outbox(file);
    reread.load();
    assert.strictEqual(
      reread.cursor,
      'w:3',
      'the queue on disk holds a cursor from an answer about somebody else\'s request'
    );
  });
});

describe('U-req the entries the queue hands out cannot be edited', () => {
  it('refuses an assignment to a request id', () => {
    const file = scratch('frozen');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('r1', 'one\n'));
    const handed = queue.entries[0];
    assert.throws(
      () => {
        (handed as { req: string }).req = 'r2';
      },
      TypeError,
      'the request id was changed under whatever is sending it'
    );
    assert.strictEqual(queue.entries[0].req, 'r1', 'the queue kept the changed name');
  });

  it('refuses an assignment to an entry that came back from find', () => {
    const file = scratch('frozen-find');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('r1', 'one\n'));
    const found = queue.find('r1');
    assert.ok(found !== undefined);
    assert.throws(() => {
      (found as unknown as { state: string }).state = 'sent';
    }, TypeError);
  });

  /*
   * AND THE QUEUE ITSELF STILL CHANGES. Freezing what is handed out must
   * not freeze what the queue does: every mutator builds a fresh state
   * and adopts it after the disk has changed, and a build that froze the
   * stored objects in a way those mutators touched would fail here.
   */
  it('still lets the queue change the entry it holds', () => {
    const file = scratch('frozen-mutate');
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('r1', 'one\n'));
    assert.strictEqual(queue.entries[0].state, 'queued');
    queue.aboutToSend('r1', 'w:8');
    assert.strictEqual(queue.entries[0].state, 'sent', 'the queue could not mark its own entry');
    assert.strictEqual(queue.entries[0].cursor, 'w:8');
    queue.markPending('r1', 'no answer');
    assert.strictEqual(queue.entries[0].state, 'pending');
    queue.resolve('r1', 'w:9');
    assert.strictEqual(queue.entries.length, 0, 'the entry could not be removed');
    assert.strictEqual(queue.cursor, 'w:9');
  });

  /*
   * AND IT SURVIVES A REREAD. The entries a fresh Outbox loads from disk
   * are handed out on the same terms; a build that only froze what it
   * had added in memory would pass the cells above.
   */
  it('hands out frozen entries after loading them from disk', () => {
    const file = scratch('frozen-reload');
    const first = new Outbox(file);
    first.load();
    first.enqueue(entry('r1', 'one\n'));
    const second = new Outbox(file);
    second.load();
    const handed = second.entries[0];
    assert.strictEqual(handed.req, 'r1');
    assert.throws(() => {
      (handed as { req: string }).req = 'r2';
    }, TypeError);
  });
});

/*
 * D1/D2: ONE DURABILITY RULE FOR EVERY QUEUE MUTATOR. (queue item 22, design
 * v4)
 *
 * A failure BEFORE the rename throws `OutboxWriteError` and changes nothing;
 * a failure of the directory flush AFTER it counts the mutation as done and
 * returns the warning. Each of the eight mutators besides `enqueue` is driven
 * through a Files stand-in that starts failing only after the seed is
 * written, so the failing write is the mutation's own.
 *
 * D1 asks three things, and each is needed (K7): the queue file, re-read by
 * a fresh reader, holds the new state; the object's own state -- read
 * without reloading first -- holds it too, which is what a mutant returning
 * before `this.data = next` fails; and the call RETURNED the warning rather
 * than throwing it. It is asked for a flush that returns its reason (the
 * shipping `syncDirectory`) and for one that throws (a stand-in may).
 */
const MUTATED = '33333333-3333-3333-3333-333333333333';

interface Mutation {
  name: string;
  seed: (outbox: Outbox) => void;
  run: (outbox: Outbox) => string | null;
  shows: (outbox: Outbox) => boolean;
}

const MUTATIONS: Mutation[] = [
  { name: 'setCursor', seed: () => undefined, run: (o) => o.setCursor('w:9'), shows: (o) => o.cursor === 'w:9' },
  {
    name: 'clearCursor',
    seed: (o) => void o.setCursor('w:9'),
    run: (o) => o.clearCursor(),
    shows: (o) => o.cursor === null
  },
  {
    name: 'markParked',
    seed: (o) => void o.enqueue(entry(MUTATED, 'one\n')),
    run: (o) => o.markParked(MUTATED, 'parked for the cell'),
    shows: (o) => o.find(MUTATED)?.state === 'parked'
  },
  {
    name: 'markPending',
    seed: (o) => void o.enqueue(entry(MUTATED, 'one\n')),
    run: (o) => o.markPending(MUTATED, 'pending for the cell'),
    shows: (o) => o.find(MUTATED)?.state === 'pending'
  },
  {
    name: 'aboutToSend',
    seed: (o) => void o.enqueue(entry(MUTATED, 'one\n')),
    run: (o) => o.aboutToSend(MUTATED, 'w:8'),
    shows: (o) => o.find(MUTATED)?.state === 'sent' && o.find(MUTATED)?.cursor === 'w:8'
  },
  {
    name: 'markImported',
    seed: (o) => void o.enqueue(entry(MUTATED, 'one\n')),
    run: (o) => o.markImported(MUTATED, 'S-dead.claim.1'),
    shows: (o) => o.find(MUTATED)?.importedBy === 'S-dead.claim.1'
  },
  {
    name: 'resolve',
    seed: (o) => void o.enqueue(entry(MUTATED, 'one\n')),
    run: (o) => o.resolve(MUTATED, null),
    shows: (o) => o.find(MUTATED) === undefined
  },
  {
    name: 'unparkAll',
    seed: (o) => {
      o.enqueue(entry(MUTATED, 'one\n'));
      o.markParked(MUTATED, 'parked for the cell');
    },
    run: (o) => {
      const released = o.unparkAll();
      assert.strictEqual(released.unparked, 1, 'unparkAll did not say it released the parked entry');
      return released.durability;
    },
    shows: (o) => o.find(MUTATED)?.state === 'queued'
  }
];

/*
 * A Files stand-in that behaves until `failing` is set, and then fails at
 * one step: the directory flush, by returning a reason or by throwing; or
 * the rename.
 */
function failingAt(step: 'returns' | 'throws' | 'rename'): { files: FileOps; fail: () => void } {
  let failing = false;
  const files: FileOps = {
    ...nodeFileOps,
    syncDirectory(directory: string): string | null {
      if (failing && step === 'returns') {
        return 'the disk said no';
      }
      if (failing && step === 'throws') {
        throw new Error('the disk threw');
      }
      return nodeFileOps.syncDirectory(directory);
    },
    rename(from: string, to: string): void {
      if (failing && step === 'rename') {
        throw new Error('the rename was refused');
      }
      nodeFileOps.rename(from, to);
    }
  };
  return { files, fail: () => (failing = true) };
}

describe('D1 a queue write whose rename landed is done, and says what it could not promise', () => {
  for (const mutation of MUTATIONS) {
    for (const [step, reason] of [
      ['returns', 'the disk said no'],
      ['throws', 'Error: the disk threw']
    ] as Array<['returns' | 'throws', string]>) {
      it(`${mutation.name}: done, on disk and in the object, with the warning returned (the flush ${step})`, () => {
        const file = scratch(`d1-${mutation.name}-${step}`);
        const stand = failingAt(step);
        const outbox = new Outbox(file, stand.files);
        outbox.load();
        mutation.seed(outbox);
        stand.fail();
        let warning: string | null = null;
        assert.doesNotThrow(() => {
          warning = mutation.run(outbox);
        }, `${mutation.name} threw about a write whose rename had landed`);
        assert.strictEqual(
          warning,
          `the queue at ${file} was written, but its directory could not be flushed (${reason}), so it ` +
            'may not survive the machine losing power',
          `${mutation.name} did not return the durability warning`
        );
        assert.ok(mutation.shows(outbox), `${mutation.name}'s own state does not show the mutation`);
        const fresh = new Outbox(file);
        fresh.load();
        assert.ok(mutation.shows(fresh), `the queue file does not show ${mutation.name}'s mutation`);
      });
    }
  }
});

describe('D2 a queue write that failed before its rename throws and changes nothing', () => {
  for (const mutation of MUTATIONS) {
    it(`${mutation.name}: OutboxWriteError, the file and the object as they were`, () => {
      const file = scratch(`d2-${mutation.name}`);
      const stand = failingAt('rename');
      const outbox = new Outbox(file, stand.files);
      outbox.load();
      mutation.seed(outbox);
      const bytes = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null;
      const held = JSON.stringify({ entries: outbox.entries, cursor: outbox.cursor });
      stand.fail();
      assert.throws(() => mutation.run(outbox), OutboxWriteError);
      assert.strictEqual(fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null, bytes, 'the queue file changed');
      assert.strictEqual(
        JSON.stringify({ entries: outbox.entries, cursor: outbox.cursor }),
        held,
        `${mutation.name} changed the object although the file was not written`
      );
    });
  }
});

/*
 * D4c: THE SINK KEEPS ONE SENTENCE PER QUEUE FILE UNTIL IT IS TAKEN, THE
 * LATEST. (queue item 22, K5) Two writes to one file between two notices say
 * one thing -- that file may not survive a power cut -- and the latest reason
 * is the one to read; a second file is a second thing.
 */
describe('D4c the durability sink says each queue file once', () => {
  it('keeps the latest of two sentences about one queue file', () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'the first reason');
    sink.add('/q/one/outbox.json', 'the second reason');
    assert.deepStrictEqual(sink.take(), ['the second reason']);
    assert.deepStrictEqual(sink.take(), [], 'what was taken was still held');
  });

  /*
   * D4d: ONE TEXT THAT CANNOT BE SHOWN DOES NOT TAKE THE OTHERS WITH IT.
   * (delivery review r2 of item 22, the W) The batch is taken before anything
   * is shown, so a `show` that throws at once for the first text used to lose
   * the second; now the second is shown and the failure reported.
   */
  it('D4d shows the second text when showing the first throws, and reports the failure', () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'about one');
    sink.add('/q/two/outbox.json', 'about two');
    const shown: string[] = [];
    const failures: unknown[] = [];
    sink.showAll(
      (text) => {
        if (text === 'about one') {
          throw new Error('the editor threw');
        }
        shown.push(text);
      },
      (error) => failures.push(error)
    );
    assert.deepStrictEqual(shown, ['about two'], 'the second text was not shown');
    assert.strictEqual(failures.length, 1, 'the failure was not reported');
    assert.match(String(failures[0]), /the editor threw/);
    assert.deepStrictEqual(sink.take(), [], 'the batch was not taken');
  });

  /*
   * D4e: A SHOW THAT REJECTS LATER IS FOLLOWED. (delivery review r3 of item 22,
   * the W) The first text's show answers with a promise that rejects; the
   * second text is shown, the rejection reaches `failed`, and nothing is left
   * unhandled.
   */
  it('D4e follows a show that rejects, and leaves nothing unhandled', async () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'about one');
    sink.add('/q/two/outbox.json', 'about two');
    const shown: string[] = [];
    const failures: unknown[] = [];
    const stray: unknown[] = [];
    const note = (reason: unknown): void => {
      stray.push(reason);
    };
    process.on('unhandledRejection', note);
    try {
      sink.showAll(
        (text) => {
          if (text === 'about one') {
            return Promise.reject(new Error('the editor refused'));
          }
          shown.push(text);
          return Promise.resolve(undefined);
        },
        (error) => failures.push(error)
      );
      await new Promise((resolve) => setTimeout(resolve, 20));
    } finally {
      process.off('unhandledRejection', note);
    }
    assert.deepStrictEqual(shown, ['about two'], 'the second text was not shown');
    assert.strictEqual(failures.length, 1, 'the rejection did not reach failed');
    assert.match(String(failures[0]), /the editor refused/);
    assert.deepStrictEqual(stray, [], 'a rejection was left unhandled');
  });

  /*
   * E1: BY WHEN IT WAS PRODUCED, NOT BY WHEN IT ARRIVED. (queue item 46) A
   * warning held on an outcome can reach the sink after a newer one about the
   * same file; its stamp says it is older, and it does not replace the newer.
   * The control: warnings that carry no stamp are taken as produced when they
   * arrive, so the later arrival is kept, as before.
   */
  it('E1 keeps the newer of two warnings about one file when the older arrives second', () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'the newer reason', 2);
    sink.add('/q/one/outbox.json', 'the older reason', 1);
    assert.deepStrictEqual(sink.take(), ['the newer reason'], 'an older warning replaced a newer one by arriving later');
  });

  it('E1 (control) keeps the later arrival when neither warning carries a stamp', () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'the first to arrive');
    sink.add('/q/one/outbox.json', 'the second to arrive');
    assert.deepStrictEqual(sink.take(), ['the second to arrive']);
  });

  it('keeps one sentence for each of two queue files', () => {
    const sink = new DurabilitySink();
    sink.add('/q/one/outbox.json', 'about one');
    sink.add('/q/two/outbox.json', 'about two');
    assert.deepStrictEqual(sink.take(), ['about one', 'about two']);
  });
});

/*
 * D4 and D4b: THE EXTENSION SHOWS WHAT THE SINK HOLDS, ONCE, AND AFTER A
 * REBUILD TOO. (queue item 22, design v4, K4/K6/K13)
 *
 * Driven through the activated extension with the editor and the core
 * stood in for (test/support/extension-schedules.js): a save leaves an entry
 * pending, and the queue directory's flush then fails once. D4: the entry
 * is parked, and the retry command's RELEASE (`unparkAll`, K6) writes under
 * that failure; the command shows the sentence once, and the next command
 * shows nothing more (delivery review r1 of item 22, S1: the entry was left
 * pending, so `unparkAll` wrote nothing and the cell read a later mark). D4b:
 * the retry's send is held, the settings change (the Saver is replaced and
 * the generation moves), and only then does the held send answer and the
 * OLD Saver write under the failure -- the sentence is still shown, because
 * the sink is the window's. A Saver-owned sink would be dropped with the
 * Saver and show nothing.
 *
 * NOTE: THE OLD SAVER'S WRITE IS ITS `markPending`, NOT A SETTLE. The design
 * names a settle; the stand-in core answers every commit `unknown`, so what
 * the old Saver writes after the rebuild is the mark. Both go to the sink by
 * the same road, which is what this cell reads.
 */
describe('D4 the window shows each queue warning once, whichever Saver raised it', () => {
  const run = (name: string): Record<string, unknown> => {
    const child = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
      encoding: 'utf8',
      timeout: 20000
    });
    assert.strictEqual(child.status, 0, child.stdout + child.stderr);
    const result = JSON.parse(child.stdout.trim().split('\n').pop() as string);
    assert.strictEqual(result.complete, true);
    return result.result;
  };
  const sentence = (queue: string): string =>
    `theourgia: the queue at ${queue} was written, but its directory could not be flushed (the disk said no), ` +
    'so it may not survive the machine losing power';
  const times = (shown: unknown, text: string): number =>
    (shown as Array<{ text: string; level: string }>).filter((n) => n.text === text && n.level === 'warning').length;

  it('D4 shows the warning of the retry command\'s release once, and the next command shows nothing more', () => {
    const r = run('durability-retry');
    assert.deepStrictEqual(r.pendingBefore, ['pending'], 'the save did not leave an entry pending');
    assert.deepStrictEqual(r.parkedBefore, ['parked'], 'the entry was not parked, so the release writes nothing');
    assert.strictEqual(r.failed, 1, 'the flush never failed, so this cell is about nothing');
    assert.strictEqual(times(r.afterRetry, sentence(r.queue as string)), 1, JSON.stringify(r.afterRetry));
    assert.strictEqual(times(r.afterNext, sentence(r.queue as string)), 0, 'the sentence was shown again by the next command');
  });

  it('D4b shows a warning the old Saver raised after the settings changed', () => {
    const r = run('durability-rebuild');
    assert.deepStrictEqual(r.pendingBefore, ['pending'], 'the save did not leave an entry pending');
    assert.strictEqual(r.failed, 1, 'the flush never failed, so this cell is about nothing');
    assert.strictEqual(times(r.afterChange, sentence(r.queue as string)), 0, 'the sentence was there before the old Saver wrote');
    assert.strictEqual(times(r.shown, sentence(r.queue as string)), 1, `the old Saver's warning was not shown: ${JSON.stringify(r.shown)}`);
  });

  /*
   * E7: THE WINDOW KEEPS THE NEWER WARNING WHEN THE OLDER ARRIVES SECOND.
   * (queue item 46, folded from its review) The shipping Saver callback must
   * hand the held warning's production stamp to the window's sink; without it
   * the older send mark, arriving after the settle's newer resolve warning,
   * replaces it.
   */
  it('E7 shows the settle\'s newer warning, not the older send mark that reached the sink after it', () => {
    const r = run('durability-order');
    assert.strictEqual(r.failed, 2, 'the two flushes did not both fail');
    assert.strictEqual(r.left, 0);
    assert.deepStrictEqual(r.after, [], 'the entry was not settled out of the queue');
    const said = (reason: string): string =>
      `theourgia: the queue at ${r.queue as string} was written, but its directory could not be flushed (${reason}), ` +
      'so it may not survive the machine losing power';
    assert.strictEqual(times(r.shown, said('the newer reason')), 1, `the newer warning was not shown: ${JSON.stringify(r.shown)}`);
    assert.strictEqual(times(r.shown, said('the older reason')), 0, 'the older warning was shown in its place');
  });

  /*
   * D4f: THE AWAITED SAVE'S OWN WARNING IS SHOWN. (queue item 22, folded from
   * its closing review r4) Its enqueue is the write that fails, so the
   * sentence rides on the save's outcome and nothing puts it in the sink;
   * only the save handler's display of `outcome.durability` shows it.
   */
  it('D4f shows the warning the awaited save carries on its outcome, once', () => {
    const r = run('durability-save');
    assert.deepStrictEqual(r.pendingBefore, ['pending'], 'the first save did not leave an entry pending');
    assert.strictEqual(r.failed, 1, 'the second save\'s enqueue never failed, so this cell is about nothing');
    assert.strictEqual(times(r.shown, sentence(r.queue as string)), 1, `the save's own warning was not shown once: ${JSON.stringify(r.shown)}`);
  });

  /*
   * NOTE: AND THROUGH THE STARTUP RETRY, NOT A COMMAND. (delivery review r2 of
   * item 22, S1) D4b's held drain is the retry command's, so the command
   * wrapper shows it; deleting the startup retry's own take left every cell
   * green. Here the held drain is a startup retry of a Saver that a second
   * settings change replaces, and no command runs at all.
   */
  it('D4b (startup) shows a warning a replaced Saver\'s startup drain raised', () => {
    const r = run('durability-startup');
    assert.deepStrictEqual(r.pendingBefore, ['pending'], 'the save did not leave an entry pending');
    assert.strictEqual(r.failed, 1, 'the flush never failed, so this cell is about nothing');
    assert.strictEqual(times(r.afterChange, sentence(r.queue as string)), 0, 'the sentence was there before the replaced Saver wrote');
    assert.strictEqual(times(r.shown, sentence(r.queue as string)), 1, `the replaced Saver's warning was not shown: ${JSON.stringify(r.shown)}`);
  });
});
