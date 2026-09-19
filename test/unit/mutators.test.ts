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
 * EVERY CHANGE TO THE QUEUE IS A READ, A CHANGE, AND A WRITE. (R11)
 *
 * ⚠️ THE READ IS THE PART THAT IS MISSING.
 *
 * Each mutator copies what this object last read, edits the copy, and
 * writes the whole file. Whatever another writer put there in between
 * is gone -- not merged, not refused: overwritten, silently, by a file
 * that was correct when it was loaded. The queue is a file two
 * processes can reach (a takeover writes into a session's queue while
 * that session may be waking up) and this object's idea of "what the
 * queue holds" can be arbitrarily old.
 *
 * The cells below build that state with TWO Outbox objects over ONE
 * file -- which is what a second process looks like from here, minus
 * the scheduling -- and ask whether the second writer's work survives.
 *
 * ⚠️ AND A STALE READER IS NOT A HYPOTHETICAL HERE. The design names
 * the cross-process race as out of scope and asks for the failure to be
 * VISIBLE; reloading under the lock is what makes a change addressed to
 * a request the writer never saw fail to erase it.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as ts from 'typescript';
import { Outbox, OutboxEntry } from '../../src/outbox';

function scratch(): string {
  return path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-queue-')), 'outbox.json');
}

function entry(req: string, id = 'a.2'): OutboxEntry {
  return {
    req,
    cursor: 'w:7',
    id,
    field: 'src',
    payload: `body for ${req}\n`,
    state: 'queued',
    createdAt: 1000,
    lastError: null,
    importedBy: null
  };
}

/*
 * TWO OBJECTS OVER ONE FILE, both having read it. `mine` is the one
 * that is about to make a change; `theirs` stands for the writer that
 * got there first.
 */
function twoReaders(file: string): { mine: Outbox; theirs: Outbox } {
  const mine = new Outbox(file);
  mine.load();
  const theirs = new Outbox(file);
  theirs.load();
  return { mine, theirs };
}

function reqsOnDisk(file: string): string[] {
  const held = new Outbox(file);
  held.load();
  return held.entries.map((e) => e.req).sort();
}

describe('R11 a change to the queue does not erase what it never read', () => {
  it('keeps an entry another writer added, when one is resolved', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.enqueue(entry('theirs-1'));
    mine.resolve('mine-1', 'w:8');
    assert.deepStrictEqual(
      reqsOnDisk(file),
      ['theirs-1'],
      'resolving one request wrote back a copy of the queue taken before the other writer added theirs'
    );
  });

  it('keeps an entry another writer added, when one is marked pending', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.enqueue(entry('theirs-1'));
    mine.markPending('mine-1', 'nobody knows');
    assert.deepStrictEqual(reqsOnDisk(file), ['mine-1', 'theirs-1']);
  });

  it('keeps an entry another writer added, when one is about to be sent', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.enqueue(entry('theirs-1'));
    mine.aboutToSend('mine-1', 'w:9');
    assert.deepStrictEqual(reqsOnDisk(file), ['mine-1', 'theirs-1']);
  });

  it('keeps an entry another writer added, when one is marked imported', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.enqueue(entry('theirs-1'));
    mine.markImported('mine-1', 'claim.3');
    assert.deepStrictEqual(reqsOnDisk(file), ['mine-1', 'theirs-1']);
  });

  it('keeps an entry another writer added, when one is enqueued', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    theirs.enqueue(entry('theirs-1'));
    mine.enqueue(entry('mine-1'));
    assert.deepStrictEqual(reqsOnDisk(file), ['mine-1', 'theirs-1']);
  });

  /*
   * ⚠️ THE NAMED SUCCESSOR TO `will not write to a queue nobody has
   * read` (durability.test.ts).
   *
   * That cell pinned a flag: an object whose `load` had never been
   * called refused to write, because `readable` was false. Reading
   * before every change retires the flag -- there is no such object any
   * more, the first mutator reads for itself -- and the property the
   * flag was protecting has to be asserted directly instead: what is on
   * disk survives a change made by something that had not looked.
   *
   * ⛔ The retired cell is NOT edited here. The expectation it holds
   * was written against the build it was written for, and changing it
   * to match new code is how a cell stops being independent evidence.
   * It goes to the main session as its own item.
   */
  it('reads the file before its first change, even if nobody loaded it', () => {
    const file = scratch();
    const seeded = new Outbox(file);
    seeded.load();
    seeded.enqueue(entry('was-here-first'));

    const nobodyRead = new Outbox(file);
    nobodyRead.enqueue(entry('arrived-later'));
    assert.deepStrictEqual(
      reqsOnDisk(file),
      ['arrived-later', 'was-here-first'],
      'a change made by an object that had never read the file replaced what was in it'
    );
  });

  /*
   * ⚠️ AND A CHANGE ADDRESSED TO SOMETHING THAT IS NOT THERE DOES
   * NOTHING AT ALL.
   *
   * The entry may have been settled by the other writer between the
   * read and the change. Reviving it -- which is what writing back a
   * copy that still holds it amounts to -- resends a request the store
   * has already answered.
   */
  it('does nothing when the request it names has already gone', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.resolve('mine-1', 'w:8');
    mine.markPending('mine-1', 'nobody knows');
    assert.deepStrictEqual(
      reqsOnDisk(file),
      [],
      'a request another writer had already settled came back when this one changed it'
    );
  });

  it('does not move the cursor of a request it is not holding', () => {
    const file = scratch();
    const { mine, theirs } = twoReaders(file);
    mine.enqueue(entry('mine-1'));
    theirs.load();
    theirs.resolve('mine-1', 'w:8');
    mine.resolve('mine-1', 'w:99');
    const held = new Outbox(file);
    held.load();
    assert.strictEqual(
      held.cursor,
      'w:8',
      'the cursor moved for an answer to a request this queue no longer held'
    );
  });
});

/*
 * ⚠️ AND THE RULE IS COUNTED, NOT REMEMBERED. A mutator added later
 * that forgets to read first is the same defect again, and the only
 * thing that would notice is a census.
 */
describe('R11 every mutator reads the file before it changes it', () => {
  it('starts every public mutator with a re-read', () => {
    const file = path.join(__dirname, '..', '..', '..', 'src', 'outbox.ts');
    const src = ts.createSourceFile(
      'outbox.ts',
      fs.readFileSync(file, 'utf8'),
      ts.ScriptTarget.ES2022,
      true
    );
    const mutators: ts.MethodDeclaration[] = [];
    const walk = (n: ts.Node): void => {
      if (ts.isMethodDeclaration(n)) {
        const body = n.body?.getText(src) ?? '';
        if (/\bthis\.commit\(/.test(body)) {
          mutators.push(n);
        }
      }
      ts.forEachChild(n, walk);
    };
    walk(src);
    assert.ok(
      mutators.length >= 6,
      `this census found ${mutators.length} mutators in outbox.ts, which is too few to be reading the file`
    );
    const without = mutators
      .filter((m) => {
        const body = m.body;
        if (body === undefined) {
          return true;
        }
        const first = body.statements[0];
        return first === undefined || !/this\.refresh\(\)/.test(first.getText(src));
      })
      .map((m) => m.name.getText(src));
    assert.deepStrictEqual(
      without,
      [],
      'these change the queue from whatever this object last read, so a change another writer ' +
        'made in between is written over rather than kept'
    );
  });
});

/*
 * R10 THE QUEUE'S CURSOR IS A POSITION IN ONE STORE'S LOG, AND IT MOVES
 * THE WAY A LOG MOVES. (section 13.3)
 *
 * `resolve` takes whatever position the answer carried, with nothing
 * asked about where the queue already stood. Two things follow, and
 * both of them are what a cursor exists to prevent:
 *
 *   - a late replay of an older request carries an older position, and
 *     the queue winds BACKWARDS to it -- the next save is then composed
 *     against a place the store has already moved past;
 *   - an answer from a different writer carries a position in a
 *     different writer's numbering, which is not comparable with this
 *     one at all, and adopting it puts a number in the field that means
 *     nothing here.
 *
 * The design's answer is one rule for each: forward only within a
 * writer, and across writers do not adopt -- drop it and ask the store
 * where it stands.
 */
describe('R10 the queue cursor only moves the way the store moved', () => {
  it('does not wind the cursor back for a late answer from the same writer', () => {
    const file = scratch();
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('older'));
    queue.enqueue(entry('newer'));
    queue.resolve('newer', 'w:9');
    assert.strictEqual(queue.cursor, 'w:9');
    queue.resolve('older', 'w:4');
    assert.strictEqual(
      queue.cursor,
      'w:9',
      'a late answer wound the queue back to a position the store has already moved past'
    );
  });

  /*
   * ⚠️ AND THE TWIN, ALONG THE AXIS UNDER TEST: the same shape with the
   * positions the other way round must move it. A build that simply
   * stopped writing the cursor would pass the cell above.
   */
  it('does move the cursor forward for a later answer from the same writer', () => {
    const file = scratch();
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('first'));
    queue.enqueue(entry('second'));
    queue.resolve('first', 'w:4');
    queue.resolve('second', 'w:9');
    assert.strictEqual(queue.cursor, 'w:9');
  });

  /*
   * ⚠️ ANOTHER WRITER'S NUMBER IS NOT A LATER NUMBER, IT IS ANOTHER
   * NUMBER. Nothing can be concluded by comparing them, so the position
   * is dropped rather than adopted, and the next send asks the store
   * where it stands.
   */
  it('drops the cursor rather than adopting one from another writer', () => {
    const file = scratch();
    const queue = new Outbox(file);
    queue.load();
    queue.enqueue(entry('mine'));
    queue.setCursor('w:9');
    queue.resolve('mine', 'x:3');
    assert.strictEqual(
      queue.cursor,
      null,
      'a position in another writer’s numbering was written into this queue as if it were comparable'
    );
  });
});
