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
 * The instrument, before anything is measured with it.
 *
 * SEVERAL X1c CELLS ASSERT THAT A COUNT IS ZERO. A recorder that counts
 * nothing satisfies every one of them, and it does so silently -- which
 * is the failure mode this whole batch keeps meeting. So the first
 * reading taken from it is a POSITIVE one: it is made to record, and
 * the count is required to be there. Only after that does a zero mean
 * anything.
 *
 * AND NOTHING MAY GO AROUND IT. A count is a claim about what this
 * process did, so a product file that calls `fs` directly is not a
 * style question -- it is a hole in the measurement, and the cell at
 * the bottom refuses one.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Answer } from '../../src/client';
import { eventFromWrite } from '../../src/cursor';
import { nodeFileOps } from '../../src/fsops';
import { initWire, parseAnswers } from '../../src/wire';
import { wroteAnswer } from '../support/answers';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-fsops-'));
}

describe('the file-operation recorder records', () => {
  it('counts a write, a rename and an unlink against the paths they touched', () => {
    const dir = scratch();
    const a = path.join(dir, 'a.md');
    const b = path.join(dir, 'b.md');
    const recorder = new RecordingFs();

    recorder.writeText(a, 'one\n');
    assert.strictEqual(recorder.countOf('writeText', a), 1, 'a write was not recorded');
    assert.strictEqual(fs.readFileSync(a, 'utf8'), 'one\n', 'the write did not reach the disk');

    recorder.rename(a, b);
    assert.strictEqual(recorder.countOf('rename', a), 1, 'a rename was not recorded against its source');
    assert.strictEqual(
      recorder.countOf('rename', b),
      1,
      'a rename was not recorded against its destination, so a file moved ONTO would count zero'
    );
    assert.ok(!fs.existsSync(a) && fs.existsSync(b), 'the rename did not happen');

    recorder.unlink(b);
    assert.strictEqual(recorder.countOf('unlink', b), 1, 'an unlink was not recorded');
    assert.ok(!fs.existsSync(b), 'the unlink did not happen');
  });

  it('counts nothing against a path nothing touched, which is the reading the cells rely on', () => {
    const dir = scratch();
    const recorder = new RecordingFs();
    recorder.writeText(path.join(dir, 'a.md'), 'one\n');
    assert.strictEqual(recorder.countOf('unlink', path.join(dir, 'a.md')), 0);
    assert.strictEqual(recorder.countOf('rename', path.join(dir, 'b.md')), 0);
  });

  it('answers about one file however the path is spelled', () => {
    const dir = scratch();
    const recorder = new RecordingFs();
    recorder.writeText(path.join(dir, 'a.md'), 'one\n');
    assert.strictEqual(
      recorder.countOf('writeText', path.join(dir, 'sub', '..', 'a.md')),
      1,
      'the same file counted zero because the path was spelled differently'
    );
  });

  it('performs the durable write it records', () => {
    const dir = scratch();
    const file = path.join(dir, 'd.md');
    const recorder = new RecordingFs();
    recorder.writeDurably(file, 'durable\n');
    assert.strictEqual(recorder.countOf('writeDurably', file), 1);
    assert.strictEqual(fs.readFileSync(file, 'utf8'), 'durable\n');
  });

  it('opens a directory that cannot be opened without failing the write that preceded it', () => {
    const dir = scratch();
    nodeFileOps.syncDirectory(path.join(dir, 'nothing-here'));
  });
});

/*
 * ⚠️ THE STAND-IN'S ANSWERS ARE READ BY THE PRODUCT BEFORE ANY CELL
 * RELIES ON THEM.
 *
 * A cell about a save that settles invented its own `ok` and got the
 * shape wrong: `(ok ((cursor . "w:2")))` is read by `eventFromWrite` as
 * an ok naming no record, so the entry was marked pending and the
 * settler never ran. The cell passed and its comment described what a
 * settled save does. The stand-in lacked the property under
 * investigation, and nothing said so.
 *
 * So the shapes live in one file and this is where they are put to the
 * product's own reader. If `wroteAnswer` ever stops being read as a
 * settlement, this fails here -- in the file about instruments -- rather
 * than silently emptying whatever cell was relying on it.
 */
describe('the answers a stand-in core gives are answers the product reads', () => {
  before(async () => {
    await initWire();
  });

  it('reads a written answer as a settlement that names its record', () => {
    const answer: Answer = {
      argv: ['set'],
      rc: 0,
      ok: true,
      kind: 'datum',
      text: '',
      answers: parseAnswers(wroteAnswer(2)),
      stderr: ''
    };
    const event = eventFromWrite(answer);
    assert.ok(
      event !== null,
      'the shape the cells send for a successful write is not read as naming a record, so every ' +
        'cell about settling is about a save that was marked pending instead'
    );
    assert.deepStrictEqual(event, { writer: 'w', seq: 2 });
  });

  /*
   * AND THE SHAPE THAT WAS WRONG IS KEPT, as the control: it is what a
   * hand-written answer looked like, and the point is that the product
   * does NOT read it as a settlement. Without this row the cell above
   * passes for any reader at all, including one that says yes to
   * everything.
   */
  it('does not read the invented shape as a settlement', () => {
    const answer: Answer = {
      argv: ['set'],
      rc: 0,
      ok: true,
      kind: 'datum',
      text: '',
      answers: parseAnswers('(ok ((cursor . "w:2")))\n'),
      stderr: ''
    };
    assert.strictEqual(
      eventFromWrite(answer),
      null,
      'the shape a cell invented is read as a settlement after all; if that changed on purpose, ' +
        'the cells that were measuring nothing have to be re-read'
    );
  });
});

/*
 * ⚠️ "NOT THERE" AND "I COULD NOT LOOK" ARE TWO ANSWERS.
 *
 * `existsSync` gives one word for both, which is the right convenience
 * for a caller deciding what to do next and the wrong one for a caller
 * WRITING A SENTENCE: a leftover file under an ancestry this process
 * cannot search does not stop existing, and the publication failure that
 * names it is read by the one person who can clear it up.
 *
 * `presenceOf` was added for that, and these cells are here because the
 * third answer is the whole point of it: without a case that produces
 * "could not be established" from the REAL file system, the branch that
 * says so is a branch only a stand-in can reach, and a cell for it would
 * be measuring a world the product does not have. Found in review.
 */
describe('presence is three answers, not two', () => {
  it('says a file that is there is there', () => {
    const dir = scratch();
    const file = path.join(dir, 'a.md');
    fs.writeFileSync(file, 'x', 'utf8');
    assert.deepStrictEqual(nodeFileOps.presenceOf(file), { known: true, there: true });
  });

  it('says a name nothing has written is not there', () => {
    const dir = scratch();
    assert.deepStrictEqual(nodeFileOps.presenceOf(path.join(dir, 'nothing')), {
      known: true,
      there: false
    });
  });

  /*
   * AND A PATH THAT RUNS THROUGH A FILE IS NOT THERE EITHER -- ENOTDIR,
   * which is an answer about this name and not a failure to look.
   */
  it('says a path leading through a file is not there', () => {
    const dir = scratch();
    const file = path.join(dir, 'a.md');
    fs.writeFileSync(file, 'x', 'utf8');
    assert.deepStrictEqual(nodeFileOps.presenceOf(path.join(file, 'child')), {
      known: true,
      there: false
    });
  });

  it('says it could not be established when the ancestry cannot be searched', () => {
    /*
     * ⚠️ ROOT CAN SEARCH ANYTHING, so this machine cannot produce the
     * state and the cell says which machine it was and what to do --
     * rather than reporting "skipped" and letting a reader take it for a
     * pass. Every run in this batch has been non-root.
     */
    if (typeof process.getuid === 'function' && process.getuid() === 0) {
      assert.fail(
        'this cell needs a directory the process may not search, and root may search every ' +
          'directory; run the suite as an ordinary user'
      );
    }
    const dir = scratch();
    const closed = path.join(dir, 'closed');
    fs.mkdirSync(closed);
    const inside = path.join(closed, 'a.md');
    fs.writeFileSync(inside, 'x', 'utf8');
    fs.chmodSync(closed, 0o000);
    try {
      /*
       * THE CONTROL, IN THE SAME BREATH: `existsSync` answers this
       * question "no" -- the comfortable answer, about a file that is
       * sitting right there. That reading is why this primitive exists,
       * and asserting it here means the day it stops being true, the
       * reason for the primitive is re-read rather than assumed.
       */
      assert.strictEqual(
        nodeFileOps.exists(inside),
        false,
        'exists no longer conflates an unsearchable path with an absent one; re-read why ' +
          'presenceOf exists before changing anything'
      );
      assert.deepStrictEqual(nodeFileOps.presenceOf(inside), { known: false });
    } finally {
      fs.chmodSync(closed, 0o700);
    }
  });
});

/*
 * THE OPERATIONS HAVE ONE DOOR.
 *
 * `src/fsops.ts` is the only file under src/ allowed to import `fs`.
 * Every other one receives a `FileOps`. Without this, "the extension
 * never unlinks a published path" would be a claim about the calls that
 * happen to go through the recorder, and a single direct `fs.unlinkSync`
 * anywhere else would make every count in this batch a measurement of
 * the wrong thing -- while reading zero, which is the answer that gets
 * believed.
 */
describe('no part of the extension reaches the file system except through fsops', () => {
  it('finds no other source file importing fs', () => {
    const dir = path.join(__dirname, '..', '..', '..', 'src');
    const offenders: string[] = [];
    for (const name of fs.readdirSync(dir)) {
      if (!name.endsWith('.ts') || name === 'fsops.ts') {
        continue;
      }
      const text = fs.readFileSync(path.join(dir, name), 'utf8');
      if (/(^|\n)\s*import[^\n]*['"]fs['"]/.test(text) || /require\(\s*['"]fs['"]\s*\)/.test(text)) {
        offenders.push(name);
      }
    }
    assert.deepStrictEqual(
      offenders,
      [],
      'these files reach the file system without passing the recorder, so its counts do not describe them'
    );
  });

  /*
   * AND THE CHECK ABOVE CAN ACTUALLY SEE ONE. A scanner that matched
   * nothing would report an empty list for a tree full of offenders,
   * which is the same reassuring zero again.
   */
  it('recognises an import when there is one', () => {
    const pattern = /(^|\n)\s*import[^\n]*['"]fs['"]/;
    assert.ok(pattern.test("import * as fs from 'fs';\n"), 'the scanner does not match a plain import');
    assert.ok(pattern.test("\nimport fs from 'fs';\n"), 'the scanner does not match a default import');
    assert.ok(pattern.test("\n  import { readFileSync } from 'fs';\n"), 'the scanner does not match a named import');
    assert.ok(!pattern.test("import * as path from 'path';\n"), 'the scanner matches something that is not fs');
  });
});
