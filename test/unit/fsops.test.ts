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
import { nodeFileOps } from '../../src/fsops';
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
