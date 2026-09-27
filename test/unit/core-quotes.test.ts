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
 * plugin-r3 item 19: src/ quotes the core by the names its files have.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';

/*
 * THE ONE CORE FILE STILL NAMED `.ss`. The core renamed its sources to `.sc`
 * (276d9f2); its build script kept its name, and the pinned core (877f0da,
 * and f5ebd58 after it) still calls it `build.ss`. Every other `<name>.ss` in src/ is a quotation
 * of a file that is `.sc` now, or gone.
 *
 * NOTE: A BARE `.ss` IS NOT A FILE NAME and is not looked for: the library
 * extension lists in `src/config.ts` name it on purpose, because a library
 * the core loads may still be a `.ss` file.
 */
const STILL_SS = new Set(['build.ss']);

/*
 * AND IN test/ (queue item 37), TWO MORE THAT ARE NOT QUOTATIONS OF A CORE FILE,
 * and one that is owned by another item:
 *
 *   - `core-refusals.ss` is a file of THIS repository (test/support/), a
 *     script the refusal census runs with `--script`; its name is its own.
 *   - outline.test.ts's sentence quoting the old `project` source pins an older core's
 *     behaviour (a recursive walk stopping at a nested document) that the
 *     pinned core no longer has (F85 R1-R3); queue item 48 rewrites those
 *     cells, and this exception goes with it. It is keyed by the sentence,
 *     not by its line, so an edit above it does not move it; an exception
 *     that matches nothing is itself a failure, so it cannot outlive item 48.
 */
const REPOSITORY_SS = new Set(['core-refusals.ss']);
const OWNED_ELSEWHERE: Array<{ file: string; text: string; owner: string }> = [
  /*
   * The text is put together so that this file does not itself spell the name
   * this cell looks for.
   */
  { file: 'unit/outline.test.ts', text: 'project' + '.ss says "a walk stops at one"', owner: 'queue item 48' }
];

describe('plugin-r3 19 the core is quoted by the names its files have', () => {
  /*
   * KEY: ONE NUMBER, AND IT IS ZERO. Sixty-seven quotations were re-read
   * against the core then pinned (877f0da) by hand (queue item 19); what keeps them from
   * coming back is this count, not the reading.
   */
  it('quotes no core file as `.ss` in src/, except the one still named so', () => {
    const src = path.join(__dirname, '..', '..', '..', 'src');
    const found: string[] = [];
    for (const name of fs.readdirSync(src).filter((f) => f.endsWith('.ts')).sort()) {
      const lines = fs.readFileSync(path.join(src, name), 'utf8').split('\n');
      lines.forEach((line, i) => {
        for (const match of line.matchAll(/[A-Za-z][\w-]*\.ss\b/g)) {
          if (!STILL_SS.has(match[0])) {
            found.push(`${name}:${i + 1} ${match[0]}`);
          }
        }
      });
    }
    assert.ok(fs.readdirSync(src).some((f) => f.endsWith('.ts')), 'no source file was read');
    assert.deepStrictEqual(found, [], `${found.length} quotations name a core file as .ss`);
  });

  /*
   * queue item 37: THE SAME RULE OVER test/. Twenty-nine quotations were left
   * there by item 19's scope; each was re-read against the core then pinned, 877f0da (the
   * address table is in item 37's NOTES), not given a new suffix.
   */
  it('quotes no core file as `.ss` in test/, except the named ones', () => {
    const test = path.join(__dirname, '..', '..', '..', 'test');
    const found: string[] = [];
    const used = new Set<number>();
    let read = 0;
    for (const folder of ['unit', 'support', 'integration']) {
      for (const name of fs.readdirSync(path.join(test, folder)).filter((f) => f.endsWith('.ts')).sort()) {
        read += 1;
        const file = `${folder}/${name}`;
        const lines = fs.readFileSync(path.join(test, folder, name), 'utf8').split('\n');
        lines.forEach((line, i) => {
          for (const match of line.matchAll(/[A-Za-z][\w-]*\.ss\b/g)) {
            if (STILL_SS.has(match[0]) || REPOSITORY_SS.has(match[0])) {
              continue;
            }
            const owned = OWNED_ELSEWHERE.findIndex((o) => o.file === file && line.includes(o.text));
            if (owned >= 0) {
              used.add(owned);
              continue;
            }
            found.push(`${file}:${i + 1} ${match[0]}`);
          }
        });
      }
    }
    assert.ok(read > 0, 'no test file was read');
    assert.deepStrictEqual(found, [], `${found.length} quotations in test/ name a core file as .ss`);
    assert.deepStrictEqual(
      OWNED_ELSEWHERE.filter((_, i) => !used.has(i)).map((o) => `${o.file}: ${o.text} (${o.owner})`),
      [],
      'an exception owned by another item matches nothing any more; remove it'
    );
  });
});
