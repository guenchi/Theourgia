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
 * THE EDITOR-HOSTED CELLS ARE COUNTED TOO.
 *
 * The unit suites are guarded by `test/unit/census.test.ts`, which runs
 * mocha in a subprocess and counts what registers. That cannot reach
 * these files: they `require('vscode')`, which exists only inside an
 * extension host, so a plain node dry-run cannot load them -- and the
 * suite they belong to therefore had NO guard at all. A whole editor
 * suite could be deleted and the run would report no failures, which
 * reads exactly like a pass. The same shape has already happened once in
 * this batch: three crash cells went during a rewrite and the suite
 * stayed green.
 *
 * SO THE COUNT IS TAKEN WHERE THE FILES CAN BE LOADED -- inside the
 * host, by the harness that loads them, BEFORE it runs them. Checking
 * first means a shortfall arrives as a failure to start rather than as a
 * cell failure, which are different news.
 *
 * THE COUNTING IS HERE, AND NOT IN THE HARNESS, SO THAT IT CAN BE
 * EXERCISED WITHOUT AN EDITOR. It takes the shape of a mocha suite
 * rather than a Mocha instance, so `test/unit/census.test.ts` can hand it
 * trees that a real run would be a poor way to produce: a file that lost
 * its cells, a file nobody counted, a table-driven suite.
 */
export interface SuiteLike {
  tests: Array<{ file?: string }>;
  suites: SuiteLike[];
}

/*
 * The current host registers 16 extension and 4 generation cells: 15 at
 * the completed v20 editor run, and the split by the editor's own
 * JavaScript symbols (P5).
 */
export const EDITOR_CELLS: Array<[string, number]> = [
  ['extension.test.js', 16],
  ['generation.test.js', 4]
];

function baseName(file: string): string {
  const at = Math.max(file.lastIndexOf('/'), file.lastIndexOf('\\'));
  return at < 0 ? file : file.slice(at + 1);
}

/*
 * HOW MANY CELLS EACH FILE REGISTERED. Registered, not written: a table
 * that produces ten cells from one `it` inside a loop counts ten, and a
 * text scan of the source counts one. That difference was measured at
 * thirty cells across this tree.
 */
export function countRegistered(root: SuiteLike): Map<string, number> {
  const counts = new Map<string, number>();
  const walk = (suite: SuiteLike): void => {
    for (const test of suite.tests) {
      const name = baseName(test.file ?? 'no file');
      counts.set(name, (counts.get(name) ?? 0) + 1);
    }
    for (const child of suite.suites) {
      walk(child);
    }
  };
  walk(root);
  return counts;
}

/*
 * WHAT IS WRONG, IN SENTENCES, or an empty list.
 *
 * BOTH DIRECTIONS, because each catches something the other cannot. A
 * file that registers fewer cells than it did has lost some. A loaded
 * file nobody counted is worse: it can lose ALL of them and no number
 * anywhere would move.
 */
export function shortfalls(
  root: SuiteLike,
  loaded: string[],
  listed: Array<[string, number]> = EDITOR_CELLS
): string[] {
  const counts = countRegistered(root);
  const problems: string[] = [];
  for (const [name, expected] of listed) {
    const found = counts.get(name) ?? 0;
    if (found < expected) {
      problems.push(
        `${name} registers ${found} cells and registered ${expected}. If cells were removed on ` +
          'purpose, lower the number in test/integration/census.ts and write which cell went and ' +
          'what covers what it covered.'
      );
    }
  }
  const numbered = new Set(listed.map(([name]) => name));
  for (const name of loaded) {
    if (!numbered.has(baseName(name))) {
      problems.push(
        `${baseName(name)} is loaded by the editor-hosted harness and has no number in ` +
          'test/integration/census.ts, so losing every cell in it would be silent.'
      );
    }
  }
  return problems;
}
