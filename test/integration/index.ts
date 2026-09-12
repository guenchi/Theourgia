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
 * The mocha run that happens inside the editor.
 *
 * THE LIST OF FILES IS READ FROM THE DIRECTORY AND ITS LENGTH IS
 * CHECKED. A glob that matched nothing would finish with no failures,
 * which reads exactly like a pass.
 */

import * as fs from 'fs';
import * as path from 'path';
import Mocha = require('mocha');
import { shortfalls } from './census';

export async function run(): Promise<void> {
  /*
   * ⚠️ `forbidOnly` AND `forbidPending`, BECAUSE THE COUNT CANNOT SEE
   * THEM. A `describe.only` leaves every cell REGISTERED and runs three
   * of seventeen; a `describe.skip` leaves them registered and runs none
   * of that suite. The census below counts registrations, so both walk
   * straight past it and the run reports no failures -- which reads
   * exactly like a pass, and is the shape this whole guard exists for.
   * Found in review.
   */
  const mocha = new Mocha({
    ui: 'bdd',
    color: true,
    timeout: 120000,
    forbidOnly: true,
    forbidPending: true
  });
  const here = __dirname;
  const files = fs.readdirSync(here).filter((f) => f.endsWith('.test.js'));
  if (files.length === 0) {
    return Promise.reject(new Error(`no editor-hosted cells were found in ${here}`));
  }
  for (const file of files) {
    mocha.addFile(path.join(here, file));
  }
  /*
   * THE CELLS ARE COUNTED BEFORE THEY ARE RUN. A suite that lost its
   * cells finishes with no failures, which reads exactly like a pass;
   * refusing to start says something different from a failing cell, and
   * that is the point. Counting needs the files loaded, which is what
   * `loadFilesAsync` does -- `mocha.run` would do it too, and too late.
   */
  await mocha.loadFilesAsync();
  const problems = shortfalls(mocha.suite, files);
  if (problems.length > 0) {
    return Promise.reject(new Error(`the editor-hosted suite is not whole:\n${problems.join('\n')}`));
  }
  return new Promise((resolve, reject) => {
    mocha.run((failures) => {
      if (failures > 0) {
        reject(new Error(`${failures} editor-hosted cell(s) failed`));
        return;
      }
      resolve();
    });
  });
}
