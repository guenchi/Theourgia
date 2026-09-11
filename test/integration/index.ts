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

export function run(): Promise<void> {
  const mocha = new Mocha({ ui: 'bdd', color: true, timeout: 120000 });
  const here = __dirname;
  const files = fs.readdirSync(here).filter((f) => f.endsWith('.test.js'));
  if (files.length === 0) {
    return Promise.reject(new Error(`no editor-hosted cells were found in ${here}`));
  }
  for (const file of files) {
    mocha.addFile(path.join(here, file));
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
