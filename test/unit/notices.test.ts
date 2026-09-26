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
 * AN INFORMATION NOTICE AND AN ERROR NOTICE REACH THE EDITOR. (queue item 39)
 *
 * Found by item 33's closing review: the extension's `show` could stop
 * calling `showInformationMessage` with every cell green -- 1140/0 on the
 * whole suite -- because no schedule scenario produced an information
 * notice. These two drive the activated extension (the editor and the core
 * stood in for, test/support/extension-schedules.js) through one save each:
 * one the store accepts and answers with a `behind` clause, whose notice is
 * information; one the store refuses, whose notice is an error. Each asks
 * for the notice by its level AND its text, so a notice routed to another
 * level, or another notice at this level, does not pass.
 */

import * as assert from 'assert';
import * as path from 'path';
import { spawnSync } from 'child_process';

function run(name: string): { shown: Array<{ text: string; level: string }>; requests: number } {
  const child = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
    encoding: 'utf8',
    timeout: 20000
  });
  assert.strictEqual(child.status, 0, child.stdout + child.stderr);
  const result = JSON.parse(child.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete, true);
  return result.result;
}

describe('plugin-r3 39 the editor is told at the level the notice has', function () {
  this.timeout(30000);

  it('shows who else landed as an information notice after a save the store accepted', () => {
    const r = run('notice-behind');
    assert.strictEqual(r.requests, 1, 'the save never reached the store, so there was no answer to read');
    const information = r.shown.filter((n) => n.level === 'information');
    assert.strictEqual(information.length, 1, `not one information notice: ${JSON.stringify(r.shown)}`);
    assert.ok(information[0].text.startsWith('theourgia: '), information[0].text);
    assert.ok(information[0].text.includes('other has 2 records'), `the notice does not name who landed: ${information[0].text}`);
    assert.ok(!information[0].text.includes('w has'), `the notice names this save's own writer: ${information[0].text}`);
  });

  it('shows a refused save as an error notice that names the block and keeps the text', () => {
    const r = run('notice-refused');
    assert.strictEqual(r.requests, 1, 'the save never reached the store, so there was no refusal to read');
    const errors = r.shown.filter((n) => n.level === 'error');
    assert.strictEqual(errors.length, 1, `not one error notice: ${JSON.stringify(r.shown)}`);
    assert.ok(errors[0].text.startsWith('theourgia: a.1: '), `the notice does not name the block: ${errors[0].text}`);
    assert.ok(errors[0].text.includes('cursor-unreachable'), `the notice lost the store's reason: ${errors[0].text}`);
    assert.ok(
      errors[0].text.endsWith('Your text is still in the file and has not been saved to the store.'),
      `the notice does not say what became of the text: ${errors[0].text}`
    );
  });
});
