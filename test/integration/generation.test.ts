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
 * What happens to a request that is in flight when the settings change.
 *
 * THE STAND-IN CORE IS USED HERE AND NOT THE REAL ONE, because the thing
 * being arranged is a moment: a read that is still running while the
 * store setting is replaced. A real core answers when it answers; a
 * stand-in can be told to take a second, which is what makes the window
 * wide enough to step into.
 *
 * THE ENVIRONMENT IS SET ON THIS PROCESS because the extension spawns
 * the core with `process.env` as its base, and it reads that when the
 * client is built -- which is on every settings change, so a variable
 * set before the change reaches the child.
 */

import * as assert from 'assert';
import * as vscode from 'vscode';
import { FakeCore } from '../support/fake';
import { StatusFacts } from '../../src/status';

const BLOCK =
  '(ok ((id . "a.2") (deleted . #f) (fields (heading-src . "## Two\\n") (src . "body\\n") (title . "Two")) (position root . 0) (edges)))';

async function settle(ms = 250): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

async function useStore(core: FakeCore, store: string): Promise<void> {
  const settings = vscode.workspace.getConfiguration('theourgia');
  process.env.FAKE_CORE_SCRIPT = core.scriptFile;
  process.env.FAKE_CORE_LOG = core.logFile;
  await settings.update('scheme', core.config().scheme, vscode.ConfigurationTarget.Global);
  await settings.update('corePath', core.corePath, vscode.ConfigurationTarget.Global);
  await settings.update('libDirs', [], vscode.ConfigurationTarget.Global);
  await settings.update('store', store, vscode.ConfigurationTarget.Global);
  await settings.update('actor', 'generation-cell', vscode.ConfigurationTarget.Global);
}

async function currentFacts(): Promise<StatusFacts> {
  return (await vscode.commands.executeCommand('theourgia.showStatus', { ask: false })) as StatusFacts;
}

describe('a setting that changes while a request is in flight', function () {
  this.timeout(180000);
  let core: FakeCore;

  before(async () => {
    const extension = vscode.extensions.getExtension('theourgia.theourgia');
    assert.ok(extension !== undefined);
    await extension?.activate();
  });

  afterEach(async () => {
    core?.dispose();
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    delete process.env.FAKE_CORE_SCRIPT;
    delete process.env.FAKE_CORE_LOG;
    await settle();
  });

  it('C1 does not open a block into the store that replaced the one it was read from', async () => {
    core = new FakeCore([
      { match: ['read', 'a.2'], stdout: `${BLOCK}\n`, rc: 0, delayMs: 2500 },
      { match: ['conflicts'], stdout: '', rc: 0 },
      { match: ['check'], stdout: '(check (writers (("w" (end 1) (torn #f) (integrity ())))) (verdict ok))\n', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    const before = vscode.workspace.textDocuments.map((d) => d.uri.toString()).sort();
    const opening = vscode.commands.executeCommand('theourgia.openBlock', 'a.2');
    await settle(400);

    /*
     * The read is still running. The store setting is replaced under it.
     */
    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    await opening;
    await settle(500);

    const after = vscode.workspace.textDocuments.map((d) => d.uri.toString()).sort();
    assert.deepStrictEqual(
      after,
      before,
      'a block read from one store was opened after the settings named another'
    );
    const facts = await currentFacts();
    assert.strictEqual(facts.store, `${core.store}-B`, 'the settings did not actually change');
  });

  it('C5 does not carry one store\'s conflict count over to another', async () => {
    core = new FakeCore([
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n(orphan "a.9")\n', rc: 0 },
      { match: ['read'], stdout: `${BLOCK}\n`, rc: 0 },
      { match: ['outline'], stdout: '', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    await vscode.commands.executeCommand('theourgia.refreshOutline');
    await settle(600);
    const asked = await currentFacts();
    assert.strictEqual(asked.conflicts, 2, `the count for store A was ${asked.conflicts}`);

    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    await settle(400);

    const carried = await currentFacts();
    assert.strictEqual(carried.store, `${core.store}-B`);
    assert.strictEqual(
      carried.conflicts,
      null,
      'a store nobody has asked was shown with the previous store\'s conflict count'
    );
  });
});
