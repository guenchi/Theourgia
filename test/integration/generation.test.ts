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

  /*
   * C6 THE COUNT THAT WAS STILL IN FLIGHT. C5 above changes the settings
   * when nothing is outstanding, so it proves that `rebuild` forgets the
   * old count -- and nothing more. The check that matters is the one
   * AFTER the wait: an answer that arrives for the store the user has
   * just left must be dropped, not painted. Without it the sequence is
   * `rebuild` clears the count, then the late answer writes store A's
   * number under store B's name, which is worse than never clearing it
   * because it looks freshly fetched.
   *
   * THE WAIT IS A CORE REQUEST, so the stand-in can hold it open for as
   * long as the cell needs. That is what makes this one guardable at all
   * -- the waits inside `openBlock` are editor calls and are not.
   */
  it('C6 does not paint a conflict count that arrived for the store the user left', async () => {
    core = new FakeCore([
      { match: ['conflicts'], stdout: '(conflict "a.1" cycle)\n(orphan "a.9")\n', rc: 0, delayMs: 2500 },
      { match: ['read'], stdout: `${BLOCK}\n`, rc: 0 },
      { match: ['outline'], stdout: '', rc: 0 }
    ]);
    await useStore(core, `${core.store}-A`);
    await settle();

    const running = vscode.commands.executeCommand('theourgia.refreshOutline') as Promise<unknown>;
    await settle(300);
    await vscode.workspace
      .getConfiguration('theourgia')
      .update('store', `${core.store}-B`, vscode.ConfigurationTarget.Global);
    await running;
    await settle(300);

    /*
     * THE REQUEST HAS TO HAVE SUCCEEDED. `refreshConflicts` answers null
     * when the count could not be fetched at all, so a cell that only
     * checks for null passes whether the guard works or the request
     * simply failed -- and a stand-in whose delayed answer never arrived
     * would look exactly like a guard doing its job. The call is
     * required to have been made and to have been answered.
     */
    const conflictCalls = core.calls().filter((c) => c.coreArgv.includes('conflicts'));
    assert.ok(conflictCalls.length > 0, 'the conflicts request was never made');
    assert.ok(
      conflictCalls.some((c) => c.event === 'answer'),
      `the delayed request did not answer, so null proves nothing: ${JSON.stringify(
        conflictCalls.map((c) => c.event)
      )}`
    );

    const facts = await currentFacts();
    assert.strictEqual(facts.store, `${core.store}-B`, 'the settings did not actually change');
    assert.strictEqual(
      facts.conflicts,
      null,
      'a count fetched for the previous store was painted under the new one'
    );
  });
});
