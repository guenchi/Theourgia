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
 * The cells that need an editor: a block is opened into a buffer, and
 * saving that buffer reaches the store.
 *
 * THE STORE IS REAL HERE. A stand-in core would let this pass against a
 * script written by the same hand that wrote the extension; what is
 * being asked is whether the whole path -- tree, buffer, save, store --
 * carries the bytes, and only the core can answer that.
 */

import * as assert from 'assert';
import * as vscode from 'vscode';
import { RealStore } from '../support/real-core';

const DOC = '# Doc One\n\nintro\n\n## Two\nbody\n\n## Three  spaced\nb3\n';

async function settle(ms = 250): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

async function idOfTitle(store: RealStore, title: string): Promise<string> {
  const outline = await store.client.request('outline', []);
  for (const line of outline.text.split('\n')) {
    const at = line.indexOf('- ');
    if (at < 0) {
      continue;
    }
    const rest = line.slice(at + 2);
    const split = rest.indexOf('  ');
    if (split > 0 && rest.slice(split + 2) === title) {
      return rest.slice(0, split);
    }
  }
  throw new Error(`no row titled ${title} in ${outline.text}`);
}

describe('the extension inside an editor', function () {
  this.timeout(180000);
  let store: RealStore;

  before(async () => {
    store = await RealStore.make('vscode-host');
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-host', vscode.ConfigurationTarget.Global);
    const extension = vscode.extensions.getExtension('theourgia.theourgia');
    assert.ok(extension !== undefined, 'the extension is not installed in this host');
    await extension?.activate();
    await settle();
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('store', undefined, vscode.ConfigurationTarget.Global);
    await settings.update('corePath', undefined, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', undefined, vscode.ConfigurationTarget.Global);
    await settings.update('actor', undefined, vscode.ConfigurationTarget.Global);
    store?.dispose();
  });

  it('registers the commands it contributes', async () => {
    const commands = await vscode.commands.getCommands(true);
    for (const name of [
      'theourgia.refreshOutline',
      'theourgia.openBlock',
      'theourgia.retryOutbox',
      'theourgia.showStatus'
    ]) {
      assert.ok(commands.includes(name), `${name} was not registered`);
    }
  });

  it('opens a block into a markdown buffer holding its heading and body', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined, 'nothing was opened');
    assert.strictEqual(editor?.document.languageId, 'markdown');
    assert.strictEqual(editor?.document.getText(), '## Two\nbody\n\n');
  });

  it('opens the same block into the same document the second time', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const first = vscode.window.activeTextEditor?.document.uri.toString();
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const second = vscode.window.activeTextEditor?.document.uri.toString();
    assert.strictEqual(first, second, 'one block reached two buffers');
  });

  it('opens two different blocks into two different documents', async () => {
    const two = await idOfTitle(store, 'Two');
    const three = await idOfTitle(store, 'Three  spaced');
    await vscode.commands.executeCommand('theourgia.openBlock', two);
    await settle();
    const firstUri = vscode.window.activeTextEditor?.document.uri.toString();
    await vscode.commands.executeCommand('theourgia.openBlock', three);
    await settle();
    const secondUri = vscode.window.activeTextEditor?.document.uri.toString();
    assert.notStrictEqual(firstUri, secondUri);
    assert.strictEqual(vscode.window.activeTextEditor?.document.getText(), '## Three  spaced\nb3\n');
  });

  it('sends the body alone when the buffer is saved', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined);
    const document = (editor as vscode.TextEditor).document;

    const logBefore = await store.client.request('log', [id]);
    await (editor as vscode.TextEditor).edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Two\nedited in the editor\n'
      );
    });
    await document.save();
    await settle(1500);

    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(readBack.text, '## Two\nedited in the editor\n');
    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length + 1,
      'saving the buffer did not append exactly one record'
    );
  });

  it('sends nothing when the heading line is edited', async () => {
    const id = await idOfTitle(store, 'Three  spaced');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined);
    const document = (editor as vscode.TextEditor).document;

    const logBefore = await store.client.request('log', [id]);
    await (editor as vscode.TextEditor).edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Renamed\nb3\n'
      );
    });
    await document.save();
    await settle(1500);

    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length,
      'editing the heading sent a write'
    );
    assert.strictEqual(
      document.getText(),
      '## Renamed\nb3\n',
      'the refused save rolled the buffer back instead of leaving it alone'
    );
  });
});
