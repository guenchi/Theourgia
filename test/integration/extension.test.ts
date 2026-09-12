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
import * as fs from 'fs';
import * as vscode from 'vscode';
import { StoreModel } from '../../src/model';
import { stringField } from '../../src/blocks';
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

  /*
   * THE LIST COMES FROM THE MANIFEST THE HOST LOADED, not from a list
   * written here. A list written here is silent about the command
   * somebody added and never registered, and that silence is what let a
   * refusal tell the user to run "Reconcile Block" for as long as it
   * did: nothing declared it, nothing registered it, and this cell --
   * naming four commands, all of which were fine -- passed.
   */
  it('registers every command its manifest declares', async () => {
    const extension = vscode.extensions.getExtension('theourgia.theourgia');
    const declared = (
      extension?.packageJSON?.contributes?.commands as Array<{ command: string }> | undefined
    )?.map((c) => c.command);
    assert.ok(declared !== undefined && declared.length > 0, 'the manifest declares no commands');
    const commands = await vscode.commands.getCommands(true);
    const missing = declared.filter((name) => !commands.includes(name));
    assert.deepStrictEqual(missing, [], 'these commands are in the palette and do nothing');
  });

  /*
   * THE RECOVERY COMMAND REACHES ITS HANDLER, AND CLOSING THE LIST DOES
   * NOTHING AT ALL.
   *
   * The unit cells drive `chooseAndRecover` directly, which is what the
   * registered handler calls in one line. What only a host can answer is
   * whether the command exists, whether invoking it arrives there, and
   * whether the editor's own quick pick -- the one part of this that
   * cannot be exercised outside a host -- reports a dismissal as the
   * flow expects.
   *
   * ⚠️ AND IT HAS TO DISMISS THE LIST. The first version of this cell
   * assumed the host would have no other sessions and the flow would
   * take its first exit. It does not: every earlier run of this suite
   * left a session directory in the host's storage, so the list opens
   * and waits on a person -- 180 seconds of it. A cell that waits on a
   * person is a cell that reports a timeout.
   */
  it('runs the recovery command, and dismissing its list does nothing', async () => {
    const running = vscode.commands.executeCommand('theourgia.otherSessions') as Promise<{
      did: string;
      because?: string;
    }>;
    await settle(1500);
    await vscode.commands.executeCommand('workbench.action.closeQuickOpen');
    const outcome = await running;
    assert.ok(outcome !== undefined, 'the command returned nothing, so it decided nothing');
    assert.strictEqual(
      outcome.did,
      'nothing',
      `dismissing the list acted on something: ${JSON.stringify(outcome)}`
    );
    /*
     * ⚠️ WHAT THIS CELL DOES NOT ESTABLISH: that nothing moved on disk.
     * It knows the command's answer and not the host's storage path, and
     * inventing one would be asserting about a directory chosen by
     * guesswork. The disk-level claim -- no claim token, no directory
     * moved -- is made by the unit cells, which create the storage they
     * then look at.
     */
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

  /*
   * X1c REPLACED THIS CELL'S SUBJECT.
   *
   * It used to require that opening a block twice reached ONE document,
   * because the file was rewritten in place from the store. Publication
   * is immutable now: a second open publishes `<n+1>.md` and shows that,
   * and the version that was there stays exactly as it was. That is not
   * a regression -- it is the property that removes the whole class of
   * "another reading replaced my baseline" (S1, S4), because there is
   * nothing to replace.
   *
   * THE SEQUENCE THE OLD CELL GUARDED is now covered by two things: the
   * note in sequences.test.ts under S1, and the assertion below that the
   * earlier version is untouched -- which is the part that actually
   * mattered about "one block, one document".
   */
  it('publishes a new version the second time and leaves the first alone', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const first = vscode.window.activeTextEditor?.document.uri.fsPath;
    assert.ok(first !== undefined, 'the first open put nothing in front of the user');
    const firstBytes = fs.readFileSync(first as string, 'utf8');

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const second = vscode.window.activeTextEditor?.document.uri.fsPath;
    assert.ok(second !== undefined, 'the second open put nothing in front of the user');
    assert.notStrictEqual(second, first, 'the second reading was written over the first');
    assert.strictEqual(
      fs.readFileSync(first as string, 'utf8'),
      firstBytes,
      'opening the block again changed the version that was already there'
    );
    assert.strictEqual(
      vscode.window.activeTextEditor?.document.getText(),
      '## Two\nbody\n\n',
      'the second version does not hold the block'
    );
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
    /*
     * AND THE BLOCK ITSELF IS UNTOUCHED. An unchanged log length says
     * only that nothing was RECORDED; a request the store answered
     * without appending satisfies it just as well as a request that was
     * never made. Reading the block back asks the question the cell is
     * named for from the other side.
     */
    assert.strictEqual(
      (await store.client.request('read', [id, '--md'])).text,
      '## Three  spaced\nb3\n',
      'the heading edit reached the store'
    );
    assert.strictEqual(
      document.getText(),
      '## Renamed\nb3\n',
      'the refused save rolled the buffer back instead of leaving it alone'
    );
  });

  it('O5 does not overwrite a buffer that has unsaved changes', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    const document = editor.document;
    const opened = document.getText();

    await editor.edit((builder) => {
      builder.insert(document.positionAt(document.getText().length), 'typed but not saved\n');
    });
    assert.ok(document.isDirty, 'the edit did not leave the buffer dirty');
    const typed = document.getText();

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    assert.strictEqual(
      document.getText(),
      typed,
      'opening the block again threw away work that had not been saved'
    );
    assert.notStrictEqual(typed, opened);

    /*
     * And the baseline did not move either: saving now must still be
     * measured against the heading the buffer was given, so the body --
     * not the whole buffer -- is what reaches the store.
     */
    const logBefore = await store.client.request('log', [id]);
    await document.save();
    await settle(1500);
    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(readBack.text, typed);
    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(logAfter.answers.length, logBefore.answers.length + 1);
  });

  it('S8 sends LF when the editor is writing CRLF and the block holds none', async () => {
    const id = await idOfTitle(store, 'Three  spaced');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    const document = editor.document;

    await editor.edit((builder) => {
      builder.setEndOfLine(vscode.EndOfLine.CRLF);
    });
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Three  spaced\nline one\nline two\n'
      );
    });
    assert.ok(document.getText().includes('\r\n'), 'the buffer is not holding CRLF');

    await document.save();
    await settle(1500);

    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(
      readBack.text,
      '## Three  spaced\nline one\nline two\n',
      'the carriage returns the editor added reached the store'
    );
  });

  it('does not overwrite a file holding a save this client refused', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    const document = editor.document;

    /*
     * A heading edit is refused, and the file has ALREADY been written
     * by the time the refusal is heard -- so the buffer is clean and
     * holds work the store has not got. That is the state the dirty flag
     * cannot see.
     *
     * THE REFUSAL HERE IS THIS CLIENT'S, NOT THE STORE'S. A changed
     * prefix is turned away by `splitDocument` before the Saver is
     * reached, so nothing is sent -- which is what this cell observes.
     * A save the CORE refused travels a different path and is not
     * covered by anything here: an implementation that kept locally
     * refused edits and overwrote ones the store turned down would pass.
     * The cell used to be named for that second path, which it has
     * never exercised. KNOWN OPEN, recorded in the README.
     */
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Renamed by hand\nand a body change too\n'
      );
    });
    await document.save();
    await settle(1500);
    assert.ok(!document.isDirty, 'the buffer is dirty, so this cell is not testing what it says');
    const refused = document.getText();

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle(800);
    assert.strictEqual(
      document.getText(),
      refused,
      'reopening the block threw away a save the core had refused'
    );
  });
});

describe('a document block with front matter, edited twice', function () {
  this.timeout(180000);
  let store: RealStore;

  /*
   * THE BASELINE AFTER A SAVE IS THE WHOLE PREFIX. Rebuilding it from
   * the heading alone drops the front matter, and the damage only shows
   * on the SECOND save: with the body emptied, the baseline then holds
   * no carriage return, so a CRLF buffer is normalised whole and the
   * untouched front matter stops matching. An ordinary body edit is
   * refused for a change nobody made.
   *
   * A UNIT CELL COULD NOT CATCH THIS. The rebuild lives in the extension
   * beside the editor, and a cell that reimplements it is checking its
   * own copy -- mutating the real line left every unit cell green.
   */
  before(async () => {
    store = await RealStore.make('vscode-front');
    await store.importMarkdown(
      'front.md',
      '---\r\ntitle: carried in the front matter\r\n---\r\n\r\nthe body\r\n'
    );
    /*
     * EVERY SETTING, not only the store. The suite above clears them all
     * in its teardown, and a describe that set only the store left the
     * extension with no core path -- so `openBlock` warned and returned,
     * and `activeTextEditor` was still the previous test's document. The
     * cell then failed for a reason that had nothing to do with it.
     */
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-front', vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settle(800);
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    store?.dispose();
  });

  it('accepts a body typed back in after the body was emptied', async () => {
    const outline = await store.client.request('outline', []);
    const fileBlock = outline.text.split('\n')[0].replace(/^- /, '').split('  ')[0];

    await vscode.commands.executeCommand('theourgia.openBlock', fileBlock);
    await settle(800);
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    assert.ok(editor !== undefined, 'the document block did not open');
    const document = editor.document;

    const opened = document.getText();
    /*
     * THE FIXTURE IS ASSERTED, NOT ASSUMED. This cell only says anything
     * if the buffer really arrives with CRLF front matter -- a fixture
     * that lost its carriage returns on the way in would make every
     * assertion below pass for the wrong reason.
     */
    const front = '---\r\ntitle: carried in the front matter\r\n---\r\n';
    assert.strictEqual(
      opened,
      `${front}\r\nthe body\r\n`,
      'the buffer is not the bytes the store holds for this block'
    );

    /*
     * Empty the body, keeping the front matter exactly as it came.
     */
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        front
      );
    });
    await document.save();
    await settle(1500);
    /*
     * THE FIRST SAVE HAS TO HAVE LANDED AND EMPTIED THE BODY, or the
     * second one is measured against a baseline that never moved and the
     * cell passes without exercising the rebuild at all.
     */
    const emptied = await new StoreModel(store.client).blockOf(fileBlock);
    assert.ok(emptied !== null);
    assert.strictEqual(
      stringField(emptied as NonNullable<typeof emptied>, 'src'),
      '',
      'the first save did not empty the body, so the second proves nothing'
    );

    const logBefore = await store.client.request('log', [fileBlock]);
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        `${front}a body typed back in\n`
      );
    });
    await document.save();
    await settle(1500);

    const logAfter = await store.client.request('log', [fileBlock]);
    assert.strictEqual(
      logAfter.answers.length,
      logBefore.answers.length + 1,
      'the second save was refused, although only the body had changed'
    );
    /*
     * THE WHOLE BYTES, not two substrings. A save that duplicated the
     * front matter into the body, or dropped a carriage return, would
     * still contain both phrases.
     */
    const readBack = await store.client.request('read', [fileBlock, '--md']);
    /*
     * CRLF, NOT LF. This block's own bytes contain carriage returns, so
     * the normalisation does not apply -- it exists only for a block
     * that holds none and an editor that writes CRLF anyway -- and the
     * editor put the buffer's own line ending on the text typed into it.
     * The first expectation written here said `\n`, which was where the
     * keystrokes went rather than what the rule says.
     */
    assert.strictEqual(
      readBack.text,
      `${front}a body typed back in\r\n`,
      'the block is not exactly the front matter followed by the new body'
    );
    const block = await new StoreModel(store.client).blockOf(fileBlock);
    assert.ok(block !== null);
    assert.strictEqual(
      stringField(block as NonNullable<typeof block>, 'front'),
      front,
      'the front matter was changed by a save that only touched the body'
    );
    assert.strictEqual(
      stringField(block as NonNullable<typeof block>, 'src'),
      'a body typed back in\r\n',
      'the front matter ended up inside the body as well'
    );
  });
});

/*
 * WHAT A RETRY REPORTS WHEN THE SETTINGS MOVE UNDER IT.
 *
 * THE COMMAND IS AN AWAIT, AND `saver` IS REBUILT ON EVERY SETTINGS
 * CHANGE. Reading it again after the wait reported whatever store was
 * configured by then: one store's outcomes beside another store's
 * count, and -- if that other queue could not be read -- the count was
 * the word "null". Six other places in this file take the generation
 * before waiting and check it after; this one did not, and nothing in
 * the suite looked at any of them.
 *
 * THE ENTRY IS STRANDED WITH A TIMEOUT, which is the one answer the
 * saver must KEEP: a save whose outcome nobody knows is exactly what
 * the queue is for. That gives the retry something to send, and the
 * send is a real subprocess, which is the window the settings change
 * has to land in.
 */
describe('a retry whose store changed while it was in flight', function () {
  this.timeout(180000);
  let store: RealStore;
  let other: RealStore;

  before(async () => {
    store = await RealStore.make('vscode-retry');
    other = await RealStore.make('vscode-retry-other');
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-retry', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settle();
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor', 'timeoutMs']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    store?.dispose();
    other?.dispose();
  });

  async function strandOneSave(): Promise<void> {
    const settings = vscode.workspace.getConfiguration('theourgia');
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await settle();
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined, 'nothing was opened');
    /*
     * ONE ORDINARY SAVE FIRST, so that the cursor is known. `ensureCursor`
     * asks the store only while it has none, and under a one-millisecond
     * timeout that request is the one that fails -- the save is then
     * refused before anything is written down, and nothing is stranded.
     * The cell is about a save that reached the queue and not the store.
     */
    await editor?.edit((b) => b.insert(new vscode.Position(1, 0), 'first\n'));
    await editor?.document.save();
    await settle(1500);
    await settings.update('timeoutMs', 1, vscode.ConfigurationTarget.Global);
    await settle();
    await editor?.edit((b) => b.insert(new vscode.Position(1, 0), 'stranded\n'));
    await editor?.document.save();
    await settle(1500);
    await settings.update('timeoutMs', undefined, vscode.ConfigurationTarget.Global);
    await settle();
    const facts = (await vscode.commands.executeCommand('theourgia.showStatus', {
      ask: false
    })) as { pending: number | null };
    assert.strictEqual(facts.pending, 1, 'no save was stranded, so there is nothing to retry');
  }

  it('reports nothing rather than reporting it against the store that replaced it', async () => {
    await strandOneSave();
    const settings = vscode.workspace.getConfiguration('theourgia');
    const running = vscode.commands.executeCommand('theourgia.retryOutbox') as Promise<unknown>;
    await settings.update('store', other.store, vscode.ConfigurationTarget.Global);
    const reported = await running;
    /*
     * THE SETTING GOES BACK BEFORE THE ASSERTION. Restoring it afterwards
     * made the next cell depend on this one passing: a failure here left
     * the other store configured, and the cell below failed too, which
     * reads as two defects and is one.
     */
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settle();
    assert.strictEqual(
      reported,
      null,
      `a retry against ${store.store} reported after the store became ${other.store}: ` +
        `${JSON.stringify(reported)}`
    );
  });

  it('still reports when the settings did not move', async () => {
    const reported = (await vscode.commands.executeCommand('theourgia.retryOutbox')) as {
      text: string;
    } | null;
    assert.ok(reported !== null, 'a retry nothing interrupted reported nothing');
    assert.ok(
      reported.text.includes(store.store),
      `the report does not name the store it is about: ${reported.text}`
    );
  });
});

/*
 * SETTINGS THAT DO NOT WORK ARE NOT A QUEUE THAT IS EMPTY.
 *
 * WHEN THE SETTINGS ARE UNUSABLE the extension drops its outbox, and the
 * facts it reports said `pending: 0` -- "nothing waiting to be saved".
 * But the queue's location is derived from the store that is no longer
 * known, so nothing has been read and nothing can be: what is waiting is
 * unknown, and saves that were waiting are still on disk. This is the
 * fourth place in this build where a question that could not be put was
 * drawn as the reassuring answer.
 */
describe('what the status reports when the settings are unusable', function () {
  this.timeout(180000);
  let store: RealStore;

  before(async () => {
    store = await RealStore.make('vscode-broken');
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-broken', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settle();
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    store?.dispose();
  });

  it('does not say nothing is waiting when it has not been able to look', async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('store', '', vscode.ConfigurationTarget.Global);
    await settle();
    const facts = (await vscode.commands.executeCommand('theourgia.showStatus', {
      ask: false
    })) as { pending: number | null };
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settle();
    assert.strictEqual(
      facts.pending,
      null,
      'a queue whose location is unknown was reported as holding nothing'
    );
  });

  it('reports a number again once the settings work', async () => {
    const facts = (await vscode.commands.executeCommand('theourgia.showStatus', {
      ask: false
    })) as { pending: number | null };
    assert.strictEqual(facts.pending, 0, 'a readable and empty queue was not reported as empty');
  });
});

