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
import * as path from 'path';
import * as vscode from 'vscode';
import { StoreModel } from '../../src/model';
import { stringField } from '../../src/blocks';
import {
  RealStore,
  daemonsMatching,
  socketPathOf,
  stopDaemonsFor
} from '../support/real-core';
import { settle, until } from '../support/until';

const DOC = '# Doc One\n\nintro\n\n## Two\nbody\n\n## Three  spaced\nb3\n';

/*
 * NOTE: THE FIXTURE'S STORES SHARE THE HOST'S RUN ROOT, deliberately.
 *
 * The extension under test was given `THEOURGIA_RUN` by the launcher. A
 * fixture that made its own would put the SAME store's socket under two
 * roots: two sockets for one key, two daemons opening one store, and
 * from inside a cell that shows up as a refusal with no cause anybody
 * can name. NEVER: It is read here rather than in `RealStore`, which must go
 * on making its own when nobody says otherwise -- a fixture that quietly
 * inherited an ambient variable would make the unit suite's run-root
 * gate compare a directory with itself.
 */
function hostRunRoot(): string {
  const given = process.env.THEOURGIA_RUN;
  assert.ok(
    given !== undefined && given.length > 0,
    'the launcher did not pass THEOURGIA_RUN into the extension host, so these cells and the ' +
      'extension would reach two different daemons for one store'
  );
  return given as string;
}

/*
 * KEY: WAITING FOR SOMETHING TO HAPPEN, RATHER THAN FOR A WHILE.
 *
 * NOTE: WHAT THE FIXED WAITS COST, MEASURED. Every one of these cells used
 * to sleep a flat 1500 ms after a save and then assert. That number was
 * chosen when a request cost about 460 ms because the core was read from
 * source; through a daemon the same request costs about 30 ms
 * (design 7.6.53). One cell in this file went red on the switch for the
 * opposite reason -- it slept while a background drain it did not know
 * about finished, and asserted that a save was still stranded after the
 * extension had quietly got it through.
 *
 * `until` and the reasons for it now live in `test/support/until.ts`,
 * because the other editor-hosted file was still sleeping and a second
 * copy of this would have been the one nobody fixed.
 */

/*
 * HOW MANY RECORDS THIS BLOCK'S LOG HOLDS. Read through the fixture's
 * own client, which does not go through the extension: a count taken
 * from the thing under test would be the thing under test agreeing with
 * itself.
 */
/*
 * THE FACTS THIS EXTENSION REPORTS, ONCE THEY SAY WHAT IS BEING WAITED
 * FOR. A settings change reaches a handler, and the handler is not the
 * caller of `update`; the readable sign that it ran is the facts
 * changing.
 */
interface Facts {
  store: string;
  actor: string;
  pending: number | null;
  blocked: string | null;
  unreachable: string | null;
}

/*
 * THE EXTENSION HAS TAKEN THE SETTINGS IT WAS JUST GIVEN.
 *
 * NOTE: A SETTING IS DELIVERED TO A HANDLER, and the caller of `update` is
 * not that handler. Sleeping afterwards asserts a duration; what is
 * waited for here is the extension reporting the store and the actor it
 * was given, which one `readConfig` takes together -- so an actor that
 * has arrived is a rebuild that has run, and everything else in the same
 * batch arrived with it.
 */
/*
 * THE BLOCK IS OPEN IN THE ACTIVE EDITOR.
 *
 * NOTE: THE COMMAND IS AWAITED AND THAT IS NOT THE SAME THING. Its handler
 * awaits `showTextDocument`, so today the document is there when the
 * command resolves -- and a cell that sleeps 250 ms afterwards is
 * asserting a duration about a step it could simply look at. The file a
 * block is opened into carries the block's id in its path, which is what
 * makes "the right block" a question with an answer.
 */
async function untilOpen(id: string): Promise<void> {
  await until(
    () =>
      `${id} was opened; the editor holds ` +
      `${vscode.window.activeTextEditor?.document.uri.fsPath ?? 'nothing'}`,
    () => (vscode.window.activeTextEditor?.document.uri.fsPath ?? '').includes(id)
  );
}

async function settingsTaken(store: string, actor: string): Promise<void> {
  await untilStatus(
    `the extension took store ${store} and actor ${actor}`,
    (facts) => facts.store === store && facts.actor === actor
  );
}

async function untilStatus(what: string, ready: (facts: Facts) => boolean): Promise<Facts> {
  let seen: Facts | null = null;
  await until(
    () => `${what}; last seen ${JSON.stringify(seen)}`,
    async () => {
      seen = (await vscode.commands.executeCommand('theourgia.showStatus', {
        ask: false
      })) as Facts;
      return ready(seen);
    }
  );
  return seen as unknown as Facts;
}

async function logLength(store: RealStore, id: string): Promise<number> {
  const answer = await store.client.request('log', [id]);
  /*
   * NEVER: A REFUSAL IS NOT A COUNT. Found in a second review round: this
   * returned `answers.length` whatever the exit code, and a refusal is
   * one datum -- `(error serve-path-occupied ...)` -- so two failed
   * reads compare equal and an assertion that nothing was appended
   * passes without the log having been read at all. That is exactly the
   * situation the cell about a daemon that cannot start is in.
   */
  assert.strictEqual(
    answer.ok,
    true,
    `the store would not answer log ${id}, so this is not a count of anything: ${answer.text.trim()}`
  );
  return answer.answers.length;
}

async function untilLogGrows(store: RealStore, id: string, from: number): Promise<void> {
  await until(`the log of ${id} grew past ${from} records`, async () => {
    return (await logLength(store, id)) > from;
  });
}

/*
 * NOTE: AND THE CELLS THAT ASSERT NOTHING HAPPENED STILL NEED A DURATION.
 *
 * There is nothing to wait for when the right behaviour is silence: the
 * extension decides not to send, shows a sentence, and returns, and
 * nothing it leaves behind distinguishes "it decided not to" from "it
 * has not decided yet". So this keeps a period -- KEY: but it WATCHES that
 * period instead of sleeping through it, and fails the moment the record
 * it forbids appears, naming it. A cell that slept and then looked would
 * report the same failure a second and a half later and would say only
 * that the total was wrong.
 *
 * NEVER: THE NUMBER IS NOT A GUESS ABOUT HOW LONG A SAVE TAKES, which is what
 * the flat waits it replaces were. It is how long this cell is willing
 * to watch, and it may be generous: the only cost of a larger one is
 * that a passing cell takes longer.
 */
const SILENCE_MS = 2000;

async function stayedAt(store: RealStore, id: string, count: number): Promise<void> {
  const deadline = Date.now() + SILENCE_MS;
  while (Date.now() < deadline) {
    const now = await logLength(store, id);
    assert.strictEqual(
      now,
      count,
      `a record was appended to the log of ${id} (${count} -> ${now}) when this cell requires ` +
        'that nothing be sent'
    );
    await settle(50);
  }
}

/*
 * EVERY ENTRY THIS WINDOW HAS WRITTEN, READ OFF THE DISK.
 *
 * The path comes from the launcher rather than being rebuilt here: a
 * cell that worked out for itself where the extension writes would be a
 * second opinion about it, and the reading that is wrong is the
 * reassuring one -- an empty directory looks exactly like a queue with
 * nothing in it.
 */
function queueEntries(): Array<{ state: string; id: string; lastError: string | null }> {
  const storage = process.env.THEOURGIA_TEST_STORAGE;
  assert.ok(
    storage !== undefined && storage.length > 0,
    'THEOURGIA_TEST_STORAGE is not set, so this cell cannot read the queue it is about'
  );
  const sessions = path.join(storage as string, 'sessions');
  if (!fs.existsSync(sessions)) {
    return [];
  }
  const out: Array<{ state: string; id: string; lastError: string | null }> = [];
  for (const session of fs.readdirSync(sessions)) {
    const directory = path.join(sessions, session);
    if (!fs.statSync(directory).isDirectory()) {
      continue;
    }
    for (const store of fs.readdirSync(directory)) {
      const file = path.join(directory, store, 'outbox.json');
      if (!fs.existsSync(file)) {
        continue;
      }
      let held: { entries?: Array<{ state: string; id: string; lastError: string | null }> };
      try {
        held = JSON.parse(fs.readFileSync(file, 'utf8')) as {
          entries?: Array<{ state: string; id: string; lastError: string | null }>;
        };
      } catch (e) {
        /*
         * A HALF-WRITTEN QUEUE IS NOT AN EMPTY ONE, and this is read
         * while the extension is writing it. Reporting nothing for this
         * file would make a waiter below give up on a state that was
         * there; the caller polls, so being unable to read it now is
         * answered by reading it again.
         */
        continue;
      }
      for (const entry of held.entries ?? []) {
        out.push(entry);
      }
    }
  }
  return out;
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
    store = await RealStore.make('vscode-host', { runRoot: hostRunRoot() });
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
    await settingsTaken(store.store, 'vscode-host');
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
  /*
   * THE LAUNCHING PROCESS'S EDITOR VARIABLES DO NOT REACH THIS HOST (queue
   * item 30). test/integration/run.ts plants this decoy and then removes every
   * VSCODE_* and ELECTRON_* key before it launches the editor; a runner that
   * did not remove them would hand this host the decoy -- and, from a terminal
   * inside the user's editor, the user's profile.
   */
  it('sees no editor variable of the process that launched it', () => {
    assert.strictEqual(
      process.env.VSCODE_THEOURGIA_DECOY,
      undefined,
      'the runner handed the test host a VSCODE_ variable it had planted to be removed'
    );
  });

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
   * NOTE: AND IT EXPECTS AN EMPTY LIST, WHICH THE RUNNER MAKES TRUE. The
   * first version of this cell assumed that and was wrong: every earlier
   * run left a session directory in the host's storage and nothing
   * reclaims them, so the list opened and waited on a person -- 180
   * seconds of it. The runner now empties that storage before it starts
   * and refuses to start if it could not, so "this host has one session"
   * is a guarantee rather than a hope, and the cell says which exit it
   * expects instead of accepting either.
   *
   * THE DISMISSAL STAYS. If a session does turn up the list opens, and a
   * cell that waits on a person is a cell that reports a timeout; this
   * way it reports the assertion below instead, which says what
   * happened.
   */
  it('runs the recovery command, and finds no other window in a fresh host', async () => {
    /*
     * NEVER: THIS CELL DOES NOT EXERCISE A DISMISSAL, and it used to say it
     * did.
     *
     * Measured in a review round: `chooseAndRecover` returns
     * `{did: 'nothing', because: 'no-other-sessions'}` BEFORE it asks the
     * chooser anything (src/recovery.ts), so in a host whose storage the
     * runner has just emptied no picker is ever opened. The cell sat on a
     * 1500 ms wait and then closed a quick pick that was not there, and a
     * probe that made the cancellation branch throw still produced the
     * expected outcome with zero calls to the picker. The name promised
     * what nothing here could establish.
     *
     * What it does establish is worth keeping and is what it is now named
     * for: the command runs, decides, and says what it decided -- in a
     * host with nothing to recover, that nothing was found. Dismissal is
     * covered where the chooser can be driven, in `recovery.test.ts`.
     */
    const outcome = (await vscode.commands.executeCommand(
      'theourgia.otherSessions'
    )) as { did: string; because?: string };
    assert.ok(outcome !== undefined, 'the command returned nothing, so it decided nothing');
    assert.deepStrictEqual(
      outcome,
      { did: 'nothing', because: 'no-other-sessions' },
      'the recovery command found another window in a host the runner had just emptied'
    );
    /*
     * NOTE: WHAT THIS CELL DOES NOT ESTABLISH: that nothing moved on disk.
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
    await untilOpen(id);
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
  it('XC reopens the current document with the same verified body and prefix', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
    const first = vscode.window.activeTextEditor?.document.uri.fsPath;
    assert.ok(first !== undefined, 'the first open put nothing in front of the user');
    const firstBytes = fs.readFileSync(first as string, 'utf8');

    /*
     * KEY: SOMETHING ELSE IS PUT IN FRONT FIRST, so that the second open
     * has work to do.
     *
     * Measured in a review round: both waits accepted the document the
     * first open had left active, so an `openBlock` that returned at once
     * when its block was already showing satisfied every assertion here
     * -- the cell was about reopening and never reopened anything.
     * Opening a different block moves the editor away; bringing this one
     * back is then a real second open.
     */
    const other = await idOfTitle(store, 'Three  spaced');
    await vscode.commands.executeCommand('theourgia.openBlock', other);
    await untilOpen(other);
    assert.notStrictEqual(
      vscode.window.activeTextEditor?.document.uri.fsPath,
      first,
      'the editor did not move away, so the open below is not a reopen'
    );

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
    const second = vscode.window.activeTextEditor?.document.uri.fsPath;
    assert.ok(second !== undefined, 'the second open put nothing in front of the user');
    assert.strictEqual(second, first, 'a second canonical document was created');
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
    await untilOpen(two);
    const firstUri = vscode.window.activeTextEditor?.document.uri.toString();
    await vscode.commands.executeCommand('theourgia.openBlock', three);
    await untilOpen(three);
    const secondUri = vscode.window.activeTextEditor?.document.uri.toString();
    assert.notStrictEqual(firstUri, secondUri);
    assert.strictEqual(vscode.window.activeTextEditor?.document.getText(), '## Three  spaced\nb3\n');
  });

  it('sends the body alone when the buffer is saved', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
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
    await untilLogGrows(store, id, logBefore.answers.length);

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
    await untilOpen(id);
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
    await stayedAt(store, id, logBefore.answers.length);

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

    /*
     * KEY: AND THEN A SAVE THAT MUST LAND, WHICH IS WHAT MAKES THE
     * SILENCE ABOVE MEAN SOMETHING.
     *
     * Watching for two seconds says nothing about the two-point-first.
     * A later save that DOES land is an observation of order rather than
     * of duration: once its record is in the log, anything the heading
     * save was going to send has either already been sent -- in which
     * case the total is two and this fails -- or is a request that will
     * never be made. The watch above stays as the first line, because it
     * reports the failure a second and a half earlier and names the
     * count it saw.
     */
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Three  spaced\nb3 with a body change\n'
      );
    });
    await document.save();
    await untilLogGrows(store, id, logBefore.answers.length);
    const afterBoth = await store.client.request('log', [id]);
    assert.strictEqual(
      afterBoth.answers.length,
      logBefore.answers.length + 1,
      'two records were appended across a refused heading save and one body save, so the heading ' +
        'edit was sent after all -- the watch above simply stopped looking before it arrived'
    );
    assert.strictEqual(
      (await store.client.request('read', [id, '--md'])).text,
      '## Three  spaced\nb3 with a body change\n',
      'the one record that landed is not the body change, so it is the heading edit'
    );
  });

  it('O5 does not overwrite a buffer that has unsaved changes', async () => {
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    const document = editor.document;
    const opened = document.getText();

    await editor.edit((builder) => {
      builder.insert(document.positionAt(document.getText().length), 'typed but not saved\n');
    });
    assert.ok(document.isDirty, 'the edit did not leave the buffer dirty');
    const typed = document.getText();

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
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
    await untilLogGrows(store, id, logBefore.answers.length);
    const readBack = await store.client.request('read', [id, '--md']);
    assert.strictEqual(readBack.text, typed);
    const logAfter = await store.client.request('log', [id]);
    assert.strictEqual(logAfter.answers.length, logBefore.answers.length + 1);
  });

  it('S8 sends LF when the editor is writing CRLF and the block holds none', async () => {
    const id = await idOfTitle(store, 'Three  spaced');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
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

    const before = await logLength(store, id);
    await document.save();
    await untilLogGrows(store, id, before);

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
    await untilOpen(id);
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
    /*
     * THE SAVE ITSELF IS THE OBSERVATION. The editor writes the file and
     * clears the flag; what this cell is about is what the extension does
     * NOT do afterwards, and that is watched below.
     */
    await until('the editor finished writing the buffer', () => !document.isDirty);
    assert.ok(!document.isDirty, 'the buffer is dirty, so this cell is not testing what it says');
    const refused = document.getText();

    /*
     * KEY: THE EDITOR IS MOVED AWAY FIRST, so the open below is a real
     * one, and the FILE is checked as well as the buffer.
     *
     * Two things were wrong with this and both were found in review
     * rounds. The waiter tested `!document.isDirty`, which line 709 had
     * already established -- a guard comparing a value with itself. And
     * it read only the buffer: a reopen that overwrote `current.md` on
     * disk while VS Code still served the cached text would satisfy it,
     * which is precisely the failure "reopening threw away a refused
     * save" describes.
     */
    const file = document.uri.fsPath;
    /*
     * NEVER: A DIFFERENT BLOCK. This said `idOfTitle(store, 'Two')` -- the
     * same block the cell is about -- so the editor never moved and the
     * open below was not a reopen at all. The fix for that was written
     * in a review round and made this same mistake; the next round
     * caught it. The id is taken from the one above rather than looked
     * up again, so the two cannot drift.
     */
    const other = await idOfTitle(store, 'Three  spaced');
    assert.notStrictEqual(other, id, 'the block moved away to is the block under test');
    await vscode.commands.executeCommand('theourgia.openBlock', other);
    await untilOpen(other);
    assert.notStrictEqual(
      vscode.window.activeTextEditor?.document.uri.fsPath,
      file,
      'the editor did not move away, so the open below is not a reopen'
    );

    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
    assert.strictEqual(
      vscode.window.activeTextEditor?.document.getText(),
      refused,
      'reopening the block threw away a save the core had refused'
    );
    assert.strictEqual(
      fs.readFileSync(file, 'utf8'),
      refused,
      'the file on disk was overwritten, and only the editor\'s cached copy still holds the ' +
        'refused text'
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
    store = await RealStore.make('vscode-front', { runRoot: hostRunRoot() });
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
    /*
     * NOTE: WAITING FOR THE EXTENSION TO HAVE TAKEN THE STORE, not for a
     * duration. A setting is delivered to a handler that rebuilds, and
     * the caller of `update` is not that handler; a cell that opened a
     * block before the rebuild ran would be reading the store it had
     * just left. Found in a third review round.
     */
    await untilStatus('the extension took the store', (f) => f.store === store.store);
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
    await until('the document block opened', () => {
      const open = vscode.window.activeTextEditor?.document;
      return open !== undefined && open.getText().length > 0;
    });
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
    /*
     * THE FIRST SAVE HAS TO HAVE LANDED AND EMPTIED THE BODY, or the
     * second one is measured against a baseline that never moved and the
     * cell passes without exercising the rebuild at all.
     */
    const deadline=Date.now()+12000;
    let emptied=await new StoreModel(store.client).blockOf(fileBlock);
    while(emptied && stringField(emptied,'src')!=='' && Date.now()<deadline) {
      await settle(100);emptied=await new StoreModel(store.client).blockOf(fileBlock);
    }
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

    const refillDeadline=Date.now()+12000;
    let refilled=await new StoreModel(store.client).blockOf(fileBlock);
    while(refilled && stringField(refilled,'src')!=='a body typed back in\r\n' && Date.now()<refillDeadline) {
      await settle(100);refilled=await new StoreModel(store.client).blockOf(fileBlock);
    }
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
    store = await RealStore.make('vscode-retry', { runRoot: hostRunRoot() });
    other = await RealStore.make('vscode-retry-other', { runRoot: hostRunRoot() });
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-retry', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settingsTaken(store.store, 'vscode-retry');
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor', 'timeoutMs']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    store?.dispose();
    other?.dispose();
  });

  async function strandOneSave(): Promise<{ entered: string; armed: string }> {
    const settings = vscode.workspace.getConfiguration('theourgia');
    const id = await idOfTitle(store, 'Two');
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await untilOpen(id);
    const editor = vscode.window.activeTextEditor;
    assert.ok(editor !== undefined, 'nothing was opened');
    // Pass every real W/read/cursor call through. Stall only commit, after the
    // extension has durably selected and queued the version it intends to send.
    const wrapper=`${store.root}/delay-commit.py`,entered=`${store.root}/commit-entered`;
    const armed=`${store.root}/stall-next-commit`;
    /*
     * NOTE: ONLY THE FIRST COMMIT IS HELD, and the marker file is what
     * remembers that -- so nothing has to be UNSET afterwards to let the
     * queue drain again.
     *
     * It used to stall every commit and the cell put the real
     * interpreter back before asserting. Changing a setting is what makes
     * this extension rebuild, and rebuilding schedules a drain; through
     * the daemon that drain now finishes in about 30 ms, so by the time
     * the assertion ran the extension had quietly got the stranded save
     * through and `pending` was 0. It had passed for years only because
     * a request read from source took about 460 ms and the drain was
     * still in flight. NEVER: The cell was not measuring what it said: it was
     * measuring that the core was slow.
     */
    /*
     * KEY: THE STALL IS ARMED, ONE COMMIT AT A TIME, and disarms itself.
     *
     * Two things need holding in this describe and they need holding at
     * different moments: the save, so that it is left stranded, and then
     * the RETRY, so that a settings change can overtake it. A wrapper
     * that stalled every commit made the first impossible to end without
     * changing a setting -- and changing a setting is what schedules the
     * drain that got the stranded save through. A wrapper that stalled
     * only the first made the second a race: nothing held the retry, so
     * a correct extension could finish it before the settings change
     * arrived and legitimately report. Found in a third review round.
     *
     * So the caller says when. A commit stalls if the arming file is
     * there, removes it before sleeping so the next one runs, and writes
     * the marker the caller waits on.
     */
    fs.writeFileSync(wrapper,[
      '#!/usr/bin/env python3','import os, sys, time',
      `scheme = ${JSON.stringify(store.config.scheme)}`,
      `marker = ${JSON.stringify(entered)}`,
      `arm = ${JSON.stringify(armed)}`,
      "if len(sys.argv) > 3 and sys.argv[3] == 'commit' and os.path.exists(arm):",
      '    os.remove(arm)',
      "    with open(marker, 'w') as f: f.write('commit')",
      '    time.sleep(30)',
      'os.execvp(scheme, [scheme, *sys.argv[1:]])',''
    ].join('\n'),{mode:0o700});
    await settings.update('scheme', wrapper, vscode.ConfigurationTarget.Global);
    await settings.update('timeoutMs', 10000, vscode.ConfigurationTarget.Global);
    /*
     * NEVER: A PREDICATE THAT IS ALWAYS TRUE IS NOT A WAIT. This read
     * `() => true`, which returns on the first reading whatever it says
     * -- a guard comparing a value with itself. Found in a fourth review
     * round.
     *
     * The facts this extension reports do not name the interpreter, so
     * the actor is changed in the same batch of settings and waited for:
     * one `readConfig` takes both, so an actor that has arrived is a
     * rebuild that has run, and the interpreter arrived with it.
     */
    await settings.update('actor', 'vscode-retry-stalling', vscode.ConfigurationTarget.Global);
    await settingsTaken(store.store, 'vscode-retry-stalling');
    fs.writeFileSync(armed, 'stall the next commit\n', 'utf8');
    await editor?.edit((b) => b.insert(new vscode.Position(1, 0), 'stranded\n'));
    await editor?.document.save();
    await until('the save reached commit after its W write', () => fs.existsSync(entered));
    /*
     * KEY: AND THEN FOR THE ENTRY TO BE IN THE STATE THIS CELL IS ABOUT,
     * read off the disk.
     *
     * NEVER: NOT for `pending` to be 1: the entry is queued BEFORE the send,
     * so that count is 1 from the moment the save handler writes it --
     * long before the send has failed. Waiting for the count would have
     * gone on to the retry while the held commit was still running. The
     * state is the difference between "written down" and "sent, and the
     * answer never came", and only the second is a stranded save.
     */
    await until('the held save was given up on and left waiting', () => {
      return queueEntries().some((entry) => entry.state === 'pending');
    });
    const facts = (await vscode.commands.executeCommand('theourgia.showStatus', {
      ask: false
    })) as { pending: number | null };
    assert.strictEqual(facts.pending, 1, 'no save was stranded, so there is nothing to retry');
    return { entered, armed };
  }

  it('reports nothing rather than reporting it against the store that replaced it', async () => {
    const stall = await strandOneSave();
    const settings = vscode.workspace.getConfiguration('theourgia');
    /*
     * KEY: THE RETRY IS HELD UNTIL THE SETTINGS CHANGE HAS BEEN MADE, and
     * the holding is observed rather than assumed.
     *
     * This cell asks what a retry reports when the store changes UNDER
     * it. Starting the retry and changing the setting straight afterwards
     * does not establish that order: a retry that finished first would
     * report, correctly, and this cell would call that a defect. So the
     * next commit is armed to stall, the retry is started, and the cell
     * waits until the stall has actually been entered before touching the
     * setting. Found in a third review round.
     */
    fs.rmSync(stall.entered, { force: true });
    fs.writeFileSync(stall.armed, 'stall the retry\n', 'utf8');
    const running = vscode.commands.executeCommand('theourgia.retryOutbox') as Promise<unknown>;
    await until('the retry reached commit and is being held', () => fs.existsSync(stall.entered));
    await settings.update('store', other.store, vscode.ConfigurationTarget.Global);
    const reported = await running;
    /*
     * THE SETTING GOES BACK BEFORE THE ASSERTION. Restoring it afterwards
     * made the next cell depend on this one passing: a failure here left
     * the other store configured, and the cell below failed too, which
     * reads as two defects and is one.
     */
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await untilStatus('the extension took the store back', (f) => f.store === store.store);
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
    store = await RealStore.make('vscode-broken', { runRoot: hostRunRoot() });
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-broken', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settingsTaken(store.store, 'vscode-broken');
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
    /*
     * NOTE: WAITING FOR THE EXTENSION TO HAVE NOTICED, not for 250 ms.
     * Found in a second review round. A settings change is delivered to
     * a handler that rebuilds; sleeping and then reading is an assertion
     * about a duration. What is waited for is the store this extension
     * says it is using, which is the first thing the rebuild replaces.
     */
    const facts = await untilStatus(
      'the extension took the empty store setting',
      (f) => f.store === ''
    );
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await untilStatus('the extension took the store setting back', (f) => f.store === store.store);
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

/*
 * plugin-r2 T1: the extension runs the thin client, and the thin client
 * runs a daemon.
 *
 * WHAT CHANGED AND WHY IT NEEDS CELLS HERE. This extension used to run
 * the old `cli` entry point -- a fresh interpreter, loading the whole
 * core, for every request. It now runs `theourgia.sc`, which finds or
 * starts a daemon
 * and sends it the request. Measured on the pinned core: about 460 ms a
 * request from source, about 40 to 50 from a product directory, about 30
 * through the daemon (design section 7.6.53). None of that is visible
 * from inside the extension; what IS visible, from outside, is whether a
 * daemon exists, whether a second request started a second one, and what
 * a user is told when one cannot be started at all.
 *
 * NOTE: THE PROCESSES ARE FOUND BY THIS STORE'S PATH, which is in the
 * daemon's own argument list. NEVER: not by the word "scheme" -- a
 * dozen of the editor's own helpers carry it, and the core spells the
 * interpreter from THEOURGIA_SCHEME, so a machine that sets that to
 * `chez` would have this find nothing and say so cleanly.
 */
describe('plugin-r2 T1 the extension reaches the core through a daemon', function () {
  this.timeout(180000);
  let store: RealStore;

  before(async () => {
    store = await RealStore.make('vscode-daemon', { runRoot: hostRunRoot() });
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-daemon', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settingsTaken(store.store, 'vscode-daemon');
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    store?.dispose();
  });

  it('starts a daemon for the block it opens, and opens a second without starting another', async () => {
    /*
     * KEY: THE FIXTURE'S DAEMON IS STOPPED FIRST, and that is what makes
     * this cell about the EXTENSION.
     *
     * NEVER: It used to open a block and observe that the count had not
     * changed -- and the daemon it was counting was the fixture's own,
     * started by the requests that fetched the ids. Measured in a review
     * round: forcing the extension onto the `cli` transport, which starts
     * no daemon at all, left every observation identical. The cell was
     * watching a process the extension had nothing to do with.
     *
     * With nothing running, the first open must bring a daemon into
     * existence -- only the thin client does that -- and the second must
     * reuse it, which is the whole of what having a daemon buys. The pid
     * settles the second half: a daemon replaced between the two opens
     * has a different one, and a count alone would read that as "still
     * just one".
     */
    const first = await idOfTitle(store, 'Two');
    const second = await idOfTitle(store, 'Three  spaced');

    stopDaemonsFor(store.store);
    assert.deepStrictEqual(
      daemonsMatching(store.store),
      [],
      "the fixture's daemon would not stop, so this cell cannot tell whose daemon it is watching"
    );

    await vscode.commands.executeCommand('theourgia.openBlock', first);
    await untilOpen(first);
    const afterFirst = daemonsMatching(store.store);
    assert.strictEqual(
      afterFirst.length,
      1,
      `opening a block left ${afterFirst.length} daemons for ${store.store}. With none running ` +
        'beforehand, exactly one means the extension went through the thin client; none means ' +
        'it did not.'
    );

    await vscode.commands.executeCommand('theourgia.openBlock', second);
    await untilOpen(second);
    assert.deepStrictEqual(
      daemonsMatching(store.store),
      afterFirst,
      'the second open started another daemon, so the first request is not being reused'
    );
  });

  it('puts that daemon socket under the run root the launcher gave this host', function () {
    const socket = socketPathOf(store.store);
    assert.notStrictEqual(socket, null, 'no running daemon names this store');
    assert.strictEqual(
      (socket as string).startsWith(hostRunRoot()),
      true,
      `the daemon's socket is at ${socket}, which is not under ${hostRunRoot()}. The extension ` +
        'and these cells would then be talking to two different daemons about one store.'
    );
  });
});

/*
 * plugin-r2 T1: and when a daemon cannot be started at all.
 *
 * KEY: THE USER IS TOLD WHAT THE CORE SAID. The thin client's whole job
 * on this path is to turn "I could not start a server" into words; an
 * extension that collapsed those into "unknown" would leave somebody
 * looking at a save that will not go with nothing to act on. Measured
 * against the pinned core, with a directory sitting where the socket
 * goes:
 *
 *   $ theourgia outline --store <s>
 *   (error serve-path-occupied (path "<run root>/<key>/socket"))
 *   rc=75
 */
describe('plugin-r2 T1 a daemon that cannot be started', function () {
  this.timeout(180000);
  let store: RealStore;
  let socket: string | null = null;

  before(async () => {
    store = await RealStore.make('vscode-nodaemon', { runRoot: hostRunRoot() });
    await store.importMarkdown('doc.md', DOC);
    const settings = vscode.workspace.getConfiguration('theourgia');
    await settings.update('corePath', store.config.corePath, vscode.ConfigurationTarget.Global);
    await settings.update('libDirs', store.config.libDirs, vscode.ConfigurationTarget.Global);
    await settings.update('scheme', store.config.scheme, vscode.ConfigurationTarget.Global);
    await settings.update('store', store.store, vscode.ConfigurationTarget.Global);
    await settings.update('actor', 'vscode-nodaemon', vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('theourgia.theourgia')?.activate();
    await settingsTaken(store.store, 'vscode-nodaemon');
  });

  after(async () => {
    const settings = vscode.workspace.getConfiguration('theourgia');
    for (const name of ['store', 'corePath', 'libDirs', 'scheme', 'actor']) {
      await settings.update(name, undefined, vscode.ConfigurationTarget.Global);
    }
    if (socket !== null) {
      fs.rmSync(socket, { recursive: true, force: true });
    }
    store?.dispose();
  });

  it('relays the words the core used, rather than calling it unknown', async () => {
    const id = await idOfTitle(store, 'Two');
    /*
     * NOTE: WAITING FOR A DIFFERENT DOCUMENT, NOT FOR ONE TO EXIST. The
     * describes above leave an editor open, so `activeTextEditor !==
     * undefined` is already true when this starts -- a guard comparing a
     * value with itself, which is how the first version of this cell
     * went on to save a document belonging to another store's block and
     * then waited half a minute for a report about it.
     */
    const was = vscode.window.activeTextEditor?.document.uri.toString() ?? '';
    await vscode.commands.executeCommand('theourgia.openBlock', id);
    await until(
      `a block of ${store.store} opened (the editor held ${was})`,
      () => (vscode.window.activeTextEditor?.document.uri.toString() ?? '') !== was
    );
    const editor = vscode.window.activeTextEditor as vscode.TextEditor;
    const document = editor.document;

    /*
     * THE OBSTRUCTION IS PUT WHERE THE CORE ITSELF PUT THE SOCKET. The
     * name under the run root is a digest of the store's resolved path,
     * and composing it here would be a second implementation of that
     * rule -- one whose mistake would be an obstruction in a directory
     * nobody uses, and a cell that then passed for the wrong reason.
     */
    /*
     * KEY: THE BASELINE IS TAKEN WHILE THE STORE CAN STILL ANSWER. It was
     * taken after the obstruction was in place, which made it a refusal
     * rather than a count -- and the comparison at the end of this cell
     * was then between two refusals, which are equal whatever the log
     * holds. Found in a second review round.
     */
    const before = await logLength(store, id);
    let readBack = '';

    socket = socketPathOf(store.store);
    assert.notStrictEqual(socket, null, 'no daemon is running, so there is no socket path to take');
    stopDaemonsFor(store.store);
    assert.deepStrictEqual(
      daemonsMatching(store.store),
      [],
      'the daemon would not stop, so the client below will simply use it'
    );
    fs.rmSync(socket as string, { force: true });
    fs.mkdirSync(socket as string, { recursive: true });

    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Two\nsaved while no daemon can start\n'
      );
    });
    await document.save();
    /*
     * KEY: AND THE STORE IS ASKED AGAIN, because that is the request whose
     * failure the extension reports where a cell can read it.
     *
     * Measured, and it is what this cell got wrong twice: the save
     * itself reaches nothing readable. A save first asks the store where
     * the cursor is; that request meets the obstruction, so the save is
     * never written down -- right in itself, and invisible. The status
     * this extension hands back is the readable channel, and what fills
     * it is the conflict count's own asking.
     */
    await vscode.commands.executeCommand('theourgia.refreshOutline');

    /*
     * NOTE: WHAT IS WAITED FOR IS THE EXTENSION SAYING IT IS BLOCKED, NOT A
     * QUEUE ENTRY.
     *
     * Measured, and it is the first thing this cell got wrong: nothing
     * reaches the queue at all. A save first asks the store where the
     * cursor is, and that request is the one that meets the obstruction
     * -- so the save never gets as far as being written down, which is
     * right (a request nobody could send is not a request to keep) and
     * is invisible in a file this cell was reading. The extension
     * reports it as the status it hands back, and `blocked` is where the
     * words come out.
     */
    let blocked: string | null = null;
    let seen = '';
    await until(() => `the extension reported that it cannot reach the store; last seen ${seen}`, async () => {
      const facts = (await vscode.commands.executeCommand('theourgia.showStatus', {
        ask: false
      })) as { blocked: string | null; unreachable: string | null };
      blocked = facts.unreachable ?? facts.blocked;
      seen = `${JSON.stringify(facts)} queue=${JSON.stringify(queueEntries())} socket=${
        fs.existsSync(socket as string) ? fs.statSync(socket as string).isDirectory() ? 'dir' : 'file' : 'gone'
      } daemons=${daemonsMatching(store.store).length}`;
      return blocked !== null;
    });
    const said = String(blocked);
    /*
     * KEY: WHAT MAKES THE SENTENCE ACTIONABLE IS THE PATH, and the cell
     * asks for that rather than for one particular refusal name.
     *
     * It listed `serve-path-occupied|serve-start-failed` and a run
     * answered `serve-busy` instead: with a directory sitting where the
     * socket goes, which of the core's names comes back depends on how
     * far the start got before it gave up -- the start may fail
     * (`serve-start-failed`, client.sc:503-556), the path may be held by
     * something that is not a socket (`serve-path-occupied`, daemon.sc:306-307),
     * or a server may start and find it cannot take the lock
     * (`serve-busy`, daemon.sc:305). All of them carry the path. Naming one of
     * them made this cell about which branch the core happened to take,
     * which is not what it is for; the twin below is what keeps the
     * assertion from being vacuous.
     */
    assert.ok(
      said.includes(socket as string),
      `the extension says "${said}", which does not name the socket path. A user reading that ` +
        'has nothing to act on; the path names a directory they can remove.'
    );
    assert.match(
      said,
      /serve-busy|serve-path-occupied|serve-start-failed|connect-failed/,
      `the extension says "${said}", which carries no refusal the core issues for this`
    );
    assert.doesNotMatch(
      said,
      /unknown/,
      'the refusal was collapsed into the word this cell exists to prevent'
    );
    /*
     * AND THE BYTES ARE STILL THERE. A save that could not go out must
     * leave the user's text where they typed it.
     */
    assert.strictEqual(document.getText(), '## Two\nsaved while no daemon can start\n');

    /*
     * AND NOTHING LANDED -- ASKED AFTER THE OBSTRUCTION IS REMOVED.
     *
     * NOTE: IT USED TO BE ASKED WHILE THE SOCKET WAS STILL BLOCKED, which
     * meant both readings were refusals and the comparison was between
     * two error data. The store has to be reachable for this question to
     * have an answer at all.
     */
    fs.rmSync(socket as string, { recursive: true, force: true });
    socket = null;
    /*
     * KEY: AND THE WATCHING IS WHAT MAKES THIS MEAN ANYTHING.
     *
     * A single comparison here asks the question at one instant, and the
     * save handler this cell is about is an asynchronous listener that
     * may not have finished -- what was waited for above was the
     * CONFLICT COUNT's request, which is a different one. Found in a
     * seventh review round. `stayedAt` watches instead, and fails the
     * moment a record appears, naming the counts.
     */
    await stayedAt(store, id, before);
    /*
     * KEY: AND THEN A SAVE THAT MUST LAND, which is what makes the silence
     * above an observation rather than a wait.
     *
     * Watching for two seconds says nothing about the two-point-first,
     * and what this cell needs to rule out is a save handler that was
     * merely slow. A later save that DOES land establishes order: once
     * its record is in the log, anything the blocked save was going to
     * append has either already appeared -- in which case the total is
     * two and this fails -- or never will. Found in an eighth review
     * round; every other negative cell in this file already had one.
     */
    await editor.edit((builder) => {
      builder.replace(
        new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
        '## Two\nsaved once the daemon can start again\n'
      );
    });
    const landed = '## Two\nsaved once the daemon can start again\n';
    await document.save();
    /*
     * KEY: WAITING FOR THE RECORD TO BE THIS ONE, not for the count to
     * move.
     *
     * Measured in a ninth review round: `untilLogGrows` returns on any
     * growth, and the assertion below asked only for `before + 1` -- so
     * a blocked save that arrived LATE and a second save that never
     * completed produced exactly that total, and the cell passed having
     * observed the opposite of what it claims. What the block holds is
     * what tells the two apart.
     */
    await until(
      () => `the second save landed; the block holds ${JSON.stringify(readBack)}`,
      async () => {
        readBack = (await store.client.request('read', [id, '--md'])).text;
        return readBack === landed;
      }
    );
    assert.strictEqual(
      await logLength(store, id),
      before + 1,
      'two records landed across a save that could not be sent and one that could, so the ' +
        'blocked save was sent after all'
    );
  });
});
