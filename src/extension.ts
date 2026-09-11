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
 * The part that talks to VS Code, and only that.
 *
 * EVERY DECISION THIS FILE MAKES IS MADE SOMEWHERE ELSE. What an outline
 * row means is in outline.ts, what a save sends is in saver.ts, what the
 * status bar says is in status.ts. What is left here is registration,
 * rendering and the two events an editor has -- a node was opened, a
 * document was saved -- because those are the things a test cannot reach
 * without an extension host, and keeping anything else beside them would
 * put it out of reach too.
 */

import * as vscode from 'vscode';
import { BlockDocument, documentFor, splitDocument } from './blocks';
import { Client } from './client';
import { CoreConfig, DEFAULT_TIMEOUT_MS, defaultActor, problemsWith } from './config';
import { documentPathFor, hasUncommittedWork, markCommitted, writeDocument } from './documents';
import { Node, StoreModel } from './model';
import { Outbox, outboxPathFor } from './outbox';
import { Saver } from './saver';
import { Notice, headingRefusedNotice, saveNotice, statusLine, wrongStoreNotice } from './status';
import { TransportError } from './transport';
import { initWire } from './wire';

function readConfig(): CoreConfig {
  const settings = vscode.workspace.getConfiguration('theourgia');
  const actor = settings.get<string>('actor', '');
  return {
    scheme: settings.get<string>('scheme', 'scheme') || 'scheme',
    corePath: settings.get<string>('corePath', ''),
    libDirs: settings.get<string[]>('libDirs', []) ?? [],
    store: settings.get<string>('store', ''),
    actor: actor.length > 0 ? actor : defaultActor(),
    timeoutMs: settings.get<number>('timeoutMs', DEFAULT_TIMEOUT_MS),
    transport: settings.get<'cli' | 'socket'>('transport', 'cli')
  };
}

class OutlineProvider implements vscode.TreeDataProvider<Node> {
  private readonly changed = new vscode.EventEmitter<Node | undefined>();
  public readonly onDidChangeTreeData = this.changed.event;
  private model: StoreModel | null;

  constructor(model: StoreModel | null) {
    this.model = model;
  }

  public use(model: StoreModel | null): void {
    this.model = model;
    this.refresh();
  }

  public refresh(): void {
    this.changed.fire(undefined);
  }

  public getTreeItem(node: Node): vscode.TreeItem {
    const item = new vscode.TreeItem(
      node.title.length > 0 ? node.title : node.id,
      node.mayHaveChildren
        ? vscode.TreeItemCollapsibleState.Collapsed
        : vscode.TreeItemCollapsibleState.None
    );
    item.id = node.id;
    item.description = node.id;
    item.contextValue = 'theourgia.block';
    if (node.marked) {
      item.iconPath = new vscode.ThemeIcon('warning');
      item.tooltip = `${node.id} is in a structural conflict`;
    } else if (node.orphan) {
      item.iconPath = new vscode.ThemeIcon('question');
      item.tooltip = `${node.id} has no parent in this store`;
    }
    item.command = {
      command: 'theourgia.openBlock',
      title: 'Open Block',
      arguments: [node.id]
    };
    return item;
  }

  public async getChildren(node?: Node): Promise<Node[]> {
    if (this.model === null) {
      return [];
    }
    try {
      return node === undefined ? await this.model.roots() : await this.model.childrenOf(node.id);
    } catch (e) {
      reportFailure(e);
      return [];
    }
  }
}

/*
 * A FAILURE IS SHOWN WITH THE SENTENCE THE LAYER THAT FOUND IT WROTE.
 * The transport already says which setting to look at when a library is
 * missing and that the child was stopped when it timed out; rewriting
 * those here would produce a second, worse description of each.
 */
function show(notice: Notice): void {
  if (notice.level === 'error') {
    vscode.window.showErrorMessage(`theourgia: ${notice.text}`);
  } else if (notice.level === 'warning') {
    vscode.window.showWarningMessage(`theourgia: ${notice.text}`);
  } else if (notice.level === 'information') {
    vscode.window.showInformationMessage(`theourgia: ${notice.text}`);
  }
}

function reportFailure(e: unknown): void {
  if (e instanceof TransportError) {
    vscode.window.showErrorMessage(`theourgia: ${e.message}`);
    return;
  }
  vscode.window.showErrorMessage(`theourgia: ${(e as Error).message ?? String(e)}`);
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  await initWire();

  const storage = context.globalStorageUri.fsPath;
  const open = new Map<string, BlockDocument>();
  const status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
  status.command = 'theourgia.showStatus';
  const provider = new OutlineProvider(null);

  let config = readConfig();
  let client: Client | null = null;
  let model: StoreModel | null = null;
  let outbox: Outbox | null = null;
  let saver: Saver | null = null;
  let conflicts: number | null = null;
  /*
   * WHICH SETTINGS A REQUEST WAS MADE UNDER. Every request here is
   * awaited, and a setting can change while one is in flight: a block
   * read from one store would then be written into a file named after
   * another, tagged with that other store, and saved into it. The
   * counter is taken before the wait and checked after; a result from a
   * generation that has been replaced is dropped rather than acted on.
   */
  let generation = 0;

  function rebuild(): void {
    generation += 1;
    /*
     * WHAT WAS KNOWN ABOUT THE OLD STORE IS NOT KNOWN ABOUT THE NEW ONE.
     * Keeping the count would show a store nobody has asked as having no
     * conflicts -- which is the one reading a user would act on.
     */
    conflicts = null;
    config = readConfig();
    const problems = problemsWith(config);
    if (problems.length > 0) {
      client = null;
      model = null;
      outbox = null;
      saver = null;
      provider.use(null);
      status.text = '$(book) theourgia $(gear)';
      status.tooltip = problems.map((p) => `${p.setting}: ${p.message}`).join('\n');
      status.show();
      return;
    }
    client = Client.fromConfig(config);
    model = new StoreModel(client);
    outbox = new Outbox(outboxPathFor(storage, config.store));
    try {
      outbox.load();
    } catch (e) {
      reportFailure(e);
    }
    saver = new Saver(client, outbox);
    provider.use(model);
    paint();
  }

  function paint(): void {
    const line = statusLine({
      store: config.store,
      actor: config.actor,
      cursor: outbox?.cursor ?? null,
      conflicts,
      pending: outbox?.pendingCount ?? 0,
      blocked: saver?.blockedBecause ?? null
    });
    status.text = line.text;
    status.tooltip = line.tooltip;
    status.backgroundColor = line.warning
      ? new vscode.ThemeColor('statusBarItem.warningBackground')
      : undefined;
    status.show();
  }

  /*
   * A QUESTION THAT COULD NOT BE PUT IS NOT AN ANSWER OF ZERO. Drawing
   * zero when `conflicts` failed tells the user the store is sound on
   * exactly the occasions when nothing is known about it.
   */
  async function refreshConflicts(): Promise<void> {
    if (model === null) {
      conflicts = null;
      paint();
      return;
    }
    const asked = generation;
    let found: number | null;
    try {
      found = await model.conflictCount();
    } catch (e) {
      found = null;
    }
    if (asked !== generation) {
      return;
    }
    conflicts = found;
    paint();
  }

  async function openBlock(id: string): Promise<void> {
    if (model === null) {
      vscode.window.showWarningMessage('theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const asked = generation;
    const store = config.store;
    let block;
    try {
      block = await model.blockOf(id);
    } catch (e) {
      reportFailure(e);
      return;
    }
    /*
     * The settings changed while the store was being read. This block
     * belongs to the store it was read from, not to the one now
     * configured, and everything downstream -- the file name, the store
     * the buffer is tagged with, the saver it would reach -- would be
     * the new one's.
     */
    if (asked !== generation) {
      vscode.window.showWarningMessage(
        `theourgia: the store setting changed while ${id} was being read, so it was not opened.`
      );
      return;
    }
    if (block === null) {
      vscode.window.showWarningMessage(`theourgia: the store has no block ${id}.`);
      return;
    }
    const file = documentPathFor(storage, store, id);
    const uri = vscode.Uri.file(file);

    /*
     * A BUFFER WITH UNSAVED WORK IN IT IS NOT OVERWRITTEN, and the
     * baseline it is measured against is not moved either. Opening a
     * block again is how a user comes back to it, not how they discard
     * what they have typed -- and moving the baseline under a dirty
     * buffer would be worse than losing the text: the heading check
     * would then compare the user's edits against a heading they never
     * saw, and a save could go out carrying the wrong body.
     */
    const alreadyOpen = vscode.workspace.textDocuments.find(
      (d) => d.uri.fsPath === file && d.isDirty
    );
    if (alreadyOpen !== undefined) {
      await vscode.window.showTextDocument(alreadyOpen, { preview: false });
      vscode.window.showInformationMessage(
        `theourgia: ${id} is already open with unsaved changes, so it was not reloaded from the store.`
      );
      return;
    }

    /*
     * AND A FILE THAT IS CLEAN MAY STILL HOLD WORK. A save the core
     * refused wrote the file to disk before this extension heard the
     * refusal, so the buffer is clean while holding text the store does
     * not have -- and another editor window, which this host cannot see
     * at all, leaves the same trace. Neither is visible in `isDirty`.
     */
    const document = documentFor(block, store);
    if (hasUncommittedWork(file)) {
      const existing = await vscode.workspace.openTextDocument(uri);
      await vscode.languages.setTextDocumentLanguage(existing, 'markdown');
      await vscode.window.showTextDocument(existing, { preview: false });
      open.set(file, document);
      vscode.window.showWarningMessage(
        `theourgia: the file for ${id} holds changes the store does not have, so it was not ` +
          'reloaded. Save it to send them, or delete the file to take the store\'s copy.'
      );
      return;
    }

    writeDocument(file, document);
    open.set(file, document);
    const opened = await vscode.workspace.openTextDocument(uri);
    await vscode.languages.setTextDocumentLanguage(opened, 'markdown');
    await vscode.window.showTextDocument(opened, { preview: false });
  }

  async function onSaved(saved: vscode.TextDocument): Promise<void> {
    const file = saved.uri.fsPath;
    const document = open.get(file);
    if (document === undefined || saver === null) {
      return;
    }
    if (document.store !== config.store) {
      show(wrongStoreNotice(document.id, document.store, config.store));
      return;
    }
    const split = splitDocument(document, saved.getText());
    if (!split.ok) {
      show(headingRefusedNotice(document.id));
      return;
    }
    let outcome;
    try {
      outcome = await saver.save(document.id, 'src', split.src);
    } catch (e) {
      reportFailure(e);
      paint();
      return;
    }
    if (outcome.status === 'saved' || outcome.status === 'replayed') {
      /*
       * THE BLOCK THIS BUFFER IS COMPARED AGAINST MOVES WITH THE SAVE.
       * Without this the next save would still be measured against the
       * body the block had when it was opened, and the heading check
       * would be right only by accident.
       */
      open.set(file, { ...document, src: split.src, text: document.headingSrc + split.src });
      /*
       * THE FILE AS IT STANDS IS NOW IN THE STORE, so the next time this
       * block is opened it may be taken from the store again. This is
       * the only place that is true; a refusal and an unresolved save
       * both leave the marker where it was, which is what keeps the file
       * from being overwritten.
       */
      markCommitted(file);
    }
    show(saveNotice(outcome, split.normalised));
    paint();
  }

  context.subscriptions.push(
    status,
    vscode.window.registerTreeDataProvider('theourgiaOutline', provider),
    vscode.commands.registerCommand('theourgia.refreshOutline', async () => {
      provider.refresh();
      await refreshConflicts();
    }),
    vscode.commands.registerCommand('theourgia.openBlock', openBlock),
    vscode.commands.registerCommand('theourgia.retryOutbox', async () => {
      if (saver === null) {
        return;
      }
      try {
        const outcomes = await saver.retry();
        const stuck = outcomes.filter((o) => o.status === 'pending').length;
        vscode.window.showInformationMessage(
          `theourgia: ${outcomes.length - stuck} of ${outcomes.length} resolved; ${saver.pendingCount} still waiting.`
        );
      } catch (e) {
        reportFailure(e);
      }
      paint();
    }),
    vscode.commands.registerCommand('theourgia.showStatus', async () => {
      await refreshConflicts();
      vscode.window.showInformationMessage(status.tooltip as string);
    }),
    vscode.workspace.onDidSaveTextDocument(onSaved),
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration('theourgia')) {
        rebuild();
      }
    })
  );

  rebuild();
  await refreshConflicts();
}

export function deactivate(): void {
  /*
   * Nothing to undo. The outbox is on disk after every change rather
   * than at shutdown, because a host that is killed does not get a
   * shutdown -- which is the case the outbox exists for.
   */
}
