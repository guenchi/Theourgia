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
import { OpenBuffers } from './open';
import { Outbox, outboxPathFor } from './outbox';
import { SaveOutcome, Saver } from './saver';
import {
  Notice,
  StatusFacts,
  prefixRefusedNotice,
  nodeTooltip,
  retryNotice,
  saveNotice,
  statusLine,
  wrongStoreNotice
} from './status';
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
  private readonly failed: (e: unknown) => void;
  private readonly unknownMarks: () => void;
  private readonly generation: () => number;

  constructor(
    model: StoreModel | null,
    failed: (e: unknown) => void,
    unknownMarks: () => void,
    generation: () => number
  ) {
    this.model = model;
    this.failed = failed;
    this.unknownMarks = unknownMarks;
    this.generation = generation;
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
    const tooltip = nodeTooltip(node.id, node.marks, node.fieldConflict);
    if (tooltip !== null) {
      /*
       * THE ICON FOLLOWS THE WORST OF WHAT IS TRUE, and orphanhood is
       * not overridden by anything else the block also is.
       */
      item.iconPath = new vscode.ThemeIcon(
        node.marks === null ? 'circle-slash' : node.orphan ? 'question' : 'warning'
      );
      item.tooltip = tooltip;
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
    /*
     * WHAT THIS REQUEST IS ABOUT IS DECIDED BEFORE IT IS MADE. A
     * settings change replaces the model while a request is still
     * running, and a late answer -- or a late failure -- belongs to the
     * store it was asked of. Without this, an expansion of one store
     * that failed could clear the conflict count of the store the user
     * had by then moved to.
     */
    const asked = this.generation();
    let nodes: Node[];
    let marksKnown: boolean;
    try {
      if (node === undefined) {
        nodes = await this.model.roots();
        marksKnown = true;
      } else {
        const listing = await this.model.childrenOf(node.id);
        nodes = listing.nodes;
        marksKnown = listing.marksKnown;
      }
    } catch (e) {
      if (asked === this.generation()) {
        this.failed(e);
      }
      return [];
    }
    /*
     * A SUBTREE THAT CAME BACK WITHOUT ITS MARKS IS STILL A FAILURE TO
     * REPORT, even though it has children in it -- and even when it has
     * NONE. An expansion that found no children returns an empty list
     * either way, so the fact travels with the listing and not with the
     * nodes.
     */
    if (!marksKnown && asked === this.generation()) {
      this.unknownMarks();
    }
    return nodes;
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
  const open = new OpenBuffers<BlockDocument>();
  const status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
  status.command = 'theourgia.showStatus';
  /*
   * A LISTING THAT FAILED ALSO INVALIDATES THE COUNT BESIDE IT. The tree
   * and the status bar ask the same store two questions, and a failure
   * that emptied the tree while the status bar went on reporting a count
   * from before it would be showing a reassuring number about a store
   * that had just refused to answer.
   */
  const provider = new OutlineProvider(
    null,
    (e: unknown) => {
      conflicts = null;
      reportFailure(e);
      paint();
    },
    () => {
      conflicts = null;
      paint();
    },
    () => generation
  );

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

  /*
   * THE FACTS ARE A VALUE, and `theourgia.showStatus` hands it back. A
   * status bar can be looked at and not read: nothing can ask it what it
   * says, so nothing could check that the conflict count belongs to the
   * store whose name is beside it. Returning the facts makes that a
   * question with an answer -- for a cell, and for anyone else who wants
   * to know what this extension believes about the store.
   */
  function facts(): StatusFacts {
    return {
      store: config.store,
      actor: config.actor,
      cursor: outbox?.cursor ?? null,
      conflicts,
      /*
       * NO OUTBOX IS NOT AN EMPTY OUTBOX. `outbox` is null only when the
       * settings are unusable, which means the queue's location is not
       * known -- not that there is nothing in it. Answering zero there
       * put "nothing waiting to be saved" in front of a user whose
       * settings had just broken while saves sat unsent on disk.
       */
      pending: outbox === null ? null : outbox.pendingCount,
      blocked: saver?.blockedBecause ?? null
    };
  }

  function paint(): void {
    const line = statusLine(facts());
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
    /*
     * THE TICKET IS TAKEN BEFORE THE READ, so that two opens of one
     * block are ordered by when they asked the store rather than by
     * which of them finished first. See src/open.ts.
     */
    const ticket = open.claim();
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
     *
     * THIS IS THE ONLY GENERATION CHECK IN THIS FUNCTION, AND THAT IS
     * DELIBERATE. There used to be five more, after each of the editor
     * waits below. They could not be guarded: the waits are VS Code
     * calls, not core requests, so no stand-in core can widen them, and
     * a configuration change started from inside `onDidOpenTextDocument`
     * -- which does fire during the first of them -- has not reached
     * this extension by the time the wait resolves. That was measured,
     * not assumed. Five checks that nothing could make fail are five
     * places where a later edit is unguarded while looking guarded, so
     * they were removed rather than left as decoration.
     *
     * WHAT STILL PROTECTS THE USER is not a check here but the store
     * recorded ON the buffer: a document opened from one store carries
     * that store's path, and a save into a differently configured store
     * is refused by name. If the settings change during one of the waits
     * below, a buffer from the old store is shown -- the wrong answer to
     * what the user last asked, and nothing worse. Do not add a check
     * back without a cell that fails when it is removed.
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
      /*
       * A NEWER READING MAY HAVE LANDED DURING THOSE THREE WAITS, and it
       * is the one the buffer was written from. Replacing it with this
       * older one gives the save path a prefix the buffer no longer
       * starts with, and a heading that fails to match is not refused --
       * it is treated as body and written into the block.
       */
      if (!open.register(file, document, ticket)) {
        return;
      }
      vscode.window.showWarningMessage(
        `theourgia: the file for ${id} holds changes the store does not have, so it was not ` +
          'reloaded. Save it to send them, or delete the file to take the store\'s copy.'
      );
      return;
    }

    /*
     * THE CLAIM COMES BEFORE THE WRITE. If a newer reading has already
     * become the baseline, this one must not put its older text into the
     * file either -- a file holding one reading while the baseline holds
     * another makes the next save fail the prefix check, which is at
     * least visible, but it is still the wrong text in front of the
     * user.
     */
    if (!open.register(file, document, ticket, () => writeDocument(file, document))) {
      return;
    }
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
      show(prefixRefusedNotice(document.id, document.front.length > 0, document.headingSrc.length > 0));
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
      /*
       * THE WHOLE PREFIX, NOT JUST THE HEADING. Rebuilding the baseline
       * from `headingSrc + src` drops the front matter, and the next
       * save is then measured against a text the buffer never held: with
       * an empty body the baseline contains no carriage return, so a
       * CRLF buffer gets normalised whole and its untouched front matter
       * no longer matches the prefix -- an ordinary body edit refused
       * for a change nobody made.
       */
      open.confirmed(file, { ...document, src: split.src, text: document.prefix + split.src });
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
    /*
     * THE SAVER THAT RAN IS THE SAVER THAT IS REPORTED. `saver` is
     * rebuilt whenever the settings change, and a retry is an await --
     * so reading it again afterwards reports the count of whatever store
     * is configured by then. Changing `theourgia.store` while a retry is
     * in flight produced a sentence pairing one store's outcomes with
     * another store's queue, and if that queue was unreadable the count
     * in it was the word "null". The generation check is the one
     * `refreshConflicts` already uses, for the same reason.
     *
     * IT RETURNS THE NOTICE for the reason `showStatus` returns its
     * facts: a message shown and not returned is a message no cell can
     * read, and what it would be wrong about is which store the numbers
     * belong to.
     */
    vscode.commands.registerCommand('theourgia.retryOutbox', async () => {
      const active = saver;
      const store = config.store;
      const asked = generation;
      if (active === null) {
        return null;
      }
      /*
       * ONE CHECK, AFTER BOTH OUTCOMES. Checking the generation inside
       * the success path and again inside the catch is two checks, and
       * only the first can be made to fail by a cell: producing a retry
       * that BOTH throws and is overtaken by a settings change is not
       * something this suite can arrange. Collecting the outcome first
       * and deciding afterwards leaves one check on the single path out,
       * which the cell that covers the success path also covers.
       */
      let outcomes: SaveOutcome[] | null = null;
      let failure: unknown = null;
      try {
        outcomes = await active.retry();
      } catch (e) {
        failure = e;
      }
      if (asked !== generation) {
        return null;
      }
      if (outcomes === null) {
        reportFailure(failure);
        paint();
        return null;
      }
      const stuck = outcomes.filter((o) => o.status === 'pending').length;
      const notice = retryNotice(store, outcomes.length - stuck, outcomes.length, active.pendingCount);
      show(notice);
      paint();
      return notice;
    }),
    vscode.commands.registerCommand('theourgia.showStatus', async (options?: { ask?: boolean }) => {
      if (options?.ask !== false) {
        await refreshConflicts();
      }
      vscode.window.showInformationMessage(status.tooltip as string);
      return facts();
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
