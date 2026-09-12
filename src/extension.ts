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

import { randomUUID } from 'crypto';
import * as path from 'path';
import * as vscode from 'vscode';
import { documentFor } from './blocks';
import { Client } from './client';
import { CoreConfig, DEFAULT_TIMEOUT_MS, defaultActor, problemsWith } from './config';
import { Node, StoreModel } from './model';
import { Outbox } from './outbox';
import { activateCore } from './activate';
import {
  OPEN_BLOCK,
  OTHER_SESSIONS,
  RECONCILE_BLOCK,
  REFRESH_OUTLINE,
  RETRY_OUTBOX,
  SHOW_STATUS
} from './commands';
import { Choice, Chooser, Destination, chooseAndRecover, destinationFor } from './recovery';
import { SaveContext, settlerFor } from './settling';
import { nodeFileOps } from './fsops';
import { SaveOutcome, Saver } from './saver';
import {
  Notice,
  StatusFacts,
  nodeTooltip,
  notABlockNotice,
  reconcileChoiceNotice,
  reconcileStaleNotice,
  reconcileUnfinishedNotice,
  reconciledNotice,
  retryNotice,
  refusalNotice,
  saveNotice,
  unreconciledNotice,
  unrecordedNotice,
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
      command: OPEN_BLOCK.id,
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

/*
 * ONE LINE OF A TEXT, FOR A LIST THAT HAS ROOM FOR ONE LINE. It is an
 * excerpt and says so when it cuts, because a truncation the reader
 * cannot see is an excerpt they will take for the whole thing -- and
 * they are choosing between texts on the strength of it.
 */
function firstLine(text: string): string {
  const line = text.split('\n', 1)[0].replace(/\r$/, '');
  const head = line.length > 80 ? line.slice(0, 80) : line;
  return head === text ? head : `${head}...`;
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

  /*
   * X1c: THE FILES BELONG TO THIS WINDOW AND TO NOTHING ELSE.
   *
   * A session directory per extension host is what makes the in-process
   * chain sufficient: no other process writes these paths, so ordering
   * them here is ordering all of their writers. The id is made once, at
   * activation, and never reused. (§12.9)
   */
  /*
   * ONE ENTRY POINT, SHARED WITH THE HARNESS. Everything activation does
   * that is not about VS Code lives in `activateCore`, so a cell that
   * sets up a session cannot do a step the extension forgets -- which is
   * exactly how "the extension never called `begin`" survived: the
   * harness called it instead. (src/activate.ts)
   */
  const files = nodeFileOps;
  const core = activateCore({
    files,
    globalStorage: storage,
    documents: {
      isOpen: (file) => vscode.workspace.textDocuments.some((d) => d.uri.fsPath === file)
    },
    stores: [],
    sessionId: randomUUID()
  });
  const sessions = core.sessions;
  const sessionId = core.sessionId;
  const chain = core.chain;
  const publisher = core.publisher;
  const saving = core.saving;

  const storeHash = core.storeHash;

  /*
   * WHAT A SAVE IN FLIGHT IS ABOUT. The Saver's answer names the request
   * and the cursor; the record beside the file needs the file and the
   * digests, and only the handler that decided to send knows them.
   */
  /*
   * ⚠️ IT OUTLIVES EVERY REBUILD AND IS KEYED BY BLOCK ID, so an entry
   * in it may have been written by a send to a different store. What
   * decides whether it belongs to an answer is inside the context -- the
   * store it was sent for and the bytes it carried -- and `settlerFor`
   * asks both. The key cannot be the request id: this is written before
   * `Saver.save` is called, and the id is made inside it.
   */
  const pendingSaves = new Map<string, SaveContext>();
  /*
   * X1c REPLACED THE IN-MEMORY BASELINE. What a save is measured against
   * now lives beside the file, on disk, in `<n>.md.meta` -- so it survives
   * a restart, and two readings of one block cannot describe each other.
   * The ordering that used to be done with tickets is done by the chain.
   */
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
    /*
     * THE QUEUE IS THIS SESSION'S. One writer -- this process -- so it
     * needs no lock; a queue outside the sessions, however it were
     * numbered, would be two windows writing one file again. (§12.9,
     * C16)
     */
    outbox = new Outbox(core.outboxPath(config.store), files);
    try {
      outbox.load();
    } catch (e) {
      reportFailure(e);
    }
    /*
     * SETTLING GOES THROUGH THE RECORD.
     *
     * The Saver no longer removes an answered entry; this says what
     * settling one means here, and it means: write the acknowledgement
     * beside the file FIRST, and remove the request only if that
     * succeeded. A build that removed it first and then failed to write
     * has destroyed its own means of retrying -- the request gone, the
     * store's answer unrecorded. `recordAnswer` performs both, in that
     * order, so the order exists in one place.
     */
    /*
     * X1c ⑨: THE FILE AN ANSWER IS ABOUT, WHEN THIS WINDOW NEVER SENT IT.
     *
     * A retry after a restart names a request the queue remembers and
     * this process does not, so `pendingSaves` is empty for it. The
     * answer used to be released with nothing written, and that block
     * then reported unsent work for ever while the store held the bytes.
     *
     * The queue kept what was sent, and the block's own directory says
     * which versions exist. If the newest one still splits into exactly
     * the text that went out, these are the bytes the store acknowledged
     * and the digests come from the file rather than from memory. If it
     * does not, this answers `undefined` and the old behaviour stands --
     * which is right, because then the file really has moved on.
     */
    /*
     * ⚠️ THIS SAVER'S OWN QUEUE AND ITS OWN STORE, captured here.
     *
     * The settler below read the module's `outbox` and `config.store`
     * -- the LIVE ones -- so an answer arriving after the settings
     * changed was settled against the queue that had replaced this
     * one. `Outbox.resolve` filters by request id and then writes the
     * cursor unconditionally: the other store's queue kept its own
     * entries, took THIS store's cursor, and committed it, while the
     * request that was actually answered stayed pending in the queue
     * nobody was looking at any more. Reproduced in the editor suite:
     * `store A's cursor was painted under store B`. Found in review.
     *
     * A Saver belongs to one queue for its whole life -- that is what
     * `serialise` keys on -- so the settler belongs to that queue too,
     * and to the store whose directory its blocks are under.
     */
    /*
     * ⚠️ THIS SAVER'S OWN QUEUE AND ITS OWN STORE, captured here and
     * handed to the settler.
     *
     * The settler read the module's `outbox` and `config.store` -- the
     * LIVE ones -- so an answer arriving after the settings changed was
     * settled against the queue that had replaced this one. That queue
     * kept its own entries, took THIS store's cursor and committed it,
     * while the request that was actually answered stayed pending in the
     * queue nobody was looking at any more. Reproduced in the editor
     * suite: `store A's cursor was painted under store B`.
     *
     * A Saver belongs to one queue for its whole life -- that is what
     * `serialise` keys on -- so the settler belongs to that queue too,
     * and to the store whose directory its blocks are under. What
     * decides all of that is in `src/settling.ts`, where a cell can
     * drive it; this is the wiring and nothing else.
     */
    const own = outbox;
    const ownStore = storeHash(config.store);
    saver = new Saver(
      client,
      own,
      settlerFor({
        queue: own,
        storeHash: ownStore,
        sessionId,
        pendingSaves,
        sessions,
        publisher,
        saving,
        report: show,
        unrecorded: unrecordedNotice
      })
    );
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
     * that store's path.
     *
     * ⚠️ AND THE SENTENCE THAT USED TO FOLLOW IS NOT VERIFIED. It said
     * "a save into a differently configured store is refused by name",
     * which is what makes showing a buffer from the store the user has
     * left merely the wrong answer to their last question rather than a
     * way to write into the wrong place. Asked for the cell that holds
     * that up, this batch could not find one: `sidecarOf` reads the
     * `.meta` beside the file and decides nothing about which store is
     * configured. The claim is left here as a claim, marked, rather than
     * stated as a fact -- the guard, if it exists, would be on the save
     * path where the sidecar's directory is compared with the configured
     * store, and if it does not exist that is a defect on the outline
     * and editor surface. First item of the next batch.
     *
     * Do not add a check back here without a cell that fails when it is
     * removed.
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
    /*
     * X1c: THE BLOCK'S VERSIONS LIVE UNDER THIS SESSION'S DIRECTORY, and
     * opening it publishes the next one. Nothing here rewrites a file
     * and nothing here deletes one: a reading from the store becomes
     * `<n+1>.md` with its own record, and what was there stays. That is
     * why "another reading replaced my baseline" has nowhere to happen
     * rather than being guarded against. (§12.9, §12.15 结构一)
     */
    const directory = sessions.directoryFor(sessionId, storeHash(store), id);
    const document = documentFor(block, store);

    /*
     * THE PUBLICATION AND EVERYTHING THAT READS IT ARE ON ONE CHAIN,
     * keyed by the directory these versions share, so a save arriving
     * for this block waits rather than interleaving. (§12.11.1)
     */
    const outcome = await chain.run(directory, async () =>
      publisher.publish({
        directory,
        storeId: store,
        blockId: id,
        prefix: document.prefix,
        text: document.text
      })
    );

    if (!outcome.published) {
      /*
       * The editor holds the path this would have written. Showing what
       * is there is the answer; writing is not. (§12.13.1)
       */
      if (outcome.file !== null) {
        const already = await vscode.workspace.openTextDocument(vscode.Uri.file(outcome.file));
        await vscode.window.showTextDocument(already, { preview: false });
      }
      return;
    }

    const opened = await vscode.workspace.openTextDocument(vscode.Uri.file(outcome.file));
    await vscode.languages.setTextDocumentLanguage(opened, 'markdown');
    await vscode.window.showTextDocument(opened, { preview: false });
  }

  /*
   * X1c: THE WAY OUT THE REFUSALS NAME.
   *
   * Two refusals -- a file holding a third version, and a store that
   * reports a different request under this save's name -- tell the user
   * to run this. Before it existed those sentences named a command that
   * was in no manifest and no registration, so the only advice the
   * extension gave led to an empty palette; the name is now one
   * constant, read by both the sentence and the registration.
   *
   * NOTHING HERE REWRITES OR REMOVES THE FILE THE USER IS LOOKING AT.
   * Both actions publish a NEW version and leave the old one alone,
   * because that file is the only copy of what they typed. (§12.15
   * 结构一, §12.11.7)
   *
   * IT TAKES AN OPTIONAL PATH so that it can be reached from somewhere
   * other than the active editor, and answers with the notice it showed
   * rather than only showing it: a message that is displayed and not
   * returned is a message no cell can read. ⚠️ THAT HOLDS FOR THE
   * OUTCOMES OF THE RECONCILIATION AND NOT FOR EVERY EXIT: the paths
   * that give up before one -- no store configured, the store could not
   * be read, the settings changed underneath, no such block -- show a
   * plain warning and answer `null`. They are about the command not
   * running rather than about what it did.
   *
   * ⚠️ THE THIRD TEXT IS REPORTED AND NOT SHOWN. `reconcile` produces
   * the version published before this one, and this function does not
   * display it: the pick offers the two actions, each with an excerpt of
   * the text it would produce, and nothing puts the previous version in
   * front of the user. That is a gap, not a design -- the earlier
   * wording here said the user was choosing "with it in view", which was
   * not true of any code. There is no action that produces it, it is on
   * disk beside the file under its own number, and this extension
   * deletes nothing; a viewer that shows the three side by side is not
   * in this batch.
   */
  async function reconcileBlock(target?: string): Promise<Notice | null> {
    const file = target ?? vscode.window.activeTextEditor?.document.uri.fsPath;
    if (file === undefined) {
      const notice = notABlockNotice('no file');
      show(notice);
      return notice;
    }
    const sidecar = publisher.sidecarOf(file);
    if (sidecar === null) {
      const notice = notABlockNotice(file);
      show(notice);
      return notice;
    }
    if (sidecar.storeId !== config.store) {
      const notice = wrongStoreNotice(sidecar.blockId, sidecar.storeId, config.store);
      show(notice);
      return notice;
    }
    if (model === null) {
      vscode.window.showWarningMessage('theourgia: set theourgia.corePath and theourgia.store first.');
      return null;
    }
    const asked = generation;
    let block;
    try {
      block = await model.blockOf(sidecar.blockId);
    } catch (e) {
      reportFailure(e);
      return null;
    }
    /*
     * THE SAME CHECK `openBlock` MAKES, for the same reason: the store
     * this reading came from is the store that was configured when it
     * was asked, and reconciling against a different one would write a
     * baseline from a store this block does not belong to.
     */
    if (asked !== generation) {
      vscode.window.showWarningMessage(
        `theourgia: the store setting changed while ${sidecar.blockId} was being read, so nothing ` +
          'was reconciled.'
      );
      return null;
    }
    if (block === null) {
      vscode.window.showWarningMessage(`theourgia: the store has no block ${sidecar.blockId}.`);
      return null;
    }
    const document = documentFor(block, config.store);
    const directory = path.dirname(file);
    /*
     * ON THE CHAIN, because it reads the file and may publish beside it,
     * and a save arriving for this block has to wait rather than
     * interleave with it. (§12.11.1)
     */
    const outcome = await chain.run(directory, async () =>
      publisher.reconcile(file, document.prefix, document.text)
    );
    if (outcome.reconciled) {
      const notice = reconciledNotice(sidecar.blockId, path.basename(file));
      show(notice);
      paint();
      return notice;
    }
    /*
     * THE PICK IS SHOWN OUTSIDE THE CHAIN. It waits on a human, and a
     * critical section held across that wait blocks every save of this
     * block for as long as the user leaves the list open. The action
     * runs on the chain when it comes back.
     */
    const items = outcome.choices.map((action) => ({
      action,
      label:
        action === 'prepend-prefix'
          ? 'Keep my text, with the block heading in front of it'
          : "Take the store's version",
      detail:
        action === 'prepend-prefix'
          ? firstLine(outcome.fileText)
          : firstLine(outcome.storeText)
    }));
    const picked = await vscode.window.showQuickPick(items, {
      placeHolder: `${sidecar.blockId}: ${file} holds a version neither this window nor the store wrote`
    });
    if (picked === undefined) {
      return null;
    }
    /*
     * ⚠️ THE ACTION IS CARRIED OUT AGAINST THE TEXT THAT WAS OFFERED, OR
     * NOT AT ALL.
     *
     * `prepend-prefix` re-reads the file. The pick waits on a human, and
     * the file is not on the chain -- another window, or another editor,
     * can replace it while the list is open. Then "keep my text" kept
     * somebody else's: a new version was published carrying bytes the
     * user was never shown, and the confirmation said their text had
     * been kept. Found in review with a reproduction, not supposed.
     *
     * The comparison happens INSIDE the critical section, against the
     * same question `reconcile` answered, so that nothing can move
     * between the check and the action. A file that changed is not an
     * error and nothing is undone -- the offer is simply stale, and the
     * user is told to look again.
     */
    const done = await chain.run(directory, async () =>
      publisher.reconcileBy(file, picked.action, document.prefix, document.text, outcome.fileText)
    );
    if (!done.done && done.because === 'file-changed') {
      const notice = reconcileStaleNotice(file);
      show(notice);
      paint();
      return notice;
    }
    if (!done.done) {
      const notice = reconcileUnfinishedNotice(file);
      show(notice);
      paint();
      return notice;
    }
    const opened = await vscode.workspace.openTextDocument(vscode.Uri.file(done.file));
    await vscode.languages.setTextDocumentLanguage(opened, 'markdown');
    await vscode.window.showTextDocument(opened, { preview: false });
    const notice = reconcileChoiceNotice(sidecar.blockId, picked.action, path.basename(done.file));
    show(notice);
    paint();
    return notice;
  }

  /*
   * X1c: WHAT A SAVE DOES, AND WHAT IT REFUSES.
   *
   * The record beside the file decides: it carries the prefix this save
   * is split against and whether the block's own body used CRLF. A file
   * with no record, or one whose publication never finished, or one
   * holding a third version, is refused BY NAME -- the one thing that
   * must not happen is a save that quietly does nothing, because that is
   * indistinguishable from one that worked. (§12.19.4, §12.17.3)
   */
  /*
   * THE EDITOR, REDUCED TO THE THREE THINGS THE RECOVERY FLOW NEEDS.
   * This object is the only part of that flow which cannot be exercised
   * outside a host, which is why it holds no decision at all: it shows
   * what it is given and reports what came back.
   */
  const editorChooser: Chooser = {
    async pick<T>(items: Array<Choice<T>>, placeHolder: string): Promise<T | undefined> {
      const picked = await vscode.window.showQuickPick(
        items.map((item) => ({
          label: item.label,
          description: item.description,
          detail: item.detail,
          value: item.value
        })),
        { placeHolder }
      );
      return picked?.value;
    },
    async confirm(text: string, confirmation: string): Promise<boolean> {
      /*
       * MODAL, because this is the one question in the extension whose
       * answer moves somebody's files or sends their requests again. A
       * notification that can be missed is not a confirmation.
       */
      const answer = await vscode.window.showWarningMessage(
        text,
        { modal: true },
        confirmation
      );
      return answer === confirmation;
    },
    say: show
  };

  /*
   * WHERE A TAKEOVER'S ENTRIES GO. Null when there is no Saver, which is
   * to say no usable store: there is then no queue of this window's own
   * to put them in, and the flow refuses before taking a token rather
   * than holding one over work it did not move.
   *
   * ⚠️ IT RUNS THE IMPORT INSIDE THE SAVER'S LOCK, through `adopt`.
   * Handing out a bare queue put the import outside whatever serialises
   * that file, and a save answering in the middle of it wrote its own
   * copy back over the imported entries.
   */
  /*
   * ⚠️ AND THE DECISION CARRIES THE GENERATION IT WAS MADE IN.
   *
   * This function runs when the command starts; the pickers after it are
   * awaits, and `rebuild` replaces `saver` and bumps `generation` the
   * moment theourgia.store changes. So the destination read here can
   * stop being this window's queue before anything is written to it --
   * and the entries went in anyway, while the report recommended a
   * command that acts on the queue that replaced it. Every other place
   * in this file that waits takes `generation` and checks it after;
   * this one did not. Traced in review.
   *
   * `destinationFor` does the checking, inside the lock, by calling this
   * closure again. It is a closure and not a snapshot for exactly that
   * reason: a captured number cannot answer a later question.
   */
  function adoptingInto(): Destination | null {
    return destinationFor(() => {
      const active = saver;
      if (active === null) {
        return null;
      }
      return {
        storeHash: storeHash(config.store),
        generation,
        adopt: (work) => active.adopt(work)
      };
    });
  }

  async function onSaved(saved: vscode.TextDocument): Promise<void> {
    const file = saved.uri.fsPath;
    const sidecar = publisher.sidecarOf(file);
    if (sidecar === null) {
      /*
       * NOT OURS. A file the user saved somewhere else is not a block,
       * and this handler leaves it entirely alone. (§12.13.4)
       */
      if (!file.startsWith(sessions.directoryFor(sessionId, '', '').replace(/\/+$/, ''))) {
        return;
      }
      show(unreconciledNotice(path.basename(file), file));
      return;
    }
    if (sidecar.storeId !== config.store) {
      show(wrongStoreNotice(sidecar.blockId, sidecar.storeId, config.store));
      return;
    }
    if (saver === null) {
      return;
    }

    const decision = await chain.run(path.dirname(file), async () =>
      saving.decide({ file, isDirty: saved.isDirty, getText: () => saved.getText() }, sidecar)
    );
    if (!decision.send) {
      show(refusalNotice(sidecar.blockId, file, decision.refusal));
      paint();
      return;
    }

    /*
     * WHAT THIS SAVE IS ABOUT, recorded before it goes, because the
     * answer names only the request and the Saver's settler needs the
     * file and the digests to write the record.
     */
    const context: SaveContext = {
      blockId: sidecar.blockId,
      storeHash: storeHash(config.store),
      file,
      rawDigest: decision.rawDigest,
      sentDigest: decision.sentDigest
    };
    pendingSaves.set(sidecar.blockId, context);

    let outcome;
    try {
      outcome = await saver.save(sidecar.blockId, decision.intent.field, decision.src);
    } catch (e) {
      /*
       * ⚠️ ONLY THIS SAVE'S OWN MEMORY IS FORGOTTEN.
       *
       * The delete was by block id and unconditional, and this line runs
       * AFTER a wait: save a block in one store, change the store, save
       * the same block there, and then let this save fail -- and the
       * entry it removes is the OTHER save's, which is still waiting for
       * an answer. When that answer came, its file could no longer be
       * recognised and it was dequeued with nothing recorded: the user's
       * saved text left as a draft nothing would send again.
       *
       * The settler had the same defect and was repaired one round
       * earlier; this copy survived because the repair was applied where
       * the finding pointed instead of everywhere the shape was. Found
       * in review, in the same place I had just looked.
       */
      if (pendingSaves.get(sidecar.blockId) === context) {
        pendingSaves.delete(sidecar.blockId);
      }
      reportFailure(e);
      paint();
      return;
    }
    /*
     * THE RECORD AND THE REMOVAL BOTH HAPPENED INSIDE THE SETTLER, in
     * that order. Nothing is written here: a second place that wrote the
     * acknowledgement would be a second place for the order to be wrong.
     */
    show(saveNotice(outcome, decision.normalised));
    paint();
  }

  context.subscriptions.push(
    status,
    vscode.window.registerTreeDataProvider('theourgiaOutline', provider),
    vscode.commands.registerCommand(REFRESH_OUTLINE.id, async () => {
      provider.refresh();
      await refreshConflicts();
    }),
    vscode.commands.registerCommand(OPEN_BLOCK.id, openBlock),
    vscode.commands.registerCommand(RECONCILE_BLOCK.id, reconcileBlock),
    /*
     * ⚠️ THE HANDLER IS ONE LINE ON PURPOSE. Everything this command
     * decides -- which windows to list, what may be done to one, what
     * the user is told it costs -- is in src/recovery.ts, where a cell
     * can drive it. A handler that made any of those decisions here
     * would be making them where nothing can look.
     */
    vscode.commands.registerCommand(OTHER_SESSIONS.id, () =>
      chooseAndRecover(sessions, editorChooser, adoptingInto())
    ),
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
    vscode.commands.registerCommand(RETRY_OUTBOX.id, async () => {
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
    vscode.commands.registerCommand(SHOW_STATUS.id, async (options?: { ask?: boolean }) => {
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
