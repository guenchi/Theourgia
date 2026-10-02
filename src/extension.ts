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
import { performance } from 'perf_hooks';
import * as vscode from 'vscode';
import { Block, documentFor, languageModeOf, prefixOf } from './blocks';
import {DatumBlockRefused, ModeNotKnown, Working, requireText, windowWriter} from './working';
import {MIGRATE_BLOCK} from './commands';
import {digestOfBytes} from './publication';
import {migrateLegacy,migrationIdentity} from './migration';
import {Owners} from './ownership';
import { Client, Note } from './client';
import { CoreConfig, DEFAULT_TIMEOUT_MS, defaultActor, problemsWith } from './config';
import { ChildListing, Node, StoreModel } from './model';
import {
  DirectoryEntry,
  FileEntry,
  FilesTree,
  NOT_IN_ANY_FILE,
  FilesRow,
  PathlessEntry,
  ViewMode,
  CHANGED_SINCE_LISTED,
  candidateOf,
  rowsOf,
  filesByDefault,
  filesTree,
  isViewMode,
  movedPath,
  newDocumentIntent,
  newDocumentPath,
  pathActionNotice,
  renamedPath
} from './directory-view';
import { Outbox } from './outbox';
import { activateCore } from './activate';
import { Composed, DOCUMENT_SCHEME, DocumentTexts, documentOf, documentQuery, readDocumentQuery, refusalOf } from './document-view';
import { HOVER_COMMANDS, HoverPlace, HoverService, blockAt as hoverBlockAt, hoverMarkdown, isSchemeLang, listEntries, nameUnder, readsSchemeNames } from './hover';
import { blockLang, datumLanguageOf, datumNotice, datumViewOf, isDatumBlock, positionOf, recordedModeOf } from './datum-view';
import { projectionNameFor } from './projection-name';
import { runSplit, splitRefusalNotice } from './split-symbols';
import { SupplyKind, SupplyOutcome, runSupply, supplyNotice } from './supply';
import {
  HOVER_MORE,
  SUGGEST_SPLIT,
  SUPPLY_CALLS,
  SUPPLY_DIAGNOSTICS,
  SUPPLY_SIGNATURES,
  GO_TO_DEFINITION,
  MOVE_TO_DIRECTORY,
  NEW_FILE_HERE,
  RENAME_FILE,
  SHOW_FILES,
  SHOW_OUTLINE,
  OPEN_AS_DOCUMENT,
  OPEN_BLOCK,
  OTHER_SESSIONS,
  RECONCILE_BLOCK,
  SEARCH_BLOCKS,
  REFRESH_OUTLINE,
  RETRY_OUTBOX,
  SHOW_STATUS
} from './commands';
import { Choice, Chooser, Destination, chooseAndRecover, destinationFor } from './recovery';
import { Hit, runSearch } from './search';
import {
  DefinitionAnswer,
  DefinitionRecord,
  definitionsOf,
  identifierAt,
  noDefinitionNotice,
  recordLabel,
  targetLine
} from './definition';
import { IntegrityWatch } from './integrity';
import { Acceptance, acceptSave } from './accepting';
import { settlerFor } from './settling';
import { Tombstones } from './tombstones';
import { DurabilitySink } from './durability';
import { coreDirectoryAt, emptyDirectory, filesUnder, insideDirectory, nodeFileOps, scratchUnder } from './fsops';
import { SaveOutcome, Saver } from './saver';
import {
  Notice,
  StatusFacts,
  incompleteWarning,
  nodeTooltip,
  notABlockNotice,
  reconcileChoiceNotice,
  reconcileMovedNotice,
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
    writer: settings.get<string>('writer', ''),
    timeoutMs: settings.get<number>('timeoutMs', DEFAULT_TIMEOUT_MS)
  };
}

/*
 * A CODE BLOCK IS SHOWN IN ITS OWN LANGUAGE (`languageModeOf`, blocks.ts); an
 * editor that knows no mode by that name shows plain text rather than
 * markdown, which would read the code as prose.
 */
async function showInLanguageOf(document: vscode.TextDocument, block: Block): Promise<void> {
  await showInMode(document, languageModeOf(block));
}

async function showInMode(document: vscode.TextDocument, mode: string): Promise<void> {
  try {
    await vscode.languages.setTextDocumentLanguage(document, mode);
  } catch (e) {
    await vscode.languages.setTextDocumentLanguage(document, 'plaintext');
  }
}

/*
 * WHAT OPENING A BLOCK SHOWED: the file, and how long the prefix is that the
 * editor shows before the block's source.
 */
interface OpenedBlock {
  uri: vscode.Uri;
  prefixLength: number;
}

/*
 * THE ROW THAT SAYS A LISTING COULD NOT SEE EVERY WRITER. It is drawn first,
 * above the blocks the listing did get, and opens nothing: it is a
 * statement about the listing, not a block.
 */
interface IncompleteRow {
  incompleteText: string;
  under: string | null;
}

/*
 * A DIRECTORY OF THE FILES VIEW: a grouping of file nodes, never a block.
 * It has no id of the store's, nothing to open, and no mark.
 */
interface DirectoryRow {
  directory: DirectoryEntry;
}

/*
 * THE ROOT GROUP OF BLOCKS IN NO FILE, in the files view.
 */
interface PathlessGroup {
  pathless: PathlessEntry[];
}

/*
 * A BLOCK AS THE FILES VIEW LISTS IT: a file node carries the file it is,
 * a block in the pathless group carries why it is there. Its children are
 * the block's outline children, as in the outline view.
 */
type ListedNode = Node & { file?: FileEntry; inNoFile?: PathlessEntry };

type OutlineElement = ListedNode | IncompleteRow | DirectoryRow | PathlessGroup;

function isIncompleteRow(element: OutlineElement): element is IncompleteRow {
  return (element as IncompleteRow).incompleteText !== undefined;
}

function isDirectoryRow(element: OutlineElement): element is DirectoryRow {
  return (element as DirectoryRow).directory !== undefined;
}

function isPathlessGroup(element: OutlineElement): element is PathlessGroup {
  return (element as PathlessGroup).pathless !== undefined;
}

/*
 * WHERE THE VIEW MODE OF THE CONFIGURED STORE IS KEPT, and how the title
 * bar is told which mode is showing. The mode is a viewer's convenience,
 * kept per store in the workspace's state.
 */
export interface ViewModes {
  get(): ViewMode | undefined;
  remember(mode: ViewMode): Thenable<unknown>;
  show(mode: ViewMode): Thenable<unknown>;
}

/*
 * THE BANNER OVER AN EDITOR WHOSE TEXT CAME FROM A READING THAT COULD NOT SEE
 * EVERY WRITER: a decoration before the first line, as information. The text
 * itself is not touched -- what is shown is what could be read, and what is
 * saved is what the person writes.
 */
function showIncompleteBanner(editor: vscode.TextEditor, notes: Note[], keep: vscode.Disposable[]): void {
  const banner = vscode.window.createTextEditorDecorationType({
    before: {
      contentText: incompleteWarning(notes),
      color: new vscode.ThemeColor('editorInfo.foreground'),
      margin: '0 1em 0 0'
    }
  });
  keep.push(banner);
  editor.setDecorations(banner, [new vscode.Range(0, 0, 0, 0)]);
}

/*
 * WHAT AN ITEM LISTED UNDER ANOTHER STORE ANSWERS, whether it is expanded,
 * opened or acted on: nothing is asked of the store now configured with an
 * id that came from the other one.
 */
const LISTED_ELSEWHERE = 'Theourgia: this item was listed under another store. Refresh the view and select it again.';

class OutlineProvider implements vscode.TreeDataProvider<OutlineElement> {
  private readonly changed = new vscode.EventEmitter<OutlineElement | undefined>();
  public readonly onDidChangeTreeData = this.changed.event;
  private model: StoreModel | null;
  private readonly failed: (e: unknown) => void;
  private readonly unknownMarks: () => void;
  private readonly generation: () => number;
  private readonly store: () => string;
  private readonly noted: (notes: Note[] | null) => void;
  /*
   * THE STORE EACH LISTED ELEMENT CAME FROM. An element is an id and the
   * store it names a block of, nothing more: what an action does with it is
   * read from the store at the moment of the action (see `changePath`).
   */
  private readonly nodeStores = new WeakMap<Node, string>();
  private readonly modes: ViewModes | null;
  /*
   * EACH DIRECTORY AND GROUP ROW'S OWN LISTING: the store it was listed
   * from and the nodes of the reading it came from. Opening it reads these,
   * not the store and not a later listing: the tree was derived from one
   * reading of every block, and pairing an old row with a newer reading
   * could show a file in two places or in none. The rows it draws are ids;
   * an action on one reads the block afresh.
   */
  private readonly rowListings = new WeakMap<DirectoryRow | PathlessGroup, { store: string; nodes: Map<string, Node> }>();

  constructor(
    model: StoreModel | null,
    failed: (e: unknown) => void,
    unknownMarks: () => void,
    generation: () => number,
    store: () => string,
    noted: (notes: Note[] | null) => void = () => undefined,
    modes: ViewModes | null = null
  ) {
    this.model = model;
    this.failed = failed;
    this.unknownMarks = unknownMarks;
    this.generation = generation;
    this.store = store;
    this.noted = noted;
    this.modes = modes;
  }

  public use(model: StoreModel | null): void {
    this.model = model;
    this.refresh();
  }

  /*
   * CALLED ON EVERY REFRESH: the hover's cache is dropped with it, since a
   * refresh is what this window does when it knows the store changed.
   */
  public onRefresh: (() => void) | null = null;

  public refresh(): void {
    this.changed.fire(undefined);
    this.onRefresh?.();
  }


  public getTreeItem(element: OutlineElement): vscode.TreeItem {
    if (isDirectoryRow(element)) {
      /*
       * NEVER: A DIRECTORY HAS NO COMMAND. It is not a block, and there is
       * nothing to open; its menu holds only "new file here".
       */
      const d = element.directory;
      const row = new vscode.TreeItem(d.name, vscode.TreeItemCollapsibleState.Collapsed);
      row.id = `dir:${d.prefix}`;
      row.iconPath = new vscode.ThemeIcon('folder');
      row.description = String(d.count);
      row.tooltip = d.prefix;
      row.contextValue = 'theourgia.dir';
      return row;
    }
    if (isPathlessGroup(element)) {
      const row = new vscode.TreeItem(NOT_IN_ANY_FILE, vscode.TreeItemCollapsibleState.Collapsed);
      row.id = 'theourgia.pathless';
      row.iconPath = new vscode.ThemeIcon('circle-outline');
      row.description = String(element.pathless.length);
      row.contextValue = 'theourgia.pathless';
      return row;
    }
    if (isIncompleteRow(element)) {
      /*
       * INFORMATION, NOT AN ERROR, AND NOTHING TO OPEN. The rows below it
       * are the store's answer; this one says whose writing is not in
       * them.
       */
      const row = new vscode.TreeItem(element.incompleteText, vscode.TreeItemCollapsibleState.None);
      row.id = `theourgia.incomplete:${element.under ?? ''}`;
      row.iconPath = new vscode.ThemeIcon('info');
      row.tooltip = element.incompleteText;
      row.contextValue = 'theourgia.incomplete';
      return row;
    }
    const node = element;
    const item = new vscode.TreeItem(
      node.title.length > 0 ? node.title : node.id,
      node.mayHaveChildren
        ? vscode.TreeItemCollapsibleState.Collapsed
        : vscode.TreeItemCollapsibleState.None
    );
    item.id = node.id;
    /*
     * THE ID, AND THE BLOCK'S KEYWORDS WHEN IT HAS ANY. They come off the
     * block's record rather than out of `outline --with-keywords`,
     * because the outline is a rendering in which only an id cannot be
     * forged by a title -- see `src/outline.ts` -- and the block is
     * already being read for its title anyway.
     */
    item.description =
      node.keywords.length > 0 ? `${node.id}  ${node.keywords}` : node.id;
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
      arguments: [node.id, this.nodeStores.get(node)]
    };
    const file = node.file;
    if (file !== undefined) {
      /*
       * A FILE NODE IS NAMED BY ITS FILE, as export names it; the title is
       * in the tooltip. Its tree id is its own, because the same block can
       * also be drawn under its parent in the outline below a document.
       */
      item.label = file.label;
      item.id = `file:${node.id}`;
      item.contextValue = 'theourgia.block.file';
      item.description = file.note === null ? node.id : `${node.id}  ${file.note}`;
      item.tooltip = [file.path, node.title, tooltip].filter((t) => t !== null && t.length > 0).join('\n');
    }
    const inNoFile = node.inNoFile;
    if (inNoFile !== undefined && inNoFile.note !== null) {
      item.description = `${node.id}  ${inNoFile.note}`;
      item.tooltip = [inNoFile.raw ?? '', tooltip ?? ''].filter((t) => t.length > 0).join('\n');
    }
    return item;
  }

  /*
   * THE ROOT LISTING, in the mode this store is shown in.
   *
   * WHICH MODE is decided once per store: at the first listing, Files when
   * any root block carries a path, and remembered. It does not change by
   * itself afterwards, when a path appears or goes. Deciding it needs the
   * paths, so an undecided store is read the files way, which answers the
   * outline's rows as well.
   */
  /*
   * NOTE: THE MODE IS RECORDED ONLY FOR THE STORE THAT WAS LISTED. A
   * settings change during the reading leaves: the listing is dropped by
   * `getChildren`, and the new store's own listing decides its mode.
   */
  private async rootListing(
    model: StoreModel
  ): Promise<ChildListing & { files: { tree: FilesTree; nodes: Map<string, Node> } | null }> {
    const asked = this.generation();
    const modes = this.modes;
    const stored = modes === null ? 'outline' : modes.get();
    if (stored === 'outline') {
      const listing = await model.roots();
      if (asked !== this.generation()) {
        return { ...listing, files: null };
      }
      if (modes !== null) {
        await modes.show('outline');
      }
      /*
       * NEVER: THE MODE READ BEFORE THE WAITS, in this branch either. It is
       * compared once, after the last wait: Files chosen while the outline
       * was on its way, or while the title bar's key was being set, is the
       * mode, and this reading has no paths to draw it with, so the listing
       * is made again in the mode chosen. A mode that reads back as none was
       * not chosen by anybody, and listing again for it would not end.
       */
      const shown = modes === null ? stored : modes.get();
      if (shown !== undefined && shown !== 'outline') {
        return this.rootListing(model);
      }
      return { ...listing, files: null };
    }
    const reading = await model.filesReading();
    if (asked !== this.generation()) {
      return { ...reading.listing, files: null };
    }
    const candidates = reading.blocks.map((b) => candidateOf(b.block, b.node.title, b.atRoot, b.exportRoot));
    /*
     * NEVER: THE MODE READ BEFORE THE WAIT. A mode chosen while this reading
     * was on its way is the mode, and the default this reading would have
     * decided is not remembered over it.
     */
    const chosen = modes === null ? stored : modes.get();
    const mode: ViewMode = chosen ?? (filesByDefault(candidates) ? 'files' : 'outline');
    if (modes !== null) {
      await Promise.all([chosen === undefined ? modes.remember(mode) : undefined, modes.show(mode)]);
    }
    /*
     * AND AGAIN AFTER THE LAST WAIT: a mode chosen while this one was being
     * remembered, or while the title bar's key was being set, is the mode,
     * and the listing is made again in it. A mode that reads back as none
     * (a store that did not keep what was remembered) is not a choice, and
     * listing again for it would not end.
     */
    const kept = modes === null ? mode : modes.get();
    if (kept !== undefined && kept !== mode) {
      return this.rootListing(model);
    }
    return {
      ...reading.listing,
      files:
        mode === 'files'
          ? { tree: filesTree(candidates), nodes: new Map(reading.blocks.map((b) => [b.block.id, b.node] as [string, Node])) }
          : null
    };
  }

  /*
   * THE ROOT OF THE FILES VIEW: directories first, then the files at the
   * top, then the group of blocks in no file.
   */
  private filesRoot(tree: FilesTree, nodes: Map<string, Node>, store: string): OutlineElement[] {
    return this.drawn(rowsOf(tree.root, tree.pathless), nodes, store);
  }

  private drawn(rows: FilesRow[], nodes: Map<string, Node>, store: string): OutlineElement[] {
    return rows.map((row) => {
      if ('file' in row) {
        return this.fileNode(row.file, nodes, store);
      }
      this.rowListings.set(row, { store, nodes });
      return row;
    });
  }

  private fileNode(file: FileEntry, nodes: Map<string, Node>, store: string): ListedNode {
    const listed: ListedNode = { ...(nodes.get(file.id) as Node), file };
    this.nodeStores.set(listed, store);
    return listed;
  }

  /*
   * THE STORE A LISTED ELEMENT CAME FROM, for a command handed the element
   * itself -- a context-menu entry -- rather than the arguments the row's
   * own command carries. Undefined for an element this view did not list.
   */
  public storeOf(element: OutlineElement): string | undefined {
    if (isDirectoryRow(element) || isPathlessGroup(element)) {
      return this.rowListings.get(element)?.store;
    }
    if (isIncompleteRow(element)) {
      return undefined;
    }
    return this.nodeStores.get(element);
  }

  /*
   * AN ELEMENT LISTED UNDER ANOTHER STORE IS NOT EXPANDED: its id names a
   * block of that store, and reading it from this one would draw another
   * block, or none, under it.
   */
  private listedElsewhere(listed: string | undefined): boolean {
    const current = this.store();
    if (listed !== undefined && listed !== current) {
      vscode.window.showWarningMessage(LISTED_ELSEWHERE);
      return true;
    }
    return false;
  }

  public async getChildren(element?: OutlineElement): Promise<OutlineElement[]> {
    if (this.model === null) {
      return [];
    }
    if (element !== undefined && isIncompleteRow(element)) {
      return [];
    }
    if (element !== undefined && (isDirectoryRow(element) || isPathlessGroup(element))) {
      const carried = this.rowListings.get(element);
      if (carried === undefined || this.listedElsewhere(carried.store)) {
        return [];
      }
      if (isDirectoryRow(element)) {
        return this.drawn(rowsOf(element.directory), carried.nodes, carried.store);
      }
      return element.pathless.map((entry) => {
        const listed: ListedNode = { ...(carried.nodes.get(entry.id) as Node), inNoFile: entry };
        this.nodeStores.set(listed, carried.store);
        return listed;
      });
    }
    const node = element as Node | undefined;
    if (node !== undefined && this.listedElsewhere(this.nodeStores.get(node))) {
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
    const listedIn = this.store();
    let nodes: Node[];
    let marksKnown: boolean;
    let notes: Note[] | null;
    let files: { tree: FilesTree; nodes: Map<string, Node> } | null = null;
    try {
      if (node === undefined) {
        /*
         * NEVER: `marksKnown` WAS A LITERAL `true` HERE.
         *
         * The root listing asks for the same marks the subtree listing
         * does, and reported them known whatever came back. Measured in
         * a sixteenth review round: a conflicts answer this build could
         * only partly read drew every top-level block as sound, while
         * the same answer one level down said the marks were not known.
         * It comes from the listing now, at both levels.
         */
        const listing = await this.rootListing(this.model);
        nodes = listing.nodes;
        marksKnown = listing.marksKnown;
        notes = listing.notes;
        files = listing.files;
      } else {
        const listing = await this.model.childrenOf(node.id);
        nodes = listing.nodes;
        marksKnown = listing.marksKnown;
        notes = listing.notes;
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
    /*
     * NOTE: MARKS UNKNOWN BECAUSE A WRITER COULD NOT BE READ ARE NOT MARKS
     * THAT COULD NOT BE ASKED FOR. The first is said by the notes -- the
     * row above the blocks and the status bar's marker -- and the conflict
     * count the store gave for the writers it did read stands; the second
     * is a failure, and clears the count.
     */
    if (asked === this.generation()) {
      this.noted(notes);
    }
    if (!marksKnown && notes === null && asked === this.generation()) {
      this.unknownMarks();
    }
    if (asked !== this.generation()) {
      return [];
    }
    for (const returned of nodes) {
      this.nodeStores.set(returned, listedIn);
    }
    let rows: OutlineElement[] = nodes;
    if (node === undefined && files !== null) {
      rows = this.filesRoot(files.tree, files.nodes, listedIn);
    }
    if (notes === null) {
      return rows;
    }
    return [{ incompleteText: incompleteWarning(notes), under: node === undefined ? null : node.id }, ...rows];
  }
}

/*
 * A FAILURE IS SHOWN WITH THE SENTENCE THE LAYER THAT FOUND IT WROTE.
 * The transport already says which setting to look at when a library is
 * missing and that the child was stopped when it timed out; rewriting
 * those here would produce a second, worse description of each.
 */
/*
 * NOTE: THE THENABLE THE EDITOR RETURNS IS HANDED BACK (queue item 33, D5),
 * at all three levels, so that a caller that follows it -- `IntegrityWatch`
 * -- hears a refusal. A caller that does not follow it discards it as it
 * always did.
 */
function show(notice: Notice): Thenable<unknown> | undefined {
  if (notice.level === 'error') {
    return vscode.window.showErrorMessage(`Theourgia: ${notice.text}`);
  } else if (notice.level === 'warning') {
    return vscode.window.showWarningMessage(`Theourgia: ${notice.text}`);
  } else if (notice.level === 'information') {
    return vscode.window.showInformationMessage(`Theourgia: ${notice.text}`);
  }
  return undefined;
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

/*
 * THE READ-ONLY DOCUMENTS OF `src/document-view.ts`, served under their own
 * scheme. NOTE: READ-ONLY IS THE SCHEME'S, NOT A REQUEST: the editor does
 * not let a document a content provider serves be edited or saved in place.
 * It does offer Save As, which writes a copy to a file the user names -- a
 * copy saved over a block's projection file is that file changed like any
 * other. What text an address gets is decided in `DocumentTexts`, where a
 * cell can ask.
 */
class DocumentViews implements vscode.TextDocumentContentProvider {
  private readonly changed = new vscode.EventEmitter<vscode.Uri>();
  public readonly onDidChange = this.changed.event;

  constructor(private readonly texts: DocumentTexts) {}

  public show(uri: vscode.Uri, text: string): void {
    this.texts.hold(uri.toString(), text);
    this.changed.fire(uri);
  }

  public forget(uri: vscode.Uri): void {
    this.texts.forget(uri.toString());
  }

  public provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    return this.texts.textFor(uri.toString(), uri.query);
  }
}

function reportFailure(e: unknown): void {
  if (e instanceof TransportError) {
    vscode.window.showErrorMessage(`Theourgia: ${e.message}`);
    return;
  }
  vscode.window.showErrorMessage(`Theourgia: ${(e as Error).message ?? String(e)}`);
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
   * activation, and never reused. (section 12.9)
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
      isOpen: (file) => vscode.workspace.textDocuments.some((d) => d.uri.fsPath === file),
      isDirty: (file) => vscode.workspace.textDocuments.some((d) => d.uri.fsPath === file && d.isDirty)
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
   * THE REQUESTS NOBODY IS TO SEND AGAIN. Under the storage root, not
   * under a session: the copies a tombstone is about do not all live in
   * one session's directory, and discarding a session moves its
   * directory away.
   */
  const tombstones = new Tombstones(storage, files);

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
    () => generation,
    () => storeIdentity(),
    (notes) => noteReading(notes),
    {
      get: () => {
        const kept = context.workspaceState.get(modeKey());
        return isViewMode(kept) ? kept : undefined;
      },
      remember: (mode: ViewMode) => context.workspaceState.update(modeKey(), mode),
      show: (mode: ViewMode) => vscode.commands.executeCommand('setContext', 'theourgia.viewMode', mode)
    }
  );
  /*
   * THE MODE IS KEPT PER STORE, under the store's directory as configured.
   * Read when it is asked for, so a store changed in the settings has its
   * own.
   */
  function modeKey(): string {
    return `theourgia.viewMode:${config.store}`;
  }

  /*
   * THE STORE A LISTED ELEMENT, A READ AND A WRITE ARE BOUND TO: the
   * configured directory, resolved. An element listed under one store is
   * refused under another, and an action whose store changed while it
   * waited sends nothing.
   *
   * NOTE: THE SCOPE IS THE SETTINGS SWITCH. A store replaced at the same
   * path, or a link retargeted under it, keeps its identity here; what the
   * store itself refuses as stale (`--if-unchanged`) still covers a block
   * that moved on, but an element listed from the replaced store is not
   * refused by this check.
   */
  function storeIdentity(): string {
    return path.resolve(config.store);
  }

  let config = readConfig();
  let client: Client | null = null;
  let model: StoreModel | null = null;
  let outbox: Outbox | null = null;
  let saver: Saver | null = null;
  /*
   * WHAT THE QUEUE'S WRITES COULD NOT PROMISE, for this window. (queue item
   * 22, design v4) Outlives every rebuild: see `DurabilitySink`.
   */
  const warnings = new DurabilitySink();
  let conflicts: number | null = null;
  /*
   * THE LAST REASON THE STORE COULD NOT BE ASKED, in the core's own
   * words, or null when the last asking worked. See StatusFacts.
   */
  let unreachable: string | null = null;
  /*
   * THE WRITERS THE LAST READING COULD NOT SEE. Every reading the window
   * shows sets it -- the tree, the conflict count, a search, a document,
   * a block, the store's check -- so the marker stays while the last one
   * was incomplete and goes with the first complete one. A save says what
   * it was told in a message and does not set it: the save's report can
   * arrive after the window has moved to another store, and a marker set
   * then would describe the wrong one.
   */
  let incomplete: Note[] | null = null;
  function noteReading(notes: Note[] | null): void {
    incomplete = notes;
    paint();
  }
  /*
   * WHICH SETTINGS A REQUEST WAS MADE UNDER. Every request here is
   * awaited, and a setting can change while one is in flight: a block
   * read from one store would then be written into a file named after
   * another, tagged with that other store, and saved into it. The
   * counter is taken before the wait and checked after; a result from a
   * generation that has been replaced is dropped rather than acted on.
   */
  let generation = 0;

  /*
   * EACH HELD DURABILITY SENTENCE, ONCE, as a warning: the user's work is
   * written and may not survive a power cut.
   */
  function showDurability(): void {
    warnings.showAll((text) => show({ level: 'warning', text }), reportFailure);
  }

  /*
   * EVERY COMMAND IS REGISTERED HERE, AND WHATEVER ITS QUEUE WRITES COULD NOT
   * PROMISE IS SHOWN WHEN IT ENDS. (queue item 22, ruled Q4) The shown
   * sentences are the sink's, whichever Saver or settle raised them, so a
   * command that drains, retries or settles says so after itself; the
   * `finally` covers a command that throws. The census in
   * `awaiting.test.ts` holds that no `registerCommand` in src goes around
   * this.
   */
  function command<A extends unknown[]>(id: string, run: (...args: A) => unknown): vscode.Disposable {
    return vscode.commands.registerCommand(id, async (...args: A) => {
      try {
        return await run(...args);
      } finally {
        showDurability();
      }
    });
  }

  function rebuild(): void {
    generation += 1;
    /*
     * WHAT WAS KNOWN ABOUT THE OLD STORE IS NOT KNOWN ABOUT THE NEW ONE.
     * Keeping the count would show a store nobody has asked as having no
     * conflicts -- which is the one reading a user would act on.
     */
    conflicts = null;
    /*
     * WHAT WAS WRONG WITH THE OLD STORE IS NOT KNOWN ABOUT THE NEW ONE,
     * for the same reason the conflict count goes: a sentence about a
     * store the user has left, standing beside the name of the one they
     * are in, is a reading somebody would act on.
     */
    unreachable = null;
    incomplete = null;
    config = readConfig();
    /*
     * NOTE: THE DIRECTORY IS PROBED HERE, and this call is the reason the
     * argument stopped being optional: it was made with one argument, so
     * the refusal about a corePath holding neither sources nor products
     * was unreachable from the running extension. Found in a third
     * review round.
     */
    const problems = problemsWith(config, coreDirectoryAt(config.corePath));
    if (problems.length > 0) {
      client = null;
      model = null;
      outbox = null;
      saver = null;
      provider.use(null);
      status.text = '$(book) Theourgia $(gear)';
      status.tooltip = problems.map((p) => `${p.setting}: ${p.message}`).join('\n');
      status.show();
      return;
    }
    client = Client.fromConfig(config);
    model = new StoreModel(client);
    /*
     * THE QUEUE IS THIS SESSION'S. One writer -- this process -- so it
     * needs no lock; a queue outside the sessions, however it were
     * numbered, would be two windows writing one file again. (section 12.9,
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
     * X1c (9): THE FILE AN ANSWER IS ABOUT, WHEN THIS WINDOW NEVER SENT IT.
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
     * NOTE: THIS SAVER'S OWN QUEUE AND ITS OWN STORE, captured here.
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
     * NOTE: THIS SAVER'S OWN QUEUE AND ITS OWN STORE, captured here and
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
    const ownIdentity = storeIdentity();
    saver = new Saver(
      client,
      own,
      settlerFor({
        queue: own,
        storeHash: ownStore,
        sessionId,
        sessions,
        saving,
        report: show,
        unrecorded: unrecordedNotice,
        durability: (file, text) => warnings.add(file, text)
      }),
      {
        durability: (file, text, at) => warnings.add(file, text, at),
        /*
         * WHAT THE RECORD BESIDE A FILE HAS ALREADY CONFIRMED, READ
         * FRESH BEFORE EVERY TRANSMISSION. (R8)
         *
         * NOTE: A FUNCTION RATHER THAN A VALUE, because the point is that
         * it is read again: another window may have saved this same
         * file and had it confirmed while this entry sat in the queue.
         * Handing over a number read now would be handing over the
         * answer to a question nobody has asked yet.
         */
        baselineOf: (file: string) => {
          const sidecar = publisher.sidecarOf(file);
          return sidecar === null ? null : { highWater: sidecar.highWater };
        },
        /*
         * WHETHER THIS REQUEST HAS BEEN RETIRED, asked of the storage
         * root rather than of this session's directory: a tombstone has
         * to outlive the session whose queue the request was in, and
         * `discard` moves that directory away.
         */
        retired: (record) => tombstones.isRetired(record.storeHash, record.req),
        /*
         * WHAT THE RECORD BESIDE A FILE SAYS ITS BLOCK IS NOW, read before a
         * write of the block's text goes out (src/saver.ts, modeOf).
         */
        modeOf: (file: string) => {
          const sidecar = publisher.sidecarOf(file);
          return sidecar === null ? undefined : sidecar.mode ?? null;
        },
        /*
         * A KEPT WRITE THE STORE REFUSED AS STALE, whichever drain sent it --
         * the retry command, the one at startup, or the drain in front of
         * another save: said once per drain however many there were, and
         * the view listed again when this Saver's store is still the one
         * shown. Decided by the store, not the generation: a settings
         * change that keeps the store does not silence it.
         */
        stale: () => {
          show({ level: 'warning', text: CHANGED_SINCE_LISTED });
          const current = storeIdentity();
          if (current === ownIdentity) {
            provider.refresh();
          }
        }
      }
    );
    provider.use(model);
    paint();
    /*
     * plugin-r3 item 14: WHAT THE STORE SAYS ABOUT ITS OWN CONDITION, asked
     * when a session starts on it. Scheduled, not awaited, for the reason
     * the drain below gives; whether to ask and whether to say anything is
     * `IntegrityWatch`, and what is said is `integrityNotice`.
     */
    checkIntegrity();
    /*
     * NOTE: AND THE QUEUE IS DRAINED, because nothing else was going to.
     *
     * A window that starts with entries already on disk -- a save that
     * was interrupted, work carried in by a takeover, a store that was
     * unreachable last time -- used to sit on them until the user saved
     * something else or ran the retry command by hand. The queue's whole
     * purpose is that unsent work is not lost, and "not lost" reads as
     * "not sent" to somebody watching their block stay a draft.
     *
     * It is scheduled rather than awaited: `rebuild` is called from
     * activation and from every settings change, and neither should wait
     * on the store. Failures are reported through the same path a retry
     * uses; a rejection here must not take the rebuild with it.
     */
    const draining = saver;
    void Promise.resolve().then(async () => {
      if (draining === null || saver !== draining) {
        return;
      }
      try {
        await draining.retry();
      } catch (e) {
        reportFailure(e);
      }
      /*
       * NOTE: SHOWN BEFORE THE CHECK BELOW, because the sink is the window's:
       * a warning raised by a Saver that has since been replaced is still
       * about this window's queue file (D4b).
       */
      showDurability();
      /*
       * THE CHECK AFTER THE WAIT LEAVES, rather than wrapping the work
       * it protects. The two read the same here, because painting is the
       * last thing this callback does -- and they do not read the same to
       * the census in `awaiting.test.ts`, which asks whether a guard
       * STOPS something. A comparison whose branch merely contains the
       * rest of the body cannot be told apart from one that was quietly
       * defanged, and that is the very substitution (`return` replaced by
       * `void asked`) the census was written after.
       */
      if (saver !== draining) {
        return;
      }
      paint();
    });
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
      blocked: saver?.blockedBecause ?? null,
      incomplete,
      unreachable
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
   * ONCE PER STORE PER SESSION, decided by `IntegrityWatch`; this only
   * hands it the window's store, model and generation. One watch for the
   * whole session, so that what it was told survives every rebuild.
   */
  const integrity = new IntegrityWatch();
  /*
   * THE EXTENSION'S OUTPUT CHANNEL, named as the extension is shown. It
   * holds what could not be said any other way: a warning about a store
   * that the editor failed to show (queue item 18).
   */
  const channel = vscode.window.createOutputChannel('Theourgia');
  context.subscriptions.push(channel);

  /*
   * THE HOVER (src/hover.ts). It answers about the store the document came
   * from, and only through that store's client: the configured store's, when
   * the document came from it; none otherwise.
   */
  const hover = new HoverService({
    clientFor: (store: string) => (client !== null && path.resolve(store) === storeIdentity() ? client : null),
    log: (line: string) => channel.appendLine(line),
    now: () => Date.now()
  });
  provider.onRefresh = () => hover.drop();

  function checkIntegrity(): void {
    if (model === null) {
      return;
    }
    const asking = model;
    void integrity.check({
      store: config.store,
      ask: async () => {
        const asked = generation;
        const verdict = await asking.storeVerdict();
        if (asked !== generation) {
          return verdict;
        }
        if (verdict.known) {
          noteReading(verdict.notes ?? null);
        }
        return verdict;
      },
      show,
      generation: () => generation,
      record: (line) => channel.appendLine(line)
    });
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
    let because: string | null = null;
    let notes: Note[] | null = incomplete;
    try {
      const reading = await model.conflictReading();
      found = reading.count;
      notes = reading.notes;
    } catch (e) {
      found = null;
      /*
       * KEY: THE REASON IS KEPT, NOT ONLY THE FAILURE. This catch used to
       * discard `e` and leave a question mark on the status bar, and the
       * thing discarded was the core's own sentence -- `serve-path-
       * occupied (path "...")` names a directory the user can remove.
       * "Something went wrong" is not something anybody can act on.
       */
      because = e instanceof Error ? e.message : String(e);
    }
    if (asked !== generation) {
      return;
    }
    conflicts = found;
    unreachable = because;
    incomplete = notes;
    paint();
  }

  const datumScratch = scratchUnder(path.join(storage, 'datum-view'));
  /*
   * THE TEXT A RESTORED DATUM TAB IS GIVEN: the datum export of the store its
   * address names, which DocumentTexts has compared with the configured one.
   */
  const composeDatum = async (asking: Client, id: string): Promise<Composed> => {
    const view = await datumViewOf(asking, id, datumScratch);
    return view.ok ? { ok: true, title: id, text: view.text } : { ok: false, reason: view.reason, ids: [] };
  };
  const views = new DocumentViews(
    new DocumentTexts(() => (client === null ? null : { client, store: config.store }), documentOf, composeDatum)
  );

  /*
   * A SPLIT OF THE SOURCE FILE IN THE EDITOR. The steps are in
   * `src/split-symbols.ts` (`runSplit`); this hands them the editor's
   * document, its symbol providers, the disk and the core, and then opens
   * the review file the core wrote and says what it proposed.
   *
   * NOTE: A REAL SOURCE FILE ONLY. A block's own file is one block already,
   * and a document view is not a file; the core splits a file on disk.
   */
  async function suggestSplit(): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (editor === undefined) {
      vscode.window.showWarningMessage('Theourgia: open the source file to split first.');
      return;
    }
    const document = editor.document;
    const within = path.relative(storage, document.uri.fsPath);
    if (document.uri.scheme !== 'file' || (!within.startsWith('..') && !path.isAbsolute(within))) {
      vscode.window.showWarningMessage("Theourgia: only a source file on disk is split this way, not a block's own file or a document view.");
      return;
    }
    const using = client;
    if (using === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const asked = generation;
    const symbolsDirectory = path.join(storage, 'symbols');
    let outcome;
    try {
      outcome = await runSplit(
        {
          uri: document.uri.toString(),
          fsPath: document.uri.fsPath,
          languageId: document.languageId,
          isDirty: () => document.isDirty,
          version: () => document.version,
          text: () => document.getText(),
          save: () => Promise.resolve(document.save())
        },
        {
          client: using,
          editorVersion: vscode.version,
          readFile: (file) => files.readBytes(file),
          symbols: () => Promise.resolve(vscode.commands.executeCommand('vscode.executeDocumentSymbolProvider', document.uri)),
          symbolsPath: () => path.join(symbolsDirectory, `${randomUUID()}.sexp`),
          writeSymbols: (written, text) => {
            files.makeDirectory(symbolsDirectory);
            files.writeText(written, text);
          },
          /*
           * A file that never came to exist -- the write failed before
           * creating it -- is nothing to remove, and is not reported.
           */
          removeSymbols: (written) => {
            try {
              files.unlink(written);
            } catch (e) {
              if ((e as NodeJS.ErrnoException).code !== 'ENOENT') {
                vscode.window.showWarningMessage(`Theourgia: the symbols file ${written} could not be removed: ${String(e)}`);
              }
            }
          }
        }
      );
    } catch (e) {
      reportFailure(e);
      return;
    }
    if (outcome.done === 'stopped') {
      vscode.window.showWarningMessage(`Theourgia: ${outcome.why}.`);
      return;
    }
    if (outcome.done === 'refused') {
      vscode.window.showErrorMessage(`Theourgia: ${splitRefusalNotice(outcome.refusal)}`);
      return;
    }
    if (asked !== generation) {
      vscode.window.showInformationMessage(`Theourgia: ${outcome.notice}; the review is at ${outcome.review}.`);
      return;
    }
    const review = await vscode.workspace.openTextDocument(vscode.Uri.file(outcome.review));
    await vscode.window.showTextDocument(review, { preview: false });
    vscode.window.showInformationMessage(`Theourgia: ${outcome.notice}.`);
  }

  /*
   * FACTS THE EDITOR COMPUTES, SUPPLIED TO THE STORE. The steps are in
   * `src/supply.ts` (`runSupply`); this hands them the core, a directory in
   * this extension's storage for the projection (one per store and writer),
   * the disk and the editor's providers, and says what came of it.
   *
   * Signatures, keywords and calls are the committed store's; diagnostics
   * are this window's writer's, from its working view. The supply goes to
   * the store the command started on, through the client taken then.
   */
  async function supplyFacts(kind: SupplyKind): Promise<void> {
    const using = client;
    if (using === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const store = storeIdentity();
    /*
     * NEVER: ANOTHER WRITER'S VIEW. The diagnostics are of what this window
     * is editing, which is in its own draft space; `theourgia.writer` names
     * a space none of its drafts are in.
     */
    const writer = kind === 'diagnostics' ? windowWriter(sessionId) : null;
    const directory = path.join(storage, 'projection', storeHash(config.store), writer === null ? '-' : encodeURIComponent(writer));
    const supplyDirectory = path.join(storage, 'supply');
    let changes = 0;
    const watching = vscode.languages.onDidChangeDiagnostics((event) => {
      const inside = event.uris.some((uri) => uri.scheme === 'file' && insideDirectory(directory, uri.fsPath));
      if (inside) {
        changes += 1;
      }
    });
    let outcome: SupplyOutcome;
    try {
      outcome = await runSupply(kind, writer, {
        client: using,
        editorVersion: vscode.version,
        directory,
        emptyDirectory: (d) => emptyDirectory(d),
        filesUnder: (d) => filesUnder(d),
        readFile: (file) => files.readBytes(file),
        fsPathOf: (d, relative) => path.join(d, ...relative.split('/')),
        open: (file) =>
          Promise.resolve(vscode.workspace.openTextDocument(vscode.Uri.file(file))).then((document) => ({
            uri: document.uri.toString(),
            languageId: document.languageId,
            version: () => document.version,
            text: () => document.getText(),
            isDirty: () => document.isDirty
          })),
        symbols: (uri) => Promise.resolve(vscode.commands.executeCommand('vscode.executeDocumentSymbolProvider', vscode.Uri.parse(uri))),
        hover: (uri, at) =>
          Promise.resolve(vscode.commands.executeCommand('vscode.executeHoverProvider', vscode.Uri.parse(uri), new vscode.Position(at.line, at.character))),
        prepareCalls: (uri, at) =>
          Promise.resolve(vscode.commands.executeCommand('vscode.prepareCallHierarchy', vscode.Uri.parse(uri), new vscode.Position(at.line, at.character))),
        outgoingCalls: (item) => Promise.resolve(vscode.commands.executeCommand('vscode.provideOutgoingCalls', item)),
        settling: {
          changes: () => changes,
          /*
           * A clock that only moves forward: a wall clock set back would
           * stretch the wait, and one set forward would end it early.
           */
          now: () => performance.now(),
          sleep: (ms) => new Promise((resolve) => setTimeout(resolve, ms))
        },
        diagnostics: (uri) => vscode.languages.getDiagnostics(vscode.Uri.parse(uri)),
        stillCurrent: () => storeIdentity() === store,
        supplyPath: () => path.join(supplyDirectory, `${randomUUID()}.sexp`),
        writeSupply: (written, text) => {
          files.makeDirectory(supplyDirectory);
          files.writeText(written, text);
        },
        /*
         * A file that never came to exist -- the write failed before creating
         * it -- is nothing to remove, and is not reported; any other failure
         * is thrown, and the command's sentence says it.
         */
        removeSupply: (written) => {
          try {
            files.unlink(written);
          } catch (e) {
            if ((e as NodeJS.ErrnoException).code !== 'ENOENT') {
              throw e;
            }
          }
        }
      });
    } catch (e) {
      /*
       * AN ERROR SAYS WHICH STORE IT HAPPENED IN when the window has
       * switched since: what it says was sent went to that store. A thrown
       * value that is not an Error is shown as one.
       */
      const error = e instanceof Error ? e : new Error(String(e));
      if (storeIdentity() !== store) {
        error.message = `for the store ${store}: ${error.message}`;
      }
      reportFailure(error);
      return;
    } finally {
      watching.dispose();
    }
    const said =
      outcome.done === 'ran' ? supplyNotice(kind, outcome) : outcome.done === 'refused' ? outcome.notice : outcome.why;
    const current = storeIdentity();
    if (current !== store) {
      vscode.window.showInformationMessage(`Theourgia: for the store ${store}: ${said}.`);
      return;
    }
    if (outcome.done === 'refused') {
      vscode.window.showErrorMessage(`Theourgia: ${said}.`);
    } else if (
      outcome.done === 'stopped' ||
      outcome.results.some((r) => r.done === 'refused') ||
      outcome.incomplete ||
      outcome.notRemoved.length > 0
    ) {
      vscode.window.showWarningMessage(`Theourgia: ${said}.`);
    } else {
      vscode.window.showInformationMessage(`Theourgia: ${said}.`);
    }
  }

  /*
   * THE FILES VIEW'S MODE AND ACTIONS. What they decide -- the paths, the
   * intent, the sentences -- is in `src/directory-view.ts`; every write goes
   * through the Saver: a new document is one batch, a move or a rename is
   * one write of the block's `path`.
   */
  /*
   * The title bar's key is set by the listing this redraw makes, for the
   * store it lists.
   */
  async function showMode(mode: ViewMode): Promise<void> {
    const asked = generation;
    await context.workspaceState.update(modeKey(), mode);
    if (asked !== generation) {
      return;
    }
    provider.refresh();
  }

  /*
   * NOTE: A SETTINGS CHANGE THAT KEEPS THE STORE DOES NOT REFUSE A ROW. The
   * rows listed before it name blocks of the same store, and every action
   * reads what it needs afresh; only a row of another store is refused.
   */
  function listedElsewhere(element: OutlineElement): boolean {
    const listed = provider.storeOf(element);
    const current = storeIdentity();
    if (listed !== undefined && listed !== current) {
      vscode.window.showWarningMessage(LISTED_ELSEWHERE);
      return true;
    }
    return false;
  }

  /*
   * A NEW DOCUMENT IS AN INSERT, WITH NO VERSION: there is no block yet to
   * have moved on. A path another block holds is the store's and export's
   * business, and is shown in the listing as it is now.
   */
  async function newFileHere(row?: DirectoryRow): Promise<void> {
    if (row !== undefined && (!isDirectoryRow(row) || listedElsewhere(row))) {
      return;
    }
    const directory = row === undefined ? '' : row.directory.prefix;
    const asked = generation;
    const store = storeIdentity();
    const name = await vscode.window.showInputBox({
      prompt: directory === '' ? 'Name of the new document' : `Name of the new document in ${directory}`,
      placeHolder: 'name.md'
    });
    if (name === undefined) {
      return;
    }
    const current = storeIdentity();
    if (current !== store) {
      vscode.window.showWarningMessage('Theourgia: the store changed while the name was being asked for. Select the directory again.');
      return;
    }
    const sending = saver;
    if (sending === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const target = newDocumentPath(directory, name);
    if (target === null) {
      vscode.window.showErrorMessage(`Theourgia: ${name.trim()} does not make a path export would write.`);
      return;
    }
    let outcome: SaveOutcome;
    try {
      outcome = await sending.batch(newDocumentIntent(target, name.trim().replace(/\.md$/, '')));
    } catch (e) {
      reportFailure(e);
      paint();
      return;
    }
    /*
     * THE OUTCOME IS SAID EITHER WAY: the request went to the queue of the
     * store it was made in. Only the redraw is for the current store.
     */
    if (asked !== generation) {
      show(pathActionNotice('created', outcome, target));
      return;
    }
    show(pathActionNotice('created', outcome, target));
    paint();
    provider.refresh();
  }

  /*
   * A MOVE OR A RENAME STARTS FROM THE BLOCK AS IT IS NOW. The row is an id:
   * its path and version are read at the moment of the action, the prompt
   * shows that path, and the write is conditional on that version, so a
   * block changed after the read -- by another window, another row, or a
   * change made while the prompt was open -- is refused by the store as
   * `changed` rather than overwritten. The read, the prompt and the send
   * belong to one store: a store switched meanwhile sends nothing.
   */
  async function changePath(node: ListedNode | undefined, how: 'move' | 'rename'): Promise<void> {
    if (node === undefined || node.file === undefined || listedElsewhere(node)) {
      return;
    }
    const reading = model;
    if (reading === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const asked = generation;
    const store = storeIdentity();
    let fresh: { block: Block | null; version: string | null };
    try {
      fresh = await reading.blockWithVersion(node.id);
    } catch (e) {
      reportFailure(e);
      return;
    }
    const readIn = storeIdentity();
    if (readIn !== store) {
      vscode.window.showWarningMessage('Theourgia: the store changed while the file was being read. Select the file again.');
      return;
    }
    const version = fresh.version;
    const now = fresh.block === null ? null : candidateOf(fresh.block, node.title, false, false).path;
    if (fresh.block === null || version === null || now === null) {
      vscode.window.showWarningMessage(`Theourgia: ${node.id} no longer has a path to ${how}; the view is refreshed.`);
      provider.refresh();
      return;
    }
    const at = now.lastIndexOf('/');
    const answer = await vscode.window.showInputBox(
      how === 'move'
        ? { prompt: `Directory to move ${now} to`, value: at < 0 ? '' : now.slice(0, at) }
        : { prompt: `New name for ${now}`, value: now.slice(at + 1) }
    );
    if (answer === undefined) {
      return;
    }
    /*
     * CHECKED RIGHT BEFORE THE SEND, and the Saver taken here: a store
     * switched while the prompt was open must not receive a write computed
     * from the other store's block.
     */
    const sendIn = storeIdentity();
    if (sendIn !== store) {
      vscode.window.showWarningMessage('Theourgia: the store changed while the name was being asked for. Select the file again.');
      return;
    }
    const sending = saver;
    if (sending === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const target = how === 'move' ? movedPath(now, answer) : renamedPath(now, answer);
    if (target === null) {
      vscode.window.showErrorMessage(`Theourgia: ${answer.trim()} does not make a path export would write.`);
      return;
    }
    if (target === now) {
      return;
    }
    let outcome: SaveOutcome;
    try {
      outcome = await sending.save(node.id, 'path', target, version);
    } catch (e) {
      reportFailure(e);
      paint();
      return;
    }
    const said = pathActionNotice(how === 'move' ? 'moved to' : 'renamed to', outcome, target);
    if (asked !== generation) {
      show(said);
      return;
    }
    show(said);
    paint();
    provider.refresh();
  }

  /*
   * THE SUBTREE UNDER A NODE, AS ONE READ-ONLY DOCUMENT (queue item 6). It
   * is composed before anything is opened, so a refusal is said as a
   * message rather than as an editor that could not load; opening it again
   * composes it again.
   */
  async function openAsDocument(node?: Node): Promise<void> {
    if (node === undefined) {
      return;
    }
    if (listedElsewhere(node)) {
      return;
    }
    if (client === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const asked = generation;
    const store = config.store;
    let composed: Composed;
    try {
      composed = await documentOf(client, node.id);
    } catch (e) {
      reportFailure(e);
      return;
    }
    if (asked !== generation) {
      vscode.window.showWarningMessage('Theourgia: the store changed while the document was being read. Select the block again.');
      return;
    }
    if (!composed.ok) {
      vscode.window.showErrorMessage(`Theourgia: ${refusalOf(composed)}`);
      return;
    }
    const uri = vscode.Uri.from({
      scheme: DOCUMENT_SCHEME,
      path: `/${projectionNameFor(composed.title, node.id)}`,
      query: documentQuery(store, node.id)
    });
    views.show(uri, composed.text);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document, { preview: false });
    const notes = composed.notes ?? null;
    if (asked === generation) {
      noteReading(notes);
    }
    if (notes !== null) {
      showIncompleteBanner(editor, notes, context.subscriptions);
    }
  }

  /*
   * NOTE: OPENING ANSWERS WHAT IT OPENED -- the file shown and the length of
   * the prefix the editor shows before the source -- so that go to
   * definition can place a line in it. The command ignores the answer.
   */
  /*
   * NOTE: THE SECOND ARGUMENT IS THE STORE THE ID WAS LISTED OR FOUND IN,
   * carried by a tree item's command and by a search's open; an id from
   * another store is refused before anything is read.
   */
  async function openBlock(id: string, sourceStore?: string): Promise<OpenedBlock | undefined> {
    const current = storeIdentity();
    if (sourceStore !== undefined && sourceStore !== current) {
      vscode.window.showWarningMessage(LISTED_ELSEWHERE);
      return;
    }
    if (model === null) {
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return;
    }
    const asked = generation;
    const store = config.store;
    const reading = client as Client;
    /*
     * THE TICKET IS TAKEN BEFORE THE READ, so that two opens of one
     * block are ordered by when they asked the store rather than by
     * which of them finished first. See src/open.ts.
     */
    const directory = sessions.directoryFor(sessionId, storeHash(store), id);
    /*
     * THE NOT-TEXT READS OF THIS BLOCK SEEN BEFORE THIS ONE (Publisher,
     * `invalidationsOf`): the publication below records text only if no
     * other read found the block not text meanwhile.
     */
    const seenBefore = publisher.invalidationsOf(directory);
    let block;
    let notes: Note[] | null;
    try {
      const reading = await model.blockReading(id);
      block = reading.block;
      notes = reading.notes;
    } catch (e) {
      /*
       * A READ THAT FAILED DID NOT SHOW TEXT, so no record of this block
       * goes on saying it did.
       */
      publisher.recordModeOfBlock(directory, null);
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
     * XO-01/02/10 exercise the recorded-store comparisons at entry and
     * inside the acceptance chain. Directory spelling is not identity.
     *
     * Do not add a check back here without a cell that fails when it is
     * removed.
     */
    if (asked !== generation) {
      vscode.window.showWarningMessage(
        `Theourgia: the store setting changed while ${id} was being read, so it was not opened.`
      );
      return;
    }
    if (block === null) {
      publisher.recordModeOfBlock(directory, null);
      vscode.window.showWarningMessage(`Theourgia: the store has no block ${id}.`);
      return;
    }
    /*
     * WHAT THIS READ SAYS THE BLOCK IS, written at once into every record of
     * it this session holds when it is not text (datum, or a mode this build
     * does not know), before anything below can be refused; text is written
     * only by the publication of this very reading.
     */
    const openedMode = recordedModeOf(block);
    if (openedMode !== 'text') {
      publisher.recordModeOfBlock(directory, openedMode);
    }
    /*
     * NEVER: A DATUM BLOCK OPENED AS TEXT TO EDIT. Its code is its `body`;
     * the working read below projects `src`, which it does not have, and a
     * save would write `src` beside the body and change nothing the store
     * runs (src/datum-view.ts). It is shown read-only instead, and nothing
     * is published, written or committed for it.
     */
    if (isDatumBlock(block)) {
      return openDatumBlock(id, block, store, reading);
    }
    // The store was captured before the read. Acceptance checks recorded store
    // identity again inside the save chain, before numbering (XO-01/02/10).

    const prefix = prefixOf(block);
    let shownPrefix = prefix;

    /*
     * THE PUBLICATION AND EVERYTHING THAT READS IT ARE ON ONE CHAIN,
     * keyed by the directory these versions share, so a save arriving
     * for this block waits rather than interleaving. (section 12.11.1)
     */
    const outcome = await chain.run(directory, async () => {
      /*
       * THE RECORD THIS OPEN WILL REPLACE, READ BEFORE THE READING. (queue
       * item 43) A save of this block lands on this chain after the open, but
       * a settlement or another window's write does not wait for it; the
       * publication names this revision and is refused `record-moved` if the
       * record moved while the working copy was read.
       */
      const expected = publisher.revisionIn(directory);
      const projection = await new Working(reading, windowWriter(sessionId)).read(id,prefix);
      shownPrefix = projection.prefix;
      return publisher.publish({directory,storeId:store,blockId:id,mode:openedMode ?? undefined,modeSeenAt:seenBefore,lang:blockLang(block) ?? undefined,prefix:projection.prefix,
        text:projection.prefix+projection.body,cursor:null,projection:projection.source,expected});
    }).catch((e: unknown) => {
      /*
       * A READING THE CORE REFUSED IS SAID BY NAME -- a text-mode block whose
       * bytes are not UTF-8 is `working-unavailable non-text-projection` --
       * and nothing is opened: an empty editor would read as an empty block.
       */
      reportFailure(e);
      return null;
    });
    if (outcome === null) {
      return;
    }

    if (!outcome.published) {
      vscode.window.showWarningMessage(outcome.because==='dirty-document'
        ? 'The store has newer content. Your unsaved edits are preserved; save or resolve them before updating.'
        : outcome.seen !== undefined
          ? `The block's file was not updated: its folder holds ${outcome.seen.join(', ')}, and this extension cannot tell which one is the block's. Keep them, and remove or move the ones that are not.`
          : `The current file was not updated (${outcome.because}). Keep the file and resolve its working or migration state before retrying.`);
      if (outcome.file !== null && !files.exists(outcome.file)) return;
      /*
       * The editor holds the path this would have written. Showing what
       * is there is the answer; writing is not. (section 12.13.1)
       */
      if (outcome.file !== null) {
        const already = await vscode.workspace.openTextDocument(vscode.Uri.file(outcome.file));
        await vscode.window.showTextDocument(already, { preview: false });
        /*
         * THE FILE KEPT, WITH THE PREFIX IT WAS WRITTEN WITH: its record says
         * which, and a draft left in it is what the person sees.
         */
        const kept = publisher.sidecarOf(outcome.file)?.prefix ?? shownPrefix;
        return { uri: already.uri, prefixLength: kept.length };
      }
      return;
    }

    const opened = await vscode.workspace.openTextDocument(vscode.Uri.file(outcome.file));
    await showInLanguageOf(opened, block);
    const editor = await vscode.window.showTextDocument(opened, { preview: false });
    if (asked === generation) {
      noteReading(notes);
    }
    if (notes !== null) {
      showIncompleteBanner(editor, notes, context.subscriptions);
    }
    return { uri: opened.uri, prefixLength: shownPrefix.length };
  }

  /*
   * A DATUM BLOCK, READ-ONLY: its library's file as the datum export writes
   * it, under the read-only scheme, opened at the block's own place. The
   * prefix length answered is where the block's source starts, so go to
   * definition finds the definition's line from there.
   *
   * NOTE: NO GENERATION CHECK AFTER THE EXPORT. The address names the store
   * the export was asked of, so the tab says which store its text is from,
   * and a restored tab is not read from another store under the same id.
   */
  async function openDatumBlock(id: string, block: Block, store: string, asking: Client): Promise<OpenedBlock | undefined> {
    let view;
    try {
      view = await datumViewOf(asking, id, datumScratch);
    } catch (e) {
      reportFailure(e);
      return;
    }
    if (!view.ok) {
      vscode.window.showErrorMessage(`Theourgia: ${id} was not opened: ${view.reason}.`);
      return;
    }
    const uri = vscode.Uri.from({
      scheme: DOCUMENT_SCHEME,
      path: `/${view.file}`,
      query: documentQuery(store, id, 'datum')
    });
    views.show(uri, view.text);
    const document = await vscode.workspace.openTextDocument(uri);
    await showInMode(document, datumLanguageOf(block));
    const at = positionOf(view.text, view.offset);
    const place = new vscode.Position(at.line, at.character);
    await vscode.window.showTextDocument(document, { preview: false, selection: new vscode.Range(place, place) });
    vscode.window.showInformationMessage(`Theourgia: ${datumNotice(id)}`);
    return { uri, prefixLength: view.offset };
  }

  /*
   * WHERE A DEFINITION RECORD OPENS: the block's file and the line in it.
   * The line is found in the document as it is displayed -- a draft with
   * lines of its own included -- after the prefix the editor shows; an
   * export opens its library's first line, because it names no line.
   */
  async function definitionTarget(record: DefinitionRecord): Promise<{ uri: vscode.Uri; line: number } | undefined> {
    const asked = generation;
    const opened = await openBlock(record.id);
    if (asked !== generation) {
      return undefined;
    }
    if (opened === undefined) {
      return undefined;
    }
    const document = await vscode.workspace.openTextDocument(opened.uri);
    if (asked !== generation) {
      return undefined;
    }
    return { uri: opened.uri, line: targetLine(record, document.getText(), opened.prefixLength) };
  }

  /*
   * GO TO DEFINITION, AS A COMMAND: the name under the cursor, the store's
   * answer, a picker when there are several, and the chosen one opened at
   * its line. A name nobody defines is said, with the nearest ones.
   */
  async function goToDefinition(): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (editor === undefined || client === null) {
      vscode.window.showInformationMessage('Theourgia: open a block and put the cursor on a name first.');
      return;
    }
    const at = editor.selection.active;
    const name = identifierAt(editor.document.getText(), at.line, at.character);
    if (name === null) {
      vscode.window.showInformationMessage('Theourgia: there is no name under the cursor.');
      return;
    }
    const asked = generation;
    let answer: DefinitionAnswer;
    try {
      answer = await definitionsOf(client, name);
    } catch (e) {
      reportFailure(e);
      return;
    }
    if (asked !== generation) {
      return;
    }
    if (answer.notes !== null) {
      show({ level: 'information', text: incompleteWarning(answer.notes) });
    }
    if (!('found' in answer)) {
      vscode.window.showInformationMessage(`Theourgia: ${noDefinitionNotice(name, answer.nearest)}`);
      return;
    }
    let chosen = answer.found[0];
    if (answer.found.length > 1) {
      const picked = await vscode.window.showQuickPick(
        answer.found.map((record) => ({ label: recordLabel(record), description: record.id, record })),
        { title: name }
      );
      if (asked !== generation) {
        return;
      }
      if (picked === undefined) {
        return;
      }
      chosen = picked.record;
    }
    const target = await definitionTarget(chosen);
    if (asked !== generation) {
      return;
    }
    if (target === undefined) {
      return;
    }
    await vscode.window.showTextDocument(target.uri, {
      preview: false,
      selection: new vscode.Range(target.line, 0, target.line, 0)
    });
  }

  /*
   * THE EDITOR'S OWN GO TO DEFINITION, on a block's file and on a document
   * view: the same answer, as locations. Only definitions are locations; an
   * export names no line, and a name nobody defines has none.
   *
   * NOTE: A BLOCK'S FILE IS SELECTED BY WHERE IT IS, NOT BY ITS LANGUAGE. A
   * prose block opens as markdown and a code block in its own language, and
   * both are this extension's files under its storage.
   */
  const definitions: vscode.DefinitionProvider = {
    provideDefinition: async (document: vscode.TextDocument, position: vscode.Position) => {
      const name = identifierAt(document.getText(), position.line, position.character);
      const asking = client;
      if (name === null || asking === null) {
        return undefined;
      }
      const asked = generation;
      const answer = await definitionsOf(asking, name);
      if (asked !== generation) {
        return undefined;
      }
      if (!('found' in answer)) {
        return undefined;
      }
      const locations: vscode.Location[] = [];
      for (const record of answer.found.filter((r) => r.kind === 'def')) {
        const target = await definitionTarget(record);
        if (asked !== generation) {
          return undefined;
        }
        if (target !== undefined) {
          locations.push(new vscode.Location(target.uri, new vscode.Position(target.line, 0)));
        }
      }
      return locations;
    }
  };

  /*
   * WHERE A HOVER IS ASKED (D1), from the document's address or record: a
   * composed document says its view and its store; a block's file says its
   * store and block in its record. Anything else is not a place the hover
   * answers about.
   */
  function hoverPlaceOf(document: vscode.TextDocument): HoverPlace | null {
    if (document.uri.scheme === DOCUMENT_SCHEME) {
      const asked = readDocumentQuery(document.uri.query);
      if (asked === null) {
        return null;
      }
      return asked.view === 'datum' ? { kind: 'datum', store: asked.store } : { kind: 'subtree', store: asked.store };
    }
    if (document.uri.scheme !== 'file') {
      return null;
    }
    const sidecar = publisher.sidecarOf(document.uri.fsPath);
    /*
     * A RECORD THAT NAMES NO STORE PLACES NOTHING: an empty store id would
     * resolve to the working directory and could match the configured store.
     */
    if (sidecar === null || sidecar.storeId.length === 0 || sidecar.blockId.length === 0) {
      return null;
    }
    /*
     * THE BLOCK'S LANG FROM ITS RECORD, written where the file was made; the
     * editor's language id only for a record that has none.
     */
    const lang = sidecar.lang ?? document.languageId;
    return { kind: 'block', store: sidecar.storeId, blockId: sidecar.blockId, scheme: isSchemeLang(lang) };
  }

  /*
   * THE HOVER: T1 by the place, the name by the reader the place calls for
   * (D2), and what the store holds about them (src/hover.ts). The links are
   * the two commands HOVER_COMMANDS names, and no other.
   */
  const hovers: vscode.HoverProvider = {
    provideHover: async (document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken) => {
      const place = hoverPlaceOf(document);
      if (place === null) {
        return undefined;
      }
      const text = document.getText();
      const t1 = hoverBlockAt(place, text, document.offsetAt(position));
      if (t1 === null) {
        return undefined;
      }
      const scheme = readsSchemeNames(place);
      const name = nameUnder(place, text, position.line, position.character, () => {
        const range = document.getWordRangeAtPosition(position);
        return range === undefined ? null : document.getText(range);
      });
      if (name === null) {
        return undefined;
      }
      const answer = await hover.answer(place.store, t1, name, scheme, token);
      if (answer === undefined) {
        return undefined;
      }
      const markdown = new vscode.MarkdownString(hoverMarkdown(answer, path.resolve(place.store), OPEN_BLOCK.id, HOVER_MORE.id, name));
      markdown.isTrusted = { enabledCommands: [...HOVER_COMMANDS] };
      return new vscode.Hover(markdown);
    }
  };

  /*
   * EVERYTHING A HOVER FOUND, run from its "N more" line: every candidate,
   * read, in the hover's order, and the chosen one opened in the store it
   * came from.
   */
  async function hoverMore(store?: string, name?: string, candidates?: unknown): Promise<void> {
    if (typeof store !== 'string' || !Array.isArray(candidates)) {
      vscode.window.showInformationMessage('Theourgia: hover a name first; this lists what its hover found.');
      return;
    }
    const asking = client;
    if (asking === null || store !== storeIdentity()) {
      vscode.window.showWarningMessage(LISTED_ELSEWHERE);
      return;
    }
    let entries;
    try {
      entries = await listEntries(asking, candidates);
    } catch (e) {
      reportFailure(e);
      return;
    }
    const picked = await vscode.window.showQuickPick(
      entries.map((e) => ({ label: e.label, description: e.description, id: e.id })),
      { placeHolder: `What the store holds about ${String(name ?? '')}` }
    );
    if (picked !== undefined) {
      await openBlock(picked.id, store);
    }
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
   * because that file is the only copy of what they typed. (section 12.15
   * structure one, section 12.11.7)
   *
   * IT TAKES AN OPTIONAL PATH so that it can be reached from somewhere
   * other than the active editor, and answers with the notice it showed
   * rather than only showing it: a message that is displayed and not
   * returned is a message no cell can read. NOTE: THAT HOLDS FOR THE
   * OUTCOMES OF THE RECONCILIATION AND NOT FOR EVERY EXIT: the paths
   * that give up before one -- no store configured, the store could not
   * be read, the settings changed underneath, no such block -- show a
   * plain warning and answer `null`. They are about the command not
   * running rather than about what it did.
   *
   * NOTE: THE THIRD TEXT IS REPORTED AND NOT SHOWN. `reconcile` produces
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
      vscode.window.showWarningMessage('Theourgia: set theourgia.corePath and theourgia.store first.');
      return null;
    }
    const asked = generation;
    const reconciling = client as Client;
    const seenAtRead = publisher.invalidationsOf(path.dirname(file));
    let block;
    try {
      block = await model.blockOf(sidecar.blockId);
    } catch (e) {
      publisher.recordModeOfBlock(path.dirname(file), null);
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
        `Theourgia: the store setting changed while ${sidecar.blockId} was being read, so nothing ` +
          'was reconciled.'
      );
      return null;
    }
    if (block === null) {
      publisher.recordModeOfBlock(path.dirname(file), null);
      vscode.window.showWarningMessage(`Theourgia: the store has no block ${sidecar.blockId}.`);
      return null;
    }
    /*
     * ONLY A TEXT BLOCK IS RECONCILED. A datum block, or one whose mode this
     * build does not know, is refused, and every record of it this session
     * holds is made to say what was read -- datum, or no mode -- so that a
     * text mode an earlier record held is not left to wave a save through.
     */
    const known = recordedModeOf(block);
    if (known !== 'text') {
      publisher.recordModeOfBlock(path.dirname(file), known);
      const notice = refusalNotice(sidecar.blockId, file, known === 'datum'
        ? {because:'datum-block'}
        : {because:'working-unavailable',detail:`the mode of ${sidecar.blockId} is not one this editor knows, so it is not reconciled`});
      show(notice);
      paint();
      return notice;
    }
    const document = documentFor(block, sidecar.storeId);
    const directory = path.dirname(file);
    /*
     * ON THE CHAIN, because it reads the file and may publish beside it,
     * and a save arriving for this block has to wait rather than
     * interleave with it. (section 12.11.1)
     */
    const outcome = await chain.run(directory, async () => {
      const leaving=publisher.reconcileLeaving(file,document.prefix,document.text);
      const result=leaving.answer;
      /*
       * THE RECORD THE OFFER IS BUILT FROM: its revision and its projection,
       * taken in the first wait (queue item 43, T10/T11). The read at the top
       * of this function stays for routing only. They come back WITH the
       * answer, read inside the same exclusive section after any write the
       * automatic route made (review r1 #1): a read after that section could
       * see a record another process wrote after the offer was built. The
       * second wait checks the revision before anything else and writes the
       * working note against this projection, never a re-read one.
       */
      const heldRecord = leaving.record;
      const held = heldRecord?.revision ?? null;
      const heldProjection = heldRecord?.projection;
      if (result.reconciled) {
        const text=files.readText(file);
        if (heldRecord?.projection) {
          try {
            const note=await new Working(reconciling,windowWriter(sessionId))
              .write(heldRecord.blockId,text.slice(heldRecord.prefix.length),heldRecord.prefix,false,heldRecord.projection,'text');
            if (!publisher.recordWorking(file,note.source,digestOfBytes(text),heldRecord.projection.id,held,'text',seenAtRead)) throw new Error('Projection changed');
          } catch (error) {
            reportFailure(error);
            return {reconciled:false as const,because:'working-unavailable' as const,choices:[],storeText:document.text,previousText:null,fileText:text,held,heldProjection};
          }
        }
      }
      return {...result,held,heldProjection};
    });
    if (outcome.reconciled) {
      const notice = reconciledNotice(sidecar.blockId, path.basename(file));
      show(notice);
      paint();
      return notice;
    }
    if (outcome.because !== undefined) {
      const notice = reconcileUnfinishedNotice(file, outcome.because);
      show(notice);
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
          : "Take the store's version and reset the working baseline",
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
     * NOTE: THE ACTION IS CARRIED OUT AGAINST THE TEXT THAT WAS OFFERED, OR
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
    const done: {done:boolean;file:string;because?:string;early?:true} = await chain.run(directory, async () => {
      const refused=publisher.reconciliationGuard(file,outcome.fileText,outcome.held);
      if (refused) return {done:false,file,because:refused,early:true as const};
      let projection;
      if (outcome.heldProjection) {
        const text=picked.action==='take-store-version'?document.text:
          outcome.fileText.startsWith(document.prefix)?outcome.fileText:document.prefix+outcome.fileText;
        /*
         * TEXT, READ AT THE START: any other mode went no further, so the
         * write needs no read of its own and the record learns text with the
         * note.
         */
        try {
          projection=(await new Working(reconciling,windowWriter(sessionId))
            .write(sidecar.blockId,text.slice(document.prefix.length),document.prefix,picked.action==='take-store-version',outcome.heldProjection,'text')).source;
        } catch (error) {reportFailure(error);return {done:false,file,because:'working-unavailable'};}
      }
      return publisher.reconcileBy(file,picked.action,document.prefix,document.text,outcome.fileText,projection,outcome.held,'text',seenAtRead);
    });
    /*
     * THE RECORD MOVED WHILE THE PICK WAS OPEN (queue item 43): refused by the
     * second wait's first check, before anything was written. A refusal with
     * the same reason from `reconcileBy` (late) goes on to the unfinished
     * notice below, whose sentence says the note may have been written.
     */
    if (!done.done && done.because === 'record-moved' && done.early === true) {
      const notice = reconcileMovedNotice(file);
      show(notice);
      paint();
      return notice;
    }
    if (!done.done && done.because === 'file-changed') {
      const notice = reconcileStaleNotice(file);
      show(notice);
      paint();
      return notice;
    }
    if (!done.done) {
      const notice = reconcileUnfinishedNotice(file, done.because);
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

  async function migrateBlock(target?: string): Promise<void> {
    const file=target ?? vscode.window.activeTextEditor?.document.uri.fsPath;
    if (!file || !client) return;
    const sidecar=publisher.sidecarOf(file) ?? migrationIdentity(files,path.dirname(file));
    if (!sidecar || sidecar.storeId!==config.store) {
      vscode.window.showWarningMessage('Select a legacy block from the configured store before migrating.');return;
    }
    const directory=path.dirname(file),reading=client;
    const sourceSession=path.basename(path.dirname(path.dirname(directory)));
    const writer=windowWriter(sourceSession);
    const confirmed=await vscode.window.showWarningMessage('Verified legacy files will move to a one-time recovery archive. Unprotected drafts and pending sends will stay in place.',{modal:true},'Migrate');
    if (confirmed!=='Migrate') return;
    const result=await chain.run(directory,()=>migrateLegacy({files,publisher,directory,storeId:sidecar.storeId,blockId:sidecar.blockId,
      sourceIsSafe:()=>sessions.migrationSourceSafe(directory),sourceStillSafe:()=>sessions.migrationSourceSafeNow(directory),pending:()=>sessions.pendingForDirectory(directory),
      claimDestination:()=>sessions.claimMigrationDestination(directory,new Owners(files)),
      isDirty:p=>vscode.workspace.textDocuments.some(d=>d.uri.fsPath===p&&d.isDirty),
      sources:async()=>{
        const committed=await new Working(reading,`migration-${randomUUID()}`).read(sidecar.blockId,sidecar.prefix);
        const selected=await new Working(reading,writer).read(sidecar.blockId,sidecar.prefix);
        return {committed:{source:committed.source,prefix:committed.prefix,text:committed.prefix+committed.body},
          working:selected.source.kind==='working'?{source:selected.source,prefix:selected.prefix,text:selected.prefix+selected.body}:null};
      }}));
    if (!result.migrated) {
      vscode.window.showWarningMessage(`Migration is incomplete (${result.because}). Preserved: ${result.retained.join(', ')}`);return;
    }
    const opened=await vscode.workspace.openTextDocument(vscode.Uri.file(result.file));
    await vscode.window.showTextDocument(opened,{preview:false});
    vscode.window.showInformationMessage(`Current file: ${result.file}. The one-time recovery archive is ${result.archive}.`);
  }

  /*
   * X1c: WHAT A SAVE DOES, AND WHAT IT REFUSES.
   *
   * The record beside the file decides: it carries the prefix this save
   * is split against and whether the block's own body used CRLF. A file
   * with no record, or one whose publication never finished, or one
   * holding a third version, is refused BY NAME -- the one thing that
   * must not happen is a save that quietly does nothing, because that is
   * indistinguishable from one that worked. (section 12.19.4, section 12.17.3)
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
   * NOTE: IT RUNS THE IMPORT INSIDE THE SAVER'S LOCK, through `adopt`.
   * Handing out a bare queue put the import outside whatever serialises
   * that file, and a save answering in the middle of it wrote its own
   * copy back over the imported entries.
   */
  /*
   * NOTE: AND THE DECISION CARRIES THE GENERATION IT WAS MADE IN.
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
       * and this handler leaves it entirely alone. (section 12.13.4)
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
    /*
     * NOTE: THE SAVER IS TAKEN BEFORE THE WAIT AND USED AFTER IT. A
     * settings change replaces the Saver, the queue and the client; the
     * save this handler accepted belongs to the one that accepted it,
     * and the record it made names that queue's store. Reading `saver`
     * again after the wait would send this file's bytes through
     * whatever window the user has moved to.
     */
    const sending = saver;
    const writing = client as Client;

    /*
     * EVERYTHING THE SAVE IS ABOUT IS READ IN HERE, AT ONE INSTANT, on
     * the chain. `decide` says whether to send; `acceptSave` reads the
     * store from the file's own record, takes the next sequence number,
     * writes it down durably, and freezes the lot into a record. After
     * this callback returns, nothing on the save path reads the
     * settings, the document or the sidecar again. (section 13.1)
     */
    const accepted = await chain.run(path.dirname(file), async () => {
      // A reconciliation ahead of this save may have changed the split or origin.
      let currentSidecar = publisher.sidecarOf(file);
      let decision = saving.decide(
        { file, isDirty: saved.isDirty, getText: () => saved.getText() },
        currentSidecar
      );
      if (!decision.send) {
        return { decision };
      }
      /*
       * THE STORE CHECK OF W PREPARATION, read once for both routes below:
       * the working write and the save through `set` of a record with no
       * projection.
       */
      const inConfiguredStore = currentSidecar?.storeId === config.store;
      /*
       * THE NOT-TEXT READS OF THIS BLOCK SEEN BEFORE THE GATE'S OWN READ: text
       * learned by it is recorded only if none came since.
       */
      const seenAtSave = publisher.invalidationsOf(path.dirname(file));
      if (currentSidecar?.projection && inConfiguredStore) {
        const capturedSidecar = currentSidecar;
        const source = currentSidecar.projection;
        try {
          const working = await new Working(writing,windowWriter(sessionId))
            .write(capturedSidecar.blockId,decision.src,capturedSidecar.prefix,false,source,capturedSidecar.mode ?? null);
          if (!publisher.recordWorking(file,working.source,decision.rawDigest,source.id,capturedSidecar.revision,working.mode,seenAtSave)) {
            throw new Error('The file or its projection changed while the working note was being saved');
          }
          currentSidecar = publisher.sidecarOf(file);
          decision = {...decision,intent:{verb:'commit',field:'src',expectation:JSON.stringify({writer:working.source.writer,version:working.source.version})}};
        } catch (error) {
          /*
           * A BLOCK THE GATE FOUND NOT TEXT IS NOT SAVED, AND EVERY RECORD OF
           * IT THIS SESSION HOLDS LEARNS IT: datum, refused next time without
           * asking the store; no mode, asked again.
           */
          if (error instanceof DatumBlockRefused) {
            publisher.recordModeOfBlock(path.dirname(file),'datum');
            return {decision:{send:false as const,refusal:{because:'datum-block' as const}}};
          }
          if (error instanceof ModeNotKnown) publisher.recordModeOfBlock(path.dirname(file),null);
          return {decision:{send:false as const,refusal:{because:'working-unavailable' as const,detail:String(error)}}};
        }
      } else if (currentSidecar && !currentSidecar.projection && inConfiguredStore) {
        /*
         * A RECORD WITH NO PROJECTION -- a file a much earlier version made --
         * saves through `set <id> src`, not a working write, and goes through
         * the same mode gate first.
         */
        const unprojected = currentSidecar;
        try {
          const read = await requireText(writing,unprojected.blockId,unprojected.mode ?? null);
          /*
           * NEVER: A RECORD THAT MOVED WHILE THE MODE WAS READ. The decision
           * was split against this record; a replacement is not adopted under
           * it. Only the record the reading was taken against learns text, and
           * only when this save read it.
           */
          if (read) {
            if (!publisher.recordText(file,unprojected.revision,seenAtSave)) {
              throw new Error('the file\'s record changed while the block\'s mode was being read; save again');
            }
            currentSidecar = publisher.sidecarOf(file);
          }
        } catch (error) {
          if (error instanceof DatumBlockRefused) {
            publisher.recordModeOfBlock(path.dirname(file),'datum');
            return {decision:{send:false as const,refusal:{because:'datum-block' as const}}};
          }
          if (error instanceof ModeNotKnown) publisher.recordModeOfBlock(path.dirname(file),null);
          return {decision:{send:false as const,refusal:{because:'working-unavailable' as const,detail:String(error)}}};
        }
      }
      return {
        decision,
        acceptance: acceptSave({
          file,
          sidecar: currentSidecar as NonNullable<typeof currentSidecar>,
          decision,
          /*
           * NOTE: THE ONE READING OF THE LIVE SETTINGS AFTER THE FIRST
           * WAIT, and it is here rather than after the callback because
           * here it cannot change between the check and the record.
           * `awaiting.test.ts` counts these: one more, one fewer, or
           * this one moved out of the callback all fail.
           */
          queueStore: config.store,
          storeHash,
          newRequestId: () => randomUUID(),
          numbering: publisher
        })
      };
    });

    if (!accepted.decision.send) {
      show(refusalNotice(sidecar.blockId, file, accepted.decision.refusal));
      paint();
      return;
    }
    const acceptance = accepted.acceptance as Acceptance;
    if (!acceptance.accepted) {
      /*
       * NOT ACCEPTED, AND EACH REASON IS ITS OWN SENTENCE. Nothing was
       * queued and nothing was sent, so the user's bytes are still only
       * in their file -- which is what the block will go on reporting.
       */
      show(
        acceptance.because === 'another-store'
          ? wrongStoreNotice(sidecar.blockId, acceptance.store, acceptance.configured)
          : unrecordedNotice(file, 'not-acknowledged')
      );
      paint();
      return;
    }

    let outcome;
    try {
      outcome = await sending.submit(acceptance.record);
    } catch (e) {
      hover.drop();
      reportFailure(e);
      /*
       * NOTE: A SAVE THAT REJECTED STILL SAYS WHAT ITS QUEUE WRITES COULD NOT
       * PROMISE: the Saver handed those to the sink before rejecting (K11).
       */
      showDurability();
      paint();
      return;
    }
    /*
     * THIS WINDOW'S SAVE OR COMMIT: what the hover holds may be old now.
     */
    hover.drop();
    /*
     * NOTE: A NUMBER THAT WAS SPENT ON A SEND THAT NEVER LEFT IS GIVEN
     * BACK. (section 13.1, I7)
     *
     * The sequence is taken and written down before anything is queued,
     * so that no send can carry a number nobody recorded. When the
     * queue write then fails, nothing was sent -- and leaving the
     * number in `outstanding` would leave the record saying a send is
     * out with no entry anywhere that could answer for it: a draft the
     * block can never stop being.
     *
     * NOTE: NO CELL REACHES THIS LINE. The Saver's half is measured --
     * `says it was not queued, so the number can be given back` in
     * `sending.test.ts` -- and this half lives in the save handler,
     * which only the editor-hosted suite drives and which would need
     * the queue made unwritable mid-run. It is named here and in the
     * delivery note rather than left looking like a site that is
     * covered.
     */
    if (outcome.notQueued === true) {
      saving.releaseSend(acceptance.record.file, acceptance.record.seq);
    }
    /*
     * THE RECORD AND THE REMOVAL BOTH HAPPENED INSIDE THE SETTLER, in
     * that order. Nothing is written here: a second place that wrote the
     * acknowledgement would be a second place for the order to be wrong.
     */
    show(saveNotice(outcome, accepted.decision.normalised));
    /*
     * NOTE: AND WHAT LANDED WHILE THIS SAVE WAS BEING PREPARED IS SAID
     * AFTER IT, AS ITS OWN SENTENCE.
     *
     * The store tells us, on a commit that SUCCEEDED, which other
     * writers reached it after this save's draft took its baseline. It
     * asks nothing of the user -- their save worked -- so it is an
     * information notice and it comes after the one about the save,
     * never instead of it. On a save whose baseline was fresh there is
     * no clause and nothing is shown.
     */
    if (outcome.behind !== undefined) {
      show({ level: 'information', text: outcome.behind });
    }
    /*
     * AND A SAVE TAKEN BY A STORE THAT COULD NOT READ EVERY WRITER SAYS SO,
     * after the save's own notice: the save went through, so this asks
     * nothing of the user and is information.
     *
     * NEVER: A SAVE DOES NOT SET THE STATUS BAR'S MARKER. This report is made
     * after a wait with no check of which store the window is on, so a
     * marker set here could describe a store the window has left; the
     * readings that set it each check first.
     */
    const notes = outcome.notes ?? null;
    if (notes !== null) {
      show({ level: 'information', text: incompleteWarning(notes) });
    }
    /*
     * NOTE: AND WHAT THE QUEUE COULD NOT PROMISE, ON THE SAME ROAD. (queue
     * item 3) The save stands -- its entry was written -- but the
     * directory could not be flushed, so the entry may not survive a power
     * cut. That asks nothing of the save, so it follows the save's notice
     * as `behind` does; it is a warning, because it is about the user's
     * work being at risk rather than about somebody else's.
     */
    if (outcome.durability !== undefined) {
      show({ level: 'warning', text: outcome.durability });
    }
    /*
     * NOTE: AND THEN WHAT THE REST OF THIS DRAIN'S WRITES COULD NOT PROMISE
     * (other saves' entries, the settle): the sink's, after this save's own.
     */
    showDurability();
    paint();
  }

  context.subscriptions.push(
    status,
    vscode.window.registerTreeDataProvider('theourgiaOutline', provider),
    command(REFRESH_OUTLINE.id, async () => {
      provider.refresh();
      await refreshConflicts();
    }),
    command(OPEN_BLOCK.id, openBlock),
    command(GO_TO_DEFINITION.id, goToDefinition),
    vscode.languages.registerDefinitionProvider(
      [
        { scheme: 'file', pattern: new vscode.RelativePattern(storage, '**') },
        { scheme: DOCUMENT_SCHEME }
      ],
      definitions
    ),
    vscode.languages.registerHoverProvider(
      [
        { scheme: 'file', pattern: new vscode.RelativePattern(storage, '**') },
        { scheme: DOCUMENT_SCHEME }
      ],
      hovers
    ),
    command(HOVER_MORE.id, hoverMore),
    vscode.workspace.registerTextDocumentContentProvider(DOCUMENT_SCHEME, views),
    vscode.workspace.onDidCloseTextDocument((closed) => {
      if (closed.uri.scheme === DOCUMENT_SCHEME) {
        views.forget(closed.uri);
      }
    }),
    command(OPEN_AS_DOCUMENT.id, openAsDocument),
    command(SUGGEST_SPLIT.id, suggestSplit),
    command(SUPPLY_SIGNATURES.id, () => supplyFacts('signatures')),
    command(SUPPLY_CALLS.id, () => supplyFacts('calls')),
    command(SUPPLY_DIAGNOSTICS.id, () => supplyFacts('diagnostics')),
    command(SHOW_FILES.id, () => showMode('files')),
    command(SHOW_OUTLINE.id, () => showMode('outline')),
    command(NEW_FILE_HERE.id, newFileHere),
    command(MOVE_TO_DIRECTORY.id, (node?: ListedNode) => changePath(node, 'move')),
    command(RENAME_FILE.id, (node?: ListedNode) => changePath(node, 'rename')),
    command(RECONCILE_BLOCK.id, reconcileBlock),
    /*
     * NOTE: THE HANDLER IS ONE LINE ON PURPOSE. Everything this command
     * decides -- which windows to list, what may be done to one, what
     * the user is told it costs -- is in src/recovery.ts, where a cell
     * can drive it. A handler that made any of those decisions here
     * would be making them where nothing can look.
     */
    command(OTHER_SESSIONS.id, () =>
      chooseAndRecover(sessions, editorChooser, adoptingInto())
    ),
    /*
     * WHAT A SEARCH DOES IS IN `src/search.ts`, where a cell can drive
     * it. This hands it the editor and the model and returns what it
     * decided, for the same reason `showStatus` returns its facts: a
     * decision that is only shown is a decision nothing can read.
     *
     * NOTE: THE MODEL IS READ AT THE MOMENT THE COMMAND RUNS. It is
     * replaced whenever the settings change, and a search is several
     * awaits long; capturing it here means the hits and the block that
     * is opened come from one store.
     */
    command(SEARCH_BLOCKS.id, () => {
      /*
       * NEVER: THE HIT BELONGS TO THE STORE IT WAS FOUND IN.
       *
       * A search is several awaits long -- a box the user types into, a
       * list they choose from -- and a settings change replaces the model
       * and bumps the generation in the middle of it. The open used to
       * hand the id to `openBlock` with no generation at all, and
       * `openBlock` takes one precisely so that it can refuse an id from
       * a store the user has left. Measured in a review round: with the
       * box held open and the store changed from A to B, an id found in
       * A was read against B. The store is taken when the command starts,
       * which is when the search is about the store it is about.
       */
      const foundIn = storeIdentity();
      const searching = model;
      return runSearch(searching === null ? null : {
        search: (query: string) => searching.search(query),
        searchReading: async (query: string) => {
          const searched = generation;
          const reading = await searching.searchReading(query);
          if (searched !== generation) {
            return reading;
          }
          noteReading(reading.notes);
          return reading;
        }
      }, {
        ask: (prompt: string) =>
          Promise.resolve(vscode.window.showInputBox({ prompt, placeHolder: 'stale baseline' })),
        pick: async (hits: Hit[], placeHolder: string, title?: string) => {
          const picked = await vscode.window.showQuickPick(
            hits.map((hit) => ({
              label: hit.id,
              description: `score ${hit.score}`,
              detail: hit.note,
              value: hit
            })),
            title === undefined ? { placeHolder, matchOnDetail: true } : { placeHolder, title, matchOnDetail: true }
          );
          return picked?.value;
        },
        say: (text: string, level: 'information' | 'error') => show({ level, text }),
        open: async (id: string) => {
          await vscode.commands.executeCommand(OPEN_BLOCK.id, id, foundIn);
        }
      });
    }),
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
    command(RETRY_OUTBOX.id, async () => {
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
        outcomes = await active.retryParked();
      } catch (e) {
        failure = e;
      }
      hover.drop();
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
    command(SHOW_STATUS.id, async (options?: { ask?: boolean }) => {
      if (options?.ask !== false) {
        await refreshConflicts();
      }
      vscode.window.showInformationMessage(status.tooltip as string);
      return facts();
    }),
    command(MIGRATE_BLOCK.id,migrateBlock),
    vscode.workspace.onDidSaveTextDocument(onSaved),
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration('theourgia')) {
        hover.drop();
        /*
         * NOTE: A SETTINGS CHANGE IS THE ONE EVENT THAT CAN ANSWER A PARKED
         * ENTRY, so it is the one that releases them.
         *
         * Some entries are parked because nothing they can wait for will
         * change the answer: the store directory does not exist, the
         * socket path is longer than the operating system allows. The
         * user's way out of those is to edit a setting -- and editing it
         * IS this event, so there is no recovery command to teach. If
         * the change did not help, the drain below parks them again on
         * the same attempt.
         *
         * It is done before `rebuild` because rebuild replaces the queue
         * object; releasing on the old one and draining with the new one
         * would be two objects over one file, which this tree has paid
         * for before.
         */
        if (outbox !== null) {
          try {
            const released = outbox.unparkAll();
            if (released.durability !== null) {
              warnings.add(outbox.path, released.durability);
            }
          } catch (error) {
            reportFailure(error);
          }
        }
        /*
         * NOTE: SHOWN BEFORE THE REBUILD AND AFTER IT, the second even if the
         * rebuild throws. (design v4, K4; delivery review r1 of item 22, L3)
         */
        showDurability();
        try {
          rebuild();
        } finally {
          showDurability();
        }
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
