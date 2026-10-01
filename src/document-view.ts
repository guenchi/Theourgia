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
 * A SUBTREE, SHOWN AS ONE DOCUMENT. (queue item 6; ruled by the main
 * session 2026-09-19 15:45 and 15:50, details 2026-09-25)
 *
 * KEY: THIS IS A VIEW, NOT A PROJECTION. It is composed from
 * `read <id> --recursive --wire` every time it is opened, it is shown as a
 * virtual document of its own scheme -- never a file on disk -- and it is
 * never written back. That the editor will not let it be edited is a
 * property of the document type, not of a message asking the user not to.
 * NOTE: IT CAN STILL BE COPIED. The editor offers Save As for such a
 * document (review r1 of item 6), which writes its text to a file the user
 * names; nothing here sends it anywhere. A block is edited through its own
 * projection, from the outline.
 *
 * NOTHING HERE IMPORTS VS CODE, so that what the document says is decided
 * where a cell can ask.
 *
 * WHY THE LEVELS ARE RECOMPUTED. `read --recursive --md` is flat for a tree
 * built with `insert`: every block prints as `# <title>` (measured on the
 * F46 pin), and where one block ends cannot be recovered from the text.
 * So each block's heading level comes from its depth under the opened
 * block, taken from the positions in the answer; the headings the block's
 * own body carries are moved down by the same depth, so they stay below
 * the block they belong to; and one line in front of each block marks
 * where it starts.
 *
 * THE MARKER IS WEAKENED, NOT HIDDEN. It is an HTML comment: a rendered
 * preview does not show it, the editor -- which shows the raw text -- does.
 * It carries the depth, because past six levels the heading cannot.
 */

import { Client, Note } from './client';
import { Block, TextNotUtf8, readBlock, textOf } from './blocks';
import { TransportError } from './transport';
import { Datum } from './wire';

export const DOCUMENT_SCHEME = 'theourgia-document';

const DEEPEST_HEADING = 6;

export function markerFor(id: string, depth: number): string {
  return `<!-- theourgia block ${id} depth ${depth} -->`;
}

/*
 * A FENCE, as CommonMark reads one: up to three spaces, then three or more
 * backticks or tildes. A closing fence is the same character, at least as
 * long, with nothing after it but spaces.
 */
interface Fence {
  mark: string;
  length: number;
}

function fenceOpening(line: string): Fence | null {
  const found = /^ {0,3}(`{3,}|~{3,})(.*)$/.exec(line);
  if (found === null) {
    return null;
  }
  const run = found[1];
  /*
   * A backtick fence's info string may not hold a backtick; a line that
   * does is inline code, not a fence.
   */
  if (run[0] === '`' && found[2].includes('`')) {
    return null;
  }
  return { mark: run[0], length: run.length };
}

function closesFence(line: string, open: Fence): boolean {
  const found = /^ {0,3}(`{3,}|~{3,})[ \t]*$/.exec(line);
  return found !== null && found[1][0] === open.mark && found[1].length >= open.length;
}

export interface Shifted {
  text: string;
  /*
   * THE FENCE THE BODY LEFT OPEN, if it did. Everything after it would be
   * read as code -- the next block's marker and heading included -- so the
   * caller closes it.
   */
  openFence: Fence | null;
}

/*
 * THE BODY'S OWN HEADINGS, MOVED DOWN BY `by` LEVELS AND KEPT AT SIX OR
 * ABOVE. Only ATX headings (`#` to `######` after at most three spaces,
 * followed by a space, a tab or the end of the line); lines inside a fenced
 * code block are left as they are -- a `# comment` in a shell example is
 * not a heading. NOTE: SETEXT HEADINGS (a line underlined with `===` or
 * `---`) ARE NOT MOVED; they are left as written, which is a known gap.
 */
export function shiftHeadings(src: string, by: number): Shifted {
  const lines = src.split('\n');
  let open: Fence | null = null;
  const out = lines.map((line) => {
    const bare = line.endsWith('\r') ? line.slice(0, -1) : line;
    if (open !== null) {
      if (closesFence(bare, open)) {
        open = null;
      }
      return line;
    }
    const fence = fenceOpening(bare);
    if (fence !== null) {
      open = fence;
      return line;
    }
    const heading = /^( {0,3})(#{1,6})(?=[ \t\r]|$)/.exec(line);
    if (heading === null || by === 0) {
      return line;
    }
    const level = Math.min(heading[2].length + by, DEEPEST_HEADING);
    return heading[1] + '#'.repeat(level) + line.slice(heading[0].length);
  });
  return { text: out.join('\n'), openFence: open };
}

export type Composed =
  | { ok: true; title: string; text: string; notes?: Note[] | null }
  | { ok: false; reason: string; ids: string[] };

/*
 * A FIELD THAT IS THERE BUT IS NOT TEXT IS NOT AN EMPTY FIELD. A field
 * with two candidate values arrives as a conflict, not a string, and
 * reading it as '' would show a body with nothing in it -- the one reading
 * nobody would question.
 *
 * NOTE: THE DECODING IS THE SHARED ONE (`textOf`, blocks.ts), so bytes --
 * a text-mode code block's source -- are text here exactly as they are in
 * the block editor. Only the rule for a field that is absent, or there and
 * not text, is this view's.
 */
function fieldText(block: Block, name: string): string | null {
  if (!block.fields.has(name)) {
    return '';
  }
  const value = textOf(block, name);
  return typeof value === 'string' ? value : null;
}

function refused(reason: string, ids: string[]): Composed {
  return { ok: false, reason, ids };
}

/*
 * THE DOCUMENT FOR THE SUBTREE UNDER `rootId`, from the records of one
 * `read <rootId> --recursive --wire`, in the order they arrived -- the
 * core's document order; sorting here would be a second opinion about it.
 *
 * NEVER: HALF A DOCUMENT. Anything this cannot place refuses the whole
 * view and names the blocks: a record that is not a block, the opened
 * block missing, a block twice, a deleted block, a block whose place is
 * unsettled or whose parent is not in the answer, a title, body or front
 * matter that is not text. A document that silently lacked one of them
 * would read as complete.
 */
export function composeDocument(rootId: string, records: Datum[]): Composed {
  /*
   * A FIELD WHOSE BYTES ARE NOT UTF-8 REFUSES THE DOCUMENT BY NAME: which
   * field, which block, and where the bytes stop decoding. A document
   * missing a block's text would read as that block being empty.
   */
  try {
    return composeFrom(rootId, records);
  } catch (e) {
    if (e instanceof TextNotUtf8) {
      return refused(`the ${e.field} of a block is not UTF-8 text (text-not-utf8, byte ${e.offset})`, [e.id]);
    }
    throw e;
  }
}

function composeFrom(rootId: string, records: Datum[]): Composed {
  const blocks: Block[] = [];
  for (const record of records) {
    const block = readBlock(record);
    if (block === null) {
      return refused('the store answered with something that is not a block', []);
    }
    blocks.push(block);
  }
  const byId = new Map<string, Block>();
  const twice: string[] = [];
  for (const block of blocks) {
    if (byId.has(block.id)) {
      twice.push(block.id);
    }
    byId.set(block.id, block);
  }
  if (twice.length > 0) {
    return refused('the store answered with the same block more than once', twice);
  }
  const root = byId.get(rootId);
  if (root === undefined) {
    return refused(`the store's answer does not include ${rootId}, the block that was opened`, [rootId]);
  }
  const deleted = blocks.filter((b) => b.deleted).map((b) => b.id);
  if (deleted.length > 0) {
    return refused('the answer holds deleted blocks', deleted);
  }
  const depths = new Map<string, number>([[rootId, 0]]);
  const unplaced: string[] = [];
  /*
   * THE PARENT OF THE OPENED BLOCK IS NOT ASKED: it is wherever the block
   * sits, outside this view. Every other block must hang, through parents
   * in this answer, from the opened one.
   */
  const depthOf = (block: Block, seen: Set<string>): number | null => {
    const known = depths.get(block.id);
    if (known !== undefined) {
      return known;
    }
    if (block.placement.kind !== 'under' || seen.has(block.id)) {
      return null;
    }
    const parent = byId.get(block.placement.parent);
    if (parent === undefined) {
      return null;
    }
    seen.add(block.id);
    const above = depthOf(parent, seen);
    if (above === null) {
      return null;
    }
    depths.set(block.id, above + 1);
    return above + 1;
  };
  for (const block of blocks) {
    if (depthOf(block, new Set()) === null) {
      unplaced.push(block.id);
    }
  }
  if (unplaced.length > 0) {
    return refused(`these blocks cannot be placed under ${rootId}`, unplaced);
  }
  const notText = blocks
    .filter((b) => ['title', 'path', 'src', 'front'].some((name) => fieldText(b, name) === null))
    .map((b) => b.id);
  if (notText.length > 0) {
    return refused('these blocks have a title, body or front matter that is not text', notText);
  }
  const headingTitle = (block: Block): string => {
    const title = fieldText(block, 'title') as string;
    /*
     * A LONE CARRIAGE RETURN ENDS A LINE TOO (review r1 of item 6): a title
     * holding one would otherwise start a second, unmoved heading of its own.
     */
    return (title.length > 0 ? title : (fieldText(block, 'path') as string)).replace(/\r\n|\r|\n/g, ' ');
  };
  const parts: string[] = [];
  /*
   * FRONT MATTER GOES FIRST OR NOWHERE: it means something only at the
   * top of a file. Only the opened block can carry it -- the core's walk
   * stops at a document nested inside a section.
   */
  const front = fieldText(root, 'front') as string;
  if (front.length > 0) {
    parts.push(front.endsWith('\n') ? front : `${front}\n`);
  }
  for (const block of blocks) {
    const depth = depths.get(block.id) as number;
    parts.push(`${markerFor(block.id, depth)}\n`);
    const title = headingTitle(block);
    if (title.length > 0) {
      parts.push(`${'#'.repeat(Math.min(depth + 1, DEEPEST_HEADING))} ${title}\n`);
    }
    const body = shiftHeadings(fieldText(block, 'src') as string, depth);
    let text = body.text;
    if (text.length > 0 && !text.endsWith('\n')) {
      text += '\n';
    }
    if (body.openFence !== null) {
      text += `${body.openFence.mark.repeat(body.openFence.length)}\n`;
    }
    parts.push(text);
  }
  return { ok: true, title: headingTitle(root), text: parts.join('') };
}

/*
 * WHAT A REFUSAL SAYS: the reason and every block it is about.
 */
export function refusalOf(composed: { reason: string; ids: string[] }): string {
  const named = composed.ids.length > 0 ? `: ${composed.ids.join(', ')}` : '';
  return `the document was not opened, because ${composed.reason}${named}.`;
}

/*
 * ASK THE STORE AND COMPOSE. A refused read is not an empty subtree: it is
 * raised, as every other read in this extension raises one.
 */
export async function documentOf(client: Client, id: string): Promise<Composed> {
  const answer = await client.request('read', [id, '--recursive', '--wire']);
  if (!answer.ok) {
    throw new TransportError(
      'unreadable',
      `the store would not read the subtree under ${id}: ${answer.text.trim()}`,
      answer.text
    );
  }
  /*
   * THE WRITERS THE SUBTREE COULD NOT SEE GO WITH THE DOCUMENT, so the view
   * that shows it can say so above the text.
   */
  const composed = composeDocument(id, answer.answers);
  return composed.ok ? { ...composed, notes: answer.notes ?? null } : composed;
}

/*
 * THE TEXT THE EDITOR IS GIVEN FOR A DOCUMENT'S ADDRESS. What a command
 * composed is held under the address until the tab is closed, so showing
 * it does not ask the store twice. An address nothing composed -- a tab
 * the editor restored after a reload -- is composed here, from the store
 * the address names and from no other: NEVER a read of the configured
 * store under an id that was taken from another one.
 */
export type Composer = (client: Client, id: string) => Promise<Composed>;

export class DocumentTexts {
  private readonly held = new Map<string, string>();

  /*
   * NOTE: TWO VIEWS SHARE THE SCHEME: a subtree composed as one document,
   * and a datum block shown as its library's datum export
   * (src/datum-view.ts). The address says which, so a restored tab is
   * composed as the view it was, not as a subtree under the same id.
   */
  constructor(
    private readonly current: () => { client: Client; store: string } | null,
    private readonly compose: Composer = documentOf,
    private readonly composeDatum: Composer | null = null
  ) {}

  public hold(address: string, text: string): void {
    this.held.set(address, text);
  }

  public forget(address: string): void {
    this.held.delete(address);
  }

  public async textFor(address: string, query: string): Promise<string> {
    const held = this.held.get(address);
    if (held !== undefined) {
      return held;
    }
    const asked = readDocumentQuery(query);
    if (asked === null) {
      throw new Error(`Theourgia: ${address} does not name a store and a block`);
    }
    const now = this.current();
    if (now === null) {
      throw new Error('Theourgia: set theourgia.corePath and theourgia.store first.');
    }
    if (now.store !== asked.store) {
      throw new Error(
        `Theourgia: this document was composed from the store ${asked.store}, and the store ` +
          `configured now is ${now.store}; it is not read from another store under the same id.`
      );
    }
    /*
     * NOTE: NO GENERATION CHECK AFTER THIS WAIT, and none is needed: the
     * store was compared BEFORE it and the read goes through that store's
     * client, so what comes back is the subtree of the store the address
     * names -- which is what the document says it is -- and nothing is
     * written anywhere.
     */
    const composer = asked.view === 'datum' ? this.composeDatum : this.compose;
    if (composer === null) {
      throw new Error(`Theourgia: ${address} names a datum view, and nothing here composes one`);
    }
    const composed = await composer(now.client, asked.id);
    if (!composed.ok) {
      throw new Error(`Theourgia: ${refusalOf(composed)}`);
    }
    this.held.set(address, composed.text);
    return composed.text;
  }
}

/*
 * WHICH STORE AND WHICH BLOCK A DOCUMENT IS OF, carried in its address.
 * The address outlives the settings -- a tab is restored after a reload --
 * so a document names its store, and one whose store is no longer the
 * configured one is not read from the other store under the same id.
 */
export type DocumentKind = 'subtree' | 'datum';

/*
 * NOTE: A SUBTREE'S ADDRESS CARRIES NO `view`, as before the datum view
 * existed, so a tab restored from an earlier version still reads as one.
 */
export function documentQuery(store: string, id: string, view: DocumentKind = 'subtree'): string {
  return new URLSearchParams(view === 'subtree' ? { store, id } : { store, id, view }).toString();
}

export function readDocumentQuery(query: string): { store: string; id: string; view: DocumentKind } | null {
  const params = new URLSearchParams(query);
  const store = params.get('store');
  const id = params.get('id');
  const view = params.get('view');
  if (store === null || id === null || store.length === 0 || id.length === 0) {
    return null;
  }
  if (view !== null && view !== 'datum') {
    return null;
  }
  return { store, id, view: view === null ? 'subtree' : 'datum' };
}
