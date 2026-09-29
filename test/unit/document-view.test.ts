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
 * plugin-r3 item 6: a subtree opened as one read-only document, its levels
 * recomputed from the positions in `read --recursive --wire`.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import { Client } from '../../src/client';
import {
  Composed,
  DocumentTexts,
  composeDocument,
  documentOf,
  documentQuery,
  markerFor,
  readDocumentQuery,
  refusalOf,
  shiftHeadings
} from '../../src/document-view';
import { initWire, parseAnswers } from '../../src/wire';
import { Counting } from '../support/counting';
import { CorePin, RealStore, checkCorePin, pinCore } from '../support/real-core';

/*
 * A RECORD IN THE SHAPE THE CORE WRITES (measured on the F46 pin):
 * `((id . "a.2") (deleted . #f) (fields ...) (position "a.1" . 0) (edges))`.
 */
function record(
  id: string,
  position: string,
  fields: Record<string, string>,
  deleted = false
): string {
  const listed = Object.entries(fields)
    .map(([name, value]) => `(${name} . ${JSON.stringify(value)})`)
    .join(' ');
  return `((id . ${JSON.stringify(id)}) (deleted . ${deleted ? '#t' : '#f'}) (fields (kind . section) ${listed}) (position ${position}) (edges))`;
}

function compose(rootId: string, ...records: string[]) {
  return composeDocument(rootId, parseAnswers(records.join('\n')));
}

function composed(rootId: string, ...records: string[]): string {
  const answer = compose(rootId, ...records);
  assert.ok(answer.ok, JSON.stringify(answer));
  return answer.ok ? answer.text : '';
}

describe('plugin-r3 6 a subtree as one read-only document', () => {
  before(async () => {
    await initWire();
  });

  it('gives each block the level of its depth under the opened one, with its marker in front', () => {
    const text = composed(
      'a.1',
      record('a.1', 'root . 0', { title: 'Top', src: 'top body\n' }),
      record('a.2', '"a.1" . 0', { title: 'Kid', src: 'kid body\n' }),
      record('a.3', '"a.2" . 0', { title: 'Grand' }),
      record('a.4', '"a.1" . 1', { title: 'Kid2', src: 'second\n' })
    );
    assert.strictEqual(
      text,
      [
        markerFor('a.1', 0),
        '# Top',
        'top body',
        markerFor('a.2', 1),
        '## Kid',
        'kid body',
        markerFor('a.3', 2),
        '### Grand',
        markerFor('a.4', 1),
        '## Kid2',
        'second',
        ''
      ].join('\n')
    );
    assert.strictEqual(markerFor('a.3', 2), '<!-- theourgia block a.3 depth 2 -->');
  });

  /*
   * KEY: THE OPENED BLOCK IS LEVEL ONE WHEREVER IT SITS. A block opened from
   * deep in the outline is the top of its own document; its parent is not
   * in the answer and is not asked for.
   */
  it('starts at level one for a block that is itself under another', () => {
    const text = composed(
      'a.5',
      record('a.5', '"a.2" . 3', { title: 'Deep' }),
      record('a.6', '"a.5" . 0', { title: 'Under it' })
    );
    assert.ok(text.includes('\n# Deep\n'), text);
    assert.ok(text.includes('\n## Under it\n'), text);
  });

  it('moves the headings in a body down by the depth of its block, and no others', () => {
    const body = [
      '## own heading',
      '#tag is not a heading',
      '   ### three spaces is',
      '    # four spaces is code',
      '```sh',
      '# a comment in a fence',
      '```',
      '~~~',
      '## inside tildes',
      '~~~',
      '#',
      ''
    ].join('\n');
    const shifted = shiftHeadings(body, 2);
    assert.strictEqual(
      shifted.text,
      [
        '#### own heading',
        '#tag is not a heading',
        '   ##### three spaces is',
        '    # four spaces is code',
        '```sh',
        '# a comment in a fence',
        '```',
        '~~~',
        '## inside tildes',
        '~~~',
        '###',
        ''
      ].join('\n')
    );
    assert.strictEqual(shifted.openFence, null);
    assert.strictEqual(shiftHeadings(body, 0).text, body, 'a depth of nothing changed the body');
  });

  /*
   * queue item 28, C28a: THE COMPOSITION SHIFTS A BODY'S HEADINGS BY ITS
   * BLOCK'S DEPTH, below the clamp. The cell above calls `shiftHeadings`
   * directly, and the composition cells look at depths five and seven, where
   * the clamp at six hides a shift of twice the depth (item 6's review, M32).
   */
  it('moves the headings of a body in a depth-one and a depth-two block by their depths when composing', () => {
    const text = composed(
      'a.1',
      record('a.1', 'root . 0', { title: 'Top', src: 'top body\n' }),
      record('a.2', '"a.1" . 0', { title: 'Kid', src: '## sub\n' }),
      record('a.3', '"a.2" . 0', { title: 'Grand', src: '## deeper\n' })
    );
    assert.ok(text.includes('\n### sub\n'), `a depth-one body heading is not one level down: ${text}`);
    assert.ok(text.includes('\n#### deeper\n'), `a depth-two body heading is not two levels down: ${text}`);
  });

  /*
   * A LINE ENDING IN A CARRIAGE RETURN is still the line it was: a closing
   * fence is still closing, and a bare `#` is still a heading.
   */
  it('reads a body written with CRLF the way it reads one written with LF', () => {
    const body = '```\r\n# code\r\n```\r\n## after\r\n#\r\n';
    assert.strictEqual(shiftHeadings(body, 1).text, '```\r\n# code\r\n```\r\n### after\r\n##\r\n');
  });

  it('does not end a fence at a shorter run, or at the other character', () => {
    /*
     * `~~~~` is as long as the opening run and only its character differs;
     * `~~~` would fail on its length alone and could not tell the two rules
     * apart (mutation M15 of item 6's first table).
     */
    const body = ['````', '```', '~~~~', '# still code', '````', '# after', ''].join('\n');
    assert.strictEqual(
      shiftHeadings(body, 1).text,
      ['````', '```', '~~~~', '# still code', '````', '## after', ''].join('\n')
    );
  });

  /*
   * KEY: SIX IS THE DEEPEST. Past it the heading stays at `######` and the
   * marker carries the depth.
   */
  it('keeps a heading at six however deep its block is, and says the depth in the marker', () => {
    const records = [record('b.0', 'root . 0', { title: 'L0' })];
    for (let depth = 1; depth <= 7; depth += 1) {
      records.push(record(`b.${depth}`, `"b.${depth - 1}" . 0`, { title: `L${depth}`, src: '## body\n' }));
    }
    const text = composed('b.0', ...records);
    assert.ok(text.includes(`${markerFor('b.5', 5)}\n###### L5\n###### body\n`), text);
    assert.ok(text.includes(`${markerFor('b.7', 7)}\n###### L7\n###### body\n`), text);
    assert.ok(!/#######/.test(text), 'a heading went past six');
  });

  it('closes a fence a body left open, so the next block is not read as code', () => {
    const text = composed(
      'c.1',
      record('c.1', 'root . 0', { title: 'Open', src: 'text\n~~~~\ncode\n' }),
      record('c.2', '"c.1" . 0', { title: 'Next' })
    );
    assert.strictEqual(
      text,
      [markerFor('c.1', 0), '# Open', 'text', '~~~~', 'code', '~~~~', markerFor('c.2', 1), '## Next', ''].join('\n')
    );
  });

  it('ends every block on a line of its own', () => {
    const text = composed(
      'c.1',
      record('c.1', 'root . 0', { title: 'A', src: 'no newline' }),
      record('c.2', '"c.1" . 0', { title: 'B' })
    );
    assert.ok(text.includes(`no newline\n${markerFor('c.2', 1)}\n`), text);
  });

  it('puts the opened block\'s front matter first, names an untitled document by its path, and keeps a title on one line', () => {
    const text = composed(
      'd.1',
      record('d.1', 'root . 0', { front: '---\nk: v\n---\n', path: 'doc.md' }),
      record('d.2', '"d.1" . 0', { title: 'two\nlines' })
    );
    assert.ok(text.startsWith(`---\nk: v\n---\n${markerFor('d.1', 0)}\n# doc.md\n`), text);
    assert.ok(text.includes('\n## two lines\n'), text);
    /*
     * A LONE CARRIAGE RETURN IS A LINE ENDING (review r1 of item 6): kept,
     * it would start a heading of its own at the wrong level.
     */
    assert.ok(
      composed('d.1', record('d.1', 'root . 0', { title: 'Child\r# injected' })).includes('\n# Child # injected\n'),
      'a title with a lone carriage return was not kept on one line'
    );
    const answer = compose('d.1', record('d.1', 'root . 0', { path: 'doc.md' }));
    assert.ok(answer.ok && answer.title === 'doc.md', JSON.stringify(answer));
  });

  /*
   * NEVER: HALF A DOCUMENT. Each of these is a subtree this cannot place,
   * and each refuses the whole view by name.
   */
  it('refuses, naming them, blocks it cannot place', () => {
    const root = record('e.1', 'root . 0', { title: 'Root' });
    const refusals: Array<[string, ReturnType<typeof compose>, string[]]> = [
      ['deleted', compose('e.1', root, record('e.2', '"e.1" . 0', { title: 'Gone' }, true)), ['e.2']],
      ['unsettled', compose('e.1', root, record('e.3', 'conflict 2', { title: 'Moved twice' })), ['e.3']],
      ['parent absent', compose('e.1', root, record('e.4', '"e.9" . 0', { title: 'Stray' })), ['e.4']],
      ['under the root of the store', compose('e.1', root, record('e.5', 'root . 1', { title: 'Beside' })), ['e.5']],
      ['opened block absent', compose('e.1', record('e.6', '"e.1" . 0', { title: 'Kid' })), ['e.1']],
      ['twice', compose('e.1', root, root), ['e.1']],
      [
        'a cycle',
        compose('e.1', root, record('e.7', '"e.8" . 0', { title: 'A' }), record('e.8', '"e.7" . 0', { title: 'B' })),
        ['e.7', 'e.8']
      ]
    ];
    for (const [name, answer, ids] of refusals) {
      assert.ok(!answer.ok, `${name}: composed anyway`);
      if (!answer.ok) {
        assert.deepStrictEqual(answer.ids, ids, `${name}: named ${answer.ids.join(', ')}`);
      }
    }
  });

  it('refuses a body or a title that is not text rather than showing it empty', () => {
    const conflict = '((id . "f.2") (deleted . #f) (fields (kind . section) (src conflict "one" "two") (title . "Split")) (position "f.1" . 0) (edges))';
    const answer = compose('f.1', record('f.1', 'root . 0', { title: 'Root' }), conflict);
    assert.ok(!answer.ok, JSON.stringify(answer));
    if (!answer.ok) {
      assert.deepStrictEqual(answer.ids, ['f.2']);
    }
    const titled = '((id . "f.3") (deleted . #f) (fields (kind . section) (title . 7)) (position "f.1" . 0) (edges))';
    const second = compose('f.1', record('f.1', 'root . 0', { title: 'Root' }), titled);
    assert.ok(!second.ok && second.ids.includes('f.3'), JSON.stringify(second));
  });

  it('refuses an answer holding something that is not a block', () => {
    const answer = composeDocument('g.1', [...parseAnswers(record('g.1', 'root . 0', { title: 'R' })), 'not a record']);
    assert.ok(!answer.ok, JSON.stringify(answer));
  });

  it('says what it refused and every block it refused over', () => {
    assert.strictEqual(
      refusalOf({ reason: 'these blocks cannot be placed under e.1', ids: ['e.4', 'e.5'] }),
      'the document was not opened, because these blocks cannot be placed under e.1: e.4, e.5.'
    );
  });

  it('carries its store and block in its address, and reads back nothing from a partial one', () => {
    const query = documentQuery('/tmp/a store?&=', 'a.1');
    assert.deepStrictEqual(readDocumentQuery(query), { store: '/tmp/a store?&=', id: 'a.1' });
    assert.strictEqual(readDocumentQuery('id=a.1'), null);
    assert.strictEqual(readDocumentQuery('store=s'), null);
    assert.strictEqual(readDocumentQuery('store=&id=a.1'), null);
  });

  /*
   * THE MENU ENTRY IS ON THE ROWS THE OUTLINE DRAWS. The manifest cannot
   * import the row's context value, so this compares the two spellings.
   */
  it('is offered on the outline\'s block rows and not in the command palette', () => {
    const root = path.join(__dirname, '..', '..', '..');
    const manifest = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
    const entries = manifest.contributes.menus['view/item/context'] as Array<{ command: string; when: string }>;
    const entry = entries.find((e) => e.command === 'theourgia.openAsDocument');
    assert.ok(entry !== undefined, 'no context-menu entry');
    assert.strictEqual(entry?.when, 'view == theourgiaOutline && viewItem =~ /^theourgia\\.block/');
    const source = fs.readFileSync(path.join(root, 'src', 'extension.ts'), 'utf8');
    assert.ok(source.includes("item.contextValue = 'theourgia.block';"), 'the rows no longer carry that context value');
    /*
     * A FILE ROW OF THE FILES VIEW IS A BLOCK ROW TOO, and a directory is not.
     */
    assert.ok(source.includes("item.contextValue = 'theourgia.block.file';"), 'the file rows no longer carry their context value');
    const offered = new RegExp((/viewItem =~ \/(.*)\/$/.exec(entry?.when ?? '') as RegExpExecArray)[1]);
    assert.deepStrictEqual(
      ['theourgia.block', 'theourgia.block.file', 'theourgia.dir', 'theourgia.pathless', 'theourgia.incomplete'].map((v) => offered.test(v)),
      [true, true, false, false, false]
    );
    const palette = manifest.contributes.menus.commandPalette as Array<{ command: string; when: string }>;
    assert.deepStrictEqual(
      palette.find((e) => e.command === 'theourgia.openAsDocument'),
      { command: 'theourgia.openAsDocument', when: 'false' }
    );
  });
});

/*
 * WHAT THE EDITOR IS GIVEN FOR AN ADDRESS. The client here is never used
 * to send anything: the composer is a stand-in that records who it was
 * asked about, so a cell can say which store a read went to.
 */
describe('plugin-r3 6 the text behind a document address', () => {
  const clientOf = (name: string) => ({ name } as unknown as Client);
  const asked: Array<[string, string]> = [];
  const composer = async (client: Client, id: string): Promise<Composed> => {
    asked.push([(client as unknown as { name: string }).name, id]);
    return { ok: true, title: 'T', text: `text of ${id}` };
  };
  const address = (store: string, id: string) => `theourgia-document:/x.md?${documentQuery(store, id)}`;

  beforeEach(() => {
    asked.length = 0;
  });

  it('gives back what a command composed without asking the store again', async () => {
    const texts = new DocumentTexts(() => ({ client: clientOf('s'), store: 's' }), composer);
    texts.hold(address('s', 'a.1'), 'held text');
    assert.strictEqual(await texts.textFor(address('s', 'a.1'), documentQuery('s', 'a.1')), 'held text');
    assert.deepStrictEqual(asked, []);
  });

  it('composes an address nothing composed, once, from the store it names', async () => {
    const texts = new DocumentTexts(() => ({ client: clientOf('s'), store: 's' }), composer);
    const where = address('s', 'a.1');
    assert.strictEqual(await texts.textFor(where, documentQuery('s', 'a.1')), 'text of a.1');
    assert.strictEqual(await texts.textFor(where, documentQuery('s', 'a.1')), 'text of a.1');
    assert.deepStrictEqual(asked, [['s', 'a.1']], 'it asked again, or asked something else');
    texts.forget(where);
    await texts.textFor(where, documentQuery('s', 'a.1'));
    assert.strictEqual(asked.length, 2, 'a forgotten address was not composed again');
  });

  /*
   * queue item 28, C28b: A TEXT HELD FOR ONE STORE IS NOT SERVED FOR ANOTHER.
   * Held under store A's address, the same block id under store B's address is
   * composed from B. The other-store cell below starts with nothing held, so a
   * holder keyed by the block id alone passed it (item 6's review, M33).
   */
  it('composes the same id under another store from that store, not from a text held for the first', async () => {
    let now = { client: clientOf('A'), store: '/stores/A' };
    const texts = new DocumentTexts(() => now, composer);
    texts.hold(address('/stores/A', 'a.1'), 'held for A');
    now = { client: clientOf('B'), store: '/stores/B' };
    const text = await texts.textFor(address('/stores/B', 'a.1'), documentQuery('/stores/B', 'a.1'));
    assert.notStrictEqual(text, 'held for A', 'the text held for store A was served under store B');
    assert.strictEqual(text, 'text of a.1');
    assert.deepStrictEqual(asked, [['B', 'a.1']], 'it did not compose from store B');
  });

  /*
   * NEVER: THE SAME ID IN ANOTHER STORE. A tab restored after the settings
   * moved names the store it came from; reading the configured one under
   * that id would show a different block under the old one's name.
   */
  it('refuses an address from another store without reading anything', async () => {
    const texts = new DocumentTexts(() => ({ client: clientOf('now'), store: '/stores/now' }), composer);
    await assert.rejects(
      texts.textFor(address('/stores/then', 'a.1'), documentQuery('/stores/then', 'a.1')),
      /composed from the store \/stores\/then, and the store configured now is \/stores\/now/
    );
    assert.deepStrictEqual(asked, []);
  });

  it('refuses when no store is configured, or the address names none', async () => {
    const none = new DocumentTexts(() => null, composer);
    await assert.rejects(none.textFor(address('s', 'a.1'), documentQuery('s', 'a.1')), /set theourgia.corePath/);
    const some = new DocumentTexts(() => ({ client: clientOf('s'), store: 's' }), composer);
    await assert.rejects(some.textFor('theourgia-document:/x.md?id=a.1', 'id=a.1'), /does not name a store and a block/);
    assert.deepStrictEqual(asked, []);
  });

  it('raises a composition that refused, with its reason and blocks, and holds nothing', async () => {
    let calls = 0;
    const refusing = async (): Promise<Composed> => {
      calls += 1;
      return { ok: false, reason: 'the answer holds deleted blocks', ids: ['a.2'] };
    };
    const texts = new DocumentTexts(() => ({ client: clientOf('s'), store: 's' }), refusing);
    const where = address('s', 'a.1');
    await assert.rejects(texts.textFor(where, documentQuery('s', 'a.1')), /because the answer holds deleted blocks: a\.2\./);
    await assert.rejects(texts.textFor(where, documentQuery('s', 'a.1')));
    assert.strictEqual(calls, 2, 'a refusal was held as if it were the text');
  });
});

describe('plugin-r3 6 the document, from a real store', function () {
  this.timeout(120000);
  let store: RealStore;
  let pinned: CorePin | undefined;

  before(async () => {
    pinned = pinCore();
    await initWire();
    store = await RealStore.make();
  });

  after(() => {
    try {
      store?.dispose();
    } finally {
      checkCorePin(pinned);
    }
  });

  async function insert(args: string[]): Promise<string> {
    const answer = await store.client.request('insert', args);
    assert.ok(answer.ok, answer.text);
    const made = /\(state \(\("([^"]+)"/.exec(answer.text);
    assert.ok(made !== null, `no id in ${answer.text}`);
    return (made as RegExpExecArray)[1];
  }

  /*
   * KEY: THE CASE THE VIEW EXISTS FOR. A tree built with `insert` prints
   * flat under `--md` -- every block `# <title>` -- so where one block ends
   * is not in the text; the positions are.
   */
  it('recomputes the levels of a tree built with insert, which --md prints flat', async () => {
    const top = await insert(['--under', 'root', '--title', 'Top', '--text', 'top body\n## body heading\n']);
    const kid = await insert(['--under', top, '--title', 'Kid', '--text', 'kid body\n']);
    const grand = await insert(['--under', kid, '--title', 'Grand', '--text', '```\n# not a heading\n```\n']);
    const gone = await insert(['--under', top, '--title', 'Gone']);
    const kid2 = await insert(['--under', top, '--title', 'Kid2']);
    const removed = await store.client.request('del', [gone]);
    assert.ok(removed.ok, removed.text);

    const flat = await store.client.request('read', [top, '--recursive', '--md']);
    assert.ok(flat.ok, flat.text);
    for (const title of ['Top', 'Kid', 'Grand', 'Kid2']) {
      assert.ok(
        new RegExp(`^# ${title}$`, 'm').test(flat.text),
        `the core no longer prints ${title} flat, and the reason for this view should be looked at again: ${flat.text}`
      );
    }

    const answer = await documentOf(store.client, top);
    assert.ok(answer.ok, JSON.stringify(answer));
    assert.strictEqual(answer.ok ? answer.title : '', 'Top');
    assert.strictEqual(
      answer.ok ? answer.text : '',
      [
        markerFor(top, 0),
        '# Top',
        'top body',
        '## body heading',
        markerFor(kid, 1),
        '## Kid',
        'kid body',
        markerFor(grand, 2),
        '### Grand',
        '```',
        '# not a heading',
        '```',
        markerFor(kid2, 1),
        '## Kid2',
        ''
      ].join('\n'),
      'the deleted block, or the order, or a level, is not what the store holds'
    );
  });

  it('puts an imported document back together with its levels taken from depth', async () => {
    await store.importMarkdown('doc.md', '# Doc Title\n\nintro\n\n## Alpha\n\nalpha body\n');
    const rows = await store.client.request('outline', []);
    const file = /- (\S+) +doc\.md/.exec(rows.text);
    assert.ok(file !== null, rows.text);
    const answer = await documentOf(store.client, (file as RegExpExecArray)[1]);
    assert.ok(answer.ok, JSON.stringify(answer));
    const text = answer.ok ? answer.text : '';
    assert.ok(/^<!-- theourgia block \S+ depth 0 -->\n# doc\.md\n/.test(text), text);
    assert.ok(/<!-- theourgia block \S+ depth 1 -->\n## Doc Title\n\nintro\n/.test(text), text);
    assert.ok(/<!-- theourgia block \S+ depth 2 -->\n### Alpha\n\nalpha body\n/.test(text), text);
  });

  /*
   * THE RULING NAMES THE MACHINE RENDERING, `--wire`. Without it the core
   * prints the records one per line and the client reads the same list, so
   * no composed text would show the difference; the request does.
   */
  it('asks for the subtree with --recursive --wire', async () => {
    const counting = new Counting(store.transport());
    const top = await insert(['--under', 'root', '--title', 'Asked']);
    const answer = await documentOf(new Client(counting), top);
    assert.ok(answer.ok, JSON.stringify(answer));
    assert.deepStrictEqual(counting.sent, [['read', top, '--recursive', '--wire']]);
  });

  it('raises a read the store refuses rather than composing nothing', async () => {
    await assert.rejects(documentOf(store.client, 'zzzzzzzz.99'), /would not read the subtree under zzzzzzzz\.99/);
  });
});
