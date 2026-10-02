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
 * THE HOVER: WHAT THE STORE HOLDS ABOUT THE NAME UNDER THE POINTER
 * (src/hover.ts; the brief's cells H1 to H24).
 *
 * KEY: THE INVARIANT. The hover reads only, and answers about the store the
 * document says it came from; anything it cannot place answers undefined.
 * Most cells drive the module through a scripted client that records what was
 * asked; H15 and H16 run on a real store; H19, H20 and H24 run in the window
 * (datum-view.test.ts drives the harness, here they are cells of their own).
 */

import * as assert from 'assert';
import { spawnSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Answer, Client } from '../../src/client';
import { HOVER_MORE, OPEN_BLOCK } from '../../src/commands';
import {
  HOVER_COMMANDS,
  HoverAnswer,
  HoverPlace,
  HoverService,
  blockAt,
  commandLink,
  firstSentence,
  gatherContext,
  holdsWord,
  hoverMarkdown,
  listEntries,
  nameUnder
} from '../../src/hover';
import { initWire, parseAnswers } from '../../src/wire';
import { RealStore } from '../support/real-core';

type Reply = string | { rc: number; text: string } | Error | Promise<string>;

/*
 * A CLIENT THAT ANSWERS FROM A SCRIPT, keyed by the request as `verb args...`,
 * and records every request. A request the script does not name answers as an
 * empty store does: no definition, no references, no matches.
 */
function scripted(script: Record<string, Reply>) {
  const asked: string[] = [];
  const client = {
    request: async (verb: string, args: string[] = []): Promise<Answer> => {
      const key = [verb, ...args].join(' ');
      asked.push(key);
      let reply: Reply | undefined = script[key];
      if (reply === undefined) {
        reply = verb === 'whereis' ? { rc: 1, text: `(error unknown-name ${args[0]} (nearest))\n` } : '';
      }
      if (reply instanceof Error) {
        throw reply;
      }
      const resolved = reply instanceof Promise ? await reply : reply;
      const { rc, text } = typeof resolved === 'string' ? { rc: 0, text: resolved } : resolved;
      return {
        argv: [verb, ...args],
        rc,
        ok: rc === 0,
        kind: 'items',
        text,
        answers: text.trim() === '' ? [] : parseAnswers(text),
        envelope: null,
        stderr: '',
        notes: null
      } as Answer;
    }
  } as unknown as Client;
  return { client, asked };
}

const record = (id: string, kind: string, title: string, src: string): string =>
  `(ok ((id . "${id}") (deleted . #f) (fields (kind . ${kind}) (src . ${JSON.stringify(src)}) (title . ${JSON.stringify(title)})) (position root . 0) (edges)))\n`;
const link = (from: string, rel: string): string => `(ref (from "${from}") (rel ${rel}) (via link))\n`;
const textRef = (from: string): string => `(ref (from "${from}") (rel ref) (via md))\n`;
const match = (id: string, text: string): string => `(match "${id}" 1 ${JSON.stringify(text)})\n`;

const NEVER = { isCancellationRequested: false };

function service(client: Client, logged: string[], now = () => 0, deadline = 800): HoverService {
  return new HoverService({ clientFor: (store) => (store === '/s' ? client : null), log: (l) => logged.push(l), now }, deadline);
}

function schedule(name: string): any {
  const run = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
    encoding: 'utf8',
    timeout: 150000
  });
  assert.strictEqual(run.status, 0, run.stdout + run.stderr);
  const result = JSON.parse(run.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete, true);
  return result.result;
}

describe('the hover says what the store holds about the name under the pointer', function () {
  this.timeout(20000);
  before(async () => {
    await initWire();
  });

  it('H1 shows an edge someone wrote with its relation, title and kind, and drops it once unlinked', async () => {
    const { client } = scripted({
      'refs t.1': link('d.1', 'implements') + link('d.2', 'depends-on'),
      'read d.1': record('d.1', 'decision', 'Grow the buffer', 'Double it. Then copy.'),
      'read d.2': record('d.2', 'section', 'Allocation', 'One arena.')
    });
    const answer = (await gatherContext(client, 't.1', 'alpha', false, () => false)) as HoverAnswer;
    assert.deepStrictEqual(
      answer.lines.map((l) => [l.section, l.rel, l.title, l.kind]),
      [
        ['edge', 'implements', 'Grow the buffer', 'decision'],
        ['edge', 'depends-on', 'Allocation', 'section']
      ]
    );
    const unlinked = scripted({});
    const logged: string[] = [];
    assert.strictEqual(await service(unlinked.client, logged).answer('/s', 't.1', 'alpha', false, NEVER), undefined);
  });

  it('H2 lists a prose block that refers to the target in its text, with its first sentence, under text references', async () => {
    const { client } = scripted({ 'refs t.1': textRef('n.1'), 'read n.1': record('n.1', 'section', 'Note', 'See [[t.1]] here. More.') });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.lines.map((l) => [l.section, l.title, l.kind, l.sentence]), [['text', 'Note', 'section', 'See [[t.1]] here.']]);
  });

  it('H3 mentions: a whole word in prose is listed; a longer identifier and a code block are not', async () => {
    const { client } = scripted({
      'grep alpha': match('p.1', 'We call alpha first.') + match('p.2', 'Only alpha_extra here.') + match('c.1', '(define (alpha x) x)'),
      'read p.1': record('p.1', 'section', 'Usage', 'We call alpha first.'),
      'read c.1': record('c.1', 'code', 'alpha', '(define (alpha x) x)')
    });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.lines.map((l) => [l.section, l.id]), [['mention', 'p.1']]);
    assert.strictEqual(answer.more, 0);
    assert.strictEqual(holdsWord('Only alpha_extra here.', 'alpha', false), false);
    assert.strictEqual(holdsWord('call (alpha x)', 'alpha', true), true);
    assert.strictEqual(holdsWord('call alpha-beta', 'alpha', true), false);
  });

  it('H4 answers undefined when nothing refers and nothing mentions, and logs nothing', async () => {
    const { client } = scripted({});
    const logged: string[] = [];
    assert.strictEqual(await service(client, logged).answer('/s', 't.1', 'alpha', false, NEVER), undefined);
    assert.deepStrictEqual(logged, []);
  });

  it('H5 answers undefined with one output line for a refused refs and for a transport failure; unknown-name is no failure', async () => {
    for (const script of [{ 'refs t.1': { rc: 1, text: '(error unknown-id "t.1" (nearest))\n' } }, { 'refs t.1': new Error('the transport failed') }]) {
      const { client } = scripted(script);
      const logged: string[] = [];
      assert.strictEqual(await service(client, logged).answer('/s', 't.1', 'alpha', false, NEVER), undefined);
      assert.strictEqual(logged.length, 1, JSON.stringify(logged));
    }
    const shown = scripted({ 'refs t.1': link('d.1', 'implements'), 'read d.1': record('d.1', 'decision', 'D', 'd.') });
    const answer = await service(shown.client, []).answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(answer !== undefined && answer.lines.length === 1);
  });

  it('H6 keeps nothing when the hover is left before the answers, and asks the token before the cache', async () => {
    let release: (text: string) => void = () => undefined;
    const held = new Promise<string>((resolve) => {
      release = resolve;
    });
    const { client, asked } = scripted({ 'refs t.1': held, 'read d.1': record('d.1', 'decision', 'D', 'd.') });
    const hovers = service(client, []);
    const token = { isCancellationRequested: false };
    const pending = hovers.answer('/s', 't.1', 'alpha', false, token);
    token.isCancellationRequested = true;
    release(link('d.1', 'implements'));
    assert.strictEqual(await pending, undefined);
    const before = asked.length;
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(asked.length > before, 'the left hover filled the cache');
    const after = asked.length;
    assert.strictEqual(await hovers.answer('/s', 't.1', 'alpha', false, { isCancellationRequested: true }), undefined);
    assert.strictEqual(asked.length, after, 'a cancelled hover asked the store');
  });

  it('H7 shows five lines in the order edges, text references, mentions, then "up to 3 more mentions", and the list holds all eight', async () => {
    const script: Record<string, Reply> = {
      'refs t.1': link('e.1', 'implements') + link('e.2', 'implements') + link('e.3', 'depends-on') + textRef('r.1') + textRef('r.2'),
      'grep alpha': match('m.1', 'alpha here') + match('m.2', 'alpha there') + match('m.3', 'alpha again')
    };
    for (const id of ['e.1', 'e.2', 'e.3', 'r.1', 'r.2', 'm.1', 'm.2', 'm.3']) {
      script[`read ${id}`] = record(id, 'section', `T ${id}`, `${id}.`);
    }
    const { client } = scripted(script);
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.lines.map((l) => l.id), ['e.1', 'e.2', 'e.3', 'r.1', 'r.2']);
    assert.strictEqual(answer.more, 0);
    assert.strictEqual(answer.unverified, 3);
    const markdown = hoverMarkdown(answer, '/s', OPEN_BLOCK.id, HOVER_MORE.id, 'alpha');
    assert.ok(markdown.includes(`[up to 3 more mentions](${commandLink(HOVER_MORE.id, ['/s', 'alpha', answer.candidates])})`), markdown);
    const listed = await listEntries(client, answer.candidates);
    assert.deepStrictEqual(listed.map((e) => e.id), ['e.1', 'e.2', 'e.3', 'r.1', 'r.2', 'm.1', 'm.2', 'm.3']);
  });

  it('says "up to" for mentions it has not read, and the list shows only the prose ones among them', async () => {
    const script: Record<string, Reply> = {
      'refs t.1': ['e.1', 'e.2', 'e.3', 'e.4', 'e.5'].map((id) => link(id, 'implements')).join(''),
      'grep alpha': match('m.1', 'alpha here') + match('l.1', '(export alpha)') + match('m.2', 'alpha there'),
      'read l.1': record('l.1', 'library', 'probe', 'x'),
      'read m.1': record('m.1', 'section', 'M1', 'm.'),
      'read m.2': record('m.2', 'task', 'M2', 'm.')
    };
    for (const id of ['e.1', 'e.2', 'e.3', 'e.4', 'e.5']) {
      script[`read ${id}`] = record(id, 'decision', `E ${id}`, `${id}.`);
    }
    const { client } = scripted(script);
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.strictEqual(answer.lines.length, 5);
    assert.strictEqual(answer.more, 0);
    assert.strictEqual(answer.unverified, 3);
    const markdown = hoverMarkdown(answer, '/s', OPEN_BLOCK.id, HOVER_MORE.id, 'alpha');
    assert.ok(markdown.includes('[up to 3 more mentions]') && !/\[\d+ more\]/.test(markdown), markdown);
    const listed = await listEntries(client, answer.candidates);
    assert.deepStrictEqual(listed.filter((e) => e.description.startsWith('mentions')).map((e) => e.id), ['m.1', 'm.2']);
  });

  it('H8 asks exactly the sequence once, answers again from the cache, asks for another name, and asks again after a drop', async () => {
    const { client, asked } = scripted({
      'whereis alpha --wire': '(def "d.9" (library (probe d)) (name alpha) (kind code))\n',
      'refs t.1': link('e.1', 'implements'),
      'refs d.9': textRef('r.1'),
      'read e.1': record('e.1', 'decision', 'E', 'e.'),
      'read r.1': record('r.1', 'section', 'R', 'r.')
    });
    const hovers = service(client, []);
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.deepStrictEqual(asked, ['whereis alpha --wire', 'refs t.1', 'grep alpha', 'refs d.9', 'read e.1', 'read r.1']);
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.strictEqual(asked.length, 6, 'the same hover asked again');
    await hovers.answer('/s', 't.1', 'beta', false, NEVER);
    assert.ok(asked.length > 6 && asked[6] === 'whereis beta --wire', JSON.stringify(asked));
    const before = asked.length;
    hovers.drop();
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(asked.length > before, 'a dropped cache answered');
  });

  it('H8 does not refill the cache from an answer that began before a drop', async () => {
    let release: (text: string) => void = () => undefined;
    const held = new Promise<string>((resolve) => {
      release = resolve;
    });
    const { client, asked } = scripted({ 'refs t.1': held, 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    const hovers = service(client, []);
    const pending = hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    hovers.drop();
    release(link('e.1', 'implements'));
    await pending;
    const before = asked.length;
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(asked.length > before, 'an answer begun before the drop was kept after it');
  });

  it('H9 links each line to open-block with its id and store, and enables exactly the two commands', async () => {
    const { client } = scripted({ 'refs t.1': link('e.1', 'implements'), 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    const markdown = hoverMarkdown(answer, '/stores/A', OPEN_BLOCK.id, HOVER_MORE.id, 'alpha');
    assert.ok(markdown.includes(commandLink(OPEN_BLOCK.id, ['e.1', '/stores/A'])), markdown);
    assert.deepStrictEqual([...HOVER_COMMANDS], [OPEN_BLOCK.id, HOVER_MORE.id]);
    const commands = [...markdown.matchAll(/\(command:([^?)]+)/g)].map((m) => m[1]);
    assert.ok(commands.every((c) => HOVER_COMMANDS.includes(c)), JSON.stringify(commands));
  });

  it('H10 shows a title and a sentence holding markdown and HTML characters escaped', async () => {
    const { client } = scripted({ 'refs t.1': link('e.1', 'implements'), 'read e.1': record('e.1', 'decision', 'A *b* <i>c</i>', 'Use [x](y) & <b>z</b>.') });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    const markdown = hoverMarkdown(answer, '/s', OPEN_BLOCK.id, HOVER_MORE.id, 'alpha');
    assert.ok(!markdown.includes('<i>') && !markdown.includes('<b>') && !markdown.includes('*b*') && !markdown.includes('[x](y)'), markdown);
    assert.ok(markdown.includes('&lt;i&gt;') && markdown.includes('&amp;'), markdown);
  });

  it('H11 cuts a 500-character first paragraph at 200 and shows the first of two sentences', () => {
    assert.strictEqual(firstSentence('x'.repeat(500)).length, 200);
    assert.strictEqual(firstSentence('One here. Two there.'), 'One here.');
    assert.strictEqual(firstSentence('\n  Line one\nline two'), 'Line one');
  });

  it('H12 shows no line for a referrer that cannot be read and counts it under "N more"', async () => {
    const { client } = scripted({
      'refs t.1': link('e.1', 'implements') + link('e.2', 'implements'),
      'read e.1': { rc: 1, text: '(error unknown-id "e.1" (nearest))\n' },
      'read e.2': record('e.2', 'decision', 'E2', 'e.')
    });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.lines.map((l) => l.id), ['e.2']);
    assert.strictEqual(answer.more, 1);
  });

  it('H13 shows the link row and not an editor-supplied calls row of the same answer', async () => {
    const { client } = scripted({
      'refs t.1': link('e.1', 'implements') + '(ref (from "c.1") (rel calls) (via (vscode "1.138.0" "c")))\n',
      'read e.1': record('e.1', 'decision', 'E', 'e.')
    });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.candidates.map((c) => c.id), ['e.1']);
  });

  it('H14 answers undefined past the deadline with one output line, and the next hover from the late answers', async () => {
    const slow = new Promise<string>((resolve) => setTimeout(() => resolve(link('e.1', 'implements')), 120));
    const { client, asked } = scripted({ 'refs t.1': slow, 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    const logged: string[] = [];
    const hovers = service(client, logged, () => 0, 40);
    assert.strictEqual(await hovers.answer('/s', 't.1', 'alpha', false, NEVER), undefined);
    assert.strictEqual(logged.length, 1, JSON.stringify(logged));
    await new Promise((resolve) => setTimeout(resolve, 200));
    const before = asked.length;
    const next = await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(next !== undefined && next.lines.length === 1);
    assert.strictEqual(asked.length, before, 'the late answers did not fill the cache');
  });

  it('H17 asks again once an entry is older than 15 seconds', async () => {
    const { client, asked } = scripted({ 'refs t.1': link('e.1', 'implements'), 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    let now = 0;
    const hovers = service(client, [], () => now);
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    const first = asked.length;
    now = 14999;
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.strictEqual(asked.length, first);
    now = 15001;
    await hovers.answer('/s', 't.1', 'alpha', false, NEVER);
    assert.ok(asked.length > first, 'an old entry was answered from');
  });

  it('H18 reads the name by the editor word range in a C file and by the Scheme reader in a Scheme block', () => {
    const c: HoverPlace = { kind: 'block', store: '/s', blockId: 'c.1', scheme: false };
    assert.strictEqual(nameUnder(c, 'buf->data = 0;', 0, 7, () => 'data'), 'data');
    const scheme: HoverPlace = { kind: 'block', store: '/s', blockId: 's.1', scheme: true };
    assert.strictEqual(nameUnder(scheme, '(set-car! x 1)', 0, 3, () => 'car'), 'set-car!');
  });

  it('H19 answers about the store the document names, and from no other store', async () => {
    const { client, asked } = scripted({ 'refs t.1': link('e.1', 'implements'), 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    const hovers = service(client, []);
    assert.ok((await hovers.answer('/s', 't.1', 'alpha', false, NEVER)) !== undefined);
    const before = asked.length;
    assert.strictEqual(await hovers.answer('/other', 't.1', 'alpha', false, NEVER), undefined);
    assert.strictEqual(asked.length, before, 'another store\'s document was answered from the configured store');
  });

  it('answers nothing, asking nothing, for a document that names no store or no block', async () => {
    const { client, asked } = scripted({ 'refs t.1': link('e.1', 'implements'), 'read e.1': record('e.1', 'decision', 'E', 'e.') });
    const hovers = new HoverService({ clientFor: () => client, log: () => undefined, now: () => 0 });
    assert.strictEqual(await hovers.answer('', 't.1', 'alpha', false, NEVER), undefined);
    assert.strictEqual(await hovers.answer('/s', '', 'alpha', false, NEVER), undefined);
    assert.deepStrictEqual(asked, []);
  });

  it('H20 places no block on a marker line of either view, and places T1 inside each view', () => {
    const header = `;; @file ${Buffer.from('(code-projection 1 "s0000001" "d.1" (("w" . 2)) datum 0)').toString('hex')}`;
    const datum = `#!chezscheme\n${header}\n(library (probe d)\n(export alpha)\n;; @block d.2\n(define (alpha x) x)\n)\n`;
    const place: HoverPlace = { kind: 'datum', store: '/s' };
    assert.strictEqual(blockAt(place, datum, datum.indexOf(';; @block') + 3), null);
    assert.strictEqual(blockAt(place, datum, datum.indexOf(';; @file') + 3), null);
    assert.strictEqual(blockAt(place, datum, datum.indexOf('(define') + 9), 'd.2');
    assert.strictEqual(blockAt(place, datum, datum.indexOf('(export') + 2), 'd.1');
    const subtree = '<!-- theourgia block a.1 depth 0 -->\n# A\n\nbody\n<!-- theourgia block a.2 depth 1 -->\n## B\n\ninner\n';
    const view: HoverPlace = { kind: 'subtree', store: '/s' };
    assert.strictEqual(blockAt(view, subtree, 3), null);
    assert.strictEqual(blockAt(view, subtree, subtree.indexOf('body')), 'a.1');
    assert.strictEqual(blockAt(view, subtree, subtree.indexOf('inner')), 'a.2');
    assert.strictEqual(blockAt({ kind: 'block', store: '/s', blockId: 'b.1', scheme: false }, 'x', 0), 'b.1');
  });

  it('H21 reads set-car! as one name in a datum view shown as plain text', () => {
    assert.strictEqual(nameUnder({ kind: 'datum', store: '/s' }, '(set-car! x 1)', 0, 5, () => 'car'), 'set-car!');
  });

  it('H22 asks refs once when the block hovered defines the name itself', async () => {
    const { client, asked } = scripted({
      'whereis alpha --wire': '(def "t.1" (library (probe d)) (name alpha) (kind code))\n',
      'refs t.1': link('e.1', 'implements'),
      'read e.1': record('e.1', 'decision', 'E', 'e.')
    });
    const answer = await gatherContext(client, 't.1', 'alpha', true, () => false);
    assert.strictEqual(asked.filter((a) => a.startsWith('refs ')).length, 1, JSON.stringify(asked));
    assert.strictEqual(answer.lines.length, 1);
  });

  it('H23 lists a mention in a task block and not in a library or a file block', async () => {
    const { client } = scripted({
      'grep alpha': match('l.1', '(export alpha)') + match('f.1', 'alpha in a file') + match('k.1', 'Do alpha first.'),
      'read l.1': record('l.1', 'library', 'probe', 'x'),
      'read f.1': record('f.1', 'file', 'a.c', 'x'),
      'read k.1': record('k.1', 'task', 'Task', 'Do alpha first.')
    });
    const answer = await gatherContext(client, 't.1', 'alpha', false, () => false);
    assert.deepStrictEqual(answer.lines.map((l) => [l.id, l.kind]), [['k.1', 'task']]);
  });
});

describe('the hover in the window: its three kinds of document, its marker lines and its cache', function () {
  this.timeout(180000);
  let r: any;
  before(() => {
    r = schedule('datum-hover');
  });

  it('H19 answers in a block file, a subtree view and a datum view, each about its own store; another store\'s address answers nothing', () => {
    for (const kind of ['file', 'subtree', 'datum']) {
      assert.ok(r[kind].shown, `${kind}: no hover: ${JSON.stringify(r[kind])}`);
      assert.ok(r[kind].asked.includes('refs'), `${kind}: ${JSON.stringify(r[kind].asked)}`);
    }
    assert.strictEqual(r.otherStore.shown, false);
    assert.deepStrictEqual(r.otherStore.asked, []);
  });

  it('H20 answers nothing, asking nothing, on the marker lines of the two views', () => {
    for (const where of ['blockMarker', 'fileMarker', 'subtreeMarker']) {
      assert.strictEqual(r[where].shown, false, where);
      assert.deepStrictEqual(r[where].asked, [], where);
    }
  });

  it('cuts the name in a restored tab by the lang its record holds, and by the editor\'s language id when it holds none', () => {
    assert.deepStrictEqual(r.restoredScheme.names, ['set-car!'], JSON.stringify(r.restoredScheme));
    assert.deepStrictEqual(r.noLang.names, ['car'], JSON.stringify(r.noLang));
    assert.strictEqual(r.recordLang, 'scheme');
  });

  it('H24 answers the second hover from the cache across a mode invalidation, and asks again after a save', () => {
    assert.deepStrictEqual(r.afterInvalidation.asked, [], JSON.stringify(r.afterInvalidation));
    assert.ok(r.afterInvalidation.shown);
    assert.ok(r.afterSave.asked.length > 0, 'a save did not drop the hover cache');
  });
});

/*
 * H15 AND H16 ON A REAL STORE: an edge someone wrote, a reference in text and
 * the whole-word pair of mentions, on a text-mode file and on a datum library;
 * and the time of a cold hover with five referrers, recorded as a reading.
 */
describe('the hover on a real store', function () {
  this.timeout(240000);
  before(async () => {
    await initWire();
  });

  it('H15 shows a decision linked to the block, a text reference and a whole-word mention, on a text file and on a datum library', async () => {
    const store = await RealStore.make('hover-real');
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-hover-'));
    try {
      fs.writeFileSync(path.join(dir, 'probe.sls'), '(library (probe d)\n  (export alpha)\n  (import (rnrs))\n  (define (alpha x) (+ x 1)))\n');
      const text = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-hover-text-'));
      fs.writeFileSync(path.join(text, 'buf.c'), 'void alpha(void) {}\n');
      const transport = store.transport();
      const client = new Client(transport);
      assert.strictEqual((await transport.send('import-code', [dir, '--datum'])).rc, 0);
      assert.strictEqual((await transport.send('import-code', [text])).rc, 0);
      const outline = (await transport.send('outline', [])).stdout;
      const idOf = (title: string): string => {
        const found = new RegExp(`^\\s*- (\\S+)  ${title.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, 'm').exec(outline);
        assert.ok(found !== null, `no ${title} in ${outline}`);
        return found[1];
      };
      const datumDef = idOf('alpha');
      const cFile = idOf('buf.c');
      const cBlock = (new RegExp(`^- ${cFile}  buf\\.c\\n  - (\\S+)  `, 'm').exec(outline) ?? [])[1];
      assert.ok(cBlock !== undefined, outline);
      const insert = async (title: string, body: string): Promise<string> => {
        const made = await transport.send('insert', ['--title', title, '--text', body]);
        const id = (/\(state \(\("([^"]+)"/.exec(made.stdout) ?? [])[1];
        assert.ok(id !== undefined, made.stdout);
        return id;
      };
      for (const [target, label] of [
        [datumDef, 'datum'],
        [cBlock, 'text']
      ] as const) {
        const decision = await insert(`Decision ${label}`, `Why ${label}. More.\n`);
        assert.strictEqual((await transport.send('set', [decision, 'kind', 'decision'])).rc, 0);
        assert.strictEqual((await transport.send('link', [decision, 'implements', target])).rc, 0);
        await insert(`Note ${label}`, `See [[${target}]] for ${label}.\n`);
      }
      await insert('Mention', 'We call alpha first.\n');
      await insert('Not a mention', 'Only alpha_extra here.\n');
      for (const [t1, scheme, label] of [
        [datumDef, true, 'datum'],
        [cBlock, false, 'text']
      ] as const) {
        const answer = await gatherContext(client, t1, 'alpha', scheme, () => false);
        const said = JSON.stringify(answer);
        assert.ok(answer.lines.some((l) => l.section === 'edge' && l.rel === 'implements' && l.kind === 'decision' && l.title === `Decision ${label}`), said);
        assert.ok(answer.lines.some((l) => l.section === 'text' && l.title === `Note ${label}`), said);
        assert.ok(answer.lines.some((l) => l.section === 'mention' && l.title === 'Mention'), said);
        assert.ok(!answer.lines.some((l) => l.title === 'Not a mention'), said);
      }
      assert.ok(cFile.length > 0);
      fs.rmSync(text, { recursive: true, force: true });
    } finally {
      store.dispose();
      fs.rmSync(dir, { recursive: true, force: true });
    }
  });

  it('H16 records the time of a cold hover with five referrers (a reading, set against the 800 ms deadline)', async () => {
    const store = await RealStore.make('hover-time');
    try {
      const transport = store.transport();
      const client = new Client(transport);
      const made = await transport.send('insert', ['--title', 'Target', '--text', 'alpha target.\n']);
      const target = (/\(state \(\("([^"]+)"/.exec(made.stdout) ?? [])[1];
      assert.ok(target !== undefined, made.stdout);
      for (let i = 0; i < 5; i += 1) {
        const ref = await transport.send('insert', ['--title', `Ref ${i}`, '--text', `Ref ${i}.\n`]);
        const id = (/\(state \(\("([^"]+)"/.exec(ref.stdout) ?? [])[1];
        assert.strictEqual((await transport.send('link', [id as string, 'implements', target])).rc, 0);
      }
      const started = Date.now();
      const answer = await gatherContext(client, target, 'alpha', false, () => false);
      const took = Date.now() - started;
      assert.strictEqual(answer.lines.length, 5);
      console.log(`      H16 reading: a cold hover with five referrers took ${took} ms (deadline 800 ms)`);
    } finally {
      store.dispose();
    }
  });
});
