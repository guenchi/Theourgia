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
 * A SPLIT OF A SOURCE FILE, CUT WHERE THE EDITOR'S SYMBOLS START.
 *
 * These cells pin the conversion of the editor's positions into byte
 * offsets of the file on disk, the symbols file, the command's steps (save
 * first, read back, collect, ask again, send), the empty list, the core's
 * refusals, the census of those refusals, and -- on a real core -- that the
 * core takes the file this extension writes and cuts where it says.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execFileSync } from 'child_process';
import { createHash } from 'crypto';
import { Client, answerKind } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { SETTINGS_REFUSALS, Saver } from '../../src/saver';
import {
  CHANGED_WHILE_COLLECTING,
  COULD_NOT_SAVE,
  SYMBOL_KIND_NAMES,
  SplitDeps,
  SplitDocument,
  fileSymbols,
  runSplit,
  savedText,
  splitRefusalNotice,
  symbolsFileText,
  topLevelSymbols
} from '../../src/split-symbols';
import { CliTransport } from '../../src/transport';
import { answerOf, initWire, isSym, parseAnswers } from '../../src/wire';
import { FakeCore } from '../support/fake';
import { IGNORED_DURABILITY } from '../support/ignored-durability';

const CHECK = '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';
import { RealStore } from '../support/real-core';

/*
 * AN OUTCOME AS TEXT FOR A FAILURE MESSAGE: the reader's integers are
 * BigInts, which JSON.stringify refuses.
 */
const shown = (value: unknown): string => JSON.stringify(value, (_key, v) => (typeof v === 'bigint' ? String(v) : v));

const range = (sl: number, sc: number, el: number, ec: number) => ({
  start: { line: sl, character: sc },
  end: { line: el, character: ec }
});

/*
 * A FILE WITH EVERYTHING THAT MAKES THE EDITOR'S COUNT DIFFER FROM THE
 * FILE'S: a byte-order mark the editor does not show, CRLF line ends, and a
 * character that is three bytes in UTF-8 and one code unit in UTF-16,
 * before the second definition on its line.
 */
const TEXT = '// one\r\nfunction alpha() {}\r\n/* \u4e2d */ function beta() {\r\n  return 2;\r\n}\r\n';
const BYTES = Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), Buffer.from(TEXT, 'utf8')]);

function byteAt(unitIndex: number): number {
  return 3 + Buffer.byteLength(TEXT.slice(0, unitIndex), 'utf8');
}

describe('P1 the editor\'s positions become byte offsets of the file on disk', () => {
  before(async () => {
    await initWire();
  });

  it('counts the mark, the carriage returns and the UTF-8 bytes, and sends each symbol at its line start', () => {
    const saved = savedText(BYTES);
    assert.ok(saved !== null);
    assert.strictEqual(saved.bom, 3);
    const betaLine = TEXT.indexOf('/* ');
    const betaEnd = TEXT.lastIndexOf('}') + 1;
    const { symbols, left } = fileSymbols(saved, [
      { name: 'beta', kind: 11, range: range(2, 11, 4, 1) },
      { name: 'alpha', kind: 11, range: range(1, 0, 1, 19) }
    ]);
    assert.strictEqual(left, 0);
    assert.deepStrictEqual(symbols, [
      { start: byteAt(TEXT.indexOf('function alpha')), end: byteAt(TEXT.indexOf('{}') + 2), kind: 'function', name: 'alpha' },
      { start: byteAt(betaLine), end: byteAt(betaEnd), kind: 'function', name: 'beta' }
    ]);
    assert.strictEqual(BYTES.subarray(symbols[1].start - 2, symbols[1].start).toString('latin1'), '\r\n', 'a start is not a line start');
  });

  it('keeps the top level of either answer form, and only this file\'s', () => {
    const tree = [
      { name: 'a', kind: 11, range: range(0, 0, 1, 0), children: [{ name: 'inner', kind: 12, range: range(0, 2, 0, 5) }] }
    ];
    assert.deepStrictEqual(topLevelSymbols(tree, 'file:///x.js').map((s) => s.name), ['a']);
    const here = { toString: () => 'file:///x.js' };
    const there = { toString: () => 'file:///y.js' };
    const flat = [
      { name: 'top', kind: 4, containerName: '', location: { uri: here, range: range(0, 0, 2, 0) } },
      { name: 'method', kind: 5, containerName: 'top', location: { uri: here, range: range(1, 0, 1, 4) } },
      { name: 'elsewhere', kind: 11, containerName: '', location: { uri: there, range: range(0, 0, 1, 0) } }
    ];
    assert.deepStrictEqual(topLevelSymbols(flat, 'file:///x.js').map((s) => s.name), ['top']);
    assert.deepStrictEqual(topLevelSymbols(undefined, 'file:///x.js'), []);
  });

  it('sends two symbols of one line as one start, and ends each before the next begins', () => {
    const text = 'const a = 1, b = 2;\nfunction c() {}\n';
    const saved = savedText(Buffer.from(text, 'utf8'));
    assert.ok(saved !== null);
    const { symbols } = fileSymbols(saved, [
      { name: 'a', kind: 12, range: range(0, 6, 0, 11) },
      { name: 'b', kind: 12, range: range(0, 13, 1, 3) },
      { name: 'c', kind: 11, range: range(1, 0, 1, 15) }
    ]);
    assert.deepStrictEqual(symbols.map((s) => [s.start, s.end, s.kind, s.name]), [
      [0, 20, 'variable', 'a, b'],
      [20, 35, 'function', 'c']
    ]);
  });

  it('takes a position between the halves of a surrogate pair after the pair, never inside the character', () => {
    const saved = savedText(Buffer.from('\ud83d\ude00x\n', 'utf8'));
    assert.ok(saved !== null);
    const { symbols } = fileSymbols(saved, [{ name: 'smile', kind: 13, range: range(0, 0, 0, 1) }]);
    assert.deepStrictEqual(symbols.map((s) => [s.start, s.end]), [[0, 4]], 'the end fell inside the four-byte character');
  });

  it('names the kinds as the core spells them, and leaves out a kind it has no name for', () => {
    assert.strictEqual(SYMBOL_KIND_NAMES.length, 26);
    assert.deepStrictEqual([SYMBOL_KIND_NAMES[0], SYMBOL_KIND_NAMES[11], SYMBOL_KIND_NAMES[21], SYMBOL_KIND_NAMES[25]], [
      'file',
      'function',
      'enummember',
      'typeparameter'
    ]);
    const saved = savedText(Buffer.from('x\n', 'utf8'));
    assert.ok(saved !== null);
    assert.deepStrictEqual(fileSymbols(saved, [{ name: 'x', kind: 99, range: range(0, 0, 0, 1) }]), { symbols: [], left: 1 });
  });

  it('writes the header and one line per symbol, a name with a quote read back as itself', () => {
    const text = symbolsFileText('ab12', '1.138.0', 'javascript', [{ start: 0, end: 5, kind: 'function', name: 'say "hi"' }]);
    const [header, line] = parseAnswers(text) as unknown[][];
    assert.strictEqual(
      text.split('\n')[0],
      '(symbols (digest "ab12") (source (vscode "1.138.0" "javascript")) (top-level #t))'
    );
    /*
     * EXACT INTEGERS, NOT FLOATS: the core refuses `#f8"..."` offsets as
     * malformed, and the writer prints a JS number that way.
     */
    assert.strictEqual(text.split('\n')[1], '(symbol 0 5 function "say \\"hi\\"")');
    assert.strictEqual(header.length, 4);
    assert.deepStrictEqual([line[1], line[2], (line[3] as { name: string }).name, line[4]], [BigInt(0), BigInt(5), 'function', 'say "hi"']);
    assert.ok(text.endsWith('\n'));
  });
});

/*
 * THE COMMAND'S STEPS, with a stand-in editor and a stand-in core.
 */
interface Rig {
  core: FakeCore;
  file: string;
  doc: SplitDocument & { dirty: boolean; ver: number; body: string; saves: number; saveWorks: boolean };
  deps: SplitDeps;
  written: Array<{ path: string; text: string }>;
  removed: string[];
}

const OK_ANSWER =
  '(ok (working-path "/r/split.js.review-1") (boundaries (0 30)) (warnings ((symbol-kinds function function))) ' +
  '(cuts-from (vscode "1.2.3" "javascript")))\n';

function rig(text: string, symbols: unknown, answer = OK_ANSWER, rc = 0): Rig {
  const core = new FakeCore([{ match: ['split-suggest'], stdout: answer, rc }]);
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-split-'));
  const file = path.join(dir, 'split.js');
  fs.writeFileSync(file, text);
  const written: Array<{ path: string; text: string }> = [];
  const removed: string[] = [];
  const doc = {
    uri: 'file:///split.js',
    fsPath: file,
    languageId: 'javascript',
    dirty: false,
    ver: 1,
    body: text,
    saves: 0,
    saveWorks: true,
    isDirty(): boolean {
      return this.dirty;
    },
    version(): number {
      return this.ver;
    },
    text(): string {
      return this.body;
    },
    async save(): Promise<boolean> {
      this.saves += 1;
      if (!this.saveWorks) {
        return false;
      }
      fs.writeFileSync(file, this.body);
      this.dirty = false;
      this.ver += 1;
      return true;
    }
  };
  const deps: SplitDeps = {
    client: new Client(new CliTransport(core.config(), core.env())),
    editorVersion: '1.2.3',
    readFile: (p) => fs.readFileSync(p),
    symbols: async () => symbols,
    symbolsPath: () => path.join(dir, `symbols-${written.length}.sexp`),
    writeSymbols: (p, t) => {
      fs.writeFileSync(p, t);
      written.push({ path: p, text: t });
    },
    removeSymbols: (p) => {
      removed.push(p);
      fs.rmSync(p, { force: true });
    }
  };
  return { core, file, doc, deps, written, removed };
}

const JS = '// one\nfunction alpha() {}\n\nfunction beta() {}\n';
const JS_SYMBOLS = [
  { name: 'alpha', kind: 11, range: range(1, 0, 1, 19), children: [] },
  { name: 'beta', kind: 11, range: range(3, 0, 3, 18), children: [] }
];

function splits(core: FakeCore): string[][] {
  return core.requests().filter((r) => r[0] === 'split-suggest');
}

describe('P1 P3 the command saves, reads back, collects, asks again, and sends', () => {
  let r: Rig;
  before(async () => {
    await initWire();
  });
  afterEach(() => r?.core.dispose());

  it('writes the symbols file against the bytes on disk, sends it, removes it, and answers the review', async () => {
    r = rig(JS, JS_SYMBOLS);
    const outcome = await runSplit(r.doc, r.deps);
    assert.strictEqual(outcome.done, 'split', shown(outcome));
    assert.strictEqual(r.written.length, 1);
    const lines = r.written[0].text.trimEnd().split('\n');
    const digest = createHash('sha256').update(fs.readFileSync(r.file)).digest('hex');
    assert.strictEqual(lines[0], `(symbols (digest "${digest}") (source (vscode "1.2.3" "javascript")) (top-level #t))`);
    assert.deepStrictEqual(lines.slice(1), [
      `(symbol ${JS.indexOf('function alpha')} ${JS.indexOf('{}') + 2} function "alpha")`,
      `(symbol ${JS.indexOf('function beta')} ${JS.lastIndexOf('{}') + 2} function "beta")`
    ]);
    const sent = splits(r.core);
    assert.strictEqual(sent.length, 1);
    assert.deepStrictEqual(sent[0].slice(0, 4), ['split-suggest', r.file, '--symbols', r.written[0].path]);
    assert.deepStrictEqual(r.removed, [r.written[0].path]);
    assert.strictEqual(fs.existsSync(r.written[0].path), false, 'the symbols file is still there');
    assert.ok(outcome.done === 'split');
    assert.strictEqual(outcome.review, '/r/split.js.review-1');
    assert.match(outcome.notice, /2 blocks proposed, cuts from \(vscode "1\.2\.3" "javascript"\); warnings \(\(symbol-kinds function function\)\)/);
  });

  it('saves a dirty document first, and counts the saved bytes', async () => {
    r = rig(JS, JS_SYMBOLS);
    const before = fs.statSync(r.file).mtimeMs;
    fs.utimesSync(r.file, new Date(before - 10000), new Date(before - 10000));
    const stale = fs.statSync(r.file).mtimeMs;
    r.doc.body = JS.replace('alpha', 'gamma');
    r.doc.dirty = true;
    const outcome = await runSplit(r.doc, r.deps);
    assert.strictEqual(outcome.done, 'split', shown(outcome));
    assert.strictEqual(r.doc.saves, 1);
    assert.ok(fs.statSync(r.file).mtimeMs > stale, 'the file was not written by the save');
    const digest = createHash('sha256').update(Buffer.from(r.doc.body, 'utf8')).digest('hex');
    assert.ok(r.written[0].text.startsWith(`(symbols (digest "${digest}")`), 'the digest is not of the saved bytes');
  });

  it('takes the digest over the file\'s own bytes, a byte-order mark included, and the editor\'s text without it', async () => {
    r = rig(JS, JS_SYMBOLS);
    fs.writeFileSync(r.file, Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), Buffer.from(JS, 'utf8')]));
    const outcome = await runSplit(r.doc, r.deps);
    assert.strictEqual(outcome.done, 'split', shown(outcome));
    const digest = createHash('sha256').update(fs.readFileSync(r.file)).digest('hex');
    assert.ok(r.written[0].text.startsWith(`(symbols (digest "${digest}")`), 'the digest is not of the bytes on disk');
    assert.ok(r.written[0].text.includes(`(symbol ${3 + JS.indexOf('function alpha')} `), 'the mark was not counted');
  });

  it('removes a symbols file whose write failed after creating it, and sends nothing', async () => {
    r = rig(JS, JS_SYMBOLS);
    r.deps.writeSymbols = (p) => {
      fs.writeFileSync(p, '(symbols (dig');
      throw new Error('the disk filled up');
    };
    await assert.rejects(runSplit(r.doc, r.deps), /the disk filled up/);
    assert.strictEqual(r.removed.length, 1, 'the half-written file was not handed to the removal');
    assert.strictEqual(fs.existsSync(r.removed[0]), false, 'the half-written file is still there');
    assert.deepStrictEqual(splits(r.core), []);
  });

  it('stops with "could not save" when the save fails, and sends nothing', async () => {
    r = rig(JS, JS_SYMBOLS);
    r.doc.dirty = true;
    r.doc.saveWorks = false;
    const outcome = await runSplit(r.doc, r.deps);
    assert.deepStrictEqual(outcome, { done: 'stopped', why: COULD_NOT_SAVE });
    assert.deepStrictEqual(splits(r.core), []);
    assert.deepStrictEqual(r.written, []);
  });

  it('stops with "changed while collecting" when the document moves during collection, and sends nothing', async () => {
    r = rig(JS, JS_SYMBOLS);
    r.deps.symbols = async () => {
      r.doc.ver += 1;
      return JS_SYMBOLS;
    };
    assert.deepStrictEqual(await runSplit(r.doc, r.deps), { done: 'stopped', why: CHANGED_WHILE_COLLECTING });
    r.core.dispose();
    r = rig(JS, JS_SYMBOLS);
    r.deps.symbols = async () => {
      fs.writeFileSync(r.file, JS + '// written by somebody else\n');
      return JS_SYMBOLS;
    };
    const again = await runSplit(r.doc, r.deps);
    assert.strictEqual(again.done, 'split', 'the editor text still equals what was read back, so this one is sent');
    r.core.dispose();
    r = rig(JS, JS_SYMBOLS);
    r.doc.body = JS + '// the editor has not reloaded a write that reached the disk\n';
    assert.deepStrictEqual(await runSplit(r.doc, r.deps), { done: 'stopped', why: CHANGED_WHILE_COLLECTING });
    assert.deepStrictEqual(splits(r.core), []);
  });
});

describe('P2 with no symbols the split runs without --symbols', () => {
  let r: Rig;
  before(async () => {
    await initWire();
  });
  afterEach(() => r?.core.dispose());

  it('sends the file alone, writes no symbols file, and says the cuts came from the patterns', async () => {
    r = rig(JS, [], '(ok (working-path "/r/x") (boundaries (0)) (warnings ()) (cuts-from regex))\n');
    const outcome = await runSplit(r.doc, r.deps);
    assert.ok(outcome.done === 'split', shown(outcome));
    assert.deepStrictEqual(splits(r.core).map((q) => q.slice(0, 2)), [['split-suggest', r.file]]);
    assert.ok(!splits(r.core)[0].includes('--symbols'));
    assert.deepStrictEqual(r.written, []);
    assert.strictEqual(outcome.notice, '1 block proposed, cuts from regex; no warnings');
  });
});

/*
 * THE REFUSALS OF THE SYMBOLS FILE: shown as the core wrote them, with the
 * table's sentence, and never sent again.
 */
const EIGHT = [
  'symbols-stale',
  'symbols-malformed',
  'symbols-past-end',
  'symbols-not-a-boundary',
  'symbols-unordered',
  'symbols-overlap',
  'symbols-not-a-line-start',
  'symbols-empty'
];

describe('P4 a refused split is shown as the core said it, and not sent again', () => {
  let r: Rig;
  before(async () => {
    await initWire();
  });
  afterEach(() => r?.core.dispose());

  it('carries symbols-stale with both digests, removes the symbols file, and sends once', async () => {
    const stale = '(error symbols-stale (digest-expected "aa") (digest-found "bb"))\n';
    r = rig(JS, JS_SYMBOLS, stale, 1);
    const outcome = await runSplit(r.doc, r.deps);
    assert.strictEqual(outcome.done, 'refused');
    assert.strictEqual(splits(r.core).length, 1);
    assert.strictEqual(fs.existsSync(r.written[0].path), false);
    assert.ok(outcome.done === 'refused');
    const notice = splitRefusalNotice(outcome.refusal);
    assert.ok(notice.includes('(error symbols-stale (digest-expected "aa") (digest-found "bb"))'), notice);
    assert.ok(notice.includes(SETTINGS_REFUSALS['symbols-stale']), notice);
  });

  it('holds a sentence for each of the eight kinds, in the settings family', () => {
    assert.deepStrictEqual(EIGHT.filter((k) => SETTINGS_REFUSALS[k] === undefined), []);
  });

  it('parks a save answered with any of the eight, for a person, with the kind\'s own sentence', async function () {
    this.timeout(60000);
    for (const kind of EIGHT) {
      const core = new FakeCore([
        { match: ['check'], stdout: CHECK, rc: 0 },
        { match: ['set'], stdout: `(error ${kind})\n`, rc: 1 }
      ]);
      try {
        const outbox = new Outbox(core.outboxFile());
        outbox.load();
        const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), outbox, () => undefined, IGNORED_DURABILITY);
        const outcome = await saver.save('a.2', 'src', 'x');
        assert.deepStrictEqual(
          [outcome.status, outcome.keptForAPerson, outcome.message],
          ['refused', true, SETTINGS_REFUSALS[kind]],
          `${kind} is not handled as the settings family`
        );
      } finally {
        core.dispose();
      }
    }
  });

  it('knows split-suggest as a verb answered by one datum', () => {
    assert.strictEqual(answerKind('split-suggest', ['/x.js', '--symbols', '/s']), 'datum');
  });
});

/*
 * THE CENSUS OF THE CORE'S REFUSALS FINDS THE EIGHT. The core raises them
 * through a named constructor, `symbols-refusal`, which the census scanner
 * reads (test/support/core-refusals.ss); without that, the refusal census
 * would be silent about all eight.
 */
describe('P4 the census scanner finds the eight in the pinned core', () => {
  it('lists every symbols refusal the core makes, from code-suggest.sc', () => {
    const directory = process.env.THEOURGIA_CORE;
    assert.ok(directory !== undefined && directory.length > 0, 'THEOURGIA_CORE is not set');
    const output = execFileSync(
      process.env.THEOURGIA_SCHEME ?? 'scheme',
      ['--script', path.join(__dirname, '../support/core-refusals.ss'), path.join(directory as string, 'code-suggest.sc')],
      { encoding: 'utf8' }
    );
    const found = output
      .trim()
      .split('\n')
      .map((row) => row.split('\t')[1])
      .filter((k) => k !== undefined && k.startsWith('symbols-'))
      .sort();
    assert.deepStrictEqual(found, [...EIGHT].sort());
  });
});

/*
 * ON A REAL CORE: the file this extension writes is the file the core
 * takes, and the cuts are where it says, the comment above a definition
 * going with it.
 */
describe('P1 on a real core the symbols file is taken and cut as given', function () {
  this.timeout(120000);
  let store: RealStore | undefined;
  afterEach(() => store?.dispose());

  it('answers the boundaries at the line starts, the comment attached, with cuts-from naming the editor', async () => {
    store = await RealStore.make();
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-split-real-'));
    const text = '// one\nfunction alpha() {\n  return 1;\n}\n\n// two\nfunction beta() {\n  return 2;\n}\n';
    const file = path.join(dir, 'split.js');
    fs.writeFileSync(file, text);
    const written: string[] = [];
    const doc: SplitDocument = {
      uri: 'file:///split.js',
      fsPath: file,
      languageId: 'javascript',
      isDirty: () => false,
      version: () => 1,
      text: () => text,
      save: async () => true
    };
    const outcome = await runSplit(doc, {
      client: store.client,
      editorVersion: '9.9.9',
      readFile: (p) => fs.readFileSync(p),
      symbols: async () => [
        { name: 'alpha', kind: 11, range: range(1, 0, 3, 1), children: [] },
        { name: 'beta', kind: 11, range: range(6, 0, 8, 1), children: [] }
      ],
      symbolsPath: () => path.join(dir, 'symbols.sexp'),
      writeSymbols: (p, t) => {
        fs.writeFileSync(p, t);
        written.push(p);
      },
      removeSymbols: (p) => fs.rmSync(p, { force: true })
    });
    assert.strictEqual(outcome.done, 'split', shown(outcome));
    assert.ok(outcome.done === 'split');
    assert.strictEqual(written.length, 1);
    assert.match(
      outcome.notice,
      new RegExp(`^2 blocks proposed, cuts from \\(vscode "9\\.9\\.9" "javascript"\\); warnings .*symbol-kinds function function`)
    );
    assert.ok(fs.existsSync(outcome.review), `the review file ${outcome.review} is not there`);
    assert.deepStrictEqual(outcome.boundaries, [0, Buffer.byteLength(text.slice(0, text.indexOf('// two')))], 'the second block does not start at its comment');
    const answer = await store.client.request('split-suggest', [file]);
    assert.ok(answer.ok, answer.text);
    const form = answerOf(answer.answers[0], 'ok');
    const from = form === null ? null : form.value('cuts-from');
    assert.ok(from !== null && from.read && isSym(from.value, 'regex'), `the twin without --symbols did not say regex: ${answer.text}`);
    fs.rmSync(dir, { recursive: true, force: true });
  });

  it('starts a symbol on the first line of a file with a byte-order mark at byte 0, and the core splits it', async () => {
    store = await RealStore.make();
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-split-real-'));
    const text = 'function a() {\n  return 1;\n}\nfunction b() {\n  return 2;\n}\n';
    const file = path.join(dir, 'marked.js');
    fs.writeFileSync(file, Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), Buffer.from(text, 'utf8')]));
    const written: string[] = [];
    const outcome = await runSplit(
      {
        uri: 'file:///marked.js',
        fsPath: file,
        languageId: 'javascript',
        isDirty: () => false,
        version: () => 1,
        text: () => text,
        save: async () => true
      },
      {
        client: store.client,
        editorVersion: '9.9.9',
        readFile: (p) => fs.readFileSync(p),
        symbols: async () => [
          { name: 'a', kind: 11, range: range(0, 0, 2, 1), children: [] },
          { name: 'b', kind: 11, range: range(3, 0, 5, 1), children: [] }
        ],
        symbolsPath: () => path.join(dir, 'symbols.sexp'),
        writeSymbols: (p, t) => {
          fs.writeFileSync(p, t);
          written.push(t);
        },
        removeSymbols: (p) => fs.rmSync(p, { force: true })
      }
    );
    assert.deepStrictEqual(written[0].trimEnd().split('\n').slice(1), [
      `(symbol 0 ${3 + text.indexOf('}\n') + 1} function "a")`,
      `(symbol ${3 + text.indexOf('function b')} ${3 + text.lastIndexOf('}') + 1} function "b")`
    ]);
    assert.strictEqual(outcome.done, 'split', shown(outcome));
    assert.ok(outcome.done === 'split');
    assert.match(outcome.notice, /^2 blocks proposed, cuts from \(vscode "9\.9\.9" "javascript"\)/);
    fs.rmSync(dir, { recursive: true, force: true });
  });

  it('cuts at the comment line above the second definition, as the patterns do', async () => {
    store = await RealStore.make();
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-split-real-'));
    const text = '// one\nfunction alpha() {\n  return 1;\n}\n\n// two\nfunction beta() {\n  return 2;\n}\n';
    const file = path.join(dir, 'split.js');
    fs.writeFileSync(file, text);
    const symbolsFile = path.join(dir, 'symbols.sexp');
    const digest = createHash('sha256').update(Buffer.from(text, 'utf8')).digest('hex');
    fs.writeFileSync(
      symbolsFile,
      symbolsFileText(digest, '9.9.9', 'javascript', [
        { start: text.indexOf('function alpha'), end: text.indexOf('}\n') + 1, kind: 'function', name: 'alpha' },
        { start: text.indexOf('function beta'), end: text.lastIndexOf('}') + 1, kind: 'function', name: 'beta' }
      ])
    );
    const answer = await store.client.request('split-suggest', [file, '--symbols', symbolsFile]);
    assert.ok(answer.ok, answer.text);
    assert.match(answer.text, new RegExp(`\\(boundaries \\(0 ${Buffer.byteLength(text.slice(0, text.indexOf('// two')))}\\)\\)`));
    fs.rmSync(dir, { recursive: true, force: true });
  });
});
