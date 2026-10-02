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
 * FACTS THE EDITOR COMPUTES, SUPPLIED TO THE STORE.
 *
 * These cells pin what the three supply commands collect from the editor's
 * providers, the supply file they write, the command's steps (empty the
 * directory, project, read back, collect, ask again, send one supply per
 * language, remove the file), and what each answer and refusal says. The
 * editor and the core are stand-ins: the core answers from a script, and a
 * client that writes the fixture projection when `export-code` is sent
 * stands in for the export itself.
 *
 * Byte offsets were counted from the fixture parts by a separate script,
 * not by the code under test; each fixture says its line starts.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { createHash } from 'crypto';
import { Answer, Client } from '../../src/client';
import { emptyDirectory, filesUnder, insideDirectory } from '../../src/fsops';
import { blockAt, readProjection } from '../../src/markers';
import { PositionLike, digestOf, lineStarts, savedText } from '../../src/split-symbols';
import {
  DIAGNOSTICS_CAP_MS,
  DIAGNOSTICS_QUIET_MS,
  Fact,
  OpenedDocument,
  ProjectedFile,
  SupplyDeps,
  SupplyOutcome,
  callFacts,
  declaredSymbols,
  declaredWords,
  diagnosticFacts,
  fileDepends,
  hoverFirstLine,
  readSupplyAnswer,
  runSupply,
  signatureFacts,
  supplyFileText,
  supplyNotice,
  supplyRefusalNotice,
  wordsOf
} from '../../src/supply';
import { CliTransport, TransportError } from '../../src/transport';
import { initWire, parseAnswers } from '../../src/wire';
import { FakeCore } from '../support/fake';
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
 * A HEADER AS THE CORE WRITES ONE, naming the file's block id; each id is
 * four characters, so every header line is 116 bytes under `// ` and 115
 * under `# `.
 */
function header(fileId: string, pad: number): string {
  const datum = `(code-projection 1 "s1" "${fileId}" (("${'w'.repeat(fileId.length)}" . 0)) text ${pad})`;
  return Buffer.from(datum, 'utf8').toString('hex');
}

function bytesOf(...parts: string[]): Buffer {
  return Buffer.from(parts.join(''), 'utf8');
}

/*
 * x.js, three blocks. Line starts: 0 header, 116 aa's marker, 135 alpha
 * (`alpha` at column 9), 169 bb's marker, 188 beta (column 9), 207 cc's
 * marker, 226 gamma (column 6). alpha's body calls a name it does not
 * declare.
 */
const X_JS = bytesOf(
  `// @file ${header('f.xx', 0)}\n`,
  '// @block aa pad 0\n',
  'function alpha(n) { fetchAll(); }\n',
  '// @block bb pad 0\n',
  'function beta() {}\n',
  '// @block cc pad 0\n',
  'const gamma = 1;\n'
);

/*
 * y.js, two blocks. Line starts: 0 header, 116 dd's marker, 135 the line
 * with a four-byte character (two UTF-16 units) before `bad` -- UTF-16
 * column 14, byte 151 --, 158 ee's marker, 177 `function e() {}`.
 */
const SMILE = String.fromCodePoint(0x1f600);
const Y_JS = bytesOf(
  `// @file ${header('f.yy', 0)}\n`,
  '// @block dd pad 0\n',
  `let s = "${SMILE}"; bad();\n`,
  '// @block ee pad 0\n',
  'function e() {}\n'
);

/*
 * z.js, one block declaring two functions: `first` at column 9, `second`
 * at column 29.
 */
const Z_JS = bytesOf(`// @file ${header('f.zz', 0)}\n`, '// @block aa pad 0\n', 'function first() {} function second() {}\n');

const Z_PY = bytesOf(`# @file ${header('f.zp', 0)}\n`, '# @block pp pad 0\n', 'def pi():\n    return 3\n');

const NOTES = bytesOf('plain notes, no header\n');

function projected(file: string, bytes: Buffer): ProjectedFile {
  const saved = savedText(bytes);
  assert.ok(saved !== null);
  const reading = readProjection(bytes, file);
  assert.ok(reading.ok, `the fixture did not read: ${JSON.stringify(reading)}`);
  return {
    path: file,
    fsPath: `/p/${file}`,
    uri: `file:///p/${file}`,
    digest: digestOf(bytes),
    saved,
    starts: lineStarts(saved.text),
    projection: reading.projection
  };
}

/*
 * A DocumentSymbol as a provider answers one: the name's selection on one
 * line, and children by name.
 */
function sym(name: string, kind: number, line: number, character: number, detail = '', children: string[] = []) {
  return {
    name,
    kind,
    detail,
    range: range(line, 0, line, character + name.length),
    selectionRange: range(line, character, line, character + name.length),
    children: children.map((c) => ({ name: c, kind: 12, detail: '', range: range(line, 0, line, 1), selectionRange: range(line, 0, line, 1) }))
  };
}

const hoverOf = (text: string) => [{ contents: [{ value: text }] }];

describe('collecting facts from the providers', () => {
  before(async () => {
    await initWire();
  });

  it('takes a signature from the detail, else from the hover\'s first line, else none', async () => {
    const x = projected('x.js', X_JS);
    const symbols = declaredSymbols([sym('alpha', 11, 2, 9, '(n: any): void'), sym('beta', 11, 4, 9), sym('gamma', 12, 6, 6)], x.uri);
    const asked: number[] = [];
    const hover = async (at: PositionLike): Promise<unknown> => {
      asked.push(at.line);
      if (at.line === 2) {
        return hoverOf('HOVER ALPHA');
      }
      return at.line === 4 ? hoverOf('```typescript\nfunction beta(): void\n```') : [];
    };
    const facts = await signatureFacts(x, symbols, hover);
    assert.deepStrictEqual(facts, [
      { kind: 'signature', id: 'aa', text: '(n: any): void', symbolKind: 'function', depends: ['aa', 'bb', 'cc'] },
      { kind: 'keywords', id: 'aa', words: ['alpha'], depends: ['aa', 'bb', 'cc'] },
      { kind: 'signature', id: 'bb', text: 'function beta(): void', symbolKind: 'function', depends: ['bb', 'aa', 'cc'] },
      { kind: 'keywords', id: 'bb', words: ['beta'], depends: ['bb', 'aa', 'cc'] },
      { kind: 'keywords', id: 'cc', words: ['gamma'], depends: ['cc', 'aa', 'bb'] }
    ]);
    assert.deepStrictEqual(asked, [4, 6], 'the hover was asked for a symbol whose detail was the signature');
  });

  it('takes the declaration in a hover\'s fence for its signature, not the heading above it', async () => {
    const x = projected('x.js', X_JS);
    const hover = async (at: PositionLike): Promise<unknown> =>
      at.line === 2 ? hoverOf('### variable `_server`\n\n---\n```cpp\nstatic int _server\n```') : hoverOf('### function `beta`');
    const facts = await signatureFacts(x, declaredSymbols([sym('alpha', 11, 2, 9), sym('beta', 11, 4, 9)], x.uri), hover);
    assert.deepStrictEqual(
      facts.filter((f) => f.kind === 'signature').map((f) => (f as { text: string }).text),
      ['static int _server', 'function beta']
    );
  });

  it('reads a hover one at a time, a fence within its own part, and code before prose', () => {
    const cases: Array<[string, unknown, string | null]> = [
      [
        'clangd: prose above the declaration fence',
        hoverOf('### function `foo`\n\n---\n\u2192 `int`\nParameters:\n- `int x`\n\n---\n```cpp\nint foo(int x)\n```'),
        'int foo(int x)'
      ],
      ['a plain hover, then a hover holding an example', [{ contents: 'int f(int x)' }, { contents: '```c\nf(1);\n```' }], 'int f(int x)'],
      [
        'an empty fence that never closes, then a hover with a heading',
        [{ contents: '```c\n' }, { contents: '### function `g`\n```c\nint g(void)\n```' }],
        'int g(void)'
      ],
      ['a fence that never closes', hoverOf('```c\nint h(void)'), 'int h(void)'],
      ['an empty fence part, then a part with a heading', [{ contents: ['```c', '### function `i`\n```c\nint i(void)\n```'] }], 'int i(void)'],
      ['rules above a plain line', hoverOf('---\n***\n___\nint k(void)'), 'int k(void)'],
      ['rules only', hoverOf('---\n\n***'), null],
      ['a part marked with its language, after prose', [{ contents: ['Doc first.', { language: 'c', value: '\nint m(void)' }] }], 'int m(void)'],
      ['nothing', [{ contents: [] }, {}], null]
    ];
    for (const [label, answer, expected] of cases) {
      assert.strictEqual(hoverFirstLine(answer), expected, label);
    }
  });

  it('ranks a call before a type before anything else, and takes the first among equals', async () => {
    const z = projected('z.js', Z_JS);
    /*
     * Kinds as VS Code numbers them: 4 class, 5 method, 8 constructor,
     * 9 enum, 10 interface, 11 function, 12 variable, 13 constant,
     * 22 struct. Each pair is declared in this order; the second wins
     * unless it ranks no better.
     */
    const pairs: Array<[number, number, 'first' | 'second']> = [
      [12, 4, 'second'],
      [13, 22, 'second'],
      [12, 9, 'second'],
      [12, 10, 'second'],
      [4, 5, 'second'],
      [10, 8, 'second'],
      [22, 11, 'second'],
      [11, 4, 'first'],
      [5, 8, 'first'],
      [4, 22, 'first'],
      [12, 13, 'first']
    ];
    for (const [a, b, expected] of pairs) {
      const symbols = declaredSymbols([sym('first', a, 2, 9, 'first'), sym('second', b, 2, 29, 'second')], z.uri);
      const facts = await signatureFacts(z, symbols, async () => []);
      assert.strictEqual((facts[0] as { text: string }).text, expected, `${a} then ${b}`);
    }
  });

  it('describes a block by its first function, not by a variable declared before it', async () => {
    const z = projected('z.js', Z_JS);
    const symbols = declaredSymbols([sym('first', 12, 2, 9, 'let first'), sym('second', 11, 2, 29, 'second()')], z.uri);
    const facts = await signatureFacts(z, symbols, async () => []);
    assert.deepStrictEqual(facts[0], { kind: 'signature', id: 'aa', text: 'second()', symbolKind: 'function', depends: ['aa'] });
  });

  it('gives a block that declares two functions one signature, the first, and the words of both', async () => {
    /*
     * WHY ONE: the core keeps one signature per block, and a block's
     * signature is what a reader of that block is shown first. Its words
     * and its calls are facts about the block, so they cover every name it
     * declares. The symbols arrive in reverse order; position decides.
     */
    const z = projected('z.js', Z_JS);
    const symbols = declaredSymbols([sym('second', 11, 2, 29, 'second()'), sym('first', 11, 2, 9, 'first()')], z.uri);
    const facts = await signatureFacts(z, symbols, async () => []);
    assert.deepStrictEqual(facts, [
      { kind: 'signature', id: 'aa', text: 'first()', symbolKind: 'function', depends: ['aa'] },
      { kind: 'keywords', id: 'aa', words: ['first', 'second'], depends: ['aa'] }
    ]);
  });

  it('leads every fact\'s dependencies with its own block, then the rest of its file in file order', async () => {
    const x = projected('x.js', X_JS);
    assert.deepStrictEqual(fileDepends(x, 'bb'), ['bb', 'aa', 'cc']);
    assert.deepStrictEqual(fileDepends(x, 'cc'), ['cc', 'aa', 'bb']);
    /*
     * A symbol whose name starts on a marker line is in no block, and gives
     * no fact at all.
     */
    const symbols = declaredSymbols([sym('ghost', 11, 1, 3), sym('gamma', 12, 6, 6, 'number')], x.uri);
    const facts = await signatureFacts(x, symbols, async () => []);
    assert.deepStrictEqual(
      facts.map((f) => [f.kind, (f as { id: string }).id, f.depends[0]]),
      [
        ['signature', 'cc', 'cc'],
        ['keywords', 'cc', 'cc']
      ]
    );
  });

  it('splits the declared names into words, once each, and never takes a name the block only uses', async () => {
    const x = projected('x.js', X_JS);
    const [parse] = declaredSymbols([sym('parseHTTPRequest2', 12, 2, 9, '', ['max_retry', 'onClose', 'close'])], x.uri);
    assert.deepStrictEqual(declaredWords(parse), ['parse', 'http', 'request', '2', 'max', 'retry', 'on', 'close']);
    assert.deepStrictEqual(wordsOf('XMLHttpRequest'), ['xml', 'http', 'request']);
    assert.deepStrictEqual(wordsOf('v2Beta'), ['v', '2', 'beta']);
    const facts = await signatureFacts(x, declaredSymbols([sym('alpha', 11, 2, 9)], x.uri), async () => []);
    const words = facts.filter((f): f is Extract<Fact, { kind: 'keywords' }> => f.kind === 'keywords').flatMap((f) => f.words);
    assert.deepStrictEqual(words, ['alpha'], 'alpha calls fetchAll; the words of a name it uses were taken');
  });

  it('splits a name in any script, keeping an accented letter in its word', () => {
    const cases: Array<[string, string[]]> = [
      ['caf\u00e9Menu', ['caf\u00e9', 'menu']],
      ['\u8bfb\u53d6\u6587\u4ef6', ['\u8bfb\u53d6\u6587\u4ef6']],
      ['cafe\u0301', ['caf\u00e9']],
      ['\u00c9t\u00e9Actif', ['\u00e9t\u00e9', 'actif']]
    ];
    for (const [name, words] of cases) {
      assert.deepStrictEqual(wordsOf(name), words, JSON.stringify(name));
    }
  });

  it('cuts a name the same whether an accent is written apart or composed, and counts only decimal digits', () => {
    /*
     * The expected words come from a separate Python function over
     * unicodedata categories, not from the code under test.
     */
    const cases: Array<[string, string[]]> = [
      ['A\u0301B', ['\u00e1b']],
      ['AB\u0301c', ['a', 'b\u0301c']],
      ['1\u03012', ['1\u03012']],
      ['1\u0301a', ['1\u0301', 'a']],
      ['a\u00bdb', ['a', 'b']],
      ['a1b2', ['a', '1', 'b', '2']]
    ];
    for (const [name, words] of cases) {
      assert.deepStrictEqual(wordsOf(name), words, JSON.stringify(name));
    }
  });

  it('keeps a call into a block of the projection, drops one into anything else, and each edge once', async () => {
    const x = projected('x.js', X_JS);
    const y = projected('y.js', Y_JS);
    const symbols = declaredSymbols([sym('alpha', 11, 2, 9), sym('beta', 11, 4, 9)], x.uri);
    const prepared: number[] = [];
    const prepare = async (at: PositionLike): Promise<unknown> => {
      prepared.push(at.line);
      return at.line === 2 ? [{ name: 'alpha' }] : undefined;
    };
    const target = (uri: string, line: number, character: number) => ({
      to: { uri: { toString: () => uri }, selectionRange: range(line, character, line, character + 1) }
    });
    const outgoing = async (): Promise<unknown> => [
      target(y.uri, 4, 9),
      target(x.uri, 4, 9),
      target('file:///lib/lib.d.ts', 6, 0),
      target(y.uri, 4, 0),
      target(y.uri, 1, 3)
    ];
    const facts = await callFacts(x, symbols, [x, y], prepare, outgoing);
    assert.deepStrictEqual(facts, [
      { kind: 'calls', from: 'aa', to: 'ee', depends: ['aa', 'bb', 'cc', 'ee', 'dd'] },
      { kind: 'calls', from: 'aa', to: 'bb', depends: ['aa', 'bb', 'cc'] }
    ]);
    assert.deepStrictEqual(prepared, [2, 4]);
  });

  it('takes the calls of every function a block declares, not only of its first', async () => {
    const z = projected('z.js', Z_JS);
    const y = projected('y.js', Y_JS);
    const symbols = declaredSymbols([sym('first', 11, 2, 9), sym('second', 11, 2, 29)], z.uri);
    const prepare = async (at: PositionLike): Promise<unknown> => (at.character === 29 ? [{ name: 'second' }] : []);
    const outgoing = async (): Promise<unknown> => [{ to: { uri: { toString: () => y.uri }, selectionRange: range(4, 9, 4, 10) } }];
    const facts = await callFacts(z, symbols, [z, y], prepare, outgoing);
    assert.deepStrictEqual(facts, [{ kind: 'calls', from: 'aa', to: 'ee', depends: ['aa', 'ee', 'dd'] }]);
  });

  it('records no call from a block to itself', async () => {
    const z = projected('z.js', Z_JS);
    const symbols = declaredSymbols([sym('first', 11, 2, 9), sym('second', 11, 2, 29)], z.uri);
    const outgoing = async (): Promise<unknown> => [{ to: { uri: { toString: () => z.uri }, selectionRange: range(2, 29, 2, 30) } }];
    const facts = await callFacts(z, symbols, [z], async () => [{ name: 'first' }], outgoing);
    assert.deepStrictEqual(facts, []);
  });

  it('gives a language with no call hierarchy no calls and no error', async () => {
    const x = projected('x.js', X_JS);
    let asked = 0;
    const facts = await callFacts(
      x,
      declaredSymbols([sym('alpha', 11, 2, 9)], x.uri),
      [x],
      async () => undefined,
      async () => {
        asked += 1;
        return [];
      }
    );
    assert.deepStrictEqual(facts, []);
    assert.strictEqual(asked, 0);
  });

  it('maps the four severities, counts the range in bytes of the file, and places it by its start', () => {
    const y = projected('y.js', Y_JS);
    const facts = diagnosticFacts(y, [
      { severity: 0, message: 'bad is not defined', range: range(2, 14, 2, 17) },
      { severity: 1, message: 'spans two blocks', range: range(2, 0, 4, 8) },
      { severity: 2, message: 'info', range: range(4, 9, 4, 10) },
      { severity: 3, message: 'hint', range: range(4, 0, 4, 1) },
      { severity: 0, message: 'on a marker line', range: range(1, 3, 1, 5) },
      { severity: 9, message: 'no such severity', range: range(4, 0, 4, 1) }
    ]);
    assert.deepStrictEqual(facts, [
      { kind: 'diagnostic', id: 'dd', severity: 'error', message: 'bad is not defined', start: 151, end: 154, depends: ['dd', 'ee'] },
      { kind: 'diagnostic', id: 'dd', severity: 'warning', message: 'spans two blocks', start: 135, end: 185, depends: ['dd', 'ee'] },
      { kind: 'diagnostic', id: 'ee', severity: 'information', message: 'info', start: 186, end: 187, depends: ['ee', 'dd'] },
      { kind: 'diagnostic', id: 'ee', severity: 'hint', message: 'hint', start: 177, end: 178, depends: ['ee', 'dd'] }
    ]);
  });

  it('counts the start of a file with a byte-order mark after the mark, and drops a range that ends before it starts', () => {
    /*
     * [0,7) the mark and `#!x\n`, the first block's prefix | [7,123) the
     * header | [123,142) aa's marker | [142,149) `run();\n`. The editor's
     * first character is `#`, byte 3.
     */
    const bytes = Buffer.concat([
      Buffer.from([0xef, 0xbb, 0xbf]),
      bytesOf('#!x\n', `// @file ${header('f.bm', 0)}\n`, '// @block aa pad 0\n', 'run();\n')
    ]);
    const file = projected('m.js', bytes);
    const facts = diagnosticFacts(file, [
      { severity: 0, message: 'at the start', range: range(0, 0, 0, 1) },
      { severity: 0, message: 'reversed', range: range(3, 3, 3, 1) }
    ]);
    assert.deepStrictEqual(facts, [
      { kind: 'diagnostic', id: 'aa', severity: 'error', message: 'at the start', start: 3, end: 4, depends: ['aa'] }
    ]);
  });

  it('gives nothing, and no error, for answers that are not lists and entries that are not shaped', async () => {
    const x = projected('x.js', X_JS);
    const y = projected('y.js', Y_JS);
    assert.deepStrictEqual(declaredSymbols(undefined, x.uri), []);
    assert.deepStrictEqual(declaredSymbols([null, { name: 'n' }, { name: 'r', kind: 11, selectionRange: { start: { line: 'a' } } }], x.uri), []);
    assert.deepStrictEqual(
      declaredSymbols(
        [
          { name: 'n', kind: 11, location: null },
          { name: 'm', kind: 11, location: { uri: null, range: range(2, 9, 2, 10) } }
        ],
        x.uri
      ),
      []
    );
    const symbols = declaredSymbols([sym('alpha', 11, 2, 9)], x.uri);
    const notAList = await callFacts(x, symbols, [x, y], async () => [{ name: 'alpha' }], async () => 'nope');
    const badTargets = await callFacts(x, symbols, [x, y], async () => [{ name: 'alpha' }], async () => [null, { to: null }, { to: { uri: { toString: () => y.uri } } }]);
    assert.deepStrictEqual([notAList, badTargets], [[], []]);
    assert.deepStrictEqual(diagnosticFacts(y, 'nope'), []);
    assert.deepStrictEqual(diagnosticFacts(y, [undefined, 'text', 7]), []);
    assert.deepStrictEqual(
      diagnosticFacts(y, [
        null,
        { severity: 0, message: 'no range' },
        { severity: 0, message: 'bad end', range: { start: { line: 2, character: 0 }, end: { line: 'x' } } }
      ]),
      []
    );
  });

  it('reads the flat form of a symbol answer, a hover given as plain strings, and a kind it has no name for', async () => {
    const x = projected('x.js', X_JS);
    const here = { toString: () => x.uri };
    const elsewhere = { toString: () => 'file:///p/other.js' };
    const flat = [
      { name: 'alpha', kind: 11, containerName: '', location: { uri: here, range: range(2, 9, 2, 14) } },
      { name: 'inner', kind: 12, containerName: 'alpha', location: { uri: here, range: range(2, 20, 2, 25) } },
      { name: 'beta', kind: 99, containerName: '', location: { uri: here, range: range(4, 9, 4, 13) } },
      { name: 'far', kind: 11, containerName: '', location: { uri: elsewhere, range: range(4, 9, 4, 12) } }
    ];
    const hover = async (at: PositionLike): Promise<unknown> => (at.line === 2 ? [{ contents: 'alpha(n)' }] : [{ contents: ['', 'beta()'] }]);
    const facts = await signatureFacts(x, declaredSymbols(flat, x.uri), hover);
    assert.deepStrictEqual(facts, [
      { kind: 'signature', id: 'aa', text: 'alpha(n)', symbolKind: 'function', depends: ['aa', 'bb', 'cc'] },
      { kind: 'keywords', id: 'aa', words: ['alpha', 'inner'], depends: ['aa', 'bb', 'cc'] },
      { kind: 'signature', id: 'bb', text: 'beta()', symbolKind: 'unknown', depends: ['bb', 'aa', 'cc'] },
      { kind: 'keywords', id: 'bb', words: ['beta'], depends: ['bb', 'aa', 'cc'] }
    ]);
  });
});

const DIGEST_A = 'a'.repeat(64);
const DIGEST_B = 'b'.repeat(64);

describe('the supply file', () => {
  before(async () => {
    await initWire();
  });

  it('writes the header and one line per fact, as the core reads them', () => {
    const text = supplyFileText(
      'signatures',
      '-',
      'javascript',
      '1.138.0',
      [
        { path: 'x.js', digest: DIGEST_A },
        { path: 'z.py', digest: DIGEST_B }
      ],
      ['x.js'],
      [
        { kind: 'signature', id: 'aa', text: 'say "hi"', symbolKind: 'function', depends: ['aa', 'bb'] },
        { kind: 'keywords', id: 'aa', words: ['say', 'hi'], depends: ['aa', 'bb'] }
      ]
    );
    assert.deepStrictEqual(text.split('\n'), [
      `(supply signatures (writer "-") (language "javascript") (source (vscode "1.138.0")) (files (("x.js" "${DIGEST_A}") ("z.py" "${DIGEST_B}"))) (replaces ("x.js")))`,
      '(signature "aa" "say \\"hi\\"" (kind function) (depends ("aa" "bb")))',
      '(keywords "aa" ("say" "hi") (depends ("aa" "bb")))',
      ''
    ]);
    const calls = supplyFileText('calls', '-', 'javascript', '1.138.0', [{ path: 'x.js', digest: DIGEST_A }], ['x.js'], [
      { kind: 'calls', from: 'aa', to: 'ee', depends: ['aa', 'ee'] }
    ]);
    assert.strictEqual(calls.split('\n')[1], '(calls "aa" "ee" (depends ("aa" "ee")))');
    const diagnostics = supplyFileText('diagnostics', 'w1', 'javascript', '1.138.0', [{ path: 'y.js', digest: DIGEST_A }], ['y.js'], [
      { kind: 'diagnostic', id: 'dd', severity: 'error', message: 'bad', start: 151, end: 154, depends: ['dd', 'ee'] }
    ]);
    assert.deepStrictEqual(diagnostics.split('\n').slice(0, 2), [
      `(supply diagnostics (writer "w1") (language "javascript") (source (vscode "1.138.0")) (files (("y.js" "${DIGEST_A}"))) (replaces ("y.js")))`,
      '(diagnostic "dd" error "bad" (range 151 154) (depends ("dd" "ee")))'
    ]);
  });

  it('writes a range as exact integers, which read back as integers and not as floats', () => {
    const text = supplyFileText('diagnostics', 'w1', 'javascript', '1.138.0', [{ path: 'y.js', digest: DIGEST_A }], ['y.js'], [
      { kind: 'diagnostic', id: 'dd', severity: 'error', message: 'bad', start: 151, end: 154, depends: ['dd'] }
    ]);
    const [, line] = parseAnswers(text) as unknown[][];
    const r = line[4] as unknown[];
    assert.deepStrictEqual([r[1], r[2]], [BigInt(151), BigInt(154)], shown(r));
  });

  it('escapes a line feed inside a message, so each fact stays on its own line', () => {
    const message = 'line one\nline two\r\nline three';
    const text = supplyFileText('diagnostics', 'w1', 'javascript', '1.138.0', [{ path: 'y.js', digest: DIGEST_A }], ['y.js'], [
      { kind: 'diagnostic', id: 'dd', severity: 'error', message, start: 1, end: 2, depends: ['dd'] }
    ]);
    const lines = text.split('\n');
    assert.strictEqual(lines.length, 3, shown(lines));
    assert.strictEqual(lines[1], '(diagnostic "dd" error "line one\\nline two\\r\\nline three" (range 1 2) (depends ("dd")))');
    const [, fact] = parseAnswers(text) as unknown[][];
    assert.strictEqual(fact[3], message);
  });

  it('writes an empty result as the header alone, still replacing its files', () => {
    const text = supplyFileText('calls', '-', 'python', '1.138.0', [{ path: 'z.py', digest: DIGEST_B }], ['z.py'], []);
    assert.deepStrictEqual(text.split('\n'), [
      `(supply calls (writer "-") (language "python") (source (vscode "1.138.0")) (files (("z.py" "${DIGEST_B}"))) (replaces ("z.py")))`,
      ''
    ]);
  });
});

/*
 * ONE ANSWER TO A `supply`, through a client and the stand-in core.
 */
async function answered(stdout: string, rc: number): Promise<Answer> {
  const core = new FakeCore([{ match: ['supply'], stdout, rc }]);
  try {
    return await new Client(new CliTransport(core.config(), core.env())).request('supply', ['signatures', '/k/s.sexp']);
  } finally {
    core.dispose();
  }
}

function refusalOf(answer: Answer) {
  const read = readSupplyAnswer(answer);
  assert.ok(!read.ok, `the answer read as a success: ${shown(read)}`);
  return read.refusal;
}

const NONE_LEFT = { unread: [], incomplete: false, notRemoved: [] };

describe('the answers and refusals', () => {
  before(async () => {
    await initWire();
  });

  it('reads the counts of an accepted supply and says them', async () => {
    const read = readSupplyAnswer(await answered('(ok (supplied (facts 3) (files 2)))\n', 0));
    assert.deepStrictEqual(read, { ok: true, facts: 3, files: 2 });
    assert.strictEqual(
      supplyNotice('calls', { results: [{ language: 'javascript', done: 'supplied', facts: 3, files: 2 }], ...NONE_LEFT }),
      'supplied 3 call facts for 2 javascript files'
    );
    assert.strictEqual(
      supplyNotice('signatures', { results: [{ language: 'python', done: 'supplied', facts: 1, files: 1 }], ...NONE_LEFT }),
      'supplied 1 signature and keyword fact for 1 python file'
    );
  });

  it('says a stale supply by the file that changed, and asks for the command again', async () => {
    const refusal = refusalOf(await answered('(error supply-stale (file "x.js"))\n', 1));
    assert.strictEqual(supplyRefusalNotice(refusal, null), 'x.js changed after it was projected; run the command again');
  });

  it('says a malformed supply is this extension\'s, with the line, the reason and where the file is kept', async () => {
    const refusal = refusalOf(await answered('(error supply-malformed (line 4) (reason fact-shape))\n', 1));
    assert.strictEqual(
      supplyRefusalNotice(refusal, '/k/s.sexp'),
      'this extension wrote a supply the core refused (line 4: fact-shape); the file is kept at /k/s.sexp'
    );
  });

  it('shows the other refusals by name, as the core wrote them', async () => {
    const reduction = '(error incomplete-reduction (notes (unreadable (writer "w2") (path "/s/w2") (reason torn))))';
    const refusal = refusalOf(await answered(`${reduction}\n`, 1));
    assert.strictEqual(supplyRefusalNotice(refusal, null), `the core refused the supply: ${reduction}`);
    const invalid = refusalOf(await answered('(error projection-invalid (reason header-limit))\n', 1));
    assert.strictEqual(supplyRefusalNotice(invalid, null), 'the core refused the supply: (error projection-invalid (reason header-limit))');
  });

  it('reads the counts by name, and refuses a shape it does not know as unreadable rather than as success', async () => {
    assert.deepStrictEqual(readSupplyAnswer(await answered('(ok (supplied (files 2) (facts 3)))\n', 0)), { ok: true, facts: 3, files: 2 });
    /*
     * A count that is not an integer never reaches the answer's reader: the
     * client's reader refuses the number itself, as unreadable.
     */
    await assert.rejects(
      answered('(ok (supplied (facts 3.5) (files 2)))\n', 0),
      (e: unknown) => e instanceof TransportError && e.failure === 'unreadable'
    );
    for (const text of ['(ok (supplied (facts 3)))', '(ok (written 3))', '(ok)']) {
      const answer = await answered(`${text}\n`, 0);
      assert.throws(
        () => readSupplyAnswer(answer),
        (e: unknown) => e instanceof TransportError && e.failure === 'unreadable',
        `${text} was read`
      );
    }
  });
});

/*
 * THE COMMAND'S STEPS, with a stand-in editor and a stand-in core.
 */
class ExportingClient extends Client {
  public constructor(transport: CliTransport, private readonly plant: (directory: string) => void) {
    super(transport);
  }

  public async request(verb: string, args: string[] = [], input?: string): Promise<Answer> {
    if (verb === 'export-code') {
      this.plant(args[0]);
    }
    return super.request(verb, args, input);
  }
}

class FakeDocument implements OpenedDocument {
  public ver = 1;
  public dirty = false;

  public constructor(public readonly uri: string, public readonly languageId: string, public body: string) {}

  public version(): number {
    return this.ver;
  }

  public text(): string {
    return this.body;
  }

  public isDirty(): boolean {
    return this.dirty;
  }
}

interface SupplyRig {
  core: FakeCore;
  root: string;
  directory: string;
  deps: SupplyDeps;
  documents: FakeDocument[];
  written: Array<{ path: string; text: string }>;
  removed: string[];
  clock: { t: number };
}

interface RigAnswers {
  exported?: string;
  exportRc?: number;
  supplied?: string;
  supplyRc?: number;
  /*
   * The stand-in reads the first supply file when the `supply` request
   * arrives, and logs what it held.
   */
  watchFirstSupply?: boolean;
}

function languageOf(file: string): string {
  return file.endsWith('.py') ? 'python' : file.endsWith('.js') ? 'javascript' : 'plaintext';
}

function supplyRig(projection: Record<string, Buffer>, answers: RigAnswers = {}): SupplyRig {
  const core = new FakeCore([
    { match: ['export-code'], stdout: answers.exported ?? `(ok (files ${Object.keys(projection).length}))\n`, rc: answers.exportRc ?? 0 },
    { match: ['supply'], stdout: answers.supplied ?? '(ok (supplied (facts 1) (files 1)))\n', rc: answers.supplyRc ?? 0 }
  ]);
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-supply-'));
  const directory = path.join(root, 'projection');
  const env = answers.watchFirstSupply === true ? core.env(path.join(root, 'supply-0.sexp')) : core.env();
  const client = new ExportingClient(new CliTransport(core.config(), env), (d) => {
    for (const [file, bytes] of Object.entries(projection)) {
      const target = path.join(d, ...file.split('/'));
      fs.mkdirSync(path.dirname(target), { recursive: true });
      fs.writeFileSync(target, bytes);
    }
  });
  const documents: FakeDocument[] = [];
  const written: Array<{ path: string; text: string }> = [];
  const removed: string[] = [];
  const clock = { t: 0 };
  const deps: SupplyDeps = {
    client,
    editorVersion: '1.138.0',
    directory,
    emptyDirectory,
    filesUnder,
    readFile: (p) => fs.readFileSync(p),
    fsPathOf: (d, relative) => path.join(d, ...relative.split('/')),
    open: async (fsPath) => {
      const saved = savedText(fs.readFileSync(fsPath));
      const document = new FakeDocument(`file://${fsPath}`, languageOf(fsPath), saved === null ? '' : saved.text);
      documents.push(document);
      return document;
    },
    symbols: async () => [],
    hover: async () => [],
    prepareCalls: async () => undefined,
    outgoingCalls: async () => [],
    settling: {
      changes: () => 0,
      now: () => clock.t,
      sleep: async (ms) => {
        clock.t += ms;
      }
    },
    diagnostics: () => [],
    stillCurrent: () => true,
    supplyPath: () => path.join(root, `supply-${written.length + removed.length}.sexp`),
    writeSupply: (p, text) => {
      fs.writeFileSync(p, text);
      written.push({ path: p, text });
    },
    removeSupply: (p) => {
      removed.push(p);
      fs.rmSync(p, { force: true });
    }
  };
  return { core, root, directory, deps, documents, written, removed, clock };
}

function sent(core: FakeCore, verb: string): string[][] {
  return core.requests().filter((r) => r[0] === verb);
}

function ran(outcome: SupplyOutcome) {
  assert.ok(outcome.done === 'ran', shown(outcome));
  return outcome;
}

const sha = (bytes: Buffer): string => createHash('sha256').update(bytes).digest('hex');

describe('running the command', () => {
  let r: SupplyRig;
  before(async () => {
    await initWire();
  });
  afterEach(() => {
    r?.core.dispose();
    if (r !== undefined) {
      fs.rmSync(r.root, { recursive: true, force: true });
    }
  });

  it('control: nothing changes, and one supply is sent for the one language', async () => {
    r = supplyRig({ 'x.js': X_JS });
    const outcome = ran(await runSupply('signatures', null, r.deps));
    assert.strictEqual(sent(r.core, 'supply').length, 1);
    assert.deepStrictEqual(outcome.results, [{ language: 'javascript', done: 'supplied', facts: 1, files: 1 }]);
  });

  it('discards the whole supply when a document\'s version moves while the providers run, and names the file', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    r.deps.symbols = async (uri) => {
      for (const d of r.documents.filter((d) => d.uri === uri && uri.endsWith('x.js'))) {
        d.ver += 1;
      }
      return [];
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'x.js changed while the facts were being collected; nothing was sent. Run the command again'
    });
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
    assert.deepStrictEqual(r.written, []);
  });

  it('discards the whole supply when a file\'s bytes change on disk while the providers run, and names the file', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    r.deps.symbols = async (uri) => {
      if (uri.endsWith('z.py')) {
        fs.appendFileSync(path.join(r.directory, 'z.py'), '# edited\n');
      }
      return [];
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'z.py changed while the facts were being collected; nothing was sent. Run the command again'
    });
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
  });

  it('sends nothing when the store changed while the providers ran', async () => {
    r = supplyRig({ 'x.js': X_JS });
    let switched = false;
    r.deps.symbols = async () => {
      switched = true;
      return [];
    };
    r.deps.stillCurrent = () => !switched;
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'the store changed while the facts were being collected; nothing was sent. Run the command again'
    });
    assert.strictEqual(sent(r.core, 'export-code').length, 1);
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
    assert.deepStrictEqual(r.written, []);
  });

  it('takes the diagnostics once they have been quiet for the quiet time', async () => {
    assert.deepStrictEqual([DIAGNOSTICS_QUIET_MS, DIAGNOSTICS_CAP_MS], [1000, 10000]);
    r = supplyRig({ 'y.js': Y_JS });
    const clock = r.clock;
    r.deps.settling.changes = () => Math.min(Math.floor(clock.t / 500), 6);
    const asked: number[] = [];
    r.deps.diagnostics = () => {
      asked.push(clock.t);
      return [];
    };
    const outcome = ran(await runSupply('diagnostics', 'w1', r.deps));
    assert.deepStrictEqual(asked, [4000], 'the last change was at 3 s; they are taken 1 s later');
    assert.strictEqual(outcome.incomplete, false);
  });

  it('takes the diagnostics at the cap when they never settle, and says the analysis may be incomplete', async () => {
    r = supplyRig({ 'y.js': Y_JS });
    const clock = r.clock;
    r.deps.settling.changes = () => Math.floor(clock.t / 500);
    const asked: number[] = [];
    r.deps.diagnostics = () => {
      asked.push(clock.t);
      return [];
    };
    const outcome = ran(await runSupply('diagnostics', 'w1', r.deps));
    assert.deepStrictEqual(asked, [10000]);
    assert.strictEqual(outcome.incomplete, true);
    assert.ok(
      supplyNotice('diagnostics', outcome).includes('the analysis may be incomplete: the diagnostics were still changing after 10 s'),
      supplyNotice('diagnostics', outcome)
    );
  });

  it('does not wait for diagnostics when it supplies signatures', async () => {
    r = supplyRig({ 'x.js': X_JS });
    const clock = r.clock;
    r.deps.settling.changes = () => Math.floor(clock.t / 500);
    ran(await runSupply('signatures', null, r.deps));
    assert.strictEqual(clock.t, 0);
  });

  it('writes the supply file, sends it, and removes it after the answer', async () => {
    r = supplyRig({ 'x.js': X_JS }, { watchFirstSupply: true });
    const outcome = ran(await runSupply('signatures', null, r.deps));
    assert.strictEqual(r.written.length, 1);
    const call = r.core.calls().find((c) => c.coreArgv[0] === 'supply');
    assert.strictEqual(call?.watched, r.written[0].text, 'the supply file was not on disk, whole, when the request arrived');
    const supplies = sent(r.core, 'supply');
    assert.deepStrictEqual(supplies[0].slice(0, 3), ['supply', 'signatures', r.written[0].path]);
    assert.deepStrictEqual(r.removed, [r.written[0].path]);
    assert.strictEqual(fs.existsSync(r.written[0].path), false);
    assert.deepStrictEqual(outcome.notRemoved, []);
  });

  it('keeps a supply file the core found malformed, and says where it is', async () => {
    r = supplyRig({ 'x.js': X_JS }, { supplied: '(error supply-malformed (line 3) (reason fact-shape))\n', supplyRc: 1 });
    const outcome = ran(await runSupply('signatures', null, r.deps));
    const kept = r.written[0].path;
    assert.deepStrictEqual(r.removed, []);
    assert.ok(fs.existsSync(kept), 'the malformed supply file was removed');
    const [result] = outcome.results;
    assert.ok(result.done === 'refused', shown(result));
    assert.strictEqual(result.notice, `this extension wrote a supply the core refused (line 3: fact-shape); the file is kept at ${kept}`);
  });

  it('says a supply file it could not remove', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.removeSupply = () => {
      throw new Error('EACCES: permission denied');
    };
    const outcome = ran(await runSupply('signatures', null, r.deps));
    assert.deepStrictEqual(outcome.notRemoved, [`${r.written[0].path} (EACCES: permission denied)`]);
    assert.ok(supplyNotice('signatures', outcome).includes(`the supply file could not be removed: ${r.written[0].path}`));
  });

  it('removes a supply file whose write failed after creating it, and sends nothing', async () => {
    r = supplyRig({ 'x.js': X_JS });
    const made: string[] = [];
    r.deps.writeSupply = (p) => {
      fs.writeFileSync(p, '(supply sig');
      made.push(p);
      throw new Error('the disk filled up');
    };
    await assert.rejects(runSupply('signatures', null, r.deps), /the disk filled up/);
    assert.deepStrictEqual(r.removed, made);
    assert.strictEqual(fs.existsSync(made[0]), false);
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
  });

  it('discards the supply when a document turns dirty while the providers run', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.symbols = async () => {
      r.documents[0].dirty = true;
      return [];
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'x.js changed while the facts were being collected; nothing was sent. Run the command again'
    });
  });

  it('discards the supply when a document\'s text changes under the same version', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.symbols = async () => {
      r.documents[0].body += ' ';
      return [];
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'x.js changed while the facts were being collected; nothing was sent. Run the command again'
    });
  });

  it('discards the supply when a file it could not read changes on disk, since it is listed', async () => {
    r = supplyRig({ 'x.js': X_JS, 'notes.txt': NOTES });
    r.deps.symbols = async () => {
      fs.appendFileSync(path.join(r.directory, 'notes.txt'), 'more\n');
      return [];
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'notes.txt changed while the facts were being collected; nothing was sent. Run the command again'
    });
  });

  it('asks again before the second language\'s supply, and says what the first one sent', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    const remove = r.deps.removeSupply;
    r.deps.removeSupply = (p) => {
      remove(p);
      for (const d of r.documents.filter((d) => d.uri.endsWith('z.py'))) {
        d.ver += 1;
      }
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'z.py changed while the facts were being collected; sent before that: javascript (1 fact). Run the command again'
    });
    assert.strictEqual(sent(r.core, 'supply').length, 1);
  });

  it('stops before the second language\'s supply when the store changed, and says what the first one sent', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    let answered = 0;
    const remove = r.deps.removeSupply;
    r.deps.removeSupply = (p) => {
      remove(p);
      answered += 1;
    };
    r.deps.stillCurrent = () => answered === 0;
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'the store changed while the facts were being collected; sent before that: javascript (1 fact). Run the command again'
    });
    assert.strictEqual(sent(r.core, 'supply').length, 1);
  });

  it('says a supply file it could not remove when the write had failed too', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.writeSupply = (p) => {
      fs.writeFileSync(p, '(supply sig');
      throw new Error('the disk filled up');
    };
    r.deps.removeSupply = () => {
      throw new Error('EACCES: permission denied');
    };
    await assert.rejects(
      runSupply('signatures', null, r.deps),
      (e: unknown) =>
        e instanceof Error &&
        e.message ===
          `the disk filled up; nothing was sent; the supply file could not be removed: ${path.join(r.root, 'supply-0.sexp')} (EACCES: permission denied)`
    );
  });

  it('says a refused first supply, and where its file is kept, when it stops before the second', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY }, { supplied: '(error supply-malformed (line 3) (reason fact-shape))\n', supplyRc: 1 });
    let asked = 0;
    r.deps.stillCurrent = () => {
      asked += 1;
      if (asked === 2) {
        for (const d of r.documents.filter((d) => d.uri.endsWith('z.py'))) {
          d.ver += 1;
        }
      }
      return true;
    };
    const outcome = await runSupply('signatures', null, r.deps);
    const kept = r.written[0].path;
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why:
        'z.py changed while the facts were being collected; sent before that: javascript (this extension wrote a supply the core ' +
        `refused (line 3: fact-shape); the file is kept at ${kept}). Run the command again`
    });
  });

  it('says a supply file it could not remove when it stops before the second language', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    r.deps.removeSupply = () => {
      throw new Error('EACCES: permission denied');
    };
    let asked = 0;
    r.deps.stillCurrent = () => {
      asked += 1;
      return asked === 1;
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why:
        'the store changed while the facts were being collected; sent before that: javascript (1 fact); the supply file could not ' +
        `be removed: ${r.written[0].path} (EACCES: permission denied). Run the command again`
    });
  });

  it('carries what was sent in the error when a later language\'s supply fails', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    const write = r.deps.writeSupply;
    let writes = 0;
    r.deps.writeSupply = (p, text) => {
      writes += 1;
      if (writes === 2) {
        throw new Error('the disk filled up');
      }
      write(p, text);
    };
    await assert.rejects(
      runSupply('signatures', null, r.deps),
      (e: unknown) => e instanceof Error && e.message === 'the disk filled up; sent before that: javascript (1 fact)'
    );
  });

  it('turns a thrown value that is not an Error into one, carrying the file it could not remove', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.writeSupply = (p) => {
      fs.writeFileSync(p, '(supply sig');
      throw 'disk full';
    };
    r.deps.removeSupply = () => {
      throw new Error('EACCES: permission denied');
    };
    await assert.rejects(
      runSupply('signatures', null, r.deps),
      (e: unknown) =>
        e instanceof Error &&
        e.message === `disk full; nothing was sent; the supply file could not be removed: ${path.join(r.root, 'supply-0.sexp')} (EACCES: permission denied)`
    );
  });

  it('stops when the directory holds fewer files than the export says it wrote, and sends nothing', async () => {
    r = supplyRig({ 'x.js': X_JS }, { exported: '(ok (files 2))\n' });
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'the projection wrote 2 files and 1 are there; nothing was sent. Run the command again'
    });
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
  });

  it('takes a document whose mixed line ends the editor made one kind, and counts its bytes from the file', async () => {
    /*
     * alpha's line ends in CRLF and every other in LF, so every line start
     * after it is one byte later than in x.js: bb's marker at 170, beta's
     * line at 189, `beta` at 198.
     */
    const mixed = Buffer.from(X_JS.toString('utf8').replace('fetchAll(); }\n', 'fetchAll(); }\r\n'), 'utf8');
    r = supplyRig({ 'x.js': mixed });
    const open = r.deps.open;
    r.deps.open = async (fsPath) => {
      const document = (await open(fsPath)) as FakeDocument;
      document.body = document.body.replace(/\r\n/g, '\n');
      return document;
    };
    r.deps.diagnostics = () => [{ severity: 1, message: 'beta', range: range(4, 9, 4, 13) }];
    ran(await runSupply('diagnostics', 'w1', r.deps));
    assert.strictEqual(r.written[0].text.split('\n')[1], '(diagnostic "bb" warning "beta" (range 198 202) (depends ("bb" "aa" "cc")))');
  });

  it('lists a file that is not UTF-8 without opening it, and says an empty projection supplied nothing', async () => {
    r = supplyRig({ 'x.js': X_JS, 'bin.dat': Buffer.from([0xff, 0xfe, 0x00]) });
    const outcome = ran(await runSupply('signatures', null, r.deps));
    assert.deepStrictEqual(outcome.unread, ['bin.dat']);
    assert.ok(r.written[0].text.split('\n')[0].includes(`("bin.dat" "${sha(Buffer.from([0xff, 0xfe, 0x00]))}")`), r.written[0].text);
    assert.strictEqual(r.documents.length, 1);
    r.core.dispose();
    fs.rmSync(r.root, { recursive: true, force: true });
    r = supplyRig({});
    const empty = ran(await runSupply('signatures', null, r.deps));
    assert.strictEqual(supplyNotice('signatures', empty), 'the projection holds no file the editor could read, so nothing was supplied');
  });

  it('stops when the document the editor opens holds other lines than the file, and sends nothing', async () => {
    r = supplyRig({ 'x.js': X_JS });
    const open = r.deps.open;
    r.deps.open = async (fsPath) => {
      const document = (await open(fsPath)) as FakeDocument;
      document.body = `${document.body}// not in the file\n`;
      return document;
    };
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why: 'x.js changed while the facts were being collected; nothing was sent. Run the command again'
    });
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
  });

  it('says a supply whose answer it could not read as sent, since the core may have kept it', async () => {
    r = supplyRig({ 'x.js': X_JS }, { supplied: '(ok (written 1))\n' });
    r.deps.removeSupply = () => {
      throw new Error('EACCES: permission denied');
    };
    await assert.rejects(
      runSupply('signatures', null, r.deps),
      (e: unknown) =>
        e instanceof TransportError &&
        e.message ===
          'the core answered the supply with something this client cannot read; sent, with an answer this client could not read: ' +
            `javascript; the supply file could not be removed: ${path.join(r.root, 'supply-0.sexp')} (EACCES: permission denied)`
    );
  });

  it('names the files it could not read and a capped analysis when it stops after a supply', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY, 'notes.txt': NOTES });
    const clock = r.clock;
    r.deps.settling.changes = () => Math.floor(clock.t / 500);
    let asked = 0;
    r.deps.stillCurrent = () => {
      asked += 1;
      return asked === 1;
    };
    const outcome = await runSupply('diagnostics', 'w1', r.deps);
    assert.deepStrictEqual(outcome, {
      done: 'stopped',
      why:
        'the store changed while the facts were being collected; sent before that: javascript (1 fact); not read: notes.txt; ' +
        'the analysis may be incomplete: the diagnostics were still changing after 10 s. Run the command again'
    });
  });

  it('refuses a supply answer it cannot read, and removes the supply file', async () => {
    r = supplyRig({ 'x.js': X_JS }, { supplied: '(ok (written 1))\n' });
    await assert.rejects(runSupply('signatures', null, r.deps), (e: unknown) => e instanceof TransportError && e.failure === 'unreadable');
    assert.deepStrictEqual(r.removed, [r.written[0].path]);
    assert.strictEqual(fs.existsSync(r.written[0].path), false);
  });

  it('puts each language\'s facts in its own supply', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY });
    r.deps.symbols = async (uri) => (uri.endsWith('x.js') ? [sym('alpha', 11, 2, 9, 'alpha()')] : [sym('pi', 11, 2, 4, 'pi()')]);
    ran(await runSupply('signatures', null, r.deps));
    const [js, py] = r.written.map((w) => w.text.split('\n').slice(1, -1));
    assert.deepStrictEqual(js, [
      '(signature "aa" "alpha()" (kind function) (depends ("aa" "bb" "cc")))',
      '(keywords "aa" ("alpha") (depends ("aa" "bb" "cc")))'
    ]);
    assert.deepStrictEqual(py, ['(signature "pp" "pi()" (kind function) (depends ("pp")))', '(keywords "pp" ("pi") (depends ("pp")))']);
  });

  it('sends nothing and writes nothing when a provider fails', async () => {
    r = supplyRig({ 'x.js': X_JS });
    r.deps.symbols = async () => {
      throw new Error('the language server stopped');
    };
    await assert.rejects(runSupply('signatures', null, r.deps), /^Error: the language server stopped$/);
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
    assert.deepStrictEqual(r.written, []);
  });

  it('takes a lone carriage return the editor made a line feed, and counts its bytes from the file', async () => {
    /*
     * alpha's line holds a lone CR, a line break to the editor and not to
     * the core's line split: `beta` is the editor's line 5, column 9, and
     * byte 197 of the file, in bb's source [188,207).
     */
    const lone = Buffer.from(X_JS.toString('utf8').replace('function alpha(n) { fetchAll(); }', 'function alpha(n) {\rfetchAll(); }'), 'utf8');
    r = supplyRig({ 'x.js': lone });
    const open = r.deps.open;
    r.deps.open = async (fsPath) => {
      const document = (await open(fsPath)) as FakeDocument;
      document.body = document.body.replace(/\r/g, '\n');
      return document;
    };
    r.deps.diagnostics = () => [{ severity: 1, message: 'beta', range: range(5, 9, 5, 13) }];
    ran(await runSupply('diagnostics', 'w1', r.deps));
    assert.strictEqual(r.written[0].text.split('\n')[1], '(diagnostic "bb" warning "beta" (range 197 201) (depends ("bb" "aa" "cc")))');
  });

  it('does not say a supply was sent when the core could not be started for it', async () => {
    r = supplyRig({ 'x.js': X_JS });
    const inner = r.deps.client;
    r.deps.client = {
      request: (verb: string, args: string[] = []) =>
        verb === 'supply' ? Promise.reject(new TransportError('spawn-failed', 'could not start scheme')) : inner.request(verb, args)
    } as unknown as Client;
    await assert.rejects(
      runSupply('signatures', null, r.deps),
      (e: unknown) => e instanceof TransportError && e.failure === 'spawn-failed' && e.message === 'could not start scheme'
    );
    assert.deepStrictEqual(r.removed.length, 1);
  });

  it('passes the hover each file\'s own uri and each symbol\'s position, through the command', async () => {
    r = supplyRig({ 'x.js': X_JS, 'y.js': Y_JS });
    r.deps.symbols = async (uri) => (uri.endsWith('x.js') ? [sym('alpha', 11, 2, 9)] : [sym('e', 11, 4, 9)]);
    r.deps.hover = async (uri, at) => [{ contents: `${path.basename(uri)}:${at.line}:${at.character}` }];
    ran(await runSupply('signatures', null, r.deps));
    const lines = r.written[0].text.split('\n');
    assert.ok(lines.includes('(signature "aa" "x.js:2:9" (kind function) (depends ("aa" "bb" "cc")))'), shown(lines));
    assert.ok(lines.includes('(signature "ee" "y.js:4:9" (kind function) (depends ("ee" "dd")))'), shown(lines));
  });

  it('asks each file\'s call hierarchy with its own uri, through the command', async () => {
    r = supplyRig({ 'x.js': X_JS, 'y.js': Y_JS });
    const yUri = `file://${path.join(r.directory, 'y.js')}`;
    r.deps.symbols = async (uri) => (uri.endsWith('x.js') ? [sym('alpha', 11, 2, 9)] : [sym('e', 11, 4, 9)]);
    r.deps.prepareCalls = async (uri, at) => (uri.endsWith('x.js') && at.line === 2 ? [{ from: uri }] : undefined);
    r.deps.outgoingCalls = async (item) =>
      (item as { from: string }).from.endsWith('x.js') ? [{ to: { uri: { toString: () => yUri }, selectionRange: range(4, 9, 4, 10) } }] : [];
    ran(await runSupply('calls', null, r.deps));
    const lines = r.written[0].text.split('\n');
    assert.deepStrictEqual(lines.slice(1), ['(calls "aa" "ee" (depends ("aa" "bb" "cc" "ee" "dd")))', '']);
  });

  it('takes a path inside the projection by whole parts, a folder named `..helpers` included', () => {
    const d = path.join(os.tmpdir(), 'projection');
    const files = [path.join(d, 'a', 'b.js'), path.join(d, '..helpers', 'x.js'), path.join(d, '..', 'x.js'), path.join(os.tmpdir(), 'other', 'x.js'), d];
    assert.deepStrictEqual(
      files.map((f) => insideDirectory(d, f)),
      [true, true, false, false, false]
    );
  });

  it('empties the directory before projecting, so a file of an earlier projection is not listed', async () => {
    r = supplyRig({ 'x.js': X_JS });
    fs.mkdirSync(path.join(r.directory, 'sub'), { recursive: true });
    fs.writeFileSync(path.join(r.directory, 'old.js'), 'left over\n');
    fs.writeFileSync(path.join(r.directory, 'sub', 'old.py'), 'left over\n');
    ran(await runSupply('signatures', null, r.deps));
    const head = r.written[0].text.split('\n')[0];
    assert.ok(head.includes(`(files (("x.js" "${sha(X_JS)}")))`), head);
    assert.strictEqual(fs.existsSync(path.join(r.directory, 'old.js')), false);
    assert.strictEqual(fs.existsSync(path.join(r.directory, 'sub')), false);
  });

  it('sends one supply per language, each listing every file and replacing its own language\'s', async () => {
    r = supplyRig({ 'x.js': X_JS, 'z.py': Z_PY, 'notes.txt': NOTES });
    const outcome = ran(await runSupply('signatures', null, r.deps));
    const files = `(files (("notes.txt" "${sha(NOTES)}") ("x.js" "${sha(X_JS)}") ("z.py" "${sha(Z_PY)}")))`;
    assert.deepStrictEqual(
      r.written.map((w) => w.text.split('\n')[0]),
      [
        `(supply signatures (writer "-") (language "javascript") (source (vscode "1.138.0")) ${files} (replaces ("x.js")))`,
        `(supply signatures (writer "-") (language "python") (source (vscode "1.138.0")) ${files} (replaces ("z.py")))`
      ]
    );
    assert.strictEqual(sent(r.core, 'supply').length, 2);
    assert.deepStrictEqual(outcome.unread, ['notes.txt']);
    assert.deepStrictEqual(sent(r.core, 'export-code')[0].slice(0, 2), ['export-code', r.directory]);
  });

  it('supplies a writer\'s diagnostics from its working view, end to end', async () => {
    r = supplyRig({ 'y.js': Y_JS });
    r.deps.diagnostics = (uri) =>
      uri.endsWith('y.js') ? [{ severity: 0, message: 'bad is not defined', range: range(2, 14, 2, 17) }] : [];
    ran(await runSupply('diagnostics', 'w1', r.deps));
    assert.deepStrictEqual(sent(r.core, 'export-code')[0].slice(0, 5), ['export-code', r.directory, '--working', '--writer', 'w1']);
    assert.deepStrictEqual(sent(r.core, 'supply')[0].slice(0, 5), ['supply', 'diagnostics', r.written[0].path, '--for', 'w1']);
    assert.deepStrictEqual(r.written[0].text.split('\n'), [
      `(supply diagnostics (writer "w1") (language "javascript") (source (vscode "1.138.0")) (files (("y.js" "${sha(Y_JS)}"))) (replaces ("y.js")))`,
      '(diagnostic "dd" error "bad is not defined" (range 151 154) (depends ("dd" "ee")))',
      ''
    ]);
  });

  it('shows a refused projection by name, and sends no supply', async () => {
    r = supplyRig({ 'x.js': X_JS }, { exported: '(error projection-invalid (reason header-limit))\n', exportRc: 1 });
    const outcome = await runSupply('signatures', null, r.deps);
    assert.deepStrictEqual(
      [outcome.done, outcome.done === 'refused' ? outcome.notice : ''],
      ['refused', 'the core refused the projection: (error projection-invalid (reason header-limit))']
    );
    assert.deepStrictEqual(sent(r.core, 'supply'), []);
  });

  it('shows incomplete-reduction by name from the projection and from the supply', async () => {
    const reduction = '(error incomplete-reduction (notes (unreadable (writer "w2") (path "/s/w2") (reason torn))))';
    r = supplyRig({ 'x.js': X_JS }, { exported: `${reduction}\n`, exportRc: 1 });
    const projectedOutcome = await runSupply('signatures', null, r.deps);
    assert.ok(projectedOutcome.done === 'refused' && projectedOutcome.notice.includes('incomplete-reduction'), shown(projectedOutcome));
    r.core.dispose();
    fs.rmSync(r.root, { recursive: true, force: true });
    r = supplyRig({ 'x.js': X_JS }, { supplied: `${reduction}\n`, supplyRc: 1 });
    const supplied = ran(await runSupply('calls', null, r.deps));
    const [result] = supplied.results;
    assert.ok(result.done === 'refused', shown(result));
    assert.strictEqual(result.notice, `the core refused the supply: ${reduction}`);
  });
});

/*
 * ON A REAL CORE: the supply this extension writes is the one the core
 * takes; a reader sees the fact with the editor named; a change to a block
 * the fact rests on makes it stale; an empty supply clears it; and a
 * diagnostic placed by this extension's marker reader is the block the
 * core maps it to.
 *
 * The editor is a stand-in that declares the two functions where the
 * projected file has them; everything else -- the export, the files, the
 * supply, the reads -- is the core's.
 */
describe('on a real core the supply is taken, read back, made stale and cleared', function () {
  this.timeout(180000);
  let store: RealStore | undefined;
  afterEach(() => store?.dispose());

  const LIB = 'function alpha() {\n  return beta();\n}\n\nfunction beta() {\n  return 2;\n}\n';

  async function codeStore(): Promise<RealStore> {
    const made = await RealStore.make();
    const from = path.join(made.root, 'src');
    fs.mkdirSync(from, { recursive: true });
    fs.writeFileSync(path.join(from, 'lib.js'), LIB);
    const imported = made.cli(['import-code', from, '--store', made.store]);
    assert.ok(imported.startsWith('(ok'), imported);
    return made;
  }

  /*
   * THE STAND-IN EDITOR'S SYMBOLS: each named function where the text has
   * it, with a detail.
   */
  function declaring(text: string, names: string[]) {
    const lines = text.split('\n');
    return names.flatMap((name) => {
      const line = lines.findIndex((l) => l.includes(`function ${name}(`));
      return line < 0 ? [] : [sym(name, 11, line, lines[line].indexOf(name), `${name}(): number`)];
    });
  }

  function onDisk(real: RealStore, names: string[]): { deps: SupplyDeps; directory: string } {
    const directory = path.join(real.root, 'projection');
    const texts = new Map<string, string>();
    const clock = { t: 0 };
    let made = 0;
    const deps: SupplyDeps = {
      client: real.client,
      editorVersion: '1.138.0',
      directory,
      emptyDirectory,
      filesUnder,
      readFile: (p) => fs.readFileSync(p),
      fsPathOf: (d, relative) => path.join(d, ...relative.split('/')),
      open: async (fsPath) => {
        const saved = savedText(fs.readFileSync(fsPath));
        const document = new FakeDocument(`file://${fsPath}`, languageOf(fsPath), saved === null ? '' : saved.text);
        texts.set(document.uri, document.body);
        return document;
      },
      symbols: async (uri) => declaring(texts.get(uri) ?? '', names),
      hover: async () => [],
      prepareCalls: async () => undefined,
      outgoingCalls: async () => [],
      settling: {
        changes: () => 0,
        now: () => clock.t,
        sleep: async (ms) => {
          clock.t += ms;
        }
      },
      diagnostics: () => [],
      stillCurrent: () => true,
      supplyPath: () => {
        made += 1;
        return path.join(real.root, `supply-${made}.sexp`);
      },
      writeSupply: (p, text) => fs.writeFileSync(p, text),
      removeSupply: (p) => fs.rmSync(p, { force: true })
    };
    return { deps, directory };
  }

  /*
   * THE BLOCK HOLDING A STRING OF THE ONE PROJECTED FILE, by this
   * extension's marker reader.
   */
  function blockHolding(directory: string, needle: string): string | null {
    const [file] = filesUnder(directory);
    const bytes = fs.readFileSync(path.join(directory, ...file.split('/')));
    const reading = readProjection(bytes, file);
    assert.ok(reading.ok, shown(reading));
    return blockAt(reading.projection, bytes.indexOf(needle));
  }

  it('takes a signatures supply, reads it back with the editor named, and drops it when a block it rests on changes', async () => {
    store = await codeStore();
    const { deps, directory } = onDisk(store, ['alpha', 'beta']);
    const outcome = ran(await runSupply('signatures', null, deps));
    /*
     * `import-code` makes the whole file one block, named after its first
     * definition: one signature (alpha's, the block's first symbol) and one
     * keywords line (both names).
     */
    assert.deepStrictEqual(outcome.results, [{ language: 'javascript', done: 'supplied', facts: 2, files: 1 }], shown(outcome));
    const alpha = blockHolding(directory, 'function alpha');
    assert.ok(alpha !== null && blockHolding(directory, 'function beta') === alpha, `the file is not one block: ${alpha}`);
    /*
     * THE CORE SAYS WHICH BLOCK IS WHICH, not this extension's reader: the
     * ids above came from the reader, and a reader that swapped two blocks
     * would agree with itself.
     */
    /*
     * A text-mode block's src reads back as bytes, so the core's own name for
     * the block is what says which block it is: the file's first definition.
     */
    assert.ok(store.cli(['read', alpha, '--store', store.store]).includes('(name . "alpha")'), 'the block the reader named is not the file\'s');
    const fresh = store.cli(['read', alpha, '--signature', '--wire', '--store', store.store]);
    for (const part of ['(signature "alpha(): number")', '(stale 0)', '(via (vscode "1.138.0" "javascript"))']) {
      assert.ok(fresh.includes(part), `${part} is not in ${fresh}`);
    }
    store.cli(['set', alpha, 'src', LIB.replace('return 2;', 'return 3;'), '--store', store.store]);
    const stale = store.cli(['read', alpha, '--signature', '--wire', '--store', store.store]);
    for (const part of ['(signature absent)', '(stale 1)']) {
      assert.ok(stale.includes(part), `${part} is not in ${stale}`);
    }
  });

  it('clears a file\'s signatures with an empty supply that names it', async () => {
    store = await codeStore();
    const first = onDisk(store, ['alpha', 'beta']);
    const supplied = ran(await runSupply('signatures', null, first.deps));
    assert.deepStrictEqual(supplied.results, [{ language: 'javascript', done: 'supplied', facts: 2, files: 1 }], shown(supplied));
    const alpha = blockHolding(first.directory, 'function alpha');
    assert.ok(alpha !== null);
    const before = store.cli(['read', alpha, '--signature', '--wire', '--store', store.store]);
    assert.ok(before.includes('(signature "alpha(): number")'), `there was nothing to clear: ${before}`);
    const empty = onDisk(store, []);
    const outcome = ran(await runSupply('signatures', null, empty.deps));
    assert.deepStrictEqual(outcome.results, [{ language: 'javascript', done: 'supplied', facts: 0, files: 1 }], shown(outcome));
    const read = store.cli(['read', alpha, '--signature', '--wire', '--store', store.store]);
    assert.ok(read.includes('(signature absent)'), read);
  });

  it('places a diagnostic in the block the core maps its range to, at the offset within that block', async () => {
    store = await codeStore();
    const local = /\(local-writer "([^"]+)"\)/.exec(store.cli(['check', '--store', store.store]));
    assert.ok(local !== null, 'the core named no local writer');
    const writer = local[1];
    const { deps, directory } = onDisk(store, []);
    deps.diagnostics = (uri) => {
      const [file] = filesUnder(directory);
      if (!uri.endsWith(file)) {
        return [];
      }
      const lines = (savedText(fs.readFileSync(path.join(directory, ...file.split('/')))) ?? { text: '' }).text.split('\n');
      const line = lines.findIndex((l) => l.includes('return beta();'));
      const character = lines[line].indexOf('beta');
      return [{ severity: 0, message: 'beta is odd', range: range(line, character, line, character + 4) }];
    };
    const outcome = ran(await runSupply('diagnostics', writer, deps));
    assert.deepStrictEqual(outcome.results, [{ language: 'javascript', done: 'supplied', facts: 1, files: 1 }], shown(outcome));
    const alpha = blockHolding(directory, 'function alpha');
    assert.ok(alpha !== null);
    /*
     * `beta` in alpha's own text: `function alpha() {\n` is 19 bytes and
     * `  return ` 9 more, so it starts at 28 and ends at 32.
     */
    const listed = store.cli(['diagnostics', '--writer', writer, '--wire', '--store', store.store]);
    for (const part of [`"${alpha}"`, '"beta is odd"', '(at 28 32)']) {
      assert.ok(listed.includes(part), `${part} is not in ${listed}`);
    }
  });
});
