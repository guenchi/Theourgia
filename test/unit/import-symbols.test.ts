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
 * AN IMPORT SPLIT AT THE EDITOR'S SYMBOLS (src/import-symbols.ts), against a
 * scripted core whose answers are written as the core prints them: `(ok
 * (items ...) ...)` with the symbols clauses after the items, an export's
 * projection with its header and markers, and a recursive read of the file
 * block. Every byte offset below is worked out from the row's own text, and
 * a setup assertion pins it against that text.
 */

import * as assert from 'assert';
import { createHash } from 'crypto';
import { Client } from '../../src/client';
import { ImportDocument, ImportSource, NO_SYMBOLS, UNSAVED, importReport, importSymbolsText, positionAtByte, runImport } from '../../src/import-symbols';
import { CHANGED_WHILE_COLLECTING, byteOffset, fileSymbols, lineStarts, savedText } from '../../src/split-symbols';
import { RawResult } from '../../src/transport';
import { initWire } from '../../src/wire';

const TWO = 'function alpha() {\n  return 1;\n}\nfunction beta() {\n  return 2;\n}\n';
const CJK = "// 漢字\nfunction 名前() {\n  return '😀';\n}\n";

function digest(text: string): string {
  return createHash('sha256').update(Buffer.from(text, 'utf8')).digest('hex');
}

/*
 * A PROJECTION AS THE CORE'S EXPORT WRITES ONE: the header line, then a
 * marker line before each block's bytes.
 */
function projection(fileId: string, blocks: Array<[string, string]>): Uint8Array {
  const head = Buffer.from(`(code-projection 1 "s1" "${fileId}" (("w" . 0)) text 0)`, 'utf8').toString('hex');
  return Buffer.from(`// @file ${head}\n` + blocks.map(([id, src]) => `// @block ${id} pad 0\n${src}`).join(''), 'utf8');
}

/*
 * A BLOCK RECORD AS `read --recursive --wire` PRINTS IT, its src a string.
 */
function record(id: string, parent: string | null, fields: string): string {
  const position = parent === null ? '(position root . 0)' : `(position "${parent}" . 0)`;
  return `((id . "${id}") (deleted . #f) (fields ${fields}) ${position} (edges))`;
}

function quoted(text: string): string {
  return JSON.stringify(text);
}

interface Rig {
  sent: string[][];
  written: string[];
  removed: string[];
  scratch: string[];
  deps: Parameters<typeof runImport>[0];
}

/*
 * THE STAND-INS: a core answering each verb from `answers`, a disk holding
 * `files`, the editor's documents and their symbols.
 */
function rig(
  files: Record<string, string>,
  documents: Record<string, ImportDocument | null>,
  symbols: Record<string, unknown>,
  answers: Record<string, { stdout: string; rc?: number }>,
  exported: Record<string, Uint8Array> = {}
): Rig {
  const sent: string[][] = [];
  const written: string[] = [];
  const removed: string[] = [];
  const scratch: string[] = [];
  const client = new Client({
    kind: 'test',
    send: async (verb: string, args: string[]): Promise<RawResult> => {
      sent.push([verb, ...args]);
      const key = verb === 'read' ? `read ${args[0]}` : verb;
      const answer = answers[key];
      if (answer === undefined) {
        throw new Error(`the scripted core has no answer for ${key}`);
      }
      return { argv: [verb, ...args], rc: answer.rc ?? 0, stdout: answer.stdout, stderr: '' };
    }
  });
  const sources: ImportSource[] = Object.keys(files).map((rel) => ({ rel, bytesPath: `/src/${rel}`, uri: `file:///src/${rel}` }));
  return {
    sent,
    written,
    removed,
    scratch,
    deps: {
      client,
      editorVersion: '1.138.0',
      directory: '/src',
      sources,
      readFile: (fsPath) => Buffer.from(files[fsPath.slice('/src/'.length)], 'utf8'),
      document: (source) => Promise.resolve(documents[source.rel] ?? null),
      symbols: (source) => Promise.resolve(symbols[source.rel] ?? []),
      symbolsPath: () => '/tmp/symbols.sexp',
      writeSymbols: (_path, text) => {
        written.push(text);
      },
      removeSymbols: (path) => {
        removed.push(path);
      },
      scratch: {
        make: () => {
          scratch.push('made');
          return '/scratch/1';
        },
        read: (_directory, file) => {
          const bytes = exported[file];
          if (bytes === undefined) {
            throw new Error(`no ${file} in the export`);
          }
          return bytes;
        },
        remove: () => {
          scratch.push('removed');
        }
      }
    }
  };
}

function editorDocument(text: string, dirty = false, languageId = 'javascript'): ImportDocument {
  return { languageId, isDirty: () => dirty, version: () => 1, text: () => text };
}

function symbol(name: string, start: [number, number], end: [number, number]): unknown {
  return {
    name,
    kind: 11,
    range: { start: { line: start[0], character: start[1] }, end: { line: end[0], character: end[1] } },
    selectionRange: { start: { line: start[0], character: start[1] }, end: { line: start[0], character: start[1] } },
    children: []
  };
}

describe('an import split at the editor\'s symbols', () => {
  before(async () => {
    await initWire();
  });

  it('setup: the offsets of the two texts, as their bytes count them', () => {
    const two = Buffer.from(TWO, 'utf8');
    assert.deepStrictEqual([two.indexOf('}\nfunction beta'), two.indexOf('function beta'), two.length], [31, 33, 65]);
    const cjk = Buffer.from(CJK, 'utf8');
    assert.deepStrictEqual(
      [cjk.indexOf('function'), cjk.indexOf("  return"), cjk.indexOf('}\n'), cjk.length],
      [10, 30, 47, 49]
    );
  });

  /*
   * CJK BEFORE AND INSIDE A SYMBOL, AND AN EMOJI. The editor counts lines and
   * UTF-16 units; the core counts bytes of the file. The two CJK characters of
   * the comment are six bytes and two units, the two of the name the same,
   * the emoji four bytes and two units.
   */
  it('counts the editor\'s positions as bytes of the file, and turns bytes back into positions, with CJK and an emoji', () => {
    const saved = savedText(Buffer.from(CJK, 'utf8'));
    assert.ok(saved !== null);
    const { symbols, left } = fileSymbols(saved, [{ name: '名前', kind: 11, range: { start: { line: 1, character: 9 }, end: { line: 3, character: 1 } } }]);
    assert.deepStrictEqual(symbols, [{ start: 10, end: 48, kind: 'function', name: '名前' }]);
    assert.strictEqual(left, 0);
    const starts = lineStarts(saved.text);
    assert.deepStrictEqual(positionAtByte(saved, starts, 48), { line: 3, character: 1 });
    assert.deepStrictEqual(positionAtByte(saved, starts, 30), { line: 2, character: 0 });
    /*
     * A BYTE INSIDE A CHARACTER IS THAT CHARACTER'S START: byte 4 is inside the
     * comment's first CJK character (bytes 3 to 5), which is unit 3 of line 0.
     */
    assert.deepStrictEqual(positionAtByte(saved, starts, 4), { line: 0, character: 3 });
    /*
     * BETWEEN THE HALVES OF THE EMOJI the editor's position is taken after it
     * (byte 30 + 10 + 4 = 44), and byte 44 reads back as unit 12 of line 2.
     */
    assert.strictEqual(byteOffset(saved, starts, { line: 2, character: 11 }), 44);
    assert.deepStrictEqual(positionAtByte(saved, starts, 44), { line: 2, character: 12 });
    for (const at of [
      { line: 0, character: 0 },
      { line: 0, character: 4 },
      { line: 1, character: 11 },
      { line: 2, character: 10 },
      { line: 2, character: 12 },
      { line: 3, character: 1 }
    ]) {
      assert.deepStrictEqual(positionAtByte(saved, starts, byteOffset(saved, starts, at)), at, `${JSON.stringify(at)} did not read back`);
    }
    const text = importSymbolsText('1.138.0', [{ rel: 'k.js', digest: digest(CJK), languageId: 'javascript', symbols }]);
    assert.strictEqual(
      text,
      `(symbols (path "k.js") (digest "${digest(CJK)}") (source (vscode "1.138.0" "javascript")) (top-level #t))\n` +
        '(symbol 10 48 function "名前")\n'
    );
  });

  it('reads a byte inside the byte-order mark as the start of the file', () => {
    const saved = savedText(Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), Buffer.from(TWO, 'utf8')]));
    assert.ok(saved !== null && saved.bom === 3);
    const starts = lineStarts(saved.text);
    assert.deepStrictEqual(positionAtByte(saved, starts, 2), { line: 0, character: 0 });
    assert.deepStrictEqual(positionAtByte(saved, starts, 3 + 33), { line: 3, character: 0 });
  });

  /*
   * A .js WITH TWO FUNCTIONS: the symbols file names it with two symbols, the
   * core imports it as a file block and two code blocks, and the export and
   * the read place them at lines 1-3 and 4-6.
   */
  it('imports a two-function file as its file block and two code blocks, each placed at its lines', async () => {
    const alpha = 'function alpha() {\n  return 1;\n}\n';
    const beta = 'function beta() {\n  return 2;\n}\n';
    const r = rig(
      { 'a.js': TWO },
      { 'a.js': editorDocument(TWO) },
      { 'a.js': [symbol('alpha', [0, 0], [2, 1]), symbol('beta', [3, 0], [5, 1])] },
      {
        'import-code': { stdout: '(ok (items (ok (event ("w" . 1))) (ok (event ("w" . 2))) (ok (event ("w" . 3)))))\n' },
        'export-code': { stdout: '(ok (files 1))\n' },
        'read w.1': {
          stdout:
            '(ok (items ' +
            record('w.1', null, `(kind . file) (mode . text) (path . "a.js")`) +
            ' ' +
            record('w.2', 'w.1', `(kind . code) (mode . text) (src . ${quoted(alpha)})`) +
            ' ' +
            record('w.3', 'w.1', `(kind . code) (mode . text) (src . ${quoted(beta)})`) +
            '))\n'
        }
      },
      { 'a.js': projection('w.1', [['w.2', alpha], ['w.3', beta]]) }
    );
    const outcome = await runImport(r.deps);
    assert.deepStrictEqual(r.written, [
      `(symbols (path "a.js") (digest "${digest(TWO)}") (source (vscode "1.138.0" "javascript")) (top-level #t))\n` +
        '(symbol 0 32 function "alpha")\n(symbol 33 64 function "beta")\n'
    ]);
    assert.deepStrictEqual(r.sent.map((s) => s.join(' ')), [
      'import-code /src --symbols /tmp/symbols.sexp --wire',
      'export-code /scratch/1',
      'read w.1 --recursive --wire'
    ]);
    assert.deepStrictEqual(r.removed, ['/tmp/symbols.sexp']);
    assert.deepStrictEqual(r.scratch, ['made', 'removed']);
    assert.strictEqual(outcome.done, 'imported');
    if (outcome.done !== 'imported') {
      return;
    }
    assert.deepStrictEqual(outcome.split, [
      {
        rel: 'a.js',
        placed: {
          fileId: 'w.1',
          blocks: [
            { id: 'w.2', range: { start: { line: 0, character: 0 }, end: { line: 3, character: 0 } } },
            { id: 'w.3', range: { start: { line: 3, character: 0 }, end: { line: 6, character: 0 } } }
          ]
        }
      }
    ]);
    assert.deepStrictEqual(importReport(outcome), [
      'a.js: imported as w.1, 2 blocks',
      '  w.2: from line 1, column 1 to line 4, column 1',
      '  w.3: from line 4, column 1 to line 7, column 1'
    ]);
  });

  it('refuses a document with unsaved edits, names it, and sends nothing', async () => {
    const r = rig(
      { 'a.js': TWO, 'b.js': TWO },
      { 'a.js': editorDocument(TWO, true), 'b.js': editorDocument(TWO) },
      { 'a.js': [symbol('alpha', [0, 0], [2, 1])], 'b.js': [symbol('alpha', [0, 0], [2, 1])] },
      {}
    );
    const outcome = await runImport(r.deps);
    assert.deepStrictEqual(outcome, { done: 'stopped', why: `a.js ${UNSAVED}` });
    assert.deepStrictEqual(r.sent, []);
    assert.deepStrictEqual(r.written, []);
  });

  it('stops when the document is not the file on disk, and sends nothing', async () => {
    const r = rig({ 'a.js': TWO }, { 'a.js': editorDocument(TWO.replace('1', '2')) }, { 'a.js': [symbol('alpha', [0, 0], [2, 1])] }, {});
    assert.deepStrictEqual(await runImport(r.deps), { done: 'stopped', why: CHANGED_WHILE_COLLECTING });
    assert.deepStrictEqual(r.sent, []);
  });

  it('sends nothing when no file has a top-level symbol', async () => {
    const r = rig({ 'a.js': TWO }, { 'a.js': editorDocument(TWO) }, { 'a.js': [] }, {});
    assert.deepStrictEqual(await runImport(r.deps), { done: 'stopped', why: NO_SYMBOLS });
    assert.deepStrictEqual(r.sent, []);
  });

  /*
   * A REFUSAL SHOWN AT ITS LINE. The core refuses k.js's cut at byte 30, the
   * first byte of line 2 (counted from 0) of the CJK text: line 3, column 1.
   * Nothing was split, so nothing is exported.
   */
  it('shows a file the core refused with the byte it names as a line and a column', async () => {
    const r = rig(
      { 'k.js': CJK },
      { 'k.js': editorDocument(CJK) },
      { 'k.js': [symbol('名前', [1, 9], [3, 1])] },
      { 'import-code': { stdout: '(ok (items) (symbols-refused (("k.js" (error symbols-not-top-level (at 30))))))\n' } }
    );
    const outcome = await runImport(r.deps);
    assert.strictEqual(outcome.done, 'imported');
    if (outcome.done !== 'imported') {
      return;
    }
    assert.deepStrictEqual(outcome.refused.map((f) => [f.rel, f.at]), [['k.js', { line: 2, character: 0 }]]);
    assert.deepStrictEqual(outcome.split, []);
    assert.deepStrictEqual(r.sent.map((s) => s[0]), ['import-code']);
    assert.deepStrictEqual(r.scratch, []);
    assert.deepStrictEqual(importReport(outcome), [
      'k.js: not imported, the core refused its symbols (symbols-not-top-level) at line 3, column 1: (error symbols-not-top-level (at 30))'
    ]);
  });

  it('says a file that followed its markers, and a file the core skipped', async () => {
    const r = rig(
      { 'a.js': TWO },
      { 'a.js': editorDocument(TWO) },
      { 'a.js': [symbol('alpha', [0, 0], [2, 1])] },
      { 'import-code': { stdout: '(ok (items) (skipped ("bin.dat")) (symbols-ignored ("a.js")))\n' } }
    );
    const outcome = await runImport(r.deps);
    assert.strictEqual(outcome.done, 'imported');
    if (outcome.done !== 'imported') {
      return;
    }
    assert.deepStrictEqual([outcome.ignored, outcome.skipped, outcome.split], [['a.js'], ['bin.dat'], []]);
    assert.deepStrictEqual(importReport(outcome), [
      'a.js: carries markers, so it followed them; its symbols were not used (symbols-ignored)',
      'bin.dat: skipped by the core, not UTF-8 text or holding a NUL byte'
    ]);
  });

  it('carries a refusal of the whole import as the core wrote it, and still removes the symbols file', async () => {
    const r = rig(
      { 'a.js': TWO },
      { 'a.js': editorDocument(TWO) },
      { 'a.js': [symbol('alpha', [0, 0], [2, 1])] },
      { 'import-code': { stdout: '(error symbols-malformed (line 1))\n', rc: 1 } }
    );
    const outcome = await runImport(r.deps);
    assert.strictEqual(outcome.done, 'refused');
    assert.deepStrictEqual(r.removed, ['/tmp/symbols.sexp']);
  });
});
