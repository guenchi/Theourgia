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
 * A TEXT-MODE CODE BLOCK CAN BE OPENED.
 *
 * The core stores a text-mode code block's source as bytes and prints them
 * as `#vu8(n ...)`. This extension's reader refused that spelling, so no
 * such block could be opened -- measured on the pinned core: `read` of an
 * imported `.ss` block answered `(src . #vu8(59 59 32 ...))` and the
 * window said "could not be read ... bad # literal". These cells pin the
 * reading of the literal, the one decoder for text fields, the document
 * view's use of it, and -- on a real core -- opening and saving such a block.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import { spawnSync } from 'child_process';
import { TextNotUtf8, documentFor, languageModeOf, readBlock, stringField, textOf, titleOf, Block } from '../../src/blocks';
import { composeDocument } from '../../src/document-view';
import { AnswerParseError, Datum, initWire, parseAnswers } from '../../src/wire';

/*
 * THE ANSWER THE PINNED CORE GAVE for an imported text source, recorded by
 * the probe that found this gap (archive/theourgia-vsc-delivery-plugin-r3-
 * item-51-2026-09-27/readings/probe-read.txt), verbatim.
 */
const RECORDED =
  '(ok ((id . "xnsczn2i.2") (deleted . #f) (fields (doc . #vu8(59 59 32 111 110 101 10 59 59 32 116 119 111 10)) ' +
  '(kind . code) (lang . scheme) (mode . text) (name . "alpha") (src . #vu8(59 59 32 111 110 101 10 59 59 32 116 ' +
  '119 111 10 40 100 101 102 105 110 101 32 97 108 112 104 97 32 49 41 10))) (position "xnsczn2i.1" . 0) (edges)))';

function blockOf(answer: string): Block {
  const form = parseAnswers(answer)[0] as Datum[];
  const block = readBlock(form[1]);
  assert.ok(block !== null, 'the recorded answer is not a block');
  return block as Block;
}

function bytesAnswer(src: number[]): string {
  return `(ok ((id . "c.1") (deleted . #f) (fields (kind . code) (lang . scheme) (mode . text) (src . #vu8(${src.join(' ')}))) (position root . 0) (edges)))`;
}

describe('B1 the reader takes a bytevector as bytes, and refuses one that is not', () => {
  before(async () => {
    await initWire();
  });

  it('reads #vu8(), #vu8(0 255), one nested in a list, and leaves "#vu8(" inside a string alone', () => {
    const [empty, two, nested, quoted] = parseAnswers('#vu8()\n#vu8(0 255)\n(a (b #vu8(1 2)) c)\n("#vu8(" #vu8(7))\n');
    assert.ok(empty instanceof Uint8Array && empty.length === 0);
    assert.deepStrictEqual(Array.from(two as Uint8Array), [0, 255]);
    const inner = ((nested as Datum[])[1] as Datum[])[1];
    assert.deepStrictEqual(Array.from(inner as Uint8Array), [1, 2]);
    assert.strictEqual((quoted as Datum[])[0], '#vu8(');
    assert.deepStrictEqual(Array.from((quoted as Datum[])[1] as Uint8Array), [7]);
  });

  it('refuses a number that is not a byte, and a literal left open, naming line and position', () => {
    assert.throws(
      () => parseAnswers('(ok)\n(x #vu8(256))\n'),
      (e: unknown) => e instanceof AnswerParseError && e.line === 2 && e.position === 3 && /not a byte/.test(e.message)
    );
    assert.throws(
      () => parseAnswers('(x #vu8(1\n'),
      (e: unknown) => e instanceof AnswerParseError && e.line === 1 && e.position === 3 && /no closing parenthesis/.test(e.message)
    );
  });
});

describe('B2 one decoder: text is itself, UTF-8 bytes decode, other bytes are refused by name', () => {
  before(async () => {
    await initWire();
  });

  it('answers a string as itself and UTF-8 bytes (CJK included) as their text', () => {
    const cjk = Array.from(Buffer.from('(define \u4e2d 1)\n', 'utf8'));
    const block = blockOf(bytesAnswer(cjk));
    assert.strictEqual(textOf(block, 'src'), '(define \u4e2d 1)\n');
    assert.strictEqual(stringField(block, 'src'), '(define \u4e2d 1)\n');
    const text = blockOf('(ok ((id . "t.1") (deleted . #f) (fields (title . "T") (src . "x\\n")) (position root . 0) (edges)))');
    assert.strictEqual(textOf(text, 'src'), 'x\n');
  });

  it('refuses bytes that are not UTF-8 as text-not-utf8, with the field, the block and the offset', () => {
    const block = blockOf(bytesAnswer([40, 97, 32, 0xff, 41]));
    assert.throws(
      () => textOf(block, 'src'),
      (e: unknown) => e instanceof TextNotUtf8 && e.field === 'src' && e.id === 'c.1' && e.offset === 3 && /text-not-utf8/.test(e.message)
    );
  });

  it("keeps the readers' own answers for an absent field and a conflicted one", () => {
    const block = blockOf(
      '(ok ((id . "k.1") (deleted . #f) (fields (path . "p.scm") (title conflict ("A" "B"))) (position root . 0) (edges)))'
    );
    assert.strictEqual(stringField(block, 'keywords'), '');
    assert.strictEqual(textOf(block, 'keywords'), undefined);
    assert.strictEqual(titleOf(block), 'p.scm', 'a conflicted title no longer falls back to the path');
  });
});

describe("B3 a text-mode block's document is its decoded source, byte for byte, in the block's language", () => {
  before(async () => {
    await initWire();
  });

  it('opens the recorded answer as its source, in the block language', () => {
    const block = blockOf(RECORDED);
    assert.strictEqual(documentFor(block, '/s').text, ';; one\n;; two\n(define alpha 1)\n');
    assert.strictEqual(languageModeOf(block), 'scheme');
  });

  it('keeps a CRLF source CRLF', () => {
    const crlf = Array.from(Buffer.from(';; one\r\n(define alpha 1)\r\n', 'utf8'));
    const block = blockOf(bytesAnswer(crlf));
    assert.strictEqual(Buffer.from(documentFor(block, '/s').text, 'utf8').toString('hex'), Buffer.from(crlf).toString('hex'));
  });

  it('shows a prose block as markdown', () => {
    const prose = blockOf('(ok ((id . "t.1") (deleted . #f) (fields (kind . section) (title . "T") (src . "x\\n")) (position root . 0) (edges)))');
    assert.strictEqual(languageModeOf(prose), 'markdown');
  });
});

describe('B4 the derived doc decodes through the same decoder', () => {
  before(async () => {
    await initWire();
  });
  it("reads the recorded answer's doc bytes as their text", () => {
    assert.strictEqual(textOf(blockOf(RECORDED), 'doc'), ';; one\n;; two\n');
  });
});

describe('B7 the document view composes through the shared decoder', () => {
  before(async () => {
    await initWire();
  });

  const as = (src: string): Datum[] =>
    parseAnswers(
      `((id . "d.1") (deleted . #f) (fields (title . "Doc") (heading-src . "# Doc\\n") (src . ${src})) (position root . 0) (edges))\n`
    );

  it('composes the same document from a bytes source as from a string one', () => {
    const bytes = `#vu8(${Array.from(Buffer.from('body \u4e2d\n', 'utf8')).join(' ')})`;
    assert.deepStrictEqual(composeDocument('d.1', as(bytes)), composeDocument('d.1', as('"body \u4e2d\\n"')));
    assert.ok(composeDocument('d.1', as(bytes)).ok);
  });

  it('refuses a conflicted field, and bytes that are not UTF-8 by name', () => {
    const conflicted = parseAnswers(
      '((id . "d.1") (deleted . #f) (fields (title conflict ("A" "B")) (src . "x")) (position root . 0) (edges))\n'
    );
    assert.strictEqual(composeDocument('d.1', conflicted).ok, false);
    const bad = composeDocument('d.1', as('#vu8(98 255)'));
    assert.ok(!bad.ok && /text-not-utf8/.test(bad.reason) && bad.ids.includes('d.1'), JSON.stringify(bad));
  });

  it('has no decoder of its own any more', () => {
    const text = fs.readFileSync(path.join(__dirname, '..', '..', '..', 'src', 'document-view.ts'), 'utf8');
    assert.doesNotMatch(text, /function textOf\(/);
    assert.match(text, /import \{[^}]*\btextOf\b[^}]*\} from '\.\/blocks'/);
  });
});

/*
 * THE HARNESS, WITH A REAL CORE: the compiled extension against a stand-in
 * VS Code API, the store reached through the shipping transport. The
 * editor host cannot read a shown document's language or a notification
 * back, so this is where they are read.
 */
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

describe('B5 a text-mode block saved unchanged on a real core reads back and exports as the same bytes', function () {
  this.timeout(180000);
  it('round-trips the bytes, refuses a buffer that begins with a mark, and keeps an imported mark', () => {
    const r = schedule('bytes-save');
    assert.deepStrictEqual(r.afterSave.filter((n: { level: string }) => n.level !== 'information'), [], JSON.stringify(r.afterSave));
    assert.strictEqual(r.rereadHex, r.srcHex, 'the saved source is not the imported bytes');
    assert.strictEqual(r.exportedHex, r.srcHex, 'the exported file is not the imported bytes');
    assert.ok(
      r.markRefusal.some((n: { text: string }) => /begins with a byte-order mark/.test(n.text)),
      `a buffer beginning with a mark was not refused: ${JSON.stringify(r.markRefusal)}`
    );
    assert.deepStrictEqual(r.afterBomSave.filter((n: { level: string }) => n.level !== 'information'), [], JSON.stringify(r.afterBomSave));
    assert.strictEqual(r.bomExportedHex, r.bomHex, 'a source imported with a mark did not export with it');
  });
});

describe('B6 opening a text-mode block on a real core', function () {
  this.timeout(180000);
  it("shows the source in the block's language, and a block that is not UTF-8 as the core's refusal by name", () => {
    const r = schedule('bytes-open');
    assert.deepStrictEqual(r.goodErrors, [], JSON.stringify(r.goodErrors));
    assert.strictEqual(r.goodFileHex, r.goodHex, 'the opened file is not the source, byte for byte');
    assert.strictEqual(r.languageId, 'scheme', 'the block was not shown in its language');
    assert.strictEqual(r.badOpened, 0, 'a block that is not UTF-8 was opened');
    assert.ok(
      r.badShown.some((n: { level: string; text: string }) => n.level === 'error' && /non-text-projection/.test(n.text)),
      `the refusal was not shown by name: ${JSON.stringify(r.badShown)}`
    );
  });
});
