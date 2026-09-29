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
 * WHERE EACH BLOCK SITS IN A PROJECTED FILE, read off its marker lines.
 *
 * Every expected byte range below was counted from the fixture's own parts
 * by a separate script, not by the reader: a header line of a one-file
 * store is 116 bytes under `// ` (9 of wrapping and `@file `, 106 of hex, a
 * line feed), and each part's length is written beside it. The layout they
 * pin is the core's `projection-encode-map` (code-markers.sc): the first
 * block's prefix is its own source, a pad line feed belongs to the marker
 * after it, and every control names the block whose source follows.
 */

import * as assert from 'assert';
import { coreRegexCompile, coreRegexMatches } from '../../src/core-regex';
import { Piece, blockAt, readProjection, sourcePrefixSize } from '../../src/markers';
import { initWire } from '../../src/wire';

const BOM = Buffer.from([0xef, 0xbb, 0xbf]);

/*
 * A HEADER AS THE CORE WRITES ONE: the store's id, the FILE'S BLOCK ID (not
 * its path), the cut, the mode and the prefix pad. Each id here is as long
 * as the path it stands beside, and the writer's name as long as the id, so
 * every header line is 116 bytes under `// `.
 */
function header(fileId: string, pad: number, version = 1, mode = 'text'): string {
  return hexOf(`(code-projection ${version} "s1" "${fileId}" (("${'w'.repeat(fileId.length)}" . 0)) ${mode} ${pad})`);
}

function hexOf(datum: string): string {
  return Buffer.from(datum, 'utf8').toString('hex');
}

function bytesOf(...parts: Array<string | Buffer>): Buffer {
  return Buffer.concat(parts.map((p) => (typeof p === 'string' ? Buffer.from(p, 'utf8') : p)));
}

const source = (id: string, start: number, end: number): Piece => ({ kind: 'source', id, start, end });
const control = (id: string, start: number, end: number): Piece => ({ kind: 'control', id, start, end });

/*
 * [0,116) header | [116,135) `// @block aa pad 0` | [135,151) a's body |
 * [151,170) `// @block bb pad 0` | [170,186) b's body.
 */
const LINE_COMMENT = bytesOf(
  `// @file ${header('f.aa', 0)}\n`,
  '// @block aa pad 0\n',
  'function a() {}\n',
  '// @block bb pad 0\n',
  'function b() {}\n'
);

function projectionOf(bytes: Buffer, file: string) {
  const reading = readProjection(bytes, file);
  assert.ok(reading.ok, `the file did not read: ${JSON.stringify(reading)}`);
  return reading.projection;
}

describe('the marker reader', () => {
  before(async () => {
    await initWire();
  });

  it('reads a line-comment file: the wrapping off its header, two blocks, each piece where the bytes put it', () => {
    const p = projectionOf(LINE_COMMENT, 'a.js');
    assert.strictEqual(p.length, 186);
    assert.deepStrictEqual(p.blocks, ['aa', 'bb']);
    assert.deepStrictEqual(p.pieces, [
      control('aa', 0, 116),
      control('aa', 116, 135),
      source('aa', 135, 151),
      control('bb', 151, 170),
      source('bb', 170, 186)
    ]);
  });

  it('reads a block-comment file by its own wrapping, and a line-comment marker inside it is source', () => {
    /*
     * [0,115) header | [115,137) `/* @block cc pad 0 *\/` | [137,163) c's
     * body, holding a line that WOULD be a marker under `// ` |
     * [163,185) `/* @block dd pad 0 *\/` | [185,192) d's body.
     */
    const bytes = bytesOf(
      `/* @file ${header('f.b', 0)} */\n`,
      '/* @block cc pad 0 */\n',
      'int c;\n// @block zz pad 0\n',
      '/* @block dd pad 0 */\n',
      'int d;\n'
    );
    const p = projectionOf(bytes, 'b.c');
    assert.deepStrictEqual(p.blocks, ['cc', 'dd']);
    assert.deepStrictEqual(p.pieces, [
      control('cc', 0, 115),
      control('cc', 115, 137),
      source('cc', 137, 163),
      control('dd', 163, 185),
      source('dd', 185, 192)
    ]);
  });

  it('keeps an escaped @@block line as source, without splitting the block', () => {
    /*
     * a's body is [135,171): `function a() {}\n` (16) and
     * `// @@block zz pad 0\n` (20).
     */
    const bytes = bytesOf(
      `// @file ${header('f.aa', 0)}\n`,
      '// @block aa pad 0\n',
      'function a() {}\n// @@block zz pad 0\n',
      '// @block bb pad 0\n',
      'function b() {}\n'
    );
    const p = projectionOf(bytes, 'a.js');
    assert.deepStrictEqual(p.blocks, ['aa', 'bb']);
    assert.deepStrictEqual(p.pieces.filter((x) => x.kind === 'source'), [source('aa', 135, 171), source('bb', 190, 206)]);
  });

  it('gives the first block its prefix above the header: a byte-order mark and a shebang, as two source pieces', () => {
    /*
     * [0,23) the mark (3) and `#!/usr/bin/env node\n` (20) | [23,139)
     * header | [139,158) ee's marker | [158,165) `run();\n` | [165,184)
     * ff's marker | [184,192) `stop();\n`.
     */
    const bytes = bytesOf(
      BOM,
      '#!/usr/bin/env node\n',
      `// @file ${header('f.cc', 0)}\n`,
      '// @block ee pad 0\n',
      'run();\n',
      '// @block ff pad 0\n',
      'stop();\n'
    );
    const p = projectionOf(bytes, 'c.js');
    assert.deepStrictEqual(p.pieces, [
      source('ee', 0, 23),
      control('ee', 23, 139),
      control('ee', 139, 158),
      source('ee', 158, 165),
      control('ff', 165, 184),
      source('ff', 184, 192)
    ]);
    assert.strictEqual(blockAt(p, 0), 'ee', 'the mark is not the first block\'s');
    assert.strictEqual(blockAt(p, 22), 'ee', 'the shebang\'s line feed is not the first block\'s');
  });

  it('counts a pad line feed with the marker after it, never with the block before', () => {
    /*
     * [0,9) `#!/bin/sh` with no line feed | [9,125) the pad line feed (1)
     * and the header (115), whose last element is 1 | [125,143) gg's
     * marker | [143,148) `x = 1` | [148,167) the pad line feed and
     * `# @block hh pad 1` | [167,172) `y = 2`, the file's end.
     */
    const bytes = bytesOf(
      '#!/bin/sh',
      '\n',
      `# @file ${header('f.dd', 1)}\n`,
      '# @block gg pad 0\n',
      'x = 1',
      '\n# @block hh pad 1\n',
      'y = 2'
    );
    const p = projectionOf(bytes, 'd.py');
    assert.deepStrictEqual(p.pieces, [
      source('gg', 0, 9),
      control('gg', 9, 125),
      control('gg', 125, 143),
      source('gg', 143, 148),
      control('hh', 148, 167),
      source('hh', 167, 172)
    ]);
    assert.strictEqual(blockAt(p, 148), null, 'the pad line feed was read as gg\'s source');
    assert.strictEqual(blockAt(p, 9), null, 'the header\'s pad line feed was read as the prefix');
  });

  it('refuses a file whose header does not read or is missing, and one with a bad marker; keeps the file id', () => {
    const body = ['// @block aa pad 0\n', 'function a() {}\n'];
    const none = 'no code-projection header line where the prefix ends';
    const headed = (datum: string) => bytesOf(`// @file ${hexOf(datum)}\n`, ...body);
    const cases: Array<[string, Buffer, string]> = [
      ['odd hex', bytesOf('// @file abc\n', ...body), none],
      ['not a projection header', bytesOf(`// @file ${Buffer.from('(ok 1)').toString('hex')}\n`, ...body), none],
      ['another version', bytesOf(`// @file ${header('f.aa', 0, 2)}\n`, ...body), none],
      ['the datum mode', bytesOf(`// @file ${header('f.aa', 0, 1, 'datum')}\n`, ...body), none],
      ['no header at all', bytesOf(...body), none],
      ['a negative sequence in the cut', headed('(code-projection 1 "s1" "f.aa" (("w" . -1)) text 0)'), none],
      ['a cut that is not a list', headed('(code-projection 1 "s1" "f.aa" 7 text 0)'), none],
      ['a store id that is not a string', headed('(code-projection 1 s1 "f.aa" () text 0)'), none],
      ['a file id that is not a string', headed('(code-projection 1 "s1" 12 () text 0)'), none],
      ['a pad of 2', headed('(code-projection 1 "s1" "f.aa" () text 2)'), none],
      ['hex that is not UTF-8', bytesOf('// @file fffe\n', ...body), none],
      ['a datum that does not parse', headed('(code-projection 1 "s1"'), none],
      ['a pad claimed at the start', bytesOf(`// @file ${header('f.aa', 1)}\n`, ...body), 'a header claiming a pad at the start of the file'],
      [
        'an id the grammar refuses',
        bytesOf(`// @file ${header('f.aa', 0)}\n`, ...body, '// @block Bad pad 0\n', 'x\n'),
        'a block marker that does not read at line 4'
      ],
      ['the id `.`', bytesOf(`// @file ${header('f.aa', 0)}\n`, '// @block . pad 0\n', 'x\n'), 'a block marker that does not read at line 2'],
      [
        'an id of 257 characters',
        bytesOf(`// @file ${header('f.aa', 0)}\n`, `// @block ${'a'.repeat(257)} pad 0\n`, 'x\n'),
        'a block marker that does not read at line 2'
      ],
      [
        'a second header',
        bytesOf(`// @file ${header('f.aa', 0)}\n`, ...body, `// @file ${header('f.aa', 0)}\n`),
        'a second header line at line 4'
      ]
    ];
    for (const [name, bytes, why] of cases) {
      assert.deepStrictEqual(readProjection(bytes, 'a.js'), { ok: false, file: 'a.js', why }, name);
    }
    assert.strictEqual(projectionOf(LINE_COMMENT, 'a.js').fileId, 'f.aa', 'the header\'s file id was not kept');
    const long = projectionOf(bytesOf(`// @file ${header('f.aa', 0)}\n`, `// @block ${'a'.repeat(256)} pad 0\n`, 'x\n'), 'a.js');
    assert.deepStrictEqual(long.blocks, ['a'.repeat(256)], 'an id of 256 characters was refused');
    const big = projectionOf(headed('(code-projection 1 "s1" "f.aa" (("w" . 9007199254740993)) text 0)'), 'a.js');
    assert.strictEqual(big.fileId, 'f.aa', 'a cut sequence past 2^53 was refused');
  });

  it('finds the header where the core\'s prefix ends, whatever header-shaped lines the prefix holds', () => {
    /*
     * Two coding lines of the first block's prefix, each a header or a marker
     * under the wrapping `# coding:utf-8 `: [0,161) the prefix | [161,277)
     * the header | [277,296) aa's marker | [296,298) `x`.
     */
    const coding = bytesOf(
      `# coding:utf-8 @file ${header('f.aa', 0)}\n`,
      '# coding:utf-8 @block fake pad 0\n',
      `// @file ${header('f.aa', 0)}\n`,
      '// @block aa pad 0\n',
      'x\n'
    );
    const p = projectionOf(coding, 'a.js');
    assert.deepStrictEqual(p.blocks, ['aa']);
    assert.deepStrictEqual(p.pieces, [source('aa', 0, 161), control('aa', 161, 277), control('aa', 277, 296), source('aa', 296, 298)]);
    /*
     * A `#!` line, then a coding line, each ending in a closing token:
     * [0,163) the prefix | [163,279) | [279,298) | [298,300).
     */
    const shebang = bytesOf(
      `#! @file ${header('f.aa', 0)} coding:utf-8\n`,
      '#! @block fake pad 0 coding:utf-8\n',
      `// @file ${header('f.aa', 0)}\n`,
      '// @block aa pad 0\n',
      'x\n'
    );
    const q = projectionOf(shebang, 'a.js');
    assert.deepStrictEqual(q.pieces, [source('aa', 0, 163), control('aa', 163, 279), control('aa', 279, 298), source('aa', 298, 300)]);
    /*
     * A comment behind a form feed, which the core's trim removes, holding a
     * header under a form feed and `# `, then a coding line: [0,127) the
     * prefix | [127,242) the header | [242,260) aa's marker | [260,266)
     * `x = 1`.
     */
    const fed = bytesOf(`\f# @file ${header('f.aa', 0)}\n`, '# coding:a\n', `# @file ${header('f.aa', 0)}\n`, '# @block aa pad 0\n', 'x = 1\n');
    assert.deepStrictEqual(projectionOf(fed, 'a.py').pieces, [
      source('aa', 0, 127),
      control('aa', 127, 242),
      control('aa', 242, 260),
      source('aa', 260, 266)
    ]);
  });

  it('measures a comment followed by a coding line as a prefix, and a byte-order mark alone with its pad line', () => {
    /*
     * [0,31) `# note` and `# -*- coding: utf-8 -*-` | [31,146) the header
     * under `# ` | [146,164) aa's marker | [164,170) `x = 1`.
     */
    const commented = bytesOf('# note\n', '# -*- coding: utf-8 -*-\n', `# @file ${header('f.aa', 0)}\n`, '# @block aa pad 0\n', 'x = 1\n');
    assert.deepStrictEqual(projectionOf(commented, 'a.py').pieces, [
      source('aa', 0, 31),
      control('aa', 31, 146),
      control('aa', 146, 164),
      source('aa', 164, 170)
    ]);
    /*
     * [0,3) the mark, the prefix | [3,120) the pad line feed and the header,
     * whose last element is 1 | [120,139) aa's marker | [139,141) `x`.
     */
    const marked = Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), bytesOf('\n', `// @file ${header('f.aa', 1)}\n`, '// @block aa pad 0\n', 'x\n')]);
    assert.deepStrictEqual(projectionOf(marked, 'a.js').pieces, [
      source('aa', 0, 3),
      control('aa', 3, 120),
      control('aa', 120, 139),
      source('aa', 139, 141)
    ]);
  });

  it('measures a prefix as the core does, where its pattern and trim differ from JavaScript\'s', () => {
    const cases: Array<[string, Buffer, number]> = [
      ['a coding line', bytesOf('# -*- coding: utf-8 -*-\nx\n'), 24],
      ['a coding line of 4099 characters, which the core\'s pattern refuses', bytesOf(`# coding=${'a'.repeat(4090)}\nx\n`), 0],
      ['a coding line of 4096 characters', bytesOf(`# coding=${'a'.repeat(4087)}\nx\n`), 4097],
      ['two lines at most', bytesOf('#!/bin/sh\n# coding: latin-1\n# coding: x\n'), 28],
      ['a comment, then a coding line', bytesOf('# note\n# coding: utf-8\nx\n'), 23],
      ['a comment alone', bytesOf('# note\nx\n'), 0],
      ['a line separator before `coding`, which the core\'s `.` crosses', bytesOf('# a\u2028 coding: x\nx\n'), 17],
      ['a no-break space before `#`, which the core\'s trim keeps', bytesOf('\u00a0# note\n# coding: utf-8\nx\n'), 0],
      ['a form feed before `#`, which the core\'s trim removes', bytesOf('\f#\n# coding:a\nx\n'), 14],
      ['a coding line whose spaces run past the core\'s work limit', bytesOf(`#coding:${' '.repeat(4000)}a\n`), 0],
      ['the same line within the limit', bytesOf(`#coding:${' '.repeat(3000)}a\n`), 3010]
    ];
    for (const [name, bytes, size] of cases) {
      assert.strictEqual(sourcePrefixSize(bytes), size, name);
    }
  });

  it('takes the header line that a block marker follows, not a shebang line holding a header', () => {
    /*
     * The first block's prefix is a shebang line whose text is a header
     * under `#! `; the core keeps it above the real header, as that
     * block's own bytes. [0,116) `#! @file <hex>` | [116,232) the header |
     * [232,251) aa's marker | [251,274) aa's body, `#! @block ghost pad 0`
     * and `x`.
     */
    const bytes = bytesOf(
      `#! @file ${header('f.aa', 0)}\n`,
      `// @file ${header('f.aa', 0)}\n`,
      '// @block aa pad 0\n',
      '#! @block ghost pad 0\nx'
    );
    const p = projectionOf(bytes, 'a.js');
    assert.deepStrictEqual(p.blocks, ['aa']);
    assert.deepStrictEqual(p.pieces, [source('aa', 0, 116), control('aa', 116, 232), control('aa', 232, 251), source('aa', 251, 274)]);
  });

  it('reads a file with no block, an empty block, and an escaped @@file line', () => {
    const empty = projectionOf(bytesOf(`// @file ${header('f.aa', 0)}\n`), 'a.js');
    assert.deepStrictEqual([empty.blocks, empty.pieces], [[], [{ kind: 'control', id: null, start: 0, end: 116 }]]);
    /*
     * [0,116) header | [116,135) aa's marker | aa's source is empty, [135,135)
     * | [135,154) bb's marker | [154,170) `// @@file abc` and `x`.
     */
    const p = projectionOf(
      bytesOf(`// @file ${header('f.aa', 0)}\n`, '// @block aa pad 0\n', '// @block bb pad 0\n', '// @@file abc\nx\n'),
      'a.js'
    );
    assert.deepStrictEqual(p.pieces, [
      control('aa', 0, 116),
      control('aa', 116, 135),
      source('aa', 135, 135),
      control('bb', 135, 154),
      source('bb', 154, 170)
    ]);
  });

  it('reads a marker with no pad as pad 0, and answers the end of a file whose last block is empty, or which has none', () => {
    /*
     * [0,116) header | [116,129) `// @block aa` | [129,131) `x` |
     * [131,144) `// @block bb`, and bb's source is empty at the end.
     */
    const p = projectionOf(bytesOf(`// @file ${header('f.aa', 0)}\n`, '// @block aa\n', 'x\n', '// @block bb\n'), 'a.js');
    assert.deepStrictEqual(p.pieces, [control('aa', 0, 116), control('aa', 116, 129), source('aa', 129, 131), control('bb', 131, 144), source('bb', 144, 144)]);
    assert.strictEqual(blockAt(p, 144), 'bb');
    const none = projectionOf(bytesOf(`// @file ${header('f.aa', 0)}\n`), 'a.js');
    assert.strictEqual(blockAt(none, 116), null);
  });

  it('answers the block of a byte: its source\'s block, null on a marker line, the last block at the end', () => {
    const p = projectionOf(LINE_COMMENT, 'a.js');
    assert.strictEqual(blockAt(p, 135), 'aa');
    assert.strictEqual(blockAt(p, 150), 'aa');
    assert.strictEqual(blockAt(p, 170), 'bb');
    assert.strictEqual(blockAt(p, 0), null, 'a byte of the header gave a block');
    assert.strictEqual(blockAt(p, 151), null, 'a byte of a marker line gave a block');
    assert.strictEqual(blockAt(p, 169), null, 'the marker\'s line feed gave a block');
    assert.strictEqual(blockAt(p, 186), 'bb', 'the end of the file is not the last block\'s');
    assert.strictEqual(blockAt(p, 187), null, 'a byte past the end gave a block');
  });

  it('reads marker lines ending in CRLF, as the core\'s line split does', () => {
    /*
     * Every part one byte longer than in the line-comment file: [0,117) |
     * [117,137) | [137,154) | [154,174) | [174,191).
     */
    const bytes = bytesOf(
      `// @file ${header('f.aa', 0)}\r\n`,
      '// @block aa pad 0\r\n',
      'function a() {}\r\n',
      '// @block bb pad 0\r\n',
      'function b() {}\r\n'
    );
    const p = projectionOf(bytes, 'a.js');
    assert.deepStrictEqual(p.pieces, [
      control('aa', 0, 117),
      control('aa', 117, 137),
      source('aa', 137, 154),
      control('bb', 154, 174),
      source('bb', 174, 191)
    ]);
  });
});

/*
 * THE CORE'S ENGINE, BEYOND THE ONE PATTERN IT SERVES: the answers below are
 * a step-for-step Python port of regex.sc's, not this transcription's.
 */
describe('the core\'s regular expressions, as regex.sc answers them', () => {
  it('matches groups and repeated groups, and refuses the patterns regex.sc refuses', () => {
    const cases: Array<[string, string, boolean]> = [
      ['(a)', 'a', true],
      ['(?:ab)+', 'abab', true],
      ['(?:ab)+', 'a', false],
      ['(a)+b', 'aab', true],
      ['^x(?:yz)*$', 'xyzyz', true],
      ['^x(?:yz)*$', 'xyzy', false],
      ['[a-c]\\d?', 'b7', true],
      ['\\s+x', ' \tx', true],
      ['^(?:a|ab)+c$', 'abc', true]
    ];
    for (const [pattern, text, matches] of cases) {
      assert.strictEqual(coreRegexMatches(coreRegexCompile(pattern), text), matches, `${pattern} on ${JSON.stringify(text)}`);
    }
    for (const pattern of ['a)', '[a-\\d]', 'a\\', '(a', '(?xa)']) {
      assert.throws(() => coreRegexCompile(pattern), /regex:/, `${pattern} was compiled`);
    }
  });
});
