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
 * GO TO DEFINITION THROUGH THE STORE'S `whereis`.
 *
 * The store says which block defines a name, or which library exports it,
 * and never which line; the line is found in the text the person is looking
 * at. These cells pin the name that is asked for, the reading of the two
 * kinds of record and of the refusal, the line, and -- in the harness, with
 * a real core -- the command and the editor's own Go to Definition.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import { spawnSync } from 'child_process';
import { Client, answerKind } from '../../src/client';
import {
  DefinitionRecord,
  definitionLine,
  definitionsOf,
  firstLineAfter,
  identifierAt,
  noDefinitionNotice,
  recordLabel,
  targetLine
} from '../../src/definition';
import { RawResult, TransportError } from '../../src/transport';
import { initWire } from '../../src/wire';

const TWO_RECORDS =
  '(ok (items (def "a.1" (library (probe d)) (name alpha) (kind code)) (export "b.1" (library (probe d)) (name alpha))) ' +
  '(cut ()) (scanned-blocks 2) (fields (names exports)))\n';

function answering(stdout: string, rc = 0, sent: string[][] = []): Client {
  return new Client({
    kind: 'test',
    send: async (verb: string, args: string[]): Promise<RawResult> => {
      sent.push([verb, ...args]);
      return { argv: [verb, ...args], rc, stdout, stderr: '' };
    }
  });
}

describe('D1 the exact name under the cursor is asked for, and the answer is a list of records', () => {
  before(async () => {
    await initWire();
  });

  it('takes the whole identifier, punctuation included, and a key inside [[ ]]', () => {
    assert.strictEqual(identifierAt('(display foo-bar)', 0, 12), 'foo-bar');
    assert.strictEqual(identifierAt('(if (foo? x) 1 2)', 0, 7), 'foo?');
    assert.strictEqual(identifierAt('see [[key]] here', 0, 7), 'key');
    assert.strictEqual(identifierAt('(a  b)', 0, 3), null, 'a place between two delimiters was taken for a name');
  });

  it('sends whereis over --wire and nothing else, and reads one def and one export as two records', async () => {
    const sent: string[][] = [];
    const answer = await definitionsOf(answering(TWO_RECORDS, 0, sent), 'alpha');
    assert.deepStrictEqual(sent, [['whereis', 'alpha', '--wire']]);
    assert.strictEqual(answerKind('whereis', ['alpha', '--wire']), 'items');
    assert.ok('found' in answer, JSON.stringify(answer));
    assert.deepStrictEqual(
      answer.found.map((r) => [r.kind, r.id, r.library, r.name]),
      [
        ['def', 'a.1', '(probe d)', 'alpha'],
        ['export', 'b.1', '(probe d)', 'alpha']
      ]
    );
  });
});

/*
 * THE LINE. The document is a one-line prefix (the heading the editor
 * shows) and then the source; lines are counted in the document.
 */
describe('D2 the line is the exact definition, outside comments, across a line break, in the document', () => {
  const PREFIX = '# Alpha\n';
  const SOURCE = [
    '(display 1)',
    '(define foo-bar-baz 2)',
    '#|',
    '(define foo-bar 0)',
    '|#',
    '(define',
    '  foo-bar 1)',
    ''
  ].join('\n');

  it('finds the real definition, not the longer name, not the commented one', () => {
    assert.strictEqual(definitionLine(PREFIX + SOURCE, PREFIX.length, 'foo-bar'), 6);
  });

  it('answers the first source line when nothing defines the name', () => {
    assert.strictEqual(definitionLine(PREFIX + SOURCE, PREFIX.length, 'nowhere'), 1);
  });

  it('does not take a define inside a line comment or a string', () => {
    const text = PREFIX + '; (define foo-bar 0)\n(display "(define foo-bar 0)")\n(define (foo-bar x) x)\n';
    assert.strictEqual(definitionLine(text, PREFIX.length, 'foo-bar'), 3);
  });
});

describe('D3 several records are listed in the answer\'s order, and an export opens without a scan', () => {
  before(async () => {
    await initWire();
  });

  it('labels each by its library and its kind, or export, in order', async () => {
    const answer = await definitionsOf(answering(TWO_RECORDS), 'alpha');
    assert.ok('found' in answer);
    assert.deepStrictEqual(answer.found.map(recordLabel), ['(probe d) code', '(probe d) export']);
  });

  it('opens an export at the first line after the prefix, even where its text defines the name further down', () => {
    const exported: DefinitionRecord = { kind: 'export', id: 'b.1', library: '(probe d)', name: 'alpha', of: null };
    const text = '# (probe d)\n(library (probe d)\n  (export alpha)\n  (define alpha 2))\n';
    assert.strictEqual(targetLine(exported, text, '# (probe d)\n'.length), firstLineAfter(text, '# (probe d)\n'.length));
    assert.strictEqual(targetLine(exported, text, '# (probe d)\n'.length), 1);
  });
});

describe('D4 no definition, another refusal, and an incomplete answer', () => {
  before(async () => {
    await initWire();
  });

  it('reads the unknown-name refusal as none, with the nearest names the store gave as symbols', async () => {
    const answer = await definitionsOf(answering('(error unknown-name foo (nearest foo-bar baz))\n', 1), 'foo');
    assert.ok('none' in answer, JSON.stringify(answer));
    assert.deepStrictEqual(answer.nearest, ['foo-bar', 'baz']);
    assert.strictEqual(noDefinitionNotice('foo', answer.nearest), 'no definition of foo; nearest: foo-bar, baz.');
  });

  /*
   * A NAME DEFINED ONLY IN BLOCKS THAT ARE NOT IN FORCE. From theourgia
   * 06a348b `whereis` leaves out records in superseded and refuted blocks,
   * and a name all of whose records were left out answers no items with an
   * `excluded` clause, not `unknown-name`. The answer below is rpc.sc's
   * whereis for that case: the scan clauses, the excluded count, the
   * versions of nothing.
   */
  const LEFT_OUT = (superseded: number, refuted: number): string =>
    `(ok (items) (cut ()) (scanned (blocks ${superseded + refuted + 1}) (fields (names exports))) ` +
    `(excluded (blocks (superseded ${superseded}) (refuted ${refuted}))) (versions ()))\n`;

  it('says a name found only in a superseded block is there, and not in force', async () => {
    const answer = await definitionsOf(answering(LEFT_OUT(1, 0)), 'foo');
    assert.ok('none' in answer, JSON.stringify(answer));
    assert.deepStrictEqual(answer.leftOut, { superseded: 1, refuted: 0 });
    assert.strictEqual(
      noDefinitionNotice('foo', answer.nearest, answer.leftOut),
      'no definition of foo in force: it is found only in a block that is superseded.'
    );
  });

  it('names both when the blocks left out are superseded and refuted', async () => {
    const answer = await definitionsOf(answering(LEFT_OUT(1, 2)), 'foo');
    assert.ok('none' in answer, JSON.stringify(answer));
    assert.strictEqual(
      noDefinitionNotice('foo', answer.nearest, answer.leftOut),
      'no definition of foo in force: it is found only in blocks that are superseded or refuted.'
    );
  });

  it('says only that there is no definition when an empty answer has no excluded clause', async () => {
    const answer = await definitionsOf(answering('(ok (items) (cut ()) (scanned (blocks 2) (fields (names exports))) (versions ()))\n'), 'foo');
    assert.ok('none' in answer, JSON.stringify(answer));
    assert.strictEqual(answer.leftOut, null);
    assert.strictEqual(noDefinitionNotice('foo', answer.nearest, answer.leftOut), 'no definition of foo.');
  });

  it('hands any other refusal back as the failure it is', async () => {
    await assert.rejects(
      definitionsOf(answering('(error unavailable (reason schedule))\n', 1), 'foo'),
      (e: unknown) => e instanceof TransportError
    );
  });

  it('carries an incomplete answer\'s notes beside the records it found', async () => {
    const clause = '(incomplete (unreadable (writer "zzzzzzzz") (path "/s/writers/zzzzzzzz") (reason "Permission denied")))';
    const answer = await definitionsOf(answering(TWO_RECORDS.replace(')\n', ` ${clause})\n`)), 'alpha');
    assert.ok('found' in answer);
    assert.strictEqual(answer.found.length, 2);
    assert.deepStrictEqual(answer.notes, [{ writer: 'zzzzzzzz', path: '/s/writers/zzzzzzzz', reason: 'Permission denied' }]);
  });
});

describe('D5 whereis is a verb this client knows, and the old sentences are gone', () => {
  it('knows whereis, as an items answer', () => {
    assert.strictEqual(answerKind('whereis', ['alpha', '--wire']), 'items');
  });

  it('no longer says the core lacks whereis anywhere in src', () => {
    const src = path.join(__dirname, '..', '..', '..', 'src');
    const stale = fs
      .readdirSync(src)
      .filter((name) => name.endsWith('.ts'))
      .filter((name) => /does not have `?whereis`? yet|the entry appears the day the verb does/.test(fs.readFileSync(path.join(src, name), 'utf8')));
    assert.deepStrictEqual(stale, []);
  });
});

/*
 * THE HARNESS: the compiled extension against a stand-in VS Code API, with a
 * REAL core through the shipping transport. The editor host cannot read a
 * picker's rows, a provider's answer through its dispatch or a selection
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

describe('D6 the command lists the def and the export, and opens the chosen def at its line', function () {
  this.timeout(180000);
  it('opens the text block at the line of its definition', () => {
    const r = schedule('definition-command');
    assert.deepStrictEqual([...r.kinds].sort(), ['def', 'export'], `the picker listed ${JSON.stringify(r.labels)}`);
    assert.ok(r.labels.some((l: string) => / export$/.test(l)), JSON.stringify(r.labels));
    assert.ok(r.opened !== null && r.fileText !== null, 'nothing was opened');
    const line = String(r.fileText).split('\n').findIndex((l: string) => l.includes('(define alpha 1)'));
    assert.ok(line > 0, `the opened file does not hold the definition: ${r.fileText}`);
    assert.strictEqual(r.opened.line, line, 'the definition opened at another line');
    assert.deepStrictEqual(r.shown.filter((s: { level: string }) => s.level === 'error'), []);
  });
});

describe('D7 the editor\'s own Go to Definition answers through its dispatch, in both contexts', function () {
  this.timeout(180000);
  it('answers the block\'s file at the definition line from a block, from a document view, and one lower in a longer draft', () => {
    const r = schedule('definition-provider');
    assert.ok(r.defLine > 0, `the definition is on line ${r.defLine}`);
    assert.deepStrictEqual(r.inBlock, [{ fsPath: r.file, line: r.defLine }], 'from the block editor');
    assert.deepStrictEqual(r.inView, [{ fsPath: r.file, line: r.defLine }], 'from a document view');
    assert.deepStrictEqual(r.inDraft, [{ fsPath: r.file, line: r.defLine + 1 }], 'from a draft one line longer');
  });
});
