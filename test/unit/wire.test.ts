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
 * P2 and P4: what happens to an answer this cannot read, and what a
 * large one costs.
 *
 * P2 IS HERE BECAUSE A LINE THAT WILL NOT PARSE HAS TO STOP THE ANSWER.
 * The fixture in vendor-sexpr.test.ts pins the reader; this pins what
 * the layer above it does with a refusal, which is the part a `continue`
 * would quietly break -- and did: the main session's mutation of exactly
 * that line survived a whole suite.
 */

import * as assert from 'assert';
import { AnswerParseError, initWire, parseAnswers, wire } from '../../src/wire';

describe('P2 an answer line that cannot be read stops the answer', () => {
  before(async () => {
    await initWire();
  });

  it('refuses the whole answer rather than returning the lines that parsed', () => {
    const stdout = ['(orphan "a.1")', '(orphan "a.2"', '(orphan "a.3")'].join('\n');
    let caught: unknown = null;
    try {
      parseAnswers(stdout);
    } catch (e) {
      caught = e;
    }
    assert.ok(caught instanceof AnswerParseError, `threw ${(caught as Error)?.name}`);
    assert.strictEqual((caught as AnswerParseError).line, 2, 'the failing line was not named');
    assert.ok((caught as AnswerParseError).position >= 0, 'no position came through');
    assert.strictEqual((caught as AnswerParseError).text, '(orphan "a.2"');
  });

  it('refuses a bad line even when it is the last one', () => {
    assert.throws(
      () => parseAnswers('(orphan "a.1")\n(orphan\n'),
      (e: unknown) => e instanceof AnswerParseError && (e as AnswerParseError).line === 2
    );
  });

  it('refuses a bad line even when it is the first one', () => {
    assert.throws(
      () => parseAnswers('(orphan\n(orphan "a.2")\n'),
      (e: unknown) => e instanceof AnswerParseError && (e as AnswerParseError).line === 1
    );
  });

  it('reads a blank line as nothing rather than as a datum', () => {
    assert.strictEqual(parseAnswers('(orphan "a.1")\n\n(orphan "a.2")\n').length, 2);
    assert.deepStrictEqual(parseAnswers(''), []);
    assert.deepStrictEqual(parseAnswers('\n\n'), []);
  });

  it('refuses two data on one line, because one line is one item', () => {
    assert.throws(() => parseAnswers('(orphan "a.1") (orphan "a.2")\n'), AnswerParseError);
  });
});

describe('P4 a large answer is read whole', () => {
  before(async () => {
    await initWire();
  });

  it('reads a two-megabyte body without losing a character of it', () => {
    const body = 'x'.repeat(2 * 1024 * 1024);
    const answers = parseAnswers(`(ok ((id . "a.2") (fields (src . "${body}"))))\n`);
    assert.strictEqual(answers.length, 1);
    const fields = ((answers[0] as unknown[])[1] as unknown[])[1] as unknown[];
    const src = (fields[1] as { tail: string }).tail;
    assert.strictEqual(src.length, body.length);
  });

  it('reads a two-megabyte body that is mostly escapes', () => {
    const body = 'a\\n'.repeat(300000);
    const answers = parseAnswers(`(ok (text "${body}"))\n`);
    const text = (answers[0] as unknown[])[1] as unknown[];
    assert.strictEqual((text[1] as string).length, 600000);
    assert.strictEqual((text[1] as string).slice(0, 4), 'a\na\n');
  });

  it('reads a thousand items in one answer', () => {
    const lines: string[] = [];
    for (let i = 0; i < 1000; i += 1) {
      lines.push(`(hit "a.${i}" 1 "snippet ${i}")`);
    }
    const answers = parseAnswers(`${lines.join('\n')}\n`);
    assert.strictEqual(answers.length, 1000);
    assert.strictEqual((answers[999] as unknown[])[1], 'a.999');
  });

  it('refuses a datum nested past the depth the authority reads to', () => {
    const depth = wire().MAX_DEPTH + 2;
    assert.throws(
      () => parseAnswers('('.repeat(depth) + ')'.repeat(depth)),
      AnswerParseError,
      'a datum too deep for the peer was read as though it were fine'
    );
  });
});
