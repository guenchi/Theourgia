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

  /*
   * THE CONTENT, NOT ONLY ITS LENGTH. A body of two million identical
   * characters is the one shape where a length check proves nothing --
   * any corruption that preserves the count passes it. The body here is
   * varied so that its bytes have to arrive in order, and it is compared
   * whole.
   */
  it('reads a two-megabyte body without losing or reordering a character of it', () => {
    /*
     * EVERY SEGMENT CARRIES ITS OWN INDEX, so exchanging two equal-length
     * stretches is visible. A repeated unit is not enough: the comparison
     * is whole-string, but two identical periods swapped produce the same
     * string, which is exactly the corruption a repeating body hides.
     */
    const segments: string[] = [];
    let built = 0;
    for (let i = 0; built < 2 * 1024 * 1024; i += 1) {
      const segment = `[${i}]abcdefghij`;
      segments.push(segment);
      built += segment.length;
    }
    const body = segments.join('');
    const answers = parseAnswers(`(ok ((id . "a.2") (fields (src . "${body}"))))\n`);
    assert.strictEqual(answers.length, 1);
    const fields = ((answers[0] as unknown[])[1] as unknown[])[1] as unknown[];
    const src = (fields[1] as { tail: string }).tail;
    assert.strictEqual(src.length, body.length);
    assert.strictEqual(src, body, 'the body came back the right length and the wrong bytes');
  });

  it('reads a body of two hundred thousand escapes, and reads all of it', () => {
    /*
     * Each unit carries its own index, so a reader that dropped,
     * duplicated or reordered one cannot produce the same string. An
     * earlier version repeated a single pair and checked the length and
     * the first four characters, which every such fault survives; the
     * version after it compared the whole string but still repeated
     * every 26 units, so an exchange of aligned stretches was invisible.
     *
     * THE COUNT IS PART OF THE FIXTURE, NOT AN INCIDENTAL NUMBER. The
     * round that added the indexes also cut this loop from 200000 to
     * 50000, which reads as a tidy-up and is not one: a reader that
     * fails after 65535 escapes -- a counter that is narrower than it
     * looks -- passes at 50000 and fails at 200000. The two-megabyte
     * fixture does not stand in for it, because its body holds no
     * escapes at all.
     */
    const units: string[] = [];
    const expected: string[] = [];
    for (let i = 0; i < 200000; i += 1) {
      /*
       * The index goes into the text beside the escape, so no two units
       * are alike and a swapped pair changes the string.
       */
      units.push(`${i}a\\n`);
      expected.push(`${i}a\n`);
    }
    const answers = parseAnswers(`(ok (text "${units.join('')}"))\n`);
    const text = (answers[0] as unknown[])[1] as unknown[];
    assert.strictEqual(text[1], expected.join(''), 'the escaped body did not come back intact');
  });

  it('reads a thousand items in one answer, and each one is its own', () => {
    const lines: string[] = [];
    for (let i = 0; i < 1000; i += 1) {
      lines.push(`(hit "a.${i}" 1 "snippet ${i}")`);
    }
    const answers = parseAnswers(`${lines.join('\n')}\n`);
    assert.strictEqual(answers.length, 1000);
    /*
     * THE INTERIOR, NOT THE LENGTH AND THE LAST ONE. Checking those two
     * accepts a reader that filled the first 999 places with copies of
     * anything, which is what a reused buffer would produce.
     */
    assert.deepStrictEqual(
      answers.map((a) => [(a as unknown[])[1], (a as unknown[])[3]]),
      lines.map((_, i) => [`a.${i}`, `snippet ${i}`]),
      'the items came back the right number and the wrong contents'
    );
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
