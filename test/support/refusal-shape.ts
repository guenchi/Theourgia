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
 * THE SHAPE U8 RULES FOR A REFUSAL, IN ONE PLACE.
 *
 * The ruling (theourgia main session, Q2): the data of any error answer
 * is wire-safe BY CONSTRUCTION. A refusal does not echo the raw intent;
 * it DESCRIBES it -- `(symbol-not-wire-safe (field rel) (spelling "1"))`
 * -- with the offending name given as a string, and the whole
 * `malformed-intent` family becomes position-plus-string.
 *
 * ⚠️ IT IS HERE, AND NOT INSIDE THE CELL, BECAUSE THE CELL THAT USES IT
 * IS RED. A red cell's expectation is checked by nothing: a stray space
 * or a wrong bracket in the pattern keeps it red for ever, and the day
 * the core lands the change the cell goes on failing while looking like
 * it is still waiting. So the same assertions are also run against a
 * written-out example of the ruled answer, which passes today -- that
 * witness is the only evidence that this expectation is satisfiable at
 * all, and that the red means what it says.
 *
 * ⚠️ AND IT READS THE ANSWER RATHER THAN MATCHING ITS TEXT. The first
 * version compared substrings, which a review showed to be wrong in both
 * directions at once. Too weak: independent matches for
 * `symbol-not-wire-safe`, `(field rel)` and `(spelling "1")` were
 * satisfied by an answer of a DIFFERENT error family, and by one that
 * put the position and the spelling outside the reason instead of
 * inside it -- neither establishes the tags or their relationship. Too
 * strong: `(field  rel)` with two spaces is the same datum and failed,
 * and a blanket ban on hex escapes rejected `(spelling "\x31;")`, which
 * carries the spelling as a STRING and is exactly what the ruling asks
 * for. Wire-safe is a property of the datum, not of its spacing, and it
 * does not mean "contains no escape sequence".
 */
import * as assert from 'assert';
import { Datum, headName, isList, isSym, parseAnswers } from '../../src/wire';

function clause(value: Datum, name: string): Datum[] | null {
  if (!isList(value)) {
    return null;
  }
  for (const item of value) {
    if (isList(item) && headName(item) === name) {
      return item;
    }
  }
  return null;
}

/*
 * The answer as a datum, read the way the client reads it. Passing text
 * rather than a datum is deliberate: what the core prints is what has to
 * be legible, and a caller that handed over an already-parsed value
 * would be asking a question that cannot fail.
 */
export function assertRuledRefusal(text: string, name: string, field: string): void {
  let data: Datum[];
  try {
    data = parseAnswers(text.endsWith('\n') ? text : `${text}\n`);
  } catch (e) {
    assert.fail(`the refusal could not be read by the wire reader: ${(e as Error).message}`);
    return;
  }
  assert.strictEqual(data.length, 1, `the refusal is not one datum: ${text}`);
  const answer = data[0];
  assert.strictEqual(headName(answer), 'error', `the refusal is not an error answer: ${text}`);
  assert.ok(isList(answer) && answer.length >= 2, `the error answer carries no family: ${text}`);
  /*
   * THE FAMILY IS PART OF IT. A refusal of the right shape under the
   * wrong family is a different answer, and substring matching accepted
   * one.
   */
  assert.ok(
    isList(answer) && isSym(answer[1], 'malformed-intent'),
    `the refusal is not in the malformed-intent family: ${text}`
  );
  const reason = clause(answer, 'symbol-not-wire-safe');
  assert.ok(reason !== null, `the refusal does not say why: ${text}`);
  /*
   * AND THE POSITION AND THE SPELLING ARE INSIDE THE REASON. Beside it
   * is a different answer: the reason would then be a bare tag and the
   * two clauses would belong to the error rather than to it.
   */
  const where = clause(reason, 'field');
  assert.ok(where !== null, `the refusal does not say which position was wrong: ${text}`);
  assert.ok(
    where !== null && where.length === 2 && isSym(where[1], field),
    `the position is not the symbol ${field}: ${text}`
  );
  const spelling = clause(reason, 'spelling');
  assert.ok(spelling !== null, `the refusal does not carry the spelling: ${text}`);
  /*
   * THE NAME COMES BACK AS A STRING, and it is compared AFTER decoding.
   * That is the whole of U8: a symbol a writer would have to escape is
   * described rather than echoed, so the answer stays readable. How the
   * string was spelled on the wire -- plainly or with escapes -- is the
   * writer's business and not this check's.
   */
  assert.ok(
    spelling !== null && spelling.length === 2 && typeof spelling[1] === 'string',
    `the spelling is not given as a string: ${text}`
  );
  assert.strictEqual(
    spelling !== null ? spelling[1] : undefined,
    name,
    `the refusal names a different spelling: ${text}`
  );
}

/*
 * THE RULING'S OWN EXAMPLE, WRITTEN OUT. The witness cell reads this
 * with the wire reader and runs the assertions above on it.
 */
export const RULED_EXAMPLE =
  '(error malformed-intent (symbol-not-wire-safe (field rel) (spelling "1")))';

/*
 * AND WHAT THE CORE ANSWERS TODAY, for the negative witness. Measured
 * against pin 9ecbd88e on 2026-09-12; the wire reader refuses it, which
 * is the defect U8 settles.
 */
export const ESCAPED_ECHO =
  '(error malformed-intent (symbol-not-wire-safe (link "a.1" \\x31; "a.2")))';
