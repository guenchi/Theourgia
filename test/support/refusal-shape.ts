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
 */
import * as assert from 'assert';

export function assertRuledRefusal(text: string, name: string, field: string): void {
  assert.match(text, /symbol-not-wire-safe/, `the refusal does not say why: ${text}`);
  assert.match(
    text,
    new RegExp(`\\(field ${field}\\)`),
    `the refusal does not say which position was wrong: ${text}`
  );
  /*
   * THE NAME COMES BACK AS A STRING. That is the whole of U8: a symbol a
   * writer would have to escape is described rather than echoed, so the
   * answer stays readable. Asserting only that the name appears
   * somewhere would pass against the escaped echo this replaces.
   */
  assert.ok(
    text.includes(`(spelling ${JSON.stringify(name)})`),
    `the refusal does not carry the spelling as a string: ${text}`
  );
  /*
   * AND NOTHING IN THE ANSWER NEEDED AN ESCAPE. A caller that got this
   * far already has an answer that parsed; this says the core did not
   * get there by escaping something a later reader would refuse.
   */
  assert.strictEqual(
    /\\x[0-9a-fA-F]+;/.test(text),
    false,
    `the refusal carries an escape the wire reader turns away: ${text}`
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
