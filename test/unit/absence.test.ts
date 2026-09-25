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
 * AN INABILITY MUST NOT BE ANSWERED AS AN ABSENCE.
 *
 * KEY: WHY THIS IS A CENSUS AND NOT A LIST OF REPAIRS. Thirteen review
 * rounds found this one shape in twelve places, and each repair went in
 * where its finding pointed: `entriesIn`, `FakeCore.calls`,
 * `countRealRunRoot`, the process readers, `fsops.list`, then
 * `fsops.isDirectory` four lines below `list` one round later, then
 * `ownerOf`, `hasUncommittedWork`, `Sessions.pendingIn`,
 * `pendingForDirectory`, the sidecar's outstanding records, and the
 * refusal-name reader in the settler. The thirteenth instance was only
 * ever a matter of time, and a review round cannot converge on it. This
 * is the supplier for that: a new one is red here on the day it is
 * written.
 *
 * WHAT IS FORBIDDEN, exactly: a `catch` in `src/` that answers with a
 * falsy or empty value while doing nothing else -- not re-raising, not
 * reporting to the user, not recording that it could not look. Those
 * three are what turn "I could not" into something a caller can act on;
 * a bare `return null` turns it into "there is nothing", which is the
 * reassuring answer given at the one moment nothing is known.
 *
 * NOTE: THE SCOPE IS `src/` AND ONLY `src/`. Tests and fixtures catch and
 * answer for good reasons -- a teardown that must not fail the cell it
 * is cleaning up after, a probe that is asking whether something is
 * there. A census that reached into them would have this line's author
 * editing cells to make a census green, which is the wrong way round.
 * The fixtures have their own guards and their own cells.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';
import {
  callsOneOf,
  cannotFinishQuietly,
  namesOfTheFailure,
  onlyWhenAbsent,
  returnsIn,
  throwsSomewhere
} from '../support/catches';

/*
 * THE EXEMPTIONS, EACH WITH ITS REASON READ FROM THE CODE.
 *
 * A key is `<file>:<the catch's line>`. Every one of these was looked at
 * when this census was written; the reason says why answering with that
 * value is not an absence dressed up as a fact. An exemption with no
 * reason is not an exemption, and neither is one nobody re-reads: the
 * line number moves when the file does, so a stale entry fails the
 * census below rather than silently covering something else.
 */
const ANSWERING_IS_RIGHT: Record<string, string> = {
  'documents.ts:150':
    'answers TRUE -- "this file may hold work". A document with no marker was written by ' +
    'something this version does not know about, and the safe answer is the one that stops it ' +
    'being overwritten.',
  'extension.ts:197':
    'answers an empty list to the TREE, and reports through `failed` -- except when the store ' +
    'changed under it, which is the one case that skips the report. The reporting call is ' +
    'conditional, so the rule no longer exempts it; the condition is `asked === generation()`, ' +
    'and when that is false nothing this listing drew is on the screen to be wrong about.',
  'fsops.ts:138':
    'the directory flush. Some filesystems refuse it and the bytes are already down by then, so ' +
    'a failure here is indistinguishable from a refusal and neither stops the save. Said at the ' +
    'site and in the README.',
  'fsops.ts:296':
    'answers FALSE for "the core directory holds this file". A directory this process may not ' +
    'search then reads as neither form, which `problemsWith` refuses by name -- so the failure ' +
    'reaches the user as a refusal about the setting rather than as a wrong extension list.',
  'model.ts:712':
    'answers NULL, which `verbs` documents as "I could not find out" and which its caller must ' +
    'tell apart from a core with no verbs. The absence has a name here.',
  'ownership.ts:111':
    'answers NULL for an owner record that will not parse, and the caller turns that into ' +
    '`{known: false}` one level up -- "I do not know who owns this", never "nobody does".',
  'sessions.ts:68':
    'answers NULL for a start time that cannot be read, and the identity that uses it is then ' +
    'undecidable rather than matching. Named absence.',
  'sessions.ts:1316':
    'answers nothing, and writes `unreadableQueue` into the takeover ledger first. The ledger ' +
    'is the survey\'s whole output, so the failure is not lost -- it is the finding. A throw ' +
    'here would end a survey of every OTHER window over one file that will not read.',
  'sessions.ts:1351':
    'as sessions.ts:1316, on the import rather than the survey: `unreadableQueue` is counted ' +
    'and the file is left untouched, because the queue holds the only copy of that window\'s ' +
    'unsent work and a takeover that repaired it would write over what it came to rescue.',
  'sessions.ts:893':
    'answers TRUE -- "a relevant send may be in flight". A migration is refused rather than ' +
    'permitted over work nobody could read.',
  'saving.ts:389':
    'answers NULL from `cleanliness`, whose contract -- written at the function -- is that null ' +
    'means "cannot say" and never "the file is clean". Its caller knows how to hold that; the ' +
    'one thing it must not do is fail the save it was asked about.',
  'saving.ts:404': 'as saving.ts:389 -- the sidecar would not read, and null is "cannot say".',
  'saving.ts:413': 'as saving.ts:389 -- the file would not read, and null is "cannot say".',
  'saving.ts:419': 'as saving.ts:389 -- the bytes are not UTF-8, and null is "cannot say".'
};

const FALSY = new Set(['null', 'undefined', 'false', 'true', '0', "''", '""', '[]', '{}']);

/*
 * A CATCH THAT DOES SOMETHING ABOUT IT. Re-raising, telling the user, or
 * writing down that the look failed all leave the caller able to act;
 * the census is about the ones that do none of the three.
 *
 * NEVER: THE WORDS ARE LOOKED FOR IN THE CODE, NOT IN THE SOURCE TEXT.
 *
 * The first version of this tested the catch's whole source against a
 * regular expression, and a comment satisfied it: measured in a
 * fourteenth review round, `return false; // throw e;` passed all four
 * cells while the reader went on answering false for every failure.
 *
 * That is the SECOND census in this delivery to read prose -- the `ps`
 * one did the same thing a round earlier and was repaired for it, and
 * this file was written in the same batch and made the same mistake.
 * Writing it down here rather than only fixing it: a census over source
 * is a census that has to be asked what it is looking at.
 */
const SPEAKS = ['reportFailure', 'reject', 'failed', 'unreadableQueue', 'ledger', 'say', 'show'];

/*
 * WHETHER A CATCH RE-RAISES OR REPORTS, asked of the parser. A `throw`
 * statement is a node; a call is a node; a comment is neither.
 */
/*
 * NEVER: ONE SPEAKING PATH IS NOT EVERY PATH, AND A PROPERTY IS NOT A
 * CALL.
 *
 * Two ways past the first version, both measured in a fifteenth review
 * round: `if (code === 'EIO') throw e; return false;` satisfied it with
 * a throw that fires for one errno and a falsy answer for all the
 * others; and `void (e as {show?: unknown}).show; return false;`
 * satisfied it by MENTIONING a reporting name in a property access that
 * calls nothing.
 *
 * So: a reporting name counts only as the thing being CALLED, and a
 * catch that both speaks and answers falsy is treated as answering --
 * because the answer is what its caller sees.
 */
/*
 * THE THREE SHAPES THAT ARE NOT AN ABSENCE, stated rather than felt.
 *
 * A fifteenth review round pushed on "does it speak?" until the question
 * had to be made exact. There are three ways a catch can answer with a
 * falsy value and still leave its caller able to act:
 *
 *   (a) it never answers at all -- every path throws or reports;
 *   (b) every answer it gives is guarded by `code === 'ENOENT'`, and
 *       some other path throws. That is the pattern this delivery
 *       installed in eight readers: an absent thing really is absent,
 *       and every other failure is refused;
 *   (c) it tells somebody -- `reportFailure`, `reject`, a ledger entry
 *       -- and then answers. The caller is not the only one who finds
 *       out.
 *
 * Anything else answers "there is nothing here" for a question it could
 * not put, and belongs in the table with a reason.
 *
 * NOTE: THE THREE QUESTIONS ARE ASKED BY `test/support/catches.ts`,
 * which the `ps` census calls as well. They used to be two rules for one
 * question, and a mutation got past each of them in a shape the other
 * would have caught.
 */
function leavesTheCallerAble(clause: ts.CatchClause, source: ts.SourceFile): boolean {
  const block = clause.block;
  const answers = returnsIn(block);
  if (answers.length === 0) {
    return throwsSomewhere(block) || callsOneOf(block, source, SPEAKS);
  }
  if (callsOneOf(block, source, SPEAKS)) {
    return true;
  }
  /*
   * NEVER: AND SHAPE (b) HAS TO REFUSE EVERY FAILURE IT DOES NOT ANSWER
   * FOR.
   *
   * This asked only that a throw existed somewhere. A sixteenth review
   * round wrote a catch that answers false for ENOENT, throws for EIO
   * and FALLS THROUGH for everything else -- with the function
   * answering false after the catch. Every guarded return is guarded and
   * every other failure still gets an answer, through the exit the
   * question did not cover. `cannotFinishQuietly` is the question that
   * covers it, and it was sitting in the same file, used by the other
   * census.
   */
  if (!cannotFinishQuietly(block)) {
    return false;
  }
  const failure = namesOfTheFailure(clause, source);
  return answers.every((answer) => onlyWhenAbsent(answer, block, source, failure));
}

export interface SilentCatch {
  where: string;
  answers: string;
}

export function silentCatches(root: string): SilentCatch[] {
  const out: SilentCatch[] = [];
  for (const name of fs.readdirSync(path.join(root, 'src')).filter((f) => f.endsWith('.ts'))) {
    const file = path.join(root, 'src', name);
    const text = fs.readFileSync(file, 'utf8');
    const parsed = ts.createSourceFile(name, text, ts.ScriptTarget.Latest, true);
    const visit = (node: ts.Node): void => {
      if (ts.isCatchClause(node)) {
        const answers: string[] = [];
        const walk = (inner: ts.Node): void => {
          if (ts.isReturnStatement(inner)) {
            /*
             * NEVER: AND THE PARENTHESES ARE NOT PART OF THE VALUE.
             *
             * `return (false);` has the expression text `(false)`, which
             * is not in the table below, so the whole catch left the
             * census -- measured in a sixteenth review round on
             * `isDirectory`, where all four cells passed a reader that
             * answers false for every failure. The parser knows what is
             * inside the brackets.
             */
            let value: ts.Expression | undefined = inner.expression;
            while (value !== undefined && ts.isParenthesizedExpression(value)) {
              value = value.expression;
            }
            answers.push(value === undefined ? 'undefined' : value.getText(parsed));
          }
          /*
           * NOTE: A FUNCTION DECLARED INSIDE THE CATCH IS NOT THIS CATCH'S
           * ANSWER. Its `return` belongs to it.
           */
          if (!ts.isFunctionLike(inner)) {
            inner.forEachChild(walk);
          }
        };
        node.block.forEachChild(walk);
        const falsy = answers.filter((a) => FALSY.has(a.trim()));
        /*
         * NOTE: A CATCH THAT SPEAKS **AND** ANSWERS FALSY STILL ANSWERS
         * FALSY. What its caller sees is the value; a throw on one
         * branch does not change what the other branch hands back.
         */
        if (falsy.length > 0 && !leavesTheCallerAble(node, parsed)) {
          out.push({
            where: `${name}:${parsed.getLineAndCharacterOfPosition(node.getStart(parsed)).line + 1}`,
            answers: falsy.join(' | ')
          });
        }
      }
      node.forEachChild(visit);
    };
    visit(parsed);
  }
  return out;
}

describe('plugin-r2 an inability is not answered as an absence', function () {
  const root = path.join(__dirname, '..', '..', '..');

  /*
   * THE CENSUS'S OWN FIRST READING. A walk that found no catch at all
   * would pass this file for ever while looking at nothing -- and this
   * one is a walk over a shape, so it is worth saying how many it sees.
   */
  it('finds the catches it is about', function () {
    const found = silentCatches(root);
    assert.ok(
      found.length >= 5,
      `the walk found ${found.length} silent catches in src, which is too few to be reading the ` +
        'files; the exemption table alone names eight'
    );
  });

  it('answers a failure with a falsy value only where that is written down', function () {
    const unexplained = silentCatches(root)
      .filter((c) => ANSWERING_IS_RIGHT[c.where] === undefined)
      .map((c) => `${c.where} answers ${c.answers}`);
    assert.deepStrictEqual(
      unexplained,
      [],
      'these catches turn a failure into "there is nothing here" without saying why that is the ' +
        'right answer. Either let the failure out, tell the user, record that the look failed, ' +
        'or add a line to ANSWERING_IS_RIGHT saying what makes the falsy answer true.'
    );
  });

  /*
   * KEY: AND AN EXEMPTION FOR A CATCH THAT IS NO LONGER THERE IS A LIE
   * THAT READS LIKE DILIGENCE. The line numbers move when the files do,
   * so a stale entry has to fail rather than sit.
   */
  it('has no exemption for a catch that is not there any more', function () {
    const present = new Set(silentCatches(root).map((c) => c.where));
    assert.deepStrictEqual(
      Object.keys(ANSWERING_IS_RIGHT).filter((where) => !present.has(where)),
      [],
      'these exemptions name catches that have moved or gone; re-read the code and move them'
    );
  });

  it('gives a reason for every exemption', function () {
    assert.deepStrictEqual(
      Object.entries(ANSWERING_IS_RIGHT)
        .filter(([, why]) => why.trim().length < 40)
        .map(([where]) => where),
      [],
      'an exemption whose reason is a few words is one nobody can check'
    );
  });
});
