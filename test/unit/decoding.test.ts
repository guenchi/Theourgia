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
 * THE DECODER, AND THE TWO WAYS ROUND IT.
 *
 * ⭐ THE COMPILER IS THE CENSUS FOR THE FIRST WAY. `clause`,
 * `clauseRest`, `clauseValue` and `headName` are no longer exported from
 * `wire.ts`, so there is no way to take a clause out of an answer
 * without naming the head it must have. Thirteen review rounds found
 * that defect in nine readers, one at a time; it is now something a
 * reader cannot be written with, and nothing here has to check for it.
 *
 * WHAT IS LEFT TO CHECK is the door held open for the two readers that
 * have no head to name -- `clauseOfRecord` and `recordValue`. A block
 * record reads `((id . "a.1") (deleted . #f) ...)` and a writer entry
 * reads `("local" (end 7))`: their first element is a pair or a name,
 * not a head, and both are values taken from a form whose head HAS been
 * checked. That is what makes reading them safe, and it is only true
 * where those values come from. So the doors are pinned to the files
 * that hold those readers.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';
import { Form, answerOf, initWire, parseAnswers } from '../../src/wire';
import { readBlock } from '../../src/blocks';
import { firstCursorFromCheck } from '../../src/cursor';
import { hitsOf } from '../../src/search';

/*
 * WHERE A HEADLESS READER IS ALLOWED, AND WHY EACH ONE IS THERE.
 */
const MAY_READ_A_RECORD: Record<string, string> = {
  'blocks.ts':
    'reads a block record, whose first element is `(id . "a.1")` -- a pair, not a head. The ' +
    'record has already come out of a form whose head was checked.',
  'cursor.ts':
    'reads a writer entry out of a `check` listing, `("local" (end 7))`, whose first element is ' +
    "the writer's name. The listing came out of a form whose head was checked."
};

/*
 * WHICH FILES CALL A THING EXPORTED FROM `wire.ts`, WHATEVER THEY CALL IT
 * LOCALLY.
 *
 * ⛔ MATCHING THE CALL SITE'S IDENTIFIER IS NOT ENOUGH. An import may
 * rename what it brings in -- `import { clauseOfRecord as unchecked }`
 * -- and a census that looks for the original name then sees nothing.
 * Measured in a fourteenth review round: with the envelope's items
 * clause read through such an alias, the whole tree type-checked and
 * every cell in this file passed. The defect the decoder exists to
 * prevent had come back through a door the census could not see.
 *
 * So the import is read first, the local name is taken from it, and the
 * calls are counted under that name.
 */
function callsIn(root: string, name: string): string[] {
  const out: string[] = [];
  for (const file of fs.readdirSync(path.join(root, 'src')).filter((f) => f.endsWith('.ts'))) {
    const text = fs.readFileSync(path.join(root, 'src', file), 'utf8');
    const parsed = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
    /*
     * ⛔ A NAMED IMPORT IS NOT THE ONLY WAY IN.
     *
     * The first version of this followed named imports and direct calls,
     * and a fifteenth review round walked past it three ways: a
     * namespace import (`rawWire.clauseOfRecord`), a local alias
     * (`const unchecked = clauseOfRecord`), and a re-export from another
     * module. Each type-checked and each left this census green.
     *
     * So the question is asked of the TEXT of the call as the parser
     * prints it, and of every local name that the original can flow
     * into: the import's own name, a namespace member access, and any
     * variable initialised from one of those. A fourth way will need a
     * fourth clause, and this comment is where it goes.
     */
    const local = new Set<string>();
    const namespaces = new Set<string>();
    const importsOf = (node: ts.Node): void => {
      if (ts.isImportDeclaration(node) && ts.isStringLiteral(node.moduleSpecifier)) {
        const bindings = node.importClause?.namedBindings;
        if (bindings !== undefined && ts.isNamedImports(bindings)) {
          for (const element of bindings.elements) {
            if ((element.propertyName?.text ?? element.name.text) === name) {
              local.add(element.name.text);
            }
          }
        }
        if (bindings !== undefined && ts.isNamespaceImport(bindings)) {
          namespaces.add(bindings.name.text);
        }
      }
      node.forEachChild(importsOf);
    };
    importsOf(parsed);

    /*
     * A VARIABLE INITIALISED FROM ONE OF THOSE NAMES IS ONE OF THOSE
     * NAMES. Repeated until nothing new is found, because an alias of an
     * alias is still an alias.
     */
    for (let round = 0; round < 8; round += 1) {
      const before = local.size;
      const aliases = (node: ts.Node): void => {
        if (ts.isVariableDeclaration(node) && node.initializer !== undefined) {
          const from = node.initializer.getText(parsed).trim();
          const carries = local.has(from) || [...namespaces].some((ns) => from === `${ns}.${name}`);
          if (ts.isIdentifier(node.name) && carries) {
            local.add(node.name.text);
          }
          /*
           * ⛔ AND A DESTRUCTURING IS A DECLARATION TOO.
           *
           * `const {clauseOfRecord: unchecked} = wireModule;` binds the
           * same function to a new name without ever writing an
           * identifier this walk was looking at. Measured in a sixteenth
           * review round -- the FOURTH way in, past a census whose own
           * comment says a fourth clause goes here. It went here.
           *
           * The pattern is read whether the initialiser is a namespace
           * or a name already known to carry the function: in the first
           * case the property must BE the name, in the second any
           * property of it already is.
           */
          if (ts.isObjectBindingPattern(node.name)) {
            const fromNamespace = [...namespaces].some((ns) => from === ns);
            for (const element of node.name.elements) {
              const property = element.propertyName ?? element.name;
              const asked = ts.isIdentifier(property) ? property.text : null;
              if (!ts.isIdentifier(element.name)) {
                continue;
              }
              if ((fromNamespace && asked === name) || (carries && asked === name)) {
                local.add(element.name.text);
              }
            }
          }
        }
        node.forEachChild(aliases);
      };
      aliases(parsed);
      if (local.size === before) {
        break;
      }
    }

    const visit = (node: ts.Node): void => {
      if (ts.isCallExpression(node)) {
        const called = node.expression.getText(parsed).trim();
        if (local.has(called) || [...namespaces].some((ns) => called === `${ns}.${name}`)) {
          out.push(file);
        }
      }
      node.forEachChild(visit);
    };
    visit(parsed);
  }
  return out;
}

describe('plugin-r2 an answer is read through the decoder', function () {
  const root = path.join(__dirname, '..', '..', '..');

  before(async () => {
    await initWire();
  });

  /*
   * THE CENSUS'S OWN FIRST READING. `answerOf` has to be in use, or the
   * two cells below are about a door nobody walks through.
   */
  it('is what the readers use', function () {
    const users = new Set(callsIn(root, 'answerOf'));
    assert.ok(
      users.size >= 6,
      `only ${users.size} files in src decode an answer, which is fewer than the readers this ` +
        'repair converted; the decoder has been routed around'
    );
  });

  it('lets a headless reader be called only where a record has no head', function () {
    const strays = [
      ...new Set([
        ...callsIn(root, 'clauseOfRecord'),
        ...callsIn(root, 'recordValue'),
        ...callsIn(root, 'assocTail')
      ])
    ]
      .filter((file) => file !== 'wire.ts' && MAY_READ_A_RECORD[file] === undefined);
    assert.deepStrictEqual(
      strays,
      [],
      'these files read a clause without naming a head, and the values they read are not the ' +
        'headless kind. Decode the answer with `answerOf` instead, or add a line to ' +
        'MAY_READ_A_RECORD saying what has no head there and where it came from.'
    );
  });

  it('has no allowance for a file that no longer reads a record', function () {
    const present = new Set([
      ...callsIn(root, 'clauseOfRecord'),
      ...callsIn(root, 'recordValue'),
      ...callsIn(root, 'assocTail')
    ]);
    assert.deepStrictEqual(
      Object.keys(MAY_READ_A_RECORD).filter((file) => !present.has(file)),
      [],
      'these allowances name files that have stopped reading a headless record; remove them'
    );
  });
});

describe('plugin-r2 what the decoder refuses', function () {
  before(async () => {
    await initWire();
  });

  const datum = (text: string): unknown => parseAnswers(text)[0];

  /*
   * ⛔ `!== null` IS NOT "A FORM CAME BACK".
   *
   * Measured in a fifteenth review round: returning `undefined` after a
   * successful check left every positive cell here passing, because
   * each asked only that the answer was not null. A decoder that
   * answers undefined has decoded nothing, and the readers that use it
   * would take that as a refusal.
   */
  const isForm = (value: Form | null): boolean =>
    value !== null && value !== undefined && typeof value.clause === 'function';

  it('gives nothing for a form whose head is not the one named', function () {
    assert.strictEqual(answerOf(datum('(garbage (items))'), 'ok'), null);
    assert.strictEqual(answerOf(datum('(error store-not-found)'), 'ok'), null);
    assert.strictEqual(answerOf(datum('"a string"'), 'ok'), null);
  });

  it('gives a form for the head it was named, and its clauses', function () {
    const form = answerOf(datum('(ok (items (hit "a.1" 6 "k")) (behind (("w" . 2))))'), 'ok');
    assert.ok(isForm(form));
    /*
     * ⭐ WHAT IS IN THE CLAUSE, not how many things. Measured in a
     * fourteenth review round: a `clause` that answered `[null]` passed
     * a cell asking only for a length of one.
     */
    const found = (form as Form).clause('items');
    assert.ok(found.read, 'the decoder did not find the one clause that is there');
    const items = found.items;
    assert.strictEqual(items.length, 1);
    assert.deepStrictEqual(
      items[0],
      parseAnswers('(hit "a.1" 6 "k")')[0],
      'the clause came back with something other than what was in it'
    );
    /*
     * ⭐ AND AN ABSENT CLAUSE SAYS WHICH KIND OF NOTHING IT IS. A
     * sixteenth review round found three callers reading "there are two
     * of these" as "there is no such clause", so the two are separate
     * answers now and a cell that accepted either would be back where
     * that started.
     */
    assert.deepStrictEqual((form as Form).clause('nothing-of-the-sort'), {
      read: false,
      because: 'absent'
    });
    const twice = answerOf(datum('(ok (items 1) (items 2))'), 'ok');
    assert.ok(isForm(twice));
    assert.deepStrictEqual((twice as Form).clause('items'), {
      read: false,
      because: 'duplicated'
    });
  });

  /*
   * ⭐ `whole` KEEPS THE NAME AND `clause` DROPS IT, and the difference
   * is not cosmetic: a working projection's identity is the digest of
   * its whole clause. A refactor that quietly changed which one a reader
   * got would change every id this build computes.
   */
  it('tells the clause from the clause with its name on it', function () {
    const form = answerOf(datum('(ok (current "h"))'), 'ok') as Form;
    assert.deepStrictEqual(form.clause('current'), { read: true, items: ['h'] });
    const kept = form.whole('current');
    assert.ok(kept.read, 'the whole clause was not found');
    const whole = kept.items;
    /*
     * ⭐ THE WHOLE CLAUSE, AND NOTHING MORE. Measured in a fifteenth
     * review round: appending an element to every clause left a cell
     * reading only elements 0 and 1 passing, and `(current "h" 99)`
     * was accepted as the whole of `(current "h")` -- which goes into
     * the digest that is a projection's identity.
     */
    assert.strictEqual(whole.length, 2, `the clause came back as ${JSON.stringify(whole)}`);
    assert.strictEqual(whole[1], 'h');
    /*
     * ⭐ AND THE NAME IS THE NAME. Measured in a fourteenth review
     * round: a `whole` answering `(WRONG "h")` passed a cell that read
     * only element 1 -- and element 0 is precisely what `whole` exists
     * to keep, because it goes into the digest that is a projection's
     * identity.
     */
    assert.deepStrictEqual(
      whole[0],
      parseAnswers('(current)')[0] === undefined ? undefined : (parseAnswers('current')[0] as unknown),
      'the clause came back under another name, and that name is part of the digest'
    );
  });

  /*
   * ⭐ AND IT CAN BE ASKED WHETHER THE ANSWER IS ABOUT WHAT WAS ASKED.
   * Two defects in this delivery were of that kind -- a record for `b.1`
   * returned for `a.1`, and a refusal about another block read as this
   * one being absent.
   */
  it('gives nothing when the answer names something else', function () {
    const said = datum('(error unknown-id "b.1" (nearest ()))');
    assert.ok(isForm(answerOf(said, 'error', { at: 1, is: 'unknown-id' })));
    assert.strictEqual(answerOf(said, 'error', { at: 2, is: 'a.1' }), null);
    assert.ok(isForm(answerOf(said, 'error', { at: 2, is: 'b.1' })));
    /*
     * ⭐ AND THE HEAD IS STILL CHECKED WHEN `expect` IS GIVEN.
     * Measured in a fourteenth review round: disabling the head test
     * only for calls that pass `expect` left every cell here passing,
     * and `(garbage unknown-id "a.1")` then produced a Form. The second
     * question does not replace the first.
     */
    assert.strictEqual(
      answerOf(datum('(garbage unknown-id "a.1")'), 'error', { at: 1, is: 'unknown-id' }),
      null,
      'the head went unchecked because a position was named as well'
    );
  });

  /*
   * ⭐ A NAME MAY BE A SYMBOL OR A STRING, and the core uses both in one
   * form: `(error unknown-id "a.1" ...)` names its family with a symbol
   * and its subject with a string. Comparing with `===` alone made the
   * symbol case never match, so a guard meant to ask "is this about what
   * I asked" refused everything -- which the suite caught, as a refusal
   * where an absence was expected.
   */
  /*
   * ⭐ `value` AND `datum` ARE THE DECODER'S OTHER TWO ANSWERS, and its
   * own file never asked about either. Measured in a fifteenth review
   * round: making `value` answer undefined always, and `datum` be null
   * always, left every cell in this file passing.
   */
  it('gives the single value of a one-value clause, and the form it read', function () {
    const form = answerOf(datum('(ok (replay #f) (cursor ("w" . 6)))'), 'ok') as Form;
    /*
     * ⛔ `false` IS BOTH AN ANSWER AND A REFUSAL HERE, so the cell
     * cannot be satisfied by a reader that gives it for everything.
     *
     * Measured in a sixteenth review round: `value: () => false` passed
     * every cell in this file and type-checked -- the replay assertion
     * wanted false, and the cursor assertion asked only that the result
     * was not undefined. The cursor is asserted by its CONTENTS now,
     * and a clause that is not there is asserted to be undefined, so
     * one constant answer cannot satisfy all three.
     */
    assert.strictEqual(form.value('replay'), false);
    assert.deepStrictEqual(form.value('cursor'), datum('("w" . 6)'));
    assert.strictEqual(form.value('nothing-of-the-sort'), undefined);
    /*
     * AND A CLAUSE WITH TWO VALUES HAS NO SINGLE VALUE -- the rule
     * `clauseValue` has always had, which nothing here asked about.
     */
    const two = answerOf(datum('(ok (cursor "w" 6))'), 'ok') as Form;
    assert.strictEqual(two.value('cursor'), undefined);
    assert.deepStrictEqual(form.datum, datum('(ok (replay #f) (cursor ("w" . 6)))'));
    assert.strictEqual(form.head, 'ok');
  });

  it('reads a name written as a symbol and one written as a string', function () {
    const said = datum('(error unknown-id "a.1" (nearest ()))');
    assert.ok(isForm(answerOf(said, 'error', { at: 1, is: 'unknown-id' })));
    assert.ok(isForm(answerOf(said, 'error', { at: 2, is: 'a.1' })));
    assert.strictEqual(answerOf(said, 'error', { at: 1, is: 'a.1' }), null);
  });
});

/*
 * plugin-r2: the doors held open for headless values are only as wide as
 * those values.
 *
 * ⭐ A HEADLESS READER THAT ACCEPTS A HEADED FORM IS THE DEFECT WITH AN
 * EXTRA STEP. `readBlock` exists because a block record has no head --
 * it begins `(id . "a.1")`, a pair -- and it checked only that the value
 * was a list. Measured in a fourteenth review round:
 * `(garbage (id . "a.1") (fields (src . "forged")) ...)` read as block
 * a.1 with a forged body, and reached a caller through the shipping
 * recursive read.
 */
describe('plugin-r2 a record reader refuses a form', function () {
  before(async () => {
    await initWire();
  });

  const datum = (text: string): unknown => parseAnswers(text)[0];

  it('refuses a value whose first element is a name rather than a pair', function () {
    const forged = '(garbage (id . "a.1") (deleted . #f) (fields (src . "forged")) (position root . 0))';
    assert.strictEqual(
      readBlock(datum(forged)),
      null,
      'a form with a head on it was taken apart by the reader that exists for values with none'
    );
  });

  /*
   * THE TWIN: a real record still reads. Without it the cell above would
   * pass on a reader that refuses everything, and every block in the
   * tree would vanish.
   */
  it('still reads a record whose first element is a pair', function () {
    const record = '((id . "a.1") (deleted . #f) (fields (src . "body")) (position root . 0) (edges))';
    const block = readBlock(datum(record));
    assert.notStrictEqual(block, null);
    assert.strictEqual((block as { id: string }).id, 'a.1');
  });
});

/*
 * plugin-r2: a clause carrying more than this build knows how to read.
 *
 * ⭐ TAKING THE FIRST IS A GUESS. `(check (writers ((...)) ((...))))`
 * carries two listings, and reading the first made a malformed answer
 * supply a cursor -- which is what lets this client write. Measured in a
 * fourteenth review round.
 */
describe('plugin-r2 a clause with more values than expected is refused', function () {
  before(async () => {
    await initWire();
  });

  it('refuses a writers clause carrying two listings', function () {
    const two = parseAnswers('(check (writers (("local" (end 7))) (("other" (end 8)))))')[0];
    assert.strictEqual(
      firstCursorFromCheck(two).ok,
      false,
      'a cursor was taken out of a clause this build cannot account for'
    );
  });

  it('still reads a writers clause carrying one', function () {
    const one = parseAnswers('(check (writers (("local" (end 7)))))')[0];
    const first = firstCursorFromCheck(one);
    assert.strictEqual(first.ok, true);
    assert.strictEqual((first as { cursor: string }).cursor, 'local:7');
  });
});

/*
 * plugin-r2: the three callers that read "there are two of these" as
 * "there is no such clause".
 *
 * The decoder began refusing a duplicated clause in a fifteenth review
 * round, and refused it by answering null -- the same answer it gives
 * for a clause that is not there. A sixteenth round measured what three
 * of its eight callers then did, and each of them is a shape this
 * delivery has repaired elsewhere: an inability answered as an absence.
 *
 * ⭐ THE CELLS ARE HERE, BESIDE THE DECODER, because what they are
 * about is the decoder's contract. The two reasons are separate values
 * now and every caller has to name the one it is handling; these three
 * are the ones whose answer had to change.
 */
describe('plugin-r2 two of a clause is not none of it', function () {
  before(async () => {
    await initWire();
  });

  it('refuses a record carrying two field lists rather than reading it as fieldless', function () {
    const record = parseAnswers(
      '((id . "a.1") (deleted . #f) (fields (src . "body")) (fields (title . "A")) (position root . 0))'
    )[0];
    assert.throws(
      () => readBlock(record),
      /two field lists/,
      'a record with two field lists was read as a block with no fields at all'
    );
    /*
     * THE TWIN: one field list still reads, or this cell would pass for
     * a reader that refused every record.
     */
    const one = parseAnswers(
      '((id . "a.1") (deleted . #f) (fields (src . "body")) (position root . 0))'
    )[0];
    const block = readBlock(one);
    assert.strictEqual(block?.fields.get('src'), 'body');
  });

  /*
   * ⚠️ AND THE SEARCH NEEDS NO GUARD OF ITS OWN, which is why this
   * cell asserts the ANSWER rather than a refusal written for it. A
   * guard was added here and deleted: a mutation run showed
   * `(ok (items ...) (items ...))` already answers null, because the
   * form falls through as the item list and `(ok ...)` is not a `hit`.
   * The cell stays, because the answer is the thing that matters and
   * nothing else pins it.
   */
  it('answers nothing readable for a search answer carrying two item lists', function () {
    assert.strictEqual(
      hitsOf(parseAnswers('(ok (items (hit "a.1" 2 "k")) (items (hit "a.2" 1 "j")))')),
      null,
      'an answer with two item lists was read as something other than an unreadable answer'
    );
    assert.deepStrictEqual(
      hitsOf(parseAnswers('(ok (items (hit "a.1" 2 "k")))')),
      [{ id: 'a.1', score: 2, note: 'k' }],
      'the twin: one item list is still a search result'
    );
  });
});
