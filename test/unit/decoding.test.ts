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
 * KEY: THE COMPILER IS THE CENSUS FOR THE FIRST WAY. `clause`,
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
 * NEVER: MATCHING THE CALL SITE'S IDENTIFIER IS NOT ENOUGH. An import may
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
     * NEVER: A NAMED IMPORT IS NOT THE ONLY WAY IN.
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
           * NEVER: AND A DESTRUCTURING IS A DECLARATION TOO.
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
   * NEVER: `!== null` IS NOT "A FORM CAME BACK".
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
     * KEY: WHAT IS IN THE CLAUSE, not how many things. Measured in a
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
     * KEY: AND AN ABSENT CLAUSE SAYS WHICH KIND OF NOTHING IT IS. A
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
   * KEY: `whole` KEEPS THE NAME AND `clause` DROPS IT, and the difference
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
     * KEY: THE WHOLE CLAUSE, AND NOTHING MORE. Measured in a fifteenth
     * review round: appending an element to every clause left a cell
     * reading only elements 0 and 1 passing, and `(current "h" 99)`
     * was accepted as the whole of `(current "h")` -- which goes into
     * the digest that is a projection's identity.
     */
    assert.strictEqual(whole.length, 2, `the clause came back as ${JSON.stringify(whole)}`);
    assert.strictEqual(whole[1], 'h');
    /*
     * KEY: AND THE NAME IS THE NAME. Measured in a fourteenth review
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
   * KEY: AND IT CAN BE ASKED WHETHER THE ANSWER IS ABOUT WHAT WAS ASKED.
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
     * KEY: AND THE HEAD IS STILL CHECKED WHEN `expect` IS GIVEN.
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
   * KEY: A NAME MAY BE A SYMBOL OR A STRING, and the core uses both in one
   * form: `(error unknown-id "a.1" ...)` names its family with a symbol
   * and its subject with a string. Comparing with `===` alone made the
   * symbol case never match, so a guard meant to ask "is this about what
   * I asked" refused everything -- which the suite caught, as a refusal
   * where an absence was expected.
   */
  /*
   * KEY: `value` AND `datum` ARE THE DECODER'S OTHER TWO ANSWERS, and its
   * own file never asked about either. Measured in a fifteenth review
   * round: making `value` answer undefined always, and `datum` be null
   * always, left every cell in this file passing.
   */
  it('gives the single value of a one-value clause, and the form it read', function () {
    const form = answerOf(datum('(ok (replay #f) (cursor ("w" . 6)))'), 'ok') as Form;
    /*
     * NEVER: `false` IS BOTH AN ANSWER AND A REFUSAL HERE, so the cell
     * cannot be satisfied by a reader that gives it for everything.
     *
     * Measured in a sixteenth review round: `value: () => false` passed
     * every cell in this file and type-checked -- the replay assertion
     * wanted false, and the cursor assertion asked only that the result
     * was not undefined. The cursor is asserted by its CONTENTS now,
     * and a clause that is not there is asserted to be undefined, so
     * one constant answer cannot satisfy all three.
     */
    assert.deepStrictEqual(form.value('replay'), { read: true, value: false });
    assert.deepStrictEqual(form.value('cursor'), { read: true, value: datum('("w" . 6)') });
    assert.deepStrictEqual(form.value('nothing-of-the-sort'), { read: false, because: 'absent' });
    /*
     * KEY: AND THE SINGLE VALUE SAYS WHICH KIND OF NOTHING, like the
     * clause reader. A seventeenth review round measured what the old
     * `undefined` for both cost: two `cursor` clauses cancelled out and
     * `eventFromWrite` took its event from the `event` clause instead.
     */
    const twice = answerOf(datum('(ok (cursor ("a" . 1)) (cursor ("b" . 2)))'), 'ok') as Form;
    assert.deepStrictEqual(twice.value('cursor'), { read: false, because: 'duplicated' });
    /*
     * AND A CLAUSE WITH TWO VALUES HAS NO SINGLE VALUE -- the rule
     * `clauseValue` has always had, which nothing here asked about.
     */
    const two = answerOf(datum('(ok (cursor "w" 6))'), 'ok') as Form;
    assert.deepStrictEqual(two.value('cursor'), { read: false, because: 'unreadable' });
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
 * KEY: A HEADLESS READER THAT ACCEPTS A HEADED FORM IS THE DEFECT WITH AN
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
 * KEY: TAKING THE FIRST IS A GUESS. `(check (writers ((...)) ((...))))`
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
 * KEY: THE CELLS ARE HERE, BESIDE THE DECODER, because what they are
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
   * NOTE: AND THE SEARCH NO LONGER OPENS AN ENVELOPE AT ALL, by a ruling
   * taken after three review rounds each found a face of it.
   *
   * Whether an envelope was asked for is a fact about the REQUEST, which
   * `interpret` already holds; a reader that decides from the shape of
   * what came back is the defect `client.ts` was repaired for. So an
   * `(ok (items ...))` handed to `hitsOf` is not a search result -- it
   * is a form, and a form is not a `hit`.
   */
  /*
   * plugin-r3 A: every route into a clause counts through one reader,
   * and a dotted occurrence is an occurrence.
   *
   * A seventeenth review round walked past both of round sixteen's
   * refusals the same way -- `clause` counted only list items, so
   * `(fields . bad) (fields . worse)` was two occurrences it could not
   * see. Each row below is a route, and each names the reading it gave
   * before the repair.
   */
  it('counts a dotted occurrence, at every route', function () {
    assert.deepStrictEqual(
      (answerOf(parseAnswers('(ok (items . bad) (items . worse) (cursor ("w" . 7)))')[0], 'ok') as Form).clause(
        'items'
      ),
      { read: false, because: 'duplicated' },
      'two dotted items clauses read as no items clause at all'
    );
    /*
     * AND ONE OF THEM IS NOT A CLAUSE EITHER. `(items . bad)` has a name
     * and no sequence after it; reading that as "there is no such
     * clause" is the same collapse with one occurrence instead of two.
     */
    assert.deepStrictEqual(
      (answerOf(parseAnswers('(ok (items . bad))')[0], 'ok') as Form).clause('items'),
      { read: false, because: 'unreadable' }
    );
    assert.throws(
      () =>
        readBlock(
          parseAnswers('((id . "a.1") (fields . bad) (fields . worse) (position root . 0))')[0]
        ),
      /two field lists/,
      'a record with two dotted field lists read as a block with no fields'
    );
  });

  it('refuses a record that names two identities', function () {
    assert.strictEqual(
      readBlock(
        parseAnswers('((id . "a.1") (id . "b.1") (deleted . #f) (fields (src . "one")) (position root . 0))')[0]
      ),
      null,
      'an identity was chosen out of a record that names two'
    );
    /*
     * AND THE OTHER TWO ASSOCIATIONS REFUSE THE SAME WAY, because the
     * reader is the same one.
     */
    for (const twice of [
      '((id . "a.1") (deleted . #f) (deleted . #t) (fields (src . "one")) (position root . 0))',
      '((id . "a.1") (deleted . #f) (fields (src . "one")) (position root . 0) (position root . 1))'
    ]) {
      assert.strictEqual(readBlock(parseAnswers(twice)[0]), null, `read a block out of ${twice}`);
    }
  });

  it('refuses a record that names one field twice', function () {
    assert.throws(
      () =>
        readBlock(
          parseAnswers('((id . "a.1") (deleted . #f) (fields (src . "one") (src . "two")) (position root . 0))')[0]
        ),
      /two fields called src/,
      'a body was chosen out of two the store named, by the order they were written in'
    );
    /*
     * THE TWIN: two DIFFERENT field names are an ordinary record.
     */
    const block = readBlock(
      parseAnswers('((id . "a.1") (deleted . #f) (fields (src . "one") (title . "A")) (position root . 0))')[0]
    );
    assert.strictEqual(block?.fields.get('src'), 'one');
    assert.strictEqual(block?.fields.get('title'), 'A');
  });

  it('reads the hits it is given, and nothing that came wrapped', function () {
    assert.deepStrictEqual(
      hitsOf(parseAnswers('(hit "a.1" 2 "k")\n(hit "a.2" 1 "j")')),
      [
        { id: 'a.1', score: 2, note: 'k' },
        { id: 'a.2', score: 1, note: 'j' }
      ]
    );
    assert.strictEqual(
      hitsOf(parseAnswers('(ok (items (hit "a.1" 2 "k")))')),
      null,
      'an envelope reached this reader and was opened here, which is the ruling that was taken'
    );
    assert.deepStrictEqual(hitsOf([]), [], 'no hits is no output on the human route');
  });
});

/*
 * plugin-r3 B: the census that keeps section A true.
 *
 * Section A repaired five routes into a clause so that all of them count
 * occurrences through one reader. That is four rounds' worth of repairs
 * to four sites, and the last five review rounds have each produced a
 * fifth site. What makes A a property of the tree rather than a list of
 * fixed places is this: a function that reaches into a form's elements
 * looking for a named one, and does not go through `occurrencesOf`, is
 * a new route and fails here.
 *
 * KEY: THE QUESTION IS PUT TO THE SYNTAX TREE, and what it looks for is a
 * comparison of a datum's head with a name -- `isSym(x[0], name)`,
 * `headName(x) === name`, `x[0].name === name`. Those are the shapes the
 * five routes had in common before they were merged.
 */
describe('plugin-r3 no sixth route into a clause', function () {
  /*
   * NEVER: THE EXEMPTIONS USED TO BE WHOLE FILES, AND THE MATCH USED TO
   * BE ONE-SIDED.
   *
   * Two ways past the first version, both measured in an eighteenth
   * review round. A new reader written as
   * `value.find((item) => Array.isArray(item) && name === item[0]?.name)`
   * was invisible, because the census looked at the LEFT of a comparison
   * and this one puts the name on the right. And a new reader appended
   * to `wire.ts` -- in the exact syntax the census does recognise -- was
   * invisible too, because that whole file was exempt.
   *
   * So the comparison is read from both sides, and an exemption names a
   * FUNCTION. A file is too big a thing to exempt: the reason a file has
   * one head comparison in it says nothing about the next one somebody
   * adds to it.
   */
  const NOT_LOOKING_FOR_A_CLAUSE: Record<string, string> = {
    'wire.ts:answerOf':
      "compares a name given by the caller's `expect` argument against a POSITION the caller " +
      'named -- `(error unknown-id "a.1")`. It is not a search for a clause.',
    'wire.ts:occurrencesOf':
      'IS the one reader. It is the function every other route was made to go through, and the ' +
      'comparison here is the counting this census exists to funnel everything into.',
    'wire.ts:isSym':
      'is the predicate every other route is written in terms of: it answers whether a datum IS ' +
      'the symbol with this name, which is the question, not a search for one.',
    'blocks.ts:fieldConflict':
      'asks whether a FIELD VALUE is a conflict marker -- `(conflict "a" "b")` in the place a ' +
      "field's value would be. The datum was handed to it; it is not searching a record for a " +
      'clause called conflict.',
    'blocks.ts:placementOf':
      'dispatches on the first element of a `position` datum: `(position root . 0)` versus ' +
      '`(position conflict 2)`. The element was handed to it by `assocTail`.',
    'saver.ts:saysNobodyKnows':
      'reads position one of `(error <name> ...)`, which `answerOf` has already verified, and ' +
      'asks whether that name is one of three. It is reading a name, not looking for one.',
    'saver.ts:whichRetryableRefusal':
      'as `saysNobodyKnows` -- position one of the same verified form, against a list of names.',
    'saver.ts:whichSettingsRefusal':
      'as `saysNobodyKnows` -- position one of the same verified form, against a list of names.',
    'model.ts:readMark':
      'reads a structural mark, `(orphan "a.1")`, by head: the head is the KIND of the mark and ' +
      'this dispatches on it. Nothing here searches a sequence for a named element.',
    'saver.ts:saysAnOperatorSettledIt':
      'as `saysNobodyKnows` -- position one of the same verified form, against one name.'
  };

  const root = path.join(__dirname, '..', '..', '..');

  /*
   * THE FUNCTION A NODE IS IN, by its name, so that an exemption can be
   * about one function rather than a file.
   */
  /*
   * NEVER: AN UNQUALIFIED NAME LETS A NEW FUNCTION INHERIT AN OLD ONE'S
   * EXEMPTION.
   *
   * Measured in a nineteenth review round: a class added to `wire.ts`
   * with a static method called `answerOf` took the exemption written
   * for the decoder's own `answerOf` and passed all three rows. The name
   * is the whole chain of things it is inside, so two different
   * functions cannot share one.
   */
  function owner(node: ts.Node, source: ts.SourceFile): string {
    const names: string[] = [];
    for (let at: ts.Node | undefined = node; at !== undefined; at = at.parent) {
      if (ts.isFunctionDeclaration(at) || ts.isMethodDeclaration(at)) {
        const named = at.name?.getText(source);
        if (named !== undefined) {
          names.push(named);
        }
      } else if (ts.isClassDeclaration(at) || ts.isInterfaceDeclaration(at)) {
        names.push(at.name?.getText(source) ?? '<anonymous class>');
      } else if (ts.isModuleDeclaration(at)) {
        /*
         * NEVER: A NAMESPACE WAS NOT PART OF THE NAME EITHER. Measured in
         * a twentieth review round, one round after the class case:
         * `namespace Extra { export function answerOf ... }` took the
         * exemption written for the decoder's own `answerOf`. Anything
         * that can hold a function has to be in the name, so the list is
         * written as one chain rather than as three cases that happened
         * to be thought of.
         */
        names.push(at.name.getText(source));
      } else if (ts.isVariableDeclaration(at) && ts.isIdentifier(at.name)) {
        names.push(at.name.text);
      } else if (ts.isPropertyAssignment(at) && ts.isIdentifier(at.name)) {
        names.push(at.name.text);
      }
    }
    return names.length === 0 ? '<top level>' : names.reverse().join('.');
  }

  function routesIn(file: string): string[] {
    const text = fs.readFileSync(path.join(root, 'src', file), 'utf8');
    const parsed = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
    const found: string[] = [];
    const note = (node: ts.Node): void => {
      found.push(`${file}:${owner(node, parsed)}`);
    };
    const walk = (node: ts.Node): void => {
      /*
       * `isSym(x, name)` -- the two-argument form is a head comparison
       * and the one-argument form is not.
       */
      if (
        ts.isCallExpression(node) &&
        node.expression.getText(parsed) === 'isSym' &&
        node.arguments.length === 2
      ) {
        note(node);
      }
      /*
       * `headName(x) === name`, `x[0].name === name`, and the same two
       * written the other way round. A census that reads one side of an
       * equality is a census somebody gets past by swapping the
       * operands, which is what happened.
       */
      if (
        ts.isBinaryExpression(node) &&
        node.operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken
      ) {
        const sides = [node.left.getText(parsed), node.right.getText(parsed)];
        /*
         * NEVER: `headName` WAS NOT THE ONLY HELPER THAT ANSWERS A HEAD.
         *
         * Measured in a nineteenth review round: a new unchecked reader
         * written with `frontName(item) === name` -- the helper this
         * delivery ADDED, and the one `occurrencesOf` itself uses --
         * passed all three rows. A census that lists the spellings it
         * knows is a census whose list is always one short; what it can
         * do is list them out loud, here, so the next one is an obvious
         * omission rather than a discovery.
         */
        if (sides.some((side) => /^(headName|frontName)\(/.test(side) || /\.name$/.test(side) || /\.name\?\.?$/.test(side))) {
          note(node);
        }
      }
      node.forEachChild(walk);
    };
    walk(parsed);
    return found;
  }

  function everyRoute(): string[] {
    return fs
      .readdirSync(path.join(root, 'src'))
      .filter((name) => name.endsWith('.ts'))
      .flatMap((name) => routesIn(name));
  }

  it('finds no route into a clause outside the one reader', function () {
    const strays = [...new Set(everyRoute())]
      .filter((where) => NOT_LOOKING_FOR_A_CLAUSE[where] === undefined)
      .sort();
    assert.deepStrictEqual(
      strays,
      [],
      'these functions compare a datum\'s head with a name of their own accord, which is another ' +
        'way of asking "is there a clause called this" -- and the five that existed before each ' +
        'gave the reassuring answer to "how many are there". Ask `occurrencesOf` instead, or add ' +
        'a line to NOT_LOOKING_FOR_A_CLAUSE saying what this comparison is for.'
    );
  });

  /*
   * KEY: AND NO EXEMPTION FOR A COMPARISON THAT IS NOT THERE. A stale
   * entry is a lie that reads like diligence, and with whole-file
   * entries there was nothing to go stale.
   */
  it('has no exemption for a comparison that is not there any more', function () {
    const present = new Set(everyRoute());
    const gone = Object.keys(NOT_LOOKING_FOR_A_CLAUSE)
      .filter((where) => !present.has(where))
      .sort();
    assert.deepStrictEqual(gone, [], 'these exemptions name comparisons that have moved or gone');
  });

  /*
   * NOTE: AND THIS ONE IS ABOUT THE TABLE, NOT ABOUT THE PRODUCT.
   *
   * A nineteenth review round pointed out that it would pass over an
   * empty `src/` -- which is true, and is what it is for. The table is
   * the only thing standing between this census and the next reader who
   * wants their new function allowed, so a one-word reason is a hole in
   * the census itself. It is named so that nobody reads it as evidence
   * about behaviour; the two rows above it are the ones that read the
   * tree.
   */
  it('has a reason, of more than a few words, beside every exemption in this table', function () {
    const thin = Object.entries(NOT_LOOKING_FOR_A_CLAUSE)
      .filter(([, why]) => why.length < 40)
      .map(([where]) => where);
    assert.deepStrictEqual(thin, []);
  });
});
