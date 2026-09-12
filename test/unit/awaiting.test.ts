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
 * EVERY WAIT IN THE EXTENSION HOST, AND WHETHER IT CHECKS WHAT CHANGED
 * UNDER IT.
 *
 * `theourgia.store` can change at any moment; `rebuild` then replaces the
 * client, the model, the queue and the Saver, and raises `generation`.
 * Anything that decided something before a wait and acts on it after is
 * acting for a window that may no longer exist, so the rule in this file
 * is: take `generation` before the wait, compare it after, and drop the
 * result if it moved.
 *
 * ⚠️ THE RULE HAD SIX CALL SITES AND ONE EXCEPTION NOBODY HAD WRITTEN
 * DOWN. `adoptingInto` read the Saver when the command started and the
 * pickers after it are waits, so changing the store while the list was
 * open put a takeover's entries into the queue the window had just left
 * -- and the report then recommended a command that acts on the new one.
 * A rule kept in six heads is a rule with no way to notice the seventh
 * place. This census is that way.
 *
 * ⚠️ IT IS READ FROM THE SYNTAX TREE, NOT FROM THE TEXT. A regular
 * expression for `generation` finds the word in comments and in the
 * assignment that raises it, cannot tell an inner function's waits from
 * its parent's, and cannot say whether the comparison comes before or
 * after the wait -- which is the entire question. The census that counts
 * cells in this tree learned the same lesson the hard way.
 *
 * THE EXCEPTIONS ARE NAMED WITH THEIR REASON, and a function that stops
 * conforming without being named here fails this file rather than
 * quietly joining them.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';

/*
 * WHY EACH WAITING FUNCTION MAY GO WITHOUT THE CHECK. One line per
 * function, and the line has to be a reason rather than a note that it
 * does not have one.
 */
const WITHOUT_THE_CHECK: Record<string, string> = {
  activate:
    'the extension is being built; there is no earlier generation for anything to have changed from',
  pick: 'it returns the user’s answer and decides nothing; its caller holds the generation',
  confirm: 'as pick',
  'vscode.commands.registerCommand(REFRESH_OUTLINE.id)':
    'its only wait is refreshConflicts, which makes the check itself, and nothing follows it',
  'vscode.commands.registerCommand(SHOW_STATUS.id)':
    'it shows the status of the store configured NOW, which is what the user asked for; a stale ' +
    'generation is the answer to a question nobody put',
  getChildren:
    'RULED-SHAPED, REPORTED: its two comparisons guard the REPORTING -- `failed` and ' +
    '`unknownMarks` -- and the nodes are returned without one, so a listing fetched for the ' +
    'previous store can still be handed to the tree. That is the outline surface, not this ' +
    'batch\'s, and it is named here rather than left looking like a site that conforms. Raised ' +
    'with the main session',
  onSaved:
    'RULED: next batch; this one measures it and does not repair it. The save is issued to the ' +
    'Saver read before the wait and the outcome is reported after it, so a store changed ' +
    'mid-save is reported into a window that has moved on. It is the save surface, this batch ' +
    'did not make it worse, and the batch\'s scope closed at kickoff. The reading is integration ' +
    'cell C7: the status bar is recomputed from the current store, so what crosses is the ' +
    'SENTENCE about the old store\'s save and not the numbers. Named here so that it is not ' +
    'mistaken for a site that conforms'
};

interface Waiting {
  name: string;
  line: number;
  checked: boolean;
  waitsAfterTheLastGuard: number;
}

function nameOf(fn: ts.Node, src: ts.SourceFile): string {
  const named = fn as ts.FunctionDeclaration;
  if (named.name !== undefined) {
    return named.name.getText(src);
  }
  const parent = fn.parent;
  if (parent !== undefined && ts.isVariableDeclaration(parent)) {
    return parent.name.getText(src);
  }
  if (parent !== undefined && ts.isPropertyAssignment(parent)) {
    return parent.name.getText(src);
  }
  if (parent !== undefined && ts.isCallExpression(parent)) {
    const called = parent.expression.getText(src);
    const first = parent.arguments[0];
    const subject = first !== undefined && first !== fn ? first.getText(src) : '';
    return `${called}(${subject})`;
  }
  return 'anonymous';
}

function isFunction(n: ts.Node): boolean {
  return (
    ts.isFunctionDeclaration(n) ||
    ts.isFunctionExpression(n) ||
    ts.isArrowFunction(n) ||
    ts.isMethodDeclaration(n)
  );
}

/*
 * ⚠️ A FUNCTION'S OWN WAITS, not those of the functions inside it. An
 * inner callback's `await` says nothing about whether THIS body resumed
 * across one, and counting it would let a handler that waits without
 * checking hide behind a helper that does.
 */
function within<T extends ts.Node>(fn: ts.Node, pick: (n: ts.Node) => n is T): T[] {
  const found: T[] = [];
  const walk = (n: ts.Node): void => {
    if (n !== fn && isFunction(n)) {
      return;
    }
    if (pick(n)) {
      found.push(n);
    }
    ts.forEachChild(n, walk);
  };
  walk(fn);
  return found;
}

/*
 * ⚠️ `for await (... of ...)` IS A WAIT AND THE SURVEY COULD NOT SEE ONE.
 * It is not an `AwaitExpression`; it is a `ForOfStatement` carrying an
 * await modifier. A reviewer added an unguarded one to `extension.ts`
 * and all three cells below stayed green -- the census was blind to a
 * whole construct, which is the worst thing a census can be.
 */
function waitsIn(fn: ts.Node): ts.Node[] {
  const expressions: ts.Node[] = within(fn, ts.isAwaitExpression);
  const loops = within(fn, ts.isForOfStatement).filter((n) => n.awaitModifier !== undefined);
  return [...expressions, ...loops];
}

/*
 * ⚠️ A COMPARISON IS NOT A GUARD UNTIL IT STOPS SOMETHING.
 *
 * The rule used to accept any `===`/`!==` mentioning `generation`
 * anywhere after the wait. A reviewer replaced `refreshConflicts`'
 * `return` with `void asked` -- the comparison gone, the identifier
 * still there, nothing stopped -- and all three cells passed. So a guard
 * is now an `if` whose test compares `generation` and whose then-branch
 * LEAVES: returns, or throws. That is the shape that makes a stale
 * answer harmless, and it is the shape that can be checked.
 */
function leaves(statement: ts.Statement): boolean {
  if (ts.isReturnStatement(statement) || ts.isThrowStatement(statement)) {
    return true;
  }
  if (ts.isBlock(statement)) {
    return statement.statements.some(leaves);
  }
  return false;
}

function guardsIn(fn: ts.Node, src: ts.SourceFile): ts.IfStatement[] {
  return within(fn, ts.isIfStatement).filter((s) => {
    const test = s.expression;
    const compares =
      ts.isBinaryExpression(test) &&
      (test.operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken ||
        test.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsEqualsToken) &&
      [test.left, test.right].some((side) => /\bgeneration\b/.test(side.getText(src)));
    return compares && leaves(s.thenStatement);
  });
}

function survey(): Waiting[] {
  const file = path.join(__dirname, '..', '..', '..', 'src', 'extension.ts');
  const src = ts.createSourceFile(
    'extension.ts',
    fs.readFileSync(file, 'utf8'),
    ts.ScriptTarget.ES2022,
    true
  );
  const out: Waiting[] = [];
  const visit = (n: ts.Node): void => {
    if (isFunction(n)) {
      const waits = waitsIn(n).sort((a, b) => a.getStart(src) - b.getStart(src));
      if (waits.length > 0) {
        const first = waits[0].getStart(src);
        const taken = within(
          n,
          (x): x is ts.Identifier => ts.isIdentifier(x) && x.text === 'generation'
        ).some((x) => x.getStart(src) < first);
        const guards = guardsIn(n, src).filter((g) => g.getStart(src) > first);
        const lastGuard = guards.reduce((at, g) => Math.max(at, g.getEnd()), -1);
        out.push({
          name: nameOf(n, src),
          line: src.getLineAndCharacterOfPosition(n.getStart(src)).line + 1,
          checked: taken && guards.length > 0,
          /*
           * A GUARD PROTECTS WHAT COMES AFTER IT AND NOTHING BEFORE.
           * Waits that happen after the last one resume with nobody
           * asking again, which is the same defect one step further in.
           */
          waitsAfterTheLastGuard:
            lastGuard < 0 ? 0 : waits.filter((w) => w.getStart(src) > lastGuard).length
        });
      }
    }
    ts.forEachChild(n, visit);
  };
  visit(src);
  return out;
}

/*
 * ⚠️ AND A GUARD ONLY COVERS THE WAITS BEFORE IT.
 *
 * A function may take the generation, wait, check, and then wait AGAIN
 * -- and the second wait resumes with nobody asking. `reconcileBlock`
 * does exactly that: its check sits before a picker the user may leave
 * open for as long as they like. Naming the ones that do it, with the
 * reason, is the same discipline as the table above; a new one has to
 * be argued for rather than joining quietly.
 */
const WAITS_AFTER_ITS_GUARD: Record<string, string> = {
  openBlock:
    'the store is captured BEFORE the first wait and every later step uses the captured one: ' +
    'the publication directory is built from it, so the version and its record are written under ' +
    'the store the block was read from however the settings move meanwhile. THIS ENTRY USED TO ' +
    'SAY the later waits were editor calls and therefore wrote nothing -- which is false: one of ' +
    'them is chain.run(publisher.publish(...)), and it writes a markdown file and a sidecar. An ' +
    'outside review caught the false premise. What survives is the argument above, which is ' +
    'about WHICH store the writing goes to rather than about whether there is any. The source ' +
    'asks for a failing cell before a check is added back',
  reconcileBlock:
    'REPORTED, NOT RULED: its check sits before `chain.run` and the confirmation the user may ' +
    'leave open, so the write happens with a generation nobody re-read. The directory was ' +
    'computed from the store the block was read from, which is where the block is -- so this may ' +
    'be right rather than merely unguarded. Raised with the main session as its own surface'
};

/*
 * ⚠️ AND THE ONE EXEMPTION WHOSE ARGUMENT IS ABOUT WHERE THE WRITING
 * GOES, CHECKED WHERE IT CAN BE.
 *
 * `openBlock` waits several times after its only generation check, and
 * one of those waits publishes a file and a record. What makes that
 * acceptable is that the store is captured BEFORE the first wait and the
 * publication directory is built from the captured one, so the writing
 * lands under the store the block was read from.
 *
 * ⚠️ THE BEHAVIOURAL CELL FOR THIS DOES NOT EXIST, AND HERE IS WHY,
 * measured rather than assumed. I wrote one: open a block in store A,
 * change the store while it is being opened, and assert the version
 * appears under A and not under B. It passed -- and it passed just as
 * well with the product mutated to use the LIVE store, because the
 * unguarded stretch is a few editor calls long and a settings update
 * cannot be timed into it. A cell that is green whether or not the
 * defect is present is worse than no cell, so it was deleted rather than
 * kept as a green row.
 *
 * What CAN be checked is the thing the argument actually rests on: that
 * the directory is built from the captured name. That is a property of
 * the source, so it is read from the source -- the same instrument, and
 * the same reason, as the census below.
 */
describe('the block being opened is published under the store it was read from', () => {
  it('builds the publication directory from the store captured before the first wait', () => {
    const file = path.join(__dirname, '..', '..', '..', 'src', 'extension.ts');
    const src = ts.createSourceFile(
      'extension.ts',
      fs.readFileSync(file, 'utf8'),
      ts.ScriptTarget.ES2022,
      true
    );
    let openBlock: ts.Node | undefined;
    const find = (n: ts.Node): void => {
      if (ts.isFunctionDeclaration(n) && n.name?.getText(src) === 'openBlock') {
        openBlock = n;
      }
      ts.forEachChild(n, find);
    };
    find(src);
    assert.ok(openBlock !== undefined, 'there is no openBlock in extension.ts any more');

    /*
     * THE CAPTURE, and it has to come before the first wait or it is not
     * a capture at all.
     */
    const waits = waitsIn(openBlock as ts.Node).sort(
      (a, b) => a.getStart(src) - b.getStart(src)
    );
    assert.ok(waits.length > 0, 'openBlock no longer waits, so this cell is about nothing');
    const captures = within(
      openBlock as ts.Node,
      (n): n is ts.VariableDeclaration =>
        ts.isVariableDeclaration(n) && n.name.getText(src) === 'store'
    );
    assert.strictEqual(captures.length, 1, 'openBlock does not capture the store exactly once');
    assert.ok(
      captures[0].getStart(src) < waits[0].getStart(src),
      'the store is captured after openBlock has already waited, so it is not the store the ' +
        'block was read from'
    );

    /*
     * AND THE DIRECTORY IS BUILT FROM IT. `storeHash(config.store)` here
     * would be the live one -- the defect this exemption's argument
     * denies -- and it is one token away.
     */
    const directories = within(
      openBlock as ts.Node,
      (n): n is ts.VariableDeclaration =>
        ts.isVariableDeclaration(n) && n.name.getText(src) === 'directory'
    );
    assert.strictEqual(directories.length, 1, 'openBlock builds more than one directory');
    const built = directories[0].initializer?.getText(src) ?? '';
    assert.ok(
      /storeHash\(\s*store\s*\)/.test(built),
      `openBlock's publication directory is not built from the captured store: ${built}`
    );
  });
});

describe('every wait in the extension host knows what may have changed under it', () => {
  const waiting = survey();

  /*
   * THE INSTRUMENT'S OWN FIRST READING. A survey that found nothing
   * would pass every check below it while saying nothing at all, and
   * that is the failure this kind of guard has in this tree's history.
   */
  it('finds the waits at all', () => {
    assert.ok(
      waiting.length >= 10,
      `the survey found ${waiting.length} waiting functions in extension.ts, which is too few to ` +
        'be reading the file'
    );
    assert.ok(
      waiting.some((w) => w.checked),
      'the survey found no function that checks the generation, so it is not reading the check'
    );
  });

  it('has a reason written down for every wait that does not check', () => {
    const unexplained = waiting
      .filter((w) => !w.checked && WITHOUT_THE_CHECK[w.name] === undefined)
      .map((w) => `${w.name} (extension.ts:${w.line})`);
    assert.deepStrictEqual(
      unexplained,
      [],
      'these functions resume after a wait without checking the generation, and no reason is ' +
        'written down for them'
    );
  });

  /*
   * AND THE LIST DOES NOT OUTLIVE ITS ENTRIES. An exception for a
   * function that now checks -- or that no longer exists -- is a licence
   * nobody asked for, kept where the next reader will believe it.
   */
  /*
   * ⚠️ THE RULE ASKS WHETHER THE COMPARISON STOPS ANYTHING. It used to
   * ask only whether a comparison existed: a reviewer replaced
   * `refreshConflicts`' `return` with `void asked` and this file stayed
   * green while the late answer went on being painted. A guard is an
   * `if` that compares the generation and LEAVES.
   */
  it('has a guard that leaves, for every wait that claims to be checked', () => {
    const late = waiting
      .filter((w) => w.checked && w.waitsAfterTheLastGuard > 0)
      .filter((w) => WAITS_AFTER_ITS_GUARD[w.name] === undefined)
      .map((w) => `${w.name} (extension.ts:${w.line}) waits ${w.waitsAfterTheLastGuard}x after its last guard`);
    assert.deepStrictEqual(
      late,
      [],
      'these functions check the generation and then wait again, with no reason written down'
    );
  });

  it('keeps no exception for a wait that does not need one', () => {
    const names = new Set(waiting.filter((w) => !w.checked).map((w) => w.name));
    const stale = Object.keys(WITHOUT_THE_CHECK).filter((name) => !names.has(name));
    assert.deepStrictEqual(stale, [], 'these exceptions are for functions that do not need one');
  });
});
