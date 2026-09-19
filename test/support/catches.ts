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
 * WHAT A CATCH DOES, ASKED OF THE PARSER.
 *
 * Two censuses in this suite ask the same question of a catch block --
 * "can this turn a failure into an answer?" -- and for a while each had
 * its own answer to it. They drifted, and they drifted in the direction
 * that passes: one read the catch's PROSE for the word `throw`, the
 * other accepted a `throw` on one path as proof about the others, and a
 * fifteenth review round got a mutation past each of them in a shape the
 * OTHER one would have caught.
 *
 * So the rule lives in one place and both censuses call it. A rule
 * copied is two rules, and the second copy is always the one that was
 * not fixed.
 *
 * NOTE: EVERY QUESTION HERE IS PUT TO THE SYNTAX TREE. A comment reading
 * "with execFileSync throwing" is not a throw, and `!(code === 'ENOENT')`
 * is not a test for absence however much of it matches a regular
 * expression.
 */

import * as ts from 'typescript';

/*
 * A NODE THAT IS SOMEWHERE INSIDE ANOTHER, by walking the parent links
 * the parser keeps. Used to ask which BRANCH of an `if` a return sits
 * in, which is the difference between refusing a failure and answering
 * for it.
 */
export function inside(node: ts.Node, ancestor: ts.Node): boolean {
  for (let at: ts.Node | undefined = node; at !== undefined; at = at.parent) {
    if (at === ancestor) {
      return true;
    }
  }
  return false;
}

/*
 * EVERY `return` THE CATCH ITSELF CAN REACH. Returns inside a nested
 * function belong to that function and say nothing about this one.
 */
export function returnsIn(block: ts.Block): ts.ReturnStatement[] {
  const found: ts.ReturnStatement[] = [];
  const walk = (node: ts.Node): void => {
    if (ts.isReturnStatement(node)) {
      found.push(node);
    }
    if (!ts.isFunctionLike(node)) {
      node.forEachChild(walk);
    }
  };
  block.forEachChild(walk);
  return found;
}

export function throwsSomewhere(block: ts.Block): boolean {
  let found = false;
  const walk = (node: ts.Node): void => {
    if (ts.isThrowStatement(node)) {
      found = true;
    }
    if (!ts.isFunctionLike(node)) {
      node.forEachChild(walk);
    }
  };
  block.forEachChild(walk);
  return found;
}

/*
 * THE NAME OF THE THING BEING CALLED, and nothing else on the line.
 *
 * NEVER: THE CALLEE'S SOURCE TEXT IS NOT ITS NAME. This searched
 * `node.expression.getText()` with `includes`, and a sixteenth review
 * round put `(/* show *\/ Boolean)(0)` past it: a real call, to
 * something that reports nothing, whose text carries a reporting name
 * inside a comment. That is the THIRD time a census in this delivery
 * has read prose, so this one asks the tree for an identifier.
 */
function calleeName(expression: ts.Expression): string | null {
  let at: ts.Expression = expression;
  while (ts.isParenthesizedExpression(at)) {
    at = at.expression;
  }
  if (ts.isIdentifier(at)) {
    return at.text;
  }
  if (ts.isPropertyAccessExpression(at)) {
    return at.name.text;
  }
  return null;
}

/*
 * A CALL TO ONE OF THESE, MADE ON EVERY PATH THROUGH THE CATCH.
 *
 * NEVER: ONE BRANCH IS NOT EVERY BRANCH. A sixteenth review round put
 * `if (code === 'EIO') void Promise.reject(e); return false;` past the
 * first version: EACCES reports nothing and answers false, while a
 * reporting call existed SOMEWHERE in the block. So the call has to sit
 * at the top level of the catch -- unconditional, before anything else
 * decides -- which is the shape every genuine reporter in this tree
 * already has. One that reports from inside a branch can go in the
 * table with a reason.
 */
export function callsOneOf(
  block: ts.Block,
  source: ts.SourceFile,
  names: readonly string[]
): boolean {
  void source;
  return block.statements.some((statement) => {
    const expression = ts.isExpressionStatement(statement) ? statement.expression : undefined;
    if (expression === undefined) {
      return false;
    }
    let at: ts.Expression = expression;
    while (ts.isAwaitExpression(at) || ts.isVoidExpression(at)) {
      at = at.expression;
    }
    if (!ts.isCallExpression(at)) {
      return false;
    }
    const called = calleeName(at.expression);
    return called !== null && names.includes(called);
  });
}

/*
 * KEY: A BLOCK THAT CANNOT FINISH QUIETLY.
 *
 * "There is a throw in here" is not the property a leak gate needs. A
 * fifteenth review round demonstrated the gap exactly: a catch reading
 *
 *     if (code !== 'EACCES') { throw e; }
 *
 * with `listing ??= ''` after it contains a throw, contains no return,
 * and still hands EACCES back to the caller as an empty process listing
 * -- by falling off the end. What has to be true is that the block
 * cannot COMPLETE, which is a question about its last statement.
 *
 * This is deliberately conservative: an `if` counts only when it has an
 * else and both sides end, and anything it does not recognise reads as
 * "can finish". A rule that guesses in the permissive direction is the
 * failure this file exists to stop.
 */
function ends(statement: ts.Statement | undefined): boolean {
  if (statement === undefined) {
    return false;
  }
  if (ts.isThrowStatement(statement)) {
    return true;
  }
  if (ts.isBlock(statement)) {
    return ends(statement.statements[statement.statements.length - 1]);
  }
  if (ts.isIfStatement(statement)) {
    return (
      statement.elseStatement !== undefined &&
      ends(statement.thenStatement) &&
      ends(statement.elseStatement)
    );
  }
  return false;
}

/*
 * NEVER: AND A `break` OR A `continue` IS AN EXIT AS SURELY AS A RETURN.
 *
 * A sixteenth review round wrote this counterexample:
 *
 *     done: { try { ... } catch (e) { if (code === 'EACCES') break done; throw e; } }
 *     return [];
 *
 * The block's last statement is a throw, there is no return in it, and
 * EACCES leaves through the label and answers with an empty list. The
 * question "can this block finish" has more than one way to say yes.
 *
 * Any break or continue in the catch refuses it, including one inside a
 * loop that is wholly within the catch -- a false alarm in that shape,
 * which has not appeared, and which would go in a table with a reason
 * rather than be guessed at here. The conservatism points at refusing.
 */
export function cannotFinishQuietly(block: ts.Block): boolean {
  let leaves = false;
  const walk = (node: ts.Node): void => {
    if (ts.isBreakOrContinueStatement(node)) {
      leaves = true;
    }
    if (!ts.isFunctionLike(node)) {
      node.forEachChild(walk);
    }
  };
  block.forEachChild(walk);
  return !leaves && ends(block.statements[block.statements.length - 1]);
}

/*
 * A TEST FOR ABSENCE, AND ONLY FOR ABSENCE.
 *
 * `code === 'ENOENT'` and nothing else. `!==` is the same defect
 * inverted -- it answers for every failure except the one that really is
 * an absence -- and `true ||`, `|| code === 'EACCES'` and
 * `!(code === 'ENOENT')` all read as an absence test to anything asking
 * whether those eight characters appear. Each of those was a live
 * mutation in a thirteenth, fourteenth or fifteenth review round.
 */
/*
 * THE NAMES IN THIS CATCH THAT CARRY THE FAILURE: its parameter, and
 * anything declared from something already in the set. `const code = (e
 * as NodeJS.ErrnoException).code` puts `code` in it; `const n = 1` does
 * not.
 */
export function namesOfTheFailure(clause: ts.CatchClause, source: ts.SourceFile): Set<string> {
  const names = new Set<string>();
  const parameter = clause.variableDeclaration?.name;
  if (parameter !== undefined && ts.isIdentifier(parameter)) {
    names.add(parameter.text);
  }
  /*
   * TWO PASSES, so that a declaration reading from an earlier one is
   * picked up whichever order they are visited in.
   */
  for (let round = 0; round < 2; round += 1) {
    const walk = (node: ts.Node): void => {
      if (ts.isVariableDeclaration(node) && ts.isIdentifier(node.name) && node.initializer !== undefined) {
        const from = node.initializer.getText(source);
        if ([...names].some((known) => new RegExp(`\\b${known}\\b`).test(from))) {
          names.add(node.name.text);
        }
      }
      if (!ts.isFunctionLike(node)) {
        node.forEachChild(walk);
      }
    };
    clause.block.forEachChild(walk);
  }
  return names;
}

export function testsForAbsence(
  expression: ts.Expression,
  source: ts.SourceFile,
  failure: Set<string>
): boolean {
  if (!ts.isBinaryExpression(expression)) {
    return false;
  }
  if (expression.operatorToken.kind !== ts.SyntaxKind.EqualsEqualsEqualsToken) {
    return false;
  }
  const named = expression.right.getText(source);
  if (named !== "'ENOENT'" && named !== '"ENOENT"') {
    return false;
  }
  /*
   * NEVER: AND THE VALUE BEING TESTED HAS TO BE THE FAILURE'S.
   *
   * `if (String('ENOENT') === 'ENOENT') return 0;` is a test for absence
   * to anything that reads the operator and the right-hand side, and it
   * answers zero for EACCES. Measured in a sixteenth review round, in
   * `countRealRunRoot` and in `isDirectory`, past both censuses. What
   * makes a guard a guard is what it is asking ABOUT.
   */
  const asked = expression.left.getText(source);
  return [...failure].some((name) => new RegExp(`\\b${name}\\b`).test(asked));
}

/*
 * AN ANSWER THAT ONLY HAPPENS WHEN THE THING IS ABSENT. The return has
 * to sit in the THEN branch of an absence test, not merely somewhere in
 * a block that contains one: a census that asked the looser question
 * passed an added `if (code === 'EACCES') return 0;` in front of the
 * real guard.
 */
export function onlyWhenAbsent(
  answer: ts.ReturnStatement,
  within: ts.Block,
  source: ts.SourceFile,
  failure: Set<string>
): boolean {
  let at: ts.Node | undefined = answer;
  for (; at !== undefined && at !== within; at = at.parent) {
    const owner: ts.Node | undefined = at.parent;
    if (owner !== undefined && ts.isIfStatement(owner) && owner.thenStatement === at) {
      if (testsForAbsence(owner.expression, source, failure)) {
        return true;
      }
    }
  }
  return false;
}
