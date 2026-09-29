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
 * NOTE: THE RULE HAD SIX CALL SITES AND ONE EXCEPTION NOBODY HAD WRITTEN
 * DOWN. `adoptingInto` read the Saver when the command started and the
 * pickers after it are waits, so changing the store while the list was
 * open put a takeover's entries into the queue the window had just left
 * -- and the report then recommended a command that acts on the new one.
 * A rule kept in six heads is a rule with no way to notice the seventh
 * place. This census is that way.
 *
 * NOTE: IT IS READ FROM THE SYNTAX TREE, NOT FROM THE TEXT. A regular
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
  migrateBlock: 'The offered store, directory and client are captured before confirmation. migrateLegacy rechecks source liveness, pending sends, complete bytes and dirty state after its source reads; the result names that captured directory.',
  sources: 'Both source reads use the captured client and source writer. migrateLegacy compares the returned authority bytes against every original after these waits before any move.',

  'openBlock/PathChain.run':
    'The working read, directory, store and writer are captured for the same open. Publisher checks dirty, bytes and owner at the final synchronous replacement boundary; a setting change cannot retarget the directory.',
  'reconcileBlock/PathChain.run(directory)#1':
    'as reconcileBlock\'s entry in WAITS_AFTER_ITS_GUARD: XR-01 binds both chain waits to the selected file ' +
    'directory, and XR-03/05/07 hold the offered bytes, owner denial and dirty confirmation (found by item 45: ' +
    'previously exempted by the text-key collision with openBlock\'s chain work; whether a save landing between ' +
    'the two waits is refused is queue item 47)',
  'reconcileBlock/PathChain.run(directory)#2':
    'as the #1 entry: the second of reconcileBlock\'s two chain waits, after the pick (found by item 45: previously ' +
    'exempted by the same text-key collision; queue item 47)',
  'onSaved/PathChain.run':
    'The save persists into the captured working namespace, verifies readback, rechecks the projection and checks live queue store in acceptSave before numbering. Its result uses the captured Saver; XO-01/02/10 and WS-28/31 cover these boundaries.',
  activate:
    'the extension is being built; there is no earlier generation for anything to have changed from',
  showInLanguageOf:
    'it sets the language mode of the document it is handed and decides nothing; its caller, openBlock, ' +
    'captured the store before its first wait and opened that document from it',
  pick: 'it returns the user’s answer and decides nothing; its caller holds the generation',
  confirm: 'as pick',
  open:
    'as pick: it hands the id to the openBlock command and decides nothing. That command ' +
    'captures the store before its own first wait, so a block chosen from a store the user has ' +
    'since left is answered unknown-id by the store they are in -- which is visible, and is not ' +
    'a write going to the wrong place',
  'activate/command(REFRESH_OUTLINE)':
    'its only wait is refreshConflicts, which makes the check itself, and nothing follows it',
  'command/registerCommand':
    'the wrapper every command is registered through (queue item 22): it awaits the command, which ' +
    'keeps its own generation check, and after it only shows what the durability sink holds -- ' +
    'sentences about this window\'s queue files, true whatever the settings did meanwhile',
  'activate/command(SHOW_STATUS)':
    'it shows the status of the store configured NOW, which is what the user asked for; a stale ' +
    'generation is the answer to a question nobody put',
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

/*
 * A NAMED FUNCTION'S NAME, for a function declaration, a method, or a
 * function written as a variable's or a property's value; undefined for any
 * other node.
 */
function functionName(n: ts.Node, src: ts.SourceFile): string | undefined {
  if ((ts.isFunctionDeclaration(n) || ts.isMethodDeclaration(n)) && n.name !== undefined) {
    return n.name.getText(src);
  }
  if ((ts.isArrowFunction(n) || ts.isFunctionExpression(n)) && n.parent !== undefined) {
    if (ts.isVariableDeclaration(n.parent) || ts.isPropertyAssignment(n.parent)) {
      return n.parent.name.getText(src);
    }
  }
  return undefined;
}

/*
 * A DECLARATION'S NAME, with the class or interface that holds it: `PathChain.run`,
 * `Promise.then`, `command`.
 */
function declarationName(declared: ts.Declaration | undefined): string | undefined {
  const name = (declared as ts.NamedDeclaration | undefined)?.name;
  if (declared === undefined || name === undefined) {
    return undefined;
  }
  const own = ts.isIdentifier(name) ? name.text : name.getText();
  const holder = declared.parent;
  if ((ts.isClassDeclaration(holder) || ts.isInterfaceDeclaration(holder)) && holder.name !== undefined) {
    return `${holder.name.text}.${own}`;
  }
  return own;
}

/*
 * A CALLBACK'S KEY: THE FUNCTION IT IS WRITTEN IN AND THE DECLARATION IT IS
 * HANDED TO. (queue item 45, ruled 2026-09-26) It used to be the call's text,
 * so changing an argument's expression -- item 24's W3, the chain keyed by
 * `path.dirname(directory)` -- lost the exception and reddened a cell about the
 * generation check. Now it is `<outer named function>/<the callee's resolved
 * declaration>`: 'openBlock/PathChain.run'. When the outer function holds
 * several calls to that declaration with a function argument (activate's
 * commands), the declaration the first argument's root identifier resolves to
 * is added -- 'activate/command(REFRESH_OUTLINE)' -- so that reordering them
 * changes nothing; an occurrence index only when that root does not resolve.
 */
function keyOfCallback(call: ts.CallExpression, src: ts.SourceFile, checker: ts.TypeChecker): string {
  const declared = checker.getResolvedSignature(call)?.declaration;
  const callee = declarationName(declared) ?? call.expression.getText(src);
  let outer: ts.Node | undefined = call.parent;
  while (outer !== undefined && functionName(outer, src) === undefined) {
    outer = outer.parent;
  }
  const key = `${outer === undefined ? '(top level)' : functionName(outer, src)}/${callee}`;
  const same = (n: ts.CallExpression): boolean =>
    declared !== undefined
      ? checker.getResolvedSignature(n)?.declaration === declared
      : n.expression.getText(src) === call.expression.getText(src);
  const siblings: ts.CallExpression[] = [];
  const look = (n: ts.Node): void => {
    if (ts.isCallExpression(n) && n.arguments.some((a) => isFunction(a)) && same(n)) {
      siblings.push(n);
    }
    ts.forEachChild(n, look);
  };
  look(outer ?? src);
  if (siblings.length <= 1) {
    return key;
  }
  const rootName = (n: ts.CallExpression): string | undefined => {
    let at: ts.Node | undefined = n.arguments[0];
    while (at !== undefined && (ts.isPropertyAccessExpression(at) || ts.isElementAccessExpression(at) || ts.isCallExpression(at))) {
      at = at.expression;
    }
    return at !== undefined && ts.isIdentifier(at) ? checker.getSymbolAtLocation(at)?.name : undefined;
  };
  const mine = rootName(call);
  if (mine === undefined) {
    return `${key}#${siblings.indexOf(call) + 1}`;
  }
  /*
   * AND AN INDEX WHEN TWO SIBLINGS SHARE THAT ROOT TOO: reconcileBlock hands
   * two works to `chain.run(directory, ...)`, one before the pick and one
   * after it.
   */
  const alike = siblings.filter((n) => rootName(n) === mine);
  return alike.length > 1 ? `${key}(${mine})#${alike.indexOf(call) + 1}` : `${key}(${mine})`;
}

/*
 * NOTE: THE CHECKER IS OPTIONAL for a caller that names a function by its own
 * name only (the integrity wiring cell below reads `activate`); a callback is
 * then keyed by its call's text, as before item 45. The wait survey always
 * passes one.
 */
function nameOf(fn: ts.Node, src: ts.SourceFile, checker?: ts.TypeChecker): string {
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
    if (checker !== undefined) {
      return keyOfCallback(parent, src, checker);
    }
    const first = parent.arguments[0];
    return `${parent.expression.getText(src)}(${first !== undefined && first !== fn ? first.getText(src) : ''})`;
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
 * NOTE: A FUNCTION'S OWN WAITS, not those of the functions inside it. An
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
 * NOTE: `for await (... of ...)` IS A WAIT AND THE SURVEY COULD NOT SEE ONE.
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
 * NOTE: A COMPARISON IS NOT A GUARD UNTIL IT STOPS SOMETHING.
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

/*
 * NOTE: AND THE OTHER SHAPE OF THE SAME GUARD: CAPTURE THE THING, COMPARE
 * THE THING.
 *
 * `const mine = saver; await ...; if (saver !== mine) return;` guards
 * exactly what the generation guard guards, and guards it better: section 13
 * settled that the generation says "something changed" while the
 * identity of the object says "the thing this decision depends on
 * changed", and a rebuild for an unrelated setting must not cancel work
 * on a queue that did not move. The first version of this file knew only
 * the generation shape, so the drain `rebuild` schedules -- which uses
 * the better one -- had to be written into the exemption table.
 *
 * NOTE: AN EXEMPTION TABLE THAT GROWS WITH CORRECT CODE IS A TRAP. The next
 * reader takes the table as the list of places that got away with
 * something, and the right way to write this becomes indistinguishable
 * from the wrong way. So the shape is recognised here instead, and the
 * table goes back to holding only what it is for. (The main session
 * asked for this the moment the row appeared.)
 */
function capturedBeforeTheWait(src: ts.SourceFile, before: number): Set<string> {
  /*
   * NOTE: THE CAPTURE IS USUALLY IN THE ENCLOSING FUNCTION, not in the one
   * that waits: `const mine = saver;` sits in `rebuild` and the compare
   * happens inside the callback it schedules. My first version looked
   * only inside the waiting function, found nothing, and asked for an
   * exemption for code that was already right.
   *
   * So every name bound before the wait counts, wherever it was bound.
   * That admits more comparisons than strictly necessary -- and it
   * admits them in the safe direction: this census exists to find waits
   * with NO guard, and the mutation that deletes the comparison still
   * turns it red.
   */
  const names = new Set<string>();
  const walk = (n: ts.Node): void => {
    if (ts.isVariableDeclaration(n) && n.getStart(src) < before && ts.isIdentifier(n.name)) {
      names.add(n.name.text);
    }
    ts.forEachChild(n, walk);
  };
  walk(src);
  return names;
}

/*
 * NOTE: A GENERATION COMPARISON COUNTS ONLY IF THE GENERATION WAS READ
 * BEFORE THE WAIT. Read afterwards it compares the live counter with
 * itself, which is true however much moved. `generationTaken` carries
 * that reading in; the identity form does not need it, because
 * `capturedBeforeTheWait` already refuses a name that was not bound
 * before the wait.
 */
function guardsIn(
  fn: ts.Node,
  src: ts.SourceFile,
  firstWait: number,
  generationTaken: boolean
): ts.IfStatement[] {
  const captured = capturedBeforeTheWait(src, firstWait);
  return within(fn, ts.isIfStatement).filter((s) => {
    const test = s.expression;
    if (!ts.isBinaryExpression(test)) {
      return false;
    }
    const equality =
      test.operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken ||
      test.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsEqualsToken;
    if (!equality || !leaves(s.thenStatement)) {
      return false;
    }
    const sides = [test.left, test.right];
    /*
     * NOTE: A GENERATION GUARD LEAVES WHEN THE GENERATION MOVED, so its
     * comparison is `!==`. The census used to accept `===` as well, and a
     * reviewer turned `checkIntegrity`'s guard around -- `asked ===
     * generation` then `return` -- which drops every current answer and
     * acts on every stale one, and this census went on calling it checked.
     * No guard in the tree was written with `===`, so the other operator
     * was never a shape anybody needed; it was only a way through.
     */
    if (sides.some((side) => /\bgeneration\b/.test(side.getText(src)))) {
      /*
       * NOTE: AND IT COMPARES THE COPY WITH THE COUNTER, NOTHING ELSE. (queue
       * item 16) Mentioning `generation` was enough, so `asked + 1 !==
       * generation` passed: it leaves after every rebuild but one, and acts
       * on the answer that one rebuild made stale. A side has to be a plain
       * name bound before the wait, and the other the counter itself -- the
       * identifier `generation`, or `this.generation()` with no arguments (the
       * shape `getChildren` uses). Review r1 of item 16 found the first
       * version of this too wide one way and too narrow the other: any
       * zero-argument call named `.generation` was a counter, so
       * `({ generation: () => generation - 1 }).generation()` was one; and a
       * parenthesised side, `(asked)`, was not a copy. The receiver is now
       * `this`, and parentheses are looked through.
       */
      const bare = (side: ts.Expression): ts.Expression => {
        let at = side;
        while (ts.isParenthesizedExpression(at)) {
          at = at.expression;
        }
        return at;
      };
      const isCounter = (side: ts.Expression): boolean => {
        const at = bare(side);
        return (
          (ts.isIdentifier(at) && at.text === 'generation') ||
          (ts.isCallExpression(at) &&
            at.arguments.length === 0 &&
            ts.isPropertyAccessExpression(at.expression) &&
            at.expression.name.text === 'generation' &&
            at.expression.expression.kind === ts.SyntaxKind.ThisKeyword)
        );
      };
      const isCopy = (side: ts.Expression): boolean => {
        const at = bare(side);
        return ts.isIdentifier(at) && captured.has(at.text);
      };
      return (
        generationTaken &&
        test.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsEqualsToken &&
        ((isCounter(test.left) && isCopy(test.right)) || (isCounter(test.right) && isCopy(test.left)))
      );
    }
    /*
     * The identity form: both sides are plain names, and one of them was
     * captured before the wait. Comparing a live binding with the copy
     * taken beforehand is the whole guard.
     */
    return (
      sides.every((side) => ts.isIdentifier(side)) &&
      sides.some((side) => captured.has((side as ts.Identifier).text))
    );
  });
}

const EXTENSION = path.join(__dirname, '..', '..', '..', 'src', 'extension.ts');

function extensionText(): string {
  return fs.readFileSync(EXTENSION, 'utf8');
}

/*
 * THE SURVEY READS TEXT IT IS GIVEN, so that a cell can hand it the
 * shipping file with one guard taken out and read what this census then
 * says. A census that can only read the tree has no way to show that it
 * would notice the guard going away.
 */
/*
 * THE GIVEN TEXT OF extension.ts, CHECKED WITH THE REST OF src. (queue item 45)
 * The survey reads text it is handed -- the shipping file, or the shipping
 * file with one guard changed -- and a callback's key is the declaration its
 * call resolves to, which only a program can say. The first program is kept
 * and handed to the next as its old program, so the files that did not change
 * are reused.
 */
let surveyed: ts.Program | undefined;

function checkedExtension(text: string): { src: ts.SourceFile; checker: ts.TypeChecker } {
  const options: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.CommonJS,
    moduleResolution: ts.ModuleResolutionKind.Node10,
    strict: true,
    skipLibCheck: true,
    noEmit: true
  };
  const root = path.dirname(EXTENSION);
  const names = fs
    .readdirSync(root)
    .filter((name) => name.endsWith('.ts'))
    .sort()
    .map((name) => path.join(root, name));
  const host = ts.createCompilerHost(options, true);
  const read = host.getSourceFile.bind(host);
  host.getSourceFile = (fileName, languageVersion, onError, shouldCreate) =>
    path.resolve(fileName) === path.resolve(EXTENSION)
      ? ts.createSourceFile(fileName, text, languageVersion, true)
      : read(fileName, languageVersion, onError, shouldCreate);
  const program = ts.createProgram(names, options, host, surveyed);
  surveyed = surveyed ?? program;
  return { src: program.getSourceFile(EXTENSION) as ts.SourceFile, checker: program.getTypeChecker() };
}

function survey(text: string = extensionText()): Waiting[] {
  const { src, checker } = checkedExtension(text);
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
        const guards = guardsIn(n, src, first, taken).filter((g) => g.getStart(src) > first);
        const lastGuard = guards.reduce((at, g) => Math.max(at, g.getEnd()), -1);
        out.push({
          name: nameOf(n, src, checker),
          line: src.getLineAndCharacterOfPosition(n.getStart(src)).line + 1,
          checked: guards.length > 0,
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
 * NOTE: AND A GUARD ONLY COVERS THE WAITS BEFORE IT.
 *
 * A function may take the generation, wait, check, and then wait AGAIN
 * -- and the second wait resumes with nobody asking. `reconcileBlock`
 * does exactly that: its check sits before a picker the user may leave
 * open for as long as they like. Naming the ones that do it, with the
 * reason, is the same discipline as the table above; a new one has to
 * be argued for rather than joining quietly.
 */
const WAITS_AFTER_ITS_GUARD: Record<string, string> = {
  suggestSplit:
    'its two waits after the guard open and show the review file the core wrote, by the path the core ' +
    'answered with; nothing of the store is read or written after the guard, and the review names the ' +
    'source file it was cut from, so a settings change during the show leaves a tab that is still about ' +
    'that file',
  rootListing:
    'its one wait after the guard records the view mode it decided and sets the title bar\'s key, both ' +
    'started in the same turn as the guard: the mode is written under the key of the store that was just ' +
    'listed, read before anything could move, and nothing is decided after the wait. getChildren drops the ' +
    'listing if the generation moved meanwhile, and the next store\'s listing sets the key for its own mode',
  openBlock:
    'the store is captured BEFORE the first wait and every later step uses the captured one: ' +
    'the publication directory is built from it, so the version and its record are written under ' +
    'the store the block was read from however the settings move meanwhile. THIS ENTRY USED TO ' +
    'SAY the later waits were editor calls and therefore wrote nothing -- which is false: one of ' +
    'them is chain.run(publisher.publish(...)), and it writes a markdown file and a sidecar. An ' +
    'outside review caught the false premise. What survives is the argument above, which is ' +
    'about WHICH store the writing goes to rather than about whether there is any. The source ' +
    'asks for a failing cell before a check is added back',
  openAsDocument:
    'the waits after the guard open and show a document whose address carries the store it was ' +
    'composed from, captured before the first wait; they write nothing, and the text they show ' +
    'is the one composed from that store. A settings change during them leaves a tab that names ' +
    'the store it came from (queue item 6)',
  goToDefinition:
    'its last wait shows the block file the definition target opened, at the line found in that file as it is ' +
    'displayed. The target captured the store before its own first wait (openBlock), checks the generation after ' +
    'each of its waits, and nothing is written after it; a settings change during the show leaves a tab naming ' +
    'the block it came from, as openAsDocument does',
  reconcileBlock:
    'XR-01 activation schedules bind both waits to the selected file directory and preserve ' +
    'the other store on real disk; XR-03 protects the offered bytes, XR-05 makes owner denial ' +
    'visible, and XR-07 refuses dirty confirmation. VS Code and core latency are substituted; ' +
    'the current-file replacement protocol is exercised again by the v20 task'

};

/*
 * NOTE: AND THE DOOR THAT IS LEFT OPEN ON PURPOSE HAS TO BE WATCHED.
 *
 * `Saver` refuses a settler that carries a queue other than its own, and
 * accepts a bare callback carrying none -- deliberately, because the
 * cells in `saver.test.ts` pass one, and what they are about is the
 * sending rather than the recording. The safety of that rests on a
 * sentence: "every settler the extension builds comes from
 * `settlerFor`, so the path that ships is always checked".
 *
 * NOTE: THAT SENTENCE WAS NARRATION. Nothing made it true, and one bare
 * arrow function at the wiring would have unmade it silently -- the same
 * shape as `openBlock`'s exemption, which stated a false premise for two
 * rounds. A review asked for the guard rather than the claim. This is
 * it: in `src/`, the settler handed to a Saver comes from `settlerFor`,
 * read from the syntax rather than from a search for the word.
 */
describe('every saver the extension builds gets a settler that knows its queue', () => {
  it('passes a settlerFor(...) to every new Saver in src', () => {
    const directory = path.join(__dirname, '..', '..', '..', 'src');
    const built: string[] = [];
    const bare: string[] = [];
    for (const name of fs.readdirSync(directory).filter((f) => f.endsWith('.ts'))) {
      const src = ts.createSourceFile(
        name,
        fs.readFileSync(path.join(directory, name), 'utf8'),
        ts.ScriptTarget.ES2022,
        true
      );
      const walk = (n: ts.Node): void => {
        if (ts.isNewExpression(n) && n.expression.getText(src) === 'Saver') {
          const settler = n.arguments?.[2];
          const where = `${name}:${src.getLineAndCharacterOfPosition(n.getStart(src)).line + 1}`;
          const from =
            settler !== undefined &&
            ts.isCallExpression(settler) &&
            settler.expression.getText(src) === 'settlerFor';
          (from ? built : bare).push(where);
        }
        ts.forEachChild(n, walk);
      };
      walk(src);
    }
    /*
     * THE INSTRUMENT'S OWN READING FIRST: a scan that found no Saver at
     * all would pass the check below while saying nothing.
     */
    assert.ok(
      built.length + bare.length > 0,
      'no Saver is constructed anywhere in src, so this cell is about nothing'
    );
    assert.deepStrictEqual(
      bare,
      [],
      'these savers are given a settler that does not come from settlerFor, so nothing checks ' +
        'that it belongs to the queue they lock'
    );
  });
});

/*
 * NOTE: WHAT A HANDLER FORGETS AFTER A WAIT.
 *
 * `pendingSaves` is keyed by block id and outlives every rebuild, so an
 * entry under a key may have been written by a different save -- to
 * another store, or of other bytes. Two places delete from it, and both
 * run after a wait: the settler, and the save handler's `catch`. The
 * settler was repaired a round earlier; the catch was found still
 * deleting unconditionally, and what it threw away was another save's
 * record of what it had sent -- that save's answer could then not
 * recognise its own file and was dequeued with nothing written, leaving
 * the user's saved text a draft nothing would send again. Reproduced in
 * review, in the place I had just repaired.
 *
 * NOTE: AND IT IS CHECKED FROM THE SOURCE BECAUSE THE HANDLER IS NOT
 * REACHABLE, said rather than left as a gap: `onSaved` is built inside
 * `activate` and needs the editor host, so no unit cell drives it, and
 * the mutation that removes this guard survives every suite. What can
 * be read is the shape the repair has: a delete guarded by an identity
 * comparison against the context this save is holding. So that is what
 * is read -- the same instrument as the census below, for the same
 * reason.
 */
/*
 * RETIRED: `nothing forgets a pending save that was not its own`.
 *
 * It held one cell -- `guards every delete from pendingSaves with an
 * identity check` -- and its subject no longer exists. section 13 makes the
 * queue entry the record, so there is no map in memory to delete from
 * and no second supplier of what a save was about; the census said so
 * itself when it went red, in its own words: "extension.ts no longer
 * deletes from pendingSaves at all".
 *
 * NOTE: THE RULE IT GUARDED DID NOT RETIRE WITH IT. What it was for was
 * "an answer must not spend a record that belongs to another save", and
 * that is now asked one layer down and more strictly:
 *
 *   - `keeps no memory of a save outside the queue` (sending.test.ts)
 *     -- a symbol census: `pendingSaves` and `recovered` appear nowhere
 *     in `src`, so the shape cannot come back unnoticed.
 *   - `ignores a context left by a different save of the same block`,
 *     `... by the same bytes saved into another store` and `leaves the
 *     later save's context alone when the earlier answer arrives`
 *     (settling.test.ts) -- each now asserts that the other send's
 *     ENTRY, on disk, still carries its own record.
 *
 * Retired on the main session's ruling, with those named as the
 * successors.
 */

/*
 * NOTE: AND THE ONE EXEMPTION WHOSE ARGUMENT IS ABOUT WHERE THE WRITING
 * GOES, CHECKED WHERE IT CAN BE.
 *
 * `openBlock` waits several times after its only generation check, and
 * one of those waits publishes a file and a record. What makes that
 * acceptable is that the store is captured BEFORE the first wait and the
 * publication directory is built from the captured one, so the writing
 * lands under the store the block was read from.
 *
 * NOTE: THE BEHAVIOURAL CELL FOR THIS DOES NOT EXIST, AND HERE IS WHY,
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

/*
 * AN OPEN TAKES ITS READING ON THE SAVE CHAIN. (queue item 24, C11/S4)
 *
 * An older reading must not overwrite what the user saved. Before the save
 * is recorded, `Publisher` refuses a late publication itself
 * (sequences.test.ts). After it is recorded, `recordWorking` has moved
 * `written` to the saved bytes, and a publication of an older reading
 * passes `Publisher`'s own check. What keeps that reading from existing is
 * order: the open reads the working copy inside the `chain.run` callback
 * keyed by the directory it publishes into, after any save of that
 * directory. Moving the read out, keying the chain by something else, or
 * publishing after the chain each left the whole suite green (measured on
 * a4e4e43, W1-W4). A design that makes `Publisher` refuse on its own is
 * queue item 43.
 *
 * NOTE: TODAY THIS IS A TRIPWIRE, NOT A MEASUREMENT. It reads where calls sit
 * in the source, by the declaration each call resolves to (the type
 * checker, as in receipts.test.ts), and a reference to either method that is
 * not a call is one it cannot follow and fails on. Where reading shape goes
 * wrong, both ways:
 *   - it can stay green on wrong code. It measures shape, not execution: a
 *     read in a closure lexically inside the chain's callback but called
 *     after the callback has returned is green here and wrong.
 *   - it can go red on correct code. A closure defined before `chain.run`
 *     and only called inside it reads on the chain, and is red here (W2h).
 *     If a refactor does that, rewrite this cell against the new shape
 *     rather than deleting it.
 *
 * NOTE: A READ INSIDE `Working` ITSELF IS WHERE ITS METHOD IS CALLED.
 * `Working.write` reads back what it wrote (`this.read`, working.ts), and the
 * first run of this cell reported it as off the chain. It runs on a chain
 * when `write` does, so a call on `this` inside a method of the class is
 * followed to every call of that method, and it is on the chain only if
 * they all are.
 *
 * What this does NOT hold: that the save's key, `path.dirname(file)`, and
 * the open's `directory` are the same path. That is a fact about paths at
 * run time; the syntax cannot show it.
 */
interface Chained {
  checker: ts.TypeChecker;
  files: Array<{ name: string; src: ts.SourceFile }>;
}

let chained: Chained | undefined;

function chainedProgram(): Chained {
  if (chained === undefined) {
    const root = path.join(__dirname, '..', '..', '..', 'src');
    const names = fs
      .readdirSync(root)
      .filter((name) => name.endsWith('.ts'))
      .sort();
    const built = ts.createProgram(
      names.map((name) => path.join(root, name)),
      {
        target: ts.ScriptTarget.ES2022,
        module: ts.ModuleKind.CommonJS,
        moduleResolution: ts.ModuleResolutionKind.Node10,
        strict: true,
        skipLibCheck: true,
        noEmit: true
      }
    );
    chained = {
      checker: built.getTypeChecker(),
      files: names.map((name) => ({ name, src: built.getSourceFile(path.join(root, name)) as ts.SourceFile }))
    };
  }
  return chained;
}

function declaredMethod(files: Chained['files'], owner: string, method: string): ts.Declaration {
  for (const { src } of files) {
    for (const statement of src.statements) {
      if (ts.isClassDeclaration(statement) && statement.name?.text === owner) {
        const found = statement.members.find(
          (m) => m.name !== undefined && ts.isIdentifier(m.name) && m.name.text === method
        );
        if (found !== undefined) {
          return found;
        }
      }
    }
  }
  throw new Error(`${owner}.${method} is not declared in src`);
}

/*
 * THE `chain.run` WHOSE WORK HOLDS THIS NODE: the innermost call resolving to
 * `PathChain.run` whose second argument is a function lexically enclosing it.
 */
function chainHolding(
  checker: ts.TypeChecker,
  run: ts.Declaration,
  node: ts.Node
): { call: ts.CallExpression; work: ts.Node } | undefined {
  for (let at: ts.Node | undefined = node.parent; at !== undefined; at = at.parent) {
    const holder: ts.Node | undefined = at.parent;
    if (
      (ts.isArrowFunction(at) || ts.isFunctionExpression(at)) &&
      holder !== undefined &&
      ts.isCallExpression(holder) &&
      holder.arguments[1] === at &&
      checker.getResolvedSignature(holder)?.declaration === run
    ) {
      return { call: holder, work: at };
    }
  }
  return undefined;
}

/*
 * EVERY CALL TO `method`, and every reference to it that is not a call.
 */
function callsTo(
  checker: ts.TypeChecker,
  files: Chained['files'],
  method: ts.Declaration
): { calls: Array<{ name: string; src: ts.SourceFile; call: ts.CallExpression }>; unreadable: string[] } {
  const calls: Array<{ name: string; src: ts.SourceFile; call: ts.CallExpression }> = [];
  const unreadable: string[] = [];
  const named = (method as ts.NamedDeclaration).name;
  for (const { name, src } of files) {
    const visit = (n: ts.Node): void => {
      if (ts.isCallExpression(n) && checker.getResolvedSignature(n)?.declaration === method) {
        calls.push({ name, src, call: n });
      } else if (ts.isBindingElement(n) && ts.isObjectBindingPattern(n.parent)) {
        const key = n.propertyName ?? n.name;
        const property = ts.isIdentifier(key) ? checker.getTypeAtLocation(n.parent).getProperty(key.text) : undefined;
        if (property !== undefined && (property.declarations ?? []).includes(method)) {
          unreadable.push(`${name}:${lineOf(src, n)} this build cannot read where this call goes`);
        }
      } else if (
        ts.isIdentifier(n) &&
        n !== named &&
        !(ts.isBindingElement(n.parent) && n.parent.name === n) &&
        !ts.isImportSpecifier(n.parent) &&
        !ts.isExportSpecifier(n.parent)
      ) {
        const symbol = checker.getSymbolAtLocation(n);
        /*
         * A CALLEE IS `x.method(...)`'s name, or -- for a function rather than a
         * method (queue item 44: `migrateLegacy`) -- the identifier that is the
         * call's own expression. An import or export of the name is not a use.
         */
        const callee =
          (ts.isPropertyAccessExpression(n.parent) &&
            n.parent.name === n &&
            ts.isCallExpression(n.parent.parent) &&
            n.parent.parent.expression === n.parent) ||
          (ts.isCallExpression(n.parent) && n.parent.expression === n);
        if (symbol !== undefined && (symbol.declarations ?? []).includes(method) && !callee) {
          unreadable.push(`${name}:${lineOf(src, n)} this build cannot read where this call goes`);
        }
      }
      ts.forEachChild(n, visit);
    };
    visit(src);
  }
  return { calls, unreadable };
}

/*
 * WHERE A CALL IS OFF THE CHAIN, as `file:line` entries: none when it is in a
 * chain's work; when it is a call on `this` inside a method of a class, the
 * entries of every call to that method (the call's own line when nothing
 * calls the method, and the method's references when one is not a call);
 * otherwise its own line.
 */
function offTheChain(
  checker: ts.TypeChecker,
  files: Chained['files'],
  run: ts.Declaration,
  name: string,
  src: ts.SourceFile,
  call: ts.CallExpression,
  following: Set<ts.Node>
): string[] {
  if (chainHolding(checker, run, call) !== undefined) {
    return [];
  }
  let method: ts.Node | undefined = call.parent;
  while (method !== undefined && !ts.isFunctionLike(method)) {
    method = method.parent;
  }
  const onThis =
    ts.isPropertyAccessExpression(call.expression) && call.expression.expression.kind === ts.SyntaxKind.ThisKeyword;
  if (method === undefined || !ts.isMethodDeclaration(method) || !onThis || following.has(method)) {
    return [`${name}:${lineOf(src, call)}`];
  }
  following.add(method);
  const { calls: callers, unreadable } = callsTo(checker, files, method);
  if (unreadable.length > 0) {
    return unreadable;
  }
  if (callers.length === 0) {
    return [`${name}:${lineOf(src, call)} (in a method nothing calls)`];
  }
  return callers.flatMap((c) => offTheChain(checker, files, run, c.name, c.src, c.call, following));
}

/*
 * WHERE A PUBLICATION IS OFF THE CHAIN, TRACED THROUGH A NAMED FUNCTION. (queue
 * item 44) As `offTheChain`, but a call outside a chain's work is followed
 * from the nearest enclosing named function or method -- `migrateLegacy`, or
 * `publish` for its `this.publishNow` -- to every call of that function, and
 * is on the chain only if they all are. Callbacks between the call and that
 * function are taken as running within it (the header's limit).
 */
function tracedOffTheChain(
  checker: ts.TypeChecker,
  files: Chained['files'],
  run: ts.Declaration,
  name: string,
  src: ts.SourceFile,
  call: ts.CallExpression,
  following: Set<ts.Node>
): string[] {
  if (chainHolding(checker, run, call) !== undefined) {
    return [];
  }
  let holder: ts.Node | undefined = call.parent;
  while (holder !== undefined && !ts.isFunctionDeclaration(holder) && !ts.isMethodDeclaration(holder)) {
    holder = holder.parent;
  }
  if (holder === undefined || following.has(holder)) {
    return [`${name}:${lineOf(src, call)}`];
  }
  following.add(holder);
  const { calls: callers, unreadable } = callsTo(checker, files, holder as ts.Declaration);
  if (unreadable.length > 0) {
    return unreadable;
  }
  if (callers.length === 0) {
    return [`${name}:${lineOf(src, call)} (in a function nothing calls)`];
  }
  return callers.flatMap((c) =>
    tracedOffTheChain(checker, files, run, c.name, c.src, c.call, following).map(
      (entry) => `${name}:${lineOf(src, call)} through ${entry}`
    )
  );
}

function lineOf(src: ts.SourceFile, node: ts.Node): number {
  return src.getLineAndCharacterOfPosition(node.getStart(src)).line + 1;
}

describe('an open takes its reading on the save chain (queue item 24)', function () {
  /*
   * NOTE: BUILDING THE PROGRAM TAKES SECONDS, once for both cells.
   */
  this.timeout(60000);

  it('reads the working copy only inside the work of a chain.run', () => {
    const { checker, files } = chainedProgram();
    const run = declaredMethod(files, 'PathChain', 'run');
    const { calls, unreadable } = callsTo(checker, files, declaredMethod(files, 'Working', 'read'));
    assert.deepStrictEqual(unreadable, [], 'these references to Working.read are calls this census cannot follow');
    assert.ok(calls.length >= 3, `the census found ${calls.length} Working.read calls, too few to be reading src`);
    const off = calls.flatMap(({ name, src, call }) => offTheChain(checker, files, run, name, src, call, new Set()));
    assert.deepStrictEqual(off, [], 'these Working.read calls are not on a save chain');
  });

  it('publishes only on that chain, after its reading, into the directory the chain is keyed by', () => {
    const { checker, files } = chainedProgram();
    const run = declaredMethod(files, 'PathChain', 'run');
    const read = declaredMethod(files, 'Working', 'read');
    const { calls, unreadable } = callsTo(checker, files, declaredMethod(files, 'Publisher', 'publish'));
    assert.deepStrictEqual(unreadable, [], 'these references to Publisher.publish are calls this census cannot follow');
    assert.ok(calls.length >= 1, 'the census found no Publisher.publish call, so it is reading nothing');
    const wrong: string[] = [];
    for (const { name, src, call } of calls) {
      const at = `${name}:${lineOf(src, call)}`;
      const chain = chainHolding(checker, run, call);
      if (chain === undefined) {
        wrong.push(`${at} publishes off the save chain`);
        continue;
      }
      let readFirst = false;
      const look = (n: ts.Node): void => {
        if (
          ts.isCallExpression(n) &&
          n.getStart(src) < call.getStart(src) &&
          checker.getResolvedSignature(n)?.declaration === read
        ) {
          readFirst = true;
        }
        ts.forEachChild(n, look);
      };
      look(chain.work);
      if (!readFirst) {
        wrong.push(`${at} publishes with no Working.read before it on its chain`);
      }
      const key = chain.call.arguments[0];
      const request = call.arguments[0];
      const given =
        request !== undefined && ts.isObjectLiteralExpression(request)
          ? request.properties.find((p) => p.name !== undefined && ts.isIdentifier(p.name) && p.name.text === 'directory')
          : undefined;
      const directory =
        given === undefined
          ? undefined
          : ts.isShorthandPropertyAssignment(given)
            ? checker.getShorthandAssignmentValueSymbol(given)
            : ts.isPropertyAssignment(given) && ts.isIdentifier(given.initializer)
              ? checker.getSymbolAtLocation(given.initializer)
              : undefined;
      const keyed = ts.isIdentifier(key) ? checker.getSymbolAtLocation(key) : undefined;
      if (directory === undefined || keyed === undefined || directory !== keyed) {
        wrong.push(
          `${at} publishes into ${given?.getText(src) ?? 'a directory this census cannot read'} ` +
            `on a chain keyed by ${key.getText(src)}`
        );
      }
    }
    /*
     * AND `publishNow`, WHICH PUBLISHES THE SAME WAY. (queue item 44) Its one
     * product route onto the chain is migration: migration.ts calls it inside
     * `migrateLegacy`, which is chain work only because its one caller
     * (extension.ts) passes it as `chain.run`'s work; `publish` calls it as
     * `this.publishNow`. Each call is traced through its named function to
     * every call site (see `tracedOffTheChain`), and only "on the chain" is
     * asserted for these: a traced site's reading and directory are the
     * caller's to hold, and `migrateLegacy`'s directory is a parameter.
     *
     * NOTE: A1 COVERS THE MIGRATION CASE TODAY, INDIRECTLY, AND THIS IS KEPT
     * ANYWAY (ruled: a second, independent reading). Moving migration off the
     * chain moves the two `sources` reads with it while they are written
     * inline at the call, and A1 reads that (W6, measured on 4fa919b: red on A1
     * only). If `sources` stops being inline, A1 stops seeing it; this does not.
     */
    const { calls: now, unreadable: unreadNow } = callsTo(checker, files, declaredMethod(files, 'Publisher', 'publishNow'));
    assert.deepStrictEqual(unreadNow, [], 'these references to Publisher.publishNow are calls this census cannot follow');
    assert.ok(now.length >= 3, `the census found ${now.length} publishNow calls, too few to be reading src`);
    for (const { name, src, call } of now) {
      for (const entry of tracedOffTheChain(checker, files, run, name, src, call, new Set())) {
        wrong.push(`${entry} publishes off the save chain`);
      }
    }
    assert.deepStrictEqual(wrong, [], 'these publications are not ordered after a save of their own directory');
  });
});

/*
 * NOTE: TODAY THIS IS A TRIPWIRE, NOT A MEASUREMENT.
 *
 * `IntegrityWatch` decides when a store's condition is asked and told, and
 * its own cells drive it (integrity.test.ts). What they cannot reach is the
 * wiring in `activate`, which needs the editor host: a review showed that
 * the call could be switched off, or the watch rebuilt on every call -- so
 * that "once per session" means once per rebuild -- with every unit cell
 * green. Both are properties of the source's shape, so the shape is read,
 * with the same instrument as the openBlock cell above.
 *
 * What this does NOT hold: that the generation handed to the watch is the
 * live one. `generation: () => 0` has the same shape as the right answer,
 * and the delivery note says so rather than a scanner being written for it.
 *
 * NOTE: AND WHERE READING SHAPE GOES WRONG, both ways, found in review:
 *   - it can go red on correct code. `const make = () => new
 *     IntegrityWatch()` in activate still builds one watch for the session,
 *     but the constructor's enclosing function is then `make`. If a
 *     refactor does that, rewrite this cell against the new shape rather
 *     than deleting it.
 *   - it can stay green on wrong code. `if (false) { checkIntegrity(); }`
 *     is a call in rebuild's syntax that never runs; nothing here judges
 *     whether a call is reached.
 */
describe('the integrity watch is built once for the session and asked on every rebuild', () => {
  const src = ts.createSourceFile('extension.ts', extensionText(), ts.ScriptTarget.ES2022, true);
  const functionNamed = (name: string): ts.Node | undefined => {
    let found: ts.Node | undefined;
    const find = (n: ts.Node): void => {
      if (ts.isFunctionDeclaration(n) && n.name?.getText(src) === name) {
        found = n;
      }
      ts.forEachChild(n, find);
    };
    find(src);
    return found;
  };
  const enclosingFunction = (n: ts.Node): ts.Node | undefined => {
    let at = n.parent;
    while (at !== undefined && !isFunction(at)) {
      at = at.parent;
    }
    return at;
  };

  it('builds one IntegrityWatch, in activate and not in checkIntegrity', () => {
    const built: ts.NewExpression[] = [];
    const walk = (n: ts.Node): void => {
      if (ts.isNewExpression(n) && n.expression.getText(src) === 'IntegrityWatch') {
        built.push(n);
      }
      ts.forEachChild(n, walk);
    };
    walk(src);
    assert.strictEqual(built.length, 1, `extension.ts builds ${built.length} IntegrityWatch objects`);
    const home = enclosingFunction(built[0]);
    assert.ok(home !== undefined, 'the IntegrityWatch is built outside any function');
    assert.strictEqual(
      nameOf(home, src),
      'activate',
      `the IntegrityWatch is built in ${nameOf(home, src)}, so it does not last the whole session`
    );
  });

  it('calls checkIntegrity from rebuild', () => {
    const rebuild = functionNamed('rebuild');
    assert.ok(rebuild !== undefined, 'there is no rebuild in extension.ts any more');
    const calls = within(
      rebuild,
      (n): n is ts.CallExpression => ts.isCallExpression(n) && n.expression.getText(src) === 'checkIntegrity'
    );
    assert.strictEqual(calls.length, 1, `rebuild calls checkIntegrity ${calls.length} times`);
  });
});

/*
 * NOTE: TODAY THIS IS A TRIPWIRE, NOT A MEASUREMENT. (queue item 7, ruled (A))
 *
 * The retry command releases parked saves before it drains -- that is how a
 * save parked for an instance mismatch, which is repaired outside the
 * editor, ever goes again. It does so by calling `Saver.retryParked`, and
 * `Saver.retry` is the one the window's start-up drain calls. The two are
 * one identifier apart at the one place the command is registered, and the
 * unit cells drive the Saver, not the command: a review measured that
 * changing the call back to `retry` leaves every unit cell green. So the
 * shape of that call is read, as the integrity wiring above is.
 *
 * It reads shape, so it shares the blind spots written above: a call that
 * never runs would pass it, and a correct refactor that moves the call into
 * a helper would turn it red -- rewrite it against the new shape then.
 */
describe('the retry command releases parked saves', () => {
  it('calls retryParked, not retry, where RETRY_OUTBOX is registered', () => {
    const src = ts.createSourceFile('extension.ts', extensionText(), ts.ScriptTarget.ES2022, true);
    const registrations: ts.CallExpression[] = [];
    const walk = (n: ts.Node): void => {
      if (
        ts.isCallExpression(n) &&
        n.expression.getText(src) === 'command' &&
        n.arguments[0]?.getText(src) === 'RETRY_OUTBOX.id'
      ) {
        registrations.push(n);
      }
      ts.forEachChild(n, walk);
    };
    walk(src);
    assert.strictEqual(registrations.length, 1, `RETRY_OUTBOX is registered ${registrations.length} times`);
    const handler = registrations[0].arguments[1];
    assert.ok(handler !== undefined && isFunction(handler), 'the RETRY_OUTBOX handler is not a function here');
    const named = (method: string): ts.CallExpression[] =>
      within(
        handler,
        (n): n is ts.CallExpression =>
          ts.isCallExpression(n) &&
          ts.isPropertyAccessExpression(n.expression) &&
          n.expression.name.text === method
      );
    assert.strictEqual(named('retryParked').length, 1, 'the retry command does not call retryParked');
    assert.strictEqual(named('retry').length, 0, 'the retry command calls retry, which leaves parked saves parked');
  });
});

/*
 * EVERY COMMAND IS REGISTERED THROUGH ONE WRAPPER, AND THE WRAPPER SHOWS THE
 * DURABILITY SINK WHEN THE COMMAND ENDS. (queue item 22, ruled Q4)
 *
 * The sink holds what the queue's writes could not promise -- a drain that
 * settled somebody else's save, a settle, a retry -- and nothing shows it
 * unless something takes it. A command registered straight through
 * `vscode.commands.registerCommand` would run its writes and never say.
 * Calls are counted by the declaration they resolve to (the type checker,
 * as the save-chain cells above), and a reference to `registerCommand` that
 * is not a call is one this census cannot follow and fails on.
 *
 * NOTE: A TRIPWIRE, NOT A MEASUREMENT, with the save-chain cells' limits: it
 * reads where calls sit, not whether they run.
 */
describe('every command shows what its queue writes could not promise', function () {
  this.timeout(60000);

  it('registers every command through the wrapper, and the wrapper shows the sink in a finally', () => {
    const { checker, files } = chainedProgram();
    const extension = files.find(({ name }) => name === 'extension.ts');
    assert.ok(extension !== undefined, 'there is no extension.ts in src');
    let register: ts.Declaration | undefined;
    const find = (n: ts.Node): void => {
      if (
        register === undefined &&
        ts.isCallExpression(n) &&
        n.expression.getText(extension.src) === 'vscode.commands.registerCommand'
      ) {
        register = checker.getResolvedSignature(n)?.declaration;
      }
      ts.forEachChild(n, find);
    };
    find(extension.src);
    assert.ok(register !== undefined, 'nothing in extension.ts calls vscode.commands.registerCommand');
    const { calls, unreadable } = callsTo(checker, files, register);
    assert.deepStrictEqual(unreadable, [], 'these references to registerCommand are calls this census cannot follow');
    const around = calls
      .filter(({ call }) => {
        let at: ts.Node | undefined = call.parent;
        while (at !== undefined && !ts.isFunctionDeclaration(at)) {
          at = at.parent;
        }
        return at === undefined || at.name?.text !== 'command';
      })
      .map(({ name, src, call }) => `${name}:${lineOf(src, call)}`);
    assert.deepStrictEqual(around, [], 'these commands are registered around the wrapper, so nothing shows their warnings');
    assert.strictEqual(calls.length, 1, `registerCommand is called ${calls.length} times; the wrapper is the one`);

    const handler = calls[0].call.arguments[1];
    assert.ok(handler !== undefined && isFunction(handler), 'the wrapper does not register a function');
    const finals = within(handler, (n): n is ts.TryStatement => ts.isTryStatement(n) && n.finallyBlock !== undefined);
    assert.strictEqual(finals.length, 1, 'the wrapper has no try with a finally around the command');
    /*
     * NOTE: AS A STATEMENT OF THE FINALLY ITSELF, not anywhere inside it.
     * (delivery review r3 of item 22, S2) A `showDurability()` under a
     * condition -- `if (succeeded)` -- passed the first version of this, and
     * a command that rejected then showed nothing.
     */
    const shows = ((finals[0] as ts.TryStatement).finallyBlock as ts.Block).statements.filter(
      (statement) =>
        ts.isExpressionStatement(statement) &&
        ts.isCallExpression(statement.expression) &&
        statement.expression.expression.getText(extension.src) === 'showDurability'
    );
    assert.strictEqual(shows.length, 1, 'the wrapper\'s finally does not show the durability sink unconditionally');
    let commands = 0;
    const count = (n: ts.Node): void => {
      if (ts.isCallExpression(n) && n.expression.getText(extension.src) === 'command') {
        commands += 1;
      }
      ts.forEachChild(n, count);
    };
    count(extension.src);
    assert.ok(commands >= 9, `only ${commands} commands are registered through the wrapper`);
  });
});

/*
 * NOTE: A MINUTE, AS THE OTHER SUITES THAT BUILD A PROGRAM HAVE. Several cells
 * here type-check the source again for each rewrite they try; on a CI runner
 * one of them took more than mocha's two seconds.
 */
describe('every wait in the extension host knows what may have changed under it', function () {
  this.timeout(60000);
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
   * NOTE: THE DRAIN IS CALLED CHECKED BECAUSE OF WHAT IT DOES, NOT BECAUSE
   * OF WHAT IS WRITTEN ABOUT IT.
   *
   * The drain `rebuild` schedules carries the identity form of the
   * guard: `const draining = saver` before the wait, `saver !== draining`
   * after it. That shape spent a round in the exemption table -- a table
   * whose entries mean "this one got away with something" -- until the
   * main session pointed out that an exemption which grows as the code
   * gets BETTER teaches the next reader the wrong lesson.
   *
   * So the census recognises the shape, and this cell is what stands
   * behind that recognition: the shipping file is read, the comparison
   * is taken out of the text, and the census is asked again. If it goes
   * on calling the drain checked with the guard gone, then it is
   * recognising the drain rather than the guard, and the row it no
   * longer needs in the table was doing the work all along.
   */
  it('stops calling the drain checked once its identity comparison is taken out', () => {
    const text = extensionText();
    const guard = 'if (saver !== draining) {\n        return;\n      }\n      paint();';
    assert.ok(
      text.includes(guard),
      'the drain no longer holds the guard this cell takes out; if it was rewritten, rewrite ' +
        'this cell against the new shape rather than deleting it'
    );
    const drainIn = (over: Waiting[]): Waiting | undefined =>
      over.find((w) => w.name === 'rebuild/Promise.then');

    const asShipped = drainIn(survey(text));
    assert.ok(asShipped !== undefined, 'the census did not find the drain in the shipping file');
    assert.strictEqual(
      asShipped.checked,
      true,
      'the census does not recognise the identity guard the drain carries'
    );

    const withoutTheGuard = drainIn(survey(text.replace(guard, 'paint();')));
    assert.ok(
      withoutTheGuard !== undefined,
      'the census lost sight of the drain altogether when the guard was removed, so this cell ' +
        'is no longer reading what it claims to read'
    );
    assert.strictEqual(
      withoutTheGuard.checked,
      false,
      'the census still calls the drain checked with its only guard deleted, so it is ' +
        'recognising the function rather than the guard'
    );
  });

  /*
   * NOTE: AND A GUARD TURNED AROUND IS NOT A GUARD. `asked === generation`
   * followed by `return` leaves exactly when nothing moved, so it throws
   * away the current answer and keeps the stale one -- while having the
   * shape of the guard in every other respect. Same instrument as the drain
   * cell above: the shipping file, one operator changed, the census asked
   * again.
   */
  it('stops calling refreshConflicts checked once its guard is turned around', () => {
    const text = extensionText();
    const guard = 'if (asked !== generation) {\n      return;\n    }\n    conflicts = found;';
    assert.ok(
      text.includes(guard),
      'refreshConflicts no longer holds the guard this cell turns around; if it was rewritten, ' +
        'rewrite this cell against the new shape rather than deleting it'
    );
    const refreshIn = (over: Waiting[]): Waiting | undefined =>
      over.find((w) => w.name === 'refreshConflicts');

    const asShipped = refreshIn(survey(text));
    assert.ok(asShipped !== undefined, 'the census did not find refreshConflicts in the shipping file');
    assert.strictEqual(asShipped.checked, true, 'the census does not recognise refreshConflicts\' guard');

    const turned = refreshIn(
      survey(text.replace(guard, guard.replace('asked !== generation', 'asked === generation')))
    );
    assert.ok(
      turned !== undefined,
      'the census lost sight of refreshConflicts altogether when its guard was turned around'
    );
    assert.strictEqual(
      turned.checked,
      false,
      'the census still calls refreshConflicts checked with its guard turned around, so it is ' +
        'counting a comparison rather than one that leaves when the generation moved'
    );
  });

  /*
   * NOTE: AND A GUARD THAT COMPARES SOMETHING ELSE IS NOT A GUARD. (queue
   * item 16) `asked + 1 !== generation` then `return` has the operator, the
   * word and the leaving branch, and acts on the answer exactly one rebuild
   * made stale. The shipping file, that one comparison changed, the census
   * asked again. The twin: `getChildren`'s `asked !== this.generation()` is
   * the counter read through a method, and still a guard.
   */
  it('stops calling refreshConflicts checked once its guard compares the copy plus one', () => {
    const text = extensionText();
    const guard = 'if (asked !== generation) {\n      return;\n    }\n    conflicts = found;';
    assert.ok(text.includes(guard), 'refreshConflicts no longer holds the guard this cell rewrites');
    const named = (over: Waiting[], name: string): Waiting | undefined => over.find((w) => w.name === name);
    const shifted = named(
      survey(text.replace(guard, guard.replace('asked !== generation', 'asked + 1 !== generation'))),
      'refreshConflicts'
    );
    assert.ok(shifted !== undefined, 'the census lost sight of refreshConflicts');
    assert.strictEqual(
      shifted.checked,
      false,
      'the census still calls refreshConflicts checked when its guard compares the copy plus one'
    );
    const children = named(survey(text), 'getChildren');
    assert.ok(children !== undefined, 'the census did not find getChildren');
    assert.ok(
      text.includes('asked !== this.generation()'),
      'getChildren no longer compares through this.generation(); rewrite this twin against its shape'
    );
    assert.strictEqual(children.checked, true, 'a guard reading the counter through a method is no longer seen');
  });

  /*
   * NOTE: THE SHAPES AROUND IT. (review r1 of item 16) The same guard,
   * rewritten one way at a time and the census asked again: parentheses are
   * looked through, a guard with its sides the other way round is held to the
   * same rule, a counter read from anything but `this` is not the counter, and
   * a `generation` call with an argument is not the counter either.
   */
  it('reads a guard by what it compares, however it is written around', () => {
    const text = extensionText();
    const guard = 'if (asked !== generation) {\n      return;\n    }\n    conflicts = found;';
    assert.ok(text.includes(guard), 'refreshConflicts no longer holds the guard this cell rewrites');
    const judged = (comparison: string): boolean | undefined =>
      survey(text.replace(guard, guard.replace('asked !== generation', comparison))).find(
        (w) => w.name === 'refreshConflicts'
      )?.checked;
    const cases: Array<[string, boolean]> = [
      ['(asked) !== generation', true],
      ['generation !== (asked)', true],
      ['generation !== asked', true],
      ['generation !== asked + 1', false],
      ['asked !== ({ generation: () => generation - 1 }).generation()', false],
      ['asked !== this.generation(1)', false]
    ];
    for (const [comparison, checked] of cases) {
      assert.strictEqual(judged(comparison), checked, `\`${comparison}\` read as ${checked ? 'not ' : ''}checked`);
    }
  });

  /*
   * AND THE LIST DOES NOT OUTLIVE ITS ENTRIES. An exception for a
   * function that now checks -- or that no longer exists -- is a licence
   * nobody asked for, kept where the next reader will believe it.
   */
  /*
   * NOTE: THE RULE ASKS WHETHER THE COMPARISON STOPS ANYTHING. It used to
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
