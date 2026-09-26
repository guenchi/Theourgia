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
 * queue item 43 (design v3 U4, ruling Q5): EVERY WRITE OF A RECORD GOES
 * THROUGH `writeSidecar`, WHAT ONLY THE SOURCE CAN SHOW.
 *
 * NOTE: A SOURCE CENSUS, AND IT SAYS SO. The revision a publication names is
 * computed by `writeSidecar` from the record on disk, so no caller of it can
 * skip the bump; a write of the record that does not go through it can. The
 * R0 rows in publication.test.ts drive every writer there is today and see
 * +1; they cannot see a writer added tomorrow. No running cell can.
 *
 * NOTE: READ FROM WHERE THE PATH IS MADE, NOT FROM WHERE IT IS WRITTEN. The
 * ruling's set is every `replaceText`, and every FileOps `writeText`,
 * `writeDurably` and `rename`, whose path contains `sidecarPathOf`. The same
 * set is read here from its other end: every call of `sidecarPathOf` in src is
 * followed forward -- into the variables that hold it, the parameters it is
 * passed to, the callers of a function that returns it -- to every place it
 * is used. A use is a read (a FileOps read, a comparison, a string method),
 * a write (`replaceText`, or any FileOps member that is not a read), or
 * unreadable (anything this follower does not know, reported as such and
 * never as clean: the open-set rule). The path made inside `writeSidecar` must
 * reach a write, and no other path may reach one.
 *
 * NOTE: WHAT IS OUTSIDE THE RULING'S SCOPE, SAID HERE SO IT IS NOT MISTAKEN
 * FOR COVERED. A record path spelled without `sidecarPathOf` (sessions.ts
 * spells `${found.file}.meta` for an ownership claim, which is not a write)
 * is not followed; neither is a rename of the whole block directory
 * (migration.ts, sessions.ts), which moves records without writing one.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';

const SRC = path.join(__dirname, '..', '..', '..', 'src');

const READS = new Set(['readText', 'readBytes', 'exists', 'presenceOf', 'list', 'readDirectory', 'isDirectory']);

interface Checked {
  checker: ts.TypeChecker;
  files: ts.SourceFile[];
}

let reused: ts.Program | undefined;

/*
 * THE SOURCES AS A PROGRAM, with one file's text replaced when a probe asks.
 * The first program is handed to the next as its old program, so the files
 * that did not change are reused.
 */
function checkedSources(replaced: { name: string; text: string } | null = null): Checked {
  const options: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.CommonJS,
    moduleResolution: ts.ModuleResolutionKind.Node10,
    strict: true,
    skipLibCheck: true,
    noEmit: true
  };
  const names = fs
    .readdirSync(SRC)
    .filter((name) => name.endsWith('.ts'))
    .sort()
    .map((name) => path.join(SRC, name));
  const host = ts.createCompilerHost(options, true);
  const read = host.getSourceFile.bind(host);
  host.getSourceFile = (fileName, languageVersion, onError, shouldCreate) =>
    replaced !== null && path.resolve(fileName) === path.join(SRC, replaced.name)
      ? ts.createSourceFile(fileName, replaced.text, languageVersion, true)
      : read(fileName, languageVersion, onError, shouldCreate);
  const program = ts.createProgram(names, options, host, reused);
  reused = reused ?? program;
  return { checker: program.getTypeChecker(), files: names.map((name) => program.getSourceFile(name) as ts.SourceFile) };
}

function every(node: ts.Node, seen: (inner: ts.Node) => void): void {
  const walk = (inner: ts.Node): void => {
    seen(inner);
    inner.forEachChild(walk);
  };
  walk(node);
}

function where(node: ts.Node): string {
  const src = node.getSourceFile();
  return `${path.basename(src.fileName)}:${src.getLineAndCharacterOfPosition(node.getStart(src)).line + 1}`;
}

function calleeName(callee: ts.Expression): ts.Node {
  return ts.isPropertyAccessExpression(callee) ? callee.name : callee;
}

function declarationOf(checker: ts.TypeChecker, node: ts.Node): ts.Declaration | undefined {
  let symbol = checker.getSymbolAtLocation(node);
  if (symbol !== undefined && (symbol.flags & ts.SymbolFlags.Alias) !== 0) {
    symbol = checker.getAliasedSymbol(symbol);
  }
  return symbol?.valueDeclaration ?? symbol?.declarations?.[0];
}

/*
 * WHAT A RESULT TAKEN FROM A CARRIED PATH IS. (review r2 #2) A string may
 * still be the path; a boolean or a number only says something about it;
 * anything else -- an array, an object -- may hold the path where this
 * follower does not go.
 */
function resultKind(checker: ts.TypeChecker, node: ts.Node): 'string' | 'scalar' | 'other' {
  const type = checker.getTypeAtLocation(node);
  const parts = type.isUnion() ? type.types : [type];
  if (parts.some((part) => (part.flags & ts.TypeFlags.StringLike) !== 0)) {
    return 'string';
  }
  const scalar = ts.TypeFlags.BooleanLike | ts.TypeFlags.NumberLike | ts.TypeFlags.Undefined | ts.TypeFlags.Null;
  return parts.every((part) => (part.flags & scalar) !== 0) ? 'scalar' : 'other';
}

/*
 * THE DECLARATION A NAME STANDS FOR AS A VALUE. (review r2 #1) A shorthand
 * property (`{ sidecarPathOf }`) names the property, not the function; the
 * function is its value symbol.
 */
function valueDeclarationOf(checker: ts.TypeChecker, node: ts.Identifier): ts.Declaration | undefined {
  if (!ts.isShorthandPropertyAssignment(node.parent)) {
    return declarationOf(checker, node);
  }
  let symbol = checker.getShorthandAssignmentValueSymbol(node.parent);
  if (symbol !== undefined && (symbol.flags & ts.SymbolFlags.Alias) !== 0) {
    symbol = checker.getAliasedSymbol(symbol);
  }
  return symbol?.valueDeclaration ?? symbol?.declarations?.[0];
}

function inSource(declaration: ts.Node): boolean {
  return path.dirname(path.resolve(declaration.getSourceFile().fileName)) === path.resolve(SRC);
}

function isFunctionNamed(declaration: ts.Node | undefined, file: string, name: string): boolean {
  return (
    declaration !== undefined &&
    ts.isFunctionDeclaration(declaration) &&
    declaration.name?.text === name &&
    path.basename(declaration.getSourceFile().fileName) === file
  );
}

/*
 * WHAT A CALL DOES WITH A PATH, when it is one of the file operations: a
 * FileOps member (the interface's own declaration, which every `files.x`
 * call in src resolves to) or `replaceText`. Null for any other call.
 */
function operationOf(declaration: ts.Declaration): { kind: 'read' | 'write'; name: string } | null {
  if (isFunctionNamed(declaration, 'temporary.ts', 'replaceText')) {
    return { kind: 'write', name: 'replaceText' };
  }
  if (
    ts.isMethodSignature(declaration) &&
    ts.isIdentifier(declaration.name) &&
    ts.isInterfaceDeclaration(declaration.parent) &&
    declaration.parent.name.text === 'FileOps'
  ) {
    const name = declaration.name.text;
    return { kind: READS.has(name) ? 'read' : 'write', name };
  }
  return null;
}

interface Use {
  kind: 'read' | 'write' | 'unreadable';
  at: string;
  how: string;
}

/*
 * ONE PATH, FOLLOWED FORWARD TO EVERY USE. Each expression is visited once,
 * so a cycle (a function passing the path to itself) ends.
 */
class Follower {
  public readonly uses: Use[] = [];
  private readonly seen = new Set<ts.Node>();

  constructor(private readonly checked: Checked) {}

  public carry(value: ts.Expression): void {
    if (this.seen.has(value)) {
      return;
    }
    this.seen.add(value);
    const parent = value.parent;
    if (
      ts.isParenthesizedExpression(parent) ||
      ts.isAsExpression(parent) ||
      ts.isNonNullExpression(parent) ||
      ts.isSatisfiesExpression(parent)
    ) {
      return this.carry(parent);
    }
    if (ts.isVariableDeclaration(parent) && parent.initializer === value) {
      return ts.isIdentifier(parent.name) ? this.references(parent.name) : this.note('unreadable', value, 'destructured');
    }
    if (ts.isBinaryExpression(parent)) {
      const operator = parent.operatorToken.kind;
      if (operator === ts.SyntaxKind.EqualsToken && parent.right === value) {
        return ts.isIdentifier(parent.left)
          ? this.references(parent.left)
          : this.note('unreadable', value, `stored in ${parent.left.getText()}`);
      }
      if (
        operator === ts.SyntaxKind.EqualsEqualsEqualsToken ||
        operator === ts.SyntaxKind.ExclamationEqualsEqualsToken ||
        operator === ts.SyntaxKind.EqualsEqualsToken ||
        operator === ts.SyntaxKind.ExclamationEqualsToken
      ) {
        return this.note('read', value, 'compared');
      }
      if (
        operator === ts.SyntaxKind.PlusToken ||
        operator === ts.SyntaxKind.QuestionQuestionToken ||
        operator === ts.SyntaxKind.BarBarToken
      ) {
        return this.carry(parent);
      }
    }
    if (ts.isConditionalExpression(parent) && parent.condition !== value) {
      return this.carry(parent);
    }
    if (ts.isTemplateSpan(parent)) {
      return this.carry(parent.parent);
    }
    /*
     * A STRING METHOD'S RESULT IS STILL THE PATH, or a piece of it (review r1
     * #2): `sidecarPathOf(file).slice(0)` is the record's path. A boolean or a
     * number -- `endsWith`, `length` -- only reads it. Anything else is
     * unreadable (review r2 #2): `split` returns an array that still holds
     * the path.
     */
    if (ts.isPropertyAccessExpression(parent) && parent.expression === value) {
      const call = parent.parent;
      const result = ts.isCallExpression(call) && call.expression === parent ? call : parent;
      const kind = resultKind(this.checked.checker, result);
      if (kind === 'string') {
        return this.carry(result);
      }
      return kind === 'scalar'
        ? this.note('read', value, `.${parent.name.text}`)
        : this.note('unreadable', value, `.${parent.name.text}, whose result is neither a string nor a boolean or number`);
    }
    if (ts.isCallExpression(parent) && parent.arguments.some((argument) => argument === value)) {
      return this.argument(parent, parent.arguments.indexOf(value), value);
    }
    if (ts.isReturnStatement(parent) || (ts.isArrowFunction(parent) && parent.body === value)) {
      return this.returned(value);
    }
    this.note('unreadable', value, `used as ${ts.SyntaxKind[parent.kind]}`);
  }

  private note(kind: Use['kind'], node: ts.Node, how: string): void {
    this.uses.push({ kind, at: where(node), how });
  }

  /*
   * EVERY LATER MENTION OF A LOCAL NAME OR PARAMETER. A name is local to its
   * file, so only that file is read. A mention that is the left side of a
   * reassignment is not a use of the value; a shorthand property (`{ meta }`)
   * is one, and it stores the path where this follower does not go.
   */
  private references(name: ts.Identifier): void {
    const { checker } = this.checked;
    const symbol = checker.getSymbolAtLocation(name);
    if (symbol === undefined) {
      return this.note('unreadable', name, 'a name with no symbol');
    }
    every(name.getSourceFile(), (node) => {
      if (!ts.isIdentifier(node) || node === name) {
        return;
      }
      if (ts.isShorthandPropertyAssignment(node.parent) && checker.getShorthandAssignmentValueSymbol(node.parent) === symbol) {
        return this.note('unreadable', node, 'stored in an object');
      }
      if (checker.getSymbolAtLocation(node) !== symbol) {
        return;
      }
      if (
        ts.isBinaryExpression(node.parent) &&
        node.parent.left === node &&
        node.parent.operatorToken.kind === ts.SyntaxKind.EqualsToken
      ) {
        return;
      }
      this.carry(node);
    });
  }

  private argument(call: ts.CallExpression, index: number, value: ts.Expression): void {
    const declaration = declarationOf(this.checked.checker, calleeName(call.expression));
    if (declaration === undefined) {
      return this.note('unreadable', value, `passed to ${call.expression.getText()}, which resolves to nothing`);
    }
    const operation = operationOf(declaration);
    if (operation !== null) {
      return this.note(operation.kind, value, operation.name);
    }
    if (!inSource(declaration)) {
      return this.carry(call);
    }
    if (!ts.isFunctionLike(declaration)) {
      return this.note('unreadable', value, `passed to ${call.expression.getText()}, which is not a function`);
    }
    /*
     * A SIGNATURE WITH NO BODY (an interface member other than FileOps's) is
     * implemented where this follower cannot see; its parameter has no
     * mentions to follow, and the path would vanish without a word.
     */
    if (!('body' in declaration) || declaration.body === undefined) {
      return this.note('unreadable', value, `passed to ${call.expression.getText()}, a signature with no body`);
    }
    const parameter = declaration.parameters[index];
    if (parameter === undefined || parameter.dotDotDotToken !== undefined || !ts.isIdentifier(parameter.name)) {
      return this.note('unreadable', value, `passed to ${call.expression.getText()} where no plain parameter takes it`);
    }
    return this.references(parameter.name);
  }

  /*
   * RETURNED: EVERY CALL OF THE FUNCTION CARRIES IT. A function that is also
   * named as a value (handed on, stored) may be called where this follower
   * cannot see, so that is unreadable; so is one nothing in src calls.
   */
  private returned(value: ts.Expression): void {
    let inner: ts.Node = value.parent;
    while (!ts.isFunctionLike(inner)) {
      inner = inner.parent;
    }
    const target: ts.Node | undefined =
      ts.isArrowFunction(inner) || ts.isFunctionExpression(inner)
        ? ts.isVariableDeclaration(inner.parent)
          ? inner.parent
          : undefined
        : inner;
    if (target === undefined) {
      return this.note('unreadable', value, 'returned from a function with no name');
    }
    const named = (target as ts.NamedDeclaration).name;
    let calls = 0;
    for (const src of this.checked.files) {
      every(src, (node) => {
        if (!ts.isIdentifier(node) || node === named || declarationOf(this.checked.checker, node) !== target) {
          return;
        }
        const access = ts.isPropertyAccessExpression(node.parent) && node.parent.name === node ? node.parent : node;
        if (ts.isCallExpression(access.parent) && access.parent.expression === access) {
          calls += 1;
          this.carry(access.parent);
        } else {
          this.note('unreadable', node, 'returned from a function that is also named as a value');
        }
      });
    }
    if (calls === 0) {
      this.note('unreadable', value, 'returned from a function nothing in src calls');
    }
  }
}

interface Origin {
  at: string;
  inDoor: boolean;
  uses: Use[];
}

/*
 * EVERY CALL OF `sidecarPathOf` IN src, AND WHERE ITS PATH GOES. (review r1
 * #3) Found by declaration, not by spelling: every identifier in src that
 * resolves to the function is looked at. A call of it is an origin; a const
 * that holds it is an alias whose every call is an origin; an import of it
 * (under any name) is how a file reaches it; anything else -- the function
 * handed on, stored, re-exported, an alias that is not a const -- is a use
 * this census cannot follow, and is reported in `asValues`.
 */
function census(checked: Checked): { origins: Origin[]; asValues: string[] } {
  const { checker, files } = checked;
  const publication = files.find((src) => path.basename(src.fileName) === 'publication.ts') as ts.SourceFile;
  const door = publication.statements.find((statement) => isFunctionNamed(statement, 'publication.ts', 'writeSidecar'));
  assert.ok(door !== undefined, 'writeSidecar was not found in publication.ts, so nothing below is measured against it');
  const target = publication.statements.find((statement) => isFunctionNamed(statement, 'publication.ts', 'sidecarPathOf'));
  assert.ok(target !== undefined && ts.isFunctionDeclaration(target), 'sidecarPathOf was not found in publication.ts');
  const origins: Origin[] = [];
  const asValues: string[] = [];
  const origin = (call: ts.CallExpression): void => {
    const follower = new Follower(checked);
    follower.carry(call);
    origins.push({
      at: where(call),
      inDoor: call.getSourceFile() === publication && call.pos >= door.pos && call.end <= door.end,
      uses: follower.uses
    });
  };
  const calledOrNot = (name: ts.Identifier, what: string): void => {
    const used = ts.isPropertyAccessExpression(name.parent) && name.parent.name === name ? name.parent : name;
    if (ts.isCallExpression(used.parent) && used.parent.expression === used) {
      origin(used.parent);
    } else {
      asValues.push(`${where(name)} ${what}`);
    }
  };
  for (const src of files) {
    every(src, (node) => {
      if (!ts.isIdentifier(node) || node === target.name || valueDeclarationOf(checker, node) !== target) {
        return;
      }
      if (ts.isImportSpecifier(node.parent) || ts.isTypeQueryNode(node.parent)) {
        return;
      }
      const alias = node.parent;
      if (
        ts.isVariableDeclaration(alias) &&
        alias.initializer === node &&
        ts.isIdentifier(alias.name) &&
        (ts.getCombinedNodeFlags(alias) & ts.NodeFlags.Const) !== 0
      ) {
        const symbol = checker.getSymbolAtLocation(alias.name);
        for (const other of files) {
          every(other, (inner) => {
            const named = ts.isIdentifier(inner) && ts.isShorthandPropertyAssignment(inner.parent)
              ? checker.getShorthandAssignmentValueSymbol(inner.parent)
              : checker.getSymbolAtLocation(inner);
            if (ts.isIdentifier(inner) && inner !== alias.name && symbol !== undefined && named === symbol) {
              calledOrNot(inner, `the alias ${alias.name.getText()} named as a value`);
            }
          });
        }
        return;
      }
      calledOrNot(node, 'sidecarPathOf named as a value');
    });
  }
  return { origins, asValues };
}

function outside(origins: Origin[]): Array<{ at: string; use: Use }> {
  return origins
    .filter((origin) => !origin.inDoor)
    .flatMap((origin) => origin.uses.filter((use) => use.kind !== 'read').map((use) => ({ at: origin.at, use })));
}

describe('queue item 43 every write of a record goes through writeSidecar (source census)', () => {
  it('follows every path sidecarPathOf makes, and only the one made in writeSidecar is written', () => {
    const { origins, asValues } = census(checkedSources());
    assert.deepStrictEqual(asValues, [], 'sidecarPathOf is used where this census cannot follow it');
    const inDoor = origins.filter((origin) => origin.inDoor);
    assert.strictEqual(inDoor.length, 1, `writeSidecar makes ${inDoor.length} record paths, not one: ${JSON.stringify(inDoor)}`);
    assert.ok(
      inDoor[0].uses.some((use) => use.kind === 'write' && use.how === 'replaceText'),
      `the path made in writeSidecar was never followed to its replaceText, so the follower reaches no write: ${JSON.stringify(inDoor[0].uses)}`
    );
    assert.ok(
      origins.length > 1,
      `only ${origins.length} call of sidecarPathOf was found, so the readers were never followed and "no other writes" says nothing`
    );
    assert.deepStrictEqual(
      outside(origins),
      [],
      `a record path made outside writeSidecar reaches a write, or a use this census cannot read (${origins.length} paths followed)`
    );
  });

  /*
   * THE CENSUS'S OWN FIRST READINGS. saving.ts changed so that a record path
   * reaches a write without writeSidecar, three ways: through a variable and a
   * parameter (one writer replaced by a helper that writes the reader's path);
   * through a string method (review r1 #2); through an alias (review r1 #3).
   * Each must be named as a write -- not as an unreadable use, which would be
   * finding the change for a reason other than the one the census exists for.
   */
  function censusOf(change: (text: string) => string): { origins: Origin[]; asValues: string[] } {
    const text = fs.readFileSync(path.join(SRC, 'saving.ts'), 'utf8');
    const changed = change(text);
    assert.notStrictEqual(changed, text, 'the probe did not change saving.ts, so it measures nothing');
    return census(checkedSources({ name: 'saving.ts', text: changed }));
  }

  function probed(change: (text: string) => string): Array<{ at: string; use: Use }> {
    const { origins, asValues } = censusOf(change);
    assert.deepStrictEqual(asValues, [], 'the probe was read as a use the census cannot follow');
    return outside(origins);
  }

  function namesTheWrite(found: Array<{ at: string; use: Use }>): void {
    assert.ok(
      found.some(({ at, use }) => at.startsWith('saving.ts:') && use.kind === 'write' && use.how === 'writeText'),
      `the direct write was not named as a write: ${JSON.stringify(found)}`
    );
  }

  it('names a write of a record path that goes around writeSidecar through a parameter', () => {
    const anchor =
      'writeSidecar(this.files,file,{...read.sidecar,outstanding:read.sidecar.outstanding.filter(o=>!(o.seq===answer.send.seq && o.req===answer.req))});';
    namesTheWrite(
      probed((text) => {
        assert.strictEqual(text.split(anchor).length - 1, 1, 'the probe\'s anchor in saving.ts does not occur exactly once');
        return (
          text.replace(anchor, 'installRecord(this.files, meta);') +
          '\nfunction installRecord(files: FileOps, where: string): void {\n  files.writeText(where, \'{}\');\n}\n'
        );
      })
    );
  });

  it('names a write of a record path taken through a string method', () => {
    namesTheWrite(
      probed(
        (text) =>
          text + "\nexport function missed(files: FileOps, file: string): void {\n  files.writeText(sidecarPathOf(file).slice(0), '{}');\n}\n"
      )
    );
  });

  it('names a write of a record path made through an alias of sidecarPathOf', () => {
    namesTheWrite(
      probed(
        (text) =>
          text +
          "\nconst recordPath = sidecarPathOf;\nexport function missed(files: FileOps, file: string): void {\n  files.writeText(recordPath(file), '{}');\n}\n"
      )
    );
  });

  /*
   * AND TWO THAT CANNOT READ AS A WRITE, SO THEY MUST READ AS UNREADABLE.
   * (review r2) A path function stored in an object, and a path taken apart
   * into an array, leave where this census follows; each must be reported,
   * not passed as clean.
   */
  it('reports sidecarPathOf stored in an object as a use it cannot follow', () => {
    const { asValues } = censusOf(
      (text) =>
        text +
        "\nexport function missed(files: FileOps, file: string): void {\n  const paths = { sidecarPathOf };\n  files.writeText(paths.sidecarPathOf(file), '{}');\n}\n"
    );
    assert.ok(
      asValues.some((entry) => entry.startsWith('saving.ts:')),
      `the object holding sidecarPathOf was not reported: ${JSON.stringify(asValues)}`
    );
  });

  it('reports a record path taken apart into an array as a use it cannot follow', () => {
    const { origins, asValues } = censusOf(
      (text) =>
        text + "\nexport function missed(files: FileOps, file: string): void {\n  files.writeText(sidecarPathOf(file).split('\\0')[0], '{}');\n}\n"
    );
    assert.deepStrictEqual(asValues, []);
    assert.ok(
      outside(origins).some(({ at, use }) => at.startsWith('saving.ts:') && use.kind === 'unreadable' && use.how.startsWith('.split')),
      `the array taken from the path was not reported: ${JSON.stringify(outside(origins))}`
    );
  });
});
