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
 * plugin-r3 item 3: THE ENQUEUE RECEIPT, WHAT ONLY THE SOURCE CAN SHOW.
 *
 * NOTE: EVERY CELL HERE IS A SOURCE CENSUS, AND SAYS SO. (condition 7 of the
 * approved design) Whether a receipt can be made outside `outbox.ts`, whether
 * it is issued inside the lock and after the commit, and whether each call
 * site holds the receipt it is handed, are properties of where statements
 * sit. No running cell can witness them; whoever deletes one of these should
 * not think a runtime cell is still holding the property. The runtime cells
 * are in sending.test.ts ("plugin-r3 3 the enqueue receipt decides whether
 * the number goes back") and sessions.test.ts (the addressed receipt on the
 * import path).
 *
 * NOTE: TWO INTERFACE CHANGES, TWO CALL-SITE CENSUSES, NOT MERGED. (condition
 * 2) `Outbox.enqueue` returning a receipt and `ImportTarget.adopt` returning
 * one are judged apart, because two interface changes read as one is the
 * shape this line has had to unpick before.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';

const SRC = path.join(__dirname, '..', '..', '..', 'src');

function sources(): Array<{ name: string; src: ts.SourceFile }> {
  return fs
    .readdirSync(SRC)
    .filter((name) => name.endsWith('.ts'))
    .sort()
    .map((name) => ({
      name,
      src: ts.createSourceFile(name, fs.readFileSync(path.join(SRC, name), 'utf8'), ts.ScriptTarget.ES2022, true)
    }));
}

function every(node: ts.Node, seen: (inner: ts.Node) => void): void {
  const walk = (inner: ts.Node): void => {
    seen(inner);
    inner.forEachChild(walk);
  };
  walk(node);
}

function line(src: ts.SourceFile, node: ts.Node): number {
  return src.getLineAndCharacterOfPosition(node.getStart(src)).line + 1;
}

/*
 * WHAT A CALL SITE DOES WITH WHAT IT IS HANDED. A call whose value is thrown
 * away is an expression statement of its own, or sits behind `void`; a call
 * whose value is kept is initialising a binding, assigned, returned, or the
 * body of an arrow. Anything else is reported by its kind, so a new shape is
 * read before it is accepted.
 */
function useOf(call: ts.CallExpression): 'dropped' | 'kept' | string {
  const parent = call.parent;
  if (ts.isExpressionStatement(parent) || ts.isVoidExpression(parent)) {
    return 'dropped';
  }
  if (
    (ts.isVariableDeclaration(parent) && parent.initializer === call) ||
    (ts.isBinaryExpression(parent) && parent.operatorToken.kind === ts.SyntaxKind.EqualsToken && parent.right === call) ||
    ts.isReturnStatement(parent) ||
    (ts.isArrowFunction(parent) && parent.body === call)
  ) {
    return 'kept';
  }
  return ts.SyntaxKind[parent.kind];
}

describe('plugin-r3 3 a receipt comes only from the enqueue that wrote the entry', () => {
  /*
   * CONDITION 1: THE BRAND STAYS PRIVATE, AND NOTHING ELSE MAKES A RECEIPT.
   * A forgery would be an object carrying the brand built elsewhere -- which
   * needs the symbol -- or a value asserted to be a `Receipt` with `as`,
   * which needs nothing at all.
   */
  it('keeps the brand in outbox.ts and lets no other file make or assert a receipt', () => {
    const all = sources();
    const outbox = all.find((f) => f.name === 'outbox.ts');
    assert.ok(outbox !== undefined, 'there is no outbox.ts, so this census reads nothing');
    let brand: ts.VariableStatement | undefined;
    every(outbox.src, (n) => {
      if (
        ts.isVariableStatement(n) &&
        n.declarationList.declarations.some((d) => d.name.getText(outbox.src) === 'WRITTEN')
      ) {
        brand = n;
      }
    });
    assert.ok(brand !== undefined, 'outbox.ts no longer declares the brand, so this census reads nothing');
    const exported = (brand.modifiers ?? []).some((m) => m.kind === ts.SyntaxKind.ExportKeyword);
    assert.strictEqual(exported, false, 'the brand is exported, so any file can make a receipt');
    let reExported = false;
    every(outbox.src, (n) => {
      if (ts.isExportSpecifier(n) && n.name.getText(outbox.src) === 'WRITTEN') {
        reExported = true;
      }
    });
    assert.strictEqual(reExported, false, 'the brand is exported through an export list');

    const elsewhere: string[] = [];
    for (const { name, src } of all) {
      if (name === 'outbox.ts') {
        continue;
      }
      every(src, (n) => {
        if (ts.isIdentifier(n) && n.text === 'WRITTEN') {
          elsewhere.push(`${name}:${line(src, n)} names the brand`);
        }
        if ((ts.isAsExpression(n) || ts.isTypeAssertionExpression(n)) && /\bReceipt\b/.test(n.type.getText(src))) {
          elsewhere.push(`${name}:${line(src, n)} asserts a value to be a Receipt`);
        }
      });
    }
    assert.deepStrictEqual(elsewhere, [], 'a receipt can be made outside the enqueue');
  });

  /*
   * CONDITION 2 (design cell 2): ISSUED INSIDE THE LOCK, AFTER THE COMMIT.
   * The object carrying the brand is built in the callback `enqueue` hands
   * `withQueueExclusive`, in the same block as the `commit` call and after
   * it. Built before the commit, a receipt could exist for a write that then
   * failed; built outside the callback, it would answer after the lock was
   * released -- which is how the previous answer failed.
   */
  it('issues the receipt inside withQueueExclusive, after the commit returned', () => {
    const outbox = sources().find((f) => f.name === 'outbox.ts');
    assert.ok(outbox !== undefined);
    const src = outbox.src;
    const branded: ts.ObjectLiteralExpression[] = [];
    every(src, (n) => {
      if (
        ts.isObjectLiteralExpression(n) &&
        n.properties.some((p) => p.name !== undefined && ts.isComputedPropertyName(p.name) && p.name.expression.getText(src) === 'WRITTEN')
      ) {
        branded.push(n);
      }
    });
    assert.strictEqual(branded.length, 1, `the brand is put on ${branded.length} objects in outbox.ts; the design has one`);
    const made = branded[0];
    let callback: ts.Node | undefined;
    for (let at: ts.Node | undefined = made.parent; at !== undefined; at = at.parent) {
      if (ts.isArrowFunction(at) || ts.isFunctionExpression(at)) {
        callback = at;
        break;
      }
    }
    assert.ok(callback !== undefined, 'the receipt is not made inside a callback');
    const call = callback.parent;
    assert.ok(
      ts.isCallExpression(call) && call.expression.getText(src) === 'withQueueExclusive',
      'the receipt is not made inside the callback withQueueExclusive runs'
    );
    let method: ts.Node | undefined;
    for (let at: ts.Node | undefined = call; at !== undefined; at = at.parent) {
      if (ts.isMethodDeclaration(at)) {
        method = at;
        break;
      }
    }
    assert.ok(
      method !== undefined && (method as ts.MethodDeclaration).name.getText(src) === 'enqueue',
      'the receipt is made somewhere other than enqueue'
    );
    const body = (callback as ts.ArrowFunction).body;
    assert.ok(ts.isBlock(body), 'the callback has no block to order statements in');
    const statementOf = (node: ts.Node): ts.Statement | undefined =>
      body.statements.find((s) => s.pos <= node.pos && node.end <= s.end);
    let commit: ts.CallExpression | undefined;
    every(body, (n) => {
      if (ts.isCallExpression(n) && n.expression.getText(src) === 'this.commit') {
        commit = n;
      }
    });
    assert.ok(commit !== undefined, 'the callback no longer commits');
    const commitAt = body.statements.indexOf(statementOf(commit) as ts.Statement);
    const madeAt = body.statements.indexOf(statementOf(made) as ts.Statement);
    assert.ok(commitAt >= 0 && madeAt >= 0, 'the commit or the receipt is not a statement of the callback itself');
    assert.ok(madeAt > commitAt, 'the receipt is made before the commit returned');
  });
});

/*
 * THE CALL SITES, BY WHAT THEY CALL, NOT BY HOW THEY ARE SPELT. (queue item
 * 23, ruled 2026-09-26: the type checker)
 *
 * The first censuses matched the spelling: `x.enqueue(...)` only, and an
 * `adopt` whose receiver is a parameter declared `ImportTarget`. A bracketed
 * call and an aliased receiver each dropped a receipt and passed (V5 and V6,
 * item 3's review r1; measured again on d00141d: both survived). Here a call
 * is counted when the checker resolves it to the method's own declaration,
 * however it is written.
 *
 * KEY: THE CENSUS READS THE CALL OR SAYS IT CANNOT. Every other reference to
 * that declaration -- a `.call`/`.apply` receiver, a destructured binding, the
 * method passed as a value, an alias -- is a call this census cannot follow,
 * and it fails there with the file and the line; it never counts by shape.
 */
interface Checked {
  checker: ts.TypeChecker;
  files: Array<{ name: string; src: ts.SourceFile }>;
}

let checked: Checked | undefined;

function program(): Checked {
  if (checked === undefined) {
    const names = fs
      .readdirSync(SRC)
      .filter((name) => name.endsWith('.ts'))
      .sort();
    const built = ts.createProgram(
      names.map((name) => path.join(SRC, name)),
      {
        target: ts.ScriptTarget.ES2022,
        module: ts.ModuleKind.CommonJS,
        moduleResolution: ts.ModuleResolutionKind.Node10,
        strict: true,
        skipLibCheck: true,
        noEmit: true
      }
    );
    checked = {
      checker: built.getTypeChecker(),
      files: names.map((name) => ({ name, src: built.getSourceFile(path.join(SRC, name)) as ts.SourceFile }))
    };
  }
  return checked;
}

function methodOf(files: Checked['files'], owner: string, method: string): ts.NamedDeclaration {
  for (const { src } of files) {
    for (const statement of src.statements) {
      if ((ts.isClassDeclaration(statement) || ts.isInterfaceDeclaration(statement)) && statement.name?.text === owner) {
        const members = statement.members as ts.NodeArray<ts.ClassElement | ts.TypeElement>;
        const found = members.find((m) => m.name !== undefined && ts.isIdentifier(m.name) && m.name.text === method);
        if (found !== undefined) {
          return found;
        }
      }
    }
  }
  throw new Error(`${owner}.${method} is not declared in src`);
}

/*
 * THE NODE THAT NAMES A PROPERTY IN A CALL'S CALLEE, when this reference is
 * one: the name of `x.m(...)`, or the literal key of `x['m'](...)`.
 */
function inCallPosition(reference: ts.Node): boolean {
  const holder = reference.parent;
  if (ts.isPropertyAccessExpression(holder) && holder.name === reference) {
    return ts.isCallExpression(holder.parent) && holder.parent.expression === holder;
  }
  if (ts.isElementAccessExpression(holder) && holder.argumentExpression === reference) {
    return ts.isCallExpression(holder.parent) && holder.parent.expression === holder;
  }
  return false;
}

function censusOf(owner: string, method: string): { sites: string[]; dropped: string[]; unreadable: string[] } {
  const { checker, files } = program();
  const target = methodOf(files, owner, method);
  const sites: string[] = [];
  const dropped: string[] = [];
  const unreadable: string[] = [];
  const isTarget = (symbol: ts.Symbol | undefined): boolean =>
    symbol !== undefined && (symbol.declarations ?? []).includes(target);
  for (const { name, src } of files) {
    every(src, (n) => {
      if (ts.isCallExpression(n) && checker.getResolvedSignature(n)?.declaration === target) {
        const use = useOf(n);
        sites.push(`${name}:${line(src, n)} ${use}`);
        if (use !== 'kept') {
          dropped.push(`${name}:${line(src, n)} ${use}`);
        }
        return;
      }
      if (n === target.name) {
        return;
      }
      let symbol: ts.Symbol | undefined;
      if (ts.isBindingElement(n) && ts.isObjectBindingPattern(n.parent)) {
        const key = n.propertyName ?? n.name;
        if (ts.isIdentifier(key)) {
          symbol = checker.getTypeAtLocation(n.parent).getProperty(key.text);
        }
      } else if (ts.isIdentifier(n) || ts.isStringLiteral(n)) {
        if (!(ts.isBindingElement(n.parent) && n.parent.name === n)) {
          symbol = checker.getSymbolAtLocation(n);
        }
      }
      if (isTarget(symbol) && !inCallPosition(n)) {
        unreadable.push(`${name}:${line(src, n)} this build cannot read where this call goes`);
      }
    });
  }
  return { sites, dropped, unreadable };
}

describe('plugin-r3 3 every call site holds the receipt it is handed', function () {
  /*
   * NOTE: BUILDING THE PROGRAM TAKES SECONDS, once for both cells.
   */
  this.timeout(60000);

  /*
   * THE `Outbox.enqueue` CALL-SITE CENSUS. An ignored return is legal
   * TypeScript, so the compiler will never point at a site that drops the
   * receipt; this names every site and what it does with it.
   */
  it('keeps the receipt at every enqueue in src', () => {
    const { sites, dropped, unreadable } = censusOf('Outbox', 'enqueue');
    assert.deepStrictEqual(unreadable, [], 'these references to Outbox.enqueue are calls this census cannot follow');
    assert.ok(sites.length >= 3, `the census found ${sites.length} enqueue sites, too few to be reading src: ${sites.join('; ')}`);
    assert.deepStrictEqual(dropped, [], 'these enqueue calls do nothing with the receipt');
  });

  /*
   * THE `ImportTarget.adopt` CALL-SITE CENSUS, apart from the one above. The
   * name `adopt` also belongs to `Saver.adopt(work)` and
   * `Sessions.adopt(sessionId)`; the checker tells them apart by the
   * declaration a call resolves to, not by the receiver's spelling.
   */
  it('keeps the receipt at every ImportTarget.adopt in src', () => {
    const { sites, dropped, unreadable } = censusOf('ImportTarget', 'adopt');
    assert.deepStrictEqual(unreadable, [], 'these references to ImportTarget.adopt are calls this census cannot follow');
    assert.ok(sites.length >= 1, 'the census found no ImportTarget.adopt call, so it is reading nothing');
    assert.deepStrictEqual(dropped, [], 'these ImportTarget.adopt calls do nothing with the receipt');
  });
});
