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
 * WHO MAY WRITE THE RECORDS BESIDE A BLOCK'S VERSIONS. (O1, O2, O3, O4)
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as ts from 'typescript';
import { nodeFileOps } from '../../src/fsops';
import { OwnerRecord, Owners, Stamp, stamped, stillOwed, verdictOn } from '../../src/ownership';
import { runHost } from '../support/host';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-owner-'));
}

function owners(): Owners {
  return new Owners(nodeFileOps);
}

describe('O1 ownership of a block directory is taken once, never negotiated', () => {
  it('takes an unowned directory as generation 0', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const held = owners().take(directory, 'S-a', []);
    assert.ok(held.held, 'an unowned directory was not taken');
    assert.strictEqual(held.record.generation, 0);
    assert.strictEqual(held.record.sessionId, 'S-a');
    assert.deepStrictEqual(held.record.expected, {}, 'a first owner inherits a debt from nobody');
    assert.ok(fs.existsSync(path.join(directory, 'owner.0')), 'no owner record was published');
  });

  /*
   * ⚠️ THE HANDOVER IS THE SAME ACT AS THE TAKING. A rewrite of the
   * existing record would be a read-then-write, and two sessions
   * reaching it together would both succeed and both believe they hold
   * the block. Create-once has no such window: the second `link` gets
   * EEXIST.
   */
  it('hands over by publishing the next generation, not by rewriting the record', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const first = owners().take(directory, 'S-a', ['1.md.meta']);
    assert.ok(first.held);
    const second = owners().take(directory, 'S-b', ['1.md.meta']);
    assert.ok(second.held, 'the block could not be taken over at all');
    assert.strictEqual(second.record.generation, 1);
    assert.ok(fs.existsSync(path.join(directory, 'owner.0')), 'the previous generation was destroyed');
    assert.ok(fs.existsSync(path.join(directory, 'owner.1')), 'the new generation was not published');
    const now = owners().ownerOf(directory);
    assert.ok(now.known && now.record !== null);
    assert.strictEqual(now.record.sessionId, 'S-b', 'the reader does not see the newest generation');
  });

  it('refuses to take a directory whose owner record it cannot read', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    fs.writeFileSync(path.join(directory, 'owner.0'), 'not json at all\n', 'utf8');
    const asked = owners().ownerOf(directory);
    assert.deepStrictEqual(
      asked,
      { known: false },
      'an unreadable owner record was reported as an answer about who owns the block'
    );
    const held = owners().take(directory, 'S-b', []);
    assert.deepStrictEqual(
      held,
      { held: false, because: 'unreadable' },
      'a directory whose owner could not be read was taken anyway'
    );
  });

  /*
   * ⚠️ A RECORD WHOSE NAME AND CONTENTS DISAGREE IS SOMEBODY ELSE'S
   * FILE. Both are written by one act, so the disagreement cannot arise
   * from this program; reading past it to an older generation would let
   * anyone who can drop `owner.999` into the directory decide who owns
   * the block.
   */
  it('does not read past a record that does not say its own generation', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const held = owners().take(directory, 'S-a', []);
    assert.ok(held.held);
    fs.writeFileSync(
      path.join(directory, 'owner.7'),
      `${JSON.stringify({ sessionId: 'S-x', generation: 2, expected: {} })}\n`,
      'utf8'
    );
    assert.deepStrictEqual(owners().ownerOf(directory), { known: false });
  });

  /*
   * ⚠️ A DIRECTORY THAT CANNOT BE WRITTEN IS NOT A RACE THAT WAS LOST.
   *
   * Both come back from the same `link` call, and collapsing them
   * tells the user somebody else owns their block when in fact nothing
   * could be written at all -- the reassuring answer, and the wrong
   * one. The two demand opposite next moves: read who owns it now, or
   * stop and fix something.
   */
  it('tells a lost race apart from a directory it could not write', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const first = owners().take(directory, 'S-a', []);
    assert.ok(first.held);

    /*
     * THE LOSER'S ANSWER, built by putting the name the next taker will
     * reach for where it will reach: `owner.1` already exists, so its
     * `link` gets EEXIST -- which is precisely what a second process
     * winning looks like from here.
     */
    fs.writeFileSync(
      path.join(directory, 'owner.1'),
      `${JSON.stringify({ sessionId: 'S-b', generation: 1, expected: {} })}\n`,
      'utf8'
    );
    const lost = new Owners({
      ...nodeFileOps,
      link: () => {
        const e: NodeJS.ErrnoException = new Error('EEXIST: file already exists');
        e.code = 'EEXIST';
        throw e;
      }
    }).take(directory, 'S-c', []);
    assert.deepStrictEqual(lost, { held: false, because: 'lost-the-race' });

    const stuck = new Owners({
      ...nodeFileOps,
      link: () => {
        const e: NodeJS.ErrnoException = new Error('EPERM: operation not permitted');
        e.code = 'EPERM';
        throw e;
      }
    }).take(directory, 'S-c', []);
    assert.strictEqual(stuck.held, false);
    assert.strictEqual(
      (stuck as { because: string }).because,
      'could-not-write',
      'a directory that could not be written was reported as a race somebody else won'
    );
  });

  /*
   * ⚠️ THE OWNER RECORD IS NEVER HALF A RECORD.
   *
   * `rewrite` only ever shrinks the debt, so losing an update costs one
   * sidecar being stamped twice. A torn record costs something else
   * entirely: it does not parse, `ownerOf` answers "I do not know", and
   * every write to the block stops. So it goes through a temporary and
   * a rename rather than over the name somebody may be reading.
   */
  it('replaces the owner record through a temporary rather than over it', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const held = owners().take(directory, 'S-a', ['1.md.meta']);
    assert.ok(held.held);

    const wrote: string[] = [];
    const renamed: Array<[string, string]> = [];
    const watching = new Owners({
      ...nodeFileOps,
      writeDurably: (file, text) => {
        wrote.push(file);
        nodeFileOps.writeDurably(file, text);
      },
      rename: (from, to) => {
        renamed.push([from, to]);
        nodeFileOps.rename(from, to);
      }
    });
    watching.rewrite(directory, stamped(held.record, '1.md.meta', { sessionId: 'S-x', generation: 9 }));

    const name = path.join(directory, 'owner.0');
    assert.deepStrictEqual(
      wrote.filter((f) => f === name),
      [],
      'the owner record was written over in place, so a stop in the middle leaves half a record'
    );
    assert.strictEqual(renamed.length, 1, 'the replacement did not go through a rename');
    assert.strictEqual(renamed[0][1], name);
    const after = owners().ownerOf(directory);
    assert.ok(after.known && after.record !== null);
    assert.strictEqual(after.record.sessionId, 'S-a', 'the rewrite moved ownership');
  });

  it('answers three ways about who may write', () => {
    const root = scratch();
    const mine = path.join(root, 'mine');
    const theirs = path.join(root, 'theirs');
    const nobodys = path.join(root, 'nobodys');
    for (const d of [mine, theirs, nobodys]) {
      fs.mkdirSync(d, { recursive: true });
    }
    owners().take(mine, 'S-a', []);
    owners().take(theirs, 'S-b', []);
    assert.strictEqual(owners().mayWrite(mine, 'S-a').may, true);
    assert.deepStrictEqual(owners().mayWrite(theirs, 'S-a'), {
      may: false,
      because: 'another-session'
    });
    assert.deepStrictEqual(owners().mayWrite(nobodys, 'S-a'), { may: false, because: 'unowned' });
  });
});

describe('O1 two processes reaching for one block directory', function () {
  this.timeout(120000);

  /*
   * ⚠️ IT CANNOT BE READ INSIDE ONE PROCESS. The scan for the highest
   * generation and the link that claims the next one are synchronous,
   * so two calls in one host never overlap -- and a cell that ran them
   * there would pass for an implementation that read, thought, and then
   * wrote.
   */
  it('lets exactly one of two processes take the same generation', async () => {
    const storage = scratch();
    const [a, b] = await Promise.all([
      runHost({
        storage,
        steps: [{ takeOwner: { directory: 'a.2', as: 'S-a', waitFor: 'gate' } }]
      }),
      runHost({
        storage,
        steps: [{ takeOwner: { directory: 'a.2', as: 'S-b', waitFor: 'gate' } }]
      })
    ]);
    const outcomeOf = (r: typeof a): { held?: boolean } =>
      (r.steps.find((s) => s.step === 'takeOwner')?.outcome ?? {}) as { held?: boolean };
    const won = [outcomeOf(a), outcomeOf(b)].filter((o) => o.held === true).length;
    assert.strictEqual(
      won,
      1,
      `${won} of two processes took the same generation: ${JSON.stringify([a.steps, b.steps])}`
    );
    const names = fs.readdirSync(path.join(storage, 'a.2')).filter((n) => n.startsWith('owner.'));
    assert.deepStrictEqual(names.sort(), ['owner.0'], 'more than one generation was published');
  });
});

describe('O3 a takeover owes the sidecars it has not stamped yet', () => {
  const previous: Stamp = { sessionId: 'S-a', generation: 0 };

  function afterTakeover(sidecars: string[]): OwnerRecord {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const first = owners().take(directory, 'S-a', sidecars);
    assert.ok(first.held);
    const second = owners().take(directory, 'S-b', sidecars);
    assert.ok(second.held);
    return second.record;
  }

  it('writes the predecessor’s stamp against every sidecar it found', () => {
    const record = afterTakeover(['1.md.meta', '2.md.meta']);
    assert.deepStrictEqual(record.expected, {
      '1.md.meta': [previous],
      '2.md.meta': [previous]
    });
  });

  /*
   * ⚠️ THE DEBT IS PER SIDECAR BECAUSE TWO VERSIONS SHARE ONE
   * PREDECESSOR. A debt cleared the moment the first version was
   * stamped leaves the second carrying a stamp the record no longer
   * expects -- and the reader calls that contamination and sends the
   * user to reconcile a block nothing is wrong with.
   */
  it('keeps owing a shared stamp until every sidecar that carries it is stamped', () => {
    const record = afterTakeover(['1.md.meta', '2.md.meta']);
    const half = stamped(record, '1.md.meta', previous);
    assert.strictEqual(verdictOn(previous, half, '1.md.meta'), 'suspect');
    assert.strictEqual(
      verdictOn(previous, half, '2.md.meta'),
      'expected',
      'stamping one version cleared the debt the other still carries'
    );
    assert.strictEqual(stillOwed(half), true);
    const done = stamped(half, '2.md.meta', previous);
    assert.strictEqual(stillOwed(done), false);
  });

  it('carries an unfinished predecessor’s debt into the next generation', () => {
    const root = scratch();
    const directory = path.join(root, 'a.2');
    fs.mkdirSync(directory, { recursive: true });
    const a = owners().take(directory, 'S-a', ['1.md.meta']);
    assert.ok(a.held);
    const b = owners().take(directory, 'S-b', ['1.md.meta']);
    assert.ok(b.held);
    const c = owners().take(directory, 'S-c', ['1.md.meta']);
    assert.ok(c.held);
    assert.deepStrictEqual(
      c.record.expected['1.md.meta'],
      [
        { sessionId: 'S-a', generation: 0 },
        { sessionId: 'S-b', generation: 1 }
      ],
      'a session that died inside its own initialisation left a debt the next one did not inherit'
    );
  });
});

describe('O4 a stamp that belongs to nobody the owner knows', () => {
  const record: OwnerRecord = {
    sessionId: 'S-b',
    generation: 1,
    expected: { '1.md.meta': [{ sessionId: 'S-a', generation: 0 }] }
  };

  it('calls its own stamp mine', () => {
    assert.strictEqual(verdictOn({ sessionId: 'S-b', generation: 1 }, record, '1.md.meta'), 'mine');
  });

  it('calls the predecessor it owes expected', () => {
    assert.strictEqual(
      verdictOn({ sessionId: 'S-a', generation: 0 }, record, '1.md.meta'),
      'expected'
    );
  });

  it('calls a record with no stamp at all expected', () => {
    assert.strictEqual(verdictOn(null, record, '1.md.meta'), 'expected');
  });

  it('calls anyone else suspect', () => {
    assert.strictEqual(
      verdictOn({ sessionId: 'S-x', generation: 1 }, record, '1.md.meta'),
      'suspect'
    );
  });

  /*
   * ⚠️ THE COMPARISON IS THE WHOLE IDENTITY. Create-once makes the
   * generations unique, so a reader could compare only the number and
   * be right -- today. That is an argument about how numbers are handed
   * out, and a reader resting on it is wrong the first time it stops
   * holding. The two sides of this pair differ in the session and agree
   * in the number.
   */
  it('does not mistake another session at the same generation for itself', () => {
    assert.strictEqual(
      verdictOn({ sessionId: 'S-other', generation: 1 }, record, '1.md.meta'),
      'suspect'
    );
  });
});

/*
 * ⚠️ "THE SESSION LAYER KEEPS THE GATE" IS A SENTENCE, AND A SENTENCE
 * IS NOT A GUARD.
 *
 * `Owners.take` does not ask whether the session it is taking from is
 * alive. That is deliberate -- liveness belongs to `Sessions.claim`,
 * which already refuses a holder it cannot judge -- and it is only
 * true while every caller of `take` really does go through `claim`
 * first. One call from somewhere else unmakes it silently, which is
 * exactly the shape of the `new Saver(` guard in awaiting.test.ts: the
 * safety rested on a claim about call sites, so the call sites are
 * counted.
 */
function leavesIn(statement: ts.Statement): boolean {
  if (
    ts.isReturnStatement(statement) ||
    ts.isThrowStatement(statement) ||
    ts.isContinueStatement(statement) ||
    ts.isBreakStatement(statement)
  ) {
    return true;
  }
  if (ts.isBlock(statement)) {
    return statement.statements.some(leavesIn);
  }
  return false;
}

describe('O1 nothing takes ownership without the session layer', () => {
  it('has at least one caller, each of them behind a claim that leaves', () => {
    const src = path.join(__dirname, '..', '..', '..', 'src');
    const offenders: string[] = [];
    let sites = 0;
    for (const name of fs.readdirSync(src).filter((f) => f.endsWith('.ts'))) {
      const file = ts.createSourceFile(
        name,
        fs.readFileSync(path.join(src, name), 'utf8'),
        ts.ScriptTarget.ES2022,
        true
      );
      const found: ts.CallExpression[] = [];
      const walk = (n: ts.Node): void => {
        if (
          ts.isCallExpression(n) &&
          ts.isPropertyAccessExpression(n.expression) &&
          n.expression.name.getText(file) === 'take' &&
          /owner/i.test(n.expression.expression.getText(file))
        ) {
          found.push(n);
        }
        ts.forEachChild(n, walk);
      };
      walk(file);
      for (const call of found) {
        sites += 1;
        let at: ts.Node | undefined = call.parent;
        let enclosing: ts.Node | null = null;
        while (at !== undefined) {
          if (
            ts.isMethodDeclaration(at) ||
            ts.isFunctionDeclaration(at) ||
            ts.isFunctionExpression(at) ||
            ts.isArrowFunction(at)
          ) {
            enclosing = at;
            break;
          }
          at = at.parent;
        }
        /*
         * ⚠️ TAKING IS LEGITIMATE IN TWO CIRCUMSTANCES, AND THE CENSUS
         * KNOWS BOTH.
         *
         * The rule it exists for is "nothing takes a block away from a
         * session that might be alive". A takeover does that, and it
         * goes through `Sessions.claim`, which refuses a holder it
         * cannot judge. Taking a directory NOBODY holds takes it from
         * nobody, so no liveness question arises -- and that is the
         * publication path, where ownership of a block begins.
         *
         * ⛔ Neither of these is an exemption: they are the two shapes
         * in which the rule is satisfied. A third shape appearing means
         * this census has to be told about it.
         */
        let legitimate = false;
        const look = (n: ts.Node): void => {
          if (
            ts.isCallExpression(n) &&
            ts.isPropertyAccessExpression(n.expression) &&
            n.expression.name.getText(file) === 'claim' &&
            n.getStart(file) < call.getStart(file)
          ) {
            legitimate = true;
          }
          if (
            ts.isIfStatement(n) &&
            n.getStart(file) < call.getStart(file) &&
            /'unowned'/.test(n.expression.getText(file)) &&
            leavesIn(n.thenStatement)
          ) {
            legitimate = true;
          }
          ts.forEachChild(n, look);
        };
        if (enclosing !== null) {
          look(enclosing);
        }
        if (!legitimate) {
          offenders.push(`${name}:${file.getLineAndCharacterOfPosition(call.getStart(file)).line + 1}`);
        }
      }
    }
    assert.ok(
      sites > 0,
      'nothing in src takes ownership of a block directory, so this census is guarding a rule ' +
        'nobody applies yet; it turns green when the takeover command is wired'
    );
    assert.deepStrictEqual(
      offenders,
      [],
      'these take ownership without a claim before them, so the liveness check the session layer ' +
        'is supposed to make never happens'
    );
  });
});

/*
 * O2: EVERY PATH THAT WRITES A SIDECAR PASSES THE SAME RULE.
 *
 * ⚠️ AND A CENSUS CANNOT PROVE THE FENCE WORKS. It can only say that
 * each write site has a check before it; whether the check is still
 * true when the write lands is the lease problem, which §13 names and
 * does not solve. The cells that transfer ownership under a paused
 * writer are the other half of this and belong with the settling work.
 */
describe('O2 every sidecar write passes the ownership rule', () => {
  function publication(): ts.SourceFile {
    const file = path.join(__dirname, '..', '..', '..', 'src', 'publication.ts');
    return ts.createSourceFile(
      'publication.ts',
      fs.readFileSync(file, 'utf8'),
      ts.ScriptTarget.ES2022,
      true
    );
  }

  function all<T extends ts.Node>(root: ts.Node, is: (n: ts.Node) => n is T): T[] {
    const found: T[] = [];
    const walk = (n: ts.Node): void => {
      if (is(n)) {
        found.push(n);
      }
      ts.forEachChild(n, walk);
    };
    walk(root);
    return found;
  }

  /*
   * ⚠️ `continue` LEAVES TOO, inside a loop -- and a loop is where one
   * of these writes lives: the migration walks a block's versions and
   * skips the ones this session may not write. A criterion that knew
   * only `return` and `throw` would have called that site unguarded
   * while it was guarded, which is the direction that makes a census
   * lie about correct code. (TypeScript refuses `continue` outside a
   * loop, so accepting it cannot widen this anywhere else.)
   */
  function leaves(statement: ts.Statement): boolean {
    if (
      ts.isReturnStatement(statement) ||
      ts.isThrowStatement(statement) ||
      ts.isContinueStatement(statement) ||
      ts.isBreakStatement(statement)
    ) {
      return true;
    }
    if (ts.isBlock(statement)) {
      return statement.statements.some(leaves);
    }
    return false;
  }

  function enclosing(node: ts.Node): ts.Node | null {
    let at: ts.Node | undefined = node.parent;
    while (at !== undefined) {
      if (
        ts.isMethodDeclaration(at) ||
        ts.isFunctionDeclaration(at) ||
        ts.isFunctionExpression(at) ||
        ts.isArrowFunction(at)
      ) {
        return at;
      }
      at = at.parent;
    }
    return null;
  }

  it('has a guard that leaves before every write of a record', () => {
    const src = publication();
    const writes = all(src, ts.isCallExpression).filter(
      (n) =>
        ts.isPropertyAccessExpression(n.expression) &&
        n.expression.name.getText(src) === 'write' &&
        n.expression.expression.kind === ts.SyntaxKind.ThisKeyword
    );
    assert.ok(
      writes.length >= 7,
      `this census found ${writes.length} sidecar writes in publication.ts, which is too few to ` +
        'be reading the file'
    );
    const unguarded = writes
      .filter((write) => {
        const fn = enclosing(write);
        if (fn === null) {
          return true;
        }
        /*
         * ⚠️ THE RULE HAS TWO ENTRY POINTS AND THEY ARE BOTH NAMED
         * HERE. `mayWrite` asks whether this session holds the block;
         * `takeIfUnowned` is the publication's version, because a
         * publication is where ownership of a block BEGINS -- there is
         * nothing to own until the directory exists.
         *
         * ⛔ THIS IS NOT AN EXEMPTION LIST. It is the list of ways to
         * ASK, and a third way appearing means this census has to be
         * told about it -- which is the right amount of friction for
         * adding one.
         */
        const asks = all(fn, ts.isCallExpression).filter(
          (call) =>
            ts.isPropertyAccessExpression(call.expression) &&
            ['mayWrite', 'takeIfUnowned'].includes(call.expression.name.getText(src)) &&
            call.getStart(src) < write.getStart(src)
        );
        if (asks.length === 0) {
          return true;
        }
        const earliest = Math.min(...asks.map((call) => call.getStart(src)));
        return !all(fn, ts.isIfStatement).some(
          (guard) =>
            guard.getStart(src) > earliest &&
            guard.getStart(src) < write.getStart(src) &&
            leaves(guard.thenStatement)
        );
      })
      .map((write) => `publication.ts:${src.getLineAndCharacterOfPosition(write.getStart(src)).line + 1}`);
    assert.deepStrictEqual(
      unguarded,
      [],
      'these paths write a record beside a block without asking whether this session still owns ' +
        'the block directory'
    );
  });
});
