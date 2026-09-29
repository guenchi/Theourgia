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
import * as os from 'os';
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
 * KEY: A KEY IS AN ANCHOR, NOT A LINE: `<file>#<function>: <the first line
 * of the try block>` (queue item 35). It used to be `<file>:<line>`, and a
 * line moves whenever anything above it grows -- items 18 and 19 each froze
 * a tree whose census was red for that reason alone, with nothing about
 * the catch changed. An anchor moves when the catch's own function is
 * renamed or the statement it guards is rewritten: exactly when the reason
 * below has to be read again. A failure still says the current line, as a
 * hint for finding it.
 *
 * Every one of these was looked at when this census was written; the
 * reason says why answering with that value is not an absence dressed up
 * as a fact. An exemption with no reason is not an exemption, and neither
 * is one nobody re-reads: a stale entry fails the census below rather than
 * silently covering something else.
 */
const ANSWERING_IS_RIGHT: Record<string, string> = {
  'documents.ts#hasUncommittedWork: marker = files.readText(markerFor(file)).trim();':
    'answers TRUE -- "this file may hold work". A document with no marker was written by ' +
    'something this version does not know about, and the safe answer is the one that stops it ' +
    'being overwritten.',
  'extension.ts#getChildren: if (node === undefined) { const listing = await this.model.roots(); nodes = listing.nodes; marksKnown = listing.marksKnown; notes = listing.notes; } else { const listing = await this.model.childrenOf(node.id); nodes = listing.nodes; marksKnown = listing.marksKnown; notes = listing.notes; }':
    'answers an empty list to the TREE, and reports through `failed` -- except when the store ' +
    'changed under it, which is the one case that skips the report. The reporting call is ' +
    'conditional, so the rule no longer exempts it; the condition is `asked === generation()`, ' +
    'and when that is false nothing this listing drew is on the screen to be wrong about. ' +
    '(Re-keyed: the listing now also hands on the writers it could not read.)',
  'split-symbols.ts#savedText: text = new TextDecoder(\'utf-8\', { fatal: true, ignoreBOM: true }).decode(bytes);':
    'answers NULL for a file whose bytes are not UTF-8: no position the editor gives can be converted ' +
    'against them. The one caller, `runSplit`, stops on null with the sentence that says so and sends ' +
    'nothing -- the null is read as "cannot count", never as an empty file.',
  'fsops.ts#syncDirectory: handle = fs.openSync(directory, \'r\');':
    'answers NULL for a directory that will not open for flushing. Some filesystems refuse to ' +
    'open a directory for reading, and a warning on every write there would teach people to ' +
    'ignore warnings; said at the site and in the README. A flush that is attempted and fails ' +
    'is not this catch: it returns its reason (queue item 3, review r1).',
  'fsops.ts#has: return fs.existsSync(path.join(corePath, name));':
    'answers FALSE for "the core directory holds this file". A directory this process may not ' +
    'search then reads as neither form, which `problemsWith` refuses by name -- so the failure ' +
    'reaches the user as a refusal about the setting rather than as a wrong extension list.',
  'integrity.ts#check: returned = on.show(notice);':
    'answers nothing to anybody: `check` returns no value and nobody waits for it. The catch is ' +
    'a `show` that threw (queue item 18, ruled): it writes the failure to the output channel ' +
    'through `write`, which calls `record` inside its own guard -- names this census does not ' +
    'count as speaking, because `record` means a save record elsewhere in src -- and returns so ' +
    'that the store stays unmarked and the next check tells the user. Reporting it with another ' +
    'message would use the path that just failed. (Re-keyed by item 33: the statement now keeps ' +
    'what `show` returned, to follow it.)',
  'model.ts#verbs: const answer = await this.client.request(\'describe\', []);':
    'answers NULL, which `verbs` documents as "I could not find out" and which its caller must ' +
    'tell apart from a core with no verbs. The absence has a name here.',
  'ownership.ts#parse: raw = JSON.parse(text);':
    'answers NULL for an owner record that will not parse, and the caller turns that into ' +
    '`{known: false}` one level up -- "I do not know who owns this", never "nobody does".',
  'sessions.ts#systemStartTime: const out = execFileSync(\'ps\', [\'-o\', \'lstart=\', \'-p\', String(pid)], { encoding: \'utf8\', timeout: 4000, env: { ...process.env, LC_ALL: \'C\' } }).trim();':
    'answers NULL for a start time that cannot be read, and the identity that uses it is then ' +
    'undecidable rather than matching. Named absence.',
  'sessions.ts#surveyQueue: source.load();':
    'answers nothing, and writes `unreadableQueue` into the takeover ledger first. The ledger ' +
    'is the survey\'s whole output, so the failure is not lost -- it is the finding. A throw ' +
    'here would end a survey of every OTHER window over one file that will not read.',
  'sessions.ts#importQueue: source.load();':
    'as `surveyQueue` above, on the import rather than the survey: `unreadableQueue` is counted ' +
    'and the file is left untouched, because the queue holds the only copy of that window\'s ' +
    'unsent work and a takeover that repaired it would write over what it came to rescue.',
  'sessions.ts#pendingForDirectory: const raw=readQueueFile(JSON.parse(this.files.readText(file)));':
    'answers TRUE -- "a relevant send may be in flight". A migration is refused rather than ' +
    'permitted over work nobody could read.',
  'saving.ts#recognise: if (!this.files.exists(meta) || !this.files.exists(file)) { return null; }':
    'answers NULL from `cleanliness`, whose contract -- written at the function -- is that null ' +
    'means "cannot say" and never "the file is clean". Its caller knows how to hold that; the ' +
    'one thing it must not do is fail the save it was asked about.',
  'saving.ts#recognise: read = sidecarFromDisk(this.files.readText(meta));': 'as the first `recognise` entry above -- the sidecar would not read, and null is "cannot say".',
  'saving.ts#recognise: bytes = this.files.readBytes(file);': 'as the first `recognise` entry above -- the file would not read, and null is "cannot say".',
  'saving.ts#recognise: text = new TextDecoder(\'utf-8\', { fatal: true }).decode(bytes);': 'as the first `recognise` entry above -- the bytes are not UTF-8, and null is "cannot say".'
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

/*
 * THE ANCHOR OF A CATCH: `<file>#<function>: <statement>`. (queue item 35)
 *
 * `<function>` is the nearest enclosing function that has a name -- a
 * declaration, a method, an accessor, or a function or arrow assigned to a
 * variable or a property; `constructor` for a constructor; `(top level)`
 * outside any. `<statement>` is the WHOLE first statement of the `try`
 * block the catch belongs to, or `(empty try)`.
 *
 * NOTE: THE NORMALISATION IS WRITTEN DOWN AND IS ALL THERE IS. The
 * statement is read as the parser's tokens: every token is kept exactly as
 * written -- names, quotes, semicolons, and the whole of a string or
 * template, spaces inside it included -- and wherever the source has
 * anything between two tokens (spaces, tabs, line breaks of any kind,
 * comments), one space stands for it. So a line wrap, a changed indent or
 * an added comment keeps the anchor; `raw=` and `raw = ` are two anchors,
 * and rewriting the guarded statement moves it -- which is the point.
 * (r2 of item 35: r1 took the first line only, which a wrap cut short
 * enough for two statements to collide, and which kept a trailing
 * comment.)
 */
export function statementText(statement: ts.Node, source: ts.SourceFile): string {
  const parts: string[] = [];
  const leaves = (at: ts.Node): void => {
    const children = at
      .getChildren(source)
      .filter((child) => child.kind < ts.SyntaxKind.FirstJSDocNode || child.kind > ts.SyntaxKind.LastJSDocNode);
    if (children.length === 0) {
      if (parts.length > 0 && at.getStart(source) > at.getFullStart()) {
        parts.push(' ');
      }
      parts.push(at.getText(source));
      return;
    }
    children.forEach(leaves);
  };
  leaves(statement);
  return parts.join('');
}
export function anchorOf(node: ts.CatchClause, source: ts.SourceFile, file: string): string {
  let name = '(top level)';
  for (let at: ts.Node | undefined = node.parent; at !== undefined; at = at.parent) {
    if ((ts.isFunctionDeclaration(at) || ts.isMethodDeclaration(at) || ts.isGetAccessor(at) || ts.isSetAccessor(at)) && at.name) {
      name = at.name.getText(source);
      break;
    }
    if (ts.isConstructorDeclaration(at)) {
      name = 'constructor';
      break;
    }
    if (
      (ts.isArrowFunction(at) || ts.isFunctionExpression(at)) &&
      (ts.isVariableDeclaration(at.parent) || ts.isPropertyAssignment(at.parent))
    ) {
      name = at.parent.name.getText(source);
      break;
    }
  }
  const first = node.parent.tryBlock.statements[0];
  const statement = first === undefined ? '(empty try)' : statementText(first, source);
  return `${file}#${name}: ${statement}`;
}

/*
 * EVERY CATCH IN src/, by its anchor and its line: what the uniqueness cell
 * reads. Not only the silent ones -- two catches with one anchor make an
 * exemption ambiguous whichever of them is silent today.
 */
export function catchAnchors(root: string): Array<{ anchor: string; where: string }> {
  const out: Array<{ anchor: string; where: string }> = [];
  for (const name of fs.readdirSync(path.join(root, 'src')).filter((f) => f.endsWith('.ts')).sort()) {
    const text = fs.readFileSync(path.join(root, 'src', name), 'utf8');
    const parsed = ts.createSourceFile(name, text, ts.ScriptTarget.Latest, true);
    const visit = (node: ts.Node): void => {
      if (ts.isCatchClause(node)) {
        out.push({
          anchor: anchorOf(node, parsed, name),
          where: `${name}:${parsed.getLineAndCharacterOfPosition(node.getStart(parsed)).line + 1}`
        });
      }
      node.forEachChild(visit);
    };
    visit(parsed);
  }
  return out;
}

export interface SilentCatch {
  anchor: string;
  /*
   * NOTE: THE LINE IS A HINT FOR A PERSON, NOT A KEY: nothing is compared
   * with it.
   */
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
            anchor: anchorOf(node, parsed, name),
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
      .filter((c) => ANSWERING_IS_RIGHT[c.anchor] === undefined)
      .map((c) => `${c.anchor} (now at ${c.where}) answers ${c.answers}`);
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
   * THAT READS LIKE DILIGENCE. A catch whose function was renamed or whose
   * guarded statement was rewritten has a new anchor, so a stale entry has
   * to fail rather than sit.
   */
  it('has no exemption for a catch that is not there any more', function () {
    const present = new Set(silentCatches(root).map((c) => c.anchor));
    assert.deepStrictEqual(
      Object.keys(ANSWERING_IS_RIGHT).filter((anchor) => !present.has(anchor)),
      [],
      'these exemptions name catches that were rewritten, renamed or removed; re-read the code ' +
        'and re-key them'
    );
  });

  /*
   * NOTE: ONE ANCHOR, ONE CATCH. (queue item 35) Two catches with the same
   * anchor would let one exemption cover both, so the census refuses the
   * tree rather than choosing; the remedy is in the code, not here.
   */
  it('gives every catch in src/ an anchor of its own', function () {
    const shared = (from: string): string[] => {
      const seen = new Map<string, string[]>();
      catchAnchors(from).forEach((c) => seen.set(c.anchor, [...(seen.get(c.anchor) ?? []), c.where]));
      return [...seen.entries()].filter(([, at]) => at.length > 1).map(([anchor, at]) => `${anchor} at ${at.join(', ')}`);
    };
    assert.ok(catchAnchors(root).length >= 20, `only ${catchAnchors(root).length} catches were read in src/`);
    assert.deepStrictEqual(shared(root), [], 'these anchors name more than one catch');
    /*
     * AND IT SAYS SO WHEN THEY DO: a source with two catches in one function
     * whose `try` blocks begin with the same statement.
     */
    const at = fs.mkdtempSync(path.join(os.tmpdir(), 'absence-twins-'));
    try {
      fs.mkdirSync(path.join(at, 'src'));
      fs.writeFileSync(
        path.join(at, 'src', 'twins.ts'),
        'export function twins(): null {\n  try {\n    load();\n  } catch {\n    return null;\n  }\n' +
          '  try {\n    load();\n  } catch {\n    return null;\n  }\n  return null;\n}\n'
      );
      assert.deepStrictEqual(shared(at), ['twins.ts#twins: load(); at twins.ts:4, twins.ts:9']);
    } finally {
      fs.rmSync(at, { recursive: true, force: true });
    }
  });

  /*
   * A COPY OF src/ WITH saving.ts CHANGED, and what the census says of it:
   * the silent catches no exemption explains, and the exemptions no catch
   * answers to. For the two cells below.
   */
  const STATEMENT = 'bytes = this.files.readBytes(file);';
  const judgeCopy = (change: (text: string) => string): { unexplained: string[]; stale: string[] } => {
    const at = fs.mkdtempSync(path.join(os.tmpdir(), 'absence-anchor-'));
    try {
      fs.mkdirSync(path.join(at, 'src'));
      for (const name of fs.readdirSync(path.join(root, 'src')).filter((f) => f.endsWith('.ts'))) {
        const text = fs.readFileSync(path.join(root, 'src', name), 'utf8');
        fs.writeFileSync(path.join(at, 'src', name), name === 'saving.ts' ? change(text) : text);
      }
      const silent = silentCatches(at);
      const present = new Set(silent.map((c) => c.anchor));
      return {
        unexplained: silent.filter((c) => ANSWERING_IS_RIGHT[c.anchor] === undefined).map((c) => c.anchor),
        stale: Object.keys(ANSWERING_IS_RIGHT).filter((anchor) => !present.has(anchor))
      };
    } finally {
      fs.rmSync(at, { recursive: true, force: true });
    }
  };

  /*
   * KEY: THE CASE THIS KEY EXISTS FOR. Lines added above an exempted catch;
   * the whitespace in its guarded statement changed (indentation, a tab
   * inside, trailing spaces, CRLF line endings); the statement wrapped after
   * its `=`; a comment put inside it and at the end of its line; lone-CR
   * line endings -- each in a copy of src/, and every exemption still
   * matches. Keyed by line, the first copy reads the two failures items 18
   * and 19 each froze with; keyed by the first line, the wrap and the
   * comment would unmatch it (r2 of item 35).
   */
  it('keeps an exemption through lines added above it and changed whitespace', function () {
    const text = fs.readFileSync(path.join(root, 'src', 'saving.ts'), 'utf8');
    assert.strictEqual(text.split(STATEMENT).length, 2, 'saving.ts no longer holds the statement this cell moves');
    assert.deepStrictEqual(
      judgeCopy((t) => `/*\n * five lines\n * added\n * above\n */\n${t}`),
      { unexplained: [], stale: [] },
      'lines added above an exempted catch unmatched it'
    );
    const copies: Array<[string, (text: string) => string]> = [
      ['changed whitespace, CRLF', (t) => t.replace(STATEMENT, `  ${STATEMENT.replace(' = ', '  =\t')}   `).replace(/\n/g, '\r\n')],
      ['a wrap after `=`', (t) => t.replace(STATEMENT, STATEMENT.replace(' = ', ' =\n        '))],
      ['a comment inside and at the end of the line', (t) => t.replace(STATEMENT, `${STATEMENT.replace(' = ', ' /* why */ = ')} // read`)],
      ['lone-CR line endings', (t) => t.replace(/\r?\n/g, '\r')]
    ];
    for (const [what, change] of copies) {
      assert.deepStrictEqual(judgeCopy(change), { unexplained: [], stale: [] }, `${what} unmatched an exemption`);
    }
  });

  /*
   * AND THE TWIN: the guarded statement itself rewritten -- one space
   * before its semicolon -- and the exemption must NOT match: both cells
   * above say so, naming the old anchor as stale and the new one as
   * unexplained. Without it, a key that matched everything would pass.
   */
  it('lets a rewritten guarded statement unmatch its exemption', function () {
    assert.deepStrictEqual(
      judgeCopy((t) => t.replace(STATEMENT, 'bytes = this.files.readBytes(file) ;')),
      {
        unexplained: ['saving.ts#recognise: bytes = this.files.readBytes(file) ;'],
        stale: ['saving.ts#recognise: bytes = this.files.readBytes(file);']
      },
      'a rewritten guarded statement still matched its old exemption'
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
