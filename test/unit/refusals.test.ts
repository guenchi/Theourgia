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
 * EVERY REFUSAL THE CORE CAN MAKE, AGAINST THE ONE TABLE THAT SORTS THEM.
 *
 * NOTE: THE LIST IS READ FROM THE CORE, NOT WRITTEN HERE.
 *
 * I wrote one by hand first, from the cells in `saver.test.ts` that
 * happened to exercise a refusal. It had seven names and looked
 * complete. The core pinned when this was written made TWENTY-FIVE, several of them on the
 * write path, so a save could receive a refusal this client had never
 * heard of -- and the sorting table would have taken the default branch
 * with nobody the wiser. A list of what our own cells have seen is not a
 * list of what the other side can say. (The same lesson this tree
 * already carries as "the manifest is complete only against its own
 * list".)
 *
 * HOW IT IS READ: Chez reads actual forms from the core's top-level
 * source files, listed from the directory rather than from git.
 * The inventory recognizes literal error constructors and excludes comments.
 * Dynamic refusal constructors still need an exported core schema to make
 * this a complete semantic catalog; the source pin remains reproducible.
 *
 * NOTE: AND IT IS THE WRONG PLACE TO BE ASKING. Which refusals a verb can
 * produce is a fact the CORE knows; deducing it by reading its source is
 * a stopgap, and the main session has put a machine-readable table on
 * the core's queue. When that lands, both tables here are derived from
 * it rather than judged by me.
 */

import * as assert from 'assert';
import {execFileSync} from 'child_process';
import {mkdtempSync, readFileSync, readdirSync, writeFileSync} from 'fs';
import * as path from 'path';
import * as ts from 'typescript';
import * as os from 'os';
import { Client } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { NOT_A_WRITES_ANSWER, SETTINGS_REFUSALS, Saver, classifyRefusal } from '../../src/saver';
import { RawResult } from '../../src/transport';
import { initWire, parseAnswers } from '../../src/wire';
import { IGNORED_DURABILITY } from '../support/ignored-durability';

/*
 * THE PIN IS NAMED IN EVERY FAILURE. A reading taken against an
 * unrecorded version of the core is a reading nobody can repeat, and the
 * two tables are about one particular core.
 */
function coreSources(): { directory: string; files: string[] } {
  const directory = process.env.THEOURGIA_CORE;
  assert.ok(
    directory !== undefined && directory.length > 0,
    'THEOURGIA_CORE is not set, so the refusals this cell is about cannot be read from the core'
  );
  /*
   * NOTE: THE DIRECTORY IS LISTED, NOT THE INDEX. This read the core's
   * sources with `git ls-files`, which answers "what is tracked" and not
   * "what is there". Two readings, both taken:
   *
   *   * a core checkout that is not a git working tree answers `fatal:
   *     not a git repository`, and all three cells below fail with it.
   *     `git init` in that same copy made them pass -- a reading about
   *     the copy rather than about the core.
   *   * a core that IS a git checkout, with one library file added to
   *     the directory but not yet to the index, answers WITHOUT it, and
   *     that is the worse of the two because nothing goes red. Measured
   *     with the `working` source (a `.ss` file then) left untracked: the inventory fell from 47
   *     refusal kinds to 43, losing `invalid-working-baseline`,
   *     `no-draft`, `working-unavailable` and `working-version-changed`
   *     -- four refusals a save can actually receive -- and every cell
   *     here still passed, because each one asks about the kinds it
   *     found.
   *
   * Top level only, as before: the libraries live beside `theourgia.sc`,
   * and `test/` holds fixtures rather than core sources.
   *
   * NOTE: `.sc` AND `.ss` BOTH. The core renamed its sources to `.sc`
   * (276d9f2) and kept `build.ss` a script; a filter on one extension
   * read a renamed core as having no refusals at all -- measured on the
   * F46 pin, 0 kinds, which the first cell below turns red.
   */
  const files = readdirSync(directory as string, {withFileTypes: true})
    .filter(entry => entry.isFile() && (entry.name.endsWith('.sc') || entry.name.endsWith('.ss')))
    .map(entry => entry.name)
    .sort()
    .map(name => path.join(directory as string, name));
  assert.ok(files.length > 0, `no core sources under ${directory as string}`);
  return { directory: directory as string, files };
}

function censusRows(): string[] {
  const {files}=coreSources();
  const output=execFileSync(process.env.THEOURGIA_SCHEME??'scheme',
    ['--script',path.join(__dirname,'../support/core-refusals.ss'),...files],{encoding:'utf8'});
  return output.trim().split('\n');
}

function kindsTheCoreMakes(): Map<string, string> {
  const found=new Map<string,string>();
  for(const row of censusRows()) {
    if(row.startsWith('#'))continue;
    const [file,kind]=row.split('\t');if(kind)found.set(kind,path.basename(file));
  }
  return found;
}

/*
 * THE CONSTRUCTOR CALLS WHOSE KIND THE CENSUS CANNOT READ, one row each:
 * `#variable`, the constructor, the file.
 */
function variableRows(): string[][] {
  return censusRows().filter((row) => row.startsWith('#variable\t')).map((row) => row.split('\t').slice(1));
}

/*
 * THE KINDS MADE AS A LOG CONDITION THAT IS RAISED, from the `#raised` rows.
 */
function raisedKinds(): Set<string> {
  return new Set(censusRows().filter((row) => row.startsWith('#raised\t')).map((row) => row.split('\t')[1]));
}

/*
 * THE KINDS NOT_A_WRITES_ANSWER EXCUSES AS "RETURNED, NEVER RAISED": records
 * in a writer's integrity list. Named here, not read off the rows' prose, so
 * that the cell below asks a question of the core.
 */
const RETURNED_ONLY = [
  'torn-in-sealed', 'frame', 'seq', 'crc',
  'retired-malformed', 'retired-missing-segment', 'retired-mismatch',
  'manifest-missing-segment', 'manifest-hash', 'manifest-range', 'retired-beyond-file'
];

describe('U-ref every refusal the core can make is sorted by name, not by default', () => {
  before(async () => {
    await initWire();
  });

  /*
   * THE INSTRUMENT'S OWN FIRST READING. An AST walk that found nothing
   * would make every check below vacuous, and "no refusals found" is
   * exactly what a changed spelling in the core would produce.
   */
  it('finds the refusals in the core at all', () => {
    const kinds = kindsTheCoreMakes();
    assert.ok(
      kinds.size >= 20,
      `only ${kinds.size} refusal kinds were found in the core, which is too few to be reading ` +
        'its sources; the literal error constructors may have changed'
    );
  });

  /*
   * THE ONE LOG CONDITION MADE FROM A KIND THE CENSUS CANNOT READ: the
   * `note!` in log.sc's `validate`, whose kinds are the literal ones its
   * callers pass (read through `note!` and `cut!`) or one read off a caught
   * condition. A second such call is a kind nobody has sorted, and this
   * turns red rather than the census missing it.
   */
  it('finds exactly one log condition made from a kind it cannot read', () => {
    assert.deepStrictEqual(
      variableRows().map(([constructor, file]) => [constructor, path.basename(file)]),
      [
        ['make-log-error', 'log.sc'],
        ['list-refused', 'log.sc'],
        ['list-refused', 'log.sc']
      ]
    );
  });

  /*
   * AN EXCUSE BY NAME IS CHECKED AGAINST THE CORE'S OWN RAISES: a kind excused
   * as a returned record that the core now raises would otherwise keep its
   * row and pass.
   */
  it('finds none of the kinds excused as returned records raised', () => {
    const raised = raisedKinds();
    assert.ok(raised.has('meta') && raised.has('writer-stopped'), `the #raised rows are not being read: ${[...raised]}`);
    assert.deepStrictEqual(RETURNED_ONLY.filter((kind) => raised.has(kind)), []);
    assert.deepStrictEqual(
      RETURNED_ONLY.filter((kind) => NOT_A_WRITES_ANSWER[kind] === undefined),
      [],
      'a kind named here has no row to excuse it'
    );
  });

  /*
   * THE SCANNER MATCHES A CONSTRUCTOR ONLY AT A LIST'S HEAD. Read on a small
   * file of the shapes a walk over every pair would mistake for calls: a
   * constructor's name in a list's tail, and in an export list. Only the one
   * real call counts.
   *
   * NOTE: A BINDING IS NOT COVERED. `(let ((make-log-error f)) ...)` stands as a
   * list with the name at its head, which no walk over forms can tell from a
   * call; the core has none, and the census states that it cannot show itself
   * complete.
   */
  it('matches a constructor only at the head of a list, not in a tail or an export list', () => {
    const dir = mkdtempSync(path.join(os.tmpdir(), 'theourgia-census-shapes-'));
    const file = path.join(dir, 'shapes.sc');
    writeFileSync(
      file,
      [
        '(library (shapes) (export make-log-error log-error?) (import (chezscheme))',
        "  (define (a) (list make-log-error 'fake-tail))",
        "  (define (b) (vector 1 make-log-error 'fake-middle 2))",
        "  (define (c) (raise (make-log-error 'real-kind #f #f #f '()))))",
        ''
      ].join('\n')
    );
    const rows = execFileSync(process.env.THEOURGIA_SCHEME ?? 'scheme', ['--script', path.join(__dirname, '../support/core-refusals.ss'), file], {
      encoding: 'utf8'
    })
      .trim()
      .split('\n')
      .map((row) => row.split('\t').slice(0, 2).join(' '));
    assert.deepStrictEqual(rows, ['#raised real-kind', `${file} real-kind`]);
  });

  it('reads the kinds the request ledger conses and the log conditions make', () => {
    const kinds = kindsTheCoreMakes();
    for (const kind of [
      'incomplete-request', 'meta', 'crc', 'writer-stopped', 'segment-unreadable',
      'serve-busy', 'owner-unreadable', 'not-needed', 'registry-unreadable'
    ]) {
      assert.ok(kinds.has(kind), `the census does not read ${kind}`);
    }
  });

  it('has a verdict or a stated provenance for every one of them', () => {
    const kinds = kindsTheCoreMakes();
    const unaccounted: string[] = [];
    for (const [kind, where] of kinds) {
      if (NOT_A_WRITES_ANSWER[kind] !== undefined) {
        continue;
      }
      /*
       * NOTE: IT IS PUT THROUGH THE PRODUCT'S OWN CLASSIFIER, not compared
       * with a copy of its table. A cell that restated the table would
       * be checking its own copy -- and the mark is the product's way of
       * saying "nobody has looked at this one", which is precisely what
       * is being asked.
       */
      const answer = parseAnswers(`(error ${kind} (detail "x"))\n`)[0];
      const settlement = classifyRefusal(answer);
      if (settlement.verdict === 'refused' && settlement.unrecognised !== undefined) {
        unaccounted.push(`${kind} (${where})`);
      }
    }
    assert.deepStrictEqual(
      unaccounted,
      [],
      'the core can answer with these and nothing has sorted them: give each a verdict, or a row ' +
        'in NOT_A_WRITES_ANSWER saying where the core makes it'
    );
  });

  /*
   * AND THE EXCLUSION LIST DOES NOT OUTLIVE THE CORE. A row for a kind
   * the core no longer makes is a licence nobody asked for, kept where
   * the next reader will believe it.
   */
  it('keeps no excuse for a refusal the core no longer makes', () => {
    const kinds = kindsTheCoreMakes();
    const stale = Object.keys(NOT_A_WRITES_ANSWER).filter((kind) => !kinds.has(kind));
    assert.deepStrictEqual(stale, [], 'these kinds are excused and the core does not make them');
  });

  /*
   * A ROW'S LINES ARE STILL WHERE ITS KIND IS MADE. Every row of
   * NOT_A_WRITES_ANSWER that cites a line of the core cites, among its lines,
   * one that holds the kind itself, read at the pinned core. A re-pin moves
   * the core's lines and its citations are moved by their text; a row whose
   * lines all moved somewhere else, or whose file was renamed, is red here
   * rather than read as provenance (a reading of all 175 citations against
   * their sentences found six that pointed at unrelated lines, archive/
   * theourgia-vsc-citation-audit-2026-10-02).
   *
   * NOTE: AT LEAST ONE, NOT EVERY ONE. A row also cites where its kind is
   * relayed or caught (rpc.sc's `(cons 'error (cdr a))` for adopt's), a line
   * that names no kind; asking that of every cited line would be red on rows
   * that are right.
   *
   * NOTE: AS A SYMBOL IN CODE, NOT AS A WORD. The kind counts only where the
   * code names it -- quoted, `'seq`, `(quote seq)`, or after `error` at the
   * head of a quoted or quasiquoted `(error seq ...)` -- and only as the whole
   * symbol (`seq?` is another one). Strings, `#|...|#`, character literals,
   * datum comments and the comment after `;` are taken out first. A word match let a row pass on a comment
   * that mentioned its kind, and would let a `seq` citation moved onto
   * `(let ((seq ...)))` pass (found in this cell's review).
   *
   * NEVER: A READER OF FILES. Each cited line is tokenised on its own, so a
   * string, a block comment or a datum comment that spans lines is not seen
   * as one. The cell is here to catch a row whose lines a re-pin moved onto
   * something else, and a line-at-a-time reading is enough for that; the
   * lines in the cell pin what it does.
   */
  it('cites, in every row that cites the core, a line that holds the row\'s kind', () => {
    /*
     * ONE LINE OF SCHEME AS TOKENS: `(` and `)` (brackets too), the quote
     * prefixes, a string or a character literal as one token each, and
     * every other atom up to a delimiter; `#| ... |#` (nested) and the rest
     * of the line after `;` are dropped, and a datum comment `#;` drops the
     * datum after it, prefixes and all.
     */
    const tokens = (line: string): string[] => {
      const out: string[] = [];
      let depth = 0;
      let i = 0;
      while (i < line.length) {
        if (depth > 0) {
          if (line.startsWith('|#', i)) {
            depth -= 1;
            i += 2;
          } else if (line.startsWith('#|', i)) {
            depth += 1;
            i += 2;
          } else {
            i += 1;
          }
          continue;
        }
        const c = line[i];
        if (/\s/.test(c)) {
          i += 1;
        } else if (line.startsWith('#|', i)) {
          depth = 1;
          i += 2;
        } else if (line.startsWith('#;', i)) {
          out.push('#;');
          i += 2;
        } else if (c === ';') {
          break;
        } else if (c === '"') {
          let j = i + 1;
          while (j < line.length && line[j] !== '"') {
            j += line[j] === '\\' ? 2 : 1;
          }
          out.push('"string"');
          i = j + 1;
        } else if (line.startsWith('#\\', i)) {
          let j = i + 3;
          while (j < line.length && /[A-Za-z0-9]/.test(line[j])) {
            j += 1;
          }
          out.push('#\\char');
          i = j;
        } else if ('([{'.includes(c) || ')]}'.includes(c)) {
          out.push('([{'.includes(c) ? '(' : ')');
          i += 1;
        } else if (line.startsWith(',@', i)) {
          out.push(',@');
          i += 2;
        } else if ("'`,".includes(c)) {
          out.push(c);
          i += 1;
        } else {
          let j = i;
          while (j < line.length && !/[\s()[\]{}";'`,]/.test(line[j])) {
            j += 1;
          }
          /*
           * `#(` opens a vector and `#vu8(` a bytevector: a list for this
           * reading, so that a datum comment over one drops all of it.
           */
          if (line[i] === '#' && line[j] === '(') {
            out.push('(');
            i = j + 1;
          } else {
            out.push(line.slice(i, j));
            i = j;
          }
        }
      }
      const prefixes = ["'", '`', ',', ',@'];
      const after = (k: number): number => {
        while (k < out.length && prefixes.includes(out[k])) {
          k += 1;
        }
        if (k < out.length && out[k] === '#;') {
          return after(after(k + 1));
        }
        if (k < out.length && out[k] === '(') {
          let open = 0;
          do {
            open += out[k] === '(' ? 1 : out[k] === ')' ? -1 : 0;
            k += 1;
          } while (k < out.length && open > 0);
          return k;
        }
        return k + 1;
      };
      const kept: string[] = [];
      for (let k = 0; k < out.length; ) {
        if (out[k] === '#;') {
          k = after(k + 1);
        } else {
          kept.push(out[k]);
          k += 1;
        }
      }
      return kept;
    };
    /*
     * NAMED IN CODE: the kind as a whole atom that the line quotes -- `'k`,
     * `(quote k)`, or anywhere inside a quoted or quasiquoted list
     * (`'(error k ...)`, `'(absent unreadable unwritable)`), and not where a
     * `,` or `,@` unquotes it.
     *
     * NOTE: NAMED, NOT MADE. A line that tests for the kind
     * (`(eq? ... 'metadata-unreadable)`, log.sc:914 at ad0bd47) names it in
     * code as well. Telling the two apart is reading the program, not a line.
     * This cell exists to catch a row whose lines a re-pin moved onto
     * something else, and a line naming the row's own kind in code is not
     * where a moved citation lands by accident.
     */
    const makes = (kind: string, line: string): boolean => {
      const t = tokens(line);
      const closes = new Map<number, number>();
      const opens: number[] = [];
      t.forEach((token, k) => {
        if (token === '(') {
          opens.push(k);
        } else if (token === ')' && opens.length > 0) {
          closes.set(opens.pop() as number, k);
        }
      });
      /*
       * `(quote X)` read as `'X`: the open and `quote` become a quote, and
       * the matching close is dropped.
       */
      const read: string[] = [];
      const dropped = new Set<number>();
      for (let k = 0; k < t.length; k += 1) {
        if (dropped.has(k)) {
          continue;
        }
        if (t[k] === '(' && t[k + 1] === 'quote' && closes.has(k)) {
          read.push("'");
          dropped.add(closes.get(k) as number);
          k += 1;
        } else {
          read.push(t[k]);
        }
      }
      const lists: boolean[] = [];
      let quote = false;
      let unquote = false;
      for (const token of read) {
        if (token === "'" || token === '`') {
          quote = true;
          continue;
        }
        if (token === ',' || token === ',@') {
          unquote = true;
          continue;
        }
        const quoted = !unquote && (quote || (lists.length > 0 && lists[lists.length - 1]));
        quote = false;
        unquote = false;
        if (token === '(') {
          lists.push(quoted);
        } else if (token === ')') {
          lists.pop();
        } else if (token === kind && quoted) {
          return true;
        }
      }
      return false;
    };
    for (const [line, held] of [
      ["(raise '(error seq x))", true],
      ['(display ";") (list \'error \'seq)', true],
      ["(f #\\; 'seq)", true],
      ["(f #\\space 'seq)", true],
      ["(raise '(error  seq))", true],
      ["(raise ' (error seq))", true],
      ['(quote (error seq))', true],
      ['( quote seq)', true],
      ['( quote (error seq))', true],
      ['`(error seq ,x)', true],
      ["(list #;ignored 'seq)", true],
      ['(list #;("(") \'seq)', true],
      ["#| 'seq |#", false],
      ["#| outer #| inner |# 'seq |#", false],
      ['(display "\'seq")', false],
      ['(list \'"x"(error seq))', false],
      ['(list \'se"x"q)', false],
      ['(error seq)', false],
      ["(list 'error 'seq?)", false],
      ["(list 'seq@)", false],
      ["(let ((seq (cadr r)))", false],
      [";; 'seq", false],
      ["(list #;#('seq) 'other)", false],
      ["(list #;#vu8(1 2) 'seq)", true],
      ["((memq (log-error-kind (car es)) '(segment-unreadable seq))", true],
      ["(define table-kinds '(absent seq unwritable))", true],
      ["`(a ,seq)", false],
      ["`(a ,@(list seq))", false],
      ["(quote (a (b seq)))", true],
      ["(list #;'seq 'other)", false],
      ["(list #;('seq) 'other)", false],
      ["(list #;' 'seq 'other)", false],
      ["(list #;['seq] 'other)", false]
    ] as const) {
      assert.strictEqual(makes('seq', line), held, `the reader of a cited line got ${line} wrong`);
    }
    const { directory } = coreSources();
    const cite = /\b([a-z0-9-]+\.sc):(\d+(?:-\d+)?(?:(?:, ?| and )\d+(?:-\d+)?)*)/g;
    const lost: string[] = [];
    let rows = 0;
    for (const [kind, where] of Object.entries(NOT_A_WRITES_ANSWER)) {
      const found = [...where.matchAll(cite)];
      if (found.length === 0) {
        continue;
      }
      rows += 1;
      let held = false;
      for (const [, file, spec] of found) {
        const source = path.join(directory, file);
        let lines: string[];
        try {
          lines = readFileSync(source, 'utf8').split('\n');
        } catch {
          lost.push(`${kind}: ${file} is not in the pinned core`);
          continue;
        }
        /*
         * Each part of a list on its own: `1619 and 1800-1803` is 1619 and
         * every line from 1800 to 1803.
         */
        const range = spec.split(/, ?| and /).flatMap((part) => {
          const [from, to] = part.split('-').map(Number);
          return Array.from({ length: (to ?? from) - from + 1 }, (_, i) => from + i);
        });
        if (range.some((n) => n >= 1 && n <= lines.length && makes(kind, lines[n - 1]))) {
          held = true;
        }
      }
      if (!held) {
        lost.push(`${kind}: none of ${found.map((m) => m[0]).join(', ')} holds it`);
      }
    }
    assert.ok(rows > 50, `only ${rows} rows cite the core, so the reader of this table found too little to judge`);
    assert.deepStrictEqual(lost, [], 'these rows cite lines that no longer hold their kind: read each against the pinned core');
  });
});

/*
 * THE F100b RE-PIN'S TWO PROVENANCE ROWS STAY TRUE ONLY WHILE THIS EXTENSION
 * SENDS WHAT IT SENDS. `candidate-unreadable` is made only by the core's
 * `publish` verb and `socket-dir-missing` only for a `--socket` other than the
 * default (NOT_A_WRITES_ANSWER, saver.ts). Each cell below is a tripwire, not a
 * measurement: it is green today and turns red the day a request builder
 * starts sending the one or passing the other, which is the day those rows
 * become false.
 */
/*
 * THE KINDS FIRST READ THROUGH THE CONSED AND LOG-CONDITION CONSTRUCTORS,
 * each answered to a save through a real Saver: where it lands is the
 * family it was sorted into, not a reading of the tables.
 */
describe('U-ref the kinds the ledger conses and the log conditions raise, each in its family', () => {
  before(async () => {
    await initWire();
  });

  const CHECK = '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';

  interface Answered {
    req: string;
    status: string;
    message: string;
    kept: boolean;
    queued: number | null;
    state: string | null;
    settled: string[];
    sets: string[];
  }

  /*
   * ONE SAVE ANSWERED WITH THE KIND, then one retry: what the Saver did with
   * it, the entry's state after the save, what the settler was told, and the
   * request id each write went out under.
   */
  async function saveAnswered(kind: string): Promise<Answered> {
    const sets: string[] = [];
    const client = new Client({
      kind: 'test',
      send: async (verb: string, args: string[]): Promise<RawResult> => {
        const argv = [verb, ...args];
        if (verb === 'check') {
          return { argv, rc: 0, stdout: CHECK, stderr: '' };
        }
        const at = args.indexOf('--req');
        sets.push(at < 0 ? '(no --req)' : args[at + 1]);
        return { argv, rc: 1, stdout: `(error ${kind} (detail "x"))\n`, stderr: '' };
      }
    });
    const outbox = new Outbox(path.join(mkdtempSync(path.join(os.tmpdir(), 'theourgia-family-')), 'outbox.json'));
    outbox.load();
    const settled: string[] = [];
    const saver = new Saver(
      client,
      outbox,
      (req, settlement) => {
        settled.push(settlement.verdict);
        if (settlement.verdict !== 'req-mismatch') {
          outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
        }
      },
      IGNORED_DURABILITY
    );
    const outcome = await saver.save('a.2', 'title', 'T');
    const state = outbox.entries.length > 0 ? outbox.entries[0].state : null;
    await saver.retry();
    return {
      req: outcome.req,
      status: outcome.status,
      message: outcome.message,
      kept: outcome.keptForAPerson === true,
      queued: outbox.pendingCount,
      state,
      settled,
      sets
    };
  }

  it('keeps pending, and sends again under the same id, what the write session raises', async () => {
    for (const kind of ['reset-pending', 'stale-epoch', 'writer-stopped']) {
      const r = await saveAnswered(kind);
      assert.deepStrictEqual([kind, r.status, r.state, r.queued, r.settled], [kind, 'pending', 'pending', 1, []]);
      assert.match(r.message, /cannot say whether this save ran/, kind);
      assert.deepStrictEqual(r.sets, [r.req, r.req], `${kind}: the retry did not go out again under the save's own request id`);
    }
  });

  it('sends again later, under the same id, what says "not now"', async () => {
    for (const kind of ['active-operation', 'temp']) {
      const r = await saveAnswered(kind);
      assert.deepStrictEqual([kind, r.status, r.state, r.queued, r.settled], [kind, 'pending', 'pending', 1, []]);
      assert.match(r.message, new RegExp(`not taking writes just now \\(${kind}\\)`), kind);
      assert.deepStrictEqual(r.sets, [r.req, r.req], `${kind}: the retry did not go out again under the save's own request id`);
    }
  });

  it('parks at once, naming the setting, a store the core cannot open, and a retry does not send it', async () => {
    const r = await saveAnswered('meta');
    assert.deepStrictEqual([r.status, r.kept, r.state, r.queued, r.settled], ['refused', true, 'parked', 1, []]);
    assert.strictEqual(r.message, SETTINGS_REFUSALS.meta);
    assert.deepStrictEqual(r.sets, [r.req], 'a parked entry was sent again by a plain retry');
  });

  it('keeps for a person, with the settlement that marks the record unresolved, what no retry can settle', async () => {
    for (const kind of ['incomplete-request', 'registry-malformed', 'manifest']) {
      const r = await saveAnswered(kind);
      assert.deepStrictEqual([kind, r.status, r.kept, r.queued, r.state], [kind, 'refused', true, 1, 'sent']);
      assert.deepStrictEqual(r.settled, ['req-mismatch', 'req-mismatch'], `${kind}: the settler was not told to keep it`);
      assert.deepStrictEqual(r.sets, [r.req, r.req], `${kind}: the kept entry did not go out again under its own request id`);
      assert.match(r.message, new RegExp(kind), kind);
    }
  });
});

describe('re-pin: what this extension sends keeps these answers out of its reach', () => {
  const SRC = path.join(__dirname, '..', '..', '..', 'src');
  const sources = (): Array<{ name: string; text: string }> =>
    readdirSync(SRC)
      .filter((name) => name.endsWith('.ts'))
      .sort()
      .map((name) => ({ name, text: readFileSync(path.join(SRC, name), 'utf8') }));

  /*
   * THE VERBS THE REQUEST BUILDERS SEND, read from the calls, not from
   * client.ts's table of verbs it knows: every `.request(<first>, ...)`; a
   * quoted first argument is the verb, and a name is followed to its `const`
   * in the same file, whose quoted strings are the verbs it can hold. A first
   * argument of any other form is reported, not passed over.
   */
  function verbsSent(): { verbs: Set<string>; unread: string[] } {
    const verbs = new Set<string>();
    const unread: string[] = [];
    for (const { name, text } of sources()) {
      for (const call of text.matchAll(/\.request\(\s*([^,)]+)/g)) {
        const first = call[1].trim();
        const quoted = /^'([a-z-]+)'$/.exec(first);
        if (quoted !== null) {
          verbs.add(quoted[1]);
          continue;
        }
        const held = /^[A-Za-z_]\w*$/.test(first)
          ? new RegExp(`const ${first}\\s*(?::[^=]+)?=\\s*([^;]+);`).exec(text)
          : null;
        if (held === null) {
          unread.push(`${name}: ${first}`);
          continue;
        }
        for (const literal of held[1].matchAll(/'([a-z-]+)'/g)) {
          verbs.add(literal[1]);
        }
      }
    }
    return { verbs, unread };
  }

  it('sends no `publish` (the only verb that answers candidate-unreadable)', () => {
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    assert.strictEqual(verbs.has('publish'), false, 'this extension now sends publish, so candidate-unreadable can answer it');
  });

  /*
   * `adopt` IS AMONG THE VERBS THIS CLIENT MAY SEND (client.ts, KNOWN_VERBS)
   * AND IS SENT NOWHERE. Its refusals -- owner-unreadable, registry-unreadable,
   * not-needed, and segment-unreadable and metadata-unreadable as heads -- are
   * excused in NOT_A_WRITES_ANSWER on that ground; the day something sends it,
   * they need a verdict.
   */
  /*
   * THE PROJECT TEMPLATE'S REFUSALS (f34d84f) ANSWER ONLY `template` AND
   * `init --template` or `--template-file`, and are excused in
   * NOT_A_WRITES_ANSWER on that ground.
   * This extension makes no store and applies no template; the day it does,
   * they need a verdict.
   */
  it('sends no `template` and no `init` (the only verbs the template refusals answer)', () => {
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    assert.strictEqual(verbs.has('template'), false, 'this extension now sends template, so the template refusals can answer it');
    assert.strictEqual(verbs.has('init'), false, 'this extension now sends init, so the refusals of init --template and --template-file can answer it');
  });

  it('sends no `adopt` (the only verb whose answers carry its refusals as heads)', () => {
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    assert.strictEqual(verbs.has('adopt'), false, 'this extension now sends adopt, so its refusals can answer it');
  });

  /*
   * QUEUE ITEM 13: IMPORTED SOURCE THAT IS NOT UTF-8 IS STORED AND CANNOT BE
   * FOUND. `import-code` without `--datum` on a file whose bytes are not
   * valid UTF-8 puts the block in the store, and none of its text is
   * visible to `search` or `grep` (core F61, not fixed). The only signal is
   * the count `(scanned ... (unreadable-blocks m))` of a `--wire` search or
   * grep answer (store.sc:1446 `search-report`, store.sc:1896 `report`).
   * This extension sends neither import verb today, so the symptom is out of
   * its reach. The day it grows a command that imports a file or a
   * directory, that command ships with a reading of that count -- the user
   * imported the file, sees it in the tree and cannot find it by searching,
   * and nothing else says why -- or it ships saying that it cannot explain
   * this. The cell is the tripwire for that day, not a measurement.
   */
  it('sends no `import-code` or `import-md` (queue item 13: an import ships with the unreadable-blocks count)', () => {
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    const imports = ['import-code', 'import-md'].filter((verb) => verbs.has(verb));
    assert.deepStrictEqual(
      imports,
      [],
      'this extension now sends an import verb: read and show (scanned ... (unreadable-blocks m)) with it, or say it cannot (queue item 13)'
    );
  });

  /*
   * THE TWO PROVENANCE ROWS READ AT THE cba98ae RE-PIN STAY TRUE ONLY WHILE THIS
   * EXTENSION SENDS NONE OF THE CORE'S UNDECLARED VERBS. `incomplete-reduction`
   * is refused only to a verb that does not declare it accepts a store
   * missing a writer, and `working-draft-unreadable` is reached only through
   * a writer's working view (`export-md`, `export-code --working`, `supply
   * --for`, derived.sc's working readers); the export and supply verbs are on
   * the core's own list (rpc.sc:2226). The list is READ FROM THE PINNED CORE, not copied
   * here, so a verb the core adds to it is checked without an edit in this
   * file. A tripwire, not a measurement.
   *
   * NOTE: THE SUPPLY COMMANDS SEND TWO OF THEM, `export-code` and `supply`,
   * and show whatever refusal either answers by name (src/supply.ts; the
   * cells in supply.test.ts drive both with `incomplete-reduction`). The
   * read-only view of a datum block sends `export-code --datum` too, and
   * says the store's refusal as the core wrote it (src/datum-view.ts; the
   * cell "says the store's refusal of the export" in datum-view.test.ts).
   * Those, from those two files only, are the expected ones; any other is
   * red.
   */
  it('sends of the core\'s undeclared verbs only export-code and supply, from the supply commands and the datum view', () => {
    const rpc = readFileSync(path.join(coreSources().directory, 'rpc.sc'), 'utf8');
    const listed = /\(define undeclared-verbs '\(([^)]*)\)\)/.exec(rpc);
    assert.ok(listed !== null, 'the pinned core defines no undeclared-verbs list this cell can read');
    const undeclared = listed[1].trim().split(/\s+/);
    assert.ok(undeclared.includes('export-md'), `the list read is not the one this cell is about: ${undeclared}`);
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    assert.deepStrictEqual(
      undeclared.filter((verb) => verbs.has(verb)).sort(),
      ['export-code', 'supply'],
      'this extension sends a verb the core refuses with incomplete-reduction on a store missing a writer other than the supply commands\' two: sort that refusal and show its notes'
    );
    const senders = sources()
      .filter(({ text }) => /\.request\(\s*'(export-code|supply)'/.test(text))
      .map(({ name }) => path.basename(name));
    assert.deepStrictEqual(senders, ['datum-view.ts', 'supply.ts'], 'export-code or supply is sent from somewhere other than src/supply.ts and src/datum-view.ts');
  });

  /*
   * A `--socket` in an argument list is a quoted string, '--socket' or
   * "--socket"; the backquoted `--socket` of a comment or a provenance row is
   * prose and is not counted.
   */
  it('passes no `--socket` (the only way to be answered socket-dir-missing)', () => {
    const found = sources()
      .filter(({ text }) => /(['"])--socket\1/.test(text))
      .map(({ name }) => name);
    assert.ok(sources().length > 0, 'no source file was read');
    assert.deepStrictEqual(found, [], 'a source file spells --socket, so socket-dir-missing can answer it');
  });
});

/*
 * QUEUE ITEM 12: TWO WORDS THAT BOTH SAY "UNREADABLE", ABOUT DIFFERENT
 * THINGS. The core's `(scanned ... (unreadable-blocks m))` counts the blocks
 * of one search or grep answer whose text is not valid UTF-8 (store.sc:1446
 * `search-report`, store.sc:1896 `report`). This client's
 * `TransportError('unreadable', ...)` (the `TransportFailure` union in
 * transport.ts) means that this client could not read the SHAPE of an
 * answer. The core named its clause with a unit, `-blocks`, so that the two
 * do not merge.
 *
 * They cannot meet today: no request this extension makes asks for that
 * clause. The requirement is for the day somebody renders the number: the
 * rendered words for `(unreadable-blocks m)` are "blocks whose text could
 * not be decoded" -- "N blocks whose text could not be decoded" -- and never
 * "N unreadable blocks", which a reader of this tree would take for the
 * other failure. That sentence belongs beside the `TransportFailure` union
 * when the rendering is written.
 *
 * The cell reads every string and template text in src/ with the TypeScript
 * parser, so comments (this one included) are prose and are not read. The
 * clause's own name as a whole string, 'unreadable-blocks', is how a caller
 * looks the clause up and is not a rendering; any other string holding
 * "unreadable block(s)" or "unreadable-block(s)" is.
 */
describe('queue item 12: the unreadable-blocks count is never rendered with this client\'s word', () => {
  const SRC = path.join(__dirname, '..', '..', '..', 'src');

  function renderings(name: string, text: string): { found: string[]; read: number } {
    const found: string[] = [];
    let read = 0;
    const parsed = ts.createSourceFile(name, text, ts.ScriptTarget.Latest, true);
    const visit = (node: ts.Node): void => {
      if (
        ts.isStringLiteral(node) ||
        ts.isNoSubstitutionTemplateLiteral(node) ||
        ts.isTemplateHead(node) ||
        ts.isTemplateMiddle(node) ||
        ts.isTemplateTail(node)
      ) {
        read += 1;
        const said = node.text;
        if (said !== 'unreadable-blocks' && /unreadable[\s-]+blocks?\b/i.test(said)) {
          found.push(`${name}: ${JSON.stringify(said)}`);
        }
      }
      node.forEachChild(visit);
    };
    visit(parsed);
    return { found, read };
  }

  it('finds no string in src/ that says "unreadable blocks" (the rendered words are "blocks whose text could not be decoded")', () => {
    /*
     * THE READER FIRST, on a source written here: a template rendering the
     * count is found, the clause's name alone is not, and a comment is not.
     */
    const probe = renderings(
      'probe.ts',
      "// 3 unreadable blocks\nconst a = `${n} unreadable blocks`;\nconst b = 'unreadable-blocks';\nconst c = `${n} unreadable-blocks`;\n"
    );
    assert.deepStrictEqual(
      probe.found,
      ['probe.ts: " unreadable blocks"', 'probe.ts: " unreadable-blocks"'],
      'the reader does not find what it exists to find'
    );
    const names = readdirSync(SRC).filter((name) => name.endsWith('.ts')).sort();
    assert.ok(names.length > 0, 'no source file was read');
    let read = 0;
    const found: string[] = [];
    for (const name of names) {
      const one = renderings(name, readFileSync(path.join(SRC, name), 'utf8'));
      read += one.read;
      found.push(...one.found);
    }
    assert.ok(read > 100, `only ${read} strings were read in src/`);
    assert.deepStrictEqual(
      found,
      [],
      'a string in src/ renders the core\'s unreadable-blocks count with this client\'s word "unreadable"; say "blocks whose text could not be decoded" (queue item 12)'
    );
  });
});
