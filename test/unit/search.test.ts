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
 * plugin-r2 T5: reading what `search` answered, and reading which verbs
 * the core has.
 *
 * MEASURED AGAINST THE CORE, NOT AGAINST A BELIEF. Every shape asserted
 * here was produced by a core pinned before F100b (877f0da or earlier) and copied in verbatim:
 *
 *   $ theourgia search "stale baseline" --wire
 *   (ok (items (hit "qqevbgov.1" 6 "concurrency, baseline, stale")))
 *   $ theourgia search "brown fox" --wire
 *   (ok (items (hit "qqevbgov.3" 1 "the quick brown fox jumps over the la")))
 *   $ theourgia search "nothinghere" --wire
 *   (ok (items))
 *   $ theourgia search "stale" "baseline" --wire
 *   (usage (search <query>))
 *
 * The third field is the keywords when the match was found in them and a
 * cut of the text otherwise; the core does not say which, and neither
 * does this -- it is shown as the line under the title, which is true of
 * both.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';
import { Asking, Hit, Searcher, hitsOf, knownVerbs, rankHits, runSearch } from '../../src/search';
import { answerOf, initWire, parseAnswers } from '../../src/wire';
import { interpret } from '../../src/client';
import { Working } from '../../src/working';
import { eventFromWrite, firstCursorFromCheck, isReplay } from '../../src/cursor';
import { RawResult, TransportError } from '../../src/transport';
import { RETRYABLE_REFUSALS, SETTINGS_REFUSALS, Saver, classifyRefusal } from '../../src/saver';
import { sidecarFromDisk } from '../../src/publication';
import { Outbox } from '../../src/outbox';
import * as os from 'os';
import { Client } from '../../src/client';
import { StoreModel } from '../../src/model';
import { CoreConfig, WITNESS_SOURCE, environmentFor, problemsWith } from '../../src/config';
import { behindNotice, statusLine } from '../../src/status';
import { IGNORED_DURABILITY } from '../support/ignored-durability';

let counterAt = 0;
const counter = (): number => (counterAt += 1);

function datum(text: string): unknown {
  return parseAnswers(text)[0];
}

/*
 * EVERY TOP-LEVEL DATUM THE CORE PRINTED, which is what `Client` hands
 * a reader for a verb whose answer is a sequence of items when no envelope
 * was opened. This extension asks a search on the wire route, `--wire`,
 * where the hits are the items of one `(ok (items ...))` the client opens;
 * the cells below hand the reader those items, a `(hit ...)` each.
 */
function data(text: string): unknown[] {
  return parseAnswers(text);
}

describe('plugin-r2 T5 reading a search answer', function () {
  before(async () => {
    await initWire();
  });

  it('reads the id, the score and the line under it from every hit', function () {
    assert.deepStrictEqual(
      hitsOf(data('(hit "qqevbgov.1" 6 "concurrency, baseline, stale")')),
      [{ id: 'qqevbgov.1', score: 6, note: 'concurrency, baseline, stale' }]
    );
  });

  /*
   * plugin-r3: a hit with a fifth element, and one with four.
   *
   * The core's S batch adds `(fields (title keywords ...))` to every hit:
   * ONE list of field names. These fixtures were first written flat,
   * `(fields title keywords)`, from a description of the clause rather than
   * from an answer, and the reader was written to match them -- so every
   * cell here passed while every real hit was refused (see the cells below).
   * A reader asking for a length of exactly four would answer "the
   * search did not happen" for every query the day that lands -- so the
   * four-element shape and the five-element shape are both read, and
   * neither is the one this client requires. `theourgia.corePath` can
   * always name an older core, so four is not a legacy spelling to be
   * removed later; it is one of two shapes that are both current.
   */
  it('reads a hit that carries the fields it matched, and one that does not', function () {
    assert.deepStrictEqual(
      hitsOf(data('(hit "a.1" 6 "k" (fields (title keywords)))')),
      [{ id: 'a.1', score: 6, note: 'k', fields: ['title', 'keywords'] }],
      'the fifth element was not read'
    );
    assert.deepStrictEqual(
      hitsOf(data('(hit "a.1" 6 "k")')),
      [{ id: 'a.1', score: 6, note: 'k' }],
      'a four-element hit stopped being readable'
    );
    /*
     * AND UNDEFINED IS NOT AN EMPTY SET. A hit from an older core says
     * nothing about which fields matched; `(fields ())` from a newer one
     * says none did. A reader that gave `[]` for both would put this
     * client's age into the store's answer.
     */
    assert.deepStrictEqual(hitsOf(data('(hit "a.1" 6 "k" (fields ()))')), [
      { id: 'a.1', score: 6, note: 'k', fields: [] }
    ]);
  });

  it('refuses a hit with fewer than four elements, and one whose fifth is not readable', function () {
    assert.strictEqual(hitsOf(data('(hit "a.1" 6)')), null, 'a three-element hit was read');
    assert.strictEqual(
      hitsOf(data('(hit "a.1" 6 "k" (fields (title)) (fields (keywords)))')),
      null,
      'two fields clauses supplied one of them'
    );
    assert.strictEqual(
      hitsOf(data('(hit "a.1" 6 "k" (fields ("title")))')),
      null,
      'a field name that is not a symbol was read as one'
    );
  });

  /*
   * THE FLAT FORM IS REFUSED. No core printed `(fields title keywords)`:
   * the clause has been `(fields (<field> ...))` since it arrived (README,
   * the search verb). A reader that also took the flat form would read a
   * guessed shape as an answer and hide the next drift.
   */
  it('refuses the flat form, which no core prints', function () {
    assert.strictEqual(hitsOf(data('(hit "a.1" 6 "k" (fields title keywords))')), null, 'the flat form was read');
    assert.strictEqual(hitsOf(data('(hit "a.1" 6 "k" (fields title))')), null, 'a single bare field name was read');
  });

  /*
   * A HIT AS THE REAL CORE PRINTED IT, copied from a search of a real store
   * (theourgia 3aad6fd and 06a348b print it the same). Before this, the
   * reader answered null for it, and the search view said "the store did
   * not answer the search" for every query that found anything.
   */
  it('reads a hit as the real core printed it', function () {
    assert.deepStrictEqual(
      hitsOf(data('(hit "pt45r9ir.3" 2 "alpha old" (fields (src)))\n(hit "pt45r9ir.4" 2 "alpha new" (fields (src)))\n')),
      [
        { id: 'pt45r9ir.3', score: 2, note: 'alpha old', fields: ['src'] },
        { id: 'pt45r9ir.4', score: 2, note: 'alpha new', fields: ['src'] }
      ]
    );
  });

  /*
   * NEVER: THE ENVELOPE IS NOT THIS READER'S TO OPEN, and these two cells
   * used to say the opposite.
   *
   * They asserted that `hitsOf` reads `(ok (items ...))` as hits -- the
   * SECOND reader in this client deciding from the shape of what came
   * back, which is the defect `client.ts` was repaired for. Three review
   * rounds each found a face of it, and the main session ruled: whether
   * an envelope was asked for is a fact about the request, `interpret`
   * holds that fact, and this reader takes the answers it already
   * opened. A form is not a `hit`, so one that reaches here is refused
   * by the loop that was always there.
   *
   * KEY: THIS IS A RULING-DRIVEN CHANGE TO AN EXPECTATION, which is the
   * kind that has to be pointed at rather than made quietly. The old
   * assertions are quoted above so that a reader of this file can see
   * what was believed and what replaced it.
   */
  it('does not open an envelope, because opening one is the request\'s question', function () {
    assert.strictEqual(
      hitsOf(data('(ok (items (hit "qqevbgov.1" 6 "concurrency, baseline, stale")))')),
      null,
      'an envelope was opened here, by a reader that cannot know whether one was asked for'
    );
    assert.strictEqual(hitsOf(data('(ok (items))')), null);
  });

  /*
   * AND THE ROUTE THAT REMAINS STILL READS. `interpret` hands over the
   * items of a `--wire` answer and the raw data of a human one, and both
   * arrive here as a sequence of hits.
   */
  it('reads a sequence of hits, however it was carried here', function () {
    const bare = '(hit "a.1" 6 "x")\n(hit "a.2" 2 "y")';
    assert.deepStrictEqual(hitsOf(data(bare)), [
      { id: 'a.1', score: 6, note: 'x' },
      { id: 'a.2', score: 2, note: 'y' }
    ]);
    /*
     * NEVER: AND THE OTHER CARRIAGE IS DRIVEN BY `interpret`, NOT BY THIS
     * CELL.
     *
     * It used to take the clause apart here -- `(opened[1] as
     * unknown[]).slice(1)` -- and compare the result with the bare hits.
     * Measured in an eighteenth review round: replacing `interpret` with
     * an unconditional throw left this passing, because nothing in the
     * comparison went through it. A cell about how an answer is CARRIED
     * has to use the thing that carries it.
     */
    const carried = interpret(
      { argv: [], rc: 0, stdout: '(ok (items (hit "a.1" 6 "x") (hit "a.2" 2 "y")))', stderr: '' },
      'search',
      'items',
      ['--wire']
    );
    assert.deepStrictEqual(hitsOf(carried.answers), hitsOf(data(bare)));
    assert.strictEqual(hitsOf(carried.answers)?.length, 2);
  });

  /*
   * NO HITS IS AN ANSWER. `(ok (items))` is the store saying it looked
   * and found nothing, and returning the same value for that as for an
   * answer this build could not read would put "nothing matched" in
   * front of a user whose search never happened.
   */
  /*
   * KEY: AN `items` CLAUSE IS NOT AN ANSWER UNLESS SOMETHING SAID `ok`.
   *
   * Found by an outside review, reproduced here: the unwrapping asked
   * only whether a clause named `items` was present and took whatever
   * was inside it, so `(error (items))` -- a refusal that happens to
   * carry an empty clause of that name -- read as "the store looked and
   * found nothing". That is the reassuring answer given at the moment
   * nothing was answered at all.
   */
  it('does not take an items clause out of a form that did not say ok', function () {
    assert.strictEqual(hitsOf(data('(error (items))')), null);
    assert.strictEqual(hitsOf(data('(garbage (items))')), null);
    assert.strictEqual(hitsOf(data('(error (items (hit "a.1" 6 "x")))')), null);
  });

  it('tells no hits apart from an answer it could not read', function () {
    assert.deepStrictEqual(hitsOf(data('')), []);
    assert.strictEqual(hitsOf(data('(usage (search <query>))')), null);
    assert.strictEqual(hitsOf(data('(error store-not-found)')), null);
    assert.strictEqual(hitsOf(data('(hit "a.1" "six" "x")')), null);
    assert.strictEqual(hitsOf(data('(hit "a.1" 6)')), null);
    assert.strictEqual(hitsOf(data('(miss "a.1" 6 "x")')), null);
    assert.strictEqual(hitsOf(data('(hit "a.1" 6 "x")\n(error store-not-found)')), null);
  });

  /*
   * THE ORDER IS THE SCORE, AND TIES ARE BROKEN BY SOMETHING THAT DOES
   * NOT MOVE.
   *
   * KEY: A LIST WHOSE ORDER DEPENDS ON THE ORDER IT ARRIVED IN is a list
   * that draws itself differently on two runs of the same search, and a
   * cell pinned to one of those orders passes until the day it does not.
   * Ids are unique within a store, so sorting equal scores by id gives
   * one answer for one store.
   */
  it('puts the best score first and settles ties by id', function () {
    const found = rankHits([
      { id: 'w.9', score: 2, note: 'b' },
      { id: 'w.2', score: 7, note: 'a' },
      { id: 'w.1', score: 2, note: 'c' }
    ]);
    assert.deepStrictEqual(
      found.map((h) => h.id),
      ['w.2', 'w.1', 'w.9']
    );
  });

  /*
   * KEY: THE EXPECTED ORDER IS WRITTEN DOWN BEFORE THE CALL, not taken
   * from the array the call was given.
   *
   * `assert.deepStrictEqual(rankHits(given), given)` compares the answer
   * with something the implementation can reach: measured in a review
   * round, a `rankHits` that reversed its argument in place passed this,
   * and so did one that emptied it. Both sides moved together.
   */
  it('does not move a hit that arrived in the right place', function () {
    const given = [
      { id: 'w.2', score: 7, note: 'a' },
      { id: 'w.1', score: 2, note: 'c' }
    ];
    const expected = [
      { id: 'w.2', score: 7, note: 'a' },
      { id: 'w.1', score: 2, note: 'c' }
    ];
    assert.deepStrictEqual(rankHits(given), expected);
  });

  /*
   * KEY: AND THE LIST IT WAS HANDED IS NOT THE LIST IT SORTS, which needs
   * an input that is OUT of order to say anything.
   *
   * The assertion was made against an already-sorted input, where an
   * in-place sort leaves everything where it was: measured in an eighth
   * review round, `[...hits].sort(...)` changed to `hits.sort(...)` left
   * all thirty-nine cells passing. A caller that hands its own array over
   * and reads it afterwards -- which `runSearch` does not do today, and
   * which nothing stops the next caller from doing -- would find it
   * rearranged under it.
   */
  it('sorts a copy, leaving the caller\'s list in the order it was given', function () {
    const given = [
      { id: 'w.9', score: 2, note: 'b' },
      { id: 'w.2', score: 7, note: 'a' },
      { id: 'w.1', score: 2, note: 'c' }
    ];
    const ranked = rankHits(given);
    assert.deepStrictEqual(
      ranked.map((h) => h.id),
      ['w.2', 'w.1', 'w.9']
    );
    assert.deepStrictEqual(
      given.map((h) => h.id),
      ['w.9', 'w.2', 'w.1'],
      'ranking rearranged the list it was handed'
    );
  });
});

/*
 * WHICH VERBS THE CORE HAS.
 *
 * NOTE: THE DEFINITION SEARCH IS NOT STUBBED OUT IN THIS EXTENSION. The
 * verb it needs, `whereis`, is not in the core yet. A command that was
 * contributed anyway and answered "not implemented" would be a promise
 * with nobody's name on it -- and a stub is a thing nobody goes back to
 * remove. Instead the core is asked what it can do, `describe` answers
 * with a catalogue, and the entry appears the day the verb does.
 *
 * Measured, from a core pinned before F100b (877f0da or earlier):
 *
 *   $ theourgia describe
 *   (ok (verbs (init (usage (init)) (description "...") ...)
 *              (search (usage (search <query>)) ...)
 *              ...)
 *       (protocol "..."))
 */
describe('plugin-r2 T5 reading the verb catalogue', function () {
  before(async () => {
    await initWire();
  });

  const CATALOGUE =
    '(ok (verbs (init (usage (init)) (description "Create a store in this directory.")' +
    ' (protocol #f) (route local))' +
    ' (search (usage (search <query>)) (description "Find blocks.")' +
    ' (protocol #f) (route daemon)))' +
    ' (protocol "A block is the unit of writing."))';

  it('names every verb the core listed', function () {
    const verbs = knownVerbs(datum(CATALOGUE));
    assert.notStrictEqual(verbs, null);
    assert.deepStrictEqual([...(verbs as Set<string>)].sort(), ['init', 'search']);
  });

  /*
   * KEY: AND SAYS NOTHING RATHER THAN SAYING "NO" when it could not ask.
   * A refused `describe` that came back as an empty set would hide every
   * verb the core has, including the ones this extension has always
   * used; the caller has to be able to tell "the core does not have it"
   * from "I could not find out".
   */
  it('tells a catalogue it could not read apart from a catalogue with nothing in it', function () {
    assert.strictEqual(knownVerbs(datum('(error store-not-found)')), null);
    assert.strictEqual(knownVerbs(datum('(ok (protocol "no verbs clause"))')), null);
    assert.deepStrictEqual(knownVerbs(datum('(ok (verbs))')), new Set());
  });

  /*
   * KEY: A `verbs` CLAUSE IS NOT A CATALOGUE UNLESS THE FORM SAID `ok`.
   *
   * The same defect as the one `hitsOf` was repaired for, in the reader
   * beside it -- and the repair to THIS reader went in without a cell,
   * so removing it again passed everything. Measured in a tenth review
   * round: `(error (verbs))` answered an empty set, which a caller reads
   * as "this core can do nothing", and `(garbage (verbs (search)))`
   * answered a catalogue built out of a form nobody can parse.
   */
  it('does not take a verbs clause out of a form that did not say ok', function () {
    assert.strictEqual(knownVerbs(datum('(error (verbs))')), null);
    assert.strictEqual(knownVerbs(datum('(garbage (verbs (search)))')), null);
    assert.strictEqual(knownVerbs(datum('(usage (describe))')), null);
  });

  it('does not invent a verb the catalogue does not name', function () {
    const verbs = knownVerbs(datum(CATALOGUE)) as Set<string>;
    assert.strictEqual(verbs.has('whereis'), false);
    assert.strictEqual(verbs.has('search'), true);
  });
});

/*
 * plugin-r2 T5: what a search DOES.
 *
 * KEY: THE EDITOR IS THREE FUNCTIONS HERE, so every decision the flow
 * makes is one a cell can drive and read back. A flow that only spoke to
 * the editor would be a flow whose decisions nothing can look at, and
 * the decisions are the whole of it.
 */
function editorThat(
  typed: string | undefined,
  picks: (hits: Hit[]) => Hit | undefined = (hits) => hits[0]
): Asking & {
  opened: string[];
  said: string[];
  levels: string[];
  offered: Hit[][];
  placeholders: string[];
  asked: string[];
} {
  const record = {
    opened: [] as string[],
    said: [] as string[],
    offered: [] as Hit[][],
    placeholders: [] as string[],
    levels: [] as string[],
    /*
     * EVERY PROMPT THE FLOW PUT UP. A cell that says "nothing was asked"
     * needs somewhere to read that from: measured in a review round,
     * adding an input box to the branch that has no store to search left
     * the cell for it passing, because nothing counted the boxes.
     */
    asked: [] as string[],
    ask: async (prompt: string): Promise<string | undefined> => {
      record.asked.push(prompt);
      return typed;
    },
    pick: async (hits: Hit[], placeHolder: string): Promise<Hit | undefined> => {
      record.offered.push(hits);
      record.placeholders.push(placeHolder);
      return picks(hits);
    },
    /*
     * KEY: THE CHANNEL IS RECORDED AS WELL AS THE WORDS.
     *
     * Measured in a tenth review round: with only the text kept, moving
     * the failure and no-store notices from `error` to `information` and
     * the no-hits notice from `information` to `error` left every cell
     * passing. A search that could not be run, shown as a quiet
     * information notice, is a failure the user will not see; "nothing
     * matched", shown as an error, is an alarm about an ordinary answer.
     */
    say: (text: string, level: 'information' | 'error'): void => {
      record.said.push(text);
      record.levels.push(level);
    },
    open: async (id: string): Promise<void> => {
      record.opened.push(id);
    }
  };
  return record;
}

const HITS = (...ids: string[]): Hit[] => ids.map((id, n) => ({ id, score: 9 - n, note: 'k' }));

/*
 * A SEARCHER THAT REMEMBERS WHAT IT WAS ASKED FOR.
 *
 * KEY: THE DOUBLES USED TO IGNORE THEIR ARGUMENT, so nothing in this
 * section observed the query reaching the store. Measured in a review
 * round: replacing `model.search(query)` with `model.search("WRONG
 * QUERY")` left every one of these cells passing. A search flow that
 * searches for the wrong thing is the one failure a user would notice
 * first.
 */
function searcherFor(hits: Hit[]): Searcher & { queries: string[] } {
  const record = {
    queries: [] as string[],
    search: async (query: string): Promise<Hit[]> => {
      record.queries.push(query);
      return hits;
    }
  };
  return record;
}

/*
 * plugin-r2: the settings check is never asked without the directory.
 *
 * KEY: A CELL CANNOT SEE A DEFAULT ARGUMENT COME BACK. Found in a fourth
 * review round, measured by the reviewer: restoring
 * `directory: CoreDirectory | null = null` and the null-skipping
 * condition left every cell in this file passing, because they all pass
 * a directory. What the batch got wrong was a CALL, and a call is
 * something to read out of the source.
 */
describe('plugin-r2 every caller of problemsWith supplies the directory', function () {
  /*
   * EVERY CALL THE COMPILER WOULD COMPILE, with how many arguments it
   * passes. One walk answers both questions this section asks -- are
   * there any calls, and does any of them omit the directory -- because
   * two instruments would be two things to keep in step, and the one
   * that drifted would be the reassuring one.
   */
  const callsInSrc = (): Array<{ where: string; args: number }> => {
    const root = path.join(__dirname, '..', '..', '..');
    const out: Array<{ where: string; args: number }> = [];
    for (const name of fs.readdirSync(path.join(root, 'src'))) {
      if (!name.endsWith('.ts')) {
        continue;
      }
      const text = fs.readFileSync(path.join(root, 'src', name), 'utf8');
      const parsed = ts.createSourceFile(name, text, ts.ScriptTarget.Latest, true);
      const visit = (node: ts.Node): void => {
        if (
          ts.isCallExpression(node) &&
          ts.isIdentifier(node.expression) &&
          node.expression.text === 'problemsWith'
        ) {
          out.push({
            where: `${name}:${parsed.getLineAndCharacterOfPosition(node.getStart(parsed)).line + 1}`,
            args: node.arguments.length
          });
        }
        node.forEachChild(visit);
      };
      visit(parsed);
    }
    return out;
  };

  /*
   * THE CENSUS'S OWN FIRST READING, and it counts CALLS rather than
   * matching text. It used to be a regular expression over the file,
   * which a commented-out call satisfies: measured in a review round,
   * replacing the call with `const problems = []; // problemsWith(...)`
   * left both cells here passing while the extension checked nothing.
   */
  it('finds the call it is about', function () {
    const calls = callsInSrc();
    assert.ok(
      calls.length >= 1,
      'nothing in src calls problemsWith any more, so the cell below reads nothing'
    );
  });

  it('passes two arguments at every call site in src', function () {
    assert.deepStrictEqual(
      callsInSrc().filter((c) => c.args < 2).map((c) => c.where),
      [],
      'these calls ask about the settings without saying what is in the core directory, so the ' +
        'refusal about a directory holding neither sources nor products cannot reach anybody'
    );
  });
});

describe('plugin-r2 T5 what a search does', function () {
  it('opens a single hit without asking anybody to choose', async () => {
    const editor = editorThat('stale baseline');
    const searcher = searcherFor(HITS('w.1'));
    const outcome = await runSearch(searcher, editor);
    assert.deepStrictEqual(
      searcher.queries,
      ['stale baseline'],
      'the words the user typed are not what reached the store'
    );
    assert.deepStrictEqual(outcome, {
      did: 'opened',
      id: 'w.1',
      query: 'stale baseline',
      of: 1
    });
    assert.deepStrictEqual(editor.opened, ['w.1']);
    assert.deepStrictEqual(editor.offered, [], 'a choice was offered between one thing');
  });

  it('offers every hit when there are several, and opens the one chosen', async () => {
    const editor = editorThat('two words', (hits) => hits[1]);
    const searcher = searcherFor(HITS('w.1', 'w.2', 'w.3'));
    const outcome = await runSearch(searcher, editor);
    assert.deepStrictEqual(searcher.queries, ['two words']);
    assert.deepStrictEqual(editor.opened, ['w.2']);
    assert.strictEqual((outcome as { of: number }).of, 3);
    assert.deepStrictEqual(
      editor.offered[0].map((h) => h.id),
      ['w.1', 'w.2', 'w.3']
    );
  });

  /*
   * NOTE: DISMISSING THE LIST OPENS NOTHING. A quick pick that was closed
   * comes back as undefined, which is the same value a list with nothing
   * in it produces -- so the two are kept apart by never showing an
   * empty list, and by this cell.
   */
  /*
   * plugin-r3 item 11: the picker says how many it shows, not how many
   * match.
   *
   * The core answers the best ten by default from its C2 on, and says how
   * many it cut only under `--wire`, which this client does not ask for
   * on a search. A sentence that states a total would therefore state
   * one it does not have.
   *
   * KEY: TWO ASSERTIONS WITH DIFFERENT JOBS. The first pins the wording
   * that was ruled. The second is the criterion, and it is what survives
   * a rewording: whatever the sentence becomes, it may not say that
   * something "matches" or was "found", because the count under it is
   * how many arrived.
   */
  it('says how many it is showing, and claims no total', async () => {
    const editor = editorThat('two words', () => undefined);
    const searcher = searcherFor(HITS('w.1', 'w.2', 'w.3'));
    await runSearch(searcher, editor);
    assert.deepStrictEqual(editor.placeholders, ['3 shown for "two words", most relevant first']);
    assert.ok(
      !/match|found|total|\bof\b/i.test(editor.placeholders[0]),
      `the picker states a count as though it were the number that exist: ${editor.placeholders[0]}`
    );
  });

  it('opens nothing when the list is dismissed', async () => {
    const editor = editorThat('two words', () => undefined);
    const searcher = searcherFor(HITS('w.1', 'w.2'));
    const outcome = await runSearch(searcher, editor);
    assert.deepStrictEqual(searcher.queries, ['two words']);
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'cancelled' });
    assert.deepStrictEqual(editor.opened, []);
    /*
     * KEY: A LIST WAS OFFERED, and it held both hits.
     *
     * The cell is named for dismissing a list and never checked that one
     * appeared: measured in a twelfth review round, a product that
     * skipped the picker entirely whenever there were two hits passed
     * it. "It was dismissed" presumes "it was shown".
     */
    assert.strictEqual(editor.offered.length, 1, 'no list was offered, so none was dismissed');
    assert.deepStrictEqual(
      editor.offered[0].map((h) => h.id),
      ['w.1', 'w.2']
    );
    /*
     * AND NOTHING WAS ANNOUNCED. A cancelled search is not a failure, and
     * a failure notice inserted into this branch used to pass.
     */
    assert.deepStrictEqual(editor.said, []);
  });

  /*
   * KEY: NOTHING MATCHED IS SAID OUT LOUD. It used to be possible to
   * write this as "show a list of the hits", and a list of no hits closes
   * itself the instant it opens -- which is exactly what a search the
   * user cancelled looks like. The two outcomes must not share a
   * appearance.
   */
  it('says so when nothing matched, and does not show an empty list', async () => {
    const editor = editorThat('nothinghere');
    const searcher = searcherFor([]);
    const outcome = await runSearch(searcher, editor);
    assert.deepStrictEqual(searcher.queries, ['nothinghere']);
    assert.deepStrictEqual(outcome, {
      did: 'nothing',
      because: 'no-hits',
      query: 'nothinghere'
    });
    assert.deepStrictEqual(editor.offered, []);
    assert.strictEqual(editor.said.length, 1);
    assert.match(editor.said[0], /nothinghere/);
    /*
     * KEY: AND THE SENTENCE SAYS NOTHING MATCHED. Measured in a tenth
     * review round: with only the query checked, changing "nothing in
     * the store matches" to "the store found matches for" passed --
     * the cell was reading the one part of the sentence that cannot be
     * wrong.
     */
    assert.match(
      editor.said[0],
      /nothing in the store matches/,
      `the sentence does not say that nothing matched: ${editor.said[0]}`
    );
    assert.deepStrictEqual(
      editor.levels,
      ['information'],
      'an ordinary answer -- nothing matched -- was raised as an alarm'
    );
    /*
     * KEY: AND NOTHING WAS OPENED. Measured in a ninth review round:
     * putting an `open()` into the branch that found nothing left this
     * cell passing, because it read the outcome and the message and
     * never the one thing that would be in front of the user.
     */
    assert.deepStrictEqual(editor.opened, [], 'a block was opened for a search that found none');
  });

  /*
   * NOTE: A BOX LEFT BLANK IS NOT A SEARCH FOR NOTHING. Sending an empty
   * query to the core answers `(usage (search <query>))` -- a complaint
   * about a command line, shown to somebody who never typed one.
   */
  it('sends neither a dismissed box nor a blank one', async () => {
    let asked = 0;
    const counting = {
      search: async (): Promise<Hit[]> => {
        asked += 1;
        return [];
      }
    };
    const dismissed = editorThat(undefined);
    const blank = editorThat('   ');
    assert.deepStrictEqual(await runSearch(counting, dismissed), {
      did: 'nothing',
      because: 'cancelled'
    });
    assert.deepStrictEqual(await runSearch(counting, blank), {
      did: 'nothing',
      because: 'empty-query'
    });
    assert.strictEqual(asked, 0, 'the store was asked to search for nothing');
    /*
     * KEY: AND NOTHING WAS OPENED EITHER. Measured in a tenth review
     * round: an `open()` added to the cancelled branch, or to the blank
     * branch, passed every cell in this file. "It did not search" and
     * "it did not put a block in front of anybody" are two claims, and
     * only the first was being made.
     */
    assert.deepStrictEqual(dismissed.opened, []);
    assert.deepStrictEqual(blank.opened, []);
    /*
     * KEY: AND NO LIST EITHER. The repair that forbade an open here was
     * made for the open alone; a quick pick inserted into the same
     * branch passed. Measured in an eleventh review round -- the third
     * time this section has been told that "it did not do X" and "it did
     * not do Y" are separate claims.
     */
    assert.deepStrictEqual(dismissed.offered, []);
    assert.deepStrictEqual(blank.offered, []);
    /*
     * KEY: AND NEITHER SAID ANYTHING. A box that was dismissed and a box
     * left blank are both ordinary; a failure notice inserted into
     * either branch passed every cell until a twelfth review round.
     */
    assert.deepStrictEqual(dismissed.said, []);
    assert.deepStrictEqual(blank.said, []);
  });

  it('reports a store it could not reach rather than reporting no hits', async () => {
    const editor = editorThat('anything');
    const asked: string[] = [];
    const outcome = await runSearch(
      {
        search: async (query: string): Promise<Hit[]> => {
          asked.push(query);
          throw new Error('the core would not answer');
        }
      },
      editor
    );
    assert.deepStrictEqual(asked, ['anything'], 'the words the user typed did not reach the store');
    assert.deepStrictEqual(outcome, {
      did: 'failed',
      query: 'anything',
      because: 'the core would not answer'
    });
    assert.strictEqual(editor.opened.length, 0);
    assert.match(editor.said[0], /did not happen/);
    /*
     * KEY: AND THE REASON SURVIVES INTO THE SENTENCE. Measured in a tenth
     * review round: dropping the reason from what is shown, while
     * keeping it in the returned outcome, passed -- so the cell was
     * reading the half of the sentence that is a constant. What a user
     * can act on is the store's own words.
     */
    assert.match(
      editor.said[0],
      /the core would not answer/,
      `the sentence discards the reason: ${editor.said[0]}`
    );
    /*
     * KEY: AND NAMES THE SEARCH IT IS ABOUT. Measured in an eleventh
     * review round: the query in the SHOWN sentence could be replaced
     * while the one sent to the store stayed right, and every cell
     * passed -- the two were checked separately and never together.
     */
    assert.match(editor.said[0], /anything/, `the sentence names another search: ${editor.said[0]}`);
    assert.deepStrictEqual(editor.levels, ['error']);
    assert.deepStrictEqual(editor.offered, [], 'a list was offered for a search that failed');
  });

  /*
   * KEY: AND THE SAME QUESTION PUT TO THE MODEL, WHICH IS WHERE IT WAS
   * WRONG.
   *
   * The cell above drives `runSearch` with a stand-in that throws, so it
   * says what the flow does with a failure and NOTHING about whether the
   * failure is noticed. `StoreModel.search` read the bytes without
   * looking at the exit code, and on the human route a refusal that
   * printed nothing parses to the same empty list as a search that
   * matched nothing.
   */
  it('makes the model refuse an answer whose exit code was not zero', async () => {
    const refusing = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 75,
        stdout: '',
        stderr: 'the client could not reach the store'
      })
    };
    const model = new StoreModel(new Client(refusing));
    await assert.rejects(
      () => model.search('needle'),
      (e: Error) => {
        assert.ok(
          e instanceof TransportError,
          `a refused search came back as ${e.constructor.name}, not as a transport failure`
        );
        return true;
      },
      'a search the store refused was reported as a search with no hits'
    );
  });

  /*
   * KEY: THE SAME QUESTION, PUT TO THE OTHER TWO READERS THAT HAD IT
   * WRONG.
   *
   * Found by a second review round: `roots` parsed `answer.text` and
   * `verbs` parsed `answer.answers[0]` without either of them consulting
   * the exit code. An outline that was refused has empty text, and empty
   * text parses to an outline with no rows -- a store shown as empty at
   * the moment it could not be reached. The catalogue reader has the
   * same shape, and its null is what tells a caller "I could not find
   * out" apart from "the core has no verbs".
   *
   * NOTE: `roots` IS OLDER THAN THIS BATCH. It is repaired here because it
   * is the same defect as the one this batch's own reader had, the
   * instrument to catch it was already in hand, and its reading on the
   * unrepaired tree is in the delivery note.
   */
  it('makes the model refuse an outline whose exit code was not zero', async () => {
    const refusing = {
      kind: 'stand-in',
      send: async (verb: string): Promise<RawResult> => ({
        argv: [],
        rc: verb === 'outline' ? 75 : 0,
        stdout: '',
        stderr: 'the client could not reach the store'
      })
    };
    const model = new StoreModel(new Client(refusing));
    await assert.rejects(
      () => model.roots(),
      (e: Error) => e instanceof TransportError,
      'a refused outline was reported as a store with no blocks in it'
    );
  });

  it('answers that it could not ask when the catalogue request was refused', async () => {
    const asked: string[] = [];
    const refusing = {
      kind: 'stand-in',
      send: async (verb: string): Promise<RawResult> => {
        asked.push(verb);
        return {
          argv: [],
          rc: 75,
          stdout: '(ok (verbs))',
          stderr: 'the client could not reach the store'
        };
      }
    };
    assert.strictEqual(
      await new StoreModel(new Client(refusing)).verbs(),
      null,
      'a refused describe was reported as a core that has no verbs, which would hide every ' +
        'entry that depends on one'
    );
    /*
     * KEY: AND IT ASKED FOR THE CATALOGUE. The repair that made the
     * successful cell watch the verb left this one as it was, because it
     * was made where the finding pointed. Found in a tenth review round.
     */
    assert.deepStrictEqual(asked, ['describe']);
  });

  /*
   * KEY: A SUCCESSFUL READ THAT SAYS NOTHING IS NOT A BLOCK THAT IS NOT
   * THERE EITHER.
   *
   * Found in a seventh review round: the refusal side was split
   * correctly and the SUCCESS side still folded everything into null --
   * exit zero with no output, `(ok)`, `(ok "not a block")` all became
   * "the store has no block ...". The store saying yes and then saying
   * something this build cannot read is the road's problem, not the
   * store's statement that the block is gone.
   */
  it('refuses a read that came back empty or unreadable', async () => {
    const answering = (stdout: string) => ({
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({ argv: [], rc: 0, stdout, stderr: '' })
    });
    /*
     * KEY: EACH ANSWER HAS ITS OWN DESCRIPTION, and the cell requires the
     * one that belongs to it.
     *
     * One alternation covering all three was the first version, and a
     * ninth review round measured what that buys: with both non-empty
     * branches changed to say "nothing at all", every cell still passed
     * -- so `(ok "not a block")` could be described as an empty answer
     * and the diagnostic would be sending its reader to look for a
     * silence that never happened.
     */
    const answers: Array<[string, RegExp]> = [
      ['', /nothing at all/],
      ['(ok)', /holds no block/],
      ['(ok "not a block")', /not a block/]
    ];
    for (const [said, description] of answers) {
      await assert.rejects(
        () => new StoreModel(new Client(answering(said))).blockOf('a.1'),
        (e: Error) => {
          assert.ok(
            e instanceof TransportError,
            `a read answering ${JSON.stringify(said)} came back as ${e.constructor.name}`
          );
          /*
           * KEY: AND THE REFUSAL NAMES THE BLOCK. Measured in an eighth
           * review round: reducing the message to the single word
           * "unreadable" left this passing. Somebody reading it has a
           * whole store to look through, and the id was in the request
           * all along.
           */
          assert.match(e.message, /a\.1/, `the refusal does not name the block: ${e.message}`);
          assert.match(
            e.message,
            description,
            `the refusal does not describe what came back for ${JSON.stringify(said)}: ` +
              e.message
          );
          return true;
        },
        `a read answering ${JSON.stringify(said)} was reported as a block that is not there`
      );
    }
  });

  /*
   * KEY: THE OTHER READ PATH HAD THE SAME COLLAPSE, and the repair to
   * `blockOf` did not reach it.
   *
   * `childrenOf` skipped a record it could not read, so a subtree with
   * one unreadable child came back one short and a subtree of nothing
   * but unreadable records came back EMPTY -- which is what a leaf looks
   * like, and a leaf is what the tree then draws. Measured in a tenth
   * review round with the same `(ok "not a block")` that `blockOf`
   * correctly refused.
   */
  it('refuses a subtree holding a record it cannot read', async () => {
    const answering = {
      kind: 'stand-in',
      send: async (verb: string): Promise<RawResult> => ({
        argv: [],
        rc: 0,
        stdout: verb === 'read' ? '(ok "not a block")' : '',
        stderr: ''
      })
    };
    await assert.rejects(
      () => new StoreModel(new Client(answering)).childrenOf('a.1'),
      (e: Error) => {
        assert.ok(e instanceof TransportError, `came back as ${e.constructor.name}`);
        assert.match(e.message, /a\.1/, `the refusal does not name the block: ${e.message}`);
        assert.match(
          e.message,
          /not a block/,
          `the refusal does not say what came back: ${e.message}`
        );
        return true;
      },
      'a subtree holding an unreadable record was reported as a block with no children'
    );
  });

  /*
   * KEY: AND THE WORDS REACH THE COMMAND LINE.
   *
   * Found in the same round: nothing between `StoreModel.search` and the
   * transport was watched, so mutating `request('search', [query])` to
   * `request('search', ['WRONG QUERY'])` left all of these passing. The
   * doubles above cover the flow's half of the journey; this covers the
   * model's.
   */
  it('sends the words it was given to the store, as one argument', async () => {
    const asked: Array<{ verb: string; args: string[] }> = [];
    const watching = {
      kind: 'stand-in',
      send: async (verb: string, args: string[]): Promise<RawResult> => {
        asked.push({ verb, args });
        /* an empty search as the core prints it under --wire */
        return {
          argv: [],
          rc: 0,
          stdout:
            '(ok (items) (cut (("w0000001" . 4))) (scanned (blocks 5) (fields (title keywords src names doc body)) ' +
            '(unreadable-blocks 0)) (coverage (defs (names 0)) (names-from (datum lexical))) (versions ()))\n',
          stderr: ''
        };
      }
    };
    await new StoreModel(new Client(watching)).search('stale baseline');
    assert.deepStrictEqual(asked, [{ verb: 'search', args: ['stale baseline', '--wire'] }]);
  });

  /*
   * KEY: A SUCCESSFUL SEARCH THROUGH THE MODEL, and it is the control the
   * section was missing.
   *
   * Every cell about a search either went through `StoreModel` and
   * measured a refusal, or went through `runSearch` with a double that
   * is not the model. Measured in an eighth review round: replacing
   * `return rankHits(hits)` with `return []` left all thirty-nine cells
   * passing -- a model that discards every result it is given.
   */
  it('gives back what the store found, best first', async () => {
    const answering = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 0,
        /* two hits as the core prints them under --wire, the lower first */
        stdout:
          '(ok (items (hit "w.9" 2 "b" (fields (src))) (hit "w.2" 7 "a" (fields (title src)))) ' +
          '(cut (("w0000001" . 4))) (scanned (blocks 5) (fields (title keywords src names doc body)) (unreadable-blocks 0)) ' +
          '(versions (("w.9" . "h9") ("w.2" . "h2"))))\n',
        stderr: ''
      })
    };
    assert.deepStrictEqual(await new StoreModel(new Client(answering)).search('two words'), [
      { id: 'w.2', score: 7, note: 'a', fields: ['title', 'src'] },
      { id: 'w.9', score: 2, note: 'b', fields: ['src'] }
    ]);
  });

  /*
   * KEY: A MALFORMED ANSWER THAT SUCCEEDED IS STILL A REFUSAL, asked
   * through the model.
   *
   * `hitsOf` is measured directly for every unreadable shape, and the
   * model's own `hits === null` branch was measured by nothing: changing
   * it to `return []` passed everything, so a store answering rubbish
   * with exit zero would have been drawn as "nothing matched".
   */
  it('refuses a search answered with something that is not a hit', async () => {
    const answering = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 0,
        stdout: '(miss "a.1" 6 "x")',
        stderr: ''
      })
    };
    await assert.rejects(
      () => new StoreModel(new Client(answering)).search('anything'),
      (e: Error) => e instanceof TransportError,
      'a search answered with an unreadable form was reported as a search with no hits'
    );
  });

  /*
   * KEY: AND A CATALOGUE THAT WAS READ. The only cell that went through
   * `StoreModel.verbs` expected null for a refusal, and the positive
   * cells call `knownVerbs` directly -- so `verbs` returning null
   * unconditionally passed everything. Measured in the same round. A
   * model that always answers "I could not find out" hides every entry
   * that depends on a verb.
   */
  it('reports the verbs a catalogue named, and asks describe for it', async () => {
    const asked: string[] = [];
    const answering = {
      kind: 'stand-in',
      send: async (verb: string): Promise<RawResult> => {
        asked.push(verb);
        return {
          argv: [],
          rc: 0,
          stdout: '(ok (verbs (init (usage (init))) (search (usage (search <query>)))))',
          stderr: ''
        };
      }
    };
    const verbs = await new StoreModel(new Client(answering)).verbs();
    /*
     * KEY: THE VERB IS WATCHED. Measured in a ninth review round:
     * `request('describe', [])` changed to `request('search', [])` left
     * both catalogue cells passing, because neither double looked at
     * what it was asked.
     */
    assert.deepStrictEqual(asked, ['describe']);
    assert.notStrictEqual(verbs, null, 'a catalogue that was read was reported as unavailable');
    assert.deepStrictEqual([...(verbs as Set<string>)].sort(), ['init', 'search']);
  });

  /*
   * KEY: AN OCCUPIED SOCKET PATH IS WAITED OUT, NOT PARKED AT ONCE AND NOT
   * LEFT UNRECOGNISED.
   *
   * What occupies it is another daemon, starting or draining, so the
   * next attempt either connects or starts one. Five attempts and still
   * occupied is a stuck path and the cap parks it for a person. Ruled by
   * the main session after a review round asked what happened to this
   * name, which until then was in no table at all.
   */
  /*
   * NOTE: ON f5ebd58 A FAILED START ALWAYS ANSWERS `serve-start-failed`, with
   * a `kind` (client.sc, `start-one!` and `exited-answer`): the head of the
   * daemon's own startup report for this start's `--attempt` token -- an
   * occupied path is `(kind serve-path-occupied)` -- or `exited` (no report
   * for the token) or `timeout` (the daemon alive at the budget with no
   * socket). The occupied path is therefore retried through
   * `serve-start-failed`; the older head stays in the table for a core that
   * still relays it.
   */
  it('counts an occupied socket path among the refusals worth trying again', function () {
    assert.ok(
      (RETRYABLE_REFUSALS as readonly string[]).includes('serve-start-failed'),
      'a failed start, which is how an occupied path arrives, is not tried again'
    );
    assert.ok(
      (RETRYABLE_REFUSALS as readonly string[]).includes('serve-path-occupied'),
      'an occupied socket path is treated as a name this build has never heard of'
    );
    assert.strictEqual(
      Object.prototype.hasOwnProperty.call(SETTINGS_REFUSALS, 'serve-path-occupied'),
      false,
      'an occupied socket path was parked at once, although waiting is what clears it'
    );
  });

  /*
   * KEY: "THE BLOCK IS NOT THERE" IS THE STORE'S STATEMENT; "I COULD NOT
   * LOOK" IS THE ROAD'S.
   *
   * Found in a third review round and ruled in a fifth: `blockOf`
   * returned null for every refusal, and `src/extension.ts` turns null
   * into "the store has no block ...". Told that, a person deletes the
   * reference. Only `(error unknown-id ...)` -- which IS a non-zero exit,
   * so the exit code alone cannot decide this -- means the block is not
   * there; every other refusal is relayed with what the store said.
   */
  it('reports a block that is not there as absent', async () => {
    const absent = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 1,
        stdout: '(error unknown-id "a.9" (nearest ("a.1")))',
        stderr: ''
      })
    };
    assert.strictEqual(await new StoreModel(new Client(absent)).blockOf('a.9'), null);
  });

  it('refuses a read it could not make, rather than calling the block absent', async () => {
    const blocked = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 75,
        /*
         * The shape a failed start has on f5ebd58 (see the note above
         * "counts an occupied socket path ...").
         */
        stdout: '(error serve-start-failed (kind serve-path-occupied) (path "/blocked") (attempt "0000abcd00000001") (exit 1))',
        stderr: ''
      })
    };
    await assert.rejects(
      () => new StoreModel(new Client(blocked)).blockOf('a.1'),
      (e: Error) => {
        assert.ok(e instanceof TransportError, `came back as ${e.constructor.name}`);
        assert.match(
          e.message,
          /serve-path-occupied/,
          `the refusal does not carry what the store said: ${e.message}`
        );
        return true;
      },
      'a read that could not be made was reported as a block that is not there'
    );
  });

  /*
   * KEY: AND THE INTERPRETER THE USER CHOSE REACHES THE DAEMON.
   *
   * Found by a second review round. `theourgia.scheme` is used to start
   * the thin client, and the client starts the daemon with
   * `THEOURGIA_SCHEME` or, failing that, whatever `scheme` resolves to
   * on PATH (`scheme-binary`, theourgia.sc:156, which `server-argv` puts at
   * the head of the daemon's argument list, :392-396; the in-process route
   * reads the same variable, core.sc:642-643). A user who set the
   * setting because `scheme` is not on their PATH got a client from the
   * path they gave and a daemon that could not be started at all -- and
   * the cells never met it, because the fixtures take the setting FROM
   * that variable, so it was always already in the environment.
   */
  it('puts the chosen interpreter in the environment, for the daemon the client starts', function () {
    const built = environmentFor(
      {
        scheme: '/opt/chez/bin/scheme',
        corePath: '/core',
        libDirs: [],
        store: '/store',
        actor: 'a',
        timeoutMs: 1000
      },
      {},
      { has: () => true }
    );
    assert.strictEqual(built.THEOURGIA_SCHEME, '/opt/chez/bin/scheme');
  });

  /*
   * KEY: THE DIRECTORY IS OPTIONAL IN THE SIGNATURE AND REQUIRED IN
   * PRACTICE.
   *
   * Found in a third review round. `problemsWith` defaults its second
   * argument to null and skips the "neither sources nor products"
   * problem when it is null -- and the extension called it with one
   * argument, so the refusal this batch added could never be shown to
   * anybody. The default is what made it possible, so the default goes.
   */
  it('cannot be asked about the settings without being given the directory', function () {
    const config: CoreConfig = {
      scheme: 'scheme',
      corePath: '/missing-core',
      libDirs: [],
      store: '/store',
      actor: 'a',
      timeoutMs: 1000
    };
    const found = problemsWith(config, { has: () => false });
    assert.ok(
      found.some((p) => p.setting === 'theourgia.corePath' && /neither/.test(p.message)),
      `a directory holding neither sources nor products was accepted: ${JSON.stringify(found)}`
    );
    assert.deepStrictEqual(
      problemsWith(config, { has: (name: string) => name === WITNESS_SOURCE }),
      [],
      'a directory holding sources was refused'
    );
  });

  /*
   * KEY: A REFUSAL WITH NOTHING ON STDOUT IS NOT AN EMPTY RESULT.
   *
   * Found by an outside review and reproduced: on the human route no
   * hits IS no output, so a refused request whose stdout is empty parses
   * to the same empty list -- and the user was told "nothing in the
   * store matches", about a search that never happened. The exit code is
   * the verdict, and it has to be read before the bytes are.
   */
  it('reports a refused search as a failure, not as nothing matching', async () => {
    const editor = editorThat('needle');
    const asked: string[] = [];
    const outcome = await runSearch(
      {
        search: async (query: string): Promise<Hit[]> => {
          asked.push(query);
          throw new TransportError('unreadable', 'the store did not answer the search: ');
        }
      },
      editor
    );
    assert.deepStrictEqual(asked, ['needle']);
    assert.strictEqual((outcome as { did: string }).did, 'failed');
    assert.doesNotMatch(editor.said[0], /nothing in the store matches/);
    assert.match(
      editor.said[0],
      /the store did not answer the search/,
      `the sentence discards the reason: ${editor.said[0]}`
    );
    assert.match(editor.said[0], /needle/, `the sentence names another search: ${editor.said[0]}`);
    assert.deepStrictEqual(editor.levels, ['error']);
    assert.deepStrictEqual(editor.opened, []);
    assert.deepStrictEqual(editor.offered, []);
  });

  it('says there is no store rather than opening a box nobody can answer', async () => {
    const editor = editorThat('anything');
    assert.deepStrictEqual(await runSearch(null, editor), {
      did: 'nothing',
      because: 'no-store'
    });
    assert.strictEqual(editor.said.length, 1);
    /*
     * KEY: AND THE SENTENCE SAYS WHAT IS WRONG. Measured in an eleventh
     * review round: replacing it with "The store is ready to search."
     * passed -- the cell counted the notices and read none of them.
     */
    assert.match(
      editor.said[0],
      /no store to search/,
      `the sentence does not say there is no store: ${editor.said[0]}`
    );
    assert.deepStrictEqual(
      editor.levels,
      ['error'],
      'a store that cannot be searched was reported as a quiet information notice'
    );
    /*
     * KEY: AND NOTHING WAS PUT IN FRONT OF ANYBODY -- no box, no list, no
     * block. The cell is named for the first and asserted none of them:
     * measured in two review rounds, a prompt added to this branch
     * passed, and so did a quick pick, and so did an open. Asking
     * somebody to choose a block from a store that is not configured is
     * asking a question whose answer has nowhere to go.
     */
    assert.deepStrictEqual(
      editor.asked,
      [],
      'a search box was opened although there is no store to search'
    );
    assert.deepStrictEqual(
      editor.offered,
      [],
      'a list was offered although there is no store to search'
    );
    assert.deepStrictEqual(editor.opened, []);
  });
});

/*
 * plugin-r2 T2, resettled: the clause the human rendering drops.
 *
 * MEASURED, BOTH WAYS, AGAINST A CORE PINNED BEFORE F100b (877f0da OR EARLIER):
 *
 *   $ theourgia commit <id> --working-version <id>=<v>
 *   (ok (events (("w" . 5))) (state (("w.1" . "fb26...")))
 *       (cursor ("w" . 5)) (replay #f))
 *
 *   $ theourgia commit <id> --working-version <id>=<v> --wire
 *   (ok (items (ok (events (("w" . 7))) (state (("w.1" . "5184...")))
 *                  (cursor ("w" . 7)) (replay #f)))
 *       (behind (("w" . 7))))
 *
 * Two facts follow, and both are why this is not simply a flag.
 *
 * NOTE: THE ANSWER THIS CLIENT ALREADY READS MOVES DOWN A LEVEL. The
 * cursor, the events and the replay flag every caller looks at are now
 * inside `items`. So `answers` goes on holding exactly what it held
 * before -- the item -- and the outer form arrives as `envelope`, beside
 * it. Nothing that reads an answer today has to change.
 *
 * NOTE: AND THE STORE NAMES US IN OUR OWN `behind`. `(behind (("w" . 7)))`
 * on a commit whose own cursor is `("w" . 7)` is this very commit's
 * record, reported as somebody else's. That is a defect in the core and
 * it is being fixed there; until then -- and harmlessly afterwards -- the
 * writer this answer's own cursor names is dropped, which is a thing the
 * answer says about itself rather than a list of names kept anywhere.
 */
describe('plugin-r2 T2 the envelope --wire puts round a commit', function () {
  before(async () => {
    await initWire();
  });

  const WIRE =
    '(ok (items (ok (events (("w" . 7))) (state (("w.1" . "5184"))) (cursor ("w" . 7))' +
    ' (replay #f))) (behind (("w" . 7) ("other" . 2))))';

  it('hands the item on as the answer, so nothing that reads one has to change', function () {
    const answer = interpret(
      { argv: [], rc: 0, stdout: WIRE, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    assert.strictEqual(answer.answers.length, 1);
    /*
     * KEY: THE EXPECTED VALUE IS WRITTEN DOWN, not read back through the
     * reader under test. Measured in a fourteenth review round: with
     * `Form.value` answering undefined for everything, the two sides of
     * this comparison became `undefined === undefined` and the cell
     * passed -- it was asking the decoder to agree with itself.
     */
    assert.strictEqual(
      (answerOf(answer.answers[0], 'ok')?.value('replay') as { value?: unknown })?.value,
      false,
      'the item did not come out of the envelope with its replay flag'
    );
    /*
     * NEVER: `clause(...) !== null` IS TRUE OF EVERY ANSWER NOW.
     *
     * When the decoder began answering `{read, because}` instead of a
     * nullable list, three assertions in this file became `an object is
     * not null`. Measured in an eighteenth review round: making
     * `clause('cursor')` answer `{read: false, because: 'absent'}`
     * passed this whole cell. The contents are asserted.
     */
    assert.deepStrictEqual(
      answerOf(answer.answers[0], 'ok')?.clause('cursor'),
      { read: true, items: parseAnswers('(("w" . 7))')[0] },
      'the cursor did not come out of the envelope'
    );
  });

  it('keeps the outer form, which is the only place the behind clause is', function () {
    const answer = interpret(
      { argv: [], rc: 0, stdout: WIRE, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    assert.notStrictEqual(answer.envelope, null);
    /*
     * AND WHAT IS IN IT, for the reason above: the envelope existing
     * says nothing about the clause this cell is named for.
     */
    assert.deepStrictEqual(
      answerOf(answer.envelope, 'ok')?.clause('behind'),
      { read: true, items: parseAnswers('((("w" . 7) ("other" . 2)))')[0] },
      'the behind clause did not survive, and the outer form is the only place it is'
    );
  });

  /*
   * KEY: AND AN ANSWER THAT WAS NOT WRAPPED IS LEFT ALONE. Every other
   * verb is still asked for in the mode it was always asked for in, and
   * an unwrapping that fired on the shape rather than on the request
   * would reach into the one verb whose own answer happens to be
   * `(ok (items ...))`.
   */
  it('leaves an answer that came back unwrapped exactly as it was', function () {
    const plain = '(ok (events (("w" . 5))) (cursor ("w" . 5)) (replay #f))';
    const answer = interpret({ argv: [], rc: 0, stdout: plain, stderr: '' }, 'commit', 'datum', []);
    assert.strictEqual(answer.answers.length, 1);
    assert.strictEqual(answer.envelope, null);
    /*
     * EXACTLY AS IT WAS means the datum, not its length. Measured in an
     * eighteenth review round: replacing the answers with `(ok)` passed
     * this cell, because one is still one and an absent clause is still
     * an object.
     */
    assert.deepStrictEqual(
      answer.answers[0],
      parseAnswers(plain)[0],
      'an answer that was never wrapped did not come back as it went in'
    );
  });

  /*
   * KEY: THE CONTROL HAS TO LOOK LIKE AN ENVELOPE, or it is not a control
   * at all.
   *
   * Found in a fourth review round, measured: with the case above as the
   * only control, removing `args.includes('--wire')` from `interpret` --
   * making the unwrapping turn on the SHAPE of the reply -- left every
   * cell in this file passing, because the reply it uses has no `items`
   * clause to be mistaken for one. `tag` with no argument answers
   * exactly the shape that gets mistaken, and this extension asks for it
   * without `--wire`.
   */
  it('does not unwrap a verb whose own answer is an items form', function () {
    const listed = '(ok (items (tag "release") (tag "beta")))';
    const answer = interpret(
      { argv: [], rc: 0, stdout: listed, stderr: '' },
      'tag',
      'items',
      []
    );
    assert.strictEqual(
      answer.envelope,
      null,
      'an ordinary answer was taken for an envelope, so the unwrapping is reading the shape'
    );
    assert.strictEqual(
      answer.answers.length,
      1,
      'the items clause was unwrapped, so this verb lost the form its caller reads'
    );
  });

  it('drops the writer this very commit advanced, and keeps the others', function () {
    const answer = interpret(
      { argv: [], rc: 0, stdout: WIRE, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    const notice = behindNotice(answer.envelope, answer.answers[0]);
    assert.notStrictEqual(notice, null);
    assert.strictEqual((notice as string).includes('other'), true);
    assert.strictEqual(
      (notice as string).includes('w has'),
      false,
      'the notice names the writer whose record this commit had just appended'
    );
  });

  /*
   * NOTE: AND WHEN THE ONLY NAME IN IT IS OURS THERE IS NOTHING TO SAY.
   * This is what every ordinary save on a one-writer store looks like
   * today, and a notice on every one of those would be noise that also
   * happens to be untrue.
   */
  it('says nothing when the only writer named is the one that just committed', function () {
    const onlyUs =
      '(ok (items (ok (cursor ("w" . 7)) (replay #f))) (behind (("w" . 7))))';
    const answer = interpret(
      { argv: [], rc: 0, stdout: onlyUs, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    assert.strictEqual(behindNotice(answer.envelope, answer.answers[0]), null);
  });
});

/*
 * plugin-r2 T1: what the status bar says when the store cannot be
 * reached.
 *
 * KEY: A QUESTION MARK IS NOT SOMETHING ANYBODY CAN ACT ON. The tooltip
 * said "conflicts: unknown, the store could not be asked" and stopped
 * there, and what had been thrown away was the core's own sentence.
 * Measured against the F46 core (877f0da), with a directory sitting where
 * the daemon's socket goes:
 *
 *   $ theourgia outline --store <s>
 *   (error serve-path-occupied (path "/tmp/vsc-t6/fb4eecc43a207f1b/socket"))
 *   rc=75
 *
 * On f5ebd58 a failed start answers `serve-start-failed` with a `kind` --
 * the daemon's report head (`serve-path-occupied` here), `exited` or
 * `timeout` -- and the report's clauses after it (read in client.sc, not yet
 * measured here); the path is still in it. That names a directory a person
 * can remove.
 */
describe('plugin-r2 T1 the status bar carries the words the core used', function () {
  const base = {
    store: '/tmp/s',
    actor: 'a',
    cursor: null,
    conflicts: null,
    pending: 0,
    blocked: null,
    unreachable: null
  };

  it('puts the reason in the tooltip, unaltered', function () {
    const line = statusLine({
      ...base,
      unreachable:
        'the core refused: (error serve-start-failed (kind serve-path-occupied) (path "/tmp/r/k/socket") (attempt "0000abcd00000001") (exit 1))'
    });
    assert.match(line.tooltip, /serve-path-occupied/);
    assert.match(line.tooltip, /\/tmp\/r\/k\/socket/);
    assert.strictEqual(line.warning, true);
  });

  /*
   * THE TWIN. A store that answers says nothing about being unreachable,
   * and a tooltip that carried a stale sentence would report a fault
   * that had been fixed -- the same defect the other way round.
   */
  it('says nothing about reachability when the last asking worked', function () {
    const line = statusLine({ ...base, conflicts: 0 });
    assert.doesNotMatch(line.tooltip, /could not be reached/);
    assert.strictEqual(line.warning, false);
  });

  /*
   * NOTE: AND IT IS ITS OWN FACT, not a second reading of the conflict
   * count. `conflicts: null` means "I could not ask" and says nothing
   * about why; the two are reported together here, and a build that
   * derived one from the other would have nothing left to put in the
   * sentence.
   */
  it('reports the count as unknown and the reason beside it', function () {
    const line = statusLine({ ...base, unreachable: 'connect-failed' });
    assert.match(line.tooltip, /conflicts: unknown/);
    assert.match(line.tooltip, /could not be reached: connect-failed/);
  });
});

/*
 * plugin-r2: the enclosing form, in every reader that takes a clause out
 * of one.
 *
 * KEY: THIS IS ONE DEFECT FOUND FIVE TIMES, and each time it was repaired
 * where the finding pointed. `hitsOf` in round one, `knownVerbs` in
 * round ten, and in round eleven three more: the envelope reader in
 * `client.ts`, `blockOf`, and `Working.read`. The cells are together so
 * that the sixth is an obvious omission rather than a discovery.
 *
 * What the shape is: a clause is taken out of a form without asking what
 * the form SAID. A refusal carrying a clause of the right name, or a
 * form nobody can parse, is then read as an answer.
 */
describe('plugin-r2 a clause is only an answer when the form said ok', function () {
  before(async () => {
    await initWire();
  });

  it('does not unwrap an envelope from a form that did not say ok', function () {
    const crafted = '(garbage (items (ok (cursor ("w" . 7)) (replay #f))))';
    const answer = interpret(
      { argv: [], rc: 0, stdout: crafted, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    assert.strictEqual(answer.envelope, null, 'a form nobody can parse was taken for an envelope');
    assert.strictEqual(eventFromWrite(answer), null, 'a cursor was read out of it');
  });

  /*
   * KEY: AND THE CURSOR IS NOT UNWRAPPED A SECOND TIME.
   *
   * `interpret` removes the envelope the REQUEST asked for. The cursor
   * readers used to reach into an `items` clause again, by shape, so a
   * nested form supplied the cursor and the replay flag instead of the
   * answer's own. Measured in an eleventh review round with the datum
   * below: the cursor came back as `wrong:99` and the save was reported
   * as a replay.
   */
  it('reads the cursor the answer states, not one nested inside it', function () {
    const nested =
      '(ok (items (ok (items (ok (cursor ("wrong" . 99)) (replay #t)))' +
      ' (cursor ("right" . 7)) (replay #f))))';
    const answer = interpret(
      { argv: [], rc: 0, stdout: nested, stderr: '' },
      'commit',
      'datum',
      ['--wire']
    );
    const event = eventFromWrite(answer);
    assert.notStrictEqual(event, null);
    assert.strictEqual((event as { writer: string }).writer, 'right');
    assert.strictEqual((event as { seq: number }).seq, 7);
    assert.strictEqual(isReplay(answer), false, 'a fresh save was reported as one already applied');
  });

  it('does not read a block out of a form that did not say ok', async () => {
    const crafted =
      '(garbage ((id . "a.1") (deleted . #f) (fields (title . "Doc")) (position root . 0) (edges)))';
    const answering = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({ argv: [], rc: 0, stdout: crafted, stderr: '' })
    };
    await assert.rejects(
      () => new StoreModel(new Client(answering)).blockOf('a.1'),
      (e: Error) => e instanceof TransportError,
      'a block was read out of a form nobody can parse'
    );
  });

  it('does not read a working projection out of a form that did not say ok', async () => {
    const crafted = '(garbage (projection working "draft" "a.1" "v1" "base" () "body" "prefix"))';
    const answering = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({ argv: [], rc: 0, stdout: crafted, stderr: '' })
    };
    await assert.rejects(
      () => new Working(new Client(answering), 'draft').read('a.1', 'prefix'),
      (e: Error) => /could not be verified/.test(e.message),
      'a working projection that looked verified was read out of a form nobody can parse'
    );
  });

  /*
   * KEY: AND AN EMPTY SUBTREE IS NOT A LEAF. `read --recursive` includes
   * the block itself -- the core's `subtree-ids` is `(cons id ...)` --
   * so a success carrying nothing is an answer this build cannot account
   * for. The repair that made the loop refuse an unreadable record left
   * this outside it: an empty list does not enter a loop.
   */
  it('refuses a subtree read that came back empty', async () => {
    const answering = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({ argv: [], rc: 0, stdout: '', stderr: '' })
    };
    await assert.rejects(
      () => new StoreModel(new Client(answering)).childrenOf('a.1'),
      (e: Error) => {
        assert.ok(e instanceof TransportError);
        assert.match(e.message, /a\.1/);
        assert.match(e.message, /nothing at all/);
        return true;
      },
      'an empty subtree answer was drawn as a block with no children'
    );
  });
});

/*
 * plugin-r2: how many writers a store has, when one of them cannot be
 * read.
 *
 * KEY: THE COUNT IS THE WHOLE DECISION HERE. One writer lets this client
 * pick a cursor and write; more than one is refused, because the core
 * does not say which is local. Skipping an entry nobody could parse made
 * a two-writer listing count as one -- so the refusal that exists
 * precisely to avoid guessing was defeated by a value nobody understood.
 */
describe('plugin-r2 an unreadable writer listing is not a shorter one', function () {
  before(async () => {
    await initWire();
  });

  it('refuses a listing holding an entry it cannot read', function () {
    const said = datum('(check (writers (("local" (end 7)) ("other" (end "bad")))))');
    const first = firstCursorFromCheck(said);
    assert.strictEqual(first.ok, false, 'a two-writer listing was read as one writer');
    assert.strictEqual((first as { reason: string }).reason, 'unreadable');
  });

  /*
   * THE TWIN: a listing this build CAN read, with one writer in it, is
   * exactly what does give a cursor. Without this the cell above would
   * pass on a reader that refuses everything.
   */
  it('still gives a cursor for a listing it can read', function () {
    const said = datum('(check (writers (("local" (end 7)))))');
    const first = firstCursorFromCheck(said);
    assert.strictEqual(first.ok, true);
    assert.strictEqual((first as { cursor: string }).cursor, 'local:7');
  });

  /*
   * KEY: AND THE SENTENCE A PERSON SEES COMES FROM THE SAVER, which the
   * cells above never reach. Measured in a twelfth review round:
   * replacing the new sentence with the no-writer one left every
   * assertion here holding, because they read a reason code and not the
   * words. "This store has no writer" and "I could not read the listing"
   * send a reader to two different places.
   */
  it('tells a person which of the two happened', async function () {
    const answering = (stdout: string) => ({
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({ argv: [], rc: 0, stdout, stderr: '' })
    });
    const blockedBy = async (stdout: string): Promise<string> => {
      const queue = new Outbox(path.join(os.tmpdir(), `tv-writers-${process.pid}-${counter()}.json`));
      const saver = new Saver(new Client(answering(stdout)), queue, () => undefined, IGNORED_DURABILITY);
      const outcome = await saver.save('a.1', 'src', 'body');
      assert.strictEqual(outcome.status, 'blocked', outcome.message);
      return outcome.message;
    };
    assert.match(
      await blockedBy('(check (writers (("local" (end 7)) ("other" (end "bad")))))'),
      /could not read/,
      'an unreadable writer listing was reported as a store with no writer'
    );
    assert.match(
      await blockedBy('(check (writers ()))'),
      /no writer/,
      'a store that reports no writer was reported as an unreadable listing'
    );
  });

  it('still refuses a listing with two writers it can read', function () {
    const said = datum('(check (writers (("local" (end 7)) ("other" (end 2)))))');
    const first = firstCursorFromCheck(said);
    assert.strictEqual(first.ok, false);
    assert.strictEqual((first as { reason: string }).reason, 'many-writers');
  });
});

/*
 * plugin-r2: the shapes this delivery repaired, searched for once more.
 *
 * KEY: EVERY CELL HERE EXISTS BECAUSE A REPAIR REACHED ONE PLACE AND THE
 * DEFECT WAS IN SEVERAL. They are together so that the next instance is
 * an obvious omission. Two shapes:
 *
 *   a clause taken out of a form without asking what the form SAID;
 *   an answer accepted without asking whether it is about what was asked.
 */
describe('plugin-r2 an answer is about what was asked, or it is refused', function () {
  before(async () => {
    await initWire();
  });

  const answering = (stdout: string, rc = 0) => ({
    kind: 'stand-in',
    send: async (): Promise<RawResult> => ({ argv: [], rc, stdout, stderr: '' })
  });

  /*
   * KEY: THE WORST ONE, AND THE LAST FOUND. A cursor read out of a form
   * nobody can parse is a save reported as landed and its record removed
   * from the queue. The exit code was being trusted for a question it
   * cannot answer.
   */
  it('takes no cursor and no replay from a form that did not say ok', function () {
    const answer = interpret(
      { argv: [], rc: 0, stdout: '(garbage (cursor ("w" . 7)) (replay #t))', stderr: '' },
      'commit',
      'datum',
      []
    );
    assert.strictEqual(eventFromWrite(answer), null, 'a cursor was taken out of it');
    assert.strictEqual(isReplay(answer), false, 'a replay was read out of it');
  });

  it('takes no first cursor from a form that is not a check', function () {
    for (const said of ['(garbage (writers (("local" (end 7)))))', '(error (writers (("local" (end 7)))))']) {
      const first = firstCursorFromCheck(datum(said));
      assert.strictEqual(first.ok, false, `a cursor was taken out of ${said}`);
    }
  });

  it('takes no behind clause from a form that did not say ok', function () {
    assert.strictEqual(behindNotice(datum('(garbage (behind (("other" . 2))))')), null);
  });

  /*
   * KEY: AND THE NOTICE READS A CURSOR THE WAY THE REST OF THIS BUILD
   * DOES. `readEvent` accepts `("w" 7)` as well as `("w" . 7)`; the
   * notice had its own narrower reader, so on that shape the writer went
   * unread -- and an unread writer is one that is not dropped. The save
   * then named itself.
   */
  it('drops the writer this commit advanced however the cursor is spelled', function () {
    const wire =
      '(ok (items (ok (cursor ("w" 7)) (replay #f))) (behind (("w" . 7) ("other" . 2))))';
    const answer = interpret({ argv: [], rc: 0, stdout: wire, stderr: '' }, 'commit', 'datum', [
      '--wire'
    ]);
    const notice = behindNotice(answer.envelope, answer.answers[0]);
    assert.notStrictEqual(notice, null);
    assert.ok((notice as string).includes('other'));
    assert.ok(
      !/\bw has\b/.test(notice as string),
      `the notice names the writer this commit advanced: ${notice}`
    );
  });

  it('refuses a block record that is not the block that was asked for', async () => {
    const other =
      '(ok ((id . "b.1") (deleted . #f) (fields (src . "other body")) (position root . 0) (edges)))';
    await assert.rejects(
      () => new StoreModel(new Client(answering(other))).blockOf('a.1'),
      (e: Error) => {
        assert.ok(e instanceof TransportError);
        assert.match(e.message, /b\.1/, `the refusal does not say whose record came back: ${e.message}`);
        return true;
      },
      "another block's record was returned for the one that was asked for"
    );
  });

  it('refuses a subtree that never mentions the block it is under', async () => {
    const elsewhere =
      '((id . "b.1") (deleted . #f) (fields (src . "x")) (position root . 0) (edges))';
    await assert.rejects(
      () => new StoreModel(new Client(answering(elsewhere))).childrenOf('a.1'),
      (e: Error) => {
        assert.ok(e instanceof TransportError);
        assert.match(e.message, /without mentioning a\.1/);
        return true;
      },
      'a subtree answer about another block was drawn as a leaf'
    );
  });

  /*
   * KEY: THE MARKS ARE THE THIRD STATE'S EVIDENCE. `marksKnown` exists so
   * that "I could not ask" is not drawn as "nothing is wrong"; skipping
   * an entry this build recognises and cannot read threw that evidence
   * away while leaving `marksKnown` true.
   */
  /*
   * KEY: EVERY HEAD THE REPAIR NAMED, not the one the finding used.
   *
   * Measured in a thirteenth review round: narrowing STRUCTURAL_HEADS to
   * `['orphan']` alone left every cell passing, because only `orphan`
   * was exercised. A list in the product needs a cell per entry, or the
   * entries nobody exercises are decoration.
   */
  for (const head of ['orphan', 'nested-document', 'conflict']) {
    it(`refuses a ${head} mark whose block cannot be read`, async () => {
      await assert.rejects(
        () => new StoreModel(new Client(answering(`(${head} 5)`))).structuralMarks(),
        (e: Error) => e instanceof TransportError,
        'a mark naming no readable block was dropped and the store drawn as sound'
      );
    });
  }

  /*
   * KEY: AND AN INCOMPLETE READING REACHES THE CALLER AS AN UNKNOWN ONE.
   *
   * `structuralMarks` reporting `complete: false` is only worth
   * anything if somebody acts on it. Measured: making `childrenOf` use
   * the marks regardless left every cell in this tree passing -- the
   * new third-state evidence was being produced and thrown away one
   * layer down, which is the same defect one step further in.
   */
  it('reports a subtree as having unknown marks when the marks were not all read', async () => {
    const answering = (verb: string): RawResult => ({
      argv: [],
      rc: 0,
      stdout:
        verb === 'conflicts'
          ? '(orphan)\n'
          : '((id . "a.1") (deleted . #f) (fields (src . "b")) (position root . 0) (edges))\n',
      stderr: ''
    });
    const listing = await new StoreModel(
      new Client({ kind: 'stand-in', send: async (verb: string) => answering(verb) })
    ).childrenOf('a.1');
    assert.strictEqual(
      listing.marksKnown,
      false,
      'a subtree whose marks could not all be read was reported as having known marks'
    );
  });

  /*
   * THE TWIN, and it is what stops the cell above from being a rule that
   * refuses everything: `conflicts` carries other kinds of report, and a
   * client that refused each one it did not recognise would stop working
   * the day the core says something new.
   */
  it('still passes over a report it does not recognise', async () => {
    const others = '(pending (event "w" 6) (missing "v" 2))\n(something-later "a.1" whatever)\n';
    const read = await new StoreModel(new Client(answering(others))).structuralMarks();
    assert.strictEqual(read.marks.size, 0);
    /*
     * KEY: AND THE READING IS STILL COMPLETE, which is the half this
     * control was missing.
     *
     * A report this build does not recognise is a future core saying
     * something new, not a mark that was lost -- and `complete: false`
     * is what makes the tree tell the user it could not say. Measured in
     * a sixteenth review round: setting `complete = false` on the
     * unknown-head branch passed this cell, which asked only for the
     * size of the map. Every store on a newer core would then have
     * reported its marks unknown, for ever.
     */
    assert.strictEqual(
      read.complete,
      true,
      'a report this build does not recognise made the whole reading incomplete, so a newer core ' +
        'would have every block drawn as one whose marks could not be read'
    );
  });
});

/*
 * plugin-r2: the refusal side, and the readers that had grown beside an
 * authority.
 *
 * KEY: THESE ARE THE OTHER FACE OF DEFECTS ALREADY REPAIRED ON THE `ok`
 * SIDE. An unparseable answer used to confirm a save; an unparseable
 * REFUSAL used to turn one down definitively and take it out of the
 * queue. One coin.
 */
describe('plugin-r2 a refusal this build cannot read is not a refusal', function () {
  before(async () => {
    await initWire();
  });

  it('does not settle a save on a refusal whose name cannot be read', function () {
    const said = datum('(error ("unknown"))');
    assert.throws(
      () => classifyRefusal(said),
      /cannot be read/,
      'a save was classified from a refusal nobody can name, and an unrecognised refusal is ' +
        'settled as refused -- which takes the request out of the queue'
    );
  });

  /*
   * THE TWIN: a refusal this build CAN name is classified as before.
   * Without it the cell above would pass on a classifier that throws at
   * everything.
   */
  it('still classifies a refusal it can name', function () {
    assert.deepStrictEqual(classifyRefusal(datum('(error stale-baseline)')), {
      verdict: 'refused'
    });
  });

  it('does not read another block\'s unknown-id as this block being absent', async () => {
    const aboutAnother = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 1,
        stdout: '(error unknown-id "b.1" (nearest ()))',
        stderr: ''
      })
    };
    await assert.rejects(
      () => new StoreModel(new Client(aboutAnother)).blockOf('a.1'),
      (e: Error) => e instanceof TransportError,
      'a statement about b.1 was read as "a.1 is not there"'
    );
  });

  it('still reads this block\'s own unknown-id as absence', async () => {
    const aboutThisOne = {
      kind: 'stand-in',
      send: async (): Promise<RawResult> => ({
        argv: [],
        rc: 1,
        stdout: '(error unknown-id "a.1" (nearest ()))',
        stderr: ''
      })
    };
    assert.strictEqual(await new StoreModel(new Client(aboutThisOne)).blockOf('a.1'), null);
  });

  /*
   * KEY: AND THE SIDECAR'S OUTSTANDING RECORDS ARE READ OR REFUSED, never
   * dropped. An empty outstanding list is what `cleanliness` reads as
   * "nothing is in flight over this file", so a record it could not read
   * used to make a file with unsent work in it report clean.
   */
  const sidecarWith = (outstanding: unknown): string =>
    JSON.stringify({
      format: 1,
      'store-id': 's',
      'block-id': 'a.1',
      prefix: '# A\n',
      written: 'w',
      phase: 'published',
      nextSeq: 2,
      outstanding
    });

  it('refuses a sidecar holding an outstanding send it cannot read', function () {
    const read = sidecarFromDisk(sidecarWith([{ seq: 'bad', req: 'R1' }]));
    assert.strictEqual(
      read.read,
      false,
      'an unreadable outstanding send was dropped, and an empty list is what reads as clean'
    );
    assert.strictEqual((read as { because: string }).because, 'unreadable');
  });

  /*
   * KEY: AND SO IS THE LIST ITSELF. The repair above reads the ENTRIES;
   * a sixteenth review round measured the container four lines from it.
   * `outstanding: {seq: 1, req: "R"}` is not an array, read as an empty
   * list, and an empty list is exactly what `cleanliness` calls clean --
   * so a sidecar naming a send in flight reported a file with nothing
   * outstanding over it.
   */
  it('refuses a sidecar whose outstanding list is not a list', function () {
    for (const shape of [{ seq: 1, req: 'R' }, 'R', 7]) {
      const read = sidecarFromDisk(sidecarWith(shape));
      assert.strictEqual(
        read.read,
        false,
        `an outstanding list written as ${JSON.stringify(shape)} was read as no sends at all`
      );
      assert.strictEqual((read as { because: string }).because, 'unreadable');
    }
  });

  /*
   * THE TWIN, TWICE: a real list still reads, and a record with no
   * outstanding key at all is an older record rather than an unreadable
   * one -- which is the whole reason this cannot simply demand an array.
   */
  it('still reads an outstanding list, and a record that has none', function () {
    const listed = sidecarFromDisk(sidecarWith([{ seq: 1, req: 'R' }]));
    assert.strictEqual(listed.read, true);
    assert.deepStrictEqual(
      (listed as { sidecar: { outstanding: unknown } }).sidecar.outstanding,
      [{ seq: 1, req: 'R' }]
    );
    const older = JSON.parse(sidecarWith([]));
    delete older.outstanding;
    const read = sidecarFromDisk(JSON.stringify(older));
    assert.strictEqual(read.read, true, 'a record with no outstanding key was refused');
    assert.deepStrictEqual((read as { sidecar: { outstanding: unknown } }).sidecar.outstanding, []);
  });

  /*
   * THE TWIN: a sidecar whose outstanding records ARE readable still
   * reads, with them. Without it the cell above would pass on a reader
   * that refuses every sidecar.
   */
  it('still reads a sidecar whose outstanding sends it can read', function () {
    const read = sidecarFromDisk(sidecarWith([{ seq: 1, req: 'R1' }]));
    assert.strictEqual(read.read, true, JSON.stringify(read));
    assert.deepStrictEqual((read as { sidecar: { outstanding: unknown } }).sidecar.outstanding, [
      { seq: 1, req: 'R1' }
    ]);
  });
});

/*
 * A SEARCH WHOSE EVERY HIT WAS LEFT OUT. On the human route the pinned core
 * (6593f78) prints its `(excluded ...)` line when every hit was in a
 * superseded or refuted block, a line that is not a hit, on which the
 * human-route reader would throw "the store did not answer the search"; an
 * older core (5c28e42) printed nothing, so this client said "nothing in the
 * store matches".
 * The search is asked with `--wire` and read as `whereis` is: the items are
 * the hits, and the `excluded` clause is the count of what was left out.
 * The answers below are the core's, clause for clause: an empty search
 * carries the scan's cut, scanned and coverage, then the validity clauses,
 * then the versions of no hit.
 */
describe('a search whose every hit was left out', () => {
  before(async () => {
    await initWire();
  });

  const SCAN =
    '(cut (("w0000001" . 4))) (scanned (blocks 3) (fields (title keywords src names doc body)) (unreadable-blocks 0)) ' +
    '(coverage (defs (names 0)) (names-from (datum lexical)))';
  const LEFT_OUT = `(ok (items) ${SCAN} (excluded (blocks (superseded 1) (refuted 0))) (versions ()))\n`;
  const NOTHING = `(ok (items) ${SCAN} (versions ()))\n`;
  const storeAnswering = (stdout: string, asked: Array<{ verb: string; args: string[] }> = []): StoreModel =>
    new StoreModel(
      new Client({
        kind: 'stand-in',
        send: async (verb: string, args: string[]): Promise<RawResult> => {
          asked.push({ verb, args });
          return { argv: [], rc: 0, stdout, stderr: '' };
        }
      })
    );

  it('asks with --wire and reads no hits and the count of what was left out', async () => {
    const asked: Array<{ verb: string; args: string[] }> = [];
    const reading = await storeAnswering(LEFT_OUT, asked).searchReading('osprey');
    assert.deepStrictEqual(asked, [{ verb: 'search', args: ['osprey', '--wire'] }]);
    assert.deepStrictEqual(reading.hits, []);
    assert.deepStrictEqual(reading.leftOut, { superseded: 1, refuted: 0 });
  });

  it('says the words match only in a block that is not in force, as information', async () => {
    const editor = editorThat('osprey');
    const outcome = await runSearch(storeAnswering(LEFT_OUT), editor);
    assert.deepStrictEqual(outcome, { did: 'nothing', because: 'no-hits', query: 'osprey' });
    assert.deepStrictEqual(editor.said, ['nothing in force matches "osprey": it is found only in a block that is superseded.']);
    assert.deepStrictEqual(editor.levels, ['information']);
    assert.deepStrictEqual(editor.offered, []);
  });

  it('TWIN: with no excluded clause, nothing matched is what is said', async () => {
    const editor = editorThat('osprey');
    const reading = await storeAnswering(NOTHING).searchReading('osprey');
    assert.strictEqual(reading.leftOut, null);
    await runSearch(storeAnswering(NOTHING), editor);
    assert.deepStrictEqual(editor.said, ['nothing in the store matches "osprey".']);
  });

  it('an (excluded ...) line, which the pinned core prints on the human route, is not a hit', () => {
    assert.strictEqual(hitsOf(parseAnswers('(excluded (blocks (superseded 1) (refuted 0)))\n')), null);
  });
});
