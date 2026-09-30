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
 * A READING THAT COULD NOT SEE EVERY WRITER IS SHOWN AS ITS ROWS PLUS A
 * WARNING: never as complete, never as a failure.
 *
 * The core appends `(incomplete (unreadable (writer w) (path p) (reason
 * r)) ...)` to every answer built from a load that could not read a
 * writer. Measured on the pinned core with a second writer's directory
 * chmod 000: outline, search, conflicts, `read --recursive` and check all
 * answered with their rows and that clause, and this extension showed the
 * store as unusable -- the outline "could not be read", the search "did
 * not happen" with its hit in the answer, a subtree held "something that is
 * not a block". These cells pin the other reading: the rows, and the notes
 * beside them.
 */

import * as assert from 'assert';
import * as path from 'path';
import { execFileSync, spawnSync } from 'child_process';
import { readdirSync } from 'fs';
import { Client, Note, interpret, mergeNotes, splitIncomplete } from '../../src/client';
import { Outbox } from '../../src/outbox';
import { StoreModel, verdictOf } from '../../src/model';
import { NOT_A_WRITES_ANSWER, Saver, Settle } from '../../src/saver';
import { Asking, Hit, Searcher, runSearch } from '../../src/search';
import { StatusFacts, incompleteWarning, statusLine } from '../../src/status';
import { CliTransport, RawResult, TransportError } from '../../src/transport';
import { Datum, answerOf, initWire, isList, parseAnswers } from '../../src/wire';
import { FakeCore } from '../support/fake';
import { IGNORED_DURABILITY } from '../support/ignored-durability';

/*
 * THE WRITER THE READING COULD NOT SEE, as the pinned core spelled it (the
 * path shortened; the shape is the core's).
 */
const PATH = '/s/store/writers/zzzzzzzz';
const CLAUSE = `(incomplete (unreadable (writer "zzzzzzzz") (path "${PATH}") (reason "Permission denied")))`;
const NOTE: Note = { writer: 'zzzzzzzz', path: PATH, reason: 'Permission denied' };

const BLOCK_A1 =
  '((id . "a.1") (deleted . #f) (fields (kind . section) (src . "a needle here\\n") (title . "Alpha")) ' +
  '(position root . 0) (edges))';
const BLOCK_A2 =
  '((id . "a.2") (deleted . #f) (fields (kind . section) (src . "under\\n") (title . "Child")) ' +
  '(position "a.1" . 0) (edges))';

function raw(stdout: string, rc = 0, argv: string[] = []): RawResult {
  return { argv, rc, stdout, stderr: '' };
}

/*
 * A CLIENT WHOSE ANSWERS ARE WRITTEN HERE, by verb and, for `read`, by
 * whether the subtree was asked for. Everything it answers goes through
 * `interpret`, the one reader of core bytes.
 */
function scripted(answers: { [key: string]: string }): Client {
  return new Client({
    kind: 'test',
    send: async (verb: string, args: string[]): Promise<RawResult> => {
      const key = verb === 'read' ? (args.includes('--recursive') ? 'read-recursive' : 'read') : verb;
      const said = answers[key];
      if (said === undefined) {
        throw new Error(`no answer written for ${key}`);
      }
      return raw(said, 0, [verb, ...args]);
    }
  });
}

function carries(datum: Datum, head: string): boolean {
  return isList(datum) && datum.some((part) => answerOf(part, head) !== null);
}

describe('I1 the clause comes off every answer family, and its notes travel beside the body', () => {
  before(async () => {
    await initWire();
  });

  it('takes it off an items answer where it is the last line, and keeps the hit', () => {
    const answer = interpret(raw(`(hit "a.1" 2 "a needle here" (fields (src)))\n${CLAUSE}\n`), 'search', 'items', ['needle']);
    assert.strictEqual(answer.answers.length, 1, 'the clause was left among the hits');
    assert.ok(answerOf(answer.answers[0], 'hit') !== null, 'the hit was lost');
    assert.deepStrictEqual(answer.notes, [NOTE]);
  });

  it('takes it off an empty items answer that is the clause alone, leaving no items', () => {
    const answer = interpret(raw(`${CLAUSE}\n`), 'conflicts', 'items', []);
    assert.deepStrictEqual(answer.answers, []);
    assert.deepStrictEqual(answer.notes, [NOTE]);
  });

  it('takes it out of a check datum, and the verdict is still read, with the notes', () => {
    const stdout =
      '(check (store "s") (writers (("w" (end 2) (torn #f) (integrity ())) ("zzzzzzzz" (end unreadable) ' +
      `(torn unreadable) (integrity ())))) (snapshots ()) (notes ()) (verdict damaged) ${CLAUSE})\n`;
    const answer = interpret(raw(stdout, 1), 'check', 'datum', []);
    assert.strictEqual(answer.answers.length, 1);
    assert.ok(!carries(answer.answers[0], 'incomplete'), 'the clause is still inside the check form');
    const verdict = verdictOf(answer.answers, answer.notes ?? null);
    assert.ok(verdict.known && verdict.verdict === 'damaged', JSON.stringify(verdict));
    assert.deepStrictEqual(verdict.notes, [NOTE]);
  });

  it("takes it out of a block read's ok form, and the block is the one asked for", async () => {
    const model = new StoreModel(scripted({ read: `(ok ${BLOCK_A1} ${CLAUSE})\n` }));
    const reading = await model.blockReading('a.1');
    assert.strictEqual(reading.block?.id, 'a.1');
    assert.deepStrictEqual(reading.notes, [NOTE]);
  });

  it('takes it out of a refusal, and the refusal is kept', () => {
    const answer = interpret(raw(`(error unknown-id "b.1" (nearest ()) ${CLAUSE})\n`, 1), 'read', 'datum', ['b.1']);
    assert.strictEqual(answer.answers.length, 1);
    assert.ok(answerOf(answer.answers[0], 'error', { at: 1, is: 'unknown-id' }) !== null, 'the refusal was lost');
    assert.ok(!carries(answer.answers[0], 'incomplete'), 'the clause is still inside the refusal');
    assert.deepStrictEqual(answer.notes, [NOTE]);
  });

  it('takes it off a --wire commit answer before the items are opened, and keeps the rest of the envelope', () => {
    const stdout =
      '(ok (items (ok (events (("w" . 9))) (state (("a.2" . "hhh"))) (cursor ("w" . 9)) (replay #f))) ' +
      `(behind (("other" . 2))) ${CLAUSE})\n`;
    const answer = interpret(raw(stdout), 'commit', 'datum', ['a.2', '--wire']);
    assert.strictEqual(answer.answers.length, 1);
    assert.ok(answerOf(answer.answers[0], 'ok') !== null, 'the inner answer was not opened');
    assert.ok(carries(answer.envelope, 'behind'), 'the envelope lost its other clauses');
    assert.ok(!carries(answer.envelope, 'incomplete'), 'the clause is still in the envelope');
    assert.deepStrictEqual(answer.notes, [NOTE]);
  });

  it('reads the text of an outline asked for with --wire, and its notes', () => {
    const answer = interpret(raw(`(ok (text "- a.1  Alpha\\n- a.2  Beta\\n") ${CLAUSE})\n`), 'outline', 'text', ['--wire']);
    assert.strictEqual(answer.text, '- a.1  Alpha\n- a.2  Beta\n');
    assert.deepStrictEqual(answer.notes, [NOTE]);
  });

  it('leaves a complete answer exactly as it was, with no notes', () => {
    const stdout = '(hit "a.1" 2 "a needle here" (fields (src)))\n(hit "a.2" 1 "under" (fields (src)))\n';
    const answer = interpret(raw(stdout), 'search', 'items', ['needle']);
    assert.deepStrictEqual(answer.answers, parseAnswers(stdout));
    assert.strictEqual(answer.notes, null);
    const one = interpret(raw(`(ok ${BLOCK_A1})\n`), 'read', 'datum', ['a.1']);
    assert.deepStrictEqual(one.answers, parseAnswers(`(ok ${BLOCK_A1})\n`));
    assert.strictEqual(one.notes, null);
  });

  it('does not take the refusal kind spelled with the same word for the clause', () => {
    const stdout =
      '(error incomplete (failed (op fsync) (path "/s/writers/w/000001.sexp") (reason "Input/output error") ' +
      '(errno 5)) (written ((append "/s/writers/w/000001.sexp"))))\n';
    const answer = interpret(raw(stdout, 1), 'set', 'datum', ['a.2']);
    assert.deepStrictEqual(answer.answers, parseAnswers(stdout));
    assert.strictEqual(answer.notes, null);
  });

  it('refuses a note it cannot read by naming the clause, rather than dropping it', () => {
    const missing = '(incomplete (unreadable (writer "zzzzzzzz") (path "/p")))';
    assert.throws(
      () => interpret(raw(`(ok (text "- a.1  Alpha\\n") ${missing})\n`), 'outline', 'text', ['--wire']),
      (e: unknown) => e instanceof TransportError && /incomplete clause has no reason/.test(e.message)
    );
    assert.throws(
      () => splitIncomplete(parseAnswers(`(ok ${CLAUSE} ${CLAUSE})\n`)[0], 'read'),
      (e: unknown) => e instanceof TransportError && /2 incomplete clauses/.test(e.message)
    );
  });
});

/*
 * A LOG CUT BY DAMAGE IS SAID IN THE SAME CLAUSE, as a second kind of note:
 * `(cut (writer w) (path p) (reason r) (kind k) (after n))`, the writer read
 * up to record n. The shape is the one the core's design gives; the re-pin
 * that brings it records a real answer beside these.
 */
const CUT = '(cut (writer "w") (path "/s/writers/w/000002.sexp") (reason "checksum mismatch") (kind crc) (after 7))';

describe('I9 a cut writer is a note of its own, said as a cut', () => {
  before(async () => {
    await initWire();
  });

  it('reads the cut with its kind and the last record kept, and says so in the warning', () => {
    const answer = interpret(raw(`(ok (text "- a.1  Alpha\\n") (incomplete ${CUT}))\n`), 'outline', 'text', ['--wire']);
    assert.deepStrictEqual(answer.notes, [
      { writer: 'w', path: '/s/writers/w/000002.sexp', reason: 'checksum mismatch', cut: { kind: 'crc', after: 7 } }
    ]);
    assert.strictEqual(
      incompleteWarning(answer.notes as Note[]),
      'Incomplete: writer w was read only up to record 7, its log being cut there (crc at /s/writers/w/000002.sexp: ' +
        'checksum mismatch). What you see is what could be read, within the usual limits.'
    );
  });

  it('keeps an unreadable writer beside a cut one, each said its own way', () => {
    const both = `(incomplete (unreadable (writer "zzzzzzzz") (path "${PATH}") (reason "Permission denied")) ${CUT})`;
    const answer = interpret(raw(`(ok (text "") ${both})\n`), 'outline', 'text', ['--wire']);
    assert.strictEqual((answer.notes as Note[]).length, 2);
    assert.deepStrictEqual((answer.notes as Note[])[0], NOTE);
    const said = incompleteWarning(answer.notes as Note[]);
    assert.match(said, /writer zzzzzzzz could not be read/);
    assert.match(said, /writer w was read only up to record 7/);
    assert.strictEqual(incompleteWarning([NOTE]), `Incomplete: writer zzzzzzzz could not be read (${PATH}: Permission denied). What you see is what could be read, within the usual limits.`);
  });

  /*
   * A WRITER STOPPED AT A SEGMENT IT CANNOT READ, in the shape the core gives
   * it (incomplete.sc, `incomplete-note-clauses`): a cut note whose kind is
   * segment-unreadable, the path the segment file and `after` the last record
   * read before it. The warning says where the writer stops, and claims
   * nothing about the other writers.
   */
  it('says of a writer stopped at an unreadable segment where it stops, not "what the other writers wrote"', () => {
    const segment =
      '(cut (writer "w") (path "/s/writers/w/000003.sexp") (reason "Input/output error") (kind segment-unreadable) (after 12))';
    const answer = interpret(raw(`(ok (text "- a.1  Alpha\\n") (incomplete ${segment}))\n`), 'outline', 'text', ['--wire']);
    assert.deepStrictEqual(answer.notes, [
      { writer: 'w', path: '/s/writers/w/000003.sexp', reason: 'Input/output error', cut: { kind: 'segment-unreadable', after: 12 } }
    ]);
    const said = incompleteWarning(answer.notes as Note[]);
    assert.strictEqual(
      said,
      'Incomplete: writer w was read only up to record 12, its log being cut there (segment-unreadable at ' +
        '/s/writers/w/000003.sexp: Input/output error). What you see is what could be read, within the usual limits.'
    );
    assert.doesNotMatch(said, /the other writers/);
  });

  it('merges notes once each, a cut and an unreadable note of one writer kept apart, two cuts apart by where', () => {
    const cut = (after: number): Note => ({ writer: 'w', path: '/p', reason: 'r', cut: { kind: 'crc', after } });
    const plain: Note = { writer: 'w', path: '/p', reason: 'r' };
    assert.deepStrictEqual(mergeNotes([cut(7)], [cut(7)]), [cut(7)]);
    assert.deepStrictEqual(mergeNotes([cut(7)], [cut(8)]), [cut(7), cut(8)]);
    assert.deepStrictEqual(mergeNotes([plain], [cut(7)]), [plain, cut(7)]);
  });

  it('still refuses a note of any other kind, and a cut without its after, by name', () => {
    assert.throws(
      () => interpret(raw('(ok (text "") (incomplete (truncated (writer "w") (path "/p") (reason "r"))))\n'), 'outline', 'text', ['--wire']),
      (e: unknown) => e instanceof TransportError && /is not a writer's note/.test(e.message)
    );
    const noAfter = '(cut (writer "w") (path "/p") (reason "r") (kind crc))';
    assert.throws(
      () => interpret(raw(`(ok (text "") (incomplete ${noAfter}))\n`), 'outline', 'text', ['--wire']),
      (e: unknown) => e instanceof TransportError && /has no after/.test(e.message)
    );
  });
});

/*
 * THE HARNESS: the compiled extension against a stand-in VS Code API. The
 * editor host cannot read a tree item, a decoration or a notification back,
 * so this is where the rows the editor is handed can be looked at.
 */
function schedule(name: string, timeout = 20000): any {
  const run = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
    encoding: 'utf8',
    timeout
  });
  assert.strictEqual(run.status, 0, run.stdout + run.stderr);
  const result = JSON.parse(run.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete, true);
  return result.result;
}

describe('I2 the tree shows every row the store read, and the warning first', function () {
  this.timeout(60000);
  it('draws the warning as an information row that opens nothing, then the blocks, and raises no error', () => {
    const r = schedule('incomplete-tree');
    assert.ok(r.items.length >= 2, `the tree drew ${JSON.stringify(r.items)}`);
    const warning = r.items[0];
    assert.strictEqual(warning.command, null, 'the warning row opens something');
    assert.strictEqual(warning.icon, 'info', 'the warning row is not drawn as information');
    assert.match(warning.label, /writer zzzzzzzz could not be read/);
    assert.match(warning.label, /\/stores\/A\/writers\/zzzzzzzz/);
    assert.match(warning.label, /Permission denied/);
    assert.doesNotMatch(warning.label, /unreadable/i, 'the warning says "unreadable"');
    assert.strictEqual(r.items[1].label, 'Alpha', 'the block the store read is not shown');
    assert.strictEqual(r.items[1].command, 'theourgia.openBlock');
    assert.deepStrictEqual(
      r.shown.filter((s: { level: string }) => s.level === 'error'),
      [],
      'an incomplete reading was raised as an error'
    );
  });
});

describe('I3 a search that could not see every writer still finds, and says so where it shows its result', () => {
  function searcher(hits: Hit[]): Searcher {
    return {
      search: async () => hits,
      searchReading: async () => ({ hits, notes: [NOTE] })
    };
  }
  function editor(): { asking: Asking; said: Array<{ text: string; level: string }>; opened: string[]; titles: Array<string | undefined>; order: string[] } {
    const said: Array<{ text: string; level: string }> = [];
    const opened: string[] = [];
    const titles: Array<string | undefined> = [];
    const order: string[] = [];
    return {
      said,
      opened,
      titles,
      order,
      asking: {
        ask: async () => 'needle',
        pick: async (hits: Hit[], _placeHolder: string, title?: string) => {
          titles.push(title);
          order.push('pick');
          return hits[0];
        },
        say: (text: string, level: 'information' | 'error') => {
          said.push({ text, level });
          order.push('say');
        },
        open: async (id: string) => {
          opened.push(id);
          order.push('open');
        }
      }
    };
  }
  const hit = (id: string): Hit => ({ id, score: 1, note: '' });
  const warning = incompleteWarning([NOTE]);

  it('makes the warning the message when nothing matched, as information', async () => {
    const e = editor();
    const outcome = await runSearch(searcher([]), e.asking);
    assert.strictEqual(outcome.did, 'nothing');
    assert.strictEqual(e.said.length, 1);
    assert.strictEqual(e.said[0].level, 'information');
    assert.ok(e.said[0].text.startsWith(warning), e.said[0].text);
  });

  it('says the warning before it opens the one hit, and opens it', async () => {
    const e = editor();
    await runSearch(searcher([hit('a.1')]), e.asking);
    assert.deepStrictEqual(e.order, ['say', 'open']);
    assert.deepStrictEqual(e.said, [{ text: warning, level: 'information' }]);
    assert.deepStrictEqual(e.opened, ['a.1']);
  });

  it('puts the warning above the picker when there are several, and opens the one chosen', async () => {
    const e = editor();
    await runSearch(searcher([hit('a.1'), hit('a.2')]), e.asking);
    assert.deepStrictEqual(e.titles, [warning]);
    assert.deepStrictEqual(e.opened, ['a.1']);
  });
});

describe('I4 a subtree read without a writer answers its blocks, and the listing carries the notes', () => {
  before(async () => {
    await initWire();
  });
  it('lists the child and hands on the notes, with the marks not known', async () => {
    const model = new StoreModel(
      scripted({ 'read-recursive': `${BLOCK_A1}\n${BLOCK_A2}\n${CLAUSE}\n`, conflicts: `${CLAUSE}\n` })
    );
    const listing = await model.childrenOf('a.1');
    assert.deepStrictEqual(listing.nodes.map((n) => n.id), ['a.2']);
    assert.deepStrictEqual(listing.notes, [NOTE]);
    assert.strictEqual(listing.marksKnown, false, 'marks read without a writer were reported known');
  });
});

describe('I5 the conflict count never counts the clause, and marks read without a writer are not known', () => {
  before(async () => {
    await initWire();
  });
  it('counts a clause-only answer as none, with the notes, and its marks as not known', async () => {
    const model = new StoreModel(scripted({ conflicts: `${CLAUSE}\n` }));
    const reading = await model.conflictReading();
    assert.strictEqual(reading.count, 0, 'the clause was counted as a conflict');
    assert.deepStrictEqual(reading.notes, [NOTE]);
    const marks = await model.structuralMarks();
    assert.strictEqual(marks.complete, false, 'marks read without a writer were reported complete');
  });

  it('counts the two conflicts beside the clause as two', async () => {
    const model = new StoreModel(scripted({ conflicts: `(orphan "a.2")\n(conflict "a.3" cycle)\n${CLAUSE}\n` }));
    assert.strictEqual((await model.conflictReading()).count, 2);
  });
});

describe('I6 the status bar says incomplete while the last reading was, and stops when one is complete', function () {
  this.timeout(60000);
  const facts = (incomplete: Note[] | null): StatusFacts => ({
    store: '/s',
    actor: 'a',
    cursor: null,
    conflicts: 0,
    pending: 0,
    blocked: null,
    unreachable: null,
    incomplete
  });

  it('draws the marker with the warning icon, and no error icon for the clause', () => {
    const line = statusLine(facts([NOTE]));
    assert.match(line.text, /\$\(warning\) incomplete/);
    assert.doesNotMatch(line.text, /\$\(error\)/);
    assert.strictEqual(line.warning, true);
    assert.ok(line.tooltip.includes(incompleteWarning([NOTE])), line.tooltip);
    assert.doesNotMatch(line.tooltip, /^no conflicts$/m, 'a count taken without a writer was called "no conflicts"');
  });

  it('shows the marker after an incomplete reading in the window and clears it after a complete one', () => {
    const r = schedule('incomplete-tree');
    assert.match(String(r.status.text), /\$\(warning\) incomplete/, 'the window never showed the marker');
    assert.doesNotMatch(String(r.statusAfter.text), /incomplete/, 'the marker stayed after a complete reading');
    assert.strictEqual(r.after[0].label, 'Alpha', 'the warning row stayed after a complete reading');
  });
});

describe('I7 the refusal kind `incomplete` is untouched, and the clause is not a refusal', () => {
  before(async () => {
    await initWire();
  });

  /*
   * MEMBERSHIP, NOT A COUNT: the pinned core still makes `(error incomplete
   * ...)`, and this extension still sorts it by its own row.
   */
  it('still finds `incomplete` among the kinds the core makes, and still has its row', () => {
    const directory = process.env.THEOURGIA_CORE;
    assert.ok(directory !== undefined && directory.length > 0, 'THEOURGIA_CORE is not set');
    const files = readdirSync(directory)
      .filter((name) => name.endsWith('.sc') || name.endsWith('.ss'))
      .map((name) => path.join(directory, name));
    const output = execFileSync(
      process.env.THEOURGIA_SCHEME ?? 'scheme',
      ['--script', path.join(__dirname, '../support/core-refusals.ss'), ...files],
      { encoding: 'utf8' }
    );
    const kinds = new Set(output.trim().split('\n').map((row) => row.split('\t')[1]));
    assert.ok(kinds.has('incomplete'), 'the census no longer finds the refusal kind incomplete');
    assert.ok(NOT_A_WRITES_ANSWER.incomplete !== undefined, 'the row for the refusal kind incomplete is gone');
  });

  it('hands an ok answer on without the clause, so nothing that sorts refusals can meet it', () => {
    const answer = interpret(raw(`(ok (events (("w" . 9))) (cursor ("w" . 9)) (replay #f) ${CLAUSE})\n`), 'set', 'datum', ['a.2']);
    assert.ok(answer.ok);
    assert.ok(!carries(answer.answers[0], 'incomplete'), 'the clause is still in the answer');
  });
});

describe('I8 a save the store took without a writer is saved, and its receipt carries the notes', function () {
  this.timeout(60000);
  let core: FakeCore | undefined;
  before(async () => {
    await initWire();
  });
  after(() => core?.dispose());

  it('saves, and the outcome carries the notes', async () => {
    core = new FakeCore([
      { match: ['check'], stdout: '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n', rc: 0 },
      { match: ['set'], stdout: `(ok (events (("w" . 8))) (state (("a.2" . "hhh"))) (cursor ("w" . 8)) (replay #f) ${CLAUSE})\n`, rc: 0 }
    ]);
    const outbox = new Outbox(core.outboxFile());
    outbox.load();
    const settle: Settle = (req, settlement) => {
      outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
    };
    const saver = new Saver(scriptedCore(core), outbox, settle, IGNORED_DURABILITY);
    const outcome = await saver.save('a.2', 'src', 'body\n');
    assert.strictEqual(outcome.status, 'saved', outcome.message);
    assert.deepStrictEqual(outcome.notes, [NOTE]);
  });

  it('shows the warning as information after the save, in the window', () => {
    const r = schedule('incomplete-save');
    const information = r.shown.filter((s: { level: string }) => s.level === 'information').map((s: { text: string }) => s.text);
    assert.ok(
      information.some((text: string) => /^Theourgia: Incomplete: writer zzzzzzzz could not be read/.test(text)),
      `the save's notes were not shown: ${JSON.stringify(r.shown)}`
    );
    assert.deepStrictEqual(r.shown.filter((s: { level: string }) => s.level === 'error'), []);
  });
});

function scriptedCore(core: FakeCore): Client {
  return new Client(new CliTransport(core.config(), core.env()));
}

/*
 * I9, THE REAL CORE. A real store with a second writer's directory chmod
 * 000, reached through the shipping transport and its daemon; only the VS
 * Code API is the stand-in, because the editor host cannot read a tree
 * item or a decoration back, so this harness is the only place the
 * assertions below are readable.
 */
describe('I9 a real store missing a writer, through the shipping transport', function () {
  this.timeout(180000);
  it('answers through the daemon, and the tree and the document show the rows and the warning', () => {
    const r = schedule('incomplete-real', 150000);
    assert.strictEqual(r.local, null, 'THEOURGIA_LOCAL is set, so the reading did not go through the daemon');
    assert.ok(r.daemons >= 1, `no daemon of this store is running under the fixture (${r.daemons})`);
    assert.ok(r.runEntries >= 1, 'the fixture run directory holds nothing');
    assert.ok(r.items.length >= 2, `the tree drew ${JSON.stringify(r.items)}`);
    assert.strictEqual(r.items[0].command, null);
    assert.strictEqual(r.items[0].icon, 'info');
    assert.match(r.items[0].label, /^Incomplete: writer zzzzzzzz could not be read \(.*writers\/zzzzzzzz: /);
    assert.ok(r.items.slice(1).some((i: { label: string }) => i.label === 'Alpha'), 'the real block is not shown');
    assert.deepStrictEqual(r.shown.filter((s: { level: string }) => s.level === 'error'), []);
    assert.ok(
      r.decorations.some((d: { before: string }) => /^Incomplete: writer zzzzzzzz could not be read/.test(d.before)),
      `the document view shows no banner: ${JSON.stringify(r.decorations)}`
    );
  });
});
