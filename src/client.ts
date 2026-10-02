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
 * One request, one answer -- the client's side of what rpc.sc calls the
 * same thing.
 *
 * THE EXIT CODE IS THE VERDICT AND NOTHING ELSE IS. The thin client exits
 * with the code the daemon computed -- 0 when the core's own `rpc-ok?` says
 * the answer is a success (daemon.sc:1881, relayed by `deliver!` in
 * theourgia.sc); `init`, which the thin client runs in-process through
 * `core.sc` (`local-verbs`, theourgia.sc:72), exits by the same predicate
 * there (core.sc:760) -- and that
 * predicate lives in the core precisely so that a shell and a client
 * cannot come to different opinions. So this file never decides success
 * from the head symbol -- `(ok ...)` on a non-zero exit is a core that
 * crashed after printing, and treating it as a confirmed write is how an
 * edit gets reported as saved when it was not.
 *
 * THE ANSWER'S KIND IS NOT ON THE WIRE, and that is the one place this
 * client is forced to hold a second opinion. The core says an answer is
 * text, items or a single datum (rpc.sc:178-183), and `render-human`
 * (render.sc:65) draws each
 * differently -- text as its own bytes, items one datum per line, and
 * anything else as one datum -- but it prints no marker saying which it
 * drew. Over a socket the whole `(ok (text ...))` form would arrive and
 * this table would not exist; over the command line there is no other
 * way to know whether `- a.1  One` is an outline or four symbols.
 *
 * THE TABLE IS THEREFORE EXHAUSTIVE ON PURPOSE. An unlisted verb is a
 * programming error and says so, rather than defaulting to a kind and
 * misreading the first answer it sees.
 */

import { CoreConfig } from './config';
import { RawResult, Transport, TransportError, transportFor } from './transport';
import { AnswerParseError, Datum, answerOf, asInteger, isList, isSym, parseAnswers } from './wire';

export type AnswerKind = 'text' | 'items' | 'datum';

export interface Answer {
  argv: string[];
  rc: number;
  ok: boolean;
  kind: AnswerKind;
  text: string;
  answers: Datum[];
  /*
   * THE OUTER FORM, WHEN THE REQUEST ASKED FOR THE MACHINE RENDERING.
   *
   * NOTE: `--wire` WRAPS AN ANSWER AND THE HUMAN RENDERING DROPS WHAT IS
   * ROUND IT. Measured on a core pinned before F100b (877f0da or earlier): a commit answers
   * `(ok (events ...) (cursor ...) (replay #f))` by default and
   * `(ok (items (ok (events ...) (cursor ...) (replay #f))) (behind ...))`
   * with the flag -- and `render-human` (render.sc:57) renders only
   * the `items` clause and `incomplete`, so every other clause beside them
   * is lost on the way out.
   *
   * So `answers` goes on holding the item, exactly as before, and the
   * form round it arrives here. Every caller that reads a cursor or an
   * event is untouched; the one caller that wants a clause from outside
   * asks for it by name.
   *
   * NULL FOR EVERY REQUEST THAT DID NOT ASK. It is not "there was no
   * envelope": a verb asked for in the ordinary mode has none to have.
   */
  envelope: Datum | null;
  stderr: string;
  /*
   * WHAT THE READING COULD NOT SEE. Null when the store read every
   * writer; otherwise one note per writer it could not read, taken out
   * of the answer by `splitIncomplete` before any parser sees it.
   * Optional only so that an answer written by hand in a test still
   * type-checks; every answer `interpret` builds carries it.
   */
  notes?: Note[] | null;
}

/*
 * ONE WRITER A READING COULD NOT SEE: the core's `(unreadable (writer w)
 * (path p) (reason r))`, one of the notes of an `(incomplete ...)`
 * clause.
 */
export interface Note {
  writer: string;
  path: string;
  reason: string;
  /*
   * PRESENT FOR A CUT. A cut is a writer whose log was read up to a record
   * and not past it, because what follows is damaged: `kind` is the log's
   * own name for the damage and `after` the last record kept. The writer's
   * records up to there ARE in the reading.
   *
   * NOTE: ABSENCE DOES NOT MEAN NOTHING OF THE WRITER WAS READ. A writer
   * stopped at a segment it cannot read is reported without this clause,
   * in the same form as a writer that could not be read at all, while its
   * records before that segment are in the reading (log.sc, `cut-note`).
   */
  cut?: { kind: string; after: number };
}

/*
 * A READING THAT COULD NOT SEE EVERY WRITER SAYS SO IN ONE CLAUSE.
 *
 * The core appends `(incomplete (unreadable (writer w) (path p) (reason
 * r)) ...)` to every answer built from a load that could not read a
 * writer, whatever the answer's head: an `ok`, a `check`, a refusal.
 * The rows beside it are still the store's answer -- what could be read
 * -- so the clause is taken out here, once, and
 * travels beside the body as notes. A parser handed the body never meets
 * it, and a view handed the notes cannot mistake the reading for a
 * complete one.
 *
 * NEVER: A CLAUSE THIS CLIENT CANNOT READ IS NOT DROPPED. A note missing
 * its writer, path or reason, or two such clauses in one answer, is a
 * refusal naming the clause: dropping it would show the reading as
 * complete, which is the one thing it is not.
 *
 * NOTE: THE CLAUSE IS LOOKED FOR AMONG THE ELEMENTS AFTER THE HEAD, never
 * at the head's position. `(error incomplete (failed ...) (written
 * ...))` is a refusal KIND spelled with the same word, and it is a name
 * in position 1, not a list headed by it.
 */
export function splitIncomplete(datum: Datum, verb: string, detail = ''): { body: Datum; notes: Note[] | null } {
  if (!isList(datum)) {
    return { body: datum, notes: null };
  }
  const at: number[] = [];
  datum.forEach((part, index) => {
    if (index > 0 && answerOf(part, 'incomplete') !== null) {
      at.push(index);
    }
  });
  if (at.length === 0) {
    return { body: datum, notes: null };
  }
  if (at.length > 1) {
    throw incompleteRefused(verb, `it carries ${at.length} incomplete clauses where one was expected`, detail);
  }
  return {
    body: datum.filter((_, index) => index !== at[0]),
    notes: notesOf(datum[at[0]], verb, detail)
  };
}

/*
 * THE SAME CLAUSE AS ITS OWN LINE. The human rendering of an items answer
 * prints the items one per line and then the clause as a line of its
 * own (render.sc), so there it is the LAST datum of the list rather than
 * an element of one. Only the last: a clause anywhere else is not where
 * the core puts it, and is left for the parsers to refuse.
 */
export function splitTrailingIncomplete(
  data: Datum[],
  verb: string,
  detail = ''
): { body: Datum[]; notes: Note[] | null } {
  const last = data.length > 0 ? data[data.length - 1] : null;
  if (last === null || answerOf(last, 'incomplete') === null) {
    return { body: data, notes: null };
  }
  return { body: data.slice(0, -1), notes: notesOf(last, verb, detail) };
}

function notesOf(clause: Datum, verb: string, detail: string): Note[] {
  const parts = isList(clause) ? clause.slice(1) : [];
  if (parts.length === 0) {
    throw incompleteRefused(verb, 'its incomplete clause names no writer', detail);
  }
  return parts.map((part, index) => {
    /*
     * TWO KINDS OF NOTE: `(unreadable (writer w) (path p) (reason r))`, a
     * writer the reading could not read, wholly or past a segment it
     * cannot read, and `(cut (writer w) (path p)
     * (reason r) (kind k) (after n))`, a writer read up to record n. Any
     * other kind is refused by name, as before: a note this build does not
     * know how to say is not one it may leave out.
     */
    const unreadable = answerOf(part, 'unreadable');
    const cut = unreadable === null ? answerOf(part, 'cut') : null;
    const note = unreadable ?? cut;
    if (note === null) {
      throw incompleteRefused(verb, `note ${index + 1} of its incomplete clause is not a writer's note`, detail);
    }
    const field = (name: string): string => {
      const value = note.value(name);
      if (!value.read || typeof value.value !== 'string') {
        throw incompleteRefused(verb, `note ${index + 1} of its incomplete clause has no ${name}`, detail);
      }
      return value.value;
    };
    const read = { writer: field('writer'), path: field('path'), reason: field('reason') };
    if (cut === null) {
      return read;
    }
    const kind = cut.value('kind');
    const after = cut.value('after');
    if (!kind.read || !isSym(kind.value)) {
      throw incompleteRefused(verb, `note ${index + 1} of its incomplete clause has no kind`, detail);
    }
    const last = after.read ? asInteger(after.value) : null;
    if (last === null || last < 0) {
      throw incompleteRefused(verb, `note ${index + 1} of its incomplete clause has no after`, detail);
    }
    return { ...read, cut: { kind: kind.value.name, after: last } };
  });
}

function incompleteRefused(verb: string, why: string, detail: string): TransportError {
  return new TransportError(
    'unreadable',
    `the core answered the ${verb} with a reading that could not see every writer, and ${why}`,
    detail
  );
}

/*
 * NOTES FROM SEVERAL ANSWERS, ONCE EACH. A view built from several
 * requests -- the tree asks for the outline, the marks and every block --
 * shows one warning per writer, not one per request.
 */
export function mergeNotes(...lists: Array<Note[] | null | undefined>): Note[] | null {
  const out: Note[] = [];
  for (const list of lists) {
    for (const note of list ?? []) {
      const same = (n: Note): boolean =>
        n.writer === note.writer &&
        n.path === note.path &&
        n.reason === note.reason &&
        (n.cut === undefined) === (note.cut === undefined) &&
        (n.cut === undefined || (n.cut.kind === note.cut?.kind && n.cut.after === note.cut?.after));
      if (!out.some(same)) {
        out.push(note);
      }
    }
  }
  return out.length === 0 ? null : out;
}

const ITEM_VERBS = new Set(['refs', 'search', 'log', 'conflicts', 'diff', 'whereis', 'grep']);

/*
 * THE CRITERION IS "APPENDS A RECORD", not "is a verb I thought of".
 * Every verb here writes into the log, so for every one of them an
 * empty answer leaves the same question open -- whether the store
 * changed -- and that question is what this set exists to refuse to
 * guess at. A verb that only reads is not here however slow or
 * important it is.
 */
const ALWAYS_WRITE_VERBS = new Set([
  'write', 'commit',
  'insert',
  'set',
  'move',
  'del',
  'link',
  'unlink',
  'batch',
  'init',
  'import-md',
  'adopt',
  'snapshot',
  'publish'
]);

/*
 * WHETHER A VERB APPENDS A RECORD IS NOT ALWAYS DECIDED BY ITS NAME.
 * `tag` with a name writes one and `tag` with no argument lists what is
 * there, and the core says so itself: trackability is a property of the
 * request, not of the verb's name. For a verb that writes, an empty
 * answer is a fault rather than a fact -- the store may or may not have
 * changed, and the answer was the only thing that would have said which.
 */
export function appendsARecord(verb: string, args: string[]): boolean {
  if (verb === 'tag') {
    return args.length > 0;
  }
  return ALWAYS_WRITE_VERBS.has(verb);
}

const KNOWN_VERBS = new Set([
  'write', 'commit', 'drafts', 'discard',
  'whereis',
  'init',
  'insert',
  'set',
  'move',
  'del',
  'link',
  'unlink',
  'batch',
  'import-md',
  'export-md',
  'adopt',
  'check',
  'snapshot',
  'publish',
  'outline',
  'read',
  'refs',
  /*
   * `grep` finds lines: one `(match <id> <line> "<text>")` per line, read by
   * the hover for the name under the pointer (src/hover.ts).
   */
  'grep',
  'search',
  'log',
  'tag',
  'diff',
  'conflicts',
  /*
   * `split-suggest` writes a review file and no record, so it is not in
   * ALWAYS_WRITE_VERBS; its answer is one datum.
   */
  'split-suggest',
  /*
   * `export-code` writes a projection of the store into a directory and
   * `supply` keeps derived facts beside the store; neither appends a
   * record, and each answers one datum. They are sent by the supply
   * commands (src/supply.ts).
   */
  'export-code',
  'supply',
  /*
   * NOTE: `describe` IS HERE BECAUSE ASKING WITHOUT IT FAILS SILENTLY.
   * An unknown verb throws out of `answerKind`, the caller that asks
   * which verbs the core has catches everything and answers "I could not
   * find out", and the entry that depends on the answer never appears --
   * with nothing anywhere reading red. Found by writing the caller.
   */
  'describe'
]);

export function answerKind(verb: string, args: string[]): AnswerKind {
  if (!KNOWN_VERBS.has(verb)) {
    throw new Error(`no answer kind is recorded for the verb ${verb}`);
  }
  if (verb === 'outline') {
    return 'text';
  }
  if (verb === 'read') {
    if (args.includes('--md')) {
      return 'text';
    }
    return args.includes('--recursive') ? 'items' : 'datum';
  }
  if (verb === 'tag') {
    return args.length === 0 ? 'items' : 'datum';
  }
  return ITEM_VERBS.has(verb) ? 'items' : 'datum';
}

/*
 * A WRITE THAT APPENDED A RECORD NAMES IT. `(cursor ("w" . 6))` on a
 * fresh write and `(event ("w" . 6))` on a replay: the same fact under
 * two names because the two answers are about different things -- where
 * the store now stands, and which record the store is standing behind.
 * A client that read only the first would stop advancing its cursor the
 * first time a retry succeeded.
 */
export class Client {
  private readonly transport: Transport;

  constructor(transport: Transport) {
    this.transport = transport;
  }

  public static fromConfig(config: CoreConfig, env: NodeJS.ProcessEnv = process.env): Client {
    return new Client(transportFor(config, env));
  }

  public get transportKind(): string {
    return this.transport.kind;
  }

  /*
   * `input` is the request's standard input, for the one verb that reads
   * its operand there (`batch`); see `Transport`.
   */
  public async request(verb: string, args: string[] = [], input?: string): Promise<Answer> {
    const kind = answerKind(verb, args);
    const raw: RawResult = await this.transport.send(verb, args, input);
    return interpret(raw, verb, kind, args);
  }
}

/*
 * A FAILED REQUEST IS ALWAYS ONE DATUM, whatever the verb would have
 * answered had it succeeded. `outline` against a directory with no store
 * in it prints `(error no-store "...")`, not an outline, so the kind
 * table applies to success and the exit code decides which branch this
 * is. Reading the failure as text would leave a caller with an error
 * message it could not tell from a store whose first line happens to
 * start with a parenthesis.
 */
export function interpret(raw: RawResult, verb: string, kind: AnswerKind, args: string[] = []): Answer {
  const ok = raw.rc === 0;
  if (!ok) {
    /*
     * A REFUSAL CAN CARRY THE CLAUSE TOO, inside its own form: it is
     * taken out and the refusal is kept, so what classifies a refusal
     * sees the refusal and nothing appended to it.
     */
    const refusal = readData(raw, verb, args);
    const split = refusal.length === 1 ? splitIncomplete(refusal[0], verb, raw.stdout) : null;
    return {
      argv: raw.argv,
      rc: raw.rc,
      ok,
      kind: 'datum',
      text: raw.stdout,
      answers: split === null ? refusal : [split.body],
      envelope: null,
      stderr: raw.stderr,
      notes: split === null ? null : split.notes
    };
  }
  if (kind === 'text') {
    /*
     * NEVER: THE CLAUSE IS NOT LOOKED FOR IN TEXT. On the human route it is
     * a line after the text, and a line of text can say anything -- a
     * title carrying a newline included. A text answer that has to be
     * read for it is asked for with `--wire`, where the text is a string
     * inside the form and the clause sits beside it.
     */
    if (args.includes('--wire')) {
      return textFromWire(raw, verb, args);
    }
    return {
      argv: raw.argv,
      rc: raw.rc,
      ok,
      kind,
      text: raw.stdout,
      answers: [],
      envelope: null,
      stderr: raw.stderr,
      notes: null
    };
  }
  const whole = readData(raw, verb, args);
  /*
   * THE CLAUSE COMES OFF BEFORE ANYTHING IS UNWRAPPED. Under `--wire`, and
   * for any single-datum answer, it is an element of the one outer form;
   * on the human route of an items answer it is the last line.
   */
  const outer =
    whole.length === 1 && (args.includes('--wire') || kind === 'datum')
      ? splitIncomplete(whole[0], verb, raw.stdout)
      : null;
  const trailing = outer === null && kind === 'items' ? splitTrailingIncomplete(whole, verb, raw.stdout) : null;
  const read = outer !== null ? [outer.body] : trailing !== null ? trailing.body : whole;
  const notes = outer !== null ? outer.notes : trailing !== null ? trailing.notes : null;
  /*
   * THE UNWRAPPING TURNS ON THE REQUEST, NOT ON THE SHAPE.
   *
   * NOTE: A SHAPE TEST WOULD REACH INTO A VERB THAT IS NOT WRAPPED. `tag`
   * with no argument, `refs`, `conflicts` -- any verb whose own answer
   * is a list of items -- would have its items taken for an envelope's
   * items and be unwrapped a second time. What was asked for is a fact
   * about this request; what came back merely looks a certain way.
   */
  /*
   * NEVER: AND THE FORM HAS TO HAVE SAID `ok`.
   *
   * The request decides whether an envelope was asked for; the head
   * decides whether what came back is one. Without it,
   * `(garbage (items (ok (cursor ("w" . 7)) (replay #f))))` was unwrapped
   * and its inner form served as the answer -- a cursor taken out of a
   * form nobody can parse. Measured in an eleventh review round. `hitsOf`
   * and `knownVerbs` were repaired for the same shape in earlier rounds
   * and this reader was not, because those repairs were made where the
   * findings pointed.
   */
  /*
   * NEVER: AND TWO `items` CLAUSES IS NOT AN ANSWER THAT CARRIES NONE.
   *
   * The decoder refuses a duplicated clause, and until a sixteenth
   * review round it refused it the same way it says "no such clause" --
   * so `(ok (items ...) (items ...))` fell through to the line below and
   * the whole form was handed on as the answers. This client asked for
   * an envelope; an answer with two of them is one it cannot open.
   */
  const wrapped = args.includes('--wire') && read.length === 1 ? answerOf(read[0], 'ok') : null;
  const items = wrapped === null ? null : wrapped.clause('items');
  if (items !== null && !items.read && items.because !== 'absent') {
    throw new TransportError(
      'unreadable',
      `the core answered the ${verb} with two item lists, and which one is the answer is not ` +
        'something this client may choose',
      raw.stdout
    );
  }
  const opened = items !== null && items.read ? items.items : null;
  const envelope = opened === null ? null : read[0];
  const answers = opened === null ? read : opened;
  if (answers.length === 0 && appendsARecord(verb, args)) {
    throw new TransportError(
      'no-answer',
      `the core exited ${raw.rc} without answering the ${verb}; ` +
        'whether the record was appended is not known',
      raw.stderr
    );
  }
  if (kind === 'datum' && answers.length > 1) {
    throw new TransportError(
      'unreadable',
      `the core answered the ${verb} with ${answers.length} data where one was expected`,
      raw.stdout
    );
  }
  return {
    argv: raw.argv,
    rc: raw.rc,
    ok,
    kind,
    text: raw.stdout,
    answers,
    envelope,
    stderr: raw.stderr,
    notes
  };
}

/*
 * A TEXT ANSWER ASKED FOR WITH `--wire`: `(ok (text "...") ...)`. The
 * clause comes off the form, and the text is the string inside it -- the
 * same bytes the human route would have printed, without the line after.
 */
function textFromWire(raw: RawResult, verb: string, args: string[]): Answer {
  const whole = readData(raw, verb, args);
  if (whole.length !== 1) {
    throw new TransportError(
      'unreadable',
      `the core answered the ${verb} with ${whole.length} data where one text form was expected`,
      raw.stdout
    );
  }
  const split = splitIncomplete(whole[0], verb, raw.stdout);
  const form = answerOf(split.body, 'ok');
  const text = form === null ? null : form.value('text');
  if (text === null || !text.read || typeof text.value !== 'string') {
    throw new TransportError(
      'unreadable',
      `the core answered the ${verb} with a form that holds no text`,
      raw.stdout
    );
  }
  return {
    argv: raw.argv,
    rc: raw.rc,
    ok: true,
    kind: 'text',
    text: text.value,
    answers: [],
    envelope: split.body,
    stderr: raw.stderr,
    notes: split.notes
  };
}

/*
 * AN UNREADABLE ANSWER NAMES WHAT IT WAS ABOUT. "the core's answer to
 * read could not be read" sends whoever gets it looking through a whole
 * store; the id was in the request all along. A block can be made
 * unreadable by something stored in it -- an edge whose name the wire
 * cannot spell, a body byte the reader refuses -- and then this message
 * is the only thing pointing at which block to go and look at.
 */
function readData(raw: RawResult, verb: string, args: string[]): Datum[] {
  try {
    return parseAnswers(raw.stdout);
  } catch (e) {
    if (e instanceof AnswerParseError) {
      const about = args.filter((a) => !a.startsWith('--'));
      const named = about.length > 0 ? ` (${about[0]})` : '';
      throw new TransportError(
        'unreadable',
        `the core's answer to ${verb}${named} could not be read at line ${e.line}: ${e.message}`,
        e.text
      );
    }
    throw e;
  }
}
