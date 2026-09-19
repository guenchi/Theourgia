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
 * One request, one answer -- the client's side of what rpc.ss calls the
 * same thing.
 *
 * THE EXIT CODE IS THE VERDICT AND NOTHING ELSE IS. cli.ss exits 0 when
 * the core's own `rpc-ok?` says the answer is a success, and that
 * predicate lives in the core precisely so that a shell and a client
 * cannot come to different opinions. So this file never decides success
 * from the head symbol -- `(ok ...)` on a non-zero exit is a core that
 * crashed after printing, and treating it as a confirmed write is how an
 * edit gets reported as saved when it was not.
 *
 * THE ANSWER'S KIND IS NOT ON THE WIRE, and that is the one place this
 * client is forced to hold a second opinion. rpc.ss classifies an answer
 * as text, items or a single datum, and print-answer draws each
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
import { AnswerParseError, Datum, answerOf, parseAnswers } from './wire';

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
   * ROUND IT. Measured on the pinned core: a commit answers
   * `(ok (events ...) (cursor ...) (replay #f))` by default and
   * `(ok (items (ok (events ...) (cursor ...) (replay #f))) (behind ...))`
   * with the flag -- and `render-human` (render.ss:53-59) renders only
   * the `items` clause, so every other clause beside it is lost on the
   * way out.
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
}

const ITEM_VERBS = new Set(['refs', 'search', 'log', 'conflicts', 'diff']);

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
  'search',
  'log',
  'tag',
  'diff',
  'conflicts',
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

  public async request(verb: string, args: string[] = []): Promise<Answer> {
    const kind = answerKind(verb, args);
    const raw: RawResult = await this.transport.send(verb, args);
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
    return {
      argv: raw.argv,
      rc: raw.rc,
      ok,
      kind: 'datum',
      text: raw.stdout,
      answers: readData(raw, verb, args),
      envelope: null,
      stderr: raw.stderr
    };
  }
  if (kind === 'text') {
    return {
      argv: raw.argv,
      rc: raw.rc,
      ok,
      kind,
      text: raw.stdout,
      answers: [],
      envelope: null,
      stderr: raw.stderr
    };
  }
  const read = readData(raw, verb, args);
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
    stderr: raw.stderr
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
