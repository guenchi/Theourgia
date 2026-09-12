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
import { AnswerParseError, Datum, parseAnswers } from './wire';

export type AnswerKind = 'text' | 'items' | 'datum';

export interface Answer {
  argv: string[];
  rc: number;
  ok: boolean;
  kind: AnswerKind;
  text: string;
  answers: Datum[];
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
  'conflicts'
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
      stderr: raw.stderr
    };
  }
  if (kind === 'text') {
    return { argv: raw.argv, rc: raw.rc, ok, kind, text: raw.stdout, answers: [], stderr: raw.stderr };
  }
  const answers = readData(raw, verb, args);
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
  return { argv: raw.argv, rc: raw.rc, ok, kind, text: raw.stdout, answers, stderr: raw.stderr };
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
