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
 * The one place this extension turns bytes into data.
 *
 * THE READER IS NOT OURS. It is goeteia's `goeteia/sexpr`, taken as a
 * published dependency and held to the golden tables that igropyr's own
 * writer generated. A fourth implementation of one wire format is how
 * two implementations start disagreeing about what a datum is, and the
 * tables are the only thing that would notice.
 *
 * THE TABLES STAY IN THIS TREE even though the reader no longer does.
 * The package does not ship them, and it should not: a reader checked
 * against a table it carries itself is checked against nothing. They
 * live under test/fixtures/goeteia and adjudicate whatever version of
 * the dependency is installed.
 *
 * IT IS LOADED DYNAMICALLY BECAUSE IT IS AN ES MODULE and an extension
 * host loads this tree as CommonJS. `initWire` is called once during
 * activation and once in a test's setup; everything after that is
 * synchronous, so no caller has to be async merely to read an answer.
 *
 * ONE LINE IS ONE DATUM, and that is a fact about the command line
 * rather than about the format: `render-human` (render.sc:69-72) prints an
 * `items` answer with one `write` per item and no wrapper around them, and
 * Chez's writer escapes
 * a newline inside a string rather than emitting it. A transport that
 * had the whole answer -- the socket one, when it exists -- would not
 * split anything.
 */


export type Datum = unknown;

export interface SymLike {
  readonly name: string;
}

export interface DottedListLike {
  readonly items: Datum[];
  readonly tail: Datum;
}

export interface SexprApi {
  read(text: string, opts?: { maxDepth?: number }): Datum;
  write(value: Datum, opts?: { maxDepth?: number }): string;
  sym(name: string): SymLike;
  dotted(items: Datum[], tail: Datum): Datum;
  Sym: new (name: string) => SymLike;
  DottedList: new (items: Datum[], tail: Datum) => DottedListLike;
  SexprError: new (message: string, position: number) => Error & { position: number };
  MAX_DEPTH: number;
}

let api: SexprApi | null = null;
let loading: Promise<SexprApi> | null = null;

/*
 * THE IMPORT IS BUILT AT RUNTIME BECAUSE THE COMPILER WOULD OTHERWISE
 * REMOVE IT. Emitting CommonJS, tsc rewrites `import(x)` into
 * `require(x)`, and require cannot load an ES module on the Node an
 * extension host of this vintage carries -- so the one call that has to
 * survive compilation is the one the compiler helpfully replaces.
 */
const dynamicImport = new Function('specifier', 'return import(specifier);') as (
  specifier: string
) => Promise<unknown>;

/*
 * THE ONE PLACE THE READER IS NAMED. Exported so that a cell can ask
 * WHICH module this loads rather than assuming it: a suite that resolves
 * the specifier for itself is checking its own copy of the name, and a
 * build pointed at some other file would go on passing it.
 */
export const READER = 'goeteia/sexpr';

export async function initWire(): Promise<SexprApi> {
  if (api !== null) {
    return api;
  }
  if (loading === null) {
    loading = dynamicImport(READER).then((mod: unknown) => {
      api = mod as SexprApi;
      return api;
    });
  }
  return loading;
}

export function wire(): SexprApi {
  if (api === null) {
    throw new Error('the s-expression reader has not been loaded; call initWire first');
  }
  return api;
}

/*
 * A LINE THAT WILL NOT PARSE FAILS THE WHOLE ANSWER. Keeping the lines
 * that did parse would hand a caller a short `items` list that looks
 * exactly like a complete one -- and the caller that reads `conflicts`
 * would then report fewer conflicts than the store holds, quietly.
 */
export class AnswerParseError extends Error {
  public readonly line: number;
  public readonly position: number;
  public readonly text: string;

  constructor(cause: Error & { position?: number }, line: number, text: string) {
    super(`answer line ${line} is not readable: ${cause.message}`);
    this.name = 'AnswerParseError';
    this.line = line;
    this.position = typeof cause.position === 'number' ? cause.position : -1;
    this.text = text;
  }
}

export function isSym(value: Datum, name?: string): value is SymLike {
  const sexpr = wire();
  if (!(value instanceof sexpr.Sym)) {
    return false;
  }
  return name === undefined || (value as SymLike).name === name;
}

export function isDotted(value: Datum): value is DottedListLike {
  return value instanceof wire().DottedList;
}

export function isList(value: Datum): value is Datum[] {
  return Array.isArray(value);
}

/*
 * Every top-level datum the command line printed, in order. Blank lines
 * are skipped rather than refused: a text answer that happens to end in
 * a newline is not two answers.
 */
export function parseAnswers(stdout: string): Datum[] {
  const sexpr = wire();
  const out: Datum[] = [];
  const lines = stdout.split('\n');
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (line.trim().length === 0) {
      continue;
    }
    try {
      out.push(sexpr.read(line));
    } catch (e) {
      throw new AnswerParseError(e as Error & { position?: number }, i + 1, line);
    }
  }
  return out;
}

/*
 * ---- reading the shapes the core prints ---------------------------------
 *
 * These know the core's spelling and nothing about what a verb means.
 * `(cursor ("w" . 6))` is read here; whether a cursor was required is
 * decided where the request was made.
 */

function headName(value: Datum): string | null {
  if (!isList(value) || value.length === 0) {
    return null;
  }
  const first = value[0];
  return isSym(first) ? first.name : null;
}

/*
 * The first element of `value` that is a list headed by `name`. The core
 * writes an answer as a sequence of such clauses -- `(ok (events ...)
 * (state ...) (cursor ...) (replay #f))` -- and a caller wants one of
 * them by name, never by position: `ok` and `ok (replay #t)` put the
 * event in different places.
 */
/*
 * READING AN ANSWER: THE HEAD AND THE CLAUSE, TOGETHER OR NOT AT ALL.
 *
 * KEY: WHY THIS EXISTS AND WHY `clause` STOPPED BEING EXPORTED.
 *
 * Thirteen review rounds found ONE defect in nine readers: a clause
 * taken out of a form without asking what the form said. `(error
 * (items))` read as "the store looked and found nothing";
 * `(garbage (cursor ("w" . 7)))` confirmed a save and took it out of the
 * queue; `(garbage (writers ...))` would have had this client choose a
 * writer and start writing. Each was repaired where its finding pointed
 * and the tenth instance was only a matter of time -- because the two
 * steps were two calls, and the first could be forgotten.
 *
 * Here they are one call. There is no way to reach a clause of an answer
 * without naming the head it must have, so the defect is not something a
 * reader can be written with. The compiler is the census.
 *
 * NOTE: `expect` IS FOR "IS THIS ABOUT WHAT I ASKED". Two further defects
 * were of that kind -- a record for `b.1` returned for `a.1`, and a
 * subtree answer that never mentions the block it is under. A reader
 * that names the id it asked about gets an answer only if the form
 * agrees.
 */
/*
 * NEVER: A `Form` IS NOT A SHAPE ANYBODY CAN BUILD.
 *
 * It was a plain structural interface, so nothing stopped a caller
 * writing `{clause: clauseOfRecord.bind(null, datum), ...}` and handing
 * that to a reader -- the provenance this type is supposed to carry was
 * a convention, not a guarantee, and the delivery note claimed the
 * compiler enforced it. Measured in a fifteenth review round.
 *
 * The brand is a private symbol: a value carrying it can only have come
 * from `answerOf` in this file, because nothing else can name it.
 */
/*
 * NOTE: A REAL SYMBOL, NOT A `declare`. The first version of this was a
 * type-only declaration, so `[VERIFIED]: true` compiled and threw at
 * run time -- the brand existed for the compiler and not for the
 * program. The suite said so at once, in 189 cells.
 *
 * It is not exported, so no file outside this one can name it, which is
 * what makes a `Form` something only `answerOf` can produce.
 */
const VERIFIED: unique symbol = Symbol('a form whose head has been checked');

/*
 * WHAT A CLAUSE LOOKUP FOUND, AND -- WHEN IT FOUND NOTHING USABLE --
 * WHICH OF THE TWO REASONS IT WAS.
 *
 * NEVER: `null` FOR BOTH WAS AN INABILITY ANSWERED AS AN ABSENCE, one
 * level below every reader that has been repaired for that shape.
 *
 * A fifteenth review round had this decoder start refusing a DUPLICATED
 * clause, because a `writers` clause carrying two listings had supplied
 * a cursor from the first. It refused by returning null -- the same
 * answer as "this form has no such clause" -- and a sixteenth round
 * measured what three of its callers then did with it: `readBlock` read
 * a record carrying two `fields` clauses as a block with NO fields, and
 * served empty prefix, src and text for it; `hitsOf` and `interpret`
 * read a duplicated `items` clause as "this answer was not an envelope"
 * and handed the whole form on as the answers.
 *
 * Every one of those is the collapse this delivery exists to remove,
 * arriving through the repair for a different one. The two reasons are
 * separate now and the type makes each caller say which it is handling;
 * five of the eight callers refuse both and were already right.
 */
export type Why = 'absent' | 'duplicated' | 'unreadable';

export type Clause = { read: true; items: Datum[] } | { read: false; because: Why };

/*
 * THE SAME THREE REASONS FOR A READER THAT WANTS ONE VALUE rather than a
 * clause: `Form.value` and the record reader both answer this.
 */
export type Single = { read: true; value: Datum } | { read: false; because: Why };

const ABSENT = { read: false as const, because: 'absent' as const };
const DUPLICATED = { read: false as const, because: 'duplicated' as const };
const UNREADABLE = { read: false as const, because: 'unreadable' as const };

/*
 * THE NAME AT THE FRONT OF A DATUM, WHETHER IT WAS WRITTEN WITH A DOT OR
 * WITHOUT ONE.
 *
 * NEVER: A DOTTED OCCURRENCE IS AN OCCURRENCE. `clause` counted only list
 * items, so `(fields . bad) (fields . worse)` was two occurrences that
 * the duplicate refusal could not see: a seventeenth review round read
 * `((id . "a.1") (fields . bad) (fields . worse) (position root . 0))`
 * as block a.1 with NO fields, and the same trick unwrapped a `--wire`
 * commit answer through `(items . bad)`. Both of the refusals added the
 * round before were walked past this way.
 */
function frontName(value: Datum): string | null {
  const head = isList(value) ? value[0] : isDotted(value) ? value.items[0] : undefined;
  return head !== undefined && isSym(head) ? head.name : null;
}

/*
 * KEY: EVERY ELEMENT NAMED `name`, AND THIS IS THE ONLY PLACE THAT COUNTS
 * THEM.
 *
 * Five routes reached into a form looking for a named element --
 * `clause`, `whole`, `value`, the record reader and the association
 * reader -- and each had its own answer to "how many are there". Rounds
 * fifteen, sixteen and seventeen each found one of them giving the
 * reassuring answer: the first of several, or none at all. They all ask
 * this now, and a sixth route that does not is what `decoding.test.ts`
 * fails on.
 */
function occurrencesOf(value: Datum, name: string): Datum[] {
  if (!isList(value)) {
    return [];
  }
  return value.filter((item) => frontName(item) === name);
}

export interface Form {
  /*
   * WHICH HEAD WAS VERIFIED. Kept so that a reader can say what it is
   * holding, and so that a `Form` obtained for one head is not silently
   * read as another.
   */
  readonly head: string;
  readonly [VERIFIED]: true;
  /*
   * THE REST OF A CLAUSE, or null when the form has no such clause. The
   * form's head is already known: that is what having a `Form` means.
   */
  clause(name: string): Clause;
  /*
   * THE CLAUSE AS IT APPEARED, its own name at the front.
   *
   * NOTE: FOR THE READERS THAT DIGEST IT. A working projection's identity
   * is the digest of its whole clause -- dropping the name would change
   * every id this build computes, silently, which is not a thing a
   * refactor may do. `clause` is what almost every reader wants; this is
   * for the ones that carry the clause somewhere rather than taking it
   * apart.
   */
  whole(name: string): Clause;
  /*
   * THE SINGLE VALUE OF A ONE-VALUE CLAUSE, and undefined for a clause
   * carrying any other number -- the same rule `clauseValue` has always
   * had, for the same reason.
   */
  value(name: string): Single;
  /*
   * THE WHOLE FORM, for the readers that need its positions rather than
   * its clauses -- an `(error unknown-id "a.1" ...)` names its subject
   * by position, not by clause.
   */
  datum: Datum;
}

export function answerOf(
  datum: Datum,
  head: string,
  expect?: { at: number; is: string }
): Form | null {
  if (headName(datum) !== head) {
    return null;
  }
  if (expect !== undefined) {
    /*
     * NOTE: A NAME IN AN ANSWER MAY BE A SYMBOL OR A STRING, and the core
     * uses both: `(error unknown-id "a.1" ...)` names its family with a
     * symbol and its subject with a string. Comparing with `===` alone
     * made the symbol case never match -- so the guard that was meant
     * to check "is this about what I asked" silently refused
     * everything, which the suite caught as a refusal where absence was
     * expected. Both spellings are the same name here.
     */
    if (!isList(datum) || datum.length <= expect.at) {
      return null;
    }
    const at = datum[expect.at];
    if (!(at === expect.is || isSym(at, expect.is))) {
      return null;
    }
  }
  return {
    head,
    [VERIFIED]: true,
    clause: (name: string) => clauseRest(datum, name),
    whole: (name: string) => clause(datum, name),
    value: (name: string) => clauseValue(datum, name),
    datum
  } as Form;
}

/*
 * A CLAUSE OF A STORE RECORD, WHICH HAS NO HEAD TO CHECK.
 *
 * NEVER: NOT FOR AN ANSWER. A block record reads
 * `((id . "a.1") (deleted . #f) (fields ...) ...)` -- its first element
 * is a pair, not a name, so there is nothing for `answerOf` to verify
 * and nothing this could check on a caller's behalf. A writer entry in
 * a `check` listing is the same. Both are values taken from a form whose
 * head HAS been checked, which is what makes reading them safe.
 *
 * Which files may call this is pinned by a census; see
 * test/unit/decoding.test.ts.
 */
export function clauseOfRecord(value: Datum, name: string): Clause {
  return clauseRest(value, name);
}

/*
 * NEVER: THIS SAID `Datum | undefined`, WHICH IS `unknown`.
 *
 * `Datum` is `unknown`, so the declared type accepted any change to what
 * this actually returns -- and when `clauseValue` began answering a
 * `Single`, the one caller went on handing it to `asInteger`, which
 * takes `unknown` too. Nothing failed to compile and 84 cells went red
 * at once. The compiler is only the census where the types say
 * something; a signature written in `unknown` is a hole in it.
 */
export function recordValue(value: Datum, name: string): Single {
  return clauseValue(value, name);
}

/*
 * NEVER: TWO CLAUSES OF ONE NAME ARE NOT ONE CLAUSE.
 *
 * This returned the first match, so
 * `(check (writers (...)) (writers (...)))` answered the first listing
 * and a malformed answer supplied a cursor -- the cardinality check
 * added a round earlier looks INSIDE one clause and could not see a
 * second clause beside it. Measured in a fifteenth review round. A form
 * carrying a name twice is a form this build cannot account for, and
 * the readers above all know how to be told so.
 */
function clause(value: Datum, name: string): Clause {
  const found = occurrencesOf(value, name);
  if (found.length === 0) {
    return ABSENT;
  }
  if (found.length > 1) {
    return DUPLICATED;
  }
  /*
   * ONE OCCURRENCE, WRITTEN WITH A DOT, IS NOT A CLAUSE. `(fields . bad)`
   * has a name and no sequence after it; reading it as "no fields" is
   * the collapse this file exists to stop, so it is a third reason
   * rather than an absence.
   */
  return isList(found[0]) ? { read: true, items: found[0] } : UNREADABLE;
}

/*
 * TWO SHAPES THAT READ ALIKE AND MEAN DIFFERENT THINGS, so there are two
 * functions and no function that guesses between them.
 *
 * An ASSOCIATION ENTRY is `(name . value)` -- `(src . "body")`, `(level
 * . 2)`. A CLAUSE is `(name value)` -- `(end 7)`, `(cursor ("w" . 6))`.
 * On the wire the second IS the first with a one-element list for its
 * value, because `(name . (value))` and `(name value)` are the same
 * datum; nothing in the text says which the writer meant. So the reader
 * cannot decide, and a helper that returned `entry[1]` when a list had
 * two elements and the rest otherwise would be deciding -- and would
 * unwrap `(fields (title . "Two"))`, whose single field then arrives
 * where a list of fields was expected.
 *
 * The caller knows which convention the core used for the thing it is
 * asking about; a reader cannot tell it from the text.
 */
/*
 * THE TAIL OF A PAIR IN AN ASSOCIATION LIST -- a headless reader, and it
 * had escaped being named as one.
 *
 * NOTE: THIS IS THE THIRD DOOR. `clauseOfRecord` and `recordValue` were
 * pinned by a census and this was not, so
 * `assocTail(parseAnswers('(error (items 7))')[0], 'items')` answered
 * `7` -- a clause of an unchecked answer, through a name nobody had
 * thought to look for. Found in a fourteenth review round, which was
 * asked to look for exactly this.
 *
 * It is censused with the other two; see test/unit/decoding.test.ts.
 */
/*
 * NEVER: AND IT TOOK THE FIRST OF SEVERAL.
 *
 * Measured in a seventeenth review round:
 * `((id . "a.1") (id . "b.1") (deleted . #f) ...)` read as block a.1 --
 * an identity chosen by this reader out of an answer that names two.
 * It counts through `occurrencesOf` like everything else now.
 */
export function assocTail(alist: Datum[], name: string): Single {
  const found = occurrencesOf(alist, name);
  if (found.length === 0) {
    return ABSENT;
  }
  if (found.length > 1) {
    return DUPLICATED;
  }
  const entry = found[0];
  if (isDotted(entry)) {
    return { read: true, value: cdrOf(entry) };
  }
  if (isList(entry)) {
    return { read: true, value: entry.slice(1) };
  }
  return UNREADABLE;
}

/*
 * Everything after the tag of the clause named `name`. `(fields (a . 1)
 * (b . 2))` gives the two entries, and `(fields)` gives none -- which is
 * a block with no fields and not a block that could not be read.
 */
function clauseRest(value: Datum, name: string): Clause {
  const found = clause(value, name);
  return found.read ? { read: true, items: found.items.slice(1) } : found;
}

/*
 * The single value of a one-value clause. A clause carrying two values
 * where one was expected is not read as the first of them: the answers
 * this reads are small and fixed, and a shape that is not the expected
 * one is a core that changed.
 */
/*
 * NEVER: AND THE SINGLE VALUE SAYS WHICH KIND OF NOTHING IT FOUND.
 *
 * `clause` was split into absent and duplicated in a sixteenth review
 * round and this was left answering `undefined` for both -- so in a
 * seventeenth round, `(ok (cursor ("a" . 1)) (cursor ("b" . 2)) (event
 * ("wrong" . 99)) (replay #f))` had the two cursors cancel each other
 * out and `eventFromWrite` take its event from the `event` clause: a
 * save settled on a cursor the answer never gave. Every `value()` call
 * goes through here.
 *
 * A clause that is there and carries a number of values other than one
 * is `unreadable` rather than absent, for the same reason.
 */
function clauseValue(value: Datum, name: string): Single {
  const rest = clauseRest(value, name);
  if (!rest.read) {
    return rest;
  }
  if (rest.items.length !== 1) {
    return UNREADABLE;
  }
  return { read: true, value: rest.items[0] };
}

/*
 * THE CDR OF A DOTTED ENTRY IS NOT ITS TAIL WHEN MORE THAN ONE ELEMENT
 * PRECEDES THE DOT. `(position "a.1" . 0)` is `position` consed onto
 * `("a.1" . 0)`, and returning the 0 -- the tail -- would hand a caller
 * the sibling index where the parent belongs. Every block the core
 * prints carries exactly this shape, so getting it wrong shows up as a
 * tree in which nothing has a parent.
 */
export function cdrOf(entry: DottedListLike): Datum {
  if (entry.items.length === 1) {
    return entry.tail;
  }
  return wire().dotted(entry.items.slice(1), entry.tail);
}

/*
 * AN INTEGER ARRIVES AS A BIGINT, ALWAYS. The reader builds every
 * integer with BigInt -- there is no small-integer case -- so `typeof x
 * === 'number'` is false for every sequence number, every `ord` and
 * every level the core prints, and a client that tested for it would
 * read a whole store as though nothing had a position. Both are accepted
 * here because the format's other numeric shapes (a flonum) are numbers,
 * and a value outside the range a double represents exactly is refused
 * rather than rounded: a cursor that is off by one names a different
 * record.
 */
export function asInteger(value: Datum): number | null {
  if (typeof value === 'bigint') {
    if (value > BigInt(Number.MAX_SAFE_INTEGER) || value < BigInt(Number.MIN_SAFE_INTEGER)) {
      return null;
    }
    return Number(value);
  }
  if (typeof value === 'number' && Number.isSafeInteger(value)) {
    return value;
  }
  return null;
}

export interface Event {
  writer: string;
  seq: number;
}

/*
 * `("w" . 6)` and `("w" 6)` are both an event in the core's output: the
 * write answers use the pair and `log` uses the list. Reading only one
 * of them would work until the first time a client looked at the log.
 */
export function readEvent(value: Datum): Event | null {
  if (isDotted(value) && value.items.length === 1) {
    const writer = value.items[0];
    const seq = asInteger(value.tail);
    if (typeof writer === 'string' && seq !== null) {
      return { writer, seq };
    }
    return null;
  }
  if (isList(value) && value.length === 2 && typeof value[0] === 'string') {
    const seq = asInteger(value[1]);
    if (seq !== null) {
      return { writer: value[0], seq };
    }
  }
  return null;
}

export function formatCursor(event: Event): string {
  return `${event.writer}:${event.seq}`;
}
