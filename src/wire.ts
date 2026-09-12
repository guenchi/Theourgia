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
 * rather than about the format: cli.ss prints an `items` answer with one
 * `write` per item and no wrapper around them, and Chez's writer escapes
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

export async function initWire(): Promise<SexprApi> {
  if (api !== null) {
    return api;
  }
  if (loading === null) {
    loading = dynamicImport('goeteia/sexpr').then((mod: unknown) => {
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

export function headName(value: Datum): string | null {
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
export function clause(value: Datum, name: string): Datum[] | null {
  if (!isList(value)) {
    return null;
  }
  for (const item of value) {
    if (isList(item) && headName(item) === name) {
      return item;
    }
  }
  return null;
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
 * asking about. rpc.ss is where that is written down.
 */
export function assocTail(alist: Datum[], name: string): Datum | undefined {
  for (const entry of alist) {
    if (isDotted(entry) && entry.items.length >= 1 && isSym(entry.items[0], name)) {
      return cdrOf(entry);
    }
    if (isList(entry) && entry.length >= 1 && isSym(entry[0], name)) {
      return entry.slice(1);
    }
  }
  return undefined;
}

/*
 * Everything after the tag of the clause named `name`. `(fields (a . 1)
 * (b . 2))` gives the two entries, and `(fields)` gives none -- which is
 * a block with no fields and not a block that could not be read.
 */
export function clauseRest(value: Datum, name: string): Datum[] | null {
  const found = clause(value, name);
  return found === null ? null : found.slice(1);
}

/*
 * The single value of a one-value clause. A clause carrying two values
 * where one was expected is not read as the first of them: the answers
 * this reads are small and fixed, and a shape that is not the expected
 * one is a core that changed.
 */
export function clauseValue(value: Datum, name: string): Datum | undefined {
  const rest = clauseRest(value, name);
  if (rest === null || rest.length !== 1) {
    return undefined;
  }
  return rest[0];
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
