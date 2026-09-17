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
 * Saving, which is one `set` at a time and never two.
 *
 * THE ANSWERS ARE SORTED BY WHAT THEY SAY ABOUT THE STORE, not by
 * whether they were called errors. Three groups, and the middle one is
 * the reason this file exists:
 *
 *   the request ran      -- ok, or a replay of the same request. Drop
 *                           the entry and carry the cursor forward.
 *   nobody can say       -- `(error unknown <why>)`, a timeout, a core
 *                           that printed nothing. KEEP the entry. The
 *                           store may hold the record; asking again with
 *                           the same id is the only way to find out, and
 *                           dropping it here is how an edit disappears.
 *   the request did not  -- every other refusal. Drop the entry and say
 *                           so, because retrying cannot change the
 *                           answer and leaving it queued would suggest
 *                           it might.
 *
 * THE VERDICT IS THE EXIT CODE. A core that printed `(ok ...)` and then
 * exited non-zero did not finish; reading the head symbol would confirm
 * a save that the core itself refuses to call a success.
 *
 * ONE AT A TIME, AND THE HEAD OF THE QUEUE BLOCKS. A second save sent
 * before the first is answered would carry a cursor that the first is
 * about to move, and the store would refuse it -- so the queue is
 * strictly serial, and an entry nobody can resolve stops the ones behind
 * it rather than being stepped over.
 */

import { randomUUID } from 'crypto';
import * as path from 'path';
import { Client } from './client';
import { Outbox, OutboxEntry } from './outbox';
import { SendRecord } from './record';
import { ImportTarget } from './sessions';
import { TransportError } from './transport';
import { eventFromWrite, firstCursorFromCheck, isReplay, isWellFormedCursor } from './cursor';
import { Datum, clauseValue, formatCursor, headName, isSym } from './wire';

/*
 * WHAT THE CORE SAID ON THE OTHER STREAM, short enough to put in a
 * sentence. Empty when it said nothing, so the sentence reads normally
 * in the ordinary case.
 */
export function aside(stderr: string): string {
  const text = stderr.trim();
  if (text.length === 0) {
    return '';
  }
  const first = text.split('\n').slice(0, 3).join(' / ');
  return `. It said: ${first.length > 300 ? `${first.slice(0, 300)}...` : first}`;
}

export type SaveStatus = 'saved' | 'replayed' | 'pending' | 'refused' | 'blocked';

/*
 * What to do with an entry the store has answered. It is handed in
 * rather than assumed, so that the one place which decides the order --
 * record first, remove second -- is the one the caller names.
 *
 * ⚠️ IT MAKES THE CHOICE EXPLICIT; IT DOES NOT ENFORCE THE ORDER. Any
 * caller can pass `(req, cursor) => outbox.resolve(req, cursor)` and
 * remove the entry with nothing recorded -- the cells in this file do
 * exactly that on purpose, because what they are about is the sending
 * and not the recording. Having no default means nobody gets that
 * behaviour without writing it down; a guarantee that the record is
 * always written first would have to live somewhere this signature
 * cannot reach.
 */
/*
 * ⚠️ WHAT THE STORE SAID, NOT JUST WHERE IT LEFT THE WRITER.
 *
 * This was `(req, cursor: string | null)`, and `null` was used for two
 * things that could not be less alike: an answer that did not name a
 * record, and A REFUSAL. The settler turned the null into an empty
 * cursor, recorded the answer as an acknowledgement, and dequeued --
 * so a save the store REJECTED came out as `draft: false`, nothing
 * pending, `unresolved: false`, and the user's bytes stopped being
 * reported as unsent work anywhere. The whole `mismatch` path in
 * `Saving` was unreachable, because the settler passed `false` for it.
 * Reproduced end to end in review.
 *
 * Three verdicts, because there are three things the store can have
 * said, and each demands something different of the file and the queue.
 */
export type Settlement =
  | { verdict: 'confirmed'; cursor: string }
  /*
   * ⚠️ SOMEBODY ELSE FINISHED IT, AND SAID WHERE. `(error
   * resolved-executed (event ("w" . 6)))` is an operator's
   * determination that the work was carried out: the store HAS the
   * bytes, and the answer names the record it was written as.
   *
   * It arrives through the refusal path and spent a round classified as
   * a refusal, so nothing was recorded and the block stayed a draft for
   * ever although the work was done. Before that it was recorded as a
   * confirmation with an EMPTY cursor -- wrong in the other direction.
   * It is neither: `confirmed` says this client's write succeeded, and
   * this one says somebody else's did, which is a different thing to
   * read in a report and a different thing to chase when something is
   * wrong. Found by asking the review's own question of the source.
   */
  /*
   * ⚠️ AND IT HAS NO POSITION, WHICH IS THE WHOLE DIFFERENCE FROM
   * `confirmed`. (§13.2)
   *
   * An earlier build read the `(event …)` clause off this answer and
   * recorded it as the position the store confirmed. The core's own
   * documentation says an operator's determination does not recover the
   * original execution's event or bindings -- so that clause, when it is
   * there at all, is not the position this write landed at, and
   * recording it as one writes a number nobody stood at. A review
   * raised it and the design settled it: the determination is recorded,
   * the position is not, and the next send asks the store where it
   * stands.
   */
  | { verdict: 'executed-by-operator'; note: string }
  | { verdict: 'refused'; unrecognised?: string }
  | { verdict: 'req-mismatch' };

export type Settle = (req: string, settlement: Settlement) => void;

export interface SaveOutcome {
  status: SaveStatus;
  req: string;
  id: string;
  message: string;
  answer: Datum | null;
  /*
   * ⚠️ THIS REFUSAL LEFT THE REQUEST IN THE QUEUE ON PURPOSE.
   *
   * `req-mismatch` is the one refusal no retry can settle -- the store
   * holds a different request under this id -- so the entry stays for a
   * person to look at and the record beside the file is marked
   * unresolved. `drain` has to be able to tell that apart from an entry
   * that is still there because the answer COULD NOT BE RECORDED, which
   * it reports in quite different words. It used to work it out from
   * "refused and still queued", and a reading derived that way is one
   * more thing to be wrong about; this is the sender saying what it did.
   */
  keptForAPerson?: true;
  /*
   * ⚠️ THE ENTRY NEVER REACHED THE QUEUE, AND A NUMBER WAS ALREADY
   * SPENT ON IT. (§13.1, I7)
   *
   * The sequence is taken and written down BEFORE anything is queued,
   * so that a send can never carry a number nobody recorded. The cost
   * of that order is this case: if the queue write then fails, the
   * record beside the file says a send is out and there is no entry
   * anywhere that could answer for it -- the block becomes a draft it
   * can never stop being.
   *
   * Nothing was sent, so the number can safely be given back, and the
   * caller that holds the record is the one that can do it. This says
   * so rather than leaving it to be inferred from `blocked`, which also
   * means "this store has no cursor" -- a case where the number must
   * NOT be given back, because the entry IS in the queue.
   */
  notQueued?: true;
}

export interface SaverOptions {
  newRequestId?: () => string;
  now?: () => number;
  /*
   * WHAT THE RECORD BESIDE A FILE SAYS ABOUT SENDS THAT HAVE ALREADY
   * BEEN CONFIRMED. (§13, R8)
   *
   * ⚠️ IT IS READ BEFORE EVERY TRANSMISSION, not once when the entry was
   * made. A queue can sit for a long time -- a store that was
   * unreachable, a window that was closed, a takeover carrying work in
   * -- and in that time another window may have saved the same file and
   * had it confirmed. Sending then would put older bytes over newer
   * ones, and the answer would arrive carrying a number below the
   * high-water mark, where it settles without becoming the baseline:
   * the work would look sent and would not be.
   *
   * Absent, no check is made. That is the same deliberate door the bare
   * settler leaves open for the cells about sending, and the census in
   * `awaiting.test.ts` keeps the shipping path on the wiring that
   * passes it.
   */
  baselineOf?: (file: string) => { highWater: number } | null;
  /*
   * WHETHER THIS REQUEST HAS BEEN RETIRED. (§13, r4-2)
   *
   * ⚠️ THE CHECK IS MADE IN FRONT OF THE TRANSMISSION, wherever the
   * entry came from. Cancelling writes a tombstone rather than removing
   * an entry, because removing one removes it from ONE queue: the same
   * request can sit in a dead session's file no takeover has reached,
   * and the next takeover carries the cancelled work back in.
   *
   * ⚠️ AND IT PARKS RATHER THAN DISCARDS. Whoever wrote the tombstone
   * did the bookkeeping that goes with a cancellation -- releasing the
   * send's number beside the file. A copy found later has no business
   * doing that again; what it must do is not send.
   */
  retired?: (record: SendRecord) => { known: true; retired: boolean } | { known: false };
}

/*
 * `unknown` IS THE ONLY REFUSAL THAT MEANS "ASK AGAIN". The core's
 * README is explicit that it never guesses: every other named refusal is
 * a determination, and `unknown` is the absence of one.
 */
function saysNobodyKnows(datum: Datum): boolean {
  return headName(datum) === 'error' && Array.isArray(datum) && datum.length >= 2 && (isSym(datum[1], 'unknown') || isSym(datum[1], 'working-unavailable'));
}

function saysAnOperatorSettledIt(datum: Datum): boolean {
  return (
    headName(datum) === 'error' &&
    Array.isArray(datum) &&
    datum.length >= 2 &&
    isSym(datum[1], 'resolved-executed')
  );
}

/*
 * EVERY ANSWER THAT REACHES A SETTLEMENT, AND WHICH VERDICT IT IS.
 *
 * ⚠️ THIS TABLE IS THE CLASSIFIER, not a comment beside one. A rule kept
 * in somebody's head has no way to notice the next answer that arrives:
 * `resolved-executed` sat in the refusal branch for a round, and the
 * cell that covered it asserted only a status and a count -- never the
 * file, which was the one place the mistake showed.
 *
 * WHAT NEVER REACHES HERE, and why keeping the entry is right for each:
 * a transport error (nothing was sent, or nothing came back); an `ok`
 * that names no record (the store accepted something this client cannot
 * carry forward); an `ok` naming a record whose cursor this client
 * cannot spell (same); a non-zero exit with nothing said (no answer at
 * all); and `(error unknown ...)` (the core's README is explicit that it
 * never guesses -- `unknown` is the ABSENCE of a determination). Their
 * common shape is that nobody knows what happened, and the safe reading
 * of that is to keep the request and let it be retried.
 */
/*
 * ⚠️ THE NAMES CAME FROM THE CORE, NOT FROM THE CELLS.
 *
 * The first version of this table had seven rows and I had drawn them
 * from the cells that happened to exercise a refusal. Reading the pinned
 * core instead (`grep "(list 'error '<kind>"` over its sources) found
 * TWENTY-FIVE kinds, several of them produced on the write path and
 * therefore reachable by a save. A list of what our own cells have seen
 * is not a list of what the other side can say.
 *
 * Everything below is classified because somebody looked at where the
 * core produces it. The ones that are not a write's answer at all are in
 * `NOT_A_WRITES_ANSWER`, each with the function and line that makes it.
 * A kind in neither list falls to `refused` WITH A MARK, and a cell
 * reddens on the mark: the core growing a name is a thing to look at,
 * not a thing to guess about.
 */
const REFUSALS: Record<string, 'req-mismatch' | 'executed-by-operator' | 'refused'> = {
  /*
   * The one refusal no retry can settle, and the one that says somebody
   * else already did the work.
   */
  'req-mismatch': 'req-mismatch',
  'resolved-executed': 'executed-by-operator',
  /*
   * The write did not happen, and the core says why. Each of these is
   * produced on the write path in the pinned core: `check-expectation`
   * (no-subject, deleted, changed), `write-outcome->answer` (refused,
   * not-written), `run-items!` (malformed-intent, receipt-timetable),
   * `write-batch!` (no-view), `ord-for` (unknown-sibling),
   * `cursor-unreachable`, and the argument parser (bad-request).
   */
  changed: 'refused',
  'cursor-unreachable': 'refused',
  'working-version-changed': 'refused',
  'stale-baseline': 'refused',
  'no-draft': 'refused',
  'invalid-working-baseline': 'refused',
  'derived-field': 'refused',
  'mode-mismatch': 'refused',
  'bad-source': 'refused',
  'projection-invalid': 'refused',
  'name-exists': 'refused',
  'operation-packet-unavailable': 'refused',
  'unsupported-printer-version': 'refused',
  'bad-request': 'refused',
  /*
   * A COMMIT THAT NAMED VERSIONS THE DRAFTS NO LONGER HAVE. `complete-plan!`
   * (store.ss) recomputes each named version before it runs anything and
   * answers this when one disagrees; `working-restore!` (working.ss) makes
   * the same name for the same reason. The extension issues `commit`
   * (saver.ts, extension.ts), so a save can be answered with it: the write
   * did not happen and the caller has to read the drafts again.
   */
  'consumes-version-mismatch': 'refused',
  'malformed-intent': 'refused',
  'no-subject': 'refused',
  deleted: 'refused',
  refused: 'refused',
  'not-written': 'refused',
  'receipt-timetable': 'refused',
  'no-view': 'refused',
  'unknown-sibling': 'refused',
  'doc-must-be-top-level': 'refused',
  /*
   * Reachable with any verb, and both mean the write did not happen:
   * the store could not be addressed, or the id names nothing.
   */
  'no-store': 'refused',
  'unknown-id': 'refused',
  internal: 'refused'
};

/*
 * ⚠️ KINDS THE CORE HAS THAT A WRITE'S ANSWER IS NOT, each with where it
 * is made in the pinned core (manifest self md5
 * 0229f9fa246b99986564728d18beefce). The reason is the provenance, not a
 * guess about intent: if the grep does not find it, the row says so
 * rather than inventing a story.
 *
 * ⚠️ AND THIS TABLE IS A STOPGAP, said here so it is not mistaken for
 * knowledge. Which refusals a given verb can produce is a fact about the
 * CORE, and the extension is deducing it by reading someone else's
 * source. The main session has put it on the core's queue: a
 * machine-readable table of refusal kinds per verb, generated from the
 * source and pinned by the core's own cells. When that exists, this list
 * is derived from it and stops being a judgement of mine.
 */
export const NOT_A_WRITES_ANSWER: Record<string, string> = {
  'working-unavailable': 'working.ss: uncertain W storage result; handled by nobodyKnows before settlement',
  'eval-value': 'eval-worker.ss: local evaluator value serialization',
  'eval-exception': 'eval-worker.ss: local evaluator exception',
  'eval-context': 'eval-worker.ss: local evaluator context',
  'eval-denied': 'eval-worker.ss: local evaluator capability refusal',
  'launcher-unavailable': 'ffi.ss: local executable launch',
  'transport-store-mismatch': 'rpc-worker.ss: rejected socket envelope before dispatch',
  'unknown-tag': 'store.ss: historical query cut lookup',
  'tag-unsettled': 'store.ss: historical query cut lookup',
  'cut-unavailable': 'store.ss: historical query cut validation',
  'already-initialised': 'store-init!, store.ss:2466 -- initialising a store, not writing to one',
  'foreign-writer': 'store-init!, store.ss:2469 -- as above',
  'ambiguous-identity': 'match-by-signature, project.ss:579 -- the markdown import path',
  'position-mismatch': 'match-sections, project.ss:513 -- the markdown import path',
  'would-delete': 'import-md, project.ss:280 -- the markdown import path',
  'invalid-candidate': 'publish-validated!, log.ss:3593 -- publication, not a block write',
  'no-candidate': 'verb-table, rpc.ss:459 -- dispatch, before any verb runs',
  'unknown-verb': 'rpc-dispatch-parsed, rpc.ss:685 -- dispatch, before any verb runs',
  'no-such-intent': 'resolve-from, store.ss:2033 -- resolving an intent by name, not writing',
  /*
   * ⚠️ READ FROM THE W DELIVERY (archive/theourgia-code-delivery-w-2026-09-17-r1),
   * not from the core the md5 above names -- that manifest is an older cut and
   * this kind does not exist in it.
   */
  'unknown-version':
    'working-restore!, working.ss:331 -- `restore` was asked for a version no plan of this ' +
    "writer's froze. It is the only site in the core, and `restore` is not a verb this " +
    'extension sends (client.ts lists write, commit, drafts, discard), so no write can be ' +
    'answered with it',
  unknown:
    'write-outcome->answer, store.ss:1204 -- it IS a write answer, and it is handled before ' +
    'classification: `unknown` is the absence of a determination, so the request is kept and ' +
    'retried rather than settled at all'
};

export function classifyRefusal(datum: Datum): Settlement {
  /*
   * ⚠️ TWO KINDS OF "NOT IN THE TABLE", AND THEY WANT OPPOSITE THINGS.
   *
   * A name this build has not seen is still an `(error ...)`, and that
   * SHAPE is the protocol's word for "the write did not happen" -- so
   * `refused` is a reading with a reason behind it, not a convenience.
   * It is marked, and a cell feeds every refusal the real core can
   * produce through here and fails if any is marked: a core that grows
   * a new refusal name then reddens the suite, which is where that
   * belongs, instead of throwing in front of somebody trying to save.
   * `malformed-intent` was exactly that case -- produced by the core
   * today, absent from the list I first drew up from the cells.
   *
   * An answer that is not an `(error ...)` at all is the other kind:
   * nothing here knows what it is, and guessing is how this batch's
   * worst defects began.
   */
  /*
   * ⚠️ AND A USAGE LINE IS AN ANSWER ON THIS PATH TOO. `(usage (set <id>
   * <field> <value>))` is what a core WITHOUT request tracking says to a
   * write carrying --req and --cursor: the write did not happen, and the
   * reason is that the core is the wrong version. It is not an `(error
   * ...)`, so the first version of this classifier threw on it -- and
   * the suite went red on the cell that has covered it all along, which
   * is the whole argument for not throwing at answers that exist.
   */
  if (headName(datum) === 'usage') {
    return { verdict: 'refused' };
  }
  if (headName(datum) !== 'error' || !Array.isArray(datum) || datum.length < 2) {
    throw new Error(
      'an answer reached the settler that this client cannot classify: it is not a refusal and ' +
        'was not accepted either'
    );
  }
  const name = isSym(datum[1]) ? (datum[1] as { name: string }).name : '';
  const known = REFUSALS[name];
  if (known === 'executed-by-operator') {
    /*
     * ⚠️ WHETHER IT NAMED A RECORD NO LONGER DECIDES ANYTHING. An
     * earlier build treated the answer as settleable only when it
     * carried an `(event …)` clause, and recorded that clause as the
     * position -- which the core says is not recoverable from an
     * operator's determination. So the clause is carried as a note for
     * a person to read and nothing is derived from it.
     */
    return { verdict: 'executed-by-operator', note: describeRefusal(datum) };
  }
  if (known === 'req-mismatch') {
    return { verdict: 'req-mismatch' };
  }
  return known === 'refused' ? { verdict: 'refused' } : { verdict: 'refused', unrecognised: name };
}

function describeRefusal(datum: Datum): string {
  /*
   * A USAGE LINE IS THE CORE SAYING IT DID NOT UNDERSTAND THE REQUEST,
   * and it is the answer a core that predates request tracking gives to
   * a tracked write: `(usage (set <id> <field> <value>))`, with no
   * mention of --req or --cursor. Rendering it through the branch below
   * produced "the core refused the write: refused", which says nothing
   * at all -- and the thing it was failing to say is that the core is
   * the wrong version, which takes a while to work out by hand.
   */
  if (headName(datum) === 'usage') {
    return (
      'the core rejected this request with its usage line. Check the version at ' +
      'theourgia.corePath and the arguments sent: a core without request tracking answers ' +
      'this way to a save carrying --req and --cursor, and so does a core that got an ' +
      'argument it did not expect'
    );
  }
  if (!Array.isArray(datum) || datum.length < 2) {
    return 'the core refused the write';
  }
  const name = isSym(datum[1]) ? (datum[1] as { name: string }).name : 'refused';
  const current = clauseValue(datum, 'current');
  if (name === 'changed' && current !== undefined) {
    return 'the block changed in the store since it was opened';
  }
  return `the core refused the write: ${name}`;
}

/*
 * One chain per outbox file, for the whole process. Keyed by the
 * resolved path so that two spellings of one file do not get two chains.
 */
const queues = new Map<string, Promise<void>>();

/*
 * WHICH ENTRY GOES NEXT. (§13, r3-4)
 *
 * ⚠️ STILL ONE AT A TIME. What changes is only which one: a `parked`
 * entry is stepped over, and so is any entry for a block that has a
 * parked entry in front of it -- because within a block the order is
 * what makes a second save mean "and then this". Other blocks carry on.
 *
 * ⚠️ AND A `pending` ENTRY IS NOT STEPPED OVER. Nobody knows whether the
 * store applied it, so nothing may be composed against a position that
 * may have moved; the loop stops at it, which is the behaviour this
 * batch inherited and does not change.
 */
export function nextRunnable(entries: OutboxEntry[]): OutboxEntry | undefined {
  const parkedBlocks = new Set<string>();
  for (const entry of entries) {
    const block = entry.record?.blockId ?? entry.id;
    if (entry.state === 'parked') {
      parkedBlocks.add(block);
      continue;
    }
    if (parkedBlocks.has(block)) {
      continue;
    }
    return entry;
  }
  return undefined;
}

export class Saver {
  private readonly client: Client;
  private readonly outbox: Outbox;
  private readonly settle: Settle & { storeHash?: string };
  private readonly newRequestId: () => string;
  private readonly now: () => number;
  private readonly baselineOf?: (file: string) => { highWater: number } | null;
  private readonly retired?: (
    record: SendRecord
  ) => { known: true; retired: boolean } | { known: false };
  private bootstrapProblem: string | null = null;

  constructor(client: Client, outbox: Outbox, settle: Settle, options: SaverOptions = {}) {
    /*
     * ⚠️ IF THE SETTLER KNOWS WHICH QUEUE IT IS FOR, IT HAS TO BE THIS
     * ONE.
     *
     * `settlerFor` checks itself against its store; nothing checked it
     * against the SAVER. These are two arguments here, so a settler
     * correctly built for one store could be handed to a Saver over
     * another, and then every answer this queue receives is settled
     * against that one: reproduced in review with the same request id in
     * both queues -- one store's answer removed the other's entry and
     * wrote its cursor beside the other's file.
     *
     * The test is object identity, not paths: the settler must hold the
     * very queue this Saver locks and reloads, not another object over
     * the same file, because two objects over one file are two copies
     * and writing one back erases what the other holds.
     *
     * ⚠️ A BARE CALLBACK IS ACCEPTED, and that is deliberate rather than
     * overlooked: the cells in this file pass one on purpose, since what
     * they are about is the sending. Every settler the extension builds
     * comes from `settlerFor`, which attaches its queue, so the path
     * that ships is always checked. What this cannot check is
     * STALENESS -- whether the queue object is one whose contents have
     * been superseded -- because that is a property of when it is used,
     * not of what it was built with.
     */
    const carried = (settle as { queue?: Outbox }).queue;
    if (carried !== undefined && carried !== outbox) {
      throw new Error(
        `a saver over ${outbox.path} was given a settler built for ${carried.path}; the answers ` +
          'this queue receives would be settled against another one'
      );
    }
    this.client = client;
    this.outbox = outbox;
    this.settle = settle;
    this.newRequestId = options.newRequestId ?? (() => randomUUID());
    this.now = options.now ?? (() => Date.now());
    this.baselineOf = options.baselineOf;
    this.retired = options.retired;
  }

  public get pendingCount(): number | null {
    return this.outbox.pendingCount;
  }

  public get cursor(): string | null {
    return this.outbox.cursor;
  }

  public get blockedBecause(): string | null {
    return this.bootstrapProblem;
  }

  /*
   * THE ENTRY IS WRITTEN DOWN, THEN THE QUEUE RUNS. Both steps are
   * inside the same serialisation, so two saves arriving together are
   * enqueued in the order they arrived and sent in that order.
   */
  public save(id: string, field: string, payload: string): Promise<SaveOutcome> {
    return this.serialise(async () => {
      const cursor = await this.ensureCursor();
      if (cursor === null) {
        return {
          status: 'blocked' as const,
          req: '',
          id,
          message: this.bootstrapProblem ?? 'this store has no cursor to write against',
          answer: null
        };
      }
      const entry: OutboxEntry = {
        req: this.newRequestId(),
        cursor,
        id,
        field,
        payload,
        state: 'queued',
        createdAt: this.now(),
        lastError: null,
        importedBy: null
      };
      this.outbox.enqueue(entry);
      const outcomes = await this.drain();
      const mine = outcomes.find((o) => o.req === entry.req);
      return (
        mine ?? {
          status: 'pending' as const,
          req: entry.req,
          id,
          message: 'the save is queued behind an earlier one whose outcome is unknown',
          answer: null
        }
      );
    });
  }

  /*
   * THE SEND THE RECORD DESCRIBES. (§13.1)
   *
   * ⚠️ NOT IMPLEMENTED YET, AND ADDED BESIDE `save` RATHER THAN
   * REPLACING IT. This is the skeleton step: the signature lands so
   * that §13's cells compile and can be read as red before the
   * behaviour exists. Changing `save` in place would take every cell
   * that calls it with three arguments down with it, and a suite that
   * does not compile is not a red reading -- it is no reading at all.
   *
   * `save` and its cells go when this is implemented, together, with
   * each retired assertion mapped to a named successor. The name is
   * `submit` rather than `send` because `send` is already the private
   * step that puts one entry on the wire, and two methods a letter
   * apart in one class is a defect waiting for a tired reader.
   *
   * What it will do: check `record.store` against the store this Saver
   * was built for, refuse and report if they differ, put the record in
   * the queue as the entry itself, bind a cursor to that entry before
   * its first transmission, and drain.
   */
  public submit(record: SendRecord): Promise<SaveOutcome> {
    return this.serialise(async () => {
      /*
       * ⚠️ THE SECOND PLACE THIS IS ASKED, AND ON PURPOSE.
       *
       * The acceptance compared the file's store with this window's
       * before it made the record. This asks again, of the record and
       * of the queue it is about to be written into, because a rule
       * kept at one end has no way to notice a second entrance -- and
       * this batch has already been bitten by exactly that (the same
       * repair applied where the finding pointed instead of everywhere
       * the shape was).
       *
       * The binding comes off the settler, which `settlerFor` has
       * already checked against the queue path for this session and
       * store. A bare callback carries none, which is the same
       * deliberate door the queue-identity check leaves open for the
       * cells about sending -- and the census in `awaiting.test.ts`
       * keeps the shipping path on `settlerFor`.
       */
      const bound = this.settle.storeHash;
      if (bound !== undefined && bound !== record.storeHash) {
        return {
          status: 'blocked' as const,
          req: record.req,
          id: record.blockId,
          message:
            `this save belongs to ${record.store}, and this queue is for another store; ` +
            'it has not been queued here',
          answer: null
        };
      }
      const cursor = await this.ensureCursor();
      if (cursor === null) {
        return {
          status: 'blocked' as const,
          req: record.req,
          id: record.blockId,
          message: this.bootstrapProblem ?? 'this store has no cursor to write against',
          answer: null
        };
      }
      /*
       * THE ENTRY IS THE RECORD, plus the lifecycle the queue keeps
       * around it. `id`, `field` and `payload` are still written for
       * the sake of a record this build wrote being readable by the one
       * before it; what SENDS reads the record when there is one.
       */
      const entry: OutboxEntry = {
        req: record.req,
        cursor,
        id: record.blockId,
        field: record.intent.field,
        payload: record.intent.body,
        state: 'queued',
        createdAt: this.now(),
        lastError: null,
        importedBy: null,
        record
      };
      try {
        this.outbox.enqueue(entry);
      } catch (e) {
        return {
          status: 'blocked' as const,
          req: record.req,
          id: record.blockId,
          message:
            `the save could not be written to the queue at ${this.outbox.path}, so it was not ` +
            `sent: ${String(e)}`,
          answer: null,
          notQueued: true as const
        };
      }
      const outcomes = await this.drain();
      const mine = outcomes.find((o) => o.req === entry.req);
      return (
        mine ?? {
          status: 'pending' as const,
          req: entry.req,
          id: record.blockId,
          message: 'the save is queued behind an earlier one whose outcome is unknown',
          answer: null
        }
      );
    });
  }

  public retry(): Promise<SaveOutcome[]> {
    return this.serialise(() => this.drain());
  }

  /*
   * ⚠️ A TAKEOVER'S ENTRIES ARRIVE THROUGH THE SAME LOCK AS A SAVE.
   *
   * The import used to be handed a bare `Outbox` and told the comment
   * "this is held by the Saver's serial chain". It was not: a save in
   * flight answers, writes ITS copy of the queue back, and the imported
   * entries are gone -- while the source is already marked as having
   * handed them over, so no later takeover offers them again. The bytes
   * survive in the dead window's file and automatic recovery stops
   * seeing them. Reproduced in review.
   *
   * Going through `serialise` also reloads this copy from disk first,
   * which is the other half: importing into a stale copy writes the
   * stale one back.
   *
   * IT IS THE SAME `serialise` THAT `save` AND `retry` USE, keyed by the
   * queue's path, so an import and a save over one file wait for each
   * other whichever objects they were reached through.
   */
  public adopt<T>(work: (into: ImportTarget) => T): Promise<T> {
    return this.serialise(async () =>
      work({
        has: (req: string) => this.outbox.find(req) !== undefined,
        adopt: (entry: OutboxEntry) => this.outbox.enqueue(entry)
      })
    );
  }

  /*
   * THE SERIALISATION BELONGS TO THE QUEUE, NOT TO THIS OBJECT. A chain
   * held in an instance field serialises that instance and nothing else,
   * and this instance is not the only one: the extension builds a new
   * Saver over the SAME outbox file every time a setting changes, and
   * the old one may still have a request in flight. Two Savers, one
   * file, two requests on the wire -- and the second carries a cursor
   * the first is about to move.
   *
   * So the chain is looked up by the outbox's path. Every Saver over one
   * file waits behind the same promise, whatever object made it.
   */
  private serialise<T>(work: () => Promise<T>): Promise<T> {
    const key = path.resolve(this.outbox.path);
    const before = queues.get(key) ?? Promise.resolve();
    /*
     * THE FILE IS THE QUEUE; THIS OBJECT IS A COPY OF IT. Waiting for
     * the previous holder of the lock is not enough -- what that holder
     * wrote is on disk, and this Saver's Outbox still holds whatever it
     * read when it was built. The extension makes a new Saver, over a
     * new Outbox, on every settings change, and the old one may add a
     * pending entry after the new one loaded: writing this copy back
     * then ERASES it, which is the one thing the outbox exists to stop.
     *
     * So the copy is refreshed from the file after taking the lock and
     * before doing anything with it.
     */
    const reloaded = () => {
      this.outbox.load();
      return work();
    };
    const next = before.then(reloaded, reloaded);
    queues.set(
      key,
      next.then(
        () => undefined,
        () => undefined
      )
    );
    return next;
  }

  private async ensureCursor(): Promise<string | null> {
    const known = this.outbox.cursor;
    if (known !== null) {
      return known;
    }
    const answer = await this.client.request('check', []);
    if (!answer.ok || answer.answers.length === 0) {
      this.bootstrapProblem = 'the store did not answer `check`, so this client has no cursor to write against';
      return null;
    }
    const first = firstCursorFromCheck(answer.answers[0]);
    if (!first.ok) {
      this.bootstrapProblem =
        first.reason === 'many-writers'
          ? `this store has ${first.writers.length} writers and the core does not yet say which is local; ` +
            'writing from here is not supported in this batch'
          : 'this store reports no writer, so there is nothing to write against';
      return null;
    }
    /*
     * A CURSOR THE CORE WOULD NOT PARSE IS NOT SENT. `--cursor` is
     * parsed by shape and anything else is `malformed-cursor`; composing
     * one out of a writer name with a colon in it would produce a save
     * refused for a reason that has nothing to do with the save.
     */
    if (!isWellFormedCursor(first.cursor)) {
      this.bootstrapProblem = `this store reports a writer this client cannot spell a cursor for: ${first.cursor}`;
      return null;
    }
    this.bootstrapProblem = null;
    this.outbox.setCursor(first.cursor);
    return first.cursor;
  }

  /*
   * The queue is walked from the front and stops at the first entry
   * whose outcome is unknown. Everything behind it was composed against
   * a cursor that entry is about to move.
   */
  private async drain(): Promise<SaveOutcome[]> {
    const outcomes: SaveOutcome[] = [];
    for (;;) {
      const entries = this.outbox.entries;
      if (entries.length === 0) {
        return outcomes;
      }
      const entry = nextRunnable(entries);
      if (entry === undefined) {
        return outcomes;
      }
      /*
       * ⚠️ AND THE BASELINE IS RE-READ BEFORE THIS ONE GOES OUT. (R8)
       *
       * Not to count the reads -- to decide. A send whose number is
       * below what the record beside its file already has confirmed has
       * been overtaken: transmitting it would put older bytes over
       * newer ones, and its answer would settle without becoming the
       * baseline, so the work would look sent and would not be.
       */
      const withdrawn = this.hasBeenRetired(entry);
      if (withdrawn !== null) {
        this.outbox.markParked(entry.req, withdrawn);
        continue;
      }
      const overtaken = this.hasBeenOvertaken(entry);
      if (overtaken !== null) {
        this.outbox.markParked(entry.req, overtaken);
        continue;
      }
      /*
       * THE ENTRY IS MARKED AS GOING OUT BEFORE IT GOES OUT, for the
       * same reason it was written down before it was sent: after this
       * line the store may have seen it, and nothing may change what it
       * says.
       */
      this.outbox.aboutToSend(entry.req, this.outbox.cursor);
      const current = this.outbox.find(entry.req) ?? entry;
      const outcome = await this.send(current);
      /*
       * ⚠️ AND IT STOPS IF THE ENTRY IS STILL HERE.
       *
       * This loop had exactly one way out: an answer of `pending`. It
       * took `entries[0]`, sent it, and went round again on the
       * assumption that the entry was gone by then -- and NOTHING
       * CHECKED THAT IT WENT. The settler is allowed to decline: when
       * the record beside the file cannot be written, `recordAnswer`
       * KEEPS the entry so the request stays retryable rather than being
       * lost, which is the safe direction and is deliberate. But then
       * the store has answered, the outcome is not `pending`, and the
       * same entry is at the front of the queue again -- so the same
       * request goes to the store on every turn, for ever, inside a
       * command the user is awaiting.
       *
       * MEASURED, NOT SUPPOSED: with a settler that records nothing, a
       * single `save` sent the same request five times and stopped only
       * because the scripted core ran out of `ok` answers. A real store
       * does not run out.
       *
       * THE ENTRY IS MARKED PENDING AND THE OUTCOME SAYS SO, because
       * "saved" would be a report that the work is done about a queue
       * that still holds it -- the count in the status bar and the
       * sentence after a retry would disagree with each other.
       */
      outcomes.push(outcome);
      /*
       * AN ANSWER OF `pending` IS ALREADY A REASON TO STOP, AND IT HAS
       * ITS OWN SENTENCE. `send` keeps the entry in that case too, so
       * the check below would be true here as well -- and would replace
       * a message naming what went wrong ("the core exited 255 without
       * saying why") with a general one. Three cells caught that.
       */
      if (outcome.status === 'pending') {
        return outcomes;
      }
      /*
       * AN ENTRY KEPT ON PURPOSE IS NOT AN ENTRY THAT COULD NOT BE
       * RECORDED. The loop still stops -- nothing goes out past it --
       * but the refusal keeps its own words, which name what the store
       * said rather than describing a write that never failed.
       */
      if (outcome.keptForAPerson === true) {
        return outcomes;
      }
      if (this.outbox.find(entry.req) !== undefined) {
        const why =
          'the store answered and the answer could not be recorded beside the file; the request ' +
          'is kept';
        this.outbox.markPending(entry.req, why);
        outcomes[outcomes.length - 1] = { ...outcome, status: 'pending', message: why };
        return outcomes;
      }
    }
  }

  /*
   * ⚠️ A `sent` ENTRY IS NOT AN EXECUTED ONE. It says this client put
   * the request on the wire, not that the store applied it -- the whole
   * reason the queue keeps it. So the check is made for a sent entry
   * too, on every retry.
   */
  /*
   * ⚠️ AND "I COULD NOT LOOK" IS NOT "NOTHING IS RETIRED". The question
   * being decided is whether to SEND; a directory this process may not
   * search reports everything inside it as absent, and reading that as
   * "nobody cancelled this" resends cancelled work.
   */
  private hasBeenRetired(entry: OutboxEntry): string | null {
    const record = entry.record;
    if (record === undefined || this.retired === undefined) {
      return null;
    }
    const asked = this.retired(record);
    if (!asked.known) {
      return 'this request could not be checked against the retired ones, so it was not sent';
    }
    return asked.retired ? 'this request was retired, so it is not sent' : null;
  }

  private hasBeenOvertaken(entry: OutboxEntry): string | null {
    const record = entry.record;
    if (record === undefined || this.baselineOf === undefined) {
      return null;
    }
    const baseline = this.baselineOf(record.file);
    if (baseline === null || baseline.highWater <= record.seq) {
      return null;
    }
    return (
      `a later save of this block has been confirmed since this one was accepted (send ` +
      `${record.seq}, the record beside the file holds ${baseline.highWater}); it is kept for you ` +
      'to look at rather than sent'
    );
  }

  private async send(entry: OutboxEntry): Promise<SaveOutcome> {
    /*
     * ⚠️ WHAT GOES ON THE WIRE COMES FROM THE RECORD WHEN THERE IS ONE.
     *
     * The entry still carries `id`, `field` and `payload` so that a
     * queue written by this build can be read by the one before it --
     * but two places holding the same fact is how this batch lost a
     * save, so at the moment of use there is one: the record if the
     * entry has one, and the old fields only for an entry written
     * before §13, which has no record and takes the legacy path.
     */
    const what =
      entry.record === undefined
        ? { id: entry.id, field: entry.field, payload: entry.payload }
        : { id: entry.record.blockId, field: entry.record.intent.field, payload: entry.record.intent.body };
    const verb = entry.record?.intent.verb === 'commit' ? 'commit' : 'set';
    let args: string[];
    if (verb === 'commit') {
      let selected: {writer:string;version:string};
      try {
        selected=JSON.parse(entry.record?.intent.expectation ?? 'null');
        if (!selected || typeof selected.writer!=='string' || typeof selected.version!=='string') throw new Error('Invalid working selection');
      } catch {
        this.outbox.markPending(entry.req,'The immutable working selection is unreadable');
        return {status:'pending',req:entry.req,id:what.id,message:'The immutable working selection is unreadable; keep the request for recovery',answer:null};
      }
      args=[what.id,'--writer',selected.writer,'--working-version',selected.version,'--req',entry.req,'--cursor',entry.cursor];
    } else args=[what.id,what.field,what.payload,'--req',entry.req,'--cursor',entry.cursor];
    let answer;
    try {
      answer = await this.client.request(verb, args);
    } catch (e) {
      if (e instanceof TransportError) {
        const why = aside(e.detail);
        this.outbox.markPending(entry.req, `${e.message}${why}`);
        return {
          status: 'pending',
          req: entry.req,
          id: what.id,
          message: `${e.message}; the save is kept and can be retried${why}`,
          answer: null
        };
      }
      throw e;
    }

    const datum = answer.answers.length > 0 ? answer.answers[0] : null;

    if (answer.ok) {
      const event = eventFromWrite(answer);
      /*
       * AN `ok` THAT NAMES NO RECORD IS NOT A SAVE THIS CLIENT CAN ACT
       * ON. Every write the core accepts answers with the event it
       * appended -- `(cursor ("w" . 6))` fresh, `(event ("w" . 6))` on a
       * replay -- and an answer with neither leaves the next request
       * with no cursor to be composed against. Dropping the entry would
       * be calling it saved on the strength of the word `ok`, and the
       * next save would then go out against a cursor from before this
       * one, which the store refuses. So the entry stays and this is
       * reported as the defect it is.
       */
      if (event === null) {
        this.outbox.markPending(entry.req, 'the core answered ok without naming a record');
        return {
          status: 'pending',
          req: entry.req,
          id: what.id,
          message:
            'the core accepted the save but did not say which record it wrote, so this client ' +
            'cannot carry the cursor forward; the save is kept and can be retried',
          answer: datum
        };
      }
      const moved = formatCursor(event);
      if (!isWellFormedCursor(moved)) {
        this.outbox.markPending(entry.req, `the core named a record this client cannot spell: ${moved}`);
        return {
          status: 'pending',
          req: entry.req,
          id: what.id,
          message: `the core named the record ${moved}, which is not a cursor the core itself would parse`,
          answer: datum
        };
      }
      this.settle(entry.req, { verdict: 'confirmed', cursor: moved });
      return {
        status: isReplay(answer) ? 'replayed' : 'saved',
        req: entry.req,
        id: what.id,
        message: isReplay(answer)
          ? 'the store had already applied this request'
          : 'saved',
        answer: datum
      };
    }

    if (datum === null) {
      /*
       * THE OTHER STREAM IS THE ONLY EVIDENCE THERE IS. A core that
       * exits without printing an answer has usually said why on stderr,
       * and a message that drops it leaves whoever reads the report with
       * an exit code and nothing to act on. This happened once in a full
       * run and could not be reproduced; the reason it could not be
       * looked into afterwards is that this sentence did not carry it.
       */
      const why = aside(answer.stderr);
      this.outbox.markPending(entry.req, `the core exited ${answer.rc} without saying why${why}`);
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message:
          `the core exited ${answer.rc} without an answer; the save is kept and can be retried${why}`,
        answer: null
      };
    }

    if (saysNobodyKnows(datum)) {
      this.outbox.markPending(entry.req, 'the store cannot say whether the request ran');
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message: 'the store cannot say whether this save ran; it is kept and can be retried',
        answer: datum
      };
    }

    /*
     * ⚠️ A REFUSAL IS CARRIED AS ONE, and the one refusal that means
     * something different to the file is named: `req-mismatch` says the
     * store holds a DIFFERENT request under this id, which nobody here
     * can settle by retrying, so the record is marked for a person and
     * the entry is kept. Every other refusal is the store declining this
     * write; the bytes stay a draft and the entry stays with them.
     */
    const settlement = classifyRefusal(datum);
    this.settle(entry.req, settlement);
    if (saysAnOperatorSettledIt(datum)) {
      return {
        status: 'replayed',
        req: entry.req,
        id: what.id,
        message: 'an operator recorded that this request had already been carried out',
        answer: datum
      };
    }
    return {
      status: 'refused',
      req: entry.req,
      id: what.id,
      message: describeRefusal(datum),
      answer: datum,
      ...(settlement.verdict === 'req-mismatch' ? { keptForAPerson: true as const } : {})
    };
  }
}
