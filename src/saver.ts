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
import { Outbox, OutboxEntry, Receipt } from './outbox';
import { behindNotice } from './status';
import { SendRecord } from './record';
import { ImportTarget } from './sessions';
import { TransportError } from './transport';
import { eventFromWrite, firstCursorFromCheck, isReplay, isWellFormedCursor } from './cursor';
import { RETRY_OUTBOX } from './commands';
import { Datum, answerOf, formatCursor, isList, isSym, wire } from './wire';
import { nextStamp } from './durability';

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
 * NOTE: IT MAKES THE CHOICE EXPLICIT; IT DOES NOT ENFORCE THE ORDER. Any
 * caller can pass `(req, cursor) => outbox.resolve(req, cursor)` and
 * remove the entry with nothing recorded -- the cells in this file do
 * exactly that on purpose, because what they are about is the sending
 * and not the recording. Having no default means nobody gets that
 * behaviour without writing it down; a guarantee that the record is
 * always written first would have to live somewhere this signature
 * cannot reach.
 */
/*
 * NOTE: WHAT THE STORE SAID, NOT JUST WHERE IT LEFT THE WRITER.
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
   * NOTE: SOMEBODY ELSE FINISHED IT, AND SAID WHERE. `(error
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
   * NOTE: AND IT HAS NO POSITION, WHICH IS THE WHOLE DIFFERENCE FROM
   * `confirmed`. (section 13.2)
   *
   * An earlier build read the `(event ...)` clause off this answer and
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
   * NOTE: THIS REFUSAL LEFT THE REQUEST IN THE QUEUE ON PURPOSE.
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
   * NOTE: THE ENTRY NEVER REACHED THE QUEUE, AND A NUMBER WAS ALREADY
   * SPENT ON IT. (section 13.1, I7)
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
  /*
   * WHO LANDED AFTER THIS SAVE'S BASELINE, as a sentence, when the store
   * said so. Absent when it did not: a notice on every save would be
   * noise with nothing behind it.
   */
  behind?: string;
  /*
   * WHAT THE QUEUE COULD NOT PROMISE, as a sentence: the entry was written
   * but its directory could not be flushed, so it may not survive a power
   * cut. It rides on the save like `behind` does, and is shown after the
   * save's own notice, because the save itself stands (queue item 3).
   */
  durability?: string;
}

export interface SaverOptions {
  /*
   * WHERE A QUEUE WRITE'S DURABILITY WARNING GOES WHEN NO SAVE IS WAITING
   * FOR IT. (queue item 22, design v4) A mutation of the queue whose rename
   * landed and whose directory flush failed counts as done and returns a
   * warning; the awaited save carries its own entry's on its outcome, and
   * every other one is handed here: the window's sink, which outlives this
   * Saver (a rebuild replaces the Saver, not the sink) and is shown after
   * each command and save.
   *
   * NOTE: REQUIRED, not defaulted (ruled 2026-09-26, Q3): a Saver built
   * without somewhere to put these would drop them in silence, and the
   * compiler is the check that none is.
   *
   * NOTE: `at` IS WHEN THE WARNING WAS PRODUCED. (queue item 46) A warning
   * handed on the moment its write returns leaves it out; one held on a
   * drain's outcome and handed on later passes the stamp it took then, so the
   * sink keeps the newest by production, not by arrival.
   */
  durability: (file: string, text: string, at?: number) => void;
  newRequestId?: () => string;
  now?: () => number;
  /*
   * WHAT THE RECORD BESIDE A FILE SAYS ABOUT SENDS THAT HAVE ALREADY
   * BEEN CONFIRMED. (section 13, R8)
   *
   * NOTE: IT IS READ BEFORE EVERY TRANSMISSION, not once when the entry was
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
   * WHETHER THIS REQUEST HAS BEEN RETIRED. (section 13, r4-2)
   *
   * NOTE: THE CHECK IS MADE IN FRONT OF THE TRANSMISSION, wherever the
   * entry came from. Cancelling writes a tombstone rather than removing
   * an entry, because removing one removes it from ONE queue: the same
   * request can sit in a dead session's file no takeover has reached,
   * and the next takeover carries the cancelled work back in.
   *
   * NOTE: AND IT PARKS RATHER THAN DISCARDS. Whoever wrote the tombstone
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
/*
 * NOTE: AND `transport-unknown` IS THE THIRD, for the same reason and not
 * by analogy. The daemon answers it when the process that owns the
 * store dies while a connection is open (daemon.sc:1215, 1372) -- the
 * request may already have been applied, and the connection is closed
 * before anything can say. NEVER: It must NOT be classified as a refusal:
 * that path resolves the entry out of the queue and releases its send
 * number, which would record "the store declined this" about a write
 * that may have landed.
 */
/*
 * NOTE: AND A BARE `unreadable` IS THE FOURTH, ruled by the main session
 * from the core's semantics on 2026-09-25. `(error unreadable (path ...)
 * (reason ...))` is what the core's catch-all guard (rpc.sc:76) makes of
 * an entry it could not read, WHEREVER in the verb that happened -- and
 * that includes reads after the append, for the answer, the frontier or
 * a snapshot. The answer carries no position, so it does not say whether
 * the write landed. The refusals that do say so have their own heads
 * (`refused-before-reserve`, `(refused writer-unreadable ...)`) and are
 * not this one.
 */
function saysNobodyKnows(datum: Datum): boolean {
  return (
    answerOf(datum, 'error') !== null &&
    Array.isArray(datum) &&
    datum.length >= 2 &&
    (isSym(datum[1], 'unknown') ||
      isSym(datum[1], 'working-unavailable') ||
      isSym(datum[1], 'transport-unknown') ||
      isSym(datum[1], 'unreadable') ||
      isSym(datum[1], 'incomplete'))
  );
}

/*
 * THE REFUSALS THAT MEAN "NOT NOW", AND NOTHING ABOUT THIS WRITE.
 *
 * Two of them come from the store and four from the transport, and they
 * are one family because the thing to do about them is one thing: send
 * the same request again, under the SAME id, on the next drain.
 *
 * From the store: `draining` is a daemon that has been asked to stop and
 * is refusing new work while it finishes what it has (daemon.sc:239);
 * `store-busy` is the store's lock still held by somebody else past the
 * waiting budget (daemon.sc:264).
 *
 * From the transport, and this is the part with a proof behind it: the
 * thin client answers `not-sent` only when it can show that not one byte
 * left. `client.sc` says so in its own words (client.sc:376) -- "A REQUEST THAT
 * DEMONSTRABLY DID NOT LEAVE IS `not-sent`, AND THE PROOF IS A COUNT" --
 * and `theourgia.sc` (`settle`, theourgia.sc:335) relays such an answer unchanged rather than
 * wrapping it, because wrapping a known outcome in `transport-unknown`
 * would replace a known thing with an unknown one. So `connect-failed`,
 * `write-failed`, `serve-start-failed` and `detach-failed` all mean the
 * store never saw these bytes.
 *
 * NOTE: `write-failed` HAS EXACTLY ONE MEANING AT THIS LAYER, and it is
 * worth saying why, because in the core it has two. A write that fails
 * after bytes have gone out is caught by the guard at client.sc:352-354
 * and becomes `(transport-error <errno>)`, which `theourgia.sc`'s
 * `settle` turns into `transport-unknown`; only the zero-byte case
 * (client.sc:385-389, `exchange-on`) arrives here under its own name. NEVER: There is no
 * cell for the two-meaning case because this extension cannot produce
 * that input -- the distinction is made inside the core, and a cell here
 * would be measuring the core's classifier through a keyhole.
 */
export const RETRYABLE_REFUSALS = [
  /*
   * NOTE: `unwritable` (f5ebd58, answers.sc's table, made at rpc.sc's
   * `rpc-dispatch-parsed` for any verb): a write failed and the request had
   * changed NOTHING -- the record was empty, or the answer would be
   * `incomplete`. So the save did not land and sending it again under the
   * same id is safe; it is the store's disk or permissions, which a person
   * fixes, so after RETRY_CAP it is parked with the answer's path and reason
   * (ruled by the main session 2026-09-27; not `unknown`, which is for "the
   * store cannot say", and this one says).
   */
  'unwritable',
  'draining',
  'store-busy',
  'connect-failed',
  'write-failed',
  'serve-start-failed',
  'detach-failed',
  /*
   * NOTE: `serve-path-occupied` IS TRANSIENT BY DEFINITION, and it was in
   * no table at all until a review round asked what happens to it.
   *
   * What occupies the socket path is another daemon, starting or
   * draining: it will either begin answering, in which case the next
   * attempt connects, or exit, in which case the next attempt starts
   * one. Five attempts and still occupied is a stuck path, and the cap
   * then parks it for a person -- which is the right end for it.
   *
   * NEVER: IT DOES NOT STAY IN "UNRECOGNISED". That branch is the default
   * for names this build has never heard of, not a resting place for one
   * it has. Ruled by the main session.
   */
  'serve-path-occupied'
] as const;

/*
 * THE REFUSALS A RETRY CANNOT HELP, BECAUSE THE ANSWER IS A SETTING.
 *
 * NOTE: THESE DO NOT GO IN THE FAMILY ABOVE EVEN THOUGH NOTHING WAS SENT.
 * `store-not-found` is the store directory named by `theourgia.store`;
 * `socket-path-too-long` is the length of the run-root path the socket
 * is computed under, against a constant the operating system fixes at
 * 104 bytes. Neither changes because time passed: five attempts under
 * the cap would be five identical failures and a slower arrival at the
 * same place.
 *
 * So the entry is parked at once, with a sentence naming what to change
 * -- and the way back is the change itself: a configuration change makes
 * this extension try each parked entry once more.
 *
 * NOTE: AND THE RETRY COMMAND (`RETRY_OUTBOX`) DOES THE SAME, because not
 * every member of this family is answered by a setting. An instance
 * mismatch (below) is repaired outside the editor -- the daemon restarted
 * under the right home, or the store adopted -- and nothing about that
 * reaches `onDidChangeConfiguration`. This file used to say there was no
 * recovery command to teach; with that member there has to be one, and it
 * is the command the user already has (`Saver.retryParked`).
 */
export const SETTINGS_REFUSALS: Record<string, string> = {
  'store-not-found':
    'the store directory in theourgia.store does not exist. Point it at a store, or run the ' +
    'core\'s `init` in it; the save is kept and goes again when the setting changes',
  'socket-path-too-long':
    'the socket path computed under THEOURGIA_RUN is longer than the 104 bytes a unix socket ' +
    'name may have. Set THEOURGIA_RUN to a shorter directory; the save is kept and goes again ' +
    'when the setting changes',
  /*
   * NOTE: `absent` (f5ebd58, answers.sc's table: ENOENT or ENOTDIR, the
   * request having changed nothing): a file or directory of the store is not
   * there -- a store moved or cut down under the daemon. No automatic retry:
   * a loop against a moved store is noise (ruled by the main session
   * 2026-09-27). The path from the answer is added to this sentence where it
   * is shown (`whichSettingsRefusal`).
   */
  absent:
    'a file or directory of the store is not there. Check the store directory in theourgia.store; ' +
    'the save is kept and goes again when the setting changes or the retry command is run'
};

/*
 * A STRING CLAUSE OF A REFUSAL, `(<name> "<text>")`, or null. For the
 * answers table's clauses (`path`, `reason`), which say where and why and
 * are shown to a person. Read through the one clause reader (`answerOf`'s
 * `value`), so a clause given twice or not as one value is not shown.
 */
function stringClauseOf(datum: Datum, name: string): string | null {
  const form = answerOf(datum, 'error');
  if (form === null) {
    return null;
  }
  const one = form.value(name);
  return one.read && typeof one.value === 'string' ? one.value : null;
}

/*
 * ", at <path> (<reason>)" from a refusal that names them, or "".
 */
function whereAndWhy(datum: Datum): string {
  const where = stringClauseOf(datum, 'path');
  const why = stringClauseOf(datum, 'reason');
  if (where === null && why === null) {
    return '';
  }
  return `, at ${where ?? '(no path given)'}${why === null ? '' : ` (${why})`}`;
}

/*
 * WHAT AN ANSWER THIS CLIENT DOES NOT RECOGNISE MEANS WHEN THE THIN
 * CLIENT IS THE ONE REFUSING.
 *
 * NOTE: THE SAME RELAY THAT BRINGS `detach-failed` CAN BRING ANY NAME. When
 * a daemon fails to start, the client reads the log it just wrote and
 * answers with the LAST `(error ...)` in it, whatever that is
 * (client.sc:555, `last-error-in`). So the set of names that can arrive this way is the
 * set of names the core can write to a startup log -- which is not a
 * list this extension can hold, and a list it held would be wrong the
 * first time the core learned a new one.
 *
 * The exit code is what separates them: 75 is the client refusing on its
 * own account. An unrecognised name at 75 is therefore a transport-side
 * refusal this build has never heard of, and the honest reading of it is
 * the one this extension already has for an outcome it cannot classify:
 * keep the entry, keep the request id, ask again. NEVER: Not `refused` --
 * that would be recording a determination nobody made.
 */
export const CLIENT_REFUSED_EXIT = 75;

/*
 * NOTE: FIVE IN A ROW, COUNTED IN MEMORY ONLY, AND THE RESET ON RESTART IS
 * DELIBERATE. Persisting the count would mean a user who restarts meets
 * an entry that has used up its allowance and will never be sent again
 * on its own -- which is harder to explain than starting the count
 * over. Both of these conditions are transient by nature: a drain
 * finishes, a lock is released. After a restart, trying once more is
 * the right thing to do. (v224)
 */
export const RETRY_CAP = 5;

function whichRetryableRefusal(datum: Datum): string | null {
  if (answerOf(datum, 'error') === null || !Array.isArray(datum) || datum.length < 2) {
    return null;
  }
  for (const kind of RETRYABLE_REFUSALS) {
    if (isSym(datum[1], kind)) {
      return kind;
    }
  }
  return null;
}

function whichSettingsRefusal(datum: Datum): { kind: string; message: string } | null {
  if (answerOf(datum, 'error') === null || !Array.isArray(datum) || datum.length < 2) {
    return null;
  }
  for (const kind of Object.keys(SETTINGS_REFUSALS)) {
    if (isSym(datum[1], kind)) {
      /*
       * `absent` names the path that is not there; the sentence carries it.
       */
      const where = kind === 'absent' ? stringClauseOf(datum, 'path') : null;
      return { kind, message: where === null ? SETTINGS_REFUSALS[kind] : `${SETTINGS_REFUSALS[kind]} (${where})` };
    }
  }
  const instance = instanceMismatchOf(datum);
  if (instance !== null) {
    return { kind: 'instance', message: instanceMessage(instance) };
  }
  return null;
}

/*
 * AN INSTANCE MISMATCH IS NAMED AT POSITION 2, NOT POSITION 1. (queue 7)
 *
 * The core answers `(error refused (instance machine))` -- quoted from
 * 877f0da by the main session on 2026-09-25: `verify-instance` found the
 * store's recorded identity (machine, device, inode or nonce) different
 * from where it is being written, `reserve-then-write!` refused before
 * the append, and `write-outcome->answer` put the reason after the word
 * `refused`. So the name alone says "the store declined this", which is
 * true, and would settle the save as a permanent refusal -- for a
 * condition a person fixes by restarting a daemon. It is read by the
 * clause at position 2, and joins the settings family: kept, parked, and
 * sent again when the person says so.
 *
 * NOTE: THE OTHER REASONS AT POSITION 2 ARE LEFT WHERE THEY WERE. `refused`
 * carries sixteen of them in 877f0da (integrity, registry-ahead,
 * no-instance, ...); only this family was ruled into the settings
 * family, and the rest stay refusals whose detail now reaches the
 * sentence (`describeRefusal`). Which of them should also be kept for a
 * person is the main session's next ruling, not a guess made here.
 *
 * KEY: WHEN THE CORE RENAMES THIS (its F31: `(error instance-mismatch
 * (what machine))`), THIS SHAPE STAYS RECOGNISED. `theourgia.corePath` can
 * always name an older core, and a reader that dropped the old shape would
 * turn every older core's instance mismatch back into a permanent refusal.
 */
function instanceMismatchOf(datum: Datum): string | null {
  const form = answerOf(datum, 'error', { at: 1, is: 'refused' });
  if (form === null || !isList(datum) || datum.length < 3) {
    return null;
  }
  const at = datum[2];
  if (!isList(at) || !isSym(at[0], 'instance')) {
    return null;
  }
  const what = form.value('instance');
  return what.read && isSym(what.value) ? what.value.name : null;
}

function instanceMessage(what: string): string {
  const where =
    what === 'machine'
      ? 'the store was created under another machine identity -- another THEOURGIA_HOME, or ' +
        'another computer -- than the one its daemon is running under'
      : what === 'device' || what === 'inode'
        ? 'the store directory is not the one the store was created in: it was moved or copied'
        : what === 'nonce'
          ? "the store's instance record does not match its owner: it was restored or copied"
          : `the store's recorded identity does not match where it is being written (${what})`;
  return (
    `the core refused the write: refused (instance ${what}). ${where}. Run the store's daemon ` +
    "under the THEOURGIA_HOME the store was created in, or adopt the store with the core's " +
    `\`adopt\`. The save is kept; once that is done, run "${RETRY_OUTBOX.title}"`
  );
}

/*
 * NOTE: IT ASKS THE CLASSIFIER RATHER THAN THE TABLE. The mark is the
 * product's own way of saying "nobody has looked at this one", and a
 * second reading of the table here would be a copy to keep in step.
 */
function isUnrecognisedRefusal(datum: Datum): boolean {
  if (answerOf(datum, 'error') === null) {
    return false;
  }
  const settlement = classifyRefusal(datum);
  return settlement.verdict === 'refused' && settlement.unrecognised !== undefined;
}

function saysAnOperatorSettledIt(datum: Datum): boolean {
  return (
    answerOf(datum, 'error') !== null &&
    Array.isArray(datum) &&
    datum.length >= 2 &&
    isSym(datum[1], 'resolved-executed')
  );
}

/*
 * EVERY ANSWER THAT REACHES A SETTLEMENT, AND WHICH VERDICT IT IS.
 *
 * NOTE: THIS TABLE IS THE CLASSIFIER, not a comment beside one. A rule kept
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
 * NOTE: THE NAMES CAME FROM THE CORE, NOT FROM THE CELLS.
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
   * (store.sc:3315) recomputes each named version before it runs anything and
   * answers this when one disagrees; `working-restore!` (working.sc:400) makes
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
  internal: 'refused',
  /*
   * NOTE: THIS ONE SHOULD BE UNREACHABLE, AND IT IS A VERDICT ROW ANYWAY.
   *
   * The core refuses a draft-space verb whose writer is unbound
   * (working.sc:116, `writer-required`) so that two agents handed only an
   * actor cannot silently
   * share one draft space. This extension binds a writer on every
   * request -- `THEOURGIA_WRITER`, defaulted to the actor -- so it
   * should never see this. A provenance row would explain the answer
   * away; a verdict row leaves the cell that binds the writer as the
   * thing that fails when the binding breaks. If it does arrive, the
   * write did not happen.
   */
  'writer-required': 'refused'
};

/*
 * NOTE: KINDS THE CORE HAS THAT A WRITE'S ANSWER IS NOT, each with where it
 * is made in the pinned core (theourgia 877f0da, the F46 pin; re-read
 * row by row by plugin-r3 queue item 19). The reason is the provenance, not a
 * guess about intent: if the grep does not find it, the row says so
 * rather than inventing a story.
 *
 * NOTE: AND THIS TABLE IS A STOPGAP, said here so it is not mistaken for
 * knowledge. Which refusals a given verb can produce is a fact about the
 * CORE, and the extension is deducing it by reading someone else's
 * source. The main session has put it on the core's queue: a
 * machine-readable table of refusal kinds per verb, generated from the
 * source and pinned by the core's own cells. When that exists, this list
 * is derived from it and stops being a judgement of mine.
 */
export const NOT_A_WRITES_ANSWER: Record<string, string> = {
  /*
   * NOTE: TWO ANSWERS OF THE THIN CLIENT THIS EXTENSION CANNOT PROVOKE. It
   * never passes `--socket` (the client computes the path from the store
   * and the run root), and it never runs `serve` itself -- the client
   * starts the daemon, and if that start fails what reaches here is the
   * relayed error, not these.
   */
  'bad-socket-path': 'theourgiad.sc:91 -- refuses an empty `--socket`; this extension never passes one',
  /*
   * NOTE: THREE KINDS FIRST READ ON f5ebd58 (the F100b re-pin, 2026-09-27).
   * `incomplete` is a write's answer and is taken before settlement, as
   * `unreadable` is; the other two cannot answer anything this extension
   * sends, and a tripwire cell in refusals.test.ts is red if what it sends
   * ever changes that (`publish`, `--socket`).
   */
  'socket-dir-missing':
    "client.sc, `ensure-daemon!` and `socket-dir-refusal` -- only for a `--socket` other than the " +
    "store's default one; this extension never passes `--socket`",
  'candidate-unreadable':
    "rpc.sc, the `publish` verb's arm (`publish <writer> <segment> <file>`) -- a log segment's " +
    'candidate that cannot be read; this extension never sends `publish`',
  incomplete:
    "rpc.sc, `rpc-dispatch-parsed` through answers.sc's table: `(error incomplete (failed (path ...) " +
    '(reason ...) (errno ...) [(op ...)]) (written <what the request changed>))`, a filesystem failure ' +
    'after the request had changed something. For a save (commit, set) `saysNobodyKnows` takes it ' +
    'before settlement, exactly as `unreadable`: pending, same request id, sent again. The read ' +
    'verbs this extension sends change nothing, so they are answered `absent` or `unreadable` ' +
    'instead. Not the `(incomplete ...)` CLAUSE an ok answer may carry, which is an older, ' +
    'different thing',
  'detach-needs-a-log':
    'theourgiad.sc:173 -- `serve --detach` without a log path; this extension never runs `serve`, the thin ' +
    'client does, and it always names the log',
  'working-unavailable': 'working.sc:220, 423: uncertain W storage result; handled by nobodyKnows before settlement',
  'eval-value': 'eval-worker.sc:169: local evaluator value serialization',
  'eval-exception': 'eval-worker.sc:217 (and eval-supervise.sc:381): local evaluator exception',
  'eval-context': 'eval-worker.sc:224, 231: local evaluator context',
  'eval-denied': 'eval-worker.sc:234: local evaluator capability refusal',
  'launcher-unavailable': 'ffi.sc:344: local executable launch',
  'transport-store-mismatch': 'daemon.sc:1267: rejected socket envelope before dispatch',
  'unknown-tag': 'resolve-cut, store.sc:2007: historical query cut lookup',
  'tag-unsettled': 'resolve-cut, store.sc:2009: historical query cut lookup',
  'cut-unavailable': 'store-diff, store.sc:2026: historical query cut validation',
  'already-initialised': 'store-init!, store.sc:3996 -- initialising a store, not writing to one',
  'foreign-writer': 'store-init!, store.sc:3999 -- as above',
  'ambiguous-identity': 'match-by-signature, project.sc:896 -- the markdown import path',
  'position-mismatch': 'match-sections, project.sc:830 -- the markdown import path',
  'would-delete': 'import-md, project.sc:597 -- the markdown import path',
  'invalid-candidate': 'publish-validated!, log.sc:3965 -- publication, not a block write',
  'no-candidate': 'verb-table, rpc.sc:1054 -- dispatch, before any verb runs',
  'unknown-verb': 'dispatch-verb, rpc.sc:1730 -- dispatch, before any verb runs',
  'no-such-intent': 'resolve-from, store.sc:3561 -- resolving an intent by name, not writing',
  /*
   * NOTE: FIRST READ FROM THE W DELIVERY
   * (archive/theourgia-code-delivery-w-2026-09-17-r1), when the pinned core
   * did not have this kind yet; the pinned core (877f0da) has it now.
   */
  'unknown-version':
    'working-restore!, working.sc:411 -- `restore` was asked for a version no plan of this ' +
    "writer's froze. It is the only site in the core, and `restore` is not a verb this " +
    'extension sends (client.ts lists write, commit, drafts, discard), so no write can be ' +
    'answered with it',
  /*
   * NOTE: THESE THREE ARE ANSWERS TO A WRITE, and they are here for the
   * reason `unknown` and `working-unavailable` are: they are handled
   * BEFORE classification, so `classifyRefusal` is never asked about
   * them in the running extension. The row says where the core makes
   * each and what this client does instead of settling it.
   */
  'transport-unknown':
    'daemon.sc:1215 and 1372 -- the process that owns the store died while this connection was ' +
    'open, so the request may already have been applied and the connection is closed before ' +
    'anything can say. `saysNobodyKnows` takes it before settlement: the entry is marked ' +
    'pending, keeps its request id and its cursor, and goes again on the next drain',
  draining:
    'daemon.sc:239 -- the daemon has been asked to stop and is refusing new work while it ' +
    'finishes what it has. Taken before settlement as a retryable refusal: the entry stays, ' +
    'under the same request id, and is parked for a person after RETRY_CAP in a row',
  'store-busy':
    'daemon.sc:264 -- the store lock was still held by somebody else past the waiting budget. ' +
    'Taken before settlement as a retryable refusal, exactly as `draining` is',
  unreadable:
    'guarded, rpc.sc:76 -- an entry the verb could not read, anywhere in the verb, including ' +
    'after the append; the answer does not say where. `saysNobodyKnows` takes it before ' +
    'settlement, exactly as `transport-unknown`: pending, same request id, sent again',
  'unknown-name':
    'whereis, rpc.sc:1216 -- a read verb naming the nearest names it could place; not a ' +
    'write and not a verb a save sends',
  /*
   * NOTE: THE FOUR `eval` KINDS AND `store-load-failed` ARRIVED WITH THE
   * CORE'S BATCH E (first read on theourgia 3017e45); the sites below are
   * re-read on the pinned core, 877f0da.
   *
   * NEVER: THE FOUR `eval` ONES CANNOT BE A WRITE'S ANSWER HERE FOR A
   * STRUCTURAL REASON, not because they look unlikely: `eval` is not a
   * verb this extension can send. `client.ts` lists twenty-six verbs in
   * `KNOWN_VERBS` and neither `eval` nor `serve` is among them, and
   * `answerKind` (client.ts:127) THROWS for a verb that is not listed --
   * so an `eval` request cannot leave this process at all.
   */
  'eval-worker-unavailable':
    'eval-worker.sc:143 (handshake) and eval-supervise.sc:172 (no-ready) -- TWO sites, not ' +
    'one: the worker says it when its handshake fails, and the supervisor says it when no ' +
    "worker became ready before its deadline (`ready-ms`, 5000 in the pinned core). Both answer " +
    'an `eval`, which this extension cannot send',
  'spawn-refused':
    'eval-supervise.sc:182 -- the worker process could not be started at all. It answers an ' +
    '`eval`; the same tag also appears in proc.sc:87 as a process DEATH REASON rather than an ' +
    'answer, which is a different thing wearing the same word',
  'eval-limit':
    'limit-answer, eval-supervise.sc:317 -- an evaluation reached its time, memory or output ' +
    'budget. It answers an `eval`, which this extension cannot send',
  'eval-worker-exit':
    'finish, eval-supervise.sc:329 -- the worker ended without a complete protocol line. It ' +
    'answers an `eval`, which this extension cannot send',
  /*
   * NEVER: AND THIS ONE IS NOT AN ANSWER TO ANYTHING. It is printed on the
   * daemon's BOOT path, before it can serve: `store-loop` (daemon.sc:788)
   * tries to open the store, and on failure calls `report` --
   * `(write x) (newline)` to the daemon's own output, daemon.sc:712 --
   * and then re-raises. NOTE: It runs BEFORE `(send main-pid '(ready))`, so
   * at that moment no connection exists for it to be an answer on. A
   * client sees the daemon fail to start, never this datum.
   */
  'store-load-failed':
    'store-loop, daemon.sc:796 -- printed by the daemon on its boot path when the store ' +
    'cannot be opened, before it signals ready and therefore before any connection exists; ' +
    'it is a startup report on the daemon\'s own output, not an answer to a request',
  unknown:
    'write-outcome->answer, store.sc:2510 -- it IS a write answer, and it is handled before ' +
    'classification: `unknown` is the absence of a determination, so the request is kept and ' +
    'retried rather than settled at all'
};

export function classifyRefusal(datum: Datum): Settlement {
  /*
   * NOTE: TWO KINDS OF "NOT IN THE TABLE", AND THEY WANT OPPOSITE THINGS.
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
   * NOTE: AND A USAGE LINE IS AN ANSWER ON THIS PATH TOO. `(usage (set <id>
   * <field> <value>))` is what a core WITHOUT request tracking says to a
   * write carrying --req and --cursor: the write did not happen, and the
   * reason is that the core is the wrong version. It is not an `(error
   * ...)`, so the first version of this classifier threw on it -- and
   * the suite went red on the cell that has covered it all along, which
   * is the whole argument for not throwing at answers that exist.
   */
  if (answerOf(datum, 'usage') !== null) {
    return { verdict: 'refused' };
  }
  if (answerOf(datum, 'error') === null || !Array.isArray(datum) || datum.length < 2) {
    throw new Error(
      'an answer reached the settler that this client cannot classify: it is not a refusal and ' +
        'was not accepted either'
    );
  }
  /*
   * NEVER: A REFUSAL WHOSE NAME CANNOT BE READ IS NOT A REFUSAL.
   *
   * This turned a non-symbol into the empty string, which no table
   * holds, so the answer fell through to `unrecognised` -- and the send
   * path settles an unrecognised refusal as `refused`, which removes the
   * request from the queue. Measured in a thirteenth review round: exit
   * 1 with `(error ("unknown"))` took a save out of the queue as
   * definitively turned down.
   *
   * This is the same defect as the one repaired on the `ok` side, where
   * an unparseable answer confirmed a save. One coin, two faces: the
   * exit code was being trusted to say what KIND of answer this is.
   */
  if (!isSym(datum[1])) {
    throw new Error(
      'an answer reached the settler whose refusal name cannot be read, so whether the store ' +
        'turned this request down is not known'
    );
  }
  const name = (datum[1] as { name: string }).name;
  const known = REFUSALS[name];
  if (known === 'executed-by-operator') {
    /*
     * NOTE: WHETHER IT NAMED A RECORD NO LONGER DECIDES ANYTHING. An
     * earlier build treated the answer as settleable only when it
     * carried an `(event ...)` clause, and recorded that clause as the
     * position -- which the core says is not recoverable from an
     * operator's determination. So the clause is carried as a note for
     * a person to read and nothing is derived from it.
     */
    return { verdict: 'executed-by-operator', note: describeRefusal(datum) };
  }
  if (known === 'req-mismatch') {
    return { verdict: 'req-mismatch' };
  }
  /*
   * NOTE: THE TWO FAMILIES THE SEND PATH INTERCEPTS ARE RECOGNISED HERE
   * ANYWAY, and the reason is that this function is what the census
   * asks. `U-ref` feeds every refusal the pinned core can make through
   * this classifier and fails on anything it marks unrecognised -- so a
   * name handled perfectly well three branches earlier in `send` would
   * still read as "nobody has looked at this one".
   *
   * NEVER: THAT IS NOT THE SAME AS THEM BEING SETTLED AS REFUSALS. Nothing
   * in the running extension reaches this line with one of them: the
   * retryable family is intercepted before, and so is the settings
   * family. What keeps that true is not this comment but a cell --
   * `plugin-r2 nothing in either family settles the entry` -- which
   * drives a real Saver with each name in both tables and asserts the
   * entry is still in the queue afterwards. Delete an interception and
   * that cell reddens; it is the guard, this is the census's answer.
   */
  if (RETRYABLE_REFUSALS.includes(name as (typeof RETRYABLE_REFUSALS)[number])) {
    return { verdict: 'refused' };
  }
  if (SETTINGS_REFUSALS[name] !== undefined) {
    return { verdict: 'refused' };
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
  if (answerOf(datum, 'usage') !== null) {
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
  /*
   * NOTE: THE FORM IS ALREADY KNOWN TO BE AN `error` HERE -- every path
   * into this function passes through the head test above. Reading its
   * clause through the decoder says so in the code rather than in a
   * comment.
   */
  const form = answerOf(datum, 'error');
  /*
   * WHAT THE CORE SAID AFTER THE NAME REACHES THE SENTENCE. (queue 7b)
   *
   * This read `datum[1]` and stopped, so `(error refused (instance
   * machine))` reached the screen as "the core refused the write:
   * refused", `(error malformed-intent (field-value-not-text (field
   * title) ...))` without saying which field, and `(error bad-request
   * kind-not-known (kind "X") ...)` without the kind. Three parts of the
   * core, one defect: the reason is at position 2 and later, and the
   * sentence was built from position 1. The rest is printed as the core
   * wrote it, and a `(remedy ...)` clause is said in words, because it
   * is the core naming what to do.
   *
   * NOTE: A REMEDY IS LEFT OUT OF THE PRINTED REST ONLY WHEN IT WAS SAID.
   * The first version left the clause out whenever the decoder found it,
   * and said it only when its value was a name or a string -- so `(remedy
   * (adopt))` or `(remedy)` vanished from the sentence altogether. Review
   * r1 of this item measured it. A clause this client cannot put into
   * words is printed as the core wrote it.
   */
  const remedy = form?.value('remedy');
  const said =
    remedy !== undefined && remedy.read && (isSym(remedy.value) || typeof remedy.value === 'string')
      ? isSym(remedy.value)
        ? remedy.value.name
        : remedy.value
      : null;
  const remedyClause = form?.whole('remedy');
  const detail = refusalDetail(
    datum.slice(2),
    said !== null && remedyClause !== undefined && remedyClause.read ? remedyClause.items : null
  );
  const rest =
    (detail.length > 0 ? ` ${detail}` : '') +
    (said !== null ? `. The core names the remedy: ${said}` : '');
  /*
   * NOTE: AND THE ONE SENTENCE WRITTEN FOR A NAME CARRIES THE REST TOO.
   * `changed` has a sentence of its own because "refused: changed" says
   * nothing a person can use; it used to stop there and drop `(current
   * ...)`, against the rule above. Review r1 of this item measured it.
   */
  const current = form?.value('current');
  if (name === 'changed' && current !== undefined && current.read) {
    return `the block changed in the store since it was opened${rest}`;
  }
  return `the core refused the write: ${name}${rest}`;
}

/*
 * THE CLAUSES AFTER A REFUSAL'S NAME, AS TEXT, without the remedy clause
 * (said in words above; the decoder found it, and it is left out by
 * identity rather than by comparing names a second time) and cut at a
 * length a notification can carry.
 *
 * NOTE: THIS IS ON THE PATH THAT REPORTS A FAILURE, so it does not throw.
 * The printer is goeteia's and has a depth limit; an answer that got this
 * far was read under the same limit and should print. If it does not, the
 * sentence SAYS so rather than going out as though there were nothing
 * after the name -- a detail that could not be printed is not an absent
 * one.
 */
const DETAIL_LIMIT = 400;

function refusalDetail(rest: Datum[], remedy: Datum | null): string {
  const shown = rest.filter((item) => item !== remedy);
  if (shown.length === 0) {
    return '';
  }
  let text: string;
  try {
    text = shown.map((item) => wire().write(item)).join(' ');
  } catch {
    return '(with a detail this client could not print)';
  }
  return text.length > DETAIL_LIMIT ? `${text.slice(0, DETAIL_LIMIT)} ...` : text;
}

/*
 * One chain per outbox file, for the whole process. Keyed by the
 * resolved path so that two spellings of one file do not get two chains.
 */
const queues = new Map<string, Promise<void>>();

/*
 * WHICH ENTRY GOES NEXT. (section 13, r3-4)
 *
 * NOTE: STILL ONE AT A TIME. What changes is only which one: a `parked`
 * entry is stepped over, and so is any entry for a block that has a
 * parked entry in front of it -- because within a block the order is
 * what makes a second save mean "and then this". Other blocks carry on.
 *
 * NOTE: AND A `pending` ENTRY IS NOT STEPPED OVER. Nobody knows whether the
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

/*
 * ONE DURABILITY SENTENCE PER QUEUE FILE, THE LATEST. (queue item 22, ruled
 * 2026-09-26 on the design's W) An outcome follows the sink's rule: several
 * writes to one file say one thing -- that file may not survive a power cut
 * -- and the latest reason is the one to read. Every warning a Saver hands
 * on is about its one queue file, so of two, the later is kept; sentences
 * about different files would be newline-joined, and a Saver has none.
 */
function latestWarning(earlier: string | null | undefined, later: string | null | undefined): string | undefined {
  if (later !== null && later !== undefined) {
    return later;
  }
  return earlier === null ? undefined : earlier;
}

/*
 * THE AWAITED SAVE'S OUTCOME WITH WHAT ITS QUEUE WRITES COULD NOT PROMISE:
 * its drain iteration's sentence when there is one, being later than the
 * enqueue's, and the enqueue's otherwise.
 */
function withEnqueueFirst(outcome: SaveOutcome, enqueued: string | null): SaveOutcome {
  const durability = latestWarning(enqueued, outcome.durability);
  return durability === undefined ? outcome : { ...outcome, durability };
}

export class Saver {
  private readonly client: Client;
  private readonly outbox: Outbox;
  private readonly settle: Settle & { storeHash?: string };
  private readonly newRequestId: () => string;
  /*
   * HOW MANY TIMES IN A ROW THIS REQUEST HAS BEEN TOLD "NOT NOW".
   * NOTE: In memory, by decision: see RETRY_CAP.
   */
  private readonly notNowCounts = new Map<string, number>();
  private readonly now: () => number;
  private readonly baselineOf?: (file: string) => { highWater: number } | null;
  private readonly retired?: (
    record: SendRecord
  ) => { known: true; retired: boolean } | { known: false };
  private bootstrapProblem: string | null = null;
  private readonly durability: (file: string, text: string, at?: number) => void;
  /*
   * THE WARNINGS OF THE DRAIN ITERATION IN PROGRESS, or null outside one.
   * See `kept` and `placeGathered`.
   */
  private gathered: Array<{ text: string; at: number }> | null = null;
  /*
   * WHEN EACH OUTCOME'S WARNING WAS PRODUCED, for the outcomes the drain
   * built from its iterations (queue item 46): `passOn` and the drain's
   * transfer on a throw hand the warning on with this stamp.
   */
  private readonly stamps = new WeakMap<SaveOutcome, number>();

  constructor(client: Client, outbox: Outbox, settle: Settle, options: SaverOptions) {
    /*
     * NOTE: IF THE SETTLER KNOWS WHICH QUEUE IT IS FOR, IT HAS TO BE THIS
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
     * NOTE: A BARE CALLBACK IS ACCEPTED, and that is deliberate rather than
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
    this.durability = options.durability;
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
      /*
       * NOTE: THIS PATH HAS NO NUMBER TO GIVE BACK, so the receipt answers
       * only one question here: whether the queue could promise the entry
       * survives a power cut. That is carried on the outcome, as `submit`
       * carries it.
       */
      const receipt = this.outbox.enqueue(entry);
      const enqueuedAt = nextStamp();
      let outcomes: SaveOutcome[];
      try {
        outcomes = await this.drain();
      } catch (e) {
        this.toSink(receipt.durability, enqueuedAt);
        throw e;
      }
      const mine = outcomes.find((o) => o.req === entry.req);
      this.passOn(outcomes, mine);
      const outcome: SaveOutcome = mine ?? {
        status: 'pending' as const,
        req: entry.req,
        id,
        message: 'the save is queued behind an earlier one whose outcome is unknown',
        answer: null
      };
      return withEnqueueFirst(outcome, receipt.durability);
    });
  }

  /*
   * THE SEND THE RECORD DESCRIBES. (section 13.1)
   *
   * NOTE: NOT IMPLEMENTED YET, AND ADDED BESIDE `save` RATHER THAN
   * REPLACING IT. This is the skeleton step: the signature lands so
   * that section 13's cells compile and can be read as red before the
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
    /*
     * NEVER: WHETHER THE ENTRY WAS QUEUED IS ASKED OF THE FILE.
     *
     * Four review rounds repaired this one exit at a time. Round sixteen
     * made every `return` before the enqueue carry `notQueued` and wrote
     * the rule down at the function; round seventeen left through the
     * `await` beside those returns; round eighteen left through
     * `serialise`, which reloads the queue from disk BEFORE this
     * callback runs at all. Round eighteen's answer was a flag set at
     * the enqueue -- and round nineteen showed the flag lying in the
     * other direction: `Outbox.write` renames the file into place and
     * THEN syncs the directory, so an enqueue can throw with the entry
     * already on disk, and the outcome said it had not been queued.
     *
     * Round nineteen asked the file in the catch, and round twenty
     * showed the file answering a different question by then: a save
     * that was queued, SENT and dequeued is not in the file either, so
     * a failure after a successful transmission read as "never queued"
     * and released the number for a send that had gone out.
     *
     * Round twenty-one's answer read the file at the enqueue and kept
     * that -- and round twenty-one showed the lock already released when
     * the read happens: `withQueueExclusive` is held inside
     * `Outbox.enqueue`, so every reader afterwards races every other
     * window. Five answers, each an account of something other than the
     * enqueue: the source's shape, which statements ran, the file at the
     * wrong moment, the file without the lock.
     *
     * KEY: SO THE ENQUEUE ANSWERS IT. (queue item 3, the enqueue receipt)
     * `Outbox.enqueue` returns a `Receipt`, made inside the lock after the
     * commit returned, that nothing else can make. Holding one is the
     * fact; not holding one -- the enqueue threw before the rename landed
     * -- is the other fact. A rename that landed with a directory flush
     * that failed IS a receipt, with a durability warning in it: the entry
     * exists, and the number must not be given back (ruled 2026-09-22).
     */
    let receipt: Receipt | null = null;
    let enqueuedAt = 0;
    return this.serialise(async () => {
      /*
       * NOTE: THE SECOND PLACE THIS IS ASKED, AND ON PURPOSE.
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
      /*
       * NEVER: A BLOCKED SUBMIT THAT NEVER QUEUED HAS TO SAY SO.
       *
       * `notQueued` is what lets the save handler give the sequence
       * number back, and without it the sidecar keeps
       * `outstanding: [{seq, req}]` for a send no entry anywhere can
       * answer for -- a draft the block can never stop being. It was
       * written on the one refusal that was in front of the reviewer
       * when the flag was added (an unwritable queue) and on neither of
       * the two above it, both of which return before `enqueue` just as
       * plainly. Measured in a sixteenth review round with
       * `(check (writers ()))`: blocked, no `notQueued`, empty queue,
       * and the number still outstanding on disk.
       *
       * The rule is positional and it is now stated: every return in
       * this function BEFORE the `enqueue` below carries `notQueued`.
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
          answer: null,
          notQueued: true as const
        };
      }
      /*
       * NEVER: AND AN EXCEPTION FROM THE BOOTSTRAP IS AN EXIT TOO.
       *
       * The rule above is positional -- every return before the enqueue
       * carries `notQueued` -- and a seventeenth review round walked out
       * through the door beside it: `ensureCursor` asks the store, and a
       * `check` that times out rejects. `submit` then rejected, the save
       * handler reported the failure and returned, and `releaseSend` is
       * reached only for a RESOLVED outcome carrying the flag. The
       * number stayed in `outstanding` for a send that was never queued,
       * and the file is a draft nothing can settle.
       *
       * The caller cannot repair this: once an exception is out of here,
       * whether the entry reached the queue is not a question it can
       * answer. This is the layer that knows, so the ambiguity is turned
       * into an outcome here, where the entry demonstrably has not been
       * queued yet.
       */
      let cursor: string | null;
      try {
        cursor = await this.ensureCursor();
      } catch (e) {
        return {
          status: 'blocked' as const,
          req: record.req,
          id: record.blockId,
          message:
            'the store could not be asked where its log ends, so this save was not queued: ' +
            (e instanceof Error ? e.message : String(e)),
          answer: null,
          notQueued: true as const
        };
      }
      if (cursor === null) {
        return {
          status: 'blocked' as const,
          req: record.req,
          id: record.blockId,
          message: this.bootstrapProblem ?? 'this store has no cursor to write against',
          answer: null,
          notQueued: true as const
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
        receipt = this.outbox.enqueue(entry);
        enqueuedAt = nextStamp();
      } catch (e) {
        /*
         * NOTE: AND THIS ONE IS RE-RAISED RATHER THAN ANSWERED HERE.
         *
         * An enqueue that threw failed BEFORE its rename (queue item 22: a
         * flush that fails after the rename is a warning in the receipt, not
         * a throw), so the entry is not on disk -- and that is the question
         * the catch below answers, with no receipt to say otherwise.
         * Answering it here would be a second place deciding the same thing,
         * and the two would differ.
         */
        throw new Error(
          `the save could not be written to the queue at ${this.outbox.path}: ${String(e)}`
        );
      }
      /*
       * NOTE: NO CHECK HERE THAT `receipt.req` IS `record.req`, and the
       * absence is deliberate (condition 4 of the approved design). The
       * receipt is issued by the line above and consumed in this function;
       * it crosses no boundary, so there is no case in which it could be
       * another entry's, and a check with no case behind it would be the
       * kind of guard this tree has deleted before. A receipt that TRAVELS
       * is checked where it arrives -- `Sessions.importFrom`, which is
       * handed one per imported entry. The day this function receives a
       * receipt from outside, this sentence is the reminder.
       */
      const held = receipt;
      const outcomes = await this.drain();
      const mine = outcomes.find((o) => o.req === entry.req);
      this.passOn(outcomes, mine);
      const outcome: SaveOutcome = mine ?? {
        status: 'pending' as const,
        req: entry.req,
        id: record.blockId,
        message: 'the save is queued behind an earlier one whose outcome is unknown',
        answer: null
      };
      return withEnqueueFirst(outcome, held.durability);
    }).catch((e) => {
      /*
       * THE ANSWER RECORDED AT THE ENQUEUE, not a second reading taken
       * now.
       *
       * A failure after the entry was queued is still a failure, and the
       * number must NOT be released then: something else will answer for
       * that entry -- or already has -- and saying a send is not
       * outstanding while it went out is how a record comes to disagree
       * with the store. Reading the file HERE would answer a different
       * question, because by now a drain may have removed the entry.
       */
      if (receipt !== null) {
        /*
         * NOTE: AND WHAT THE ENQUEUE SAID ABOUT DURABILITY IS NOT LOST WITH
         * THE OUTCOME. (queue item 22, K11) No outcome carries it now, so it
         * goes to the sink before the rejection does.
         */
        this.toSink(receipt.durability, enqueuedAt);
        throw e;
      }
      return {
        status: 'blocked' as const,
        req: record.req,
        id: record.blockId,
        message:
          'this save was not queued: ' + (e instanceof Error ? e.message : String(e)),
        answer: null,
        notQueued: true as const
      };
    });
  }

  public retry(): Promise<SaveOutcome[]> {
    return this.serialise(async () => this.passOn(await this.drain(), undefined));
  }

  /*
   * THE RETRY A PERSON ASKS FOR: PARKED ENTRIES GO BACK FIRST. (queue 7)
   *
   * A parked entry is stepped over by the drain, and so is every later
   * save of its block. Until now only a settings change released them,
   * which answered the settings family and nothing else -- an instance
   * mismatch is repaired outside the editor, and a save parked for it had
   * no way back. So the command releases them before it drains. One that
   * is still not answerable is parked again on the same attempt, by the
   * same rule that parked it, so this adds a way out and takes nothing
   * away.
   *
   * NOTE: `retry` ITSELF IS UNCHANGED, because it is also the drain the
   * extension schedules when a window starts. Releasing there would send
   * every save parked after RETRY_CAP five more times on each start,
   * which nobody asked for.
   */
  public retryParked(): Promise<SaveOutcome[]> {
    return this.serialise(async () => {
      this.toSink(this.outbox.unparkAll().durability);
      return this.passOn(await this.drain(), undefined);
    });
  }

  /*
   * NOTE: A TAKEOVER'S ENTRIES ARRIVE THROUGH THE SAME LOCK AS A SAVE.
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
    /*
     * NEVER: THE EXIT CODE IS NOT ASKED. (queue item 15) The core makes
     * `check` exit non-zero exactly when its verdict is not `ok`, so a
     * damaged store answers with exit 1 AND a complete `check` form --
     * writers, local writer, verdict. Asking `answer.ok` here told the user
     * "the store did not answer" about a store that had answered in full.
     * Whether there is an answer is whether there is a `check` form; what is
     * in it is `firstCursorFromCheck`'s to read, and the store's condition
     * is a fact about the store, not a failure of the question.
     */
    /*
     * NEVER: MORE THAN ONE FORM IS NOT AN ANSWER TO READ THE FIRST OF. The
     * client checks that a `check` carries one datum only when the exit is
     * zero; on a non-zero exit it hands on whatever arrived, and reading the
     * first form let `(check ...)` followed by `(error ...)` supply a cursor
     * (review r1 of item 15). One form, and it is a `check`, or no cursor.
     */
    if (answer.answers.length > 1) {
      this.bootstrapProblem =
        `the store answered \`check\` with ${answer.answers.length} forms where one was expected, so ` +
        'which of them is its answer is not known and nothing is written against a guess';
      return null;
    }
    if (answer.answers.length === 0 || answerOf(answer.answers[0], 'check') === null) {
      this.bootstrapProblem = 'the store did not answer `check`, so this client has no cursor to write against';
      return null;
    }
    const first = firstCursorFromCheck(answer.answers[0]);
    if (!first.ok) {
      this.bootstrapProblem =
        first.reason === 'many-writers'
          ? `this store has ${first.writers.length} writers and the core did not say which of them is ` +
            'local, so nothing is written from here. A core with `local-writer` in its `check` ' +
            'answer (theourgia F45 and later) says it; one older than that cannot'
          : first.reason === 'unreadable' && first.unreadable === 'local-end'
            ? `the store answered \`check\` naming its local writer (${first.local ?? '?'}) without a ` +
              'position this build could read for it, so there is no cursor to write against'
          : first.reason === 'unreadable' && first.unreadable === 'local-writer'
            ? 'the store answered `check` naming its local writer in a form this build could not ' +
              'read, so which writer is this store\'s own is not known and nothing may be written ' +
              'against a guess'
          : /*
             * NOTE: THE LISTING'S SENTENCE CLAIMS NO COUNT. It said "how many
             * writers it has is not known", which is false when every writer is
             * named and only one entry's end cannot be read -- review r2 of item
             * 2 found it, and it was measured: two writers listed, the sentence
             * saying their number was unknown. What is always true of an
             * unreadable listing is that it could not be read in full, so no
             * position in it can be trusted.
             */
            first.reason === 'unreadable'
            ? 'the store answered `check` with a writer listing this build could not read in ' +
              'full, so no writer\'s position in it can be trusted and nothing may be written ' +
              'against a guess'
            : first.reason === 'local-writer-twice'
              ? `the store's answer to \`check\` lists its local writer (${first.local ?? '?'}) more than ` +
                'once, so where that writer ends is not one answer and nothing is written against a guess'
            : first.reason === 'local-writer-not-listed'
              ? `the store's answer to \`check\` contradicts itself: the local writer it names ` +
                `(${first.local ?? '?'}) is not among its writers, so nothing is written from here ` +
                'until the store says one thing'
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
    this.kept(this.outbox.setCursor(first.cursor));
    return first.cursor;
  }

  /*
   * The queue is walked from the front and stops at the first entry
   * whose outcome is unknown. Everything behind it was composed against
   * a cursor that entry is about to move.
   */
  /*
   * NOTE: A DRAIN THAT THROWS HANDS THE WARNINGS OF THE OUTCOMES IT HAD
   * ALREADY PRODUCED TO THE SINK FIRST. (queue item 22, delivery review r1,
   * the N) An earlier iteration's warning waits on its outcome for the
   * caller to select or pass on; a later iteration that throws takes the
   * whole array with it, and nothing would select or pass on anything. The
   * throwing iteration's own warnings went to the sink in its `finally`.
   */
  private async drain(): Promise<SaveOutcome[]> {
    const outcomes: SaveOutcome[] = [];
    try {
      return await this.drainInto(outcomes);
    } catch (e) {
      for (const outcome of outcomes) {
        this.toSink(outcome.durability, this.stamps.get(outcome));
      }
      throw e;
    }
  }

  private async drainInto(outcomes: SaveOutcome[]): Promise<SaveOutcome[]> {
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
       * NOTE: AND THE BASELINE IS RE-READ BEFORE THIS ONE GOES OUT. (R8)
       *
       * Not to count the reads -- to decide. A send whose number is
       * below what the record beside its file already has confirmed has
       * been overtaken: transmitting it would put older bytes over
       * newer ones, and its answer would settle without becoming the
       * baseline, so the work would look sent and would not be.
       */
      const withdrawn = this.hasBeenRetired(entry);
      if (withdrawn !== null) {
        this.kept(this.outbox.markParked(entry.req, withdrawn));
        continue;
      }
      const overtaken = this.hasBeenOvertaken(entry);
      if (overtaken !== null) {
        this.kept(this.outbox.markParked(entry.req, overtaken));
        continue;
      }
      /*
       * NOTE: FROM HERE THE ITERATION'S QUEUE WARNINGS ARE GATHERED FOR THE
       * OUTCOME IT PRODUCES. (queue item 22, ruled Q5) The `finally` below
       * hangs them on that outcome, or, when the iteration throws before
       * producing one, hands them to the sink before the throw goes on
       * (K11). The two parks above are iterations with no outcome; theirs
       * went to the sink as they happened.
       */
      const at = outcomes.length;
      this.gathered = [];
      try {
        /*
         * THE ENTRY IS MARKED AS GOING OUT BEFORE IT GOES OUT, for the
         * same reason it was written down before it was sent: after this
         * line the store may have seen it, and nothing may change what it
         * says.
         */
        this.kept(this.outbox.aboutToSend(entry.req, this.outbox.cursor));
        const current = this.outbox.find(entry.req) ?? entry;
        const outcome = await this.send(current);
        /*
         * NOTE: AND IT STOPS IF THE ENTRY IS STILL HERE.
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
          this.kept(this.outbox.markPending(entry.req, why));
          outcomes[outcomes.length - 1] = { ...outcome, status: 'pending', message: why };
          return outcomes;
        }
      } finally {
        this.placeGathered(outcomes, at);
      }
    }
  }

  /*
   * A QUEUE WRITE'S DURABILITY WARNING, WHERE IT BELONGS. (queue item 22)
   * Inside a drain iteration it is gathered for the outcome that iteration
   * produces; anywhere else it goes to the window's sink.
   */
  private kept(warning: string | null): void {
    if (warning === null) {
      return;
    }
    if (this.gathered !== null) {
      this.gathered.push({ text: warning, at: nextStamp() });
      return;
    }
    this.toSink(warning);
  }

  private toSink(warning: string | null | undefined, at?: number): void {
    if (warning !== null && warning !== undefined) {
      this.durability(this.outbox.path, warning, at);
    }
  }

  /*
   * WHERE AN ITERATION'S WARNINGS GO WHEN IT ENDS: onto the outcome it
   * produced, when it produced one (the save that awaits that entry keeps
   * it; `passOn` hands any other to the sink); to the sink when it produced
   * none, which is the path of a throw.
   */
  private placeGathered(outcomes: SaveOutcome[], at: number): void {
    const gathered = this.gathered ?? [];
    this.gathered = null;
    if (gathered.length === 0) {
      return;
    }
    if (outcomes.length > at) {
      const produced = outcomes[at];
      const latest = gathered[gathered.length - 1];
      outcomes[at] = { ...produced, durability: latestWarning(produced.durability, latest.text) };
      this.stamps.set(outcomes[at], latest.at);
      return;
    }
    for (const warning of gathered) {
      this.toSink(warning.text, warning.at);
    }
  }

  /*
   * EVERY OUTCOME BUT THE AWAITED ONE HANDS ITS WARNING TO THE SINK, and
   * comes back without it: a drain may settle other saves' entries, and the
   * caller selecting its own outcome would otherwise discard theirs (K2).
   * With nothing awaited (a retry), every outcome's goes.
   */
  private passOn(outcomes: SaveOutcome[], mine: SaveOutcome | undefined): SaveOutcome[] {
    return outcomes.map((outcome) => {
      if (outcome === mine || outcome.durability === undefined) {
        return outcome;
      }
      this.toSink(outcome.durability, this.stamps.get(outcome));
      const { durability: _handedOn, ...rest } = outcome;
      return rest;
    });
  }

  /*
   * NOTE: A `sent` ENTRY IS NOT AN EXECUTED ONE. It says this client put
   * the request on the wire, not that the store applied it -- the whole
   * reason the queue keeps it. So the check is made for a sent entry
   * too, on every retry.
   */
  /*
   * NOTE: AND "I COULD NOT LOOK" IS NOT "NOTHING IS RETIRED". The question
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
     * NOTE: WHAT GOES ON THE WIRE COMES FROM THE RECORD WHEN THERE IS ONE.
     *
     * The entry still carries `id`, `field` and `payload` so that a
     * queue written by this build can be read by the one before it --
     * but two places holding the same fact is how this batch lost a
     * save, so at the moment of use there is one: the record if the
     * entry has one, and the old fields only for an entry written
     * before section 13, which has no record and takes the legacy path.
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
        this.kept(this.outbox.markPending(entry.req,'The immutable working selection is unreadable'));
        return {status:'pending',req:entry.req,id:what.id,message:'The immutable working selection is unreadable; keep the request for recovery',answer:null};
      }
      /*
       * NOTE: `--wire` IS ASKED FOR HERE AND NOWHERE ELSE YET.
       *
       * The human rendering drops every clause beside `items` except
       * `incomplete` (`render-human`, render.sc:57), and `(behind ...)` -- who else has landed
       * records since this save's baseline -- is one of them. A program
       * should be reading the machine form of everything; this batch
       * changes the one verb whose answer this build is losing
       * something from, and the rest is recorded as a whole change of
       * its own rather than smuggled in here.
       *
       * The item the answer wraps arrives as the answer, so nothing
       * below reads differently; the form round it arrives as
       * `answer.envelope`.
       */
      args=[what.id,'--writer',selected.writer,'--working-version',selected.version,'--req',entry.req,'--cursor',entry.cursor,'--wire'];
    } else args=[what.id,what.field,what.payload,'--req',entry.req,'--cursor',entry.cursor];
    let answer;
    try {
      answer = await this.client.request(verb, args);
    } catch (e) {
      if (e instanceof TransportError) {
        const why = aside(e.detail);
        this.kept(this.outbox.markPending(entry.req, `${e.message}${why}`));
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
        this.kept(this.outbox.markPending(entry.req, 'the core answered ok without naming a record'));
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
        this.kept(this.outbox.markPending(entry.req, `the core named a record this client cannot spell: ${moved}`));
        return {
          status: 'pending',
          req: entry.req,
          id: what.id,
          message: `the core named the record ${moved}, which is not a cursor the core itself would parse`,
          answer: datum
        };
      }
      this.settle(entry.req, { verdict: 'confirmed', cursor: moved });
      /*
       * NOTE: WHAT LANDED WHILE THIS SAVE WAS BEING PREPARED IS CARRIED OUT
       * WITH THE SUCCESS, NOT INSTEAD OF IT. The clause is informational:
       * the commit worked, and the sentence saves whoever reads it a
       * `drafts` round trip. NEVER: It must not change the status, the
       * settlement or the queue -- a save that succeeded is a save that
       * succeeded.
       */
      /*
       * NOTE: THE CLAUSE IS OUTSIDE THE ITEM, so it is read from the
       * envelope; `datum` is the item and has never carried it. It is
       * read WITH the item as well, because the writer this commit
       * itself advanced is named in its own `behind` and the item's
       * cursor is what says which writer that is.
       */
      const behind = behindNotice(answer.envelope, datum);
      return {
        status: isReplay(answer) ? 'replayed' : 'saved',
        req: entry.req,
        id: what.id,
        message: isReplay(answer)
          ? 'the store had already applied this request'
          : 'saved',
        answer: datum,
        ...(behind === null ? {} : { behind })
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
      this.kept(this.outbox.markPending(entry.req, `the core exited ${answer.rc} without saying why${why}`));
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message:
          `the core exited ${answer.rc} without an answer; the save is kept and can be retried${why}`,
        answer: null
      };
    }

    const notNow = whichRetryableRefusal(datum);
    if (notNow !== null) {
      const soFar = (this.notNowCounts.get(entry.req) ?? 0) + 1;
      if (soFar >= RETRY_CAP) {
        /*
         * NEVER: PARKED, NOT PENDING, AND THE DIFFERENCE IS WHO IS WAITING.
         * `pending` stops the whole queue behind this entry; after this
         * many refusals nothing here is going to change on its own, so
         * the queue steps over it and carries on with other blocks while
         * a person looks at this one. The count is in the reason because
         * "the store was busy" and "the store was busy five times
         * running" ask for different things from whoever reads it.
         */
        this.notNowCounts.delete(entry.req);
        /*
         * `unwritable` says where and why (its path and reason clauses), and
         * that is what the person who has to look needs; the other members
         * of this family carry neither.
         */
        const where = notNow === 'unwritable' ? whereAndWhy(datum) : '';
        const why = `the store answered ${notNow}${where} ${soFar} times in a row; a person has to look`;
        this.kept(this.outbox.markParked(entry.req, why));
        return {
          status: 'refused',
          req: entry.req,
          id: what.id,
          message: why,
          answer: datum,
          keptForAPerson: true as const
        };
      }
      this.notNowCounts.set(entry.req, soFar);
      const why = `the store answered ${notNow}; attempt ${soFar} of ${RETRY_CAP}`;
      this.kept(this.outbox.markPending(entry.req, why));
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message:
          `the store is not taking writes just now (${notNow}); the save is kept and goes again ` +
          'under the same request id',
        answer: datum
      };
    }
    /*
     * NOTE: ANY OTHER ANSWER BREAKS THE RUN. "Five in a row" means in a
     * row: an intervening answer of a different kind is the store
     * having looked at this request, so the next `not now` starts from
     * one.
     */
    this.notNowCounts.delete(entry.req);

    /*
     * NOTE: A REFUSAL ONLY A SETTING CAN ANSWER IS PARKED AT ONCE.
     *
     * Nothing about the store directory or the length of the run-root
     * path changes because time passed, so the five attempts the family
     * above is given would be five identical failures. The entry is kept
     * -- the user's bytes are still only in their file -- and the queue
     * steps over it and carries on with other blocks.
     *
     * The way back is the setting itself: a configuration change makes
     * the extension try each parked entry once more, so there is no
     * recovery command to learn.
     */
    const settings = whichSettingsRefusal(datum);
    if (settings !== null) {
      this.kept(this.outbox.markParked(entry.req, settings.message));
      return {
        status: 'refused',
        req: entry.req,
        id: what.id,
        message: settings.message,
        answer: datum,
        keptForAPerson: true as const
      };
    }

    /*
     * NOTE: AN UNRECOGNISED NAME AT THE CLIENT'S OWN EXIT CODE IS UNKNOWN,
     * NOT REFUSED.
     *
     * When a daemon fails to start, the thin client answers with the
     * last `(error ...)` in the log it has just written -- whatever the
     * core happened to put there. The names that can arrive that way are
     * the names the core can write to a startup log, which is not a list
     * this extension can hold and would be wrong the first time the core
     * learned a new one. Exit 75 is the client refusing on its own
     * account; at any other code an `(error ...)` is the STORE saying the
     * write did not happen, which settles.
     */
    if (answer.rc === CLIENT_REFUSED_EXIT && isUnrecognisedRefusal(datum)) {
      const why = `the client refused this request with ${describeRefusal(datum)}`;
      this.kept(this.outbox.markPending(entry.req, why));
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message:
          `${why}; this build does not recognise that refusal, so the outcome is kept unknown ` +
          'and the save goes again under the same request id',
        answer: datum
      };
    }

    if (saysNobodyKnows(datum)) {
      this.kept(this.outbox.markPending(entry.req, 'the store cannot say whether the request ran'));
      return {
        status: 'pending',
        req: entry.req,
        id: what.id,
        message: 'the store cannot say whether this save ran; it is kept and can be retried',
        answer: datum
      };
    }

    /*
     * NOTE: A REFUSAL IS CARRIED AS ONE, and the one refusal that means
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
