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
 * Session directories: whose files these are, whether that session is
 * still running, and what a user may do about one that is not.
 * (§12.9, §12.11.3-5, §12.19, §12.21.4, §12.23)
 *
 * A SESSION DIRECTORY IS PRIVATE TO ONE EXTENSION HOST. Block files are
 * never shared between processes, which is what makes the in-process
 * chain sufficient and what makes S7 unreachable rather than guarded
 * against. The price is stated and not hidden: two windows editing one
 * block both save, and the store takes the later one -- that is a store
 * question, and X1b answers it. (§12.9)
 *
 * NOTHING HERE DELETES ANYTHING. This extension never unlinks and never
 * renames a file it published. The single operation that moves anything
 * is a user-initiated discard of a whole DEAD session directory, to a
 * uniquely named place under `trash/`. (§12.23)
 */

import { execFileSync } from 'child_process';
import { randomUUID } from 'crypto';
import * as path from 'path';
import { FileOps } from './fsops';
import { Outbox, OutboxEntry } from './outbox';
import { Publisher, sidecarFromDisk, sidecarPathOf } from './publication';

/*
 * WHEN A PID STARTED, as epoch seconds, or null when this platform will
 * not say. It is a parameter of `Sessions` so that a cell can supply one
 * that fails -- the "cannot tell" answer has to be reachable, and on a
 * machine where `ps` works it otherwise never is.
 *
 * THE COMMAND IS GIVEN A DEADLINE. It is another program; one that hangs
 * would hang the window, and the honest answer to a command that did not
 * return is "cannot tell", not "dead". (§12.11.4, §12.13.5)
 */
export type StartTimeReader = (pid: number) => number | null;

export const systemStartTime: StartTimeReader = (pid) => {
  try {
    const out = execFileSync('ps', ['-o', 'lstart=', '-p', String(pid)], {
      encoding: 'utf8',
      timeout: 4000,
      env: { ...process.env, LC_ALL: 'C' }
    }).trim();
    if (out.length === 0) {
      return null;
    }
    const at = Date.parse(out);
    return Number.isFinite(at) ? Math.floor(at / 1000) : null;
  } catch (e) {
    return null;
  }
};

/*
 * THE COMPARISON CARRIES A SECOND OF SLACK, because the platform
 * commands report whole seconds and the value recorded at activation
 * may have been rounded the other way. (§12.13.5)
 */
function sameStart(recorded: number | null, now: number | null): boolean | null {
  /*
   * NO RECORDED START TIME IS NOT A MATCH.
   *
   * This returned `true` for a session whose `session.json` carried no
   * start time, so such a session was reported `identity-matches` --
   * an answer that claims the identity was checked when nothing was
   * compared. The rule says the third answer: cannot tell.
   *
   * ⚠️ AND SEVERAL FIXTURES ENDORSED THE MISTAKE. Cells that wanted a
   * live session wrote `startedAt: null` because it was the easy way to
   * make the comparison pass -- so the cells agreed with the defect
   * instead of catching it. They now record a real start time.
   */
  if (recorded === null || now === null) {
    return null;
  }
  return Math.abs(recorded - now) <= 1;
}

export interface SessionIdentity {
  sessionId: string;
  pid: number;
  /*
   * The start time of that pid, as epoch seconds, normalised. It is
   * what tells a reused pid from the original holder, and comparing it
   * carries a one-second tolerance because the platform commands report
   * whole seconds. (§12.13.5)
   */
  startedAt: number | null;
  nonce: string;
  stores: string[];
}

/*
 * Three states and the reason for each. "Cannot tell" is not "dead" and
 * not "alive": it is answered when the platform will not say, and the
 * caller errs towards alive and SAYS SO. (§12.21.4, C8/C19)
 */
/*
 * ⚠️ THE UNDECIDABLE CASES ARE THREE AND THEY ARE NOT THE SAME NEWS.
 *
 * `start-time-unavailable` is a failure to OBSERVE: the pid is there and
 * this machine could not be asked when it started. Waiting fixes it.
 * `record-missing` and `record-unreadable` are failures of EVIDENCE: the
 * window never wrote its identity, or what it wrote will not parse, and
 * no amount of waiting produces it.
 *
 * They were one value. That mattered the moment a way out of the second
 * kind was offered, because offering it for the first would be offering
 * to duplicate a request over a reading this machine could have simply
 * taken again a second later. (§12.9, U-claim)
 */
export type Liveness =
  | { alive: true; because: 'identity-matches' | 'identity-matches-permission-denied' }
  | { alive: false; because: 'pid-absent' | 'pid-reused' }
  | {
      decidable: false;
      because:
        | 'start-time-unavailable'
        | 'start-time-unrecorded'
        | 'liveness-unobtainable'
        | 'record-missing'
        | 'record-unreadable';
    };

/*
 * A session other than this one, as the "other sessions" list shows it.
 * (§12.19.1)
 */
/*
 * ⚠️ `identity` IS NULL WHEN THE WINDOW LEFT NO READABLE RECORD, AND THE
 * ROW IS STILL LISTED.
 *
 * It used to be skipped: `others` read the record and moved on when it
 * would not parse or was not there. So the one state `claim` refuses --
 * a directory with a queue in it and no record beside it -- was invisible
 * in the only listing the user has, and they could not even see the
 * thing they were being refused. A row nobody can see is not a refusal
 * they can act on. (U-claim)
 *
 * `sessionId` is therefore beside `identity` rather than inside it: the
 * directory's name is known whatever the record says, and inventing an
 * identity to fill the field would be drawing the unknown as an answer.
 */
export interface OtherSession {
  sessionId: string;
  identity: SessionIdentity | null;
  liveness: Liveness;
  drafts: string[];
  pendingEntries: number;
  liveAdopters: string[];
  /*
   * WHETHER AN EXPLICIT FORCED TAKEOVER IS OFFERED FOR THIS ROW. True
   * only for a record that is MISSING -- see `claim`. The listing is
   * where the user finds it, so the listing is where it is decided,
   * once, rather than in whatever draws the row.
   */
  forceable: boolean;
}

export type ClaimOutcome =
  | { claimed: true; token: string; sequence: number }
  | { claimed: false; because: 'already-claimed' | 'session-alive' | 'undecidable' | 'not-found' };

/*
 * Which claim an import belongs to. Importing is tied to the token that
 * was won, not to the session id: a later generation takes the NEXT
 * sequence number, and an import that named only the dead session could
 * not tell the two apart. (§12.11.3, C8)
 */
/*
 * WHERE IMPORTED ENTRIES GO. It is the adopting session's own queue,
 * reached through whatever holds it -- the `Saver`'s serial chain in the
 * extension, a plain `Outbox` in a cell -- so that an import cannot race
 * a save that is already in flight. (§12.11.3, P1-1)
 */
export interface ImportTarget {
  has(req: string): boolean;
  adopt(entry: OutboxEntry): void;
}

export interface ClaimToken {
  deadSessionId: string;
  sequence: number;
  file: string;
}

/*
 * Adopting twice keeps the marker that is there. Re-creating it would
 * be a second write to a create-once token, and C19 exists because an
 * implementation that overwrote it would look identical from outside.
 * (§12.19.3, C19)
 */
export type AdoptOutcome =
  | { adopted: true; marker: string }
  | { adopted: false; because: 'directory-gone' | 'already-adopted-by-this-session' };

/*
 * `liveAdopters` travels with the refusal AND with the success, because
 * the confirmation the user is shown has to name the windows that still
 * have documents open in that directory. (§12.19.1, §12.23)
 */
/*
 * THE TWO SENTENCES ARE FIXED AND THEY TRAVEL WITH THE ANSWER. One says
 * the editor's own backups are not covered by this listing; the other
 * says that requests already sent, or already taken over by another
 * window, may still be applied and cannot be recalled from here.
 * Neither is decoration: a user deciding to discard is deciding on the
 * strength of the listing, and the listing is only about this disk.
 * (§12.21.3, §12.22.2)
 */
export const DISCARD_BACKUP_NOTE =
  'The editor may hold unsaved edits in its own backups that this listing does not show; ' +
  'it reflects the disk only.';

export const DISCARD_REACH_NOTE =
  'This moves local recovery files only. Requests this session already sent, or that another ' +
  'window has taken over, may still be applied by the store and cannot be recalled from here.';

export type DiscardOutcome =
  | { discarded: true; trash: string; liveAdopters: string[]; notes: string[] }
  | {
      discarded: false;
      because: 'session-alive' | 'undecidable' | 'not-found';
      liveAdopters: string[];
      notes: string[];
    };

/*
 * ⚠️ EVERY REQUEST A TAKEOVER SAW LANDS IN EXACTLY ONE BUCKET, AND THE
 * BUCKETS ADD UP TO WHAT IT SAW.
 *
 * Three times in one function a count answered with a smaller, more
 * comfortable number than the truth: an unreadable queue counted as
 * zero, a queue the loader rejects counted as empty, the queue the
 * takeover was told to take failing in silence. Each was fixed where it
 * was found, and the next one would have been fixed the same way, one
 * at a time, for ever -- a cell per shape cannot catch a shape nobody
 * has thought of.
 *
 * So the report is a ledger and the law is conservation: `observed` is
 * everything this takeover saw, and every one of those things is in
 * exactly one of the buckets below. A cell adds the buckets and compares
 * them with `observed`.
 *
 * ⚠️ WHAT THAT CATCHES, EXACTLY: a thing counted as seen and put in no
 * bucket. It does NOT catch a thing that was never counted as seen
 * either -- dropping both sides preserves the equality -- nor a thing
 * put in the WRONG bucket, nor an entry the destination did not really
 * keep. Arithmetic is not membership and it is not truth; it is one
 * property, and the cells for the individual buckets are the others.
 *
 * ⚠️ WHAT THE LAW DOES NOT SAY, WRITTEN DOWN BECAUSE IT COST A DEFECT:
 * it governs WHERE what was seen went, and says nothing about whether
 * anything was seen at all. A takeover that visited no queue at all
 * satisfies it perfectly -- `observed` is zero and so is every bucket --
 * and that is exactly what happened when `list` answered `[]` for a
 * directory it could not read: the queue this takeover was ASKED about
 * was never opened, and the ledger added up, about nothing.
 *
 * Presence is a separate guarantee and has a separate mechanism:
 * `importFrom` walks the union of the queues it DISCOVERED and the one
 * it was NAMED -- and the named one is not filtered by an existence
 * check, because that check answers "no" for a file under an ancestry
 * this process cannot search. Loading decides. Do not read a balanced
 * ledger as evidence that everything was looked at.
 *
 * WHAT ONE "THING" IS: a request, where requests can be seen; a whole
 * FILE where they cannot, because the contents of a queue nobody can
 * parse are not observable and pretending to count them would be the
 * same lie in a new place.
 */
export interface TakeoverLedger {
  /*
   * Everything this takeover saw. Not everything that exists: a queue it
   * could not open is one thing seen, whatever is inside it.
   */
  observed: number;
  /* Moved into this window's queue by this run. */
  imported: number;
  /*
   * Already carried away -- found in the destination by this run, or
   * marked by an earlier takeover. Not lost, and not waiting.
   */
  skippedDuplicate: number;
  /* Waiting, in a queue belonging to a store this run did not take. */
  leftOtherStore: number;
  /*
   * Waiting, in the queue from before stores had their own directories.
   * Nothing can say which store those were written for.
   */
  leftUnknownStore: number;
  /*
   * A whole file that could not be trusted -- unparseable, not an
   * object, a version this build does not know, or an `entries` that is
   * not a list. One per file.
   */
  unreadableQueue: number;
  /*
   * A request the takeover could not move, because moving it threw --
   * the destination refused it, or the source could not be marked.
   *
   * ⚠️ THIS REPLACES `malformedEntry`, WHICH NOTHING COULD FILL once the
   * survey and the import agreed about which files are acceptable: the
   * loader rejects a file holding an element that is not a request, so
   * such a file is one `unreadableQueue` and never a collection of
   * individually malformed items. A bucket nothing can ever put anything
   * into is a report that can never mention it, which is a lie of its
   * own kind. Reported to the main session as a deviation from the
   * bucket list it named.
   */
  failedToMove: number;
  /*
   * ⚠️ MOVED, AND THE SOURCE STILL SAYS IT IS WAITING.
   *
   * The entry is in this window's queue and will be sent from here; what
   * failed is the mark on the OTHER window's copy. Those two outcomes
   * were one bucket, and the sentence for it said the requests "could
   * not be moved and are still in that window's queue" -- the opposite
   * of the truth for this half, about work that had in fact arrived.
   * Found in review.
   *
   * Nothing is sent twice: a later takeover offers them again and the
   * destination recognises them by request id. What is wrong is only the
   * bookkeeping in the file this window does not own, and the user is
   * told that rather than told their work is stuck.
   */
  movedButUnmarked: number;
  /*
   * ⚠️ THE DESTINATION COULD NOT SAY WHETHER IT HAS IT.
   *
   * Whether an entry arrived used to be inferred from `adopt` returning
   * without throwing, which is not the same statement: a destination
   * that took the entry and then threw was reported as one that refused
   * it, and one that returned without storing anything was reported as
   * having it. Both sentences were then false, in opposite directions,
   * about somebody's unsent work. Found in review, both reproduced.
   *
   * So membership is ASKED of the destination afterwards rather than
   * inferred -- and when that question cannot be answered either, the
   * answer is this bucket and not a guess. An unknown drawn as one of
   * the two comfortable answers is the shape this batch has met more
   * times than any other.
   */
  outcomeUnknown: number;
}

export function emptyLedger(): TakeoverLedger {
  return {
    observed: 0,
    imported: 0,
    skippedDuplicate: 0,
    leftOtherStore: 0,
    leftUnknownStore: 0,
    unreadableQueue: 0,
    failedToMove: 0,
    movedButUnmarked: 0,
    outcomeUnknown: 0
  };
}

/*
 * The sum of the buckets.
 *
 * ⚠️ IT IS A HAND-WRITTEN SUM AND THE COMPILER WILL NOT NOTICE A MISSING
 * TERM. An earlier comment here claimed it would; a review showed that
 * adding a bucket to the interface and initialising it leaves this
 * function compiling and quietly short. What notices is a cell that adds
 * the ledger's OWN KEYS and compares them with this -- the guard exists,
 * and it is a cell rather than the type system.
 */
export function ledgerTotal(ledger: TakeoverLedger): number {
  return (
    ledger.imported +
    ledger.skippedDuplicate +
    ledger.leftOtherStore +
    ledger.leftUnknownStore +
    ledger.unreadableQueue +
    ledger.failedToMove +
    ledger.movedButUnmarked +
    ledger.outcomeUnknown
  );
}

export class Sessions {
  private readonly files: FileOps;
  private readonly globalStorage: string;
  private readonly startTime: StartTimeReader;
  private mine: string | null = null;
  /*
   * THIS WINDOW'S INCARNATION. `begin` makes it; a claim token carries
   * it so that re-entering a claim is about the window that published
   * it and not about a window that happens to have the same name.
   */
  private nonce: string | null = null;

  constructor(files: FileOps, globalStorage: string, startTime: StartTimeReader = systemStartTime) {
    this.files = files;
    this.globalStorage = globalStorage;
    this.startTime = startTime;
  }

  private sessionsRoot(): string {
    return path.join(this.globalStorage, 'sessions');
  }

  private sessionDirectory(sessionId: string): string {
    return path.join(this.sessionsRoot(), sessionId);
  }

  private identityFile(sessionId: string): string {
    return path.join(this.sessionDirectory(sessionId), 'session.json');
  }

  /*
   * THREE ANSWERS, BECAUSE "ABSENT" AND "UNREADABLE" ARE NOT THE SAME.
   *
   * A missing `session.json` means there is no such session. A file
   * that will not parse, or that carries no pid, means there IS one and
   * this window cannot tell whose -- it may be being written right now,
   * or a read may have failed. Collapsing the two into `null` made
   * `claim` and `discard` skip the liveness check entirely and act on a
   * session that might be running. (§12.21.4, C19)
   */
  private identityOf(sessionId: string): { known: true; identity: SessionIdentity } | { known: false; because: 'absent' | 'unreadable' } {
    const file = this.identityFile(sessionId);
    if (!this.files.exists(file)) {
      /*
       * ⚠️ "NOT THERE" AND "CANNOT LOOK" ARE DIFFERENT ANSWERS, and
       * `exists` gives one word for both: a directory this process may
       * not search reports every path inside it as absent.
       *
       * That matters here more than anywhere else in this file, because
       * a missing record is the ONE undecidable a forced takeover is
       * open for -- and its justification is that no amount of waiting
       * produces the evidence. A permission that could be fixed is
       * exactly the opposite: waiting, or a chmod, does produce it. So
       * the directory is listed, which fails when it cannot be searched,
       * and only a directory this process really can read is allowed to
       * say the record is absent. Found in review.
       */
      const listing = this.files.readDirectory(this.sessionDirectory(sessionId));
      if (!listing.read) {
        return { known: false, because: listing.because === 'absent' ? 'absent' : 'unreadable' };
      }
      return {
        known: false,
        because: listing.names.includes('session.json') ? 'unreadable' : 'absent'
      };
    }
    let raw: Record<string, unknown>;
    try {
      raw = JSON.parse(this.files.readText(file)) as Record<string, unknown>;
    } catch (e) {
      return { known: false, because: 'unreadable' };
    }
    if (typeof raw !== 'object' || raw === null || !Number.isFinite(Number(raw.pid))) {
      return { known: false, because: 'unreadable' };
    }
    return {
      known: true,
      identity: {
        sessionId,
        pid: Number(raw.pid),
        startedAt: typeof raw.startedAt === 'number' ? raw.startedAt : null,
        nonce: String(raw.nonce ?? ''),
        stores: Array.isArray(raw.stores) ? (raw.stores as string[]) : []
      }
    };
  }

  /*
   * The liveness of a session named by id, with "its record cannot be
   * read" answered as cannot-tell rather than as dead.
   */
  /*
   * ⚠️ IT NO LONGER ANSWERS `null` FOR A MISSING RECORD. `null` meant
   * "no record at all" and was read at four call sites as "no such
   * session", which is a different statement: a directory can be there,
   * with a queue in it, and no record beside it. The absence is now a
   * named undecidable, and whether there is a directory is asked
   * separately by whoever cares.
   */
  private async livenessOfSession(sessionId: string): Promise<Liveness> {
    const read = this.identityOf(sessionId);
    if (!read.known) {
      return {
        decidable: false,
        because: read.because === 'absent' ? 'record-missing' : 'record-unreadable'
      };
    }
    return this.livenessOf(read.identity);
  }

  /*
   * `<globalStorage>/sessions/<session-id>/<store-hash>/<id>/`. The id
   * is a uuid made at activation, so two hosts never share a path.
   * (§12.9)
   */
  public directoryFor(sessionId: string, storeHash: string, blockId: string): string {
    return path.join(this.sessionDirectory(sessionId), storeHash, blockId);
  }

  /*
   * Writes `session.json` with pid, that pid's start time and a nonce.
   * (§12.9, §12.11.4)
   */
  public begin(sessionId: string, stores: string[]): SessionIdentity {
    /*
     * ⚠️ AN ID WITH A NEWLINE IN IT IS NOT A NAME, IT IS TWO.
     *
     * A claim token is written as the id on one line and the nonce on
     * the next, and re-entry compares both. An id containing a newline
     * therefore frames its own second line: a window called `"M\n" + b`
     * publishes a token that the window called `M` with nonce `b` reads
     * as its own -- and re-enters a claim it does not hold, which is the
     * double-send the token exists to prevent. Reached through the
     * public API, with both real nonces untouched. Found in review.
     *
     * Production ids are uuids and cannot do this; nothing made that a
     * requirement, so it is one now, checked where the id arrives rather
     * than trusted where it is used.
     */
    if (sessionId.length === 0 || /[\n\r]/.test(sessionId)) {
      throw new Error(
        `a session id may not be empty or contain a line break; got ${JSON.stringify(sessionId)}`
      );
    }
    const identity: SessionIdentity = {
      sessionId,
      pid: process.pid,
      startedAt: this.startTime(process.pid),
      nonce: randomUUID(),
      stores
    };
    /*
     * ⚠️ THE RECORD IS PUBLISHED BEFORE THIS WINDOW CALLS ITSELF BEGUN.
     *
     * These two assignments used to come first, so a failure to write
     * the identity left the object believing it had one: `claim`'s new
     * guard passed, and it published a token naming a window whose
     * `session.json` does not exist -- which nobody, including itself,
     * can judge. Activation propagates the error, so this is about the
     * API's failure path; an object that says who it is when nothing on
     * disk agrees is worth closing anyway. Found in review.
     */
    /*
     * ⚠️ PUBLISHED THROUGH A TEMPORARY FILE, because `writeText` empties
     * the target first. A second `begin` on one object -- or a rerun
     * after a crash -- could therefore leave `session.json` holding half
     * a record, and the object kept the identity it already had: it went
     * on claiming, with a record on disk that nobody, including itself,
     * could read. The record is either the old one or the new one.
     * Found in review.
     */
    this.files.makeDirectory(this.sessionDirectory(sessionId));
    const file = this.identityFile(sessionId);
    /*
     * ⚠️ A NAME NO OTHER WRITER CAN BE USING, AND CLEARED UP IF THE
     * RENAME DOES NOT HAPPEN. `<file>.<pid>.tmp` is the same name on
     * every call in one process, so two writers with that pid would
     * share it; and a failed rename left it lying beside the record for
     * ever. The claim token has used a uuid for this since it was
     * written; this did not. Found in review.
     */
    const temporary = `${file}.${process.pid}.${randomUUID()}.tmp`;
    this.files.writeDurably(temporary, `${JSON.stringify(identity, null, 2)}\n`);
    try {
      this.files.rename(temporary, file);
    } catch (e) {
      /*
       * THE OLD RECORD IS STILL THERE AND THE HALF-WRITTEN ONE IS NOT.
       * Removing the temporary is best effort: failing to remove it must
       * not hide why the publication failed.
       */
      try {
        this.files.unlink(temporary);
      } catch (ignored) {
        /* the reason to report is the rename's, not this one's */
      }
      throw e;
    }
    this.mine = sessionId;
    this.nonce = identity.nonce;
    return identity;
  }

  /*
   * ESRCH first, identity second -- in that order. A pid that is absent
   * is dead with no further question; a pid that is PRESENT may be a
   * reused one, so its start time is compared then. Reversing the two
   * makes a reused pid read as alive for ever, which is the state a
   * reboot leaves behind. (§12.15 修补, §12.21.4, C19)
   *
   * EPERM IS NOT A REASON TO STOP ASKING. It says the pid exists and
   * belongs to someone else -- which a REUSED pid also does. So it
   * goes to the same start-time comparison as the no-throw case, and
   * only its outcome decides. Treating EPERM as alive on its own was
   * in the first draft of this file and contradicts §12.21.4. (C19)
   *
   * IT AWAITS because the start time comes from another program (`ps`,
   * or PowerShell on Windows). A synchronous signature would force
   * either a blocking spawn or a cache, and a cached liveness is a
   * liveness that can be stale exactly when it matters.
   */
  public async livenessOf(identity: SessionIdentity): Promise<Liveness> {
    let permissionDenied = false;
    try {
      process.kill(identity.pid, 0);
    } catch (e) {
      const code = (e as NodeJS.ErrnoException).code;
      if (code === 'ESRCH') {
        /*
         * THE PID IS NOT THERE. Nothing else needs asking, and asking
         * would only add a way to fail. (§12.15 修补, §12.21.4)
         */
        return { alive: false, because: 'pid-absent' };
      }
      if (code === 'EPERM') {
        /*
         * IT EXISTS AND IS SOMEONE ELSE'S -- which a REUSED pid also is.
         * So this is not an answer yet; it goes to the same comparison
         * as the no-throw case. (C19)
         */
        permissionDenied = true;
      } else {
        /*
         * ⚠️ ASKING WHETHER THE PID EXISTS FAILED FOR A REASON NOBODY
         * HERE UNDERSTANDS. That is not the start time being unavailable
         * -- the start time has not been asked for yet -- and labelling
         * it so sends the user a sentence about waiting for a reading
         * this code never took. Found in review.
         */
        return { decidable: false, because: 'liveness-unobtainable' };
      }
    }
    /*
     * ⚠️ TWO WAYS TO HAVE NO START TIME, AND THEY ARE DIFFERENT NEWS.
     * The RECORD may not carry one -- written by an older build, or by a
     * platform that could not supply it -- in which case no later
     * attempt will produce it and "try again in a moment" is advice
     * that cannot work. Or this machine may have failed to read the
     * running process's, which a later attempt may well answer. They
     * were one value, and the sentence for it described the second.
     */
    if (identity.startedAt === null) {
      return { decidable: false, because: 'start-time-unrecorded' };
    }
    const same = sameStart(identity.startedAt, this.startTime(identity.pid));
    if (same === null) {
      return { decidable: false, because: 'start-time-unavailable' };
    }
    if (!same) {
      return { alive: false, because: 'pid-reused' };
    }
    return {
      alive: true,
      because: permissionDenied ? 'identity-matches-permission-denied' : 'identity-matches'
    };
  }

  /*
   * `sessions/<session-id>/<store-hash>/outbox.json`. One writer -- this
   * process -- so it needs no lock, which is the whole reason the queue
   * moved inside the session. A queue outside them, however it were
   * numbered, would be two windows writing one file again. (§12.9, C16)
   *
   * ⚠️ AND ONE PER STORE, WHICH §12.9's WORDING DOES NOT SAY. A queue
   * carries a cursor, and a cursor belongs to one store: with a single
   * queue per session, switching the store left the saver holding a
   * position the new store had never issued, and every save came back
   * `cursor-unreachable`. That was measured in the editor, not
   * reasoned about -- the modules are correct on their own and only the
   * wiring could show it.
   *
   * `storeHash` is optional so that the older one-per-session shape
   * remains addressable for a session's listing, where the store is not
   * known.
   */
  /*
   * ⚠️ AN EMPTY STORE NAME IS NOT A STORE. `path.join(dir, '', 'x')` is
   * `dir/x`, so passing `''` here silently resolved to the queue from
   * before stores had their own directories -- the one whose store
   * nothing can establish, and which an import must not treat as any
   * particular store's. Omitting the argument means that queue on
   * purpose; passing an empty one is a mistake and says so.
   */
  public outboxPathFor(sessionId: string, storeHash?: string): string {
    if (storeHash === undefined) {
      return path.join(this.sessionDirectory(sessionId), 'outbox.json');
    }
    if (storeHash.length === 0) {
      throw new Error('a store name may not be empty; omit the argument for the legacy queue');
    }
    /*
     * ⚠️ IT MUST BE THE NAME OF A DIRECT CHILD OF THE SESSION'S
     * DIRECTORY, AND THAT IS CHECKED HERE RATHER THAN AFTERWARDS.
     *
     * `path.join` resolves `..`, so a store name of `../live/a`
     * addressed a LIVE window's queue -- and a takeover then imported
     * from it and marked it as carried away by a claim on a different
     * session. Production names are digests and cannot do this; nothing
     * made that a requirement. Found in review.
     *
     * THE TEST IS WHAT THE NAME MUST BE, not a list of what it must not
     * contain: `basename` of a single path component is that component,
     * and of anything carrying a separator it is not. A blocklist is a
     * guess at the spellings somebody will try; this is the property.
     * (Ruled by the main session after the review.)
     */
    /*
     * ⚠️ ON EVERY PLATFORM'S RULES, NOT ONLY THIS ONE'S. `path.basename`
     * on POSIX does not treat a backslash as a separator, so `a\\b`
     * passes here and is a path on Windows -- and these names travel:
     * the directory is written by whichever window made it and read by
     * whichever window recovers it. A name that is one component here
     * and two somewhere else is not a name.
     */
    const oneComponent =
      path.posix.basename(storeHash) === storeHash && path.win32.basename(storeHash) === storeHash;
    /*
     * ⚠️ AND NO CONTROL CHARACTER. A NUL cannot occur in a filename on
     * any platform this runs on, and one in a store name got as far as
     * the read, where node refuses it -- and the takeover then counted
     * that as one more queue it could not read, reporting a file that
     * was never there. A name the filesystem cannot hold is refused
     * where names arrive. Found in review.
     */
    if (/[\u0000-\u001f]/.test(storeHash)) {
      throw new Error(
        `a store name may not contain a control character; got ${JSON.stringify(storeHash)}`
      );
    }
    if (!oneComponent || storeHash === '.' || storeHash === '..') {
      throw new Error(
        'a store name must be the name of a single directory inside the session, not a path; ' +
          `got ${JSON.stringify(storeHash)}`
      );
    }
    return path.join(this.sessionDirectory(sessionId), storeHash, 'outbox.json');
  }

  public async others(): Promise<OtherSession[]> {
    const out: OtherSession[] = [];
    for (const name of this.files.list(this.sessionsRoot())) {
      if (name === this.mine || !this.files.isDirectory(this.sessionDirectory(name))) {
        continue;
      }
      const read = this.identityOf(name);
      const identity = read.known ? read.identity : null;
      const liveness = read.known
        ? await this.livenessOf(read.identity)
        : ({
            decidable: false,
            because: read.because === 'absent' ? 'record-missing' : 'record-unreadable'
          } as Liveness);
      const adopters: string[] = [];
      for (const marker of this.files.list(this.sessionDirectory(name))) {
        if (marker.startsWith('adopted-by.') && !marker.includes('.tmp-')) {
          const who = marker.slice('adopted-by.'.length);
          const state = await this.livenessOfSession(who);
          /*
           * AN ADOPTER THIS WINDOW CANNOT JUDGE COUNTS AS PRESENT. The
           * list warns the user before a discard, and a warning that is
           * omitted because a file would not parse is the wrong way to
           * be wrong.
           */
          if (!('alive' in state) || state.alive) {
            adopters.push(who);
          }
        }
      }
      const drafts = this.draftsIn(name);
      const pendingEntries = this.pendingIn(name);
      /*
       * ⚠️ A DIRECTORY WITH NO RECORD IS LISTED ONLY IF THERE IS
       * SOMETHING IN IT TO RECOVER.
       *
       * Every directory under `sessions/` was becoming a row, so an
       * empty `sessions/junk/` -- left by anything at all -- appeared as
       * a window nobody can judge, with an explicit takeover offered for
       * it. Offering an expensive action over nothing teaches the user
       * to press it, which is the opposite of what a second confirmation
       * is for.
       *
       * ⚠️ IT IS THE ABSENCE OF A RECORD THAT MAKES A DIRECTORY
       * ANONYMOUS, not the failure to read one. A `session.json` that
       * will not parse is still a window saying it was here, so that row
       * is listed whatever it holds -- it is the only trace of it, and
       * an unreadable record is never forceable anyway. The first
       * version of this test said `identity === null`, which covers both
       * and hid the unreadable case from the listing.
       */
      const anonymous = !('alive' in liveness) && liveness.because === 'record-missing';
      if (anonymous && drafts.length === 0 && pendingEntries === 0) {
        continue;
      }
      out.push({
        sessionId: name,
        identity,
        liveness,
        drafts,
        pendingEntries,
        liveAdopters: adopters,
        forceable: !('alive' in liveness) && liveness.because === 'record-missing'
      });
    }
    return out;
  }

  /*
   * EVERY QUEUE THIS SESSION HOLDS, ACROSS ALL ITS STORES.
   *
   * ⚠️ THIS READ `<session>/outbox.json` WHILE PRODUCTION WROTE
   * `<session>/<store-hash>/outbox.json`. The writer moved when the
   * cursor turned out to belong to a store; the readers did not, so a
   * real queue with real entries was counted as zero and an import
   * carried nothing. Both halves of a path have to move together --
   * this is the second time in one batch that a writer moved without
   * its readers.
   */
  public outboxPathsFor(sessionId: string): string[] {
    const out: string[] = [];
    const session = this.sessionDirectory(sessionId);
    const legacy = path.join(session, 'outbox.json');
    if (this.files.exists(legacy)) {
      out.push(legacy);
    }
    for (const name of this.files.list(session)) {
      const candidate = path.join(session, name, 'outbox.json');
      if (this.files.isDirectory(path.join(session, name)) && this.files.exists(candidate)) {
        out.push(candidate);
      }
    }
    return out;
  }

  private pendingIn(sessionId: string): number {
    let total = 0;
    for (const file of this.outboxPathsFor(sessionId)) {
      try {
        const raw = JSON.parse(this.files.readText(file)) as { entries?: unknown[] };
        total += Array.isArray(raw.entries) ? raw.entries.length : 0;
      } catch (e) {
        /*
         * A QUEUE THIS BUILD CANNOT READ IS NOT AN EMPTY ONE, and the
         * listing says so by counting it as work rather than as nothing.
         */
        total += 1;
      }
    }
    return total;
  }

  /*
   * `sessions/<dead-id>.claim.<n>`, published by writing a temporary
   * file and `link`ing it into place: a create-once token whose content
   * is complete the moment it is visible. Only offered for a session
   * judged dead. (§12.13.5, §12.11.3, C8)
   *
   * THE SEQUENCE IS NOT THE CALLER'S TO CHOOSE. This reads the tokens
   * that exist: if the newest claimant is still alive the answer is
   * `already-claimed`; otherwise it creates the next number and returns
   * it. A caller that passed a number would have to read the directory
   * to pick one, and two callers reading before either writes is the
   * race the token exists to settle. (§12.11.3)
   */
  /*
   * ⚠️ `forced` IS THE WAY OUT OF ONE UNDECIDABLE AND NOT OF THE OTHERS.
   *
   * It is open only when the record is MISSING -- a failure of evidence
   * that waiting cannot repair. It is closed when the start time could
   * not be obtained, because that is a failure of OBSERVATION on this
   * machine and a second later the same question may answer itself;
   * offering to duplicate a request over that would be offering the cost
   * for nothing. It is closed for a record that will not parse, because
   * that record may yet be read by a build that understands it, and
   * because nothing here can tell a corrupt record from one written by a
   * newer format.
   *
   * WHAT IT COSTS, AND WHY IT IS OFFERABLE AT ALL: if that window is in
   * fact still running, the same request goes to the store twice. The
   * store answers the second by request identity -- `replay` -- and does
   * not apply it again, so the cost is the transmission and not the
   * work. That is the sentence the confirmation has to carry, and it is
   * in status.ts rather than here.
   *
   * IT IS NEVER TAKEN WITHOUT A PERSON ASKING. Nothing in this file
   * calls it; the command does, after a second confirmation. (C13,
   * U-claim)
   */
  public async claim(deadSessionId: string, forced = false): Promise<ClaimOutcome> {
    /*
     * ⚠️ A WINDOW THAT NEVER SAID WHO IT IS CANNOT TAKE A CLAIM.
     *
     * Without `begin` this published a token reading `unnamed` with no
     * nonce -- which its own next call could not re-enter, and which no
     * other window could judge either, so that queue was stranded by an
     * attempt to rescue it. Activation calls `begin` first; nothing said
     * so, and an unguarded precondition is one somebody will meet.
     */
    if (this.mine === null || this.nonce === null) {
      throw new Error('claim was called before begin; this window has not recorded who it is');
    }
    /*
     * THE SESSION BEING TAKEN OVER MUST BE JUDGED DEAD, and a record
     * that cannot be read is not a judgement. Treating "unreadable" as
     * "no such session" let a claim proceed against a window that might
     * still be draining its queue -- which double-sends. (§12.9)
     */
    const liveness = await this.livenessOfSession(deadSessionId);
    if ('alive' in liveness) {
      if (liveness.alive) {
        return { claimed: false, because: 'session-alive' };
      }
    } else if (liveness.because !== 'record-missing') {
      /*
       * A RECORD THAT WILL NOT READ, OR A START TIME THIS MACHINE COULD
       * NOT OBTAIN, IS NOT A JUDGEMENT. Treating either as "no such
       * session" let a claim proceed against a window that might still
       * be draining its queue -- which double-sends. (§12.9)
       */
      return { claimed: false, because: 'undecidable' };
    } else if (this.files.exists(this.sessionDirectory(deadSessionId))) {
      /*
       * A DIRECTORY WITH NO RECORD AT ALL, AND `discard` REFUSES IT FOR
       * THE SAME REASON THIS DOES.
       *
       * A directory that exists without a record is a window in the
       * middle of `begin`: the directory is made first and the record is
       * written after it, so the gap is real and it is on the path every
       * window takes. Taking its queue over produces a second sender for
       * entries the first is still holding, which is the double-send
       * this whole mechanism exists to prevent.
       *
       * ⚠️ THIS IS NOT SYMMETRIC WITH `discard`'S COST. A refused
       * discard leaves files on disk; a claim that should have been
       * refused sends somebody else's requests a second time. Erring
       * toward "still running" is the cheap direction here.
       *
       * ⚠️ AND IT IS NOT THE END OF IT. Refusing for ever would strand
       * the queue of a window whose record was deleted: no sequence of
       * ordinary claims can ever change that answer. `forced` is the way
       * out, and the listing names this state so the user can find it.
       * (U-claim)
       */
      if (!forced) {
        return { claimed: false, because: 'undecidable' };
      }
    } else {
      /*
       * NOTHING OF THAT SESSION IS HERE, NOW. There is no queue to take
       * over and no record to judge, so a token would name a directory
       * that is not there. It does NOT say the session never existed --
       * it may have been discarded, and sibling claim tokens for it may
       * still be lying beside it. `discard` answers `not-found` for the
       * same state, in the same words.
       */
      return { claimed: false, because: 'not-found' };
    }

    const prefix = `${deadSessionId}.claim.`;
    let highest = 0;
    let newest: string | null = null;
    for (const name of this.files.list(this.sessionsRoot())) {
      if (!name.startsWith(prefix) || name.includes('.tmp-')) {
        continue;
      }
      const n = Number(name.slice(prefix.length));
      if (Number.isFinite(n) && n > highest) {
        highest = n;
        newest = name;
      }
    }
    if (newest !== null) {
      /*
       * A TOKEN WHOSE HOLDER IS STILL RUNNING IS THE ANSWER. One whose
       * holder has died is not: the next window takes the next number
       * and carries on, so an interrupted takeover is not the end of
       * the queue. (§12.11.3)
       */
      const written = this.files.readText(path.join(this.sessionsRoot(), newest));
      const holder = written.split('\n')[0];
      /*
       * ⚠️ THE SECOND LINE IS THE NONCE, AND RE-ENTRY NEEDS BOTH.
       *
       * Comparing the id alone was not ownership. The text was trimmed
       * before the comparison, so a window called `"S "` published a
       * token that read back as `"S"` and a DIFFERENT window called
       * `"S"` re-entered its claim -- and sent what the first was still
       * draining, which is the double-send the token exists to prevent.
       * Reproduced in review. A window that died and was replaced by one
       * with the same id was accepted too, and inherited a generation
       * number that then described two claimants.
       *
       * The nonce is made fresh at `begin` and identifies the
       * INCARNATION. A token written before this carries no second line,
       * and is not re-enterable: it falls through to the liveness check
       * below, which is the answer this code gave before re-entry
       * existed and is the conservative one.
       */
      const stamp = written.split('\n')[1] ?? '';
      /*
       * ⚠️ A CLAIM THIS WINDOW ALREADY HOLDS IS RE-ENTERED, NOT REFUSED.
       *
       * The token names a dead SESSION and that session may have a queue
       * per store, so one takeover cannot finish the job: rescuing the
       * first store's requests took the token, and the second store's
       * were then refused as `already-claimed` -- by this window, to
       * this window, for ever. The same wall stood in front of every
       * other way a takeover can stop halfway: an import that threw
       * after moving some entries, or one that found the destination
       * queue unreadable, left this window holding a claim over work it
       * had not moved and no way to try again. Found in review, with
       * each of those reproduced.
       *
       * Re-entering is safe in the way the refusal is meant to be:
       * `already-claimed` exists to stop a SECOND window sending what a
       * first is still draining, and this window is not a second one. It
       * keeps the sequence it already has rather than taking another,
       * because a new number would say a new generation took over.
       * (§12.11.3)
       */
      if (this.mine !== null && holder === this.mine && stamp !== '' && stamp === this.nonce) {
        return {
          claimed: true,
          token: path.join(this.sessionsRoot(), newest),
          sequence: highest
        };
      }
      /*
       * A HOLDER THIS WINDOW CANNOT IDENTIFY COUNTS AS RUNNING. Erring
       * the other way means taking over a queue somebody may still be
       * draining, which double-sends; erring this way costs a takeover
       * that is not offered, and the listing says so. (§12.9)
       */
      const state = await this.livenessOfSession(holder);
      if (!('alive' in state) || state.alive) {
        return { claimed: false, because: 'already-claimed' };
      }
    }

    const sequence = highest + 1;
    const token = path.join(this.sessionsRoot(), `${prefix}${sequence}`);
    const temporary = `${token}.tmp-${randomUUID()}`;
    this.files.makeDirectory(this.sessionsRoot());
    /*
     * THE ID AND THE INCARNATION, one per line. The id is what another
     * window reads to judge the holder; the nonce is what this window
     * reads to know the claim is its OWN and not one belonging to a
     * different window that happens to share a name.
     */
    this.files.writeDurably(temporary, `${this.mine ?? 'unnamed'}\n${this.nonce ?? ''}\n`);
    try {
      this.files.link(temporary, token);
    } catch (e) {
      return { claimed: false, because: 'already-claimed' };
    }
    return { claimed: true, token, sequence };
  }

  /*
   * Copies the dead session's outbox entries into this one's, skipping
   * any whose `req` is already present, and marks them `imported-by` in
   * the source. Never started without a user asking. (§12.11.3, C13)
   */
  /*
   * TAKING OVER A DEAD SESSION'S QUEUE, THROUGH THE QUEUE'S OWN CODE.
   *
   * ⚠️ AN EARLIER VERSION READ AND WROTE THE QUEUE FILE HERE. That threw
   * away everything `Outbox` exists for: a destination it could not
   * parse became `[]` and was then written over; the write truncated the
   * real queue before replacing it, so a stop in between destroyed the
   * adopting session's own entries; and it bypassed the serialisation,
   * so a save already in flight wrote its own idea of the queue back
   * afterwards and the imported entries vanished. A second
   * implementation of a thing that took a dozen rounds to get right is
   * not a shortcut. (§12.11.3)
   *
   * THE IMPORT IS TIED TO THE TOKEN THAT WAS WON, and the source entries
   * are marked with it, so which generation of takeover carried an entry
   * is recorded rather than guessed. (§12.11.3, P2-5)
   */
  /*
   * ⚠️ ONE STORE'S QUEUE, NOT ALL OF THEM.
   *
   * A session writes to as many stores as it was configured for and
   * keeps a queue for each, because a queue carries one cursor and a
   * cursor belongs to one store. This walked every one of them into a
   * single destination -- so a window that had been writing to two
   * stores had both sets of requests dropped into whichever store the
   * adopting window happens to have configured, and its next drain SENT
   * them there. A user agreeing to rescue one store's unsent work has
   * not agreed to redirect those writes into a different store.
   * Reproduced in review; the requests landed and would have been sent.
   *
   * `storeHash` names the destination's store, and only the queue under
   * that name is imported. Omitting it keeps the old behaviour and is
   * for callers that really do mean every queue -- there are none in
   * production, and a cell says so.
   */
  public importFrom(token: ClaimToken, into: ImportTarget, storeHash?: string): TakeoverLedger {
    const ledger = emptyLedger();
    if (!this.files.exists(token.file)) {
      return ledger;
    }
    const all = this.outboxPathsFor(token.deadSessionId);
    /*
     * ⚠️ THE NAMED QUEUE IS NOT FILTERED BY `exists`. It was, and an
     * ancestry this process cannot search makes `exists` answer false
     * for a file that is right there -- so with the enumeration also
     * empty, nothing was loaded and every field of the ledger stayed
     * zero. Loading decides instead: a queue that really is absent loads
     * as an empty one and contributes nothing, and one that cannot be
     * read is counted as unreadable. Found in review.
     */
    const queues =
      storeHash === undefined ? all : [this.outboxPathFor(token.deadSessionId, storeHash)];
    const legacy = path.join(this.sessionDirectory(token.deadSessionId), 'outbox.json');
    /*
     * ⚠️ THE UNION, BECAUSE THE ENUMERATION CAN COME BACK EMPTY. `list`
     * answers `[]` for a directory it cannot read, so a selected queue
     * that plainly exists was never visited and never counted -- an
     * all-zero ledger over a file full of unsent work. Walking the
     * discovered paths AND the selected one, without repeating either,
     * is what makes "everything it saw" include the thing it was asked
     * about. Found in review.
     */
    const seen = new Set<string>();
    const walk: string[] = [];
    for (const queue of [...queues, ...all]) {
      const key = path.resolve(queue);
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      walk.push(queue);
    }
    for (const queue of walk) {
      if (queues.includes(queue)) {
        this.importQueue(queue, token, into, ledger);
        continue;
      }
      this.surveyQueue(queue, path.resolve(queue) === path.resolve(legacy), ledger);
    }
    return ledger;
  }

  /*
   * WHERE AN ENTRY ENDED UP AFTER A MOVE THAT THREW. The destination is
   * asked; if it cannot answer, nothing here knows, and saying so is the
   * whole of what can honestly be reported.
   */
  private whereItEndedUp(
    into: ImportTarget,
    req: string
  ): 'movedButUnmarked' | 'failedToMove' | 'outcomeUnknown' {
    try {
      return into.has(req) ? 'movedButUnmarked' : 'failedToMove';
    } catch (e) {
      return 'outcomeUnknown';
    }
  }

  /*
   * A QUEUE THIS TAKEOVER IS NOT OPENING, COUNTED THROUGH THE SAME DOOR
   * IT WOULD BE OPENED BY.
   *
   * ⚠️ IT USED TO HAVE ITS OWN PARSER, and the two disagreed about which
   * files are acceptable: the survey took `{"cursor":42}` and an entry
   * carrying only a `req` as ordinary work, while the loader refuses
   * both. A count of what is waiting that describes files the import
   * would refuse is a count of something else -- and the advice that
   * goes with it, "configure that store and run this again", would then
   * fail. So the survey loads the queue exactly as the import would, and
   * what it reports is what could actually be recovered. Found in
   * review.
   */
  private surveyQueue(queue: string, unknownStore: boolean, ledger: TakeoverLedger): void {
    const source = new Outbox(queue, this.files);
    try {
      source.load();
    } catch (e) {
      ledger.observed += 1;
      ledger.unreadableQueue += 1;
      return;
    }
    for (const entry of source.entries) {
      ledger.observed += 1;
      /*
       * ALREADY CARRIED AWAY BY AN EARLIER TAKEOVER. `Outbox` reads a
       * mark that is not a string as no mark at all and WILL import that
       * entry, so this asks the loader's own answer rather than the
       * bytes.
       */
      if (entry.importedBy !== null) {
        ledger.skippedDuplicate += 1;
        continue;
      }
      if (unknownStore) {
        ledger.leftUnknownStore += 1;
        continue;
      }
      ledger.leftOtherStore += 1;
    }
  }


  private importQueue(
    queue: string,
    token: ClaimToken,
    into: ImportTarget,
    ledger: TakeoverLedger
  ): void {
    const source = new Outbox(queue, this.files);
    try {
      source.load();
    } catch (e) {
      /*
       * A SOURCE THAT WILL NOT READ IS LEFT ALONE. It holds the only
       * record of that window's unsent work, and a takeover that
       * repaired it would write over exactly what it came to rescue.
       *
       * ⚠️ AND IT IS COUNTED. This answered "nothing imported" and
       * nothing else, so a takeover of the ONE queue it was asked to
       * take reported `imported: 0` with no explanation -- which reads
       * as "there was nothing there" over a file full of somebody's
       * unsent work. Only the queues it did NOT open were counted as
       * unreadable; the one it did open could fail in silence. Found in
       * review.
       */
      ledger.observed += 1;
      ledger.unreadableQueue += 1;
      return;
    }
    for (const entry of source.entries) {
      ledger.observed += 1;
      if (entry.importedBy !== null) {
        ledger.skippedDuplicate += 1;
        continue;
      }
      /*
       * ⚠️ A MOVE THAT THROWS IS A BUCKET, NOT AN ESCAPE.
       *
       * `observed` was counted first and the three things that can throw
       * came after it, so a destination that refused an entry, or a
       * source that could not be marked, left that request in NO bucket
       * -- and the exception went out through `importFrom`, out of the
       * command, so the user was told nothing at all, including about
       * the entries that had already arrived. Found in review with a
       * failing source mark.
       *
       * The request is counted as one this takeover could not move, and
       * the rest of the queue is still walked: one entry the destination
       * would not take is not a reason to abandon the others.
       */
      /*
       * ⚠️ THE TWO FAILURES ARE DIFFERENT NEWS AND ARE COUNTED APART.
       *
       * If the destination refuses the entry, it did not move. If the
       * destination took it and the SOURCE could not be marked, it did
       * move -- and one bucket for both made the report say "could not
       * be moved, still in that window's queue" about work that had
       * arrived. A user acting on that would go looking for it where it
       * is not.
       */
      try {
        /*
         * DEDUPLICATION IS BY REQUEST, NOT BY CONTENT: two saves of one
         * text are two requests and both belong; one request carried
         * across several generations belongs once. (C19)
         */
        if (into.has(entry.req)) {
          ledger.skippedDuplicate += 1;
          continue;
        }
        into.adopt({ ...entry });
        source.markImported(entry.req, `${token.deadSessionId}.claim.${token.sequence}`);
        ledger.imported += 1;
      } catch (e) {
        /*
         * ⚠️ WHETHER IT ARRIVED IS ASKED, NOT INFERRED.
         *
         * This used to read "`adopt` returned without throwing", which
         * is a different statement: a destination that stored the entry
         * and then threw was reported as one that refused it, and one
         * that returned without storing anything was reported as having
         * it -- two sentences, each false, in opposite directions, about
         * somebody's unsent work. The destination's own answer is the
         * only evidence there is; when it cannot give one either, that
         * is a third outcome and not a guess.
         */
        ledger[this.whereItEndedUp(into, entry.req)] += 1;
      }
    }
  }

  public adopt(otherSessionId: string): AdoptOutcome {
    const directory = this.sessionDirectory(otherSessionId);
    if (!this.files.exists(directory)) {
      /*
       * THE DIRECTORY IS NOT RE-CREATED. The user discarded it; putting
       * a marker beside files that are now in the trash would say
       * something untrue about where the documents live. (§12.19.3)
       */
      return { adopted: false, because: 'directory-gone' };
    }
    const marker = path.join(directory, `adopted-by.${this.mine ?? 'unnamed'}`);
    if (this.files.exists(marker)) {
      return { adopted: false, because: 'already-adopted-by-this-session' };
    }
    /*
     * THE TEMPORARY NAME IS NOT A MARKER NAME. Calling it
     * `adopted-by.<sid>.tmp-…` put it inside the prefix the listing
     * scans, so the half-written file was reported as a second window
     * with documents open -- a warning about a window that does not
     * exist, in the confirmation a user reads before discarding.
     */
    const temporary = path.join(directory, `.tmp-adopt-${randomUUID()}`);
    this.files.writeDurably(temporary, `${this.mine ?? 'unnamed'}\n`);
    try {
      this.files.link(temporary, marker);
    } catch (e) {
      return { adopted: false, because: 'already-adopted-by-this-session' };
    }
    return { adopted: true, marker };
  }

  /*
   * Moves a whole DEAD session directory to `trash/<sid>-<time>-<uuid>/`
   * -- files and sidecars together, so no record is separated from what
   * it describes, and to a name that never collides, so nothing is ever
   * overwritten. Refused for a live or undecidable session. (§12.23)
   */
  public async discard(sessionId: string): Promise<DiscardOutcome> {
    const notes = [DISCARD_BACKUP_NOTE, DISCARD_REACH_NOTE];
    const directory = this.sessionDirectory(sessionId);
    const adopters = (await this.others()).find((o) => o.sessionId === sessionId)?.liveAdopters ?? [];
    /*
     * THE SAME RULE AS `claim`: a record that will not read is not a
     * death certificate, and discarding moves a whole directory.
     */
    const liveness = await this.livenessOfSession(sessionId);
    if ('alive' in liveness) {
      if (liveness.alive) {
        return { discarded: false, because: 'session-alive', liveAdopters: adopters, notes };
      }
    } else if (this.files.exists(this.sessionDirectory(sessionId))) {
      /*
       * NOTHING HERE CAN JUDGE IT AND THERE IS SOMETHING TO LOSE -- for
       * any of the three reasons. Discarding moves a whole directory,
       * and unlike a takeover there is no cost this window can offer to
       * pay in exchange: the files would simply be gone.
       */
      return { discarded: false, because: 'undecidable', liveAdopters: adopters, notes };
    }
    if (!this.files.exists(directory)) {
      return { discarded: false, because: 'not-found', liveAdopters: adopters, notes };
    }
    /*
     * THE WHOLE DIRECTORY, TO A NAME THAT CANNOT ALREADY EXIST. Files and
     * their records move together so no record is separated from what it
     * describes, and the unique name means the one operation that moves
     * anything can never be the one that overwrites something. (§12.23)
     */
    const stamp = new Date().toISOString().replace(/[:.]/g, '-');
    const trash = path.join(this.globalStorage, 'trash', `${sessionId}-${stamp}-${randomUUID()}`);
    this.files.makeDirectory(path.dirname(trash));
    this.files.rename(directory, trash);
    return { discarded: true, trash, liveAdopters: adopters, notes };
  }

  /*
   * Every `<n>.md` whose digest differs from its sidecar's
   * `acknowledged-raw`. Computed from the files alone, with no reference
   * to the outbox, so that a save completed but never sent is still
   * found after a restart. (§12.17.4, C6)
   *
   * A `local-only` VERSION IS ALWAYS A DRAFT, whatever its digests say:
   * `reconcile` established that baseline here rather than from the
   * store, so the store has never seen it. (§12.19.2, §12.13.2)
   */
  /*
   * The judge of what a file on disk is. It is made here rather than
   * held, because nothing in a session listing has an editor to offer
   * it -- and it never publishes.
   */
  private publisher(): Publisher {
    return new Publisher(this.files, { isOpen: () => false });
  }

  public draftsIn(sessionId: string): string[] {
    const out: string[] = [];
    const walk = (directory: string): void => {
      for (const name of this.files.list(directory)) {
        const full = path.join(directory, name);
        if (this.files.isDirectory(full)) {
          walk(full);
          continue;
        }
        if (!name.endsWith('.md')) {
          continue;
        }
        const meta = sidecarPathOf(full);
        if (!this.files.exists(meta)) {
          continue;
        }
        /*
         * ⚠️ A RECORD THAT WILL NOT READ IS LISTED, NOT SKIPPED. Skipping
         * it hid exactly the files that most need looking at: a window
         * stopped mid-write leaves an unreadable record beside bytes
         * nobody has sent, and a scan that passed over them made
         * surviving work invisible.
         */
        if (!sidecarFromDisk(this.files.readText(meta)).read) {
          out.push(full);
          continue;
        }
        /*
         * THE SAME RULE AS `standingOf`, ASKED OF IT. A second copy of
         * "what is a draft" is a second answer waiting to disagree --
         * and it did: this one called every freshly published version a
         * draft, because it compared only against `acknowledged-raw`.
         */
        const standing = this.publisher().standingOf(full);
        if (standing.kind === 'third-version' || (standing.kind === 'published' && standing.draft)) {
          out.push(full);
        }
      }
    };
    walk(this.sessionDirectory(sessionId));
    return out;
  }
}
