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
export type Liveness =
  | { alive: true; because: 'identity-matches' | 'identity-matches-permission-denied' }
  | { alive: false; because: 'pid-absent' | 'pid-reused' }
  | { decidable: false; because: 'start-time-unavailable' };

/*
 * A session other than this one, as the "other sessions" list shows it.
 * (§12.19.1)
 */
export interface OtherSession {
  identity: SessionIdentity;
  liveness: Liveness;
  drafts: string[];
  pendingEntries: number;
  liveAdopters: string[];
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

export class Sessions {
  private readonly files: FileOps;
  private readonly globalStorage: string;
  private readonly startTime: StartTimeReader;
  private mine: string | null = null;

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
      return { known: false, because: 'absent' };
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
  private async livenessOfSession(sessionId: string): Promise<Liveness | null> {
    const read = this.identityOf(sessionId);
    if (!read.known) {
      return read.because === 'absent' ? null : { decidable: false, because: 'start-time-unavailable' };
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
    const identity: SessionIdentity = {
      sessionId,
      pid: process.pid,
      startedAt: this.startTime(process.pid),
      nonce: randomUUID(),
      stores
    };
    this.mine = sessionId;
    this.files.makeDirectory(this.sessionDirectory(sessionId));
    this.files.writeText(this.identityFile(sessionId), `${JSON.stringify(identity, null, 2)}\n`);
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
        return { decidable: false, because: 'start-time-unavailable' };
      }
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
  public outboxPathFor(sessionId: string, storeHash?: string): string {
    return storeHash === undefined
      ? path.join(this.sessionDirectory(sessionId), 'outbox.json')
      : path.join(this.sessionDirectory(sessionId), storeHash, 'outbox.json');
  }

  public async others(): Promise<OtherSession[]> {
    const out: OtherSession[] = [];
    for (const name of this.files.list(this.sessionsRoot())) {
      if (name === this.mine || !this.files.isDirectory(this.sessionDirectory(name))) {
        continue;
      }
      const read = this.identityOf(name);
      if (!read.known) {
        continue;
      }
      const identity = read.identity;
      const liveness = await this.livenessOf(identity);
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
          if (state === null || !('alive' in state) || state.alive) {
            adopters.push(who);
          }
        }
      }
      out.push({
        identity,
        liveness,
        drafts: this.draftsIn(name),
        pendingEntries: this.pendingIn(name),
        liveAdopters: adopters
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
  public async claim(deadSessionId: string): Promise<ClaimOutcome> {
    /*
     * THE SESSION BEING TAKEN OVER MUST BE JUDGED DEAD, and a record
     * that cannot be read is not a judgement. Treating "unreadable" as
     * "no such session" let a claim proceed against a window that might
     * still be draining its queue -- which double-sends. (§12.9)
     */
    const liveness = await this.livenessOfSession(deadSessionId);
    if (liveness !== null) {
      if (!('alive' in liveness)) {
        return { claimed: false, because: 'undecidable' };
      }
      if (liveness.alive) {
        return { claimed: false, because: 'session-alive' };
      }
    } else if (this.files.exists(this.sessionDirectory(deadSessionId))) {
      /*
       * A DIRECTORY WITH NO RECORD AT ALL, AND `discard` REFUSES IT FOR
       * THE SAME REASON THIS DOES.
       *
       * `null` here means `session.json` is absent -- not unreadable,
       * which is judged above. A directory that exists without one is a
       * window in the middle of `begin`: the directory is made first and
       * the record is written after it, so the gap is real and it is on
       * the path every window takes. Taking its queue over produces a
       * second sender for entries the first is still holding, which is
       * the double-send this whole mechanism exists to prevent.
       *
       * ⚠️ THIS IS NOT SYMMETRIC WITH `discard`'S COST. A refused
       * discard leaves files on disk; a claim that should have been
       * refused sends somebody else's requests a second time. Erring
       * toward "still running" is the cheap direction here, and the
       * listing tells the user why the takeover was not offered. (§12.9,
       * §12.11.3)
       */
      return { claimed: false, because: 'undecidable' };
    } else {
      /*
       * NOTHING OF THAT SESSION IS HERE. There is no queue to take over
       * and no record to judge, so a token would name a session this
       * disk has never seen. `discard` answers `not-found` for the same
       * state, in the same words.
       */
      return { claimed: false, because: 'not-found' };
    }

    /*
     * WHICH TOKENS EXIST. `.tmp-` names are half-written publications and
     * are not claims: a window that died between writing one and linking
     * it must not strand the queue. (§12.13.5)
     */
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
      const holder = this.files.readText(path.join(this.sessionsRoot(), newest)).trim();
      /*
       * A HOLDER THIS WINDOW CANNOT IDENTIFY COUNTS AS RUNNING. Erring
       * the other way means taking over a queue somebody may still be
       * draining, which double-sends; erring this way costs a takeover
       * that is not offered, and the listing says so. (§12.9)
       */
      const state = await this.livenessOfSession(holder);
      if (state === null || !('alive' in state) || state.alive) {
        return { claimed: false, because: 'already-claimed' };
      }
    }

    const sequence = highest + 1;
    const token = path.join(this.sessionsRoot(), `${prefix}${sequence}`);
    const temporary = `${token}.tmp-${randomUUID()}`;
    this.files.makeDirectory(this.sessionsRoot());
    this.files.writeDurably(temporary, `${this.mine ?? 'unnamed'}\n`);
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
  public importFrom(token: ClaimToken, into: ImportTarget): { imported: number; skipped: number } {
    if (!this.files.exists(token.file)) {
      return { imported: 0, skipped: 0 };
    }
    let imported = 0;
    let skipped = 0;
    for (const queue of this.outboxPathsFor(token.deadSessionId)) {
      const outcome = this.importQueue(queue, token, into);
      imported += outcome.imported;
      skipped += outcome.skipped;
    }
    return { imported, skipped };
  }

  private importQueue(
    queue: string,
    token: ClaimToken,
    into: ImportTarget
  ): { imported: number; skipped: number } {
    const source = new Outbox(queue, this.files);
    try {
      source.load();
    } catch (e) {
      /*
       * A SOURCE THAT WILL NOT READ IS LEFT ALONE. It holds the only
       * record of that window's unsent work, and a takeover that
       * repaired it would write over exactly what it came to rescue.
       */
      return { imported: 0, skipped: 0 };
    }
    let imported = 0;
    let skipped = 0;
    for (const entry of source.entries) {
      if (entry.importedBy !== null) {
        skipped += 1;
        continue;
      }
      /*
       * DEDUPLICATION IS BY REQUEST, NOT BY CONTENT: two saves of one
       * text are two requests and both belong; one request carried
       * across several generations belongs once. (C19)
       */
      if (into.has(entry.req)) {
        skipped += 1;
        continue;
      }
      into.adopt({ ...entry });
      source.markImported(entry.req, `${token.deadSessionId}.claim.${token.sequence}`);
      imported += 1;
    }
    return { imported, skipped };
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
    const adopters = (await this.others()).find((o) => o.identity.sessionId === sessionId)?.liveAdopters ?? [];
    /*
     * THE SAME RULE AS `claim`: a record that will not read is not a
     * death certificate, and discarding moves a whole directory.
     */
    const liveness = await this.livenessOfSession(sessionId);
    if (liveness !== null) {
      if (!('alive' in liveness)) {
        return { discarded: false, because: 'undecidable', liveAdopters: adopters, notes };
      }
      if (liveness.alive) {
        return { discarded: false, because: 'session-alive', liveAdopters: adopters, notes };
      }
    } else if (this.files.exists(this.sessionDirectory(sessionId))) {
      /*
       * A DIRECTORY WITH NO RECORD AT ALL. There is nothing to judge and
       * something to lose, so it is not discarded without one.
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
