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
import { digestOfBytes, sidecarFromDisk } from './publication';

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
  if (recorded === null) {
    return true;
  }
  if (now === null) {
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
  | { claimed: false; because: 'already-claimed' | 'session-alive' | 'undecidable' };

/*
 * Which claim an import belongs to. Importing is tied to the token that
 * was won, not to the session id: a later generation takes the NEXT
 * sequence number, and an import that named only the dead session could
 * not tell the two apart. (§12.11.3, C8)
 */
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

  private identityOf(sessionId: string): SessionIdentity | null {
    const file = this.identityFile(sessionId);
    if (!this.files.exists(file)) {
      return null;
    }
    try {
      const raw = JSON.parse(this.files.readText(file)) as Record<string, unknown>;
      return {
        sessionId,
        pid: Number(raw.pid),
        startedAt: typeof raw.startedAt === 'number' ? raw.startedAt : null,
        nonce: String(raw.nonce ?? ''),
        stores: Array.isArray(raw.stores) ? (raw.stores as string[]) : []
      };
    } catch (e) {
      return null;
    }
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
   * `sessions/<session-id>/outbox.json`. One writer, this process, so no
   * lock is needed for it -- which is the whole reason the queue moved
   * inside the session. A queue outside them, however it were numbered,
   * would be two windows writing one file again. (§12.9, C16)
   */
  public outboxPathFor(sessionId: string): string {
    return path.join(this.sessionDirectory(sessionId), 'outbox.json');
  }

  public async others(): Promise<OtherSession[]> {
    const out: OtherSession[] = [];
    for (const name of this.files.list(this.sessionsRoot())) {
      if (name === this.mine || !this.files.isDirectory(this.sessionDirectory(name))) {
        continue;
      }
      const identity = this.identityOf(name);
      if (identity === null) {
        continue;
      }
      const liveness = await this.livenessOf(identity);
      const adopters: string[] = [];
      for (const marker of this.files.list(this.sessionDirectory(name))) {
        if (marker.startsWith('adopted-by.')) {
          const who = marker.slice('adopted-by.'.length);
          const theirs = this.identityOf(who);
          if (theirs !== null) {
            const state = await this.livenessOf(theirs);
            if (!('alive' in state) || state.alive) {
              adopters.push(who);
            }
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

  private pendingIn(sessionId: string): number {
    const file = this.outboxPathFor(sessionId);
    if (!this.files.exists(file)) {
      return 0;
    }
    try {
      const raw = JSON.parse(this.files.readText(file)) as { entries?: unknown[] };
      return Array.isArray(raw.entries) ? raw.entries.length : 0;
    } catch (e) {
      return 0;
    }
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
    const identity = this.identityOf(deadSessionId);
    if (identity !== null) {
      const liveness = await this.livenessOf(identity);
      if (!('alive' in liveness)) {
        return { claimed: false, because: 'undecidable' };
      }
      if (liveness.alive) {
        return { claimed: false, because: 'session-alive' };
      }
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
      const theirs = this.identityOf(holder);
      /*
       * A HOLDER THIS WINDOW CANNOT IDENTIFY COUNTS AS RUNNING. Erring
       * the other way means taking over a queue somebody may still be
       * draining, which double-sends; erring this way costs a takeover
       * that is not offered, and the listing says so. (§12.9)
       */
      if (theirs === null) {
        return { claimed: false, because: 'already-claimed' };
      }
      const state = await this.livenessOf(theirs);
      if (!('alive' in state) || state.alive) {
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
  public importFrom(token: ClaimToken): { imported: number; skipped: number } {
    const source = this.outboxPathFor(token.deadSessionId);
    if (!this.files.exists(source) || this.mine === null) {
      return { imported: 0, skipped: 0 };
    }
    const readQueue = (file: string): Array<Record<string, unknown>> => {
      if (!this.files.exists(file)) {
        return [];
      }
      try {
        const raw = JSON.parse(this.files.readText(file)) as { entries?: unknown[] };
        return Array.isArray(raw.entries) ? (raw.entries as Array<Record<string, unknown>>) : [];
      } catch (e) {
        return [];
      }
    };
    const theirs = readQueue(source);
    const target = this.outboxPathFor(this.mine);
    const ours = readQueue(target);
    /*
     * DEDUPLICATION IS BY REQUEST, NOT BY CONTENT. Two saves of the same
     * text are two requests and both belong; one request carried across
     * several generations of takeover belongs once. (§12.11.3, C19)
     */
    const have = new Set(ours.map((e) => String(e.req)));
    let imported = 0;
    let skipped = 0;
    for (const entry of theirs) {
      if (have.has(String(entry.req))) {
        skipped += 1;
        continue;
      }
      ours.push({ ...entry, 'imported-from': token.deadSessionId });
      have.add(String(entry.req));
      imported += 1;
    }
    this.files.writeDurably(target, `${JSON.stringify({ version: 1, cursor: null, entries: ours }, null, 2)}\n`);
    return { imported, skipped };
  }

  /*
   * `adopted-by.<sid>`, created once with `link`. Display and save
   * routing only: it never decides whether anything may be written.
   * (§12.19.3, C9)
   */
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
    const temporary = `${marker}.tmp-${randomUUID()}`;
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
    const identity = this.identityOf(sessionId);
    const adopters = identity === null ? [] : (await this.others()).find((o) => o.identity.sessionId === sessionId)?.liveAdopters ?? [];
    if (identity !== null) {
      const liveness = await this.livenessOf(identity);
      if (!('alive' in liveness)) {
        return { discarded: false, because: 'undecidable', liveAdopters: adopters, notes };
      }
      if (liveness.alive) {
        return { discarded: false, because: 'session-alive', liveAdopters: adopters, notes };
      }
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
        const meta = `${full}.meta`;
        if (!this.files.exists(meta)) {
          continue;
        }
        const read = sidecarFromDisk(this.files.readText(meta));
        if (!read.read) {
          continue;
        }
        const digest = digestOfBytes(this.files.readBytes(full));
        /*
         * A `local-only` VERSION IS ALWAYS A DRAFT: its baseline came
         * from the file rather than from an answer, so the store has
         * never seen it whatever the digests say. (§12.19.2)
         */
        if (read.sidecar.localOnly || read.sidecar.acknowledgedRaw !== digest) {
          out.push(full);
        }
      }
    };
    walk(this.sessionDirectory(sessionId));
    return out;
  }
}
