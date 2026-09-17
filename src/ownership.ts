import {withExclusive,controlDirectory} from './fsops';
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
 * WHO MAY WRITE THE RECORDS BESIDE A BLOCK'S VERSIONS. (§13, r4-3..r7-2)
 *
 * ⚠️ THE UNIT IS THE BLOCK DIRECTORY, NOT THE VERSION.
 *
 * A sidecar belongs to one version, so ownership held per sidecar would
 * mean that publishing a new version produces an unowned record -- and
 * another process could take that one while this one holds the rest.
 * The same block would then have two owners, two confirmed baselines
 * and two `highWater` axes. The block directory is the boundary
 * `publish`, `latestIn` and `reconcile` already share.
 *
 * ⚠️ TAKING IT AND HANDING IT OVER ARE THE SAME PRIMITIVE.
 *
 * Ownership is a record named with its generation, `owner.<N>`, and it
 * is claimed by LINKING a new name: whoever's `link` returns holds it,
 * and the loser gets EEXIST. A rename with a generation check would be
 * half a primitive -- it detects a stale writer and cannot detect two
 * fresh ones both moving N to N+1, because both write N+1 and both
 * stamps then agree.
 *
 * ⚠️ AND IT IS NOT A FENCE.
 *
 * Two processes that both hold a reading of one sidecar can still write
 * over one another: this is the lease/fencing problem, and it is named
 * here rather than solved. What ownership buys is that the loss becomes
 * VISIBLE -- every sidecar write stamps its writer's (session,
 * generation), and a reader that finds a stamp belonging to neither the
 * current owner nor an expected predecessor says so instead of carrying
 * on. Best effort, not a guarantee.
 */

import { randomUUID } from 'crypto';
import * as path from 'path';
import { FileOps, nodeFileOps } from './fsops';

export interface Stamp {
  sessionId: string;
  generation: number;
}

/*
 * WHAT THE OWNER RECORD SAYS.
 *
 * `expected` is the initialisation debt: the stamps a takeover has
 * promised to overwrite, listed PER SIDECAR. It is per sidecar because
 * two versions of one block share a predecessor's stamp, and a debt
 * cleared when the first of them was stamped left the second looking
 * contaminated. A stamp leaves the record only when no sidecar still
 * lists it.
 */
export interface OwnerRecord {
  sessionId: string;
  generation: number;
  expected: Record<string, Stamp[]>;
}

export type Held =
  | { held: true; record: OwnerRecord }
  /*
   * ⚠️ "SOMEBODY ELSE GOT IT" AND "I COULD NOT" ARE DIFFERENT ANSWERS.
   *
   * `link` fails with EEXIST when the name is taken, which is the race
   * being lost and is ordinary. It fails with other codes when the
   * directory cannot be written -- and reporting that as a lost race
   * would tell the user somebody else owns their block, which is the
   * reassuring answer and the wrong one. The caller's next move differs:
   * one is "read who owns it now", the other is "this is not going to
   * work until something is fixed".
   */
  | { held: false; because: 'lost-the-race' | 'unreadable' | 'could-not-write'; detail?: string };

export type OwnerOf =
  | { known: true; record: OwnerRecord | null }
  | { known: false };

/*
 * WHAT A STAMP FOUND IN A SIDECAR MEANS TO THE READER.
 *
 * `expected` is its own answer rather than a shade of `mine`: a
 * predecessor's stamp on a sidecar a takeover has not reached yet is
 * ORDINARY, and reading it as contamination would put every ordinary
 * takeover behind a reconcile nobody needs. It is also not `mine`,
 * because the initialisation step still owes that sidecar a write.
 */
export type StampVerdict = 'mine' | 'expected' | 'suspect';

const OWNER = 'owner.';

function parse(text: string): OwnerRecord | null {
  let raw: unknown;
  try {
    raw = JSON.parse(text);
  } catch (e) {
    return null;
  }
  if (typeof raw !== 'object' || raw === null) {
    return null;
  }
  const held = raw as Record<string, unknown>;
  if (typeof held.sessionId !== 'string' || typeof held.generation !== 'number') {
    return null;
  }
  const expected: Record<string, Stamp[]> = {};
  const listed = held.expected;
  if (typeof listed === 'object' && listed !== null) {
    for (const [name, stamps] of Object.entries(listed as Record<string, unknown>)) {
      if (!Array.isArray(stamps)) {
        continue;
      }
      const kept = stamps.filter(
        (s): s is Stamp =>
          typeof s === 'object' &&
          s !== null &&
          typeof (s as Stamp).sessionId === 'string' &&
          typeof (s as Stamp).generation === 'number'
      );
      if (kept.length > 0) {
        expected[name] = kept;
      }
    }
  }
  return { sessionId: held.sessionId, generation: held.generation, expected };
}

export function sameStamp(a: Stamp, b: Stamp): boolean {
  return a.sessionId === b.sessionId && a.generation === b.generation;
}

/*
 * ⚠️ THE COMPARISON IS THE WHOLE IDENTITY, NOT THE GENERATION.
 *
 * Create-once makes the generations unique, so two owners cannot both
 * be at N. That is an argument about how the number is handed out, and
 * a reader comparing numbers depends on it being true for ever. The
 * record says who as well as when; asking for both costs nothing and
 * does not rest on the argument.
 */
export function verdictOn(writtenBy: Stamp | null, record: OwnerRecord, sidecar: string): StampVerdict {
  if (writtenBy === null) {
    return 'expected';
  }
  if (sameStamp(writtenBy, { sessionId: record.sessionId, generation: record.generation })) {
    return 'mine';
  }
  const owed = record.expected[sidecar] ?? [];
  return owed.some((s) => sameStamp(s, writtenBy)) ? 'expected' : 'suspect';
}

/*
 * The stamp is cleared for ONE sidecar. Whether the debt is gone
 * altogether is a question about the record, and `stillOwed` answers it.
 */
export function stamped(record: OwnerRecord, sidecar: string, applied: Stamp): OwnerRecord {
  const owed = record.expected[sidecar];
  if (owed === undefined) {
    return record;
  }
  const left = owed.filter((s) => !sameStamp(s, applied));
  const expected = { ...record.expected };
  if (left.length === 0) {
    delete expected[sidecar];
  } else {
    expected[sidecar] = left;
  }
  return { ...record, expected };
}

export function stillOwed(record: OwnerRecord): boolean {
  return Object.keys(record.expected).length > 0;
}

export class Owners {
  private readonly files: FileOps;

  constructor(files: FileOps = nodeFileOps) {
    this.files = files;
  }

  /*
   * ⚠️ "NOT OWNED" AND "I COULD NOT LOOK" ARE DIFFERENT ANSWERS, and a
   * directory this process may not search reports everything inside it
   * as absent. A caller deciding whether to take ownership must not read
   * an unreadable directory as a free one.
   */
  private location(directory:string):string {
    const control=controlDirectory(directory);
    const fresh=this.files.readDirectory(control);
    if (!fresh.read && fresh.because==='unreadable') return control;
    if (fresh.read && fresh.names.some(n=>/^owner\.\d+$/.test(n))) return control;
    const legacy=this.files.readDirectory(directory);
    return legacy.read && legacy.names.some(n=>/^owner\.\d+$/.test(n))?directory:control;
  }

  public ownerOf(directory: string): OwnerOf {
    directory=this.location(directory);
    const listing = this.files.readDirectory(directory);
    if (!listing.read) {
      return listing.because === 'absent' ? { known: true, record: null } : { known: false };
    }
    const generations = listing.names
      .filter((name) => name.startsWith(OWNER))
      .map((name) => Number(name.slice(OWNER.length)))
      .filter((n) => Number.isInteger(n) && n >= 0)
      .sort((a, b) => b - a);
    if (generations.length === 0) {
      return { known: true, record: null };
    }
    /*
     * ⚠️ THE HIGHEST GENERATION IS THE ANSWER, AND IF IT DOES NOT READ
     * THE ANSWER IS "I DO NOT KNOW".
     *
     * Reading on to the next one down would hand ownership to whoever
     * can drop a malformed `owner.<huge>` into the directory: the
     * broken newest record would be skipped and an older, valid one
     * would be believed. The name and the contents are written by one
     * act, so a disagreement between them is not a state this program
     * produces.
     */
    const highest = generations[0];
    let text: string;
    try {
      text = this.files.readText(path.join(directory, `${OWNER}${highest}`));
    } catch (e) {
      return { known: false };
    }
    const record = parse(text);
    if (record === null || record.generation !== highest) {
      return { known: false };
    }
    return { known: true, record };
  }

  /*
   * TAKE THE NEXT GENERATION, OR FIND OUT SOMEBODY ELSE DID.
   *
   * The predecessor's stamp is written into the new record's debt for
   * every sidecar in the directory, and whatever the predecessor still
   * owed is carried in with it: a takeover of a session that died
   * half-way through its own initialisation inherits the unfinished
   * part rather than mistaking it for contamination.
   */
  public take(directory: string, sessionId: string, sidecars: string[]): Held {
    return withExclusive(directory, (): Held => {
    const current = this.ownerOf(directory);
    if (!current.known) {
      return { held: false, because: 'unreadable' };
    }
    const previous = current.record;
    const generation = previous === null ? 0 : previous.generation + 1;
    const expected: Record<string, Stamp[]> = {};
    for (const [name, owed] of Object.entries(previous?.expected ?? {})) {
      expected[name] = [...owed];
    }
    if (previous !== null) {
      const theirs: Stamp = { sessionId: previous.sessionId, generation: previous.generation };
      for (const name of sidecars) {
        const owed = expected[name] ?? [];
        if (!owed.some((s) => sameStamp(s, theirs))) {
          expected[name] = [...owed, theirs];
        }
      }
    }
    const record: OwnerRecord = { sessionId, generation, expected };
    const namespace=this.location(directory);
    const name = path.join(namespace, `${OWNER}${generation}`);
    const temporary = `${name}.tmp-${randomUUID()}`;
    this.files.makeDirectory(namespace);
    this.files.writeDurably(temporary, `${JSON.stringify(record)}\n`);
    try {
      this.files.link(temporary, name);
    } catch (e) {
      const code = (e as { code?: string }).code;
      return code === 'EEXIST'
        ? { held: false, because: 'lost-the-race' }
        : { held: false, because: 'could-not-write', detail: String(code ?? e) };
    } finally {
      try {
        this.files.unlink(temporary);
      } catch (e) {
        void e;
      }
    }
    this.files.syncDirectory(namespace);
    return { held: true, record };

    });
  }

  /*
   * REWRITE THE RECORD IN PLACE FOR THE GENERATION THAT HOLDS IT. Only
   * the debt changes; the name carries the generation, so this never
   * moves ownership.
   */
  public rewrite(directory: string, record: OwnerRecord): void {
    return withExclusive(directory, (): void => {
    /*
     * ⚠️ THROUGH A TEMPORARY AND A RENAME, NOT OVER THE RECORD.
     *
     * Writing in place puts a window where the owner record is half a
     * record -- and a record that does not parse is read as "I do not
     * know who owns this", which stops every write to the block. The
     * debt only ever shrinks, so the cost of losing an update is one
     * sidecar being stamped twice; the cost of a torn record is a block
     * nobody can write.
     */
    const namespace=this.location(directory);
    const name = path.join(namespace, `${OWNER}${record.generation}`);
    const temporary = `${name}.tmp-${randomUUID()}`;
    this.files.writeDurably(temporary, `${JSON.stringify(record)}\n`);
    this.files.rename(temporary, name);
    this.files.syncDirectory(namespace);

    });
  }

  /*
   * ⚠️ THE ANSWER IS THREE-WAY, and a boolean would collapse the two
   * that demand opposite things: a directory nobody owns is one this
   * session may take, and a directory it could not read is one where
   * carrying on writes into somebody else's block.
   */
  public mayWrite(
    directory: string,
    sessionId: string
  ): { may: true; record: OwnerRecord } | { may: false; because: 'another-session' | 'unowned' | 'unreadable' } {
    const current = this.ownerOf(directory);
    if (!current.known) {
      return { may: false, because: 'unreadable' };
    }
    if (current.record === null) {
      return { may: false, because: 'unowned' };
    }
    return current.record.sessionId === sessionId
      ? { may: true, record: current.record }
      : { may: false, because: 'another-session' };
  }
}
