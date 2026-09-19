import {withExclusive} from './fsops';
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
 * What this client asked for and has not been told the outcome of.
 *
 * IT IS WRITTEN BEFORE THE REQUEST IS SENT, NOT AFTER. The window this
 * closes is the one between sending and hearing, and an entry recorded
 * after the send does not exist during it: a host that goes away in that
 * window leaves a record in the store that no one here remembers asking
 * for. So a save that cannot be persisted is a save that is not sent.
 *
 * A RETRY SENDS THE SAME BYTES, NOT THE CURRENT ONES. The request id and
 * the cursor are what let the store answer "this exact request already
 * ran" instead of running it twice, and they only mean that while the
 * payload is also the same. Re-reading the file on retry would produce a
 * different request wearing the first one's id, which the store refuses
 * as `req-mismatch` -- correctly, and after the user has been told their
 * work was queued.
 *
 * THE FILE IS REPLACED, NEVER APPENDED TO. A torn append is a JSON file
 * that will not parse, and the whole queue would be lost to save one
 * fsync.
 */

import { FileOps, nodeFileOps } from './fsops';
import { sameWriter } from './cursor';
import { newerEvent } from './publication';
import { SendRecord, freezeRecord } from './record';
import * as path from 'path';

export const OUTBOX_VERSION = 1;

/*
 * THREE STATES, AND THE MIDDLE ONE IS WHY THERE ARE THREE. `queued` is
 * written down and not yet sent; `pending` is sent and answered with
 * something nobody can act on. Between them is `sent`: the request is
 * out and no answer has arrived, which on disk is indistinguishable from
 * `queued` unless it is recorded -- and the difference matters, because
 * a queued entry may still have its cursor corrected and a sent one may
 * not. A host killed between the send and the answer leaves exactly this
 * state behind, and a restart that read it as `queued` would move the
 * cursor of a request the store has already seen, turning a retry into a
 * different request wearing the first one's id.
 */
/*
 * NOTE: `parked` IS NOT `pending`, AND THE DIFFERENCE IS WHO IS WAITING.
 *
 * `pending` means the store was asked and could not say what happened:
 * the answer may still arrive, and nothing past it may go out, because
 * the next send would compose against a position that may have moved.
 * `parked` means this send is not going to be made as it stands -- a
 * later confirmation has overtaken it, or a person has to look at it --
 * so the queue steps over it and carries on with other blocks. One
 * stops the queue; the other is stepped over. (section 13, r3-4)
 */
export type EntryState = 'queued' | 'sent' | 'pending' | 'parked';

export interface OutboxEntry {
  req: string;
  cursor: string;
  id: string;
  field: string;
  payload: string;
  state: EntryState;
  createdAt: number;
  lastError: string | null;
  /*
   * WHICH TAKEOVER CARRIED THIS ENTRY AWAY, named by the claim token
   * that won it. A dead session's queue may be taken over across several
   * generations, and "already imported" has to say by WHOM -- otherwise
   * a later generation cannot tell an entry it should skip from one it
   * should carry. (section 12.11.3)
   */
  importedBy: string | null;
  /*
   * WHAT THE SAVE WAS ABOUT, once the entry carries it. (section 13.1)
   *
   * NOTE: OPTIONAL FOR NOW, AND NOT BECAUSE IT IS OPTIONAL. Section 13
   * makes the entry the record and deletes the map in memory that held
   * this; entries written before that change have no record, and the
   * versioned legacy path reads them. The skeleton step declares the
   * field so section 13's cells compile and read red; nothing writes it yet.
   */
  record?: SendRecord;
}

/*
 * The one place an entry is made unwritable. It is `Object.freeze` and
 * not a deep freeze: every field of an entry is a string, a number or
 * null, so there is nothing under them to protect, and a deep freeze
 * would be a promise this shape does not need and the next field might
 * quietly break.
 */
function freezeEntry(entry: OutboxEntry): OutboxEntry {
  return Object.freeze(entry);
}

interface OutboxFile {
  version: number;
  cursor: string | null;
  entries: OutboxEntry[];
}

/*
 * THE DIRECTORY SYNC MOVED TO `fsops`, with the reason it swallows a
 * failure: some file systems refuse to open a directory for it, and the
 * bytes are already down -- failing the save there would report an
 * error about a record that had in fact been written. It lives with the
 * other operations now so that what this process did to a path can be
 * counted in one place.
 */

function emptyFile(): OutboxFile {
  return { version: OUTBOX_VERSION, cursor: null, entries: [] };
}

export class OutboxWriteError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'OutboxWriteError';
  }
}

/*
 * WHAT A PERSON CAN DO ABOUT IT. Refusing to touch a queue this build
 * cannot read is the right thing to do with it -- but it also means no
 * save will go out until something changes, and a message that stops at
 * "could not be read" leaves the user stuck with no way forward and no
 * idea that there is one. Saying it costs a sentence.
 *
 * THE ADVICE DEPENDS ON THE FAULT, which is why the fault is a
 * parameter. A file whose BYTES this build cannot make sense of is
 * repaired by moving that file; a file this build could not REACH is
 * not, and telling someone to move a file they cannot read is advice
 * that fails in the same way the read did. One sentence for both cases
 * was a sentence that was wrong in one of them.
 *
 * AND IT DOES NOT NAME THE CAUSE. The first version of this branch said
 * "a permission or a device error", which is a guess: the cell that
 * covers it produces EISDIR -- a directory standing where the file
 * belongs -- which is neither. Everything but ENOENT arrives here, so
 * the sentence says where to look rather than what is wrong.
 *
 * MOVING THE FILE ASIDE IS OFFERED, NOT DONE, and its cost is stated
 * rather than softened. The queue holds whole requests, including ones
 * that were never sent: setting it aside does not merely lose a list of
 * unknown outcomes, it takes that unsent work out of every future
 * retry, because the next load finds no file and starts an empty queue.
 * Saying "what is lost is only the record" invited a user to discard
 * saves they still had.
 */
type OutboxFault = 'unreachable' | 'unreadable';

function andWhatToDo(file: string, fault: OutboxFault): string {
  const wayBack =
    fault === 'unreachable'
      ? `Make ${file} readable again -- check the path, its permissions, and the device; ` +
        'the contents may be intact'
      : `Repair ${file}, or move it aside to carry on with an empty queue`;
  return (
    `. It holds this client's record of saves -- both those whose outcome is not known and ` +
    'any that were written down and never sent -- so it is left untouched and no further ' +
    `save will be sent. ${wayBack}. Setting it aside does not touch the store, but the work ` +
    'recorded in it will not be retried, so copy anything out of it that matters first'
  );
}

function withQueueExclusive<T>(file:string,work:()=>T):T {
  try {return withExclusive(file,work);} catch(error) {
    if(error instanceof OutboxWriteError)throw error;
    const wrapped=new OutboxWriteError(`Cannot update ${file}: ${String(error)}`) as OutboxWriteError & {code?:string;cause?:unknown};
    wrapped.code=(error as NodeJS.ErrnoException)?.code;wrapped.cause=error;throw wrapped;
  }
}

export class Outbox {
  private readonly file: string;
  private readonly files: FileOps;
  private data: OutboxFile;
  private readable: boolean;

  constructor(file: string, files: FileOps = nodeFileOps) {
    this.file = file;
    this.files = files;
    this.data = emptyFile();
    /*
     * NOT READABLE UNTIL IT HAS BEEN READ. A queue that was never loaded
     * is not known to be empty, and writing to it would replace whatever
     * is there.
     */
    this.readable = false;
  }

  public get path(): string {
    return this.file;
  }

  /*
   * ONLY A FILE THAT IS NOT THERE IS AN EMPTY QUEUE. Every other reason
   * a read can fail -- a permission that changed, a device error, a
   * directory in the way -- means there may be work recorded that could
   * not be seen, and answering "no work" to that question is how it gets
   * destroyed: the next save persists the empty queue over the file that
   * could not be read. So anything but a missing file is a throw, and a
   * throw stops this outbox being written to at all.
   *
   * A FILE THAT WILL NOT PARSE IS KEPT, NOT REPLACED, for the same
   * reason: it holds the only record of work whose outcome is unknown,
   * and overwriting it turns a problem that can be looked at into one
   * that cannot.
   */
  public load(): void {
    let text: string;
    try {
      text = this.files.readText(this.file);
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
        this.data = emptyFile();
        this.readable = true;
        return;
      }
      this.readable = false;
      throw new OutboxWriteError(
        `the outbox at ${this.file} could not be read: ${String(e)}` +
          `${andWhatToDo(this.file, 'unreachable')}`
      );
    }
    let parsed: unknown;
    try {
      parsed = JSON.parse(text);
    } catch (e) {
      this.readable = false;
      throw new OutboxWriteError(
        `the outbox at ${this.file} is not readable as JSON: ${String(e)}` +
          `${andWhatToDo(this.file, 'unreadable')}`
      );
    }
    try {
      this.data = normalise(parsed);
    } catch (e) {
      this.readable = false;
      throw new OutboxWriteError(
        `the outbox at ${this.file} is in a shape this build cannot read: ` +
          `${(e as Error).message}${andWhatToDo(this.file, 'unreadable')}`
      );
    }
    this.readable = true;
  }

  public get cursor(): string | null {
    return this.data.cursor;
  }

  /*
   * NOTE: THE ENTRIES COME OUT FROZEN.
   *
   * `slice()` copies the ARRAY and hands out the same objects, so a
   * caller could change an entry's `req` -- and the drain's guard, which
   * asks whether the request it just sent is still queued, would then
   * miss and go on sending under a changed identity. No ordinary
   * settlement renames a request; nothing stopped one either, and an
   * invariant nothing enforces is a comment.
   *
   * FREEZING RATHER THAN COPYING, because a copy lets the change happen
   * silently and simply loses it. A frozen object throws on assignment
   * in strict mode -- and every module here is one -- so the attempt
   * arrives as a TypeError at the line that made it rather than as a
   * request that quietly went out twice.
   *
   * IT IS THE STORED OBJECTS THAT ARE FROZEN. They are replaced, never
   * edited, by every mutator below: each builds a fresh state through
   * `copy()` and adopts it only after the disk has changed. Freezing
   * what is handed out would leave the stored one writable, which is
   * where the caller's reference points.
   */
  public get entries(): OutboxEntry[] {
    return this.data.entries.map(freezeEntry);
  }

  /*
   * NULL WHEN THE QUEUE COULD NOT BE READ. The count of entries in a
   * queue nobody could read is not zero; it is not known.
   */
  public get pendingCount(): number | null {
    return this.readable ? this.data.entries.length : null;
  }

  public find(req: string): OutboxEntry | undefined {
    const found = this.data.entries.find((e) => e.req === req);
    return found === undefined ? undefined : freezeEntry(found);
  }

  /*
   * NOTHING CHANGES HERE UNTIL THE DISK HAS CHANGED. Every mutator
   * builds the state it wants, writes THAT, and adopts it only if the
   * write succeeded -- because a queue that changed in memory and not on
   * disk is worse than one that changed in neither: the next call sees
   * the new state, believes it was recorded, and acts on it. An entry
   * removed that way is an edit nobody can find again, and an entry
   * added that way is a request sent with nothing written down.
   */
  /*
   * NOTE: READ BEFORE CHANGING, AND THE REASON LIVES HERE ONLY.
   * (section 13, r5-5)
   *
   * A mutator used to edit whatever this object last read and write the
   * whole file back, so anything another writer had put there in
   * between was overwritten -- not merged, not refused, gone. The queue
   * is a file two processes reach: a takeover writes into a session's
   * queue while that session may be waking up.
   *
   * Every mutator now holds withQueueExclusive from before this refresh
   * through its write. The stable kernel lock prevents two participating
   * processes from reading and overwriting the same old queue snapshot.
   *
   * NOTE: THE RULE IS WRITTEN ONCE. It used to be repeated at all six call
   * sites, which is six places for it to drift and six things to edit
   * when the scope line moves. The call sites say `this.refresh()` and
   * mean it.
   *
   * RE-READ THE FILE, AND LEAVE THIS OBJECT ALONE IF IT CANNOT BE READ.
   *
   * NOTE: `load` IS NOT SAFE TO CALL FROM A MUTATOR ON ITS OWN. When the
   * read fails it marks the queue unreadable, and the entries this
   * object was holding stop being counted -- so a save whose WRITE
   * failed would additionally disappear from "what is still unsent",
   * which is the one number the durability rule exists to keep true.
   * The caller wanted a fresher reading, not the loss of the one it
   * had.
   */
  private refresh(): void {
    const held = this.data;
    const readable = this.readable;
    try {
      this.load();
    } catch (e) {
      this.data = held;
      this.readable = readable;
      throw e;
    }
  }

  private commit(next: OutboxFile): void {
    if (!this.readable) {
      throw new OutboxWriteError(
        `the outbox at ${this.file} has not been read successfully, so it will not be written`
      );
    }
    this.write(next);
    this.data = next;
  }

  private copy(): OutboxFile {
    return {
      version: this.data.version,
      cursor: this.data.cursor,
      entries: this.data.entries.map((e) => ({ ...e }))
    };
  }

  /*
   * THE ENTRY IS ON DISK BEFORE THIS RETURNS. Every caller treats the
   * return as permission to send, so a failure here has to be a throw
   * and not a logged warning.
   */
  public enqueue(entry: OutboxEntry): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    next.entries.push({ ...entry });
    this.commit(next);

    });
  }

  /*
   * FORGET WHERE THE STORE STOOD. (section 13.3)
   *
   * NOTE: THIS IS NOT `setCursor(null)` AND THE DIFFERENCE IS THE POINT. A
   * position is set when an answer establishes one; this says the
   * position we were holding can no longer be vouched for -- an
   * operator determined the work had been carried out and the core does
   * not recover where. The next send asks the store rather than
   * composing against a number nobody stood at.
   */
  public clearCursor(): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    next.cursor = null;
    this.commit(next);

    });
  }

  public setCursor(cursor: string): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    next.cursor = cursor;
    this.commit(next);

    });
  }

  public resolve(req: string, cursor: string | null): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    const before = next.entries.length;
    next.entries = next.entries.filter((e) => e.req !== req);
    /*
     * NOTE: THE CURSOR MOVES ONLY IF THIS QUEUE WAS HOLDING THE REQUEST.
     *
     * It used to move whenever a cursor was passed, for any request id
     * at all -- and that is how another store's cursor was committed
     * into this file: the settler read the live queue rather than its
     * own, found nothing by that id (because the answer was about a
     * different store's request), removed nothing, and wrote the cursor
     * anyway. The window it belonged to kept its entry, unsettled, while
     * this queue advanced to a position its store has never been in.
     * Reproduced in the editor suite.
     *
     * The caller was repaired; this is the second layer, and it is the
     * one that states the rule: a cursor is what an answer to one of MY
     * requests establishes. An answer about a request this queue does
     * not hold establishes nothing here, and the safe direction is to
     * leave the cursor where it is -- the next save composes against a
     * position the store really acknowledged.
     *
     * Every caller in the tree answers a request its own queue holds:
     * both are in the settler, and `drain` sends only `entries[0]` of
     * this queue. Nothing needs the other behaviour; if something ever
     * does, it needs to say WHICH store the cursor came from, which is
     * exactly the fact that went missing here.
     */
    if (cursor !== null && next.entries.length !== before) {
      /*
       * NOTE: AND IT MOVES THE WAY A LOG MOVES. (section 13.3, R10)
       *
       * It used to take whatever position the answer carried, with
       * nothing asked about where this queue already stood. Two things
       * followed, and both are what a cursor exists to prevent: a late
       * replay of an older request carries an older position and wound
       * the queue BACKWARDS to it, so the next save was composed
       * against a place the store had already moved past; and an answer
       * from a different writer carries a position in a different
       * writer's numbering, which is not comparable with this one at
       * all.
       *
       * NOTE: ACROSS WRITERS THE POSITION IS DROPPED, NOT ADOPTED AND NOT
       * KEPT. Adopting it writes a number that means nothing here;
       * keeping the old one asserts a position this answer gives no
       * reason to believe. Dropping it makes the next send ask the
       * store, which is the only thing that is true.
       */
      const held = next.cursor;
      if (held === null) {
        next.cursor = cursor;
      } else if (!sameWriter(held, cursor)) {
        next.cursor = null;
      } else if (newerEvent(held, cursor)) {
        next.cursor = cursor;
      }
    }
    this.commit(next);

    });
  }

  /*
   * A CURSOR MAY BE CORRECTED ONLY BEFORE THE FIRST SEND, and this is
   * the only moment that is known to be before one. While an entry is
   * still queued no store has seen it, so moving it forward to the
   * position the previous answer established is a correction rather than
   * a change of request. Once it has been sent the cursor is part of the
   * request's identity and is never touched again -- that is what makes
   * a retry a retry.
   *
   * THE CORRECTION AND THE RECORD THAT IT IS GOING OUT ARE ONE WRITE.
   * Two writes could be interrupted between, leaving an entry whose
   * cursor had been moved and whose state still said it had never been
   * sent.
   */
  public aboutToSend(req: string, cursor: string | null): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    let changed = false;
    for (const entry of next.entries) {
      if (entry.req !== req || entry.state !== 'queued') {
        continue;
      }
      if (cursor !== null && entry.cursor !== cursor) {
        entry.cursor = cursor;
      }
      entry.state = 'sent';
      changed = true;
    }
    if (changed) {
      this.commit(next);
    }

    });
  }

  /*
   * Records that a takeover has carried this entry into another
   * session's queue. The entry stays here: it is the only trace of what
   * that window was doing, and nothing in this batch deletes such a
   * trace. (section 12.11.3, section 12.23)
   */
  public markImported(req: string, token: string): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    let changed = false;
    for (const entry of next.entries) {
      if (entry.req === req && entry.importedBy === null) {
        entry.importedBy = token;
        changed = true;
      }
    }
    if (changed) {
      this.commit(next);
    }

    });
  }

  /*
   * THIS SEND IS NOT GOING OUT AS IT STANDS, AND THE QUEUE CARRIES ON.
   * (section 13, r3-4)
   */
  public markParked(req: string, why: string): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    for (const entry of next.entries) {
      if (entry.req === req) {
        entry.state = 'parked';
        entry.lastError = why;
      }
    }
    this.commit(next);

    });
  }

  /*
   * PUT EVERY PARKED ENTRY BACK IN THE QUEUE, AND SAY HOW MANY.
   *
   * NOTE: NOT A TIMER AND NOT A RETRY BUTTON. An entry is parked because
   * nothing it can wait for will change the answer -- the store
   * directory does not exist, the socket path is longer than a unix
   * socket name may be, a person has to look at a mismatch. What
   * releases it is the event that could have changed the answer: a
   * configuration change. If it did not, the same rule parks it again
   * on the same attempt, which is why "once" needs no counter here.
   *
   * NOTE: THE REASON STAYS ON THE ENTRY. It is what a person reads to find
   * out what was wrong, and the state is what decides whether it goes
   * out; overwriting the sentence would lose the only record of why it
   * stopped.
   */
  /*
   * NEVER: IT WROTE THE WHOLE QUEUE BACK WITHOUT THE LOCK.
   *
   * Every other mutator here takes `withQueueExclusive` before its
   * refresh, and this one read, edited and committed outside it.
   * Measured in a sixteenth review round with a controlled interleaving:
   * this window read a queue holding one parked request, another window
   * added a save, and the commit here wrote back the file it had read --
   * the second window's save was gone, with no failure anywhere. A
   * read-modify-write of a whole file is exactly the operation the lock
   * exists for, and the reason this one was missed is that it is the
   * only mutator whose body reads like a report: it counts.
   */
  public unparkAll(): number {
    return withQueueExclusive(this.file, (): number => {
    this.refresh();
    const next = this.copy();
    let released = 0;
    for (const entry of next.entries) {
      if (entry.state === 'parked') {
        entry.state = 'queued';
        released += 1;
      }
    }
    if (released > 0) {
      this.commit(next);
    }
    return released;

    });
  }

  public markPending(req: string, why: string): void {
    return withQueueExclusive(this.file, (): void => {
    this.refresh();
    const next = this.copy();
    for (const entry of next.entries) {
      if (entry.req === req) {
        entry.state = 'pending';
        entry.lastError = why;
      }
    }
    this.commit(next);

    });
  }

  /*
   * RENAMED INTO PLACE, AND THE BYTES ARE ON THE DEVICE FIRST. A writer
   * that truncates the real file and then fails has destroyed the queue
   * it was recording; a rename either happens or does not.
   *
   * BUT AN ATOMIC RENAME IS NOT A DURABLE ONE. Rename decides what a
   * reader sees; it says nothing about what survives a machine losing
   * power, and the whole reason this file exists is to be there after
   * the process that wrote it is gone. So the contents are flushed
   * before the rename and the directory entry after it -- two syncs,
   * because the file's bytes and the name that reaches them are two
   * different things to lose.
   */
  private write(next: OutboxFile): void {
    const directory = path.dirname(this.file);
    try {
      this.files.makeDirectory(directory);
    } catch (e) {
      throw new OutboxWriteError(`the outbox directory ${directory} could not be made: ${String(e)}`);
    }
    const temporary = `${this.file}.${process.pid}.tmp`;
    try {
      this.files.writeDurably(temporary, `${JSON.stringify(next, null, 2)}\n`);
      this.files.rename(temporary, this.file);
      this.files.syncDirectory(directory);
    } catch (e) {
      try {
        this.files.unlink(temporary);
      } catch (ignored) {
        /*
         * The temporary file is only litter; the failure to report is
         * the write, and replacing it with the failure to clean up would
         * hide the one that matters.
         */
      }
      throw new OutboxWriteError(`the outbox at ${this.file} could not be written: ${String(e)}`);
    }
  }
}

/*
 * WHAT CAME BACK OFF DISK IS TREATED AS A STRANGER -- AND A STRANGER
 * THIS CANNOT READ IS REFUSED, NOT REPAIRED. It was written by a
 * previous version of this extension, or edited by hand, and an entry
 * missing its cursor would otherwise be sent as `--cursor undefined`.
 *
 * SILENTLY DROPPING THE PARTS IT DID NOT UNDERSTAND WAS WORSE THAN THAT.
 * A file holding `null`, or one entry without a cursor, came back as a
 * queue with that work missing -- and the next save then wrote the
 * repaired queue over the file, so work nobody could read became work
 * nobody had. Refusing leaves the file alone, which is the whole of what
 * `load` promises when it cannot read one.
 */
/*
 * THE ONE READER OF A QUEUE FILE.
 *
 * NOTE: EXPORTED BECAUSE TWO OTHER READERS HAD GROWN BESIDE IT. The
 * recovery listing counted entries with its own checks and the migration
 * guard matched them with its own, and each of them accepted files this
 * one refuses: a queue declaring an unsupported version, an entry with
 * no cursor. Measured in a thirteenth review round -- `{"version": 99,
 * "entries": []}` read as an empty queue here and as unreadable there,
 * so two readers gave opposite verdicts about one file.
 *
 * A rule that lives in one place cannot disagree with itself.
 */
export function readQueueFile(parsed: unknown): OutboxFile {
  return normalise(parsed);
}

function normalise(parsed: unknown): OutboxFile {
  const out = emptyFile();
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
    throw new OutboxWriteError('the outbox is not an object');
  }
  const raw = parsed as { version?: unknown; cursor?: unknown; entries?: unknown };
  if (raw.version !== undefined && raw.version !== OUTBOX_VERSION) {
    throw new OutboxWriteError(
      `the outbox says it is version ${String(raw.version)}, and this build writes ${OUTBOX_VERSION}`
    );
  }
  if (raw.cursor !== undefined && raw.cursor !== null && typeof raw.cursor !== 'string') {
    throw new OutboxWriteError('the outbox cursor is not a string');
  }
  if (typeof raw.cursor === 'string') {
    out.cursor = raw.cursor;
  }
  /*
   * A QUEUE WITH NO `entries` AT ALL IS NOT AN EMPTY QUEUE. Every build
   * that has written this file wrote the key, even when the list was
   * empty, so a file without it is a file this build does not
   * understand -- and treating it as empty would let the next save write
   * over whatever it really was.
   */
  if (!Array.isArray(raw.entries)) {
    throw new OutboxWriteError(
      raw.entries === undefined ? 'the outbox has no entries list' : 'the outbox entries are not a list'
    );
  }
  for (let i = 0; i < raw.entries.length; i += 1) {
    out.entries.push(readEntry(raw.entries[i], i));
  }
  return out;
}

function readEntry(item: unknown, at: number): OutboxEntry {
  if (typeof item !== 'object' || item === null || Array.isArray(item)) {
    throw new OutboxWriteError(`outbox entry ${at} is not an object`);
  }
  const raw = item as Record<string, unknown>;
  for (const name of ['req', 'cursor', 'id', 'field', 'payload']) {
    if (typeof raw[name] !== 'string') {
      throw new OutboxWriteError(
        `outbox entry ${at} has no readable ${name}, so what it was asking for is not known`
      );
    }
  }
  if (raw.state !== undefined && !['queued', 'sent', 'pending', 'parked'].includes(raw.state as string)) {
    throw new OutboxWriteError(`outbox entry ${at} is in a state this build does not know: ${String(raw.state)}`);
  }
  return {
    req: raw.req as string,
    cursor: raw.cursor as string,
    id: raw.id as string,
    field: raw.field as string,
    payload: raw.payload as string,
    state:
      raw.state === 'pending'
        ? 'pending'
        : raw.state === 'parked'
          ? 'parked'
          : raw.state === 'sent'
            ? 'sent'
            : 'queued',
    createdAt: typeof raw.createdAt === 'number' ? raw.createdAt : 0,
    lastError: typeof raw.lastError === 'string' ? raw.lastError : null,
    /*
     * NOTE: THE KEY IS THIS FILE'S CONVENTION, NOT THE SIDECAR'S. The
     * queue has always written its fields under the names the code uses
     * (`lastError`, `createdAt`), because it serialises the record
     * directly; the sidecar, which is new, spells its keys the way section 12
     * does and has an explicit mapping for it. Reading `imported-by`
     * here while the writer emitted `importedBy` meant the mark was
     * written and never read back -- the two halves of one field
     * disagreeing, which nothing but a round-trip notices.
     */
    importedBy: typeof raw.importedBy === 'string' ? (raw.importedBy as string) : null,
    ...readRecord(raw.record, at)
  };
}

/*
 * THE RECORD AN ENTRY CARRIES, READ ALL OF IT OR NONE OF IT.
 *
 * NOTE: ABSENT AND BROKEN ARE DIFFERENT. An entry written before section 13 has
 * no record at all, and that is an ordinary thing with a path of its
 * own: it is sent and dequeued as before, and its answer may not write
 * a baseline, because the provenance a baseline needs was never
 * recorded. An entry whose record is HALF there is a file this build
 * did not write, and repairing it would mean inventing the missing
 * half -- so it refuses, the way every other record on disk does.
 */
function readRecord(raw: unknown, at: number): { record?: SendRecord } {
  if (raw === undefined || raw === null) {
    return {};
  }
  if (typeof raw !== 'object' || Array.isArray(raw)) {
    throw new OutboxWriteError(`outbox entry ${at} has a record that is not an object`);
  }
  const held = raw as Record<string, unknown>;
  const text = (key: string): string | null =>
    typeof held[key] === 'string' ? (held[key] as string) : null;
  const intent = held.intent as Record<string, unknown> | undefined;
  const missing = [
    'req',
    'store',
    'storeHash',
    'blockId',
    'file',
    'rawDigest',
    'sentDigest',
    'prefixDigest'
  ].filter((key) => text(key) === null);
  if (missing.length > 0 || typeof held.seq !== 'number') {
    throw new OutboxWriteError(
      `outbox entry ${at} carries half a record: ${
        missing.length > 0 ? `no ${missing.join(', ')}` : 'no seq'
      }`
    );
  }
  if (
    typeof intent !== 'object' ||
    intent === null ||
    typeof intent.verb !== 'string' ||
    typeof intent.field !== 'string' ||
    typeof intent.body !== 'string' ||
    !(intent.expectation === null || typeof intent.expectation === 'string')
  ) {
    throw new OutboxWriteError(`outbox entry ${at} carries a record with no readable intent`);
  }
  return {
    record: freezeRecord({
      req: text('req') as string,
      store: text('store') as string,
      storeHash: text('storeHash') as string,
      blockId: text('blockId') as string,
      file: text('file') as string,
      rawDigest: text('rawDigest') as string,
      sentDigest: text('sentDigest') as string,
      prefixDigest: text('prefixDigest') as string,
      projectionId: text('projectionId') ?? undefined,
      seq: held.seq,
      intent: {
        verb: intent.verb,
        field: intent.field,
        expectation: intent.expectation as string | null,
        body: intent.body
      }
    })
  };
}

/*
 * ONE FILE PER STORE, NAMED BY THE STORE'S PATH. Two windows open on two
 * stores share a global storage directory, and a single queue would let
 * a request meant for one store be retried against the other -- where
 * its cursor names a writer that does not exist.
 */
export function outboxPathFor(storageDirectory: string, store: string): string {
  return path.join(storageDirectory, namespaceFor(store), 'outbox.json');
}

export function namespaceFor(store: string): string {
  const resolved = path.resolve(store);
  let hash = 0x811c9dc5;
  for (let i = 0; i < resolved.length; i += 1) {
    hash ^= resolved.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  const base = path.basename(resolved).replace(/[^A-Za-z0-9._-]/g, '_') || 'store';
  return `${base}-${hash.toString(16).padStart(8, '0')}`;
}
