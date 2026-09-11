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

import * as fs from 'fs';
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
export type EntryState = 'queued' | 'sent' | 'pending';

export interface OutboxEntry {
  req: string;
  cursor: string;
  id: string;
  field: string;
  payload: string;
  state: EntryState;
  createdAt: number;
  lastError: string | null;
}

interface OutboxFile {
  version: number;
  cursor: string | null;
  entries: OutboxEntry[];
}

/*
 * A DIRECTORY THAT CANNOT BE SYNCED IS NOT A FAILED WRITE. Some file
 * systems refuse to open a directory for this, and the bytes are already
 * down; failing the save at that point would report an error about a
 * record that had in fact been written.
 */
function syncDirectory(directory: string): void {
  let handle: number | null = null;
  try {
    handle = fs.openSync(directory, 'r');
    fs.fsyncSync(handle);
  } catch (e) {
    return;
  } finally {
    if (handle !== null) {
      try {
        fs.closeSync(handle);
      } catch (ignored) {
        return;
      }
    }
  }
}

function emptyFile(): OutboxFile {
  return { version: OUTBOX_VERSION, cursor: null, entries: [] };
}

export class OutboxWriteError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'OutboxWriteError';
  }
}

export class Outbox {
  private readonly file: string;
  private data: OutboxFile;
  private readable: boolean;

  constructor(file: string) {
    this.file = file;
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
      text = fs.readFileSync(this.file, 'utf8');
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
        this.data = emptyFile();
        this.readable = true;
        return;
      }
      this.readable = false;
      throw new OutboxWriteError(
        `the outbox at ${this.file} could not be read and was left alone: ${String(e)}`
      );
    }
    let parsed: unknown;
    try {
      parsed = JSON.parse(text);
    } catch (e) {
      this.readable = false;
      throw new OutboxWriteError(
        `the outbox at ${this.file} could not be read as JSON and was left alone: ${String(e)}`
      );
    }
    this.data = normalise(parsed);
    this.readable = true;
  }

  public get cursor(): string | null {
    return this.data.cursor;
  }

  public get entries(): OutboxEntry[] {
    return this.data.entries.slice();
  }

  public get pendingCount(): number {
    return this.data.entries.length;
  }

  public find(req: string): OutboxEntry | undefined {
    return this.data.entries.find((e) => e.req === req);
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
    const next = this.copy();
    next.entries.push({ ...entry });
    this.commit(next);
  }

  public setCursor(cursor: string): void {
    const next = this.copy();
    next.cursor = cursor;
    this.commit(next);
  }

  public resolve(req: string, cursor: string | null): void {
    const next = this.copy();
    next.entries = next.entries.filter((e) => e.req !== req);
    if (cursor !== null) {
      next.cursor = cursor;
    }
    this.commit(next);
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
  }

  public markPending(req: string, why: string): void {
    const next = this.copy();
    for (const entry of next.entries) {
      if (entry.req === req) {
        entry.state = 'pending';
        entry.lastError = why;
      }
    }
    this.commit(next);
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
      fs.mkdirSync(directory, { recursive: true });
    } catch (e) {
      throw new OutboxWriteError(`the outbox directory ${directory} could not be made: ${String(e)}`);
    }
    const temporary = `${this.file}.${process.pid}.tmp`;
    try {
      const handle = fs.openSync(temporary, 'w');
      try {
        fs.writeFileSync(handle, `${JSON.stringify(next, null, 2)}\n`, 'utf8');
        fs.fsyncSync(handle);
      } finally {
        fs.closeSync(handle);
      }
      fs.renameSync(temporary, this.file);
      syncDirectory(directory);
    } catch (e) {
      try {
        fs.unlinkSync(temporary);
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
 * WHAT CAME BACK OFF DISK IS TREATED AS A STRANGER. It was written by a
 * previous version of this extension, or edited by hand, and an entry
 * missing its cursor would otherwise be sent as `--cursor undefined`.
 */
function normalise(parsed: unknown): OutboxFile {
  const out = emptyFile();
  if (typeof parsed !== 'object' || parsed === null) {
    return out;
  }
  const raw = parsed as { version?: unknown; cursor?: unknown; entries?: unknown };
  if (typeof raw.cursor === 'string') {
    out.cursor = raw.cursor;
  }
  if (!Array.isArray(raw.entries)) {
    return out;
  }
  for (const item of raw.entries) {
    const entry = normaliseEntry(item);
    if (entry !== null) {
      out.entries.push(entry);
    }
  }
  return out;
}

function normaliseEntry(item: unknown): OutboxEntry | null {
  if (typeof item !== 'object' || item === null) {
    return null;
  }
  const raw = item as Record<string, unknown>;
  if (
    typeof raw.req !== 'string' ||
    typeof raw.cursor !== 'string' ||
    typeof raw.id !== 'string' ||
    typeof raw.field !== 'string' ||
    typeof raw.payload !== 'string'
  ) {
    return null;
  }
  return {
    req: raw.req,
    cursor: raw.cursor,
    id: raw.id,
    field: raw.field,
    payload: raw.payload,
    state: raw.state === 'pending' ? 'pending' : raw.state === 'sent' ? 'sent' : 'queued',
    createdAt: typeof raw.createdAt === 'number' ? raw.createdAt : 0,
    lastError: typeof raw.lastError === 'string' ? raw.lastError : null
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
