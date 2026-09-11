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

export type EntryState = 'queued' | 'pending';

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

  constructor(file: string) {
    this.file = file;
    this.data = emptyFile();
  }

  public get path(): string {
    return this.file;
  }

  /*
   * A FILE THAT WILL NOT PARSE IS KEPT, NOT REPLACED. It holds the only
   * record of work whose outcome is unknown; overwriting it with an
   * empty queue would turn a problem that can be looked at into one that
   * cannot.
   */
  public load(): void {
    let text: string;
    try {
      text = fs.readFileSync(this.file, 'utf8');
    } catch (e) {
      this.data = emptyFile();
      return;
    }
    let parsed: unknown;
    try {
      parsed = JSON.parse(text);
    } catch (e) {
      throw new OutboxWriteError(
        `the outbox at ${this.file} could not be read as JSON and was left alone: ${String(e)}`
      );
    }
    this.data = normalise(parsed);
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
   * THE ENTRY IS ON DISK BEFORE THIS RETURNS. Every caller treats the
   * return as permission to send, so a failure here has to be a throw
   * and not a logged warning.
   */
  public enqueue(entry: OutboxEntry): void {
    this.data.entries.push(entry);
    this.persist();
  }

  public setCursor(cursor: string): void {
    this.data.cursor = cursor;
    this.persist();
  }

  public resolve(req: string, cursor: string | null): void {
    this.data.entries = this.data.entries.filter((e) => e.req !== req);
    if (cursor !== null) {
      this.data.cursor = cursor;
    }
    this.persist();
  }

  /*
   * A CURSOR MAY BE CORRECTED ONLY BEFORE THE FIRST SEND. While an
   * entry is still queued no store has seen it, so moving it forward to
   * the position the previous answer established is a correction rather
   * than a change of request. Once it has been sent the cursor is part
   * of the request's identity and is never touched again -- that is what
   * makes a retry a retry.
   */
  public retarget(req: string, cursor: string): void {
    let changed = false;
    for (const entry of this.data.entries) {
      if (entry.req === req && entry.state === 'queued' && entry.cursor !== cursor) {
        entry.cursor = cursor;
        changed = true;
      }
    }
    if (changed) {
      this.persist();
    }
  }

  public markPending(req: string, why: string): void {
    for (const entry of this.data.entries) {
      if (entry.req === req) {
        entry.state = 'pending';
        entry.lastError = why;
      }
    }
    this.persist();
  }

  private persist(): void {
    const directory = path.dirname(this.file);
    try {
      fs.mkdirSync(directory, { recursive: true });
    } catch (e) {
      throw new OutboxWriteError(`the outbox directory ${directory} could not be made: ${String(e)}`);
    }
    /*
     * RENAMED INTO PLACE. A writer that truncates the real file and then
     * fails has destroyed the queue it was recording; a rename either
     * happens or does not.
     */
    const temporary = `${this.file}.${process.pid}.tmp`;
    try {
      fs.writeFileSync(temporary, `${JSON.stringify(this.data, null, 2)}\n`, 'utf8');
      fs.renameSync(temporary, this.file);
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
    state: raw.state === 'pending' ? 'pending' : 'queued',
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
