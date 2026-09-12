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
 * A REQUEST THAT HAS BEEN RETIRED, AND WHERE THAT IS RECORDED.
 * (§13, r4-2, r5-3)
 *
 * ⚠️ CANCELLING IS A TOMBSTONE, NOT A DEQUEUE. An entry removed from a
 * queue is removed from ONE queue: the same request may sit in a dead
 * session's file that a takeover has not reached yet, and the next
 * takeover carries the cancelled work back in. A tombstone is the fact
 * that outlives every copy.
 *
 * ⚠️ AND IT LIVES UNDER THE STORAGE ROOT, not in a session directory.
 * Discarding a session moves its directory away; the tombstone has to
 * survive that, because the copies it is about do not all live there.
 *
 * ⚠️ IT IS PROBED BY NAME. Whoever asks holds a request id and a store;
 * that is a path, and the answer is whether the path is there. Listing
 * the directory turns every check into work proportional to how much
 * has ever been cancelled -- and the check sits on the import path, the
 * repair path and in front of every transmission.
 */

import * as path from 'path';
import { FileOps, nodeFileOps } from './fsops';
import { under } from './paths';

const RETIRED = 'retired';

const STORE: { noun: string; inside: string } = {
  noun: 'a store name',
  inside: 'the retired directory'
};

/*
 * ⚠️ THE REQUEST ID IS UNTRUSTED INPUT. It is read out of a queue file
 * written by another session -- which a user can edit, and which a
 * takeover reads before anything has vouched for it. It goes through
 * the same rule as the store name and for the same reason.
 */
const REQ: { noun: string; inside: string } = {
  noun: 'a request id',
  inside: 'the store’s retired directory'
};

export type Retired =
  | { known: true; retired: boolean }
  /*
   * ⚠️ "I COULD NOT LOOK" IS NOT "NOTHING IS RETIRED". A directory this
   * process may not search reports everything inside it as absent, and
   * the caller asking this question is deciding whether to SEND. The
   * safe reading of "I do not know" is not the convenient one.
   */
  | { known: false };

export class Tombstones {
  private readonly files: FileOps;
  private readonly storage: string;

  constructor(storage: string, files: FileOps = nodeFileOps) {
    this.storage = storage;
    this.files = files;
  }

  public pathFor(storeHash: string, req: string): string {
    return under(this.storage, [
      { name: RETIRED, kind: STORE },
      { name: storeHash, kind: STORE },
      { name: req, kind: REQ }
    ]);
  }

  /*
   * ⚠️ THE FACT IS THE NAME. What is inside the file is for a person
   * reading the directory; nothing in this program reads it, because a
   * tombstone whose meaning depended on its contents would have a state
   * where it exists and says nothing -- and that state would arrive
   * exactly when a process stopped between creating and writing.
   *
   * Retiring twice is not an error: the caller may be re-entering a
   * cancellation that was interrupted, and the second call has nothing
   * to add.
   */
  public retire(storeHash: string, req: string): void {
    const file = this.pathFor(storeHash, req);
    this.files.makeDirectory(path.dirname(file));
    this.files.writeDurably(file, `retired ${req}\n`);
    this.files.syncDirectory(path.dirname(file));
  }

  public isRetired(storeHash: string, req: string): Retired {
    const found = this.files.presenceOf(this.pathFor(storeHash, req));
    return found.known ? { known: true, retired: found.there } : { known: false };
  }
}
