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
 * One queue per block file, so that the three critical sections cannot
 * interleave with each other.  (§12.11.1)
 *
 * THE KEY IS THE RESOLVED PATH, NOT THE BLOCK ID. One id under two
 * stores is two files, and two spellings of one path are one file; a
 * queue keyed by either of the other two would serialise the wrong set.
 * (§12.11.1, and C18 exists because a chain keyed by the original
 * string passes every other ordering cell.)
 *
 * AWAITING INSIDE A CRITICAL SECTION IS ALLOWED -- reading the store and
 * writing a file both await. What the chain guarantees is that no two
 * critical sections for one path are in flight together. The editor's
 * own writes are NOT on the chain and cannot be put on it, which is why
 * every critical section re-reads the file digest at its start and
 * checks it again before writing. (§12.11.1)
 */
import * as path from 'path';

export class PathChain {
  /*
   * ONE ENTRY PER FILE, and the entry is the promise the next critical
   * section waits on. `saver.ts` serialises its queue the same way and
   * for the same reason; this is that pattern with the key changed from
   * the outbox path to the block file's.
   */
  private readonly tails = new Map<string, Promise<unknown>>();
  private readonly waiting = new Map<string, number>();
  /*
   * Runs `work` when every critical section already queued for this
   * path has finished. (§12.11.1)
   */
  public run<T>(file: string, work: () => Promise<T>): Promise<T> {
    const key = path.resolve(file);
    this.waiting.set(key, (this.waiting.get(key) ?? 0) + 1);
    const before = this.tails.get(key) ?? Promise.resolve();
    /*
     * THE NEXT SECTION RUNS WHETHER THE LAST ONE SUCCEEDED OR NOT. A
     * chain that only continued on success would be stopped for that
     * file by one refused store read -- and saving would go quiet with
     * nothing reported, which is the failure this whole batch is about.
     */
    const next = before.then(work, work);
    this.tails.set(
      key,
      next.then(
        () => undefined,
        () => undefined
      )
    );
    const done = (): void => {
      const left = (this.waiting.get(key) ?? 1) - 1;
      if (left <= 0) {
        this.waiting.delete(key);
      } else {
        this.waiting.set(key, left);
      }
    };
    next.then(done, done);
    return next;
  }

  /*
   * How many critical sections are queued or running for this path.
   * Present so that a cell can observe the queue rather than infer it
   * from timing. (§12.12 "链" 三格)
   */
  public depth(file: string): number {
    return this.waiting.get(path.resolve(file)) ?? 0;
  }
}
