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
 * Which block each open buffer holds, and whose reading of it is the
 * newest.
 *
 * THE BASELINE IS WHAT A SAVE IS MEASURED AGAINST. A buffer's entry here
 * carries the text the store last gave for that block, and the save path
 * splits the buffer against its `prefix`: front matter plus heading. An
 * entry that is older than the buffer makes that split wrong, and the
 * way it goes wrong is not a refusal -- a prefix that no longer matches
 * what the buffer starts with turns the heading into part of the body,
 * and the body is what gets written. So the store receives its own
 * heading back as content.
 *
 * OPENING A BLOCK IS SEVERAL WAITS LONG, and two opens of the same block
 * can be in flight together -- the user clicking twice, or a refresh
 * arriving during a click. Whichever finishes LAST used to win, which
 * has nothing to do with which read the store answered most recently.
 * So each open takes a ticket before it reads, and a registration is
 * refused if a later ticket has already registered for that file.
 *
 * IT IS A VALUE RATHER THAN A MAP IN THE EXTENSION so that the ordering
 * rule can be driven directly by a cell. The interleaving that breaks it
 * cannot be produced through the editor's own API -- the waits are VS
 * Code's and nothing can widen them -- so a rule that lived inside
 * `activate` would be a rule nothing could test.
 */

export interface Registered<T> {
  value: T;
  ticket: number;
  /*
   * WHICH KIND OF EVENT PUT IT THERE. A registration refused because a
   * newer READ won needs no explanation: that read is on the screen. One
   * refused because a SAVE was confirmed leaves nothing in its place, so
   * the person who asked for the block has to be told why they did not
   * get it.
   */
  from: Outcome;
}

export type Outcome = 'read' | 'save';

/*
 * `taken` means this registration is now the baseline. The other two say
 * which kind of event outranked it, because the answer to a refusal
 * differs: one is already visible to the user, the other is not.
 */
export type Admission = 'taken' | 'superseded-by-read' | 'superseded-by-save';

export class OpenBuffers<T> {
  private readonly held = new Map<string, Registered<T>>();
  private next = 0;

  /*
   * TAKEN BEFORE THE READ, NOT AFTER IT. The ticket has to order the
   * moment each open STARTED asking the store; handing them out at
   * registration time would number them by finishing order, which is
   * the very thing that must not decide the winner.
   */
  public claim(): number {
    this.next += 1;
    return this.next;
  }

  /*
   * Returns whether this registration was taken. A refusal is not an
   * error: it means a newer reading of the same block is already the
   * baseline, which is the outcome this exists to produce.
   *
   * `commit` IS RUN BETWEEN THE DECISION AND THE RECORD, and that is the
   * whole reason it is a parameter rather than the caller's next line.
   * Opening a block writes the file before the baseline is recorded; if
   * the write is admitted and then fails, a caller that had already
   * recorded the baseline leaves one describing a file that was never
   * written -- and when the prefix of that baseline is empty, the save
   * path does not refuse the mismatch, it takes the buffer's heading for
   * body and sends it. Running the write here means a failure records
   * nothing, and nothing can interleave between the two: `commit` is
   * synchronous and so is this.
   */
  public register(file: string, value: T, ticket: number, commit?: () => void): Admission {
    const held = this.held.get(file);
    if (held !== undefined && held.ticket > ticket) {
      return held.from === 'save' ? 'superseded-by-save' : 'superseded-by-read';
    }
    if (commit !== undefined) {
      commit();
    }
    this.held.set(file, { value, ticket, from: 'read' });
    return 'taken';
  }

  public get(file: string): T | undefined {
    return this.held.get(file)?.value;
  }

  /*
   * WHAT THE STORE CONFIRMED IS THE NEWEST THING ANYONE KNOWS. A save
   * that has been answered establishes the block's content at a later
   * moment than any read that was already in flight, so it takes a fresh
   * ticket -- the highest -- and every open that started earlier is
   * refused when it arrives.
   *
   * THE COST OF THIS RULE IS STATED RATHER THAN HIDDEN. It is an
   * admission policy, not a claim about which reading is fresher: a read
   * that began before a save was confirmed may still have sampled the
   * store after it, and this refuses that genuinely newer reading. The
   * refusal is safe -- nothing is overwritten -- but it leaves the user
   * without the block they asked for, so the refusal says which kind of
   * event outranked it and the caller tells them to ask again.
   *
   * AN EARLIER VERSION KEPT THE TICKET THAT WAS ALREADY THERE, on the
   * reasoning that a save reorders nothing. It admitted two sequences.
   * One: a save captures the baseline, a newer open replaces it, and the
   * save then installs its own older reading under the newer open's
   * ticket. The other: an open starts reading, the user saves, the save
   * is confirmed and the file marked committed, and the open's older
   * answer then arrives with a ticket above the baseline's and
   * overwrites the file the user just saved. Ordering opens against each
   * other is not enough; what a save confirmed has to outrank a read
   * that began before it.
   */
  public confirmed(file: string, value: T): void {
    this.held.set(file, { value, ticket: this.claim(), from: 'save' });
  }
}
