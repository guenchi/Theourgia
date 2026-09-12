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
}

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
   */
  public register(file: string, value: T, ticket: number): boolean {
    const held = this.held.get(file);
    if (held !== undefined && held.ticket > ticket) {
      return false;
    }
    this.held.set(file, { value, ticket });
    return true;
  }

  public get(file: string): T | undefined {
    return this.held.get(file)?.value;
  }

  /*
   * A SAVE DOES NOT REORDER ANYTHING. It revises the baseline that is
   * already there, so it keeps that entry's ticket rather than taking a
   * new one -- taking one would let a save promote a stale open above a
   * newer one.
   */
  public revise(file: string, value: T): void {
    const held = this.held.get(file);
    this.held.set(file, { value, ticket: held?.ticket ?? this.claim() });
  }
}
