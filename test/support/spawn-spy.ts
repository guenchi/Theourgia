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
 * Counting where the process is actually started.
 *
 * A WRAPPER ROUND THE TRANSPORT COUNTS CALLS INTO IT, NOT PROCESSES OUT
 * OF IT. If the transport itself started the core twice and returned the
 * first answer, a wrapper would see one call and the store would record
 * one event -- the duplicate would be invisible from both ends. The
 * stand-in cells count arrivals at the subprocess; this is the same
 * vantage point for the real core.
 *
 * IT CHANGES NOTHING IT OBSERVES. The original is called with the same
 * arguments and its result returned untouched; the only effect is a line
 * in a list. And it is put back in a `finally`, because a spy left
 * installed would follow every later cell in the run.
 */

import * as childProcess from 'child_process';

type Spawn = typeof childProcess.spawn;

export class SpawnSpy {
  private original: Spawn | null = null;
  public readonly launches: string[][] = [];

  public install(): void {
    if (this.original !== null) {
      throw new Error('the spawn spy is already installed');
    }
    this.original = childProcess.spawn;
    const original = this.original;
    const launches = this.launches;
    (childProcess as { spawn: Spawn }).spawn = function spy(
      this: unknown,
      command: string,
      ...rest: unknown[]
    ) {
      const args = Array.isArray(rest[0]) ? (rest[0] as string[]) : [];
      launches.push([command, ...args]);
      return (original as (...a: unknown[]) => unknown).call(this, command, ...rest);
    } as unknown as Spawn;
  }

  public remove(): void {
    if (this.original !== null) {
      (childProcess as { spawn: Spawn }).spawn = this.original;
      this.original = null;
    }
  }

  /*
   * The launches whose argument vector carries this verb. The verb is
   * the first argument after the script path, so a launch is matched by
   * the presence of the verb rather than by its position -- the position
   * is pinned by its own cell.
   */
  public carrying(verb: string): string[][] {
    return this.launches.filter((l) => l.includes(verb));
  }
}
