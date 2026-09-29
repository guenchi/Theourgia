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
 * Where the user's own sockets live, and what has appeared there.
 *
 * KEY: ONE COPY, BECAUSE A SECOND COPY IS A SECOND PLACE FOR THE DEFECT.
 * These lived inside `run-root.test.ts`, and the cells that reach a real
 * core had their own `entriesIn` beside them. The copy in the test file
 * was repaired to refuse an unreadable directory instead of calling it
 * empty; the other copy was not, because the repair was made where the
 * finding pointed. They are here so that there is nowhere for them to
 * drift apart. NEVER: not in a test file -- importing one test file from
 * another registers its cells twice, which the census reads as a suite
 * having grown.
 */

import * as fs from 'fs';
import * as path from 'path';

/*
 * SPELLED THE WAY THE CORE SPELLS IT (`run-root`, client.sc:73-75). NEVER: Not read from
 * `THEOURGIA_RUN`: a process that has one set would then be measuring
 * some scratch directory against itself, and this could never fail.
 */
export function usersRunRoot(): string {
  return path.join(process.env.HOME ?? '/tmp', '.theourgia', 'run');
}

/*
 * NEVER: ONLY AN ABSENT DIRECTORY IS AN EMPTY ONE.
 *
 * This caught every error and answered with an empty list, so a run root
 * that could not be READ -- a permission, a broken mount -- was reported
 * as holding nothing, and the growth this gate exists to catch was
 * hidden by the gate itself. Measured in a ninth review round with
 * EACCES injected: `additionsTo` answered "nothing was added" while the
 * directory filled up.
 *
 * A directory that is not there really is empty, and that case is
 * ordinary: the user may never have run the core. Every other failure is
 * a reading this cannot take, and it says so.
 */
export function entriesIn(dir: string): string[] {
  try {
    return fs.readdirSync(dir).sort();
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
      return [];
    }
    throw new Error(`could not read ${dir}, so nothing here can say what is in it: ${(e as Error).message}`);
  }
}

/*
 * WHAT APPEARED IN A DIRECTORY SINCE A LIST WAS TAKEN.
 *
 * KEY: THE HOOK AND THE CELL THAT CHECKS IT CALL THIS SAME FUNCTION. They
 * used to compute the comparison separately, and a review round measured
 * what that is worth: with the hook's own comparison replaced by an
 * empty list, the hook passed AND its control stayed green, because the
 * control was exercising a copy of the logic rather than the logic. A
 * second implementation of a check is not a check on the first.
 */
export function additionsTo(dir: string, before: string[]): string[] {
  return entriesIn(dir).filter((entry) => !before.includes(entry));
}
