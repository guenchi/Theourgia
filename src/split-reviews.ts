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
 * WHERE A SPLIT'S REVIEW FILE LIVES, AND WHEN IT GOES.
 *
 * A split asks the core to write its review to a path this extension
 * names (`--output`, src/split-symbols.ts), so that nothing is written
 * beside the user's source. The path is
 *
 *   <root>/<session id>/<fresh uuid>/<the source's own name>
 *
 * one directory per split, so that two splits of one file never ask for
 * the same path (the core refuses an output that exists), and the source's
 * name kept, so that the review opens in the source's language.
 *
 * A review goes when the editor showing it closes: `closed` is told which
 * files are still shown in any tab, and removes the directory of every
 * review it opened that is no longer among them. A review the window never
 * got to close -- the window was closed or reloaded, or crashed -- is left
 * behind, and `sweep` removes it at a later activation.
 *
 * NEVER: THE SWEEP DOES NOT REMOVE ANOTHER WINDOW'S OPEN REVIEW. The root
 * is under the global storage, which every window of this editor shares,
 * so "every directory under the root is a leftover" is false while a
 * second window is open. Each window's reviews are under its own session
 * id, and a session's directory is removed only when the session records
 * (src/sessions.ts) say that window is gone: its process is not running,
 * or its record has been removed. A window whose liveness cannot be told
 * keeps its reviews.
 */

import { randomUUID } from 'crypto';
import * as path from 'path';
import { insideDirectory, nodeFileOps, removeTree } from './fsops';
import { Liveness } from './sessions';

export interface ReviewDisk {
  makeDirectory(directory: string): void;
  /*
   * The names in a directory; none when it does not exist, and a failure
   * for any other reason it cannot be read.
   */
  list(directory: string): string[];
  removeTree(directory: string): void;
}

export const nodeReviewDisk: ReviewDisk = {
  makeDirectory: (directory) => nodeFileOps.makeDirectory(directory),
  list: (directory) => nodeFileOps.list(directory),
  removeTree: (directory) => removeTree(directory)
};

/*
 * A REMOVAL THAT FAILED, with the directory it was of. Removal is
 * housekeeping: a failure is reported to the user and stops nothing.
 */
export interface ReviewRemovalFailure {
  directory: string;
  error: unknown;
}

export interface SweepOutcome {
  removed: string[];
  kept: string[];
  failed: ReviewRemovalFailure[];
}

/*
 * WHETHER A SESSION'S REVIEWS MAY BE REMOVED. Its process not running is
 * the plain case. A MISSING RECORD counts as gone too: a session writes
 * its record at activation, before it can split anything, so a session
 * that has a reviews directory had a record, and a record that is missing
 * since was removed by a discard or a takeover of that session. Every
 * other undecidable -- an unreadable record, a start time that cannot be
 * read -- keeps the reviews.
 */
export function sessionIsGone(liveness: Liveness): boolean {
  if ('alive' in liveness) {
    return !liveness.alive;
  }
  return liveness.because === 'record-missing';
}

export class SplitReviews {
  private readonly shown = new Set<string>();

  constructor(
    private readonly root: string,
    private readonly sessionId: string,
    private readonly liveness: (sessionId: string) => Liveness,
    private readonly disk: ReviewDisk = nodeReviewDisk
  ) {}

  private mine(): string {
    return path.join(this.root, this.sessionId);
  }

  /*
   * A FRESH PATH FOR ONE SPLIT'S REVIEW of `source`; its directory exists,
   * the file does not.
   */
  public place(source: string): string {
    const directory = path.join(this.mine(), randomUUID());
    this.disk.makeDirectory(directory);
    return path.join(directory, path.basename(source));
  }

  /*
   * THE DIRECTORY OF A REVIEW THIS WINDOW PLACED, OR NULL. A path from
   * anywhere else -- another window's, or one this extension did not
   * make -- is never removed by this window.
   */
  private directoryOf(review: string): string | null {
    const directory = path.dirname(review);
    return path.dirname(directory) === this.mine() && insideDirectory(this.mine(), directory) ? directory : null;
  }

  /*
   * A failure is added to `failed`, which the caller hands to the user;
   * whether this removal worked is whether `failed` grew.
   */
  private remove(directory: string, failed: ReviewRemovalFailure[]): void {
    try {
      this.disk.removeTree(directory);
    } catch (error) {
      failed.push({ directory, error });
    }
  }

  /*
   * A review the split did not end with: its directory goes at once.
   */
  public discard(review: string): ReviewRemovalFailure[] {
    const failed: ReviewRemovalFailure[] = [];
    const directory = this.directoryOf(review);
    if (directory !== null) {
      this.remove(directory, failed);
    }
    return failed;
  }

  /*
   * A review now open in an editor, to be removed when no tab shows it.
   */
  public opened(review: string): void {
    if (this.directoryOf(review) !== null) {
      this.shown.add(review);
    }
  }

  /*
   * THE TABS CHANGED. Every review opened here that `stillShown` no longer
   * answers for has its directory removed, and is forgotten whether or not
   * the removal worked: a failure is reported once, and the sweep of a
   * later activation tries again.
   */
  public closed(stillShown: (file: string) => boolean): { removed: string[]; failed: ReviewRemovalFailure[] } {
    const removed: string[] = [];
    const failed: ReviewRemovalFailure[] = [];
    for (const review of [...this.shown]) {
      if (stillShown(review)) {
        continue;
      }
      this.shown.delete(review);
      const directory = this.directoryOf(review);
      if (directory !== null) {
        const before = failed.length;
        this.remove(directory, failed);
        if (failed.length === before) {
          removed.push(review);
        }
      }
    }
    return { removed, failed };
  }

  /*
   * AT ACTIVATION: the reviews of every other session that is gone. This
   * window's own directory is new and is left alone.
   */
  public sweep(): SweepOutcome {
    const out: SweepOutcome = { removed: [], kept: [], failed: [] };
    for (const name of this.disk.list(this.root)) {
      if (name === this.sessionId) {
        continue;
      }
      const directory = path.join(this.root, name);
      let gone = false;
      try {
        gone = sessionIsGone(this.liveness(name));
      } catch {
        gone = false;
      }
      if (!gone) {
        out.kept.push(directory);
        continue;
      }
      const before = out.failed.length;
      this.remove(directory, out.failed);
      if (out.failed.length === before) {
        out.removed.push(directory);
      }
    }
    return out;
  }
}
