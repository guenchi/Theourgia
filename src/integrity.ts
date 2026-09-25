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
 * WHEN THE USER IS TOLD THAT A STORE IS NOT SOUND.
 *
 * What the sentence says is `integrityNotice`'s business; this decides
 * whether to ask and whether to say it. It lives outside the extension
 * host so that a cell can drive it: when it sat inside `activate`, a
 * review showed that the whole of it could be switched off -- never
 * asking `check` at all -- with every cell in the tree still green.
 *
 * ONCE PER STORE PER SESSION, and only when there is something to say.
 *
 * KEY: A STORE IS MARKED ONLY WHEN THE USER WAS TOLD. A sound store is
 * not marked, and neither is one whose condition could not be read --
 * so switching back to it asks again, which costs one `check` and is
 * the only way a store that went bad during the session is ever seen.
 * Repeating a warning on every save is what teaches a person to stop
 * reading warnings, so a store that has been reported is not reported
 * again in this session.
 */

import { StoreVerdict } from './model';
import { Notice, integrityNotice } from './status';

/*
 * What one check needs from the window that asks it. `store` is the one
 * configured when the check starts; `generation` is read live, so that an
 * answer arriving after the window moved to another store is recognised.
 */
export interface IntegrityQuestion {
  store: string;
  ask: () => Promise<StoreVerdict>;
  show: (notice: Notice) => void;
  generation: () => number;
}

export class IntegrityWatch {
  private readonly told = new Set<string>();

  public async check(on: IntegrityQuestion): Promise<void> {
    if (this.told.has(on.store)) {
      return;
    }
    const asked = on.generation();
    /*
     * A QUESTION THAT FAILED IS AN ANSWER NOBODY COULD READ, and it is
     * treated as one: nothing is said and the store is not marked, so it is
     * asked again. `storeVerdict` already turns a transport failure into
     * that; this is for any `ask` that rejects instead, whose rejection
     * would otherwise escape a call the extension does not wait for.
     *
     * NOTE: THE REJECTED VALUE IS NOT LOOKED AT. A rejection can carry any
     * value, and looking at it can throw: `String` on an object with no
     * prototype, `instanceof` on a proxy whose prototype trap throws, an
     * Error whose `message` getter throws. Three reviews in a row found one
     * more such value, each letting the rejection out of this catch after
     * all; a catch that reads nothing has no such value. The reason of an
     * unknown verdict is not shown to anybody, so a fixed sentence loses
     * nothing a person would read.
     */
    let verdict: StoreVerdict;
    try {
      verdict = await on.ask();
    } catch {
      verdict = { known: false, because: 'the question about this store failed' };
    }
    /*
     * AN ANSWER FOR A WINDOW THAT HAS MOVED ON is dropped, and the store
     * is not marked: nobody was told, so the next time it is opened it is
     * asked again.
     */
    if (asked !== on.generation()) {
      return;
    }
    /*
     * AND THE MARK IS READ AGAIN AFTER THE WAIT. Two checks of one store
     * can be waiting at once; the first to come back tells the user, and
     * the second would tell them again if the mark were only read before
     * it asked.
     */
    if (this.told.has(on.store)) {
      return;
    }
    const notice = integrityNotice(on.store, verdict);
    if (notice === null) {
      return;
    }
    /*
     * SHOWN, THEN MARKED. A `show` that throws has told nobody, and a mark
     * written before it would keep the store from being asked again for
     * the rest of the session.
     */
    on.show(notice);
    this.told.add(on.store);
  }
}
