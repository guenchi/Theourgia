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
 * WHAT AN ANSWER FROM THE STORE MEANS FOR THE FILE IT WAS ABOUT.
 *
 * ⚠️ WHY THIS IS A MODULE AND NOT A CLOSURE IN `extension.ts`.
 *
 * It used to be one, and everything it decides was therefore decided
 * where no cell could drive it: which queue the answer settles, which
 * store's directory the block's versions are looked for under, whether
 * the record beside the file was written and so whether the entry may
 * be dequeued. Two of those went wrong at once and it took an outside
 * review plus an editor-hosted cell to see it -- the closure read the
 * module's LIVE `outbox` and `config.store`, so an answer arriving after
 * the settings changed settled against a queue belonging to another
 * store: that store's `outbox.json` took this store's cursor, and the
 * request that was actually answered stayed queued where nobody was
 * looking.
 *
 * The repair was to bind the queue and the store. Binding them inside a
 * closure in `activate` would have left the repair exactly as hard to
 * check as the defect was to find, so the decision moved here, beside
 * `chooseAndRecover` and `destinationFor`, which are in src for the same
 * reason. `rebuild` now does the wiring and nothing else.
 */

import { Outbox, OutboxEntry } from './outbox';
import { Publisher } from './publication';
import { Saving, Unrecorded } from './saving';
import { Sessions } from './sessions';
import { Notice } from './status';

/*
 * WHAT THIS CLIENT REMEMBERS ABOUT A SAVE IT SENT: which block, which
 * file, and the two digests the record beside that file is written
 * against.
 */
export interface SaveContext {
  blockId: string;
  file: string;
  rawDigest: string;
  sentDigest: string;
}

/*
 * ⚠️ THE QUEUE AND THE STORE ARE PARAMETERS, and they are the whole
 * point of the interface. A settler serves one queue for its life --
 * that is what the Saver's lock is keyed on -- and the blocks whose
 * versions it reads are filed under one store. Passing them in makes
 * "which queue" and "which store" visible at the wiring, where a reader
 * can see whether they were captured or read live.
 */
export interface SettlingParts {
  queue: Outbox;
  storeHash: string;
  sessionId: string;
  pendingSaves: Map<string, SaveContext>;
  sessions: Sessions;
  publisher: Publisher;
  saving: Saving;
  /*
   * WHAT TO SAY WHEN THE RECORD COULD NOT BE WRITTEN. The sentence is
   * the caller's because showing it is: this module decides that it
   * happened, not how a window tells somebody.
   */
  report: (notice: Notice) => void;
  unrecorded: (file: string, because: Unrecorded) => Notice;
}

export type Settler = (req: string, cursor: string | null) => void;

export function settlerFor(parts: SettlingParts): Settler {
  const { queue, storeHash, sessionId, pendingSaves, sessions, publisher, saving } = parts;

  /*
   * THE FILE AN ANSWER IS ABOUT, WHEN THIS WINDOW NEVER SENT IT.
   *
   * A retry after a restart names a request the queue remembers and this
   * process does not, so `pendingSaves` is empty for it. The answer used
   * to be released with nothing written, and that block then reported
   * unsent work for ever while the store held the bytes.
   *
   * The queue kept what was sent, and the block's own directory says
   * which versions exist. If the newest one still splits into exactly
   * the text that went out, these are the bytes the store acknowledged
   * and the digests come from the file rather than from memory. If it
   * does not, this answers `undefined` and the old behaviour stands --
   * which is right, because then the file really has moved on.
   *
   * ⚠️ AND IT LOOKS UNDER THE STORE THIS SETTLER BELONGS TO. Read live,
   * it would look for one store's block under another after a settings
   * change, find nothing, and treat an answer this client can account
   * for as one it cannot.
   */
  const recovered = (entry: OutboxEntry): SaveContext | undefined => {
    const directory = sessions.directoryFor(sessionId, storeHash, entry.id);
    const file = publisher.latestIn(directory);
    if (file === null) {
      return undefined;
    }
    const digests = saving.recognise(file, entry.payload);
    return digests === null ? undefined : { blockId: entry.id, file, ...digests };
  };

  return (req: string, cursor: string | null): void => {
    const entry = queue.find(req);
    const context = entry === undefined ? undefined : pendingSaves.get(entry.id) ?? recovered(entry);
    if (context === undefined) {
      /*
       * NOTHING IN MEMORY AND NOTHING ON DISK SAYS WHICH VERSION THIS
       * ANSWER IS ABOUT. `recovered` has already looked: the block's
       * newest version does not hold the body this request sent, so the
       * user has edited since and the file is a draft, correctly. The
       * entry is released because the store HAS answered it. (§12.17.4)
       */
      queue.resolve(req, cursor);
      return;
    }
    const recorded = saving.recordAnswer(
      context.file,
      {
        req,
        cursor: cursor ?? '',
        rawDigest: context.rawDigest,
        sentDigest: context.sentDigest,
        mismatch: false
      },
      () => queue.resolve(req, cursor)
    );
    if (!recorded.dequeued) {
      /*
       * THE ENTRY STAYS. Whatever stopped the record from being written
       * -- the bytes moved, the file has no record, the answer is older
       * than one already there -- leaves the request retryable rather
       * than lost.
       */
      parts.report(parts.unrecorded(context.file, recorded.because));
    }
    pendingSaves.delete(context.blockId);
  };
}
