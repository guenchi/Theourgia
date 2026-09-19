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
 * NOTE: WHY THIS IS A MODULE AND NOT A CLOSURE IN `extension.ts`.
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

import * as path from 'path';
import { Outbox } from './outbox';
import { Settlement } from './saver';

import { Saving, Unrecorded } from './saving';
import { Sessions } from './sessions';
import { Notice } from './status';

/*
 * NOTE: THE QUEUE AND THE STORE ARE PARAMETERS, and they are the whole
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
  sessions: Sessions;
  saving: Saving;
  /*
   * WHAT TO SAY WHEN THE RECORD COULD NOT BE WRITTEN. The sentence is
   * the caller's because showing it is: this module decides that it
   * happened, not how a window tells somebody.
   */
  report: (notice: Notice) => void;
  unrecorded: (file: string, because: Unrecorded) => Notice;
}

/*
 * NOTE: A SETTLER CARRIES THE QUEUE IT WAS BUILT FOR.
 *
 * The pairing checked below is settler-to-store. `Saver` takes its own
 * outbox and a settlement callback as two separate arguments, so a
 * correctly built settler for one store can still be attached to a Saver
 * over another -- and then the answers one queue receives are settled
 * against the other. Reproduced in review: with the same request id in
 * both, one store's answer removed the other's entry and wrote its
 * cursor beside the other's file.
 *
 * The queue rides along on the function so that `Saver` can compare it
 * with its own by identity, at construction. A bare callback carries
 * none and is accepted -- the cells in `saver.test.ts` pass one on
 * purpose, because what they are about is the sending -- and every
 * settler the extension builds comes from `settlerFor`, so the path
 * that ships is always checked.
 */
export type Settler = ((req: string, settlement: Settlement) => void) & {
  queue?: Outbox;
  storeHash?: string;
};

export function settlerFor(parts: SettlingParts): Settler {
  const { queue, storeHash, sessionId, sessions, saving } = parts;

  /*
   * NOTE: THE QUEUE AND THE STORE HAVE TO BE THE SAME WINDOW'S.
   *
   * Moving this out of `extension.ts` bought a place a cell can drive --
   * and created an interface that will accept any queue with any store
   * hash. Told store B while holding store A's queue, a settler
   * acknowledges B's matching file and removes A's request: the defect
   * this module exists to repair, now reachable through its own front
   * door. A review pointed that out about the move itself.
   *
   * The pairing is checkable from here, so it is checked here rather
   * than trusted: the queue's own path is what `outboxPathFor` builds
   * from the session and the store. The wiring in `rebuild` pairs them
   * correctly today; this is what makes that a property of the code
   * rather than of who happened to write the call.
   *
   * NOTE: WHAT THIS CANNOT CHECK, said plainly: that the queue is not a
   * STALE copy, and that the settler is only ever called inside
   * `Saver.serialise`, which reloads the file after taking the lock.
   * Both are properties of when it is called, not of what it was built
   * with, and a settler over a stale queue writes back a copy that
   * erases whatever another instance enqueued meanwhile. The caller
   * that guarantees it is `Saver`, which owns the lock and hands the
   * settler its answers; nothing else may call the returned function.
   * Reported for the next batch.
   */
  const belongs = sessions.outboxPathFor(sessionId, storeHash);
  if (path.resolve(queue.path) !== path.resolve(belongs)) {
    throw new Error(
      `a settler was built over ${queue.path}, which is not the queue this session keeps for ` +
        `that store (${belongs}); the answers it settles would be recorded against another ` +
        'store\'s files'
    );
  }

  /*
   * NOTE: THERE IS NO SECOND SUPPLIER ANY MORE. (section 13.1)
   *
   * What an answer is about used to be looked up twice: in a map kept
   * by block id while the window was up, and -- after a restart, when
   * that map is empty -- reconstructed from the block's newest version
   * by `recovered()`. Two derivations of one fact, with their
   * conditions spelled differently, and they disagreed: a block saved
   * in store A, the store changed, the same block saved in store B,
   * and A's answer wrote A's cursor beside store B's file using store
   * B's digests.
   *
   * The entry IS the record now. In flight and after a restart the
   * settler reads the same bytes, because there is only one place the
   * answer can be read from.
   */
  const settle: Settler = (req: string, settlement: Settlement): void => {
    /*
     * NOTE: A REFUSAL IS NOT AN ACKNOWLEDGEMENT, AND USED TO BE RECORDED AS
     * ONE.
     *
     * The store said no. Nothing about the file changed, so nothing is
     * written beside it and the entry stays where it is: the bytes the
     * user wrote are still only in their file, which is what `draft`
     * means, and the queue still holds the request that was turned
     * down. Writing an acknowledgement here -- which is what an empty
     * cursor and `mismatch: false` amounted to -- told the user their
     * rejected edit had been saved and took it out of every count that
     * would have shown otherwise. Reproduced end to end in review.
     *
     * `req-mismatch` is the refusal that leaves a mark: the store holds
     * a different request under this id and no retry can settle that, so
     * the record is marked unresolved for a person to look at -- the
     * path in `Saving` that this function used to make unreachable by
     * passing `mismatch: false` for every answer.
     */
    const entry = queue.find(req);
    const record = entry?.record;
    const cursor = settlement.verdict === 'confirmed' ? settlement.cursor : null;
    if (settlement.verdict === 'refused') {
      /*
       * NOTE: THE NUMBER COMES OUT AND NOTHING ELSE IS WRITTEN. The store
       * declined this write, so the bytes are still only in the user's
       * file and no baseline may be recorded -- and the send is over,
       * so leaving its number counted as out would keep the block a
       * draft it can never stop being.
       *
       * An entry with no record has no number to release: it was
       * written before section 13 and never took one.
       */
      if (record !== undefined) {
        saving.releaseSend(record.file, record.seq);
      }
      queue.resolve(req, null);
      return;
    }
    /*
     * NOTE: AN ENTRY WITH NO RECORD IS NOT AN ENTRY NOBODY KNOWS ABOUT.
     *
     * It is either a request this queue does not hold -- an answer that
     * belongs somewhere else, and `resolve` will not move the cursor
     * for one -- or an entry written before section 13, which carries a
     * request and bytes and nothing else. Neither may write a record
     * beside a file: the first because we do not know which file, the
     * second because the provenance a baseline needs was never
     * recorded. The answer moves the queue and the notice, and the
     * block stays a draft until the user saves it again.
     *
     * NOTE: A `req-mismatch` IS NOT RELEASED HERE EITHER. The store is
     * saying this id names another request; the entry is what a person
     * will look at, and there is no file to mark.
     */
    if (record === undefined) {
      if (settlement.verdict === 'req-mismatch') {
        return;
      }
      queue.resolve(req, cursor);
      return;
    }
    const recorded = saving.recordAnswer(
      record.file,
      {
        req,
        cursor: cursor ?? '',
        rawDigest: record.rawDigest,
        sentDigest: record.sentDigest,
        mismatch: settlement.verdict === 'req-mismatch',
        /*
         * WHICH SEND THIS ANSWER IS ABOUT, straight off the record. The
         * number decides whether this becomes the baseline, the split
         * says what the send was measured against, and `by` separates
         * "this client's write landed" from "somebody determined it had
         * already been carried out" -- which are different things to
         * read in a report and different things to chase.
         */
        send: {
          seq: record.seq,
          prefixDigest: record.prefixDigest,
          projectionId: record.projectionId,
          by: settlement.verdict === 'executed-by-operator' ? 'operator' : 'store'
        }
      },
      () => queue.resolve(req, cursor)
    );
    /*
     * NOTE: AND AN OPERATOR'S DETERMINATION LEAVES THE QUEUE WITHOUT A
     * POSITION. (section 13.3)
     *
     * Nothing was learned about where the store stands -- the
     * determination does not recover the execution's event -- so the
     * position this queue was holding is no longer known to be current.
     * Clearing it makes the next send ask, which is the only honest
     * thing to do with a number nobody can vouch for.
     */
    if (recorded.dequeued && settlement.verdict === 'executed-by-operator') {
      queue.clearCursor();
    }
    if (!recorded.dequeued) {
      /*
       * THE ENTRY STAYS. Whatever stopped the record from being written
       * -- the bytes moved, the file has no record, the answer is older
       * than one already there -- leaves the request retryable rather
       * than lost.
       */
      parts.report(parts.unrecorded(record.file, recorded.because));
    }
  };
  settle.queue = queue;
  /*
   * NOTE: AND WHICH STORE, for the same reason the queue is carried: the
   * Saver has to be able to refuse a record that belongs somewhere
   * else, and the only binding it holds is this one. A settler built by
   * `settlerFor` has already checked itself against the queue path for
   * this session and store.
   */
  settle.storeHash = storeHash;
  return settle;
}
