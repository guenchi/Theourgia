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

import * as path from 'path';
import { Outbox, OutboxEntry } from './outbox';
import { Publisher, digestOfBytes } from './publication';
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
  /*
   * ⚠️ WHICH STORE THIS SEND WAS FOR. It is known where the context is
   * written -- the window has a configured store at the moment it sends
   * -- and it is the one fact that separates two sends of the same block
   * with the SAME BYTES, which a digest cannot. See `settlerFor`.
   */
  storeHash: string;
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
   * ⚠️ THE QUEUE AND THE STORE HAVE TO BE THE SAME WINDOW'S.
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
   * ⚠️ WHAT THIS CANNOT CHECK, said plainly: that the queue is not a
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
    return digests === null ? undefined : { blockId: entry.id, file, storeHash, ...digests };
  };

  /*
   * ⚠️ AND THE CONTEXT IN MEMORY HAS TO BE ABOUT THIS SEND.
   *
   * `pendingSaves` lives as long as the window, survives every rebuild,
   * and is keyed by BLOCK ID -- while what an answer asks is "which
   * send was this". Those are the same question only while one store
   * has one save of that block in flight. A review reproduced the rest:
   * save a block in store A, change the store, save the SAME block in
   * store B before A answers -- B's context replaces A's under that key
   * -- and A's answer then wrote A's cursor and acknowledgement beside
   * STORE B's file, using store B's digests, and deleted the context
   * store B's own request still needed.
   *
   * The queue and the store were already bound. This is the third thing
   * an answer has to own, and the fact that decides it is on the entry:
   * `payload` is exactly what this request sent, and `sentDigest` is
   * what the remembered context was sent for. If they disagree, the
   * memory is somebody else's and the answer falls back to reading the
   * block's own versions, which are filed under this settler's store.
   *
   * The key cannot simply become the request id: `pendingSaves` is
   * written before `Saver.save` is called and the id is made inside it,
   * so the caller does not have one to key by.
   */
  /*
   * ⚠️ TWO DIFFERENT QUESTIONS, BECAUSE ONE OF THEM HAS A BLIND SPOT.
   *
   * The digest asks "were these the bytes this send carried". That is
   * the right question for two sends of one block with different text,
   * and it is BLIND to the case a review then named: open the same block
   * in another store and save the SAME bytes, and the digests agree
   * while the contexts belong to different stores. The answer would take
   * the other store's context again -- the same poisoned record, reached
   * through the repair.
   *
   * So the store is asked first and the digest second, and they are
   * different mechanisms rather than two spellings of one: the store
   * separates sends that differ in where they went, the digest separates
   * sends that differ in what they carried. A single input can defeat
   * either alone; nothing in this code defeats both.
   *
   * ⚠️ AND WHAT IS STILL INDISTINGUISHABLE, said rather than left to be
   * found: the same store, the same block, the same bytes, twice in
   * flight. Those two sends carry identical requests, so settling either
   * against that context records the same thing about the same file --
   * which is why this is a repetition rather than a confusion.
   */
  const remembered = (entry: OutboxEntry): SaveContext | undefined => {
    const held = pendingSaves.get(entry.id);
    if (held === undefined || held.storeHash !== storeHash) {
      return undefined;
    }
    return digestOfBytes(Buffer.from(entry.payload, 'utf8')) === held.sentDigest ? held : undefined;
  };

  return (req: string, cursor: string | null): void => {
    const entry = queue.find(req);
    const mine = entry === undefined ? undefined : remembered(entry);
    const context = entry === undefined ? undefined : mine ?? recovered(entry);
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
    /*
     * ⚠️ AND ONLY THE MEMORY THAT WAS OURS IS FORGOTTEN. The key is the
     * block id, so deleting after a context that came from `recovered`
     * would throw away whatever another save of the same block is still
     * waiting to settle -- which is the second half of the defect above,
     * reached from the other direction.
     */
    if (mine !== undefined) {
      pendingSaves.delete(context.blockId);
    }
  };
}
