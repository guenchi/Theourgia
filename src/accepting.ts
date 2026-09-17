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
 * THE MOMENT A SAVE IS ACCEPTED. (§13.1)
 *
 * `decide` says send; this is the instant that follows, and everything
 * the send is about is read HERE and never again: which store (from the
 * file's own record, never from the settings), which file, which bytes,
 * which request, which sequence number.
 *
 * ⚠️ IT RUNS INSIDE THE CHAIN'S CRITICAL SECTION, in the same callback
 * as `decide`. Outside it, the answers can change between the reading
 * and the record -- which is the defect four rounds of review kept
 * finding a new face of.
 *
 * ⚠️ AND THE NUMBER IS ON DISK BEFORE ANYTHING IS QUEUED. `takeSequence`
 * advances `nextSeq` and adds the number to `outstanding` in one durable
 * write. If that write fails the save is NOT accepted: nothing is
 * queued, nothing is sent, and the user is told. A request that went out
 * carrying a number nobody wrote down would be handed that number again
 * after a restart.
 */

import * as path from 'path';
import {withExclusive} from './fsops';
import { SaveDecision } from './saving';
import { SendRecord, recordFor } from './record';
import { Sidecar, digestOfBytes } from './publication';

export interface Numbering {
  acceptsSnapshot?(file:string,rawDigest:string,sidecar:Sidecar):boolean;
  takeSequence(
    file: string,
    req: string
  ):
    | { taken: true; seq: number }
    | { taken: false; because: 'no-record' | 'could-not-write' | 'not-ours'; detail?: string };
}

export interface AcceptParts {
  file: string;
  sidecar: Sidecar;
  decision: Extract<SaveDecision, { send: true }>;
  /*
   * The store the queue this window would send through belongs to. It
   * is compared with the store the FILE says it belongs to, and they
   * are different questions: the settings say what this window is
   * looking at now, the sidecar says where these bytes came from.
   */
  queueStore: string;
  storeHash: (store: string) => string;
  newRequestId: () => string;
  numbering: Numbering;
}

export type Acceptance =
  | { accepted: true; record: SendRecord }
  /*
   * ⚠️ EACH REFUSAL IS ITS OWN WORD BECAUSE EACH IS A DIFFERENT THING
   * FOR THE USER TO DO. "Another window is configured for this store",
   * "this file has no record beside it any more" and "the record could
   * not be written" are three sentences, and a boolean would make them
   * one.
   */
  | { accepted: false; because: 'another-store'; store: string; configured: string }
  | { accepted: false; because: 'no-record' }
  | { accepted: false; because: 'projection-changed' }
  /*
   * ANOTHER WINDOW HOLDS THIS BLOCK. Not a refusal about the bytes:
   * nothing is wrong with them, and the thing to do is look at the
   * window that holds the block rather than save again.
   */
  | { accepted: false; because: 'not-ours'; detail: string }
  | { accepted: false; because: 'could-not-number'; detail: string };

export function acceptSave(parts: AcceptParts): Acceptance {
  return withExclusive(path.dirname(parts.file), (): Acceptance => {
  const { sidecar, decision } = parts;
  /*
   * ⚠️ THE STORE COMES FROM THE FILE. A window configured for another
   * store must not write this file's send into its own queue: that
   * queue carries another store's cursor, and a window that IS
   * configured for this file's store may be writing the queue it
   * belongs in. Two writers over one file is one of them erased.
   */
  if (sidecar.storeId !== parts.queueStore) {
    return {
      accepted: false,
      because: 'another-store',
      store: sidecar.storeId,
      configured: parts.queueStore
    };
  }
  if(parts.numbering.acceptsSnapshot && !parts.numbering.acceptsSnapshot(parts.file,decision.rawDigest,sidecar)) {
    return {accepted:false,because:'projection-changed'};
  }
  const req = parts.newRequestId();
  const numbered = parts.numbering.takeSequence(parts.file, req);
  if (!numbered.taken) {
    if (numbered.because === 'no-record') {
      return { accepted: false, because: 'no-record' };
    }
    if (numbered.because === 'not-ours') {
      return { accepted: false, because: 'not-ours', detail: numbered.detail ?? '' };
    }
    return { accepted: false, because: 'could-not-number', detail: numbered.detail ?? '' };
  }
  return {
    accepted: true,
    record: recordFor({
      projectionId: sidecar.projection?.id,
      req,
      store: sidecar.storeId,
      storeHash: parts.storeHash(sidecar.storeId),
      blockId: sidecar.blockId,
      file: parts.file,
      rawDigest: decision.rawDigest,
      sentDigest: decision.sentDigest,
      /*
       * THE SPLIT THIS SEND WAS MADE AGAINST. `reconcile` can adopt a
       * different heading without touching a byte of the file, and a
       * baseline that recorded only the file's digest would then call
       * the block settled while the text that would be sent from it has
       * changed.
       */
      prefixDigest: digestOfBytes(sidecar.prefix),
      seq: numbered.seq,
      intent: {
        verb: decision.intent.verb,
        field: decision.intent.field,
        expectation: decision.intent.expectation,
        body: decision.src
      }
    })
  };
  });
}
