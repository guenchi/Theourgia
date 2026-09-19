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
 * WHAT A SAVE IS ABOUT, CAPTURED ONCE. (section 13.1)
 *
 * Who (which store), what (which file, which bytes) and which send
 * (which request, which sequence number) are read at ONE instant --
 * when `decide` says send, inside the chain's critical section -- and
 * written down here. Everything downstream reads this record; nothing
 * downstream asks the settings, the document or the sidecar again.
 *
 * ⚠️ THE THREE USED TO BE READ AT THREE MOMENTS, and that is the defect
 * this type exists to make unsayable. The store came from the live
 * configuration after a wait, the bytes from a map kept by block id
 * that outlived the rebuild which replaced everything around it, and
 * the request id from inside the sender. Four rounds of review found
 * four faces of it; each repair pinned one pair and left the third.
 *
 * ⚠️ AND THE STORE COMES FROM THE FILE, NEVER FROM THE SETTINGS. A file
 * under a session directory belongs to the store it was published from,
 * and `sidecar.storeId` is the file saying so. The settings say what
 * this window is looking at now, which is a different question and can
 * be answered differently a moment later.
 */

export interface SendIntent {
  readonly verb: string;
  readonly field: string;
  readonly expectation: string | null;
  readonly body: string;
}

export interface SendRecord {
  readonly projectionId?: string;
  readonly req: string;
  readonly store: string;
  readonly storeHash: string;
  readonly blockId: string;
  readonly file: string;
  readonly rawDigest: string;
  readonly sentDigest: string;
  readonly prefixDigest: string;
  /*
   * THE FILE'S OWN SEND NUMBER, taken from `sidecar.nextSeq` at the
   * same instant and written to the sidecar before anything is sent
   * (I7). It is the axis the sidecar's ordering guard turns on: a
   * cursor belongs to a store and two stores' cursors cannot be
   * compared, while two sends of one file always can.
   */
  readonly seq: number;
  readonly intent: SendIntent;
}

/*
 * ⚠️ A SHALLOW FREEZE WOULD BE A PROMISE THIS SHAPE CANNOT KEEP.
 *
 * `Object.freeze` stops the top-level fields and says nothing about
 * `intent`, whose `body` is the bytes that go to the store. I1 is that
 * no field of the record is written after the moment of capture, and a
 * freeze that leaves the payload writable enforces the half of it that
 * was never in question.
 */
export function freezeRecord(record: SendRecord): SendRecord {
  Object.freeze(record.intent);
  return Object.freeze(record);
}

export interface RecordParts {
  projectionId?: string;
  req: string;
  store: string;
  storeHash: string;
  blockId: string;
  file: string;
  rawDigest: string;
  sentDigest: string;
  prefixDigest: string;
  seq: number;
  intent: SendIntent;
}

/*
 * THE ONE PLACE A RECORD IS MADE.
 *
 * It is a constructor rather than an object literal at the call site so
 * that "made" and "frozen" are one act. A record built somewhere else
 * and frozen afterwards has a window in which it is writable, and the
 * whole of I1 is that there is no such window.
 */
export function recordFor(parts: RecordParts): SendRecord {
  return freezeRecord({
    projectionId: parts.projectionId,
    req: parts.req,
    store: parts.store,
    storeHash: parts.storeHash,
    blockId: parts.blockId,
    file: parts.file,
    rawDigest: parts.rawDigest,
    sentDigest: parts.sentDigest,
    prefixDigest: parts.prefixDigest,
    seq: parts.seq,
    intent: {
      verb: parts.intent.verb,
      field: parts.intent.field,
      expectation: parts.intent.expectation,
      body: parts.intent.body
    }
  });
}
