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
 * What happens when the user saves: one snapshot, verified, sent once.
 * (§12.19.4, §12.17.3, §12.15 结构二)
 *
 * ONLY A CLEAN DOCUMENT IS SENT, AND ONLY ITS SNAPSHOT. When the
 * handler runs with `isDirty` false, `getText()` is the text of the
 * last COMPLETED save, so there is no torn read and no second reading:
 * the snapshot is taken once and that same value is what goes out.
 * Dirty means the user typed again after saving -- the next save will
 * carry the newer text, so this one sends nothing. (§12.19.4)
 *
 * THE BYTES ON DISK ARE EVIDENCE, NOT CONTENT. They are read only to
 * check that they decode strictly as UTF-8, carry no BOM, and equal the
 * snapshot; `acknowledged-raw` is their digest. If any of that fails,
 * nothing is sent and the next save tries again -- a misjudged torn
 * read costs one save, never the wrong content. (§12.17.3)
 */

import { FileOps } from './fsops';
import { digestOfBytes, newerEvent, Sidecar, sidecarFromDisk, sidecarToDisk } from './publication';

export interface SaveDocument {
  file: string;
  isDirty: boolean;
  getText(): string;
}

/*
 * Why nothing was sent, named. Each of these has a different thing for
 * the user to do, and C5/C17 exist because a single "not sent" would
 * let an implementation confuse them. (§12.17.3, §12.13.4)
 */
export type Refusal =
  | { because: 'document-dirty' }
  | { because: 'byte-order-mark' }
  | { because: 'not-utf8' }
  | { because: 'disk-differs-from-snapshot' }
  | { because: 'prefix-changed'; prefix: string }
  | { because: 'no-sidecar' }
  /*
   * The sidecar says `publishing`: the file and the record do not yet
   * describe one another, so the prefix cannot be trusted to split
   * against. (§12.7.3, C3)
   */
  | { because: 'publication-incomplete' }
  | { because: 'unresolved' }
  | { because: 'outside-session'; file: string };

/*
 * WHAT IS SENT, NAMED SEPARATELY FROM WHAT IS DECIDED.
 *
 * `decide` answers which bytes are the body; turning that into a request
 * is somebody else's job, and the request's shape is about to change: a
 * later batch replaces `set <id> src <bytes>` with a `write` carrying an
 * expectation token, because the store is growing a working version
 * distinct from the committed one. The baseline, the draft scan and the
 * session directory all survive that change -- what moves is the verb
 * and the token.
 *
 * So the seam is here and it is empty on purpose: X1c fills in `set`
 * and no expectation, and the next batch fills in the other one without
 * touching anything that decided the body.
 */
export interface WriteIntent {
  verb: string;
  field: string;
  expectation: string | null;
}

export type SaveDecision =
  | {
      send: true;
      src: string;
      rawDigest: string;
      sentDigest: string;
      normalised: boolean;
      intent: WriteIntent;
    }
  | { send: false; refusal: Refusal };

/*
 * Whether the outbox entry may now be removed, and if not, why. The
 * entry outlives anything this cannot record. (§12.7.4, C7, C20)
 */
export type AnswerRecording =
  | { dequeued: true }
  | { dequeued: false; because: 'not-acknowledged' | 'req-mismatch' | 'file-moved' };

export class Saving {
  private readonly files: FileOps;

  constructor(files: FileOps) {
    this.files = files;
  }

  /*
   * Decides, without sending anything: take the snapshot once, verify
   * the disk bytes against it, split against the sidecar's prefix.
   * Returning a decision rather than performing it is what lets a cell
   * assert "zero sends" without watching a transport. (§12.19.4, C5,
   * C17)
   */
  public decide(document: SaveDocument, sidecar: Sidecar | null): SaveDecision {
    /*
     * DIRTY FIRST. Everything below reasons about "the text of the last
     * completed save", and that is only what `getText` means while the
     * document is clean.
     */
    if (document.isDirty) {
      return { send: false, refusal: { because: 'document-dirty' } };
    }
    if (sidecar === null) {
      return { send: false, refusal: { because: 'no-sidecar' } };
    }
    if (sidecar.phase !== 'published') {
      return { send: false, refusal: { because: 'publication-incomplete' } };
    }
    if (sidecar.unresolved) {
      return { send: false, refusal: { because: 'unresolved' } };
    }

    /*
     * THE SNAPSHOT IS TAKEN ONCE, HERE, and this value is what goes out.
     * Reading the document again after the checks below would mean the
     * bytes verified are not the bytes sent. (§12.19.4)
     */
    const snapshot = document.getText();

    let bytes: Buffer;
    try {
      bytes = this.files.readBytes(document.file);
    } catch (e) {
      return { send: false, refusal: { because: 'disk-differs-from-snapshot' } };
    }
    if (bytes.length >= 3 && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf) {
      return { send: false, refusal: { because: 'byte-order-mark' } };
    }
    /*
     * A UTF-16 MARK IS NOT A UTF-8 ONE. `FF FE` and `FE FF` say the file
     * is not this encoding at all, which is a different thing to say
     * than "it begins with a mark this build strips".
     *
     * ⚠️ AND WITHOUT A MARK, UTF-16 CANNOT BE RECOGNISED HERE. All-ASCII
     * text in UTF-16LE is a run of ASCII bytes separated by NULs, which
     * is perfectly valid UTF-8 -- measured, not supposed. Such a file is
     * still not sent, but the refusal that catches it is the comparison
     * with the snapshot, not this one.
     */
    if (bytes.length >= 2 && ((bytes[0] === 0xff && bytes[1] === 0xfe) || (bytes[0] === 0xfe && bytes[1] === 0xff))) {
      return { send: false, refusal: { because: 'not-utf8' } };
    }
    let decoded: string;
    try {
      /*
       * STRICTLY. `Buffer.toString('utf8')` substitutes U+FFFD for an
       * invalid sequence and returns a perfectly good string, so a
       * check written as "decode and compare" passes whenever the
       * replacement lands outside the compared region. (§12.17.3)
       */
      decoded = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
    } catch (e) {
      return { send: false, refusal: { because: 'not-utf8' } };
    }
    if (decoded !== snapshot) {
      return { send: false, refusal: { because: 'disk-differs-from-snapshot' } };
    }
    if (!snapshot.startsWith(sidecar.prefix)) {
      return { send: false, refusal: { because: 'prefix-changed', prefix: sidecar.prefix } };
    }

    const body = snapshot.slice(sidecar.prefix.length);
    /*
     * A BLOCK THAT HOLDS NO CARRIAGE RETURN, IN A FILE THAT USES THEM,
     * is sent as LF and the user is told. The block's own bytes decide:
     * one that really contains CRLF is sent verbatim. (§12.17.3)
     */
    const normalised = body.includes('\r\n') && !sidecar.prefix.includes('\r');
    const src = normalised ? body.replace(/\r\n/g, '\n') : body;
    return {
      send: true,
      src,
      rawDigest: digestOfBytes(bytes),
      sentDigest: digestOfBytes(Buffer.from(src, 'utf8')),
      normalised,
      intent: { verb: 'set', field: 'src', expectation: null }
    };
  }

  /*
   * THE ANSWER CRITICAL SECTION, AND THE ONLY PLACE THE ORDER LIVES.
   *
   * The record beside the file is written FIRST; the outbox entry is
   * removed only if that succeeded. A build that dequeued first and
   * then crashed would have destroyed its own means of retrying -- the
   * request is gone and the store may or may not hold it. (§12.7.4, C7)
   *
   * IT RE-READS THE FILE INSIDE THE SECTION. The editor is not on the
   * chain; if the bytes moved while the answer was in flight, the
   * acknowledgement would describe a version that is no longer there,
   * so the write is abandoned and the ENTRY IS KEPT. (§12.11.1, C20)
   *
   * A `req-mismatch` KEEPS THE ENTRY TOO, marked unresolved: the store
   * is saying it has a different request under that id, which nobody
   * here can resolve by retrying. (§12.9, C7)
   */
  public recordAnswer(
    file: string,
    answer: { req: string; cursor: string; rawDigest: string; sentDigest: string; mismatch: boolean },
    dequeue: () => void = () => undefined
  ): AnswerRecording {
    const meta = `${file}.meta`;
    if (!this.files.exists(meta)) {
      return { dequeued: false, because: 'not-acknowledged' };
    }
    const read = sidecarFromDisk(this.files.readText(meta));
    if (!read.read) {
      return { dequeued: false, because: 'not-acknowledged' };
    }
    if (answer.mismatch) {
      /*
       * THE STORE HAS A DIFFERENT REQUEST UNDER THAT NAME. Nobody here
       * can settle that by retrying, so the entry stays and the file is
       * marked for a person to look at. (§12.9)
       */
      this.files.writeText(
        meta,
        `${JSON.stringify(sidecarToDisk({ ...read.sidecar, unresolved: true }), null, 2)}\n`
      );
      return { dequeued: false, because: 'req-mismatch' };
    }
    const here = this.files.exists(file) ? digestOfBytes(this.files.readBytes(file)) : null;
    if (here !== answer.rawDigest) {
      /*
       * THE BYTES MOVED WHILE THE ANSWER WAS IN FLIGHT. Recording an
       * acknowledgement against them would mark a draft as sent, so the
       * write is abandoned and the ENTRY IS KEPT. (§12.11.1, C20)
       */
      return { dequeued: false, because: 'file-moved' };
    }
    if (!newerEvent(read.sidecar.cursor, answer.cursor)) {
      /*
       * A LATE REPLAY IS A DUPLICATE, NOT AN ERROR. The request has been
       * settled; the cursor stays where the newer answer put it, and the
       * entry goes. (§12.7.4)
       */
      dequeue();
      return { dequeued: true };
    }
    /*
     * THE RECORD FIRST, THE QUEUE SECOND, AND BOTH FROM HERE. A caller
     * that dequeued afterwards would put the order in a second place,
     * and nothing could observe which happened first.
     */
    this.files.writeText(
      meta,
      `${JSON.stringify(
        sidecarToDisk({
          ...read.sidecar,
          acknowledgedRaw: answer.rawDigest,
          sent: answer.sentDigest,
          cursor: answer.cursor
        }),
        null,
        2
      )}\n`
    );
    dequeue();
    return { dequeued: true };
  }
}
