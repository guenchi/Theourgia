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
import {
  digestOfBytes,
  newerEvent,
  Sidecar,
  sidecarFromDisk,
  sidecarPathOf,
  writeSidecar
} from './publication';

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

/*
 * CRLF FOLDED TO LF. Comparing two texts for equality of CONTENT rather
 * than of line endings is what this is for; it is never used on bytes
 * that are about to be sent, because what goes out keeps the block's own
 * line endings.
 */
function foldEol(text: string): string {
  return text.replace(/\r\n/g, '\n');
}

/*
 * WHERE A POSITION IN THE FOLDED TEXT FALLS IN THE ORIGINAL.
 *
 * Folding is not length-preserving, so an index into the folded text
 * cannot be used on the original: slicing at it cuts a CRLF in half for
 * every line ending before it. This walks the original one character --
 * or one CRLF pair -- at a time, counting folded characters, and returns
 * the index in the ORIGINAL at which that many have gone by. `null`
 * means the text ran out first, which only happens when the caller did
 * not check that the folded text begins with the folded prefix.
 */
function prefixEndOf(text: string, folded: number): number | null {
  let at = 0;
  let counted = 0;
  while (counted < folded) {
    if (at >= text.length) {
      return null;
    }
    at += text[at] === '\r' && text[at + 1] === '\n' ? 2 : 1;
    counted += 1;
  }
  return at;
}

/*
 * THE BODY A TEXT WOULD SEND, GIVEN THE RECORD BESIDE IT, or `null` when
 * it does not begin with the heading that record was split from.
 *
 * IT IS ONE FUNCTION BECAUSE THERE ARE TWO CALLERS. `decide` asks it
 * about a buffer being saved; `recognise` asks it about a file on disk,
 * to find out whether that file holds the body a request already sent.
 * A second copy of this split would be a second place for the EOL
 * handling to be subtly different, and the two answers would then
 * disagree about what was sent -- which is unobservable from either
 * side.
 *
 * AND WHETHER THE BODY WAS NORMALISED IS ABOUT THE BODY. It was computed
 * from the whole snapshot, so a document whose heading used CRLF and
 * whose body did not announced a normalisation that never touched a byte
 * of what was sent.
 */
function bodyOf(text: string, sidecar: Sidecar): { body: string; normalised: boolean } | null {
  const wanted = foldEol(sidecar.prefix);
  const end = foldEol(text).startsWith(wanted) ? prefixEndOf(text, wanted.length) : null;
  if (end === null) {
    return null;
  }
  const body = text.slice(end);
  const normalised = !sidecar.bodyHasCrlf && body.includes('\r\n');
  return { body: normalised ? foldEol(body) : body, normalised };
}

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
    /*
     * THE LINE ENDINGS ARE SETTLED BEFORE THE PREFIX IS COMPARED.
     *
     * ⚠️ THIS ORDER WAS WRONG AND THE EDITOR CELLS FOUND IT. Comparing
     * first meant a buffer the editor writes as CRLF never matched a
     * prefix the store gave as LF, so every save of such a file was
     * refused as "the heading changed" -- by the extension, about a
     * change the user had not made. §12.17.3 puts the EOL handling
     * first for exactly this reason: the comparison and the split both
     * happen on the normalised text.
     *
     * WHETHER TO NORMALISE IS THE BLOCK'S OWN FACT, recorded when the
     * version was published. A block whose stored body really did use
     * CRLF is sent verbatim; the prefix's line endings say nothing
     * about the body's.
     */
    /*
     * ⚠️ AND THE COMPARISON IS ALWAYS EOL-AGNOSTIC, WHICH IT WAS NOT.
     * Normalising only when the block's own body had no CRLF left two
     * shapes refusing a save as "the heading changed" when the user had
     * changed nothing:
     *
     *   - the record holds a CRLF prefix (the version was published
     *     while the editor was writing CRLF) and the buffer is now LF:
     *     `bodyHasCrlf` is false but the snapshot has no CRLF either, so
     *     nothing is normalised and a CRLF prefix is compared with LF
     *     text;
     *   - `bodyHasCrlf` is true, so normalisation is off by rule, and
     *     the buffer's line endings differ from the record's in either
     *     direction.
     *
     * A heading's line endings are the editor's business, not the
     * user's: they are not something anybody typed, so they cannot be
     * the evidence that the heading was edited. The comparison is made
     * on both sides with CRLF folded to LF, always.
     *
     * THE BODY IS THEN CUT OUT OF THE ORIGINAL TEXT, not out of the
     * folded one, so a block whose stored body really does use CRLF is
     * still sent verbatim. `prefixEndOf` walks the original counting
     * folded characters, which is the only way to turn a position in one
     * into a position in the other.
     */
    const split = bodyOf(snapshot, sidecar);
    if (split === null) {
      return { send: false, refusal: { because: 'prefix-changed', prefix: sidecar.prefix } };
    }
    const { body: src, normalised } = split;
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
   * X1c ⑨: AN ANSWER TO A REQUEST THIS WINDOW DID NOT SEND.
   *
   * A retry after a restart carries a request the queue remembers and
   * this process does not: nothing in memory says which file it was
   * about, so the answer was released and NOTHING WAS RECORDED. The
   * store had the bytes and the record beside the file went on saying it
   * did not, so that block reported unsent work for ever -- and saving
   * it again would send bytes the store already has under a new request.
   * The comment that described this called it "the file is judged a
   * draft by its digests", which is true and is the defect.
   *
   * IT CAN BE RECOVERED WITHOUT GUESSING. The queue kept the text that
   * was sent. If the file still splits -- against its own recorded
   * heading -- into exactly that text, then these bytes are the bytes
   * the store acknowledged, and the digests can be computed from what is
   * here rather than remembered. If it does not, the user has edited
   * since, and the answer is `null`: the caller then does what it did
   * before, which is the safe direction.
   *
   * ⚠️ IT DOES NOT WRITE. Recording goes through `recordAnswer` like
   * every other acknowledgement, so the order -- record first, entry
   * second -- stays in one place. A function that both recognised and
   * recorded would be a second critical section. (§12.7.4, C7)
   */
  public recognise(file: string, sentText: string): { rawDigest: string; sentDigest: string } | null {
    const meta = sidecarPathOf(file);
    if (!this.files.exists(meta) || !this.files.exists(file)) {
      return null;
    }
    /*
     * EVERY READ HERE IS GUARDED, because this runs inside the settler.
     * A throw from it would escape into the Saver's answer handling,
     * where the request has been answered and the entry has not been
     * removed -- and the one thing this function must not do is turn a
     * question it could not answer into a failure of the save it was
     * asked about. `null` means "cannot say", which is what the caller
     * already knows how to do.
     */
    let read;
    try {
      read = sidecarFromDisk(this.files.readText(meta));
    } catch (e) {
      return null;
    }
    if (!read.read || read.sidecar.phase !== 'published' || read.sidecar.unresolved) {
      return null;
    }
    let bytes: Buffer;
    try {
      bytes = this.files.readBytes(file);
    } catch (e) {
      return null;
    }
    let text: string;
    try {
      text = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
    } catch (e) {
      return null;
    }
    const split = bodyOf(text, read.sidecar);
    if (split === null || split.body !== sentText) {
      return null;
    }
    return {
      rawDigest: digestOfBytes(bytes),
      sentDigest: digestOfBytes(Buffer.from(sentText, 'utf8'))
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
    const meta = sidecarPathOf(file);
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
      writeSidecar(this.files, file, { ...read.sidecar, unresolved: true });
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
    /*
     * THROUGH THE SAME DOOR AS EVERY OTHER RECORD. This was a bare
     * `writeText` -- a truncating rewrite of the one file that says what
     * this version was based on, with the paragraph forbidding exactly
     * that sitting on the other implementation in publication.ts.
     */
    /*
     * AND `local-only` IS CLEARED, BECAUSE IT IS NO LONGER TRUE. It says
     * the baseline came from the file rather than from an answer and the
     * store has therefore never seen these bytes -- which an
     * acknowledgement of them is precisely the refutation of. Leaving it
     * set made every reconciled version a draft for ever: the user
     * resolved the conflict, saved, the store took it, and the block
     * went on reporting unsent work with nothing they could do about it.
     */
    writeSidecar(this.files, file, {
      ...read.sidecar,
      acknowledgedRaw: answer.rawDigest,
      sent: answer.sentDigest,
      cursor: answer.cursor,
      localOnly: false
    });
    dequeue();
    return { dequeued: true };
  }
}
