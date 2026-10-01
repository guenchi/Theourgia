import * as path from 'path';
import {Owners} from './ownership';
import {withExclusive} from './fsops';
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
 * (section 12.19.4, section 12.17.3, section 12.15 structure two)
 *
 * ONLY A CLEAN DOCUMENT IS SENT, AND ONLY ITS SNAPSHOT. When the
 * handler runs with `isDirty` false, `getText()` is the text of the
 * last COMPLETED save, so there is no torn read and no second reading:
 * the snapshot is taken once and that same value is what goes out.
 * Dirty means the user typed again after saving -- the next save will
 * carry the newer text, so this one sends nothing. (section 12.19.4)
 *
 * THE BYTES ON DISK ARE EVIDENCE, NOT CONTENT. They are read only to
 * check that they decode strictly as UTF-8, carry no BOM, and equal the
 * snapshot; `acknowledged-raw` is their digest. If any of that fails,
 * nothing is sent and the next save tries again -- a misjudged torn
 * read costs one save, never the wrong content. (section 12.17.3)
 */

import { FileOps } from './fsops';
import {
  digestOfBytes,
  newerEvent,
  replacesBaseline,
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
 * let an implementation confuse them. (section 12.17.3, section 12.13.4)
 */
export type Refusal =
  | { because: 'document-dirty' }
  | { because: 'working-unavailable'; detail:string }
  /*
   * The block is a datum: its code is not the text this file holds, and
   * this editor cannot write one (src/datum-view.ts). Nothing was sent.
   */
  | { because: 'datum-block' }
  | { because: 'byte-order-mark' }
  | { because: 'not-utf8' }
  | { because: 'disk-differs-from-snapshot' }
  | { because: 'prefix-changed'; prefix: string }
  | { because: 'no-sidecar' }
  /*
   * The sidecar says `publishing`: the file and the record do not yet
   * describe one another, so the prefix cannot be trusted to split
   * against. (section 12.7.3, C3)
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
 * entry outlives anything this cannot record. (section 12.7.4, C7, C20)
 */
/*
 * WHY A RECORD BESIDE THE FILE COULD NOT BE WRITTEN. Named as a type
 * because three places have to agree on the list: the one that decides,
 * the one that writes the sentence, and the settler that carries the
 * one to the other. Spelled out at each of them, a new reason reaches
 * two of the three.
 */
export type Unrecorded =
  | 'not-acknowledged'
  | 'req-mismatch'
  | 'file-moved'
  | 'split-changed'
  /*
   * TWO SENDS CLAIM ONE NUMBER. (section 13.3)
   *
   * The baseline holds this send's number under a different request, so
   * the two records disagree about what the number means -- and nothing
   * here can say which is right. It is a different sentence from
   * `req-mismatch`, which is the STORE saying an id names another
   * request: this one is our own two records disagreeing, and the thing
   * to go and look at is different.
   */
  | 'number-taken';

export type AnswerRecording =
  | { dequeued: true }
  | {
      dequeued: false;
      because: Unrecorded;
    };

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

  constructor(files: FileOps, private readonly ownership?: {owners:Owners;sessionId:string}) {
    this.files = files;
  }

  /*
   * Decides, without sending anything: take the snapshot once, verify
   * the disk bytes against it, split against the sidecar's prefix.
   * Returning a decision rather than performing it is what lets a cell
   * assert "zero sends" without watching a transport. (section 12.19.4, C5,
   * C17)
   */
  private requireOwnership(file:string):void {
    if (this.ownership && !this.ownership.owners.mayWrite(path.dirname(file),this.ownership.sessionId).may) {
      throw Object.assign(new Error(`Ownership was lost for ${file}; the receipt and current file are retained.`),{code:'OWNERSHIP_LOST'});
    }
  }

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
     * bytes verified are not the bytes sent. (section 12.19.4)
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
     * NOTE: AND WITHOUT A MARK, UTF-16 CANNOT BE RECOGNISED HERE. All-ASCII
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
       * replacement lands outside the compared region. (section 12.17.3)
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
     * NOTE: THIS ORDER WAS WRONG AND THE EDITOR CELLS FOUND IT. Comparing
     * first meant a buffer the editor writes as CRLF never matched a
     * prefix the store gave as LF, so every save of such a file was
     * refused as "the heading changed" -- by the extension, about a
     * change the user had not made. section 12.17.3 puts the EOL handling
     * first for exactly this reason: the comparison and the split both
     * happen on the normalised text.
     *
     * WHETHER TO NORMALISE IS THE BLOCK'S OWN FACT, recorded when the
     * version was published. A block whose stored body really did use
     * CRLF is sent verbatim; the prefix's line endings say nothing
     * about the body's.
     */
    /*
     * NOTE: AND THE COMPARISON IS ALWAYS EOL-AGNOSTIC, WHICH IT WAS NOT.
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
   * X1c (9): AN ANSWER TO A REQUEST THIS WINDOW DID NOT SEND.
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
   * NOTE: IT DOES NOT WRITE. Recording goes through `recordAnswer` like
   * every other acknowledgement, so the order -- record first, entry
   * second -- stays in one place. A function that both recognised and
   * recorded would be a second critical section. (section 12.7.4, C7)
   */
  public recognise(file: string, sentText: string): { rawDigest: string; sentDigest: string } | null {
    /*
     * NOTE: THE EXISTENCE PROBES ARE GUARDED TOO. They sat outside the
     * `try` below, and `FileOps.exists` is an interface: an
     * implementation that reports a permission failure by throwing would
     * escape from here into the settler, where the request has been
     * answered and the entry has not been removed. The shipped adapter
     * uses `fs.existsSync`, which ordinarily returns false instead --
     * but this function's whole contract is that it answers "cannot
     * say" rather than failing the save it was asked about, and a
     * contract that holds only for one implementation is not one.
     */
    const meta = sidecarPathOf(file);
    try {
      if (!this.files.exists(meta) || !this.files.exists(file)) {
        return null;
      }
    } catch (e) {
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
   * request is gone and the store may or may not hold it. (section 12.7.4, C7)
   *
   * IT RE-READS THE FILE INSIDE THE SECTION. The editor is not on the
   * chain; if the bytes moved while the answer was in flight, the
   * acknowledgement would describe a version that is no longer there,
   * so the write is abandoned and the ENTRY IS KEPT. (section 12.11.1, C20)
   *
   * A `req-mismatch` KEEPS THE ENTRY TOO, marked unresolved: the store
   * is saying it has a different request under that id, which nobody
   * here can resolve by retrying. (section 12.9, C7)
   */
  /*
   * A SEND IS OVER AND NOTHING ELSE IS RECORDED. (section 13.2, R5.6)
   *
   * NOTE: A REFUSAL HAS TO WRITE EXACTLY ONE THING. The store declined the
   * write, so no baseline may be written -- the bytes are still only in
   * the user's file. But the send IS over, and leaving its number in
   * `outstanding` would keep the block a draft it can never stop being:
   * the same false state as recording an acknowledgement, reached from
   * the other direction. "Every verdict writes the record or none of
   * them does" is not one of the choices.
   */
  public releaseSend(file: string, seq: number): boolean {
    return withExclusive(path.dirname(file), (): boolean => {
      this.requireOwnership(file);
    const meta = sidecarPathOf(file);
    if (!this.files.exists(meta)) {
      return false;
    }
    const read = sidecarFromDisk(this.files.readText(meta));
    if (!read.read) {
      return false;
    }
    writeSidecar(this.files, file, {
      ...read.sidecar,
      outstanding: read.sidecar.outstanding.filter((out) => out.seq !== seq)
    });
    return true;

    });
  }

  public recordAnswer(
    file: string,
    answer: {
      req: string;
      cursor: string;
      rawDigest: string;
      sentDigest: string;
      mismatch: boolean;
      /*
       * WHICH SEND THIS ANSWER IS ABOUT. (section 13.3)
       *
       * The record beside a file keeps two different things apart: what
       * the store has CONFIRMED, and what the file now holds. The first
       * of those is about a send -- its number, the split it was made
       * against, and whether it was this client's write or an operator's
       * determination that the work had been carried out.
       *
       * It is required rather than optional because every answer is
       * about some send; a caller that could leave it out would be a
       * caller whose record says the store confirmed something without
       * saying what.
       */
      send: { seq: number; prefixDigest: string; by: 'store' | 'operator'; projectionId?: string };
    },
    dequeue: () => void = () => undefined
  ): AnswerRecording {
    return withExclusive(path.dirname(file), (): AnswerRecording => {
      this.requireOwnership(file);
    const meta = sidecarPathOf(file);
    if (!this.files.exists(meta)) {
      return { dequeued: false, because: 'not-acknowledged' };
    }
    const read = sidecarFromDisk(this.files.readText(meta));
    if (!read.read) {
      return { dequeued: false, because: 'not-acknowledged' };
    }
    if (read.sidecar.phase !== 'published') return {dequeued:false,because:'not-acknowledged'};
    if (read.sidecar.projection && read.sidecar.projection.id !== answer.send.projectionId) {
      if (answer.mismatch) return {dequeued:false,because:'req-mismatch'};
      // Retire only this request's transport evidence. The new origin is untouched.
      writeSidecar(this.files,file,{...read.sidecar,outstanding:read.sidecar.outstanding.filter(o=>!(o.seq===answer.send.seq && o.req===answer.req))});
      dequeue();
      return {dequeued:true};
    }
    if (answer.mismatch) {
      /*
       * THE STORE HAS A DIFFERENT REQUEST UNDER THAT NAME. Nobody here
       * can settle that by retrying, so the entry stays and the file is
       * marked for a person to look at. (section 12.9)
       */
      /*
       * NOTE: AND THE NUMBER STAYS OUT. This send has not settled -- the
       * store is saying it cannot say what happened to it -- so removing
       * it from `outstanding` would make the block read as though
       * nothing were in flight, which is the one thing that is certainly
       * false here. (section 13.2)
       */
      writeSidecar(this.files, file, { ...read.sidecar, unresolved: true });
      return { dequeued: false, because: 'req-mismatch' };
    }
    /*
     * NOTE: THE FILE IS READ ONCE, AND EVERY QUESTION BELOW IS ASKED OF
     * THAT ONE BUFFER.
     *
     * The digest and the split used to be two separate reads, and the
     * writer they are about is in another process. A write landing
     * between them let an acknowledgement be assembled out of two
     * different versions of the file: the first read satisfied the
     * digest, the second satisfied the split, and neither version on its
     * own would have. Reproduced in review, ending in `draft: false`
     * over a record that would send bytes the store never saw.
     *
     * Reading once does not lock anybody out; it makes the two answers
     * be about the same thing, which is what was missing.
     */
    let bytes: Buffer | null;
    try {
      bytes = this.files.exists(file) ? this.files.readBytes(file) : null;
    } catch (e) {
      bytes = null;
    }
    const here = bytes === null ? null : digestOfBytes(bytes);
    if (here !== answer.rawDigest) {
      /*
       * THE BYTES MOVED WHILE THE ANSWER WAS IN FLIGHT. Recording an
       * acknowledgement against them would mark a draft as sent, so the
       * write is abandoned and the ENTRY IS KEPT. (section 12.11.1, C20)
       */
      return { dequeued: false, because: 'file-moved' };
    }
    if (answer.send.by !== 'operator' && !newerEvent(read.sidecar.cursor, answer.cursor)) {
      /*
       * A LATE REPLAY IS A DUPLICATE, NOT AN ERROR. The request has been
       * settled; the cursor stays where the newer answer put it, and the
       * entry goes. (section 12.7.4)
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
     * NOTE: AND THE RECORD MUST STILL SPLIT THE FILE THE WAY THE SEND DID.
     *
     * The digest above says the BYTES have not moved. It says nothing
     * about the PREFIX, and the prefix lives in this same record and can
     * be changed by something else while the answer is in flight --
     * `reconcile` adopting a longer heading does exactly that, and
     * leaves the file's bytes untouched.
     *
     * The sequence, found in review with a reproduction: the file holds
     * `P + X + Y` and the record says the prefix is `P`, so a save sends
     * `X + Y`. While that answer is outstanding, a reconciliation adopts
     * `P + X` as the prefix. The answer arrives; the whole-file digest
     * still matches; the acknowledgement is recorded and `local-only`
     * cleared, so the version reads as fully sent -- while a save under
     * the record as it now stands would send `Y`, which the store has
     * never seen. Work the user can no longer see is pending.
     *
     * So the record is asked to produce the body that went out. If it
     * does not, nothing is written and the ENTRY IS KEPT: the request is
     * still retryable, and a retry re-reads this record.
     */
    const split = bodyOf((bytes as Buffer).toString('utf8'), read.sidecar);
    if (split === null || digestOfBytes(Buffer.from(split.body, 'utf8')) !== answer.sentDigest) {
      return { dequeued: false, because: 'split-changed' };
    }
    /*
     * AND `local-only` IS CLEARED, BECAUSE IT IS NO LONGER TRUE. It says
     * the baseline came from the file rather than from an answer and the
     * store has therefore never seen these bytes -- which an
     * acknowledgement of them is precisely the refutation of. Leaving it
     * set made every reconciled version a draft for ever: the user
     * resolved the conflict, saved, the store took it, and the block
     * went on reporting unsent work with nothing they could do about it.
     */
    /*
     * NOTE: WHICH SEND THE STORE CONFIRMED, AND WHETHER IT REPLACES WHAT
     * WAS THERE. (section 13.3)
     *
     * The axis is `highWater`, not the baseline's own number: an answer
     * for an older send arriving late must not rebuild a baseline that
     * `reconcile` deliberately took off. `replacesBaseline` states the
     * comparison in one place.
     *
     * An answer for an older send still SETTLES -- its number leaves
     * `outstanding` and its entry goes -- it just does not become the
     * baseline. Leaving the number out would keep the block a draft for
     * ever over a send that has been answered.
     */
    const settled = read.sidecar.outstanding.filter((out) => out.seq !== answer.send.seq);
    const replaces = replacesBaseline(read.sidecar.highWater, answer.send.seq);
    /*
     * NOTE: AND IF THE BASELINE ALREADY HOLDS THIS NUMBER FOR ANOTHER
     * REQUEST, NOBODY HERE CAN SAY WHICH SEND IT MEANS.
     *
     * The number is taken from `nextSeq` and written down before
     * anything is sent, so one window cannot repeat it. It can still
     * arrive repeated -- a takeover carries entries numbered against a
     * record somebody has since replaced, and a stop between taking a
     * number and writing it down leaves the next start free to hand it
     * out again. Settling quietly would make one of the two sends
     * disappear; this marks the record and keeps the entry, which is
     * what section 13 asks for when two of our own records disagree.
     */
    const baseline = read.sidecar.confirmed;
    if (
      !replaces &&
      baseline !== null &&
      baseline.by !== 'legacy' &&
      baseline.by !== 'publication' &&
      baseline.seq === answer.send.seq &&
      baseline.req !== answer.req
    ) {
      writeSidecar(this.files, file, { ...read.sidecar, unresolved: true });
      return { dequeued: false, because: 'number-taken' };
    }
    /*
     * NOTE: AN OPERATOR'S DETERMINATION HAS NO POSITION TO RECORD. The core
     * says such a determination does not recover the original
     * execution's event, so there is no cursor -- and writing the
     * queue's own position here would be recording where WE stood as
     * though the store had said it.
     */
    const cursor = answer.send.by === 'operator' ? null : answer.cursor;
    writeSidecar(this.files, file, {
      ...read.sidecar,
      confirmed: replaces
        ? {
            by: answer.send.by,
            req: answer.req,
            seq: answer.send.seq,
            sentDigest: answer.sentDigest,
            rawDigest: answer.rawDigest,
            prefixDigest: answer.send.prefixDigest,
            cursor
          }
        : read.sidecar.confirmed,
      highWater: replaces ? answer.send.seq : read.sidecar.highWater,
      outstanding: settled,
      /*
       * NOTE: THE OLDER FIELDS ARE WRITTEN TOO, and they are a projection
       * of this same act rather than a second record of it: one write,
       * one instant, derived from the same answer. They are what the
       * build before section 13 reads, and what this build's own draft listing
       * still reads until it moves to the pure function. When it does,
       * these become write-only compatibility and can go.
       */
      acknowledgedRaw: answer.rawDigest,
      sent: answer.sentDigest,
      cursor: cursor ?? read.sidecar.cursor,
      localOnly: false
    });
    dequeue();
    return { dequeued: true };

    });
  }
}
