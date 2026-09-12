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
 * Putting a reading of a block into a file, and deciding what a file
 * found on disk is.  (§12.15 结构一, §12.11.1, §12.13.2, §12.17)
 *
 * PUBLICATION IS IMMUTABLE. Every reading taken from the store is
 * written to a NEW path `<n>.md`; this extension never rewrites a file
 * that exists, and never unlinks or renames one it has published. The
 * only writer of an existing file is the editor, through a document it
 * has open. That is what removes the whole class of "checked, then
 * something wrote between the check and the write" -- there is nothing
 * to overwrite. (§12.15 结构一, §12.23)
 *
 * THE SIDECAR IS WRITTEN FIRST, IN THREE STEPS, so that every point at
 * which the process can die is decidable afterwards. (§12.7.3 as it
 * stands in §12.9: phase + previous)
 */

import { createHash } from 'crypto';
import * as path from 'path';
import { FileOps } from './fsops';

/*
 * ONE DIGEST FUNCTION. The sidecar's four digest fields and the draft
 * scan all have to agree about what a digest of some bytes is, and two
 * spellings of that would be two answers to one question.
 */
export function digestOfBytes(bytes: Buffer | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

/*
 * WHAT THE EXTENSION CAN ASK THE EDITOR. `publish` must refuse when a
 * document is open on the target path, because then the editor is a
 * writer and this is not -- and `FileOps` cannot answer that. It is a
 * parameter for the reason the file operations are: this does not
 * need to know whose editor it is, and saying so is what lets a cell
 * drive it. (§12.13.1)
 */
export interface OpenDocuments {
  isOpen(file: string): boolean;
}

/*
 * The record beside `<n>.md`, written as `<n>.md.meta`. The field names are
 * the design's. (§12.9, §12.13.2, §12.17.3)
 */
export interface Sidecar {
  /*
   * THE RECORD SAYS WHICH SHAPE IT IS. A queue written without a version
   * field was the one shape the outbox could not tell from a corrupt
   * one, and a record on disk outlives the build that wrote it.
   * (outbox 的同一课)
   */
  format: 1;
  storeId: string;
  blockId: string;
  /*
   * Which of the three publication steps was last completed. (§12.7.3)
   */
  phase: 'publishing' | 'published';
  /*
   * front + heading-src as they were written, the bytes a save is split
   * against. (§12.1)
   */
  prefix: string;
  /*
   * The digest of the whole text this publication intended to write --
   * an identity for the publication, never a licence to discard
   * content. (§12.13.2)
   */
  written: string;
  /*
   * The digest of the previous version's text, so that a file found
   * mid-publication can be told from a third version the editor wrote.
   * (§12.9)
   */
  previous: string | null;
  /*
   * The digest of the disk bytes that were verified and sent, once the
   * store has answered. Draft detection compares against THIS.
   * (§12.17.3, §12.13.2)
   */
  acknowledgedRaw: string | null;
  /*
   * The digest of the normalised text actually sent. (§12.7.5)
   */
  sent: string | null;
  /*
   * The store position the last answer established. (§12.9)
   */
  cursor: string | null;
  /*
   * Set when `reconcile` established this baseline locally rather than
   * from the store; such a version is treated as a draft. (§12.13.2)
   */
  localOnly: boolean;
  /*
   * Set when the file was found to be a third version; cleared only by
   * `reconcile`. (§12.9, §12.11.7, C15)
   */
  unresolved: boolean;
  /*
   * Whether the block's own body, as the store gave it, contains CRLF.
   * The save path needs this to decide whether a CRLF file is the
   * editor's doing or the block's; the prefix cannot answer it.
   * (§12.17.3, P2-7)
   */
  bodyHasCrlf: boolean;
  /*
   * ⚠️ EVERYTHING BELOW IS §13's RECORD OF WHICH SEND THE STORE
   * CONFIRMED, AND IT IS DELIBERATELY SEPARATE FROM WHAT THE FILE NOW
   * HOLDS.
   *
   * The fields above answer "what was published"; these answer "which
   * send was acknowledged, and what is still out". Keeping them apart is
   * what lets `draft` be a function of persisted inputs rather than of
   * whatever the queue happens to hold in memory -- and it is what makes
   * the sequence the reviews kept finding answer correctly: publish X,
   * save Y and lose the answer, undo to X, save again; when Y's
   * confirmation arrives, `confirmed` becomes Y and the file holds X, so
   * the block is a draft by the model rather than by luck of timing.
   *
   * NOTHING READS THEM YET. This step adds them and the rule for reading
   * an older record; the switch-over is one landing of its own, because
   * a half-switched save path would have two suppliers of one state --
   * which is the defect this whole section exists to remove.
   */
  confirmed: Confirmed | null;
  /*
   * The sends that have been written down and not yet settled, by
   * sequence and request. A file with any of these is not clean however
   * its bytes compare: something is out there whose outcome nobody
   * knows.
   */
  outstanding: Outstanding[];
  /*
   * The highest sequence ever confirmed for this file. It only goes up,
   * and `reconcile` may drop `confirmed` without touching it -- so a
   * late answer from an older send cannot re-establish a baseline the
   * user has just removed. Both the supersede check before sending and
   * the replacement at settlement use THIS, so the two cannot disagree.
   */
  highWater: number;
  /*
   * The next sequence to hand out for this file. Taken and persisted
   * before a send, so that two sends cannot share a number across a
   * restart.
   */
  nextSeq: number;
  /*
   * Who wrote this record, by session and by the generation of the
   * ownership they held. A reader that finds a generation other than the
   * current owner's is looking at a write by a session that had lost the
   * block: this cannot prevent that write, but it can stop it being
   * silent.
   */
  writtenBy: WrittenBy | null;
}

/*
 * WHICH SEND THE STORE CONFIRMED -- not what the file holds now.
 */
export type Confirmed =
  | {
      by: 'store' | 'operator';
      req: string;
      seq: number;
      sentDigest: string;
      rawDigest: string;
      prefixDigest: string;
      /*
       * Null when an operator determined the work had been carried out:
       * the core says such a determination does not recover the original
       * execution's event, so there is no position to record.
       */
      cursor: string | null;
    }
  /*
   * ⚠️ WHAT A RECORD WRITTEN BEFORE §13 STILL TELLS US.
   *
   * My first version read an older sidecar as having NO baseline, on the
   * grounds that deriving one would invent a request id and a prefix
   * digest nobody wrote down. That reasoning was right about the
   * inventing and wrong about the conclusion: `acknowledgedRaw` is a
   * fact the old build recorded -- the bytes the store took -- and
   * throwing it away turns every block that was ever saved into a draft
   * the moment this build runs. I had just written the rule that stops
   * exactly that for `outstanding`, and then broke it one field along.
   * The main session caught it.
   *
   * So the old fields become a baseline that says only what they said:
   * which bytes were acknowledged, and where the store stood. It has no
   * request id because none was recorded, and the type says so rather
   * than leaving a field empty for somebody to read as one. `seq` is 0,
   * so the first confirmation this build writes replaces it.
   */
  | { by: 'legacy'; seq: 0; rawDigest: string; cursor: string | null };

export interface Outstanding {
  seq: number;
  req: string;
}

export interface WrittenBy {
  sid: string;
  generation: number;
}

/*
 * What a file on disk is, judged against its sidecar. Every answer
 * carries why, because the caller's next move differs for each and
 * because a boolean here was how three of the earlier rounds went
 * wrong. (§12.9, §12.17.4)
 */
export type Standing =
  | { kind: 'published'; draft: boolean }
  | { kind: 'mid-publication'; complete: boolean }
  | { kind: 'third-version' }
  | { kind: 'absent' };

/*
 * Whether the answer was recorded, and if not, why -- because the
 * caller may only remove the outbox entry once it WAS. Returning
 * nothing would leave "record then dequeue" as an ordering the code
 * merely happens to be written in. (§12.7.4, C7, C20)
 */
export type Acknowledgement =
  | { recorded: true }
  | { recorded: false; because: 'older-event' | 'no-sidecar' };

export interface PublishRequest {
  directory: string;
  storeId: string;
  blockId: string;
  prefix: string;
  text: string;
}

export type PublishOutcome =
  | { published: true; file: string; version: number }
  | { published: false; because: 'document-open' | 'digest-moved'; file: string | null };

/*
 * THE NAMES ON DISK ARE THE DESIGN'S NAMES, NOT THIS LANGUAGE'S.
 *
 * `<n>.md.meta` is a record that outlives the build that wrote it and may
 * be read by something that is not this extension, so its keys are the
 * ones §12 uses -- `acknowledged-raw`, `local-only` -- rather than
 * whatever casing TypeScript is comfortable with. Inside the program
 * the fields are camelCase like everything else, and these two
 * functions are the whole of the difference.
 *
 * IT IS A MAPPING, NOT A CAST. Writing `JSON.stringify(sidecar)` would
 * put the language's spelling on disk and nothing would notice until
 * another reader tried; naming both sides here makes the file format a
 * thing that was decided. (§12.9, and the outbox's `format` lesson)
 */
export function sidecarToDisk(sidecar: Sidecar): Record<string, unknown> {
  return {
    format: sidecar.format,
    'store-id': sidecar.storeId,
    'block-id': sidecar.blockId,
    phase: sidecar.phase,
    prefix: sidecar.prefix,
    written: sidecar.written,
    previous: sidecar.previous,
    'acknowledged-raw': sidecar.acknowledgedRaw,
    sent: sidecar.sent,
    cursor: sidecar.cursor,
    'local-only': sidecar.localOnly,
    unresolved: sidecar.unresolved,
    'body-has-crlf': sidecar.bodyHasCrlf,
    /*
     * ⚠️ WRITTEN EVEN WHILE NOTHING READS THEM. A record this build
     * writes must be one this build can read back with the same meaning,
     * and a field that is only sometimes present is a second shape on
     * disk. `confirmed` is null until a settlement writes one; the
     * counters start where `confirmationFrom` says an older record
     * starts, so a file written now and a file written before §13 are
     * read the same way.
     */
    /*
     * ⚠️ A LEGACY BASELINE IS NOT WRITTEN BACK. It is what an older
     * record MEANS, worked out on the way in -- and the fields it was
     * worked out FROM (`acknowledged-raw`, `cursor`) are written above,
     * unchanged, so the next read derives it again.
     *
     * Writing it would be worse than useless: `confirmed` would then be
     * present but without the request id and digests a real one has, so
     * the reader would refuse it and answer null -- and the block would
     * become a draft, which is the exact failure this baseline exists to
     * prevent. I wrote the comment saying this and then wrote the code
     * doing the opposite; the compiler had nothing to say about it.
     */
    confirmed:
      sidecar.confirmed === null || sidecar.confirmed.by === 'legacy'
        ? null
        : confirmedToDisk(sidecar.confirmed),
    outstanding: sidecar.outstanding,
    'high-water': sidecar.highWater,
    'next-seq': sidecar.nextSeq,
    'written-by': sidecar.writtenBy
  };
}

function confirmedToDisk(confirmed: Exclude<Confirmed, { by: 'legacy' }>): Record<string, unknown> {
  return {
    req: confirmed.req,
    seq: confirmed.seq,
    'sent-digest': confirmed.sentDigest,
    'raw-digest': confirmed.rawDigest,
    'prefix-digest': confirmed.prefixDigest,
    cursor: confirmed.cursor,
    by: confirmed.by
  };
}

/*
 * WHAT A FILE THIS BUILD HAS NOT NUMBERED STARTS FROM. One place, so
 * that a fresh publication and a record read from an older build agree
 * about where the counters begin.
 */
export const UNNUMBERED: Pick<
  Sidecar,
  'confirmed' | 'outstanding' | 'highWater' | 'nextSeq' | 'writtenBy'
> = {
  confirmed: null,
  outstanding: [],
  highWater: 0,
  nextSeq: 1,
  writtenBy: null
};

/*
 * Refuses a record it cannot read rather than repairing it, for the
 * reason the outbox refuses one: a record this build does not
 * understand may be the only trace of work, and a silent repair writes
 * over it. (§12.9)
 */
export type SidecarRead =
  | { read: true; sidecar: Sidecar }
  | { read: false; because: 'absent' | 'unreadable' | 'later-format'; detail: string };

export function sidecarFromDisk(text: string): SidecarRead {
  let raw: unknown;
  try {
    raw = JSON.parse(text);
  } catch (e) {
    return { read: false, because: 'unreadable', detail: String(e) };
  }
  if (typeof raw !== 'object' || raw === null || Array.isArray(raw)) {
    return { read: false, because: 'unreadable', detail: 'the record is not an object' };
  }
  const record = raw as Record<string, unknown>;
  /*
   * A LATER FORMAT IS NOT A CORRUPT ONE, and the two need different
   * answers: a build that read a later record as this one would act on
   * fields it does not understand. (§12.9)
   */
  if (record.format !== 1) {
    return { read: false, because: 'later-format', detail: `format ${String(record.format)}` };
  }
  const text_ = (key: string): string | null => {
    const value = record[key];
    return typeof value === 'string' ? value : null;
  };
  const required = (key: string): string | null => text_(key);
  const phase = record.phase;
  if (phase !== 'publishing' && phase !== 'published') {
    return { read: false, because: 'unreadable', detail: `phase ${String(phase)}` };
  }
  for (const key of ['store-id', 'block-id', 'prefix', 'written']) {
    if (required(key) === null) {
      return { read: false, because: 'unreadable', detail: `no ${key}` };
    }
  }
  return {
    read: true,
    sidecar: {
      format: 1,
      storeId: required('store-id') as string,
      blockId: required('block-id') as string,
      phase,
      prefix: required('prefix') as string,
      written: required('written') as string,
      previous: text_('previous'),
      acknowledgedRaw: text_('acknowledged-raw'),
      sent: text_('sent'),
      cursor: text_('cursor'),
      localOnly: record['local-only'] === true,
      unresolved: record.unresolved === true,
      bodyHasCrlf: record['body-has-crlf'] === true,
      ...confirmationFrom(record)
    }
  };
}

/*
 * ⚠️ WHAT A RECORD WRITTEN BEFORE §13 MEANS, WRITTEN DOWN RATHER THAN
 * LEFT TO ARITHMETIC.
 *
 * An older sidecar has no `confirmed`, no `outstanding`, no `highWater`
 * and no `nextSeq`. Every one of those has an answer that is obvious
 * once stated and wrong if guessed:
 *
 * - `highWater` and the absent `confirmed`'s seq are **0**, not null and
 *   not undefined. Comparisons are `record.seq > highWater`, and `1 >
 *   null` is true in this language for reasons that have nothing to do
 *   with sequence numbers -- an answer that is right by accident, which
 *   this batch has already been caught by once.
 * - `outstanding` is **empty**. Read as anything else, every block
 *   written by an older build becomes a draft the moment this one runs.
 * - `nextSeq` is **1**: the first sequence this build hands out for a
 *   file it did not number.
 * - `confirmed` stays **null**. The old fields still say what was
 *   acknowledged, and the readers that use them are unchanged in this
 *   step; deriving a `confirmed` from them here would invent a request
 *   id and a prefix digest that nobody recorded.
 */
function confirmationFrom(
  record: Record<string, unknown>
): Pick<Sidecar, 'confirmed' | 'outstanding' | 'highWater' | 'nextSeq' | 'writtenBy'> {
  const held = record.confirmed;
  const confirmed =
    typeof held === 'object' && held !== null && !Array.isArray(held)
      ? readConfirmed(held as Record<string, unknown>)
      : legacyBaseline(record);
  const out = Array.isArray(record.outstanding) ? record.outstanding : [];
  const outstanding: Outstanding[] = [];
  for (const item of out) {
    if (typeof item === 'object' && item !== null && !Array.isArray(item)) {
      const entry = item as Record<string, unknown>;
      if (typeof entry.seq === 'number' && typeof entry.req === 'string') {
        outstanding.push({ seq: entry.seq, req: entry.req });
      }
    }
  }
  const wrote = record['written-by'];
  const writtenBy =
    typeof wrote === 'object' && wrote !== null && !Array.isArray(wrote)
      ? readWrittenBy(wrote as Record<string, unknown>)
      : null;
  return {
    confirmed,
    outstanding,
    highWater: typeof record['high-water'] === 'number' ? record['high-water'] : 0,
    nextSeq: typeof record['next-seq'] === 'number' ? record['next-seq'] : 1,
    writtenBy
  };
}

/*
 * A CONFIRMATION IS ALL OF ITS FIELDS OR NONE OF THEM. A half-read one
 * would be a baseline naming a send nobody can identify, which is worse
 * than having none: the absence is handled, the half is believed.
 */
function readConfirmed(held: Record<string, unknown>): Confirmed | null {
  const text = (key: string): string | null =>
    typeof held[key] === 'string' ? (held[key] as string) : null;
  const req = text('req');
  const sentDigest = text('sent-digest');
  const rawDigest = text('raw-digest');
  const prefixDigest = text('prefix-digest');
  const by = held.by;
  if (
    req === null ||
    sentDigest === null ||
    rawDigest === null ||
    prefixDigest === null ||
    typeof held.seq !== 'number' ||
    (by !== 'store' && by !== 'operator')
  ) {
    return null;
  }
  return { req, seq: held.seq, sentDigest, rawDigest, prefixDigest, cursor: text('cursor'), by };
}

/*
 * THE BASELINE AN OLDER RECORD IMPLIES, AND NOTHING MORE.
 *
 * `acknowledged-raw` is the digest of the bytes the store took. If it is
 * absent the old build never had an answer for this file either, and the
 * honest reading is that there is no baseline -- the block is a draft,
 * which is what it was before this build ran too.
 */
function legacyBaseline(record: Record<string, unknown>): Confirmed | null {
  const raw = record['acknowledged-raw'];
  if (typeof raw !== 'string') {
    return null;
  }
  const cursor = record.cursor;
  return { by: 'legacy', seq: 0, rawDigest: raw, cursor: typeof cursor === 'string' ? cursor : null };
}

function readWrittenBy(wrote: Record<string, unknown>): WrittenBy | null {
  if (typeof wrote.sid !== 'string' || typeof wrote.generation !== 'number') {
    return null;
  }
  return { sid: wrote.sid, generation: wrote.generation };
}

/*
 * The only way out of `unresolved`. (§12.11.7, C3, C15)
 *
 * WHEN THE FILE ALREADY STARTS WITH THE STORE'S PREFIX there is nothing
 * to ask: the baseline can be established from what is there, and the
 * body becomes work the store has not got -- which is a draft, and
 * saveable. Otherwise the user is shown three texts and picks, because
 * nothing here can know which of them they meant.
 */
export type Reconciliation =
  | { reconciled: true; because: 'prefix-already-present' }
  | {
      reconciled: false;
      /*
       * Both actions run on the chain. `prepend-prefix` keeps the user's
       * bytes and puts the store's prefix in front of them;
       * `take-store-version` publishes a new version from the store and
       * leaves the old file alone -- it is never deleted, because this
       * extension deletes nothing. (§12.11.7, §12.23)
       */
      choices: Array<'prepend-prefix' | 'take-store-version'>;
      storeText: string;
      previousText: string | null;
      fileText: string;
    };

/*
 * WHETHER AN ANSWER IS NEWS. For the SAME writer the sequence numbers
 * are compared numerically; a DIFFERENT writer is a later generation and
 * is accepted. A late replay carrying an older position must not move
 * the cursor backwards. (§12.7.4)
 */
export function newerEvent(held: string | null, arriving: string): boolean {
  if (held === null) {
    return true;
  }
  const split = (value: string): [string, number] => {
    const at = value.lastIndexOf(':');
    return at < 0 ? [value, 0] : [value.slice(0, at), Number(value.slice(at + 1))];
  };
  const [heldWriter, heldSeq] = split(held);
  const [writer, seq] = split(arriving);
  if (heldWriter !== writer) {
    return true;
  }
  return Number.isFinite(seq) && Number.isFinite(heldSeq) && seq >= heldSeq;
}

/*
 * WHERE A VERSION'S RECORD LIVES. One spelling, because a reader that
 * looked somewhere else than the writer is the shape this batch has
 * already met twice -- once with a field name and once with a queue
 * path, both of which read as "there is nothing there".
 */
export function sidecarPathOf(file: string): string {
  return `${file}.meta`;
}

/*
 * THE ONLY WAY A RECORD IS WRITTEN, AND IT IS NEVER TRUNCATED.
 *
 * `writeText` opens for writing, which empties the file first: a process
 * stopped in that window leaves a record nothing can read, and an
 * unreadable record makes its file count as absent -- it vanishes from
 * the draft listing, so surviving work becomes invisible through its own
 * bookkeeping. A temporary file renamed into place is either the old
 * record or the new one and never neither.
 *
 * THE BYTES ARE ON THE DEVICE BEFORE THE RENAME, and the directory entry
 * after it, for the reason the outbox does the same: a rename decides
 * what a reader sees and says nothing about what survives a machine
 * losing power.
 *
 * ⚠️ IT IS A FUNCTION RATHER THAN A METHOD BECAUSE IT HAS TWO CALLERS.
 * `saving.ts` wrote the record with a bare `writeText` -- the exact
 * truncation this paragraph forbids -- while the comment explaining why
 * that is unsafe sat in this file, on the other implementation. A rule
 * stated in one place and enforced in one of two is a rule half the code
 * does not have.
 */
export function writeSidecar(files: FileOps, file: string, sidecar: Sidecar): void {
  const meta = sidecarPathOf(file);
  const temporary = `${meta}.${process.pid}.tmp`;
  files.writeDurably(temporary, `${JSON.stringify(sidecarToDisk(sidecar), null, 2)}\n`);
  files.rename(temporary, meta);
  files.syncDirectory(path.dirname(meta));
}

/*
 * IS THIS FILE HOLDING WORK THE STORE HAS NOT GOT?
 *
 * ⚠️ A FUNCTION OF WHAT IS ON DISK, AND OF NOTHING ELSE.
 *
 * Four persisted inputs: the file's bytes, its record, who owns the
 * block, and what the owner's queue still holds for it. Nothing in
 * memory is an input -- not the Saver, not a map of pending saves, not
 * whether a drain happens to be running. That is the whole point: the
 * answer this returns has to be the same for a window that has just
 * started as for one that has been running all day, because the user is
 * asking about their file, not about our process.
 *
 * ⚠️ THE QUEUE IS AN INPUT, AND IT IS NOT THE SAME AS THE RECORD. The
 * record's `outstanding` can be lost -- it lives in a file that is
 * rewritten whole -- while the queue still holds the entry, and the
 * reverse happens when a takeover carries the entry away. Asking both
 * and saying so when they disagree is the difference between reporting a
 * state and guessing one.
 *
 * NOTHING CALLS THIS YET. `standingOf` still answers from the older
 * fields; switching the callers is a landing of its own.
 */
export type Cleanliness =
  | { clean: true }
  | { clean: false; because: 'never-confirmed' | 'bytes-moved' | 'prefix-moved' | 'still-out' }
  | { clean: false; because: 'records-disagree'; detail: string };

export interface QueueView {
  /*
   * The sequences this file still has entries for in the owner's queue,
   * in any state that is not terminal.
   */
  unsettled: number[];
}

export function cleanliness(bytes: Buffer, sidecar: Sidecar, queue: QueueView): Cleanliness {
  const confirmed = sidecar.confirmed;
  /*
   * THE TWO RECORDS OF WHAT IS OUT MUST AGREE, and when they do not the
   * answer is neither "clean" nor "draft" -- it is that this file's
   * records contradict each other, which is a thing for a person. The
   * three ways a disagreement is NOT one are named where they arise:
   * an entry carried away by a takeover (the queue is right to be
   * missing it), a request that has been retired (the tombstone is the
   * authority), and an orphan marker from a crash between the two
   * writes (reconcile clears it).
   */
  const recorded = sidecar.outstanding.map((o) => o.seq).sort((a, b) => a - b);
  const held = [...queue.unsettled].sort((a, b) => a - b);
  if (recorded.length === 0 && held.length > 0) {
    return {
      clean: false,
      because: 'records-disagree',
      detail: `the record says nothing is out and the queue holds ${held.join(', ')}`
    };
  }
  if (confirmed === null) {
    return { clean: false, because: 'never-confirmed' };
  }
  if (recorded.length > 0 || held.length > 0) {
    return { clean: false, because: 'still-out' };
  }
  if (digestOfBytes(bytes) !== confirmed.rawDigest) {
    return { clean: false, because: 'bytes-moved' };
  }
  /*
   * ⚠️ A LEGACY BASELINE IS NOT ASKED A QUESTION IT CANNOT ANSWER. The
   * older build recorded no prefix digest, so comparing one would make
   * every upgraded file fail a check about something nobody wrote down.
   * What guarded the prefix before this build is unchanged and still
   * guards it; this baseline is replaced by the first confirmation with
   * a sequence above zero.
   */
  if (confirmed.by === 'legacy') {
    return { clean: true };
  }
  /*
   * AND THE SPLIT HAS TO BE THE ONE THAT WAS SENT. `reconcile` can adopt
   * a longer heading without touching a byte of the body: the file then
   * equals what was acknowledged while the text that would be SENT from
   * it no longer does.
   */
  if (digestOfBytes(sidecar.prefix) !== confirmed.prefixDigest) {
    return { clean: false, because: 'prefix-moved' };
  }
  return { clean: true };
}

export class Publisher {
  /*
   * `files` and `documents` are both handed in: this does not need to
   * know whose file system or whose editor it is, and that is what lets
   * a cell count what it did. (§12.20, C2/C10)
   */
  private readonly files: FileOps;
  private readonly documents: OpenDocuments;

  constructor(files: FileOps, documents: OpenDocuments) {
    this.files = files;
    this.documents = documents;
  }

  private metaOf(file: string): string {
    return sidecarPathOf(file);
  }

  private nextVersion(directory: string): number {
    let n = 1;
    while (this.files.exists(path.join(directory, `${n}.md`)) || this.files.exists(path.join(directory, `${n}.md.meta`))) {
      n += 1;
    }
    return n;
  }

  /*
   * THE NEWEST VERSION IN A DIRECTORY, or `null` when there is none.
   *
   * ⚠️ IT IS PUBLIC BECAUSE AN ANSWER CAN OUTLIVE THE WINDOW THAT SENT
   * IT. A retry after a restart names a request and no file, and the
   * block's own directory is the only place to look.
   *
   * ⚠️ IT IS A CANDIDATE, NOT THE VERSION THE REQUEST CAME FROM. This
   * said the newest version "is the one a save was sent from", which is
   * false: a save can go out from `1.md` and `2.md` be published before
   * the answer arrives. What `saving.recognise` then establishes is that
   * the candidate's body is exactly the text that was sent -- so the
   * acknowledgement it records is TRUE OF THAT FILE, which is what the
   * record claims. It is not a claim about which version the request was
   * composed from, and nothing here should be read as one.
   */
  public latestIn(directory: string): string | null {
    return this.latestFile(directory);
  }

  private latestFile(directory: string): string | null {
    let n = 1;
    let last: string | null = null;
    for (;;) {
      const candidate = path.join(directory, `${n}.md`);
      if (!this.files.exists(candidate) && !this.files.exists(`${candidate}.meta`)) {
        return last;
      }
      last = candidate;
      n += 1;
    }
  }

  /*
   * Every record this class writes goes through `writeSidecar`, which
   * says why it is a rename and not a write.
   */
  private write(file: string, sidecar: Sidecar): void {
    writeSidecar(this.files, file, sidecar);
  }

  /*
   * Writes the next version: sidecar `publishing`, then the file, then
   * sidecar `published`. Refuses when a document is open on the target
   * path, because then the editor is a writer and this is not.
   * (§12.13.1, §12.15 结构一)
   */
  public async publish(request: PublishRequest): Promise<PublishOutcome> {
    this.files.makeDirectory(request.directory);
    const version = this.nextVersion(request.directory);
    const file = path.join(request.directory, `${version}.md`);
    if (this.documents.isOpen(file)) {
      return { published: false, because: 'document-open', file };
    }
    const previousFile = this.latestFile(request.directory);
    const previous =
      previousFile !== null && this.files.exists(previousFile)
        ? digestOfBytes(this.files.readBytes(previousFile))
        : null;
    return this.publishInto(request.directory, request, previous);
  }

  /*
   * THE ONE PLACE A VERSION IS WRITTEN. Both `publish` and the
   * store-version branch of `reconcileBy` come here, so the refusal for
   * an open target and the three-step record exist once. (§12.13.1,
   * §12.15 结构一)
   */
  private publishInto(
    directory: string,
    what: { storeId: string; blockId: string; prefix: string; text: string },
    previous: string | null
  ): PublishOutcome {
    this.files.makeDirectory(directory);
    const version = this.nextVersion(directory);
    const file = path.join(directory, `${version}.md`);
    if (this.documents.isOpen(file)) {
      return { published: false, because: 'document-open', file };
    }
    const record: Sidecar = {
      ...UNNUMBERED,
      format: 1,
      storeId: what.storeId,
      blockId: what.blockId,
      phase: 'publishing',
      prefix: what.prefix,
      written: digestOfBytes(Buffer.from(what.text, 'utf8')),
      previous,
      acknowledgedRaw: null,
      sent: null,
      cursor: null,
      localOnly: false,
      unresolved: false,
      /*
       * WHETHER THE BLOCK ITSELF HOLDS CARRIAGE RETURNS, recorded when
       * it is written rather than guessed later from the prefix. The
       * prefix's line endings say nothing about the body's, and a save
       * that guessed from them normalised a block whose stored body
       * really did contain CRLF. (§12.17.3, P2-7)
       */
      bodyHasCrlf: what.text.slice(what.prefix.length).includes('\r\n')
    };
    this.write(file, record);
    this.files.writeText(file, what.text);
    this.write(file, { ...record, phase: 'published' });
    return { published: true, file, version };
  }

  /*
   * Reads `<n>.md.meta` and `<n>.md` and says what the file is. The three
   * mid-publication answers are distinguished by comparing the file's
   * digest with `written` and with `previous`; neither ⇒ the editor
   * wrote a third version. (§12.9)
   */
  public standingOf(file: string): Standing {
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return { kind: 'absent' };
    }
    /*
     * A RECORD WITH NO FILE IS NOT AN EMPTY PLACE. It is a publication
     * that died between saying `publishing` and writing -- the first of
     * the three points -- and the answer the caller needs is "the write
     * never happened, republish it", not "there is nothing here".
     * Checking the file's existence before the record's phase collapsed
     * those two into one answer.
     */
    if (!this.files.exists(file)) {
      return sidecar.phase === 'publishing'
        ? { kind: 'mid-publication', complete: false }
        : { kind: 'absent' };
    }
    const digest = digestOfBytes(this.files.readBytes(file));
    if (sidecar.phase === 'publishing') {
      if (digest === sidecar.written) {
        return { kind: 'mid-publication', complete: true };
      }
      if (sidecar.previous !== null && digest === sidecar.previous) {
        return { kind: 'mid-publication', complete: false };
      }
      /*
       * NEITHER THE TARGET NOR WHAT WAS THERE. Something else wrote
       * while the publication was in flight, and its bytes are the only
       * copy -- so they are never overwritten. (§12.9)
       */
      return { kind: 'third-version' };
    }
    if (sidecar.unresolved) {
      return { kind: 'third-version' };
    }
    /*
     * A DRAFT IS BYTES THE STORE HAS NOT ACKNOWLEDGED, and a baseline
     * `reconcile` built locally is always one: the store has never seen
     * it. (§12.13.2, §12.19.2)
     */
    /*
     * A FRESHLY PUBLISHED VERSION IS NOT A DRAFT. It holds exactly the
     * bytes the store gave, which is what `written` records -- there is
     * no work pending in it. Judging only against `acknowledged-raw`
     * made every version a draft from the moment it was written, before
     * the user had touched it. (§12.19.2's own rule names both digests.)
     *
     * ⚠️ BUT `written` STOPS BEING THE BASELINE THE MOMENT THE STORE
     * ANSWERS. Accepting EITHER digest for ever hid a real unsent edit:
     * publish, edit the body, save, and the store now holds the new
     * body; type the ORIGINAL text back in and the file matches
     * `written` again, so the block reported nothing pending while the
     * store held something else. The bytes in the file differ from the
     * bytes the store has -- that is a draft, and it is the one a user
     * is least likely to suspect, because they got there by undoing.
     *
     * So `acknowledged-raw` REPLACES `written` once it exists, rather
     * than joining it.
     *
     * A `local-only` VERSION IS ALWAYS ONE, whatever its digests say:
     * its baseline came from the file rather than from an answer.
     */
    const baseline = sidecar.acknowledgedRaw ?? sidecar.written;
    const draft = sidecar.localOnly || digest !== baseline;
    return { kind: 'published', draft };
  }

  public sidecarOf(file: string): Sidecar | null {
    const meta = this.metaOf(file);
    if (!this.files.exists(meta)) {
      return null;
    }
    const read = sidecarFromDisk(this.files.readText(meta));
    return read.read ? read.sidecar : null;
  }

  /*
   * Records what the store confirmed, and says whether it did.
   *
   * THIS IS THE ONLY PLACE THE ORDER LIVES. The outbox entry may be
   * removed only after this answers `recorded: true`; the caller that
   * dequeues is `saver.ts`'s answer critical section, and nowhere else
   * decides it. A rule spread over two call sites is a rule that gets
   * half-changed. (§12.7.4, C7, C20)
   *
   * NEWER IS DEFINED, NOT ASSUMED. A late replay must not move the
   * cursor backwards: for the SAME writer the sequence numbers are
   * compared numerically; a DIFFERENT writer is a later generation and
   * is accepted. Anything else is `older-event`. (§12.7.4)
   */
  /*
   * Offers the way out, without taking it. (§12.11.7, C3)
   */
  public reconcile(file: string, storePrefix: string, storeText: string): Reconciliation {
    const fileText = this.files.readText(file);
    if (fileText.startsWith(storePrefix)) {
      const sidecar = this.sidecarOf(file);
      if (sidecar !== null) {
        /*
         * ONLY THE RECORD CHANGES HERE. The bytes are already what the
         * user wants; what was missing was a baseline to measure a save
         * against, and `local-only` says the store has not seen them.
         */
        /*
         * THE BASELINE IS WHAT IS THERE. The body becomes work the store
         * has not got, which is a draft and is saveable -- and the
         * version is marked `local-only`, because this baseline came
         * from the file rather than from an answer. (§12.11.7)
         */
        this.write(file, {
          ...sidecar,
          prefix: storePrefix,
          written: digestOfBytes(Buffer.from(fileText, 'utf8')),
          phase: 'published',
          acknowledgedRaw: null,
          localOnly: true,
          unresolved: false
        });
      }
      return { reconciled: true, because: 'prefix-already-present' };
    }
    /*
     * THE THIRD TEXT IS THE VERSION BEFORE THIS ONE, if it is still on
     * disk. The user is choosing between three things and can only do
     * that if they are shown three things; `null` here meant the offer
     * named a text it never produced.
     */
    const directory = path.dirname(file);
    const versions = this.files
      .list(directory)
      .filter((name) => /^\d+\.md$/.test(name))
      .map((name) => Number(name.slice(0, -3)))
      .sort((a, b) => a - b);
    const mine = Number(path.basename(file).slice(0, -3));
    const before = versions.filter((n) => n < mine).pop();
    const previousText =
      before === undefined ? null : this.files.readText(path.join(directory, `${before}.md`));
    return {
      reconciled: false,
      choices: ['prepend-prefix', 'take-store-version'],
      storeText,
      previousText,
      fileText
    };
  }

  /*
   * Takes one of the two offered actions. Clearing `unresolved` happens
   * here and nowhere else: a build that cleared it on startup would
   * re-arm the overwrite the third-version judgement exists to prevent,
   * and would pass every other cell. (§12.11.7, C15)
   */
  public reconcileBy(
    file: string,
    action: 'prepend-prefix' | 'take-store-version',
    storePrefix: string,
    storeText: string,
    offered?: string
  ): { done: boolean; file: string; because?: 'no-record' | 'file-changed' | 'document-open' } {
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return { done: false, file, because: 'no-record' };
    }
    /*
     * ⚠️ THE ACTION IS CARRIED OUT AGAINST THE TEXT THAT WAS OFFERED, OR
     * NOT AT ALL.
     *
     * `reconcile` shows the user three texts and they pick; the pick
     * waits on a human, and this file is not theirs alone -- another
     * window can replace it while the list is open. `prepend-prefix`
     * then re-read the file and published somebody else's bytes with the
     * heading in front, while the confirmation said the user's own text
     * had been kept. Found in review, with a reproduction.
     *
     * THE CHECK LIVES HERE RATHER THAN AT THE CALL SITE because the
     * caller that shows the list is the one part of this that no cell
     * can reach. `offered` is optional so that a caller which did not
     * offer anything -- a cell exercising the actions themselves -- is
     * not forced to invent a value; passing it is what makes the
     * guarantee, and the extension passes it.
     */
    /*
     * ⚠️ ONE READ, AND THE ACTION USES THAT SAME TEXT.
     *
     * The first version of this guard read the file, compared it, and
     * then let `prepend-prefix` read the file AGAIN -- two reads with a
     * gap between them, and the writer this is protecting against is in
     * another process, which the chain cannot exclude. A write landing
     * in that gap passed the guard and was then published: exactly the
     * defect the guard was added for, one step further along. Found in
     * review with a reproduction.
     *
     * A DISAPPEARING FILE IS `file-changed` AND NOT A THROW. It went
     * away, which is a thing the user has to be told in the words the
     * caller already has for "look again"; an ENOENT escaping from here
     * would leave the command with no answer at all.
     */
    let current: string | null;
    try {
      current = this.files.readText(file);
    } catch (e) {
      current = null;
    }
    if (offered !== undefined && current !== offered) {
      return { done: false, file, because: 'file-changed' };
    }
    if (current === null) {
      return { done: false, file, because: 'file-changed' };
    }
    if (action === 'prepend-prefix') {
      /*
       * THE USER'S BYTES ARE KEPT AND A NEW VERSION CARRIES THEM WITH
       * THE PREFIX IN FRONT.
       *
       * ⚠️ THE FILE IS NOT REWRITTEN. An earlier version of this read
       * the file and wrote the joined text back over it -- over the only
       * copy of a draft, through a truncating write, on a path the
       * editor may have open and which is not on the chain. A process
       * stopped there leaves zero bytes where the user's work was. The
       * rule that publication is immutable is not suspended because the
       * user authorised the content; it is exactly what makes the
       * authorisation safe. (§12.15 结构一, §12.11.7)
       *
       * The new version is marked `local-only`: its baseline came from
       * the file rather than from an answer, so the store has not seen
       * it and it counts as a draft until a save is confirmed.
       */
      /*
       * THE TEXT THE GUARD ABOVE READ, not another read of the same
       * path. See the paragraph there.
       */
      const joined = current.startsWith(storePrefix) ? current : `${storePrefix}${current}`;
      const outcome = this.publishInto(
        path.dirname(file),
        { storeId: sidecar.storeId, blockId: sidecar.blockId, prefix: storePrefix, text: joined },
        digestOfBytes(this.files.readBytes(file))
      );
      if (!outcome.published) {
        return { done: false, file, because: 'document-open' };
      }
      const fresh = this.sidecarOf(outcome.file);
      if (fresh !== null) {
        this.write(outcome.file, { ...fresh, localOnly: true });
      }
      this.write(file, { ...sidecar, unresolved: false });
      return { done: true, file: outcome.file };
    }
    /*
     * TAKING THE STORE'S VERSION PUBLISHES A NEW ONE, THROUGH THE SAME
     * DOOR. The file the user had stays exactly where it is: this
     * extension deletes nothing, and the bytes they typed are the only
     * copy of them. (§12.23)
     *
     * IT GOES THROUGH `publishInto` rather than repeating the three
     * steps, because the refusal for a path the editor has open belongs
     * to every publication and a second copy of the sequence is a second
     * place for that check to be missing -- which is exactly what it
     * was. (§12.13.1)
     */
    const directory = path.dirname(file);
    const outcome = this.publishInto(
      directory,
      { storeId: sidecar.storeId, blockId: sidecar.blockId, prefix: storePrefix, text: storeText },
      digestOfBytes(this.files.readBytes(file))
    );
    if (!outcome.published) {
      return { done: false, file, because: 'document-open' };
    }
    this.write(file, { ...sidecar, unresolved: false });
    return { done: true, file: outcome.file };
  }

  public acknowledge(
    file: string,
    rawDigest: string,
    sentDigest: string,
    cursor: string
  ): Acknowledgement {
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return { recorded: false, because: 'no-sidecar' };
    }
    if (!newerEvent(sidecar.cursor, cursor)) {
      return { recorded: false, because: 'older-event' };
    }
    /*
     * `unresolved` IS NOT CLEARED HERE. Only `reconcile` clears it; an
     * answer arriving for some other version of this block says nothing
     * about the third version sitting in the file. (§12.11.7, C15)
     */
    /*
     * AND THE STORE HAS NOW SEEN IT. `local-only` said the baseline was
     * built here rather than taken from an answer; an answer has just
     * arrived for these bytes, so it no longer holds -- leaving it set
     * would keep a confirmed version listed as unsent for ever.
     */
    this.write(file, {
      ...sidecar,
      acknowledgedRaw: rawDigest,
      sent: sentDigest,
      cursor,
      localOnly: false
    });
    return { recorded: true };
  }
}
