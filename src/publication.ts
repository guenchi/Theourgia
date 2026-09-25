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
 * Putting a reading of a block into a file, and deciding what a file
 * found on disk is.  (section 12.15 structure one, and sections 12.11.1,
 * 12.13.2 and 12.17)
 *
 * V20 PUBLICATION REPLACES current.md. A unique durable temporary is
 * installed with one rename after the final ownership, dirty-buffer
 * and byte checks. A session-scoped kernel lock protects plugin writers.
 * The editor itself does not participate in that lock.
 *
 * The prepared sidecar retains the previous full source record. Recovery
 * selects a source only when complete body bytes identify it unambiguously.
 */

import { createHash, randomUUID } from 'crypto';
import * as path from 'path';
import { FileOps } from './fsops';
import {cleanupFailure, cleanupTemporary, replaceText, temporaryFor} from './temporary';
import { projectionFileIn, projectionNameFor, titleOfPrefix } from './projection-name';
import { Owners } from './ownership';

/*
 * ONE DIGEST FUNCTION. The sidecar's four digest fields and the draft
 * scan all have to agree about what a digest of some bytes is, and two
 * spellings of that would be two answers to one question.
 */
export function digestOfBytes(bytes: Buffer | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

/*
 * WHAT THE EXTENSION CAN ASK THE EDITOR. `publish` refuses a dirty
 * document on the target path. Clean open documents may refresh, and
 * `FileOps` cannot answer this question. It is a
 * parameter for the reason the file operations are: this does not
 * need to know whose editor it is, and saying so is what lets a cell
 * drive it. (section 12.13.1)
 */
export interface OpenDocuments {
  isOpen(file: string): boolean;
  isDirty?(file: string): boolean;
}

/*
 * The record beside current.md (or a retained legacy numbered file). The
 * field names are the design's. (sections 12.9, 12.13.2 and 12.17.3)
 */
export interface ProjectionSource {
  id: string;
  kind: 'committed' | 'working' | 'local';
  writer: string;
  version: string;
  basedOn: string | null;
  cut?: string;
}

export interface Sidecar {
  /*
   * THE RECORD SAYS WHICH SHAPE IT IS. A queue written without a version
   * field was the one shape the outbox could not tell from a corrupt
   * one, and a record on disk outlives the build that wrote it.
   * (the same lesson as the outbox)
   */
  projection?: ProjectionSource;
  prior?: Sidecar | null;
  format: 1;
  storeId: string;
  blockId: string;
  /*
   * Which of the three publication steps was last completed. (section 12.7.3)
   */
  phase: 'publishing' | 'published';
  /*
   * front + heading-src as they were written, the bytes a save is split
   * against. (section 12.1)
   */
  prefix: string;
  /*
   * The digest of the whole text this publication intended to write --
   * an identity for the publication, never a licence to discard
   * content. (section 12.13.2)
   */
  written: string;
  /*
   * The digest of the previous version's text, so that a file found
   * mid-publication can be told from a third version the editor wrote.
   * (section 12.9)
   */
  previous: string | null;
  /*
   * The digest of the disk bytes that were verified and sent, once the
   * store has answered. Draft detection compares against THIS.
   * (section 12.17.3, section 12.13.2)
   */
  acknowledgedRaw: string | null;
  /*
   * The digest of the normalised text actually sent. (section 12.7.5)
   */
  sent: string | null;
  /*
   * The store position the last answer established. (section 12.9)
   */
  cursor: string | null;
  /*
   * Set when `reconcile` established this baseline locally rather than
   * from the store; such a version is treated as a draft. (section 12.13.2)
   */
  localOnly: boolean;
  /*
   * Set when the file was found to be a third version; cleared only by
   * `reconcile`. (section 12.9, section 12.11.7, C15)
   */
  unresolved: boolean;

  /*
   * NOTE: A SEND IS OUT THAT CANNOT WRITE A RECORD. (section 13.1, D6)
   *
   * A queue entry made before section 13 carries a request and bytes and
   * nothing else: no file, no digests, no sequence number. Its answer
   * may only touch the queue and the notice -- writing a baseline from
   * it would mean inventing the provenance the entry never had. But the
   * send is REAL, and while it is out the block's versions cannot be
   * called settled either.
   *
   * So the migration marks every version of the block, and the mark is
   * what makes them drafts until the old entry reaches a final verdict.
   * The mark is written on the record rather than worked out from the
   * queue, because the queue belongs to one session and the question
   * "is this file settled" is asked by anyone holding the file.
   */
  legacySend: boolean;
  /*
   * Whether the block's own body, as the store gave it, contains CRLF.
   * The save path needs this to decide whether a CRLF file is the
   * editor's doing or the block's; the prefix cannot answer it.
   * (section 12.17.3, P2-7)
   */
  bodyHasCrlf: boolean;
  /*
   * NOTE: EVERYTHING BELOW IS section 13's RECORD OF WHICH SEND THE STORE
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
   * NOTE: WHAT A RECORD WRITTEN BEFORE section 13 STILL TELLS US.
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
  | { by: 'legacy'; seq: 0; rawDigest: string; cursor: string | null }
  /*
   * NOTE: THE BASELINE A PUBLICATION ESTABLISHES, WHICH IS NOT A SEND.
   *
   * A version published from the store holds bytes the store gave us.
   * That is a baseline -- these bytes are what the store had -- and it
   * is emphatically not a confirmation of a send: there is no request,
   * no sequence number and no sent body, because nothing was sent.
   *
   * The first version of this borrowed the `by: 'store'` shape with
   * `req: ''` and `seq: 0`. An empty request id is a sentence about a
   * request that does not exist, and this tree has been bitten by the
   * same shape before -- an absent thing drawn as the reassuring value
   * (`?? 0`, `catch { return 0 }`). Naming the fourth kind costs one
   * line and makes the type refuse to compare a `sentDigest` nobody
   * wrote. (section 13.6 (11))
   */
  | { by: 'publication'; rawDigest: string; prefixDigest: string; cursor: string | null };

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
 * wrong. (section 12.9, section 12.17.4)
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
 * merely happens to be written in. (section 12.7.4, C7, C20)
 */
export type Acknowledgement =
  | { recorded: true }
  | { recorded: false; because: 'older-event' | 'no-sidecar' | 'not-ours' };

export interface PublishRequest {
  projection?: ProjectionSource;
  directory: string;
  storeId: string;
  blockId: string;
  prefix: string;
  text: string;
  /*
   * WHERE THE STORE STOOD WHEN THESE BYTES WERE READ, as far as this
   * window knows.
   *
   * NOTE: IT IS NOT TAKEN FROM THE READ ITSELF, because the core's answer
   * to a read carries no position -- only a write's answer does. What
   * is recorded here is the position this window held for the store at
   * the moment it published, which can be behind. It is written down
   * because a baseline with no provenance at all is worse, and it is
   * described in these words so that nobody later reads it as "the
   * store was exactly here when it gave us this".
   */
  cursor: string | null;
}

export type PublishOutcome =
  | { published: true; file: string; version: number }
  | {
      published: false;
      /*
       * `not-ours` IS NOT `digest-moved`. Another window holds this
       * block's directory: nothing is wrong with the bytes, and the
       * thing to do is look at that window rather than save again.
       */
      because: 'document-open' | 'dirty-document' | 'digest-moved' | 'not-ours' | 'projection-incomplete' | 'migration-required' | 'unknown-file';
      file: string | null;
      /*
       * THE NAMES SEEN, when a block directory holds more than one projection
       * and nothing here can say which is the block's (queue item 5).
       */
      seen?: string[];
    };

/*
 * THE NAMES ON DISK ARE THE DESIGN'S NAMES, NOT THIS LANGUAGE'S.
 *
 * `<n>.md.meta` is a record that outlives the build that wrote it and may
 * be read by something that is not this extension, so its keys are the
 * ones section 12 uses -- `acknowledged-raw`, `local-only` -- rather than
 * whatever casing TypeScript is comfortable with. Inside the program
 * the fields are camelCase like everything else, and these two
 * functions are the whole of the difference.
 *
 * IT IS A MAPPING, NOT A CAST. Writing `JSON.stringify(sidecar)` would
 * put the language's spelling on disk and nothing would notice until
 * another reader tried; naming both sides here makes the file format a
 * thing that was decided. (section 12.9, and the outbox's `format` lesson)
 */
export function sidecarToDisk(sidecar: Sidecar): Record<string, unknown> {
  return {
    projection: sidecar.projection,
    prior: sidecar.prior === undefined ? undefined : sidecar.prior === null ? null : sidecarToDisk(sidecar.prior),
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
    'legacy-send': sidecar.legacySend,
    /*
     * NOTE: WRITTEN EVEN WHILE NOTHING READS THEM. A record this build
     * writes must be one this build can read back with the same meaning,
     * and a field that is only sometimes present is a second shape on
     * disk. `confirmed` is null until a settlement writes one; the
     * counters start where `confirmationFrom` says an older record
     * starts, so a file written now and a file written before section 13 are
     * read the same way.
     */
    /*
     * NOTE: A LEGACY BASELINE IS NOT WRITTEN BACK. It is what an older
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
  if (confirmed.by === 'publication') {
    return {
      'raw-digest': confirmed.rawDigest,
      'prefix-digest': confirmed.prefixDigest,
      cursor: confirmed.cursor,
      by: confirmed.by
    };
  }
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
/*
 * THE FIRST NUMBER A SEND CAN HAVE. `nextSeq` starts here, so a record
 * claiming a smaller one was never issued by this build.
 */
export const FIRST_SEQ = 1;

export const UNNUMBERED: Pick<
  Sidecar,
  'confirmed' | 'outstanding' | 'highWater' | 'nextSeq' | 'writtenBy' | 'legacySend'
> = {
  confirmed: null,
  outstanding: [],
  highWater: 0,
  nextSeq: FIRST_SEQ,
  writtenBy: null,
  /*
   * A RECORD THIS BUILD WRITES HAS NO OLD-FORMAT SEND BEHIND IT. The
   * mark is put on by the migration, which is the only thing that knows
   * an entry without a record exists.
   */
  legacySend: false
};

/*
 * Refuses a record it cannot read rather than repairing it, for the
 * reason the outbox refuses one: a record this build does not
 * understand may be the only trace of work, and a silent repair writes
 * over it. (section 12.9)
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
   * fields it does not understand. (section 12.9)
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
  const projection = record.projection;
  if (projection !== undefined && (!projection || typeof projection !== 'object' ||
      !['id','writer','version'].every(k => typeof (projection as Record<string,unknown>)[k] === 'string') ||
      !['committed','working','local'].includes(String((projection as Record<string,unknown>).kind)) ||
      !((projection as Record<string,unknown>).basedOn === null || typeof (projection as Record<string,unknown>).basedOn === 'string') ||
      !((projection as Record<string,unknown>).cut === undefined || typeof (projection as Record<string,unknown>).cut === 'string'))) {
    return {read:false,because:'unreadable',detail:'invalid projection source'};
  }
  let prior: Sidecar | null | undefined;
  if (record.prior !== undefined) {
    if (record.prior === null) prior = null;
    else {
      if (typeof record.prior !== 'object' || 'prior' in record.prior) return {read:false,because:'unreadable',detail:'nested preparation'};
      const parsed = sidecarFromDisk(JSON.stringify(record.prior));
      if (!parsed.read || parsed.sidecar.phase !== 'published') return {read:false,because:'unreadable',detail:'invalid prior projection'};
      prior = parsed.sidecar;
    }
  }
  const confirmed = confirmationFrom(record);
  if (confirmed === null) {
    return {
      read: false,
      because: 'unreadable',
      detail:
        'the sidecar holds an outstanding send this build cannot read, so whether anything is ' +
        'in flight over this file is not known'
    };
  }
  return {
    read: true,
    sidecar: {
      ...(projection===undefined?{}:{projection:projection as ProjectionSource}),
      ...(prior===undefined?{}:{prior}),
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
      legacySend: record['legacy-send'] === true,
      ...confirmed
    }
  };
}

/*
 * NOTE: WHAT A RECORD WRITTEN BEFORE section 13 MEANS, WRITTEN DOWN RATHER THAN
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
/*
 * NULL MEANS THE RECORD COULD NOT BE READ, and the caller turns that into
 * the refusal this file already speaks -- `{read: false, because:
 * 'unreadable'}` -- rather than into a sidecar with parts missing.
 */
function confirmationFrom(
  record: Record<string, unknown>
): Pick<Sidecar, 'confirmed' | 'outstanding' | 'highWater' | 'nextSeq' | 'writtenBy'> | null {
  const held = record.confirmed;
  /*
   * NOTE: `null` HERE DOES NOT MEAN "THIS BUILD SAYS THERE IS NO
   * BASELINE", and reading it that way costs a block its baseline.
   *
   * A derived baseline is deliberately NOT written back -- what goes to
   * disk is `confirmed: null` beside the fields it was derived FROM --
   * so the next read has to derive it again. Treating the explicit
   * `null` as a statement would mean that the first time anything
   * rewrote an old record, the block it belongs to became a draft. That
   * is the defect this whole migration exists to avoid, arriving
   * through the door built to avoid it.
   *
   * What separates "no baseline" from "one that has to be derived" is
   * not the key: it is whether the record carries the fields a
   * derivation needs, and `local-only` -- the older build's own word
   * for "this baseline came from the file". `legacyBaseline` asks both.
   */
  const confirmed =
    typeof held === 'object' && held !== null && !Array.isArray(held)
      ? readConfirmed(held as Record<string, unknown>)
      : legacyBaseline(record);
  /*
   * NEVER: AND THE LIST ITSELF IS A RECORD THAT CAN BE THERE AND NOT
   * READ.
   *
   * A thirteenth review round repaired the ENTRIES -- an item that will
   * not parse refuses the whole sidecar, four lines below -- and left
   * the container: anything that is not an array read as an empty list,
   * which `cleanliness` takes for "no send is in flight over this
   * file". Measured in a sixteenth round with `outstanding: {seq: 1,
   * req: "R"}`: the sidecar parsed, the file was called clean, and the
   * send it names was invisible. The repair and the hole were four
   * lines apart, because the repair was made where the finding pointed.
   *
   * A missing key is still an empty list: an older record that never
   * had one is not an unreadable record.
   */
  if (record.outstanding !== undefined && !Array.isArray(record.outstanding)) {
    return null;
  }
  const out = Array.isArray(record.outstanding) ? record.outstanding : [];
  const outstanding: Outstanding[] = [];
  for (const item of out) {
    /*
     * NEVER: A RECORD THAT IS THERE AND CANNOT BE READ IS NOT A RECORD
     * THAT IS NOT THERE.
     *
     * Dropping it made an unreadable sidecar parse with
     * `outstanding: []`, and an empty outstanding list is what
     * `cleanliness` reads as "no send is in flight over this file" --
     * so a file with unsent work in it was reported clean. Measured in
     * a thirteenth review round with `outstanding: [{seq: "bad", req:
     * "R1"}]`.
     */
    const entry =
      typeof item === 'object' && item !== null && !Array.isArray(item)
        ? (item as Record<string, unknown>)
        : null;
    if (entry === null || typeof entry.seq !== 'number' || typeof entry.req !== 'string') {
      return null;
    }
    outstanding.push({ seq: entry.seq, req: entry.req });
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
  if (held.by === 'publication') {
    const raw = text('raw-digest');
    const prefix = text('prefix-digest');
    if (raw === null || prefix === null) {
      return null;
    }
    return { by: 'publication', rawDigest: raw, prefixDigest: prefix, cursor: text('cursor') };
  }
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
  const cursor = record.cursor;
  const raw = record['acknowledged-raw'];
  if (typeof raw === 'string') {
    return { by: 'legacy', seq: 0, rawDigest: raw, cursor: typeof cursor === 'string' ? cursor : null };
  }
  /*
   * NOTE: AND A VERSION THE OLDER BUILD PUBLISHED AND NOBODY SAVED FROM
   * STILL HAS A BASELINE: `written`. (section 13.6, ruled after the trace
   * below.)
   *
   * My first reading made these drafts, on the grounds that nothing had
   * been acknowledged. That is a sentence about saves and the question
   * is about bytes: a published version holds exactly what the store
   * gave, and calling it a draft tells the user they have unsent work
   * when they have none -- a lie in the direction that costs them a
   * search.
   *
   * NOTE: IT IS ONLY TRUE WHERE `written` REALLY HOLDS THE STORE'S BYTES,
   * AND THAT IS A TRACE, NOT A BELIEF. Every place this build assigns
   * `written`:
   *
   *   - `publishInto`, from `publish`: the text came from the store
   *     (`openBlock` reads the block and publishes it).
   *   - `publishInto`, from `reconcileBy`'s `take-store-version`: the
   *     store's text.
   *   - `publishInto`, from `reconcileBy`'s `prepend-prefix`: the
   *     USER'S bytes with the store's heading in front. NOT the
   *     store's.
   *   - `reconcile`'s `prefix-already-present` path: the FILE's bytes.
   *     NOT the store's.
   *
   * The last two are exactly the two that set `local-only`, which is
   * the older build's own word for "this baseline came from the file".
   * So the derivation is allowed only where that word is absent -- and
   * it is the record's word, not an inference about it.
   */
  /*
   * NOTE: AND IT ONLY APPLIES TO A RECORD FROM BEFORE section 13. Every record
   * this build writes carries `next-seq`; a record without it was
   * written by the older one. `confirmed: null` cannot be the signal --
   * this build writes exactly that for a version it knows has no
   * baseline, and reading it as "work it out from `written`" would give
   * a baseline to the user's own bytes.
   *
   * NOTE: UNLIKE THE LEGACY ONE, THIS BASELINE IS WRITTEN BACK. It invents
   * nothing: every field comes from the record (`written`, `prefix`,
   * `cursor`), so recording it states what was already there. The
   * legacy baseline is not written back because it HAS no request, sent
   * digest or split to record, and writing one would be inventing them.
   * That is also what makes this trigger safe to lose: after the first
   * rewrite the baseline is recorded and nothing needs to be derived
   * again.
   */
  if (record['next-seq'] !== undefined) {
    return null;
  }
  if (record['local-only'] === true) {
    return null;
  }
  const written = record.written;
  const prefix = record.prefix;
  if (typeof written !== 'string' || typeof prefix !== 'string') {
    return null;
  }
  return {
    by: 'publication',
    rawDigest: written,
    prefixDigest: digestOfBytes(prefix),
    cursor: typeof cursor === 'string' ? cursor : null
  };
}

function readWrittenBy(wrote: Record<string, unknown>): WrittenBy | null {
  if (typeof wrote.sid !== 'string' || typeof wrote.generation !== 'number') {
    return null;
  }
  return { sid: wrote.sid, generation: wrote.generation };
}

/*
 * The only way out of `unresolved`. (section 12.11.7, C3, C15)
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
      because?: 'not-ours' | 'dirty-document' | 'working-unavailable';
      /*
       * Both actions run on the chain. `prepend-prefix` keeps the user's
       * bytes and puts the store's prefix in front of them;
       * `take-store-version` publishes a new version from the store and
       * leaves the old file alone -- it is never deleted, because this
       * extension deletes nothing. (section 12.11.7, section 12.23)
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
 * the cursor backwards. (section 12.7.4)
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
 * NOTE: IT IS A FUNCTION RATHER THAN A METHOD BECAUSE IT HAS TWO CALLERS.
 * `saving.ts` wrote the record with a bare `writeText` -- the exact
 * truncation this paragraph forbids -- while the comment explaining why
 * that is unsafe sat in this file, on the other implementation. A rule
 * stated in one place and enforced in one of two is a rule half the code
 * does not have.
 */
export function writeSidecar(files: FileOps, file: string, sidecar: Sidecar): void {
  replaceText(files, sidecarPathOf(file), `${JSON.stringify(sidecarToDisk(sidecar), null, 2)}\n`);
}

/*
 * IS THIS FILE HOLDING WORK THE STORE HAS NOT GOT?
 *
 * NOTE: A FUNCTION OF WHAT IS ON DISK, AND OF NOTHING ELSE.
 *
 * Four persisted inputs: the file's bytes, its record, who owns the
 * block, and what the owner's queue still holds for it. Nothing in
 * memory is an input -- not the Saver, not a map of pending saves, not
 * whether a drain happens to be running. That is the whole point: the
 * answer this returns has to be the same for a window that has just
 * started as for one that has been running all day, because the user is
 * asking about their file, not about our process.
 *
 * NOTE: THE QUEUE IS AN INPUT, AND IT IS NOT THE SAME AS THE RECORD. The
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
  | {
      clean: false;
      because: 'never-confirmed' | 'bytes-moved' | 'prefix-moved' | 'still-out' | 'legacy-send-out';
    }
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
  /*
   * NOTE: AN OLD-FORMAT SEND IS OUT, AND IT HAS ITS OWN WORD. (D6)
   *
   * It could be folded into `still-out` -- both mean "something is in
   * flight" -- and then the one thing the user can be told about it
   * would be lost: this send cannot write a baseline whatever it comes
   * back as, so the block will need saving again even if it succeeds.
   * `still-out` reads as "wait", and this one reads as "save it again
   * once the answer is in". Different sentences, different words.
   */
  if (sidecar.legacySend) {
    return { clean: false, because: 'legacy-send-out' };
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
   * NOTE: A LEGACY BASELINE IS NOT ASKED A QUESTION IT CANNOT ANSWER. The
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

/*
 * WHETHER A CONFIRMATION BECOMES THE BASELINE. (section 13.3)
 *
 * NOTE: THE AXIS IS `highWater`, NOT `confirmed.seq`, and that is the whole
 * reason this is a named function rather than a comparison written at
 * the one place that needed it. `highWater` only ever rises and
 * `reconcile` does not touch it; `confirmed.seq` disappears when
 * `reconcile` removes a baseline, and a late answer for an older send
 * would then read `4 > 0` and put the removed baseline back -- reading
 * as clean a file somebody deliberately took the baseline off.
 *
 * NOTE: A DERIVED BASELINE HAS NO SEND NUMBER OF ITS OWN. A legacy record
 * and a publication both sit at `highWater` 0, so the first real send
 * replaces them -- and that is a consequence of how they are written,
 * not a rule this function may assume. It compares the one axis; the
 * cells state the consequence for each kind of baseline, including the
 * combination that must NOT replace: a high-water mark above the
 * arriving send.
 */
export function replacesBaseline(highWater: number, arriving: number): boolean {
  if (!Number.isInteger(arriving) || arriving < FIRST_SEQ) {
    return false;
  }
  return arriving > highWater;
}

export class Publisher {
  /*
   * `files` and `documents` are both handed in: this does not need to
   * know whose file system or whose editor it is, and that is what lets
   * a cell count what it did. (section 12.20, C2/C10)
   */
  private readonly files: FileOps;
  private readonly documents: OpenDocuments;
  /*
   * WHO THIS PUBLISHER IS, WHEN IT IS SOMEBODY. (section 13, r3-3)
   *
   * Every path that writes a record beside a block passes one rule: the
   * block directory is owned by this session. The rule lives in
   * `ownership.ts`; what each path DOES when it fails differs, so the
   * check is made at each of them and the reaction belongs to the path.
   *
   * NOTE: ABSENT MEANS NO CHECK, AND THAT IS A DOOR LEFT OPEN ON PURPOSE:
   * the cells about publication are about publication, and making every
   * one of them build a session and take ownership would be measuring
   * the fixture. What keeps the shipping path honest is the census in
   * `ownership.test.ts` over `new Publisher(` in `src`.
   */
  private readonly ownership?: { owners: Owners; sessionId: string };

  constructor(
    files: FileOps,
    documents: OpenDocuments,
    ownership?: { owners: Owners; sessionId: string }
  ) {
    this.files = files;
    this.documents = documents;
    this.ownership = ownership;
  }

  /*
   * NOTE: THE RULE IS ONE FUNCTION AND THE REACTIONS ARE MANY. Whether
   * this session may write beside a file is asked here; what to do when
   * it may not is the caller's, because "the save is not queued", "the
   * reconciliation did not happen" and "the record was not written" are
   * three different sentences to a user.
   */
  private mayWrite(file: string): { may: true } | { may: false; because: string } {
    if (this.ownership === undefined) {
      return { may: true };
    }
    const asked = this.ownership.owners.mayWrite(path.dirname(file), this.ownership.sessionId);
    return asked.may ? { may: true } : { may: false, because: asked.because };
  }

  /*
   * TAKE THE BLOCK DIRECTORY IF NOBODY HOLDS IT. A publication creates
   * the directory, so there is nothing to own until it does; every
   * later write asks `mayWrite` instead.
   */
  private takeIfUnowned(directory: string): { may: true } | { may: false; because: string } {
    if (this.ownership === undefined) {
      return { may: true };
    }
    const asked = this.ownership.owners.mayWrite(directory, this.ownership.sessionId);
    if (asked.may) {
      return { may: true };
    }
    if (asked.because !== 'unowned') {
      return { may: false, because: asked.because };
    }
    const held = this.ownership.owners.take(directory, this.ownership.sessionId, []);
    return held.held ? { may: true } : { may: false, because: held.because };
  }

  private metaOf(file: string): string {
    return sidecarPathOf(file);
  }

  /*
   * THE DIRECTORY'S PROJECTION, found rather than spelled (queue item 5). One
   * `.md` with a sidecar, whatever it is called -- `current.md` in a directory
   * made before names carried titles. None, or more than one, is null: this
   * reader has nothing it can call the block's.
   */
  public latestIn(directory: string): string | null {
    const found = projectionFileIn(this.files, directory);
    return found.found === 'one' ? found.file : null;
  }

  private write(file: string, sidecar: Sidecar): void {
    writeSidecar(this.files, file, sidecar);
  }

  public recoverCurrent(file: string, supplied?: {source:ProjectionSource;text:string}): boolean {
    return withExclusive(path.dirname(file), (): boolean => {
    const record = this.sidecarOf(file);
    if (!record || !record.projection) return false;
    if (record.phase === 'published') return true;
    if (this.documents.isDirty?.(file) || !this.mayWrite(file).may) return false;
    if (!this.files.exists(file)) {
      if (record.prior!==null || !supplied || supplied.source.id!==record.projection.id || digestOfBytes(supplied.text)!==record.written) return false;
      replaceText(this.files,file,supplied.text);
    }
    const digest = digestOfBytes(this.files.readBytes(file));
    const old = record.prior;
    const isOld = old != null && digest === old.written;
    const isNew = digest === record.written;
    // Equal bytes cannot select between two different origins after a crash.
    if (isOld && isNew && old?.projection?.id !== record.projection.id) return false;
    if (isOld) { this.write(file, old as Sidecar); return true; }
    if (isNew) { const {prior: _prior, ...next} = record; this.write(file, {...next, phase:'published'}); return true; }
    return false;

    });
  }

  public async publish(request: PublishRequest): Promise<PublishOutcome> {
    return this.publishNow(request);
  }

  public publishNow(request: PublishRequest): PublishOutcome {
    return this.publishInto(request.directory, request, {cursor:request.cursor}, false);
  }

  private publishInto(directory: string, what: Omit<PublishRequest,'directory'|'cursor'>,
      baseline: {cursor:string|null} | null, explicit: boolean, expectedRaw?:string): PublishOutcome {
    return withExclusive(directory, (): PublishOutcome => {
    /*
     * THE FILE IS THE ONE ALREADY HERE, OR A NEW NAME. (queue item 5) A block
     * published before keeps the name it was given, `current.md` included;
     * a first publication names the file after the title it has now, and that
     * name then stays. Two projections in one directory are not chosen
     * between: the publication is refused, naming both.
     */
    const found = projectionFileIn(this.files, directory);
    if (found.found === 'unknown') {
      return {published:false,because:'unknown-file',file:null,seen:found.names};
    }
    const file = found.found === 'one'
      ? found.file
      : path.join(directory, projectionNameFor(titleOfPrefix(what.prefix), what.blockId));
    const refuse = (because: Extract<PublishOutcome,{published:false}>['because']): PublishOutcome => ({published:false,because,file});
    if (this.documents.isDirty?.(file)) return refuse('dirty-document');
    if (this.files.list(directory).some(n => /^\d+\.md(?:\.meta)?$/.test(n))) return refuse('migration-required');
    let old = this.sidecarOf(file);
    if (this.files.exists(file) && (!old || !old.projection)) return refuse('unknown-file');
    if (old && (old.storeId !== what.storeId || old.blockId !== what.blockId)) return refuse('unknown-file');
    if (old?.phase === 'publishing' && !explicit) {
      if (!this.recoverCurrent(file)) return refuse('projection-incomplete');
      old = this.sidecarOf(file);
    }
    let before: string | null = null;
    if (this.files.exists(file)) before = digestOfBytes(this.files.readBytes(file));
    if (expectedRaw!==undefined && before!==expectedRaw) return refuse('digest-moved');
    if (explicit && old?.phase==='publishing' && before!==null && old.projection) {
      old={...old,phase:'published',written:before,prior:undefined,confirmed:null,localOnly:true,
        projection:{...old.projection,id:randomUUID(),kind:'local'}};
    }
    if (old && !explicit && (before !== old.written || old.localOnly)) return refuse('digest-moved');
    this.files.makeDirectory(directory);
    if (!this.takeIfUnowned(directory).may) return refuse('not-ours');
    const projection = what.projection ?? {id:randomUUID(),kind:baseline===null?'local':'committed',writer:this.ownership?.sessionId ?? 'local',version:randomUUID(),basedOn:null};
    const record: Sidecar = {
      ...UNNUMBERED,
      nextSeq:old?.nextSeq ?? 1, highWater:old?.highWater ?? 0, outstanding:old?.outstanding ?? [],
      writtenBy:old?.writtenBy ?? null, legacySend:old?.legacySend ?? false,
      format:1, storeId:what.storeId, blockId:what.blockId,
      phase:'publishing', projection, prior:old ?? null,
      prefix:what.prefix, written:digestOfBytes(what.text), previous:before,
      confirmed:baseline===null ? null : {by:'publication',rawDigest:digestOfBytes(what.text),prefixDigest:digestOfBytes(what.prefix),cursor:baseline.cursor},
      acknowledgedRaw:null,sent:null,cursor:null,localOnly:baseline===null&&projection.kind!=='working',unresolved:false,
      bodyHasCrlf:what.text.slice(what.prefix.length).includes('\r\n')
    };
    const temporary = temporaryFor(this.files,file);
    let promoted=false;
    try {
      this.files.writeDurably(temporary,what.text);
      this.write(file,record);
      // No asynchronous preparation follows this final editor/owner/byte check.
      const now = this.files.exists(file) ? digestOfBytes(this.files.readBytes(file)) : null;
      if (this.documents.isDirty?.(file) || now !== before || !this.mayWrite(file).may) {
        const reason = this.documents.isDirty?.(file) ? 'dirty-document' : now !== before ? 'digest-moved' : 'not-ours';
        throw Object.assign(new Error(reason), {publicationRefusal:reason});
      }
      this.files.rename(temporary,file);
      promoted=true;
      this.files.syncDirectory(directory);
      this.files.syncDirectory(path.dirname(temporary));
      const {prior: _prior,...stable} = record;
      this.write(file,{...stable,phase:'published'});
      return {published:true,file,version:1};
    } catch (error) {
      if (promoted) throw error;
      const observation=cleanupTemporary(this.files,temporary,error);
      if (typeof error==='object' && error!==null && 'publicationRefusal' in error) {
        // The prepared record is retained for explicit recovery if ownership changed.
        if (old && this.mayWrite(file).may) this.write(file,old);
        if (observation.presence==='present' || observation.presence==='unknown') throw cleanupFailure(observation);
        return refuse((error as {publicationRefusal:Extract<PublishOutcome,{published:false}>['because']}).publicationRefusal);
      }
      throw cleanupFailure(observation);
    }

    });
  }

  public standingOf(file: string, queue?: QueueView): Standing {
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
    const bytes = this.files.readBytes(file);
    const digest = digestOfBytes(bytes);
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
       * copy -- so they are never overwritten. (section 12.9)
       */
      return { kind: 'third-version' };
    }
    if (sidecar.unresolved) {
      return { kind: 'third-version' };
    }
    /*
     * A DRAFT IS BYTES THE STORE HAS NOT ACKNOWLEDGED, and a baseline
     * `reconcile` built locally is always one: the store has never seen
     * it. (section 12.13.2, section 12.19.2)
     */
    /*
     * A FRESHLY PUBLISHED VERSION IS NOT A DRAFT. It holds exactly the
     * bytes the store gave, which is what `written` records -- there is
     * no work pending in it. Judging only against `acknowledged-raw`
     * made every version a draft from the moment it was written, before
     * the user had touched it. (section 12.19.2's own rule names both digests.)
     *
     * NOTE: BUT `written` STOPS BEING THE BASELINE THE MOMENT THE STORE
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
    /*
     * NOTE: ONE RULE, ONE PLACE. What a draft is used to be decided here
     * AND in the listing that scans a session, in two spellings -- and
     * they disagreed: the listing called every freshly published
     * version a draft because it compared against `acknowledged-raw`
     * alone. section 13 makes it a pure function of four persistent inputs and
     * this is now the only caller-facing way in.
     */
    const verdict = cleanliness(bytes, sidecar, queue ?? { unsettled: [] });
    return { kind: 'published', draft: !verdict.clean };
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
   * half-changed. (section 12.7.4, C7, C20)
   *
   * NEWER IS DEFINED, NOT ASSUMED. A late replay must not move the
   * cursor backwards: for the SAME writer the sequence numbers are
   * compared numerically; a DIFFERENT writer is a later generation and
   * is accepted. Anything else is `older-event`. (section 12.7.4)
   */
  /*
   * Offers the way out, without taking it. (section 12.11.7, C3)
   */
  public reconcile(file: string, storePrefix: string, storeText: string): Reconciliation {
    return withExclusive(path.dirname(file), (): Reconciliation => {
    const fileText = this.files.readText(file);
    if (this.documents.isDirty?.(file)) {
      return {reconciled:false, because:'dirty-document', choices:[], storeText, previousText:null, fileText};
    }
    if (fileText.startsWith(storePrefix)) {
      const ours = this.mayWrite(file);
      if (!ours.may) {
        return { reconciled: false, because: 'not-ours', choices: [], storeText, previousText: null, fileText };
      }
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
         * from the file rather than from an answer. (section 12.11.7)
         */
        /*
         * NOTE: AND THE BASELINE GOES WITH IT. `local-only` says this
         * version's baseline came from the FILE rather than from the
         * store; a `confirmed` left over from the publication would say
         * the opposite, in the field that now decides whether the block
         * is a draft. The two would contradict each other and the
         * cheerful one would win.
         *
         * Found by switching the draft rule onto the pure function: a
         * reconciled version whose digests happened to agree read as
         * settled, and the cell that has said otherwise since C6 caught
         * it. The cell was right; this is where the defect was.
         */
        this.write(file, {
          ...sidecar,
          prefix: storePrefix,
          projection: sidecar.projection ? {...sidecar.projection,id:randomUUID(),kind:'local'} : undefined,
          written: digestOfBytes(Buffer.from(fileText, 'utf8')),
          phase: 'published',
          confirmed: null,
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

    });
  }

  /*
   * Takes one of the two offered actions. Clearing `unresolved` happens
   * here and nowhere else: a build that cleared it on startup would
   * re-arm the overwrite the third-version judgement exists to prevent,
   * and would pass every other cell. (section 12.11.7, C15)
   */
  public reconciliationGuard(file: string, offered: string): string | null {
    if (this.documents.isDirty?.(file)) return 'dirty-document';
    if (!this.mayWrite(file).may) return 'not-ours';
    if (!this.files.exists(file) || this.files.readText(file)!==offered) return 'file-changed';
    return null;
  }

  public reconcileBy(file: string, action: 'prepend-prefix' | 'take-store-version', storePrefix: string,
      storeText: string, offered?: string, projection?: ProjectionSource): {done:boolean;file:string;because?:string} {
    return withExclusive(path.dirname(file), (): {done:boolean;file:string;because?:string} => {
    if (this.documents.isDirty?.(file)) return {done:false,file,because:'dirty-document'};
    const sidecar=this.sidecarOf(file);
    if (!sidecar) return {done:false,file,because:'no-record'};
    if (!this.mayWrite(file).may) return {done:false,file,because:'not-ours'};
    const current=this.files.exists(file)?this.files.readText(file):null;
    if (current===null || offered!==undefined && current!==offered) return {done:false,file,because:'file-changed'};
    const text=action==='take-store-version'?storeText:current.startsWith(storePrefix)?current:storePrefix+current;
    const result=this.publishInto(path.dirname(file),{storeId:sidecar.storeId,blockId:sidecar.blockId,prefix:storePrefix,text,projection},
      action==='take-store-version'?{cursor:null}:null,true,digestOfBytes(current));
    return result.published?{done:true,file:result.file}:{done:false,file,because:result.because};

    });
  }

  public acceptsSnapshot(file:string,rawDigest:string,sidecar:Sidecar):boolean {
    const actual=this.sidecarOf(file);
    return !!actual && actual.phase==='published' && actual.storeId===sidecar.storeId && actual.blockId===sidecar.blockId &&
      actual.projection?.id===sidecar.projection?.id && actual.prefix===sidecar.prefix &&
      !this.documents.isDirty?.(file) && this.files.exists(file) && digestOfBytes(this.files.readBytes(file))===rawDigest;
  }

  public recordWorking(file: string, projection: ProjectionSource, rawDigest: string, priorId?: string): boolean {
    return withExclusive(path.dirname(file), (): boolean => {
    if (!this.mayWrite(file).may) return false;
    const held=this.sidecarOf(file);
    if (!held || held.phase!=='published' || held.projection?.id!==priorId ||
        !this.files.exists(file) || digestOfBytes(this.files.readBytes(file))!==rawDigest) return false;
    this.write(file,{...held,projection,written:rawDigest,confirmed:null,acknowledgedRaw:null,localOnly:false});
    return true;

    });
  }

  public takeSequence(
    file: string,
    req: string
  ):
    | { taken: true; seq: number }
    | { taken: false; because: 'no-record' | 'could-not-write' | 'not-ours'; detail?: string } {
    return withExclusive(path.dirname(file), (): | { taken: true; seq: number }
    | { taken: false; because: 'no-record' | 'could-not-write' | 'not-ours'; detail?: string } => {
    const ours = this.mayWrite(file);
    if (!ours.may) {
      return { taken: false, because: 'not-ours', detail: ours.because };
    }
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return { taken: false, because: 'no-record' };
    }
    const seq = sidecar.nextSeq;
    try {
      this.write(file, {
        ...sidecar,
        nextSeq: seq + 1,
        outstanding: [...sidecar.outstanding, { seq, req }]
      });
    } catch (e) {
      return { taken: false, because: 'could-not-write', detail: String(e) };
    }
    return { taken: true, seq };

    });
  }

  /*
   * MARK EVERY VERSION OF A BLOCK AS HAVING AN OLD-FORMAT SEND OUT, and
   * say which records were marked. (D6)
   *
   * NOTE: EVERY VERSION, NOT THE NEWEST. The entry names a block and
   * carries no file; the answer, when it comes, says nothing about
   * which version the bytes went from. Marking only the newest would
   * leave the others reading as settled while a send nobody can
   * attribute is still out.
   *
   * NOTE: AND IT RETURNS THE SET. Removing the mark later is the entry's
   * job, and an entry that had to re-derive which records it marked
   * would be a second supplier of that fact -- the shape this batch
   * deleted `recovered()` for. The caller puts this list in the entry.
   */
  public markLegacySend(directory: string): string[] {
    return withExclusive(directory, (): string[] => {
    const marked: string[] = [];
    for (const name of this.files.list(directory).filter((n) => /^\d+\.md$/.test(n))) {
      const file = path.join(directory, name);
      const ours = this.mayWrite(file);
      if (!ours.may) {
        continue;
      }
      const sidecar = this.sidecarOf(file);
      if (sidecar === null) {
        continue;
      }
      this.write(file, { ...sidecar, legacySend: true });
      marked.push(file);
    }
    return marked;

    });
  }

  /*
   * TAKE THE MARK OFF ONE RECORD. Which records, and when, is the
   * settling side's decision -- only a final verdict clears it, and
   * `unknown` and `req-mismatch` are not final. This is the act, not
   * the policy.
   */
  public clearLegacySend(file: string): boolean {
    return withExclusive(path.dirname(file), (): boolean => {
    const ours = this.mayWrite(file);
    if (!ours.may) {
      return false;
    }
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return false;
    }
    this.write(file, { ...sidecar, legacySend: false });
    return true;

    });
  }

  public acknowledge(
    file: string,
    rawDigest: string,
    sentDigest: string,
    cursor: string
  ): Acknowledgement {
    return withExclusive(path.dirname(file), (): Acknowledgement => {
    const ours = this.mayWrite(file);
    if (!ours.may) {
      return { recorded: false, because: 'not-ours' };
    }
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
     * about the third version sitting in the file. (section 12.11.7, C15)
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

    });
  }
}
