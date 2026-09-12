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
 * parameter for the reason `placeReading` takes one. (§12.13.1)
 */
export interface OpenDocuments {
  isOpen(file: string): boolean;
}

/*
 * The record beside `<n>.md`, written as `<n>.meta`. The field names are
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
 * `<n>.meta` is a record that outlives the build that wrote it and may
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
    'body-has-crlf': sidecar.bodyHasCrlf
  };
}

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
      bodyHasCrlf: record['body-has-crlf'] === true
    }
  };
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

export class Publisher {
  /*
   * `files` is handed in for the reason `placeReading` is handed an
   * editor: this does not need to know whose file system it is, and
   * that is what lets a cell count what it did. (§12.20, C2/C10)
   */
  private readonly files: FileOps;
  private readonly documents: OpenDocuments;

  constructor(files: FileOps, documents: OpenDocuments) {
    this.files = files;
    this.documents = documents;
  }

  private metaOf(file: string): string {
    return `${file}.meta`;
  }

  private nextVersion(directory: string): number {
    let n = 1;
    while (this.files.exists(path.join(directory, `${n}.md`)) || this.files.exists(path.join(directory, `${n}.md.meta`))) {
      n += 1;
    }
    return n;
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
   * THE RECORD IS REPLACED, NEVER TRUNCATED IN PLACE.
   *
   * `writeText` opens for writing, which empties the file first: a
   * process that stops there leaves a record nothing can read -- and an
   * unreadable record made `standingOf` answer `absent` and `draftsIn`
   * skip the file, so surviving work became invisible. A temporary file
   * renamed into place is either the old record or the new one.
   *
   * THE BYTES ARE ON THE DEVICE BEFORE THE RENAME, and the directory
   * entry after it, for the reason the outbox does the same: a rename
   * decides what a reader sees and says nothing about what survives a
   * machine losing power.
   */
  private write(file: string, sidecar: Sidecar): void {
    const meta = this.metaOf(file);
    const temporary = `${meta}.${process.pid}.tmp`;
    this.files.writeDurably(temporary, `${JSON.stringify(sidecarToDisk(sidecar), null, 2)}\n`);
    this.files.rename(temporary, meta);
    this.files.syncDirectory(path.dirname(meta));
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
   * Reads `<n>.meta` and `<n>.md` and says what the file is. The three
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
     * A `local-only` VERSION IS ALWAYS ONE, whatever its digests say:
     * its baseline came from the file rather than from an answer.
     */
    const draft =
      sidecar.localOnly || (digest !== sidecar.acknowledgedRaw && digest !== sidecar.written);
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
    storeText: string
  ): { done: boolean; file: string } {
    const sidecar = this.sidecarOf(file);
    if (sidecar === null) {
      return { done: false, file };
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
      const fileText = this.files.readText(file);
      const joined = fileText.startsWith(storePrefix) ? fileText : `${storePrefix}${fileText}`;
      const outcome = this.publishInto(
        path.dirname(file),
        { storeId: sidecar.storeId, blockId: sidecar.blockId, prefix: storePrefix, text: joined },
        digestOfBytes(this.files.readBytes(file))
      );
      if (!outcome.published) {
        return { done: false, file };
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
      return { done: false, file };
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
