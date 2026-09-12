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
 * C2, C3 and C15: publication is immutable, and every point it can die
 * at is decidable afterwards.
 *
 * THE WHOLE CLASS OF "CHECKED, THEN SOMETHING WROTE" IS GONE BY
 * CONSTRUCTION, not by checking harder: a reading from the store goes to
 * a NEW path, so there is nothing to overwrite. These cells hold the
 * build to that with the file-operation recorder -- a claim about which
 * lines of source call `writeFileSync` would be a claim about the lines
 * somebody grepped for. (§12.15 结构一, C2)
 *
 * AND THE RECORD IS WRITTEN FIRST, IN THREE STEPS. `publishing` with the
 * target digest and the previous one, then the file, then `published`.
 * Given any of those three corpses, what the file is can be worked out:
 * digest == written ⇒ the write finished, finish the record; == previous
 * ⇒ the write never happened; neither ⇒ the editor wrote a third version
 * and nothing may be overwritten. (§12.7.3 as it stands in §12.9, C3)
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
  OpenDocuments,
  Publisher,
  sidecarFromDisk,
  sidecarToDisk,
  Sidecar,
  writeSidecar,
  UNNUMBERED,
  cleanliness,
  digestOfBytes
} from '../../src/publication';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-pub-'));
}

function nothingOpen(): OpenDocuments {
  return { isOpen: () => false };
}

function openOn(file: string): OpenDocuments {
  return { isOpen: (f) => path.resolve(f) === path.resolve(file) };
}

function request(directory: string, text: string, prefix = '## Two\n') {
  return { directory, storeId: 's1', blockId: 'a.2', prefix, text };
}

/*
 * D1 WHAT A RECORD WRITTEN BEFORE §13 MEANS TO THIS BUILD.
 *
 * ⚠️ THE FIRST VERSION OF THIS RULE MADE EVERY OLD BLOCK A DRAFT.
 *
 * §13 keeps "which send the store confirmed" in a `confirmed` record
 * that older sidecars do not have. I read their absence as "no baseline
 * at all", reasoning that deriving one would invent a request id and a
 * prefix digest nobody wrote down. The inventing part was right; the
 * conclusion was not. `acknowledged-raw` IS a fact the old build
 * recorded, and discarding it turns every block that was ever saved into
 * a draft the moment this build runs -- the same failure I had just
 * written a rule to prevent for `outstanding`, one field along. The main
 * session caught it.
 *
 * So an old record yields a baseline that says only what it said: these
 * bytes were acknowledged, the store stood here. No request id, because
 * none was recorded -- and the type says so rather than leaving a field
 * empty for a reader to take as one.
 */
describe('D1 a record from before the send-record still says what it knew', () => {
  const older = {
    format: 1,
    'store-id': '/tmp/store',
    'block-id': 'a.2',
    phase: 'published',
    prefix: '## Two\n',
    written: 'digest-of-written',
    previous: null,
    'acknowledged-raw': digestOfBytes(Buffer.from('## Two\nbody\n', 'utf8')),
    sent: 'digest-of-sent',
    cursor: 'w:4',
    'local-only': false,
    unresolved: false,
    'body-has-crlf': false
  };

  it('reads an acknowledged older record as a baseline, not as none', () => {
    const read = sidecarFromDisk(JSON.stringify(older));
    assert.ok(read.read, JSON.stringify(read));
    const confirmed = (read as { sidecar: Sidecar }).sidecar.confirmed;
    assert.ok(confirmed !== null, 'an older record that WAS acknowledged was read as having no baseline');
    assert.strictEqual(confirmed?.by, 'legacy');
    assert.strictEqual(confirmed?.seq, 0, 'the first confirmation this build writes must replace it');
    assert.strictEqual(confirmed?.rawDigest, older['acknowledged-raw']);
    assert.strictEqual(confirmed?.cursor, 'w:4');
  });

  it('calls an unchanged older block clean', () => {
    const read = sidecarFromDisk(JSON.stringify(older));
    assert.ok(read.read);
    const sidecar = (read as { sidecar: Sidecar }).sidecar;
    assert.deepStrictEqual(
      cleanliness(Buffer.from('## Two\nbody\n', 'utf8'), sidecar, { unsettled: [] }),
      { clean: true },
      'a block nobody has edited since it was saved became a draft on upgrade'
    );
  });

  it('calls an older block that has been edited a draft', () => {
    const read = sidecarFromDisk(JSON.stringify(older));
    assert.ok(read.read);
    const sidecar = (read as { sidecar: Sidecar }).sidecar;
    assert.deepStrictEqual(
      cleanliness(Buffer.from('## Two\nedited since\n', 'utf8'), sidecar, { unsettled: [] }),
      { clean: false, because: 'bytes-moved' }
    );
  });

  /*
   * AND AN OLDER RECORD THAT WAS NEVER ACKNOWLEDGED HAS NO BASELINE --
   * which is what it had before this build too. Without this row, a
   * build that manufactured a baseline out of `written` would pass the
   * rows above.
   */
  it('gives an older record that was never acknowledged no baseline at all', () => {
    const read = sidecarFromDisk(JSON.stringify({ ...older, 'acknowledged-raw': null }));
    assert.ok(read.read);
    const sidecar = (read as { sidecar: Sidecar }).sidecar;
    assert.strictEqual(sidecar.confirmed, null);
    assert.deepStrictEqual(
      cleanliness(Buffer.from('## Two\nbody\n', 'utf8'), sidecar, { unsettled: [] }),
      { clean: false, because: 'never-confirmed' }
    );
  });

  /*
   * ⚠️ AND THE DERIVED BASELINE IS NEVER WRITTEN BACK. If it were, the
   * next read would find a `confirmed` without a request id, refuse it,
   * and answer none -- turning the block into a draft by the very route
   * this baseline exists to close. The fields it is derived FROM are
   * written, so the derivation happens again.
   */
  it('does not write the derived baseline into the record', () => {
    const read = sidecarFromDisk(JSON.stringify(older));
    assert.ok(read.read);
    const written = sidecarToDisk((read as { sidecar: Sidecar }).sidecar);
    assert.strictEqual(written.confirmed, null, 'a derived baseline was written as a record');
    assert.strictEqual(written['acknowledged-raw'], older['acknowledged-raw']);
    assert.strictEqual(written.cursor, 'w:4');
    /*
     * AND READING WHAT WE WROTE GIVES THE SAME ANSWER. A record this
     * build writes must mean to it what the record it read meant.
     */
    const again = sidecarFromDisk(`${JSON.stringify(written)}\n`);
    assert.ok(again.read);
    assert.strictEqual((again as { sidecar: Sidecar }).sidecar.confirmed?.by, 'legacy');
  });
});

describe('C2 a published file is written once and never touched again', () => {
  it('writes each version to its own path', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());

    const first = await publisher.publish(request(dir, '## Two\none\n'));
    const second = await publisher.publish(request(dir, '## Two\ntwo\n'));
    assert.ok(first.published && second.published);
    if (first.published && second.published) {
      assert.notStrictEqual(first.file, second.file, 'the second reading replaced the first file');
      assert.strictEqual(second.version, first.version + 1, 'versions do not advance by one');
    }
  });

  /*
   * THE ASSERTION C2 ACTUALLY MAKES: at most one write per path, by this
   * process, across any sequence. An implementation that rewrote a file
   * in place would still leave a consistent final state -- this is what
   * tells them apart.
   */
  /*
   * THE RULE IS ABOUT THE BLOCK FILES. `<n>.md` is written once and
   * never moved; its record `<n>.meta` is REPLACED three times, and is
   * replaced by writing a temporary file and renaming it -- which is
   * what stops a stopped process from leaving a record nothing can
   * read. Saying "nothing is ever renamed" would forbid the very thing
   * that makes the record recoverable, so the cells name the paths the
   * rule covers. (§12.23, §12.7.3)
   */
  function blockFiles(files: RecordingFs): string[] {
    return Array.from(new Set(files.touched('writeText').concat(files.touched('writeDurably')))).filter(
      (f) => f.endsWith('.md')
    );
  }

  it('never writes a block file it has written before', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());
    for (const text of ['## Two\none\n', '## Two\ntwo\n', '## Two\nthree\n']) {
      await publisher.publish(request(dir, text));
    }
    const written = blockFiles(files);
    assert.strictEqual(written.length, 3, `three readings produced ${written.length} files`);
    for (const file of written) {
      const writes = files.countOf('writeText', file) + files.countOf('writeDurably', file);
      assert.strictEqual(
        writes,
        1,
        `${file} was written ${writes} times; publication is supposed to be immutable`
      );
    }
  });

  it('never unlinks or renames a block file', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());
    for (const text of ['## Two\none\n', '## Two\ntwo\n']) {
      await publisher.publish(request(dir, text));
    }
    assert.deepStrictEqual(files.touched('unlink'), [], 'something was unlinked');
    for (const moved of files.touched('rename')) {
      assert.ok(
        !moved.endsWith('.md'),
        `${moved} was renamed; only a record’s temporary file may be`
      );
    }
  });

  /*
   * AND THE RECORD IS REPLACED, NOT EMPTIED. A process stopped inside an
   * in-place rewrite leaves a record nothing can read, and an unreadable
   * record made this file count as absent and vanish from the draft
   * listing -- surviving work made invisible by its own bookkeeping.
   */
  it('replaces the record through a temporary file rather than truncating it', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    await new Publisher(files, nothingOpen()).publish(request(dir, '## Two\none\n'));
    const meta = files.touched('rename').filter((f) => f.endsWith('.meta'));
    assert.ok(meta.length > 0, 'the record was written in place, so a stopped process leaves it unreadable');
    assert.strictEqual(
      files.countOf('writeText', meta[0]),
      0,
      'the record path was opened for writing directly, which empties it first'
    );
  });

  /*
   * AND IT DOES NOT WRITE AT ALL WHEN THE EDITOR HAS THE FILE. Then the
   * editor is a writer and this is not; the open command only shows what
   * is there and offers a refresh, which publishes a NEW version.
   * (§12.13.1)
   */
  /*
   * THE REFUSAL IS ABOUT THE PATH BEING WRITTEN, NOT ABOUT THE BLOCK.
   *
   * The first version of this cell opened a document on version 1 and
   * expected version 2 to be refused, which is a misreading: refreshing
   * a block the user has open is the ORDINARY path and publishes the
   * next version (§12.15 结构一). What must never happen is writing a
   * path the editor holds -- so the document here is open on the path
   * the publication is about to use.
   */
  it('writes nothing when a document is open on the path it would write', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const first = await new Publisher(files, nothingOpen()).publish(request(dir, '## Two\none\n'));
    assert.ok(first.published);

    const next = path.join(dir, '2.md');
    const held = new RecordingFs();
    const second = await new Publisher(held, openOn(next)).publish(request(dir, '## Two\ntwo\n'));
    assert.deepStrictEqual(second, { published: false, because: 'document-open', file: next });
    assert.deepStrictEqual(held.touched('writeText'), [], 'a file was written while the editor had it open');
  });

  /*
   * AND REFRESHING A BLOCK THE USER HAS OPEN IS ALLOWED, which is the
   * twin that stops the refusal above from being read as "never publish
   * while anything is open" -- that would make the refresh command
   * impossible.
   */
  it('publishes the next version while a document is open on the previous one', async () => {
    const dir = scratch();
    const first = await new Publisher(new RecordingFs(), nothingOpen()).publish(request(dir, '## Two\none\n'));
    assert.ok(first.published);
    const second = await new Publisher(
      new RecordingFs(),
      openOn(first.published ? first.file : '')
    ).publish(request(dir, '## Two\ntwo\n'));
    assert.ok(second.published, 'a refresh was refused because the old version was on screen');
  });
});

describe('C3 every point a publication can die at is decidable', () => {
  /*
   * The three corpses are built by hand rather than by killing a
   * process, because what is being tested is the JUDGEMENT, not the
   * dying: given these bytes and this record, what is this file. The
   * crash cells that produce them for real are in the two-process
   * suite.
   */
  function corpse(dir: string, sidecar: Partial<Sidecar>, fileText: string | null): string {
    const file = path.join(dir, '1.md');
    const full: Sidecar = {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'publishing',
      prefix: '## Two\n',
      written: 'unset',
      previous: null,
      acknowledgedRaw: null,
      sent: null,
      cursor: null,
      localOnly: false,
      unresolved: false,
      bodyHasCrlf: false,
      ...sidecar
    };
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(`${file}.meta`, JSON.stringify(sidecarToDisk(full)), 'utf8');
    if (fileText !== null) {
      fs.writeFileSync(file, fileText, 'utf8');
    }
    return file;
  }

  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  it('says the write never happened when the file is still the previous version', () => {
    const dir = scratch();
    const old = '## Two\nold\n';
    const file = corpse(dir, { written: digestOf('## Two\nnew\n'), previous: digestOf(old) }, old);
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(file), {
      kind: 'mid-publication',
      complete: false
    });
  });

  it('says the write finished when the file is the target', () => {
    const dir = scratch();
    const next = '## Two\nnew\n';
    const file = corpse(dir, { written: digestOf(next), previous: digestOf('## Two\nold\n') }, next);
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(file), {
      kind: 'mid-publication',
      complete: true
    });
  });

  /*
   * THE THIRD VERSION IS THE ONE THAT MUST NOT BE OVERWRITTEN. Neither
   * digest matches, which means the editor wrote while the publication
   * was in flight -- the bytes there are the user's and are the only
   * copy.
   */
  it('says a third version is a third version', () => {
    const dir = scratch();
    const file = corpse(
      dir,
      { written: digestOf('## Two\nnew\n'), previous: digestOf('## Two\nold\n') },
      '## Two\nwhat the user typed\n'
    );
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(file), {
      kind: 'third-version'
    });
  });

  it('says a finished publication is published, and whether it holds a draft', () => {
    const dir = scratch();
    const text = '## Two\nbody\n';
    const settled = corpse(dir, { phase: 'published', written: digestOf(text), acknowledgedRaw: digestOf(text) }, text);
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(settled), {
      kind: 'published',
      draft: false
    });

    const other = scratch();
    const edited = corpse(
      other,
      { phase: 'published', written: digestOf(text), acknowledgedRaw: digestOf(text) },
      '## Two\nedited since\n'
    );
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(edited), {
      kind: 'published',
      draft: true
    });
  });

  it('says absent when there is no file', () => {
    const dir = scratch();
    const file = corpse(dir, { phase: 'published' }, null);
    assert.deepStrictEqual(new Publisher(new RecordingFs(), nothingOpen()).standingOf(file), {
      kind: 'absent'
    });
  });
});

describe('C15 the record on disk uses the design names and says which shape it is', () => {
  it('writes the hyphenated names the design uses, not this language’s', () => {
    const sidecar: Sidecar = {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'published',
      prefix: '## Two\n',
      written: 'w',
      previous: null,
      acknowledgedRaw: 'r',
      sent: 's',
      cursor: 'w:7',
      localOnly: true,
      unresolved: false,
      bodyHasCrlf: false
    };
    const onDisk = sidecarToDisk(sidecar);
    assert.ok('acknowledged-raw' in onDisk, 'the record does not use the name the design gave it');
    assert.ok('local-only' in onDisk, 'the record does not use the name the design gave it');
    assert.ok(!('acknowledgedRaw' in onDisk), 'this language’s spelling reached the file format');
    assert.strictEqual(onDisk.format, 1, 'the record does not say which shape it is');
  });

  it('reads back exactly what it wrote', () => {
    const sidecar: Sidecar = {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'publishing',
      prefix: '',
      written: 'w',
      previous: 'p',
      acknowledgedRaw: null,
      sent: null,
      cursor: null,
      localOnly: false,
      unresolved: true,
      bodyHasCrlf: false
    };
    const read = sidecarFromDisk(JSON.stringify(sidecarToDisk(sidecar)));
    assert.deepStrictEqual(read, { read: true, sidecar });
  });

  it('refuses a record from a later build rather than reading it as this one', () => {
    const later = { format: 2, 'block-id': 'a.2' };
    const read = sidecarFromDisk(JSON.stringify(later));
    assert.strictEqual(read.read, false);
    assert.strictEqual(read.read === false && read.because, 'later-format');
  });

  it('refuses bytes it cannot read rather than repairing them', () => {
    const read = sidecarFromDisk('this is not json');
    assert.strictEqual(read.read, false);
    assert.strictEqual(read.read === false && read.because, 'unreadable');
  });

  /*
   * AND `unresolved` SURVIVES EVERYTHING BUT `reconcile`. C15 names the
   * implementation this kills: one that clears the flag at startup,
   * which would silently re-arm the very overwrite the third-version
   * judgement exists to prevent.
   */
  it('keeps unresolved across a read and a write of the record', () => {
    const sidecar: Sidecar = {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'published',
      prefix: '## Two\n',
      written: 'w',
      previous: null,
      acknowledgedRaw: 'r',
      sent: 's',
      cursor: 'w:7',
      localOnly: false,
      unresolved: true,
      bodyHasCrlf: false
    };
    const round = sidecarFromDisk(JSON.stringify(sidecarToDisk(sidecar)));
    assert.ok(round.read);
    assert.strictEqual(round.read && round.sidecar.unresolved, true, 'unresolved did not survive a round trip');
  });
});

/*
 * C3: the way out of a third version, and the two shapes it takes.
 *
 * `reconcile` IS THE ONLY EXIT. A file judged `third-version` is never
 * overwritten and never saved from, which without an exit would mean
 * the user's bytes are stranded for ever. (§12.11.7)
 */
describe('C3 reconcile is the only way out of a third version', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  function stranded(dir: string, fileText: string): string {
    const file = path.join(dir, '1.md');
    fs.mkdirSync(dir, { recursive: true });
    const sidecar: Sidecar = {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'publishing',
      prefix: '## Two\n',
      written: digestOf('## Two\nthe store version\n'),
      previous: digestOf('## Two\nolder\n'),
      acknowledgedRaw: null,
      sent: null,
      cursor: null,
      localOnly: false,
      unresolved: true,
      bodyHasCrlf: false
    };
    fs.writeFileSync(`${file}.meta`, JSON.stringify(sidecarToDisk(sidecar)), 'utf8');
    fs.writeFileSync(file, fileText, 'utf8');
    return file;
  }

  /*
   * THE CASE THAT NEEDS NO QUESTION. When the file already begins with
   * the store's prefix, the baseline is simply what is there, and the
   * body is work the store has not got -- a draft, which can be saved.
   */
  it('establishes the baseline without asking when the prefix is already there', () => {
    const file = stranded(scratch(), '## Two\nwhat the user typed\n');
    const offer = new Publisher(new RecordingFs(), nothingOpen()).reconcile(
      file,
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.deepStrictEqual(offer, { reconciled: true, because: 'prefix-already-present' });
  });

  it('offers both actions, with all three texts, when it cannot tell', () => {
    const file = stranded(scratch(), 'no heading at all\n');
    const offer = new Publisher(new RecordingFs(), nothingOpen()).reconcile(
      file,
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.strictEqual(offer.reconciled, false);
    if (!offer.reconciled) {
      assert.deepStrictEqual(offer.choices.slice().sort(), ['prepend-prefix', 'take-store-version']);
      assert.strictEqual(offer.fileText, 'no heading at all\n', 'the user’s bytes are not among the three');
      assert.strictEqual(offer.storeText, '## Two\nthe store version\n');
    }
  });

  it('keeps the user’s bytes when they choose to put the prefix in front of them', () => {
    const file = stranded(scratch(), 'no heading at all\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nthe store version\n');
    assert.ok(done.done);
    assert.ok(
      fs.readFileSync(done.file, 'utf8').includes('no heading at all'),
      'choosing to keep the local bytes discarded them'
    );
  });

  /*
   * AND THE OTHER CHOICE DOES NOT DELETE ANYTHING. Taking the store's
   * version publishes a NEW version; the file the user had stays where
   * it is, because this extension deletes nothing. (§12.23)
   */
  it('publishes a new version when they choose the store’s, and leaves the old file alone', () => {
    const dir = scratch();
    const file = stranded(dir, 'no heading at all\n');
    const files = new RecordingFs();
    const done = new Publisher(files, nothingOpen()).reconcileBy(
      file,
      'take-store-version',
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.ok(done.done);
    assert.notStrictEqual(done.file, file, 'the store’s version was written over the user’s');
    assert.strictEqual(
      fs.readFileSync(file, 'utf8'),
      'no heading at all\n',
      'the file the user had was changed'
    );
    assert.deepStrictEqual(files.touched('unlink'), [], 'reconciling deleted something');
  });

  it('clears unresolved only by reconciling, and the file can then be saved from', () => {
    const file = stranded(scratch(), '## Two\nwhat the user typed\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    /*
     * READ THE FLAG BEFORE RECONCILING. The first version of this cell
     * called `reconcile` first and then checked that the file was still
     * stranded -- but the prefix-already-present case reconciles as it
     * answers, so the check was asking about a state the cell had just
     * left.
     */
    const before = publisher.sidecarOf(file);
    assert.strictEqual(before?.unresolved, true, 'the record did not say it was stranded to begin with');
    publisher.reconcile(file, '## Two\n', '## Two\nthe store version\n');
    publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nthe store version\n');
    assert.strictEqual(
      publisher.sidecarOf(file)?.unresolved,
      false,
      'reconciling left the file stranded'
    );
  });
});

/*
 * C15: `unresolved` is a fact about the file, not a mood of the process.
 *
 * THE IMPLEMENTATION THIS KILLS is one that clears the flag when it
 * starts up, or when the block is opened, or after any save is
 * answered. Each of those passes the round-trip cell already there,
 * because each writes the flag faithfully -- right up until the moment
 * it decides to reset it.
 */
describe('C15 unresolved survives everything except reconcile', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  function strandedFile(dir: string): string {
    const file = path.join(dir, '1.md');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('## Two\nstore\n'),
          previous: null,
          acknowledgedRaw: digestOf('## Two\nstore\n'),
          sent: null,
          cursor: 'w:7',
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    fs.writeFileSync(file, '## Two\na third version\n', 'utf8');
    return file;
  }

  it('is still set after a fresh reader opens the record', () => {
    const file = strandedFile(scratch());
    assert.strictEqual(
      new Publisher(new RecordingFs(), nothingOpen()).sidecarOf(file)?.unresolved,
      true,
      'reading the record cleared the flag'
    );
  });

  it('is still set after an answer is recorded against the file', () => {
    const file = strandedFile(scratch());
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    publisher.acknowledge(file, 'raw', 'sent', 'w:9');
    assert.strictEqual(
      publisher.sidecarOf(file)?.unresolved,
      true,
      'recording an answer cleared a flag only reconcile may clear'
    );
  });

  it('is still set for a reader that has never seen this file before', () => {
    const file = strandedFile(scratch());
    new Publisher(new RecordingFs(), nothingOpen()).sidecarOf(file);
    const restarted = new Publisher(new RecordingFs(), nothingOpen());
    assert.strictEqual(
      restarted.sidecarOf(file)?.unresolved,
      true,
      'a restart cleared the flag, which re-arms the overwrite it exists to prevent'
    );
  });
});

/*
 * P2-6 and P2-7, both found by review: a second publication path that
 * skipped a check, and a question answered from the wrong evidence.
 */
describe('every publication path asks the same questions', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  /*
   * THE STORE-VERSION BRANCH OF `reconcileBy` PUBLISHES TOO, and for one
   * round it wrote without asking whether the editor had the target
   * open -- the same requirement, missing from the second of two copies
   * of the sequence. Both now go through one door.
   */
  it('refuses to take the store’s version onto a path the editor has open', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, 'no heading at all\n', 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('no heading at all\n'),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    const files = new RecordingFs();
    const done = new Publisher(files, openOn(path.join(dir, '2.md'))).reconcileBy(
      file,
      'take-store-version',
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.strictEqual(done.done, false, 'a version was published onto a path the editor had open');
    assert.deepStrictEqual(
      files.touched('writeText').filter((f) => f.endsWith('.md')),
      [],
      'a block file was written while the editor had it open'
    );
  });

  /*
   * WHETHER THE BODY USES CRLF IS A FACT ABOUT THE BLOCK, recorded when
   * it is published. Asking the PREFIX instead normalised a block whose
   * stored body really did contain CRLF, on every save, even when the
   * user had changed nothing.
   */
  it('records whether the block’s own body uses CRLF', async () => {
    const dir = scratch();
    const published = await new Publisher(new RecordingFs(), nothingOpen()).publish({
      directory: dir,
      storeId: 's1',
      blockId: 'a.2',
      prefix: '## Two\n',
      text: '## Two\nline one\r\nline two\r\n'
    });
    assert.ok(published.published);
    if (published.published) {
      assert.strictEqual(
        new Publisher(new RecordingFs(), nothingOpen()).sidecarOf(published.file)?.bodyHasCrlf,
        true,
        'the record does not say the block’s body uses CRLF, so a save will normalise it away'
      );
    }
  });

  it('records a body with no carriage returns as having none', async () => {
    const dir = scratch();
    const published = await new Publisher(new RecordingFs(), nothingOpen()).publish({
      directory: dir,
      storeId: 's1',
      blockId: 'a.2',
      prefix: '## Two\r\n',
      text: '## Two\r\nplain body\n'
    });
    assert.ok(published.published);
    if (published.published) {
      assert.strictEqual(
        new Publisher(new RecordingFs(), nothingOpen()).sidecarOf(published.file)?.bodyHasCrlf,
        false,
        'a CRLF heading was taken as evidence about the body'
      );
    }
  });
});

/*
 * P1-3: the draft's only copy is never written over.
 *
 * ⚠️ THIS FIX ALSO HAD NO CELL UNTIL A MUTATION SURVIVED. Putting the
 * truncating rewrite back left every existing cell green -- they checked
 * that the user's bytes were still THERE, which they are right up until
 * the process stops halfway through replacing them.
 */
describe('reconciling never writes over the file it is reconciling', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  function strandedAt(dir: string, fileText: string): string {
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, fileText, 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf(fileText),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    return file;
  }

  it('puts the prefix in front of the user’s bytes in a NEW version', () => {
    const dir = scratch();
    const file = strandedAt(dir, 'no heading at all\n');
    const files = new RecordingFs();
    const done = new Publisher(files, nothingOpen()).reconcileBy(
      file,
      'prepend-prefix',
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.ok(done.done);
    assert.notStrictEqual(done.file, file, 'the user’s only copy was written over');
    assert.strictEqual(
      files.countOf('writeText', file) + files.countOf('writeDurably', file),
      0,
      'the file holding the draft was opened for writing, which empties it first'
    );
    assert.strictEqual(fs.readFileSync(file, 'utf8'), 'no heading at all\n', 'the draft was changed');
    assert.strictEqual(
      fs.readFileSync(done.file, 'utf8'),
      '## Two\nno heading at all\n',
      'the new version does not carry the prefix and the user’s bytes'
    );
  });
});

/*
 * X1c ⑧: THE PARTS OF RECONCILIATION AND OF THE RECORD THAT NO CELL WAS
 * WATCHING.
 *
 * Each of these was a mutation that survived a whole green suite. They
 * are collected here rather than folded into the cells above because
 * what they have in common is the shape, not the subject: every one of
 * them is a value that is COMPUTED and then never looked at, so the
 * build that drops it and the build that keeps it produce the same
 * answers to every question the suite was asking.
 */
describe('X1c ⑧ what reconciliation computes and what the record keeps', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  /*
   * TWO VERSIONS, THE SECOND STRANDED. The third text the user is shown
   * is the version published BEFORE the one they are looking at, so a
   * fixture with one version cannot tell whether it is produced: `null`
   * is the right answer there, and it is also the answer a build that
   * never looks gives.
   */
  function strandedSecond(dir: string): string {
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, '1.md'), '## Two\nthe version before\n', 'utf8');
    const file = path.join(dir, '2.md');
    fs.writeFileSync(file, 'no heading at all\n', 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('no heading at all\n'),
          previous: digestOf('## Two\nthe version before\n'),
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    return file;
  }

  it('shows the version published before this one as the third text', () => {
    const file = strandedSecond(scratch());
    const offer = new Publisher(new RecordingFs(), nothingOpen()).reconcile(
      file,
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.strictEqual(offer.reconciled, false);
    if (!offer.reconciled) {
      /*
       * ALL THREE ARE DIFFERENT FROM EACH OTHER. A build that filled the
       * third slot with either of the other two would satisfy "it is not
       * null", and the user would be choosing with one of their options
       * shown twice.
       */
      assert.strictEqual(offer.previousText, '## Two\nthe version before\n');
      assert.notStrictEqual(offer.previousText, offer.fileText);
      assert.notStrictEqual(offer.previousText, offer.storeText);
    }
  });

  it('says there is no third text when this is the first version', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, 'no heading at all\n', 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('no heading at all\n'),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    const offer = new Publisher(new RecordingFs(), nothingOpen()).reconcile(
      file,
      '## Two\n',
      '## Two\nthe store version\n'
    );
    assert.strictEqual(offer.reconciled, false);
    if (!offer.reconciled) {
      assert.strictEqual(offer.previousText, null, 'a text was offered that no version holds');
    }
  });

  /*
   * `local-only` IS WHAT MAKES A RECONCILED BASELINE A DRAFT. The
   * baseline came from the file rather than from an answer, so the store
   * has never seen those bytes; without the flag the digests agree with
   * each other and the version reads as saved. A mutation that dropped
   * it left every cell green -- and left the user's work reported as
   * already in the store.
   */
  it('marks a baseline it took from the file as one the store has never seen', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nwhat the user typed\n', 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('## Two\nwhat the user typed\n'),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    assert.deepStrictEqual(publisher.reconcile(file, '## Two\n', '## Two\nthe store version\n'), {
      reconciled: true,
      because: 'prefix-already-present'
    });
    const sidecar = publisher.sidecarOf(file);
    assert.ok(sidecar !== null);
    assert.strictEqual(sidecar?.localOnly, true, 'work the store has never seen was recorded as sent');
    const standing = publisher.standingOf(file);
    assert.strictEqual(
      standing.kind === 'published' && standing.draft,
      true,
      'the reconciled version was not reported as holding a draft'
    );
  });

  it('marks the version it makes from the user’s bytes the same way', () => {
    const dir = scratch();
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, 'no heading at all\n', 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('no heading at all\n'),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nstored\n');
    assert.ok(done.done);
    assert.strictEqual(
      publisher.sidecarOf(done.file)?.localOnly,
      true,
      'bytes only this window has were recorded as bytes the store has'
    );
  });

  /*
   * THE DIRECTORY ENTRY IS FLUSHED AFTER THE RENAME.
   *
   * A rename decides what a reader sees and says nothing about what
   * survives a machine losing power: the bytes are durable before the
   * rename, and the NAME is durable only once the directory itself is
   * flushed. Dropping that call changes no answer any other cell asks,
   * which is why the mutation survived -- the loss is only visible after
   * a power cut, and by then nothing is left to ask.
   */
  it('flushes the directory after the rename that publishes a record', () => {
    const dir = scratch();
    const files = new RecordingFs();
    const file = path.join(dir, '1.md');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    writeSidecar(files, file, {
    ...UNNUMBERED,
    format: 1,
      storeId: 's1',
      blockId: 'a.2',
      phase: 'published',
      prefix: '## Two\n',
      written: digestOf('## Two\nbody\n'),
      previous: null,
      acknowledgedRaw: null,
      sent: null,
      cursor: null,
      localOnly: false,
      unresolved: false,
      bodyHasCrlf: false
    });
    const renameAt = files.entries.findIndex(
      (e) => e.op === 'rename' && e.file === `${file}.meta`
    );
    const flushAt = files.entries.findIndex(
      (e) => e.op === 'syncDirectory' && path.resolve(e.file) === path.resolve(dir)
    );
    assert.ok(renameAt >= 0, 'the record was not published by a rename');
    assert.ok(flushAt >= 0, 'the directory holding the record was never flushed');
    /*
     * AND IN THAT ORDER. Flushing the directory before the rename
     * flushes an entry that does not name the new record yet, which is
     * the same as not flushing at all.
     */
    assert.ok(flushAt > renameAt, 'the directory was flushed before the name it had to make durable');
  });
});

/*
 * X1c ⑨: THE DRAFT THAT GOT THERE BY UNDOING.
 *
 * A version is published, the user edits the body and saves, the store
 * takes it. Then they type the ORIGINAL text back in. The file now holds
 * bytes the store does not have -- an unsent edit -- but it matches
 * `written`, which was being accepted as a baseline for ever alongside
 * `acknowledged-raw`. The block reported nothing pending, which is the
 * worst way to be wrong about a draft: silence, arrived at by undoing.
 */
describe('X1c ⑨ what a version is measured against after the store has answered', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  function versionAt(dir: string, text: string, over: Partial<Sidecar>): string {
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, text, 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf('## Two\nas published\n'),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: false,
          bodyHasCrlf: false,
          ...over
        })
      ),
      'utf8'
    );
    return file;
  }

  const publisher = (): Publisher => new Publisher(new RecordingFs(), nothingOpen());

  function draftOf(file: string): boolean {
    const standing = publisher().standingOf(file);
    assert.strictEqual(standing.kind, 'published', `the file is ${standing.kind}, not published`);
    return standing.kind === 'published' && standing.draft;
  }

  it('reports work the store has not got when the file is put back to the published text', () => {
    /*
     * THE STORE HOLDS THE EDITED BODY -- that is what `acknowledged-raw`
     * records -- and the file holds the text it was published with.
     * Those are different bytes, so there is something to send.
     */
    const file = versionAt(scratch(), '## Two\nas published\n', {
      acknowledgedRaw: digestOf('## Two\nthe edit that was sent\n'),
      sent: digestOf('the edit that was sent\n'),
      cursor: 'w:3'
    });
    assert.strictEqual(
      draftOf(file),
      true,
      'the file was put back to its published text and the block reported nothing pending'
    );
  });

  /*
   * THE TWO TWINS THAT STOP THAT FROM BEING SATISFIED BY "EVERYTHING IS
   * A DRAFT". A version nobody has saved yet is measured against
   * `written`; one whose bytes the store has acknowledged is not a draft
   * at all.
   */
  it('does not call a version a draft before anything has been saved from it', () => {
    const file = versionAt(scratch(), '## Two\nas published\n', {});
    assert.strictEqual(draftOf(file), false, 'a freshly published version was reported as unsent work');
  });

  it('does not call a version a draft when its bytes are the ones the store took', () => {
    const text = '## Two\nthe edit that was sent\n';
    const file = versionAt(scratch(), text, {
      acknowledgedRaw: digestOf(text),
      sent: digestOf('the edit that was sent\n'),
      cursor: 'w:3'
    });
    assert.strictEqual(draftOf(file), false, 'the bytes the store acknowledged were reported as unsent');
  });
});

/*
 * REVIEW ROUND 20: THE CHOICE IS CARRIED OUT AGAINST THE TEXT THAT WAS
 * OFFERED, OR NOT AT ALL.
 *
 * `reconcile` shows three texts and the user picks. The pick waits on a
 * human, and the file is not this window's alone: another window can
 * replace it while the list is open. `prepend-prefix` then re-read the
 * file, published somebody else's bytes with the heading in front, and
 * the confirmation said the user's own text had been kept -- bytes they
 * were never shown, under a sentence saying the opposite.
 *
 * THE CELL IS HERE AND NOT IN THE EDITOR SUITE because the part that
 * shows the list is the one part no cell can reach; the rule was moved
 * into `reconcileBy` so that this could be asked at all.
 */
describe('review 20 a reconciliation is measured against the text it offered', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  function stranded(dir: string, fileText: string): string {
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, fileText, 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf(fileText),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    return file;
  }

  it('does nothing when the file changed between the offer and the choice', () => {
    const dir = scratch();
    const file = stranded(dir, 'alpha\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const offer = publisher.reconcile(file, '## Two\n', '## Two\nstored\n');
    assert.strictEqual(offer.reconciled, false);
    const offered = offer.reconciled ? '' : offer.fileText;
    assert.strictEqual(offered, 'alpha\n');

    /*
     * ANOTHER WINDOW WRITES. This is the whole sequence: nothing about
     * it is exotic, and nothing in the chain can prevent it, because the
     * writer is not in this process.
     */
    fs.writeFileSync(file, 'beta\n', 'utf8');

    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nstored\n', offered);
    assert.deepStrictEqual(
      done,
      { done: false, file, because: 'file-changed' },
      'a choice made about one text was carried out against another'
    );
    assert.strictEqual(
      fs.existsSync(path.join(dir, '2.md')),
      false,
      'a version was published carrying bytes the user was never shown'
    );
    assert.strictEqual(fs.readFileSync(file, 'utf8'), 'beta\n', 'the other window’s bytes were touched');
  });

  it('does nothing when the file changed before the store’s version is taken either', () => {
    const dir = scratch();
    const file = stranded(dir, 'alpha\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const offer = publisher.reconcile(file, '## Two\n', '## Two\nstored\n');
    const offered = offer.reconciled ? '' : offer.fileText;
    fs.writeFileSync(file, 'beta\n', 'utf8');
    const done = publisher.reconcileBy(
      file,
      'take-store-version',
      '## Two\n',
      '## Two\nstored\n',
      offered
    );
    /*
     * THE OTHER ACTION TOO. It does not re-read the file, so it would
     * have published the right text -- but the user chose between three
     * texts and one of them is no longer there, so the choice they made
     * is not the choice they would make now.
     */
    assert.strictEqual(done.done, false, 'the store’s version was taken against a stale offer');
    assert.strictEqual(done.because, 'file-changed');
  });

  /*
   * THE GREEN TWIN. Without it every cell above is satisfied by a build
   * that refuses every reconciliation, which would leave the only way
   * out of a third version permanently shut.
   */
  it('carries the choice out when the file is still what was offered', () => {
    const dir = scratch();
    const file = stranded(dir, 'alpha\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const offer = publisher.reconcile(file, '## Two\n', '## Two\nstored\n');
    const offered = offer.reconciled ? '' : offer.fileText;
    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nstored\n', offered);
    assert.strictEqual(done.done, true, `the ordinary path was refused: ${JSON.stringify(done)}`);
    assert.strictEqual(fs.readFileSync(done.file, 'utf8'), '## Two\nalpha\n');
  });

  /*
   * AND A CALLER THAT OFFERED NOTHING IS NOT FORCED TO INVENT A VALUE.
   * `offered` is optional; the guarantee is made by passing it.
   */
  it('acts without the check when no offer was made', () => {
    const dir = scratch();
    const file = stranded(dir, 'alpha\n');
    const done = new Publisher(new RecordingFs(), nothingOpen()).reconcileBy(
      file,
      'prepend-prefix',
      '## Two\n',
      '## Two\nstored\n'
    );
    assert.strictEqual(done.done, true, JSON.stringify(done));
  });
});

/*
 * REVIEW ROUND 21: THE OFFER IS NOT THE BASELINE, AND ONE READ IS NOT
 * TWO.
 *
 * Both of these were mutations that survived the cells above. They are
 * about the same confusion from two sides: what the action is measured
 * against, and when it is measured.
 */
describe('review 21 what the reconciliation action is measured against', () => {
  const digestOf = (text: string): string =>
    require('crypto').createHash('sha256').update(text, 'utf8').digest('hex');

  /*
   * THE FILE AND THE BASELINE ARE DIFFERENT TEXTS HERE. Every earlier
   * fixture set `written` to the digest of the file, so a build that
   * compared the file with `written` instead of with the offer passed
   * all of them -- and publishes text the user never saw whenever the
   * two differ, which is exactly what a stranded third version IS.
   */
  function strandedOver(dir: string, fileText: string, baseline: string): string {
    fs.mkdirSync(dir, { recursive: true });
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, fileText, 'utf8');
    fs.writeFileSync(
      `${file}.meta`,
      JSON.stringify(
        sidecarToDisk({
    ...UNNUMBERED,
    format: 1,
          storeId: 's1',
          blockId: 'a.2',
          phase: 'published',
          prefix: '## Two\n',
          written: digestOf(baseline),
          previous: null,
          acknowledgedRaw: null,
          sent: null,
          cursor: null,
          localOnly: false,
          unresolved: true,
          bodyHasCrlf: false
        })
      ),
      'utf8'
    );
    return file;
  }

  it('refuses when the file differs from the offer even though it matches the record', () => {
    const dir = scratch();
    /*
     * THE OFFER WAS TAKEN WHEN THE FILE HELD `alpha`; by the time the
     * user chose, somebody had put the file back to the text the RECORD
     * describes. A build measuring against the record sees no change and
     * publishes `beta`, which the user was never shown.
     */
    const file = strandedOver(dir, 'alpha\n', 'beta\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    const offer = publisher.reconcile(file, '## Two\n', '## Two\nstored\n');
    const offered = offer.reconciled ? '' : offer.fileText;
    assert.strictEqual(offered, 'alpha\n');
    fs.writeFileSync(file, 'beta\n', 'utf8');
    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nstored\n', offered);
    assert.strictEqual(
      done.because,
      'file-changed',
      'the action was measured against the record rather than against the offer'
    );
    assert.strictEqual(fs.existsSync(path.join(dir, '2.md')), false);
  });

  /*
   * ⚠️ AND THE CHECK AND THE USE ARE ONE READ. The guard read the file,
   * then the action read it again; a write landing between them passed
   * the guard and was published -- the defect the guard exists for, one
   * step further along. This cannot be staged from outside the call, so
   * the write is injected into the FileOps between the two reads.
   */
  it('publishes the text it checked, even if the file changes between the two reads', () => {
    const dir = scratch();
    const file = strandedOver(dir, 'alpha\n', 'alpha\n');
    let reads = 0;
    const racing = new (class extends RecordingFs {
      public readText(target: string): string {
        const text = super.readText(target);
        if (target === file) {
          reads += 1;
          /*
           * AFTER THE FIRST READ OF THE FILE, somebody else writes. A
           * build that reads again gets `beta`; one that kept what it
           * checked gets `alpha`.
           */
          fs.writeFileSync(file, 'beta\n', 'utf8');
        }
        return text;
      }
    })();
    const done = new Publisher(racing, nothingOpen()).reconcileBy(
      file,
      'prepend-prefix',
      '## Two\n',
      '## Two\nstored\n',
      'alpha\n'
    );
    assert.ok(reads >= 1, 'the file was never read, so nothing was raced');
    assert.strictEqual(done.done, true, `the ordinary path was refused: ${JSON.stringify(done)}`);
    assert.strictEqual(
      fs.readFileSync(done.file, 'utf8'),
      '## Two\nalpha\n',
      'the version carries bytes that arrived after the check'
    );
  });

  /*
   * AND A FILE THAT WENT AWAY IS A REFUSAL, NOT A THROW. The caller has
   * a sentence for "look again" and none for an exception; an ENOENT
   * escaping from here leaves the command with no answer at all.
   */
  it('refuses rather than throwing when the file is gone', () => {
    const dir = scratch();
    const file = strandedOver(dir, 'alpha\n', 'alpha\n');
    const publisher = new Publisher(new RecordingFs(), nothingOpen());
    fs.unlinkSync(file);
    const done = publisher.reconcileBy(file, 'prepend-prefix', '## Two\n', '## Two\nstored\n', 'alpha\n');
    assert.deepStrictEqual(done, { done: false, file, because: 'file-changed' });
  });
});
