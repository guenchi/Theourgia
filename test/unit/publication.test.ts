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
import { OpenDocuments, Publisher, sidecarFromDisk, sidecarToDisk, Sidecar } from '../../src/publication';
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
  it('never writes a path it has written before', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());
    for (const text of ['## Two\none\n', '## Two\ntwo\n', '## Two\nthree\n']) {
      await publisher.publish(request(dir, text));
    }
    for (const file of new Set(files.touched('writeText').concat(files.touched('writeDurably')))) {
      const writes =
        files.countOf('writeText', file) + files.countOf('writeDurably', file);
      assert.ok(
        writes <= 1 || file.endsWith('.meta'),
        `${file} was written ${writes} times by the extension; publication is supposed to be immutable`
      );
    }
  });

  it('never unlinks or renames anything it published', async () => {
    const dir = scratch();
    const files = new RecordingFs();
    const publisher = new Publisher(files, nothingOpen());
    for (const text of ['## Two\none\n', '## Two\ntwo\n']) {
      await publisher.publish(request(dir, text));
    }
    assert.deepStrictEqual(files.touched('unlink'), [], 'a published path was unlinked');
    assert.deepStrictEqual(files.touched('rename'), [], 'a published path was renamed');
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
      unresolved: false
    };
    const onDisk = sidecarToDisk(sidecar);
    assert.ok('acknowledged-raw' in onDisk, 'the record does not use the name the design gave it');
    assert.ok('local-only' in onDisk, 'the record does not use the name the design gave it');
    assert.ok(!('acknowledgedRaw' in onDisk), 'this language’s spelling reached the file format');
    assert.strictEqual(onDisk.format, 1, 'the record does not say which shape it is');
  });

  it('reads back exactly what it wrote', () => {
    const sidecar: Sidecar = {
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
      unresolved: true
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
      unresolved: true
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
      unresolved: true
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
          unresolved: true
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
