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
 * plugin-r3 item 5: a block's projection file is named after its title, once,
 * and found again by its sidecar rather than by its name.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { nodeFileOps } from '../../src/fsops';
import { projectionFileIn, projectionNameFor, slugOf } from '../../src/projection-name';
import { Publisher, UNNUMBERED, writeSidecar } from '../../src/publication';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-names-'));
}

function block(): string {
  const directory = path.join(scratch(), 'a.2');
  fs.mkdirSync(directory, { recursive: true });
  return directory;
}

const request = (directory: string, prefix: string, body = 'body\n') => ({
  directory,
  storeId: 's1',
  blockId: 'a.2',
  prefix,
  text: `${prefix}${body}`,
  cursor: null
});

describe('plugin-r3 5 the slug of a title', () => {
  it('keeps a Chinese title as it is written', () => {
    assert.strictEqual(slugOf('两段 文字'), '两段-文字');
  });

  it('lower-cases ASCII letters and joins the words of a mixed title', () => {
    assert.strictEqual(slugOf('  Part 2: 中文 Notes '), 'part-2-中文-notes');
  });

  it('has nothing left of a title made of symbols', () => {
    assert.strictEqual(slugOf('!!! / ?? * <>'), '');
    /*
     * AND A NUMBER THAT IS NOT A DIGIT IS A SYMBOL HERE: a fraction, a
     * circled number, a Roman numeral (review r1 of item 5).
     */
    assert.strictEqual(slugOf('½ ① Ⅻ'), '');
  });

  it('has nothing left of an empty title', () => {
    assert.strictEqual(slugOf(''), '');
  });

  it('stops at forty code points and does not end on a dash', () => {
    const long = `${'字'.repeat(39)} tail and more`;
    const slug = slugOf(long);
    assert.ok(Array.from(slug).length <= 40, `the slug is ${Array.from(slug).length} code points long`);
    assert.strictEqual(slug, '字'.repeat(39), 'the cut left a trailing dash, or cut somewhere else');
  });

  it('drops a leading dot, so the file is not hidden', () => {
    assert.strictEqual(slugOf('.hidden notes'), 'hidden-notes');
  });
});

describe('plugin-r3 5 the projection is named once and found by its sidecar', () => {
  it('names a first publication <slug>-<blockId>.md, and <blockId>.md when there is no slug', async () => {
    assert.strictEqual(projectionNameFor('Two Sections', 'a.2'), 'two-sections-a.2.md');
    assert.strictEqual(projectionNameFor('!!!', 'a.2'), 'a.2.md');
    const named = block();
    const first = await new Publisher(new RecordingFs(), { isOpen: () => false }).publish(request(named, '## Two Sections\n'));
    assert.ok(first.published, JSON.stringify(first));
    assert.strictEqual(path.basename(first.file), 'two-sections-a.2.md');
    const untitled = block();
    const bare = await new Publisher(new RecordingFs(), { isOpen: () => false }).publish(request(untitled, ''));
    assert.ok(bare.published, JSON.stringify(bare));
    assert.strictEqual(path.basename(bare.file), 'a.2.md');
  });

  /*
   * KEY: THE NAME IS CHOSEN ONCE. A later title does not rename the file: the
   * editor may have it open, and nothing reads the name back.
   */
  it('keeps the first name when the title changes', async () => {
    const directory = block();
    const publisher = new Publisher(new RecordingFs(), { isOpen: () => false });
    const first = await publisher.publish(request(directory, '## First Title\n'));
    assert.ok(first.published);
    const second = await publisher.publish(request(directory, '## A Different Title\n'));
    assert.ok(second.published, JSON.stringify(second));
    assert.strictEqual(second.file, first.file, 'the file was renamed after its title changed');
    assert.deepStrictEqual(fs.readdirSync(directory).sort(), ['first-title-a.2.md', 'first-title-a.2.md.meta']);
  });

  /*
   * KEY: A DIRECTORY MADE BEFORE NAMES CARRIED TITLES IS STILL READ. It holds
   * `current.md` and its sidecar, laid down here the way an older build left
   * them; the block's projection is that file, and a publication updates it in
   * place rather than making a second one.
   */
  it('reads and updates a current.md an older build left', async () => {
    const directory = block();
    const publisher = new Publisher(new RecordingFs(), { isOpen: () => false });
    const scratchDir = block();
    const seeded = await publisher.publish(request(scratchDir, '## Two\n', 'old body\n'));
    assert.ok(seeded.published);
    const current = path.join(directory, 'current.md');
    fs.copyFileSync(seeded.file, current);
    fs.copyFileSync(`${seeded.file}.meta`, `${current}.meta`);
    assert.strictEqual(publisher.latestIn(directory), current, 'the old name is not found as the projection');
    const updated = await publisher.publish(request(directory, '## Two\n', 'new body\n'));
    assert.ok(updated.published, JSON.stringify(updated));
    assert.strictEqual(updated.file, current, 'the old projection was not the one updated');
    assert.strictEqual(fs.readFileSync(current, 'utf8'), '## Two\nnew body\n');
    assert.deepStrictEqual(fs.readdirSync(directory).sort(), ['current.md', 'current.md.meta']);
  });

  it('refuses to choose between two projections, and names both', async () => {
    const directory = block();
    for (const name of ['one-a.2.md', 'two-a.2.md']) {
      const file = path.join(directory, name);
      fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
      writeSidecar(nodeFileOps, file, {
        ...UNNUMBERED,
        format: 1,
        storeId: 's1',
        blockId: 'a.2',
        phase: 'published',
        prefix: '## Two\n',
        written: 'w',
        previous: null,
        acknowledgedRaw: null,
        sent: null,
        cursor: null,
        localOnly: false,
        unresolved: false,
        bodyHasCrlf: false
      });
    }
    assert.deepStrictEqual(projectionFileIn(nodeFileOps, directory), { found: 'unknown', names: ['one-a.2.md', 'two-a.2.md'] });
    const refused = await new Publisher(new RecordingFs(), { isOpen: () => false }).publish(request(directory, '## Two\n'));
    assert.deepStrictEqual(refused, { published: false, because: 'unknown-file', file: null, seen: ['one-a.2.md', 'two-a.2.md'] });
  });

  it('does not write a second file beside a .md it does not know', async () => {
    const directory = block();
    const stranger = path.join(directory, 'notes.md');
    fs.writeFileSync(stranger, 'somebody else\n', 'utf8');
    const refused = await new Publisher(new RecordingFs(), { isOpen: () => false }).publish(request(directory, '## Two\n'));
    assert.deepStrictEqual(refused, { published: false, because: 'unknown-file', file: null, seen: ['notes.md'] });
    assert.deepStrictEqual(fs.readdirSync(directory), ['notes.md']);
  });

  /*
   * AND A TEMPORARY NEVER SITS WHERE THE PROJECTION IS LOOKED FOR. Staging used
   * to be chosen by the name `current.md`; a titled name must be staged outside
   * the block directory the same way.
   */
  it('stages a titled projection and its sidecar outside the block directory', async () => {
    const directory = block();
    const files = new RecordingFs();
    const published = await new Publisher(files, { isOpen: () => false }).publish(request(directory, '## Two\n'));
    assert.ok(published.published);
    const staged = files.touched('writeDurably');
    assert.ok(staged.length > 0, 'nothing was written durably, so this cell saw no staging');
    for (const file of staged) {
      assert.notStrictEqual(path.dirname(file), directory, `${file} was staged inside the block directory`);
    }
  });
});
