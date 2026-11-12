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
 * WHERE A SPLIT'S REVIEW LIVES AND WHEN IT GOES (src/split-reviews.ts): a
 * fresh directory per split under this window's session, removed when no
 * tab shows the review, and at activation the reviews of windows that are
 * gone -- never those of a window that is still open, nor of one whose
 * liveness cannot be told.
 */

import * as assert from 'assert';
import { execFileSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { nodeFileOps } from '../../src/fsops';
import { Liveness, Sessions } from '../../src/sessions';
import { ReviewDisk, SplitReviews, filesShownBy, nodeReviewDisk, sessionIsGone } from '../../src/split-reviews';

const ALIVE: Liveness = { alive: true, because: 'identity-matches' };
const DEAD: Liveness = { alive: false, because: 'pid-absent' };
const MISSING: Liveness = { decidable: false, because: 'record-missing' };
const UNREADABLE: Liveness = { decidable: false, because: 'record-unreadable' };

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-split-reviews-'));
}

describe('a split review is placed in a directory of its own, under this window', () => {
  let root: string;
  beforeEach(() => {
    root = scratch();
  });
  afterEach(() => fs.rmSync(root, { recursive: true, force: true }));

  it('names <root>/<session>/<fresh>/<the source\'s name>, the directory made and the file not', () => {
    const reviews = new SplitReviews(root, 'me', () => ALIVE);
    const one = reviews.place('/somewhere/src/split.js');
    const two = reviews.place('/somewhere/src/split.js');
    assert.notStrictEqual(one, two, 'two splits of one file were given one path');
    for (const review of [one, two]) {
      assert.strictEqual(path.basename(review), 'split.js');
      assert.strictEqual(path.dirname(path.dirname(review)), path.join(root, 'me'));
      assert.ok(fs.statSync(path.dirname(review)).isDirectory(), `${path.dirname(review)} was not made`);
      assert.strictEqual(fs.existsSync(review), false, `${review} exists before the core wrote it`);
    }
  });

  it('discards the directory of a review it placed, and nothing it did not place', () => {
    const reviews = new SplitReviews(root, 'me', () => ALIVE);
    const review = reviews.place('/src/a.js');
    fs.writeFileSync(review, 'x');
    const other = path.join(root, 'someone', 'u', 'a.js');
    fs.mkdirSync(path.dirname(other), { recursive: true });
    fs.writeFileSync(other, 'y');
    const outside = path.join(scratch(), 'a.js');
    fs.writeFileSync(outside, 'z');
    assert.deepStrictEqual(reviews.discard(review), []);
    assert.strictEqual(fs.existsSync(path.dirname(review)), false, 'the placed review\'s directory is still there');
    assert.deepStrictEqual(reviews.discard(other), []);
    assert.deepStrictEqual(reviews.discard(outside), []);
    assert.strictEqual(fs.existsSync(other), true, 'another window\'s review was removed');
    assert.strictEqual(fs.existsSync(outside), true, 'a file outside the root was removed');
    fs.rmSync(path.dirname(outside), { recursive: true, force: true });
  });
});

describe('a split review goes with the last tab that shows it', () => {
  let root: string;
  beforeEach(() => {
    root = scratch();
  });
  afterEach(() => fs.rmSync(root, { recursive: true, force: true }));

  it('keeps a review still shown, removes one no longer shown, and forgets it once removed', () => {
    const reviews = new SplitReviews(root, 'me', () => ALIVE);
    const kept = reviews.place('/src/a.js');
    const gone = reviews.place('/src/b.js');
    fs.writeFileSync(kept, 'a');
    fs.writeFileSync(gone, 'b');
    reviews.opened(kept);
    reviews.opened(gone);
    const first = reviews.closed([kept]);
    assert.deepStrictEqual(first, { removed: [gone], failed: [] });
    assert.strictEqual(fs.existsSync(path.dirname(gone)), false, 'the closed review\'s directory is still there');
    assert.strictEqual(fs.existsSync(kept), true, 'the review still shown was removed');
    assert.deepStrictEqual(reviews.closed([kept]), { removed: [], failed: [] }, 'a removed review was removed again');
    assert.deepStrictEqual(reviews.closed([]).removed, [kept]);
    assert.strictEqual(fs.existsSync(path.dirname(kept)), false);
  });

  it('removes only reviews it opened: a placed review never opened is left to the sweep', () => {
    const reviews = new SplitReviews(root, 'me', () => ALIVE);
    const placed = reviews.place('/src/a.js');
    fs.writeFileSync(placed, 'a');
    reviews.opened(path.join(os.tmpdir(), 'not-mine', 'u', 'a.js'));
    assert.deepStrictEqual(reviews.closed([]), { removed: [], failed: [] });
    assert.strictEqual(fs.existsSync(placed), true);
  });

  /*
   * ONE FILE, TWO SPELLINGS. The editor hands a review back as a Uri's
   * fsPath, which on Windows writes the drive letter in lower case; the
   * review was placed under the storage path as given. Read with Windows'
   * rules on any system, through a disk that only records.
   */
  it('takes a Windows path back with its drive letter in another case as the review it placed, and removes it', () => {
    const made: string[] = [];
    const gone: string[] = [];
    const disk: ReviewDisk = {
      makeDirectory: (d) => {
        made.push(d);
      },
      list: () => [],
      removeTree: (d) => {
        gone.push(d);
      }
    };
    const reviews = new SplitReviews('C:\\Users\\u\\storage\\split', 'me', () => ALIVE, disk, 'win32');
    const review = reviews.place('D:\\work\\src\\Split.js');
    assert.ok(review.startsWith('C:\\Users\\u\\storage\\split\\me\\'), review);
    assert.ok(review.endsWith('\\Split.js'), review);
    const back = 'c' + review.slice(1);
    reviews.opened(back);
    assert.deepStrictEqual(reviews.closed([back.toUpperCase()]).removed, [], 'a review still shown, spelled in other case, was removed');
    assert.deepStrictEqual(reviews.closed([]).removed, [back]);
    assert.deepStrictEqual(gone.map((d) => d.toLowerCase()), [made[0].toLowerCase()]);
  });

  /*
   * A `..` AS IT ARRIVES. The paths are spelled out by hand, never joined
   * or resolved, so the `..` reaches SplitReviews unread. `<root>/me/../a.js`
   * is the shape that matters: its parent's parent spells this window's
   * directory while its parent is the root itself. Through a disk that only
   * records, under both systems' rules.
   */
  it('never takes a path with a raw .. in it for one of its own reviews, nor removes what the .. reaches', () => {
    const cases: Array<[NodeJS.Platform, string, string]> = [
      ['linux', '/r/storage/split', '/'],
      ['win32', 'C:\\r\\storage\\split', '\\']
    ];
    for (const [platform, top, sep] of cases) {
      const gone: string[] = [];
      const disk: ReviewDisk = {
        makeDirectory: () => undefined,
        list: () => [],
        removeTree: (d) => {
          gone.push(d);
        }
      };
      const reviews: SplitReviews = new SplitReviews(top, 'me', () => ALIVE, disk, platform);
      const raw = [top, 'me', '..', 'a.js'].join(sep);
      assert.ok(raw.includes(`${sep}..${sep}`), `${raw} reached the cell without its ..`);
      assert.deepStrictEqual(reviews.discard(raw), []);
      reviews.opened(raw);
      assert.deepStrictEqual(reviews.closed([]).removed, [], `${platform}: ${raw} was taken for a review`);
      assert.deepStrictEqual(gone, [], `${platform}: a directory reached through .. was removed`);
    }
  });

  /*
   * WHAT THE TABS SHOW, by the fields of each tab's input: a text tab, a
   * custom editor and a notebook by `uri`, a text or notebook diff by both
   * sides, and a webview or an input with no file not at all. A review
   * still open in a custom editor after its text tab closed is kept.
   */
  it('reads every file a tab shows, in any kind of editor, and keeps a review open only in a custom editor', () => {
    const file = (fsPath: string) => ({ scheme: 'file', fsPath });
    const groups = [
      { tabs: [{ input: { uri: file('/t/text.js') } }, { input: { uri: file('/t/table.csv'), viewType: 'csv.edit' } }] },
      {
        tabs: [
          { input: { uri: file('/t/book.ipynb'), notebookType: 'jupyter-notebook' } },
          { input: { original: file('/t/a.js'), modified: file('/t/b.js') } },
          { input: { original: file('/t/old.ipynb'), modified: file('/t/new.ipynb'), notebookType: 'jupyter-notebook' } },
          { input: { viewType: 'markdown.preview' } },
          { input: { uri: '/t/not-a-uri.js' } },
          { input: undefined },
          { input: null }
        ]
      }
    ];
    assert.deepStrictEqual(
      filesShownBy(groups).sort(),
      ['/t/text.js', '/t/table.csv', '/t/book.ipynb', '/t/a.js', '/t/b.js', '/t/old.ipynb', '/t/new.ipynb'].sort()
    );
    const reviews = new SplitReviews(root, 'me', () => ALIVE);
    const review = reviews.place('/src/table.csv');
    fs.writeFileSync(review, 'a,b');
    reviews.opened(review);
    const customOnly = [{ tabs: [{ input: { uri: file(review), viewType: 'csv.edit' } }] }];
    assert.deepStrictEqual(reviews.closed(filesShownBy(customOnly)).removed, [], 'a review open in a custom editor was removed');
    assert.strictEqual(fs.existsSync(review), true);
    assert.deepStrictEqual(reviews.closed(filesShownBy([])).removed, [review]);
  });

  it('reports a removal that failed, throws nothing, and does not try it again', () => {
    const disk: ReviewDisk = {
      ...nodeReviewDisk,
      removeTree: () => {
        throw new Error('EACCES');
      }
    };
    const reviews = new SplitReviews(root, 'me', () => ALIVE, disk);
    const review = reviews.place('/src/a.js');
    reviews.opened(review);
    const outcome = reviews.closed([]);
    assert.deepStrictEqual(outcome.removed, []);
    assert.deepStrictEqual(outcome.failed.map((f) => [f.directory, String(f.error)]), [[path.dirname(review), 'Error: EACCES']]);
    assert.deepStrictEqual(reviews.closed([]), { removed: [], failed: [] });
    assert.strictEqual(reviews.discard(review).length, 1, 'a failed discard was not reported');
  });
});

describe('at activation the reviews of windows that are gone are swept, and only those', () => {
  let root: string;
  beforeEach(() => {
    root = scratch();
  });
  afterEach(() => fs.rmSync(root, { recursive: true, force: true }));

  it('takes a session as gone when its process is not running or its record was removed, and on nothing else', () => {
    assert.strictEqual(sessionIsGone(DEAD), true);
    assert.strictEqual(sessionIsGone({ alive: false, because: 'pid-reused' }), true);
    assert.strictEqual(sessionIsGone(MISSING), true);
    assert.strictEqual(sessionIsGone(ALIVE), false);
    assert.strictEqual(sessionIsGone({ alive: true, because: 'identity-matches-permission-denied' }), false);
    for (const because of ['start-time-unavailable', 'start-time-unrecorded', 'liveness-unobtainable', 'record-unreadable'] as const) {
      assert.strictEqual(sessionIsGone({ decidable: false, because }), false, because);
    }
  });

  it('removes the dead and the unrecorded, keeps the alive, the undecidable and one it cannot ask about, and skips its own', () => {
    const states: Record<string, Liveness | 'throws'> = { dead: DEAD, unrecorded: MISSING, alive: ALIVE, unreadable: UNREADABLE, asking: 'throws' };
    for (const name of [...Object.keys(states), 'me']) {
      fs.mkdirSync(path.join(root, name, 'u'), { recursive: true });
      fs.writeFileSync(path.join(root, name, 'u', 'a.js'), name);
    }
    const asked: string[] = [];
    const reviews = new SplitReviews(root, 'me', (id) => {
      asked.push(id);
      const state = states[id];
      if (state === 'throws' || state === undefined) {
        throw new Error(`no answer for ${id}`);
      }
      return state;
    });
    const outcome = reviews.sweep();
    assert.ok(!asked.includes('me'), 'this window asked about itself');
    assert.deepStrictEqual(outcome.removed.sort(), [path.join(root, 'dead'), path.join(root, 'unrecorded')]);
    assert.deepStrictEqual(outcome.kept.sort(), [path.join(root, 'alive'), path.join(root, 'asking'), path.join(root, 'unreadable')]);
    assert.deepStrictEqual(outcome.failed, []);
    assert.deepStrictEqual(fs.readdirSync(root).sort(), ['alive', 'asking', 'me', 'unreadable']);
  });

  it('sweeps nothing and fails nothing when there is no root yet', () => {
    const reviews = new SplitReviews(path.join(root, 'absent'), 'me', () => DEAD);
    assert.deepStrictEqual(reviews.sweep(), { removed: [], kept: [], failed: [] });
  });

  /*
   * WITH THE SESSION RECORDS THEMSELVES: a window that began in this
   * process is alive, a record whose pid has exited is dead, and an id
   * with no record is unrecorded. The liveness is the one `Sessions`
   * answers, through the accessor the extension hands the sweep.
   */
  it('reads liveness from real session records', () => {
    const storage = scratch();
    const sessions = new Sessions(nodeFileOps, storage);
    sessions.begin('open-window', []);
    const exited = Number(execFileSync(process.execPath, ['-e', 'process.stdout.write(String(process.pid))']).toString());
    fs.mkdirSync(path.join(storage, 'sessions', 'closed-window'), { recursive: true });
    fs.writeFileSync(
      path.join(storage, 'sessions', 'closed-window', 'session.json'),
      JSON.stringify({ pid: exited, startedAt: 1, nonce: 'n', stores: [] })
    );
    assert.deepStrictEqual(sessions.livenessById('closed-window'), DEAD, 'the exited pid is not read as absent');
    const splitRoot = path.join(storage, 'split');
    for (const name of ['open-window', 'closed-window', 'never-recorded']) {
      fs.mkdirSync(path.join(splitRoot, name, 'u'), { recursive: true });
    }
    const outcome = new SplitReviews(splitRoot, 'this-window', (id) => sessions.livenessById(id)).sweep();
    assert.deepStrictEqual(outcome.removed.sort(), [path.join(splitRoot, 'closed-window'), path.join(splitRoot, 'never-recorded')]);
    assert.deepStrictEqual(outcome.kept, [path.join(splitRoot, 'open-window')]);
    fs.rmSync(storage, { recursive: true, force: true });
  });
});
