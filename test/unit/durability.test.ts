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
 * What happens to the queue when the disk says no.
 *
 * ALL OF THESE CAME OUT OF A REVIEW, and each names a way the queue
 * could disagree with what is on disk. The pattern in every one is the
 * same: a step changed memory and then failed to change the file, and
 * everything afterwards acted on a state nothing had recorded.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { MAX_TIMEOUT_MS, problemsWith } from '../../src/config';
import { Outbox, OutboxEntry, OutboxWriteError } from '../../src/outbox';

function scratch(name: string): string {
  return path.join(fs.mkdtempSync(path.join(os.tmpdir(), `theourgia-${name}-`)), 'outbox.json');
}

function entry(req: string, payload: string): OutboxEntry {
  return {
    req,
    cursor: 'w:7',
    id: 'a.2',
    field: 'src',
    payload,
    state: 'queued',
    createdAt: 0,
    lastError: null
  };
}

describe('a change that did not reach the disk did not happen', () => {
  it('does not hold an entry in memory that it could not write down', () => {
    /*
     * The queue is read while its directory is real -- a missing file is
     * an empty queue -- and only then is the directory replaced by a
     * file, so that the WRITE is what fails and not the read. Breaking
     * it before the read would test the other rule.
     */
    const file = scratch('unwritable');
    const outbox = new Outbox(file);
    outbox.load();
    const directory = path.dirname(file);
    fs.rmSync(directory, { recursive: true, force: true });
    fs.writeFileSync(directory, 'this is a file, not a directory\n', 'utf8');

    assert.throws(() => outbox.enqueue(entry('11111111-1111-1111-1111-111111111111', 'one\n')), OutboxWriteError);
    assert.strictEqual(
      outbox.pendingCount,
      0,
      'the entry stayed in memory after the write failed, so a later retry would send a request nothing recorded'
    );
  });

  it('does not forget an entry it could not record as resolved', () => {
    const file = scratch('resolve');
    const outbox = new Outbox(file);
    outbox.load();
    outbox.enqueue(entry('22222222-2222-2222-2222-222222222222', 'one\n'));
    assert.strictEqual(outbox.pendingCount, 1);

    /*
     * The directory is replaced by a file, so the next write cannot
     * succeed. This is the shape of a disk that filled up or a
     * permission that changed between two saves.
     */
    fs.rmSync(path.dirname(file), { recursive: true, force: true });
    fs.writeFileSync(path.dirname(file), 'in the way\n', 'utf8');

    assert.throws(() => outbox.resolve('22222222-2222-2222-2222-222222222222', 'w:9'), OutboxWriteError);
    assert.strictEqual(
      outbox.pendingCount,
      1,
      'the entry was dropped from memory although the removal was never recorded'
    );
    assert.strictEqual(outbox.cursor, null, 'the cursor moved on a write that failed');
  });

  it('does not keep a cursor it could not record', () => {
    const file = scratch('cursor');
    const outbox = new Outbox(file);
    outbox.load();
    fs.rmSync(path.dirname(file), { recursive: true, force: true });
    fs.writeFileSync(path.dirname(file), 'in the way\n', 'utf8');
    assert.throws(() => outbox.setCursor('w:9'), OutboxWriteError);
    assert.strictEqual(outbox.cursor, null);
  });
});

describe('a queue that could not be read is not an empty queue', () => {
  it('reads a file that is not there as an empty queue', () => {
    const outbox = new Outbox(scratch('missing'));
    outbox.load();
    assert.strictEqual(outbox.pendingCount, 0);
    assert.strictEqual(outbox.cursor, null);
  });

  it('refuses to carry on when the file exists and could not be read', () => {
    const file = scratch('unreadable');
    fs.writeFileSync(file, '{"version":1,"cursor":"w:7","entries":[]}\n', 'utf8');
    /*
     * A directory where the file was: the read fails with something
     * other than "not there", which is every reason that leaves the
     * question of what was recorded open.
     */
    fs.rmSync(file);
    fs.mkdirSync(file);
    const outbox = new Outbox(file);
    assert.throws(() => outbox.load(), OutboxWriteError);
  });

  it('will not write over a file it could not read', () => {
    const file = scratch('no-clobber');
    fs.writeFileSync(file, 'this is not json\n', 'utf8');
    const outbox = new Outbox(file);
    assert.throws(() => outbox.load(), OutboxWriteError);
    assert.throws(
      () => outbox.setCursor('w:9'),
      OutboxWriteError,
      'the unread file was about to be replaced with an empty queue'
    );
    assert.strictEqual(
      fs.readFileSync(file, 'utf8'),
      'this is not json\n',
      'the file that could not be read was overwritten anyway'
    );
  });

  it('will not write to a queue nobody has read', () => {
    const outbox = new Outbox(scratch('unloaded'));
    assert.throws(() => outbox.enqueue(entry('33333333-3333-3333-3333-333333333333', 'x\n')), OutboxWriteError);
  });
});

describe('a timeout the timer cannot honour is refused where it is read', () => {
  const base = {
    scheme: 'scheme',
    corePath: '/core',
    libDirs: [],
    store: '/store',
    actor: 'someone',
    transport: 'cli' as const
  };

  it('accepts the largest delay the timer takes', () => {
    assert.deepStrictEqual(problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS }), []);
  });

  it('refuses one larger, which the timer would turn into a millisecond', () => {
    const problems = problemsWith({ ...base, timeoutMs: MAX_TIMEOUT_MS + 1 });
    assert.strictEqual(problems.length, 1);
    assert.strictEqual(problems[0].setting, 'theourgia.timeoutMs');
    assert.match(problems[0].message, /millisecond/);
  });

  it('still refuses a timeout of zero or less', () => {
    assert.strictEqual(problemsWith({ ...base, timeoutMs: 0 }).length, 1);
    assert.strictEqual(problemsWith({ ...base, timeoutMs: -1 }).length, 1);
  });
});
