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
 * A CANCELLED REQUEST STAYS CANCELLED. (R9.2, R9.8)
 *
 * The check sits in front of every transmission, every import and every
 * repair, and the two names it composes into a path both come from
 * somewhere this program does not control: the store name from the
 * settings, the request id out of another session's queue file.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { FileOps, nodeFileOps } from '../../src/fsops';
import { Tombstones } from '../../src/tombstones';

const STORE = 'store-0a1b2c3d';
const REQ = '11111111-2222-3333-4444-555555555555';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-retired-'));
}

/*
 * A FILE SYSTEM THAT COUNTS THE TWO CALLS THIS IS ABOUT. `list` and
 * `readDirectory` are the ones a tombstone check must never make: the
 * question is whether ONE name is there.
 */
function counting(inner: FileOps = nodeFileOps): { files: FileOps; listings: number } {
  const state = { listings: 0 };
  const files: FileOps = {
    ...inner,
    list: (directory) => {
      state.listings += 1;
      return inner.list(directory);
    },
    readDirectory: (directory) => {
      state.listings += 1;
      return inner.readDirectory(directory);
    }
  };
  return {
    files,
    get listings(): number {
      return state.listings;
    }
  } as { files: FileOps; listings: number };
}

describe('R9 a cancelled request is retired where every copy can see it', () => {
  it('records the retirement under the storage root, not in a session', () => {
    const storage = scratch();
    const tombstones = new Tombstones(storage, nodeFileOps);
    tombstones.retire(STORE, REQ);
    assert.strictEqual(
      fs.existsSync(path.join(storage, 'retired', STORE, REQ)),
      true,
      'the tombstone is not at the path the design names'
    );
  });

  it('says a retired request is retired, and an untouched one is not', () => {
    const storage = scratch();
    const tombstones = new Tombstones(storage, nodeFileOps);
    tombstones.retire(STORE, REQ);
    assert.deepStrictEqual(tombstones.isRetired(STORE, REQ), { known: true, retired: true });
    assert.deepStrictEqual(
      tombstones.isRetired(STORE, '99999999-8888-7777-6666-555555555555'),
      { known: true, retired: false }
    );
  });

  /*
   * ⚠️ A TOMBSTONE BELONGS TO ONE STORE. The same request id can exist
   * in two stores -- nothing about a uuid says which store it was made
   * for -- and a check that ignored the store would cancel a live
   * request somewhere else.
   */
  it('does not find a retirement left under another store', () => {
    const storage = scratch();
    const tombstones = new Tombstones(storage, nodeFileOps);
    tombstones.retire('store-elsewhere', REQ);
    assert.deepStrictEqual(tombstones.isRetired(STORE, REQ), { known: true, retired: false });
  });

  it('retires the same request twice without complaining', () => {
    const storage = scratch();
    const tombstones = new Tombstones(storage, nodeFileOps);
    tombstones.retire(STORE, REQ);
    tombstones.retire(STORE, REQ);
    assert.deepStrictEqual(tombstones.isRetired(STORE, REQ), { known: true, retired: true });
  });

  /*
   * ⚠️ THE CHECK IS A PROBE, AND THAT IS A COST STATEMENT AS WELL AS A
   * CORRECTNESS ONE. It runs before every transmission, every import
   * and every repair; a listing makes each of those proportional to how
   * much has ever been cancelled in that store.
   */
  it('answers by name, whatever else is in the directory', () => {
    const storage = scratch();
    const plain = new Tombstones(storage, nodeFileOps);
    plain.retire(STORE, REQ);
    const directory = path.join(storage, 'retired', STORE);
    for (let n = 0; n < 500; n += 1) {
      fs.writeFileSync(path.join(directory, `00000000-0000-0000-0000-${String(n).padStart(12, '0')}`), '', 'utf8');
    }
    const counted = counting();
    const tombstones = new Tombstones(storage, counted.files);
    assert.deepStrictEqual(tombstones.isRetired(STORE, REQ), { known: true, retired: true });
    assert.deepStrictEqual(
      tombstones.isRetired(STORE, '99999999-8888-7777-6666-555555555555'),
      { known: true, retired: false }
    );
    assert.strictEqual(
      counted.listings,
      0,
      'the check listed the directory, so it costs as much as everything ever cancelled in this store'
    );
  });

  /*
   * ⚠️ AND "I COULD NOT LOOK" IS ITS OWN ANSWER. The caller is deciding
   * whether to send; reading an unsearchable directory as "nothing is
   * retired" is the convenient answer and the one that resends
   * cancelled work.
   */
  it('says it could not look rather than that nothing is retired', () => {
    const storage = scratch();
    const blind: FileOps = {
      ...nodeFileOps,
      presenceOf: () => ({ known: false })
    };
    const tombstones = new Tombstones(storage, blind);
    assert.deepStrictEqual(tombstones.isRetired(STORE, REQ), { known: false });
  });
});

describe('R9 the names a tombstone path is built from are checked', () => {
  const tombstones = new Tombstones('/storage', nodeFileOps);

  it('refuses a request id that is a path', () => {
    assert.throws(() => tombstones.pathFor(STORE, '../../elsewhere'), /request id/);
    assert.throws(() => tombstones.pathFor(STORE, 'a/b'), /request id/);
    assert.throws(() => tombstones.pathFor(STORE, '..'), /request id/);
  });

  it('refuses a request id carrying a control character', () => {
    assert.throws(() => tombstones.pathFor(STORE, `a${String.fromCharCode(0)}b`), /control character/);
    assert.throws(() => tombstones.pathFor(STORE, 'a\nb'), /control character/);
  });

  it('refuses a store name that is a path', () => {
    assert.throws(() => tombstones.pathFor('../live', REQ), /store name/);
  });

  /*
   * ⚠️ AND SAYS WHICH OF THE TWO WAS WRONG. One message for both names
   * would leave the reader of a report unable to tell a store setting
   * from a queue file somebody edited -- and those have different
   * people to go and talk to.
   */
  it('names which of the two it refused', () => {
    assert.throws(() => tombstones.pathFor('../live', REQ), /a store name/);
    assert.throws(() => tombstones.pathFor(STORE, '../live'), /a request id/);
  });

  it('builds the path the design names when both are ordinary', () => {
    assert.strictEqual(
      tombstones.pathFor(STORE, REQ),
      path.join('/storage', 'retired', STORE, REQ)
    );
  });
});
