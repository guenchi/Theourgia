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
 * Every file operation this extension performs, named in one place.
 *
 * IT EXISTS SO THAT WHAT THIS PROCESS DID TO A PATH CAN BE ASKED. The
 * design this batch implements says the extension never unlinks and
 * never renames a path it has published, and the only way to hold a
 * build to that is to count what it actually called -- a claim about
 * source text would be a claim about the lines somebody happened to
 * grep for. So the operations arrive as an object, and a cell can hand
 * over one that records.
 *
 * IT IS A PARAMETER, NOT A HOOK. Nothing here exists for the benefit of
 * a test: a component that writes files genuinely does not need to know
 * that they are Node's files, and saying so in the signature is what
 * makes the recording possible. The editor is handed to `Publisher`
 * the same way and for the same reason.
 *
 * THE SET IS EXACTLY WHAT IS USED, and it stays that way. A wider
 * interface than the product needs is a set of operations nothing
 * records the absence of -- and "the extension never unlinks" is a
 * statement about operations, so an operation that can be reached
 * without passing through here is a hole in the measurement. The cell
 * `fsops.test.ts` refuses any other file under src/ that imports `fs`.
 */

import * as fs from 'fs';

export interface FileOps {
  readText(file: string): string;
  /*
   * THE BYTES, UNDECODED. The save handler has to know whether what is
   * on disk carries a byte-order mark and whether it is strictly UTF-8;
   * `readText` has already decided both, lossily, and a string cannot
   * be asked what it used to be. (§12.17.3)
   */
  readBytes(file: string): Buffer;
  writeText(file: string, text: string): void;
  makeDirectory(directory: string): void;
  rename(from: string, to: string): void;
  unlink(file: string): void;
  /*
   * THE DURABLE WRITE IS ONE OPERATION, not an open/write/fsync/close
   * quartet on the caller's side. Flushing a file and then its directory
   * is the part that gets forgotten, and a recorder that saw four calls
   * would still not be able to say whether the pair happened.
   */
  writeDurably(file: string, text: string): void;
  syncDirectory(directory: string): void;
  exists(file: string): boolean;
  /*
   * ⚠️ THE HONEST FORM OF `exists`, FOR THE CALLER THAT REPORTS WHAT IT
   * FINDS.
   *
   * `existsSync` answers false for a file that is not there AND for one
   * under an ancestry this process may not search -- the same word for
   * "no" and for "I could not look". That is the right convenience for a
   * caller choosing what to do next, and the wrong one for a caller
   * writing a sentence a user will act on: a leftover file that becomes
   * unreachable does not stop existing, and permissions can be given
   * back. So this one distinguishes them, the way `readDirectory` does
   * for `list`. Found in review.
   */
  presenceOf(file: string): { known: true; there: boolean } | { known: false };
  /*
   * A CREATE-ONCE PUBLICATION. `link` fails with EEXIST if the name is
   * taken, which is what makes a claim token a token: whoever's call
   * succeeds holds it, and no read-then-write window exists for two
   * windows to race in. (§12.13.5)
   */
  link(existing: string, fresh: string): void;
  list(directory: string): string[];
  /*
   * ⚠️ THE HONEST FORM OF `list`, FOR THE ONE CALLER THAT CANNOT AFFORD
   * ITS ANSWER.
   *
   * `list` returns `[]` when it cannot read the directory, which is the
   * right convenience for a caller asking "what versions are here" -- an
   * absent directory holds none. It is the wrong answer for a caller
   * asking "is the record really not there", because a directory this
   * process may not search reports everything inside it as absent, and
   * that reading is the one thing an explicit session takeover is opened
   * by. The two questions needed two primitives rather than one word
   * meaning both.
   */
  readDirectory(directory: string):
    | { read: true; names: string[] }
    | { read: false; because: 'absent' | 'unreadable' };
  isDirectory(file: string): boolean;
}

export const nodeFileOps: FileOps = {
  readText: (file) => fs.readFileSync(file, 'utf8'),
  readBytes: (file) => fs.readFileSync(file),
  writeText: (file, text) => fs.writeFileSync(file, text, 'utf8'),
  makeDirectory: (directory) => {
    fs.mkdirSync(directory, { recursive: true });
  },
  rename: (from, to) => fs.renameSync(from, to),
  unlink: (file) => fs.unlinkSync(file),
  writeDurably: (file, text) => {
    const handle = fs.openSync(file, 'w');
    try {
      fs.writeFileSync(handle, text, 'utf8');
      fs.fsyncSync(handle);
    } finally {
      fs.closeSync(handle);
    }
  },
  syncDirectory: (directory) => {
    /*
     * A DIRECTORY THAT WILL NOT OPEN IS NOT AN ERROR HERE. Some file
     * systems refuse to open a directory for reading; the rename has
     * already happened and reporting this would turn a completed write
     * into a failure. What it costs is stated in the README rather than
     * hidden: on those systems the rename may not be durable across a
     * power loss.
     */
    let handle: number;
    try {
      handle = fs.openSync(directory, 'r');
    } catch (e) {
      return;
    }
    try {
      fs.fsyncSync(handle);
    } catch (e) {
      /* as above */
    } finally {
      fs.closeSync(handle);
    }
  },
  exists: (file) => fs.existsSync(file),
  presenceOf: (file) => {
    try {
      fs.statSync(file);
      return { known: true, there: true };
    } catch (e) {
      /*
       * ONLY ENOENT MEANS "NOT THERE". A path whose ancestry cannot be
       * searched, one that is not a directory where a directory was
       * expected, an i/o failure -- each of those is this machine being
       * unable to look, and nothing here can turn that into an answer.
       */
      const code = (e as NodeJS.ErrnoException).code;
      if (code === 'ENOENT' || code === 'ENOTDIR') {
        return { known: true, there: false };
      }
      return { known: false };
    }
  },
  link: (existing, fresh) => fs.linkSync(existing, fresh),
  list: (directory) => {
    try {
      return fs.readdirSync(directory);
    } catch (e) {
      return [];
    }
  },
  readDirectory: (directory) => {
    try {
      return { read: true, names: fs.readdirSync(directory) };
    } catch (e) {
      /*
       * ONLY ENOENT MEANS "NOT THERE". Everything else -- a permission
       * this process does not have, a path that is not a directory, an
       * i/o failure -- is this machine being unable to look, and the
       * conservative reading of that is that nothing here can judge.
       */
      const code = (e as NodeJS.ErrnoException).code;
      return { read: false, because: code === 'ENOENT' ? 'absent' : 'unreadable' };
    }
  },
  isDirectory: (file) => {
    try {
      return fs.statSync(file).isDirectory();
    } catch (e) {
      return false;
    }
  }
};
