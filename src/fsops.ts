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
 * Projection, sidecar and queue file operations, named in one place.
 *
 * IT EXISTS SO THAT WHAT THIS PROCESS DID TO A PATH CAN BE ASKED. The
 * current-file design installs complete body bytes with one rename,
 * and the way to hold a build to that is to count what it actually
 * called -- a claim about
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
 * `fsops.test.ts` refuses other TypeScript product files importing fs.
 * The stable native lock below has separate control IO, outside this
 * recorder. Its open/flock/close protocol is tested with real processes;
 * FileOps traces do not measure those native calls.
 */

import * as fs from 'fs';
import * as path from 'path';
import { CoreDirectory } from './config';
import {createHash} from 'crypto';

export interface FileOps {
  readText(file: string): string;
  /*
   * THE BYTES, UNDECODED. The save handler has to know whether what is
   * on disk carries a byte-order mark and whether it is strictly UTF-8;
   * `readText` has already decided both, lossily, and a string cannot
   * be asked what it used to be. (section 12.17.3)
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
  /*
   * FLUSH A DIRECTORY, AND SAY WHY NOT WHEN IT COULD NOT. Null when it was
   * flushed, and also when the directory would not even open for reading
   * -- some file systems refuse that, the README says so, and reporting it
   * would warn on every write there. A sentence when the flush itself
   * failed: until queue item 3 that was swallowed here too, because a
   * failure then could only turn a completed write into a failed one. Now
   * the caller decides -- `Outbox.enqueue` carries it as a durability
   * warning; every other caller ignores the value and is silent exactly as
   * before.
   */
  syncDirectory(directory: string): string | null;
  exists(file: string): boolean;
  /*
   * NOTE: THE HONEST FORM OF `exists`, FOR THE CALLER THAT REPORTS WHAT IT
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
   * windows to race in. (section 12.13.5)
   */
  link(existing: string, fresh: string): void;
  list(directory: string): string[];
  /*
   * NOTE: THE HONEST FORM OF `list`, FOR THE ONE CALLER THAT CANNOT AFFORD
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
      return null;
    }
    try {
      fs.fsyncSync(handle);
      return null;
    } catch (e) {
      /*
       * THE FLUSH FAILED, AND THAT IS SAID, NOT SWALLOWED. The rename has
       * happened either way; what this returns is only what could not be
       * promised about surviving a power cut.
       */
      return e instanceof Error ? e.message : String(e);
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
  /*
   * NEVER: ONLY AN ABSENT DIRECTORY IS AN EMPTY ONE, and this is the
   * product, not a fixture.
   *
   * It caught every error and answered with no names. Measured in a
   * twelfth review round with EACCES injected for the sessions root:
   * `chooseAndRecover` answered `no-other-sessions` and told the user
   * "No other window has left anything here ... there is none" -- about
   * a directory it had not been able to open. The unsent work of another
   * window is exactly what that sentence is denying the existence of.
   *
   * `readDirectory` below already draws this line; `list` is the older
   * spelling beside it and did not.
   */
  list: (directory) => {
    try {
      return fs.readdirSync(directory);
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
        return [];
      }
      throw e;
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
  /*
   * NEVER: A STAT THAT FAILED IS NOT A PATH THAT IS NOT A DIRECTORY.
   *
   * It answered false for every error, and the recovery listing skips
   * anything that answers false. Measured in a thirteenth review round
   * with EACCES on one enumerated session: the window vanished and
   * `chooseAndRecover` said "No other window has left anything here" --
   * the same sentence, from the same cause, one repair later. `list`
   * above was fixed in the previous round and this sits four lines
   * below it.
   *
   * Only ENOENT is an answer: the path is not there, so it is not a
   * directory.
   */
  isDirectory: (file) => {
    try {
      return fs.statSync(file).isDirectory();
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
        return false;
      }
      throw e;
    }
  }
};


interface NativeLease { acquire(file:string):number; release(fd:number):void; }
let nativeLease:NativeLease|undefined;
const heldResources=new Map<string,number>();

export function controlDirectory(directory:string): string {
  return path.join(path.dirname(directory),'.block-control',path.basename(directory));
}

function canonicalResource(resource:string):string {
  let base=path.resolve(resource);const suffix:string[]=[];
  for (;;) {
    try {return path.join(fs.realpathSync(base),...suffix);}
    catch (error) {
      if ((error as NodeJS.ErrnoException).code!=='ENOENT') throw error;
      const parent=path.dirname(base);if(parent===base)throw error;
      suffix.unshift(path.basename(base));base=parent;
    }
  }
}

// The callback is synchronous: no human or network wait holds this resource.
// Reentry is confined to the same JavaScript call stack, never an async owner.
export function withExclusive<T>(resource:string,work:()=>T):T {
  const resolved=canonicalResource(resource);
  const marker=`${path.sep}sessions${path.sep}`,at=resolved.lastIndexOf(marker);
  const canonical=at<0?resolved:resolved.slice(0,at+marker.length)+resolved.slice(at+marker.length).split(path.sep)[0];
  if(heldResources.has(canonical))return work();
  const sessionMarker=`${path.sep}sessions${path.sep}`;
  const sessionAt=canonical.lastIndexOf(sessionMarker);
  const directory=sessionAt>=0?path.join(canonical.slice(0,sessionAt),'.resource-locks'):
    path.join(path.dirname(canonical),'.resource-locks');
  fs.mkdirSync(directory,{recursive:true});
  const lock=path.join(directory,createHash('sha256').update(canonical).digest('hex')+'.lock');
  nativeLease ??= require('./native/lease.node') as NativeLease;
  let descriptor:number;
  try {descriptor=nativeLease.acquire(lock);}catch(error){
    if(error instanceof Error)error.message+=`: ${canonical}. Retry after the other operation finishes.`;
    throw error;
  }
  heldResources.set(canonical,descriptor);
  try {
    const result=work();
    if(result && typeof (result as {then?:unknown}).then==='function')throw new Error('A synchronous resource guard cannot enclose an asynchronous operation');
    return result;
  } finally {heldResources.delete(canonical);nativeLease.release(descriptor);}
}

/*
 * WHAT A CORE DIRECTORY HOLDS, asked through the one module that is
 * allowed to touch the file system.
 *
 * NOTE: IT LIVES HERE AND NOT BESIDE THE RULE IT FEEDS. `config.ts` is a
 * plain-value module on purpose -- a cell runs the transport with no
 * extension host and no disk around it -- and a census in this tree
 * holds every other source file to importing `fs` only through here. I
 * put the probe in `config.ts` first and that census caught it.
 */
export function coreDirectoryAt(corePath: string): CoreDirectory {
  return {
    has: (name: string) => {
      try {
        return fs.existsSync(path.join(corePath, name));
      } catch (e) {
        /*
         * A directory this process may not search answers the same as
         * one that does not hold the file. The caller that cares about
         * the difference is `problemsWith`, whose answer -- "neither
         * sources nor products" -- is a sentence a user can act on
         * either way.
         */
        return false;
      }
    }
  };
}
