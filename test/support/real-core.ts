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
 * A real store, built by the real core.
 *
 * WHEN THE CORE IS NOT THERE THESE CELLS FAIL. They do not skip. A suite
 * that skipped them would report the same green on a machine where the
 * end-to-end path has never once been run, and the two situations --
 * "this works" and "nobody has checked" -- would be one colour. What is
 * missing and how to supply it is said in the failure, so a red here is
 * actionable rather than merely loud.
 */

import { createHash } from 'crypto';
import { execFileSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
  CLIENT_PROGRAM,
  CoreConfig,
  DEFAULT_TIMEOUT_MS,
  environmentFor
} from '../../src/config';
import { coreDirectoryAt } from '../../src/fsops';
import { CliTransport } from '../../src/transport';
import { Client } from '../../src/client';
import { initWire } from '../../src/wire';

export const CORE_PATH_ENV = 'THEOURGIA_CORE';
export const LIBDIRS_ENV = 'THEOURGIA_LIBDIRS';
export const SCHEME_ENV = 'THEOURGIA_SCHEME';

export interface CoreLocation {
  corePath: string;
  libDirs: string[];
  scheme: string;
}

/*
 * WHAT THE CORE WAS WHEN THIS RAN.
 *
 * THE CORE IS SOMEBODY ELSE'S WORKING TREE unless it is deliberately
 * frozen, and a suite that reads one is only as stable as the editing
 * going on in it. That is not a hypothetical: a run of these cells once
 * reported `the core exited 255 without an answer`, and a probe built to
 * fish for it caught `Exception: variable t is not bound` at the exact
 * second another session wrote the `store` source (a `.ss` file then). Neither was a defect in the
 * core -- both were a half-written file being read.
 *
 * SO THE DIGEST IS TAKEN AT THE START AND CHECKED AT THE END. A failure
 * that arrives with "the core changed while this ran" is a reading to
 * throw away; one that arrives without it is about the code. Point
 * THEOURGIA_CORE at a copy nobody is editing and this never fires.
 */
export interface CoreDigest {
  /*
   * TWO DIGESTS, BECAUSE THE TWO ANSWERS ARE DIFFERENT NEWS. `bytes` is
   * what the core says; `stamped` also covers size and modification
   * time, which is what catches the case this guard exists for -- a file
   * written and then restored between the two readings has the same
   * bytes at both ends and was a different file in between.
   *
   * KEEPING THEM APART IS WHAT STOPS THE GUARD BEING IGNORED. A checkout
   * or a re-export rewrites every timestamp without changing a byte, and
   * a guard that reported that as "the core changed" in the same words
   * it uses for a real edit would be trained away within a day.
   */
  bytes: string;
  stamped: string;
}

export function coreDigest(corePath: string): CoreDigest {
  const bytes = createHash('sha256');
  const stamped = createHash('sha256');
  /*
   * NEVER: NOT ONLY THE SOURCES. This listed `.ss` alone, and a core
   * directory may be a PRODUCT directory -- the compiled libraries beside
   * the two scripts -- which this extension supports and a cell may run
   * against. Measured in a review round: with `client.so` and `rpc.so`
   * rewritten between the two readings, the pin reported no change at
   * all. What the pin is for is saying whether the readings were taken
   * against a moving tree, and in that directory form the loaded code is
   * exactly what it was not looking at.
   *
   * `.sc` is here for the renaming the core has ruled, so that the pin
   * does not silently narrow on the day it lands.
   */
  const interesting = (name: string): boolean =>
    name.endsWith('.ss') || name.endsWith('.so') || name.endsWith('.sc');
  for (const name of fs.readdirSync(corePath).filter(interesting).sort()) {
    const content = fs.readFileSync(path.join(corePath, name));
    const stat = fs.statSync(path.join(corePath, name));
    bytes.update(name);
    bytes.update(content);
    stamped.update(name);
    stamped.update(String(stat.size));
    stamped.update(String(stat.mtimeMs));
    stamped.update(content);
  }
  return { bytes: bytes.digest('hex').slice(0, 16), stamped: stamped.digest('hex').slice(0, 16) };
}

export interface CorePin {
  corePath: string;
  digest: CoreDigest;
}

export function pinCore(): CorePin {
  const where = locateCore();
  return { corePath: where.corePath, digest: coreDigest(where.corePath) };
}

export function checkCorePin(pinned: CorePin | undefined): void {
  /*
   * A PIN THAT WAS NEVER TAKEN IS NOT A PIN THAT FAILED. When the setup
   * itself threw -- no core at that path -- this runs with nothing to
   * compare, and dereferencing it would replace an actionable setup
   * failure with a TypeError about the check.
   */
  if (pinned === undefined) {
    return;
  }
  const now = coreDigest(pinned.corePath);
  if (now.bytes !== pinned.digest.bytes) {
    throw new Error(
      `the core at ${pinned.corePath} was EDITED while these cells ran ` +
        `(${pinned.digest.bytes} -> ${now.bytes}). Every reading in this file was taken against a ` +
        'moving tree and none of them mean anything. Point THEOURGIA_CORE at a copy nobody is ' +
        'editing and run again.'
    );
  }
  if (now.stamped !== pinned.digest.stamped) {
    throw new Error(
      `the core at ${pinned.corePath} was REWRITTEN while these cells ran: the bytes are the same ` +
        `(${now.bytes}) and the files are not (${pinned.digest.stamped} -> ${now.stamped}). That is ` +
        'a checkout, a re-export, or an edit that was undone -- in the last case a request in the ' +
        'middle of this run saw something neither reading shows. Take the copy out of reach of ' +
        'whatever rewrote it and run again.'
    );
  }
}

export function locateCore(): CoreLocation {
  const corePath = process.env[CORE_PATH_ENV];
  /*
   * NOTE: `theourgia.sc` (CLIENT_PROGRAM) IS THE ONE THAT MUST BE THERE. Both forms of a
   * core directory hold it -- a checkout beside its library sources, a
   * product directory beside the compiled ones -- and it is what the
   * shipping transport runs (build.ss copies it into a product directory,
   * :175-195). Asking only for the old `cli` entry point let a directory
   * with no thin client in it pass this check and fail later, inside
   * Chez, about a library.
   */
  if (!corePath || !fs.existsSync(path.join(corePath, CLIENT_PROGRAM))) {
    throw new Error(
      `the real core is needed for this cell and was not found. Set ${CORE_PATH_ENV} to the ` +
        `directory holding ${CLIENT_PROGRAM} -- a checkout of ` +
        'https://github.com/guenchi/theourgia, or a directory its build.ss produced -- and ' +
        `${LIBDIRS_ENV} to a colon-separated list of directories holding igropyr. ` +
        `${CORE_PATH_ENV} is currently ${corePath ?? 'unset'}.`
    );
  }
  const libDirs = (process.env[LIBDIRS_ENV] ?? '').split(':').filter((d) => d.length > 0);
  return { corePath, libDirs, scheme: process.env[SCHEME_ENV] ?? 'scheme' };
}

let counter = 0;

export class RealStore {
  public readonly root: string;
  public readonly store: string;
  public readonly config: CoreConfig;
  public readonly client: Client;
  public readonly runRoot: string;
  public readonly ownsRunRoot: boolean;
  /*
   * NOTE: THE ENVIRONMENT IS PUBLIC BECAUSE CELLS BUILD THEIR OWN
   * TRANSPORTS. Several wrap one in a counter or in a stand-in that
   * loses the answer, and every `new CliTransport(store.config)` written
   * without this took `process.env` -- which names no run root, so the
   * daemon went to the user's. Measured: with only the client inside
   * this class fixed, a full unit run still left three daemons and three
   * directories in `~/.theourgia/run`. KEY: Use `transport()` below rather
   * than this; it is exported so that a wrapper can be handed one that
   * is already right.
   */
  public readonly env: NodeJS.ProcessEnv;

  private constructor(root: string, config: CoreConfig, runRoot: string, ownsRunRoot: boolean) {
    this.root = root;
    this.store = config.store;
    this.config = config;
    this.runRoot = runRoot;
    this.ownsRunRoot = ownsRunRoot;
    /*
     * NOTE: THE FIXTURE OWNS EVERY PATH THE PRODUCT COMPUTES.
     *
     * The shipping transport starts a daemon, and the daemon's socket
     * goes under `THEOURGIA_RUN` -- which, unset, is the user's own
     * `~/.theourgia/run`. Measured here before this was written: one
     * unit run left THIRTEEN directories and thirteen live daemons in
     * it. The core's line found the same hole in six of its fixtures
     * and settled the rule this follows: every variable the product
     * reads is set to somewhere this fixture owns, and both this
     * process and the children it starts get it.
     */
    this.env = {
      ...process.env,
      THEOURGIA_RUN: runRoot,
      THEOURGIA_HOME: path.join(root, 'home')
    };
    this.client = new Client(this.transport());
  }

  /*
   * A TRANSPORT ONTO THIS STORE, with this store's directories. NEVER: Not
   * `new CliTransport(store.config)`: that spelling is the one that
   * leaked, and it is still the shorter thing to write, so what keeps it
   * out is the gate in run-root.test.ts rather than anybody's memory.
   */
  public transport(): CliTransport {
    return new CliTransport(this.config, this.env);
  }

  /*
   * NOTE: A SUPPLIED RUN ROOT IS SHARED AND IS NOT THIS STORE'S TO DELETE.
   * The editor-hosted cells pass the one the launcher gave the extension
   * host, because the extension and the fixture must reach ONE daemon
   * for one store: two roots produce two sockets for the same key, two
   * daemons open the store, and what that looks like from a cell is a
   * refusal nobody can place.
   */
  public static async make(
    actor = 'cell',
    options: { runRoot?: string } = {}
  ): Promise<RealStore> {
    /*
     * THE READER IS LOADED HERE RATHER THAN BY EVERY CALLER. It is an
     * ES module and arrives through a promise, so a caller that forgot
     * would fail at the first answer with a message about loading
     * order rather than about the store.
     */
    await initWire();
    const where = locateCore();
    counter += 1;
    const root = fs.mkdtempSync(path.join(os.tmpdir(), `theourgia-real-${process.pid}-${counter}-`));
    /*
     * NOTE: SHORT, AND NOT UNDER `root`. A unix socket name may be 104
     * bytes and the path is this root plus a sixteen-character key plus
     * `/socket`; a macOS temporary directory name is long enough on its
     * own that nesting one more level pushes past it. The pid and the
     * counter keep two runs and two stores apart, which is also what
     * lets the teardown below tell this store's daemon from anybody
     * else's.
     */
    const ownRoot = options.runRoot === undefined;
    const runRoot = options.runRoot ?? path.join(os.tmpdir(), `tvr-${process.pid}-${counter}`);
    fs.mkdirSync(runRoot, { recursive: true });
    fs.mkdirSync(path.join(root, 'home'), { recursive: true });
    const config: CoreConfig = {
      scheme: where.scheme,
      corePath: where.corePath,
      libDirs: where.libDirs,
      store: path.join(root, 'store'),
      actor,
      writer: '',
      timeoutMs: DEFAULT_TIMEOUT_MS
    };
    const store = new RealStore(root, config, runRoot, ownRoot);
    /*
     * NEVER: A SETUP THAT FAILS MUST NOT LEAVE A DAEMON BEHIND.
     *
     * Found by an outside review. `init` is a request, and a request is
     * what starts a daemon -- so a refusal here left one running with
     * nothing holding a reference to stop it: the caller never gets the
     * store, and every teardown in this suite is written `store?.
     * dispose()`. The suite-wide hook would have reported it and could
     * not have stopped it, because it asserts rather than kills.
     *
     * The failure is re-raised unchanged; only the cleanup is added.
     */
    let started;
    try {
      started = await store.client.request('init', []);
    } catch (e) {
      store.dispose();
      throw e;
    }
    if (!started.ok) {
      store.dispose();
      throw new Error(`the core refused to make a store: ${started.text} ${started.stderr}`);
    }
    return store;
  }

  /*
   * THE CORE, RUN DIRECTLY, FOR WORK THAT IS THE FIXTURE'S AND NOT THE
   * PRODUCT'S.
   *
   * NOTE: A CELL THAT BUILDS A SECOND STORE HAS TO DRIVE THAT STORE, and
   * the extension's client is bound to one. Adopting a copy and
   * publishing a segment out of it are things a person does with the
   * command line; putting them through the client under test would mean
   * teaching it verbs it has no reason to have.
   *
   * It runs with this fixture's own environment, so the second store's
   * daemon lands under this fixture's run root and is stopped with the
   * rest.
   */
  public cli(args: string[]): string {
    /*
     * NOTE: THROUGH `environmentFor`, NOT WITH THE RAW ENVIRONMENT. The
     * library directories and the extension list are computed there,
     * from the core directory's own form -- sources or products -- and a
     * call that passed the bare environment got
     * `Exception: library (theourgia client) not found`, which reads
     * like a broken core rather than like a missing variable.
     */
    return execFileSync(
      this.config.scheme,
      ['--script', path.join(this.config.corePath, CLIENT_PROGRAM), ...args],
      {
        env: environmentFor(this.config, this.env, coreDirectoryAt(this.config.corePath)),
        encoding: 'utf8'
      }
    );
  }

  /*
   * A DOCUMENT IS IMPORTED RATHER THAN INSERTED, because only an
   * imported block carries a `heading-src`, and the heading is what the
   * save path takes apart. A store built with `insert` would exercise
   * the headingless branch and never the other one.
   */
  public async importMarkdown(name: string, text: string): Promise<void> {
    const from = path.join(this.root, 'import');
    fs.mkdirSync(from, { recursive: true });
    fs.writeFileSync(path.join(from, name), text, 'utf8');
    const answer = await this.client.request('import-md', [from]);
    if (!answer.ok) {
      throw new Error(`import-md refused: ${answer.text} ${answer.stderr}`);
    }
  }

  /*
   * NOTE: THE DAEMON THIS STORE STARTED IS STOPPED BY PID, AND THEN THE
   * STOPPING IS CHECKED.
   *
   * "Sent a signal" is not "it went": the core's line wrote a teardown
   * that deleted the file a daemon was writing and never signalled the
   * daemon at all, and what caught it was counting processes
   * afterwards. The daemons are found by this store's own run root,
   * which no other process can be using -- NEVER: not by a pattern like
   * `scheme`, which matches this line's own commands and, on this
   * machine, a dozen editor helpers whose arguments contain the word.
   */
  public dispose(): void {
    /*
     * NOTE: EVERY DAEMON THIS FIXTURE'S DIRECTORY GAVE RISE TO, not only
     * the one for its store.
     *
     * A cell may build a SECOND store under this root -- a copy of the
     * first, adopted, so that the store has more than one log writer --
     * and that copy gets a daemon of its own. Scoping the teardown to
     * `this.store` left it running: measured, one process per run of the
     * cell that does it, invisible to the fixture and caught only by the
     * gate over the whole suite. `this.root` is a temporary directory
     * this fixture made and nothing else can be under it.
     */
    stopDaemonsFor(this.root);
    fs.rmSync(this.root, { recursive: true, force: true });
    if (this.ownsRunRoot) {
      fs.rmSync(this.runRoot, { recursive: true, force: true });
    }
  }
}


/*
 * EVERY PROCESS WHOSE ARGUMENTS NAME THIS STORE'S SOCKET DIRECTORY.
 *
 * NOTE: THE MATCH IS THE RUN ROOT, NOT THE WORD "scheme". On this machine
 * `ps -ax -o command | grep scheme` returns a dozen VS Code helper
 * processes -- their argument lists carry `--standard-schemes=` and
 * half a dozen more -- and a teardown built on that would report them
 * as leaked daemons and try to kill them.
 *
 * NEVER: AND IT IS NOT THE WORD "scheme" FOR A SECOND REASON, which is the
 * one that would have hurt: the core spells the interpreter as
 * `THEOURGIA_SCHEME` or `scheme` (`scheme-binary`, theourgia.sc:142), so on a machine that
 * sets that variable to `chez` this filter would exclude the daemon it
 * exists to find and the teardown would quietly do nothing -- and
 * report a clean nought processes left while doing it.
 *
 * What is matched instead is what the core actually writes into the
 * daemon's argument list: `--socket <run root>/<store key>/socket`
 * (`server-argv`, theourgia.sc:377-381). Both parts are required, because the run
 * root on its own would also match the client process that is asking
 * for one on the rare occasion the paths are spelled out.
 */
export function daemonsMatching(...needles: string[]): number[] {
  let listing: string;
  try {
    listing = execFileSync('ps', ['-ax', '-o', 'pid=,command='], { encoding: 'utf8' });
  } catch (e) {
    /*
     * NEVER: A LOOK THAT FAILED IS NOT A LOOK THAT FOUND NOTHING.
     *
     * This returned an empty list, so a `ps` that could not be run made
     * `stopDaemonsFor` signal nobody and report `{asked: 0, left: 0}`,
     * and made the suite-wide gate announce that nothing had leaked.
     * Measured in an eighth review round with `execFileSync` throwing
     * EACCES: every reading came back clean while daemons went on
     * running. This is the instrument the whole hygiene story rests on,
     * and the one answer it must never give is the reassuring one.
     */
    throw new Error(
      'could not list the processes on this machine, so nothing here can say whether a daemon ' +
        `is running: ${(e as Error).message}`
    );
  }
  const found: number[] = [];
  for (const line of listing.split('\n')) {
    const match = /^\s*(\d+)\s+(.*)$/.exec(line);
    if (match === null) {
      continue;
    }
    const pid = Number(match[1]);
    const command = match[2];
    if (pid === process.pid || !command.includes('--socket')) {
      continue;
    }
    if (!needles.every((needle) => command.includes(needle))) {
      continue;
    }
    found.push(pid);
  }
  return found;
}

export function daemonsUnder(runRoot: string): number[] {
  return daemonsMatching(runRoot);
}

/*
 * NOTE: SCOPED TO ONE STORE, NOT TO A DIRECTORY.
 *
 * A run root may be SHARED -- the editor-hosted cells hand their stores
 * the same root the extension under test was given, because two roots
 * for one store means two sockets and two daemons opening it. Stopping
 * everything under the root would then have one store's teardown killing
 * another store's daemon, which is a defect this line would then spend a
 * morning on. The store path is in the daemon's own argument list
 * (`serve <store>`, `server-argv` at theourgia.sc:377-381) and is unique per fixture.
 */
/*
 * THE SOCKET PATH A RUNNING DAEMON WAS GIVEN, read off its own argument
 * list rather than composed here.
 *
 * NOTE: THE NAME UNDER THE RUN ROOT IS A DIGEST OF THE STORE'S REAL PATH
 * (the rule at client.sc:76, `store-key` at :228) -- resolved through symlinks, with the components
 * below the longest existing prefix appended. A cell that worked it out
 * for itself would be a second implementation of that rule, and the one
 * that was wrong would create an obstruction in a directory the core is
 * not using and then report that nothing went wrong.
 */
export function socketPathOf(storePath: string): string | null {
  const listing = execFileSync('ps', ['-ax', '-o', 'pid=,command='], { encoding: 'utf8' });
  for (const line of listing.split('\n')) {
    if (!line.includes(storePath) || !line.includes('--socket')) {
      continue;
    }
    const found = /--socket\s+(\S+)/.exec(line);
    if (found !== null) {
      return found[1];
    }
  }
  return null;
}

export function stopDaemonsFor(storePath: string): { asked: number; left: number } {
  const ours = daemonsMatching(storePath);
  for (const pid of ours) {
    try {
      process.kill(pid, 'SIGKILL');
    } catch (e) {
      /* already gone; the count below is what decides */
    }
  }
  return { asked: ours.length, left: daemonsMatching(storePath).length };
}
