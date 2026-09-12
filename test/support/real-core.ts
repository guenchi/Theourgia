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
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { CoreConfig, DEFAULT_TIMEOUT_MS } from '../../src/config';
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
 * second another session wrote `store.ss`. Neither was a defect in the
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
  for (const name of fs.readdirSync(corePath).filter((f) => f.endsWith('.ss')).sort()) {
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
  if (!corePath || !fs.existsSync(path.join(corePath, 'cli.ss'))) {
    throw new Error(
      `the real core is needed for this cell and was not found. Set ${CORE_PATH_ENV} to the ` +
        'directory holding cli.ss (a checkout of https://github.com/guenchi/theourgia), and ' +
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

  private constructor(root: string, config: CoreConfig) {
    this.root = root;
    this.store = config.store;
    this.config = config;
    this.client = new Client(new CliTransport(config));
  }

  public static async make(actor = 'cell'): Promise<RealStore> {
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
    const config: CoreConfig = {
      scheme: where.scheme,
      corePath: where.corePath,
      libDirs: where.libDirs,
      store: path.join(root, 'store'),
      actor,
      timeoutMs: DEFAULT_TIMEOUT_MS,
      transport: 'cli'
    };
    const store = new RealStore(root, config);
    const started = await store.client.request('init', []);
    if (!started.ok) {
      throw new Error(`the core refused to make a store: ${started.text} ${started.stderr}`);
    }
    return store;
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

  public dispose(): void {
    fs.rmSync(this.root, { recursive: true, force: true });
  }
}
