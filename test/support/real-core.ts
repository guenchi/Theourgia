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
