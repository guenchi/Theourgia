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
 * A transport that answers with bytes the cell chooses.
 *
 * IT EXISTS FOR ANSWERS NO CORE WILL PRODUCE. Some shapes have to be
 * read back even though the store's own write predicate refuses to
 * create them -- a datum that arrived from an older store, or from a
 * peer that is not this core. A stand-in core is still a program being
 * asked to write them; this is the bytes themselves.
 */

import { RawResult, Transport } from '../../src/transport';

export class Says implements Transport {
  public readonly kind = 'says';
  private readonly stdout: string;
  public readonly sent: string[][] = [];

  constructor(stdout: string) {
    this.stdout = stdout;
  }

  public async send(verb: string, args: string[]): Promise<RawResult> {
    this.sent.push([verb, ...args]);
    return { argv: ['says', verb, ...args], rc: 0, stdout: this.stdout, stderr: '' };
  }
}
