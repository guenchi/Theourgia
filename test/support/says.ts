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
 * IT EXISTS FOR ANSWERS THE BYTES ARE EASIER TO STATE THAN TO CAUSE.
 * Some shapes take a store and several verbs to produce, and a cell
 * that only wants to know what this client does with the bytes should
 * not have to build one. Where the shape's REACHABILITY is the point,
 * a real store is used instead and this is not a substitute for it.
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
