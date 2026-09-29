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
 * A transport that counts what went out and changes nothing else.
 *
 * WHAT A STORE RECORDS IS NOT WHAT A CLIENT SENT. A tracked write sent
 * twice appends one record -- that is what the request id is for -- so a
 * cell that counts records cannot see a client that sends the same
 * request twice, and the stand-in cells that count requests would catch
 * a regression the real-core ones would pass. Counting here, at the
 * point where the bytes leave, is the only place the two questions come
 * apart.
 */

import { RawResult, Transport } from '../../src/transport';

export class Counting implements Transport {
  public readonly kind: string;
  private readonly inner: Transport;
  public readonly sent: string[][] = [];

  constructor(inner: Transport) {
    this.inner = inner;
    this.kind = `counting-${inner.kind}`;
  }

  public send(verb: string, args: string[], input?: string): Promise<RawResult> {
    this.sent.push([verb, ...args]);
    return this.inner.send(verb, args, input);
  }

  public countOf(verb: string): number {
    return this.sent.filter((c) => c[0] === verb).length;
  }

  public forget(): void {
    this.sent.length = 0;
  }
}
