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
 * A transport that lets the request through and loses the answer.
 *
 * THIS IS THE FAILURE THE OUTBOX EXISTS FOR, and it is the one a test
 * cannot produce by refusing to send: the record HAS to reach the store,
 * or the retry would find nothing to replay and the cell would pass
 * against a store that was never written to. So the real transport runs,
 * the core finishes, and only then is the answer dropped -- which is
 * what a host killed between the write and the answer leaves behind.
 */

import { RawResult, Transport, TransportError } from '../../src/transport';

export class LosesTheAnswer implements Transport {
  public readonly kind = 'cli-losing-answers';
  private readonly inner: Transport;
  private readonly loseWhen: (verb: string, args: string[]) => boolean;
  public readonly delivered: string[][] = [];

  constructor(inner: Transport, loseWhen: (verb: string, args: string[]) => boolean) {
    this.inner = inner;
    this.loseWhen = loseWhen;
  }

  public async send(verb: string, args: string[], input?: string): Promise<RawResult> {
    const result = await this.inner.send(verb, args, input);
    if (!this.loseWhen(verb, args)) {
      return result;
    }
    this.delivered.push([verb, ...args]);
    throw new TransportError(
      'no-answer',
      'the answer was lost after the core had finished',
      result.stdout
    );
  }
}
