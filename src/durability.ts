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
 * THE WINDOW'S DURABILITY SINK. (queue item 22, design v4)
 *
 * A queue write whose rename landed and whose directory flush failed counts
 * as done, and says that the queue may not survive a power cut. The awaited
 * save carries its own entry's sentence on its outcome and a takeover
 * carries its own in its report; every other such sentence -- from a drain
 * settling somebody else's save, a settle, a retry, a configuration change
 * -- is handed here, and the extension shows what is here after each
 * command and save.
 *
 * NOTE: ONE PER WINDOW, OWNED BY THE EXTENSION, NOT BY A SAVER. A settings
 * change replaces the Saver while an old one may still have a request in
 * flight; a sink the Saver owned would be dropped with it, and the warning
 * its late answer raises with it (K4, D4b).
 *
 * NOTE: ONE SENTENCE PER QUEUE FILE UNTIL IT IS TAKEN, the latest kept
 * (K5). Several writes to one file between two notices say one thing: that
 * file may not survive a power cut; the latest reason is the one to read.
 */
/*
 * THE ORDER IN WHICH WARNINGS WERE PRODUCED, for this process. (queue item 46)
 * A counter, not a clock: two warnings in one millisecond are still two, and
 * the order is the same on every run. A warning reported the moment it is
 * produced takes its stamp on arrival; one held back on a drain's outcome
 * took its stamp when it was produced and carries it (see `Saver.kept`).
 */
let produced = 0;

export function nextStamp(): number {
  produced += 1;
  return produced;
}

export class DurabilitySink {
  private readonly held = new Map<string, { text: string; at: number }>();

  /*
   * THE LATEST PER FILE BY WHEN IT WAS PRODUCED, NOT BY WHEN IT ARRIVED.
   * (queue item 46, from item 22's review r1, L1) A warning held on an
   * outcome reaches the sink after the settle has reported a newer one
   * straight away; kept by arrival, the older reason overwrote the newer.
   * `at` is the warning's production stamp; left out, the warning is taken as
   * produced now.
   */
  public add(file: string, text: string, at: number = nextStamp()): void {
    const was = this.held.get(file);
    if (was === undefined || at > was.at) {
      this.held.set(file, { text, at });
    }
  }

  /*
   * EVERYTHING HELD, AND NOTHING HELD AFTERWARDS: what is returned is shown
   * once.
   */
  public take(): string[] {
    const texts = [...this.held.values()].map((held) => held.text);
    this.held.clear();
    return texts;
  }

  /*
   * TAKE EVERYTHING HELD AND SHOW EACH TEXT, EACH ON ITS OWN. (delivery review
   * r2 of item 22, the W, ruled: fix now) `take` has already cleared the batch,
   * so a `show` that throws for one text must not take the rest with it -- that
   * would be a warning lost after all. A failure goes to `failed` and the loop
   * carries on.
   *
   * NOTE: AND A SHOW THAT FAILS LATER IS FOLLOWED TOO. (delivery review r3 of
   * item 22, the W) The editor answers a notice with a thenable, and one that
   * rejects was left for nobody -- an unhandled rejection, and the failure
   * never reached `failed`. It is followed in the form integrity.ts uses
   * (queue item 33, design A5): a fresh promise resolved with the value and
   * one `then`, whose fulfilment callback returns nothing. `failed` must not
   * throw.
   */
  public showAll(show: (text: string) => unknown, failed: (error: unknown) => void): void {
    for (const text of this.take()) {
      try {
        const shown = show(text);
        new Promise<unknown>((resolve) => resolve(shown)).then(() => undefined, failed);
      } catch (error) {
        failed(error);
      }
    }
  }
}
