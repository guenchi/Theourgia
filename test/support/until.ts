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
 * WAITING FOR SOMETHING THAT CAN BE SEEN, rather than for a number of
 * milliseconds.
 *
 * NEVER: A DURATION IS NOT AN OBSERVATION OF ANYTHING. Too short and the
 * cell asserts before the work; too long and it asserts after work the
 * cell did not ask for -- one cell here slept while a background drain
 * it did not know about finished, and then asserted that a save was
 * still stranded after the extension had quietly got it through.
 *
 * NOTE: AND THE STARTING POINT IS PINNED FIRST. "Wait until the log is
 * not empty" is satisfied for ever by a log that was not empty to begin
 * with. Every caller reads the count BEFORE acting and waits for a
 * number greater than that one.
 *
 * KEY: ONE COPY, TWO SUITES. This lived in `extension.test.ts` while
 * `generation.test.ts` slept; the second file now waits the same way
 * rather than growing a second version of this that would be the one
 * nobody fixed.
 */

export const PATIENCE_MS = 30000;

export async function settle(ms = 250): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

export async function until(
  what: string | (() => string),
  ready: () => Promise<boolean> | boolean
): Promise<void> {
  const deadline = Date.now() + PATIENCE_MS;
  for (;;) {
    if (await ready()) {
      return;
    }
    if (Date.now() >= deadline) {
      /*
       * THE MESSAGE MAY BE A FUNCTION, so that a cell can put the LAST
       * reading into it. A timeout that says only what was hoped for
       * leaves the reader to guess what was actually there, which is the
       * one thing the waiting loop was in a position to know.
       */
      throw new Error(
        `waited ${PATIENCE_MS} ms and ${typeof what === 'function' ? what() : what} never happened`
      );
    }
    await settle(50);
  }
}
