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
 * WHEN THE USER IS TOLD THAT A STORE IS NOT SOUND.
 *
 * What the sentence says is `integrityNotice`'s business; this decides
 * whether to ask and whether to say it. It lives outside the extension
 * host so that a cell can drive it: when it sat inside `activate`, a
 * review showed that the whole of it could be switched off -- never
 * asking `check` at all -- with every cell in the tree still green.
 *
 * ONCE PER STORE PER SESSION, and only when there is something to say.
 *
 * KEY: A STORE IS MARKED ONLY WHEN THE USER WAS TOLD. A sound store is
 * not marked, and neither is one whose condition could not be read --
 * so switching back to it asks again, which costs one `check` and is
 * the only way a store that went bad during the session is ever seen.
 * Repeating a warning on every save is what teaches a person to stop
 * reading warnings, so a store that has been reported is not reported
 * again in this session.
 */

import { StoreVerdict } from './model';
import { Notice, integrityNotice } from './status';

/*
 * What one check needs from the window that asks it. `store` is the one
 * configured when the check starts; `generation` is read live, so that an
 * answer arriving after the window moved to another store is recognised.
 *
 * KEY: `ask`, `generation`, `show` AND `record` ARE CALLED AS METHODS OF
 * THE QUESTION, with the question as their receiver, so an implementation
 * may read `this`. (The extension's own functions do not.)
 *
 * NOTE: WHAT THE CALLER GUARANTEES, stated as what the product keeps rather
 * than as a contract for any caller (queue item 34, ruled L): `generation`
 * returns a number -- the extension's is a closure over one -- and `ask`'s
 * verdict has data properties -- the model builds it as a plain object from
 * the parsed answer. A `generation` that throws, or a verdict whose `known` is
 * a getter that throws, makes `check` reject, and no source of this product
 * produces either.
 *
 * KEY: `store` IS THE CONFIGURED STRING, AND IT IS THE KEY AS IT IS. Two
 * strings are two stores to the watch, even when they name one directory on
 * a case-insensitive file system; deciding that is not the watch's.
 */
export interface IntegrityQuestion {
  store: string;
  ask: () => Promise<StoreVerdict>;
  /*
   * NOTE: IT MAY RETURN A THENABLE (queue item 33): the editor's
   * `showWarningMessage` does. The watch never waits for it; it follows it,
   * once, to hear of a refusal (see `follow`).
   */
  show: (notice: Notice) => void | PromiseLike<unknown>;
  generation: () => number;
  /*
   * WHERE A WARNING THAT COULD NOT BE SHOWN IS WRITTEN DOWN: the extension's
   * output channel. Not another message -- what just failed is showing one.
   * It may return a thenable too, followed the same way and never awaited.
   */
  record: (line: string) => void | PromiseLike<unknown>;
}

/*
 * A THROWN VALUE, NAMED, AND NAMING IT NEVER THROWS. A name that could throw
 * inside the catch that is writing it down would lose the line it was for.
 * Review r1 of item 18 measured three values the first version threw on: a
 * proxy whose `getPrototypeOf` trap throws, a revoked proxy, and a function
 * whose `Symbol.toPrimitive` throws. So the whole of it is in a `try`, and a
 * value nothing can be read from is named as such.
 *
 * An Error is named by its `name` and `message`, each read on its own, since
 * either can be a getter that throws; only when neither can be read is it
 * named like any other object.
 */
const UNNAMEABLE = '(a value that could not be named)';

function errorText(value: object): string | null {
  const parts: string[] = [];
  try {
    const name: unknown = (value as { name?: unknown }).name;
    if (typeof name === 'string' && name.length > 0) {
      parts.push(name);
    }
  } catch {
    /*
     * NOTE: the name could not be read; the message may still be.
     */
  }
  try {
    const message: unknown = (value as { message?: unknown }).message;
    if (typeof message === 'string' && message.length > 0) {
      parts.push(message);
    }
  } catch {
    /*
     * NOTE: the message could not be read; the name, if read, stands alone.
     */
  }
  return parts.length > 0 ? parts.join(': ') : null;
}

export function describeValue(value: unknown): string {
  try {
    if (typeof value === 'bigint') {
      return `${value.toString()}n`;
    }
    if (typeof value === 'function') {
      return '(a function)';
    }
    if (typeof value === 'object' && value !== null) {
      if (value instanceof Error) {
        const text = errorText(value);
        if (text !== null) {
          return text;
        }
      }
      return Object.getPrototypeOf(value) === null ? '(an object with no prototype)' : '(an object)';
    }
    if (Object.is(value, -0)) {
      return '-0';
    }
    return String(value);
  } catch {
    return UNNAMEABLE;
  }
}

/*
 * THE ONE WAY A RETURNED VALUE IS READ (queue item 33, design D2).
 *
 * A fresh promise resolved with the value, and one `then` on it. That is a
 * single entry into the language's own promise resolution: a value that is
 * not a thenable resolves; a thenable's `then` is read and called once with
 * two one-shot callbacks, so its first terminal signal wins and every later
 * one is ignored; a `then` getter or call that throws is a rejection; a
 * thenable that never settles holds nothing open. NOT `Promise.resolve`: it
 * hands a native promise back unchanged, and its own `then` or
 * `constructor` would then be read by the watch itself.
 *
 * KEY: NOTHING ELSE OF THE VALUE IS READ, and that is the guarantee's
 * boundary: a promise the value's own `then` creates and returns, and the
 * original rejection of a native promise that refuses subscription, are the
 * value's, not the watch's (design D2 (a), (b)).
 *
 * KEY: THE FULFILMENT IS NOT PASSED THROUGH (design A5). With no fulfilment
 * callback, the promise `then` derives would resolve itself with the value
 * and read its `then` a second time; a getter that throws on that read
 * rejected the derived promise, which nobody holds -- an unhandled rejection
 * of the watch's own (measured, codex r11 F0). The fulfilment callback
 * returns `undefined`, so the derived promise settles with `undefined`, and
 * `onRejected` never throws, so it never rejects.
 */
function follow(returned: unknown, onRejected: (reason: unknown) => void): void {
  new Promise<unknown>((resolve) => resolve(returned)).then(() => undefined, onRejected);
}

export class IntegrityWatch {
  private readonly told = new Set<string>();

  /*
   * WRITING THE LINE NEVER THROWS AND NEVER WAITS (design D4). A synchronous
   * throw is swallowed -- a disposed channel throws "Channel has been
   * closed" -- and whatever `record` returns is followed and its rejection
   * swallowed: there is nowhere left to say either. It is never awaited, so
   * a line that never finishes neither delays the unmark nor keeps `check`
   * pending.
   */
  private write(on: IntegrityQuestion, thrown: unknown): void {
    try {
      follow(
        on.record(
          `Theourgia: the warning about ${on.store} could not be shown (${describeValue(thrown)}) ` +
            `at ${new Date().toISOString()}`
        ),
        () => undefined
      );
    } catch {
      /*
       * NOTE: NOTHING. The line could not be written either; the store is
       * left unmarked all the same, which is what brings the warning back.
       */
    }
  }

  public async check(on: IntegrityQuestion): Promise<void> {
    if (this.told.has(on.store)) {
      return;
    }
    const asked = on.generation();
    /*
     * A QUESTION THAT FAILED IS AN ANSWER NOBODY COULD READ, and it is
     * treated as one: nothing is said and the store is not marked, so it is
     * asked again. `storeVerdict` already turns a transport failure into
     * that; this is for any `ask` that rejects instead, whose rejection
     * would otherwise escape a call the extension does not wait for.
     *
     * NOTE: THE REJECTED VALUE IS NOT LOOKED AT. A rejection can carry any
     * value, and looking at it can throw: `String` on an object with no
     * prototype, `instanceof` on a proxy whose prototype trap throws, an
     * Error whose `message` getter throws. Three reviews in a row found one
     * more such value, each letting the rejection out of this catch after
     * all; a catch that reads nothing has no such value. The reason of an
     * unknown verdict is not shown to anybody, so a fixed sentence loses
     * nothing a person would read.
     */
    let verdict: StoreVerdict;
    try {
      verdict = await on.ask();
    } catch {
      verdict = { known: false, because: 'the question about this store failed' };
    }
    /*
     * AN ANSWER FOR A WINDOW THAT HAS MOVED ON is dropped, and the store
     * is not marked: nobody was told, so the next time it is opened it is
     * asked again.
     */
    if (asked !== on.generation()) {
      return;
    }
    /*
     * AND THE MARK IS READ AGAIN AFTER THE WAIT. Two checks of one store
     * can be waiting at once; the first to come back tells the user, and
     * the second would tell them again if the mark were only read before
     * it asked.
     */
    if (this.told.has(on.store)) {
      return;
    }
    const notice = integrityNotice(on.store, verdict);
    if (notice === null) {
      return;
    }
    /*
     * SHOWN, THEN MARKED. A `show` that throws has told nobody, and a mark
     * written before it would keep the store from being asked again for
     * the rest of the session.
     *
     * KEY: AND A `show` THAT THROWS DOES NOT ESCAPE. (queue item 18, ruled
     * 2026-09-25) Nobody waits for this call -- the extension discards its
     * promise -- so a throw here was an unhandled rejection. It is written
     * down on the output channel, the store is left unmarked so the next
     * check tells the user, and the check resolves. If writing it down
     * throws too, there is nowhere left to say it, and that is swallowed.
     */
    let returned: void | PromiseLike<unknown>;
    try {
      returned = on.show(notice);
    } catch (thrown) {
      this.write(on, thrown);
      return;
    }
    /*
     * KEY: TOLD IS THE HANDOVER, NOT THE DISMISSAL (design D1). The mark is
     * written when `show` returns, as it always was; what `show` returned is
     * then followed, not awaited -- awaiting it would let a second check of
     * the same store show again while the first waits (measured: 2 shown).
     *
     * A REFUSAL TAKES THE MARK BACK AND IS WRITTEN DOWN (D2, D3): tied to the
     * store alone, never to the generation, so a refusal that arrives after
     * the window moved away and back still unmarks. Each follow hears at most
     * one refusal, and a later show can only come after the unmark, so no
     * refusal can undo a later mark.
     */
    this.told.add(on.store);
    follow(returned, (reason) => {
      this.told.delete(on.store);
      this.write(on, reason);
    });
  }
}
