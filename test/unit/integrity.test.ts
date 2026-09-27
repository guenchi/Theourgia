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
 * plugin-r3 item 14: the store says it is not sound, and the user is told.
 *
 * The decision lives in pieces a cell can drive -- `verdictOf` reads the
 * answer, `integrityNotice` decides what to say, `IntegrityWatch` decides
 * whether to ask and whether to say it -- and one method that puts the
 * question through a real client, which is where the exit code of a
 * damaged store would otherwise be misread.
 *
 * NOTE: WHAT HAS NO RUNTIME CELL, said rather than left to be assumed:
 * the wiring in `extension.ts` that hands `IntegrityWatch` the window. It
 * is right when three things hold -- `rebuild` calls it, the watch is built
 * once for the session, and the generation it is handed is the live one --
 * and every cell here stays green when any of them is broken. The first
 * two are read from the source by a tripwire in `awaiting.test.ts`; the
 * third is held by nothing that runs and is named in the delivery note.
 */

import * as assert from 'assert';
import { spawnSync } from 'child_process';
import * as path from 'path';
import { Client } from '../../src/client';
import { StoreVerdict, StoreModel, verdictOf } from '../../src/model';
import { IntegrityQuestion, IntegrityWatch, describeValue } from '../../src/integrity';
import { Notice, integrityNotice } from '../../src/status';
import { CliTransport } from '../../src/transport';
import { initWire, parseAnswers } from '../../src/wire';
import { FakeCore } from '../support/fake';

const CHECK = (verdict: string): string =>
  `(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) ` +
  `(registry outside-store) (notes ()) ${verdict})`;

describe('plugin-r3 a store that says it is not sound', function () {
  /*
   * ITS CELLS START A CORE PROCESS PER REQUEST -- the stand-in,
   * test/fake-core.js, run as the configured `scheme` -- so they get the
   * timeout the suites that start processes have (queue item 29; the queue's
   * wording said the real core, which this describe does not start): under
   * load a cell here timed out at mocha's two seconds, and a red that comes
   * from load reads like a kill in a mutation table.
   */
  this.timeout(120000);
  let core: FakeCore | undefined;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('reads the verdict a check answer states', () => {
    assert.deepStrictEqual(verdictOf(parseAnswers(CHECK('(verdict ok)'))), { known: true, verdict: 'ok' });
    assert.deepStrictEqual(verdictOf(parseAnswers(CHECK('(verdict damaged)'))), {
      known: true,
      verdict: 'damaged'
    });
  });

  /*
   * KEY: EVERY WAY OF NOT HAVING A VERDICT IS A NAMED UNKNOWN, and none of
   * them is `ok`. A reader that fell back to "sound" on something it
   * could not read would tell the user nothing on exactly the occasions
   * it cannot see.
   */
  it('reads no verdict out of an answer it cannot account for', () => {
    const unknowns: Array<[string, string]> = [
      ['two verdict clauses', CHECK('(verdict ok) (verdict damaged)')],
      ['a verdict that is not a name', CHECK('(verdict "ok")')],
      ['no verdict clause', CHECK('')],
      ['a form that is not a check', '(ok (verdict ok))'],
      ['two answers', `${CHECK('(verdict ok)')}\n${CHECK('(verdict ok)')}`],
      ['no answer at all', '']
    ];
    for (const [what, text] of unknowns) {
      const read = verdictOf(parseAnswers(text));
      assert.strictEqual(read.known, false, `${what} was read as a verdict: ${JSON.stringify(read)}`);
    }
  });

  it('says something only about a store that is not sound, and says its own word', () => {
    const damaged: StoreVerdict = { known: true, verdict: 'damaged' };
    const notice = integrityNotice('/stores/A', damaged);
    assert.ok(notice !== null, 'a damaged store was not reported');
    assert.strictEqual(notice.level, 'warning');
    assert.match(notice.text, /"damaged"/, 'the sentence does not carry the verdict the store gave');
    assert.match(notice.text, /\/stores\/A/, 'the sentence does not say which store');
    assert.strictEqual(integrityNotice('/stores/A', { known: true, verdict: 'ok' }), null);
    /*
     * AND AN UNKNOWN IS NOT REPORTED AS SOUND OR AS DAMAGED. It is left to
     * the status bar, where the conflict count asked at the same moment
     * puts the core's own sentence.
     */
    assert.strictEqual(integrityNotice('/stores/A', { known: false, because: 'could not ask' }), null);
    /*
     * NOTE: AND IT DOES NOT PROMISE THAT SAVING STILL WORKS, because on a
     * store with no cursor yet the bootstrap can hold a save back. A claim
     * this function cannot check is not in its sentence.
     */
    assert.doesNotMatch(notice.text, /sav(e|ing)/i, 'the sentence makes a promise about saving');
  });

  /*
   * KEY: THE CASE THE READER EXISTS FOR ARRIVES WITH A NON-ZERO EXIT.
   *
   * The core makes `check` a failure exactly when its verdict is not
   * `ok` (`rpc-ok?`, rpc.sc:338 in the pinned core). So a damaged store
   * answers with exit 1 AND a complete check form, and a method that
   * asked `answer.ok` first would report "could not ask" on precisely
   * the occasion this item exists for. Put through a real client, over
   * the stand-in, with the exit the core really gives.
   */
  it('reads the verdict of a damaged store, which answers with a non-zero exit', async () => {
    core = new FakeCore([{ match: ['check'], stdout: `${CHECK('(verdict damaged)')}\n`, rc: 1 }]);
    const model = new StoreModel(new Client(new CliTransport(core.config(), core.env())));
    assert.deepStrictEqual(await model.storeVerdict(), { known: true, verdict: 'damaged' });
  });

  it('reads a sound store as sound, with the zero exit it gives', async () => {
    core = new FakeCore([{ match: ['check'], stdout: `${CHECK('(verdict ok)')}\n`, rc: 0 }]);
    const model = new StoreModel(new Client(new CliTransport(core.config(), core.env())));
    assert.deepStrictEqual(await model.storeVerdict(), { known: true, verdict: 'ok' });
  });
});

/*
 * A window as `IntegrityWatch` sees it: which store is configured, what
 * `check` answers for each store, what the user was shown, and how many
 * times each store was asked, and what was written to the output channel
 * (`recorded`). `hold` keeps answers back until `release`
 * is called, so that a cell can move the window, or start a second check,
 * while they wait.
 */
function windowOn(answers: Record<string, StoreVerdict>): {
  question: (store: string) => IntegrityQuestion;
  asked: Record<string, number>;
  shown: Notice[];
  recorded: string[];
  moveOn: () => void;
  hold: () => void;
  release: () => void;
} {
  const asked: Record<string, number> = {};
  const shown: Notice[] = [];
  const recorded: string[] = [];
  let generation = 0;
  let held: Array<() => void> = [];
  let holding = false;
  return {
    question: (store) => ({
      store,
      ask: () => {
        asked[store] = (asked[store] ?? 0) + 1;
        const answer = answers[store];
        if (!holding) {
          return Promise.resolve(answer);
        }
        return new Promise<StoreVerdict>((resolve) => {
          held.push(() => resolve(answer));
        });
      },
      show: (notice) => {
        shown.push(notice);
      },
      generation: () => generation,
      record: (line) => {
        recorded.push(line);
      }
    }),
    asked,
    shown,
    recorded,
    moveOn: () => {
      generation += 1;
    },
    hold: () => {
      holding = true;
    },
    release: () => {
      holding = false;
      const go = held;
      held = [];
      assert.ok(go.length > 0, 'nothing was waiting for an answer, so there was nothing to release');
      go.forEach((answer) => answer());
    }
  };
}

const DAMAGED: StoreVerdict = { known: true, verdict: 'damaged' };
const SOUND: StoreVerdict = { known: true, verdict: 'ok' };

describe('plugin-r3 when the user is told that a store is not sound', () => {
  it('asks the store, and tells the user when it is not sound', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    await new IntegrityWatch().check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 1, 'the store was not asked about its condition');
    assert.strictEqual(w.shown.length, 1, 'a damaged store was not reported');
    assert.match(w.shown[0].text, /\/stores\/A/);
  });

  it('tells the user about a store once in a session, and does not ask again', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, `the same store was reported ${w.shown.length} times`);
    assert.strictEqual(w.asked['/stores/A'], 1, 'a store already reported was asked again');
  });

  /*
   * KEY: A STORE IS MARKED ONLY WHEN THE USER WAS TOLD. A sound store and
   * one whose condition could not be read are asked again next time, which
   * is the only way a store that goes bad during the session is seen.
   */
  it('asks again about a store it had nothing to say about, and tells when it has gone bad', async () => {
    const answers: Record<string, StoreVerdict> = {
      '/stores/A': SOUND,
      '/stores/U': { known: false, because: 'could not ask' }
    };
    const w = windowOn(answers);
    const watch = new IntegrityWatch();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/U'));
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/U'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'a sound store was not asked again');
    assert.strictEqual(w.asked['/stores/U'], 2, 'a store that could not be read was not asked again');
    assert.strictEqual(w.shown.length, 0, 'something was said about a sound or unreadable store');
    answers['/stores/A'] = DAMAGED;
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'a store that went bad during the session was not reported');
  });

  /*
   * AND BACK TO EACH ONE: telling the user about B must not make the watch
   * forget that it told them about A, and remembering A must not stand in
   * for remembering B.
   */
  it('keeps what it was told about one store to that store', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/B'));
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/B'));
    assert.strictEqual(w.asked['/stores/B'], 1, 'the second store was asked again after it was reported');
    assert.strictEqual(w.asked['/stores/A'], 1, 'the first store was asked again after the second was reported');
    assert.deepStrictEqual(
      w.shown.map((n) => /\/stores\/[AB]/.exec(n.text)?.[0]),
      ['/stores/A', '/stores/B'],
      'each damaged store is not reported once under its own name'
    );
  });

  /*
   * AND THE MARKS ARE NOT A WINDOW OF THE LAST FEW. (queue item 17) A watch
   * that cleared its marks once it held two passed the cell above -- A, B, A,
   * B never lets two be forgotten before they are asked again -- and one that
   * cleared them at three passed this cell's first version, A, B, C, A (review
   * r1). So the first store is followed by thirty-two others: a watch that
   * forgets past any number up to that asks it again. Past thirty-two is not
   * measured.
   */
  it('keeps every store it told about, however many others it tells about after', async () => {
    const others = Array.from({ length: 32 }, (_, i) => `/stores/other-${i}`);
    const answers: Record<string, StoreVerdict> = { '/stores/A': DAMAGED };
    others.forEach((store) => {
      answers[store] = DAMAGED;
    });
    const w = windowOn(answers);
    const watch = new IntegrityWatch();
    for (const store of ['/stores/A', ...others, '/stores/A']) {
      await watch.check(w.question(store));
    }
    assert.strictEqual(w.shown.length, 33, `${w.shown.length} stores were reported where 33 were damaged`);
    assert.strictEqual(w.asked['/stores/A'], 1, 'the first store was asked again after two others were reported');
    assert.strictEqual(
      w.shown.filter((n) => n.text.includes('/stores/A')).length,
      1,
      'the first store was reported again after two others were'
    );
  });

  /*
   * KEY: AN ANSWER FOR A WINDOW THAT HAS MOVED ON IS DROPPED, and the store
   * it was about is not marked, so it is asked again. The same cell also
   * lets an answer through when nothing moved: a guard turned around would
   * drop that one and pass the stale one.
   */
  it('drops an answer that arrives after the window moved on, and passes one that does not', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    w.hold();
    const late = watch.check(w.question('/stores/A'));
    w.moveOn();
    w.release();
    await late;
    assert.strictEqual(w.shown.length, 0, 'an answer that arrived after the window moved on was shown');
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'a store whose answer was dropped was not asked again');
    assert.strictEqual(w.shown.length, 1, 'an answer that arrived with nothing moved was not shown');
  });

  it('tells the user once when two checks of one store are waiting at the same time', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    w.hold();
    const first = watch.check(w.question('/stores/A'));
    const second = watch.check(w.question('/stores/A'));
    w.release();
    await Promise.all([first, second]);
    assert.strictEqual(w.asked['/stores/A'], 2, 'the two checks were not both waiting');
    assert.strictEqual(w.shown.length, 1, `one store was reported ${w.shown.length} times at once`);
  });

  /*
   * KEY: A WARNING THAT COULD NOT BE SHOWN HAS TOLD NOBODY, so the store is
   * not marked and the next check shows it.
   */
  it('keeps a store unmarked when its warning could not be shown, and shows it next time', async () => {
    /*
     * What `show` throws is not this watch's to choose, and the mark must not
     * depend on its shape. AND EVERY FALSY VALUE (queue item 17): a watch that
     * took the mark back only `if (e)` passed with an Error and a string, and
     * one that did so only `if (e !== null)` passed with those plus
     * `undefined`, `0` and `''` (review r1). All eight falsy values, and four
     * that are not.
     *
     * KEY: AND THE CHECK RESOLVES. (queue item 18, ruled 2026-09-25) This
     * cell used to assert that it rejected -- today's behaviour then, marked as
     * a tripwire. Now: nothing escapes, nothing is shown, exactly one line is
     * written to the output channel naming the store and the value, and the
     * next check tells the user.
     */
    const thrown: unknown[] = [
      new Error('the editor could not show it'),
      'the editor could not show it',
      7,
      Object.create(null),
      undefined,
      null,
      false,
      0,
      -0,
      NaN,
      '',
      BigInt(0),
      Symbol('show failed')
    ];
    for (const value of thrown) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        show: () => {
          throw value;
        }
      };
      const what = `${typeof value} ${describeValue(value)}`;
      await watch.check(failing);
      assert.strictEqual(w.shown.length, 0, `(${what}) a warning that threw was counted as shown`);
      assert.strictEqual(w.recorded.length, 1, `(${what}) ${w.recorded.length} lines were written for one failed warning`);
      assert.ok(w.recorded[0].includes('/stores/A'), `(${what}) the line does not name the store: ${w.recorded[0]}`);
      assert.ok(w.recorded[0].includes(`(${describeValue(value)})`), `(${what}) the line does not name the value: ${w.recorded[0]}`);
      assert.match(w.recorded[0], / at \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/, `(${what}) the line carries no time`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.asked['/stores/A'], 2, `(${what}) a store whose warning was never shown was not asked again`);
      assert.strictEqual(w.shown.length, 1, `(${what}) the warning that could not be shown was not shown the next time`);
    }
  });

  /*
   * AND WHEN THE LINE CANNOT BE WRITTEN EITHER, the check still resolves and
   * the store is still unmarked: there is nowhere left to say it, and the
   * unmarked store is what brings the warning back. (queue item 18)
   */
  it('resolves and leaves the store unmarked when the output channel throws too', async () => {
    /*
     * An object with no prototype and an Error (review r1 of item 18: a guard
     * that rethrew Errors passed with the first alone).
     */
    for (const channelThrows of [Object.create(null), new Error('channel failure')] as unknown[]) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        show: () => {
          throw new Error('the editor could not show it');
        },
        record: () => {
          throw channelThrows;
        }
      };
      /*
       * NOTE: WHAT ESCAPES IS CAUGHT HERE, NOT BY THE RUNNER. The value thrown
       * may be an object with no prototype, and a runner that is handed one as
       * a failure cannot print it: measured on this cell's first version, with
       * the inner `try` removed, mocha stopped in the middle of the suite and
       * exited 0 with no summary -- a failure that read as a pass.
       */
      let escaped = false;
      try {
        await watch.check(failing);
      } catch {
        escaped = true;
      }
      const what = describeValue(channelThrows);
      assert.strictEqual(escaped, false, `(${what}) a throw from the output channel escaped the check`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.asked['/stores/A'], 2, `(${what}) a store whose warning and whose line both failed was not asked again`);
      assert.strictEqual(w.shown.length, 1, `(${what}) the warning was not shown the next time`);
    }
  });

  /*
   * AN ERROR IS NAMED BY WHAT IT SAYS. (review r1 of item 18) The most common
   * thing a `show` throws is an Error, and a line reading "(an object)" tells
   * the reader nothing; its name and message are read, each on its own, since
   * either can be a getter that throws.
   */
  it('names an Error in the line by its name and message, whichever can be read', async () => {
    const noMessage = new RangeError('unread');
    Object.defineProperty(noMessage, 'message', {
      get() {
        throw new Error('no message for you');
      }
    });
    const cases: Array<[unknown, string]> = [
      [new TypeError('the editor is gone'), '(TypeError: the editor is gone)'],
      [noMessage, '(RangeError)']
    ];
    for (const [value, expected] of cases) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        show: () => {
          throw value;
        }
      };
      await new IntegrityWatch().check(failing);
      assert.strictEqual(w.recorded.length, 1, `${expected}: ${w.recorded.length} lines`);
      assert.ok(w.recorded[0].includes(expected), `the line does not carry ${expected}: ${w.recorded[0]}`);
    }
  });

  /*
   * AND A VALUE NOTHING CAN BE READ FROM STILL GETS ITS LINE. (review r1 of
   * item 18) Naming these threw, inside the guarded `record`, so the line was
   * lost: a proxy whose `getPrototypeOf` trap throws, a revoked proxy, a
   * function whose `Symbol.toPrimitive` throws.
   */
  it('writes the line for a value that cannot be named', async () => {
    const revoked = Proxy.revocable({}, {});
    revoked.revoke();
    const primitive = function (): void {
      return undefined;
    };
    Object.defineProperty(primitive, Symbol.toPrimitive, {
      value: () => {
        throw new Error('no primitive');
      }
    });
    const values: unknown[] = [
      new Proxy(
        {},
        {
          getPrototypeOf() {
            throw new Error('no prototype for you');
          }
        }
      ),
      revoked.proxy,
      primitive
    ];
    for (const value of values) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        show: () => {
          throw value;
        }
      };
      let escaped = false;
      try {
        await new IntegrityWatch().check(failing);
      } catch {
        escaped = true;
      }
      assert.strictEqual(escaped, false, 'a value that cannot be named escaped the check');
      assert.strictEqual(w.recorded.length, 1, `no line was written for ${describeValue(value)}`);
    }
  });

  /*
   * THE TWIN: a warning that WAS shown writes nothing on the channel. Without
   * it a watch that wrote every warning down would pass the cells above.
   */
  it('writes nothing on the output channel when the warning was shown', async () => {
    /*
     * EVERY SHAPE OF VERDICT (review r1 of item 18): a known one the core
     * writes (`damaged`, `ok`), a known one this client has never heard of,
     * and one that could not be read. Only the first and third are said.
     */
    const verdicts: Array<[StoreVerdict, number]> = [
      [DAMAGED, 1],
      [SOUND, 0],
      [{ known: true, verdict: 'corrupt' }, 1],
      [{ known: false, because: 'could not ask' }, 0]
    ];
    for (const [verdict, said] of verdicts) {
      const w = windowOn({ '/stores/A': verdict });
      await new IntegrityWatch().check(w.question('/stores/A'));
      const what = JSON.stringify(verdict);
      assert.strictEqual(w.shown.length, said, `${what}: shown ${w.shown.length} times`);
      assert.deepStrictEqual(w.recorded, [], `${what}: something was written for a warning that was shown`);
    }
  });

  /*
   * KEY: A QUESTION THAT FAILED IS AN ANSWER NOBODY COULD READ. Nothing is
   * said, nothing escapes -- the extension does not wait for this call --
   * and the store is asked again.
   */
  it('says nothing about a store it could not ask, lets nothing escape, and asks again', async () => {
    /*
     * Three shapes of rejection. The last is an object with no prototype,
     * which `String` cannot convert: a reason built that way threw inside
     * the catch and let the rejection out after all.
     */
    const rejections: Array<[string, unknown]> = [
      ['an Error', new Error('the core did not answer')],
      ['a string', 'the core did not answer'],
      ['an object with no prototype', Object.create(null)]
    ];
    for (const [what, value] of rejections) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        ask: () => Promise.reject(value)
      };
      await watch.check(failing);
      assert.strictEqual(w.shown.length, 0, `(${what}) something was said about a store that could not be asked`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.asked['/stores/A'], 1, `(${what}) a store that could not be asked was not asked again`);
      assert.strictEqual(w.shown.length, 1, `(${what}) a store that could not be asked was not reported once it answered`);
    }
  });

  /*
   * AND AN `ask` THAT THROWS BEFORE IT RETURNS A PROMISE. (queue item 17)
   * Every failing `ask` above rejects; a watch that called `ask` outside its
   * `try` and awaited the promise inside passed all of them, and lets this
   * one out of a call nobody waits for. And whatever it throws (review r1: a
   * catch that handled only `Error` passed with an Error alone).
   */
  it('treats an ask that throws at once as one that failed', async () => {
    const thrown: unknown[] = [new Error('the question could not be put'), 'failed', 7, Object.create(null), undefined, null, 0, ''];
    for (const value of thrown) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const failing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        ask: () => {
          throw value;
        }
      };
      const what = `${typeof value} ${describeValue(value)}`;
      await watch.check(failing);
      assert.strictEqual(w.shown.length, 0, `(${what}) something was said about a store that could not be asked`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.shown.length, 1, `(${what}) a store whose question threw was not asked again and reported`);
    }
  });
});

/*
 * ONE TURN OF THE EVENT LOOP: every promise the watch followed has settled
 * and been handled, and an unhandled rejection, if any, has been reported.
 */
function turn(): Promise<void> {
  return new Promise((resolve) => setImmediate(resolve));
}

/*
 * THE CELL'S OWN COUNT OF UNHANDLED REJECTIONS, added beside Mocha's
 * listener and never in place of it. KEY: IT IS THE ONLY GUARD: Mocha 10
 * re-emits a user rejection on `process` and nothing else, so a rejection
 * nobody else listens for vanishes (measured for queue item 33:
 * archive/theourgia-vsc-item-38-2026-09-25/leaves.out). Counted by promise,
 * because that re-emit reaches this listener a second time.
 */
function countingUnhandled(): { stop: () => number } {
  const seen = new Set<unknown>();
  const listener = (_reason: unknown, promise: unknown): void => {
    seen.add(promise);
  };
  process.on('unhandledRejection', listener);
  return {
    stop: () => {
      process.removeListener('unhandledRejection', listener);
      return seen.size;
    }
  };
}

/*
 * plugin-r3 item 33: what `show` and `record` return (design
 * archive/theourgia-vsc-item-33-design-2026-09-25.md, v7). Marked when `show`
 * returns; what it returned is followed, never awaited; a refusal unmarks and
 * writes the line.
 */
describe('plugin-r3 33 what show and record return', function () {
  this.timeout(30000);
  const THROWN: unknown[] = [
    new Error('the editor refused it'),
    'refused',
    7,
    Object.create(null),
    undefined,
    null,
    false,
    0,
    -0,
    NaN,
    '',
    BigInt(0),
    Symbol('refused')
  ];

  /*
   * C1: A REFUSAL THAT COMES LATER UNMARKS AND IS WRITTEN DOWN, for every
   * value item 18's synchronous cell throws -- a handler that tested the
   * reason (`if (!reason) return`) would miss the falsy ones. The line's
   * time lies between the start of the check and the assertion; the watch is
   * built 5 ms before that start, so a time taken when the watch was built
   * (L0, measured) falls outside.
   */
  it('C1 unmarks a store and writes one line when show returns a promise that rejects later', async () => {
    for (const value of THROWN) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      await new Promise((resolve) => setTimeout(resolve, 5));
      const started = Date.now();
      const unhandled = countingUnhandled();
      const refusing: IntegrityQuestion = {
        ...w.question('/stores/A'),
        show: () => new Promise((_resolve, reject) => setImmediate(() => reject(value)))
      };
      await watch.check(refusing);
      await turn();
      await turn();
      const what = `${typeof value} ${describeValue(value)}`;
      assert.strictEqual(w.recorded.length, 1, `(${what}) ${w.recorded.length} lines for one refusal`);
      assert.ok(w.recorded[0].includes('/stores/A') && w.recorded[0].includes(`(${describeValue(value)})`), `(${what}) ${w.recorded[0]}`);
      assert.match(w.recorded[0], / at \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/, `(${what}) the line carries no time`);
      const at = Date.parse(w.recorded[0].slice(w.recorded[0].lastIndexOf(' at ') + 4));
      assert.ok(at >= started && at <= Date.now(), `(${what}) the line's time is not the time of the refusal`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.asked['/stores/A'], 2, `(${what}) the refused store was not asked again`);
      assert.strictEqual(w.shown.length, 1, `(${what}) the refused store was not shown again`);
      await turn();
      assert.strictEqual(unhandled.stop(), 0, `(${what}) a rejection was left unhandled`);
    }
  });

  /*
   * C2: A PENDING SHOW MARKS AT ONCE AND IS NOT WAITED FOR. A second check
   * during the wait shows nothing, and the first check's own promise settles
   * while `show`'s is still pending -- an implementation that marked and then
   * awaited would hang here.
   */
  it('C2 marks at once, shows nothing more, and does not wait for a pending show', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let release: () => void = () => undefined;
    let pendingShows = 0;
    const pending: IntegrityQuestion = {
      ...w.question('/stores/A'),
      show: () => {
        pendingShows += 1;
        return new Promise<void>((resolve) => {
          release = resolve;
        });
      }
    };
    let settled = false;
    const first = watch.check(pending).then(() => {
      settled = true;
    });
    await turn();
    await turn();
    assert.strictEqual(settled, true, 'the check waited for a show that had not finished');
    await watch.check(pending);
    assert.strictEqual(pendingShows, 1, 'a second check showed again while the first show was pending');
    assert.strictEqual(w.asked['/stores/A'], 1, 'a second check asked again while the store was marked');
    release();
    await first;
    await turn();
    await watch.check(pending);
    assert.strictEqual(pendingShows, 1, 'a third check showed again after the show finished');
    assert.deepStrictEqual(w.recorded, []);
  });

  it('C3 keeps the mark and writes nothing when show returns a promise that resolves', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let shows = 0;
    const resolving: IntegrityQuestion = {
      ...w.question('/stores/A'),
      show: () => {
        shows += 1;
        return Promise.resolve('dismissed');
      }
    };
    await watch.check(resolving);
    await turn();
    await watch.check(resolving);
    assert.strictEqual(shows, 1);
    assert.strictEqual(w.asked['/stores/A'], 1);
    assert.deepStrictEqual(w.recorded, []);
  });

  /*
   * C4: THE LINE FAILING NEVER ESCAPES AND IS NEVER WAITED FOR. `record`
   * throws, returns a rejecting promise, returns something whose `then`
   * getter throws, or returns a promise that never settles -- inside the path
   * a refusal of `show` takes, and after a synchronous throw of `show`.
   *
   * NOTE: THIS PAIR IS THE ONLY CELL THAT MEASURES 'THE UNMARK DOES NOT WAIT
   * FOR THE LINE' (D4) AGAINST AN UNBOUNDED WAIT: a show that rejects with a
   * record that never settles. An unmark placed after awaiting record (U2,
   * measured) turns this pair red, and C11 (which measures the bounded wait).
   * A bounded wait shorter than this cell's two turns passes here; C11
   * measures it.
   */
  it('C4 lets nothing out of a line that fails or never finishes, and waits for none', async () => {
    const recordings: Array<[string, () => unknown]> = [
      ['throws', () => {
        throw new Error('Channel has been closed');
      }],
      ['rejects', () => Promise.reject(new Error('the channel refused'))],
      ['a then getter that throws', () => ({
        get then(): unknown {
          throw new Error('no then');
        }
      })],
      ['never settles', () => new Promise(() => undefined)]
    ];
    const shows: Array<[string, () => void | PromiseLike<unknown>]> = [
      ['a show that rejects', () => Promise.reject(new Error('refused'))],
      ['a show that throws', () => {
        throw new Error('refused at once');
      }]
    ];
    for (const [showing, show] of shows) {
      for (const [recording, record] of recordings) {
        const what = `${showing}, a record that ${recording}`;
        const w = windowOn({ '/stores/A': DAMAGED });
        const watch = new IntegrityWatch();
        const unhandled = countingUnhandled();
        let resolved = false;
        let calls = 0;
        const counted = (): unknown => {
          calls += 1;
          return record();
        };
        const check = watch
          .check({ ...w.question('/stores/A'), show, record: counted as IntegrityQuestion['record'] })
          .then(() => {
            resolved = true;
          });
        await turn();
        await turn();
        assert.strictEqual(resolved, true, `(${what}) the check did not settle`);
        assert.strictEqual(calls, 1, `(${what}) record was called ${calls} times, not once`);
        await check;
        await watch.check(w.question('/stores/A'));
        assert.strictEqual(w.shown.length, 1, `(${what}) the store was not shown on the next check`);
        await turn();
        assert.strictEqual(unhandled.stop(), 0, `(${what}) a rejection was left unhandled`);
        /*
         * READ AGAIN AT THE END: a second call that comes later than the
         * first read is caught here, up to the last turn this cell waits.
         * A call later than that is outside any finite window (L).
         */
        assert.strictEqual(calls, 1, `(${what}) record was called ${calls} times by the end, not once`);
      }
    }
  });

  /*
   * C5: A VALUE THAT REFUSES TO BE FOLLOWED IS A REFUSAL: a `then` getter
   * that throws, a `then` that throws when called.
   */
  it('C5 reads a then that cannot be read or called as a refusal', async () => {
    const values: Array<[string, unknown]> = [
      ['a then getter that throws', {
        get then(): unknown {
          throw new Error('no then');
        }
      }],
      ['a then that throws', {
        then(): never {
          throw new Error('then refused');
        }
      }]
    ];
    for (const [what, value] of values) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      await watch.check({ ...w.question('/stores/A'), show: () => value as PromiseLike<unknown> });
      await turn();
      assert.strictEqual(w.recorded.length, 1, `(${what}) ${w.recorded.length} lines`);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.shown.length, 1, `(${what}) the store was not shown on the next check`);
    }
  });

  /*
   * C6: VALUES THAT PLAY WITH THE PROTOCOL, and what the watch does with each
   * -- the watch's side only; what a value does with its own promises is the
   * value's (design D2's boundary), and is measured apart, below.
   */
  it('C6 follows a hostile value once, and keeps to what the language promises', async () => {
    const outcome = async (value: () => unknown): Promise<{ lines: number; askedAgain: boolean }> => {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      await watch.check({ ...w.question('/stores/A'), show: () => value() as PromiseLike<unknown> });
      await turn();
      await turn();
      await watch.check(w.question('/stores/A'));
      return { lines: w.recorded.length, askedAgain: w.asked['/stores/A'] === 2 };
    };
    const failThenThrow = {
      then(_ok: unknown, fail: (r: unknown) => void): never {
        fail(new Error('refused'));
        throw new Error('and then threw');
      }
    };
    assert.deepStrictEqual(await outcome(() => failThenThrow), { lines: 1, askedAgain: true }, 'a failure then a throw');
    const okThenFail = {
      then(ok: (v: unknown) => void, fail: (r: unknown) => void): void {
        ok('shown');
        fail(new Error('too late'));
      }
    };
    assert.deepStrictEqual(await outcome(() => okThenFail), { lines: 0, askedAgain: false }, 'a success then a failure');
    const thenGetterThrows = (): unknown => {
      const p = Promise.resolve('shown');
      Object.defineProperty(p, 'then', {
        get(): never {
          throw new Error('no then');
        }
      });
      return p;
    };
    assert.deepStrictEqual(await outcome(thenGetterThrows), { lines: 1, askedAgain: true }, 'a native promise whose then getter throws');
    const constructorGetterThrows = (): unknown => {
      const p = Promise.resolve('shown');
      Object.defineProperty(p, 'constructor', {
        get(): never {
          throw new Error('no constructor');
        }
      });
      return p;
    };
    assert.deepStrictEqual(await outcome(constructorGetterThrows), { lines: 1, askedAgain: true }, 'a native promise whose constructor getter throws');
    /*
     * THE SAVED CALLBACK: refused once, then shown and marked; the saved
     * failure callback invoked again must neither unmark nor write.
     */
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let saved: (reason: unknown) => void = () => undefined;
    const saving = Promise.resolve('shown');
    saving.then = function (this: Promise<string>, _ok?: unknown, fail?: unknown): Promise<never> {
      saved = fail as (reason: unknown) => void;
      saved(new Error('refused the first time'));
      return Promise.reject(new Error('unused')).catch(() => undefined) as Promise<never>;
    } as typeof saving.then;
    await watch.check({ ...w.question('/stores/A'), show: () => saving });
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.recorded.length, 1);
    saved(new Error('the same refusal again'));
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'a callback invoked again undid a later mark');
    assert.strictEqual(w.recorded.length, 1, 'a callback invoked again wrote a second line');
  });

  /*
   * C6, THE VALUE'S OWN REJECTIONS, measured in a child process so that none of
   * them is left in this one: a thenable that calls the failure callback and
   * also returns a rejecting promise; one that only returns it (the watch's
   * wrapper never settles); a native rejected promise whose `then` getter
   * throws. The watch's side is asserted: its lines, whether it asks again,
   * and that no unhandled rejection is a promise the value did not create
   * (`foreign`). Each case registers the promises it creates in `own`; the
   * count of those left unhandled is read and kept in the failure message, not
   * asserted -- it is the value's (design D2 (a), (b)). Counting every
   * unhandled promise, as r1 did, could not tell a rejection the watch leaks
   * from one the value owns.
   */
  it('C6 answers for the watch and not for the value, in a process of its own', () => {
    const integrity = path.join(__dirname, '..', '..', 'src', 'integrity.js');
    const script = `
      const { IntegrityWatch } = require(${JSON.stringify(integrity)});
      const turn = () => new Promise((r) => setImmediate(r));
      const cases = {
        'fails and returns a rejection': (own) => ({ then(_ok, fail) { fail(new Error('refused')); const r = Promise.reject(new Error("the value's own")); own.add(r); return r; } }),
        'only returns a rejection': (own) => ({ then() { const r = Promise.reject(new Error("the value's own")); own.add(r); return r; } }),
        'a rejected promise whose then getter throws': (own) => { const p = Promise.reject(new Error("the value's own")); own.add(p); Object.defineProperty(p, 'then', { get() { throw new Error('no then'); } }); return p; }
      };
      (async () => {
        const out = {};
        for (const [name, value] of Object.entries(cases)) {
          const seen = new Set(); const count = (r, p) => seen.add(p); const own = new Set();
          process.on('unhandledRejection', count);
          let asked = 0; const lines = [];
          const q = (show) => ({ store: '/s', ask: async () => { asked += 1; return { known: true, verdict: 'damaged' }; }, show, generation: () => 0, record: (l) => { lines.push(l); } });
          const watch = new IntegrityWatch();
          await watch.check(q(() => value(own)));
          await turn(); await turn();
          await watch.check(q(() => undefined));
          await turn(); await turn();
          process.removeListener('unhandledRejection', count);
          const foreign = [...seen].filter((p) => !own.has(p)).length;
          out[name] = { lines: lines.length, askedAgain: asked === 2, foreign, valuesOwn: seen.size - foreign };
        }
        console.log(JSON.stringify(out));
      })();`;
    const run = spawnSync(process.execPath, ['-e', script], { encoding: 'utf8', timeout: 20000 });
    assert.strictEqual(run.status, 0, run.stdout + run.stderr);
    const out = JSON.parse(run.stdout.trim().split('\n').pop() as string);
    const read = JSON.stringify(out);
    assert.deepStrictEqual({ lines: out['fails and returns a rejection'].lines, askedAgain: out['fails and returns a rejection'].askedAgain, foreign: out['fails and returns a rejection'].foreign }, { lines: 1, askedAgain: true, foreign: 0 }, read);
    assert.deepStrictEqual({ lines: out['only returns a rejection'].lines, askedAgain: out['only returns a rejection'].askedAgain, foreign: out['only returns a rejection'].foreign }, { lines: 0, askedAgain: false, foreign: 0 }, read);
    assert.deepStrictEqual({ lines: out['a rejected promise whose then getter throws'].lines, askedAgain: out['a rejected promise whose then getter throws'].askedAgain, foreign: out['a rejected promise whose then getter throws'].foreign }, { lines: 1, askedAgain: true, foreign: 0 }, read);
  });

  /*
   * C7: THE EXTENSION, NOT ONLY THE WATCH. Activated against a stand-in whose
   * `showWarningMessage` returns a rejecting promise: the output channel gets
   * the line, and the process sees no unhandled rejection. Today's shape (the
   * thenable discarded) left one. The twin: a warning that resolves writes
   * nothing -- and leaves nothing unhandled (a discarded chain on the
   * fulfilment, Q0, added behaviour, L by design A9, leaves one). And a
   * stand-in whose `showWarningMessage` throws at once: the line is written
   * too (a wrapper that swallowed the throw, J2, measured, wrote nothing and
   * kept the store marked).
   */
  it('C7 writes a refused warning to the output channel and leaves nothing unhandled, in the activated extension', () => {
    const run = (name: string): { lines: string[]; unhandled: number; warnings: string[] } => {
      const child = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
        encoding: 'utf8',
        timeout: 20000
      });
      assert.strictEqual(child.status, 0, child.stdout + child.stderr);
      const result = JSON.parse(child.stdout.trim().split('\n').pop() as string);
      assert.strictEqual(result.complete, true);
      return result.result;
    };
    const refused = run('integrity-show-rejects');
    assert.strictEqual(refused.warnings.length, 1, 'no warning was shown to be refused');
    assert.strictEqual(refused.lines.length, 1, `lines: ${JSON.stringify(refused.lines)}`);
    assert.ok(refused.lines[0].includes('/stores/A') && refused.lines[0].includes('the editor refused the warning'), refused.lines[0]);
    assert.strictEqual(refused.unhandled, 0, 'a rejection was left unhandled in the activated extension');
    const threw = run('integrity-show-throws');
    assert.strictEqual(threw.lines.length, 1, `lines when the editor threw: ${JSON.stringify(threw.lines)}`);
    assert.ok(threw.lines[0].includes('the editor threw'), threw.lines[0]);
    assert.strictEqual(threw.unhandled, 0, 'a rejection was left unhandled when the editor threw');
    const control = run('integrity-show-control');
    assert.strictEqual(control.warnings.length, 1);
    assert.deepStrictEqual(control.lines, [], 'a warning that was shown was written to the channel');
    assert.strictEqual(control.unhandled, 0, 'a rejection was left unhandled when the warning was shown');
  });

  /*
   * C9: A SHOW THAT RE-ENTERS THE CHECK AND THEN THROWS. On its first call it
   * checks the same store again (that nested check's show returns), then
   * throws: the nested check found no mark (asked 2), showed once, and marked
   * -- the outer throw returns without touching the mark. A watch that marked
   * before `show` would read asked 1.
   */
  it('C9 leaves the mark a nested check wrote when the outer show then throws', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let calls = 0;
    let nested: Promise<void> = Promise.resolve();
    const reentering: IntegrityQuestion = {
      ...w.question('/stores/A'),
      show: (notice) => {
        calls += 1;
        if (calls === 1) {
          nested = watch.check(reentering);
          throw new Error('refused after re-entering');
        }
        w.shown.push(notice);
      }
    };
    await watch.check(reentering);
    await nested;
    await turn();
    assert.strictEqual(w.asked['/stores/A'], 2, 'the nested check found a mark');
    assert.strictEqual(w.shown.length, 1);
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'the store the nested check showed was not left marked');
  });

  /*
   * C10: THE UNMARK IS THE STORE'S, NOT THE WINDOW'S. A's show is refused
   * after the window moved to B and before it came back: A is asked and shown
   * again, and B is left alone.
   */
  it('C10 unmarks only the refused store, whatever the window did meanwhile', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    let refuse: (reason: unknown) => void = () => undefined;
    await watch.check({
      ...w.question('/stores/A'),
      show: (notice) => {
        w.shown.push(notice);
        return new Promise((_resolve, reject) => {
          refuse = reject;
        });
      }
    });
    w.moveOn();
    await watch.check(w.question('/stores/B'));
    refuse(new Error('refused late'));
    await turn();
    w.moveOn();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/B'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'the refused store was not asked again');
    assert.strictEqual(w.shown.filter((n) => n.text.includes('/stores/A')).length, 2, 'the refused store was not shown again');
    assert.strictEqual(w.asked['/stores/B'], 1, 'the other store was asked again');
    assert.strictEqual(w.shown.filter((n) => n.text.includes('/stores/B')).length, 1, 'the other store was shown again');
  });

  /*
   * C11: THE UNMARK DOES NOT WAIT FOR THE LINE, NOT EVEN FOR A WHILE (D4). A
   * show that rejects, a record that never settles; after the check returns,
   * only microtasks run -- no macrotask -- and the next check must ask and
   * show again. Measured on the compiled code: the unmark lands at the second
   * microtask after the check returns, so twenty leave a margin of eighteen.
   * An unmark that waits for the line up to one macrotask (B1, measured) is
   * red here and nowhere else in the file. A bounded wait shorter than twenty
   * microtasks passes here (L).
   */
  it('C11 unmarks before any macrotask runs, when the line never finishes', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: (() => new Promise(() => undefined)) as IntegrityQuestion['record']
    });
    for (let i = 0; i < 20; i += 1) {
      await Promise.resolve();
    }
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'the store was still marked after twenty microtasks');
  });

  /*
   * C12: A REFUSAL DELIVERED INSIDE A FULFILMENT IS A REFUSAL (D2). The value
   * fulfils with a thenable that refuses; native resolution adopts it, so the
   * watch hears the refusal. A follow that reads `then` itself and settles on
   * the first callback (R2, measured) is red here and nowhere else.
   */
  it('C12 reads a thenable that fulfils with a refusing thenable as a refusal', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const value = {
      then(ok: (v: unknown) => void): void {
        ok({
          then(_ok: unknown, fail: (e: unknown) => void): void {
            fail(new Error('inner refused'));
          }
        });
      }
    };
    await watch.check({ ...w.question('/stores/A'), show: () => value as PromiseLike<unknown> });
    await turn();
    assert.strictEqual(w.recorded.length, 1, `${w.recorded.length} lines for a refusal delivered inside a fulfilment`);
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'the store was not shown on the next check');
  });

  /*
   * C13: THE MARK IS IN PLACE BEFORE THE VALUE IS READ (D1). Native resolution
   * reads `then` at once, inside the follow; a getter that starts a check of
   * the same store must find the mark. A mark placed after the follow (R3,
   * measured) is red here and nowhere else.
   */
  it('C13 marks the store before the value show returned is read', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let nested: Promise<void> | undefined;
    const value = {
      get then(): undefined {
        nested = watch.check(w.question('/stores/A'));
        return undefined;
      }
    };
    await watch.check({ ...w.question('/stores/A'), show: () => value as unknown as PromiseLike<unknown> });
    await nested;
    await turn();
    assert.ok(nested !== undefined, 'the then getter was never read');
    assert.strictEqual(w.asked['/stores/A'], 1, 'the check started by the then getter found no mark');
    assert.strictEqual(w.shown.length, 0, 'the check started by the then getter showed the store again');
  });

  /*
   * C14: C6 ON THE OTHER SIDE -- the value `record` returns plays with the
   * protocol, measured in a child process for the same reason as C6. Each
   * case registers the promises it creates in `own`; no unhandled rejection
   * may be one the value did not create (`foreign`), and the store is asked
   * again. A write that reads what record returned and chains on it (R1,
   * measured) leaks one rejection of its own and is red here and nowhere
   * else.
   */
  it('C14 answers for the watch and not for the value record returned, in a process of its own', () => {
    const integrity = path.join(__dirname, '..', '..', 'src', 'integrity.js');
    const script = `
      const { IntegrityWatch } = require(${JSON.stringify(integrity)});
      const turn = () => new Promise((r) => setImmediate(r));
      const cases = {
        'fails and returns a rejection': (own) => ({ then(_ok, fail) { fail(new Error('refused')); const r = Promise.reject(new Error("the value's own")); own.add(r); return r; } }),
        'only returns a rejection': (own) => ({ then() { const r = Promise.reject(new Error("the value's own")); own.add(r); return r; } })
      };
      (async () => {
        const out = {};
        for (const [name, value] of Object.entries(cases)) {
          const seen = new Set(); const count = (r, p) => seen.add(p); const own = new Set();
          process.on('unhandledRejection', count);
          const watch = new IntegrityWatch();
          let asked = 0;
          const q = (show, record) => ({ store: '/s', ask: async () => { asked += 1; return { known: true, verdict: 'damaged' }; }, show, generation: () => 0, record });
          await watch.check(q(() => Promise.reject(new Error('the show refused')), () => value(own)));
          await turn(); await turn();
          await watch.check(q(() => undefined, () => undefined));
          await turn(); await turn();
          process.removeListener('unhandledRejection', count);
          const foreign = [...seen].filter((p) => !own.has(p)).length;
          out[name] = { askedAgain: asked === 2, foreign, valuesOwn: seen.size - foreign };
        }
        console.log(JSON.stringify(out));
      })();`;
    const run = spawnSync(process.execPath, ['-e', script], { encoding: 'utf8', timeout: 20000 });
    assert.strictEqual(run.status, 0, run.stdout + run.stderr);
    const out = JSON.parse(run.stdout.trim().split('\n').pop() as string);
    const read = JSON.stringify(out);
    for (const name of ['fails and returns a rejection', 'only returns a rejection']) {
      assert.deepStrictEqual({ askedAgain: out[name].askedAgain, foreign: out[name].foreign }, { askedAgain: true, foreign: 0 }, `${name}: ${read}`);
    }
  });

  /*
   * C15: ONE SUBSCRIPTION (D2). A value that refuses on its first
   * subscription and fulfils on any later one: the watch subscribes once, so
   * it hears the refusal -- one line, and the store is shown again. A second
   * subscription, before or after the one that listens (S1, S1b, measured),
   * is red here and nowhere else.
   */
  it('C15 subscribes to the value show returned once', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let calls = 0;
    const value = {
      then(ok: (v: unknown) => void, fail: (e: unknown) => void): void {
        calls += 1;
        if (calls === 1) {
          fail(new Error('refused on the first subscription'));
        } else {
          ok(undefined);
        }
      }
    };
    await watch.check({ ...w.question('/stores/A'), show: () => value as PromiseLike<unknown> });
    await turn();
    assert.strictEqual(calls, 1, `then was called ${calls} times`);
    assert.strictEqual(w.recorded.length, 1, `${w.recorded.length} lines`);
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'the store was not shown on the next check');
  });

  /*
   * C16: WHAT RECORD RETURNS GOES THROUGH NATIVE RESOLUTION TOO (D2, D4).
   * `record` returns a thenable that fulfils with a rejected promise; native
   * resolution adopts that promise and the watch swallows its rejection, so
   * NOTHING is left unhandled -- the count asserted is the total, not only
   * the watch's own. That is the difference from C6 and C14: there the
   * values' own rejected promises are ones resolution never adopts, so they
   * stay the values'. A write that calls the value's `then` itself instead
   * (S2, measured) leaves the adopted promise unhandled and is red here and
   * nowhere else.
   */
  it('C16 leaves nothing unhandled when record fulfils with a rejected promise', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const unhandled = countingUnhandled();
    const record = (): unknown => ({
      then(ok: (v: unknown) => void): void {
        ok(Promise.reject(new Error('the line was refused inside')));
      }
    });
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: record as IntegrityQuestion['record']
    });
    await turn();
    await turn();
    assert.strictEqual(unhandled.stop(), 0, 'a rejection was left unhandled');
  });

  /*
   * C17: NOTHING OF THE VALUE BUT THE ONE READ OF `then` (I3). `show`
   * returns a Proxy whose handler is itself a Proxy, so every trap goes
   * through one function: `get` of `then` answers `undefined`; any other trap
   * is written into `touched`, `getPrototypeOf` then throws and `get` answers
   * `undefined`. Native resolution only reads `then`, so the value is a plain
   * fulfilment: the store is marked, no line is written, nothing is left
   * unhandled, the check resolves, and `touched` stays empty. An `instanceof`
   * throws at once (X6, measured); a read of `constructor` (V2) or an `in`
   * (V4) is written into `touched` (measured). Truthiness and `typeof` call
   * nothing on the value and cannot be observed by any cell (L, design A4
   * I3).
   */
  it('C17 reads nothing of the value but then, not even its prototype', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const unhandled = countingUnhandled();
    const touched: string[] = [];
    let thenReads = 0;
    const value = new Proxy({}, new Proxy({}, {
      get(_h: object, trap: string | symbol): unknown {
        return (target: object, ...rest: unknown[]): unknown => {
          if (trap === 'get' && rest[0] === 'then') {
            thenReads += 1;
            return undefined;
          }
          touched.push(trap === 'get' || trap === 'has' ? `${String(trap)} ${String(rest[0])}` : String(trap));
          if (trap === 'getPrototypeOf') {
            throw new Error('the prototype was asked for');
          }
          if (trap === 'get') {
            return undefined;
          }
          return (Reflect as unknown as Record<string, (...a: unknown[]) => unknown>)[String(trap)](target, ...rest);
        };
      }
    }) as ProxyHandler<object>);
    let settled = 'pending';
    await watch.check({ ...w.question('/stores/A'), show: () => value as PromiseLike<unknown> }).then(
      () => {
        settled = 'resolved';
      },
      (reason: unknown) => {
        settled = `rejected: ${String(reason)}`;
      }
    );
    await turn();
    assert.strictEqual(settled, 'resolved', 'the check did not resolve');
    assert.strictEqual(w.recorded.length, 0, `${w.recorded.length} lines for a value that fulfilled`);
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 1, 'the store was not left marked');
    assert.strictEqual(unhandled.stop(), 0, 'a rejection was left unhandled');
    assert.deepStrictEqual(touched, [], 'the value was touched beyond reading then');
    /*
     * ONE READ OF `then`: resolving the fresh promise reads it. (Until r11
     * this asserted two: the second came from the derived promise passing
     * the fulfilment through, and was taken for the language's normal
     * behaviour; a getter that throws on that read left an unhandled
     * rejection of the watch's own -- C20. The fulfilment callback now
     * returns `undefined`, design A5.) A second read (T1, a guarded extra
     * read before the follow, measured) is red here.
     */
    assert.strictEqual(thenReads, 1, `then was read ${thenReads} times`);
  });

  /*
   * C18: C15 AND C17 ON THE RECORD SIDE (I2, I3 -- both sides). `show`
   * refuses; `record` returns a Proxy whose `getPrototypeOf` trap throws and
   * whose `get` trap counts reads of `then`, and the `then` it hands out
   * counts its calls; as in C17, the handler is itself a Proxy and every
   * other trap is written into `touched`. Native resolution reads `then`
   * once and calls it once, and touches nothing else. A second follow of
   * what record returned (Y1, measured) reads it twice; an `instanceof`
   * before the follow (Y2, measured) throws inside write's guard, which
   * swallows it, so the only trace is that the follow never happens: `then`
   * read 0 times; a read of `constructor` (V1) or an `in` (V5) is written
   * into `touched` (measured). Each is red here and nowhere else.
   */
  it('C18 reads then of what record returned once, subscribes once, and reads nothing else', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let reads = 0;
    let calls = 0;
    const touched: string[] = [];
    const recorded = new Proxy({}, new Proxy({}, {
      get(_h: object, trap: string | symbol): unknown {
        return (target: object, ...rest: unknown[]): unknown => {
          if (trap === 'get' && rest[0] === 'then') {
            reads += 1;
            return (ok: (v: unknown) => void): void => {
              calls += 1;
              ok(undefined);
            };
          }
          touched.push(trap === 'get' || trap === 'has' ? `${String(trap)} ${String(rest[0])}` : String(trap));
          if (trap === 'getPrototypeOf') {
            throw new Error('the prototype was asked for');
          }
          if (trap === 'get') {
            return undefined;
          }
          return (Reflect as unknown as Record<string, (...a: unknown[]) => unknown>)[String(trap)](target, ...rest);
        };
      }
    }) as ProxyHandler<object>);
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: (() => recorded) as unknown as IntegrityQuestion['record']
    });
    await turn();
    await turn();
    assert.strictEqual(reads, 1, `then was read ${reads} times`);
    assert.strictEqual(calls, 1, `then was called ${calls} times`);
    assert.deepStrictEqual(touched, [], 'what record returned was touched beyond reading then');
  });

  /*
   * C19: A NATIVE PROMISE FROM `record` GOES THROUGH THE FRESH PROMISE (I2,
   * record side). `show` refuses; `record` returns a fulfilled native
   * promise whose own `then` is a getter that counts and hands back
   * `Promise.prototype.then`, and whose `constructor` getter throws. The
   * fresh promise's resolution reads `then` once. `Promise.resolve` on the
   * same value reads `constructor` first -- it throws, write's guard
   * swallows it, and `then` is never read (Q1, measured: 0 reads). C18
   * cannot tell the two apart: on a thenable that is not a native promise,
   * `Promise.resolve` reads and calls `then` once as well.
   */
  it('C19 hands a native promise record returned to the fresh promise too', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let thenReads = 0;
    const recorded = Promise.resolve('written');
    Object.defineProperty(recorded, 'then', {
      get(): unknown {
        thenReads += 1;
        return Promise.prototype.then;
      }
    });
    Object.defineProperty(recorded, 'constructor', {
      get(): never {
        throw new Error('the constructor was asked for');
      }
    });
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: (() => recorded) as unknown as IntegrityQuestion['record']
    });
    await turn();
    await turn();
    assert.strictEqual(thenReads, 1, `then was read ${thenReads} times`);
    /*
     * THE SAME VALUE ON SHOW'S SIDE (N09): the fresh promise reads `then`
     * once; `Promise.all([returned])` (N09, measured) goes through
     * `Promise.resolve`, reads `constructor` first, and never reads `then`.
     */
    let showReads = 0;
    const shown = Promise.resolve('shown');
    Object.defineProperty(shown, 'then', {
      get(): unknown {
        showReads += 1;
        return Promise.prototype.then;
      }
    });
    Object.defineProperty(shown, 'constructor', {
      get(): never {
        throw new Error('the constructor was asked for');
      }
    });
    const w2 = windowOn({ '/stores/A': DAMAGED });
    await new IntegrityWatch().check({ ...w2.question('/stores/A'), show: () => shown });
    await turn();
    await turn();
    assert.strictEqual(showReads, 1, `(show side) then was read ${showReads} times`);
  });

  /*
   * C20: A `then` THAT THROWS ON ITS SECOND READ LEAVES NOTHING OF THE
   * WATCH'S UNHANDLED (I7, design A5). The value's `then` getter answers
   * `undefined` the first time and throws the second; it creates no promise,
   * so any unhandled rejection is the watch's. On both sides -- as the value
   * `show` returns, and as the value `record` returns after a refusal. With
   * the fulfilment passed through (F0r, the entry as it was until r11,
   * measured) the derived promise reads `then` again and rejects, unheld.
   */
  it('C20 leaves nothing unhandled when then throws on a second read, on both sides', async () => {
    for (const side of ['show', 'record']) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const unhandled = countingUnhandled();
      let reads = 0;
      const value = {
        get then(): undefined {
          reads += 1;
          if (reads === 1) {
            return undefined;
          }
          throw new Error('then read a second time');
        }
      };
      await watch.check({
        ...w.question('/stores/A'),
        show: side === 'show' ? () => value as unknown as PromiseLike<unknown> : () => Promise.reject(new Error('refused')),
        record: (side === 'record' ? () => value : () => undefined) as unknown as IntegrityQuestion['record']
      });
      await turn();
      await turn();
      assert.strictEqual(reads, 1, `(${side}) then was read ${reads} times`);
      assert.strictEqual(unhandled.stop(), 0, `(${side}) a rejection was left unhandled`);
    }
  });

  /*
   * C21: THE SYNCHRONOUS-THROW BRANCH OF WRITE GETS THE SAME HOSTILE VALUES
   * (I2, I3 -- both branches of write; design A6). `show` throws at once, so
   * the line is written from the catch, not from the refusal handler; `record`
   * returns, in turn: C18's Proxy (then read once and called once, nothing
   * else touched), C19's native promise whose constructor getter throws (then
   * read once), a native promise whose own then getter counts and whose
   * constructor is ordinary (then read once -- the built-in `then` called on
   * it directly never reads its own `then`; on C19's fixture the built-in
   * call throws at the constructor and K9 falls back to the follow, so that
   * fixture alone cannot catch it), and C20's then that throws on a
   * second read (read once, nothing unhandled). Promise.resolve in this
   * branch only (K1), a property descriptor read here (K3) and the built-in
   * then (K9) are each red here (measured).
   */
  it('C21 treats what record returns the same way when show throws at once', async () => {
    const fixtures: Array<[string, () => { value: unknown; verify: () => void }]> = [
      ['a Proxy that counts every trap', () => {
        const touched: string[] = [];
        let reads = 0;
        let calls = 0;
        const value = new Proxy({}, new Proxy({}, {
          get(_h: object, trap: string | symbol): unknown {
            return (target: object, ...rest: unknown[]): unknown => {
              if (trap === 'get' && rest[0] === 'then') {
                reads += 1;
                return (ok: (v: unknown) => void): void => {
                  calls += 1;
                  ok(undefined);
                };
              }
              touched.push(trap === 'get' || trap === 'has' ? `${String(trap)} ${String(rest[0])}` : String(trap));
              if (trap === 'getPrototypeOf') {
                throw new Error('the prototype was asked for');
              }
              if (trap === 'get') {
                return undefined;
              }
              return (Reflect as unknown as Record<string, (...a: unknown[]) => unknown>)[String(trap)](target, ...rest);
            };
          }
        }) as ProxyHandler<object>);
        return {
          value,
          verify: () => {
            assert.strictEqual(reads, 1, `then was read ${reads} times`);
            assert.strictEqual(calls, 1, `then was called ${calls} times`);
            assert.deepStrictEqual(touched, [], 'what record returned was touched beyond reading then');
          }
        };
      }],
      ['a native promise whose constructor getter throws', () => {
        let reads = 0;
        const value = Promise.resolve('written');
        Object.defineProperty(value, 'then', {
          get(): unknown {
            reads += 1;
            return Promise.prototype.then;
          }
        });
        Object.defineProperty(value, 'constructor', {
          get(): never {
            throw new Error('the constructor was asked for');
          }
        });
        return { value, verify: () => assert.strictEqual(reads, 1, `then was read ${reads} times`) };
      }],
      ['a native promise whose own then is a counting getter', () => {
        let reads = 0;
        const value = Promise.resolve('written');
        Object.defineProperty(value, 'then', {
          get(): unknown {
            reads += 1;
            return Promise.prototype.then;
          }
        });
        return { value, verify: () => assert.strictEqual(reads, 1, `then was read ${reads} times`) };
      }],
      ['a then that throws on its second read', () => {
        let reads = 0;
        const value = {
          get then(): undefined {
            reads += 1;
            if (reads === 1) {
              return undefined;
            }
            throw new Error('then read a second time');
          }
        };
        return { value, verify: () => assert.strictEqual(reads, 1, `then was read ${reads} times`) };
      }]
    ];
    for (const [what, make] of fixtures) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const unhandled = countingUnhandled();
      const { value, verify } = make();
      await watch.check({
        ...w.question('/stores/A'),
        show: () => {
          throw new Error('refused at once');
        },
        record: (() => value) as unknown as IntegrityQuestion['record']
      });
      await turn();
      await turn();
      try {
        verify();
      } catch (error) {
        throw new Error(`(${what}) ${(error as Error).message}`);
      }
      assert.strictEqual(unhandled.stop(), 0, `(${what}) a rejection was left unhandled`);
    }
  });

  /*
   * C22: ONE SUBSCRIPTION PER CALL, NOT PER VALUE (I2). Two stores' shows
   * return the same thenable object: each check subscribes to it, and a
   * refusal delivered to both unmarks both. Following by value identity (K2,
   * measured) subscribes once and leaves B marked.
   */
  it('C22 subscribes once per check even when two shows return the same thenable', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    const refusals: Array<(reason: unknown) => void> = [];
    const shared = {
      then(_ok: unknown, fail: (reason: unknown) => void): void {
        refusals.push(fail);
      }
    };
    await watch.check({ ...w.question('/stores/A'), show: () => shared as PromiseLike<unknown> });
    await watch.check({ ...w.question('/stores/B'), show: () => shared as PromiseLike<unknown> });
    await turn();
    assert.strictEqual(refusals.length, 2, `then was called ${refusals.length} times for two checks`);
    for (const refuse of refusals) {
      refuse(new Error('refused'));
    }
    await turn();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/B'));
    assert.deepStrictEqual(w.asked, { '/stores/A': 2, '/stores/B': 2 }, 'a refused store was not asked again');
  });

  /*
   * C23: THE UNMARK PRECEDES THE LINE (I17). `record` starts a check of the
   * same store synchronously, from inside the refusal handler: that nested
   * check finds the store unmarked and asks. Writing before unmarking (K4,
   * measured) lets the nested check find the mark.
   */
  it('C23 unmarks before it writes, so a record that checks again finds no mark', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let nested: Promise<void> | undefined;
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: () => {
        nested = watch.check(w.question('/stores/A'));
      }
    });
    await turn();
    assert.ok(nested !== undefined, 'record was never called');
    await nested;
    assert.strictEqual(w.asked['/stores/A'], 2, 'the check started from record found the store still marked');
  });

  /*
   * C24: THE REFUSED STORE'S MARK, NOT ANOTHER'S (I5). A is marked first and
   * its show never settles; B is marked second and refused: A stays marked,
   * B is asked again. (C10 refuses the first store; this refuses the second.)
   * Deleting the first mark in the set (K5, measured) unmarks A instead.
   */
  it('C24 unmarks the refused store when another was marked before it', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check({ ...w.question('/stores/A'), show: () => new Promise<never>(() => undefined) });
    await watch.check({ ...w.question('/stores/B'), show: () => Promise.reject(new Error('refused')) });
    await turn();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/B'));
    assert.deepStrictEqual(w.asked, { '/stores/A': 1, '/stores/B': 2 }, 'the wrong store was unmarked');
  });

  /*
   * C25: ONLY SHOW'S REFUSAL UNMARKS (I0, I14). `show` refuses and the line's
   * `record` returns a promise that rejects later -- after the store was asked
   * again, shown and marked: that later mark survives, and nothing more is
   * written. A record rejection that unmarks (K6, measured) lets a third
   * check ask again.
   */
  it('C25 keeps a later mark when the line of an earlier refusal fails late', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let failLine: (reason: unknown) => void = () => undefined;
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: () =>
        new Promise<void>((_resolve, reject) => {
          failLine = reject;
        })
    });
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'the refused store was not shown again');
    failLine(new Error('the line failed late'));
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'a late failure of the line undid the later mark');
    assert.strictEqual(w.recorded.length, 0, `${w.recorded.length} lines through the default record`);
  });

  /*
   * C26: A LINE REFUSED WITH A FALSY REASON IS SWALLOWED TOO (I7, I11).
   * `record` returns a promise rejecting with each falsy value: nothing is
   * left unhandled, the store is unmarked by show's refusal, and the next
   * check shows. A handler that rethrows falsy reasons (K8, measured) leaves
   * one unhandled.
   */
  it('C26 swallows a line refused with any falsy reason', async () => {
    for (const reason of [undefined, null, false, 0, -0, NaN, '']) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const unhandled = countingUnhandled();
      await watch.check({
        ...w.question('/stores/A'),
        show: () => Promise.reject(new Error('refused')),
        record: () => Promise.reject(reason)
      });
      await turn();
      await turn();
      const what = describeValue(reason);
      await watch.check(w.question('/stores/A'));
      assert.strictEqual(w.shown.length, 1, `(${what}) the refused store was not shown again`);
      await turn();
      assert.strictEqual(unhandled.stop(), 0, `(${what}) a rejection was left unhandled`);
    }
  });

  /*
   * C27: EVERY REFUSAL IS WRITTEN, NOT ONLY A STORE'S FIRST (I0, I9; design
   * A7). One watch, one store, refused twice: two lines. Writing at most once
   * per store (G0, measured) leaves the second refusal unwritten. (C1 builds a
   * new watch for each reason, so it never refuses one store twice.) And the
   * same watch refusing two stores writes each line of its own -- store,
   * reason, time; a watch that reused its first line (Q1, added behaviour, L
   * by design A9) is red here all the same.
   */
  it('C27 writes a line for each refusal of the same store', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const refusing = { ...w.question('/stores/A'), show: () => Promise.reject(new Error('refused')) };
    await watch.check(refusing);
    await turn();
    await watch.check(refusing);
    await turn();
    assert.strictEqual(w.recorded.length, 2, `${w.recorded.length} lines for two refusals of one store`);
    const w2 = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    await watch.check({ ...w2.question('/stores/A'), show: () => Promise.reject(new Error('first refusal')) });
    await turn();
    await watch.check({ ...w2.question('/stores/B'), show: () => Promise.reject(new Error('second refusal')) });
    await turn();
    assert.ok(w2.recorded[0].includes('/stores/A') && w2.recorded[0].includes('first refusal'), w2.recorded[0]);
    assert.ok(w2.recorded[1].includes('/stores/B') && w2.recorded[1].includes('second refusal'), w2.recorded[1]);
    const timeOf = (line: string): number => Date.parse(line.slice(line.lastIndexOf(' at ') + 4));
    assert.ok(timeOf(w2.recorded[1]) >= timeOf(w2.recorded[0]), 'the second line carries an earlier time than the first');
  });

  /*
   * C28: THE LINE NAMES THE REFUSED STORE (I9). A is marked (its show never
   * settles), B is refused: the one line names B, not A. Naming a store taken
   * from the marks (G1, measured) writes A.
   */
  it('C28 names the refused store in its line when another store is marked', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check({ ...w.question('/stores/A'), show: () => new Promise<never>(() => undefined) });
    await watch.check({ ...w.question('/stores/B'), show: () => Promise.reject(new Error('refused')) });
    await turn();
    assert.strictEqual(w.recorded.length, 1, `${w.recorded.length} lines`);
    assert.ok(w.recorded[0].includes('/stores/B') && !w.recorded[0].includes('/stores/A'), w.recorded[0]);
  });

  /*
   * C29: A REASON THAT IS ITSELF A THENABLE STAYS A REASON (I7). `show`
   * rejects with a thenable that refuses, and -- after a refusal -- `record`'s
   * promise rejects with one: nothing is left unhandled, and the refusal is
   * written. A handler that returns its reason (G3 on show's side, G2 on
   * record's, measured) lets the derived promise adopt the thenable and
   * reject, unheld.
   */
  it('C29 leaves nothing unhandled when a reason is itself a thenable, on both sides', async () => {
    for (const side of ['show', 'record']) {
      const w = windowOn({ '/stores/A': DAMAGED });
      const watch = new IntegrityWatch();
      const unhandled = countingUnhandled();
      const reason = {
        then(_ok: unknown, fail: (r: unknown) => void): void {
          fail(new Error('the reason refused in turn'));
        }
      };
      let lines = 0;
      await watch.check({
        ...w.question('/stores/A'),
        show: side === 'show' ? () => Promise.reject(reason) : () => Promise.reject(new Error('refused')),
        record: side === 'record' ? () => Promise.reject(reason) : () => {
          lines += 1;
        }
      });
      await turn();
      await turn();
      if (side === 'show') {
        assert.strictEqual(lines, 1, `(show) ${lines} lines`);
      }
      assert.strictEqual(unhandled.stop(), 0, `(${side}) a rejection was left unhandled`);
    }
  });

  /*
   * C30: C25'S TWIN -- A LINE THAT SUCCEEDS LATE KEEPS A LATER MARK TOO (I0,
   * I14). `show` refuses; `record` returns a promise that fulfils after the
   * store was asked again, shown and marked: the later mark survives. A line
   * fulfilment that unmarks on the refusal path (G6, measured) lets a third
   * check ask.
   */
  it('C30 keeps a later mark when the line of an earlier refusal succeeds late', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let finishLine: () => void = () => undefined;
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: () =>
        new Promise<void>((resolve) => {
          finishLine = resolve;
        })
    });
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.shown.length, 1, 'the refused store was not shown again');
    finishLine();
    await turn();
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, 'a late success of the line undid the later mark');
  });

  /*
   * C31: C6'S SAVED-CALLBACK CASE ON THE RECORD SIDE (I2, I6, I7). `record`
   * returns a thenable that fulfils with a rejected promise and then refuses:
   * native resolution takes the first signal and adopts the rejected promise,
   * so nothing is left unhandled. Delaying the fulfilment signal (G10,
   * measured) lets the later refusal win, and the adopted promise is never
   * subscribed: one unhandled.
   */
  it('C31 takes the first signal of what record returned and adopts what it fulfils with', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const unhandled = countingUnhandled();
    await watch.check({
      ...w.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: (() => ({
        then(ok: (v: unknown) => void, fail: (r: unknown) => void): void {
          ok(Promise.reject(new Error('fulfilled with a rejection')));
          fail(new Error('too late'));
        }
      })) as unknown as IntegrityQuestion['record']
    });
    await turn();
    await turn();
    assert.strictEqual(unhandled.stop(), 0, 'a rejection was left unhandled');
  });

  /*
   * C32: A FUNCTION WITH A `then` IS A THENABLE (I2). Native resolution treats
   * any object or function whose `then` is callable as a thenable. On show's
   * side a callable thenable that refuses is a refusal (one line, asked
   * again); on record's side a callable thenable is subscribed once. Skipping
   * functions (G4 on show's side, G5 on record's, measured) drops both.
   */
  it('C32 follows a callable thenable on both sides', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    const refusingFunction = Object.assign(() => undefined, {
      then(_ok: unknown, fail: (r: unknown) => void): void {
        fail(new Error('refused'));
      }
    });
    await watch.check({ ...w.question('/stores/A'), show: () => refusingFunction as unknown as PromiseLike<unknown> });
    await turn();
    assert.strictEqual(w.recorded.length, 1, `(show) ${w.recorded.length} lines`);
    await watch.check(w.question('/stores/A'));
    assert.strictEqual(w.asked['/stores/A'], 2, '(show) the refused store was not asked again');
    let calls = 0;
    const recordedFunction = Object.assign(() => undefined, {
      then(ok: (v: unknown) => void): void {
        calls += 1;
        ok(undefined);
      }
    });
    const w2 = windowOn({ '/stores/A': DAMAGED });
    await new IntegrityWatch().check({
      ...w2.question('/stores/A'),
      show: () => Promise.reject(new Error('refused')),
      record: (() => recordedFunction) as unknown as IntegrityQuestion['record']
    });
    await turn();
    assert.strictEqual(calls, 1, `(record) then was called ${calls} times`);
  });

  /*
   * C33: ASK, GENERATION, SHOW AND RECORD ARE CALLED AS METHODS OF THE
   * QUESTION (the interface says so). The question is a class instance whose
   * four methods read `this`: the warning is shown, the refusal is written
   * with its reason, and the store is asked again. Taking a method off the
   * question before calling it (H1 for show, H2 for record, L1 for ask,
   * measured) loses its receiver: the warning is not shown, or the line is
   * lost.
   */
  it('C33 calls ask, generation, show and record as methods of the question', async () => {
    const shown: Notice[] = [];
    const lines: string[] = [];
    let asks = 0;
    class Question {
      public readonly store = '/stores/A';
      public readonly verdict: StoreVerdict = DAMAGED;
      public readonly current = 0;
      public ask(): Promise<StoreVerdict> {
        asks += 1;
        return Promise.resolve(this.verdict);
      }
      public generation(): number {
        return this.current;
      }
      public show(notice: Notice): PromiseLike<unknown> {
        if (this.store === '/stores/A') {
          shown.push(notice);
        }
        return Promise.reject(new Error('refused'));
      }
      public record(line: string): void {
        if (this.store === '/stores/A') {
          lines.push(line);
        }
      }
    }
    const watch = new IntegrityWatch();
    await watch.check(new Question());
    await turn();
    assert.strictEqual(shown.length, 1, 'show was not called on the question');
    assert.strictEqual(lines.length, 1, 'record was not called on the question');
    assert.ok(lines[0].includes('(Error: refused)'), lines[0]);
    await watch.check(new Question());
    assert.strictEqual(asks, 2);
  });

  /*
   * C34: A STORE IS ITS CONFIGURED STRING (the interface says so). Two stores
   * whose paths differ only in case are two stores to the watch: each is asked
   * and each is shown. Whether they are the same store on disk is not the
   * watch's to decide. Folding the case of the keys (J0, measured) takes the
   * second for the first; so does keying by the last path segment (Q2, added
   * behaviour, L by design A9) for two stores of the same name.
   */
  it('C34 keeps two stores whose names differ only in case apart', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/a': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check(w.question('/stores/A'));
    await watch.check(w.question('/stores/a'));
    assert.deepStrictEqual(w.asked, { '/stores/A': 1, '/stores/a': 1 }, 'a store was taken for another');
    assert.strictEqual(w.shown.length, 2, `${w.shown.length} warnings for two stores`);
    const w2 = windowOn({ '/projects/one/store': DAMAGED, '/projects/two/store': DAMAGED });
    await watch.check(w2.question('/projects/one/store'));
    await watch.check(w2.question('/projects/two/store'));
    assert.deepStrictEqual(w2.asked, { '/projects/one/store': 1, '/projects/two/store': 1 }, 'a store was taken for another of the same name');
  });

  /*
   * C35: THE LINE'S TIME IS THE REFUSAL'S (I9). The refusal arrives 5 ms
   * after the check: the time in the line is not earlier than the moment
   * the refusal was delivered. A time taken when the check started (P0,
   * measured) is earlier -- in the editor, a warning is refused when the user
   * dismisses it, which can be long after the check.
   */
  it('C35 writes the time of the refusal, not the time of the check', async () => {
    const w = windowOn({ '/stores/A': DAMAGED });
    const watch = new IntegrityWatch();
    let refusedAt = 0;
    await watch.check({
      ...w.question('/stores/A'),
      show: () =>
        new Promise((_resolve, reject) =>
          setTimeout(() => {
            refusedAt = Date.now();
            reject(new Error('refused later'));
          }, 5)
        )
    });
    await new Promise((resolve) => setTimeout(resolve, 20));
    assert.strictEqual(w.recorded.length, 1, `${w.recorded.length} lines`);
    const at = Date.parse(w.recorded[0].slice(w.recorded[0].lastIndexOf(' at ') + 4));
    assert.ok(refusedAt > 0 && at >= refusedAt, `the line's time ${at} is before the refusal at ${refusedAt}`);
  });

  /*
   * C36: A SHOW THAT THROWS AT ONCE TAKES BACK NO OTHER MARK (I5). A was
   * shown and marked; B's show throws synchronously; A is still marked.
   * Clearing the marks in that branch (P1, measured; M11 clears them on the
   * asynchronous refusal instead) makes A show again.
   */
  it('C36 leaves other stores marked when a show throws at once', async () => {
    const w = windowOn({ '/stores/A': DAMAGED, '/stores/B': DAMAGED });
    const watch = new IntegrityWatch();
    await watch.check(w.question('/stores/A'));
    await watch.check({
      ...w.question('/stores/B'),
      show: () => {
        throw new Error('refused at once');
      }
    });
    await watch.check(w.question('/stores/A'));
    assert.deepStrictEqual(w.asked, { '/stores/A': 1, '/stores/B': 1 }, 'a store shown before was asked again');
  });
});
