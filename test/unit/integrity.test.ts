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

describe('plugin-r3 a store that says it is not sound', () => {
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
   * `ok` (`rpc-ok?`, rpc.ss in the pinned core). So a damaged store
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
