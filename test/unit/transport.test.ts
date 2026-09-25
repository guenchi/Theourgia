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
 * T1 - T4 of the X1a cell list, against the stand-in core.
 */

import * as assert from 'assert';
import * as path from 'path';
import { Client, appendsARecord } from '../../src/client';
import {
  CLIENT_PROGRAM,
  LIBRARY_EXTENSIONS,
  PRODUCT_EXTENSIONS,
  WITNESS_PRODUCT,
  WITNESS_SOURCE,
  clientPath,
  coreFormOf,
  environmentFor,
  libraryExtensionsFor
} from '../../src/config';
import { CliTransport, GRACE_MS, TransportError } from '../../src/transport';
import { readBlock, stringField } from '../../src/blocks';
import { answerOf, initWire, isSym } from '../../src/wire';
import { FakeCore, childrenStillRunning } from '../support/fake';

describe('T1 an answer and a verdict are separate things', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('keeps stdout when the exit code is not zero', async () => {
    core = new FakeCore([
      { match: ['set'], stdout: '(error changed (current "h"))\n', rc: 1 }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('set', ['a.2', 'src', 'body']);
    assert.strictEqual(answer.rc, 1);
    assert.strictEqual(answer.ok, false);
    assert.strictEqual(answer.answers.length, 1);
    assert.notStrictEqual(answerOf(answer.answers[0], 'error'), null);
    const current = answerOf(answer.answers[0], 'error')?.whole('current');
    assert.ok(current !== undefined && current.read);
    assert.strictEqual(current.items[1], 'h');
  });

  it('does not turn a refusal by the core into a transport failure', async () => {
    core = new FakeCore([{ match: ['read'], stdout: '(error unknown-id "nope" (nearest ()))\n', rc: 1 }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('read', ['nope']);
    assert.strictEqual(answer.ok, false);
    assert.ok(isSym((answer.answers[0] as unknown[])[1], 'unknown-id'));
  });

  it('refuses to call a write confirmed when the core exited non-zero having printed ok', async () => {
    core = new FakeCore([{ match: ['set'], stdout: '(ok (cursor ("w" . 9)))\n', rc: 1 }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('set', ['a.2', 'src', 'body']);
    assert.strictEqual(answer.ok, false, 'the exit code is the verdict, not the head symbol');
  });
});

describe('T2 an empty answer means one thing for a read and another for a write', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('reads no output with a zero code as a complete, empty answer', async () => {
    core = new FakeCore([{ match: ['conflicts'], exitWithoutAnswer: true }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('conflicts', []);
    assert.strictEqual(answer.ok, true);
    assert.strictEqual(answer.kind, 'items');
    assert.deepStrictEqual(answer.answers, []);
  });

  it('refuses to read no output from a write as a success', async () => {
    core = new FakeCore([{ match: ['set'], exitWithoutAnswer: true }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    await assert.rejects(
      () => client.request('set', ['a.2', 'src', 'body']),
      (e: unknown) => e instanceof TransportError && e.failure === 'no-answer'
    );
  });

  it('reads an empty outline as empty text rather than as a missing answer', async () => {
    core = new FakeCore([{ match: ['outline'], stdout: '', rc: 0 }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('outline', []);
    assert.strictEqual(answer.ok, true);
    assert.strictEqual(answer.kind, 'text');
    assert.strictEqual(answer.text, '');
  });
});

describe('T3 a core that does not answer is stopped, and noise is not an answer', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('gives up after the configured time and stops the child', async () => {
    core = new FakeCore([{ match: ['outline'], stdout: '- a.1  One\n', rc: 0, delayMs: 5000 }]);
    const client = new Client(new CliTransport(core.config({ timeoutMs: 300 }), core.env()));
    await assert.rejects(
      () => client.request('outline', []),
      (e: unknown) => e instanceof TransportError && e.failure === 'timeout'
    );
    await new Promise((resolve) => setTimeout(resolve, 200));
    const signals = core.calls().filter((c) => c.event === 'signal');
    assert.strictEqual(signals.length, 1, 'the child was not stopped, only stopped being waited for');
    assert.strictEqual(signals[0].signal, 'SIGTERM');
  });

  it('answers, and insists, when the child does not stop when asked', async function () {
    this.timeout(30000);
    core = new FakeCore([{ match: ['outline'], stdout: '- a.1  One\n', rc: 0, delayMs: 60000 }]);
    const client = new Client(new CliTransport(core.config({ timeoutMs: 300 }), core.stubbornEnv()));
    const started = Date.now();
    await assert.rejects(
      () => client.request('outline', []),
      (e: unknown) => e instanceof TransportError && e.failure === 'timeout',
      'the caller was left waiting on a child that ignored the signal'
    );
    const waited = Date.now() - started;
    assert.ok(
      waited < 300 + GRACE_MS + 5000,
      `the caller waited ${waited} ms, well past the timeout and the grace it allows`
    );
    const signals = core.calls().filter((c) => c.event === 'signal');
    assert.ok(signals.length >= 1, 'the child was never asked to stop');
    assert.strictEqual(signals[0].signal, 'SIGTERM', 'the first signal was not the polite one');

    /*
     * AND THE CHILD IS ACTUALLY GONE. Rejecting after the grace period
     * while leaving the process running satisfies every assertion above
     * -- the caller stopped waiting, which is not the same as the core
     * stopping. SIGKILL cannot be caught, so the only witness is that
     * the process is no longer there.
     */
    assert.ok(core.pidsSeen().length >= 1, 'the stand-in never recorded a process to look for');
    await new Promise((resolve) => setTimeout(resolve, 500));
    const alive = core
      .calls()
      .filter((c) => c.event === 'signal')
      .some((c) => c.signal === 'SIGKILL');
    assert.ok(
      !alive,
      'SIGKILL was caught, which it cannot be -- this stand-in is not the one being tested'
    );
    assert.deepStrictEqual(
      childrenStillRunning(core),
      [],
      'the caller was answered but the core it started is still running'
    );
  });

  it('reads stdout with noise on the other stream', async () => {
    core = new FakeCore([
      {
        match: ['conflicts'],
        stdout: '(orphan "a.2")\n',
        stderr: 'warning: something entirely unrelated\n',
        rc: 0
      }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('conflicts', []);
    assert.strictEqual(answer.answers.length, 1);
    assert.notStrictEqual(answerOf(answer.answers[0], 'orphan'), null);
    assert.ok(answer.stderr.includes('entirely unrelated'));
  });

  it('names the setting when the core cannot find a library', async () => {
    core = new FakeCore([
      {
        match: ['outline'],
        stdout: '',
        stderr: 'Exception: library (igropyr sexpr) not found\n',
        rc: 1
      }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    await assert.rejects(
      () => client.request('outline', []),
      (e: unknown) =>
        e instanceof TransportError &&
        e.failure === 'spawn-failed' &&
        e.message.includes('theourgia.libDirs') &&
        e.message.includes('igropyr sexpr')
    );
  });
});

describe('T4 the argument vector and the environment are what the core expects', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * NOTE: THE PROGRAM IN THIS ROW CHANGED WITH plugin-r2 AND THE ORDER DID
   * NOT. What this cell is about is the order -- the verb first, the
   * store and actor last -- because a store option placed ahead of the
   * verb is taken AS the verb. The program is now the thin client
   * (design 7.6.53); the assertion about it lives in its own row above,
   * and this one keeps the path only so that the vector is compared
   * whole.
   */
  it('puts the verb first and the store and actor options last', async () => {
    core = new FakeCore([{ match: ['read'], stdout: '(ok ((id . "a.2") (deleted . #f) (fields) (position root . 0) (edges)))\n', rc: 0 }]);
    const config = core.config({ actor: 'someone' });
    const client = new Client(new CliTransport(config, core.env()));
    await client.request('read', ['a.2', '--md']);
    const logged = core.calls().filter((c) => c.event === 'answer');
    assert.strictEqual(logged.length, 1);
    assert.deepStrictEqual(logged[0].argv, [
      '--script',
      path.join(config.corePath, CLIENT_PROGRAM),
      'read',
      'a.2',
      '--md',
      '--store',
      config.store,
      '--actor',
      'someone'
    ]);
  });

  it('passes a value beginning with a dash through as a value', async () => {
    core = new FakeCore([{ match: ['set'], stdout: '(ok (cursor ("w" . 1)))\n', rc: 0 }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    await client.request('set', ['a.2', 'src', '-x --not-an-option']);
    const logged = core.requests();
    assert.deepStrictEqual(logged[0].slice(0, 3), ['set', 'a.2', 'src']);
    assert.strictEqual(logged[0][3], '-x --not-an-option');
  });

  it('carries the library path and the extension list the core needs', async () => {
    core = new FakeCore([{ match: ['check'], stdout: '(check (verdict ok))\n', rc: 0 }]);
    const extra = path.join(core.root, 'libs');
    const config = core.config({ libDirs: [extra] });
    const client = new Client(new CliTransport(config, core.env()));
    await client.request('check', []);
    const logged = core.calls().filter((c) => c.event === 'answer')[0];
    assert.strictEqual(logged.env.CHEZSCHEMELIBDIRS, `${config.corePath}:${extra}`);
    assert.strictEqual(logged.env.CHEZSCHEMELIBEXTS, LIBRARY_EXTENSIONS);
    assert.ok(
      !(logged.env.CHEZSCHEMELIBEXTS ?? '').includes('.so'),
      'a stale compiled object beside the source would be loaded in preference to it'
    );
  });

  /*
   * THE CELL FOR THE SOCKET TRANSPORT IS GONE WITH THE CLASS.
   *
   * It asserted that an unimplemented adapter refused rather than
   * quietly falling back to the command line -- a good thing to assert
   * while a setting could select it. When `transport` lost its `socket`
   * value the class became unreachable from anywhere, and a green cell
   * over a class nothing can reach measures nothing at all. Both are
   * gone; the socket belongs to the core's thin client, and a second
   * implementation of its envelope here would be a third packer of the
   * same bytes. Ruled by the main session.
   */
});

/*
 * T1 OF plugin-r2: THE PROGRAM IS THE THIN CLIENT, AND WHO WE ARE
 * TRAVELS IN THE ENVIRONMENT. (design 7.6.50, 7.6.53)
 *
 * The extension used to run `cli.ss`, which loads the whole core into a
 * fresh process for every request. `theourgia.ss` is the thin client:
 * it knows the transport and nothing else, finds or starts the daemon,
 * and prints what the daemon rendered. Measured on this machine against
 * the g-r5 core, one `outline`: 618 ms through `cli.ss` from source,
 * 56 ms through `cli.ss` from a product directory, 35 ms through the
 * thin client and a daemon.
 *
 * NOTE: THE IDENTITIES GO IN THE ENVIRONMENT, NOT IN THE ARGUMENT VECTOR.
 * The client scans argv for four transport options and passes the rest
 * through untouched; `--writer` is deliberately not one of them, and a
 * writer spliced into argv by this extension would arrive at a verb
 * whose option table does not take it and come back as usage (design
 * 7.6.50 v262 -- it happened to the core's own shell and made it
 * unusable).
 */
describe('plugin-r2 T1 the thin client is the program, and identity is bound by the environment', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * KEY: THIS IS THE ONE CELL THAT SPELLS THE NAMES, and it is meant to.
   *
   * The core ruled two renamings -- every library `.ss` became `.sc`
   * (276d9f2), and the entry points split so that `cli.ss` went away
   * (e55b680, 877f0da) -- so the names live in `src/config.ts` and
   * everything else in this tree reads them from there. A rename then
   * edits one file and turns this cell red, which is a reading somebody
   * has to look at, rather than being followed silently everywhere at
   * once. It went red twice on 2026-09-25 and both were read: for the
   * renaming, `'theourgia.sc'` where `'theourgia.ss'` was expected; for
   * the deletion of the `transport` setting, a compile error naming
   * `FALLBACK_PROGRAM` here and in `test/support/fake.ts`. There is no
   * second program any more, so the cell names one.
   */
  it('runs theourgia.sc, and nothing else', async () => {
    assert.strictEqual(CLIENT_PROGRAM, 'theourgia.sc');
    assert.strictEqual(WITNESS_SOURCE, 'client.sc');
    assert.strictEqual(WITNESS_PRODUCT, 'client.so');
    core = new FakeCore([{ match: ['outline'], stdout: '- a.1  One\n', rc: 0 }]);
    const config = core.config();
    const client = new Client(new CliTransport(config, core.env()));
    await client.request('outline', []);
    const logged = core.calls().filter((c) => c.event === 'answer');
    assert.strictEqual(logged.length, 1);
    assert.strictEqual(
      logged[0].argv[1],
      path.join(config.corePath, CLIENT_PROGRAM),
      'the extension is still starting the whole core for every request'
    );
    assert.strictEqual(clientPath(config), path.join(config.corePath, CLIENT_PROGRAM));
  });

  it('carries the actor and the writer in the environment and not in the argument vector', async () => {
    core = new FakeCore([{ match: ['write'], stdout: '(ok (version "v1"))\n', rc: 0 }]);
    const config = core.config({ actor: 'someone', writer: 'a-draft-space' });
    const client = new Client(new CliTransport(config, core.env()));
    await client.request('write', ['a.2', 'src', 'text']);
    const logged = core.calls().filter((c) => c.event === 'answer')[0];
    assert.strictEqual(logged.env.THEOURGIA_ACTOR, 'someone');
    assert.strictEqual(logged.env.THEOURGIA_WRITER, 'a-draft-space');
    assert.ok(
      !logged.argv.includes('--writer'),
      'a writer spliced into the argument vector reaches verbs whose option table refuses it'
    );
  });

  /*
   * NOTE: THE WRITER DOES NOT FALL BACK TO ANYTHING IN THE CORE, and it
   * does here -- for a different reason and at a different layer. The
   * core refuses an unbound writer (`writer-required`) so that two
   * agents cannot silently share one draft space; this extension is ONE
   * agent, one window, and the setting's default is the actor's name so
   * that a user who has not thought about draft spaces still has one.
   * Two windows that want separate spaces set the setting.
   */
  it('defaults the writer to the actor, since one window is one agent', () => {
    core = new FakeCore([]);
    const config = core.config({ actor: 'someone', writer: '' });
    const env = environmentFor(config, {}, { has: (n: string) => n === WITNESS_SOURCE });
    assert.strictEqual(env.THEOURGIA_WRITER, 'someone');
  });
});

/*
 * NOTE: WHICH EXTENSION LIST DEPENDS ON WHAT THE DIRECTORY IS, and both
 * answers are needed for a reason that has a reading behind it.
 *
 * Measured on this machine, g-r5 core:
 *   - the no-object list against a product directory:
 *     `Exception: library (theourgia client) not found` -- so "corePath
 *     may be a product directory" is unreachable under it;
 *   - `.so` first against a source tree holding a stale object: a
 *     `trace.ss` replaced by a line that is not Scheme at all was
 *     ignored, the stale `trace.so` ran, and `outline` answered
 *     normally. The hazard the no-object list was written for is real
 *     and it is silent.
 *
 * So the list is chosen from what the directory holds, and when both
 * are there the source wins -- that IS the stale-object case, and the
 * safe direction is the loud one. `client` is the witness because it is
 * the library the thin client itself imports; its form is the form the
 * client will run in.
 */
describe('plugin-r2 T1 the extension list is chosen from what the core directory holds', () => {
  const held = (...names: string[]) => ({ has: (n: string) => names.includes(n) });

  it('calls a directory of sources a source directory', () => {
    assert.deepStrictEqual(coreFormOf(held(WITNESS_SOURCE, CLIENT_PROGRAM)), { form: 'source' });
    assert.strictEqual(libraryExtensionsFor({ form: 'source' }), LIBRARY_EXTENSIONS);
    assert.ok(!LIBRARY_EXTENSIONS.includes('.so'));
  });

  it('calls a directory of objects a product directory', () => {
    assert.deepStrictEqual(coreFormOf(held(WITNESS_PRODUCT, CLIENT_PROGRAM)), { form: 'product' });
    assert.strictEqual(libraryExtensionsFor({ form: 'product' }), PRODUCT_EXTENSIONS);
    assert.ok(PRODUCT_EXTENSIONS.startsWith('.so'));
  });

  it('prefers the source when a stale object is lying beside it', () => {
    assert.deepStrictEqual(
      coreFormOf(held(WITNESS_SOURCE, WITNESS_PRODUCT, CLIENT_PROGRAM)),
      { form: 'source' },
      'the object would be loaded in preference to the source it no longer matches'
    );
  });

  /*
   * NOTE: AND A DIRECTORY THAT IS NEITHER IS SAID, NOT GUESSED. Picking a
   * list for it would send the user a library-not-found exception from
   * inside Chez about a path they would have to work backwards from.
   */
  it('says so when the directory holds neither', () => {
    assert.deepStrictEqual(coreFormOf(held('readme.txt')), { form: 'neither' });
  });
});

describe('T5 stdout arriving in pieces is one answer', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  /*
   * THE BREAKS ARE PLACED WHERE THEY DO DAMAGE. A break between two
   * whole characters proves nothing: any implementation survives it. The
   * two that catch a client decoding per chunk are a break inside a
   * multi-byte character -- which becomes two replacement characters,
   * silently, in a body that is then saved back -- and a break inside an
   * escape sequence, which turns one character into two.
   */
  function breaksInside(stdout: string): number[] {
    const bytes = Buffer.from(stdout, 'utf8');
    const escapeAt = bytes.indexOf(Buffer.from('\\n', 'utf8'));
    const wideAt = bytes.indexOf(Buffer.from('中', 'utf8'));
    assert.ok(escapeAt > 0, 'the fixture has no escape to break inside');
    assert.ok(wideAt > 0, 'the fixture has no multi-byte character to break inside');
    /*
     * THE BREAK POINTS ARE CHECKED TO BE WHERE THEY CLAIM. A cell whose
     * name says "inside an escape" and whose arithmetic lands between
     * two whole characters proves nothing, and nothing else here would
     * notice: every implementation survives a harmless break.
     */
    assert.strictEqual(
      bytes.subarray(escapeAt, escapeAt + 2).toString('utf8'),
      '\\n',
      'the first break is not inside a two-character escape'
    );
    assert.strictEqual(
      bytes.subarray(wideAt, wideAt + 3).toString('utf8'),
      '中',
      'the second break is not inside a three-byte character'
    );
    return [escapeAt + 1, wideAt + 1];
  }

  /*
   * A TEXT ANSWER HAS NO ESCAPES IN IT -- the core prints those bytes as
   * they are -- so the only damaging break available here is inside a
   * multi-byte character, and this cell says so rather than claiming
   * both. The escape case belongs to the datum answer below, which is
   * where a `\n` sequence actually appears on the wire.
   */
  it('reads a text answer broken inside a multi-byte character', async () => {
    const stdout = '- a.1  One中\n  - a.2  Two\n';
    core = new FakeCore([
      { match: ['outline'], stdout, rc: 0, chunkAt: [Buffer.from(stdout, 'utf8').indexOf(Buffer.from('中', 'utf8')) + 1] }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('outline', []);
    assert.strictEqual(answer.text, stdout, 'the text came back changed');
    assert.ok(!answer.text.includes('�'), 'a character was split and replaced');
  });

  it('reads a datum answer broken in both places', async () => {
    const stdout = '(ok ((id . "a.2") (fields (src . "line\\nmore 中文"))))\n';
    core = new FakeCore([{ match: ['read'], stdout, rc: 0, chunkAt: breaksInside(stdout) }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('read', ['a.2']);
    assert.strictEqual(answer.answers.length, 1);
    const block = readBlock((answer.answers[0] as unknown[])[1]);
    assert.ok(block !== null);
    assert.strictEqual(stringField(block as NonNullable<typeof block>, 'src'), 'line\nmore 中文');
  });

  it('reads an items answer whose last piece is flushed by the exit', async () => {
    const stdout = '(orphan "a.1")\n(orphan "a.2")\n(orphan "a.3")\n';
    const bytes = Buffer.from(stdout, 'utf8');
    core = new FakeCore([
      { match: ['conflicts'], stdout, rc: 0, chunkAt: [10, bytes.length - 6] }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('conflicts', []);
    assert.strictEqual(answer.answers.length, 3, 'a piece written at exit was lost');
    assert.strictEqual((answer.answers[2] as unknown[])[1], 'a.3');
  });
});

describe('a verb that appends a record is known by what it does, not by its name', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('treats an empty answer to `tag <name>` as a save whose outcome is unknown', async () => {
    core = new FakeCore([{ match: ['tag', 'release'], exitWithoutAnswer: true }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    await assert.rejects(
      () => client.request('tag', ['release']),
      (e: unknown) => e instanceof TransportError && e.failure === 'no-answer',
      'a tag that writes a record was allowed to answer nothing'
    );
  });

  it('treats an empty answer to `tag` with no argument as a store with no tags', async () => {
    core = new FakeCore([{ match: ['tag'], exitWithoutAnswer: true }]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('tag', []);
    assert.strictEqual(answer.ok, true);
    assert.deepStrictEqual(answer.answers, []);
  });

  it('agrees with the core about which verbs take a request id', () => {
    assert.strictEqual(appendsARecord('set', ['a.2', 'src', 'x']), true);
    assert.strictEqual(appendsARecord('tag', ['release']), true);
    assert.strictEqual(appendsARecord('tag', []), false);
    assert.strictEqual(appendsARecord('read', ['a.2']), false);
    assert.strictEqual(appendsARecord('outline', []), false);
    assert.strictEqual(appendsARecord('conflicts', []), false);
    assert.strictEqual(appendsARecord('check', []), false);
  });
});

/*
 * plugin-r2: an envelope this client asked for, and two of them came
 * back.
 *
 * `interpret` unwraps when the REQUEST carried `--wire` and the head is
 * `ok` -- the repair that stopped it deciding from the shape of what
 * came back. The decoder refuses a duplicated clause, and until a
 * sixteenth review round it refused it by answering null, which this
 * reader could not tell from "no envelope here": the whole form was then
 * handed on as the answers, and a reader downstream took its first
 * element for the answer to the request.
 */
describe('plugin-r2 two envelopes are not one answer', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('refuses an answer carrying two item lists rather than handing on the whole form', async () => {
    core = new FakeCore([
      {
        match: ['read'],
        stdout: '(ok (items ((id . "a.1") (fields (src . "one")))) (items ((id . "a.1") (fields (src . "two")))))\n',
        rc: 0
      }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    await assert.rejects(
      () => client.request('read', ['a.1', '--wire']),
      /two item lists/,
      'an answer with two item lists was handed on instead of refused'
    );
  });

  it('still opens the envelope when there is one of it', async () => {
    core = new FakeCore([
      { match: ['read'], stdout: '(ok (items ((id . "a.1") (fields (src . "one")))))\n', rc: 0 }
    ]);
    const client = new Client(new CliTransport(core.config(), core.env()));
    const answer = await client.request('read', ['a.1', '--wire']);
    assert.strictEqual(answer.answers.length, 1);
    assert.ok(answer.envelope !== null, 'the envelope was not recorded');
  });
});
