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
import { LIBRARY_EXTENSIONS } from '../../src/config';
import { CliTransport, GRACE_MS, SocketTransport, TransportError } from '../../src/transport';
import { readBlock, stringField } from '../../src/blocks';
import { clause, headName, initWire, isSym } from '../../src/wire';
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
    assert.strictEqual(headName(answer.answers[0]), 'error');
    const current = clause(answer.answers[0], 'current');
    assert.ok(current !== null);
    assert.strictEqual((current as unknown[])[1], 'h');
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
    assert.strictEqual(headName(answer.answers[0]), 'orphan');
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

  it('puts the verb first and the store and actor options last', async () => {
    core = new FakeCore([{ match: ['read'], stdout: '(ok ((id . "a.2") (deleted . #f) (fields) (position root . 0) (edges)))\n', rc: 0 }]);
    const config = core.config({ actor: 'someone' });
    const client = new Client(new CliTransport(config, core.env()));
    await client.request('read', ['a.2', '--md']);
    const logged = core.calls().filter((c) => c.event === 'answer');
    assert.strictEqual(logged.length, 1);
    assert.deepStrictEqual(logged[0].argv, [
      '--script',
      path.join(config.corePath, 'cli.ss'),
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

  it('sends nothing at all when the socket transport is selected', async () => {
    core = new FakeCore([{ match: ['outline'], stdout: '', rc: 0 }]);
    const client = new Client(new SocketTransport());
    await assert.rejects(
      () => client.request('outline', []),
      (e: unknown) => e instanceof TransportError && e.failure === 'unsupported'
    );
    assert.deepStrictEqual(core.requests(), []);
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
