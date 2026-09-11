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
import { Client } from '../../src/client';
import { LIBRARY_EXTENSIONS } from '../../src/config';
import { CliTransport, SocketTransport, TransportError } from '../../src/transport';
import { clause, headName, initWire, isSym } from '../../src/wire';
import { FakeCore } from '../support/fake';

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
