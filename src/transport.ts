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
 * How a request reaches the core. Two implementations, one interface.
 *
 * A TRANSPORT CARRIES BYTES AND HAS NO OPINION ABOUT ANSWERS. It returns
 * the exit code and the two streams; whether `(error changed ...)` is a
 * failure is the core's rule and is read above this layer. The one thing
 * it decides is whether the request reached the core at all, and that
 * distinction is the reason its failures have their own type: a save
 * that timed out has NOT been refused, and an outbox that could not tell
 * the two apart would drop work on the floor.
 */

import { ChildProcess, spawn } from 'child_process';
import { CoreConfig, clientPath, environmentFor } from './config';
import { coreDirectoryAt } from './fsops';

/*
 * `no-answer` IS NOT `unreadable`. A verb that appends a record and
 * printed nothing left the store in a state this client cannot name:
 * the write may have landed. Reading an empty answer, by contrast, is a
 * complete answer -- no conflicts, no references, no hits -- and the
 * core says so in its README. The two are one exit code apart and mean
 * opposite things, so they are never the same value here.
 */
export type TransportFailure =
  | 'spawn-failed'
  | 'timeout'
  | 'killed'
  | 'no-answer'
  | 'unreadable';

export class TransportError extends Error {
  public readonly failure: TransportFailure;
  public readonly detail: string;

  constructor(failure: TransportFailure, message: string, detail = '') {
    super(message);
    this.name = 'TransportError';
    this.failure = failure;
    this.detail = detail;
  }
}

export interface RawResult {
  argv: string[];
  rc: number;
  stdout: string;
  stderr: string;
}

/*
 * `input`, WHEN GIVEN, IS THE REQUEST'S STANDARD INPUT. One verb needs it:
 * the core's `batch` always takes its intents from standard input as a
 * positional of its own (arguments.sc, `argument-stdin`), so intents passed
 * as an argument as well are two positionals and the answer is the verb's
 * usage line -- measured on the pinned core. Without it, standard input is
 * closed, as it always was.
 */
export interface Transport {
  readonly kind: string;
  send(verb: string, args: string[], input?: string): Promise<RawResult>;
}

/*
 * THE VERB COMES FIRST AND THE OPTIONS COME LAST. The in-process route
 * (`main`, core.sc) takes its verb from the first argument, so a store
 * option placed ahead of the verb is read AS the verb and refused. The thin
 * client this extension runs (`scan`, theourgia.sc:85) takes `--store` and
 * `--actor` wherever they stand before a `--` (after it, everything is an
 * argument: theourgia.sc:101-106) -- so today the order matters on one route
 * and not the other, and it is pinned here in one function so that both
 * routes are handed the same list.
 */
export function buildArgv(config: CoreConfig, verb: string, args: string[]): string[] {
  return [
    config.scheme,
    '--script',
    clientPath(config),
    verb,
    ...args,
    '--store',
    config.store,
    '--actor',
    config.actor
  ];
}

const MISSING_LIBRARY = /library \(([^)]*)\) not found/;

/*
 * HOW LONG A CHILD IS GIVEN TO STOP AFTER BEING ASKED. Long enough for
 * one to flush and exit, short enough that a caller who has already
 * waited out the whole timeout is not waiting again.
 */
export const GRACE_MS = 2000;


/*
 * A MISSING LIBRARY IS A SETTING, NOT A BACKTRACE. Chez reports
 * `Exception: library (igropyr sexpr) not found` and stops; forwarding
 * that text tells a user which library and nothing about what to do,
 * and the thing to do is always the same -- put the directory holding it
 * on theourgia.libDirs.
 */
export function describeStderr(stderr: string): string | null {
  const found = MISSING_LIBRARY.exec(stderr);
  if (found === null) {
    return null;
  }
  return (
    `the core could not load the library (${found[1]}). ` +
    'Add the directory that holds it to the theourgia.libDirs setting; ' +
    'the core imports (igropyr crypto), (igropyr platform) and (igropyr sexpr) ' +
    'besides its own libraries.'
  );
}

export class CliTransport implements Transport {
  public readonly kind = 'cli';
  private readonly config: CoreConfig;
  private readonly env: NodeJS.ProcessEnv;

  constructor(config: CoreConfig, env: NodeJS.ProcessEnv = process.env) {
    this.config = config;
    this.env = environmentFor(config, env, coreDirectoryAt(config.corePath));
  }

  public send(verb: string, args: string[], input?: string): Promise<RawResult> {
    const argv = buildArgv(this.config, verb, args);
    return new Promise<RawResult>((resolve, reject) => {
      let child: ChildProcess;
      try {
        /*
         * NO SHELL. An argument list is passed as a list; composing one
         * command string would make a block whose body contains a quote
         * into a syntax error in a language nobody asked for.
         */
        child = spawn(argv[0], argv.slice(1), {
          env: this.env,
          stdio: [input === undefined ? 'ignore' : 'pipe', 'pipe', 'pipe']
        });
      } catch (e) {
        reject(new TransportError('spawn-failed', `could not start ${argv[0]}`, String(e)));
        return;
      }

      const out: Buffer[] = [];
      const err: Buffer[] = [];
      let settled = false;
      let timedOut = false;

      /*
       * THE STREAMS ARE COLLECTED AS BYTES AND DECODED ONCE. A chunk
       * boundary can fall inside a multi-byte character, and decoding
       * per chunk turns one CJK character into two replacement
       * characters -- in a block's body, which is then saved back.
       */
      child.stdout?.on('data', (chunk: Buffer) => out.push(chunk));
      child.stderr?.on('data', (chunk: Buffer) => err.push(chunk));

      /*
       * THE INPUT IS WRITTEN WHOLE AND THE STREAM CLOSED, so the core's read
       * of standard input reaches its end. A child that exits without
       * reading it breaks the pipe; what happened is then said by the
       * child's exit and its output, below, not by this write.
       */
      if (input !== undefined && child.stdin !== null) {
        child.stdin.on('error', () => undefined);
        child.stdin.end(input, 'utf8');
      }

      /*
       * A CHILD ASKED TO STOP IS NOT A CHILD THAT HAS STOPPED, and the
       * only proof this transport had was `close` -- so a core that
       * ignores the signal, or one that has been suspended, left the
       * caller waiting for ever on a request it had already given up on.
       * The ask is followed by an insistence, and the caller is answered
       * when the second one is due whether or not the child has gone.
       */
      let hardStop: NodeJS.Timeout | null = null;
      const timer = setTimeout(() => {
        timedOut = true;
        child.kill('SIGTERM');
        hardStop = setTimeout(() => {
          child.kill('SIGKILL');
          finish(() => {
            reject(
              new TransportError(
                'timeout',
                `the core did not answer within ${this.config.timeoutMs} ms and did not stop when asked`,
                Buffer.concat(err).toString('utf8')
              )
            );
          });
        }, GRACE_MS);
      }, this.config.timeoutMs);

      const finish = (fn: () => void): void => {
        if (settled) {
          return;
        }
        settled = true;
        clearTimeout(timer);
        if (hardStop !== null) {
          clearTimeout(hardStop);
        }
        fn();
      };

      child.on('error', (e: Error) => {
        finish(() => {
          reject(new TransportError('spawn-failed', `could not start ${argv[0]}`, e.message));
        });
      });

      child.on('close', (code: number | null, signal: string | null) => {
        const stdout = Buffer.concat(out).toString('utf8');
        const stderr = Buffer.concat(err).toString('utf8');
        finish(() => {
          if (timedOut) {
            reject(
              new TransportError(
                'timeout',
                `the core did not answer within ${this.config.timeoutMs} ms and was stopped`,
                stderr
              )
            );
            return;
          }
          if (code === null) {
            reject(
              new TransportError('killed', `the core was stopped by ${signal ?? 'a signal'}`, stderr)
            );
            return;
          }
          const hint = describeStderr(stderr);
          if (hint !== null && stdout.trim().length === 0) {
            reject(new TransportError('spawn-failed', hint, stderr));
            return;
          }
          resolve({ argv, rc: code, stdout, stderr });
        });
      });
    });
  }
}

/*
 * THERE IS NO SOCKET ADAPTER HERE ANY MORE, AND THAT IS THE DESIGN.
 *
 * One used to sit here, unimplemented, so that this interface had two
 * implementors from the start. It became unreachable when the transport
 * setting lost its `socket` value -- no setting selected it, no cell
 * could drive it, and a class nothing can reach is a class whose passing
 * cell measures nothing. It is gone rather than kept.
 *
 * The socket is the core's, and the thin client is its adapter: a second
 * implementation of that envelope in TypeScript would be a third packer
 * of the same bytes, kept in step by hand. Ruled by the main session.
 */
export function transportFor(config: CoreConfig, env: NodeJS.ProcessEnv = process.env): Transport {
  return new CliTransport(config, env);
}
