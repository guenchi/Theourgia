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
 * A second extension host: a real operating-system process.
 *
 * WHY A PROCESS AND NOT A SECOND OBJECT. The whole of X1c exists
 * because four rounds of in-process ordering could not answer what
 * another WINDOW does, and a cell that made two objects in one process
 * would be testing the thing that was already known to be insufficient.
 * Two hosts means two pids, two session directories, and no shared
 * memory to accidentally order them.
 *
 * THE CHILD IS SCRIPTED, AND IT REPORTS WHAT IT DID. A step list goes in
 * as JSON; every step's result comes back on stdout as one JSON line,
 * so a cell can assert the ORDER of what happened and not merely the
 * final state -- C18 exists because a final state is also reached by an
 * implementation that dropped one of the operations.
 *
 * CRASHES ARE A STEP. `{ "crash": "after-sidecar" }` makes the child
 * call `process.exit(9)` at that named point, which is a death with no
 * unwinding: no finally, no flush, no atexit. A cell that "simulated" a
 * crash by throwing would be testing the cleanup path it is trying to
 * prove unnecessary.
 *
 * EVERY RUN HAS A DEADLINE. A harness that catches hangs has to have a
 * timeout of its own, or the first hang it finds is its own; the child
 * is killed and the partial output is returned rather than discarded,
 * because the steps that did complete are the evidence.
 */

import { spawn } from 'child_process';
import * as path from 'path';

export interface Step {
  [key: string]: unknown;
}

export interface HostResult {
  /*
   * One entry per step the child reported, in the order it reported
   * them. A step that never reported is absent -- which is what a crash
   * looks like from here, and is exactly what the crash cells assert.
   */
  steps: Array<Record<string, unknown>>;
  code: number | null;
  signal: string | null;
  stderr: string;
  timedOut: boolean;
}

export interface HostOptions {
  storage: string;
  steps: Step[];
  env?: NodeJS.ProcessEnv;
  timeoutMs?: number;
}

const DEFAULT_TIMEOUT_MS = 60000;

export function runHost(options: HostOptions): Promise<HostResult> {
  const script = path.join(__dirname, 'host-child.js');
  return new Promise((resolve) => {
    const child = spawn(
      process.execPath,
      [script, options.storage, JSON.stringify(options.steps)],
      { env: { ...process.env, ...(options.env ?? {}) }, stdio: ['ignore', 'pipe', 'pipe'] }
    );
    const out: Buffer[] = [];
    const err: Buffer[] = [];
    let settled = false;
    let timedOut = false;

    const timer = setTimeout(() => {
      timedOut = true;
      child.kill('SIGKILL');
    }, options.timeoutMs ?? DEFAULT_TIMEOUT_MS);

    child.stdout.on('data', (b: Buffer) => out.push(b));
    child.stderr.on('data', (b: Buffer) => err.push(b));

    const finish = (code: number | null, signal: string | null): void => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timer);
      const text = Buffer.concat(out).toString('utf8');
      const steps: Array<Record<string, unknown>> = [];
      for (const line of text.split('\n')) {
        if (line.trim().length === 0) {
          continue;
        }
        try {
          steps.push(JSON.parse(line) as Record<string, unknown>);
        } catch (e) {
          /*
           * A LINE THAT WILL NOT PARSE IS KEPT, NOT DROPPED. It is
           * usually the child dying mid-write, and that is evidence
           * about where it died.
           */
          steps.push({ unparsed: line });
        }
      }
      resolve({ steps, code, signal, stderr: Buffer.concat(err).toString('utf8'), timedOut });
    };

    child.on('error', (e) => finish(null, `spawn-failed: ${e.message}`));
    child.on('close', (code, signal) => finish(code, signal));
  });
}

/*
 * The pid of a live child that does nothing, for the liveness cells.
 * Returned with a way to end it; the caller must end it, and the cells
 * do so in a `finally`.
 */
export function idleProcess(): { pid: number; stop: () => void } {
  const child = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], {
    stdio: 'ignore'
  });
  return {
    pid: child.pid as number,
    stop: () => {
      child.kill('SIGKILL');
    }
  };
}
