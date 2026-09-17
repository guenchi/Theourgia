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
 * Setting up a fake core for one cell.
 *
 * EACH CELL GETS ITS OWN DIRECTORY, named by a counter and the process
 * id rather than by the process id alone: a scratch directory named only
 * by pid is reused the moment the operating system reuses the number,
 * and two runs then share a script file and a call log.
 */

import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { CoreConfig, DEFAULT_TIMEOUT_MS } from '../../src/config';

let counter = 0;

export interface ScriptedCall {
  name?: string;
  match?: string[];
  contains?: string[];
  stdout?: string;
  stderr?: string;
  rc?: number;
  delayMs?: number;
  exitWithoutAnswer?: boolean;
  once?: boolean;
  chunkAt?: number[];
}

export interface LoggedCall {
  argv: string[];
  coreArgv: string[];
  env: { CHEZSCHEMELIBDIRS: string | null; CHEZSCHEMELIBEXTS: string | null };
  event: string;
  name: string | null;
  watched: string | null;
  signal?: string;
  rc?: number;
  wrote?: string;
  started: number;
  at: number;
}

export class FakeCore {
  public readonly root: string;
  public readonly scriptFile: string;
  public readonly logFile: string;
  public readonly corePath: string;
  public readonly store: string;

  constructor(calls: ScriptedCall[], workingProjection?:{prefix:string;body:string}) {
    counter += 1;
    this.root = fs.mkdtempSync(path.join(os.tmpdir(), `theourgia-cell-${process.pid}-${counter}-`));
    this.scriptFile = path.join(this.root, 'script.json');
    this.logFile = path.join(this.root, 'calls.jsonl');
    this.corePath = path.join(this.root, 'core');
    this.store = path.join(this.root, 'store');
    fs.mkdirSync(this.corePath, { recursive: true });
    fs.mkdirSync(this.store, { recursive: true });
    /*
     * The stand-in never reads it, but the client composes a path to it
     * and a cell pins that path; a file that is not there would make the
     * pin pass for the wrong reason.
     */
    fs.writeFileSync(path.join(this.corePath, 'cli.ss'), ';; stand-in\n', 'utf8');
    fs.writeFileSync(this.scriptFile, JSON.stringify({ calls, workingProjection }, null, 2), 'utf8');
  }

  public config(overrides: Partial<CoreConfig> = {}): CoreConfig {
    return {
      scheme: path.join(__dirname, '..', 'fake-core.js'),
      corePath: this.corePath,
      libDirs: [],
      store: this.store,
      actor: 'cell',
      timeoutMs: DEFAULT_TIMEOUT_MS,
      transport: 'cli',
      ...overrides
    };
  }

  /*
   * `watch` names a file the stand-in reads at the moment a request
   * arrives, and records what it held. That is the only vantage point
   * from which "written down before it was sent" is observable.
   */
  public env(watch?: string): NodeJS.ProcessEnv {
    return {
      ...process.env,
      FAKE_CORE_SCRIPT: this.scriptFile,
      FAKE_CORE_LOG: this.logFile,
      ...(watch === undefined ? {} : { FAKE_CORE_WATCH: watch })
    };
  }

  /*
   * A stand-in that records the signal and stays. The client has to stop
   * it some other way or answer without it.
   */
  public stubbornEnv(): NodeJS.ProcessEnv {
    return { ...this.env(), FAKE_CORE_IGNORE_SIGNALS: '1' };
  }

  public calls(): LoggedCall[] {
    let text: string;
    try {
      text = fs.readFileSync(this.logFile, 'utf8');
    } catch (e) {
      return [];
    }
    return text
      .split('\n')
      .filter((line) => line.trim().length > 0)
      .map((line) => JSON.parse(line) as LoggedCall);
  }

  public requests(): string[][] {
    return this.calls()
      .filter((c) => c.event === 'answer' || c.event === 'unscripted')
      .map((c) => c.coreArgv);
  }

  public outboxFile(): string {
    return path.join(this.root, 'outbox.json');
  }

  /*
   * The process ids this stand-in reported. Every run records its own,
   * so a cell can ask afterwards whether any of them is still there.
   */
  public pidsSeen(): number[] {
    return this.calls()
      .map((c) => (c as unknown as { pid?: number }).pid)
      .filter((p): p is number => typeof p === 'number');
  }

  public dispose(): void {
    fs.rmSync(this.root, { recursive: true, force: true });
  }
}

/*
 * WHICH OF THIS STAND-IN'S PROCESSES ARE STILL ALIVE. A caller that
 * stopped waiting has not necessarily stopped the process -- and that
 * difference is the whole of what the escalation to SIGKILL buys, so it
 * needs a witness that is not the child itself. A killed process cannot
 * write a line saying so.
 */
export function childrenStillRunning(core: FakeCore): number[] {
  const out: number[] = [];
  for (const pid of core.pidsSeen()) {
    try {
      process.kill(pid, 0);
      out.push(pid);
    } catch (e) {
      /*
       * ONLY ESRCH PROVES ABSENCE. A lookup refused for any other reason
       * -- EPERM, say -- says nothing about whether the process is
       * there, and treating it as "gone" would turn the one assertion
       * this helper exists for into a formality.
       */
      if ((e as NodeJS.ErrnoException).code !== 'ESRCH') {
        throw new Error(
          `could not tell whether process ${pid} is still running: ${(e as Error).message}`
        );
      }
    }
  }
  return out;
}
