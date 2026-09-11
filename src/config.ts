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
 * Where the core is and who we are when we write to it.
 *
 * THIS IS A PLAIN VALUE AND NOT A SETTINGS OBJECT, so that the transport
 * can be run by a test with no extension host around it. The one
 * function that knows about VS Code is `fromWorkspace`, and it is the
 * only thing in this file a unit test cannot call.
 */

import * as os from 'os';
import * as path from 'path';

export type TransportKind = 'cli' | 'socket';

export interface CoreConfig {
  scheme: string;
  corePath: string;
  libDirs: string[];
  store: string;
  actor: string;
  timeoutMs: number;
  transport: TransportKind;
}

export const DEFAULT_TIMEOUT_MS = 30000;

/*
 * NODE'S TIMER TAKES A 32-BIT DELAY AND SILENTLY CLAMPS ANYTHING LARGER
 * TO ONE MILLISECOND. A timeout of a month is therefore a timeout of an
 * instant, and the message the caller is then shown names the month. A
 * setting this client cannot honour is refused where it is read, rather
 * than accepted and turned into its opposite.
 */
export const MAX_TIMEOUT_MS = 2147483647;

/*
 * THE LIBRARY EXTENSIONS CHEZ IS GIVEN LIST `.no-obj` AFTER EVERY SOURCE
 * EXTENSION AND NEVER `.so`. A compiled object beside the source is
 * loaded in preference to the source, and the working tree of a library
 * under development grows stale objects; a run that loaded one would be
 * a reading of code nobody is editing.
 */
export const LIBRARY_EXTENSIONS =
  '.chezscheme.sls::.no-obj:.ss::.no-obj:.sls::.no-obj:.scm::.no-obj:.sch::.no-obj:.sc::.no-obj';

export function cliPath(config: CoreConfig): string {
  return path.join(config.corePath, 'cli.ss');
}

/*
 * THE CORE PATH IS ALWAYS FIRST. The core resolves `(theourgia ...)`
 * from its own directory, and a second directory that happened to hold
 * an older copy would otherwise decide which one runs.
 *
 * THE REST ARE THERE BECAUSE THE CORE DOES NOT STAND ALONE: it imports
 * (igropyr crypto), (igropyr platform) and (igropyr sexpr), so the
 * directory holding `igropyr/` has to be on the path or the core exits
 * before it reads a single argument.
 */
export function libraryDirectories(config: CoreConfig): string[] {
  const out = [config.corePath];
  for (const dir of config.libDirs) {
    if (dir.length > 0 && !out.includes(dir)) {
      out.push(dir);
    }
  }
  return out;
}

export function environmentFor(config: CoreConfig, base: NodeJS.ProcessEnv): NodeJS.ProcessEnv {
  return {
    ...base,
    CHEZSCHEMELIBDIRS: libraryDirectories(config).join(':'),
    CHEZSCHEMELIBEXTS: LIBRARY_EXTENSIONS
  };
}

export function defaultActor(): string {
  try {
    const name = os.userInfo().username;
    if (typeof name === 'string' && name.length > 0) {
      return name;
    }
  } catch (e) {
    /*
     * os.userInfo throws when the uid has no passwd entry, which happens
     * inside some containers. The core has its own fallback and would
     * record "cli"; naming ourselves is better evidence than that.
     */
  }
  return 'vscode';
}

export interface ConfigProblem {
  setting: string;
  message: string;
}

/*
 * WHAT IS MISSING IS REPORTED ALL AT ONCE. A client that stopped at the
 * first empty setting would send the user back to the settings page
 * once per setting.
 */
export function problemsWith(config: CoreConfig): ConfigProblem[] {
  const out: ConfigProblem[] = [];
  if (config.corePath.length === 0) {
    out.push({ setting: 'theourgia.corePath', message: 'no core directory is set' });
  }
  if (config.store.length === 0) {
    out.push({ setting: 'theourgia.store', message: 'no store directory is set' });
  }
  if (!Number.isFinite(config.timeoutMs) || config.timeoutMs <= 0) {
    out.push({ setting: 'theourgia.timeoutMs', message: 'the timeout must be a positive number of milliseconds' });
  } else if (config.timeoutMs > MAX_TIMEOUT_MS) {
    out.push({
      setting: 'theourgia.timeoutMs',
      message:
        `the timeout must be at most ${MAX_TIMEOUT_MS} ms; a larger one is clamped by the ` +
        'timer to a single millisecond, which would stop every request almost at once'
    });
  }
  return out;
}
