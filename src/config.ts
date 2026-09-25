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

export interface CoreConfig {
  scheme: string;
  corePath: string;
  libDirs: string[];
  store: string;
  actor: string;
  /*
   * WHICH DRAFT SPACE THIS WINDOW WRITES INTO. Empty means "the actor's
   * name", which is what one window with one agent in it wants; two
   * windows that need separate draft spaces set it to two names.
   */
  writer: string;
  timeoutMs: number;
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

/*
 * THE FILE NAMES THIS EXTENSION EXPECTS IN A CORE DIRECTORY, in one
 * place.
 *
 * NOTE: THEY CHANGED, AND THIS IS WHERE. The core renamed every library
 * `.ss` to `.sc` (276d9f2) and split its entry points by role (e55b680,
 * 877f0da): `theourgia.sc` the client, `theourgiad.sc` the daemon it
 * starts, `core.sc` the in-process route that `cli.ss` was. Spelling any
 * of these into a fixture or a cell would make that a search across the
 * tree; spelling them here made it an edit here, and one cell pins each
 * name so that the rename was visible in exactly one reading rather than
 * quietly followed.
 */
export const CLIENT_PROGRAM = 'theourgia.sc';
export const WITNESS_SOURCE = 'client.sc';
export const WITNESS_PRODUCT = 'client.so';

/*
 * THE SECOND EXTENSION LIST: OBJECTS FIRST, FOR A DIRECTORY THAT HOLDS
 * NOTHING ELSE. (design 7.6.49, 7.6.53)
 *
 * A delivered core is a product directory -- 101 compiled libraries and
 * three scripts. Its libraries exist only as `.so`, so the list above,
 * which never names `.so`, cannot load them at all: measured against
 * the g-r5 product directory it stops at `Exception: library (theourgia
 * client) not found` before reading an argument.
 */
export const PRODUCT_EXTENSIONS = '.so::.ss::.sls::.sc::.scm';

/*
 * WHAT A CORE DIRECTORY IS, and it is asked rather than configured.
 *
 * NOTE: BOTH LISTS ARE NEEDED AND EACH IS WRONG FOR THE OTHER DIRECTORY.
 * Measured on this machine against the g-r5 core:
 *
 *   - the source list against a product directory: the library is not
 *     found and nothing runs;
 *   - the object list against a source tree holding a stale object: a
 *     `trace.ss` replaced by a line that is not Scheme at all was never
 *     read, the stale `trace.so` ran, and the answer came back looking
 *     perfectly ordinary.
 *
 * So the list follows the directory, and where both forms are present
 * the SOURCE wins -- that combination IS the stale-object case, and of
 * the two ways to be wrong the loud one is the one to choose.
 */
export type CoreForm = { form: 'source' } | { form: 'product' } | { form: 'neither' };

export interface CoreDirectory {
  has(name: string): boolean;
}

/*
 * NOTE: `client` IS THE WITNESS BECAUSE IT IS THE LIBRARY THE THIN CLIENT
 * ITSELF IMPORTS. Asking about a library the client does not need would
 * be asking a question whose answer does not decide anything; this one
 * is the library whose absence the run actually stops on.
 */
export function coreFormOf(directory: CoreDirectory): CoreForm {
  if (directory.has(WITNESS_SOURCE)) {
    return { form: 'source' };
  }
  if (directory.has(WITNESS_PRODUCT)) {
    return { form: 'product' };
  }
  return { form: 'neither' };
}

/*
 * NOTE: A DIRECTORY THAT IS NEITHER GETS NO LIST AND NO GUESS. Picking one
 * would hand the user a library-not-found exception from inside Chez
 * about a path they would have to work backwards from; `problemsWith`
 * names the setting instead.
 */
export function libraryExtensionsFor(form: CoreForm): string {
  return form.form === 'product' ? PRODUCT_EXTENSIONS : LIBRARY_EXTENSIONS;
}

/*
 * THE PROGRAM THE EXTENSION RUNS, and there is one. `theourgia.sc` is the
 * thin client: it knows the transport and nothing else, finds or starts
 * the store's daemon (`theourgiad.sc`, which it launches itself), and
 * prints what the daemon rendered.
 *
 * NOTE: THE `transport` SETTING WAS DELETED, NOT RENAMED. It chose between
 * this and `cli.ss`, which loaded the whole core for every request and
 * was kept for one version as a way back. The core split `cli.ss` by
 * role (877f0da): what is left of it is `core.sc`, the in-process route,
 * and nothing here runs that. A setting whose meaning would have changed
 * with no end-to-end cell to see it is removed rather than kept under an
 * old name.
 */
export function clientPath(config: CoreConfig): string {
  return path.join(config.corePath, CLIENT_PROGRAM);
}

/*
 * WHO THIS WINDOW IS WHEN IT WRITES. (design 7.6.50 v247/v254)
 *
 * NOTE: THE CORE REFUSES AN UNBOUND WRITER AND THIS EXTENSION STILL HAS A
 * DEFAULT, and the two are not in conflict: the core's refusal exists so
 * that two agents given only an actor cannot silently share one draft
 * space. A VS Code window is one agent. Its default is the actor's own
 * name, so a user who has never heard of draft spaces has one; two
 * windows that want separate spaces set `theourgia.writer` to two
 * names.
 */
export function writerFor(config: CoreConfig): string {
  return config.writer.length > 0 ? config.writer : config.actor;
}

export function libraryDirectories(config: CoreConfig): string[] {
  const out = [config.corePath];
  for (const dir of config.libDirs) {
    if (dir.length > 0 && !out.includes(dir)) {
      out.push(dir);
    }
  }
  return out;
}

/*
 * NOTE: THE IDENTITIES TRAVEL HERE AND NOT IN THE ARGUMENT VECTOR. The
 * thin client scans argv for the four options that say WHERE a request
 * goes and passes everything else through untouched; `--writer` is
 * deliberately not one of them, and a writer spliced into argv arrives
 * at a verb whose option table does not take it and comes back as a
 * usage line. That happened to the core's own shell and made it
 * unusable (design 7.6.50 v262).
 */
export function environmentFor(
  config: CoreConfig,
  base: NodeJS.ProcessEnv,
  directory: CoreDirectory
): NodeJS.ProcessEnv {
  return {
    ...base,
    CHEZSCHEMELIBDIRS: libraryDirectories(config).join(':'),
    CHEZSCHEMELIBEXTS: libraryExtensionsFor(coreFormOf(directory)),
    THEOURGIA_ACTOR: config.actor,
    THEOURGIA_WRITER: writerFor(config),
    /*
     * NEVER: THE INTERPRETER THE USER CHOSE HAS TO REACH THE DAEMON TOO.
     *
     * This command line starts the thin client, and the client starts
     * the daemon -- with `THEOURGIA_SCHEME`, or failing that with
     * whatever `scheme` resolves to on PATH (cli.ss:420, and
     * theourgia.ss:136 and :336). So a user who set this setting BECAUSE
     * `scheme` is not on their PATH got a client from the path they gave
     * and a daemon that could not be started at all.
     *
     * Found in a second review round, and the reason no cell had met it
     * is worth keeping: the fixtures take this setting FROM that
     * variable, so it was always already in the environment they passed
     * on. A fixture that inherits the thing under test cannot see it
     * missing.
     */
    THEOURGIA_SCHEME: config.scheme
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
/*
 * NEVER: THE DIRECTORY IS NOT OPTIONAL, AND IT USED TO BE.
 *
 * It defaulted to null and the "neither sources nor products" problem
 * was skipped when it was null -- and the extension called this with one
 * argument, so that problem could never reach anybody. Found in a third
 * review round, and it is the shape a guard takes when nothing can make
 * it fire. The argument is required now, so there is no call that can
 * omit it.
 */
export function problemsWith(
  config: CoreConfig,
  directory: CoreDirectory
): ConfigProblem[] {
  const out: ConfigProblem[] = [];
  if (config.corePath.length === 0) {
    out.push({ setting: 'theourgia.corePath', message: 'no core directory is set' });
  } else if (coreFormOf(directory).form === 'neither') {
    out.push({
      setting: 'theourgia.corePath',
      message:
        `corePath holds neither sources nor products: there is no ${WITNESS_SOURCE} and no ` +
        `${WITNESS_PRODUCT} in it. Point it at a checkout of the core, or at a directory built ` +
        'by its build.ss.'
    });
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
