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
 * Starting an editor to run the cells that need one.
 *
 * THE EXECUTABLE IS LOOKED FOR RATHER THAN ASSUMED. The downloader
 * returns the path it expects the binary at, and on macOS that path is
 * wrong for current builds: the download unpacks an application whose
 * executable is named `Code`, and the harness composes a path ending in
 * `Electron`. Spawning it fails with ENOENT, which reads like "no
 * editor" rather than "the name changed". So the returned path is
 * checked and the directory is searched for the names a VS Code build
 * has used, and a failure to find one says which directory was looked
 * in.
 *
 * THE CORE'S LOCATION IS PASSED THROUGH. The extension host is a fresh
 * process with its own environment, and the cells inside it build a real
 * store; without these three variables they would fail for the reason
 * the harness failed rather than for anything about the extension.
 */

import { createHash } from 'crypto';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { downloadAndUnzipVSCode, runTests } from '@vscode/test-electron';

const KNOWN_EXECUTABLE_NAMES = ['Code', 'Code - Insiders', 'Electron', 'code', 'code-insiders'];

function resolveExecutable(reported: string): string {
  if (fs.existsSync(reported)) {
    return reported;
  }
  const directory = path.dirname(reported);
  if (!fs.existsSync(directory)) {
    throw new Error(
      `the downloaded editor has no executable directory at ${directory}; ` +
        'delete .vscode-test and run again'
    );
  }
  for (const name of KNOWN_EXECUTABLE_NAMES) {
    const candidate = path.join(directory, name);
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  const present = fs.readdirSync(directory).join(', ');
  throw new Error(
    `no VS Code executable was found in ${directory}. It holds: ${present}. ` +
      '@vscode/test-electron expected one named Electron.'
  );
}

/*
 * WHERE THE TEST HOST'S PROFILE GOES, AND WHY IT IS NOT SIMPLY BESIDE
 * THE REPOSITORY.
 *
 * The editor puts a unix socket inside the profile -- `1.13-main.sock`
 * -- and a unix socket path is limited to about 104 bytes. Measured: a
 * checkout at /Users/<u>/Workshop/theourgia-vsc gives a 75-byte socket
 * path and works; the same code run from a git worktree under a
 * session's scratchpad gave `listen EINVAL: invalid argument ...
 * 1.13-main.sock` and the run died before a single cell, reported only
 * as `Test run failed with code 1`.
 *
 * So the candidates are tried in order -- beside the checkout first,
 * then under the temporary directory -- and the first that fits is
 * taken, with a sentence that says what is wrong when neither does. A
 * limit that shows up as EINVAL from inside the editor is a limit nobody
 * will diagnose from here.
 */
const SOCKET_ALLOWANCE = 24;
const SOCKET_LIMIT = 104;

/*
 * ⚠️ THE LIMIT IS IN BYTES AND THE MEASUREMENT MUST BE TOO. This counted
 * JavaScript string units, which are not bytes: a checkout under a path
 * of non-ASCII characters measured well under the limit and produced a
 * socket path well over it, so the check passed and the editor then died
 * with the opaque failure this exists to prevent. Found in review with
 * an arithmetic example.
 */
function fits(candidate: string): boolean {
  return Buffer.byteLength(candidate, 'utf8') + SOCKET_ALLOWANCE <= SOCKET_LIMIT;
}

function chooseProfile(root: string): string {
  /*
   * THE FALLBACK'S NAME CARRIES THE CHECKOUT IT IS FOR. One fixed name
   * under the temporary directory is shared by every checkout on the
   * machine, so two test hosts started from two trees would contend for
   * one profile -- isolated from the developer's editor and not from
   * each other.
   */
  const mark = createHash('sha256').update(root).digest('hex').slice(0, 8);
  const candidates = [
    path.resolve(root, '.vscode-test', 'user-data'),
    path.join(os.tmpdir(), `theourgia-vsc-test-${mark}`)
  ];
  for (const candidate of candidates) {
    if (fits(candidate)) {
      return candidate;
    }
  }
  throw new Error(
    `no place for the test host's profile is short enough: the editor puts a socket inside it and ` +
      `the path may not exceed about ${SOCKET_LIMIT} bytes. Tried ${candidates.join(' and ')}. ` +
      'Run from a shorter path, or set TMPDIR to one.'
  );
}

async function main(): Promise<void> {
  const root = path.resolve(__dirname, '..', '..', '..');
  const profile = chooseProfile(root);
  try {
    const reported = await downloadAndUnzipVSCode();
    const executable = resolveExecutable(reported);
    await runTests({
      vscodeExecutablePath: executable,
      extensionDevelopmentPath: root,
      extensionTestsPath: path.resolve(__dirname, 'index'),
      /*
       * ⚠️ ITS OWN USER DATA DIRECTORY, AND ITS OWN EXTENSIONS
       * DIRECTORY.
       *
       * Without them the test host tries to claim the same instance as
       * whatever VS Code the developer has open, and refuses to start:
       * `Running extension tests from the command line is currently only
       * supported if no other instance of Code is running`. That is not
       * a failing cell and it is not a passing one -- it is the suite
       * not running at all, on a machine where somebody is working,
       * which is every machine this is ever run on. A suite that can
       * only be run by closing the editor is a suite that stops being
       * run.
       *
       * The directory is beside the downloaded editor rather than in the
       * repository, and is deliberately NOT cleaned between runs: a
       * fresh profile costs a first-run window every time, and nothing
       * these cells assert depends on what is in it.
       */
      launchArgs: [
        '--disable-extensions',
        '--disable-gpu',
        '--no-sandbox',
        '--user-data-dir',
        profile,
        '--extensions-dir',
        path.join(profile, 'extensions')
      ],
      extensionTestsEnv: {
        THEOURGIA_CORE: process.env.THEOURGIA_CORE ?? '',
        THEOURGIA_LIBDIRS: process.env.THEOURGIA_LIBDIRS ?? '',
        THEOURGIA_SCHEME: process.env.THEOURGIA_SCHEME ?? 'scheme'
      }
    });
  } catch (e) {
    process.stderr.write(`the editor-hosted cells could not be run: ${String(e)}\n`);
    process.exit(1);
  }
}

void main();
