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

import * as fs from 'fs';
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

async function main(): Promise<void> {
  const root = path.resolve(__dirname, '..', '..', '..');
  try {
    const reported = await downloadAndUnzipVSCode();
    const executable = resolveExecutable(reported);
    await runTests({
      vscodeExecutablePath: executable,
      extensionDevelopmentPath: root,
      extensionTestsPath: path.resolve(__dirname, 'index'),
      launchArgs: ['--disable-extensions', '--disable-gpu', '--no-sandbox'],
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
