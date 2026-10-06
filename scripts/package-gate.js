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
 * THE PACKAGING GATE (queue item 9).
 *
 * A vsix once shipped without the goeteia reader, and the extension never
 * activated at all: `activate()` awaits the reader first. Two different
 * packaging routes lose that file, so the gate is about what the package
 * DOES, not about which flags built it. It builds the package, reads its
 * listing, installs it into an editor that is not the user's, starts that
 * editor, and makes one save go through the installed extension to a store.
 *
 * KEY: IT PRINTS READINGS AND DRAWS NO CONCLUSION. Each step says what it
 * ran and what came back; whether that is good enough is for the person
 * reading it. It exits non-zero only when a reading could not be taken.
 *
 * NOTHING HERE TOUCHES THE USER'S EDITOR OR STORES. The editor is the one
 * `@vscode/test-electron` keeps in `.vscode-test/`, started with a profile
 * and an extensions directory of its own; the store, the core's run and
 * home directories and the package are made under one temporary directory,
 * and every directory made is removed at the end, with the count said.
 *
 * Needs THEOURGIA_CORE and THEOURGIA_LIBDIRS, as the real-core cells do.
 * Run after nothing in particular: it compiles first.
 */

const { spawnSync, execFileSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const root = path.join(__dirname, '..');
const EDITOR_VERSION = '1.138.0';
const EXTENSION_ID = 'theourgia.theourgos';
const MARK = `gate line ${process.pid}`;

/*
 * THE FILES THE PACKAGE MUST CARRY FOR THE EXTENSION TO START, by their
 * place in the vsix. The reader is the one that was lost; the native lock
 * and the entry point are the other two things `activate()` loads.
 */
const REQUIRED = [
  'extension/package.json',
  'extension/out/src/extension.js',
  'extension/out/src/native/lease.node',
  'extension/node_modules/goeteia/package.json',
  'extension/node_modules/goeteia/rt/sexpr.mjs'
];

let unreadable = 0;

function say(text) {
  process.stdout.write(`${String(text).split('\n').map((line) => `[gate] ${line}`).join('\n')}\n`);
}

function run(label, command, args, options = {}) {
  const done = spawnSync(command, args, { encoding: 'utf8', ...options });
  /*
   * THE WHOLE OUTPUT WHEN A STEP FAILED, the last lines when it did not: a
   * failure's cause is usually above its last six lines.
   */
  const lines = `${done.stdout ?? ''}${done.stderr ?? ''}`.trim().split('\n');
  const tail = (done.status === 0 ? lines.slice(-6) : lines).join('\n    ');
  say(`${label}: rc ${done.status}${done.error ? ` (${done.error.message})` : ''}`);
  if (tail.length > 0) {
    say(`    ${tail}`);
  }
  if (done.status !== 0) {
    unreadable += 1;
  }
  return done;
}

/*
 * THE DRIVER RUNS INSIDE THE EDITOR, where only the extension API reaches
 * the extension. It is written into the temporary directory, beside the
 * empty extension that carries it, so that nothing of it is left behind.
 */
const DRIVER = `
const vscode = require('vscode');
const fs = require('fs');
exports.run = async () => {
  const out = process.env.GATE_RESULT;
  const result = {};
  const note = (key, value) => { result[key] = value; fs.writeFileSync(out, JSON.stringify(result, null, 2)); };
  try {
    const ext = vscode.extensions.getExtension(${JSON.stringify(EXTENSION_ID)});
    note('found', ext ? { path: ext.extensionPath, version: ext.packageJSON.version } : null);
    if (!ext) { return; }
    await ext.activate();
    note('active', ext.isActive);
    await vscode.commands.executeCommand('theourgia.openBlock', process.env.GATE_BLOCK);
    const editor = vscode.window.activeTextEditor;
    note('opened', editor ? { uri: editor.document.uri.toString(), text: editor.document.getText() } : null);
    if (!editor) { return; }
    const document = editor.document;
    const end = document.lineAt(document.lineCount - 1).range.end;
    await editor.edit((b) => b.insert(end, ${JSON.stringify(`${MARK}\n`)}));
    note('saved', await document.save());
    const out_ = process.env.GATE_OUT;
    const { initWire } = require(out_ + '/src/wire');
    const { Client } = require(out_ + '/src/client');
    await initWire();
    const client = Client.fromConfig(JSON.parse(process.env.GATE_CONFIG));
    const started = Date.now();
    let body = null;
    while (Date.now() - started < 60000) {
      const read = await client.request('read', [process.env.GATE_BLOCK]);
      body = read.text;
      if (body.includes(${JSON.stringify(MARK)})) { break; }
      await new Promise((r) => setTimeout(r, 500));
    }
    note('store-read-after-ms', Date.now() - started);
    note('store-read', body);
  } catch (e) {
    note('error', String((e && e.stack) || e));
  }
};
`;

/*
 * THE EDITOR'S EXECUTABLE, which `@vscode/test-electron` reports as
 * `Electron` and this build names `Code` -- the same lookup the
 * editor-hosted runner makes (test/integration/run.ts).
 */
function resolveExecutable(reported) {
  if (fs.existsSync(reported)) {
    return reported;
  }
  const directory = path.dirname(reported);
  for (const name of ['Code', 'Code - Insiders', 'Electron', 'code', 'code-insiders']) {
    if (fs.existsSync(path.join(directory, name))) {
      return path.join(directory, name);
    }
  }
  throw new Error(`no editor executable in ${directory}`);
}

/*
 * A REFUSED `log` IS NOT A BLOCK WITH NO HISTORY. It is a reading that could
 * not be taken, and it is said as one.
 */
async function logEntries(client, id) {
  const answer = await client.request('log', [id]);
  if (!answer.ok) {
    unreadable += 1;
    return `unreadable (rc ${answer.rc}: ${answer.text.trim().slice(0, 200)})`;
  }
  return (answer.text.match(/\(entry /g) ?? []).length;
}

/*
 * THE DAEMONS THIS RUN STARTED: a command naming this run's core AND a
 * socket inside this run's own root, with the separator, so that a root
 * whose name merely begins the same way is not taken for it.
 */
function daemonsUnder(runRoot, corePath) {
  const listing = execFileSync('ps', ['-ax', '-o', 'pid=,command='], { encoding: 'utf8' });
  return listing
    .split('\n')
    .map((line) => /^\s*(\d+)\s+(.*)$/.exec(line))
    .filter((m) => m !== null && Number(m[1]) !== process.pid && m[2].includes(`--socket ${runRoot}/`) && m[2].includes(corePath))
    .map((m) => Number(m[1]));
}

async function main() {
  /*
   * KEY: NO VARIABLE FROM THE USER'S EDITOR REACHES THIS ONE. The editor
   * takes its profile from VSCODE_PORTABLE or VSCODE_APPDATA BEFORE
   * `--user-data-dir` (its main.js, measured in 1.138), and test-electron
   * hands the editor this process's whole environment -- so a gate started
   * from a terminal inside the user's editor would run in the user's
   * profile, with the user's settings and store. Every VSCODE_* and
   * ELECTRON_* variable is removed first, and their names are said.
   */
  const stripped = Object.keys(process.env).filter((k) => /^(VSCODE_|ELECTRON_)/.test(k)).sort();
  stripped.forEach((k) => delete process.env[k]);
  say(`environment: removed ${stripped.length} editor variable(s)${stripped.length > 0 ? `: ${stripped.join(', ')}` : ''}`);
  const corePath = process.env.THEOURGIA_CORE ?? '';
  const libDirs = (process.env.THEOURGIA_LIBDIRS ?? '').split(':').filter((d) => d.length > 0);
  say(`core: ${corePath || '(THEOURGIA_CORE unset)'}; libDirs: ${libDirs.join(':') || '(none)'}`);

  const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'tvg-'));
  const made = ['p', 'u', 'e', 'd', 's', 'r', 'h'].map((name) => path.join(scratch, name));
  const [packageDir, userData, extensionsDir, stub, storeParent, runRoot, coreHome] = made;
  made.forEach((d) => fs.mkdirSync(d));
  say(`scratch: ${scratch} (${made.length + 1} directories made)`);

  try {
    run('compile', 'npm', ['run', 'compile'], { cwd: root });

    /*
     * KEY: DEPENDENCY DETECTION STAYS ON. `vsce package --no-dependencies`
     * leaves out every file under node_modules, the reader included, however
     * `.vscodeignore` re-includes it -- measured the first time this gate ran
     * (2026-09-25). The listing below it says so each time, as a reading.
     */
    const vsix = path.join(packageDir, 'theourgia.vsix');
    run('package (vsce package --allow-missing-repository)', 'npx', [
      'vsce', 'package', '--allow-missing-repository', '--out', vsix
    ], { cwd: root });
    /*
     * NEVER: A LISTING THAT COULD NOT BE TAKEN IS NOT A LISTING WITHOUT THE
     * FILE. Both listings below say "could not list" rather than ABSENT when
     * their command failed.
     */
    const without = spawnSync('npx', ['vsce', 'ls', '--no-dependencies'], { cwd: root, encoding: 'utf8' });
    if (without.status !== 0) {
      unreadable += 1;
      say(`the same files with --no-dependencies: could not list (vsce ls rc ${without.status})`);
    } else {
      const withoutNames = (without.stdout ?? '').split('\n').map((l) => l.trim());
      say(`the same files with --no-dependencies (vsce ls, rc 0): ` +
        REQUIRED.filter((r) => r.startsWith('extension/node_modules/'))
          .map((r) => `${r.slice('extension/'.length)} ${withoutNames.includes(r.slice('extension/'.length)) ? 'present' : 'ABSENT'}`)
          .join(', '));
    }
    if (fs.existsSync(vsix)) {
      say(`package: ${vsix}, ${fs.statSync(vsix).size} bytes`);
      const unzipped = spawnSync('unzip', ['-l', vsix], { encoding: 'utf8' });
      if (unzipped.status !== 0) {
        unreadable += 1;
        say(`listing: could not list (unzip rc ${unzipped.status}${unzipped.error ? `, ${unzipped.error.message}` : ''})`);
      } else {
        const names = (unzipped.stdout ?? '').split('\n').map((l) => /\s(\S+)$/.exec(l.trim())).filter((m) => m).map((m) => m[1]);
        say(`listing: ${names.filter((n) => n.startsWith('extension/')).length} entries under extension/`);
        for (const want of REQUIRED) {
          say(`    ${names.includes(want) ? 'present' : 'ABSENT '} ${want}`);
        }
      }
    } else {
      say('package: no file was written');
      unreadable += 1;
    }

    const { downloadAndUnzipVSCode, resolveCliPathFromVSCodeExecutablePath, runTests } = require('@vscode/test-electron');
    /*
     * THEOURGIA_TEST_CODE NAMES AN EDITOR ALREADY ON DISK, as it does for the
     * editor-hosted runner; otherwise the one `.vscode-test/` keeps, fetched
     * if it is not there.
     */
    const executable = resolveExecutable(process.env.THEOURGIA_TEST_CODE ?? await downloadAndUnzipVSCode(EDITOR_VERSION));
    const cli = resolveCliPathFromVSCodeExecutablePath(executable);
    say(`editor: ${executable}${process.env.THEOURGIA_TEST_CODE ? ' (THEOURGIA_TEST_CODE)' : ` (${EDITOR_VERSION})`}`);
    run('install', cli, [`--user-data-dir=${userData}`, `--extensions-dir=${extensionsDir}`, '--install-extension', vsix]);
    run('installed', cli, [`--user-data-dir=${userData}`, `--extensions-dir=${extensionsDir}`, '--list-extensions', '--show-versions']);

    const env = { ...process.env, THEOURGIA_RUN: runRoot, THEOURGIA_HOME: coreHome };
    Object.assign(process.env, { THEOURGIA_RUN: runRoot, THEOURGIA_HOME: coreHome });
    const { initWire } = require(path.join(root, 'out/src/wire'));
    const { Client } = require(path.join(root, 'out/src/client'));
    await initWire();
    const config = {
      scheme: process.env.THEOURGIA_SCHEME ?? 'scheme',
      corePath,
      libDirs,
      store: path.join(storeParent, 'store'),
      actor: 'gate',
      writer: 'gate',
      timeoutMs: 30000
    };
    const client = Client.fromConfig(config);
    const init = await client.request('init', []);
    const inserted = await client.request('insert', ['--under', 'root', '--title', 'Gate', '--text', 'gate body\n']);
    const block = (/\(state \(\("([^"]+)"/.exec(inserted.text) ?? [])[1] ?? null;
    say(`store: init ok=${init.ok}, insert ok=${inserted.ok}, block ${block}`);
    const before = block === null ? null : await logEntries(client, block);
    say(`store log entries for ${block}, before the editor: ${before}`);

    fs.writeFileSync(path.join(stub, 'package.json'), JSON.stringify({
      name: 'gate-driver', publisher: 'gate', version: '0.0.0', engines: { vscode: '*' }
    }));
    const driver = path.join(stub, 'driver.js');
    fs.writeFileSync(driver, DRIVER);
    fs.mkdirSync(path.join(userData, 'User'), { recursive: true });
    fs.writeFileSync(path.join(userData, 'User', 'settings.json'), JSON.stringify({
      'theourgia.corePath': corePath,
      'theourgia.libDirs': libDirs,
      'theourgia.store': config.store,
      'theourgia.actor': 'gate',
      'theourgia.writer': 'gate'
    }, null, 2));
    const resultFile = path.join(scratch, 'result.json');
    try {
      await runTests({
        vscodeExecutablePath: executable,
        extensionDevelopmentPath: stub,
        extensionTestsPath: driver,
        launchArgs: ['--disable-gpu', '--no-sandbox', `--user-data-dir=${userData}`, `--extensions-dir=${extensionsDir}`],
        extensionTestsEnv: {
          ...env,
          GATE_RESULT: resultFile,
          GATE_BLOCK: block ?? '',
          GATE_OUT: path.join(root, 'out'),
          GATE_CONFIG: JSON.stringify(config)
        }
      });
      say('editor: ran and exited');
    } catch (e) {
      say(`editor: ${String(e)}`);
      unreadable += 1;
    }
    const result = fs.existsSync(resultFile) ? JSON.parse(fs.readFileSync(resultFile, 'utf8')) : null;
    say(`in the editor: ${result === null ? 'no result was written' : JSON.stringify(result, null, 2).split('\n').join('\n    ')}`);

    const logs = [];
    const walk = (d) => {
      for (const name of fs.existsSync(d) ? fs.readdirSync(d) : []) {
        const p = path.join(d, name);
        if (fs.statSync(p).isDirectory()) {
          walk(p);
        } else if (name === 'exthost.log') {
          logs.push(p);
        }
      }
    };
    walk(path.join(userData, 'logs'));
    say(`exthost.log: ${logs.length} file(s)`);
    for (const log of logs) {
      const lines = fs.readFileSync(log, 'utf8').split('\n').filter((l) => l.includes(EXTENSION_ID));
      say(`    ${path.relative(userData, log)}: ${lines.length} line(s) naming ${EXTENSION_ID}`);
      lines.slice(0, 8).forEach((l) => say(`      ${l}`));
    }

    if (block !== null) {
      say(`store log entries for ${block}, after the editor: ${await logEntries(client, block)} (before: ${before})`);
      const read = await client.request('read', [block]);
      say(`store body carries the line written in the editor: ${read.text.includes(MARK)}`);
    }
  } finally {
    const daemons = corePath.length > 0 ? daemonsUnder(runRoot, corePath) : [];
    daemons.forEach((pid) => {
      try {
        process.kill(pid, 'SIGTERM');
      } catch (e) {
        say(`daemon ${pid}: ${e.message}`);
      }
    });
    const deadline = Date.now() + 5000;
    let still = corePath.length > 0 ? daemonsUnder(runRoot, corePath) : [];
    while (still.length > 0 && Date.now() < deadline) {
      execFileSync('sleep', ['0.2']);
      still = daemonsUnder(runRoot, corePath);
    }
    say(`daemons under this run's root: ${daemons.length} asked to stop, ${still.length} still running`);
    fs.rmSync(scratch, { recursive: true, force: true });
    const left = [scratch, ...made].filter((d) => fs.existsSync(d));
    say(`cleanup: removed ${made.length + 1 - left.length} of ${made.length + 1} directories${left.length > 0 ? `; left ${left.join(', ')}` : ''}`);
  }
  process.exitCode = unreadable > 0 ? 1 : 0;
  say(`readings that could not be taken: ${unreadable}`);
}

main().catch((e) => {
  say(`the gate stopped: ${(e && e.stack) || e}`);
  process.exitCode = 1;
});
