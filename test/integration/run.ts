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
import { execFileSync } from 'child_process';
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
 * NOTE: THE LIMIT IS IN BYTES AND THE MEASUREMENT MUST BE TOO. This counted
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

/*
 * THE EXTENSION'S OWN STORAGE, EMPTIED BEFORE EVERY RUN.
 *
 * Each activation records a session directory under the host's
 * globalStorage, and NOTHING RECLAIMS IT -- by design: those directories
 * hold unsent work, and a window that tidied up another window's would
 * be deleting the one thing this whole mechanism exists to rescue. In a
 * test host that means they accumulate, one per run. Measured: 26 of
 * them after a day of runs, which is a recovery list 26 rows long that
 * nothing in the suite put there.
 *
 * NOTE: SO IT IS THE HARNESS THAT CLEANS, NOT THE PRODUCT. The profile
 * belongs to this runner; the accumulation is an artefact of reusing it,
 * and the product's refusal to reclaim is the behaviour under test.
 *
 * NOTE: AND THE EMPTINESS IS CHECKED RATHER THAN ASSUMED. What this catches
 * is a removal that failed -- a locked file, a permission, a path that
 * drifted -- because a clean that silently did nothing looks exactly
 * like a clean that worked, right up until a cell wonders why the list
 * has thirty rows in it.
 *
 * THE IDENTIFIER COMES FROM THE MANIFEST. A hard-coded one that drifted
 * would clean a directory nobody writes to, and the check would then
 * pass on an empty path for ever while the real one filled up.
 */
function extensionStorage(root: string, profile: string): string {
  const manifest = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8')) as {
    publisher?: string;
    name?: string;
  };
  if (!manifest.publisher || !manifest.name) {
    throw new Error('package.json names no publisher or no name, so the storage path is a guess');
  }
  return path.join(profile, 'User', 'globalStorage', `${manifest.publisher}.${manifest.name}`);
}

function emptyTheStorage(root: string, profile: string): void {
  const storage = extensionStorage(root, profile);
  fs.rmSync(storage, { recursive: true, force: true });
  const left = fs.existsSync(storage) ? fs.readdirSync(storage) : [];
  if (left.length > 0) {
    throw new Error(
      `the extension's storage at ${storage} could not be emptied and still holds ` +
        `${left.join(', ')}. Every run leaves a session directory there and nothing reclaims ` +
        'them, so a clean that quietly fails makes the recovery list grow until a cell trips ' +
        'over it.'
    );
  }
}


/*
 * WHERE THIS RUN'S DAEMONS LIVE, AND THE READING THAT SAYS WHETHER ANY
 * ESCAPED. (design 7.6.50 v257-v261; the core's own runner does this)
 *
 * The thin client starts a daemon per store, under the run root. Left to
 * the product's default that root is `~/.theourgia/run` -- the user's
 * own -- and a suite that runs there leaves directories and live
 * processes behind in it. The core's line found the same hole in six of
 * its fixtures, four times by counting directories rather than by any
 * cell going red, and settled on three layers: the runner exports a run
 * root of its own before anything starts, each fixture sets its own as
 * well, and a gate reads the REAL root before and after and fails on
 * GROWTH. Growth, not total: another session's daemon is not this run's
 * to account for.
 *
 * NOTE: AND THE RUN ROOT MUST BE SHORT. A unix socket name may be 104
 * bytes; the path is the root plus a sixteen-character key plus
 * `/socket`, and macOS temporary directories are long enough on their
 * own to push a nested one past it. This one is made directly under the
 * system temporary directory for that reason, not under the profile.
 */
function realRunRoot(): string {
  return path.join(os.homedir(), '.theourgia', 'run');
}

/*
 * NEVER: ONLY AN ABSENT DIRECTORY COUNTS AS NONE. This caught every error
 * and answered zero, so a run root that could not be read was reported
 * as empty at both ends and the growth gate compared nothing with
 * nothing. Found in a ninth review round, the same shape as the process
 * listing below and in two other files.
 */
function countRealRunRoot(): number {
  try {
    return fs.readdirSync(realRunRoot()).length;
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
      return 0;
    }
    throw new Error(
      `could not read ${realRunRoot()}, so this run cannot say whether it left anything there: ` +
        (e as Error).message
    );
  }
}

/*
 * NOTE: THE PROCESS LIST IS MATCHED BY PATH AND THE READER EXCLUDES
 * ITSELF. `pkill -f scheme` has hit this line's own commands before --
 * the pattern is in the command that carries it. These are found by the
 * run root this run created, which no other process can be using, and
 * the reader's own pid is dropped.
 */
/*
 * NEVER: AND IT NO LONGER LOOKS FOR THE WORD "scheme", FOR TWO REASONS.
 *
 * The first is noise: on a developer's machine `ps | grep scheme`
 * returns a dozen of the editor's own helper processes, whose argument
 * lists carry `--standard-schemes=`.
 *
 * The second is the one that would have hurt. The core spells the
 * interpreter as `THEOURGIA_SCHEME` or `scheme` (`scheme-binary`, theourgia.sc:137), so on a
 * machine where that variable names `chez` this filter would exclude the
 * daemon it exists to find -- and the gate would print a clean
 * `0 still running` while the process it was written to catch went on
 * running. What is matched instead is what the core itself puts in the
 * daemon's argument list: `--socket <run root>/<key>/socket`
 * (`server-argv`, theourgia.sc:357-361).
 *
 * NEVER: AND THE MARKER IS REQUIRED. It was optional, and the branch that
 * dropped it -- every `scheme` on the machine, killed -- was reachable
 * from one careless call. Nothing called it; the type allowed it.
 */
function schemeProcesses(marker: string): Array<{ pid: number; command: string }> {
  /*
   * NEVER: A LOOK THAT FAILED IS NOT A LOOK THAT FOUND NOTHING.
   *
   * This caught the failure and answered with an empty list, so a `ps`
   * that could not be run made `stopOurDaemons` signal nobody, answer
   * `{asked: 0, left: []}`, and made this runner print
   * `0 still running` -- the one sentence it must never print falsely.
   *
   * NEVER: AND IT WAS MISSED WHEN THE SAME DEFECT WAS REPAIRED IN
   * `test/support/real-core.ts`. That repair named three call sites and
   * there were four; this is the fourth. A hazard is enumerated by
   * searching for its shape, not by fixing the file that happened to be
   * open.
   */
  const listing = execFileSync('ps', ['-ax', '-o', 'pid=,command='], { encoding: 'utf8' });
  const found: Array<{ pid: number; command: string }> = [];
  for (const line of listing.split('\n')) {
    const match = /^\s*(\d+)\s+(.*)$/.exec(line);
    if (match === null) {
      continue;
    }
    const pid = Number(match[1]);
    const command = match[2];
    if (pid === process.pid || !command.includes('--socket') || !command.includes(marker)) {
      continue;
    }
    found.push({ pid, command });
  }
  return found;
}

/*
 * ASK THE DAEMONS THIS RUN STARTED TO STOP, BY PID.
 *
 * NOTE: AND THEN CHECK, BECAUSE "SENT A SIGNAL" IS NOT "IT WENT". The
 * core's line wrote a teardown that removed the file a daemon was
 * writing and never signalled the daemon at all; what caught it was
 * counting processes afterwards, not reading the teardown.
 */
function stopOurDaemons(marker: string): { asked: number; left: Array<{ pid: number; command: string }> } {
  const ours = schemeProcesses(marker);
  for (const one of ours) {
    try {
      process.kill(one.pid, 'SIGTERM');
    } catch (e) {
      /* it had already gone; the count below is what decides */
    }
  }
  const deadline = Date.now() + 5000;
  let left = schemeProcesses(marker);
  while (left.length > 0 && Date.now() < deadline) {
    execFileSync('sleep', ['0.2']);
    left = schemeProcesses(marker);
  }
  for (const one of left) {
    try {
      process.kill(one.pid, 'SIGKILL');
    } catch (e) {
      /* as above */
    }
  }
  return { asked: ours.length, left: schemeProcesses(marker) };
}

async function main(): Promise<void> {
  const root = path.resolve(__dirname, '..', '..', '..');
  const profile = chooseProfile(root);
  emptyTheStorage(root, profile);
  /*
   * NOTE: MADE DIRECTLY UNDER THE SYSTEM TEMPORARY DIRECTORY AND KEPT
   * SHORT, because the socket name computed under it has 104 bytes to
   * fit in. The name carries this run's pid so that two runs cannot
   * share a root -- and so that the process listing below can tell this
   * run's daemons from anybody else's.
   */
  const scratch = fs.mkdtempSync(path.join(os.tmpdir(), `tv-${process.pid}-`));
  const runRoot = path.join(scratch, 'r');
  const coreHome = path.join(scratch, 'h');
  fs.mkdirSync(runRoot, { recursive: true });
  fs.mkdirSync(coreHome, { recursive: true });
  const realBefore = countRealRunRoot();
  let failed = false;
  try {
    const reported = process.env.THEOURGIA_TEST_CODE ?? await downloadAndUnzipVSCode();
    const executable = resolveExecutable(reported);
    await runTests({
      vscodeExecutablePath: executable,
      extensionDevelopmentPath: root,
      extensionTestsPath: path.resolve(__dirname, 'index'),
      /*
       * NOTE: ITS OWN USER DATA DIRECTORY, AND ITS OWN EXTENSIONS
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
        THEOURGIA_SCHEME: process.env.THEOURGIA_SCHEME ?? 'scheme',
        /*
         * NOTE: WHERE THE QUEUES ARE, FOR THE CELLS THAT HAVE TO READ THE
         * FILES.
         *
         * A cell inside the host can see what the extension REPORTS --
         * the store, the counts, the sentences -- and that is the wrong
         * instrument for a defect that writes the wrong bytes into a
         * queue nobody is looking at. One did: an answer for store A
         * committed A's cursor into B's `outbox.json` while A's entry
         * stayed unsettled in A's. Both halves of that are on disk and
         * neither is in any report.
         *
         * This is the path the launcher already computes to empty the
         * storage between runs, handed to the cells rather than guessed
         * at by them: a cell that derived it independently would be a
         * second opinion about where the extension writes, and the one
         * that was wrong would read an empty directory as a clean queue.
         */
        THEOURGIA_TEST_STORAGE: extensionStorage(root, profile),
        /*
         * NOTE: THE CORE READS BOTH OF THESE, SO THE FIXTURE SETS BOTH.
         * Setting only one is how the core's own fixtures ended up
         * computing two different socket paths for one store -- the
         * defect those very cells were about, reproduced by the cells.
         */
        THEOURGIA_RUN: runRoot,
        THEOURGIA_HOME: coreHome
      }
    });
  } catch (e) {
    failed = true;
    process.stderr.write(`the editor-hosted cells could not be run: ${String(e)}\n`);
  }

  /*
   * THE TWO LINES THE CORE'S RUNNER PRINTS, AND THEY ARE PRINTED WHETHER
   * OR NOT ANYTHING LEAKED. A gate that only speaks up when it is unhappy
   * is a gate nobody has seen a reading from.
   */
  const stopped = stopOurDaemons(runRoot);
  const realAfter = countRealRunRoot();
  const leftHere = fs.existsSync(runRoot) ? fs.readdirSync(runRoot) : [];
  process.stdout.write(
    `run root: ${realBefore} -> ${realAfter} directories under ${realRunRoot()}; ` +
      `this run's root ${runRoot} holds ${leftHere.length}\n`
  );
  process.stdout.write(
    `daemons: asked ${stopped.asked} to stop, ${stopped.left.length} still running\n`
  );

  if (realAfter > realBefore) {
    process.stderr.write(
      `LEAKED-INTO-REAL-RUN-ROOT: ${realAfter - realBefore} directories appeared under ` +
        `${realRunRoot()}, which belongs to whoever is using this machine\n`
    );
    failed = true;
  }
  if (stopped.left.length > 0) {
    process.stderr.write(
      `LEAKED-PROCESSES: ${stopped.left.length} still running after being asked and killed: ` +
        `${stopped.left.map((one) => one.pid).join(', ')}\n`
    );
    failed = true;
  }

  if (failed) {
    process.exit(1);
  }
}

void main();
