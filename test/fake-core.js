#!/usr/bin/env node
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
 * A core that answers from a script.
 *
 * IT STANDS IN THE SCHEME EXECUTABLE'S PLACE, not the command line's, so
 * the argument vector it receives is the one the real core would receive
 * -- `--script`, the path to cli.ss, the verb, the arguments and the
 * options, in that order. A stand-in invoked some shorter way would make
 * the cell that checks the argument vector check the stand-in's shape
 * instead of the core's.
 *
 * IT RECORDS BEFORE IT ANSWERS. Every call is appended to a log with its
 * arguments, the two library environment variables, the time it started
 * and the time it answered; a cell that asks whether two saves
 * overlapped reads those times, and a cell that asks whether a retry
 * sent the same bytes compares two recorded vectors. A stand-in that
 * logged after answering would be unable to record a call it was killed
 * during -- which is the one T3 is about.
 *
 * WHAT IT CAN BE TOLD TO DO
 *   stdout              the bytes to print
 *   rc                  the exit code, which is the verdict
 *   stderr              noise on the other stream
 *   delayMs             wait this long before answering
 *   exitWithoutAnswer   exit 0 having printed nothing
 *   once                use this entry at most once
 *   chunkAt             byte offsets to break stdout at, so that a
 *                       reader sees it arrive in pieces
 *
 * CHUNKS ARE BYTES, NOT CHARACTERS. The point of breaking the output is
 * to land a break in the middle of something -- an escape sequence, or a
 * multi-byte character -- and a break measured in JavaScript characters
 * can never fall inside a UTF-8 sequence, which is the case a client
 * decoding per chunk gets wrong.
 */

const fs = require('fs');
const path = require('path');

const SCRIPT = process.env.FAKE_CORE_SCRIPT;
const LOG = process.env.FAKE_CORE_LOG;

function append(record) {
  if (!LOG) {
    return;
  }
  fs.mkdirSync(path.dirname(LOG), { recursive: true });
  fs.appendFileSync(LOG, `${JSON.stringify(record)}\n`, 'utf8');
}

const argv = process.argv.slice(2);

/*
 * The core's own arguments are everything after the script path. The
 * two leading entries belong to the interpreter and are recorded rather
 * than matched on, because a cell that pins them is pinning how the
 * client starts Chez, which is a different question from what it asked.
 */
function coreArgvOf(all) {
  const at = all.indexOf('--script');
  if (at >= 0 && at + 1 < all.length) {
    return all.slice(at + 2);
  }
  return all;
}

const coreArgv = coreArgvOf(argv);
const started = Date.now();

/*
 * WHAT A WATCHED FILE HELD AT THE MOMENT THE REQUEST ARRIVED. The outbox
 * has to be on disk BEFORE the request goes out, and the only witness to
 * that order is the side receiving the request: a check made by the
 * sender afterwards cannot tell which of its own two steps ran first.
 */
function watched() {
  const file = process.env.FAKE_CORE_WATCH;
  if (!file) {
    return null;
  }
  try {
    return fs.readFileSync(file, 'utf8');
  } catch (e) {
    return null;
  }
}

const base = {
  argv,
  coreArgv,
  pid: process.pid,
  cwd: process.cwd(),
  env: {
    CHEZSCHEMELIBDIRS: process.env.CHEZSCHEMELIBDIRS || null,
    CHEZSCHEMELIBEXTS: process.env.CHEZSCHEMELIBEXTS || null
  },
  watched: watched(),
  started
};

/*
 * A SIGNAL IS AN OUTCOME AND IS WRITTEN DOWN AS ONE. T3 asks whether the
 * client actually stopped the child rather than merely stopped waiting
 * for it, and the only witness to that is the child.
 */
/*
 * `ignoreSignals` MAKES A CHILD THAT WILL NOT GO. A stand-in that always
 * exits on the first signal can only ever show a cooperative core, and
 * the case worth checking is the other one: a client that asks a process
 * to stop and then waits for it to has not stopped it.
 */
const IGNORES_SIGNALS = process.env.FAKE_CORE_IGNORE_SIGNALS === '1';

for (const signal of ['SIGTERM', 'SIGINT', 'SIGHUP']) {
  process.on(signal, () => {
    append({ ...base, event: 'signal', signal, at: Date.now() });
    if (IGNORES_SIGNALS) {
      return;
    }
    process.exit(128);
  });
}

function loadScript() {
  if (!SCRIPT) {
    return { calls: [] };
  }
  try {
    return JSON.parse(fs.readFileSync(SCRIPT, 'utf8'));
  } catch (e) {
    process.stderr.write(`fake-core: cannot read ${SCRIPT}: ${e}\n`);
    process.exit(70);
  }
  return { calls: [] };
}

function matches(entry, request) {
  const wanted = entry.match || [];
  if (wanted.length > request.length) {
    return false;
  }
  for (let i = 0; i < wanted.length; i += 1) {
    if (wanted[i] !== request[i]) {
      return false;
    }
  }
  for (const item of entry.contains || []) {
    if (!request.includes(item)) {
      return false;
    }
  }
  return true;
}

/*
 * USES ARE COUNTED IN THE SCRIPT FILE so that they survive across
 * processes: each request is a new process, and `once` means "the first
 * request that matches", not "the first time this process looks".
 */
function claim(script, index) {
  if (!SCRIPT) {
    return;
  }
  script.calls[index].used = (script.calls[index].used || 0) + 1;
  const temporary = `${SCRIPT}.${process.pid}.tmp`;
  fs.writeFileSync(temporary, JSON.stringify(script, null, 2), 'utf8');
  fs.renameSync(temporary, SCRIPT);
}

function answer(entry) {
  const finish = () => {
    if (entry.exitWithoutAnswer) {
      append({ ...base, event: 'answer', name: entry.name || null, rc: 0, wrote: '', at: Date.now() });
      process.exit(0);
    }
    if (entry.stderr) {
      process.stderr.write(entry.stderr);
    }
    const stdout = entry.stdout === undefined ? '' : entry.stdout;
    append({
      ...base,
      event: 'answer',
      name: entry.name || null,
      rc: entry.rc || 0,
      wrote: stdout,
      at: Date.now()
    });
    writeThenExit(Buffer.from(stdout, 'utf8'), entry.chunkAt || [], entry.rc || 0);
  };
  if (entry.delayMs) {
    setTimeout(finish, entry.delayMs);
    return;
  }
  finish();
}

/*
 * THE LAST PIECE IS HANDED OVER AND THE PROCESS ENDS IMMEDIATELY, so
 * that it is flushed by the exit rather than before it: a client that
 * stopped reading when it saw a complete-looking answer would lose it.
 */
function writeThenExit(bytes, offsets, rc) {
  const cuts = offsets.filter((n) => n > 0 && n < bytes.length).sort((a, b) => a - b);
  const pieces = [];
  let at = 0;
  for (const cut of cuts) {
    pieces.push(bytes.subarray(at, cut));
    at = cut;
  }
  pieces.push(bytes.subarray(at));

  const step = (i) => {
    if (i >= pieces.length) {
      process.exit(rc);
      return;
    }
    process.stdout.write(pieces[i]);
    if (i === pieces.length - 1) {
      process.exit(rc);
      return;
    }
    setTimeout(() => step(i + 1), 20);
  };
  step(0);
}

const script = loadScript();
const calls = Array.isArray(script.calls) ? script.calls : [];
let chosen = null;
for (let i = 0; i < calls.length; i += 1) {
  const entry = calls[i];
  if (!matches(entry, coreArgv)) {
    continue;
  }
  if (entry.once && (entry.used || 0) >= 1) {
    continue;
  }
  if (entry.once) {
    claim(script, i);
  }
  chosen = entry;
  break;
}

if (chosen === null) {
  append({ ...base, event: 'unscripted', at: Date.now() });
  process.stderr.write(`fake-core: no scripted answer for ${JSON.stringify(coreArgv)}\n`);
  process.stdout.write('(error unscripted-request)\n');
  process.exit(1);
}

answer(chosen);
