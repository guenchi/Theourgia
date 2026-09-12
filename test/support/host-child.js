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
 * The scripted half of the second extension host.
 *
 * IT IS PLAIN JAVASCRIPT because it is spawned directly, not compiled
 * with the suite; the fake core beside it is the same for the same
 * reason. It requires the COMPILED product out of `out/`, so it runs
 * the same code the cells do.
 *
 * ONE LINE PER STEP, WRITTEN SYNCHRONOUSLY. `process.exit` discards
 * buffered output, and the crash steps exist to kill this process
 * mid-sequence -- so every report is a synchronous write to fd 1 and is
 * on its way out before the next step begins. A report that could be
 * lost by the crash it is describing would make every crash cell
 * unreadable.
 */

'use strict';

const fs = require('fs');
const path = require('path');

const storage = process.argv[2];
const steps = JSON.parse(process.argv[3]);

function report(entry) {
  fs.writeSync(1, `${JSON.stringify(entry)}\n`);
}

/*
 * The product is loaded lazily and reported on, so that a step list
 * which never touches it still runs -- the harness's own control cells
 * do exactly that, and they must not depend on the product existing.
 */
let product = null;
function load() {
  if (product === null) {
    product = {
      fsops: require(path.join(__dirname, '..', '..', 'src', 'fsops.js')),
      publication: require(path.join(__dirname, '..', '..', 'src', 'publication.js')),
      sessions: require(path.join(__dirname, '..', '..', 'src', 'sessions.js'))
    };
  }
  return product;
}

/*
 * A file system that dies after the nth write.
 *
 * THE INJECTION IS A PARAMETER, NOT A HOOK IN THE PRODUCT. `FileOps` is
 * already handed in, so a cell can hand in one that stops -- and the
 * three points a publication can die at (after the record says
 * `publishing`, after the file, before the record says `published`) are
 * reachable by counting writes.
 *
 * IT USES process.exit, for the reason the crash step does: a throw
 * would run the unwinding that the recovery design exists to do
 * without.
 */
function crashingOps(inner, afterWrites) {
  let writes = 0;
  const wrap = (name) => (...args) => {
    if (name === 'writeText' || name === 'writeDurably') {
      writes += 1;
      if (writes > afterWrites) {
        report({ step: 'crash', at: `write-${writes}`, file: args[0] });
        process.exit(9);
      }
    }
    return inner[name](...args);
  };
  const out = {};
  for (const name of Object.keys(inner)) {
    out[name] = typeof inner[name] === 'function' ? wrap(name) : inner[name];
  }
  return out;
}

async function main() {
  for (const step of steps) {
    const name = Object.keys(step)[0];
    const argument = step[name];
    try {
      switch (name) {
        case 'note':
          report({ step: 'note', value: argument });
          break;
        case 'pid':
          report({ step: 'pid', value: process.pid });
          break;
        case 'writeFile': {
          const file = path.join(storage, argument.name);
          load().fsops.nodeFileOps.makeDirectory(path.dirname(file));
          load().fsops.nodeFileOps.writeText(file, argument.text);
          report({ step: 'writeFile', file, bytes: argument.text.length });
          break;
        }
        case 'readFile': {
          const file = path.join(storage, argument.name);
          report({ step: 'readFile', file, text: load().fsops.nodeFileOps.readText(file) });
          break;
        }
        case 'sleep':
          await new Promise((r) => setTimeout(r, argument));
          report({ step: 'sleep', ms: argument });
          break;
        case 'beginSession': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          const identity = sessions.begin(argument.sessionId, argument.stores || []);
          report({ step: 'beginSession', identity });
          break;
        }
        case 'publish': {
          const p = load();
          const ops =
            argument.crashAfterWrites === undefined
              ? p.fsops.nodeFileOps
              : crashingOps(p.fsops.nodeFileOps, argument.crashAfterWrites);
          const publisher = new p.publication.Publisher(ops, { isOpen: () => false });
          const outcome = await publisher.publish({
            directory: path.join(storage, argument.directory),
            storeId: argument.storeId,
            blockId: argument.blockId,
            prefix: argument.prefix,
            text: argument.text
          });
          report({ step: 'publish', outcome });
          break;
        }
        case 'standingOf': {
          const p = load();
          const publisher = new p.publication.Publisher(p.fsops.nodeFileOps, { isOpen: () => false });
          report({ step: 'standingOf', standing: publisher.standingOf(path.join(storage, argument)) });
          break;
        }
        case 'drafts': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          report({ step: 'drafts', drafts: sessions.draftsIn(argument) });
          break;
        }
        case 'claim': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          report({ step: 'claim', outcome: await sessions.claim(argument) });
          break;
        }
        case 'crash':
          /*
           * NOT AN EXCEPTION. `process.exit` with a non-zero code is a
           * death with no unwinding: no finally, no flush, no atexit
           * handler. Throwing here would exercise the cleanup path that
           * the recovery design exists to do without.
           */
          report({ step: 'crash', at: argument });
          process.exit(9);
          break;
        default:
          report({ step: name, error: 'unknown step' });
          break;
      }
    } catch (e) {
      report({ step: name, error: String(e && e.message ? e.message : e) });
    }
  }
  report({ step: 'done' });
}

main().then(
  () => process.exit(0),
  (e) => {
    report({ step: 'main', error: String(e && e.message ? e.message : e) });
    process.exit(1);
  }
);
