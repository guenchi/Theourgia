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
let core = null;
function load() {
  if (product === null) {
    product = {
      fsops: require(path.join(__dirname, '..', '..', 'src', 'fsops.js')),
      publication: require(path.join(__dirname, '..', '..', 'src', 'publication.js')),
      saving: require(path.join(__dirname, '..', '..', 'src', 'saving.js')),
      outbox: require(path.join(__dirname, '..', '..', 'src', 'outbox.js')),
      sessions: require(path.join(__dirname, '..', '..', 'src', 'sessions.js')),
      activate: require(path.join(__dirname, '..', '..', 'src', 'activate.js'))
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
          /*
           * ⚠️ THROUGH THE PRODUCT'S OWN ENTRY, AND NOTHING ELSE.
           *
           * This used to call `sessions.begin()` directly -- one step,
           * innocuous-looking, and the extension did not do it. Every
           * two-process cell therefore had a `session.json` that the
           * real thing never wrote, and the missing wiring was invisible
           * for as long as the harness kept covering for it.
           *
           * Whatever `activateCore` forgets, this forgets too. That is
           * the only arrangement in which these cells say anything about
           * the extension.
           */
          const p = load();
          core = p.activate.activateCore({
            files: p.fsops.nodeFileOps,
            globalStorage: storage,
            documents: { isOpen: () => false },
            stores: argument.stores || [],
            sessionId: argument.sessionId
          });
          report({ step: 'beginSession', identity: core.identity });
          break;
        }
        case 'publish': {
          const p = load();
          const ops =
            argument.crashAfterWrites === undefined
              ? p.fsops.nodeFileOps
              : crashingOps(p.fsops.nodeFileOps, argument.crashAfterWrites);
          const publisher = new p.publication.Publisher(ops, { isOpen: () => false });
          /*
           * THE PATH COMES FROM `Sessions`, NOT FROM THE CELL. A cell
           * that composed the directory itself published outside the
           * sessions tree, so the very separation it was about never
           * happened.
           */
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          const outcome = await publisher.publish({
            directory: sessions.directoryFor(argument.sessionId, argument.storeHash || 'st', argument.blockId),
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
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          const where =
            typeof argument === 'string'
              ? path.join(storage, argument)
              : path.join(
                  sessions.directoryFor(argument.sessionId, argument.storeHash || 'st', argument.blockId),
                  argument.name
                );
          report({ step: 'standingOf', standing: publisher.standingOf(where) });
          break;
        }
        case 'drafts': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          report({ step: 'drafts', drafts: sessions.draftsIn(argument) });
          break;
        }
        case 'enqueue': {
          const p = load();
          const sessions = core ? core.sessions : new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          const queue = core
            ? core.outboxPath(argument.store || '/stores/one')
            : sessions.outboxPathFor(argument.sessionId);
          const outbox = new p.outbox.Outbox(queue);
          outbox.load();
          outbox.setCursor('w:7');
          outbox.enqueue({
            req: argument.req,
            cursor: 'w:7',
            id: argument.id || 'a.2',
            field: 'src',
            payload: argument.payload,
            state: 'sent',
            createdAt: 0,
            lastError: null,
            importedBy: null
          });
          report({ step: 'enqueue', file: queue, count: outbox.entries.length });
          break;
        }
        case 'countQueue': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          /*
           * COUNTED THE WAY THE LISTING COUNTS, across every store this
           * session holds a queue for -- asking one hard-coded path is
           * how a real queue read as empty.
           */
          let count = 0;
          for (const file of sessions.outboxPathsFor(argument)) {
            const outbox = new p.outbox.Outbox(file);
            try {
              outbox.load();
              count += outbox.entries.length;
            } catch (e) {
              count += 1;
            }
          }
          report({ step: 'countQueue', sessionId: argument, count });
          break;
        }
        case 'save': {
          const p = load();
          const saving = new p.saving.Saving(p.fsops.nodeFileOps);
          const sessions2 = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          const file = path.join(
            sessions2.directoryFor(argument.sessionId, argument.storeHash || 'st', argument.blockId),
            argument.name
          );
          const publisher = new p.publication.Publisher(p.fsops.nodeFileOps, { isOpen: () => false });
          const decision = saving.decide(
            { file, isDirty: false, getText: () => argument.text },
            publisher.sidecarOf(file)
          );
          report({ step: 'save', decision });
          break;
        }
        case 'claim': {
          const p = load();
          const sessions = new p.sessions.Sessions(p.fsops.nodeFileOps, storage);
          sessions.begin(argument.as, []);
          if (argument.waitFor) {
            /*
             * A RENDEZVOUS FILE, so two processes can be made to reach
             * the scan-then-link section together. Within one process
             * the section never overlaps -- it is synchronous -- so a
             * cell that ran both claims there would pass for a broken
             * read-then-write publication.
             */
            const gate = path.join(storage, argument.waitFor);
            fs.writeFileSync(`${gate}.${process.pid}`, 'ready');
            const deadline = Date.now() + 10000;
            while (Date.now() < deadline) {
              if (fs.readdirSync(storage).filter((n) => n.startsWith(path.basename(gate))).length >= 2) {
                break;
              }
            }
          }
          report({ step: 'claim', outcome: await sessions.claim(argument.dead) });
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
