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
 * The file a block is edited in.
 *
 * ONE FILE PER (STORE, BLOCK), AND THE SAME FILE EVERY TIME. Opening a
 * block twice has to reach the same document or the editor would hold
 * two buffers for one block and the second save would overwrite the
 * first with older text. The store is part of the name because two
 * stores can hold the same id.
 *
 * THE FILE IS A VIEW, NOT A COPY TO BE KEPT. It is rewritten from the
 * store whenever the block is opened afresh, and nothing reads it back
 * except the save path, which reads what the editor has in the buffer
 * anyway. Treating it as a cache is how an extension starts disagreeing
 * with the store it is editing.
 */

import { createHash } from 'crypto';
import { FileOps, nodeFileOps } from './fsops';
import * as path from 'path';
import { BlockDocument } from './blocks';
import { namespaceFor } from './outbox';

/*
 * AN ID IS PUT IN A FILE NAME, SO IT IS MADE SAFE FOR ONE. Block ids are
 * `<writer>.<n>` today and nothing else would pass, but an id arriving
 * from another machine is a string the core accepted and this has to
 * survive it -- a `/` in a file name is a directory that does not exist,
 * and on a case-insensitive file system two ids differing only in case
 * would share one file.
 */
export function fileNameFor(id: string): string {
  const safe = id.replace(/[^A-Za-z0-9._-]/g, '_');
  let hash = 0x811c9dc5;
  for (let i = 0; i < id.length; i += 1) {
    hash ^= id.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return `${safe}-${hash.toString(16).padStart(8, '0')}.md`;
}

export function documentPathFor(storageDirectory: string, store: string, id: string): string {
  return path.join(storageDirectory, namespaceFor(store), 'blocks', fileNameFor(id));
}

/*
 * A NOTE BESIDE THE FILE SAYING WHAT THE STORE LAST AGREED IT HELD.
 *
 * THE EDITOR'S DIRTY FLAG IS NOT THIS QUESTION. A save that the core
 * refused has already written the file to disk -- the handler runs on
 * `onDidSaveTextDocument`, after the bytes have landed -- so the buffer
 * is CLEAN while holding work the store does not have. Opening the
 * block again would then find nothing dirty and rewrite the file from
 * the store, and the sentence the user was shown, that the file still
 * holds what they wrote, would stop being true.
 *
 * IT IS A FILE AND NOT A FIELD because the other reader is another
 * process. Two editor windows on one store compute the same path for a
 * block, and a map held inside one host knows nothing about the other's
 * unsaved work.
 */
function markerFor(file: string): string {
  return `${file}.committed`;
}

function digestOf(text: string): string {
  return createHash('sha256').update(text, 'utf8').digest('hex');
}

export function writeDocument(file: string, document: BlockDocument, files: FileOps = nodeFileOps): void {
  files.makeDirectory(path.dirname(file));
  files.writeText(file, document.text);
  markCommitted(file, document.text, files);
}

/*
 * Called when the file as it stands is known to be in the store: after
 * it has just been written from the store, and after a save the core
 * confirmed. Never after one it refused.
 */
/*
 * THE TEXT THAT WAS CONFIRMED, NOT THE TEXT THAT IS THERE NOW.
 *
 * Reading the file here asked the wrong question. A save is an await,
 * and a second save can write the buffer to disk while the first is
 * still in flight: the first one's answer then marked the SECOND one's
 * bytes as being in the store. The file looked clean and committed
 * while holding text the store had never seen, so the next open
 * overwrote it -- work lost with nothing refused and nothing reported.
 *
 * The caller knows what it sent, so it passes it. When the bytes on
 * disk are something else, the digests disagree, `hasUncommittedWork`
 * answers true, and the file is left alone -- which is the safe
 * direction and the one this marker exists to take.
 */
export function markCommitted(file: string, committed: string, files: FileOps = nodeFileOps): void {
  try {
    files.writeText(markerFor(file), `${digestOf(committed)}\n`);
  } catch (e) {
    /*
     * The marker is an optimisation in one direction only: without it
     * the file is treated as holding work, which refuses to overwrite.
     * Failing to write it is therefore safe, and reporting it here would
     * turn a successful save into an error message.
     */
  }
}

/*
 * ⚠️ NOTHING IN THE SHIPPING EXTENSION CALLS THIS. It is the legacy
 * helper; the current path asks the publisher. It is repaired anyway,
 * because a reader that answers "no work here" for a file it could not
 * open is the shape this delivery has been chasing, and leaving one
 * behind because nobody calls it today is how the tenth instance of a
 * defect gets written.
 */
export function hasUncommittedWork(file: string, files: FileOps = nodeFileOps): boolean {
  let text: string;
  try {
    text = files.readText(file);
  } catch (e) {
    /*
     * ⛔ ONLY AN ABSENT FILE HAS NO WORK IN IT. Every other failure
     * leaves the question open, and answering `false` closes it in the
     * direction that loses somebody's edits.
     */
    if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
      return false;
    }
    throw e;
  }
  let marker: string;
  try {
    marker = files.readText(markerFor(file)).trim();
  } catch (e) {
    /*
     * A file with no marker was written by something this version does
     * not know about. It may hold work; it is not overwritten.
     */
    return true;
  }
  return marker !== digestOf(text);
}
