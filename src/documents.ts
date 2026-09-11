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

import * as fs from 'fs';
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

export function writeDocument(file: string, document: BlockDocument): void {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, document.text, 'utf8');
}
