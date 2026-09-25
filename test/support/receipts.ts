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
 * RECEIPTS FOR STAND-IN DESTINATIONS, AND THERE IS ONLY ONE WAY TO GET ONE.
 *
 * `ImportTarget.adopt` returns the receipt of the enqueue that took the
 * entry (queue item 3), and a receipt cannot be built outside
 * `src/outbox.ts` -- its brand is a symbol that file does not export. So a
 * stand-in that keeps entries in an array gets its receipt the way the real
 * destination does: by enqueuing the entry into a queue file. Each call
 * uses a fresh file under the system's temporary directory.
 *
 * NOTE: A DESTINATION THAT KEEPS NOTHING CANNOT BE WRITTEN THE OLD WAY ANY
 * MORE. `adopt: () => undefined` no longer type-checks, and that is the
 * interface doing its job. What such a destination can still do wrong is
 * answer with a receipt that is not this entry's -- `strangersReceipt` --
 * which is the case the import loop now checks for.
 */

import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Outbox, OutboxEntry, Receipt } from '../../src/outbox';

function scratchQueue(): Outbox {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-receipt-'));
  const outbox = new Outbox(path.join(directory, 'outbox.json'));
  outbox.load();
  return outbox;
}

export function receiptFor(entry: OutboxEntry): Receipt {
  return scratchQueue().enqueue({ ...entry });
}

export function strangersReceipt(): Receipt {
  return scratchQueue().enqueue({
    req: 'a-request-nobody-asked-about',
    cursor: 'w:1',
    id: 'z.1',
    field: 'src',
    payload: 'not this entry\n',
    state: 'queued',
    createdAt: 1,
    lastError: null,
    importedBy: null
  });
}
