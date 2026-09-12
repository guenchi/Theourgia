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
 * Everything activation does that is not about VS Code.
 *
 * ⚠️ IT EXISTS BECAUSE THE HARNESS WAS DOING THE PRODUCT'S WORK. The
 * two-process cells called `sessions.begin()` themselves, so every one
 * of them had a `session.json` -- while the extension never called it at
 * all. The recovery listing could not see a single directory the real
 * extension made, and nothing failed, because the stand-in supplied
 * exactly the step that was missing.
 *
 * A STAND-IN THAT DOES MORE THAN THE PRODUCT IS A MASK. The usual
 * worry is a stand-in that does less -- it lacks the property under
 * investigation. This is the other direction and it is worse, because
 * the missing thing is missing in the PRODUCT and the cells are the
 * reason nobody notices.
 *
 * So the harness and `activate` now call this one function, and the
 * harness may call nothing else to set a session up. Whatever this
 * forgets, both of them forget.
 */

import { createHash } from 'crypto';
import { PathChain } from './chain';
import { FileOps } from './fsops';
import { Owners } from './ownership';
import { OpenDocuments, Publisher } from './publication';
import { Saving } from './saving';
import { SessionIdentity, Sessions } from './sessions';

export interface CoreDeps {
  files: FileOps;
  /*
   * Where this host keeps its own state. In the extension it is
   * `context.globalStorageUri.fsPath`; a cell gives a directory of its
   * own.
   */
  globalStorage: string;
  documents: OpenDocuments;
  stores: string[];
  sessionId: string;
}

export interface Core {
  sessionId: string;
  identity: SessionIdentity;
  sessions: Sessions;
  chain: PathChain;
  publisher: Publisher;
  saving: Saving;
  /*
   * The queue is per session AND per store: a queue carries one cursor
   * and a cursor belongs to one store, so a session that writes to two
   * of them needs two. (§12.9 as amended after the editor measured it.)
   */
  outboxPath(store: string): string;
  storeHash(store: string): string;
}

export function activateCore(deps: CoreDeps): Core {
  const sessions = new Sessions(deps.files, deps.globalStorage);
  /*
   * THE SESSION IS RECORDED BEFORE ANYTHING ELSE. Everything downstream
   * -- the listing another window reads, a takeover, a discard -- starts
   * from `session.json`, and a session that never wrote one is a session
   * whose unsent work nobody can reach.
   */
  const identity = sessions.begin(deps.sessionId, deps.stores);

  /*
   * ONE DIRECTORY PER STORE INSIDE THE SESSION, named by a digest of the
   * store's path so two stores with the same block id do not share a
   * place.
   */
  const storeHash = (store: string): string =>
    createHash('sha256').update(store, 'utf8').digest('hex').slice(0, 16);

  return {
    sessionId: deps.sessionId,
    identity,
    sessions,
    chain: new PathChain(),
    /*
     * ⚠️ THE PUBLISHER THE EXTENSION USES KNOWS WHOSE SESSION IT IS.
     *
     * Every path that writes a record beside a block passes the
     * ownership rule, and the rule needs to know who is asking. The one
     * `Sessions` makes for a draft scan does not write and is built
     * without it. (§13, r3-3)
     */
    publisher: new Publisher(deps.files, deps.documents, {
      owners: new Owners(deps.files),
      sessionId: deps.sessionId
    }),
    saving: new Saving(deps.files),
    /*
     * ASKED OF `Sessions`, NOT COMPOSED HERE. The listing and the import
     * read the queue through the same function; when this composed its
     * own path the two disagreed and a real queue became invisible --
     * pending counted zero and a takeover carried nothing.
     */
    outboxPath: (store: string) => sessions.outboxPathFor(deps.sessionId, storeHash(store)),
    storeHash
  };
}
