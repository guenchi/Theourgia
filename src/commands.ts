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
 * THE NAMES OF THE COMMANDS, IN ONE PLACE.
 *
 * A command has three appearances: the manifest declares it, the
 * activation registers it, and a sentence somewhere tells the user to
 * run it. Those three were three separate strings, and one of them named
 * a command that did not exist -- a refusal told the user to run
 * "theourgia: Reconcile Block" while nothing declared it and nothing
 * registered it, so the only advice the message gave led to an empty
 * palette.
 *
 * The registration and the sentence now read the same constant, so they
 * cannot disagree. The manifest cannot import this file, so it is not
 * covered by construction -- it is covered by a cell that compares the
 * two, which is why `title` is here as well as `id`.
 */
export interface CommandName {
  id: string;
  title: string;
}

export const REFRESH_OUTLINE: CommandName = {
  id: 'theourgia.refreshOutline',
  title: 'theourgia: Refresh Outline'
};

export const OPEN_BLOCK: CommandName = {
  id: 'theourgia.openBlock',
  title: 'theourgia: Open Block'
};

export const RETRY_OUTBOX: CommandName = {
  id: 'theourgia.retryOutbox',
  title: 'theourgia: Retry Pending Saves'
};

export const SHOW_STATUS: CommandName = {
  id: 'theourgia.showStatus',
  title: 'theourgia: Show Store Status'
};

export const RECONCILE_BLOCK: CommandName = {
  id: 'theourgia.reconcileBlock',
  title: 'theourgia: Reconcile Block'
};

/*
 * EVERY COMMAND, so that the cell which compares this with the manifest
 * can say "these two lists are the same" rather than checking the ones
 * somebody remembered to name. A list that only mentions the commands a
 * reader thought of will not shout about the one they forgot.
 */
export const COMMANDS: CommandName[] = [
  REFRESH_OUTLINE,
  OPEN_BLOCK,
  RETRY_OUTBOX,
  SHOW_STATUS,
  RECONCILE_BLOCK
];
