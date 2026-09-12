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
 * Putting a reading of a block in front of the user: claim the baseline,
 * write the file, open it.
 *
 * IT IS HERE, AND IT TAKES THE EDITOR AS AN ARGUMENT, BECAUSE THE ORDER
 * OF THOSE THREE IS THE WHOLE POINT. The registry in open.ts decides
 * which reading wins; this is the code that has to act on that decision,
 * and inside `activate` nothing could check it. The proof that it needed
 * checking is that when `register` changed its answer from a boolean,
 * the call site's `if (!admission)` went on compiling, stopped being
 * true, and let a losing open write the file -- with every editor-hosted
 * cell still green.
 *
 * THE EDITOR IS A PARAMETER RATHER THAN A HOOK. Nothing here is added
 * for the benefit of a test: this function genuinely does not need to
 * know that its three editor operations come from VS Code, and saying so
 * in the signature is what lets a cell hand it three functions and drive
 * the interleaving that the editor's own API cannot be made to produce.
 */

import { Admission, OpenBuffers, Outcome } from './open';

export interface EditorSurface<D> {
  openDocument(file: string): Promise<D>;
  setLanguage(document: D, language: string): Promise<unknown>;
  reveal(document: D): Promise<unknown>;
}

export type Placement =
  | { placed: true }
  | { placed: false; by: Outcome };

export async function placeReading<T, D>(
  buffers: OpenBuffers<T>,
  file: string,
  value: T,
  ticket: number,
  write: () => void,
  editor: EditorSurface<D>
): Promise<Placement> {
  const admission: Admission = buffers.register(file, value, ticket, write);
  if (!admission.admitted) {
    return { placed: false, by: admission.by };
  }
  const document = await editor.openDocument(file);
  await editor.setLanguage(document, 'markdown');
  await editor.reveal(document);
  return { placed: true };
}
