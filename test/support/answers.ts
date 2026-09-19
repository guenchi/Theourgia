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
 * THE ANSWERS A STAND-IN CORE GIVES, IN ONE PLACE.
 *
 * NOTE: WHY THIS FILE EXISTS. A cell about a save that settles wrote its
 * own `ok`: `(ok ((cursor . "w:2")))`. That is not the shape a core
 * answers with -- the event is a pair, `(cursor ("w" . 2))` -- and
 * `eventFromWrite` reads the invented one as "ok, naming no record", so
 * the save was marked pending and THE SETTLER WAS NEVER CALLED. The cell
 * passed, and its comment, and the delivery note, described what a
 * settled save does. An outside review found it.
 *
 * A stand-in is a theory of the other side. Spelled by hand once per
 * cell, the theory is re-invented once per cell, and the cell that gets
 * it wrong is the one that stops testing what it says it tests.
 */

/*
 * A WRITE THE CORE ACCEPTED AND APPENDED. `events` is what it wrote,
 * `cursor` is where the writer now stands, and `replay` says this was
 * not a request it had already applied.
 */
export function wroteAnswer(seq: number, writer = 'w', block = 'a.2'): string {
  return (
    `(ok (events ((${JSON.stringify(writer)} . ${seq}))) ` +
    `(state ((${JSON.stringify(block)} . "hhh"))) ` +
    `(cursor (${JSON.stringify(writer)} . ${seq})) (replay #f))\n`
  );
}

/*
 * THE SAME WRITE ARRIVING A SECOND TIME. The store recognises the
 * request by its id and says so rather than doing the work again.
 */
export function replayedAnswer(seq: number, writer = 'w', block = 'a.2'): string {
  return (
    `(ok (events ((${JSON.stringify(writer)} . ${seq}))) ` +
    `(state ((${JSON.stringify(block)} . "hhh"))) ` +
    `(event (${JSON.stringify(writer)} . ${seq})) (replay #t))\n`
  );
}
