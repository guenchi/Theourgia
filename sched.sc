#!r6rs
;; Copyright 2026 guenchi
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; (theourgia sched) -- processes, mailboxes and the scheduler.
;;;
;;; THIS IS A FACADE. Every use this core makes of igropyr goes through a
;;; library of its own, so the dependency has a single seam: `test/
;;; facades.sexp` names them and two gates read that list -- one for who
;;; may import igropyr, one for what these files hand on.
;;;
;;; TEN NAMES, NOT THE TWENTY `(igropyr actor)` OFFERS. `demonitor`,
;;; `process-trap-exit`, `kill`, the `register`/`whereis` family,
;;; `process-count` and the `critical!` pair are deliberately absent: a
;;; wholesale re-export would make the list say nothing about what this
;;; core uses, and would widen what a replacement has to provide.
;;;
;;; NOTE: `receive` IS SYNTAX, AND ITS `(after ms ...)` CLAUSE IS PART OF
;;; THAT SYNTAX -- not a procedure this file could wrap. Re-exporting the
;;; macro keeps the clause working; wrapping `receive` in a procedure
;;; would take the clause away and there would be no timeout at all.
;;; That is why this file re-exports rather than defines: a facade that
;;; redefined these would have to reimplement a scheduler.

;;;
;;; START-SCHEDULER IS THE ONE PROCEDURE DEFINED HERE, and for one reason:
;;; the mutation record (theourgia ffi, F100 D1') is keyed by the running
;;; actor, and ffi cannot import the actor library without loading libuv
;;; into every program. So the key is handed to ffi from here, as a thunk
;;; (the value of `self` read now would be the register one line before the
;;; first process exists; igropyr's own uv-set-self! says the same). It is
;;; installed when a scheduler starts, not when this library loads: Chez
;;; runs a library's body only when a variable the library defines is
;;; referenced, and everything else here is igropyr's own binding, so an
;;; installer in the body could stay unrun in a program that starts one.
;;; Outside a started scheduler ffi keys by one object for the process.
;;; test/facades.sc pins this one definition by name (D1-05).

(library (theourgia sched)
  (export start-scheduler spawn spawn&link receive send self monitor demonitor
          link kill process-alive? process-count process-monitor-count sleep-ms)
  (import (only (chezscheme) define lambda)
          (only (theourgia ffi) mutation-set-self!)
          (rename (only (igropyr actor) start-scheduler)
                  (start-scheduler igropyr-start-scheduler))
          (only (igropyr actor)
                spawn spawn&link receive send self monitor demonitor
                link kill process-alive? process-count process-monitor-count sleep-ms))

  (define (start-scheduler boot-thunk)
    (mutation-set-self! (lambda () self))
    (igropyr-start-scheduler boot-thunk)))
