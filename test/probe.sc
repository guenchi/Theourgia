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

;; Non-blocking check from another process: can it take the lock now?
(import (chezscheme) (theourgia ffi))
;; AN ARGUMENT THIS PROBE CANNOT DO WITHOUT, ASKED FOR BY NAME. Reading
;; `(cadr (command-line))` with no argument raises on `cadr`, and a probe
;; that died that way is indistinguishable from one that crashed -- so a
;; runner classifying scripts by what they did cannot tell "needs an
;; argument" from "broken", and either has to keep a list of names or
;; report the probe as red for ever. One line makes the class decidable
;; from the outcome.
(define (probe-argument)
  (let ((a (command-line)))
    (if (null? (cdr a))
        (begin (printf "usage: probe.ss <lock-path>\n") (exit 2))
        (cadr a))))
(define lock (probe-argument))
;; Bounded from outside by an alarm: if this prints nothing, the lock
;; was held and this process was still waiting when it was killed.
(printf "~a\n" (guard (e (#t 'error)) (with-exclusive-lock lock (lambda (fd) 'acquired))))
(flush-output-port (current-output-port))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "probe complete\n")
