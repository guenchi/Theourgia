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

;; Whether the barrier PARKED is a question about elapsed time, not
;; about the order lines came out of a pipe.
(import (chezscheme) (theourgia ffi))
(define (now) (let ((t (current-time 'time-monotonic)))
                (+ (time-second t) (/ (time-nanosecond t) 1000000000.0))))
(define t0 (now))
;; AN ARGUMENT THIS PROBE CANNOT DO WITHOUT, ASKED FOR BY NAME. Reading
;; `(cadr (command-line))` with no argument raises on `cadr`, and a probe
;; that died that way is indistinguishable from one that crashed -- so a
;; runner classifying scripts by what they did cannot tell "needs an
;; argument" from "broken", and either has to keep a list of names or
;; report the probe as red for ever. One line makes the class decidable
;; from the outcome.
(define (barrier-probe-argument)
  (let ((a (command-line)))
    (if (null? (cdr a))
        (begin (printf "usage: barrier-probe.sc <barrier-name>\n") (exit 2))
        (cadr a))))
(barrier! (string->symbol (barrier-probe-argument)))
(printf "parked for ~a seconds\n" (exact->inexact (- (now) t0)))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "barrier-probe complete\n")
