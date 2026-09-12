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

;; The two inputs the reviews named, timed against serialising the same
;; actor -- the check must not cost more than the work it guards.
(import (chezscheme) (theourgia wire))
(define outcome 'none)
(define (ms thunk)
  (let ((t0 (current-time 'time-monotonic)))
    (set! outcome (guard (e (#t (if (vector? e) (list 'refused-by-codec (vector-ref e 1)) 'refused-by-check)))
                    (thunk) 'accepted))
    (let ((t1 (current-time 'time-monotonic)))
      (+ (* 1000 (- (time-second t1) (time-second t0)))
         (/ (- (time-nanosecond t1) (time-nanosecond t0)) 1000000.0)))))
(define dag (let loop ((n 70) (x "a")) (if (= n 0) x (loop (- n 1) (vector x x)))))
(printf "70-deep shared DAG actor:  ~a ms  ~a\n"
        (ms (lambda () (encode-record 0 0 (list dag 0 0 0 0) '() '(a)))) outcome)
(define long (make-list 20000 'a))
(printf "20000-element list actor:  ~a ms  ~a\n"
        (ms (lambda () (encode-record 0 0 (list long 0 0 0 0) '() '(a)))) outcome)
(printf "ordinary five-field actor: ~a ms  ~a\n"
        (ms (lambda () (encode-record 0 0 (list "agent:claude" (cons "w" "r") 0 "f" (cons "w" 2) (cons "w" 3)) '() '(a)))) outcome)

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "actor-cost complete\n")
