#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;;; (theourgia timing) -- the write path's timing seam, THEOURGIA_TIMING=1 (test-only).
;;;
;;; REACHED ONLY WHEN THE SWITCH WAS SET AT EXPANSION. timing-wrappers.sc's wrappers
;;; (timed, intent-timed, timing-call, timing-quiet) read THEOURGIA_TIMING when
;;; they are expanded, as THEOURGIA_INJECT's branches are: set, a wrapped site
;;; expands to a call into this library through `environment`; unset, to the
;;; bare expression, and nothing here is loaded (from source, nor compiled; a
;;; build compiles this library like any other, and calls it only when built
;;; with the variable set).
;;;
;;; ONE LINE PER READING, ON STDERR: `(timing request <phase> <us>)` for a
;;; request's phases, once each; `(timing intents <n> ((<phase> <us>) ...))`
;;; for each window of 100 intents, summed. The clock is the monotonic one, in
;;; microseconds: an intent's phases are well under a millisecond, and a wall
;;; clock can step.

(library (theourgia timing)
  (export monotonic-us timed-run intent-timed-run intent-window! request-begin! request-line!
          delivered! delivered-count reset-sums! quiet-run)
  (import (chezscheme))

  (define (monotonic-us)
    (let ((t (current-time 'time-monotonic)))
      (+ (* 1000000 (time-second t)) (quotient (time-nanosecond t) 1000))))

  ;; A failure to print is swallowed, as the trace's is: timing must not turn
  ;; a write that worked into one that raised.
  ;; ONE STRING, WRITTEN WITH INTERRUPTS DISABLED, for the reason the
  ;; trace's line is: the error port is every actor's, and a line written in
  ;; pieces can be split by another actor's flush. The guard is inside the
  ;; region, so a failed write is handled there and the region ends by
  ;; returning.
  (define (timing-line! . fields)
    (guard (e (#t #f))
      (let ((p (current-error-port))
            (line (call-with-string-output-port
                    (lambda (s) (write (cons 'timing fields) s) (newline s)))))
        (with-interrupts-disabled
          (guard (e (#t #f))
            (put-string p line)
            (flush-output-port p))))))

  ;; every record delivered, applied or not, since the request began
  (define delivered (vector 0))
  ;; #t while the rules rehearse the branch dry: its intents and its plan are
  ;; not the request's, and neither is counted or printed
  (define quiet (vector #f))
  ;; The eight steps of an intent, then where the rest of its time goes:
  ;; shape (run-intents!'s checks of the intent), after (the caller's
  ;; after-each), intent (one-intent! whole, which holds check..receipt and
  ;; the three after it), view, payload-check and frame (inside one-intent!),
  ;; and actor (the record's actor, made before one-intent! is called).
  ;; Inside append (log.sc real-append!): metadata-check (this writer's
  ;; metadata readable), barrier (every writer's metadata flushed if it
  ;; changed), versions (every writer's metadata versions compared, and a
  ;; reload when they changed), catch-up (this writer's prefix discovered)
  ;; and write (the record framed and written).
  (define intent-phases
    '#(refs check resolve deps append reduce applied receipt intent shape after view payload-check frame actor
       metadata-check barrier versions catch-up write))
  (define intent-sums (make-vector (vector-length intent-phases) 0))

  ;; -> the request's start, its counters reset
  (define (request-begin!)
    (vector-fill! intent-sums 0)
    (vector-set! delivered 0 0)
    (monotonic-us))
  (define (request-line! what value)
    (unless (vector-ref quiet 0) (timing-line! 'request what value)))
  (define (timed-run what thunk)
    (if (vector-ref quiet 0)
        (thunk)
        (let* ((t0 (monotonic-us)) (v (thunk)))
          (timing-line! 'request what (- (monotonic-us) t0))
          v)))
  (define (intent-timed-run i thunk)
    (if (vector-ref quiet 0)
        (thunk)
        (let* ((t0 (monotonic-us)) (v (thunk)))
          (vector-set! intent-sums i (+ (vector-ref intent-sums i) (- (monotonic-us) t0)))
          v)))
  (define (intent-window! n)
    (when (and (not (vector-ref quiet 0)) (> n 0) (= 0 (mod n 100)))
      (timing-line! 'intents n
                    (let loop ((k (- (vector-length intent-phases) 1)) (out '()))
                      (if (< k 0) out
                          (loop (- k 1) (cons (list (vector-ref intent-phases k) (vector-ref intent-sums k)) out)))))
      (vector-fill! intent-sums 0)))
  (define (delivered!) (vector-set! delivered 0 (+ 1 (vector-ref delivered 0))))
  (define (delivered-count) (vector-ref delivered 0))
  (define (reset-sums!) (vector-fill! intent-sums 0))
  (define (quiet-run thunk)
    (dynamic-wind
      (lambda () (vector-set! quiet 0 #t))
      thunk
      (lambda () (vector-set! quiet 0 #f))))
)
