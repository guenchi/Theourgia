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

;;; (theourgia eval-admission) -- the admission of evaluations: a bounded
;;; pool of slots per run root, taken by every core.sc evaluation before
;;; its cut, view, projection or scratch.
;;;
;;; NOTE: A LIBRARY OF ITS OWN, reached from core.sc through `later`, so
;;; only an evaluation pays to load it. Every command line start loads
;;; (theourgia client); the admission there cost every command its
;;; expansion (the startup alternation read 1.08% above the base).

(library (theourgia eval-admission)
  (export eval-admit! eval-slots-count)
  (import (rnrs base) (rnrs control)
          (only (chezscheme) guard raise sleep make-time parameterize
                current-time time-second time-nanosecond)
          (only (theourgia client) run-root)
          (only (theourgia ffi)
                env-or hold-point! theourgia-stage
                mkdir-p-unrecorded! file-ensure-unrecorded! lock-try-acquire! lock-release!
                lock-fd fd-close-on-exec! online-processors))

  ;;
  ;; KEY: A BOUND ON HOW MANY EVALUATIONS RUN AT ONCE, AT THE ENTRANCE EVERY
  ;; core.sc EVALUATION PASSES. Without it N shells give N evaluations, each
  ;; within its own quotas. The bound is K slots, K files
  ;; <run root>/admission/<i>; an evaluation runs while it holds an
  ;; exclusive flock on one of them, and the lock is released by the system
  ;; when the process that took it ends -- so a holder that dies frees its
  ;; slot, and a reused pid cannot hold one.
  ;;
  ;; NEVER: A CHILD DOES NOT INHERIT THE SLOT. The descriptor is marked
  ;; close-on-exec as soon as the lock is taken, before the worker or the
  ;; runner is spawned: a runner outliving its supervisor would otherwise
  ;; hold the slot wherever the spawn keeps unmarked descriptors. libuv's
  ;; spawn, which the worker and runner go through, closes them anyway
  ;; (measured on macOS and FreeBSD 15.0); the mark is what holds on any
  ;; other path. A mark that fails releases the lock and raises, as the
  ;; descriptor's failure.
  ;;
  ;; NEVER: THE SLOT FILES ARE ADMINISTRATION, NOT THE REQUEST'S WRITES. The
  ;; directory and the files are made, when absent, with the unrecorded
  ;; variants, so a first use of a run root does not appear in an unrelated
  ;; refusal's written clause. They are never removed or replaced while held.
  ;; The directory is `admission`, NOT a name under the scratch prefix
  ;; `eval-`: anything that takes `eval-*` for scratch -- a row checking that
  ;; nothing was left behind, an operator clearing leftovers -- must not
  ;; reach a slot file, since a slot unlinked while held lets a second holder
  ;; in on a new file of the same name.
  ;;
  ;; NOTE: WHAT THIS BOUNDS: cooperating clients of one run root, the command
  ;; line and MCP shells alike. It is not a machine-wide bound on process
  ;; trees: an evaluation library called directly, or a worker started by
  ;; hand, does not pass here; two run roots are two pools; and the slot is
  ;; released when core.sc ends even if a descendant it orphaned is still
  ;; running. K is operator configuration, and two processes with different
  ;; K on one root see different ranges -- a mismatch is not detected.

  ;; K: THEOURGIA_EVAL_SLOTS in this process's environment, else the online
  ;; processors (ffi.sc's one reading, with its test seam). -> a positive
  ;; integer, or #f when the value is not one -- never unlimited by accident.
  (define (eval-slots-count)
    (let ((v (env-or "THEOURGIA_EVAL_SLOTS")))
      (if v
          (let ((n (string->number v 10)))
            (and n (exact? n) (integer? n) (> n 0) n))
          (online-processors))))

  ;; -> (values <lock> #f) once a slot is held, or (values #f <refusal>):
  ;; (error bad-request (reason eval-slots)) for a K that is not a positive
  ;; integer, (error eval-busy (slots K) (waited-ms n)) when no slot came
  ;; free within timeout-ms. The wait is not part of the evaluation's own
  ;; deadline, which starts at ready. A filesystem condition making the
  ;; slots raises, for the request's table.
  ;;
  ;; NEVER: NO ATTEMPT AFTER THE BUDGET. The slots are tried at once, then
  ;; every 100 ms, and the budget is read before EVERY attempt, each slot's
  ;; included: no attempt begins once a reading has reached timeout-ms (one
  ;; begun just before can finish just after). Each sleep is cut to what is
  ;; left of the budget, and the refusal comes once a reading reaches it,
  ;; which scheduling can put somewhat after; no further whole-step sleep is
  ;; requested. The one exception is the very first attempt, on the first
  ;; slot, which is made whatever the clock reads: a millisecond boundary
  ;; crossed at the start must not refuse a request with a 1 ms budget on
  ;; an empty pool. The wait is measured on the monotonic clock, as the MCP
  ;; shell's watchdog is, so a step of the wall clock neither stretches it
  ;; nor cuts it short.
  (define (monotonic-ms)
    (let ((t (current-time 'time-monotonic)))
      (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

  (define (eval-admit! timeout-ms)
    (let ((k (eval-slots-count)))
      (if (not k)
          (values #f '(error bad-request (reason eval-slots)))
          (let ((dir (string-append (run-root) "/admission")))
            (define (slot i) (string-append dir "/" (number->string i)))
            (mkdir-p-unrecorded! dir)
            (let make ((i 0))
              (when (< i k)
                (file-ensure-unrecorded! (slot i))
                (make (+ i 1))))
            (hold-point! 'eval-admission)
            (let ((start (monotonic-ms)))
              (define (elapsed) (- (monotonic-ms) start))
              (define (busy waited)
                (values #f (list 'error 'eval-busy (list 'slots k) (list 'waited-ms waited))))
              (let try ((first? #t))
                (let ((held (let scan ((i 0))
                              (and (< i k)
                                   (or (and first? (= i 0)) (< (elapsed) timeout-ms))
                                   (or (lock-try-acquire! (slot i) 'exclusive)
                                       (scan (+ i 1)))))))
                  (if held
                      (begin
                        (guard (e (#t (guard (x (#t #f)) (lock-release! held)) (raise e)))
                          (parameterize ((theourgia-stage 'admission))
                            (fd-close-on-exec! (lock-fd held))))
                        (values held #f))
                      (let ((waited (elapsed)))
                        (if (>= waited timeout-ms)
                            (busy waited)
                            (begin
                              (sleep (make-time 'time-duration
                                                (* 1000000 (min 100 (- timeout-ms waited))) 0))
                              (let ((waited (elapsed)))
                                (if (>= waited timeout-ms)
                                    (busy waited)
                                    (try #f))))))))))))))

)
