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

;;; (theourgia timing-wrappers) -- the write path's timing wrappers, decided at
;;; expansion (test-only).
;;;
;;; THEOURGIA_TIMING=1 IN THE PROCESS THAT EXPANDS THIS LIBRARY, the way
;;; THEOURGIA_INJECT's branches are read (ffi.sc). Unset, each wrapper expands
;;; to the expression it wraps -- no closure, no call, no flag test -- and the
;;; seam (timing.sc) is never loaded (from source, nor compiled; a build
;;; compiles timing.sc like any library and calls it only when built set).
;;; Set, a wrapper expands to a call into (theourgia timing), the clock,
;;; counters, windows and printer, reached through `environment`. Development
;;; runs from source, so expansion and run are one process and the variable
;;; behaves as a run-time one; a compiled object calls the seam only when it
;;; was built with the variable set.
;;;
;;; A LIBRARY OF ITS OWN, NOT PART OF ffi.sc. ffi.sc exported no keyword
;;; before the wrappers were put there, and with them it did: every program
;;; loads ffi.sc, and its start-up from source measured about 3% slower with
;;; the wrappers defined in it and never used (2026-10-13, the gate's
;;; alternation). Only store.sc and code-project.sc import them.
;;;
;;; THE WRAPPERS ARE DEFINED IN THE BRANCH ITSELF, so which of them exist is
;;; decided in the one expansion that decides whether timing-seam exists: a
;;; transformer that read the variable again when it was used could, with
;;; this library compiled unset and another expanded set, name a helper the
;;; compiled library does not have.
;;;   (timed <phase> <expr>): <expr>, timed as a request phase
;;;   (intent-timed <i> <expr>): <expr>, its time added to phase I of the window
;;;   (timing-call <name> <arg> ...): the seam's NAME applied, or #f -- unset,
;;;     the arguments are not evaluated
;;;   (timing-quiet <expr>): <expr>, nothing it does counted or printed

(library (theourgia timing-wrappers)
  (export timed intent-timed timing-call timing-quiet)
  (import (chezscheme))

  (meta define timing-mode
    (let ((v (getenv "THEOURGIA_TIMING")))
      (and v (string=? v "1") #t)))
  (meta-cond
    (timing-mode
     ;; the seam's procedures, looked up once each
     (define timing-procs (make-eq-hashtable))
     (define (timing-seam name)
       (or (hashtable-ref timing-procs name #f)
           (let ((p (eval name (environment '(theourgia timing)))))
             (hashtable-set! timing-procs name p)
             p)))
     (define-syntax timed
       (syntax-rules () ((_ what e) ((timing-seam 'timed-run) what (lambda () e)))))
     (define-syntax intent-timed
       (syntax-rules () ((_ i e) ((timing-seam 'intent-timed-run) i (lambda () e)))))
     (define-syntax timing-call
       (syntax-rules () ((_ name arg ...) ((timing-seam 'name) arg ...))))
     (define-syntax timing-quiet
       (syntax-rules () ((_ e) ((timing-seam 'quiet-run) (lambda () e))))))
    (else
     (define-syntax timed
       (syntax-rules () ((_ what e) e)))
     (define-syntax intent-timed
       (syntax-rules () ((_ i e) e)))
     (define-syntax timing-call
       (syntax-rules () ((_ name arg ...) #f)))
     (define-syntax timing-quiet
       (syntax-rules () ((_ e) e))))))
