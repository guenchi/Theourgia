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

;; Does R6RS guard re-enter the dynamic extent when a clause DECLINES?
;; If it does, the re-entry check I added to call-with-lock fires during
;; ordinary exception propagation and replaces the real error.
(import (chezscheme) (theourgia ffi))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    (system (string-append "mkdir -p " path))
    path))

(define dir (test-dir "gwork"))
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define lock (string-append dir "/lock"))
(printf "outer guard sees: ~s\n"
  (guard (e (#t (list 'outer (if (and (vector? e) (> (vector-length e) 0)) (vector-ref e 0) 'not-a-vector))))
    (guard (e ((and (vector? e) (eq? (vector-ref e 0) 'never-matches)) 'inner))
      (with-exclusive-lock lock
        (lambda (fd) (raise (vector 'durable-error 'write (cons "/x" 5))))))))
(printf "plain dynamic-wind control: ~s\n"
  (let ((log '()))
    (guard (e (#t (reverse (cons 'caught log))))
      (guard (e ((eq? e 'never) 'inner))
        (dynamic-wind
          (lambda () (set! log (cons 'in log)))
          (lambda () (raise 'boom))
          (lambda () (set! log (cons 'out log))))))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "verify-guard complete\n")
