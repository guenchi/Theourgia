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

(import (chezscheme))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME -- see the note in the other
;; fixtures. A hardcoded scratchpad path is green only while that exact
;; directory survives.
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
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))
(define probe-dir (test-dir "ffiprobe"))

(load-shared-object "libc.dylib")
(printf "foreign-entry? __error = ~a\n" (foreign-entry? "__error"))
(printf "foreign-entry? __errno_location = ~a\n" (foreign-entry? "__errno_location"))
(define c-errloc (foreign-procedure "__error" () void*))
(define (errno) (foreign-ref 'int (c-errloc) 0))
(define c-open (foreign-procedure "open" (string int) int))
(define c-close (foreign-procedure "close" (int) int))
(define c-write (foreign-procedure "write" (int u8* size_t) ssize_t))
(define c-lseek (foreign-procedure "lseek" (int integer-64 int) integer-64))
(define c-ftruncate (foreign-procedure "ftruncate" (int integer-64) int))
(define c-fsync (foreign-procedure "fsync" (int) int))
(define c-flock (foreign-procedure "flock" (int int) int))
;; a failing open, to see errno arrive
(let ((fd (c-open "/no/such/path/at/all" 0)))
  (printf "open missing -> fd=~a errno=~a (ENOENT should be 2)\n" fd (errno)))
(define p (string-append probe-dir "/ffiprobe.dat"))
(let ((op (open-file-output-port p (file-options no-fail no-truncate))))
  (close-port op))
(let ((fd (c-open p 2)))
  (printf "open rdwr fd=~a\n" fd)
  (let ((n (c-write fd (string->utf8 "hello world\n") 12)))
    (printf "write -> ~a\n" n))
  (printf "lseek end -> ~a\n" (c-lseek fd 0 2))
  (printf "flock ex -> ~a\n" (c-flock fd 2))
  (printf "flock un -> ~a\n" (c-flock fd 8))
  (printf "fsync -> ~a\n" (c-fsync fd))
  (printf "ftruncate 5 -> ~a\n" (c-ftruncate fd 5))
  (printf "lseek end after trunc -> ~a\n" (c-lseek fd 0 2))
  (c-close fd))
;; directory fd + fsync
(let ((fd (c-open probe-dir 0)))
  (printf "dir fd=~a fsync=~a\n" fd (c-fsync fd))
  (c-close fd))
(printf "file contents now: ~s\n"
        (let ((ip (open-file-input-port p)))
          (let ((bv (get-bytevector-all ip))) (close-port ip) bv)))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "ffiprobe complete\n")
