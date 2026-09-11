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
(import (chezscheme) (theourgia ffi))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Naming an absolute path
;; under one session's scratchpad is green only while that exact
;; directory survives: tmp is swept, and another machine has no such path
;; at all -- the whole suite would then be red for a reason with nothing
;; to do with the code under test. THEOURGIA_TEST_ROOT overrides the
;; default; the pid keeps two runs, or two fixtures, out of each other's
;; way. The directories are left behind deliberately, as evidence.
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

(define dir (test-dir "ffiwork"))
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define log (string-append dir "/000001.sexp"))
(define lock (string-append dir "/lock"))

(printf "-- ensure/open/write/fsync/size --\n")
(define fd (fd-open log '(read-write create append)))
(printf "fd=~a\n" (> fd 0))
(printf "write-all -> ~a\n" (write-all! fd (string->utf8 "abcdef\n") "log"))
(fsync! fd "log" 'commit)
(fd-close fd)
(printf "file-size -> ~a\n" (file-size log))

(printf "-- append flag really appends --\n")
(let ((fd (fd-open log '(read-write append))))
  (write-all! fd (string->utf8 "ghij\n") "log")
  (fsync! fd "log" 'commit)
  (fd-close fd))
(printf "file-size -> ~a\n" (file-size log))
(printf "contents ~s\n" (let ((p (open-file-input-port log)))
                          (let ((b (get-bytevector-all p))) (close-port p) (utf8->string b))))

(printf "-- ftruncate to last newline --\n")
(let ((fd (fd-open log '(read-write))))
  (ftruncate! fd 7 "log")
  (fsync! fd "log" 'commit)
  (printf "fd-size -> ~a\n" (fd-size fd "log"))
  (fd-close fd))
(printf "file-size -> ~a\n" (file-size log))

(printf "-- fsync-dir --\n")
(fsync-dir! dir 'commit)
(printf "ok\n")

(printf "-- lock held across body, released on exception --\n")
;; The lock file is created once, at store init -- the lock helpers
;; deliberately do not create it, so that a reader cannot.
(file-ensure! lock)
(with-exclusive-lock lock (lambda (lfd) (printf "in exclusive, lfd int? ~a\n" (integer? lfd))))
(guard (e (#t (printf "escaped with: ~a\n" (condition? e))))
  (with-exclusive-lock lock (lambda (lfd) (error 'test "boom"))))
(with-exclusive-lock lock (lambda (lfd) (printf "re-acquired after exception: yes\n")))
(with-shared-lock lock (lambda (lfd) (printf "shared ok\n")))

(printf "-- errors carry op and errno --\n")
(guard (e ((fs-error? e)
           (printf "op=~a target=~a errno=~a\n"
                   (fs-error-op e) (fs-error-target e) (fs-error-errno e))))
  (fd-open (string-append dir "/no-such-file") '(read)))
(guard (e ((fs-error? e)
           (printf "bad fd: op=~a errno=~a\n" (fs-error-op e) (fs-error-errno e))))
  (fsync! 9999 "nope" 'commit))

(printf "-- igropyr durable-error? accepts our shape --\n")
(printf "~a\n"
        (guard (e (#t ((let () (import (only (igropyr durable) durable-error?)) durable-error?) e)))
          (fd-open (string-append dir "/nope") '(read))))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "smoke-ffi complete\n")
