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

;; The session shape needs a lock that survives the call that took it.
;; What must be true: it is really held after the acquiring call
;; RETURNS, the scoped helpers still work (they are built on it), and
;; release is idempotent.
(import (chezscheme) (theourgia ffi) (theourgia trace))

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

(define d (test-dir "ulwork"))
(system (string-append "rm -rf " d "; mkdir -p " d))
;; THE SECOND PROCESS IS WRITTEN HERE, because nothing else writes it.
;; This fixture loaded `<d>/../probe.sc` and never created it: when the
;; file was absent, both probe rows printed
;; `Exception in load: ... no such file or directory` AS THEIR READING
;; and the file still printed its completion line. A row whose answer is
;; the text of an error is not a row that failed -- it is a row that was
;; never asked, and it reads like an observation about locking.
(define probe-path (string-append d "/../probe.sc"))
(call-with-port (open-file-output-port probe-path (file-options no-fail)
                                       'block (native-transcoder))
  (lambda (o)
    (put-string o
      (string-append
        "(import (chezscheme) (theourgia ffi))\n"
        "(let* ((p (cadr (command-line))) (l (lock-acquire! p 'exclusive)))\n"
        "  (printf \"acquired\\n\")\n"
        "  (lock-release! l))\n"))))
(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

;; AN EMPTY FILE READS BACK AS eof, NOT AS "". `get-string-all` on a
;; file with no bytes answers the eof object, and the empty file is
;; exactly what the held-lock row expects -- so the helper that was meant
;; to normalise whitespace raised on the one reading the row exists for.
(define (trimmed t)
  (if (not (string? t))
      ""
      (let loop ((i (string-length t)))
        (cond ((= i 0) "")
              ((char-whitespace? (string-ref t (- i 1))) (loop (- i 1)))
              (else (substring t 0 i))))))
(define lock (string-append d "/lock"))
(file-ensure! lock)
(define (take) (lock-acquire! lock 'exclusive))
(define l (take))
(printf "held after the acquiring call returned: ~a\n" (lock-held? l))
(printf "fd is an integer: ~a\n" (integer? (lock-fd l)))
;; a second process must not get in while we hold it
(system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                       " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                       " perl -e 'alarm 4; exec @ARGV' scheme --script " d "/../probe.sc " lock " > " d "/r1 2>&1"))
;; WHILE THE LOCK IS HELD the second process gets nothing: it blocks and
;; the alarm ends it, so its output is empty. Asserted rather than
;; printed, so that "it printed something unexpected" is a failure
;; instead of a line someone has to read.
(want "while held, the second process does not get in"
      (trimmed (call-with-input-file (string-append d "/r1") get-string-all))
      "")
(lock-release! l)
(printf "release is idempotent: ")
(lock-release! l)
(printf "yes\n")
(system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                       " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                       " perl -e 'alarm 4; exec @ARGV' scheme --script " d "/../probe.sc " lock " > " d "/r2 2>&1"))
;; AND AFTER THE RELEASE IT DOES. Without this half the row above is
;; also passed by a probe that can never acquire anything -- a wrong
;; path, a missing library, a script that does not run at all. That is
;; exactly how this fixture was reading before: empty output from a
;; process that never started looks like a lock working perfectly.
(want "after release, the second process acquires it"
      (trimmed (call-with-input-file (string-append d "/r2") get-string-all))
      "acquired")
(printf "scoped helper still works: ~a\n" (with-exclusive-lock lock (lambda (fd) 'ok)))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "unscoped-lock complete\n")
