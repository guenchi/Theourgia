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

;; The defects the three review letters named, each with a case that
;; would have failed before the fix and a control that must keep passing.
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace))

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

(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define (raises? t) (guard (e (#t #t)) (t) #f))
(define d (test-dir "log5work"))
(system (string-append "rm -rf " d " " d "-home; mkdir -p " d))

(printf "== a stale temporary must never be reused ==\n")
;; Before: the temp name was opened with create-if-absent and no
;; truncation, so a leftover from a crashed run with the same pid and
;; counter was written INTO, leaving new bytes followed by old ones.
(define target (string-append d "/t.sexp"))
(atomic-write! target (string->utf8 "AAAAAAAAAAAAAAAAAAAAAAAA\n") 'snapshot)
(want "first write" (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "AAAAAAAAAAAAAAAAAAAAAAAA\n")
;; plant a leftover under every name the next call could pick
(let loop ((i 1))
  (when (< i 8)
    (call-with-port (open-file-output-port
                      (string-append target ".tmp-" (number->string (get-process-id))
                                     "-" (number->string i))
                      (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 "STALESTALESTALESTALESTALE\n"))))
    (loop (+ i 1))))
(atomic-write! target (string->utf8 "BB\n") 'snapshot)
(want "a short replacement is exactly itself, with no stale tail"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "BB\n")

(printf "== a trailing slash is refused before anything is written ==\n")
(want "refused" (raises? (lambda () (atomic-write! (string-append d "/sub/") (string->utf8 "x") 'snapshot))) #t)
;; The first version of this row compared one expression with ITSELF,
;; which is true however the code behaves. The listing is captured
;; before the refused call and compared with the listing after it.
(define before-slash (sort string<? (directory-list d)))
(want "refused again, with the directory captured before and after"
      (begin (raises? (lambda () (atomic-write! (string-append d "/sub2/") (string->utf8 "x") 'snapshot)))
             (sort string<? (directory-list d)))
      before-slash)

(printf "== the install declares a stage, so a fault can be aimed at it ==\n")
;; Without a declared stage every fault selecting `snapshot` was inert
;; and the run looked exactly like one where the injection had fired and
;; found nothing to break.
(want "a snapshot-stage fsync fault reaches this install"
      (guard (e ((fs-error? e) (list 'raised (fs-error-op e) (fs-error-errno e))))
        (atomic-write! (string-append d "/aimed.sexp") (string->utf8 "x\n") 'snapshot)
        'not-reached)
      (if (theourgia-fault) '(raised fsync 5) 'not-reached))
(printf "  (fault selected: ~s)\n" (theourgia-fault))

(printf "== meta must carry a supported format version ==\n")
(define (store-with meta)
  (let ((s (string-append d "/store")))
    (system (string-append "rm -rf " s "; mkdir -p " s "/writers"))
    (call-with-port (open-file-output-port (string-append s "/meta.sexp") (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 meta))))
    ;; A load now holds the store's shared lock for its whole duration,
    ;; so a store without a lock file cannot be opened at all -- and the
    ;; lock helpers deliberately do not create one. init writes it; this
    ;; fixture stands in for init.
    (file-ensure! (string-append s "/lock"))
    s))
(want "format 999 is refused"
      (guard (e ((log-error? e) (log-error-kind e))) (log-open (store-with "((format 999))\n")) 'opened)
      'meta)
(want "a datum with no format is refused"
      (guard (e ((log-error? e) (log-error-kind e))) (log-open (store-with "((store-id \"x\"))\n")) 'opened)
      'meta)
(want "CONTROL: format 1 opens"
      (guard (e ((log-error? e) (log-error-kind e)))
        (begin (log-open (store-with "((format 1) (store-id \"x\"))\n")) 'opened))
      'opened)

(printf "== a fifo named like a segment is not a segment ==\n")
;; Opening a fifo for reading blocks until a writer appears -- and the
;; reader does that INSIDE the shared lock, so a name check alone can
;; hold the store's lock forever. Nothing raises, so no handler helps.
(define fdir (string-append d "/writers/aaaaaaaa"))
(system (string-append "mkdir -p " fdir "; mkfifo " fdir "/000002.sexp"))
(call-with-port (open-file-output-port (string-append fdir "/000001.sexp")
                                       (file-options no-fail))
  (lambda (p) (put-bytevector p (string->utf8 "x\n"))))
(want "the fifo is not enumerated as a segment"
      (enumerate-segment-files d "aaaaaaaa") '(1))
(want "CONTROL: a regular file with the same name IS enumerated"
      (begin (system (string-append "rm -f " fdir "/000002.sexp"))
             (call-with-port (open-file-output-port (string-append fdir "/000002.sexp")
                                                    (file-options no-fail))
               (lambda (p) (put-bytevector p (string->utf8 "y\n"))))
             (enumerate-segment-files d "aaaaaaaa"))
      '(1 2))

(printf "== a creation failure that is not a name clash is re-raised ==\n")
;; Catching everything turned an unwritable directory into 65 retries
;; and then a generic error naming neither the cause nor the candidate.
(define ro (string-append d "/readonly"))
(system (string-append "rm -rf " ro "; mkdir -p " ro "; chmod 500 " ro))
(want "the real cause survives, rather than becoming a generic temp error"
      (guard (e ((log-error? e) (list 'generic (log-error-kind e)))
                (#t 'raised-something-specific))
        (atomic-write! (string-append ro "/x.sexp") (string->utf8 "x") 'snapshot)
        'wrote)
      'raised-something-specific)
(system (string-append "chmod 700 " ro))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log5 complete\n")
