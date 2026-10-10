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

;; Piece 1: atomic-write!, segment naming/enumeration, the manifest.
;; Every row has a control -- an assertion that only fires when the
;; thing under test is wrong, paired with one that must keep passing.
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

;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   want ~s" expect)))))

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
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define (raises? t) (guard (e (#t #t)) (t) #f))
(define d (test-dir "log1work"))
(system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/k3m9x2qa " d "/writers/c9xq01mz"))

(printf "== segment names ==\n")
(want "1 -> 000001.sexp" (segment-file-name 1) "000001.sexp")
(want "42 -> 000042.sexp" (segment-file-name 42) "000042.sexp")
(want "parse back" (segment-file-number "000042.sexp") 42)
(want "the sibling files are NOT segments"
      (map segment-file-number '("owner.sexp" "retired.sexp" "published.sexp" "quarantine.sexp"))
      '(#f #f #f #f))
(want "wrong digit count rejected"
      (map segment-file-number '("00001.sexp" "0000001.sexp" "00000a.sexp" "000000.sexp"))
      '(#f #f #f #f))
(want "zero and negative refused"
      (list (raises? (lambda () (segment-file-name 0)))
            (raises? (lambda () (segment-file-name -1)))
            (raises? (lambda () (segment-file-name 1234567))))
      '(#t #t #t))

(printf "== atomic-write! ==\n")
(define target (string-append d "/meta.sexp"))
(atomic-write! target (string->utf8 "(format 1)\n") 'snapshot)
(want "file has the content"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "(format 1)\n")
(atomic-write! target (string->utf8 "(format 2)\n") 'snapshot)
(want "replacement replaces"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "(format 2)\n")
(want "no temporary left behind on success"
      (filter (lambda (f) (and (> (string-length f) 4) (string=? (substring f 0 4) "meta")))
              (directory-list d))
      '("meta.sexp"))
;; THE DIRECTORY FLUSH IS INVISIBLE TO EVERY FUNCTIONAL ASSERTION -- the
;; file reads back correctly whether or not it happened. The trace is
;; the only evidence, so it is asserted rather than printed.
(define trace-lines '())
;; ONLY A LINE THAT STARTS "(trace " IS AN EVENT. Matching a bare
;; open paren also matched the parenthesis inside a PAIR subject --
;; (rename (old . new)) yielded "rename" and then a fragment of a path
;; as if it were a second event. Subjects have been pairs since flock;
;; this helper only met one when rename started being traced.
(define (ops-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (acc '()))
      (cond
        ((>= i n) (reverse acc))
        ((and (<= (+ i 7) n) (string=? (substring text i (+ i 7)) "(trace "))
         (let ((j (let scan ((j (+ i 7)))
                    (if (or (>= j n) (char=? (string-ref text j) #\space))
                        j
                        (scan (+ j 1))))))
           (loop j (cons (substring text (+ i 7) j) acc))))
        (else (loop (+ i 1) acc))))))
(let ((p (open-output-string)))
  (parameterize ((theourgia-trace? #t) (current-error-port p))
    (atomic-write! (string-append d "/probe.sexp") (string->utf8 "x\n") 'snapshot))
  (set! trace-lines (get-output-string p)))
;; The whole directory-entry sequence is traced now: the crash model
;; reconstructs which entries survive by reading the trace, so it has to
;; see the temporary appear (create), the bytes land, the file flush, the
;; entry move (rename) and the directory flush. The rename's position
;; BETWEEN the two fsyncs is the ordering this row exists for, and an
;; untraced create would be an entry the reconstruction cannot know was
;; ever there.
(want "atomic-write! is create, write, fsync file, rename, fsync directory"
      (ops-of trace-lines) '("create" "write" "fsync" "rename" "fsync"))
(want "and the last fsync is the DIRECTORY, not the file"
      (let* ((ls (let loop ((i 0) (start 0) (acc '()))
                   (cond ((>= i (string-length trace-lines)) (reverse acc))
                         ((char=? (string-ref trace-lines i) #\newline)
                          (loop (+ i 1) (+ i 1) (cons (substring trace-lines start i) acc)))
                         (else (loop (+ i 1) start acc)))))
             (last (car (reverse ls))))
        (and (> (string-length last) 0)
             ;; THE SUBJECT'S OWN ENDING, not any ".tmp" anywhere in the
             ;; line: a work-directory root containing ".tmp" made this
             ;; reject a perfectly correct directory flush.
             (let* ((n (string-length last))
                    (cut (let loop ((i (- n 1)))
                           (cond ((< i 0) #f)
                                 ((char=? (string-ref last i) #\space) i)
                                 (else (loop (- i 1))))))
                    (e (or cut n)))
               (not (and (> e 4)
                         (let scan ((k 0))
                           (cond ((> (+ k 4) e) #f)
                                 ((and (string=? (substring last k (+ k 4)) ".tmp")
                                       (or (= (+ k 4) e)
                                           (char-numeric? (string-ref last (+ k 4)))
                                           (char=? (string-ref last (+ k 4)) #\-)))
                                  #t)
                                 (else (scan (+ k 1))))))))))
      #t)

(printf "== enumeration ==\n")
(define wa (string-append d "/writers/k3m9x2qa"))
(for-each (lambda (n)
            (call-with-port (open-file-output-port (string-append wa "/" (segment-file-name n))
                                                   (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "x\n")))))
          '(3 1 2))
;; sibling files that must not be counted
(for-each (lambda (f)
            (call-with-port (open-file-output-port (string-append wa "/" f) (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "()\n")))))
          '("owner.sexp" "quarantine.sexp"))
(want "ascending, and only the segments" (enumerate-segment-files d "k3m9x2qa") '(1 2 3))
(want "writers are found" (store-writers d) '("c9xq01mz" "k3m9x2qa"))
(want "a store with no writers dir" (store-writers "/nonexistent") '())

(printf "== manifest ==\n")
(want "missing manifest is #f, not ()" (read-manifest d "k3m9x2qa") #f)
(write-manifest! d "c9xq01mz" '((1 "aa" 1 2) (2 "bb" 3 4)))
(want "round trip" (read-manifest d "c9xq01mz") '((1 "aa" 1 2) (2 "bb" 3 4)))
(want "segments of it" (manifest-segments (read-manifest d "c9xq01mz")) '(1 2))
(want "empty manifest is () and not #f"
      (begin (write-manifest! d "c9xq01mz" '()) (read-manifest d "c9xq01mz")) '())
(want "descending or duplicate entries refused"
      (list (raises? (lambda () (write-manifest! d "c9xq01mz" '((2 "b" 3 4) (1 "a" 1 2)))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a" 1 2) (1 "b" 3 4)))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 5 1 2))))))
      '(#t #t #t))
;; ONE SHAPE, AND THE OLD ONE IS NOT A DIALECT OF IT. An entry of two
;; elements carries no range, so every layout decision made from it would
;; be made from a number nobody wrote down. It is refused rather than
;; read leniently, because a second accepted shape is a second format
;; with nobody to keep the two agreeing.
;; BOTH TWO-ELEMENT SHAPES. The dotted pair is what the old format
;; wrote; the two-element list is what a hand-edit produces, and a length
;; check that admits it would be caught by neither the dotted-pair row
;; nor the three-element one.
(want "a two-element entry is refused, and so is a range that runs backwards"
      (list (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a")))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 . "a")))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a" 1)))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a" 4 2)))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a" 0 2)))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 "a" 1 "x"))))))
      '(#t #t #t #t #t #t))
;; AND THE TWIN THAT MUST PASS, so the row above is refusing the shape
;; rather than everything handed to it.
(want "CONTROL: a well-formed entry is accepted"
      (begin (write-manifest! d "c9xq01mz" '((1 "a" 1 2)))
             (read-manifest d "c9xq01mz"))
      '((1 "a" 1 2)))

(printf "== which segments participate ==\n")
;; THE SELECTION RULE IS NOT TESTED HERE ANY MORE. loadable-segments and
;; current-segment-number answered "which segments count" and "where does
;; an append go" beside discover-prefix, which owns both -- and they
;; answered differently: with segment 2 listed but absent, loadable-
;; segments returned (1 3) while discovery stops the extent at 1. Nothing
;; called them, so they were a bypass waiting for a caller rather than a
;; second opinion anyone had acted on. They are gone, and each row that
;; stood here names its successor:
;;   "an unlisted segment file is IGNORED"  -> log6, L4'(a), through the
;;      extent, with the control that listing it makes it count
;;   "the local writer needs no manifest"   -> log6's local-writer extent
;;   "current is the highest"               -> log9 C7-1, which asserts
;;      physical-current, the field that actually names the append target
;;   listed-but-missing                     -> log9 C8-1
(write-manifest! d "c9xq01mz" '((1 "aa" 1 2)))
(for-each (lambda (n)
            (call-with-port (open-file-output-port
                              (string-append d "/writers/c9xq01mz/" (segment-file-name n))
                              (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "x\n")))))
          '(1 2))
(want "CONTROL: both files really exist"
      (enumerate-segment-files d "c9xq01mz") '(1 2))
;; A SHAPE THIS BUILD DOES NOT UNDERSTAND IS AN INTEGRITY ERROR, NOT A
;; CRASH. A two-element entry is refused by write-manifest! -- but a file
;; already holding one arrives through read-manifest, and there the
;; length check has to answer before any accessor reaches past the end of
;; the entry. Widening the check so a two-element entry passes it turns
;; that answer into a raw `caddr' error, which reaches the caller as a
;; broken tool rather than as a broken manifest.
(want "a manifest file carrying the old two-element entry is an integrity error"
      (begin
        (call-with-port (open-file-output-port
                          (string-append d "/writers/c9xq01mz/published.sexp")
                          (file-options no-fail))
          (lambda (p) (put-bytevector p (string->utf8 "((1 \"aa\"))\n"))))
        (guard (e ((log-error? e) (list 'log-error (log-error-kind e) (log-error-writer e)))
                  (#t (list 'raw-error)))
          (read-manifest d "c9xq01mz") 'no-error))
      '(log-error manifest "c9xq01mz"))
(want "a corrupt manifest raises a log-error, not a parse error"
      (begin
        (call-with-port (open-file-output-port (string-append d "/writers/c9xq01mz/published.sexp")
                                               (file-options no-fail no-truncate))
          (lambda (p) (put-bytevector p (string->utf8 "(oops"))))
        (guard (e ((log-error? e) (list 'log-error (log-error-kind e) (log-error-writer e))))
          (read-manifest d "c9xq01mz") 'no-error))
      '(log-error manifest "c9xq01mz"))
(printf "\n~a failures\n" bad)
;; A run that did not reach here is not a pass; the runner greps for it.
(printf "rows: ~a\n" rows-run)
(printf "log1 complete\n")
