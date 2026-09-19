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

;; Retirement and quarantine coordinates, verified at scan time.
;;
;; Both markers name a boundary, and neither was checked against what the
;; bytes actually say. A retirement marker was believed; a quarantine fork
;; lowered end-seq while end-segment, end-offset and the ranges went on
;; describing history the fork excludes -- so delivery still opened the
;; excluded segments.
;;
;; THE READ WITNESS IS AN UNREADABLE FILE. There is no trace event for
;; reading a segment, and none is added for this: a segment that delivery
;; must not open is made unreadable AFTER discovery, and the load
;; standing rather than aborting is the evidence it was not opened. The
;; abort IS the open.
;;
;; That witness is a NEGATIVE reading -- nothing happened -- so it also
;; passes when the stimulus never arrived: running as root, or a chmod
;; that did not take, makes those rows green for the wrong reason. The
;; last row here makes a segment delivery MUST read unreadable and
;; requires the abort, which is what shows the instrument bites.
;;
;; THE RULE THESE ROWS ENCODE, as ruled by the main session: a retirement
;; marker is a logical claim whose seq is the primary coordinate, and its
;; segment/offset are physical coordinates that exist to check it.
;; The extent is min(declared seq, scan-verified seq). A physical
;; coordinate that disagrees is an integrity error and nothing more: it
;; never lowers the extent by itself, because a claim already shown to be
;; wrong about this file cannot be trusted to bound it either.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi))

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

(define d (test-dir "log8work"))
(define A "k3m9x2qa")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" A " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n")))
(define (seg! n . rs)
  (put! (string-append d "/writers/" A "/00000" (number->string n) ".sexp") (apply cat rs)))
(define (segpath n) (string-append d "/writers/" A "/00000" (number->string n) ".sexp"))
(define (retire! s) (put! (string-append d "/writers/" A "/retired.sexp") (string->utf8 s)))
(define (quar! f)
  (put! (string-append d "/writers/" A "/quarantine.sexp")
        (string->utf8 (string-append "((format 1) (fork " (number->string f) "))\n"))))
(define (unreadable! n) (system (string-append "chmod 000 " (segpath n))))
(define (readable! n) (system (string-append "chmod 644 " (segpath n))))
;; Every record this fixture writes is the same size, which is what lets
;; the expected offsets below be written as multiples of it rather than
;; read back out of the implementation.
(define R (bytevector-length (rec 1 '() '(put "a.1" ()))))
(define (r n) (rec n '() (list 'put (string-append "a." (number->string n)) '())))

;; Reads the whole picture at once: an endpoint is three coordinates and a
;; row that checks only end-seq cannot see two of them disagreeing.
(define (survey . damage)
  (let* ((ls (log-open d)) (p (load-prefix ls A)) (seen '()))
    (for-each (lambda (t) (t)) damage)
    (load-deliver! ls '() (lambda (w seg off seq . rest) (set! seen (cons seq seen))))
    (let ((o (load-outcome ls))
          (rt (discovery-retired p)))
      (list (list (discovery-end-segment p) (discovery-end-offset p) (discovery-end-seq p))
            (discovery-segment-ranges p)
            (map log-error-kind (discovery-integrity p))
            (reverse seen)
            (if (pair? o) (car o) o)
            ;; only the coordinates: the trailing version is a checksum of
            ;; the marker's own bytes and would pin the fixture's text
            (and rt (not (eq? (car rt) 'malformed)) (list-head rt 3))))))

(printf "== a retirement marker is verified against the bytes, not believed ==\n")

;; Pre-fix reading: ((1 120 2) ((1 1 2)) () (1 2) open (1 120 1)) -- the
;; marker declares the writer ended at seq 1 and the reader published 2.
(build!)
(seg! 1 (r 1) (r 2))
(retire! (string-append "((prefix 1 " (number->string (* 2 R)) " 1) (tx \"t1\"))\n"))
(want "the declared seq bounds the extent, and the disagreeing offset is flagged"
      (survey)
      (list (list 1 R 1) (list (list 1 1 1)) '(retired-mismatch) '(1) 'open
            (list 1 (* 2 R) 1)))

;; THE OTHER HALF OF THE MINIMUM. Without this row, "always believe the
;; marker's seq" passes the row above -- and would publish seq 3 out of a
;; file that only ever held two records.
(build!)
(seg! 1 (r 1) (r 2))
(retire! (string-append "((prefix 1 " (number->string (* 2 R)) " 3) (tx \"t1\"))\n"))
(want "a marker claiming more than the bytes hold is bounded by the bytes"
      (survey)
      (list (list 1 (* 2 R) 2) (list (list 1 1 2)) '(retired-mismatch) '(1 2) 'open
            (list 1 (* 2 R) 3)))

;; Pre-fix reading: ((1 120 2) ((1 1 2)) () (1 2) open (9 0 1)) -- segment
;; 9 does not exist, so the branch that would honour the marker never ran
;; and the marker had no effect at all.
(build!)
(seg! 1 (r 1) (r 2))
(retire! "((prefix 9 0 1) (tx \"t1\"))\n")
(want "an absent named segment is flagged, and the declared seq still bounds"
      (survey)
      (list (list 1 R 1) (list (list 1 1 1)) '(retired-missing-segment) '(1) 'open
            (list 9 0 1)))

;; The companion: a marker that cannot be located must not be turned into
;; a reason to discard history that is present and valid.
(build!)
(seg! 1 (r 1) (r 2))
(retire! "((prefix 9 0 5) (tx \"t1\"))\n")
(want "an absent named segment does not cut history the bytes support"
      (survey)
      (list (list 1 (* 2 R) 2) (list (list 1 1 2)) '(retired-missing-segment) '(1 2) 'open
            (list 9 0 5)))

;; GREEN TWIN. The offset is past the end of a file holding two records
;; while the declared seq agrees with them: min(2, 2) = 2, and the bad
;; offset is flagged without moving the extent.
;;
;; My first version of this row expected an extent of 0, on the reading
;; that an unverifiable marker should stop everything. The main session
;; ruled otherwise and the reasoning is the point: no coordinate supports
;; 0, so that expectation would have made "unverifiable" mean "discard
;; valid history".
(build!)
(seg! 1 (r 1) (r 2))
(retire! "((prefix 1 200 2) (tx \"t1\"))\n")
(want "CONTROL: a bad offset is flagged and the agreeing seq still stands"
      (survey)
      (list (list 1 (* 2 R) 2) (list (list 1 1 2)) '(retired-beyond-file) '(1 2) 'open
            (list 1 200 2)))

;; GREEN TWIN. Without it the cheapest way to pass every row above is to
;; report an integrity error for every retirement in the store.
(build!)
(seg! 1 (r 1) (r 2))
(retire! (string-append "((prefix 1 " (number->string R) " 1) (tx \"t1\"))\n"))
(want "CONTROL: coordinates that agree are accepted with no diagnostic"
      (survey)
      (list (list 1 R 1) (list (list 1 1 1)) '() '(1) 'open (list 1 R 1)))

(printf "== the quarantined suffix leaves the extent entirely ==\n")

;; Pre-fix reading: ((3 60 1) ((1 1 1) (2 2 2) (3 3 3)) () (1) aborted #f)
;; -- end-seq was capped at the fork and every other coordinate went on
;; describing segment 3, so delivery walked the ranges into segment 2 and
;; the whole load aborted on history the fork excludes.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(quar! 2)
(want "the endpoint, the ranges and the delivery all stop at the fork"
      (survey (lambda () (unreadable! 2)))
      (list (list 1 R 1) (list (list 1 1 1)) '() '(1) 'open #f))
(readable! 2)



;; The same store without the unreadable segment does not complain about
;; itself at all -- which is why the row above has to force the question.
;; Here the retirement sits ABOVE the fork: the fork wins, and the
;; retirement must still be reported rather than becoming a missing-
;; segment error because scanning stopped before reaching segment 3.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(quar! 2)
(retire! (string-append "((prefix 3 " (number->string R) " 3) (tx \"t1\"))\n"))
(want "an earlier fork wins over a later retirement, which is still reported"
      (survey (lambda () (unreadable! 2)))
      (list (list 1 R 1) (list (list 1 1 1)) '() '(1) 'open (list 3 R 3)))
(readable! 2)

;; CONTROL: without a fork the same three segments deliver in full. A cap
;; applied unconditionally would pass both rows above.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(want "CONTROL: without a fork the same store delivers all three"
      (survey)
      (list (list 3 R 3) (list (list 1 1 1) (list 2 2 2) (list 3 3 3)) '() '(1 2 3) 'open #f))

;; CONTROL FOR THE INSTRUMENT ITSELF. The two fork rows are negative
;; readings: they pass when delivery skips the segment AND when the chmod
;; never bit. Segment 2 here is inside the extent and is not the retained
;; current buffer, so delivery must open it -- and must refuse.
;;
;; NOTHING IS DELIVERED. This used to hand out record 1 and then fail on
;; segment 2's read; the barrier now refuses before the first callback,
;; because a reader may not promise a record it could not flush. The
;; earlier version had already promised record 1.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(want "CONTROL: a segment delivery MUST read, made unreadable, stops the load"
      (guard (e (#t 'raised)) (survey (lambda () (unreadable! 2))))
      'raised)
(readable! 2)
;; TWIN: the same store with nothing made unreadable delivers all three,
;; so the row above is about the segment and not about the survey.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(want "TWIN: undamaged, the same survey delivers all three"
      (survey)
      (list (list 3 R 3) (list (list 1 1 1) (list 2 2 2) (list 3 3 3)) '() '(1 2 3) 'open #f))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log8 complete\n")
