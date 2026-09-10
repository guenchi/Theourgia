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
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
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
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" A " " d "/snap"))
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
;; current buffer, so delivery must open it -- and must abort.
(build!)
(seg! 1 (r 1)) (seg! 2 (r 2)) (seg! 3 (r 3))
(want "CONTROL: a segment delivery MUST read, made unreadable, aborts the load"
      (survey (lambda () (unreadable! 2)))
      (list (list 3 R 3) (list (list 1 1 1) (list 2 2 2) (list 3 3 3)) '() '(1) 'aborted #f))
(readable! 2)

(printf "\n~a failures\n" bad)
(printf "log8 complete\n")
