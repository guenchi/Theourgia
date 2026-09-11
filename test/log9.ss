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

;; The coordinates the delivery rows never looked at.
;;
;; A review named eight mutations that the forty rows of log6 leave alive.
;; They share a shape: the rows assert what delivery YIELDS, and every one
;; of these mutations changes only HOW it got there -- which segment was
;; opened, what the recorded ranges said, where the endpoint pointed. A
;; wrong range with a compensating full-replay fallback produces byte-
;; identical output.
;;
;; So each row here asserts a coordinate, and carries beside it the
;; "output is still correct" reading that shows why the coordinate needed
;; its own row.
;;
;; THE READ WITNESS, as in log8: a segment delivery must not open is made
;; unreadable AFTER discovery, and the load standing rather than aborting
;; is the evidence. It is a negative reading, so each use is paired with a
;; segment delivery MUST open, made unreadable, which must abort.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (only (igropyr crypto) sha256 bytevector->hex))

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
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d (test-dir "log9work"))
(define A "k3m9x2qa")
(define M "c9xq01mz")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (r n) (rec n '() (list 'put (string-append "a." (number->string n)) '())))
(define R (bytevector-length (r 1)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (take-bytes bv n)
  (let ((o (make-bytevector n))) (bytevector-copy! bv 0 o 0 n) o))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" A " " d "/writers/" M " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n")))
(define (wpath w n) (string-append d "/writers/" w "/" (segment-file-name n)))
(define (seg! n . rs) (put! (wpath A n) (apply cat rs)))
(define (mseg! n . rs) (put! (wpath M n) (apply cat rs)))
(define (unreadable! w n) (system (string-append "chmod 000 " (wpath w n))))
(define (readable! w n) (system (string-append "chmod 644 " (wpath w n))))
(define (hash-of path)
  (bytevector->hex (sha256 (call-with-port (open-file-input-port path) get-bytevector-all))))

;; THE RANGE A SEGMENT HOLDS, READ OUT OF THE SEGMENT. A manifest entry
;; declares first and last sequence beside the hash. A fixture that
;; declared them from memory would be asserting its own arithmetic
;; rather than what it actually wrote, and the product's own check for a
;; manifest that contradicts its bytes would then be measuring the
;; fixture.
(define (segment-seqs bytes)
  (let ((text (utf8->string bytes)))
    (let loop ((i 0) (start 0) (seqs '()))
      (cond
        ((>= i (string-length text)) (reverse seqs))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? r) (eq? (car r) 'ok)) (cons (cadr r) seqs) seqs))))
        (else (loop (+ i 1) start seqs))))))

(define (manifest-entry n hash bytes)
  (let ((seqs (segment-seqs bytes)))
    (if (null? seqs)
        (list n hash 1 1)
        (list n hash (apply min seqs) (apply max seqs)))))

(define (manifest-entry-text n hash bytes)
  (let ((e (manifest-entry n hash bytes)))
    (string-append "(" (number->string n) " \"" hash "\" "
                   (number->string (caddr e)) " " (number->string (cadddr e)) ")")))

(define (slurp-bytes path)
  (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
    (if (eof-object? b) (make-bytevector 0) b)))
(define (publish! . ns)
  (put! (string-append d "/writers/" M "/published.sexp")
        (string->utf8
          (string-append "("
            (apply string-append
              (map (lambda (n)
                     (manifest-entry-text n (hash-of (wpath M n)) (slurp-bytes (wpath M n))))
                   ns))
            ")\n"))))
;; Three segments of two records each: the shape every range row needs,
;; because a single-segment store cannot tell a right range from a range
;; that merely starts in the right place.
(define (build-three!)
  (build!)
  (seg! 1 (r 1) (r 2)) (seg! 2 (r 3) (r 4)) (seg! 3 (r 5) (r 6)))
(define (deliver-with cut . damage)
  (let* ((ls (log-open d)) (seen '()))
    (for-each (lambda (t) (t)) damage)
    (let ((res (load-deliver! ls cut
                 (lambda (w seg off seq ts actor deps payload)
                   (set! seen (cons seq seen))))))
      (let ((o (load-outcome ls)))
        (list (if (pair? res) (car res) res)
              (reverse seen)
              (if (pair? o) (car o) o))))))
(define (ranges-of)
  (let* ((ls (log-open d)) (p (load-prefix ls A)))
    (load-commit! ls)
    (discovery-segment-ranges p)))

(printf "== C5-1: every range is asserted, not just the first ==\n")
(build-three!)
(want "each segment reports the sequences it actually holds"
      (ranges-of) '((1 1 2) (2 3 4) (3 5 6)))
(want "CONTROL: the delivered output is correct either way, which is why the row above exists"
      (deliver-with (list (cons A 2))) (list 'delivered '(3 4 5 6) 'open))

(printf "== C5-2 and C6-1: the cut's segment is opened directly ==\n")
;; A wrong range makes segment-holding fail to find the cut, and delivery
;; falls back to the first segment. The output is unchanged, so only the
;; unopened segment can show it.
(build-three!)
(want "with the cut in segment 2, segment 1 is never reopened"
      (deliver-with (list (cons A 3)) (lambda () (unreadable! A 1)))
      (list 'delivered '(4 5 6) 'open))
(readable! A 1)
(build-three!)
(want "CONTROL: the segment holding the cut IS opened, and its loss aborts"
      (deliver-with (list (cons A 3)) (lambda () (unreadable! A 2)))
      (list 'delivery-failed '() 'aborted))
(readable! A 2)

(printf "== C6-2: a cut at a segment's last sequence ==\n")
(build-three!)
(want "a cut at the last sequence of segment 1 starts in segment 2"
      (deliver-with (list (cons A 2)) (lambda () (unreadable! A 1)))
      (list 'delivered '(3 4 5 6) 'open))
(readable! A 1)
(build-three!)
(want "a cut at the last sequence of segment 2 starts in segment 3"
      (deliver-with (list (cons A 4)) (lambda () (unreadable! A 1) (unreadable! A 2)))
      (list 'delivered '(5 6) 'open))
(readable! A 1) (readable! A 2)

(printf "== C7-1: a freshly rotated empty segment ==\n")
;; L10's case at the level of coordinates rather than of end-seq alone.
(build!)
(seg! 1 (r 1) (r 2))
(put! (wpath A 2) (make-bytevector 0))
(let* ((ls (log-open d)) (p (load-prefix ls A)) (buf (discovery-current-buffer p)))
  (want "the extent ends in segment 1, and the append target is the empty segment 2"
        (list (list (discovery-end-segment p) (discovery-end-offset p) (discovery-end-seq p))
              (discovery-physical-current p))
        (list (list 1 (* 2 R) 2) (list 2 0)))
  (want "the retained buffer is the empty segment 2, not segment 1"
        (list (and buf (car buf)) (and buf (bytevector-length (cdr buf))))
        (list 2 0))
  (load-commit! ls))

(printf "== C7-2: a torn tail keeps its own range ==\n")
;; The existing L1 row checks end-seq and the torn segment. Losing the
;; range the torn branch builds would leave that row green.
(build!)
(let ((whole (cat (r 1) (r 2))))
  (put! (wpath A 1) (take-bytes whole (+ R 20))))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the endpoint stops after record 1 while the file is longer"
        (list (list (discovery-end-segment p) (discovery-end-offset p) (discovery-end-seq p))
              (discovery-physical-current p)
              (discovery-torn p)
              (discovery-segment-ranges p))
        (list (list 1 R 1) (list 1 (+ R 20)) (list 1 R 1) '((1 1 1))))
  (load-commit! ls))
(want "and the record before the residual is delivered"
      (deliver-with '()) (list 'delivered '(1) 'open))

(printf "== C8-1: a listed segment that is missing stops the extent before it ==\n")
(build!)
(mseg! 1 (r 1)) (mseg! 2 (r 2)) (mseg! 3 (r 3))
(publish! 1 2 3)
(system (string-append "rm -f " (wpath M 2)))
(let* ((ls (log-open d)) (p (load-prefix ls M)))
  (want "the extent stops at the gap, and the gap is named"
        (list (discovery-end-seq p) (discovery-segment-ranges p)
              (map log-error-kind (discovery-integrity p)))
        (list 1 '((1 1 1)) '(manifest-missing-segment)))
  (load-commit! ls))
(want "nothing from the gap onward is delivered"
      (deliver-with '()) (list 'delivered '(1) 'open))
(let ((ls (log-open d)))
  (snapshot-write! (string-append d "/snap/" (segment-file-name 1))
                   (list (cons M 2)) '((block "x" ())))
  (load-commit! ls))
(let* ((ls (log-open d)) (res (list (load-snapshot-cut ls) (load-snapshot-reason ls))))
  (load-commit! ls)
  (want "a snapshot whose cut reaches past the gap is refused"
        res (list #f 'unsupported-cut)))

(printf "== C8-2: a later segment whose hash is wrong stops the extent before it ==\n")
;; The existing hash row damages the FIRST segment, so the branch that
;; keeps the prefix below a hash failure has never been walked.
(build!)
(mseg! 1 (r 1)) (mseg! 2 (r 2)) (mseg! 3 (r 3))
(publish! 1 2 3)
(mseg! 2 (r 2) (r 3))
(let* ((ls (log-open d)) (p (load-prefix ls M)))
  (want "the prefix below the bad segment survives, and the segment is named"
        (list (discovery-end-seq p) (discovery-segment-ranges p)
              (map log-error-kind (discovery-integrity p)))
        (list 1 '((1 1 1)) '(manifest-hash)))
  (load-commit! ls))
(want "and nothing from the bad segment onward is delivered"
      (deliver-with '()) (list 'delivered '(1) 'open))

(printf "\n~a failures\n" bad)
(printf "log9 complete\n")
