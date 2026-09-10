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

;; The rewritten reader path: one validated-prefix discovery.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace) (theourgia crc32)
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
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d (test-dir "log6work"))
(define A "k3m9x2qa") (define B "c9xq01mz")
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
(define (hash-of path)
  (bytevector->hex (sha256 (call-with-port (open-file-input-port path) get-bytevector-all))))
(define (build!)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" A " " d "/writers/" B " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (put! (string-append d "/writers/" A "/000001.sexp")
        (cat (rec 1 '() '(put "a.1" ())) (rec 2 '() '(put "a.2" ()))))
  (put! (string-append d "/writers/" B "/000001.sexp") (rec 1 '() '(put "b.1" ())))
  (put! (string-append d "/writers/" B "/published.sexp")
        (string->utf8 (string-append "((1 . \"" (hash-of (string-append d "/writers/" B "/000001.sexp")) "\"))\n"))))

(printf "== one discovery answers the question ==\n")
(build!)
(define ls (log-open d))
(define pa (load-prefix ls A))
(define pb (load-prefix ls B))
(want "A is local, B is mirrored"
      (list (discovery-origin pa) (discovery-origin pb)) '(local mirrored))
(want "A's validated extent ends at seq 2" (discovery-end-seq pa) 2)
(want "B's at 1" (discovery-end-seq pb) 1)
(want "no integrity anywhere" (load-integrity ls) '())
(want "segment ranges are recorded" (discovery-segment-ranges pa) '((1 1 2)))

(printf "== L10: the extent and the append target are different facts ==\n")
(put! (string-append d "/writers/" A "/000002.sexp") (make-bytevector 0))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the extent still ends at 2 despite an empty rotated segment"
        (discovery-end-seq p) 2)
  (want "and the append target is the new empty segment"
        (car (discovery-physical-current p)) 2)
  (load-commit! ls))

(printf "== physical-current says 'no append target' where there is none ==\n")
(let* ((ls (log-open d)) (p (load-prefix ls B)))
  (want "a mirrored writer has no append target"
        (discovery-physical-current p) 'no-append-target)
  (load-commit! ls))

(printf "== an interrupted first publication is ignored entirely ==\n")
(build!)
(system (string-append "mkdir -p " d "/writers/zzzzzzzz"))
(put! (string-append d "/writers/zzzzzzzz/000001.sexp") (rec 1 '() '(put "z.1" ())))
(let* ((ls (log-open d)) (p (load-prefix ls "zzzzzzzz")))
  (want "classified as an interrupted publication"
        (discovery-origin p) 'incomplete-publication)
  (want "and it contributes no extent" (discovery-end-seq p) 0)
  (load-commit! ls))

(printf "== a malformed retirement marker is an integrity error, not 'never retired' ==\n")
(build!)
(put! (string-append d "/writers/" A "/retired.sexp") (string->utf8 "(oops"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the marker is reported"
        (map log-error-kind (discovery-integrity p)) '(retired-malformed))
  (want "and no history is exposed past it" (discovery-end-seq p) 0)
  (load-commit! ls))

(printf "== a retirement offset past the end of its file is reported ==\n")
(build!)
(put! (string-append d "/writers/" A "/retired.sexp")
      (string->utf8 "((format 1) (tx \"t1\") (prefix 1 999999 100))\n"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "reported rather than silently clipped"
        (map log-error-kind (discovery-integrity p)) '(retired-beyond-file))
  (load-commit! ls))

(printf "== a swapped published segment is caught by the hash ==\n")
(build!)
(put! (string-append d "/writers/" B "/000001.sexp") (rec 1 '() '(put "SWAPPED" ())))
(let* ((ls (log-open d)) (p (load-prefix ls B)))
  (want "manifest-hash, and nothing of B is in the extent"
        (list (map log-error-kind (discovery-integrity p)) (discovery-end-seq p))
        '((manifest-hash) 0))
  (want "CONTROL: A is untouched by B's damage"
        (discovery-end-seq (load-prefix ls A)) 2)
  (load-commit! ls))

(printf "== the quarantined suffix leaves the extent, not just snapshot eligibility ==\n")
(build!)
(put! (string-append d "/writers/" A "/quarantine.sexp")
      (string->utf8 "((format 1) (fork 2))\n"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the extent stops before the fork" (discovery-end-seq p) 1)
  (want "and the fork is reported" (cadr (discovery-quarantine p)) 2)
  (load-commit! ls))

(printf "== the load holds one shared lock and releases it on commit ==\n")
(build!)
(let ((p (open-output-string)))
  (parameterize ((current-error-port p))
    (trace-enable! #t)
    (let ((ls (log-open d))) (load-commit! ls))
    (trace-enable! #f))
  (let ((s (get-output-string p)))
    (define (count-of needle)
      (let ((n (string-length s)) (m (string-length needle)))
        (let loop ((i 0) (k 0))
          (cond ((> (+ i m) n) k)
                ((string=? (substring s i (+ i m)) needle) (loop (+ i 1) (+ k 1)))
                (else (loop (+ i 1) k))))))
    (want "exactly one shared acquisition for the whole load" (count-of "(trace flock") 1)
    (want "and exactly one release" (count-of "(trace unlock") 1)))

(printf "== delivery: from the cut's segment, bounded by the extent ==\n")
(build!)
(put! (string-append d "/writers/" A "/000002.sexp") (cat (rec 3 '() '(put "a.3" ()))))
(define (deliver-with cut)
  (let* ((ls (log-open d)) (seen '()))
    (let ((r (load-deliver! ls cut
               (lambda (w seg off seq ts actor deps payload)
                 (set! seen (cons (list w seq) seen))))))
      (when (eq? r 'delivered) (load-commit! ls))
      (list r (reverse seen)))))
(want "with no cut, everything in the extent arrives"
      (deliver-with '())
      (list 'delivered (list (list B 1) (list A 1) (list A 2) (list A 3))))
(want "a cut at A.2 suppresses only what it covers"
      (deliver-with (list (cons A 2)))
      (list 'delivered (list (list B 1) (list A 3))))
(want "a cut at A.3 leaves A with nothing to deliver"
      (deliver-with (list (cons A 3)))
      (list 'delivered (list (list B 1))))

(printf "== the extent bounds delivery, not a scan-until-failure ==\n")
(build!)
(put! (string-append d "/writers/" A "/quarantine.sexp")
      (string->utf8 "((format 1) (fork 2))\n"))
(want "the quarantined suffix is not delivered even though its bytes are fine"
      (deliver-with '()) (list 'delivered (list (list B 1) (list A 1))))

(printf "== a delivery read failure aborts the WHOLE load ==\n")
;; The failure must be LATE: failing the first open would let an
;; ordinary streaming implementation pass.
;; THE FAILING SEGMENT MUST BE A SEALED ONE. The first attempt made the
;; HIGHEST segment unreadable, and delivery never noticed: that segment
;; is the retained current buffer, so it is not re-read at all. Correct
;; behaviour, wrong fixture -- and it would have read as "the abort path
;; does not work".
(build!)
(put! (string-append d "/writers/" A "/000002.sexp") (cat (rec 3 '() '(put "a.3" ()))))
(put! (string-append d "/writers/" A "/000003.sexp") (cat (rec 4 '() '(put "a.4" ()))))
(let* ((ls (log-open d)) (seen '()))
  ;; unreadable only after discovery has run, and sealed rather than current
  (system (string-append "chmod 000 " d "/writers/" A "/000002.sexp"))
  (let ((r (load-deliver! ls '()
             (lambda (w seg off seq ts actor deps payload)
               (set! seen (cons (list w seq) seen))))))
    (system (string-append "chmod 644 " d "/writers/" A "/000002.sexp"))
    (want "the load reports delivery-failed for that writer"
          r (list 'delivery-failed A))
    (want "records WERE provisionally delivered before the failure"
          (> (length seen) 0) #t)
    (want "and the load is aborted, not committed"
          (let ((o (load-outcome ls))) (if (pair? o) (car o) o)) 'aborted)))


(printf "== L4: a reader changes nothing ==\n")
(build!)
(define (listing)
  (let loop ((dirs (list d)) (acc '()))
    (if (null? dirs) (list-sort string<? acc)
        (let* ((cur (car dirs)) (es (directory-list cur)))
          (loop (append (cdr dirs)
                        (map (lambda (e) (string-append cur "/" e))
                             (filter (lambda (e) (file-directory? (string-append cur "/" e))) es)))
                (append acc (map (lambda (e)
                                   (let ((p (string-append cur "/" e)))
                                     (if (file-directory? p) (string-append p "/")
                                         (string-append p ":"
                                           (crc32-hex (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
                                                        (if (eof-object? b) (make-bytevector 0) b)))))))
                                 es)))))))
(define before (listing))
(let ((ls (log-open d)))
  (load-deliver! ls '() (lambda args (if #f #f)))
  (load-commit! ls))
(want "the recursive listing and every hash are identical"
      (if (equal? (listing) before) (list 'identical (length before)) (listing))
      (list 'identical (length before)))

(printf "== L4'(a): an unlisted segment file is ignored ==\n")
(build!)
(put! (string-append d "/writers/" B "/000002.sexp") (rec 2 '() '(put "ghost" ())))
(want "the ghost is not in B's extent"
      (discovery-end-seq (load-prefix (log-open d) B)) 1)
(want "CONTROL: listing it makes it count"
      (begin (put! (string-append d "/writers/" B "/published.sexp")
                   (string->utf8 (string-append "((1 . \"" (hash-of (string-append d "/writers/" B "/000001.sexp"))
                                                "\") (2 . \"" (hash-of (string-append d "/writers/" B "/000002.sexp")) "\"))\n")))
             (discovery-end-seq (load-prefix (log-open d) B)))
      2)

(printf "== L1 through the reader: a torn tail on the local current segment ==\n")
(build!)
(let* ((p (string-append d "/writers/" A "/000001.sexp"))
       (whole (call-with-port (open-file-input-port p) get-bytevector-all))
       (cut (make-bytevector (- (bytevector-length whole) 9))))
  (bytevector-copy! whole 0 cut 0 (bytevector-length cut))
  (put! p cut))
(let* ((ls (log-open d)) (pa (load-prefix ls A)))
  (want "the first record survives and the residual is a torn tail"
        (list (discovery-end-seq pa) (car (discovery-torn pa))) '(1 1))
  (want "and it is not an integrity error" (discovery-integrity pa) '())
  (load-commit! ls))


(printf "== L7: snapshot selection inside the load ==\n")
(define (snap! n cut rows) (snapshot-write! (string-append d "/snap/" (segment-file-name n)) cut rows))
(define (adopted)
  (let ((ls (log-open d)))
    (let ((r (list (load-snapshot-cut ls) (load-snapshot-rows ls) (load-snapshot-reason ls))))
      (load-commit! ls) r)))
(build!)
(snap! 1 (list (cons A 2)) '((block "x" ())))
(want "a supported snapshot is adopted" (adopted) (list (list (cons A 2)) '((block "x" ())) #f))
;; THE REJECTED SNAPSHOT MUST BE THE ONLY ONE, or the row cannot tell
;; "rejected" from "fell back to the older one" -- both leave a valid
;; adoption behind. The fallback gets its own row below.
(want "a cut beyond the log, as the only snapshot, is refused with its reason"
      (begin (system (string-append "rm -f " d "/snap/000001.sexp"))
             (snap! 2 (list (cons A 99)) '((block "ghost" ())))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'unsupported-cut))
(want "CONTROL: with a supported older one present, it falls back to that"
      (begin (snap! 1 (list (cons A 2)) '((block "x" ()))) (car (adopted)))
      (list (cons A 2)))
(want "a void frame falls back to the older valid one"
      (begin (put! (string-append d "/snap/000003.sexp")
                   (string->utf8 "(snapshot 1 ((\"k3m9x2qa\" . 2)))\n"))
             (car (adopted)))
      (list (cons A 2)))
(want "CONTROL: the void one alone leaves nothing adopted"
      (begin (system (string-append "rm -f " d "/snap/000001.sexp " d "/snap/000002.sexp"))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'no-end))
(want "a cut naming one writer twice is malformed, not first-entry-wins"
      (begin (system (string-append "rm -f " d "/snap/000003.sexp"))
             (snap! 4 (list (cons A 1) (cons A 2)) '((block "d" ())))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'duplicate-writer))
(want "a cut reaching into quarantined history is refused"
      (begin (system (string-append "rm -f " d "/snap/000004.sexp"))
             (system (string-append "rm -f " d "/snap/000003.sexp " d "/snap/000004.sexp"))
             (snap! 5 (list (cons A 2)) '((block "q" ())))
             (put! (string-append d "/writers/" A "/quarantine.sexp")
                   (string->utf8 "((format 1) (fork 2))\n"))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'quarantined))

(printf "== the adopted cut is what delivery honours ==\n")
(build!)
(snap! 1 (list (cons A 1)) '((block "x" ())))
(let* ((ls (log-open d)) (seen '()))
  (load-deliver! ls (load-snapshot-cut ls)
    (lambda (w seg off seq ts actor deps payload) (set! seen (cons (list w seq) seen))))
  (load-commit! ls)
  (want "A.1 is covered by the snapshot and not re-delivered"
        (reverse seen) (list (list B 1) (list A 2))))

(printf "\n~a failures\n" bad)
(printf "log6 complete\n")
