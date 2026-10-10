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

;; Piece 2: scan-segment, against L1 (recoverable torn tail), L1' (page
;; model), L2 (CRC and frame) and L3 (sequence continuity with an exact
;; injection point).
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia crc32))
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


(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (take bv k) (let ((o (make-bytevector k))) (bytevector-copy! bv 0 o 0 k) o))

;; fx3: three records, the last carrying multibyte text so a truncation
;; can land inside a UTF-8 sequence (plan section 0).
(define r1 (rec 1 '(put "k3m9x2qa.1" ((title . "a")))))
(define r2 (rec 2 '(put "k3m9x2qa.2" ((title . "b")))))
(define r3 (rec 3 (list 'set "k3m9x2qa.1" 'title "a2\x00b7;\x65e5;\x672c;\x8a9e;\x1f600;")))
(define fx3 (cat r1 r2 r3))
(define o1 0)
(define o2 (bytevector-length r1))
(define o3 (+ o2 (bytevector-length r2)))

(define (scan bv recoverable?)
  (let ((seen '()))
    (let ((out (scan-segment bv "k3m9x2qa" 7 1 recoverable?
                             (lambda (off seq ts actor deps payload)
                               (set! seen (cons (list off seq) seen))))))
      (list out (reverse seen)))))

(printf "== a whole segment ==\n")
(want "three records at their offsets"
      (cadr (scan fx3 #t)) (list (list o1 1) (list o2 2) (list o3 3)))
(want "outcome complete, ending at the file end"
      (car (scan fx3 #t)) (list 'complete 3 (bytevector-length fx3)))

(printf "== L1: every truncation of the last line ==\n")
;; k = 0 is a clean prefix; 0 < k < L is a residual. Both are recoverable
;; on the local writer's current segment and neither loses record 2.
(define L (bytevector-length r3))
(define l1-results
  (map (lambda (k)
         (let* ((bv (take fx3 (+ o3 k))) (r (scan bv #t)))
           (list k (car (car r)) (length (cadr r)))))
       (list 0 1 8 9 20 (- L 12) (- L 2) (- L 1))))
(want "k=0 is a complete prefix of two records"
      (car l1-results) (list 0 'complete 2))
(want "every partial k is a torn tail, and two records still arrive"
      (map cdr (cdr l1-results))
      (map (lambda (_) '(torn 2)) (cdr l1-results)))
(want "the torn offset is where the residual starts"
      (cadr (car (scan (take fx3 (+ o3 5)) #t))) o3)
(want "CONTROL: the whole thing yields three, so the loss above is the truncation"
      (length (cadr (scan fx3 #t))) 3)

(printf "== a torn tail in a SEALED segment is an integrity error ==\n")
(define (integrity-shape* r)
  (if (eq? (car r) 'integrity)
      (list (car r) (log-error-kind (cadr r)) (log-error-offset (cadr r)))
      (list 'not-an-integrity-outcome (car r))))
(want "a sealed segment's residual is integrity, with kind and offset"
      (integrity-shape* (car (scan (take fx3 (+ o3 5)) #f)))
      (list 'integrity 'torn-in-sealed o3))
(want "CONTROL: the same bytes are recoverable when it IS the current segment"
      (car (car (scan (take fx3 (+ o3 5)) #t))) 'torn)

(printf "== L2: CRC and frame ==\n")
(define (flip bv i) (let ((c (bytevector-copy bv)))
                      (bytevector-u8-set! c i (if (= 65 (bytevector-u8-ref c i)) 66 65)) c))
(define (integrity-shape r)
  (if (eq? (car r) 'integrity)
      (list (car r) (log-error-kind (cadr r)) (log-error-offset (cadr r)))
      (list 'not-an-integrity-outcome (car r))))
(want "a flipped payload byte in record 2 is a CRC error at record 2's offset"
      (integrity-shape (car (scan (flip fx3 (+ o2 30)) #t))) (list 'integrity 'crc o2))
(want "a flipped CRC digit is also crc, at the same offset"
      (integrity-shape (car (scan (flip fx3 (+ o2 1)) #t))) (list 'integrity 'crc o2))
(want "records BEFORE the damage were still delivered"
      (cadr (scan (flip fx3 (+ o2 30)) #t)) (list (list o1 1)))

(printf "== L3: sequence continuity, reported at the injection point ==\n")
;; DOES NOT DEREFERENCE THE RESULT UNDER TEST. Reaching into (cadr r)
;; for a log-error field only makes sense once (car r) says integrity;
;; doing it unconditionally turns a wrong outcome into a CRASH rather
;; than a clean red, and a crashed run is the failure a harness reports
;; worst. Measured: with the sequence check disabled the outcome is
;; (complete 4 ...), and the first version died on (log-error-kind 4)
;; before printing a single FAIL.
(define (seqcase label rs expect-off expect-exp expect-act)
  (let* ((bv (apply cat rs)) (r (car (scan bv #t))))
    (want label
          (if (eq? (car r) 'integrity)
              (list (car r) (log-error-kind (cadr r)) (log-error-offset (cadr r))
                    (cdr (assq 'expected (log-error-detail (cadr r))))
                    (cdr (assq 'actual (log-error-detail (cadr r)))))
              (list 'not-an-integrity-outcome (car r)))
          (list 'integrity 'seq expect-off expect-exp expect-act))))
(define s1 (rec 1 '(put "a" ())))
(define s2 (rec 2 '(put "b" ())))
(define s4 (rec 4 '(put "d" ())))
(define off-third (+ (bytevector-length s1) (bytevector-length s2)))
(seqcase "a gap 1,2,4 reports expected 3 actual 4 at the third record"
         (list s1 s2 s4) off-third 3 4)
(seqcase "a repeat 1,2,2 reports expected 3 actual 2"
         (list s1 s2 s2) off-third 3 2)
(seqcase "a reversal 1,2,1 reports expected 3 actual 1"
         (list s1 s2 s1) off-third 3 1)
(want "CONTROL: 1,2,3 is clean" (car (car (scan (cat s1 s2 (rec 3 '(put "c" ()))) #t))) 'complete)

(printf "== L1': the page model ==\n")
;; A record spanning four 4096-byte pages, each page's content distinct,
;; followed by a valid record.
(define big (rec 1 (list 'put "k3m9x2qa.1" (list (cons 'body (make-string 12500 #\x))))))
(define after (rec 2 '(put "k3m9x2qa.2" ())))
(define pages (cat big after))
(want "the big record really spans four pages"
      (> (bytevector-length big) (* 3 4096)) #t)
(define (zero-page bv p)
  (let ((c (bytevector-copy bv)))
    (do ((i (* p 4096) (+ i 1))) ((or (>= i (* (+ p 1) 4096)) (>= i (bytevector-length c))) c)
      (bytevector-u8-set! c i 0))))
(let* ((hurt (zero-page pages 1)) (r (car (scan hurt #t))))
  (want "CONTROL: the injection really changed the bytes" (equal? hurt pages) #f)
  (want "a zeroed second page is an integrity error at the record's start"
        (if (eq? (car r) 'integrity)
            (list (car r) (log-error-offset (cadr r)))
            (list 'not-an-integrity-outcome (car r)))
        (list 'integrity 0)))
(want "CONTROL: undamaged, both records arrive"
      (length (cadr (scan pages #t))) 2)

(printf "\n~a failures\n" bad)

;; A RUN THAT DID NOT REACH HERE IS NOT A PASS. The mutation harness
;; greps for this line; without it, a crashed run and a clean run are
;; indistinguishable in its output.
(printf "rows: ~a\n" rows-run)
(printf "log2 complete\n")
