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

;; The transcript of a scan (log.sc, "the transcript of a scan"), held to
;; scan-segment itself.
;;
;; RS-1. Over every segment of the corpus (segment-corpus.ss), every
;; transcript of it (none decoded yet, stopped after each record, whole,
;; and one TAKEN under each expected sequence, recoverable tail and stop of
;; the grid below, so that what a take under one argument records is
;; replayed under every other), and every combination of writer and
;; segment, expected sequence (none,
;; right, wrong), recoverable tail, and a callback stopping at each record
;; in turn or never: replay-scan of the transcript makes the same callback
;; calls as scan-segment of the bytes, and answers the same outcome, the
;; error's every field included.
;;
;; RS-2. For every split of every segment into a prefix and the whole, and
;; every transcript of the prefix (those that ended in an error included):
;; the transcript extended to the whole, replayed, answers what
;; scan-segment answers for the whole, and once replayed to its end it IS
;; the whole's transcript. And HISTORIES on one transcript: for every
;; split A of every segment, A then A+B then the whole (B ending one byte
;; after A, which leaves a record cut inside a line still a residual, and
;; B ending half way to the end), with the transcript of A taken whole and
;; taken by two stopped replays (stopped at the first record, then grown
;; by a replay stopped at the second).
;;
;; RS-3. The work, read off the `parse` trace that decode-line emits once
;; per line it parses: a transcript taken by a replay stopped at record k
;; (the first is 1) parses k lines; an extension parses only the lines from
;; the offset its transcript covers.
;;
;; THE ERROR IS OPENED, NOT COMPARED BY equal?. A log-error is a record,
;; and Chez's equal? compares records by identity, so two errors built
;; apart with the same fields are not equal?. `opened` turns every record
;; into its type's name and the values of every field its type has (read
;; through the record type, so a field added later is compared without
;; this file naming it).

(import (chezscheme) (theourgia log) (theourgia wire) (theourgia crc32)
        (only (theourgia trace) trace-enable!))

(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

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

(include "segment-corpus.ss")

(define (opened x)
  (cond
    ((pair? x) (cons (opened (car x)) (opened (cdr x))))
    ((vector? x) (vector-map opened x))
    ((record? x)
     (let walk ((rtd (record-rtd x)) (fields '()))
       (if (not rtd)
           (cons* 'record (record-type-name (record-rtd x)) fields)
           (walk (record-type-parent rtd)
                 (append (let loop ((i 0) (acc '()))
                           (if (= i (vector-length (record-type-field-names rtd)))
                               (reverse acc)
                               (loop (+ i 1)
                                     (cons (list (vector-ref (record-type-field-names rtd) i)
                                                 (opened ((record-accessor rtd i) x)))
                                           acc))))
                         fields)))))
    (else x)))

;; A callback that records every call and answers stop on the call
;; numbered `stop-at` (0 is the first; #f never).
(define (recorder stop-at)
  (let ((calls '()) (i 0))
    (values
      (lambda args
        (set! calls (cons args calls))
        (let ((k i))
          (set! i (+ i 1))
          (if (eqv? k stop-at) 'stop 'go)))
      (lambda () (reverse calls)))))

(define (scan-reading bv writer segment expected recoverable stop-at)
  (let-values (((deliver calls) (recorder stop-at)))
    (let ((out (scan-segment bv writer segment expected recoverable deliver)))
      (list (opened out) (calls)))))

(define (replay-reading tr writer segment expected recoverable stop-at)
  (let-values (((deliver calls) (recorder stop-at)))
    (let-values (((out tr*) (replay-scan tr writer segment expected recoverable deliver)))
      (list (opened out) (calls)))))

(define (replayed tr stop-at)
  (let-values (((deliver calls) (recorder stop-at)))
    (let-values (((out tr*) (replay-scan tr "w" 1 #f #t deliver)))
      tr*)))

(define (whole tr) (replayed tr #f))

;; Every transcript of `bv`: none decoded, stopped after each record, whole.
(define (transcripts bv)
  (let* ((t0 (transcript-of bv))
         (n (length (transcript-frames (whole t0)))))
    (append (list t0)
            (map (lambda (k) (replayed t0 k)) (iota n))
            (list (whole t0)))))

(define (transcript-content tr)
  (list (transcript-frames tr) (transcript-covered tr) (transcript-ending tr)))

(define writers '(("k3m9x2qa" 7) ("mirrorz9" 1)))

(define (records-of bv) (length (transcript-frames (whole (transcript-of bv)))))
(define (first-seq bv)
  (let ((fs (transcript-frames (whole (transcript-of bv)))))
    (if (null? fs) 1 (caddr (car fs)))))

(define (stops bv) (cons #f (iota (records-of bv))))

;; TRANSCRIPTS TAKEN UNDER EVERY ARGUMENT, not only under the one set
;; `transcripts` uses: a load takes a transcript with the expected
;; sequence it has and, for a sealed segment, with no recoverable tail,
;; and what such a take records (a sealed residual, a first-record
;; sequence error that is the caller's) must replay right under every
;; other argument. One per expected sequence (none, right, wrong),
;; recoverable tail and stop of RS-1's grid, all under the first writer
;; and segment: a transcript records neither.
(define (taken-transcripts bv)
  (apply append
    (map (lambda (expected)
           (apply append
             (map (lambda (recoverable)
                    (map (lambda (stop-at)
                           (let-values (((deliver calls) (recorder stop-at)))
                             (let-values (((out tr) (replay-scan (transcript-of bv) (car (car writers))
                                                                 (cadr (car writers)) expected
                                                                 recoverable deliver)))
                               tr)))
                         (stops bv)))
                  '(#t #f))))
         (list #f (first-seq bv) (+ 7 (first-seq bv))))))

(printf "== CONTROL: the corpus is damaged as it says ==\n")
(define (scan-class bv)
  (let ((out (scan-segment bv "w" 1 #f #t (lambda args 'go))))
    (if (eq? (car out) 'integrity) (log-error-kind (cadr out)) (car out))))
(want "every segment's scan answers its class"
      (filter (lambda (x) x)
              (map (lambda (c) (and (not (eq? (scan-class (cadr c)) (caddr c)))
                                    (list (car c) (scan-class (cadr c)))))
                   segment-corpus))
      '())
(define (scan-reason bv)
  (let ((out (scan-segment bv "w" 1 #f #t (lambda args 'go))))
    (and (eq? (car out) 'integrity)
         (let ((r (assq 'reason (log-error-detail (cadr out))))) (and r (cdr r))))))
(define (name-has? name word)
  (let ((n (string-length word)))
    (let loop ((i 0))
      (and (<= (+ i n) (string-length name))
           (or (string=? (substring name i (+ i n)) word) (loop (+ i 1)))))))
(want "every frame reason the scanner reaches has a witness at the first, a middle and the last record, failing for that reason"
      (map (lambda (reason)
             (let ((cs (filter (lambda (c) (name-has? (car c) (string-append "frame-" (symbol->string reason) "-")))
                               segment-corpus)))
               (list reason (length cs) (for-all (lambda (c) (eq? (scan-reason (cadr c)) reason)) cs))))
           '(crc-field encoding padding parse shape))
      (map (lambda (reason) (list reason 3 #t)) '(crc-field encoding padding parse shape)))
(want "each class is in the corpus"
      (map (lambda (k) (and (memq k (map caddr segment-corpus)) #t))
           '(complete torn crc frame seq))
      '(#t #t #t #t #t))
(want "CONTROL: opened sees an error's detail"
      (equal? (opened (make-log-error 'torn-in-sealed "w" 1 0 '((bytes . 3))))
              (opened (make-log-error 'torn-in-sealed "w" 1 0 '())))
      #f)
(want "CONTROL: equal? alone does not, which is why the outcomes are opened"
      (equal? (make-log-error 'crc "w" 1 0 '()) (make-log-error 'crc "w" 1 0 '()))
      #f)

(printf "== RS-1: replay-scan answers what scan-segment answers ==\n")
(define rs1-compared 0)
(define (rs1-mismatches name bv)
  (let ((misses '()))
    (for-each
      (lambda (tr)
        (for-each
          (lambda (ws)
            (for-each
              (lambda (expected)
                (for-each
                  (lambda (recoverable)
                    (for-each
                      (lambda (stop-at)
                        (set! rs1-compared (+ rs1-compared 1))
                        (let ((s (scan-reading bv (car ws) (cadr ws) expected recoverable stop-at))
                              (r (replay-reading tr (car ws) (cadr ws) expected recoverable stop-at)))
                          (unless (equal? s r)
                            (set! misses (cons (list 'covered (transcript-covered tr)
                                                  'expected expected 'recoverable recoverable
                                                  'stop stop-at 'scan s 'replay r)
                                            misses)))))
                      (stops bv)))
                  '(#t #f)))
              (list #f (first-seq bv) (+ 7 (first-seq bv)))))
          writers))
      (append (transcripts bv) (taken-transcripts bv)))
    (let ((n (length misses)))
      (if (zero? n) '() (list n (car (reverse misses)))))))

(for-each
  (lambda (c)
    (want (format "RS-1 ~a" (car c)) (rs1-mismatches (car c) (cadr c)) '()))
  segment-corpus)
(printf "RS-1 compared ~a combinations over ~a segments\n" rs1-compared (length segment-corpus))
;; The fewest a segment can give is 24: two transcripts (none decoded and
;; whole), two writers, three expected sequences, two tails, one stop.
(want "RS-1 compared at least the fewest combinations for every segment"
      (>= rs1-compared (* 24 (length segment-corpus))) #t)

(printf "== RS-2: a transcript extends by resumption ==\n")
;; EVERY SPLIT: each byte count from none to the whole.
(define (splits bv) (iota (+ (bytevector-length bv) 1)))

;; THE PARSE TRACE, counted: decode-line emits one `(trace parse ...)` line
;; per line it parses (wire.sc), on the error port while tracing is on.
(define (parses thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (let* ((text (get-output-string p)) (needle "(trace parse") (k (string-length needle)))
      (let loop ((i 0) (n 0))
        (cond ((> (+ i k) (string-length text)) n)
              ((string=? (substring text i (+ i k)) needle) (loop (+ i k) (+ n 1)))
              (else (loop (+ i 1) n)))))))

(define (scan-parses bv) (parses (lambda () (scan-segment bv "w" 1 #f #t (lambda args 'go)))))

(define rs2-extended 0)
(define rs2-errored 0)
(define rs3-extension-misses '())
(define (rs2-mismatches name bv)
  (let ((target (transcript-content (whole (transcript-of bv))))
        (scan-a (scan-reading bv "k3m9x2qa" 7 #f #f #f))
        (scan-b (scan-reading bv "mirrorz9" 1 (first-seq bv) #t 1))
        (all-parses (scan-parses bv))
        (misses '()))
    (for-each
      (lambda (p)
        (for-each
          (lambda (tr)
            (let ((ext (transcript-extend tr bv))
                  (errored? (let ((e (transcript-ending tr))) (and e (eq? (car e) 'error)))))
              (if (not ext)
                  (set! misses (cons (list 'split p 'covered (transcript-covered tr) 'not-extended) misses))
                  (let* ((a (replay-reading ext "k3m9x2qa" 7 #f #f #f))
                         (b (replay-reading ext "mirrorz9" 1 (first-seq bv) #t 1))
                         (t #f)
                         (n (parses (lambda () (set! t (transcript-content (whole ext))))))
                         (want-n (- all-parses (length (transcript-frames tr)))))
                    (set! rs2-extended (+ rs2-extended 1))
                    (when errored? (set! rs2-errored (+ rs2-errored 1)))
                    (unless (= n want-n)
                      (set! rs3-extension-misses
                            (cons (list name 'split p 'covered (transcript-covered tr) 'parsed n 'wanted want-n)
                                  rs3-extension-misses)))
                    (unless (and (equal? a scan-a) (equal? b scan-b) (equal? t target))
                      (set! misses (cons (list 'split p 'covered (transcript-covered tr)
                                               'replay-a (equal? a scan-a) 'replay-b (equal? b scan-b)
                                               'transcript (equal? t target))
                                         misses)))))))
          (transcripts (corpus-take bv p))))
      (splits bv))
    (let ((n (length misses)))
      (if (zero? n) '() (list n (car (reverse misses)))))))

(for-each
  (lambda (c)
    (want (format "RS-2 ~a" (car c)) (rs2-mismatches (car c) (cadr c)) '()))
  segment-corpus)
(printf "RS-2 extended ~a transcripts, ~a of them ended in an error\n" rs2-extended rs2-errored)
(want "RS-2 extended transcripts, errored ones among them" (list (> rs2-extended 0) (> rs2-errored 0)) '(#t #t))
(want "RS-2 CONTROL: bytes that do not begin with the transcript's are refused"
      (let ((tr (whole (transcript-of corpus-fx3))))
        (list (transcript-extend tr (corpus-flip corpus-fx3 40))
              (transcript-extend tr (corpus-take corpus-fx3 10))
              (and (transcript-extend tr corpus-fx3) #t)))
      '(#f #f #t))
(want "RS-2 CONTROL: a replay leaves the transcript it was given as it was"
      (let* ((t0 (transcript-of corpus-fx3)) (before (transcript-content t0)))
        (whole t0)
        (equal? before (transcript-content t0)))
      #t)

(printf "== RS-2: histories on one transcript ==\n")
;; A, then A+B, then the whole. The transcript of A is taken whole, and
;; also by two stopped replays (the first stopped at the first record, the
;; second, on what the first answered, at the second), so the transcript
;; that is extended was grown by a replay. B ends one byte after A (a
;; record cut inside its line is still a residual after that append) and
;; half way to the end. The transcript of A+B is replayed whole before the
;; second extension.
(define rs2-histories 0)
(define (history-mismatches bv)
  (let* ((n (bytevector-length bv))
         (target (transcript-content (whole (transcript-of bv))))
         (scan-a (scan-reading bv "k3m9x2qa" 7 #f #f #f))
         (scan-b (scan-reading bv "mirrorz9" 1 (first-seq bv) #t 1))
         (misses '()))
    (for-each
      (lambda (p1)
        (let ((a (corpus-take bv p1)))
          (for-each
            (lambda (ta)
              (for-each
                (lambda (p2)
                  (set! rs2-histories (+ rs2-histories 1))
                  (let* ((t2 (transcript-extend ta (corpus-take bv p2)))
                         (t3 (and t2 (transcript-extend (whole t2) bv))))
                    (if (not t3)
                        (set! misses (cons (list 'split p1 p2 'not-extended) misses))
                        (let ((ra (replay-reading t3 "k3m9x2qa" 7 #f #f #f))
                              (rb (replay-reading t3 "mirrorz9" 1 (first-seq bv) #t 1))
                              (t (transcript-content (whole t3))))
                          (unless (and (equal? ra scan-a) (equal? rb scan-b) (equal? t target))
                            (set! misses (cons (list 'split p1 p2 'covered (transcript-covered ta)
                                                     'replay-a (equal? ra scan-a) 'replay-b (equal? rb scan-b)
                                                     'transcript (equal? t target))
                                               misses)))))))
                (let ((one (+ p1 1)) (half (+ p1 (div (- n p1) 2))))
                  (filter (lambda (p2) (and (> p2 p1) (<= p2 n)))
                          (if (= one half) (list one) (list one half))))))
            (list (whole (transcript-of a))
                  (replayed (replayed (transcript-of a) 0) 1)))))
      (splits bv))
    (let ((k (length misses)))
      (if (zero? k) '() (list k (car (reverse misses)))))))

(for-each
  (lambda (c)
    (want (format "RS-2 history ~a" (car c)) (history-mismatches (cadr c)) '()))
  segment-corpus)
(printf "RS-2 ran ~a histories\n" rs2-histories)
(want "RS-2 ran histories" (> rs2-histories 0) #t)

(printf "== RS-3: the work, by the parse trace ==\n")
(want "RS-3 CONTROL: a whole scan of three records parses three lines"
      (scan-parses corpus-fx3)
      3)
(want "RS-3 a transcript taken by a replay stopped at record k parses k lines, and one never stopped parses what scan-segment parses"
      (filter (lambda (x) x)
              (map (lambda (c)
                     (let* ((bv (cadr c))
                            (got (map (lambda (k) (parses (lambda () (replayed (transcript-of bv) k))))
                                      (iota (records-of bv))))
                            (whole-n (parses (lambda () (whole (transcript-of bv))))))
                       (and (not (and (equal? got (map (lambda (k) (+ k 1)) (iota (records-of bv))))
                                      (= whole-n (scan-parses bv))))
                            (list (car c) got whole-n (scan-parses bv)))))
                   segment-corpus))
      '())
(want "RS-3 an extension parses only the lines from the offset its transcript covers (every RS-2 extension)"
      (let ((k (length rs3-extension-misses)))
        (if (zero? k) '() (list k (car (reverse rs3-extension-misses)))))
      '())

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "replay-scan complete\n")
(exit (if (zero? bad) 0 1))
