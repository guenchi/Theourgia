#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; The guard on a row's expected value (expected.ss), read through rows of its
;; own.
;;
;; Each probe runs rows through this fixture's own want, with the output and
;; the counters set aside: a probe's deliberate FAIL lines are what is read,
;; and they are not this fixture's failures.
(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (with-expected label expected (x) (want-1 label (caught got) x)))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
;; Runs THUNK's rows with the output kept and the counters restored.
;; -> (output failures-added rows-added finished?)
(define (probe thunk)
  (let ((r0 rows) (b0 bad) (finished #f))
    (let ((text (with-output-to-string
                  (lambda () (guard (e (#t #f)) (thunk) (set! finished #t))))))
      (let ((result (list text (- bad b0) (- rows r0) finished)))
        (set! rows r0)
        (set! bad b0)
        result))))
(define (line-for text label kind)
  (contains? text (string-append kind " " label)))

(let ((p (probe (lambda () (want "p1" 1 (raise 'boom)) (want "p2" 1 1)))))
  (want "EG-1 an expected value that raises is a FAIL line, counted, and the row after it runs"
        (list (line-for (car p) "p1" "FAIL") (cadr p) (line-for (car p) "p2" "ok") (caddr p) (cadddr p))
        '(#t 1 #t 2 #t)))
(let ((p (probe (lambda () (want "p3" (raise 'boom) (raise 'boom))))))
  (want "EG-2 two rows that raise the same message do not compare equal: a FAIL line"
        (list (line-for (car p) "p3" "FAIL") (cadr p))
        '(#t 1)))
(let ((p (probe (lambda () (want "p4" (list 'expected-raised 'boom) (raise 'boom))))))
  (want "EG-3 a computed value spelled as the raise's report does not pass a raising expected value"
        (list (line-for (car p) "p4" "FAIL") (cadr p))
        '(#t 1)))
(let ((p (probe (lambda ()
                  (want "p5" (assertion-violation 'probe "the computed side") (assertion-violation 'probe "the expected side"))))))
  (want "EG-3 a raise's message is in the FAIL line"
        (list (line-for (car p) "p5" "FAIL") (contains? (car p) "the expected side"))
        '(#t #t)))
(let ((p (probe (lambda () (want "p6" (+ 1 2) 3) (want "p7" 4 5)))))
  (want "EG-4 CONTROL: expected values that return compare as before -- equal is ok, different is a FAIL line"
        (list (line-for (car p) "p6" "ok") (line-for (car p) "p7" "FAIL") (cadr p) (caddr p))
        '(#t #t 1 2)))

(printf "rows: ~a\n~a failures\nexpected-guard complete\n" rows bad)
(exit (if (= bad 0) 0 1))
