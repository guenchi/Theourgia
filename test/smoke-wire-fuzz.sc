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

;; Round trip and injectivity over generated data. Round trip alone
;; would have passed the defect this found -- (a . #\b) came back as a
;; plausible list -- so the second property is checked separately:
;; two data that encode alike must be alike.
(import (chezscheme) (theourgia wire))

(define seed 20260910)
(define (rnd n) (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
                (modulo (quotient seed 65536) n))

(define atoms
  (vector 'a 'b 'kind 'section "s" "" "#%char" "#%sym" "#%quote" "#%dot"
          "#%zzz" 0 1 -3 7/2 #t #f '()
          #\a #\newline #\x1F600 #\space
          (string->symbol "12") (string->symbol "weird sym")
          (string->symbol "#%char") (string->symbol "")))

(define (gen depth)
  (if (or (= depth 0) (< (rnd 10) 4))
      (vector-ref atoms (rnd (vector-length atoms)))
      (case (rnd 3)
        ((0) (let loop ((k (rnd 4)) (acc '()))
               (if (= k 0) acc (loop (- k 1) (cons (gen (- depth 1)) acc)))))
        ((1) (let loop ((k (+ 1 (rnd 3))) (acc (gen (- depth 1))))
               (if (= k 0) acc (loop (- k 1) (cons (gen (- depth 1)) acc)))))
        (else (let* ((k (rnd 3)) (v (make-vector k)))
                (do ((i 0 (+ i 1))) ((= i k) v)
                  (vector-set! v i (gen (- depth 1)))))))))

(define n 20000)
(define seen (make-hashtable equal-hash equal?))
(define rt-bad 0) (define inj-bad 0) (define write-bad 0)
(define improper 0)
(define (has-improper? x)
  (cond ((pair? x) (or (has-improper? (car x))
                       (let ((d (cdr x)))
                         (if (or (pair? d) (null? d)) (has-improper? d) #t))))
        ((vector? x) (let loop ((i 0)) (and (< i (vector-length x))
                                            (or (has-improper? (vector-ref x i))
                                                (loop (+ i 1))))))
        (else #f)))

(let loop ((i 0))
  (unless (= i n)
    (let* ((x (gen 4)) (e (storable-encode x)))
      (when (has-improper? x) (set! improper (+ improper 1)))
      (unless (equal? (storable-decode e) x)
        (set! rt-bad (+ rt-bad 1))
        (when (< rt-bad 4) (printf "ROUNDTRIP ~s -> ~s -> ~s\n" x e (storable-decode e))))
      (guard (exn (#t (set! write-bad (+ write-bad 1))
                      (when (< write-bad 4) (printf "UNWRITABLE ~s -> ~s\n" x e))))
        (sexpr->string-extended e))
      (let ((prev (hashtable-ref seen e 'none)))
        (if (eq? prev 'none)
            (hashtable-set! seen e x)
            (unless (equal? prev x)
              (set! inj-bad (+ inj-bad 1))
              (when (< inj-bad 4)
                (printf "COLLISION ~s and ~s both -> ~s\n" prev x e))))))
    (loop (+ i 1))))
(printf "~a data, ~a of them improper somewhere\n" n improper)
(printf "round-trip failures ~a\ninjectivity collisions ~a\nunwritable encodings ~a\n"
        rt-bad inj-bad write-bad)

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "smoke-wire-fuzz complete\n")
