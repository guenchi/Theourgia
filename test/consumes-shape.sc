#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;; A PLAN'S CONSUMES LIST: ONE ITEM PER BLOCK, CHECKED IN TIME LINEAR IN IT.
;;
;; consumes? (request.sc) refuses a consumes list that names one block twice.
;; It found a repeat by searching the blocks already seen, a list, for every
;; item: O(c^2) for c items, paid each time a judgement asks plan? of the
;; plan (a commit of a large draft carries one item per block). The blocks
;; seen are a table now. The answers do not change, and that is what the
;; rows pin (CS-1, CS-2): repeats adjacent, far apart and at the end are
;; refused, distinct blocks are taken whatever their versions, and every
;; malformed shape is refused as before. A plan of 3000 consumes items is
;; applied through the reduction and consumes what it names (CS-3).
;;
;; THE COST IS A READING, NOT A ROW: the time consumes? takes on 2000, 4000
;; and 8000 items, and the time CS-3's records take to apply, are printed.
;; On the list the time grows about four times per doubling; on the table
;; about two.

(import (chezscheme) (theourgia request) (theourgia reduce))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)))) e))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define (item block version) (list block version "h0" '()))
(define (consumes-of items) (list 'consumes "dw" items))
(define (blocks n) (map (lambda (k) (format "b~a" k)) (iota n)))
(define (items-of names) (map (lambda (b) (item b (string-append "v" b))) names))

(want "CS-1 every shape answers as before: distinct blocks taken, a repeat refused wherever it is, malformed items and heads refused"
      (map consumes?
           (list (consumes-of (items-of '("b1" "b2" "b3")))
                 (consumes-of '())
                 (consumes-of (items-of '("b1" "b1")))
                 (consumes-of (items-of '("b1" "b2" "b3" "b1")))
                 (consumes-of (items-of '("b2" "b3" "b3")))
                 (consumes-of (list (item "b1" "v") (item "b2" "v")))
                 (consumes-of (list (item "b1" "v1") (item "b1" "v2")))
                 (consumes-of (list (list "b1" "v" "h0")))
                 (consumes-of (list (item 'b1 "v")))
                 (list 'consumed "dw" (items-of '("b1")))
                 (list 'consumes "dw")))
      '(#t #t #f #f #f #t #f #f #f #f #f))

(want "CS-2 at a thousand items: distinct taken; the last naming the first, and two in the middle naming one block, refused"
      (let ((names (blocks 1000)))
        (list (consumes? (consumes-of (items-of names)))
              (consumes? (consumes-of (items-of (append names (list (car names))))))
              (consumes? (consumes-of (items-of (append (list-head names 500) (list "b250") (list-tail names 500)))))
              (plan? (list 'plan "p" "fp" (cons "o" 0) (list (cons 0 '(insert root #f ((kind . section)))))
                           (consumes-of (items-of (append names (list "b999"))))))))
      '(#t #f #f #f))

;; A PLAN OF 3000 CONSUMES ITEMS AND TWO ENTRIES, its members applied: no
;; gate, and its first and last items consumed.
(define origin "cs000000")
(define after (cons origin 0))
(define key (cons origin "c"))
(define (intent title) (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title title))))
(define (produced title)
  (list 'put (list (cons 'kind 'section) (cons 'title title) '(parent . root) '(ord . 1))))
(define many (blocks 3000))
(define (apply-plan!)
  (let ((r (reduce-empty)))
    (reduce-apply! r origin 1 '()
                   (list 'plan "c" "fp" after (list (cons 0 (intent "x")) (cons 1 (intent "y")))
                         (consumes-of (items-of many)))
                   (list "test" key 'plan "fp" #f after))
    (reduce-apply! r origin 2 '() (produced "x") (list "test" key 0 "fp" (cons origin 1) after))
    (reduce-apply! r origin 3 '() (produced "y") (list "test" key 1 "fp" (cons origin 1) after))
    r))
(define t0 (cpu-time))
(define applied (apply-plan!))
(define apply-ms (- (cpu-time) t0))
(want "CS-3 a plan of 3000 consumes items applies its members without a gate and consumes its first and last items"
      (list (reduce-gates applied) (length (state-datum applied))
            (state-consumed? applied "dw" "vb0") (state-consumed? applied "dw" "vb2999"))
      '(() 2 #t #t))

(define (ms-for n)
  (let ((c (consumes-of (items-of (blocks n)))) (t (cpu-time)))
    (consumes? c)
    (- (cpu-time) t)))
(printf "\nreading: consumes? cpu ms at 2000 / 4000 / 8000 items: ~a / ~a / ~a\n" (ms-for 2000) (ms-for 4000) (ms-for 8000))
(printf "reading: CS-3's plan and two members applied in ~a cpu ms\n" apply-ms)

(printf "\n~a failures\nrows: ~a\nconsumes-shape complete\n" bad rows)
(exit (if (= bad 0) 0 1))
