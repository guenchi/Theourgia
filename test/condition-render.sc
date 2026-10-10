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

;; WHAT A RED ROW IS ALLOWED TO LEAVE OUT.
;;
;; This measures `condition-render.ss`, the renderer every fixture that
;; catches a raise is meant to use. The rule it exists to keep: a reading
;; may not drop the facts the failure carried.
;;
;; The readings below are compared against what `display-condition` actually
;; produces on THIS implementation rather than against sentences written
;; here by hand, except where the point IS the sentence -- a row that
;; restated the expected text would pass over a renderer that had stopped
;; substituting anything.

(import (chezscheme))
(include "condition-render.ss")

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (condition->text e)))) e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define (holds? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; The message-only reading, kept here so the rows can state the difference
;; as a comparison rather than as a claim. This is what every fixture did
;; before, and what the mutation in the delivery notes puts back.
(define (message-only e)
  (if (and (condition? e) (message-condition? e)) (condition-message e) e))

(define (raised proc)
  (guard (e (#t (list (condition->text e) (message-only e)))) (proc)))

(printf "\n== the irritants reach the reading ==\n")

;; A DIRECTIVE THIS IMPLEMENTATION DOES NOT SUBSTITUTE. This is the exact
;; shape that was found while building the round before this one, and the
;; reason the item was carried: the message names a variable it does not
;; contain.
(define unsub (raised (lambda () (assertion-violation 'probe "variable ~:s is not bound" 'zork))))

(want "CR-1 the message alone loses the one fact the failure carried"
      (list (cadr unsub) (holds? (cadr unsub) "zork"))
      (list "variable ~:s is not bound" #f))

(want "CR-2 and the rendered text keeps it"
      (holds? (car unsub) "zork")
      #t)

(want "CR-3 TWIN: the directive is still visible, so nothing pretends to have substituted it"
      (holds? (car unsub) "~:s")
      #t)

;; A DIRECTIVE IT DOES SUBSTITUTE. The two halves matter separately: the row
;; above says an unsubstituted directive does not cost the fact, this one
;; says an ordinary message is not left with a template in it.
(define subbed (raised (lambda () (car '()))))

(want "CR-4 an ordinary message is substituted, not left as a template"
      (list (holds? (car subbed) "~s") (holds? (car subbed) "()"))
      (list #f #t))

(want "CR-5 TWIN: and the message alone WAS left as a template"
      (list (cadr subbed) (holds? (cadr subbed) "~s"))
      (list "~s is not a pair" #t))

;; TWO IRRITANTS, because a renderer that took `(car (condition-irritants e))`
;; would pass every row above and lose the second one here.
(define two (raised (lambda () (vector-ref (vector 1 2) 9))))

(want "CR-6 both irritants reach the reading, not just the first"
      (list (holds? (car two) "9") (holds? (car two) "#(1 2)"))
      (list #t #t))

;; NOT A CONDITION AT ALL. `raise` takes any object, and a fixture that asked
;; `condition-message` of a symbol would raise a SECOND time while rendering
;; the first -- turning a red row into a dead file.
(define bare (raised (lambda () (raise 'a-bare-symbol))))

(want "CR-7 a raise that is not a condition still renders, and names the object"
      (holds? (car bare) "a-bare-symbol")
      #t)

(printf "\n== which part of the row broke ==\n")

;; THE THREE-PART ROW. Before this, all three of these answered the same
;; `RAISED`, and the reading could not tell them apart.
(define (three-part which)
  (list (part 'first  (if (eq? which 'first)  (car '()) 'ok-1))
        (part 'second (if (eq? which 'second) (car '()) 'ok-2))
        (part 'third  (if (eq? which 'third)  (car '()) 'ok-3))))

(want "CR-8 a three-part row says which part broke, and the other two still answer"
      (let ((r (three-part 'second)))
        (list (car r) (car (cadr r)) (cadr (cadr r)) (caddr r)))
      (list 'ok-1 'RAISED-IN 'second 'ok-3))

(want "CR-9 TWIN: the same failure in another part reads differently"
      (let ((r (three-part 'third)))
        (list (car r) (cadr r) (car (caddr r)) (cadr (caddr r))))
      (list 'ok-1 'ok-2 'RAISED-IN 'third))

;; THE ROW THAT SAYS THE TWO READINGS ARE NOT THE SAME. Completion condition
;; three from the round before: a renderer that goes back to the message
;; alone must make some reading unable to tell two failures apart. These two
;; failures differ ONLY in their irritants.
(define left  (raised (lambda () (assertion-violation 'probe "variable ~:s is not bound" 'alpha))))
(define right (raised (lambda () (assertion-violation 'probe "variable ~:s is not bound" 'omega))))

(want "CR-10 two failures that differ only in their irritants read differently"
      (equal? (car left) (car right))
      #f)

(want "CR-11 and under the message alone they are the same reading, which is the defect"
      (equal? (cadr left) (cadr right))
      #t)

(printf "rows: ~a\n~a failures\ncondition-render complete\n" rows bad)
(exit (if (zero? bad) 0 1))
