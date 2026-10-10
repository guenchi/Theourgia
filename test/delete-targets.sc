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

;; A DELETE JUDGES THE FAR ENDS OF ITS BLOCK'S EDGES.
;;
;; A delete unlinks every edge of its block as far as a reader of edges can
;; tell, so the live block at the other end of each edge, in either
;; direction, is a target of the write, as it is when the edge is unlinked.
;; A review whose only cited result is deleted is judged in that write and
;; refused by the rule it would break; a block no live block has an edge to
;; is deleted as before; check's audit is what it was; a store with no rule
;; in force makes no copy of its state and deletes as before.

(import (chezscheme) (theourgia rpc)
        (only (theourgia reduce) block-id)
        (only (theourgia trace) trace-enable!))

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x))))
             e))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/delete-targets-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (run store . args) (rpc-dispatch store args "author"))
(define (new-id a)
  (let ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a)))))
    (and ev (pair? (cadr ev)) (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (batch store . intents) (run store 'batch (format "~s" intents)))
(define (made store title) (new-id (car (cadr (batch store (list 'insert 'root #f (list '(kind . doc) (cons 'title title))))))))
;; A review citing each of RESULTS, made in one write.
(define (review store title . results)
  (new-id (car (cadr (apply batch store (list 'insert 'root #f (list '(kind . doc) (cons 'title title) '(slot . "review")))
                            (map (lambda (r) (list 'link '(from 0) 'cites r)) results))))))
;; A read of a deleted block succeeds and says (deleted . #t): live is a read
;; that answers and holds no such pair.
(define (holds-deleted? x)
  (cond ((and (pair? x) (eq? (car x) 'deleted) (eq? (cdr x) #t)) #t)
        ((pair? x) (or (holds-deleted? (car x)) (holds-deleted? (cdr x))))
        (else #f)))
(define (live? store id) (let ((a (run store 'read id))) (and (pair? a) (eq? (car a) 'ok) (not (holds-deleted? a)))))
;; -> (error refused rule-violation) and the (rule block) of each failure, or the answer's head.
(define (refusal-shape a)
  (if (and (list? a) (>= (length a) 4) (eq? (car a) 'error) (eq? (cadr a) 'refused))
      (list (list (car a) (cadr a) (caddr a))
            (map (lambda (f) (list (cadr (assq 'rule f)) (cadr (assq 'block f)))) (cdr (list-ref a 3))))
      (and (pair? a) (car a))))
(define (traced thunk)
  (let-values (((p get) (open-string-output-port)))
    (trace-enable! #t)
    (let ((v (parameterize ((current-error-port p)) (thunk))))
      (trace-enable! #f)
      (cons v (get)))))
(define (count-in text needle)
  (let loop ((i 0) (n 0))
    (cond ((> (+ i (string-length needle)) (string-length text)) n)
          ((string=? (substring text i (+ i (string-length needle))) needle) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))
(define review-cites '(rule "review-cites" "--on" "doc" "--where" "(field ?w \"slot\" \"review\")" "--must" "(edge ?w cites ?s)"))

;; ---- a store with the rule in force -------------------------------------------------------

(define D (string-append root "/d"))
(tolerant (run D 'init))
(tolerant (apply run D review-cites))
(define R (tolerant (made D "R")))
(define W (tolerant (review D "W" R)))
(define R3 (tolerant (made D "R3")))
(define R4 (tolerant (made D "R4")))
(define W2 (tolerant (review D "W2" R3 R4)))
(define lone (tolerant (made D "lone")))

(want "D deleting the result a review's only citation names is refused by review-cites, naming the review; the result stays"
      (in-order (refusal-shape (run D 'del R)) (live? D R))
      (list (list '(error refused rule-violation) (list (list 'review-cites W))) #t))
(want "D a block no live block has an edge to is deleted"
      (in-order (car (run D 'del lone)) (live? D lone))
      '(ok #f))
(want "D deleting one of two results a review cites is taken: the review is judged and still holds"
      (in-order (car (run D 'del R3)) (live? D R3) (live? D W2))
      '(ok #f #t))
;; The other direction: a block that must keep an incoming citation, and the
;; one review citing it. Deleting the review is refused by the cited block's
;; rule, naming it.
(define R5 (tolerant (made D "R5")))
(define W3 (tolerant (review D "W3" R5)))
(tolerant (run D 'set R5 "keep" "yes"))
(tolerant (run D 'rule "kept-cited" "--on" "doc" "--where" "(field ?w \"keep\" \"yes\")" "--must" "(edge ?a cites ?w)"))
(want "D the far end in the other direction: deleting the only review citing a block that must stay cited is refused by kept-cited, naming that block; the review stays"
      (in-order (refusal-shape (run D 'del W3)) (live? D W3))
      (list (list '(error refused rule-violation) (list (list 'kept-cited R5))) #t))

;; ---- check's audit ------------------------------------------------------------------------

;; A review that lost its citation before the rule was declared: the audit
;; lists it, the verdict unchanged.
(define A (string-append root "/a"))
(tolerant (run A 'init))
(define AR (tolerant (made A "R")))
(define AW (tolerant (review A "W" AR)))
(tolerant (run A 'del AR))
(tolerant (apply run A review-cites))
(want "A check audits the rule over the current state: the review whose result was deleted before the rule is listed; verdict ok"
      (let ((c (cdr (run A 'check))))
        (in-order (map (lambda (r) (list (cadr (assq 'rule (cdr r))) (cadr (assq 'block (cdr r)))))
                       (filter (lambda (r) (eq? (car r) 'rule-violation)) (cdr (or (assq 'rules c) '(rules)))))
                  (cadr (assq 'verdict c))))
      (list (list (list 'review-cites AW)) 'ok))

;; ---- N: no rule in force ------------------------------------------------------------------

(define N (string-append root "/n"))
(tolerant (run N 'init))
(define NR (tolerant (made N "R")))
(define NW (tolerant (review N "W" NR)))
(want "N a store with no rule in force: deleting a cited result is taken, with no copy of the state made"
      (let ((t (traced (lambda () (run N 'del NR)))))
        (in-order (and (pair? (car t)) (car (car t))) (count-in (cdr t) "(trace rehearsal") (live? N NR) (live? N NW)))
      '(ok 0 #f #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ndelete-targets complete\n" bad rows)
(exit (if (= bad 0) 0 1))
