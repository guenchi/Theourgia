#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; The query language: the terms and goals, registration, recursion, the
;; least model against a second implementation, the canonical answer, the
;; facts against their providers, the rule library, the refusals, the verb
;; and the session. Most rows run on reductions built from events; the
;; facts that need a store (search, the defs index, the verb) on a store.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce defs-index)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read state-outline block-hash block-id reduce-applied-cut)
        (only (theourgia wire) storable-encode sexpr->string-extended)
        (only (theourgia render) render-human)
        (only (theourgia digest) sha256 bytevector->hex)
        (theourgia query))

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(register-verbs! extension-verbs)

(define (state-of . events)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e)) events)
    r))
(define (sec n) `("a" ,n () (put ((kind . section) (title . ,(format "s~a" n))))))
(define (link n from rel to) `("a" ,n () (link ,from ,rel ,to)))

;; The rows of a goal, sorted; or the refusal.
(define (q state goal . extra)
  (let ((a (session-answer (apply make-query-session state extra) goal)))
    (if (eq? (car a) 'ok) (cadr a) a)))
(define (digest state goal) (cadddr (session-answer (make-query-session state) goal)))

;; ---- Q1: the language ---------------------------------------------------------------------

;; a.1 -> a.2 -> a.3 by depends-on; a.4 alone.
(define q1 (state-of (sec 1) (sec 2) (sec 3) (sec 4)
                     (link 5 "a.1" 'depends-on "a.2") (link 6 "a.2" 'depends-on "a.3")))
(want "Q1 a query that is a fact goal, each pattern of bound and unbound arguments"
      (in-order (q q1 '(edge "a.1" depends-on ?y))
                (q q1 '(edge ?x depends-on "a.3"))
                (q q1 '(edge ?x depends-on ?y))
                (q q1 '(edge "a.1" ?r ?y)))
      '((("a.2")) (("a.2")) (("a.1" "a.2") ("a.2" "a.3")) ((depends-on "a.2"))))
(want "Q1 `_` is never reported and never joined"
      (in-order (q q1 '(edge ?x _ _)) (q q1 '(and (edge ?x _ ?y) (edge ?y _ _))))
      '((("a.1") ("a.2")) (("a.1" "a.2"))))
(want "Q1 each test: =, /=, string<, member"
      (in-order (q q1 '(and (edge ?x depends-on ?y) (= ?x "a.1")))
                (q q1 '(and (edge ?x depends-on ?y) (/= ?x "a.1")))
                (q q1 '(and (edge ?x depends-on ?y) (string< ?y "a.3")))
                (q q1 '(and (kind ?x ?k) (member ?x ("a.2" "a.4")))))
      '((("a.1" "a.2")) (("a.2" "a.3")) (("a.1" "a.2")) (("a.2" section) ("a.4" section))))
(want "Q1 a conjunction of two fact goals, and of a fact goal and a rule goal"
      (in-order (q q1 '(and (edge ?x depends-on ?y) (edge ?y depends-on ?z)))
                (q q1 '(and (kind ?x section) (depends* ?x "a.3"))))
      '((("a.1" "a.2" "a.3")) (("a.1") ("a.2"))))
(want "Q1 a goal with no variable: one empty row when it holds, none otherwise"
      (in-order (q q1 '(edge "a.1" depends-on "a.2")) (q q1 '(edge "a.1" depends-on "a.3")))
      '((()) ()))

;; ---- Q2: registration --------------------------------------------------------------------

(define (refused-by rules)
  (guard (e (#t (if (and (condition? e) (message-condition? e)) (condition-message e) 'RAISED-OTHER)))
    (check-rules (append rule-library rules))
    'accepted))
(want "Q2 the loader refuses, by name, each violation of registration; the shipped library passes"
      (in-order (refused-by '(((bad-rule ?a) (nosuch ?a))))
                (refused-by '(((bad-rule ?a) (edge ?a depends-on))))
                (refused-by '(((bad-rule ?a ?b) (kind ?a _))))
                (refused-by '(((bad-rule ?a) (= ?a "x") (kind ?a _))))
                (refused-by '(((bad-rule ?a) (and (kind ?a _)))))
                (check-rules rule-library))
      '("unknown relation" "wrong arity" "a head variable in no body goal"
        "a test variable not bound by an earlier goal" "and in a rule" #t))

;; ---- Q3: recursion ---------------------------------------------------------------------------

(define chain q1)
(define diamond (state-of (sec 1) (sec 2) (sec 3) (sec 4)
                          (link 5 "a.1" 'depends-on "a.2") (link 6 "a.1" 'depends-on "a.3")
                          (link 7 "a.2" 'depends-on "a.4") (link 8 "a.3" 'depends-on "a.4")))
(define cycle (state-of (sec 1) (sec 2) (sec 3)
                        (link 4 "a.1" 'depends-on "a.2") (link 5 "a.2" 'depends-on "a.3") (link 6 "a.3" 'depends-on "a.1")))
(define self (state-of (sec 1) (link 2 "a.1" 'depends-on "a.1")))
(want "Q3 depends* on a chain, a diamond (one row for the shared end), a cycle (each member once), a self edge"
      (in-order (q chain '(depends* "a.1" ?y))
                (q diamond '(depends* "a.1" ?y))
                (q cycle '(depends* "a.1" ?y))
                (q self '(depends* "a.1" ?y)))
      '((("a.2") ("a.3")) (("a.2") ("a.3") ("a.4")) (("a.1") ("a.2") ("a.3")) (("a.1"))))
(define nested (state-of `("a" 1 () (put ((kind . section) (title . "top"))))
                         `("a" 2 () (put ((kind . section) (title . "mid") (parent . "a.1") (ord . 1))))
                         `("a" 3 () (put ((kind . section) (title . "low") (parent . "a.2") (ord . 1))))))
(want "Q3 under* to the root, and within agrees with under* on every pair"
      (in-order (q nested '(under* "a.3" ?p))
                (equal? (q nested '(under* ?a ?b)) (q nested '(within ?a ?b))))
      '((("a.1") ("a.2") ("root")) #t))
;; TWO WRITERS, two interleavings of the same records.
(define (two-writers order)
  (apply state-of
         (map (lambda (i) (list-ref (list '("a" 1 () (put ((kind . section) (title . "a"))))
                                          '("b" 1 () (put ((kind . section) (title . "b"))))
                                          '("a" 2 (("b" . 1)) (link "a.1" depends-on "b.1"))
                                          '("b" 2 (("a" . 1)) (link "b.1" depends-on "a.1")))
                                    i))
              order)))
(want "Q3 the same answers when the records are applied in another order"
      (in-order (q (two-writers '(0 1 2 3)) '(depends* ?x ?y))
                (equal? (q (two-writers '(0 1 2 3)) '(depends* ?x ?y)) (q (two-writers '(1 0 3 2)) '(depends* ?x ?y))))
      '((("a.1" "a.1") ("a.1" "b.1") ("b.1" "a.1") ("b.1" "b.1")) #t))

;; ---- Q4: the least model against a second implementation -------------------------------------

;; odd(a,b): a path of odd length; even(a,b): of even length, at least two.
(define parity-rules
  '(((odd ?a ?b) (edge ?a nx ?b))
    ((odd ?a ?b) (edge ?a nx ?c) (even ?c ?b))
    ((even ?a ?b) (edge ?a nx ?c) (odd ?c ?b))))
(define seed 12345)
(define (next!) (set! seed (mod (+ (* seed 1103515245) 12345) 2147483648)) seed)
;; A HARD BOUND THE GENERATOR REFUSES PAST: no edge set is longer than this,
;; whatever the arithmetic above it does, so a mistake here ends the row
;; with its reason instead of growing until the alarm.
(define most-edges 7)
(define (random-edges)
  (let ((n (+ 1 (mod (next!) most-edges))))
    (let loop ((k 0) (out '()))
      (cond ((> k most-edges)
             (assertion-violation 'random-edges "the edge set passed its bound" most-edges k))
            ((= k n) out)
            (else (loop (+ k 1) (cons (cons (+ 1 (mod (next!) 5)) (+ 1 (mod (next!) 5))) out)))))))
;; THE REFERENCE: odd and even reachability by a bottom-up loop to a fixed point.
(define (reference edges)
  (let loop ((odd (map (lambda (e) (list (car e) (cdr e))) edges)) (even '()))
    (let* ((odd2 (append odd (apply append (map (lambda (e) (map (lambda (p) (list (car e) (cadr p)))
                                                                 (filter (lambda (p) (= (car p) (cdr e))) even)))
                                                edges))))
           (even2 (append even (apply append (map (lambda (e) (map (lambda (p) (list (car e) (cadr p)))
                                                                   (filter (lambda (p) (= (car p) (cdr e))) odd)))
                                                  edges))))
           (dedup (lambda (l) (let d ((l l) (o '())) (cond ((null? l) o) ((member (car l) o) (d (cdr l) o)) (else (d (cdr l) (cons (car l) o)))))))
           (o (dedup odd2)) (e (dedup even2)))
      (if (and (= (length o) (length (dedup odd))) (= (length e) (length (dedup even))))
          (list-sort (lambda (x y) (or (< (car x) (car y)) (and (= (car x) (car y)) (< (cadr x) (cadr y))))) o)
          (loop o e)))))
(define (edge-state edges)
  (apply state-of
         (append (map (lambda (i) `("a" ,i () (put ((kind . section) (title . ,(format "n~a" i)))))) '(1 2 3 4 5))
                 (let loop ((es edges) (n 6) (out '()))
                   (if (null? es) (reverse out)
                       (loop (cdr es) (+ n 1) (cons `("a" ,n () (link ,(block-id "a" (car (car es))) nx ,(block-id "a" (cdr (car es))))) out)))))))
(define (as-numbers rows-of)
  (list-sort (lambda (x y) (or (< (car x) (car y)) (and (= (car x) (car y)) (< (cadr x) (cadr y)))))
             (map (lambda (r) (map (lambda (id) (string->number (substring id 2 (string-length id)) 36)) r)) rows-of)))
(want "Q4 the least model: a mutually recursive pair equals a bottom-up reference on 200 random edge sets"
      (let loop ((k 0) (wrong '()))
        (if (= k 200)
            (list (length wrong) (if (pair? wrong) (car wrong) 'none))
            (let* ((edges (random-edges))
                   (got (as-numbers (q (edge-state edges) '(odd ?a ?b) parity-rules)))
                   (ref (reference edges)))
              (loop (+ k 1) (if (equal? got ref) wrong (cons (list edges got ref) wrong))))))
      '(0 none))
(want "Q4 an answer that needs more than one pass: odd from a node of a three-cycle reaches every node"
      (map car (q (edge-state '((1 . 2) (2 . 3) (3 . 1))) '(odd "a.1" ?b) parity-rules))
      '("a.1" "a.2" "a.3"))

;; ---- Q5: canonical ------------------------------------------------------------------------------

(define nan-a (/ 0. 0.))
(define nan-b (- (/ 0. 0.)))
;; THE VALUE IS THE FIRST VARIABLE, and the blocks hold the values so that
;; neither their id order nor its reverse is the byte order: whichever order
;; the provider meets them in, it is not the order the answer must give.
(define q5 (state-of (sec 1) (sec 2) (sec 3) (sec 4)
                     `("a" 5 () (set "a.1" x #vu8(1 2))) `("a" 6 () (set "a.2" x ,nan-a))
                     `("a" 7 () (set "a.3" x #\a)) `("a" 8 () (set "a.4" x ,nan-b))))
(define (rendered row) (string->utf8 (sexpr->string-extended (storable-encode row))))
(define (bytes<? a b)
  (let loop ((i 0))
    (cond ((= i (bytevector-length a)) (< i (bytevector-length b)))
          ((= i (bytevector-length b)) #f)
          ((= (bytevector-u8-ref a i) (bytevector-u8-ref b i)) (loop (+ i 1)))
          (else (< (bytevector-u8-ref a i) (bytevector-u8-ref b i))))))
(want "Q5 a character, a bytevector and two NaNs: four rows, sorted by their rendered bytes"
      (let ((rs (q q5 '(and (field _ "x" ?v) (field ?id "x" ?v)))))
        (in-order (length rs)
                  (let loop ((l rs)) (or (null? l) (null? (cdr l)) (and (bytes<? (rendered (car l)) (rendered (cadr l))) (loop (cdr l)))))))
      '(4 #t))
(want "Q5 rows are deduplicated: a binding found twice is one row"
      (q diamond '(and (depends* "a.1" ?y) (depends* "a.1" "a.4")))
      '(("a.2") ("a.3") ("a.4")))
(define d-before (digest chain '(depends* "a.1" ?y)))
(want "Q5 the digest: unchanged by an unrelated write, changed by a relevant one, the same at two cuts with the same bindings"
      (in-order (equal? d-before (digest (state-of (sec 1) (sec 2) (sec 3) (sec 4) (link 5 "a.1" 'depends-on "a.2")
                                                   (link 6 "a.2" 'depends-on "a.3") '("a" 7 () (set "a.4" title "other")))
                                         '(depends* "a.1" ?y)))
                (equal? d-before (digest (state-of (sec 1) (sec 2) (sec 3) (sec 4) (link 5 "a.1" 'depends-on "a.2")
                                                   (link 6 "a.2" 'depends-on "a.3") (link 7 "a.3" 'depends-on "a.4"))
                                         '(depends* "a.1" ?y)))
                (equal? (reduce-applied-cut chain)
                        (reduce-applied-cut (state-of (sec 1) (sec 2) (sec 3) (sec 4) (link 5 "a.1" 'depends-on "a.2")
                                                      (link 6 "a.2" 'depends-on "a.3") '("a" 7 () (set "a.4" title "other"))))))
      '(#t #f #f))
(define (nest n) (if (= n 0) 'x (list (nest (- n 1)))))
(want "Q5 a value past the codec's nesting answers unrenderable, naming the relation; the row is not dropped"
      (q (state-of (sec 1) `("a" 2 () (set "a.1" deep ,(nest 70)))) '(field ?id "deep" ?v))
      '(error unrenderable (relation field) (reason nesting)))

;; ---- Q7 and Q7b: the rules ---------------------------------------------------------------------------

;; t = a.1 (a task-like section) with child a.2; a.2 depends on a.3; decisions:
;; a.4 open under t, a.5 open under a.3, a.6 implemented (by a.c, the code
;; block at seq 12: ids are base 36), a.7 closed, a.8 open but superseded by
;; a.9, a.a (seq 10) open but inference.
(define q7 (state-of `("a" 1 () (put ((kind . section) (title . "t"))))
                     `("a" 2 () (put ((kind . section) (title . "child") (parent . "a.1") (ord . 1))))
                     `("a" 3 () (put ((kind . section) (title . "premise"))))
                     `("a" 4 () (put ((kind . decision) (title . "open under t") (parent . "a.1") (ord . 2))))
                     `("a" 5 () (put ((kind . decision) (title . "open under premise") (parent . "a.3") (ord . 1))))
                     `("a" 6 () (put ((kind . decision) (title . "implemented") (parent . "a.1") (ord . 3))))
                     `("a" 7 () (put ((kind . decision) (title . "closed") (parent . "a.1") (ord . 4) (status . "done"))))
                     `("a" 8 () (put ((kind . decision) (title . "superseded") (parent . "a.1") (ord . 5))))
                     `("a" 9 () (put ((kind . section) (title . "newer"))))
                     `("a" 10 () (put ((kind . decision) (title . "inference") (parent . "a.1") (ord . 6) (class . inference))))
                     `("a" 11 () (link "a.2" depends-on "a.3"))
                     `("a" 12 () (put ((kind . code) (title . "impl"))))
                     `("a" 13 () (link "a.c" implements "a.6"))
                     `("a" 14 () (link "a.9" supersedes "a.8"))))
(want "Q7 unsettled-for: an open decision under t, one under a premise reached by depends*; not closed, implemented, superseded or non-authoritative"
      (q q7 '(unsettled-for "a.1" ?d))
      '(("a.4") ("a.5")))
(want "Q7 unsettled-for holds for the block itself when it is an unsettled decision"
      (q q7 '(unsettled-for "a.4" ?d))
      '(("a.4")))
(want "Q7 scope-of holds the block, its subtree, and what either depends on"
      (map car (q q7 '(scope-of "a.1" ?m)))
      '("a.1" "a.2" "a.3" "a.4" "a.6" "a.7" "a.8" "a.a"))
(want "Q7 contradicts is symmetric and needs both ends in force and authoritative"
      (let ((s (state-of (sec 1) (sec 2) (sec 3) (link 4 "a.1" 'conflicts-with "a.2")
                         '("a" 5 () (set "a.3" class inference)) (link 6 "a.3" 'conflicts-with "a.1"))))
        (q s '(contradicts ?a ?b)))
      '(("a.1" "a.2") ("a.2" "a.1")))
(want "Q7b hard called with a constant role answers that role only"
      (let ((rs (q q7 '(hard "a.1" ?h unit ?about))))
        (in-order (length rs) (map car rs)))
      '(7 ("a.1" "a.2" "a.4" "a.6" "a.7" "a.8" "a.a")))
(want "Q7b hard: a row for each role; a block with two roles answers two rows"
      (let ((rs (q q7 '(hard "a.1" ?h ?role ?about))))
        (in-order (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                             (let d ((l (map cadr rs)) (o '())) (cond ((null? l) o) ((memq (car l) o) (d (cdr l) o)) (else (d (cdr l) (cons (car l) o))))))
                  (length (filter (lambda (r) (equal? (car r) "a.4")) rs))))
      '((cause member supersedes unit unsettled) 3))

;; ---- Q11: the session ----------------------------------------------------------------------------------

(want "Q11 two queries in one session sharing a call of depends*: the second expands it zero times and answers as alone"
      (let* ((S (make-query-session chain))
             (first (session-answer S '(depends* "a.1" ?y)))
             (n1 (session-expansions S 'depends* "a.1" '?y))
             (second (session-answer S '(and (depends* "a.1" ?y) (kind ?y section))))
             (n2 (session-expansions S 'depends* "a.1" '?y)))
        (in-order (> n1 0) (- n2 n1)
                  (equal? (cadr second) (cadr (session-answer (make-query-session chain) '(and (depends* "a.1" ?y) (kind ?y section)))))))
      '(#t 0 #t))

;; ---- Q8: refusals ----------------------------------------------------------------------------------------

(want "Q8 refusals by name: not a goal, unknown relation, wrong arity, a variable only a test mentions, score with its text unbound"
      (map (lambda (g) (let ((a (q chain g))) (and (pair? a) (eq? (car a) 'error) (list (cadr a) (caddr a)))))
           (list "edge" '(nosuch ?x) '(edge ?x) '(and (= ?x "a.1") (kind ?x _)) '(score ?id ?t ?n)))
      '((bad-request not-a-goal) (bad-request unknown-relation) (bad-request wrong-arity)
        (bad-request test-variable-unbound) (bad-request unbound-argument)))
(want "Q8 score unbound names the relation and the position"
      (q chain '(score ?id ?t ?n))
      '(error bad-request unbound-argument (relation score) (position 2)))
(want "Q8 the budget: a relation sized past it names the relation"
      (let ((a (session-answer (make-query-session cycle '() 5) '(depends* ?x ?y))))
        (list (car a) (cadr a) (car (caddr a))))
      '(error query-budget relation))

;; ---- the review's findings: a row each ------------------------------------------------------------

;; A session rule whose name a goal can spell is never displaced by the
;; query's own synthetic relation. Where that relation is a symbol
;; `%query-N`, N is found from a session's expansion counts and the next
;; query's name is given to a rule; where it is not a symbol, there is
;; nothing to find and a rule named `%query-1` stands for every such name.
(define (synthetic-next)
  (let ((S0 (make-query-session chain)))
    (session-answer S0 '(kind ?x section))
    (let loop ((k 1))
      (cond ((> k 200000) #f)
            ((> (session-expansions S0 (string->symbol (string-append "%query-" (number->string k))) '?x) 0) (+ k 1))
            (else (loop (+ k 1)))))))
(want "Q2 a session rule named as the query's own synthetic relation is not displaced by it"
      (let* ((name (string->symbol (string-append "%query-" (number->string (or (synthetic-next) 1)))))
             (S (make-query-session chain (list (list (list name '?x) '(kind ?x section))))))
        (cadr (session-answer S (list name '?x))))
      '(("a.1") ("a.2") ("a.3") ("a.4")))

(want "Q2 a lone ? is a variable: reported, and bound by the goal"
      (let ((a (session-answer (make-query-session chain) '(kind ? section))))
        (in-order (caddr a) (length (cadr a))))
      '((?) 4))
(want "Q2 _ in a test is a variable no earlier goal binds: refused in a query and in a rule"
      (in-order (q chain '(and (kind ?x _) (= _ _)))
                (refused-by '(((bad-rule ?a) (kind ?a _) (= _ ?a)))))
      '((error bad-request test-variable-unbound (variable _)) "a test variable not bound by an earlier goal"))
(want "Q2 member takes a term then a list constant, and a list constant nowhere else: refused in a query and in a rule"
      (in-order (q chain '(and (kind ?x ?k) (member (a) (section))))
                (q chain '(and (kind ?x ?k) (member ?k section)))
                (q chain '(and (kind ?x ?k) (member ?k ?x)))
                (refused-by '(((bad-rule ?a) (kind ?a ?k) (member ?k ?a))))
                (refused-by '(((bad-rule ?a) (kind ?a (section doc))))))
      '((error bad-request not-a-term (term (a))) (error bad-request not-a-term (term section))
        (error bad-request not-a-term (term ?x))
        "a body argument is not a term of its position" "a body argument is not a term of its position"))
(want "Q2 and as a rule head is refused as and in a rule"
      (refused-by '(((and ?a) (kind ?a _))))
      "and in a rule")

(define (spent S) ((guard (e (#t (lambda (S) 'no-reading))) (eval 'session-spent (environment '(theourgia query)))) S))
(want "Q8 score's hits are counted against the budget, under score's name"
      (let ((s (state-of `("a" 1 () (put ((kind . section) (title . "alpha one"))))
                         `("a" 2 () (put ((kind . section) (title . "alpha two"))))
                         `("a" 3 () (put ((kind . section) (title . "alpha three")))))))
        (session-answer (make-query-session s '() 1) '(score ?id "alpha" ?n)))
      '(error query-budget (relation score) (tuples 3)))
(want "Q8 a bound version call is counted against the budget, under version's name"
      (session-answer (make-query-session chain '() 0) '(version "a.1" ?h))
      '(error query-budget (relation version) (tuples 1)))
(define cost-q1 (let ((S (make-query-session chain))) (session-answer S '(depends* "a.1" ?y)) (spent S)))
(want "Q8 the budget is per query: a second query reading only complete tables spends 0 of it"
      (let ((S (make-query-session chain)))
        (session-answer S '(and (depends* "a.1" ?y) (kind ?y section)))
        (let ((first (spent S)))
          (session-answer S '(and (depends* "a.1" ?y) (kind ?y doc)))
          (in-order (and (integer? first) (> first 0)) (spent S))))
      '(#t 0))
(want "Q8 the budget is per query: two queries each within it are both answered, though together they pass it"
      (let ((S (make-query-session chain '() cost-q1)))
        (map car (list (session-answer S '(depends* "a.1" ?y)) (session-answer S '(depends* "a.1" ?z)))))
      '(ok ok))
(want "Q8 the budget is per query: a query that alone passes it refuses, though the session had room"
      (let ((S (make-query-session chain '() (- cost-q1 1))))
        (in-order (car (session-answer S '(title "a.1" ?t))) (list-head (session-answer S '(depends* "a.1" ?y)) 2)))
      '(ok (error query-budget)))

(want "Q6 in-library: a code block left under a deleted library is in none"
      (q (state-of `("a" 1 () (put ((kind . library) (name . (qa gone)) (title . "lib"))))
                   `("a" 2 () (put ((kind . code) (title . "c") (parent . "a.1") (ord . 1))))
                   '("a" 3 () (del "a.1")))
         '(in-library "a.2" ?n))
      '((none)))
(define unplaced (state-of (sec 1) (sec 2) (sec 3)
                           '("a" 4 () (move "a.3" "a.1" 1))
                           '("b" 1 (("a" . 3)) (move "a.3" "a.2" 1))))
;; NOTE: move's third argument is a position. Written as #f the two moves were
;; refused, a.3 stayed placed at the root, and the row below read "root" for
;; a reason that had nothing to do with an unresolved placement.
(want "SETUP the two concurrent moves leave a.3 marked in the outline"
      (let ((row (find (lambda (r) (equal? (caddr r) "a.3")) (state-outline unplaced))))
        (and row (length row)))
      4)
(want "Q6 under: a block whose placement is unresolved has no parent, not root"
      (q unplaced '(under "a.3" ?p))
      '())
(want "Q6 ref: a live block's reference to itself is a reference"
      (q (state-of '("a" 1 () (put ((kind . section) (title . "s") (src . "see [[a.1]]"))))) '(ref "a.1" ?to))
      '(("a.1")))
(want "Q6 ref: text stored as UTF-8 bytes is read as the store reads it"
      (q (state-of (sec 1) `("a" 2 () (put ((kind . section) (title . "u") (src . ,(string->utf8 "see [[a.1]]"))))))
         '(ref "a.2" ?to))
      '(("a.1")))
(define (nest n) (if (= n 0) 'x (list (nest (- n 1)))))
(want "Q5 unrenderable names the relation that bound the value, not the first goal"
      (q (state-of (sec 1) `("a" 2 () (set "a.1" deep ,(nest 70)))) '(and (kind ?id section) (field ?id "deep" ?v)))
      '(error unrenderable (relation field) (reason nesting)))
(define (joined strings)
  (if (null? strings) "" (fold-left (lambda (acc s) (string-append acc "\n" s)) (car strings) (cdr strings))))
(want "Q5 the digest is sha256 of the rendered rows joined by newlines"
      (digest chain '(depends* "a.1" ?y))
      (bytevector->hex (sha256 (string->utf8 (joined (map (lambda (r) (sexpr->string-extended (storable-encode r)))
                                                          '(("a.2") ("a.3"))))))))
(want "Q6 edge and ref leave out a deleted endpoint (with it live, both hold)"
      (let ((events (list (sec 1) (sec 2) (link 3 "a.1" 'depends-on "a.2")
                          '("a" 4 () (put ((kind . section) (title . "r") (src . "see [[a.2]]")))))))
        (in-order (q (apply state-of events) '(edge "a.1" ?r ?to)) (q (apply state-of events) '(ref "a.4" ?to))
                  (q (apply state-of (append events '(("a" 5 () (del "a.2"))))) '(edge "a.1" ?r ?to))
                  (q (apply state-of (append events '(("a" 5 () (del "a.2"))))) '(ref "a.4" ?to))))
      '(((depends-on "a.2")) (("a.2")) () ()))
(want "Q6 verified-by: a live verifier with a current edge; none once the verifier is deleted"
      (let ((events (list (sec 1) (sec 2) (link 3 "a.2" 'verifies "a.1"))))
        (in-order (q (apply state-of events) '(verified-by "a.1" ?v))
                  (q (apply state-of (append events '(("a" 4 () (del "a.2"))))) '(verified-by "a.1" ?v))))
      '((("a.2")) ()))
(want "Q7 contradicts needs both ends in force: a superseded end gives none"
      (q (state-of (sec 1) (sec 2) (link 3 "a.1" 'conflicts-with "a.2") (sec 4) (link 5 "a.4" 'supersedes "a.2"))
         '(contradicts ?a ?b))
      '())

;; t = a.1; a.2 a decision under t in review (a.3 implements it, then a.2 is
;; edited); a.6 a member superseded by a.8, which a.7 supersedes; a.b a
;; member refuted by a.c, which is then deleted.
(define q7r-events
  (list `("a" 1 () (put ((kind . section) (title . "t"))))
        `("a" 2 () (put ((kind . decision) (title . "in review") (parent . "a.1") (ord . 1))))
        `("a" 3 () (put ((kind . code) (title . "impl"))))
        '("a" 4 () (link "a.3" implements "a.2"))
        '("a" 5 () (set "a.2" title "in review, edited"))
        `("a" 6 () (put ((kind . section) (title . "m") (parent . "a.1") (ord . 2))))
        `("a" 7 () (put ((kind . section) (title . "x1"))))
        `("a" 8 () (put ((kind . section) (title . "x2"))))
        '("a" 9 () (link "a.7" supersedes "a.8"))
        '("a" 10 () (link "a.8" supersedes "a.6"))
        `("a" 11 () (put ((kind . section) (title . "m2") (parent . "a.1") (ord . 3))))
        `("a" 12 () (put ((kind . section) (title . "refuter"))))
        '("a" 13 () (link "a.c" refutes "a.b"))))
(define q7r (apply state-of (append q7r-events '(("a" 14 () (del "a.c"))))))
(want "Q7 unsettled-for finds a decision in review"
      (q q7r '(unsettled-for "a.1" ?d))
      '(("a.2")))
(want "Q7b hard: the implementer of a decision in review, with the pair that moved"
      (q q7r '(hard "a.1" ?h implementer ?about))
      '(("a.3" "a.2")))
(want "Q7b hard: a superseder reached through a chain answers with the member it reaches"
      (q q7r '(hard "a.1" ?h supersedes "a.6"))
      '(("a.7") ("a.8")))
(want "Q7b hard: a cause that is deleted answers none (while it lives, it is a cause)"
      (in-order (q q7r '(hard "a.1" ?h cause "a.b"))
                (q (apply state-of q7r-events) '(hard "a.1" ?h cause "a.b")))
      '(() (("a.c"))))

;; ---- the second review's findings: a row each -----------------------------------------------------

;; THE CODEC'S BOUNDARY, found rather than assumed: the deepest value that
;; renders alone; as a row it is one list deeper and does not render.
(define deepest
  (let loop ((n 1))
    (if (guard (e (#t #f)) (sexpr->string-extended (storable-encode (nest (+ n 1)))) #t)
        (if (> n 500) n (loop (+ n 1)))
        n)))
(want "SETUP at the boundary the value renders alone and not as a row"
      (in-order (guard (e (#t #f)) (sexpr->string-extended (storable-encode (nest deepest))) #t)
                (guard (e (#t #f)) (sexpr->string-extended (storable-encode (list "a.1" (nest deepest)))) #t))
      '(#t #f))
(want "Q5 unrenderable at the boundary still names the relation that bound the value"
      (q (state-of (sec 1) `("a" 2 () (set "a.1" deep ,(nest deepest)))) '(and (kind ?id section) (field ?id "deep" ?v)))
      '(error unrenderable (relation field) (reason nesting)))

(want "Q6 field keeps a stored value whose head is conflict but which is not the reducer's conflict"
      (q (state-of (sec 1) '("a" 2 () (set "a.1" x (conflict foo)))) '(field "a.1" "x" ?v))
      '(((conflict foo))))

;; A LIBRARY WHOSE NAME IS CONTESTED HAS NO NAME, and no fact speaks one: a.1
;; and a.3 are libraries whose names two writers set at once; a.2 is code in
;; a.1 that calls f, a.4 code in a.3 that defines f. a.5 is a library with a
;; settled name whose imports two writers set at once.
(define contested
  (state-of `("a" 1 () (put ((kind . library) (name . (qx one)) (title . "A"))))
            `("a" 2 () (put ((kind . code) (lang . "javascript") (title . "a use") (src . "f()") (parent . "a.1") (ord . 1))))
            `("a" 3 () (put ((kind . library) (name . (qx two)) (title . "B"))))
            `("a" 4 () (put ((kind . code) (lang . "javascript") (title . "b def") (name . f) (src . "function f() { return 1 }") (parent . "a.3") (ord . 1))))
            `("a" 5 () (put ((kind . library) (name . (qx seven)) (imports . ((rnrs))) (title . "C"))))
            '("b" 1 (("a" . 5)) (set "a.1" name (qx three)))
            '("c" 1 (("a" . 5)) (set "a.1" name (qx four)))
            '("d" 1 (("a" . 5)) (set "a.3" name (qx five)))
            '("e" 1 (("a" . 5)) (set "a.3" name (qx six)))
            '("f" 1 (("a" . 5)) (set "a.5" imports ((rnrs) (qx eight))))
            '("g" 1 (("a" . 5)) (set "a.5" imports ((qx nine))))))
(want "Q6 a contested library name: no library row (a settled one has its row)"
      (in-order (q contested '(library "a.1" ?n)) (q contested '(library "a.5" ?n)))
      '(() (("(qx seven)"))))
(want "Q6 a contested library name: no in-library row for the code under it, neither none nor a name"
      (q contested '(in-library "a.2" ?n))
      '())
(want "Q6 a contested library name: no def row for its definitions"
      (q contested '(def ?d ?lib "f"))
      '())
(want "Q6 a contested imports field gives no imports rows"
      (q contested '(imports "(qx seven)" ?m))
      '())
(want "Q7 def-for does not join two libraries whose names are contested"
      (q contested '(def-for "a.2" "f" ?d))
      '())

(want "Q2 a rule headed by a test is refused"
      (refused-by '(((= ?a ?b) (kind ?a ?b))))
      "a rule heads a test")

;; HAND-COUNTED COSTS, so the rows below do not take their limit from the
;; product's own reading. Three sections titled alpha: kind's build is 3,
;; score's hits for alpha are 3 and are built once, the query's table holds 3.
(define alphas (state-of `("a" 1 () (put ((kind . section) (title . "alpha one"))))
                         `("a" 2 () (put ((kind . section) (title . "alpha two"))))
                         `("a" 3 () (put ((kind . section) (title . "alpha three"))))))
(want "Q8 score is searched once per text in a session, however many ids ask: the query costs 9"
      (let ((S (make-query-session alphas)))
        (in-order (length (cadr (session-answer S '(and (kind ?id section) (score ?id "alpha" ?n))))) (spent S)))
      '(3 9))
;; chain has four sections: version's build is 4, the bound calls read it, the
;; query's table holds 4.
(want "Q8 a bound version call reads the session's full build: the query costs 8"
      (let ((S (make-query-session chain)))
        (in-order (length (cadr (session-answer S '(and (version ?id ?h) (version ?id ?h2))))) (spent S)))
      '(4 8))
;; chain's depends* from a.1: edge's build 2; depends of a.1, a.2, a.3: 1, 1,
;; 0; depends* of a.1, a.2, a.3: 2, 1, 0; the query's table 2.
(want "Q8 a derived query's cost, counted by hand: 9"
      (let ((S (make-query-session chain))) (session-answer S '(depends* "a.1" ?y)) (spent S))
      9)
(want "Q8 per query, with the hand-counted limit: two queries each within 9 are both answered"
      (let ((S (make-query-session chain '() 9)))
        (map car (list (session-answer S '(depends* "a.1" ?y)) (session-answer S '(depends* "a.1" ?z)))))
      '(ok ok))
(want "Q8 a derived relation past the budget names a relation the query reached, and one tuple past it"
      (let ((a (session-answer (make-query-session cycle '() 5) '(depends* ?x ?y))))
        (in-order (list (car a) (cadr a)) (and (memq (cadr (caddr a)) '(depends depends* query)) #t) (cadddr a)))
      '((error query-budget) #t (tuples 6)))

(want "Q7b hard: a cause that is gone answers none: a deleted premise is named by premise-gone, and kind keeps it out"
      (let ((s (state-of (sec 1) `("a" 2 () (put ((kind . section) (title . "m") (parent . "a.1") (ord . 1))))
                         (sec 3) (link 4 "a.2" 'depends-on "a.3") '("a" 5 () (del "a.3")))))
        (in-order (q s '(validity-reason "a.2" ?why ?by)) (q s '(hard "a.1" ?h cause "a.2"))))
      '(((premise-gone "a.3")) ()))
(want "Q6 verified-by leaves out a verifier whose edge moved (the subject edited after it)"
      (q (state-of (sec 1) (sec 2) (link 3 "a.2" 'verifies "a.1") '("a" 4 () (set "a.1" title "edited")))
         '(verified-by "a.1" ?v))
      '())
(want "Q6 edge and ref leave out a deleted source"
      (let ((events (list (sec 1) (sec 2) (link 3 "a.1" 'depends-on "a.2")
                          '("a" 4 () (put ((kind . section) (title . "r") (src . "see [[a.2]]")))))))
        (in-order (q (apply state-of (append events '(("a" 5 () (del "a.1"))))) '(edge ?f ?r "a.2"))
                  (q (apply state-of (append events '(("a" 5 () (del "a.4"))))) '(ref ?f "a.2"))))
      '(() ()))

;; ---- the third review's findings: a row each ------------------------------------------------------

;; A ROW WHOSE FIRST VALUE IS A `#%` STRING is wrapped as a whole by the
;; encoder, a level no single value shows: k is the deepest value that renders
;; as a one-element list, and beside a `#%` title the row does not render.
(define k-deep (- deepest 1))
(want "SETUP beside a #% title the row is one level deeper than either value as a list"
      (in-order (guard (e (#t #f)) (sexpr->string-extended (storable-encode (list (nest k-deep)))) #t)
                (guard (e (#t #f)) (sexpr->string-extended (storable-encode (list "#%tag"))) #t)
                (guard (e (#t #f)) (sexpr->string-extended (storable-encode (list "#%tag" (nest k-deep)))) #t))
      '(#t #t #f))
(want "Q5 unrenderable names the relation that bound the value when the encoder wraps the whole row"
      (q (state-of `("a" 1 () (put ((kind . section) (title . "#%tag")))) `("a" 2 () (set "a.1" deep ,(nest k-deep))))
         '(and (title "a.1" ?t) (field "a.1" "deep" ?v)))
      '(error unrenderable (relation field) (reason nesting)))

;; A LITERAL WITH THE CONFLICT'S WHOLE SHAPE, written once, is a value; only
;; two surviving candidates make a conflict.
(define conflict-shaped '(conflict ((left "w1" 1) (right "w2" 1))))
(define literal (state-of (sec 1) `("a" 2 () (set "a.1" x ,conflict-shaped))
                          `("a" 3 () (put ((kind . code) (title . "c") (lang . ,conflict-shaped))))))
(want "Q6 field and lang keep a value written once that has the conflict's whole shape"
      (in-order (q literal '(field "a.1" "x" ?v)) (q literal '(lang "a.3" ?l)))
      (list (list (list conflict-shaped)) (list (list conflict-shaped))))
(want "Q6 library: a name written once with the conflict's shape is that name as stored; only two writers make it no name"
      (in-order (q (state-of `("a" 1 () (put ((kind . library) (title . "l") (name . (conflict (((qx a) "w1" 1) ((qx b) "w2" 1))))))))
                   '(library "a.1" ?n))
                (q (state-of `("a" 1 () (put ((kind . library) (title . "l") (name . (qx a)))))
                             '("b" 1 (("a" . 1)) (set "a.1" name (qx b))) '("c" 1 (("a" . 1)) (set "a.1" name (qx c))))
                   '(library "a.1" ?n)))
      '((("(conflict)")) ()))
(want "Q6 kind: a kind written once as a conflict-headed value is unreadable, not conflict (two writers make conflict)"
      (in-order (q (state-of `("a" 1 () (put ((kind . (conflict foo)) (title . "odd"))))) '(kind "a.1" ?k))
                (q (state-of `("a" 1 () (put ((kind . (conflict ((code "w1" 1) (doc "w2" 1)))) (title . "odd"))))) '(kind "a.1" ?k))
                (q (state-of (sec 1) '("b" 1 (("a" . 1)) (set "a.1" kind code)) '("c" 1 (("a" . 1)) (set "a.1" kind doc)))
                   '(kind "a.1" ?k)))
      '(((unreadable)) ((unreadable)) ((conflict))))

(printf "rows: ~a\n~a failures\nquery-cells complete\n" rows bad)
(exit (if (= bad 0) 0 1))
