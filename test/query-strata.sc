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

;; The query language's forms (query.sc's NOT and AGG handlers): (not
;; <goal>), (count ?n <goal>), (sum ?s ?x <goal>), (min ?m ?x <goal>) and
;; (max ?m ?x <goal>), in queries, rule values and stored relations' clauses,
;; never in the library's. A form's goal is a nested scope, kind-checked with
;; the outer kinds flowing in, evaluated to completion before it is read; a
;; stored relation that depends on itself through a form is unstratified.
;; Each group has a store of its own; a concurrent writer's record is
;; published as another writer's segment, as derived-relations does.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia query) make-query-session session-answer session-spent session-expansions session-budget-set!
              check-rules rule-library fact-relations)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut block-id state-rule)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha))

;; LOOKED UP, NOT IMPORTED, so the fixture loads on a tree without it (the
;; base) and its rows read red there one by one
(define state-derived
  (guard (e (#t (lambda (r name) 'not-in-this-tree))) (eval 'state-derived (environment '(theourgia reduce)))))
(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (with-expected name expected (x) (want-1 name (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                                                                 (condition-message e) e)
                                                                     (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
                                                    got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/query-strata-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(register-verbs! extension-verbs)

;; ---- stores, verbs, readings --------------------------------------------------------

(define n 0)
(define (store! name)
  (set! n (+ n 1))
  (let ((st (string-append root "/" name "-" (number->string n))))
    (rpc-dispatch st '(init) "author")
    st))
(define (on st . args) (rpc-dispatch st args "author"))
(define (state st) (open-and-reduce st))
(define (cut-of st) (reduce-applied-cut (state st)))
(define (head-of a) (if (and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)) (and (pair? a) (car a))))
(define (head3 a) (if (and (list? a) (>= (length a) 3)) (list-head a 3) a))
(define (head4 a) (if (and (list? a) (>= (length a) 4)) (list-head a 4) a))
(define (clause-of a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (new-id a)
  (let* ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (batch st . intents) (on st 'batch (format "~s" intents)))
(define (make st kind title)
  (let ((a (batch st (list 'insert 'root #f (list (cons 'kind kind) (cons 'title title))))))
    (new-id (car (cadr a)))))
(define (link! st from to) (on st 'link from "cites" to))
(define (field! st id name value) (batch st (list 'set id name value)))
(define (derive! st name value) (on st 'derive (symbol->string name) (format "~s" value)))
(define (retire! st name) (on st 'derive (symbol->string name) "--retire"))
(define (rule! st name . options) (apply on st 'rule name options))
(define (title! st id text) (on st 'set id "title" text))
;; THE CLIENT ROUTE, the query verb (the kind pass included)
(define (q st goal) (on st 'query (format "~s" goal)))
(define (rows-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a))) (map cdr (cdr (cadr a))) a))
(define (query-rows st goal) (rows-of (q st goal)))
(define (sorted rs) (list-sort (lambda (x y) (string<? (format "~s" x) (format "~s" y))) rs))
(define (sorted-rows st goal) (let ((r (query-rows st goal))) (if (list? r) (sorted r) r)))
;; THE VARIABLES A QUERY REPORTS, through a session (session-answer answers
;; (ok <rows> <variables> <digest>))
(define (reported st goal)
  (let ((a (session-answer (make-query-session (state st)) goal)))
    (if (and (pair? a) (eq? (car a) 'ok)) (caddr a) a)))
(define (local-writer st)
  (let ((p (clause-of (on st 'position) 'position))) (and p (car (cadr p)))))
;; A RECORD FROM ANOTHER WRITER, PEER (eight characters), whose premise is PAST
;; on the local writer
(define (forge! st peer payload past)
  (let* ((w (local-writer st))
         (bytes (encode-record 1 1789000000001 "peer" (list (cons w (cdr (assoc w past)))) (storable-encode payload))))
    (log-publish! st peer 1 bytes (segment-sha bytes))))
;; one whose premise is the whole cut, every writer's: it supersedes what any
;; writer declared before it
(define (forge-seeing! st peer payload)
  (let ((bytes (encode-record 1 1789000000001 "peer" (filter (lambda (e) (> (cdr e) 0)) (cut-of st)) (storable-encode payload))))
    (log-publish! st peer 1 bytes (segment-sha bytes))))
(define (over-kind name kind) (list (list 'head (list name 'ID)) (list 'clause (list name '?x) (list 'kind '?x kind))))
;; the fourth table's availability of NAME
(define (availability-of st name)
  (let ((a (on st 'query "--relations")))
    (and (pair? a) (eq? (car a) 'ok)
         (let ((e (find (lambda (i) (and (pair? i) (eq? (car i) 'derived) (eq? (cadr i) name))) (cdr (cadr a)))))
           (and e (list-ref e (- (length e) 1)))))))

;; ---- NOT ------------------------------------------------------------------------------------
(printf "== NOT: no row of its goal ==\n")
;; code c1 c2 c3 c4 c5, docs d1 d2; c1 and c2 cite each other, c3 cites c1,
;; c5 cites d1, c4 cites nothing
(define N (store! "not"))
(define c1 (make N 'code "c1"))
(define c2 (make N 'code "c2"))
(define c3 (make N 'code "c3"))
(define c4 (make N 'code "c4"))
(define c5 (make N 'code "c5"))
(define d1 (make N 'doc "d1"))
(define d2 (make N 'doc "d2"))
(for-each (lambda (e) (link! N (car e) (cdr e))) (list (cons c1 c2) (cons c2 c1) (cons c3 c1) (cons c5 d1)))
(want "NOT-1 the code blocks that cite nothing"
      (sorted-rows N '(and (kind ?b code) (not (edge ?b cites _))))
      (list (list c4)))
(want "NOT-2 with ?c bound by an earlier goal: the blocks cited by one they do not cite back"
      (sorted-rows N '(and (kind ?b code) (edge ?c cites ?b) (not (edge ?b cites ?c))))
      (list (list c1 c3)))
(want "NOT-3 ?o unbound outside is local to the not (no ?o such that): the same answer as with _"
      (sorted-rows N '(and (kind ?b code) (not (edge ?b cites ?o))))
      (list (list c4)))
(want "NOT-4 a local named again by a later goal: local-variable-reused"
      (q N '(and (kind ?b code) (not (edge ?b cites ?o)) (title ?o ?t)))
      '(error bad-request local-variable-reused (variable ?o) (form not)))
(define code-blocks (list c1 c2 c3 c4 c5))
(want "NOT-5 a not of a test with both operands bound: the ordered pairs of distinct code blocks"
      (sorted-rows N '(and (kind ?x code) (kind ?y code) (not (= ?x ?y))))
      (sorted (apply append (map (lambda (x) (map (lambda (y) (list x y)) (remove x code-blocks))) code-blocks))))
(want "NOT-6 a not of an and: the code blocks that cite no code block"
      (sorted-rows N '(and (kind ?b code) (not (and (edge ?b cites ?o) (kind ?o code)))))
      (sorted (list (list c4) (list c5))))
(derive! N 'citesany '((head (citesany ID)) (clause (citesany ?b) (edge ?b cites ?o))))
(want "NOT-7 a not over a stored relation"
      (sorted-rows N '(and (kind ?b code) (not (citesany ?b))))
      (list (list c4)))
(want "NOT-8 a not exports nothing: the query reports the outer variables only"
      (reported N '(and (kind ?b code) (not (edge ?b cites ?o))))
      '(?b))
(want "NOT-9 a disjoint constant inside a not is static emptiness, not a refusal: every block of every kind answered"
      (sorted-rows N '(and (kind ?b ?k) (not (= ?k "doc"))))
      (sorted (append (map (lambda (c) (list c 'code)) code-blocks) (list (list d1 'doc) (list d2 'doc)))))
(want "NOT-10 a disjoint join inside a not: accepted, the docs answered; and a kind after the not is what it was before"
      (in-order (sorted-rows N '(and (kind ?b doc) (not (and (kind ?b ?k) (title ?b ?k)))))
                (sorted-rows N '(and (kind ?b ?k) (not (= ?k "doc")) (= ?k doc))))
      (list (sorted (list (list d1) (list d2))) (sorted (list (list d1 'doc) (list d2 'doc)))))
(want "NOT-14 a refinement inside a not stays inside: ?v compared with an integer inside, joined with a title after, accepted"
      (q N '(and (field ?b "title" ?v) (not (= ?v 1)) (title ?x ?v)))
      (q N '(and (field ?b "title" ?v) (title ?x ?v))))
(define twice '(not (not (kind ?b doc))))
(want "NOT-15 one goal datum asked alone and then after ?b is bound to a code block: a doc exists; no code block is a doc"
      (let ((S (make-query-session (state N))))
        (in-order (let ((a (session-answer S twice))) (and (pair? a) (list-head a 2)))
                  (let ((a (session-answer S (list 'and '(kind ?b code) twice)))) (and (pair? a) (list-head a 2)))))
      '((ok (())) (ok ())))
(want "NOT-11 an unknown relation inside a not: unknown-relation, as outside"
      (head4 (q N '(and (kind ?b code) (not (nosuch ?b)))))
      '(error bad-request unknown-relation (relation nosuch)))
(want "NOT-12 a form's shape: a second argument, a non-goal, a form inside itself"
      (in-order (q N '(not (kind ?b code) (kind ?b doc)))
                (q N '(not 5))
                (head3 (on N 'query "#0=(not #0#)")))
      '((error bad-request wrong-arity (relation not) (arity 1) (given 2))
        (error bad-request not-a-term (term 5))
        (error bad-request not-a-goal)))
(want "NOT-13 every malformed goal before any binding: an unbound test in a not, then an unknown relation, answers the unknown relation"
      (head4 (q N '(and (not (= ?u 1)) (nosuch ?b))))
      '(error bad-request unknown-relation (relation nosuch)))

;; ---- AGG ------------------------------------------------------------------------------------
(printf "== AGG: count, sum, min, max ==\n")
;; docs e1 e2 e3 titled "b" "a" "c", n = 5 7 1, m = 1 "x", g = ?z ?z w;
;; code k1 cites e1 and e2, k2 cites e1, k3 cites nothing; e1 depends on e2
;; both ways (depends-on and implements), one depends row
(define A (store! "agg"))
(define e1 (make A 'doc "b"))
(define e2 (make A 'doc "a"))
(define e3 (make A 'doc "c"))
(define k1 (make A 'code "k1"))
(define k2 (make A 'code "k2"))
(define k3 (make A 'code "k3"))
(for-each (lambda (e) (link! A (car e) (cdr e))) (list (cons k1 e1) (cons k1 e2) (cons k2 e1)))
(on A 'link e1 "depends-on" e2)
(on A 'link e1 "implements" e2)
(field! A e1 'n 5)
(field! A e2 'n 7)
(field! A e3 'n 1)
(field! A e1 'm 1)
(field! A e2 'm "x")
(field! A e1 'g '?z)
(field! A e2 'g '?z)
(field! A e3 'g 'w)
(want "AGG-1 count over _ projects no column: one row present, three docs, counts 1"
      (query-rows A '(count ?n (kind _ doc)))
      '((1)))
(want "AGG-2 count over a named variable: the docs"
      (query-rows A '(count ?n (kind ?b doc)))
      '((3)))
(want "AGG-3 count grouped by a variable bound outside: each code block's cited blocks, 0 for none"
      (sorted-rows A '(and (kind ?c code) (count ?n (edge ?c cites ?d))))
      (sorted (list (list k1 2) (list k2 1) (list k3 0))))
(want "AGG-4 count over no rows: 0"
      (query-rows A '(count ?n (kind ?b nosuchkind)))
      '((0)))
(want "AGG-5 count of distinct rows, not of derivations: two blocks cite (k1 twice), one depends row (two edges)"
      (in-order (query-rows A '(count ?n (edge ?b cites _)))
                (query-rows A '(count ?n (depends ?a ?b))))
      '(((2)) ((1))))
(want "AGG-6 sum over integer values: 13"
      (query-rows A '(sum ?s ?v (field ?b "n" ?v)))
      '((13)))
(want "AGG-7 sum over no rows: 0"
      (query-rows A '(sum ?s ?v (field ?b "nosuch" ?v)))
      '((0)))
(want "AGG-8 sum over a STR column: wrong-kind"
      (q A '(sum ?s ?t (title ?b ?t)))
      '(error bad-request wrong-kind (form sum) (variable ?t) (kind STR)))
(want "AGG-9 sum over an ANY column holding a string: aggregate-value naming it"
      (q A '(sum ?s ?v (field ?b "m" ?v)))
      '(error bad-request aggregate-value (form sum) (variable ?v) (value "x")))
(want "AGG-10 min and max over integers"
      (in-order (query-rows A '(min ?m ?v (field ?b "n" ?v))) (query-rows A '(max ?m ?v (field ?b "n" ?v))))
      '(((1)) ((7))))
(want "AGG-11 min and max over strings, by bytes"
      (in-order (query-rows A '(and (kind ?b doc) (min ?m ?t (and (kind ?c doc) (title ?c ?t)))))
                (query-rows A '(max ?m ?t (and (kind ?c doc) (title ?c ?t)))))
      (list (sorted (list (list e1 "a") (list e2 "a") (list e3 "a"))) '(("c"))))
(want "AGG-12 min over no rows: no binding, no row"
      (query-rows A '(min ?m ?v (field ?b "nosuch" ?v)))
      '())
(want "AGG-13 max over no rows: no row"
      (query-rows A '(max ?m ?v (field ?b "nosuch" ?v)))
      '())
(want "AGG-14 min over integers and strings together: aggregate-value"
      (list-head (q A '(min ?m ?v (field ?b "m" ?v))) 5)
      '(error bad-request aggregate-value (form min) (variable ?v)))
(want "AGG-15 the result bound before the form: aggregate-result-bound"
      (q A '(and (field ?b "n" ?n) (count ?n (kind ?x doc))))
      '(error bad-request aggregate-result-bound (variable ?n) (form count)))
(want "AGG-16 the result inside its own goal: aggregate-result-in-goal"
      (q A '(count ?n (field ?b "n" ?n)))
      '(error bad-request aggregate-result-in-goal (variable ?n) (form count)))
(want "AGG-17 the column bound before the form: aggregate-column-bound"
      (q A '(and (field ?b "n" ?v) (sum ?s ?v (field ?c "n" ?v))))
      '(error bad-request aggregate-column-bound (variable ?v) (form sum)))
(want "AGG-18 a local of the aggregate named by a later goal: local-variable-reused"
      (q A '(and (count ?n (kind ?b doc)) (title ?b ?t)))
      '(error bad-request local-variable-reused (variable ?b) (form count)))
(want "AGG-19 a query with an aggregate reports the outer variables and the result"
      (reported A '(and (kind ?c code) (count ?n (edge ?c cites ?d))))
      '(?c ?n))
(want "AGG-20 a group whose value is the symbol ?z is seeded, never read as a variable: two blocks share it, one has w"
      (sorted-rows A '(and (field ?b "g" ?v) (count ?n (field ?c "g" ?v))))
      (sorted (list (list e1 '?z 2) (list e2 '?z 2) (list e3 'w 1))))
(derive! A 'nd '((head (nd ID INT)) (clause (nd ?c ?n) (kind ?c code) (count ?n (edge ?c cites ?d)))))
(want "AGG-21 a stored head that is an aggregate's result, called bound: the matching value answers, another does not"
      (in-order (query-rows A (list 'nd k1 2)) (query-rows A (list 'nd k1 5)))
      '((()) ()))
(want "AGG-22 a count inside a not: the code blocks whose count is not 0"
      (sorted-rows A '(and (kind ?c code) (not (and (count ?n (edge ?c cites ?d)) (= ?n 0)))))
      (sorted (list (list k1) (list k2))))
(want "AGG-23 a not inside a count's goal: the docs nothing cites"
      (query-rows A '(count ?n (and (kind ?b doc) (not (edge ?c cites ?b)))))
      '((1)))
(want "AGG-24 a column the immediate goal does not bind (a not exports nothing): aggregate-column-unbound"
      (q A '(sum ?s ?x (not (kind ?x doc))))
      '(error bad-request aggregate-column-unbound (variable ?x) (form sum)))
(want "AGG-25 a refusal through a form: aggregate-value, not emptiness; the session's spent count charged; its next query answers"
      (let* ((S (make-query-session (state A)))
             (first (session-answer S '(and (kind ?c code) (not (sum ?s ?v (field ?b "m" ?v))))))
             (spent (session-spent S))
             (next (session-answer S '(sum ?s ?v (field ?b "n" ?v)))))
        (list (head3 first) (> spent 0) (and (pair? next) (list-head next 2))))
      '((error bad-request aggregate-value) #t (ok ((13)))))
(want "AGG-26 validation before emptiness: an unknown relation after a disjoint test, inside a not"
      (head4 (q A '(not (and (= 1 "x") (nosuch ?b)))))
      '(error bad-request unknown-relation (relation nosuch)))
(want "AGG-27 validation before emptiness: an unbound test variable after a disjoint test"
      (q A '(not (and (= 1 "x") (= ?u 1))))
      '(error bad-request test-variable-unbound (variable ?u)))
(want "AGG-28 a sum over STR inside a not: wrong-kind, the form's own rule"
      (q A '(not (sum ?s ?x (title ?b ?x))))
      '(error bad-request wrong-kind (form sum) (variable ?x) (kind STR)))
(want "AGG-32 a disjoint goal inside a form does not stop the kind check: a sum over STR after it, and a column known before it"
      (in-order (q A '(not (and (= 1 "x") (sum ?s ?t (title ?b ?t)))))
                (q A '(sum ?s ?t (and (title ?b ?t) (= 1 "x"))))
                (q A '(sum ?s ?t (title 1 ?t)))
                (q A '(sum ?s ?t (and (kind ?b ?k) (title ?k ?t)))))
      '((error bad-request wrong-kind (form sum) (variable ?t) (kind STR))
        (error bad-request wrong-kind (form sum) (variable ?t) (kind STR))
        (error bad-request wrong-kind (form sum) (variable ?t) (kind STR))
        (error bad-request wrong-kind (form sum) (variable ?t) (kind STR))))
(want "AGG-33 a statically empty goal evaluates nothing: a count over an external relation beside a disjoint test is 0"
      (query-rows A '(count ?n (and (score ?b ?q ?s) (= 1 "x"))))
      '((0)))
(want "AGG-34 two forms over one group with different bodies: each doc's cited blocks and its citers, apart"
      (sorted-rows A '(and (kind ?d doc) (count ?n (edge ?d cites ?x)) (count ?m (edge ?y cites ?d))))
      (sorted (list (list e1 0 2) (list e2 0 1) (list e3 0 0))))
(derive! A 'qn '((head (qn INT)) (clause (qn ?n) (count ?n (kind ?b doc)))))
(want "AGG-35 min's result is its column's kind met with INT or STR: over an INT relation it fits an INT head; over field's ANY it does not"
      (in-order (head-of (derive! A 'mm '((head (mm INT)) (clause (mm ?m) (min ?m ?x (qn ?x))))))
                (derive! A 'mf '((head (mf INT)) (clause (mf ?m) (min ?m ?x (field ?b "n" ?x))))))
      '(ok (error bad-request head-kind (relation mf) (position 1) (declared INT) (clause (INT STR)))))
(want "AGG-36 sum, min and max over a statically empty goal: 0, no row, no row, nothing evaluated"
      (in-order (query-rows A '(sum ?s ?v (and (field ?b "n" ?v) (= 1 "x"))))
                (query-rows A '(min ?m ?v (and (field ?b "n" ?v) (= 1 "x"))))
                (query-rows A '(max ?m ?v (and (field ?b "n" ?v) (= 1 "x")))))
      '(((0)) () ()))
(want "AGG-38 sum over an INT relation: the docs counted once, 3"
      (query-rows A '(sum ?s ?n (qn ?n)))
      '((3)))
(want "AGG-39 a nested aggregate the group's values make wrong, before a contradiction: the count's goal is empty, 0, nothing evaluated"
      (in-order (sorted-rows A '(and (field ?b "m" ?v) (count ?n (and (sum ?s ?x (and (field ?c "m" ?x) (= ?x ?v))) (= 1 "x")))))
                (sorted-rows A '(and (field ?b "g" ?v) (count ?n (and (sum ?s ?x (and (field ?c "g" ?x) (= ?x ?v))) (= ?s "text"))))))
      (list (sorted (list (list e1 1 0) (list e2 "x" 0)))
            (sorted (list (list e1 '?z 0) (list e2 '?z 0) (list e3 'w 0)))))
(want "AGG-40 a nested min over a title whose own nested sum the group's value makes wrong: still a STR result, so (= ?m 0) is the contradiction, 0"
      (sorted-rows A '(and (field ?b "g" ?v) (count ?n (and (min ?m ?t (and (title ?a ?t) (sum ?s ?x (and (field ?c "g" ?x) (= ?x ?v))))) (= ?m 0)))))
      (sorted (list (list e1 '?z 0) (list e2 '?z 0) (list e3 'w 0))))
(field! A e3 'f #f)
(want "AGG-37 a value the form cannot take that is #f: aggregate-value naming it"
      (q A '(sum ?s ?v (field ?b "f" ?v)))
      '(error bad-request aggregate-value (form sum) (variable ?v) (value #f)))
(want "AGG-29 min over a SYM column: wrong-kind; count's result is INT, so joining it with a title is join-across-kinds"
      (in-order (q A '(min ?m ?k (kind ?b ?k)))
                (head4 (q A '(and (count ?n (kind ?b doc)) (title ?x ?n)))))
      '((error bad-request wrong-kind (form min) (variable ?k) (kind SYM))
        (error bad-request join-across-kinds (variable ?n))))
(want "AGG-30 an aggregate's shape: the result where a variable must be, the column likewise"
      (in-order (q A '(count _ (kind ?b doc))) (q A '(sum ?s "x" (kind ?b doc))))
      '((error bad-request not-a-term (term _)) (error bad-request not-a-term (term "x"))))
(want "AGG-31 a head variable bound only inside a not is unbound: the derive verb refuses it"
      (derive! A 'hv '((head (hv ID)) (clause (hv ?b) (not (kind ?b doc)))))
      '(error bad-request head-variable-unbound (relation hv) (variable ?b)))

;; ---- STRATIFICATION ----------------------------------------------------------------------
(printf "== STRAT: no cycle through a form ==\n")
(define (body name . goals) (list (list 'head (list name 'ID)) (cons 'clause (cons (list name '?x) goals))))
;; THE MERGED CYCLE: p and q in force over the docs; the local writer
;; redeclares p as the docs not in q, a concurrent writer q as the docs not
;; in p
(define S (store! "strata"))
(define s1 (make S 'doc "s1"))
(derive! S 'p (over-kind 'p 'doc))
(derive! S 'q (over-kind 'q 'doc))
(rule! S "needp" "--on" "doc" "--must" "(p ?w)")
(derive! S 't (body 't '(p ?x)))
(define s-past (cut-of S))
(define s-p (derive! S 'p (body 'p '(kind ?x doc) '(not (q ?x)))))
(forge! S "peerstrq" (list 'derive 'q (body 'q '(kind ?x doc) '(not (p ?x)))) s-past)
(want "STRAT-1 two writers' negations merged: p and q unstratified, each cycle from itself (the queries are read at the end)"
      (list (head-of s-p) (availability-of S 'p) (availability-of S 'q))
      '(ok (unavailable (state (unstratified (cycle p q)))) (unavailable (state (unstratified (cycle q p))))))
(want "STRAT-2 a relation reaching an unstratified one: unavailable through it"
      (availability-of S 't)
      '(unavailable (state (unstratified (cycle p q))) (through p)))
(want "STRAT-4 conflicts does not list an unstratified relation (no contest)"
      (let ((a (on S 'conflicts)))
        (and (pair? a) (eq? (car a) 'ok) (filter (lambda (x) (and (pair? x) (eq? (car x) 'derive-contested))) (cdr (cadr a)))))
      '())
;; A POSITIVE CALL DOES NOT REPAIR: q replayed as the docs IN p leaves p
;; -not-> q -> p, still a negative component
(forge-seeing! S "peerstr2" (list 'derive 'q (body 'q '(kind ?x doc) '(p ?x))))
(want "STRAT-5 q redeclared with a positive call of p: q's clause is the positive one, and both stay unstratified"
      (list (state-derived (state S) 'q) (availability-of S 'p) (availability-of S 'q))
      (list (list 'in-force (body 'q '(kind ?x doc) '(p ?x)))
            '(unavailable (state (unstratified (cycle p q)))) '(unavailable (state (unstratified (cycle q p))))))
(retire! S 'q)
(want "STRAT-6 q retired: the cycle is broken, p unavailable through q, retired"
      (availability-of S 'p)
      '(unavailable (state retired) (through q)))
(derive! S 'q (over-kind 'q 'code))
(want "STRAT-7 q redeclared naming nothing of p: p available, its rows the docs not in q"
      (list (availability-of S 'p) (query-rows S '(p ?x)))
      (list 'available (list (list s1))))
;; A CALLEE RETIRED UNDER A FORM: reported through it, not as the form's kinds
(derive! S 'qi '((head (qi INT)) (clause (qi ?n) (count ?n (kind ?b doc)))))
(derive! S 'pm '((head (pm INT)) (clause (pm ?m) (min ?m ?x (qi ?x)))))
(retire! S 'qi)
(want "STRAT-13 a callee under a min retired: the caller unavailable through it, retired"
      (availability-of S 'pm)
      '(unavailable (state retired) (through qi)))
;; AT THE VERB
(define V (store! "strata-verb"))
(make V 'doc "v1")
(derive! V 'q (over-kind 'q 'doc))
(derive! V 'p (body 'p '(kind ?x doc) '(not (q ?x))))
(want "STRAT-8 the verb refuses a declaration that closes a cycle through a not: unstratified, the cycle from the new relation"
      (derive! V 'q (body 'q '(kind ?x doc) '(not (p ?x))))
      '(error bad-request unstratified (relation q) (cycle q p)))
(want "STRAT-9 a relation naming itself under a not, and a count over its own relation: unstratified"
      (in-order (derive! V 'ps (body 'ps '(kind ?x doc) '(not (ps ?x))))
                (derive! V 'pc '((head (pc ID INT)) (clause (pc ?x ?n) (kind ?x doc) (count ?n (pc ?x ?m))))))
      '((error bad-request unstratified (relation ps) (cycle ps)) (error bad-request unstratified (relation pc) (cycle pc))))
(derive! V 'q2 (over-kind 'q2 'doc))
(derive! V 'p2 (body 'p2 '(kind ?x doc) '(not (not (q2 ?x)))))
(want "STRAT-10 under a double not the edge is still negative: q2 naming p2 closes an unstratified cycle"
      (derive! V 'q2 (body 'q2 '(kind ?x doc) '(p2 ?x)))
      '(error bad-request unstratified (relation q2) (cycle q2 p2)))
;; A POSITIVE CYCLE WITH A NEGATION LEAVING IT, and a three-cycle with one
;; negation, built by one forged segment each
(define W (store! "strata-shapes"))
(define w1 (make W 'doc "w1"))
(define (forge-many! st peer payloads)
  (let* ((w (local-writer st)) (past (cut-of st))
         (bytes (let loop ((ps payloads) (i 1) (acc '()))
                  (if (null? ps)
                      (let ((bvs (reverse acc)))
                        (let ((out (make-bytevector (apply + (map bytevector-length bvs)))))
                          (let fill ((bvs bvs) (k 0))
                            (if (null? bvs) out
                                (begin (bytevector-copy! (car bvs) 0 out k (bytevector-length (car bvs)))
                                       (fill (cdr bvs) (+ k (bytevector-length (car bvs)))))))))
                      (loop (cdr ps) (+ i 1)
                            (cons (encode-record i 1789000000001 "peer" (list (cons w (cdr (assoc w past)))) (storable-encode (car ps)))
                                  acc))))))
    (log-publish! st peer 1 bytes (segment-sha bytes))))
(forge-many! W "peershap"
             (list (list 'derive 'nc (over-kind 'nc 'code))
                   (list 'derive 'ca (body 'ca '(kind ?x doc) '(cb ?x)))
                   (list 'derive 'cb (body 'cb '(kind ?x doc) '(ca ?x) '(not (nc ?x))))
                   (list 'derive 'x3 (body 'x3 '(kind ?x doc) '(y3 ?x)))
                   (list 'derive 'y3 (body 'y3 '(kind ?x doc) '(z3 ?x)))
                   (list 'derive 'z3 (body 'z3 '(kind ?x doc) '(not (x3 ?x))))))
(want "STRAT-11 a positive cycle whose negation leaves it: available, and right"
      (list (availability-of W 'ca) (availability-of W 'cb) (query-rows W '(cb ?x)))
      (list 'available 'available '()))
(want "STRAT-12 a three-cycle with one negation: every member unstratified, each cycle from itself in callee order"
      (map (lambda (r) (availability-of W r)) '(x3 y3 z3))
      '((unavailable (state (unstratified (cycle x3 y3 z3))))
        (unavailable (state (unstratified (cycle y3 z3 x3))))
        (unavailable (state (unstratified (cycle z3 x3 y3))))))

;; ---- COMPLETION ----------------------------------------------------------------------------
(printf "== COMP: a form's goal completed before it is read ==\n")
;; a -> b -> c -> d by cites; reach left-recursive, so its first expansion
;; cannot finish the closure
(define C (store! "complete"))
(define ca (make C 'doc "a"))
(define cb (make C 'doc "b"))
(define cc (make C 'doc "c"))
(define cd (make C 'doc "d"))
(for-each (lambda (e) (link! C (car e) (cdr e))) (list (cons ca cb) (cons cb cc) (cons cc cd)))
(derive! C 'reach '((head (reach ID ID)) (clause (reach ?a ?b) (reach ?a ?m) (edge ?m cites ?b)) (clause (reach ?a ?b) (edge ?a cites ?b))))
(derive! C 'rq (list '(head (rq ID)) (list 'clause '(rq ?x) (list 'reach ca '?x))))
(derive! C 'pp '((head (pp ID)) (clause (pp ?x) (kind ?x doc) (not (rq ?x)))))
(want "STRAT-14 a stratified chain: pp the docs not in rq, rq what a reaches through the recursive reach: available, a alone"
      (list (availability-of C 'pp) (query-rows C '(pp ?x)))
      (list 'available (list (list ca))))
(want "COMP-1 a not over a left-recursive relation: a reaches d, so no row"
      (query-rows C (list 'and '(kind ?x doc) (list '= '?x ca) (list 'not (list 'reach '?x cd))))
      '())
(want "COMP-2 a count over it: three, not the first pass's one"
      (query-rows C (list 'count '?n (list 'reach ca '?x)))
      '((3)))
(want "COMP-3 a group completed once: the key's expansions are the nested loop's passes, the same for a second outer binding of that group"
      (let* ((S (make-query-session (state C)))
             (a (session-answer S (list 'and '(kind ?x doc) (list '= '?x ca) (list 'count '?n (list 'reach '?x '?y)))))
             (once (session-expansions S 'reach ca '?y))
             (b (session-answer S (list 'and '(kind ?z doc) (list '= '?z ca) (list 'count '?k (list 'reach '?z '?w))))))
        (list (and (pair? a) (list-head a 2)) once (and (pair? b) (list-head b 2)) (session-expansions S 'reach ca '?y)))
      (list (list 'ok (list (list ca 3))) 4 (list 'ok (list (list ca 3))) 4))
;; THE BUDGET IS WHAT THE FACTS COST ALONE (the kind and edge builds, a query
;; with no row) and one tuple more, so the negated relation's completion
;; crosses it; raised, the same session answers the same goal whole
(want "COMP-4 a negated relation whose completion exceeds the budget: query-budget naming it; the same session, its budget raised, answers it whole"
      (let* ((facts (let ((S0 (make-query-session (state C))))
                      (session-answer S0 '(and (kind ?x doc) (edge ?a cites ?b) (= ?a "no such block")))
                      (session-spent S0)))
             (S (make-query-session (state C) '() (+ facts 1)))
             (goal (list 'and '(kind ?x doc) (list '= '?x ca) (list 'not (list 'reach '?x cd))))
             (refused (head3 (session-answer S goal))))
        (session-budget-set! S 1000000)
        (in-order refused (let ((a (session-answer S goal))) (and (pair? a) (list-head a 2)))
                  (let ((a (session-answer S (list 'count '?n (list 'reach ca '?y))))) (and (pair? a) (list-head a 2)))))
      '((error query-budget (relation reach)) (ok ()) (ok ((3)))))

(want "COMP-5 several groups share one budget: what one group costs fits, the four docs' groups together do not"
      (let* ((one (let ((S0 (make-query-session (state C))))
                    (session-answer S0 (list 'and '(kind ?x doc) (list '= '?x ca) (list 'count '?n (list 'reach '?x '?y))))
                    (session-spent S0)))
             (S (make-query-session (state C) '() (+ one 1))))
        (head-of (session-answer S '(and (kind ?x doc) (count ?n (reach ?x ?y))))))
      '(error query-budget))

;; ---- RULE VALUES ---------------------------------------------------------------------------
(printf "== RV: forms in rule values ==\n")
;; A WRITE RULE READS THE STATE BEFORE THE WRITE: the doc given its phase
;; before the rule is the one a write may touch
(define R (store! "rules"))
(define rd (make R 'doc "d"))
(define rp (make R 'doc "p"))
(field! R rp 'phase "draft")
(rule! R "phased" "--on" "doc" "--must-not" "(and (kind+ ?w doc) (not (field ?w \"phase\" _)))")
(want "RV-1 a must-not with a not: a doc without a phase written is refused; one with a phase, judged and accepted"
      (in-order (head3 (title! R rd "d2")) (head-of (title! R rp "p2")))
      '((error refused rule-violation) ok))
(rule! R "phased" "--retire")
(want "RV-2 a must with a not of a per-write fact is a WRITE rule, and judges a write"
      (in-order (head-of (rule! R "unreadw" "--on" "doc" "--must" "(not (unread ?w))"))
                (and (state-rule (state R) 'unreadw) (car (car (cadr (state-rule (state R) 'unreadw)))))
                (cadr (car (cadr (state-rule (state R) 'unreadw))))
                (head-of (title! R rd "d4")))
      '(ok class write ok))
(rule! R "unreadw" "--retire")
(want "RV-5 a per-write fact inside a not is the write's: a doc write meets kind+ there (must-not (not ...) holds); must (not ...) is refused"
      (in-order (head-of (rule! R "kindin" "--on" "doc" "--must-not" "(not (kind+ ?w doc))"))
                (head-of (title! R rp "p3"))
                (head-of (rule! R "kindin" "--retire"))
                (head-of (rule! R "kindout" "--on" "doc" "--must" "(not (kind+ ?w doc))"))
                (head3 (title! R rp "p4"))
                (head-of (rule! R "kindout" "--retire")))
      '(ok ok ok ok (error refused rule-violation) ok))
;; A STORED NAME ONLY INSIDE A RULE'S FORM: the rule is re-checked with the
;; kinds when it is evaluated, so rk redeclared from INT to STR makes its sum
;; wrong-kind (without the recheck it would be evaluated, aggregate-value)
(derive! R 'rk '((head (rk INT)) (clause (rk 1))))
(rule! R "inform" "--on" "doc" "--must" "(sum ?s ?x (rk ?x))")
(derive! R 'rk '((head (rk STR)) (clause (rk "a"))))
(want "RV-6 a stored relation named only inside a rule's sum gets the kind recheck: rk redeclared STR, the rule unevaluable, wrong-kind"
      (let ((a (title! R rp "p5")))
        (list (head3 a) (and (clause-of a 'reason) (head3 (cadr (clause-of a 'reason))))))
      '((error refused rule-unevaluable) (error bad-request wrong-kind)))
(rule! R "inform" "--retire")
(want "RV-7 a library relation reaching an external, inside an aggregate: rule-relation-not-allowed"
      (rule! R "amb" "--on" "doc" "--must" "(count ?n (ambiguous ?w ?name))")
      '(error bad-request rule-relation-not-allowed (relation ambiguous)))
(rule! R "cites-read" "--on" "doc" "--must-not" "(and (cited ?w ?s) (not (unread ?s)))")
(want "RV-8 a per-write fact the write fills inside a not: a depends-on link with no receipt makes its target unread, so the must-not holds"
      (in-order (head-of (on R 'link rp "depends-on" rd)) (head-of (rule! R "cites-read" "--retire")))
      '(ok ok))
(forge! R "rulefrmz" '(rule legacyform ((class state) (on doc) (must-not (sum ?s ?t (title ?w ?t))))) (cut-of R))
(want "RV-9 describe lists a stored rule whose form fails the kinds with that refusal"
      (let* ((d (assq 'declared-rules (cdr (rpc-dispatch R '(describe) "author" (state R)))))
             (r (and d (assq 'legacyform (cdr d)))))
        (and r (cddr r)))
      '((kind-mismatch (error bad-request wrong-kind (form sum) (variable ?t) (kind STR)))))
(rule! R "legacyform" "--retire")
(want "RV-3 an external relation inside an aggregate, in a rule and in a stored relation: rule-relation-not-allowed"
      (in-order (rule! R "scored" "--on" "doc" "--must" "(count ?n (score ?w \"x\" ?s))")
                (derive! R 'ps '((head (ps INT)) (clause (ps ?n) (count ?n (and (kind ?b doc) (score ?b "x" ?s)))))))
      '((error bad-request rule-relation-not-allowed (relation score))
        (error bad-request rule-relation-not-allowed (relation score))))
(want "RV-4 a where with a count, judged: a write leaving two docs is accepted, the insert of a third refused"
      (in-order (head-of (rule! R "fewdocs" "--on" "doc" "--where" "(and (count ?n (kind ?b doc)) (= ?n 3))" "--must" "(kind ?w code)"))
                (head-of (title! R rd "d5"))
                (let ((a (batch R (list 'insert 'root #f '((kind . doc) (title . "e"))))))
                  (head3 (if (and (pair? a) (eq? (car a) 'batch) (pair? (cadr a))) (car (cadr a)) a))))
      '(ok ok (error refused rule-violation)))

;; ---- LIBRARY and NAMES -----------------------------------------------------------------------
(printf "== LIB: the library stays positive; the five names are no relation's ==\n")
(want "LIB-1 a library clause with a not does not load: unknown relation"
      (guard (e ((and (condition? e) (message-condition? e)) (condition-message e)))
        (check-rules (append rule-library (list '((usesnot ?x) (kind ?x doc) (not (kind ?x code))))))
        'loaded)
      "unknown relation")
(want "LIB-4 a library clause headed by any form's name does not load; no fact relation is named like one"
      (in-order (map (lambda (nm)
                       (guard (e ((and (condition? e) (message-condition? e)) (condition-message e)))
                         (check-rules (append rule-library (list (list (list nm '?x) '(kind ?x doc)))))
                         'loaded))
                     '(not count sum min max))
                (filter (lambda (nm) (assq nm fact-relations)) '(not count sum min max)))
      '(("a rule heads a form" "a rule heads a form" "a rule heads a form" "a rule heads a form" "a rule heads a form") ()))
(define L (store! "names"))
(make L 'doc "l")
(want "LIB-2 the five form heads refused as a stored relation's name: name-taken, where form"
      (map (lambda (nm) (derive! L nm (over-kind nm 'doc))) '(not count sum min max))
      (map (lambda (nm) (list 'error 'bad-request 'name-taken (list 'name nm) '(where form))) '(not count sum min max)))
(forge! L "peernmcn" (list 'derive 'count (over-kind 'count 'doc)) (cut-of L))
(want "LIB-3 a record that bypassed the verb under a form's name is masked: listed shadowed, the form still answers"
      (list (availability-of L 'count) (query-rows L '(count ?n (kind ?b doc))))
      '((unavailable (shadowed FORM)) ((1))))

;; ---- EVALUATION OF AN UNSTRATIFIED PAIR, last ----------------------------------------------
;; A query, a write and the audit that would evaluate p and q, each refused
;; before any evaluation; without the stratification they loop, so they come
;; after every other row
(printf "== STRAT-E: an unstratified pair refused before evaluation ==\n")
(define SE (store! "strata-eval"))
(define se1 (make SE 'doc "se1"))
(derive! SE 'p (over-kind 'p 'doc))
(derive! SE 'q (over-kind 'q 'doc))
(rule! SE "needp" "--on" "doc" "--must" "(p ?w)")
(define se-past (cut-of SE))
(derive! SE 'p (body 'p '(kind ?x doc) '(not (q ?x))))
(forge! SE "peerseqq" (list 'derive 'q (body 'q '(kind ?x doc) '(not (p ?x)))) se-past)
(want "STRAT-E1 a query of either relation of the pair: relation-unavailable, unstratified"
      (in-order (q SE '(p ?x)) (q SE '(q ?x)))
      '((error bad-request relation-unavailable (relation p) (state (unstratified (cycle p q))))
        (error bad-request relation-unavailable (relation q) (state (unstratified (cycle q p))))))
(want "STRAT-E2 a rule naming p: the next write it judges is rule-unevaluable, with that state"
      (let ((a (title! SE se1 "se1b")))
        (list (head3 a) (clause-of a 'reason)))
      '((error refused rule-unevaluable)
        (reason (error bad-request relation-unavailable (relation p) (state (unstratified (cycle p q)))))))
(want "STRAT-E3 check's audit lists the rule as rule-unevaluable, naming p's state"
      ;; check answers (check ... (rules <finding> ...) ...)
      (let ((rules (clause-of (on SE 'check) 'rules)))
        (and rules (find (lambda (x) (and (pair? x) (eq? (car x) 'rule-unevaluable))) (cdr rules))))
      '(rule-unevaluable (rule needp) (reason (error bad-request relation-unavailable (relation p) (state (unstratified (cycle p q)))))))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nquery-strata complete\n" rows bad)
(exit (if (zero? bad) 0 1))
