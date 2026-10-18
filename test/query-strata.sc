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
        (only (theourgia query) make-query-session session-answer session-spent session-expansions check-rules rule-library)
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
(define (forge! st peer payload past)
  (let* ((w (local-writer st))
         (bytes (encode-record 1 1789000000001 "peer" (list (cons w (cdr (assoc w past)))) (storable-encode payload))))
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
(want "NOT-5 a not of a test with both operands bound: the ordered pairs of distinct code blocks"
      (length (query-rows N '(and (kind ?x code) (kind ?y code) (not (= ?x ?y)))))
      20)
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
      (length (query-rows N '(and (kind ?b ?k) (not (= ?k "doc")))))
      7)
(want "NOT-10 a disjoint join inside a not: accepted, the docs answered; and a kind after the not is what it was before"
      (in-order (sorted-rows N '(and (kind ?b doc) (not (and (kind ?b ?k) (title ?b ?k)))))
                (sorted-rows N '(and (kind ?b ?k) (not (= ?k "doc")) (= ?k doc))))
      (list (sorted (list (list d1) (list d2))) (sorted (list (list d1 'doc) (list d2 'doc)))))
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
(want "STRAT-1 two writers' negations merged: p and q unstratified, each cycle from itself, a query of either refused"
      (list (head-of s-p) (availability-of S 'p) (availability-of S 'q) (q S '(p ?x)))
      '(ok (unavailable (state (unstratified (cycle p q)))) (unavailable (state (unstratified (cycle q p))))
        (error bad-request relation-unavailable (relation p) (state (unstratified (cycle p q))))))
(want "STRAT-2 a relation reaching an unstratified one: unavailable through it"
      (availability-of S 't)
      '(unavailable (state (unstratified (cycle p q))) (through p)))
(want "STRAT-3 a rule naming an unstratified relation: the next write it judges is rule-unevaluable, with that state"
      (let ((a (title! S s1 "s1b")))
        (list (head3 a) (clause-of a 'reason)))
      '((error refused rule-unevaluable)
        (reason (error bad-request relation-unavailable (relation p) (state (unstratified (cycle p q)))))))
(want "STRAT-4 conflicts does not list an unstratified relation (no contest)"
      (let ((a (on S 'conflicts)))
        (and (pair? a) (eq? (car a) 'ok) (filter (lambda (x) (and (pair? x) (eq? (car x) 'derive-contested))) (cdr (cadr a)))))
      '())
;; A POSITIVE CALL DOES NOT REPAIR: q replayed as the docs IN p leaves p
;; -not-> q -> p, still a negative component
(forge! S "peerstr2" (list 'derive 'q (body 'q '(kind ?x doc) '(p ?x))) (cut-of S))
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
        (list (and (pair? a) (list-head a 2)) once (session-expansions S 'reach ca '?y)))
      (list (list 'ok (list (list ca 3))) 4 4))
(want "COMP-4 a negated relation whose completion exceeds the budget: query-budget naming it"
      (let ((S (make-query-session (state C) '() 5)))
        (head3 (session-answer S (list 'and '(kind ?x doc) (list '= '?x ca) (list 'not (list 'reach '?x cd))))))
      '(error query-budget (relation reach)))

;; ---- RULE VALUES ---------------------------------------------------------------------------
(printf "== RV: forms in rule values ==\n")
(define R (store! "rules"))
(define rd (make R 'doc "d"))
(rule! R "phased" "--on" "doc" "--must-not" "(and (kind+ ?w doc) (not (field ?w \"phase\" _)))")
(want "RV-1 a must-not with a not: a doc written without a phase is refused; with one, judged and accepted"
      (in-order (head3 (title! R rd "d2"))
                (head-of (batch R (list 'set rd 'phase "draft") (list 'set rd 'title "d3"))))
      '((error refused rule-violation) batch))
(rule! R "phased" "--retire")
(want "RV-2 a must with a not of a per-write fact is a WRITE rule, and judges a write"
      (in-order (head-of (rule! R "unreadw" "--on" "doc" "--must" "(not (unread ?w))"))
                (and (state-rule (state R) 'unreadw) (car (car (cadr (state-rule (state R) 'unreadw)))))
                (cadr (car (cadr (state-rule (state R) 'unreadw))))
                (head-of (title! R rd "d4")))
      '(ok class write ok))
(rule! R "unreadw" "--retire")
(want "RV-3 an external relation inside an aggregate, in a rule and in a stored relation: rule-relation-not-allowed"
      (in-order (rule! R "scored" "--on" "doc" "--must" "(count ?n (score ?w \"x\" ?s))")
                (derive! R 'ps '((head (ps INT)) (clause (ps ?n) (count ?n (and (kind ?b doc) (score ?b "x" ?s)))))))
      '((error bad-request rule-relation-not-allowed (relation score))
        (error bad-request rule-relation-not-allowed (relation score))))
(want "RV-4 a where with a count, judged: a write leaving one doc is accepted, the insert of a second refused"
      (in-order (head-of (rule! R "fewdocs" "--on" "doc" "--where" "(and (count ?n (kind ?b doc)) (= ?n 2))" "--must" "(kind ?w code)"))
                (head-of (title! R rd "d5"))
                (let ((a (batch R (list 'insert 'root #f '((kind . doc) (title . "e"))))))
                  (and (pair? a) (eq? (car a) 'batch) (head3 (car (cadr a))))))
      '(ok ok (error refused rule-violation)))

;; ---- LIBRARY and NAMES -----------------------------------------------------------------------
(printf "== LIB: the library stays positive; the five names are no relation's ==\n")
(want "LIB-1 a library clause with a not does not load: unknown relation"
      (guard (e ((and (condition? e) (message-condition? e)) (condition-message e)))
        (check-rules (append rule-library (list '((usesnot ?x) (kind ?x doc) (not (kind ?x code))))))
        'loaded)
      "unknown relation")
(define L (store! "names"))
(make L 'doc "l")
(want "LIB-2 the five form heads refused as a stored relation's name: name-taken, where form"
      (map (lambda (nm) (derive! L nm (over-kind nm 'doc))) '(not count sum min max))
      (map (lambda (nm) (list 'error 'bad-request 'name-taken (list 'name nm) '(where form))) '(not count sum min max)))
(forge! L "peernmcn" (list 'derive 'count (over-kind 'count 'doc)) (cut-of L))
(want "LIB-3 a record that bypassed the verb under a form's name is masked: listed shadowed, the form still answers"
      (list (availability-of L 'count) (query-rows L '(count ?n (kind ?b doc))))
      '((unavailable (shadowed FORM)) ((1))))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nquery-strata complete\n" rows bad)
(exit (if (zero? bad) 0 1))
