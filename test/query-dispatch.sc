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

;; The evaluator's goal dispatch (query.sc, "the handlers"): each goal is
;; answered by the one handler of its environment that applies to its
;; relation -- TEST, FACT, WRITE-FACT, RULE -- in three environments
;; (library load, a rule value, a session). The rows pin the environments'
;; exclusivity and its four declared refusals, each route's unknown-relation
;; answer and the two-pass order, the session's recheck in its own
;; environment, and that the dispatch is real: a handler a fixture registers
;; through the seam (extra-handlers) is answered, and one overlapping a
;; built-in handler is ambiguous-goal at the dispatcher and through solve.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia query) fact-relations rule-library rule-relations rule-only-facts check-rules
              rule-value-check make-query-session session-answer refusal? refusal-answer
              make-handler handler-name dispatch extra-handlers library-env rule-value-env session-env-of
              session-check session-evaluate)
        (only (theourgia store) open-and-reduce))

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
;; a refusal raised by the query library, as its answer; anything else
;; passes through the row's own guard
(define-syntax refused
  (syntax-rules ()
    ((_ e) (guard (c ((refusal? c) (refusal-answer c))) e))))
;; an assertion's message and irritants, or ACCEPTED
(define-syntax asserted
  (syntax-rules ()
    ((_ e) (guard (c ((and (condition? c) (message-condition? c))
                      (list (condition-message c) (if (irritants-condition? c) (condition-irritants c) '()))))
             e
             'ACCEPTED))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/query-dispatch-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define S (string-append root "/s"))
(define (run . args) (rpc-dispatch S args "author"))
(define (state) (open-and-reduce S))
(define (head-of a) (if (and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)) (and (pair? a) (car a))))
;; a query answer's rows: (ok (items (row <value> ...) ...) ...)
(define (rows-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a))) (map cdr (cdr (cadr a))) a))
(define (session-of . options) (apply make-query-session (state) options))

(register-verbs! extension-verbs)
(run 'init)
(run 'insert "--title" "A")

;; the names of each table, and the library's heads
(define test-names '(= /= string< member))
(define fact-names (map car fact-relations))
(define rule-only-names (map car rule-only-facts))
(define library-heads (map car (rule-relations rule-library)))
;; the per-write facts a rule check's session has: every rule-only fact, no rows
(define write-facts (map (lambda (f) (list (car f) (cadr f))) rule-only-facts))

;; ---- DISPATCH-1: each environment's handlers are exclusive ---------------------
(printf "== DISPATCH-1: each environment's handlers are exclusive ==\n")
;; EACH NAME'S HANDLER IN ENV, by name, or #f: a name two handlers answered
;; would raise ambiguous-goal here
(define (handlers-of env names) (map (lambda (n) (let ((h (dispatch env n))) (and h (handler-name h)))) names))
(define (expected-of test fact rule-only rule)
  (append (map (lambda (n) test) test-names) (map (lambda (n) fact) fact-names)
          (map (lambda (n) rule-only) rule-only-names) (map (lambda (n) rule) library-heads)))
(define all-names (append test-names fact-names rule-only-names library-heads))
(want "DISPATCH-1 library load: TEST, FACT and RULE, one each; a per-write fact is unknown there"
      (refused (handlers-of (library-env rule-library) all-names))
      (expected-of 'TEST 'FACT #f 'RULE))
(want "DISPATCH-1 a rule value: TEST, FACT, WRITE-FACT over the rule-only facts, RULE over the library, one each"
      (refused (handlers-of (rule-value-env) all-names))
      (expected-of 'TEST 'FACT 'WRITE-FACT 'RULE))
(want "DISPATCH-1 a session with per-write facts: TEST, FACT, WRITE-FACT, RULE, one each"
      (refused (handlers-of (session-env-of (session-of '() 1000000 #f write-facts)) all-names))
      (expected-of 'TEST 'FACT 'WRITE-FACT 'RULE))

;; THE EXTRA CLAUSES' ENVIRONMENT HAS NO WRITE-FACT: a clause reading a
;; per-write fact is an unknown relation there, even beside that fact
(want "DISPATCH-1 an extra clause reading a per-write fact, the fact given: unknown relation, the clause its irritant"
      (asserted (session-of (list '((probe ?x) (unread ?x))) 1000000 #f '((unread 1 ("x")))))
      (list "unknown relation" (list '((probe ?x) (unread ?x)))))

;; ---- DISPATCH-2: the declared acceptance changes at load and at session creation ----
(printf "== DISPATCH-2, DISPATCH-7: the declared acceptance changes ==\n")
(want "DISPATCH-2 an extra clause heading a per-write fact beside it: ambiguous-relation, WRITE-FACT and RULE"
      (refused (session-of (list '((unread ?x) (kind ?x section))) 1000000 #f '((unread 1 ("x")))))
      '(error bad-request ambiguous-relation (relation unread) (handlers (WRITE-FACT RULE))))
(want "DISPATCH-2b a library clause heading a per-write fact is refused when the library is checked"
      (asserted (check-rules (append rule-library (list '((unread ?x) (kind ?x section))))))
      (list "a rule heads a per-write fact" (list '((unread ?x) (kind ?x section)))))
(want "DISPATCH-2b CONTROL: an extra clause heading a rule-only fact's name, no such per-write fact given, is accepted"
      (let ((r (refused (session-of (list '((unread ?x) (kind ?x _))) 1000000 #f '()))))
        (if (and (pair? r) (eq? (car r) 'error)) r 'accepted))
      'accepted)
(want "DISPATCH-7 a per-write fact named like a fact relation: ambiguous-relation, FACT and WRITE-FACT"
      (refused (session-of '() 1000000 #f '((kind 2 ("x" section)))))
      '(error bad-request ambiguous-relation (relation kind) (handlers (FACT WRITE-FACT))))
(want "DISPATCH-7 a per-write fact named like a test: ambiguous-relation, TEST and WRITE-FACT"
      (refused (session-of '() 1000000 #f '((member 2))))
      '(error bad-request ambiguous-relation (relation member) (handlers (TEST WRITE-FACT))))
(define a-head (car (rule-relations rule-library)))
(want "DISPATCH-7 a per-write fact named like a library rule's head: ambiguous-relation, WRITE-FACT and RULE"
      (refused (session-of '() 1000000 #f (list (list (car a-head) (cadr a-head)))))
      (list 'error 'bad-request 'ambiguous-relation (list 'relation (car a-head)) '(handlers (WRITE-FACT RULE))))

;; A STORED CONSTRAINT'S NAME IS NOT A RELATION: constraints named kind and
;; member are accepted, and the relations keep answering
(want "DISPATCH-7 CONTROL: constraints named kind and member are stored, and kind and member still answer as relation and test"
      (in-order (head-of (run 'rule "kind" "--on" "section" "--must-not" "(kind ?w code)"))
                (head-of (run 'rule "member" "--on" "section" "--must-not" "(member ?w (\"zzz\"))"))
                (pair? (rows-of (run 'query "(kind ?b ?k)")))
                (pair? (rows-of (run 'query "(and (kind ?b ?k) (member ?k (section)))"))))
      '(ok ok #t #t))

;; ---- DISPATCH-4: an unknown relation, in each route's form ----------------------------
(printf "== DISPATCH-4: an unknown relation in each route ==\n")
(want "DISPATCH-4 the query verb: unknown-relation, the known list in its order (facts, the library's rules, the tests)"
      (run 'query "(nosuch ?x)")
      (list 'error 'bad-request 'unknown-relation '(relation nosuch) (list 'known (append fact-names library-heads test-names))))
(want "DISPATCH-4 the rule verb's check: unknown-relation, the rule-only facts among the rules"
      (rule-value-check '((on section) (must (nosuch ?w))))
      (list 'error 'bad-request 'unknown-relation '(relation nosuch)
            (list 'known (append fact-names library-heads rule-only-names test-names))))
(want "DISPATCH-4 the library check: the assertion unknown relation, the enclosing rule its irritant"
      (asserted (check-rules (append rule-library (list '((probe ?x) (nosuch ?x))))))
      (list "unknown relation" (list '((probe ?x) (nosuch ?x)))))
(want "DISPATCH-4 the two passes: an unknown relation is reported before a test's unbound variable"
      (let ((a (run 'query "(and (= ?x \"x\") (nosuch ?x))"))) (and (pair? a) (list (car a) (cadr a) (caddr a) (cadddr a))))
      '(error bad-request unknown-relation (relation nosuch)))

(want "DISPATCH-4 a session with per-write facts and an extra clause: the known list holds the extra head, not the per-write facts"
      (session-answer (session-of (list '((probe2 ?x) (kind ?x section))) 1000000 #f '((unread 1 ("x")))) '(nosuch ?x))
      (list 'error 'bad-request 'unknown-relation '(relation nosuch) (list 'known (append fact-names library-heads '(probe2) test-names))))
(want "DISPATCH-4 the kinds after the binding, as before: a test's unbound variable is reported before a wrong kind"
      (run 'query "(and (= ?x \"x\") (kind ?b \"code\"))")
      '(error bad-request test-variable-unbound (variable ?x)))

;; ---- DISPATCH-5: the dispatch is real --------------------------------------------------
(printf "== DISPATCH-5: a handler registered through the seam is answered ==\n")
;; probe-rel/1: one tuple, ("probe"); its goal's argument a variable
(define probe
  (make-handler 'PROBE (lambda (rel) (eq? rel 'probe-rel)) (lambda (rel) 1) (lambda (rel) #f) (lambda (rel) #f) #f #t
                (lambda (S g b rest) (rest (cons (cons (cadr g) "probe") b)))
                (lambda (g seen) #f)))
(want "DISPATCH-5 the query verb answers probe-rel through the seam, in process"
      (rows-of (parameterize ((extra-handlers (list probe))) (run 'query "(probe-rel ?x)")))
      '(("probe")))
(want "DISPATCH-5 with the seam unbound the same query is unknown-relation"
      (let ((a (run 'query "(probe-rel ?x)"))) (and (pair? a) (list (car a) (cadr a) (caddr a) (cadddr a))))
      '(error bad-request unknown-relation (relation probe-rel)))

;; ---- DISPATCH-6: exactly one handler, at the dispatcher and through solve --------------
(printf "== DISPATCH-6: an overlapping handler is ambiguous-goal ==\n")
(define over
  (make-handler 'OVER (lambda (rel) (eq? rel 'kind)) (lambda (rel) 2) (lambda (rel) #f) (lambda (rel) #f) #f #t
                (lambda (S g b rest) (rest b))
                (lambda (g seen) #f)))
(want "DISPATCH-6 the dispatcher itself, on kind with an overlapping handler registered past the validator: ambiguous-goal"
      (refused (parameterize ((extra-handlers (list over))) (dispatch (library-env rule-library) 'kind)))
      '(error ambiguous-goal (relation kind) (handlers (FACT OVER))))
(define S6 (session-of '()))
(define goals6 (session-check S6 '(kind ?b ?k)))
(want "DISPATCH-6 through solve, the static pass already run, the seam bound before the evaluation: ambiguous-goal"
      (in-order goals6 (refused (parameterize ((extra-handlers (list over))) (call-with-values (lambda () (session-evaluate S6 goals6)) list))))
      (list '((kind ?b ?k)) '(error ambiguous-goal (relation kind) (handlers (FACT OVER)))))

;; ---- DISPATCH-8: the session's recheck in its own environment --------------------------
(printf "== DISPATCH-8: a session with per-write facts answers them ==\n")
(want "DISPATCH-8 a session built with per-write facts answers (unread ?x) through session-answer"
      (let ((a (session-answer (session-of '() 1000000 #f '((unread 1 ("b1")))) '(unread ?x))))
        (if (and (pair? a) (eq? (car a) 'ok)) (list (car a) (cadr a) (caddr a)) a))
      '(ok (("b1")) (?x)))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nquery-dispatch complete\n" rows bad)
(exit (if (zero? bad) 0 1))
