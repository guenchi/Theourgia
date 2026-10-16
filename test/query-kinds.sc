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

;; The column kinds of the query and rule language (query.sc, "the kinds"):
;; the goals refused because two kinds that must share a value are
;; disjoint, each in a query and inside a rule value; the goals that must
;; keep answering, read statically and through the verb on a store; every
;; query and rule example of the README; the library rules' inferred and
;; declared kinds; a rule stored before the kinds, which a write must still
;; accept; and the two verbs' wiring.

(import (chezscheme) (theourgia rpc)
        (only (theourgia query) query-kind-refusal rule-value-check rule-kinds declared-rule-kinds
              rule-library rule-relations library-relation-kinds refusal? refusal-answer)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut block-id)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha))

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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/query-kinds-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define S (string-append root "/s"))
(define (run . args) (rpc-dispatch S args "author"))
(define (state) (open-and-reduce S))
(define (head-of a) (if (and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)) (and (pair? a) (car a))))
;; a rule value holding GOAL as its one must-not, on sections
(define (in-rule goal) (list '(on section) (list 'must-not goal)))
;; what the `rule` verb's check answers for it: the refusal, or ok
(define (rule-answer goal) (let ((v (rule-value-check (in-rule goal)))) (if (and (pair? v) (eq? (car v) 'error)) v 'ok)))
;; a refusal for a kind, as opposed to any other refusal
(define (kind-refusal? a) (and (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a))
                               (memq (caddr a) '(wrong-kind join-across-kinds)) #t))

(run 'init)

;; ---- REFUSE: disjoint kinds, each in a query and inside a rule value ---------------------
(printf "== REFUSE: a goal with disjoint kinds is refused by name ==\n")
(define refused
  '(("REFUSE-1 C1 a string into kind's symbol column" (kind ?b "code")
     (error bad-request wrong-kind (relation kind) (position 2) (column SYM) (got "code")))
    ("REFUSE-2 C1 a symbol into field's name column" (field ?b title ?v)
     (error bad-request wrong-kind (relation field) (position 2) (column STR) (got title)))
    ("REFUSE-3 C1 a symbol into under's parent column" (under ?b root)
     (error bad-request wrong-kind (relation under) (position 2) (column PARENT) (got root)))
    ("REFUSE-4 C1 a string into validity's symbol set" (validity ?b "valid")
     (error bad-request wrong-kind (relation validity) (position 2) (column (SYM valid needs-review refuted superseded)) (got "valid")))
    ("REFUSE-5 C1 an integer into version's string column" (version ?b 12)
     (error bad-request wrong-kind (relation version) (position 2) (column STR) (got 12)))
    ("REFUSE-6 C1 a symbol into title's string column" (title ?b code)
     (error bad-request wrong-kind (relation title) (position 2) (column STR) (got code)))
    ("REFUSE-7 C1 a string into moved's end column" (moved ?a ?r ?b "target")
     (error bad-request wrong-kind (relation moved) (position 4) (column (SYM source target)) (got "target")))
    ("REFUSE-9 C2 a symbol column joined with a string column" (and (kind ?b ?k) (title ?x ?k))
     (error bad-request join-across-kinds (variable ?k) (positions ((kind 2) (title 2))) (kinds (SYM STR))))
    ("REFUSE-10 C2 a symbol column joined with a parent column" (and (kind ?b ?k) (under ?x ?k))
     (error bad-request join-across-kinds (variable ?k) (positions ((kind 2) (under 2))) (kinds (SYM PARENT))))
    ("REFUSE-11 C2 a string column joined with a symbol column" (and (version ?b ?h) (relation ?h _))
     (error bad-request join-across-kinds (variable ?h) (positions ((version 2) (relation 1))) (kinds (STR SYM))))
    ("REFUSE-12 C2 ANY narrowed to STR, then met with SYM" (and (field ?b "slot" ?v) (title ?x ?v) (kind ?y ?v))
     (error bad-request join-across-kinds (variable ?v) (positions ((field 3) (title 2) (kind 2))) (kinds (ANY STR SYM))))
    ("REFUSE-13 C2 ANY refined to STR by =, then met with SYM" (and (field ?b "slot" ?v) (= ?v "x") (kind ?c ?v))
     (error bad-request join-across-kinds (variable ?v) (positions ((field 3) (kind 2))) (kinds (ANY SYM))))
    ("REFUSE-14 C2 the same with the equality's operands reversed" (and (field ?b "slot" ?v) (= "x" ?v) (kind ?c ?v))
     (error bad-request join-across-kinds (variable ?v) (positions ((field 3) (kind 2))) (kinds (ANY SYM))))
    ("REFUSE-15 C3 string< on a symbol" (and (kind ?b ?k) (string< ?k "m"))
     (error bad-request wrong-kind (test string<) (operand 1) (kind SYM) (needs STR)))
    ("REFUSE-16 C3 member with no element of the operand's kind" (and (kind ?b ?k) (member ?k ("valid" "x")))
     (error bad-request wrong-kind (test member) (operand 1) (kind SYM) (got ("valid" "x"))))
    ("REFUSE-17 C3 = between a string and a symbol" (and (title ?b ?t) (= ?t none))
     (error bad-request wrong-kind (test =) (operand 2) (kinds (STR (SYM none)))))))
(for-each (lambda (r)
            (want (string-append (car r) ": in a query and in a rule's must-not")
                  (in-order (query-kind-refusal (cadr r)) (rule-answer (cadr r)))
                  (list (caddr r) (caddr r))))
          refused)
(want "REFUSE-8 C1 score's text as a symbol is wrong-kind before any evaluation; in a rule, today's external-relation refusal comes first"
      (in-order (query-kind-refusal '(score ?b text ?s)) (rule-answer '(score ?b text ?s)))
      '((error bad-request wrong-kind (relation score) (position 2) (column STR) (got text))
        (error bad-request rule-relation-not-allowed (relation score))))
(want "REFUSE-18 ?w is an ID in a rule value: (kind ?b ?w) is refused there, and is no refusal as a query"
      (in-order (query-kind-refusal '(kind ?b ?w)) (rule-answer '(kind ?b ?w)))
      '(#f (error bad-request join-across-kinds (variable ?w) (positions ((written) (kind 2))) (kinds (ID SYM)))))

;; ---- KEEP: goals that must keep answering ---------------------------------------------------
(printf "== KEEP: a filter or a compatible goal is not refused ==\n")
;; a block titled "root" at the top, so the parent column meets it
(run 'insert "--title" "root")
(define a1 (let ((a (run 'insert "--title" "a"))) (let ((e (car (cadr (assq 'events (cdr a)))))) (block-id (car e) (cdr e)))))
(define kept
  (list '(and (imports ?m ?n) (in-library ?b ?m))
        '(ambiguous ?b ?n)
        (list 'and (list 'field a1 "slot" '?v) '(string< ?v "z"))
        (list 'and (list 'field a1 "slot" '?v) (list 'field a1 "slot" '?v))
        '(and (title ?b ?t) (/= ?t code))
        '(and (kind ?b ?k) (member ?k (section "section")))
        '(in-library ?b "none")
        '(and (title ?b ?t) (under ?x ?t))
        '(lang ?b x) '(lang ?b "x") '(lang ?b 7)
        '(moved ?a ?r ?b target)
        '(kind ?b other)))
(want "KEEP-1 no kept goal is refused for a kind, as a query or in a rule (a rule may refuse an external relation, as today)"
      (filter (lambda (g) (or (query-kind-refusal g) (kind-refusal? (rule-answer g)))) kept)
      '())
(want "KEEP-2 every kept goal answers ok through the query verb"
      (filter (lambda (g) (not (eq? 'ok (head-of (run 'query (format "~s" g)))))) kept)
      '())
(want "KEEP-3 a title met with under's parent column answers: the top-level block titled root"
      (let ((a (run 'query "(and (title ?b ?t) (under ?x ?t))")))
        (in-order (head-of a) (let ((items (assq 'items (cdr a)))) (and items (pair? (cdr items)) #t))))
      '(ok #t))

;; ---- README: every query and rule example ---------------------------------------------------
(printf "== README: every example passes ==\n")
(define readme (call-with-input-file "../README.md" get-string-all))
(define (string-index-from s sub from)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i from)) (cond ((> (+ i m) n) #f) ((string=? (substring s i (+ i m)) sub) i) (else (loop (+ i 1)))))))
;; every datum written after MARKER and closed by the next single quote
(define (quoted-after marker)
  (let loop ((i 0) (out '()))
    (let ((at (string-index-from readme marker i)))
      (if (not at)
          (reverse out)
          (let* ((from (+ at (string-length marker))) (to (string-index-from readme "'" from)))
            (loop (or to (string-length readme))
                  (let ((d (and to (guard (e (#t #f)) (read (open-string-input-port (substring readme from to)))))))
                    (if (pair? d) (cons d out) out))))))))
;; THE LANGUAGE CHAPTER'S EXAMPLES are its indented lines that start with a
;; goal, written bare, a "->" and the answer after it; a goal on such a
;; line is its first datum. Any `query '...'` elsewhere in the README too.
(define (chapter-goals)
  (let* ((a (string-index-from readme "## The query and rule language" 0))
         (b (and a (string-index-from readme "\n## " (+ a 1))))
         (chapter (if a (substring readme a (or b (string-length readme))) ""))
         (p (open-string-input-port chapter)))
    (let loop ((out '()))
      (let ((l (get-line p)))
        (if (eof-object? l)
            (reverse out)
            (loop (if (and (> (string-length l) 5) (string=? (substring l 0 5) "    ("))
                      (let ((d (guard (e (#t #f)) (read (open-string-input-port l))))) (if (pair? d) (cons d out) out))
                      out)))))))
(define readme-queries (append (chapter-goals) (quoted-after "query '")))
(define readme-rule-goals (append (quoted-after "--must '") (quoted-after "--must-not '") (quoted-after "--where '")))
(want "README-1 no example of the language chapter, nor any query in the README, is refused for a kind (CONTROL: at least ten read)"
      (in-order (filter query-kind-refusal readme-queries) (>= (length readme-queries) 10))
      '(() #t))
(want "README-2 no rule example's goal is refused for a kind (CONTROL: at least three goals read)"
      (in-order (filter (lambda (g) (kind-refusal? (rule-value-check (in-rule g)))) readme-rule-goals)
                (>= (length readme-rule-goals) 3))
      '(() #t))

;; ---- LIB: the library rules' kinds -----------------------------------------------------------
(printf "== LIB: the library rules check against the kinds ==\n")
(define (kinds-refusal rules declared)
  (guard (e ((refusal? e) (refusal-answer e))) (rule-kinds rules declared) 'no-refusal))
(want "LIB-1 every library relation has kinds, and unit, def-for, scope-name and hard are as inferred and declared"
      (in-order (filter (lambda (r) (not (library-relation-kinds r))) (map car (rule-relations rule-library)))
                (map library-relation-kinds '(unit def-for scope-name hard)))
      '(() ((PARENT ID) (ID STR ID) (ID NAME-OR-NONE) (PARENT ID (SYM unit member unsettled supersedes cause implementer stale) PARENT))))
(want "LIB-2 C4 a declaration narrower than its clauses (under* as (ID, ID)) is refused with head-kind"
      (kinds-refusal rule-library (cons '(under* ID ID) (filter (lambda (d) (not (eq? (car d) 'under*))) declared-rule-kinds)))
      '(error head-kind (relation under*) (position 2) (declared ID) (clause PARENT)))
(want "LIB-3 C4 a hard clause with a role outside hard's declared set is refused by containment"
      (kinds-refusal (append rule-library '(((hard ?t ?h bogus ?t) (unit ?t ?h)))) declared-rule-kinds)
      '(error head-kind (relation hard) (position 3) (declared (SYM unit member unsettled supersedes cause implementer stale))
              (clause (SYM bogus))))
(want "LIB-4 a recursive relation left undeclared has no order to infer it in: kind-order"
      (let ((a (kinds-refusal rule-library (filter (lambda (d) (not (eq? (car d) 'under*))) declared-rule-kinds))))
        (and (pair? a) (list (car a) (cadr a) (and (memq 'under* (cadr (caddr a))) #t))))
      '(error kind-order #t))

;; ---- STORED: a rule stored before the kinds -----------------------------------------------------
(printf "== STORED: a rule stored before the kinds keeps its behaviour ==\n")
;; WRITTEN AS DATA, as an older writer's build could have: another writer's
;; record after everything local, holding a rule whose goal now fails the
;; kinds. Its must-not never matches (a kind is a symbol, never "code").
(define stored-value '((class state) (on section) (must-not (kind ?w "code"))))
(define writer (car (car (reduce-applied-cut (state)))))
(define forged
  (encode-record 1 1789000000001 "peer" (list (cons writer (cdr (assoc writer (reduce-applied-cut (state))))))
                 (storable-encode (list 'rule 'old-kinds stored-value))))
(log-publish! S "rulezzzz" 1 forged (segment-sha forged))
(define declared-rules (let ((d (assq 'declared-rules (cdr (rpc-dispatch S '(describe) "author" (state)))))) (if d (cdr d) '())))
(want "STORED-1 the stored rule is in force, and an ordinary write of the kind it is on is accepted"
      (in-order (and (assq 'old-kinds declared-rules) #t) (head-of (run 'insert "--title" "after the rule")))
      '(#t ok))
(want "STORED-2 describe lists it with the kind mismatch it would meet if declared now"
      (let ((r (assq 'old-kinds declared-rules))) (and r (cddr r)))
      '((kind-mismatch (error bad-request wrong-kind (relation kind) (position 2) (column SYM) (got "code")))))
(want "STORED-3 declaring the same rule anew is refused, by the verb and in a batch"
      (in-order (head-of (run 'rule "old-kinds-again" "--on" "section" "--must-not" "(kind ?w \"code\")"))
                (head-of (car (cadr (run 'batch (format "~s" (list (list 'rule 'old-kinds-batch stored-value))))))))
      '((error bad-request) (error bad-request)))

;; ---- VERB: the two verbs' wiring ------------------------------------------------------------------
(printf "== VERB: the query and rule verbs ==\n")
(want "VERB-1 the query verb answers a kind refusal, and an arity error is still reported first"
      (in-order (run 'query "(kind ?b \"code\")") (head-of (run 'query "(kind ?b \"code\" ?x)")))
      '((error bad-request wrong-kind (relation kind) (position 2) (column SYM) (got "code")) (error bad-request)))
(want "VERB-2 the rule verb refuses a C2 goal, and declares a kept one"
      (in-order (run 'rule "joins" "--on" "section" "--must-not" "(and (kind ?b ?k) (title ?x ?k))")
                (head-of (run 'rule "keeps" "--on" "section" "--must-not" "(and (title ?w ?t) (/= ?t code))")))
      '((error bad-request join-across-kinds (variable ?k) (positions ((kind 2) (title 2))) (kinds (SYM STR))) ok))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nquery-kinds complete\n" rows bad)
(exit (if (zero? bad) 0 1))
