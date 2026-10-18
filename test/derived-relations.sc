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

;; Stored derived relations (the `derive` verb, query.sc's STORED handler):
;; a relation a store declares, which queries, rules and other stored
;; relations name as they name the library's. Its record is folded as rules
;; are (candidates, supersession, agreement, contest, retirement); its clauses
;; are data checked at the cut they are used at, so a goal naming one that is
;; contested, retired or failing its own check there is refused
;; relation-unavailable, and a rule naming it is rule-unevaluable at the next
;; write it judges. Each group has a store of its own. A record that bypassed
;; the verb (an older or a forged writer's) is published as another writer's
;; segment, as completion-stale and query-kinds do.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia query) make-query-session session-answer refusal? refusal-answer check-rules rule-library
              relation-external? rule-value-check)
        (only (theourgia store) open-and-reduce with-store-write make-write-request)
        (only (theourgia reduce) reduce-applied-cut block-id state-hash reduce-clone state->rows rows->state
              reduce-empty reduce-apply! reduce-gates state-rule)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha))

;; LOOKED UP, NOT IMPORTED, so the fixture loads on a tree without it (the base)
;; and its rows read red there one by one
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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/derived-relations-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(register-verbs! extension-verbs)

;; ---- stores, verbs, readings --------------------------------------------------------

(define n 0)
;; a new store, initialised: -> its path
(define (store! name)
  (set! n (+ n 1))
  (let ((st (string-append root "/" name "-" (number->string n))))
    (rpc-dispatch st '(init) "author")
    st))
(define (on st . args) (rpc-dispatch st args "author"))
(define (state st . cut) (apply open-and-reduce st cut))
(define (cut-of st) (reduce-applied-cut (state st)))
(define (head-of a) (if (and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)) (and (pair? a) (car a))))
(define (head3 a) (if (and (list? a) (>= (length a) 3)) (list-head a 3) a))
(define (clause-of a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (new-id a)
  (let* ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (batch st . intents) (on st 'batch (format "~s" intents)))
;; a block of KIND, made by a batch of one insert, under PARENT (root when
;; not given): -> its id
(define (make st kind title . parent)
  (let ((a (batch st (list 'insert (if (pair? parent) (car parent) 'root) #f (list (cons 'kind kind) (cons 'title title))))))
    (new-id (car (cadr a)))))
(define (derive! st name value) (on st 'derive (symbol->string name) (format "~s" value)))
(define (retire! st name) (on st 'derive (symbol->string name) "--retire"))
(define (q st goal) (on st 'query (format "~s" goal)))
;; a query answer's rows, sorted as the answer sorts them; anything else as it is
(define (rows-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a))) (map cdr (cdr (cadr a))) a))
(define (query-rows st goal) (rows-of (q st goal)))
;; the fourth table's entry for NAME, from query --relations
(define (derived-entry st name)
  (let ((a (on st 'query "--relations")))
    (and (pair? a) (eq? (car a) 'ok)
         (find (lambda (i) (and (pair? i) (eq? (car i) 'derived) (eq? (cadr i) name))) (cdr (cadr a))))))
;; THE STORE'S OWN LOG WRITER, as position names it: the cut lists every
;; writer, a forged peer's among them, in an order that is not the local one's
(define (local-writer st)
  (let ((p (clause-of (on st 'position) 'position))) (and p (car (cadr p)))))
;; A RECORD FROM ANOTHER WRITER, PEER (eight characters), whose premise is
;; PAST, the local writer's cut it saw (now when not given): the record a
;; writer that bypassed the verb, or a concurrent writer, leaves.
(define (forge! st peer payload . past)
  (let* ((w (local-writer st))
         (seen (if (pair? past) (car past) (cut-of st)))
         (bytes (encode-record 1 1789000000001 "peer" (list (cons w (cdr (assoc w seen)))) (storable-encode payload))))
    (log-publish! st peer 1 bytes (segment-sha bytes))))
;; a rule's verb, its options given as strings
(define (rule! st name . options) (apply on st 'rule name options))
;; a write: a block's title set
(define (title! st id text) (on st 'set id "title" text))
;; the refusal's first three elements and its reason clause, for a rule refusal
(define (unevaluable a) (and (pair? a) (eq? (car a) 'error) (list (head3 a) (clause-of a 'rule) (clause-of a 'reason))))

;; the values used below: NAME's one-clause relation of kind ID over the blocks of KIND
(define (over-kind name kind) (list (list 'head (list name 'ID)) (list 'clause (list name '?x) (list 'kind '?x kind))))

;; ---- DERIVE: declared, called, checked at the verb ----------------------------------
(printf "== DERIVE: declared, called, and checked where it is written ==\n")
(define D (store! "derive"))
(define dA (make D 'section "A"))
(define dB (make D 'doc "B"))
(define dC (make D 'section "C" dA))
(define (sorted rs) (list-sort (lambda (x y) (string<? (format "~s" x) (format "~s" y))) rs))
(want "DERIVE-1 a relation declared and called from a query: its rows"
      (in-order (head-of (derive! D 'secs (over-kind 'secs 'section))) (sorted (query-rows D '(secs ?x))))
      (list 'ok (sorted (list (list dA) (list dC)))))
(want "DERIVE-2 a relation of two clauses: the rows of either"
      (in-order (head-of (derive! D 'secdoc (list '(head (secdoc ID)) '(clause (secdoc ?x) (kind ?x section)) '(clause (secdoc ?x) (kind ?x doc)))))
                (sorted (query-rows D '(secdoc ?x))))
      (list 'ok (sorted (list (list dA) (list dB) (list dC)))))
(want "DERIVE-3 a recursive relation answers its least model: every ancestor of C"
      (in-order (head-of (derive! D 'anc '((head (anc ID PARENT)) (clause (anc ?x ?p) (under ?x ?p))
                                           (clause (anc ?x ?p) (under ?x ?m) (anc ?m ?p)))))
                (sorted (query-rows D (list 'anc dC '?p))))
      (list 'ok (sorted (list (list dA) (list "root")))))
(want "DERIVE-4 the declared kinds: a call with a constant of a disjoint kind is refused wrong-kind"
      (in-order (head-of (derive! D 'kk '((head (kk SYM)) (clause (kk ?k) (kind ?b ?k))))) (q D '(kk "code")))
      '(ok (error bad-request wrong-kind (relation kk) (position 1) (column SYM) (got "code"))))
(define (taken name) (derive! D name (over-kind name 'section)))
(want "DERIVE-5 a name the library defines is refused name-taken, library"
      (taken 'depends*) '(error bad-request name-taken (name depends*) (where library)))
(want "DERIVE-6 a fact relation's name is refused name-taken, fact"
      (taken 'kind) '(error bad-request name-taken (name kind) (where fact)))
(want "DERIVE-7 a test's name is refused name-taken, test"
      (taken 'member) '(error bad-request name-taken (name member) (where test)))
(want "DERIVE-8 a per-write fact's name is refused name-taken, write-fact"
      (taken 'kind+) '(error bad-request name-taken (name kind+) (where write-fact)))
(want "DERIVE-9 a declared edge relation's name is refused name-taken, edge-relation"
      (in-order (head-of (on D 'relation "linksto" "--as" "nothing")) (taken 'linksto))
      '(ok (error bad-request name-taken (name linksto) (where edge-relation))))
(want "DERIVE-10 a stored rule's name is refused name-taken, rule"
      (in-order (head-of (rule! D "titled" "--on" "doc" "--must-not" "(title ?w \"never\")")) (taken 'titled))
      '(ok (error bad-request name-taken (name titled) (where rule))))
(want "DERIVE-11 RECIPROCAL: rule refuses a stored relation's name"
      (rule! D "secs" "--on" "doc" "--must-not" "(title ?w \"never\")")
      '(error bad-request name-taken (name secs) (where derive)))
(want "DERIVE-12 RECIPROCAL: relation refuses a stored relation's name"
      (on D 'relation "secs" "--as" "nothing")
      '(error bad-request name-taken (name secs) (where derive)))
(want "DERIVE-13 a body reaching an external relation is refused"
      (derive! D 'ex '((head (ex ID)) (clause (ex ?x) (def ?x _ _))))
      '(error bad-request rule-relation-not-allowed (relation def)))
;; helper -> def, by a record that bypassed the verb
(forge! D "helperzz" '(derive helper ((head (helper ID)) (clause (helper ?x) (def ?x _ _)))))
(want "DERIVE-14 INDIRECT: a caller of a bypassed helper that reads def is refused through helper"
      (derive! D 'caller '((head (caller ID)) (clause (caller ?x) (helper ?x))))
      '(error bad-request rule-relation-not-allowed (relation helper)))
(want "DERIVE-15 INDIRECT: a rule naming that helper is refused the same way"
      (rule! D "useshelper" "--on" "doc" "--must-not" "(helper ?w)")
      '(error bad-request rule-relation-not-allowed (relation helper)))
(want "DERIVE-16 the external walk follows stored clauses at a state, and the library alone at none"
      (list (relation-external? 'helper (state D)) (relation-external? 'helper))
      '(#t #f))
;; the first item's answer of a batch, or the answer when it is not a batch
(define (first-item a) (if (and (pair? a) (eq? (car a) 'batch) (pair? (cadr a))) (car (cadr a)) a))
(define (by-batch st text) (first-item (on st 'batch text)))
(want "DERIVE-17 a value with shared structure is refused before it is copied, by the verb and in a batch"
      (in-order (on D 'derive "sh" "((head (sh ID)) #0=(clause (sh ?x) (kind ?x section)) #0#)")
                (by-batch D "((derive sh ((head (sh ID)) #0=(clause (sh ?x) (kind ?x section)) #0#)))"))
      '((error bad-request derive-malformed (reason derive-not-a-tree))
        (error bad-request derive-malformed (reason derive-not-a-tree))))
(want "DERIVE-19 and is the conjunction, never a stored relation's name: refused, by the verb and in a batch"
      (in-order (on D 'derive "and" "((head (and STR)) (clause (and \"x\")))")
                (by-batch D "((derive and ((head (and STR)) (clause (and \"x\")))))"))
      '((error bad-request name-taken (name and) (where conjunction))
        (error bad-request name-taken (name and) (where conjunction))))
(want "DERIVE-18 a value holding a plan marker is refused, by the verb and in a batch"
      (in-order (on D 'derive "mk" "((head (mk ID)) (clause (mk ?x) (title ?x (\"#%new\" 1))))")
                (by-batch D "((derive mk ((head (mk ID)) (clause (mk ?x) (title ?x (\"#%new\" 1))))))"))
      '((error bad-request derive-malformed (reason derive-holds-marker))
        (error bad-request derive-malformed (reason derive-holds-marker))))

;; ---- DECLARE-CHECK: an invalid body, by the verb and on the batch route ----------------
(printf "== DECLARE-CHECK: each invalid body refused by the verb and in a batch ==\n")
(define (both name value)
  (in-order (derive! D name value) (first-item (batch D (list 'derive name value)))))
(define (four a) (if (and (list? a) (>= (length a) 4)) (list-head a 4) a))
(want "DECLARE-1 a goal of the wrong arity: wrong-arity"
      (both 'ar '((head (ar ID)) (clause (ar ?x) (kind ?x))))
      '((error bad-request wrong-arity (relation kind) (arity 2) (given 1))
        (error bad-request wrong-arity (relation kind) (arity 2) (given 1))))
(want "DECLARE-2 a list in a position that takes a term: not-a-term"
      (both 'mt '((head (mt ID)) (clause (mt ?x) (kind ?x (a b)))))
      '((error bad-request not-a-term (term (a b))) (error bad-request not-a-term (term (a b)))))
(want "DECLARE-3 a head variable no body goal binds: head-variable-unbound"
      (both 'hv '((head (hv ID)) (clause (hv ?y) (kind ?x section))))
      '((error bad-request head-variable-unbound (relation hv) (variable ?y))
        (error bad-request head-variable-unbound (relation hv) (variable ?y))))
(want "DECLARE-4 a body goal with a constant of a disjoint kind: wrong-kind"
      (both 'kr '((head (kr ID)) (clause (kr ?x) (kind ?x "code"))))
      '((error bad-request wrong-kind (relation kind) (position 2) (column SYM) (got "code"))
        (error bad-request wrong-kind (relation kind) (position 2) (column SYM) (got "code"))))
(want "DECLARE-5 a clause head wider than the declared kind: head-kind"
      (both 'hc '((head (hc ID)) (clause (hc ?t) (title ?b ?t))))
      '((error bad-request head-kind (relation hc) (position 1) (declared ID) (clause STR))
        (error bad-request head-kind (relation hc) (position 1) (declared ID) (clause STR))))
(want "DECLARE-6 a body naming an undeclared relation: unknown-relation"
      (map four (both 'uc '((head (uc ID)) (clause (uc ?x) (nosuch ?x)))))
      '((error bad-request unknown-relation (relation nosuch)) (error bad-request unknown-relation (relation nosuch))))

;; ---- HISTORY: read at a cut, superseded, agreed, contested, retired ------------------
(printf "== HISTORY: a declaration's candidates ==\n")
;; GOAL's answer in a session at CUT
(define (answer-at st cut goal)
  (let ((a (session-answer (make-query-session (state st cut)) goal)))
    (if (and (pair? a) (eq? (car a) 'ok)) (list 'ok (sorted (cadr a))) a)))
(define H (store! "history"))
(define hS (make H 'section "S"))
(define hD (make H 'doc "D"))
(define h-before (cut-of H))
(derive! H 'r (over-kind 'r 'section))
(define h-v1 (cut-of H))
(derive! H 'r (over-kind 'r 'doc))
(define h-v2 (cut-of H))
(want "HISTORY-1 at a cut before the declaration the relation is unknown"
      (let ((a (answer-at H h-before '(r ?x)))) (and (pair? a) (list-head a 4)))
      '(error bad-request unknown-relation (relation r)))
(want "HISTORY-2 V1 then V2 by one writer: V1's rows at its cut, V2's at the later one"
      (list (answer-at H h-v1 '(r ?x)) (answer-at H h-v2 '(r ?x)))
      (list (list 'ok (list (list hS))) (list 'ok (list (list hD)))))
;; two writers declaring concurrently: the local writer's value, and a
;; peer's whose premise is the cut before it
(define (concurrent name local peer-value)
  (let* ((st (store! "concurrent")) (s (make st 'section "S")) (past (cut-of st)))
    (derive! st name local)
    (forge! st "peerzzzz" (list 'derive name peer-value) past)
    (list st s)))
(define agree (concurrent 'r (over-kind 'r 'section) (over-kind 'r 'section)))
(want "HISTORY-3 two writers' EQUAL concurrent values agree: one value in force, rows answered"
      (in-order (car (state-derived (state (car agree)) 'r)) (query-rows (car agree) '(r ?x)))
      (list 'in-force (list (list (cadr agree)))))
(define contest (concurrent 'r (over-kind 'r 'section) (over-kind 'r 'doc)))
(want "HISTORY-4 two writers' DISTINCT concurrent values contest: relation-unavailable, contested"
      (q (car contest) '(r ?x))
      '(error bad-request relation-unavailable (relation r) (state contested)))
(define two-clauses '((head (r ID)) (clause (r ?x) (kind ?x section)) (clause (r ?x) (kind ?x doc))))
(define reordered (concurrent 'r two-clauses (list (car two-clauses) (caddr two-clauses) (cadr two-clauses))))
(want "HISTORY-5 the same clauses in another order are another value: contested"
      (q (car reordered) '(r ?x))
      '(error bad-request relation-unavailable (relation r) (state contested)))
(define hR (store! "retired"))
(derive! hR 'r (over-kind 'r 'section))
(retire! hR 'r)
(want "HISTORY-6 a retired relation named in a goal: relation-unavailable, retired, not unknown"
      (q hR '(r ?x))
      '(error bad-request relation-unavailable (relation r) (state retired)))
(want "HISTORY-7 conflicts lists a contested relation with its candidates"
      (let ((c (find (lambda (x) (and (pair? x) (eq? (car x) 'derive-contested))) (let ((a (on (car contest) 'conflicts))) (if (and (pair? a) (eq? (car a) 'ok)) (cdr (cadr a)) '())))))
        (and c (list (car c) (cadr c) (length (cdr (caddr c))))))
      '(derive-contested r 2))
(want "HISTORY-8 the fourth table: in force with arity, kinds and availability; retired; contested with its candidates' heads"
      (list (derived-entry H 'r) (derived-entry hR 'r)
            (let ((e (derived-entry (car contest) 'r)) (d (state-derived (state (car contest)) 'r)))
              (and e (list (car e) (cadr e) (caddr e) (car (cadddr e))
                           (sorted (map (lambda (c) (list (car c) (cadr c))) (cdr (cadddr e))))
                           ;; EVERY CANDIDATE'S WRITER AND SEQUENCE as the reduction holds them
                           (equal? (sorted (cdr (cadddr e)))
                                   (sorted (map (lambda (c) (list (cadr (car (car c))) (cadr c) (caddr c))) (cdr d))))
                           (for-all (lambda (c) (and (integer? (caddr c)) (> (caddr c) 0))) (cdr (cadddr e)))
                           (list-ref e 4)))))
      (list '(derived r (arity 1) (kinds (ID)) available)
            '(derived r (retired) (unavailable (state retired)))
            (list 'derived 'r '(contested) 'candidates (sorted (list (list '(r ID) (local-writer (car contest))) (list '(r ID) "peerzzzz")))
                  #t #t '(unavailable (state contested)))))

;; ---- the table in the reduction: snapshot, clone, hash -------------------------------
(printf "== the derived table in the reduction ==\n")
(define (same-after f st) (let ((s (state st))) (and (state-derived s 'r) (equal? (state-derived (f s) 'r) (state-derived s 'r)))))
(want "STATE-1 a snapshot's rows carry the table: in force, contested (every candidate, writer and sequence) and retired"
      (map (lambda (st) (same-after (lambda (s) (rows->state (state->rows s))) st)) (list H (car contest) hR))
      '(#t #t #t))
(want "STATE-2 a clone holds the table: in force, contested and retired"
      (map (lambda (st) (same-after reduce-clone st)) (list H (car contest) hR))
      '(#t #t #t))
;; A REBUILD IS FORCED BY A RECORD THAT REVERSES AN APPLIED MEMBERSHIP: two
;; records claiming one request's identity (relation-effects' RE-9 does the
;; same with edge relations); the second gates both, and the state is folded
;; again from the applied records
(define (claim-actor) (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0)))
(define rebuilt
  (guard (e (#t 'RAISED))
    (let ((r (reduce-empty)))
      (reduce-apply! r "decl0000" 1 '() (list 'derive 'keep (over-kind 'keep 'section)))
      (reduce-apply! r "aaa00000" 1 '() (list 'derive 'gone (over-kind 'gone 'section)) (claim-actor))
      (reduce-apply! r "bbb00000" 1 '() (list 'derive 'gone (over-kind 'gone 'section)) (claim-actor))
      r)))
(want "STATE-4 a rebuild after a retraction: both claims gated, and the derived table is what the applied records say"
      (and (not (eq? rebuilt 'RAISED))
           (list (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates rebuilt)))) (and p (cdr p)))) '("aaa00000" "bbb00000"))
                 (state-derived rebuilt 'gone)
                 (car (state-derived rebuilt 'keep))))
      '((plan-conflict plan-conflict) #f in-force))
(define hH (store! "hash"))
(make hH 'section "S")
(define hash-0 (state-hash (state hH)))
(derive! hH 'r (over-kind 'r 'section))
(want "STATE-3 a write that only declares a relation changes the state hash"
      (equal? (state-hash (state hH)) hash-0)
      #f)

;; ---- AVAILABILITY: derived at the cut ------------------------------------------------
(printf "== AVAILABILITY: a relation's own check at the cut it is used at ==\n")
(define A (store! "available"))
(define aT (make A 'section "T1"))
(derive! A 'r '((head (r STR)) (clause (r ?t) (title ?b ?t))))
(derive! A 's '((head (s STR)) (clause (s ?x) (r ?x))))
(define a-rows-before (query-rows A '(s ?x)))
(define a-widened (derive! A 'r '((head (r ANY)) (clause (r ?v) (field ?b "title" ?v)))))
(want "AVAIL-1 r widened to ANY under s/STR: s's own head containment fails at the next cut, in a query and in the listing"
      (in-order a-rows-before (head-of a-widened) (q A '(s ?x)) (derived-entry A 's))
      '((("T1")) ok
        (error bad-request relation-unavailable (relation s)
               (state (check (error bad-request head-kind (relation s) (position 1) (declared STR) (clause ANY)))))
        (derived s (arity 1) (kinds (STR))
                 (unavailable (state (check (error bad-request head-kind (relation s) (position 1) (declared STR) (clause ANY))))))))
(want "AVAIL-2 the widening's answer says what it affects: s"
      (clause-of a-widened 'affects)
      '(affects (derive s)))
(want "AVAIL-3 s redeclared with head ANY: available again, its rows answered"
      (in-order (head-of (derive! A 's '((head (s ANY)) (clause (s ?x) (r ?x))))) (query-rows A '(s ?x)))
      '(ok (("T1"))))
(define A5 (store! "narrow"))
(define a5D (make A5 'doc "y"))
(derive! A5 'r '((head (r ANY)) (clause (r ?v) (field ?b "title" ?v))))
(rule! A5 "callsx" "--on" "doc" "--must-not" "(r \"x\")")
(want "AVAIL-4 NARROWING r/ANY to r/STR under a rule calling (r \"x\"): accepted, and the rule judges by the new clauses (which answer \"x\")"
      (in-order (head-of (derive! A5 'r '((head (r STR)) (clause (r "x"))))) (head3 (title! A5 a5D "z")))
      '(ok (error refused rule-violation)))
(define A6 (store! "arity"))
(make A6 'section "S")
(derive! A6 'r (over-kind 'r 'section))
(derive! A6 's '((head (s ID)) (clause (s ?x) (r ?x))))
(derive! A6 'r '((head (r ID SYM)) (clause (r ?x ?k) (kind ?x ?k))))
(want "AVAIL-5 ARITY r/1 to r/2: a query naming its caller s is refused with the wrong-arity check"
      (q A6 '(s ?x))
      '(error bad-request relation-unavailable (relation s)
              (state (check (error bad-request wrong-arity (relation r) (arity 2) (given 1))))))
;; WITHIN A TIME, INTERRUPTIBLY: the declarations and the query run in a
;; child process under an alarm, so a walk that would not end reads timed-out
;; in its row and the rows after it still run. SPECS are (<name> <value>),
;; declared in order; -> (<each declaration's answer head> <the query's
;; answer>), or timed-out.
(define chain-child (string-append root "/chain.ss"))
(call-with-output-file chain-child
  (lambda (p)
    (for-each (lambda (d) (write d p) (newline p))
      '((import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))
        (register-verbs! extension-verbs)
        (define args (command-line-arguments))
        (define st (car args))
        (define in (call-with-input-file (cadr args) read))
        (let loop ((specs (car in)) (out '()))
          (if (null? specs)
              (let ((a (rpc-dispatch st (list 'query (format "~s" (cadr in))) "author")))
                (call-with-output-file (caddr args) (lambda (p) (write (list (reverse out) a) p)) 'replace))
              (let ((a (rpc-dispatch st (list 'derive (symbol->string (car (car specs))) (format "~s" (cadr (car specs)))) "author")))
                (loop (cdr specs) (cons (if (pair? a) (car a) a) out)))))))))
(define (in-a-child seconds st specs goal)
  (let ((in (string-append st ".chain-in")) (out (string-append st ".chain-out")))
    (call-with-output-file in (lambda (p) (write (list specs goal) p)) 'replace)
    (system (string-append "perl -e 'alarm " (number->string seconds) "; exec @ARGV' scheme --script '" chain-child "' '"
                           st "' '" in "' '" out "' < /dev/null > /dev/null 2>&1"))
    (if (file-exists? out) (call-with-input-file out read) 'timed-out)))
(define (chain-reading r) (if (pair? r) (list (for-all (lambda (h) (eq? h 'ok)) (car r)) (rows-of (cadr r))) r))
;; ONE WALK PER RELATION: f<i> names f<i-1> and f<i-2>, thirty deep; a walk
;; that went down every path would visit f0 more than half a million times
(define A8 (store! "chain"))
(define a8S (make A8 'section "S"))
(define (chain-name i) (string->symbol (string-append "f" (number->string i))))
;; IN ORDER, each naming ones already declared
(define chain-specs
  (let loop ((i 0) (out '()))
    (if (= i 30)
        (reverse out)
        (loop (+ i 1)
              (cons (list (chain-name i)
                          (if (< i 2)
                              (over-kind (chain-name i) 'section)
                              (list (list 'head (list (chain-name i) 'ID))
                                    (list 'clause (list (chain-name i) '?x) (list (chain-name (- i 1)) '?x) (list (chain-name (- i 2)) '?x)))))
                    out)))))
(want "AVAIL-7 thirty relations each naming the two before: all declared, the last answers, inside a minute"
      (chain-reading (in-a-child 60 A8 chain-specs (list (chain-name 29) '?x)))
      (list #t (list (list a8S))))
;; A RECURSIVE BASE UNDER DOUBLED CALLS: g0 names itself, g<i> names g<i-1>
;; twice, thirty deep. The availability walk keeps a recursive
;; relation's answer once its own walk ends, and the walk for a relation
;; outside the log visits each relation once: either walking every path would
;; visit g0 hundreds of millions of times
(define A10 (store! "doubled"))
(define a10S (make A10 'section "S"))
(define (doubled-name i) (string->symbol (string-append "g" (number->string i))))
(define doubled-specs
  (let loop ((i 0) (out '()))
    (if (= i 30)
        (reverse out)
        (loop (+ i 1)
              (cons (list (doubled-name i)
                          (if (= i 0)
                              '((head (g0 ID)) (clause (g0 ?x) (kind ?x section)) (clause (g0 ?x) (g0 ?x)))
                              (list (list 'head (list (doubled-name i) 'ID))
                                    (list 'clause (list (doubled-name i) '?x)
                                          (list (doubled-name (- i 1)) '?x) (list (doubled-name (- i 1)) '?x)))))
                    out)))))
(want "AVAIL-9 a recursive base under doubled calls, thirty deep: all declared, the last answers, inside a minute"
      (chain-reading (in-a-child 60 A10 doubled-specs (list (doubled-name 29) '?x)))
      (list #t (list (list a10S))))
;; ONE CYCLE THROUGH THE WHOLE CHAIN: h<i> names h<i-1> twice, and h0 is then
;; redeclared to name h25 as well. Every relation on the cycle leans on h0
;; while h0's walk is on; its partial answer, read again on that walk, stands
(define A11 (store! "cycle-doubled"))
(define a11S (make A11 'section "S"))
(define (cyc-name i) (string->symbol (string-append "h" (number->string i))))
(define cycle-specs
  (let loop ((i 0) (out '()))
    (if (= i 26)
        (reverse (cons (list 'h0 '((head (h0 ID)) (clause (h0 ?x) (kind ?x section)) (clause (h0 ?x) (h25 ?x)))) out))
        (loop (+ i 1)
              (cons (list (cyc-name i)
                          (if (= i 0)
                              (over-kind 'h0 'section)
                              (list (list 'head (list (cyc-name i) 'ID))
                                    (list 'clause (list (cyc-name i) '?x) (list (cyc-name (- i 1)) '?x) (list (cyc-name (- i 1)) '?x)))))
                    out)))))
(want "AVAIL-10 a cycle through a doubled chain, h0 naming h25: all declared, the last answers, inside a minute"
      (chain-reading (in-a-child 60 A11 cycle-specs (list (cyc-name 25) '?x)))
      (list #t (list (list a11S))))
;; A CYCLE WITH AN UNAVAILABLE WAY OUT: ca names cb and cz, cb names ca, cz
;; retired. The listing asks ca first (its names sorted); cb, met on ca's walk
;; while ca was on the stack, read available there, and that partial answer
;; is not the one cb is listed with
(define A9 (store! "cycle"))
(make A9 'section "S")
(derive! A9 'cz (over-kind 'cz 'section))
(derive! A9 'ca (over-kind 'ca 'section))
(derive! A9 'cb '((head (cb ID)) (clause (cb ?x) (ca ?x))))
(define a9-setup
  (in-order (head-of (derive! A9 'ca '((head (ca ID)) (clause (ca ?x) (cb ?x)) (clause (ca ?x) (cz ?x)))))
            (retire! A9 'cz)))
(want "AVAIL-8 a cycle whose way out is retired: both listed unavailable through it, and the retirement says it affects both"
      (list (car a9-setup) (clause-of (cadr a9-setup) 'affects) (derived-entry A9 'ca) (derived-entry A9 'cb))
      '(ok (affects (derive ca cb))
        (derived ca (arity 1) (kinds (ID)) (unavailable (state retired) (through cz)))
        (derived cb (arity 1) (kinds (ID)) (unavailable (state retired) (through ca cz)))))
(define A7 (store! "through"))
(make A7 'section "S")
(derive! A7 'r (over-kind 'r 'section))
(derive! A7 's '((head (s ID)) (clause (s ?x) (r ?x))))
(derive! A7 't '((head (t ID)) (clause (t ?x) (s ?x))))
(retire! A7 'r)
(want "AVAIL-6 THROUGH: t names s names r, r retired: a query naming t is refused retired, through s r; the listing says the same"
      (in-order (q A7 '(t ?x)) (derived-entry A7 't))
      '((error bad-request relation-unavailable (relation t) (state retired) (through s r))
        (derived t (arity 1) (kinds (ID)) (unavailable (state retired) (through s r)))))

;; ---- SHADOWED: records that bypassed the verb under reserved names -------------------
(printf "== SHADOWED: a bypassed record under a reserved goal name is masked ==\n")
(define W (store! "shadowed"))
(define wA (make W 'section "A"))
(define wB (make W 'section "B" wA))
(on W 'link wA "depends-on" wB)
(define w-depends-before (query-rows W '(depends* ?a ?b)))
(forge! W "shadowaa" '(derive depends* ((head (depends* ID ID)) (clause (depends* ?a ?b) (kind ?a section) (kind ?b section)))))
(forge! W "shadowbb" '(derive kind+ ((head (kind+ ID SYM)) (clause (kind+ ?x ?k) (kind ?x ?k)))))
(want "SHADOW-1 a bypassed record named like a library head: the library's rows answer, the same as before"
      (in-order w-depends-before (equal? (query-rows W '(depends* ?a ?b)) w-depends-before))
      (list (list (list wA wB)) #t))
(want "SHADOW-2 query --relations lists both as unavailable, shadowed by their owners"
      (list (derived-entry W 'depends*) (derived-entry W 'kind+))
      '((derived depends* (unavailable (shadowed RULE))) (derived kind+ (unavailable (shadowed WRITE-FACT)))))
(want "SHADOW-3 a write session is built without a raise, and a rule naming field+ and field judges: a retitle is its witness"
      (in-order (head-of (rule! W "fieldpair" "--on" "section" "--must-not" "(and (field+ ?w \"title\" ?v) (field ?w \"title\" ?old))"))
                (head3 (title! W wA "A2"))
                (head-of (rule! W "fieldpair" "--retire")))
      '(ok (error refused rule-violation) ok))
(want "SHADOW-4 a query naming kind+ with no per-write facts: unknown relation, as without the record"
      (let ((a (q W '(kind+ ?x ?k)))) (and (pair? a) (list-head a 4)))
      '(error bad-request unknown-relation (relation kind+)))
(on W 'relation "linksto" "--as" "nothing")
(rule! W "titled" "--on" "doc" "--must-not" "(title ?w \"never\")")
(forge! W "shadowcc" '(derive linksto ((head (linksto ID)) (clause (linksto ?x) (kind ?x section)))))
(forge! W "shadowdd" '(derive titled ((head (titled ID)) (clause (titled ?x) (kind ?x section)))))
(want "SHADOW-5 bypassed records named like an edge relation and like a stored rule are served by STORED"
      (list (sorted (query-rows W '(linksto ?x))) (sorted (query-rows W '(titled ?x))))
      (list (sorted (list (list wA) (list wB))) (sorted (list (list wA) (list wB)))))
(forge! W "shadowee" '(derive member ((head (member ID ID)) (clause (member ?x ?y) (kind ?x section) (kind ?y section)))))
(forge! W "shadowff" '(derive kind ((head (kind ID SYM)) (clause (kind ?x ?k) (title ?x ?t) (kind ?x ?k)))))
(want "SHADOW-6 ones named like a test and a fact relation are shadowed, and the fact relation still answers"
      (list (derived-entry W 'member) (derived-entry W 'kind)
            (let ((a (q W '(kind ?b ?k)))) (and (pair? a) (eq? (car a) 'ok) (pair? (rows-of a)))))
      '((derived member (unavailable (shadowed TEST))) (derived kind (unavailable (shadowed FACT))) #t))
(define WK (store! "bodywrite"))
(define wkS (make WK 'section "S"))
(forge! WK "bodyaaaa" '(derive bk ((head (bk ID)) (clause (bk ?x) (kind+ ?x doc)))))
(forge! WK "bodybbbb" '(rule usesbk ((class write) (on section) (where (kind+ ?w section)) (must-not (bk ?w)))))
(want "SHADOW-7 a body naming kind+: unavailable with the unknown-relation check in a query and inside a write session, the same answer"
      (let* ((qa (q WK '(bk ?x))) (wa (title! WK wkS "S2")) (reason (clause-of wa 'reason)))
        (list (and (pair? qa) (list-head qa 4))
              (let ((c (and (pair? qa) (list-ref qa 4)))) (and c (list (car c) (car (cadr c)) (list-head (cadr (cadr c)) 4))))
              (and reason (equal? (cadr reason) qa))))
      '((error bad-request relation-unavailable (relation bk))
        (state check (error bad-request unknown-relation (relation kind+)))
        #t))

;; ---- the session's cut: a write rule at PRE, a state rule at POST --------------------
(printf "== the session's cut ==\n")
(define P (store! "pre"))
(define pB (make P 'doc "b"))
(derive! P 'r '((head (r ID)) (clause (r ?x) (kind ?x code))))
(rule! P "prerule" "--on" "doc" "--where" "(kind+ ?w doc)" "--must-not" "(r ?w)")
(want "CUT-1 WRITE RULE AT PRE: a batch retiring r and setting b's title is accepted, r read where the rule was selected"
      (let ((a (batch P '(derive r retired) (list 'set pB 'title "t2"))))
        (and (pair? a) (list (car a) (map car (cadr a)))))
      '(batch (ok ok)))
(define P2 (store! "post"))
(define p2B (make P2 'doc "b"))
(derive! P2 'r (over-kind 'r 'code))
(rule! P2 "postrule" "--on" "doc" "--must-not" "(r ?w)")
(want "CUT-2 STATE RULE AT POST: a batch redeclaring r to answer docs and setting b's title is judged by the new r"
      (head3 (batch P2 (list 'derive 'r (over-kind 'r 'doc)) (list 'set p2B 'title "t2")))
      '(error refused rule-violation))

;; ---- RULES: a rule naming a stored relation ----------------------------------------
(printf "== RULES: a rule naming a stored relation ==\n")
(define R (store! "rules"))
(define rB (make R 'doc "b"))
(define rq-value '((class state) (on doc) (must-not (r ?w))))
(want "RULES-1 a rule declared beside the relation it names judges the NEXT write, not its own batch"
      (in-order (let ((a (batch R (list 'derive 'r (over-kind 'r 'doc)) (list 'rule 'rq rq-value) (list 'set rB 'title "1"))))
                  (and (pair? a) (list (car a) (map car (cadr a)))))
                (head3 (title! R rB "2")))
      '((batch (ok ok ok)) (error refused rule-violation)))
(define r-retire (retire! R 'r))
(want "RULES-2 retiring r while rq is settled: accepted, and the answer says it affects rq"
      (list (head-of r-retire) (clause-of r-retire 'affects))
      '(ok (affects (rule rq))))
(want "RULES-3 the next write rq judges: rule-unevaluable, the reason r's unavailability"
      (unevaluable (title! R rB "3"))
      '((error refused rule-unevaluable) (rule rq) (reason (error bad-request relation-unavailable (relation r) (state retired)))))
(want "RULES-4 redeclaring r restores rq without touching it: the next write is judged as usual"
      (in-order (head-of (derive! R 'r (over-kind 'r 'code))) (head-of (title! R rB "4")))
      '(ok ok))
(define R5 (store! "rules-retire"))
(define r5B (make R5 'doc "b"))
(derive! R5 'r (over-kind 'r 'code))
(rule! R5 "rq" "--on" "doc" "--must-not" "(r ?w)")
(retire! R5 'r)
(want "RULES-5 retiring the rule instead ends the refusals"
      (in-order (head3 (title! R5 r5B "1")) (head-of (rule! R5 "rq" "--retire")) (head-of (title! R5 r5B "2")))
      '((error refused rule-unevaluable) ok ok))
(define R6 (store! "rules-widen"))
(define r6B (make R6 'doc "b"))
(derive! R6 'r '((head (r STR)) (clause (r ?t) (title ?b ?t))))
(rule! R6 "rq" "--on" "doc" "--must-not" "(and (r ?t) (= ?t \"never\"))")
(want "RULES-6 r widened under rq with a test that still types: accepted, and rq judges by the new clauses (which answer \"never\")"
      (in-order (head-of (derive! R6 'r '((head (r ANY)) (clause (r "never"))))) (head3 (title! R6 r6B "1")))
      '(ok (error refused rule-violation)))
(define R7 (store! "rules-arity"))
(define r7B (make R7 'doc "b"))
(derive! R7 'r (over-kind 'r 'code))
(rule! R7 "rq" "--on" "doc" "--must-not" "(r ?w)")
(derive! R7 'r '((head (r ID SYM)) (clause (r ?x ?k) (kind ?x ?k))))
(want "RULES-7 r's arity changed under rq: rule-unevaluable with the wrong-arity refusal as the reason"
      (unevaluable (title! R7 r7B "1"))
      '((error refused rule-unevaluable) (rule rq) (reason (error bad-request wrong-arity (relation r) (arity 2) (given 1)))))
(define R8 (store! "rules-kinds"))
(define r8B (make R8 'doc "b"))
(derive! R8 'r '((head (r ANY)) (clause (r "x"))))
(rule! R8 "rq" "--on" "doc" "--must-not" "(r \"x\")")
(derive! R8 'r '((head (r SYM)) (clause (r x))))
(want "RULES-8 KINDS under rq: r replaced by r/SYM, the next write is rule-unevaluable with the wrong-kind refusal"
      (unevaluable (title! R8 r8B "1"))
      '((error refused rule-unevaluable) (rule rq) (reason (error bad-request wrong-kind (relation r) (position 1) (column SYM) (got "x")))))
;; A RULE STORED BEFORE THE KINDS (written as data), whose goal fails them
(define RL (store! "legacy"))
(define rlS (make RL 'section "s"))
(forge! RL "legacyaa" '(rule legacy ((class state) (on section) (must-not (kind ?w "code")))))
(want "RULES-9 GUARD: a legacy rule retired in a batch beside a write: accepted, its goal evaluated at POST without a kind pass"
      (let ((a (batch RL '(rule legacy retired) (list 'set rlS 'title "t"))))
        (and (pair? a) (list (car a) (map car (cadr a)))))
      '(batch (ok ok)))
(define RL2 (store! "legacy2"))
(define rl2S (make RL2 'section "s"))
(forge! RL2 "legacyaa" '(rule legacy ((class state) (on section) (must-not (kind ?w "code")))))
(want "RULES-10 GUARD: a rule naming no stored relation is checked without the kinds: the legacy rule keeps judging"
      (head-of (title! RL2 rl2S "t"))
      'ok)
(define R11 (store! "rules-batch"))
(define r11B (make R11 'doc "b"))
(derive! R11 'r (over-kind 'r 'code))
(rule! R11 "rq" "--on" "doc" "--must-not" "(r ?w)")
(want "RULES-11 the batch retire rq, retire r, change b: refused, rq selected from PRE and r retired at POST"
      (head3 (batch R11 '(rule rq retired) '(derive r retired) (list 'set r11B 'title "x")))
      '(error refused rule-unevaluable))
(want "RULES-12 split into two batches: both accepted"
      (in-order (let ((a (batch R11 '(rule rq retired)))) (and (pair? a) (map car (cadr a))))
                (let ((a (batch R11 '(derive r retired) (list 'set r11B 'title "x")))) (and (pair? a) (map car (cadr a)))))
      '((ok) (ok ok)))
;; UNDER MERGE: the local writer declares rq naming r while a peer retires r
(define RM (store! "merge"))
(define rmB (make RM 'doc "b"))
(derive! RM 'r (over-kind 'r 'code))
(define rm-past (cut-of RM))
(rule! RM "rq" "--on" "doc" "--must-not" "(r ?w)")
(forge! RM "mergerzz" '(derive r retired) rm-past)
(want "RULES-13 UNDER MERGE: the next write rq judges is refused retired, and check lists rq unevaluable naming r"
      (in-order (unevaluable (title! RM rmB "1"))
                (let ((rs (assq 'rules (cdr (on RM 'check)))))
                  (and rs (find (lambda (x) (and (pair? x) (eq? (car x) 'rule-unevaluable))) (cdr rs)))))
      '(((error refused rule-unevaluable) (rule rq) (reason (error bad-request relation-unavailable (relation r) (state retired))))
        (rule-unevaluable (rule rq) (reason (error bad-request relation-unavailable (relation r) (state retired))))))
(define RC (store! "audit"))
(make RC 'doc "b")
;; the peer saw the store before r was declared, so the two values contest
(define rc-past (cut-of RC))
(derive! RC 'r (over-kind 'r 'code))
(rule! RC "rq" "--on" "doc" "--must-not" "(r ?w)")
(forge! RC "auditorz" (list 'derive 'r (over-kind 'r 'doc)) rc-past)
(want "RULES-14 the AUDIT on a state whose relation is contested: a row naming the rule and the state, never an assertion"
      (let ((rs (assq 'rules (cdr (on RC 'check)))))
        (and rs (find (lambda (x) (and (pair? x) (eq? (car x) 'rule-unevaluable))) (cdr rs))))
      '(rule-unevaluable (rule rq) (reason (error bad-request relation-unavailable (relation r) (state contested)))))
(define RN (store! "nested"))
(define rnB (make RN 'doc "b"))
(derive! RN 'r (over-kind 'r 'doc))
(rule! RN "rq" "--on" "doc" "--where" "(and (kind ?w doc) (and (r ?w)))" "--must" "(title ?w ?t)")
(retire! RN 'r)
(want "RULES-15 a stored call only inside a nested --where is a dependency: rule-unevaluable after r is retired"
      (unevaluable (title! RN rnB "1"))
      '((error refused rule-unevaluable) (rule rq) (reason (error bad-request relation-unavailable (relation r) (state retired)))))
(define RF (store! "constant"))
(define rfS (make RF 'section "s"))
(derive! RF 'r (over-kind 'r 'doc))
(forge! RF "constant" '(rule legacy ((class state) (on section) (must-not (and (kind ?w "code") (member r (r)))))))
(want "RULES-16 a name only in a constant is no dependency: a forged legacy rule with (member r (r)) keeps the no-kinds exemption"
      (head-of (title! RF rfS "t"))
      'ok)

;; ---- BUDGET: the write's single budget ------------------------------------------------
(printf "== BUDGET: one budget for the write ==\n")
(define RB (store! "budget"))
(apply batch RB (map (lambda (i) (list 'insert 'root #f (list '(kind . section) (cons 'title (number->string i))))) (iota 85)))
(define rbD (make RB 'doc "d"))
(define (cube name arity)
  (let ((vars (map (lambda (i) (string->symbol (string-append "?v" (number->string i)))) (iota arity))))
    (list (list 'head (cons name (map (lambda (v) 'ID) vars)))
          (cons 'clause (cons (cons name vars) (map (lambda (v) (list 'kind v 'section)) vars))))))
(define rb-setup
  (in-order (head-of (derive! RB 'c3a (cube 'c3a 3))) (head-of (derive! RB 'c3b (cube 'c3b 3)))
            (head-of (derive! RB 'c4 (cube 'c4 4)))
            (head-of (rule! RB "a3" "--on" "doc" "--must-not" "(and (c3a ?a ?b ?c) (= ?a \"nope\"))"))))
(want "BUDGET-1 the three relations and the rule are declared, and one rule whose stored call fits the budget alone judges the write"
      (in-order rb-setup (head-of (title! RB rbD "1")))
      '((ok ok ok ok) ok))
;; TWO RULES, ONE BUDGET: each call fits alone, the write's two rules do not
(rule! RB "b3" "--on" "doc" "--must-not" "(and (c3b ?a ?b ?c) (= ?a \"nope\"))")
(want "BUDGET-2 two rules whose calls fit alone but not together: refused at the later one, query-budget"
      (let ((a (title! RB rbD "2")))
        (and (pair? a) (eq? (car a) 'error)
             (let ((r (cadr (clause-of a 'reason)))) (list (head3 a) (car r) (cadr r) (caddr r)))))
      '((error refused rule-unevaluable) error query-budget (relation c3b)))
(rule! RB "a3" "--retire")
(rule! RB "b3" "--retire")
(rule! RB "four" "--on" "doc" "--must-not" "(c4 ?a ?b ?c ?d)")
(want "BUDGET-3 a stored relation that blows the budget: rule-unevaluable, query-budget"
      (let ((a (title! RB rbD "3")))
        (and (pair? a) (eq? (car a) 'error) (list (head3 a) (head-of (cadr (clause-of a 'reason))))))
      '((error refused rule-unevaluable) (error query-budget)))

;; ---- LOAD and SHAPE ------------------------------------------------------------------------
(printf "== LOAD and SHAPE ==\n")
(want "LOAD-1 GUARD: a library clause naming a stored relation does not load: unknown relation"
      (guard (e ((and (condition? e) (message-condition? e)) (condition-message e)))
        (check-rules (append rule-library (list '((usesstored ?x) (secs ?x)))))
        'loaded)
      "unknown relation")
(define SH (store! "shape"))
(make SH 'section "s")
(for-each (lambda (peer value) (forge! SH peer (list 'derive (string->symbol peer) value)))
          '("badheada" "badclaus" "badkinda" "badfalse" "badmarkr")
          '(((head badheada) (clause (badheada ?x) (kind ?x section)))
            ((head (badclaus ID)))
            ((head (badkinda FOO)) (clause (badkinda ?x) (kind ?x section)))
            #f
            ((head (badmarkr ID)) (clause (badmarkr ?x) (title ?x ("#%new" 1))))))
(want "SHAPE-1 on the replay route, a malformed head, no clause, an unknown kind, #f and a marker pair are refused: none is in the table"
      (map (lambda (n) (state-derived (state SH) n)) '(badheada badclaus badkinda badfalse badmarkr))
      '(#f #f #f #f #f))
(want "SHAPE-2 on the verb route, a cyclic pair, a cyclic vector and a shared vector are refused before anything copies them"
      (in-order (on SH 'derive "cy" "((head (cy ID)) . #0=((clause (cy ?x) (kind ?x section)) . #0#))")
                (on SH 'derive "cv" "((head (cv ID)) (clause (cv ?x) (title ?x #0=#(1 #0#))))")
                (on SH 'derive "sv" "((head (sv ID)) (clause (sv ?x) (title ?x #0=#(1))) (clause (sv ?x) (title ?x #0#)))"))
      '((error bad-request derive-malformed (reason derive-not-a-tree))
        (error bad-request derive-malformed (reason derive-not-a-tree))
        (error bad-request derive-malformed (reason derive-not-a-tree))))
(want "SHAPE-3 the plain string \"#%new\" is data: accepted, and answered"
      (in-order (head-of (derive! SH 'sn '((head (sn STR)) (clause (sn "#%new"))))) (query-rows SH '(sn ?x)))
      '(ok (("#%new"))))
;; A CURSOR IS WRITTEN writer:sequence, from the store's own position
(define (cursor-text st)
  (let ((p (cadr (assq 'position (cdr (on st 'position)))))) (string-append (car p) ":" (number->string (cdr p)))))
(define sh-req (format "~s" (list '(derive tracked ((head (tracked ID)) (clause (tracked ?x) (kind ?x section))))
                                  '(insert root #f ((kind . section) (title . "t"))))))
(define sh-cursor (cursor-text SH))
(define sh-first (on SH 'batch sh-req "--req" "TR1" "--cursor" sh-cursor))
(want "SHAPE-4 a tracked plan holding a derive intent round-trips: written, then the same request answers replay"
      (in-order (and (pair? sh-first) (car sh-first))
                (clause-of (first-item (on SH 'batch sh-req "--req" "TR1" "--cursor" sh-cursor)) 'replay)
                (car (state-derived (state SH) 'tracked)))
      '(batch (replay #t) in-force))

;; A RESENT RULE REQUEST IS ITS REPLAY: the rule names rr, rr is retired, and
;; the same request with the same cursor answers the replay; a new request of
;; the same rule is refused at the state it would write in
(define RP (store! "replay"))
(make RP 'section "s")
(derive! RP 'rr (over-kind 'rr 'section))
(define rp-args (list "q5" "--on" "doc" "--must-not" "(rr ?w)" "--req" "TR5" "--cursor" (cursor-text RP)))
(define rp-first (apply on RP 'rule rp-args))
(define rp-retire (retire! RP 'rr))
(want "SHAPE-5 a tracked rule naming a stored relation, resent after the relation is retired: the replay; a new request: relation-unavailable"
      (in-order (head-of rp-first) (clause-of rp-retire 'affects) (clause-of (apply on RP 'rule rp-args) 'replay)
                (on RP 'rule "q6" "--on" "doc" "--must-not" "(rr ?w)"))
      '(ok (affects (rule q5)) (replay #t) (error bad-request relation-unavailable (relation rr) (state retired))))

;; A PLAN HOLDING A DERIVE MEMBER: a commit-shaped request of two members, a
;; derive and an insert, written as one plan; the same request again is its
;; replay, which reads each member's record as the intent it declared
(define PL (store! "plan"))
;; a block first, so the store has a writer and the cursor a position
(make PL 'section "s")
(define (pl-request)
  (let ((w (car (car (reduce-applied-cut (open-and-reduce PL))))))
    (make-write-request "author" 'commit '() "PLANDRV01" (cons w (cdr (assoc w pl-cut))))))
(define pl-cut (reduce-applied-cut (open-and-reduce PL)))
(define pl-intents
  (list '(derive pd ((head (pd ID)) (clause (pd ?x) (kind ?x section)))) '(insert root #f ((kind . section) (title . "P")))))
(define (holds-replay? a) (and (pair? a) (or (equal? a '(replay #t)) (holds-replay? (car a)) (holds-replay? (cdr a)))))
(define pl-first (guard (e (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
                   (with-store-write PL (lambda (state view) pl-intents) "author" (pl-request) #f #t)))
(want "SHAPE-6 a plan holding a derive member: written, the relation in force, and the same request again is its replay"
      (in-order (holds-replay? pl-first)
                (car (state-derived (state PL) 'pd))
                (holds-replay? (guard (e (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
                                 (with-store-write PL (lambda (state view) pl-intents) "author" (pl-request) #f #t))))
      '(#f in-force #t))


;; A RULE VALUE'S ONE FORM, READ FROM ITS SHAPE: what the rule verb hands the
;; write session when its own read refused the value. For values the check
;; accepts, the same form the check gives; a shape the check refuses, as it
;; is. Looked up: the base has no such procedure.
(define rule-value-form
  (guard (e (#t (lambda (v) 'not-in-this-tree))) (eval 'rule-value-form (environment '(theourgia query)))))
(define form-values
  '(((on doc) (must-not (kind ?w section)))
    ((on doc section) (where (title ?w ?t)) (must (and (kind ?w doc) (title ?w ?t))))
    ((on section) (must-not (and (kind+ ?w section) (title ?w "x"))))
    ((on doc) (where (field+ ?w "title" ?v)) (must (title ?w ?v)))))
(want "RULES-17 a rule value's form from its shape is the check's form for every accepted value, write facts making the class write; another shape as it is"
      (list (map (lambda (v) (equal? (rule-value-form v) (rule-value-check v #f))) form-values)
            (map (lambda (v) (car (rule-value-form v))) form-values)
            (rule-value-form '((builtin citation-coverage)))
            (rule-value-form '((on doc))))
      '((#t #t #t #t) ((class state) (class state) (class write) (class write)) ((builtin citation-coverage)) ((on doc))))

;; THE RULE VERB GIVEN AN OLDER STATE: its own read refuses q (r retired
;; there), the write session reads the log, where r is in force again, and
;; accepts q, in its one form, the class from kind+
(define TS (store! "stale"))
(make TS 'doc "b")
(derive! TS 'r (over-kind 'r 'code))
(retire! TS 'r)
(define ts-old (state TS))
(derive! TS 'r (over-kind 'r 'code))
(want "RULES-18 the rule verb on a state older than the log: refused at its read, accepted by the write session in its one form"
      (in-order (head-of (rpc-dispatch TS '(rule "q" "--on" "doc" "--where" "(kind+ ?w doc)" "--must-not" "(r ?w)") "author" ts-old))
                (state-rule (state TS) 'q))
      '(ok (in-force ((class write) (on doc) (where (kind+ ?w doc)) (must-not (r ?w))))))
(want "RULES-19 a rule goal written with a cyclic datum label: refused rule-malformed, and the verb answers"
      (on TS 'rule "qc" "--on" "doc" "--must-not" "#0=(and #0#)")
      '(error bad-request rule-malformed))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nderived-relations complete\n" rows bad)
(exit (if (zero? bad) 0 1))
