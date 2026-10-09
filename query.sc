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

;;; (theourgia query) -- a language of queries and rules over one reduction.
;;;
;;; QUERIES AND RULES AND NOTHING ELSE, over ONE reduction at ONE cut. Its
;;; facts are what the store's providers already answer; its rules combine
;;; them; the executor evaluates one goal and answers every binding, in a
;;; canonical order, with a digest. It never reads the log, never evaluates
;;; user code, persists nothing and takes no rule over the wire.
;;;
;;; THE LANGUAGE. A term is a constant (a string, a symbol, an exact
;;; integer, or a list of those as the second argument of `member`), a
;;; variable (a symbol whose first character is `?`) or `_`, a variable
;;; never reported and never joined. The primitives are the fact relations
;;; and four tests on bound values: (= a b), (/= a b), (string< a b),
;;; (member x <list>). A rule body is a conjunction of goals read left to
;;; right; several rules with one head are a disjunction; a rule may call
;;; itself; a head argument is a variable or a constant. `(and <goal> ...)`
;;; is a conjunction as a goal, allowed only as a query. There is no
;;; negation: a program of positive rules has one least model whatever the
;;; order of evaluation.
;;;
;;; THE EXECUTOR. A call is a relation with its arguments, each a constant
;;; or unbound; its table is the set of ground answers found for it. A body
;;; is solved left to right: a fact goal asks its provider with what is
;;; bound so far, a rule goal reads its call's table, a test continues or
;;; fails. The query is the body of one synthetic rule whose head lists its
;;; named variables. A PASS expands that call; a call met for the first time
;;; in a pass is expanded there, and one met again in the same pass is read
;;; as its table stands. Passes repeat until one adds nothing to any table.
;;; Tables only grow and every tuple is derived from facts by rules, so the
;;; last pass leaves every touched table closed under its rules: the least
;;; model. No rule builds a value, so it ends; the budget bounds it. When a
;;; query's passes end every table it touched is complete, and a later query
;;; of the same session reads it without expanding it again.
;;;
;;; THE ANSWER. One row per distinct binding of the query's named variables,
;;; in order of first appearance; each row rendered as one datum the way a
;;; block hash renders a block (storable-encode, then the wire's extended
;;; printer); rows deduplicated and sorted by those bytes; the digest the
;;; sha256 of the rows joined by newlines. The cut is not in the digest.
(library (theourgia query)
  (export query-verb fact-relations rule-library rule-relations check-rules
          rule-only-facts relation-external? rule-value-check builtin-rules
          make-query-session session-query session-answer session-expansions session-spent session-passes query-relations-items
          query-relations-text
          query-budget-default)
  (import (rnrs) (rnrs mutable-pairs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids state-outline state-edges block-hash
                reduce-applied-cut state-field-contested? relation-kind effect-relation-names
                state-declared-relations kind-known? known-kinds)
          (only (theourgia store) library-locator defs-index search-state field-strings)
          (only (theourgia md) md-refs)
          (only (theourgia name-use) name-use-table import-library-name library-name-proper live-kind)
          (only (theourgia field-reading) field-of field-missing?)
          (only (theourgia wire) storable-encode sexpr->string-extended string->sexpr-extended)
          (only (theourgia digest) sha256 bytevector->hex)
          (prefix (theourgia lifecycle) lc:)
          (prefix (theourgia attest) at:)
          (only (theourgia extensions) query-usage))

  ;; ---- refusals --------------------------------------------------------------------

  ;; A refusal is an answer, raised to the verb from wherever it is found.
  (define-record-type (refusal make-refusal refusal?) (fields answer))
  (define (refuse . answer) (raise (make-refusal answer)))

  ;; ---- terms -------------------------------------------------------------------------

  (define (variable? x)
    (and (symbol? x) (let ((s (symbol->string x))) (and (> (string-length s) 0) (char=? (string-ref s 0) #\?)))))
  (define (wildcard? x) (eq? x '_))
  (define (simple-constant? x)
    (or (string? x) (and (integer? x) (exact? x)) (and (symbol? x) (not (variable? x)) (not (wildcard? x)))))
  (define (list-constant? x) (and (list? x) (for-all simple-constant? x)))

  ;; ---- the tables ---------------------------------------------------------------------

  ;; (<relation> <arity> "<one line>" stored|external). The provider of each
  ;; is in fact-tuples. THE FOURTH COLUMN SAYS WHETHER A FACT IS A FUNCTION OF
  ;; THE RECORDS ALONE (stored) or reads something outside the log (external):
  ;; `score` the keyword hook and the editor-supplied signature tables,
  ;; `uses-name` the name-use table, which consults the language catalogue
  ;; and the known-forms registry, both replaceable at run time, `def` the
  ;; defs index through the text view, which asks the language catalogue. A
  ;; rule may name only stored facts and rules over them, so its answer is a
  ;; function of the records and the rules (rule-value-check).
  (define fact-relations
    '((kind 2 "every live block, and its settled kind, else none, conflict or unreadable" stored)
      (class 2 "every live block, and its effective class" stored)
      (validity 2 "every live block, and its validity: valid, needs-review, refuted or superseded" stored)
      (version 2 "every live block that can be hashed, and its version" stored)
      (title 2 "a block whose title is a settled string, and that title" stored)
      (field 3 "every settled field of a live block: its name, and its value as stored" stored)
      (edge 3 "every surviving edge between two live blocks: from, relation, to" stored)
      (edge-kind 3 "an edge whose relation has a kind, built-in or declared, `nothing` included: from, that kind, to" stored)
      (relation 2 "every relation with an effect kind: the six by their own names, and each name declared in force with its kind, nothing included" stored)
      (under 2 "a live block and its settled parent, \"root\" at the top" stored)
      (ref 2 "a text reference [[id]] in a live block's text to a live block" stored)
      (library 2 "a library block and its name" stored)
      (in-library 2 "every code block, and the name of the library containing it, or none; nothing under a library whose name is contested" stored)
      (imports 2 "a library, by name, and a library name it imports" stored)
      (def 3 "a definition record: the block, its library's name or none, the name; nothing in a library whose name is contested" external)
      (uses-name 2 "a code block and a name it uses (name use, syntactic)" external)
      (lang 2 "a code block's language" stored)
      (validity-reason 3 "each reason of a block that is not valid, and the block that causes it" stored)
      (decision-state 2 "a live decision, and its state: closed, open, review, verified or implemented" stored)
      (moved 4 "an edge of an effect-bearing relation one of whose ends moved, and that end" stored)
      (moved-kind 4 "a moved edge with its relation's kind in place of its name, and the end that moved" stored)
      (verified-by 2 "a live block, and a live block with a current verifies edge to it" stored)
      (score 3 "exactly the hits search <text> returns with no cap, each with its score; text required" external)))

  (define tests '((= 2) (/= 2) (string< 2) (member 2)))

  ;; THE RULE LIBRARY, one table of data. Each rule is (<head> <goal> ...).
  ;; A RULE ABOUT AN EFFECT ASKS THE KIND (edge-kind, moved-kind), so an edge
  ;; under a declared name is read as its kind's; `edge` stays the literal
  ;; fact, and a query by a declared name still works.
  (define rule-library
    '(((depends ?a ?b) (edge-kind ?a depends-on ?b))
      ((depends ?a ?b) (edge-kind ?a implements ?b))
      ((depends* ?a ?b) (depends ?a ?b))
      ((depends* ?a ?b) (depends ?a ?c) (depends* ?c ?b))
      ((under* ?a ?b) (under ?a ?b))
      ((under* ?a ?b) (under ?a ?c) (under* ?c ?b))
      ((within ?d ?m) (under ?d ?m))
      ((within ?d ?m) (under ?c ?m) (within ?d ?c))
      ((supersedes* ?a ?b) (edge-kind ?a supersedes ?b))
      ((supersedes* ?a ?b) (edge-kind ?a supersedes ?c) (supersedes* ?c ?b))
      ((affects ?a ?b) (depends* ?b ?a))
      ((in-force ?x) (validity ?x ?v) (member ?v (valid needs-review)))
      ((authoritative ?x) (class ?x ?c) (member ?c (observation ruling verification)))
      ((unit ?t ?t) (kind ?t _))
      ((unit ?t ?u) (within ?u ?t))
      ((scope-of ?t ?m) (unit ?t ?m))
      ((scope-of ?t ?m) (unit ?t ?u) (depends* ?u ?m))
      ((unsettled ?d) (decision-state ?d ?s) (member ?s (open review)))
      ((unsettled-for ?t ?d) (scope-of ?t ?d) (unsettled ?d) (in-force ?d) (authoritative ?d))
      ((unsettled-for ?t ?d) (scope-of ?t ?m) (within ?d ?m) (unsettled ?d) (in-force ?d) (authoritative ?d))
      ((scope-name ?b ?n) (in-library ?b ?n))
      ((scope-name ?b ?n) (in-library ?b ?m) (imports ?m ?n))
      ((def-for ?b ?name ?d) (uses-name ?b ?name) (def ?d ?lib ?name) (/= ?lib none)
                             (scope-name ?b ?lib) (/= ?b ?d) (in-force ?d) (authoritative ?d))
      ((def-for ?b ?name ?d) (in-library ?b none) (uses-name ?b ?name) (def ?d none ?name)
                             (/= ?b ?d) (lang ?b ?l) (lang ?d ?l) (in-force ?d) (authoritative ?d))
      ((ambiguous ?b ?name) (def-for ?b ?name ?x) (def-for ?b ?name ?y) (string< ?x ?y))
      ((contradicts ?a ?b) (edge-kind ?a conflicts-with ?b) (in-force ?a) (in-force ?b)
                           (authoritative ?a) (authoritative ?b))
      ((contradicts ?a ?b) (edge-kind ?b conflicts-with ?a) (in-force ?a) (in-force ?b)
                           (authoritative ?a) (authoritative ?b))
      ((replaces ?c ?o) (edge-kind ?c supersedes ?o))
      ((replaces ?c ?o) (edge-kind ?c refutes ?o))
      ((evidence-for ?c ?v) (verified-by ?c ?v))
      ((evidence-for ?c ?v) (edge-kind ?v implements ?c))
      ((hard ?t ?h unit ?t) (unit ?t ?h))
      ((hard ?t ?h member ?t) (scope-of ?t ?h))
      ((hard ?t ?d unsettled ?t) (unsettled-for ?t ?d))
      ((hard ?t ?x supersedes ?m) (scope-of ?t ?m) (supersedes* ?x ?m))
      ((hard ?t ?b cause ?m) (scope-of ?t ?m) (validity-reason ?m _ ?b) (kind ?b _))
      ((hard ?t ?i implementer ?d) (unsettled-for ?t ?d) (decision-state ?d review)
                                   (moved-kind ?i implements ?d _))))

  ;; The one line each rule relation is described by.
  (define rule-lines
    '((depends "a depends-on or implements edge from a to b")
      (depends* "depends, transitively")
      (under* "b is an ancestor of a (walks up from a bound a)")
      (within "d is inside m (walks down from a bound m)")
      (supersedes* "a supersedes b, through a chain too")
      (affects "b depends on a, transitively")
      (in-force "x is valid or needs review")
      (authoritative "x's effective class is observation, ruling or verification")
      (unit "u is t or inside t")
      (scope-of "m is in t's unit, or what the unit depends on, transitively")
      (unsettled "a decision whose state is open or review")
      (unsettled-for "an unsettled, in-force, authoritative decision in t's scope or inside it")
      (scope-name "a library name visible to b: its own library and those it imports")
      (def-for "a definition d of a name b uses, in b's scope, in force and authoritative")
      (ambiguous "b uses a name two such definitions answer")
      (contradicts "a and b conflict, both in force and authoritative, either way round")
      (replaces "c supersedes or refutes o")
      (evidence-for "v verifies c currently, or implements it")
      (hard "what context must show for t: the block h, its role, and the block it is about")))

;; ---- rules of the store: what their goals may name ------------------------------------
  ;;
  ;; THE FACTS ONLY A RULE CHECK HAS, with their arities: the write's view of
  ;; its targets (the `+` facts) and the write's own evidence. No query,
  ;; context or premise session has them; a rule naming one is a WRITE rule.
  (define rule-only-facts
    '((kind+ 2) (field+ 3) (edge+ 3) (edge-kind+ 3) (cited 2) (unread 1) (receipt-carried 0)))
  ;; As heads with no body, so query-goals knows their arities when it checks
  ;; a rule's goals; never added to a session.
  (define rule-only-heads
    (map (lambda (f)
           (list (cons (car f) (let loop ((k (cadr f)) (out '()))
                                 (if (= k 0) out (loop (- k 1) (cons (string->symbol (string-append "?a" (number->string k))) out)))))))
         rule-only-facts))

  ;; A relation is EXTERNAL when it is a fact marked so, or a rule of the
  ;; library one of whose bodies names an external relation, through any
  ;; depth of rules.
  (define (relation-external? rel)
    (let walk ((rel rel) (seen '()))
      (cond
        ((assq rel fact-relations) => (lambda (f) (eq? (list-ref f 3) 'external)))
        ((memq rel seen) #f)
        (else
         (exists (lambda (rule)
                   (and (eq? (car (car rule)) rel)
                        (exists (lambda (g) (walk (car g) (cons rel seen))) (cdr rule))))
                 rule-library)))))

  ;; THE BUILT-IN RULES a store may enable by name.
  (define builtin-rules '(citation-coverage))

  ;; A RULE'S VALUE, as the `rule` verb or a batch's intent gives it, checked
  ;; and put in its one form: (class state|write) (on <kind> ...) [(where
  ;; <goal>)] (must <goal>)|(must-not <goal>), the class computed from the
  ;; goals; or ((builtin <name>)); or retired. -> the value, or a refusal
  ;; (error bad-request ...). Each goal is checked as a query's goals are,
  ;; with the rule-only facts known, and a relation that reads outside the
  ;; log is refused.
  (define (rule-value-check value)
    (define (clause k) (assq k value))
    (define (goal-relations g)
      (if (and (pair? g) (eq? (car g) 'and)) (map car (cdr g)) (list (car g))))
    (define (goal-refusal g)
      (or (guard (e ((refusal? e) (refusal-answer e)))
            (query-goals g (append rule-library rule-only-heads))
            #f)
          (let ((x (find relation-external? (goal-relations g))))
            (and x (list 'error 'bad-request 'rule-relation-not-allowed (list 'relation x))))))
    ;; A PLAN'S MARKER IS NOT DATA OF A RULE: a plan binds every ("#%new" k)
    ;; in a member it declares (request.sc, bind-new), so a rule value holding
    ;; one would not be the record it was declared as.
    (define (holds-marker? x)
      (and (pair? x) (or (equal? (car x) "#%new") (holds-marker? (car x)) (holds-marker? (cdr x)))))
    ;; A FINITE TREE: no pair or vector reached twice, so no walk below can
    ;; go round a cycle.
    (define (tree? x)
      (let ((seen (make-eq-hashtable)))
        (let walk ((x x))
          (cond ((or (pair? x) (vector? x))
                 (and (not (hashtable-ref seen x #f))
                      (begin (hashtable-set! seen x #t)
                             (if (pair? x)
                                 (and (walk (car x)) (walk (cdr x)))
                                 (for-all walk (vector->list x))))))
                (else #t)))))
    (cond
      ((eq? value 'retired) 'retired)
      ((not (tree? value)) '(error bad-request rule-malformed))
      ;; THE SHAPE FIRST, so no clause is read before it is known to be one.
      ((not (and (list? value) (pair? value)
                 (for-all (lambda (c) (and (list? c) (pair? c) (memq (car c) '(class on where must must-not builtin))))
                          value)))
       '(error bad-request rule-malformed))
      ((holds-marker? value) '(error bad-request (reason rule-value-holds-marker)))
      ((clause 'builtin)
       (let ((b (clause 'builtin)))
         (if (and (= 1 (length value)) (= 2 (length b)) (memq (cadr b) builtin-rules))
             value
             (list 'error 'bad-request 'builtin-rule-not-known
                   (list 'builtin (if (= 2 (length b)) (cadr b) (cdr b))) (list 'known builtin-rules)))))
      ((not (let ((o (clause 'on))) (and o (pair? (cdr o)) (for-all symbol? (cdr o)))))
       '(error bad-request (reason rule-needs-kinds)))
      ((find (lambda (k) (not (kind-known? k))) (cdr (clause 'on)))
       => (lambda (k) (list 'error 'bad-request 'kind-not-known (list 'kind k) (list 'known known-kinds))))
      ((not (= 1 (length (filter (lambda (c) (memq (car c) '(must must-not))) value))))
       '(error bad-request (reason rule-needs-one-of-must-must-not)))
      ((exists (lambda (k) (let ((c (clause k))) (and c (not (= 2 (length c)))))) '(where must must-not))
       '(error bad-request rule-malformed))
      (else
       ;; EVERY GOAL GIVEN IS CHECKED, whatever its value: a goal of #f is a
       ;; goal the query language refuses (not-a-goal), not an absent one.
       (let* ((goals (map cadr (filter values (map clause '(where must must-not)))))
              (bad (exists goal-refusal goals)))
         (or bad
             (let ((write? (exists (lambda (g) (exists (lambda (r) (assq r rule-only-facts)) (goal-relations g))) goals))
                   (polarity (find (lambda (c) (memq (car c) '(must must-not))) value)))
               (append (list (list 'class (if write? 'write 'state)) (clause 'on))
                       (if (clause 'where) (list (clause 'where)) '())
                       (list polarity))))))))

  ;; (<relation> <arity>) of every rule head, in the order first defined.
  (define (rule-relations rules)
    (let loop ((rs rules) (out '()))
      (if (null? rs)
          (reverse out)
          (let ((h (car (car rs))))
            (loop (cdr rs) (if (assq (car h) out) out (cons (list (car h) (length (cdr h))) out)))))))

  ;; ---- registration (checked when the library loads) -----------------------------------

  (define (arity-of rel rules)
    (cond ((assq rel fact-relations) => cadr)
          ((assq rel tests) => cadr)
          ((assq rel (rule-relations rules)) => cadr)
          (else #f)))

  (define (goal-variables g) (filter variable? (cdr g)))
  ;; WHAT A TEST NEEDS BOUND BY AN EARLIER GOAL: its variables, and `_`, which
  ;; no goal ever binds -- so a test that names it is refused.
  (define (test-variables g) (filter (lambda (a) (or (variable? a) (wildcard? a))) (cdr g)))
  ;; THE FIRST ARGUMENT OF A BODY GOAL THAT ITS POSITION DOES NOT ALLOW, as a
  ;; one-element list, or #f. Every position takes a variable, `_` or a
  ;; constant; a list constant is allowed only as member's second, which must
  ;; be one. Queries and rules are checked by this one procedure.
  (define (misplaced-term g)
    (let ((term? (lambda (a) (or (variable? a) (wildcard? a) (simple-constant? a)))))
      (if (eq? (car g) 'member)
          (cond ((not (term? (cadr g))) (list (cadr g)))
                ((not (list-constant? (caddr g))) (list (caddr g)))
                (else #f))
          (let loop ((as (cdr g)))
            (cond ((null? as) #f)
                  ((term? (car as)) (loop (cdr as)))
                  (else (list (car as))))))))

  ;; Each violation is an assertion naming the check and printing the rule.
  (define (check-rules rules)
    (define (bad what rule) (assertion-violation 'query what rule))
    (for-each
      (lambda (rule)
        (let ((head (car rule)) (body (cdr rule)))
          (unless (and (pair? head) (symbol? (car head))) (bad "a rule head is not a relation" rule))
          (when (eq? (car head) 'and) (bad "and in a rule" rule))
          (when (assq (car head) tests) (bad "a rule heads a test" rule))
          (when (assq (car head) fact-relations) (bad "a rule heads a fact relation" rule))
          (for-each (lambda (a) (unless (or (variable? a) (simple-constant? a)) (bad "a head argument is not a variable or a constant" rule)))
                    (cdr head))
          (for-each
            (lambda (g)
              (unless (pair? g) (bad "a goal is not a relation" rule))
              (when (eq? (car g) 'and) (bad "and in a rule" rule))
              (let ((n (arity-of (car g) rules)))
                (unless n (bad "unknown relation" rule))
                (unless (= n (length (cdr g))) (bad "wrong arity" rule))
                (when (misplaced-term g) (bad "a body argument is not a term of its position" rule))))
            body)
          (unless (= (arity-of (car head) rules) (length (cdr head))) (bad "wrong arity" rule))
          (let ((bound (apply append (map goal-variables (filter (lambda (g) (not (assq (car g) tests))) body)))))
            (for-each (lambda (v) (unless (memq v bound) (bad "a head variable in no body goal" rule)))
                      (goal-variables head)))
          (let loop ((gs body) (seen '()))
            (unless (null? gs)
              (let ((g (car gs)))
                (if (assq (car g) tests)
                    (begin
                      (for-each (lambda (v) (unless (memq v seen) (bad "a test variable not bound by an earlier goal" rule)))
                                (test-variables g))
                      (loop (cdr gs) seen))
                    (loop (cdr gs) (append (goal-variables g) seen))))))))
      rules)
    #t)

  (define library-checked (check-rules rule-library))

  ;; ---- the session ----------------------------------------------------------------------

  ;; ONE STATE, ONE REQUEST: the providers' builds, the tables and what is
  ;; complete, kept for every query of the session.
  (define-record-type (session new-session session?)
    (fields state rules index builds memo tables complete expansion-counts
            (mutable visited) (mutable changed) (mutable tuples) budget
            (mutable provider) (mutable attestation) (mutable consulted) keyword-hook (mutable passes)))

  (define query-budget-default 1000000)

  ;; Optional: extra rules (a fixture's), a budget, and the search verb's
  ;; keyword hook (a procedure of a state, as `search` passes it) for `score`.
  (define (make-query-session state . options)
    (let* ((extra (if (pair? options) (car options) '()))
           (budget (if (and (pair? options) (pair? (cdr options))) (cadr options) query-budget-default))
           (keyword-hook (and (pair? options) (pair? (cdr options)) (pair? (cddr options)) (caddr options)))
           (rules (append rule-library extra))
           (index (make-eq-hashtable)))
      (unless (null? extra) (check-rules rules))
      (for-each (lambda (r) (hashtable-set! index (car (car r)) (append (hashtable-ref index (car (car r)) '()) (list r))))
                rules)
      (new-session state rules index (make-eq-hashtable) (make-hashtable equal-hash equal?)
                   (make-hashtable equal-hash equal?) (make-hashtable equal-hash equal?)
                   (make-hashtable equal-hash equal?) (make-hashtable equal-hash equal?) #f 0 budget
                   #f #f #f keyword-hook 0)))

  ;; How many times one call was expanded in the session: (rel arg ...),
  ;; a variable for an unbound argument.
  (define (session-expansions S rel . args)
    (hashtable-ref (session-expansion-counts S) (cons rel (map (lambda (a) (if (variable? a) unbound a)) args)) 0))

  (define (provider S)
    (or (session-provider S)
        (let ((L (lc:lifecycle (session-state S)))) (session-provider-set! S L) L)))
  (define (attestation S)
    (or (session-attestation S)
        (let ((A (at:make-attestation (session-state S)))) (session-attestation-set! S A) A)))

  ;; THE BUDGET BOUNDS ONE QUERY'S NEW WORK: the relations it builds and the
  ;; tuples it derives, counted from zero at each query (session-query). A
  ;; build or a table complete from an earlier query of the session costs
  ;; nothing; that is what keeping them is for. A session's memory is bounded
  ;; by the tables it keeps, not by this counter.
  ;; The query's own table is named `query` in a refusal: its relation is a
  ;; string no goal can spell (session-query).
  (define (count! S rel n)
    (session-tuples-set! S (+ (session-tuples S) n))
    (when (> (session-tuples S) (session-budget S))
      (refuse 'error 'query-budget (list 'relation (if (string? rel) 'query rel)) (list 'tuples (session-tuples S)))))
  ;; What the session's last query was charged; its passes are session-passes.
  (define (session-spent S) (session-tuples S))

  ;; ---- the facts --------------------------------------------------------------------------

  ;; THE UNBOUND MARKER of a call's argument, one object no value can be.
  (define-record-type (unbound-marker make-unbound unbound?))
  (define unbound (make-unbound))

  (define (live-ids S) (state-block-ids (session-state S)))
  (define (row S id) (state-read (session-state S) id))
  (define (field S id name) (let ((r (row S id))) (if r (field-of r name) (field-of '((fields)) name))))
  ;; A FIELD IS CONTESTED when the reducer holds two or more surviving
  ;; candidates for it; its value cannot say so, since a value written once can
  ;; have the conflict's shape (state-field-contested?). SETTLED: present and
  ;; not contested.
  (define (contested? S id name v) (state-field-contested? (session-state S) id name v))
  (define (settled? S id name v) (not (or (field-missing? v) (contested? S id name v))))
  (define (kind-symbol S id)
    (let ((k (field S id 'kind)))
      (cond ((field-missing? k) 'none) ((contested? S id 'kind k) 'conflict) ((symbol? k) k) (else 'unreadable))))
  (define (written x) (call-with-string-output-port (lambda (p) (write x p))))
  ;; A library's name as the facts speak it: a string, `none` when the block
  ;; holds no library name, and #f when two writers contest it -- a contested
  ;; name is no name, and no fact speaks one for it (a shared token would join
  ;; every contested library with every other).
  ;; ONE NAME PER LIBRARY PER SESSION, kept by the library's id: the def
  ;; provider asks it for every definition record, and a contested name (#f)
  ;; is kept as well as a found one. The key's head is no fact relation, so
  ;; it never meets a call's key in the same table.
  (define no-entry (list 'no-entry))
  (define (library-name S id)
    (let* ((key (list 'library-name id))
           (hit (hashtable-ref (session-memo S) key no-entry)))
      (if (eq? hit no-entry)
          (let ((n (read-library-name S id)))
            (hashtable-set! (session-memo S) key n)
            n)
          hit)))
  ;; ONE LOCATOR PER SESSION: in-library and def both find a block's library,
  ;; and building one walks the whole outline.
  (define (session-locator S)
    (let ((hit (hashtable-ref (session-memo S) '(library-locator) no-entry)))
      (if (eq? hit no-entry)
          (let ((l (library-locator (session-state S) (live-kind (session-state S)))))
            (hashtable-set! (session-memo S) '(library-locator) l)
            l)
          hit)))
  (define (read-library-name S id)
    (let ((stored (field S id 'name)))
      (cond ((contested? S id 'name stored) #f)
            ((and (list? stored) (library-name-proper stored)) => written)
            (else 'none))))

  ;; Every tuple of a relation, built once per session.
  (define (all-tuples S rel)
    (or (hashtable-ref (session-builds S) rel #f)
        (let ((ts (build S rel)))
          (hashtable-set! (session-builds S) rel ts)
          (count! S rel (length ts))
          ts)))

  (define (build S rel)
    (let ((ids (live-ids S)) (st (session-state S)))
      (case rel
        ((kind) (map (lambda (id) (list id (kind-symbol S id))) ids))
        ((class) (map (lambda (id) (list id (lc:effective-class (row S id) (contested? S id 'class (field S id 'class))))) ids))
        ((validity) (map (lambda (id) (list id (car (lc:validity-of (provider S) id)))) ids))
        ((version) (filter values (map (lambda (id) (let ((h (guard (e (#t #f)) (block-hash st id)))) (and h (list id h)))) ids)))
        ((title) (filter values (map (lambda (id) (let ((t (field S id 'title))) (and (string? t) (list id t)))) ids)))
        ((field) (apply append
                        (map (lambda (id)
                               (map (lambda (f) (list id (symbol->string (car f)) (cdr f)))
                                    (filter (lambda (f) (settled? S id (car f) (cdr f))) (cdr (assq 'fields (row S id))))))
                             ids)))
        ((edge) (let ((live (live-table S)))
                  (filter values (map (lambda (e) (and (hashtable-ref live (car e) #f) (hashtable-ref live (caddr e) #f)
                                                       (list (car e) (cadr e) (caddr e))))
                                      (state-edges st)))))
        ;; A PLACEMENT THAT IS NOT SETTLED HAS NO PARENT: the outline shows such a
        ;; block at the root with a fourth element (conflict or unplaced), and
        ;; that row is left out.
        ((under) (map (lambda (r) (list (caddr r) (if (eq? (car r) 'root) "root" (car r))))
                      (filter (lambda (r) (null? (cdddr r))) (state-outline st))))
        ;; THE TEXT IS READ AS THE STORE READS IT (field-strings): a UTF-8
        ;; bytevector decoded, each candidate of a conflict. A block's reference
        ;; to itself is a reference.
        ((ref) (let ((live (live-table S)) (seen (make-hashtable equal-hash equal?)))
                 (apply append
                        (map (lambda (from)
                               (filter values
                                       (map (lambda (to)
                                              (let ((t (list from to)))
                                                (and (string? to) (hashtable-ref live to #f)
                                                     (not (hashtable-ref seen t #f))
                                                     (begin (hashtable-set! seen t #t) t))))
                                            (apply append (map (lambda (text) (map caddr (md-refs text)))
                                                               (field-strings (session-state S) from (row S from) 'src))))))
                             ids))))
        ((library) (filter values (map (lambda (id) (and (eq? (kind-symbol S id) 'library)
                                                         (let ((n (library-name S id)))
                                                           (and (string? n) (list id n)))))
                                       ids)))
        ((in-library) (let ((locate (session-locator S)))
                        (filter values (map (lambda (id) (and (eq? (kind-symbol S id) 'code)
                                                              (let* ((lib (locate id))
                                                                     (n (if lib (library-name S lib) 'none)))
                                                                (and n (list id n)))))
                                            ids))))
        ((imports) (apply append
                          (map (lambda (id)
                                 (let ((n (library-name S id)) (imps (field S id 'imports)))
                                   (if (and (string? n) (eq? (kind-symbol S id) 'library) (list? imps) (not (contested? S id 'imports imps)))
                                       (filter values (map (lambda (spec) (let ((m (import-library-name spec))) (and m (list n (written m))))) imps))
                                       '())))
                               ids)))
        ;; A DEFINITION'S LIBRARY is found as in-library finds it, and named as
        ;; the library facts name it: the record's stored name cannot say
        ;; whether two writers contest it.
        ((def) (let ((index (defs-index st)) (locate (session-locator S)) (out '()))
                 (vector-for-each
                   (lambda (records)
                     (for-each (lambda (r)
                                 (let ((lib (and (eq? (car r) 'def)
                                                 (let ((up (locate (cadr r)))) (if up (library-name S up) 'none)))))
                                   (when lib
                                     (set! out (cons (list (cadr r) lib (symbol->string (cadr (assq 'name (cddr r)))))
                                                     out)))))
                               records))
                   (let-values (((ks vs) (hashtable-entries index))) vs))
                 out))
        ((uses-name) (session-consulted-set! S #t)
                     (let ((by-name (vector-ref (name-use-table st) 0)) (out '()))
                       (vector-for-each (lambda (name) (for-each (lambda (id) (set! out (cons (list id name) out)))
                                                                 (hashtable-ref by-name name '())))
                                        (hashtable-keys by-name))
                       out))
        ((lang) (filter values (map (lambda (id) (let ((l (field S id 'lang))) (and (eq? (kind-symbol S id) 'code) (settled? S id 'lang l) (list id l)))) ids)))
        ((validity-reason) (apply append (map (lambda (id) (map (lambda (r) (list id (car r) (cadr r)))
                                                                (cdr (lc:validity-of (provider S) id))))
                                              ids)))
        ((decision-state) (filter values (map (lambda (id) (let ((s (lc:decision-state-of (provider S) id))) (and s (list id (car s))))) ids)))
        ((relation) (append (map (lambda (n) (list n n)) effect-relation-names)
                            (map (lambda (d) (list (car d) (cadr d)))
                                 (filter (lambda (d) (symbol? (cadr d))) (state-declared-relations st)))))
        ((moved) (let ((live (live-table S)) (A (attestation S)))
                   (apply append
                          (map (lambda (e)
                                 (let ((w (and (memq (relation-kind st (cadr e)) effect-relation-names) (hashtable-ref live (car e) #f)
                                               (hashtable-ref live (caddr e) #f) (at:edge-watch A (car e) (cadr e) (caddr e)))))
                                   (if (and w (eq? (car w) 'moved))
                                       (map (lambda (end) (list (car e) (cadr e) (caddr e) end)) (cdr w))
                                       '())))
                               (state-edges st)))))
        ((verified-by) (apply append (map (lambda (x) (map (lambda (v) (list x (car v)))
                                                           (filter (lambda (v) (eq? (cadr v) 'current)) (lc:verified-by (provider S) x))))
                                          ids)))
        (else (assertion-violation 'query "no provider" rel)))))

  ;; A VIEW OVER A BUILD: its tuples with the relation in the second place
  ;; replaced by that relation's kind, those of no kind left out, each once.
  ;; edge-kind is this over the edge build and moved-kind over the moved
  ;; build: the view shares the build and its charge and is charged nothing
  ;; of its own, so a query over edge-kind costs what the same query over
  ;; edge costs.
  (define (kind-view S rel)
    (let ((key (list 'kind-view rel)))
      (or (hashtable-ref (session-memo S) key #f)
          (let* ((st (session-state S))
                 (seen (make-hashtable equal-hash equal?))
                 (ts (filter values
                             (map (lambda (t)
                                    (let ((k (relation-kind st (cadr t))))
                                      (and k
                                           (let ((v (cons (car t) (cons k (cddr t)))))
                                             (and (not (hashtable-ref seen v #f))
                                                  (begin (hashtable-set! seen v #t) v))))))
                                  (all-tuples S rel)))))
            (hashtable-set! (session-memo S) key ts)
            ts))))

  (define (live-table S)
    (let ((t (make-hashtable string-hash string=?)))
      (for-each (lambda (id) (hashtable-set! t id #t)) (live-ids S))
      t))

  ;; The tuples of one fact call: those of the relation whose bound
  ;; positions hold the call's constants; memoized per call.
  (define (fact-tuples S rel args)
    (let ((key (cons rel args)))
      (or (hashtable-ref (session-memo S) key #f)
          (let ((ts (case rel
                      ((score)
                       (unless (string? (cadr args))
                         (refuse 'error 'bad-request 'unbound-argument '(relation score) '(position 2)))
                       ;; THE HITS THE SEARCH VERB WOULD RETURN: with no cap, and
                       ;; with its default filter, which leaves out what is
                       ;; superseded or refuted.
                       ;; The editor's keywords, as search consults them: the
                       ;; hook the verb handed the session, #f when it has none.
                       ;; ONE SEARCH PER TEXT IN A SESSION, charged once: the hits
                       ;; are a build keyed by the text, which every call with
                       ;; that text filters, whatever else it binds.
                       (let* ((key (list 'score-hits (cadr args)))
                              (hits (or (hashtable-ref (session-memo S) key #f)
                                        (let* ((r (search-state (session-state S) (cadr args) #f (session-keyword-hook S)
                                                                (lambda (st) (lc:search-filter (provider S) #f))))
                                               (hits (map (lambda (h) (list (car h) (cadr args) (cadr h))) (cdr (assq 'items r)))))
                                          (hashtable-set! (session-memo S) key hits)
                                          (count! S rel (length hits))
                                          hits))))
                         (filter-by args hits)))
                      ((version)
                       ;; A BOUND CALL HASHES ONE BLOCK, unless the session already
                       ;; built every version, which it then reads.
                       (if (or (unbound? (car args)) (hashtable-ref (session-builds S) rel #f))
                           (filter-by args (all-tuples S rel))
                           (let* ((h (and (member (car args) (live-ids S)) (guard (e (#t #f)) (block-hash (session-state S) (car args)))))
                                  (ts (if h (list (list (car args) h)) '())))
                             (count! S rel (length ts))
                             (filter-by args ts))))
                      ((edge-kind) (filter-by args (kind-view S 'edge)))
                      ((moved-kind) (filter-by args (kind-view S 'moved)))
                      (else (filter-by args (all-tuples S rel))))))
            (hashtable-set! (session-memo S) key ts)
            ts))))

  (define (filter-by args tuples)
    (filter (lambda (t) (for-all (lambda (a v) (or (unbound? a) (equal? a v))) args t)) tuples))

  ;; ---- the executor ------------------------------------------------------------------------

  (define (test-holds? rel vals)
    (case rel
      ((=) (equal? (car vals) (cadr vals)))
      ((/=) (not (equal? (car vals) (cadr vals))))
      ((string<) (and (string? (car vals)) (string? (cadr vals)) (string<? (car vals) (cadr vals))))
      ((member) (and (list? (cadr vals)) (member (car vals) (cadr vals)) #t))
      (else #f)))

  (define (value-of a b) (if (variable? a) (cdr (assq a b)) a))
  (define (call-args args b)
    (map (lambda (a) (cond ((wildcard? a) unbound)
                           ((variable? a) (let ((e (assq a b))) (if e (cdr e) unbound)))
                           (else a)))
         args))
  ;; Bindings extended so ARGS match TUPLE, or #f.
  (define (unify args tuple b)
    (let loop ((as args) (vs tuple) (b b))
      (cond ((null? as) b)
            ((wildcard? (car as)) (loop (cdr as) (cdr vs) b))
            ((variable? (car as))
             (let ((e (assq (car as) b)))
               (cond ((not e) (loop (cdr as) (cdr vs) (cons (cons (car as) (car vs)) b)))
                     ((equal? (cdr e) (car vs)) (loop (cdr as) (cdr vs) b))
                     (else #f))))
            ((equal? (car as) (car vs)) (loop (cdr as) (cdr vs) b))
            (else #f))))

  (define (table S key)
    (or (hashtable-ref (session-tables S) key #f)
        (let ((t (cons (make-hashtable equal-hash equal?) '())))
          (hashtable-set! (session-tables S) key t)
          t)))
  (define (table-add! S key tuple)
    (let ((t (table S key)))
      (unless (hashtable-ref (car t) tuple #f)
        (hashtable-set! (car t) tuple #t)
        (set-cdr! t (cons tuple (cdr t)))
        (session-changed-set! S #t)
        (count! S (car key) 1))))
  (define (table-tuples S key) (reverse (cdr (table S key))))

  (define (solve S goals b k)
    (if (null? goals)
        (k b)
        (let* ((g (car goals)) (rel (car g)) (args (cdr g)))
          (cond
            ((assq rel tests)
             (when (test-holds? rel (map (lambda (a) (value-of a b)) args)) (solve S (cdr goals) b k)))
            ((assq rel fact-relations)
             (for-each (lambda (t) (let ((b2 (unify args t b))) (when b2 (solve S (cdr goals) b2 k))))
                       (fact-tuples S rel (call-args args b))))
            (else
             (let ((key (cons rel (call-args args b))))
               (ensure-expanded S key)
               (for-each (lambda (t) (let ((b2 (unify args t b))) (when b2 (solve S (cdr goals) b2 k))))
                         (table-tuples S key))))))))

  (define (ensure-expanded S key)
    (unless (or (hashtable-ref (session-complete S) key #f)
                (hashtable-ref (session-visited S) key #f))
      (expand S key)))

  ;; The head matched against the call: the bindings it starts the body
  ;; with, or #f when a constant disagrees.
  (define (head-bindings head-args call)
    (let loop ((hs head-args) (cs call) (b '()))
      (cond ((null? hs) b)
            ((variable? (car hs))
             (let ((e (assq (car hs) b)))
               (cond ((unbound? (car cs)) (loop (cdr hs) (cdr cs) b))
                     ((not e) (loop (cdr hs) (cdr cs) (cons (cons (car hs) (car cs)) b)))
                     ((equal? (cdr e) (car cs)) (loop (cdr hs) (cdr cs) b))
                     (else #f))))
            ((or (unbound? (car cs)) (equal? (car hs) (car cs))) (loop (cdr hs) (cdr cs) b))
            (else #f))))

  (define (expand S key)
    (hashtable-set! (session-visited S) key #t)
    (hashtable-update! (session-expansion-counts S) key (lambda (n) (+ n 1)) 0)
    (for-each
      (lambda (rule)
        (let ((b (head-bindings (cdr (car rule)) (cdr key))))
          (when b
            (solve S (cdr rule) b
                   (lambda (b2)
                     (let ((tuple (map (lambda (a) (value-of a b2)) (cdr (car rule)))))
                       ;; A SOLUTION THAT DISAGREES WITH A BOUND ARGUMENT OF THE CALL
                       ;; is not an answer of it (a head variable repeated, bound
                       ;; once by the call, once by the body).
                       (when (for-all (lambda (c v) (or (unbound? c) (equal? c v))) (cdr key) tuple)
                         (table-add! S key tuple))))))))
      (hashtable-ref (session-index S) (car key) '())))

  ;; ---- one query ------------------------------------------------------------------------------

  ;; The goals of a query, the variables it reports, checked as A3 says.
  (define (query-goals goal rules)
    (unless (and (pair? goal) (list? goal) (symbol? (car goal)))
      (refuse 'error 'bad-request 'not-a-goal (list 'goal goal)))
    (let ((goals (if (eq? (car goal) 'and) (cdr goal) (list goal))))
      (when (null? goals) (refuse 'error 'bad-request 'not-a-goal (list 'goal goal)))
      (for-each
        (lambda (g)
          (unless (and (pair? g) (list? g) (symbol? (car g)))
            (refuse 'error 'bad-request 'not-a-goal (list 'goal g)))
          (when (eq? (car g) 'and) (refuse 'error 'bad-request 'not-a-goal (list 'goal g)))
          (let ((n (arity-of (car g) rules)))
            (unless n
              (refuse 'error 'bad-request 'unknown-relation (list 'relation (car g))
                      (list 'known (append (map car fact-relations) (map car (rule-relations rules)) (map car tests)))))
            (unless (= n (length (cdr g)))
              (refuse 'error 'bad-request 'wrong-arity (list 'relation (car g)) (list 'arity n) (list 'given (length (cdr g))))))
          (let ((bad (misplaced-term g)))
            (when bad (refuse 'error 'bad-request 'not-a-term (list 'term (car bad))))))
        goals)
      (let loop ((gs goals) (seen '()))
        (unless (null? gs)
          (let ((g (car gs)))
            (if (assq (car g) tests)
                (begin
                  (for-each (lambda (v) (unless (memq v seen)
                                          (refuse 'error 'bad-request 'test-variable-unbound (list 'variable v))))
                            (test-variables g))
                  (loop (cdr gs) seen))
                (loop (cdr gs) (append (goal-variables g) seen))))))
      goals))

  (define query-counter 0)

  ;; -> (values <rows> <vars>), the rows as lists of values. Raises a refusal.
  (define (session-query S goal)
    (let* ((goals (query-goals goal (session-rules S)))
           (vars (let loop ((gs goals) (out '()))
                   (if (null? gs) (reverse out)
                       (loop (cdr gs) (fold-left (lambda (acc v) (if (memq v acc) acc (cons v acc))) out (goal-variables (car gs)))))))
           ;; KEY: THE QUERY'S OWN RELATION IS A STRING, which no goal and no
           ;; rule head can spell (both must be symbols): a session rule of any
           ;; name is never displaced by it. Each query of a session has its own.
           (name (begin (set! query-counter (+ query-counter 1))
                        (string-append "query " (number->string query-counter))))
           (key (cons name (map (lambda (v) unbound) vars))))
      (hashtable-set! (session-index S) name (list (cons (cons name vars) goals)))
      (session-tuples-set! S 0)
      (session-passes-set! S 0)
      (let loop ()
        (session-passes-set! S (+ (session-passes S) 1))
        (session-changed-set! S #f)
        (session-visited-set! S (make-hashtable equal-hash equal?))
        (expand S key)
        (when (session-changed S) (loop)))
      ;; EVERY TABLE THE PASSES TOUCHED IS COMPLETE: the last pass expanded each
      ;; against tables that did not change.
      (vector-for-each (lambda (k) (hashtable-set! (session-complete S) k #t))
                       (hashtable-keys (session-visited S)))
      (values (table-tuples S key) vars)))

  ;; ---- the answer ---------------------------------------------------------------------------

  ;; THE BYTES OF A ROW, as a block hash renders a block.
  ;; WHEN A ROW CANNOT BE RENDERED, THE PROBE RENDERS WHAT THE ROW RENDERS: the
  ;; row with the values after position i replaced by 0, for i = 1, 2, ...;
  ;; the first that fails names value i's relation. A probe of one value alone
  ;; cannot see a level that depends on the row: the encoder wraps a whole
  ;; list whose first value is a `#%` string, so the same value fits alone
  ;; and not in the row. Each prefix keeps the first value, so it keeps that
  ;; wrapper, and an atom in place of a value never adds depth.
  (define (row-prefix row i)
    (let loop ((r row) (j 0) (out '()))
      (if (null? r) (reverse out) (loop (cdr r) (+ j 1) (cons (if (< j i) (car r) 0) out)))))
  (define (render-row row vars goals)
    (guard (e (#t (let ((bad (let loop ((i 1) (vs vars))
                               (cond ((null? vs) #f)
                                     ((guard (e2 (#t #t)) (sexpr->string-extended (storable-encode (row-prefix row i))) #f) (car vs))
                                     (else (loop (+ i 1) (cdr vs)))))))
                    (refuse 'error 'unrenderable
                            (list 'relation (if bad (relation-binding bad goals) (car (car goals))))
                            '(reason nesting)))))
      (string->utf8 (sexpr->string-extended (storable-encode row)))))
  (define (relation-binding v goals)
    (let ((g (find (lambda (g) (and (not (assq (car g) tests)) (memq v (cdr g)))) goals)))
      (if g (car g) (car (car goals)))))

  (define (bytes<? a b)
    (let ((n (bytevector-length a)) (m (bytevector-length b)))
      (let loop ((i 0))
        (cond ((= i n) (< n m))
              ((= i m) #f)
              ((< (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #t)
              ((> (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #f)
              (else (loop (+ i 1)))))))

  ;; -> (values <rows sorted> <digest hex>)
  (define (canonical rows vars goals)
    (let* ((rendered (map (lambda (r) (cons (render-row r vars goals) r)) rows))
           (sorted (list-sort (lambda (x y) (bytes<? (car x) (car y))) rendered))
           (unique (let loop ((l sorted) (out '()))
                     (cond ((null? l) (reverse out))
                           ((and (pair? out) (bytevector=? (car (car out)) (car (car l)))) (loop (cdr l) out))
                           (else (loop (cdr l) (cons (car l) out))))))
           (joined (let loop ((l unique) (acc '()))
                     (if (null? l) (apply append (reverse acc))
                         (loop (cdr l) (cons (bytevector->u8-list (car (car l)))
                                             (if (null? acc) acc (cons '(10) acc))))))))
      (values (map cdr unique) (bytevector->hex (sha256 (u8-list->bytevector joined))))))

  ;; -> (ok <rows sorted> <vars> <digest>), or the refusal's answer.
  (define (session-answer S goal)
    (guard (e ((refusal? e) (refusal-answer e)))
      (let-values (((rows vars) (session-query S goal)))
        (let-values (((sorted digest) (canonical rows vars (query-goals goal (session-rules S)))))
          (list 'ok sorted vars digest)))))

  ;; --relations: the two tables, one item a relation.
  (define (query-relations-items)
    (append (map (lambda (f) (list 'fact (car f) (cadr f) (caddr f))) fact-relations)
            (map (lambda (r) (list 'rule (car r) (cadr r) (cadr (assq (car r) rule-lines))))
                 (rule-relations rule-library))))

  ;; THE README'S SECTION, generated from the two tables: docs-check compares
  ;; the README with this text, so neither can change alone.
  (define (query-relations-text)
    (define (line name arity text)
      (string-append "- `" (symbol->string name) "/" (number->string arity) "` -- " text "\n"))
    (string-append
      "Fact relations:\n\n"
      (apply string-append (map (lambda (f) (line (car f) (cadr f) (caddr f))) fact-relations))
      "\nTests, on bound values only: "
      (let loop ((ts tests) (acc ""))
        (if (null? ts) acc
            (loop (cdr ts) (string-append acc (if (string=? acc "") "" ", ")
                                          "`" (symbol->string (car (car ts))) "/" (number->string (cadr (car ts))) "`"))))
      ".\n\nRules of the library:\n\n"
      (apply string-append (map (lambda (r) (line (car r) (cadr r) (cadr (assq (car r) rule-lines))))
                                (rule-relations rule-library)))))

  ;; ---- the verb ---------------------------------------------------------------------------------

  (define (query-verb store actor args req options state writer cwd)
    (cond
      ;; A THIRD TABLE AFTER THE TWO: the store's declared relations,
      ;; (declared <name> <kind> (from <selector>) (to <selector>)), or
      ;; (declared <name> (contested)); none in a store with no declaration.
      ((argument-option options "--relations")
       (if (null? args)
           ((dispatch-helper 'guarded)
            (lambda ()
              ((dispatch-helper 'items)
               (append (query-relations-items)
                       (map (lambda (d) (cons 'declared d))
                            (state-declared-relations ((dispatch-helper 'reduction-for) store state)))))))
           ((dispatch-helper 'usage) query-usage)))
      ((not (= 1 (length args))) ((dispatch-helper 'usage) query-usage))
      (else
       ((dispatch-helper 'guarded)
        (lambda ()
          (let ((view ((dispatch-helper 'reduction-for) store state)))
            (let ((goal (guard (e (#t 'unreadable)) (string->sexpr-extended (car args)))))
              (if (eq? goal 'unreadable)
                  (list 'error 'bad-request 'not-a-goal (list 'goal (car args)))
                  (let* ((S (make-query-session view '() query-budget-default ((dispatch-helper 'keyword-hook) store)))
                         (a (session-answer S goal)))
                    (if (not (eq? (car a) 'ok))
                        a
                        (append ((dispatch-helper 'items) (map (lambda (r) (cons 'row r)) (cadr a)))
                                (list (list 'vars (caddr a)) (list 'digest (cadddr a)) (list 'cut (reduce-applied-cut view)))
                                (if (session-consulted S) '((name-use syntactic)) '()))))))))))))
)
