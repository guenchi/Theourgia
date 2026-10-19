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
;;; is a conjunction as a goal, allowed only as a query, at any depth: a
;;; nested and is its goals in place. There is no
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
          rule-kinds declared-rule-kinds rule-kind-mismatch library-relation-kinds query-kind-refusal
          rule-only-facts relation-external? rule-value-check builtin-rules
          make-query-session session-query session-answer session-expansions session-spent session-passes query-relations-items
          session-budget-set! refusal? refusal-answer session-rows
          query-relations-text
          query-budget-default
          derive-value-check derive-affects rule-names-stored? rule-value-form stored-walk-counter
          make-handler handler-name dispatch extra-handlers library-env rule-value-env session-env-of
          session-check session-evaluate)
  (import (rnrs) (rnrs mutable-pairs)
          (only (chezscheme) make-parameter make-weak-eq-hashtable)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids state-outline state-edges block-hash
                reduce-applied-cut state-field-contested? relation-kind effect-relation-names
                state-declared-relations kind-known? known-kinds
                state-derived state-derived-names state-derived-all state-declaration state-rule reduction?
                derive-value-reason state-declared-rules)
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

  ;; (<relation> <arity> "<one line>" stored|external <kinds>). The provider of each
  ;; is in fact-tuples. THE FOURTH COLUMN SAYS WHETHER A FACT IS A FUNCTION OF
  ;; THE RECORDS ALONE (stored) or reads something outside the log (external):
  ;; `score` the keyword hook and the editor-supplied signature tables,
  ;; `uses-name` the name-use table, which consults the language catalogue
  ;; and the known-forms registry, both replaceable at run time, `def` the
  ;; defs index through the text view, which asks the language catalogue. A
  ;; rule may name only stored facts and rules over them, so its answer is a
  ;; function of the records and the rules (rule-value-check).
  ;;
  ;; <kinds> IS EACH COLUMN'S KIND, exactly as wide as the provider emits
  ;; (see "the kinds" below): ID, STR, SYM, INT, ANY, PARENT (an ID or the
  ;; string "root"), NAME-OR-NONE (a string or the symbol none), or
  ;; (SYM <symbol> ...), one of those symbols. The kinds are read by the
  ;; static check alone; no provider and no evaluation consults them.
  (define fact-relations
    '((kind 2 "every live block, and its settled kind, else none, conflict or unreadable" stored (ID SYM))
      (class 2 "every live block, and its effective class" stored (ID SYM))
      (validity 2 "every live block, and its validity: valid, needs-review, refuted or superseded" stored (ID (SYM valid needs-review refuted superseded)))
      (version 2 "every live block that can be hashed, and its version" stored (ID STR))
      (title 2 "a block whose title is a settled string, and that title" stored (ID STR))
      (field 3 "every settled field of a live block: its name, and its value as stored" stored (ID STR ANY))
      (edge 3 "every surviving edge between two live blocks: from, relation, to" stored (ID SYM ID))
      (edge-kind 3 "an edge whose relation has a kind, built-in or declared, `nothing` included: from, that kind, to" stored (ID SYM ID))
      (relation 2 "every relation with an effect kind: the six by their own names, and each name declared in force with its kind, nothing included" stored (SYM SYM))
      (under 2 "a live block and its settled parent, \"root\" at the top" stored (ID PARENT))
      (ref 2 "a text reference [[id]] in a live block's text to a live block" stored (ID ID))
      (library 2 "a library block and its name" stored (ID STR))
      (in-library 2 "every code block, and the name of the library containing it, or none; nothing under a library whose name is contested" stored (ID NAME-OR-NONE))
      (imports 2 "a library, by name, and a library name it imports" stored (STR STR))
      (def 3 "a definition record: the block, its library's name or none, the name; nothing in a library whose name is contested" external (ID NAME-OR-NONE STR))
      (uses-name 2 "a code block and a name it uses (name use, syntactic)" external (ID STR))
      (lang 2 "a code block's language" stored (ID ANY))
      (validity-reason 3 "each reason of a block that is not valid, and the block that causes it" stored (ID SYM ID))
      (decision-state 2 "a live decision, and its state: closed, open, review, verified or implemented" stored (ID (SYM closed open review verified implemented)))
      (moved 4 "an edge of an effect-bearing relation one of whose ends moved, and that end" stored (ID SYM ID (SYM source target)))
      (moved-kind 4 "a moved edge with its relation's kind in place of its name, and the end that moved" stored (ID SYM ID (SYM source target)))
      (verified-by 2 "a live block, and a live block with a current verifies edge to it" stored (ID ID))
      (verified-moved 2 "a live block, and a live block with a verifies edge to it that it has moved past" stored (ID ID))
      (score 3 "exactly the hits search <text> returns with no cap, each with its score; text required" external (ID STR INT))))

  ;; (<test> <arity> <rule>): what the kind check asks of its operands --
  ;; meet (=: not disjoint, and both refined to the meet), free (/=: nothing,
  ;; an inequality across kinds is simply true), string (string<: each not
  ;; disjoint from STR), one-of (member: one element of the list not disjoint
  ;; from the operand).
  (define tests '((= 2 meet) (/= 2 free) (string< 2 string) (member 2 one-of)))

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
                                   (moved-kind ?i implements ?d _))
      ;; A VERIFICATION GONE STALE: a block in t's scope (t itself among them)
      ;; whose verifies edge's target has moved past it -- the target is hard,
      ;; about the verifier, so context shows it and the receipt holds it.
      ;; Stale is the lifecycle's word (verified-moved), not moved-kind's: an
      ;; edge under several names of kind verifies is stale only when the target
      ;; moved past every one of them.
      ((hard ?t ?c stale ?m) (scope-of ?t ?m) (verified-moved ?c ?m))))

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
    '((kind+ 2 (ID SYM)) (field+ 3 (ID STR ANY)) (edge+ 3 (ID SYM ID)) (edge-kind+ 3 (ID SYM ID))
      (cited 2 (ID ID)) (unread 1 (ID)) (receipt-carried 0 ())))
  ;; As heads with no body, so query-goals knows their arities when it checks
  ;; a rule's goals; never added to a session.
  (define rule-only-heads
    (map (lambda (f)
           (list (cons (car f) (let loop ((k (cadr f)) (out '()))
                                 (if (= k 0) out (loop (- k 1) (cons (string->symbol (string-append "?a" (number->string k))) out)))))))
         rule-only-facts))

  ;; A relation is EXTERNAL when it is a fact marked so, or a rule of the
  ;; library one of whose bodies names an external relation, through any
  ;; depth of rules -- and, given a STATE, a stored relation of that state
  ;; one of whose clauses does (OVERRIDE, when given, a value about to be
  ;; written, read in force in its place: the verb's check).
  (define (relation-external? rel . state+override)
    (define clauses
      (append rule-library
              (if (and (pair? state+override) (car state+override))
                  (let ((table (stored-table (car state+override) (and (pair? (cdr state+override)) (cadr state+override)))))
                    (apply append (map (lambda (e) (if (eq? (car (cdr e)) 'in-force) (stored-rules (cadr (cdr e))) '()))
                                       table)))
                  '())))
    ;; ONE VISIT PER RELATION: a relation already visited answered #f or
    ;; is on the walk's way, and the walk stops at the first #t, so a graph
    ;; is walked in its size, not in the number of its paths; a relation's
    ;; clauses are found by their head, not by reading every clause
    (let ((seen (make-eq-hashtable)) (by-head (make-eq-hashtable)))
      (for-each (lambda (rule) (hashtable-update! by-head (car (car rule)) (lambda (l) (cons rule l)) '()))
                (reverse clauses))
      (let walk ((rel rel))
        (cond
          ((assq rel fact-relations) => (lambda (f) (eq? (list-ref f 3) 'external)))
          ((hashtable-ref seen rel #f) #f)
          (else
           (hashtable-set! seen rel #t)
           (exists (lambda (rule) (exists walk (apply append (map goal-atom-names (cdr rule)))))
                   (hashtable-ref by-head rel '())))))))

;; ---- the goal forms: not, count, sum, min, max ------------------------------------------
  ;;
  ;; A FORM is a goal whose last argument is a goal: (not <goal>), (count ?n
  ;; <goal>), (sum ?s ?x <goal>), (min ?m ?x <goal>), (max ?m ?x <goal>).
  ;; Its goal is a nested scope (query-goals-with), evaluated to completion
  ;; before it is read (form-solve). The five names are no relation's.
  (define form-heads '(not count sum min max))
  (define (form-goal? g) (and (pair? g) (memq (car g) form-heads) #t))
  (define (form-arity head) (case head ((not) 1) ((count) 2) (else 3)))
  ;; a form's goal, its result variable and its aggregated column
  (define (form-sub g) (list-ref g (form-arity (car g))))
  (define (form-result g) (cadr g))
  (define (form-column g) (caddr g))
  ;; the goals of a conjunction as written, nested ands read through; for a
  ;; goal already checked
  (define (goal-conjuncts g)
    (if (and (pair? g) (eq? (car g) 'and) (list? g)) (apply append (map goal-conjuncts (cdr g))) (list g)))
  ;; EVERY ATOMIC GOAL of a goal, through `and` and the forms: ((<goal> <sign>
  ;; <depth>) ...), the sign negative under a not or inside an aggregate's
  ;; goal at any depth, the depth the number of forms around it. One walker
  ;; for every reader that classifies, scans or graphs a goal's relations;
  ;; each pair is read once, so a datum label's cycle or shared structure
  ;; ends (its check refuses it), and what is not a goal is passed over.
  (define (goal-atoms goal)
    (let ((seen (make-eq-hashtable)) (out '()))
      (let walk ((g goal) (sign 'positive) (depth 0))
        (when (and (pair? g) (list? g) (symbol? (car g)) (not (hashtable-ref seen g #f)))
          (hashtable-set! seen g #t)
          (cond ((eq? (car g) 'and) (for-each (lambda (c) (walk c sign depth)) (cdr g)))
                ((and (form-goal? g) (= (length (cdr g)) (form-arity (car g))))
                 (walk (form-sub g) 'negative (+ depth 1)))
                (else (set! out (cons (list g sign depth) out))))))
      (reverse out)))
  ;; the relations a goal names, through and and the forms, in order
  (define (goal-atom-names goal) (map (lambda (a) (car (car a))) (goal-atoms goal)))
  ;; EVERY VARIABLE of a goal at any depth, in the order met, each once: an
  ;; atom's argument positions (member's list is a constant), a form's result
  ;; and column and its goal's
  (define (goal-all-variables goal)
    (let ((seen (make-eq-hashtable)) (out '()))
      (define (note! v) (when (and (variable? v) (not (memq v out))) (set! out (cons v out))))
      (let walk ((g goal))
        (when (and (pair? g) (list? g) (symbol? (car g)) (not (hashtable-ref seen g #f)))
          (hashtable-set! seen g #t)
          (cond ((eq? (car g) 'and) (for-each walk (cdr g)))
                ((and (form-goal? g) (= (length (cdr g)) (form-arity (car g))))
                 (let take ((as (cdr g)) (k (- (form-arity (car g)) 1)))
                   (when (> k 0) (note! (car as)) (take (cdr as) (- k 1))))
                 (walk (form-sub g)))
                ((eq? (car g) 'member) (when (pair? (cdr g)) (note! (cadr g))))
                (else (for-each note! (cdr g))))))
      (reverse out)))

  ;; THE BUILT-IN RULES a store may enable by name.
  (define builtin-rules '(citation-coverage))

  ;; A RULE'S VALUE, as the `rule` verb or a batch's intent gives it, checked
  ;; and put in its one form: (class state|write) (on <kind> ...) [(where
  ;; <goal>)] (must <goal>)|(must-not <goal>), the class computed from the
  ;; goals; or ((builtin <name>)); or retired. -> the value, or a refusal
  ;; (error bad-request ...). Each goal is checked as a query's goals are,
  ;; with the rule-only facts known, and a relation that reads outside the
  ;; log is refused.
  ;;
  ;; THE KINDS ARE CHECKED UNLESS THE CALLER SAYS #f: the `rule` verb and a
  ;; batch's rule intent declare a rule and check them (?w an ID); a write's
  ;; revalidation of a stored rule (rules.sc, rule-plan) passes #f, so a rule
  ;; stored before the kinds keeps the behaviour it had and never turns a
  ;; write into rule-unevaluable for a kind.
  ;; STATE, the optional argument after KINDS?, is the state the rule is
  ;; checked at: its stored relations are known there (STORED), and a goal
  ;; naming one that is not available is refused relation-unavailable.
  (define (rule-value-check value . options)
    (define kinds? (or (null? options) (car options)))
    (define state (and (pair? options) (pair? (cdr options)) (cadr options)))
    (define (clause k) (assq k value))
    ;; The relations a goal names, through every nested and and every form
    ;; (goal-atoms), for a goal query-goals has accepted.
    (define (goal-relations g)
      (rule-value-goals g state)
      (goal-atom-names g))
    (define (goal-refusal g)
      (or (guard (e ((refusal? e) (refusal-answer e)))
            (rule-value-goals g state)
            #f)
          (let ((x (find (lambda (r) (relation-external? r state)) (goal-relations g))))
            (and x (list 'error 'bad-request 'rule-relation-not-allowed (list 'relation x))))
          (and kinds?
               (goals-kind-refusal (rule-value-goals g state) written-block-seed (rule-value-env state)))))
    ;; A PLAN'S MARKER IS NOT DATA OF A RULE: a plan binds every pair it reads
    ;; as a marker in a member it declares (request.sc, bind-new: a pair
    ;; headed "#%new" with a pair after it), so a rule value holding one would
    ;; not be the record it was declared as. Exactly what bind-new rewrites is
    ;; refused, and nothing else: the string "#%new" alone is data.
    (define (holds-marker? x)
      (and (pair? x)
           (or (and (equal? (car x) "#%new") (pair? (cdr x)))
               (holds-marker? (car x))
               (holds-marker? (cdr x)))))
    ;; A TREE OF PLAIN DATA: no pair or vector reached twice. A cycle would
    ;; send every walk below round it; shared structure, which a datum label
    ;; makes as well, would let a short text stand for a value every walk
    ;; reads exponentially many times. The same rule as a channel's datum
    ;; (channel.sc, plain-datum).
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
      ;; One clause of each kind, as the reducer reads a rule (reduce.sc,
      ;; rule-value-reason): a second where would otherwise go unchecked.
      ((let dup ((cs value) (seen '()))
         (cond ((null? cs) #f) ((memq (caar cs) seen) #t) (else (dup (cdr cs) (cons (caar cs) seen)))))
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

;; A RULE VALUE IN ITS ONE FORM, read from its shape alone: the class
  ;; computed from the relations its goals name (through every nested and),
  ;; as rule-value-check computes it for a value it accepts. For a value
  ;; whose check refused it at one state and is checked again at another (the
  ;; rule verb hands it to the write session): there it is either refused, by
  ;; its own check, or accepted, and then it must already be in its form. A
  ;; value of another shape is answered as it is, for its check to refuse.
  (define (rule-value-form value)
    ;; EACH PAIR ONCE: a goal may be a datum with labels, cyclic or shared,
    ;; which its check refuses; goal-atoms reads each pair once, in one pass,
    ;; through the forms too, and passes over what is no goal
    (define (names-write-fact? g) (exists (lambda (r) (and (assq r rule-only-facts) #t)) (goal-atom-names g)))
    (let ((polarity (and (list? value) (for-all pair? value)
                         (find (lambda (c) (memq (car c) '(must must-not))) value))))
      (if (or (not polarity) (assq 'builtin value) (not (assq 'on value))
              (exists (lambda (k) (let ((c (assq k value))) (and c (not (and (list? c) (= 2 (length c)))))))
                      '(where must must-not)))
          value
          (let ((write? (exists (lambda (k) (let ((c (assq k value))) (and c (names-write-fact? (cadr c)))))
                                '(where must must-not))))
            (append (list (list 'class (if write? 'write 'state)) (assq 'on value))
                    (if (assq 'where value) (list (assq 'where value)) '())
                    (list polarity))))))

  ;; (<relation> <arity>) of every rule head, in the order first defined.
  (define (rule-relations rules)
    (let loop ((rs rules) (out '()))
      (if (null? rs)
          (reverse out)
          (let ((h (car (car rs))))
            (loop (cdr rs) (if (assq (car h) out) out (cons (list (car h) (length (cdr h))) out)))))))

  ;; ---- the kinds ------------------------------------------------------------------------
  ;;
  ;; A STATIC CHECK, BESIDE ARITY AND BINDING: each fact column has a kind
  ;; (fact-relations, rule-only-facts), each test a rule (tests), and the
  ;; recursive library relations and `hard` a declaration (below); the other
  ;; library relations' kinds are inferred from their clauses. A query, a
  ;; rule value and a library clause are read left to right, each variable's
  ;; kind being the MEET of the columns it has stood in so far.
  ;;
  ;; KEY: THE ONE RULE IS DISJOINTNESS, NEVER INCLUSION. A goal is refused only
  ;; when two kinds that must share a value have no value in common (their
  ;; meet is BOTTOM): such a goal answers no row at every cut of every store.
  ;; A wide column followed by a narrow one is a filter, not an error, and no
  ;; goal that answers a row today is refused (Postel).
  ;;
  ;; A KIND, AS THE CHECK HOLDS IT, is a union of atoms: any, str, id, root
  ;; (the string "root"), sym, int and (syms <symbol> ...); the empty union is
  ;; BOTTOM. ID <= STR, the string "root" <= STR, every (syms ...) <= SYM,
  ;; everything <= ANY. A constant has its own kind: a string STR, a symbol
  ;; s the singleton (SYM s), an exact integer INT.
  ;;
  ;; NEVER: CONSULTED AT RUN TIME. The evaluator does not read a kind, so a
  ;; query or rule that passes this check answers exactly as before.

  ;; THE DECLARED LIBRARY RELATIONS: the recursive ones, which no order of
  ;; inference reaches, and `hard`, whose role column is the set of roles its
  ;; clauses name. Each clause of one must have head kinds CONTAINED in the
  ;; declaration (head-kind otherwise); every other relation's kinds are the
  ;; join, across its clauses, of each clause's head kinds.
  (define declared-rule-kinds
    '((depends* ID ID) (supersedes* ID ID) (under* ID PARENT) (within ID PARENT)
      (hard PARENT ID (SYM unit member unsettled supersedes cause implementer stale) PARENT)))

  (define (kind-union k)
    (case k
      ((ID) '(id)) ((STR) '(str)) ((SYM) '(sym)) ((INT) '(int)) ((ANY) '(any))
      ((PARENT) '(id root)) ((NAME-OR-NONE) '(str (syms none)))
      (else (if (and (pair? k) (eq? (car k) 'SYM) (for-all symbol? (cdr k)))
                (list (cons 'syms (cdr k)))
                (assertion-violation 'query "not a kind" k)))))
  (define (constant-kind c)
    (cond ((string? c) '(str)) ((symbol? c) (list (list 'syms c))) (else '(int))))

  ;; two atoms' meet, an atom, or #f for none
  (define (atom-meet a b)
    (cond ((eq? a 'any) b)
          ((eq? b 'any) a)
          ((and (symbol? a) (eq? a b)) a)
          ((eq? a 'str) (and (memq b '(id root)) b))
          ((eq? b 'str) (and (memq a '(id root)) a))
          ((eq? a 'sym) (and (pair? b) b))
          ((eq? b 'sym) (and (pair? a) a))
          ((and (pair? a) (pair? b))
           (let ((both (filter (lambda (x) (memq x (cdr b))) (cdr a)))) (and (pair? both) (cons 'syms both))))
          (else #f)))
  (define (atom<=? a b)
    (or (eq? b 'any) (equal? a b)
        (and (memq a '(id root)) (eq? b 'str))
        (and (pair? a) (eq? b 'sym))
        (and (pair? a) (pair? b) (for-all (lambda (x) (memq x (cdr b))) (cdr a)))))
  ;; a union with no atom another one contains, its symbol sets merged
  (define (kind-normal u)
    (let* ((syms (apply append (map cdr (filter pair? u))))
           (atoms (append (filter symbol? u)
                          (if (null? syms) '()
                              (list (cons 'syms (fold-left (lambda (acc x) (if (memq x acc) acc (append acc (list x)))) '() syms)))))))
      (let loop ((as atoms) (out '()))
        (cond ((null? as) (reverse out))
              ((or (exists (lambda (b) (and (not (equal? b (car as))) (atom<=? (car as) b))) atoms)
                   (member (car as) out))
               (loop (cdr as) out))
              (else (loop (cdr as) (cons (car as) out)))))))
  (define (kind-meet u v)
    (kind-normal (filter values (apply append (map (lambda (a) (map (lambda (b) (atom-meet a b)) v)) u)))))
  (define (kind-join u v) (kind-normal (append u v)))
  ;; containment of the sets the unions stand for: both are normalized first,
  ;; so a symbol set written as several atoms is read as one
  (define (kind<=? u v)
    (let ((u (kind-normal u)) (v (kind-normal v)))
      (for-all (lambda (a) (exists (lambda (b) (atom<=? a b)) v)) u)))
  ;; a kind as an answer names it: one of the named kinds when it is one, else
  ;; its atoms, BOTTOM for none
  (define (kind-name u)
    (let ((named (find (lambda (k) (let ((ku (kind-normal (kind-union k)))) (and (kind<=? ku u) (kind<=? u ku))))
                       '(ID STR SYM INT ANY PARENT NAME-OR-NONE))))
      (cond (named named)
            ((null? u) 'BOTTOM)
            ((and (= 1 (length u)) (pair? (car u))) (cons 'SYM (cdr (car u))))
            (else (map (lambda (a) (cond ((pair? a) (cons 'SYM (cdr a))) ((eq? a 'root) "root") (else (string->symbol (string-upcase (symbol->string a))))))
                       u)))))

  ;; the column kinds of a relation, given the library relations' table
  (define (relation-column-kinds rel rule-kinds-table)
    (cond ((assq rel fact-relations) => (lambda (f) (map kind-union (list-ref f 4))))
          ((assq rel rule-only-facts) => (lambda (f) (map kind-union (caddr f))))
          (else (hashtable-ref rule-kinds-table rel #f))))

  ;; ONE GOAL LIST, LEFT TO RIGHT. ENV holds each variable seen: (<variable>
  ;; <kind> ((<relation> <position>) . <column kind>) ...). -> ENV after the
  ;; goals, or a refusal: wrong-kind (C1, a constant; C3, a test's operand),
  ;; join-across-kinds (C2, a variable's meet is BOTTOM).
  (define (term-kind a env)
    (cond ((variable? a) (let ((e (assq a env))) (if e (cadr e) '(any))))
          ((wildcard? a) '(any))
          (else (constant-kind a))))
  (define (refined env a u)
    (let ((e (and (variable? a) (assq a env))))
      (if e (cons (list a u (caddr e)) (remq e env)) env)))
  (define (test-kinds g rule env)
    (case rule
      ((free) env)
      ((meet)
       (let* ((a (cadr g)) (b (caddr g)) (ka (term-kind a env)) (kb (term-kind b env)) (m (kind-meet ka kb)))
         (when (null? m)
           (refuse 'error 'bad-request 'wrong-kind (list 'test (car g)) '(operand 2)
                   (list 'kinds (list (kind-name ka) (kind-name kb)))))
         (refined (refined env a m) b m)))
      ((string)
       (let loop ((as (cdr g)) (n 1))
         (unless (null? as)
           (let ((k (term-kind (car as) env)))
             (when (null? (kind-meet k '(str)))
               (refuse 'error 'bad-request 'wrong-kind (list 'test (car g)) (list 'operand n) (list 'kind (kind-name k)) '(needs STR)))
             (loop (cdr as) (+ n 1)))))
       env)
      ((one-of)
       (let ((k (term-kind (cadr g) env)))
         (unless (exists (lambda (x) (pair? (kind-meet k (constant-kind x)))) (caddr g))
           (refuse 'error 'bad-request 'wrong-kind (list 'test (car g)) '(operand 1) (list 'kind (kind-name k))
                   (list 'got (caddr g))))
         env))
      (else env)))
  (define (relation-kinds g cols env)
    (if (not cols)
        env
        (let loop ((as (cdr g)) (cs cols) (n 1) (env env))
          (if (or (null? as) (null? cs))
              env
              (let ((a (car as)) (c (car cs)))
                (cond
                  ((wildcard? a) (loop (cdr as) (cdr cs) (+ n 1) env))
                  ((variable? a)
                   (let ((e (assq a env)) (here (cons (list (car g) n) c)))
                     (if e
                         (let ((m (kind-meet (cadr e) c)) (seen (append (caddr e) (list here))))
                           (when (null? m)
                             (refuse 'error 'bad-request 'join-across-kinds (list 'variable a)
                                     (list 'positions (map car seen))
                                     (list 'kinds (map (lambda (p) (kind-name (cdr p))) seen))))
                           (loop (cdr as) (cdr cs) (+ n 1) (cons (list a m seen) (remq e env))))
                         (loop (cdr as) (cdr cs) (+ n 1) (cons (list a c (list here)) env)))))
                  (else
                   (when (null? (kind-meet (constant-kind a) c))
                     (refuse 'error 'bad-request 'wrong-kind (list 'relation (car g)) (list 'position n)
                             (list 'column (kind-name c)) (list 'got a)))
                   (loop (cdr as) (cdr cs) (+ n 1) env))))))))
  ;; KINDS-OF answers a relation's column kinds, (test-rule . <rule>) for a
  ;; test, (form . <result-kind>) for a form, or #f when they are not known
  ;; (the goal is then not read).
  (define (goal-kinds goals env kinds-of)
    (let loop ((gs goals) (env env))
      (if (null? gs)
          env
          (let* ((g (car gs)) (k (kinds-of (car g))))
            (loop (cdr gs) (cond ((and (pair? k) (eq? (car k) 'test-rule)) (test-kinds g (cdr k) env))
                                 ((and (pair? k) (eq? (car k) 'form))
                                  ((cdr k) g env (lambda (goals env) (goal-kinds goals env kinds-of))))
                                 (else (relation-kinds g k env))))))))

  ;; A FORM'S KINDS. Its goal is read with the outer kinds flowing in and
  ;; refines nothing outside: after a not every variable has the kind it had
  ;; before. A constant or a join inside it that no cut can satisfy does not
  ;; refuse it: the goal is STATICALLY EMPTY (not holds, count 0, sum 0, min
  ;; and max no row), which evaluation finds by itself. The form's own rules
  ;; refuse, wherever the form stands: sum's column not disjoint from INT,
  ;; min's and max's not disjoint from both INT and STR (wrong-kind, with
  ;; (form ...)). count's and sum's results are INT; min's and max's the
  ;; column's kind met with INT or STR.
;; A GOAL NO CUT SATISFIES, read again one variable at a time, every other
  ;; argument `_`: each variable still takes its column's kind (a sum over a
  ;; title is over STR however empty its goal), and one that cannot keeps the
  ;; kind it had
  (define (lenient-kinds goal-kinds-of g env)
    (if (or (form-goal? g) (not (list? g)) (assq (car g) tests))
        env
        (let loop ((i 0) (env env))
          (if (= i (length (cdr g)))
              env
              (loop (+ i 1)
                    (if (variable? (list-ref (cdr g) i))
                        (guard (e ((refusal? e) env))
                          (goal-kinds-of (list (cons (car g) (let mask ((as (cdr g)) (j 0))
                                                                (if (null? as) '()
                                                                    (cons (if (= j i) (car as) '_) (mask (cdr as) (+ j 1)))))))
                                         env))
                        env))))))
  (define (form-result-kind g env goal-kinds-of)
    (define (static-emptiness? a)
      (and (pair? a) (pair? (cdr a)) (pair? (cddr a)) (memq (caddr a) '(wrong-kind join-across-kinds))
           (not (exists (lambda (c) (and (pair? c) (eq? (car c) 'form))) a))))
    ;; GOAL BY GOAL: one that no cut can satisfy leaves the kinds as they were
    ;; and the check goes on, so a later goal's own refusal (a nested form's
    ;; wrong-kind) is still met and a column's kind is still known
    (let ((inner (let loop ((cs (goal-conjuncts (form-sub g))) (env env))
                   (if (null? cs)
                       env
                       (loop (cdr cs) (guard (e ((and (refusal? e) (static-emptiness? (refusal-answer e)))
                                                 (lenient-kinds goal-kinds-of (car cs) env)))
                                        (goal-kinds-of (list (car cs)) env)))))))
      (case (car g)
        ((not) env)
        ((count) (cons (list (form-result g) '(int) '()) env))
        (else
         (let* ((x (form-column g)) (kx (term-kind x inner)))
           (when (if (eq? (car g) 'sum)
                     (null? (kind-meet kx '(int)))
                     (and (null? (kind-meet kx '(int))) (null? (kind-meet kx '(str)))))
             (refuse 'error 'bad-request 'wrong-kind (list 'form (car g)) (list 'variable x) (list 'kind (kind-name kx))))
           (cons (list (form-result g) (if (eq? (car g) 'sum) '(int) (kind-meet kx '(int str))) '()) env))))))

  ;; THE LIBRARY RELATIONS' KINDS, from RULES and DECLARED: -> a table of
  ;; relation -> column kinds, or a refusal (a clause's body as a query's;
  ;; kind-order when no relation left to infer has its body's kinds known --
  ;; a recursive relation must be declared; head-kind when a declared
  ;; relation's clause is not contained in its declaration). The inferred
  ;; ones first, each as soon as the relations its clauses name are known;
  ;; the declared ones' clauses last.
  (define (rule-kinds rules declared)
    (let* ((table (make-eq-hashtable))
           (rels (map car (rule-relations rules)))
           (clauses-of (lambda (r) (filter (lambda (c) (eq? (car (car c)) r)) rules)))
           (kinds-of (lambda (r) (let ((t (assq r tests))) (if t (cons 'test-rule (caddr t)) (relation-column-kinds r table)))))
           (head-kinds (lambda (c) (let ((env (goal-kinds (cdr c) '() kinds-of)))
                                     (map (lambda (a) (term-kind a env)) (cdr (car c))))))
           (known? (lambda (r) (or (assq r tests) (relation-column-kinds r table)))))
      (for-each (lambda (d) (hashtable-set! table (car d) (map kind-union (cdr d)))) declared)
      (let loop ((todo (filter (lambda (r) (not (assq r declared))) rels)))
        (unless (null? todo)
          (let ((r (find (lambda (r) (for-all (lambda (c) (for-all (lambda (g) (known? (car g))) (cdr c))) (clauses-of r)))
                         todo)))
            (unless r (refuse 'error 'kind-order (list 'relations todo)))
            (let ((heads (map head-kinds (clauses-of r))))
              (hashtable-set! table r (fold-left (lambda (acc h) (map kind-join acc h)) (car heads) (cdr heads))))
            (loop (remq r todo)))))
      (for-each
        (lambda (d)
          (for-each
            (lambda (c)
              (let loop ((hs (head-kinds c)) (ds (cdr d)) (n 1))
                (unless (or (null? hs) (null? ds))
                  (unless (kind<=? (car hs) (kind-normal (kind-union (car ds))))
                    (refuse 'error 'head-kind (list 'relation (car d)) (list 'position n)
                            (list 'declared (car ds)) (list 'clause (kind-name (car hs)))))
                  (loop (cdr hs) (cdr ds) (+ n 1)))))
            (clauses-of (car d))))
        (filter (lambda (d) (memq (car d) rels)) declared))
      table))

  ;; THE CHECK OF ONE GOAL LIST against the kinds its handlers in ENV give:
  ;; #f, or the refusal's answer. SEED is the environment before the first
  ;; goal.
  (define (goals-kind-refusal goals seed env)
    (guard (e ((refusal? e) (refusal-answer e)))
      (goal-kinds goals seed (lambda (r) (let ((h (dispatch env r))) (and h ((handler-kinds-of h) r)))))
      #f))
  ;; A CLIENT QUERY'S KIND CHECK: #f when the goal passes, or when
  ;; query-goals refuses it (the session answers that refusal as before);
  ;; else the kind refusal's answer.
  ;; ENV, when given, is the client session's environment, so a call to a
  ;; stored relation is read against its declared kinds; only the client
  ;; route (the query verb) asks this, and session-rows, the path a rule's
  ;; evaluation takes, runs no kind pass.
  (define (query-kind-refusal goal . env)
    (guard (e ((refusal? e) #f))
      (let ((env (if (pair? env) (car env) (session-env rule-library '()))))
        (goals-kind-refusal (query-goals goal env (query-reporter rule-library)) '() env))))
  ;; A RULE VALUE'S GOALS ARE ASKED WITH ?w BOUND TO THE WRITTEN BLOCK, so ?w
  ;; starts as an ID.
  (define written-block-seed (list (list '?w '(id) (list (cons '(written) '(id))))))

  ;; a library relation's column kinds, by name, or #f
  (define (library-relation-kinds rel)
    (let ((k (hashtable-ref library-kinds rel #f))) (and k (map kind-name k))))

  ;; A STORED RULE'S KIND MISMATCH, for a listing: #f, or the refusal its
  ;; first goal with one would meet if declared now. A stored rule is never
  ;; refused for it on a write (rules.sc asks rule-value-check without the
  ;; kinds), so this is information for its owner, never a verdict; a value
  ;; that is not a goal rule, or does not read, has none.
  (define (rule-kind-mismatch value)
    (guard (e (#t #f))
      (and (list? value)
           (exists (lambda (k)
                     (let ((c (assq k value)))
                       (and c (pair? (cdr c))
                            (goals-kind-refusal (rule-value-goals (cadr c)) written-block-seed (rule-value-env)))))
                   '(where must must-not)))))

  ;; ---- registration (checked when the library loads) -----------------------------------

  ;; ---- the handlers: one per kind of goal --------------------------------------------------
  ;;
  ;; KEY: A GOAL IS ANSWERED BY THE ONE HANDLER THAT APPLIES TO ITS RELATION.
  ;; Four handlers, built from the three tables (tests, fact-relations,
  ;; rule-only-facts) and a rule list, which stay as they are and keep their
  ;; readers: TEST, FACT, WRITE-FACT (a rule check's per-write facts) and
  ;; RULE. A new kind of goal is a new handler with its own predicate; the
  ;; four are not edited for it (steps 3 to 5).
  ;;
  ;; A handler: its name; applies? (relation -> boolean); arity-of
  ;; (relation -> arity); kinds-of (relation -> its column kinds, (test-rule
  ;; . <rule>) for a test, or #f when unknown); external? (relation ->
  ;; boolean); write? (whether it reads a write's facts); binds? (whether its
  ;; goal binds its variables for the goals after it -- a test's does not);
  ;; solve (session goal bindings rest -> unspecified, REST being the
  ;; continuation that solves the remaining goals with new bindings); check
  ;; (goal seen -> #f or the violation, (test-variable-unbound <variable>),
  ;; SEEN being the variables bound before the goal).
  ;; AVAILABLE (relation -> #f, or the clauses of its refusal: (state ...)
  ;; and, reached through other relations, (through ...)) is asked right
  ;; after the dispatch, before the arity: a relation that cannot answer at
  ;; this state is a refusal of the goal naming it, in the static pass. The
  ;; four built-in kinds are always available; a stored relation is when
  ;; its candidates are in force and its clauses check at the state.
  ;; TERMS (goal -> #f, or the first argument its position does not allow,
  ;; as a one-element list), EXPORTS (goal -> the variables bound after it)
  ;; and RESULT-KIND (goal, the kinds so far, the kind pass for a goal list
  ;; -> the kinds after it) are what a structured goal answers differently:
  ;; a form's arguments are not all terms, and it binds less than it names.
  ;; Every handler of a relation answers them by today's rules.
  (define-record-type (handler make-handler* handler?)
    (fields name applies? arity-of kinds-of external? write? binds? solve check available terms exports result-kind))
  ;; a handler that is always available (every built-in one, and a fixture's),
  ;; answering the three structured questions by the rules for a relation
  (define (make-handler name applies? arity-of kinds-of external? write? binds? solve check)
    (make-handler* name applies? arity-of kinds-of external? write? binds? solve check (lambda (rel) #f)
                   relation-terms (if binds? relation-exports (lambda (g) '())) relation-result-kind))
  (define (relation-terms g) (misplaced-term g))
  (define (relation-exports g) (goal-variables g))
  (define (relation-result-kind g env goal-kinds-of) (goal-kinds-of (list g) env))

  (define test-handler
    (make-handler 'TEST
                  (lambda (rel) (and (assq rel tests) #t))
                  (lambda (rel) (cadr (assq rel tests)))
                  (lambda (rel) (cons 'test-rule (caddr (assq rel tests))))
                  (lambda (rel) #f)
                  #f #f
                  (lambda (S g b rest)
                    (when (test-holds? (car g) (map (lambda (a) (value-of a b)) (cdr g))) (rest b)))
                  (lambda (g seen)
                    (let ((v (find (lambda (v) (not (memq v seen))) (test-variables g))))
                      (and v (list 'test-variable-unbound v))))))
  (define fact-handler
    (make-handler 'FACT
                  (lambda (rel) (and (assq rel fact-relations) #t))
                  (lambda (rel) (cadr (assq rel fact-relations)))
                  (lambda (rel) (map kind-union (list-ref (assq rel fact-relations) 4)))
                  (lambda (rel) (eq? (list-ref (assq rel fact-relations) 3) 'external))
                  #f #t
                  (lambda (S g b rest)
                    (for-each (lambda (t) (let ((b2 (unify (cdr g) t b))) (when b2 (rest b2))))
                              (fact-tuples S (car g) (call-args (cdr g) b))))
                  (lambda (g seen) #f)))
  ;; FACTS: ((<relation> <arity> <tuple> ...) ...), a session's per-write
  ;; facts; for a rule value's check, rule-only-facts, whose rows are never
  ;; read (a rule value is checked, not evaluated).
  (define (write-fact-handler facts)
    (make-handler 'WRITE-FACT
                  (lambda (rel) (and (assq rel facts) #t))
                  (lambda (rel) (cadr (assq rel facts)))
                  (lambda (rel) (let ((f (assq rel rule-only-facts))) (and f (map kind-union (caddr f)))))
                  (lambda (rel) #f)
                  #t #t
                  (lambda (S g b rest)
                    (let ((f (assq (car g) facts)))
                      (count! S (car g) (length (cddr f)))
                      (for-each (lambda (t) (let ((b2 (unify (cdr g) t b))) (when b2 (rest b2))))
                                (cddr f))))
                  (lambda (g seen) #f)))
  ;; RULES: the clauses whose heads are callable (the library's, and a
  ;; session's extra clauses). A stored rule's NAME is a constraint's
  ;; identifier, never a relation, and is registered nowhere.
  (define (rule-handler rules)
    (let ((rels (rule-relations rules)))
      (make-handler 'RULE
                    (lambda (rel) (and (assq rel rels) #t))
                    (lambda (rel) (cadr (assq rel rels)))
                    (lambda (rel) (relation-column-kinds rel library-kinds))
                    ;; the facts' column only, as the design gives it; whether a
                    ;; rule reads outside the log stays rule-value-check's own
                    ;; recursive question (relation-external?)
                    (lambda (rel) #f)
                    #f #t
                    (lambda (S g b rest)
                      (let ((key (cons (car g) (call-args (cdr g) b))))
                        (ensure-expanded S key)
                        (for-each (lambda (t) (let ((b2 (unify (cdr g) t b))) (when b2 (rest b2))))
                                  (table-tuples S key))))
                    (lambda (g seen) #f))))

  ;; THE FIXTURES' SEAM: handlers a fixture registers, in process, for the
  ;; extent of a parameterize. The product never binds it, and nothing in a
  ;; request, a stored constraint or an option reaches it. Its handlers are
  ;; read at each dispatch and are not validated (they come after the
  ;; validator), which is what lets a fixture reach the runtime rule.
  (define extra-handlers (make-parameter '()))

  ;; ---- the forms' handlers: NOT and AGG -----------------------------------------------------
  ;;
  ;; NOT answers (not <goal>), AGG (count ?n <goal>), (sum ?s ?x <goal>),
  ;; (min ?m ?x <goal>) and (max ?m ?x <goal>). They are in the session, the
  ;; rule-value and the stored-check environments, never in the library's:
  ;; the library stays positive. Their goal is checked by the same two passes
  ;; in a nested scope (query-goals-with), kind-checked with the outer kinds
  ;; flowing in (form-result-kind), and evaluated to completion before it is
  ;; read (form-solve). Neither reads outside the log nor a write's facts of
  ;; its own; what its goal names is read through goal-atoms.
  ;; A FORM'S ARGUMENTS: a variable where the result or the column stands
  ;; (`_` is no variable), a goal where the goal stands
  (define (form-terms g)
    (let ((goal? (lambda (x) (and (pair? x) (list? x) (symbol? (car x)))))
          (args (cdr g)))
      (let loop ((as args) (n 1))
        (cond ((null? as) #f)
              ((= n (form-arity (car g))) (if (goal? (car as)) (loop (cdr as) (+ n 1)) (list (car as))))
              ((variable? (car as)) (loop (cdr as) (+ n 1)))
              (else (list (car as)))))))
  (define not-handler
    (make-handler* 'NOT
                   (lambda (rel) (eq? rel 'not))
                   (lambda (rel) 1)
                   (lambda (rel) (cons 'form form-result-kind))
                   (lambda (rel) #f)
                   #f #f
                   (lambda (S g b rest) (form-solve S g b rest))
                   (lambda (g seen) #f)
                   (lambda (rel) #f)
                   form-terms
                   ;; nothing: no variable of its goal is visible after it
                   (lambda (g) '())
                   (lambda (g env goal-kinds-of) (form-result-kind g env goal-kinds-of))))
  (define agg-handler
    (make-handler* 'AGG
                   (lambda (rel) (and (memq rel '(count sum min max)) #t))
                   (lambda (rel) (form-arity rel))
                   (lambda (rel) (cons 'form form-result-kind))
                   (lambda (rel) #f)
                   #f #t
                   (lambda (S g b rest) (form-solve S g b rest))
                   (lambda (g seen) #f)
                   (lambda (rel) #f)
                   form-terms
                   ;; the result only
                   (lambda (g) (list (form-result g)))
                   (lambda (g env goal-kinds-of) (form-result-kind g env goal-kinds-of))))

  ;; ---- stored relations: the fifth handler ------------------------------------------------
  ;;
  ;; A STORED RELATION is a `derive` record's value, read at a state
  ;; (reduce.sc, state-derived): ((head (<name> <kind> ...)) (clause (<name>
  ;; <term> ...) <goal> ...) ...). Its clauses are data checked at the state
  ;; they are used at; nothing is recorded about them.
  ;;
  ;; THE GOAL NAMES THE NAMESPACE RESERVES: a form's head, a test, a fact
  ;; relation, a rule-only (per-write) fact, a library head. The verb refuses a stored
  ;; relation so named (with the declared edge relations and the stored
  ;; rules, which are reserved for the operator's sake and are no goal
  ;; names); a record that bypassed the verb under such a name is MASKED by
  ;; the STORED handler -- the handler that owns the name serves it, or it is
  ;; unknown where that handler does not apply -- whichever handlers an
  ;; environment holds. -> the owner, FORM TEST FACT WRITE-FACT RULE, or #f.
  (define (goal-name-owner rel)
    (cond ((memq rel form-heads) 'FORM)
          ((assq rel tests) 'TEST)
          ((assq rel fact-relations) 'FACT)
          ((assq rel rule-only-facts) 'WRITE-FACT)
          ((assq rel (rule-relations rule-library)) 'RULE)
          (else #f)))
  (define (stored-head v) (cadr (car v)))
  ;; a value's clauses as rules: (<head pattern> <goal> ...)
  (define (stored-rules v) (map (lambda (c) (cons (cadr c) (cddr c))) (cdr v)))
  (define (spelled-kind? k) (guard (e (#t #f)) (kind-union k) #t))
  ;; THE STORED RELATIONS OF A STATE, the masked ones left out: -> ((<name>
  ;; . <state-derived>) ...); OVERRIDE, when given, is (<name> <value>), a
  ;; value the `derive` verb is about to write, read in force in its place.
  (define (stored-table state override)
    (let ((table (filter (lambda (e) (not (goal-name-owner (car e))))
                         (if (and state (reduction? state)) (state-derived-all state) '()))))
      (if override
          (cons (cons (car override) (if (eq? (cadr override) 'retired) '(retired) (list 'in-force (cadr override))))
                (filter (lambda (e) (not (eq? (car e) (car override)))) table))
          table)))

;; THE COUNTING SEAM: #f, or a procedure the own check calls with (own
  ;; <relation>) and a sink read with (sink <relation>). A fixture binds it in
  ;; process to count the walk's work; nothing else reads it.
  (define stored-walk-counter (make-parameter #f))
  (define (walk-count! what rel) (let ((c (stored-walk-counter))) (when c (c what rel))))

  ;; THE AVAILABILITY OF A STATE'S STORED RELATIONS, one entry a state at its
  ;; applied cut: the declared table indexed by name; each relation's OWN
  ;; answer, checked once; the CLOSURE of the names asked so far (closed
  ;; under callees); and for each unavailable member its distance to the
  ;; nearest own-failing relation, its next hop and its sink.
  ;; A relation is UNAVAILABLE iff its own check fails (contested, retired,
  ;; or its clauses fail the static check against the heads they name), or
  ;; it reaches, through own-passing relations, one that fails. The answer
  ;; is the sink's own (state ...) and, for a relation that is not itself
  ;; the sink, (through <hop> ... <sink>): the SHORTEST chain to an
  ;; own-failing relation, ties by callee order.
  (define-record-type (stored-entry make-stored-entry stored-entry?)
    (fields cut order index own closure (mutable members) distance hop sink (mutable check-env)))
  ;; A NEW ENTRY carries its own checks' environment, whose STORED handler
  ;; reads the same entry's heads
  (define (new-stored-entry state override)
    (let ((table (stored-table state override)) (index (make-eq-hashtable)))
      (for-each (lambda (e) (hashtable-set! index (car e) (cdr e))) table)
      (let ((e (make-stored-entry (and state (reduction? state) (reduce-applied-cut state)) (map car table) index
                                  (make-eq-hashtable) (make-eq-hashtable) '()
                                  (make-eq-hashtable) (make-eq-hashtable) (make-eq-hashtable) #f)))
        (stored-entry-check-env-set! e (make-dispatch-env (list test-handler fact-handler (rule-handler rule-library)
                                                                not-handler agg-handler (stored-handler-of e))))
        e)))
  ;; THE TABLE BELONGS TO THE STATE AT ITS APPLIED CUT: every environment
  ;; built on a state shares one entry while the state has not moved, and a
  ;; state whose applied cut differs (the same reduction rehearsed, then
  ;; written to and published) gets a fresh one. A hypothetical head (the
  ;; derive verb's OVERRIDE) has an entry of its own, shared with nothing.
  (define stored-entries (make-weak-eq-hashtable))
  (define (stored-entry-for state override)
    (if (or override (not (and state (reduction? state))))
        (new-stored-entry state override)
        (let ((e (hashtable-ref stored-entries state #f)))
          (if (and e (equal? (stored-entry-cut e) (reduce-applied-cut state)))
              e
              (let ((fresh (new-stored-entry state #f)))
                (hashtable-set! stored-entries state fresh)
                fresh)))))
  (define (entry-in-force e rel)
    (let ((d (hashtable-ref (stored-entry-index e) rel #f))) (and d (eq? (car d) 'in-force) (cadr d))))
  ;; the stored relations an in-force relation's clauses name, in clause
  ;; order and goal order
  (define (entry-callees e rel) (map car (entry-signed-callees e rel)))
  ;; the same, each with whether the occurrence is NEGATIVE: under a not or
  ;; inside an aggregate's goal, at any depth (goal-atoms)
  (define (entry-signed-callees e rel)
    (let ((v (entry-in-force e rel)))
      (if v
          (filter (lambda (p) (hashtable-contains? (stored-entry-index e) (car p)))
                  (apply append (map (lambda (c) (map (lambda (a) (cons (car (car a)) (eq? (cadr a) 'negative)))
                                                      (apply append (map goal-atoms (cddr c)))))
                                     (cdr v))))
          '())))
  ;; STRATIFICATION, over the closure's own-passing members and their signed
  ;; edges: one pass of Tarjan's components. A member of a component holding
  ;; a negative edge (a cycle through a not or an aggregate, of any length,
  ;; itself included) is UNSTRATIFIED: its own answer becomes (state
  ;; (unstratified (cycle <r1> ... <rk>))), a sink of the search like any
  ;; own failure; the cycle is the shortest one from that member back to
  ;; itself inside the component, read in callee order.
  (define (entry-stratify! e members)
    (let ((index (make-eq-hashtable)) (low (make-eq-hashtable)) (on (make-eq-hashtable)) (stack '()) (n 0))
      (define (passing? v) (and (hashtable-contains? (stored-entry-closure e) v) (not (entry-own e v))))
      (define (lower! v x) (when (< x (hashtable-ref low v 0)) (hashtable-set! low v x)))
      (define (strong v)
        (hashtable-set! index v n) (hashtable-set! low v n) (set! n (+ n 1))
        (set! stack (cons v stack)) (hashtable-set! on v #t)
        (for-each (lambda (p)
                    (let ((w (car p)))
                      (when (passing? w)
                        (cond ((not (hashtable-contains? index w)) (strong w) (lower! v (hashtable-ref low w 0)))
                              ((hashtable-ref on w #f) (lower! v (hashtable-ref index w 0)))))))
                  (entry-signed-callees e v))
        (when (= (hashtable-ref low v 0) (hashtable-ref index v 0))
          (let ((comp (make-eq-hashtable)))
            (let pop ((members '()))
              (let ((w (car stack)))
                (set! stack (cdr stack)) (hashtable-set! on w #f) (hashtable-set! comp w #t)
                (if (eq? w v)
                    (let ((members (cons w members)))
                      (when (exists (lambda (u) (exists (lambda (p) (and (cdr p) (hashtable-contains? comp (car p))))
                                                        (entry-signed-callees e u)))
                                    members)
                        (for-each (lambda (m)
                                    (hashtable-set! (stored-entry-own e) m
                                                    (list (list 'state (list 'unstratified (cons 'cycle (component-cycle e m comp)))))))
                                  members)))
                    (pop (cons w members))))))))
      (for-each (lambda (v) (when (and (passing? v) (not (hashtable-contains? index v))) (strong v))) members)))
  ;; the shortest path from M back to M inside the component COMP, read by a
  ;; breadth-first search in callee order: -> (M <r2> ... <rk>)
  (define (component-cycle e m comp)
    (let ((parent (make-eq-hashtable)))
      (let search ((level (list m)))
        (let next ((vs level) (found '()))
          (if (null? vs)
              (if (null? found) (list m) (search (reverse found)))
              (let ((v (car vs)))
                (let try ((ps (entry-signed-callees e v)) (found found))
                  (cond ((null? ps) (next (cdr vs) found))
                        ((not (hashtable-contains? comp (car (car ps)))) (try (cdr ps) found))
                        ((eq? (car (car ps)) m)
                         (let path ((u v) (out '()))
                           (if (eq? u m) (cons m out) (path (hashtable-ref parent u #f) (cons u out)))))
                        ((or (eq? (car (car ps)) m) (hashtable-contains? parent (car (car ps)))) (try (cdr ps) found))
                        (else (hashtable-set! parent (car (car ps)) v) (try (cdr ps) (cons (car (car ps)) found)))))))))))
  ;; a relation's own answer, checked once an entry
  (define (entry-own e rel)
    (let ((known (hashtable-ref (stored-entry-own e) rel 'none)))
      (if (not (eq? known 'none))
          known
          (let ((answer (begin
                          (walk-count! 'own rel)
                          (let ((d (hashtable-ref (stored-entry-index e) rel #f)))
                            (case (car d)
                              ((contested) '((state contested)))
                              ((retired) '((state retired)))
                              (else (let ((r (stored-clauses-refusal rel (cadr d) (stored-entry-check-env e))))
                                      (and r (list (list 'state (list 'check r)))))))))))
            (hashtable-set! (stored-entry-own e) rel answer)
            answer))))
  ;; THE CLOSURE EXTENDED by NAMES' reach, and, when it grew, the search run
  ;; again over the whole closure: answers inside a closure never depend on
  ;; anything outside it, so a recomputation changes no earlier answer.
  ;; (i) the reach, read by head; an own-failing relation is a sink and its
  ;; callees are not followed; (ii) one multi-source reverse breadth-first
  ;; search from the own-failing members gives each member its distance;
  ;; (iii) each unavailable member's next hop is its first callee, in callee
  ;; order, at distance one less, and its sink the hop's sink, read once
  ;; each in order of increasing distance.
  (define (entry-prime! e names)
    (let ((closure (stored-entry-closure e)) (grew #f))
      (let reach ((todo (filter (lambda (n) (hashtable-contains? (stored-entry-index e) n)) names)))
        (unless (null? todo)
          (let ((rel (car todo)))
            (if (hashtable-contains? closure rel)
                (reach (cdr todo))
                (begin
                  (hashtable-set! closure rel #t)
                  (stored-entry-members-set! e (cons rel (stored-entry-members e)))
                  (set! grew #t)
                  (reach (if (entry-own e rel) (cdr todo) (append (entry-callees e rel) (cdr todo)))))))))
      (when grew
        (entry-stratify! e (reverse (stored-entry-members e)))
        (let ((members (reverse (stored-entry-members e)))
              (distance (stored-entry-distance e)) (hop (stored-entry-hop e)) (sink (stored-entry-sink e))
              (callers (make-eq-hashtable)))
          (hashtable-clear! distance) (hashtable-clear! hop) (hashtable-clear! sink)
          (for-each (lambda (v)
                      (unless (entry-own e v)
                        (for-each (lambda (c) (hashtable-update! callers c (lambda (l) (cons v l)) '()))
                                  (entry-callees e v))))
                    members)
          (let ((sources (filter (lambda (v) (entry-own e v)) members)))
            (for-each (lambda (v) (hashtable-set! distance v 0)) sources)
            ;; LEVEL BY LEVEL, so the members are read in the order the search
            ;; reached them, by distance
            (let search ((level sources) (reached '()))
              (if (null? level)
                  (for-each
                    (lambda (v)
                      (let ((d (hashtable-ref distance v #f)))
                        (if (= d 0)
                            (hashtable-set! sink v v)
                            (let ((h (find (lambda (c) (eqv? (hashtable-ref distance c #f) (- d 1))) (entry-callees e v))))
                              (hashtable-set! hop v h)
                              (walk-count! 'sink v)
                              (hashtable-set! sink v (hashtable-ref sink h #f))))))
                    (reverse reached))
                  (let next-level ((vs level) (next '()) (reached reached))
                    (if (null? vs)
                        (search (reverse next) reached)
                        (let* ((v (car vs)) (d (+ 1 (hashtable-ref distance v 0)))
                               ;; MARKED AS FOUND, so a caller naming v twice enters once
                               (found (fold-left (lambda (acc u)
                                                   (if (hashtable-contains? distance u)
                                                       acc
                                                       (begin (hashtable-set! distance u d) (cons u acc))))
                                                 '() (reverse (hashtable-ref callers v '())))))
                          (next-level (cdr vs) (append found next) (cons v reached))))))))))))
  ;; ONE RELATION'S AVAILABILITY in an entry: -> #f, or ((state
  ;; contested|retired|(check <refusal>)) [(through <hop> ... <sink>)]); the
  ;; chain is followed only here, for the relation asked
  (define (entry-available e rel)
    (entry-prime! e (list rel))
    (let ((d (hashtable-ref (stored-entry-distance e) rel #f)))
      (cond ((not d) #f)
            ((= d 0) (entry-own e rel))
            (else
             (append (entry-own e (hashtable-ref (stored-entry-sink e) rel #f))
                     (list (cons 'through (let chain ((v (hashtable-ref (stored-entry-hop e) rel #f)))
                                            (if (= (hashtable-ref (stored-entry-distance e) v 0) 0)
                                                (list v)
                                                (cons v (chain (hashtable-ref (stored-entry-hop e) v #f))))))))))))

  ;; THE STORED HANDLER of a state. applies? answers for every name declared
  ;; there and not masked -- in force, contested or retired, so a retired
  ;; name is never an unknown relation; arity-of and kinds-of read the
  ;; in-force head (asked only after available answered #f); solve is the
  ;; library rules' over the clauses, which the session indexes under the
  ;; name; available is the relation's availability, read from the state's
  ;; entry (stored-entry-for).
  ;; AVAILABILITY: in force, and its clauses check in the STORED-CHECK
  ;; environment of the same state WITH the kinds, every stored relation they
  ;; name read by its head; then the REACH: every stored relation its clauses
  ;; name, through any depth, available by the same reading of its own.
  (define (stored-handler state override) (stored-handler-of (stored-entry-for state override)))
  (define (stored-handler-of e)
    (make-handler* 'STORED
                   (lambda (rel) (hashtable-contains? (stored-entry-index e) rel))
                   ;; #f for one with no head in force: its own check reads the
                   ;; heads only, and the reach reports such a callee
                   (lambda (rel) (let ((v (entry-in-force e rel))) (and v (length (cdr (stored-head v))))))
                   (lambda (rel) (let ((v (entry-in-force e rel)))
                                   (and v (for-all spelled-kind? (cdr (stored-head v)))
                                        (map kind-union (cdr (stored-head v))))))
                   (lambda (rel) #f)
                   #f #t
                   (lambda (S g b rest)
                     (let ((key (cons (car g) (call-args (cdr g) b))))
                       (ensure-expanded S key)
                       (for-each (lambda (t) (let ((b2 (unify (cdr g) t b))) (when b2 (rest b2))))
                                 (table-tuples S key))))
                   (lambda (g seen) #f)
                   (lambda (rel) (entry-available e rel))
                   relation-terms relation-exports relation-result-kind))
  ;; the STORED-CHECK environment the handler checks its clauses in: TEST,
  ;; FACT, RULE over the library, STORED; no WRITE-FACT, so a stored
  ;; relation's availability is the same whatever facts a session carries
  (define (stored-check-env state . override)
    (make-dispatch-env (list test-handler fact-handler (rule-handler rule-library) not-handler agg-handler
                             (stored-handler state (and (pair? override) (car override))))))

  ;; ONE STORED RELATION'S CLAUSES, checked in ENV (its STORED-CHECK
  ;; environment) as a library clause is at load, refusals as a query's:
  ;; every head term a variable or a constant, the body's relations, arity,
  ;; terms and the test-variable rule (query-goals, the stored relations read
  ;; by their heads, not their availability), every head variable bound by a
  ;; goal that binds, the kinds of the body and the head contained in the
  ;; declared head kinds. -> #f, or the refusal's answer.
  (define (stored-clauses-refusal rel v env)
    (guard (e ((refusal? e) (refusal-answer e)))
      (let ((declared (cdr (stored-head v))))
        (cond
          ((find (lambda (k) (not (spelled-kind? k))) declared)
           => (lambda (k) (refuse 'error 'bad-request 'kind-not-known (list 'relation rel) (list 'kind k))))
          (else
           (for-each
             (lambda (c)
               (let ((head (cadr c)) (body (cddr c)))
                 (for-each (lambda (a) (unless (or (variable? a) (simple-constant? a))
                                         (refuse 'error 'bad-request 'not-a-term (list 'term a))))
                           (cdr head))
                 ;; NO AND IN A CLAUSE, as in the library: a body is its goals,
                 ;; read as they are by the evaluator, the reach and the
                 ;; external walk alike
                 (for-each (lambda (g) (when (eq? (car g) 'and)
                                         (refuse 'error 'bad-request 'and-in-a-clause (list 'relation rel))))
                           body)
                 (let* ((goals (if (null? body) '() (query-goals-with (cons 'and body) env (query-reporter rule-library) #f '())))
                        (bound (apply append (map (lambda (g) ((handler-exports (dispatch env (car g))) g)) goals))))
                   (for-each (lambda (x) (unless (memq x bound)
                                           (refuse 'error 'bad-request 'head-variable-unbound (list 'relation rel) (list 'variable x))))
                             (goal-variables head))
                   (let ((kenv (goal-kinds goals '() (lambda (r) (let ((h (dispatch env r))) (and h ((handler-kinds-of h) r))))))
                         ;; A CALLEE WITH NO HEAD IN FORCE gives no kinds: the
                         ;; head's containment is not read for this clause (an
                         ;; untyped variable would read as ANY), the reach is
                         ;; what reports the callee
                         (unknown (exists (lambda (a) (let ((h (dispatch env (car (car a))))) (and h (not ((handler-kinds-of h) (car (car a)))))))
                                          (apply append (map goal-atoms goals)))))
                     (let loop ((as (if unknown '() (cdr head))) (ds declared) (n 1))
                       (unless (null? as)
                         (let ((k (term-kind (car as) kenv)))
                           (unless (kind<=? k (kind-normal (kind-union (car ds))))
                             (refuse 'error 'bad-request 'head-kind (list 'relation rel) (list 'position n)
                                     (list 'declared (car ds)) (list 'clause (kind-name k)))))
                         (loop (cdr as) (cdr ds) (+ n 1))))))))
             (cdr v))
           #f)))))

  ;; AN ENVIRONMENT: the handler set of one place goals are read.
  (define-record-type (dispatch-env make-dispatch-env dispatch-env?)
    (fields handlers))
  ;; LIBRARY LOAD, and a session's extra clauses: TEST, FACT, RULE over
  ;; RULES; no WRITE-FACT, no STORED and no form, so a body naming one is an
  ;; unknown relation (the library stays positive).
  (define (library-env rules) (make-dispatch-env (list test-handler fact-handler (rule-handler rules))))
  ;; A RULE VALUE (the rule verb): the rule-only facts as WRITE-FACT, the
  ;; library as RULE, the forms, and the stored relations of STATE when one
  ;; is given.
  (define (rule-value-env . state)
    (make-dispatch-env (append (list test-handler fact-handler (write-fact-handler rule-only-facts) (rule-handler rule-library)
                                     not-handler agg-handler)
                               (if (and (pair? state) (car state)) (list (stored-handler (car state) #f)) '()))))
  ;; A SESSION: its per-write facts as WRITE-FACT (none but a rule check's),
  ;; the library and its extra clauses as RULE, the forms, the stored
  ;; relations of its state as STORED.
  (define (session-env rules write-facts . state)
    (make-dispatch-env (append (list test-handler fact-handler (write-fact-handler write-facts) (rule-handler rules)
                                     not-handler agg-handler)
                               (if (and (pair? state) (car state)) (list (stored-handler (car state) #f)) '()))))

  ;; THE ONE HANDLER FOR REL in ENV, or #f when none applies. More than one
  ;; is ambiguous-goal: the validators below make each environment's
  ;; handlers exclusive, so only a handler registered past them (the seam)
  ;; can reach it.
  ;; ON THE EVALUATOR'S PATH, so nothing is allocated unless the seam is
  ;; bound or two handlers apply: one walk finds the handler and goes on to
  ;; the end, since a second applicable one is the refusal.
  (define (dispatch env rel)
    (let* ((x (extra-handlers))
           (hs (if (null? x) (dispatch-env-handlers env) (append (dispatch-env-handlers env) x))))
      (let loop ((l hs) (found #f))
        (cond ((null? l) found)
              (((handler-applies? (car l)) rel)
               (if found
                   (refuse 'error 'ambiguous-goal (list 'relation rel)
                           (list 'handlers (map handler-name (filter (lambda (h) ((handler-applies? h) rel)) hs))))
                   (loop (cdr l) (car l))))
              (else (loop (cdr l) found))))))

  ;; EXCLUSIVITY AT SESSION CREATION, where acceptance changes: a relation
  ;; two of the session's handlers answer -- an extra clause heading a
  ;; per-write fact, a per-write fact named like a fact relation or a test,
  ;; or like a library rule's head -- is refused, naming the handlers, before
  ;; any goal is read. (A clause heading a test or a fact relation is
  ;; check-rules' refusal, as before.)
  (define (session-exclusive! env write-facts extra)
    (for-each
      (lambda (rel)
        (let ((hs (filter (lambda (h) ((handler-applies? h) rel)) (dispatch-env-handlers env))))
          (when (> (length hs) 1)
            (refuse 'error 'bad-request 'ambiguous-relation (list 'relation rel) (list 'handlers (map handler-name hs))))))
      (append (map car write-facts) (map (lambda (r) (car (car r))) extra))))

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
  ;; RULES are a library's own clauses; EXTRA, when given, a session's extra
  ;; clauses, checked with them in the library-load environment. A head naming
  ;; a per-write fact is refused for the library's own clauses only (an extra
  ;; clause doing it is the session's ambiguous-relation).
  (define (check-rules rules . extra)
    (define (bad what rule) (assertion-violation 'query what rule))
    (define all (if (pair? extra) (append rules (car extra)) rules))
    ;; THE HEADS FIRST, every clause's: the environment is built from the
    ;; heads, so a head naming a test or a fact relation is refused by its own
    ;; assertion before any body is dispatched (a body naming that relation
    ;; would otherwise meet two handlers first)
    (for-each
      (lambda (rule)
        (let ((head (car rule)))
          (unless (and (pair? head) (symbol? (car head))) (bad "a rule head is not a relation" rule))
          (when (eq? (car head) 'and) (bad "and in a rule" rule))
          (when (memq (car head) form-heads) (bad "a rule heads a form" rule))
          (when (assq (car head) tests) (bad "a rule heads a test" rule))
          (when (assq (car head) fact-relations) (bad "a rule heads a fact relation" rule))
          (when (and (memq rule rules) (assq (car head) rule-only-facts)) (bad "a rule heads a per-write fact" rule))
          (for-each (lambda (a) (unless (or (variable? a) (simple-constant? a)) (bad "a head argument is not a variable or a constant" rule)))
                    (cdr head))))
      all)
    (check-bodies all (library-env all) bad)
    #t)
  ;; THE BODIES of check-rules' clauses, in ENV, BAD its assertion
  (define (check-bodies all env bad)
    (define (handler-of rel rule) (or (dispatch env rel) (bad "unknown relation" rule)))
    (for-each
      (lambda (rule)
        (let ((head (car rule)) (body (cdr rule)))
          (for-each
            (lambda (g)
              (unless (pair? g) (bad "a goal is not a relation" rule))
              (when (eq? (car g) 'and) (bad "and in a rule" rule))
              (let ((n ((handler-arity-of (handler-of (car g) rule)) (car g))))
                (unless (= n (length (cdr g))) (bad "wrong arity" rule))
                (when (misplaced-term g) (bad "a body argument is not a term of its position" rule))))
            body)
          (unless (= ((handler-arity-of (handler-of (car head) rule)) (car head)) (length (cdr head))) (bad "wrong arity" rule))
          (let ((bound (apply append (map goal-variables (filter (lambda (g) (handler-binds? (handler-of (car g) rule))) body)))))
            (for-each (lambda (v) (unless (memq v bound) (bad "a head variable in no body goal" rule)))
                      (goal-variables head)))
          (let loop ((gs body) (seen '()))
            (unless (null? gs)
              (let* ((g (car gs)) (h (handler-of (car g) rule)))
                (when ((handler-check h) g seen) (bad "a test variable not bound by an earlier goal" rule))
                (loop (cdr gs) (if (handler-binds? h) (append (goal-variables g) seen) seen)))))))
      all))

  (define library-checked (check-rules rule-library))
  ;; NEVER: A LIBRARY CLAUSE THAT DOES NOT CHECK IS A BUILD ERROR, as an arity is.
  (define library-kinds
    (guard (e ((refusal? e) (assertion-violation 'query "a rule of the library does not check against the kinds"
                                                 (refusal-answer e))))
      (rule-kinds rule-library declared-rule-kinds)))

  ;; ---- the session ----------------------------------------------------------------------

  ;; ONE STATE, ONE REQUEST: the providers' builds, the tables and what is
  ;; complete, kept for every query of the session.
  (define-record-type (session new-session session?)
    (fields state rules index builds memo tables complete expansion-counts
            (mutable visited) (mutable changed) (mutable tuples) (mutable budget)
            (mutable provider) (mutable attestation) (mutable consulted) keyword-hook (mutable passes)
            write-facts env-of))

  (define query-budget-default 1000000)

  ;; Optional: extra rules (a fixture's), a budget, and the search verb's
  ;; keyword hook (a procedure of a state, as `search` passes it) for `score`.
  ;; OPTIONS: extra rules, the budget, the keyword hook, and THE PER-WRITE FACTS
  ;; -- ((<relation> <arity> <tuple> ...) ...), the facts only a rule check
  ;; of one write has (kind+, field+, edge+, edge-kind+, cited, unread,
  ;; receipt-carried). Only the rule check passes them; every other session
  ;; has none, and a goal naming one is an unknown relation there.
  (define (make-query-session state . options)
    (let* ((extra (if (pair? options) (car options) '()))
           (budget (if (and (pair? options) (pair? (cdr options))) (cadr options) query-budget-default))
           (keyword-hook (and (pair? options) (pair? (cdr options)) (pair? (cddr options)) (caddr options)))
           (write-facts (if (and (pair? options) (pair? (cdr options)) (pair? (cddr options)) (pair? (cdddr options)))
                            (cadddr options)
                            '()))
           (rules (append rule-library extra))
           (index (make-eq-hashtable))
           (env (session-env rules write-facts state)))
      (unless (null? extra) (check-rules rule-library extra))
      (unless (and (null? extra) (null? write-facts)) (session-exclusive! env write-facts extra))
      ;; THE STORED RELATIONS' CLAUSES ARE INDEXED WITH THE RULES, under their
      ;; names, so the fixpoint that answers a library head answers them; a
      ;; masked name is not among them, nor one not in force (a goal naming
      ;; that is refused before evaluation)
      (for-each (lambda (r) (hashtable-set! index (car (car r)) (append (hashtable-ref index (car (car r)) '()) (list r))))
                (append rules
                        (apply append (map (lambda (e) (if (eq? (car (cdr e)) 'in-force) (stored-rules (cadr (cdr e))) '()))
                                           (stored-table state #f)))))
      (new-session state rules index (make-eq-hashtable) (make-hashtable equal-hash equal?)
                   (make-hashtable equal-hash equal?) (make-hashtable equal-hash equal?)
                   (make-hashtable equal-hash equal?) (make-hashtable equal-hash equal?) #f 0 budget
                   #f #f #f keyword-hook 0 write-facts env)))

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
        ((verified-moved) (apply append (map (lambda (x) (map (lambda (v) (list x (car v)))
                                                              (filter (lambda (v) (eq? (cadr v) 'moved)) (lc:verified-by (provider S) x))))
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

  ;; EACH GOAL BY ITS ONE HANDLER: the dispatcher finds it, the handler
  ;; solves the goal and calls REST with each new binding.
  (define (solve S goals b k)
    (if (null? goals)
        (k b)
        (let* ((g (car goals))
               (h (or (dispatch (session-env-of S) (car g))
                      (refuse 'error 'bad-request 'unknown-relation (list 'relation (car g))))))
          ((handler-solve h) S g b (lambda (b2) (solve S (cdr goals) b2 k))))))

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

;; ---- a form, evaluated -------------------------------------------------------------------
  ;;
  ;; A FORM'S GOAL IS A RELATION OF ITS OWN, named by the occurrence ("form
  ;; n", a string no goal can spell), whose head is the GROUP, the goal's
  ;; variables bound outside it (at any depth), seeded as the call's
  ;; arguments (opaque values, never written back into the goal), and its
  ;; LOCALS, what the goal's own conjuncts export. A nested form's locals
  ;; are its own and no column of this one. Its table is completed before it is read: a NESTED pass loop
  ;; as session-evaluate's, over the call's key and all it reaches, until a
  ;; pass adds nothing; then every key it touched is complete, and a later
  ;; call of the same group reads the complete table. A refusal inside
  ;; unwinds the query as any does, and marks nothing complete.
  ;; ONE NAME AN OCCURRENCE AND ITS HEAD: the same goal met where other
  ;; variables are bound outside it (one datum shared by two queries) has
  ;; another head, so another table
  (define form-names (make-weak-eq-hashtable))
  (define form-counter 0)
  (define (form-name g vars)
    (let* ((known (hashtable-ref form-names g '())) (e (assoc vars known)))
      (if e
          (cdr e)
          (begin (set! form-counter (+ form-counter 1))
                 (let ((n (string-append "form " (number->string form-counter))))
                   (hashtable-set! form-names g (cons (cons vars n) known))
                   n)))))
  (define (complete-key! S key)
    (unless (hashtable-ref (session-complete S) key #f)
      (let ((outer-visited (session-visited S)) (outer-changed (session-changed S)))
        (let loop ((added #f))
          (session-changed-set! S #f)
          (session-visited-set! S (make-hashtable equal-hash equal?))
          (expand S key)
          (if (session-changed S)
              (loop #t)
              (begin
                (vector-for-each (lambda (k) (hashtable-set! (session-complete S) k #t))
                                 (hashtable-keys (session-visited S)))
                (session-visited-set! S outer-visited)
                ;; ITS ADDITIONS ARE THE OUTER PASS'S CHANGE: one more outer
                ;; pass, which finds these tables complete
                (session-changed-set! S (or outer-changed added))))))))
  ;; THE ANSWER OF A FORM over its completed rows: not holds iff there is
  ;; none; count is their number (the table holds each row once: distinct
  ;; rows over the locals, the group fixed); sum adds the column's exact
  ;; integers (0 over none); min and max take the least or greatest of
  ;; integers or of strings, by bytes (no row over none). A value the form
  ;; cannot take refuses the query (aggregate-value).
;; A STATICALLY EMPTY GOAL, read with the group's values as their kinds: a
  ;; constant or a join inside it no cut can satisfy. Its form evaluates
  ;; nothing (not holds, count and sum 0, min and max no row).
  (define (value-kind v)
    (cond ((string? v) '(str)) ((symbol? v) (list (list 'syms v))) ((and (integer? v) (exact? v)) '(int)) (else '(any))))
  ;; A FORM'S OWN REFUSAL MET WITH THE VALUES' KINDS (a nested sum whose
  ;; column the group's value makes a string) is no emptiness of the goal
  ;; around it: the form gives the kinds it still can -- a not none, count's
  ;; and sum's result INT, min's and max's their column's kind (its goal read
  ;; the same way) met with INT or STR -- and the goals after it are read.
  (define (form-own-refusal? a) (and (pair? a) (exists (lambda (c) (and (pair? c) (eq? (car c) 'form))) a)))
  (define (forms-lenient kinds-of)
    (letrec ((lenient
              (lambda (r)
                (let ((k (kinds-of r)))
                  (if (and (pair? k) (eq? (car k) 'form))
                      (cons 'form
                            (lambda (g env recur)
                              (guard (e ((and (refusal? e) (form-own-refusal? (refusal-answer e))) (form-kinds-left g env recur)))
                                ((cdr k) g env recur))))
                      k)))))
      lenient))
  (define (form-kinds-left g env recur)
    (case (car g)
      ((not) env)
      ((count sum) (cons (list (form-result g) '(int) '()) env))
      (else
       (let* ((inner (guard (e ((refusal? e) env)) (recur (goal-conjuncts (form-sub g)) env)))
              (k (kind-meet (term-kind (form-column g) inner) '(int str))))
         (cons (list (form-result g) (if (null? k) '(int str) k) '()) env)))))
  (define (form-empty? S g b vars)
    (let* ((env (session-env-of S))
           (kinds-of (forms-lenient (lambda (r) (let ((h (dispatch env r))) (and h ((handler-kinds-of h) r))))))
           (seed (filter values (map (lambda (v) (let ((e (assq v b))) (and e (list v (value-kind (cdr e)) '())))) vars))))
      (define (static? a)
        (and (pair? a) (pair? (cdr a)) (pair? (cddr a)) (memq (caddr a) '(wrong-kind join-across-kinds))
             (not (form-own-refusal? a))))
      ;; GOAL BY GOAL: one no cut satisfies is the emptiness; one refused for
      ;; another reason is passed over, and the rest still read
      (let loop ((cs (goal-conjuncts (form-sub g))) (env seed))
        (and (pair? cs)
             (let ((next (guard (e ((refusal? e) (if (static? (refusal-answer e)) 'empty env)))
                           (goal-kinds (list (car cs)) env kinds-of))))
               (or (eq? next 'empty) (loop (cdr cs) next)))))))
  (define (form-solve S g b rest)
    (let* ((sub (form-sub g))
           (exported (apply append (map (lambda (c) ((handler-exports (dispatch (session-env-of S) (car c))) c))
                                        (goal-conjuncts sub))))
           (vars (filter (lambda (v) (or (assq v b) (memq v exported))) (goal-all-variables sub)))
           (name (form-name g (map (lambda (v) (cons v (and (assq v b) #t))) vars))))
      (unless (hashtable-contains? (session-index S) name)
        (hashtable-set! (session-index S) name (list (cons (cons name vars) (goal-conjuncts sub)))))
      (let* ((key (cons name (map (lambda (v) (let ((e (assq v b))) (if e (cdr e) unbound))) vars)))
             (empty (form-empty? S g b vars)))
        (unless empty (complete-key! S key))
        (let ((rows (if empty '() (table-tuples S key))) (head (car g)))
          (define (bind v value) (let ((b2 (unify (list v) (list value) b))) (when b2 (rest b2))))
          (define (column-values)
            (let ((i (let loop ((vs vars) (i 0)) (if (eq? (car vs) (form-column g)) i (loop (cdr vs) (+ i 1))))))
              (map (lambda (t) (list-ref t i)) rows)))
          (define (value-refusal v)
            (refuse 'error 'bad-request 'aggregate-value (list 'form head) (list 'variable (form-column g)) (list 'value v)))
          (case head
            ((not) (when (null? rows) (rest b)))
            ((count) (bind (form-result g) (length rows)))
            ((sum)
             (let ((vs (column-values)))
               (let ((bad (memp (lambda (v) (not (and (integer? v) (exact? v)))) vs)))
                 (when bad (value-refusal (car bad))))
               (bind (form-result g) (apply + vs))))
            (else
             (let ((vs (column-values)))
               (unless (null? vs)
                 (let* ((ints? (and (integer? (car vs)) (exact? (car vs))))
                        (bad (memp (lambda (v) (not (if ints? (and (integer? v) (exact? v)) (string? v)))) vs)))
                   (when (or bad (not (or ints? (string? (car vs))))) (value-refusal (if bad (car bad) (car vs))))
                   (let ((better? (if ints?
                                      (if (eq? head 'min) < >)
                                      (lambda (a b) (let ((x (string->utf8 a)) (y (string->utf8 b)))
                                                      (if (eq? head 'min) (bytes<? x y) (bytes<? y x)))))))
                     (bind (form-result g) (fold-left (lambda (m v) (if (better? v m) v m)) (car vs) (cdr vs)))))))))))))

  ;; ---- one query ------------------------------------------------------------------------------

  ;; The goals of a query, the variables it reports, checked as A3 says, each
  ;; through the handler ENV dispatches it to. REPORT is the route's reporter
  ;; of an unknown relation (query-reporter). BOUND, when given, names
  ;; variables bound before the first goal: a rule's goals are asked with ?w
  ;; bound to the target. TWO PASSES, as before: every goal's relation, arity
  ;; and terms, then the bindings, so an unknown relation is reported before
  ;; a test's unbound variable. A relation the dispatch finds but that is
  ;; not AVAILABLE (a stored relation contested, retired or failing its own
  ;; check, or reaching one) is refused relation-unavailable in the first
  ;; pass, right after the dispatch.
  (define (query-goals goal env report . bound+)
    (query-goals-with goal env report #t (if (pair? bound+) (car bound+) '())))
  ;; AVAIL? #f reads stored relations by their heads only: a stored
  ;; relation's own clauses are checked so (stored-clauses-refusal), a cycle
  ;; of relations member by member. BOUND is the variables bound before the
  ;; first goal.
  (define (query-goals-with goal env report avail? bound)
    ;; AN AND IS A CONJUNCTION WHEREVER IT STANDS: (and a (and b c)) is the
    ;; conjunction a b c, its goals in the order written. An empty and is
    ;; no goal, nested or not, and an and found inside itself (a datum
    ;; label's cycle) is refused rather than walked for ever; one and shared
    ;; by two places, which is no cycle, is read in each.
    ;; SEEN0 holds the ands and forms around GOAL, so a datum label's cycle
    ;; through a form is refused as one through an and is
    (define (conjuncts goal seen0)
      (let walk ((g goal) (seen seen0))
        (unless (and (pair? g) (list? g) (symbol? (car g)))
          (refuse 'error 'bad-request 'not-a-goal (list 'goal g)))
        (cond ((not (eq? (car g) 'and)) (list g))
              ((or (null? (cdr g)) (memq g seen)) (refuse 'error 'bad-request 'not-a-goal (list 'goal g)))
              (else (apply append (map (lambda (c) (walk c (cons g seen))) (cdr g)))))))
    ;; PASS ONE, every goal at every depth: its relation, availability, arity
    ;; and terms; a form's goal is read in its turn, so every malformed goal
    ;; anywhere is reported before any binding is
    (define (pass-one goals seen)
      (for-each
        (lambda (g)
          (let* ((h (or (dispatch env (car g)) (report (car g))))
                 (u (and avail? ((handler-available h) (car g))))
                 (n (begin
                      (when u (apply refuse 'error 'bad-request 'relation-unavailable (list 'relation (car g)) u))
                      ((handler-arity-of h) (car g)))))
            ;; NO ARITY is only a stored callee with no head in force, met
            ;; with AVAIL? #f: its clauses' check reads heads, and the reach
            ;; is what reports it
            (unless (or (not n) (= n (length (cdr g))))
              (refuse 'error 'bad-request 'wrong-arity (list 'relation (car g)) (list 'arity n) (list 'given (length (cdr g)))))
            (let ((bad ((handler-terms h) g)))
              (when bad (refuse 'error 'bad-request 'not-a-term (list 'term (car bad)))))
            (when (form-goal? g)
              (when (memq g seen) (refuse 'error 'bad-request 'not-a-goal (list 'goal g)))
              (pass-one (conjuncts (form-sub g) (cons g seen)) (cons g seen)))))
        goals))
    ;; PASS TWO, the bindings, scope by scope: a goal's check against the
    ;; variables bound before it, and what it exports. A form's goal is a
    ;; NESTED SCOPE: it starts with the outer bindings; a variable unbound
    ;; outside and bound inside is LOCAL to the form, and nothing local is
    ;; visible after it (a not exports nothing, an aggregate its result). A
    ;; local may not be named by a later goal of the same conjunction
    ;; (local-variable-reused), since a positive goal there would bind it
    ;; afresh. An aggregate's result must be unbound before it and absent
    ;; from its goal; its column (sum, min, max) unbound before it and bound
    ;; by its immediate goal. -> the variables bound after GOALS, and the
    ;; locals consumed, ((<variable> . <form>) ...).
    (define (pass-two goals seen consumed)
      (let loop ((gs goals) (seen seen) (consumed consumed))
        (if (null? gs)
            (values seen consumed)
            (let* ((g (car gs)) (h (dispatch env (car g)))
                   (reused (find (lambda (v) (and (assq v consumed) (not (memq v seen)))) (goal-all-variables g))))
              (when reused
                (refuse 'error 'bad-request 'local-variable-reused (list 'variable reused) (list 'form (cdr (assq reused consumed)))))
              (if (form-goal? g)
                  (let ((head (car g)) (sub (form-sub g)))
                    (unless (eq? head 'not)
                      (let ((r (form-result g)))
                        (when (memq r seen) (refuse 'error 'bad-request 'aggregate-result-bound (list 'variable r) (list 'form head)))
                        (when (memq r (goal-all-variables sub))
                          (refuse 'error 'bad-request 'aggregate-result-in-goal (list 'variable r) (list 'form head))))
                      (unless (eq? head 'count)
                        (let ((x (form-column g)))
                          (when (memq x seen) (refuse 'error 'bad-request 'aggregate-column-bound (list 'variable x) (list 'form head))))))
                    (let-values (((inside inner-consumed) (pass-two (conjuncts sub '()) seen consumed)))
                      (let ((locals (filter (lambda (v) (not (memq v seen))) inside)))
                        (when (memq head '(sum min max))
                          (let ((x (form-column g)))
                            (unless (memq x locals)
                              (refuse 'error 'bad-request 'aggregate-column-unbound (list 'variable x) (list 'form head)))))
                        (loop (cdr gs) (append ((handler-exports h) g) seen)
                              (append (map (lambda (v) (cons v head)) locals) inner-consumed)))))
                  (let ((v ((handler-check h) g seen)))
                    (when v (refuse 'error 'bad-request (car v) (list 'variable (cadr v))))
                    (loop (cdr gs) (append ((handler-exports h) g) seen) consumed)))))))
    (let ((goals (conjuncts goal '())))
      (pass-one goals '())
      (pass-two goals bound '())
      goals))

  ;; THE QUERY ROUTES' REPORTER: unknown-relation with the known list in its
  ;; order -- the fact relations, the first definition of each rule of RULES,
  ;; the tests. A session's per-write facts are not listed, as they never
  ;; were; a rule value's rule-only facts are listed among the rules (RULES
  ;; holds their heads).
  (define (query-reporter rules)
    (lambda (rel)
      (refuse 'error 'bad-request 'unknown-relation (list 'relation rel)
              (list 'known (append (map car fact-relations) (map car (rule-relations rules)) (map car tests))))))
  ;; A RULE VALUE'S GOALS: the rule-value environment (with the stored
  ;; relations of STATE when one is given), ?w bound
  (define (rule-value-goals g . state)
    (query-goals g (rule-value-env (and (pair? state) (car state)))
                 (query-reporter (append rule-library rule-only-heads)) '(?w)))

;; ---- the derive verb's checks, and what a declaration affects ---------------------------

  ;; A STORED RELATION AS THE `derive` VERB WRITES IT, at STATE (the batch's
  ;; evolving prefix): the name free of every handler's goal names and of the
  ;; declared edge relations and stored rules (name-taken, with where); the
  ;; value's shape (reduce.sc, derive-value-reason); no clause reaching an
  ;; external relation, through stored relations too; and the relation
  ;; available at STATE with this value in force -- its own clauses checked
  ;; with the kinds, the declared head the containment obligation, and every
  ;; stored relation it reaches available. -> the value, or a refusal.
  (define (derive-value-check state name value)
    (let ((where (cond ((memq name form-heads) 'form)
                       ((assq name tests) 'test)
                       ((assq name fact-relations) 'fact)
                       ((assq name rule-only-facts) 'write-fact)
                       ((assq name (rule-relations rule-library)) 'library)
                       ((and state (reduction? state) (state-declaration state name)) 'edge-relation)
                       ((and state (reduction? state) (state-rule state name)) 'rule)
                       (else #f))))
      (cond
        (where (list 'error 'bad-request 'name-taken (list 'name name) (list 'where where)))
        ;; AND IS THE QUERY LANGUAGE'S CONJUNCTION, never a relation's name,
        ;; as a library head may not be
        ((eq? name 'and) (list 'error 'bad-request 'name-taken (list 'name name) '(where conjunction)))
        ((eq? value 'retired) value)
        ((derive-value-reason name value) => (lambda (r) (list 'error 'bad-request 'derive-malformed (list 'reason r))))
        ;; THROUGH THE FORMS TOO: every relation a clause names (goal-atoms)
        ((exists (lambda (c) (find (lambda (r) (relation-external? r state (list name value)))
                                   (apply append (map goal-atom-names (cddr c)))))
                 (cdr value))
         => (lambda (r) (list 'error 'bad-request 'rule-relation-not-allowed (list 'relation r))))
        (else
         (let ((u ((handler-available (stored-handler state (list name value))) name)))
           (cond ((not u) value)
                 ;; its own clauses' refusal, as it is
                 ((and (null? (cdr u)) (pair? (cadr (car u))) (eq? (car (cadr (car u))) 'check))
                  (cadr (cadr (car u))))
                 ;; ITSELF ON A CYCLE THROUGH A NOT OR AN AGGREGATE at this state
                 ((and (null? (cdr u)) (pair? (cadr (car u))) (eq? (car (cadr (car u))) 'unstratified))
                  (list 'error 'bad-request 'unstratified (list 'relation name) (cadr (cadr (car u)))))
                 (else (apply list 'error 'bad-request 'relation-unavailable (list 'relation name) u))))))))

  ;; THE RELATIONS A RULE'S GOALS NAME BY POSITION, through every nested and
  ;; and every form -- never a symbol in a constant -- that are stored relations of STATE
  ;; (masked names aside): whether the rule must be re-checked with the kinds
  ;; when it is evaluated there (rules.sc, rule-plan).
  (define (rule-names-stored? value state)
    (define (heads g) (goal-atom-names g))
    (let ((index (stored-entry-index (stored-entry-for state #f))))
      (and (> (hashtable-size index) 0) (list? value)
           (exists (lambda (k)
                     (let ((c (assq k value)))
                       (and c (list? c) (pair? (cdr c))
                            (exists (lambda (r) (hashtable-contains? index r)) (heads (cadr c))))))
                   '(where must must-not)))))

;; A STATE'S ENTRY with NAMES in its closure, one computation for them all
  (define (stored-entry-primed state names)
    (let ((e (stored-entry-for state #f))) (entry-prime! e names) e))
  ;; whether REL is unavailable in an entry: a distance, and no chain built
  (define (entry-unavailable? e rel)
    (entry-prime! e (list rel))
    (hashtable-contains? (stored-entry-distance e) rel))
  ;; WHAT A DECLARATION AFFECTS: the rules in force and the stored relations
  ;; in force at PRE whose evaluability or availability is not the same at
  ;; POST, the state after the write. -> (affects [(rule <name> ...)]
  ;; [(derive <name> ...)]), or #f when there is none. Informational: the
  ;; next write a broken rule judges is the one refused.
  ;; WHAT A WRITE CHANGES: the settled rules and the in-force stored relations
  ;; (NAME aside) whose AVAILABILITY differs between PRE and POST, available
  ;; to unavailable or back; one that stays unavailable with another chain or
  ;; sink is no effect. Each state's table is computed once, for every name.
  (define (derive-affects pre post name)
    (define (rule-unevaluable? value state)
      (let ((v (rule-value-check value (rule-names-stored? value state) state)))
        (and (pair? v) (eq? (car v) 'error))))
    ;; EVERY DECLARED NAME of each state primed at once, the rules' names
    ;; among them: one computation a state, whatever the rules ask after
    (let* ((pre-entry (stored-entry-for pre #f))
           (post-entry (stored-entry-for post #f))
           (names (filter (lambda (n) (and (not (eq? n name)) (entry-in-force pre-entry n))) (stored-entry-order pre-entry)))
           (pre-entry (stored-entry-primed pre (stored-entry-order pre-entry)))
           (post-entry (stored-entry-primed post (stored-entry-order post-entry)))
           (rules (filter (lambda (r) (not (eq? (rule-unevaluable? (cadr r) pre) (rule-unevaluable? (cadr r) post))))
                          (state-declared-rules pre)))
           (derived (filter (lambda (n) (not (eq? (entry-unavailable? pre-entry n) (entry-unavailable? post-entry n))))
                            names)))
      (and (or (pair? rules) (pair? derived))
           (append (list 'affects)
                   (if (pair? rules) (list (cons 'rule (map car rules))) '())
                   (if (pair? derived) (list (cons 'derive derived)) '())))))

  ;; THE FOURTH TABLE, query --relations: every derive record's name at
  ;; STATE. In force: (derived <name> (arity <n>) (kinds (<kind> ...))
  ;; <availability>); retired: (derived <name> (retired) <availability>);
  ;; contested: (derived <name> (contested) (candidates (<head> <writer>
  ;; <seq>) ...) <availability>); a name the namespace reserves, which only
  ;; a record that bypassed the verb carries: (derived <name> (unavailable
  ;; (shadowed <owner>))). The availability is `available` or (unavailable
  ;; (state ...) [(through ...)]).
  (define (stored-relations-items state)
    (let ((h (stored-handler state #f)))
      ;; every name asked at once: one computation
      (stored-entry-primed state (if (and state (reduction? state)) (state-derived-names state) '()))
      (map (lambda (n+d)
             (let ((n (car n+d)) (owner (goal-name-owner (car n+d))) (d (cdr n+d)))
               (if owner
                   (list 'derived n (list 'unavailable (list 'shadowed owner)))
                   (let ((avail (let ((u ((handler-available h) n))) (if u (cons 'unavailable u) 'available))))
                     (case (car d)
                       ((retired) (list 'derived n '(retired) avail))
                       ((contested)
                        (list 'derived n '(contested)
                              (cons 'candidates (map (lambda (c) (cons (if (pair? (car c)) (stored-head (car c)) (car c)) (cdr c)))
                                                     (cdr d)))
                              avail))
                       (else (let ((hd (stored-head (cadr d))))
                               (list 'derived n (list 'arity (length (cdr hd))) (list 'kinds (cdr hd)) avail))))))))
           (if (and state (reduction? state)) (state-derived-all state) '()))))

  (define query-counter 0)

  ;; THE SESSION'S STATIC PASS: the goals, checked in the session's
  ;; environment. Raises a refusal.
  (define (session-check S goal)
    (query-goals goal (session-env-of S) (query-reporter (session-rules S))))
  ;; -> (values <rows> <vars>), the rows as lists of values. Raises a refusal.
  (define (session-query S goal)
    (session-evaluate S (session-check S goal)))
  ;; THE EVALUATION OF GOALS the static pass has accepted.
  (define (session-evaluate S goals)
    ;; THE REPORTED VARIABLES are what each goal exports: a form's locals are
    ;; not among them
    (let* ((vars (let loop ((gs goals) (out '()))
                   (if (null? gs) (reverse out)
                       (loop (cdr gs) (fold-left (lambda (acc v) (if (memq v acc) acc (cons v acc))) out
                                                 (let ((h (dispatch (session-env-of S) (car (car gs)))))
                                                   (if h ((handler-exports h) (car gs)) (goal-variables (car gs)))))))))
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

  ;; -> the rows of GOAL as an answer would hold them -- sorted, unique, and
  ;; each one renderable, a row that is not raising the query's refusal
  ;; (unrenderable) -- for a caller that counts or lists rows itself.
  (define (session-rows S goal)
    (let-values (((rows vars) (session-query S goal)))
      (let-values (((sorted digest) (canonical rows vars (session-check S goal))))
        sorted)))

  ;; -> (ok <rows sorted> <vars> <digest>), or the refusal's answer.
  (define (session-answer S goal)
    (guard (e ((refusal? e) (refusal-answer e)))
      (let-values (((rows vars) (session-query S goal)))
        ;; THE RECHECK IN THE SESSION'S ENVIRONMENT, per-write facts included
        ;; (it once lacked them, and refused a query it had just evaluated)
        (let-values (((sorted digest) (canonical rows vars (session-check S goal))))
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
    ;; a fact's line carries its column kinds: (ID, SYM), a symbol set as
    ;; (SYM <symbol> ...)
    (define (kind-text k)
      (if (symbol? k)
          (symbol->string k)
          (string-append "(" (fold-left (lambda (acc x) (string-append acc (if (string=? acc "") "" " ") (symbol->string x))) "" k) ")")))
    (define (fact-line f)
      (string-append "- `" (symbol->string (car f)) "/" (number->string (cadr f)) "` ("
                     (fold-left (lambda (acc k) (string-append acc (if (string=? acc "") "" ", ") (kind-text k))) "" (list-ref f 4))
                     ") -- " (caddr f) "\n"))
    (string-append
      "Fact relations:\n\n"
      (apply string-append (map fact-line fact-relations))
      "\nTests, on bound values only: "
      (let loop ((ts tests) (acc ""))
        (if (null? ts) acc
            (loop (cdr ts) (string-append acc (if (string=? acc "") "" ", ")
                                          "`" (symbol->string (car (car ts))) "/" (number->string (cadr (car ts))) "`"))))
      ".\n\nForms, each over a goal of the language: "
      (let loop ((fs form-heads) (acc ""))
        (if (null? fs) acc
            (loop (cdr fs) (string-append acc (if (string=? acc "") "" ", ")
                                          "`" (symbol->string (car fs)) "/" (number->string (form-arity (car fs))) "`"))))
      ".\n\nRules of the library:\n\n"
      (apply string-append (map (lambda (r) (line (car r) (cadr r) (cadr (assq (car r) rule-lines))))
                                (rule-relations rule-library)))))

  ;; ---- the verb ---------------------------------------------------------------------------------

  (define (query-verb dispatch-helper store actor args req options state writer cwd)
    (cond
      ;; A THIRD TABLE AFTER THE TWO: the store's declared relations,
      ;; (declared <name> <kind> (from <selector>) (to <selector>)), or
      ;; (declared <name> (contested)); none in a store with no declaration.
      ((argument-option options "--relations")
       (if (null? args)
           ((dispatch-helper 'guarded)
            (lambda ()
              ((dispatch-helper 'items)
               (let ((view ((dispatch-helper 'reduction-for) store state)))
                 (append (query-relations-items)
                         (map (lambda (d) (cons 'declared d)) (state-declared-relations view))
                         (stored-relations-items view))))))
           ((dispatch-helper 'usage) query-usage)))
      ((not (= 1 (length args))) ((dispatch-helper 'usage) query-usage))
      (else
       ((dispatch-helper 'guarded)
        (lambda ()
          (let ((view ((dispatch-helper 'reduction-for) store state)))
            (let ((goal (guard (e (#t 'unreadable)) (string->sexpr-extended (car args)))))
              (if (eq? goal 'unreadable)
                  (list 'error 'bad-request 'not-a-goal (list 'goal (car args)))
                  ;; THE KINDS ARE CHECKED AFTER ARITY AND BINDING, BEFORE ANY
                  ;; EVALUATION: a goal query-goals refuses is answered by the
                  ;; session as before; one it accepts is read against the kinds.
                  ;; THE SESSION FIRST, its environment the kinds': a call to a
                  ;; stored relation is read against the relation's head
                  (let* ((S (make-query-session view '() query-budget-default ((dispatch-helper 'keyword-hook) store)))
                         (kinds (query-kind-refusal goal (session-env-of S)))
                         (a (or kinds (session-answer S goal))))
                    (if (not (eq? (car a) 'ok))
                        a
                        (append ((dispatch-helper 'items) (map (lambda (r) (cons 'row r)) (cadr a)))
                                (list (list 'vars (caddr a)) (list 'digest (cadddr a)) (list 'cut (reduce-applied-cut view)))
                                (if (session-consulted S) '((name-use syntactic)) '()))))))))))))
)
