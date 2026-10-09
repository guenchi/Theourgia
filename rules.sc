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

;;; (theourgia rules) -- a write judged against the store's typed
;;; declarations and rules, before anything of it is written.
;;;
;;; THE INPUTS are the state before the write, the state the write would
;;; produce (a rehearsal's copy of the reduction after the store's own
;;; writing code ran against it), the write's targets and its receipt. The
;;; store makes the rehearsal and asks this library; this library writes
;;; nothing and answers #f or the refusal.
;;;
;;; THE ORDER is the store's: the premises and the verb's own check have
;;; already passed; typed endpoints are judged next, then the rules.
;;;
;;; ONE BUDGET FOR THE WRITE. Every query asked here -- the rules' goals and
;;; selectors, and the per-write facts read for them -- is charged to one
;;; count, query-budget-default for the whole write; each query is handed
;;; what is left, and the first that runs out refuses the write as
;;; unevaluable, naming the rule and the block.

(library (theourgia rules)
  (export write-judgement state-audit)
  (import (rnrs)
          (only (theourgia query) make-query-session session-rows session-spent session-budget-set!
                refusal? refusal-answer query-budget-default)
          (only (theourgia reduce) state-declared-rules state-declared-relations known-kinds relation-kind
                state-edges))

  ;; At most this many witness rows are listed for a failing must-not; the
  ;; count beside them is exact.
  (define witness-limit 10)

  ;; ---- reading a state through a query session -------------------------------------------------

  ;; The goal with ?w replaced by the target's id.
  (define (bind-target x id)
    (cond ((eq? x '?w) id)
          ((pair? x) (cons (bind-target (car x) id) (bind-target (cdr x) id)))
          (else x)))

  (define (unique xs)
    (let loop ((xs xs) (out '()))
      (cond ((null? xs) (reverse out))
            ((member (car xs) out) (loop (cdr xs) out))
            (else (loop (cdr xs) (cons (car xs) out))))))

  (define (as-symbol r) (if (string? r) (string->symbol r) r))

  ;; EVERY EDGE THE STATE HOLDS, as (from relation to), an end at a deleted
  ;; block included: a link to a retired block is a write the store accepts,
  ;; and its typing and its citing are judged like any other. (The query
  ;; facts' `edge` holds only edges between live blocks.)
  (define (held-edges r) (map (lambda (e) (list (car e) (cadr e) (caddr e))) (state-edges r)))
  (define (held-edges-at r id)
    (filter (lambda (e) (or (equal? (car e) id) (equal? (caddr e) id))) (held-edges r)))

  ;; IN ORDER, because every query is charged to one budget and the one that
  ;; exhausts it is named: R6RS's map leaves the order of its calls open.
  (define (map-in-order f xs)
    (let loop ((xs xs) (out '()))
      (if (null? xs) (reverse out) (loop (cdr xs) (cons (f (car xs)) out)))))
  (define (append-map-in-order f xs) (apply append (map-in-order f xs)))
  (define (filter-map-in-order f xs) (filter values (map-in-order f xs)))

  ;; ---- the judgement ----------------------------------------------------------------------------
  ;;
  ;; ASK answers a goal's rows as an answer would hold them (session-rows):
  ;; sorted, unique, each renderable -- a row that cannot be rendered is the
  ;; query's refusal, as it is to a reader.

  ;; PRE and POST are reductions, TARGETS the write's target ids (live in
  ;; POST, sorted), RECEIPT the block ids of the write's premise set or #f
  ;; when it carried none. -> #f, or the refusal.
  (define (write-judgement pre post targets receipt)
    (let ((remaining query-budget-default)
          (post-session #f)
          (pre-session #f))
      (define (post-S)
        (or post-session (begin (set! post-session (make-query-session post '())) post-session)))
      (define (pre-S)
        (or pre-session (begin (set! pre-session (make-query-session pre '())) pre-session)))
      ;; THE ROWS OF ONE GOAL, charged to the write's budget: each row the
      ;; values of the goal's variables, in the order they first appear.
      (define (ask S goal)
        (session-budget-set! S remaining)
        ;; WHAT A QUERY SPENT IS SPENT, whether it answered or was refused:
        ;; a query the budget stopped has spent what was left.
        (guard (e ((refusal? e)
                   (set! remaining (max 0 (- remaining (session-spent S))))
                   (raise e)))
          (let ((rows (session-rows S goal)))
            (set! remaining (- remaining (session-spent S)))
            rows)))
      (define (kind-of S id)
        (let ((rows (ask S (list 'kind id '?k))))
          (if (pair? rows) (car (car rows)) 'none)))
      (define (field-values S id name)
        (map car (ask S (list 'field id (symbol->string name) '?v))))
      ;; Every edge with ID at either end, as (from relation to).
      (define (edges-at S id rel)
        (let* ((out (ask S (list rel id '?r '?b)))
               (in (ask S (list rel '?a '?r id))))
          (unique (append (map (lambda (row) (list id (car row) (cadr row))) out)
                          (map (lambda (row) (list (car row) (cadr row) id)) in)))))
      (or (guard (e ((refusal? e)
                     (list 'error 'refused 'relation-endpoint-unevaluable (list 'reason (refusal-answer e)))))
            (endpoint-refusal pre post targets post-S pre-S kind-of field-values edges-at))
          (rule-refusal pre post targets receipt post-S pre-S ask kind-of edges-at
                        (lambda () remaining)))))

  ;; ---- typed endpoints --------------------------------------------------------------------------

  ;; A declared relation's ends are selectors: () for any block, else
  ;; ((kind <k>)) or ((kind <k>) (field <name> <value>)).
  (define (selector-holds? sel S id kind-of field-values)
    (or (null? sel)
        (and (eq? (kind-of S id) (cadr (car sel)))
             (or (null? (cdr sel))
                 (let ((f (cadr sel)))
                   (and (member (caddr f) (field-values S id (cadr f))) #t))))))

  ;; THE EDGES JUDGED are every typed edge the write adds -- the state after
  ;; holds it, the state before does not, whether or not its ends are live
  ;; -- and every typed edge at a target whose kind or a field some typed
  ;; relation's selector names the write changes. Each end of each is held to
  ;; its selector on the state the write produces; every mismatch is listed
  ;; in one answer.
  (define (endpoint-refusal pre post targets post-S pre-S kind-of field-values edges-at)
    (let ((typed (typed-relations pre)))
      (and (pair? typed)
           (let* ((selector-fields
                    (unique (apply append
                                   (map (lambda (d)
                                          (map cadr (filter (lambda (c) (eq? (car c) 'field))
                                                            (append (cadr (caddr d)) (cadr (cadddr d))))))
                                        typed))))
                  (typed-of (lambda (r) (assq (as-symbol r) typed)))
                  (changed?
                    (lambda (id)
                      (or (let* ((before (kind-of (pre-S) id)) (after (kind-of (post-S) id)))
                            (not (eq? before after)))
                          (exists (lambda (f)
                                    (let* ((before (field-values (pre-S) id f)) (after (field-values (post-S) id f)))
                                      (not (equal? before after))))
                                  selector-fields))))
                  (added
                    (let ((before (make-hashtable equal-hash equal?)))
                      (for-each (lambda (e) (hashtable-set! before e #t)) (held-edges pre))
                      (filter (lambda (e) (and (typed-of (cadr e)) (not (hashtable-ref before e #f))))
                              (held-edges post))))
                  (judged
                    (unique
                      (append added
                              (append-map-in-order
                                (lambda (id)
                                  (if (changed? id)
                                      (filter (lambda (e) (typed-of (cadr e))) (held-edges-at post id))
                                      '()))
                                targets))))
                  (failures (endpoint-failures typed (list-sort (lambda (a b) (string<? (edge-key a) (edge-key b))) judged)
                                               (post-S) kind-of field-values)))
             (and (pair? failures)
                  (list 'error 'bad-request 'relation-endpoint (cons 'failures failures)))))))

  ;; -> ((<relation> <edge> <end> <expected>) ...) for each end of each EDGE
  ;; whose block its typed relation's selector does not hold, on S's state.
  (define (endpoint-failures typed edges S kind-of field-values)
    (append-map-in-order
      (lambda (e)
        (let* ((d (assq (as-symbol (cadr e)) typed))
               (from (cadr (caddr d))) (to (cadr (cadddr d)))
               (from-holds? (selector-holds? from S (car e) kind-of field-values))
               (to-holds? (selector-holds? to S (caddr e) kind-of field-values)))
          (append
            (if from-holds? '() (list (list (list 'relation (car d)) (cons 'edge e) '(end from) (list 'expected from))))
            (if to-holds? '() (list (list (list 'relation (car d)) (cons 'edge e) '(end to) (list 'expected to)))))))
      edges))

  (define (typed-relations state)
    (filter (lambda (d)
              (and (= (length d) 4)
                   (or (pair? (cadr (caddr d))) (pair? (cadr (cadddr d))))))
            (state-declared-relations state)))

  (define (edge-key e)
    (string-append (car e) " " (let ((r (cadr e))) (if (symbol? r) (symbol->string r) r)) " " (caddr e)))

  ;; ---- the rules --------------------------------------------------------------------------------

  ;; A RULE AS IT IS EVALUATED: (name class kinds where goal polarity witness),
  ;; a built-in expanded to the goal it stands for.
  (define (rule-plan name value)
    (define (clause k) (let ((c (assq k value))) (and c (cadr c))))
    (cond
      ((assq 'builtin value)
       ;; CITATION COVERAGE: a write that carried a receipt and cites, from a
       ;; target, a block its receipt does not hold.
       (list name 'write known-kinds #f
             '(and (receipt-carried) (cited ?w ?s) (unread ?s))
             'must-not
             (lambda (id row) (list 'citation-not-read (list 'block id) (list 'cites (car row))))))
      (else
       (list name (clause 'class) (cdr (assq 'on value)) (clause 'where)
             (or (clause 'must) (clause 'must-not))
             (if (assq 'must value) 'must 'must-not)
             #f))))

;; ONE PAIR OF A RULE AND A BLOCK, on session S, the block's kind K: #f when
  ;; the rule does not apply (a kind it does not list, a selector with no row
  ;; for the block) or holds; else the failure, (rule) (block) (goal, ?w
  ;; bound) (expected some|none) (rows n) and, for a must-not, at most ten
  ;; witness rows. A query's refusal is raised to the caller.
  (define (pair-failure ask S p id k)
    (and (memq k (caddr p))
         (or (not (cadddr p)) (pair? (ask S (bind-target (cadddr p) id))))
         (let* ((goal (bind-target (list-ref p 4) id))
                (rows (ask S goal)))
           (case (list-ref p 5)
             ((must)
              (and (null? rows)
                   (list (list 'rule (car p)) (list 'block id) (list 'goal goal) '(expected some) '(rows 0))))
             (else
              (and (pair? rows)
                   (append (list (list 'rule (car p)) (list 'block id) (list 'goal goal)
                                 '(expected none) (list 'rows (length rows)))
                           (list (cons 'witness
                                       (map (lambda (row) (if (list-ref p 6) ((list-ref p 6) id row) row))
                                            (let take ((rs rows) (n 0))
                                              (if (or (null? rs) (= n witness-limit)) '()
                                                  (cons (car rs) (take (cdr rs) (+ n 1)))))))))))))))

  (define (rule-refusal pre post targets receipt post-S pre-S ask kind-of edges-at remaining)
    (let* ((rules (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                             (state-declared-rules pre)))
           (plans (map (lambda (r) (rule-plan (car r) (cadr r))) rules))
           (state-rules (filter (lambda (p) (eq? (cadr p) 'state)) plans))
           (write-rules (filter (lambda (p) (eq? (cadr p) 'write)) plans)))
      (and (pair? plans) (pair? targets)
           (call/cc
             (lambda (return)
               ;; A TARGET'S KIND AFTER THE WRITE, read once; a refusal reading it
               ;; names the first rule and the block.
               (define kinds (make-hashtable equal-hash equal?))
               (define (judge-kind id)
                 (or (hashtable-ref kinds id #f)
                     (let ((k (guard (e ((refusal? e)
                                         (return (list 'error 'refused 'rule-unevaluable (list 'rule (car (car plans)))
                                                       (list 'block id) (list 'reason (refusal-answer e))))))
                                (kind-of (post-S) id))))
                       (hashtable-set! kinds id k)
                       k)))
               (define (judge S p id)
                 (let ((k (judge-kind id)))
                   (guard (e ((refusal? e)
                              (return (list 'error 'refused 'rule-unevaluable (list 'rule (car p)) (list 'block id)
                                            (list 'reason (refusal-answer e))))))
                     (pair-failure ask S p id k))))
               (let* ((state-failures
                        (append-map-in-order
                          (lambda (id) (filter-map-in-order (lambda (p) (judge (post-S) p id)) state-rules))
                          targets))
                      ;; THE WRITE RULES' TARGETS: those whose kind after the write
                      ;; some write rule lists, the first pair of such a target and
                      ;; rule named if reading their facts fails. No such target, no
                      ;; write session.
                      (write-pairs
                        (append-map-in-order
                          (lambda (id)
                            (let ((k (judge-kind id)))
                              (map (lambda (p) (cons id p)) (filter (lambda (p) (memq k (caddr p))) write-rules))))
                          targets))
                      (write-targets (unique (map car write-pairs)))
                      (write-session
                        (and (pair? write-pairs)
                             (guard (e ((refusal? e)
                                        (return (list 'error 'refused 'rule-unevaluable
                                                      (list 'rule (car (cdr (car write-pairs)))) (list 'block (car (car write-pairs)))
                                                      (list 'reason (refusal-answer e))))))
                               (make-query-session pre '() (remaining) #f
                                                   (write-facts pre post write-targets receipt post-S pre-S ask edges-at)))))
                      (write-failures
                        (if write-session
                            (append-map-in-order
                              (lambda (id) (filter-map-in-order (lambda (p) (judge write-session p id)) write-rules))
                              write-targets)
                            '()))
                      (failures
                        (list-sort (lambda (a b)
                                     (let ((ba (cadr (cadr a))) (bb (cadr (cadr b))))
                                       (or (string<? ba bb)
                                           (and (string=? ba bb)
                                                (string<? (symbol->string (cadr (car a))) (symbol->string (cadr (car b))))))))
                                   (append state-failures write-failures))))
                 (and (pair? failures)
                      (list 'error 'refused 'rule-violation (cons 'failures failures)))))))))

  ;; THE PER-WRITE FACTS, read for the targets: kind+, field+, edge+ and
  ;; edge-kind+ from the state the write produces; cited, each edge from a
  ;; target under a relation of kind depends-on that the state after holds
  ;; and the state before does not -- edges compared as edges, by name, before
  ;; any is read as its kind, so a second name for an existing dependence
  ;; cites; unread, a cited block the receipt does not hold; receipt-carried,
  ;; when the write carried one. -> ((<relation> <arity> <tuple> ...) ...).
  (define (write-facts pre post targets receipt post-S pre-S ask edges-at)
    (let* ((kinds (append-map-in-order (lambda (id) (map (lambda (row) (cons id row)) (ask (post-S) (list 'kind id '?k))))
                                       targets))
           (fields (append-map-in-order (lambda (id) (map (lambda (row) (cons id row)) (ask (post-S) (list 'field id '?f '?v))))
                                        targets))
           (edges (unique (append-map-in-order (lambda (id) (edges-at (post-S) id 'edge)) targets)))
           (edge-kinds (unique (append-map-in-order (lambda (id) (edges-at (post-S) id 'edge-kind)) targets)))
           (cited (unique
                    (append-map-in-order
                                (lambda (id)
                                  (let* ((now (filter (lambda (e) (equal? (car e) id)) (held-edges post)))
                                         (before (filter (lambda (e) (equal? (car e) id)) (held-edges pre))))
                                    (map (lambda (e) (list id (caddr e)))
                                         (filter (lambda (e) (and (not (member e before))
                                                                  (eq? (relation-kind post (as-symbol (cadr e))) 'depends-on)))
                                                 now))))
                                targets)))
           (unread (unique (map (lambda (c) (list (cadr c)))
                                (filter (lambda (c) (not (and receipt (member (cadr c) receipt)))) cited)))))
      (list (cons* 'kind+ 2 kinds)
            (cons* 'field+ 3 fields)
            (cons* 'edge+ 3 edges)
            (cons* 'edge-kind+ 3 edge-kinds)
            (cons* 'cited 2 cited)
            (cons* 'unread 1 unread)
            (cons* 'receipt-carried 0 (if receipt (list '()) '())))))
;; ---- check's view ------------------------------------------------------------------------------

  ;; THE CURRENT STATE AUDITED, no write in hand: every state rule over every
  ;; live block of a kind it lists, under one budget for the audit; each write
  ;; rule, a built-in among them, listed once as skipped; every typed edge
  ;; whose end its selector does not hold. An evaluation that fails is listed
  ;; as unevaluable for its rule, never as a violation, and the rule's other
  ;; blocks are not asked. -> (values <rule rows> <endpoint rows>):
  ;;   (rule-violation <failure clauses>) (rule-unevaluable (rule) (block)
  ;;   (reason)) (rule-skipped (rule) (reason write-rule)) and
  ;;   (relation-endpoint (relation) (edge) (end) (expected)).
  (define (state-audit state)
    (let* ((remaining query-budget-default)
           (S (make-query-session state '())))
      (define (ask S goal)
        (session-budget-set! S remaining)
        ;; WHAT A QUERY SPENT IS SPENT, whether it answered or was refused:
        ;; a query the budget stopped has spent what was left.
        (guard (e ((refusal? e)
                   (set! remaining (max 0 (- remaining (session-spent S))))
                   (raise e)))
          (let ((rows (session-rows S goal)))
            (set! remaining (- remaining (session-spent S)))
            rows)))
      (define (kind-of S id)
        (let ((rows (ask S (list 'kind id '?k))))
          (if (pair? rows) (car (car rows)) 'none)))
      (define (field-values S id name)
        (map car (ask S (list 'field id (symbol->string name) '?v))))
      (let* ((rules (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                               (state-declared-rules state)))
             (kinds (guard (e ((refusal? e) #f)) (ask S '(kind ?b ?k))))
             (rule-rows
               (append-map-in-order
                 (lambda (r)
                   (let ((p (rule-plan (car r) (cadr r))))
                     (cond
                       ((eq? (cadr p) 'write)
                        (list (list 'rule-skipped (list 'rule (car p)) '(reason write-rule))))
                       ((not kinds)
                        (list (list 'rule-unevaluable (list 'rule (car p)) '(reason query-budget))))
                       (else
                        (let loop ((ids (list-sort string<? (map car (filter (lambda (row) (memq (cadr row) (caddr p))) kinds))))
                                   (out '()))
                          (if (null? ids)
                              (reverse out)
                              (let ((f (guard (e ((refusal? e)
                                                  (list 'unevaluable
                                                        (list 'rule-unevaluable (list 'rule (car p)) (list 'block (car ids))
                                                              (list 'reason (refusal-answer e))))))
                                         (pair-failure ask S p (car ids)
                                                       (cadr (assoc (car ids) kinds))))))
                                (cond ((not f) (loop (cdr ids) out))
                                      ((eq? (car f) 'unevaluable) (reverse (cons (cadr f) out)))
                                      (else (loop (cdr ids) (cons (cons 'rule-violation f) out)))))))))))
                 rules))
             (typed (typed-relations state))
             (endpoint-rows
               (if (null? typed)
                   '()
                   (guard (e ((refusal? e) (list (list 'relation-endpoint-unevaluable (list 'reason (refusal-answer e))))))
                     (map (lambda (f) (cons 'relation-endpoint f))
                          (endpoint-failures
                            typed
                            (list-sort (lambda (a b) (string<? (edge-key a) (edge-key b)))
                                       (filter (lambda (e) (assq (as-symbol (cadr e)) typed))
                                               (unique (held-edges state))))
                            S kind-of field-values))))))
        (values rule-rows endpoint-rows))))
)
