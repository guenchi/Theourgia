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
  (export write-judgement)
  (import (rnrs)
          (only (theourgia query) make-query-session session-query session-spent session-budget-set!
                refusal? refusal-answer query-budget-default)
          (only (theourgia reduce) state-declared-rules state-declared-relations known-kinds))

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

  ;; ---- the judgement ----------------------------------------------------------------------------

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
        (let-values (((rows vars) (session-query S goal)))
          (set! remaining (- remaining (session-spent S)))
          rows))
      (define (kind-of S id)
        (let ((rows (ask S (list 'kind id '?k))))
          (if (pair? rows) (car (car rows)) 'none)))
      (define (field-values S id name)
        (map car (ask S (list 'field id (symbol->string name) '?v))))
      ;; Every edge with ID at either end, as (from relation to).
      (define (edges-at S id rel)
        (unique (append (map (lambda (row) (list id (car row) (cadr row))) (ask S (list rel id '?r '?b)))
                        (map (lambda (row) (list (car row) (cadr row) id)) (ask S (list rel '?a '?r id))))))
      (or (endpoint-refusal pre post targets post-S pre-S kind-of field-values edges-at)
          (rule-refusal pre targets receipt post-S pre-S ask kind-of edges-at
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

  ;; THE EDGES JUDGED are the ones the write adds, and every typed edge at a
  ;; target whose kind or a field some typed relation's selector names the
  ;; write changes. Each end of each is held to its selector on the state
  ;; the write produces; every mismatch is listed in one answer.
  (define (endpoint-refusal pre post targets post-S pre-S kind-of field-values edges-at)
    (let ((typed (filter (lambda (d)
                           (and (= (length d) 4)
                                (or (pair? (cadr (caddr d))) (pair? (cadr (cadddr d))))))
                         (state-declared-relations pre))))
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
                      (or (not (eq? (kind-of (pre-S) id) (kind-of (post-S) id)))
                          (exists (lambda (f) (not (equal? (field-values (pre-S) id f) (field-values (post-S) id f))))
                                  selector-fields))))
                  (judged
                    (unique
                      (apply append
                             (map (lambda (id)
                                    (let ((now (edges-at (post-S) id 'edge))
                                          (before (edges-at (pre-S) id 'edge))
                                          (all? (changed? id)))
                                      (filter (lambda (e) (and (typed-of (cadr e)) (or all? (not (member e before)))))
                                              now)))
                                  targets))))
                  (failures
                    (apply append
                           (map (lambda (e)
                                  (let* ((d (typed-of (cadr e)))
                                         (from (cadr (caddr d))) (to (cadr (cadddr d))))
                                    (append
                                      (if (selector-holds? from (post-S) (car e) kind-of field-values) '()
                                          (list (list (list 'relation (car d)) (cons 'edge e) '(end from) (list 'expected from))))
                                      (if (selector-holds? to (post-S) (caddr e) kind-of field-values) '()
                                          (list (list (list 'relation (car d)) (cons 'edge e) '(end to) (list 'expected to)))))))
                                (list-sort (lambda (a b) (string<? (edge-key a) (edge-key b))) judged)))))
             (and (pair? failures)
                  (list 'error 'bad-request 'relation-endpoint (cons 'failures failures)))))))

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

  (define (rule-refusal pre targets receipt post-S pre-S ask kind-of edges-at remaining)
    (let* ((rules (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                             (state-declared-rules pre)))
           (plans (map (lambda (r) (rule-plan (car r) (cadr r))) rules))
           (state-rules (filter (lambda (p) (eq? (cadr p) 'state)) plans))
           (write-rules (filter (lambda (p) (eq? (cadr p) 'write)) plans)))
      (and (pair? plans) (pair? targets)
           (call/cc
             (lambda (return)
               (define (judge S p id)
                 (guard (e ((refusal? e)
                            (return (list 'error 'refused 'rule-unevaluable (list 'rule (car p)) (list 'block id)
                                          (list 'reason (refusal-answer e))))))
                   (and (memq (kind-of (post-S) id) (caddr p))
                        (or (not (cadddr p)) (pair? (ask S (bind-target (cadddr p) id))))
                        (let* ((goal (bind-target (list-ref p 4) id))
                               (rows (ask S goal)))
                          (case (list-ref p 5)
                            ((must)
                             (and (null? rows)
                                  (list (list 'rule (car p)) (list 'block id) (list 'goal goal)
                                        '(expected some) '(rows 0))))
                            (else
                             (and (pair? rows)
                                  (append (list (list 'rule (car p)) (list 'block id) (list 'goal goal)
                                                '(expected none) (list 'rows (length rows)))
                                          (list (cons 'witness
                                                      (map (lambda (row) (if (list-ref p 6) ((list-ref p 6) id row) row))
                                                           (let take ((rs rows) (n 0))
                                                             (if (or (null? rs) (= n witness-limit)) '()
                                                                 (cons (car rs) (take (cdr rs) (+ n 1))))))))))))))))
               (let* ((state-failures
                        (apply append
                               (map (lambda (id)
                                      (filter values (map (lambda (p) (judge (post-S) p id)) state-rules)))
                                    targets)))
                      (write-session
                        (and (pair? write-rules)
                             (guard (e ((refusal? e)
                                        (return (list 'error 'refused 'rule-unevaluable
                                                      (list 'rule (car (car write-rules))) (list 'block (car targets))
                                                      (list 'reason (refusal-answer e))))))
                               (make-query-session pre '() (remaining) #f
                                                   (write-facts targets receipt post-S pre-S ask edges-at)))))
                      (write-failures
                        (if write-session
                            (apply append
                                   (map (lambda (id)
                                          (filter values (map (lambda (p) (judge write-session p id)) write-rules)))
                                        targets))
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
  ;; edge-kind+ from the state the write produces; cited, the depends-on
  ;; edges from a target that the write adds; unread, a cited block the
  ;; receipt does not hold; receipt-carried, when the write carried one.
  ;; -> ((<relation> <arity> <tuple> ...) ...).
  (define (write-facts targets receipt post-S pre-S ask edges-at)
    (let* ((kinds (apply append (map (lambda (id) (map (lambda (row) (cons id row)) (ask (post-S) (list 'kind id '?k))))
                                     targets)))
           (fields (apply append (map (lambda (id) (map (lambda (row) (cons id row)) (ask (post-S) (list 'field id '?f '?v))))
                                      targets)))
           (edges (unique (apply append (map (lambda (id) (edges-at (post-S) id 'edge)) targets))))
           (edge-kinds (unique (apply append (map (lambda (id) (edges-at (post-S) id 'edge-kind)) targets))))
           (cited (unique
                    (apply append
                           (map (lambda (id)
                                  (let ((now (ask (post-S) (list 'edge-kind id 'depends-on '?s)))
                                        (before (ask (pre-S) (list 'edge-kind id 'depends-on '?s))))
                                    (map (lambda (row) (list id (car row)))
                                         (filter (lambda (row) (not (member row before))) now))))
                                targets))))
           (unread (unique (map (lambda (c) (list (cadr c)))
                                (filter (lambda (c) (not (and receipt (member (cadr c) receipt)))) cited)))))
      (list (cons* 'kind+ 2 kinds)
            (cons* 'field+ 3 fields)
            (cons* 'edge+ 3 edges)
            (cons* 'edge-kind+ 3 edge-kinds)
            (cons* 'cited 2 cited)
            (cons* 'unread 1 unread)
            (cons* 'receipt-carried 0 (if receipt (list '()) '())))))
)
