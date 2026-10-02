#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;;; (theourgia commitments) -- which decisions are still owed.
;;;
;;; A DECISION IS A BLOCK OF KIND `decision`, AND IT CARRIES AN OBLIGATION.
;;; The obligation is discharged in one of two ways: something implements
;;; it (an incoming `implements` edge, `link <impl> implements <decision>`),
;;; or its `status` field says done or dropped. Until then it is open, and
;;; `commitments` lists it.
;;;
;;; TIME HERE IS A CUT, NEVER A NUMBER. A decision's ORIGIN is the causal
;;; cut just after the put that created it, and orders the listing.
;;;
;;; WHAT DISCHARGES A DECISION, WHO IMPLEMENTS IT AND WHO HAS DRIFTED ARE
;;; THE LIFECYCLE PROVIDER'S, read here and defined nowhere else: a decision
;;; is open when its state there is open; each live implementer is listed
;;; with the cut at which it was said to implement the decision (its links'
;;; join); one DRIFTS when its own content moved past every single witness
;;; of the edge -- each link of it, each content write of the decision --
;;; and is listed with that content's cut. Sequence numbers of two writers
;;; are never compared with each other.
;;;
;;; NEVER: NOTHING IS WRITTEN. The query reads the committed reduction --
;;; in the daemon, the published one, which it does not change.
;;;
;;; This library is entered on dispatch of `commitments` only (rpc.sc's
;;; registry); the entry that names it is data in (theourgia extensions).
(library (theourgia commitments)
  (export commitments-verb commitments-answer)
  (import (rnrs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids state-put-events state-event-cut
                cut-covers? block-id)
          (only (theourgia lifecycle) lifecycle implementation-of decision-state-of)
          (only (theourgia store) parse-cut)
          (only (theourgia project) subtree-ids)
          (only (theourgia extensions) commitments-usage)
          (only (theourgia template-read) query-scope-root)
          (only (theourgia field-reading) field-of conflict-form? conflict-values lenient-status
                decision-statuses rows-left-out))

  ;; The handler, with the eight arguments every verb's handler takes.
  (define (commitments-verb store actor args req options state writer cwd)
    (let ((open (argument-option options "--open"))
          (all (argument-option options "--all"))
          (drifted (argument-option options "--drifted"))
          (since-text (argument-option options "--since"))
          (under (argument-option options "--under")))
      (let ((since (and since-text (parse-cut since-text))))
        (if (or (not (null? args)) (and open all) (and since-text (not since))
                (and under (= 0 (string-length under))))
            ((dispatch-helper 'usage) commitments-usage)
            ((dispatch-helper 'guarded)
             (lambda ()
               (let ((view ((dispatch-helper 'reduction-for) store state)))
                 (commitments-answer view (if all 'all 'open) (and drifted #t) since under))))))))

  ;; ---- reading one field ----------------------------------------------------

  ;; WHAT KIND OF ROW A BLOCK IS FOR THIS QUERY: decision, a skip reason,
  ;; or #f for a block that is not a decision at all.
  ;;
  ;; A KIND SPELT AS THE STRING "decision" IS NOT A DECISION: a legacy store
  ;; can hold it, and listing it as one would answer for a block whose kind
  ;; the reducer does not know. It is said, as a skipped row. A kind in
  ;; conflict where one side is a decision cannot be called either, and is
  ;; said the same way.
  (define (row-kind row)
    (let ((k (field-of row 'kind)))
      (cond ((eq? k 'decision) 'decision)
            ((equal? k "decision") 'kind-not-a-symbol)
            ((and (conflict-form? k)
                  (exists (lambda (v) (or (eq? v 'decision) (equal? v "decision")))
                          (conflict-values k)))
             'unreadable-block)
            (else #f))))

  ;; STATUS IS READ LENIENTLY: the symbol or the string spelling of open,
  ;; done or dropped. Anything else is open, and the answer says which.
  (define (status-reading v) (lenient-status v decision-statuses))

  ;; ---- cuts -----------------------------------------------------------------

  (define (event-cut view event)
    (or (state-event-cut view event)
        (assertion-violation 'commitments "an applied event has no causal cut" event)))

  (define (sorted-cut cut)
    (list-sort (lambda (x y) (string<? (car x) (car y))) cut))

  (define (comparable? a b) (or (cut-covers? a b) (cut-covers? b a)))

  ;; ---- the answer -----------------------------------------------------------

  ;; One decision as the query sees it.
  (define-record-type decision
    (fields id event title status origin implementers drifted deleted discharged))

  ;; THE SCOPE: `--under <id>` is that block's subtree, `--under root` the
  ;; whole store, and with neither the root the store's template names for
  ;; commitments, when it has a readable one and exactly one block carries
  ;; that root's slug; otherwise the whole store.
  ;;
;; NEVER: A SCOPE THE CALLER DID NOT ASK FOR IS SAID. When the template chose
  ;; it, the last item is (scope <root-id> (outside <n>)): n is the number of
  ;; rows this same request would list with --under root that this answer does
  ;; not (rows-left-out, field-reading.sc), so a narrowed ledger of obligations
  ;; never reads as the whole one, and the count obeys the same filters. An
  ;; item, not a clause, because the human rendering prints items.
  (define (commitments-answer view which drifted-only since under)
    (let* ((root (and (not under) (query-scope-root view 'commitments)))
           (scope (cond ((equal? under "root") #f)
                        (under (subtree-ids view under))
                        (root (subtree-ids view root))
                        (else #f))))
      (if (and under (not (equal? under "root")) (not scope))
          ((dispatch-helper 'unknown-id) view under)
          (let* ((L (lifecycle view))
                 (rows (commitment-rows L view which drifted-only since scope)))
            (append
              ((dispatch-helper 'items)
               (if (and root scope)
                   (append rows
                           (list (list 'scope root
                                       (list 'outside (rows-left-out
                                                        rows
                                                        (commitment-rows L view which drifted-only since #f))))))
                   rows))
              ((dispatch-helper 'receipt) view (listed-ids rows)))))))

  ;; THE BLOCKS THIS ANSWER LISTS, in order of first appearance: each
  ;; decision, then the implementers its row names (a drifted one is among
  ;; them), then the decisions it names as concurrent with it, and each
  ;; skipped row's block. A deleted implementer has no version and the
  ;; receipt leaves it out; the scope item names a root, not a result.
  (define (listed-ids rows)
    (apply append
           (map (lambda (r)
                  (cond
                    ((and (pair? r) (eq? (car r) 'decision))
                     (cons (cadr r)
                           (append (map car (cdr (assq 'implemented-by (cddr r))))
                                   (let ((c (assq 'concurrent-with (cddr r)))) (if c (cdr c) '())))))
                    ((and (pair? r) (eq? (car r) 'skipped)) (list (cadr r)))
                    (else '())))
                rows)))

  ;; The decision rows and the skipped rows for one scope (#f: the whole
  ;; store), with the request's filters.
  (define (commitment-rows L view which drifted-only since scope)
    (let ((origins (make-hashtable string-hash string=?)))
      (for-each (lambda (event)
                  (hashtable-set! origins (block-id (car event) (cdr event)) event))
                (state-put-events view))
      (let loop ((ids (state-block-ids view)) (decisions '()) (skipped '()))
        (if (null? ids)
            (append (listing (filter (lambda (d) (wanted? d which drifted-only since))
                                     decisions))
                    (map (lambda (s) (list 'skipped (car s) (cdr s)))
                         (list-sort (lambda (x y) (string<? (car x) (car y))) skipped)))
            (let* ((id (car ids))
                   (row (and (or (not scope) (member id scope)) (state-read view id)))
                   (kind (and row (row-kind row))))
              (cond
                ((not kind) (loop (cdr ids) decisions skipped))
                ((not (eq? kind 'decision))
                 (loop (cdr ids) decisions (cons (cons id kind) skipped)))
                ((not (string? (field-of row 'title)))
                 (loop (cdr ids) decisions (cons (cons id 'unreadable-block) skipped)))
                ((not (hashtable-ref origins id #f))
                 (loop (cdr ids) decisions (cons (cons id 'no-origin) skipped)))
                (else
                 (loop (cdr ids)
                       (cons (read-decision L view id row (hashtable-ref origins id #f))
                             decisions)
                       skipped))))))))

  ;; A DELETED SOURCE DOES NOT DISCHARGE AND IS NOT LISTED AS AN
  ;; IMPLEMENTER; it is named apart, so a decision a deletion reopened says
  ;; why. All three lists, and whether the decision is discharged, are the
  ;; provider's.
  (define (read-decision L view id row event)
    (let ((impl (implementation-of L id)))
      (make-decision id event (field-of row 'title) (status-reading (field-of row 'status))
                     (event-cut view event)
                     (map (lambda (i) (list (car i) (sorted-cut (cadr i)))) (cdr (assq 'implemented-by impl)))
                     (map (lambda (i) (list (car i) (sorted-cut (cadr i)))) (cdr (assq 'drifted impl)))
                     (cdr (assq 'deleted impl))
                     (not (eq? 'open (car (decision-state-of L id)))))))

  (define (wanted? d which drifted-only since)
    (and (or (eq? which 'all) (not (decision-discharged d)))
         (or (not drifted-only) (pair? (decision-drifted d)))
         (or (not since) (not (cut-covers? since (decision-origin d))))))

  ;; OLDEST ORIGIN FIRST, AS A TOPOLOGICAL ORDER: of the decisions not yet
  ;; listed, those whose origin covers no other's are eligible, and the
  ;; creating put's (writer, seq) chooses among them. Never a comparison of
  ;; two origins that are not ordered -- three incomparable cuts compared
  ;; pairwise can form a cycle. Origins not ordered against another listed
  ;; one say so, by id, in listing order.
  ;;
  ;; ONE PASS, NOT A SEARCH PER STEP. Each decision starts with the number of
  ;; other origins its own covers; it is eligible at zero, and listing one
  ;; takes one from every decision still waiting whose origin covers it. Two
  ;; distinct origins never cover each other (each holds its own put, which
  ;; the other's past cannot contain unless it is later), so the counts are
  ;; those of a graph without cycles. Asking each step again which decisions
  ;; covered none of the remaining ones gave the same order at a cost cubic
  ;; in the number of decisions.
  (define (listing decisions)
    (let* ((v (list->vector decisions))
           (n (vector-length v))
           (origin (lambda (i) (decision-origin (vector-ref v i))))
           (count (make-vector n 0))
           (listed (make-vector n #f)))
      (do ((i 0 (+ i 1))) ((= i n))
        (do ((j 0 (+ j 1))) ((= j n))
          (when (and (not (= i j)) (cut-covers? (origin i) (origin j)))
            (vector-set! count i (+ 1 (vector-ref count i))))))
      (let ((ordered
              (let loop ((k 0) (out '()))
                (if (= k n)
                    (reverse out)
                    (let ((first
                            (let pick ((i 0) (best #f))
                              (cond ((= i n) best)
                                    ((and (not (vector-ref listed i)) (= 0 (vector-ref count i))
                                          (or (not best)
                                              (put<? (decision-event (vector-ref v i))
                                                     (decision-event (vector-ref v best)))))
                                     (pick (+ i 1) i))
                                    (else (pick (+ i 1) best))))))
                      (vector-set! listed first #t)
                      (do ((i 0 (+ i 1))) ((= i n))
                        (when (and (not (vector-ref listed i)) (cut-covers? (origin i) (origin first)))
                          (vector-set! count i (- (vector-ref count i) 1))))
                      (loop (+ k 1) (cons (vector-ref v first) out)))))))
      (map (lambda (d)
             (let ((concurrent (filter (lambda (o) (and (not (eq? o d))
                                                        (not (comparable? (decision-origin d) (decision-origin o)))))
                                       ordered)))
               (append
                 (list 'decision (decision-id d)
                       (list 'title (decision-title d))
                       (cons 'status (let ((s (decision-status d))) (if (pair? s) s (list s))))
                       (list 'origin (sorted-cut (decision-origin d)))
                       (cons 'implemented-by (decision-implementers d)))
                 (if (null? (decision-drifted d)) '() (list (cons 'drifted (decision-drifted d))))
                 (if (null? (decision-deleted d)) '() (list (cons 'implementer-deleted (decision-deleted d))))
                 (if (null? concurrent) '() (list (cons 'concurrent-with (map decision-id concurrent)))))))
           ordered))))

  (define (put<? a b)
    (or (string<? (car a) (car b))
        (and (string=? (car a) (car b)) (< (cdr a) (cdr b))))))
