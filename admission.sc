#!r6rs
;; Copyright 2026 guenchi
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
(library (theourgia admission)
  (export make-admission admission-copy admission-add! admission-gates admission-gated?
          admission-lifted admission-added admission-changed? pending-work-counter)
  (import (rnrs) (theourgia request)
          (only (chezscheme) make-parameter)
          (only (theourgia wire) sexpr->string-extended)
          (only (theourgia trace) trace-enabled? trace-event!)
          (theourgia degeneracy))

  ;; This index tracks causal availability independently of application.
  ;; Revoking an applied gate still makes the reducer replay its retained set.
  ;; JUDGEMENTS holds each bucket's kept judgement (request.sc, make-judgement):
  ;; an arrival is judged against it, not the bucket judged again.
  ;; THE GATES ARE A TABLE, event to status, changed in place by each touched
  ;; bucket's own diff; GATE-LIST is their sorted list, produced when asked
  ;; and kept until the table changes (#f until then).
  ;; LIFTED: the gate keys the last admission-add! removed from the buckets it
  ;; touched, for reduce.sc to wake the records it holds back under those keys.
  ;; ADDED: the keys it newly gated, for reduce.sc to ask whether one of them
  ;; was already applied (a reversal). CHANGED: whether it changed the gates at
  ;; all, a status included.
  (define-record-type (admission make-admission/raw admission?)
    (fields buckets events dependents waits pasts clock judgements
            gate-table (mutable gate-list) (mutable lifted) (mutable added) (mutable changed)))

  ;; THE COUNTING SEAM of the pending queue (reduce.sc re-exports it): #f, or a
  ;; procedure called with a kind and a count -- examine, scan or heap. Defined
  ;; here because the gate scan below is part of the queue's work and this
  ;; library is the lower one. A fixture binds it in process; nothing else
  ;; reads it.
  (define pending-work-counter (make-parameter #f))
  (define (work! kind n) (let ((c (pending-work-counter))) (when c (c kind n))))
  (define (table) (make-hashtable string-hash string=?))
  (define (key x) (sexpr->string-extended x))
  (define (ref t x default) (hashtable-ref t (key x) default))
  (define (put! t x value) (hashtable-set! t (key x) value))
  (define (add! t x value) (put! t x (cons value (ref t x '()))))
  (define (make-admission)
    (make-admission/raw (table) (table) (table) (table) (table) (table) (table)
                        (make-hashtable equal-hash equal?) '() '() '() #f))
  ;; A COPY THAT SHARES NOTHING IT COULD CHANGE: each table copied (its values
  ;; are replaced on update, never changed in place), each bucket's judgement
  ;; copied whole (its tables are changed in place by arrivals), the gate table
  ;; copied (it is changed in place), the sorted list and the reported keys
  ;; shared (they are replaced).
  (define (admission-copy a)
    (make-admission/raw (hashtable-copy (admission-buckets a) #t)
                        (hashtable-copy (admission-events a) #t)
                        (hashtable-copy (admission-dependents a) #t)
                        (hashtable-copy (admission-waits a) #t)
                        (hashtable-copy (admission-pasts a) #t)
                        (hashtable-copy (admission-clock a) #t)
                        (let ((copy (table)))
                          (let-values (((ids js) (hashtable-entries (admission-judgements a))))
                            (vector-for-each (lambda (id j) (hashtable-set! copy id (judgement-copy j))) ids js))
                          copy)
                        (hashtable-copy (admission-gate-table a) #t) (admission-gate-list a)
                        (admission-lifted a) (admission-added a) (admission-changed a)))
  ;; THE GATES, sorted: ((event . status) ...), produced from the table when
  ;; asked after a change, every element counted
  (define (admission-gates a)
    (or (admission-gate-list a)
        (let ((l (let-values (((events statuses) (hashtable-entries (admission-gate-table a))))
                   (gates-sorted (map cons (vector->list events) (vector->list statuses))))))
          (work! 'gate (length l))
          (admission-gate-list-set! a l)
          l)))
  ;; -> EVENT's gate status, or #f when it is not gated: one probe
  (define (admission-gated? a event) (hashtable-ref (admission-gate-table a) event #f))
  (define (admission-changed? a) (admission-changed a))
  ;; THE GATES' ORDER IS CANONICAL: by event, writer then seq, the order the
  ;; notes are made in (reduce.sc gate-notes). Nothing reads another order.
  (define (event<? a b)
    (or (string<? (car a) (car b)) (and (string=? (car a) (car b)) (< (cdr a) (cdr b)))))
  (define (gates-sorted gates) (list-sort (lambda (x y) (event<? (car x) (car y))) gates))
  (define (bucket-judgement a id)
    (or (ref (admission-judgements a) id #f)
        (let ((j (make-judgement))) (put! (admission-judgements a) id j) j)))

  (define (event rec) (cons (car rec) (cadr rec)))
  (define (actor rec) (list-ref rec 4))
  (define (identity rec) (actor-identity (actor rec)))
  (define (premises rec)
    (append (if (= (cadr rec) 1) '() (list (cons (car rec) (- (cadr rec) 1))))
            (caddr rec)))
  (define (join a b)
    (fold-left
      (lambda (out p)
        (let ((old (assoc (car p) out)))
          (cond ((and old (>= (cdr old) (cdr p))) out)
                (old (cons p (filter (lambda (x) (not (string=? (car x) (car p)))) out)))
                (else (cons p out))))) a b))
  (define (missing a rec)
    (find (lambda (p) (< (ref (admission-clock a) (car p) -1) (cdr p))) (premises rec)))
  (define (evidence a rec)
    (let ((past (ref (admission-pasts a) (event rec) #f)))
      (make-evidence (event rec) (actor rec) (if past past (caddr rec))
                     (cadddr rec) 'valid-history (and past #t) '())))

;; REHEARSAL?, when given and true, says the admission belongs to a
  ;; rehearsal's copy of a reduction: the probes' trace lines are marked.
  (define (admission-add! a rec . rehearsal?)
    (let ((changed (table)) (id (identity rec)) (woken '())
          (marked (and (pair? rehearsal?) (car rehearsal?))))
      (define (touch! id) (when id (put! changed id id)))
      (define (touch-event! e)
        (let ((r (ref (admission-events a) e #f))) (when r (touch! (identity r))))
        (for-each touch! (ref (admission-dependents a) e '())))
      (define (schedule! rec)
        (unless (ref (admission-pasts a) (event rec) #f)
          (let ((need (missing a rec)))
            (if need
                (add! (admission-waits a) need rec)
                (let* ((e (event rec))
                       (past (fold-left
                               (lambda (out p) (join out (ref (admission-pasts a) p '())))
                               (list e) (premises rec))))
                  (put! (admission-pasts a) e past)
                  (put! (admission-clock a) (car e) (cdr e))
                  (set! woken (cons rec woken))
                  (touch-event! e)
                  (let ((waiting (append (ref (admission-waits a) e '())
                                         (if (= (cdr e) 1)
                                             (ref (admission-waits a) (cons (car e) 0) '()) '()))))
                    (hashtable-delete! (admission-waits a) (key e))
                    (when (= (cdr e) 1)
                      (hashtable-delete! (admission-waits a) (key (cons (car e) 0))))
                    (for-each schedule! waiting)))))))
      (put! (admission-events a) (event rec) rec)
      (when id
        (add! (admission-buckets a) id rec)
        (touch! id)
        (let ((plan (actor-plan-event (actor rec))))
          (when plan (add! (admission-dependents a) plan id))))
      (touch-event! (event rec))
      (schedule! rec)
      ;; THE ARRIVALS: the record, then every record its delivery woke, each in
      ;; its own bucket's judgement (the record itself once, delivered or not).
      (when id (judgement-arrive! (bucket-judgement a id) (evidence a rec)))
      (for-each (lambda (w)
                  (let ((wid (identity w)))
                    (when (and wid (not (eq? w rec)))
                      (judgement-arrive! (bucket-judgement a wid) (evidence a w)))))
                (reverse woken))
      (let-values (((keys ids) (hashtable-entries changed)))
        (define lifted '())
        (define added '())
        (define moved #f)
        (define gates (admission-gate-table a))
        (vector-for-each
          (lambda (id)
            ;; THE PROBE TRACE IS THE WHOLE JUDGEMENT'S: every touched bucket,
            ;; in this table's order, probes each of its records, newest first,
            ;; marked in a rehearsal -- written only while tracing is on, so an
            ;; untraced admission does not walk the bucket.
            (when (trace-enabled?)
              (for-each (lambda (r) (trace-event! 'identity-probe (event r) (and marked '(rehearsal))))
                        (ref (admission-buckets a) id '())))
            (let ((j (ref (admission-judgements a) id #f)))
              (when j
                ;; THE BUCKET'S GATES REPLACED BY ITS OWN DIFF. A bucket's gates are
                ;; its judgement's fresh set, and no other bucket holds its records,
                ;; so replacing them in the table is deleting the keys it dropped
                ;; (LIFTED: no longer gated at all) and setting the keys it holds
                ;; now (ADDED when the table had none; a key whose reason changed
                ;; stays and lifts nothing). The work is the bucket's own gates,
                ;; before and after, never the whole gate set.
                (let ((old (judgement-fresh-gates j)))
                  (judgement-run! j)
                  (let ((new (judgement-fresh-gates j)) (kept (make-hashtable equal-hash equal?)))
                    (work! 'scan (+ (length old) (length new)))
                    (work! 'gate (+ (length old) (length new)))
                    (for-each (lambda (p) (hashtable-set! kept (car p) #t)) new)
                    (for-each (lambda (p)
                                (unless (hashtable-contains? kept (car p))
                                  (hashtable-delete! gates (car p))
                                  (set! lifted (cons (car p) lifted))
                                  (set! moved #t)))
                              old)
                    (for-each (lambda (p)
                                (let ((was (hashtable-ref gates (car p) #f)))
                                  (cond ((not was) (set! added (cons (car p) added)) (set! moved #t))
                                        ((not (equal? was (cdr p))) (set! moved #t)))
                                  (hashtable-set! gates (car p) (cdr p))))
                              new))))))
          ids)
        (when moved (admission-gate-list-set! a #f))
        (admission-lifted-set! a lifted)
        (admission-added-set! a added)
        (admission-changed-set! a moved)
        (degeneracy-check (judgements-compare a ids)))
      (if #f #f)))

  ;; THE WHOLE JUDGEMENT BESIDE THE KEPT ONE, under the degeneracy switch only
  ;; (degeneracy.sc): after every admission, every touched bucket (the
  ;; record's, the woken records', the dependents') judged as a whole from
  ;; scratch, and its final statuses, its classification-stage bindings and
  ;; its gates compared with the kept judgement's; a difference raises at
  ;; that record, naming the bucket and what differs.
  (degeneracy-definitions
    ;; THE REFERENCE: one bucket judged as a whole, from scratch, as every
    ;; admission did before judgements followed only what an arrival can
    ;; change. -> (values statuses bindings fresh), fresh being the non-valid
    ;; statuses, in the order the whole judgement gives them.
    (define (whole-bucket-judgement a id)
      (let ((es (map (lambda (r) (evidence a r)) (ref (admission-buckets a) id '()))))
        (let-values (((statuses bindings) (identity-gates/full es)))
          (values statuses bindings (filter (lambda (p) (not (eq? (cdr p) 'valid))) statuses)))))
    (define (judgements-compare a ids)
      (vector-for-each
        (lambda (id)
          (let ((j (ref (admission-judgements a) id #f)))
            (when j
              (let-values (((statuses bindings fresh) (whole-bucket-judgement a id)))
                (define (differ! what kept whole)
                  (unless (equal? kept whole)
                    (assertion-violation 'degeneracy "the kept judgement differs from the whole one"
                                         id what kept whole)))
                (differ! 'statuses (gates-sorted (judgement-finals j)) (gates-sorted statuses))
                (differ! 'bindings
                         (list-sort event<? (judgement-bound-events j))
                         (list-sort event<? (map ev-event bindings)))
                (differ! 'gates (gates-sorted (judgement-fresh-gates j)) (gates-sorted fresh))
                (differ! 'gate-list
                         (filter (lambda (p) (judgement-holds? j (car p))) (admission-gates a))
                         (gates-sorted fresh))))))
        ids)))
)
