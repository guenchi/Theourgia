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

;;; (theourgia lifecycle) -- whether a block is still in force, and where a
;;; decision stands.
;;;
;;; ONE PROVIDER, DERIVED, NOTHING STORED. `(lifecycle state)` is built once
;;; per request and computes what is asked of it on first use; every reader
;;; of validity or of a decision's state (commitments, read, the search
;;; filters) asks it, and none keeps a copy of a rule.
;;;
;;; THE RELATIONS WITH AN EFFECT are one table below; every other relation
;;; name has no effect and is still an edge. The reducer holds their NAMES
;;; (effect-relation-names), so that a reader can tell a store that links
;;; none of them without loading this library; the two must name the same
;;; relations, and this library refuses to load when they do not.
;;;
;;;   supersedes      newer -> older       the older block is superseded
;;;   refutes         evidence -> claim    the claim is refuted while the
;;;                                        edge is current, needs review
;;;                                        once the claim moved past it
;;;   depends-on      dependent -> premise the dependent needs review when
;;;                                        the premise is gone, superseded,
;;;                                        refuted, needs review, or moved
;;;   implements      implementer -> decision
;;;                                        discharges the decision; the
;;;                                        implementer needs review when
;;;                                        either end moved, or the decision
;;;                                        is gone, superseded, refuted or
;;;                                        needs review
;;;   verifies        check -> subject     positive evidence while current
;;;   conflicts-with  block <-> block      no effect here unless proposed
;;;
;;; AN EFFECT NEEDS A LIVE SOURCE (the block exists and is not deleted;
;;; "live" never means "valid", so no effect depends on its source's own
;;; validity and the rules have no circle). An edge from a block to itself
;;; has no effect. supersedes, refutes and conflicts-with take effect only
;;; from an AUTHORITATIVE source; from any other they are proposals, and
;;; the target needs review with the reason proposed-<relation>.
;;;
;;; VALIDITY is one of superseded, refuted, needs-review, valid, in that
;;; order of precedence, with the reasons of the one answered. needs-review
;;; is a least fixed point: a block enters it once, when a first reason
;;; holds, and its dependents are visited then, so a cycle terminates. The
;;; answer, reasons included, does not depend on the order the edges are
;;; visited in: the reasons are sorted.
(library (theourgia lifecycle)
  (export lifecycle lifecycle-state lifecycle-fast? effect-table effect-relations
          effective-class authoritative? validity-of validity-reasons decision-state-of
          edge-watch verified-by implementation-of validity-filter
          read-validity-clause listed-validity-clause search-clauses search-filter whereis-split)
  (import (rnrs)
          (only (theourgia reduce) state-read state-edges state-effect-relation?
                effect-relation-names known-classes state-field-contested?)
          (prefix (theourgia attest) attest:)
          (only (theourgia field-reading) field-of lenient-status decision-statuses task-statuses))

  ;; (<relation> <what it watches>): the ends whose movement the relation
  ;; reads. The effect of each is in validity-table and decision-state-of.
  (define effect-table
    '((supersedes nothing)
      (refutes target)
      (depends-on target)
      (implements both)
      (verifies target)
      (conflicts-with nothing)))

  (define effect-relations (map car effect-table))

  ;; NEVER: TWO LISTS OF ONE THING THAT DISAGREE. A relation the table gives
  ;; an effect and the reducer does not name would be skipped by the fast
  ;; path on every store that links nothing else; a name the reducer lists
  ;; and the table lacks would take the slow path for nothing.
  ;; A definition, not an expression: a library body's definitions come
  ;; first, and this one is evaluated when the library is.
  (define effect-table-checked
    (or (and (for-all (lambda (n) (memq n effect-relation-names)) effect-relations)
             (for-all (lambda (n) (memq n effect-relations)) effect-relation-names))
        (assertion-violation 'lifecycle "the effect table and the reducer's effect-relation names differ"
                             effect-relations effect-relation-names)))

  ;; ---- class ------------------------------------------------------------------

  ;; THE EFFECTIVE CLASS: the symbol or its string spelling; absent gives the
  ;; default, ruling for a decision and observation for anything else; a
  ;; field in conflict gives conflict; anything else unreadable. CONTESTED is
  ;; the reducer's answer for the row's class (state-field-contested?).
  (define (effective-class row contested)
    (let ((c (lenient-status (field-of row 'class) known-classes contested)))
      (cond ((eq? c 'absent) (if (eq? (field-of row 'kind) 'decision) 'ruling 'observation))
            ((symbol? c) c)
            (else 'unreadable))))

  ;; NEVER: A CLASS IN CONFLICT, OR ONE NOBODY CAN READ, IS NOT AUTHORITY.
  (define (authoritative? class) (and (memq class '(observation ruling verification)) #t))

  ;; ---- the provider -----------------------------------------------------------

  (define-record-type (provider new-provider provider?)
    (fields state attestation fast rows by-target (mutable table) (mutable targets) left-out))

  ;; The optional argument permutes the edges before they are visited; the
  ;; answers do not depend on it, and a row says so.
  (define (lifecycle state . order)
    (let ((fast (not (state-effect-relation? state))))
      (new-provider state (attest:make-attestation state) fast
                    (make-hashtable string-hash string=?)
                    (if fast '() ((if (pair? order) (car order) (lambda (es) es))
                                  (filter (lambda (e) (memq (cadr e) effect-relations))
                                          (state-edges state))))
                    #f #f (make-hashtable equal-hash equal?))))

  (define (lifecycle-state L) (provider-state L))
  (define (lifecycle-fast? L) (provider-fast L))

  (define (row-of L id)
    (let ((memo (provider-rows L)))
      (let ((r (hashtable-ref memo id 'none)))
        (if (eq? r 'none)
            (let ((r (state-read (provider-state L) id)))
              (hashtable-set! memo id r)
              r)
            r))))

  (define (live? L id)
    (let ((row (row-of L id))) (and row (not (cdr (assq 'deleted row))))))

  (define (contested? L id field row) (state-field-contested? (provider-state L) id field (field-of row field)))
  (define (authority? L id) (let ((row (row-of L id))) (authoritative? (effective-class row (contested? L id 'class row)))))

  ;; The edges of one relation to one block, from an index of the edges by
  ;; target built once, on first use.
  (define (edges-to L id rel)
    (let ((t (or (provider-targets L)
                 (let ((t (make-hashtable equal-hash equal?)))
                   (for-each (lambda (e) (hashtable-set! t (caddr e) (cons e (hashtable-ref t (caddr e) '()))))
                             (reverse (provider-by-target L)))
                   (provider-targets-set! L t)
                   t))))
      (filter (lambda (e) (eq? (cadr e) rel)) (hashtable-ref t id '()))))

  (define (edge-watch L a rel b) (attest:edge-watch (provider-attestation L) a rel b))

  ;; ---- validity -----------------------------------------------------------------

  ;; THE REASONS, in the order a reason list is sorted by. Each is (<why>
  ;; <by>), <by> being the block that causes it.
  (define reason-order
    '(superseded-by refuted-by refutation-moved
      proposed-supersedes proposed-refutes proposed-conflicts-with
      premise-gone premise-superseded premise-refuted premise-needs-review premise-moved
      implementation-moved))

  (define (reason<? x y)
    (let ((i (length (memq (car x) reason-order))) (j (length (memq (car y) reason-order))))
      (or (> i j) (and (= i j) (string<? (cadr x) (cadr y))))))

  (define (sorted-reasons rs) (list-sort reason<? rs))

  ;; -> a hashtable from id to (<validity> <reason> ...), for every block that
  ;; is not valid. Built on first use.
  (define (validity-table L)
    (or (provider-table L)
        (let ((t (compute-validity L)))
          (provider-table-set! L t)
          t)))

  (define (compute-validity L)
    (let ((A (provider-attestation L))
          (sup (make-hashtable string-hash string=?))
          (ref (make-hashtable string-hash string=?))
          (nr (make-hashtable string-hash string=?))
          (dependents (make-hashtable string-hash string=?)))
      (define (add! t id reason)
        (let ((rs (hashtable-ref t id '())))
          (unless (member reason rs) (hashtable-set! t id (cons reason rs)))))
      (define (direct-validity id)
        (cond ((pair? (hashtable-ref sup id '())) 'superseded)
              ((pair? (hashtable-ref ref id '())) 'refuted)
              (else #f)))
      ;; THE DIRECT EFFECTS: superseded and refuted read no other block's
      ;; validity, so they are settled in one pass, before the fixed point.
      (for-each
        (lambda (e)
          (let ((a (car e)) (rel (cadr e)) (b (caddr e)))
            (when (and (not (equal? a b)) (live? L a))
              (let ((auth (authority? L a)))
                ;; A TARGET THAT IS GONE TAKES NO EFFECT, ruling or proposal:
                ;; its dependents answer premise-gone, and nothing else.
                (case (if (and (memq rel '(supersedes refutes conflicts-with)) (attest:end-gone? A b)) 'none rel)
                  ((supersedes)
                   (if auth (add! sup b (list 'superseded-by a)) (add! nr b (list 'proposed-supersedes a))))
                  ((refutes)
                   (cond ((not auth) (add! nr b (list 'proposed-refutes a)))
                         ((attest:target-moved? A a rel b) (add! nr b (list 'refutation-moved a)))
                         (else (add! ref b (list 'refuted-by a)))))
                  ((conflicts-with)
                   (unless auth (add! nr b (list 'proposed-conflicts-with a))))
                  ((depends-on implements)
                   (hashtable-set! dependents b (cons a (hashtable-ref dependents b '())))
                   (if (attest:end-gone? A b)
                       (add! nr a (list 'premise-gone b))
                       (begin
                         (when (attest:target-moved? A a rel b) (add! nr a (list 'premise-moved b)))
                         (when (and (eq? rel 'implements) (attest:source-moved? A a rel b))
                           (add! nr a (list 'implementation-moved b))))))
                  (else #f))))))
        (provider-by-target L))
      ;; A premise superseded or refuted is a reason of its dependent's.
      (for-each
        (lambda (e)
          (let ((a (car e)) (rel (cadr e)) (b (caddr e)))
            (when (and (memq rel '(depends-on implements)) (not (equal? a b)) (live? L a)
                       (not (attest:end-gone? A b)))
              (case (direct-validity b)
                ((superseded) (add! nr a (list 'premise-superseded b)))
                ((refuted) (add! nr a (list 'premise-refuted b)))
                (else #f)))))
        (provider-by-target L))
      ;; THE FIXED POINT: a block whose validity is needs-review gives each
      ;; live dependent the reason premise-needs-review. A block is queued
      ;; once, when it first needs review, so a cycle ends.
      (let ((queued (make-hashtable string-hash string=?)))
        (define (needs-review? id) (and (not (direct-validity id)) (pair? (hashtable-ref nr id '()))))
        (let loop ((work (filter needs-review? (vector->list (hashtable-keys nr)))))
          (for-each (lambda (id) (hashtable-set! queued id #t)) work)
          (unless (null? work)
            (let ((next '()))
              (for-each
                (lambda (b)
                  (for-each
                    (lambda (a)
                      (unless (equal? a b)
                        (add! nr a (list 'premise-needs-review b))
                        (when (and (needs-review? a) (not (hashtable-ref queued a #f)) (not (member a next)))
                          (set! next (cons a next)))))
                    (hashtable-ref dependents b '())))
                work)
              (loop next)))))
      (let ((t (make-hashtable string-hash string=?)))
        (for-each (lambda (id) (hashtable-set! t id (cons 'superseded (sorted-reasons (hashtable-ref sup id '())))))
                  (vector->list (hashtable-keys sup)))
        (for-each (lambda (id)
                    (unless (hashtable-contains? t id)
                      (hashtable-set! t id (cons 'refuted (sorted-reasons (hashtable-ref ref id '()))))))
                  (vector->list (hashtable-keys ref)))
        (for-each (lambda (id)
                    (unless (hashtable-contains? t id)
                      (hashtable-set! t id (cons 'needs-review (sorted-reasons (hashtable-ref nr id '()))))))
                  (vector->list (hashtable-keys nr)))
        t)))

  ;; -> (<validity> (<why> <by>) ...). THE FAST PATH: a state with no
  ;; effect-bearing link answers valid without reading a cut.
  (define (validity-of L id)
    (if (provider-fast L)
        '(valid)
        (hashtable-ref (validity-table L) id '(valid))))

  (define (validity-reasons v) (cdr v))

  ;; FOR THE SEARCH FILTERS: a procedure from a block id to #f, superseded
  ;; or refuted -- the validities a default search leaves out.
  (define (validity-filter L)
    (lambda (id)
      (let ((v (car (validity-of L id))))
        (and (memq v '(superseded refuted)) v))))

  ;; ---- what a read or a search says ---------------------------------------------
  ;;
  ;; Built here and not in the dispatcher, so that a start which answers no
  ;; read of a store with an effect-bearing link compiles none of it.

  ;; `(validity <v> <reason> ...)` for a block that is not valid; nothing for
  ;; one that is. A clause saying `valid` on every read would be a constant.
  (define (read-validity-clause L id)
    (let ((v (validity-of L id)))
      (if (eq? (car v) 'valid) '() (list (cons 'validity v)))))

  ;; `(validity ((<id> <v> <reason> ...) ...))` for the listed blocks that are
  ;; not valid, in the order listed and each once; nothing when there is none.
  (define (listed-validity-clause L ids)
    (let* ((seen (make-hashtable string-hash string=?))
           (rows (filter (lambda (r) (not (eq? (cadr r) 'valid)))
                         (map (lambda (id) (cons id (validity-of L id)))
                              (filter (lambda (id) (and (not (hashtable-ref seen id #f))
                                                        (begin (hashtable-set! seen id #t) #t)))
                                      ids)))))
      (if (null? rows) '() (list (list 'validity rows)))))

  ;; A search verb's clauses: `(excluded (blocks (superseded <n>) (refuted
  ;; <n>)))` when the filter left a block out, counted in blocks, then the
  ;; listed hits that are not valid.
  (define (search-clauses L ids)
    (let* ((vs (let-values (((ks vals) (hashtable-entries (provider-left-out L)))) (vector->list vals)))
           (sup (length (filter (lambda (v) (eq? v 'superseded)) vs)))
           (ref (length (filter (lambda (v) (eq? v 'refuted)) vs))))
      (append (if (> (+ sup ref) 0)
                  (list (list 'excluded (list 'blocks (list 'superseded sup) (list 'refuted ref))))
                  '())
              (listed-validity-clause L ids))))

  ;; THE FILTER HANDED TO store-search AND store-grep: true for a block to
  ;; skip, which it records here, once per block, so the store library
  ;; only skips and the counts are the provider's. #f when nothing is left
  ;; out (--all-validity, which then names what is not valid).
  (define (search-filter L all?)
    (and (not all?)
         (let ((f (validity-filter L)))
           (lambda (id)
             (let ((v (f id)))
               (and v (begin (hashtable-set! (provider-left-out L) id v) #t)))))))

  ;; WHEREIS FILTERS THE RECORDS AFTER THE NAME WAS FOUND: a name whose every
  ;; record is left out answers no items and says so, not unknown-name. The
  ;; records kept; the blocks left out are recorded as the search filter
  ;; records them.
  (define (whereis-split L records all?)
    (let ((skip? (search-filter L all?)))
      (if skip? (filter (lambda (r) (not (skip? (cadr r)))) records) records)))

  ;; ---- a decision -----------------------------------------------------------------

  ;; WHETHER AN IMPLEMENTS EDGE DISCHARGES DEPENDS ON THE KIND OF ITS SOURCE,
  ;; a dispatch with one default clause. A task discharges only once it is
  ;; done: an implements edge from a task says what will implement the
  ;; decision, not that it has.
  (define (edge-discharges? L source)
    (let ((row (row-of L source)))
      (case (and row (let ((k (field-of row 'kind))) (and (symbol? k) k)))
        ((task) (eq? 'done (lenient-status (field-of row 'status) task-statuses (contested? L source 'status row))))
        (else #t))))

  (define (implements-sources L d)
    (let ((seen (make-hashtable string-hash string=?)))
      (list-sort string<?
                 (filter (lambda (a) (and (not (hashtable-ref seen a #f)) (hashtable-set! seen a #t) #t))
                         (map car (edges-to L d 'implements))))))

  ;; -> ((implemented-by (<source> <said>) ...) (drifted (<source> <changed>)
  ;; ...) (deleted <source> ...)), sources by id. A drifted implementer is one
  ;; whose own content moved past every witness of the edge
  ;; (source-moved); a deleted or unknown source is named apart and neither
  ;; discharges nor is listed.
  (define (implementation-of L d)
    (let* ((A (provider-attestation L))
           ;; A DECISION THAT IS GONE IS IMPLEMENTED BY NOTHING: it takes no
           ;; effect, as a gone target of any relation does.
           (sources (if (attest:end-gone? A d) '() (implements-sources L d)))
           (deleted (filter (lambda (a) (attest:end-gone? A a)) sources))
           (live (filter (lambda (a) (not (member a deleted))) sources)))
      (list (cons 'implemented-by (map (lambda (a) (list a (attest:said A a 'implements d))) live))
            (cons 'drifted (map (lambda (a) (list a (attest:changed A a)))
                                (filter (lambda (a) (and (not (equal? a d)) (attest:source-moved? A a 'implements d)))
                                        live)))
            (cons 'deleted deleted))))

  ;; -> the live blocks with a verifies edge to id, by id, each (<v> current)
  ;; or (<v> moved): moved when id changed past every witness of the edge. A
  ;; subject that is deleted or unknown is gone, and verified by nothing.
  (define (verified-by L id)
    (let ((A (provider-attestation L)) (seen (make-hashtable string-hash string=?)))
      (if (attest:end-gone? A id) '() (map (lambda (v) (list v (if (attest:target-moved? A v 'verifies id) 'moved 'current)))
           (list-sort string<?
                      (filter (lambda (v) (and (not (equal? v id)) (live? L v)
                                               (not (hashtable-ref seen v #f)) (hashtable-set! seen v #t) #t))
                              (map car (edges-to L id 'verifies))))))))

  ;; -> #f for a block that is not a decision, or one that is gone; otherwise
  ;; the first of these that holds, in this order:
  ;;   (closed <status>)            its status reads done or dropped
  ;;   (open)                       no live implementer discharges it
  ;;   (review (<impl> <end> ...) ...) a live implements pair has moved
  ;;   (verified <v> ...)           every pair current, and a current verifies
  ;;   (implemented)                every pair current
  ;; An implements edge from the decision to itself discharges it, as it
  ;; always has, and is never a moved pair.
  (define (decision-state-of L d)
    (let ((row (row-of L d)))
      (and row (not (cdr (assq 'deleted row))) (eq? (field-of row 'kind) 'decision)
           (let ((status (lenient-status (field-of row 'status) decision-statuses (contested? L d 'status row)))
                 (live (cdr (assq 'implemented-by (implementation-of L d)))))
             (cond
               ((or (eq? status 'done) (eq? status 'dropped)) (list 'closed status))
               ((not (exists (lambda (i) (edge-discharges? L (car i))) live)) '(open))
               (else
                (let ((moved (filter pair?
                                     (map (lambda (i)
                                            (let ((w (edge-watch L (car i) 'implements d)))
                                              (and (eq? (car w) 'moved) (cons (car i) (cdr w)))))
                                          live)))
                      (verifiers (map car (filter (lambda (v) (eq? (cadr v) 'current)) (verified-by L d)))))
                  (cond ((pair? moved) (cons 'review moved))
                        ((pair? verifiers) (cons 'verified verifiers))
                        (else '(implemented)))))))))))
