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

;;; (theourgia incomplete) -- a reduction missing a writer, refused to a
;;; caller that did not say it accepts one (F77 R2e, delivered by F77c).
;;;
;;; A load that could not read every writer builds an INCOMPLETE reduction.
;;; Whoever obtains one must have DECLARED that it accepts one, and then
;;; carries the load's notes into what it answers; whoever did not declare
;;; is refused, and the refusal travels as the condition defined here.
;;;
;;; THE DECLARATION IS ONE VALUE, compared with eq?. It is passed as an
;;; argument, lexically, from the route that answers -- never a parameter:
;;; under the scheduler every green thread shares a parameter, so one
;;; request's declaration would be another's (F100b). And it is not a truth
;;; value: several of the functions it travels through take more than one
;;; argument of the same shape, and a #t that reached its place by a slip
;;; must not declare. #f, or anything else, is "not declared".
;;;
;;; THE NOTES are (writer path reason) triples, the form (theourgia log)
;;; keeps them in, for a writer or segment that could not be read; a
;;; writer whose history was CUT by readable damage has a fourth element,
;;; (writer path reason (cut <kind> <after>)): the damage's kind and the
;;; last sequence kept. `incomplete-note-clauses` is the one spelling of
;;; them in an answer, shared by the refusal below and by the clause a
;;; declared answer carries -- `unreadable` for a triple, `cut` for the
;;; other -- and `clause->note` is its inverse, so a note that crossed a
;;; wire as a clause comes back whole.
;;;
;;; THIS LIBRARY SITS BELOW (theourgia log) AND (theourgia answers), which
;;; both need the condition, and imports neither.

(library (theourgia incomplete)
  (export incomplete-accepted incomplete-refused declared? cut-note? refusing-notes
          make-incomplete-reduction incomplete-reduction? incomplete-reduction-notes
          incomplete-note-clauses clause->note incomplete-reduction-answer)
  (import (chezscheme))

  ;; A record nobody else can construct: the only instance is the value.
  (define-record-type incomplete-acceptance (fields))
  (define incomplete-accepted (make-incomplete-acceptance))
  (define (declared? declaration) (eq? declaration incomplete-accepted))

  ;; THE STRICT DECLARATION, for a consumer that writes a projection, a
  ;; snapshot or records from the reduction (rpc's undeclared verbs): it
  ;; accepts nothing missing, so a cut refuses it as an unreadable writer
  ;; does. Another value nobody else can construct.
  (define-record-type incomplete-refusal (fields))
  (define incomplete-refused (make-incomplete-refusal))

  ;; A CUT IS A NOTE, NEVER A LOAD REFUSAL, except for the strict consumer.
  ;; A writer that could not be read (a triple) leaves a reduction nobody
  ;; can build on unknowingly, and refuses every undeclared consumer. A
  ;; writer whose history was cut (four elements) still delivers its
  ;; prefix, and every other consumer keeps today's integrity semantics:
  ;; the write path's integrity refusal, writer-stopped with the adopt
  ;; remedy, adopt itself, snapshots and requests all load such a store.
  (define (cut-note? n) (and (list? n) (= (length n) 4)))
  (define (refusing-notes notes strict?)
    (if strict? notes (filter (lambda (n) (not (cut-note? n))) notes)))

  (define-condition-type &incomplete-reduction &error
    make-incomplete-reduction-condition incomplete-reduction?
    (notes incomplete-reduction-notes))

  (define (make-incomplete-reduction notes)
    (unless (and (list? notes) (pair? notes))
      (assertion-violation 'make-incomplete-reduction "notes must be a non-empty list" notes))
    (condition (make-incomplete-reduction-condition notes)
               (make-message-condition "the reduction is missing a writer")))

  ;; (writer path reason) -> (unreadable (writer w) (path p) (reason r));
  ;; (writer path reason (cut k n)) -> (cut (writer w) (path p) (reason r)
  ;; (kind k) (after n)).
  (define (incomplete-note-clauses notes)
    (map (lambda (u)
           (let ((cut (and (= (length u) 4) (pair? (cadddr u)) (eq? (car (cadddr u)) 'cut) (cadddr u))))
             (if cut
                 (list 'cut (list 'writer (car u)) (list 'path (cadr u)) (list 'reason (caddr u))
                       (list 'kind (cadr cut)) (list 'after (caddr cut)))
                 (list 'unreadable (list 'writer (car u))
                       (list 'path (cadr u)) (list 'reason (caddr u))))))
         notes))

  ;; THE INVERSE: a clause as incomplete-note-clauses spells it, back to its
  ;; note -- a triple from `unreadable`, four elements from `cut` -- or #f
  ;; for anything else.
  (define (clause->note c)
    (let ((field (lambda (k) (let ((f (and (pair? c) (list? c) (assq k (cdr c))))) (and f (pair? (cdr f)) (cadr f))))))
      (cond
        ((and (pair? c) (eq? (car c) 'unreadable))
         (list (field 'writer) (field 'path) (field 'reason)))
        ((and (pair? c) (eq? (car c) 'cut))
         (list (field 'writer) (field 'path) (field 'reason) (list 'cut (field 'kind) (field 'after))))
        (else #f))))

  ;; The refusal as an answer: (error incomplete-reduction (notes <clause> ...)).
  (define (incomplete-reduction-answer c)
    (list 'error 'incomplete-reduction
          (cons 'notes (incomplete-note-clauses (incomplete-reduction-notes c))))))
