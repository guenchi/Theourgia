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
;;; keeps them in; `incomplete-note-clauses` is the one spelling of them
;;; in an answer, shared by the refusal below and by the clause a declared
;;; answer carries.
;;;
;;; THIS LIBRARY SITS BELOW (theourgia log) AND (theourgia answers), which
;;; both need the condition, and imports neither.

(library (theourgia incomplete)
  (export incomplete-accepted declared?
          make-incomplete-reduction incomplete-reduction? incomplete-reduction-notes
          incomplete-note-clauses incomplete-reduction-answer)
  (import (chezscheme))

  ;; A record nobody else can construct: the only instance is the value.
  (define-record-type incomplete-acceptance (fields))
  (define incomplete-accepted (make-incomplete-acceptance))
  (define (declared? declaration) (eq? declaration incomplete-accepted))

  (define-condition-type &incomplete-reduction &error
    make-incomplete-reduction-condition incomplete-reduction?
    (notes incomplete-reduction-notes))

  (define (make-incomplete-reduction notes)
    (unless (and (list? notes) (pair? notes))
      (assertion-violation 'make-incomplete-reduction "notes must be a non-empty list" notes))
    (condition (make-incomplete-reduction-condition notes)
               (make-message-condition "the reduction is missing a writer")))

  ;; (writer path reason) ... -> ((unreadable (writer w) (path p) (reason r)) ...)
  (define (incomplete-note-clauses notes)
    (map (lambda (u)
           (list 'unreadable (list 'writer (car u))
                 (list 'path (cadr u)) (list 'reason (caddr u))))
         notes))

  ;; The refusal as an answer: (error incomplete-reduction (notes <clause> ...)).
  (define (incomplete-reduction-answer c)
    (list 'error 'incomplete-reduction
          (cons 'notes (incomplete-note-clauses (incomplete-reduction-notes c))))))
