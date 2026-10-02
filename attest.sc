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

;;; (theourgia attest) -- what changed, what was said, and which end of an
;;; edge has moved since.
;;;
;;; ALL OVER ONE REDUCTION; NOTHING IS STORED. "Seen" means one thing here:
;;; in an event's causal cut. Every record a store writes names the store's
;;; whole applied cut as its premises, so in one store the causal order is
;;; the admission order and only mirrored or merged writers are concurrent.
;;; Sequence numbers of two writers are never compared; cut-join and
;;; cut-covers? are the only order.
;;;
;;; - CHANGED(x): the join of the causal cuts of the events of x's surviving
;;;   CONTENT field candidates, an absence candidate included; the empty cut
;;;   when there are none. Content is every field except the bookkeeping
;;;   ones (field-reading.sc), so a field nobody classified is content and
;;;   its change is a change. The creating put is not a term of its own: its
;;;   fields are candidates with the put as their event, and whatever later
;;;   replaced them had seen it.
;;; - SAID(a, rel, b): the join of the causal cuts of the edge's surviving
;;;   link events. It says when the edge was stated; the predicates below do
;;;   not compare against it.
;;; - THE WITNESSES of an edge for one of its ends x: each surviving link
;;;   event of the edge, and each surviving content event of the OTHER end.
;;;   One at a time, never joined: two writers who each saw a part of x do
;;;   not make one who saw the whole.
;;; - TARGET-MOVED(a, rel, b): no witness for b has all of changed(b) in its
;;;   causal cut. SOURCE-MOVED likewise for a. An end with no content has
;;;   an empty changed and has not moved.
;;; - GONE: an end that is deleted or unknown, answered apart and never as
;;;   moved or current. SELF: an edge from a block to itself, for which
;;;   neither predicate is asked.
;;;
;;; "Seen" is causal, not "read": an edit of one end made for another
;;; reason covers it. Linking again is the attestation.
(library (theourgia attest)
  (export make-attestation attestation-state changed said content-events
          target-moved? source-moved? end-gone? edge-watch)
  (import (rnrs)
          (only (theourgia reduce) state-read state-field-events state-edges
                state-event-cut cut-join cut-covers?)
          (only (theourgia field-reading) bookkeeping-fields))

  ;; ONE PER REQUEST: the state and the memos of what has been computed over
  ;; it. A memo is only ever read for the state it was filled from.
  (define-record-type (attestation new-attestation attestation?)
    (fields state event-cuts changes contents (mutable edges)))

  (define (make-attestation state)
    (new-attestation state
                     (make-hashtable equal-hash equal?)
                     (make-hashtable string-hash string=?)
                     (make-hashtable string-hash string=?)
                     #f))

  ;; The surviving link events of one edge, from a table of every edge built
  ;; once, on first use, in one pass over the links.
  (define (link-events A a rel b)
    (let ((t (or (attestation-edges A)
                 (let ((t (make-hashtable equal-hash equal?)))
                   (for-each (lambda (e) (hashtable-set! t (list (car e) (cadr e) (caddr e)) (cdddr e)))
                             (state-edges (attestation-state A)))
                   (attestation-edges-set! A t)
                   t))))
      (hashtable-ref t (list a rel b) '())))

  (define (event-cut A event)
    (let ((memo (attestation-event-cuts A)))
      (or (hashtable-ref memo event #f)
          (let ((c (or (state-event-cut (attestation-state A) event)
                       (assertion-violation 'attest "an applied event has no causal cut" event))))
            (hashtable-set! memo event c)
            c))))

  ;; The events of a block's surviving content field candidates.
  (define (content-events A id)
    (let ((memo (attestation-contents A)))
      (or (hashtable-ref memo id #f)
          (let ((es (state-field-events (attestation-state A) id bookkeeping-fields)))
            (hashtable-set! memo id es)
            es))))

  (define (join-cuts A events)
    (fold-left (lambda (acc e) (cut-join acc (event-cut A e))) '() events))

  (define (changed A id)
    (let ((memo (attestation-changes A)))
      (or (hashtable-ref memo id #f)
          (let ((c (join-cuts A (content-events A id))))
            (hashtable-set! memo id c)
            c))))

  (define (said A a rel b)
    (join-cuts A (link-events A a rel b)))

  ;; Whether no single witness has seen all of x's content: x is one end of
  ;; the edge, other the other end.
  (define (moved? A x other a rel b)
    (let ((c (changed A x)))
      (and (pair? c)
           (not (exists (lambda (w) (cut-covers? (event-cut A w) c))
                        (append (link-events A a rel b)
                                (content-events A other)))))))

  (define (target-moved? A a rel b) (moved? A b a a rel b))
  (define (source-moved? A a rel b) (moved? A a b a rel b))

  ;; Deleted, or not a block this state knows.
  (define (end-gone? A id)
    (let ((row (state-read (attestation-state A) id)))
      (or (not row) (and (cdr (assq 'deleted row)) #t))))

  ;; -> (self) | (gone <end> ...) | (current) | (moved <end> ...), the ends
  ;; named source before target.
  (define (edge-watch A a rel b)
    (cond
      ((equal? a b) '(self))
      ((or (end-gone? A a) (end-gone? A b))
       (cons 'gone (append (if (end-gone? A a) '(source) '())
                           (if (end-gone? A b) '(target) '()))))
      (else
       (let ((ends (append (if (source-moved? A a rel b) '(source) '())
                           (if (target-moved? A a rel b) '(target) '()))))
         (if (null? ends) '(current) (cons 'moved ends)))))))
