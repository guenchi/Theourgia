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

;;; (theourgia answers) -- how a filesystem failure becomes an answer.
;;;
;;; F100 D1' (the answers), delivered by F100b. ffi.sc classifies a failure
;;; at its source: a non-mutating step raises unreadable-entry, a mutating
;;; one raises durable-error, and every point that answers a request keeps
;;; a mutation record of what it changed. This library is the one table
;;; that turns such a condition and such a record into an answer, and the
;;; one rule that adds a later record to an answer already made. It does no
;;; filesystem work of its own.
;;;
;;; THE TABLE (classify-failure condition record):
;;;   unreadable-entry, errno ENOENT or ENOTDIR, empty record
;;;     -> (error absent (path p) (reason r) (errno e))
;;;   unreadable-entry, any other errno, empty record
;;;     -> (error unreadable (path p) (reason r) (errno e))
;;;   durable-error, empty record
;;;     -> (error unwritable (op o) (path p) (reason r) (errno n))
;;;   either, non-empty record
;;;     -> (error incomplete (failed <the clauses above>) (written <record>))
;;;   anything else -> #f (the caller keeps its own answer for it)
;;; An unreadable-entry's clauses carry no `op` (its reason is strerror
;;; text and names no verb; ruling D2). THE errno CLAUSE CARRIES WHAT THE
;;; CONDITION CARRIES: a durable-error's is a NUMBER (13); an
;;; unreadable-entry's is a NAME for the ten codes ffi's errno-reason knows
;;; (EACCES, EIO, ELOOP, EOVERFLOW, ENAMETOOLONG, EMFILE, EPERM, ENOENT,
;;; ENOTDIR, EISDIR), the NUMBER for any other (EROFS reads 30), and #f
;;; from one site only, ffi's chez-unreadable! when Chez's condition has no
;;; class (its one caller is close-unwritten-port!; no row can make a Chez
;;; port close fail). F106 unifies the forms. (written ()) never appears:
;;; the first three kinds carry no written clause at all.
;;;
;;; THE AGGREGATION RULE (combine-report answer entries): a record made
;;; later (a process's own entries, a boot's) is added to an answer made
;;; earlier. Empty entries leave the answer as it was, byte for byte. A
;;; table answer of kind absent, unreadable or unwritable becomes
;;; incomplete, its clauses moving under `failed`; an incomplete answer
;;; appends the entries to its `written`; any other refusal -- a NAMED
;;; outcome -- gains them through with-written and keeps its name; a
;;; non-refusal is returned as it was (a success reports what it did in its
;;; own terms).

(library (theourgia answers)
  (export classify-failure combine-report with-written)
  (import (chezscheme)
          (only (theourgia ffi)
                unreadable-entry? unreadable-entry-path unreadable-entry-reason
                unreadable-entry-errno
                fs-error? fs-error-op fs-error-target fs-error-errno errno-text))

  (define absence-errnos '(ENOENT ENOTDIR))

  ;; The clauses after the kind, in the table's order.
  (define (failure-clauses c)
    (cond
      ((unreadable-entry? c)
       (list (list 'path (unreadable-entry-path c))
             (list 'reason (unreadable-entry-reason c))
             (list 'errno (unreadable-entry-errno c))))
      ((fs-error? c)
       (list (list 'op (fs-error-op c))
             (list 'path (fs-error-target c))
             (list 'reason (reason-of (fs-error-op c) (fs-error-errno c)))
             (list 'errno (fs-error-errno c))))
      (else #f)))

;; A durable-error's reason: strerror's text for its errno. With no errno,
  ;; a write's failure is a short write (write-one!, write-all!); any other
  ;; step's is only "no errno" (ffi's errno-text), since nothing says more.
  (define (reason-of op n)
    (cond (n (errno-text n))
          ((eq? op 'write) "short write")
          (else (errno-text n))))

  (define (failure-kind c)
    (cond
      ((unreadable-entry? c)
       (if (memq (unreadable-entry-errno c) absence-errnos) 'absent 'unreadable))
      ((fs-error? c) 'unwritable)
      (else #f)))

  (define (classify-failure c record)
    (let ((clauses (failure-clauses c)))
      (cond
        ((not clauses) #f)
        ((null? record) (cons* 'error (failure-kind c) clauses))
        (else (list 'error 'incomplete (cons 'failed clauses) (list 'written record))))))

  (define table-kinds '(absent unreadable unwritable))

  (define (refusal? answer)
    (and (pair? answer) (eq? (car answer) 'error) (pair? (cdr answer))))

  ;; An answer's `written` clause, or #f. NOT assq: a named outcome may
  ;; carry atoms after its kind -- store.sc answers `(error not-written
  ;; reserved-not-written (sequence n))` -- and assq over a list holding a
  ;; symbol raises instead of answering (F100b M1 review r1, F1).
  (define (written-clause answer)
    (find (lambda (c) (and (pair? c) (eq? (car c) 'written))) (cddr answer)))

  ;; A NAMED OUTCOME keeps its name and gains the entries: one written
  ;; clause, holding what it already held and then the new entries.
  (define (with-written answer entries)
    (cond
      ((null? entries) answer)
      ((not (refusal? answer)) answer)
      ((written-clause answer)
       => (lambda (w)
            (cons* (car answer) (cadr answer)
                   (map (lambda (clause)
                          (if (eq? clause w)
                              (list 'written (append (cadr w) entries))
                              clause))
                        (cddr answer)))))
      (else (append answer (list (list 'written entries))))))

  (define (combine-report answer entries)
    (cond
      ((null? entries) answer)
      ((not (refusal? answer)) answer)
      ((memq (cadr answer) table-kinds)
       (list 'error 'incomplete (cons 'failed (cddr answer)) (list 'written entries)))
      (else (with-written answer entries)))))
