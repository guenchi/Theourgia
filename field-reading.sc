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

;;; (theourgia field-reading) -- reading one field of a block as a query
;;; answers it.
;;;
;;; ONE READING FOR EVERY QUERY THAT ASKS WHAT A FIELD SAYS, so that a
;;; decision's status and a task's status are read by the same rule and
;;; answer in the same words: a missing field, a field in conflict, a value
;;; in the vocabulary given (as a symbol or as its string spelling), or
;;; anything else, which is unreadable and printed as it was written.
(library (theourgia field-reading)
  (export field-of field-missing? conflict-values lenient-status written-text
          decision-statuses task-statuses bookkeeping-fields rows-left-out)
  (import (rnrs))

  ;; NEVER: A MISSING FIELD IS NOT A VALUE A FIELD CAN HOLD. It is this
  ;; object, which no record can contain; a symbol here would read a field
  ;; whose value is that symbol as no field at all.
  (define missing (list 'missing))
  (define (field-missing? v) (eq? v missing))

  ;; The value of a field in a row as state-read answers it: the value, a
  ;; conflict form, or the missing object.
  (define (field-of row name)
    (let ((e (assq name (cdr (assq 'fields row))))) (if e (cdr e) missing)))

  ;; NEVER: A VALUE DOES NOT SAY WHETHER IT IS A CONFLICT. state-read renders a
  ;; field two writers left as `(conflict ((<value> <writer> <seq>) ...))`, and
  ;; a value written once can have exactly that shape; only the reducer, by
  ;; its surviving candidates, can tell (state-field-contested?, reduce.sc).
  ;; The predicate that read a conflict from a value's head is gone with that.

  ;; The values a conflict form holds, each as written: for a field the reducer
  ;; has said is contested. A form without that shape holds no values.
  (define (conflict-values v)
    (if (and (pair? (cdr v)) (list? (cadr v)))
        (map car (filter pair? (cadr v)))
        '()))

;; WHAT A NARROWED ANSWER LEFT OUT: how many rows of the whole store's answer
  ;; to the same request are not among the scoped answer's rows, a row being
  ;; known by its head and its id. The one count behind every
  ;; (scope <root> (outside <n>)) item.
  (define (rows-left-out scoped whole)
    (let ((keys (map (lambda (r) (list (car r) (cadr r))) scoped)))
      (length (filter (lambda (r) (not (member (list (car r) (cadr r)) keys))) whole))))

  ;; THE WORDS A STATUS MAY SAY, per kind of block, each spelt once here.
  (define decision-statuses '(open done dropped))
  (define task-statuses '(todo doing done dropped))

  ;; NEVER: A WRITE TO ONE OF THESE IS NOT A CHANGE OF THE BLOCK. They record
  ;; where a block stands -- its status, its batch, how it is found and
  ;; named -- and not what it says, so a task set to done after it was
  ;; linked has not drifted from the decision it implements.
  (define bookkeeping-fields '(status batch keywords class slug))

  (define (written-text v)
    (call-with-string-output-port (lambda (port) (write v port))))

  ;; -> absent | conflict | one symbol of VOCABULARY | (unreadable "<text>").
  ;; Lenient: the symbol or its string spelling are the same word. CONTESTED
  ;; is the reducer's answer for this field (state-field-contested?), which
  ;; every caller supplies: the value cannot say it.
  (define (lenient-status v vocabulary contested)
    (cond ((field-missing? v) 'absent)
          (contested 'conflict)
          ((and (symbol? v) (memq v vocabulary)) v)
          ((and (string? v) (memq (string->symbol v) vocabulary)) (string->symbol v))
          (else (list 'unreadable (if (string? v) v (written-text v)))))))
