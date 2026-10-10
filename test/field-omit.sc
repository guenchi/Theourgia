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

;; A FIELD WHOSE VALUE IS THE SYMBOL `omit` IS A FIELD. The reducer marks a
;; removed field with its own absence marker; the reader once turned that
;; marker into the symbol `omit` and then dropped every field equal to
;; `omit`, so a field SET to the symbol omit read as absent. Absence is
;; decided by the candidate now, and these rows read it on every reader of a
;; block's fields: state-read, the query's field fact, and state-datum -- with
;; a true absence beside it, which must still read absent.
(import (chezscheme)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read state-datum)
        (only (theourgia query) make-query-session session-answer))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

;; One block: `note` set to the symbol omit; `gone` written and then removed
;; (a set with no value: the absence); `kept` an ordinary value beside them.
(define W "aaaa0000")
(define B "aaaa0000.1")
(define r (reduce-empty))
(for-each (lambda (rec) (apply reduce-apply! r W rec))
          (list (list 1 '() '(put ((kind . section) (title . "T") (parent . root) (ord . 1))))
                (list 2 '() (list 'set B 'note 'omit))
                (list 3 '() (list 'set B 'gone "x"))
                (list 4 '() (list 'set B 'gone))
                (list 5 '() (list 'set B 'kept "k"))))

(define (fields-of row) (let ((f (and row (assq 'fields row)))) (if f (cdr f) '())))

(want "FO-1 state-read: a field set to the symbol omit reads back as omit"
      (assq 'note (fields-of (state-read r B)))
      '(note . omit))
(want "FO-1 TWIN: a removed field is not there, and an ordinary one is"
      (list (assq 'gone (fields-of (state-read r B))) (assq 'kept (fields-of (state-read r B))))
      '(#f (kept . "k")))
;; The field fact names a field by its name as a string (query.sc).
(want "FO-2 the query's field fact names note with the value omit, and no gone"
      (let* ((a (caught (session-answer (make-query-session r) (list 'field B '?n '?v))))
             (items (if (and (pair? a) (pair? (cdr a))) (cadr a) a)))
        (list (and (list? items) (exists (lambda (i) (and (pair? i) (member "note" i) (member 'omit i) #t)) items))
              (and (list? items) (exists (lambda (i) (and (pair? i) (member "gone" i) #t)) items))))
      '(#t #f))
(want "FO-3 state-datum keeps the field set to omit as the value omit, and the removed field as its absence candidate"
      (let* ((blk (find (lambda (b) (and (pair? b) (equal? (cadr b) B))) (state-datum r)))
             (fields (and blk (cadr (assq 'fields (cddr blk)))))
             (of (lambda (k) (and fields (let ((e (assq k fields))) (and e (format "~s" (cadr e))))))))
        (list (of 'note) (of 'gone)))
      (list (format "~s" (list (cons 'omit (cons W 2))))
            (format "~s" (list (cons (list "#%absent") (cons W 4))))))

(printf "rows: ~a\n~a failures\nfield-omit complete\n" rows bad)
(exit (if (= bad 0) 0 1))
