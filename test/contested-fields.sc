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

;; A STORED VALUE THAT LOOKS LIKE A CONFLICT IS NOT ONE, and the reducer is the
;; one place that knows. state-read renders a field whose candidates two
;; writers left as `(conflict ((<value> <writer> <seq>) ...))`; a value
;; written once can have exactly that shape (a record from elsewhere, which no
;; caller check sees). Every reader that says "conflict" asks the reducer how
;; many candidates survive, and a value written once is read as the value it
;; is: unreadable where it is not a word of the field's vocabulary.
;;
;; Each row writes the shape ONCE and asks one reader; its control has two
;; writers set the field at once, which every reader still calls a conflict.
;; Reductions are built from events, as the reducer applies records.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) reduce-empty reduce-apply!)
        (only (theourgia commitments) commitments-answer)
        (only (theourgia tasks) tasks-answer)
        (only (theourgia name-use) block-name-use)
        (only (theourgia store) search-state)
        (only (theourgia query) make-query-session session-answer))

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
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

(register-verbs! extension-verbs)

(define (state-of . events)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e)) events)
    r))
;; a.1, put by writer a with FIELDS; then, for the control, writers b and c
;; each set FIELD to one of two values at once.
(define (written-once fields) (state-of `("a" 1 () (put ,fields))))
(define (two-writers fields field v1 v2)
  (state-of `("a" 1 () (put ,fields))
            `("b" 1 (("a" . 1)) (set "a.1" ,field ,v1))
            `("c" 1 (("a" . 1)) (set "a.1" ,field ,v2))))
(define (find-item items head id)
  (find (lambda (x) (and (pair? x) (eq? (car x) head) (pair? (cdr x)) (equal? (cadr x) id))) items))
(define (items-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (caadr a) 'items)) (cdadr a) '()))
(define (status-of item) (and item (let ((s (assq 'status (cddr item)))) (and s (cdr s)))))
(define (contains? x needle) (or (equal? x needle) (and (pair? x) (or (contains? (car x) needle) (contains? (cdr x) needle)))))

;; THE SHAPES, each the reducer's own: a list of (value writer seq), two or more.
(define (shaped v1 v2) (list 'conflict (list (list v1 "w1" 1) (list v2 "w2" 1))))

;; ---- the class: the lifecycle's effective class, as the query reads it ---------------------------------

(define (class-of s) (let ((a (session-answer (make-query-session s) '(class "a.1" ?c)))) (if (eq? (car a) 'ok) (cadr a) a)))
(want "class written once with the conflict's shape is unreadable; two writers make conflict"
      (in-order (class-of (written-once `((kind . section) (title . "s") (class . ,(shaped 'ruling 'observation)))))
                (class-of (two-writers '((kind . section) (title . "s")) 'class 'ruling 'observation)))
      '(((unreadable)) ((conflict))))

;; ---- commitments: a decision's status, and a kind -------------------------------------------------------

(define (commitments s) (items-of (commitments-answer s 'all #f #f "root")))
(want "commitments: a decision's status written once with the conflict's shape is unreadable; two writers make conflict"
      (in-order (status-of (find-item (commitments (written-once `((kind . decision) (title . "d") (status . ,(shaped 'done 'open)))))
                                      'decision "a.1"))
                (status-of (find-item (commitments (two-writers '((kind . decision) (title . "d")) 'status 'done 'open))
                                      'decision "a.1")))
      (list (list 'unreadable (call-with-string-output-port (lambda (p) (write (shaped 'done 'open) p))))
            '(conflict)))
(want "commitments: a kind written once with the conflict's shape, a decision among its values, is no decision and no skipped row; two writers skip it as unreadable"
      (in-order (let ((items (commitments (written-once `((kind . ,(shaped 'decision 'section)) (title . "k"))))))
                  (list (and (find-item items 'decision "a.1") #t) (and (find-item items 'skipped "a.1") #t)))
                (find-item (commitments (two-writers '((kind . section) (title . "k")) 'kind 'decision 'section))
                           'skipped "a.1"))
      '((#f #f) (skipped "a.1" unreadable-block)))

;; ---- tasks: a task's status --------------------------------------------------------------------------------

(define (tasks s) (items-of (tasks-answer s #f #f "root")))
(want "tasks: a task's status written once with the conflict's shape is unreadable; two writers make conflict"
      (in-order (status-of (find-item (tasks (written-once `((kind . task) (title . "t") (status . ,(shaped 'done 'todo)))))
                                      'task "a.1"))
                (status-of (find-item (tasks (two-writers '((kind . task) (title . "t")) 'status 'done 'todo))
                                      'task "a.1")))
      (list (list 'unreadable (call-with-string-output-port (lambda (p) (write (shaped 'done 'todo) p))))
            '(conflict)))

;; ---- name use: a datum body ----------------------------------------------------------------------------------

(define (names-of a) (let ((n (and (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'names))) a)))) (and n (cdr n))))
(want "name use: a datum body written once with the conflict's shape is read as code, its names found; two writers leave it unreadable"
      (let ((once (block-name-use (written-once `((kind . code) (mode . datum) (title . "c") (body . ,(shaped '(f) '(g))))) "a.1")))
        (in-order (contains? once 'unreadable-code)
                  (let ((ns (names-of once))) (and ns (list (and (memq 'f ns) #t) (and (memq 'g ns) #t))))
                  (contains? (block-name-use (two-writers '((kind . code) (mode . datum) (title . "c")) 'body '(f) '(g)) "a.1")
                             'unreadable-code)))
      '(#f (#t #t) #t))

;; ---- the store's text: search and the query's references --------------------------------------------------

;; a.1 is a section; a.2's src is the shape written once, its "candidates" a
;; reference to a.1 beside the word zibbet. Neither is a text of a.2.
(define (src-state src-events)
  (apply state-of `("a" 1 () (put ((kind . section) (title . "target")))) src-events))
(define once-src (src-state (list `("a" 2 () (put ((kind . section) (title . "holder") (src . ,(shaped "see [[a.1]] zibbet" "other"))))))))
(define twice-src (src-state (list `("a" 2 () (put ((kind . section) (title . "holder"))))
                                   '("b" 1 (("a" . 2)) (set "a.2" src "see [[a.1]] zibbet"))
                                   '("c" 1 (("a" . 2)) (set "a.2" src "other")))))
(define (search-ids s word) (map car (cdr (assq 'items (search-state s word #f)))))
(define (refs-from s) (let ((a (session-answer (make-query-session s) '(ref "a.2" ?to)))) (if (eq? (car a) 'ok) (cadr a) a)))
(want "search and the query's ref do not read the candidates of a src written once with the conflict's shape; two writers' are read"
      (in-order (search-ids once-src "zibbet") (refs-from once-src) (search-ids twice-src "zibbet") (refs-from twice-src))
      '(() () ("a.2") (("a.1"))))

;; ---- the readers that cannot tell, and say why ----------------------------------------------------------------
;;
;; NOTE: TWO READERS GIVE THE SAME ANSWER EITHER WAY, and have no row here. A
;; task's status decides whether its implements edge discharges, and a
;; decision's status whether it is closed: both ask only "is it done" (or
;; dropped), and a contested status and an unreadable one both answer no.
;; They take the contest as an argument like every other reader, so a later
;; reading that does tell them apart has it.

(printf "rows: ~a\n~a failures\ncontested-fields complete\n" rows bad)
(exit (if (= bad 0) 0 1))
