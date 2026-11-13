;; Copyright 2018 - 2026 guenchi
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

;; WHO HOLDS A PATH, AND A BLOCK'S LINKS, BY INDEX (F437 D6-1, D6-2, D6-3).
;;
;;   PH-1  the structure's holder table answers what a walk over every block
;;         answers, for every path a block names, after each kind of change:
;;         a path set, changed, contested by two writers, made absent; a block
;;         deleted and written after; a datum library beside a text file of
;;         one path; a snapshot's rows restored; a clone changed
;;   PH-2  asked with #f for kind, mode or path, the question refuses
;;   PH-3  duplicated paths are the table's keys of the two judged kinds with
;;         two or more holders, sorted by path and then kind
;;   LI-1  a block's edges, the blocks that point at it, and a link's events,
;;         read by id, equal what the whole link list gives, after links,
;;         unlinks, a snapshot's rows and a clone
;;
;; The walk and the whole list are written here, not taken from the library,
;; so the rows judge the library against a second reading.
(import (chezscheme) (theourgia reduce))
;; state-field-contested? is the library's own answer to "several candidates":
;; a single value shaped (conflict ...) is a value, not a contest

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

;; ---- applying records ------------------------------------------------------
(define seqs (make-hashtable string-hash string=?))
(define (ap! r w payload . deps)
  (let ((seq (+ 1 (hashtable-ref seqs w 0))))
    (hashtable-set! seqs w seq)
    (reduce-apply! r w seq (if (pair? deps) (car deps) '()) payload)
    (block-id w seq)))
(define (file! r w path . kind-mode)
  (let ((kind (if (pair? kind-mode) (car kind-mode) 'file))
        (mode (if (pair? kind-mode) (cadr kind-mode) 'text)))
    (ap! r w (list 'put (list (cons 'kind kind) (cons 'mode mode) (cons 'path path)
                              '(parent . root) (cons 'ord (hashtable-ref seqs w 0)))))))

;; ---- the walk: every block read, a holder alive with the three fields equal
(define (walk-holders r kind mode rel)
  (list-sort string<?
    (filter (lambda (id)
              (let ((b (state-read r id)))
                (and b (not (cdr (assq 'deleted b)))
                     (let ((fs (cdr (assq 'fields b))))
                       (define (field k) (let ((p (assq k fs))) (and p (cdr p))))
                       (and (equal? (field 'kind) kind) (equal? (field 'mode) mode)
                            (equal? (field 'path) rel))))))
            (state-block-ids-all r))))
;; every block id, deleted ones too: the ids state->rows names
(define (state-block-ids-all r)
  (let loop ((rows (state->rows r)) (out '()))
    (cond ((null? rows) (reverse out))
          ((and (pair? (car rows)) (eq? 'block (car (car rows))) (pair? (cdr (car rows))) (string? (cadr (car rows))))
           (loop (cdr rows) (cons (cadr (car rows)) out)))
          (else (loop (cdr rows) out)))))
;; -> for every key some block names (each field a plain value), the walk's
;; holders, and the library's answer to the same question
(define (holders-by-both r)
  (let ((keys '()))
    (for-each (lambda (id)
                (let* ((b (state-read r id))
                       (fs (cdr (assq 'fields b)))
                       (plain (lambda (k) (let ((p (assq k fs)))
                                            (and p (not (state-field-contested? r id k (cdr p))) (list (cdr p))))))
                       (kind (plain 'kind)) (mode (plain 'mode)) (path (plain 'path)))
                  (when (and kind mode path)
                    (let ((key (list (car kind) (car mode) (car path))))
                      (unless (member key keys) (set! keys (cons key keys)))))))
              (state-block-ids-all r))
    (map (lambda (key)
           (list key (walk-holders r (car key) (cadr key) (caddr key))
                 (state-path-claimants r (car key) (cadr key) (caddr key))))
         (reverse keys))))
;; -> #t when the library answers every key as the walk does; else the keys
;; that differ, each with both answers
(define (table-agrees? r)
  (let ((differ (filter (lambda (row) (not (equal? (cadr row) (caddr row)))) (holders-by-both r))))
    (or (null? differ) differ)))

(printf "== PH-1: the holder table against the walk ==\n")
(define r (reduce-empty))
(define x1 (file! r "a" "x.py"))
(define x2 (file! r "a" "x.py"))
(want "PH-1 two text files at one path: both hold it, the table as the walk"
      (list (state-path-claimants r 'file 'text "x.py") (table-agrees? r))
      (list (list-sort string<? (list x1 x2)) #t))
(ap! r "a" (list 'set x1 'path "y.py"))
(want "PH-1 a path changed: the block leaves the old path and holds the new one"
      (list (state-path-claimants r 'file 'text "x.py") (state-path-claimants r 'file 'text "y.py") (table-agrees? r))
      (list (list x2) (list x1) #t))
(ap! r "b" (list 'set x2 'path "z.py"))
(want "PH-1 a path contested by two writers that did not see each other: the block holds neither, as the walk says"
      (list (state-path-claimants r 'file 'text "x.py") (state-path-claimants r 'file 'text "z.py") (table-agrees? r))
      (list '() '() #t))
(ap! r "a" (list 'set x1 'path))
(want "PH-1 a path made absent: the block holds nothing"
      (list (state-path-claimants r 'file 'text "y.py") (table-agrees? r))
      (list '() #t))
(define x3 (file! r "a" "w.py"))
(ap! r "a" (list 'del x3))
(define held-after-delete (state-path-claimants r 'file 'text "w.py"))
(ap! r "a" (list 'set x3 'path "v.py"))
(want "PH-1 a block deleted holds nothing, and a write to it after does not put it back"
      (list held-after-delete (state-path-claimants r 'file 'text "v.py") (table-agrees? r))
      (list '() '() #t))
(define l1 (file! r "a" "p.sc" 'library 'datum))
(define t1 (file! r "a" "p.sc"))
(want "PH-1 a datum library and a text file of one path are two questions"
      (list (state-path-claimants r 'library 'datum "p.sc") (state-path-claimants r 'file 'text "p.sc") (table-agrees? r))
      (list (list l1) (list t1) #t))
(define restored (rows->state (state->rows r)))
(want "PH-1 a snapshot's rows restored: the same holders, the table as the walk"
      (list (equal? (holders-by-both restored) (holders-by-both r)) (table-agrees? restored))
      '(#t #t))
(define clone (reduce-clone r))
(define before (holders-by-both r))
;; writer a, whose own put of t1 the set sees: a path set by a writer that had
;; not seen the block's put would be contested, and held by nobody
(ap! clone "a" (list 'set t1 'path "q.sc"))
(want "PH-1 a clone changed: the clone's table moves, the original's does not"
      (list (equal? before (holders-by-both r)) (state-path-claimants clone 'file 'text "q.sc")
            (state-path-claimants r 'file 'text "q.sc") (table-agrees? clone))
      (list #t (list t1) '() #t))

;; the stopgap read's cases: kind and mode changed, a key emptied and filled
;; again, and a single value shaped like a contest. Written by writer k: the
;; clone row above took writer a's next sequence number in the clone, so a
;; record of a's here would wait in the original for the one it never saw
(define k1 (file! r "k" "k.py"))
(ap! r "k" (list 'set k1 'kind 'library))
(define after-kind (list (state-path-claimants r 'file 'text "k.py") (state-path-claimants r 'library 'text "k.py")))
(ap! r "k" (list 'set k1 'mode 'datum))
(want "PH-1 kind and then mode changed: the block moves from key to key, as the walk says"
      (list after-kind (state-path-claimants r 'library 'datum "k.py") (table-agrees? r))
      (list (list '() (list k1)) (list k1) #t))
(ap! r "k" (list 'set k1 'path "e.py"))
(define emptied (state-path-claimants r 'library 'datum "k.py"))
(ap! r "k" (list 'set k1 'path "k.py"))
(want "PH-1 a key emptied and filled again: no holder while empty, the block again after"
      (list emptied (state-path-claimants r 'library 'datum "k.py") (table-agrees? r))
      (list '() (list k1) #t))
(define c1 (file! r "k" '(conflict ("x"))))
(want "PH-1 a single path value shaped like a contest is a value: the block holds it, as the walk says"
      (list (state-path-claimants r 'file 'text '(conflict ("x"))) (table-agrees? r))
      (list (list c1) #t))

(printf "== PH-2: #f is refused ==\n")
(want "PH-2 a #f kind, mode or path refuses by name"
      (map (lambda (args) (caught (apply state-path-claimants r args)))
           '((#f text "x.py") (file #f "x.py") (file text #f)))
      '((RAISED "kind is #f") (RAISED "mode is #f") (RAISED "path is #f")))

(printf "== PH-3: duplicated paths ==\n")
(define d (reduce-empty))
(define m1 (file! d "m" "b.sc" 'library 'datum))
(define m2 (file! d "m" "b.sc" 'library 'datum))
(define m3 (file! d "m" "b.sc"))
(define m4 (file! d "m" "b.sc"))
(define m5 (file! d "m" "a.py"))
(define m6 (file! d "m" "a.py"))
(define m7 (file! d "m" "c.md" 'doc 'text))
(define m8 (file! d "m" "c.md" 'doc 'text))
(want "PH-3 duplicated paths: the two judged kinds only, sorted by path and then kind (file before library)"
      (state-duplicated-paths d)
      (list (list 'duplicate-path "a.py" (list 'ids (list-sort string<? (list m5 m6))))
            (list 'duplicate-path "b.sc" (list 'ids (list-sort string<? (list m3 m4))))
            (list 'duplicate-path "b.sc" (list 'ids (list-sort string<? (list m1 m2))))))

(printf "== LI-1: links by id against the whole list ==\n")
;; -> per block: its edges, its referrers, and each of its edges' events, by
;; the library's index and by the whole list (state-edges)
(define (links-by-both r)
  (let ((edges (state-edges r)))
    (map (lambda (id)
           (list id
                 (cdr (assq 'edges (state-read r id)))
                 (list-sort (lambda (x y) (if (eq? (car x) (car y)) (string<? (cdr x) (cdr y))
                                              (string<? (symbol->string (car x)) (symbol->string (car y)))))
                            (map (lambda (e) (cons (cadr e) (caddr e))) (filter (lambda (e) (equal? (car e) id)) edges)))
                 (state-refs r id)
                 (list-sort (lambda (x y) (if (string=? (car x) (car y))
                                              (string<? (symbol->string (cdr x)) (symbol->string (cdr y)))
                                              (string<? (car x) (car y))))
                            (map (lambda (e) (cons (car e) (cadr e))) (filter (lambda (e) (equal? (caddr e) id)) edges)))
                 (map (lambda (e) (state-link-events r (car e) (cadr e) (caddr e))) (filter (lambda (e) (equal? (car e) id)) edges))
                 (map cdddr (filter (lambda (e) (equal? (car e) id)) edges))))
         (state-block-ids-all r))))
(define (links-agree? r)
  (let ((differ (filter (lambda (row) (not (and (equal? (list-ref row 1) (list-ref row 2))
                                                (equal? (list-ref row 3) (list-ref row 4))
                                                (equal? (list-ref row 5) (list-ref row 6)))))
                        (links-by-both r))))
    (or (null? differ) differ)))
(define g (reduce-empty))
(define g1 (file! g "g" "g1.py"))
(define g2 (file! g "g" "g2.py"))
(define g3 (file! g "g" "g3.py"))
(ap! g "g" (list 'link g1 'relates g2))
(ap! g "g" (list 'link g1 'relates g3))
(ap! g "g" (list 'link g2 'relates g3))
(ap! g "h" (list 'link g1 'relates g2))
(want "LI-1 links made, one twice by two writers: by id as by the whole list"
      (list (links-agree? g) (cdr (assq 'edges (state-read g g1))) (length (state-link-events g g1 'relates g2)))
      (list #t (list-sort (lambda (x y) (string<? (cdr x) (cdr y))) (list (cons 'relates g2) (cons 'relates g3))) 2))
(ap! g "g" (list 'unlink g1 'relates g3))
(ap! g "g" (list 'unlink g2 'relates g1))
(want "LI-1 an unlink of a link, and of one that does not exist: by id as by the whole list"
      (list (links-agree? g) (cdr (assq 'edges (state-read g g1))) (state-refs g g3))
      (list #t (list (cons 'relates g2)) (list (cons g2 'relates))))
;; an unlink by a writer that had not seen the link leaves it (its past does
;; not cover the link's event)
(ap! g "u" (list 'unlink g1 'relates g2))
(want "LI-1 an unlink that did not see the link leaves it, by id as by the whole list"
      (list (links-agree? g) (cdr (assq 'edges (state-read g g1))))
      (list #t (list (cons 'relates g2))))
(define g-restored (rows->state (state->rows g)))
(define g-clone (reduce-clone g))
;; a writer of its own: the sequence numbers are counted per writer across
;; every reduction here, and a writer seen elsewhere would leave a gap
(ap! g-clone "z" (list 'link g3 'relates g1))
(want "LI-1 a snapshot's rows restored, and a clone given a link the original has not: each by id as by its whole list"
      (list (links-agree? g-restored) (equal? (links-by-both g-restored) (links-by-both g))
            (links-agree? g-clone) (state-refs g g1) (state-refs g-clone g1))
      (list #t #t #t '() (list (cons g3 'relates))))

(printf "\n~a failures\nrows: ~a\npath-holders complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
