#!r6rs
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

;; THE STRUCTURE, MAINTAINED, AGAINST THE COMPUTATION IT REPLACED (F269 C).
;;
;; reduce.sc keeps the block structure -- each parent's children, the
;; conflicts, orphans, unplaced and nested-documents sets -- and updates only
;; what a change reaches. Under the degeneracy switch (THEOURGIA_DEGENERACY=1
;; at expansion, degeneracy.sc) the old state-structure and state-outline run
;; beside it after every applied record, and the old siblings beside every
;; ord-for question; a difference raises at that record. The scenario drives
;; what the structure must follow: an import-shaped run, nesting, an orphan
;; whose parent is then materialised by a set on its unseen id, a parent
;; deleted under its children, a cycle made and broken, a cycle through a
;; tombstoned block, a kind change to doc under a parent, a position two
;; writers contest, a snapshot's rows restored, a rehearsal's clone, a
;; store's import, inserts, moves and a delete through ord-for, and rows
;; under the symbol root and under a string "root" (a block of that id),
;; which print alike and must still come out in one order.
;;
;; It runs in a child under each value of the switch: on, nothing may differ
;; (SG-1); off, the switch is off and the readings are the same (SG-2), and
;; every named check holds (SG-3). SG-4 pins the outline's total order with
;; what the base also has.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)))) e))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/structure-graph-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home' '" root "/src'"))
(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))
(for-each (lambda (k)
            (call-with-output-file (string-append root "/src/f" (number->string k) ".js")
              (lambda (p) (put-string p (format "function f~a() { return ~a }\n" k k))) 'replace))
          '(1 2 3))

;; THE SCENARIO, written as the child's program. It answers
;;   (structure-graph <switch on?> ok <records applied> <readings>)
;; or, when anything raised, (structure-graph <switch on?> raised <message> <irritants>).
(define scenario
  '((import (chezscheme) (theourgia rpc) (theourgia reduce) (theourgia degeneracy)
             (only (theourgia store) open-and-reduce))
    (define store-dir (car (command-line-arguments)))
    (define checks '())
    (define (check! name ok?) (set! checks (cons (cons name (and ok? #t)) checks)))
    (define result
      (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                          (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
        (let ((r (reduce-empty)) (n 0) (seqs (make-hashtable string-hash string=?)))
          (define (ap! r w payload)
            (let ((seq (+ 1 (hashtable-ref seqs w 0))))
              (hashtable-set! seqs w seq)
              (set! n (+ n 1))
              (reduce-apply! r w seq '() payload)))
          (define (a k) (block-id "a" k))
          (let loop ((k 1))
            (when (<= k 30)
              (ap! r "a" (list 'put (list '(kind . section) (cons 'title (number->string k)) '(parent . root) (cons 'ord k))))
              (loop (+ k 1))))
          (let loop ((k 1))
            (when (<= k 3)
              (ap! r "a" (list 'put (list '(kind . section) (cons 'title "child") (cons 'parent (a 1)) (cons 'ord k))))
              (loop (+ k 1))))
          (ap! r "b" (list 'put (list '(kind . section) '(title . "orphan") '(parent . "zz.9") '(ord . 1))))
          (let ((st (state-structure r)))
            (check! 'orphan-before-its-parent (equal? (cdr (assq 'orphans st)) (list (block-id "b" 1)))))
          (ap! r "b" '(set "zz.9" title "materialised"))
          (let ((st (state-structure r)))
            (check! 'parent-materialised (and (null? (cdr (assq 'orphans st)))
                                             (equal? (cdr (assq 'unplaced st)) '("zz.9")))))
          (ap! r "a" (list 'del (a 1)))
          (check! 'parent-deleted (equal? (cdr (assq 'orphans (state-structure r))) (list (a 31) (a 32) (a 33))))
          (ap! r "a" (list 'move (a 2) (a 3) 1))
          (ap! r "a" (list 'move (a 3) (a 2) 1))
          (check! 'cycle-made (equal? (cdr (assq 'conflicts (state-structure r))) (list (a 2) (a 3))))
          (ap! r "a" (list 'move (a 3) 'root 99))
          (check! 'cycle-broken (null? (cdr (assq 'conflicts (state-structure r)))))
          (ap! r "a" (list 'move (a 4) (a 5) 1))
          (ap! r "a" (list 'del (a 5)))
          (ap! r "a" (list 'move (a 5) (a 4) 1))
          (check! 'cycle-through-a-tombstone (equal? (cdr (assq 'conflicts (state-structure r))) (list (a 4))))
          (ap! r "a" (list 'move (a 6) (a 7) 1))
          (ap! r "a" (list 'set (a 6) 'kind 'doc))
          (check! 'kind-doc-under-a-parent (equal? (cdr (assq 'nested-documents (state-structure r))) (list (a 6))))
          (ap! r "c" (list 'move (a 8) (a 9) 1))
          (ap! r "d" (list 'move (a 8) (a 10) 1))
          (check! 'position-contested (equal? (cdr (assq 'unplaced (state-structure r))) (list (a 8) "zz.9")))
          (check! 'last-under-root (equal? (state-sibling-last-ord r 'root) 99))
          (check! 'bracket-of-a-placed-sibling (equal? (state-sibling-bracket r 'root (a 10)) '(10 . 11)))
          (check! 'bracket-of-a-marked-sibling (equal? (state-sibling-bracket r 'root (a 8)) '(0 . 0)))
          (check! 'bracket-of-no-sibling (not (state-sibling-bracket r 'root "no.1")))
          (check! 'last-of-a-childless-parent (not (state-sibling-last-ord r (a 9))))
          (check! 'siblings-listed (pair? (state-siblings r 'root)))
          (let ((restored (rows->state (state->rows r))))
            (check! 'restored-from-rows (and (equal? (state-outline restored) (state-outline r))
                                            (equal? (state-structure restored) (state-structure r)))))
          (let* ((before (state-outline r)) (clone (reduce-clone r)))
            (ap! clone "e" (list 'move (a 11) (a 12) 1))
            (check! 'clone-independent (and (equal? before (state-outline r)) (not (equal? before (state-outline clone))))))
          (let* ((head (lambda (a) (and (pair? a) (car a))))
                 (root-count (lambda ()
                               (length (filter (lambda (row) (eq? (car row) 'root))
                                               (state-outline (open-and-reduce store-dir))))))
                 (made (head (rpc-dispatch store-dir '(init) "test")))
                 (imported (head (rpc-dispatch store-dir (list 'import-code (string-append store-dir "/../src")) "test")))
                 (before (root-count))
                 (ins (lambda args
                        (let* ((answer (rpc-dispatch store-dir (append (list 'insert "--title" "t") args) "test"))
                               (ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
                          (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e)))))))
                 (x (ins)) (y (ins)) (z (and x (ins "--after" x)))
                 (moved (head (rpc-dispatch store-dir (list 'move y "root" "--after" z) "test")))
                 (deleted (head (rpc-dispatch store-dir (list 'del x) "test"))))
            (check! 'store-answers (equal? (list made imported (and x #t) (and y #t) (and z #t) moved deleted)
                                           '(ok ok #t #t #t ok ok)))
            (check! 'store-root-rows (= (root-count) (+ before 2))))
          ;; writers not used above: each one's first record is its sequence 1
          (let ((r2 (reduce-empty)))
            (ap! r2 "m" '(put ((parent . "root") (ord . 0))))
            (ap! r2 "z" '(put ((parent . root) (ord . 0))))
            (ap! r2 "n" '(put ((parent . "root") (ord . 0))))
            (ap! r2 "g" '(put ((parent . root) (ord . 0))))
            (check! 'root-and-the-string-root-ordered
                    (and (equal? (state-outline r2)
                                 (list (list 'root 0 (block-id "g" 1)) (list 'root 0 (block-id "z" 1))
                                       (list "root" 0 (block-id "m" 1)) (list "root" 0 (block-id "n" 1))))
                         (equal? (state-sibling-bracket r2 'root (block-id "z" 1)) '(0 . #f))
                         (equal? (state-sibling-bracket r2 "root" (block-id "m" 1)) '(0 . 0)))))
          (list 'ok n))))
    (write (list 'structure-graph (degeneracy-active) (car result)
                 (if (eq? (car result) 'ok) (cadr result) (cdr result))
                 (reverse checks)))))

(define (run-scenario tag on?) (run-program tag scenario on?))
(define (run-program tag program on?)
  (let ((script (string-append root "/scenario-" tag ".ss"))
        (out (string-append root "/scenario-" tag ".out"))
        (err (string-append root "/scenario-" tag ".err"))
        (dir (string-append root "/store-" tag)))
    (call-with-output-file script (lambda (p) (for-each (lambda (f) (write f p) (newline p)) program)) 'replace)
    (system (string-append (if on? "THEOURGIA_DEGENERACY=1 " "env -u THEOURGIA_DEGENERACY ")
                           "THEOURGIA_HOME='" root "/home' scheme --script '" script "' '" dir "' > '" out "' 2> '" err "' < /dev/null"))
    ;; THE ANSWER IS ON STDOUT ALONE: the product announces THEOURGIA_HOME on
    ;; stderr (ffi.sc), and an error's report goes there too
    (guard (e (#t (list 'UNREADABLE (file-text out) (file-text err))))
      (let ((x (read (open-input-string (file-text out)))))
        (if (eof-object? x) (list 'UNREADABLE (file-text out) (file-text err)) x)))))

;; THE ORDER OF ROWS UNDER TWO PARENTS THAT PRINT ALIKE, read through what
;; the base also has (reduce-apply! and state-outline, no switch library), so
;; the row is red on a tree whose sort lets the input order decide. Raw records
;; name a string "root" parent beside root's own children (the command line
;; always writes the symbol). Two rows, the string's first: a stable sort that
;; ties them keeps that order; the total order puts the symbol first. Four
;; rows: root's children as ord-for read them before, the outline's rows under
;; root sorted by ord alone.
(define order-program
  '((import (chezscheme) (theourgia reduce))
    (define (state-of . records)
      (let ((r (reduce-empty)))
        (for-each (lambda (w p) (reduce-apply! r w 1 '() p)) (map car records) (map cdr records))
        r))
    (define two (state-of '("m" . (put ((parent . "root") (ord . 0)))) '("z" . (put ((parent . root) (ord . 0))))))
    (define four (state-of '("m" . (put ((parent . "root") (ord . 0)))) '("z" . (put ((parent . root) (ord . 0))))
                           '("n" . (put ((parent . "root") (ord . 0)))) '("g" . (put ((parent . root) (ord . 0))))))
    (write (list 'structure-graph-order
                 (state-outline two)
                 (map caddr (list-sort (lambda (a b) (< (cadr a) (cadr b)))
                                       (filter (lambda (row) (eq? (car row) 'root)) (state-outline four))))))))

(define on (run-scenario "on" #t))
(define off (run-scenario "off" #f))
(want "SG-1 under the degeneracy switch the maintained structure equals the recomputed one after every record and every sibling question: no difference raised"
      (and (pair? on) (list (cadr on) (caddr on)))
      '(#t ok))
(want "SG-2 the switch unset: the same scenario answers the same readings, and the switch reads off"
      (list (and (pair? off) (cadr off)) (and (pair? off) (caddr off))
            (and (pair? on) (pair? off) (equal? (cdddr on) (cdddr off))))
      '(#f ok #t))
(want "SG-3 every reading of the scenario holds: the orphan, the materialised parent, the deleted parent, the cycle made, broken and through a tombstone, the nested document, the contested position, the sibling answers, the restored rows, the clone's independence, the store's answers and root rows"
      (and (pair? off) (eq? (caddr off) 'ok)
           (filter (lambda (c) (not (cdr c))) (car (cddddr off))))
      '())
(define order (run-program "order" order-program #f))
(want "SG-4 a string \"root\" parent beside root's children: the outline's order is total, the symbol root first, and root's children as ord-for reads them are in id order"
      order
      '(structure-graph-order ((root 0 "z.1") ("root" 0 "m.1")) ("g.1" "z.1")))

(system (string-append "rm -rf '" root "'"))
(printf "~%~a failures~%rows: ~a~%structure-graph complete~%" bad rows)
(exit (if (= bad 0) 0 1))
