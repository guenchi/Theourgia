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
;; writers contest, a snapshot's rows restored, a rehearsal's clone, and a
;; store's import, inserts, moves and a delete through ord-for.
;;
;; It runs in a child under each value of the switch: on, nothing may differ
;; (SG-1); off, the switch is off and the readings are the same (SG-2).

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
    (define readings '())
    (define (read! x) (set! readings (cons x readings)))
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
          (read! (state-structure r))
          (ap! r "b" '(set "zz.9" title "materialised"))
          (read! (state-structure r))
          (ap! r "a" (list 'del (a 1)))
          (read! (state-structure r))
          (ap! r "a" (list 'move (a 2) (a 3) 1))
          (ap! r "a" (list 'move (a 3) (a 2) 1))
          (read! (state-structure r))
          (ap! r "a" (list 'move (a 3) 'root 99))
          (read! (state-structure r))
          (ap! r "a" (list 'move (a 4) (a 5) 1))
          (ap! r "a" (list 'del (a 5)))
          (ap! r "a" (list 'move (a 5) (a 4) 1))
          (read! (state-structure r))
          (ap! r "a" (list 'move (a 6) (a 7) 1))
          (ap! r "a" (list 'set (a 6) 'kind 'doc))
          (read! (state-structure r))
          (ap! r "c" (list 'move (a 8) (a 9) 1))
          (ap! r "d" (list 'move (a 8) (a 10) 1))
          (read! (state-structure r))
          (read! (list (state-sibling-last-ord r 'root)
                       (state-sibling-bracket r 'root (a 10))
                       (state-sibling-bracket r 'root (a 8))
                       (state-sibling-bracket r 'root "no.1")
                       (state-sibling-last-ord r (a 9))))
          (let ((restored (rows->state (state->rows r))))
            (read! (equal? (state-outline restored) (state-outline r))))
          (let* ((before (state-outline r)) (clone (reduce-clone r)))
            (ap! clone "e" (list 'move (a 11) (a 12) 1))
            (read! (list (equal? before (state-outline r)) (equal? before (state-outline clone)))))
          (rpc-dispatch store-dir '(init) "test")
          (rpc-dispatch store-dir (list 'import-code (string-append store-dir "/../src")) "test")
          (let* ((ins (lambda args
                        (let* ((answer (rpc-dispatch store-dir (append (list 'insert "--title" "t") args) "test"))
                               (ev (assq 'events (cdr answer))))
                          (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e)))))))
                 (x (ins)) (y (ins)) (z (ins "--after" x)))
            (rpc-dispatch store-dir (list 'move y "root" "--after" z) "test")
            (rpc-dispatch store-dir (list 'del x) "test")
            (read! (length (filter (lambda (row) (eq? (car row) 'root)) (state-outline (open-and-reduce store-dir))))))
          (list 'ok n))))
    (write (list 'structure-graph (degeneracy-active) (car result)
                 (if (eq? (car result) 'ok) (cadr result) (cdr result))
                 (reverse readings)))))

(define (run-scenario tag on?)
  (let ((script (string-append root "/scenario-" tag ".ss"))
        (out (string-append root "/scenario-" tag ".out"))
        (dir (string-append root "/store-" tag)))
    (call-with-output-file script (lambda (p) (for-each (lambda (f) (write f p) (newline p)) scenario)) 'replace)
    (system (string-append (if on? "THEOURGIA_DEGENERACY=1 " "env -u THEOURGIA_DEGENERACY ")
                           "THEOURGIA_HOME='" root "/home' scheme --script '" script "' '" dir "' > '" out "' 2>&1 < /dev/null"))
    (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-input-string (file-text out))))))

(define on (run-scenario "on" #t))
(define off (run-scenario "off" #f))
(want "SG-1 under the degeneracy switch the maintained structure equals the recomputed one after every record and every sibling question: no difference raised"
      (and (pair? on) (list (cadr on) (caddr on)))
      '(#t ok))
(want "SG-2 the switch unset: the same scenario answers the same readings, and the switch reads off"
      (list (and (pair? off) (cadr off)) (and (pair? off) (caddr off))
            (and (pair? on) (pair? off) (equal? (cdddr on) (cdddr off))))
      '(#f ok #t))

(system (string-append "rm -rf '" root "'"))
(printf "~%~a failures~%rows: ~a~%structure-graph complete~%" bad rows)
(exit (if (= bad 0) 0 1))
