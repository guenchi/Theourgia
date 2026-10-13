#!chezscheme
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

;; position: the local log writer's last sequence, the cursor a tracked
;; request may name, read with no write. A fresh writer answers 0; after a
;; write it equals that write's cursor; a store with no local writer, or
;; whose only local writer cannot be read, refuses no-local-writer; and a
;; tracked commit placed at it commits and replays, whatever draft writer
;; the commit names.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia log) writer-directory)
        (only (theourgia reduce) block-id))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/position-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'position "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
(define (ask st . req) (rpc-dispatch st req "test"))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (head-of a) (cond ((not (pair? a)) a) ((eq? (car a) 'error) (list 'error (cadr a))) (else (car a))))
(define (new-id a) (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev))))
(define n 0)
;; -> (store . local-writer)
(define (fresh-store!)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")))
    (sh "mkdir -p " (quoted st) " " (quoted (string-append d "/home")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((init (ask st 'init)))
      (cons st (cadr (assq 'writer (cdr init)))))))
(define (cursor-text p) (string-append (car p) ":" (number->string (cdr p))))

(printf "== PO-1, PO-2: where the local writer stands ==\n")
(let* ((s (fresh-store!)) (st (car s)) (w (cdr s)))
  (want "PO-1 a fresh writer answers 0"
        (ask st 'position)
        (list 'ok (list 'position (cons w 0))))
  (let ((a (ask st 'insert "--title" "one")))
    (want "PO-2 after one write it equals the write answer's cursor"
          (ask st 'position)
          (list 'ok (list 'position (cadr (clause-of a 'cursor)))))))

(printf "== PO-3: no local writer ==\n")
(let* ((s (fresh-store!)) (st (car s)) (w (cdr s)) (owner (string-append (writer-directory st w) "/owner.sexp")))
  (ask st 'insert "--title" "one")
  (sh "mv " (quoted owner) " " (quoted (string-append owner ".aside")))
  (let ((a (ask st 'position)))
    (sh "mv " (quoted (string-append owner ".aside")) " " (quoted owner))
    (want "PO-3 a store whose writer is not this store's own (no owner file) refuses no-local-writer"
          (head-of a)
          '(error no-local-writer))))
(let* ((s (fresh-store!)) (st (car s)) (w (cdr s)) (dir (writer-directory st w)))
  (ask st 'insert "--title" "one")
  (sh "chmod 000 " (quoted dir))
  (let ((a (caught (ask st 'position))))
    (sh "chmod 700 " (quoted dir))
    (want "PO-3 an unreadable sole local writer refuses no-local-writer, with the load's (incomplete ...)"
          (list (head-of a) (and (clause-of a 'incomplete) #t))
          '((error no-local-writer) #t))))

(printf "== PO-4, PO-5: a tracked commit at that position ==\n")
(let* ((s (fresh-store!)) (st (car s)) (w (cdr s))
       (b (new-id (ask st 'insert "--title" "block")))
       (draft (ask st 'write b "new text" "--writer" "window-d"))
       (v (cadr (clause-of draft 'version)))
       (p (cadr (cadr (ask st 'position))))
       (commit (lambda () (ask st 'commit b "--writer" "window-d" "--working-version" (string-append b "=" v)
                               "--req" "po-4" "--cursor" (cursor-text p))))
       (first (commit))
       (again (commit)))
  (want "PO-5 the cursor's writer is the log writer, not the draft writer the commit names"
        (list (car p) (string=? (car p) "window-d"))
        (list w #f))
  (want "PO-4 a tracked commit with --cursor from position commits, and its replay names the event"
        (list (head-of first)
              (clause-of again 'replay)
              (let ((e (clause-of again 'event))) (and e (equal? (car (cadr e)) w))))
        '(ok (replay #t) #t)))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nposition complete\n" rows bad)
(exit (if (zero? bad) 0 1))
