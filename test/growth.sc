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

;; THE VERBS A CLIENT MEETS GROW WITH THE STORE, NOT WITH ITS SQUARE (F437 D6-6).
;;
;; A per-block accessor that is itself a scan cannot be found by reading the
;; code for it, so this reads behaviour: two generated stores, one twice the
;; other (files and blocks), and on each the verbs a client meets.
;;
;;   GR-0  the stores: files and blocks, the second twice the first
;;   GR-1  each verb's user CPU on the larger store, its cost on an empty store
;;         taken off, is under 2.6 times the same on the smaller: a linear verb
;;         reads about 2, one that asks a whole-store question per file about 4.
;;         The arms are interleaved and each is read by its smallest of three,
;;         so a load that moves meanwhile falls on both. Wall time is not used.
;;         read's block id is found before the timing (it was once found inside
;;         it, by an outline, and read measured outline). UNDER THE
;;         DEGENERACY SWITCH export-code, changed-files and outline are printed
;;         and not judged: their checks (path-holders, children-list,
;;         changed-files-reference) cost what the code they replaced cost,
;;         which is the point of them; GR-2 and GR-3 count the work instead.
;;   GR-2  where the work is counted: the path questions an export asks, each
;;         counted by the holders it examines (pending-work-counter's `path`),
;;         under 2.2 times on the larger store.
;;
;;   GR-3  where the work is counted: the children an export and a
;;         changed-files read (pending-work-counter's `children`, one per child
;;         listed), under 2.2 times on the larger store.
;;
;; The commit and its verdict are not here (their own brief); `read` stays
;; linear by design.
(import (chezscheme) (theourgia rpc) (only (theourgia reduce) pending-work-counter)
        (only (theourgia degeneracy) degeneracy-active))

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

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/growth-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; each file imports as its block and one child (the split keeps a file's
;; definitions together here), so 1000 files are about 2000 blocks
(define files-n 1000)

;; -> a store under ROOT/NAME holding FILES generated Scheme files, imported,
;; tagged t0, one block's text changed by a set, tagged t1 (a second import of
;; the directory would add blocks, not update them)
(define (store! name files)
  (let ((src (string-append root "/" name "-src")) (st (string-append root "/" name)))
    (system (string-append "mkdir -p '" src "'"))
    (let loop ((k 0))
      (when (< k files)
        (call-with-output-file (string-append src "/f" (number->string k) ".scm")
          (lambda (p) (fprintf p "(define (f~a x) (+ x ~a))\n" k k))
          'replace)
        (loop (+ k 1))))
    (rpc-dispatch st '(init) "test")
    (when (> files 0)
      (rpc-dispatch st (list 'import-code src) "test")
      (rpc-dispatch st '(tag "t0") "test")
      (let ((id (some-code-id st)))
        (when id (rpc-dispatch st (list 'set id "src" "(define (f0 x) (- x 1))\n") "test"))))
    (rpc-dispatch st '(tag "t1") "test")
    st))
;; -> the id of the first code block the outline lists, or #f
(define (some-code-id st)
  (let ((o (rpc-dispatch st '(outline) "test")))
    (and (pair? o) (eq? 'ok (car o))
         (let ((row (find (lambda (r) (and (pair? r) (eq? 'row (car r)) (pair? (cddr r)) (eq? 'code (caddr r))))
                          (let ((rows (assq 'rows (cdr o)))) (if rows (cdr rows) '())))))
           (and row (cadr row))))))
(define (block-count st)
  (let ((o (rpc-dispatch st '(check) "test")))
    (if (and (pair? o) (eq? 'ok (car o)))
        (let ((b (assq 'blocks (cdr o)))) (and b (cadr b)))
        o)))

(define empty (store! "empty" 0))
(define small (store! "small" files-n))
(define large (store! "large" (* 2 files-n)))
(printf "GR-0 blocks: small ~s large ~s\n" (block-count small) (block-count large))
(want "GR-0 the larger store has twice the files of the smaller, and every import answered"
      (list (length (directory-list (string-append root "/small-src"))) (length (directory-list (string-append root "/large-src"))))
      (list files-n (* 2 files-n)))

;; ---- the verbs, timed by user CPU ------------------------------------------
(define (out-dir) (string-append root "/out-" (number->string (random 1000000000))))
(define verbs
  (list (cons 'check (lambda (st) (rpc-dispatch st '(check) "test")))
        (cons 'conflicts (lambda (st) (rpc-dispatch st '(conflicts) "test")))
        (cons 'export-code (lambda (st) (rpc-dispatch st (list 'export-code (out-dir)) "test")))
        (cons 'changed-files (lambda (st) (rpc-dispatch st '(changed-files "t0" "t1") "test")))
        (cons 'outline (lambda (st) (rpc-dispatch st '(outline) "test")))
        (cons 'read (let ((ids (map (lambda (st) (cons st (or (some-code-id st) "no-such-block")))
                                    (list empty small large))))
                      (lambda (st) (rpc-dispatch st (list 'read (cdr (assoc st ids))) "test"))))))
(define (cpu-of thunk)
  (collect)
  (let ((t0 (cpu-time))) (thunk) (- (cpu-time) t0)))
;; -> (empty small large): each arm's smallest of three, interleaved
(define (readings run)
  (let loop ((k 0) (e '()) (s '()) (l '()))
    (if (= k 3)
        (list (apply min e) (apply min s) (apply min l))
        (let* ((ee (cpu-of (lambda () (run empty))))
               (ss (cpu-of (lambda () (run small))))
               (ll (cpu-of (lambda () (run large)))))
          (loop (+ k 1) (cons ee e) (cons ss s) (cons ll l))))))
;; the verbs whose checks cost what the code they replaced cost: under the
;; switch their row reads not-judged, so the fixture runs the same rows either
;; way
(define checked-verbs '(export-code changed-files outline))
(define switched? (degeneracy-active))
(for-each
  (lambda (v)
    (let* ((r (readings (cdr v)))
           (small-ms (max 1 (- (cadr r) (car r))))
           (large-ms (max 1 (- (caddr r) (car r))))
           (ratio (/ (inexact large-ms) small-ms)))
      (define judged? (not (and switched? (memq (car v) checked-verbs))))
      (printf "~a ~a: cpu ms empty ~a small ~a large ~a, ratio ~,2f\n"
              (if judged? "GR-1" "GR-1 reading (not judged)") (car v) (car r) (cadr r) (caddr r) ratio)
      (want (format "GR-1 ~a on twice the store costs under 2.6 times, its empty-store cost taken off (under the switch, for export-code, changed-files and outline: not judged)" (car v))
            (if judged? (< ratio 2.6) 'not-judged)
            (if judged? #t 'not-judged))))
  verbs)

;; ---- the path questions, counted ---------------------------------------------
(define (path-work st)
  (let ((n 0))
    (parameterize ((pending-work-counter (lambda (kind k) (when (eq? kind 'path) (set! n (+ n k))))))
      (rpc-dispatch st (list 'export-code (out-dir)) "test"))
    n))
(let ((s (path-work small)) (l (path-work large)))
  (printf "GR-2 path work of an export: small ~a large ~a\n" s l)
  (want "GR-2 an export's path questions are counted, and on twice the store their work is under 2.2 times"
        (list (> s 0) (and (> s 0) (< (/ (inexact l) s) 2.2)))
        '(#t #t)))

;; ---- the children questions, counted ----------------------------------------
(define (children-work st thunk)
  (let ((n 0))
    (parameterize ((pending-work-counter (lambda (kind k) (when (eq? kind 'children) (set! n (+ n k))))))
      (thunk st))
    n))
(for-each
  (lambda (v)
    (let ((s (children-work small (cdr v))) (l (children-work large (cdr v))))
      (printf "GR-3 children work of ~a: small ~a large ~a\n" (car v) s l)
      (want (format "GR-3 ~a's children questions are counted, and on twice the store their work is under 2.2 times" (car v))
            (list (> s 0) (and (> s 0) (< (/ (inexact l) s) 2.2)))
            '(#t #t))))
  (filter (lambda (v) (memq (car v) '(export-code changed-files))) verbs))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ngrowth complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
