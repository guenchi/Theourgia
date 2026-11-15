;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; AN EXPECTATION THAT NO LONGER HOLDS IS REFUSED WITH THE BLOCK AS IT IS NOW
;; (README, set and batch: `(error changed (current <version>) (fields ...))`).
;; A writer that expected an older version is told what it would have
;; overwritten -- the block's current field table, in the shape a read gives
;; it -- without a second request. Through the command line.
;;
;;   EF-1  batch (expect <old> (set ...)): changed, current, the current fields
;;   EF-2  set --if-unchanged <old>: the same refusal
;;   EF-3  CONTROL: an expectation that holds: ok, state, no fields
(import (chezscheme))

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
                   "/expect-fields-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(define home (string-append root "/home"))
(define cli "../core.sc")

;; -> the first datum a command answered, with TEXT on its standard input
(define (run store text . args)
  (call-with-output-file (string-append root "/in") (lambda (p) (display text p)) 'replace)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER THEOURGIA_HOME=" home
            " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " --wire < " root "/in > " root "/out 2> " root "/err"))
  (let ((x (call-with-input-file (string-append root "/out") read)))
    (if (eof-object? x) (list 'no-answer) x)))
(define n-store 0)
;; -> (<store> <block id> <cursor text>): a store holding one block, and the
;; cursor after it
(define (store-with-a-block!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (run d "" "init")
    (let* ((a (run d "" "insert" "--under" "root" "--title" "A"))
           (w (cadr (assq 'cursor (cdr a))))
           (id (car (car (cadr (assq 'state (cdr a)))))))
      (list d id (string-append (car w) ":" (number->string (cdr w)))))))
(define (end-of d)
  (let* ((c (run d "" "check")) (w (cadr (assq 'local-writer (cdr c)))))
    (cadr (assq 'end (cdr (assoc w (cadr (assq 'writers (cdr c)))))))))
;; a batch answer's items, or the answer itself when it is not one
(define (items a) (if (and (pair? a) (eq? (car a) 'batch) (pair? (cdr a))) (cadr a) (list 'not-a-batch a)))
(define (clause a k) (and (pair? a) (list? a) (assq k (filter pair? (cdr a)))))
;; -> (<version> <fields>): what a read answers for the block
(define (read-of d id)
  (let ((r (run d "" "read" id)))
    (list (cadr (clause r 'version)) (cdr (assq 'fields (cadr r))))))
(define (refusal-parts a) (list (car a) (cadr a) (clause a 'current) (clause a 'fields) (clause a 'state)))

;; P reads the block, Q sets note "x"; P then writes expecting the version it
;; read. The refusal carries the block as it is now: its version and its
;; field table, the one a read gives.
(define s (store-with-a-block!))
(define d (car s))
(define b (cadr s))
(define before (read-of d b))
(run d "" "set" b "note" "x")
(define now (read-of d b))
(printf "the block before: ~s\nand after the other write: ~s\n" before now)

(want "EF-1 a batch's expect on the version before another write is refused changed, with the block's current version and its current fields, note \"x\" among them"
      (let ((a (car (items (run d (format "((expect ~s (set ~s note \"y\")))" (car before) b) "batch")))))
        (list (refusal-parts a) (let ((f (clause a 'fields))) (and f (equal? (assq 'note (cadr f)) '(note . "x"))))))
      (list (list 'error 'changed (list 'current (car now)) (list 'fields (cadr now)) #f) #t))

(want "EF-2 set --if-unchanged on the version before another write is refused the same way"
      (refusal-parts (run d "" "set" b "note" "y" "--if-unchanged" (car before)))
      (list 'error 'changed (list 'current (car now)) (list 'fields (cadr now)) #f))

(want "EF-3 CONTROL: a set whose expected version is the current one succeeds, and its answer carries state and no fields"
      (let ((a (run d "" "set" b "note" "z" "--if-unchanged" (car now))))
        (list (car a) (and (clause a 'state) #t) (clause a 'fields)))
      '(ok #t #f))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nexpect-fields complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
