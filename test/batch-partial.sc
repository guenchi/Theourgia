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

;; A TRACKED BATCH STOPPED PART WAY, SENT AGAIN (README, Requests and replay:
;; the batch states). A batch is not atomic: an item the store refuses stops
;; the rest with the earlier items on disk. Sent again under the same request,
;; the store's judgement is `partial` -- a prefix on disk and the rest
;; provably not run -- and the answer names it, with the count applied and the
;; next item; it was answered `unknown`. Through the command line, as a client
;; meets it.
;;
;;   BP-1  three items, the second refused; sent again: (error partial (applied 1)
;;         (next 1) (present (0))), and nothing more written
;;   BP-2  CONTROL: three items that all ran; sent again: a replay
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
                   "/batch-partial-" (number->string (get-process-id)))))
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

(want "BP-1 a tracked batch stopped by its second item's refusal, sent again under its request, answers partial: one applied, the next is item 1, and nothing more is written"
      (let* ((s (store-with-a-block!)) (d (car s)) (a (cadr s)) (cursor (caddr s))
             (intents (format "((set ~s title \"x\") (set \"no-such.1\" title \"y\") (set ~s note \"z\"))" a a))
             (first (run d intents "batch" "--req" "r-p" "--cursor" cursor))
             (end1 (end-of d))
             (again (run d intents "batch" "--req" "r-p" "--cursor" cursor)))
        (list (map car (items first)) (items again) (= end1 (end-of d))))
      '((ok error) ((error partial (applied 1) (next 1) (present (0)))) #t))

(want "BP-2 CONTROL: a tracked batch of three that all ran, sent again under its request, answers a replay"
      (let* ((s (store-with-a-block!)) (d (car s)) (a (cadr s)) (cursor (caddr s))
             (intents (format "((set ~s title \"x\") (set ~s note \"y\") (set ~s note \"z\"))" a a a))
             (first (run d intents "batch" "--req" "r-w" "--cursor" cursor))
             (again (run d intents "batch" "--req" "r-w" "--cursor" cursor)))
        (list (map car (items first))
              (let ((i (items again))) (and (pair? i) (pair? (car i)) (assq 'replay (cdr (car i)))))))
      '((ok ok ok) (replay #t)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nbatch-partial complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
