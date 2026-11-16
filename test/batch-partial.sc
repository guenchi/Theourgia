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
;; the store's judgement may be `partial` (a prefix on disk) or `planned` (the
;; receipt alone), each meaning "the rest provably did not run" -- and the
;; store cannot know that yet: a writer chain cut past the receipt (a sealed
;; segment torn, a segment missing) reaches the load as an `incomplete` cut and
;; not as an uncertain interval, so the judgement would claim it anyway. Both
;; answer unknown, with what the store saw (the indices on disk, for partial).
;;
;;   BP-1  three items, the second refused; sent again: (error unknown (0)),
;;         and nothing more written
;;   BP-2  CONTROL: three items that all ran; sent again: a replay
;;   BP-3  three items, the first refused; sent again: (error unknown)
;;   BP-4  the chain: the receipt and item 0 in segment 2, a later record alone
;;         in segment 3 (sealed, the middle), segment 4 current; segment 3 torn:
;;         sent again, unknown, the load's incomplete cut said
;;   BP-5  the same with segment 3's file removed
(import (chezscheme) (theourgia rpc) (only (theourgia log) log-clock) (only (theourgia reduce) block-id))

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

(want "BP-1 a tracked batch stopped by its second item's refusal, sent again under its request, answers unknown with the indices on disk, and nothing more is written"
      (let* ((s (store-with-a-block!)) (d (car s)) (a (cadr s)) (cursor (caddr s))
             (intents (format "((set ~s title \"x\") (set \"no-such.1\" title \"y\") (set ~s note \"z\"))" a a))
             (first (run d intents "batch" "--req" "r-p" "--cursor" cursor))
             (end1 (end-of d))
             (again (run d intents "batch" "--req" "r-p" "--cursor" cursor)))
        (list (map car (items first)) (items again) (= end1 (end-of d))))
      '((ok error) ((error unknown (0))) #t))

(want "BP-2 CONTROL: a tracked batch of three that all ran, sent again under its request, answers a replay"
      (let* ((s (store-with-a-block!)) (d (car s)) (a (cadr s)) (cursor (caddr s))
             (intents (format "((set ~s title \"x\") (set ~s note \"y\") (set ~s note \"z\"))" a a a))
             (first (run d intents "batch" "--req" "r-w" "--cursor" cursor))
             (again (run d intents "batch" "--req" "r-w" "--cursor" cursor)))
        (list (map car (items first))
              (let ((i (items again))) (and (pair? i) (pair? (car i)) (assq 'replay (cdr (car i)))))))
      '((ok ok ok) (replay #t)))

(want "BP-3 a tracked batch whose first item is refused, sent again under its request, answers unknown, and nothing more is written"
      (let* ((s (store-with-a-block!)) (d (car s)) (a (cadr s)) (cursor (caddr s))
             (intents (format "((set \"no-such.1\" title \"y\") (set ~s title \"x\") (set ~s note \"z\"))" a a))
             (first (run d intents "batch" "--req" "r-n" "--cursor" cursor))
             (end1 (end-of d))
             (again (run d intents "batch" "--req" "r-n" "--cursor" cursor)))
        (list (map car (items first)) (items again) (= end1 (end-of d))))
      '((error) ((error unknown)) #t))

;; ---- the chain cut past the receipt, in process ----------------------------
;; the fixture's own machine home, as the command-line rows above use it
(putenv "THEOURGIA_HOME" home)
;; The log's clock moves an hour between the steps, past the rotation age, so
;; each lands in a segment of its own.
(define hour 3600000)
(define c1 1757300000000)
(define (at-clock c thunk) (parameterize ((log-clock (lambda () c))) (thunk)))
(define (dispatch d c . args) (at-clock c (lambda () (rpc-dispatch d args "tester"))))
(define (id-of a) (let* ((ev (assq 'events (cdr a))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (segments d w)
  (list-sort string<?
             (filter (lambda (f) (and (> (string-length f) 5) (char-numeric? (string-ref f 0))
                                      (string=? (substring f (- (string-length f) 5) (string-length f)) ".sexp")))
                     (directory-list (string-append d "/writers/" w)))))
(define (bytes-of p) (call-with-port (open-file-input-port p) get-bytevector-all))
;; -> (store writer intents cursor): the chain built, four segments
(define (chain! name)
  (let* ((d (string-append root "/" name))
         (w (cadr (assq 'writer (cdr (dispatch d c1 'init)))))
         (a (id-of (dispatch d c1 'insert "--title" "A")))
         (b (dispatch d c1 'insert "--title" "B"))
         (cursor (let ((c (cadr (assq 'cursor (cdr b))))) (format "~a:~a" (car c) (cdr c))))
         (intents (format "((set ~s title \"x\") (set \"no-such.1\" title \"y\") (set ~s note \"z\"))" a a)))
    (dispatch d (+ c1 hour 1) 'batch intents "--req" "R" "--cursor" cursor)
    (dispatch d (+ c1 (* 2 hour) 2) 'insert "--title" "C")
    (dispatch d (+ c1 (* 3 hour) 3) 'insert "--title" "D")
    (list d w intents cursor)))
(define (again s) (dispatch (car s) (+ c1 (* 3 hour) 4) 'batch (caddr s) "--req" "R" "--cursor" (cadddr s)))
(define (cut-kind a)
  (let ((i (and (list? a) (assq 'incomplete (filter pair? (cdr a))))))
    (and i (let ((c (assq 'cut (filter pair? (cdr i))))) (and c (let ((k (assq 'kind (filter pair? (cdr c))))) (and k (cadr k))))))))

(define torn (chain! "torn"))
(define torn-path (string-append (car torn) "/writers/" (cadr torn) "/" (list-ref (segments (car torn) (cadr torn)) 2)))
(define torn-before (again torn))
(define torn-size (bytevector-length (bytes-of torn-path)))
(let ((b (bytes-of torn-path)))
  (call-with-port (open-file-output-port torn-path (file-options no-fail))
    (lambda (p) (put-bytevector p b 0 (- (bytevector-length b) 3)))))
(define torn-after (again torn))
(printf "BP-4: before the tear ~s\n  after ~s\n" torn-before torn-after)
(want "BP-4 a writer chain whose sealed middle segment past the receipt is torn: four segments, the tear there, the load says the cut, and the batch sent again answers unknown"
      (list (length (segments (car torn) (cadr torn))) (- torn-size (bytevector-length (bytes-of torn-path)))
            (cut-kind torn-after) (and (pair? torn-after) (eq? (car torn-after) 'batch) (cadr torn-after)))
      '(4 3 torn-in-sealed ((error unknown (0)))))

(define gone (chain! "gone"))
(define gone-path (string-append (car gone) "/writers/" (cadr gone) "/" (list-ref (segments (car gone) (cadr gone)) 2)))
(define gone-count (length (segments (car gone) (cadr gone))))
(delete-file gone-path)
(define gone-after (again gone))
(printf "BP-5: after ~s\n" gone-after)
(want "BP-5 a writer chain whose middle segment past the receipt is removed: four segments before, three after, the load says the cut, and the batch sent again answers unknown"
      (list gone-count (length (segments (car gone) (cadr gone))) (cut-kind gone-after)
            (and (pair? gone-after) (eq? (car gone-after) 'batch) (cadr gone-after)))
      '(4 3 seq ((error unknown (0)))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nbatch-partial complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
