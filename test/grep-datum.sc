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

;; GREP READS A DATUM BLOCK AS IT IS PRINTED, LINE BY LINE (README, grep):
;; the datum projection's printer, what export-code --datum writes, and the
;; line numbers count from 1 in that printed body. It read the body written
;; on one line after the doc, so a form matched whole, on line 2.
;;
;;   GD-1  a form the printer breaks over lines: the match is the printed line
;;         holding the word, alone, with its number in the printed body
;;   GD-2  CONTROL: a text block's match is its source line, numbered as before
(import (chezscheme) (only (theourgia ffi) source-datum-print))
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
                   "/grep-datum-" (number->string (get-process-id)))))
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
;; -> the hits of grep: ((<id> <line> <text>) ...)
(define (hits a)
  (let ((i (and (list? a) (assq 'items (filter pair? (cdr a))))))
    (if i (map (lambda (m) (if (and (pair? m) (eq? (car m) 'match)) (cdr m) m)) (cdr i)) (list 'no-items a))))
(define (lines text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text)) (reverse (if (< start i) (cons (substring text start i) out) out)))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))
(define (contains? hay needle)
  (let ((n (string-length needle)) (m (string-length hay)))
    (let loop ((i 0)) (and (<= (+ i n) m) (or (string=? (substring hay i (+ i n)) needle) (loop (+ i 1)))))))
(define (index-of pred xs) (let loop ((xs xs) (k 1)) (cond ((null? xs) #f) ((pred (car xs)) k) (else (loop (cdr xs) (+ k 1))))))

(define form "(define (k)\n  (list 'alpha-element 'beta-element 'gamma-element 'delta-element\n        'needle 'epsilon-element 'zeta-element))\n")
(define (store-with-file! datum? . text)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))) (src (string-append root "/src" (number->string n-store))))
    (run d "" "init")
    (system (string-append "mkdir -p '" src "'"))
    (call-with-output-file (string-append src "/k.scm")
      (lambda (p) (display (if (pair? text) (car text) form) p)))
    (if datum? (run d "" "import-code" src "--datum") (run d "" "import-code" src))
    d))

(define dd (store-with-file! #t))
(define found (hits (run dd "" "grep" "needle")))
(printf "grep needle over the datum block: ~s\n" found)
(define printed
  (and (pair? found) (pair? (car found)) (string? (car (car found)))
       (source-datum-print (cdr (assq 'body (cdr (assq 'fields (cadr (run dd "" "read" (car (car found)))))))))))
(printf "the body as printed:\n~a" printed)
(want "GD-1 a datum form the printer breaks over lines: grep answers the printed line holding the word, alone, numbered in the printed body"
      (list (length found) (cdr (car found))
            (> (length (lines printed)) 1))
      (let ((ls (lines printed)))
        (list 1 (let ((k (index-of (lambda (l) (contains? l "needle")) ls))) (list k (list-ref ls (- k 1)))) #t)))

(define dt (store-with-file! #f))
;; A WORD PAST ASCII (GD-3, GD-4): the export escapes it, and grep reads the
;; body with it as it is written, so the word is found, and search agrees.
(define wide-form "(define (greet)\n  (list \"na\x00ef;ve caf\x00e9;\" 'other))\n")
(define dw (store-with-file! #t wide-form))
(define wide (hits (run dw "" "grep" "na\x00ef;ve")))
(printf "grep na\x00ef;ve over the datum block: ~s\n" wide)
(want "GD-3 grep finds a word past ASCII in a datum body, on its printed line"
      (and (pair? wide) (pair? (car wide)) (string? (caddr (car wide)))
           (contains? (caddr (car wide)) "na\x00ef;ve caf\x00e9;"))
      #t)
(want "GD-4 grep and search agree on that word: both name the one datum block"
      (let ((sr (run dw "" "search" "na\x00ef;ve")))
        (list (map car wide)
              (let ((i (and (list? sr) (assq 'items (filter pair? (cdr sr))))))
                (if i (map (lambda (x) (if (pair? x) (cadr x) x)) (cdr i)) (list 'no-items sr)))))
      ;; exactly one block, named by both: two empty answers do not agree
      (if (= 1 (length wide)) (let ((ids (map car wide))) (list ids ids)) '(one-block one-block)))

(want "GD-2 CONTROL: in a text block the match is its source line, line 3 of the file"
      (map cdr (hits (run dt "" "grep" "needle")))
      '((3 "        'needle 'epsilon-element 'zeta-element))")))

;; GD-5, A DATUM BODY THAT IS A STRING OR A BYTEVECTOR is printed as the export
;; prints it (datum-print): the string as its literal "...", the bytevector as
;; #vu8(...), on body line 1 (codex round (g)).
(set! n-store (+ n-store 1))
(define d5 (string-append root "/s" (number->string n-store)))
(run d5 "" "init")
(define b5 (run d5 "((insert root #f ((kind . code) (mode . datum) (body . \"zqneedle\"))) (insert root #f ((kind . code) (mode . datum) (body . #vu8(7 8 9)))))" "batch"))
(printf "GD-5 the batch: ~s\n" b5)
(define g5s (hits (run d5 "" "grep" "\"zqneedle\"")))
(define g5b (hits (run d5 "" "grep" "#vu8(7 8 9)")))
(printf "GD-5 grep of the quoted string: ~s; of the bytevector: ~s\n" g5s g5b)
(want "GD-5 a datum body that is a string is found as its printed literal, and a bytevector body as #vu8(...), each on body line 1"
      (list (and (pair? g5s) (pair? (car g5s)) (cadr (car g5s)))
            (and (pair? g5b) (pair? (car g5b)) (cadr (car g5b))))
      '(1 1))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ngrep-datum complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
