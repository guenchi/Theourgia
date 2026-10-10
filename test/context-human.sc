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

;; CONTEXT'S HUMAN OUTPUT CARRIES THE RECEIPT.
;;
;; The human rendering of a context answer ends with the receipt on one
;; line, the clause exactly as --wire writes it, so a person reading the
;; human form can hand it to commit --premises as it stands; an incomplete
;; clause follows it, as on the wire. Everything before that line is what
;; it was: with a base tree named (THEOURGIA_BASE_LIBDIR, an opt-in as the
;; change-stream guard's is), the human output is compared with the base's,
;; a trailing receipt line left out of both.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-id)
        (only (theourgia render) render-wire render-human answer-printing!))

(register-verbs! extension-verbs)
;; As the command line prints: once per process, before any answer.
(answer-printing!)

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x))))
             e))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/context-human-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (run store . args) (rpc-dispatch store args "author"))
(define (new-id a)
  (let ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a)))))
    (and ev (pair? (cadr ev)) (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (string-contains? text needle)
  (let ((n (string-length text)) (k (string-length needle)))
    (let loop ((i 0)) (cond ((> (+ i k) n) #f) ((string=? (substring text i (+ i k)) needle) #t) (else (loop (+ i 1)))))))
;; The text's lines, each without its newline.
(define (lines text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text))
           (reverse (if (= start i) out (cons (substring text start i) out))))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))
(define (last-of xs) (and (pair? xs) (list-ref xs (- (length xs) 1))))
(define (but-last xs) (if (pair? xs) (list-head xs (- (length xs) 1)) xs))
(define (join ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))
(define (ctx store id) (run store 'context "--for" id "--budget" "4000"))

;; A store with a task that depends on a contract.
(define S (string-append root "/s"))
(tolerant (run S 'init))
(define K (tolerant (new-id (run S 'insert "--title" "the contract"))))
(define T (tolerant (new-id (run S 'insert "--title" "the task"))))
(tolerant (run S 'link T "depends-on" K))
(define answer (tolerant (ctx S T)))
(define human (tolerant (render-human answer)))
(define receipt-line (tolerant (last-of (lines human))))

(want "H the human output ends with the receipt on one line: read back, it is the answer's receipt clause"
      (in-order (car answer) (read (open-string-input-port receipt-line)) )
      (list 'ok (clause answer 'receipt)))
(want "H the line is the receipt exactly as --wire writes it"
      (in-order (and (pair? (clause answer 'receipt)) (car (clause answer 'receipt)))
                (string-contains? (render-wire answer) receipt-line))
      '(receipt #t))
(want "H commit --premises takes the line as printed"
      (car (run S 'commit "--writer" "author" "--premises" receipt-line))
      'ok)

;; THE LINES' SHAPE, whatever the base: each line that is not an entry (an
;; entry is indented), by its first word -- the section names in order, the
;; tail clauses, then the receipt.
(define (head-word l)
  (let loop ((i 0))
    (if (or (= i (string-length l)) (char=? (string-ref l i) #\space)) (substring l 0 i) (loop (+ i 1)))))
(want "H the lines that are not entries: for, the five sections, notes, excluded, budget, then the receipt"
      (map head-word (filter (lambda (l) (and (> (string-length l) 0) (not (char=? (string-ref l 0) #\space)))) (lines human)))
      '("for" "constraints" "evidence" "to-verify" "counterexamples" "background" "notes" "(excluded" "(budget" "(receipt"))

;; An incomplete clause follows the receipt, as it does on the wire.
(define constructed
  '(ok (for ("a.1" (level full) (title "t"))) (constraints ()) (evidence ()) (to-verify ())
       (counterexamples ()) (background ()) (notes ())
       (excluded (definitions 0) (evidence 0) (counterexamples 0) (dependents 0) (relevant 0) (ancestors 0))
       (budget (tokens 4000) (used 10) (reserve 400))
       (receipt (premise "a.1" "h"))
       (cut (("a" . 1))) (versions (("a.1" . "h")))
       (incomplete (writers "w"))))
(want "H a writer that could not be read: the receipt line, then the incomplete clause; no cut, no versions"
      (let ((ls (lines (render-human constructed))))
        (in-order (list-tail ls (- (length ls) 2))
                  (exists (lambda (l) (string-contains? l "(cut ")) ls)
                  (exists (lambda (l) (string-contains? l "(versions ")) ls)))
      (list (list "(receipt (premise \"a.1\" \"h\"))" "(incomplete (writers \"w\"))") #f #f))

;; ---- N: everything before the receipt line is the base tree's --------------------------------
;;
;; A TRAILING RECEIPT LINE IS LEFT OUT OF BOTH, whether or not the base prints
;; one, so the row does not say what the base lacks: it says the rest is the
;; same. That this tree prints one is its own element.
(define (receipt-line? l) (and (>= (string-length l) 8) (string=? (substring l 0 8) "(receipt")))
(define (without-receipt-line text)
  (let ((ls (lines text)))
    (join (if (and (pair? ls) (receipt-line? (last-of ls))) (but-last ls) ls))))

(define base-lib (getenv "THEOURGIA_BASE_LIBDIR"))
(cond
  ((not (and base-lib (> (string-length base-lib) 0)))
   (printf "SKIP: THEOURGIA_BASE_LIBDIR is unset: set it to a library root holding the base tree's theourgia/ and igropyr/ to compare the human output with the base's (an opt-in, not a failure)~%"))
  (else
   (let ((child (string-append root "/base-human.ss"))
         (out (string-append root "/base-human.txt")))
     (call-with-output-file child
       (lambda (p)
         (write '(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs)
                         (only (theourgia render) render-human)) p)
         (write '(register-verbs! extension-verbs) p)
         (write `(display (render-human (rpc-dispatch ,S '(context "--for" ,T "--budget" "4000") "author"))) p))
       'truncate)
     (system (string-append "THEOURGIA_HOME='" root "/home' CHEZSCHEMELIBDIRS='" base-lib "' CHEZSCHEMELIBEXTS='"
                            (getenv "CHEZSCHEMELIBEXTS") "' scheme --script '" child "' < /dev/null > '" out "' 2>/dev/null"))
     (want "N the human output is the base tree's, byte for byte, a trailing receipt line left out of both; this tree's ends with one"
           (let ((base (call-with-input-file out get-string-all))
                 (now (render-human (ctx S T))))
             (in-order (> (string-length base) 0)
                       (equal? (without-receipt-line base) (without-receipt-line now))
                       (receipt-line? (or (last-of (lines now)) ""))))
           '(#t #t #t)))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ncontext-human complete\n" bad rows)
(exit (if (= bad 0) 0 1))
