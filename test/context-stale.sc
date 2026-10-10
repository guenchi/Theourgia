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

;; A STALE VERIFICATION, AS CONTEXT SHOWS IT.
;;
;; A block whose verifies edge's target has moved past it holds a check of
;; something that is no longer there. context names it: the target is a
;; hard block, about the verifier, entered in evidence with (why
;; stale-verification <m>), and the notes say (stale-verification <m>
;; verifies <c>). The receipt holds it, so a receipt taken before the target
;; moved is refused, and so is one taken while it was stale once the target
;; moves again or the verifier links it again (linking again attests anew).
;; Stale is the lifecycle's: an edge under two names of kind verifies is stale
;; only while the target has moved past both. A store with no moved verifies
;; edge answers as before: with a base tree named (THEOURGIA_BASE_LIBDIR, an
;; opt-in as the change-stream guard's is), its whole answer, receipt
;; included, is compared with the base's, byte for byte.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-id)
        (only (theourgia wire) sexpr->string-extended))

(register-verbs! extension-verbs)

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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/context-stale-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (run store . args) (rpc-dispatch store args "author"))
(define (new-id a)
  (let ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a)))))
    (and ev (pair? (cadr ev)) (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (section a name) (let ((c (clause a name))) (and c (cadr c))))
(define (entry-in a name id) (let ((es (section a name))) (and (list? es) (find (lambda (e) (equal? (car e) id)) es))))
;; A hard block in force is in constraints when authoritative, else in
;; evidence; a stale verification moves neither.
(define (in-force-entry a id) (or (entry-in a 'evidence id) (entry-in a 'constraints id)))
(define (whys e) (if (pair? e) (filter (lambda (c) (and (pair? c) (eq? (car c) 'why))) (cdr e)) '()))
(define (receipt-text a) (let ((r (clause a 'receipt))) (if r (sexpr->string-extended r) "()")))
(define (ctx store id) (run store 'context "--for" id "--budget" "4000"))

;; A store with a block C and a result S that verifies it.
(define (store-with-verification name)
  (let* ((s (string-append root "/" name))
         (_ (run s 'init))
         (c (new-id (run s 'insert "--title" "C")))
         (made (run s 'batch (format "~s" (list '(insert root #f ((kind . doc) (title . "S"))) (list 'link '(from 0) 'verifies c)))))
         (v (new-id (car (cadr made)))))
    (list s c v)))

(define st (tolerant (store-with-verification "s")))
(define S (tolerant (car st)))
(define C (tolerant (cadr st)))
(define V (tolerant (caddr st)))
(define before (tolerant (ctx S V)))
(want "N while the verification is current, context for the verifier names no stale verification"
      (in-order (car before)
                (filter (lambda (n) (and (pair? n) (eq? (car n) 'stale-verification))) (or (section before 'notes) '()))
                (in-force-entry before C))
      '(ok () #f))

(tolerant (run S 'set C "note" "changed after the verification"))
(define after (tolerant (ctx S V)))
(want "S the moved target is a hard block in force, about the verifier: (why stale-verification <m>); not to verify"
      (in-order (and (in-force-entry after C) #t)
                (whys (in-force-entry after C))
                (and (entry-in after 'to-verify C) #t))
      (list #t (list (list 'why 'stale-verification V)) #f))
(want "S the notes say it: (stale-verification <m> verifies <c>)"
      (filter (lambda (n) (and (pair? n) (eq? (car n) 'stale-verification))) (or (section after 'notes) '()))
      (list (list 'stale-verification V 'verifies C)))
(want "S a receipt taken before the target moved is refused; the one taken after holds"
      (in-order (let ((a (run S 'commit "--writer" "author" "--premises" (receipt-text before)))) (and (pair? a) (list (car a) (cadr a))))
                (let ((a (run S 'commit "--writer" "author" "--premises" (receipt-text after)))) (and (pair? a) (car a))))
      '((error premise-changed) ok))

(define (string-contains? text needle)
  (let ((n (string-length text)) (k (string-length needle)))
    (let loop ((i 0)) (cond ((> (+ i k) n) #f) ((string=? (substring text i (+ i k)) needle) #t) (else (loop (+ i 1)))))))
(define (stale-notes a) (filter (lambda (n) (and (pair? n) (eq? (car n) 'stale-verification))) (or (section a 'notes) '())))
(define (refused-by-premise? store a)
  (let ((r (run store 'commit "--writer" "author" "--premises" (receipt-text a))))
    (and (pair? r) (pair? (cdr r)) (list (car r) (cadr r)))))
(tolerant (run S 'set C "note" "changed again, the verification still stale"))
(want "S the target moves again while the verification is stale: the receipt taken while stale is refused"
      (refused-by-premise? S after)
      '(error premise-changed))
(define stale-again (tolerant (ctx S V)))
(tolerant (run S 'link V "verifies" C))
(define attested (tolerant (ctx S V)))
(want "S the verifier links the target again: no stale verification, and the receipt taken while stale is refused"
      (in-order (stale-notes attested) (in-force-entry attested C) (refused-by-premise? S stale-again))
      '(() #f (error premise-changed)))

;; Two names of kind verifies on one edge: stale only past both.
(define S3 (string-append root "/s3"))
(tolerant (run S3 'init))
(tolerant (run S3 'relation "checks" "--as" "verifies"))
(define C3 (tolerant (new-id (run S3 'insert "--title" "C"))))
(define V3 (tolerant (new-id (car (cadr (run S3 'batch (format "~s" (list '(insert root #f ((kind . doc) (title . "S")))
                                                                          (list 'link '(from 0) 'verifies C3)
                                                                          (list 'link '(from 0) 'checks C3)))))))))
(tolerant (run S3 'set C3 "note" "changed after both"))
(define both-moved (tolerant (ctx S3 V3)))
(tolerant (run S3 'link V3 "checks" C3))
(define one-relinked (tolerant (ctx S3 V3)))
(want "S an edge under two names of kind verifies: stale while the target moved past both, not once one of them is linked again"
      (in-order (stale-notes both-moved) (stale-notes one-relinked))
      (list (list (list 'stale-verification V3 'verifies C3)) '()))

;; ---- N: a store with no moved verifies edge keeps its receipt bytes ----------------------------

(define base-lib (getenv "THEOURGIA_BASE_LIBDIR"))
(define st2 (tolerant (store-with-verification "s2")))
;; The whole answer -- sections, notes, budget, receipt -- its versions
;; clause left out on both sides.
(define (without-versions a) (if (pair? a) (cons (car a) (filter (lambda (c) (not (and (pair? c) (eq? (car c) 'versions)))) (cdr a))) a))
(define product-answer (tolerant (sexpr->string-extended (without-versions (ctx (car st2) (caddr st2))))))
(cond
  ((not (and base-lib (> (string-length base-lib) 0)))
   (printf "SKIP: THEOURGIA_BASE_LIBDIR is unset: set it to a library root holding the base tree's theourgia/ and igropyr/ to compare a receipt with the base's (an opt-in, not a failure)~%"))
  (else
   (let ((child (string-append root "/base-receipt.ss"))
         (out (string-append root "/base-receipt.txt")))
     (call-with-output-file child
       (lambda (p)
         (write '(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs)
                         (only (theourgia wire) sexpr->string-extended)) p)
         (write '(register-verbs! extension-verbs) p)
         (write `(let ((a (rpc-dispatch ,(car st2) '(context "--for" ,(caddr st2) "--budget" "4000") "author")))
                   (display (sexpr->string-extended
                              (cons (car a) (filter (lambda (c) (not (and (pair? c) (eq? (car c) 'versions)))) (cdr a))))))
                p))
       'truncate)
     (system (string-append "THEOURGIA_HOME='" root "/home' CHEZSCHEMELIBDIRS='" base-lib "' CHEZSCHEMELIBEXTS='"
                            (getenv "CHEZSCHEMELIBEXTS") "' scheme --script '" child "' < /dev/null > '" out "' 2>/dev/null"))
     (want "N a store with no moved verifies edge: the answer, receipt included, is the base tree's, byte for byte"
           (let ((base (call-with-input-file out get-string-all)))
             (in-order (and (string? product-answer) (> (string-length product-answer) 2)
                            (string-contains? product-answer "(receipt"))
                       (equal? base product-answer)))
           '(#t #t)))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ncontext-stale complete\n" bad rows)
(exit (if (= bad 0) 0 1))
