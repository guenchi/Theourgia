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

;; A REQUEST'S ACTORS COME FROM ONE FINGERPRINT.
;;
;; Every record a tracked request writes carries an actor, and the actor
;; carries the request's fingerprint: a hash over the request's whole
;; arguments, which for an import are every intent it holds. It was computed
;; again for every record, so an import of n intents hashed its own arguments
;; n times. store.sc's request-actors makes a request's actors from one
;; fingerprint, computed at the first actor (inside the loop's guard for that
;; intent, where it always was) and kept.
;;
;; RA-1: the actors are what an actor has always been -- the request's who,
;; identity, the sub, the fingerprint, the plan event and the cursor -- for
;; indexed subs, plan and single. RA-2: over a thousand subs the fingerprint
;; is asked for once (a counting stand-in, request-actors' own argument), and
;; every actor carries what it answered. RA-3: no request, no actor, and no
;; fingerprint asked for. RA-4: made and never asked for an actor, nothing is
;; hashed (a loop that stops before its first actor). request-actors is looked up by eval, so a tree
;; without it loads this file and its rows read red with the reason.

(import (chezscheme) (theourgia request) (only (theourgia store) make-write-request))

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

(define (actors-of . args)
  (apply (eval 'request-actors (environment '(theourgia store))) args))

(define after (cons "w0000000" 3))
(define args (list "a.js" "b.js" (make-string 2000 #\x)))
(define req (make-write-request "test" 'import args "r1" after))
(define plan-event (cons "w0000000" 4))
(define (actor-of sub)
  (list "test" (request-identity after "r1") sub (request-fingerprint "test" 'import args after) plan-event after))

(want "RA-1 the actors are the request's who, identity, sub, fingerprint, plan event and cursor, for indexed subs, plan and single"
      (let ((actors (actors-of req plan-event)))
        (map (lambda (sub) (equal? (actors sub) (actor-of sub))) '(0 1 2 plan single)))
      '(#t #t #t #t #t))

(want "RA-2 a thousand actors of one request: the fingerprint asked for once, and every actor carries it"
      (let* ((asked 0)
             (counting (lambda (r) (set! asked (+ asked 1)) "fp-counted"))
             (actors (actors-of req plan-event counting))
             (made (map actors (iota 1000))))
        (list asked
              (for-all (lambda (a) (equal? (list-ref a 3) "fp-counted")) made)
              (equal? (car made) (list "test" (request-identity after "r1") 0 "fp-counted" plan-event after))))
      '(1 #t #t))

(want "RA-3 no request: every actor is #f and the fingerprint is not asked for"
      (let* ((asked 0)
             (counting (lambda (r) (set! asked (+ asked 1)) "fp"))
             (actors (actors-of #f plan-event counting)))
        (list (map actors '(0 plan single)) asked))
      '((#f #f #f) 0))

(want "RA-4 made and never asked for an actor: the fingerprint is not asked for"
      (let* ((asked 0)
             (counting (lambda (r) (set! asked (+ asked 1)) "fp"))
             (actors (actors-of req plan-event counting)))
        (list (procedure? actors) asked))
      '(#t 0))

(printf "\n~a failures\nrows: ~a\nrequest-actors complete\n" bad rows)
(exit (if (= bad 0) 0 1))
