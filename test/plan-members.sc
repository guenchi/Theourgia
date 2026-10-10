#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; A plan another writer made, whose members are wrapped in expect.
;;
;; A plan this build writes declares its members bare; a plan another
;; writer made can wrap one in (expect <hash> <intent>), and a completion
;; runs it as declared. Every reader of a plan's members reads the intent
;; inside (request.sc, plan-member-intent): the reducer asking whether
;; consumes covers the member (plan-reason), and the completion asking
;; whether the text matches the version consumed (consumes-mismatch).
;;
;; THE PLANS ARE FORGED, as datum-completion forges one: a commit plan of
;; another writer's making, with the retry's identity computed by the rule
;; the store computes it with, checked first against a real commit's plan;
;; its past is the store's applied cut.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (theourgia request) (theourgia wire) (theourgia log))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (with-expected label expected (x) (want-1 label (caught got) x)))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/plan-members-" (number->string (get-process-id))))
(when (file-exists? root) (error 'plan-members "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
(define (call . args)
  (rpc-dispatch store
                (if (and (memq (car args) '(write commit drafts read)) (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (state) (open-and-reduce store))
(define (src id)
  (let* ((b (state-read (state) id)) (p (and b (assq 'src (cdr (assq 'fields b))))))
    (and p (cdr p))))
;; #t when X, or a list anywhere inside it, BEGINS with WANTED: a refusal
;; carries clauses after its own (commit's answer appends its usage).
(define (holds? x wanted)
  (or (and (list? x) (>= (length x) (length wanted)) (equal? (list-head x (length wanted)) wanted))
      (and (pair? x) (or (holds? (car x) wanted) (holds? (cdr x) wanted)))))

;; Two text blocks, P and Q, and a third the real commit uses.
(define (insert-block! title text)
  (let ((a (call 'insert "--title" title "--text" text)))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define T (insert-block! "T" "old T"))
(define P (insert-block! "P" "old P"))
(define Q (insert-block! "Q" "old Q"))

;; ---- a real commit's plan: the request's actor, and the fingerprint rule ----
(call 'write T "the text I wrote")
(define v0 (list-ref (assq 'projection (cdr (call 'read T "--working-info"))) 4))
(define cursor
  (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
(define r0 (call 'commit T "--req" "R0" "--cursor" cursor "--working-version" v0))
(define plan-ev
  (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) (store-evidence store (cons writer "R0"))))
(define a0 (and plan-ev (ev-actor plan-ev)))
(define (commit-fingerprint who after block version)
  (request-fingerprint who 'commit (cons writer (list (string-append block (string #\nul) version))) after))
(want "PM-SETUP a committed tracked commit with its plan, and the fingerprint rule reproduces that plan's"
      (list (rpc-ok? r0) (pair? a0)
            (and (pair? a0) (equal? (commit-fingerprint (list-ref a0 0) (list-ref a0 5) T v0) (list-ref a0 3))))
      '(#t #t #t))

(define who (list-ref a0 0))
(define after (list-ref a0 5))
;; A commit plan consuming BLOCK at VERSION, whose one member sets BLOCK's
;; src to TEXT inside an expect wrapper at the block's current hash. -> the
;; publish answer's head and the version.
(define (forge-plan! req block text version stream)
  (let* ((s (state))
         (hash (block-hash s block))
         (cut (reduce-applied-cut s))
         (fingerprint (commit-fingerprint who after block version))
         (payload (list 'plan req fingerprint after
                        (list (list 0 'expect hash (list 'set block 'src text)))
                        (list 'consumes writer (list (list block version hash cut)))))
         (record (encode-record 1 1789000000008
                                (list who (request-identity after req) 'plan fingerprint (list-ref a0 4) after)
                                cut (storable-encode payload))))
    (car (log-publish! store stream 1 record (segment-sha record)))))
(define (retry req block version)
  (call 'commit block "--req" req "--cursor" cursor "--working-version" (string-append block "=" version)))

;; ---- a wrapped member whose text the consumed version does not name ----
(define (version-of text block)
  (let ((s (state))) (draft-version (string->utf8 text) (block-hash s block) (reduce-applied-cut s))))
(define p-version (version-of "the text the version names" P))
(want "PM-SETUP the plan for P publishes: its member says another text than the version names"
      (forge-plan! "R8" P "a text the version does not name" p-version "planzzz8")
      'published)
(define p-retry (retry "R8" P p-version))
(printf "PM-1 reading: ~s\n" p-retry)
(want "PM-1 completing it is refused consumes-version-mismatch, naming P, and P keeps its text"
      (list (holds? p-retry (list 'error 'consumes-version-mismatch (list 'block P))) (src P))
      '(#t "old P"))

;; ---- TWIN: a wrapped member whose text the version names completes ----
(define q-text "the text the plan sets")
(define q-version (version-of q-text Q))
(want "PM-SETUP the plan for Q publishes: its member's text is the one the version names"
      (forge-plan! "R9" Q q-text q-version "planzzz9")
      'published)
(define q-retry (retry "R9" Q q-version))
(printf "PM-2 reading: ~s\n" q-retry)
(want "PM-2 TWIN: completing it writes the member: Q holds the text"
      (list (rpc-ok? q-retry) (src Q))
      (list #t q-text))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nplan-members complete\n" rows bad)
(exit (if (= bad 0) 0 1))
