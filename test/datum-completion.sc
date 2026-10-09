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

;; A datum block takes no src when a persisted plan is completed.
;;
;; A fresh write meets the batch rule before its first record (store.sc,
;; datum-src-refusal). A completion does not: the plan was written earlier,
;; and its members are run as declared. A commit plan written before plans
;; carried a consumes clause, holding a member that sets the src of a block
;; that is datum and has not changed since, passes completion's own datum
;; check (which reads consumes) and its stale judgement, and reaches resolve.
;; Resolve's datum check is then the one that refuses.
;;
;; THE PLAN IS FORGED, as revoke-restore forges one: no route of this build
;; writes a plan without consumes, or one that sets a datum block's src. Its
;; identity is the retry's, computed by the rule the store computes it with
;; (request.sc, request-fingerprint), and that rule is first checked against
;; a real commit's plan.

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
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     ;; AN EXPECTED VALUE THAT RAISES IS A FAIL LINE, with no comparison.
     (call-with-current-continuation
       (lambda (k)
         (let ((x (guard (e (#t (expected-raised! label e) (k #f))) expected)))
           (want-1 label (caught got) x)))))))
(define (expected-raised! label e)
  (set! rows (+ rows 1))
  (set! bad (+ bad 1))
  (printf "FAIL ~a: the expected value raised ~s\n" label
          (if (and (condition? e) (message-condition? e)) (condition-message e) e)))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/datum-completion-" (number->string (get-process-id))))
(when (file-exists? root) (error 'datum-completion "Use a fresh test root" root))
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

;; A text block T, and a datum definition D.
(define T
  (let ((a (call 'insert "--title" "T" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define datum-dir (string-append root "/datum-src"))
(mkdir-p! datum-dir)
(call-with-output-file (string-append datum-dir "/dd.sc")
  (lambda (p) (put-string p "(library (dd) (export h) (import (rnrs))\n(define (h) 'committed))\n")))
(define datum-import (call 'import-code datum-dir "--datum"))
(define D
  (let ((s (state)))
    (find (lambda (id) (datum-mode-block? s id)) (filter (lambda (id) (not (equal? id T))) (state-block-ids s)))))

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
(want "DC-SETUP a datum definition, a committed tracked commit with its plan, and the fingerprint rule reproduces that plan's"
      (list (string? D) (car datum-import) (rpc-ok? r0) (pair? a0)
            (and (pair? a0) (equal? (commit-fingerprint (list-ref a0 0) (list-ref a0 5) T v0) (list-ref a0 3))))
      '(#t ok #t #t #t))

;; ---- a plan written before consumes, setting D's src, its member missing ----
(define who (list-ref a0 0))
(define after (list-ref a0 5))
(define version (draft-version (string->utf8 "(define (h) 'completed)") (block-hash (state) D)
                               (reduce-applied-cut (state))))
(define fingerprint (commit-fingerprint who after D version))
(define plan-payload
  (list 'plan "R7" fingerprint after (list (list 0 'set D 'src "(define (h) 'completed)"))))
;; ITS PAST IS THE STORE AS IT STANDS: the dependencies are the applied cut,
;; so D's history is inside the plan's cut and the stale judgement finds
;; nothing changed since. A record with none would have only itself in its
;; past, and the completion would be refused stale before resolve.
(define plan-deps (reduce-applied-cut (state)))
(define plan-record
  (encode-record 1 1789000000007 (list who (request-identity after "R7") 'plan fingerprint (list-ref a0 4) after)
                 plan-deps (storable-encode plan-payload)))
(want "DC-SETUP the forged plan's past holds every writer of the store, it publishes, and D holds no src"
      (let* ((covers (and (pair? plan-deps) (assoc writer plan-deps) #t))
             (published (car (log-publish! store "planzzzz" 1 plan-record (segment-sha plan-record))))
             (d-src (src D)))
        (list covers published d-src))
      '(#t published #f))

;; ---- the retry completes it: resolve refuses the member ----
(define retry (call 'commit D "--req" "R7" "--cursor" cursor "--working-version" (string-append D "=" version)))
(want "DC-1 completing a plan whose member sets a datum block's src is refused by resolve, naming the block, and D holds no src"
      (list (holds? retry (list 'error 'bad-request 'draft-on-datum-unsupported (list 'block D) '(use def)))
            (src D))
      '(#t #f))
(printf "DC-1 reading: ~s\n" retry)

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\ndatum-completion complete\n" rows bad)
(exit (if (= bad 0) 0 1))
