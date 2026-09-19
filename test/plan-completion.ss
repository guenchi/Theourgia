#!r6rs
;; Copyright 2026 guenchi
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

;; RETRYING A COMMIT AFTER THE DRAFTS ARE GONE.
;;
;; A commit retires the drafts it consumed. A client that did not see
;; the answer retries -- and by then there may be a NEW draft in the same
;; slot, written by the same person who is still editing. The retry must
;; answer for the request it sent, not for what is on disk now.
;;
;; This is what the commit packet used to be for: an immutable file
;; beside the drafts holding the request and the envelopes it had read.
;; §7.5.9 takes it away and puts both facts in the log -- the plan
;; freezes the declared text, its `consumes` names the versions -- so a
;; retry is answered from the log alone.
;;
;; THESE ROWS ARE THE NAMED SUCCESSORS of three rows retired in
;; `working1.ss` and one in `working-projection.ss`:
;;
;;   W4'  <- WS-07 "identity is checked before the old baseline"
;;           WS-08 "frozen replay still succeeds"
;;   fingerprint components <- WS-25 "changed retry arguments are refused"
;;   W4"  <- WP-04 "immutable packet replay precedes live version
;;           selection" (and the other half of WS-07/08)

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia ffi)
        (only (theourgia log) writer-directory))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/plan-completion-" (number->string (get-process-id))))
(when (file-exists? root) (error 'plan-completion "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; ⭐ THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either being told. The core
;; now refuses that call instead; naming the same writer the old fallback
;; would have chosen keeps every row below asking what it asked before.
;;
;; ⛔ AND ONLY WHERE IT WAS MISSING: a call that already names a writer is
;; naming it to make a point, and must keep the one it names.
(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call . args)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (insert title)
  (let ((a (call 'insert "--title" title "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define A (insert "A"))
(define B (insert "B"))
(define (state) (open-and-reduce store))
(define (src id)
  (cdr (assq 'src (cdr (assq 'fields (state-read (state) id))))))
(define (cursor)
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
(define (version-of id)
  (let ((a (call 'read id "--working-info")))
    (and (rpc-ok? a) (list-ref (assq 'projection (cdr a)) 4))))
(define (kind a) (if (and (pair? a) (pair? (cdr a))) (cadr a) a))
(define (draft-count)
  (let ((a (call 'drafts))) (if (rpc-ok? a) (length (cdr (assq 'items (cdr a)))) 'error)))

;; ---- W4' the retry after the drafts are gone ------------------------------

(call 'write A "v1 text")
(define v1 (version-of A))
(define c0 (cursor))
(want "W4' the commit succeeds"
      (rpc-ok? (call 'commit A "--req" "R1" "--cursor" c0 "--working-version" v1)) #t)
(want "W4' it applied the draft's bytes" (src A) "v1 text")
(want "W4' and the draft is gone" (draft-count) 0)

;; The same person keeps editing: a NEW draft in the same slot.
(call 'write A "v2 text")
(define v2 (version-of A))
(want "W4' TWIN: the new draft has a different version" (equal? v1 v2) #f)

(define log-before
  (call-with-port (open-file-input-port
                    (string-append (writer-directory store writer) "/000001.sexp"))
    get-bytevector-all))

(define retry (call 'commit A "--req" "R1" "--cursor" c0 "--working-version" v1))

;; ---- W4' the retry is a replay, and it touches nothing --------------------
;;
;; The drafts this request consumed are gone and a LATER draft is in the
;; slot. The answer is about the request's identity, which the log still
;; holds: the plan, its `consumes` naming v1, and its member.
(want "W4' the retry is a replay"
      (let ((items (cdr (assq 'items (cdr retry)))))
        (and (= 1 (length items)) (assq 'replay (cdr (car items)))))
      '(replay #t))
(want "W4' it wrote nothing"
      (call-with-port (open-file-input-port
                        (string-append (writer-directory store writer) "/000001.sexp"))
        get-bytevector-all)
      log-before)
;; ⭐ AND IT DOES NOT RETIRE THE LATER DRAFT. A replay retires nothing:
;; what this request consumed was retired by the execution it is a
;; replay of, and what is in the slot now is work nobody committed.
;; Measured -- before this was fixed the retry threw v2 away and a
;; `--working` read came back with the committed text.
(want "W4' and the new draft is untouched" (call 'read A "--working") '(ok (text "v2 text")))
(want "W4' the committed text is still the one the request carried" (src A) "v1 text")

;; ---- the fingerprint components -------------------------------------------
;;
;; A retry is the same request only if every part of its identity is the
;; same. Each row below changes ONE part and nothing else.
;;
;; ⛔ EACH IS ITS OWN ROW. A single row that changed two things at once
;; would be green for a build that checked either of them.

;; ⭐ IDENTITY BEFORE PREMISES, WHICH IS WHERE THIS ROW USED TO READ
;; `no-draft`. §7.5.4 fixes the order, and `working-commit!` used to
;; answer about the drafts before the identity had been judged at all --
;; so a retry whose drafts its own first attempt had retired was told
;; there was no draft. `no-draft`, `working-version-changed` and
;; `stale-baseline` are all statements about the store as it is NOW, and
;; they moved into the preflight, which runs after the verdict.
(want "fingerprint: a different block under the same req id is a mismatch"
      (kind (call 'commit B "--req" "R1" "--cursor" c0 "--working-version" v1))
      'req-mismatch)

(call 'write B "b text")
(want "fingerprint: a different VERSION under the same req id is a mismatch"
      (kind (call 'commit A "--req" "R1" "--cursor" c0 "--working-version" v2))
      'req-mismatch)

(want "fingerprint: TWIN: the same request under a NEW req id is accepted"
      (rpc-ok? (call 'commit B "--req" "R2" "--cursor" (cursor)
                     "--working-version" (version-of B))) #t)

;; A different actor is a different `who`, and `who` is in the
;; fingerprint.
(want "fingerprint: a different actor under the same req id is a mismatch"
      (kind (rpc-dispatch store (list 'commit A "--req" "R1" "--cursor" c0
                                      "--working-version" v1 "--writer" writer)
                          "someone-else"))
      'req-mismatch)

;; ---- and which of the two answers comes first --------------------------
;;
;; ⭐ FOUR LAYERS, IN THIS ORDER (§7.6.50 v249): well-formed (the verb
;; exists, the arity fits, the required identity fields are there) ->
;; request identity (replay, mismatch) -> premises (the base) ->
;; execution. So a resend that DROPPED a required field is answered
;; `writer-required`, not `req-mismatch`.
;;
;; ⛔ NOT AN ARBITRARY TIE-BREAK. A fingerprint is computed over a
;; well-formed request, and a request missing a required field has no
;; comparable one; a retry is by definition byte-identical, so something
;; with a field removed is not a retry of anything. Answering
;; `req-mismatch` there would name the difference the caller can see
;; least: it would say "this does not match what you sent before" when
;; what is wrong is the request in hand.
;;
;; THE ROW ABOVE IS THIS ONE'S TWIN and the pair is the whole claim:
;; same req id, one field removed -> the well-formedness answer; same req
;; id, one field CHANGED -> the identity answer. A build that ran the two
;; checks in the other order passes exactly one of them.
(want "layers: the same req id with the writer left out is not a mismatch"
      (kind (rpc-dispatch store (list 'commit A "--req" "R1" "--cursor" c0
                                      "--working-version" v1)
                          "test"))
      'writer-required)

(printf "rows: ~a\n~a failures\nplan-completion complete\n" rows bad)
