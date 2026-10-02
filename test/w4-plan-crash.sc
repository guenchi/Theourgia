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

;; W4": THE PLAN IS DURABLE AND ITS MEMBER IS NOT.
;;
;; A commit writes its plan, makes it durable, and then writes the
;; members. A process that dies in between leaves a request that has
;; been ACCEPTED and not carried out -- and the client, who saw no
;; answer, retries.
;;
;; WHAT MUST HAPPEN: the retry completes the request from the plan's own
;; frozen declaration. NEVER: It does not read the drafts. By then the same
;; person may have written a new draft into the same slot, and carrying
;; THAT out would execute a request nobody sent.
;;
;; THE CRASH IS REAL, NOT SIMULATED. A child process commits with a
;; barrier armed on every append; the first append -- the plan -- is
;; released, and the child is killed while it waits at the second. The
;; store is left exactly as a crash would leave it. The dance is the
;; helper crash-at.ss, which other fixtures share.
;;
;; NOTE: EVERY WAIT HERE IS BOUNDED. A fifo write with no reader blocks
;; forever, and this suite has already lost fifteen minutes a fixture to
;; that: `cli1` spun out a bounded wait for children that had died and
;; then wrote to a fifo nobody would ever read. The whole dance runs
;; under one alarm, the script kills its child on any exit, and the
;; fixture asserts what it observed rather than assuming the dance
;; worked.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia ffi) (theourgia wire) (theourgia working)
        (theourgia log)
        (only (theourgia store) store-evidence))

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

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define cli (string-append script-dir "/../core.sc"))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/w4-plan-crash-" (number->string (get-process-id))))
(when (file-exists? root) (error 'w4-plan-crash "Use a fresh test root" root))
(mkdir-p! root)
(define home (string-append root "/home"))
(mkdir-p! home)
(putenv "THEOURGIA_HOME" home)
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; KEY: THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either being told. The core
;; now refuses that call instead; naming the same writer the old fallback
;; would have chosen keeps every row below asking what it asked before.
;;
;; NEVER: AND ONLY WHERE IT WAS MISSING: a call that already names a writer is
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
(define (state) (open-and-reduce store))
(define (src id)
  (let* ((b (state-read (state) id)) (p (and b (assq 'src (cdr (assq 'fields b))))))
    (and p (cdr p))))
(define A
  (let ((a (call 'insert "--title" "A" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(call 'write A "frozen text")
(define v1 (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 4))
(define cursor
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))

;; ---- the crash ------------------------------------------------------------

(include "crash-at.ss")
;; The child commits; its first append (the plan) is released and it is
;; killed while it waits at the second (the member).
(define barriers
  (car (crash-at 2 root home cli
                 (list "commit" A "--req" "R1" "--cursor" cursor "--working-version" v1
                       "--writer" writer "--store" store))))

;; KEY: THE FIXTURE ASSERTS THAT THE CRASH HAPPENED WHERE IT MEANT TO.
;; Two barriers observed means the plan's append was released and the
;; child was waiting at the member's. One means it never got past the
;; plan; none means it never started. Any of those makes every row below
;; a statement about something else.
(want "W4\" the child stopped at the second append" barriers 2)

;; ---- what the crash left --------------------------------------------------

(define evidence (store-evidence store (cons writer "R1")))
(want "W4\" the plan is durable and its member is not"
      (map (lambda (e) (actor-sub (ev-actor e))) evidence) '(plan))
(want "W4\" so the block still says what it said before the commit" (src A) "old")

;; The same person keeps editing while the commit is in limbo.
(call 'write A "later text")
(want "W4\" a new draft is in the slot" (call 'read A "--working") '(ok (text "later text")))

;; ---- the retry ------------------------------------------------------------

;; NOTE: THE RETRY IS SENT THE SAME WAY THE FIRST ATTEMPT WAS.
;;
;; A request's fingerprint is taken over its argument STRINGS, so a
;; retry that spells its arguments differently is a different request --
;; which is the rule, not a defect. The first attempt was a command
;; line; this one is too. Sending it in process, without `--store`,
;; answered `req-mismatch`, and that answer was about the fixture.
(define out-path (string-append root "/retry.txt"))
(define (cli-retry)
  (system (string-append
            "env THEOURGIA_HOME='" home "' scheme --script '" cli "' "
            "commit '" A "' --req R1 --cursor '" cursor "' "
            "--working-version '" v1 "' --writer '" writer "' "
            "--store '" store "' > '" out-path "' 2>&1"))
  (call-with-input-file out-path
    (lambda (p)
      (let loop ((last #f))
        (let ((x (guard (e (#t 'unreadable)) (read p))))
          (cond ((eof-object? x) last)
                ((eq? x 'unreadable) last)
                (else (loop x))))))))
(define retry (cli-retry))
(printf "retry observation ~s\n" retry)
(want "W4\" the retry succeeds" (and (pair? retry) (eq? 'ok (car retry))) #t)
(want "W4\" it completed the plan's own text, not the later draft (section-13 L21)" (src A) "frozen text")
(want "W4\" the plan now has its member"
      (map (lambda (e) (actor-sub (ev-actor e)))
           (store-evidence store (cons writer "R1")))
      '(plan 0))

;; NEVER: AND THE LATER DRAFT IS STILL A DRAFT. It was never consumed: the
;; request named v1, and the completion path does not look at the slot.
(want "W4\" the later draft survives" (call 'read A "--working") '(ok (text "later text")))

;; ---- W6-consumes-version-mismatch: the frozen text and its name -----------
;;
;; A plan says two things about the same draft: "this is the text" and
;; "this was version V of it", where V is sha256(text || based-on ||
;; cut). NEVER: A REAL COMMIT CANNOT MAKE THEM DISAGREE -- it computes both
;; from one envelope -- so the only input that separates "the completion
;; checks" from "the completion trusts" is a FORGED plan. That is not a
;; detour around the rule; it is the rule's only discriminating input.
;;
;; The plan is published under the identity a later `working-commit!`
;; will compute, so the retry sees it as its own accepted plan and takes
;; the completion path.

;; `as-bytes`, when given and true, declares the frozen text as its UTF-8
;; bytes -- the spelling a plan for a text-mode block carries since F119.
(define (mismatch-store text-frozen text-named . as-bytes)
  (let* ((bytes? (and (pair? as-bytes) (car as-bytes)))
         (d (string-append root "/mm" (number->string (string-length text-named))
                           (if bytes? "b" "")))
         (who "test"))
    (mkdir-p! d)
    (rpc-dispatch d '(init) "test")
    (let* ((ins (rpc-dispatch d '(insert "--title" "M" "--text" "old") "test"))
           (ev (car (cadr (assq 'events (cdr ins)))))
           (block (block-id (car ev) (cdr ev)))
           (w (car ev)))
      ;; A draft, so the versions are the store's own.
      (rpc-dispatch d (list 'write block text-frozen "--writer" w) "test")
      (let* ((info (assq 'projection
                         (cdr (rpc-dispatch d (list 'read block "--working-info"
                                                    "--writer" w)
                                            "test"))))
             (based-on (list-ref info 5))
             (cut (list-ref info 6))
             (named (draft-version (string->utf8 text-named) based-on cut))
             (after (or (assoc w (reduce-applied-cut (open-and-reduce d))) (cons w 0)))
             ;; The identity `working-commit!` will build for this
             ;; request: its own arguments, then the draft writer, then
             ;; the sorted (block . version) pairs.
             (real (draft-version (string->utf8 text-frozen) based-on cut))
             ;; THE IDENTITY'S ARGUMENTS ARE THE CANONICAL PARTS ONLY:
             ;; the draft writer, then the sorted (block . version)
             ;; pairs. The caller's own argument strings are not in it.
             (args (cons w (list (string-append block "\x0;" real))))
             (fp (request-fingerprint who 'commit args after))
             (plan (list 'plan "MM" fp after
                         (list (cons 0 (list 'set block 'src
                                             (if bytes? (string->utf8 text-frozen) text-frozen))))
                         (list 'consumes w (list (list block named based-on cut)))))
             (actor (list who (cons w "MM") 'plan fp #f after))
             ;; PUBLISHED AS ANOTHER WRITER'S RECORD. A request's
             ;; identity is (cursor-writer . req-id), which the ACTOR
             ;; carries -- the record may sit in any writer's stream.
             ;; Publishing it into this store's own stream, where it
             ;; already has records, is refused as unverifiable.
;; ITS PREMISES ARE THE STORE'S APPLIED CUT, as a real plan's are
             ;; (every record names the whole applied cut). With none, the
             ;; plan's causal cut is empty and every record of the store lies
             ;; outside it, which a completion reads as written since.
             (frame (encode-record 1 1789000000005 actor (reduce-applied-cut (open-and-reduce d)) (storable-encode plan))))
        (log-publish! d "forged00" 1 frame (segment-sha frame))
        (list d block w after real)))))

;; (a) the names disagree: the completion must refuse.
(define mm (mismatch-store "the frozen text" "a different text"))
(define mm-store (list-ref mm 0))
(define mm-block (list-ref mm 1))
(define mm-writer (list-ref mm 2))
(define mm-after (list-ref mm 3))
(define mm-log (string-append (writer-directory mm-store mm-writer) "/000001.sexp"))
(define (mm-bytes)
  (call-with-port (open-file-input-port mm-log) get-bytevector-all))
(define before-mm (mm-bytes))
(define mm-answer
  (working-commit! mm-store mm-writer (list mm-block) "test"
                   (make-write-request "test" 'commit (list mm-block) "MM" mm-after)
                   (list (list-ref mm 4))))
(want "W6-consumes-version-mismatch the completion refuses"
      (list (car mm-answer) (cadr mm-answer) (caddr mm-answer))
      (list 'error 'consumes-version-mismatch (list 'block mm-block)))
(want "W6-consumes-version-mismatch TWIN: and it wrote nothing" (mm-bytes) before-mm)

;; (b) the names agree: the completion goes through.
(define ok-mm (mismatch-store "the frozen text" "the frozen text"))
(define ok-answer
  (working-commit! (list-ref ok-mm 0) (list-ref ok-mm 2) (list (list-ref ok-mm 1)) "test"
                   (make-write-request "test" 'commit (list (list-ref ok-mm 1)) "MM" (list-ref ok-mm 3))
                   (list (list-ref ok-mm 4))))
(want "W6-consumes-version-mismatch TWIN: a matching pair completes"
      (and (pair? ok-answer) (eq? 'ok (car ok-answer))) #t)

;; (c) THE SAME DISAGREEMENT, WITH THE TEXT DECLARED AS BYTES (F119 r1
;; review). A plan for a text-mode block declares its src as bytes since
;; F119; the check read only a string, so this plan completed with a
;; version naming other bytes. Red on f5ebd58 too: there a plan spelled
;; this way was never checked either.
(define mmb (mismatch-store "the frozen text" "a different text" #t))
(define mmb-log (string-append (writer-directory (list-ref mmb 0) (list-ref mmb 2)) "/000001.sexp"))
(define (mmb-bytes)
  (call-with-port (open-file-input-port mmb-log) get-bytevector-all))
(define before-mmb (mmb-bytes))
(define mmb-answer
  (working-commit! (list-ref mmb 0) (list-ref mmb 2) (list (list-ref mmb 1)) "test"
                   (make-write-request "test" 'commit (list (list-ref mmb 1)) "MM" (list-ref mmb 3))
                   (list (list-ref mmb 4))))
(want "W6-consumes-version-mismatch (bytes) the completion refuses a plan that declares bytes"
      (list (car mmb-answer) (cadr mmb-answer) (caddr mmb-answer))
      (list 'error 'consumes-version-mismatch (list 'block (list-ref mmb 1))))
(want "W6-consumes-version-mismatch (bytes) TWIN: and it wrote nothing" (mmb-bytes) before-mmb)

;; ---- W4"-no-draft-read IS NOT HERE, AND THIS IS WHERE IT GOES --------------
;;
;; §7.5.16 says the completion path opens no draft file: "trace on zero
;; `working/` reads". The rows above show that the ANSWER obeys it --
;; the plan's frozen text is what gets carried out, and a replacement
;; draft is neither read for its bytes nor retired. What they do not
;; show is that the file is not opened at all, and it is: `working-commit!`
;; calls `active-entries` before it reaches `with-store-write`, and that
;; opens every envelope in the writer's draft directory.
;;
;; NEVER: THE ANSWERS ARE RIGHT AND THE READS REMAIN. A draft that cannot be
;; read no longer answers ahead of the request's identity -- the failure
;; is caught and carried to `preflight`, which a completion never
;; reaches -- so what is left is a cost and a contract line, not a wrong
;; answer.
;;
;; Removing the reads means deferring the whole `entries` computation
;; until after the verdict, which also makes the `consumes` list a thunk,
;; because it is built from those envelopes. That is a structural change
;; and it is the next batch's. The row that belongs here is a trace
;; assertion: run the retry with THEOURGIA_TRACE on and count zero opens
;; under `working/`.

(printf "rows: ~a\n~a failures\nw4-plan-crash complete\n" rows bad)
