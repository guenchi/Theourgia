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

;; WHICH VERSION A DRAFT WAS EDITED FROM, AND WHO SAYS SO.
;;
;; A draft carries the state it was taken against; that is what lets a
;; second writer be told what the first one did. The question is where
;; that state comes from.
;;
;; NEVER: NOT "NOW". A client reads a block, edits for a while, and saves.
;; If the store recorded the block's hash AT SAVE TIME, then a commit
;; that landed in between becomes the draft's baseline -- and the draft's
;; own commit overwrites it without ever being refused. The client names
;; the version it read, and the store CHECKS that the block really had
;; that hash at that cut.
;;
;; TWO WAYS A NAMED BASELINE FAILS, AND THEY ARE DIFFERENT ANSWERS: the
;; cut cannot be used at all, or the cut is fine and the block never had
;; that hash there. The first leaves the client with an explicit rebase
;; as their only move; the second says they got it wrong.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (theourgia wire) (only (theourgia log) writer-directory))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/baseline-rules-" (number->string (get-process-id))))
(when (file-exists? root) (error 'baseline-rules "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
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
(define (hash id) (block-hash (state) id))
(define (cut-text) (format "~s" (reduce-applied-cut (state))))
(define (src id)
  (cdr (assq 'src (cdr (assq 'fields (state-read (state) id))))))
(define (kind a) (if (and (pair? a) (pair? (cdr a))) (cadr a) a))
(define (reason a) (and (pair? a) (> (length a) 2) (caddr a)))
(define A
  (let ((a (call 'insert "--title" "A" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define (envelope-bytes)
  (let ((p (string-append (writer-directory store writer) "/working/" A)))
    (and (file-exists? p)
         (call-with-port (open-file-input-port p) get-bytevector-all))))

;; ---- BR-01 the first write carries the hash the client READ -------------

(define h0 (hash A))
(define c0 (cut-text))
;; Somebody else commits while our client is editing.
(call 'set A "src" "theirs")
(define h1 (hash A))
(want "BR-01 the block moved" (equal? h0 h1) #f)

(want "BR-01 the first write names the version that was read"
      (rpc-ok? (call 'write A "mine" "--based-on" h0 "--working-cut" c0)) #t)
(define refusal (call 'commit A))
(want "BR-01 so the commit is refused" (kind refusal) 'stale-baseline)
(want "BR-01 and it hands back the commit that got there first"
      (cadr (assq 'now (filter pair? (cdr refusal)))) h1)
(want "BR-01 TWIN: which is not the baseline the draft named"
      (cadr (assq 'based-on (filter pair? (cdr refusal)))) h0)

;; ---- BR-02 a hash the block never had ------------------------------------

(define before-bad (envelope-bytes))
(define never (make-string 64 #\a))
(define bad-answer (call 'write A "mine" "--based-on" never "--working-cut" c0))
(want "BR-02 a baseline the block never had is refused"
      (kind bad-answer) 'invalid-working-baseline)
(want "BR-02 naming which of the two ways it failed" (reason bad-answer) '(reason hash-not-at-cut))
(want "BR-02 TWIN: and it is refused BEFORE anything is written"
      (envelope-bytes) before-bad)

;; ---- BR-03 the cut is the one that is checked ----------------------------
;;
;; (H0, C0) is a true pair and (H0, C1) is not: the block did have h0,
;; but not at the later cut. A build that checked the hash against the
;; CURRENT state would accept the second and reject nothing.

(define c1 (cut-text))
(want "BR-03 TWIN: the two cuts really are different" (equal? c0 c1) #f)
(define wrong-cut (call 'write A "mine" "--based-on" h0 "--working-cut" c1))
(want "BR-03 the same hash at the wrong cut is refused"
      (kind wrong-cut) 'invalid-working-baseline)
(want "BR-03 TWIN: and the same hash at its own cut is accepted"
      (rpc-ok? (call 'write A "mine" "--based-on" h0 "--working-cut" c0)) #t)

;; ---- BR-03b the two ways a named cut fails are different answers --------
;;
;; NOTE: THESE ROWS EXIST BECAUSE A SEEDED CHANGE SURVIVED WITHOUT THEM.
;; Deleting the "the cut cannot be used" arm left every row above green:
;; the arm below it caught everything the rows here reach, so the
;; distinction was implemented and unguarded.

(define before-unusable (envelope-bytes))
(define malformed (call 'write A "mine" "--based-on" h0 "--working-cut" "not-a-cut"))
(want "BR-03b a cut that cannot be read at all says so"
      (list (kind malformed) (reason malformed))
      '(invalid-working-baseline (reason cut-unusable)))
(want "BR-03b TWIN: and nothing was written" (envelope-bytes) before-unusable)

;; NOTE: AND A CUT THAT PARSES BUT NAMES EVENTS THIS STORE DOES NOT HAVE.
;; "not-a-cut" fails at the parser, which is one way in; a well-formed
;; cut naming a writer the store never saw is the other, and a build
;; that tested `(not historical)` instead of `(not (reduction? ...))`
;; would answer differently for it.
(define absent-cut "((\"nosuchwr\" . 9))")
(define unknown-writer (call 'write A "mine" "--based-on" h0 "--working-cut" absent-cut))
(want "BR-03b a well-formed cut this store cannot reduce to also says so"
      (list (kind unknown-writer) (reason unknown-writer))
      '(invalid-working-baseline (reason cut-unusable)))

;; A cut from before this block existed reduces perfectly well and does
;; not contain it -- which is neither "unusable" nor "wrong hash".
(define empty-cut "()")
(define too-early (call 'write A "mine" "--based-on" h0 "--working-cut" empty-cut))
(want "BR-03b a cut the block is not in says THAT"
      (list (kind too-early) (reason too-early))
      '(invalid-working-baseline (reason block-not-at-cut)))
(want "BR-03b TWIN: still nothing written" (envelope-bytes) before-unusable)

;; ---- BR-04 what the envelope recorded ------------------------------------
;;
;; The store is at C1 now; the draft was saved against (h0, C0), and that
;; is what has to be in the envelope. A build that recorded "now" would
;; pass every row above.

(define saved (call 'read A "--working-info"))
(want "BR-04 the envelope records the baseline the client named"
      (list-ref (assq 'projection (cdr saved)) 5) h0)
(want "BR-04 and the cut the client named, not the current one"
      (format "~s" (list-ref (assq 'projection (cdr saved)) 6)) c0)

;; ---- BR-05 a bare rebase -------------------------------------------------

(define before-rebase (envelope-bytes))
(want "BR-05 a bare --rebase is refused"
      (kind (call 'write A "merged" "--rebase")) 'bad-request)
(want "BR-05 naming the rule" (reason (call 'write A "merged" "--rebase")) 'rebase-needs-baseline)
(want "BR-05 TWIN: and the envelope is untouched" (envelope-bytes) before-rebase)

;; ---- BR-06 a rebase that names what it merged onto -----------------------

;; KEY: THE STORE MOVES ON BEFORE THE REBASE, AND THAT IS THE WHOLE ROW.
;;
;; The client merged against (h1, c1). While they were merging, somebody
;; else committed, so the block is at h2 by the time they say `--rebase
;; --based-on h1`. An implementation that validates the named baseline
;; and then records "now" saves h2 -- and the draft's own commit would
;; then overwrite that commit without being refused.
;;
;; NOTE: MEASURED: this row used to rebase onto (h1, c1) while they were
;; STILL CURRENT, and the implementation did record "now" -- the row was
;; green against the defect it was written for.
(call 'set A "src" "theirs again")
(define h2 (hash A))
(want "BR-06 TWIN: the block moved while the client was merging"
      (equal? h1 h2) #f)
(want "BR-06 an explicit rebase onto an older version is accepted"
      (rpc-ok? (call 'write A "merged" "--rebase" "--based-on" h1 "--working-cut" c1)) #t)
(want "BR-06 and the envelope records THAT version, not the current one"
      (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 5) h1)
;; KEY: AND THE CUT IT NAMED, TOO. The hash alone does not pin it: a build
;; that kept the named hash and took the CURRENT cut would satisfy the
;; row above and every refusal below, and its draft would carry a pair
;; that was never true together.
(want "BR-06 and the cut it named, not the current one"
      (format "~s" (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 6)) c1)
(define refusal2 (call 'commit A))
(want "BR-06 so the commit is refused and hands back the newer one"
      (list (kind refusal2) (cadr (assq 'now (filter pair? (cdr refusal2)))))
      (list 'stale-baseline h2))

;; ---- BR-07 no baseline at all -------------------------------------------
;;
;; A `write` with neither flag records "now". NOTE: THAT IS A DECLARATION,
;; not a proof -- the client is saying "I edited from the current text"
;; and nothing checks it. It is kept for the hand-typed case; the rows
;; above are why an editor should not use it.

(call 'discard A)
(want "BR-07 a write with no baseline is accepted" (rpc-ok? (call 'write A "typed")) #t)
(want "BR-07 and it records the block's hash as it is now"
      (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 5) (hash A))
(want "BR-07 TWIN: so it commits without a refusal" (rpc-ok? (call 'commit A)) #t)

(printf "rows: ~a\n~a failures\nbaseline-rules complete\n" rows bad)
