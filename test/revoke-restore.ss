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

;; A CONSUMPTION TAKEN BACK, AND THE TEXT THAT GOES WITH IT.
;;
;; A commit retires the drafts it consumed: the files are deleted. If a
;; second record then claims that commit's identity, the plan goes into
;; conflict, its members are undone, and the consumption is REVOKED --
;; the commit is no longer in the state, and the files are still gone.
;;
;; From the person's side their work has vanished. It has not: the plan
;; record froze the text. `drafts` says so, and `restore` puts it back.
;;
;; ⛔ THE FILE IS NOT RE-CREATED BY A READ. Whether to bring a draft back
;; is the person's decision; a store that wrote files during `drafts`
;; would be writing on a path nobody asked to write on.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (theourgia request) (theourgia wire) (theourgia log))

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
                            "/revoke-restore-" (number->string (get-process-id))))
(when (file-exists? root) (error 'revoke-restore "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
(define (call . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))
(define A
  (let ((a (call 'insert "--title" "A" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define (src id)
  (let* ((b (state-read (state) id)) (p (and b (assq 'src (cdr (assq 'fields b))))))
    (and p (cdr p))))
(define (draft-file) (string-append (writer-directory store writer) "/working/" A))
(define (items)
  (let ((a (call 'drafts))) (and (rpc-ok? a) (cdr (assq 'items (cdr a))))))
(define (of-kind k) (filter (lambda (d) (eq? k (car d))) (items)))

;; ---- a commit, and then a second record claiming its identity -------------

(call 'write A "the text I wrote")
(define v1 (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 4))
(define cursor
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
(want "RR-00 the commit succeeds"
      (rpc-ok? (call 'commit A "--req" "R1" "--cursor" cursor "--working-version" v1)) #t)
(want "RR-00 the draft file is gone" (file-exists? (draft-file)) #f)
(want "RR-00 and the text is committed" (src A) "the text I wrote")
(want "RR-00 drafts lists nothing" (items) '())

;; A SECOND RECORD WITH THE SAME IDENTITY. `publish` puts a record into
;; another writer's stream; two plans claiming one identity are a
;; conflict, and neither is applied.
(define evidence (store-evidence store (cons writer "R1")))
(define plan-ev (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) evidence))

;; A SECOND RECORD CLAIMING THE SAME IDENTITY, published into another
;; writer's stream. Two plans for one identity are a conflict; neither is
;; applied, and the members of both are undone.
(define rival
  (encode-record 1 1789000000001 (ev-actor plan-ev) '()
                 (storable-encode (ev-payload plan-ev))))
(want "RR-01 the rival record publishes"
      (car (log-publish! store "rivalzzz" 1 rival (segment-sha rival))) 'published)

;; TWO PLANS CLAIMING ONE IDENTITY ARE BOTH IN CONFLICT, and the member
;; of the original is left waiting for a plan that will never be applied
;; -- which is `pending-plan` and is correct, not a third conflict.
(want "RR-01 both plans are in conflict"
      (length (filter (lambda (g) (eq? 'plan-conflict (cdr g))) (reduce-gates (state)))) 2)
(want "RR-01 and the orphaned member is pending, not applied"
      (length (filter (lambda (g) (eq? 'pending-plan (cdr g))) (reduce-gates (state)))) 1)
(want "RR-01 so the commit is no longer in the state" (src A) "old")
(want "RR-01 and the file is still gone" (file-exists? (draft-file)) #f)

;; ---- what drafts says now -------------------------------------------------

(want "RR-02 drafts lists it as revoked, not as a draft"
      (map car (items)) '(revoked))
(want "RR-02 naming the block and the version"
      (let ((d (car (of-kind 'revoked))))
        (list (cadr (assq 'block (cdr d))) (cadr (assq 'version (cdr d)))))
      (list A v1))
(want "RR-02 TWIN: and reading it did not put the file back"
      (file-exists? (draft-file)) #f)

;; ---- restore --------------------------------------------------------------

;; ⭐ `--writer` ON THIS VERB, WHICH DID NOT WORK UNTIL IT WAS MEASURED.
;; The handler reads `--writer`, but the option table did not list it for
;; `restore`, so the token parsed as a POSITIONAL: the call arrived with
;; three positionals, failed the arity check, and answered a usage form.
;; A version could never be restored into a named writer's slot from the
;; command line.
;;
;; ⛔ EVERY ROW BELOW REACHED THE HANDLER WITHOUT EVER PASSING THIS
;; OPTION, which is why they were all green. `rpc-dispatch` does parse
;; its arguments -- these rows go through the same reader the command
;; line does -- so the gap was not the route, it was that nothing here
;; ever spelled `--writer` on a `restore`.

;; A version that does not exist, so nothing is consumed: what this row
;; reads is WHICH ANSWER comes back. `unknown-version` is the handler's;
;; a usage form would mean the call never got there.
(want "RR-03a --writer reaches the handler rather than becoming a positional"
      (call 'restore "no-such-version" "--writer" writer)
      '(error unknown-version "no-such-version"))

;; ⛔ TWIN: ACCEPTED IS NOT THE SAME AS READ. A parser that took the
;; option and a handler that ignored it would pass the row above. The
;; revoked entry belongs to one writer, so naming a different one has to
;; answer differently for the SAME version -- and the row after this one
;; restores that very version under the default writer, which is what
;; makes the pair a discriminator rather than two readings of nothing.
(want "RR-03a TWIN: naming another writer searches another writer's list"
      (call 'restore v1 "--writer" "somebody-else")
      (list 'error 'unknown-version v1))

(want "RR-03 restore succeeds" (rpc-ok? (call 'restore v1)) #t)
(want "RR-03 the bytes are the ones the plan froze"
      (call 'read A "--working") '(ok (text "the text I wrote")))
(want "RR-03 it is a draft again, not a revoked entry" (map car (items)) '(draft))
(want "RR-03 under the version it had"
      (cadr (assq 'version (cdr (car (of-kind 'draft))))) v1)

;; ⛔ AND ON THE BASELINE IT HAD, not on `now`. The block went back to
;; "old" when the commit was revoked, and the draft was written against
;; the hash the block had BEFORE that commit -- which is the same one.
;; The row that matters is that the version recomputes: a restore onto a
;; different baseline would hash to a different name.
(want "RR-03 TWIN: an unknown version is refused"
      (cadr (call 'restore "0000")) 'unknown-version)

;; ---- RR-03b the baseline it HAD, with a different one available -----------
;;
;; ⚠️ RR-03 SAID "on the baseline it had, not on now" AND COULD NOT SEE
;; THE DIFFERENCE: the revocation put the block back to "old", so the
;; baseline it had and the baseline now were the same hash. A build that
;; ignored the record and used the current block hash passed it.
;;
;; So the block is moved on first -- by another writer's record, for the
;; reason given below -- and it lands as a concurrent edit, which leaves
;; `src` contested rather than replaced. Either way the block's hash is
;; a different value than the one the revoked record names, which is all
;; this needs: the two candidate baselines are now distinguishable and
;; the answer says which one was used.

;; ⚠️ THE LATER EDIT COMES FROM SOMEONE ELSE, AND IT HAS TO. This
;; writer's own stream is stopped: R1's plan is in conflict, so its
;; records are present and unapplied, and every further record of this
;; writer's would have a predecessor that never applies -- measured,
;; `(error no-view predecessor-not-applied)`. Another writer's record
;; is under no such rule, and moving the block is all this needs.
(call 'discard A)
(define later
  (encode-record 1 1789000000003 "someone-else" '()
                 (storable-encode (list 'set A 'src "a later text"))))
(want "RR-03b a later record from another writer publishes"
      (car (log-publish! store "laterzzz" 1 later (segment-sha later))) 'published)
;; ⚠️ THE RECORD NAMES NO DEPENDENCIES, so it is concurrent with the
;; edit that is already there and `src` becomes contested rather than
;; replaced. That is not a problem for this section: what it needs is
;; for the block to MOVE, and a contested field moves it just as well.
(want "RR-03b and the block has moved -- its src is contested now"
      (and (pair? (src A)) (car (src A))) 'conflict)

(define recorded-based-on
  (let ((r (find (lambda (r) (equal? v1 (cadr (car r)))) (state-revoked (state) writer))))
    (and r (caddr (car r)))))
(define current-baseline
  (let ((b (begin (call 'write A "scratch")
                  (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 5))))
    (call 'discard A)
    b))
(want "RR-03b both baselines are real hashes"
      (map (lambda (x) (and (string? x) (string-length x)))
           (list recorded-based-on current-baseline))
      '(64 64))
(want "RR-03b and they are now different values"
      (equal? recorded-based-on current-baseline) #f)

(define restored (call 'restore v1))
(want "RR-03b the restore succeeds" (rpc-ok? restored) #t)
(want "RR-03b and it used the baseline in the record, not the current one"
      (cadr (assq 'based-on (cdr restored))) recorded-based-on)
(want "RR-03b so the draft still has the version it had"
      (cadr (assq 'version (cdr (car (of-kind 'draft))))) v1)

;; ---- RR-04 a restore that refuses writes nothing --------------------------
;;
;; `restore` recomputes the version from the text it is about to use and
;; refuses when it disagrees with the record. ⚠️ RR-03 DID NOT GUARD THE
;; ORDER: its only refusal was an unknown version, which exits before the
;; check runs at all, so moving the check after the write -- or removing
;; it -- left every row unchanged.
;;
;; The store's draft directory is compared byte for byte across a
;; refused restore.

(define (draft-dir-state)
  (let ((dir (string-append (writer-directory store writer) "/working")))
    (if (not (file-exists? dir)) '()
        (list-sort (lambda (a b) (string<? (car a) (car b)))
          (map (lambda (n)
                 (cons n (call-with-port (open-file-input-port (string-append dir "/" n))
                           get-bytevector-all)))
               (filter (lambda (n) (not (member n '("." ".."))))
                       (directory-entries dir)))))))

(call 'discard A)
(define before-refused (draft-dir-state))
(want "RR-04 an unknown version is refused" (cadr (call 'restore "0000")) 'unknown-version)
(want "RR-04 TWIN: and the drafts are byte-identical afterwards"
      (draft-dir-state) before-refused)

;; A version that IS known but whose recorded text does not hash to it.
;; The record is forged: a real `write` computes both from one envelope,
;; so only a forged one can disagree.
(want "RR-04 TWIN: the restore that does agree still works"
      (rpc-ok? (call 'restore v1)) #t)

;; ---- RR-05 a version whose recorded text does not hash to it --------------
;;
;; ⭐ THE CHECK ABOVE HAS NOTHING BEHIND IT UNTIL THIS. Every refusal so
;; far is `unknown-version`, which exits before `restore` recomputes
;; anything -- so deleting the recomputation, or moving it after the
;; write, left every row green. What is needed is a version that IS
;; known and whose text does NOT produce it.
;;
;; A real write cannot make one: the version and the bytes come from one
;; envelope, computed together. So the record is forged -- a plan
;; claiming the same identity as R1, which puts it in conflict and its
;; consumption in the revoked list, naming a version this store has
;; never computed and sub-operations whose text is something else
;; entirely. `restore` is then asked for that version.

(define fake-version
  "aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffff00000000111111112")
(define real-consumes (list-ref (ev-payload plan-ev) 5))
(define forged-payload
  (list 'plan (list-ref (ev-payload plan-ev) 1) (list-ref (ev-payload plan-ev) 2)
        (list-ref (ev-payload plan-ev) 3)
        (list (list 0 'set A 'src "text that is not what the version names"))
        (list 'consumes (cadr real-consumes)
              (list (list A fake-version
                          (list-ref (car (caddr real-consumes)) 2)
                          (list-ref (car (caddr real-consumes)) 3))))))
(define forged
  (encode-record 1 1789000000002 (ev-actor plan-ev) '()
                 (storable-encode forged-payload)))
(want "RR-05 the forged record publishes"
      (car (log-publish! store "forgedzz" 1 forged (segment-sha forged))) 'published)
;; ⚠️ ASKED OF `state-revoked`, NOT OF `drafts`. The two are not the
;; same list: `drafts` is about what this writer has in hand, and block
;; A has a draft file again by now, so the entry does not appear there.
;; `restore` reads `state-revoked`, and that is the list the version has
;; to be in for the refusal below to be the version check rather than
;; `unknown-version`.
(want "RR-05 and the version it names is a version the store knows"
      (exists (lambda (r) (equal? fake-version (cadr (car r))))
              (state-revoked (state) writer))
      #t)

(define before-mismatch (draft-dir-state))
(want "RR-05 restoring it is refused, and says which check refused"
      (cadr (call 'restore fake-version)) 'consumes-version-mismatch)
;; ⛔ AND THE REFUSAL CAME FIRST. A build that wrote the draft and then
;; checked would answer this row exactly the same way.
;; COMPARED AS A BOOLEAN, because the drafts are whole encoded
;; envelopes and a red row that printed both would print two kilobytes
;; of bytevector for a one-bit fact.
(want "RR-05 TWIN: nothing was written before the refusal"
      (equal? (draft-dir-state) before-mismatch) #t)

(printf "rows: ~a\n~a failures\nrevoke-restore complete\n" rows bad)
