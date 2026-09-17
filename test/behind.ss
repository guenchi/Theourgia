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

;; W11-behind: A COMMIT THAT SUCCEEDED SAYS WHO MOVED UNDER IT.
;;
;; A draft is written against a baseline. By the time its commit lands,
;; other writers may have committed -- and the person who just committed
;; is the one who most wants to know, because they are about to decide
;; whether to test again. The fact costs nothing to produce: both cuts
;; are already in hand when the answer is built.
;;
;; ⛔ IT IS INFORMATION, NOT A VERDICT. It does not refuse, does not
;; hold the commit back, and triggers nothing (§7.5.22, v161: the
;; earlier "refuse when behind" and "dry-run before committing" were
;; both ruled out). A build that turns it into a refusal fails the first
;; row here, which is why that row asserts the commit SUCCEEDED before
;; it looks at the field.
;;
;; ⭐ WHAT THE SEEDS SAY, measured against this file:
;;
;;     make it a refusal                  4 red, first "the commit succeeds"
;;     append the field always            2 red, both twins, `(behind ())`
;;     stop excluding this writer         1 red, naming our own writer
;;     read the opening cut, not the live one
;;                                        1 red, the released writer
;;     keep the field on a replay         1 red, naming all three writers
;;     skip it where nothing is written   1 red
;;     make the helper raise              5 red, every one of them a MISSING
;;                                        field -- and no row about a commit
;;                                        failing. That is the guard: the
;;                                        verb is wrapped in `problem`, so an
;;                                        unguarded raise here would answer
;;                                        `working-unavailable` for a commit
;;                                        whose records are already durable.
;;
;; ⚠️ AND ITS ABSENCE IS PART OF THE SHAPE. A baseline that is not
;; behind gets no `behind` item at all -- not an empty one. The twin
;; below is what makes the first row about the field rather than about
;; the code path that always appends it.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (theourgia wire) (theourgia log))

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
                            "/behind-" (number->string (get-process-id))))
(when (file-exists? root) (error 'behind "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
(define (call . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))
(define (insert title)
  (let ((a (call 'insert "--title" title "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define (cursor-now)
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
(define (version-of id)
  (list-ref (assq 'projection (cdr (call 'read id "--working-info"))) 4))
(define (behind-of answer) (assq 'behind (cdr answer)))

(define A (insert "A"))
(define B (insert "B"))

;; ---- W11-behind: somebody else lands between the draft and the commit --

;; THE DRAFT FIRST, so its baseline is taken before the foreign record.
(call 'write A "a text")
(define a-version (version-of A))

;; ⚠️ THE OTHER WRITER IS A PUBLISHED RECORD, not a second `call`. This
;; store's own writer is the only one `rpc-dispatch` writes as, so the
;; only way to have somebody else's record land after this draft's
;; baseline is to put one in another writer's stream directly.
(define foreign
  (encode-record 1 1789000000010 "someone-else" '()
                 (storable-encode (list 'set B 'src "moved under you"))))
(want "W11-behind the other writer's record publishes"
      (car (log-publish! store "movedzzz" 1 foreign (segment-sha foreign))) 'published)
(want "W11-behind and it really is in the applied cut"
      (cdr (or (assoc "movedzzz" (reduce-applied-cut (state))) '(#f . 0))) 1)

(define answer
  (call 'commit A "--req" "R1" "--cursor" (cursor-now) "--working-version" a-version))

;; ⛔ SUCCEEDED FIRST. A build that refuses when the baseline is behind
;; fails here, before the shape of the field is looked at.
(want "W11-behind the commit succeeds" (rpc-ok? answer) #t)
(want "W11-behind and the block really holds the committed text"
      (cdr (assq 'src (cdr (assq 'fields (state-read (state) A))))) "a text")

(want "W11-behind the answer says who moved"
      (behind-of answer) '(behind (("movedzzz" . 1))))

;; ---- the twin: a baseline that is not behind gets no item ------------
;;
;; ⭐ WITHOUT THIS ROW the one above is also green for a build that
;; appends `(behind ...)` to every commit -- and then the field says
;; nothing, because it is always there.

(call 'write B "b text")
(define b-version (version-of B))
(define fresh-answer
  (call 'commit B "--req" "R2" "--cursor" (cursor-now) "--working-version" b-version))
(want "W11-behind TWIN: the fresh commit succeeds" (rpc-ok? fresh-answer) #t)
(want "W11-behind TWIN: and carries no behind item at all"
      (behind-of fresh-answer) #f)

;; ---- the writer is not behind itself ---------------------------------
;;
;; ⭐ A DECISION, NOT AN ACCIDENT, and this is the row that says so.
;; The rule reads "every writer with records after the baseline", and
;; taken literally that includes THIS writer: commit another block
;; between writing a draft and committing it and your own cut has moved.
;; Telling somebody they are behind themselves is noise in the one place
;; the field is read, so the answer names other writers only. If that is
;; ever reversed, this row is where it shows.

(define C (insert "C"))
(define D (insert "D"))
(call 'write C "c text")
(define c-version (version-of C))

;; OUR OWN CUT MOVES IN BETWEEN.
(call 'write D "d text")
(want "W11-behind the in-between commit of our own succeeds"
      (rpc-ok? (call 'commit D "--req" "R3" "--cursor" (cursor-now)
                     "--working-version" (version-of D))) #t)

(define self-answer
  (call 'commit C "--req" "R4" "--cursor" (cursor-now) "--working-version" c-version))
(want "W11-behind the commit after our own commit succeeds" (rpc-ok? self-answer) #t)
(want "W11-behind TWIN: and our own records do not make us behind"
      (behind-of self-answer) #f)

;; ---- two of them, named in a fixed order ------------------------------
;;
;; ⚠️ SORTED, because the answer is compared byte for byte by the CLI's
;; `--wire` cells and by the daemon's; a set that comes out in hashtable
;; order is a different answer on a different day.

(define E (insert "E"))
(call 'write E "e text")
(define e-version (version-of E))
;; ⚠️ BOTH RECORDS LAND AFTER THIS DRAFT'S BASELINE, and the first
;; version of this section forgot that. `movedzzz`'s earlier record was
;; already in the cut when E's draft was written, so it is not behind
;; anything -- the answer named one writer and the row wanted two. The
;; second record from the SAME writer is what puts it back in the list,
;; and it also makes the row about the SEQ: what is reported is that
;; writer's current one (2), not the one the baseline missed (1).
(define foreign2
  (encode-record 1 1789000000011 "someone-else" '()
                 (storable-encode (list 'set B 'src "and another"))))
;; ⚠️ SEQ 2, AND SEGMENT 2. `encode-record`'s first argument is the
;; record's SEQUENCE and `log-publish!`'s third is the SEGMENT; the
;; first version of this record reused seq 1, which is the same sequence
;; with different bytes and is exactly what the publish path calls a
;; divergence. It also declares its predecessor: a second record in a
;; writer's own stream with an empty past has forked rather than
;; continued.
(define foreign3
  (encode-record 2 1789000000012 "someone-else" '(("movedzzz" . 1))
                 (storable-encode (list 'set B 'src "moved again"))))
;; ⚠️ EIGHT CHARACTERS. A writer id is exactly 8 base36 characters
;; (log.ss `writer-id?`), and `log-publish!` answers `refused` to
;; anything else -- which is what the first version of this row got,
;; with a nine-letter name chosen only because it sorts first.
(want "W11-behind a second foreign writer publishes"
      (car (log-publish! store "aaaamovd" 1 foreign2 (segment-sha foreign2))) 'published)
(want "W11-behind and the first one moves again"
      (car (log-publish! store "movedzzz" 2 foreign3 (segment-sha foreign3))) 'published)
(define two-answer
  (call 'commit E "--req" "R5" "--cursor" (cursor-now) "--working-version" e-version))
(want "W11-behind the commit with two of them succeeds" (rpc-ok? two-answer) #t)
(want "W11-behind both are named, in writer order"
      (behind-of two-answer) '(behind (("aaaamovd" . 1) ("movedzzz" . 2))))

;; ---- the writer our own write releases --------------------------------
;;
;; ⭐ THIS IS WHY THE CUT COMES FROM THE WRITE, NOT FROM THE OPEN. A
;; foreign record can be waiting on a record THIS WRITER has not made
;; yet: it is in the store, it is not applied, and the reduction this
;; verb opened with does not have it. Appending this commit's own
;; records releases it -- so by the time the answer is built that writer
;; HAS landed after the baseline, and an answer taken from the opening
;; cut omits it. That is the field being wrong, not merely stale.
;;
;; Reported by codex (E step 0, finding 1) with this mechanism; the row
;; is the mechanism made reachable.

(define G (insert "G"))
(call 'write G "g text")
(define g-version (version-of G))
(define next-seq
  (+ 1 (cdr (assoc writer (reduce-applied-cut (state))))))
(define pending
  (encode-record 1 1789000000013 "someone-else"
                 (list (cons writer next-seq))
                 (storable-encode (list 'set B 'src "released by your own record"))))
(want "W11-behind the waiting record publishes"
      (car (log-publish! store "waitszzz" 1 pending (segment-sha pending))) 'published)
;; ⛔ AND IT IS NOT APPLIED YET. Without this the row below would pass
;; for a build that reads the opening cut, because the record would
;; already be in it.
(want "W11-behind and it is NOT in the applied cut before the commit"
      (assoc "waitszzz" (reduce-applied-cut (state))) #f)

;; ⚠️ THE CURSOR IS KEPT, because the retry below has to be the SAME
;; request. A resend with a fresh cursor is a DIFFERENT request that
;; reuses an id, and the store says so: measured, `(error req-mismatch
;; ...)`. Request identity is taken over the cursor and the versions,
;; not over the id alone.
(define g-cursor (cursor-now))
(define released-answer
  (call 'commit G "--req" "R6" "--cursor" g-cursor "--working-version" g-version))
(want "W11-behind the commit that releases it succeeds" (rpc-ok? released-answer) #t)
(want "W11-behind and the released writer is named"
      (behind-of released-answer) '(behind (("waitszzz" . 1))))

;; ---- a replay is not asked who moved ----------------------------------
;;
;; ⛔ ITS DRAFTS ARE GONE, so `entries` is empty and an empty baseline
;; makes every writer in the store look like a mover. The commit this
;; repeats already answered the question.

(define replay-answer
  (call 'commit G "--req" "R6" "--cursor" g-cursor "--working-version" g-version))
(want "W11-behind the replay succeeds" (rpc-ok? replay-answer) #t)
(want "W11-behind TWIN: and it really is a replay"
      (exists (lambda (a) (equal? '(replay #t) (assq 'replay (cdr a))))
              (cdr (assq 'items (cdr replay-answer))))
      #t)
(want "W11-behind TWIN: a replay carries no behind item"
      (behind-of replay-answer) #f)

;; ---- two drafts, one baseline: the JOIN ------------------------------
;;
;; A commit can name several drafts, written at different times, so
;; there is no single baseline -- the rule says the JOIN of theirs, and
;; a join takes the greater sequence per writer.
;;
;; ⚠️ WHICH MEANS THE JOIN CAN HIDE THE OLDER DRAFT'S STALENESS: commit
;; an old draft together with a fresh one and the writer that moved
;; between them is NOT named, because the fresh draft's baseline covers
;; it. That is what the rule says and the pair of rows below pins both
;; halves -- the same old draft committed alone DOES name it.

(define H (insert "H"))
(define I (insert "I"))
(call 'write H "h text")
(define h-version (version-of H))
(define third
  (encode-record 1 1789000000014 "someone-else" '()
                 (storable-encode (list 'set B 'src "between the two drafts"))))
(want "W11-behind the third writer publishes"
      (car (log-publish! store "betwnzzz" 1 third (segment-sha third))) 'published)
(call 'write I "i text")
(define i-version (version-of I))

;; ⚠️ `<block>=<version>` WHEN THERE IS MORE THAN ONE. A bare version is
;; only unambiguous for a single block; with two ids the parser answers
;; `(error bad-request malformed-working-version)`, which is what the
;; first version of this row got.
(define joined
  (call 'commit H I "--req" "R7" "--cursor" (cursor-now)
        "--working-version" (string-append H "=" h-version)
        "--working-version" (string-append I "=" i-version)))
(want "W11-behind the two-draft commit succeeds" (rpc-ok? joined) #t)
(want "W11-behind and the join covers the writer that moved between them"
      (behind-of joined) #f)

;; THE OTHER HALF: the same old draft on its own does name it.
(define H2 (insert "H2"))
(call 'write H2 "h2 text")
(define h2-version (version-of H2))
(define fourth
  (encode-record 1 1789000000015 "someone-else" '()
                 (storable-encode (list 'set B 'src "after the lone draft"))))
(want "W11-behind the fourth writer publishes"
      (car (log-publish! store "lonelzzz" 1 fourth (segment-sha fourth))) 'published)
(define lone
  (call 'commit H2 "--req" "R8" "--cursor" (cursor-now) "--working-version" h2-version))
(want "W11-behind the lone commit succeeds" (rpc-ok? lone) #t)
(want "W11-behind and alone it does name the writer that moved"
      (behind-of lone) '(behind (("lonelzzz" . 1))))

;; ---- the commit that writes nothing at all ---------------------------
;;
;; ⭐ A SECOND SUCCESS BRANCH, AND IT USED TO SKIP THE FIELD. A commit
;; with no `--req` whose drafts produce no sub-operations writes no
;; record -- and still retires the drafts, so it is a commit and owes
;; the same fact. Reported by codex (E step 0, finding 6).
;;
;; The draft here is byte-identical to what the block already holds, so
;; there is nothing to write; `drafts` calls such an entry `unchanged`.

(define K (insert "K"))
(call 'write K "old")
;; ⚠️ `unchanged` IS A FIELD, NOT A KIND. Every active entry comes back
;; as `draft`; whether it would write anything is `(unchanged #t)`
;; inside it. The first version of this row asked for a kind and got
;; `(draft)`, which says nothing about the arm being exercised.
(want "W11-behind the draft would write nothing"
      (let ((mine (car (filter (lambda (d) (equal? K (cadr (assq 'block (cdr d)))))
                               (cdr (assq 'items (cdr (call 'drafts))))))))
        (list (car mine) (assq 'unchanged (cdr mine))))
      '(draft (unchanged #t)))
(define fifth
  (encode-record 1 1789000000016 "someone-else" '()
                 (storable-encode (list 'set B 'src "while nothing was written"))))
(want "W11-behind the fifth writer publishes"
      (car (log-publish! store "nowrtzzz" 1 fifth (segment-sha fifth))) 'published)
(define nothing-written (call 'commit K))
(want "W11-behind the commit that writes nothing succeeds" (rpc-ok? nothing-written) #t)
(want "W11-behind TWIN: and it wrote no items"
      (cdr (assq 'items (cdr nothing-written))) '())
(want "W11-behind and it still says who moved"
      (behind-of nothing-written) '(behind (("nowrtzzz" . 1))))

(printf "rows: ~a\n~a failures\nbehind complete\n" rows bad)
