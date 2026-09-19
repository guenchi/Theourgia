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

;; W3: what a stale refusal HANDS BACK.
;;
;; THIS IS THE MECHANISM, NOT A DETAIL OF THE MESSAGE. The whole reason
;; a draft carries the state it was taken against is so that a second
;; writer can be told what the first one did -- design.md 7.5.5: a block
;; stands at commit5; the first writer commits and it becomes commit6; the
;; second arrives holding commit5, fails the check, is handed commit6's
;; commit information, and merges before committing again. The W brief's
;; line for the cells: a stale premise is refused, the log is unchanged
;; byte for byte, and the refusal names the first committer's record id,
;; actor and content. The core does not merge and does not rebase;
;; handing the fact back IS the feature. A refusal that said only
;; `stale-baseline` would be correct and useless, and nothing asserted
;; otherwise: `working1.ss` checks the refusal's KIND and its `block`,
;; and every other fixture stops there, so emptying `since` broke
;; nothing that anyone was measuring.
;;
;; AND THE ANSWER IS BOUNDED, which is the other half of the same
;; sentence: it is bounded. When many records have landed in between,
;; `since` gives the list of identities and actors plus the current
;; content, and the rest is fetched with log, diff and read -- the size of
;; a refusal must not grow with the length of the history. The
;; design fixes that a bound exists and that the answer says where the
;; rest is; the two numbers below (8 records, 8192 bytes) are
;; `baseline.ss`'s, and the rows name them so that changing one is a
;; decision somebody makes rather than a drift nobody sees.

(import (chezscheme) (theourgia rpc) (theourgia store)
        (theourgia reduce) (theourgia ffi) (theourgia wire))

(define failures 0)
(define rows-run 0)
;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE -- and both
;; sides are guarded, because a row whose expectation is derived from
;; the store's own earlier answer can fail to compute just as the answer
;; under test can. See q7.ss for the reading that produced this rule.
;;
;; NOTE: AND A RAISE IS A FAILURE EVEN WHEN BOTH SIDES RAISE THE SAME WAY.
;; The form this was copied from wraps each side and then compares the
;; two wrapped values, so two identical raises are `equal?` and the row
;; prints `ok`. That is not a corner: both sides of several rows here go
;; through `since-of`, so one malformed refusal makes them raise
;; together. Measured on this file with `refusal6` replaced by `#f`: the
;; row "every entry that fits is delivered complete" printed `ok` while
;; neither side had a value. The outcome is therefore carried beside the
;; value instead of being folded into it, and a row is `ok` only when
;; BOTH sides returned.
(define (caught-value a) (cdr a))
(define (caught-ok? a) (eq? 'returned (car a)))
(define (want-1 name actual expected)
  (cond
    ((not (caught-ok? actual))
     (set! failures (+ failures 1))
     (printf "FAIL ~a: the observation raised: ~s\n" name (caught-value actual)))
    ((not (caught-ok? expected))
     (set! failures (+ failures 1))
     (printf "FAIL ~a: the expectation raised: ~s\n" name (caught-value expected)))
    ((equal? (caught-value actual) (caught-value expected))
     (printf "ok ~a\n" name))
    (else
     (set! failures (+ failures 1))
     (printf "FAIL ~a: ~s WANT ~s\n" name (caught-value actual) (caught-value expected)))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (cons 'raised
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       (cons 'returned e0)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/working3-" (number->string (get-process-id))))
(when (file-exists? root) (error 'working3 "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define n-store 0)
;; KEY: EACH STORE'S OWN WRITER IS REMEMBERED AT `init`, BECAUSE A DRAFT
;; VERB IS NO LONGER TOLD ONE BY DEFAULT. It used to fall back to the
;; store's local log writer, so every row below asked its question as
;; that writer without saying so; the core now refuses an unnamed writer
;; outright. Naming the same one keeps the refusals these rows are about
;; -- `no-draft`, `stale-baseline`, the count bound -- the refusals they
;; get, instead of replacing all of them with one about identity.
;;
;; NEVER: AND A STORE WHOSE WRITER WAS NEVER RECORDED IS AN ERROR, NOT A
;; FALLBACK TO #f. Silently omitting the option would turn whichever row
;; used that store into a test of `writer-required`, and it would still
;; be a refusal, and the row would still look like it was working.
(define store-writers '())

(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (mkdir-p! d)
    (let ((a (rpc-dispatch d '(init) "test")))
      (set! store-writers
            (cons (cons d (cadr (assq 'writer (cdr a)))) store-writers)))
    d))

(define (writer-of store)
  (let ((hit (assoc store store-writers)))
    (if hit
        (cdr hit)
        (error 'working3 "no writer was recorded for this store" store))))

(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call store args actor)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" (writer-of store)))
                    args)
                actor))
(define (insert! store title)
  (let* ((a (call store (list 'insert "--title" title "--text" "old") "test"))
         (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))
;; THE RECORD ID COMES FROM THE WRITER'S OWN ANSWER, not from a literal
;; and not from the refusal. Two independent paths through the store
;; then have to agree about which record was written; a row that read
;; the id out of the refusal and compared it with itself would pass
;; against any refusal at all.
(define (commit-by! store actor id text)
  (let ((a (call store (list 'set id "src" text) actor)))
    (unless (rpc-ok? a) (error 'working3 "the earlier commit did not happen" a))
    (car (cadr (assq 'events (cdr a))))))
(define (fields answer) (filter pair? (cdr answer)))
(define (field answer key) (assq key (fields answer)))
(define (since-of answer)
  (let ((p (field answer 'since))) (if p (cdr p) 'no-since-field)))
(define (kind answer) (if (and (pair? answer) (pair? (cdr answer))) (cadr answer) answer))
(define (encoded-size value)
  (bytevector-length (string->utf8 (sexpr->string-extended (storable-encode value)))))
(define (since-bytes answer) (apply + (map encoded-size (since-of answer))))

(printf "\n== one earlier committer ==\n")
;; The common case the design names: exactly one writer got there first,
;; and the loser must be able to merge without asking a second question.
(define d1 (fresh-store!))
(define a1 (insert! d1 "A"))
(call d1 (list 'write a1 "mine") "test")
(define first-record (commit-by! d1 "agent:first" a1 "theirs"))
(define refusal (call d1 (list 'commit a1) "test"))

(want "W3-01 a commit against a superseded state is refused"
      (kind refusal) 'stale-baseline)
(want "W3-01 the refusal names the block"
      (field refusal 'block) (list 'block a1))
(want "W3-02 since carries exactly one earlier commit"
      (length (since-of refusal)) 1)
(want "W3-03 the entry's first position is the earlier committer's record id"
      (car (car (since-of refusal))) first-record)
(want "W3-04 the entry's second position is the earlier committer"
      (cadr (car (since-of refusal))) "agent:first")
(want "W3-05 the entry's third position is what they committed"
      (caddr (car (since-of refusal))) (list 'set a1 'src "theirs"))
;; AND THE WHOLE ENTRY AT ONCE, so that a rearrangement of the three
;; positions -- each of which is checked above against a different
;; source -- cannot pass by satisfying them out of order.
(want "W3-05 the entry is those three positions in that order"
      (car (since-of refusal))
      (list first-record "agent:first" (list 'set a1 'src "theirs")))

;; TWIN: THE REFUSAL IS NOT UNCONDITIONAL. Without this row a store that
;; refused every commit, naming whatever it liked, would pass the rows
;; above; and a `since` that simply echoed the whole history would too.
(define d2 (fresh-store!))
(define a2 (insert! d2 "A"))
(call d2 (list 'write a2 "mine") "test")
(want "W3-06 TWIN: a commit whose state is current is accepted"
      (rpc-ok? (call d2 (list 'commit a2) "test")) #t)
(want "W3-06 TWIN: and the accepted commit is what is now stored"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce d2) a2))))) "mine")

(printf "\n== records that are not about this block ==\n")
;; TWIN FOR THE FILTER: `since` is what happened TO THIS BLOCK. A
;; refusal that handed back every record written since the draft would
;; satisfy every row above -- the entry it is asked about is in there --
;; and would grow without bound with a store that is merely busy.
(define d3 (fresh-store!))
(define a3 (insert! d3 "A"))
(define b3 (insert! d3 "B"))
(call d3 (list 'write a3 "mine") "test")
(define elsewhere (commit-by! d3 "agent:elsewhere" b3 "not about A"))
(define mine-record (commit-by! d3 "agent:first" a3 "theirs"))
(define refusal3 (call d3 (list 'commit a3) "test"))
(want "W3-07 a commit to another block does not enter since"
      (since-of refusal3)
      (list (list mine-record "agent:first" (list 'set a3 'src "theirs"))))
(want "W3-07 and the record that was skipped really was written"
      (list (car elsewhere) (< (cdr elsewhere) (cdr mine-record))) (list (car mine-record) #t))

(printf "\n== the bound, and the answer that says where the rest is ==\n")
;; A refusal under the bound must NOT wear the marks of a truncated one.
;; Without these two rows an implementation that always said `truncated`
;; and always pointed at the log would pass the truncation rows below,
;; and the loser would fetch a history they already had in full.
(want "W3-08 an answer under the bound is not marked truncated"
      (field refusal 'truncated) #f)
(want "W3-09 an answer under the bound carries no retrieval instruction"
      (field refusal 'retrieve) #f)

(define d4 (fresh-store!))
(define a4 (insert! d4 "A"))
(define b4 (insert! d4 "B"))
(call d4 (list 'write a4 "mine") "test")
;; Ten commits on the block, and one on another block in the middle of
;; them, so that the window's contents show the filter as well as the
;; count: the record between the fifth and sixth is not about this block
;; and has to be absent from a window that otherwise spans it.
(define many
  (let loop ((i 1) (out '()))
    (if (> i 10) (reverse out)
        (let ((ev (commit-by! d4 (string-append "agent:" (number->string i)) a4
                              (string-append "v" (number->string i)))))
          (when (= i 5) (commit-by! d4 "agent:other" b4 "unrelated"))
          (loop (+ i 1) (cons ev out))))))
(define refusal4 (call d4 (list 'commit a4) "test"))
(want "W3-10 ten earlier commits are cut to eight entries"
      (length (since-of refusal4)) 8)
;; NEVER: W3-11's FIRST ROW IS RETIRED. It asserted the eight in INGESTION
;; order, newest first. §7.5.11 orders them causally instead -- the
;; winner first, then the rest as a reader would apply them -- because
;; ingestion order is an accident of who synced first, and the same two
;; records delivered the other way round would otherwise give a
;; different answer to the same question.
;;
;; SUCCESSOR: `since-contract.ss`, SC-03 (a -> b -> z, two ingestion
;; orders, one answer) and SC-01/SC-02 (the winner is first and is the
;; newest).
;;
;; THE SECOND ROW SURVIVES: that every entry is about this block is
;; still the rule, and nothing else asserts it here.
(want "W3-11 the eight are the newest eight of the ten"
      (list-sort string<? (map (lambda (e) (number->string (cdr (car e)))) (since-of refusal4)))
      (list-sort string<? (map (lambda (e) (number->string (cdr e))) (list-tail many 2))))
(want "W3-11 and each of them is about this block"
      (map (lambda (e) (cadr (caddr e))) (since-of refusal4))
      (make-list 8 a4))
(want "W3-12 a cut answer says so"
      (field refusal4 'truncated) '(truncated #t))
(want "W3-13 a cut answer says where the rest is"
      (field refusal4 'retrieve) (list 'retrieve (list 'log a4) (list 'read a4)))

(printf "\n== the byte budget is retired; see since-contract.ss ==\n")
;; NEVER: EVERY ROW FROM W3-14 TO W3-33 IS RETIRED, AND THIS IS WHERE THEY
;; WERE. Their oracle was one rule: "take entries, newest first, until a
;; running byte total is used up; an entry that does not fit is passed
;; over." Each row measured a consequence of it -- the total is
;; cumulative rather than per entry, a passed-over entry does not
;; consume a place, quotation marks cost what they encode to, and so on.
;;
;; §7.5.11 and §7.5.16 replace the rule. The bound is eight entries, and
;; an entry is NEVER dropped for its size: a content over 1024 bytes is
;; handed back as `(content elided <bytes>)` with its identity and its
;; actor intact, and it takes its slot. The reason is the case these
;; rows were green for: under the byte budget, a refusal whose single
;; relevant commit was long came back with an EMPTY `since` and a
;; `truncated` mark -- the one record the reader needed had been dropped
;; for being long, and the answer told them to go and look it up.
;;
;; NEVER: THE EXPECTATIONS ARE NOT EDITED TO MATCH THE NEW RULE. A row whose
;; question no longer exists does not have a new answer; it has a
;; successor that asks the question the new rule is about.
;;
;; SUCCESSORS, by name, in `since-contract.ss`:
;;
;;   W3-14, W3-15        -> SC-04 (an oversized winner keeps its entry,
;;                          content elided, not truncated)
;;   W3-16, W3-18, W3-27,
;;   W3-28, W3-29, W3-30,
;;   W3-31               -> SC-06 (a big one among small ones costs no
;;                          other entry its place) and SC-07 (nine of
;;                          mixed size give eight, big ones taking slots)
;;   W3-17               -> SC-01 (unrelated records do not enter the
;;                          answer) -- also still covered by W3-21 below
;;   W3-22               -> SC-08 (a long actor is preserved whole)
;;   W3-23, W3-32, W3-33 -> SC-05 (the limit is on BYTES: a multi-byte
;;                          content under the limit in characters is
;;                          elided, and one under it in bytes is not)
;;   W3-24, W3-25, W3-26 -> SC-05's boundary rows
;;
;; W3-19, W3-20 and W3-21 below are NOT retired: the count bound, what
;; marks an answer truncated, and "a record about another block is not
;; an omission" are all still the rule.

(printf "\n== the count bound, and what makes an answer truncated ==\n")
;; THE PAIR IS WHAT SEPARATES TWO READINGS OF `truncated`. Eight earlier
;; commits and nine differ by one record; under "the log holds more" both
;; are marked, under "an entry was left out" only the second is. A single
;; row at either number is passed by both readings.
(define (commits-then-refuse n)
  (let* ((d (fresh-store!))
         (a (insert! d "A")))
    (call d (list 'write a "mine") "test")
    (let loop ((i 1))
      (when (<= i n)
        (commit-by! d (string-append "agent:" (number->string i)) a
                    (string-append "v" (number->string i)))
        (loop (+ i 1))))
    (call d (list 'commit a) "test")))
(define at-eight (commits-then-refuse 8))
(define at-nine (commits-then-refuse 9))

(want "W3-19 eight earlier commits are all handed back"
      (length (since-of at-eight)) 8)
(want "W3-19 and nothing was left out, so the answer is not marked"
      (list (field at-eight 'truncated) (field at-eight 'retrieve))
      (list #f #f))
(want "W3-20 the ninth earlier commit does not fit"
      (length (since-of at-nine)) 8)
(want "W3-20 and leaving it out is what marks the answer"
      (list (field at-nine 'truncated) (field at-nine 'retrieve))
      (list '(truncated #t) (list 'retrieve (list 'log (cadr (field at-nine 'block)))
                                  (list 'read (cadr (field at-nine 'block))))))
;; AND A RECORD THAT IS NOT AN ENTRY IS NOT AN OMISSION. Eight commits
;; on this block, and afterwards a commit on another one: the log holds
;; a record beyond the eight, and it is not one that belonged in
;; `since`, so the answer must still be unmarked. Without this row the
;; ruling is satisfied by counting records rather than entries.
(define d9 (fresh-store!))
(define a9 (insert! d9 "A"))
(define b9 (insert! d9 "B"))
(call d9 (list 'write a9 "mine") "test")
;; THE UNRELATED RECORD GOES BEFORE THE EIGHT, NOT AFTER THEM. The walk
;; is newest-first, so a record written LAST is passed over long before
;; the count bound is reached and the lookahead never meets it -- with
;; it at the end, removing the block test from the lookahead changed
;; nothing and this row stayed green. Written first, it is the next
;; record the lookahead sees, which is the position the row is about.
(commit-by! d9 "agent:elsewhere" b9 "not about A")
(let loop ((i 1))
  (when (<= i 8)
    (commit-by! d9 (string-append "agent:" (number->string i)) a9
                (string-append "v" (number->string i)))
    (loop (+ i 1))))
(define at-eight-plus-noise (call d9 (list 'commit a9) "test"))
(want "W3-21 a record about another block is not an omission"
      (list (length (since-of at-eight-plus-noise))
            (field at-eight-plus-noise 'truncated))
      (list 8 #f))

(printf "\n~a failures\n" failures)
(printf "rows: ~a\n" rows-run)
(printf "working3 complete\n")
(exit (if (= failures 0) 0 1))
