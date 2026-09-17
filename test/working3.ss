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
;; writer can be told what the first one did -- design.md 7.5.5: "块在
;; commit5，先到者提交后变成 commit6；后到者带着 commit5 来，校验失败，把
;; commit6 的提交信息交给他，合并之后再提交", and the W brief's line for
;; the cells: "前提陈旧 ⇒ 拒绝且日志逐字节不变；拒绝里点名先提交者的
;; record-id、actor、内容". The core does not merge and does not rebase;
;; handing the fact back IS the feature. A refusal that said only
;; `stale-baseline` would be correct and useless, and nothing asserted
;; otherwise: `working1.ss` checks the refusal's KIND and its `block`,
;; and every other fixture stops there, so emptying `since` broke
;; nothing that anyone was measuring.
;;
;; AND THE ANSWER IS BOUNDED, which is the other half of the same
;; sentence: "有界：中间落了很多条时 since 只给身份与 actor 的清单加当前
;; 内容，其余用 log/diff/read 取——拒绝的大小不能随历史长度增长." The
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
;; ⚠️ AND A RAISE IS A FAILURE EVEN WHEN BOTH SIDES RAISE THE SAME WAY.
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
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (mkdir-p! d)
    (rpc-dispatch d '(init) "test")
    d))
(define (call store args actor) (rpc-dispatch store args actor))
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
(want "W3-11 the eight are the most recent, newest first"
      (map car (since-of refusal4))
      (reverse (list-tail many 2)))
(want "W3-11 and each of them is about this block"
      (map (lambda (e) (cadr (caddr e))) (since-of refusal4))
      (make-list 8 a4))
(want "W3-12 a cut answer says so"
      (field refusal4 'truncated) '(truncated #t))
(want "W3-13 a cut answer says where the rest is"
      (field refusal4 'retrieve) (list 'retrieve (list 'log a4) (list 'read a4)))

(printf "\n== the bound is on bytes as well as on records ==\n")
;; Three commits, the middle one far larger than the whole budget. A
;; bound counted only in records would hand back an answer the size of
;; the history it was supposed to replace.
(define d5 (fresh-store!))
(define a5 (insert! d5 "A"))
(call d5 (list 'write a5 "mine") "test")
(define small-1 (commit-by! d5 "agent:small-1" a5 "small-one"))
(define huge (commit-by! d5 "agent:huge" a5 (make-string 9000 #\x)))
(define small-2 (commit-by! d5 "agent:small-2" a5 "small-two"))
(define refusal5 (call d5 (list 'commit a5) "test"))
(want "W3-14 a commit larger than the byte budget is left out"
      (map cadr (since-of refusal5)) '("agent:small-2" "agent:small-1"))
(want "W3-14 and leaving it out marks the answer truncated"
      (field refusal5 'truncated) '(truncated #t))
(want "W3-15 the commits that fit are still handed back whole"
      (map caddr (since-of refusal5))
      (list (list 'set a5 'src "small-two") (list 'set a5 'src "small-one")))
(want "W3-15 the answer is smaller than the commit it left out"
      (< (apply + (map encoded-size (since-of refusal5))) 9000) #t)

(printf "\n== the byte bound is cumulative, not per entry ==\n")
;; THE ROWS ABOVE DO NOT ESTABLISH THE BUDGET. Every one of them is also
;; satisfied by a bound applied to each entry on its own: the oversized
;; commit is left out either way. A per-entry bound lets any number of
;; individually acceptable entries add up past it, which is exactly the
;; thing the design forbids -- "拒绝的大小不能随历史长度增长".
;;
;; SEVEN COMMITS, SO THE COUNT BOUND CANNOT BE WHAT CUTS THEM. Seven is
;; below the eight-entry window, and each commit is far below 8192 bytes
;; on its own, so anything left out here was left out by the total.
;; THE COMMITS ARE KEPT, so the entries can be built here rather than
;; read out of the answer. A row that only counted entries, or only
;; looked at their shape, was satisfied by five fabricated ones and by
;; an empty list: both were tried, and both printed ok.
(define (seven-commits-on store block)
  (let loop ((i 1) (out '()))
    (if (> i 7) (reverse out)
        (let ((text (make-string 1400 (integer->char (+ 97 i)))))
          (loop (+ i 1)
                (cons (list (commit-by! store (string-append "agent:" (number->string i))
                                        block text)
                            (string-append "agent:" (number->string i))
                            text)
                      out))))))
(define (entries-of commits block)
  (map (lambda (c) (list (car c) (cadr c) (list 'set block 'src (caddr c)))) commits))
;; AND HOW MANY OF THEM FIT IS COMPUTED FROM THE BOUND, not taken from
;; the answer. The seven are the same size to within a digit, so
;; "stop at the first that does not fit" and "skip the ones that do not
;; fit" give the same prefix here; the fixture does not have to know
;; which the product does.
(define (fitting budget entries)
  (let loop ((xs entries) (out '()) (size 0))
    (cond ((null? xs) (reverse out))
          ((> (+ size (encoded-size (car xs))) budget) (reverse out))
          (else (loop (cdr xs) (cons (car xs) out)
                      (+ size (encoded-size (car xs))))))))
(define d6 (fresh-store!))
(define a6 (insert! d6 "A"))
(call d6 (list 'write a6 "mine") "test")
(define seven (seven-commits-on d6 a6))
(define refusal6 (call d6 (list 'commit a6) "test"))
(define expected6 (fitting 8192 (reverse (entries-of seven a6))))
(want "W3-16 seven entries that fit one at a time do not all fit together"
      (< (length (since-of refusal6)) 7) #t)
(want "W3-16 what is handed back fits inside the byte budget"
      (<= (apply + (map encoded-size (since-of refusal6))) 8192) #t)
(want "W3-16 and cutting by total marks the answer truncated"
      (field refusal6 'truncated) '(truncated #t))
;; TWIN: the entries that did come back are the newest commits, whole.
(want "W3-16 TWIN: what comes back is those commits, newest first, entire"
      (since-of refusal6) expected6)
(want "W3-16 TWIN: and it is not empty"
      (> (length (since-of refusal6)) 0) #t)

;; AND THE ALLOWANCE DOES NOT GROW WITH THE HISTORY. That is the
;; sentence the bound exists for -- "拒绝的大小不能随历史长度增长" -- and
;; none of the rows above reaches it: a budget of, say, 1024 bytes per
;; record in the log satisfies every one of them on a short history and
;; hands back an answer that keeps growing on a long one. The same seven
;; commits are made again behind a hundred records that have nothing to
;; do with this block.
(define d7 (fresh-store!))
(define a7 (insert! d7 "A"))
(define b7 (insert! d7 "B"))
(let loop ((i 1))
  (when (<= i 100)
    (commit-by! d7 "agent:noise" b7 (string-append "n" (number->string i)))
    (loop (+ i 1))))
(call d7 (list 'write a7 "mine") "test")
(define seven-again (seven-commits-on d7 a7))
(define refusal7 (call d7 (list 'commit a7) "test"))
(want "W3-17 a hundred unrelated records later, the same commits give the same answer"
      (list (length (since-of refusal7))
            (map cadr (since-of refusal7))
            (<= (apply + (map encoded-size (since-of refusal7))) 8192))
      (list (length (since-of refusal6))
            (map cadr (since-of refusal6))
            #t))
(want "W3-17 and it is still exactly the newest commits that fit"
      (since-of refusal7)
      (fitting 8192 (reverse (entries-of seven-again a7))))

;; AND NOT WITH THE NUMBER OF ELIGIBLE COMMITS EITHER. The pair above
;; puts its hundred extra records on ANOTHER block, so both of its
;; stores still have seven commits on this one: a budget of, say,
;; max(8192, 1024 x eligible-commits) passes every row up to here, and
;; the ten-commit case earlier is made of tiny texts that fit whatever
;; the budget is. Ten commits of the same 1400 characters separate them
;; -- the total is over the budget either way, and how many come back is
;; not the same number.
(define d8 (fresh-store!))
(define a8 (insert! d8 "A"))
(call d8 (list 'write a8 "mine") "test")
(define ten
  (let loop ((i 1) (out '()))
    (if (> i 10) (reverse out)
        (let ((text (make-string 1400 (integer->char (+ 97 i)))))
          (loop (+ i 1)
                (cons (list (commit-by! d8 (string-append "agent:" (number->string i)) a8 text)
                            (string-append "agent:" (number->string i))
                            text)
                      out))))))
(define refusal8 (call d8 (list 'commit a8) "test"))
(want "W3-18 ten large commits are cut by the same fixed total"
      (since-of refusal8)
      (fitting 8192 (reverse (entries-of ten a8))))
(want "W3-18 fewer than eight come back, and they are within the total"
      (list (< (length (since-of refusal8)) 8) (<= (since-bytes refusal8) 8192))
      (list #t #t))
;; AND THE SELECTION IS MAXIMAL: the next commit down was left out
;; because it did not fit, not because the answer stopped early. Without
;; this, every "within 8192" row above is also satisfied by a budget
;; SMALLER than 8192, and by a count cap wearing a byte cap's name.
(define (first-omitted entries answer)
  (let ((kept (length (since-of answer))))
    (and (> (length entries) kept) (list-ref entries kept))))
(want "W3-18 and the next commit down is the one that would not fit"
      (let ((next (first-omitted (reverse (entries-of ten a8)) refusal8)))
        (and next (> (+ (since-bytes refusal8) (encoded-size next)) 8192)))
      #t)

;; == the budget is a TOTAL, at the same byte ==
;; W3-24 pins one entry against 8192, and a bound of the shape
;; "each entry at most 8192, and the total at most 8400" satisfies it and
;; every selection above. Two entries adding to exactly the limit, and to
;; one past it, are what separate a per-entry edge from a cumulative one.
(define (two-commit-refusal t1 t2)
  (let* ((d (fresh-store!)) (a (insert! d "A")))
    (call d (list 'write a "mine") "test")
    (commit-by! d "agent:older" a t1)
    (commit-by! d "agent:newer" a t2)
    (cons a (call d (list 'commit a) "test"))))
(define pair-probe (two-commit-refusal (make-string 1000 #\p) (make-string 1000 #\q)))
(define pair-size (since-bytes (cdr pair-probe)))
(define (pair-at n)
  (two-commit-refusal (make-string 1000 #\p)
                      (make-string (+ 1000 (- n pair-size)) #\q)))
(define pair-8192 (pair-at 8192))
(define pair-8193 (pair-at 8193))
(want "W3-25 the probe pair is under the bound and both entries come back"
      (list (length (since-of (cdr pair-probe))) (field (cdr pair-probe) 'truncated))
      (list 2 #f))
(want "W3-25 two entries adding to exactly the budget both come back"
      (list (since-bytes (cdr pair-8192)) (length (since-of (cdr pair-8192)))
            (field (cdr pair-8192) 'truncated))
      (list 8192 2 #f))
(want "W3-25 and one byte more costs the older of them"
      (list (map cadr (since-of (cdr pair-8193))) (field (cdr pair-8193) 'truncated))
      (list '("agent:newer") '(truncated #t)))

;; == an entry that does not fit is skipped, not a stopping point ==
;; EVERY CASE ABOVE USES ENTRIES OF ONE SIZE, so "stop at the first that
;; does not fit" and "skip it and carry on" choose the same set. Three
;; entries of decreasing size separate them: newest-first about 7000,
;; 2000 and 1000 bytes, where the middle one does not fit beside the
;; first and the last one does. W3-14's oversized entry does not reach
;; this -- it is over the whole budget by itself, so either reading drops
;; it.
(define (three-commit-refusal sizes)
  (let* ((d (fresh-store!)) (a (insert! d "A")))
    (call d (list 'write a "mine") "test")
    (for-each (lambda (n i)
                (commit-by! d (string-append "agent:" (number->string i)) a
                            (make-string n (integer->char (+ 97 i)))))
              sizes (list 1 2 3))
    (cons a (call d (list 'commit a) "test"))))
;; The sizes are written newest-LAST here, so the answer's order is the
;; reverse: the 7000 first, then the 2000, then the 1000.
(define graded (three-commit-refusal (list 1000 2000 7000)))
(want "W3-26 a middle entry that does not fit is passed over, not a full stop"
      (map cadr (since-of (cdr graded))) '("agent:3" "agent:1"))
(want "W3-26 and passing it over marks the answer"
      (field (cdr graded) 'truncated) '(truncated #t))
(want "W3-26 and what came back is still inside the budget"
      (<= (since-bytes (cdr graded)) 8192) #t)

;; == the actor is part of what is counted ==
;; EVERY LARGE ENTRY ABOVE IS LARGE IN ITS CONTENT, and the actors are
;; all `agent:` and a digit -- so a budget that counted only the payload
;; would select the same entries in every one of those cases and hand
;; back an answer that grows with the length of the committers' names.
;; `baseline.ss` says the bound includes actor text; this is the row
;; that asks.
(define d10 (fresh-store!))
(define a10 (insert! d10 "A"))
(call d10 (list 'write a10 "mine") "test")
(define wordy
  (let loop ((i 1) (out '()))
    (if (> i 7) (reverse out)
        (let ((actor (string-append "agent:" (make-string 1400 (integer->char (+ 97 i))))))
          (loop (+ i 1)
                (cons (list (commit-by! d10 actor a10 (string-append "v" (number->string i)))
                            actor
                            (string-append "v" (number->string i)))
                      out))))))
(define refusal10 (call d10 (list 'commit a10) "test"))
(want "W3-22 seven commits with long actor names do not all fit either"
      (< (length (since-of refusal10)) 7) #t)
(want "W3-22 and what comes back is the newest of them, within the total"
      (list (since-of refusal10) (<= (since-bytes refusal10) 8192))
      (list (fitting 8192 (reverse (entries-of wordy a10))) #t))
(want "W3-22 and leaving the rest out marks the answer"
      (field refusal10 'truncated) '(truncated #t))

;; == the budget is counted in bytes, and its edge is exactly 8192 ==
;;
;; EVERY FIXTURE ABOVE IS ASCII, so a budget that counted PRINTED
;; CHARACTERS selects the same entries in all of them. One entry of 1400
;; ideographs measures 1454 characters and 4254 bytes; the two readings
;; part company there and nowhere else in this file.
(define d11 (fresh-store!))
(define a11 (insert! d11 "A"))
(call d11 (list 'write a11 "mine") "test")
(define wide
  (let loop ((i 1) (out '()))
    (if (> i 3) (reverse out)
        (let ((text (make-string 1400 (integer->char (+ #x754C i)))))
          (loop (+ i 1)
                (cons (list (commit-by! d11 (string-append "agent:" (number->string i)) a11 text)
                            (string-append "agent:" (number->string i))
                            text)
                      out))))))
(define refusal11 (call d11 (list 'commit a11) "test"))
(want "W3-23 three multibyte commits are measured in bytes, not characters"
      (list (length (since-of refusal11)) (<= (since-bytes refusal11) 8192))
      (list (length (fitting 8192 (reverse (entries-of wide a11)))) #t))
(want "W3-23 and they are the newest of them"
      (since-of refusal11) (fitting 8192 (reverse (entries-of wide a11))))
(define (printed-size value) (string-length (sexpr->string-extended (storable-encode value))))
;; AND THE TWO MEASUREMENTS ARE SHOWN TO DISAGREE ON THIS DATA rather
;; than asserted to. The first version of this row compared two
;; constants -- `(< 8192 (* 1400 3))` is #f whatever the store does.
(want "W3-23 these entries measure differently in characters and in bytes"
      (let* ((es (entries-of wide a11))
             (chars (apply + (map printed-size es)))
             (bytes (apply + (map encoded-size es))))
        ;; ALL THREE TOGETHER STRADDLE THE BUDGET: counted as characters
        ;; they fit and nothing would be left out, counted as bytes they
        ;; do not. That is the difference the rows above are reading.
        (list (> bytes (* 2 chars)) (<= chars 8192) (> bytes 8192)))
      (list #t #t #t))

;; AND THE EDGE IS 8192 EXACTLY. Every row above is also satisfied by a
;; budget of 8000, or of 8400: the entries are about 1455 bytes, so five
;; fit under all three and six fit under none. A single entry sized to
;; the byte is what pins the number -- one that measures exactly 8192
;; must be delivered, and one that measures 8193 must not.
;;
;; THE SIZE IS REACHED BY MEASURING, not by arithmetic on a guess: an
;; entry's encoding includes the writer name, the sequence number and
;; the actor, so a probe commit is measured first and the payload of the
;; real one is adjusted by the difference. The two stores are different
;; stores with different writer names -- but a writer name is always
;; eight base-36 characters and the record is always at the same
;; sequence, so the two entries differ only by the payload the fixture
;; chose.
(define (one-commit-refusal text)
  (let* ((d (fresh-store!)) (a (insert! d "A")))
    (call d (list 'write a "mine") "test")
    (commit-by! d "agent:edge" a text)
    (cons a (call d (list 'commit a) "test"))))
(define probe (one-commit-refusal (make-string 1000 #\p)))
(define probe-size (since-bytes (cdr probe)))
(define (sized n) (make-string (+ 1000 (- n probe-size)) #\p))
(define at-limit (one-commit-refusal (sized 8192)))
(define over-limit (one-commit-refusal (sized 8193)))
(want "W3-24 an entry measuring exactly the budget is delivered"
      (list (since-bytes (cdr at-limit)) (length (since-of (cdr at-limit)))
            (field (cdr at-limit) 'truncated))
      (list 8192 1 #f))
(want "W3-24 and one byte more is not"
      (list (length (since-of (cdr over-limit))) (field (cdr over-limit) 'truncated))
      (list 0 '(truncated #t)))

;; == an entry rejected for its size does not use up a place ==
;; The two bounds are separate: eight ENTRIES, and 8192 bytes of them.
;; An implementation that counted a byte-rejected entry toward the eight
;; passes every case above, because no case there has both an oversized
;; entry and more than eight candidates behind it. This one does: one
;; small commit, then seven of nine thousand bytes each, then one small
;; commit. The seven are each over the budget on their own and are
;; passed over; the two small ones are what must come back.
(define d12 (fresh-store!))
(define a12 (insert! d12 "A"))
(call d12 (list 'write a12 "mine") "test")
(define first-small (commit-by! d12 "agent:first-small" a12 "small-one"))
(let loop ((i 1))
  (when (<= i 7)
    (commit-by! d12 (string-append "agent:huge-" (number->string i)) a12
                (make-string 9000 (integer->char (+ 97 i))))
    (loop (+ i 1))))
(define last-small (commit-by! d12 "agent:last-small" a12 "small-two"))
(define refusal12 (call d12 (list 'commit a12) "test"))
(want "W3-27 an entry passed over for its size does not consume a place in the eight"
      (map cadr (since-of refusal12)) '("agent:last-small" "agent:first-small"))
(want "W3-27 and the answer says the seven were left out"
      (field refusal12 'truncated) '(truncated #t))

;; AND THE SAME FOR AN ENTRY REJECTED BY THE TOTAL RATHER THAN BY ITS
;; OWN SIZE. W3-27's seven are each over the whole budget, so an
;; implementation that charged only CUMULATIVE rejections to the count
;; passes it. Eight commits of 1400 bytes and, older than all of them,
;; one of fifty: five of the eight fit, the next three are rejected
;; because the total would go over, and the fifty-byte one still fits
;; beside the five. Charging those three to the count exits the walk at
;; eight and the oldest one never comes back.
;; FOUR SMALL ONES, NOT ONE, because the count has to be able to
;; OVERFLOW. With a single small commit behind the eight, an
;; implementation that RESETS the count on a rejection rather than
;; charging it still hands back six entries and satisfies "at most
;; eight". Four small ones make the difference visible: preserved, the
;; answer stops at eight entries; reset, a ninth comes back.
(define d13 (fresh-store!))
(define a13 (insert! d13 "A"))
(call d13 (list 'write a13 "mine") "test")
(let loop ((i 1))
  (when (<= i 4)
    (commit-by! d13 (string-append "agent:small-" (number->string i)) a13
                (string-append "t" (number->string i)))
    (loop (+ i 1))))
(let loop ((i 1))
  (when (<= i 8)
    (commit-by! d13 (string-append "agent:big-" (number->string i)) a13
                (make-string 1400 (integer->char (+ 97 i))))
    (loop (+ i 1))))
(define refusal13 (call d13 (list 'commit a13) "test"))
(want "W3-28 an entry rejected by the total does not use up a place either"
      (exists (lambda (e) (equal? (cadr e) "agent:small-4")) (since-of refusal13)) #t)
(want "W3-28 and the accepted ones are still counted, so the answer stops at eight"
      (list (length (since-of refusal13)) (<= (since-bytes refusal13) 8192))
      (list 8 #t))

;; AND AN OVERSIZED ENTRY DOES NOT CLEAR WHAT WAS ALREADY SPENT. Every
;; case above surrounds its oversized rejection with entries small
;; enough that a bound which RESET the running total when it met one
;; would choose the same set anyway. Three commits of five thousand,
;; nine thousand and five thousand bytes separate them: the first five
;; thousand is taken, the nine thousand is over the budget on its own
;; and is passed over, and the second five thousand would take the total
;; to about ten thousand -- so it must be left out too. Reset, both five
;; thousands come back and the answer is half again the budget.
(define graded2 (three-commit-refusal (list 5000 9000 5000)))
(want "W3-29 an entry passed over for its size does not clear the running total"
      (list (length (since-of (cdr graded2))) (<= (since-bytes (cdr graded2)) 8192))
      (list 1 #t))
(want "W3-29 and the one that came back is the newest"
      (map cadr (since-of (cdr graded2))) '("agent:3"))
(want "W3-29 and the answer says the other two were left out"
      (field (cdr graded2) 'truncated) '(truncated #t))

;; AND AN OVERSIZED ENTRY DOES NOT CLEAR THE COUNT EITHER. W3-28 puts
;; count preservation under pressure across a CUMULATIVE rejection;
;; nothing yet does it across an individually oversized one, and the two
;; are separate branches of the same decision. Five small commits, then
;; one of nine thousand bytes, then five more small ones: walking
;; newest-first the first five are taken, the nine thousand is passed
;; over, and three more fill the window -- eight. Clearing the count
;; there lets all ten through.
;;
;; THAT CLOSES THE SQUARE. The rejection branch carries two
;; accumulators and there are two kinds of rejection, so there are four
;; ways to lose one: count across a cumulative rejection (W3-28), count
;; across an oversized one (here), the running total across an oversized
;; one (W3-29), and the running total across a cumulative one, which
;; W3-16's twin already reads because it names the entries it expects.
(define d14 (fresh-store!))
(define a14 (insert! d14 "A"))
(call d14 (list 'write a14 "mine") "test")
(let loop ((i 1))
  (when (<= i 5)
    (commit-by! d14 (string-append "agent:low-" (number->string i)) a14
                (string-append "t" (number->string i)))
    (loop (+ i 1))))
(commit-by! d14 "agent:oversized" a14 (make-string 9000 #\z))
(let loop ((i 1))
  (when (<= i 5)
    (commit-by! d14 (string-append "agent:high-" (number->string i)) a14
                (string-append "u" (number->string i)))
    (loop (+ i 1))))
(define refusal14 (call d14 (list 'commit a14) "test"))
(want "W3-30 an entry passed over for its size does not clear the count either"
      (length (since-of refusal14)) 8)
(want "W3-30 and the oversized one is not among what came back"
      (exists (lambda (e) (equal? (cadr e) "agent:oversized")) (since-of refusal14)) #f)

;; AND A BYTE OMISSION IS REMEMBERED ACROSS THE REST OF THE WALK. Both
;; cases above leave eligible records beyond the window, so the mark is
;; earned twice over and dropping the one the byte bound set changes
;; nothing. Eight small commits and, older than all of them, one over the
;; budget: the window fills exactly, and the only reason the answer is
;; marked is the one entry the bytes left out.
(define d15 (fresh-store!))
(define a15 (insert! d15 "A"))
(call d15 (list 'write a15 "mine") "test")
(let loop ((i 1))
  (when (<= i 8)
    (commit-by! d15 (string-append "agent:fits-" (number->string i)) a15
                (string-append "v" (number->string i)))
    (loop (+ i 1))))
;; ⚠️ THE OVERSIZED ONE IS THE NEWEST, and the order is the whole row.
;; Written OLDEST, as this was at first, the walk fills the window on the
;; eight small ones and the mark then comes from the lookahead finding
;; that record still sitting in the history -- nothing to do with its
;; size, and a bound that forgot the byte omission at a full window
;; passed. Newest, it is met and rejected on its size FIRST, the eight
;; then fill the window exactly, and nothing is left for the lookahead:
;; the mark can only come from the byte the bound refused.
(commit-by! d15 "agent:only-omission" a15 (make-string 9000 #\y))
(define refusal15 (call d15 (list 'commit a15) "test"))
(want "W3-31 the window fills exactly and the byte omission is still reported"
      (list (length (since-of refusal15)) (field refusal15 'truncated))
      (list 8 '(truncated #t)))
(want "W3-31 and the omitted one is the oversized commit"
      (exists (lambda (e) (equal? (cadr e) "agent:only-omission")) (since-of refusal15)) #f)

;; == the budget counts what the encoding costs, not what the text holds ==
;; EVERY PAYLOAD ABOVE IS MADE OF CHARACTERS THAT COST ONE BYTE AND NEED
;; NO ESCAPING, so a bound that measured the raw text plus a fixed
;; overhead would choose the same entries everywhere -- including in the
;; multibyte case, where it would still be counting bytes. Quotation
;; marks separate them: five thousand of them encode to about ten
;; thousand bytes, so the entry does not fit, while their raw size is
;; five thousand and would.
(define d16 (fresh-store!))
(define a16 (insert! d16 "A"))
(call d16 (list 'write a16 "mine") "test")
(define quoted (commit-by! d16 "agent:quoted" a16 (make-string 5000 (integer->char 34))))
(define refusal16 (call d16 (list 'commit a16) "test"))
(want "W3-32 five thousand quotation marks cost more than five thousand bytes"
      (> (encoded-size (list quoted "agent:quoted" (list 'set a16 'src (make-string 5000 (integer->char 34))))) 8192) #t)
(want "W3-32 so the entry does not fit, and the answer says it was left out"
      (list (length (since-of refusal16)) (field refusal16 'truncated))
      (list 0 '(truncated #t)))

;; AND THE ACTOR IS MEASURED THE SAME WAY. W3-22 shows the actor counts,
;; but its actors are plain letters -- so an estimator that encoded the
;; payload properly and took the actor's RAW bytes agreed with the
;; encoder everywhere in this file. Five thousand quotation marks in the
;; NAME cost about ten thousand bytes; raw they cost five thousand and
;; the entry would be delivered.
(define d17 (fresh-store!))
(define a17 (insert! d17 "A"))
(call d17 (list 'write a17 "mine") "test")
(define wild-actor (string-append "agent:" (make-string 5000 (integer->char 34))))
(define wild (commit-by! d17 wild-actor a17 "short"))
(define refusal17 (call d17 (list 'commit a17) "test"))
(want "W3-33 a name full of quotation marks costs what it encodes to"
      (> (encoded-size (list wild wild-actor (list 'set a17 'src "short"))) 8192) #t)
(want "W3-33 so that entry does not fit either"
      (list (length (since-of refusal17)) (field refusal17 'truncated))
      (list 0 '(truncated #t)))

;; == the mark is about an omission, not about the log ==
;;
;; RULED 2026-09-17: `truncated` says that an entry which BELONGED in
;; `since` was left out -- about this block, after the draft's cut, and
;; dropped for one of the two bounds. It does not say that the log holds
;; other records. The two readings differ exactly at the count bound,
;; and the difference was visible: a block always carries the record
;; that created it, so "is any history left" was true for every refusal
;; that reached eight, and an answer carrying all eight of the eight
;; commits that existed still told its reader to go and fetch the rest.
;;
;; THE PAIR IS WHAT SEPARATES THEM. Eight earlier commits and nine
;; earlier commits differ by one record; under the old reading both are
;; marked, under the ruling only the second is. A single row at either
;; number is passed by both readings.
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
