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

;; WHAT A STALE REFUSAL HANDS BACK, UNDER THE CONTRACT THAT REPLACED THE
;; BYTE BUDGET.
;;
;; The refusal gives the loser the winner's commit so they can merge
;; without asking a second question, and its size must not grow with the
;; history. The bound used to be a byte budget, and an entry that did
;; not fit was DROPPED -- so the one record a reader needed could vanish
;; because somebody had written a long paragraph.
;;
;; THE CONTRACT NOW (design 7.5.11, 7.5.16):
;;
;;   * at most eight entries;
;;   * the winner is the first and is always present;
;;   * the rest in CAUSAL order, not in the order they were ingested;
;;   * an entry is never dropped for its size -- a content over 1024
;;     bytes is replaced by `(content elided <bytes>)`, identity and
;;     actor intact, and it TAKES ITS SLOT;
;;   * `truncated` only when a relevant entry was left out;
;;   * no relevant record at all carries its own reason instead of an
;;     empty `since` for the client to guess at.
;;
;; THESE ROWS ARE THE NAMED SUCCESSORS of the group retired in
;; `working3.sc`: W3-11 and W3-14 through W3-33, whose oracle was "pick
;; entries until the byte budget runs out".

(import (chezscheme) (theourgia rpc) (theourgia store)
        (theourgia reduce) (theourgia ffi) (theourgia wire) (theourgia log)
        (theourgia request))

(define failures 0)
(define rows-run 0)
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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (cons 'raised
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       (cons 'returned e0)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/since-contract-" (number->string (get-process-id))))
(when (file-exists? root) (error 'since-contract "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define n-store 0)
;; KEY: EACH STORE'S OWN WRITER IS REMEMBERED AT `init`, BECAUSE A DRAFT
;; VERB IS NO LONGER TOLD ONE BY DEFAULT. It used to fall back to the
;; store's local log writer; the core now refuses an unnamed writer. The
;; rows here are about what `since` hands back with a refusal, so they
;; need the refusal they were written for and not one about identity.
;;
;; NEVER: AND A STORE WHOSE WRITER WAS NEVER RECORDED IS AN ERROR, NOT A
;; FALLBACK TO #f: that would quietly turn a row into a test of
;; `writer-required`, which is also a refusal, and it would still pass
;; `refuse!`.
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
        (error 'since-contract "no writer was recorded for this store" store))))

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
(define (commit-by! store actor id text)
  (let ((a (call store (list 'set id "src" text) actor)))
    (unless (rpc-ok? a) (error 'since-contract "the earlier commit did not happen" a))
    (car (cadr (assq 'events (cdr a))))))
(define (fields answer) (filter pair? (cdr answer)))
(define (field answer key) (assq key (fields answer)))
(define (since-of answer)
  (let ((p (field answer 'since))) (if p (cdr p) 'no-since-field)))
(define (ids-of answer) (map car (since-of answer)))
(define (actors-of answer) (map cadr (since-of answer)))
(define (contents-of answer) (map caddr (since-of answer)))
(define (refuse! store id) (call store (list 'commit id) "test"))
(define (kind-of answer) (if (and (pair? answer) (pair? (cdr answer))) (cadr answer) answer))
(define (encoded-size value)
  (bytevector-length (string->utf8 (sexpr->string-extended (storable-encode value)))))

;; A DRAFT, THEN OTHER PEOPLE COMMIT, THEN THE DRAFT IS OFFERED. That is
;; the only way to get a refusal, and every row below is one of those.
(define (staged store id n make-text)
  (call store (list 'write id "mine") "test")
  (let loop ((i 0))
    (when (< i n)
      (commit-by! store (string-append "agent:" (number->string i)) id (make-text i))
      (loop (+ i 1))))
  (refuse! store id))

;; ---- SC-01 exactly eight -------------------------------------------------

(define d1 (fresh-store!))
(define a1 (insert! d1 "A"))
(define r8 (staged d1 a1 8 (lambda (i) (string-append "text-" (number->string i)))))
(want "SC-01 a refusal is what comes back" (kind-of r8) 'stale-baseline)
(want "SC-01 eight relevant commits give eight entries" (length (since-of r8)) 8)
(want "SC-01 and nothing was left out, so it is not truncated"
      (field r8 'truncated) #f)
(want "SC-01 the winner is first"
      (car (actors-of r8)) "agent:7")

;; ---- SC-02 nine ----------------------------------------------------------

(define d2 (fresh-store!))
(define a2 (insert! d2 "A"))
(define r9 (staged d2 a2 9 (lambda (i) (string-append "text-" (number->string i)))))
(want "SC-02 nine relevant commits still give eight" (length (since-of r9)) 8)
(want "SC-02 and the answer says one was left out"
      (field r9 'truncated) '(truncated #t))
(want "SC-02 the winner is still first, and it is the newest"
      (car (actors-of r9)) "agent:8")
(want "SC-02 TWIN: the one dropped is the OLDEST, not the first"
      (exists (lambda (a) (equal? a "agent:0")) (actors-of r9)) #f)

;; KEY: AND THE SEVEN BEHIND THE WINNER ARE IN CAUSAL ORDER. Without this
;; row a build that kept them newest-first -- which is the order the
;; window is chosen in -- passes every other assertion here: the count,
;; the mark, the winner and the absence of the oldest are all unchanged
;; by reversing the tail.
(want "SC-02 the rest follow the winner oldest-first"
      (cdr (actors-of r9))
      '("agent:1" "agent:2" "agent:3" "agent:4" "agent:5" "agent:6" "agent:7"))


;; ---- SC-03 the order is causal, not the order they arrived ---------------
;;
;; The winner is first because it is the record the loser needs. The
;; REST are given in the order a reader would apply them: premises
;; before the records that depend on them.
;;
;; Here a, b and z are three commits of one writer, so `a -> b -> z` is
;; their causal order by construction and z is the winner.

(define d3 (fresh-store!))
(define a3 (insert! d3 "A"))
(call d3 (list 'write a3 "mine") "test")
(commit-by! d3 "agent:a" a3 "a")
(commit-by! d3 "agent:b" a3 "b")
(commit-by! d3 "agent:z" a3 "z")
(define r3 (refuse! d3 a3))
(want "SC-03 the winner comes first" (car (actors-of r3)) "agent:z")
(want "SC-03 and the rest follow in causal order, premises first"
      (cdr (actors-of r3)) '("agent:a" "agent:b"))

;; ---- SC-03b two ingestion orders, one answer -----------------------------
;;
;; KEY: THIS IS THE ROW THE ORDERING RULE EXISTS FOR, and the rows above do
;; not replace it: with one writer the causal order and the arrival order
;; are the same sequence, so both a causal implementation and an
;; ingestion-ordered one answer alike. The old `baseline.sc` sorted by
;; ingestion and passed a "z depends on a, z arrived first" row; only a
;; schedule where the PREMISE ARRIVES SECOND separates them.
;;
;; So the same three records are published into two fresh stores in
;; opposite orders. `b` depends on `a`, so the causal order is fixed by
;; the records and the arrival order is not.

(define (framed seq deps actor payload)
  (encode-record seq (+ 1789000000000 seq) actor deps (storable-encode payload)))

(define creator-block "creator0.1")
(define creator
  (framed 1 '() "ordinary"
          '(put ((kind . section) (title . "A") (parent . root) (ord . 1)))))
;; KEY: THE WRITER NAMES RUN AGAINST THE CAUSAL ORDER, ON PURPOSE.
;;
;; The premise is written by `zzzzzzzz` and the dependent by `aaaaaaaa`,
;; so sorting by (writer, seq) -- which is what an ingestion-ordered
;; implementation does -- puts them the WRONG way round. Measured: with
;; the names the other way, seeding `baseline.sc` back to ingestion order
;; left every row here green, because the names happened to agree with
;; causality and the cell was asserting a coincidence.
(define rec-a
  (framed 1 '(("creator0" . 1)) "agent:premise" (list 'set creator-block 'src "a-text")))
(define rec-b
  (framed 1 '(("zzzzzzzz" . 1)) "agent:dependent" (list 'set creator-block 'src "b-text")))

;; THE DRAFT IS WRITTEN WHILE ONLY THE CREATOR IS IN, so its baseline is
;; the hash before either commit and both of them are eligible.
(define (publish! d writer bytes)
  (log-publish! d writer 1 bytes (segment-sha bytes)))

(define (two-order-refusal first second)
  (let ((d (fresh-store!)))
    (publish! d "creator0" creator)
    (call d (list 'write creator-block "mine") "test")
    (publish! d (car first) (cdr first))
    (publish! d (car second) (cdr second))
    (call d (list 'commit creator-block) "test")))

(define pair-a (cons "zzzzzzzz" rec-a))
(define pair-b (cons "aaaaaaaa" rec-b))
(define r-ab (two-order-refusal pair-a pair-b))
(define r-ba (two-order-refusal pair-b pair-a))

(want "SC-03b the premise-first delivery refuses" (kind-of r-ab) 'stale-baseline)
(want "SC-03b the dependent-first delivery refuses too" (kind-of r-ba) 'stale-baseline)
(want "SC-03b both name the same two commits"
      (list (length (since-of r-ab)) (length (since-of r-ba))) '(2 2))
(want "SC-03b the winner is the dependent, in both"
      (list (car (actors-of r-ab)) (car (actors-of r-ba))) '("agent:dependent" "agent:dependent"))
(want "SC-03b TWIN: and the premise follows it, in both"
      (list (cadr (actors-of r-ab)) (cadr (actors-of r-ba))) '("agent:premise" "agent:premise"))
(want "SC-03b the two answers' since lists are identical"
      (since-of r-ab) (since-of r-ba))

;; ---- SC-03c a concurrent record, which is what a comparator breaks on ----
;;
;; NOTE: SC-03 AND SC-03b USE CAUSAL CHAINS, AND A CHAIN CANNOT SEPARATE
;; THE TWO IMPLEMENTATIONS. "Causal, else lexical" only differs from a
;; ranked order when there is a pair the causal relation does NOT
;; relate: with `zzzzzzzz` before `aaaaaaaa` and `mmmmmmmm` concurrent
;; with both, the ascending relation cycles -- z < a causally, a < m
;; lexically, m < z lexically -- and a sort over it answers whatever its
;; traversal produces.
;;
;; The winner depends on all three, so all four are relevant and the
;; three are ordered among themselves.

;; KEY: THE WRITER NAMES ARE CHOSEN SO THAT THE TWO IMPLEMENTATIONS ANSWER
;; DIFFERENTLY. premise=zzzzzzzz, dependent=aaaaaaaa, concurrent=wwwwwwww,
;; winner=mmmmmmmm. Measured on exactly this arrangement:
;;
;;     "causal, else lexical":  winner, dependent, concurrent, premise
;;     this build's sort key:   winner, concurrent, premise, dependent
;;
;; -- the first puts the DEPENDENT before its own PREMISE, which is what
;; a relation that cycles does to a sort. An earlier version of this
;; fixture used names under which both answered alike, and recorded that
;; "no reachable input separates them". That was a statement about the
;; names I had picked.
(define rec-m
  (framed 1 '(("creator0" . 1)) "agent:concurrent" (list 'set creator-block 'src "m-text")))
(define rec-w
  (framed 1 '(("aaaaaaaa" . 1) ("wwwwwwww" . 1)) "agent:winner"
          (list 'set creator-block 'src "w-text")))

(define (three-order-refusal . order)
  (let ((d (fresh-store!)))
    (publish! d "creator0" creator)
    (call d (list 'write creator-block "mine") "test")
    (for-each (lambda (p) (publish! d (car p) (cdr p))) order)
    (call d (list 'commit creator-block) "test")))

(define pair-m (cons "wwwwwwww" rec-m))
(define pair-w (cons "mmmmmmmm" rec-w))
(define r-fwd (three-order-refusal pair-a pair-m pair-b pair-w))
(define r-rev (three-order-refusal pair-m pair-b pair-a pair-w))

(want "SC-03c both deliveries refuse"
      (list (kind-of r-fwd) (kind-of r-rev)) '(stale-baseline stale-baseline))
(want "SC-03c both name four commits"
      (list (length (since-of r-fwd)) (length (since-of r-rev))) '(4 4))
(want "SC-03c the winner is first in both"
      (list (car (actors-of r-fwd)) (car (actors-of r-rev)))
      '("agent:winner" "agent:winner"))
;; KEY: THE PREMISE BEFORE ITS DEPENDENT, IN BOTH -- which is the pair the
;; cycling relation reverses.
(define (position lst x)
  (let loop ((l lst) (i 0))
    (cond ((null? l) -1) ((equal? (car l) x) i) (else (loop (cdr l) (+ i 1))))))
(want "SC-03c the premise precedes its dependent, whichever way they arrived"
      (list (< (position (actors-of r-fwd) "agent:premise")
               (position (actors-of r-fwd) "agent:dependent"))
            (< (position (actors-of r-rev) "agent:premise")
               (position (actors-of r-rev) "agent:dependent")))
      '(#t #t))
(want "SC-03c and the two answers are identical" (since-of r-fwd) (since-of r-rev))

;; ---- SC-04 an oversized winner -------------------------------------------
;;
;; KEY: THE ENTRY STAYS. Under the byte budget this whole refusal came back
;; with an empty `since` and a `truncated` mark: the one record the
;; reader needed had been dropped for being long.

(define big (make-string 4000 #\x))
(define d4 (fresh-store!))
(define a4 (insert! d4 "A"))
(define r-big (staged d4 a4 1 (lambda (i) big)))
(want "SC-04 the single oversized commit is still an entry" (length (since-of r-big)) 1)
(want "SC-04 its actor is intact" (actors-of r-big) '("agent:0"))
;; KEY: THE SIZE IS THE BODY'S BYTES, computed here from the body and not
;; from the store's own encoding of the record. An expectation that ran
;; the implementation's calculation would agree with it whatever that
;; calculation was -- and it did: this row read
;; `(encoded-size (set ... src big))`, which counts the wrapper and the
;; escaping too.
(want "SC-04 its content is elided, with the body's size in bytes"
      (car (contents-of r-big))
      (list 'content 'elided (bytevector-length (string->utf8 big))))
(want "SC-04 and nothing was omitted, so it is not truncated"
      (field r-big 'truncated) #f)

;; ---- SC-05 the byte boundary ---------------------------------------------
;;
;; The limit is on the CONTENT's encoded size. The text is multi-byte so
;; that a build counting characters answers differently from one counting
;; bytes.

(define (at-size n)
  ;; `set` wraps the text, so the content's encoded size is the text's
  ;; plus a fixed wrapper; the row asserts the measured size rather than
  ;; assuming the wrapper's width.
  (make-string n #\x))

(define d5 (fresh-store!))
(define a5 (insert! d5 "A"))
(define r-edge (staged d5 a5 1 (lambda (i) (at-size 900))))
(want "SC-05 a content under the limit is handed back whole"
      (car (contents-of r-edge)) (list 'set a5 'src (at-size 900)))

(define d6 (fresh-store!))
(define a6 (insert! d6 "A"))
(define wide (list->string (map (lambda (i) #\x4e2d) (iota 400))))
(define r-wide (staged d6 a6 1 (lambda (i) wide)))
(want "SC-05 a multi-byte content over the limit is elided by BYTES"
      (car (contents-of r-wide))
      (list 'content 'elided (bytevector-length (string->utf8 wide))))
(want "SC-05 TWIN: and it really is under the limit in CHARACTERS"
      (< (string-length wide) 1024) #t)

;; ---- SC-05b the exact boundary, and escaping ------------------------------
;;
;; NOTE: THESE ROWS EXIST BECAUSE THE ONES ABOVE DID NOT PIN THE NUMBER.
;; 900 and 1200 bytes are both far from 1024: changing the limit to 1025
;; would leave every row above green. And a body of quotation marks is
;; the case that separates "the body's bytes" from "the encoded
;; content's bytes" -- 1024 quotes encode to 2066.

(define (quotes n) (make-string n #\"))

(define d10 (fresh-store!))
(define a10 (insert! d10 "A"))
(define r-1024 (staged d10 a10 1 (lambda (i) (quotes 1024))))
(want "SC-05b a body of exactly 1024 bytes is handed back whole"
      (car (contents-of r-1024)) (list 'set a10 'src (quotes 1024)))
(want "SC-05b TWIN: and that body encodes to far more than the limit"
      (> (encoded-size (list 'set a10 'src (quotes 1024))) 2000) #t)

(define d11 (fresh-store!))
(define a11 (insert! d11 "A"))
(define r-1025 (staged d11 a11 1 (lambda (i) (quotes 1025))))
(want "SC-05b one byte more is elided, and the number is the body's"
      (car (contents-of r-1025)) '(content elided 1025))
(want "SC-05b TWIN: neither of the two is truncated"
      (list (field r-1024 'truncated) (field r-1025 'truncated)) '(#f #f))

;; ---- SC-04b an elided entry keeps its identity ---------------------------
;;
;; The whole point of eliding rather than dropping is that the reader can
;; still find the record. A build that blanked the identity of elided
;; entries passes SC-04, SC-06 and SC-07 as they were written.

;; NOTE: THE SHAPE OF AN IDENTITY IS NOT THE IDENTITY. An earlier version
;; of this row asked only whether the entry carried a (string . integer)
;; pair -- which every forged pair does. Measured: replacing every
;; elided record's id with ("wrongxxx" . 0) satisfied it.
;;
;; The record the store said it wrote is the expectation, and it comes
;; from the store's own answer to the commit rather than from the
;; refusal being examined.
(define d13 (fresh-store!))
(define a13 (insert! d13 "A"))
(call d13 (list 'write a13 "mine") "test")
(define big-event (commit-by! d13 "agent:big" a13 big))
(define r-big2 (refuse! d13 a13))
(want "SC-04b the elided entry names the record the store wrote"
      (car (car (since-of r-big2))) big-event)
(want "SC-04b TWIN: and its content really was elided"
      (car (contents-of r-big2))
      (list 'content 'elided (bytevector-length (string->utf8 big))))

;; ---- SC-06 a big one in the middle ---------------------------------------

(define d7 (fresh-store!))
(define a7 (insert! d7 "A"))
(define r-mid (staged d7 a7 3 (lambda (i) (if (= i 1) big (string-append "small-" (number->string i))))))
(want "SC-06 all three identities are there" (length (since-of r-mid)) 3)
(want "SC-06 and the answer is not truncated" (field r-mid 'truncated) #f)
(want "SC-06 only the big one's content is elided"
      (map (lambda (c) (and (pair? c) (eq? 'content (car c)) #t)) (contents-of r-mid))
      '(#f #f #t))

;; KEY: AND THE ELIDED ONE IS STILL THE RECORD IT WAS. SC-04b checks a
;; singleton, where the entry is the winner; a build that blanked the
;; identity of NON-winner elided entries passes that one. Here the big
;; body belongs to agent:1, which the answer places LAST -- winner
;; first, then the tail in causal order, so agent:2, agent:0, agent:1.
;; The slot and the name are two different claims: the row above counts
;; slots, this one asks whose record is in the elided one.
(want "SC-06 TWIN: the elided entry keeps its actor, not only its slot"
      (list-ref (actors-of r-mid) 2) "agent:1")
(want "SC-06 TWIN: and each entry carries a distinct record id"
      (let ((ids (map car (since-of r-mid))))
        (list (length ids) (length (filter (lambda (i) (member i (cdr (memq i ids)))) ids))))
      '(3 0))

;; ---- SC-07 nine of mixed size --------------------------------------------

(define d8 (fresh-store!))
(define a8 (insert! d8 "A"))
(define r-mixed (staged d8 a8 9 (lambda (i) (if (even? i) big (string-append "s" (number->string i))))))
(want "SC-07 nine give eight, big ones taking their slots" (length (since-of r-mixed)) 8)
(want "SC-07 and the answer is truncated" (field r-mixed 'truncated) '(truncated #t))
(want "SC-07 the winner is first and whole-identity"
      (car (actors-of r-mixed)) "agent:8")
(want "SC-07 the same holds when the entries are of mixed size"
      (cdr (actors-of r-mixed))
      '("agent:1" "agent:2" "agent:3" "agent:4" "agent:5" "agent:6" "agent:7"))

;; ---- SC-08 a long actor ---------------------------------------------------

(define d9 (fresh-store!))
(define a9 (insert! d9 "A"))
(call d9 (list 'write a9 "mine") "test")
(define long-actor (string-append "agent:" (make-string 9000 #\a)))
(commit-by! d9 long-actor a9 "short body")
(define r-actor (refuse! d9 a9))
(want "SC-08 the entry is there" (length (since-of r-actor)) 1)
(want "SC-08 and the actor is preserved whole" (car (actors-of r-actor)) long-actor)
(want "SC-08 with its content, which is short" (car (contents-of r-actor))
      (list 'set a9 'src "short body"))

;; ---- SC-09 a refusal with nothing to hand back says why -------------------
;;
;; The hash can differ with NO record about this block after the draft's
;; cut: a commit was retracted, and the block went back to what it was.
;; NEVER: An empty `since` alone leaves the client guessing whether the store
;; forgot or there is genuinely nothing; the contract carries a reason
;; and a place to look instead.
;;
;; NOTE: THIS ROW EXISTS BECAUSE THE BRANCH HAD NO GUARD. Deleting the two
;; lines that append the reason left every other row in this file green:
;; none of them constructs a refusal with zero relevant records.

(define d12 (fresh-store!))
(define a12 (insert! d12 "A"))
(define h-before (block-hash (open-and-reduce d12) a12))
;; THE WRITER IS READ OFF THE BLOCK ID, which is `<writer>.<seq>`.
;; Asking `init` again answers `already-initialised`, which is correct
;; and is not a way to learn the writer's name.
(define cursor12
  (let loop ((i 0))
    (cond ((= i (string-length a12)) a12)
          ((char=? #\. (string-ref a12 i)) (substring a12 0 i))
          (else (loop (+ i 1))))))
(call d12 (list 'write a12 "first") "test")
(define v12 (list-ref (assq 'projection (cdr (call d12 (list 'read a12 "--working-info") "test"))) 4))
;; `--req` NEEDS ITS CURSOR: a request id without one is refused before
;; anything else, which is `req-without-cursor` and not this row's
;; question.
(define commit12
  (call d12 (list 'commit a12 "--req" "R1" "--working-version" v12
                  "--cursor" (string-append cursor12 ":"
                                            (number->string
                                              (cdr (assoc cursor12 (reduce-applied-cut (open-and-reduce d12)))))))
        "test"))
(want "SC-09 the commit landed" (rpc-ok? commit12) #t)
(define h-after (block-hash (open-and-reduce d12) a12))
(want "SC-09 TWIN: and it moved the block" (equal? h-before h-after) #f)

;; A draft taken against the committed state.
(call d12 (list 'write a12 "mine" "--based-on" h-after
                "--working-cut" (format "~s" (reduce-applied-cut (open-and-reduce d12)))) "test")

;; Now the commit is retracted: a second record claims its identity.
(define ev12 (store-evidence d12 (cons cursor12 "R1")))
(define plan12 (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) ev12))
(define rival12
  (encode-record 1 1789000000002 (ev-actor plan12) '() (storable-encode (ev-payload plan12))))
(want "SC-09 the rival record publishes"
      (car (log-publish! d12 "rivalzzz" 1 rival12 (segment-sha rival12))) 'published)
(want "SC-09 TWIN: and the block is back where it started"
      (block-hash (open-and-reduce d12) a12) h-before)

(define r-none (call d12 (list 'commit a12) "test"))
(want "SC-09 the commit is refused" (kind-of r-none) 'stale-baseline)
(want "SC-09 with nothing to hand back" (since-of r-none) '())
(want "SC-09 so it says why instead of leaving an empty list"
      (field r-none 'reason) '(reason candidate-set-changed))
(want "SC-09 and where to look" (field r-none 'conflicts) (list 'conflicts a12))
;; NEVER: AND IT IS NOT MARKED TRUNCATED. Nothing was omitted -- there was
;; nothing to omit -- and an answer that said otherwise would send the
;; reader to `log` for a history that holds no relevant record at all.
(want "SC-09 TWIN: an empty answer is not a truncated one"
      (field r-none 'truncated) #f)
(want "SC-09 TWIN: and it offers no retrieve either"
      (field r-none 'retrieve) #f)

(printf "rows: ~a\n~a failures\nsince-contract complete\n" rows-run failures)
