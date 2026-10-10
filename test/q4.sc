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

;; Batch receipts: what a retry can say about a batch it may have run.
;;
;; A receipt is written and made durable before the first item executes.
;; Without one, a crash between two items leaves a set of records and no
;; statement of what the set was supposed to be -- "item 3 is missing"
;; and "there were only three items" are then the same picture. With one,
;; the retry can ask about indices the batch DECLARED rather than about
;; indices it can infer from the survivors.
;;
;; These are pure functions of a set of records and the rows below feed
;; them sets built by hand, for the same reason the rules in q2 are fed
;; that way: the state must be the same for the same records however they
;; arrived, so a case that is awkward to produce on disk -- a second
;; receipt, an item whose dependencies do not name the receipt -- is
;; still a case the rules can be asked about directly.
;;
;; THE ORDER OF THE TESTS IS AS MUCH THE SUBJECT AS THE ANSWERS ARE.
;; Every row here reads as much for which rule answered as for what it
;; said: a store that cannot say what it holds must not answer a question
;; about what the client did, so a quarantined record outranks a
;; fingerprint that disagrees.

(import (chezscheme) (theourgia request))

(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define W "w3kxxxxx")
(define WHO "agent:claude")
(define AFTER (cons W 12))
(define RECEIPT-EVENT (cons W 13))
(define FP (request-fingerprint WHO 'batch (list "b" "t" "x") AFTER))

;; The receipt as the design writes it: the batch's request id, the
;; fingerprint of its whole immutable content, the cursor it was written
;; against, and the cursor each item's record was to be written against.
(define (receipt entries)
  (list 'batch "b1" FP AFTER entries))
;; THE DECLARED CURSOR IS A PRE-WRITE CURSOR, so an item's own event
;; sits strictly ABOVE the cursor the receipt recorded for it: item 0 is
;; written against 13 and lands at 14. A fixture that declared 14 for the
;; record at 14 would be asserting a position no append can produce, and
;; every row that reads a cursor would be reading a number the product
;; could never see.
(define THREE (list (cons 0 (cons W 13)) (cons 1 (cons W 14)) (cons 2 (cons W 15))))
(define R (receipt THREE))

;; An item record. Its identity is the batch item, its sub-operation
;; index is its position, and its dependencies name the receipt -- which
;; is what makes it evidence of THIS batch rather than a record that
;; happens to share an identity.
(define (item index seq . opts)
  (let ((placement (if (pair? opts) (car opts) 'valid-history))
        (marks (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) '()))
        (deps (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts)))
                  (caddr opts)
                  (list RECEIPT-EVENT)))
        ;; `single`, WHICH IS WHAT A BATCH ITEM ACTUALLY CARRIES. An
        ;; item of a batch is its own request; its actor's slot describes
        ;; that item's OWN internal shape, and an item that is one record
        ;; is `single` with no plan above it -- `item-actor` in the write
        ;; path says exactly that. This defaulted to the BATCH index,
        ;; which is the item's identity's index and not its slot: rule 6
        ;; above says in so many words that the two answer different
        ;; questions, and this helper was reading one for the other. It
        ;; went unnoticed while the batch layer looked only at the
        ;; identity; it stopped being invisible the moment the layer
        ;; started asking each item's own request whether it ran.
        (sub (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts))
                      (pair? (cdddr opts)))
                 (cadddr opts)
                 'single)))
    ;; The actor carries this item's OWN cursor, the one the receipt
    ;; declared for it -- not the batch's.
    (make-evidence (cons W seq)
                   (list WHO (cons W (list 'batch "b1" index)) sub FP #f
                         (cons W (- seq 1)))
                   deps
                   (list 'set "b" "t" "x")
                   placement #t marks)))

(define (receipt-record r)
  (make-evidence RECEIPT-EVENT
                 (list WHO (cons W "b1") 'plan FP #f AFTER)
                 '() r 'valid-history #t '()))

(define (state n evidence . opts)
  (let ((intervals (if (pair? opts) (car opts) '()))
        (chain (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #t)))
    (batch-state n evidence FP AFTER intervals chain '())))

(printf "== Q11: the receipt is checked whole ==\n")
;; A receipt read from a log was written by some version of this program,
;; and a half-understood one is worse than an unreadable one: it answers
;; some questions and lies about the rest.
(want "the design's shape is accepted"
      (receipt? R)
      #t)
(want "and so is a batch of no items"
      (receipt? (receipt '()))
      #t)
(want "everything that is not that shape is refused"
      (map receipt? (list (list 'batch "b1" FP AFTER)
                          (list 'plan "b1" FP AFTER THREE)
                          (list 'batch "b 1" FP AFTER THREE)
                          (list 'batch "b1" 'not-a-hash AFTER THREE)
                          (list 'batch "b1" FP (list W 12) THREE)
                          (list 'batch "b1" FP AFTER 'not-a-list)))
      '(#f #f #f #f #f #f))
;; ASCENDING AND DISTINCT IS A STATEMENT ABOUT THE WRITING, not a
;; preference: items are written in index order, so entries out of order
;; or repeating an index did not come from a correct execution.
(want "entries out of order, repeated, or malformed are refused"
      (map (lambda (e) (receipt? (receipt e)))
           (list (list (cons 1 (cons W 15)) (cons 0 (cons W 14)))
                 (list (cons 0 (cons W 14)) (cons 0 (cons W 15)))
                 (list (cons -1 (cons W 14)))
                 (list (list 0 W 14))
                 (list (cons 0 (cons W -1)))))
      '(#f #f #f #f #f))

(printf "\n== Q11: what the receipt is asked ==\n")
(want "each index's own cursor, and #f for one it does not cover"
      (list (receipt-item-after R 0) (receipt-item-after R 2) (receipt-item-after R 3))
      (list (cons W 13) (cons W 15) #f))
;; NOT-STARTED IS ABOUT THE RECEIPT, NOT ABOUT THE RECORDS. An index the
;; receipt does not list was never part of this batch, so there is
;; nothing for a retry to finish and nothing to be uncertain about -- the
;; one state that can be answered without reading any history.
(want "an index the receipt lists is covered; one it does not is not-started"
      (list (batch-item-state R 2) (batch-item-state R 3))
      '(covered not-started))
(want "and with no receipt at all no index can be answered for"
      (batch-item-state #f 0)
      'unknown)

(printf "\n== Q11': a second receipt is integrity, not a choice ==\n")
;; Two receipts are two statements of what the batch was. Picking either
;; would make the answer depend on which the scan reached first.
(want "one receipt is not a duplicate"
      (receipt-duplicate (list (receipt-record R)))
      #f)
(want "a second one is named, and it is the second in log order"
      (receipt-duplicate
        (list (receipt-record R)
              (make-evidence (cons W 20)
                             (list WHO (cons W "b1") 'plan FP #f AFTER)
                             '() (receipt '()) 'valid-history #t '())))
      (list 'batch-duplicate (cons W 20)))
(want "and the batch then has no state to report"
      (state 3 (list (receipt-record R)
                     (make-evidence (cons W 20)
                                    (list WHO (cons W "b1") 'plan FP #f AFTER)
                                    '() (receipt '()) 'valid-history #t '())))
      (list 'unknown (list 'batch-duplicate (cons W 20))))

(printf "\n== Q11': the retry recomputes the fingerprint ==\n")
;; The id was reused for different content. Executing would append a
;; second, different batch under an id that already names one.
(want "a receipt whose fingerprint disagrees with the request in hand"
      (batch-state 3 (list (receipt-record (list 'batch "b1" "other" AFTER THREE)))
                   FP AFTER '() #t '())
      (list 'unknown (list 'req-mismatch "other")))
(want "TWIN: the same receipt when the fingerprints agree"
      (state 3 (list (receipt-record R)))
      '(planned))

(printf "\n== Q12: the states ==\n")
;; NOT-COMMITTED IS A CLAIM ABOUT THE FUTURE OF A CURSOR, not about an
;; empty list: nothing was found AND the positions this batch could
;; occupy are clear of every stretch the store is unsure about.
(want "no receipt, no evidence, and a clear cursor: it did not begin"
      (state 3 '())
      '(not-committed))
(want "TWIN: the same emptiness with the cursor inside an uncertain stretch"
      (state 3 '() (list (list W 10 20)))
      (list 'unknown (list 'range-overlaps (list W 10 20))))
;; THE ONE THING WRITTEN BEFORE EVERYTHING ELSE IS THE ONE THING MISSING.
;; Reconstructing the batch from the items that survive would be taking
;; the survivors for the whole.
(want "item records but no receipt"
      (state 3 (list (item 0 14)))
      '(unknown (receipt-absent)))
(want "the receipt and nothing else"
      (state 3 (list (receipt-record R)))
      '(planned))
(want "the receipt and every item"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15) (item 2 16)))
      (list 'committed '(0 1 2) (cons W 16)))
(want "the receipt and a prefix of the items"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15)))
      '(partial (0 1)))
;; A SET THAT IS NOT A PREFIX IS NOT "SOMETHING IS MISSING". Items are
;; written in index order, so {0,2} is a set no correct execution
;; produces and the store may not guess which story it is looking at.
(want "a set of items that is not a prefix"
      (state 3 (list (receipt-record R) (item 0 14) (item 2 16)))
      '(unknown (plan-order (0 2))))

;; A RECEIPT THAT COVERS LESS THAN THE REQUEST IS NOT A BATCH THAT GOT
;; PART OF THE WAY. The request declares three items; this receipt
;; mentions two. Index 2 is `not-started` as an item -- nothing to
;; finish -- but the batch cannot be called `partial` over it, because
;; `partial` is the claim that the rest provably did not run and the one
;; record written before anything else does not mention them at all.
;; The fingerprint cannot catch this: it is taken over the request's
;; content and the entry list is not part of the content.
(want "a receipt that covers fewer indices than the request declares"
      (state 3 (list (receipt-record (receipt (list (cons 0 (cons W 13))
                                                   (cons 1 (cons W 14)))))
                     (item 0 14) (item 1 15)))
      '(unknown (receipt-coverage 2)))
(want "TWIN: with the request declaring only the two it covers, it is committed"
      (state 2 (list (receipt-record (receipt (list (cons 0 (cons W 13))
                                                   (cons 1 (cons W 14)))))
                     (item 0 14) (item 1 15)))
      (list 'committed '(0 1) (cons W 15)))

(printf "\n== Q12': an intention is not an execution ==\n")
;; AN ITEM THAT IS SEVERAL RECORDS WRITES ITS OWN PLAN FIRST, and that
;; plan record carries the ITEM'S identity -- so it carries the item's
;; index. The index says which item a record belongs to; it does not say
;; that the item ran. Counting the plan record as the item being present
;; gives a batch that stopped just after writing its plans the same word
;; a batch that ran to the end gets, and names a plan record as the
;; record the promise reaches.
;;
;; THE TWO LAYERS DISAGREED, AND THE BATCH LAYER WAS THE WRONG ONE. The
;; plan layer's `present-indices` already refuses these records, because
;; `executed-member-set` asks what ran; only the batch layer's version
;; read the identity and stopped there.
(want "a plan record is not the item it belongs to"
      (state 2 (list (receipt-record (receipt (list (cons 0 (cons W 13)) (cons 1 (cons W 14)))))
                     (item 0 14 'valid-history '() (list RECEIPT-EVENT) 'plan)
                     (item 1 15 'valid-history '() (list RECEIPT-EVENT) 'plan)))
      '(unknown (evidence-names-missing 0)))
;; AND THE ANSWER IS NOT `planned` EITHER. `planned` says the rest
;; provably did not run, and a plan record naming index 0 is evidence
;; that something once wrote that index down -- which is exactly the
;; question `item-naming-missing` asks, and why THAT function must go on
;; counting plan records while this one must not.
(want "TWIN: the receipt alone, with nothing naming an index, is still planned (section-13 L21)"
      (state 2 (list (receipt-record (receipt (list (cons 0 (cons W 13)) (cons 1 (cons W 14)))))))
      '(planned))
;; TWIN: THE SAME RECORDS WITH THE SLOT AN EXECUTION USES. Without this
;; the row above would pass for an implementation that had stopped
;; counting items altogether.
(want "TWIN: the same two items with an execution's slot are committed"
      (state 2 (list (receipt-record (receipt (list (cons 0 (cons W 13)) (cons 1 (cons W 14)))))
                     (item 0 14 'valid-history '() (list RECEIPT-EVENT) 'single)
                     (item 1 15 'valid-history '() (list RECEIPT-EVENT) 'single)))
      (list 'committed '(0 1) (cons W 15)))
;; AND AN INDEXED SLOT IS AN EXECUTION TOO -- it is how an item that is
;; several records names its own sub-operations. A test that admitted
;; only `single` would refuse every multi-record item.
;; AN INDEXED SLOT BELONGS TO AN ITEM THAT IS SEVERAL RECORDS, and only
;; there. An item whose actor carries an index while pointing at no plan
;; is a record whose two statements about where it belongs disagree --
;; rule 6 says so -- so the twin for the indexed case is an item that has
;; a plan of its own and carries it out, not a `single` item wearing an
;; index. An earlier version of this row asserted the latter, and it
;; passed only while the batch layer read the identity and stopped.
(want "TWIN: an item that is several records completes when its plan does"
      (let* ((ipe (cons W 14))
             (iplan (list 'plan "b1" FP (cons W 13)
                          (list (cons 0 (list 'set "b" "t" "x")))))
             (plan-rec
               (make-evidence ipe
                              (list WHO (cons W (list 'batch "b1" 0)) 'plan FP #f (cons W 13))
                              (list RECEIPT-EVENT) iplan 'valid-history #t '()))
             (member-rec
               (make-evidence (cons W 15)
                              (list WHO (cons W (list 'batch "b1" 0)) 0 FP ipe (cons W 13))
                              (list RECEIPT-EVENT) (list 'set "b" "t" "x")
                              'valid-history #t '())))
        (batch-state 1 (list (receipt-record (receipt (list (cons 0 (cons W 13)))))
                             plan-rec member-rec)
                     FP AFTER '() #t '()))
      (list 'committed '(0) (cons W 15)))
;; A HALF-PLANNED BATCH: item 0 ran, item 1 only wrote its plan. The
;; plan record names index 1, so `partial` -- which claims the rest
;; provably did not run -- is not available.
(want "an item that only planned does not extend the prefix"
      (state 2 (list (receipt-record (receipt (list (cons 0 (cons W 13)) (cons 1 (cons W 14)))))
                     (item 0 14 'valid-history '() (list RECEIPT-EVENT) 'single)
                     (item 1 15 'valid-history '() (list RECEIPT-EVENT) 'plan)))
      '(unknown (evidence-names-missing 1)))

(printf "\n== Q12': partial asks for four things, not one ==\n")
;; (i) the chain must read to its end, or "there are no records past
;; here" is a statement about how far the READER got.
(want "a prefix, but the writer chain does not read to its end"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15)) '() #f)
      '(unknown (chain-unreadable)))
;; (ii) nothing anywhere may name a missing index. A record set aside as
;; superseded says nothing about what the batch did, and everything about
;; whether the index it names is really untouched.
(want "a prefix, but a superseded record names the missing index"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15)
                     (item 2 16 'valid-history '(superseded))))
      '(unknown (evidence-names-missing 2)))
;; (iii) each missing index is asked about ITS OWN cursor. The batch's
;; cursor is 12 and it is clear of this stretch; the missing item's
;; declared cursor is 16 and it is not. Taking the batch's cursor for all
;; of them would miss an interval that sits between two items.
(want "a prefix, and an uncertain stretch over the missing item's own cursor"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15))
             (list (list W 16 18)))
      (list 'unknown (list 'range-overlaps (list W 16 18))))
(want "TWIN: a stretch that ends below the missing item's cursor is not in the way"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15))
             (list (list W 10 14)))
      '(partial (0 1)))
;; AND THAT SAME STRETCH IS NOT CLEAR OF THE BATCH'S OWN CURSOR, which is
;; 12. So the two rows above differ in nothing but which cursor was
;; asked, and the per-item cursor is what the receipt is FOR: without it
;; the only cursor to hand is the batch's, and this batch would be
;; reported uncertain about an item written eleven positions away from
;; anything the store is unsure of.
(want "the same stretch is in the way of the batch's own cursor"
      (state 3 '() (list (list W 10 14)))
      (list 'unknown (list 'range-overlaps (list W 10 14))))
;; (iv) every item must name the receipt. One that does not cannot have
;; been written by an execution that went through the receipt, and
;; counting it would let a stray record finish a batch.
;; NAMING THE RECEIPT IN `deps` IS ONE OF TWO WAYS TO BE LINKED TO IT,
;; and on the same writer it is not the one that is used: a record's
;; dependencies list the OTHER writers it builds on, because the sequence
;; already says which of two records on one writer came first. So this
;; item is written on a DIFFERENT writer, where there is no such order to
;; appeal to and the dependency is the only link there is.
(define (foreign-item index seq deps)
  (make-evidence (cons "otherwrt" seq)
                 (list WHO (cons W (list 'batch "b1" index)) 'single FP #f
                       (cons "otherwrt" (- seq 1)))
                 deps (list 'set "b" "t" "x") 'valid-history #t '()))
;; THE RECEIPT HAS TO HAVE RECORDED THE CURSOR THE ITEM WAS WRITTEN AT,
;; and for an item on another writer that cursor is on that writer. A
;; receipt naming this writer for all three while the third item sits on
;; another is a receipt that contradicts its own batch -- and it is now
;; caught as one, so these rows carry a receipt that agrees with where
;; the items actually are.
(define R-FOREIGN
  (receipt (list (cons 0 (cons W 13)) (cons 1 (cons W 14))
                 (cons 2 (cons "otherwrt" 39)))))
(want "an item on another writer whose dependencies do not name the receipt"
      (state 3 (list (receipt-record R-FOREIGN) (item 0 14) (item 1 15)
                     (foreign-item 2 40 '())))
      (list 'unknown (list 'receipt-unlinked (cons "otherwrt" 40))))
(want "TWIN: the same item with the receipt among its dependencies"
      (state 3 (list (receipt-record R-FOREIGN) (item 0 14) (item 1 15)
                     (foreign-item 2 40 (list (cons W 99) RECEIPT-EVENT))))
      (list 'committed '(0 1 2) (cons "otherwrt" 40)))
;; TWIN: and on the receipt's own writer, after it, the log's order IS
;; the link -- repeating it in `deps` would be a second supplier of an
;; ordering the sequence already gives.
(want "TWIN: an item after the receipt on its own writer needs no dependency"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15)
                     (item 2 16 'valid-history '() '())))
      (list 'committed '(0 1 2) (cons W 16)))

(printf "\n== Q12': planned is partial with k = -1 ==\n")
;; "The receipt is durable and items 0..k ran, and the rest provably did
;; not" is what `partial` says; `planned` is that sentence with k = -1.
;; They pass the same tests. An arm that answered `planned` on the
;; strength of an empty item list alone would be answering "nothing that
;; I can READ ran" -- a torn record leaves an uncertain stretch and no
;; readable identity, so it is invisible to every test that looks at
;; records and visible only to the one that looks at intervals. And a
;; client reading `planned` starts at index 0.
(want "the receipt alone, with a stretch over the first item's position"
      (state 3 (list (receipt-record R)) (list (list W 13 20)))
      (list 'unknown (list 'range-overlaps (list W 13 20))))
(want "the receipt alone, with the writer chain unreadable"
      (state 3 (list (receipt-record R)) '() #f)
      '(unknown (chain-unreadable)))
(want "TWIN: the receipt alone with neither is planned"
      (state 3 (list (receipt-record R)))
      '(planned))

(printf "\n== Q12': the receipt must match the request in both directions ==\n")
;; A receipt listing MORE indices than the request describes a different
;; batch. Checking only the short direction reports `committed` for a
;; request of two items against a receipt for three: the extra index is
;; simply never asked about.
(want "a receipt that covers more indices than the request declares"
      (state 2 (list (receipt-record R) (item 0 14) (item 1 15)))
      '(unknown (receipt-coverage 2)))
;; AN ITEM SAYS WHICH INDEX IT IS TWICE -- in its identity and in its
;; actor's sub-operation slot. Every rule reads the slot, so a record
;; whose identity claims 2 and whose slot says `single` is counted by
;; nothing: not as present, not as naming a missing index. The retry is
;; then told to resume at an index a record already stands at.
;; AN ITEM'S INDEX IS ITS IDENTITY'S, NOT ITS ACTOR'S SLOT. Each item of
;; a batch is its own request -- `(batch <req> <k>)` -- and its actor's
;; sub-operation slot describes that item's OWN internal shape: `single`
;; when it is one record, an index into its own plan when it is several.
;; The two answer different questions. A record whose identity carries no
;; index at all cannot say which item it is, and stops the batch.
(want "an item whose identity carries no index"
      (state 3 (list (receipt-record R) (item 0 14) (item 1 15)
                     (make-evidence (cons W 16)
                                    (list WHO (cons W "not-an-item") 'single FP #f
                                          (cons W 15))
                                    (list RECEIPT-EVENT) (list 'set "b" "t" "x")
                                    'valid-history #t '())))
      (list 'unknown (list 'item-index-missing (cons W 16))))
;; A RECEIPT THAT WAS SET ASIDE IS NOT A RECEIPT. It is the record every
;; other answer is built on, so a marked one leaves the batch with no
;; receipt -- not a quiet fall-back to the next one found.
(want "a superseded receipt leaves the batch without one"
      (state 3 (list (make-evidence RECEIPT-EVENT
                                    (list WHO (cons W "b1") 'plan FP #f AFTER)
                                    '() R 'valid-history #t '(superseded))
                     (item 0 14)))
      '(unknown (receipt-absent)))
;; THE SIZE IS NOT OPTIONAL. Without it the coverage test and every
;; missing-index test have nothing to range over and quietly pass.
;; AND THE REFUSAL IS READ BY NAME, not by the fact that something was
;; raised. Without the check, `n` reaches an arithmetic comparison and
;; that raises too -- so a row catching any condition at all would pass
;; whether the check was there or not, which is no assertion.
(want "a retry that cannot say how many items the batch has is refused, by name"
      (guard (e ((assertion-violation? e) (list (condition-who e) (condition-message e)))
                (#t (list 'some-other-condition)))
        (batch-state #f (list (receipt-record R)) FP AFTER '() #t '()))
      '(batch-state "the request's item count is required"))

(printf "\n== Q12': unreadable outranks every comparison ==\n")
;; A store that cannot say what it holds must not answer a question about
;; what the client did. If the fingerprint check came first, a client
;; would be told `req-mismatch`, would "fix" its request id, and would
;; execute the batch a second time.
(want "a quarantined item beside a fingerprint that disagrees"
      (batch-state 3 (list (receipt-record (list 'batch "b1" "other" AFTER THREE))
                           (item 0 14 'quarantined))
                   FP AFTER '() #t '())
      (list 'unknown (list 'quarantined (cons W 14))))
(want "TWIN: with the item readable, the fingerprint is what answers"
      (batch-state 3 (list (receipt-record (list 'batch "b1" "other" AFTER THREE))
                           (item 0 14))
                   FP AFTER '() #t '())
      (list 'unknown (list 'req-mismatch "other")))
;; A duplicate receipt outranks even that: there is no receipt to compare
;; a fingerprint against when there are two of them.
(want "two receipts beside a fingerprint that disagrees"
      (batch-state 3 (list (receipt-record (list 'batch "b1" "other" AFTER THREE))
                           (make-evidence (cons W 20)
                                          (list WHO (cons W "b1") 'plan FP #f AFTER)
                                          '() (receipt '()) 'valid-history #t '()))
                   FP AFTER '() #t '())
      (list 'unknown (list 'batch-duplicate (cons W 20))))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q4 complete\n")
