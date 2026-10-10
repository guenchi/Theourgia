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

;; WHAT A COMMIT CONSUMED, READ OFF THE LOG.
;;
;; A draft is consumed once a plan that named its version has COMPLETED.
;; Until this existed the fact lived beside the draft, in an immutable
;; packet per request, which meant two stores to keep in step and a scan
;; of all the packets on every read of a draft.
;;
;; NOTE: NOTHING IN THIS CORE WRITES A SIX-ELEMENT PLAN YET. These rows put
;; the records into a reduction directly, which is what the index is
;; built from, and they are here before the writer exists on purpose: a
;; reader that cannot yet be produced is a reader nobody has measured.
;;
;; THE TWO HARD ROWS ARE THE ONES ABOUT TIME.
;;
;;   DELIVERED IS NOT APPLIED. A plan and its member can both be on disk
;;   while the plan waits on a dependency that has not arrived. Nothing
;;   has been applied, so nothing has been consumed -- an index fed from
;;   "the records are here" says the opposite.
;;
;;   A SNAPSHOT CAN BE CUT MID-PLAN. Its `consumed` row therefore has to
;;   carry the bookkeeping -- how many members were declared, which have
;;   been applied -- or a resumed reduction can never finish a plan whose
;;   last member arrives after the cut.

(import (chezscheme) (theourgia reduce) (theourgia request))

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
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define origin "plan0000")
(define after (cons origin 0))
(define key (cons origin "p"))
;; THE DECLARED INTENT AND THE MEMBER'S PAYLOAD ARE NOT THE SAME DATUM.
;; A plan declares `(insert root #f ((kind . section) (title . T)))`; the
;; record that carries it out is the `put` that intent produces, with the
;; parent and ord filled in. Using the intent as the member's payload
;; gave `plan-mismatch` and a member that never applied -- measured, and
;; it read as "the index saw nothing" rather than as a malformed fixture.
(define (intent title)
  (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title title))))
(define (produced title)
  (list 'put (list (cons 'kind 'section) (cons 'title title)
                   '(parent . root) '(ord . 1))))
(define (item block version) (list block version "h0" '()))
(define (plan-payload entries consumes)
  (if consumes
      (list 'plan "p" "fp" after entries consumes)
      (list 'plan "p" "fp" after entries)))
(define (plan-actor) (list "test" key 'plan "fp" #f after))
(define (member-actor i plan-event) (list "test" key i "fp" plan-event after))

;; ---- CI-00 the instrument -------------------------------------------------

(define one (reduce-empty))
(reduce-apply! one origin 1 '()
               (plan-payload (list (cons 0 (intent "a")))
                             (list 'consumes "dw" (list (item "dw.1" "v1"))))
               (plan-actor))
(want "CI-00 a six-element plan is admitted without a gate" (reduce-gates one) '())
(reduce-apply! one origin 2 '() (produced "a") (member-actor 0 (cons origin 1)))
(want "CI-00 and its member applies" (length (state-datum one)) 1)

;; ---- CI-01 completion -----------------------------------------------------

(want "CI-01 the draft it named is consumed" (state-consumed? one "dw" "v1") #t)
(want "CI-01 TWIN: a version it did not name is not"
      (state-consumed? one "dw" "v2") #f)
(want "CI-01 TWIN: nor the same version under another owner"
      (state-consumed? one "other" "v1") #f)
(want "CI-01 completion is the member event, not the plan event"
      (list-ref (car (state-consumption one)) 5) (cons origin 2))

;; ---- CI-02 delivered is not applied ---------------------------------------
;;
;; The plan depends on a record from another writer that has not arrived,
;; so neither it nor its member can be applied although both are here.

(define waiting (reduce-empty))
(reduce-apply! waiting origin 1 '(("base0000" . 1))
               (plan-payload (list (cons 0 (intent "b")))
                             (list 'consumes "dw" (list (item "dw.1" "v9"))))
               (plan-actor))
(reduce-apply! waiting origin 2 '() (produced "b") (member-actor 0 (cons origin 1)))
(want "CI-02 both records are in the history"
      (length (reduce-pending waiting)) 2)
(want "CI-02 and nothing is consumed while they wait"
      (state-consumed? waiting "dw" "v9") #f)
(reduce-apply! waiting "base0000" 1 '() (produced "base") "ordinary")
(want "CI-02 the dependency arrives and the consumption appears"
      (state-consumed? waiting "dw" "v9") #t)

;; ---- CI-03 an empty plan --------------------------------------------------

(define empty (reduce-empty))
(reduce-apply! empty origin 1 '()
               (plan-payload '() (list 'consumes "dw" (list (item "dw.1" "v0"))))
               (plan-actor))
(want "CI-03 an empty plan consumes at once" (state-consumed? empty "dw" "v0") #t)
(want "CI-03 and its completion is the plan event itself"
      (list-ref (car (state-consumption empty)) 5) (cons origin 1))

;; ---- CI-04 the snapshot carries it ----------------------------------------

(want "CI-04 a snapshot round trip preserves the index"
      (state-consumption (rows->state (state->rows one)))
      (state-consumption one))
(want "CI-04 and the answer it gives"
      (state-consumed? (rows->state (state->rows one)) "dw" "v1") #t)

;; ---- CI-05 a snapshot without the row -------------------------------------
;;
;; KEY: THE TWO PATHS HAVE TO AGREE. One reads the row; the other rebuilds
;; from the records the snapshot carries. A cell that only checked the
;; first would be green against a build whose second path returns
;; nothing, and that build makes every consumed draft visible again.

(define (drop-consumed rows)
  (filter (lambda (row) (not (eq? 'consumed (car row)))) rows))

(want "CI-05 a snapshot with no consumed row rebuilds the same index"
      (state-consumption (rows->state (drop-consumed (state->rows one))))
      (state-consumption one))
(want "CI-05 and the same answer"
      (state-consumed? (rows->state (drop-consumed (state->rows one))) "dw" "v1") #t)
(want "CI-05 the empty plan rebuilds too"
      (state-consumption (rows->state (drop-consumed (state->rows empty))))
      (state-consumption empty))

;; ---- CI-06 a snapshot cut before the last member --------------------------
;;
;; This is the row the bookkeeping exists for. The plan has two members;
;; one is applied, the snapshot is cut, the reduction is resumed from it,
;; and the second member arrives afterwards. It must complete -- and give
;; the same index as folding all four records without a snapshot.

(define (two-member-fold deliver-second-before-cut?)
  (let ((r (reduce-empty)))
    (reduce-apply! r origin 1 '()
                   (plan-payload (list (cons 0 (intent "x")) (cons 1 (intent "y")))
                                 (list 'consumes "dw" (list (item "dw.1" "v7"))))
                   (plan-actor))
    (reduce-apply! r origin 2 '() (produced "x") (member-actor 0 (cons origin 1)))
    (when deliver-second-before-cut?
      (reduce-apply! r origin 3 '() (produced "y") (member-actor 1 (cons origin 1))))
    r))

(define whole (two-member-fold #t))
(want "CI-06 a two-member plan completes at the GREATER member event"
      (list-ref (car (state-consumption whole)) 5) (cons origin 3))
(want "CI-06 TWIN: and both members are recorded, so the choice is a choice"
      (map car (list-ref (car (state-consumption whole)) 4)) '(0 1))

(define mid (two-member-fold #f))
(want "CI-06 a plan with a member outstanding is not consumed"
      (state-consumed? mid "dw" "v7") #f)
(define resumed (rows->state (state->rows mid)))
(reduce-apply! resumed origin 3 '() (produced "y") (member-actor 1 (cons origin 1)))
(want "CI-06 the last member arrives after the cut and completes the plan"
      (state-consumed? resumed "dw" "v7") #t)
(want "CI-06 and the resumed index equals the one folded straight through"
      (state-consumption resumed)
      (state-consumption (two-member-fold #t)))

;; ---- CI-07 the old shapes still load --------------------------------------

(define five (reduce-empty))
(reduce-apply! five origin 1 '() (plan-payload (list (cons 0 (intent "z"))) #f)
               (plan-actor))
(reduce-apply! five origin 2 '() (produced "z") (member-actor 0 (cons origin 1)))
(want "CI-07 a five-element plan applies and consumes nothing"
      (list (length (state-datum five)) (state-consumption five)) '(1 ()))
(want "CI-07 a snapshot of it round trips"
      (state-consumption (rows->state (state->rows five))) '())
(want "CI-07 and so does one with the row removed"
      (state-consumption (rows->state (drop-consumed (state->rows five)))) '())

;; ---- CI-08 a rebuild does not lose the index -----------------------------
;;
;; `rebuild-request-state!` drops the whole state and folds the retained
;; records again, and the index is dropped with it. A build that KEPT the
;; old index across a rebuild would pass every row above, and would then
;; disagree with the records exactly when a record was taken back.
;;
;; NOTE: THE ROW HAS TO MAKE THE REBUILD HAPPEN. An unrelated arrival does
;; not: a rebuild is forced only by a record that reverses a membership
;; that was already applied. The first version of this row appended an
;; ordinary record, took the rebuild path never, and compared the index
;; against a different fixture's -- it failed for the second reason and
;; would have been green for neither.

(define rebuilt (reduce-empty))
(reduce-apply! rebuilt origin 1 '()
               (plan-payload (list (cons 0 (intent "r")))
                             (list 'consumes "dw" (list (item "dw.1" "v5"))))
               (plan-actor))
(reduce-apply! rebuilt origin 2 '() (produced "r") (member-actor 0 (cons origin 1)))
(define before-rebuild (state-consumption rebuilt))
(want "CI-08 the plan is consumed before the reversal"
      (state-consumed? rebuilt "dw" "v5") #t)

;; Two records claiming one single identity: the second reverses the
;; first, which is what forces the rebuild.
(define (claim! r w)
  (reduce-apply! r w 1 '() (produced "s")
                 (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0))))
(claim! rebuilt "aaa00000")
(claim! rebuilt "bbb00000")
(want "CI-08 the reversal really took the rebuild path"
      (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates rebuilt)))) (and p (cdr p))))
           '("aaa00000" "bbb00000"))
      '(plan-conflict plan-conflict))
(want "CI-08 and the index came back unchanged"
      (state-consumption rebuilt) before-rebuild)
(want "CI-08 with the same answer" (state-consumed? rebuilt "dw" "v5") #t)

;; ---- CI-09 a reversed plan gives its consumption back ---------------------
;;
;; The rebuild is not only a way of not losing the index; it is how a
;; consumption is TAKEN BACK. A second record claiming the plan's
;; identity puts both in conflict, neither is applied, and the drafts
;; that plan named have to become visible again -- an index that only
;; ever grows reports them as consumed by a commit that is no longer in
;; the state.

(define dup-key (cons "dup00000" "d"))
(define dup-after (cons "dup00000" 0))
(define (dup-plan-actor) (list "test" dup-key 'plan "fp" #f dup-after))
(define (dup-plan w)
  (reduce-apply! w "dup00000" 1 '()
                 (list 'plan "d" "fp" dup-after (list (cons 0 (intent "d")))
                       (list 'consumes "dw" (list (item "dw.1" "v3"))))
                 (dup-plan-actor)))

(define reversed (reduce-empty))
(dup-plan reversed)
(reduce-apply! reversed "dup00000" 2 '() (produced "d")
               (list "test" dup-key 0 "fp" (cons "dup00000" 1) dup-after))
(want "CI-09 the plan consumed its draft" (state-consumed? reversed "dw" "v3") #t)

;; A second plan record with the same identity, from another writer.
(reduce-apply! reversed "dup10000" 1 '()
               (list 'plan "d" "fp" dup-after (list (cons 0 (intent "d"))))
               (list "test" dup-key 'plan "fp" #f dup-after))
(want "CI-09 both plan records are in conflict"
      (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates reversed)))) (and p (cdr p))))
           '("dup00000" "dup10000"))
      '(plan-conflict plan-conflict))
(want "CI-09 and the consumption is taken back"
      (state-consumed? reversed "dw" "v3") #f)
(want "CI-09 the index holds nothing about it"
      (state-consumption reversed) '())

;; ---- CI-10 a plan's records are one writer's, in sequence ---------------
;;
;; NOTE: AN EARLIER VERSION OF THIS SECTION SAID A CROSS-WRITER MEMBER IS
;; REFUSED, AND MEASURED IT. The measurement was of the wrong thing: the
;; record it built named `splitaaa.1` as its dependency while its
;; predecessor in the plan was `splitaaa.2`, so what `plan-order`
;; refused was the MISSING PREDECESSOR, not the foreign writer. Link the
;; predecessor and the foreign member is admitted.
;;
;; Both facts get a row now, because they are different facts.

(define (two-member-plan!)
  (let ((r (reduce-empty)))
    (reduce-apply! r "splitaaa" 1 '()
                   (list 'plan "s" "fp" (cons "splitaaa" 0)
                         (list (cons 0 (intent "p")) (cons 1 (intent "q")))
                         (list 'consumes "dw" (list (item "dw.1" "vj"))))
                   (list "test" (cons "splitaaa" "s") 'plan "fp" #f (cons "splitaaa" 0)))
    (reduce-apply! r "splitaaa" 2 '() (produced "p")
                   (list "test" (cons "splitaaa" "s") 0 "fp" (cons "splitaaa" 1) (cons "splitaaa" 0)))
    r))

;; (a) a member whose predecessor is not in its past is refused.
(define gapped (two-member-plan!))
(reduce-apply! gapped "splitbbb" 1 '(("splitaaa" . 1)) (produced "q")
               (list "test" (cons "splitaaa" "s") 1 "fp" (cons "splitaaa" 1) (cons "splitaaa" 0)))
(want "CI-10 a member that does not follow its predecessor is refused"
      (let ((g (assoc (cons "splitbbb" 1) (reduce-gates gapped)))) (and g (cdr g)))
      'plan-order)
(want "CI-10 so that plan consumes nothing" (state-consumed? gapped "dw" "vj") #f)

;; (b) link the predecessor and the SAME foreign writer is admitted --
;; which is what makes the join a different answer from "the greatest
;; member's cut".
(define split (two-member-plan!))
(reduce-apply! split "splitbbb" 1 '(("splitaaa" . 2)) (produced "q")
               (list "test" (cons "splitaaa" "s") 1 "fp" (cons "splitaaa" 1) (cons "splitaaa" 0)))
(want "CI-10 TWIN: with its predecessor in its past it is admitted"
      (assoc (cons "splitbbb" 1) (reduce-gates split)) #f)
(want "CI-10 and the plan completes" (state-consumed? split "dw" "vj") #t)
(want "CI-10 its members really are from two writers"
      (list-sort (lambda (a b) (string<? (car a) (car b)))
                 (map cdr (list-ref (car (state-consumption split)) 4)))
      '(("splitaaa" . 2) ("splitbbb" . 1)))

;; NOTE: IN THIS PARTICULAR ARRANGEMENT the two rules agree, because the
;; greatest member by (writer, seq) is also the last one applied, so its
;; cut already covers the other. NEVER: THAT IS NOT A GENERAL FACT -- I
;; recorded it as one, twice, and it is false; see CI-12 below, where
;; the chain runs the other way and the join reaches a member the
;; completion event's own cut does not.
(define parent (car (state-consumed-parent-cuts split "dw" "vj")))
(define (reach cut writer) (let ((e (assoc writer cut))) (and e (cdr e))))
(want "CI-10 the parent cut reaches the first writer's member"
      (>= (or (reach parent "splitaaa") -1) 2) #t)
(want "CI-10 and the second writer's member"
      (>= (or (reach parent "splitbbb") -1) 1) #t)
(want "CI-10 the greatest member's own cut already covers the earlier one"
      (let ((own (state-event-cut split (cons "splitbbb" 1))))
        (list (>= (or (reach own "splitbbb") -1) 1)
              (>= (or (reach own "splitaaa") -1) 2)))
      '(#t #t))
;; NOTE: COMPARED AS SETS. A cut is an association list and the join
;; rebuilds it, so the two carry the same reaches in a different
;; spelling; comparing the lists as written would be asserting the order
;; of an alist.
(define (cut-set c) (list-sort (lambda (a b) (string<? (car a) (car b))) c))
(want "CI-10 TWIN: which is why the join equals it here"
      (map cut-set (state-consumed-parent-cuts split "dw" "vj"))
      (list (cut-set (state-event-cut split (cons "splitbbb" 1)))))

;; ---- CI-12 the greatest member, and why the join is not the same -------
;;
;; NOTE: TWO EARLIER EXPLANATIONS IN THIS FILE WERE WRONG, and the second
;; was wrong in a way that hid a real difference.
;;
;;   First: "a member from another writer is refused." It is not -- what
;;   was refused was a member whose predecessor was not in its past.
;;   Second: "members are causally chained, so the greatest member's cut
;;   covers every earlier one." That holds only when the greatest member
;;   is also the LAST one applied. It need not be: (writer, seq) ranks
;;   `zplan000.2` above `amemb000.1`, while the causal chain runs the
;;   other way -- z's member first, a's member after it.
;;
;; KEY: SO THIS IS THE CASE THAT SEPARATES EVERYTHING. The greatest member
;; is `zplan000.2`, whose own cut does NOT contain `amemb000.1`; the
;; join of both member cuts does. And "the greatest" is not "the last
;; applied" here either, so a build that recorded whichever member
;; happened to be applied last answers differently.

(define chain (reduce-empty))
(define chain-key (cons "zplan000" "c"))
(define chain-after (cons "zplan000" 0))
(reduce-apply! chain "zplan000" 1 '()
               (list 'plan "c" "fp" chain-after
                     (list (cons 0 (intent "c0")) (cons 1 (intent "c1")))
                     (list 'consumes "dw" (list (item "dw.1" "vc"))))
               (list "test" chain-key 'plan "fp" #f chain-after))
(reduce-apply! chain "zplan000" 2 '() (produced "c0")
               (list "test" chain-key 0 "fp" (cons "zplan000" 1) chain-after))
(reduce-apply! chain "amemb000" 1 '(("zplan000" . 2)) (produced "c1")
               (list "test" chain-key 1 "fp" (cons "zplan000" 1) chain-after))

(want "CI-12 the plan completed" (state-consumed? chain "dw" "vc") #t)
(want "CI-12 TWIN: its members are zplan000.2 and amemb000.1"
      (list-sort (lambda (a b) (string<? (car a) (car b)))
                 (map cdr (list-ref (car (state-consumption chain)) 4)))
      '(("amemb000" . 1) ("zplan000" . 2)))
(want "CI-12 TWIN: and the LAST APPLIED is amemb000.1"
      (state-consumed? chain "dw" "vc") #t)

;; KEY: COMPLETION IS THE GREATEST, NOT THE LAST APPLIED. A build that
;; recorded the last member to arrive answers `("amemb000" . 1)`.
(want "CI-12 completion is the greatest member by (writer, seq)"
      (list-ref (car (state-consumption chain)) 5) (cons "zplan000" 2))

;; KEY: AND THE PARENT CUT IS THE JOIN, WHICH THE GREATEST MEMBER'S CUT IS
;; NOT. This is the row that was missing: the join reaches amemb000.1
;; and the completion event's own cut does not.
(define chain-parent (car (state-consumed-parent-cuts chain "dw" "vc")))
(define (reached cut w) (let ((e (assoc w cut))) (and e (cdr e))))
(want "CI-12 the parent cut reaches both members"
      (list (>= (or (reached chain-parent "zplan000") -1) 2)
            (>= (or (reached chain-parent "amemb000") -1) 1))
      '(#t #t))
(want "CI-12 TWIN: while the completion event's own cut reaches only one"
      (let ((own (state-event-cut chain (cons "zplan000" 2))))
        (list (>= (or (reached own "zplan000") -1) 2)
              (>= (or (reached own "amemb000") -1) 1)))
      '(#t #f))

;; ---- CI-13 the snapshot really carries the index -------------------------
;;
;; NOTE: CI-04 AND CI-05 BETWEEN THEM DID NOT REQUIRE THE ROW TO EXIST.
;; Removing it from `state->rows` left both green, because the absent-row
;; path rebuilds from `request-history` and answers the same. The row has
;; to be asked for by name.

(want "CI-13 a snapshot carries a consumed row"
      (and (assq 'consumed (state->rows one)) #t) #t)
(want "CI-13 and what it carries is the index"
      (cadr (assq 'consumed (state->rows one))) (state-consumption one))

;; ---- CI-14 what a plan NAMED survives, and is not consumption ------------
;;
;; `seen` is read by `drafts` and `restore`; nothing above asked for it,
;; so replacing its writer with a no-op left every row green.

(want "CI-14 the plan's name for the draft is remembered"
      (map car (state-seen one "dw" "v1")) (list (cons origin 1)))
(want "CI-14 TWIN: and a version nobody named is not"
      (state-seen one "dw" "nosuch") '())
(want "CI-14 it survives a snapshot"
      (map car (state-seen (rows->state (state->rows one)) "dw" "v1"))
      (list (cons origin 1)))

;; ---- CI-11 a malformed consumes does not enter the index -----------------
;;
;; `payload-reason` notes a plan whose `consumes` does not cover its own
;; sub-operations. Registration used to happen first, so the version it
;; named came back as consumed by a record the reduction had refused.

(define wrong (reduce-empty))
;; A COMMIT'S SUB-OPERATION IS `(set <block> src <text>)`, and the
;; correspondence rule is about that shape: an `insert` names no block
;; of its own, so there is nothing for a consumes item to correspond to.
(reduce-apply! wrong origin 1 '()
               (plan-payload (list (cons 0 (list 'set "plan0000.1" 'src "W")))
                             (list 'consumes "dw" (list (item "other.9" "vbad"))))
               (plan-actor))
(reduce-apply! wrong origin 2 '() (list 'set "plan0000.1" 'src "W")
               (member-actor 0 (cons origin 1)))
(want "CI-11 the plan is noted as malformed"
      (exists (lambda (n) (and (pair? n) (eq? 'malformed-record (car n))
                               (equal? '(reason consumes-does-not-cover-sub-operation)
                                       (caddr n))))
              (reduce-noted wrong))
      #t)
(want "CI-11 and the version it named is not consumed"
      (state-consumed? wrong "dw" "vbad") #f)
(want "CI-11 the index holds nothing about it" (state-consumption wrong) '())

;; ---- CI-15 a late duplicate MEMBER, and what drafts must still show ------
;;
;; NOTE: MEASURED GAP: the rows above all revoke by putting the PLAN in
;; conflict. A second record claiming a MEMBER's slot leaves the plan
;; applied and gates the members -- and the lookup that resolves a gated
;; event to its plan found nothing, so a draft the commit had already
;; retired was reported neither as a draft nor as revoked, and `restore`
;; answered `unknown-version` for work that existed only in the log.

(define dup (reduce-empty))
(define dup-key (cons "dupm0000" "d"))
(define dup-after (cons "dupm0000" 0))
(reduce-apply! dup "dupm0000" 1 '()
               (list 'plan "d" "fp" dup-after
                     (list (cons 0 (list 'set "dupm0000.9" 'src "D")))
                     (list 'consumes "dw" (list (item "dupm0000.9" "vd"))))
               (list "test" dup-key 'plan "fp" #f dup-after))
(reduce-apply! dup "dupm0000" 2 '() (list 'set "dupm0000.9" 'src "D")
               (list "test" dup-key 0 "fp" (cons "dupm0000" 1) dup-after))
(want "CI-15 the plan consumed its draft" (state-consumed? dup "dw" "vd") #t)

;; A second record for the SAME member slot.
(reduce-apply! dup "dupn0000" 1 '() (list 'set "dupm0000.9" 'src "D")
               (list "test" dup-key 0 "fp" (cons "dupm0000" 1) dup-after))
(want "CI-15 both member records are in conflict"
      (length (filter (lambda (g) (eq? 'plan-conflict (cdr g))) (reduce-gates dup))) 2)
(want "CI-15 TWIN: and the plan record itself is not gated"
      (assoc (cons "dupm0000" 1) (reduce-gates dup)) #f)
(want "CI-15 the consumption is withdrawn" (state-consumed? dup "dw" "vd") #f)
;; KEY: AND THE DRAFT IS DISCOVERABLE. Its file is gone -- the commit
;; retired it -- so `drafts` has to be able to say `revoked`, and
;; `restore` has to be able to find it.
(want "CI-15 and the version is reported as revoked"
      (map (lambda (r) (cadr (car r))) (state-revoked dup "dw")) '("vd"))

;; ---- CI-16 what a PENDING plan is not -------------------------------------
;;
;; NOTE: Replacing the gate filter with #t left all 55 rows green: nothing
;; asked `state-revoked` about a plan that is merely waiting.

;; A FRESH ONE: the `waiting` reduction above has since had its
;; dependency delivered, and a plan that has completed is not the
;; question here.
(define still-waiting (reduce-empty))
(reduce-apply! still-waiting origin 1 '(("base0000" . 1))
               (plan-payload (list (cons 0 (intent "pend")))
                             (list 'consumes "dw" (list (item "dw.1" "vp"))))
               (plan-actor))
(reduce-apply! still-waiting origin 2 '() (produced "pend")
               (member-actor 0 (cons origin 1)))
(want "CI-16 a plan waiting on a dependency consumes nothing"
      (state-consumed? still-waiting "dw" "vp") #f)
(want "CI-16 TWIN: and it is not revoked either -- nothing was taken back"
      (state-revoked still-waiting "dw") '())
(want "CI-16 TWIN: the plan really is only WAITING, not in conflict"
      (let ((g (assoc (cons origin 1) (reduce-gates still-waiting)))) (and g (cdr g)))
      'pending-plan)

;; ---- CI-17 `seen` survives a rebuild, not only a snapshot -----------------
;;
;; NOTE: Removing `fill-seen-from-history!` from `rebuild-request-state!`
;; left every row green: CI-14 asks about a snapshot, and nothing asked
;; after a membership reversal, which is the other path that clears it.

(want "CI-17 after a reversal the plan's name for the draft is still known"
      (map car (state-seen reversed "dw" "v3")) (list (cons "dup00000" 1)))

(printf "rows: ~a\n~a failures\nconsumption-index complete\n" rows bad)
