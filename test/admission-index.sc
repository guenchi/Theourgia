#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;; ADMISSION'S JUDGEMENT, INDEXED, AGAINST THE ONE IT REPLACED (A1).
;;
;; request.sc judges a request's records with its lookups indexed -- the
;; statuses and the bindings by event, a slot by index, a predecessor by
;; (plan, index), a plan's validity and its entries kept per judgement -- and
;; admission.sc drops a bucket's old gates by a table of its events. Under
;; the degeneracy switch (THEOURGIA_DEGENERACY=1 at expansion, degeneracy.sc)
;; the whole-bucket judgement as it was runs beside the new one on every
;; bucket of every admission-add!, and the replacement as it was beside the
;; new one; a difference in the statuses, the bindings or the gate list
;; raises at that record. The scenarios drive what admission must judge: an
;; import-shaped plan applied in order, a plan whose items name blocks made
;; by earlier items ("#%new"), members that arrive before their plan and are
;; woken, two records at one index, a member out of order, a member whose
;; record is not what its plan declared, an integer member with no plan, two
;; plan records for one request, two plans at one index, two requests
;; interleaved, a rehearsal's clone, a snapshot's rows restored, a contested
;; single slot, a plan whose declaration is not its actor's, a member with no
;; predecessor, an item whose "#%new" names an index not yet applied, and an
;; index the plan does not declare; and the edges a judgement of one
;; arrival must follow rather than re-judging the bucket (A2): a binding
;; withdrawn at k after a later index bound through it, a binding established
;; at k after its consumer was delivered (through an index and through
;; single), a late plan, index 0 arriving after 1 and 2, one record waking two
;; requests, an undelivered index 0 after a delivered index 1, and a later
;; index's binding that the pass does not yet see, and a single record that
;; names a plan arriving before it, and a plan record naming the plan event
;; of a plan that arrives after it, and a plan-sub record whose payload is not
;; a plan.
;;
;; It runs in a child under each value of the switch: on, nothing may differ
;; (AI-1); off, the switch is off and the readings are the same (AI-2), and
;; every named check holds (AI-3).

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)))) e))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/admission-index-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))

;; THE SCENARIOS, written as the child's program. It answers
;;   (admission-index <switch on?> ok <readings> <checks>)
;; or, when anything raised, (admission-index <switch on?> raised <message> <irritants>).
;; A reading is (<scenario> <gates> <blocks in the datum>).
(define scenario
  '((import (chezscheme) (theourgia reduce) (theourgia degeneracy))
    (define checks '())
    (define (check! name ok?) (set! checks (cons (cons name (and ok? #t)) checks)))
    (define readings '())
    (define (read! name r)
      (set! readings (cons (list name (reduce-gates r) (length (state-datum r))) readings)))
    (define (gate r w n) (let ((p (assoc (cons w n) (reduce-gates r)))) (and p (cdr p))))
    (define (intent title) (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title title))))
    (define (produced title)
      (list 'put (list (cons 'kind 'section) (cons 'title title) '(parent . root) '(ord . 1))))
    (define (plan-of origin req entries)
      (list 'plan req "fp" (cons origin 0) entries))
    (define (plan-actor origin req) (list "test" (cons origin req) 'plan "fp" #f (cons origin 0)))
    (define (member-actor origin req i plan-event) (list "test" (cons origin req) i "fp" plan-event (cons origin 0)))
    ;; A plan of N inserts under root, recorded as writer ORIGIN's 1.
    (define (plan! r origin req n)
      (reduce-apply! r origin 1 '()
                     (plan-of origin req (map (lambda (i) (cons i (intent (format "t~a" i)))) (iota n)))
                     (plan-actor origin req)))
    ;; A put of TITLE under PARENT, and an insert declared under ("#%new" K).
    (define (under title parent)
      (list 'put (list (cons 'kind 'section) (cons 'title title) (cons 'parent parent) '(ord . 1))))
    (define (new-intent title k)
      (list 'insert (list "#%new" k) #f (list (cons 'kind 'section) (cons 'title title))))
    ;; Member I of that plan, recorded as writer W's SEQ with DEPS.
    (define (member! r w seq deps origin req i . title)
      (reduce-apply! r w seq deps (produced (if (pair? title) (car title) (format "t~a" i)))
                     (member-actor origin req i (cons origin 1))))
    (define result
      (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                          (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
        ;; (1) AN IMPORT'S SHAPE: one plan, its members in order, one writer.
        (let ((r (reduce-empty)))
          (plan! r "imp00000" "i" 120)
          (for-each (lambda (i) (member! r "imp00000" (+ i 2) '() "imp00000" "i" i)) (iota 120))
          (read! 'import r)
          (check! 'import-admits-every-member (and (null? (reduce-gates r)) (= 120 (length (state-datum r))))))
        ;; (2) ITEMS THAT NAME EARLIER ITEMS' BLOCKS: item k inserts under the
        ;; block item k-1 made, ("#%new" k-1), bound from k-1's record.
        (let ((r (reduce-empty)) (o "new00000"))
          (reduce-apply! r o 1 '()
                         (plan-of o "n" (cons (cons 0 (intent "n0"))
                                             (map (lambda (k) (cons k (list 'insert (list "#%new" (- k 1)) #f
                                                                            (list (cons 'kind 'section) (cons 'title (format "n~a" k))))))
                                                  '(1 2 3 4 5))))
                         (plan-actor o "n"))
          (reduce-apply! r o 2 '() (produced "n0") (member-actor o "n" 0 (cons o 1)))
          (for-each
            (lambda (k)
              (reduce-apply! r o (+ k 2) '()
                             (list 'put (list (cons 'kind 'section) (cons 'title (format "n~a" k))
                                              (cons 'parent (format "~a.~a" o (+ k 1))) '(ord . 1)))
                             (member-actor o "n" k (cons o 1))))
            '(1 2 3 4 5))
          (read! 'new-chain r)
          (check! 'new-chain-binds-each-item (and (null? (reduce-gates r)) (= 6 (length (state-datum r)))))
          ;; and one more whose parent is not the block its item names
          (let ((w (reduce-clone r)))
            (reduce-apply! w "new10000" 1 (list (cons o 7))
                           (list 'put (list (cons 'kind 'section) (cons 'title "n5") (cons 'parent (format "~a.2" o)) '(ord . 1)))
                           (member-actor o "n" 5 (cons o 1)))
            (read! 'new-chain-wrong-parent w)
            (check! 'new-chain-a-wrong-parent-is-a-mismatch (eq? (gate w "new10000" 1) 'plan-mismatch))))
        ;; (3) MEMBERS BEFORE THEIR PLAN: they wait, the plan wakes them.
        (let ((r (reduce-empty)) (o "late0000"))
          (for-each (lambda (i) (member! r o (+ i 2) '() o "l" i)) (reverse (iota 30)))
          (read! 'late-before r)
          (check! 'late-members-wait-applied-nothing (= 0 (length (state-datum r))))
          (plan! r o "l" 30)
          (read! 'late-after r)
          (check! 'late-members-woken (and (null? (reduce-gates r)) (= 30 (length (state-datum r))))))
        ;; (4) TWO RECORDS AT ONE INDEX, both following the plan.
        (let ((r (reduce-empty)) (o "two00000"))
          (plan! r o "t" 2)
          (member! r o 2 '() o "t" 0)
          (member! r "two10000" 1 (list (cons o 1)) o "t" 0)
          (member! r o 3 '() o "t" 1)
          (read! 'contested r)
          (check! 'contested-both-plan-conflict
                  (equal? (list (gate r o 2) (gate r "two10000" 1)) '(plan-conflict plan-conflict)))
          (check! 'contested-the-next-index-waits (eq? (gate r o 3) 'pending-plan)))
        ;; (5) A MEMBER THAT DOES NOT FOLLOW ITS PREDECESSOR.
        (let ((r (reduce-empty)) (o "gap00000"))
          (plan! r o "g" 2)
          (member! r o 2 '() o "g" 0)
          (member! r "gap10000" 1 (list (cons o 1)) o "g" 1)
          (read! 'gapped r)
          (check! 'gapped-plan-order (eq? (gate r "gap10000" 1) 'plan-order)))
        ;; (6) RECORDS THAT ARE NOT WHAT THE PLAN DECLARED, and an integer
        ;; member with no plan at all.
        (let ((r (reduce-empty)) (o "mis00000"))
          (plan! r o "m" 3)
          (member! r o 2 '() o "m" 0)
          (member! r o 3 '() o "m" 1 "not-t1")
          (reduce-apply! r "mis10000" 1 '() (produced "x") (list "test" (cons "mis10000" "q") 0 "fp" #f (cons "mis10000" 0)))
          (read! 'mismatch r)
          (check! 'mismatch-the-wrong-title (eq? (gate r o 3) 'plan-mismatch))
          (check! 'mismatch-an-integer-member-without-a-plan (eq? (gate r "mis10000" 1) 'plan-mismatch))
          (check! 'mismatch-the-first-member-stands (not (gate r o 2))))
        ;; (7) TWO PLAN RECORDS FOR ONE REQUEST.
        (let ((r (reduce-empty)) (o "dup00000"))
          (plan! r o "d" 1)
          (member! r o 2 '() o "d" 0)
          (reduce-apply! r "dup10000" 1 '() (plan-of o "d" (list (cons 0 (intent "t0")))) (plan-actor o "d"))
          (read! 'two-plans r)
          (check! 'two-plans-both-conflict
                  (equal? (list (gate r o 1) (gate r "dup10000" 1)) '(plan-conflict plan-conflict))))
        ;; (8) TWO PLAN EVENTS AT ONE INDEX: a member naming another plan
        ;; record of the same request, which is not a plan.
        (let ((r (reduce-empty)) (o "pe000000"))
          (plan! r o "p" 2)
          (member! r o 2 '() o "p" 0)
          (reduce-apply! r o 3 '() (produced "t0") (member-actor o "p" 0 (cons o 2)))
          (read! 'two-plan-events r)
          (check! 'two-plan-events-the-first-stands (not (gate r o 2)))
          (check! 'two-plan-events-the-other-waits-for-a-plan-it-cannot-have (eq? (gate r o 3) 'pending-plan)))
        ;; (9) TWO REQUESTS INTERLEAVED: one bucket's gates stay while the
        ;; other's change.
        (let ((r (reduce-empty)) (a "ia000000") (b "ib000000"))
          (plan! r a "a" 3)
          (plan! r b "b" 3)
          (member! r a 2 '() a "a" 0)
          (member! r "ia100000" 1 (list (cons a 1)) a "a" 0)
          (member! r b 2 '() b "b" 0)
          (member! r b 3 '() b "b" 1)
          (member! r a 3 '() a "a" 1)
          (member! r b 4 '() b "b" 2)
          (read! 'interleaved r)
          (check! 'interleaved-a-keeps-its-conflict
                  (equal? (list (gate r a 2) (gate r "ia100000" 1)) '(plan-conflict plan-conflict)))
          (check! 'interleaved-b-admitted (and (not (gate r b 2)) (not (gate r b 3)) (not (gate r b 4))))
          ;; (10) A REHEARSAL'S CLONE goes on alone; the original is unchanged.
          (let ((before (reduce-gates r)) (c (reduce-clone r)))
            (member! c a 4 '() a "a" 2)
            (reduce-apply! c "ib100000" 1 (list (cons b 1)) (produced "t2") (member-actor b "b" 2 (cons b 1)))
            (read! 'clone c)
            (read! 'clone-original r)
            (check! 'clone-original-unchanged (equal? (reduce-gates r) before))
            (check! 'clone-its-own-conflict (eq? (gate c "ib100000" 1) 'plan-conflict))))
        ;; (11) A SNAPSHOT'S ROWS RESTORED answer alike and go on alike. The
        ;; rows carry only the request history the applied cut covers
        ;; (state->rows), so the state snapshotted has every record of its
        ;; request applied; the restored admission is folded again from it,
        ;; and the same two records -- the next member and a second record at
        ;; that index -- go to both. Gates are compared as a set: the order
        ;; they were found in is the fold's.
        (let ((r (reduce-empty)) (o "rs000000")
              (as-set (lambda (gates) (list-sort (lambda (x y) (string<? (format "~s" x) (format "~s" y))) gates))))
          (plan! r o "s" 3)
          (member! r o 2 '() o "s" 0)
          (member! r o 3 '() o "s" 1)
          (let ((s (rows->state (state->rows r))))
            (check! 'restored-answers-as-the-original
                    (and (null? (reduce-gates s)) (equal? (state-hash s) (state-hash r))))
            (for-each (lambda (x)
                        (member! x o 4 '() o "s" 2)
                        (member! x "rs100000" 1 (list (cons o 3)) o "s" 2))
                      (list r s))
            (read! 'restored s)
            (check! 'restored-goes-on-as-the-original
                    (and (equal? (as-set (reduce-gates s)) (as-set (reduce-gates r)))
                         (equal? (list (gate s o 4) (gate s "rs100000" 1)) '(plan-conflict plan-conflict))
                         (equal? (state-hash s) (state-hash r))))))
;; (12) TWO SINGLE RECORDS OF ONE REQUEST: the single slot contested.
        (let ((r (reduce-empty)) (single (lambda (w) (list "test" (cons "sg000000" "s") 'single "fp" #f (cons "sg000000" 0)))))
          (reduce-apply! r "sg100000" 1 '() (produced "s") (single "sg100000"))
          (reduce-apply! r "sg200000" 1 '() (produced "s") (single "sg200000"))
          (read! 'single-contested r)
          (check! 'single-contested-both-plan-conflict
                  (equal? (list (gate r "sg100000" 1) (gate r "sg200000" 1)) '(plan-conflict plan-conflict))))
        ;; (13) A PLAN RECORD WHOSE DECLARATION IS NOT ITS ACTOR'S: the
        ;; fingerprint differs, so declaration-ok? refuses it, and its member
        ;; has no plan to be judged against.
        (let ((r (reduce-empty)) (o "dc000000"))
          (reduce-apply! r o 1 '() (list 'plan "d" "other-fp" (cons o 0) (list (cons 0 (intent "t0")))) (plan-actor o "d"))
          (member! r o 2 '() o "d" 0)
          (read! 'declaration r)
          (check! 'declaration-mismatch-the-plan (eq? (gate r o 1) 'plan-mismatch))
          (check! 'declaration-mismatch-the-member-waits (eq? (gate r o 2) 'pending-plan)))
        ;; (14) A MEMBER WITH NO PREDECESSOR AT ALL: index 1 arrives, index 0
        ;; never does.
        (let ((r (reduce-empty)) (o "np000000"))
          (plan! r o "n" 2)
          (member! r o 2 '() o "n" 1)
          (read! 'no-predecessor r)
          (check! 'no-predecessor-plan-order (eq? (gate r o 2) 'plan-order)))
        ;; (15) AN ITEM WHOSE "#%new" NAMES AN INDEX NOT YET APPLIED, and
        ;; (16) AN INDEX THE PLAN DOES NOT DECLARE.
        (let ((r (reduce-empty)) (o "ud000000"))
          (reduce-apply! r o 1 '()
                         (plan-of o "u" (list (cons 0 (intent "u0"))
                                              (cons 1 (list 'insert (list "#%new" 0) #f (list (cons 'kind 'section) (cons 'title "u1"))))))
                         (plan-actor o "u"))
          (reduce-apply! r "ud100000" 1 (list (cons o 1))
                         (list 'put (list (cons 'kind 'section) (cons 'title "u1") (cons 'parent (format "~a.2" o)) '(ord . 1)))
                         (member-actor o "u" 1 (cons o 1)))
          (member! r "ud200000" 1 (list (cons o 1)) o "u" 5 "u5")
          (read! 'undeclared-and-unbound r)
          (check! 'unbound-new-waits (eq? (gate r "ud100000" 1) 'pending-plan))
          (check! 'undeclared-index-mismatch (eq? (gate r "ud200000" 1) 'plan-mismatch)))
        ;; (17) A BINDING WITHDRAWN: index 1 is bound through ("#%new" 0); a
        ;; second valid record at 0 arrives, both conflict, the binding at 0
        ;; goes, and index 1 can no longer be bound.
        (let ((r (reduce-empty)) (o "wd000000"))
          (reduce-apply! r o 1 '() (plan-of o "w" (list (cons 0 (intent "w0")) (cons 1 (new-intent "w1" 0))))
                         (plan-actor o "w"))
          (member! r o 2 '() o "w" 0 "w0")
          (reduce-apply! r o 3 '() (under "w1" (format "~a.2" o)) (member-actor o "w" 1 (cons o 1)))
          (check! 'withdrawn-bound-before (not (gate r o 3)))
          (member! r "wd100000" 1 (list (cons o 1)) o "w" 0 "w0")
          (read! 'withdrawn r)
          (check! 'withdrawn-after
                  (equal? (list (gate r o 2) (gate r "wd100000" 1) (gate r o 3)) '(plan-conflict plan-conflict pending-plan))))
        ;; (18) A BINDING ESTABLISHED AFTER ITS CONSUMER: index 1, declared
        ;; under ("#%new" 0), is delivered (it depends on the plan only) before
        ;; index 0 arrives; nothing wakes it, and its classification changes.
        (let ((r (reduce-empty)) (o "es000000"))
          (reduce-apply! r o 1 '() (plan-of o "e" (list (cons 0 (intent "e0")) (cons 1 (new-intent "e1" 0))))
                         (plan-actor o "e"))
          (reduce-apply! r "es100000" 1 (list (cons o 1)) (under "e1" (format "~a.2" o)) (member-actor o "e" 1 (cons o 1)))
          (check! 'established-before (eq? (gate r "es100000" 1) 'pending-plan))
          (member! r o 2 '() o "e" 0 "e0")
          (read! 'established r)
          (check! 'established-after (equal? (list (gate r o 2) (gate r "es100000" 1)) '(#f plan-order))))
        ;; (19) THE SAME THROUGH ("#%new" single): index 0 declared under the
        ;; block the request's single record makes, which arrives later from
        ;; another writer.
        (let ((r (reduce-empty)) (o "sn000000"))
          (reduce-apply! r o 1 '() (plan-of o "g" (list (cons 0 (new-intent "g0" 'single)))) (plan-actor o "g"))
          (reduce-apply! r o 2 '() (under "g0" "sn100000.1") (member-actor o "g" 0 (cons o 1)))
          (check! 'single-before (eq? (gate r o 2) 'pending-plan))
          (reduce-apply! r "sn100000" 1 '() (produced "s") (list "test" (cons o "g") 'single "fp" #f (cons o 0)))
          (read! 'single-new r)
          (check! 'single-after (equal? (list (gate r o 2) (gate r "sn100000" 1)) '(#f #f))))
        ;; (20) A LATE PLAN: two members delivered (another writer, no causal
        ;; link to the plan) before their plan arrives.
        (let ((r (reduce-empty)) (o "lp000000"))
          (member! r "lp100000" 1 '() o "p" 0)
          (member! r "lp100000" 2 '() o "p" 1)
          (check! 'late-plan-before
                  (equal? (list (gate r "lp100000" 1) (gate r "lp100000" 2)) '(pending-plan pending-plan)))
          (plan! r o "p" 2)
          (read! 'late-plan r)
          (check! 'late-plan-after
                  (equal? (list (gate r "lp100000" 1) (gate r "lp100000" 2)) '(plan-order pending-plan))))
        ;; (21) INDEX 0 AFTER 1 AND 2: the members at 1 and 2 are judged with no
        ;; record at 0; one arrives (refused) and the order chain moves.
        (let ((r (reduce-empty)) (o "zx000000"))
          (plan! r o "z" 3)
          (member! r o 2 '() o "z" 1)
          (member! r o 3 '() o "z" 2)
          (check! 'index0-last-before (equal? (list (gate r o 2) (gate r o 3)) '(plan-order pending-plan)))
          (member! r "zx100000" 1 (list (cons o 1)) o "z" 0 "wrong")
          (read! 'index0-last r)
          (check! 'index0-last-after
                  (equal? (list (gate r "zx100000" 1) (gate r o 2) (gate r o 3)) '(plan-mismatch pending-plan pending-plan))))
        ;; (22) ONE RECORD WAKES TWO BUCKETS: in each request a member at 0 waits
        ;; on a plain record beside a rival already there; the record arrives,
        ;; both members wake and conflict with their rivals.
        (let ((r (reduce-empty)) (a "xa000000") (b "xb000000"))
          (plan! r a "a" 1)
          (plan! r b "b" 1)
          (member! r "xa100000" 1 (list (cons a 1)) a "a" 0)
          (member! r "xb100000" 1 (list (cons b 1)) b "b" 0)
          (member! r a 2 (list (cons "xx000000" 1)) a "a" 0)
          (member! r b 2 (list (cons "xx000000" 1)) b "b" 0)
          (check! 'two-buckets-before
                  (equal? (list (gate r a 2) (gate r "xa100000" 1) (gate r b 2) (gate r "xb100000" 1))
                          '(pending-plan #f pending-plan #f)))
          (reduce-apply! r "xx000000" 1 '() (produced "x"))
          (read! 'two-buckets r)
          (check! 'two-buckets-after
                  (equal? (list (gate r a 2) (gate r "xa100000" 1) (gate r b 2) (gate r "xb100000" 1))
                          '(plan-conflict plan-conflict plan-conflict plan-conflict))))
        ;; (23) AN UNDELIVERED INDEX 0 AFTER A DELIVERED INDEX 1: the plan's
        ;; inserts are independent; index 1 has no predecessor (plan-order)
        ;; until a record at 0 arrives, undelivered, and is its predecessor.
        (let ((r (reduce-empty)) (o "ux000000"))
          (plan! r o "x" 2)
          (member! r "ux100000" 1 (list (cons o 1)) o "x" 1)
          (check! 'undelivered-index0-before (eq? (gate r "ux100000" 1) 'plan-order))
          (member! r "ux200000" 2 (list (cons o 1)) o "x" 0)
          (read! 'undelivered-index0 r)
          (check! 'undelivered-index0-after
                  (equal? (list (gate r "ux200000" 2) (gate r "ux100000" 1)) '(pending-plan pending-plan))))
        ;; (24) A LATER INDEX'S BINDING IS NOT VISIBLE: index 1 declared under
        ;; ("#%new" 2), and index 2 is already there and valid; the pass binds 2
        ;; after it classifies 1, so 1 stays unbound.
        (let ((r (reduce-empty)) (o "vf000000"))
          (reduce-apply! r o 1 '()
                         (plan-of o "v" (list (cons 0 (intent "v0")) (cons 1 (new-intent "v1" 2)) (cons 2 (intent "v2"))))
                         (plan-actor o "v"))
          (member! r o 2 '() o "v" 0 "v0")
          (member! r "vf100000" 1 (list (cons o 1)) o "v" 2 "v2")
          (reduce-apply! r "vf200000" 1 (list (cons o 1)) (under "v1" "vf100000.1") (member-actor o "v" 1 (cons o 1)))
          (read! 'visibility r)
          (check! 'visibility-later-index-unbound (eq? (gate r "vf200000" 1) 'pending-plan)))
        ;; (25) A SINGLE RECORD THAT NAMES A PLAN: delivered before the plan, it
        ;; waits for it; the plan declares no single, so once it arrives the
        ;; record is not what its plan declared.
        (let ((r (reduce-empty)) (o "sp000000"))
          (reduce-apply! r "sp100000" 1 '() (produced "s") (list "test" (cons o "q") 'single "fp" (cons o 1) (cons o 0)))
          (check! 'single-naming-a-plan-before (eq? (gate r "sp100000" 1) 'pending-plan))
          (plan! r o "q" 1)
          (read! 'single-naming-a-plan r)
          (check! 'single-naming-a-plan-after (eq? (gate r "sp100000" 1) 'plan-mismatch)))
        ;; (26) A PLAN RECORD THAT NAMES A PLAN EVENT, delivered before the plan
        ;; it names: it waits for that plan's binding, which the plan slot never
        ;; sees; the plan arriving binds and is valid, and the waiting record is
        ;; judged once.
        (let ((r (reduce-empty)) (o "pn000000"))
          (reduce-apply! r "pn100000" 1 '() (plan-of o "q" (list (cons 0 (intent "t0"))))
                         (list "test" (cons o "q") 'plan "fp" (cons o 1) (cons o 0)))
          (check! 'plan-naming-a-plan-before (eq? (gate r "pn100000" 1) 'pending-plan))
          (plan! r o "q" 1)
          (read! 'plan-naming-a-plan r)
          (check! 'plan-naming-a-plan-after
                  (equal? (list (gate r "pn100000" 1) (gate r o 1)) '(pending-plan #f))))
        ;; (27) A PLAN-SUB RECORD WHOSE PAYLOAD IS NOT A PLAN reaches admission
        ;; (reduce-apply! does not refuse it first): admitted without a raise,
        ;; and judged as the whole judgement judges it (the switched child).
        (let ((r (reduce-empty)) (o "mp000000"))
          (reduce-apply! r o 1 '() '(plan "q" "fp") (plan-actor o "q"))
          (read! 'malformed-plan r)
          (check! 'malformed-plan-admitted #t))
        (list 'ok)))
    (write (list 'admission-index (degeneracy-active) (car result)
                 (if (eq? (car result) 'ok) (reverse readings) (cdr result))
                 (reverse checks)))))

(define (run-scenario tag on?)
  (let ((script (string-append root "/scenario-" tag ".ss"))
        (out (string-append root "/scenario-" tag ".out")))
    (call-with-output-file script (lambda (p) (for-each (lambda (f) (write f p) (newline p)) scenario)) 'replace)
    (system (string-append (if on? "THEOURGIA_DEGENERACY=1 " "env -u THEOURGIA_DEGENERACY ")
                           "THEOURGIA_HOME='" root "/home' scheme --script '" script "' > '" out "' 2>&1 < /dev/null"))
    ;; The answer is the child's (admission-index ...) datum, wherever it is
    ;; among what the child printed: a compiler warning ahead of it is not
    ;; the answer, and no such datum is UNREADABLE with the text.
    (let ((text (file-text out)))
      (or (guard (e (#t #f))
            (let ((p (open-input-string text)))
              (let loop ()
                (let ((d (read p)))
                  (cond ((eof-object? d) #f)
                        ((and (pair? d) (eq? (car d) 'admission-index)) d)
                        (else (loop)))))))
          (list 'UNREADABLE text)))))

(define on (run-scenario "on" #t))
(define off (run-scenario "off" #f))
(want "AI-1 under the degeneracy switch the indexed judgement equals the whole-bucket one on every bucket of every record, and the gate list the old replacement's: no difference raised"
      (and (pair? on) (list (cadr on) (caddr on)))
      '(#t ok))
(want "AI-2 the switch unset: the same scenarios answer the same gates and blocks, and the switch reads off"
      (list (and (pair? off) (cadr off)) (and (pair? off) (caddr off))
            (and (pair? on) (pair? off) (equal? (cdddr on) (cdddr off))))
      '(#f ok #t))
(want "AI-3 every check of the scenarios holds: the import, the #%new chain and its wrong parent, the late members, the contested index, the gap, the mismatches, the two plans, the two plan events, the interleaved requests, the clone, the restored rows, the contested single slot, the plan whose declaration is not its actor's, the member with no predecessor, the unbound #%new, the undeclared index, the withdrawn and the late-established bindings, the single's binding, the late plan, index 0 last, two requests woken at once, the undelivered index 0, the later index's binding, the single record naming a plan, the plan record naming a later plan and the payload that is not a plan"
      (and (pair? off) (eq? (caddr off) 'ok)
           (list (length (car (cddddr off)))
                 (filter (lambda (c) (not (cdr c))) (car (cddddr off)))))
      '(46 ()))

;; THE REFERENCE IS NOT IN THE PRODUCT. identity-gates/full, the whole-bucket
;; judgement AI-1 compares with, is built only under the switch: a child
;; without it reads #f where the procedure would be, a child with it reads a
;; procedure. AI-2 shows the outputs agree; this shows the code is absent.
(define (reference-kind on?)
  (let ((script (string-append root "/reference-" (if on? "on" "off") ".ss"))
        (out (string-append root "/reference-" (if on? "on" "off") ".out")))
    (call-with-output-file script
      (lambda (p)
        (write '(import (chezscheme) (only (theourgia request) identity-gates/full)) p) (newline p)
        (write '(write (list 'reference-kind (cond ((procedure? identity-gates/full) 'procedure)
                                                   ((not identity-gates/full) 'absent)
                                                   (else 'other))))
               p)
        (newline p))
      'replace)
    (system (string-append (if on? "THEOURGIA_DEGENERACY=1 " "env -u THEOURGIA_DEGENERACY ")
                           "scheme --script '" script "' > '" out "' 2>&1 < /dev/null"))
    (or (guard (e (#t #f))
          (let ((p (open-input-string (file-text out))))
            (let loop ()
              (let ((d (read p)))
                (cond ((eof-object? d) #f)
                      ((and (pair? d) (eq? (car d) 'reference-kind)) (cadr d))
                      (else (loop)))))))
        (list 'UNREADABLE (file-text out)))))
(want "AI-4 the whole-bucket reference is absent from the product (#f without the switch) and a procedure under it"
      (list (reference-kind #f) (reference-kind #t))
      '(absent procedure))

(system (string-append "rm -rf '" root "'"))
(printf "~%~a failures~%rows: ~a~%admission-index complete~%" bad rows)
(exit (if (= bad 0) 0 1))
