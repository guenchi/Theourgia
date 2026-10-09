#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; THE ORDER A REDUCTION'S HISTORY IS IN, AND WHO MAY READ ONE FROM IT. A
;; record enters the history when it is DELIVERED, before it is applied (a
;; record waiting on another writer's is delivered first), and delivery
;; order is the route's: a fresh replay delivers writer by writer; a state
;; seeded from a snapshot, or kept by the daemon, holds its history and
;; delivers what came after it. So the same records can stand in two
;; orders, and an answer that prints that order differs by route.
;;
;; THE CONSTRUCTION: one store, a snapshot taken part-way, more records
;; after it; and a copy of the store without its snapshot. The store opens
;; seeded from the snapshot, the copy by a full replay: the same records,
;; delivered in two orders. Each row says which order its reader needs and
;; holds it: a reader that needs none answers alike on both.
;;
;; THE CENSUS (every walk of the history, and every reader of an order from
;; it):
;;   DO-1 check's reserved-relations clause: printed, so it needs ONE order
;;        on every route -- sorted by event, writer then seq.
;;   DO-2 commitments (state-put-events): a table by block id, and a
;;        listing ordered by origin and then by the put's (writer, seq) --
;;        needs no delivery order.
;;   DO-3 the consumption index (fill-seen-from-history!,
;;        rebuild-consumption-from-history!): keyed tables; state-consumed?
;;        needs no order; state-consumed-parent-cuts gives its cuts in
;;        delivery order, compared here as a set -- its order is read by
;;        DO-4. state-seen keeps its candidates newest-delivered first and
;;        has no product reader.
;;   DO-4 working's latest-parent-cut, over those cuts: the cut that covers
;;        every other, wherever it stands in the list -- it gave up at the
;;        first pair not ordered, so (A B C) and (C A B) answered apart.
;;   DO-5 state-revoked: one plan event per version, the smallest by
;;        (writer, seq) -- it kept the first met, in gate order, which is
;;        delivery order; drafts prints it.
;;   baseline-touching (baseline.sc) reads request-history and sorts it by
;;        causal weight, writer and seq, a total order: needs none.
;;   plan-members-applied (completion.sc) walks request-history; its readers
;;        sort the member indices or look up by index: needs none.
;;   NOT HERE, DELIVERY ORDER BY DESIGN: rebuild-request-state! refolds the
;;        history as delivered (relation-effects RE-9's rebuild row,
;;        consumption-index CI-17), and state->rows / rows->state keep it
;;        across a snapshot (relation-effects RE-9's snapshot rows,
;;        consumption-index CI-14).
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia reduce) reduce-empty reduce-apply! block-id state-put-events
              state-reserved-relation-records state-consumed? state-revoked
              state-consumed-parent-cuts state-seen reduce-gates)
        (only (theourgia working) latest-parent-cut))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))
(define-syntax define-caught
  (syntax-rules ()
    ((_ name e0) (define name (caught e0)))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/delivery-order-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (ask store . args) (rpc-dispatch store args "test"))
;; THE DAEMON'S ROUTE: a verb handed the state it answers from.
(define (ask-with store state . args) (rpc-dispatch store args "test" state))

;; ANOTHER WRITER'S RECORD, published as its own segment -- the way two
;; machines' records meet in one store (change-stream's mirror!).
(define (forge! store writer seq deps payload)
  (let ((bytes (encode-record seq (+ 1789000000000 seq) "peer" deps (storable-encode payload))))
    (log-publish! store writer seq bytes (segment-sha bytes))))

;; THE TWO ROUTES OF ONE STORE: S seeded from its snapshot, and a copy of S
;; without the snapshot, replayed in full.
(define (copy-without-snapshot! from to)
  (system (string-append "rm -rf '" to "'; cp -R '" from "' '" to "'; rm -rf '" to "/snap'")))

;; Finds the clause headed NAME anywhere in an answer, or #f.
(define (clause-in x name)
  (cond ((and (pair? x) (eq? (car x) name)) x)
        ((pair? x) (or (clause-in (car x) name) (clause-in (cdr x) name)))
        (else #f)))
(define (by-print xs) (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) xs))

;; EACH READER ROW CARRIES ITS CONSTRUCTION: a store whose snapshot was not
;; taken replays in full on both routes, and a reader row that did not ask
;; would read green with the two orders never built.

;; ---- DO-1 check's reserved-relations clause -----------------------------------
;;
;; Records under a reserved relation name can only be another writer's (the
;; write path refuses them now), so they are forged: writer A's first and
;; B's first, a snapshot, then A's second. A full replay delivers A1 A2 B1;
;; the snapshot-seeded state holds A1 B1 and delivers A2 after them.

(define S1 (string-append root "/s1"))
(define S1copy (string-append root "/s1-replayed"))
(ask S1 'init)
(forge! S1 "aaaa0000" 1 '() '(link "aaaa0000.9" calls "bbbb0000.9"))
(forge! S1 "bbbb0000" 1 '() '(link "bbbb0000.9" guards "aaaa0000.9"))
(define-caught snap1 (ask S1 'snapshot))
(forge! S1 "aaaa0000" 2 '() '(link "aaaa0000.9" uses "bbbb0000.9"))
(copy-without-snapshot! S1 S1copy)
(define-caught seeded1 (open-and-reduce S1))
(define-caught replayed1 (open-and-reduce S1copy))
(define expected-reserved
  '(("aaaa0000.9" calls "bbbb0000.9" (event "aaaa0000" 1))
    ("aaaa0000.9" uses "bbbb0000.9" (event "aaaa0000" 2))
    ("bbbb0000.9" guards "aaaa0000.9" (event "bbbb0000" 1))))

(define built1
  (let ((a (state-reserved-relation-records seeded1)) (b (state-reserved-relation-records replayed1)))
    (and (equal? (by-print a) (by-print b)) (not (equal? a b)) (= 3 (length a)))))
(want "DO-1 THE CONSTRUCTION: the two routes hold the same reserved records in two delivery orders"
      built1 #t)
(want "DO-1 check prints the reserved-relations clause sorted by event, writer then seq, on the snapshot-seeded route"
      (list built1 (clause-in (ask-with S1 seeded1 'check) 'reserved-relations))
      (list #t (list 'reserved-relations expected-reserved)))
(want "DO-1 and the same clause, byte for byte, on the full replay"
      (list built1 (format "~s" (clause-in (ask-with S1copy replayed1 'check) 'reserved-relations)))
      (list #t (format "~s" (list 'reserved-relations expected-reserved))))

;; ---- DO-2 commitments ---------------------------------------------------------
;;
;; Decisions made by two writers' puts, the same construction: A's first and
;; B's first, a snapshot, A's second. state-put-events gives them in two
;; orders; the commitments answer is the same.

(define S2 (string-append root "/s2"))
(define S2copy (string-append root "/s2-replayed"))
(ask S2 'init)
(define (decision-put title ord)
  (list 'put (list (cons 'kind 'decision) (cons 'title title) '(parent . root) (cons 'ord ord))))
(forge! S2 "aaaa0000" 1 '() (decision-put "Decision A one" 1))
(forge! S2 "bbbb0000" 1 '() (decision-put "Decision B one" 2))
(define-caught snap2 (ask S2 'snapshot))
(forge! S2 "aaaa0000" 2 '() (decision-put "Decision A two" 3))
(copy-without-snapshot! S2 S2copy)
(define-caught seeded2 (open-and-reduce S2))
(define-caught replayed2 (open-and-reduce S2copy))

(define built2
  (let ((a (state-put-events seeded2)) (b (state-put-events replayed2)))
    (and (equal? (by-print a) (by-print b)) (not (equal? a b)))))
(want "DO-2 THE CONSTRUCTION: the two routes give the same puts in two delivery orders"
      built2 #t)
(want "DO-2 commitments --all answers byte for byte alike on the two routes, three decisions"
      (let ((a (ask-with S2 seeded2 'commitments "--all")) (b (ask-with S2copy replayed2 'commitments "--all")))
        (list built2 (equal? (format "~s" a) (format "~s" b))
              (length (filter (lambda (x) (and (pair? x) (eq? (car x) 'decision)))
                              (let ((c (clause-in a 'items))) (if c (cdr c) '()))))))
      '(#t #t 3))

;; ---- DO-3 the consumption index -----------------------------------------------
;;
;; Two writers' plans each consume the same draft version and complete; the
;; same records delivered writer A first and writer B first. Built in the
;; reducer (consumption-index's shape), where the delivery order is the
;; fixture's to choose.

(define (plan-records writer)
  (let* ((after (cons writer 0)) (key (cons writer "p")))
    (list (list writer 1 '()
                (list 'plan "p" "fp" after
                      (list (cons 0 (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title writer)))))
                      (list 'consumes "dw" (list (list "dw.1" "v1" "h0" '()))))
                (list "test" key 'plan "fp" #f after))
          (list writer 2 '()
                (list 'put (list (cons 'kind 'section) (cons 'title writer) '(parent . root) '(ord . 1)))
                (list "test" key 0 "fp" (cons writer 1) after)))))
(define (delivered writers)
  (let ((r (reduce-empty)))
    (for-each (lambda (w) (for-each (lambda (rec) (apply reduce-apply! r rec)) (plan-records w))) writers)
    r))
(define-caught a-first (delivered '("aaaa0000" "bbbb0000")))
(define-caught b-first (delivered '("bbbb0000" "aaaa0000")))

(define built3
  (let ((a (map car (state-seen a-first "dw" "v1"))) (b (map car (state-seen b-first "dw" "v1"))))
    (and (= 2 (length a)) (equal? (by-print a) (by-print b)) (not (equal? a b)))))
(want "DO-3 THE CONSTRUCTION: the index saw both plans name the draft, in the two delivery orders"
      built3 #t)
(want "DO-3 the index's readers answer alike: consumed, revoked, and the consumed parent cuts as a set"
      (list built3
            (equal? (state-consumed? a-first "dw" "v1") (state-consumed? b-first "dw" "v1"))
            (state-consumed? a-first "dw" "v1")
            (equal? (state-revoked a-first "dw") (state-revoked b-first "dw"))
            (equal? (by-print (state-consumed-parent-cuts a-first "dw" "v1"))
                    (by-print (state-consumed-parent-cuts b-first "dw" "v1")))
            (length (state-consumed-parent-cuts a-first "dw" "v1")))
      '(#t #t #t #t #t 2))

;; ---- DO-4 latest-parent-cut ---------------------------------------------------
;;
;; A and B are not ordered; C covers both. The cut that covers every other
;; is C, wherever it stands in the list.

(define cut-a '(("aaaa0000" . 2)))
(define cut-b '(("bbbb0000" . 2)))
(define cut-c '(("aaaa0000" . 2) ("bbbb0000" . 2)))
(want "DO-4 latest-parent-cut answers C, the cut that covers A and B, in every order of the three"
      (map latest-parent-cut
           (list (list cut-a cut-b cut-c) (list cut-c cut-a cut-b) (list cut-b cut-c cut-a)))
      (list cut-c cut-c cut-c))
(want "DO-4 TWIN: with no cut covering the others it answers #f, in either order"
      (map latest-parent-cut (list (list cut-a cut-b) (list cut-b cut-a)))
      '(#f #f))

;; ---- DO-5 state-revoked -------------------------------------------------------
;;
;; consumption-index CI-15's shape: a plan consumes a draft, then a second
;; record claims its member's slot, and both members are in conflict -- two
;; gates, each naming the same version. Delivered with the second claim
;; last and with it before the plan's own member.

(define (dup-state order)
  (let ((r (reduce-empty)) (key (cons "dupm0000" "d")) (after (cons "dupm0000" 0)))
    (define plan
      (list "dupm0000" 1 '()
            (list 'plan "d" "fp" after
                  (list (cons 0 (list 'set "dupm0000.9" 'src "D")))
                  (list 'consumes "dw" (list (list "dupm0000.9" "vd" "h0" '()))))
            (list "test" key 'plan "fp" #f after)))
    (define member (list "dupm0000" 2 '() (list 'set "dupm0000.9" 'src "D") (list "test" key 0 "fp" (cons "dupm0000" 1) after)))
    (define rival (list "dupn0000" 1 '() (list 'set "dupm0000.9" 'src "D") (list "test" key 0 "fp" (cons "dupm0000" 1) after)))
    (for-each (lambda (rec) (apply reduce-apply! r rec))
              (if (eq? order 'rival-last) (list plan member rival) (list plan rival member)))
    r))
(define-caught rival-last (dup-state 'rival-last))
(define-caught rival-first (dup-state 'rival-first))
(define built5
  (let ((a (map car (reduce-gates rival-last))) (b (map car (reduce-gates rival-first))))
    (and (= 2 (length a)) (equal? (by-print a) (by-print b)) (not (equal? a b)))))
(want "DO-5 THE CONSTRUCTION: two members in conflict, their gates in two orders"
      built5 #t)
(want "DO-5 state-revoked names one plan event for the version, the smallest by (writer, seq), on both deliveries"
      (list built5
            (map (lambda (r) (list (cadr (car r)) (cadr r))) (state-revoked rival-last "dw"))
            (map (lambda (r) (list (cadr (car r)) (cadr r))) (state-revoked rival-first "dw")))
      (list #t '(("vd" ("dupm0000" . 2))) '(("vd" ("dupm0000" . 2)))))

(printf "rows: ~a\n~a failures\ndelivery-order complete\n" rows bad)
(system (string-append "rm -rf '" root "'"))
(exit (if (= bad 0) 0 1))
