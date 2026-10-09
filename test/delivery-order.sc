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
;;        rebuild-consumption-from-history!): keyed tables; its product
;;        readers (state-consumed?, state-revoked, state-consumed-parent-cuts,
;;        in working.sc) need no delivery order. state-seen keeps its
;;        candidates newest-delivered first and has no product reader.
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
              state-consumed-parent-cuts state-seen))

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

(want "DO-1 THE CONSTRUCTION: the two routes hold the same reserved records in two delivery orders"
      (let ((a (state-reserved-relation-records seeded1)) (b (state-reserved-relation-records replayed1)))
        (list (equal? (by-print a) (by-print b)) (equal? a b) (length a)))
      '(#t #f 3))
(want "DO-1 check prints the reserved-relations clause sorted by event, writer then seq, on the snapshot-seeded route"
      (clause-in (ask-with S1 seeded1 'check) 'reserved-relations)
      (list 'reserved-relations expected-reserved))
(want "DO-1 and the same clause, byte for byte, on the full replay"
      (format "~s" (clause-in (ask-with S1copy replayed1 'check) 'reserved-relations))
      (format "~s" (list 'reserved-relations expected-reserved)))

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

(want "DO-2 THE CONSTRUCTION: the two routes give the same puts in two delivery orders"
      (let ((a (state-put-events seeded2)) (b (state-put-events replayed2)))
        (list (equal? (by-print a) (by-print b)) (equal? a b)))
      '(#t #f))
(want "DO-2 commitments --all answers byte for byte alike on the two routes, three decisions"
      (let ((a (ask-with S2 seeded2 'commitments "--all")) (b (ask-with S2copy replayed2 'commitments "--all")))
        (list (equal? (format "~s" a) (format "~s" b))
              (length (filter (lambda (x) (and (pair? x) (eq? (car x) 'decision)))
                              (let ((c (clause-in a 'items))) (if c (cdr c) '()))))))
      '(#t 3))

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

(want "DO-3 THE CONSTRUCTION: the index saw both plans name the draft, in the two delivery orders"
      (let ((a (map car (state-seen a-first "dw" "v1"))) (b (map car (state-seen b-first "dw" "v1"))))
        (list (length a) (equal? (by-print a) (by-print b)) (equal? a b)))
      '(2 #t #f))
(want "DO-3 the index's product readers answer alike: consumed, revoked, the consumed parent cuts"
      (list (equal? (state-consumed? a-first "dw" "v1") (state-consumed? b-first "dw" "v1"))
            (state-consumed? a-first "dw" "v1")
            (equal? (state-revoked a-first "dw") (state-revoked b-first "dw"))
            (equal? (format "~s" (state-consumed-parent-cuts a-first "dw" "v1"))
                    (format "~s" (state-consumed-parent-cuts b-first "dw" "v1"))))
      '(#t #t #t #t))

(printf "rows: ~a\n~a failures\ndelivery-order complete\n" rows bad)
(system (string-append "rm -rf '" root "'"))
(exit (if (= bad 0) 0 1))
