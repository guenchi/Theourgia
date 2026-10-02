#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; `commitments`: which decisions are still owed, and which implementations
;; changed after they were said to implement one.
;;
;; THE QUESTIONS ABOUT TIME ARE ASKED OF REDUCTIONS BUILT RECORD BY RECORD.
;; Who wrote what having seen what is the whole subject, and a store written
;; through the command line has one writer whose sequence numbers only grow:
;; nothing there can make two origins incomparable, or give the causally
;; later event the smaller number. So the first half feeds the reducer
;; records (writer seq deps payload) and asks the query directly; the second
;; half asks a real store through each route a caller has.
;;
;; A CUT IN AN EXPECTED ANSWER IS WORKED OUT FROM THE RECORDS ABOVE IT: the
;; cut just after an event holds that event and its causal past, one entry
;; per writer, sorted by writer.
(import (chezscheme) (theourgia rpc) (theourgia ffi)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia commitments) commitments-answer)
        (only (theourgia reduce) reduce-empty reduce-apply! state-datum state-read state-event-cut
              cut-covers? state-put-events state-field-events state-link-events state-block-ids)
        (only (theourgia store) open-and-reduce)
        (only (theourgia render) render-human render-wire)
        (only (theourgia wire) string->sexpr-extended)
        (only (theourgia crc32) crc32-hex))

(include "forge-record.ss")

;; THE VALUES A ROW READS ARE TAKEN IN THE ORDER THEY ARE WRITTEN: R6RS
;; leaves the order of a call's arguments open.
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
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

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))

;; A FIXTURE IS A CALLER LIKE ANY OTHER: the verb exists in this process once
;; it is registered, as core.sc and the daemon register it.
(register-verbs! extension-verbs)

;; ---- reductions ---------------------------------------------------------------

(define (state-of . events)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e)) events)
    r))
(define (feed! r . events)
  (for-each (lambda (e) (apply reduce-apply! r e)) events))

(define (decision title) `(put ((kind . decision) (title . ,title))))
(define (decision-under title parent) `(put ((kind . decision) (title . ,title) (parent . ,parent) (ord . 1))))
(define (section title) `(put ((kind . section) (title . ,title))))
(define (section-under title parent) `(put ((kind . section) (title . ,title) (parent . ,parent) (ord . 1))))
(define (code title) `(put ((kind . code) (title . ,title))))

;; (q r) is the default, `--open`; options as keywords after it.
(define (q r . opts)
  (commitments-answer r
                      (if (memq 'all opts) 'all 'open)
                      (and (memq 'drifted opts) #t)
                      (let ((s (memq 'since opts))) (and s (cadr s)))
                      (let ((u (memq 'under opts))) (and u (cadr u)))))

(define (items-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (eq? (caadr a) 'items)) (cdadr a) (list 'NOT-ITEMS a)))
(define (decisions-of a) (filter (lambda (x) (and (pair? x) (eq? (car x) 'decision))) (items-of a)))
(define (ids a) (map cadr (decisions-of a)))
(define (row a id) (find (lambda (x) (equal? (cadr x) id)) (decisions-of a)))
(define (clause a id name)
  (let ((r (row a id))) (if r (let ((c (assq name (cddr r)))) (if c (cdr c) 'NO-CLAUSE)) 'NO-ROW)))
(define (skipped-of a) (map cdr (filter (lambda (x) (and (pair? x) (eq? (car x) 'skipped))) (items-of a))))

;; ---- the five reducer exports, each on its own ---------------------------------

(define acc
  (state-of '("a" 1 () (put ((kind . decision) (title . "D"))))
            '("a" 2 () (set "a.1" summary "s"))
            '("a" 3 () (set "a.1" summary))
            '("a" 4 () (move "a.1" root 5))
            '("a" 5 () (put ((kind . code) (title . "I"))))
            '("b" 1 (("a" . 5)) (link "a.5" implements "a.1"))
            '("c" 1 (("a" . 5)) (link "a.5" implements "a.1"))
            '("a" 6 () (put ((kind . code) (title . "J"))))
            '("a" 7 () (link "a.6" implements "a.1"))
            '("a" 8 () (unlink "a.6" implements "a.1"))
            '("q" 1 () (set "q.7" title "placeholder"))))

(want "X1 state-field-events: an unknown id has none"
      (state-field-events acc "zz.1")
      '())
(want "X1 state-field-events: the put's fields and the absence that superseded `summary`, not the move"
      (list-sort (lambda (x y) (< (cdr x) (cdr y))) (state-field-events acc "a.1"))
      '(("a" . 1) ("a" . 1) ("a" . 3)))
(want "X2 state-link-events: two concurrent links leave two events"
      (list-sort (lambda (x y) (string<? (car x) (car y))) (state-link-events acc "a.5" 'implements "a.1"))
      '(("b" . 1) ("c" . 1)))
(want "X2 state-link-events: none after an unlink that had seen the link"
      (state-link-events acc "a.6" 'implements "a.1")
      '())
(want "X3 cut-covers?: a component absent from a and present in b is not covered; present at or above is"
      (list (cut-covers? '(("a" . 3)) '(("a" . 1) ("b" . 1)))
            (cut-covers? '(("a" . 3) ("b" . 2)) '(("a" . 1) ("b" . 1)))
            (cut-covers? '(("a" . 3)) '(("a" . 4)))
            (cut-covers? '(("a" . 3)) '()))
      '(#f #t #f #t))
(want "X4 state-put-events: the applied puts, and no event for an id only a set named"
      (state-put-events acc)
      '(("a" . 1) ("a" . 5) ("a" . 6)))
(want "X5 state-block-ids: alive ids in the order the reduction made them"
      (list (state-block-ids acc)
            (state-block-ids (state-of '("a" 1 () (put ((kind . section) (title . "x")))) '("a" 2 () (del "a.1")))))
      '(("a.1" "a.5" "a.6" "q.7") ()))

;; ---- C1: what discharges a decision ---------------------------------------------
;;
;; An id is the creating put's writer and its sequence number IN BASE 36:
;; the put at seq 11 makes "a.b", at 26 "a.q".

(define c1
  (state-of `("a" 1 () ,(decision "open"))
            `("a" 2 () ,(decision "implemented"))
            `("a" 3 () ,(code "impl"))
            '("a" 4 () (link "a.3" implements "a.2"))
            `("a" 5 () ,(decision "done, string"))
            '("a" 6 () (set "a.5" status "done"))
            `("a" 7 () ,(decision "done, symbol"))
            '("a" 8 () (set "a.7" status done))
            `("a" 9 () ,(decision "dropped, symbol"))
            '("a" 10 () (set "a.9" status dropped))
            `("a" 11 () ,(decision "dropped, string"))
            '("a" 12 () (set "a.b" status "dropped"))
            `("a" 13 () ,(decision "referenced only"))
            '("a" 14 () (link "a.3" ref "a.d"))
            `("a" 15 () ,(decision "status wip"))
            '("a" 16 () (set "a.f" status "wip"))
            `("a" 17 () ,(decision "status a number"))
            '("a" 18 () (set "a.h" status 5))
            `("a" 19 () ,(decision "deleted"))
            '("a" 20 () (del "a.j"))
            `("a" 21 () ,(decision "kind changed"))
            '("a" 22 () (set "a.l" kind section))
            `("a" 23 () ,(decision "done and implemented"))
            '("a" 24 () (set "a.n" status done))
            '("a" 25 () (link "a.3" implements "a.n"))
            `("a" 26 () ,(decision "status in conflict"))
            '("b" 1 (("a" . 26)) (set "a.q" status "done"))
            '("a" 27 () (set "a.q" status "open"))))

(want "C1 --open lists the open decisions, oldest first"
      (ids (q c1))
      '("a.1" "a.d" "a.f" "a.h" "a.q"))
(want "C1 --all lists every decision once, and not a deleted one or one whose kind changed"
      (ids (q c1 'all))
      '("a.1" "a.2" "a.5" "a.7" "a.9" "a.b" "a.d" "a.f" "a.h" "a.n" "a.q"))
(want "C1 an implements edge discharges, and says by what and when"
      (list (clause (q c1 'all) "a.2" 'implemented-by) (clause (q c1 'all) "a.2" 'status))
      '((("a.3" (("a" . 4)))) (absent)))
(want "C1 status done as a string and as a symbol, dropped as a symbol and as a string"
      (map (lambda (id) (clause (q c1 'all) id 'status)) '("a.5" "a.7" "a.9" "a.b"))
      '((done) (done) (dropped) (dropped)))
(want "C1 an edge of another relation does not discharge"
      (list (and (member "a.d" (ids (q c1))) #t) (clause (q c1) "a.d" 'implemented-by))
      '(#t ()))
(want "C1 a status that cannot be read stays open, and says what it read"
      (map (lambda (id) (clause (q c1) id 'status)) '("a.f" "a.h" "a.q" "a.1"))
      '((unreadable "wip") (unreadable "5") (conflict) (absent)))
(want "C1 done and implemented: listed once, with both facts"
      (list (length (filter (lambda (id) (equal? id "a.n")) (ids (q c1 'all))))
            (clause (q c1 'all) "a.n" 'status)
            (clause (q c1 'all) "a.n" 'implemented-by))
      '(1 (done) (("a.3" (("a" . 25))))))
(define c1a
  (state-of `("a" 1 () ,(decision "status is the symbol absent"))
            '("a" 2 () (set "a.1" status absent))
            `("a" 3 () ,(decision "no status"))))
(want "C1 a status whose value is the symbol absent is unreadable, not missing; a missing one is absent"
      (list (clause (q c1a) "a.1" 'status) (clause (q c1a) "a.3" 'status))
      '((unreadable "absent") (absent)))
(want "C1 the whole row of an open decision"
      (row (q c1) "a.1")
      '(decision "a.1" (title "open") (status absent) (origin (("a" . 1))) (implemented-by)))

;; A DELETED SOLE IMPLEMENTER REOPENS THE DECISION, and the answer says why.
(define c1d
  (state-of `("a" 1 () ,(decision "D"))
            `("a" 2 () ,(code "I"))
            '("a" 3 () (link "a.2" implements "a.1"))
            '("a" 4 () (del "a.2"))))
(want "C1 a deleted sole implementer leaves the decision open, named under implementer-deleted"
      (q c1d)
      '(ok (items (decision "a.1" (title "D") (status absent) (origin (("a" . 1)))
                            (implemented-by) (implementer-deleted "a.2")))))

;; ---- C2: the order ----------------------------------------------------------------
;;
;; z.1 is the older decision. Its id, its title and its place in the
;; enumeration all say otherwise: a.1 sorts first, "A younger" sorts first,
;; and m's early set on a.1 makes a.1 the first block the reduction knows.
;; z.1 is edited after a.1 exists, so its frontier is the later one too.
(define c2
  (state-of '("m" 1 () (set "a.1" note "early"))
            `("z" 1 () ,(decision "B older"))
            `("a" 1 (("z" . 1)) ,(decision "A younger"))
            '("z" 2 (("a" . 1)) (set "z.1" summary "edited after a.1 was made"))))

(want "C2 CONTROL: the enumeration, the ids and the titles all put the younger one first"
      (list (state-block-ids c2) (string<? "a.1" "z.1") (string<? "A younger" "B older"))
      '(("a.1" "z.1") #t #t))
(want "C2 the listing is by origin, older first"
      (ids (q c2 'all))
      '("z.1" "a.1"))

;; THREE WRITERS, the cyclic witness. Z=((z . 3)), A=((a . 1) (z . 3)),
;; M=((m . 2)): Z is before A, M is ordered against neither, and A, the
;; causally later of the ordered pair, has the smaller sequence number.
(define c2w
  (state-of `("z" 1 () ,(section "filler"))
            `("z" 2 () ,(section "filler"))
            `("z" 3 () ,(decision "Z"))
            `("m" 1 () ,(section "filler"))
            `("m" 2 () ,(decision "M"))
            `("a" 1 (("z" . 3)) ,(decision "A"))))

(want "C2 the origins are the witness"
      (map (lambda (id) (clause (q c2w) id 'origin)) '("z.3" "m.2" "a.1"))
      '(((("z" . 3))) ((("m" . 2))) ((("a" . 1) ("z" . 3)))))
(want "C2 topological, (writer, seq) of the creating put choosing among the eligible"
      (ids (q c2w))
      '("m.2" "z.3" "a.1"))
(want "C2 concurrent-with names every listed decision the origin is not ordered against"
      (map (lambda (id) (clause (q c2w) id 'concurrent-with)) '("m.2" "z.3" "a.1"))
      '(("z.3" "a.1") ("m.2") ("m.2")))

;; ---- C3: drift ----------------------------------------------------------------------

;; Writer a decides and edits the decision up to seq 6; writer b, having
;; seen that, writes the implementation and links it. b's numbers stay
;; below a's throughout.
(define c3
  (state-of `("a" 1 () ,(decision "D"))
            `("a" 2 () ,(section "filler"))
            `("a" 3 () ,(section "filler"))
            `("a" 4 () ,(section "filler"))
            `("a" 5 () ,(section "filler"))
            '("a" 6 () (set "a.1" summary "s1"))
            `("b" 1 (("a" . 6)) ,(code "I"))
            '("b" 2 () (link "b.1" implements "a.1"))))

(want "C3 an implementer written after the decision's last edit and then linked has not drifted"
      (list (ids (q c3 'all 'drifted)) (clause (q c3 'all) "a.1" 'implemented-by))
      '(() (("b.1" (("a" . 6) ("b" . 2))))))
(feed! c3 '("b" 3 () (set "b.1" src "changed")))
(want "C3 edited after the link: drifted, with the change cut, which is not the link's cut"
      (list (ids (q c3 'all 'drifted))
            (clause (q c3 'all) "a.1" 'drifted)
            (clause (q c3 'all) "a.1" 'implemented-by))
      '(("a.1") (("b.1" (("a" . 6) ("b" . 3)))) (("b.1" (("a" . 6) ("b" . 2))))))
(feed! c3 '("a" 7 (("b" . 3)) (set "a.1" summary "s2")))
(want "C3 the decision edited by a writer who had seen the change: not drifted"
      (ids (q c3 'all 'drifted))
      '())

;; TWO CONCURRENT EDITS OF THE DECISION'S ONE FIELD, one by a writer who had
;; seen the implementation's change and one by a writer who had not. Both
;; survive, and the frontier is their join; choosing one is red for one of
;; the two orders of the writers' names.
(define (c3-join seen-by unseen-by)
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "I"))
            '("b" 2 () (link "b.1" implements "a.1"))
            '("b" 3 () (set "b.1" src "changed"))
            `(,seen-by 1 (("b" . 3)) (set "a.1" summary "seen"))
            `(,unseen-by 1 (("a" . 1)) (set "a.1" summary "not seen"))))
(want "C3 two concurrent edits of the decision join: not drifted, either order of names"
      (list (ids (q (c3-join "c" "x") 'all 'drifted)) (ids (q (c3-join "x" "c") 'all 'drifted)))
      '(() ()))
(want "C3 CONTROL: the concurrent edits are both still there"
      (let ((fs (cdr (assq 'fields (state-read (c3-join "c" "x") "a.1")))))
        (car (cdr (assq 'summary fs))))
      'conflict)

;; AN EDGE OF ANOTHER RELATION, LINKED AND UNLINKED AFTER EVERYTHING: not a
;; change of either end. d1's implementer was attested and stays so; d2's
;; had drifted and stays drifted.
(define c3e
  (state-of `("a" 1 () ,(decision "D1"))
            `("b" 1 (("a" . 1)) ,(code "I1"))
            '("b" 2 () (link "b.1" implements "a.1"))
            `("a" 2 () ,(decision "D2"))
            `("b" 3 (("a" . 2)) ,(code "I2"))
            '("b" 4 () (link "b.3" implements "a.2"))
            '("b" 5 () (set "b.3" src "changed"))))
(define c3e-before (list (ids (q c3e 'all 'drifted)) (clause (q c3e 'all) "a.2" 'drifted)))
;; One edge at each end SURVIVES, so that a reading that took a surviving edge
;; for a change of either end has something to take; the third is linked and
;; unlinked.
(feed! c3e
       '("a" 3 (("b" . 5)) (link "b.1" ref "a.1"))
       '("a" 4 () (link "b.3" ref "a.2"))
       '("a" 5 () (link "b.1" ref "a.2"))
       '("a" 6 () (unlink "b.1" ref "a.2")))
(want "C3 CONTROL: the two ref edges are there after the queries' edges were written"
      (list (state-link-events c3e "b.1" 'ref "a.1") (state-link-events c3e "b.3" 'ref "a.2")
            (state-link-events c3e "b.1" 'ref "a.2"))
      '((("a" . 3)) (("a" . 4)) ()))
(want "C3 CONTROL: before the edges, d2 has drifted and d1 has not"
      c3e-before
      '(("a.2") (("b.3" (("a" . 2) ("b" . 5))))))
(want "C3 a link and an unlink of another relation make no drift, and do not clear one"
      (list (ids (q c3e 'all 'drifted)) (clause (q c3e 'all) "a.2" 'drifted))
      '(("a.2") (("b.3" (("a" . 2) ("b" . 5))))))

(define c3two
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "first"))
            '("b" 2 () (link "b.1" implements "a.1"))
            `("b" 3 () ,(code "second"))
            '("b" 4 () (link "b.3" implements "a.1"))
            '("b" 5 () (set "b.3" src "changed"))))
(want "C3 two implementers, the second drifted: the second is named"
      (clause (q c3two 'all) "a.1" 'drifted)
      '(("b.3" (("a" . 1) ("b" . 5)))))

(define c3re
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "I"))
            '("b" 2 () (link "b.1" implements "a.1"))
            '("b" 3 () (set "b.1" src "changed"))))
(define c3re-before (ids (q c3re 'all 'drifted)))
(feed! c3re '("b" 4 () (unlink "b.1" implements "a.1")) '("b" 5 () (link "b.1" implements "a.1")))
(want "C3 unlinked and linked again by a writer who had seen the change: attested anew"
      (list c3re-before (ids (q c3re 'all 'drifted)) (clause (q c3re 'all) "a.1" 'implemented-by))
      '(("a.1") () (("b.1" (("a" . 1) ("b" . 5))))))

(define c3conc
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "I"))
            '("c" 1 (("b" . 1)) (set "b.1" src "c's edit"))
            '("b" 2 () (link "b.1" implements "a.1"))))
(want "C3 an edit by a second writer that the linker had not seen: drifted"
      (clause (q c3conc 'all) "a.1" 'drifted)
      '(("b.1" (("a" . 1) ("b" . 1) ("c" . 1)))))

(define c3links
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "I"))
            '("b" 2 () (set "b.1" src "second version"))
            '("c" 1 (("b" . 1)) (link "b.1" implements "a.1"))
            '("d" 1 (("b" . 2)) (link "b.1" implements "a.1"))))
(want "C3 two concurrent links: the attestation is their join"
      (list (ids (q c3links 'all 'drifted)) (clause (q c3links 'all) "a.1" 'implemented-by))
      '(() (("b.1" (("a" . 1) ("b" . 2) ("c" . 1) ("d" . 1))))))

(define c3move
  (state-of `("a" 1 () ,(decision "D"))
            `("b" 1 (("a" . 1)) ,(code "I"))
            '("b" 2 () (link "b.1" implements "a.1"))
            '("b" 3 () (move "b.1" root 9))))
(want "C3 a move of the implementer after the link is not a change"
      (ids (q c3move 'all 'drifted))
      '())

;; ---- C4: --under ---------------------------------------------------------------------

(define c4
  (state-of `("a" 1 () ,(section "S"))
            `("a" 2 () ,(section-under "S1" "a.1"))
            `("a" 3 () ,(decision-under "deep" "a.2"))
            `("a" 4 () ,(section "T"))
            `("a" 5 () ,(decision-under "sibling" "a.4"))
            '("a" 6 () (put ((kind . "decision") (title . "legacy inside S") (parent . "a.2") (ord . 2))))
            `("b" 1 (("a" . 6)) ,(code "in"))
            '("b" 2 () (link "b.1" implements "a.3"))
            '("b" 3 () (set "b.1" src "changed"))
            `("b" 4 () ,(code "out"))
            '("b" 5 () (link "b.4" implements "a.5"))
            '("b" 6 () (set "b.4" src "changed"))))

(want "C4 CONTROL: without --under both drifted decisions are there"
      (ids (q c4 'all 'drifted))
      '("a.3" "a.5"))
(want "C4 --under a block: the decision two levels down, not the one under a sibling"
      (list (ids (q c4 'all 'under "a.1")) (ids (q c4 'all 'under "a.4")))
      '(("a.3") ("a.5")))
(want "C4 the root block itself is included"
      (ids (q c4 'all 'under "a.3"))
      '("a.3"))
(want "C4 with --since and with --drifted the scope still applies"
      (list (ids (q c4 'all 'under "a.1" 'since '())) (ids (q c4 'all 'drifted 'under "a.1")))
      '(("a.3") ("a.3")))
(want "C4 skipped rows are listed only inside the scope"
      (list (skipped-of (q c4 'under "a.1")) (skipped-of (q c4 'under "a.4")))
      '((("a.6" kind-not-a-symbol)) ()))
(want "C4 an unknown root answers unknown-id"
      (let ((a (q c4 'under "zz.9"))) (and (pair? a) (list (car a) (cadr a) (caddr a))))
      '(error unknown-id "zz.9"))

;; ---- C5: --since ---------------------------------------------------------------------

(define c5
  (state-of `("a" 1 () ,(decision "older"))
            `("a" 2 () ,(decision "younger"))))
(want "C5 since the first decision's origin: only the later one"
      (ids (q c5 'all 'since '(("a" . 1))))
      '("a.2"))
(feed! c5 '("a" 3 () (set "a.1" summary "edited past the boundary")))
(want "C5 the older one edited past the boundary still does not list: origin, not frontier"
      (ids (q c5 'all 'since '(("a" . 1))))
      '("a.2"))
(want "C5 a cut covering both: none"
      (ids (q c5 'all 'since '(("a" . 2))))
      '())
(feed! c5 `("b" 1 () ,(decision "another writer's")))
(want "C5 a cut from another writer lists what it does not cover, and only that"
      (list (ids (q c5 'all 'since '(("a" . 3)))) (ids (q c5 'all 'since '(("b" . 1)))))
      '(("b.1") ("a.1" "a.2")))

;; ---- C6: blocks that cannot be read as decisions ----------------------------------------

(define c6
  (state-of '("a" 1 () (put ((kind . "decision") (title . "legacy"))))
            '("a" 2 () (put ((kind . decision) (title . 5))))
            '("q" 1 () (set "q.9" kind decision))
            '("q" 2 () (set "q.9" title "placeholder"))
            '("a" 3 () (put ((kind . section) (title . "plain"))))
            `("a" 4 () ,(decision "readable"))
            '("a" 5 () (put ((kind . decision) (title . "kind in conflict"))))
            '("c" 1 (("a" . 5)) (set "a.5" kind section))
            '("a" 6 () (set "a.5" kind decision))))
(define c6-skipped '(("a.1" kind-not-a-symbol) ("a.2" unreadable-block) ("a.5" unreadable-block) ("q.9" no-origin)))
(want "C6 each is a skipped row with its reason, and never a decision"
      (list (ids (q c6 'all)) (skipped-of (q c6 'all)))
      (list '("a.4") c6-skipped))
(want "C6 skipped rows are there under --drifted and --since too"
      (list (skipped-of (q c6 'drifted)) (skipped-of (q c6 'since '(("a" . 9) ("c" . 9) ("q" . 9)))))
      (list c6-skipped c6-skipped))
(want "C6 skipped is an item, so the human rendering prints it"
      (let ((h (render-human (q c6 'all))))
        (map (lambda (needle) (and (string-contains? h needle) #t))
             '("(skipped \"a.1\" kind-not-a-symbol)" "(skipped \"q.9\" no-origin)")))
      '(#t #t))

;; A PUT THE REDUCER REFUSED MAKES NO ORIGIN. Its writer's cursor still moves
;; past it, so "applied" alone would take it for the block's creation.
(define c6m
  (state-of '("a" 1 () (put broken))
            '("a" 2 () (set "a.1" kind decision))
            '("a" 3 () (set "a.1" title "D"))))
(want "C6 a decision whose put was malformed is a no-origin skipped row, and not in the put events"
      (list (skipped-of (q c6m 'all)) (ids (q c6m 'all)) (state-put-events c6m))
      '((("a.1" no-origin)) () ()))

;; A STORED VALUE WITH THE HEAD `conflict` AND NOT THE SHAPE the reducer gives
;; one: it came from a record, and reading it must not stop the query.
(define c6c
  (state-of '("a" 1 () (put ((kind . (conflict)) (title . "legacy"))))
            `("a" 2 () ,(decision "readable"))))
(want "C6 a stored kind shaped (conflict) is not a decision and does not stop the answer"
      (let ((a (q c6c 'all))) (list (car a) (ids a) (skipped-of a)))
      '(ok ("a.2") ()))

;; ---- the real store: C6 from a forged log, C7, C8 ---------------------------------------

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/commitments-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/cm" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))


(define (run . args) (rpc-dispatch store args "test"))
(define (id-by-title title)
  (let ((s (open-and-reduce store)))
    (find (lambda (id)
            (let ((f (assq 'title (cdr (assq 'fields (state-read s id))))))
              (and f (equal? (cdr f) title))))
          (state-block-ids s))))

;; ALL THE WRITES COME FIRST. Every query below runs after the pins are taken
;; and before they are taken again, so "nothing was written" is asked of
;; every query in this half, on every route.
(run 'init)
(run 'insert "--under" "root" "--title" "Adopt the ledger")
(run 'insert "--under" "root" "--title" "Keep the wire in UTF-8")
(run 'insert "--under" "root" "--title" "Already done")
(run 'insert "--under" "root" "--title" "The ledger, implemented")
(define d1 (id-by-title "Adopt the ledger"))
(define d2 (id-by-title "Keep the wire in UTF-8"))
(define d3 (id-by-title "Already done"))
(define impl (id-by-title "The ledger, implemented"))
(run 'set d1 "kind" "decision")
(run 'set d2 "kind" "decision")
(run 'set d3 "kind" "decision")
(run 'set d3 "status" "done")
;; The event the link was written at, from the link's own answer.
(define (first-event answer)
  (let find ((x answer))
    (cond ((and (pair? x) (eq? (car x) 'events) (pair? (cdr x)) (pair? (cadr x)) (pair? (car (cadr x))))
           (car (cadr x)))
          ((pair? x) (or (find (car x)) (find (cdr x))))
          (else #f))))
(define link-event (first-event (run 'link impl "implements" d1)))
;; A DRAFT IS NOT A CHANGE: written and never committed.
(define draft (run 'write impl "a new body" "--writer" "drafter"))
;; C6 FROM A LOG THE FIXTURE WROTE: below the caller's checks, as a record
;; from another build arrives. The store's own writer is read from an id it
;; made: the draft made a second writer directory, and the first entry of a
;; listing may be that one.
(define writer (let loop ((i (- (string-length d1) 1)))
                 (if (char=? (string-ref d1 i) #\.) (substring d1 0 i) (loop (- i 1)))))
(define forged-seq (forge-record-as! store writer "(put ((kind . \"decision\") (title . \"legacy\")))"))
(define forged-id (string-append writer "." (string-downcase (number->string forged-seq 36))))

(want "C1 CONTROL: the store was built (four ids, the link's event, the draft)"
      (list (string? d1) (string? d2) (string? d3) (string? impl) (pair? link-event) (car draft))
      '(#t #t #t #t #t ok))

;; THE EXPECTED ROWS, from the ids and the link's event alone: an id is its
;; creating put's writer and base-36 sequence number, and the cut after an
;; event is the reduction's `state-event-cut`, which this query did not add.
(define (event-of-id id)
  (let loop ((i (- (string-length id) 1)))
    (if (char=? (string-ref id i) #\.)
        (cons (substring id 0 i) (string->number (substring id (+ i 1) (string-length id)) 36))
        (loop (- i 1)))))
(define built (open-and-reduce store))
(define (cut-after event) (state-event-cut built event))
(define expected-d1
  (list 'decision d1 '(title "Adopt the ledger") '(status absent) (list 'origin (cut-after (event-of-id d1)))
        (list 'implemented-by (list impl (cut-after link-event)))))
(define expected-d2
  (list 'decision d2 '(title "Keep the wire in UTF-8") '(status absent) (list 'origin (cut-after (event-of-id d2)))
        '(implemented-by)))
(define expected-d3
  (list 'decision d3 '(title "Already done") '(status done) (list 'origin (cut-after (event-of-id d3)))
        '(implemented-by)))

;; THE PINS, after the last write and before the first query.
(define (log-bytes)
  (let ((f (string-append root "/size.txt")))
    (system (string-append "find '" store "' -name '*.sexp' -path '*writers*' -exec cat {} + | wc -c | tr -d ' ' > '" f "'"))
    (file-text f)))
(define (file-list)
  (let ((f (string-append root "/files.txt")))
    (system (string-append "cd '" store "' && find . | LC_ALL=C sort > '" f "'"))
    (file-text f)))
(define published (open-and-reduce store))
(define pins-before (list (log-bytes) (file-list) (state-datum published)))

(want "C7 a populated query precedes the pins' second reading"
      (let ((a (rpc-dispatch store (list 'commitments "--all") "test" published)))
        (list (car a) (ids a)))
      (list 'ok (list d1 d2 d3)))
(want "C1 status done written by the command line is a string, and discharges"
      (list (ids (run 'commitments)) (clause (run 'commitments "--all") d3 'status))
      (list (list d2) '(done)))
(want "C3 a draft written and not committed does not make drift"
      (ids (run 'commitments "--all" "--drifted"))
      '())
(want "C5 --since reads a cut written as text"
      (ids (run 'commitments "--all" "--since"
                (call-with-string-output-port (lambda (p) (write (cut-after (event-of-id d1)) p)))))
      (list d2 d3))
(want "C4 --under with an empty id is a usage answer, as are --open and --all together"
      (list (car (run 'commitments "--under" "")) (car (run 'commitments "--open" "--all"))
            (car (run 'commitments "--since" "not a cut")) (car (run 'commitments "extra")))
      '(usage usage usage usage))
(want "C6 a kind spelt as a string, from the log: skipped in the wire answer and in human output"
      (let ((a (run 'commitments)))
        (list (and (member (list forged-id 'kind-not-a-symbol) (skipped-of a)) #t)
              (and (string-contains? (render-wire a) (string-append "(skipped \"" forged-id "\" kind-not-a-symbol)")) #t)
              (and (string-contains? (render-human a) (string-append "(skipped \"" forged-id "\" kind-not-a-symbol)")) #t)))
      '(#t #t #t))

;; ---- C8: three routes, one answer ---------------------------------------------------

(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run THEOURGIA_TRACE=1 " extra " "))
(define (sh-out name command)
  (let ((out (string-append root "/" name ".out")))
;; A command that names its own input keeps it: a second `<` would win.
    (system (string-append command " > '" out "' 2> '" root "/" name ".err'"
                           (if (string-contains? command " < ") "" " < /dev/null")))
    (file-text out)))
(define (first-datum text)
  (guard (e (#t (list 'UNREADABLE text)))
    (string->sexpr-extended (let loop ((i 0))
                              (cond ((>= i (string-length text)) text)
                                    ((char=? (string-ref text i) #\newline) (substring text 0 i))
                                    (else (loop (+ i 1))))))))

(define local-wire
  (sh-out "local" (string-append (env "THEOURGIA_LOCAL=1") "scheme --script ../core.sc commitments --all --store '" store "' --wire")))
;; The daemon's own trace lines saying it dispatched a request, counted
;; just before and just after the client's commitments call, so the
;; difference is that call's.
(define (served-count)
  (let ((f (string-append root "/served.txt")))
    (system (string-append "cat '" sock-root "'/run/*/serve.log 2>/dev/null | grep -c 'daemon-dispatch' > '" f "' || true"))
    (let ((t (file-text f))) (if (> (string-length t) 0) (string->number (substring t 0 (- (string-length t) 1))) 0))))
;; THE DAEMON'S OWN STATE, read through other verbs before and after it
;; answers commitments: the pins below read the disk and this process's
;; reduction, and cannot see the daemon's memory.
(define (daemon-says tag . verb)
  (sh-out tag (string-append (env "") "scheme --script ../theourgia.sc " (apply string-append verb) " --store '" store "' --wire")))
(define daemon-before (list (daemon-says "d-outline-1" "outline") (daemon-says "d-read-1" "read " d1)))
(define served-before-client (served-count))
(define client-wire
  (sh-out "client" (string-append (env "") "scheme --script ../theourgia.sc commitments --all --store '" store "' --wire")))
(define served-after-client (served-count))
(define daemon-after (list (daemon-says "d-outline-2" "outline") (daemon-says "d-read-2" "read " d1)))
(want "C7 the daemon answers outline and read as it did before it answered commitments, and both answered ok"
      (list (equal? daemon-before daemon-after) (map (lambda (t) (car (first-datum t))) daemon-before))
      '(#t (ok ok)))

(define mcp-out
  (let ((in (string-append root "/mcp-in.jsonl")))
    (call-with-output-file in
      (lambda (p)
        (put-string p (string-append
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                        "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                        "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                        "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":"
                        "{\"name\":\"theourgia_commitments\",\"arguments\":{\"argv\":[\"--all\"]}}}\n"))))
    (sh-out "mcp" (string-append (env "") "scheme --script ../mcp/server.sc --store '" store "' < '" in "'"))))
;; The text of the id-3 answer's first content item, unescaped.
(define mcp-text
  (let* ((at (let find ((i 0))
               (cond ((> (+ i 6) (string-length mcp-out)) #f)
                     ((string=? (substring mcp-out i (+ i 6)) "\"id\":3") i)
                     (else (find (+ i 1))))))
         (key "\"text\":\"")
         (k (and at (let find ((i at))
                      (cond ((> (+ i (string-length key)) (string-length mcp-out)) #f)
                            ((string=? (substring mcp-out i (+ i (string-length key))) key) (+ i (string-length key)))
                            (else (find (+ i 1))))))))
    (and k (let loop ((i k) (acc '()))
             (cond ((>= i (string-length mcp-out)) #f)
                   ((char=? (string-ref mcp-out i) #\\)
                    (let ((c (string-ref mcp-out (+ i 1))))
                      (loop (+ i 2) (cons (cond ((char=? c #\n) #\newline) ((char=? c #\t) #\tab) (else c)) acc))))
                   ((char=? (string-ref mcp-out i) #\") (list->string (reverse acc)))
                   (else (loop (+ i 1) (cons (string-ref mcp-out i) acc))))))))

(want "C8 the local route answers exactly the decision rows worked out from the records, in order, then the skipped row"
      (let ((a (first-datum local-wire))) (list (car a) (items-of a)))
      (list 'ok (list expected-d1 expected-d2 expected-d3 (list 'skipped forged-id 'kind-not-a-symbol))))
(want "C8 the thin client's answer, through the daemon, equals the local one byte for byte"
      (if (string=? client-wire local-wire) 'identical (list 'client client-wire 'local local-wire))
      'identical)
(want "C8 and the daemon served it: one more dispatch than before the client's call"
      (- served-after-client served-before-client)
      1)
(want "C8 the MCP shell's text decodes to the same datum"
      (if mcp-text (equal? (first-datum mcp-text) (first-datum local-wire)) (list 'NO-TEXT mcp-out))
      #t)

(system (string-append "pkill -f 'serve " store "' 2>/dev/null"))

;; ---- C7: nothing was written ---------------------------------------------------------
;;
;; Every query above -- in this process with and without the published
;; reduction, and through core.sc, the daemon and the MCP shell -- ran between
;; the two readings. A few more in-process ones, on the published reduction,
;; close the bracket.
(for-each (lambda (args) (rpc-dispatch store (cons 'commitments args) "test" published))
          '(() ("--all") ("--all" "--drifted") ("--since" "()") ("--under" "root")))
(define pins-after (list (log-bytes) (file-list) (state-datum published)))
(want "C7 the log's length, the store's files and the published reduction are what they were"
      (map equal? pins-before pins-after)
      '(#t #t #t))

(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\ncommitments-verb complete\n" bad rows)
(exit (if (= bad 0) 0 1))
