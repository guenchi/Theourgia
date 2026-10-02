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

;; The model of attest.sc and lifecycle.sc, on reductions built from events
;; with explicit pasts: what changed, what was said, which end of an edge
;; moved, validity, and a decision's state. Two writers are concurrent here
;; exactly when neither's event names the other's in its premises; a
;; writer's own earlier events are always in the past of its later ones.
;;
;; An event is (<writer> <seq> <premises> <payload>); a put by writer w at
;; seq n makes the block "w.n".
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia commitments) commitments-answer)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read effect-relation-names known-classes)
        (prefix (theourgia attest) attest:)
        (prefix (theourgia lifecycle) lc:))

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

(register-verbs! extension-verbs)

;; ---- reductions -------------------------------------------------------------------------

(define (state-of . events)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e)) events)
    r))
(define (section title) `(put ((kind . section) (title . ,title))))
(define (decision title) `(put ((kind . decision) (title . ,title))))
(define (task title status) `(put ((kind . task) (title . ,title) (status . ,status))))
(define (code title) `(put ((kind . code) (title . ,title))))
(define (classed title class) `(put ((kind . section) (title . ,title) (class . ,class))))

;; A cut is an alist whose order carries nothing; rows compare it sorted.
(define (sorted-cut c) (list-sort (lambda (x y) (string<? (car x) (car y))) c))
(define (A s) (attest:make-attestation s))
(define (watch s a rel b) (attest:edge-watch (A s) a rel b))
(define (validity s id) (lc:validity-of (lc:lifecycle s) id))
(define (state-of-decision s id) (lc:decision-state-of (lc:lifecycle s) id))

;; ---- B1: changed --------------------------------------------------------------------------

(define b1 '(("a" 1 () (put ((kind . section) (title . "x"))))))
(define (b1+ . more) (apply state-of (append b1 more)))
(want "B1 a put's own fields are its content: changed is the put's cut"
      (attest:changed (A (b1+)) "a.1")
      '(("a" . 1)))
(want "B1 a title write moves changed to its cut"
      (attest:changed (A (b1+ '("a" 2 () (set "a.1" title "y")))) "a.1")
      '(("a" . 2)))
(want "B1 a status, batch, keywords, class or slug write does not change it"
      (map (lambda (f) (attest:changed (A (b1+ `("a" 2 () (set "a.1" ,f "v")))) "a.1"))
           '(status batch keywords class slug))
      '((("a" . 1)) (("a" . 1)) (("a" . 1)) (("a" . 1)) (("a" . 1))))
(want "B1 a field nobody classified is content: its write moves changed"
      (attest:changed (A (b1+ '("a" 2 () (set "a.1" zzz "v")))) "a.1")
      '(("a" . 2)))
(want "B1 an absence candidate (a field made absent) moves changed"
      (attest:changed (A (b1+ '("a" 2 () (set "a.1" title)))) "a.1")
      '(("a" . 2)))
;; NO CREATING-PUT TERM: a put whose fields are all bookkeeping leaves no
;; content, and changed is empty; a rule that joined the put's own cut in
;; would answer (("a" . 1)).
(want "B1 a put of bookkeeping fields only: changed is empty, the put itself is not a term"
      (attest:changed (A (state-of '("a" 1 () (put ((status . done)))))) "a.1")
      '())
(want "B1 a block with no creating put answers the join of its field events"
      (attest:changed (A (state-of '("q" 1 () (set "q.7" title "t")) '("r" 1 () (set "q.7" src "s")))) "q.7")
      '(("q" . 1) ("r" . 1)))

;; ---- B2: said -----------------------------------------------------------------------------

(define b2 '(("a" 1 () (put ((kind . section) (title . "x"))))
             ("b" 1 () (put ((kind . section) (title . "y"))))))
(define (b2+ . more) (apply state-of (append b2 more)))
(want "B2 one link: said is its cut"
      (sorted-cut (attest:said (A (b2+ '("a" 2 (("b" . 1)) (link "a.1" cites "b.1")))) "a.1" 'cites "b.1"))
      '(("a" . 2) ("b" . 1)))
(want "B2 two concurrent links: said is their join"
      (sorted-cut (attest:said (A (b2+ '("a" 2 (("b" . 1)) (link "a.1" cites "b.1"))
                           '("c" 1 (("a" . 1) ("b" . 1)) (link "a.1" cites "b.1"))))
                   "a.1" 'cites "b.1"))
      '(("a" . 2) ("b" . 1) ("c" . 1)))
(want "B2 an unlink that had seen one of them leaves the other's cut"
      (sorted-cut (attest:said (A (b2+ '("a" 2 (("b" . 1)) (link "a.1" cites "b.1"))
                           '("c" 1 (("a" . 1) ("b" . 1)) (link "a.1" cites "b.1"))
                           '("b" 2 (("a" . 2)) (unlink "a.1" cites "b.1"))))
                   "a.1" 'cites "b.1"))
      '(("a" . 1) ("b" . 1) ("c" . 1)))
(want "B2 a second link by the same writer moves said to the later cut"
      (sorted-cut (attest:said (A (b2+ '("a" 2 (("b" . 1)) (link "a.1" cites "b.1"))
                           '("b" 2 () (set "b.1" title "y2"))
                           '("a" 3 (("b" . 2)) (link "a.1" cites "b.1"))))
                   "a.1" 'cites "b.1"))
      '(("a" . 3) ("b" . 2)))

;; ---- B3: which end moved -------------------------------------------------------------------

;; x = a.1 (source), y = a.2 (target), both by writer a.
(define b3 '(("a" 1 () (put ((kind . section) (title . "x"))))
             ("a" 2 () (put ((kind . section) (title . "y"))))))
(define (b3+ . more) (apply state-of (append b3 more)))
(want "B3 an edit in the link's past: neither end moved"
      (watch (b3+ '("a" 3 () (set "a.1" title "x2")) '("a" 4 () (link "a.1" cites "a.2"))) "a.1" 'cites "a.2")
      '(current))
(want "B3 the target edited by a writer who had seen the link: target"
      (watch (b3+ '("a" 3 () (link "a.1" cites "a.2")) '("a" 4 () (set "a.2" title "y2"))) "a.1" 'cites "a.2")
      '(moved target))
(want "B3 the source edited by a writer who had seen the link: source"
      (watch (b3+ '("a" 3 () (link "a.1" cites "a.2")) '("a" 4 () (set "a.1" title "x2"))) "a.1" 'cites "a.2")
      '(moved source))
(want "B3 the target, then the source by a writer who had seen the target's edit: source only"
      (watch (b3+ '("a" 3 () (link "a.1" cites "a.2")) '("a" 4 () (set "a.2" title "y2"))
                  '("a" 5 () (set "a.1" title "x2")))
             "a.1" 'cites "a.2")
      '(moved source))
;; THE CONCURRENT LINK-AND-EDIT SEQUENCE: A links S to T, B edits T
;; without having seen the link, A edits S without having seen B's edit.
(define concurrent-link-and-edit (list '("a" 3 () (link "a.1" cites "a.2"))
                     '("b" 1 (("a" . 2)) (set "a.2" title "y-b"))
                     '("a" 4 () (set "a.1" title "x-a"))))
(want "B3 the concurrent link-and-edit sequence: BOTH ends moved"
      (watch (apply b3+ concurrent-link-and-edit) "a.1" 'cites "a.2")
      '(moved source target))
(want "B3 an edit concurrent with the link, which the linker had not seen: moved"
      (watch (b3+ '("b" 1 (("a" . 2)) (set "a.2" title "y-b")) '("a" 3 () (link "a.1" cites "a.2")))
             "a.1" 'cites "a.2")
      '(moved target))
(want "B3 a link again by a writer who had seen both edits: neither"
      (watch (apply b3+ (append concurrent-link-and-edit (list '("c" 1 (("a" . 4) ("b" . 1)) (link "a.1" cites "a.2")))))
             "a.1" 'cites "a.2")
      '(current))
(want "B3 a link again by a writer who had seen only one: the other is still moved"
      (watch (apply b3+ (append concurrent-link-and-edit (list '("c" 1 (("a" . 4)) (link "a.1" cites "a.2")))))
             "a.1" 'cites "a.2")
      '(moved target))

;; ---- B3b: one witness at a time -------------------------------------------------------------

;; Two writers each edit one end, having seen the puts, and each links; the
;; links' join covers both edits, no single witness does.
(define (two-writers end f1 f2)
  (b3+ `("b" 1 (("a" . 2)) (set ,end ,f1 "by-b"))
       '("b" 2 () (link "a.1" cites "a.2"))
       `("c" 1 (("a" . 2)) (set ,end ,f2 "by-c"))
       '("c" 2 () (link "a.1" cites "a.2"))))
(want "B3b two concurrent title writes of the target, each writer linking: target moved although the links' join covers both"
      (watch (two-writers "a.2" 'title 'title) "a.1" 'cites "a.2")
      '(moved target))
(want "B3b the title-and-text-by-two-writers sequence, each linking: target moved"
      (watch (two-writers "a.2" 'title 'src) "a.1" 'cites "a.2")
      '(moved target))
(want "B3b then one more link by a writer who has seen both: neither"
      (watch (state-of (car b3) (cadr b3)
                       '("b" 1 (("a" . 2)) (set "a.2" title "by-b")) '("b" 2 () (link "a.1" cites "a.2"))
                       '("c" 1 (("a" . 2)) (set "a.2" src "by-c")) '("c" 2 () (link "a.1" cites "a.2"))
                       '("d" 1 (("b" . 2) ("c" . 2)) (link "a.1" cites "a.2")))
             "a.1" 'cites "a.2")
      '(current))
(want "B3b the ends exchanged: two concurrent title writes of the source, each linking: source moved"
      (watch (two-writers "a.1" 'title 'title) "a.1" 'cites "a.2")
      '(moved source))
(want "B3b the ends exchanged: the title-and-text-by-two-writers sequence on the source: source moved"
      (watch (two-writers "a.1" 'title 'src) "a.1" 'cites "a.2")
      '(moved source))
(want "B3b the ends exchanged: one more link by a writer who has seen both: neither"
      (watch (state-of (car b3) (cadr b3)
                       '("b" 1 (("a" . 2)) (set "a.1" title "by-b")) '("b" 2 () (link "a.1" cites "a.2"))
                       '("c" 1 (("a" . 2)) (set "a.1" src "by-c")) '("c" 2 () (link "a.1" cites "a.2"))
                       '("d" 1 (("b" . 2) ("c" . 2)) (link "a.1" cites "a.2")))
             "a.1" 'cites "a.2")
      '(current))

;; ---- B3c: an edge from a block to itself ------------------------------------------------------

(want "B3c a self edge answers self"
      (watch (b3+ '("a" 3 () (link "a.1" cites "a.1"))) "a.1" 'cites "a.1")
      '(self))
(want "B3c x supersedes x leaves x valid; x refutes x leaves x valid"
      (in-order (validity (b3+ '("a" 3 () (link "a.1" supersedes "a.1"))) "a.1")
                (validity (b3+ '("a" 3 () (link "a.1" refutes "a.1"))) "a.1"))
      '((valid) (valid)))
(define self-d (state-of '("a" 1 () (put ((kind . decision) (title . "D"))))
                         '("a" 2 () (link "a.1" implements "a.1"))
                         '("a" 3 () (set "a.1" title "D edited after"))))
(want "B3c a decision that implements itself is discharged, and its state is implemented, never review"
      (in-order (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) 'decision)))
                                  (cdr (cadr (commitments-answer self-d 'open #f #f #f)))))
                (state-of-decision self-d "a.1")
                (validity self-d "a.1"))
      '(() (implemented) (valid)))

;; ---- B4: gone ------------------------------------------------------------------------------------

(want "B4 an edge to a deleted block, from a deleted block, to an unknown id: gone"
      (in-order (watch (b3+ '("a" 3 () (link "a.1" cites "a.2")) '("a" 4 () (del "a.2"))) "a.1" 'cites "a.2")
                (watch (b3+ '("a" 3 () (link "a.1" cites "a.2")) '("a" 4 () (del "a.1"))) "a.1" 'cites "a.2")
                (watch (b3+ '("a" 3 () (link "a.1" cites "zz.9"))) "a.1" 'cites "zz.9"))
      '((gone target) (gone source) (gone target)))
(want "B4 no effect from a deleted source: a deleted block that supersedes or refutes leaves its target valid"
      (in-order (validity (b3+ '("a" 3 () (link "a.1" supersedes "a.2")) '("a" 4 () (del "a.1"))) "a.2")
                (validity (b3+ '("a" 3 () (link "a.1" refutes "a.2")) '("a" 4 () (del "a.1"))) "a.2"))
      '((valid) (valid)))
;; A TARGET THAT IS GONE TAKES NO EFFECT, ruling or proposal: a dependent of
;; a deleted premise that something superseded, refuted or proposed against
;; answers premise-gone and nothing else; a gone subject is verified by
;; nothing.
(want "B4 a deleted premise superseded or refuted: its dependent answers premise-gone only"
      (map (lambda (effect)
             (validity (b3+ '("a" 3 () (put ((kind . section) (title . "w"))))
                            '("a" 4 () (link "a.3" depends-on "a.2"))
                            effect
                            '("a" 6 () (del "a.2")))
                       "a.3"))
           (list '("a" 5 () (link "a.1" supersedes "a.2"))
                 '("a" 5 () (link "a.1" refutes "a.2"))))
      '((needs-review (premise-gone "a.2")) (needs-review (premise-gone "a.2"))))
(want "B4 an inference that refutes a deleted premise: the dependent answers premise-gone only"
      (validity (b3+ '("a" 3 () (put ((kind . section) (title . "w"))))
                     '("a" 4 () (link "a.3" depends-on "a.2"))
                     '("a" 5 () (set "a.1" class inference))
                     '("a" 6 () (link "a.1" refutes "a.2"))
                     '("a" 7 () (del "a.2")))
                "a.3")
      '(needs-review (premise-gone "a.2")))
(want "B4 a deleted subject, and an unknown one, are verified by nothing"
      (in-order (lc:verified-by (lc:lifecycle (b3+ '("a" 3 () (link "a.1" verifies "a.2")) '("a" 4 () (del "a.2")))) "a.2")
                (lc:verified-by (lc:lifecycle (b3+ '("a" 3 () (link "a.1" verifies "zz.9")))) "zz.9"))
      '(() ()))
(want "B4 a premise deleted, or unknown: premise-gone"
      (in-order (validity (b3+ '("a" 3 () (link "a.1" depends-on "a.2")) '("a" 4 () (del "a.2"))) "a.1")
                (validity (b3+ '("a" 3 () (link "a.1" depends-on "zz.9"))) "a.1"))
      '((needs-review (premise-gone "a.2")) (needs-review (premise-gone "zz.9"))))

;; ---- B7: validity -----------------------------------------------------------------------------------

;; a.1 .. a.4: four sections; e = "e.1", an evidence block by writer e.
(define b7 '(("a" 1 () (put ((kind . section) (title . "one"))))
             ("a" 2 () (put ((kind . section) (title . "two"))))
             ("a" 3 () (put ((kind . section) (title . "three"))))
             ("a" 4 () (put ((kind . section) (title . "four"))))))
(define (b7+ . more) (apply state-of (append b7 more)))
(want "B7 superseded: a live authoritative block supersedes it"
      (validity (b7+ '("a" 5 () (link "a.1" supersedes "a.2"))) "a.2")
      '(superseded (superseded-by "a.1")))
(want "B7 refuted: a live authoritative block refutes it and the edge is current"
      (validity (b7+ '("a" 5 () (link "a.1" refutes "a.2"))) "a.2")
      '(refuted (refuted-by "a.1")))
(want "B7 a refutes edge whose claim moved past it: needs review, refutation-moved"
      (validity (b7+ '("a" 5 () (link "a.1" refutes "a.2")) '("a" 6 () (set "a.2" title "two, revised"))) "a.2")
      '(needs-review (refutation-moved "a.1")))
(want "B7 a proposal from an inference source does not supersede: needs review, proposed-supersedes"
      (validity (b7+ '("a" 5 () (set "a.1" class inference)) '("a" 6 () (link "a.1" supersedes "a.2"))) "a.2")
      '(needs-review (proposed-supersedes "a.1")))
(want "B7 proposals from an external source: proposed-refutes, proposed-conflicts-with"
      (in-order (validity (b7+ '("a" 5 () (set "a.1" class external)) '("a" 6 () (link "a.1" refutes "a.2"))) "a.2")
                (validity (b7+ '("a" 5 () (set "a.1" class external)) '("a" 6 () (link "a.1" conflicts-with "a.2"))) "a.2"))
      '((needs-review (proposed-refutes "a.1")) (needs-review (proposed-conflicts-with "a.1"))))
(want "B7 conflicts-with between two authoritative blocks has no effect here"
      (validity (b7+ '("a" 5 () (link "a.1" conflicts-with "a.2"))) "a.2")
      '(valid))
(want "B7 depends-on a premise superseded, refuted, needing review, moved"
      (in-order (validity (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (link "a.3" supersedes "a.2"))) "a.1")
                (validity (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (link "a.3" refutes "a.2"))) "a.1")
                (validity (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (link "a.2" depends-on "zz.9"))) "a.1")
                (validity (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (set "a.2" title "two, revised"))) "a.1"))
      '((needs-review (premise-superseded "a.2"))
        (needs-review (premise-refuted "a.2"))
        (needs-review (premise-needs-review "a.2"))
        (needs-review (premise-moved "a.2"))))
(want "B7 implements: the implementation changed after it was said to implement: implementation-moved"
      (validity (b7+ '("a" 5 () (link "a.1" implements "a.2")) '("a" 6 () (set "a.1" title "one, revised"))) "a.1")
      '(needs-review (implementation-moved "a.2")))
(want "B7 implements: the decision moved, superseded, gone: premise reasons"
      (in-order (validity (b7+ '("a" 5 () (link "a.1" implements "a.2")) '("a" 6 () (set "a.2" title "two, revised"))) "a.1")
                (validity (b7+ '("a" 5 () (link "a.1" implements "a.2")) '("a" 6 () (link "a.3" supersedes "a.2"))) "a.1")
                (validity (b7+ '("a" 5 () (link "a.1" implements "a.2")) '("a" 6 () (del "a.2"))) "a.1"))
      '((needs-review (premise-moved "a.2"))
        (needs-review (premise-superseded "a.2"))
        (needs-review (premise-gone "a.2"))))
(want "B7 precedence: superseded over refuted over needs-review, with that validity's reasons only"
      (in-order (validity (b7+ '("a" 5 () (link "a.1" supersedes "a.4")) '("a" 6 () (link "a.2" refutes "a.4"))
                               '("a" 7 () (link "a.4" depends-on "zz.9")))
                          "a.4")
                (validity (b7+ '("a" 5 () (link "a.2" refutes "a.4")) '("a" 6 () (link "a.4" depends-on "zz.9"))) "a.4"))
      '((superseded (superseded-by "a.1")) (refuted (refuted-by "a.2"))))
(want "B7 every reason is listed, in a fixed order"
      (validity (b7+ '("a" 5 () (link "a.4" depends-on "zz.9")) '("a" 6 () (link "a.4" depends-on "a.2"))
                     '("a" 7 () (set "a.1" class inference)) '("a" 8 () (link "a.1" supersedes "a.4"))
                     '("a" 9 () (link "a.3" supersedes "a.2")))
                "a.4")
      '(needs-review (proposed-supersedes "a.1") (premise-gone "zz.9") (premise-superseded "a.2")))
;; A cycle: p = a.1 depends on q = a.2, q on r = a.3, r on p; e = a.4 refutes q.
(define cycle (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (link "a.2" depends-on "a.3"))
                   '("a" 7 () (link "a.3" depends-on "a.1")) '("a" 8 () (link "a.4" refutes "a.2"))))
(want "B7 a depends-on cycle with one refuted member terminates and marks every member"
      (map (lambda (id) (validity cycle id)) '("a.1" "a.2" "a.3"))
      '((needs-review (premise-refuted "a.2"))
        (refuted (refuted-by "a.4"))
        (needs-review (premise-needs-review "a.1"))))
(want "B7 a three-step chain propagates: one depends on two, two on three, three superseded"
      (let ((s (b7+ '("a" 5 () (link "a.1" depends-on "a.2")) '("a" 6 () (link "a.2" depends-on "a.3"))
                    '("a" 7 () (link "a.4" supersedes "a.3")))))
        (map (lambda (id) (validity s id)) '("a.1" "a.2" "a.3")))
      '((needs-review (premise-needs-review "a.2"))
        (needs-review (premise-superseded "a.3"))
        (superseded (superseded-by "a.4"))))
(define mutual (b7+ '("a" 5 () (link "a.1" supersedes "a.2")) '("a" 6 () (link "a.2" supersedes "a.1"))))
(define (all-validities s order)
  (let ((L (lc:lifecycle s order)))
    (map (lambda (id) (lc:validity-of L id)) '("a.1" "a.2" "a.3" "a.4"))))
(define (rotate l) (if (null? l) l (append (cdr l) (list (car l)))))
(want "B7 A supersedes B and B supersedes A: both superseded, the same on every order of visit"
      (let ((orders (list (lambda (es) es) reverse rotate (lambda (es) (rotate (rotate es))))))
        (in-order (all-validities mutual (car orders))
                  (map (lambda (o) (equal? (all-validities mutual o) (all-validities mutual (car orders)))) orders)
                  (map (lambda (o) (equal? (all-validities cycle o) (all-validities cycle (car orders)))) orders)))
      '(((superseded (superseded-by "a.2")) (superseded (superseded-by "a.1")) (valid) (valid))
        (#t #t #t #t)
        (#t #t #t #t)))

;; ---- C49-1b: the two lists of effect relations ------------------------------------------------------

(define (symbol-set l) (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) l))
(want "C49-1b the reducer's effect-relation names are the lifecycle table's names"
      (in-order (symbol-set effect-relation-names) (equal? (symbol-set effect-relation-names) (symbol-set lc:effect-relations)))
      '((conflicts-with depends-on implements refutes supersedes verifies) #t))

;; ---- C49-2: the effective class -----------------------------------------------------------------------

(define (effective s id) (lc:effective-class (state-read s id)))
(want "C49-2 absent on a decision reads ruling, absent elsewhere observation; the string \"ruling\" reads ruling"
      (let ((s (state-of '("a" 1 () (put ((kind . decision) (title . "d"))))
                         '("a" 2 () (put ((kind . section) (title . "s"))))
                         '("a" 3 () (put ((kind . section) (title . "r") (class . "ruling")))))))
        (map (lambda (id) (effective s id)) '("a.1" "a.2" "a.3")))
      '(ruling observation ruling))
(define conflicted (state-of '("a" 1 () (put ((kind . section) (title . "c"))))
                             '("a" 2 () (put ((kind . section) (title . "target"))))
                             '("b" 1 (("a" . 2)) (set "a.1" class inference))
                             '("c" 1 (("a" . 2)) (set "a.1" class ruling))
                             '("c" 2 (("b" . 1)) (link "a.1" supersedes "a.2"))))
(want "C49-2 two concurrent class writes read conflict and are not authoritative; a proposal from such a block does not supersede"
      (in-order (effective conflicted "a.1")
                (lc:authoritative? (effective conflicted "a.1"))
                (validity conflicted "a.2"))
      '(conflict #f (needs-review (proposed-supersedes "a.1"))))
(want "C49-2 a record with a class outside the list still applies, and its effective class is unreadable"
      (let ((s (state-of '("a" 1 () (put ((kind . section) (title . "n") (class . nonsense)))))))
        (in-order (and (state-read s "a.1") #t) (effective s "a.1") (lc:authoritative? (effective s "a.1"))))
      '(#t unreadable #f))
(want "C49-2 the known classes"
      known-classes
      '(observation inference ruling verification external))

;; ---- B8 and C49-5b: a decision's state -------------------------------------------------------------------

;; d = a.1 (decision), i = a.2 (code), v = a.3 (a check).
(define b8 '(("a" 1 () (put ((kind . decision) (title . "D"))))
             ("a" 2 () (put ((kind . code) (title . "impl"))))
             ("a" 3 () (put ((kind . section) (title . "check"))))))
(define (b8+ . more) (apply state-of (append b8 more)))
(want "B8 open: no implementer; implemented: a code block implements it"
      (in-order (state-of-decision (b8+) "a.1")
                (state-of-decision (b8+ '("a" 4 () (link "a.2" implements "a.1"))) "a.1"))
      '((open) (implemented)))
(want "B8 review by each end: the implementation edited, the decision edited"
      (in-order (state-of-decision (b8+ '("a" 4 () (link "a.2" implements "a.1")) '("a" 5 () (set "a.2" title "impl 2"))) "a.1")
                (state-of-decision (b8+ '("a" 4 () (link "a.2" implements "a.1")) '("a" 5 () (set "a.1" title "D 2"))) "a.1"))
      '((review ("a.2" source)) (review ("a.2" target))))
(want "B8 closed; a done decision with no implementer is closed, not open; one with a moved pair is closed, not review"
      (in-order (state-of-decision (b8+ '("a" 4 () (link "a.2" implements "a.1")) '("a" 5 () (set "a.1" status "done"))) "a.1")
                (state-of-decision (b8+ '("a" 4 () (set "a.1" status dropped))) "a.1")
                (state-of-decision (b8+ '("a" 4 () (link "a.2" implements "a.1")) '("a" 5 () (set "a.2" title "impl 2"))
                                        '("a" 6 () (set "a.1" status "done")))
                                   "a.1"))
      '((closed done) (closed dropped) (closed done)))
(want "B8 a task discharges only once it is done"
      (in-order (state-of-decision (b8+ '("a" 4 () (put ((kind . task) (title . "t") (status . "todo"))))
                                        '("a" 5 () (link "a.4" implements "a.1")))
                                   "a.1")
                (state-of-decision (b8+ '("a" 4 () (put ((kind . task) (title . "t") (status . "todo"))))
                                        '("a" 5 () (link "a.4" implements "a.1")) '("a" 6 () (set "a.4" status "done")))
                                   "a.1"))
      '((open) (implemented)))
;; C49-5b: the five, in order, on one fixture.
(define five (list '("a" 4 () (link "a.2" implements "a.1"))
                   '("a" 5 () (link "a.3" verifies "a.1"))
                   '("a" 6 () (set "a.1" src "the decision's text, edited"))
                   '("a" 7 () (link "a.2" implements "a.1"))
                   '("a" 8 () (link "a.3" verifies "a.1"))))
(define (prefix-state n) (apply b8+ (list-head five n)))
(want "C49-5b open, implemented, verified, review once the decision's text is edited, implemented again once relinked (the check out of date), verified again"
      (map (lambda (n) (state-of-decision (prefix-state n) "a.1")) '(0 1 2 3 4 5))
      '((open) (implemented) (verified "a.3") (review ("a.2" target)) (implemented) (verified "a.3")))
(want "C49-5b verified-by says current, then moved after the edit, then current again"
      (map (lambda (n) (lc:verified-by (lc:lifecycle (prefix-state n)) "a.1")) '(2 3 4 5))
      '((("a.3" current)) (("a.3" moved)) (("a.3" moved)) (("a.3" current))))
;; A MOVED PAIR WITH A CURRENT CHECK: the decision edited, the check linked
;; again after the edit, the implementation not. review comes before
;; verified; and done closes it whatever the pair.
(define moved-pair-current-check
  (append (list-head five 3) (list '("a" 7 () (link "a.3" verifies "a.1")))))
(want "C49-5b a moved pair with a current check is review, not verified; the check is current"
      (let ((s (apply b8+ moved-pair-current-check)))
        (in-order (state-of-decision s "a.1") (lc:verified-by (lc:lifecycle s) "a.1")))
      '((review ("a.2" target)) (("a.3" current))))
(want "C49-5b and set done, the same decision is closed"
      (state-of-decision (apply b8+ (append moved-pair-current-check (list '("a" 8 () (set "a.1" status "done"))))) "a.1")
      '(closed done))

;; B8: commitments reads the provider. Each decision is listed by --open
;; exactly when the provider says open, and its drifted implementers are
;; exactly those whose own content moved past every witness.
(define (open-ids s) (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) 'decision)))
                                       (cdr (cadr (commitments-answer s 'open #f #f #f))))))
(define (drifted-of s id)
  (let ((row (find (lambda (x) (and (pair? x) (eq? (car x) 'decision) (equal? (cadr x) id)))
                   (cdr (cadr (commitments-answer s 'all #f #f #f))))))
    (let ((d (and row (assq 'drifted (cddr row))))) (if d (map car (cdr d)) '()))))
(define with-task (b8+ '("a" 4 () (put ((kind . task) (title . "t") (status . "todo"))))
                       '("a" 5 () (link "a.4" implements "a.1"))))
(want "B8 commitments lists a decision as open exactly when the provider's state is open (a todo task does not discharge)"
      (in-order (open-ids with-task) (state-of-decision with-task "a.1"))
      '(("a.1") (open)))
;; Two writers each edit a part of the implementation and each link: the
;; links' join covers both edits and no single witness does.
(define split-witness
  (b8+ '("b" 1 (("a" . 3)) (set "a.2" title "by-b")) '("b" 2 () (link "a.2" implements "a.1"))
       '("c" 1 (("a" . 3)) (set "a.2" src "by-c")) '("c" 2 () (link "a.2" implements "a.1"))))
(want "B8 commitments' drifted is the provider's source-moved: one witness at a time"
      (in-order (drifted-of split-witness "a.1")
                (state-of-decision split-witness "a.1")
                (validity split-witness "a.2"))
      '(("a.2") (review ("a.2" source)) (needs-review (implementation-moved "a.1"))))

;; B8: ONE DEFINITION OF THE RULES. commitments.sc, read as data, imports the
;; provider's decision-state-of and implementation-of, defines none of the
;; names the rule had before it moved (edge-discharges? frontier
;; status-discharges? deleted?) and refers to none of what such a rule
;; reads (state-link-events state-field-events cut-join task-statuses
;; bookkeeping-fields).
(define (forms-of path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (symbols-in x)
  (cond ((symbol? x) (list x)) ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x)))) (else '())))
(define commitments-library (car (filter (lambda (f) (and (pair? f) (eq? (car f) 'library))) (forms-of "../commitments.sc"))))
(define (defined-names form)
  (cond ((and (pair? form) (eq? (car form) 'define) (pair? (cdr form)))
         (list (if (pair? (cadr form)) (car (cadr form)) (cadr form))))
        ((pair? form) (apply append (map defined-names (filter pair? form))))
        (else '())))
(want "B8 commitments imports the provider's state and implementation, and defines and reads no rule of its own"
      (let* ((imports (cdr (find (lambda (x) (and (pair? x) (eq? (car x) 'import))) commitments-library)))
             (lc (find (lambda (i) (and (pair? i) (eq? (car i) 'only) (equal? (cadr i) '(theourgia lifecycle)))) imports))
             (syms (symbols-in commitments-library))
             (defs (defined-names commitments-library)))
        (in-order (and lc (memq 'decision-state-of lc) (memq 'implementation-of lc) #t)
                  (filter (lambda (n) (memq n defs)) '(edge-discharges? frontier status-discharges? deleted?))
                  (filter (lambda (n) (memq n syms)) '(state-link-events state-field-events cut-join task-statuses bookkeeping-fields))))
      '(#t () ()))

(printf "rows: ~a\n~a failures\nattest-model complete\n" rows bad)
(exit (if (= bad 0) 0 1))
