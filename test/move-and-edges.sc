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

;; A move one writer makes under a block's own descendant, and what link
;; and unlink found.
;;
;; MOVE: a move whose parent is the block or one of its descendants, in the
;; reduction the write is checked against, is refused would-cycle with the
;; chain, and nothing is written. The walk up the parents stops, without a
;; refusal, at root, at a position in conflict, at a block the store does
;; not hold, and at a block already visited; a deleted ancestor is a step.
;; A cycle two writers make together is a conflict after the merge, not a
;; refusal. The other writer's records arrive through publish, as a
;; mirror's do, with the dependencies they declare.
;; EDGES: link and unlink answer `(matched 0|1)`, the logical edges they
;; found before they were applied.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) block-id reduce-applied-cut state-read state-structure)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia wire) encode-record storable-encode))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/move-and-edges-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'move-and-edges "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define n 0)
(define (ask store . req) (rpc-dispatch store req "test"))
(define (new-id answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (clause-of a head)
  (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))
;; -> (store writer)
(define (fresh!)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")))
    (system (string-append "mkdir -p " st " " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((init (ask st 'init)))
      (list st (cadr (assq 'writer (cdr init)))))))
(define (insert! st under title) (new-id (ask st 'insert "--under" under "--title" title)))
(define (cut-of st) (reduce-applied-cut (open-and-reduce st)))
(define (seq-of st w) (let ((p (assoc w (cut-of st)))) (if p (cdr p) 0)))
(define (conflict-ids st) (cdr (assq 'conflicts (state-structure (open-and-reduce st)))))
;; The other writer, as a mirror: its records published one segment each,
;; with the dependencies they declare, the payload storage-encoded before it
;; is framed, as the writer does.
(define M "mirrorbb")
(define (publish! st seg seq deps payload)
  (let ((bytes (encode-record seq (+ 1757300000000 seq) "peer" deps (storable-encode payload))))
    (log-publish! st M seg bytes (segment-sha bytes))))
(define (would-cycle? a) (and (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (eq? (cadr a) 'would-cycle)))

;; =============================================================================
(printf "== MOVE: a block under itself or its own descendant ==\n")

;; W1: A > B > C, one writer, move A under C.
(let* ((c (fresh!)) (st (car c))
       (A (insert! st "root" "A")) (B (insert! st A "B")) (C (insert! st B "C"))
       (before (cut-of st))
       (a (ask st 'move A C)))
  (want "W1 moving A under its own descendant C is refused would-cycle with the chain (C B A), nothing written, no conflict"
        (list a (equal? before (cut-of st)) (conflict-ids st))
        (list (list 'error 'would-cycle (list 'id A) (list 'parent C) (list 'through (list C B A))) #t '())))

;; W2: a block under itself.
(let* ((c (fresh!)) (st (car c)) (A (insert! st "root" "A")))
  (want "W2 moving A under A is refused would-cycle with the one-element chain"
        (ask st 'move A A)
        (list 'error 'would-cycle (list 'id A) (list 'parent A) (list 'through (list A)))))

;; W1c: a deleted ancestor is a step, not a stop.
(let* ((c (fresh!)) (st (car c))
       (A (insert! st "root" "A")) (B (insert! st A "B")) (C (insert! st B "C")))
  (ask st 'del B)
  (want "W1c with B deleted, moving A under C is still refused, through (C B A)"
        (ask st 'move A C)
        (list 'error 'would-cycle (list 'id A) (list 'parent C) (list 'through (list C B A)))))

;; W2b (i) PIN: D's position is in conflict -- one candidate under P, one
;; under Q, both below X -- so the walk up from D has no settled parent and
;; stops; moving X under D is not refused would-cycle.
(let* ((c (fresh!)) (st (car c)) (w (cadr c))
       (X (insert! st "root" "X")) (P (insert! st X "P")) (Q (insert! st X "Q")) (D (insert! st "root" "D"))
       (seen (seq-of st w)))
  (ask st 'move D P)
  (publish! st 1 1 (list (cons w seen)) (list 'move D Q 1))
  (let ((pos (cdr (assq 'position (state-read (open-and-reduce st) D)))))
    (want "W2b (i) PIN D's position is in conflict (two candidates), and moving X under D is not refused would-cycle"
          (list (and (pair? pos) (car pos)) (would-cycle? (ask st 'move X D)))
          '(conflict #f))))

;; W2b (ii) PIN: P's parent is a block this store never received; the walk
;; stops there and the move answers as it does without the rule: ok.
(let* ((c (fresh!)) (st (car c)) (w (cadr c)) (X (insert! st "root" "X")))
  (publish! st 1 1 '() '(put ((kind . section) (title . "P"))))
  (publish! st 2 2 (list (cons M 1)) (list 'move (block-id M 1) "zzzzzzzz.9" 1))
  (want "W2b (ii) PIN a parent chain that leaves the store stops the walk: moving X under P answers ok, as without the rule"
        (head-of (ask st 'move X (block-id M 1)))
        'ok))

;; W2b (iii) PIN: U and V already form a cycle (two writers, W3's shape);
;; a move whose walk enters it stops at the repeat.
(let* ((c (fresh!)) (st (car c)) (w (cadr c))
       (U (insert! st "root" "U")) (V (insert! st "root" "V")) (X (insert! st "root" "X"))
       (seen (seq-of st w)))
  (ask st 'move U V)
  (publish! st 1 1 (list (cons w seen)) (list 'move V U 1))
  (want "W2b (iii) PIN with U and V already in a cycle, moving X under U is not refused would-cycle"
        (list (and (member U (conflict-ids st)) #t) (would-cycle? (ask st 'move X U)))
        '(#t #f)))

;; W3 PIN: the designed multi-writer cycle. A and B at root; the local
;; writer moves A under B; the other writer, not having seen that move
;; (its dependency cut stops before it), moves B under A. Neither is
;; refused; after the merge conflicts lists the cycle.
(let* ((c (fresh!)) (st (car c)) (w (cadr c))
       (A (insert! st "root" "A")) (B (insert! st "root" "B"))
       (seen (seq-of st w))
       (local (ask st 'move A B))
       (move-seq (seq-of st w))
       (published (caught (publish! st 1 1 (list (cons w seen)) (list 'move B A 1)))))
  (want "W3 PIN independent moves (the other writer's cut stops before the local move) are both accepted and merge into a cycle"
        (list (head-of local) (< seen move-seq) (head-of published) (list-sort string<? (conflict-ids st)))
        (list 'ok #t 'published (list-sort string<? (list A B)))))

;; =============================================================================
(printf "== EDGES: what link and unlink found ==\n")

(define (edges-of st id) (cdr (assq 'edges (state-read (open-and-reduce st) id))))
;; W4 (i): the edge is there; unlink finds it.
(let* ((c (fresh!)) (st (car c)) (A (insert! st "root" "A")) (B (insert! st "root" "B")))
  (ask st 'link A "cites" B)
  (let ((a (ask st 'unlink A "cites" B)))
    (want "W4 (i) unlink of an edge that is there answers (matched 1), and the edge is gone"
          (list (head-of a) (clause-of a 'matched) (edges-of st A))
          '(ok (matched 1) ()))))
;; W4 (ii): the positionals swapped; unlink finds nothing, the edge stays,
;; and the record is still written (the set semantics).
(let* ((c (fresh!)) (st (car c)) (A (insert! st "root" "A")) (B (insert! st "root" "B")))
  (ask st 'link A "cites" B)
  (let* ((before (cut-of st)) (a (ask st 'unlink B "cites" A)))
    (want "W4 (ii) unlink with from and to swapped answers (matched 0), the edge is still there, and an event is written"
          (list (head-of a) (clause-of a 'matched) (length (edges-of st A)) (equal? before (cut-of st)))
          '(ok (matched 0) 1 #f))))
;; W4 (iii): two identical link records are one edge.
(let* ((c (fresh!)) (st (car c)) (A (insert! st "root" "A")) (B (insert! st "root" "B")))
  (ask st 'link A "cites" B)
  (ask st 'link A "cites" B)
  (want "W4 (iii) after two identical links, unlink answers (matched 1): edges are counted, not records"
        (clause-of (ask st 'unlink A "cites" B) 'matched)
        '(matched 1)))
;; W4 (iv): link says whether the edge was already there.
(let* ((c (fresh!)) (st (car c)) (A (insert! st "root" "A")) (B (insert! st "root" "B")))
  (let* ((first (ask st 'link A "cites" B)) (second (ask st 'link A "cites" B)))
    (want "W4 (iv) link answers (matched 0) for a new edge and (matched 1) for one already there"
          (list (clause-of first 'matched) (clause-of second 'matched))
          '((matched 0) (matched 1)))))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows)
(printf "move-and-edges complete\n")
(exit (if (= bad 0) 0 1))
