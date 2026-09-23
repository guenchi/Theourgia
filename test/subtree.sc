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

;; What "under this block" means, asked of the two verbs that ask it.
;;
;; KEY: THE TWO ANSWERS ARE DIFFERENT, AND BOTH ARE PINNED HERE. `read
;; --recursive` stops at a nested document -- the rule export states for
;; itself, because a nested document is its own file -- while `grep
;; --under` searches everything below the block, nested documents
;; included. Both walk the tree with one procedure, `outline-subtree`,
;; which stops at nothing; each applies its own rule to the result. A
;; change that moved either rule into the shared walk would change the
;; other verb's answer, and one of the first two rows below would say so.
;;
;; NEVER: `#f` IS NOT AN ANSWER THE WALK GIVES. To `read` it means "no such
;; block"; to `grep` it means "no --under was given, do not filter".
;;
;; Before this change, grep's filter was `(if under (subtree-ids rows under)
;; #f)` -- the walk's answer WAS the filter -- so a walk answering `#f` for a
;; root it could not find would have switched a filter on a missing block
;; off: every block scanned, and an answer that looks entirely normal. That
;; is the hazard this change removed, and it is history. Today the walk's
;; answer is handed to `for-each` to build the filter (`store.sc`, where
;; `wanted` is bound), so a walk answering `#f` would raise there instead.
;;
;; The section "a root that names no block" pins the behaviour the removal
;; was for: a missing root filters to nothing. The old hazard rebuilt in the
;; CALLER -- a root not found in the rows switching the filter off -- turns
;; its grep row red with a wrong answer. A walk that itself answered `#f`
;; turns two of its rows red another way: the grep row, because the raise
;; inside `store-grep` comes back from the grep handler as an error answer,
;; and the row that asks the walk directly.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia reduce) outline-subtree state-outline block-id)
        (only (theourgia store) open-and-reduce)
        (theourgia log))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/subtree-" (number->string (get-process-id))))
(system (string-append "rm -rf " here "; mkdir -p " here "/store " here "/home"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(define store (string-append here "/store"))
(define (ask . req) (rpc-dispatch store req "test"))

;; ---- the tree ---------------------------------------------------------------
;;
;;   A  document
;;     B  section            "needle in b"
;;       C  DOCUMENT, nested
;;         D  section        "needle in d"
;;       E  section          "needle in e"
;;       F  section          "needle in f"
;;
;; NOTE: C IS WRITTEN THROUGH THE LOG, NOT THROUGH A VERB. Every write verb
;; refuses a document that is not at the top level, so a nested one arrives
;; only from history written before that rule or from another store. This
;; is the way `md2.sc` builds one, for the same reason.
(ask 'init)

(define (ids-in-outline)
  (map caddr (state-outline (open-and-reduce store))))

(define (the-new-id before)
  (let loop ((xs (ids-in-outline)))
    (cond ((null? xs) #f)
          ((member (car xs) before) (loop (cdr xs)))
          (else (car xs)))))

(define (insert! . args)
  (let ((before (ids-in-outline)))
    (apply ask 'insert args)
    (the-new-id before)))

(define A (insert! "--title" "A" "--text" "top"))
(ask 'set A "kind" "doc")
(define B (insert! "--under" A "--title" "B" "--text" "needle in b"))
(define C
  (let* ((sess (log-begin store (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "subtree-fixture" '()
                                      (list 'put (list (cons 'kind 'doc)
                                                       (cons 'path "inner.md")
                                                       (cons 'title "C")
                                                       (cons 'parent B)
                                                       (cons 'ord 0)))))
    (session-commit! sess)
    (log-end! sess)
    id))
(define D (insert! "--under" C "--title" "D" "--text" "needle in d"))
(define E (insert! "--under" B "--title" "E" "--text" "needle in e"))
(define F (insert! "--under" B "--title" "F" "--text" "needle in f"))

;; CONTROL: the tree is the one drawn above. Every row below reads its
;; answer against these six names, so a setup that went wrong has to show
;; here first rather than as six confusing failures later.
(want "S-0 CONTROL: the six blocks exist, and C is under B and D under C"
      (let* ((outline (state-outline (open-and-reduce store)))
             (parent-of (lambda (id)
                          (let loop ((rs outline))
                            (cond ((null? rs) 'NOT-PLACED)
                                  ((equal? (caddr (car rs)) id) (car (car rs)))
                                  (else (loop (cdr rs))))))))
        (list (length outline)
              (equal? (parent-of B) A)
              (equal? (parent-of C) B)
              (equal? (parent-of D) C)
              (equal? (parent-of E) B)
              (equal? (parent-of F) B)))
      (list 6 #t #t #t #t #t))

(define (ids-of answer)
  (if (and (pair? answer) (eq? (car answer) 'ok) (pair? (cadr answer)))
      (map (lambda (it) (let ((e (and (pair? it) (assq 'id it)))) (if e (cdr e) it)))
           (cdr (cadr answer)))
      answer))

(define (match-ids answer)
  (if (and (pair? answer) (eq? (car answer) 'ok) (pair? (cadr answer)))
      (map (lambda (it) (if (and (pair? it) (eq? (car it) 'match)) (cadr it) it))
           (cdr (cadr answer)))
      answer))

(define (scanned-blocks answer)
  (let ((s (and (pair? answer) (assq 'scanned (cdr answer)))))
    (and s (let ((b (assq 'blocks (cdr s)))) (and b (cadr b))))))

;; ---- the two answers, one per rule ----------------------------------------------

;; COMPARED AS AN ORDERED LIST, so this row also holds the order: the root,
;; then each child followed by what is under it, siblings in the outline's
;; order. C and D are absent because C is a document.
(want "S-1 read <doc> --recursive stops at a nested document and everything under it"
      (ids-of (ask 'read A "--recursive"))
      (list A B E F))

;; The stop applies to what is found UNDER the root. Asked of the nested
;; document itself, `read` answers with it and its section.
(want "S-1 TWIN: read <nested doc> --recursive keeps its own root, whatever its kind"
      (ids-of (ask 'read C "--recursive"))
      (list C D))

;; D is here and it is not in S-1: the two rows together are the
;; difference between the verbs. Compared sorted, because the question is
;; which blocks were searched, and grep's order is the subject of its own
;; fixtures.
(want "S-2 grep --under <doc> searches into the nested document"
      (list-sort string<? (match-ids (ask 'grep "needle" "--under" A)))
      (list-sort string<? (list B D E F)))

;; The root is part of what it names. B holds a needle; asked under B, B is
;; among the answers.
(want "S-2 TWIN: grep --under <block> includes the block itself"
      (list-sort string<? (match-ids (ask 'grep "needle" "--under" B)))
      (list-sort string<? (list B D E F)))

;; ---- a root that names no block ------------------------------------------------

;; NEVER: A MISSING ROOT FILTERS TO NOTHING, IT DOES NOT SWITCH THE FILTER
;; OFF. Both halves are read: no match, AND no block scanned. The second is
;; the one that tells the two apart -- a grep with no filter over a store
;; without the needle would also have no match, but it would have scanned
;; every block.
(want "S-3 grep --under <a block that does not exist> scans nothing"
      (let ((a (ask 'grep "needle" "--under" "nosuch.1")))
        (list (match-ids a) (scanned-blocks a)))
      (list '() 0))

;; CONTROL: the needle is in the store and an unfiltered grep scans every
;; block, so the empty answer above comes from the filter and not from
;; there being nothing to find.
(want "S-3 CONTROL: with no --under the same grep scans all six blocks and finds four"
      (let ((a (ask 'grep "needle")))
        (list (scanned-blocks a) (length (match-ids a))))
      (list 6 4))

(want "S-4 read <a block that does not exist> --recursive answers unknown-id"
      (let ((a (ask 'read "nosuch.1" "--recursive")))
        (list (car a) (cadr a)))
      (list 'error 'unknown-id))

;; The walk itself, asked directly. `#f` is never among its answers; a root
;; that is in no row comes back alone.
(want "S-5 the shared walk answers a root it cannot find with that root alone, never #f"
      (let ((outline (state-outline (open-and-reduce store))))
        (list (outline-subtree outline "nosuch.1")
              (outline-subtree '() "anything")))
      (list (list "nosuch.1") (list "anything")))

;; ---- siblings with the same ord ------------------------------------------------
;;
;; NOTE: WHEN TWO SIBLINGS SHARE AN ORD, THE OUTLINE ORDERS THEM BY BLOCK ID,
;; compared as text. The walk inherits that order from the rows rather than
;; deciding one. To tell "by id" apart from "in the order they were
;; written", the two are written so that those disagree. The sequence in an
;; id is BASE 36 (`block-id` in `reduce.sc`), so the place where text order
;; and written order part is `.z` followed by `.10`: written in that order,
;; and as text `.10` sorts first.
;;
;; NEVER: THE FIRST VERSION OF THIS ROW ASSUMED DECIMAL, from ids seen in
;; passing that happened to end in digits, and aimed at `.9` and `.10`. The
;; id after `.9` is `.a`, which sorts after `.9` as text too, so the row
;; could not have told the two orders apart. The encoding is read from the
;; code that makes the ids, not from examples of them.
(define store2 (string-append here "/store2"))
(system (string-append "mkdir -p " store2))
(rpc-dispatch store2 '(init) "test")
(define (raw-put! st fields)
  (let* ((sess (log-begin st (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "subtree-fixture" '()
                                      (list 'put fields)))
    (session-commit! sess)
    (log-end! sess)
    id))
(define (seq-of id)
  (let loop ((i (- (string-length id) 1)))
    (if (char=? (string-ref id i) #\.)
        (let digits ((k (+ i 1)) (n 0))
          (if (= k (string-length id))
              n
              (let ((c (string-ref id k)))
                (digits (+ k 1)
                        (+ (* n 36)
                           (if (char-numeric? c)
                               (- (char->integer c) (char->integer #\0))
                               (+ 10 (- (char->integer c) (char->integer #\a)))))))))
        (loop (- i 1)))))
(define P (raw-put! store2 (list (cons 'kind 'section) (cons 'title "P")
                                 (cons 'parent 'root) (cons 'ord 0))))
(define fillers
  (let loop ((ord 1) (acc '()))
    (let ((next (raw-put! store2 (list (cons 'kind 'section) (cons 'title "filler")
                                       (cons 'parent P) (cons 'ord ord)))))
      (if (>= (seq-of next) 34)
          (reverse (cons next acc))
          (loop (+ ord 1) (cons next acc))))))
(define X (raw-put! store2 (list (cons 'kind 'section) (cons 'title "X")
                                 (cons 'parent P) (cons 'ord 100))))
(define Y (raw-put! store2 (list (cons 'kind 'section) (cons 'title "Y")
                                 (cons 'parent P) (cons 'ord 100))))

;; CONTROL: the two really are written in the order X then Y, and as text
;; Y's id sorts before X's. Without both, the row below cannot tell the two
;; orders apart.
(want "S-6 CONTROL: X is written before Y, and Y's id sorts first as text"
      (list (seq-of X) (seq-of Y) (string<? Y X))
      (list 35 36 #t))

(want "S-6 siblings that share an ord come back in block-id order, not the order written"
      (let ((ids (ids-of (rpc-dispatch store2 (list 'read P "--recursive") "test"))))
        (list-tail ids (- (length ids) 2)))
      (list Y X))

;; ---- --under together with --all, and the limits ------------------------------
;;
;; KEY: THE FILTER RUNS BEFORE ANY LIMIT COUNTS A LINE. A block outside the
;; subtree is skipped before its lines are matched, so it can neither take
;; room from the blocks inside nor add to what the answer says it left out.
;; Both halves are asked below, each by block id, never by a total alone.
;;
;; The tree: X at the top with eleven sections under it, each holding thirty
;; lines that match. That is more than one block may show without --all
;; (twenty), and the eleven together are more than one answer may show
;; (two hundred), so both limits are reached inside X before the scan leaves
;; it. Y, with thirty more, sits under W, which is beside X.
;;
;; Y IS UNDER W, NOT AT THE TOP, SO THAT IT IS SCANNED LAST. grep scans in
;; the outline's order, which sorts rows by their parent first. The top
;; level's parent is the name `root`, and whether that sorts before or after
;; a block id depends on the writer name each run draws. W is written after
;; X, so W's id sorts after X's while both sequence numbers are one base36
;; digit, and every section under X comes before Y. A limit that is reached
;; only after the scan has passed the outside block would go unasked
;; otherwise. The CONTROL row pins the order.
(define store3 (string-append here "/store3"))
(system (string-append "mkdir -p " store3))
(define (ask3 . req) (rpc-dispatch store3 req "test"))
(ask3 'init)
(define (ids3) (map caddr (state-outline (open-and-reduce store3))))
(define (insert3! . args)
  (let ((before (ids3)))
    (apply ask3 'insert args)
    (let loop ((xs (ids3)))
      (cond ((null? xs) #f) ((member (car xs) before) (loop (cdr xs))) (else (car xs))))))
(define thirty-lines
  (let loop ((k 1) (acc ""))
    (if (> k 30)
        acc
        (loop (+ k 1) (string-append acc (if (= k 1) "" "\n") "hay " (number->string k))))))
(define X (insert3! "--title" "X" "--text" "no match here"))
(define sections
  (let loop ((k 1) (acc '()))
    (if (> k 11)
        (reverse acc)
        (loop (+ k 1)
              (cons (insert3! "--under" X "--title" (string-append "X" (number->string k))
                              "--text" thirty-lines)
                    acc)))))
(define W (insert3! "--title" "W" "--text" "no match here"))
(define Y (insert3! "--under" W "--title" "Y" "--text" thirty-lines))
(define V (insert3! "--title" "V" "--text" "no match here"))

;; Each block the answer names, with how many of its lines it shows, in
;; the order the answer gives them.
(define (lines-per-block answer)
  (let loop ((xs (match-ids answer)) (acc '()))
    (cond ((null? xs) (reverse acc))
          ((and (pair? acc) (string=? (car xs) (car (car acc))))
           (loop (cdr xs) (cons (list (car xs) (+ 1 (cadr (car acc)))) (cdr acc))))
          (else (loop (cdr xs) (cons (list (car xs) 1) acc))))))
(define (each-with n ids) (map (lambda (id) (list id n)) ids))
(define (first-n n xs) (if (= n 0) '() (cons (car xs) (first-n (- n 1) (cdr xs)))))

;; CONTROL: an unfiltered grep with --all sees all twelve blocks, thirty
;; lines each, and Y after every section of X. Without this the rows below
;; could pass on a store where Y matched nothing or came first.
(want "S-7 CONTROL: without --under, grep --all finds thirty lines in each of X's sections, then Y"
      (lines-per-block (ask3 'grep "hay" "--all"))
      (each-with 30 (append sections (list Y))))

(define (lines-and-truncated answer)
  (let ((t (assq 'truncated (cdr answer))))
    (list (lines-per-block answer) (and t (cdr t)))))

;; With --all nothing is left out, so the answer carries no truncated
;; clause at all -- Y's thirty lines are not X's to count as omitted.
(want "S-7 grep --under X --all answers every matching line in X's subtree and none from Y"
      (lines-and-truncated (ask3 'grep "hay" "--under" X "--all"))
      (list (each-with 30 sections) #f))

;; Without --all the first ten sections show twenty each, which is the two
;; hundred one answer may show; the eleventh shows none. Left out: ten from
;; each of the ten and all thirty of the eleventh -- 130 lines -- and one
;; block that matched and showed nothing. Y's thirty are not X's to report.
(want "S-7 TWIN: without --all, what the answer says it left out counts X's subtree only"
      (lines-and-truncated (ask3 'grep "hay" "--under" X))
      (list (each-with 20 (first-n 10 sections))
            (list (list 'lines 130) (list 'blocks 1))))

;; The rows above reach the blocks outside the subtree only before any
;; line is shown or after the answer is full. Here the outside blocks come
;; while the answer is part way: the first section is the whole subtree,
;; and the ten sections after it are outside it.
(want "S-7 a section alone: the sections after it are outside, though the answer is not yet full"
      (lines-and-truncated (ask3 'grep "hay" "--under" (car sections)))
      (list (each-with 20 (list (car sections)))
            (list (list 'lines 10) (list 'blocks 0))))

;; A subtree that exists, is scanned, and matches nothing answers nothing,
;; while blocks outside it match.
(want "S-7 a subtree with no match answers no lines, though blocks outside it match"
      (let ((a (ask3 'grep "hay" "--under" V)))
        (list (lines-and-truncated a) (scanned-blocks a)))
      (list (list '() #f) 1))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\nsubtree complete\n" rows bad)
(exit (if (zero? bad) 0 1))
