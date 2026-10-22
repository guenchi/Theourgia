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

;; A WRITE JUDGED BY THE STORE'S RULES AND TYPED RELATIONS.
;;
;; A store with a rule or a typed relation in force runs each committed write
;; first against a copy of its state, with nothing written, judges what that
;; produced, and only then writes. These rows: a store with neither makes no
;; copy; a rule refuses with every failing pair and writes nothing; the
;; targets of a write; a write rule's two views; a batch's prefix; citation
;; coverage; typed endpoints; the order of a write's checks; check's audit;
;; the copy of a reduction sharing nothing it could change; a large write,
;; its + facts charged by the tuples a call matches.

(import (chezscheme) (theourgia rpc)
        (only (theourgia reduce) block-id reduce-applied-cut state-hash block-hash state->rows state-read state-edges
              reduce-empty reduce-apply! reduce-gates rows->state state-declaration)
        (only (theourgia store) open-and-reduce)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) writer-directory log-begin log-end! session-view session-append! make-frame
              log-publish! segment-sha
              view-revision view-epoch view-writer view-expect-seq session-written-events)
        (only (theourgia trace) trace-enable!)
        (only (theourgia crc32) crc32-hex)
        (only (theourgia query) make-query-session session-answer session-spent)
        (only (theourgia extensions) extension-verbs))

;; The verbs registered from outside the core table (query among them), as
;; core.sc and the daemon register them.
(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
;; EVERY TOP-LEVEL VALUE AND STEP IS COMPUTED UNDER A GUARD, but the paths
;; and the closing lines: a top-level form that raises would stop the file
;; before its rows; under the guard it is (RAISED ...), and the rows that
;; read it fail.
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x))))
             e))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (with-expected name expected (x) (want-1 name (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                             (condition-message e) e)
                                 (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
               got) x)))))

;; The names this change adds are looked up when called, so on a tree
;; without them the rows run and say what is missing.
(define (reduce-clone r) ((eval 'reduce-clone (environment '(theourgia reduce))) r))
(define (session-dry-clone s) ((eval 'session-dry-clone (environment '(theourgia log))) s))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/rule-check-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define S (string-append root "/s"))
(define (run . args) (rpc-dispatch S args "author"))
(define (state) (open-and-reduce S))
(define (events) (apply + (map cdr (reduce-applied-cut (state)))))
(define (new-id a)
  (let* ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (item-ids a) (map new-id (cadr a)))
(define (batch . intents) (run 'batch (format "~s" intents)))
;; A block of KIND made by a batch of one insert, with a title when given.
(define (make kind . title)
  (car (item-ids (batch (list 'insert 'root #f (append (list (cons 'kind kind))
                                                        (if (pair? title) (list (cons 'title (car title))) '())))))))
(define (log-bytes)
  (let ((dir (writer-directory S writer)))
    (apply + (map (lambda (f) (bytevector-length (call-with-port (open-file-input-port (string-append dir "/" f)) get-bytevector-all)))
                  (filter (lambda (f) (and (> (string-length f) 5)
                                           (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))))
                          (directory-list dir))))))
;; The id the next block this store's writer creates will have.
(define (next-id) (block-id writer (+ 1 (cdr (assoc writer (reduce-applied-cut (state)))))))
;; THUNK's value and the trace it printed, as (value . text).
(define (traced thunk)
  (let-values (((p get) (open-string-output-port)))
    (trace-enable! #t)
    (let ((v (parameterize ((current-error-port p)) (thunk))))
      (trace-enable! #f)
      (cons v (get)))))
(define (count-in text needle)
  (let loop ((i 0) (n 0))
    (cond ((> (+ i (string-length needle)) (string-length text)) n)
          ((string=? (substring text i (+ i (string-length needle))) needle) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))
(define (premise-of id) (format "((premise ~s ~s))" id (block-hash (state) id)))
(define (head a n) (if (and (list? a) (>= (length a) n)) (list-head a n) a))

(tolerant (run 'init))
;; Made before any rule: an untitled doc, which check's audit finds later.
(define U (tolerant (make 'doc)))
(define writer (tolerant (car (car (reduce-applied-cut (state))))))
(define D1 (tolerant (make 'doc "D1")))
(define D2 (tolerant (make 'doc "D2")))
(define V (tolerant (make 'doc "victim")))
(define D5 (tolerant (make 'doc "D5")))
(define S0 (tolerant (make 'section)))
(tolerant (run 'set D5 "phase" "draft"))
(tolerant (run 'link D1 "asks" D2))
;; A doc deleted before any rule, an end for a typed link between deleted blocks.
(define W (tolerant (make 'doc "W")))
(tolerant (run 'del W))

;; ---- a store with no rule and no typed relation makes no copy -------------------------------

(want "N a write in a store with no rule and no typed relation makes no rehearsal"
      (let ((t (traced (lambda () (make 'doc "plain")))))
        (count-in (cdr t) "(trace rehearsal"))
      0)

;; ---- a state rule ---------------------------------------------------------------------------

(tolerant (run 'rule "doc-titled" "--on" "doc" "--must" "(title ?w ?t)"))
(want "N a write in a store with a rule in force is rehearsed once, the trace line marked"
      (let ((t (traced (lambda () (make 'doc "titled")))))
        (list (count-in (cdr t) "(trace rehearsal") (> (count-in (cdr t) "(rehearsal)") 0)))
      '(1 #t))
(define would-be (tolerant (next-id)))
(define bytes-before (tolerant (log-bytes)))
(define events-before (tolerant (events)))
(define refused (tolerant (batch (list 'insert 'root #f '((kind . doc))))))
(want "J a must rule refuses a write whose target has no row: rule, block by the id it would have had, goal with ?w bound, expected, rows; answered bare"
      refused
      (list 'error 'refused 'rule-violation
            (list 'failures (list '(rule doc-titled) (list 'block would-be) (list 'goal (list 'title would-be '?t))
                                  '(expected some) '(rows 0)))))
(want "J a refused write writes nothing: the log as long in records and in bytes"
      (in-order (- (events) events-before) (- (log-bytes) bytes-before))
      '(0 0))
(want "J the next write gets the id the refused one would have had"
      (make 'doc "after")
      would-be)

;; Two targets fail one rule in one write: both pairs, by block.
(define two-ids (tolerant (let ((n (cdr (assoc writer (reduce-applied-cut (state))))))
                            (list-sort string<? (list (block-id writer (+ n 1)) (block-id writer (+ n 2)))))))
(want "J every failing pair is listed, by block: two untitled docs in one batch"
      (let ((a (batch (list 'insert 'root #f '((kind . doc))) (list 'insert 'root #f '((kind . doc))))))
        (and (pair? a) (eq? (car a) 'error) (map (lambda (f) (cadr (cadr f))) (cdr (list-ref a 3)))))
      two-ids)

;; ---- a must-not and its witness -------------------------------------------------------------

(tolerant (run 'rule "no-cites" "--on" "doc" "--must-not" "(edge ?w cites ?x)"))
(want "J a must-not refuses with its witness rows and the exact count, only for the block that fails"
      (run 'link D1 "cites" D2)
      (list 'error 'refused 'rule-violation
            (list 'failures (list '(rule no-cites) (list 'block D1) (list 'goal (list 'edge D1 'cites '?x))
                                  '(expected none) '(rows 1) (list 'witness (list D2))))))
(tolerant (run 'rule "no-cites" "--retire"))
(want "J a retired rule judges nothing"
      (car (run 'link D1 "cites" D2))
      'ok)

;; A must-not with more rows than the witness holds: the count exact, ten rows listed.
(define H (tolerant (make 'doc "H")))
(define spokes (tolerant (item-ids (apply batch (map (lambda (i) (list 'insert 'root #f (list '(kind . doc) (cons 'title (format "S~a" i)))))
                                                     '(1 2 3 4 5 6 7 8 9 10 11))))))
(tolerant (apply batch (map (lambda (x) (list 'link H 'cites x)) spokes)))
(tolerant (run 'rule "hub-few" "--on" "doc" "--where" "(field ?w \"hub\" \"yes\")" "--must-not" "(edge ?w cites ?x)"))
(want "J a must-not's count is exact and its witness at most ten rows"
      (let ((a (run 'set H "hub" "yes")))
        (and (pair? a) (eq? (car a) 'error)
             (let ((f (car (cdr (list-ref a 3)))))
               (in-order (assq 'rows f) (length (cdr (assq 'witness f)))))))
      '((rows 11) 10))
(tolerant (run 'rule "hub-few" "--retire"))

;; ---- the selector -----------------------------------------------------------------------------

(tolerant (run 'rule "reviews-cite" "--on" "doc" "--where" "(field ?w \"role\" \"review\")" "--must" "(edge ?w depends-on ?s)"))
(define R (tolerant (make 'doc "R")))
(want "J a block the selector does not match is not judged; one the write makes match is"
      (in-order (string? R) (head (run 'set R "role" "review") 3))
      '(#t (error refused rule-violation)))
(tolerant (run 'rule "reviews-cite" "--retire"))

;; A NESTED AND IN A RULE JUDGES A WRITE: the selector matches through its
;; nested and, the goal fails through its, and the failure names the goal
;; with ?w bound in every place.
(define NJ (tolerant (make 'doc "NJ")))
(define NJ2 (tolerant (make 'doc "NJ2")))
(tolerant (run 'set NJ "phase" "live"))
(tolerant (run 'rule "nested-judged" "--on" "doc" "--where" "(and (field ?w \"role\" \"nested\") (and (field ?w \"phase\" \"live\")))"
               "--must" "(and (title ?w ?t) (and (edge ?w depends-on ?s) (title ?s ?u)))"))
(want "J a rule whose where and goal hold nested ands judges a write: refused, the goal named with ?w bound throughout"
      (let ((a (run 'set NJ "role" "nested")))
        (and (pair? a) (eq? (car a) 'error) (>= (length a) 4)
             (in-order (list-head a 3) (cadr (assq 'goal (car (cdr (list-ref a 3))))))))
      (list '(error refused rule-violation)
            (list 'and (list 'title NJ '?t) (list 'and (list 'edge NJ 'depends-on '?s) '(title ?s ?u)))))
(want "J the selector's nested condition alone keeps a block out: NJ2, role nested and no phase, is not judged"
      (car (run 'set NJ2 "role" "nested"))
      'ok)
(tolerant (run 'rule "nested-judged" "--retire"))

;; ---- the targets of a write -------------------------------------------------------------------

(define L (tolerant (make 'doc "L")))
(tolerant (run 'set L "locked" "yes"))
(tolerant (run 'rule "locked-no-in" "--on" "doc" "--where" "(field ?w \"locked\" \"yes\")" "--must-not" "(edge ?a ?r ?w)"))
(tolerant (run 'rule "locked-no-out" "--on" "doc" "--where" "(field ?w \"locked\" \"yes\")" "--must-not" "(edge ?w ?r ?b)"))
(want "J both ends of a link are targets: a link to the block and one from it are refused, each by its rule, naming the block"
      (in-order (let ((a (run 'link D2 "relates" L))) (and (pair? a) (eq? (car a) 'error) (map (lambda (f) (list (car f) (cadr f))) (cdr (list-ref a 3)))))
                (let ((a (run 'link L "relates" D2))) (and (pair? a) (eq? (car a) 'error) (map (lambda (f) (list (car f) (cadr f))) (cdr (list-ref a 3))))))
      (list (list (list '(rule locked-no-in) (list 'block L)))
            (list (list '(rule locked-no-out) (list 'block L)))))
(tolerant (run 'rule "locked-no-in" "--retire"))
(tolerant (run 'rule "locked-no-out" "--retire"))
(tolerant (run 'rule "keep-victim" "--on" "doc" "--where" "(title ?w \"victim\")" "--must" "(field ?w \"kept\" \"yes\")"))
;; A SANITY ROW, not a discriminating one: a deleted block holds no facts, so
;; no rule applies to it whether or not it is a target.
(want "J a block the write deletes is no target: a rule it would fail does not refuse its deletion"
      (car (run 'del V))
      'ok)
(tolerant (run 'rule "keep-victim" "--retire"))

;; ---- a write rule: the + facts read the write's view, the plain facts the state before ---------

(tolerant (run 'rule "done-from-ready" "--on" "doc" "--where" "(field+ ?w \"phase\" \"done\")" "--must" "(field ?w \"phase\" \"ready\")"))
(want "J a write rule: done is refused from draft, then ready is not selected, then done from ready passes"
      (in-order (head (run 'set D5 "phase" "done") 3)
                (car (run 'set D5 "phase" "ready"))
                (car (run 'set D5 "phase" "done")))
      '((error refused rule-violation) ok ok))
(tolerant (run 'rule "done-from-ready" "--retire"))

;; ---- a batch's prefix -------------------------------------------------------------------------

(define stale "0000000000000000000000000000000000000000000000000000000000000000")
(define prefix-events (tolerant (events)))
(want "J a batch whose second expectation fails and whose prefix fails a rule writes nothing"
      (in-order (head (batch (list 'insert 'root #f '((kind . doc)))
                             (list 'expect stale (list 'set D1 'title "z")))
                      3)
                (- (events) prefix-events))
      '((error refused rule-violation) 0))
(define p1-events (tolerant (events)))
(want "J one whose prefix holds writes the prefix and answers as it did: the first item ok, the second refused, done 1; one record, the block there"
      (let ((a (batch (list 'insert 'root #f '((kind . doc) (title . "P1")))
                      (list 'expect stale (list 'set D1 'title "z")))))
        (in-order (car a) (car (car (cadr a))) (car (cadr (cadr a))) (caddr a) (- (events) p1-events)
                  (let ((id (new-id (car (cadr a))))) (and id (cdr (assq 'title (cdr (assq 'fields (state-read (state) id)))))))))
      '(batch ok error (done 1) 1 "P1"))

;; ---- a commit's plan -------------------------------------------------------------------------

(tolerant (run 'rule "section-titled" "--on" "section" "--must" "(title ?w ?t)"))
(tolerant (run 'write S0 "a draft" "--writer" "rc"))
(define commit-events (tolerant (events)))
(want "J a commit is judged as it would land: its member's block fails a rule, and the commit writes nothing"
      (in-order (head (run 'commit S0 "--writer" "rc") 3) (- (events) commit-events))
      '((error refused rule-violation) 0))
(tolerant (run 'rule "section-titled" "--retire"))

;; ---- check's audit ----------------------------------------------------------------------------

(want "J check audits a state rule over the current state: the untitled doc made before the rule is listed, the verdict unchanged"
      (let ((c (cdr (run 'check))))
        (in-order (filter (lambda (r) (eq? (car r) 'rule-violation)) (cdr (or (assq 'rules c) '(rules))))
                  (cadr (assq 'verdict c))))
      (list (list (list 'rule-violation '(rule doc-titled) (list 'block U) (list 'goal (list 'title U '?t))
                        '(expected some) '(rows 0)))
            'ok))

;; ---- the per-write facts are the rule check's only --------------------------------------------

(want "J the per-write facts are unknown to query, every one of them"
      (map (lambda (g) (head (run 'query g) 3))
           '("(kind+ ?a ?k)" "(field+ ?a ?f ?v)" "(edge+ ?a ?r ?b)" "(edge-kind+ ?a ?k ?b)" "(cited ?a ?b)" "(unread ?a)" "(receipt-carried)"))
      (map (lambda (x) '(error bad-request unknown-relation)) '(1 2 3 4 5 6 7)))

;; ---- typed endpoints --------------------------------------------------------------------------

(tolerant (run 'relation "answers" "--as" "nothing" "--from" "(kind doc)" "--to" "(kind decision)"))
(want "E a link whose end its selector does not match is refused, every mismatch listed"
      (run 'link D1 "answers" D2)
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation answers) (list 'edge D1 'answers D2) '(end to) '(expected ((kind decision)))))))
(want "E a link to a deleted block is judged too: its end holds no kind, and the selector is not matched"
      (run 'link D1 "answers" V)
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation answers) (list 'edge D1 'answers V) '(end to) '(expected ((kind decision)))))))
(want "E a link between two deleted blocks is judged at both ends, though neither is a target"
      (run 'link V "answers" W)
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation answers) (list 'edge V 'answers W) '(end from) '(expected ((kind doc))))
                            (list '(relation answers) (list 'edge V 'answers W) '(end to) '(expected ((kind decision)))))))
(define made-q (tolerant (batch (list 'insert 'root #f '((kind . decision) (title . "Q")))
                                (list 'link D1 'answers '(from 0)))))
(define Q (tolerant (car (item-ids made-q))))
(want "E a block created and linked in one batch is judged with the kind the batch gives it"
      (map car (cadr made-q))
      '(ok ok))
(want "E a write that retypes an end of a typed edge is refused"
      (run 'set Q "kind" "doc")
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation answers) (list 'edge D1 'answers Q) '(end to) '(expected ((kind decision)))))))
(tolerant (run 'relation "asks" "--as" "nothing" "--from" "(kind decision)"))
(want "E an edge that predates its relation's typing is listed by check"
      (cdr (or (assq 'relation-endpoints (cdr (run 'check))) '(relation-endpoints)))
      (list (list 'relation-endpoint '(relation asks) (list 'edge D1 'asks D2) '(end from) '(expected ((kind decision))))))

;; A store with a typed relation and no rule is rehearsed and judged too.
(define S4 (string-append root "/s4"))
(tolerant (rpc-dispatch S4 '(init) "author"))
(define (make4 kind title)
  (car (item-ids (rpc-dispatch S4 (list 'batch (format "~s" (list (list 'insert 'root #f (list (cons 'kind kind) (cons 'title title)))))) "author"))))
(define F1 (tolerant (make4 'doc "F1")))
(define F2 (tolerant (make4 'doc "F2")))
(tolerant (rpc-dispatch S4 '(relation "answers" "--as" "nothing" "--to" "(kind decision)") "author"))
(want "E a store whose only judge is a typed relation refuses a link its selector does not match"
      (head (rpc-dispatch S4 (list 'link F1 "answers" F2) "author") 3)
      '(error bad-request relation-endpoint))

;; THE WILDCARD END: `_` in a selector's field clause holds for any value of a
;; field the block carries, and a variable there is refused when the relation
;; is declared, writing nothing.
(define S6 (string-append root "/s6"))
(tolerant (rpc-dispatch S6 '(init) "author"))
(define (run6 . args) (rpc-dispatch S6 args "author"))
(define (batch6 . intents) (run6 'batch (format "~s" intents)))
(define (events6) (apply + (map cdr (reduce-applied-cut (open-and-reduce S6)))))
(define (make6 fields) (car (item-ids (batch6 (list 'insert 'root #f fields)))))
(define G1 (tolerant (make6 '((kind . doc) (title . "G1")))))
(define G2 (tolerant (make6 '((kind . doc) (title . "G2")))))
(want "E a selector whose field clause is the wildcard is declared"
      (head (run6 'relation "lists" "--as" "nothing" "--from" "(kind doc) (field decision _)") 1)
      '(ok))
(define made-r (tolerant (batch6 (list 'insert 'root #f '((kind . doc) (title . "R") (decision . "report")))
                                 (list 'link '(from 0) 'lists G1))))
(want "E a block that carries the field, whatever its value, matches the wildcard: created and linked in one batch"
      (map car (cadr made-r))
      '(ok ok))
(want "E a block that does not carry the field does not match it, and the link is refused"
      (run6 'link G2 "lists" G1)
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation lists) (list 'edge G2 'lists G1) '(end from)
                                  '(expected ((kind doc) (field decision _)))))))
(define R6 (tolerant (make6 '((kind . doc) (title . "R6") (decision . "report")))))
(tolerant (run6 'relation "tags" "--as" "nothing" "--from" "(kind doc) (field decision \"tag\")"))
(want "E CONTROL: a literal field clause holds only for its own value: a report block is refused under \"tag\""
      (head (run6 'link R6 "tags" G1) 3)
      '(error bad-request relation-endpoint))
(define events-before-variable (tolerant (events6)))
(want "E a variable in a field clause is refused when the relation is declared, and nothing is written"
      (in-order (run6 'relation "picks" "--as" "nothing" "--from" "(kind doc) (field decision ?k)")
                (- (events6) events-before-variable))
      '((error malformed-intent (selector-value-not-literal)) 0))
(define events-before-batch (tolerant (events6)))
(define batch-variable (tolerant (run6 'batch "((relation picks2 (nothing ((kind doc) (field decision ?k)) ())))")))
(want "E a batch that declares with a variable is refused the same way, and nothing is written"
      (in-order (and (memq 'selector-value-not-literal
                           (let flat ((x batch-variable))
                             (cond ((pair? x) (append (flat (car x)) (flat (cdr x)))) ((null? x) '()) (else (list x)))))
                     #t)
                (- (events6) events-before-batch))
      '(#t 0))

;; A DECLARATION AN EARLIER BUILD WROTE WITH A VARIABLE IS STILL APPLIED AS
;; WRITTEN: the refusal is the write path's, and replay and a snapshot apply
;; the record, comparing `?k` as the literal symbol it is.
(include "forge-record.ss")
(define S7 (string-append root "/s7"))
(tolerant (rpc-dispatch S7 '(init) "author"))
(define (run7 . args) (rpc-dispatch S7 args "author"))
(define (make7 fields) (car (item-ids (run7 'batch (format "~s" (list (list 'insert 'root #f fields)))))))
(define (declared7)
  (filter (lambda (i) (and (pair? i) (eq? (car i) 'declared) (eq? (cadr i) 'picks)))
          (let ((a (run7 'query "--relations"))) (if (and (pair? a) (assq 'items (cdr a))) (cdr (assq 'items (cdr a))) '()))))
(tolerant (forge-record! S7 "(relation picks (nothing ((kind doc) (field custom ?k)) ()))"))
(define K7 (tolerant (make7 '((kind . doc) (title . "K7") (custom . ?k)))))
(define X7 (tolerant (make7 '((kind . doc) (title . "X7") (custom . "x")))))
(define declared7-replayed (tolerant (declared7)))
(want "E a declaration recorded with a variable by an earlier build is in force on replay"
      declared7-replayed
      '((declared picks nothing (from ((kind doc) (field custom ?k))) (to ()))))
(want "E it compares the symbol as a literal: a block holding ?k is linked, one holding \"x\" is refused"
      (in-order (head (run7 'link K7 "picks" X7) 1) (head (run7 'link X7 "picks" K7) 3))
      '((ok) (error bad-request relation-endpoint)))
(define snapshot7 (tolerant (run7 'snapshot)))
(want "E the snapshot is taken"
      (head snapshot7 1)
      '(ok))
;; THE RESTORE PATH ITSELF: the reduction's rows, read back by rows->state,
;; hold the declaration replay applied, and the store read after the
;; snapshot answers it too.
(define reduced7 (tolerant (open-and-reduce S7)))
(want "E the rows round trip and the read after the snapshot keep the declaration replay applied"
      (in-order (state-declaration reduced7 'picks)
                (equal? (state-declaration (rows->state (state->rows reduced7)) 'picks) (state-declaration reduced7 'picks))
                (equal? (tolerant (declared7)) declared7-replayed))
      '((in-force (nothing ((kind doc) (field custom ?k)) ())) #t #t))

;; A rule that reached the store by another writer's record and names a
;; relation the rule verb refuses (def reads outside the log) is never asked:
;; every write it applies to is refused unevaluable with the verb's refusal,
;; and check lists it unevaluable.
(define S5 (string-append root "/s5"))
(tolerant (rpc-dispatch S5 '(init) "author"))
(define G1 (tolerant (car (item-ids (rpc-dispatch S5 (list 'batch (format "~s" (list '(insert root #f ((kind . doc) (title . "G1")))))) "author")))))
(define writer5 (tolerant (car (car (reduce-applied-cut (open-and-reduce S5))))))
(define forged5 (tolerant
  (encode-record 1 1789000000001 "peer" (list (cons writer5 (cdr (assoc writer5 (reduce-applied-cut (open-and-reduce S5))))))
                 (storable-encode (list 'rule 'outside '((class state) (on doc) (must-not (def ?w ?l "x"))))))))
(tolerant (log-publish! S5 "peerzzzz" 1 forged5 (segment-sha forged5)))
(want "J a stored rule its value's check refuses is never asked: the write is unevaluable with that refusal, and check lists it"
      (in-order (let ((a (rpc-dispatch S5 (list 'set G1 "role" "x") "author")))
                  (and (pair? a) (eq? (car a) 'error) (list (head a 3) (list-ref a 3) (head (cadr (list-ref a 5)) 3))))
                (let ((r (assq 'rules (cdr (rpc-dispatch S5 '(check) "author")))))
                  (and r (map car (cdr r)))))
      (list (list '(error refused rule-unevaluable) '(rule outside) '(error bad-request rule-relation-not-allowed))
            '(rule-unevaluable)))

;; Two more rules the verb would not write, each from another writer: a
;; built-in this build does not know, and a class its goals do not give. A
;; write is refused naming the first; check lists both unevaluable, each with
;; the verb's answer.
(define S6 (string-append root "/s6"))
(tolerant (rpc-dispatch S6 '(init) "author"))
(define G6 (tolerant (car (item-ids (rpc-dispatch S6 (list 'batch (format "~s" (list '(insert root #f ((kind . doc) (title . "G6")))))) "author")))))
(define writer6 (tolerant (car (car (reduce-applied-cut (open-and-reduce S6))))))
(define (forge6 peer value)
  (tolerant
    (let ((bytes (encode-record 1 1789000000001 "peer" (list (cons writer6 (cdr (assoc writer6 (reduce-applied-cut (open-and-reduce S6))))))
                                (storable-encode value))))
      (log-publish! S6 peer 1 bytes (segment-sha bytes)))))
(forge6 "peeraaaa" '(rule future ((builtin future-rule))))
(forge6 "peerbbbb" '(rule wrongclass ((class write) (on doc) (must (title ?w ?t)))))
(want "J a stored built-in this build does not know, and a stored class its goals do not give, are unevaluable; check lists both"
      (in-order (let ((a (rpc-dispatch S6 (list 'set G6 "role" "x") "author")))
                  (and (pair? a) (eq? (car a) 'error) (list (head a 3) (list-ref a 3))))
                (let ((r (assq 'rules (cdr (rpc-dispatch S6 '(check) "author")))))
                  (and r (map (lambda (x) (list (car x) (cadr x) (head (cadr (list-ref x 2)) 3))) (cdr r)))))
      (list (list '(error refused rule-unevaluable) '(rule future))
            (list (list 'rule-unevaluable '(rule future) '(error bad-request builtin-rule-not-known))
                  (list 'rule-unevaluable '(rule wrongclass) '(error bad-request rule-not-in-its-form)))))

;; ---- citation coverage and the order of a write's checks --------------------------------------

(define C1 (tolerant (make 'doc "C1")))
(define C2 (tolerant (make 'doc "C2")))
(tolerant (run 'rule "cover" "--builtin" "citation-coverage"))
(want "C a write that carried premises and cites a block they do not hold is refused, the witness named"
      (let ((a (run 'link C1 "depends-on" C2 "--premises" (premise-of C1))))
        (in-order (head a 3) (and (list? a) (>= (length a) 4) (assq 'witness (cdr (car (cdr (list-ref a 3))))))))
      (list '(error refused rule-violation) (list 'witness (list 'citation-not-read (list 'block C1) (list 'cites C2)))))
(want "C one whose premises hold the cited block passes; one without premises is not covered"
      (in-order (car (run 'link C1 "depends-on" C2 "--premises" (premise-of C2)))
                (car (run 'unlink C1 "depends-on" C2))
                (car (run 'link C1 "depends-on" C2)))
      '(ok ok ok))
(tolerant (run 'unlink C1 "depends-on" C2))
(tolerant (run 'relation "leans-on" "--as" "depends-on"))
(want "C an edge under a second name of kind depends-on cites, though the block already depends on the same one"
      (in-order (car (run 'link C1 "depends-on" C2 "--premises" (premise-of C2)))
                (head (run 'link C1 "leans-on" C2 "--premises" (premise-of C1)) 3))
      '(ok (error refused rule-violation)))
(tolerant (run 'unlink C1 "depends-on" C2))
(want "C a batch that links then unlinks cites nothing: both items written, no edge left"
      (let ((a (run 'batch (format "~s" (list (list 'link C1 'depends-on C2) (list 'unlink C1 'depends-on C2)))
                    "--premises" (premise-of C1))))
        (in-order (car a) (and (pair? a) (list? (cadr a)) (map car (cadr a))) (caddr a)
                  (filter (lambda (e) (and (equal? (car e) C1) (equal? (caddr e) C2))) (state-edges (state)))))
      '(batch (ok ok) (done 2) ()))
(tolerant (run 'relation "builds-on" "--as" "depends-on" "--to" "(kind decision)"))
(want "O premises fail before the endpoints, the endpoints before the rules"
      (in-order (head (run 'link C1 "builds-on" C2 "--premises" (format "((premise ~s ~s))" C2 stale)) 2)
                (head (run 'link C1 "builds-on" C2 "--premises" (premise-of C1)) 3))
      '((error premise-changed) (error bad-request relation-endpoint)))

;; ---- the copy of a reduction ------------------------------------------------------------------

(define (claim-actor) (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0)))
(define original
  (tolerant
    (let ((r (reduce-empty)))
      (reduce-apply! r "a" 1 '() '(put ((kind . section))))
      (reduce-apply! r "a" 2 '() '(set "a.1" title "p"))
      r)))
(define copy (tolerant (reduce-clone original)))
(define hash-0 (tolerant (state-hash original)))
(define rows-0 (tolerant (state->rows original)))
(want "K a copy answers as its original: the same hash and the same rows"
      (in-order (equal? (state-hash copy) hash-0) (equal? (state->rows copy) rows-0))
      '(#t #t))
(tolerant (reduce-apply! copy "a" 3 '() '(set "a.1" title "q")))
(tolerant (reduce-apply! copy "aaa00000" 1 '() '(put ((kind . section))) (claim-actor)))
(tolerant (reduce-apply! copy "bbb00000" 1 '() '(put ((kind . section))) (claim-actor)))
(want "K reducing into the copy changes nothing its original holds: a block's field, the admission gates, the rows"
      (in-order (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates copy)))) (and p (cdr p)))) '("aaa00000" "bbb00000"))
                (equal? (state-hash original) hash-0) (equal? (state->rows original) rows-0) (reduce-gates original))
      '((plan-conflict plan-conflict) #t #t ()))
;; The admission tables are the copy's own: with them shared, a claim reduced
;; into the original after two claims of the same identity in the copy would
;; meet them there and be gated.
(define orig2 (tolerant (let ((r (reduce-empty))) (reduce-apply! r "a" 1 '() '(put ((kind . section)))) r)))
(define copy2 (tolerant (reduce-clone orig2)))
(tolerant (reduce-apply! copy2 "aaa00000" 1 '() '(put ((kind . section))) (claim-actor)))
(tolerant (reduce-apply! copy2 "bbb00000" 1 '() '(put ((kind . section))) (claim-actor)))
(tolerant (reduce-apply! orig2 "ccc00000" 1 '() '(put ((kind . section))) (claim-actor)))
(want "K a copy's admission is its own: two claims in the copy conflict there; one in the original after them finds none"
      (in-order (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates copy2)))) (and p (cdr p)))) '("aaa00000" "bbb00000"))
                (reduce-gates orig2))
      '((plan-conflict plan-conflict) ()))
(tolerant (reduce-apply! original "a" 3 '() '(set "a.1" title "q")))
(tolerant (reduce-apply! original "aaa00000" 1 '() '(put ((kind . section))) (claim-actor)))
(tolerant (reduce-apply! original "bbb00000" 1 '() '(put ((kind . section))) (claim-actor)))
(want "K the same records reduced into the original bring it to the copy's hash, rows and gates"
      (in-order (equal? (state-hash original) (state-hash copy)) (equal? (state->rows original) (state->rows copy))
                (equal? (reduce-gates original) (reduce-gates copy)))
      '(#t #t #t))

;; ---- a declaration takes effect from its record on -----------------------------------------
;;
;; A write is judged by the rules in force before it: a batch that declares a
;; rule and then breaks it is written; the next write that breaks it is not.
(want "J a batch that declares a rule and breaks it is written; the next write that breaks it is refused"
      (in-order (let ((a (batch (list 'rule 'task-titled '((class state) (on task) (must (title ?w ?t))))
                                (list 'insert 'root #f '((kind . task))))))
                  (and (pair? a) (eq? (car a) 'batch) (map car (cadr a))))
                (head (batch (list 'insert 'root #f '((kind . task)))) 3))
      '((ok ok) (error refused rule-violation)))
(tolerant (run 'rule "task-titled" "--retire"))

;; ---- an import's refusal is answered bare ----------------------------------------------------

(define md-dir (string-append root "/md"))
(tolerant (system (string-append "mkdir -p '" md-dir "' && printf '# Bad\\nbody\\n' > '" md-dir "/bad.md'")))
(tolerant (run 'rule "no-bad" "--on" "doc" "--must-not" "(title ?w \"Bad\")"))
(want "J import-md answers a rule's refusal as it is, not inside its import clause"
      (head (run 'import-md md-dir) 3)
      '(error refused rule-violation))
(tolerant (run 'rule "no-bad" "--retire"))

;; ---- the dry session stops where the real one stops -------------------------------------------
;;
;; A frame bound to a sequence the view does not expect: the dry session
;; refuses it as the real one does, before anything is reserved; a fresh dry
;; copy hands the right frame the sequence the view expects, and the real
;; session has written nothing.
(define dry-readings
  (tolerant
    (let ((s (log-begin S (lambda args 'applied))))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let* ((v (session-view s))
                 (frame-at (lambda (seq) (make-frame (view-revision v) (view-epoch v) (view-writer v) seq
                                                     "author" '() '(put ((kind . section))))))
                 (wrong (frame-at (+ 5 (view-expect-seq v))))
                 (dry-answer (session-append! (session-dry-clone s) wrong))
                 (real-answer (session-append! s wrong))
                 (right (session-append! (session-dry-clone s) (frame-at (view-expect-seq v)))))
            (list dry-answer real-answer
                  (list (car right) (= (cadr right) (view-expect-seq v)) (caddr right))
                  (session-written-events s))))
        (lambda () (log-end! s))))))
(want "D a dry session refuses a frame bound to an unexpected sequence as the real one does, and numbers the right one as the view expects"
      dry-readings
      '((refused-before-reserve expect-seq) (refused-before-reserve expect-seq) (committed #t #f) ()))

;; ---- one judgement, one caller, every route --------------------------------------------------
;;
;; Every committed write reaches the judgement through with-store-write's
;; one writing branch, a plan's completion included: the judgement's entry
;; is named once where it is defined and once where it is called, and no
;; other product file enters the rules library.
(define (forms-of path)
  (tolerant (call-with-input-file path
              (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
(define (occurrences sym x)
  (cond ((eq? x sym) 1) ((pair? x) (+ (occurrences sym (car x)) (occurrences sym (cdr x)))) (else 0)))
(define product-files
  (tolerant
    (filter (lambda (f) (and (> (string-length f) 3) (string=? ".sc" (substring f (- (string-length f) 3) (string-length f)))))
            (directory-list ".."))))
(want "J the judgement is defined once and called once, in store.sc's writing branch; only store.sc enters (theourgia rules)"
      (let ((store-forms (forms-of "../store.sc")))
        (in-order (occurrences 'rehearsal-refusal store-forms)
                  (occurrences 'write-judgement-procedure store-forms)
                  (list-sort string<?
                             (filter (lambda (f) (and (not (string=? f "rules.sc"))
                                                      (let ((text (call-with-input-file (string-append "../" f) get-string-all)))
                                                        (let loop ((i 0))
                                                          (cond ((> (+ i 16) (string-length text)) #f)
                                                                ((string=? (substring text i (+ i 16)) "(theourgia rules") #t)
                                                                (else (loop (+ i 1))))))))
                                     product-files))))
      '(2 2 ("store.sc")))

;; ---- a large write: a + fact is charged by the tuples a call matches -------------------------
;;
;; A write rule is asked at every target of a write, and every ask reads the
;; write's + facts. A call is charged the tuples it matches, each time it is
;; dispatched; were it charged the whole relation at each ask, the cost of a
;; batch would grow as the square of its size and a few hundred inserts
;; would exhaust the write's one budget.

(define (store-at name)
  (let ((s (string-append root "/" name)))
    (tolerant (rpc-dispatch s '(init) "author"))
    s))
(define (on s . args) (rpc-dispatch s args "author"))
(define (events-of s) (apply + (map cdr (reduce-applied-cut (open-and-reduce s)))))
(define (items-ok? a) (and (pair? a) (pair? (cdr a)) (list? (cadr a)) (for-all (lambda (i) (and (pair? i) (eq? (car i) 'ok))) (cadr a))))
(define (clause-in a name) (and (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) name))) a)))
(define (reason-of a) (let ((r (clause-in a 'reason))) (and r (cadr r))))

;; THE LEDGER'S DECLARATIONS, as a ledger of items declares them: two
;; vocabularies, the risk list and its gate, the decision's edges, and nine
;; rules, five of them write rules over field+.
(define ledger-declarations
  (list
    (list 'derive "ledger-status"
          "((head (ledger-status STR)) (clause (ledger-status \"open\") (= \"open\" \"open\")) (clause (ledger-status \"in-progress\") (= \"open\" \"open\")) (clause (ledger-status \"landed\") (= \"open\" \"open\")) (clause (ledger-status \"closed\") (= \"open\" \"open\")) (clause (ledger-status \"deferred\") (= \"open\" \"open\")) (clause (ledger-status \"accepted\") (= \"open\" \"open\")))")
    (list 'derive "ledger-severity"
          "((head (ledger-severity STR)) (clause (ledger-severity \"high\") (= \"open\" \"open\")) (clause (ledger-severity \"medium\") (= \"open\" \"open\")) (clause (ledger-severity \"low\") (= \"open\" \"open\")) (clause (ledger-severity \"none\") (= \"open\" \"open\")))")
    (list 'derive "release-risk"
          "((head (release-risk ID)) (clause (release-risk ?i) (field ?i \"ledger\" \"F\") (field ?i \"severity\" \"high\") (field ?i \"status\" ?s) (member ?s (\"open\" \"in-progress\" \"deferred\" \"accepted\"))))")
    (list 'derive "release-blocker"
          "((head (release-blocker ID)) (clause (release-blocker ?i) (release-risk ?i) (field ?i \"status\" ?s) (/= ?s \"accepted\")))")
    (list 'relation "lists" "--as" "nothing" "--from" "(kind doc) (field ledger \"D\")" "--to" "(kind doc) (field ledger \"F\")")
    (list 'rule "status-vocab" "--on" "doc" "--where" "(field ?w \"ledger\" \"F\")"
          "--must" "(and (field ?w \"status\" ?s) (ledger-status ?s))")
    (list 'rule "severity-vocab" "--on" "doc" "--where" "(field ?w \"ledger\" \"F\")"
          "--must" "(and (field ?w \"severity\" ?s) (ledger-severity ?s))")
    (list 'rule "accepted-has-ruling" "--on" "doc" "--where" "(field ?w \"status\" \"accepted\")"
          "--must" "(field ?w \"accepted\" _)")
    (list 'rule "decision-vocab" "--on" "doc" "--where" "(field ?w \"decision\" _)"
          "--must" "(and (field ?w \"decision\" ?k) (member ?k (\"release\" \"tag\" \"deploy\" \"report\")))")
    (list 'rule "decision-lists-risks" "--on" "doc" "--where" "(and (field+ ?w \"decision\" _) (not (field ?w \"decision\" _)))"
          "--must-not" "(and (release-risk ?i) (not (edge+ ?w lists ?i)))")
    (list 'rule "decision-ledger-settled" "--on" "doc" "--where" "(and (field+ ?w \"decision\" _) (not (field ?w \"decision\" _)))"
          "--must-not" "(and (field ?i \"ledger\" \"F\") (not (and (field ?i \"status\" ?s) (ledger-status ?s) (field ?i \"severity\" ?v) (ledger-severity ?v))))")
    (list 'rule "action-not-blocked" "--on" "doc" "--where" "(and (field+ ?w \"decision\" _) (not (field ?w \"decision\" _)))"
          "--must-not" "(and (field+ ?w \"decision\" ?k) (member ?k (\"release\" \"tag\" \"deploy\")) (release-blocker ?i))")
    (list 'rule "decision-batch-pure" "--on" "doc" "--on" "section" "--where" "(field+ ?w \"ledger\" \"F\")"
          "--must-not" "(and (field+ ?d \"decision\" _) (not (field ?d \"decision\" _)) (not (and (field ?w \"ledger\" \"F\") (field ?w \"status\" ?s) (field+ ?w \"status\" ?s) (field ?w \"severity\" ?v) (field+ ?w \"severity\" ?v))))")
    (list 'rule "decision-batch-keeps-members" "--on" "doc" "--on" "section" "--where" "(field ?w \"ledger\" \"F\")"
          "--must-not" "(and (field+ ?d \"decision\" _) (not (field ?d \"decision\" _)) (not (field+ ?w \"ledger\" \"F\")))")))
;; ONE LEDGER ROW: an item, a status and a severity from the vocabularies,
;; an accepted row with its ruling, and a line of text.
(define (ledger-row i)
  (let* ((status (list-ref '("open" "in-progress" "landed" "closed" "deferred" "accepted") (mod i 6)))
         (severity (list-ref '("high" "medium" "low" "none") (mod i 4))))
    (list 'insert 'root #f
          (append (list '(kind . doc) (cons 'title (format "row ~a" i)) '(ledger . "F") (cons 'item (format "F~a" i))
                        (cons 'status status) (cons 'severity severity)
                        (cons 'src (format " **row ~a** | the item's text, as a ledger line holds it | ~a |" i status))
                        (cons 'size (list-ref '("small" "medium" "large") (mod i 3))))
                  (if (string=? status "accepted") (list '(accepted . "2026-10-22: ruled")) '())))))

(define LS (store-at "ledger"))
(want "W the ledger's four relations, its edge relation and its nine rules are declared on a fresh store, each answering ok"
      ;; IN ORDER, a rule after the relations it names: map's order is open
      (let loop ((ds ledger-declarations) (out '()))
        (if (null? ds)
            (reverse out)
            (let ((a (apply on LS (car ds))))
              (unless (and (pair? a) (eq? (car a) 'ok)) (printf "reading: ~a refused ~s\n" (cadr (car ds)) a))
              (loop (cdr ds) (cons (and (pair? a) (car a)) out)))))
      (map (lambda (d) 'ok) ledger-declarations))
(define ledger-events (tolerant (events-of LS)))
(define ledger-batch (tolerant (on LS 'batch (format "~s" (map ledger-row (iota 307))))))
(want "W 307 ledger rows in one batch are accepted under the nine rules: every item ok, 307 blocks written"
      (in-order (head ledger-batch 1) (items-ok? ledger-batch) (and (items-ok? ledger-batch) (length (cadr ledger-batch))) (- (events-of LS) ledger-events))
      '((batch) #t 307 307))

;; CONTROL: at that size the nine rules still judge: a decision block in
;; the same batch as 307 more rows is refused by the batch's purity rule.
(define mixed-events (tolerant (events-of LS)))
(define mixed-batch
  (tolerant (on LS 'batch (format "~s" (append (map (lambda (i) (ledger-row (+ 1000 i))) (iota 307))
                                               (list (list 'insert 'root #f '((kind . doc) (title . "release") (ledger . "D") (decision . "report")))))))))
(want "W CONTROL: a decision block in a batch with 307 ledger rows is refused by decision-batch-pure, nothing written"
      (in-order (head mixed-batch 3)
                (let ((f (clause-in mixed-batch 'failures)))
                  (and f (memq 'decision-batch-pure (map (lambda (x) (cadr (car x))) (cdr f))) #t))
                (- (events-of LS) mixed-events))
      '((error refused rule-violation) #t 0))

;; A WRITE RULE OVER field+ ALONE, at every target of a batch of 300.
(define PS (store-at "owned"))
(tolerant (on PS 'rule "phase-owned" "--on" "doc" "--where" "(field+ ?w \"phase\" _)" "--must" "(field+ ?w \"owner\" _)"))
(define (owned-row i owner?)
  (list 'insert 'root #f
        (append (list '(kind . doc) (cons 'title (format "o~a" i)) (cons 'phase (format "p~a" (mod i 5)))
                      (cons 'note (format "n~a" i)))
                (if owner? (list (cons 'owner (format "w~a" (mod i 7)))) '()))))
(define owned-events (tolerant (events-of PS)))
(define owned-batch (tolerant (on PS 'batch (format "~s" (map (lambda (i) (owned-row i #t)) (iota 300))))))
(want "W a batch of 300 inserts under a field+ write rule is accepted: every item ok, 300 blocks written"
      (in-order (head owned-batch 1) (items-ok? owned-batch) (and (items-ok? owned-batch) (length (cadr owned-batch))) (- (events-of PS) owned-events))
      '((batch) #t 300 300))
(define unowned-events (tolerant (events-of PS)))
(define unowned-batch (tolerant (on PS 'batch (format "~s" (map (lambda (i) (owned-row i (not (= i 150)))) (iota 300))))))
(want "W CONTROL: the rule still bites at that size: the one insert of 300 without an owner refuses the batch, one failure, nothing written"
      (in-order (head unowned-batch 3)
                (let ((f (clause-in unowned-batch 'failures))) (and f (map (lambda (x) (cadr (car x))) (cdr f))))
                (- (events-of PS) unowned-events))
      '((error refused rule-violation) (phase-owned) 0))

;; WHAT A CALL IS CHARGED, read from the budget: field+ of 50 tuples, two of
;; them a's; a call naming a matches those two. Each dispatch is charged the
;; tuples it matches, the ones it then unifies; the query runs two passes.
(define fifty
  (list (cons* 'field+ 3 (list "a" "p" "1") (list "a" "q" "2")
               (map (lambda (i) (list (format "b~a" i) "p" (number->string i))) (iota 48)))))
(define (fact-session budget) (make-query-session (open-and-reduce PS) '() budget #f fifty))
(define a-goal '(field+ "a" ?f ?v))
(want "W a field+ call is charged by its matched count each time it is dispatched, not the fifty of the relation: 2 at each of two passes and 2 for its two rows, 6"
      (let* ((S (fact-session 1000000)) (a (session-answer S a-goal)))
        (list (car a) (length (cadr a)) (session-spent S)))
      '(ok 2 6))
(want "W the same call again in the session is charged by its matched count each time again: the memo saves the scan, not the charge, 6"
      (let ((S (fact-session 1000000)))
        (session-answer S a-goal)
        (let ((a (session-answer S a-goal))) (list (car a) (session-spent S))))
      '(ok 6))
(want "W a budget below the call's matched count refuses the query, naming field+ and the count it reached"
      (session-answer (fact-session 1) a-goal)
      '(error query-budget (relation field+) (tuples 2)))
;; A JOIN WHOSE TEST REJECTS EVERY PAIR adds no row, so only the calls'
;; charges bound its work: 300 tuples of one phase, every pair unified and
;; every pair rejected. The second call is dispatched once per binding of
;; the first, each charged its 300.
(define one-phase
  (list (cons* 'field+ 3 (map (lambda (i) (list (format "c~a" i) "phase" "same")) (iota 300)))))
(want "W a field+ join whose test rejects every pair is bounded: at budget 1000 refused at the fourth dispatch, naming field+"
      (session-answer (make-query-session (open-and-reduce PS) '() 1000 #f one-phase)
                      '(and (field+ ?a "phase" ?x) (field+ ?b "phase" ?y) (/= ?x ?y)))
      '(error query-budget (relation field+) (tuples 1200)))

;; A JOIN OF field+ WITH ITSELF THAT NO BUDGET HOLDS: every pair of the 300
;; targets sharing a phase, at each target.
(tolerant (on PS 'rule "phase-pairs" "--on" "doc" "--must-not" "(and (field+ ?a \"phase\" ?x) (field+ ?b \"phase\" ?x))"))
(define join-events (tolerant (events-of PS)))
(define join-batch (tolerant (on PS 'batch (format "~s" (map (lambda (i) (owned-row (+ 1000 i) #t)) (iota 300))))))
(printf "reading: the join's refusal reason ~s\n" (reason-of join-batch))
(want "W a field+ join over budget is still refused as unevaluable at its rule, query-budget, nothing written"
      (in-order (head join-batch 3) (clause-in join-batch 'rule)
                (let ((r (reason-of join-batch))) (and (pair? r) (head r 2)))
                (- (events-of PS) join-events))
      '((error refused rule-unevaluable) (rule phase-pairs) (error query-budget) 0))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nrule-check complete\n" bad rows)
(exit (if (= bad 0) 0 1))
