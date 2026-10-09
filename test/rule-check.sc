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
;; the copy of a reduction sharing nothing it could change.

(import (chezscheme) (theourgia rpc)
        (only (theourgia reduce) block-id reduce-applied-cut state-hash block-hash state->rows
              reduce-empty reduce-apply! reduce-gates)
        (only (theourgia store) open-and-reduce)
        (only (theourgia log) writer-directory)
        (only (theourgia trace) trace-enable!))

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
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (want-1 name
             (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                             (condition-message e) e)
                                 (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
               got)
             expected))))

;; The names this change adds are looked up when called, so on a tree
;; without them the rows run and say what is missing.
(define (reduce-clone r) ((eval 'reduce-clone (environment '(theourgia reduce))) r))

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

;; ---- the selector -----------------------------------------------------------------------------

(tolerant (run 'rule "reviews-cite" "--on" "doc" "--where" "(field ?w \"role\" \"review\")" "--must" "(edge ?w depends-on ?s)"))
(define R (tolerant (make 'doc "R")))
(want "J a block the selector does not match is not judged; one the write makes match is"
      (in-order (string? R) (head (run 'set R "role" "review") 3))
      '(#t (error refused rule-violation)))
(tolerant (run 'rule "reviews-cite" "--retire"))

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
(want "J one whose prefix holds writes the prefix and answers as it did: the first item ok, the second refused, done 1"
      (let ((a (batch (list 'insert 'root #f '((kind . doc) (title . "P1")))
                      (list 'expect stale (list 'set D1 'title "z")))))
        (in-order (car a) (car (car (cadr a))) (car (cadr (cadr a))) (caddr a)))
      '(batch ok error (done 1)))

;; ---- a commit's plan -------------------------------------------------------------------------

(tolerant (run 'rule "section-titled" "--on" "section" "--must" "(title ?w ?t)"))
(tolerant (run 'write S0 "a draft"))
(define commit-events (tolerant (events)))
(want "J a commit is judged as it would land: its member's block fails a rule, and the commit writes nothing"
      (in-order (head (run 'commit S0) 3) (- (events) commit-events))
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

(want "J the per-write facts are unknown to query"
      (in-order (head (run 'query "(cited ?a ?b)") 4) (head (run 'query "(kind+ ?a ?k)") 4))
      '((error bad-request unknown-relation (relation cited)) (error bad-request unknown-relation (relation kind+))))

;; ---- typed endpoints --------------------------------------------------------------------------

(tolerant (run 'relation "answers" "--as" "nothing" "--from" "(kind doc)" "--to" "(kind decision)"))
(want "E a link whose end its selector does not match is refused, every mismatch listed"
      (run 'link D1 "answers" D2)
      (list 'error 'bad-request 'relation-endpoint
            (list 'failures (list '(relation answers) (list 'edge D1 'answers D2) '(end to) '(expected ((kind decision)))))))
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
(want "C a batch that links then unlinks cites nothing"
      (car (run 'batch (format "~s" (list (list 'link C1 'depends-on C2) (list 'unlink C1 'depends-on C2)))
                "--premises" (premise-of C1)))
      'batch)
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
      (in-order (equal? (state-hash original) hash-0) (equal? (state->rows original) rows-0) (reduce-gates original))
      '(#t #t ()))
(tolerant (reduce-apply! original "a" 3 '() '(set "a.1" title "q")))
(tolerant (reduce-apply! original "aaa00000" 1 '() '(put ((kind . section))) (claim-actor)))
(tolerant (reduce-apply! original "bbb00000" 1 '() '(put ((kind . section))) (claim-actor)))
(want "K the same records reduced into the original bring it to the copy's hash, rows and gates"
      (in-order (equal? (state-hash original) (state-hash copy)) (equal? (state->rows original) (state->rows copy))
                (equal? (reduce-gates original) (reduce-gates copy)))
      '(#t #t #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nrule-check complete\n" bad rows)
(exit (if (= bad 0) 0 1))
