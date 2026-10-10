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

;; `context --for <id> --budget <tokens>`: the hard material is the answer of
;; five queries and the records of the blocks they name, each hard block in
;; exactly one section; the preferred material fills what is left in a stated
;; order; the receipt is what a commit gives back, and a real commit with it
;; is refused exactly when the hard material changed; the answer is within
;; its budget as the bytes a caller receives. Every row runs on a store,
;; through the verbs.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read state-block-ids block-hash block-id reduce-applied-cut)
        (only (theourgia render) render-wire render-human)
        (only (theourgia wire) sexpr->string-extended string->sexpr-extended storable-encode))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (write-file! path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (key<? a b) (string<? (format "~s" a) (format "~s" b)))
(define (sorted xs) (list-sort key<? xs))
(define (unique xs)
  (let loop ((l xs) (out '()))
    (cond ((null? l) (reverse out))
          ((member (car l) out) (loop (cdr l) out))
          (else (loop (cdr l) (cons (car l) out))))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/context-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(define home (string-append root "/home"))
(putenv "THEOURGIA_HOME" home)

;; ---- a store, built through the verbs ------------------------------------------------------------------
;;
;; A STORE is a record of its path and its writer; every helper takes one.
(define-record-type st (fields path writer))
(define store-count 0)
(define (fresh-store! . template)
  (set! store-count (+ store-count 1))
  (let* ((p (string-append root "/s" (number->string store-count)))
         (a (rpc-dispatch p (if (pair? template) (list 'init "--template" (car template)) '(init)) "test")))
    (make-st p (cadr (assq 'writer (cdr a))))))
(define (run s . args) (rpc-dispatch (st-path s) args "test"))
(define (state-of s) (open-and-reduce (st-path s)))
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
;; A block under PARENT ("root" at the top) with a title, then each field set
;; in turn: kind, class, status, text.
(define (make! s parent title . fields)
  (let ((id (new-id (run s 'insert "--under" parent "--title" title))))
    (let loop ((fs fields))
      (unless (null? fs)
        (run s 'set id (car fs) (cadr fs))
        (loop (cddr fs))))
    id))
(define (link! s from rel to) (run s 'link from rel to))
(define (ver s id) (block-hash (state-of s) id))
(define (log-digest s)
  (let ((out (string-append root "/digest.txt")))
    (system (string-append "cd " (quoted (st-path s)) " && find writers -type f | LC_ALL=C sort | xargs cat 2>/dev/null | md5 > " (quoted out)))
    (file-text out)))

;; ---- reading an answer -----------------------------------------------------------------------------------

(define (ctx s for budget . flags) (apply run s 'context "--for" for "--budget" (number->string budget) flags))
(define (ok? a) (and (pair? a) (eq? (car a) 'ok)))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
;; A section's entries: `for` is one entry, every other section a list.
(define (section a name) (let ((c (clause a name))) (and c (cadr c))))
(define (ids-in a name) (let ((es (section a name))) (if (list? es) (map car es) 'NO-SECTION)))
(define (for-id a) (let ((e (section a 'for))) (and (pair? e) (car e))))
(define hard-sections '(constraints evidence to-verify))
(define (entry-of a id)
  (or (and (pair? (section a 'for)) (equal? (car (section a 'for)) id) (section a 'for))
      (exists (lambda (n) (let ((es (section a n))) (and (list? es) (find (lambda (e) (equal? (car e) id)) es))))
              '(constraints evidence to-verify counterexamples background))))
;; Where an id is entered: every section that holds it, in the answer's order.
(define (places a id)
  (append (if (equal? (for-id a) id) '(for) '())
          (filter (lambda (n) (let ((es (section a n))) (and (list? es) (member id (map car es)) #t)))
                  '(constraints evidence to-verify counterexamples background))))
(define (sub e name) (and (pair? e) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr e))))
(define (whys e) (sorted (if (pair? e) (filter (lambda (c) (and (pair? c) (eq? (car c) 'why))) (cdr e)) '())))
(define (level-of e) (let ((l (sub e 'level))) (and l (cadr l))))
(define (validity-of e) (let ((v (sub e 'validity))) (and v (cadr v))))
;; The hard material: `for`, the three hard sections and the notes, as data.
(define (hard-part a) (map (lambda (n) (clause a n)) '(for constraints evidence to-verify notes)))
;; ON A TREE WITHOUT THE VERB (red first) every answer is a refusal; the
;; accessors answer empty rather than raise, so each row reads its own red.
(define (receipt-of a) (clause a 'receipt))
(define (receipt-premises a) (let ((r (receipt-of a))) (if r (cdr r) '())))
;; THE RECEIPT IS GIVEN BACK AS THE ANSWER PRINTS IT: the clause itself,
;; (receipt <premise> ...), not its contents.
(define (receipt-text a) (let ((r (receipt-of a))) (if r (sexpr->string-extended r) "()")))
;; A COMMIT GIVES THE RECEIPT BACK. The commit writes nothing (no drafts), so
;; its answer is the premise check's own: ok, or premise-changed naming what
;; moved. This is the landed premises code, not a copy of the check.
(define (commit-with s a) (run s 'commit "--writer" (st-writer s) "--premises" (receipt-text a)))
(define (refused? answer) (and (pair? answer) (eq? (car answer) 'error) (pair? (cdr answer)) (eq? (cadr answer) 'premise-changed)))
;; The rows of a goal through the query verb, for comparison.
(define (query-rows s goal)
  (let ((a (run s 'query (sexpr->string-extended goal))))
    (if (ok? a) (map cdr (cdr (cadr a))) (list 'REFUSED a))))
(define (hard-ids s T) (unique (map car (query-rows s `(hard ,T ?h _ _)))))
;; Every hard block is in exactly one of `for` and the three hard sections.
(define (one-place-each a ids)
  (map (lambda (id) (list id (filter (lambda (p) (memq p (cons 'for hard-sections))) (places a id)))) ids))

;; ---- C1: the first answer ---------------------------------------------------------------------------------
;;
;; A store from the project template. T a task under tasks with a child
;; note; T depends-on S, a design section, and implements D1; under S an
;; open decision D2 and an implemented one D3 (a code block implements it);
;; the note depends-on a code block C; D1 supersedes D0.
(define (field-of s id f)
  (let ((r (state-read (state-of s) id)))
    (and r (let ((e (assq f (cdr (assq 'fields r))))) (and e (cdr e))))))
;; The block the template created with SLUG.
(define (created-of s slug)
  (let ((st (state-of s)))
    (find (lambda (id) (let ((r (state-read st id)))
                         (and r (let ((f (assq 'slug (cdr (assq 'fields r))))) (and f (equal? (cdr f) slug))))))
          (state-block-ids st))))
;; The blocks of the C1 store, by name.
(define (c1! . extra-depends)
  (let* ((s (fresh-store! "project"))
         (design (created-of s "design"))
         (tasks (created-of s "tasks"))
         (T (make! s tasks "Write the reader" "kind" "task" "status" "todo"))
         (note (make! s T "A note on the reader"))
         (S (make! s design "The reader's design"))
         (D1 (make! s design "Adopt the new reader" "kind" "decision"))
         (C (make! s "root" "reader core" "kind" "code" "lang" "scheme"))
         (D2 (make! s S "Choose the buffer size" "kind" "decision"))
         (D3 (make! s S "Keep the old parser" "kind" "decision"))
         (X3 (make! s "root" "parser kept" "kind" "code" "lang" "scheme"))
         (D0 (make! s design "Adopt the old reader" "kind" "decision")))
    (link! s T "depends-on" S)
    (link! s T "implements" D1)
    (link! s note "depends-on" C)
    (link! s X3 "implements" D3)
    (link! s D1 "supersedes" D0)
    (list s (list (cons 'T T) (cons 'note note) (cons 'S S) (cons 'D1 D1) (cons 'C C)
                  (cons 'D2 D2) (cons 'D3 D3) (cons 'X3 X3) (cons 'D0 D0)
                  (cons 'design design) (cons 'tasks tasks)))))
(define (named f name) (cdr (assq name (cadr f))))
(define c1 (c1!))
(define c1s (car c1))
(define (c1-id n) (named c1 n))
(define c1a (ctx c1s (c1-id 'T) 4000))
(want "C1 the answer is ok and its clauses come in the stated order"
      (and (ok? c1a)
           (filter (lambda (n) (memq n '(for constraints evidence to-verify counterexamples background notes
                                         insufficient excluded budget receipt cut versions name-use)))
                   (map car (cdr c1a))))
      '(for constraints evidence to-verify counterexamples background notes excluded budget receipt cut versions))
(want "C1 for is T"
      (for-id c1a)
      (c1-id 'T))
;; HARD ORDER (B4): T; the rest of the unit in document order (the note);
;; the other members by distance from the unit over depends, ties by id (S,
;; D1 and C are each one step away); then the blocks that are not members,
;; unsettled first (D2).
(want "C1 constraints are the note, S, D1, C and D2, in hard order"
      (ids-in c1a 'constraints)
      (append (list (c1-id 'note)) (list-sort string<? (list (c1-id 'S) (c1-id 'D1) (c1-id 'C))) (list (c1-id 'D2))))
(want "C1 the whys: the note unit, S member, D1 member and unsettled open, C member, D2 unsettled open"
      (map (lambda (n) (whys (entry-of c1a (c1-id n)))) '(note S D1 C D2))
      (list '((why unit)) '((why member)) (sorted '((why member) (why unsettled open))) '((why member)) '((why unsettled open))))
(want "C1 D3 (implemented) is in no hard section and not in for"
      (filter (lambda (p) (memq p (cons 'for hard-sections))) (places c1a (c1-id 'D3)))
      '())
(want "C1 D0 is a counterexample, why superseded-by D1, with no flag given"
      (in-order (places c1a (c1-id 'D0)) (whys (entry-of c1a (c1-id 'D0))))
      (list '(counterexamples) (list (list 'why 'superseded-by (c1-id 'D1)))))
(want "C1 evidence and to-verify are empty, and there are no notes"
      (in-order (section c1a 'evidence) (section c1a 'to-verify) (section c1a 'notes))
      '(() () ()))
(want "C1 the receipt: a block premise for every hard block, and the five query premises with T's id"
      (let* ((r (receipt-premises c1a))
             (blocks (filter (lambda (p) (string? (cadr p))) r))
             (queries (filter (lambda (p) (pair? (cadr p))) r))
             (T (c1-id 'T)))
        (in-order (sorted (map cadr blocks))
                  (map (lambda (p) (caddr p)) blocks)
                  (map (lambda (p) (cadr (cadr p))) queries)))
      (let ((T (c1-id 'T)))
        (list (sorted (hard-ids c1s T))
              (map (lambda (p) (ver c1s (cadr p)))
                   (filter (lambda (p) (string? (cadr p))) (receipt-premises c1a)))
              (list `(and (hard ,T ?h ?role ?about) (validity ?h ?v))
                    `(and (hard ,T ?h _ _) (validity-reason ?h ?why ?by))
                    `(and (unsettled-for ,T ?d) (decision-state ?d ?s))
                    `(and (unsettled-for ,T ?d) (decision-state ?d review) (moved-kind ?i implements ?d ?end))
                    `(and (scope-of ,T ?a) (contradicts ?a ?b))))))
(want "C1 a commit giving the receipt back at once is accepted"
      (car (commit-with c1s c1a))
      'ok)

;; ---- C2: the table ------------------------------------------------------------------------------------------
;;
;; T a task that depends-on every member below:
;;   M1 superseded by X1, X1 a member too;
;;   M2 superseded by R, which X2 supersedes (R and X2 are not members);
;;   M3 of class inference, needs-review (it depends-on P, edited after);
;;   M4 refuted by F (F is not a member);
;;   G proposed refuted by Pr, a block of class inference;
;;   Dm an open decision;
;;   M5 whose premise Q is deleted.
(define c2s (fresh-store!))
;; T sits under a parent section, so it has an ancestor: a block that is not
;; hard and is near, for the row that says nothing else is in a hard section.
(define (c2! title . fields) (apply make! c2s "root" title fields))
(define c2P (c2! "Table parent"))
(define c2T (make! c2s c2P "Table task" "kind" "task" "status" "todo"))
(define M1 (c2! "member one")) (define X1 (c2! "superseder one"))
(define M2 (c2! "member two")) (define R2 (c2! "middle superseder")) (define X2 (c2! "top superseder"))
(define M3 (c2! "member three" "class" "inference")) (define P3 (c2! "premise three"))
(define M4 (c2! "member four")) (define F4 (c2! "refuter four"))
(define G6 (c2! "proposal target")) (define Pr6 (c2! "proposer" "class" "inference"))
(define Dm (c2! "open decision member" "kind" "decision"))
(define M5 (c2! "member five")) (define Q5 (c2! "premise five"))
(for-each (lambda (m) (link! c2s c2T "depends-on" m)) (list M1 X1 M2 M3 M4 G6 Dm M5))
(link! c2s X1 "supersedes" M1)
(link! c2s R2 "supersedes" M2)
(link! c2s X2 "supersedes" R2)
(link! c2s M3 "depends-on" P3)
(run c2s 'set P3 "title" "premise three, edited after the link")
(link! c2s F4 "refutes" M4)
(link! c2s Pr6 "refutes" G6)
(link! c2s M5 "depends-on" Q5)
(run c2s 'del Q5)
(define c2a (ctx c2s c2T 100000))
(want "C2 the placements: superseded and refuted in to-verify, in force authoritative in constraints, in force not authoritative in evidence"
      (map (lambda (id) (filter (lambda (p) (memq p (cons 'for hard-sections))) (places c2a id)))
           (list c2T M1 X1 M2 R2 X2 M3 P3 M4 F4 G6 Pr6 Dm M5 Q5))
      '((for) (to-verify) (constraints) (to-verify) (to-verify) (constraints) (evidence) (constraints)
        (to-verify) (constraints) (constraints) (evidence) (constraints) (constraints) ()))
(want "C2 X1 is entered once, with every why it has: member, supersedes M1, cause M1"
      (whys (entry-of c2a X1))
      (sorted (list '(why member) (list 'why 'supersedes M1) (list 'why 'cause M1))))
(want "C2 a chain: R is to-verify (supersedes M2, cause M2), X2 a constraint (supersedes M2)"
      (in-order (whys (entry-of c2a R2)) (whys (entry-of c2a X2)))
      (list (sorted (list (list 'why 'supersedes M2) (list 'why 'cause M2))) (list (list 'why 'supersedes M2))))
(want "C2 the validity of each entry that is not valid, with its reasons"
      (map (lambda (id) (sub (entry-of c2a id) 'validity)) (list M1 M2 R2 M3 M4 G6 M5 X1 F4 Dm))
      (list (list 'validity 'superseded (list 'superseded-by X1))
            (list 'validity 'superseded (list 'superseded-by R2))
            (list 'validity 'superseded (list 'superseded-by X2))
            (list 'validity 'needs-review (list 'premise-moved P3))
            (list 'validity 'refuted (list 'refuted-by F4))
            (list 'validity 'needs-review (list 'proposed-refutes Pr6))
            (list 'validity 'needs-review (list 'premise-gone Q5))
            #f #f #f))
(want "C2 the refuter and the proposer are hard by cause: F a constraint, Pr evidence"
      (in-order (whys (entry-of c2a F4)) (whys (entry-of c2a Pr6)))
      (list (list (list 'why 'cause M4)) (list (list 'why 'cause G6))))
(want "C2 a decision that is unsettled and a member: one entry, two whys"
      (in-order (length (places c2a Dm)) (whys (entry-of c2a Dm)))
      (list 1 (sorted '((why member) (why unsettled open)))))
(want "C2 every block of (hard T ...) is in exactly one of for and the hard sections, and nothing else is there"
      (let ((hard (sorted (hard-ids c2s c2T))))
        (in-order (filter (lambda (r) (not (= 1 (length (cadr r))))) (one-place-each c2a hard))
                  (sorted (append (list (for-id c2a)) (apply append (map (lambda (n) (ids-in c2a n)) hard-sections))))))
      (list '() (sorted (hard-ids c2s c2T))))
(want "C2 no constraint is superseded, refuted or of a class that is not authoritative"
      (filter (lambda (e) (or (memq (validity-of e) '(superseded refuted))
                              (member (field-of c2s (car e) 'class) '(inference "inference"))))
              (section c2a 'constraints))
      '())
;; T of class inference and T superseded: each is `for`, whatever its class
;; or validity, and is entered nowhere else.
(define Tinf (c2! "Inference task" "kind" "task" "class" "inference"))
(define Tsup (c2! "Superseded task" "kind" "task"))
(define Ysup (c2! "its superseder"))
(link! c2s Ysup "supersedes" Tsup)
(want "C2 T of class inference and T superseded: in for, marked, and nowhere else"
      (let ((ai (ctx c2s Tinf 100000)) (as (ctx c2s Tsup 100000)))
        (in-order (places ai Tinf) (places as Tsup) (validity-of (section as 'for))))
      '((for) (for) superseded))

;; ---- C3: the receipt by construction -----------------------------------------------------------------------

(define (query-premises a) (filter (lambda (p) (pair? (cadr p))) (receipt-premises a)))
(want "C3a the five goals in the receipt are the five the verb ran: each digest equals the query verb's on the same goal"
      (map (lambda (p)
             (let ((q (run c1s 'query (sexpr->string-extended (cadr (cadr p))))))
               (equal? (caddr p) (cadr (clause q 'digest)))))
           (query-premises c1a))
      '(#t #t #t #t #t))
;; EACH CHANGE ALONE, on a store of its own: the C1 store with what the
;; change needs already in place; context taken; the change; the commit with
;; the receipt. VARIANT adds: E a member superseded by Y; K a todo task that
;; implements D2 and was edited after the link; Pi an inference block that
;; refutes D2, so D2 (hard, not a member) is already needs-review.
(define (c3-variant!)
  (let* ((f (c1!)) (s (car f)) (T (named f 'T)) (D2 (named f 'D2))
         (E (make! s "root" "member superseded")) (Y (make! s "root" "its superseder"))
         (K (make! s "root" "task implementing D2" "kind" "task" "status" "todo"))
         (Pi (make! s "root" "inference refuting D2" "class" "inference")))
    (link! s T "depends-on" E)
    (link! s Y "supersedes" E)
    (link! s K "implements" D2)
    (run s 'set K "title" "task implementing D2, edited after the link")
    (link! s Pi "refutes" D2)
    (list s (append (cadr f) (list (cons 'E E) (cons 'Y Y) (cons 'K K) (cons 'Pi Pi))))))
(define (refused-after change)
  (let* ((f (c3-variant!)) (s (car f)) (a (ctx s (named f 'T) 100000)))
    (change s (lambda (n) (named f n)))
    (let ((c (commit-with s a))) (if (refused? c) 'refused c))))
(define c3-changes
  (list (cons "an edit of S" (lambda (s n) (run s 'set (n 'S) "title" "The reader's design, edited")))
        (cons "a new decision under S" (lambda (s n) (make! s (n 'S) "A new decision" "kind" "decision")))
        (cons "a new depends-on from the note" (lambda (s n) (link! s (n 'note) "depends-on" (make! s "root" "a new premise"))))
        (cons "a block linked supersedes D1" (lambda (s n) (link! s (make! s "root" "newer than D1") "supersedes" (n 'D1))))
        (cons "a second block linked supersedes a superseded member" (lambda (s n) (link! s (make! s "root" "second superseder") "supersedes" (n 'E))))
        (cons "a block linked supersedes a shown superseder" (lambda (s n) (link! s (make! s "root" "above Y") "supersedes" (n 'Y))))
        (cons "the task that implements D2 set done after its text was edited (D2 open to review)" (lambda (s n) (run s 'set (n 'K) "status" "done")))
        (cons "a conflicts-with between D1 and D2" (lambda (s n) (link! s (n 'D1) "conflicts-with" (n 'D2))))
        (cons "an inference block refutes a hard non-member already needs-review" (lambda (s n) (link! s (make! s "root" "second inference" "class" "inference") "refutes" (n 'D2))))))
(for-each
  (lambda (c) (want (string-append "C3b the receipt is refused after " (car c)) (refused-after (cdr c)) 'refused))
  c3-changes)
(want "C3b the variant's own receipt, given back at once, is accepted (the rows above are not refused for nothing)"
      (let* ((f (c3-variant!)) (s (car f)) (a (ctx s (named f 'T) 100000))) (car (commit-with s a)))
      'ok)
;; A HARD BLOCK THAT DID NOT FIT IS STILL PREMISED: C's title is made long
;; enough that, at the smallest budget, C is named under insufficient; then
;; it is shortened.
(want "C3b the receipt is refused after the title of a hard block that did not fit is shortened so that it now fits"
      (let* ((f (c1!)) (s (car f)) (C (named f 'C)))
        (run s 'set C "title" (make-string 6000 #\c))
        (let* ((m (let ((c (clause (ctx s (named f 'T) 1) 'minimum))) (if c (cadr c) 1000)))
               (a (ctx s (named f 'T) m))
               (missing (let ((i (clause a 'insufficient))) (and i (map car (cadr (assq 'missing (cdr i))))))))
          (run s 'set C "title" "core")
          (in-order (and (list? missing) (member C missing) #t) (refused? (commit-with s a)))))
      '(#t #t))
(define (accepted-after change . flags)
  (let* ((f (c1!)) (s (car f)) (a (apply ctx s (named f 'T) 100000 flags)))
    (change s (lambda (n) (named f n)) f)
    (let ((b (apply ctx s (named f 'T) 100000 flags)) (c (commit-with s a)))
      (list (car c) (equal? (hard-part a) (hard-part b))))))
(want "C3c not refused after an edit of a background entry (the tasks root, an ancestor of T)"
      (accepted-after (lambda (s n f) (run s 'set (n 'tasks) "text" "the tasks root, edited")))
      '(ok #t))
(want "C3c not refused after an edit of an unrelated block"
      (accepted-after (lambda (s n f) (run s 'set (make! s "root" "unrelated") "title" "unrelated, edited")))
      '(ok #t))
(want "C3c not refused after ten new blocks that match T's text, and the hard sections are byte-identical"
      (accepted-after (lambda (s n f) (let loop ((k 0)) (when (< k 10) (make! s "root" (format "Write the reader ~a" k)) (loop (+ k 1))))))
      '(ok #t))
(want "C3c not refused after the cut moved with none of the above"
      (accepted-after (lambda (s n f) (run s 'tag "a-tag")))
      '(ok #t))

;; C3d THE PROPERTY: on random small stores and one random operation each, if
;; `for`, a hard section or the notes differ after the operation, the old
;; receipt is refused. A fixed generator (a linear congruential sequence), so
;; a failing case is the same case on the next run; the count of cases whose
;; hard material changed is read too, so the property is not met vacuously.
(define seed 20261004)
(define (rand n) (set! seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (mod (quotient seed 65536) n))
(define (pick xs) (list-ref xs (rand (length xs))))
(define relations '("depends-on" "implements" "supersedes" "refutes" "conflicts-with" "verifies"))
(define (random-store!)
  (let* ((s (fresh-store!))
         (T (make! s "root" "random task" "kind" "task" "status" "todo"))
         (n (+ 4 (rand 4)))
         (blocks (let loop ((k 0) (acc (list T)))
                   (if (= k n) (reverse acc)
                       (let* ((kind (pick '("section" "decision" "task" "section")))
                              (id (make! s (pick (cons "root" acc)) (format "block ~a" k) "kind" kind)))
                         (when (= 0 (rand 4)) (run s 'set id "class" "inference"))
                         (when (and (equal? kind "decision") (= 0 (rand 3))) (run s 'set id "status" "done"))
                         (loop (+ k 1) (cons id acc)))))))
    (let loop ((k (+ 3 (rand 6))))
      (unless (= k 0)
        (let ((a (pick blocks)) (b (pick blocks)))
          (unless (equal? a b) (link! s a (pick relations) b)))
        (loop (- k 1))))
    (list s T blocks)))
(define (random-operation! s T blocks)
  (let ((b (pick blocks)) (other (pick (filter (lambda (x) (not (equal? x T))) blocks))))
    (case (rand 6)
      ((0) (run s 'set b "title" "edited"))
      ((1) (let ((c (pick blocks))) (unless (equal? b c) (link! s b (pick relations) c))))
      ((2) (run s 'del other))
      ((3) (run s 'set other "status" "done"))
      ((4) (run s 'set other "class" "inference"))
      (else (make! s (pick (cons "root" blocks)) "a new block")))))
(define c3d
  (caught
  (let loop ((k 0) (changed 0) (violations '()))
    (if (= k 200)
        (list changed violations)
        (let* ((r (random-store!)) (s (car r)) (T (cadr r))
               (a (ctx s T 100000)))
          (random-operation! s T (caddr r))
          (let* ((b (ctx s T 100000))
                 (differs (not (equal? (hard-part a) (hard-part b))))
                 (c (commit-with s a)))
            (loop (+ k 1) (if differs (+ changed 1) changed)
                  (if (and differs (not (refused? c))) (cons (list k (st-path s)) violations) violations))))))))
(want "C3d on 200 random stores: every operation that changed the hard material refuses the old receipt"
      (cadr c3d)
      '())
(want "C3d CONTROL: the operations changed the hard material in at least 40 of the 200 cases"
      (>= (car c3d) 40)
      #t)

;; ---- C4: the budget -------------------------------------------------------------------------------------
;;
;; A TOKEN IS FOUR BYTES of the answer as it is rendered on the wire, the cut
;; clause left out; usable is nine tenths of the budget. Measured on the
;; bytes the caller receives: here the dispatcher's answer rendered by the
;; wire printer the daemon writes with, and below (C10) the bytes read back
;; from a daemon.
(define (without-cut a) (filter (lambda (c) (not (and (pair? c) (eq? (car c) 'cut)))) a))
(define (wire-bytes a) (bytevector-length (string->utf8 (render-wire (without-cut a)))))
(define (within-usable? a budget) (<= (* 10 (wire-bytes a)) (* 36 budget)))
;; T depends-on thirty members with long titles: at a small budget most of
;; them cannot be shown.
(define c4s (fresh-store!))
(define c4T (make! c4s "root" "Budget task" "kind" "task"))
(define c4-members
  (let loop ((k 0) (acc '()))
    (if (= k 30) (reverse acc)
        (let ((m (make! c4s "root" (string-append (format "member ~a " k) (make-string 400 #\m)))))
          (link! c4s c4T "depends-on" m)
          (loop (+ k 1) (cons m acc))))))
(define c4-minimum-read (let ((a (ctx c4s c4T 1))) (and (pair? a) (eq? (car a) 'error) (clause a 'minimum) (cadr (clause a 'minimum)))))
;; a stand-in where there is no reading (red first), so the rows still run
(define c4-minimum (if (integer? c4-minimum-read) c4-minimum-read 1000))
(want "C4 below the reservation: budget-too-small with the minimum, and no sections"
      (let ((a (ctx c4s c4T 1)))
        (in-order (list (car a) (cadr a)) (integer? c4-minimum-read) (section a 'constraints)))
      '((error budget-too-small) #t #f))
(want "C4 the minimum is exact: one token less is too small, the minimum answers ok"
      (in-order (cadr (ctx c4s c4T (- c4-minimum 1))) (car (ctx c4s c4T c4-minimum)))
      '(budget-too-small ok))
(define (missing-of a) (let ((i (clause a 'insufficient))) (if i (map car (cadr (assq 'missing (cdr i)))) '())))
(define (more-of a) (let ((i (clause a 'insufficient))) (and i (let ((m (assq 'more (cdr i)))) (and m (cadr m))))))
(define (entered-hard a) (append (list (for-id a)) (apply append (map (lambda (n) (ids-in a n)) hard-sections))))
(define c4-ladder (map (lambda (k) (+ c4-minimum (* k 150))) '(0 1 2 3 5 8 13 21 34 55)))
(define c4-answers (map (lambda (b) (ctx c4s c4T b)) c4-ladder))
(want "C4 at the minimum: twenty named under insufficient and (more 10 ...): thirty members do not fit"
      (let ((a (car c4-answers))) (in-order (length (missing-of a)) (more-of a)))
      '(20 10))
(want "C4 IN EVERY ANSWER: the size without the cut clause is within usable, measured on the rendered bytes"
      (map (lambda (a b) (within-usable? a b)) c4-answers c4-ladder)
      (map (lambda (b) #t) c4-ladder))
(want "C4 in every answer: the hard blocks entered and the ones named missing are disjoint, and the receipt premises every hard block"
      (map (lambda (a)
             (let ((entered (entered-hard a)) (missing (missing-of a))
                   (premised (map cadr (filter (lambda (p) (string? (cadr p))) (receipt-premises a)))))
               (list (null? (filter (lambda (m) (member m entered)) missing))
                     (equal? (sorted premised) (sorted (hard-ids c4s c4T))))))
           c4-answers)
      (map (lambda (b) '(#t #t)) c4-ladder))
(want "C4 in every answer: entered plus missing plus the count under more is the whole hard set"
      (map (lambda (a) (= (+ (length (entered-hard a)) (length (missing-of a)) (or (more-of a) 0))
                          (length (hard-ids c4s c4T))))
           c4-answers)
      (map (lambda (b) #t) c4-ladder))
(want "C4 a later hard block that fits is entered although an earlier one did not: the last member, given a short title"
      (begin
        (run c4s 'set (list-ref c4-members 29) "title" "short")
        (let ((a (ctx c4s c4T (+ c4-minimum 150))))
          (in-order (and (member (list-ref c4-members 29) (entered-hard a)) #t)
                    (and (member (list-ref c4-members 0) (missing-of a)) #t))))
      '(#t #t))
;; THE EDIT ABOVE MOVED A PREMISE OF T: T now needs review, its entry carries
;; the reason, and the reservation, which holds T at identity, grows. The
;; rows below take the minimum again rather than the one read before it.
(define c4-minimum-after
  (let ((a (ctx c4s c4T 1))) (if (and (clause a 'minimum) (integer? (cadr (clause a 'minimum)))) (cadr (clause a 'minimum)) 1000)))
(want "C4 with a large budget every hard block is entered in full and nothing is insufficient"
      (let ((a (ctx c4s c4T 1000000)))
        (in-order (clause a 'insufficient)
                  (unique (map level-of (append (list (section a 'for))
                                                (apply append (map (lambda (n) (section a n)) hard-sections)))))))
      '(#f (full)))
;; THE UPGRADES FOLLOW HARD ORDER (B6): one pass in hard order, each to full
;; when it fits, else to summary. Among members of one size (the shortened
;; one left out), the levels never rise along hard order: full, then
;; summary, then identity.
(define (rank l) (case l ((full) 2) ((summary) 1) ((identity) 0) (else -1)))
(want "C4 with room for some upgrades, they go to the first blocks in hard order"
      (let* ((b (ctx c4s c4T (+ c4-minimum-after 2000)))
             (levels (map level-of (filter (lambda (e) (and (member (car e) c4-members)
                                                            (not (equal? (car e) (list-ref c4-members 29)))))
                                           (or (section b 'constraints) '())))))
        (in-order (and (pair? levels) (not (eq? (car levels) 'identity)) (memq 'identity levels) #t)
                  (let loop ((l (map rank levels)))
                    (or (null? l) (null? (cdr l)) (and (>= (car l) (cadr l)) (loop (cdr l)))))))
      '(#t #t))
;; THE CUT DOES NOT DECIDE: an unrelated write that lengthens the cut by a
;; digit (the writer's count crossing a power of ten) leaves the hard
;; sections the same, at EVERY budget from the minimum to four hundred tokens
;; above it: a reservation that held the cut would move each comparison by a
;; byte, and among that many consecutive budgets one decision would flip.
(define c4-small (let loop ((k 400) (acc '())) (if (< k 0) acc (loop (- k 1) (cons (+ c4-minimum-after k) acc)))))
(define c4-before (map (lambda (b) (hard-part (ctx c4s c4T b))) c4-small))
(define (writer-count s) (cdr (assoc (st-writer s) (reduce-applied-cut (state-of s)))))
;; Writes until the writer's count is all nines, then the one that adds a digit.
(let* ((n (writer-count c4s)) (target (- (expt 10 (string-length (number->string n))) 1)))
  (let loop ()
    (when (< (writer-count c4s) target)
      (make! c4s "root" "unrelated")
      (loop))))
(define c4-count-before (writer-count c4s))
(make! c4s "root" "the write that adds a digit")
(want "C4 after an unrelated write that lengthens the cut by a digit, the hard sections are the same at every budget from the minimum to 400 above"
      (in-order (string-length (number->string c4-count-before)) (string-length (number->string (writer-count c4s)))
                (filter values (map (lambda (b before) (and (not (equal? before (hard-part (ctx c4s c4T b)))) b)) c4-small c4-before)))
      (list (string-length (number->string c4-count-before)) (+ 1 (string-length (number->string c4-count-before))) '()))

;; ---- C5: order --------------------------------------------------------------------------------------------

;; H is a member under INNER under OUTER; OUTER was made first, so the nearer
;; ancestor has the larger id. At the first budget where exactly one of the
;; two ancestors is entered, it is the nearer.
(define c5s (fresh-store!))
(define c5T (make! c5s "root" "Zq order task" "kind" "task"))
(define OUTER (make! c5s "root" "outer ancestor"))
(define INNER (make! c5s OUTER "inner ancestor"))
(define H5 (make! c5s INNER "the member"))
(link! c5s c5T "depends-on" H5)
(define c5-min (let ((c (clause (ctx c5s c5T 1) 'minimum))) (if c (cadr c) 1000)))
(define (ancestors-in a) (filter (lambda (id) (member id (list OUTER INNER))) (or (and (list? (section a 'background)) (ids-in a 'background)) '())))
(want "C5 two ancestors of one hard block whose ids sort against their distance: when one fits, it is the nearer"
      (let loop ((b c5-min))
        (if (> b (+ c5-min 4000))
            'NEVER-EXACTLY-ONE
            (let ((in (ancestors-in (ctx c5s c5T b))))
              (if (= 1 (length in)) in (loop (+ b 2))))))
      (list INNER))
(want "C5 with room for both, the nearer is first"
      (ancestors-in (ctx c5s c5T 100000))
      (list INNER OUTER))
;; Two members of class inference, N1 then N2 (so N1 is first in hard order);
;; DA depends-on N2 and was made first, DB depends-on N1.
(define N1 (make! c5s "root" "inference member one" "class" "inference"))
(define N2 (make! c5s "root" "inference member two" "class" "inference"))
(define DA (make! c5s "root" "rests on two"))
(define DB (make! c5s "root" "rests on one"))
(link! c5s c5T "depends-on" N1)
(link! c5s c5T "depends-on" N2)
(link! c5s DA "depends-on" N2)
(link! c5s DB "depends-on" N1)
(want "C5 two dependents of two non-authoritative members: by the member's hard order, not by their own ids"
      (let ((a (ctx c5s c5T 100000)))
        (in-order (filter (lambda (id) (member id (list DA DB))) (ids-in a 'background))
                  (whys (entry-of a DB)) (whys (entry-of a DA))))
      (list (list DB DA) (list (list 'why 'rests-on N1)) (list (list 'why 'rests-on N2))))
(want "C5 the same request twice answers the same bytes"
      (equal? (render-wire (ctx c5s c5T 3000)) (render-wire (ctx c5s c5T 3000)))
      #t)
(define c5-before-snapshot (render-wire (without-cut (ctx c5s c5T 3000))))
(want "C5 the same bytes from a state seeded by a snapshot"
      (in-order (car (run c5s 'snapshot)) (equal? c5-before-snapshot (render-wire (without-cut (ctx c5s c5T 3000)))))
      '(ok #t))

;; ---- C6: definitions ------------------------------------------------------------------------------------

;; Datum libraries: (ctxa) defines fa, which uses fz; (ctxb) imports (ctxa)
;; and defines gb, which uses fa. T depends-on gb's block.
(define (lib-store! files)
  (let* ((s (fresh-store!)) (dir (string-append (st-path s) "-src")))
    (system (string-append "mkdir -p " (quoted dir)))
    (for-each (lambda (f) (write-file! (string-append dir "/" (car f)) (cdr f))) files)
    (run s 'import-code dir "--datum")
    s))
(define (def-id s name) (let ((r (query-rows s `(def ?d ?lib ,name)))) (and (pair? r) (pair? (car r)) (caar r))))
(define lib-a "(library (ctxa) (export fa fz) (import (rnrs))\n(define (fz) 0)\n(define (fa) (fz)))\n")
(define lib-b "(library (ctxb) (export gb) (import (rnrs) (ctxa))\n(define (gb) (fa)))\n")
(define c6s (lib-store! (list (cons "a.sc" lib-a) (cons "b.sc" lib-b))))
(define c6T (make! c6s "root" "Definitions task" "kind" "task"))
(define GB (def-id c6s "gb"))
(define FA (def-id c6s "fa"))
(define FZ (def-id c6s "fz"))
(link! c6s c6T "depends-on" GB)
(define c6a (ctx c6s c6T 100000))
(want "C6 a code constraint's used name brings its definition from an imported library, why defines; not transitively"
      (in-order (and GB FA FZ #t) (places c6a FA) (whys (entry-of c6a FA)) (places c6a FZ))
      (list #t '(background) (list (list 'why 'defines "fa" GB)) '()))
(want "C6 (name-use syntactic) when name use was consulted"
      (clause c6a 'name-use)
      '(name-use syntactic))
;; Whether a string occurs anywhere in a datum.
(define (mentions-string? x str) (or (equal? x str) (and (pair? x) (or (mentions-string? (car x) str) (mentions-string? (cdr x) str)))))
(define lib-c "(library (ctxc) (export fa) (import (rnrs))\n(define (fa) 2))\n")
(define lib-b2 "(library (ctxb) (export gb) (import (rnrs) (ctxa) (ctxc))\n(define (gb) (fa)))\n")
(define c6s2 (lib-store! (list (cons "a.sc" lib-a) (cons "c.sc" lib-c) (cons "b.sc" lib-b2))))
(define c6T2 (make! c6s2 "root" "Ambiguous task" "kind" "task"))
(link! c6s2 c6T2 "depends-on" (def-id c6s2 "gb"))
(want "C6 two candidates: both brought, each with (ambiguous) in its why, and no nogood"
      (let* ((a (ctx c6s2 c6T2 100000))
             (fas (map car (query-rows c6s2 '(def ?d ?lib "fa")))))
        (in-order (length fas)
                  (map (lambda (d) (and (member 'background (places a d))
                                        (exists (lambda (w) (member '(ambiguous) (cdr w))) (whys (entry-of a d)))
                                        #t))
                       fas)
                  (section a 'notes)))
      '(2 (#t #t) ()))
;; Text code with no library: a use of k in javascript; k defined in
;; javascript and in python. Only the same language is brought.
(define c6s3 (fresh-store!))
(run c6s3 'batch "((insert root #f ((kind . code) (lang . \"javascript\") (title . \"k use\") (src . \"k()\")))
                  (insert root #f ((kind . code) (lang . \"javascript\") (title . \"k def js\") (name . k) (src . \"function k() { return 1 }\")))
                  (insert root #f ((kind . code) (lang . \"python\") (title . \"k def py\") (name . k) (src . \"def k(): return 1\"))))")
(define (titled s t) (find (lambda (id) (equal? (field-of s id 'title) t)) (state-block-ids (state-of s))))
(define c6T3 (make! c6s3 "root" "Text code task" "kind" "task"))
(link! c6s3 c6T3 "depends-on" (titled c6s3 "k use"))
(want "C6 text code brings the definition in its own language only"
      (let ((a (ctx c6s3 c6T3 100000)))
        (in-order (places a (titled c6s3 "k def js")) (places a (titled c6s3 "k def py"))))
      '((background) ()))

;; ---- C6b: implementers ----------------------------------------------------------------------------------

(define c6bs (fresh-store!))
(define c6bT (make! c6bs "root" "Implementer task" "kind" "task"))
(define c6bD (make! c6bs "root" "An open decision" "kind" "decision"))
(define c6bK (make! c6bs "root" "the implementing task" "kind" "task" "status" "todo"))
(link! c6bs c6bT "depends-on" c6bD)
(link! c6bs c6bK "implements" c6bD)
(run c6bs 'set c6bK "title" "the implementing task, edited after the link")
(want "C6b a todo task that implements an OPEN decision and was edited after its link is not hard"
      (let ((a (ctx c6bs c6bT 100000)))
        (in-order (filter (lambda (p) (memq p (cons 'for hard-sections))) (places a c6bK))
                  (whys (entry-of a c6bD))))
      (list '() (sorted '((why member) (why unsettled open)))))
(define c6bC (make! c6bs "root" "the code that discharges it" "kind" "code" "lang" "scheme"))
(link! c6bs c6bC "implements" c6bD)
(want "C6b the same pair on a decision a code block discharges: the decision is in review and the task is hard, why implementer"
      (let ((a (ctx c6bs c6bT 100000)))
        (in-order (filter (lambda (p) (memq p (cons 'for hard-sections))) (places a c6bK))
                  (whys (entry-of a c6bK))
                  (and (member (list 'why 'unsettled 'review) (whys (entry-of a c6bD))) #t)))
      (list '(constraints) (list (list 'why 'implementer c6bD 'source)) #t))

;; ---- C7: nogood -------------------------------------------------------------------------------------------

(define c7s (fresh-store!))
(define c7T (make! c7s "root" "Nogood task" "kind" "task"))
(define Da (make! c7s "root" "ruling a" "kind" "decision" "status" "done"))
(define Db (make! c7s "root" "ruling b" "kind" "decision" "status" "done"))
(link! c7s c7T "depends-on" Da)
(link! c7s c7T "depends-on" Db)
(link! c7s Da "conflicts-with" Db)
(want "C7 two ruling decisions in scope with conflicts-with: one note for the pair, and both stay constraints"
      (let ((a (ctx c7s c7T 100000)))
        (in-order (length (section a 'notes))
                  (let ((n (car (section a 'notes)))) (list (car n) (sorted (list (cadr n) (caddr n))) (cadddr n)))
                  (places a Da) (places a Db)))
      (list 1 (list 'nogood (sorted (list Da Db)) 'conflicts-with) '(constraints) '(constraints)))
(link! c7s Db "supersedes" Da)
(want "C7 after supersedes between them the note is gone and the superseded one is in to-verify"
      (let ((a (ctx c7s c7T 100000)))
        (in-order (section a 'notes) (places a Da) (places a Db)))
      '(() (to-verify) (constraints)))

;; ---- C7b: counterexamples -----------------------------------------------------------------------------------

(define c7bs (fresh-store!))
(define c7bT (make! c7bs "root" "Counterexample task" "kind" "task"))
(define c7bC (make! c7bs "root" "the constraint"))
(define c7bO (make! c7bs "root" "the refuted one"))
(link! c7bs c7bT "depends-on" c7bC)
(link! c7bs c7bC "refutes" c7bO)
(want "C7b a block a constraint refutes is a counterexample, why refuted-by"
      (let ((a (ctx c7bs c7bT 100000)))
        (in-order (places a c7bO) (whys (entry-of a c7bO))))
      (list '(counterexamples) (list (list 'why 'refuted-by c7bC))))
(run c7bs 'set c7bO "title" "the refuted one, edited after the refutation")
(want "C7b edited after the refutation it is in force again and is not one"
      (places (ctx c7bs c7bT 100000) c7bO)
      '())

;; ---- C8: --all-validity -----------------------------------------------------------------------------------

(define c8s (fresh-store!))
(define c8T (make! c8s "root" "Quokka task" "kind" "task"))
(define c8R (make! c8s "root" "Quokka task notes"))
(define c8F (make! c8s "root" "the refuter"))
(link! c8s c8F "refutes" c8R)
(want "C8 a refuted block relevant by score is in background, marked, only with the flag"
      (let ((plain (ctx c8s c8T 100000)) (all (ctx c8s c8T 100000 "--all-validity")))
        (in-order (places plain c8R) (places all c8R) (validity-of (entry-of all c8R))))
      '(() (background) refuted))
(want "C8 for, the hard sections, the notes and the receipt are byte-identical with and without the flag"
      (let ((plain (ctx c8s c8T 100000)) (all (ctx c8s c8T 100000 "--all-validity")))
        (equal? (render-wire (append (hard-part plain) (list (receipt-of plain))))
                (render-wire (append (hard-part all) (list (receipt-of all))))))
      #t)

;; ---- C9: any block ----------------------------------------------------------------------------------------

(want "C9 --for a section, a code block and a decision each answer"
      (map (lambda (n) (car (ctx c1s (c1-id n) 100000))) '(S C D2))
      '(ok ok ok))
(define gone (make! c1s "root" "to be deleted"))
(run c1s 'del gone)
(want "C9 an unknown id and a deleted one refuse as read does"
      (in-order (equal? (ctx c1s "zz.99" 4000) (run c1s 'read "zz.99"))
                (equal? (ctx c1s gone 4000) (run c1s 'read gone)))
      '(#t #t))

;; ---- C10: the routes, nothing written, loaded on dispatch, README ------------------------------------------

(define (cli-wire s . args)
  (let* ((out (string-append root "/cli.out"))
         (rc (system (string-append "THEOURGIA_LOCAL=1 scheme --script ../core.sc "
                                    (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                    "--store " (quoted (st-path s)) " --wire > " (quoted out) " 2>/dev/null < /dev/null"))))
    (if (= rc 0) (file-text out) (list 'FAILED rc (file-text out)))))
(define socket-dir (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket (string-append socket-dir "/cx-" (number->string (get-process-id)) ".sock"))
(define daemon-log (string-append root "/daemon.log"))
(define (sleep-ms n) (sleep (make-time 'time-duration (* n 1000000) 0)))
(define (start-daemon! s)
  (system (string-append "THEOURGIA_TRACE=1 scheme --script ../theourgiad.sc serve " (quoted (st-path s)) " --socket " (quoted socket)
                         " > " (quoted daemon-log) " 2>&1 < /dev/null &"))
  (let wait ((k 0))
    (cond ((file-exists? socket) 'up)
          ((> k 200) 'never-came-up)
          (else (sleep-ms 50) (wait (+ k 1))))))
;; EVERY DAEMON SERVING THE STORE: the pattern's [c] keeps it from matching
;; the shell that runs pgrep or pkill.
(define (store-daemons s)
  (let ((out (string-append root "/daemons.txt")))
    (system (string-append "pgrep -f " (quoted (string-append "theourgiad.s[c] serve " (st-path s) " ")) " > " (quoted out) " 2>/dev/null"))
    (length (filter (lambda (l) (> (string-length l) 0))
                    (let loop ((p (open-input-string (file-text out))) (acc '()))
                      (let ((l (get-line p))) (if (eof-object? l) acc (loop p (cons l acc)))))))))
(define (stop-daemon! s)
  (system (string-append "pkill -f " (quoted (string-append "theourgiad.s[c] serve " (st-path s) " ")) " 2>/dev/null"))
  (let wait ((k 0)) (when (and (> (store-daemons s) 0) (< k 100)) (sleep-ms 50) (wait (+ k 1)))))
(define (through-daemon s . args)
  (let* ((out (string-append root "/daemon-cli.out"))
         (rc (system (string-append "scheme --script ../core.sc " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                    "--store " (quoted (st-path s)) " --socket " (quoted socket) " --wire > " (quoted out) " 2>/dev/null < /dev/null"))))
    (if (= rc 0) (file-text out) (list 'FAILED rc))))
(define (served-contexts)
  (let ((t (file-text daemon-log)) (needle "(trace daemon-dispatch context"))
    (let loop ((i 0) (n 0))
      (cond ((> (+ i (string-length needle)) (string-length t)) n)
            ((string=? (substring t i (+ i (string-length needle))) needle) (loop (+ i 1) (+ n 1)))
            (else (loop (+ i 1) n))))))
;; THE BYTES RECEIVED, WITHOUT THE CUT CLAUSE: the clause is found in the
;; text by its opening and its matching close (a cut holds writer names and
;; counts, no string with a parenthesis), and taken out with the space
;; before it.
(define (text-without-cut t)
  (let ((n (string-length t)) (open " (cut "))
    (let find ((i 0))
      (cond ((> (+ i (string-length open)) n) t)
            ((string=? (substring t i (+ i (string-length open))) open)
             (let close ((j (+ i 1)) (depth 0))
               (cond ((>= j n) t)
                     ((char=? (string-ref t j) #\() (close (+ j 1) (+ depth 1)))
                     ((char=? (string-ref t j) #\))
                      (if (= depth 1)
                          (string-append (substring t 0 i) (substring t (+ j 1) n))
                          (close (+ j 1) (- depth 1))))
                     (else (close (+ j 1) depth)))))
            (else (find (+ i 1)))))))
(define (received-within-usable? text budget)
  (let ((body (let strip ((x text)) (if (and (> (string-length x) 0) (char=? (string-ref x (- (string-length x) 1)) #\newline))
                                         (strip (substring x 0 (- (string-length x) 1))) x))))
    (<= (* 10 (bytevector-length (string->utf8 (text-without-cut body)))) (* 36 budget))))
(define c10-digest-before (log-digest c4s))
(define c10-budgets (list c4-minimum-after (+ c4-minimum-after 7) (+ c4-minimum-after 150) 100000))
(define in-process (map (lambda (b) (render-wire (ctx c4s c4T b))) c10-budgets))
(want "C10 the command line (local route) answers the same bytes as the dispatcher"
      (map (lambda (b w) (let ((t (cli-wire c4s "context" "--for" c4T "--budget" (number->string b))))
                           (and (string? t) (string=? (text-without-cut t) (text-without-cut w)))))
           c10-budgets in-process)
      (map (lambda (b) #t) c10-budgets))
(define daemon-state (start-daemon! c4s))
(define served-before (served-contexts))
(define via-daemon (map (lambda (b) (through-daemon c4s "context" "--for" c4T "--budget" (number->string b))) c10-budgets))
(define served-after (served-contexts))
(stop-daemon! c4s)
(want "C10 through the daemon: the same bytes as the dispatcher"
      (in-order daemon-state
                (map (lambda (t w) (and (string? t) (string=? (text-without-cut t) (text-without-cut w)))) via-daemon in-process))
      (list 'up (map (lambda (b) #t) c10-budgets)))
(want "C10 CONTROL: the daemon served those requests -- none in its trace before, one per request after"
      (list served-before served-after)
      (list 0 (length c10-budgets)))
(want "C10 the bytes received from the daemon, without the cut clause, are within usable at every budget"
      (map (lambda (t b) (and (string? t) (received-within-usable? t b))) via-daemon c10-budgets)
      (map (lambda (b) #t) c10-budgets))
(want "C10 no daemon of the store outlives the routes"
      (store-daemons c4s)
      0)
(want "C10 nothing is written: the store's log is the same bytes after every request above"
      (equal? c10-digest-before (log-digest c4s))
      #t)
(define child (string-append root "/ondemand.sc"))
(write-file! child
  (string-append
    "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
    "(register-verbs! extension-verbs)\n"
    "(define (loaded?) (and (member '(theourgia context) (library-list)) #t))\n"
    (format "(define store ~s)\n" (st-path c4s))
    "(rpc-dispatch store '(search \"member\") \"test\")\n"
    "(define before (loaded?))\n"
    (format "(rpc-dispatch store '(context \"--for\" ~s \"--budget\" \"4000\") \"test\")\n" c4T)
    "(write (list before (loaded?)))\n"))
(want "C10 the library is absent until context is dispatched, and present after"
      (let ((out (string-append root "/ondemand.out")))
        (system (string-append "scheme --script " (quoted child) " > " (quoted out) " 2>/dev/null < /dev/null"))
        (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out)))))
      '(#f #t))
(want "C10 README names the verb, the placement table, the hard order and the receipt"
      (let ((t (file-text "../README.md")))
        (map (lambda (needle) (string-contains? t needle))
             '("context --for" "to-verify" "counterexamples" "budget-too-small" "insufficient" "(receipt")))
      '(#t #t #t #t #t #t))
(want "C10 the human rendering: each section's name and one line per entry"
      (let ((h (render-human (ctx c1s (c1-id 'T) 4000))))
        (map (lambda (needle) (string-contains? h needle)) (list "constraints" (c1-id 'note) (c1-id 'D2))))
      '(#t #t #t))

;; ---- C11: v400's experiment, as rows --------------------------------------------------------------------------
;;
;; Two writers. A (the store's own) takes context for a task that depends-on a
;; contract K; B edits K through a draft and a commit of its own; A's commit
;; with its receipt is refused naming K. W linked depends-on K before B's
;; edit needs review with the reason naming K; W2 linked after it is valid.
(define c11s (fresh-store!))
(define c11T (make! c11s "root" "A's task" "kind" "task"))
(define K11 (make! c11s "root" "the contract"))
(define W11 (make! c11s "root" "work linked before"))
(link! c11s c11T "depends-on" K11)
(link! c11s W11 "depends-on" K11)
(define c11a (ctx c11s c11T 100000))
(run c11s 'write K11 "B's new text of the contract" "--writer" "bwriter")
(define c11-b-commit (run c11s 'commit K11 "--writer" "bwriter"))
(define W211 (make! c11s "root" "work linked after"))
(link! c11s W211 "depends-on" K11)
(want "C11 B's edit committed; A's commit with its receipt is refused, naming K"
      (let ((c (commit-with c11s c11a)))
        (in-order (car c11-b-commit) (refused? c) (mentions-string? c K11)))
      '(ok #t #t))
(want "C11 W linked before the edit needs review, the reason naming K; W2 linked after is valid"
      (in-order (query-rows c11s `(validity-reason ,W11 ?why ?by)) (query-rows c11s `(validity ,W211 ?v)))
      (list (list (list 'premise-moved K11)) '((valid))))

;; ---- a library whose name is contested ------------------------------------------------------------------------
;;
;; The unit sits in a library whose name two writers set at once: no fact
;; speaks a name for it (part 3), and none of the five goals joins on a
;; library name, so the verb still answers. Built from events and handed to
;; the dispatcher as a supplied reduction, as the daemon hands one.
(define contested
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e))
              (list `("a" 1 () (put ((kind . library) (name . (qx one)) (title . "A"))))
                    `("a" 2 () (put ((kind . code) (lang . "scheme") (title . "in A") (src . "(define (u) (v))") (name . u) (parent . "a.1") (ord . 1))))
                    `("a" 3 () (put ((kind . section) (title . "a premise"))))
                    '("a" 4 () (link "a.2" depends-on "a.3"))
                    '("b" 1 (("a" . 4)) (set "a.1" name (qx two)))
                    '("c" 1 (("a" . 4)) (set "a.1" name (qx three)))))
    r))
(define contested-store (fresh-store!))
(want "the unit in a library whose name is contested: the verb answers, for is the block, the premise is a constraint"
      (let ((a (rpc-dispatch (st-path contested-store) '(context "--for" "a.2" "--budget" "100000") "test" contested #f #f #f)))
        (in-order (car a) (for-id a) (ids-in a 'constraints) (length (filter (lambda (p) (pair? (cadr p))) (receipt-premises a)))))
      '(ok "a.2" ("a.3") 5))

;; ---- a hard block that cannot be hashed --------------------------------------------------------------------
;;
;; A FIELD VALUE NESTED PAST WHAT THE HASH CAN RENDER, written by a record that
;; can: a block's hash renders the field one list deeper than a set record
;; carries it, so there is a depth the store writes and reads and the hash
;; refuses. The depth is found rather than assumed: from below the codec's
;; boundary upward, the first set that is written and whose block then has no
;; hash.
(define (nest n) (let loop ((k n) (v 1)) (if (= k 0) v (loop (- k 1) (list v)))))
(define codec-deepest
  (let loop ((n 1))
    (if (guard (e (#t #f)) (sexpr->string-extended (storable-encode (nest (+ n 1)))) #t)
        (if (> n 2000) n (loop (+ n 1)))
        n)))
(define (hash-or-false s id) (guard (e (#t #f)) (block-hash (state-of s) id)))
(define (deep-set! s id n) (run s 'batch (format "((set ~s deep ~s))" id (nest n))))
;; -> the depth, or #f when no set below the codec's refusal leaves its block
;; without a hash.
(define (unhashable-depth s probe)
  (let loop ((n (max 1 (- codec-deepest 12))))
    (let ((a (deep-set! s probe n)))
      (cond ((not (and (pair? a) (not (eq? (car a) 'error)))) #f)
            ((not (hash-or-false s probe)) n)
            ((> n (+ codec-deepest 12)) #f)
            (else (loop (+ n 1)))))))
;; Da a member of T, Db a member through Da; Db made first, so the ids sort
;; against the hard order.
(define uh-s (fresh-store!))
(define uh-depth (unhashable-depth uh-s (make! uh-s "root" "depth probe")))
(define uh-T (make! uh-s "root" "Unhashable task" "kind" "task"))
(define uh-Db (make! uh-s "root" "the deeper one"))
(define uh-Da (make! uh-s "root" "the nearer one"))
(link! uh-s uh-T "depends-on" uh-Da)
(link! uh-s uh-Da "depends-on" uh-Db)
(when uh-depth (deep-set! uh-s uh-Da uh-depth) (deep-set! uh-s uh-Db uh-depth))
(want "SETUP a depth the store writes and the hash refuses exists, and both blocks are read with no hash"
      (in-order (integer? uh-depth)
                (and (state-read (state-of uh-s) uh-Da) #t) (hash-or-false uh-s uh-Da)
                (and (state-read (state-of uh-s) uh-Db) #t) (hash-or-false uh-s uh-Db))
      '(#t #t #f #t #f))
(want "a hard block that cannot be hashed: the answer is the refusal naming every such block, in hard order, and nothing else"
      (ctx uh-s uh-T 100000)
      (list 'error 'unhashable-hard-block (list 'id uh-Da uh-Db)))

;; ---- a writer that cannot be read: the clause the dispatcher appends --------------------------------------
;;
;; A SECOND WRITER whose segment cannot be read -- a writer of another store,
;; its directory copied in (a commit with another --writer is recorded under
;; the store's own writer, so it makes no second segment): the dispatcher appends
;; (incomplete ...) to every answer built from that state, and the budget
;; counts it. At budgets at the edge, from the minimum up: the bytes received,
;; without the cut, are within usable; and the verb's own `used` is the size of
;; exactly what was received, so the clause it counted is the clause that was
;; appended. The titles hold characters outside ASCII and a tab, where two
;; printers can disagree.
(define (writer-dirs s)
  (let ((out (string-append root "/writer-dirs.txt")))
    (system (string-append "ls " (quoted (string-append (st-path s) "/writers")) " > " (quoted out)))
    (let loop ((p (open-input-string (file-text out))) (acc '()))
      (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc)))))))
(define ia-s (fresh-store!))
(define ia-T (make! ia-s "root" "T\x2014;ache \x00e9;crite\tavec \x2713;" "kind" "task"))
(define ia-members
  (map (lambda (k) (let ((m (make! ia-s "root" (format "membre ~a \x00e9;t\x00e9; \x2014; \x03bb;\tfin" k))))
                     (link! ia-s ia-T "depends-on" m)
                     m))
       '(0 1 2 3 4 5)))
(define ia-dirs-before (writer-dirs ia-s))
(define ia-second (fresh-store!))
(define ia-b-commit (run ia-second 'insert "--title" "written by the second writer"))
(system (string-append "cp -R " (quoted (string-append (st-path ia-second) "/writers/" (st-writer ia-second))) " "
                       (quoted (string-append (st-path ia-s) "/writers/"))))
(define ia-new-dirs (filter (lambda (d) (not (member d ia-dirs-before))) (writer-dirs ia-s)))
(define (ia-chmod! mode)
  (for-each (lambda (d) (system (string-append "chmod " mode " " (quoted (string-append (st-path ia-s) "/writers/" d)) "/*.sexp")))
            ia-new-dirs))
(ia-chmod! "000")
(define ia-min (let ((c (clause (ctx ia-s ia-T 1) 'minimum))) (if (and c (integer? (cadr c))) (cadr c) 1000)))
(define ia-budgets (map (lambda (k) (+ ia-min k)) '(0 1 2 3 5 8 13 21 34 55 89 144 233)))
(define ia-answers (map (lambda (b) (ctx ia-s ia-T b)) ia-budgets))
(ia-chmod! "644")
(define (received-without-cut-bytes a)
  (bytevector-length (string->utf8 (text-without-cut (render-wire a)))))
(define (used-of a) (let ((b (clause a 'budget))) (and b (let ((u (assq 'used (cdr b)))) (and u (cadr u))))))
(want "SETUP the second writer's directory is in the store, and each answer carries the incomplete clause"
      (in-order (car ia-b-commit) (length ia-new-dirs)
                (map (lambda (a) (and (ok? a) (clause a 'incomplete) #t)) ia-answers))
      (list 'ok 1 (map (lambda (b) #t) ia-budgets)))
(want "the incomplete clause counted: at every budget from the minimum, the received bytes without the cut are within usable"
      (map (lambda (a b) (and (ok? a) (<= (* 10 (received-without-cut-bytes a)) (* 36 b)))) ia-answers ia-budgets)
      (map (lambda (b) #t) ia-budgets))
(want "the clause counted is the clause appended: the verb's used is the size of what was received, in tokens"
      (map (lambda (a) (and (ok? a) (equal? (used-of a) (div (+ (received-without-cut-bytes a) 3) 4)))) ia-answers)
      (map (lambda (b) #t) ia-budgets))

;; ---- the third review's findings: a row each -------------------------------------------------------------

;; A PARENT THAT IS NOT IN THE STATE: a reduction holding a live T whose
;; parent id no record made -- the reducer accepts any well-formed parent id,
;; as a record from elsewhere can carry one -- handed to the dispatcher as the
;; daemon hands one. (Built through the verbs it cannot be: a block inserted
;; under another writer's block depends on that writer, and is pending with
;; it.) The answer is ok, at every budget from the minimum up, and names no
;; ancestor that is not a block.
(define mp-state
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e))
              (list `("a" 1 () (put ((kind . task) (title . "Task under a missing parent") (parent . "gone.1") (ord . 1))))
                    `("a" 2 () (put ((kind . section) (title . "its premise"))))
                    '("a" 3 () (link "a.1" depends-on "a.2"))))
    r))
(define mp-s (fresh-store!))
(define mp-P "gone.1")
(define (mp-ctx b)
  (rpc-dispatch (st-path mp-s) (list 'context "--for" "a.1" "--budget" (number->string b)) "test" mp-state #f #f #f))
(define mp-min-answer (mp-ctx 1))
(define mp-min (let ((c (clause mp-min-answer 'minimum))) (if (and c (integer? (cadr c))) (cadr c) 1000)))
(define mp-answers (map (lambda (k) (mp-ctx (+ mp-min k))) '(0 1 5 21 89 400 100000)))
(want "SETUP the reduction holds T under a parent id, and no block for that id"
      (in-order (and (state-read mp-state "a.1") #t) (state-read mp-state mp-P))
      '(#t #f))
(want "a T whose parent is not in the state: budget-too-small names a minimum, and from it every answer is ok"
      (in-order (list (car mp-min-answer) (cadr mp-min-answer)) (map car mp-answers))
      (list '(error budget-too-small) (map (lambda (a) 'ok) mp-answers)))
(want "a T whose parent is not in the state: no answer enters that parent (T's own record still says where it sits)"
      (map (lambda (a) (exists (lambda (n) (let ((es (section a n))) (and (list? es) (member mp-P (map car es)) #t)))
                               '(constraints evidence to-verify counterexamples background)))
           mp-answers)
      (map (lambda (a) #f) mp-answers))

;; THE RECEIPT AS PRINTED: commit --premises given the clause exactly as the
;; answer's printed form spells it -- the clause's own text cut out of the
;; rendered answer, not rebuilt -- is accepted.
(define (clause-text t head)
  (let* ((open (string-append "(" head " ")) (n (string-length t)))
    (let find ((i 0))
      (cond ((> (+ i (string-length open)) n) #f)
            ((string=? (substring t i (+ i (string-length open))) open)
             (let close ((j i) (depth 0) (in-string #f) (escaped #f))
               (cond ((>= j n) #f)
                     (in-string (close (+ j 1) depth
                                       (not (and (not escaped) (char=? (string-ref t j) #\")))
                                       (and (not escaped) (char=? (string-ref t j) #\\))))
                     ((char=? (string-ref t j) #\") (close (+ j 1) depth #t #f))
                     ((char=? (string-ref t j) #\() (close (+ j 1) (+ depth 1) #f #f))
                     ((char=? (string-ref t j) #\))
                      (if (= depth 1) (substring t i (+ j 1)) (close (+ j 1) (- depth 1) #f #f)))
                     (else (close (+ j 1) depth #f #f)))))
            (else (find (+ i 1)))))))
(want "the receipt clause exactly as the answer prints it is accepted by commit --premises"
      (let* ((a (ctx c1s (c1-id 'T) 100000))
             (printed (clause-text (render-wire a) "receipt")))
        (in-order (and (string? printed) (string=? (substring printed 0 9) "(receipt ") #t)
                  (car (run c1s 'commit "--writer" (st-writer c1s) "--premises" (or printed "()")))))
      '(#t ok))

;; A TEXT-MODE CODE BLOCK'S SUMMARY: text mode stores the source as bytes. A
;; hard one, at the first budget where it is entered at summary level, shows
;; its first line: the second line runs past byte 240, so the summary is cut
;; back to the end of the first.
(define tm-s (fresh-store!))
(define tm-dir (string-append root "/textmode"))
(define tm-first (string-append "// the first line of the reader " (make-string 160 #\r)))
(system (string-append "mkdir -p " (quoted tm-dir)))
(write-file! (string-append tm-dir "/reader.js")
             (string-append tm-first "\n" "function reader() { return 1 } // " (make-string 80 #\x) "\n"
                            (apply string-append (map (lambda (k) (format "// filler ~a ~a\n" k (make-string 60 #\f))) (iota 60)))))
(run tm-s 'import-code tm-dir)
(define (bytevector-slice-head bv n) (let ((out (make-bytevector n))) (bytevector-copy! bv 0 out 0 n) out))
(define tm-block
  (find (lambda (id) (let ((v (field-of tm-s id 'src)))
                       (and (bytevector? v) (>= (bytevector-length v) 20)
                            (string=? (utf8->string (bytevector-slice-head v 20)) (substring tm-first 0 20)))))
        (state-block-ids (state-of tm-s))))
(define tm-T (make! tm-s "root" "Text mode task" "kind" "task"))
(when tm-block (link! tm-s tm-T "depends-on" tm-block))
(define tm-min (let ((c (clause (ctx tm-s tm-T 1) 'minimum))) (if (and c (integer? (cadr c))) (cadr c) 1000)))
(define tm-summary
  (let loop ((b tm-min))
    (if (> b (+ tm-min 6000))
        'NEVER-AT-SUMMARY
        (let ((e (entry-of (ctx tm-s tm-T b) tm-block)))
          (if (eq? (level-of e) 'summary) (sub e 'summary) (loop (+ b 10)))))))
(want "SETUP text mode stored the source as bytes"
      (bytevector? (and tm-block (field-of tm-s tm-block 'src)))
      #t)
(want "a hard text-mode code block at summary level shows its first line"
      tm-summary
      (list 'summary tm-first))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\ncontext-verb complete\n" rows bad)
(exit (if (= bad 0) 0 1))
