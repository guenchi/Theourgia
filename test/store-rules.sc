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

;; RULES OF THE STORE, STORED AND LISTED; BACK-REFERENCES AT AN EDGE'S ENDS.
;;
;; Two things a later build judges writes with, here only written, kept and
;; read back: a batch can create a block and link it in one request, and a
;; store can hold rules -- declared, checked when declared, kept as
;; candidates of their names, listed by describe, check and conflicts. A
;; write judged by the rules is test/rule-check.sc's.

(import (chezscheme) (theourgia rpc)
        (only (theourgia reduce) block-id reduce-applied-cut state-hash
              state-edges state-read state-datum state->rows reduce-empty reduce-apply! reduce-gates)
        (only (theourgia request) intent-produced?)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia store) open-and-reduce with-store-write make-write-request)
        (only (theourgia wire) encode-record storable-encode sexpr->string-extended)
        (only (theourgia log) log-publish! segment-sha writer-directory)
        (only (theourgia query) fact-relations))

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
;; EVERY TOP-LEVEL VALUE AND STEP IS COMPUTED UNDER A GUARD, but the paths
;; and the closing lines: on a tree without this change, or with a store that
;; answers wrongly, a top-level form that raises would stop the file before
;; its rows; under the guard it is (RAISED ...), and the rows that read it fail.
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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/store-rules-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define S (string-append root "/s"))
(define (run . args) (rpc-dispatch S args "author"))
(define (state) (open-and-reduce S))
(define (events) (apply + (map cdr (reduce-applied-cut (state)))))
;; The bytes of every log file this store's writer has: a refused write
;; appends nothing, whether or not the reduction would apply it.
(define (log-bytes store w)
  (let ((dir (writer-directory store w)))
    (apply + (map (lambda (f) (bytevector-length (call-with-port (open-file-input-port (string-append dir "/" f)) get-bytevector-all)))
                  (filter (lambda (f) (and (> (string-length f) 5)
                                           (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))))
                          (directory-list dir))))))
(define (new-id a)
  (let* ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (item-ids a) (map new-id (cadr a)))
(define (one a key) (let ((c (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) key))) (cdr a))))) (and c (cadr c))))
(define (edges id) (cdr (assq 'edges (state-read (state) id))))
(define (conflict-items) (let ((a (run 'conflicts))) (cdr (assq 'items (cdr a)))))
;; THE NAMES THIS CHANGE ADDS ARE LOOKED UP WHEN THEY ARE CALLED, not
;; imported: on a tree without them the rows still run and say what is
;; missing, instead of the whole file failing to load.
(define (state-declared-rules r) ((eval 'state-declared-rules (environment '(theourgia reduce))) r))
(define (relation-external? x) ((eval 'relation-external? (environment '(theourgia query))) x))
;; describe reads the declared tables from the state it is handed: in process
;; it is handed one.
(define (describe-of store) (cdr (rpc-dispatch store '(describe) "author" (open-and-reduce store))))

(tolerant (run 'init))
(define Y (tolerant (new-id (run 'insert "--title" "Y"))))
;; The store's own writer is in the cut once it has written a record.
(define writer (tolerant (car (car (reduce-applied-cut (state))))))

;; A PLAN'S MEMBER DECLARED UNDER AN EXPECT WRAPPER is the record of the intent
;; inside it. The insert row is about the tree before this change too: it read
;; such a member as not produced (no expect case in intent-produced?).
(want "B a member declared as (expect h (insert ...)) is the record of the insert, and not of another"
      (in-order (intent-produced? '(expect "h" (insert root #f ((kind . section) (title . "x"))))
                                  '(put ((kind . section) (title . "x") (parent . root) (ord . 1))))
                (intent-produced? '(expect "h" (insert root #f ((kind . section) (title . "x"))))
                                  '(put ((kind . section) (title . "y") (parent . root) (ord . 1)))))
      '(#t #f))
(want "B a member declared as (expect h (link ...)) is the record of the link, and not of another"
      (in-order (intent-produced? '(expect "h" (link "a" relates "b")) '(link "a" relates "b"))
                (intent-produced? '(expect "h" (link "a" relates "b")) '(link "a" relates "c")))
      '(#t #f))

(want "N a store with no rule hashes as before: the blocks' datum alone"
      (let ((st (state)))
        (equal? (state-hash st)
                (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended (storable-encode (state-datum st))))))))
      #t)

;; ---- back-references at a link's and an unlink's ends ------------------------------

(define b1b (tolerant (run 'batch (format "~s" (list '(insert root #f ((kind . section) (title . "X")))
                                           (list 'link '(from 0) 'relates Y)
                                           (list 'link Y 'relates '(from 0)))))))
(define X (tolerant (car (item-ids b1b))))
(want "B a batch creates a block and links it, from and to, through (from 0)"
      (in-order (car b1b) (edges X) (and (member (cons 'relates X) (edges Y)) #t))
      (list 'batch (list (cons 'relates Y)) #t))
(define b2 (tolerant (run 'batch (format "~s" (list '(insert root #f ((kind . section) (title . "Z")))
                                          (list 'link '(from 0) 'relates Y)
                                          (list 'unlink '(from 0) 'relates Y))))))
(want "B a link then an unlink of the same new block in one batch leaves no edge"
      (in-order (car b2) (edges (car (item-ids b2))))
      '(batch ()))
(want "B a back-reference at a link's end naming no insert is no-such-intent; one that is not a form is malformed"
      (in-order (let ((a (run 'batch (format "~s" (list (list 'link '(from 5) 'relates Y))))))
                  (list-head (car (cadr a)) 3))
                (let ((a (run 'batch (format "~s" (list '(insert root #f ((kind . section) (title . "W")))
                                                        (list 'link Y 'relates Y)
                                                        (list 'link '(from 1) 'relates Y))))))
                  (list-head (list-ref (cadr a) 2) 3))
                (let ((a (run 'batch (format "~s" (list (list 'link '(from) 'relates Y))))))
                  (list-head (car (cadr a)) 2)))
      (list '(error no-such-intent 5) '(error no-such-intent 1) '(error malformed-intent)))
(define (cursor) (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
(define tracked (format "~s" (list '(insert root #f ((kind . section) (title . "T")))
                                   (list 'link '(from 0) 'relates Y))))
(define c0 (tolerant (cursor)))
(define t1 (tolerant (run 'batch tracked "--req" "RULES-T1" "--cursor" c0)))
(define after-t1 (tolerant (events)))
(define t2 (tolerant (run 'batch tracked "--req" "RULES-T1" "--cursor" c0)))
(want "B a tracked batch with a link through (from 0): written once, and the same request again writes nothing"
      (in-order (car t1) (edges (car (item-ids t1))) (- (events) after-t1)
                (conflict-items))
      (list 'batch (list (cons 'relates Y)) 0 '()))

;; ---- the rule verb -------------------------------------------------------------------

(define before-rule (tolerant (events)))
(define r1 (tolerant (run 'rule "sections-titled" "--on" "section" "--must" "(title ?w ?t)")))
(want "R a rule is written as one record, in its one form, a state rule"
      (in-order (car r1) (- (events) before-rule) (state-declared-rules (state)))
      (list 'ok 1 '((sections-titled ((class state) (on section) (must (title ?w ?t)))))))
(define r2 (tolerant (run 'rule "reviews-carry" "--on" "doc" "--on" "section" "--where" "(field+ ?w \"slot\" \"review\")" "--must" "(receipt-carried)")))
(want "R a goal that names a fact only a rule check has makes a write rule; --on repeats; where comes before must"
      (in-order (car r2) (assq 'reviews-carry (state-declared-rules (state))))
      (list 'ok '(reviews-carry ((class write) (on doc section) (where (field+ ?w "slot" "review")) (must (receipt-carried))))))
(define n-before (tolerant (events)))
(want "R the same rule again answers unchanged and writes nothing"
      (in-order (run 'rule "sections-titled" "--on" "section" "--must" "(title ?w ?t)") (- (events) n-before))
      '((ok (unchanged)) 0))
(tolerant (run 'rule "sections-titled" "--on" "section" "--must-not" "(title ?w \"forbidden\")"))
(want "R another value replaces the one in force"
      (assq 'sections-titled (state-declared-rules (state)))
      '(sections-titled ((class state) (on section) (must-not (title ?w "forbidden")))))
(tolerant (run 'rule "cover" "--builtin" "citation-coverage"))
(want "R a built-in is enabled by name"
      (assq 'cover (state-declared-rules (state)))
      '(cover ((builtin citation-coverage))))
(want "R describe lists the rules in force; check skips each write rule, a built-in as one, audits the state rule (which holds here), verdict unchanged"
      (in-order (map car (cdr (assq 'declared-rules (describe-of S))))
                (cdr (assq 'rules (cdr (run 'check))))
                (cadr (assq 'verdict (cdr (run 'check)))))
      (list '(cover reviews-carry sections-titled)
            '((rule-skipped (rule cover) (reason write-rule))
              (rule-skipped (rule reviews-carry) (reason write-rule)))
            'ok))
(tolerant (run 'rule "cover" "--retire"))
(want "R a retired rule is no longer in force or listed"
      (in-order (map car (state-declared-rules (state)))
                (map car (cdr (assq 'declared-rules (describe-of S)))))
      '((reviews-carry sections-titled) (reviews-carry sections-titled)))

;; ---- what a rule may say ---------------------------------------------------------------

(define (refusal . args) (let ((a (apply run 'rule "r" args))) (if (pair? a) (list-head a (min 4 (length a))) a)))
(want "R the class is write exactly when a goal names a rule-only fact: in where alone, in must alone, inside an and"
      (map (lambda (args) (let ((a (apply run 'rule "cls" args)))
                            (and (pair? a) (eq? (car a) 'ok) (cadr (assq 'class (cadr (assq 'cls (state-declared-rules (state)))))))))
           '(("--on" "doc" "--where" "(kind+ ?w doc)" "--must" "(title ?w ?t)")
             ("--on" "doc" "--must-not" "(and (cited ?w ?s) (unread ?s))")
             ("--on" "doc" "--must" "(and (title ?w ?t) (kind ?w doc))")))
      '(write write state))
(want "R each rule-only fact alone makes a write rule, and a goal naming none a state rule"
      (map (lambda (g) (let ((a (run 'rule "cls" "--on" "doc" "--must" g)))
                         (and (pair? a) (eq? (car a) 'ok) (cadr (assq 'class (cadr (assq 'cls (state-declared-rules (state)))))))))
           '("(kind+ ?w doc)" "(field+ ?w \"a\" \"b\")" "(edge+ ?w cites ?x)" "(edge-kind+ ?w cites doc)"
             "(cited ?w ?s)" "(unread ?s)" "(receipt-carried)" "(title ?w ?t)"))
      '(write write write write write write write state))
(tolerant (run 'rule "cls" "--retire"))
(define before-refusals (tolerant (events)))
(define bytes-before-refusals (tolerant (log-bytes S writer)))
(want "R a goal naming a relation that reads outside the log is refused, as a fact and through a rule"
      (map (lambda (g) (refusal "--on" "code" "--must" g))
           '("(score ?w \"x\" ?s)" "(uses-name ?w ?n)" "(def ?w ?l ?n)" "(def-for ?w ?n ?d)" "(ambiguous ?w ?n)"))
      '((error bad-request rule-relation-not-allowed (relation score))
        (error bad-request rule-relation-not-allowed (relation uses-name))
        (error bad-request rule-relation-not-allowed (relation def))
        (error bad-request rule-relation-not-allowed (relation def-for))
        (error bad-request rule-relation-not-allowed (relation ambiguous))))
(want "R an unknown relation, a wrong arity, an unknown kind and an unknown built-in are refused"
      (in-order (list-head (refusal "--on" "section" "--must" "(nosuch ?w)") 3)
                (list-head (refusal "--on" "section" "--must" "(title ?w)") 3)
                (list-head (refusal "--on" "widget" "--must" "(title ?w ?t)") 3)
                (list-head (refusal "--builtin" "nosuch") 3))
      '((error bad-request unknown-relation) (error bad-request wrong-arity)
        (error bad-request kind-not-known) (error bad-request builtin-rule-not-known)))
(want "R a rule without --on, with both --must and --must-not, or with neither, answers usage"
      (map (lambda (args) (car (apply run 'rule "r" args)))
           '(("--must" "(title ?w ?t)") ("--on" "section" "--must" "(title ?w ?t)" "--must-not" "(title ?w ?t)")
             ("--on" "section")))
      '(usage usage usage))
(want "R a goal of #f, given as --must or as --where, is refused; so is a value holding a plan's marker"
      (in-order (list-head (refusal "--on" "doc" "--must" "#f") 3)
                (list-head (refusal "--on" "doc" "--where" "#f" "--must" "(title ?w ?t)") 3)
                (list-head (refusal "--on" "doc" "--must" "(member ?w (\"#%new\" 0))") 3))
      '((error bad-request not-a-goal) (error bad-request not-a-goal) (error bad-request (reason rule-value-holds-marker))))
(want "R a goal reading as the symbol absent or unreadable is a goal, and refused as one; so is a value that is not a finite tree"
      (in-order (list-head (refusal "--on" "doc" "--where" "absent" "--must" "(title ?w ?t)") 3)
                (list-head (refusal "--on" "doc" "--must" "unreadable") 3)
                (list-head (refusal "--on" "doc" "--must" "#0=(title . #0#)") 3))
      '((error bad-request not-a-goal) (error bad-request not-a-goal) (error bad-request rule-malformed)))
(want "R a value with shared structure is refused as one with a cycle is; a clause given twice is refused"
      (in-order (list-head (refusal "--on" "doc" "--must" "(and #0=(title ?w ?t) #0#)") 3)
                ((eval 'rule-value-check (environment '(theourgia query)))
                 '((on doc) (where (title ?w ?t)) (where (score ?w "x" ?s)) (must (title ?w ?u)))))
      '((error bad-request rule-malformed) (error bad-request rule-malformed)))
(want "R nothing was written by a refused rule: no record, the log as long as before, in records and in bytes"
      (in-order (assq 'r (state-declared-rules (state))) (- (events) before-refusals) (- (log-bytes S writer) bytes-before-refusals))
      '(#f 0 0))
(want "R the string \"#%new\" alone is data; a pair a plan would read as its marker is refused"
      (in-order (car (run 'rule "names-new" "--on" "doc" "--must" "(title ?w \"#%new\")"))
                (list-head (refusal "--on" "doc" "--must" "(title ?w \"#%new\" 0)") 3))
      '(ok (error bad-request (reason rule-value-holds-marker))))
(tolerant (run 'rule "names-new" "--retire"))
(want "R ?w is bound before a rule's goals are asked: a test may name it first; another variable a test names first is still refused"
      (in-order (car (run 'rule "test-on-w" "--on" "doc" "--where" "(= ?w \"x\")" "--must" "(title ?w ?t)"))
                (list-head (refusal "--on" "doc" "--must" "(= ?z \"x\")") 3))
      '(ok (error bad-request test-variable-unbound)))
(tolerant (run 'rule "test-on-w" "--retire"))
(define (batch-first intents) (car (cadr (run 'batch (format "~s" intents)))))
(want "R a rule intent in a batch is written in its one form, and refused with the form in another"
      (in-order (car (batch-first (list (list 'rule 'in-batch '((class state) (on section) (must (title ?w ?t)))))))
                (list-head (batch-first (list (list 'rule 'out-of-order '((must (title ?w ?t)) (on section))))) 3))
      '(ok (error bad-request rule-not-in-its-form)))
(define bytes-before-long (tolerant (log-bytes S writer)))
(want "R a rule intent with a part past its value is refused, not written shortened, and the log is as long as before"
      (in-order (list-head (batch-first (list (list 'rule 'long '((class state) (on section) (must (title ?w ?t))) 'extra))) 3)
                (- (log-bytes S writer) bytes-before-long))
      '((error malformed-intent (too-many-arguments (verb rule) (given 3) (needs 2))) 0))

;; ---- liveness: the candidate rule a declaration has ------------------------------------

;; A second writer's rule of the same name, concurrent with the local one:
;; its record depends on the local writer only up to before the local rule.
(define before-local (tolerant (cdr (assoc writer (reduce-applied-cut (state))))))
(tolerant (run 'rule "contested-one" "--on" "section" "--must" "(title ?w ?t)"))
(define forged (tolerant
  (encode-record 1 1789000000001 "peer" (list (cons writer before-local))
                 (storable-encode (list 'rule 'contested-one '((class state) (on doc) (must (title ?w ?t))))))))
(tolerant (log-publish! S "rulezzzz" 1 forged (segment-sha forged)))
(want "C two writers' concurrent rules of one name: contested, not in force, listed by conflicts with both"
      (in-order (assq 'contested-one (state-declared-rules (state)))
                (let ((c (find (lambda (x) (and (pair? x) (eq? (car x) 'rule-contested))) (conflict-items))))
                  (and c (list (cadr c) (length (cdr (caddr c)))))))
      '(#f (contested-one 2)))
(tolerant (run 'rule "contested-one" "--on" "decision" "--must" "(title ?w ?t)"))
(want "C a writer who has seen both declares again, and the rule is in force"
      (in-order (assq 'contested-one (state-declared-rules (state)))
                (find (lambda (x) (and (pair? x) (eq? (car x) 'rule-contested))) (conflict-items)))
      '((contested-one ((class state) (on decision) (must (title ?w ?t)))) #f))

;; ---- replay and snapshot keep the rules ----------------------------------------------

(define rules-before (tolerant (state-declared-rules (state))))
(define hash-before (tolerant (state-hash (state))))
(want "K a snapshot and the state read from it hold the same rules and the same hash"
      (in-order (car (run 'snapshot)) (equal? (state-declared-rules (state)) rules-before) (equal? (state-hash (state)) hash-before))
      '(ok #t #t))
(define S2 (string-append root "/s2"))
(system (string-append "cp -R '" S "' '" S2 "' && rm -rf '" S2 "/snap'"))
(want "K a replay of the log alone holds the same rules and the same hash"
      (in-order (equal? (state-declared-rules (open-and-reduce S2)) rules-before) (equal? (state-hash (open-and-reduce S2)) hash-before))
      '(#t #t))

;; ---- the stored/external marking, against the providers ------------------------------
;;
;; A fact is external when its provider reads outside the log: the name-use
;; table, the defs index, the keyword hook, the language catalogue or a
;; derived table. The providers are read from query.sc as data -- the arms
;; of `build` and of `fact-tuples` -- and the names they use compared with the
;; table's fourth column.
(define query-forms (tolerant
  (call-with-input-file "../query.sc"
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
(define (find-define forms name)
  (let walk ((x forms))
    (cond ((not (pair? x)) #f)
          ((and (eq? (car x) 'define) (pair? (cdr x)) (pair? (cadr x)) (eq? (car (cadr x)) name)) x)
          (else (or (walk (car x)) (walk (cdr x)))))))
(define (case-arms form)
  (let walk ((x form))
    (cond ((not (pair? x)) '())
          ((and (eq? (car x) 'case) (pair? (cdr x)) (eq? (cadr x) 'rel)) (cddr x))
          (else (append (walk (car x)) (walk (cdr x)))))))
(define (symbols-in x)
  (cond ((symbol? x) (list x)) ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x)))) (else '())))
(define outside-markers '("name-use" "defs-index" "keyword" "language" "derived" "view-read"))
(define (reads-outside? arm)
  (exists (lambda (sym)
            (let ((t (symbol->string sym)))
              (exists (lambda (m) (let ((n (string-length m)))
                                    (let loop ((i 0)) (cond ((> (+ i n) (string-length t)) #f)
                                                            ((string=? (substring t i (+ i n)) m) #t)
                                                            (else (loop (+ i 1)))))))
                      outside-markers)))
          (symbols-in (cdr arm))))
(define provider-arms (tolerant
  (append (case-arms (find-define query-forms 'build))
          (case-arms (find-define query-forms 'fact-tuples)))))
(define read-outside (tolerant
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (fold-left (lambda (acc x) (if (memq x acc) acc (cons x acc))) '()
                        (apply append (map (lambda (a) (if (and (pair? (car a)) (reads-outside? a)) (car a) '())) provider-arms))))))
(want "M the facts marked external are exactly those whose providers read outside the log"
      (in-order read-outside
                (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                           (map car (filter (lambda (f) (eq? (list-ref f 3) 'external)) fact-relations))))
      '((def score uses-name) (def score uses-name)))
(want "M CONTROL: every fact has a provider arm the census read, and the library's rules over them are external too"
      (in-order (filter (lambda (name) (not (exists (lambda (a) (and (pair? (car a)) (memq name (car a)))) provider-arms)))
                        (map car fact-relations))
                (map relation-external? '(def-for ambiguous scope-name depends title)))
      '(() (#t #t #f #f #f)))

;; ---- a store made from the project template holds no rule ---------------------------

(define P (string-append root "/project"))
(tolerant (rpc-dispatch P '(init "--template" "project") "author"))
(want "P a fresh project store has no rule record at all, retired or not, and describe and check list none"
      (in-order (length (filter (lambda (row) (eq? (car row) 'rule)) (state->rows (open-and-reduce P))))
                (assq 'declared-rules (describe-of P))
                (assq 'rules (cdr (rpc-dispatch P '(check) "author"))))
      '(0 #f #f))

;; ---- a rebuild keeps the rules --------------------------------------------------------
;;
;; As relation-effects RE-9 forces one: two records claiming one request's
;; identity; the first is applied, the second gates both, and the state is
;; folded again from the records.
(define (claim-actor) (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0)))
(define kept '((class state) (on doc) (must (title ?w ?t))))
(define rebuilt (tolerant
  (let ((r (reduce-empty)))
    (reduce-apply! r "decl0000" 1 '() (list 'rule 'kept kept))
    (reduce-apply! r "decl0000" 2 '() (list 'rule 'gone kept))
    (reduce-apply! r "decl0000" 3 '() '(rule gone retired))
    (reduce-apply! r "aaa00000" 1 '() (list 'rule 'claimed kept) (claim-actor))
    (reduce-apply! r "bbb00000" 1 '() (list 'rule 'claimed kept) (claim-actor))
    r)))
(want "K a rebuild after a retraction: both claims gated, a rule in force kept, a retired one still retired"
      (in-order (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates rebuilt)))) (and p (cdr p))))
                     '("aaa00000" "bbb00000"))
                (state-declared-rules rebuilt))
      (list '(plan-conflict plan-conflict) (list (list 'kept kept))))
(define (mentions? x y) (or (equal? x y) (and (pair? x) (or (mentions? (car x) y) (mentions? (cdr x) y)))))
(want "K after the rebuild the retired rule is still stored, as its retirement"
      (let ((row (find (lambda (row) (and (pair? row) (eq? (car row) 'rule) (pair? (cdr row)) (eq? (cadr row) 'gone)))
                       (state->rows rebuilt))))
        (and row (mentions? (cddr row) 'retired)))
      #t)

;; ---- a plan's declared references ------------------------------------------------------
;;
;; A plan holds each member as the run will read it: a well-formed (from k)
;; at a link's end becomes the plan's marker, a malformed one is kept as
;; written for the run to refuse, and a link too short for its ends is
;; still declared. A store of its own, so the plan left incomplete here
;; touches no other row.
(define S3 (string-append root "/s3"))
(tolerant (rpc-dispatch S3 '(init) "author"))
(define Y3 (tolerant (new-id (rpc-dispatch S3 '(insert "--title" "Y3") "author"))))
(define writer3 (tolerant (car (car (reduce-applied-cut (open-and-reduce S3))))))
(define planned
  (tolerant
    (with-store-write S3
      (lambda (state view)
        (list '(insert root #f ((kind . section) (title . "P")))
              (list 'link '(from 0 extra) 'relates Y3)
              (list 'link '(from) 'relates Y3)
              (list 'link '(from 0) 'relates Y3)
              (list 'link '(from 0) 'relates)))
      "author"
      (make-write-request "author" 'commit '() "RULESPLAN01"
                          (cons writer3 (cdr (assoc writer3 (reduce-applied-cut (open-and-reduce S3))))))
      #f #t)))
(define plan-entries
  (tolerant
    (let ((row (find (lambda (rec) (let ((p (list-ref rec 3))) (and (pair? p) (eq? (car p) 'plan))))
                     (cadr (assq 'request-history (state->rows (open-and-reduce S3)))))))
      (list-ref (list-ref row 3) 4))))
(want "B a plan keeps a malformed (from ...) at a link's end as written, marks a well-formed one, and declares a link too short for its ends"
      (map (lambda (k) (cdr (assv k plan-entries))) '(1 2 3 4))
      (list (list 'link '(from 0 extra) 'relates Y3)
            (list 'link '(from) 'relates Y3)
            (list 'link '("#%new" 0) 'relates Y3)
            '(link ("#%new" 0) relates)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nstore-rules complete\n" bad rows)
(exit (if (= bad 0) 0 1))
