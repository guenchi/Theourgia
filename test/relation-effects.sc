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

;; Relation effects declared as data: `relation <name> --as <kind>` binds a
;; name to one of the six effect kinds, or to nothing, by a record of the
;; store. The edge keeps its name; every reader of an effect reads its kind.
;; A differential pair of stores -- one shape, linked once with the six
;; names and once with six declared names -- must answer every reader the
;; same, modulo the name; a census pins where the six names are written in
;; the shipped sources; the declaration's admission, its table on every
;; route back (replay, rebuild, snapshot), and two writers' disagreement.
;;
;; The reducer's new procedures are reached by name at run time, so this
;; file loads on a tree without them and its rows read red there.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-empty reduce-apply! state-hash reduce-gates
              reduce-applied-cut state->rows rows->state block-id)
        (only (theourgia lifecycle) lifecycle lifecycle-fast? validity-of decision-state-of)
        (only (theourgia query) make-query-session session-answer session-spent)
        (only (theourgia wire) encode-record storable-encode))

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
;; A definition whose value may raise on a tree without the feature: the
;; raise is kept as the value, and the rows that read it go red.
(define-syntax define-caught
  (syntax-rules ()
    ((_ name e0) (define name (caught e0)))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/relation-effects-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define stores 0)
(define (fresh-store!)
  (set! stores (+ stores 1))
  (string-append root "/s" (number->string stores)))
(define (ask store . args) (rpc-dispatch store args "test"))
(define (writer-of init) (cadr (assq 'writer (cdr init))))
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev))))
    (block-id (car e) (cdr e))))
(define (items-of a)
  (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'items))
      (cdr (cadr a))
      '()))
(define (by-print xs) (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) xs))

;; The reducer's procedures of this feature, by name.
(define (reducer name) (eval name (environment '(theourgia reduce))))
(define (declared-table st) ((reducer 'state-declared-relations) st))
(define (declaration st name) ((reducer 'state-declaration) st name))

;; ---- the differential pair ----------------------------------------------------

(define declarations
  '(("cites" . "depends-on") ("answers" . "implements") ("obsoletes" . "supersedes")
    ("disproves" . "refutes") ("checks" . "verifies") ("clashes" . "conflicts-with")))
(define name->kind
  (map (lambda (d) (cons (string->symbol (car d)) (string->symbol (cdr d)))) declarations))
(define (declared-name kind) (car (find (lambda (d) (string=? (cdr d) kind)) declarations)))

;; ONE SHAPE, TWO SPELLINGS. Both stores declare the six names first, so
;; their records stay in step and a block has the same sequence in both;
;; NAME turns a kind into the relation each store links with.
;;   U  a section holding W, V, I and T: the unit context is asked for
;;   W  depends on P, which changes after the link, and on the decision X
;;   V  depends on C, which R refutes
;;   I  implements D and changes after the link; T, a task to do, too
;;   K  verifies D; S supersedes D0; X conflicts with Y
;;   Q  an inference, proposes to supersede Z
;; -> (store writer ids), ids an alist of key to id.
(define (build! name)
  (let* ((store (fresh-store!))
         (init (ask store 'init))
         (w (writer-of init))
         (ids '()))
    (define (id k) (cdr (assq k ids)))
    (define (ins! k title . under)
      (set! ids (cons (cons k (new-id (apply ask store 'insert "--title" title "--text" (string-append title " text")
                                             (if (pair? under) (list "--under" (id (car under))) '()))))
                      ids)))
    (define (link! a kind b) (ask store 'link (id a) (name kind) (id b)))
    (for-each (lambda (d) (ask store 'relation (car d) "--as" (cdr d))) declarations)
    (ins! 'U "Unit lantern")
    (ins! 'D "Codec decision lantern") (ask store 'set (id 'D) "kind" "decision")
    (ins! 'D0 "Old ruling lantern") (ask store 'set (id 'D0) "kind" "decision")
    (ins! 'S "New ruling lantern") (ask store 'set (id 'S) "kind" "decision")
    (ins! 'X "First clash lantern") (ask store 'set (id 'X) "kind" "decision")
    (ins! 'Y "Second clash lantern") (ask store 'set (id 'Y) "kind" "decision")
    (ins! 'P "Premise lantern")
    (ins! 'C "Claim lantern")
    (ins! 'R "Refuter lantern")
    (ins! 'K "Check lantern")
    (ins! 'Q "Proposal lantern") (ask store 'set (id 'Q) "class" "inference")
    (ins! 'Z "Proposed target lantern")
    (ins! 'W "Work lantern" 'U)
    (ins! 'V "Second work lantern" 'U)
    (ins! 'I "Implementation lantern" 'U)
    (ins! 'T "Task lantern" 'U) (ask store 'set (id 'T) "kind" "task") (ask store 'set (id 'T) "status" "todo")
    (link! 'W "depends-on" 'P)
    (link! 'W "depends-on" 'X)
    (link! 'V "depends-on" 'C)
    (link! 'R "refutes" 'C)
    (link! 'S "supersedes" 'D0)
    (link! 'I "implements" 'D)
    (link! 'T "implements" 'D)
    (link! 'K "verifies" 'D)
    (link! 'X "conflicts-with" 'Y)
    (link! 'Q "supersedes" 'Z)
    (ask store 'set (id 'P) "src" "Premise lantern text, changed")
    (ask store 'set (id 'I) "src" "Implementation lantern text, changed")
    (list store w (reverse ids))))

(define A (build! (lambda (kind) kind)))
(define B (build! declared-name))
(define (store-of x) (car x))
(define (writer x) (cadr x))
(define (id-in x k) (cdr (assq k (caddr x))))
(define keys (map car (caddr A)))

;; MODULO THE NAME: a store's writer and its block ids written without the
;; writer, a hash as H, a declared name as its kind, and a reason's
;; (relation <name>) clause dropped.
(define (hex64? s)
  (and (= (string-length s) 64)
       (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))
(define (relation-clause? e)
  (and (pair? e) (eq? (car e) 'relation) (pair? (cdr e)) (symbol? (cadr e)) (null? (cddr e))
       (assq (cadr e) name->kind) #t))
(define (norm x w)
  (cond ((string? x)
         (let ((n (string-length w)))
           (cond ((string=? x w) "W")
                 ((and (> (string-length x) (+ n 1)) (string=? (substring x 0 (+ n 1)) (string-append w ".")))
                  (string-append "W" (substring x n (string-length x))))
                 ((hex64? x) "H")
                 (else x))))
        ((symbol? x) (let ((e (assq x name->kind))) (if e (cdr e) x)))
        ((pair? x) (if (relation-clause? (car x)) (norm (cdr x) w) (cons (norm (car x) w) (norm (cdr x) w))))
        (else x)))

;; -> 'same when both stores answer alike modulo the name and the first
;; answer says something (INFORMATIVE?), else what each said.
(define (differential read informative?)
  (let ((a (read A)) (b (read B)))
    (let ((na (norm a (writer A))) (nb (norm b (writer B))))
      (cond ((not (equal? na nb)) (list 'differ na nb))
            ((not (informative? a)) (list 'says-nothing na))
            (else 'same)))))

(define (validities x)
  (let ((L (lifecycle (open-and-reduce (store-of x)))))
    (map (lambda (k) (cons k (validity-of L (id-in x k)))) keys)))
(define (some-not-valid? vs) (exists (lambda (v) (not (eq? (cadr v) 'valid))) vs))

;; ---- RE-1: a declared cites edge takes depends-on's effect ---------------------

(define-caught LB (lifecycle (open-and-reduce (store-of B))))
(want "RE-1 cites declared as depends-on: the citing block needs review when the cited block is refuted, and the reason names cites"
      (validity-of LB (id-in B 'V))
      (list 'needs-review (list 'premise-refuted (id-in B 'C) '(relation cites))))
(want "RE-1 and when the cited block moved past the edge"
      (validity-of LB (id-in B 'W))
      (list 'needs-review (list 'premise-moved (id-in B 'P) '(relation cites))))
(want "RE-1 CONTROL: the same edges under the six names give the reasons as before, with no relation clause"
      (validity-of (lifecycle (open-and-reduce (store-of A))) (id-in A 'V))
      (list 'needs-review (list 'premise-refuted (id-in A 'C))))
(want "RE-1 a read of the block carries the reason with the relation named"
      (let ((a (ask (store-of B) 'read (id-in B 'V))))
        (find (lambda (c) (and (pair? c) (eq? (car c) 'validity))) (cdr a)))
      (list 'validity 'needs-review (list 'premise-refuted (id-in B 'C) '(relation cites))))

;; ---- RE-2: every reader, the same modulo the name -----------------------------

(want "RE-2 validity of every block"
      (differential validities some-not-valid?)
      'same)
(want "RE-2 decision states"
      (differential (lambda (x)
                      (let ((L (lifecycle (open-and-reduce (store-of x)))))
                        (map (lambda (k) (cons k (decision-state-of L (id-in x k)))) '(D D0 S X Y))))
                    (lambda (ds) (exists (lambda (d) (and (pair? (cdr d)) (eq? (cadr d) 'review))) ds)))
      'same)
(want "RE-2 decision states CONTROL: the decision an implementer moved past is in review"
      (decision-state-of (lifecycle (open-and-reduce (store-of B))) (id-in B 'D))
      (list 'review (list (id-in B 'I) 'source)))
(define (ok-with-items? a) (and (pair? a) (eq? (car a) 'ok) (pair? (items-of a))))
(want "RE-2 the search filter: what search leaves out and says"
      (differential (lambda (x) (ask (store-of x) 'search "lantern" "--all"))
                    (lambda (a) (and (ok-with-items? a) (assq 'excluded (cdr a)) #t)))
      'same)
(want "RE-2 tasks"
      (differential (lambda (x) (ask (store-of x) 'tasks)) ok-with-items?)
      'same)
(want "RE-2 commitments"
      (differential (lambda (x) (ask (store-of x) 'commitments "--all")) ok-with-items?)
      'same)
(define context-sections '(for constraints evidence to-verify notes))
(want "RE-2 context: the hard sections and the notes"
      (differential (lambda (x)
                      (let ((a (ask (store-of x) 'context "--for" (id-in x 'U) "--budget" "100000")))
                        (if (and (pair? a) (eq? (car a) 'ok))
                            (filter (lambda (c) (and (pair? c) (memq (car c) context-sections))) (cdr a))
                            a)))
                    (lambda (cs) (and (list? cs) (exists (lambda (c) (and (pair? c) (eq? (car c) 'to-verify))) cs))))
      'same)
(define (query-rows x goal)
  (let ((a (ask (store-of x) 'query goal)))
    (if (and (pair? a) (eq? (car a) 'ok)) (cons 'rows (items-of a)) a)))
(define (rows-said? r) (and (pair? r) (eq? (car r) 'rows) (pair? (cdr r))))
(define (normalised-rows x goal)
  (let ((r (query-rows x goal)))
    (if (rows-said? r) (cons 'rows (by-print (norm (cdr r) (writer x)))) r)))
(for-each
  (lambda (g)
    (want (string-append "RE-2 query rows of " (if (string? g) g "a context goal"))
          (let* ((goal (lambda (x) (if (string? g) g (g x))))
                 (a (normalised-rows A (goal A))) (b (normalised-rows B (goal B))))
            (cond ((not (equal? a b)) (list 'differ a b))
                  ((not (rows-said? a)) (list 'says-nothing a))
                  (else 'same)))
          'same))
  (list "(depends ?a ?b)" "(depends* ?a ?b)" "(supersedes* ?a ?b)" "(contradicts ?a ?b)"
        "(replaces ?a ?b)" "(evidence-for ?a ?b)" "(validity-reason ?a ?w ?b)" "(decision-state ?d ?s)"
        "(edge-kind ?a ?k ?b)" "(moved-kind ?i ?k ?d ?e)"
        (lambda (x) (format "(and (hard ~s ?h ?role ?about) (validity ?h ?v))" (id-in x 'U)))
        (lambda (x) (format "(and (hard ~s ?h _ _) (validity-reason ?h ?why ?by))" (id-in x 'U)))
        (lambda (x) (format "(and (unsettled-for ~s ?d) (decision-state ?d ?s))" (id-in x 'U)))
        (lambda (x) (format "(and (unsettled-for ~s ?d) (decision-state ?d review) (moved-kind ?i implements ?d ?end))" (id-in x 'U)))
        (lambda (x) (format "(and (scope-of ~s ?a) (contradicts ?a ?b))" (id-in x 'U)))))
(want "RE-2 CONTROL: edge, the literal fact, answers the declared name"
      (query-rows B (format "(edge ~s ?r ?b)" (id-in B 'V)))
      (list 'rows (list 'row 'cites (id-in B 'C))))

;; ---- RE-3: the census ------------------------------------------------------------
;;
;; NO LITERAL RELATION NAME IS COMPARED OUTSIDE relation-kind. Every place in
;; the shipped sources where one of the six names is written as a symbol, by
;; the definition it is inside and how many times, read as data (comments and
;; strings are not symbols). Each is relation-kind's table, data printed as
;; it is, a kind compared after relation-kind answered, or another
;; vocabulary that happens to share a word:
;;   reduce.sc effect-relation-names    relation-kind's table
;;   lifecycle.sc effect-table          what each kind watches
;;   lifecycle.sc compute-validity, decision-state-of, implementation-of,
;;     verified-by                      kinds, asked of relation-kind
;;   query.sc rule-library              the kind constants of edge-kind and
;;                                      moved-kind goals, and the role supersedes
;;   context.sc hard-goals, preferred-steps, member-distances
;;                                      kind constants and kinds asked;
;;                                      preferred-steps' why labels
;;   context.sc role-order, hard-whys, assemble
;;                                      the role supersedes and the note label
;;   tasks.sc task-row                  a kind asked, and the answer's label
;;   templates.sc templates             the template's relation table
;;   request.sc supersedes-list?        a resolution record's own clause
;;   rules.sc write-facts               the kind depends-on, compared after
;;                                      relation-kind answered (citation)
;; A place added, or a count grown, reds this row: it is read again.
(define relation-names '(supersedes refutes depends-on implements verifies conflicts-with))
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define tree (string-append script-dir "/.."))
(define (source-name? name)
  (exists (lambda (suffix)
            (let ((n (string-length name)) (k (string-length suffix)))
              (and (> n k) (string=? suffix (substring name (- n k) n)))))
          '(".sc" ".sls" ".ss" ".scm")))
;; -> relative paths of the sources under dir, not test/ or .git, not
;; through a symbolic link.
(define (sources-under dir rel)
  (apply append
         (map (lambda (name)
                (let ((path (string-append dir "/" name))
                      (r (if (string=? rel "") name (string-append rel "/" name))))
                  (cond ((member name '("test" ".git")) '())
                        ((file-symbolic-link? path) '())
                        ((file-directory? path) (sources-under path r))
                        ((source-name? name) (list r))
                        (else '()))))
              (directory-list dir))))
(define (read-forms path)
  (call-with-input-file path
    (lambda (port) (let loop ((out '())) (let ((d (read port))) (if (eof-object? d) (reverse out) (loop (cons d out))))))))
(define (places-in form enclosing)
  (cond ((symbol? form) (if (memq form relation-names) (list enclosing) '()))
        ((pair? form)
         (let ((name (cond ((not (eq? (car form) 'define)) enclosing)
                           ((not (pair? (cdr form))) enclosing)
                           ((symbol? (cadr form)) (cadr form))
                           ((and (pair? (cadr form)) (symbol? (car (cadr form)))) (car (cadr form)))
                           (else enclosing))))
           (let loop ((xs form) (acc '()))
             (cond ((pair? xs) (loop (cdr xs) (append acc (places-in (car xs) name))))
                   ((null? xs) acc)
                   (else (append acc (places-in xs name)))))))
        (else '())))
(define (census)
  (list-sort string<?
    (apply append
           (map (lambda (rel)
                  (let ((hits (apply append (map (lambda (f) (places-in f #f)) (read-forms (string-append tree "/" rel))))))
                    (let loop ((hs hits) (out '()))
                      (if (null? hs)
                          (map (lambda (e) (format "~a ~a ~a" rel (car e) (cdr e))) out)
                          (let ((e (assq (car hs) out)))
                            (if e
                                (begin (set-cdr! e (+ (cdr e) 1)) (loop (cdr hs) out))
                                (loop (cdr hs) (cons (cons (car hs) 1) out))))))))
                (sources-under tree "")))))
(want "RE-3 the places the six relation names are written in the shipped sources, and how often"
      (census)
      '("context.sc assemble 3" "context.sc hard-goals 1" "context.sc hard-whys 2"
        "context.sc member-distances 2" "context.sc preferred-steps 5" "context.sc role-order 1"
        "lifecycle.sc compute-validity 11" "lifecycle.sc decision-state-of 1" "lifecycle.sc effect-table 6"
        "lifecycle.sc implementation-of 1" "lifecycle.sc verified-by 1"
        "query.sc rule-library 11" "reduce.sc effect-relation-names 6" "request.sc supersedes-list? 1"
        "rules.sc write-facts 1" "tasks.sc task-row 4" "templates.sc templates 7"))

;; ---- RE-4: the declared table, and the relation fact ------------------------------

(want "RE-4 query --relations lists the declared table after the two"
      (filter (lambda (i) (eq? (car i) 'declared)) (items-of (ask (store-of B) 'query "--relations")))
      '((declared answers implements (from ()) (to ())) (declared checks verifies (from ()) (to ()))
        (declared cites depends-on (from ()) (to ())) (declared clashes conflicts-with (from ()) (to ()))
        (declared disproves refutes (from ()) (to ())) (declared obsoletes supersedes (from ()) (to ()))))
(define six '((conflicts-with conflicts-with) (depends-on depends-on) (implements implements)
              (refutes refutes) (supersedes supersedes) (verifies verifies)))
(want "RE-4 (relation ?n ?k) has the six built-ins and the declared names"
      (by-print (map cdr (items-of (ask (store-of B) 'query "(relation ?n ?k)"))))
      (by-print (append six '((answers implements) (checks verifies) (cites depends-on)
                              (clashes conflicts-with) (disproves refutes) (obsoletes supersedes)))))

;; ---- RE-5: admission ------------------------------------------------------------

(define S5 (fresh-store!))
(ask S5 'init)
(define (cut5) (reduce-applied-cut (open-and-reduce S5)))
(define-caught first5 (ask S5 'relation "cites" "--as" "depends-on"))
(define-caught cut-after-first (cut5))
(want "RE-5 a first declaration is a record"
      (and (pair? first5) (car first5) (assq 'events (cdr first5)) #t)
      #t)
(want "RE-5 the same whole value again answers unchanged and writes nothing"
      (list (ask S5 'relation "cites" "--as" "depends-on") (equal? (cut5) cut-after-first))
      '((ok (unchanged)) #t))
(want "RE-5 another kind is a new record, in force from it on"
      (list (car (ask S5 'relation "cites" "--as" "refutes")) (declaration (open-and-reduce S5) 'cites))
      '(ok (in-force (refutes () ()))))
(want "RE-5 the whole value: a selector alone makes a new declaration, and query --relations prints it"
      (list (car (ask S5 'relation "cites" "--as" "refutes" "--to" "(kind doc) (field slot \"result\")"))
            (filter (lambda (i) (eq? (car i) 'declared)) (items-of (ask S5 'query "--relations"))))
      '(ok ((declared cites refutes (from ()) (to ((kind doc) (field slot "result")))))))
(want "RE-5 retiring makes it a plain edge again, and retiring again writes nothing"
      (list (car (ask S5 'relation "cites" "--retire")) (declaration (open-and-reduce S5) 'cites)
            ((reducer 'relation-kind) (open-and-reduce S5) 'cites)
            (ask S5 'relation "cites" "--retire"))
      '(ok (retired) #f (ok (unchanged))))
(want "RE-5 a retired name is not in the declared table"
      (declared-table (open-and-reduce S5))
      '())

;; ---- RE-6: what is refused ----------------------------------------------------------

(want "RE-6 a built-in name is not declared"
      (ask S5 'relation "depends-on" "--as" "implements")
      '(error relation-is-built-in (relation depends-on)))
(want "RE-6 a reserved name is refused as link refuses it"
      (ask S5 'relation "calls" "--as" "nothing")
      '(error reserved-relation (relation calls)))
(want "RE-6 a kind outside the seven is refused with them"
      (ask S5 'relation "cites" "--as" "follows")
      '(error bad-request kind-not-known (kind "follows")
              (known (supersedes refutes depends-on implements verifies conflicts-with nothing))))
(want "RE-6 batch refuses the same kind with the same detail"
      (let ((a (ask S5 'batch "((relation cites (follows () ())))")))
        (and (pair? a) (memq 'kind-not-known (let flat ((x a)) (cond ((pair? x) (append (flat (car x)) (flat (cdr x)))) ((null? x) '()) (else (list x))))) #t))
      #t)
(want "RE-6 neither --as nor --retire, or both, is the usage line"
      (map (lambda (a) (car (apply ask S5 'relation a)))
           '(("cites") ("cites" "--retire" "--as" "nothing")))
      '(usage usage))
(want "RE-6 a selector that is not one is malformed"
      (ask S5 'relation "cites" "--as" "depends-on" "--from" "(kind nonsense)")
      '(error malformed-intent (selector-malformed)))

;; ---- RE-7: nothing, a listed edge --------------------------------------------------

(define S7 (fresh-store!))
(ask S7 'init)
(define a7 (new-id (ask S7 'insert "--title" "Doc seven")))
(define b7 (new-id (ask S7 'insert "--title" "Code seven")))
(ask S7 'relation "documents" "--as" "nothing")
(ask S7 'link a7 "documents" b7)
(ask S7 'set b7 "src" "changed after the link")
;; `nothing` IS ONE OF THE SEVEN KINDS (D1): relation-kind answers it for
;; documents, so edge-kind lists the edge under it, and it has no effect.
(want "RE-7 nothing declares a listed edge: the relation fact names it, edge-kind lists it as nothing, it has no effect, and the fast path holds"
      (list (by-print (map cdr (items-of (ask S7 'query "(relation ?n ?k)"))))
            (lifecycle-fast? (lifecycle (open-and-reduce S7)))
            (map cdr (items-of (ask S7 'query "(edge-kind ?a ?k ?b)")))
            ((reducer 'relation-kind) (open-and-reduce S7) 'documents)
            (validity-of (lifecycle (open-and-reduce S7)) a7)
            (map (lambda (r) (cdr r)) (items-of (ask S7 'query "(edge ?a ?r ?b)"))))
      (list (by-print (append six '((documents nothing)))) #t (list (list a7 'nothing b7)) 'nothing '(valid)
            (list (list a7 'documents b7))))

;; ---- RE-8: the template -------------------------------------------------------------

(define S8 (fresh-store!))
(ask S8 'init "--template" "project")
(want "RE-8 the project template declares documents, as nothing, and nothing else"
      (declared-table (open-and-reduce S8))
      '((documents nothing (from ()) (to ()))))
(define S8b (fresh-store!))
(ask S8b 'init)
(want "RE-8 a store made without a template has only the six"
      (list (by-print (map cdr (items-of (ask S8b 'query "(relation ?n ?k)"))))
            (declared-table (open-and-reduce S8b)))
      (list (by-print six) '()))

;; ---- RE-9: replay, rebuild, snapshot --------------------------------------------------

(define table-b
  '((answers implements (from ()) (to ())) (checks verifies (from ()) (to ()))
    (cites depends-on (from ()) (to ())) (clashes conflicts-with (from ()) (to ()))
    (disproves refutes (from ()) (to ())) (obsoletes supersedes (from ()) (to ()))))
(want "RE-9 a replay from the log gives the table"
      (declared-table (open-and-reduce (store-of B)))
      table-b)
(want "RE-9 a reduction written as snapshot rows and read back gives the table, and the same hash"
      (let* ((st (open-and-reduce (store-of B))) (back (rows->state (state->rows st))))
        (list (declared-table back) (equal? (state-hash back) (state-hash st))))
      (list table-b #t))
(want "RE-9 a store reopened from its snapshot gives the table"
      (begin (ask (store-of B) 'snapshot)
             (list (declared-table (open-and-reduce (store-of B)))
                   (validity-of (lifecycle (open-and-reduce (store-of B))) (id-in B 'V))))
      (list table-b (list 'needs-review (list 'premise-refuted (id-in B 'C) '(relation cites)))))
;; A REBUILD IS FORCED BY A RECORD THAT REVERSES AN APPLIED MEMBERSHIP: two
;; records claiming one request's identity. The first is applied, then the
;; second gates both, and the state is folded again from the records.
(define (claim-actor) (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0)))
(define-caught rebuilt
  (let ((r (reduce-empty)))
    (reduce-apply! r "decl0000" 1 '() '(relation cites (depends-on () ())))
    (reduce-apply! r "aaa00000" 1 '() '(relation answers (implements () ())) (claim-actor))
    (reduce-apply! r "bbb00000" 1 '() '(relation answers (implements () ())) (claim-actor))
    r))
(want "RE-9 a rebuild after a retraction: both claims gated, and the table is what the applied records say"
      (list (map (lambda (w) (let ((p (assoc (cons w 1) (reduce-gates rebuilt)))) (and p (cdr p))))
                 '("aaa00000" "bbb00000"))
            (declared-table rebuilt))
      '((plan-conflict plan-conflict) ((cites depends-on (from ()) (to ())))))

;; ---- RE-10: two writers -----------------------------------------------------------------

(define (fold-records . recs)
  (let ((r (reduce-empty)))
    (for-each (lambda (x) (apply reduce-apply! r x)) recs)
    r))
(define rec-a '("wa000000" 1 () (relation cites (depends-on () ()))))
(define rec-b '("wb000000" 1 () (relation cites (refutes () ()))))
(define edge-recs
  '(("wc000000" 1 () (put ((kind . section) (title . "citing"))))
    ("wc000000" 2 () (put ((kind . section) (title . "cited"))))
    ("wc000000" 3 () (link "wc000000.1" cites "wc000000.2"))))
(define-caught ab (apply fold-records rec-a rec-b edge-recs))
(define-caught ba (apply fold-records (append edge-recs (list rec-b rec-a))))
(define contested-ab
  '(contested ((depends-on () ()) "wa000000" 1) ((refutes () ()) "wb000000" 1)))
(want "RE-10 concurrent declarations of one name leave it contested, whatever the arrival order, with the same hash"
      (list (declaration ab 'cites) (declaration ba 'cites) (equal? (state-hash ab) (state-hash ba)))
      (list contested-ab contested-ab #t))
(want "RE-10 a contested name has no effect: its edges are plain"
      (list ((reducer 'relation-kind) ab 'cites) (lifecycle-fast? (lifecycle ab)))
      '(#f #t))
(want "RE-10 conflicts' source lists it with both candidates, in either order"
      (list ((reducer 'state-relation-contested) ab) ((reducer 'state-relation-contested) ba))
      (let ((row (list (list 'relation-contested 'cites (cons 'candidates (cdr contested-ab))))))
        (list row row)))
(want "RE-10 a later declaration whose deps cover both resolves it"
      (let ((r (apply fold-records rec-a rec-b
                      (append edge-recs (list '("wa000000" 2 (("wb000000" . 1)) (relation cites (depends-on () ()))))))))
        (list (declaration r 'cites) ((reducer 'relation-kind) r 'cites) (lifecycle-fast? (lifecycle r))))
      '((in-force (depends-on () ())) depends-on #f))
(want "RE-10 two declarations differing only in a selector are contested too"
      (let ((x '("wa000000" 1 () (relation cites (depends-on ((kind doc)) ()))))
            (y '("wb000000" 1 () (relation cites (depends-on () ())))))
        (list (car (declaration (fold-records x y) 'cites)) (car (declaration (fold-records y x) 'cites))))
      '(contested contested))
(want "RE-10 agreement is by value: two equal concurrent declarations are in force, and one superseded alone contests"
      (let ((x '("wa000000" 1 () (relation cites (depends-on () ()))))
            (y '("wb000000" 1 () (relation cites (depends-on () ()))))
            (z '("wa000000" 2 () (relation cites (refutes () ())))))
        (list (declaration (fold-records x y) 'cites)
              (declaration (fold-records x y z) 'cites)))
      '((in-force (depends-on () ()))
        (contested ((refutes () ()) "wa000000" 2) ((depends-on () ()) "wb000000" 1))))

;; THROUGH THE STORE: a peer's declaration published beside the local one.
(define S10 (fresh-store!))
(define w10 (writer-of (ask S10 'init)))
(define-caught local10 (ask S10 'relation "cites" "--as" "depends-on"))
(define peer-file (string-append root "/peer.bin"))
(call-with-port (open-file-output-port peer-file (file-options no-fail))
  (lambda (p)
    (let ((r (encode-record 1 1789000000001 "peer" '() (storable-encode '(relation cites (refutes () ()))))))
      (put-bytevector p (if (string? r) (string->utf8 r) r)))))
(ask S10 'publish "peerzzzz" "1" peer-file)
(define (local-candidate)
  (let ((e (car (cadr (assq 'events (cdr local10)))))) (list '(depends-on () ()) (car e) (cdr e))))
(want "RE-10 conflicts lists the contested name with both candidates, and query --relations prints it contested"
      (list (filter (lambda (i) (eq? (car i) 'relation-contested)) (items-of (ask S10 'conflicts)))
            (filter (lambda (i) (eq? (car i) 'declared)) (items-of (ask S10 'query "--relations"))))
      (list (list (list 'relation-contested 'cites
                        (cons 'candidates
                              (list-sort (lambda (a b) (string<? (cadr a) (cadr b)))
                                         (list (local-candidate) '((refutes () ()) "peerzzzz" 1))))))
            '((declared cites (contested)))))
(want "RE-10 the same declaration again is not unchanged while contested: it writes, and resolves the name"
      (list (car (ask S10 'relation "cites" "--as" "depends-on"))
            (declaration (open-and-reduce S10) 'cites)
            (filter (lambda (i) (eq? (car i) 'relation-contested)) (items-of (ask S10 'conflicts))))
      '(ok (in-force (depends-on () ())) ()))

;; ---- RE-11: what a query over edge-kind costs ------------------------------------------

(define (cost goal budget)
  (let* ((S (make-query-session (open-and-reduce (store-of A)) '() budget))
         (a (session-answer S goal)))
    (list (car a) (session-spent S))))
(define-caught edge-cost (cost '(edge ?a depends-on ?b) 1000000))
(want "RE-11 a query over edge-kind costs what the same query over edge costs"
      (list (cost '(edge-kind ?a depends-on ?b) 1000000) edge-cost)
      (list edge-cost edge-cost))
(want "RE-11 and the budget boundary reads the same: at the cost both answer, one under it both refuse alike"
      (let ((n (cadr edge-cost)))
        (list (map (lambda (g) (car (session-answer (make-query-session (open-and-reduce (store-of A)) '() n) g)))
                   '((edge ?a depends-on ?b) (edge-kind ?a depends-on ?b)))
              (let ((r (map (lambda (g) (session-answer (make-query-session (open-and-reduce (store-of A)) '() (- n 1)) g))
                            '((edge ?a depends-on ?b) (edge-kind ?a depends-on ?b)))))
                (list (car (car r)) (equal? (car r) (cadr r))))))
      '((ok ok) (error #t)))

;; ---- the first review's findings ---------------------------------------------------

;; RE-12 AN UNCHANGED DECLARATION INSIDE A TRACKED BATCH: each item of a
;; tracked batch writes the record its receipt declared, so the shortcut
;; (ok (unchanged)) is not taken there -- taken, it wrote nothing and the next
;; item's position no longer matched (receipt-timetable).
(define S12 (fresh-store!))
(define w12 (writer-of (ask S12 'init)))
(ask S12 'relation "cites" "--as" "depends-on")
(define (count12) (cdr (assoc w12 (reduce-applied-cut (open-and-reduce S12)))))
(define before12 (count12))
(define-caught b12
  (ask S12 'batch "((relation cites (depends-on () ())) (insert root #f ((title . \"after the declaration\"))))"
       "--req" "re-12-batch" "--cursor" (string-append w12 ":" (number->string before12))))
(want "RE-12 a tracked batch holding the declaration in force and an insert: both items ok, both written, the name still in force"
      (list (and (pair? b12) (car b12))
            (and (pair? b12) (pair? (cdr b12)) (list? (cadr b12)) (map (lambda (i) (and (pair? i) (car i))) (cadr b12)))
            (>= (- (count12) before12) 2)
            (declaration (open-and-reduce S12) 'cites))
      '(batch (ok ok) #t (in-force (depends-on () ()))))
(want "RE-12 CONTROL: outside a tracked batch the same declaration still answers unchanged and writes nothing"
      (let ((n (count12))) (list (ask S12 'relation "cites" "--as" "depends-on") (= n (count12))))
      '((ok (unchanged)) #t))

;; RE-13 A PROPAGATED REASON NAMES THE DECLARED RELATION: A cites B, B
;; depends-on C, C moved past its edge. B needs review for C through a
;; built-in edge (no clause); A needs review for B through cites, and its
;; reason says so.
(define S13 (fresh-store!))
(ask S13 'init)
(ask S13 'relation "cites" "--as" "depends-on")
(define a13 (new-id (ask S13 'insert "--title" "Citing thirteen")))
(define b13 (new-id (ask S13 'insert "--title" "Cited thirteen")))
(define c13 (new-id (ask S13 'insert "--title" "Premise thirteen")))
(ask S13 'link a13 "cites" b13)
(ask S13 'link b13 "depends-on" c13)
(ask S13 'set c13 "src" "changed after the link")
(want "RE-13 the propagated reason carries (relation cites); the built-in one below it does not"
      (let ((L (lifecycle (open-and-reduce S13))))
        (list (validity-of L a13) (validity-of L b13)))
      (list (list 'needs-review (list 'premise-needs-review b13 '(relation cites)))
            (list 'needs-review (list 'premise-moved c13))))

;; RE-14 DESCRIBE PRINTS THE DECLARED TABLE (D4), and only when it is not
;; empty: a store with no declaration -- never declared, or every name
;; retired -- has no such clause, so describe reads as it did.
(define S14 (fresh-store!))
(ask S14 'init)
(define (declared-clause a) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) 'declared-relations))) (cdr a))))
;; describe never opens a store: its store clauses come from the state the
;; caller hands it, as the daemon does, so the row hands it.
(define (describe14) (rpc-dispatch S14 '(describe) "test" (open-and-reduce S14)))
(define-caught d14-empty (describe14))
(ask S14 'relation "cites" "--as" "depends-on")
(ask S14 'relation "answers" "--as" "implements" "--to" "(kind decision)")
(define-caught d14-declared (describe14))
(ask S14 'relation "cites" "--retire")
(ask S14 'relation "answers" "--retire")
(define-caught d14-retired (describe14))
(want "RE-14 describe appends the declared table, a selector only when it is given"
      (declared-clause d14-declared)
      '(declared-relations (answers implements (to ((kind decision)))) (cites depends-on)))
(want "RE-14 with no declaration in force describe has no such clause, and is the same answer before and after"
      (list (declared-clause d14-empty) (declared-clause d14-retired) (equal? d14-empty d14-retired))
      '(#f #f #t))

;; RE-15 THE STATED LIMIT OF CONTEXT (D4, ruled): context builds a hard
;; block's validity from the rows of its five queries, whose validity-reason
;; fact is (id why by); so context prints the kind's reason WITHOUT the
;; (relation <name>) clause, while a validity read prints it.
(define S15 (fresh-store!))
(ask S15 'init)
(ask S15 'relation "cites" "--as" "depends-on")
(define t15 (new-id (ask S15 'insert "--title" "Context fifteen task")))
(ask S15 'set t15 "kind" "task")
(define m15 (new-id (ask S15 'insert "--title" "Member fifteen")))
(define n15 (new-id (ask S15 'insert "--title" "Cited fifteen")))
(define r15 (new-id (ask S15 'insert "--title" "Refuter fifteen")))
(ask S15 'link t15 "depends-on" m15)
(ask S15 'link m15 "cites" n15)
(ask S15 'link r15 "refutes" n15)
(want "RE-15 the member's validity read names cites; its context entry carries the kind's reason without the clause"
      (let* ((a (ask S15 'context "--for" t15 "--budget" "100000"))
             (entries (apply append (map (lambda (n) (let ((c (find (lambda (c) (and (pair? c) (eq? (car c) n))) (cdr a))))
                                                       (if (and c (list? (cadr c))) (cadr c) '())))
                                         '(constraints evidence to-verify))))
             (e (find (lambda (e) (and (pair? e) (equal? (car e) m15))) entries)))
        (list (validity-of (lifecycle (open-and-reduce S15)) m15)
              (and e (find (lambda (c) (and (pair? c) (eq? (car c) 'validity))) (cdr e)))))
      (list (list 'needs-review (list 'premise-refuted n15 '(relation cites)))
            (list 'validity 'needs-review (list 'premise-refuted n15))))

;; RE-16 THE HASH COVERS THE DECLARED TABLE: a state, and the same rows
;; with the declaration rows left out, differ in the table and nothing
;; else, and hash apart; read back whole, they hash alike.
(want "RE-16 the state hash covers the declared table: the rows without the declarations hash apart, the rows whole alike"
      (let* ((st (open-and-reduce (store-of B))) (rows (state->rows st))
             (bare (rows->state (filter (lambda (r) (not (and (pair? r) (eq? (car r) 'relation)))) rows))))
        (list (null? (declared-table bare)) (equal? (state-hash bare) (state-hash st))
              (equal? (state-hash (rows->state rows)) (state-hash st))))
      '(#t #f #t))

;; RE-17 TWO NAMES OF ONE KIND BETWEEN THE SAME TWO BLOCKS: an end has moved
;; when it moved past every edge of the kind. The implementer changes after
;; its implements edge and before its answers edge: the decision is
;; implemented; changed again, past both, it is in review.
(define S17 (fresh-store!))
(ask S17 'init)
(ask S17 'relation "answers" "--as" "implements")
(define d17 (new-id (ask S17 'insert "--title" "Decision seventeen")))
(ask S17 'set d17 "kind" "decision")
(define i17 (new-id (ask S17 'insert "--title" "Implementer seventeen")))
(ask S17 'link i17 "implements" d17)
(ask S17 'set i17 "src" "changed after the implements link")
(ask S17 'link i17 "answers" d17)
(want "RE-17 moved past one of two edges of the kind the decision is implemented; past both, in review"
      (let ((before (decision-state-of (lifecycle (open-and-reduce S17)) d17)))
        (ask S17 'set i17 "src" "changed after both links")
        (list before (decision-state-of (lifecycle (open-and-reduce S17)) d17)))
      (list '(implemented) (list 'review (list i17 'source))))

;; RE-18 MOVED IS OVER THE EDGES WITH AN EFFECT: a nothing edge whose end
;; moved is not listed; an effect edge between the same blocks is.
(define S18 (fresh-store!))
(ask S18 'init)
(ask S18 'relation "documents" "--as" "nothing")
(ask S18 'relation "cites" "--as" "depends-on")
(define a18 (new-id (ask S18 'insert "--title" "Source eighteen")))
(define b18 (new-id (ask S18 'insert "--title" "Target eighteen")))
(ask S18 'link a18 "documents" b18)
(ask S18 'link a18 "cites" b18)
(ask S18 'set b18 "src" "changed after the links")
(want "RE-18 moved lists the cites edge whose target moved, not the documents edge beside it"
      (map cdr (items-of (ask S18 'query "(moved ?a ?r ?b ?end)")))
      (list (list a18 'cites b18 'target)))

;; RE-19 THE MEMBER WALK READS THE KIND: context orders the hard members by
;; their distance from the unit over depends-on and implements edges under
;; any name, a member with a distance before one without. M is at distance 1
;; through cites, P at 1 and Q at 2 through depends-on: Q comes last. A walk
;; by the edge's name would leave M with no distance, after Q.
(define S19 (fresh-store!))
(ask S19 'init)
(ask S19 'relation "cites" "--as" "depends-on")
(define t19 (new-id (ask S19 'insert "--title" "Context nineteen task")))
(ask S19 'set t19 "kind" "task")
(define m19 (new-id (ask S19 'insert "--title" "Cited nineteen")))
(define p19 (new-id (ask S19 'insert "--title" "Premise nineteen")))
(define q19 (new-id (ask S19 'insert "--title" "Second premise nineteen")))
(ask S19 'link t19 "cites" m19)
(ask S19 'link t19 "depends-on" p19)
(ask S19 'link p19 "depends-on" q19)
(want "RE-19 the member reached through cites is ordered by its distance, before the member two steps away"
      (let* ((a (ask S19 'context "--for" t19 "--budget" "100000"))
             (ids (apply append
                         (map (lambda (n)
                                (let ((c (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) n))) (cdr a)))))
                                  (if (and c (pair? (cdr c)) (list? (cadr c))) (map (lambda (e) (and (pair? e) (car e))) (cadr c)) '())))
                              context-sections)))
             (order (filter (lambda (x) (member x (list m19 p19 q19))) ids)))
        (list (length order) (and (= 3 (length order)) (equal? (caddr order) q19))))
      '(3 #t))

(printf "rows: ~a\n~a failures\nrelation-effects complete\n" rows bad)
(system (string-append "rm -rf '" root "'"))
(exit (if (= bad 0) 0 1))
