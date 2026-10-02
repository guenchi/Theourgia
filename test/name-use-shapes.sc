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
;; The name-use walk's shapes against Chez's own expander.
;;
;; THE RULE UNDER TEST: a known form's rule fires only on the form's whole
;; shape as Chez Scheme accepts it, and any other shape falls back (walks
;; every element). Writing that rule form by form, by hand, never closed:
;; each review found the next dimension. So this file asks a second,
;; independent judge -- the expander -- over a space of forms stated here,
;; before the first run, and the row is: the walk's rule fired if and only
;; if Chez accepts the form.
;;
;; THE REFERENCE. (expand T env) in a fresh environment per form: a copy of
;; the (chezscheme) environment holding only the syntax T names (a whole
;; copy per form grows slower with every copy, measured: 500 copies took 6 s,
;; the next 500 45 s). Every other symbol of T is defined in it as a fresh
;; variable (an imported variable such as car too, so that set! of it is a
;; question of shape, not of an immutable import). Which symbols are syntax
;; is read from the environment's own symbol list and top-level-bound?:
;; top-level-syntax? answers #t for an unbound symbol and for a variable. A name a record definition's (parent <name>) clause
;; names is defined as a record type; and the right-hand side of a
;; define-syntax of three parts is replaced by (syntax-rules ()), because
;; expansion EVALUATES a transformer expression and its value is not the
;; definition's shape. Auxiliary keywords (else, =>, _, unquote, the record
;; clause words) stay the keywords they are. Apart from those transformers
;; and those record types, expansion runs nothing, and it is called only
;; here: (theourgia name-use) never expands or evaluates.
;;
;; THE POSITIONS. A known form is judged as a block's top form: expanded at
;; the top level, and asked of the walk with name-use-rule-fires?. A
;; defining form is judged twice: as a top form, and at the head of a body,
;; where both judges are asked (lambda () D 0).
;;
;; WHAT "THE RULE FIRED" MEANS: (name-use-rule-fires? form), the walk's own
;; answer for the form's own rule (in a body, for the lambda around it). A
;; raise there is its own outcome, never a firing: it is a disagreement on
;; every form.
;;
;; THE SPACE, stated before the first run (its first statement, s1, was
;; widened after codex reviewed the space and before any run; NOTES says so):
;;   SEEDS: well-formed forms per head (below), each accepted by the
;;     reference and fired by the walk (the control row says so).
;;   OPERATORS, each applied ONCE to a seed, at every position of every list
;;     inside it (the empty list included) but the outer head:
;;     O1  drop the element;
;;     O2  duplicate it in place;
;;     O3  replace it by each of: zz, __zz, 7, -1, 1/2, 1.5, "s", #t, #\c, (),
;;         (zz), (zz . zy), #(zz), #vu8(1);
;;     O4  if it is a symbol, replace it by each other symbol of the seed but
;;         the head (a binder named twice, a name used where another was);
;;     O4k if it is a symbol, replace it by each of: begin, define, else, =>,
;;         lambda, quote (a keyword shadowed, or used as a name);
;;     O5  swap it with the element after it;
;;     O6  insert (begin) before it, and at the end of each list;
;;     O7  insert (define zz 0) before it, and at the end of each list;
;;     O8  make the list improper after it: its tail becomes the atom zz;
;;     O9  truncate the list after it (every shorter prefix, the outer head
;;         kept).
;;   A STATED LIMIT: the operators walk lists only, never into a vector, so a
;;     vector template is examined only as its seeds write it.
;;   PINNED: forms asked the same question without being generated: the gaps
;;     recorded when the rule was last written by hand.
;; The row prints how many forms the space holds, and the coverage rows ask
;; that every head and every operator produced forms and that every form of
;; the space has a verdict.
;;
;; CLASS A, the disagreements whose defect is one operand, is recognised by a
;; predicate (below), with two sub-classes; the row S2 asks that everything
;; else agrees or is in the exception table. S5 and the A-name pin are
;; regression checks, not proofs.
;;
;; THE EXCEPTION TABLE is data: (form position direction reason). A
;; disagreement it lists is accepted with its reason; an entry whose form is
;; in the space and agrees is stale, which is red too. It starts with the
;; macro keywords the walk deliberately does not know: they are never seeds,
;; and their entries say so.

(import (chezscheme) (only (theourgia name-use) name-use-rule-fires? datum-uses))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" name got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAILED ROW, not the end of the file.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (want-1 name (caught got) expected))))

;; ---- the heads and the seeds ----------------------------------------------------------

(define known-heads
  '(quote quasiquote lambda case-lambda let let* letrec letrec* let-values let*-values do begin
    if and or when unless set! cond case guard parameterize rec foreign-procedure))
(define defining-heads '(define define-syntax define-values define-record-type))

(define seeds
  '((quote d)
    (quasiquote (a (unquote b)))
    (quasiquote (a (unquote-splicing b)))
    (lambda (a b) (f a b))
    (lambda (a) (define b a) (define c a) (f b c))
    (lambda (a) (define b a) (b))
    (case-lambda ((a) a) ((a b) b))
    (let ((a 1) (b 2)) (f a b))
    (let lp ((a 1)) (lp a))
    (let* ((a 1) (b a)) b)
    (letrec ((a (g b)) (b 1)) a)
    (letrec* ((a 1) (b a)) b)
    (let-values (((a b) (g)) ((c) (k))) (f a b c))
    (let*-values (((a) (g)) ((b) a)) b)
    (do ((i 0 (f i))) ((p i) i) (g i))
    (begin (f) (g))
    (if a b c)
    (and a b)
    (or a b)
    (when a b)
    (unless a b)
    (set! a b)
    (cond ((p a) b) ((q a) => f) (else c))
    (case k ((a b) c) (else d))
    (guard (e ((p e) e) (else 0)) (f))
    (parameterize ((p 1)) (f))
    (rec f (g f))
    (foreign-procedure "f" (int) int)
    (foreign-procedure __collect_safe "f" (int) int)
    (foreign-procedure (__varargs_after 1) "f" (int int) int)
    (foreign-procedure "f" () void)
    (foreign-procedure __varargs "f" (int double) double)
    (foreign-procedure "f" (double float uptr) double)
    (foreign-procedure "f" ((* t) (& t)) (* t))
    (foreign-procedure __collect_safe __varargs "f" (int double) int)
    (quasiquote (a (quasiquote (b (unquote (unquote c))))))
    (quasiquote #(a (unquote b)))
    (define (f a) (g a))
    (define x 1)
    (define-syntax m (syntax-rules () ((_ a) a)))
    (define-values (a b) (g))
    (define-record-type (p mk p?) (fields (immutable x px) (mutable y py set-py!)) (protocol (g)) (sealed #t))
    (define-record-type c (parent p) (fields z))
    (define-record-type c (parent-rtd prtd pcd) (fields z))
    (define-record-type c (opaque #t) (nongenerative))))

;; The gaps recorded with the last hand-written rule, asked here by name.
(define pinned
  '((lambda () (define begin list) (begin))
    (define-record-type r (fields (mutable x get-x)))
    (define-record-type (r same same))
    (define-record-type r (fields x x))
    (foreign-procedure __bogus entry (int) int)
    (foreign-procedure (__varargs_after banana) entry (int) int)
    (foreign-procedure entry (17) int)
    (foreign-procedure entry (int) (oops x))
    (lambda () 1 (define x 2))
    (lambda () (define x 1) (define x 2) x)
    (do () (#t) (define x 1) x)
    (begin (begin))
    (define-record-type c (parent p) (parent-rtd a b))
    (define-record-type (p mk p))
    (foreign-procedure __collect_safe __collect_safe entry (int) int)
    (foreign-procedure #f entry (int) int)
    (quasiquote (unquote (unquote b)))
    (define-record-type c (parent lambda) (fields z))
    (define-record-type r (protocol (lambda (parent) (parent p))))
    (lambda (a b . a) (f a b))
    (define-values (a b . a) (g))))

;; (form position direction reason). position: top or body; direction:
;; stricter (Chez accepts, the walk falls back), looser (the walk fires,
;; Chez refuses), or not-a-seed (a head the walk deliberately does not
;; know: never generated).
(define exceptions
  '(((syntax-rules) any not-a-seed "a transformer is a macro of its own; the walk walks it whole by design")
    ((syntax-case) any not-a-seed "as syntax-rules")
    ((let-syntax) any not-a-seed "a macro scope; walked whole by design")
    ((letrec-syntax) any not-a-seed "as let-syntax")
    ((define-syntax syntax-rules (syntax-rules () ((_ a) a))) top looser
     "a reference artifact: the fixture replaces the right-hand side by (syntax-rules ()), which then names the keyword being defined; the form also fails direct top-level expansion, so the refusal is not the replacement's alone")
    ;; A keyword moved into an expression position by deletion, duplication
    ;; or a swap. Chez refuses the keyword there; the walk judges what
    ;; stands in an expression position where it meets it (class A's
    ;; limit), but the change is not to one operand that a substitute could
    ;; stand for, so each form is listed (main's ruling).
    ((quasiquote (a (unquote unquote b))) top looser
     "duplication makes unquote an operand of unquote, a keyword in an expression position of the template")
    ((quasiquote (a (unquote-splicing unquote-splicing b))) top looser
     "duplication makes unquote-splicing an operand of unquote-splicing, a keyword in an expression position")
    ((lambda (a) (define b a) (c define a) (f b c)) top looser
     "a swap makes define an operand of an application, a keyword in an expression position")
    ((lambda (a) (b define a) (b)) top looser
     "a swap makes define an operand of an application, a keyword in an expression position")
    ((cond ((p a) b) (=> f) (else c)) top looser
     "deleting the test leaves => as a clause's test, a keyword in an expression position")
    ((cond ((p a) b) ((q a) (q a) => f) (else c)) top looser
     "duplicating the test leaves => in the clause's body, a keyword in an expression position")
    ((cond ((p a) b) (=> (q a) f) (else c)) top looser
     "a swap puts => in the test position, a keyword in an expression position")
    ((cond ((p a) b) ((q a) f =>) (else c)) top looser
     "a swap leaves => last in the clause's body, a keyword in an expression position")
    ((cond ((p a) b) ((q a) => f) (else else c)) top looser
     "duplication puts else in the else clause's body, a keyword in an expression position")
    ((cond ((p a) b) ((q a) => f) (c else)) top looser
     "a swap puts else in a clause's body, a keyword in an expression position")
    ((case k ((a b) c) (else else d)) top looser
     "duplication puts else in the else clause's body, a keyword in an expression position")
    ((case k ((a b) c) (d else)) top looser
     "a swap puts else in a clause's body, a keyword in an expression position")
    ((guard (e ((p e) e) (else 0)) (e ((p e) e) (else 0)) (f)) top looser
     "duplicating the clause list makes it a body expression, whose (else 0) puts else in an expression position")
    ((guard (e ((p e) e) (else else 0)) (f)) top looser
     "duplication puts else in the else clause's body, a keyword in an expression position")
    ((guard (e ((p e) e) (0 else)) (f)) top looser
     "a swap puts else in a clause's body, a keyword in an expression position")))

;; ---- the generator ----------------------------------------------------------------------

(define (symbols-in x)
  (cond ((symbol? x) (list x))
        ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x))))
        ((vector? x) (apply append (map symbols-in (vector->list x))))
        (else '())))

(define (dedup xs)
  (let ((seen (make-hashtable equal-hash equal?)))
    (let loop ((xs xs) (out '()))
      (cond ((null? xs) (reverse out))
            ((hashtable-ref seen (car xs) #f) (loop (cdr xs) out))
            (else (hashtable-set! seen (car xs) #t) (loop (cdr xs) (cons (car xs) out)))))))

;; Every list inside X, the empty list included, as the path of indices that
;; reaches it ('() is X).
(define (list-paths x)
  (let walk ((x x) (path '()))
    (if (or (null? x) (and (pair? x) (list? x)))
        (cons (reverse path)
              (apply append
                     (let loop ((l x) (i 0) (out '()))
                       (if (null? l) (reverse out)
                           (loop (cdr l) (+ i 1) (cons (walk (car l) (cons i path)) out))))))
        '())))

(define (get x path) (if (null? path) x (get (list-ref x (car path)) (cdr path))))
(define (update x path f)
  (if (null? path) (f x)
      (let loop ((l x) (i 0))
        (if (= i (car path))
            (cons (update (car l) (cdr path) f) (cdr l))
            (cons (car l) (loop (cdr l) (+ i 1)))))))
(define (splice l i new)
  (let loop ((l l) (k 0))
    (if (= k i) (append new (cdr l)) (cons (car l) (loop (cdr l) (+ k 1))))))
(define (insert-before l i v)
  (let loop ((l l) (k 0))
    (if (= k i) (cons v l) (cons (car l) (loop (cdr l) (+ k 1))))))
(define (improper-after l i)
  (let loop ((l l) (k 0))
    (if (= k i) (cons (car l) 'zz) (cons (car l) (loop (cdr l) (+ k 1))))))
(define (prefix l k) (if (= k 0) '() (cons (car l) (prefix (cdr l) (- k 1)))))

(define o3-replacements '(zz __zz 7 -1 1/2 1.5 "s" #t #\c () (zz) (zz . zy) #(zz) #vu8(1)))
(define o4k-names '(begin define else => lambda quote))
(define operators '(O1 O2 O3 O4 O4k O5 O6 O7 O8 O9))

;; Each operator once at every position but the outer head.
;; -> (form operator place) triples. PLACE names the ONE operand the change
;; made, as (path . index): the operand replaced or inserted (O3, O4, O4k,
;; O6, O7, at any path), or, for an operator that changes a list's length or
;; spine (O1, O2, O5, O8, O9), the list itself when it is an operand of an
;; enclosing list, that is when its path is not the outer form's. #f when
;; no single operand is what changed.
(define (last-of l) (if (null? (cdr l)) (car l) (last-of (cdr l))))
(define (but-last l) (if (null? (cdr l)) '() (cons (car l) (but-last (cdr l)))))
(define (mutants-of seed)
  (let ((names (dedup (filter (lambda (s) (not (eq? s (car seed)))) (symbols-in (cdr seed))))))
    (apply append
           (map (lambda (path)
                  (let* ((l (get seed path)) (n (length l)) (outer? (null? path))
                         (whole (if outer? #f (cons (but-last path) (last-of path)))))
                    (define (at f op place) (list (update seed path f) op place))
                    (append
                      (list (at (lambda (l) (append l '((begin)))) 'O6 (cons path n))
                            (at (lambda (l) (append l '((define zz 0)))) 'O7 (cons path n)))
                      (apply append
                             (map (lambda (i)
                                    (if (and outer? (= i 0))
                                        '()
                                        (let ((e (list-ref l i)))
                                          (append
                                            (list (at (lambda (l) (splice l i '())) 'O1 whole)
                                                  (at (lambda (l) (splice l i (list e e))) 'O2 whole))
                                            (map (lambda (r) (at (lambda (l) (splice l i (list r))) 'O3 (cons path i))) o3-replacements)
                                            (if (symbol? e)
                                                (append
                                                  (map (lambda (s) (at (lambda (l) (splice l i (list s))) 'O4 (cons path i)))
                                                       (filter (lambda (s) (not (eq? s e))) names))
                                                  (map (lambda (s) (at (lambda (l) (splice l i (list s))) 'O4k (cons path i)))
                                                       (filter (lambda (s) (not (eq? s e))) o4k-names)))
                                                '())
                                            (if (< (+ i 1) n)
                                                (list (at (lambda (l) (splice (splice l i (list (list-ref l (+ i 1))))
                                                                              (+ i 1) (list e)))
                                                          'O5 whole))
                                                '())
                                            (list (at (lambda (l) (insert-before l i '(begin))) 'O6 (cons path i))
                                                  (at (lambda (l) (insert-before l i '(define zz 0))) 'O7 (cons path i))
                                                  (at (lambda (l) (improper-after l i)) 'O8 whole)
                                                  (at (lambda (l) (prefix l i)) 'O9 whole))))))
                                  (iota n))))))
                (list-paths seed)))))

(define all-triples
  (filter (lambda (p) (and (pair? (car p)) (memq (car (car p)) (append known-heads defining-heads))))
          (apply append (map mutants-of seeds))))
;; (form . operator), each form once, the first operator that made it kept.
(define generated-pairs
  (let ((seen (make-hashtable equal-hash equal?)))
    (filter (lambda (p) (and (not (hashtable-ref seen (car p) #f)) (begin (hashtable-set! seen (car p) #t) #t)))
            (map (lambda (t) (cons (car t) (cadr t))) all-triples))))
(define generated (map car generated-pairs))
;; Every one-operand change that made a form: form -> ((operator . place) ...).
(define replacements
  (let ((h (make-hashtable equal-hash equal?)))
    (for-each (lambda (t) (when (caddr t)
                            (hashtable-set! h (car t) (cons (cons (cadr t) (caddr t)) (hashtable-ref h (car t) '())))))
              all-triples)
    h))
(define space (dedup (append seeds generated pinned)))
(define (positions-of x) (if (memq (car x) defining-heads) '(top body) '(top)))

;; ---- the two judges --------------------------------------------------------------------

;; THE REFERENCE: does Chez accept the form in that position? BIND-FREE? #f
;; is the reference with an environment binding nothing, for the mutant that
;; shows the reference is asked.
(define bind-free? #t)
(define (prepared x)
  (if (and (pair? x) (eq? (car x) 'define-syntax) (list? x) (= (length x) 3))
      (list 'define-syntax (cadr x) '(syntax-rules ()))
      x))
;; The (parent <name>) names of every record definition in T, read from
;; each definition's own clauses only: (parent p) inside a protocol
;; expression is an application.
(define (parent-names t)
  (cond ((not (pair? t)) '())
        ((and (eq? (car t) 'define-record-type) (list? t) (pair? (cdr t)))
         (append (apply append
                        (map (lambda (c) (if (and (list? c) (= (length c) 2) (eq? (car c) 'parent) (symbol? (cadr c)))
                                             (list (cadr c))
                                             '()))
                             (cddr t)))
                 (apply append (map parent-names (cddr t)))))
        (else (append (parent-names (car t)) (parent-names (cdr t))))))
;; The ftype names a foreign-procedure's types name, (* t) or (& t): defined
;; as ftypes, not variables.
(define (ftype-names t)
  (cond ((not (pair? t)) '())
        ((and (eq? (car t) 'foreign-procedure) (list? t))
         (dedup (filter symbol?
                        (map (lambda (y) (and (list? y) (= (length y) 2) (memq (car y) '(* &)) (cadr y)))
                             (apply append (map (lambda (e) (if (list? e) e (list e))) (cdr t)))))))
        (else (append (ftype-names (car t)) (ftype-names (cdr t))))))
;; The body wrapper: (lambda () D 0), or, when the form names lambda (a
;; record parent called lambda would capture it), (let () D 0), or, when it
;; names let too, (letrec () D 0). A form naming all three gets the lambda
;; wrapper and is excepted by name. Both judges are asked the same form.
(define (body-wrapper x)
  (let ((names (symbols-in x)))
    (cond ((not (memq 'lambda names)) 'lambda)
          ((not (memq 'let names)) 'let)
          ((not (memq 'letrec names)) 'letrec)
          (else 'lambda))))
(define (positioned x position)
  (if (eq? position 'body) (list (body-wrapper x) '() x 0) x))
(define base-environment (scheme-environment))
(define base-symbols
  (let ((h (make-eq-hashtable)))
    (for-each (lambda (s) (hashtable-set! h s #t)) (environment-symbols base-environment))
    h))
;; A symbol is syntax when the environment lists it and it is not a variable
;; there: top-level-syntax? answers #t for variables such as car as well.
(define (syntax-symbol? s) (and (hashtable-ref base-symbols s #f) (not (top-level-bound? s base-environment))))
;; -> #t, #f, or undecided when the expander did not answer within its budget.
(define (accepted? x position)
  (let* ((d (prepared x))
         (t (positioned d position))
         (symbols (dedup (cons* 'define-record-type 'define-ftype (symbols-in t))))
         (ftypes (ftype-names t))
         (env (copy-environment base-environment #t (filter syntax-symbol? symbols)))
         (judge (make-engine
                  (lambda ()
                    (guard (e (#t #f))
                      (when bind-free?
                        (for-each (lambda (s) (unless (or (syntax-symbol? s) (memq s ftypes)) (define-top-level-value s 0 env))) symbols)
                        (for-each (lambda (p) (eval (list 'define-record-type p) env)) (dedup (parent-names t)))
                        (for-each (lambda (t) (eval `(define-ftype ,t (struct [x int])) env)) ftypes))
                      (expand t env)
                      #t)))))
    (judge 20000000 (lambda (ticks v) v) (lambda (k) 'undecided))))

;; THE WALK: does its rule fire on the form? A raise is 'raised. In the
;; body position it is asked what the reference is asked, the form inside
;; its wrapper: a definition that fits alone can still make a body
;; malformed (a name defined twice).
(define (fired x) (guard (e (#t 'raised)) (name-use-rule-fires? x)))
(define (fired-at x position) (fired (positioned x position)))

;; ---- class A: the operand, not the shape ---------------------------------------------
;;
;; A STATED LIMIT OF THE RULE (main's ruling): what stands in an expression
;; slot is judged where the walk meets it, not by the enclosing form's rule.
;; A disagreement is class A when the rule fired, Chez refused, the
;; generator changed exactly ONE operand of one enclosing list (replaced
;; it, inserted it, or changed it at a nested path: the PLACE of the
;; generator above), and a fresh variable at that place gives a form on
;; which the rule still fires, which Chez accepts, and in which the walk
;; reaches the variable as a use (datum-uses reports it): the defect is
;; then in an operand the walk evaluates. A place the walk does not reach
;; (data, a binder) is never class A; it stays a disagreement, fixed or
;; excepted by name. Recognised by this predicate, never by a list. Two
;; sub-classes:
;;   A-expression  a constant (0) there is accepted by both as well;
;;   A-name        only the variable is: the slot takes a name (a set!
;;                 target, say). A-name is small and its (head position
;;                 place) set is PINNED (S6), so a binder slot that some
;;                 rule fails to check turns S6 red instead of joining A.
(define (with-substitute form place v)
  (update form (car place) (lambda (l) (splice l (cdr place) (list v)))))
;; Does the walk reach the place as an expression? A fresh variable put
;; there must be among the names datum-uses reports for the form, in its
;; position (main's ruling): a place the walk takes as data or as a binder
;; is not class A. REACH? #f is the predicate without this, for its mutant.
(define reach? #t)
(define (reached? f2 position)
  (or (not reach?)
      (and (memq 'zfresh (guard (e (#t '())) (datum-uses (positioned f2 position)))) #t)))
(define operand-classes (make-hashtable equal-hash equal?))
;; -> (A-expression place), (A-name place), or #f.
(define (operand-class v)
  (or (hashtable-ref operand-classes v #f)
      (let ((answer
              (and (eq? (caddr v) #t) (eq? (cadddr v) #f)
                   (let* ((position (cadr v))
                          (places (map cdr (hashtable-ref replacements (car v) '())))
                          (both-accept? (lambda (f2) (and (eq? (fired-at f2 position) #t) (eq? (accepted? f2 position) #t))))
                          (taking-name (filter (lambda (place)
                                                 (let ((f2 (with-substitute (car v) place 'zfresh)))
                                                   (and (both-accept? f2) (reached? f2 position))))
                                               places)))
                     (and (pair? taking-name)
                          (let ((e (find (lambda (place) (both-accept? (with-substitute (car v) place 0))) taking-name)))
                            (if e (list 'A-expression e) (list 'A-name (car taking-name)))))))))
        (hashtable-set! operand-classes v answer)
        answer)))
(define (class-a? v) (and (operand-class v) #t))

;; The A-name (head position place) set this tree expects; place is the
;; operand's index path in the form.
(define a-name-expected '(UNPINNED))

;; ---- the rows ----------------------------------------------------------------------------

(want "S0 CONTROL: the reference is asked: it accepts (lambda (x) x), refuses (lambda (x)) and (if), and lets set! assign an imported name"
      (map (lambda (x) (accepted? x 'top)) '((lambda (x) x) (lambda (x)) (if) (set! car 1)))
      '(#t #f #f #t))
(want "S1 CONTROL: every seed is accepted by the reference in each of its positions and fired by the walk"
      (filter (lambda (s) (not (for-all (lambda (p) (and (accepted? s p) (eq? (fired-at s p) #t))) (positions-of s)))) seeds)
      '())

(define verdicts
  (apply append
         (map (lambda (x) (map (lambda (p) (list x p (fired-at x p) (accepted? x p))) (positions-of x)))
              space)))
(define (direction v)
  (cond ((eq? (caddr v) 'raised) 'raised) ((eq? (cadddr v) 'undecided) 'undecided) ((caddr v) 'looser) (else 'stricter)))
(define (exception-for v)
  (find (lambda (e) (and (equal? (car e) (car v)) (memq (cadr e) (list (cadr v) 'any)))) exceptions))
(define disagreements
  (filter (lambda (v) (not (eq? (caddr v) (cadddr v)))) verdicts))
(define excepted
  (filter (lambda (v) (let ((e (exception-for v))) (and e (eq? (caddr e) (direction v))))) disagreements))
(define class-a (filter (lambda (v) (and (not (member v excepted)) (class-a? v))) disagreements))
(define unexplained
  (filter (lambda (v) (not (or (member v excepted) (member v class-a)))) disagreements))
(printf "   space: ~a forms (seeds ~a, generated ~a, pinned ~a), ~a verdicts; disagreements ~a: class A ~a, excepted ~a, unexplained ~a\n"
        (length space) (length seeds) (length generated) (length pinned) (length verdicts)
        (length disagreements) (length class-a) (length excepted) (length unexplained))
(define (count-by key vs)
  (let ((h (make-hashtable equal-hash equal?)))
    (for-each (lambda (v) (hashtable-update! h (key v) (lambda (n) (+ n 1)) 0)) vs)
    (let-values (((ks ns) (hashtable-entries h)))
      (list-sort (lambda (a b) (string<? (format "~a" (car a)) (format "~a" (car b)))) (map cons (vector->list ks) (vector->list ns))))))
(define a-expression (filter (lambda (v) (eq? (car (operand-class v)) 'A-expression)) (filter operand-class class-a)))
(define a-name (filter (lambda (v) (eq? (car (operand-class v)) 'A-name)) (filter operand-class class-a)))
(define (index-path place) (append (car place) (list (cdr place))))
(define a-name-set
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
             (dedup (map (lambda (v) (list (car (car v)) (cadr v) (index-path (cadr (operand-class v))))) a-name))))
(printf "   class A: A-expression ~a, A-name ~a\n" (length a-expression) (length a-name))
(printf "   A-name set: ~s\n" a-name-set)
(printf "   class A by head: ~s\n" (count-by (lambda (v) (car (car v))) class-a))
(printf "   class A by operator: ~s\n"
        (count-by (lambda (v) (let ((r (hashtable-ref replacements (car v) '()))) (if (pair? r) (car (car r)) 'none))) class-a))
(for-each (lambda (op) (printf "   operator ~a: ~a forms\n" op (length (filter (lambda (p) (eq? (cdr p) op)) generated-pairs))))
          operators)
(for-each (lambda (v) (printf "   DISAGREE ~a ~a ~s\n" (direction v) (cadr v) (car v))) unexplained)

(want "S1 CONTROL: coverage -- every form of the space has a verdict in each of its positions"
      (length verdicts)
      (apply + (map (lambda (x) (length (positions-of x))) space)))
(want "S1 CONTROL: coverage -- every head and every operator produced generated forms"
      (list (filter (lambda (h) (not (exists (lambda (x) (eq? (car x) h)) generated))) (append known-heads defining-heads))
            (filter (lambda (op) (not (exists (lambda (p) (eq? (cdr p) op)) generated-pairs))) operators))
      '(() ()))
(want "S1 CONTROL: coverage -- every pinned form has its verdicts"
      (filter (lambda (x) (not (assoc x verdicts))) pinned)
      '())
(want "S2 over the whole space, the walk's rule fires if and only if Chez accepts, but for class A and the exception table"
      (length unexplained)
      0)
(want "S3 every exception whose form is in the space still disagrees as it says"
      (filter (lambda (e)
                (and (memq (caddr e) '(stricter looser))
                     (let ((v (find (lambda (v) (and (equal? (car v) (car e)) (eq? (cadr v) (cadr e)))) verdicts)))
                       (and v (or (eq? (caddr v) (cadddr v)) (not (eq? (caddr e) (direction v))))))))
              exceptions)
      '())
(want "S4 every pinned gap agrees, or is in the exception table with its reason"
      (dedup (map car (filter (lambda (v) (member (car v) pinned)) unexplained)))
      '())

(want "S5 CONTROL: no pinned known gap is classified as class A"
      (dedup (map car (filter (lambda (v) (and (member (car v) pinned) (class-a? v))) verdicts)))
      '())
(want "S6 CONTROL: the A-name set, places that take a name, is the pinned one"
      a-name-set
      a-name-expected)

(printf "\n~a failures\nrows: ~a\nname-use-shapes complete\n" bad rows)
(exit (if (= bad 0) 0 1))
