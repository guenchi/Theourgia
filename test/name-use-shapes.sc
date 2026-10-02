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
;; where the reference expands (lambda () D 0).
;;
;; WHAT "THE RULE FIRED" MEANS: (name-use-rule-fires? form), the walk's own
;; answer for the form's own rule. A raise there is its own outcome, never a
;; firing: it is a disagreement on every form.
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
;;   PINNED: forms asked the same question without being generated: the gaps
;;     recorded when the rule was last written by hand.
;; The row prints how many forms the space holds, and the coverage rows ask
;; that every head and every operator produced forms and that every form of
;; the space has a verdict.
;;
;; THE EXCEPTION TABLE is data: (form position direction reason). A
;; disagreement it lists is accepted with its reason; an entry whose form is
;; in the space and agrees is stale, which is red too. It starts with the
;; macro keywords the walk deliberately does not know: they are never seeds,
;; and their entries say so.

(import (chezscheme) (only (theourgia name-use) name-use-rule-fires?))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
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
    (define-record-type c (parent p) (parent-rtd a b))))

;; (form position direction reason). position: top or body; direction:
;; stricter (Chez accepts, the walk falls back), looser (the walk fires,
;; Chez refuses), or not-a-seed (a head the walk deliberately does not
;; know: never generated).
(define exceptions
  '(((syntax-rules) any not-a-seed "a transformer is a macro of its own; the walk walks it whole by design")
    ((syntax-case) any not-a-seed "as syntax-rules")
    ((let-syntax) any not-a-seed "a macro scope; walked whole by design")
    ((letrec-syntax) any not-a-seed "as let-syntax")))

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

;; Each operator once at every position but the outer head. -> (form . operator) pairs.
(define (mutants-of seed)
  (let ((names (dedup (filter (lambda (s) (not (eq? s (car seed)))) (symbols-in (cdr seed))))))
    (apply append
           (map (lambda (path)
                  (let* ((l (get seed path)) (n (length l)) (outer? (null? path)))
                    (define (at f op) (cons (update seed path f) op))
                    (append
                      (list (at (lambda (l) (append l '((begin)))) 'O6)
                            (at (lambda (l) (append l '((define zz 0)))) 'O7))
                      (apply append
                             (map (lambda (i)
                                    (if (and outer? (= i 0))
                                        '()
                                        (let ((e (list-ref l i)))
                                          (append
                                            (list (at (lambda (l) (splice l i '())) 'O1)
                                                  (at (lambda (l) (splice l i (list e e))) 'O2))
                                            (map (lambda (r) (at (lambda (l) (splice l i (list r))) 'O3)) o3-replacements)
                                            (if (symbol? e)
                                                (append
                                                  (map (lambda (s) (at (lambda (l) (splice l i (list s))) 'O4))
                                                       (filter (lambda (s) (not (eq? s e))) names))
                                                  (map (lambda (s) (at (lambda (l) (splice l i (list s))) 'O4k))
                                                       (filter (lambda (s) (not (eq? s e))) o4k-names)))
                                                '())
                                            (if (< (+ i 1) n)
                                                (list (at (lambda (l) (splice (splice l i (list (list-ref l (+ i 1))))
                                                                              (+ i 1) (list e)))
                                                          'O5))
                                                '())
                                            (list (at (lambda (l) (insert-before l i '(begin))) 'O6)
                                                  (at (lambda (l) (insert-before l i '(define zz 0))) 'O7)
                                                  (at (lambda (l) (improper-after l i)) 'O8)
                                                  (at (lambda (l) (prefix l i)) 'O9))))))
                                  (iota n))))))
                (list-paths seed)))))

;; (form . operator), each form once, the first operator that made it kept.
(define generated-pairs
  (let ((seen (make-hashtable equal-hash equal?)))
    (filter (lambda (p)
              (and (pair? (car p)) (memq (car (car p)) (append known-heads defining-heads))
                   (not (hashtable-ref seen (car p) #f))
                   (begin (hashtable-set! seen (car p) #t) #t)))
            (apply append (map mutants-of seeds)))))
(define generated (map car generated-pairs))
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
(define (parent-names t)
  (let walk ((t t) (in-record? #f))
    (cond ((not (pair? t)) '())
          ((and in-record? (list? t) (= (length t) 2) (eq? (car t) 'parent) (symbol? (cadr t))) (list (cadr t)))
          (else (let ((in (or in-record? (eq? (car t) 'define-record-type))))
                  (append (walk (car t) in) (walk (cdr t) in)))))))
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
         (t (if (eq? position 'body) (list 'lambda '() d 0) d))
         (symbols (dedup (cons 'define-record-type (symbols-in t))))
         (env (copy-environment base-environment #t (filter syntax-symbol? symbols)))
         (judge (make-engine
                  (lambda ()
                    (guard (e (#t #f))
                      (when bind-free?
                        (for-each (lambda (s) (unless (syntax-symbol? s) (define-top-level-value s 0 env))) symbols)
                        (for-each (lambda (p) (eval (list 'define-record-type p) env)) (dedup (parent-names t))))
                      (expand t env)
                      #t)))))
    (judge 20000000 (lambda (ticks v) v) (lambda (k) 'undecided))))

;; THE WALK: does its rule fire on the form? A raise is 'raised.
(define (fired x) (guard (e (#t 'raised)) (name-use-rule-fires? x)))

;; ---- the rows ----------------------------------------------------------------------------

(want "S0 CONTROL: the reference is asked: it accepts (lambda (x) x), refuses (lambda (x)) and (if), and lets set! assign an imported name"
      (map (lambda (x) (accepted? x 'top)) '((lambda (x) x) (lambda (x)) (if) (set! car 1)))
      '(#t #f #f #t))
(want "S1 CONTROL: every seed is accepted by the reference in each of its positions and fired by the walk"
      (filter (lambda (s) (not (and (for-all (lambda (p) (accepted? s p)) (positions-of s)) (eq? (fired s) #t)))) seeds)
      '())

(define verdicts
  (apply append
         (map (lambda (x)
                (let ((f (fired x)))
                  (map (lambda (p) (list x p f (accepted? x p))) (positions-of x))))
              space)))
(define (direction v)
  (cond ((eq? (caddr v) 'raised) 'raised) ((eq? (cadddr v) 'undecided) 'undecided) ((caddr v) 'looser) (else 'stricter)))
(define (exception-for v)
  (find (lambda (e) (and (equal? (car e) (car v)) (memq (cadr e) (list (cadr v) 'any)))) exceptions))
(define disagreements
  (filter (lambda (v) (not (eq? (caddr v) (cadddr v)))) verdicts))
(define unexplained
  (filter (lambda (v) (let ((e (exception-for v))) (not (and e (eq? (caddr e) (direction v))))))
          disagreements))
(printf "   space: ~a forms (seeds ~a, generated ~a, pinned ~a), ~a verdicts; disagreements ~a, unexplained ~a\n"
        (length space) (length seeds) (length generated) (length pinned) (length verdicts)
        (length disagreements) (length unexplained))
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
(want "S2 over the whole space, the walk's rule fires if and only if Chez accepts, but for the exception table"
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

(printf "\n~a failures\nrows: ~a\nname-use-shapes complete\n" bad rows)
(exit (if (= bad 0) 0 1))
