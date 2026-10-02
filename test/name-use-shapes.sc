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
;; THE REFERENCE. (expand T env) in a fresh copy of the (chezscheme)
;; environment per form, where every symbol of T that is not bound as syntax
;; there is first defined as a variable, so that only syntax is judged.
;; Auxiliary keywords (else, =>, _, unquote, the record clause words) stay
;; the keywords they are. Expansion runs nothing, and it is called only
;; here: (theourgia name-use) never expands or evaluates.
;;   T is the form itself for a known form, and (lambda () D 0) for a
;;   defining form D: the walk knows a definition at a block's top form and
;;   at the head of a body, and the body is where Chez judges one too.
;;
;; WHAT "THE RULE FIRED" MEANS: the walk did not fall back on the form. The
;; fallback walks the form's head as a name, so: the head is among the
;; uses, no proper sub-form with the same head explains it, and the head
;; occurs nowhere in the form but in head positions. For a defining form the
;; uses of (lambda () D 0) are read.
;;
;; THE SPACE, stated before the first run:
;;   SEEDS: one or two well-formed forms per head (below), each accepted by
;;     the reference and fired by the walk (the control row says so), with
;;     no other known or defining head inside it except where a body's own
;;     definition is the point of the seed.
;;   OPERATORS, each applied ONCE to a seed, at every position of every list
;;     inside it but the outer head:
;;     O1 drop the element;
;;     O2 duplicate it in place;
;;     O3 replace it by each of: zz, __zz, 7, (), (zz), (zz . zy);
;;     O4 if it is a symbol, replace it by each other symbol of the seed but
;;        the head (this makes a binder name twice, or a name used where
;;        another was);
;;     O5 swap it with the element after it;
;;     O6 insert (begin) before it, and at the end of each list;
;;     O7 insert (define zz 0) before it, and at the end of each list.
;;   PINNED: forms that are not generated but are asked the same question:
;;     the gaps recorded when the rule was last written by hand.
;; The row prints how many forms the space holds.
;;
;; THE EXCEPTION TABLE is data: (form direction reason). A disagreement it
;; lists is accepted with its reason; an entry whose form is in the space
;; and agrees is stale, which is red too. It starts with the macro keywords
;; the walk deliberately does not know: they are never seeds, and their
;; entries say so.

(import (chezscheme) (only (theourgia name-use) datum-uses))

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
    (lambda (a b) (f a b))
    (lambda (a) (define b a) (define c a) (f b c))
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
    (define (f a) (g a))
    (define x 1)
    (define-syntax m (syntax-rules () ((_ a) a)))
    (define-values (a b) (g))
    (define-record-type (p mk p?) (fields (immutable x px) (mutable y py set-py!)) (protocol (g)) (sealed #t))))

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
    (begin (begin))))

;; (form direction reason). direction: stricter (Chez accepts, the walk
;; falls back), looser (the walk fires, Chez refuses), or not-a-seed (a head
;; the walk deliberately does not know: never generated).
(define exceptions
  '(((syntax-rules) not-a-seed "a transformer is a macro of its own; the walk walks it whole by design")
    ((syntax-case) not-a-seed "as syntax-rules")
    ((let-syntax) not-a-seed "a macro scope; walked whole by design")
    ((letrec-syntax) not-a-seed "as let-syntax")))

;; ---- the generator ----------------------------------------------------------------------

(define (symbols-in x)
  (cond ((symbol? x) (list x))
        ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x))))
        ((vector? x) (apply append (map symbols-in (vector->list x))))
        (else '())))

(define (dedup xs)
  (let loop ((xs xs) (out '()))
    (cond ((null? xs) (reverse out))
          ((member (car xs) out) (loop (cdr xs) out))
          (else (loop (cdr xs) (cons (car xs) out))))))

;; Every list inside X, as the path of indices that reaches it ('() is X).
(define (list-paths x)
  (let walk ((x x) (path '()))
    (if (and (pair? x) (list? x))
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
    (cond ((= k i) (append new (if (null? l) '() (cdr l))))
          (else (cons (car l) (loop (cdr l) (+ k 1)))))))
(define (insert-before l i v)
  (let loop ((l l) (k 0))
    (cond ((= k i) (cons v l))
          (else (cons (car l) (loop (cdr l) (+ k 1)))))))

(define o3-replacements '(zz __zz 7 () (zz) (zz . zy)))

;; Each operator once at every position but the outer head. -> the forms.
(define (mutants-of seed)
  (let ((head (car seed))
        (names (dedup (filter (lambda (s) (not (eq? s (car seed)))) (symbols-in (cdr seed))))))
    (apply append
           (map (lambda (path)
                  (let* ((l (get seed path)) (n (length l)))
                    (append
                      ;; O6, O7 at the end of the list
                      (list (update seed path (lambda (l) (append l '((begin)))))
                            (update seed path (lambda (l) (append l '((define zz 0))))))
                      (apply append
                             (map (lambda (i)
                                    (if (and (null? path) (= i 0))
                                        '()
                                        (let ((e (list-ref l i)))
                                          (append
                                            (list (update seed path (lambda (l) (splice l i '())))
                                                  (update seed path (lambda (l) (splice l i (list e e)))))
                                            (map (lambda (r) (update seed path (lambda (l) (splice l i (list r)))))
                                                 o3-replacements)
                                            (if (symbol? e)
                                                (map (lambda (s) (update seed path (lambda (l) (splice l i (list s)))))
                                                     (filter (lambda (s) (not (eq? s e))) names))
                                                '())
                                            (if (< (+ i 1) n)
                                                (list (update seed path
                                                              (lambda (l)
                                                                (splice (splice l i (list (list-ref l (+ i 1))))
                                                                        (+ i 1) (list e)))))
                                                '())
                                            (list (update seed path (lambda (l) (insert-before l i '(begin))))
                                                  (update seed path (lambda (l) (insert-before l i '(define zz 0)))))))))
                                  (iota n))))))
                (list-paths seed)))))

(define generated
  (dedup (filter (lambda (x) (and (pair? x) (memq (car x) (append known-heads defining-heads))))
                 (apply append (map mutants-of seeds)))))
(define space (dedup (append seeds generated pinned)))

;; ---- the two judges --------------------------------------------------------------------

(define (test-form x) (if (memq (car x) defining-heads) (list 'lambda '() x 0) x))

;; THE REFERENCE: does Chez accept the form? (BIND-FREE? #f is the reference
;; with an environment binding nothing, for the mutant that shows the
;; reference is asked.)
(define bind-free? #t)
(define (accepted? x)
  (let ((t (test-form x)) (env (copy-environment (scheme-environment) #t)))
    (when bind-free?
      (for-each (lambda (s)
                  (unless (or (top-level-syntax? s env) (top-level-bound? s env))
                    (define-top-level-value s 0 env)))
                (dedup (symbols-in t))))
    (guard (e (#t #f)) (expand t env) #t)))

;; THE WALK: did its rule fire on the form?
(define (head-only-at-heads? h x)
  (let walk ((x x) (head? #f))
    (cond ((symbol? x) (or head? (not (eq? x h))))
          ((pair? x) (and (walk (car x) #t)
                          (let loop ((l (cdr x)))
                            (cond ((pair? l) (and (walk (car l) #f) (loop (cdr l))))
                                  ((null? l) #t)
                                  (else (walk l #f))))))
          ((vector? x) (for-all (lambda (e) (walk e #f)) (vector->list x)))
          (else #t))))
(define (proper-subforms x)
  (cond ((pair? x) (append (if (pair? (car x)) (cons (car x) (proper-subforms (car x))) (proper-subforms (car x)))
                           (proper-subforms-of-tail (cdr x))))
        (else '())))
(define (proper-subforms-of-tail l)
  (cond ((pair? l) (append (if (pair? (car l)) (cons (car l) (proper-subforms (car l))) '())
                           (proper-subforms-of-tail (cdr l))))
        (else '())))
(define (uses-of x) (guard (e (#t '(RAISED))) (datum-uses (test-form x))))
(define (fired? x)
  (let ((h (car x)))
    (not (and (memq h (uses-of x))
              (head-only-at-heads? h x)
              (not (exists (lambda (y) (and (pair? y) (eq? (car y) h) (memq h (uses-of y))))
                           (proper-subforms x)))))))

;; ---- the rows ----------------------------------------------------------------------------

(want "S0 CONTROL: the reference is asked: it accepts (lambda (x) x), refuses (lambda (x)) and (if)"
      (map accepted? '((lambda (x) x) (lambda (x)) (if)))
      '(#t #f #f))
(want "S1 CONTROL: every seed is accepted by the reference and fired by the walk, its head only at heads"
      (filter (lambda (s) (not (and (accepted? s) (fired? s) (head-only-at-heads? (car s) s)))) seeds)
      '())

(define verdicts
  (map (lambda (x) (list x (fired? x) (accepted? x))) space))
(define (exception-for x)
  (find (lambda (e) (equal? (car e) x)) exceptions))
(define disagreements
  (filter (lambda (v) (not (eq? (cadr v) (caddr v)))) verdicts))
(define unexplained
  (filter (lambda (v)
            (let ((e (exception-for (car v))))
              (not (and e (eq? (cadr e) (if (cadr v) 'looser 'stricter))))))
          disagreements))
(printf "   space: ~a forms (seeds ~a, generated ~a, pinned ~a); disagreements ~a, unexplained ~a\n"
        (length space) (length seeds) (length generated) (length pinned) (length disagreements) (length unexplained))
(for-each (lambda (v) (printf "   DISAGREE ~a ~s\n" (if (cadr v) 'looser 'stricter) (car v))) unexplained)
(want "S1 CONTROL: the space was generated (more than a thousand forms)"
      (> (length space) 1000)
      #t)
(want "S2 over the whole space, the walk's rule fires if and only if Chez accepts, but for the exception table"
      (length unexplained)
      0)
(want "S3 every exception whose form is in the space still disagrees as it says"
      (filter (lambda (e)
                (and (memq (cadr e) '(stricter looser))
                     (let ((v (assoc (car e) verdicts)))
                       (and v (or (eq? (cadr v) (caddr v))
                                  (not (eq? (cadr e) (if (cadr v) 'looser 'stricter)))))))))
              exceptions)
      '())
(want "S4 every pinned gap agrees, or is in the exception table with its reason"
      (map car (filter (lambda (v) (and (member (car v) pinned) (member v unexplained))) verdicts))
      '())

(printf "\n~a failures\nrows: ~a\nname-use-shapes complete\n" bad rows)
(exit (if (= bad 0) 0 1))
