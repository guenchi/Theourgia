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

;;; (theourgia name-use) -- which names a code block uses, and which
;;; libraries a library block imports.
;;;
;;; KEY: SYNTACTIC, AND EVERY ANSWER SAYS SO. What a block uses is what its
;;; stored code shows, never what a compiler would resolve: a datum block's
;;; body is walked as data, a text block's source is cut into the tokens its
;;; language's identifier pattern matches. Every answer built on this says
;;; `(name-use syntactic ...)`, or `(name-use none (reason <r>))` when a
;;; block could not be read this way.
;;;
;;; NEVER: NOTHING IS EVALUATED, PRINTED OR READ. The walk takes the stored
;;; value as it is; source text is never re-read and code never run.
;;;
;;; THE DATUM WALK, ONE RULE. walk(form, E), E the names bound here:
;;;   - a symbol is a use unless it is in E;
;;;   - any other atom, and a vector or bytevector literal, uses nothing;
;;;   - a pair whose head is a symbol naming a KNOWN FORM and not in E
;;;     follows that form's rule, and the head itself is not a use;
;;;   - every other pair is walked element by element with E, head
;;;     included, improper tails too.
;;; So an application and an unknown macro are the same here: a macro's
;;; pattern variables and clause words count as uses (the over-
;;; approximation), and a name only an expansion introduces is not seen
;;; (the under-approximation).
;;;
;;; THE KNOWN FORMS ARE A TABLE, head -> rule, and the walk dispatches
;;; through it: a form is added by adding an entry, never by editing the
;;; walk. A rule answers #f when the form's shape does not fit it, and the
;;; form is then walked by the fallback; no rule raises.
;;;
;;; THE DEFINING FORMS ARE KNOWN IN TWO POSITIONS ONLY: the block's own top
;;; form and the head of a body (a lambda's, a let's, a do's commands, a
;;; guard's, a parameterize's). Anywhere else, `(m (define x y))` included,
;;; they are pairs like any other.
;;;
;;; This library is entered on dispatch of `names` or `uses` only (rpc.sc's
;;; registry); the entries that name it are data in (theourgia extensions).
(library (theourgia name-use)
  (export names-verb uses-verb block-name-use name-use-table datum-uses text-uses
          import-library-name register-name-use-form! default-identifier-pattern
          name-use-rule-fires?)
  (import (rnrs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids)
          (only (theourgia store) library-locator)
          (only (theourgia project) subtree-ids)
          (only (theourgia datum-code) datum-names)
          (only (theourgia languages) language-for-name language-property)
          (only (theourgia regex) regex-compile regex-match-at)
          (only (theourgia extensions) names-usage uses-usage)
          (only (theourgia field-reading) field-of field-missing?))

  ;; ---- sets of symbols --------------------------------------------------------

  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

  ;; -> the distinct symbols of a list, sorted by their text.
  (define (symbol-set xs)
    (let ((seen (make-eq-hashtable)))
      (for-each (lambda (x) (hashtable-set! seen x #t)) xs)
      (list-sort symbol<? (vector->list (hashtable-keys seen)))))

  ;; ---- the walk -----------------------------------------------------------------

  (define (bound? s env) (memq s env))

  ;; A formals list: a list or an improper list of symbols, or one symbol,
  ;; no name twice. Anything else makes the form malformed, and it falls back.
  (define (formals? f)
    (and (let shape ((f f))
           (cond ((symbol? f) #t)
                 ((null? f) #t)
                 ((pair? f) (and (symbol? (car f)) (shape (cdr f))))
                 (else #f)))
         (distinct? (formals-names f))))

  ;; -> #t when no symbol occurs twice in NAMES.
  (define (distinct? names)
    (let ((seen (make-eq-hashtable)))
      (for-all (lambda (n) (and (not (hashtable-ref seen n #f)) (begin (hashtable-set! seen n #t) #t))) names)))

  ;; The names a formals list binds: a list, an improper list or one symbol.
  (define (formals-names f)
    (cond ((symbol? f) (list f))
          ((pair? f) (append (formals-names (car f)) (formals-names (cdr f))))
          (else '())))

  (define (walk x env)
    (cond
      ((symbol? x) (if (bound? x env) '() (list x)))
      ((pair? x)
       (let* ((h (car x))
              (rule (and (symbol? h) (not (bound? h env)) (hashtable-ref known-forms h #f)))
              (answer (and rule (list? x) (rule x env walk walk-body))))
        (or answer (walk-elements x env))))
      (else '())))

  ;; The fallback: every element, head included, an improper tail too.
  (define (walk-elements x env)
    (let loop ((l x) (out '()))
      (cond ((pair? l) (loop (cdr l) (append (walk (car l) env) out)))
            ((null? l) out)
            (else (append (walk l env) out)))))

  (define (walk-all forms env)
    (apply append (map (lambda (f) (walk f env)) forms)))

  ;; ---- the defining forms, at a top form and at the head of a body ----------------

  (define defining-heads '(define define-syntax define-record-type define-values))

  ;; A definition whose shape does not fit (a define whose formals are not
  ;; formals, define-values whose names are not formals, a define-syntax
  ;; whose name is not a symbol) is not a definition: it is walked by the
  ;; fallback, as any malformed known form is.
  (define (definition? f env)
    (and (pair? f) (symbol? (car f)) (not (bound? (car f) env)) (memq (car f) defining-heads)
         (list? f) (pair? (cdr f))
         (let ((target (cadr f)) (n (length f)))
           (case (car f)
             ((define) (if (symbol? target)
                           (<= n 3)
                           (and (pair? target) (symbol? (car target)) (formals? (cdr target)) (>= n 3)
                                (body-shape? (cddr f) (append (formals-names (cdr target)) env)))))
             ((define-syntax) (and (symbol? target) (= n 3)))
             ((define-values) (and (formals? target) (= n 3)))
             ((define-record-type) (record-definition-shape? f))
             (else #f)))))

  ;; (define-record-type <name spec> <clause> ...) as R6RS shapes it: the
  ;; name spec a symbol or three symbols; each clause one of R6RS's, each
  ;; kind at most once, parent and parent-rtd not both:
  ;;   (fields <field spec> ...)   a symbol, (immutable n [accessor]) or
  ;;                               (mutable n [accessor [mutator]])
  ;;   (parent <name>)  (protocol <expression>)  (sealed <boolean>)
  ;;   (opaque <boolean>)  (nongenerative [<uid>])  (parent-rtd <e> <e>)
  (define (record-definition-shape? f)
    (let ((spec (cadr f)) (clauses (cddr f)))
      (define (field-spec? s)
        (or (symbol? s)
            (and (pair? s) (list? s) (for-all symbol? s)
                 (case (car s)
                   ((immutable) (<= 2 (length s) 3))
                   ((mutable) (memv (length s) '(2 4)))
                   (else #f)))))
      (define (clause? c)
        (and (pair? c) (list? c)
             (case (car c)
               ((fields) (for-all field-spec? (cdr c)))
               ((parent) (and (= (length c) 2) (symbol? (cadr c))))
               ((protocol) (= (length c) 2))
               ((sealed opaque) (and (= (length c) 2) (boolean? (cadr c))))
               ((nongenerative) (or (= (length c) 1) (and (= (length c) 2) (symbol? (cadr c)))))
               ((parent-rtd) (= (length c) 3))
               (else #f))))
      (and (or (symbol? spec)
               (and (list? spec) (= (length spec) 3) (for-all symbol? spec)))
           (for-all clause? clauses)
           (distinct? (map car clauses))
           ;; The constructor, the predicate, the accessors and the mutators,
           ;; defaults derived, are distinct, as Chez requires of one record
           ;; definition at the top level; the record name and the field
           ;; names may equal any of them. In a body the body rule refuses
           ;; any name defined twice, the record name included.
           (let ((names (datum-names f)))
             (or (null? names) (distinct? (cdr names))))
           (not (and (assq 'parent clauses) (assq 'parent-rtd clauses))))))

  ;; The names a definition defines. define-values' formals are its names;
  ;; the others are datum-names', the one reading of a definition's names.
  (define (defined-names d)
    (if (eq? (car d) 'define-values)
        (formals-names (cadr d))
        (datum-names d)))

  ;; The uses inside one definition, with ENV holding every name bound at
  ;; its position (a body's own definitions included).
  (define (walk-definition d env)
    (case (car d)
      ((define)
       (let ((target (cadr d)))
         (if (pair? target)
             (walk-body (cddr d) (append (formals-names (cdr target)) env))
             (walk-all (cddr d) env))))
      ((define-syntax) (walk-all (cddr d) env))
      ((define-values) (walk-all (cddr d) env))
      ((define-record-type)
       (apply append
              (map (lambda (clause)
                     (if (and (pair? clause) (symbol? (car clause)) (list? clause)
                              (memq (car clause) '(protocol parent-rtd parent)))
                         (walk-all (cdr clause) env)
                         '()))
                   (cddr d))))
      (else '())))

  ;; A BODY: the definitions at its head, begin-spliced ones included, are
  ;; collected first and bound in the whole body, their own right-hand sides
  ;; included. A form whose head is bound here is not a definition.
  ;; Every begin in the body is spliced first, nested and empty ones too, as
  ;; the expander splices them; a begin among the expressions walks to the
  ;; same names spliced or not.
  ;; A BODY AS CHEZ SHAPES IT: definitions, then at least one expression.
  ;; In the definition part a begin splices (its forms are taken in its
  ;; place, an empty one is nothing), and a name a definition binds is bound
  ;; for the forms after it, so after (define begin list) a begin is an
  ;; application. From the first form that is not a definition on, every
  ;; form is an expression: a definition there, or a begin that is empty or
  ;; holds one, makes the body malformed, and so does a name defined twice.
  ;; -> (definitions . expressions), or #f.
  (define (body-split forms env)
    (and (list? forms)
         (let loop ((fs forms) (defs '()) (env env))
           (cond
             ((null? fs) #f)
             ((and (pair? (car fs)) (eq? (car (car fs)) 'begin) (not (bound? 'begin env)) (list? (car fs)))
              (loop (append (cdr (car fs)) (cdr fs)) defs env))
             ((definition? (car fs) env)
              (loop (cdr fs) (cons (car fs) defs) (append (defined-names (car fs)) env)))
             (else
              (let ((defs (reverse defs)))
                (and (distinct? (apply append (map defined-names defs)))
                     (for-all (lambda (f) (expression-form? f env)) fs)
                     (cons defs fs))))))))

  ;; A form in an expression position of a body or a command sequence: not a
  ;; definition, and not a begin that is empty or holds a definition.
  (define (expression-form? f env)
    (cond
      ((and (pair? f) (symbol? (car f)) (not (bound? (car f) env)) (memq (car f) defining-heads)) #f)
      ((and (pair? f) (eq? (car f) 'begin) (not (bound? 'begin env)) (list? f))
       (and (pair? (cdr f)) (for-all (lambda (g) (expression-form? g env)) (cdr f))))
      (else #t)))

  (define (body-shape? forms env) (and (body-split forms env) #t))

  (define (walk-body forms env)
    (let ((split (body-split forms env)))
      (if (not split)
          (if (list? forms) (walk-all forms env) (walk-elements forms env))
          (let* ((defs (car split))
                 (env2 (append (apply append (map defined-names defs)) env)))
            (append (apply append (map (lambda (d) (walk-definition d env2)) defs))
                    (walk-all (cdr split) env2))))))

  ;; A BLOCK'S TOP FORM: a definition, a begin, which splices (its forms are
  ;; top forms, an empty one is nothing), or an expression.
  (define (top-begin? body) (and (pair? body) (eq? (car body) 'begin) (list? body)))
  (define (top-uses body)
    (cond ((definition? body '()) (walk-definition body '()))
          ((top-begin? body) (apply append (map top-uses (cdr body))))
          (else (walk body '()))))
  (define (datum-uses body) (symbol-set (top-uses body)))

  ;; ---- the known forms ----------------------------------------------------------
  ;;
  ;; A rule is (lambda (form env walk walk-body) ...) -> the uses, or #f when
  ;; the form's shape does not fit (it is then walked by the fallback).

  (define known-forms (make-eq-hashtable))

  (define (register-name-use-form! head rule)
    (unless (and (symbol? head) (procedure? rule))
      (assertion-violation 'register-name-use-form! "a symbol and a procedure" head rule))
    (hashtable-set! known-forms head rule))

  ;; A let-family binding: (name init), exactly.
  (define (binding? b) (and (pair? b) (symbol? (car b)) (list? b) (= (length b) 2)))
  (define (bindings? bs) (and (list? bs) (for-all binding? bs)))
  (define (binding-inits bs env) (apply append (map (lambda (b) (walk-all (cdr b) env)) bs)))

  ;; quasiquote's template, at a depth: unquote and unquote-splicing at depth 1
  ;; walk their expression; a nested quasiquote raises the depth. Each of the
  ;; three is template syntax only where it is not bound: under
  ;; (let ((unquote list)) ...) an (unquote x) in a template is data.
  (define (template x depth env)
    (cond
      ((and (pair? x) (memq (car x) '(unquote unquote-splicing)) (not (bound? (car x) env)) (list? x) (pair? (cdr x)))
       (if (= depth 1) (walk-all (cdr x) env) (template (cdr x) (- depth 1) env)))
      ((and (pair? x) (eq? (car x) 'quasiquote) (not (bound? 'quasiquote env)) (pair? (cdr x)) (null? (cddr x)))
       (template (cadr x) (+ depth 1) env))
      ((pair? x) (append (template (car x) depth env) (template (cdr x) depth env)))
      ((vector? x) (apply append (map (lambda (e) (template e depth env)) (vector->list x))))
      (else '())))

  ;; A clause of cond, or of guard: `else` at its head and `=>` as its second
  ;; element, both positions read on the clause as written, are not uses
  ;; unless bound; every other element is walked.
  (define (cond-clause c env)
    (let ((else? (and (eq? (car c) 'else) (not (bound? 'else env))))
          (arrow? (and (pair? (cdr c)) (eq? (cadr c) '=>) (not (bound? '=> env)))))
      (append (if else? '() (walk (car c) env))
              (if (pair? (cdr c)) (if arrow? '() (walk (cadr c) env)) '())
              (if (pair? (cdr c)) (walk-all (cddr c) env) '()))))

  ;; Chez's foreign type names, as its User's Guide lists them for
  ;; foreign-procedure, and the two ftype forms.
  (define foreign-type-names
    '(integer-8 unsigned-8 integer-16 unsigned-16 integer-24 unsigned-24 integer-32 unsigned-32
      integer-40 unsigned-40 integer-48 unsigned-48 integer-56 unsigned-56 integer-64 unsigned-64
      short unsigned-short int unsigned unsigned-int long unsigned-long long-long unsigned-long-long
      char wchar_t wchar float double single-float double-float size_t ssize_t ptrdiff_t
      iptr uptr void* boolean fixnum string wstring utf-8 utf-16 utf-16le utf-16be
      utf-32 utf-32le utf-32be u8* u16* u32* scheme-object))
  (define (foreign-type? t)
    (or (and (symbol? t) (memq t foreign-type-names) #t)
        (and (list? t) (= (length t) 2) (memq (car t) '(* &)) (symbol? (cadr t)))))

  ;; -> the first K elements of the list L.
  (define (first-n l k) (if (= k 0) '() (cons (car l) (first-n (cdr l) (- k 1)))))

  (define (lists? xs) (and (list? xs) (for-all (lambda (c) (and (pair? c) (list? c))) xs)))

  ;; The clauses of a cond or a guard, as R6RS shapes them: at least one;
  ;; an else clause last and with an expression; an arrow clause exactly
  ;; (test => receiver).
  (define (cond-clauses-shape? clauses env)
    (and (lists? clauses) (pair? clauses)
         (let loop ((cs clauses))
           (or (null? cs)
               (let* ((c (car cs))
                      (else? (and (eq? (car c) 'else) (not (bound? 'else env))))
                      (arrow? (and (pair? (cdr c)) (eq? (cadr c) '=>) (not (bound? '=> env)))))
                 (and (cond (else? (and (null? (cdr cs)) (>= (length c) 2)))
                            (arrow? (= (length c) 3))
                            (else #t))
                      (loop (cdr cs))))))))

  ;; The clauses of a case: at least one, each (<datums> <expression> ...),
  ;; the datums a list or, as Chez accepts, one datum; an else clause where
  ;; Chez accepts one, anywhere, clauses after it included.
  (define (case-clauses-shape? clauses env)
    (and (lists? clauses) (pair? clauses)
         (let loop ((cs clauses))
           (or (null? cs)
               (let ((c (car cs)))
                 (and (>= (length c) 2)
                      (loop (cdr cs))))))))

  ;; A definition rather than a bare expression: a library body's
  ;; definitions may not follow an expression.
  (define built-in-forms-registered
    (for-each
     (lambda (entry) (register-name-use-form! (car entry) (cdr entry)))
     (list
      (cons 'quote (lambda (x env walk body) (and (= (length x) 2) '())))
      (cons 'quasiquote
            (lambda (x env walk body)
              (and (= (length x) 2) (template (cadr x) 1 env))))
      (cons 'lambda
            (lambda (x env walk body)
              (and (>= (length x) 3) (formals? (cadr x))
                   (body-shape? (cddr x) (append (formals-names (cadr x)) env))
                   (body (cddr x) (append (formals-names (cadr x)) env)))))
      (cons 'case-lambda
            (lambda (x env walk body)
              (and (lists? (cdr x))
                   (for-all (lambda (c) (and (formals? (car c)) (body-shape? (cdr c) (append (formals-names (car c)) env))))
                            (cdr x))
                   (apply append (map (lambda (c) (body (cdr c) (append (formals-names (car c)) env)))
                                      (cdr x))))))
      (cons 'let
            (lambda (x env walk body)
              (cond
                ((and (>= (length x) 4) (symbol? (cadr x)) (bindings? (caddr x)) (distinct? (map car (caddr x)))
                      (body-shape? (cdddr x) (append (list (cadr x)) (map car (caddr x)) env)))
                 (let ((bs (caddr x)))
                   (append (binding-inits bs env)
                           (body (cdddr x) (append (list (cadr x)) (map car bs) env)))))
                ((and (>= (length x) 3) (bindings? (cadr x)) (distinct? (map car (cadr x)))
                      (body-shape? (cddr x) (append (map car (cadr x)) env)))
                 (let ((bs (cadr x)))
                   (append (binding-inits bs env) (body (cddr x) (append (map car bs) env)))))
                (else #f))))
      (cons 'let*
            (lambda (x env walk body)
              (and (>= (length x) 3) (bindings? (cadr x))
                   (body-shape? (cddr x) (append (reverse (map car (cadr x))) env))
                   (let loop ((bs (cadr x)) (env env) (out '()))
                     (if (null? bs)
                         (append out (body (cddr x) env))
                         (loop (cdr bs) (cons (car (car bs)) env)
                               (append out (walk-all (cdr (car bs)) env))))))))
      (cons 'letrec
            (lambda (x env walk body)
              (and (>= (length x) 3) (bindings? (cadr x)) (distinct? (map car (cadr x)))
                   (body-shape? (cddr x) (append (map car (cadr x)) env))
                   (let ((env2 (append (map car (cadr x)) env)))
                     (append (binding-inits (cadr x) env2) (body (cddr x) env2))))))
      (cons 'letrec*
            (lambda (x env walk body)
              (and (>= (length x) 3) (bindings? (cadr x)) (distinct? (map car (cadr x)))
                   (body-shape? (cddr x) (append (map car (cadr x)) env))
                   (let ((env2 (append (map car (cadr x)) env)))
                     (append (binding-inits (cadr x) env2) (body (cddr x) env2))))))
      (cons 'let-values
            (lambda (x env walk body)
              (and (>= (length x) 3) (lists? (cadr x))
                   (for-all (lambda (b) (and (= (length b) 2) (formals? (car b)))) (cadr x))
                   (distinct? (apply append (map (lambda (b) (formals-names (car b))) (cadr x))))
                   (body-shape? (cddr x) (append (apply append (map (lambda (b) (formals-names (car b))) (cadr x))) env))
                   (append (apply append (map (lambda (b) (walk (cadr b) env)) (cadr x)))
                           (body (cddr x) (append (apply append (map (lambda (b) (formals-names (car b))) (cadr x)))
                                                  env))))))
      (cons 'let*-values
            (lambda (x env walk body)
              (and (>= (length x) 3) (lists? (cadr x))
                   (for-all (lambda (b) (and (= (length b) 2) (formals? (car b)))) (cadr x))
                   (body-shape? (cddr x) (append (apply append (map (lambda (b) (formals-names (car b))) (cadr x))) env))
                   (let loop ((bs (cadr x)) (env env) (out '()))
                     (if (null? bs)
                         (append out (body (cddr x) env))
                         (loop (cdr bs) (append (formals-names (car (car bs))) env)
                               (append out (walk (cadr (car bs)) env))))))))
      (cons 'do
            (lambda (x env walk body)
              (and (>= (length x) 3) (lists? (cadr x)) (list? (caddr x)) (pair? (caddr x))
                   (for-all (lambda (s) (and (symbol? (car s)) (<= 2 (length s) 3))) (cadr x))
                   (distinct? (map car (cadr x)))
                   (for-all (lambda (f) (expression-form? f (append (map car (cadr x)) env))) (cdddr x))
                   (let* ((specs (cadr x)) (env2 (append (map car specs) env)))
                     (append (apply append (map (lambda (s) (walk (cadr s) env)) specs))
                             (apply append (map (lambda (s) (walk-all (cddr s) env2)) specs))
                             (walk-all (caddr x) env2)
                             (walk-all (cdddr x) env2))))))
      ;; begin as an expression holds at least one (a body's begins are
      ;; spliced before this is reached); if has a test, a consequent and at
      ;; most an alternative; when and unless a test and an expression.
      (cons 'begin (lambda (x env walk body) (and (>= (length x) 2) (walk-all (cdr x) env))))
      (cons 'if (lambda (x env walk body) (and (<= 3 (length x) 4) (walk-all (cdr x) env))))
      (cons 'and (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'or (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'when (lambda (x env walk body) (and (>= (length x) 3) (walk-all (cdr x) env))))
      (cons 'unless (lambda (x env walk body) (and (>= (length x) 3) (walk-all (cdr x) env))))
      (cons 'set!
            (lambda (x env walk body)
              (and (= (length x) 3) (symbol? (cadr x)) (walk-all (cdr x) env))))
      (cons 'cond
            (lambda (x env walk body)
              (and (cond-clauses-shape? (cdr x) env)
                   (apply append (map (lambda (c) (cond-clause c env)) (cdr x))))))
      (cons 'case
            (lambda (x env walk body)
              (and (>= (length x) 3) (case-clauses-shape? (cddr x) env)
                   (append (walk (cadr x) env)
                           (apply append
                                  (map (lambda (c) (walk-all (cdr c) env)) (cddr x)))))))
      (cons 'guard
            (lambda (x env walk body)
              (and (>= (length x) 3) (pair? (cadr x)) (symbol? (car (cadr x))) (list? (cadr x))
                   (cond-clauses-shape? (cdr (cadr x)) (cons (car (cadr x)) env))
                   (body-shape? (cddr x) env)
                   (let ((env2 (cons (car (cadr x)) env)))
                     (append (apply append (map (lambda (c) (cond-clause c env2)) (cdr (cadr x))))
                             (body (cddr x) env))))))
      (cons 'parameterize
            (lambda (x env walk body)
              (and (>= (length x) 3) (lists? (cadr x)) (for-all (lambda (b) (= (length b) 2)) (cadr x))
                   (body-shape? (cddr x) env)
                   (append (apply append (map (lambda (b) (walk-all b env)) (cadr x)))
                           (body (cddr x) env)))))
      (cons 'rec
            (lambda (x env walk body)
              (and (= (length x) 3) (symbol? (cadr x)) (walk (caddr x) (cons (cadr x) env)))))
      ;; (foreign-procedure <convention> ... <entry> (<parameter type> ...)
      ;; <result type>): the types are data, the entry is walked. A type is
      ;; one of Chez's foreign type names, or (* <name>) or (& <name>) for an
      ;; ftype, whose name the walk cannot check; void only as the result.
      ;; The conventions, as Chez answers them: each is #f, __collect_safe,
      ;; __varargs or (__varargs_after <n>); none is given twice (#f #f
      ;; included); at most one is __varargs or __varargs_after; __varargs
      ;; needs a parameter, and (__varargs_after <n>) an exact n with
      ;; 1 <= n <= the parameter count; #f and __collect_safe go with any
      ;; other.
      (cons 'foreign-procedure
            (lambda (x env walk body)
              (let ((n (length x)))
                (and (>= n 4)
                     (let ((params (list-ref x (- n 2)))
                           (conventions (first-n (cdr x) (- n 4))))
                       (and (list? params) (for-all foreign-type? params)
                            (let distinct ((cs conventions))
                              (or (null? cs) (and (not (member (car cs) (cdr cs))) (distinct (cdr cs)))))
                            (<= (length (filter (lambda (c) (or (eq? c '__varargs) (pair? c))) conventions)) 1)
                            (for-all (lambda (c)
                                       (or (not c)
                                           (eq? c '__collect_safe)
                                           (and (eq? c '__varargs) (pair? params))
                                           (and (list? c) (= (length c) 2) (eq? (car c) '__varargs_after)
                                                (integer? (cadr c)) (exact? (cadr c))
                                                (<= 1 (cadr c) (length params)))))
                                     conventions)))
                     (or (eq? (list-ref x (- n 1)) 'void) (foreign-type? (list-ref x (- n 1))))
                     (walk (list-ref x (- n 3)) env))))))))

  ;; DOES THE WALK KNOW THIS FORM'S SHAPE: #t when, at a block's top form, the
  ;; known form's rule fires on FORM, or FORM is a definition; #f when the walk
  ;; would fall back. It decides nothing new: it asks the rule the walk asks,
  ;; in the empty environment of a block's top form, and is exported for the
  ;; fixture that compares the walk's shapes with Chez's expander.
  (define (name-use-rule-fires? form)
    (cond
      ((not (and (pair? form) (symbol? (car form)) (list? form))) #f)
      ((memq (car form) defining-heads) (and (definition? form '()) #t))
      ((top-begin? form) #t)
      (else (let ((rule (hashtable-ref known-forms (car form) #f)))
              (and rule (rule form '() walk walk-body) #t)))))

  ;; ---- text code: the identifier tokens -------------------------------------------
  ;;
  ;; NEVER: NOTHING IS STRIPPED. Every token of the text that matches the
  ;; language's identifier pattern is a use, in a comment or a string as much
  ;; as in code: a lexer that knows one language's comments and strings is
  ;; that language's own item. The answer says (lexing whole-text).
  ;; A token is the longest match of the pattern starting where the scan is;
  ;; a character that starts no match is passed over, so in `9abc` the token
  ;; is `abc`. The match is anchored at the scan position and reads the text
  ;; in place, no copy taken; it reads at most 4096 characters, the bounded
  ;; matcher's own limit, so a token longer than that is read as more than
  ;; one.

  (define default-identifier-pattern "[A-Za-z_][A-Za-z0-9_]*")
  (define longest-token 4096)

  ;; -> the distinct tokens, as symbols, sorted.
  (define (text-uses text pattern)
    (let* ((ast (regex-compile (string-append "(" pattern ")")))
           (n (string-length text)))
      (let loop ((i 0) (out '()))
        (if (>= i n)
            (symbol-set out)
            (let* ((caps (regex-match-at ast text i (min n (+ i longest-token))))
                   (span (and caps (assv 1 caps)))
                   (end (and span (cddr span))))
              (if (and end (> end i))
                  (loop end (cons (string->symbol (substring text i end)) out))
                  (loop (+ i 1) out)))))))

  ;; ---- imports ----------------------------------------------------------------------
  ;;
  ;; -> the library name an import spec names, or #f when it does not reduce:
  ;; only, except, prefix, rename and for (with its levels) are removed to any
  ;; depth, the library wrapper too, and a trailing version reference dropped.
  ;; A name that starts with one of those words is written inside the
  ;; library wrapper (R6RS 7.1), so `(only)` bare is a malformed spec, not
  ;; the library (only).
  (define (import-library-name spec)
    (cond
      ((not (and (pair? spec) (list? spec))) #f)
      ((memq (car spec) '(only except prefix rename for))
       (and (pair? (cdr spec)) (import-library-name (cadr spec))))
      ((eq? (car spec) 'library)
       (and (= (length spec) 2) (library-name-proper (cadr spec))))
      (else (library-name-proper spec))))

  (define (library-name-proper name)
    (and (pair? name) (list? name)
         (let* ((last (list-ref name (- (length name) 1)))
                (parts (if (and (list? last) (> (length name) 1)) (reverse (cdr (reverse name))) name)))
           (and (pair? parts) (for-all symbol? parts) parts))))

  ;; ---- one block --------------------------------------------------------------------

  ;; THE REDUCER'S CONFLICT, BY ITS WHOLE SHAPE: (conflict (<candidate> ...))
  ;; with two candidates or more, each (value writer seq). The head alone
  ;; would take a stored application of a procedure named conflict for one.
  ;; Not field-reading's conflict-form?, which knows a conflict by its head
  ;; alone: a stored body is code, and (conflict x) is an application there.
  (define (reducer-conflict-value? v)
    (and (list? v) (= (length v) 2) (eq? (car v) 'conflict)
         (list? (cadr v)) (>= (length (cadr v)) 2)
         (for-all (lambda (c) (and (list? c) (= (length c) 3) (string? (cadr c))
                                   (integer? (caddr c)) (exact? (caddr c))))
                  (cadr v))))

  ;; -> ((names <sym> ...) (imports <lib> ...) (import-unreadable <d> ...)
  ;;     (contract syntactic [(lexing whole-text)] | none (reason <r>)))
  ;; for the block ID of STATE, or #f when STATE has no such block.
  ;; NEVER: A BLOCK THAT CANNOT BE READ IS AN ANSWER, NOT A FAILURE OF THE
  ;; VERB, and the catch-all below is that rule. A stored row can hold what
  ;; no reader expects -- a fields list that is not a proper list, a record
  ;; definition whose field specs are malformed so that datum-names raises
  ;; on it -- and one such block must not take `uses` down for the whole
  ;; store. It answers (name-use none (reason unreadable-code)), which
  ;; `uses` counts under skipped.
  (define (block-name-use state id)
    (let ((row (state-read state id)))
      (and row
           (guard (e (#t (none 'unreadable-code)))
             (block-answer row)))))

  (define (block-answer row)
           (let ((kind (field-of row 'kind)))
             (cond
               ((cdr (assq 'deleted row)) (none 'deleted))
               ((eq? kind 'library) (library-answer (field-of row 'imports)))
               ((not (eq? kind 'code)) (none 'not-code))
               ((eq? (field-of row 'mode) 'datum)
                (let ((body (field-of row 'body)))
                  (if (or (field-missing? body) (reducer-conflict-value? body))
                      (none 'unreadable-code)
                      (answer (datum-uses body) '() '() '(syntactic)))))
               (else
                (let* ((lang (field-of row 'lang))
                       (entry (and (or (symbol? lang) (string? lang)) (language-for-name lang)))
                       (src (field-of row 'src))
                       (text (cond ((string? src) src) ((bytevector? src) (utf8->string src)) (else #f))))
                  (cond
                    ((not entry) (none 'no-language))
                    ((not text) (none 'unreadable-code))
                    (else
                     (answer (text-uses text (language-property entry 'identifier default-identifier-pattern))
                             '() '() '(syntactic (lexing whole-text))))))))))

  (define (answer names imports unreadable contract)
    (list (cons 'names names) (cons 'imports imports) (cons 'import-unreadable unreadable)
          (cons 'contract contract)))
  (define (none reason) (answer '() '() '() (list 'none (list 'reason reason))))

  (define (library-answer imports)
    (if (not (list? imports))
        (answer '() '() (if (field-missing? imports) '() (list imports)) '(syntactic))
        (let loop ((specs imports) (names '()) (bad '()))
          (if (null? specs)
              (answer '() (reverse names) (reverse bad) '(syntactic))
              (let ((n (import-library-name (car specs))))
                (if n
                    (loop (cdr specs) (if (member n names) names (cons n names)) bad)
                    (loop (cdr specs) names (cons (car specs) bad))))))))

  ;; ---- every block, for one request -------------------------------------------------
  ;;
  ;; NEVER: NO INDEX IS KEPT. One table per request, from the state the request
  ;; holds, dropped with the answer: reading and walking every block of a
  ;; 2326-block store measured 15 and 4 ms. -> a vector:
  ;;   #(<name text -> block ids> <id -> mode> <id -> reason>)
  ;; over the live blocks; a reason is no-language or unreadable-code.
  ;; Everything is read from the provider's answers, never from a row again:
  ;; a row the provider could not read must not take the table down here.
  (define (name-use-table state)
    (let ((by-name (make-hashtable string-hash string=?))
          (modes (make-hashtable string-hash string=?))
          (skipped (make-hashtable string-hash string=?)))
      (for-each
        (lambda (id)
          (let* ((u (block-name-use state id))
                 (contract (cdr (assq 'contract u))))
            (if (eq? (car contract) 'none)
                (let ((reason (cadr (cadr contract))))
                  (when (memq reason '(no-language unreadable-code))
                    (hashtable-set! skipped id reason)))
                (begin
                  (hashtable-set! modes id (if (member '(lexing whole-text) (cdr contract)) 'text 'datum))
                  (for-each (lambda (n)
                              (let ((k (symbol->string n)))
                                (hashtable-set! by-name k (cons id (hashtable-ref by-name k '())))))
                            (cdr (assq 'names u)))))))
        (state-block-ids state))
      (vector by-name modes skipped)))

  ;; ---- the verbs ----------------------------------------------------------------------

  ;; A block's kind as `uses` reads it for the library column: a deleted
  ;; library is no library, so a block left under one says `none`.
  (define (live-kind view)
    (lambda (id)
      (let ((row (state-read view id)))
        (and row (not (cdr (assq 'deleted row)))
             (guard (e (#t #f)) (field-of row 'kind))))))

  (define (names-verb store actor args req options state writer cwd)
    (if (not (and (= 1 (length args)) (> (string-length (car args)) 0)))
        ((dispatch-helper 'usage) names-usage)
        ((dispatch-helper 'guarded)
         (lambda ()
           (let* ((view ((dispatch-helper 'reduction-for) store state))
                  (u (block-name-use view (car args))))
             (if (not u)
                 ((dispatch-helper 'unknown-id) view (car args))
                 (append ((dispatch-helper 'items)
                          (append (map (lambda (n) (list 'name n)) (cdr (assq 'names u)))
                                  (map (lambda (l) (list 'import l)) (cdr (assq 'imports u)))
                                  (map (lambda (d) (list 'import-unreadable d)) (cdr (assq 'import-unreadable u)))))
                         (list (cons 'name-use (cdr (assq 'contract u))))
                         ((dispatch-helper 'receipt) view (list (car args))))))))))

  (define (uses-verb store actor args req options state writer cwd)
    (let ((under (argument-option options "--under")))
      (if (or (not (= 1 (length args))) (= 0 (string-length (car args)))
              (and under (= 0 (string-length under))))
          ((dispatch-helper 'usage) uses-usage)
          ((dispatch-helper 'guarded)
           (lambda ()
             (let* ((view ((dispatch-helper 'reduction-for) store state))
                    (scope (and under (subtree-ids view under))))
               (if (and under (not scope))
                   ((dispatch-helper 'unknown-id) view under)
                   (uses-answer view (car args) scope))))))))

  (define (uses-answer view name scope)
    (let* ((table (name-use-table view))
           ;; The scope as a set, built once: every hit and every skipped
           ;; block is asked about it.
           (scope-set (and scope (let ((h (make-hashtable string-hash string=?)))
                                   (for-each (lambda (id) (hashtable-set! h id #t)) scope)
                                   h)))
           (in-scope? (lambda (id) (or (not scope-set) (hashtable-ref scope-set id #f))))
           (library-of (library-locator view (live-kind view)))
           (ids (list-sort string<? (filter in-scope? (hashtable-ref (vector-ref table 0) name '()))))
           (skipped (let-values (((ks vs) (hashtable-entries (vector-ref table 2))))
                      (filter (lambda (p) (in-scope? (car p))) (map cons (vector->list ks) (vector->list vs)))))
           (count (lambda (reason) (length (filter (lambda (p) (eq? (cdr p) reason)) skipped))))
           (counts (filter (lambda (c) (> (cadr c) 0))
                           (list (list 'no-language (count 'no-language))
                                 (list 'unreadable-code (count 'unreadable-code))))))
      (append ((dispatch-helper 'items)
               (map (lambda (id)
                      (list 'use id
                            (list 'library (or (library-of id) 'none))
                            (list 'mode (hashtable-ref (vector-ref table 1) id 'text))))
                    ids))
              (list '(name-use syntactic))
              (if (null? counts) '() (list (cons 'skipped counts)))
              ;; Each block listed as a use.
              ((dispatch-helper 'receipt) view ids)))))
