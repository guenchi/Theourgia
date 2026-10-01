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
          import-library-name register-name-use-form! default-identifier-pattern)
  (import (rnrs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids)
          (only (theourgia store) library-locator)
          (only (theourgia project) subtree-ids)
          (only (theourgia datum-code) datum-names)
          (only (theourgia languages) language-for-name language-property)
          (only (theourgia regex) regex-compile regex-match)
          (only (theourgia extensions) names-usage uses-usage))

  ;; ---- sets of symbols --------------------------------------------------------

  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

  ;; -> the distinct symbols of a list, sorted by their text.
  (define (symbol-set xs)
    (let ((seen (make-eq-hashtable)))
      (for-each (lambda (x) (hashtable-set! seen x #t)) xs)
      (list-sort symbol<? (vector->list (hashtable-keys seen)))))

  ;; ---- the walk -----------------------------------------------------------------

  (define (bound? s env) (memq s env))

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

  (define (definition? f env)
    (and (pair? f) (symbol? (car f)) (not (bound? (car f) env)) (memq (car f) defining-heads)
         (list? f) (pair? (cdr f))))

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
         (cond
           ((and (pair? target) (symbol? (car target)))
            (walk-body (cddr d) (append (formals-names (cdr target)) env)))
           ((pair? (cddr d)) (walk-all (cddr d) env))
           (else '()))))
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
  (define (walk-body forms env)
    (let collect ((fs forms) (defs '()))
      (cond
        ((and (pair? fs) (definition? (car fs) env))
         (collect (cdr fs) (cons (car fs) defs)))
        ((and (pair? fs) (pair? (car fs)) (eq? (car (car fs)) 'begin) (not (bound? 'begin env))
              (list? (car fs)) (pair? (cdr (car fs)))
              (for-all (lambda (g) (definition? g env)) (cdr (car fs))))
         (collect (cdr fs) (append (reverse (cdr (car fs))) defs)))
        (else
         (let* ((defs (reverse defs))
                (env2 (append (apply append (map defined-names defs)) env)))
           (append (apply append (map (lambda (d) (walk-definition d env2)) defs))
                   (if (list? fs) (walk-all fs env2) (walk-elements fs env2))))))))

  ;; -> the names a datum block's stored body uses, a sorted set.
  (define (datum-uses body)
    (symbol-set (if (definition? body '()) (walk-definition body '()) (walk body '()))))

  ;; ---- the known forms ----------------------------------------------------------
  ;;
  ;; A rule is (lambda (form env walk walk-body) ...) -> the uses, or #f when
  ;; the form's shape does not fit (it is then walked by the fallback).

  (define known-forms (make-eq-hashtable))

  (define (register-name-use-form! head rule)
    (unless (and (symbol? head) (procedure? rule))
      (assertion-violation 'register-name-use-form! "a symbol and a procedure" head rule))
    (hashtable-set! known-forms head rule))

  (define (binding? b) (and (pair? b) (symbol? (car b)) (list? b) (<= (length b) 2)))
  (define (bindings? bs) (and (list? bs) (for-all binding? bs)))
  (define (binding-inits bs env) (apply append (map (lambda (b) (walk-all (cdr b) env)) bs)))

  ;; quasiquote's template, at a depth: unquote and unquote-splicing at depth 1
  ;; walk their expression; a nested quasiquote raises the depth.
  (define (template x depth env)
    (cond
      ((and (pair? x) (memq (car x) '(unquote unquote-splicing)) (pair? (cdr x)) (null? (cddr x)))
       (if (= depth 1) (walk (cadr x) env) (template (cadr x) (- depth 1) env)))
      ((and (pair? x) (eq? (car x) 'quasiquote) (pair? (cdr x)) (null? (cddr x)))
       (template (cadr x) (+ depth 1) env))
      ((pair? x) (append (template (car x) depth env) (template (cdr x) depth env)))
      ((vector? x) (apply append (map (lambda (e) (template e depth env)) (vector->list x))))
      (else '())))

  ;; A clause of cond, or of guard: `else` at its head and `=>` as its second
  ;; element are not uses unless bound; every other element is walked.
  (define (cond-clause c env)
    (let* ((c (if (and (eq? (car c) 'else) (not (bound? 'else env))) (cdr c) c))
           (c (if (and (pair? c) (pair? (cdr c)) (eq? (cadr c) '=>) (not (bound? '=> env)))
                  (cons (car c) (cddr c))
                  c)))
      (walk-all c env)))

  (define (lists? xs) (and (list? xs) (for-all (lambda (c) (and (pair? c) (list? c))) xs)))

  ;; A definition rather than a bare expression: a library body's
  ;; definitions may not follow an expression.
  (define built-in-forms-registered
    (for-each
     (lambda (entry) (register-name-use-form! (car entry) (cdr entry)))
     (list
      (cons 'quote (lambda (x env walk body) '()))
      (cons 'quasiquote
            (lambda (x env walk body)
              (and (= (length x) 2) (template (cadr x) 1 env))))
      (cons 'lambda
            (lambda (x env walk body)
              (and (>= (length x) 2) (body (cddr x) (append (formals-names (cadr x)) env)))))
      (cons 'case-lambda
            (lambda (x env walk body)
              (and (lists? (cdr x))
                   (apply append (map (lambda (c) (body (cdr c) (append (formals-names (car c)) env)))
                                      (cdr x))))))
      (cons 'let
            (lambda (x env walk body)
              (cond
                ((and (>= (length x) 3) (symbol? (cadr x)) (bindings? (caddr x)))
                 (let ((bs (caddr x)))
                   (append (binding-inits bs env)
                           (body (cdddr x) (append (list (cadr x)) (map car bs) env)))))
                ((and (>= (length x) 2) (bindings? (cadr x)))
                 (let ((bs (cadr x)))
                   (append (binding-inits bs env) (body (cddr x) (append (map car bs) env)))))
                (else #f))))
      (cons 'let*
            (lambda (x env walk body)
              (and (>= (length x) 2) (bindings? (cadr x))
                   (let loop ((bs (cadr x)) (env env) (out '()))
                     (if (null? bs)
                         (append out (body (cddr x) env))
                         (loop (cdr bs) (cons (car (car bs)) env)
                               (append out (walk-all (cdr (car bs)) env))))))))
      (cons 'letrec
            (lambda (x env walk body)
              (and (>= (length x) 2) (bindings? (cadr x))
                   (let ((env2 (append (map car (cadr x)) env)))
                     (append (binding-inits (cadr x) env2) (body (cddr x) env2))))))
      (cons 'letrec*
            (lambda (x env walk body)
              (and (>= (length x) 2) (bindings? (cadr x))
                   (let ((env2 (append (map car (cadr x)) env)))
                     (append (binding-inits (cadr x) env2) (body (cddr x) env2))))))
      (cons 'let-values
            (lambda (x env walk body)
              (and (>= (length x) 2) (lists? (cadr x))
                   (for-all (lambda (b) (= (length b) 2)) (cadr x))
                   (append (apply append (map (lambda (b) (walk (cadr b) env)) (cadr x)))
                           (body (cddr x) (append (apply append (map (lambda (b) (formals-names (car b))) (cadr x)))
                                                  env))))))
      (cons 'let*-values
            (lambda (x env walk body)
              (and (>= (length x) 2) (lists? (cadr x))
                   (for-all (lambda (b) (= (length b) 2)) (cadr x))
                   (let loop ((bs (cadr x)) (env env) (out '()))
                     (if (null? bs)
                         (append out (body (cddr x) env))
                         (loop (cdr bs) (append (formals-names (car (car bs))) env)
                               (append out (walk (cadr (car bs)) env))))))))
      (cons 'do
            (lambda (x env walk body)
              (and (>= (length x) 3) (lists? (cadr x)) (list? (caddr x))
                   (for-all (lambda (s) (and (symbol? (car s)) (<= 2 (length s) 3))) (cadr x))
                   (let* ((specs (cadr x)) (env2 (append (map car specs) env)))
                     (append (apply append (map (lambda (s) (walk (cadr s) env)) specs))
                             (apply append (map (lambda (s) (walk-all (cddr s) env2)) specs))
                             (walk-all (caddr x) env2)
                             (body (cdddr x) env2))))))
      (cons 'begin (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'if (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'and (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'or (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'when (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'unless (lambda (x env walk body) (walk-all (cdr x) env)))
      (cons 'set!
            (lambda (x env walk body)
              (and (= (length x) 3) (symbol? (cadr x)) (walk-all (cdr x) env))))
      (cons 'cond
            (lambda (x env walk body)
              (and (lists? (cdr x))
                   (apply append (map (lambda (c) (cond-clause c env)) (cdr x))))))
      (cons 'case
            (lambda (x env walk body)
              (and (>= (length x) 2) (lists? (cddr x))
                   (append (walk (cadr x) env)
                           (apply append
                                  (map (lambda (c)
                                         (let ((rest (cdr c)))
                                           (walk-all (if (and (pair? rest) (eq? (car rest) '=>) (not (bound? '=> env)))
                                                         (cdr rest)
                                                         rest)
                                                     env)))
                                       (cddr x)))))))
      (cons 'guard
            (lambda (x env walk body)
              (and (>= (length x) 2) (pair? (cadr x)) (symbol? (car (cadr x))) (lists? (cdr (cadr x)))
                   (let ((env2 (cons (car (cadr x)) env)))
                     (append (apply append (map (lambda (c) (cond-clause c env2)) (cdr (cadr x))))
                             (body (cddr x) env))))))
      (cons 'parameterize
            (lambda (x env walk body)
              (and (>= (length x) 2) (lists? (cadr x)) (for-all (lambda (b) (= (length b) 2)) (cadr x))
                   (append (apply append (map (lambda (b) (walk-all b env)) (cadr x)))
                           (body (cddr x) env)))))
      (cons 'rec
            (lambda (x env walk body)
              (and (= (length x) 3) (symbol? (cadr x)) (walk (caddr x) (cons (cadr x) env)))))
      ;; The last two operands, the parameter types and the result type, are
      ;; data; the one before them, the entry, is walked; any operand before
      ;; the entry is a calling convention, data too.
      (cons 'foreign-procedure
            (lambda (x env walk body)
              (and (>= (length x) 4)
                   (walk (list-ref x (- (length x) 3)) env)))))))

  ;; ---- text code: the identifier tokens -------------------------------------------
  ;;
  ;; NEVER: NOTHING IS STRIPPED. Every token of the text that matches the
  ;; language's identifier pattern is a use, in a comment or a string as much
  ;; as in code: a lexer that knows one language's comments and strings is
  ;; that language's own item. The answer says (lexing whole-text).
  ;; A token is the longest match of the pattern starting where the scan is;
  ;; a character that starts no match is passed over, so in `9abc` the token
  ;; is `abc`. The pattern is matched against a window of the text, short
  ;; first and the long one only when a match fills the short one; a token
  ;; longer than the long window is cut there and read on as a second one.

  (define default-identifier-pattern "[A-Za-z_][A-Za-z0-9_]*")
  (define short-window 32)
  (define long-window 1024)

  ;; -> the distinct tokens, as symbols, sorted.
  (define (text-uses text pattern)
    (let* ((ast (regex-compile (string-append "(" pattern ")")))
           (n (string-length text)))
      (define (token-end i size)
        (let* ((window (substring text i (min n (+ i size))))
               (caps (regex-match ast window #t))
               (span (and caps (assv 1 caps))))
          (and span (cddr span))))
      (let loop ((i 0) (out '()))
        (if (>= i n)
            (symbol-set out)
            (let* ((short (token-end i short-window))
                   (end (if (and short (= short short-window)) (token-end i long-window) short)))
              (if (and end (> end 0))
                  (loop (+ i end) (cons (string->symbol (substring text i (+ i end))) out))
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

  (define missing (list 'missing))
  (define (field-of row name)
    (let ((e (assq name (cdr (assq 'fields row))))) (if e (cdr e) missing)))
  (define (conflict-form? v) (and (pair? v) (eq? (car v) 'conflict)))

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
                  (if (or (eq? body missing) (conflict-form? body))
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
        (answer '() '() (if (eq? imports missing) '() (list imports)) '(syntactic))
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
                         (list (cons 'name-use (cdr (assq 'contract u)))))))))))

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
              (if (null? counts) '() (list (cons 'skipped counts)))))))
