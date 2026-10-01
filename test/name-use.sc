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
;; `names` and `uses`: which names a code block uses, as its stored code
;; shows them, and which blocks use a name.
;;
;; THE WALK IS ASKED DIRECTLY FIRST. Every walk row states the exact sorted
;; set a form gives, worked out by hand from the rule, so a walk that is
;; generous or stingy by one name is red. The second half asks a real store
;; through each route a caller has.
;;
;; THE VALUES A ROW READS ARE TAKEN IN THE ORDER THEY ARE WRITTEN: R6RS
;; leaves the order of a call's arguments open.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia name-use) datum-uses text-uses import-library-name block-name-use
              name-use-table register-name-use-form! default-identifier-pattern)
        (only (theourgia reduce) state-read state-block-ids reduce-applied-cut)
        (only (theourgia store) open-and-reduce with-store-write)
        (only (theourgia languages) language-table language-property register-language!)
        (only (theourgia wire) string->sexpr-extended encode-record storable-encode))

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
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (want-1 name
             (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                             (condition-message e) e))))
               got)
             expected))))

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))

(register-verbs! extension-verbs)

;; ---- the datum walk ----------------------------------------------------------------

(want "U1 a definition's body: the formals are bound, the rest are uses"
      (datum-uses '(define (f x) (g x y)))
      '(g y))
(want "U2 a recursive top definition uses itself; a let-bound f shadows it"
      (in-order (datum-uses '(define (f n) (f (- n 1))))
                (datum-uses '(define (f) (let ((f g)) (f)))))
      '((- f) (g)))
(want "U3 let inits see the outer scope; let* is sequential; letrec sees itself; a named let's name is not in its inits"
      (in-order (datum-uses '(let ((x x)) x))
                (datum-uses '(let* ((a 1) (b a)) b))
                (datum-uses '(letrec ((f (lambda () f))) (f)))
                (datum-uses '(let loop ((x (loop seed))) (loop x))))
      '((x) () () (loop seed)))
(want "U3b letrec* is not let*: every name is bound in every init"
      (datum-uses '(letrec* ((x y) (y 1)) x))
      '())
(want "U4 do: inits outside, steps and test inside; let-values parallel, let*-values sequential"
      (in-order (datum-uses '(do ((i i (+ i 1))) ((> i limit) i)))
                (datum-uses '(let-values (((x) p) ((y) x)) y))
                (datum-uses '(let*-values (((x) p) ((y) x)) y)))
      '((+ > i limit) (p x) (p)))
(want "U4b do's commands are inside the variables' scope"
      (datum-uses '(do ((i 0 (+ i 1))) ((done? i) i) (emit i)))
      '(+ done? emit))
(want "U5 internal definitions bind in the whole body: one, two mutually recursive, two spliced from a begin"
      (in-order (datum-uses '(lambda () (define (f x) (g x)) (f y)))
                (datum-uses '(lambda () (define (ev? n) (od? n)) (define (od? n) (ev? n)) (ev? 1)))
                (datum-uses '(lambda () (begin (define (f) 1) (define (g) (f))) (g))))
      '((g y) () ()))
(want "U5b a bound define is not a definition: the collection obeys shadowing"
      (datum-uses '(lambda (define) (define x y) x))
      '(x y))
(want "U6 quote, quasiquote with an unquote, nested depth, a vector template, unquote-splicing, a quoted unquote"
      (in-order (datum-uses '(quote (a b)))
                (datum-uses '(quasiquote (p (unquote (r 1)))))
                (datum-uses '(quasiquote (a (quasiquote (b (unquote (c (unquote d))))))))
                (datum-uses '(quasiquote #(1 (unquote v))))
                (datum-uses '(quasiquote (1 (unquote-splicing xs))))
                (datum-uses '(quasiquote (quote (unquote x)))))
      '(() (r) (d) (v) (xs) (x)))
(want "U6b an ordinary vector literal uses nothing"
      (datum-uses '(g #(x)))
      '(g))
(want "U7 a locally bound quote is a variable"
      (datum-uses '(let ((quote list)) (quote x)))
      '(list x))
(want "U8 lambda formals: a list, a rest argument, one symbol; case-lambda clause by clause"
      (in-order (datum-uses '(lambda (a b) (f a b c)))
                (datum-uses '(lambda (a . r) (g a r s)))
                (datum-uses '(lambda args (h args)))
                (datum-uses '(case-lambda ((x) (f x)) ((x . y) (g x y z)))))
      '((c f) (g s) (h) (f g z)))
(want "U9 the fallback: an unknown macro's every name; a bound name stays bound and a quote is still a quote"
      (in-order (datum-uses '(my-macro (a b) c))
                (datum-uses '(lambda (x) (my-macro x 'q))))
      '((a b c my-macro) (my-macro)))
(want "U9b an improper application"
      (datum-uses '(my-macro a . b))
      '(a b my-macro))
(want "U9c a defining form outside the two known positions is walked by the fallback"
      (datum-uses '(my-macro (define x y)))
      '(define my-macro x y))
(want "U10 define-record-type: defined names and field words are not uses; protocol's free names and the parent are"
      (datum-uses '(define-record-type point (fields x (mutable y))
                     (protocol (lambda (new) (lambda (a) (new a (scale a)))))
                     (parent base)))
      '(base scale))
(want "U10b parent-rtd's two expressions are uses"
      (datum-uses '(define-record-type child (parent-rtd rtd rcd)))
      '(rcd rtd))
(want "U11 the stated approximation: a define's expression, and a transformer's pattern variables"
      (in-order (datum-uses '(define x e))
                (datum-uses '(define-syntax m (syntax-rules () ((_ x) x)))))
      '((e) (_ syntax-rules x)))
(want "U24 a body that would exit is walked, not run"
      (datum-uses '(begin (exit 7)))
      '(exit))

(want "K1 if, and, or, when, unless walk their operands"
      (in-order (datum-uses '(if a b c)) (datum-uses '(and a (or b c))) (datum-uses '(when a (unless b c))))
      '((a b c) (a b c) (a b c)))
(want "K2 set!: the target by the symbol rule"
      (in-order (datum-uses '(set! x (f y))) (datum-uses '(lambda (x) (set! x (f y)))))
      '((f x y) (f y)))
(want "K3 cond: else and => are not uses; a bound cond is a variable and its operands fall back"
      (in-order (datum-uses '(cond ((p a) => f) (else b)))
                (datum-uses '(let ((cond list)) (cond (else 1)))))
      '((a b f p) (else list)))
(want "K4 case: the key and the bodies, not the datums or else"
      (datum-uses '(case k ((p q) (f r)) (else g)))
      '(f g k r))
(want "K5 guard: the variable is bound in the clauses, not in the body"
      (in-order (datum-uses '(guard (e ((p? e) 1)) 2))
                (datum-uses '(guard (e (#t (h e))) (body e))))
      '((p?) (body e h)))
(want "K6 parameterize: parameters, values and the body"
      (datum-uses '(parameterize ((p v)) (f)))
      '(f p v))
(want "K7 rec binds its name; define-values defines at the top and at the head of a body"
      (in-order (datum-uses '(rec f (lambda () (f))))
                (datum-uses '(define-values (a b) (g)))
                (datum-uses '(lambda () (define-values (a b) (g)) (h a b))))
      '(() (g) (g h)))
(want "K8 foreign-procedure: the entry is walked; types, result and a convention before the entry are data"
      (in-order (datum-uses '(foreign-procedure "open" (string int) int))
                (datum-uses '(foreign-procedure __collect_safe "sleep" (unsigned) unsigned))
                (datum-uses '(foreign-procedure entry-name (int) int)))
      '(() () (entry-name)))
(want "K9 a malformed known form falls back and does not raise"
      (in-order (datum-uses '(cond x)) (datum-uses '(guard)) (datum-uses '(case))
                (datum-uses '(set!)) (datum-uses '(rec)))
      '((cond x) (guard) (case) (set!) (rec)))

;; K10: two definitions of this repository, read from their files as data.
(define (internal-define path name)
  (call-with-input-file path
    (lambda (p)
      (let loop ()
        (let ((x (read p)))
          (cond
            ((eof-object? x) #f)
            ((and (pair? x) (eq? (car x) 'library))
             (or (find (lambda (f) (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
                                        (pair? (cadr f)) (eq? (car (cadr f)) name)))
                       x)
                 (loop)))
            (else (loop))))))))
(want "K10 this repository's own code: latest-parent-cut and datum-names, counted by hand"
      (in-order (datum-uses (internal-define "../working.sc" 'latest-parent-cut))
                (datum-uses (internal-define "../datum-code.sc" 'datum-names)))
      '((car cdr cut-covers? fold-left pair?)
        (= > >= append apply cadddr caddr cadr car cddr cdr eq? filter find
         length list list? map memq not pair? string->symbol string-append
         symbol->string symbol?)))

(want "K11 CONTROL: an unregistered binder falls back"
      (datum-uses '(my-binder q (f q)))
      '(f my-binder q))
(register-name-use-form! 'my-binder
  (lambda (x env walk body) (and (= (length x) 3) (walk (caddr x) (cons (cadr x) env)))))
(want "K11 a form registered from outside is honoured without a change to the walk"
      (datum-uses '(my-binder q (f q)))
      '(f))

;; ---- text: every identifier token -------------------------------------------------

(define (pattern-of lang) (language-property (find (lambda (e) (equal? (language-property e 'lang #f) lang))
                                                   (language-table))
                                             'identifier #f))
(want "U12' a word only in a comment or only in a string is listed"
      (text-uses "x = 1 # only_comment\ny = \"only_string\"\n" (pattern-of "python"))
      '(only_comment only_string x y))
(want "U13 the language's pattern: a Scheme name with - and >; the default never starts a token with a digit"
      (in-order (text-uses "(define (a-b x) (c->d x))" (pattern-of "scheme"))
                (text-uses "1abc x1" default-identifier-pattern))
      '((a-b c->d define x) (abc x1)))
(want "U13b JavaScript takes $; a regex literal's digits are no token"
      (text-uses "$foo; foo; /0/;" (pattern-of "javascript"))
      '($foo foo))
(want "U14b a word after a line comment is listed"
      (text-uses "alpha; // beta" (pattern-of "javascript"))
      '(alpha beta))
(want "U13b every language in the table states its identifier pattern, and each reads its own names"
      (map (lambda (e)
             (let ((lang (language-property e 'lang #f)) (p (language-property e 'identifier #f)))
               (list lang (and (string? p) (text-uses "a_1 $x" p)))))
           (language-table))
      (map (lambda (e)
             (let ((lang (language-property e 'lang #f)))
               (list lang (if (member lang '("javascript" "typescript" "scheme" "chez")) '($x a_1) '(a_1 x)))))
           (language-table)))
(want "U13 a token longer than the short window is read whole"
      (text-uses (string-append "a" (make-string 40 #\b) " c") default-identifier-pattern)
      (list (string->symbol (string-append "a" (make-string 40 #\b))) 'c))

;; ---- imports -------------------------------------------------------------------------

(want "U16 each wrapper reduces to the library's name; a version is dropped; a plain spec is itself"
      (map import-library-name
           '((only (foo bar) x) (except (foo) y) (prefix (foo) p:) (rename (foo) (a b))
             (for (foo) run expand) (foo bar (1 2)) (library (only))
             (for (prefix (only (foo (1)) x) p:) run (meta 1)) (rnrs)))
      '((foo bar) (foo) (foo) (foo) (foo) (foo bar) (only) (foo) (rnrs)))
(want "U16 a spec that does not reduce answers #f"
      (map import-library-name '(42 (only) (1 2) ()))
      '(#f #f #f #f))

;; ---- the real store -------------------------------------------------------------------

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/name-use-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/nu" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))

(define (run . args) (rpc-dispatch store args "test"))
(run 'init)
;; -> the id the one insert made: the store's ids after it, less those before.
(define (insert! parent fields)
  (let ((before (state-block-ids (open-and-reduce store))))
    (with-store-write store (lambda (s v) (list (list 'insert parent #f fields))) "test")
    (let ((new (filter (lambda (id) (not (member id before))) (state-block-ids (open-and-reduce store)))))
      (if (= 1 (length new)) (car new) (list 'NOT-ONE-NEW new)))))
(define (datum parent body)
  (insert! parent (list '(kind . code) '(mode . datum) '(lang . chez) (cons 'body body))))
(define (text parent lang src)
  (insert! parent (list '(kind . code) '(mode . text) (cons 'lang lang) (cons 'src (string->utf8 src)))))
(define (library parent name imports)
  (insert! parent (list '(kind . library) '(mode . datum) '(lang . chez) (cons 'name name) (cons 'imports imports))))

(define L1 (library 'root '(a) '((rnrs) (only (foo bar) x) 42)))
(define P1 (insert! 'root (list '(kind . library) '(mode . datum) '(lang . chez) '(name . (prog))
                                '(shape . program) '(imports . ((rnrs base) (prefix (zed) z:))))))
(define D1 (datum L1 '(define (h) (car x))))
(define D2 (datum L1 '(define (car y) y)))
(define D3 (datum L1 '(define (k) (let ((car 1)) car))))
(define T1 (text L1 'javascript "car(x); car(y); // car again\n"))
(define D4 (datum L1 '(define (m) (cart 1))))
(define D5 (datum L1 '(define (n) (CAR 1))))
(define D9 (datum L1 '(define (twice) 1)))
(define D11 (datum L1 '(define (u1) (twice undefined-thing))))
(define TN (text L1 'cobol "MOVE car TO x."))
(define L2 (library 'root '(b) '((rnrs))))
(define D6 (datum L2 '(define (p) (car 1))))
(define D7 (datum 'root '(define (q) (car 1))))
(define L3 (library 'root '(c) '((rnrs))))
(define D10 (datum L3 '(define (twice) 2)))
(define D12 (datum L3 '(define (u3) (twice))))
(define DU (datum L3 '(lambda () (define-record-type r (fields . 5)) (car 1))))
(define T2 (text 'root 'javascript "function target() {}\n"))
(define S1 (insert! 'root '((kind . section) (title . "A section"))))
(run 'del L2)
(run 'del D7)

(want "U18 CONTROL: the store was built (every insert made exactly one block)"
      (for-all string? (list L1 P1 D1 D2 D3 T1 D4 D5 D9 D11 TN L2 D6 D7 L3 D10 D12 DU T2 S1))
      #t)

(want "U18 names: a datum block, a text block, a library block, exact answers"
      (in-order (run 'names D1) (run 'names T1) (run 'names L1))
      (list '(ok (items (name car) (name x)) (name-use syntactic))
            '(ok (items (name again) (name car) (name x) (name y)) (name-use syntactic (lexing whole-text)))
            '(ok (items (import (rnrs)) (import (foo bar)) (import-unreadable 42)) (name-use syntactic))))
(want "U17 a program block answers its imports as a library block does"
      (run 'names P1)
      '(ok (items (import (rnrs base)) (import (zed))) (name-use syntactic)))
(want "U18 names: each none reason, a whole datum"
      (in-order (run 'names D7) (run 'names S1) (run 'names DU) (run 'names TN))
      '((ok (items) (name-use none (reason deleted)))
        (ok (items) (name-use none (reason not-code)))
        (ok (items) (name-use none (reason unreadable-code)))
        (ok (items) (name-use none (reason no-language)))))
(want "U18 names: an unknown id refuses as read does; no id or two is a usage answer"
      (in-order (let ((a (run 'names "zz.zz"))) (list (car a) (cadr a)))
                (let ((a (run 'read "zz.zz"))) (list (car a) (cadr a)))
                (car (run 'names)) (car (run 'names D1 D2)))
      '((error unknown-id) (error unknown-id) usage usage))

;; Unscoped, the store holds one block with no language (TN) and one whose
;; body cannot be read (DU).
(define all-skipped '(skipped (no-language 1) (unreadable-code 1)))
(define (row id lib mode) (list 'use id (list 'library lib) (list 'mode mode)))
(define (sorted-rows . rs) (list-sort (lambda (a b) (string<? (cadr a) (cadr b))) rs))
(want "U19 uses: datum and text listed with library and mode; a definition, a local binding, cart and CAR are not; a deleted library is none; a deleted block is absent; by id"
      (run 'uses "car")
      (list 'ok (cons 'items (sorted-rows (row D1 L1 'datum) (row T1 L1 'text) (row D6 'none 'datum)))
            '(name-use syntactic) all-skipped))
(want "U19 uses: case matters, and a name nothing uses answers ok with no items"
      (in-order (run 'uses "CAR") (run 'uses "nothing-uses-this"))
      (list (list 'ok (list 'items (row D5 L1 'datum)) '(name-use syntactic) all-skipped)
            (list 'ok '(items) '(name-use syntactic) all-skipped)))
(want "U19b a text block that defines a name lists it"
      (run 'uses "target")
      (list 'ok (list 'items (row T2 'none 'text)) '(name-use syntactic) all-skipped))
(want "U20 no resolution: a name nothing defines is listed; a name two libraries define lists both users"
      (in-order (run 'uses "undefined-thing") (run 'uses "twice"))
      (list (list 'ok (list 'items (row D11 L1 'datum)) '(name-use syntactic) all-skipped)
            (list 'ok (cons 'items (sorted-rows (row D11 L1 'datum) (row D12 L3 'datum)))
                  '(name-use syntactic) all-skipped)))
(want "U21 --under: two scopes and none give their own items and skipped counts"
      (in-order (run 'uses "twice" "--under" L1) (run 'uses "twice" "--under" L3) (run 'uses "twice" "--under" T2))
      (list (list 'ok (list 'items (row D11 L1 'datum)) '(name-use syntactic) '(skipped (no-language 1)))
            (list 'ok (list 'items (row D12 L3 'datum)) '(name-use syntactic) '(skipped (unreadable-code 1)))
            '(ok (items) (name-use syntactic))))
(want "U21 --under an unknown id refuses; an empty name or --under is a usage answer"
      (in-order (let ((a (run 'uses "twice" "--under" "zz.zz"))) (list (car a) (cadr a)))
                (car (run 'uses "")) (car (run 'uses "twice" "--under" "")) (car (run 'uses)))
      '((error unknown-id) usage usage usage))
(want "U21b a block the walk cannot read is counted, and the healthy block beside it is listed"
      (run 'uses "twice" "--under" L3)
      (list 'ok (list 'items (row D12 L3 'datum)) '(name-use syntactic) '(skipped (unreadable-code 1))))
(want "U21b CONTROL: the unreadable block's body does use car, so a walk that read it would list it"
      (datum-uses '(car 1))
      '(car))

;; U15: the no-language block is counted only in the scope that holds it.
(want "U15 no language entry: counted under skipped within --under only"
      (in-order (cddr (run 'uses "car" "--under" L1)) (cddr (run 'uses "car" "--under" L3)))
      '(((name-use syntactic) (skipped (no-language 1))) ((name-use syntactic) (skipped (unreadable-code 1)))))

(want "V4 the table is the provider's answers: every live code block that reads is in it once"
      (let ((t (name-use-table (open-and-reduce store))))
        (in-order (list-sort string<? (hashtable-ref (vector-ref t 0) "car" '()))
                  (hashtable-ref (vector-ref t 2) TN #f)
                  (hashtable-ref (vector-ref t 2) DU #f)))
      (list (list-sort string<? (list D1 T1 D6)) 'no-language 'unreadable-code))

;; ---- U24b: the stored value, never a printed or re-read one --------------------------

(define DB (datum 'root (list 'define '(f) (list (string->symbol "a b") "(x)"))))
(want "U24b a symbol that needs bars and a string holding a parenthesis: the stored datum's names"
      (run 'names DB)
      (list 'ok (list 'items (list 'name (string->symbol "a b"))) '(name-use syntactic)))
;; THE SPY IS THE LIBRARY'S OWN TEXT: no procedure that prints, reads or
;; evaluates is named anywhere in it, read as data.
(define (symbols-in x)
  (cond ((symbol? x) (list x))
        ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x))))
        ((vector? x) (symbols-in (vector->list x)))
        (else '())))
(define library-symbols
  (call-with-input-file "../name-use.sc"
    (lambda (p) (let loop ((out '())) (let ((x (read p))) (if (eof-object? x) out (loop (append (symbols-in x) out))))))))
(want "U24b the library names no printer, reader or evaluator"
      (filter (lambda (s) (memq s library-symbols))
              '(read get-datum eval interpret load compile write display put-datum format pretty-print
                with-output-to-string open-string-input-port open-input-string string->sexpr-extended))
      '())
(want "U24b CONTROL: the spy reads the library (it finds the walk and the verbs)"
      (list (and (memq 'walk-body library-symbols) #t) (and (memq 'uses-verb library-symbols) #t))
      '(#t #t))

;; ---- U23: the provider answers from the state it is given ----------------------------

(define before-cut (reduce-applied-cut (open-and-reduce store)))
(define published (open-and-reduce store))
(define DZ (datum 'root '(define (z) (zebra 1))))
(with-store-write store (lambda (s v) (list (list 'set D1 'body '(define (h) (cdr x))))) "test")
(want "U23 a state given to the dispatch is the one answered: a later write is not seen in it, and is seen in a fresh one"
      (in-order (rpc-dispatch store '(uses "zebra") "test" published)
                (run 'uses "zebra"))
      (list (list 'ok '(items) '(name-use syntactic) all-skipped)
            (list 'ok (list 'items (row DZ 'none 'datum)) '(name-use syntactic) all-skipped)))
(want "U23 at a cut before the edit, the old code's names are answered; now, the new code's"
      (in-order (cdr (assq 'names (block-name-use (open-and-reduce store before-cut) D1)))
                (cdr (assq 'names (block-name-use (open-and-reduce store) D1))))
      '((car x) (cdr x)))
(want "U23 uses at that cut lists the block the edit took car out of"
      (and (member (row D1 L1 'datum)
                   (cdadr (rpc-dispatch store '(uses "car") "test" (open-and-reduce store before-cut))))
           #t)
      #t)

;; ---- the pins, after the last write and before the routes ----------------------------

(define (log-bytes)
  (let ((f (string-append root "/size.txt")))
    (system (string-append "find '" store "' -name '*.sexp' -path '*writers*' -exec cat {} + | wc -c | tr -d ' ' > '" f "'"))
    (file-text f)))
(define (file-list)
  (let ((f (string-append root "/files.txt")))
    (system (string-append "cd '" store "' && find . | LC_ALL=C sort > '" f "'"))
    (file-text f)))
(define pins-before (list (log-bytes) (file-list)))

;; ---- U25: three routes, one answer ---------------------------------------------------

(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run " extra " "))
(define (sh-out name command)
  (let ((out (string-append root "/" name ".out")))
;; A command that names its own input keeps it: a second `<` would win.
    (system (string-append command " > '" out "' 2> '" root "/" name ".err'"
                           (if (string-contains? command " < ") "" " < /dev/null")))
    (file-text out)))
(define (first-datum text)
  (guard (e (#t (list 'UNREADABLE text)))
    (string->sexpr-extended (let loop ((i 0))
                              (cond ((>= i (string-length text)) text)
                                    ((char=? (string-ref text i) #\newline) (substring text 0 i))
                                    (else (loop (+ i 1))))))))
(define (stop-daemons!) (system (string-append "pkill -f 'serve " store "' 2>/dev/null; sleep 1")))

;; The text of the answer with this id's first content item, unescaped.
(define (mcp-text out id)
  (let* ((key-id (string-append "\"id\":" (number->string id)))
         (find-from (lambda (key from)
                      (let find ((i from))
                        (cond ((> (+ i (string-length key)) (string-length out)) #f)
                              ((string=? (substring out i (+ i (string-length key))) key) (+ i (string-length key)))
                              (else (find (+ i 1)))))))
         (at (find-from key-id 0))
         (k (and at (find-from "\"text\":\"" at))))
    (and k (let loop ((i k) (acc '()))
             (cond ((>= i (string-length out)) #f)
                   ((char=? (string-ref out i) #\\)
                    (let ((c (string-ref out (+ i 1))))
                      (loop (+ i 2) (cons (cond ((char=? c #\n) #\newline) ((char=? c #\t) #\tab) (else c)) acc))))
                   ((char=? (string-ref out i) #\") (list->string (reverse acc)))
                   (else (loop (+ i 1) (cons (string-ref out i) acc))))))))
(define (json-string s) (string-append "\"" s "\""))
(define (mcp-session name env-text calls)
  (let ((in (string-append root "/" name "-in.jsonl")))
    (call-with-output-file in
      (lambda (p)
        (put-string p (string-append
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                        "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                        "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"))
        (let loop ((cs calls) (id 3))
          (unless (null? cs)
            (put-string p (string-append
                            "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id) ",\"method\":\"tools/call\",\"params\":"
                            "{\"name\":\"" (car (car cs)) "\",\"arguments\":{\"argv\":["
                            (let join ((xs (cdr (car cs))))
                              (cond ((null? xs) "") ((null? (cdr xs)) (json-string (car xs)))
                                    (else (string-append (json-string (car xs)) "," (join (cdr xs))))))
                            "]}}}\n"))
            (loop (cdr cs) (+ id 1)))))
      'replace)
    (sh-out name (string-append env-text "scheme --script ../mcp/server.sc --store '" store "' < '" in "'"))))

(define route-questions (list (list "names" D1) (list "uses" "car")))
(define (cli-args q) (string-append (car q) " '" (cadr q) "'"))
(define local-wires
  (map (lambda (q) (sh-out "local" (string-append (env "THEOURGIA_LOCAL=1") "scheme --script ../core.sc " (cli-args q)
                                                  " --store '" store "' --wire")))
       route-questions))
(define client-wires
  (map (lambda (q) (sh-out "client" (string-append (env "") "scheme --script ../theourgia.sc " (cli-args q)
                                                   " --store '" store "' --wire")))
       route-questions))
(define mcp-out
  (mcp-session "mcp" (env "") (map (lambda (q) (list (string-append "theourgia_" (car q)) (cadr q))) route-questions)))
(stop-daemons!)
(want "U25 the local route answers what this process answers"
      (map (lambda (q w) (equal? (first-datum w) (apply run (string->symbol (car q)) (cdr q))))
           route-questions local-wires)
      '(#t #t))
(want "U25 CONTROL: the local answers are the populated ones"
      (map (lambda (w) (let ((a (first-datum w))) (and (pair? a) (car a) (pair? (cdr a)) (pair? (cdadr a)))))
           local-wires)
      '(#t #t))
(want "U25 the thin client's answer, through the daemon, equals the local one byte for byte"
      (map (lambda (c l) (if (string=? c l) 'identical (list 'client c 'local l))) client-wires local-wires)
      '(identical identical))
(want "U25 the MCP shell's texts decode to the same datums"
      (list (equal? (first-datum (or (mcp-text mcp-out 3) "")) (first-datum (car local-wires)))
            (equal? (first-datum (or (mcp-text mcp-out 4) "")) (first-datum (cadr local-wires))))
      '(#t #t))

;; ---- U26: the library is entered on dispatch only ------------------------------------
;;
;; A copy of the library that writes a file when its body runs, first on the
;; library path, as test/verb-registry.sc does for commitments.
(define marklib (string-append root "/marklib"))
(define marker (string-append root "/marker"))
(system (string-append "mkdir -p '" marklib "/theourgia'"))
(let* ((text (file-text "../name-use.sc"))
       (anchor "  (define (names-verb ")
       (at (let find ((i 0)) (cond ((> (+ i (string-length anchor)) (string-length text)) #f)
                                   ((string=? (substring text i (+ i (string-length anchor))) anchor) i)
                                   (else (find (+ i 1)))))))
  (call-with-output-file (string-append marklib "/theourgia/name-use.sc")
    (lambda (p)
      (put-string p (string-append (substring text 0 at)
                                   "  (define marker-written (let ((p (open-file-output-port \"" marker
                                   "\" (file-options no-fail)))) (close-port p) #t))\n"
                                   (substring text at (string-length text)))))
    'replace))
(define (env-marked extra)
  (string-append "CHEZSCHEMELIBDIRS=" marklib ":" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run " extra " "))
;; -> (answer-head marker-written?)
(define (marked? program verb extra)
  (system (string-append "rm -f '" marker "'"))
  (let ((t (sh-out "marked" (string-append (env-marked extra) "scheme --script ../" program " " verb " --store '" store "' --wire"))))
    (list (guard (e (#t 'UNREADABLE)) (car (read (open-string-input-port t))))
          (file-exists? marker))))
(define (mcp-marked? tool argv)
  (system (string-append "rm -f '" marker "'"))
  (let ((out (mcp-session "mcp-marked" (env-marked "") (list (cons tool argv)))))
    (list (car (first-datum (or (mcp-text out 3) "")))
          (file-exists? marker))))
(want "U26 core.sc, the thin client and the MCP shell answering outline do not run the library; answering names does"
      (in-order (marked? "core.sc" "outline" "THEOURGIA_LOCAL=1")
                (marked? "core.sc" (string-append "names " D1) "THEOURGIA_LOCAL=1")
                (begin (stop-daemons!) (marked? "theourgia.sc" "outline" ""))
                (marked? "theourgia.sc" (string-append "uses car") "")
                (begin (stop-daemons!) (mcp-marked? "theourgia_outline" '()))
                (mcp-marked? "theourgia_names" (list D1)))
      '((ok #f) (ok #t) (ok #f) (ok #t) (ok #f) (ok #t)))
(stop-daemons!)

(define pins-after (list (log-bytes) (file-list)))
(want "U24 every route above wrote nothing: the log's length and the store's files are what they were"
      (map equal? pins-before pins-after)
      '(#t #t))

;; ---- K12: no cache ----------------------------------------------------------------------

(define (client-uses name)
  (first-datum (sh-out "k12" (string-append (env "") "scheme --script ../theourgia.sc uses '" name "' --store '" store "' --wire"))))
(define yak-before (client-uses "yak"))
;; A commit by another writer, made outside the daemon: one record for a
;; mirror writer, published.
(define mirror-file (string-append root "/mirror.bin"))
(call-with-port (open-file-output-port mirror-file (file-options no-fail))
  (lambda (p)
    (put-bytevector p (encode-record 1 1789000000001 "peer" '()
                                     (storable-encode '(put ((kind . code) (mode . datum) (lang . chez)
                                                             (body . (define (y) (yak 1))))))))))
(define published-mirror (run 'publish "mirrorzy" "1" mirror-file))
(define yak-after (client-uses "yak"))
(stop-daemons!)
(want "K12 a commit by another writer is in the daemon's next answer"
      (in-order (car published-mirror) (cadr yak-before) (map cadr (cdadr yak-after)))
      '(ok (items) ("mirrorzy.1")))

(define move-before (run 'uses "MOVE"))
(register-language! '((lang "cobol") (extensions ("cob")) (def-heads ()) (identifier "[A-Z]+")))
(define move-after (run 'uses "MOVE"))
(want "K12 a language registered after a request is read by the next one"
      (in-order (cadr move-before) (cadddr move-before) (cadr move-after) (length (cddr move-after)))
      (list '(items) all-skipped (list 'items (row TN L1 'text)) 2))

;; ---- U27: the README -------------------------------------------------------------------

(define readme (file-text "../README.md"))
(want "U27 README states both approximations, whole-text lexing, not find-references, and a text definition listed"
      (map (lambda (s) (string-contains? readme s))
           '("### `names`" "### `uses`" "over-approximation" "under-approximation" "(lexing whole-text)"
             "It is not find-references" "`function target() {}` is listed by `uses target`"))
      '(#t #t #t #t #t #t #t))

(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\nname-use complete\n" bad rows)
(exit (if (= bad 0) 0 1))
