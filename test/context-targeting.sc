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

;; CONTEXT THAT ANSWERS THE CODER'S QUESTION: a text file's library and
;; imports read from its header, no-library definitions scoped by file,
;; text read as forms, ambiguity folded, and the names an edit will use.
;;
;; THE FILES ARE REAL SPLIT FILES: each goes through split-suggest's review
;; copy and import-code, so a library file's first piece is the unclosed
;; header and its last ends with the library's closer. C1's library holds
;; a vector literal, which this reader refuses: a header read only from a
;; whole library form would find no name there. Where a row needs a
;; cut the splitter would not make (a comment-only first piece, a cut inside
;; the import form or inside a definition), the review copy is written with
;; its marker lines by hand, as a person editing it would.
;;
;; EVERY ROW IS RED BEFORE THE CHANGE unless it says PIN (a preservation the
;; change must keep) or READING (a measurement printed, not judged). The
;; reader library does not exist before the change, so it is reached by
;; eval, and a tree without it answers each of its rows red.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce with-store-write search-positions-judged-count)
        (only (theourgia reduce) state-read state-block-ids state-outline block-id)
        (only (theourgia render) render-wire render-human)
        (only (theourgia wire) sexpr->string-extended)
        (only (theourgia query) rule-value-check make-session-provider)
        (only (theourgia languages) register-language! language-for-name))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
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

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/context-targeting-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- helpers ------------------------------------------------------------------------------

(define (write-file! path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (key<? a b) (string<? (format "~s" a) (format "~s" b)))
(define (sorted xs) (list-sort key<? xs))
(define (ok? a) (and (pair? a) (eq? (car a) 'ok)))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (new-id a)
  (let* ((ev (and (ok? a) (assq 'events (cdr a)))) (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))

(define store-count 0)
(define writers '())
(define (fresh!)
  (set! store-count (+ store-count 1))
  (let* ((p (string-append root "/s" (number->string store-count)))
         (a (rpc-dispatch p '(init) "test")))
    (set! writers (cons (cons p (cadr (assq 'writer (cdr a)))) writers))
    p))
(define (writer-of s) (cdr (assoc s writers)))
(define (run s . args) (rpc-dispatch s args "test"))

;; FILES, ((<name> . <text>) ...), split by split-suggest and imported
(define dir-count 0)
(define (new-dir!)
  (set! dir-count (+ dir-count 1))
  (let ((d (string-append root "/d" (number->string dir-count))))
    (system (string-append "mkdir -p '" d "/src' '" d "/split'"))
    d))
(define (project! s files)
  (let ((d (new-dir!)))
    (for-each (lambda (f)
                (write-file! (string-append d "/src/" (car f)) (cdr f))
                (run s 'split-suggest (string-append d "/src/" (car f)) "--output" (string-append d "/split/" (car f))))
              files)
    (run s 'import-code (string-append d "/split"))))
;; FILES written as they are, marker lines included, and imported
(define (import-as-is! s files)
  (let ((d (new-dir!)))
    (for-each (lambda (f) (write-file! (string-append d "/split/" (car f)) (cdr f))) files)
    (run s 'import-code (string-append d "/split"))))
(define marker ";; @block new pad 0\n")

(define (text-of v) (cond ((bytevector? v) (utf8->string v)) ((string? v) v) (else #f)))
(define (fields-of S id) (let ((r (state-read S id))) (if r (cdr (assq 'fields r)) '())))
(define (fld S id name) (let ((e (assq name (fields-of S id)))) (and e (cdr e))))
;; the file holder whose path is PATH, and its code children in order
(define (holder-of s path)
  (let ((S (open-and-reduce s)))
    (find (lambda (id) (and (eq? (fld S id 'kind) 'file) (equal? (fld S id 'path) path))) (state-block-ids S))))
(define (pieces s path)
  (let* ((S (open-and-reduce s)) (h (holder-of s path)))
    (map caddr (filter (lambda (r) (equal? (car r) h)) (state-outline S)))))
;; the piece of PATH whose text contains NEEDLE
(define (piece s path needle)
  (let ((S (open-and-reduce s)))
    (find (lambda (id) (let ((t (text-of (fld S id 'src)))) (and t (contains? t needle)))) (pieces s path))))

(define (query-rows s goal)
  (let ((a (run s 'query goal)))
    (if (ok? a) (sorted (map cdr (filter (lambda (i) (and (pair? i) (eq? (car i) 'row))) (cdr (cadr a))))) (list 'REFUSED a))))
(define (in-library-of s id)
  (let ((r (query-rows s (format "(in-library ~s ?n)" id)))) (if (and (pair? r) (pair? (car r))) (caar r) r)))
(define (def-for-of s id name) (map car (query-rows s (format "(def-for ~s ~s ?d)" id name))))
(define (names-of s id)
  (let ((a (run s 'names id)))
    (if (ok? a) (sorted (map cadr (filter (lambda (i) (and (pair? i) (eq? (car i) 'name))) (cdr (cadr a))))) (list 'REFUSED a))))
(define (contract-of s id) (let ((a (run s 'names id))) (clause a 'name-use)))

;; a context answer's background entries, as (<id> <first why>)
(define (ctx s . args) (apply run s 'context args))
(define (background a) (let ((c (clause a 'background))) (if c (cadr c) '())))
(define (whys e) (filter (lambda (c) (and (pair? c) (eq? (car c) 'why))) (cdr e)))
(define (entries-by a kind)
  (map car (filter (lambda (e) (exists (lambda (w) (eq? (cadr w) kind)) (whys e))) (background a))))
(define (why-of a id) (let ((e (find (lambda (e) (equal? (car e) id)) (background a)))) (and e (whys e))))
(define (excluded-definitions a) (let ((c (clause a 'excluded))) (and c (cadr (assq 'definitions (cdr c))))))
(define (refused-minimum a) (let ((c (and (pair? a) (eq? (car a) 'error) (clause a 'minimum)))) (and c (cadr c))))

;; ---- C1: two text library files, b importing a ---------------------------------------------

(printf "== C1: a text file's library and imports read from its header ==\n")
(define a-text "(library (a)\n  (export f g)\n  (import (rnrs))\n  (define (g y) y)\n\n  (define (v) '#(1 2))\n\n  (define (f x) (g x))\n\n  (define (a-last) 0))\n")
(define b-text "(library (b)\n  (export h)\n  (import (rnrs) (a))\n  (define (h z) (f z))\n\n  (define (b-last) 0))\n")
(define b-alone-text "(library (b)\n  (export h)\n  (import (rnrs))\n  (define (h z) (f z))\n\n  (define (b-last) 0))\n")
(define c1 (fresh!))
(project! c1 (list (cons "a.sc" a-text) (cons "b.sc" b-text)))
(define c1-f (piece c1 "a.sc" "(define (f x)"))
(define c1-h (piece c1 "b.sc" "(define (h z)"))
(want "C1 SETUP each file split into pieces, the header its own"
      (list (length (pieces c1 "a.sc")) (length (pieces c1 "b.sc")) (and c1-f c1-h #t))
      '(5 3 #t))
(want "C1 in-library answers \"(a)\" and \"(b)\" for every piece"
      (list (map (lambda (id) (in-library-of c1 id)) (pieces c1 "a.sc"))
            (map (lambda (id) (in-library-of c1 id)) (pieces c1 "b.sc")))
      '(("(a)" "(a)" "(a)" "(a)" "(a)") ("(b)" "(b)" "(b)")))
(want "C1 def-for answers a's definition of f, through the first clause (its def names the library \"(a)\")"
      (list (def-for-of c1 c1-h "f") (query-rows c1 (format "(def ~s ?l \"f\")" c1-f)))
      (list (list c1-f) '(("(a)"))))
(want "C1 the definitions index records the library as library-name-proper gives it: (library (a))"
      (let* ((a (run c1 'whereis "f")) (items (and (ok? a) (cdr (cadr a)))) (r (and items (assoc c1-f (map cdr items)))))
        (and r (assq 'library (cdr r))))
      '(library (a)))
(want "C1 uses of f answers b's piece with the located id: b's file holder"
      (let* ((a (run c1 'uses "f")) (items (and (ok? a) (cdr (cadr a))))
             (u (and items (find (lambda (i) (equal? (cadr i) c1-h)) items))))
        (and u (cadr (assq 'library (cddr u)))))
      (holder-of c1 "b.sc"))

(define c1b (fresh!))
(project! c1b (list (cons "a.sc" a-text) (cons "b.sc" b-alone-text)))
(want "C1b b not importing a: def-for answers nothing for f"
      (def-for-of c1b (piece c1b "b.sc" "(define (h z)") "f")
      '())

;; ---- C1c: a datum library, and a text file under one --------------------------------------

(printf "== C1c: the library ancestor first ==\n")
(define c1c (fresh!))
(let ((d (new-dir!)))
  (write-file! (string-append d "/split/c.sc") "(library (c)\n  (export p)\n  (import (rnrs) (cimp))\n  (define (p) (q))\n  (define (q) 1))\n")
  (run c1c 'import-code (string-append d "/split") "--datum"))
(define c1c-lib (let ((S (open-and-reduce c1c))) (find (lambda (id) (eq? (fld S id 'kind) 'library)) (state-block-ids S))))
(define c1c-p (let ((S (open-and-reduce c1c)))
                (find (lambda (id) (and (eq? (fld S id 'kind) 'code) (equal? (fld S id 'body) '(define (p) (q))))) (state-block-ids S))))
(want "C1c PIN a datum library's code is in it, and its imports are its own"
      (list (and c1c-p (in-library-of c1c c1c-p)) (query-rows c1c "(imports \"(c)\" ?n)"))
      '("(c)" (("(cimp)") ("(rnrs)"))))
(project! c1c (list (cons "t.sc" "(import (rnrs) (zz))\n(define (u) (w))\n")))
(run c1c 'move (holder-of c1c "t.sc") c1c-lib)
(define c1c-u (piece c1c "t.sc" "(define (u)"))
(want "C1c a text file under the datum library: its pieces are in (c), and the file's own header imports are not in their scope (green before the change too: the row holds the script-import rule to its none condition)"
      (list (in-library-of c1c c1c-u) (query-rows c1c (format "(scope-name ~s ?n)" c1c-u)))
      '("(c)" (("(c)") ("(cimp)") ("(rnrs)"))))

;; ---- C1d: the three relations are external ------------------------------------------------

(printf "== C1d: in-library, imports and file-imports are read through the catalogue ==\n")
(define (rule-answer goal) (let ((v (rule-value-check (list '(on section) (list 'must-not goal))))) (if (and (pair? v) (eq? (car v) 'error)) v 'ok)))
(want "C1d a rule naming in-library is refused as one naming def is"
      (list (rule-answer '(def ?d ?l ?n)) (rule-answer '(in-library ?b ?n)) (rule-answer '(imports ?a ?b)))
      '((error bad-request rule-relation-not-allowed (relation def))
        (error bad-request rule-relation-not-allowed (relation in-library))
        (error bad-request rule-relation-not-allowed (relation imports))))

;; ---- C1e: a script's imports ------------------------------------------------------------------

(printf "== C1e: a script's top-level import ==\n")
(define c1e (fresh!))
(project! c1e (list (cons "a.sc" a-text) (cons "s.sc" "(import (rnrs) (a))\n(define (k) (f 1))\n")))
(define c1e-k (piece c1e "s.sc" "(define (k)"))
(want "C1e the script is in no library, its holder imports \"(a)\", and def-for answers a's f"
      (list (in-library-of c1e c1e-k)
            (and (member '("(a)") (query-rows c1e (format "(file-imports ~s ?n)" (holder-of c1e "s.sc")))) #t)
            (def-for-of c1e c1e-k "f"))
      (list 'none #t (list (piece c1e "a.sc" "(define (f x)"))))
(define c1e2 (fresh!))
(project! c1e2 (list (cons "a.sc" a-text) (cons "s.sc" "(import (rnrs))\n(define (k) (f 1))\n")))
(want "C1e the same script without the import: def-for answers nothing"
      (def-for-of c1e2 (piece c1e2 "s.sc" "(define (k)") "f")
      '())

;; ---- C1f, C1h: where the header is ------------------------------------------------------------

(printf "== C1f, C1h: the header across a mark, a comment-only piece, a cut in the import ==\n")
(define c1f (fresh!))
(project! c1f (list (cons "a.sc" (string-append (string (integer->char #xFEFF)) a-text))))
;; E4: A SRC STORED AS BYTES NEVER BRINGS THE MARK TO THE READER, since
;; utf8->string drops a leading U+FEFF, so this row cannot tell a reader that
;; consumes the mark from one that does not: it holds that such a file keeps
;; its name. C3j reads a string with the mark, and that is where m14 dies.
(want "C1f PIN a library file that begins with the byte-order mark: every piece is in (a)"
      (map (lambda (id) (in-library-of c1f id)) (pieces c1f "a.sc"))
      '("(a)" "(a)" "(a)" "(a)" "(a)"))
(define c1h (fresh!))
(import-as-is! c1h
  (list (cons "comment.sc" (string-append marker ";; a comment, and nothing else\n" marker "(library (a)\n  (export f)\n  (import (rnrs) (ca))\n" marker "  (define (f x) x))\n"))
        (cons "cut.sc" (string-append marker "(library (k)\n  (export f2)\n  (import (rnrs)\n" marker "          (kb))\n" marker "  (define (f2 x) x))\n"))
        (cons "long.sc" (string-append marker "(library (l)\n  (export f3)\n  ;; " (make-string 70000 #\x) "\n  (import (rnrs))\n" marker "  (define (f3 x) x))\n"))))
(want "C1h SETUP the pieces are where the markers put them"
      (map (lambda (p) (length (pieces c1h p))) '("comment.sc" "cut.sc" "long.sc"))
      '(3 3 2))
(want "C1h the header spans the pieces: names and imports for a comment-only first piece and a cut inside the import form"
      (list (map (lambda (id) (in-library-of c1h id)) (pieces c1h "comment.sc"))
            (map (lambda (id) (in-library-of c1h id)) (pieces c1h "cut.sc"))
            (query-rows c1h "(imports \"(a)\" ?n)") (query-rows c1h "(imports \"(k)\" ?n)"))
      '(("(a)" "(a)" "(a)") ("(k)" "(k)" "(k)") (("(ca)") ("(rnrs)")) (("(kb)") ("(rnrs)"))))
;; A HEADER LOOKUP COSTS THE PREFIX: a small header piece, then a piece of
;; 8 MB. The pieces after the header are read only up to the limit, so the
;; lookup allocates about the prefix, not the piece (r1 finding: the whole
;; piece was copied, then cut).
(define c1h-big (fresh!))
(import-as-is! c1h-big (list (cons "big.sc" (string-append marker "(library (big)\n  (export f)\n  (import (rnrs))\n" marker ";; " (make-string 8000000 #\x) "\n  (define (f) 1))\n"))))
(want "C1h a header read beside an 8 MB piece allocates less than 2 MB"
      (guard (e (#t 'NO-FILE-HEADER))
        (let* ((file-header (eval 'file-header (environment '(theourgia store))))
               (S (open-and-reduce c1h-big)) (h (holder-of c1h-big "big.sc"))
               (before (sstats-bytes (statistics))) (hdr (file-header S h)) (after (sstats-bytes (statistics))))
          (list (and (pair? hdr) (car hdr)) (< (- after before) 2000000))))
      '(library #t))
(want "C1h a header that has not closed its import form in 65536 bytes: no name (green before the change, when no file has one)"
      (map (lambda (id) (in-library-of c1h id)) (pieces c1h "long.sc"))
      '(none none))

;; ---- C1i: a header that cannot be read is no header, and nothing raises ---------------------

(printf "== C1i: a first piece that is not UTF-8, and a header the reader refuses ==\n")
;; THE HEADER READ HAS NO GUARD (unreadable-census U11-ii): it answers every
;; input it can meet by value. No verb stores bytes that are not UTF-8, so
;; u.sc's first piece is set through with-store-write, as code-import's rows
;; do. The two bytes before its library form decode to U+FFFD, which the
;; reader takes as the first form, so the file has no header. r.sc's import
;; form holds a vector, which the reader refuses.
(define c1i (fresh!))
(project! c1i (list (cons "u.sc" "(library (u)\n  (export f)\n  (import (rnrs))\n  (define (f) 1)\n\n  (define (u-last) 0))\n")
                    (cons "r.sc" "(library (r)\n  (export f)\n  (import (rnrs) #(1))\n  (define (f) 1)\n\n  (define (r-last) 0))\n")))
(define c1i-u-head (let ((ps (pieces c1i "u.sc"))) (and (pair? ps) (car ps))))
(with-store-write c1i
  (lambda (st v)
    (list (list 'set c1i-u-head 'src
                (u8-list->bytevector (append '(#xFF #xFE) (bytevector->u8-list (string->utf8 "(library (u)\n  (export f)\n  (import (rnrs))\n")))))))
  "test")
(want "C1i SETUP u.sc's first piece holds bytes that are not UTF-8, and utf8->string gives U+FFFD for them"
      (let ((v (fld (open-and-reduce c1i) c1i-u-head 'src)))
        (list (length (pieces c1i "u.sc")) (length (pieces c1i "r.sc"))
              (and (bytevector? v) (bytevector-u8-ref v 0)) (and (bytevector? v) (string-ref (utf8->string v) 0))))
      (list 3 3 #xFF #\xFFFD))
(want "C1i no header for either file, and in-library none for every piece: nothing raises"
      (let ((file-header (guard (e (#t #f)) (eval 'file-header (environment '(theourgia store)))))
            (S (open-and-reduce c1i)))
        (list (and file-header (list (file-header S (holder-of c1i "u.sc")) (file-header S (holder-of c1i "r.sc"))))
              (map (lambda (id) (in-library-of c1i id)) (pieces c1i "u.sc"))
              (map (lambda (id) (in-library-of c1i id)) (pieces c1i "r.sc"))))
      '((#f #f) (none none none) (none none none)))

;; ---- C2: no-library definitions are scoped by file ---------------------------------------------

(printf "== C2: a no-library definition's scope is its file ==\n")
(define c2 (fresh!))
(project! c2 (list (cons "x1.sc" "(define (A) 1)\n\n(define (u) (A))\n") (cons "x2.sc" "(define (A) 2)\n")))
(want "C2 a use of A in the first file answers the first file's A only"
      (def-for-of c2 (piece c2 "x1.sc" "(define (u)") "A")
      (list (piece c2 "x1.sc" "(define (A) 1)")))
(define c2b (fresh!))
(define c2b-doc (new-id (run c2b 'insert "--title" "doc")))
(define (datum-block! s parent body)
  (let ((id (new-id (run s 'insert "--under" parent "--title" "datum"))))
    (run s 'set id "kind" "code") (run s 'set id "mode" "datum") (run s 'set id "lang" "scheme") (run s 'set id "body" body)
    id))
(define c2b-1 (datum-block! c2b c2b-doc "(define (A) (A))"))
(define c2b-2 (datum-block! c2b c2b-doc "(define (A) (A))"))
(want "C2b a datum code block under a document is its own holder"
      (query-rows c2b (format "(holder ~s ?h)" c2b-1))
      (list (list c2b-1)))
(want "C2b two such blocks each defining and using A see no definition of each other's"
      (list (def-for-of c2b c2b-1 "A") (def-for-of c2b c2b-2 "A"))
      '(() ()))

;; ---- C3: a text block read as forms ------------------------------------------------------------

(printf "== C3: name use reads the text as forms ==\n")
(define c3 (fresh!))
(define c3-files
  (list (cons "c3.sc" "(define (f S) (g S))\n")
        (cons "c3b.sc" "(define (f S) (g S))\n(\n")
        (cons "c3c.sc" ";; with-store-write\n")
        (cons "c3d.sc" "#;(x y)\n#| a comment |#\n(define (f) (k))\n")
        (cons "c3d2.sc" "#0=(a . #0#)\n")
        (cons "c3f.sc" "(foreign-procedure (__varargs_after 1) \"f\" (int) int)\n")
        (cons "c3f2.sc" "(foreign-procedure (__varargs_after 99999999999999999999) \"f\" (int) int)\n")
        (cons "c3g.sc" "(g #(x))\n")
        (cons "c3h.sc" "(define (f) (g 99999999999999999999 1.5))\n")))
(import-as-is! c3 c3-files)
(define (only-piece s path) (let ((ps (pieces s path))) (and (= 1 (length ps)) (car ps))))
(define c3-1 (only-piece c3 "c3.sc"))
(want "C3 (define (f S) (g S)): the names are g, the contract (reading forms), and uses says (mode text)"
      (list (names-of c3 c3-1) (contract-of c3 c3-1)
            (let* ((a (run c3 'uses "g")) (u (and (ok? a) (find (lambda (i) (equal? (cadr i) c3-1)) (cdr (cadr a))))))
              (and u (assq 'mode (cddr u)))))
      '((g) (name-use syntactic (reading forms)) (mode text)))
(want "C3b the same followed by an open bracket: the whole piece is tokens, S among them, (reason unbalanced)"
      (let ((id (only-piece c3 "c3b.sc"))) (list (and (memq 'S (names-of c3 id)) #t) (contract-of c3 id)))
      '(#t (name-use syntactic (lexing whole-text) (reason unbalanced))))
(define c3b2 (fresh!))
(import-as-is! c3b2 (list (cons "half.sc" (string-append marker "(define (f S)\n" marker "  (g S))\n"))))
(want "C3b the two halves of a definition cut by a marker line both fall back"
      (map (lambda (id) (contract-of c3b2 id)) (pieces c3b2 "half.sc"))
      '((name-use syntactic (lexing whole-text) (reason unbalanced)) (name-use syntactic (lexing whole-text) (reason unbalanced))))
(want "C3c a piece that is a comment naming with-store-write uses nothing"
      (names-of c3 (only-piece c3 "c3c.sc"))
      '())
(want "C3d datum and block comments read; a datum label falls back with (reason label)"
      (list (names-of c3 (only-piece c3 "c3d.sc")) (contract-of c3 (only-piece c3 "c3d.sc")) (contract-of c3 (only-piece c3 "c3d2.sc")))
      '((k) (name-use syntactic (reading forms)) (name-use syntactic (lexing whole-text) (reason label))))
(want "C3f the foreign-procedure rule matches an exact count: int is not a use"
      (names-of c3 (only-piece c3 "c3f.sc"))
      '())
(want "C3f PIN an oversized count is not matched: the fallback walk names the types"
      (and (memq 'int (names-of c3 (only-piece c3 "c3f2.sc"))) #t)
      #t)
(want "C3g a vector literal is unsupported: the whole piece falls back"
      (contract-of c3 (only-piece c3 "c3g.sc"))
      '(name-use syntactic (lexing whole-text) (reason unsupported)))
(want "C3h numbers are no names: (reading forms) and exactly {g}"
      (list (contract-of c3 (only-piece c3 "c3h.sc")) (names-of c3 (only-piece c3 "c3h.sc")))
      '((name-use syntactic (reading forms)) (g)))

;; THE LIMITS, each answered in a second
(define (rep n s) (let loop ((k 0) (out '())) (if (= k n) (apply string-append out) (loop (+ k 1) (cons s out)))))
(define c3e (fresh!))
(import-as-is! c3e
  (list (cons "deep.sc" (string-append (rep 1001 "(") (rep 1001 ")") "\n"))
        (cons "token.sc" (string-append (make-string 70000 #\a) "\n"))
        (cons "cdeep.sc" (string-append "#;" (rep 1001 "(") (rep 1001 ")") "\n(f)\n"))
        (cons "nodes.sc" (string-append (rep 200001 "a ") "\n"))
        (cons "cnodes.sc" (string-append "#;(" (rep 200001 "a ") ")\n(f)\n"))))
(define (timed-contract s id)
  (let* ((t0 (real-time)) (c (contract-of s id)) (ms (- (real-time) t0)))
    (list c (< ms 1000))))
;; r2's two findings: the lists quote marks build are nodes (201 groups of
;; 1000 marks and an atom, every one at a legal depth), and a token that
;; begins like a boolean is held to the token limit like any other
(import-as-is! c3e (list (cons "comments.sc" (string-append (rep 1001 "#;") "x\n"))
                         (cons "marks.sc" (string-append (rep 201 (string-append (rep 1000 "'") "x ")) "\n"))
                         (cons "booltoken.sc" (string-append "#t" (make-string 70000 #\a) "\n"))))
(want "C3e the limits: depth, token, depth inside #;, nodes, nodes inside #;, each within a second; 1001 #; waiting is depth too; marks' lists are nodes; a boolean-shaped token is held to the token limit"
      (map (lambda (p) (timed-contract c3e (only-piece c3e p))) '("deep.sc" "token.sc" "cdeep.sc" "nodes.sc" "cnodes.sc" "comments.sc" "marks.sc" "booltoken.sc"))
      '(((name-use syntactic (lexing whole-text) (reason depth)) #t)
        ((name-use syntactic (lexing whole-text) (reason token)) #t)
        ((name-use syntactic (lexing whole-text) (reason depth)) #t)
        ((name-use syntactic (lexing whole-text) (reason nodes)) #t)
        ((name-use syntactic (lexing whole-text) (reason nodes)) #t)
        ((name-use syntactic (lexing whole-text) (reason depth)) #t)
        ((name-use syntactic (lexing whole-text) (reason nodes)) #t)
        ((name-use syntactic (lexing whole-text) (reason token)) #t)))

;; THE READER ITSELF, reached by eval: before the change it is not there
(define (reader-call name . args)
  (guard (e (#t 'NO-READER))
    (apply (eval name (environment '(theourgia reader))) args)))
(define (number-text-of x)
  (and (eq? #t (reader-call 'number-text? x)) (list 'number-text (reader-call 'number-text-text x))))
(want "C3j the reader: small integers exact, every other number an opaque record carrying its text"
      (list (reader-call 'read-datum "12") (reader-call 'read-datum "-7")
            (number-text-of (reader-call 'read-datum "99999999999999999999"))
            (number-text-of (reader-call 'read-datum "1.5"))
            (number-text-of (reader-call 'read-datum "#e1e100000"))
            (let ((d (reader-call 'read-datum "(f 99999999999999999999)")))
              (and (pair? d) (number-text-of (cadr d))))
            ;; THE MARK, given as text: Chez's utf8->string already drops a
            ;; leading byte-order mark, so a src stored as bytes never brings
            ;; one here; a src stored as a string can
            (reader-call 'read-forms (string-append (string (integer->char #xFEFF)) "(a)")))
      '(12 -7 (number-text "99999999999999999999") (number-text "1.5") (number-text "#e1e100000")
        (number-text "99999999999999999999") ((a))))

;; C3i, A READING: a wide quasiquote template and a wide convention list
(define c3i (fresh!))
(define (qq-text n) (string-append "`(" (rep n "a1 ") ")\n"))
(define (fp-text n) (string-append "(foreign-procedure " (rep n "c1 ") "\"f\" () void)\n"))
(import-as-is! c3i (list (cons "qq10k.sc" (qq-text 10000)) (cons "qq200k.sc" (qq-text 199999))
                         (cons "fp10k.sc" (fp-text 10000))))
(for-each (lambda (p)
            (let* ((id (only-piece c3i p)) (t0 (real-time)) (n (names-of c3i id)) (ms (- (real-time) t0)))
              (printf "reading C3i ~a: names ~a, contract ~s, ~a ms\n" p (if (list? n) (length n) n) (contract-of c3i id) ms)))
          '("qq10k.sc" "qq200k.sc" "fp10k.sc"))
(want "C3i a quasiquoted flat list of 10000 atoms reads and uses nothing"
      (let ((id (only-piece c3i "qq10k.sc"))) (list (names-of c3i id) (contract-of c3i id)))
      '(() (name-use syntactic (reading forms))))

;; C3k: the last piece of a library file
(define c3k (fresh!))
(import-as-is! c3k
  (list (cons "lib.sc" (string-append marker "(library (L)\n  (export f)\n  (import (rnrs))\n" marker "  (define (f x) (g x)))\n"))
        (cons "plain.sc" (string-append marker "(define (z) 1)\n" marker "  (define (f x) (g x)))\n"))
        (cons "after.sc" (string-append marker "(library (M)\n  (export f)\n  (import (rnrs))\n" marker "  (define (f x) (g x))) (h)\n"))))
(define (last-piece s p) (let ((ps (pieces s p))) (and (pair? ps) (list-ref ps (- (length ps) 1)))))
(want "C3k under a library header the last piece reads, the library's closer tolerated: {g}"
      (let ((id (last-piece c3k "lib.sc"))) (list (contract-of c3k id) (names-of c3k id)))
      '((name-use syntactic (reading forms)) (g)))
(want "C3k the same piece in a file whose header is no library falls back; a closer followed by a form falls back"
      (list (contract-of c3k (last-piece c3k "plain.sc")) (contract-of c3k (last-piece c3k "after.sc")))
      '((name-use syntactic (lexing whole-text) (reason unbalanced)) (name-use syntactic (lexing whole-text) (reason unbalanced))))

;; ---- C4: ambiguity folded ---------------------------------------------------------------------------

(printf "== C4: ambiguity folded per (using block, name) ==\n")
(define c4 (fresh!))
(project! c4 (list (cons "X.sc" "(define (x) 1)\n\n(define (x) 2)\n\n(define (x) 3)\n\n(define (c) (x))\n")
                   (cons "Y.sc" "(define (y) 1)\n\n(define (y) 2)\n\n(define (cy) (y))\n")))
(define c4-defs (filter (lambda (id) (not (equal? id (piece c4 "X.sc" "(define (c)")))) (pieces c4 "X.sc")))
(define c4-caller (piece c4 "X.sc" "(define (c)"))
(define c4-a (ctx c4 "--for" c4-caller "--budget" "100000"))
(want "C4 SETUP three definitions of x in the caller's file, ids in order"
      (list (length c4-defs) (equal? c4-defs (list-sort string<? c4-defs)))
      '(3 #t))
(want "C4 the first two by id are entered, the fold is said, nothing excluded"
      (list (entries-by c4-a 'defines) (clause c4-a 'folded) (excluded-definitions c4-a))
      (list (list (car c4-defs) (cadr c4-defs))
            (list 'folded (list "x" c4-caller '(candidates 3) '(shown 2)))
            0))
(define c4-y (piece c4 "Y.sc" "(define (cy)"))
(define c4b-a (ctx c4 "--for" c4-y "--budget" "100000"))
(want "C4b PIN exactly two definitions: both entered, no folded clause"
      (list (length (entries-by c4b-a 'defines)) (clause c4b-a 'folded))
      '(2 #f))
;; the smallest budget at which one definition is entered
(define c4-min (refused-minimum (ctx c4 "--for" c4-caller "--budget" "1")))
(define c4c-a
  (let loop ((b (or c4-min 1000)) (k 0))
    (let ((a (ctx c4 "--for" c4-caller "--budget" (number->string b))))
      (cond ((> k 4000) a)
            ((and (ok? a) (pair? (entries-by a 'defines))) a)
            (else (loop (+ b 1) (+ k 1)))))))
(want "C4c a budget that admits one: (shown 1), and the other counted excluded"
      (list (length (entries-by c4c-a 'defines)) (clause c4c-a 'folded) (excluded-definitions c4c-a))
      (list 1 (list 'folded (list "x" c4-caller '(candidates 3) '(shown 1))) 1))

;; C4e: overlapping groups, datum blocks under one file holder (a datum
;; block's definitions are all its names)
(define c4e (fresh!))
(define c4e-h (new-id (run c4e 'insert "--title" "H")))
(define c4e-ids (map (lambda (k) (new-id (run c4e 'insert "--under" c4e-h "--title" (format "b~a" k)))) (iota 8)))
(run c4e 'set c4e-h "kind" "file")
(define c4e-user (car c4e-ids))
;; d1 ... d7 BY ID, whatever order the inserts gave them
(define c4e-d (list-sort string<? (cdr c4e-ids)))
(for-each (lambda (id body)
            (run c4e 'set id "kind" "code") (run c4e 'set id "mode" "datum") (run c4e 'set id "lang" "scheme") (run c4e 'set id "body" body))
          (cons c4e-user c4e-d)
          '("(define (user) (x) (y))" "(define-values (x y) (values 1 1))" "(define (x) 2)" "(define (x) 3)" "(define (x) 4)"
            "(define (y) 5)" "(define (y) 6)" "(define (y) 7)"))
(define c4e-a (ctx c4e "--for" c4e-user "--budget" "100000"))
(want "C4e SETUP the ids sort in creation order"
      (equal? c4e-d (list-sort string<? c4e-d))
      #t)
(want "C4e overlapping groups: x enters d1 d2 of 4, y enters d5 d6 of 3 (d1 already seen), d1 once"
      (list (entries-by c4e-a 'defines) (clause c4e-a 'folded))
      (list (list (list-ref c4e-d 0) (list-ref c4e-d 1) (list-ref c4e-d 4) (list-ref c4e-d 5))
            (list 'folded (list "x" c4e-user '(candidates 4) '(shown 2)) (list "y" c4e-user '(candidates 3) '(shown 2)))))
;; C4f: a folded-away definition relevant to T's text is entered by relevance
(define c4f (fresh!))
(project! c4f (list (cons "X.sc" "(define (x) 1)\n\n(define (x) 2)\n\n(define (x) \"zebra quagga okapi\")\n\n(define (c) (x) \"zebra quagga okapi\")\n")))
(define c4f-caller (piece c4f "X.sc" "(define (c)"))
(define c4f-third (piece c4f "X.sc" "(define (x) \"zebra"))
(run c4f 'set c4f-caller "title" "zebra")
(define c4f-a (ctx c4f "--for" c4f-caller "--budget" "100000"))
(want "C4f the third definition, folded away, is entered by the relevance step for its own reason"
      (let ((w (why-of c4f-a c4f-third))) (and w (map cadr w)))
      '(relevant))
;; C4d: the human mode prints both new clauses
(define c5-names-answer #f)

;; ---- C5: the names the edit will use ----------------------------------------------------------------

(printf "== C5: --names ==\n")
(define c5 (fresh!))
(project! c5 (list (cons "store.sc" "(library (theourgia store)\n  (export with-store-write other)\n  (import (rnrs))\n  (define (with-store-write x) x)\n\n  (define (other) 1))\n")
                   (cons "working.sc" "(library (theourgia working)\n  (export w)\n  (import (rnrs) (theourgia store))\n  (define (w) 1)\n\n  (define (w2) 2))\n")))
(define c5-sws (piece c5 "store.sc" "(define (with-store-write"))
(define c5-w (piece c5 "working.sc" "(define (w)"))
(define c5-a (ctx c5 "--for" c5-w "--budget" "20000" "--names" "with-store-write" "--names" "no-such-name"))
(set! c5-names-answer c5-a)
(want "C5 the named definition arrives with (why names ...), and asked names the one with no definition"
      (list (why-of c5-a c5-sws) (clause c5-a 'asked))
      (list '((why names "with-store-write"))
            '(asked (names "with-store-write" "no-such-name") (no-definition "no-such-name"))))
(want "C5 the receipt passed back to commit --premises is accepted"
      (let* ((r (clause c5-a 'receipt))
             (w (writer-of c5)))
        (car (run c5 'commit "--writer" w "--premises" (sexpr->string-extended r))))
      'ok)
;; THE COMMAND LINE parses the request as the dispatcher does: two --names
(define (cli-answer . args)
  (let ((out (string-append root "/cli.out")))
    (system (string-append "THEOURGIA_LOCAL=1 scheme --script ../core.sc "
                           (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                           "--store '" c5 "' --wire > '" out "' 2>/dev/null < /dev/null"))
    (guard (e (#t (list 'UNREADABLE (call-with-input-file out get-string-all))))
      (call-with-input-file out read))))
(want "C5 through the command line: both --names are taken, and asked answers both"
      (clause (cli-answer "context" "--for" c5-w "--budget" "20000" "--names" "with-store-write" "--names" "no-such-name") 'asked)
      '(asked (names "with-store-write" "no-such-name") (no-definition "no-such-name")))
(define c5b-a (ctx c5 "--for" (holder-of c5 "working.sc") "--budget" "20000" "--names" "with-store-write"))
(want "C5b T the file holder: the same definition arrives"
      (why-of c5b-a c5-sws)
      '((why names "with-store-write")))
(define c5c-a (ctx c4 "--for" c4-caller "--budget" "100000" "--names" "x"))
(want "C5c a requested name is never folded: d1 d2 d3 by names, no folded entry"
      (list (entries-by c5c-a 'names) (clause c5c-a 'folded))
      (list c4-defs #f))
(define c5d (fresh!))
(project! c5d (list (cons "one.sc" "(define (f) 1)\n")))
(define c5d-f (car (pieces c5d "one.sc")))
(want "C5d a name defined only by T is in-hard; T a holder: in-hard names the child; a name defined nowhere is no-definition"
      (list (clause (ctx c5d "--for" c5d-f "--budget" "20000" "--names" "f") 'asked)
            (clause (ctx c5d "--for" (holder-of c5d "one.sc") "--budget" "20000" "--names" "f") 'asked)
            (clause (ctx c5d "--for" c5d-f "--budget" "20000" "--names" "zzz") 'asked))
      (list (list 'asked '(names "f") (list 'in-hard "f" c5d-f))
            (list 'asked '(names "f") (list 'in-hard "f" c5d-f))
            '(asked (names "zzz") (no-definition "zzz"))))
(define c5e (fresh!))
(project! c5e (list (cons "five.sc" "(define (f) 1)\n\n(define (f) 2)\n\n(define (f) 3)\n\n(define (f) 4)\n\n(define (f) 5)\n")))
(define c5e-h (holder-of c5e "five.sc"))
(define c5e-min (refused-minimum (ctx c5e "--for" c5e-h "--budget" "1" "--names" "f")))
(define c5e-a (and c5e-min (ctx c5e "--for" c5e-h "--budget" (number->string c5e-min) "--names" "f")))
(want "C5e five children defining f, at the minimum the request reports: ok, five in-hard rows"
      (list (and (ok? c5e-a) 'ok) (let ((c (clause c5e-a 'asked))) (and c (length (filter (lambda (x) (eq? (car x) 'in-hard)) (cdr c))))))
      '(ok 5))
(want "C4d the human mode prints the folded line and the asked line"
      (list (contains? (render-human c4-a) "(folded (\"x\"") (contains? (render-human c5-names-answer) "(asked (names"))
      '(#t #t))

;; ---- C6: min+ ---------------------------------------------------------------------------------------

(printf "== C6: --budget min+<t> ==\n")
(define c6-min (refused-minimum (ctx c1 "--for" c1-h "--budget" "1")))
(define c6-a (ctx c1 "--for" c1-h "--budget" "min+500"))
(want "C6 min+500 answers ok at the refused minimum plus 500, and says the number"
      (list (and (ok? c6-a) 'ok) (let ((b (clause c6-a 'budget))) (and b (cadr (assq 'tokens (cdr b))))))
      (list 'ok (and c6-min (+ c6-min 500))))

;; ---- C7: the hard material is untouched by names -------------------------------------------------------

(printf "== C7: --names leaves the hard material and the receipt as they are ==\n")
(define c7 c5)
(define c7-rule (new-id (run c7 'insert "--title" "a ruling about w")))
(run c7 'set c7-rule "class" "ruling")
(run c7 'link c5-w "depends-on" c7-rule)
(define c7-plain (ctx c7 "--for" c5-w "--budget" "20000"))
(define c7-named (ctx c7 "--for" c5-w "--budget" "20000" "--names" "with-store-write"))
(define (hard-part a) (map (lambda (n) (clause a n)) '(for constraints evidence to-verify notes)))
(want "C7 the hard sections and the receipt are byte-identical with and without --names, the named definition in background"
      (list (equal? (hard-part c7-plain) (hard-part c7-named))
            (and (pair? (cadr (clause c7-named 'constraints))) #t)
            (string=? (render-wire (clause c7-plain 'receipt)) (render-wire (clause c7-named 'receipt)))
            (why-of c7-named c5-sws))
      '(#t #t #t ((why names "with-store-write"))))

;; ---- C1g, C8, C8b, C8c: one session kept across calls ------------------------------------------------------

(printf "== C1g, C8: a kept session, in process ==\n")
;; THE DAEMON'S PROVIDER, in process: its published state is the box's, its
;; trace collected; installed as the daemon installs it
(define (in-process s)
  (let* ((box (vector (open-and-reduce s))) (seen '())
         (p (make-session-provider (lambda () (vector-ref box 0)) (lambda () 0) 1000000
                                   (lambda words (set! seen (append seen (list words)))))))
    (query-session-provider! p)
    (list p box (lambda () (let ((x seen)) (set! seen '()) x)))))
(define (ask-in ip s . args)
  (let ((a (rpc-dispatch s args "test" (vector-ref (cadr ip) 0) #f #f #f)))
    (list a ((car ip) 'served) ((caddr ip)))))
(define scheme-entry (language-for-name "scheme"))
(define (entry-without keys) (filter (lambda (p) (not (memq (car p) keys))) scheme-entry))
(define (restore-catalogue!) (register-language! scheme-entry))

(define g-ip (in-process c1))
(define g1 (ask-in g-ip c1 'query (format "(in-library ~s ?n)" c1-h)))
(register-language! (entry-without '(reader header)))
(define g2 (ask-in g-ip c1 'query (format "(in-library ~s ?n)" c1-h)))
(restore-catalogue!)
(want "C1g the catalogue replaced, no store write: the next in-library answers none, and the session was dropped for the catalogue"
      (list (cdr (cadr (cadr (car g1)))) (cdr (cadr (cadr (car g2)))) (and (member '(dropped catalogue) (caddr g2)) #t))
      '(("(b)") (none) #t))

(define c8 (fresh!))
(project! c8 (list (cons "r.sc" "(define (old) 1)\n\n(define (t) (old) (new))\n")))
(define c8-d (piece c8 "r.sc" "(define (old)"))
(define c8-t (piece c8 "r.sc" "(define (t)"))
(define c8-ip (in-process c8))
(define (defines-pairs a) (map (lambda (e) (let ((w (car (whys e)))) (list (caddr w) (car e))))
                               (filter (lambda (e) (exists (lambda (w) (eq? (cadr w) 'defines)) (whys e))) (background a))))
(define c8-1 (ask-in c8-ip c8 'context "--for" c8-t "--budget" "20000"))
(define c8-2 (ask-in c8-ip c8 'context "--for" c8-t "--budget" "20000"))
(want "C8b the served line after a context call carries the four counters; the second call reports no index build and a reused session"
      (let ((s1 (cadr c8-1)) (s2 (cadr c8-2)))
        (list (and (list? s1) (for-all (lambda (k) (and (assq k (cdddr s1)) #t)) '(defs-index-builds fact-builds preferred-candidates session)))
              (and (list? s2) (assq 'defs-index-builds (cdddr s2)))
              (and (list? s2) (assq 'session (cdddr s2)))))
      '(#t (defs-index-builds 0) (session reused)))
(run c8 'set c8-d "src" "(define (new) 1)\n\n")
(vector-set! (cadr c8-ip) 0 (open-and-reduce c8))
(define c8-3 (ask-in c8-ip c8 'context "--for" c8-t "--budget" "20000"))
(want "C8 PIN a definition renamed between two calls of one session: the old name's entry is gone, the new name's is there"
      (list (defines-pairs (car c8-1)) (defines-pairs (car c8-3)))
      (list (list (list "old" c8-d)) (list (list "new" c8-d))))
(define c8c-1 (ask-in c8-ip c8 'context "--for" c8-t "--budget" "20000"))
(register-language! (cons (list 'def-heads '("^\\(define-syntax\\s+([^\\s()\\[\\]\";]+)")) (entry-without '(def-heads))))
(define c8c-2 (ask-in c8-ip c8 'context "--for" c8-t "--budget" "20000"))
(restore-catalogue!)
(want "C8c at one cut, the catalogue's def-heads without define: the next call answers no definitions"
      (list (length (defines-pairs (car c8c-1))) (length (defines-pairs (car c8c-2))))
      '(1 0))
(query-session-provider! #f)

;; THE RELEVANT STEP ASKS A SOURCE BY ITS DISTINCT TOKENS. T's source is one
;; word eight times and every block's source holds that word, so asking the
;; source as written judges eight positions in every block and asking its
;; distinct tokens judges one. Context's searches are the title's and the
;; source's: at most (title tokens + distinct source tokens) positions per
;; block, here 2 per block.
(define sl3 (fresh!))
(define sl3-ids
  (map (lambda (t) (new-id (run sl3 'insert "--title" t))) '("tee" "you" "vee" "dub")))
(for-each (lambda (id src) (run sl3 'set id "src" src))
          sl3-ids
          (list "zeta zeta zeta zeta zeta zeta zeta zeta" "zeta here" "zeta there" "zeta again"))
(want "SL-3 context on a block whose source repeats one word judges at most 2 positions per block, and the relevant step still names the three"
      (let* ((b (length (state-outline (open-and-reduce sl3))))
             (p0 (search-positions-judged-count))
             (a (ctx sl3 "--for" (car sl3-ids) "--budget" "20000"))
             (judged (- (search-positions-judged-count) p0)))
        (list (ok? a) (<= judged (* 2 b)) (list-sort string<? (entries-by a 'relevant))))
      (list #t #t (list-sort string<? (cdr sl3-ids))))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\ncontext-targeting complete\n" rows bad)
(exit (if (= bad 0) 0 1))
