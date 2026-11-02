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

;; import-code --symbols, the answers, the cuts and the ends:
;;   SI-1 the human form prints what the answer lists after its items (the
;;        skipped clause and the three of --symbols), and the wire form is
;;        unchanged;
;;   SI-2 one supplier of cuts: the import cuts where split-suggest --symbols
;;        cuts, the comment lines above a definition going with it;
;;   SI-3 an end is never a refusal: it moves forward to where the scanner
;;        sees top-level code begin, and is no cut when that is the next
;;        block's start; a start inside a body is still refused, by name;
;;   SI-4 import --datum's skipped clause and uses' skipped counts are
;;        printed last without --wire as well;
;;   SI-5 the cuts cost time linear in the file.
;; On the tree before these rules SI-1's first row prints nothing, SI-2 cuts
;; each JSDoc off its definition, SI-3 refuses the file at an end,
;; SI-3c's refusal does not name the symbol, SI-4's human forms end
;; before the skipped clause, and SI-5's 4N file costs about 16 times the N.
;;
;; Every run is core.sc in a child process on the local route
;; (THEOURGIA_LOCAL=1); what an import wrote is read from the store in
;; process.
(import (chezscheme) (theourgia rpc)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-children code-field)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia code-suggest) read-import-symbols import-symbol-cuts)
        (only (theourgia languages) language-for-name))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (with-expected name expected (x)
       (want-1 name (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) got)
               x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/code-symbols-import-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (write! path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (write-bytes! path bv) (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (digest-of s) (bytevector->hex (sha256 (string->utf8 s))))
(define (bytes-of s) (bytevector-length (string->utf8 s)))
(define (slice s from to) (utf8->string (let ((b (string->utf8 s))) (let ((out (make-bytevector (- to from)))) (bytevector-copy! b from out 0 (- to from)) out))))
(define (offset-of text needle)
  (let ((b (string->utf8 text)) (n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b)) #f)
            ((let check ((k 0)) (or (= k (bytevector-length n))
                                    (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))
(define (clause a key)
  (let ((p (and (pair? a) (list? a) (assq key (filter pair? (cdr a)))))) (and p (cadr p))))
(define (text-lines t)
  (let loop ((p (open-input-string t)) (acc '()))
    (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc))))))
(define (read-all t)
  (guard (e (#t 'UNREADABLE))
    (let loop ((p (open-input-string t)) (acc '()))
      (let ((d (read p))) (if (eof-object? d) (reverse acc) (loop p (cons d acc)))))))

;; ---- the symbols file ------------------------------------------------------------
(define (lines . ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))
(define (header-of path text language)
  (format "~s" (list 'symbols (list 'path path) (list 'digest (digest-of text))
                     (list 'source (list 'vscode "1.138.0" language)) '(top-level #t))))
(define (section path text language . symbols) (apply lines (header-of path text language) symbols))
;; split-suggest's file: one file's header, no path
(define (one-section text language . symbols)
  (apply lines (format "~s" (list 'symbols (list 'digest (digest-of text))
                                  (list 'source (list 'vscode "1.138.0" language)) '(top-level #t)))
         symbols))
(define (sym start end kind name) (format "~s" (list 'symbol start end kind name)))

;; ---- a store, a directory, and the route ------------------------------------------
(define cases 0)
;; -> (store dir symbols-path dir-of-case)
(define (case! files symbols-text)
  (set! cases (+ cases 1))
  (let* ((d (string-append root "/case-" (number->string cases)))
         (store (string-append d "/store")) (src (string-append d "/src")) (symbols (string-append d "/symbols.sexp")))
    (sh "mkdir -p " (quoted src))
    (rpc-dispatch store '(init) "test")
    (for-each (lambda (f) (if (bytevector? (cdr f))
                              (write-bytes! (string-append src "/" (car f)) (cdr f))
                              (write! (string-append src "/" (car f)) (cdr f))))
              files)
    (when symbols-text (write! symbols symbols-text))
    (list store src symbols d)))
(define outs 0)
;; -> (rc stdout), core.sc on the local route; --wire is the caller's to add
(define (run store . args)
  (set! outs (+ outs 1))
  (let* ((out (string-append root "/out-" (number->string outs)))
         (rc (sh "THEOURGIA_LOCAL=1 scheme --script ../core.sc "
                 (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                 "--store " (quoted store) " > " (quoted out) " 2>/dev/null < /dev/null")))
    (list rc (file-text out))))
(define (wire store . args)
  (let ((r (apply run store (append args (list "--wire")))))
    (let ((ds (read-all (cadr r)))) (if (and (list? ds) (= 1 (length ds))) (car ds) (list 'NOT-ONE-ANSWER r)))))
(define (import-wire c) (wire (car c) "import-code" (cadr c) "--symbols" (caddr c)))
(define (import-human c) (run (car c) "import-code" (cadr c) "--symbols" (caddr c)))

;; WHAT A STORE HOLDS: the children's src of the file at `path`, in order.
(define (blocks-of store path)
  (let* ((s (open-and-reduce store))
         (f (find (lambda (f) (equal? (code-field s f 'path) path)) (code-files s))))
    (and f (map (lambda (c) (utf8->string (code-field s c 'src))) (code-children s f)))))
(define (field-of store path key)
  (let* ((s (open-and-reduce store))
         (f (find (lambda (f) (equal? (code-field s f 'path) path)) (code-files s))))
    (and f (map (lambda (c) (let ((v (code-field s c key))) (if (bytevector? v) (utf8->string v) v)))
                (code-children s f)))))
;; -> the offsets the blocks start at, 0 first, and the file's size last
(define (cuts-of blocks)
  (let loop ((bs blocks) (at 0) (acc '(0)))
    (if (null? bs) (reverse acc)
        (let ((n (+ at (bytes-of (car bs))))) (loop (cdr bs) n (cons n acc))))))

;; ---- the texts ---------------------------------------------------------------------
(define two "function alpha() {\n  return 1;\n}\nfunction beta() {\n  return 2;\n}\n")
(define two-symbols (list (sym 0 32 'function "alpha") (sym 33 64 'function "beta")))
(define open-text "function alpha() {\n  return 1;\n\nfunction beta() {\n  return 2;\n}\n")
(define review (string-append "// @block new pad 0\n" (slice two 0 33) "// @block new pad 0\n" (slice two 33 65)))
(want "SI-0 setup: TWO's offsets (alpha's body line, beta, the length) and OPEN's length"
      (list (offset-of two "  return 1") (offset-of two "function beta") (bytes-of two) (bytes-of open-text))
      '(19 33 65 64))

;; ---- SI-1: the human form prints what the answer lists ------------------------------
;; EVERY FILE REFUSED: a.js's section carries the digest of other bytes, b.js
;; names a start at byte 1 (not a line start), missing.js is not there. Nothing
;; is written, so there is no item; the answer's one clause is symbols-refused.
(let* ((c (case! (list (cons "a.js" two) (cons "b.js" two))
                 (string-append (section "a.js" "other bytes" "javascript" (sym 0 32 'function "alpha"))
                                (section "b.js" two "javascript" (sym 1 32 'function "alpha"))
                                (section "missing.js" two "javascript" (sym 0 32 'function "alpha")))))
       (w (run (car c) "import-code" (cadr c) "--symbols" (caddr c) "--wire"))
       (h (import-human c))
       (refused (list (list "a.js" (list 'error 'symbols-stale (list 'digest-expected (digest-of "other bytes"))
                                         (list 'digest-found (digest-of two))))
                      '("b.js" (error symbols-not-a-line-start (at 1)))
                      '("missing.js" (error symbols-no-file)))))
  (want "SI-1 every file refused: the human form is not empty, rc 0, one line, the symbols-refused clause with the three refusals"
        (list (car h) (length (text-lines (cadr h))) (read-all (cadr h)))
        (list 0 1 (list (list 'symbols-refused refused))))
  (want "SI-1 PIN every file refused: the wire form, byte for byte"
        w
        (list 0 (string-append "(ok (items) (symbols-refused ((\"a.js\" (error symbols-stale (digest-expected \""
                               (digest-of "other bytes") "\") (digest-found \"" (digest-of two)
                               "\"))) (\"b.js\" (error symbols-not-a-line-start (at 1))) (\"missing.js\" (error symbols-no-file)))))\n"))))

;; A PARTIAL RUN: a.js imported, b.js refused (a start not at a line start),
;; o.js imported whole (its brace never closes), r.js a review copy that
;; follows its markers, and c.bin skipped (a NUL byte). The items are printed
;; and then the four clauses, one line each, in the answer's order.
(let* ((files (list (cons "a.js" two) (cons "b.js" two) (cons "o.js" open-text) (cons "r.js" review)
                    (cons "c.bin" (bytevector 1 0 2))))
       (symbols (string-append (apply section "a.js" two "javascript" two-symbols)
                               (section "b.js" two "javascript" (sym 1 32 'function "alpha"))
                               (section "o.js" open-text "javascript" (sym 0 30 'function "alpha") (sym 32 63 'function "beta"))
                               (section "r.js" review "javascript" (sym 0 (bytes-of review) 'module "whole"))))
       (c (case! files symbols))
       (k (case! files symbols))
       (p (case! files symbols))
       (h (import-human c))
       (w (import-wire k))
       (raw (cadr (run (car p) "import-code" (cadr p) "--symbols" (caddr p) "--wire")))
       (tags '(skipped symbols-ignored symbols-imported-whole symbols-refused))
       (hs (read-all (cadr h)))
       (tail (filter (lambda (d) (and (pair? d) (memq (car d) tags))) (if (list? hs) hs '()))))
  (want "SI-1 a partial run: the wire answer is ok and lists all four clauses (the setup the human rows read)"
        (and (pair? w) (list (car w) (map (lambda (t) (and (clause w t) #t)) tags)))
        '(ok (#t #t #t #t)))
  (want "SI-1 a partial run, human form: rc 0, the items first, then exactly the four clauses, equal to the wire's, in its order"
        (list (car h)
              (and (list? hs) (pair? hs) (eq? (car (car hs)) 'ok))
              (and (list? hs) (list-tail hs (- (length hs) (length tail))))
              tail)
        (list 0 #t (map (lambda (t) (list t (clause w t))) tags) (map (lambda (t) (list t (clause w t))) tags)))
  (want "SI-1 PIN a partial run, the wire form after its items, byte for byte"
        (let ((i (offset-of raw " (skipped ("))) (and i (substring raw i (string-length raw))))
        (string-append " (skipped (\"c.bin\")) (symbols-ignored (\"r.js\"))"
                       " (symbols-imported-whole ((\"o.js\" (error symbols-unscanned (reason unbalanced) (at 64)))))"
                       " (symbols-refused ((\"b.js\" (error symbols-not-a-line-start (at 1))))))\n")))

;; ---- SI-2: one supplier of cuts --------------------------------------------------------
;; A header comment before an import, a JSDoc above each of two functions,
;; and one above a const. split-suggest --symbols and import-code --symbols
;; are given the same symbols; every top-level statement after the import
;; is a symbol, so the two cut alike.
(define ts (string-append "// header\nimport { a } from './a';\n\n"
                          "/** Doc of f. */\nexport function f() {\n  return 1;\n}\n\n"
                          "/**\n * Doc of g.\n */\nexport function g() {\n  return 2;\n}\n\n"
                          "/** Doc of K. */\nexport const K = 3;\n"))
(define ts-symbols
  (list (sym (offset-of ts "export function f") (+ (offset-of ts "return 1;\n}") 11) 'function "f")
        (sym (offset-of ts "export function g") (+ (offset-of ts "return 2;\n}") 11) 'function "g")
        (sym (offset-of ts "export const K") (+ (offset-of ts "= 3;") 4) 'constant "K")))
(want "SI-2 setup: TS's offsets (f's JSDoc, f, g's JSDoc, g, K's JSDoc, K, the length)"
      (list (offset-of ts "/** Doc of f") (offset-of ts "export function f") (offset-of ts "/**\n * Doc of g")
            (offset-of ts "export function g") (offset-of ts "/** Doc of K") (offset-of ts "export const K") (bytes-of ts))
      '(36 53 90 111 148 165 185))
(let* ((c (case! (list (cons "a.ts" ts)) (apply section "a.ts" ts "typescript" ts-symbols)))
       (one (string-append (cadddr c) "/one.sexp"))
       (written (write! one (apply one-section ts "typescript" ts-symbols)))
       (split (wire (car c) "split-suggest" (string-append (cadr c) "/a.ts") "--output" (string-append (cadddr c) "/a.review.ts")
                    "--symbols" one))
       (a (import-wire c))
       (blocks (blocks-of (car c) "a.ts")))
  (want "SI-2 split-suggest --symbols cuts at 0, f's JSDoc, g's JSDoc and K's JSDoc"
        (clause split 'boundaries)
        '(0 36 90 148))
  (want "SI-2 import-code --symbols cuts where split-suggest does, and the blocks run together to the file"
        (list (car a) (clause a 'symbols-refused) (and blocks (reverse (cdr (reverse (cuts-of blocks)))))
              (and blocks (apply string-append blocks)))
        (list 'ok #f (clause split 'boundaries) ts))
  (want "SI-2 no block is comment only: f's and g's blocks begin with their JSDoc, and their derived doc carries it"
        (list (field-of (car c) "a.ts" 'name) (field-of (car c) "a.ts" 'doc))
        (list '(#f "f" "g" #f) (list "// header\n" "/** Doc of f. */\n" "/**\n * Doc of g.\n */\n" "/** Doc of K. */\n")))
  (want "SI-2 the editor's title of the const under its JSDoc is still stored; none where the name is derived"
        (field-of (car c) "a.ts" 'title)
        '(#f #f #f "K")))

;; ---- SI-3: an end is never a refusal ---------------------------------------------------
;; ITER'S SHAPE: the editor's range for the binding name, its start taken
;; to its line start, ends inside the statement, whose type literal runs on
;; to `};`. The end moves to f's line, the next block's start, so it is no
;; cut: two blocks, the first the whole statement.
(define req (string-append "const { apiClient } = require('@/api/client') as {\n  get: () => void;\n};\n"
                           "export function f() {}\n"))
(define req-symbols
  (list (sym 0 (+ (offset-of req "apiClient") 9) 'variable "apiClient")
        (sym (offset-of req "export function f") (- (bytes-of req) 1) 'function "f")))
(define req-gap (string-append "const { apiClient } = require('@/api/client') as {\n  get: () => void;\n};\n"
                               "setup();\nexport function f() {}\n"))
(define req-gap-symbols
  (list (sym 0 (+ (offset-of req-gap "apiClient") 9) 'variable "apiClient")
        (sym (offset-of req-gap "export function f") (- (bytes-of req-gap) 1) 'function "f")))
(want "SI-3 setup: REQ's offsets (apiClient's start and end, the line after it, f, the length) and REQ-GAP's (setup, f)"
      (list (offset-of req "apiClient") (+ (offset-of req "apiClient") 9) (+ 1 (offset-of req "\n"))
            (offset-of req "export function f") (bytes-of req)
            (offset-of req-gap "setup();") (offset-of req-gap "export function f"))
      '(8 17 51 73 96 73 82))
(let* ((c (case! (list (cons "t.ts" req)) (apply section "t.ts" req "typescript" req-symbols)))
       (a (import-wire c)))
  (want "SI-3 iter's shape: imported, nothing refused, two blocks, the first holding the whole const statement"
        (list (car a) (clause a 'symbols-refused) (blocks-of (car c) "t.ts"))
        (list 'ok #f (list (slice req 0 73) (slice req 73 96)))))
(let* ((c (case! (list (cons "t.ts" req) (cons "u.ts" req-gap))
                 (string-append (apply section "t.ts" req "typescript" req-symbols)
                                (apply section "u.ts" req-gap "typescript" req-gap-symbols))))
       (a (import-wire c)))
  (want "SI-3b an end whose move reaches the next symbol's start makes no gap block: t.ts's two blocks are adjacent"
        (list (car a) (length (or (blocks-of (car c) "t.ts") '())))
        '(ok 2))
  (want "SI-3b an end whose move stops at code before the next start cuts there: u.ts's gap block holds setup();"
        (blocks-of (car c) "u.ts")
        (list (slice req-gap 0 73) "setup();\n" (slice req-gap 82 (bytes-of req-gap)))))
;; A START INSIDE A BODY IS STILL REFUSED, and the refusal names the symbol.
(let* ((c (case! (list (cons "a.js" two))
                 (section "a.js" two "javascript" (sym 19 30 'variable "inner") (sym 33 64 'function "beta"))))
       (a (import-wire c)))
  (want "SI-3c a start inside alpha's body: a.js refused symbols-not-top-level at 19, the refusal naming inner, nothing written"
        (list (car a) (clause a 'symbols-refused) (blocks-of (car c) "a.js"))
        (list 'ok '(("a.js" (error symbols-not-top-level (at 19) (symbol "inner")))) #f)))
;; AN END INSIDE A STRING THAT SPANS LINES moves past the string: the
;; template literal's second line is inside it, run(); is where code begins.
(define tpl "const s = `a\nb`;\nrun();\nfunction f() {\n}\n")
(want "SI-3d setup: TPL's offsets (the template's second line, run, f, the length)"
      (list (+ 1 (offset-of tpl "\n")) (offset-of tpl "run();") (offset-of tpl "function f") (bytes-of tpl))
      '(13 17 24 41))
(let* ((c (case! (list (cons "s.js" tpl))
                 (section "s.js" tpl "javascript" (sym 0 12 'variable "s") (sym 24 39 'function "f"))))
       (a (import-wire c)))
  (want "SI-3d an end inside a template literal spanning lines moves past it: three blocks, the gap run();"
        (list (car a) (clause a 'symbols-refused) (blocks-of (car c) "s.js"))
        (list 'ok #f (list (slice tpl 0 17) "run();\n" (slice tpl 24 41)))))

;; ---- SI-4: the other two answers whose skipped clause is printed --------------------
;; import --datum lists the file it does not read in skipped, and uses counts
;; the blocks it could not read; without --wire each prints that clause last,
;; one line, equal to the wire's.
(let* ((files (list (cons "a.sc" "(define x 1)\n(define y x)\n") (cons "n.xyz" "x y\n")))
       (c (case! files #f))
       (k (case! files #f))
       (h (run (car c) "import-code" (cadr c) "--datum"))
       (w (wire (car k) "import-code" (cadr k) "--datum"))
       (hs (read-all (cadr h))))
  (want "SI-4 import --datum: the wire's skipped clause lists n.xyz, and the human form ends with it, rc 0"
        (list (car h) (clause w 'skipped) (and (pair? hs) (list? hs) (list-tail hs (- (length hs) 1))))
        (list 0 '("n.xyz") (list (list 'skipped '("n.xyz"))))))
(let* ((files (list (cons "a.sc" "(define x 1)\n(define y x)\n") (cons "n.xyz" "x y\n")))
       (c (case! files #f)))
  (run (car c) "import-code" (cadr c))
  (let* ((w (wire (car c) "uses" "x"))
         (h (run (car c) "uses" "x"))
         (hs (read-all (cadr h)))
         (skipped (and (pair? w) (assq 'skipped (filter pair? (cdr w))))))
    (want "SI-4 uses: the wire's answer counts the block with no language, and the human form ends with that clause, rc 0"
          (list (car h) skipped (and (pair? hs) (list? hs) (list-tail hs (- (length hs) 1))))
          (list 0 '(skipped (no-language 1)) (list '(skipped (no-language 1)))))))

;; ---- SI-5: the cost of the cuts is linear in the file --------------------------------
;; N and 4N JSDoc'd functions, each a symbol: the cuts of the 4N file cost at
;; most six times the N file's. THE BOUND IS A GUARD, NOT A MEASUREMENT: a
;; linear cut reads 4 and one that searches the starts or the code lines from
;; the head for each symbol reads 16, so 6 leaves half again for noise and
;; still tells them apart. The median of three alternating pairs decides it,
;; so one stalled window cannot. Each pair's cuts are checked too: every
;; function one block, the comment above it included.
(define (jsdoc-file n)
  (let loop ((k 0) (at 0) (parts '()) (syms '()))
    (if (= k n)
        (list (apply string-append (reverse parts)) (reverse syms))
        (let* ((head (format "// f~a\n" k))
               (part (format "~afunction f~a() {\n  return ~a;\n}\n\n" head k k))
               (start (+ at (bytes-of head))))
          (loop (+ k 1) (+ at (bytes-of part)) (cons part parts)
                (cons (sym start (+ at (bytes-of part) -2) 'function (format "f~a" k)) syms))))))
(let* ((n 2000)
       (entry (language-for-name "javascript"))
       (prepared (lambda (k)
                   (let* ((f (jsdoc-file k)) (text (car f))
                          (path (string-append root "/cost-" (number->string k) ".sexp")))
                     (write! path (apply section "a.js" text "javascript" (cadr f)))
                     (list (string->utf8 text) (car (read-import-symbols path))))))
       (f1 (prepared n))
       (f4 (prepared (* 4 n)))
       (timed (lambda (f)
                (let* ((t0 (real-time)) (r (import-symbol-cuts (cadr f) (car f) entry)) (t1 (real-time)))
                  (list (length (car r)) (- t1 t0)))))
       (pairs (let loop ((k 0) (out '()))
                (if (= k 3) (reverse out) (let* ((r1 (timed f1)) (r4 (timed f4))) (loop (+ k 1) (cons (cons r1 r4) out))))))
       (median (lambda (xs) (list-ref (list-sort < xs) 1)))
       (m1 (median (map (lambda (p) (cadr (car p))) pairs)))
       (m4 (median (map (lambda (p) (cadr (cdr p))) pairs))))
  (for-each (lambda (p) (printf "  SI-5 times: N ~a ms, 4N ~a ms\n" (cadr (car p)) (cadr (cdr p)))) pairs)
  (printf "  SI-5 medians: N ~a ms, 4N ~a ms\n" m1 m4)
  (want "SI-5 N and 4N JSDoc'd functions: every pair one block a function (N+1 and 4N+1 cuts), the 4N cuts' median at most 6 times the N cuts'"
        (list (map (lambda (p) (list (car (car p)) (car (cdr p)))) pairs) (<= m4 (* 6 (max 1 m1))))
        (list (let ((one (list (+ n 1) (+ (* 4 n) 1)))) (list one one one)) #t)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ncode-symbols-import complete\n" bad rows)
(exit (if (= bad 0) 0 1))
