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

;; import-code --symbols: a first import of a file an editor's symbols file
;; names is split at those symbols into the blocks a marked import of the
;; same cuts makes; a file that carries markers follows them; a file whose
;; cuts the scanner does not see at the top level is not imported and is
;; listed with its refusal. Every expected src below is a slice of the row's
;; own text at numbers a setup row pins against that text.
;;
;; Imports run through core.sc, on the local route (THEOURGIA_LOCAL=1) and
;; through a daemon; what they wrote is read from the store in process,
;; after the daemon has stopped. On the tree before --symbols every row
;; below is red: the command line does not take the option.
(import (chezscheme) (theourgia rpc)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut)
        (only (theourgia code-project) code-files code-children code-field)
        (only (theourgia digest) sha256 bytevector->hex))

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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/import-symbols-" (number->string (get-process-id))))
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
;; NEVER: A ROW'S SETUP CANNOT END THE FILE. What a setup reads from a
;; product answer or from the store is read through `setup`, which turns a
;; raise into a marker the row's first want reads as red; on a tree whose
;; import-code does not take --symbols the rows after it still run and say
;; their red.
(define-syntax setup
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'SETUP-RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x))))
             e))))

;; ---- the symbols file ------------------------------------------------------------
(define (lines . ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))
(define (section path text . symbols)
  (apply lines
         (format "~s" (list 'symbols (list 'path path) (list 'digest (digest-of text))
                            '(source (vscode "1.138.0" "javascript")) '(top-level #t)))
         symbols))
(define (sym start end kind name) (format "~s" (list 'symbol start end kind name)))

;; ---- a store, a directory, and the routes ------------------------------------------
(define cases 0)
;; -> (store dir symbols-path): a fresh store, a directory holding `files`
;; ((name . text) ...), and the path the symbols file is written to.
(define (case! files symbols-text)
  (set! cases (+ cases 1))
  (let* ((d (string-append root "/case-" (number->string cases)))
         (store (string-append d "/store")) (src (string-append d "/src")) (symbols (string-append d "/symbols.sexp")))
    (sh "mkdir -p " (quoted src))
    (rpc-dispatch store '(init) "test")
    (for-each (lambda (f) (write! (string-append src "/" (car f)) (cdr f))) files)
    (when symbols-text (write! symbols symbols-text))
    (list store src symbols)))
(define outs 0)
(define (answer-of out)
  (guard (e (#t 'UNREADABLE))
    (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d))))
;; THE LOCAL ROUTE: core.sc in process, the answer in wire form.
(define (local store . args)
  (set! outs (+ outs 1))
  (let ((out (string-append root "/out-" (number->string outs))))
    (sh "THEOURGIA_LOCAL=1 scheme --script ../core.sc "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire > " (quoted out) " 2>/dev/null < /dev/null")
    (answer-of out)))
(define socket-dir (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket (string-append socket-dir "/is-" (number->string (get-process-id)) ".sock"))
(define daemon-log (string-append root "/daemon.log"))
(define (sleep-ms n) (sleep (make-time 'time-duration (* n 1000000) 0)))
(define (start-daemon! store)
  (sh "THEOURGIA_TRACE=1 scheme --script ../theourgiad.sc serve " (quoted store) " --socket " (quoted socket)
      " > " (quoted daemon-log) " 2>&1 < /dev/null &")
  (let wait ((k 0))
    (cond ((file-exists? socket) 'up)
          ((> k 200) 'never-came-up)
          (else (sleep-ms 50) (wait (+ k 1))))))
(define (store-daemons store)
  (let ((out (string-append root "/daemons.txt")))
    (sh "pgrep -f " (quoted (string-append "theourgiad.s[c] serve " store " ")) " > " (quoted out) " 2>/dev/null")
    (length (filter (lambda (l) (> (string-length l) 0))
                    (let loop ((p (open-input-string (file-text out))) (acc '()))
                      (let ((l (get-line p))) (if (eof-object? l) acc (loop p (cons l acc)))))))))
(define (stop-daemon! store)
  (sh "pkill -f " (quoted (string-append "theourgiad.s[c] serve " store " ")) " 2>/dev/null")
  (let wait ((k 0)) (when (and (> (store-daemons store) 0) (< k 100)) (sleep-ms 50) (wait (+ k 1)))))
;; THE DAEMON ROUTE: core.sc forwards to the daemon on `socket`.
(define (through-daemon store . args)
  (set! outs (+ outs 1))
  (let ((out (string-append root "/out-" (number->string outs))))
    (sh "scheme --script ../core.sc "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --socket " (quoted socket) " --wire > " (quoted out) " 2>/dev/null < /dev/null")
    (answer-of out)))
(define (served-imports)
  (let ((t (file-text daemon-log)) (needle "(trace daemon-dispatch import-code"))
    (let loop ((i 0) (n 0))
      (cond ((> (+ i (string-length needle)) (string-length t)) n)
            ((string=? (substring t i (+ i (string-length needle))) needle) (loop (+ i 1) (+ n 1)))
            (else (loop (+ i 1) n))))))

;; WHAT A STORE HOLDS: every live file block by path, with its children's
;; src in order, sorted by path.
(define (files-of store)
  (let ((s (open-and-reduce store)))
    (list-sort (lambda (a b) (string<? (car a) (car b)))
               (map (lambda (f) (cons (code-field s f 'path)
                                      (map (lambda (c) (utf8->string (code-field s c 'src))) (code-children s f))))
                    (code-files s)))))
;; -> each block of the file at `path`: (kind mode name), the file first.
(define (shapes-of store path)
  (let* ((s (open-and-reduce store))
         (f (find (lambda (f) (equal? (code-field s f 'path) path)) (code-files s))))
    (and f (map (lambda (id) (list (code-field s id 'kind) (code-field s id 'mode) (code-field s id 'name)))
                (cons f (code-children s f))))))
(define (children-of store path)
  (let* ((s (open-and-reduce store))
         (f (find (lambda (f) (equal? (code-field s f 'path) path)) (code-files s))))
    (and f (code-children s f))))

;; ---- the texts ---------------------------------------------------------------------
;; TWO: two functions, no blank line between. The editor's ranges end at each
;; closing brace; the end is taken to the next line, so the cut is 33.
(define two "function alpha() {\n  return 1;\n}\nfunction beta() {\n  return 2;\n}\n")
(want "IS0 setup: TWO's offsets (alpha's brace, beta, alpha's body line, beta's brace, the length)"
      (list (offset-of two "}\nfunction beta") (offset-of two "function beta") (offset-of two "  return 1")
            (offset-of two "}\n" ) (bytes-of two))
      '(31 33 19 31 65))
(define two-symbols (list (sym 0 32 'function "alpha") (sym 33 64 'function "beta")))
(define two-blocks (list (slice two 0 33) (slice two 33 65)))
;; GAPS: a comment before, a blank line between, a comment after. The blocks
;; are the comment, alpha, the blank line, beta and the last comment.
(define gaps "// header\nfunction alpha() {\n  return 1;\n}\n\nfunction beta() {\n  return 2;\n}\n// trailer\n")
(want "IS0 setup: GAPS's offsets (alpha, its brace, beta, its brace, the trailer, the length)"
      (list (offset-of gaps "function alpha") (offset-of gaps "}\n\n") (offset-of gaps "function beta")
            (offset-of gaps "}\n// trailer") (offset-of gaps "// trailer") (bytes-of gaps))
      '(10 41 44 74 76 87))
;; OPEN: alpha's brace never closes, so beta's line is inside it.
(define open-text "function alpha() {\n  return 1;\n\nfunction beta() {\n  return 2;\n}\n")
(want "IS0 setup: OPEN's offsets (beta, the length)"
      (list (offset-of open-text "function beta") (bytes-of open-text))
      '(32 64))

;; ---- IS1: a two-function file splits into its file and two code blocks -------------
(let* ((c (case! (list (cons "a.js" two)) (apply section "a.js" two two-symbols)))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS1 local: ok, no symbols clause, and a.js is two code blocks whose src are TWO cut at 33"
        (list (car a) (clause a 'symbols-ignored) (clause a 'symbols-refused) (files-of (car c)))
        (list 'ok #f #f (list (cons "a.js" two-blocks))))
  (want "IS1 local: the blocks run together to the file's bytes"
        (apply string-append (cdr (assoc "a.js" (files-of (car c)))))
        two)
  (want "IS1 local: a text file block and two text code blocks, each named from its source: alpha, beta"
        (shapes-of (car c) "a.js")
        '((file text #f) (code text "alpha") (code text "beta"))))

;; ---- IS2: what no symbol covers is a block of its own ------------------------------
(let* ((c (case! (list (cons "g.js" gaps))
                 (section "g.js" gaps (sym 10 42 'function "alpha") (sym 44 75 'function "beta"))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS2 the comment before, the blank line between and the comment after are blocks of their own, cut at 10, 43, 44 and 76"
        (list (car a) (files-of (car c)))
        (list 'ok (list (list "g.js" (slice gaps 0 10) (slice gaps 10 43) (slice gaps 43 44) (slice gaps 44 76) (slice gaps 76 87)))))
  (want "IS2 the five blocks run together to the file's bytes"
        (apply string-append (cdr (assoc "g.js" (files-of (car c)))))
        gaps))

;; ---- IS3: a cut inside a body refuses that file, and the rest goes on --------------
;; a.js names its body line as a symbol: the cut at 19 is inside alpha's
;; braces. b.js, the same text, is cut as IS1's.
(let* ((c (case! (list (cons "a.js" two) (cons "b.js" two))
                 (string-append (section "a.js" two (sym 19 30 'variable "inner") (sym 33 64 'function "beta"))
                                (apply section "b.js" two two-symbols))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS3 a.js is refused symbols-not-top-level at 19 and listed; no block holds it; b.js is imported cut at 33"
        (list (car a) (clause a 'symbols-refused) (files-of (car c)))
        (list 'ok '(("a.js" (error symbols-not-top-level (at 19)))) (list (cons "b.js" two-blocks)))))

;; ---- IS3b: a section whose digest is not the file's refuses that file ---------------
;; a.js's header carries the digest of other bytes; the section's own checks
;; refuse it before any cut is made. b.js is cut as IS1's.
(let* ((c (case! (list (cons "a.js" two) (cons "b.js" two))
                 (string-append (section "a.js" "other bytes" (sym 0 32 'function "alpha") (sym 33 64 'function "beta"))
                                (apply section "b.js" two two-symbols))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS3b a.js is refused symbols-stale with both digests and not imported; b.js is imported cut at 33"
        (list (car a) (clause a 'symbols-refused) (files-of (car c)))
        (list 'ok
              (list (list "a.js" (list 'error 'symbols-stale (list 'digest-expected (digest-of "other bytes"))
                                       (list 'digest-found (digest-of two)))))
              (list (cons "b.js" two-blocks)))))

;; ---- IS4: a file the scanner cannot follow is refused with the byte ----------------
(let* ((c (case! (list (cons "o.js" open-text) (cons "b.js" two))
                 (string-append (section "o.js" open-text (sym 0 30 'function "alpha") (sym 32 63 'function "beta"))
                                (apply section "b.js" two two-symbols))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS4 an unclosed brace: o.js is refused symbols-unscanned, unbalanced at its end (64), and not imported; b.js is"
        (list (car a) (clause a 'symbols-refused) (map car (files-of (car c))))
        (list 'ok '(("o.js" (error symbols-unscanned (reason unbalanced) (at 64)))) '("b.js"))))

;; ---- IS5: a file the symbols file does not name imports as before ------------------
(let* ((c (case! (list (cons "a.js" two) (cons "w.js" two)) (apply section "a.js" two two-symbols)))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS5 a.js is split; w.js, not named, is one block holding the whole file"
        (list (car a) (files-of (car c)))
        (list 'ok (list (cons "a.js" two-blocks) (list "w.js" two)))))

;; ---- IS6: a re-import follows the markers and ignores the symbols ------------------
;; IS1's import, exported with its markers and edited in beta's body, then
;; imported again with a section that would make one block of the whole
;; file. The file block exists, so the markers decide: the same two
;; children, beta's src edited, and a.js listed as symbols-ignored.
(let* ((c (case! (list (cons "a.js" two)) (apply section "a.js" two two-symbols)))
       (first (local (car c) "import-code" (cadr c) "--symbols" (caddr c)))
       (before (setup (children-of (car c) "a.js")))
       (out (string-append root "/case-" (number->string cases) "/out"))
       (exported (begin (sh "mkdir -p " (quoted out)) (local (car c) "export-code" out)))
       (path (string-append out "/a.js"))
       (at (setup (offset-of (file-text path) "return 2;")))
       ;; NO EXPORTED a.js, NO EDIT: with nothing to find, the file stays
       ;; as it is and the first want below reads `at` as red.
       (edited (let ((t (file-text path)))
                 (if (integer? at)
                     (string-append (substring t 0 at) "return 22;" (substring t (+ at 9) (string-length t)))
                     t)))
       (symbols (string-append root "/case-" (number->string cases) "/again.sexp")))
  (write! path edited)
  (write! symbols (section "a.js" edited (sym 0 (bytes-of edited) 'module "whole")))
  (let ((a (local (car c) "import-code" out "--symbols" symbols)))
    (want "IS6 the re-import answers ok and lists a.js as symbols-ignored"
          (list (car first) (car exported) (integer? at) (car a) (clause a 'symbols-ignored))
          '(ok ok #t ok ("a.js")))
    (want "IS6 the same two children, by id, and beta's src is the edited one"
          (list (equal? (children-of (car c) "a.js") before) (files-of (car c)))
          (list #t (list (list "a.js" (car two-blocks) "function beta() {\n  return 22;\n}\n"))))))

;; ---- IS7: a symbols file that cannot be read as sections refuses the request -------
(define (refused-whole label text expected)
  (let* ((c (case! (list (cons "a.js" two)) text))
         (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
    (want (string-append "IS7 " label ", and nothing is written")
          (list a (files-of (car c)))
          (list expected '()))))
(refused-whole "a symbol line before any header is symbols-malformed at line 1"
               (lines (sym 0 32 'function "alpha"))
               '(error symbols-malformed (line 1)))
(refused-whole "a second section for one path is symbols-malformed at its header's line, 4"
               (string-append (apply section "a.js" two two-symbols) (section "a.js" two (sym 0 32 'function "alpha")))
               '(error symbols-malformed (line 4)))
(refused-whole "a path that is not a plain relative path is symbols-malformed at its line"
               (section "../a.js" two (sym 0 32 'function "alpha"))
               '(error symbols-malformed (line 1)))
(refused-whole "a file with no section is symbols-empty"
               ""
               '(error symbols-empty))
(let* ((c (case! (list (cons "a.scm" "(define (f) 1)\n")) (section "a.scm" "(define (f) 1)\n" (sym 0 14 'function "f"))))
       (a (local (car c) "import-code" (cadr c) "--datum" "--symbols" (caddr c))))
  (want "IS7 --symbols with --datum is refused incompatible-import-options, and nothing is written"
        (list a (files-of (car c)))
        (list '(error bad-request incompatible-import-options) '())))

;; ---- IS8: a section that names no text file is listed, and the rest goes on --------
(let* ((c (case! (list (cons "a.js" two))
                 (string-append (apply section "a.js" two two-symbols) (section "missing.js" two (sym 0 32 'function "alpha")))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS8 missing.js is listed as symbols-refused, symbols-no-file; a.js is split"
        (list (car a) (clause a 'symbols-refused) (files-of (car c)))
        (list 'ok '(("missing.js" (error symbols-no-file))) (list (cons "a.js" two-blocks)))))

;; ---- IS10: a cut inside a body that only the normalised end makes ---------------
;; Both starts are at the top level; alpha's end, one line short, at the end
;; of `function alpha() {`, is taken to the start of the line after it, 19,
;; which is inside alpha's braces. A scanner asked only about the starts
;; would pass this file.
(let* ((c (case! (list (cons "a.js" two) (cons "b.js" two))
                 (string-append (section "a.js" two (sym 0 18 'function "alpha") (sym 33 64 'function "beta"))
                                (apply section "b.js" two two-symbols))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS10 an end one line short puts the cut at 19, inside alpha's body: a.js is refused symbols-not-top-level at 19; b.js imports"
        (list (car a) (clause a 'symbols-refused) (map car (files-of (car c))))
        (list 'ok '(("a.js" (error symbols-not-top-level (at 19)))) '("b.js"))))

;; ---- IS11: what the scanner's state says is still inside -------------------------
;; PY-BLANK: f's end, one line short, is taken to the blank line inside its
;; body (19); the blank line has no indentation, and the line after it does.
;; ARROW: inc's end at the end of its first line is taken to the line of its
;; expression body (19), and the line before ends in =>. DEF: f's end at the
;; end of `def f():` is taken to its indented body (9).
(define py-blank "def f():\n    a = 1\n\n    return a\n\ndef g():\n    return 2\n")
(define arrow "const inc = (x) =>\n  x + 1;\nfunction g() {\n  return 2;\n}\n")
(define def-colon "def f():\n    return 1\n")
(want "IS0 setup: PY-BLANK's, ARROW's and DEF's offsets (the blank line in f, g, the expression body, g, the body, the lengths)"
      (list (+ 1 (offset-of py-blank "\n\n    return a")) (offset-of py-blank "def g") (bytes-of py-blank)
            (+ 1 (offset-of arrow "\n  x + 1")) (offset-of arrow "function g") (bytes-of arrow)
            (+ 1 (offset-of def-colon "\n")) (bytes-of def-colon))
      '(19 34 56 19 28 57 9 22))
(for-each
  (lambda (c)
    (let* ((name (car c)) (text (cadr c))
           (k (case! (list (cons name text) (cons "b.js" two))
                     (string-append (apply section name text (caddr c)) (apply section "b.js" two two-symbols))))
           (a (local (car k) "import-code" (cadr k) "--symbols" (caddr k))))
      (want (string-append "IS11 " (cadddr c) ": " name " is refused symbols-not-top-level at "
                           (number->string (list-ref c 4)) ", not imported; b.js imports")
            (list (car a) (clause a 'symbols-refused) (map car (files-of (car k))))
            (list 'ok (list (list name (list 'error 'symbols-not-top-level (list 'at (list-ref c 4))))) '("b.js")))))
  (list (list "f.py" py-blank (list (sym 0 18 'function "f") (sym 34 55 'function "g"))
              "a blank line inside a Python body, the next line indented" 19)
        (list "inc.js" arrow (list (sym 0 18 'variable "inc") (sym 28 56 'function "g"))
              "an arrow function's expression body on the next line, after =>" 19)
        (list "d.py" def-colon (list (sym 0 8 'function "f"))
              "the line after def f():, its indented body" 9)))

;; ---- IS12: boundaries the stricter test still takes --------------------------------
;; A blank line between two top-level Python definitions; a comment line
;; ending in `.` before a symbol; a line ending in a string that holds `:`.
(define py-gap "def f():\n    return 1\n\ndef g():\n    return 2\n")
(define comment-end "// The end.\nfunction f() {\n  return 1;\n}\n")
(define string-end "x = 'a:'\nfunction f() {\n  return 1;\n}\n")
(want "IS0 setup: PY-GAP's, COMMENT-END's and STRING-END's offsets (the blank line, g, f, x's line end, f, the lengths)"
      (list (+ 1 (offset-of py-gap "\n\ndef g")) (offset-of py-gap "def g") (bytes-of py-gap)
            (offset-of comment-end "function f") (bytes-of comment-end)
            (offset-of string-end "\nfunction") (offset-of string-end "function f") (bytes-of string-end))
      '(22 23 45 12 41 8 9 38))
(for-each
  (lambda (c)
    (let* ((name (car c)) (text (cadr c))
           (k (case! (list (cons name text)) (apply section name text (caddr c))))
           (a (local (car k) "import-code" (cadr k) "--symbols" (caddr k))))
      (want (string-append "IS12 CONTROL " (cadddr c) ": " name " imports, cut where the symbols say")
            (list (car a) (clause a 'symbols-refused) (files-of (car k)))
            (list 'ok #f (list (cons name (map (lambda (r) (slice text (car r) (cadr r))) (list-ref c 4))))))))
  (list (list "g.py" py-gap (list (sym 0 21 'function "f") (sym 23 44 'function "g"))
              "a blank line between two top-level definitions" '((0 22) (22 23) (23 45)))
        (list "c.js" comment-end (list (sym 12 40 'function "f"))
              "a comment line ending in . before a definition" '((0 12) (12 41)))
        (list "s.js" string-end (list (sym 0 8 'variable "x") (sym 9 37 'function "f"))
              "a line ending in a string that holds :" '((0 9) (9 38)))))

;; ---- IS13: a symbols file that does not read wins over a directory that is not there
;; The sections are read before the directory: with no directory, a malformed
;; or empty symbols file is still what is refused. CONTROL: a symbols file
;; that reads, with no directory, is refused for the directory.
(let* ((missing (string-append root "/no-such-directory"))
       (c (case! (list (cons "a.js" two)) (lines (sym 0 32 'function "alpha"))))
       (e (case! (list (cons "a.js" two)) ""))
       (v (case! (list (cons "a.js" two)) (apply section "a.js" two two-symbols))))
  (want "IS13 with no directory, a symbol line before any header is still symbols-malformed at line 1, and an empty file symbols-empty"
        (list (local (car c) "import-code" missing "--symbols" (caddr c))
              (local (car e) "import-code" missing "--symbols" (caddr e)))
        '((error symbols-malformed (line 1)) (error symbols-empty)))
  (want "IS13 CONTROL with no directory and a symbols file that reads, the refusal is the directory's"
        (local (car v) "import-code" missing "--symbols" (caddr v))
        '(error projection-invalid (reason not-a-directory))))

;; ---- IS14: a file with markers and no header follows its markers -------------------
;; split-suggest's review copy: a `// @block new` line before each block and
;; no file header. Its section would make one block of the whole file. It is
;; a first import by its markers: alpha and beta, without the marker lines,
;; and it is listed as symbols-ignored.
(define review (string-append "// @block new pad 0\n" (car two-blocks) "// @block new pad 0\n" (cadr two-blocks)))
(let* ((c (case! (list (cons "r.js" review)) (section "r.js" review (sym 0 (bytes-of review) 'module "whole"))))
       (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
  (want "IS14 a review copy's markers decide: two blocks without the marker lines, and r.js listed as symbols-ignored"
        (list (car a) (clause a 'symbols-ignored) (files-of (car c)))
        (list 'ok '("r.js") (list (cons "r.js" two-blocks)))))

;; ---- IS15: a captured request replays from its packet, not from the symbols file ---
;; NEVER: THE SYMBOLS FILE IS AN INPUT OF THE CAPTURE. A request with an
;; identity (--req and --cursor) is captured once, in the operation packet;
;; a retry of the same request replays the packet. The symbols file is read
;; inside the capture, so emptying it after the first try changes nothing:
;; the retry answers ok, writes nothing more, and the file is not read
;; (empty, it would be refused symbols-empty).
(let* ((c (case! (list (cons "a.js" two)) (apply section "a.js" two two-symbols)))
       ;; a request's cursor names a writer; a fresh store has none, so one
       ;; section is written first (files-of reads code files only)
       (seeded (setup (rpc-dispatch (car c) '(insert "--title" "seed") "test")))
       (cursor (let ((v (setup (let ((e (car (reduce-applied-cut (open-and-reduce (car c))))))
                                 (string-append (car e) ":" (number->string (cdr e)))))))
                 (if (string? v) v "no-cursor:0")))
       (first (local (car c) "import-code" (cadr c) "--symbols" (caddr c) "--req" "replay-once" "--cursor" cursor))
       (held (setup (list (files-of (car c)) (reduce-applied-cut (open-and-reduce (car c))))))
       (emptied (write! (caddr c) ""))
       (retry (local (car c) "import-code" (cadr c) "--symbols" (caddr c) "--req" "replay-once" "--cursor" cursor))
       (after (setup (list (files-of (car c)) (reduce-applied-cut (open-and-reduce (car c)))))))
  (want "IS15 the first try splits a.js at 33"
        (list (car first) (car held))
        (list 'ok (list (cons "a.js" two-blocks))))
  (want "IS15 with the symbols file emptied, the same request replays: ok, the same blocks, the cut unchanged"
        (list (car retry) (equal? after held))
        '(ok #t))
  (want "IS15 CONTROL a new request with the emptied symbols file is refused symbols-empty"
        (let ((a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
          (and (pair? a) (list (car a) (cadr a))))
        '(error symbols-empty)))

;; ---- IS16-IS24: the strict test, token by token -----------------------------------
;; One file each, named by its symbols. -> (answer refused files): the answer's
;; head, its symbols-refused clause, and the store's files.
(define (one-file name text . symbols)
  (let* ((c (case! (list (cons name text)) (apply section name text symbols)))
         (a (local (car c) "import-code" (cadr c) "--symbols" (caddr c))))
    (list (if (pair? a) (car a) a) (clause a 'symbols-refused) (setup (files-of (car c))))))
;; A TOKEN IS A WHOLE OPERATOR: "x++" ends a statement, "1." ends a number, and
;; the shell's "/" ends a word; none of them continues the line.
(want "IS16 a line ending in x++ does not continue: the cut after it is taken"
      (one-file "a.js" "let x = 0;\nx++\nfunction f() {\n}\n" (sym 0 10 'variable "x") (sym 15 32 'function "f"))
      (list 'ok #f (list (list "a.js" "let x = 0;\n" "x++\n" "function f() {\n}\n"))))
(want "IS17 a line ending in the number 1. does not continue: the cut after it is taken"
      (one-file "f.py" "x = 1.\ndef f():\n    pass\n" (sym 0 6 'variable "x") (sym 7 25 'function "f"))
      (list 'ok #f (list (list "f.py" "x = 1.\n" "def f():\n    pass\n"))))
(want "IS18 a shell line ending in cd / does not continue: the shell's tokens are |, && and || only"
      (one-file "s.sh" "cd /\nf() { :; }\n" (sym 0 4 'variable "cd") (sym 5 16 'function "f"))
      (list 'ok #f (list (list "s.sh" "cd /\n" "f() { :; }\n"))))
;; THE ESCAPED CHARACTER IS CODE: after "&&\x" the line ends in x, not in &&.
(want "IS19 a shell line ending in &&\\x ends in the escaped x, and does not continue"
      (one-file "e.sh" "true &&\\x\nf() { :; }\n" (sym 0 9 'variable "t") (sym 10 21 'function "f"))
      (list 'ok #f (list (list "e.sh" "true &&\\x\n" "f() { :; }\n"))))
(want "IS19 CONTROL an escaped pipe is a literal character: a line ending in \\| does not continue"
      (one-file "p.sh" "echo a\\|\nf() { :; }\n" (sym 0 8 'variable "e") (sym 9 20 'function "f"))
      (list 'ok #f (list (list "p.sh" "echo a\\|\n" "f() { :; }\n"))))
;; A LINE OF SPACES IS BLANK, judged by the next non-blank line.
(want "IS20 a line of spaces before top-level code is the top level: the cut at it is taken"
      (one-file "w.py" "def f():\n    return 1\n    \ndef g():\n    return 2\n" (sym 0 21 'function "f") (sym 27 48 'function "g"))
      (list 'ok #f (list (list "w.py" "def f():\n    return 1\n" "    \n" "def g():\n    return 2\n"))))
(want "IS20 CONTROL a line of spaces before indented code is inside: refused at it"
      (one-file "v.py" "def f():\n    a = 1\n    \n    return a\n" (sym 0 18 'function "f"))
      (list 'ok (list (list "v.py" '(error symbols-not-top-level (at 19)))) '()))
;; COMMENTS, PROSE AND ORDER.
(want "IS21 a token before an inline line comment continues: the code is read without its comment"
      (one-file "c.js" "const x = 1 + // note\n  2;\nfunction f() {\n}\n" (sym 0 21 'variable "x") (sym 27 44 'function "f"))
      (list 'ok (list (list "c.js" '(error symbols-not-top-level (at 22)))) '()))
(want "IS22 a cut inside a block comment opened after an operator is refused by the comment, before any token test"
      (one-file "k.js" "const y = 1 + /* note\n more */ 2;\nfunction f() {\n}\n" (sym 0 21 'variable "y") (sym 34 51 'function "f"))
      (list 'ok (list (list "k.js" '(error symbols-not-top-level (at 22)))) '()))
(want "IS23 Markdown is prose: a line ending in : before a heading does not continue"
      (one-file "m.md" "Example:\n# Next\ntext\n" (sym 0 8 'string "Example") (sym 9 21 'string "Next"))
      (list 'ok #f (list (list "m.md" "Example:\n" "# Next\ntext\n"))))
(want "IS24 two blank-line cuts inside a body: the refusal names the first in the file, 19"
      (one-file "o.py" "def f():\n    a = 1\n\n\n    return a\n" (sym 0 18 'function "f") (sym 20 33 'variable "rest"))
      (list 'ok (list (list "o.py" '(error symbols-not-top-level (at 19)))) '()))

;; ---- IS25-IS29: the ellipsis, the escape's literal, a comment's close, the tokens --
(want "IS25 a stub-style def ending in Python's ... does not continue: the next definition's cut is taken"
      (one-file "q.py" "def f(self) -> int: ...\ndef g():\n    pass\n" (sym 0 23 'function "f") (sym 24 42 'function "g"))
      (list 'ok #f (list (list "q.py" "def f(self) -> int: ...\n" "def g():\n    pass\n"))))
;; THE LAST CHARACTER IS LITERAL ONLY IF THE ESCAPE MADE IT SO: in `echo \\|`
;; the escape takes the second backslash and the pipe is real.
(want "IS26 a shell line ending in \\\\| ends in a real pipe and continues: the cut after it is refused"
      (one-file "r.sh" "echo \\\\|\nf() { :; }\n" (sym 0 8 'variable "e") (sym 9 20 'function "f"))
      (list 'ok (list (list "r.sh" '(error symbols-not-top-level (at 9)))) '()))
(want "IS26 a shell line ending in \\\\ ends in a literal backslash and does not continue: the cut after it is taken"
      (one-file "b.sh" "echo \\\\\nf() { :; }\n" (sym 0 7 'variable "e") (sym 8 19 'function "f"))
      (list 'ok #f (list (list "b.sh" "echo \\\\\n" "f() { :; }\n"))))
(want "IS27 a line ending at a block comment's close does not continue, whatever the code before it: the cut after it is taken"
      (one-file "z.js" "const z = 1 + /* note */\nfunction f() {\n}\n" (sym 0 24 'variable "z") (sym 25 42 'function "f"))
      (list 'ok #f (list (list "z.js" "const z = 1 + /* note */\n" "function f() {\n}\n"))))
;; THE TOKENS THEMSELVES: each continues, and the cut after it is refused.
(want "IS28 the shell's |, && and || continue"
      (list (one-file "l.sh" "ls |\nwc -l\nf() { :; }\n" (sym 0 4 'variable "ls") (sym 11 22 'function "f"))
            (one-file "t.sh" "true &&\nfalse\nf() { :; }\n" (sym 0 7 'variable "t") (sym 14 25 'function "f"))
            (one-file "o.sh" "false ||\ntrue\nf() { :; }\n" (sym 0 8 'variable "o") (sym 14 25 'function "f")))
      (list (list 'ok (list (list "l.sh" '(error symbols-not-top-level (at 5)))) '())
            (list 'ok (list (list "t.sh" '(error symbols-not-top-level (at 8)))) '())
            (list 'ok (list (list "o.sh" '(error symbols-not-top-level (at 9)))) '())))
(want "IS29 a binary - and a : continue"
      (list (one-file "m.js" "const m = 1 -\n  2;\nfunction f() {\n}\n" (sym 0 13 'variable "m") (sym 19 36 'function "f"))
            (one-file "n.js" "const t = c ? a :\n  b;\nfunction f() {\n}\n" (sym 0 17 'variable "t") (sym 23 40 'function "f")))
      (list (list 'ok (list (list "m.js" '(error symbols-not-top-level (at 14)))) '())
            (list 'ok (list (list "n.js" '(error symbols-not-top-level (at 18)))) '())))

;; IS30: RUST'S OPEN RANGE CONTINUES. Only "..." (Python's ellipsis) ends a line;
;; "0.." at a line's end is the range operator, and the cut after it is inside.
(want "IS30 a Rust line ending in the range 0.. continues: the cut after it is refused"
      (one-file "r.rs" "const R: Range = 0..\n10;\nfn f() {\n}\n" (sym 0 20 'constant "R") (sym 25 36 'function "f"))
      (list 'ok (list (list "r.rs" '(error symbols-not-top-level (at 21)))) '()))
;; IS31: WHAT THE ESCAPE MADE LITERAL IS ASKED OF ITS OWN LINE ONLY. The first
;; line ends in an escaped "|" at index 6, the second in a real "|" at the same
;; index: the first cut is taken, the second refused. (A multi-character escape
;; cannot be rowed here: the import runs in a child process, where a language
;; registered in this fixture does not exist, and every built-in escape is one
;; character.)
(want "IS31 an escaped | on one line does not make the next line's | at the same index a literal"
      (one-file "x.sh" "echo \\|\necho a|\nf() { :; }\n" (sym 0 7 'variable "e1") (sym 8 15 'variable "e2") (sym 16 27 'function "f"))
      (list 'ok (list (list "x.sh" '(error symbols-not-top-level (at 16)))) '()))

;; IS32: IN A PARENTHESISED LANGUAGE NO OPERATOR CONTINUES A LINE. "-", "+" and
;; "." are identifier characters in Scheme; a top-level datum ending in one is
;; complete, and a line continues only through an open bracket.
(want "IS32 a Scheme top-level datum ending in x- is complete: the cut after it is taken"
      (one-file "s.scm" "(define a 1)\nx-\n(define (f) 2)\n" (sym 0 12 'variable "a") (sym 16 30 'function "f"))
      (list 'ok #f (list (list "s.scm" "(define a 1)\n" "x-\n" "(define (f) 2)\n"))))

;; ---- IS9: the same through a daemon ------------------------------------------------
;; One store and one daemon: IS1's split, IS3's refusal next to a file that
;; splits, and IS7's malformed file, each forwarded. The store is read after
;; the daemon has stopped.
(let* ((c (case! (list (cons "a.js" two)) (apply section "a.js" two two-symbols)))
       (store (car c))
       (d2 (string-append root "/case-" (number->string cases) "/src2"))
       (s2 (string-append root "/case-" (number->string cases) "/symbols2.sexp"))
       (s3 (string-append root "/case-" (number->string cases) "/symbols3.sexp")))
  (sh "mkdir -p " (quoted d2))
  (write! (string-append d2 "/x.js") two)
  (write! (string-append d2 "/y.js") two)
  (write! s2 (string-append (section "x.js" two (sym 19 30 'variable "inner") (sym 33 64 'function "beta"))
                            (apply section "y.js" two two-symbols)))
  (write! s3 (lines (sym 0 32 'function "alpha")))
  (let* ((state (start-daemon! store))
         (served-before (served-imports))
         (a1 (through-daemon store "import-code" (cadr c) "--symbols" (caddr c)))
         (a2 (through-daemon store "import-code" d2 "--symbols" s2))
         (a3 (through-daemon store "import-code" d2 "--symbols" s3))
         (served-after (served-imports)))
    (stop-daemon! store)
    (want "IS9 through the daemon: the split answers ok, the refusal lists x.js, the malformed file is refused whole"
          (list state (car a1) (car a2) (clause a2 'symbols-refused) a3)
          (list 'up 'ok 'ok '(("x.js" (error symbols-not-top-level (at 19)))) '(error symbols-malformed (line 1))))
    (want "IS9 CONTROL: the daemon served the three imports -- none in its trace before, three after"
          (list served-before served-after)
          '(0 3))
    (want "IS9 what the daemon wrote: a.js and y.js each cut at 33, and no block for x.js"
          (files-of store)
          (list (cons "a.js" two-blocks) (cons "y.js" two-blocks)))
    (want "IS9 no daemon of the store outlives the route"
          (store-daemons store)
          0)))

(system (string-append "rm -rf '" root "'; rm -f '" socket "'"))
(printf "\n~a failures\nrows: ~a\nimport-symbols complete\n" bad rows)
(exit (if (= bad 0) 0 1))
