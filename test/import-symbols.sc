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
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))

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
       (before (children-of (car c) "a.js"))
       (out (string-append root "/case-" (number->string cases) "/out"))
       (exported (begin (sh "mkdir -p " (quoted out)) (local (car c) "export-code" out)))
       (path (string-append out "/a.js"))
       (edited (let* ((t (file-text path)) (at (offset-of t "return 2;")))
                 (string-append (substring t 0 at) "return 22;" (substring t (+ at 9) (string-length t)))))
       (symbols (string-append root "/case-" (number->string cases) "/again.sexp")))
  (write! path edited)
  (write! symbols (section "a.js" edited (sym 0 (bytes-of edited) 'module "whole")))
  (let ((a (local (car c) "import-code" out "--symbols" symbols)))
    (want "IS6 the re-import answers ok and lists a.js as symbols-ignored"
          (list (car first) (car exported) (car a) (clause a 'symbols-ignored))
          '(ok ok ok ("a.js")))
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
