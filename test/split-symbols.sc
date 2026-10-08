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

;; split-suggest --symbols: the cuts come from an editor's list of top-level
;; symbols instead of the language's definition patterns, and the scanner's
;; own rules (comment attachment, the protected prefix, what is top level)
;; still apply. Every expected boundary below is a number worked out from the
;; row's own text, not read from the scanner; a setup row pins those numbers
;; against the text so a change to the text cannot move them silently.
(import (chezscheme) (theourgia rpc) (theourgia ffi)
        (only (theourgia languages) register-language!)
        (only (theourgia digest) sha256 bytevector->hex))

(define bad 0)
(define rows 0)
(define (want name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/split-symbols-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(rpc-dispatch store '(init) "test")

(define (write-bytes! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (write! path s) (write-bytes! path (string->utf8 s)))
(define (digest-of s) (bytevector->hex (sha256 (string->utf8 s))))
(define (offset-of text needle)
  (let ((b (string->utf8 text)) (n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b)) #f)
            ((let check ((k 0)) (or (= k (bytevector-length n))
                                    (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))

;; Each case gets its own directory, so "nothing was written" is a listing
;; of that directory before and after.
(define counter 0)
(define (case-dir!)
  (set! counter (+ counter 1))
  (let ((d (string-append root "/case-" (number->string counter)))) (mkdir-p! d) d))
(define (listing d) (list-sort string<? (directory-list d)))

(define header-source '(vscode "1.138.0" "javascript"))
(define (header digest source)
  (format "~s" (list 'symbols (list 'digest digest) (list 'source source) '(top-level #t))))
(define (symbol-line start end kind name) (format "~s" (list 'symbol start end kind name)))
(define (lines . ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))

;; -> (answer listing-before listing-after): the source written as `text`
;; under `name`, the symbols file beside it holding `symbols-text` (or none),
;; and split-suggest run in process.
(define (suggest name text symbols-text)
  (let* ((d (case-dir!)) (path (string-append d "/" name)) (sym (string-append d "/symbols.sexp")))
    (write! path text)
    (when symbols-text (write! sym symbols-text))
    (let* ((before (listing d))
           (a (rpc-dispatch store (append (list 'split-suggest path) (if symbols-text (list "--symbols" sym) '())) "test"))
           (after (listing d)))
      (list a before after))))
(define (clause a key)
  (let ((p (and (pair? a) (list? a) (assq key (filter pair? (cdr a)))))) (and p (cadr p))))

;; ---- S1: agreement ---------------------------------------------------------------
;; Three functions, the second with two comment lines above it. The editor's
;; starts are the three `function` lines; the cuts are 0 (the first
;; function's comment starts the file), 50 (the second function's comment),
;; 126 (the third, after a blank line). The scanner's own definition
;; patterns cut the same file in the same places.
(define s1 "// first helper\nfunction alpha() {\n  return 1;\n}\n\n// second helper\n// with two comment lines\nfunction beta() {\n  return 2;\n}\n\nfunction gamma() {\n  return 3;\n}\n")
(want "S1 setup: the text's offsets (alpha, the second comment, beta, gamma, the length)"
      (list (offset-of s1 "function alpha") (offset-of s1 "// second helper") (offset-of s1 "function beta")
            (offset-of s1 "function gamma") (bytevector-length (string->utf8 s1)))
      '(16 50 93 126 159))
(define s1-symbols
  (lines (header (digest-of s1) header-source)
         (symbol-line 16 49 'function "alpha")
         (symbol-line 93 125 'function "beta")
         (symbol-line 126 159 'function "gamma")))
(let ((r (suggest "s1.js" s1 s1-symbols))
      (twin (suggest "s1.js" s1 #f)))
  (want "S1 with --symbols: boundaries (0 50 126), the kinds in the warnings, cuts-from the editor"
        (list (car (car r)) (clause (car r) 'boundaries) (clause (car r) 'warnings) (clause (car r) 'cuts-from))
        (list 'ok '(0 50 126) '((symbol-kinds function function function)) header-source))
  (want "S1 TWIN without --symbols: the same boundaries, no warnings, cuts-from regex"
        (list (car (car twin)) (clause (car twin) 'boundaries) (clause (car twin) 'warnings) (clause (car twin) 'cuts-from))
        (list 'ok '(0 50 126) '() 'regex)))

;; ---- S2: attachment --------------------------------------------------------------
;; The second symbol's range starts at its keyword, and the comment line above
;; it goes with it: the cut is at the comment (25), not at the keyword (33).
(define s2 "// note\nfunction a() {\n}\n// lead\nfunction b() {\n}\n")
(want "S2 setup: the text's offsets (a, the lead comment, b, the length)"
      (list (offset-of s2 "function a") (offset-of s2 "// lead") (offset-of s2 "function b") (bytevector-length (string->utf8 s2)))
      '(8 25 33 50))
(let ((r (suggest "s2.js" s2 (lines (header (digest-of s2) header-source)
                                    (symbol-line 8 25 'function "a")
                                    (symbol-line 33 50 'function "b")))))
  (want "S2 a symbol whose range excludes its comment is cut at the comment's first line: (0 25)"
        (clause (car r) 'boundaries) '(0 25)))

;; ---- S3: the protected prefix ----------------------------------------------------
;; A shebang (20 bytes), a symbol on the very next line (start 20, the
;; prefix's size: dropped, as the scanner drops its own there) and a second
;; one further down that survives.
(define s3 "#!/usr/bin/env node\nfunction first() {\n  return 1;\n}\n\nfunction second() {\n  return 2;\n}\n")
(want "S3 setup: the text's offsets (first, second, the length)"
      (list (offset-of s3 "function first") (offset-of s3 "function second") (bytevector-length (string->utf8 s3)))
      '(20 54 88))
(let ((r (suggest "s3.js" s3 (lines (header (digest-of s3) header-source)
                                    (symbol-line 20 53 'function "first")
                                    (symbol-line 54 88 'function "second")))))
  (want "S3 the shebang and the first symbol stay in the first block, the second symbol cuts: (0 54)"
        (clause (car r) 'boundaries) '(0 54)))

;; ---- S4: stale -------------------------------------------------------------------
(let ((r (suggest "s4.js" s1 (lines (header (digest-of "other bytes") header-source)
                                    (symbol-line 16 49 'function "alpha")))))
  (want "S4 a digest of other bytes is refused symbols-stale with both digests, and nothing is written"
        (list (car r) (equal? (cadr r) (caddr r)))
        (list (list 'error 'symbols-stale (list 'digest-expected (digest-of "other bytes"))
                    (list 'digest-found (digest-of s1)))
              #t)))

;; ---- S5: malformed, one row per reason -------------------------------------------
;; A file with a two-byte character at 6-7 and two functions at 9 and 28.
(define s5 "// caf\x00e9;\nfunction one() {\n}\nfunction two() {\n}\n")
(want "S5 setup: the text's offsets (the two-byte character, one, two, the length)"
      (list (offset-of s5 "\x00e9;") (offset-of s5 "function one") (offset-of s5 "function two") (bytevector-length (string->utf8 s5)))
      '(6 9 28 47))
(define (s5-case label . symbol-lines)
  (let ((r (suggest "s5.js" s5 (apply lines symbol-lines))))
    (list (car r) (equal? (cadr r) (caddr r)))))
(define h5 (header (digest-of s5) header-source))
(want "S5 overlap: a range that starts before the previous one ends"
      (s5-case "overlap" h5 (symbol-line 9 30 'function "one") (symbol-line 28 47 'function "two"))
      '((error symbols-overlap (at 28)) #t))
(want "S5 not-a-boundary: a start inside the two-byte character"
      (s5-case "not-a-boundary" h5 (symbol-line 7 28 'function "one"))
      '((error symbols-not-a-boundary (at 7)) #t))
(want "S5 not-a-line-start: a start inside a line"
      (s5-case "not-a-line-start" h5 (symbol-line 10 28 'function "one"))
      '((error symbols-not-a-line-start (at 10)) #t))
(want "S5 past-end: an end one byte past the file"
      (s5-case "past-end" h5 (symbol-line 28 48 'function "two"))
      '((error symbols-past-end (at 48)) #t))
(want "S5 unordered: starts not strictly ascending"
      (s5-case "unordered" h5 (symbol-line 28 47 'function "two") (symbol-line 9 28 'function "one"))
      '((error symbols-unordered) #t))
(want "S5 empty: a header and no symbols"
      (s5-case "empty" h5)
      '((error symbols-empty) #t))
(want "S5 malformed line: an offset that is not a number, named by its line"
      (s5-case "malformed" h5 (symbol-line 9 28 'function "one") "(symbol 28 x function \"two\")")
      '((error symbols-malformed (line 3)) #t))
(want "S5 unknown kind: a kind outside the SymbolKind names, named by its line"
      (s5-case "kind" h5 (symbol-line 9 28 'function "one") (symbol-line 28 47 'widget "two"))
      '((error symbols-malformed (line 3)) #t))
(want "S5 missing header: the first line is a symbol"
      (s5-case "no-header" (symbol-line 9 28 'function "one"))
      '((error symbols-malformed (line 1)) #t))

;; ---- S6: no language entry -------------------------------------------------------
(define s6 "first thing\nsecond thing\nthird thing\n")
(define s6-source '(vscode "1.138.0" "plaintext"))
(let ((r (suggest "s6.xyz" s6 (lines (header (digest-of s6) s6-source)
                                     (symbol-line 12 25 'variable "second")
                                     (symbol-line 25 37 'variable "third")))))
  (want "S6 a file the language table does not know: the starts are the cuts, warned no-language-entry, cuts-from the editor"
        (list (car (car r)) (clause (car r) 'boundaries) (clause (car r) 'warnings) (clause (car r) 'cuts-from))
        (list 'ok '(0 12 25) '((no-language-entry) (symbol-kinds variable variable)) s6-source)))

;; ---- S7: the usage form ----------------------------------------------------------
(let* ((d (case-dir!)) (path (string-append d "/s7.js")))
  (write! path s1)
  (let ((a (rpc-dispatch store (list 'split-suggest path "extra") "test")))
    (want "S7 a bad call answers the usage form, and the form names --symbols"
          a
          '(usage (split-suggest <file> ("--output" <review-file>) ("--symbols" <symbols-file>))))))

;; ---- S8: a start the scanner does not see at the top level ------------------------
;; The second symbol starts inside a block comment: it is not a cut, and the
;; warnings say where it was. The block comment above the third function goes
;; with it (20).
(define s8 "function head() {\n}\n/* a\nfunction fake() {}\n*/\nfunction real() {\n}\n")
(want "S8 setup: the text's offsets (the comment, fake, real, the length)"
      (list (offset-of s8 "/* a") (offset-of s8 "function fake") (offset-of s8 "function real") (bytevector-length (string->utf8 s8)))
      '(20 25 47 67))
(let ((r (suggest "s8.js" s8 (lines (header (digest-of s8) header-source)
                                    (symbol-line 0 20 'function "head")
                                    (symbol-line 25 44 'function "fake")
                                    (symbol-line 47 67 'function "real")))))
  (want "S8 a start inside a block comment is not a cut and is warned symbol-not-top-level"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings))
        '((0 20) ((symbol-not-top-level (at 25)) (symbol-kinds function function function)))))

;; ---- S9: the symbols decide -------------------------------------------------------
;; A top-level constant that is not an arrow function: the language's
;; definition patterns do not see it, the editor's symbols do. With --symbols
;; it is cut (17); the TWIN without --symbols is one block. Every row above
;; could pass with the symbols ignored and the patterns used instead; this one
;; cannot.
(define s9 "function a() {\n}\nconst limit = 10;\n")
(want "S9 setup: the text's offsets (limit, the length)"
      (list (offset-of s9 "const limit") (bytevector-length (string->utf8 s9)))
      '(17 35))
(let ((r (suggest "s9.js" s9 (lines (header (digest-of s9) header-source)
                                    (symbol-line 0 17 'function "a")
                                    (symbol-line 17 35 'variable "limit"))))
      (twin (suggest "s9.js" s9 #f)))
  (want "S9 a constant the patterns do not know is cut by the symbols (0 17), and not without them (0)"
        (list (clause (car r) 'boundaries) (clause (car twin) 'boundaries))
        '((0 17) (0))))

;; ---- S10: a language with no suggestion profile ----------------------------------
;; Registered here, in this process: an entry the language table knows, with
;; no suggest-only profile. The scanner cannot follow it, so the editor's
;; starts are the cuts as given, and the warning names why.
(register-language! '((lang "rows") (extensions ("rws")) (line-comment "#") (def-heads ())))
(define s10 "alpha\nbeta\ngamma\n")
(define s10-source '(vscode "1.138.0" "plaintext"))
(let ((r (suggest "s10.rws" s10 (lines (header (digest-of s10) s10-source)
                                       (symbol-line 6 11 'variable "beta")
                                       (symbol-line 11 17 'variable "gamma")))))
  (want "S10 a known language without a suggestion profile: the starts are the cuts, warned no-suggest-profile"
        (list (car (car r)) (clause (car r) 'boundaries) (clause (car r) 'warnings) (clause (car r) 'cuts-from))
        (list 'ok '(0 6 11) '((no-suggest-profile) (symbol-kinds variable variable)) s10-source)))

;; ---- S11: a start inside the protected prefix -------------------------------------
;; A shebang and a coding line are the protected prefix (47 bytes). A symbol
;; on the coding line (23) is inside it: dropped, and warned. A symbol at the
;; prefix's end (47) starts the first block and is not warned; the next one
;; cuts (70).
(define s11 "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\ndef a():\n    return 1\n\ndef b():\n    return 2\n")
(want "S11 setup: the text's offsets (the coding line, a, b, the length)"
      (list (offset-of s11 "# -*-") (offset-of s11 "def a") (offset-of s11 "def b") (bytevector-length (string->utf8 s11)))
      '(23 47 70 92))
(let ((r (suggest "s11.py" s11 (lines (header (digest-of s11) '(vscode "1.138.0" "python"))
                                      (symbol-line 23 47 'variable "coding")
                                      (symbol-line 47 69 'function "a")
                                      (symbol-line 70 92 'function "b")))))
  (want "S11 a start inside the protected prefix is dropped and warned symbol-in-prefix; one at its end is not"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings))
        '((0 70) ((symbol-in-prefix (at 23)) (symbol-kinds variable function function)))))

;; ---- S12: a start on a fence's opening line ---------------------------------------
;; Markdown: the second symbol starts on the line that opens a fence. That line
;; is not a definition and is warned, like the lines inside the fence.
(define s12 "# Title\n\n```\ncode\n```\n\n# Next\n")
(want "S12 setup: the text's offsets (the fence, its close, Next, the length)"
      (list (offset-of s12 "```") (offset-of s12 "```\n\n") (offset-of s12 "# Next") (bytevector-length (string->utf8 s12)))
      '(9 18 23 30))
(let ((r (suggest "s12.md" s12 (lines (header (digest-of s12) '(vscode "1.138.0" "markdown"))
                                      (symbol-line 0 9 'string "Title")
                                      (symbol-line 9 22 'string "code")
                                      (symbol-line 23 30 'string "Next")))))
  (want "S12 a start on a fence's opening line is not a cut and is warned symbol-not-top-level"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings))
        '((0 23) ((symbol-not-top-level (at 9)) (symbol-kinds string string string)))))

;; ---- S13: a file that begins with a byte-order mark ------------------------------
;; S1's source with a mark in front. The symbols name byte offsets into the file
;; as it is on disk, mark included, so each is S1's plus 3, and the cuts are S1's
;; plus 3 after 0. Without --symbols the same cuts. It used to answer
;; invalid-utf8 and one block, with and without --symbols.
(define s13 (string-append "\xFEFF;" s1))
(let ((r (suggest "s13.js" s13
                  (lines (header (digest-of s13) header-source)
                         (symbol-line 19 52 'function "alpha")
                         (symbol-line 96 128 'function "beta")
                         (symbol-line 129 162 'function "gamma"))))
      (twin (suggest "s13.js" s13 #f)))
  (want "S13 a marked file with --symbols cuts as S1 does, each cut after 0 moved by the mark's 3 bytes, and without --symbols the same, no invalid-utf8"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings)
              (clause (car twin) 'boundaries) (clause (car twin) 'warnings))
        '((0 53 129) ((symbol-kinds function function function)) (0 53 129) ())))

;; ---- S14, S15: split-suggest keeps the lenient top-level test ------------------
;; NEVER: THE IMPORT'S STRICT TEST IS NOT SPLIT-SUGGEST'S. import-code --symbols
;; refuses a cut after a line whose code ends in a continuation token, and, in
;; an indent profile, a cut at a blank line whose next non-blank line is
;; indented (code-suggest.sc, the strict flag). split-suggest writes a review
;; copy a person reads before anything is imported, and keeps its old test:
;; both starts below are cuts, and neither is warned symbol-not-top-level.
(define s14 "const f = x =>\n  x + 1;\nfunction g() {\n}\n")
(want "S14 setup: the text's offsets (the arrow body's line, g, the length)"
      (list (offset-of s14 "  x + 1;") (offset-of s14 "function g") (bytevector-length (string->utf8 s14)))
      '(15 24 41))
(let ((r (suggest "s14.js" s14 (lines (header (digest-of s14) header-source)
                                      (symbol-line 0 14 'variable "f")
                                      (symbol-line 15 23 'function "body")
                                      (symbol-line 24 41 'function "g")))))
  (want "S14 a start on the line after one ending in => is a cut for split-suggest, with no symbol-not-top-level warning"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings))
        '((0 15 24) ((symbol-kinds variable function function)))))
(define s15 "def f():\n    a = 1\n\n    return a\n\ndef g():\n    pass\n")
(define s15-source '(vscode "1.138.0" "python"))
(want "S15 setup: the text's offsets (the blank line inside f, g, the length)"
      (list (offset-of s15 "\n\n    return") (offset-of s15 "def g") (bytevector-length (string->utf8 s15)))
      '(18 34 52))
(let ((r (suggest "s15.py" s15 (lines (header (digest-of s15) s15-source)
                                      (symbol-line 0 19 'function "f")
                                      (symbol-line 19 33 'variable "rest")
                                      (symbol-line 34 52 'function "g")))))
  (want "S15 a start at a blank line whose next line is indented is a cut for split-suggest, with no symbol-not-top-level warning"
        (list (clause (car r) 'boundaries) (clause (car r) 'warnings))
        '((0 19 34) ((symbol-kinds function variable function)))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nsplit-symbols complete\n" bad rows)
(exit (if (= bad 0) 0 1))
