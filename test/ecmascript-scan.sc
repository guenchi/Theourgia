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

;; THE ECMASCRIPT SCAN: JavaScript and TypeScript are scanned with every live
;; reading of the regex/division ambiguity kept, instead of the whole file
;; falling back at the first "/" or "`" (or "<" in TypeScript). A line start is
;; a cut only when every reading that survives the file agrees it is at the
;; top level.
;;
;; Each row asks split-suggest (boundaries and warnings) and, where it says
;; import, import-code --symbols on a fresh store with a symbols file naming the
;; given lines. Every expected boundary and offset is computed from the row's
;; own text (the byte offset of a named line), never read from the scan.
(import (chezscheme) (theourgia rpc)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-children code-field)
        (only (theourgia languages) language-for-name)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia digest) sha256 bytevector->hex))

(define bad 0)
(define rows 0)
(define (want name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/ecmascript-scan-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(define (write! path s)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 s)))))
(define (bytes-of s) (bytevector-length (string->utf8 s)))

;; THE BYTE OFFSET OF NEEDLE in TEXT, which must start a line; a needle that is
;; not there, or not at a line start, raises (a fixture mistake, not a reading).
(define (off text needle)
  (let ((b (string->utf8 text)) (n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b))
             (assertion-violation 'off "needle not in text" needle))
            ((and (or (= i 0) (= 10 (bytevector-u8-ref b (- i 1))))
                  (let check ((k 0)) (or (= k (bytevector-length n))
                                         (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1))))))
             i)
            (else (loop (+ i 1)))))))
;; THE BYTE OFFSET OF NEEDLE anywhere in TEXT (the first one).
(define (at text needle)
  (let ((b (string->utf8 text)) (n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b)) (assertion-violation 'at "needle not in text" needle))
            ((let check ((k 0)) (or (= k (bytevector-length n))
                                    (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))
;; THE END OF THE LINE AT OFFSET O: the offset after its line feed, or the size.
(define (line-end text o)
  (let ((b (string->utf8 text)))
    (let loop ((i o))
      (cond ((= i (bytevector-length b)) i)
            ((= 10 (bytevector-u8-ref b i)) (+ i 1))
            (else (loop (+ i 1)))))))

;; ---- split-suggest ------------------------------------------------------------------
(define store (string-append root "/store"))
(rpc-dispatch store '(init) "test")
(define counter 0)
(define (suggest ext text)
  (set! counter (+ counter 1))
  (let ((path (string-append root "/case-" (number->string counter) "." ext)))
    (write! path text)
    (rpc-dispatch store (list 'split-suggest path) "test")))
(define (clause a key)
  (let ((p (and (pair? a) (list? a) (assq key (filter pair? (cdr a)))))) (and p (cadr p))))
;; -> (boundaries warnings) of split-suggest on TEXT as a file with extension EXT
(define (split ext text) (let ((a (suggest ext text))) (list (clause a 'boundaries) (clause a 'warnings))))
(define (bounds ext text) (car (split ext text)))
;; TWO BLOCKS: the cut at the line NEEDLE starts, no warning
(define (two text needle) (list (list 0 (off text needle)) '()))
;; ONE BLOCK with the warning (code REASON byte-offset O)
(define (one reason o) (list '(0) (list (list 'code reason 'byte-offset o))))

;; ---- import-code --symbols --------------------------------------------------------
;; A fresh store and directory per import; the symbols file names the lines
;; NEEDLES start, each symbol ending at its line's end. -> (accepted <blocks>)
;; with the file's blocks' srcs, (refused <kind> <at>) for a file refused with
;; (error <kind> (at <n>)), (whole <kind> <reason> <blocks>) for a file
;; imported whole because its symbols could not be checked (listed in
;; symbols-imported-whole), else what was answered.
(define imports 0)
(define (import-with ext text needles)
  (set! imports (+ imports 1))
  (let* ((d (string-append root "/imp-" (number->string imports)))
         (st (string-append d "/store")) (src (string-append d "/src")) (syms (string-append d "/symbols.sexp"))
         (name (string-append "f." ext)))
    (sh "mkdir -p " (quoted src))
    (rpc-dispatch st '(init) "test")
    (write! (string-append src "/" name) text)
    (write! syms (apply string-append
                        (format "~s\n" (list 'symbols (list 'path name) (list 'digest (bytevector->hex (sha256 (string->utf8 text))))
                                             (list 'source (list 'vscode "1.0.0" (if (string=? ext "ts") "typescript" "javascript")))
                                             '(top-level #t)))
                        (map (lambda (n) (let ((o (off text n))) (format "~s\n" (list 'symbol o (line-end text o) 'function "s"))))
                             needles)))
    (let* ((a (rpc-dispatch st (list 'import-code src "--symbols" syms) "test"))
           (refused (clause a 'symbols-refused))
           (whole (clause a 'symbols-imported-whole))
           (srcs (lambda ()
                   (let* ((s (open-and-reduce st))
                          (f (find (lambda (f) (equal? (code-field s f 'path) name)) (code-files s))))
                     (and f (map (lambda (c) (let ((v (code-field s c 'src))) (if (bytevector? v) (utf8->string v) v)))
                                 (code-children s f)))))))
      (cond ((and (pair? a) (eq? (car a) 'ok) (pair? whole))
             (let ((e (cadr (car whole))))
               (if (and (pair? e) (eq? (car e) 'error) (pair? (cddr e)) (pair? (caddr e)))
                   (list 'whole (cadr e) (cadr (caddr e)) (srcs))
                   (list 'whole-as e))))
            ((and (pair? a) (eq? (car a) 'ok) (pair? refused))
             (let ((e (cadr (car refused))))
               (if (and (pair? e) (eq? (car e) 'error) (pair? (cddr e)) (pair? (caddr e)))
                   (list 'refused (cadr e) (cadr (caddr e)))
                   (list 'refused-as e))))
            ((and (pair? a) (eq? (car a) 'ok))
             (let* ((s (open-and-reduce st))
                    (f (find (lambda (f) (equal? (code-field s f 'path) name)) (code-files s))))
               (if f
                   (list 'accepted (map (lambda (c) (let ((v (code-field s c 'src))) (if (bytevector? v) (utf8->string v) v)))
                                        (code-children s f)))
                   (list 'not-held a))))
            (else (list 'answered a))))))
(define (accepted-in n text) (lambda (r) (and (eq? (car r) 'accepted) (= (length (cadr r)) n) (string=? (apply string-append (cadr r)) text))))
(define (refused-at text needle) (list 'refused 'symbols-not-top-level (off text needle)))

;; ==== safety rows: a candidate inside a literal is never a cut ====================
(printf "== safety ==\n")
(let ((t "const of = 1;\nconst x = of / `a/\nfunction fake() {}\nconst z = /b` / 2;\nfunction real() {}\n"))
  (want "S1 `of / ` keeps both readings: fake (inside the division reading's template) is not a cut, real is; an import with a symbol at fake is refused there"
        (list (split "js" t) (import-with "js" t '("function fake")))
        (list (two t "function real") (refused-at t "function fake"))))
(let ((t "if (a) /x/\n/ `a/\nfunction fake() {}\nconst z = /b` / 2;\nfunction real() {}\n"))
  (want "S2 a regex after `if (a)` then a line-leading /: the same answers as S1"
        (list (split "js" t) (import-with "js" t '("function fake")))
        (list (two t "function real") (refused-at t "function fake"))))
(let ((t "const t = `\nfunction fake() {}\n${ {a: 1}.a }\nfunction fake2() {}\n`;\nfunction real() {}\n"))
  (want "S3 a top-level multiline template with an interpolation: fake and fake2 are not cuts, real is; imports at fake, at fake2: refused; at real only: accepted, two blocks"
        (list (split "js" t) (import-with "js" t '("function fake()")) (import-with "js" t '("function fake2"))
              ((accepted-in 2 t) (import-with "js" t '("function real"))))
        (list (two t "function real") (refused-at t "function fake()") (refused-at t "function fake2") #t)))
(let ((t "const t = `${\nfunction fake() {}\n}`;\nfunction real() {}\n"))
  (want "S4 a candidate inside an interpolation spanning lines is not a cut; real is"
        (split "js" t) (two t "function real")))
(let ((t "const t = `a\\` ${\"}\"} \\${ `;\nfunction real() {}\n"))
  (want "S5 an escaped backtick and an escaped ${ open nothing, a \"}\" in an interpolation's string closes nothing: real is a cut"
        (split "js" t) (two t "function real")))
(let* ((cr "// c\r const s = `a\nfunction fake() {}\n// d\rb`;\nfunction real() {}\n")
       (ls (string-append "// c" (string #\x2028) " const s = `a\nfunction fake() {}\nfunction real() {}\n"))
       (ps (string-append "// c" (string #\x2029) " const s = `a\nfunction fake() {}\nfunction real() {}\n")))
  (want "S6 a lone CR, a U+2028 and a U+2029 each fall the whole file back lexically-uncertain at their offset"
        (list (split "js" cr) (split "js" ls) (split "ts" ps))
        (list (one 'lexically-uncertain 4) (one 'lexically-uncertain 4) (one 'lexically-uncertain 4))))
(let ((t "const x = value as void / `a/\nfunction fake() {}\nconst z = /b` / 2;\nfunction real() {}\n"))
  (want "S7 TypeScript `as void /` forks (void is also a type): fake is not a cut, real is; an import at fake is refused"
        (list (split "ts" t) (import-with "ts" t '("function fake")))
        (list (two t "function real") (refused-at t "function fake"))))
(let ((t "const x = 1 / /* c */\nfunction fake() {};\nfunction real() {}\n")
      (c "const x = 1; /* c */\nfunction g() {}\n"))
  (want "S8 the operand owed by `/` survives a comment: an import at fake and real is refused at fake; at real only, accepted; control `1; /* c */` before g: accepted"
        (list (import-with "js" t '("function fake" "function real")) ((accepted-in 2 t) (import-with "js" t '("function real")))
              ((accepted-in 2 c) (import-with "js" c '("function g"))))
        (list (refused-at t "function fake") #t #t)))
(let ((t "const x = a + /* c */\nfunction fake() {}\nfunction real() {}\n"))
  (want "S8b the operand owed by `+` survives a comment: an import at fake and real is refused at fake; at real only, accepted"
        (list (import-with "js" t '("function fake" "function real")) ((accepted-in 2 t) (import-with "js" t '("function real"))))
        (list (refused-at t "function fake") #t)))
(let* ((a "<!-- `\nconst x = `a\nfunction fake() {}\n`;\n--> `\nfunction real() {}\n")
       (b "const x = `a\nfunction fake() {}\n`;\n--> `\nfunction real() {}\n")
       (c1 "let x = 3;\nwhile (x-->0) {}\nfunction f() {}\n")
       (c2 "const s = \"<!--\";\nfunction f() {}\n")
       (c3 "const a = 1, b = 2;\na <!--b;\nfunction f() {}\n"))
  (want "S9 JavaScript's HTML-like comments: <!-- falls the file back at its offset, a line-start --> at its; x-->0 and a <!-- in a string are code; in TypeScript a <!--b is code"
        (list (split "js" a) (split "js" b) (split "js" c1) (split "js" c2) (split "ts" c3))
        (list (one 'lexically-uncertain 0) (one 'lexically-uncertain (off b "--> `"))
              (two c1 "function f") (two c2 "function f") (two c3 "function f"))))
;; JSX IN A .js FILE: the scan does not follow JSX text. Without the refusal,
;; line 2 below (JSX text) is judged top level by every reading and `fake`
;; becomes a cut. Every JSX element ends with "</" or "/>", so the file's text
;; is tested for those, and the whole file falls back at the first. A "<" that
;; compares is no closer.
(let ((j "const x = <div>\nfunction fake() {}\n</div>; // end\nfunction real() {}\n")
      (lt "const y = a < b;\nfunction f() {}\n")
      (lt2 "if (a <b) {}\nfunction f() {}\n"))
  (want "J1 JSX in a .js file falls the whole file back lexically-uncertain at its closing </; a < after a and a < after a in a condition are comparisons: two blocks each"
        (list (split "js" j) (split "js" lt) (split "js" lt2))
        (list (one 'lexically-uncertain (at j "</div>")) (two lt "function f") (two lt2 "function f"))))
;; A PRIVATE NAME CAN END AN EXPRESSION: `this.#return / ...` divides (the
;; template after it hides fake), and its name is not the reserved word. Read
;; as "#" then `return`, the "/" would be a regex only, and fake a cut.
(let ((t "class C { #return=1; m() { const x=this.#return / `a/}}\nfunction fake() { {\nconst z = /b` / 2; } }\nfunction real() {}\n")
      (c "const v = o.return / 2;\nfunction f() {}\n"))
  (want "P1 this.#return / forks in JavaScript and TypeScript: fake is not a cut, real is, an import at fake is refused; o.return / 2 is a division: two blocks"
        (list (split "js" t) (split "ts" t) (import-with "js" t '("function fake")) (split "js" c))
        (list (two t "function real") (two t "function real") (refused-at t "function fake") (two c "function f"))))
;; U+FEFF IS ECMASCRIPT WHITESPACE: a line holding only a space and a FEFF
;; before "-->" is still a fresh line, and the HTML-like close comment refuses
;; the file. Read as a name, FEFF would hide the "-->", and fake be a cut.
(let ((t (string-append "const n = 0;\n " (string #\xFEFF) "--> `\nconst t = `a\nfunction fake() {}\n// `;\nfunction real() {}\n"))
      (c "const n = 0;\n/* c */ --> `\nconst t = `a\nfunction fake() {}\n// `;\nfunction real() {}\n"))
  (want "F1 a space and a U+FEFF before a line-start --> keep the line fresh: lexically-uncertain at the -->; after a block comment, likewise"
        (list (split "js" t) (split "js" c))
        (list (one 'lexically-uncertain (at t "-->")) (one 'lexically-uncertain (at c "-->")))))
;; D6 HOLDS ANYWHERE IN THE FILE, THE PROTECTED PREFIX INCLUDED: a lone CR or
;; a U+2028 in a hashbang line falls the file back at its offset in the file;
;; a CRLF ending the hashbang line is one line end, and the definition after
;; the prefix starts the first block.
(let ((h1 "#!/usr/bin/env node\r x\nfunction f() {}\n")
      (h2 (string-append "#!/usr/bin/env node" (string #\x2028) "\nfunction f() {}\n"))
      (h3 "#!/usr/bin/env node\r\nfunction f() {}\nfunction g() {}\n"))
  (want "D6p a lone CR and a U+2028 in the hashbang line: lexically-uncertain at their offset; a CRLF hashbang: g is a cut, f starts the first block"
        (list (split "js" h1) (split "js" h2) (split "js" h3))
        (list (one 'lexically-uncertain 19) (one 'lexically-uncertain 19) (two h3 "function g"))))

;; ---- JSX: the text test ---------------------------------------------------------
;; A STATEMENT CAN START in more places than a lexical state names (after a
;; condition, a "}", await, yield, of, `export default`, and by an automatic
;; semicolon after any word such as `debugger`), and JSX text can look like a
;; comment or a string to the scan. So JSX is refused on the file's text: the
;; first "<" whose next character other than whitespace is "/", or "/" whose
;; next such character is ">", falls the whole file back. Each row is red on
;; the scan that refused JSX only by its lexical state.
(define (jsx-in head) (string-append head "<div>\nfunction fake() {}\n</div>; // end\nfunction real() {}\n"))
(let ((t (jsx-in "if (ok) ")))
  (want "JX1 JSX after an if's condition: lexically-uncertain at its </, and an import at fake is not cut there: the file is imported whole, listed symbols-unscanned"
        (list (split "js" t) (import-with "js" t '("function fake")))
        (list (one 'lexically-uncertain (at t "</div>")) (list 'whole 'symbols-unscanned 'lexically-uncertain (list t)))))
(let ((w (jsx-in "while (ok) ")) (f (jsx-in "for (;;) ")) (o (jsx-in "with (o) ")))
  (want "JX2 JSX after a while's, a for's and a with's condition: lexically-uncertain at its </ each"
        (list (split "js" w) (split "js" f) (split "js" o))
        (list (one 'lexically-uncertain (at w "</div>")) (one 'lexically-uncertain (at f "</div>")) (one 'lexically-uncertain (at o "</div>")))))
(let ((t (jsx-in "if (a) {}\n")))
  (want "JX3 JSX on the line after a block's }: lexically-uncertain at its </"
        (split "js" t) (one 'lexically-uncertain (at t "</div>"))))
(let ((a "async function g() { await <div>\nfunction fake() {}\n</div>; }\nfunction real() {}\n")
      (y "function* g() { yield <div>\nfunction fake() {}\n</div>; }\nfunction real() {}\n")
      (o "for (a of <x/>) {}\nfunction f() {}\n"))
  (want "JX4 JSX after await and after yield (g closed), and after of: lexically-uncertain at its </ or /> each"
        (list (split "js" a) (split "js" y) (split "js" o))
        (list (one 'lexically-uncertain (at a "</div>")) (one 'lexically-uncertain (at y "</div>")) (one 'lexically-uncertain (at o "/>")))))
(let ((t "export default <x/>;\nfunction f() {}\n"))
  (want "JX5 export default <x/>: lexically-uncertain at its />"
        (split "js" t) (one 'lexically-uncertain (at t "/>"))))
;; JSX AFTER AN AUTOMATIC SEMICOLON, and a closer the scan reads as a comment
;; (`// </div>` is JSX text): the test reads text, not the scan's reading.
(let ((d "debugger\n<div>\nfunction fake() {}\n// </div>;\nfunction real() {}\n")
      (f "for await (const x of xs) <div>\nfunction fake() {}\n// </div>;\nfunction real() {}\n"))
  (want "JX6 JSX after debugger and an automatic semicolon, and after for await's condition, closed inside what the scan reads as a comment: lexically-uncertain at that </, and an import at fake is not cut there: the file is imported whole, listed symbols-unscanned"
        (list (split "js" d) (import-with "js" d '("function fake")) (split "js" f))
        (list (one 'lexically-uncertain (at d "</div>")) (list 'whole 'symbols-unscanned 'lexically-uncertain (list d))
              (one 'lexically-uncertain (at f "</div>")))))
;; WHITESPACE AND COMMENTS BETWEEN: a closing tag's "<" and "/" may be
;; separated by spaces, a line end or a comment (which starts with "/"); a
;; self-close's "/" and ">" by spaces. A "// > quoted" comment is caught the
;; same way, at its second "/".
(let ((a "x = <div>\nfunction fake() {}\n< /div>;\nfunction real() {}\n")
      (b "x = <div>\nfunction fake() {}\n<\n/div>;\nfunction real() {}\n")
      (c "x = <div>\nfunction fake() {}\n< /*c*/ /div>;\nfunction real() {}\n")
      (d "x = <br / >;\nfunction f() {}\n")
      (e "x = 1; // > quoted\nfunction f() {}\n"))
  (want "JX7 < /div>, < and /div> on two lines, < /*c*/ /div>, / > and // > quoted: lexically-uncertain at the < or the / each"
        (list (split "js" a) (split "js" b) (split "js" c) (split "js" d) (split "js" e))
        (list (one 'lexically-uncertain (at a "< /div>")) (one 'lexically-uncertain (at b "<\n/div>"))
              (one 'lexically-uncertain (at c "< /*c*/")) (one 'lexically-uncertain (at d "/ >"))
              (one 'lexically-uncertain (+ (at e "// >") 1)))))
;; THE COST, ACCEPTED (the safe direction): valid JavaScript whose text holds
;; "</" or "/>" falls back too.
(let ((a "const small = x</re/.source.length;\nfunction real() {}\n")
      (b "if (ok) />/.test(s);\nfunction real() {}\n")
      (c "s = s.replace(/>/g, '&gt;');\nfunction f() {}\n")
      (d "s = \"</p>\";\nfunction f() {}\n")
      (e "/* a <br/> in a comment */\nfunction f() {}\n"))
  (want "JX8 the accepted cost: x</re/, a regex statement />/, .replace(/>/g ...), \"</p>\" in a string and <br/> in a comment: lexically-uncertain at the first </ or /> each"
        (list (split "js" a) (split "js" b) (split "js" c) (split "js" d) (split "js" e))
        (list (one 'lexically-uncertain (at a "</re")) (one 'lexically-uncertain (at b "/>"))
              (one 'lexically-uncertain (at c "/>")) (one 'lexically-uncertain (at d "</p"))
              (one 'lexically-uncertain (at e "/>")))))
(let ((c1 "if (f(x) <b) {}\nfunction g() {}\n") (c2 "x = a[i] <b;\nfunction f() {}\n") (c3 "x = (a) <b;\nfunction f() {}\n")
      (c4 "for (let i=0;i<n;i++) {}\nfunction f() {}\n") (c5 "export default /re/;\nfunction f() {}\n")
      (c6 "const r = a < b ? c / d : e / f;\nfunction g() {}\n") (c7 "x = a < b / c;\nfunction f() {}\n"))
  (want "JX9 controls in .js: f(x) <b, a[i] <b, (a) <b, i<n in a for header, export default /re/, and comparisons with divisions: two blocks each"
        (list (split "js" c1) (split "js" c2) (split "js" c3) (split "js" c4) (split "js" c5) (split "js" c6) (split "js" c7))
        (list (two c1 "function g") (two c2 "function f") (two c3 "function f") (two c4 "function f") (two c5 "function f")
              (two c6 "function g") (two c7 "function f"))))
(let ((t1 "if (ok) <T>x;\nfunction f() {}\n") (t2 "const s = \"</p>\";\nfunction f() {}\n")
      (t3 "x = a.replace(/>/g, '');\nfunction f() {}\n"))
  (want "JX10 TypeScript is not tested: if (ok) <T>x, \"</p>\" in a string and /> in a regex: two blocks each"
        (list (split "ts" t1) (split "ts" t2) (split "ts" t3))
        (list (two t1 "function f") (two t2 "function f") (two t3 "function f"))))

;; ==== ambiguity rows: forks ===========================================================
(printf "== ambiguity ==\n")
(let ((d "debugger\n/\"/.test(s);\nfunction f() {}\n") (l "let x\n/\"/.test(s);\nfunction f() {}\n"))
  (want "A1 after `debugger` and after `let x` at a line end the division reading dies at the unclosed string and the regex reading lives: two blocks each"
        (list (split "js" d) (split "js" l)) (list (two d "function f") (two l "function f"))))
(let ((t "type T = number[]\n/\"/.test(s);\nfunction f() {}\n"))
  (want "A2 TypeScript `number[]` then a line-leading regex: two blocks" (split "ts" t) (two t "function f")))
(let ((a "const x = value! / 2;\nfunction f() {}\n") (b "const r = f<number> / 2;\nfunction f() {}\n")
      (c "const r = <T>/x/;\nfunction f() {}\n"))
  (want "A3 TypeScript `!` and `>` fork: value! / 2, f<number> / 2 and <T>/x/ are two blocks each"
        (list (split "ts" a) (split "ts" b) (split "ts" c)) (list (two a "function f") (two b "function f") (two c "function f"))))
(let ((a "const x = f<Array<number>> / `a/\nfunction fake() {}\nconst z = /b` / 2;\nfunction real() {}\n")
      (b "const x = f<Array<Array<number>>> / `a/\nfunction fake() {}\nconst z = /b` / 2;\nfunction real() {}\n")
      (c "let k = a >>= /x/.source.length;\nfunction f() {}\n"))
  (want "A3b TypeScript nested closers >> and >>> fork: fake is not a cut, real is, and an import at fake is refused; `>>= /x/` is a regex: two blocks"
        (list (split "ts" a) (import-with "ts" a '("function fake")) (split "ts" b) (import-with "ts" b '("function fake")) (split "ts" c))
        (list (two a "function real") (refused-at a "function fake") (two b "function real") (refused-at b "function fake")
              (two c "function f"))))
(let ((t "if (a) /[)]/.test(s);\nfunction f() {}\n"))
  (want "A4 the division reading dies at `)` against `[`, locally: two blocks" (split "js" t) (two t "function f")))
(let ((t "const x = {} / 2;\nfunction f() {}\n"))
  (want "A5 an object literal divided: two blocks" (split "js" t) (two t "function f")))
(let ((t "if (a) /`/.test(s);\nconst k = 1;\n`;\nfunction f() {}\n"))
  (want "A6 the regex reading dies at end of file in a template it opened on line 3; the division reading survives: f is a cut, const k is not; an import at const k and f is refused at const k; at f only, accepted"
        (list (split "js" t) (import-with "js" t '("const k" "function f")) ((accepted-in 2 t) (import-with "js" t '("function f"))))
        (list (two t "function f") (refused-at t "const k") #t)))
;; THE CAP: every fork's regex reading swallows the next piece's opener up to
;; the following "/", so the readings' stacks are the different subsequences of
;; the alternating ( and { openers and stay distinct; their number passes 32
;; before the line ends. The fallback is at one of the line's slashes, past the
;; first (where the scan without the cap would not stop).
(let* ((t (string-append "x = a / (b / {c / (d / {e / (f / {g / (h / {i / (j / {k / (l / {m\n" "function f() {}\n"))
       (r (split "js" t))
       (slashes (let loop ((i 0) (out '()))
                  (let ((b (string->utf8 t)))
                    (cond ((= i (bytevector-length b)) (reverse out))
                          ((= 47 (bytevector-u8-ref b i)) (loop (+ i 1) (cons i out)))
                          (else (loop (+ i 1) out)))))))
  (want "A7 past 32 live readings the file falls back lexically-uncertain at a forking slash after the first"
        (let ((w (cadr r)))
          (list (car r) (and (= 1 (length w)) (eq? (cadr (car w)) 'lexically-uncertain))
                (and (= 1 (length w)) (let ((o (cadddr (car w)))) (and (memv o (cdr slashes)) #t)))))
        '((0) #t #t)))
;; LINEAR: the 4N file costs at most six times the N file, both cut at f.
(let* ((n 2000)
       (body (lambda (k) (apply string-append (let loop ((i 0) (out '())) (if (= i k) out (loop (+ i 1) (cons "x = (a) / b / c;\n" out)))))))
       (t1 (string-append (body n) "function f() {}\n"))
       (t4 (string-append (body (* 4 n)) "function f() {}\n"))
       (entry (language-for-name "javascript"))
       (timed (lambda (t)
                (let ((b (string->utf8 t)))
                  (let* ((t0 (real-time)) (r (suggest-boundaries entry b)) (t1 (real-time)))
                    (list r (- t1 t0))))))
       (r1 (timed t1)) (r4 (timed t4)))
  (printf "  A8 times: N ~a ms, 4N ~a ms\n" (cadr r1) (cadr r4))
  (want "A8 N and 4N lines of `x = (a) / b / c;`: both two blocks at f, the 4N scan at most 6 times the N scan"
        (list (car r1) (car r4) (<= (cadr r4) (* 6 (max 1 (cadr r1)))))
        (list (list (list 0 (off t1 "function f")) '()) (list (list 0 (off t4 "function f")) '()) #t)))
(let ((t "const r = (a) / 2;\nfunction f() {}\nconst q = /x/;\nfunction g() {}\n"))
  (want "A9 the regex reading of `(a) / 2;` dies at its line end: f and g are cuts; an import at f and g is accepted"
        (list (bounds "js" t) ((accepted-in 4 t) (import-with "js" t '("function f" "function g"))))
        (list (list 0 (off t "function f") (off t "function g")) #t)))

;; ==== ordinary rows ===================================================================
(printf "== ordinary ==\n")
(let ((t "export const m: Record<string, unknown> = {};\nfunction f() {}\n"))
  (want "E1 TypeScript generics: two blocks" (split "ts" t) (two t "function f")))
(let ((t "const r = (a + b) / 2;\nfunction f() {}\n"))
  (want "E2 a division, JavaScript and TypeScript: two blocks each" (list (split "js" t) (split "ts" t)) (list (two t "function f") (two t "function f"))))
(let ((t "const re = /[\"{]/g;\nfunction f() {}\n"))
  (want "E3 a regex holding a quote and a brace: two blocks" (split "js" t) (two t "function f")))
;; Moved from CT-04's fallback table in code-suggest1.sc: a regex and a template
;; in a function no longer fall the file back.
(let ((r "function a() { return /x/; }\nfunction b() {}\n") (tp "function a() { return `x`; }\nfunction b() {}\n"))
  (want "E4 a regex and a template inside a function: two blocks each"
        (list (split "js" r) (split "js" tp)) (list (two r "function b") (two tp "function b"))))
(let ((c "const re = /[/]/;\nfunction f() {}\n") (e "const re = /a\\/b/;\nfunction f() {}\n"))
  (want "E5 E6 a / in a regex class and an escaped / in a regex: two blocks each"
        (list (split "js" c) (split "js" e)) (list (two c "function f") (two e "function f"))))
(let ((t "const s = `${ /* } */ /}/.source }`;\nfunction f() {}\n"))
  (want "E7 a comment and a regex inside an interpolation hide their braces: two blocks" (split "js" t) (two t "function f")))
(let ((t "const x = a /\n  b;\nfunction f() {}\n") (r "const re = /x/\nfunction f() {}\n"))
  (want "E8 a line ending in a division continues: an import at line 2 and f is refused at line 2; at f only, accepted; a line ending in a regex literal does not continue: accepted"
        (list (import-with "js" t '("  b;" "function f")) ((accepted-in 2 t) (import-with "js" t '("function f")))
              ((accepted-in 2 r) (import-with "js" r '("function f"))))
        (list (refused-at t "  b;") #t #t)))
(let ((t "const s = \"a\\\n/\";\nfunction f() {}\n"))
  (want "E9 a line-continuation escape keeps the string reading alive across the line: two blocks" (split "js" t) (two t "function f")))
(let* ((ls '("export const routes: Record<string, number> = { a: 1 };\n"
             "const isId = (s: string) => /^[a-z]+$/.test(s);\n"
             "export function half(n: number): number { return n / 2; }\n"
             "const label = `item ${routes.a}/${half(4)}`;\n"
             "export type Pair<T> = [T, T];\n"
             "export const cfg = { re: /\\// } as Record<string, RegExp>;\n"))
       (t (apply string-append ls)))
  (want "E10 the iter shape: six declarations with generics, as Record<...>, an arrow returning a regex, a template with ${}; an import at each line: six blocks, byte-equal to the file"
        ((accepted-in 6 t) (import-with "ts" t (map (lambda (l) (substring l 0 (- (string-length l) 1))) ls)))
        #t))

;; ==== the end states: what a scan that cannot finish answers ==========================
;; THE SAME REASONS AS THE TABLE PATH for the same end state: an unclosed
;; '...' or "..." at a line end is unclosed-quote there; any state open at the
;; end of the file (a pair, a template, a continued string, a block comment) is
;; unbalanced at the file's length; a closer that matches nothing is unbalanced
;; at it; a regex open at a line end, which the table path has no state for, is
;; lexically-uncertain there.
(printf "== end states ==\n")
(let ((p "function a() {\nfunction b() {}\n") (tp "const t = `abc\nfunction f() {}\n")
      (q "const s = 'abc\nfunction f() {}\n") (c "/* abc\nfunction f() {}\n")
      (cs "function f() {}\nconst s = 'abc\\\n")
      (rx "const r = /abc\nfunction f() {}\n") (m "function a() { ]\nfunction b() {}\n"))
  (want "Q1 an open pair, an open template, an open block comment and a string continued past the last line at end of file: unbalanced at the file's length; an unclosed quote: unclosed-quote at its line's end; an open regex: lexically-uncertain at its line's end; a stray closer: unbalanced at it"
        (list (split "js" p) (split "js" tp) (split "js" c) (split "js" cs) (split "js" q) (split "js" rx) (split "js" m))
        (list (one 'unbalanced (bytes-of p)) (one 'unbalanced (bytes-of tp)) (one 'unbalanced (bytes-of c)) (one 'unbalanced (bytes-of cs))
              (one 'unclosed-quote (- (off q "function f") 1)) (one 'lexically-uncertain (- (off rx "function f") 1))
              (one 'unbalanced (at m "]")))))

;; ==== reach: import-git of a file held as one block ===================================
;; A file imported as ONE block (as the old rules held it) and changed in one
;; line by a commit: import-git's pieces are the new rules', every byte is kept
;; (the export is the commit's), and the old block keeps its id as the first
;; piece.
(printf "== reach ==\n")
(let* ((d (string-append root "/g1")) (wt (string-append d "/wt")) (st (string-append d "/store")) (src (string-append d "/src"))
       (v0 "function a() { return /x/; }\nfunction b() { return 1; }\nfunction c() { return 2; }\n")
       (v1 "function a() { return /x/; }\nfunction b() { return 3; }\nfunction c() { return 2; }\n")
       (git (lambda args (sh "git -C " (quoted wt) " " (apply string-append (map (lambda (a) (string-append a " ")) args)) " > /dev/null 2>&1")))
       (rev (lambda () (let ((f (string-append d "/rev")))
                          (sh "git -C " (quoted wt) " rev-parse HEAD > " (quoted f))
                          (let ((t (call-with-input-file f get-line))) (if (string? t) t "")))))
       (out (string-append d "/import.out")))
    (sh "mkdir -p " (quoted wt) " " (quoted src))
    (git "init -q") (git "config user.name fixture") (git "config user.email fixture@example.invalid") (git "config commit.gpgsign false")
    (write! (string-append wt "/a.js") v0) (git "add -A") (git "commit -q -m base")
    (let ((c0 (rev)))
      (write! (string-append wt "/a.js") v1) (git "commit -q -am edit")
      (let ((c1 (rev)))
        (rpc-dispatch st '(init) "test")
        (write! (string-append src "/a.js") v0)
        (rpc-dispatch st (list 'import-code src) "test")
        (let* ((before (let ((s (open-and-reduce st))) (code-children s (car (code-files s))))))
          (sh "scheme --script ../tools/import-git.sc " (quoted st) " " (quoted wt) " " c0 " " c1 " --tag g1 > " (quoted out) " 2>&1 < /dev/null")
          (let* ((s (open-and-reduce st)) (f (car (code-files s))) (after (code-children s f))
                 (srcs (map (lambda (c) (let ((v (code-field s c 'src))) (if (bytevector? v) (utf8->string v) v))) after)))
            (want "G1 a file held as one block and changed in one line: after import-git it is the new rules' three pieces, byte-equal to the commit, the old block's id kept by the first"
                  (list (length before) srcs (and (pair? after) (pair? before) (equal? (car after) (car before))))
                  (list 1 (list "function a() { return /x/; }\n" "function b() { return 3; }\n" "function c() { return 2; }\n") #t)))))))

(printf "rows: ~a\n~a failures\necmascript-scan complete\n" rows bad)
(exit (if (zero? bad) 0 1))
