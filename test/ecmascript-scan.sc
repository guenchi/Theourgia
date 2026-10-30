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
;; becomes a cut. The "<" of `<div>` stands where an operand is expected, so
;; the whole file falls back `opener` there. A "<" that compares is no opener.
(let ((j "const x = <div>\nfunction fake() {}\n</div>; // end\nfunction real() {}\n")
      (lt "const y = a < b;\nfunction f() {}\n")
      (lt2 "if (a <b) {}\nfunction f() {}\n"))
  (want "J1 JSX in a .js file falls the whole file back at its opener (opener); a < after a and a < after a in a condition are comparisons: two blocks each"
        (list (split "js" j) (split "js" lt) (split "js" lt2))
        (list (one 'opener (at j "<div>")) (two lt "function f") (two lt2 "function f"))))
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

;; ---- JSX: the opener and the self-close (rules O and S) --------------------------
;; THE OPENER: in plain JavaScript a "<" where an operand is expected (the
;; scan's rx) opens JSX: the file falls back `opener`, on any live reading.
;; THE SELF-CLOSE: a "/" read as division whose next significant character
;; is ">" cannot be JavaScript (">" begins no divisor): `self-close`. Neither
;; looks into comments, strings, template text or regex bodies, so markup there
;; does not refuse. Each refusing row is red on the raw-text test it replaces;
;; each splitting row with markup in text is red there too.
(define (blocks text . needles) (list (cons 0 (map (lambda (n) (off text n)) needles)) '()))
;; O1-O5: markup that is not code splits; so do comparisons.
(let ((t "/* <script>x</script> */\nfunction f() {}\nfunction g() {}\n"))
  (want "O1 <script>...</script> in a block comment: the file splits"
        (split "js" t) (two t "function g")))
(let ((t "x = 0; // >0\nfunction f() {}\n"))
  (want "O2 a trailing // >0 comment: the file splits" (split "js" t) (two t "function f")))
(let ((d "s = \"</p>\";\nfunction f() {}\n") (q "s = '<br/>';\nfunction f() {}\n") (b "s = `</p>`;\nfunction f() {}\n"))
  (want "O3 markup in a double-quoted, a single-quoted and a template string: each splits"
        (list (split "js" d) (split "js" q) (split "js" b))
        (list (two d "function f") (two q "function f") (two b "function f"))))
(let ((r "s = s.replace(/</g, \"&lt;\");\nfunction f() {}\n") (c "re = /[^>]*>([^<]*)</g;\nfunction f() {}\n"))
  (want "O4 a regex /</g and a regex holding </: each splits"
        (list (split "js" r) (split "js" c)) (list (two r "function f") (two c "function f"))))
(let ((a "y = a<b;\nfunction f() {}\n") (b "y = f() < g;\nfunction f() {}\n") (c "y = x\n< y;\nfunction f() {}\n"))
  (want "O5 comparisons a<b, f() < g, and x then a line end then < y: each splits"
        (list (split "js" a) (split "js" b) (split "js" c))
        (list (two a "function f") (two b "function f") (two c "function f"))))
;; O6: the real JSX contexts are rx, refused at the opener.
(for-each
  (lambda (c)
    (let ((t (string-append (cadr c) "\nfunction f() {}\n")))
      (want (string-append "O6 " (car c) ": refused opener at its <") (split "js" t) (one 'opener (at t (caddr c))))))
  '(("return" "function g() { return <a/>; }" "<a/>") ("=" "x = <a/>;" "<a/>") ("(" "h(<a/>);" "<a/>")
    ("," "h(1, <a/>);" "<a/>") ("?" "y = c ? <a/> : 0;" "<a/>") (":" "y = c ? 0 : <a/>;" "<a/>")
    ("=>" "const g = () => <a/>;" "<a/>") ("&&" "y = c && <a/>;" "<a/>") ("||" "y = c || <a/>;" "<a/>")
    ("??" "y = c ?? <a/>;" "<a/>") ("[" "y = [<a/>];" "<a/>") ("default" "export default <a/>;" "<a/>")
    ("an object's k:" "y = { k: <a/> };" "<a/>") ("case 1:" "switch (v) { case 1: <a/>; }" "<a/>")
    ("${" "y = `${<a/>}`;" "<a/>") ("!" "y = !<a/>;" "<a/>") ("..." "y = [...<a/>];" "<a/>")
    ("a URL in JSX text" "x = <a>see https://x.org</a>;" "<a>") ("an apostrophe in JSX text" "y = <p>don't stop</p>;" "<p>")
    ("a fragment" "y = <>x</>;" "<>")))
;; SC1-SC3: a self-closing element where the scan expects an operator.
(for-each
  (lambda (c)
    (let ((t (cadr c)))
      (want (string-append "SC1 " (car c) ": refused self-close at its /") (split "js" t) (one 'self-close (at t (caddr c))))))
  '(("if (ok) <br/>" "if (ok) <br/>\nfunction fake() {}\n" "/>") ("a block's } then <br/>" "if (a) {}\n<br/>\nfunction fake() {}\n" "/>")
    ("debugger then <br/>" "debugger\n<br/>\nfunction fake() {}\n" "/>") ("yield <br/>" "function* g() {\n  yield <br/>;\n}\nfunction f() {}\n" "/>")
    ("if (ok) <img src={a}/>" "if (ok) <img src={a}/>\nfunction fake() {}\n" "/>") ("return await <a/>" "async function g() {\n  return await <a/>;\n}\nfunction f() {}\n" "/>")))
(let ((c "x = a / /*c*/ >\nfunction f() {}\n") (n "if (ok) <br/\n>\nfunction fake() {}\n"))
  (want "SC2 / /*c*/ > and / then a line end then >: refused self-close at the /"
        (list (split "js" c) (split "js" n))
        (list (one 'self-close (at c "/ /*")) (one 'self-close (at n "/\n>")))))
;; SC6: S's lookahead skips a // comment and a /* */ comment across lines: the
;; ">" after either still refuses, at the first "/".
(let ((lc "x = a / //\n>\nfunction f() {}\n") (mb "x = a / /* c\n*/ >\nfunction f() {}\n"))
  (want "SC6 / then a // comment then > on the next line, and / then a /* */ comment over two lines then >: refused self-close at the first /"
        (list (split "js" lc) (split "js" mb))
        (list (one 'self-close (at lc "/ //")) (one 'self-close (at mb "/ /*")))))
;; SC7 CONTROLS for the lookahead: an unterminated /* (the lookahead ends at
;; the file's end; the regex reading "/ /" survives), a // at the file's end,
;; a lone "/" as the last character, and a ">" inside a skipped comment before
;; an operand (the lookahead does not merely search for ">").
(let ((ub "x = a / /* c\nfunction f() {}\n") (le "x = a / //") (lo "x = a /")
      (gt "x = a / /* > */ b;\nfunction f() {}\n"))
  (want "SC7 CONTROL an unterminated /*, a // at the end, a lone / at the end, and a > inside a skipped comment: none refused self-close"
        (list (split "js" ub) (split "js" le) (split "js" lo) (split "js" gt))
        (list (two ub "function f") '((0) ()) '((0) ()) (two gt "function f"))))
(let ((a "y = a / b > c;\nfunction f() {}\n") (b "y = a/2>b;\nfunction f() {}\n") (c "y = a /= 2 > 1;\nfunction f() {}\n"))
  (want "SC3 CONTROL a / b > c, a/2>b and a /= 2 > 1: each splits"
        (list (split "js" a) (split "js" b) (split "js" c))
        (list (two a "function f") (two b "function f") (two c "function f"))))
;; O7: an earlier fk "/" forks the readings; on the regex reading the "<" is
;; in a regex body, on the division reading it is at rx. A gate that asks
;; only the first reading (the regex one) lets it split: the element does not
;; self-close and its closer is in // text, so nothing but the opener refuses it.
(let ((t "x = a / b; y = <p>\nfunction fake() {}\n// </p>;\nfunction real() {}\n"))
  (want "O7 a fork before the opener: the second (division) reading meets <p> at rx, refused opener"
        (split "js" t) (one 'opener (at t "<p>"))))
;; L1-L3, THE STATED LIMIT: a non-self-closing element whose outermost opener
;; is where the scan expects an operator (fk), its closers only in // text, is
;; NOT refused. In L1 and L2 the file is mis-split (a cut inside JSX; the
;; blocks still join to the file's bytes), and the comment line holding the
;; closer is taken as the comment above `real`, so real's block starts there.
;; L3's JSX is inside g, so it only passes. Making fk exact needs
;; the statement/expression distinction, which the scan does not make; a scan
;; that knows statement starts turns these red, and that is meant.
(let ((t "debugger\n<div>\nfunction fake() {}\n// </div>;\nfunction real() {}\n"))
  (want "L1 THE LIMIT: debugger, <div>, closer in // text: split (mis-cut)"
        (split "js" t) (blocks t "function fake" "// </div>")))
(let ((t "if (ok) <div>\nfunction fake() {}\n// </div>;\nfunction real() {}\n"))
  (want "L2 THE LIMIT: if (ok) <div>, closer in // text: split (mis-cut)"
        (split "js" t) (blocks t "function fake" "// </div>")))
(let ((t "function* g() {\n  yield <div>\nfunction fake() {}\n// </div>;\n}\nfunction real() {}\n"))
  (want "L3 THE LIMIT: yield <div>, closer in // text: passes, its one cut at real outside g"
        (split "js" t) (blocks t "function real")))
;; L4, L5: the same limit with the closer in CODE: its "/" opens a regex at
;; rx that a later "/" on the line closes, so the scan stays at the top level
;; inside the JSX. The cut at fake lands between the opener and the closer.
(let ((t "if (ok) <div>\nfunction fake() {}\ntext</div> / 2;\nfunction real() {}\n"))
  (want "L4 THE LIMIT: if (ok) <div>, the closer swallowed by a regex a later / closes: split, the cut at fake inside the JSX"
        (list (split "js" t) (< (at t "<div>") (at t "function fake") (at t "</div>")))
        (list (blocks t "function fake" "function real") #t)))
(let ((t "if (ok) <>\nfunction fake() {}\ntext</> / 2;\nfunction real() {}\n"))
  (want "L5 THE LIMIT: the fragment form of L4: split"
        (split "js" t) (blocks t "function fake" "function real")))
;; SC4, S'S STATED COST: a regex beginning with ">" where the scan expects an
;; operator keeps a division reading, whose "/" is followed by ">", so the file
;; is refused though the program is a regex. The raw-text net refused it too.
(let ((a "if (ok) />/.test(s);\nfunction f() {}\n") (d "debugger\n/>/.test(s);\nfunction f() {}\n"))
  (want "SC4 THE COST: a regex />/ at an fk position (after if's condition, after debugger and a line end): refused self-close"
        (list (split "js" a) (split "js" d))
        (list (one 'self-close (at a "/>/")) (one 'self-close (at d "/>/")))))
;; L6, THE LIMIT stated on readings: a live reading reaches the end of the
;; file without meeting O or S. Here the self-close's "/>" is consumed inside a
;; JS string (JSX attribute strings have no escape; JS strings do), so no
;; reading meets S, and the file splits.
(let ((t "if (ok) <a x=\"\\\"/>; /\"/;\nfunction f() {}\n"))
  (want "L6 THE LIMIT: a self-close whose /> a reading consumes in a string: split"
        (split "js" t) (two t "function f")))
;; SC5, THE COST stated on readings: some live reading, speculative ones
;; included, meets O or S. Here the regex reading of the first "/" ends inside
;; the string, and on it the "/" before ">" is a division: refused, though the
;; program is plain JavaScript.
(let ((t "const z = a / \"x/y/>\" / b;\nfunction f() {}\n"))
  (want "SC5 THE COST: a speculative regex reading ends inside a string and meets S: refused self-close"
        (split "js" t) (one 'self-close (at t "/>"))))
;; O8, THE IMPORT: a file refused opener is imported whole by import-code
;; --symbols, its symbols unchecked and listed with the reason.
(let ((t "const x = <div>\nfunction fake() {}\n</div>;\nfunction real() {}\n"))
  (want "O8 import --symbols at fake in a file refused opener: imported whole, listed symbols-unscanned with reason opener"
        (import-with "js" t '("function fake")) (list 'whole 'symbols-unscanned 'opener (list t))))
;; I1-I4: iter's four shapes, minimal: a doc comment holding markup, a trailing
;; // >0, regexes /</g and />/g with HTML strings, and a regex holding </ with a
;; string holding markup. Each splits.
(let ((p "/**\n * <script>x</script> is text\n */\nexport function parse(s) {\n  return s;\n}\nexport function g() {}\n")
      (g "const o = { abortAfterBytes: 0 }   // >0\nfunction f() {}\n")
      (a "function esc(s) {\n  return s.replace(/</g, \"&lt;\").replace(/>/g, \"&gt;\");\n}\nfunction row() {\n  return \"<label>\" + esc(1) + \"</label>\";\n}\n")
      (m "const re = /data=\"([^\"]+)\"[^>]*>([^<]*)</g;\ncheck(\"<span>ID</span>\");\nfunction f() {}\n"))
  (want "I1-I4 iter's shapes (doc comment, // >0, regexes with HTML strings, a regex holding </): each splits"
        (list (split "js" p) (split "js" g) (split "js" a) (split "js" m))
        (list (two p "export function g") (two g "function f") (two a "function row") (two m "function f"))))

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
;; THE BOUND IS A GUARD, NOT A MEASUREMENT: a linear scan reads 4 and a
;; quadratic one 16, so 6 leaves half again for noise and still tells the
;; two apart. THE MEDIAN OF THREE ALTERNATING PAIRS decides it, so one
;; stalled window (a collection, the process descheduled) cannot: on a
;; loaded three-core runner a single pair read 14 and 94 ms, where this
;; scan's pairs read about 8 and 31 alone and under load.
(let* ((n 2000)
       (body (lambda (k) (apply string-append (let loop ((i 0) (out '())) (if (= i k) out (loop (+ i 1) (cons "x = (a) / b / c;\n" out)))))))
       (t1 (string-append (body n) "function f() {}\n"))
       (t4 (string-append (body (* 4 n)) "function f() {}\n"))
       (entry (language-for-name "javascript"))
       (timed (lambda (t)
                (let ((b (string->utf8 t)))
                  (let* ((t0 (real-time)) (r (suggest-boundaries entry b)) (t1 (real-time)))
                    (list r (- t1 t0))))))
       (pairs (let loop ((k 0) (out '()))
                (if (= k 3) (reverse out) (let* ((r1 (timed t1)) (r4 (timed t4))) (loop (+ k 1) (cons (cons r1 r4) out))))))
       (median (lambda (xs) (list-ref (list-sort < xs) 1)))
       (m1 (median (map (lambda (p) (cadr (car p))) pairs)))
       (m4 (median (map (lambda (p) (cadr (cdr p))) pairs))))
  (for-each (lambda (p) (printf "  A8 times: N ~a ms, 4N ~a ms\n" (cadr (car p)) (cadr (cdr p)))) pairs)
  (printf "  A8 medians: N ~a ms, 4N ~a ms\n" m1 m4)
  (want "A8 N and 4N lines of `x = (a) / b / c;`: every pair two blocks at f, the 4N scan's median at most 6 times the N scan's"
        (list (map (lambda (p) (list (car (car p)) (car (cdr p)))) pairs) (<= m4 (* 6 (max 1 m1))))
        (list (let ((one (list (list (list 0 (off t1 "function f")) '()) (list (list 0 (off t4 "function f")) '()))))
                (list one one one))
              #t)))
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
