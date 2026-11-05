#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce) (theourgia ffi)
        (theourgia languages) (theourgia code-project) (theourgia code-markers)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia landing-files) file-pieces)
        (only (theourgia extensions) extension-verbs))
(define bad 0)
(define (want name got expected)
  (if (equal? got expected) (printf "ok ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" name got expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/code-suggest-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(rpc-dispatch store '(init) "test")
(define (write! path s)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 s)))))
(define counter 0)
(define (result-value a key)
  (let ((p (and (pair? a) (assq key (filter pair? (cdr a))))))
    (and p (cadr p))))
(define (suggest extension input)
  (set! counter (+ counter 1))
  (let ((path (string-append root "/case-" (number->string counter) "." extension)))
    (write! path input)
    (let* ((before (reduce-applied-cut (open-and-reduce store)))
          (a (rpc-dispatch store (list 'split-suggest path) "test")))
      (want "CT-20 suggestion never writes store events" (reduce-applied-cut (open-and-reduce store)) before)
      (want "CT-20 suggestion leaves its original input intact" (read-code-bytes path) (string->utf8 input)) a)))
(define (boundaries a) (result-value a 'boundaries))
(define (byte-count s) (bytevector-length (string->utf8 s)))
(for-each
  (lambda (row)
    (let* ((name (car row)) (ext (cadr row)) (outer (caddr row)) (tail (cadddr row))
           (a (suggest ext (string-append outer tail))))
      (want (string-append "CT-03 " name " nested head stays in outer block") (boundaries a) (list 0 (byte-count outer)))
      (let ((path (result-value a 'working-path)))
        (when path
          (want "CT-20 review copy recovers exact source bytes"
                (apply append (map (lambda (e) (bytevector->u8-list (cadr e)))
                                   (cadr (projection-decode (language-for-path path) (read-code-bytes path)))))
                (bytevector->u8-list (string->utf8 (string-append outer tail))))))))
  '(("javascript" "js" "function outer() {\nfunction inner() {}\n}\n" "function next() {}\n")
    ("python" "py" "class C:\n  def method(self):\n    pass\n" "def next():\n  pass\n")
    ("go" "go" "func outer() {\nfunc inner() {}\n}\n" "func next() {}\n")
    ("rust" "rs" "impl Thing {\nfn inner() {}\n}\n" "fn next() {}\n")
    ("scheme" "ss" "(define (outer)\n(define (inner) 1)\n inner)\n" "(define (next) 2)\n")
    ("markdown" "md" "# Outside\n```scheme\n## Inside\n```\n" "## Next\n")))
(let* ((first "def first():\n  pass\n") (second "# docs\n@decorator\ndef second():\n  pass\n"))
  (want "CT-03 comments and decorators attach to next Python definition"
        (boundaries (suggest "py" (string-append first second))) (list 0 (byte-count first))))
(for-each
  (lambda (row)
    (let* ((a (suggest (car row) (cadr row))))
      (want "CT-04 ambiguous or unbalanced syntax falls back to whole file" (boundaries a) '(0))
      (want "CT-04 fallback carries an explicit warning" (pair? (result-value a 'warnings)) #t)))
  ;; THE REGEX AND THE TEMPLATE IN A JAVASCRIPT FUNCTION NO LONGER FALL BACK:
  ;; they moved to ecmascript-scan.sc's E4, which cuts them. JavaScript keeps a
  ;; fallback row here by a lone CR, which the ecmascript scan cannot follow.
  '(("js" "function a() {\nfunction b() {}\n")
    ("js" "function a() {}\r function b() {}\n")
    ("py" "def a():\n  x = \"unterminated\ndef b():\n  pass\n")
    ("ss" "(define a #;ignored 1)\n(define b 2)\n")
    ;; the shell row that stood here, a heredoc in a function, is cut by the
    ;; line scan now (LS-1b); its lexical fallback is pinned by LS-11
    ("md" "# A\n```\n## B\n")))
(let* ((first "def a():\n  x = \"\"\"\ndef fake():\n  pass\n\"\"\"\n") (next "def b():\n  pass\n"))
  (want "CT-03 triple-quoted Python string hides definition text"
        (boundaries (suggest "py" (string-append first next))) (list 0 (byte-count first))))
(let* ((first "func a() { x := `\nfunc fake() {}\n` }\n") (next "func b() {}\n"))
  (want "CT-03 raw Go string hides definition text"
        (boundaries (suggest "go" (string-append first next))) (list 0 (byte-count first))))
(let* ((first "function a() { /* }\nfunction fake() {}\n*/ return \"}\"; }\n") (next "function b() {}\n"))
  (want "CT-03 comments and quotes suspend delimiter counting"
        (boundaries (suggest "js" (string-append first next))) (list 0 (byte-count first))))
(want "CT-02 unknown language remains one block" (boundaries (suggest "toy" "thing First {}\nthing Second {}\n")) '(0))
(register-language! '((lang "toy") (extensions ("toy")) (line-comment "//")
                      (def-heads ("^thing ([A-Za-z]+)")) (name-capture 1)
                      (suggest-only ((top-level "brace") (pairs ("{}")) (quote-delimiters ("\""))
                                     (uncertain-tokens ()) (prefix-lines ())))))
(want "CT-02 registered data row activates existing profile"
      (boundaries (suggest "toy" "thing First {}\nthing Second {}\n")) '(0 15))
;; A FILE THAT BEGINS WITH A BYTE-ORDER MARK IS TEXT, and its offsets count the
;; mark: they are into the file as it is on disk, so every offset after 0 is
;; the plain file's plus the mark's three bytes. It used to answer invalid-utf8,
;; because the whole-file text test decoded the mark away and the re-encoding
;; did not give it back.
(let* ((plain "function a() {}\nfunction b() {}\n")
       (marked (string-append "\xFEFF;" plain))
       (p (suggest "js" plain))
       (m (suggest "js" marked)))
  (want "CT-21 a JavaScript file beginning with a byte-order mark is cut as the plain one, every offset after 0 moved by the mark's 3 bytes, with no warning"
        (list (boundaries p) (boundaries m) (result-value m 'warnings))
        (list (list 0 (byte-count "function a() {}\n")) (list 0 (+ 3 (byte-count "function a() {}\n"))) '())))
(let* ((head "class A {\n  String s = \"")
       (plain (string-append head "\\u0041\";\n}\nclass B {}\n"))
       (marked (string-append "\xFEFF;" plain))
       (p (suggest "java" plain))
       (m (suggest "java" marked)))
  (want "CT-21 an uncertain token in a file beginning with a byte-order mark is reported at its offset on disk: the plain file's plus 3"
        (list (result-value p 'warnings) (result-value m 'warnings))
        (list (list (list 'code 'lexically-uncertain 'byte-offset (byte-count head)))
              (list (list 'code 'lexically-uncertain 'byte-offset (+ 3 (byte-count head)))))))
;; ---- Scheme character literals and library bodies --------------------------
;;
;; A CHARACTER LITERAL IS READ BY THE ESCAPE: `#` is code, and `\` takes the
;; character after it as code, so `#\(` opens nothing, `#\"` no string, `#\;`
;; no comment. Until this the Scheme profile listed `#\` as uncertain and the
;; whole file was one block. A DATUM COMMENT `#;` still is.
(define (ss-bounds text) (boundaries (suggest "ss" text)))
(define (ss-warnings text) (result-value (suggest "ss" text) 'warnings))
;; the scanner itself, for an entry and, when given, the strict symbols path
(define (scan lang text . starts)
  (if (pair? starts)
      (suggest-boundaries (language-for-name lang) (string->utf8 text) (car starts) #t)
      (suggest-boundaries (language-for-name lang) (string->utf8 text))))
(let ((a "(define a #\\( )\n"))
  (want "CS-1 a #\\( literal: two boundaries, no warning"
        (list (ss-bounds (string-append a "(define b 2)\n")) (ss-warnings (string-append a "(define b 2)\n")))
        (list (list 0 (byte-count a)) '())))
(let ((a "(define a #\\\")\n"))
  (want "CS-2 a #\\\" literal opens no string: two boundaries"
        (ss-bounds (string-append a "(define b 2)\n")) (list 0 (byte-count a))))
(let ((a "(define a #\\;)\n") (a2 "(define a #\\|)\n"))
  (want "CS-3 #\\; starts no comment and #\\| no block comment: two boundaries each"
        (list (ss-bounds (string-append a "(define b 2)\n")) (ss-bounds (string-append a2 "(define b 2)\n")))
        (list (list 0 (byte-count a)) (list 0 (byte-count a2)))))
(let ((a "(define a #\\space)\n") (b "(define b #\\x41)\n"))
  (want "CS-4 #\\space, #\\x41 and #\\λ: three boundaries"
        (ss-bounds (string-append a b "(define c #\\λ)\n"))
        (list 0 (byte-count a) (+ (byte-count a) (byte-count b)))))
(let ((a "#\\\\\n"))
  (want "CS-5 the strict symbols path: a #\\\\ line does not continue, and b's start is accepted with no warning, code beginning on both lines"
        (scan "scheme" (string-append a "(define b 2)\n") (list (byte-count a)))
        (list (list 0 (byte-count a)) '() (list 0 (byte-count a)))))
(let ((a "(define a #\\(x)\n"))
  (want "CS-5b #\\( is one character: x) closes the define, two boundaries"
        (ss-bounds (string-append a "(define b 2)\n")) (list 0 (byte-count a))))
(let ((a "(define a #\\\n)\n") (open "(define a #\\"))
  (want "CS-5c a literal prefix at a line end continues the form, the next line closes it and b is a cut; at the end of the file it is unbalanced"
        (list (ss-bounds (string-append a "(define b 2)\n"))
              (let ((r (suggest "ss" open))) (list (boundaries r) (result-value r 'warnings))))
        (list (list 0 (byte-count a))
              (list '(0) (list (list 'code 'unbalanced 'byte-offset (byte-count open)))))))
(let ((r (suggest "ss" "(define a #;ignored 1)\n(define b 2)\n")))
  (want "CS-6 PIN a datum comment still makes the whole file one block, with a warning"
        (list (boundaries r) (map cadr (result-value r 'warnings)))
        '((0) (lexically-uncertain))))
(let ((s1 "(define a \"\n(define fake 1)\n\")\n") (c1 "(define a 1)\n") (s2 "(define a \"#\\\\(\")\n"))
  (want "CS-7 PIN an unbalanced file falls back; a define inside a string is hidden; a literal inside a string or a block comment changes nothing"
        (list (map cadr (ss-warnings "(define a (\n(define b 2)\n"))
              (ss-bounds (string-append s1 "(define b 2)\n"))
              (ss-bounds (string-append s2 "(define b 2)\n"))
              (ss-bounds (string-append c1 "#| #\\( |#\n(define b 2)\n")))
        (list '(unbalanced) (list 0 (byte-count s1)) (list 0 (byte-count s2)) (list 0 (byte-count c1)))))
(let* ((a "(define a #\\( )\n") (text (string-append a "(define b 2)\n")))
  (want "CS-8 the chez runner entry cuts CS-1's text as the scheme entry does: (0 N), no warning, code beginning on both lines"
        (list (scan "chez" text) (scan "scheme" text))
        (list (list (list 0 (byte-count a)) '() (list 0 (byte-count a)))
              (list (list 0 (byte-count a)) '() (list 0 (byte-count a))))))

;; A LIBRARY'S BODY: the first opener met with an empty stack is read for its
;; head; `library` designates that frame, and the definitions at its first
;; level are cuts at their line's start, indentation included.
(define lb-head "(library (a)\n  (export f g)\n  (import (rnrs))\n")
(let* ((f "  (define (f) 1)\n") (g "  (define (g) 2))\n")
       (o1 (byte-count lb-head)) (o2 (+ o1 (byte-count f))))
  (want "LB-1 a library's two definitions are cuts at their lines, the name, export and import in the first block"
        (list (ss-bounds (string-append lb-head f g)) (ss-warnings (string-append lb-head f g)))
        (list (list 0 o1 o2) '()))
  (want "LB-5 PIN the strict symbols path keeps depth zero: starts at f and g in a library are not top level"
        (cadr (scan "scheme" (string-append lb-head f g) (list o1 o2)))
        (list (list 'symbol-not-top-level (list 'at o1)) (list 'symbol-not-top-level (list 'at o2)))))
(let* ((f "  (define (f)\n(define (h) 1)\n    (h))\n") (g "  (define (g) 2))\n")
       (o1 (byte-count lb-head)) (o2 (+ o1 (byte-count f))))
  (want "LB-2 a define nested in f's body at column 0 is not a cut: still three boundaries"
        (ss-bounds (string-append lb-head f g)) (list 0 o1 o2)))
(want "LB-3 PIN a define before the library: the first opener's head is define, nothing is designated, f is not a cut"
      (ss-bounds "(define x 1)\n(library (a)\n  (export)\n  (import (rnrs))\n  (define (f) 1))\n")
      '(0))
(let ((before "42\n(library (a)\n  (export)\n  (import (rnrs))\n"))
  (want "LB-3b an atom before the library does not matter: f is a cut"
        (ss-bounds (string-append before "  (define (f) 1))\n")) (list 0 (byte-count before))))
(let* ((f "  (define (f) #\\( )\n") (g "  (define (g) 2))\n")
       (o1 (byte-count lb-head)) (o2 (+ o1 (byte-count f)))
       (one "(library (a) (export) (import (rnrs)) (define (f) #\\( ) (define (g) 2))\n"))
  (want "LB-4 a library on one line is one block with no warning; written over lines with a literal, three boundaries"
        (list (ss-bounds one) (ss-warnings one) (ss-bounds (string-append lb-head f g)))
        (list '(0) '() (list 0 o1 o2))))

;; ---- the line scan: shell and perl, called without starts -------------------
;;
;; AN ENTRY WITH `line-suggest` IS CUT LINE BY LINE WHEN NO STARTS ARE GIVEN
;; (split-suggest without symbols, and landing-files' file-pieces): a
;; definition is a def-head matching the RAW line, from column 0; no bracket,
;; quote or heredoc is followed; a comment run above a definition goes with
;; it and a blank line ends the run; and perl stops at a line that is exactly
;; __END__ or __DATA__, that line and the rest staying in the last block.
;; With starts, the empty list included, the lexical scan is as it was.
;; Every row is red on the tree before the line scan unless it says PIN or
;; says why it is green there.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
;; the byte offset of NEEDLE's first occurrence in TEXT
(define (off text needle)
  (let ((n (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) needle) (byte-count (substring text 0 i)))
            (else (loop (+ i 1)))))))
;; the line starts a line scan reads as code, worked out from the text: every
;; line that is not blank and does not begin, past its indentation, with "#"
(define (code-line-starts text)
  (let loop ((i 0) (out '()))
    (if (>= i (string-length text))
        (reverse out)
        (let* ((e (let f ((j i)) (if (or (= j (string-length text)) (char=? (string-ref text j) #\newline)) j (f (+ j 1)))))
               (line (substring text i e))
               (body (let f ((k 0))
                       (if (and (< k (string-length line)) (char-whitespace? (string-ref line k)))
                           (f (+ k 1))
                           (substring line k (string-length line))))))
          (loop (+ e 1)
                (if (or (string=? body "") (char=? (string-ref body 0) #\#))
                    out
                    (cons (byte-count (substring text 0 i)) out)))))))
(define (scan-raw lang bytes . starts)
  (caught (if (pair? starts)
              (suggest-boundaries (language-for-name lang) bytes (car starts) #t)
              (suggest-boundaries (language-for-name lang) bytes))))
;; the review copy's pieces run together, as bytes
(define (decoded a)
  (caught (let ((path (result-value a 'working-path)))
            (and path (apply append (map (lambda (e) (bytevector->u8-list (cadr e)))
                                         (cadr (projection-decode (language-for-path path) (read-code-bytes path)))))))))
(define (u8s s) (bytevector->u8-list (string->utf8 s)))

(define sh-a "a() {\n  cat <<EOF\nsome text\nEOF\n}\n")
(define sh-b "b() {\n  y=$(date)\n  case $y in\n    *) echo \"$y\" ;;\n  esac\n}\n")
(define sh-c "function c {\n  for i in 1 2; do echo `pwd`; done\n}\n")
(define sh1 (string-append "x=1\n" sh-a sh-b sh-c))
(want "LS-1 a shell file with a heredoc, a $(...), a case and a backtick cuts at its three functions, no warning, every non-blank non-comment line a code line"
      (scan-raw "shell" (string->utf8 sh1))
      (list (list 0 (off sh1 "a()") (off sh1 "b()") (off sh1 "function c")) '() (code-line-starts sh1)))
(let ((a (suggest "sh" sh1)))
  (want "LS-1 the same through split-suggest: the cuts, no warning, and the review copy's pieces run together are the file"
        (list (boundaries a) (result-value a 'warnings) (equal? (decoded a) (u8s sh1)))
        (list (list 0 (off sh1 "a()") (off sh1 "b()") (off sh1 "function c")) '() #t)))
(want "LS-1b the shell row CT-04 used to pin, a heredoc opened in a one-line function, is cut at b"
      (boundaries (suggest "sh" "a() { cat <<END; }\nb() { :; }\n"))
      (list 0 (byte-count "a() { cat <<END; }\n")))
(want "LS-2 an indented `  f() {` is not a cut: the def-head is matched from column 0"
      (boundaries (suggest "sh" "a() {\n  :\n}\n  f() {\n    :\n  }\n"))
      '(0))
(let ((t "a() {\n  cat <<EOF\nfake() {\nEOF\n}\nb() {\n  :\n}\n"))
  (want "LS-3 the stated limit, held to: a function head at column 0 inside a heredoc is a cut"
        (boundaries (suggest "sh" t))
        (list 0 (off t "fake()") (off t "b()"))))
(let ((t "a() {\n  :\n}\n# about b\n# more\nb() {\n  :\n}\n# loose\n\nc() {\n  :\n}\n"))
  (want "LS-4 PIN the comment run above b goes with b; a blank line between a comment and c ends the run (the lexical scan cut this text the same way)"
        (boundaries (suggest "sh" t))
        (list 0 (off t "# about b") (off t "c()"))))

(define pl
  (string-append "use strict;\n" "package Foo;\n" "\n" "# the first\n"
                 "sub one {\n  my $t = <<END;\n  text\nEND\n  return $t;\n}\n"
                 "sub Foo::two {\n  return 2;\n}\n" "sub three { 3 }\n" "sub four;\n"
                 "my $sub = sub { 1 };\n" "  sub inner { 0 }\n"
                 "__END__\n" "sub after { 9 }\n"))
(define pl-cuts
  (list 0 (off pl "package Foo;") (off pl "# the first") (off pl "sub Foo::two") (off pl "sub three") (off pl "sub four;")))
(want "LS-5 a perl file cuts at the package, the four subs (one with its comment, a qualified one, a one-line one, a forward declaration), not at `my $sub = sub`, an indented sub or anything after __END__; the code lines stop before __END__"
      (scan-raw "perl" (string->utf8 pl))
      (list pl-cuts '() (code-line-starts (substring pl 0 (off pl "__END__")))))
(let ((a (suggest "pl" pl)))
  (want "LS-5 the same through split-suggest, the review copy's pieces run together are the file, its tail in the last piece"
        (list (boundaries a) (result-value a 'warnings) (equal? (decoded a) (u8s pl)))
        (list pl-cuts '() #t)))
(want "LS-5 file-pieces cuts the perl file as split-suggest does: six pieces, the last from `sub four;` to the end, __END__ and its tail included"
      (caught (map utf8->string (vector->list (file-pieces "x.pl" (string->utf8 pl)))))
      (let loop ((cs pl-cuts) (out '()))
        (if (null? cs)
            (reverse out)
            (loop (cdr cs) (cons (substring pl (car cs) (if (pair? (cdr cs)) (cadr cs) (string-length pl))) out)))))
(want "LS-12 file-pieces cuts a new shell file of three functions into three pieces that run together to the file"
      (caught (map utf8->string (vector->list (file-pieces "x.sh" (string->utf8 (string-append sh-a sh-b sh-c))))))
      (list sh-a sh-b sh-c))

;; THE STOP MARKER: a line that, less one trailing CR, is exactly __END__ or
;; __DATA__. The scan stops at it and the marker and the rest stay in the
;; block begun before it. Each text has a real head before the marker and,
;; where there can be one, a fake head and ordinary code after it, so a scan
;; that read on would answer another cut and other code lines.
(define after-marker "sub fake { 3 }\nmy $x = 1;\n")
(for-each
  (lambda (row)
    (let* ((name (car row)) (t (cadr row)) (a (suggest "pl" t)))
      (want (string-append "LS-6 " name ": the cuts, no warning and the code lines, which stop at the marker")
            (scan-raw "perl" (string->utf8 t))
            (list (caddr row) '() (cadddr row)))
      (want (string-append "LS-6 " name ": through split-suggest the same cuts, and the review copy's pieces run together are the file")
            (list (boundaries a) (equal? (decoded a) (u8s t)))
            (list (caddr row) #t))))
  (list (list "a marker on the first line makes the file one block"
              (string-append "__END__\n" after-marker) '(0) '())
        (list "a marker with CRLF"
              "sub a { 1 }\r\nsub b { 2 }\r\n__END__\r\nsub fake { 3 }\r\nmy $x = 1;\r\n" '(0 13) '(0 13))
        (list "a marker on the last line, with no newline" "sub a { 1 }\nsub b { 2 }\n__END__" '(0 12) '(0 12))
        (list "a marker after a blank line"
              (string-append "sub a { 1 }\nsub b { 2 }\n\n__END__\n" after-marker) '(0 12) '(0 12))
        (list "a __DATA__ marker"
              (string-append "sub a { 1 }\nsub b { 2 }\n__DATA__\n" after-marker) '(0 12) '(0 12))
        (list "a __DATA__ marker with CRLF"
              "sub a { 1 }\r\nsub b { 2 }\r\n__DATA__\r\nsub fake { 3 }\r\nmy $x = 1;\r\n" '(0 13) '(0 13))
        (list "a comment run directly above the marker invents no cut at it"
              (string-append "sub a { 1 }\nsub b { 2 }\n# the data\n__END__\n" after-marker) '(0 12) '(0 12))))
(define near-misses '(" __END__" "__END__ " "__END__ # x" "__END__;" " __DATA__" "__DATA__;"))
(want "LS-7 near misses are not stops: a leading blank, a trailing blank, a trailing comment, a trailing semicolon; the head after each is a cut"
      (map (lambda (nm) (boundaries (suggest "pl" (string-append "sub a { 1 }\n" nm "\nsub b { 2 }\n"))))
           near-misses)
      (map (lambda (nm) (list 0 (byte-count (string-append "sub a { 1 }\n" nm "\n")))) near-misses))
(let ((t "\xFEFF;#!/usr/bin/perl\nsub f { 'caf\xE9; \x2615;' }\nsub g { 2 }\n# \xFC;ber h\nsub h { 3 }\n"))
  (want "LS-9 setup: the text's byte offsets, the mark, the shebang and the multibyte text counted (f at 19, g at 41, h's comment at 53, h at 63)"
        (list (off t "sub f") (off t "sub g") (off t "# ") (off t "sub h"))
        '(19 41 53 63))
  (want "LS-9 a byte-order mark, a shebang, then `sub f` on the next line: the prefix stays with the first block; g and h (with its comment) are cuts at their offsets on disk, no warning"
        (scan-raw "perl" (string->utf8 t))
        (list (list 0 41 53) '() (list 19 41 63))))
(want "LS-10 a perl file that is not UTF-8 falls back invalid-utf8, as every scan does"
      (scan-raw "perl" (u8-list->bytevector (append (u8s "sub a { 1 }\n") '(255 10) (u8s "sub b { 2 }\n"))))
      '((0) ((code invalid-utf8 byte-offset 0)) ()))
;; THE DISPATCH IS ON STARTS BEING #f: an explicit empty list is a list of
;; starts, strict or not, and takes the lexical scan.
(want "LS-11 PIN with starts, even the empty list, strict and not, a shell file is the lexical scan's: LS-1's file falls back at its heredoc"
      (list (caught (suggest-boundaries (language-for-name "shell") (string->utf8 sh1) '() #t))
            (caught (suggest-boundaries (language-for-name "shell") (string->utf8 sh1) '() #f))
            (caught (suggest-boundaries (language-for-name "shell") (string->utf8 sh1) '())))
      (let ((lexical (list '(0) (list (list 'code 'lexically-uncertain 'byte-offset (off sh1 "<<"))) '())))
        (list lexical lexical lexical)))
(want "LS-11 the same file with starts #f, given explicitly, is the line scan's: three cuts"
      (car (caught (suggest-boundaries (language-for-name "shell") (string->utf8 sh1) #f)))
      (list 0 (off sh1 "a()") (off sh1 "b()") (off sh1 "function c")))

;; A CR BEFORE THE CR OF A CRLF IS THE LINE'S OWN: "__END__\r" is not a marker.
(let ((t "sub a { 1 }\n__END__\r\r\nsub b { 2 }\n"))
  (want "LS-7 a marker line ending CR CR LF is not a stop: b after it is a cut"
        (car (scan-raw "perl" (string->utf8 t)))
        (list 0 (off t "sub b"))))

;; A LINE THAT BEGINS WITH U+FEFF. Inside a file the mark is a character, and
;; the decoder that reads a line keeps it: every scan answers its three
;; elements, the marked line is not a head (its first character is the mark),
;; and the cut after it is at its offset on disk. Each scan raised before.
(define (marked-bytes . parts)
  (u8-list->bytevector (apply append (map (lambda (p) (if (string? p) (u8s p) p)) parts))))
(define mark '(239 187 191))
(want "LS-13 the line scan: a perl line beginning with the mark is read, not a cut; c is a cut at 27"
      (scan-raw "perl" (marked-bytes "sub a { 1 }\n" mark "sub b { 2 }\nsub c { 3 }\n"))
      '((0 27) () (0 12 27)))
(want "LS-13 the table scan: a scheme line beginning with the mark is read, not a cut; c is a cut at 29"
      (scan-raw "scheme" (marked-bytes "(define a 1)\n" mark "(define b 2)\n(define c 3)\n"))
      '((0 29) () (0 13 29)))
(want "LS-13 the ecmascript scan: a javascript line beginning with the mark is read, not a cut; c is a cut at 35"
      (scan-raw "javascript" (marked-bytes "function a() {}\n" mark "function b() {}\nfunction c() {}\n"))
      '((0 35) () (0 16 35)))

;; AN UNBALANCED CLOSER IS REPORTED AT ITS BYTE OFFSET, the text before it on
;; its line counted in bytes, as every other offset the scan reports is.
(want "LS-14 a closer with no opener after an e-acute on its line is unbalanced at its byte offset, 15"
      (scan-raw "scheme" (string->utf8 "(define a 1)\n\xE9;)\n"))
      '((0) ((code unbalanced byte-offset 15)) ()))
(want "LS-14 a closer with no opener after a mark on its line is unbalanced at its byte offset, 16"
      (scan-raw "scheme" (marked-bytes "(define a 1)\n" mark ")\n"))
      '((0) ((code unbalanced byte-offset 16)) ()))

;; ---- names and uses after the split: the whole-text token scan ------------
;;
;; A split shell or perl file's pieces are text blocks with a language and no
;; reader: names, uses and context's users read them by the entry's
;; identifier pattern over the whole text, comments and strings included.
(register-verbs! extension-verbs)
(define ls-store (string-append root "/ls-store"))
(rpc-dispatch ls-store '(init) "test")
(define (run . args) (rpc-dispatch ls-store args "test"))
(define ls-dir (string-append root "/ls"))
(mkdir-p! (string-append ls-dir "/src"))
(mkdir-p! (string-append ls-dir "/split"))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (items a) (let ((c (and (pair? a) (eq? (car a) 'ok) (clause a 'items)))) (if c (cdr c) '())))
(for-each
  (lambda (f)
    (write! (string-append ls-dir "/src/" (car f)) (cdr f))
    (run 'split-suggest (string-append ls-dir "/src/" (car f)) "--output" (string-append ls-dir "/split/" (car f))))
  (list (cons "u.sh" "f1() {\n  echo one\n}\nf2() {\n  # see f3\n  f1\n}\nf3() {\n  :\n}\n")
        (cons "u.pl" "sub Foo::bar {\n  return 1;\n}\nsub q {\n  Foo::bar();\n}\nsub u {\n  bar();\n}\n")))
(define ls-import (run 'import-code (string-append ls-dir "/split")))
(define (src-of S id)
  (let* ((r (state-read S id)) (f (and r (assq 'fields r))) (s (and f (assq 'src (cdr f)))))
    (and s (bytevector? (cdr s)) (utf8->string (cdr s)))))
(define (contains? text needle) (and text (off text needle) #t))
;; the code block whose text begins with HEAD
(define (piece head)
  (let ((S (open-and-reduce ls-store)))
    (find (lambda (id) (let ((t (src-of S id))) (and t (>= (string-length t) (string-length head))
                                                   (string=? (substring t 0 (string-length head)) head))))
          (state-block-ids S))))
(define f1 (piece "f1()")) (define f2 (piece "f2()")) (define f3 (piece "f3()"))
(define pbar (piece "sub Foo::bar")) (define pq (piece "sub q")) (define pu (piece "sub u"))
(define (use-ids name) (map cadr (filter (lambda (i) (and (pair? i) (eq? (car i) 'use))) (items (run 'uses name)))))
(define (user-ids id)
  (let ((u (clause (run 'context "--for" id "--budget" "6000") 'users)))
    (if u (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) 'user))) (cdr u))) '())))
(define (distinct xs) (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))
(want "LS-N SETUP the two files were imported as six distinct pieces, each found by its head (the shell file was cut by its brace profile before the line scan too; the perl file was one block)"
      (list (car ls-import) (for-all string? (list f1 f2 f3 pbar pq pu)) (length (distinct (list f1 f2 f3 pbar pq pu))))
      (list 'ok #t 6))
(want "LS-N PIN names of f2 has f1 (its call) and f3 (a comment's mention, the table's known over-count)"
      (let ((ns (map cadr (filter (lambda (i) (and (pair? i) (eq? (car i) 'name))) (items (run 'names f2))))))
        (list (and (memq 'f1 ns) #t) (and (memq 'f3 ns) #t)))
      '(#t #t))
(want "LS-N PIN uses f1 has f2's piece; uses f3 has f2's too (the comment counts)"
      (list (and (member f2 (use-ids "f1")) #t) (and (member f2 (use-ids "f3")) #t))
      '(#t #t))
(want "LS-N PIN context for f1 lists f2 as a user"
      (and (member f2 (user-ids f1)) #t)
      #t)
(want "LS-N a qualified call is a use of the qualified sub: uses Foo::bar has q, not u, whose bar() is unqualified (no namespace resolution)"
      (let ((us (use-ids "Foo::bar"))) (list (and (member pq us) #t) (and (member pu us) #t)))
      '(#t #f))
(want "LS-N context for Foo::bar lists q as a user and not u"
      (let ((us (user-ids pbar))) (list (and (member pq us) #t) (and (member pu us) #t)))
      '(#t #f))
(want "LS-N whereis finds a sub by its captured name, Foo::bar and q"
      (map (lambda (n) (map cadr (filter (lambda (i) (and (pair? i) (eq? (car i) 'def))) (items (run 'whereis n)))))
           '("Foo::bar" "q"))
      (list (list pbar) (list pq)))
(printf "~a failures\ncode-suggest1 complete\n" bad)
(exit (if (zero? bad) 0 1))
