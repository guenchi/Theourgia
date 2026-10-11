#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce) (theourgia ffi)
        (theourgia languages) (theourgia code-project) (theourgia code-markers)
        (only (theourgia code-suggest) suggest-boundaries))
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
  '(("js" "function a() {\nfunction b() {}\n")
    ("js" "function a() { return /x/; }\nfunction b() {}\n")
    ("js" "function a() { return `x`; }\nfunction b() {}\n")
    ("py" "def a():\n  x = \"unterminated\ndef b():\n  pass\n")
    ("ss" "(define a #;ignored 1)\n(define b 2)\n")
    ("md" "# A\n```\n## B\n")
    ("sh" "a() { cat <<END; }\nb() { :; }\n")))
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
(let ((a "(define a #\;)\n") (a2 "(define a #\\|)\n"))
  (want "CS-3 #\; starts no comment and #\\| no block comment: two boundaries each"
        (list (ss-bounds (string-append a "(define b 2)\n")) (ss-bounds (string-append a2 "(define b 2)\n")))
        (list (list 0 (byte-count a)) (list 0 (byte-count a2)))))
(let ((a "(define a #\\space)\n") (b "(define b #\\x41)\n"))
  (want "CS-4 #\\space, #\\x41 and #\\λ: three boundaries"
        (ss-bounds (string-append a b "(define c #\\λ)\n"))
        (list 0 (byte-count a) (+ (byte-count a) (byte-count b)))))
(let ((a "#\\\\\n"))
  (want "CS-5 the strict symbols path: a #\\\\ line does not continue, and b's start is accepted with no warning"
        (scan "scheme" (string-append a "(define b 2)\n") (list (byte-count a)))
        (list (list 0 (byte-count a)) '())))
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
  (want "CS-8 the chez runner entry cuts CS-1's text as the scheme entry does: (0 N), no warning"
        (list (scan "chez" text) (scan "scheme" text))
        (list (list (list 0 (byte-count a)) '()) (list (list 0 (byte-count a)) '()))))

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
(printf "~a failures\ncode-suggest1 complete\n" bad)
(exit (if (zero? bad) 0 1))
