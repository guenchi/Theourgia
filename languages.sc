#!r6rs
;; Copyright 2026 guenchi
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
(library (theourgia languages)
  (export language-table register-language! language-for-path
    language-for-name language-property language-runner runner-valid?
    override-valid? runner-problem runner-with-override checked-catalogue
    language-catalogue-generation)
  ;; The entry accessor moved to (theourgia markers), which
  ;; CANNOT REACH THIS LIBRARY -- it imports (rnrs) and two names from
  ;; (theourgia wire), and nothing else. That, not "it imports nothing",
  ;; is the property the move was made for, and `test/closures.sc` reads
  ;; it off the import graph. The names come back here unchanged, so no
  ;; caller of this library changed. See markers.sc for why.
  (import (only (theourgia markers) language-property)
          (rnrs))
  ;; HOW `eval --lang` RUNS A SOURCE IN THIS LANGUAGE, or #f: an alist of
  ;; (argv <strings>), (source-name <one path component>) and, optionally,
  ;; (env ((<name> <value>) ...)). argv is a list, never a shell string;
  ;; "{file}" in it stands for the source's absolute path, "{dir}" for the
  ;; projection directory and "{libdirs}" for the launcher's own library
  ;; path, each as a whole argument. env's pairs are
  ;; added to the interpreter's environment after it is cleared to PATH,
  ;; HOME and LANG, so none of those three can be named; in a value
  ;; "{file}", "{dir}" and "{libdirs}" (the launcher's own library path) are
  ;; replaced wherever they occur. A language with no runner is answered
  ;; no-runner.
  (define (language-runner entry) (language-property entry 'runner #f))

  ;; NEVER: NO NUL IN ANYTHING THAT CROSSES THE EXEC. The interpreter's
  ;; argv and environment are handed over as C strings, which end at the
  ;; first NUL, so a string holding one would reach the interpreter as a
  ;; different, shorter string.
  (define (nul-free? s) (not (memv #\nul (string->list s))))
  ;; argv[0] is the interpreter: an empty name would be looked up as
  ;; nothing and answered interpreter-missing, far from its cause.
  (define (runner-argv-valid? a)
    (and (list? a) (pair? a) (for-all string? a) (for-all nul-free? a)
         (> (string-length (car a)) 0)))
  (define (runner-source-name-valid? n)
    (and (string? n) (> (string-length n) 0)
         (not (member n '("." "..")))
         (not (memv #\/ (string->list n)))
         (nul-free? n)))
  (define (runner-env-valid? e)
    (and (list? e)
         (for-all
           (lambda (p)
             (and (list? p) (= (length p) 2) (string? (car p)) (string? (cadr p))
                  (let ((name (car p)))
                    (and (> (string-length name) 0)
                         (not (memv #\= (string->list name)))
                         (nul-free? name)
                         (not (member name '("PATH" "HOME" "LANG")))))
                  (nul-free? (cadr p))))
           e)))
  (define runner-field-checks
    (list (cons 'argv runner-argv-valid?)
          (cons 'source-name runner-source-name-valid?)
          (cons 'env runner-env-valid?)))

  ;; -> #f when `r` is a runner of this shape holding every field in
  ;; `required`, else the name of what is wrong: `runner` when it is not a
  ;; list of (<field> <value>), otherwise the first field that is unknown,
  ;; repeated, missing or whose value its check refuses.
  (define (runner-problem r required)
    (if (not (and (list? r)
                  (for-all (lambda (e) (and (list? e) (= (length e) 2) (symbol? (car e)))) r)))
        'runner
        (let ((keys (map car r)))
          (cond
            ((find (lambda (k) (not (assq k runner-field-checks))) keys) => (lambda (k) k))
            ((let repeated ((ks keys))
               (and (pair? ks) (if (memq (car ks) (cdr ks)) (car ks) (repeated (cdr ks)))))
             => (lambda (k) k))
            ((find (lambda (k) (not (assq k r))) required) => (lambda (k) k))
            ((find (lambda (e) (not ((cdr (assq (car e) runner-field-checks)) (cadr e)))) r)
             => (lambda (e) (car e)))
            (else #f)))))
  (define (runner-valid? r) (not (runner-problem r '(argv source-name))))
  ;; AN OPERATOR'S OVERRIDE names any non-empty set of the fields, each held
  ;; to the same check as the table's.
  (define (override-valid? r) (and (pair? r) (not (runner-problem r '()))))

  ;; THE TABLE'S RUNNER WITH AN OVERRIDE IN PLACE, FIELD BY FIELD: a field
  ;; the override names replaces the table's whole -- env included, whose
  ;; pairs are not merged one by one, so (env ()) empties it -- and the
  ;; table's other fields stay.
  (define (runner-with-override runner override)
    (append (map (lambda (f) (or (assq (car f) override) f)) runner)
            (filter (lambda (f) (not (assq (car f) runner))) override)))

  ;; NEVER: THE BUILT-IN TABLE IS HELD TO THE CHECK register-language!
  ;; APPLIES. The table is built through this constructor, so an entry
  ;; whose runner the check would refuse stops the library's load with the
  ;; entry and the field named, instead of running as a runner the table
  ;; could never have accepted.
  (define (checked-catalogue entries)
    (for-each
      (lambda (e)
        (let ((r (language-runner e)))
          (when (and r (not (runner-valid? r)))
            (assertion-violation 'checked-catalogue
              "Invalid runner in the language table"
              (language-property e 'lang #f)
              (runner-problem r '(argv source-name))))))
      entries)
    (vector entries))

  (define catalog
    (checked-catalogue
      '(((comment-prefixes (";")) (lang "scheme") (extensions ("ss" "sc" "scm" "sls")) (identifier "[A-Za-z!$%&*/:<=>?^_~+.@\\-][A-Za-z0-9!$%&*/:<=>?^_~+.@\\-]*")
          (line-comment ";;") (block-comment ("#|" "|#"))
          ;; THE SOURCE IS READ AS FORMS by reader.sc for name use, and a text
          ;; file's first form is its header (its library and imports): the
          ;; only entries with a reader and a header
          (reader #t) (header #t)
          (def-heads
            ("^\\(define\\s+\\(([^\\s()\\[\\]\";]+)"
              "^\\(define\\s+([^\\s()\\[\\]\";]+)"
              "^\\(define-syntax\\s+([^\\s()\\[\\]\";]+)"))
          (name-capture 1)
          (suggest-only
            ((multiline-quotes ("\"")) (top-level "paren") (pairs ("()" "[]" "{}"))
              (quote-delimiters ("\"")) (escaped-character "\\")
              ;; a character literal is read by the escape: `#` and then `\`
              ;; with the character after it, both code (code-suggest.sc); a
              ;; datum comment still cannot be followed
              (nested-block-comment #t) (uncertain-tokens ("#;"))
              ;; a library's definitions are cuts inside its one form
              (body-forms ("library"))
              ;; no operator continues a line here: "-", "+", "." and the rest
              ;; are identifier characters, and an open bracket is the stack's
              (continuation-tokens ())
              (fallback "whole-file-with-warning") (prefix-lines ())))
          (name-vectors
            ("(define (f x) x)"
              "(define x 1)"
              "(define-syntax m (syntax-rules () ((_ x) x)))")))
         ((lang "javascript") (extensions ("js" "mjs" "cjs")) (identifier "[A-Za-z_$][A-Za-z0-9_$]*") (line-comment "//")
           (runner ((argv ("node" "{file}")) (source-name "__eval.mjs")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:export\\s+)?(?:async\\s+)?function\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?class\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?const\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*(?:\\([^)]*\\)|([A-Za-z_$][A-Za-z0-9_$]*))\\s*=>"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               ;; the ecmascript scan (code-suggest.sc): every live reading of
               ;; "/" is kept, templates are followed, and what it does not
               ;; follow refuses the file: Annex B's HTML-like comments, and JSX
               ;; (by its opener and its self-close: code-suggest.sc)
               (lexer "ecmascript") (annex-b-comments #t)
               (uncertain-tokens ())
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("function f() {}"
               "class C {}"
               "const inc = (x) => x + 1;")))
         ((lang "typescript") (extensions ("ts" "mts" "cts")) (identifier "[A-Za-z_$][A-Za-z0-9_$]*") (line-comment "//")
           (runner ((argv ("node" "{file}")) (source-name "__eval.mts")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:export\\s+)?interface\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?type\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*="
               "^(?:export\\s+)?function\\s+([A-Za-z_$][A-Za-z0-9_$]*)"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               ;; the ecmascript scan with TypeScript's type syntax: a run of
               ;; ">" and a postfix "!" can end an expression, and `void` is
               ;; also a type; "<" and ">" are no pairs
               (lexer "ecmascript") (type-syntax #t)
               (uncertain-tokens ())
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("interface Shape {}"
               "type Count = number;"
               "function f(): void {}")))
         ((lang "typescriptreact") (extensions ("tsx")) (identifier "[A-Za-z_$][A-Za-z0-9_$]*") (line-comment "//")
           ;; no runner: eval --lang answers no-runner, as for markdown
           (block-comment ("/*" "*/"))
           ;; typescript's interface and type heads, then a function head that
           ;; also takes `export default` and `async` (a component is
           ;; `export default function Screen(`), and javascript's class and
           ;; const-arrow heads
           (def-heads
             ("^(?:export\\s+)?interface\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?type\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*="
               "^(?:export\\s+(?:default\\s+)?)?(?:async\\s+)?function\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?class\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?const\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*(?:\\([^)]*\\)|([A-Za-z_$][A-Za-z0-9_$]*))\\s*=>"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               ;; the ecmascript scan with TypeScript's type syntax and JSX
               ;; followed (code-suggest.sc): an element where an operand is
               ;; expected is read through its tags, attribute strings,
               ;; children and containers, and no line inside it is a cut. What
               ;; the scan does not follow refuses the file: type parameters or
               ;; a function type where an operand is expected (`<T,>(`,
               ;; `<T extends U>(`, `type F = <T>(x: T) => T`), type arguments
               ;; on a tag name, an element as an attribute's value, ">" or "}"
               ;; as text. The arrangement of a tag's tokens is not checked:
               ;; that is the type checker's business. A module, so no
               ;; HTML-like comments.
               (lexer "ecmascript") (type-syntax #t) (jsx #t) (annex-b-comments #f)
               (uncertain-tokens ())
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("interface Props {}"
               "export default function Screen() {}"
               "const Row = (p) => <li>{p.label}</li>;")))
         ((lang "javascriptreact") (extensions ("jsx")) (identifier "[A-Za-z_$][A-Za-z0-9_$]*") (line-comment "//")
           ;; no runner: eval --lang answers no-runner, as for markdown
           (block-comment ("/*" "*/"))
           ;; javascript's heads, the function head also taking `export default`
           (def-heads
             ("^(?:export\\s+(?:default\\s+)?)?(?:async\\s+)?function\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?class\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?const\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*(?:\\([^)]*\\)|([A-Za-z_$][A-Za-z0-9_$]*))\\s*=>"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               ;; the ecmascript scan with JSX followed (code-suggest.sc), as
               ;; for typescriptreact but without type syntax: an element
               ;; where an operand is expected is followed, and no line inside
               ;; it is a cut; type arguments on a tag name, an element as an
               ;; attribute's value, ">" or "}" as text refuse the file, and the
               ;; arrangement of a tag's tokens is not checked. A module, so no
               ;; HTML-like comments.
               (lexer "ecmascript") (jsx #t) (annex-b-comments #f)
               (uncertain-tokens ())
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("export default function Screen() {}"
               "class Panel {}"
               "const Row = (p) => <li>{p.label}</li>;")))
         ((lang "python") (extensions ("py" "pyw")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "#")
           (runner ((argv ("python3" "{file}")) (source-name "__eval.py")))
           (block-comment #f)
           (def-heads
             ("^(?:async\\s+)?def\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\("
               "^class\\s+([A-Za-z_][A-Za-z0-9_]*)(?:\\s*[:(])"
               "^([A-Za-z_][A-Za-z0-9_]*)\\s*="))
           (name-capture 1)
           (suggest-only
             ((multiline-quotes ("\"\"\"" "'''")) (top-level "indent") (pairs ("()" "[]" "{}"))
               (quote-delimiters ("\"\"\"" "'''" "\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens ("f\"" "f'" "t\"" "t'"))
               (fallback "whole-file-with-warning") (prefix-lines ("^@"))))
           (name-vectors
             ("def f():\n\treturn 1" "class C:\n\tpass" "answer = 42")))
         ;; GO, RUST, C AND JAVA RUN A FILE WITH A STANDARD INSTALL AND NO
         ;; PROJECT: `go run`, the compiler then the binary it wrote beside the
         ;; source (one `sh -c` line, the source as $1), and Java's single-file
         ;; source launch. TypeScript runs under node, which strips a file's
         ;; types from Node 22.18 and 23.6 on. Each is replaced whole by its
         ;; THEOURGIA_RUNNER_<LANG> (eval-runner.sc). Go's source is eval.go,
         ;; not __eval.go: the go tool leaves out a file whose name begins
         ;; with _ or ., one named on its command line included.
         ((lang "go") (extensions ("go")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "//")
           (runner ((argv ("go" "run" "{file}")) (source-name "eval.go")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^func\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\("
               "^type\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+"
               "^func\\s+\\([^)]*\\)\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\("))
           (name-capture 1)
           (suggest-only
             ((multiline-quotes ("`")) (raw-quotes ("`")) (top-level "brace")
               (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'" "`"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens ()) (fallback "whole-file-with-warning")
               (prefix-lines ())))
           (name-vectors
             ("func f() {}" "type Count int" "func (x T) M() {}")))
         ((lang "rust") (extensions ("rs")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "//")
           (runner ((argv ("sh" "-c" "rustc -o \"$1.bin\" \"$1\" && exec \"$1.bin\"" "rust" "{file}"))
                    (source-name "__eval.rs")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:pub\\s+)?(?:async\\s+)?fn\\s+([A-Za-z_][A-Za-z0-9_]*)"
               "^(?:pub\\s+)?struct\\s+([A-Za-z_][A-Za-z0-9_]*)"
               "^impl\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\{"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #t)
               (uncertain-tokens ("r#" "r\"" "'"))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("fn f() {}" "struct Thing {}" "impl Thing {}")))
         ((lang "c") (extensions ("c" "h")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "//")
           (runner ((argv ("sh" "-c" "cc -o \"$1.bin\" \"$1\" && exec \"$1.bin\"" "c" "{file}"))
                    (source-name "__eval.c")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:static\\s+)?int\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\([^;]*\\)\\s*\\{"
               "^(?:static\\s+)?void\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\([^;]*\\)\\s*\\{"
               "^struct\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\{"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens ("#" "\\\n"))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("int f(void) { return 1; }"
               "void g(void) {}"
               "struct Point { int x; };")))
         ((lang "java") (extensions ("java")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "//")
           (runner ((argv ("java" "{file}")) (source-name "__eval.java")))
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:public\\s+)?class\\s+([A-Za-z_][A-Za-z0-9_]*)"
               "^(?:public\\s+)?interface\\s+([A-Za-z_][A-Za-z0-9_]*)"
               "^(?:public\\s+)?enum\\s+([A-Za-z_][A-Za-z0-9_]*)"))
           (name-capture 1)
           (suggest-only
             ((global-uncertain-tokens ("\\u")) (top-level "brace") (pairs ("()" "[]" "{}"))
               (quote-delimiters ("\"" "'")) (escaped-character "\\")
               (nested-block-comment #f)
               (uncertain-tokens ("\\u" "\"\"\""))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("class C {}" "interface I {}" "enum E { A }")))
         ((lang "shell") (extensions ("sh" "bash")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment "#")
           (runner ((argv ("sh" "{file}")) (source-name "__eval.sh")))
           (block-comment #f)
           (def-heads
             ("^([A-Za-z_][A-Za-z0-9_]*)\\s*\\(\\s*\\)\\s*\\{"
               "^function\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\{"
               "^function\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\(\\s*\\)\\s*\\{"))
           (name-capture 1)
           (suggest-only
             ((multiline-quotes ("\"" "'")) (raw-quotes ("'")) (top-level "brace")
               (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens
                 ("<<" "$(" "`" "case " "if " "for " "while "))
               ;; "/", "=", ":" and "*" end ordinary shell words (cd /, x=):
               ;; only these continue a shell line (code-suggest.sc, strict)
               (continuation-tokens ("|" "&&" "||"))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("f() { :; }" "function g { :; }" "function h() { :; }")))
         ((lang "markdown") (extensions ("md" "markdown")) (identifier "[A-Za-z_][A-Za-z0-9_]*") (line-comment #f)
           (block-comment ("<!--" "-->"))
           (def-heads
             ("^#\\s+(.+?)\\s*#*$"
               "^##\\s+(.+?)\\s*#*$"
               "^###\\s+(.+?)\\s*#*$"))
           (name-capture 1)
           (suggest-only
             ((top-level "fence") (pairs ()) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens ()) (fallback "whole-file-with-warning")
               (prefix-lines ())))
           (name-vectors ("# Alpha" "## Beta" "### Gamma")))
         ;; A RUNNER FOR SCHEME SOURCE OUTSIDE THE SANDBOX, over the projection,
         ;; with the libraries the store holds on its path. It claims no file
         ;; extension -- files are Scheme's, and `--lang scheme` stays the
         ;; sandbox -- and it is placed after `scheme`, so a lookup that takes
         ;; the first entry whose comment syntax matches still finds `scheme`.
         ;; Its libraries are searched in the projection first, then where the
         ;; calling process found its own; its extensions put `.sc` first, as
         ;; the repositories it will hold spell their libraries. An operator
         ;; replaces any of its fields with THEOURGIA_RUNNER_CHEZ
         ;; (eval-runner.sc).
         ((comment-prefixes (";")) (lang "chez") (extensions ()) (identifier "[A-Za-z!$%&*/:<=>?^_~+.@\\-][A-Za-z0-9!$%&*/:<=>?^_~+.@\\-]*") (line-comment ";;")
           (block-comment ("#|" "|#"))
           (runner ((argv ("scheme" "--script" "{file}")) (source-name "__eval.ss")
                    (env (("CHEZSCHEMELIBDIRS" "{dir}:{libdirs}")
                          ("CHEZSCHEMELIBEXTS" ".sc:.ss:.sls:.scm")))))
           (def-heads
             ("^\\(define\\s+\\(([^\\s()\\[\\]\";]+)"
               "^\\(define\\s+([^\\s()\\[\\]\";]+)"
               "^\\(define-syntax\\s+([^\\s()\\[\\]\";]+)"))
           (name-capture 1)
           (suggest-only
             ((multiline-quotes ("\"")) (top-level "paren") (pairs ("()" "[]" "{}"))
               (quote-delimiters ("\"")) (escaped-character "\\")
               (nested-block-comment #t) (uncertain-tokens ("#;"))
               (body-forms ("library"))
               ;; as scheme's: no operator continues a line
               (continuation-tokens ())
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("(define (f x) x)"
               "(define x 1)"
               "(define-syntax m (syntax-rules () ((_ x) x)))"))))))
  (define (language-table) (vector-ref catalog 0))

  (define (language-for-name name)
    (find
      (lambda (e)
        (equal?
          (language-property e 'lang #f)
          (if (symbol? name) (symbol->string name) name)))
      (language-table)))
  (define (language-for-path path)
    (let ([extension (let loop ([i (- (string-length path) 1)])
                       (cond
                         [(< i 0) ""]
                         [(char=? (string-ref path i) #\.)
                          (substring path (+ i 1) (string-length path))]
                         [(char=? (string-ref path i) #\/) ""]
                         [else (loop (- i 1))]))])
      (find
        (lambda (e)
          (member extension (language-property e 'extensions '())))
        (language-table))))
  ;; THE CATALOGUE'S GENERATION: how many times an entry was registered in
  ;; this process. What a store answers through the catalogue (definitions
  ;; by their def-heads, a text file's header, name use through a reader) is
  ;; kept under the cut AND this number, so replacing an entry, which writes
  ;; nothing to any store, is seen by every one of those answers.
  (define catalogue-generation 0)
  (define (language-catalogue-generation) catalogue-generation)
  (define (register-language! entry)
    (unless (and (list? entry)
                 (string? (language-property entry 'lang #f))
                 (list? (language-property entry 'extensions #f))
                 (list? (language-property entry 'def-heads #f))
                 (let ((r (language-runner entry))) (or (not r) (runner-valid? r))))
      (assertion-violation 'register-language!
        "Invalid language entry"
        entry))
    (vector-set!
      catalog
      0
      (cons
        entry
        (filter
          (lambda (e)
            (not (equal?
                   (language-property e 'lang #f)
                   (language-property entry 'lang #f))))
          (language-table))))
    (set! catalogue-generation (+ catalogue-generation 1))))
