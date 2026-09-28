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
    language-for-name language-property language-runner runner-valid?)
  ;; The entry accessor moved to (theourgia markers), which
  ;; CANNOT REACH THIS LIBRARY -- it imports (rnrs) and two names from
  ;; (theourgia wire), and nothing else. That, not "it imports nothing",
  ;; is the property the move was made for, and `test/closures.sc` reads
  ;; it off the import graph. The names come back here unchanged, so no
  ;; caller of this library changed. See markers.sc for why.
  (import (only (theourgia markers) language-property)
          (rnrs))
  (define catalog
    (vector
      '(((comment-prefixes (";")) (lang "scheme") (extensions ("ss" "sc" "scm" "sls"))
          (line-comment ";;") (block-comment ("#|" "|#"))
          (def-heads
            ("^\\(define\\s+\\(([^\\s()\\[\\]\";]+)"
              "^\\(define\\s+([^\\s()\\[\\]\";]+)"
              "^\\(define-syntax\\s+([^\\s()\\[\\]\";]+)"))
          (name-capture 1)
          (suggest-only
            ((multiline-quotes ("\"")) (top-level "paren") (pairs ("()" "[]" "{}"))
              (quote-delimiters ("\"")) (escaped-character "\\")
              (nested-block-comment #t) (uncertain-tokens ("#;" "#\\"))
              (fallback "whole-file-with-warning") (prefix-lines ())))
          (name-vectors
            ("(define (f x) x)"
              "(define x 1)"
              "(define-syntax m (syntax-rules () ((_ x) x)))")))
         ((lang "javascript") (extensions ("js" "mjs" "cjs")) (line-comment "//")
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
               (uncertain-tokens ("/" "`"))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("function f() {}"
               "class C {}"
               "const inc = (x) => x + 1;")))
         ((lang "typescript") (extensions ("ts" "mts" "cts")) (line-comment "//")
           (block-comment ("/*" "*/"))
           (def-heads
             ("^(?:export\\s+)?interface\\s+([A-Za-z_$][A-Za-z0-9_$]*)"
               "^(?:export\\s+)?type\\s+([A-Za-z_$][A-Za-z0-9_$]*)\\s*="
               "^(?:export\\s+)?function\\s+([A-Za-z_$][A-Za-z0-9_$]*)"))
           (name-capture 1)
           (suggest-only
             ((top-level "brace") (pairs ("()" "[]" "{}")) (quote-delimiters ("\"" "'"))
               (escaped-character "\\") (nested-block-comment #f)
               (uncertain-tokens ("/" "`" "<"))
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("interface Shape {}"
               "type Count = number;"
               "function f(): void {}")))
         ((lang "python") (extensions ("py" "pyw")) (line-comment "#")
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
         ((lang "go") (extensions ("go")) (line-comment "//")
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
         ((lang "rust") (extensions ("rs")) (line-comment "//")
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
         ((lang "c") (extensions ("c" "h")) (line-comment "//")
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
         ((lang "java") (extensions ("java")) (line-comment "//")
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
         ((lang "shell") (extensions ("sh" "bash")) (line-comment "#")
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
               (fallback "whole-file-with-warning") (prefix-lines ())))
           (name-vectors
             ("f() { :; }" "function g { :; }" "function h() { :; }")))
         ((lang "markdown") (extensions ("md" "markdown")) (line-comment #f)
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
           (name-vectors ("# Alpha" "## Beta" "### Gamma"))))))
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
  ;; HOW `eval --lang` RUNS A SOURCE IN THIS LANGUAGE, or #f: an alist of
  ;; exactly (argv <strings>) and (source-name <one path component>). argv
  ;; is a list, never a shell string; "{file}" in it stands for the source's
  ;; absolute path and "{dir}" for the projection directory. A language with
  ;; no runner is answered no-runner.
  (define (language-runner entry) (language-property entry 'runner #f))
  (define (runner-valid? r)
    (and (list? r) (= (length r) 2)
         (for-all (lambda (e) (and (pair? e) (pair? (cdr e)) (null? (cddr e)))) r)
         (let ((argv (assq 'argv r)) (name (assq 'source-name r)))
           (and argv name
                (list? (cadr argv)) (pair? (cadr argv)) (for-all string? (cadr argv))
                (let ((n (cadr name)))
                  (and (string? n) (> (string-length n) 0)
                       (not (member n '("." "..")))
                       (not (memv #\/ (string->list n)))))))))
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
          (language-table))))))
