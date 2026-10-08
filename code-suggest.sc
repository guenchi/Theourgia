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
(library (theourgia code-suggest)
  (export suggest-boundaries split-suggest read-import-symbols import-symbol-cuts)
  (import (rnrs) (theourgia languages) (theourgia text-code) (theourgia regex)
          (only (theourgia markers) mark-length after-mark)
          (theourgia code-project) (theourgia code-markers) (theourgia trace)
          (only (theourgia wire) string->sexpr-extended)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia log) atomic-write! directory-entry-durable!)
          (only (theourgia ffi) process-id wall-clock-ms file-create-exclusive! link! unlink!))

  ;; Profiles are lexical evidence for suggestions, never import authority.
  ;; A state we cannot prove resets the entire suggestion to one block.
  ;;
  ;; AN EDITOR'S SYMBOLS REPLACE THE DEFINITION TEST, NOT THE SCANNER. With
  ;; `starts` (byte offsets of line starts an editor's symbol provider named
  ;; as top-level definitions), a line is a definition when its offset is
  ;; one of them, instead of when a def-head matches it. Everything else is
  ;; the scanner's: the delimiter, quote and comment state, the comment
  ;; lines a definition takes with it (`pending`), and the protected prefix
  ;; that stays with the first block -- so where the two suppliers agree on
  ;; a definition they cut in the same place. A start the scanner does not
  ;; see at the top level (inside a string, a block comment, a fence or an
  ;; open bracket) is not a cut, and is said in the warnings.
  ;; A PROFILE THE SCANNER CAN FOLLOW: the one test for "this language has
  ;; lexical evidence to offer", asked by the scanner and by split-suggest.
  (define (usable-profile? entry)
    (let ((profile (language-property entry 'suggest-only #f)))
      (and profile
           (member (language-property profile 'top-level #f) '("paren" "brace" "indent" "fence"))
           #t)))

  ;; STRICT, the optional argument after `starts`, is the import's test of a
  ;; cut (import-symbol-cuts); split-suggest does not ask it. Two more lines
  ;; are then not at the top level, as the scanner's state reads them:
  ;;   - a line after one whose CODE ends in a continuation token: the
  ;;     profile's `continuation-tokens`, else =>, =, +, -, *, /, ., ",", (,
  ;;     [, {, &&, ||, ?, :, and the profile's escape in either case. A token
  ;;     counts only as a whole operator: not a "+" or "-" that ends "++" or
  ;;     "--", not a "." that ends a number ("1."), not a character the
  ;;     escape makes literal ("\|").
  ;;     The code is the line without its line comment, and a line that ends
  ;;     in a string, or inside or at the close of a block comment, does not
  ;;     continue: "// The end." or "x = 'a:'" before a definition is not
  ;;     the middle of a statement. Not in a fence profile, which is prose.
  ;;   - in an indent profile, a blank line whose next non-blank line is
  ;;     indented: a blank line inside a body has no indentation of its own,
  ;;     and is judged by what follows it. A blank line between two
  ;;     top-level definitions still is the top level. A line of spaces or
  ;;     tabs only is a blank line here.
  (define (suggest-boundaries entry bytes . rest)
    (define starts (and (pair? rest) (car rest)))
    (define strict (and (pair? rest) (pair? (cdr rest)) (cadr rest)))
    (define not-top-level '())
    (define (definition? offset trimmed)
      (if starts (and (memv offset starts) #t) (definition-name entry trimmed)))
    (define (symbol-not-top-level! offset)
      (when (and starts (memv offset starts))
        (set! not-top-level (cons (list 'symbol-not-top-level (list 'at offset)) not-top-level))))
    (call/cc
      (lambda (return)
        (define (fallback reason offset)
          (return (list '(0) (list (list 'code reason 'byte-offset offset)))))
        (let* ((profile (language-property entry 'suggest-only #f))
               (get (lambda (k default) (language-property profile k default)))
               (kind (get 'top-level #f))
               (pairs (get 'pairs '())) (quotes (get 'quote-delimiters '()))
               (multiline (get 'multiline-quotes '())) (raw-quotes (get 'raw-quotes '()))
               (escape (get 'escaped-character "\\"))
               (block (language-property entry 'block-comment #f))
               (uncertain (get 'uncertain-tokens '()))
               (prefix-patterns (map regex-compile (get 'prefix-lines '())))
               (stack '()) (quoted #f) (comment-depth 0) (continued? #f)
               (fence #f) (pending #f) (boundaries '(0))
               (last-kind 'none) (code-end 0) (last-escaped #f) (continues? #f) (blank-starts '())
               (continuation-tokens
                 (append (get 'continuation-tokens '("=>" "=" "+" "-" "*" "/" "." "," "(" "[" "{" "&&" "||" "?" ":"))
                         (list escape)))
               (prefix (source-prefix-size bytes)))
          ;; WHAT A LINE'S CODE LAST WAS, for the strict test: `code` with the
          ;; index after the last code character, `string`, `comment`, or
          ;; `none` for a line with no code.
          (define (code! i) (set! last-kind 'code) (set! code-end (+ i 1)))
          ;; A WHOLE OPERATOR, NOT THE END OF A LONGER TOKEN: "x++", "1." and
          ;; Python's "..." end in a token's character and do not continue. (A
          ;; line whose last character the escape made literal is not asked:
          ;; see `last-escaped` where the line is read.)
          (define (continuation? code)
            (exists (lambda (t)
                      (let ((n (string-length t)) (m (string-length code)))
                        (and (>= m n) (string=? (substring code (- m n) m) t)
                             (let ((before (and (> m n) (string-ref code (- m n 1)))))
                               (not (and before
                                         (or (and (member t '("+" "-")) (char=? before (string-ref t 0)))
                                             (and (string=? t ".")
                                                  (or (char-numeric? before) (char=? before #\.))))))))))
                    continuation-tokens))
          (define (at-any s i tokens) (find (lambda (t) (string-prefix-at? s t i)) tokens))
          (define (fence-run line)
            (let* ((s (trim-left line)) (indent (- (string-length line) (string-length s))))
              (and (<= indent 3) (> (string-length s) 0) (memv (string-ref s 0) '(#\` #\~))
                   (let loop ((i 1))
                     (if (and (< i (string-length s)) (char=? (string-ref s i) (string-ref s 0))) (loop (+ i 1))
                         (and (>= i 3) (list (string-ref s 0) i (substring s i (string-length s)))))))))
          (define (scan-line line offset)
            (set! continued? #f)
            (set! last-kind 'none)
            (set! last-escaped #f)
            (let loop ((i 0))
              (if (= i (string-length line))
                  (begin
                    (when (> comment-depth 0) (set! last-kind 'comment))
                    (when (and quoted (not continued?) (not (member quoted multiline))) (fallback 'unclosed-quote (+ offset (bytevector-length (string->utf8 line))))))
                  (let ((c (string-ref line i)))
                    (cond
                      ((> comment-depth 0)
                       (cond ((string-prefix-at? line (cadr block) i)
                              (set! comment-depth (- comment-depth 1)) (set! last-kind 'comment)
                              (loop (+ i (string-length (cadr block)))))
                             ((and (get 'nested-block-comment #f) (string-prefix-at? line (car block) i))
                              (set! comment-depth (+ comment-depth 1)) (loop (+ i (string-length (car block)))))
                             (else (loop (+ i 1)))))
                      (quoted
                       (cond ((and (not (member quoted raw-quotes)) (string-prefix-at? line escape i))
                              (if (= (+ i (string-length escape)) (string-length line))
                                  (set! continued? #t)
                                  (loop (min (string-length line) (+ i (string-length escape) 1)))))
                             ((string-prefix-at? line quoted i)
                              (let ((n (string-length quoted))) (set! quoted #f) (set! last-kind 'string) (loop (+ i n))))
                             (else (loop (+ i 1)))))
                      ((at-any line i (comment-prefixes entry)) (if #f #f))
                      ((and block (string-prefix-at? line (car block) i))
                       (set! comment-depth 1) (set! last-kind 'comment) (loop (+ i (string-length (car block)))))
                      ((at-any line i uncertain) (fallback 'lexically-uncertain (+ offset (bytevector-length (string->utf8 (substring line 0 i))))))
                      ((at-any line i quotes) => (lambda (q) (set! quoted q) (set! last-kind 'string) (loop (+ i (string-length q)))))
                      ((exists (lambda (p) (char=? c (string-ref p 0))) pairs)
                       (let ((p (find (lambda (p) (char=? c (string-ref p 0))) pairs)))
                         (code! i) (set! stack (cons (string-ref p 1) stack)) (loop (+ i 1))))
                      ((exists (lambda (p) (char=? c (string-ref p 1))) pairs)
                       (unless (and (pair? stack) (char=? c (car stack))) (fallback 'unbalanced (+ offset i)))
                       (code! i) (set! stack (cdr stack)) (loop (+ i 1)))
                      ((string-prefix-at? line escape i)
                       (if (= (+ i (string-length escape)) (string-length line)) (set! continued? #t)
                           ;; the escaped character is code, as written
                           (begin (code! (+ i (string-length escape)))
                                  (set! last-escaped (+ i (string-length escape)))
                                  (loop (min (string-length line) (+ i (string-length escape) 1))))))
                      (else (unless (char-whitespace? c) (code! i)) (loop (+ i 1))))))))
          (unless (usable-profile? entry)
            (fallback 'unknown-profile 0))
          ;; NEVER: A FILE THAT BEGINS WITH A BYTE-ORDER MARK IS TEXT. The whole
          ;; file is tested without its mark, and an offset found in the
          ;; mark-less text counts the mark back in: offsets are into the
          ;; file as it is on disk.
          (unless (safe-utf8 (after-mark bytes)) (fallback 'invalid-utf8 0))
          (for-each
            (lambda (token)
              (let ((s (safe-utf8 (after-mark bytes))))
                (let loop ((i 0))
                  (when (< i (string-length s))
                    (if (string-prefix-at? s token i) (fallback 'lexically-uncertain (+ (mark-length bytes) (bytevector-length (string->utf8 (substring s 0 i)))))
                        (loop (+ i 1))))))) (get 'global-uncertain-tokens '()))
          (for-each
            (lambda (row)
              (let* ((offset (+ prefix (car row)))
                     (line (safe-utf8 (byte-slice bytes offset (+ prefix (cadr row)))))
                     (trimmed (trim-left line))
                     (top? (and (null? stack) (not quoted) (zero? comment-depth) (not continued?)
                                (or (not (string=? kind "indent")) (string=? line trimmed)
                                    ;; strict: a line of spaces only is blank,
                                    ;; judged by the next non-blank line below
                                    (and strict (string=? trimmed ""))))))
                (if (string=? kind "fence")
                    (let ((run (fence-run line)))
                      (cond
                        (fence
                         (symbol-not-top-level! offset)
                         (when (and run (char=? (car run) (car fence)) (>= (cadr run) (cadr fence))
                                    (string=? (trim-left (caddr run)) "")) (set! fence #f)))
                        (run (symbol-not-top-level! offset) (set! fence run))
                        ((definition? offset trimmed)
                         (when (> offset (car boundaries)) (set! boundaries (cons offset boundaries))))))
                    (begin
                      (unless top? (symbol-not-top-level! offset))
                      (when strict
                        (when continues? (symbol-not-top-level! offset))
                        (when (string=? kind "indent")
                          (cond ((string=? trimmed "")
                                 (when (and starts (memv offset starts)) (set! blank-starts (cons offset blank-starts))))
                                (else
                                 (unless (string=? line trimmed) (for-each symbol-not-top-level! blank-starts))
                                 (set! blank-starts '())))))
                      (cond
                        ((and top? (definition? offset trimmed))
                         (let ((start (or pending offset)))
                           (when (> start (car boundaries)) (set! boundaries (cons start boundaries))))
                         (set! pending #f))
                        ((and top? (or (at-any trimmed 0 (comment-prefixes entry))
                                       (and block (string-prefix-at? trimmed (car block) 0))
                                       (exists (lambda (p) (regex-match p trimmed)) prefix-patterns)))
                         (unless pending (set! pending offset)))
                        ((and top? (string=? trimmed "")) (set! pending #f))
                        ((and top? (not (> comment-depth 0))) (set! pending #f)))
                      (scan-line line offset)
                      (unless (string=? trimmed "")
                        ;; a line that ends in a character the escape made
                        ;; literal ends in that character, not in an operator
                        (set! continues? (and (eq? last-kind 'code)
                                              (not (eqv? last-escaped (- code-end 1)))
                                              (continuation? (substring line 0 code-end)))))))))
            (byte-lines (byte-slice bytes prefix (bytevector-length bytes))))
          (when (or (pair? stack) quoted (> comment-depth 0) continued? fence)
            (fallback 'unbalanced (bytevector-length bytes)))
          ;; A protected prefix belongs to the first actual block.
          ;; In file order: the strict blank-line test names a cut only when the
          ;; line after it is read.
          (list (filter (lambda (n) (or (= n 0) (> n prefix))) (reverse boundaries))
                (if strict
                    (list-sort (lambda (a b) (< (cadadr a) (cadadr b))) (reverse not-top-level))
                    (reverse not-top-level)))))))

  ;; ---- the symbols file ---------------------------------------------------
  ;;
  ;; One datum per line, read with the wire's datum reader (it builds data
  ;; and runs nothing): a header
  ;;   (symbols (digest "<sha256 of the file's bytes>")
  ;;            (source (vscode "<version>" "<languageId>")) (top-level #t))
  ;; then one line per top-level symbol, in the file's order,
  ;;   (symbol <start> <end> <kind> "<name>")
  ;; with byte offsets into the file as read-code-bytes reads it.
  ;;
  ;; NEVER: MALFORMED INPUT IS REFUSED BY NAME, NOT FOLDED INTO ONE BLOCK. A
  ;; list that cannot be trusted says why, and the first failure in this
  ;; order is the one named: a line that does not read or has the wrong
  ;; shape, or numbers that are not exact non-negative integers with start
  ;; below end (symbols-malformed, by line); a digest of other bytes
  ;; (symbols-stale); no symbols (symbols-empty); a start at or past the
  ;; end of the file or an end past it (symbols-past-end); an offset inside
  ;; a UTF-8 character (symbols-not-a-boundary); starts not strictly
  ;; ascending (symbols-unordered); overlapping ranges (symbols-overlap); a
  ;; start that is not the first byte of its line (symbols-not-a-line-start).
  ;; Each check runs over the whole list before the next one does.
  (define (symbols-refusal kind . details)
    (raise (cons* 'error kind details)))
  (define unread (list 'unread))
  (define (symbols-header d)
    (and (list? d) (= 4 (length d)) (eq? (car d) 'symbols)
         (let ((digest (cadr d)) (source (caddr d)) (top (cadddr d)))
           (and (list? digest) (= 2 (length digest)) (eq? (car digest) 'digest) (string? (cadr digest))
                (list? source) (= 2 (length source)) (eq? (car source) 'source)
                (let ((v (cadr source)))
                  (and (list? v) (= 3 (length v)) (eq? (car v) 'vscode)
                       (string? (cadr v)) (string? (caddr v))))
                (equal? top '(top-level #t))
                (list (cadr digest) (cadr source))))))
  ;; THE KINDS AN EDITOR'S SYMBOL PROVIDER NAMES: VS Code's SymbolKind
  ;; names, lower-cased and written as one word. The producer emits exactly
  ;; these; anything else is a line this reader does not understand.
  (define known-symbol-kinds
    '(file module namespace package class method property field constructor enum
      interface function variable constant string number boolean array object key
      null enummember struct event operator typeparameter))
  (define (symbol-line d)
    (and (list? d) (= 5 (length d)) (eq? (car d) 'symbol)
         (memq (list-ref d 3) known-symbol-kinds) (string? (list-ref d 4))
         (let ((start (cadr d)) (end (caddr d)))
           (and (integer? start) (exact? start) (>= start 0)
                (integer? end) (exact? end) (>= end 0)
                (< start end)
                (list start end (list-ref d 3))))))
  ;; The symbols file's lines as data, `unread` for a line that does not read.
  (define (symbols-data path)
    (let ((raw (read-code-bytes path)))
      (map (lambda (row)
             (let ((text (safe-utf8 (byte-slice raw (car row) (cadr row)))))
               (if text (guard (e (#t unread)) (string->sexpr-extended text)) unread)))
           (byte-lines raw))))
  ;; -> (source starts kinds), or raises one of the refusals above.
  (define (read-symbols symbols-path bytes)
    (let* ((data (symbols-data symbols-path))
           (header (and (pair? data) (symbols-header (car data))))
           (symbols
             (if (not header)
                 (symbols-refusal 'symbols-malformed (list 'line 1))
                 (let loop ((ds (cdr data)) (n 2) (out '()))
                   (cond
                     ((null? ds) (reverse out))
                     ((symbol-line (car ds)) => (lambda (sym) (loop (cdr ds) (+ n 1) (cons sym out))))
                     (else (symbols-refusal 'symbols-malformed (list 'line n))))))))
      (checked-symbols (car header) symbols bytes)
      (list (cadr header) (map car symbols) (map caddr symbols))))
  ;; THE CHECKS ON ONE FILE'S SYMBOLS, in the order above from the digest
  ;; on: -> the symbols, or raises the first refusal.
  (define (checked-symbols digest symbols bytes)
    (let ((found (bytevector->hex (sha256 bytes)))
          (size (bytevector-length bytes)))
      (define (continuation? i) (and (< i size) (= #x80 (bitwise-and (bytevector-u8-ref bytes i) #xC0))))
      (unless (string=? digest found)
        (symbols-refusal 'symbols-stale (list 'digest-expected digest) (list 'digest-found found)))
      (when (null? symbols) (symbols-refusal 'symbols-empty))
      (for-each (lambda (sym)
                  (cond ((>= (car sym) size) (symbols-refusal 'symbols-past-end (list 'at (car sym))))
                        ((> (cadr sym) size) (symbols-refusal 'symbols-past-end (list 'at (cadr sym))))))
                symbols)
      (for-each (lambda (sym)
                  (cond ((continuation? (car sym)) (symbols-refusal 'symbols-not-a-boundary (list 'at (car sym))))
                        ((continuation? (cadr sym)) (symbols-refusal 'symbols-not-a-boundary (list 'at (cadr sym))))))
                symbols)
      (let loop ((ss symbols))
        (when (and (pair? ss) (pair? (cdr ss)))
          (unless (< (car (car ss)) (car (cadr ss))) (symbols-refusal 'symbols-unordered))
          (loop (cdr ss))))
      (let loop ((ss symbols))
        (when (and (pair? ss) (pair? (cdr ss)))
          (when (< (car (cadr ss)) (cadr (car ss)))
            (symbols-refusal 'symbols-overlap (list 'at (car (cadr ss)))))
          (loop (cdr ss))))
      (for-each (lambda (sym)
                  (unless (or (= 0 (car sym)) (= 10 (bytevector-u8-ref bytes (- (car sym) 1))))
                    (symbols-refusal 'symbols-not-a-line-start (list 'at (car sym)))))
                symbols)
      symbols))

  ;; ---- the symbols file of an import -----------------------------------------
  ;;
  ;; `import-code <dir> --symbols <file>` reads one file for the whole
  ;; directory: one SECTION per source file, a header that names the file
  ;; by its path under the directory, as the walk spells it,
  ;;   (symbols (path "<path>") (digest "<sha256 of the file's bytes>")
  ;;            (source (vscode "<version>" "<languageId>")) (top-level #t))
  ;; then that file's symbol lines, as split-suggest's symbols file has
  ;; them. Sections follow one another in any order.
  ;;
  ;; NEVER: A FILE THAT CANNOT BE READ AS SECTIONS REFUSES THE REQUEST, before
  ;; the directory is read: a line that does not read or is neither a header
  ;; nor a symbol line, a symbol line before any header, a path that is not
  ;; a plain relative path, and a second section for a path already given,
  ;; each symbols-malformed with its line; and a file with no section at
  ;; all, symbols-empty. What one section says about its own file is judged
  ;; with that file (import-symbol-cuts).
  ;; -> ((<path> <digest> <line> <symbols>) ...), in the file's order.
  (define (read-import-symbols path)
    (let loop ((ds (symbols-data path)) (n 1) (current #f) (out '()))
      (define (close) (if current (cons (list (car current) (cadr current) (caddr current) (reverse (cadddr current))) out) out))
      (cond
        ((null? ds)
         (when (and (not current) (null? out)) (symbols-refusal 'symbols-empty))
         (reverse (close)))
        ((import-symbols-header (car ds))
         => (lambda (h)
              (when (or (and current (string=? (car current) (car h)))
                        (assoc (car h) out))
                (symbols-refusal 'symbols-malformed (list 'line n)))
              (loop (cdr ds) (+ n 1) (list (car h) (cadr h) n '()) (close))))
        ((and current (symbol-line (car ds)))
         => (lambda (sym) (loop (cdr ds) (+ n 1) (list (car current) (cadr current) (caddr current) (cons sym (cadddr current))) out)))
        (else (symbols-refusal 'symbols-malformed (list 'line n))))))
  ;; -> (<path> <digest>) for a section's header, else #f.
  (define (import-symbols-header d)
    (and (list? d) (= 5 (length d)) (eq? (car d) 'symbols)
         (let ((p (cadr d)))
           (and (list? p) (= 2 (length p)) (eq? (car p) 'path) (code-safe-path? (cadr p))
                (let ((h (symbols-header (cons 'symbols (cddr d)))))
                  (and h (list (cadr p) (car h))))))))

  ;; THE CUTS A FIRST IMPORT MAKES FROM ONE FILE'S SECTION. Every start is a
  ;; cut, and every end is taken to the start of the line after it (an end
  ;; already there stays), so what follows a symbol on its last line stays
  ;; with it. Between two symbols, and before the first and after the last,
  ;; what is left is a block of its own: the blocks are the file's bytes cut
  ;; at those places, and they run together to the file. A cut AT the
  ;; protected prefix's end is dropped, the prefix staying with the first
  ;; block, as split-suggest drops one.
  ;;
  ;; NEVER: A CUT THE SCANNER DOES NOT SEE AT THE TOP LEVEL IS REFUSED, not
  ;; moved and not dropped. After the section's own checks (checked-symbols),
  ;; the first failure in this order is raised for the file:
  ;;   symbols-unchecked (reason no-language-entry | no-suggest-profile):
  ;;     no scanner can say what is top level in this file;
  ;;   symbols-in-prefix (at <byte>): a cut inside the shebang, coding line
  ;;     or byte-order mark;
  ;;   symbols-unscanned (reason <r>) (at <byte>): the scanner cannot follow
  ;;     the file -- unbalanced, an unclosed quote, a token it cannot judge;
  ;;   symbols-not-top-level (at <byte>): the first cut inside a body, a
  ;;     string, a block comment or an open bracket.
  ;; -> the cuts, 0 and the file's size included.
  (define (import-symbol-cuts section bytes entry)
    (let* ((symbols (checked-symbols (cadr section) (cadddr section) bytes))
           (size (bytevector-length bytes))
           (line-after (lambda (e)
                         (if (or (= e size) (= 10 (bytevector-u8-ref bytes (- e 1)))) e
                             (let loop ((i e))
                               (cond ((= i size) size)
                                     ((= 10 (bytevector-u8-ref bytes i)) (+ i 1))
                                     (else (loop (+ i 1))))))))
           (inner (let loop ((ns (list-sort < (append (map car symbols) (map (lambda (s) (line-after (cadr s))) symbols))))
                             (out '()))
                    (cond ((null? ns) (reverse out))
                          ((or (= (car ns) 0) (= (car ns) size) (and (pair? out) (= (car ns) (car out)))) (loop (cdr ns) out))
                          (else (loop (cdr ns) (cons (car ns) out))))))
           (prefix (source-prefix-size bytes)))
      (cond ((not entry) (symbols-refusal 'symbols-unchecked '(reason no-language-entry)))
            ((not (usable-profile? entry)) (symbols-refusal 'symbols-unchecked '(reason no-suggest-profile))))
      (cond ((find (lambda (n) (< n prefix)) inner) => (lambda (n) (symbols-refusal 'symbols-in-prefix (list 'at n)))))
      (let* ((cuts (filter (lambda (n) (> n prefix)) inner))
             (scan (suggest-boundaries entry bytes cuts #t))
             (said (cadr scan)))
        (cond
          ((and (pair? said) (eq? (caar said) 'code))
           (symbols-refusal 'symbols-unscanned (list 'reason (cadr (car said))) (list 'at (cadddr (car said)))))
          ((pair? said) (symbols-refusal 'symbols-not-top-level (cadr (car said)))))
        (append (list 0) cuts (list size)))))

  ;; WITHOUT A PROFILE THE SCANNER CAN FOLLOW -- no language entry, or an
  ;; entry with no suggest profile -- the editor's starts are the only
  ;; knowledge there is: they are the cuts as given, the protected prefix
  ;; still staying with the first block, and the warnings name which of the
  ;; two it was. Falling back to one block instead would make --symbols do
  ;; nothing and say nothing.
  (define (cuts-as-given bytes starts)
    (let ((prefix (source-prefix-size bytes)))
      (cons 0 (filter (lambda (n) (> n prefix)) starts))))

  (define counter 0)
  (define (review-path path)
    (set! counter (+ counter 1))
    (let* ((suffix (let loop ((i (- (string-length path) 1)))
                     (cond ((< i 0) "") ((char=? (string-ref path i) #\/) "")
                           ((char=? (string-ref path i) #\.) (substring path i (string-length path)))
                           (else (loop (- i 1)))))))
      (string-append path ".review-" (number->string (process-id)) "-"
                     (number->string (wall-clock-ms)) "-" (number->string counter) suffix)))
  ;; (split-suggest path output [symbols-path]). The answer says which
  ;; supplier chose the cuts, as its last clause: (cuts-from regex), or the
  ;; symbols file's source, (cuts-from (vscode "<version>" "<languageId>")).
  ;; The symbols file is read and checked before any cut is computed.
  (define (split-suggest path output . rest)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
      (let* ((bytes (read-code-bytes path)) (entry (language-for-path path))
             (symbols (and (pair? rest) (car rest) (read-symbols (car rest) bytes)))
             (suggestion
               (cond ((not symbols) (suggest-boundaries entry bytes))
                     ((not entry) (list (cuts-as-given bytes (cadr symbols)) '((no-language-entry))))
                     ((not (usable-profile? entry))
                      (list (cuts-as-given bytes (cadr symbols)) '((no-suggest-profile))))
                     (else (suggest-boundaries entry bytes (cadr symbols)))))
             ;; NEVER: NO START IS DROPPED IN SILENCE. One inside the
             ;; protected prefix (a shebang, a coding line, a BOM) cannot
             ;; start a block -- the prefix stays with the first one -- and
             ;; the warnings say so, as they do for a start not at the top
             ;; level. A start AT the prefix's end starts the first block, so
             ;; it loses nothing and is not warned.
             (in-prefix
               (if symbols
                   (let ((prefix (source-prefix-size bytes)))
                     (map (lambda (n) (list 'symbol-in-prefix (list 'at n)))
                          (filter (lambda (n) (< n prefix)) (cadr symbols))))
                   '()))
             (warnings (if symbols
                           (append (cadr suggestion) in-prefix (list (cons 'symbol-kinds (caddr symbols))))
                           (cadr suggestion)))
             (cuts (car suggestion))
             (target (or output (review-path path))) (tmp (string-append (review-path path) ".tmp"))
             (entries (map (lambda (from to) (list "new" (byte-slice bytes from to)))
                           cuts (append (cdr cuts) (list (bytevector-length bytes))))))
        (trace-event! 'suggest-scanned path (bytevector-length bytes))
        (unless (equal? bytes (read-code-bytes path)) (projection-failure 'input-changed))
        (unless (file-create-exclusive! tmp) (projection-failure 'review-collision))
        (dynamic-wind
          (lambda () (if #f #f))
          (lambda ()
            (atomic-write! tmp (projection-encode entry #f entries) 'working)
            (when (eq? (link! tmp target) 'exists) (projection-failure 'output-exists))
            (directory-entry-durable! target 'working)
            (list 'ok (list 'working-path target) (list 'boundaries cuts) (list 'warnings warnings)
                  (list 'cuts-from (if symbols (car symbols) 'regex))))
          (lambda () (guard (e (#t #f)) (unlink! tmp)))))))
)
