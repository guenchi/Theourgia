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

  ;; ---- the ecmascript scan ------------------------------------------------
  ;;
  ;; ALL LIVE READINGS. In JavaScript a "/" after a token that can end an
  ;; expression is a division or the start of a regex, and which one is not
  ;; decided by the token's spelling (automatic semicolons, contextual
  ;; keywords, TypeScript's postfix "!" and type-argument ">"). So the scan
  ;; keeps every reading that can still be right: a READING is the complete
  ;; scanner state, and the scan holds a set of them, forking at such a "/"
  ;; and letting each reading die where the text disproves it (a regex or a
  ;; '...' / "..." open at a line end, a closer that does not match, anything
  ;; open at the end of the file). A line start is at the top level only when
  ;; every reading that SURVIVES the file says so.
  ;;
  ;; A STATE is (stack lex prev owed cont fresh), interned, so two equal states
  ;; are eq? and a merge is a pointer comparison:
  ;;   stack  frames, innermost first: paren, bracket, brace, tmpl (a
  ;;          template's text), expr (a template's "${"), as an interned list
  ;;   lex    code, sq, dq, regex, class (a regex's [...]), comment, tmpl
  ;;   prev   what the last significant token was, for a "/": rx (it cannot
  ;;          end an expression: a regex follows), fk (it can: fork), dot ("."
  ;;          or "?.": a reserved word after it is a property name)
  ;;   owed   the last significant token continues the statement (the table
  ;;          scan's continuation rule, asked of the token): it survives
  ;;          comments and line ends
  ;;   cont   a quote's escape at its line's end continues it onto the next
  ;;   fresh  no code token yet on this line (Annex B's line-start "-->")
  ;;
  ;; THE SCAN GOES BY CHARACTER INDEX IN LOCKSTEP: every reading at the lowest
  ;; index takes one token (and may fork into two), then readings at the same
  ;; index with the same state merge. Past 32 readings after a merge the file
  ;; falls back lexically-uncertain at the "/" that forked. Each reading's
  ;; judgments of the line starts it passed are a node in a shared history:
  ;; a fork shares its parent, a merge makes an AND node of both, and the
  ;; answer is read once, at the end, from the survivors' nodes.
  ;;
  ;; THE END STATES, as the table scan names the same ones: a reading open in
  ;; '...' or "..." at a line end without a continuing escape dies
  ;; unclosed-quote at that line end; a regex open at a line end dies
  ;; lexically-uncertain there (the table scan has no such state); a closer
  ;; that does not match dies unbalanced at the closer; any state still open at
  ;; the end of the file -- a pair, a template, a continued string, a block
  ;; comment -- dies unbalanced at the file's size. When no reading is left,
  ;; the file falls back with the last death's reason and offset.
  ;;
  ;; LINES is the list of the file's lines (after the protected prefix) as
  ;; strings without their line feeds. TYPES? is TypeScript's syntax (a run of
  ;; ">" and a postfix "!" can end an expression, `void` is also a type);
  ;; ANNEX-B? is plain JavaScript's: its HTML-like comments are refused for the
  ;; whole file (JSX is refused before this scan, on the file's text).
  ;; OWES? asks whether a token continues the statement. -> a vector with one
  ;; (top? . owed?) per line, or (fallback <reason> <line> <char>), <line>
  ;; being 'eof for the file's end.
  (define (ecmascript-judgments lines types? annex-b? owes?)
    (call/cc
      (lambda (give-up)
        (define stack-ids (make-eqv-hashtable))
        (define state-ids (make-eqv-hashtable))
        (define next-id 1)
        ;; an interned stack node: #(id frame below); the empty stack is 0
        (define (frame-code f) (case f ((paren) 1) ((bracket) 2) ((brace) 3) ((tmpl) 4) ((expr) 5) (else 0)))
        (define (stack-id s) (if (eqv? s 0) 0 (vector-ref s 0)))
        (define (push s f)
          (let ((key (+ (* (stack-id s) 8) (frame-code f))))
            (or (hashtable-ref stack-ids key #f)
                (let ((n (vector next-id f s)))
                  (set! next-id (+ next-id 1))
                  (hashtable-set! stack-ids key n)
                  n))))
        (define (top s) (if (eqv? s 0) #f (vector-ref s 1)))
        (define (pop s) (vector-ref s 2))
        (define (lex-code l) (case l ((code) 0) ((sq) 1) ((dq) 2) ((regex) 3) ((class) 4) ((comment) 5) ((tmpl) 6)))
        (define (prev-code p) (case p ((rx) 0) ((fk) 1) ((dot) 2)))
        ;; an interned state: #(id stack lex prev owed cont fresh)
        (define (state stack lex prev owed cont fresh)
          (let ((key (+ (* (stack-id stack) 1024) (* (lex-code lex) 64) (* (prev-code prev) 8)
                        (if owed 4 0) (if cont 2 0) (if fresh 1 0))))
            (or (hashtable-ref state-ids key #f)
                (let ((v (vector next-id stack lex prev owed cont fresh)))
                  (set! next-id (+ next-id 1))
                  (hashtable-set! state-ids key v)
                  v))))
        (define (s-stack s) (vector-ref s 1))
        (define (s-lex s) (vector-ref s 2))
        (define (s-prev s) (vector-ref s 3))
        (define (s-owed s) (vector-ref s 4))
        (define (s-cont s) (vector-ref s 5))
        (define (s-fresh s) (vector-ref s 6))
        (define (with s . kvs)
          (let loop ((kvs kvs) (stack (s-stack s)) (lex (s-lex s)) (prev (s-prev s))
                     (owed (s-owed s)) (cont (s-cont s)) (fresh (s-fresh s)))
            (if (null? kvs)
                (state stack lex prev owed cont fresh)
                (let ((v (cadr kvs)))
                  (case (car kvs)
                    ((stack) (loop (cddr kvs) v lex prev owed cont fresh))
                    ((lex) (loop (cddr kvs) stack v prev owed cont fresh))
                    ((prev) (loop (cddr kvs) stack lex v owed cont fresh))
                    ((owed) (loop (cddr kvs) stack lex prev v cont fresh))
                    ((cont) (loop (cddr kvs) stack lex prev owed v fresh))
                    ((fresh) (loop (cddr kvs) stack lex prev owed cont v)))))))
        ;; A READING: #(state index history). A HISTORY NODE: #(line top-ok owed
        ;; parent other), `line` #f and both parents set for an AND node.
        (define (reading st i h) (vector st i h))
        (define (r-state r) (vector-ref r 0))
        (define (r-index r) (vector-ref r 1))
        (define (r-history r) (vector-ref r 2))
        (define (and-node a b) (if (eq? a b) a (vector #f #f #f a b)))
        ;; THE LAST DEATH, for the fallback when no reading is left
        (define last-death #f)
        (define (die! reason line char) (set! last-death (list 'fallback reason line char)) '())
        (define regex-keywords
          (if types?
              '("return" "typeof" "instanceof" "in" "new" "delete" "throw" "case" "do" "else" "default")
              '("return" "typeof" "instanceof" "in" "new" "delete" "throw" "case" "do" "else" "default" "void")))
        ;; longest first: the scan takes the longest that matches
        (define operators
          '(">>>=" "..." "===" "!==" "**=" "<<=" ">>=" ">>>" "&&=" "||=" "??="
            "=>" "==" "!=" "<=" ">=" "&&" "||" "??" "?." "++" "--" "+=" "-=" "*=" "%=" "&=" "|=" "^=" "<<" ">>" "**"
            "=" "+" "-" "*" "%" "&" "|" "^" "!" "~" "?" ":" ";" "," "." "<" ">" "@"))
        ;; WHITESPACE, as an approximation that holds ECMAScript's: Unicode's
        ;; White_Space (a little wider: U+0085 is not ECMAScript whitespace) and
        ;; U+FEFF, which char-whitespace? does not hold. A FEFF inside a line is
        ;; a space, not the start of a name (it would end the line's freshness
        ;; before a line-start "-->").
        (define (es-space? c) (or (char-whitespace? c) (char=? c #\xFEFF)))
        (define (name-char? c)
          (or (char-alphabetic? c) (memv c '(#\_ #\$ #\\)) (and (char>? c #\x7F) (not (es-space? c)))))
        ;; A PRIVATE NAME, "#" and its name, is one name-like token: it can end
        ;; an expression, and `#return` is not the reserved word.
        (define (ident-start? c) (or (name-char? c) (char=? c #\#)))
        (define (ident-char? c) (or (name-char? c) (char-numeric? c)))
        (define (gt-run? t) (and (> (string-length t) 0) (for-all (lambda (c) (char=? c #\>)) (string->list t))))
        ;; ONE TOKEN of reading R at its index on LINE (number LN) -> the
        ;; readings it becomes: one, two at a fork, none at a death
        (define (step r line ln)
          (let* ((s (r-state r)) (i (r-index r)) (n (string-length line)) (c (string-ref line i)) (h (r-history r))
                 (at (lambda (j) (and (< j n) (string-ref line j))))
                 (go (lambda (st j) (list (reading st j h))))
                 (code (lambda (prev owed) (with s 'lex 'code 'prev prev 'owed owed 'fresh #f)))
                 ;; the index of the next character from J that is one of
                 ;; CS, or the line's end: what lies between changes nothing
                 (skip (lambda (j cs) (let loop ((j j)) (if (or (= j n) (memv (string-ref line j) cs)) j (loop (+ j 1)))))))
            (case (s-lex s)
              ((comment)
               (cond ((and (char=? c #\*) (eqv? (at (+ i 1)) #\/)) (go (with s 'lex 'code) (+ i 2)))
                     (else (go s (skip (+ i 1) '(#\*))))))
              ((sq dq)
               (cond ((char=? c #\\)
                      (if (or (= (+ i 1) n) (and (= (+ i 2) n) (eqv? (at (+ i 1)) #\return)))
                          (go (with s 'cont #t) n)
                          (go s (+ i 2))))
                     ((char=? c (if (eq? (s-lex s) 'sq) #\' #\")) (go (code 'fk #f) (+ i 1)))
                     (else (go s (skip (+ i 1) (list #\\ (if (eq? (s-lex s) 'sq) #\' #\")))))))
              ((regex)
               (cond ((char=? c #\\) (if (< (+ i 1) n) (go s (+ i 2)) (go s n)))
                     ((char=? c #\[) (go (with s 'lex 'class) (+ i 1)))
                     ((char=? c #\/)
                      (let flags ((j (+ i 1)))
                        (if (and (< j n) (ident-char? (string-ref line j))) (flags (+ j 1)) (go (code 'fk #f) j))))
                     (else (go s (skip (+ i 1) '(#\\ #\[ #\/))))))
              ((class)
               (cond ((char=? c #\\) (if (< (+ i 1) n) (go s (+ i 2)) (go s n)))
                     ((char=? c #\]) (go (with s 'lex 'regex) (+ i 1)))
                     (else (go s (skip (+ i 1) '(#\\ #\]))))))
              ((tmpl)
               (cond ((char=? c #\\) (go s (min n (+ i 2))))
                     ((char=? c #\`) (go (with s 'stack (pop (s-stack s)) 'lex 'code 'prev 'fk 'owed #f 'fresh #f) (+ i 1)))
                     ((and (char=? c #\$) (eqv? (at (+ i 1)) #\{))
                      (go (with s 'stack (push (s-stack s) 'expr) 'lex 'code 'prev 'rx 'owed #f) (+ i 2)))
                     (else (go s (skip (+ i 1) '(#\\ #\` #\$))))))
              (else
               (cond
                 ((es-space? c) (go s (let ws ((j (+ i 1))) (if (and (< j n) (es-space? (string-ref line j))) (ws (+ j 1)) j))))
                 ((and annex-b? (string-prefix-at? line "<!--" i)) (give-up (list 'fallback 'lexically-uncertain ln i)))
                 ((and annex-b? (s-fresh s) (string-prefix-at? line "-->" i)) (give-up (list 'fallback 'lexically-uncertain ln i)))
                 ((char=? c #\/)
                  (cond ((eqv? (at (+ i 1)) #\/) (go s n))
                        ((eqv? (at (+ i 1)) #\*) (go (with s 'lex 'comment) (+ i 2)))
                        (else
                         (let* ((assign? (eqv? (at (+ i 1)) #\=))
                                (tok (if assign? "/=" "/"))
                                (division (reading (code 'rx (owes? tok)) (+ i (string-length tok)) h))
                                (regex (reading (with s 'lex 'regex 'fresh #f) (+ i 1) h)))
                           (if (eq? (s-prev s) 'fk) (list regex division) (list regex))))))
                 ((char=? c #\`) (go (with s 'stack (push (s-stack s) 'tmpl) 'lex 'tmpl 'fresh #f) (+ i 1)))
                 ((char=? c #\') (go (with s 'lex 'sq 'fresh #f) (+ i 1)))
                 ((char=? c #\") (go (with s 'lex 'dq 'fresh #f) (+ i 1)))
                 ((memv c '(#\( #\[ #\{))
                  (let ((f (case c ((#\() 'paren) ((#\[) 'bracket) (else 'brace))))
                    (go (with (code 'rx (owes? (string c))) 'stack (push (s-stack s) f)) (+ i 1))))
                 ((memv c '(#\) #\]))
                  (if (eq? (top (s-stack s)) (if (char=? c #\)) 'paren 'bracket))
                      (go (with (code 'fk #f) 'stack (pop (s-stack s))) (+ i 1))
                      (die! 'unbalanced ln i)))
                 ((char=? c #\})
                  (case (top (s-stack s))
                    ((expr) (go (with s 'stack (pop (s-stack s)) 'lex 'tmpl) (+ i 1)))
                    ((brace) (go (with (code 'fk #f) 'stack (pop (s-stack s))) (+ i 1)))
                    (else (die! 'unbalanced ln i))))
                 ((or (char-numeric? c) (and (char=? c #\.) (let ((d (at (+ i 1)))) (and d (char-numeric? d)))))
                  (let num ((j (+ i 1)))
                    (let ((d (at j)))
                      (cond ((and d (or (ident-char? d) (char=? d #\.))) (num (+ j 1)))
                            ((and d (memv d '(#\+ #\-)) (memv (string-ref line (- j 1)) '(#\e #\E))
                                  (not (string-prefix-at? line "0x" i)) (not (string-prefix-at? line "0X" i)))
                             (num (+ j 1)))
                            (else (go (code 'fk #f) j))))))
                 ((ident-start? c)
                  (let word ((j (+ i 1)))
                    (if (and (< j n) (ident-char? (string-ref line j)))
                        (word (+ j 1))
                        (let ((w (substring line i j)))
                          (go (code (if (and (not (eq? (s-prev s) 'dot)) (member w regex-keywords)) 'rx 'fk) #f) j)))))
                 (else
                  (let ((op (or (find (lambda (o) (string-prefix-at? line o i)) operators) (string c))))
                    (go (code (cond ((member op '("." "?.")) 'dot)
                                    ((member op '("++" "--")) 'fk)
                                    ((and types? (or (gt-run? op) (string=? op "!"))) 'fk)
                                    (else 'rx))
                              (and (not (member op '("++" "--"))) (owes? op)))
                        (+ i (string-length op))))))))))
        ;; MERGE readings at the same index with the same state; -> the list
        (define (merged rs)
          (let loop ((rs rs) (out '()))
            (if (null? rs)
                (reverse out)
                (let* ((r (car rs))
                       (same (find (lambda (o) (and (eq? (r-state o) (r-state r)) (= (r-index o) (r-index r)))) out)))
                  (if same
                      (loop (cdr rs) (map (lambda (o) (if (eq? o same) (reading (r-state o) (r-index o) (and-node (r-history o) (r-history r))) o)) out))
                      (loop (cdr rs) (cons r out)))))))
        (define line-count (length lines))
        (define (judge s) (and (eqv? (s-stack s) 0) (eq? (s-lex s) 'code) (not (s-cont s))))
        (let line-loop ((ls lines) (ln 0) (live (list (reading (state 0 'code 'rx #f #f #t) 0 #f))))
          (if (null? ls)
              ;; THE END OF THE FILE: a reading with anything open dies
              (let ((survivors (filter (lambda (r) (judge (r-state r))) live)))
                (when (null? survivors)
                  (give-up (if (null? live) (or last-death (list 'fallback 'lexically-uncertain 'eof 0))
                               (list 'fallback 'unbalanced 'eof 0))))
                ;; ONE TRAVERSAL of the survivors' histories: a line some
                ;; reachable node judges not top-level is not, one some node
                ;; judges owed continues
                (let ((not-top (make-vector line-count #f)) (owed (make-vector line-count #f)) (seen (make-eq-hashtable)))
                  (let walk ((todo (map r-history survivors)))
                    (unless (null? todo)
                      (let ((node (car todo)))
                        (if (or (not node) (hashtable-ref seen node #f))
                            (walk (cdr todo))
                            (begin
                              (hashtable-set! seen node #t)
                              (when (vector-ref node 0)
                                (unless (vector-ref node 1) (vector-set! not-top (vector-ref node 0) #t))
                                (when (vector-ref node 2) (vector-set! owed (vector-ref node 0) #t)))
                              (walk (cons* (vector-ref node 3) (vector-ref node 4) (cdr todo))))))))
                  (let ((out (make-vector line-count #f)))
                    (let fill ((k 0))
                      (when (< k line-count)
                        (vector-set! out k (cons (not (vector-ref not-top k)) (vector-ref owed k)))
                        (fill (+ k 1))))
                    out)))
              (let* ((line (car ls)) (n (string-length line))
                     ;; THE LINE START: a fresh line, and each reading's judgment
                     ;; of it a node on its history
                     (starts (merged (map (lambda (r)
                                            (let ((s (with (r-state r) 'fresh #t 'cont #f)))
                                              (reading s 0 (vector ln (judge (r-state r)) (s-owed (r-state r)) (r-history r) #f))))
                                          live))))
                (let scan ((live starts))
                  (let ((pending (filter (lambda (r) (< (r-index r) n)) live)))
                    (if (null? pending)
                        ;; THE LINE END: a regex dies; a quote dies unless an
                        ;; escape continues it
                        (let ((next (apply append
                                           (map (lambda (r)
                                                  (let ((s (r-state r)))
                                                    (case (s-lex s)
                                                      ((regex class) (die! 'lexically-uncertain ln n))
                                                      ((sq dq) (if (s-cont s) (list (reading s 0 (r-history r))) (die! 'unclosed-quote ln n)))
                                                      (else (list (reading s 0 (r-history r)))))))
                                                live))))
                          (when (null? next) (give-up last-death))
                          (line-loop (cdr ls) (+ ln 1) (merged next)))
                        (let* ((i (apply min (map r-index pending)))
                               (stepped (apply append (map (lambda (r) (if (= (r-index r) i) (step r line ln) (list r))) live)))
                               (now (merged stepped)))
                          (when (null? now) (give-up last-death))
                          (when (> (length now) 32) (give-up (list 'fallback 'lexically-uncertain ln i)))
                          (scan now)))))))))))

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
               ;; A BODY FORM: the first opener met with an empty stack is read
               ;; for its head, once per file; a head the profile lists in
               ;; `body-forms` designates that frame, and the definitions at
               ;; its first level are cuts as top-level ones are, until its
               ;; closer is popped. `unattempted`, `designated`, then `done`;
               ;; `none` when the head is another or the profile lists none.
               ;; HEAD is the head read so far while it is being read, else #f.
               (body-heads (get 'body-forms '()))
               (designation (if (null? body-heads) 'none 'unattempted))
               (head #f)
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
          ;; Python's "..." end in a token's character and do not continue;
          ;; Rust's open range "0.." does. (A
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
                                                  (or (char-numeric? before)
                                                      ;; Python's ellipsis; Rust's ".." range continues
                                                      (and (>= m 3) (string=? (substring code (- m 3) m) "...")))))))))))
                    continuation-tokens))
          (define (at-any s i tokens) (find (lambda (t) (string-prefix-at? s t i)) tokens))
          ;; The head being read ends at anything that is not one of its
          ;; characters; whitespace, a comment or a line end before its first
          ;; character does not end the wait.
          (define (head-end!)
            (when head
              (set! designation (if (member head body-heads) 'designated 'none))
              (set! head #f)))
          (define (head-pause!)
            (when (and head (> (string-length head) 0)) (head-end!)))
          (define (fence-run line)
            (let* ((s (trim-left line)) (indent (- (string-length line) (string-length s))))
              (and (<= indent 3) (> (string-length s) 0) (memv (string-ref s 0) '(#\` #\~))
                   (let loop ((i 1))
                     (if (and (< i (string-length s)) (char=? (string-ref s i) (string-ref s 0))) (loop (+ i 1))
                         (and (>= i 3) (list (string-ref s 0) i (substring s i (string-length s)))))))))
          ;; ONE LINE'S DECISION, given its judgment: TOP? (the line starts at
          ;; the top level as the scanner's state reads it) and CONTINUES? (for
          ;; strict, the line before it continues the statement). The table
          ;; scan asks it line by line as it goes; the ecmascript scan asks it
          ;; for every line after the file is read, with the judgments every
          ;; surviving reading agrees on. So there is one rule for what a line
          ;; is -- a cut, a comment line pending for the next definition, the
          ;; end of such a run, a start the scanner does not see at the top
          ;; level -- whichever scan judged it. A top-level line is outside
          ;; every comment, so the comment depth is not asked here.
          (define (decide-line! offset line trimmed top? continues?)
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
              (top? (set! pending #f))))
          (define (scan-line line offset)
            (set! continued? #f)
            (set! last-kind 'none)
            (set! last-escaped #f)
            (let loop ((i 0))
              (if (= i (string-length line))
                  (begin
                    (head-pause!)
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
                      ((at-any line i (comment-prefixes entry)) (head-pause!))
                      ((and block (string-prefix-at? line (car block) i))
                       (head-pause!)
                       (set! comment-depth 1) (set! last-kind 'comment) (loop (+ i (string-length (car block)))))
                      ((at-any line i uncertain) (fallback 'lexically-uncertain (+ offset (bytevector-length (string->utf8 (substring line 0 i))))))
                      ((at-any line i quotes) => (lambda (q) (head-end!) (set! quoted q) (set! last-kind 'string) (loop (+ i (string-length q)))))
                      ((exists (lambda (p) (char=? c (string-ref p 0))) pairs)
                       (let ((p (find (lambda (p) (char=? c (string-ref p 0))) pairs)))
                         (head-end!)
                         (code! i)
                         (when (and (null? stack) (eq? designation 'unattempted)) (set! head ""))
                         (set! stack (cons (string-ref p 1) stack)) (loop (+ i 1))))
                      ((exists (lambda (p) (char=? c (string-ref p 1))) pairs)
                       (unless (and (pair? stack) (char=? c (car stack))) (fallback 'unbalanced (+ offset i)))
                       (head-end!)
                       (code! i) (set! stack (cdr stack))
                       (when (and (null? stack) (eq? designation 'designated)) (set! designation 'done))
                       (loop (+ i 1)))
                      ((string-prefix-at? line escape i)
                       (head-end!)
                       (if (= (+ i (string-length escape)) (string-length line)) (set! continued? #t)
                           ;; the escaped character is code, as written
                           (begin (code! (+ i (string-length escape)))
                                  (set! last-escaped (+ i (string-length escape)))
                                  (loop (min (string-length line) (+ i (string-length escape) 1))))))
                      ((char-whitespace? c) (head-pause!) (loop (+ i 1)))
                      (else (code! i)
                            (when head (set! head (string-append head (string c))))
                            (loop (+ i 1))))))))
          ;; JSX, IN PLAIN JAVASCRIPT (the profile with annex-b-comments), as TEXT:
          ;; every JSX element ends with a closing tag "</ name>" or a self-close
          ;; "/>", so a file holding JSX holds one of the two, whatever a lexical
          ;; reading would call comment, string or regex there. The whole file,
          ;; after its byte-order mark, falls back lexically-uncertain at the
          ;; first "<" whose next character other than whitespace (line ends
          ;; included) is "/", and at the first "/" whose next such character is
          ;; ">". So "</div>", "< /div>", "<" and "/div>" on two lines, and
          ;; "< /*c*/ /div>" are caught: a comment starts with "/", so whatever
          ;; separates a closing tag's "<" from its "/", the "<" is caught. A
          ;; self-close is caught as "/ >" or "/ /*c*/ >" (the comment's last
          ;; "/" then ">"); one whose "/" and ">" a line comment separates is
          ;; not, and is the one JSX spelling this test misses. THE COST, in
          ;; the safe direction: a JavaScript file with HTML in a string or a
          ;; comment ("</p>", "<br/>"), a regex such as /a/>b or />/, a
          ;; comparison such as x</re/, or a comment such as "// > quoted"
          ;; falls back too. TypeScript is not tested.
          (define (jsx-pass)
            (let* ((s (safe-utf8 (after-mark bytes))) (n (string-length s)))
              (define (space? c) (or (char-whitespace? c) (char=? c #\xFEFF)))
              (define (next i) (if (and (< i n) (space? (string-ref s i))) (next (+ i 1)) i))
              (let loop ((i 0))
                (when (< i n)
                  (let ((c (string-ref s i)))
                    (if (and (memv c '(#\< #\/))
                             (let ((j (next (+ i 1))))
                               (and (< j n) (char=? (string-ref s j) (if (char=? c #\<) #\/ #\>)))))
                        (fallback 'lexically-uncertain (+ (mark-length bytes) (bytevector-length (string->utf8 (substring s 0 i)))))
                        (loop (+ i 1))))))))
          ;; THE ECMASCRIPT PROFILES: every line is judged by the live readings
          ;; (ecmascript-judgments) and then decided, line by line, by the same
          ;; decide-line! the table scan asks. A lone CR (one not before a line
          ;; feed), U+2028 or U+2029 anywhere in the file, the protected prefix
          ;; included, is a line end the byte-line scan cannot follow: the whole
          ;; file falls back at it, at its offset in the file. In plain
          ;; JavaScript, jsx-pass then tests the text.
          (define (ecmascript-pass)
            (let ((size (bytevector-length bytes)))
              (let loop ((i 0))
                (when (< i size)
                  (let ((b (bytevector-u8-ref bytes i)))
                    (cond ((and (= b 13) (not (and (< (+ i 1) size) (= 10 (bytevector-u8-ref bytes (+ i 1))))))
                           (fallback 'lexically-uncertain i))
                          ((and (= b #xE2) (< (+ i 2) size) (= #x80 (bytevector-u8-ref bytes (+ i 1)))
                                (memv (bytevector-u8-ref bytes (+ i 2)) '(#xA8 #xA9)))
                           (fallback 'lexically-uncertain i))
                          (else (loop (+ i 1))))))))
            (when (get 'annex-b-comments #f) (jsx-pass))
            (let* ((rows (byte-lines (byte-slice bytes prefix (bytevector-length bytes))))
                   (offsets (map (lambda (row) (+ prefix (car row))) rows))
                   (lines (map (lambda (row) (safe-utf8 (byte-slice bytes (+ prefix (car row)) (+ prefix (cadr row))))) rows))
                   (judged (ecmascript-judgments lines (get 'type-syntax #f) (get 'annex-b-comments #f) continuation?)))
              (when (and (pair? judged) (eq? (car judged) 'fallback))
                (let ((ln (caddr judged)) (ch (cadddr judged)))
                  (fallback (cadr judged)
                            (if (eq? ln 'eof)
                                (bytevector-length bytes)
                                (+ (list-ref offsets ln) (bytevector-length (string->utf8 (substring (list-ref lines ln) 0 ch))))))))
              (let loop ((os offsets) (ls lines) (k 0))
                (unless (null? os)
                  (let ((j (vector-ref judged k)))
                    (decide-line! (car os) (car ls) (trim-left (car ls)) (car j) (cdr j)))
                  (loop (cdr os) (cdr ls) (+ k 1))))))
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
          ;; the ecmascript profiles take their own scan; every other profile
          ;; the table scan below, line by line, and its end-of-file check
          (if (equal? (get 'lexer #f) "ecmascript")
              (ecmascript-pass)
              (begin
          (for-each
            (lambda (row)
              (let* ((offset (+ prefix (car row)))
                     (line (safe-utf8 (byte-slice bytes offset (+ prefix (cadr row)))))
                     (trimmed (trim-left line))
                     ;; the first level of a designated body form counts as the
                     ;; top level for the regex supplier only: supplied starts
                     ;; keep depth zero (the editor-symbol contract)
                     (top? (and (or (null? stack)
                                    (and (not starts) (eq? designation 'designated) (= (length stack) 1)))
                                (not quoted) (zero? comment-depth) (not continued?)
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
                      (decide-line! offset line trimmed top? continues?)
                      (scan-line line offset)
                      (unless (string=? trimmed "")
                        ;; a line that ends in a character the escape made
                        ;; literal ends in that character, not in an operator
                        (set! continues? (and (eq? last-kind 'code)
                                              (not (eqv? last-escaped (- code-end 1)))
                                              (continuation? (substring line 0 code-end)))))))))
            (byte-lines (byte-slice bytes prefix (bytevector-length bytes))))
          (when (or (pair? stack) quoted (> comment-depth 0) continued? fence)
            (fallback 'unbalanced (bytevector-length bytes)))))
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
                (list start end (list-ref d 3) (list-ref d 4))))))
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
  ;;
  ;; A BLOCK CUT AT A SYMBOL THAT THE LANGUAGE DOES NOT NAME TAKES THE
  ;; SYMBOL'S NAME AS ITS TITLE. A block's name is derived from its source
  ;; (text-properties, as the view derives it), and where it is derived it
  ;; stays in charge: a title stored there would outrank it and keep the
  ;; first import's name after an edit renamed the definition. So the
  ;; editor's name is the block's title only where the derivation answers
  ;; nothing (a class, a const, an export default the definition patterns
  ;; do not read). The first block holds the protected prefix, and its
  ;; symbol is the LAST one that starts within the prefix or at its end: a
  ;; symbol starting inside the prefix names the header matter there (a
  ;; shebang, a header comment), and the definition the block carries is
  ;; the one that starts where the prefix ends. Every other block holds at
  ;; most one start, its own first byte. A block that does not start at a
  ;; symbol, and a symbol named "", gets #f.
  ;; -> (<cuts> <titles>): the cuts, 0 and the file's size included, and
  ;;    for each block they make, in order, its title or #f.
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
        (let* ((all (append (list 0) cuts (list size)))
               (title (lambda (from to)
                        (let* ((at (filter (lambda (s) (if (= from 0) (<= (car s) prefix) (= (car s) from))) symbols))
                               (sym (and (pair? at) (car (reverse at)))))
                          (and sym (not (string=? (cadddr sym) ""))
                               (not (car (text-properties entry (byte-slice bytes from to))))
                               (cadddr sym))))))
          (list all (map title (reverse (cdr (reverse all))) (cdr all)))))))

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
