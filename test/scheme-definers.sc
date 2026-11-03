#!chezscheme
;; Copyright 2018 - 2026 guenchi
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

;; The Scheme definers beyond define and define-syntax: define-checked,
;; define-record-type in both spellings, define-condition-type,
;; define-values, define-enumeration, define-integrable and define-structure
;; bind a name at the top level, so split-suggest cuts before each and the
;; block's name is the bound one; names, uses and context then see them. The
;; file scheme-definers/consumer.sc has the shape of a consumer's file that
;; held five define-checked forms in its neighbour's block. CX rows: an entry
;; head names a code block that has no title by its name.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch register-verbs!)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia languages) language-for-name register-language!)
        (only (theourgia render) render-human render-wire)
        (only (theourgia text-code) definition-name)
        (only (theourgia reduce) block-id))

(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/scheme-definers-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'scheme-definers "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (write-file! path text)
  (sh "mkdir -p " (quoted (path-parent path)))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (file-text path)
  (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b))))))
(define n 0)
;; -> the directory of a fresh store, its home set
(define (fresh-store!)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")))
    (sh "mkdir -p " (quoted st) " " (quoted (string-append d "/home")) " " (quoted (string-append d "/raw"))
        " " (quoted (string-append d "/src")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (ask st 'init)
    d))
(define (store-in d) (string-append d "/store"))
;; split-suggest's boundaries for TEXT written at <d>/raw/<path>
(define (boundaries-of d path text)
  (write-file! (string-append d "/raw/" path) text)
  (let ((c (clause-of (ask (store-in d) 'split-suggest (string-append d "/raw/" path)) 'boundaries)))
    (if c (cadr c) 'NO-BOUNDARIES)))
;; TEXT imported split at split-suggest's cuts: its review copy, imported in its place
(define (split-import! d path text)
  (let ((st (store-in d)))
    (write-file! (string-append d "/raw/" path) text)
    (sh "mkdir -p " (quoted (path-parent (string-append d "/src/" path))))
    (ask st 'split-suggest (string-append d "/raw/" path) "--output" (string-append d "/src/" path))
    (unless (eq? 'ok (car (ask st 'import-code (string-append d "/src"))))
      (assertion-violation 'scheme-definers "split import failed" path))
    st))
(define (file-at st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state))))
(define (children-at st path) (code-children (open-and-reduce st) (file-at st path)))
(define (names-at st path)
  (let ((state (open-and-reduce st))) (map (lambda (k) (code-field state k 'name)) (children-at st path))))
;; the child of PATH whose src begins with PREFIX, or #f
(define (child-starting st path prefix)
  (let ((state (open-and-reduce st)))
    (find (lambda (k)
            (let ((s (utf8->string (code-field state k 'src))))
              (and (>= (string-length s) (string-length prefix)) (string=? prefix (substring s 0 (string-length prefix))))))
          (children-at st path))))

;; ---- SD-1..SD-3: where the cuts are -----------------------------------------------------
(printf "== SD-1..SD-3: the definers at the top level of a library body ==\n")
(define d1 (fresh-store!))
(define definers
  (string-append
    "(library (t)\n  (export a b c d &e f h i)\n  (import (chezscheme))\n"
    "  (define (a) 1)\n  (define-checked (b [x string?]) x)\n  (define-record-type c (fields x))\n"
    "  (define-record-type (d make-d d?) (fields y))\n  (define-condition-type &e &error make-e e?)\n"
    "  (define-values (f g) (values 1 2))\n  (define-enumeration h (r s) h-set)\n  (define-integrable (i x) x))\n"))
;; the base cuts only before `(define (a) 1)`: (0 65)
(want "SD-1 a cut before each definer at the library body's top level, at the start of its line"
      (boundaries-of d1 "definers.sc" definers)
      '(0 65 82 119 155 203 249 286 323))
(want "SD-1 each block is named by the name its form binds: a record type, a values form by the first"
      (cdr (names-at (split-import! d1 "definers.sc" definers) "definers.sc"))
      '("a" "b" "c" "d" "&e" "f" "h" "i"))
(want "SD-1 Chez's define-structure is a cut as well"
      (boundaries-of d1 "s.ss" "(define (p) 1)\n(define-structure (pt x y))\n")
      '(0 15))
;; the base: only the define and define-syntax heads name (a, m), the rest #f
(want "SD-1 both Scheme entries, scheme and chez, name each definer's form by the name it binds"
      (map (lambda (lang)
             (map (lambda (s) (definition-name (language-for-name lang) s))
                  '("(define (a) 1)" "(define-syntax m (syntax-rules ()))" "(define-checked (b [x string?]) x)"
                    "(define-record-type c (fields x))" "(define-record-type (d make-d d?) (fields y))"
                    "(define-condition-type &e &error make-e e?)" "(define-values (f g) (values 1 2))"
                    "(define-enumeration h (r s) h-set)" "(define-integrable (i x) x)" "(define-integrable j (lambda (x) x))"
                    "(define-structure (pt x y))" "(define-values (only-first second third) (values 1 2 3))"
                    ;; a longer definer's name is not a definer: each pattern needs the space after its head
                    "(define-checkedx (k) 1)" "(define-record-types r (fields x))" "(define-condition-typed &q &error)"
                    "(define-values* (v w) 1)" "(define-enumerations e (x) e-set)" "(define-integrable-x (n) n)"
                    "(define-structures (s a))" "(define-syntaxes (m) 1)")))
           '(scheme chez))
      (let ((names '("a" "m" "b" "c" "d" "&e" "f" "h" "i" "j" "pt" "only-first" #f #f #f #f #f #f #f #f)))
        (list names names)))
;; CONTROLS, green on the base as well: the scan is unchanged
(want "SD-2 CONTROL a define-checked inside a string and inside a comment line is not a cut"
      (boundaries-of d1 "quoted.sc"
                     "(define (a) \"x\n(define-checked (fake) 1)\ny\")\n;; (define-checked (gone) 1)\n(define (b) 2)\n")
      '(0 45))
(define nested
  "(define (outer)\n  (let ((x 1))\n    (define (inner) x)\n    (define-checked (inner2 [y integer?]) y)\n    (inner)))\n(define (next) 2)\n")
(want "SD-3 CONTROL indented in a let body neither is a cut, and an editor's start at either is refused symbol-not-top-level alike"
      (let ((scheme (language-for-name 'scheme)) (bytes (string->utf8 nested)))
        (list (car (suggest-boundaries scheme bytes))
              (let ((r (suggest-boundaries scheme bytes '(0 31 54 113))))
                (list (car r) (filter (lambda (w) (and (pair? w) (eq? (car w) 'symbol-not-top-level))) (cadr r))))))
      '((0 113) ((0 113) ((symbol-not-top-level (at 31)) (symbol-not-top-level (at 54))))))

;; ---- SD-4..SD-6: the consumer's file ---------------------------------------------------------
(printf "== SD-4..SD-6: a consumer's file of five define-checked forms and a define ==\n")
(define consumer (file-text "scheme-definers/consumer.sc"))
(define d2 (fresh-store!))
;; the base: (0 1463), the define alone
(want "SD-4 the consumer's file is cut before each of its six definitions, a comment above one going with it"
      (boundaries-of d2 "consumer.sc" consumer)
      '(0 412 767 968 1091 1226 1463))
(define st2 (split-import! d2 "consumer.sc" consumer))
(want "SD-4 its blocks after the header are named by their definitions"
      (cdr (names-at st2 "consumer.sc"))
      '("money-op!" "refund-op!" "hold-op!" "release-op!" "settle-op!" "run-day"))
(define money (child-starting st2 "consumer.sc" "  ;; the first definition"))
(define refund (child-starting st2 "consumer.sc" "  (define-checked (refund-op!"))
(define run-day (child-starting st2 "consumer.sc" "  (define (run-day"))
(define uses (let ((a (ask st2 'uses "money-op!")))
               (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) 'use))) (cdr (or (clause-of a 'items) '(items)))))))
(want "SD-5 uses money-op! answers each caller's own block, refund-op!'s and run-day's"
      (list (and refund (member refund uses) #t) (and run-day (member run-day uses) #t))
      '(#t #t))
(define ctx (ask st2 'context "--for" (or run-day "none") "--budget" "6000"))
(define (entries-of a key) (let ((c (clause-of a key))) (if (and c (pair? (cdr c))) (cadr c) '())))
(want "SD-6 context --for run-day's block names money-op!'s block as the definition it uses"
      (let ((e (find (lambda (e) (equal? (car e) money)) (entries-of ctx 'background))))
        (and e (clause-of (cons 'entry (cdr e)) 'why)))
      (list 'why 'defines "money-op!" run-day))

;; ---- CX: an entry's title ----------------------------------------------------------------
;; A store of plain defines, which the base cuts as well, so these rows turn
;; on the title alone. THE BASE prints (title "") for a named code block.
(printf "== CX: a named code block with no title is titled by its name ==\n")
(define d3 (fresh-store!))
(define st3 (split-import! d3 "cx.sc" "(define (helper) 1)\n(define (caller) (helper))\n"))
(define helper (child-starting st3 "cx.sc" "(define (helper)"))
(define caller (child-starting st3 "cx.sc" "(define (caller)"))
(define (title-of e) (let ((c (clause-of (cons 'entry (cdr e)) 'title))) (and c (cadr c))))
(define cx (ask st3 'context "--for" (or caller "none") "--budget" "6000"))
(define (entry-for a id) (find (lambda (e) (equal? (car e) id)) (entries-of a 'background)))
(want "CX-1 the for head of a code block with a name and no title carries the name as its title"
      (let ((f (clause-of cx 'for))) (and f (title-of (cadr f))))
      "caller")
(want "CX-1 so does an entry the answer lists for it, the definition it uses"
      (let ((e (entry-for cx helper))) (and e (title-of e)))
      "helper")
(want "CX-2 CONTROL a block with neither keeps the empty title: the file"
      (let ((e (entry-for cx (file-at st3 "cx.sc")))) (and e (title-of e)))
      "")
;; THE HUMAN FORM, through the CLI as a person runs it: "  <id> <level> <title>"
(define (cli-out d . argv)
  (let ((out (string-append d "/cli.out")))
    (sh "cd " (quoted (store-in d)) " && THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted (string-append d "/home"))
        " scheme --script " (quoted (string-append (path-parent (current-directory)) "/theourgia.sc"))
        (apply string-append (map (lambda (a) (string-append " " (quoted a))) argv))
        " > " (quoted out) " 2>/dev/null < /dev/null")
    (file-text out)))
(define (lines-of s)
  (let loop ((i 0) (from 0) (out '()))
    (cond ((= i (string-length s)) (reverse (if (< from i) (cons (substring s from i) out) out)))
          ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s from i) out)))
          (else (loop (+ i 1) from out)))))
(want "CX-3 the human form prints the name where the title goes, on the definition's line"
      (and helper
           (member (string-append "  " helper " summary helper")
                   (lines-of (cli-out d3 "context" "--for" caller "--budget" "6000")))
           #t)
      #t)
;; NOT ONLY CODE: a block of another kind with a stored name and no title (freeze1 gated the fallback on code)
(define named (let* ((a (ask st3 'insert "--title" "temporary" "--text" "a block of prose"))
                     (ev (let ((c (clause-of a 'events))) (and c (car (cadr c))))))
                (and ev (block-id (car ev) (cdr ev)))))
(want "CX-1 a block of another kind with a name and no title is titled by its name"
      ;; insert asks for a title; the row empties it, then names the block
      (begin (and named (ask st3 'set named "title" "") (ask st3 'set named "name" "Overview"))
             (let ((f (clause-of (ask st3 'context "--for" (or named "none") "--budget" "6000") 'for)))
               (list (and f (cadr (clause-of (cons 'entry (cdr (cadr f))) 'kind))) (and f (title-of (cadr f))))))
      '(section "Overview"))
(want "CX-4 CONTROL a code block with a title keeps it"
      (begin (ask st3 'set caller "title" "The caller")
             (let ((f (clause-of (ask st3 'context "--for" caller "--budget" "6000") 'for))) (and f (title-of (cadr f)))))
      "The caller")

;; THE FALLBACK ITSELF, as context.sc's title-from: looked up when a row calls
;; it, so the file loads on the base (where these rows read red)
(define (title-from . args) (apply (eval 'title-from (environment '(theourgia context))) args))
(want "CX-5 a symbol name, a string name, a list's first, an empty or invalid name, and a title before a name"
      (list (title-from "" 'sym) (title-from "" "str") (title-from "" '(first second)) (title-from "" '("one" two))
            (title-from "" '()) (title-from "" 42) (title-from "" '(1 2)) (title-from "" #f) (title-from "Kept" 'sym))
      '("sym" "str" "first" "one" "" "" "" "" "Kept"))
(define d4 (fresh-store!))
(write-file! (string-append d4 "/src/datum.sc") "(define (sym-def) 1)\n")
(ask (store-in d4) 'import-code (string-append d4 "/src") "--datum")
(want "CX-6 a datum block, whose view's name is a symbol, is titled by its text"
      (let* ((st (store-in d4))
             ;; a datum import makes a library block holding a code block: the outline's code row
             (k (let ((r (find (lambda (r) (and (pair? r) (eq? (car r) 'row) (eq? (caddr r) 'code)))
                               (cdr (or (clause-of (ask st 'outline) 'rows) '(rows))))))
                  (and r (cadr r))))
             (f (and k (clause-of (ask st 'context "--for" k "--budget" "6000") 'for))))
        (list (and k (code-field (open-and-reduce st) k 'name)) (and f (title-of (cadr f)))))
      '(sym-def "sym-def"))
;; A NAME WITH A DOUBLE QUOTE IN IT: a language whose head captures one
(register-language! '((lang "quoted-names") (extensions ("qn")) (line-comment "#")
                      (def-heads ("^def ([^ ]+)")) (name-capture 1)))
(define d5 (fresh-store!))
(write-file! (string-append d5 "/src/q.qn") "def a\"b\n")
(ask (store-in d5) 'import-code (string-append d5 "/src"))
(define q-id (let ((ks (children-at (store-in d5) "q.qn"))) (and (pair? ks) (car ks))))
(define q-ctx (ask (store-in d5) 'context "--for" (or q-id "none") "--budget" "6000"))
(want "CX-7 a name holding a double quote: the wire, read back, gives it whole; the human id line prints it as text"
      (list (let ((f (clause-of (guard (e (#t #f)) (read (open-string-input-port (render-wire q-ctx)))) 'for)))
              (and f (title-of (cadr f))))
            (and q-id (member (string-append "  " q-id " full a\"b") (lines-of (render-human q-ctx))) #t))
      '("a\"b" #t))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nscheme-definers complete\n" rows bad)
(exit (if (zero? bad) 0 1))
