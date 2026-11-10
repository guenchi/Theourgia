#!r6rs
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

;; Spans on the parsed tree, verbatim raw-md, one supplier of references.
;;
;; md->blocks answers every block node and every reference with
;; `(span <start> <end>)`: half-open byte offsets into the UTF-8 encoding of
;; the src it was given, after the node's scalar elements and before its
;; children. A block's span runs from the start of its first line to the end
;; of its last line's terminator, container prefixes included; a reference's
;; from the first byte of `[[` to the byte after `]]`. A raw-md node's text
;; is the bytes of its span, and it carries `(refs <ref> ...)`. md-ref-nodes
;; answers the tree's reference nodes in document order; it replaces the
;; line lexer as the supplier of the references `refs` and `query` derive.
;;
;; EXPECTED VALUES ARE LITERALS. Every span below was computed from the
;; bytes of the source it is compared with and is written out; nothing
;; expected is taken from the answer under test.
;;
;; RED FIRST. Rows that read a span, a raw-md text or a refs clause are red
;; on the base by shape. A row green on the base is marked PIN and stands
;; beside a row that is red there, or is a CONTROL.
;;
;; THE MEMORY CORPUS. SP-5c's second half and SP-8 read a frozen copy of a
;; private store, named by THEOURGIA_MD_CORPUS_STORE: a git archive of the
;; memory-store at commit 1711cb04a635ce98a64d2a004d5a88eaa9ee8de6, 40
;; files, applied cut ((nxddg1k2 . 415) (xq56ijd1 . 3654)). It is copied into
;; this run's scratch root and read there. Unset, those rows print SKIP and
;; how to make the copy. Diagnostics name block ids and byte offsets, never
;; block text.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-outline reduce-applied-cut block-id block-hash)
        (only (theourgia md) md->blocks blocks->md))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/ast-spans-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- helpers ------------------------------------------------------------------------

;; LOOKED UP WHEN A ROW CALLS IT, not imported: on a tree without it the rows
;; that use it are red and the rest run.
(define (md-ref-nodes . args)
  (apply (eval 'md-ref-nodes (environment '(theourgia md))) args))
(define (ast-with-targets . args)
  (apply (eval 'ast-with-targets (environment '(theourgia reduce))) args))

(define (bytes . xs) (utf8->string (u8-list->bytevector xs)))
(define (span? x) (and (pair? x) (eq? (car x) 'span)))
;; Every span removed, at any depth.
(define (erase x)
  (cond ((pair? x) (map erase (filter (lambda (y) (not (span? y))) x)))
        (else x)))
;; Each raw-md text that does not end in LF gets one, at any depth.
(define (terminate x)
  (cond ((and (pair? x) (eq? (car x) 'raw-md) (pair? (cdr x)) (string? (cadr x)))
         (let ((t (cadr x)))
           (cons* 'raw-md
                  (if (and (> (string-length t) 0) (char=? (string-ref t (- (string-length t) 1)) #\newline))
                      t
                      (string-append t "\n"))
                  (map terminate (cddr x)))))
        ((pair? x) (map terminate x))
        (else x)))
(define (contract? t) (equal? (erase (md->blocks (blocks->md t))) (erase (terminate t))))
;; The span of a node, the first element headed span, or #f.
(define (span-of x) (and (pair? x) (list? x) (find span? x)))
;; Every node of a tree with a span, in document order, raw-md's refs included.
(define (spanned tree)
  (let walk ((xs tree) (out '()))
    (cond ((null? xs) out)
          ((and (pair? (car xs)) (list? (car xs)))
           (let ((x (car xs)))
             (walk (cdr xs) (walk (cdr x) (if (span-of x) (cons x out) out)))))
          (else (walk (cdr xs) out)))))
(define (all-nodes tree) (reverse (spanned tree)))
;; Every child span within its parent's, over the whole tree.
(define (nested? tree)
  (let check ((x (cons 'top tree)) (outer #f))
    (if (not (and (pair? x) (list? x)))
        #t
        (let* ((s (span-of x))
               (inside (or (not s) (not outer) (and (<= (cadr outer) (cadr s)) (<= (caddr s) (caddr outer))))))
          (and inside (for-all (lambda (c) (check c (or s outer))) (cdr x)))))))
(define (refs-in tree) (filter (lambda (x) (eq? (car x) 'ref)) (all-nodes tree)))
(define (slice text s e)
  (let ((b (string->utf8 text)))
    (utf8->string (let ((o (make-bytevector (- e s)))) (bytevector-copy! b s o 0 (- e s)) o))))
(define (raw-mds tree) (filter (lambda (x) (eq? (car x) 'raw-md)) (all-nodes tree)))
;; One paragraph's text, or what the tree is when it is not one.
(define (one-para tree)
  (if (and (= (length tree) 1) (eq? (car (car tree)) 'para)) (erase (car tree)) (list 'NOT-ONE-PARA (erase tree))))

;; ---- SP-1 block spans --------------------------------------------------------------

(define sp1-lines
  '("Lead para line one\n" "continues here.\n" "\n" "- item one\n" "  continued\n" "- item two\n" "\n"
    "```scheme\n" "(f x)\n" "```\n" "\n" "> quoted para\n" "> - qa\n" "> - qb\n" "\n" "---\n" "\n"
    "| a | b |\n" "| 1 | 2 |\n" "\n" "    indented code\n" "\n" "Closing para.\n"))
(define (crlf s) (let ((n (string-length s))) (if (and (> n 0) (char=? (string-ref s (- n 1)) #\newline)) (string-append (substring s 0 (- n 1)) "\r\n") s)))
(define sp1 (apply string-append sp1-lines))
(define sp1-crlf (apply string-append (map crlf sp1-lines)))
(define sp1-tree
  '((para (span 0 35) "Lead para line one\ncontinues here.")
    (list #f 1 (span 36 70) (item (span 36 59) (para (span 36 59) "item one\ncontinued")) (item (span 59 70) (para (span 59 70) "item two")))
    (code "```" "scheme" "(f x)" (span 71 91))
    (quote (span 92 120) (para (span 92 106) "quoted para") (list #f 1 (span 106 120) (item (span 106 113) (para (span 106 113) "qa")) (item (span 113 120) (para (span 113 120) "qb"))))
    (hr (span 121 125))
    (raw-md "| a | b |\n| 1 | 2 |\n" (span 126 146) (refs))
    (raw-md "    indented code\n" (span 147 165) (refs))
    (para (span 166 180) "Closing para.")))
(define sp1-crlf-tree
  '((para (span 0 37) "Lead para line one\ncontinues here.")
    (list #f 1 (span 39 76) (item (span 39 64) (para (span 39 64) "item one\ncontinued")) (item (span 64 76) (para (span 64 76) "item two")))
    (code "```" "scheme" "(f x)" (span 78 101))
    (quote (span 103 134) (para (span 103 118) "quoted para") (list #f 1 (span 118 134) (item (span 118 126) (para (span 118 126) "qa")) (item (span 126 134) (para (span 126 134) "qb"))))
    (hr (span 136 141))
    (raw-md "| a | b |\r\n| 1 | 2 |\r\n" (span 143 165) (refs))
    (raw-md "    indented code\r\n" (span 167 186) (refs))
    (para (span 188 203) "Closing para.")))

(want "SP-1 every kind, each node's span the literal byte pair, raw-md verbatim"
      (md->blocks sp1)
      sp1-tree)
(want "SP-1 the same src with CRLF: every endpoint moves by the CRs before it, raw-md keeps its CRLF"
      (md->blocks sp1-crlf)
      sp1-crlf-tree)
(want "SP-1 the CRLF tree with spans erased is the LF tree's but for raw-md's bytes"
      (list (erase (md->blocks sp1-crlf)) (erase sp1-tree))
      (list (erase sp1-crlf-tree) (erase sp1-tree)))
(want "SP-1 a CJK character before a boundary: the endpoints are bytes, not characters"
      (md->blocks "\x7532;\x4E59; para\n\n> \x4E19; q\n")
      '((para (span 0 12) "\x7532;\x4E59; para") (quote (span 13 21) (para (span 13 21) "\x4E19; q"))))
(want "SP-1 a last line with no terminator ends at the src's length"
      (md->blocks "a\n\nb")
      '((para (span 0 2) "a") (para (span 3 4) "b")))
(want "SP-1 a blank line between two items lies in the list and in neither item; the blank after the last item is in no node"
      (md->blocks "- a\n\n- b\n\nafter\n")
      '((list #f 1 (span 0 9) (item (span 0 4) (para (span 0 4) "a")) (item (span 5 9) (para (span 5 9) "b")))
        (para (span 10 16) "after")))
(want "SP-1 every child's span lies within its parent's, over the whole tree, both line ends"
      (list (nested? (md->blocks sp1)) (nested? (md->blocks sp1-crlf)) (length (all-nodes (md->blocks sp1))))
      '(#t #t 18))
(want "SP-1 an unclosed fence runs to the src's end"
      (md->blocks "a\n\n```\nx\n")
      '((para (span 0 2) "a") (code "```" "" "x" (span 3 9))))

;; ---- SP-2 reference spans ----------------------------------------------------------

(want "SP-2 one reference on one line"
      (md->blocks "[[x]]\n")
      '((para (span 0 6) (ref "[[x]]" "x" (span 0 5)))))
(want "SP-2 two references on one line"
      (md->blocks "[[a]] and [[b]]\n")
      '((para (span 0 16) (ref "[[a]]" "a" (span 0 5)) " and " (ref "[[b]]" "b" (span 10 15)))))
(want "SP-2 a reference after a multibyte prefix on its line"
      (md->blocks "\x7532; [[x]]\n")
      '((para (span 0 10) "\x7532; " (ref "[[x]]" "x" (span 4 9)))))
(want "SP-2 a reference inside em inside a list item inside a quote: the sub-parse rebased to the top src"
      (md->blocks "> - *e [[x]]*\n")
      '((quote (span 0 14) (list #f 1 (span 0 14) (item (span 0 14) (para (span 0 14) (em "e " (ref "[[x]]" "x" (span 7 12)))))))))
(want "SP-2 a reference inside strong and one inside em on the same line, each sub-parse rebased"
      (md->blocks "**s [[a]]** *e [[b]]*\n")
      '((para (span 0 22) (strong "s " (ref "[[a]]" "a" (span 4 9))) " " (em "e " (ref "[[b]]" "b" (span 15 20))))))
(want "SP-2 a lexeme across a line inside a quote: the interval contains the interior prefix"
      (md->blocks "> [[foo\n> bar]]\n")
      '((quote (span 0 16) (para (span 0 16) (ref "[[foo\nbar]]" "foo\nbar" (span 2 15))))))
(want "SP-2 after an escape and an entity on its line, the position is the cursor's before decoding"
      (md->blocks "\\* &amp; [[x]]\n")
      '((para (span 0 15) "* & " (ref "[[x]]" "x" (span 9 14)))))

;; ---- SP-3 code regions ---------------------------------------------------------------

(define sp3-code
  (list "```\n[[x]]\n```\n" "    [[x]]\n" "a `[[x]]` b\n" "a `x\n[[y]]` b\n" "> ```\n> [[x]]\n> ```\n" "\\[[x]]\n"))
(want "SP-3 PIN no reference node in a fence, indented code, a code span, a multi-line code span, a quoted fence, after an escape"
      (map (lambda (s) (length (filter (lambda (x) (and (pair? x) (eq? (car x) 'ref))) (let flat ((x (md->blocks s))) (if (pair? x) (cons x (apply append (map (lambda (y) (if (pair? y) (flat y) '())) x))) '())))))
           sp3-code)
      '(0 0 0 0 0 0))
(want "SP-3 md-ref-nodes answers nothing for each of them (the last three are the line lexer's errors)"
      (map md-ref-nodes sp3-code)
      '(() () () () () ()))
(want "SP-3 the indented block's raw-md carries (refs) and no reference"
      (md->blocks "    [[x]]\n")
      '((raw-md "    [[x]]\n" (span 0 10) (refs))))
(want "SP-3 CONTROL: the same text outside code is a reference, one each"
      (map (lambda (s) (length (md-ref-nodes s))) (list "[[x]]\n" "a [[x]] b\n" "a x\n[[y]] b\n" "> [[x]]\n"))
      '(1 1 1 1))

;; ---- SP-4 raw-md verbatim ------------------------------------------------------------

(define table-crlf "| a | [[foo]] |\r\n| 1 | 2 |\r\n")
(define quoted-table (string-append "> | a | [[foo]] |\r\n" "> | 1 | 2 |\r\n"))
(want "SP-4 a table with CRLF and a reference is one raw-md, its text the source slice, its ref spanned"
      (md->blocks table-crlf)
      `((raw-md ,table-crlf (span 0 28) (refs (ref "[[foo]]" "foo" (span 6 13))))))
(want "SP-4 the same table in a quote keeps every prefix in its text"
      (md->blocks quoted-table)
      `((quote (span 0 32) (raw-md ,quoted-table (span 0 32) (refs (ref "[[foo]]" "foo" (span 8 15)))))))
(want "SP-4 a table's reference span names the lexeme's bytes in the src"
      (map (lambda (r) (let ((s (span-of r))) (slice quoted-table (cadr s) (caddr s)))) (refs-in (md->blocks quoted-table)))
      '("[[foo]]"))
(define sp4-doc (string-append "lead\n\n" quoted-table "\n- " "| x |\n" "\n    code\r\n\n" "<div>[[h]]</div>\n" "\n[^1]: note [[f]]\n"))
(want "SP-4 text == the src's bytes between the span's ends, for every raw-md of a tree"
      (map (lambda (r) (let ((s (span-of r))) (equal? (cadr r) (slice sp4-doc (cadr s) (caddr s))))) (raw-mds (md->blocks sp4-doc)))
      '(#t #t #t #t #t))
(want "SP-4 an HTML block's and a footnote's references are in their refs clauses"
      (map (lambda (r) (map (lambda (x) (cadr x)) (cdr (assq 'refs (filter pair? (cdr r)))))) (raw-mds (md->blocks sp4-doc)))
      '(("[[foo]]") () () ("[[h]]") ("[[f]]")))

;; ---- SP-5 printing ---------------------------------------------------------------------

(want "SP-5 the parsed SP-1 tree printed and parsed again equals it, spans erased and raw-md terminated"
      (list (contract? (md->blocks sp1)) (contract? (md->blocks sp1-crlf))
            (map car (md->blocks sp1)))
      '(#t #t (para list code quote hr raw-md raw-md para)))
(want "SP-5 the CRLF table in a quote prints back byte for byte: no prefix added, no LF added"
      (blocks->md (md->blocks quoted-table))
      quoted-table)
(want "SP-5 the EOF table: parsed without a terminator, printed with one, parsed again with it"
      (let* ((t (md->blocks "| a |")) (p (blocks->md t)) (t2 (md->blocks p)))
        (list t p t2 (equal? (erase t2) (erase (terminate t))) (equal? (erase t2) (erase t))))
      '(((raw-md "| a |" (span 0 5) (refs))) "| a |\n" ((raw-md "| a |\n" (span 0 6) (refs))) #t #f))
(want "SP-5 a hand-built raw-md without a terminator prints with one and reads back before the next block"
      (let ((p (blocks->md '((raw-md "| a |") (para "next")))))
        (list p (erase (md->blocks p))))
      '("| a |\n\nnext\n" ((raw-md "| a |\n" (refs)) (para "next"))))
(want "SP-5 a hand-built quote holding a raw-md with its own prefixes prints them once"
      (blocks->md '((quote (raw-md "> | a |\n> | b |\n"))))
      "> | a |\n> | b |\n")
(want "SP-5 the fence witness: a body ending in a blank line prints back and reads back the same"
      (let* ((s "```\nx\n\n```\n") (t (md->blocks s)))
        (list t (blocks->md t) (erase (md->blocks (blocks->md t)))))
      '(((code "```" "" "x\n" (span 0 11))) "```\nx\n\n```\n" ((code "```" "" "x\n"))))
(want "SP-5 PIN beside it: an empty body prints as fence LF fence, a body x as today"
      (list (blocks->md '((code "```" "" ""))) (blocks->md '((code "```" "" "x"))))
      '("```\n```\n" "```\nx\n```\n"))
(want "SP-5 the nested witness: an item holding a quote holding a table, no terminator at the end, keeps the contract"
      (let ((t (md->blocks "- > | a |")))
        (list t (contract? t) (blocks->md t)))
      '(((list #f 1 (span 0 9) (item (span 0 9) (quote (span 0 9) (raw-md "- > | a |" (span 0 9) (refs))))))
        #t "- > | a |\n"))
(want "SP-5 the hand-built mirror: an item holding a quote holding a raw-md with its own prefixes prints them once"
      (blocks->md '((list #f 1 (item (quote (raw-md "- > | a |\n"))))))
      "- > | a |\n")
(want "SP-5 the quoted-code witness: a line of spaces in a quoted fence prints back and reads back the same"
      (let* ((s "> ```\n>   \n> ```\n") (t (md->blocks s)))
        (list t (blocks->md t) (erase (md->blocks (blocks->md t)))))
      '(((quote (span 0 17) (code "```" "" "  " (span 0 17)))) "> ```\n>   \n> ```\n" ((quote (code "```" "" "  ")))))
(want "SP-5 PIN beside it: two quoted paragraphs still print > between them and read back as two"
      (let ((p (blocks->md '((quote (para "a") (para "b"))))))
        (list p (erase (md->blocks p))))
      '("> a\n>\n> b\n" ((quote (para "a") (para "b")))))
(want "SP-5 the two-block item witness: a paragraph and a fence in one item print back and read back the same"
      (let* ((s "- a\n  ```\n  x\n  ```\n") (t (md->blocks s)))
        (list t (blocks->md t) (erase (md->blocks (blocks->md t)))))
      '(((list #f 1 (span 0 20) (item (span 0 20) (para (span 0 4) "a") (code "```" "" "x" (span 4 20)))))
        "- a\n  ```\n  x\n  ```\n"
        ((list #f 1 (item (para "a") (code "```" "" "x"))))))

(want "SP-5 the adjacent-lists witness: two lists the source separates by two blank lines print back and read back as two"
      (let* ((s "- a\n\n\n- b\n") (t (md->blocks s)))
        (list t (blocks->md t) (erase (md->blocks (blocks->md t)))))
      '(((list #f 1 (span 0 4) (item (span 0 4) (para (span 0 4) "a"))) (list #f 1 (span 6 10) (item (span 6 10) (para (span 6 10) "b"))))
        "- a\n\n\n- b\n"
        ((list #f 1 (item (para "a"))) (list #f 1 (item (para "b"))))))
(want "SP-5 the same two lists inside a quote print back byte for byte and read back as two"
      (let* ((s "> - a\n>\n>\n> - b\n") (t (md->blocks s)))
        (list (blocks->md t) (erase (md->blocks (blocks->md t)))))
      '("> - a\n>\n>\n> - b\n" ((quote (list #f 1 (item (para "a"))) (list #f 1 (item (para "b")))))))
(want "SP-5 PIN beside it: one blank line between two items is one list, printed as today with none"
      (let ((t (md->blocks "- a\n\n- b\n")))
        (list (erase t) (blocks->md t)))
      '(((list #f 1 (item (para "a")) (item (para "b")))) "- a\n- b\n"))

;; ---- SP-5d the one known loss -------------------------------------------------------------

;; THE PARSER'S NEXT SLICE FLIPS THIS ROW. take-list's different-kind branch strips `1. ` and
;; keeps `- b` in the item, which no print spells back: printed with no
;; blank between an item's children it reads as two sibling items.
(want "SP-5d PIN mixed stacked markers parse to one item holding a nested list, and the print reads back as two items"
      (let* ((t (md->blocks "- a\n  1. - b\n")) (p (blocks->md t)))
        (list (erase t) (contract? t) (length (cdddr (erase (car (md->blocks p)))))))
      '(((list #f 1 (item (para "a") (list #f 1 (item (para "b")))))) #f 2))

;; ---- SP-5e line-initial markers and the printer's four spellings ----------------------------

;; Each source is one paragraph; it prints back byte for byte (or as the
;; stated bytes) and parses again to the same paragraph.
(define (spell src) (let* ((t (md->blocks src)) (p (blocks->md t))) (list (one-para t) p (one-para (md->blocks p)))))
(define (identical src text) (list (list 'para text) src (list 'para text)))
(want "SP-5e an escaped ordered marker, digits then . or ), prints its escape"
      (list (spell "1\\. a\n") (spell "2\\) b\n"))
      (list (identical "1\\. a\n" "1. a") (identical "2\\) b\n" "2) b")))
(want "SP-5e a table start, four spaces, a space before a bullet and a tilde fence print their escapes"
      (list (spell "\\| a |\n") (spell "\\    a\n") (spell " \\- a\n") (spell "\\~~~\n"))
      (list (identical "\\| a |\n" "| a |") (identical "\\    a\n" "    a")
            (identical " \\- a\n" " - a") (identical "\\~~~\n" "~~~")))
(want "SP-5e after an LF inside the text: an hr shape, a bullet, a quote, an ordered marker"
      (list (spell "a\n\\---\n") (spell "a\n\\- b\n") (spell "a\n\\> q\n") (spell "a\n1\\. b\n"))
      (list (identical "a\n\\---\n" "a\n---") (identical "a\n\\- b\n" "a\n- b")
            (identical "a\n\\> q\n" "a\n> q") (identical "a\n1\\. b\n" "a\n1. b")))
(want "SP-5e whitespace: a one-space paragraph, a space line inside the text, an empty line inside it"
      (list (spell "\\ \n") (spell "a\n\\ \nb\n") (spell (bytes #x61 #x0a #x5c #x0a #x62 #x0a)))
      (list (identical "\\ \n" " ") (identical "a\n\\ \nb\n" "a\n \nb")
            (identical (bytes #x61 #x0a #x5c #x0a #x62 #x0a) "a\n\nb")))
(want "SP-5e a tab-indented paragraph takes one index-0 escape, and a tab then a bullet no second one"
      (list (spell (bytes #x5c #x09 #x61 #x0a)) (spell (bytes #x5c #x09 #x2d #x20 #x61 #x0a)))
      (list (identical (bytes #x5c #x09 #x61 #x0a) "\ta") (identical (bytes #x5c #x09 #x2d #x20 #x61 #x0a) "\t- a")))
(want "SP-5e a line of two spaces inside the text: the last space escaped, the stated bytes, the same paragraph"
      (spell (bytes #x61 #x0a #x5c #x20 #x5c #x20 #x0a #x62 #x0a))
      (list '(para "a\n  \nb") (bytes #x61 #x0a #x20 #x5c #x20 #x0a #x62 #x0a) '(para "a\n  \nb")))
(want "SP-5e two spaces before an LF inside the text: the last escaped, not a hard break"
      (spell (bytes #x61 #x5c #x20 #x5c #x20 #x0a #x62 #x0a))
      (list '(para "a  \nb") (bytes #x61 #x20 #x5c #x20 #x0a #x62 #x0a) '(para "a  \nb")))
(want "SP-5e a CR at the src's end: the text holds it, the print spells it &#13;"
      (list (spell (bytes #x61 #x0d)) (spell (bytes #x5c #x0d)))
      (list (list '(para "a\r") "a&#13;\n" '(para "a\r")) (list '(para "\r") "&#13;\n" '(para "\r"))))
(want "SP-5e a CR before the LF inside the text: one paragraph a CR LF b, printed with the entity"
      (spell (bytes #x61 #x0d #x0d #x0a #x62 #x0a))
      (list '(para "a\r\nb") "a&#13;\nb\n" '(para "a\r\nb")))
(want "SP-5e the entity as source: a&#13; LF b decodes to a CR LF b and prints as itself"
      (spell "a&#13;\nb\n")
      (identical "a&#13;\nb\n" "a\r\nb"))
(want "SP-5e the entity read anywhere: a &#13; b is a CR between spaces and prints the CR raw"
      (spell "a &#13; b\n")
      (list '(para "a \r b") (bytes #x61 #x20 #x0d #x20 #x62 #x0a) '(para "a \r b")))
(want "SP-5e CONTROL a genuine hard break, and a CR inside a line, print as today"
      (list (spell "a  \nb\n") (spell (bytes #x61 #x0d #x62 #x0a)))
      (list (list '(para "a" (br) "b") "a  \nb\n" '(para "a" (br) "b"))
            (list '(para "a\rb") (bytes #x61 #x0d #x62 #x0a) '(para "a\rb"))))
(want "SP-5e PIN the unconditional index-0 escapes print as today"
      (list (spell "\\- a\n") (spell "\\> a\n") (spell "\\---\n") (spell "\\# a\n"))
      (list (identical "\\- a\n" "- a") (identical "\\> a\n" "> a") (identical "\\---\n" "---") (identical "\\# a\n" "# a")))
(want "SP-5e CONTROL a marker shape not at a line start is not escaped; --- x is a paragraph either way"
      (list (spell "a 1. b\n") (spell "x | y\n") (spell "--- x\n") (spell "\\--- x\n"))
      (list (identical "a 1. b\n" "a 1. b") (identical "x | y\n" "x | y")
            (list '(para "--- x") "\\--- x\n" '(para "--- x")) (identical "\\--- x\n" "--- x")))

;; AN INDENTED LINE A CODE SPAN OR A LEXEME BEGINS TAKES NO ESCAPE: the
;; parser keeps it in the paragraph, and a backslash there reads back inside
;; the code or the lexeme. The text-led indented line still takes one (the
;; "\\    a" row above). The freeze1 printer, which escaped every such line,
;; is the mutant these three rows are red on.
(want "SP-5e a code span with a four-space continuation line prints back byte for byte"
      (spell "`a\n    b`\n")
      (list '(para (code "a\n    b")) "`a\n    b`\n" '(para (code "a\n    b"))))
(want "SP-5e a reference whose lexeme has a four-space continuation line prints back byte for byte"
      (spell "[[a\n    b]]\n")
      (list '(para (ref "[[a\n    b]]" "a\n    b")) "[[a\n    b]]\n" '(para (ref "[[a\n    b]]" "a\n    b"))))
(want "SP-5e a code span with a tab-led continuation line inside a paragraph prints back byte for byte"
      (spell (bytes #x78 #x20 #x60 #x61 #x0a #x09 #x62 #x60 #x20 #x79 #x0a))
      (list '(para "x " (code "a\n\tb") " y") (bytes #x78 #x20 #x60 #x61 #x0a #x09 #x62 #x60 #x20 #x79 #x0a)
            '(para "x " (code "a\n\tb") " y")))

;; ---- SP-5c the contract over corpora ----------------------------------------------------------

;; THE KNOWN LIST, by path or block id: each entry names its shape and the
;; index of the first differing top-level node, and is admitted by the main
;; session's ruling before the freeze. A listed entry that round-trips is
;; red, and an unlisted failure is red.
(define known-a1 '())
;; THE MEMORY COPY'S KNOWN LIST: 58 srcs of one shape, emphasis-delimiter, the
;; parser's own defect: find-delim takes a delimiter after a backslash, and closes
;; at the first eligible run, so an em or strong holding a literal * or _, or
;; one nested in another, cannot be printed back. Each entry is the src's
;; first differing top-level node and that node's span. Seven were put in
;; this family because the row holds them to their differing node and span,
;; not because their shape was read. Two (23a and 239) open with a heading
;; line, which is a paragraph of its own, so they differ at node 1.
(define known-memory
  '(("xq56ijd1.127" emphasis-delimiter (3 (span 512 554)))
    ("xq56ijd1.196" emphasis-delimiter (5 (span 876 1092)))
    ("xq56ijd1.1a1" emphasis-delimiter (2 (span 495 929)))
    ("xq56ijd1.1hr" emphasis-delimiter (6 (span 1405 1931)))
    ("xq56ijd1.1tx" emphasis-delimiter (2 (span 667 833)))
    ("xq56ijd1.20" emphasis-delimiter (0 (span 0 5949)))
    ("xq56ijd1.21" emphasis-delimiter (2 (span 261 5933)))
    ("xq56ijd1.22" emphasis-delimiter (0 (span 0 5902)))
    ("xq56ijd1.23" emphasis-delimiter (0 (span 0 5896)))
    ("xq56ijd1.1z3" emphasis-delimiter (2 (span 232 804)))
    ("xq56ijd1.204" emphasis-delimiter (5 (span 1056 1581)))
    ("xq56ijd1.q2" emphasis-delimiter (1 (span 114 616)))
    ("xq56ijd1.qn" emphasis-delimiter (2 (span 1092 1455)))
    ("xq56ijd1.wa" emphasis-delimiter (2 (span 479 629)))
    ("xq56ijd1.14i" emphasis-delimiter (5 (span 696 1055)))
    ("xq56ijd1.21k" emphasis-delimiter (1 (span 35 2575)))
    ("xq56ijd1.239" emphasis-delimiter (1 (span 109 438)))
    ("xq56ijd1.23a" emphasis-delimiter (1 (span 53 515)))
    ("xq56ijd1.268" emphasis-delimiter (1 (span 61 932)))
    ("xq56ijd1.26b" emphasis-delimiter (0 (span 0 3553)))
    ("xq56ijd1.26c" emphasis-delimiter (0 (span 0 1142)))
    ("xq56ijd1.26e" emphasis-delimiter (0 (span 0 904)))
    ("xq56ijd1.26f" emphasis-delimiter (1 (span 168 1013)))
    ("xq56ijd1.26h" emphasis-delimiter (0 (span 0 3199)))
    ("xq56ijd1.26i" emphasis-delimiter (1 (span 51 1031)))
    ("xq56ijd1.26l" emphasis-delimiter (3 (span 696 1221)))
    ("xq56ijd1.26q" emphasis-delimiter (1 (span 68 738)))
    ("xq56ijd1.26t" emphasis-delimiter (1 (span 49 803)))
    ("xq56ijd1.26z" emphasis-delimiter (1 (span 49 1364)))
    ("xq56ijd1.278" emphasis-delimiter (1 (span 145 2043)))
    ("xq56ijd1.279" emphasis-delimiter (1 (span 256 2158)))
    ("xq56ijd1.29v" emphasis-delimiter (1 (span 208 5994)))
    ("xq56ijd1.29z" emphasis-delimiter (0 (span 0 3529)))
    ("xq56ijd1.2a0" emphasis-delimiter (0 (span 0 3572)))
    ("xq56ijd1.2a1" emphasis-delimiter (0 (span 0 4445)))
    ("xq56ijd1.2a2" emphasis-delimiter (0 (span 0 3041)))
    ("xq56ijd1.2a8" emphasis-delimiter (1 (span 80 2546)))
    ("xq56ijd1.2ab" emphasis-delimiter (0 (span 0 2809)))
    ("xq56ijd1.2ac" emphasis-delimiter (0 (span 0 2453)))
    ("xq56ijd1.2ae" emphasis-delimiter (1 (span 52 2894)))
    ("xq56ijd1.19x" emphasis-delimiter (1 (span 118 593)))
    ("xq56ijd1.1nt" emphasis-delimiter (1 (span 88 378)))
    ("xq56ijd1.1r4" emphasis-delimiter (2 (span 129 217)))
    ("xq56ijd1.1uc" emphasis-delimiter (3 (span 659 1099)))
    ("xq56ijd1.1wo" emphasis-delimiter (9 (span 985 1689)))
    ("xq56ijd1.1zj" emphasis-delimiter (1 (span 325 1009)))
    ("xq56ijd1.61" emphasis-delimiter (0 (span 0 2544)))
    ("xq56ijd1.63" emphasis-delimiter (0 (span 0 2263)))
    ("xq56ijd1.217" emphasis-delimiter (0 (span 1 551)))
    ("xq56ijd1.27t" emphasis-delimiter (1 (span 554 1388)))
    ("xq56ijd1.287" emphasis-delimiter (2 (span 295 1727)))
    ("xq56ijd1.2ex" emphasis-delimiter (1 (span 102 2698)))
    ("xq56ijd1.qt" emphasis-delimiter (2 (span 168 255)))
    ("xq56ijd1.u" emphasis-delimiter (0 (span 0 533)))
    ("xq56ijd1.vk" emphasis-delimiter (1 (span 27 279)))
    ("xq56ijd1.xp" emphasis-delimiter (2 (span 137 392)))
    ("xq56ijd1.xv" emphasis-delimiter (7 (span 966 2190)))
    ("xq56ijd1.yq" emphasis-delimiter (1 (span 106 403)))))
(define (first-difference t)
  (let ((a (erase (md->blocks (blocks->md t)))) (b (erase (terminate t))))
    (let loop ((a a) (b b) (k 0) (ts t))
      (cond ((and (null? a) (null? b)) #f)
            ((or (null? a) (null? b) (not (equal? (car a) (car b))))
             (list k (and (pair? ts) (span-of (car ts)))))
            (else (loop (cdr a) (cdr b) (+ k 1) (if (pair? ts) (cdr ts) ts)))))))
(define (corpus-report named)
  (let ((fails (filter cdr (map (lambda (p) (cons (car p) (first-difference (md->blocks (cdr p))))) named))))
    (list (length named) fails)))
(define (known-check report known)
  (let ((fails (cadr report)))
    (list (map (lambda (k) (let ((f (assoc (car k) fails))) (and f (equal? (cdr f) (caddr k)) 'known))) known)
          (map car (filter (lambda (f) (not (assoc (car f) known))) fails)))))
(define a1-dir "a1/corpus")
(define a1-files
  (let walk ((dir a1-dir) (out '()))
    (fold-left (lambda (acc name)
                 (let ((p (string-append dir "/" name)))
                   (cond ((file-directory? p) (walk p acc))
                         ((string=? (path-extension p) "md") (cons p acc))
                         (else acc))))
               out (list-sort string<? (directory-list dir)))))
(define a1-named (map (lambda (p) (cons p (call-with-input-file p get-string-all))) (reverse a1-files)))
(define a1-report (corpus-report a1-named))
(printf "SP-5c a1 corpus: ~a srcs, ~a failing: ~s\n" (car a1-report) (length (cadr a1-report)) (cadr a1-report))
(want "SP-5c the a1 corpus keeps the contract but for its known list, each listed entry failing as listed"
      (list (> (car a1-report) 5) (known-check a1-report known-a1))
      (list #t (list (map (lambda (k) 'known) known-a1) '())))

(define corpus-store (getenv "THEOURGIA_MD_CORPUS_STORE"))
(define corpus-cut '(("nxddg1k2" . 415) ("xq56ijd1" . 3654)))
(define corpus-copy (string-append root "/memory-copy"))
(define corpus-state
  (and corpus-store (file-directory? corpus-store)
       (begin (system (string-append "cp -R '" corpus-store "' '" corpus-copy "' && chmod -R u+w '" corpus-copy "'"))
              (open-and-reduce corpus-copy))))
;; An outline row is (parent position id); the srcs are keyed by the id.
(define (markdown-srcs st)
  (filter values
          (map (lambda (row)
                 (let* ((r (state-read st (caddr row)))
                        (f (and (pair? r) (assq 'fields (cdr r))))
                        (fs (if f (cdr f) '()))
                        (k (assq 'kind fs))
                        (s (assq 'src fs)))
                   (and k s (memq (cdr k) '(doc section decision task)) (string? (cdr s)) (cons (caddr row) (cdr s)))))
               (state-outline st))))
(if (not corpus-state)
    (display "SKIP SP-5c memory half and SP-8: THEOURGIA_MD_CORPUS_STORE is not set to a store directory (an opt-in on private data). Make the copy with: mkdir -p ~/.cache/theourgia-f16c-corpus/memory-store && git -C ~/Workshop/memory-store archive 1711cb04a635ce98a64d2a004d5a88eaa9ee8de6 | tar -x -C ~/.cache/theourgia-f16c-corpus/memory-store, then set the variable to that directory\n")
    (let* ((named (markdown-srcs corpus-state))
           (report (corpus-report named)))
      (printf "SP-5c memory copy: cut ~s, ~a srcs, ~a failing: ~s\n" (reduce-applied-cut corpus-state) (car report) (length (cadr report)) (cadr report))
      (want "SP-5c the memory copy is the pinned one: its cut, and more than 3000 markdown srcs"
            (list (reduce-applied-cut corpus-state) (> (car report) 3000))
            (list corpus-cut #t))
      (want "SP-5c the memory copy keeps the contract but for its known list, each listed entry failing as listed"
            (known-check report known-memory)
            (list (map (lambda (k) 'known) known-memory) '()))
      ;; SP-8 (R0i): the refs query over the copy, its budget three times the
      ;; base's median, 316 ms read on 856a9edc, ten runs.
      (let* ((times (map (lambda (i) (let ((t0 (real-time))) (rpc-dispatch corpus-copy '(refs "xq56ijd1.28") "test") (- (real-time) t0))) (iota 5)))
             (median (list-ref (list-sort < times) 2)))
        (printf "SP-8 refs over the memory copy, five runs ~s, median ~a ms, budget 948 ms\n" times median)
        (want "SP-8 the refs query over the memory copy within three times the base's median"
              (<= median 948)
              #t))))

;; ---- SP-6 through the handler ---------------------------------------------------------------

(define (events-of answer)
  (if (and (pair? answer) (eq? (car answer) 'batch))
      (apply append (map events-of (cadr answer)))
      (cadr (assq 'events (cdr answer)))))
(define (made answer) (let ((e (car (events-of answer)))) (block-id (car e) (cdr e))))
(define store (string-append root "/s"))
(define (run . args) (rpc-dispatch store args "test"))
(define (ins! parent fields) (made (run 'batch (format "((insert ~s #f ~s))" parent fields))))
(run 'init)
(define A (ins! 'root '((kind . section) (title . "Alpha") (src . "alpha\n"))))
(define (lex id) (string-append "[[" id "]]"))
(define sp6-src (string-append "See " (lex A) ".\n\n" "| t | " (lex A) " |\n\n" "> - q " (lex A) "\n"))
(define DOC (ins! 'root `((kind . doc) (title . "Doc") (src . ,sp6-src))))
(define SEC (ins! DOC `((kind . section) (title . "Sec") (level . 2) (heading-src . "## Sec\n") (src . "old [[gone]]\n"))))
(define la (string-length (lex A)))
(define sp6-tree
  `((para (span 0 ,(+ 6 la)) "See " (ref ,(lex A) ,A ,A (span 4 ,(+ 4 la))) ".")
    (raw-md ,(string-append "| t | " (lex A) " |\n") (span ,(+ 7 la) ,(+ 7 la 9 la)) (refs (ref ,(lex A) ,A ,A (span ,(+ 7 la 6) ,(+ 7 la 6 la)))))
    (quote (span ,(+ 7 la 9 la 1) ,(+ 7 la 9 la 1 7 la)) (list #f 1 (span ,(+ 7 la 9 la 1) ,(+ 7 la 9 la 1 7 la))
             (item (span ,(+ 7 la 9 la 1) ,(+ 7 la 9 la 1 7 la)) (para (span ,(+ 7 la 9 la 1) ,(+ 7 la 9 la 1 7 la)) "q " (ref ,(lex A) ,A ,A (span ,(+ 7 la 9 la 1 6) ,(+ 7 la 9 la 1 6 la)))))))))
(define (ast-of answer) (let ((b (and (pair? answer) (pair? (cdr answer)) (cadr answer)))) (and (pair? b) (list? b) (let ((a (assq 'ast (cddr b)))) (and a (cdr a))))))
(want "SP-6 read --ast: spans on every block node, the target before the span, the raw-md's refs resolved"
      (ast-of (run 'read DOC "--ast"))
      sp6-tree)
(want "SP-6 every span of the answer's tree names the bytes of the block's src it covers, each ref its lexeme"
      (map (lambda (r) (let ((s (span-of r))) (slice sp6-src (cadr s) (caddr s)))) (refs-in (ast-of (run 'read DOC "--ast"))))
      (list (lex A) (lex A) (lex A)))
(define cut-before (reduce-applied-cut (open-and-reduce store)))
(run 'tag "before")
(run 'set SEC "src" (string-append "new and longer, " (lex A) "\n"))
(want "SP-6 --recursive gives each item its spans; --cut before an edit answers the old src's spans"
      (list (map (lambda (b) (let ((a (assq 'ast (cddr b)))) (and a (cdr a)))) (cdr (cadr (run 'read DOC "--ast" "--recursive"))))
            (ast-of (run 'read SEC "--ast" "--cut" "before")))
      (list (list sp6-tree `((para (span 0 ,(+ 17 la)) "new and longer, " (ref ,(lex A) ,A ,A (span 16 ,(+ 16 la))))))
            '((para (span 0 13) "old " (ref "[[gone]]" "gone" #f (span 4 12))))))
(define st6 (open-and-reduce store))
(want "SP-6 PIN the --ast envelope's heads, and a plain read is the record, its version and the receipt"
      (let ((a (run 'read DOC "--ast")))
        (list (car a) (car (cadr a)) (map (lambda (c) (and (pair? c) (car c))) (cddr a)) (run 'read A)))
      `(ok block (cut versions)
        (ok ,(state-read st6 A) (version ,(block-hash st6 A)) (cut ,(reduce-applied-cut st6)) (versions ((,A . ,(block-hash st6 A)))))))

;; ---- SP-7 the supplier ---------------------------------------------------------------------

(want "SP-7 md-ref-nodes on the lexer's old corpus: lexemes, keys and span starts at the old offsets"
      (map (lambda (s) (map (lambda (r) (list (cadr (span-of r)) (cadr r) (caddr r))) (md-ref-nodes s)))
           (list "see [[foo]] and [[foo.md]]\n" "[[a]] then [[a]] again\n" "[[ent:abc|Display Name]]\n" "`[[no]]` but [[yes]]\n" "| a | [[no]] |\n"))
      '(((4 "[[foo]]" "foo") (16 "[[foo.md]]" "foo")) ((0 "[[a]]" "a") (11 "[[a]]" "a"))
        ((0 "[[ent:abc|Display Name]]" "ent:abc")) ((13 "[[yes]]" "yes")) ((6 "[[no]]" "no"))))
(want "SP-7 md-ref-nodes in document order through em, strong, quote, item and raw-md"
      (map (lambda (r) (caddr r)) (md-ref-nodes "*[[a]]* **[[b]]** [[c]]\n\n> [[d]]\n\n- [[e]]\n\n| [[f]] |\n"))
      '("a" "b" "c" "d" "e" "f"))
(define T (ins! 'root '((kind . section) (title . "Target") (src . "target\n"))))
(define (cite . parts) (apply string-append parts))
(define RP (ins! 'root `((kind . section) (title . "Para") (src . ,(cite "in a para " (lex T) "\n")))))
(define RT (ins! 'root `((kind . section) (title . "Table") (src . ,(cite "| x | " (lex T) " |\n")))))
(define RL (ins! 'root `((kind . section) (title . "List") (src . ,(cite "- an item " (lex T) "\n")))))
(define RM (ins! 'root `((kind . section) (title . "Multi") (src . ,(cite "a `x\ny` then " (lex T) "\n")))))
(define RF (ins! 'root `((kind . section) (title . "Fence") (src . ,(cite "> ```\n> " (lex T) "\n> ```\n")))))
(define (md-referrers-of id)
  (let ((a (run 'refs id)))
    (if (and (pair? a) (eq? (car a) 'ok))
        (list-sort string<? (map (lambda (r) (cadr (assq 'from (cdr r))))
                                 (filter (lambda (r) (and (pair? r) (eq? (car r) 'ref) (equal? (assq 'via (cdr r)) '(via md))))
                                         (cdr (assq 'items (cdr a))))))
        (list 'REFS a))))
(define (query-referrers id)
  (let ((a (run 'query (format "(ref ?f ~s)" id))))
    (if (and (pair? a) (eq? (car a) 'ok))
        (list-sort string<? (map cadr (cdr (assq 'items (cdr a)))))
        (list 'QUERY a))))
(want "SP-7 refs lists a para, a table, a list and the ref after a multi-line code span, and not the quoted fence's"
      (md-referrers-of T)
      (list-sort string<? (list RP RT RL RM)))
(run 'set RP "src" "no reference now\n")
(want "SP-7 after a write the para is gone from refs and the rest stay"
      (md-referrers-of T)
      (list-sort string<? (list RT RL RM)))
(want "SP-7 the query relation agrees with refs on the same state"
      (query-referrers T)
      (list-sort string<? (list RT RL RM)))
(define SELF-answer (run 'batch (format "((insert root #f ~s))" '((kind . section) (title . "Self") (src . "-\n")))))
(define SELF (made SELF-answer))
(run 'set SELF "src" (cite "I cite " (lex SELF) "\n"))
(want "SP-7 CONTROL a block citing itself: query keeps the self edge, refs leaves it out, as on the base"
      (list (query-referrers SELF) (md-referrers-of SELF))
      (list (list SELF) '()))
(want "SP-7 TRIPWIRE no md-refs or line-refs definition in the product files; one md->blocks, one parse-inline"
      (let* ((files (let walk ((dir "..") (out '()))
                      (fold-left (lambda (acc name)
                                   (let ((p (string-append dir "/" name)))
                                     (cond ((member p '("../test" "../.git")) acc)
                                           ((file-directory? p) (walk p acc))
                                           ((or (string=? (path-extension p) "sc") (string=? (path-extension p) "ss")) (cons p acc))
                                           (else acc))))
                                 out (list-sort string<? (directory-list dir)))))
             (count (lambda (needle)
                      (apply + (map (lambda (f)
                                      (let ((t (call-with-input-file f get-string-all)) (n (string-length needle)))
                                        (let loop ((i 0) (k 0))
                                          (cond ((> (+ i n) (string-length t)) k)
                                                ((string=? (substring t i (+ i n)) needle) (loop (+ i n) (+ k 1)))
                                                (else (loop (+ i 1) k))))))
                                    files)))))
        (list (> (length files) 50) (count "(define (md-refs") (count "(define (line-refs")
              (count "(define (md->blocks") (count "(define (parse-inline")))
      '(#t 0 0 1 1))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nast-spans complete\n" bad rows)
(exit (if (= bad 0) 0 1))
