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

;; H1: the three markdown pieces, each judged by its own evidence.
;;
;; WHAT EACH SECTION IS THE EVIDENCE FOR, said here because it is easy
;; to get backwards: the byte rows judge md-split/md-join and say
;; NOTHING about the parser -- export replays stored bytes, so a parser
;; that understood nothing would pass every one of them. The parser is
;; judged by the AST round trips, where there are no bytes to replay,
;; and by the reference rows.
(import (chezscheme) (theourgia md))

(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define (round-trips? text)
  (let ((d (md-split text)))
    (string=? (md-join (doc-front d) (doc-src d) (doc-sections d)) text)))

(define (shape text)
  (let ((d (md-split text)))
    (list (doc-front d) (doc-src d)
          (map (lambda (s) (list (section-level s) (section-title s)
                                 (section-heading-src s) (section-src s)))
               (doc-sections d)))))

(printf "== section 2.4: the file is cut into ranges that put back together ==\n")
;; EVERY ONE OF THESE IS A FILE THE CORPUS ACTUALLY CONTAINS SOMEWHERE.
;; The empty file and the file with no final newline are the two that
;; break naive splitters; CRLF and the BOM are the two that break naive
;; normalisers.
(define corpus
  (list
    (cons "empty" "")
    (cons "no heading at all" "just prose\n")
    (cons "no final newline" "# A\nbody")
    (cons "crlf" "# A\r\nbody\r\n")
    (cons "bom" "\xFEFF;# A\nbody\n")
    (cons "three blank lines between headings" "## A\n\n\n\n## B\n")
    (cons "a level skipped" "# A\n### B\n")
    (cons "closing hashes" "## A ##\ntext\n")
    (cons "the same title twice" "# A\nx\n# A\ny\n")
    (cons "front matter" "---\ntitle: t\n---\nlead\n# A\nbody\n")
    (cons "a hash inside a fence" "# A\n```\n# not a heading\n```\ntail\n")
    (cons "a hash with no space" "#no-space\n")
    (cons "six levels" "# 1\n## 2\n### 3\n#### 4\n##### 5\n###### 6\n")
    (cons "seven hashes is not a heading" "####### x\n")))
(want "every fixture comes back byte for byte"
      (map car (filter (lambda (p) (not (round-trips? (cdr p)))) corpus))
      '())
;; THE RANGES THEMSELVES, not just that they concatenate. A splitter that
;; put everything in doc.src and found no sections would pass the row
;; above and fail this one.
(want "the blank lines before a heading belong to the section above it"
      (shape "## A\n\n\n\n## B\n")
      (list "" "" (list (list 2 "A" "## A\n" "\n\n\n") (list 2 "B" "## B\n" ""))))
(want "front matter is its own range and stops at the closing marker"
      (shape "---\ntitle: t\n---\nlead\n# A\nbody\n")
      (list "---\ntitle: t\n---\n" "lead\n" (list (list 1 "A" "# A\n" "body\n"))))
(want "a heading inside a fence is not a heading"
      (length (caddr (shape "# A\n```\n# not a heading\n```\ntail\n")))
      1)
(want "closing hashes are not part of the title"
      (map cadr (caddr (shape "## A ##\ntext\n")))
      '("A"))
(want "a hash with no space after it starts nothing"
      (caddr (shape "#no-space\n"))
      '())
(want "seven hashes is not a heading either"
      (caddr (shape "####### x\n"))
      '())

(printf "== A3'(c5): reordering adds one newline, and only then ==\n")
;; THE ONLY PIECE THAT MAY LACK A FINAL NEWLINE IS THE LAST ONE, so an
;; unmodified export never adds anything. Move that piece into the
;; middle and exactly one newline appears, belonging to the piece that
;; follows it.
;; THE BODIES ARE CJK ON PURPOSE. Written as the UTF-8 bytes spelled
;; out one escape each, this row compared three Latin-1 characters
;; against themselves and said nothing about multi-byte text.
(define c5 "# A\n\x7532;\n# B\n\x4E59;")
(want "unreordered, the file is unchanged"
      (round-trips? c5)
      #t)
(want "with B first, one newline is added after B's body and none elsewhere"
      (let* ((d (md-split c5))
             (ss (doc-sections d)))
        (md-join (doc-front d) (doc-src d) (list (cadr ss) (car ss))))
      "# B\n\x4E59;\n# A\n\x7532;\n")

(printf "== A7: an AST with no bytes to fall back on ==\n")
;; THESE ARE THE CASES WHERE A GENERATOR THAT DOES NOT ESCAPE PRODUCES
;; MARKDOWN THAT READS BACK AS SOMETHING ELSE. There is no src here, so
;; nothing covers for the generator.
(define asts
  (list
    (cons "a paragraph that starts like a heading" '((para "# not a heading")))
    (cons "the characters that change a parse" '((para "a*b_c`d<e&f")))
    ;; PAIRED DELIMITERS, WHICH IS WHERE ESCAPING ACTUALLY MATTERS. A
    ;; single unescaped "*" with no closer stays literal on its own, so
    ;; a generator that escaped nothing still round trips text holding
    ;; one of each -- these are the strings that come back as emphasis,
    ;; as code, and as a link if the escapes are dropped.
    (cons "literal text holding a matched pair of stars" '((para "a*b*c")))
    (cons "literal text holding a matched pair of underscores" '((para "a_b_c")))
    (cons "literal text holding a matched pair of backticks" '((para "a`b`c")))
    (cons "literal text shaped like a link" '((para "[x](y)")))
    (cons "literal text shaped like a reference" '((para "[[x]]")))
    (cons "a backslash of its own" '((para "a\\b")))
    (cons "a tilde fence" '((code "~~~" "scheme" "(+ 1 2)")))
    (cons "a backtick fence" '((code "```" "" "plain")))
    (cons "emphasis and strong" '((para "plain " (em "slanted") " and " (strong "bold"))))
    (cons "an inline span holding what looks like a reference"
          '((para (code "[[not a ref]]"))))
    (cons "an inline span holding backticks" '((para (code "a ` b"))))
    (cons "a reference" '((para "see " (ref "[[foo.md]]" "foo"))))
    (cons "a link" '((para (link "http://x/y" "text"))))
    (cons "a bullet list" '((list #f 1 (item (para "one")) (item (para "two")))))
    (cons "an ordered list that does not start at one"
          '((list #t 3 (item (para "three")) (item (para "four")))))
    (cons "a quote" '((quote (para "quoted"))))
    (cons "a rule" '((hr)))))
;; A PARSED TREE CARRIES SPANS, which a hand-built one has no bytes for: the
;; parsed side is compared with every (span ...) removed.
(define (erase x)
  (cond ((pair? x) (map erase (filter (lambda (y) (not (and (pair? y) (eq? (car y) 'span)))) x)))
        (else x)))
(want "every AST survives being printed and read back"
      (map car (filter (lambda (p) (not (equal? (cdr p) (erase (md->blocks (blocks->md (cdr p)))))))
                       asts))
      '())
;; RAW-MD IS VERBATIM, so its two cases are written out rather than erased:
;; printed, a raw-md text with no terminator gains the printer's LF, and read
;; back it is the slice of the printed bytes, that LF included, with its
;; span and its references. The text's length and bytes are asserted apart,
;; so a span ending at the content's end, before the LF, is red.
(want "raw markdown holding a reference: printed, read back as the text with its LF, its span and its reference"
      (md->blocks (blocks->md '((raw-md "| a | b |\n| [[x]] | 2 |"))))
      '((raw-md "| a | b |\n| [[x]] | 2 |\n" (span 0 24) (refs (ref "[[x]]" "x" (span 12 17))))))
(want "raw markdown holding a reference: the text read back is 24 bytes, the last an LF"
      (let ((t (cadr (car (md->blocks (blocks->md '((raw-md "| a | b |\n| [[x]] | 2 |"))))))))
        (list (bytevector-length (string->utf8 t)) (string-ref t (- (string-length t) 1))))
      '(24 #\newline))
(want "an indented code block kept whole: read back as its text with its LF, its span, and no reference"
      (md->blocks (blocks->md '((raw-md "    [[not a ref]]"))))
      '((raw-md "    [[not a ref]]\n" (span 0 18) (refs))))
;; AND THE MARKDOWN IS WHAT A READER WOULD WRITE. Equality above is
;; satisfied by any printer whose reader undoes it -- including one that
;; escaped every character. This pins the text for the case that matters.
(want "the escaped heading is a backslash and nothing more"
      (blocks->md '((para "# not a heading")))
      "\\# not a heading\n")
(want "a paragraph with no special characters is printed as itself"
      (blocks->md '((para "plain words here")))
      "plain words here\n")

(printf "== A2: which bracket pairs are references ==\n")
;; THE PARSER'S REFERENCE NODES, each (ref <lexeme> <key> (span s e)), read
;; here as (<start> <lexeme> <key>): on this ASCII text a span's start is the
;; character offset the lexer answered.
(define (refs-at text)
  (map (lambda (r) (list (cadr (cadddr r)) (cadr r) (caddr r))) (md-ref-nodes text)))
(want "the .md form and the bare form share a key, and keep their lexemes"
      (refs-at "see [[foo]] and [[foo.md]]\n")
      '((4 "[[foo]]" "foo") (16 "[[foo.md]]" "foo")))
;; A MULTISET, NOT A SET. Two occurrences of one target are two entries;
;; an implementation that deduplicated would report one and the corpus
;; count would be quietly low.
(want "the same target twice is two entries"
      (refs-at "[[a]] then [[a]] again\n")
      '((0 "[[a]]" "a") (11 "[[a]]" "a")))
(want "code of every kind quotes its contents"
      (list (refs-at "```\n[[no]]\n```\n")
            (refs-at "~~~\n[[no]]\n~~~\n")
            (refs-at "    [[no]]\n")
            (refs-at "text `[[no]]` more\n")
            (refs-at "| a | `[[no]]` |\n"))
      '(() () () () ()))
(want "CONTROL: the same lines outside code are found"
      (list (length (refs-at "[[no]]\n"))
            (length (refs-at "text [[no]] more\n"))
            (length (refs-at "| a | [[no]] |\n")))
      '(1 1 1))
(want "a display form keeps its lexeme and resolves to the target"
      (refs-at "[[ent:abc|Display Name]]\n")
      '((0 "[[ent:abc|Display Name]]" "ent:abc")))
(want "one quoted and one real on the same line"
      (refs-at "`[[no]]` but [[yes]]\n")
      '((13 "[[yes]]" "yes")))

(printf "== the parser is not allowed to give up on everything ==\n")
;; raw-md IS A LEGAL ANSWER FOR WHAT v1 DOES NOT PARSE, AND THAT IS WHY
;; THIS ROW EXISTS. A parser that answered raw-md for every block would
;; pass every byte row in this file, because export replays bytes.
;; Measured on the 463-file corpus: 7726 block nodes, of which raw-md is
;; 1.8% (para 76.7, list 12.9, quote 5.6, code 2.5, hr 0.5). The bound
;; below is set from that measurement with room to spare, not from
;; taste; a parser that degenerates goes to ~100% and dies here.
(define mixed
  (string-append
    "A paragraph.\n\n"
    "- one\n- two\n\n"
    "```scheme\n(+ 1 2)\n```\n\n"
    "> quoted\n\n"
    "---\n\n"
    "| a | b |\n| 1 | 2 |\n\n"
    "Another paragraph.\n"))
(define mixed-kinds (map car (md->blocks mixed)))
(want "a document using the whole vocabulary yields the whole vocabulary"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 (let dedupe ((xs mixed-kinds) (out '()))
                   (cond ((null? xs) out)
                         ((memq (car xs) out) (dedupe (cdr xs) out))
                         (else (dedupe (cdr xs) (cons (car xs) out))))))
      '(code hr list para quote raw-md))
(want "and raw-md is a minority of it"
      (let ((n (length mixed-kinds))
            (raw (length (filter (lambda (k) (eq? k 'raw-md)) mixed-kinds))))
        (list n raw (<= (* 100 raw) (* 20 n))))
      (list 7 1 #t))

;; A BACKTICK RUN WITH A BACKTICK AFTER IT ON ITS LINE OPENS NO FENCE.
;; CommonMark: the info string of a backtick fence holds no backtick, so
;; "```x()```" is a code span. The splitter and the block parser ask the
;; same fence-at, so one such line taken for an opener hid every heading
;; after it from the splitter and every block after it from the parser.
;; Inside an open fence the same line, taken for a closer, ended the block
;; early; it is now a line of the block's text.
(printf "== a one-line backtick run is a span, and a heading-shaped line ends a paragraph ==\n")
(define (no-span b) (cons (car b) (cddr b)))
(want "MD-1a the splitter: a one-line backtick run leaves the heading after it a section"
      (map cadr (caddr (shape "# A\n```refusal()```\n\n# B\nbody\n")))
      '("A" "B"))
(want "MD-1b the refs: a reference after a one-line backtick run is found"
      (map caddr (md-ref-nodes "```x()```\n\nsee [[other]]\n"))
      '("other"))
(want "MD-1c the parser: a paragraph holding a one-line backtick run is one paragraph with a code span"
      (map no-span (md->blocks "before\n```refusal()```\nafter\n"))
      '((para "before\n" (code "refusal()") "\nafter")))
(want "MD-1d CONTROL: a backtick fence with an info string still opens, and a tilde fence's info may hold a backtick"
      (list (map car (md->blocks "```scheme\n(+ 1 2)\n```\n"))
            (caddr (car (md->blocks "```scheme\n(+ 1 2)\n```\n")))
            (map car (md->blocks "~~~ a`b\nx\n~~~\n")))
      '((code) "scheme" (code)))
(want "MD-1e the parser: inside an open backtick fence a one-line backtick run is a line of the block, which ends at its real closer"
      (let ((bs (md->blocks "```\nbefore\n```x()```\nafter\n```\ntail\n")))
        (list (map car bs) (cadddr (car bs))))
      '((code para) "before\n```x()```\nafter"))
(want "MD-1f the splitter: inside an open backtick fence a one-line backtick run closes nothing, so the heading-shaped line after it is not a section and the heading after the real closer is"
      (map cadr (caddr (shape "# A\n```\n```x()```\n# not a heading\n```\n# B\nbody\n")))
      '("A" "B"))
;; A HEADING-SHAPED LINE IN A BLOCK'S TEXT ENDS THE PARAGRAPH BEFORE IT.
;; md->blocks has no heading node (headings are sections, split out
;; first), so the line stays a paragraph of its own; what changes is that a
;; code span can no longer cross it.
(want "MD-2a a heading-shaped line is a paragraph of its own between two others"
      (map no-span (md->blocks "a\n# h\nb\n"))
      '((para "a") (para "# h") (para "b")))
(want "MD-2b a code span does not cross a heading-shaped line"
      (map no-span (md->blocks "Call `unfinished\n# Heading `refusal`\n"))
      '((para "Call `unfinished") (para "# Heading " (code "refusal"))))
(want "MD-2c CONTROL: a hash with no space after it continues the paragraph"
      (map no-span (md->blocks "a\n#b\n"))
      '((para "a\n#b")))
;; THE PRINTER ESCAPES EVERY LINE THE PARSER WOULD SPLIT AT, and a list
;; item keeps a heading-shaped line inside its paragraph: an item's
;; paragraphs cannot be printed apart (a blank line ends the list). Each row
;; is the parse with spans erased, and whether printing it and parsing the
;; print gives the same tree.
(define (erase-spans x)
  (if (pair? x)
      (map erase-spans (filter (lambda (y) (not (and (pair? y) (eq? (car y) 'span)))) x))
      x))
(define (round-trip src)
  (let ((t (erase-spans (md->blocks src))))
    (list t (equal? t (erase-spans (md->blocks (blocks->md (md->blocks src))))))))
(want "MD-2d an escaped heading-shaped line inside a paragraph is printed escaped, so the paragraph round trips"
      (round-trip "a\n\\# h\nb\n")
      '(((para "a\n# h\nb")) #t))
(want "MD-2e the same inside a list item"
      (round-trip "- a\n  \\# h\n  b\n")
      '(((list #f 1 (item (para "a\n# h\nb")))) #t))
(want "MD-2f the same inside a quote"
      (round-trip "> a\n> \\# h\n> b\n")
      '(((quote (para "a\n# h\nb"))) #t))
(want "MD-2g in a list item a heading-shaped line continues the paragraph: one item with one paragraph, as c8c08c9 parsed it, and it round trips"
      (round-trip "- a\n  # h\n  b\n")
      '(((list #f 1 (item (para "a\n# h\nb")))) #t))
(want "MD-2h in a quote a heading-shaped line is a paragraph of its own between two others, and the three round trip"
      (round-trip "> a\n> # h\n> b\n")
      '(((quote (para "a") (para "# h") (para "b"))) #t))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "md1 complete\n")
(exit (if (zero? bad) 0 1))
