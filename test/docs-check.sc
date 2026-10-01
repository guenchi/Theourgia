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

;; The README, checked against the tree it describes.
;;
;; KEY: THREE CLAIMS, EACH WITH ITS TRUTH TAKEN FROM THE PROGRAM: the verbs
;; it documents, the options it advertises, and the environment variables
;; it lists. A README is the part of a repository that goes stale first,
;; because nothing runs it -- and the way it goes stale is not by saying
;; something false but by falling silent about something new.
;;
;; NEVER: NONE OF THE THREE IS A LIST IN THIS FILE. A gate that carried its
;; own copy of the verbs would go green on the day both copies were
;; wrong together.

(import (rnrs)
        (only (chezscheme) with-input-from-file call-with-input-file
              get-string-all directory-list file-exists? load)
        (only (theourgia arguments) parse-arguments)
        (only (theourgia rpc) rpc-verbs write-protocol verb-catalogue register-verbs!)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia platform-numbers) platform-readings reading-key))

;; THE VERBS REGISTERED FROM OUTSIDE THE CORE TABLE, as core.sc and the daemon
;; register them: this census reads the registry itself, not a copy of it.
(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (display (string-append "ok " label "\n"))
      (begin (set! bad (+ bad 1))
             (display (string-append "FAIL " label ": "))
             (write got) (display " WANT ") (write expected) (newline))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; ---- reading text -------------------------------------------------------------

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (lines-of text)
  (let loop ((i 0) (this '()) (out '()))
    (cond ((>= i (string-length text))
           (reverse (if (null? this) out (cons (list->string (reverse this)) out))))
          ((char=? (string-ref text i) #\newline)
           (loop (+ i 1) '() (cons (list->string (reverse this)) out)))
          (else (loop (+ i 1) (cons (string-ref text i) this) out)))))

(define (starts-with? text prefix)
  (let ((n (string-length prefix)))
    (and (>= (string-length text) n) (string=? (substring text 0 n) prefix))))

(define (index-of text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))

(define (contains? text needle) (and (index-of text needle 0) #t))

(define (add-unique x xs) (if (member x xs) xs (cons x xs)))

(define (sorted-strings xs) (list-sort string<? xs))

;; ---- the README's verb headings -----------------------------------------------
;;
;; NEVER: EVERY `###` IS A VERB SECTION, with no exemption list. Two headings
;; that were not verbs -- `published.sexp` and `incoming/` -- are `####`
;; for exactly this reason. NOTE: The alternative, "count the `###` whose
;; first word happens to be a verb", assumes the answer: a verb with no
;; section is then simply not counted, which is the one thing this row
;; exists to notice.

(define readme (file-text "../README.md"))

(define (heading-verb line)
  (and (starts-with? line "### `")
       (let loop ((i 5) (acc '()))
         (cond ((>= i (string-length line)) #f)
               ((let ((c (string-ref line i)))
                  (or (char=? c #\space) (char=? c #\`)))
                (and (pair? acc) (list->string (reverse acc))))
               (else (loop (+ i 1) (cons (string-ref line i) acc)))))))

(define readme-heading-verbs
  (sorted-strings
    (fold-left (lambda (acc l) (let ((v (heading-verb l))) (if v (add-unique v acc) acc)))
               '() (lines-of readme))))

;; NEVER: THE VERBS EACH PROGRAM ANSWERS ITSELF ARE READ FROM ITS DISPATCH
;; TABLE (F64), by the reader `options-gate.sc` uses. This was the literal
;; `'(eval serve)`, here and again in DOC-2, so a third verb dispatched by
;; either program was one the README never had to document.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))

(load (string-append script-dir "/own-verbs.sc"))

(define programs-own-verbs
  (append (own-verbs-of "../core.sc") (own-verbs-of "../theourgiad.sc")))

(define program-verbs
  (sorted-strings (map symbol->string (append (rpc-verbs) programs-own-verbs))))

(want "DOC-1 the README documents exactly the verbs the program answers to"
      readme-heading-verbs program-verbs)

;; ---- the options it advertises ------------------------------------------------
;;
;; NEVER: ONLY THE INDENTED BLOCKS COUNT AS AN ADVERTISEMENT. Prose has to be
;; able to NAME an option in order to say something about it -- that it was
;; broken, that it is the same spelling another verb uses -- and a scanner
;; reading the whole section cannot tell the two apart. Measured while
;; writing that README: a paragraph saying "`--writer` does not work on this
;; verb" was read as the verb advertising `--writer`.
;;
;; NEVER: AND NOT THE HEADING (F52). The heading used to carry the usage form
;; too, so every option was written twice in the section, and this set was
;; the union of the two: an option dropped from the block stayed advertised
;; by the heading and turned nothing red. A heading is the verb's name only
;; now -- a row below holds it to that -- and the blocks are the one place.

(define (dashed-tokens line)
  ;; NOTE: `--` MUST BE FOLLOWED BY A LETTER. Without that test the `---`
  ;; of a Markdown table row is an option, and the row reports a defect
  ;; in a table separator.
  (let loop ((i 0) (out '()))
    (cond
      ((>= i (- (string-length line) 2)) out)
      ((and (char=? (string-ref line i) #\-)
            (char=? (string-ref line (+ i 1)) #\-)
            (char-alphabetic? (string-ref line (+ i 2))))
       (let scan ((j (+ i 2)) (acc (list #\- #\-)))
         (cond ((or (>= j (string-length line))
                    (not (or (char-alphabetic? (string-ref line j))
                             (char=? (string-ref line j) #\-))))
                (loop j (add-unique (list->string (reverse acc)) out)))
               (else (scan (+ j 1) (cons (string-ref line j) acc))))))
      (else (loop (+ i 1) out)))))

(define advertised
  (let loop ((ls (lines-of readme)) (verb #f) (out '()))
    (cond
      ((null? ls) out)
      (else
       (let ((l (car ls)))
         (cond
           ((heading-verb l)
            => (lambda (v) (loop (cdr ls) v out)))
           ((and (starts-with? l "#") (not (starts-with? l "####")))
            (loop (cdr ls) #f out))
           ((and verb (starts-with? l "    "))
            (loop (cdr ls) verb
                  (append (map (lambda (o) (list (string->symbol verb) o)) (dashed-tokens l))
                          out)))
           (else (loop (cdr ls) verb out))))))))

(define (accepted? verb option)
  (let ((nodes (parse-arguments verb (list option "x"))))
    (and (pair? nodes) (pair? (car nodes)) (memq (caar nodes) '(option flag)) #t)))

(want "DOC-2 every option the README advertises is accepted by that verb"
      (filter (lambda (p) (not (accepted? (car p) (cadr p)))) advertised)
      '())

;; NEVER: AND THE OTHER WAY. Without this row the README could advertise
;; nothing at all and stay green -- which is how a document falls behind:
;; not by saying something false, but by never mentioning what arrived.
(define common-options '("--store" "--wire" "--actor" "--req" "--cursor" "--socket"))

(define every-spelling
  (let ((text (file-text "../arguments.sc")))
    (fold-left (lambda (acc l) (append (dashed-tokens l) acc)) '() (lines-of text))))

(define unadvertised
  (fold-left
    (lambda (acc verb)
      (append acc
              (map (lambda (o) (list verb o))
                   (filter (lambda (o)
                             (and (accepted? verb o)
                                  (not (member o common-options))
                                  (not (member (list verb o) advertised))))
                           every-spelling))))
    '() (append (rpc-verbs) programs-own-verbs)))

(want "DOC-2 every verb-specific option the parser accepts is in the README"
      unadvertised '())

;; ---- F52: a heading is a name, and the usage form is the product's --------------
;;
;; KEY: THE USAGE FORM IS WRITTEN ONCE, IN THE FIRST BLOCK OF THE VERB'S
;; SECTION, and what it names is compared with the form the product itself
;; gives for the verb: the catalogue's entry (what `describe` publishes),
;; or for a verb a program answers itself, that program's `<verb>-usage`.
;; A name the README writes and the product's form lacks is red -- an
;; option the verb never had, a placeholder the verb does not take.
;;
;; NOTE: WHAT THIS DOES NOT COMPARE. Names only, not the form's structure:
;; which clauses are optional, and their order, are not read. A name the
;; product form has and the README block leaves out is not red here; DOC-2
;; above is what asks for every option the parser accepts. And the answer
;; shapes a section shows in its later blocks are compared with NOTHING --
;; there is no product table of answer shapes to hold them to.

(define verb-headings
  (filter heading-verb (lines-of readme)))

;; NEVER: ANY `--`, NOT ONLY AN OPTION-SHAPED ONE. `dashed-tokens` wants a
;; letter after the two dashes, so a heading ending in a bare `--` passed.
;; The rule is that a heading is the verb's name, and a name has no `--`.
(want "F52 no verb heading contains --: the usage form is written once, in the section's block"
      (filter (lambda (l) (contains? l "--")) verb-headings)
      '())

;; The first indented block after a verb's heading, as text, or #f when the
;; section has none before the next heading.
;;
;; NOTE: A BLANK LINE DOES NOT END AN INDENTED BLOCK when the next non-blank
;; line is still indented; that is Markdown's rule, and the block a reader
;; sees. Ending at the first blank line let a datum written after one escape
;; the one-datum check. Blank lines at the block's end are not part of it.
;; A blank line is spaces or tabs, and is kept as it is written, so an
;; offset after it is the offset in the source (r2 review).
(define (blank-line? l)
  (for-all (lambda (c) (or (char=? c #\space) (char=? c #\tab))) (string->list l)))
(define (first-block-after ls)
  (let skip ((ls ls))
    (cond ((null? ls) #f)
          ((and (starts-with? (car ls) "#") (not (starts-with? (car ls) "####"))) #f)
          ((starts-with? (car ls) "    ")
           (let take ((ls ls) (acc '()))
             (cond ((and (pair? ls) (starts-with? (car ls) "    "))
                    (take (cdr ls) (cons (car ls) acc)))
                   ((and (pair? ls) (blank-line? (car ls))
                         (let next ((rest (cdr ls)))
                           (cond ((null? rest) #f)
                                 ((blank-line? (car rest)) (next (cdr rest)))
                                 (else (starts-with? (car rest) "    ")))))
                    (take (cdr ls) (cons (car ls) acc)))
                   (else
                    (fold-left (lambda (s l) (string-append s l "\n")) "" (reverse acc))))))
          (else (skip (cdr ls))))))

;; Every name in a form: strings, symbols, anything that is not a pair.
(define (names-in x)
  (cond ((pair? x) (append (names-in (car x)) (names-in (cdr x))))
        ((null? x) '())
        ((vector? x) (names-in (vector->list x)))
        (else (list x))))

;; A USAGE FORM IS SPELLED IN THREE KINDS OF NAME after the verb's own: an
;; option in a string, `"--under"`; a placeholder, `<id>`; and `...`. An
;; answer shape can be headed by the verb's name too -- `tag` answers
;; `(tag (name "<name>") ...)` -- and its bare words are what tell it apart.
(define (usage-name? x)
  (or (and (string? x) (starts-with? x "--"))
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (or (string=? s "...") (starts-with? s "<"))))))

;; The character offset in `text` at which a second datum begins, after the
;; first one is read, or #f when only whitespace and comments follow it. A
;; comment is not a datum (r1 review): a `;` line, a `#| |#` block (nested),
;; and a datum discarded with `#;` are skipped, and the offset names the datum
;; after them. A text that does not read at all has no first datum and
;; answers #f here; the row about usage forms reports that section.
(define (second-datum-offset text)
  (guard (e (#t #f))
    (let ((port (open-string-input-port text)) (n (string-length text)))
      (define (at i) (and (< i n) (string-ref text i)))
      (read port)
      (let skip ()
        (let ((i (port-position port)) (c (peek-char port)))
          (cond ((eof-object? c) #f)
                ((char-whitespace? c) (read-char port) (skip))
                ((char=? c #\;)
                 (let line () (let ((d (read-char port))) (unless (or (eof-object? d) (char=? d #\newline)) (line))))
                 (skip))
                ((and (char=? c #\#) (eqv? (at (+ i 1)) #\|))
                 (let block ((j (+ i 2)) (depth 1))
                   (cond ((>= j n) (set-port-position! port n))
                         ((and (eqv? (at j) #\|) (eqv? (at (+ j 1)) #\#))
                          (if (= depth 1) (set-port-position! port (+ j 2)) (block (+ j 2) (- depth 1))))
                         ((and (eqv? (at j) #\#) (eqv? (at (+ j 1)) #\|)) (block (+ j 2) (+ depth 1)))
                         (else (block (+ j 1) depth))))
                 (skip))
                ((and (char=? c #\#) (eqv? (at (+ i 1)) #\;))
                 (set-port-position! port (+ i 2))
                 (read port)
                 (skip))
                (else i)))))))

(want "F104 CONTROL: comments after the usage form are not a second datum, and a datum after them is named at its own offset"
      (list (second-datum-offset "(move <id>) ; a note\n")
            (second-datum-offset "(move <id>) #| a #| nested |# note |#")
            (second-datum-offset "(move <id>) #;(discarded form)")
            (second-datum-offset "(move <id>) ; a note\n <bogus>"))
      (list #f #f #f 22))

;; `(verb datum extra)` for every verb section of `text`: `datum` is the
;; section's first block read as a form headed by the verb's name and spelled
;; in usage names only, or #f; `extra` is the offset within that block of a
;; second datum, or #f.
(define (usage-blocks-of text)
  (let loop ((ls (lines-of text)) (out '()))
    (cond
      ((null? ls) (reverse out))
      ((heading-verb (car ls))
       => (lambda (v)
            (let* ((verb (string->symbol v))
                   (block (first-block-after (cdr ls)))
                   (datum (and block
                               (guard (e (#t #f))
                                 (read (open-string-input-port block))))))
              (loop (cdr ls)
                    (cons (list verb
                                (and (pair? datum) (eq? (car datum) verb)
                                     (for-all usage-name? (names-in (cdr datum)))
                                     datum)
                                (and block (second-datum-offset block)))
                          out)))))
      (else (loop (cdr ls) out)))))

;; `(verb . datum)` for every verb section whose first block reads as a form
;; headed by the verb's name and spelled in usage names only, and `(verb .
;; #f)` for every other section.
(define section-usage-blocks
  (map (lambda (s) (cons (car s) (cadr s))) (usage-blocks-of readme)))

;; The product's own usage form for a verb, or #f.
(define (program-usage-form verb)
  (let search ((programs '("../core.sc" "../theourgiad.sc")))
    (cond
      ((null? programs) #f)
      ((memq verb (own-verbs-of (car programs)))
       (let ((name (string->symbol (string-append (symbol->string verb) "-usage"))))
         (let find ((fs (own-verbs-forms (car programs))))
           (cond ((null? fs) #f)
                 ((and (pair? (car fs)) (eq? (car (car fs)) 'define)
                       (pair? (cdr (car fs))) (eq? (cadr (car fs)) name)
                       (pair? (cddr (car fs))) (pair? (caddr (car fs)))
                       (eq? (car (caddr (car fs))) 'quote))
                  (cadr (caddr (car fs))))
                 (else (find (cdr fs)))))))
      (else (search (cdr programs))))))

(define (product-usage-form verb)
  (let ((entry (assq verb (verb-catalogue))))
    (if entry (cadr entry) (program-usage-form verb))))

(define (names-missing-from product block)
  (let ((theirs (names-in product)))
    (fold-left (lambda (acc n) (if (member n theirs) acc (append acc (list n))))
               '() (names-in block))))

(define compared
  (filter (lambda (s) (and (cdr s) (product-usage-form (car s)))) section-usage-blocks))

(want "F52 every name a section's usage-form block writes is in the product's own form for that verb"
      (fold-left (lambda (acc s)
                   (let ((extra (names-missing-from (product-usage-form (car s)) (cdr s))))
                     (if (null? extra) acc (append acc (list (cons (car s) extra))))))
                 '() compared)
      '())

(want "F52 every section with a usage-form block has a product form to compare it with"
      (map car (filter (lambda (s) (and (cdr s) (not (product-usage-form (car s)))))
                       section-usage-blocks))
      '())

;; NEVER: EVERY VERB SECTION OPENS WITH ITS USAGE FORM, and this row holds the
;; list of those that do not at empty. Nine sections -- refs, search, grep,
;; whereis, log, tag, diff, conflicts, batch -- opened with an answer shape or
;; an example and were compared with nothing; each was given its form from
;; the catalogue, before the answer shape, under a name-only heading. A
;; section that opens any other way is outside the comparison above, so it
;; is red here rather than silently skipped.
(want "F52 every verb section's first block is its usage form, so the comparison above reads every section"
      (map car (filter (lambda (s) (not (cdr s))) section-usage-blocks))
      '())

(want "F52 CONTROL: the comparison read at least twenty sections, and it reports a name the product form lacks"
      (list (>= (length compared) 20)
            (names-missing-from (product-usage-form 'insert) '(insert "--under" <id> ("--bogus"))))
      '(#t ("--bogus")))

;; NEVER: THE USAGE BLOCK HOLDS EXACTLY ONE DATUM (F104, I2). The comparison
;; above reads the block's first datum and nothing after it, so a second form
;; written into the same block -- an extra placeholder, an option the verb
;; does not take -- was compared with nothing and turned no row red. A second
;; datum is red here, with the verb and its offset in the block.
(define (sections-with-a-second-datum text)
  (fold-left (lambda (acc s) (if (caddr s) (append acc (list (list (car s) (caddr s)))) acc))
             '() (usage-blocks-of text)))

(want "F104 every verb section's usage block holds exactly one datum"
      (sections-with-a-second-datum readme)
      '())

;; A README copy with `<bogus>` written after move's usage form, in the same
;; block: the check names move and the offset where `<bogus>` begins.
(define (with-second-datum text verb extra)
  (let* ((heading (string-append "### `" verb "`"))
         (at (index-of text heading 0))
         (form (and at (index-of text "\n    (" at)))
         (end (and form (index-of text "\n" (+ form 1)))))
    (and end (string-append (substring text 0 end) " " extra (substring text end (string-length text))))))

(want "F104 CONTROL: a README copy with a second datum in move's usage block is red, naming move and the offset"
      (let ((copy (with-second-datum readme "move" "<bogus>")))
        (and copy (sections-with-a-second-datum copy)))
      (let* ((copy (with-second-datum readme "move" "<bogus>"))
             (block (first-block-after
                      (cdr (let find ((ls (lines-of copy)))
                             (if (equal? (heading-verb (car ls)) "move") ls (find (cdr ls))))))))
        (list (list 'move (and block (index-of block "<bogus>" 0))))))

;; A README copy with a blank line after move's usage form and then an
;; indented line in the same block. The expected offset is counted from the
;; form's own line, not from the function under test: the form line, its
;; newline, the blank line's newline, and four spaces of indent.
(define (with-line-after-blank text verb line)
  (let* ((heading (string-append "### `" verb "`"))
         (at (index-of text heading 0))
         (form (and at (index-of text "\n    (" at)))
         (end (and form (index-of text "\n" (+ form 1)))))
    (and end (list (string-append (substring text 0 end) "\n\n    " line
                                  (substring text end (string-length text)))
                   (- end (+ form 1))))))

(want "F104 CONTROL: a second datum after a blank line inside move's usage block is red, naming move and the offset"
      (let ((copy (with-line-after-blank readme "move" "<bogus>")))
        (and copy (sections-with-a-second-datum (car copy))))
      (let ((copy (with-line-after-blank readme "move" "<bogus>")))
        (and copy (list (list 'move (+ (cadr copy) 1 1 4))))))

;; The blank line is a single tab (r2 review): still blank, still inside the
;; block, and kept as written, so the offset counts it -- the form line, its
;; newline, the tab and its newline, and four spaces of indent.
(define (with-line-after-tab text verb line)
  (let* ((heading (string-append "### `" verb "`"))
         (at (index-of text heading 0))
         (form (and at (index-of text "\n    (" at)))
         (end (and form (index-of text "\n" (+ form 1)))))
    (and end (list (string-append (substring text 0 end) "\n\t\n    " line
                                  (substring text end (string-length text)))
                   (- end (+ form 1))))))

(want "F104 CONTROL: a second datum after a tab-only blank line inside move's usage block is red, at its source offset"
      (let ((copy (with-line-after-tab readme "move" "<bogus>")))
        (and copy (sections-with-a-second-datum (car copy))))
      (let ((copy (with-line-after-tab readme "move" "<bogus>")))
        (and copy (list (list 'move (+ (cadr copy) 1 2 4))))))

(want "F104 CONTROL: a comment after a blank line inside move's usage block is not a second datum"
      (let ((copy (with-line-after-blank readme "move" "; a note")))
        (and copy (sections-with-a-second-datum (car copy))))
      '())


;; ---- DOC-4: the options every verb takes ---------------------------------------
;;
;; KEY: DOC-2 CANNOT SEE THESE. It asks, per verb, whether the options a
;; verb advertises are accepted and whether the ones it accepts are
;; advertised -- and it excuses the common ones, because listing
;; `--store` under thirty-three verbs would say it was special to each.
;; So an option accepted by EVERY verb is advertised by none of them, and
;; falls through both directions. Measured: `--wire` was in no section of
;; the README at all while DOC-2 was green.
;;
;; NEVER: BOTH WAYS, against the parser's own common list.
(define transport-options
  (let* ((text (file-text "../arguments.sc"))
         (at (index-of text "(append '(\"--store\"" 0)))
    (and at
         (let ((end (index-of text ")" at)))
           (and end (dashed-tokens (substring text at end)))))))

;; `--wire` is the flag half of the same idea, spelled in a different
;; place; it is named here because the table it lives in is `(cons
;; "--wire" (case verb ...))` and a scan for a quoted list would miss it.
(define every-verb-options
  (and transport-options (cons "--wire" transport-options)))

(define global-section
  (let ((at (index-of readme "## Global options" 0)))
    (and at
         (let ((end (index-of readme "\n## " (+ at 5))))
           (and end (substring readme at end))))))

(want "DOC-4 the README has a section for the options every verb takes"
      (if global-section 'present 'MISSING)
      'present)

(want "DOC-4 every option the parser takes for all verbs is in that section"
      (if (and global-section every-verb-options)
          (filter (lambda (o) (not (contains? global-section o))) every-verb-options)
          'COULD-NOT-READ-ONE-OF-THEM)
      '())

;; NEVER: AND THE OTHER WAY, so the section cannot list an option that is not
;; global -- which would tell a reader every verb takes something only
;; one of them does.
(want "DOC-4 every option that section lists really is taken by every verb"
      (if (and global-section every-verb-options)
          (filter (lambda (o) (not (member o every-verb-options)))
                  (dashed-tokens global-section))
          'COULD-NOT-READ-ONE-OF-THEM)
      '())


;; ---- DOC-5: the holding rule reaches both readers ------------------------------
;;
;; NOTE: IT IS A CONVENTION WITH NO MACHINE BEHIND IT -- nothing records who
;; holds a writer id and nothing refuses a second process -- so the only
;; thing that can carry it is the text. It has to be in BOTH places: the
;; README for a person, and `write-protocol` for an agent, which is what
;; the MCP tool descriptions are built from.
;;
;; NEVER: CHEAP ON PURPOSE. This file already holds both texts; the row is one
;; phrase looked for in each.
(define holding-phrase "one agent at a time")

(want "DOC-5 the writer-holding rule is in the README and in the protocol constant"
      (list (if (contains? readme holding-phrase) 'in-the-readme 'MISSING-FROM-README)
            (if (contains? write-protocol holding-phrase) 'in-the-constant 'MISSING-FROM-CONSTANT))
      '(in-the-readme in-the-constant))

;; ---- F19: what import-code --datum does with comments, in both places ----------
;;
;; NOTE: THE SAME SENTENCE, IN THE README'S import-code SECTION AND IN THE
;; VERB'S CATALOGUE DESCRIPTION, which is what `describe` publishes and the
;; MCP tool description is built from. The catalogue is read as the running
;; product answers it, not as source text. Whitespace is compared squashed:
;; the README wraps the sentence across lines, and that is not a difference in
;; what is said. What the answer does
;; is held by `datum-import.sc`'s F19 rows, not here.
(define datum-comment-phrase
  "the whole-line ; comments directly above a form become its doc; a ; comment inside a form is dropped, and the answer warns with its line and column. A #| |# block comment, and any comment inside a datum discarded with #;, is dropped with neither")

(define (squashed text)
  (let loop ((cs (string->list text)) (space #f) (out '()))
    (cond ((null? cs) (list->string (reverse out)))
          ((char-whitespace? (car cs)) (loop (cdr cs) #t out))
          (space (loop (cdr cs) #f (cons (car cs) (if (null? out) out (cons #\space out)))))
          (else (loop (cdr cs) #f (cons (car cs) out))))))

(define import-code-section
  (let ((at (index-of readme "### `import-code`" 0)))
    (and at
         (let ((end (index-of readme "\n### " (+ at 5))))
           (substring readme at (or end (string-length readme)))))))

(define import-code-description
  (let ((entry (assq 'import-code (verb-catalogue))))
    (and entry (caddr entry))))

(want "F19 what import-code --datum does with comments is in the README's import-code section and in the verb's catalogue description"
      (list (if (and import-code-section (contains? (squashed import-code-section) datum-comment-phrase))
                'in-the-readme-section 'MISSING-FROM-README-SECTION)
            (if (and import-code-description
                     (contains? (squashed import-code-description) datum-comment-phrase))
                'in-the-description 'MISSING-FROM-DESCRIPTION))
      '(in-the-readme-section in-the-description))

;; NOTE: WHICH FILES import-code --datum READS, AND HOW A REFUSAL NAMES ITS
;; FILE, IN BOTH PLACES, compared as the sentence above is. What the answer
;; does is held by datum-import.sc's selection rows, not here.
(define datum-selection-phrase
  "only Scheme files are read: those the language table gives to Scheme by extension (ss, sc, scm, sls, matched exactly); every other file the directory walk returns (it does not enter a name that starts with a dot) is listed, in the order it was walked, in the answer's skipped clause, which is there only when something was skipped. A file the reader refuses is named in the refusal's path clause.")
(want "which files import-code --datum reads, and the file a refusal names, are in the README's import-code section and in the verb's catalogue description"
      (list (if (and import-code-section (contains? (squashed import-code-section) datum-selection-phrase))
                'in-the-readme-section 'MISSING-FROM-README-SECTION)
            (if (and import-code-description
                     (contains? (squashed import-code-description) datum-selection-phrase))
                'in-the-description 'MISSING-FROM-DESCRIPTION))
      '(in-the-readme-section in-the-description))

;; NOTE: WHAT THE TEXT IMPORT SKIPS, IN BOTH PLACES, compared as the sentences
;; above are. What the answer does is held by code-import.sc's text rows.
(define text-selection-phrase
  "skips a file that is not UTF-8 text or that holds a NUL byte, and lists it in the same skipped clause; it is decided by the bytes, not the name, so a source file in a legacy 8-bit encoding or in UTF-16 is skipped and listed, not imported, unless its bytes happen to be valid UTF-8 with no NUL.")
(want "what the text import skips, and why, is in the README's import-code section and in the verb's catalogue description"
      (list (if (and import-code-section (contains? (squashed import-code-section) text-selection-phrase))
                'in-the-readme-section 'MISSING-FROM-README-SECTION)
            (if (and import-code-description
                     (contains? (squashed import-code-description) text-selection-phrase))
                'in-the-description 'MISSING-FROM-DESCRIPTION))
      '(in-the-readme-section in-the-description))

;; ---- two README sentences the product's behaviour depends on ---------------
;;
;; NOTE: LITERAL, BECAUSE THE NAMES ROW CANNOT SEE THEM. The usage rows above
;; compare option names, and `--under` is named whether it is bracketed or
;; not; whether insert's usage says it is optional, and whether export-code
;; says which files it writes by default, is only in the words.
(define (readme-section heading)
  (let ((at (index-of readme heading 0)))
    (and at
         (let ((end (index-of readme "\n### " (+ at 5))))
           (substring readme at (or end (string-length readme)))))))
(want "insert's README usage brackets --under as optional, and says where the block goes without it"
      (let ((sec (readme-section "### `insert`")))
        (list (and sec (contains? sec "(insert (\"--under\" <id>)") #t)
              (and sec (contains? (squashed sec) "Without --under the block goes under root.") #t)))
      '(#t #t))
(want "export-code's README section says it writes the text-mode files by default, and (files 0) when there are none"
      (let ((sec (readme-section "### `export-code`")))
        (and sec (contains? (squashed sec) "Without `--datum` it writes the text-mode files -- the blocks `import-code` made without `--datum`; a store with no text-mode files answers `(ok (files 0))`.") #t))
      #t)

;; ---- the platforms it names, against the table's rows -------------------------
;;
;; KEY: THE PLATFORMS ARE THE TABLE'S, not a list here: every row's
;; (system machine) is read from platform-numbers.sc, and the README's two
;; sentences -- the one that lists the rows and the one that lists what is
;; refused -- are read as segments, each starting at a system's name and
;; holding the machine names that follow it. The only copy kept here is
;; the README's word for each system.
(define system-words '(("Darwin" . "macOS") ("Linux" . "Linux") ("FreeBSD" . "FreeBSD")))
(define row-pairs
  (map (lambda (r) (let ((k (reading-key r))) (list (car k) (cadr k)))) (platform-readings)))
(define row-machines
  (fold-left (lambda (acc p) (add-unique (cadr p) acc)) '() row-pairs))

;; -> the text from the first occurrence of START to the first END after
;; it, or #f.
(define (span text start end)
  (let ((i (index-of text start 0)))
    (and i (let ((j (index-of text end (+ i (string-length start)))))
             (and j (substring text i j))))))

;; -> the (system machine) pairs a sentence names: each system word opens a
;; segment that runs to the next system word, and every row's machine name
;; found in a segment pairs with that segment's system.
(define (named-pairs sentence)
  (let* ((starts (filter (lambda (x) (car x))
                         (map (lambda (sw) (cons (index-of sentence (cdr sw) 0) (car sw))) system-words)))
         (starts (list-sort (lambda (a b) (< (car a) (car b))) starts)))
    (let loop ((ss starts) (out '()))
      (if (null? ss)
          (list-sort (lambda (a b) (string<? (apply string-append a) (apply string-append b))) out)
          (let* ((from (caar ss))
                 (to (if (null? (cdr ss)) (string-length sentence) (caadr ss)))
                 (segment (substring sentence from to)))
            (loop (cdr ss)
                  (fold-left (lambda (acc m) (if (contains? segment m) (add-unique (list (cdar ss) m) acc) acc))
                             out row-machines)))))))

(define (sorted-pairs ps)
  (list-sort (lambda (a b) (string<? (apply string-append a) (apply string-append b))) ps))

(define rows-sentence (let ((t (span readme "The rows:" "."))) (and t (squashed t))))
(define refused-sentence (let ((t (span readme "On any other platform" "writes"))) (and t (squashed t))))

(want "DOC-P the README's sentence listing the rows names exactly the table's (system machine) pairs"
      (and rows-sentence (named-pairs rows-sentence))
      (sorted-pairs row-pairs))
(want "DOC-P the README's list of refused platforms names no platform the table has a row for"
      (and refused-sentence (filter (lambda (p) (member p row-pairs)) (named-pairs refused-sentence)))
      '())
(want "DOC-P CONTROL: the reader of the rows sentence sees a pair it lacks, and a pair the table lacks"
      (list (named-pairs "The rows: macOS on arm64, Linux on x86_64 and on aarch64 with glibc, and FreeBSD 15 on amd64.")
            (named-pairs "On any other platform -- another architecture, macOS on x86_64 or under Rosetta, a Linux with musl --"))
      (list (sorted-pairs '(("Darwin" "arm64") ("Linux" "x86_64") ("Linux" "aarch64") ("FreeBSD" "amd64")))
            '(("Darwin" "x86_64"))))

;; ---- the environment variables it lists ---------------------------------------
;;
;; NOTE: THE TRUTH SOURCE IS NOT ONLY THE LIBRARIES. `THEOURGIA_LIBDIR` has
;; no `getenv` anywhere in Scheme -- `test/env.sh` and `test/paths.py`
;; read it -- and exempting it would have been a rule with one permanent
;; exception standing where the rule should be. The two files are read
;; instead, so the set is the whole of what the tree reads.

(define (scheme-sources dir)
  (filter (lambda (f)
            (let ((n (string-length f)))
              (and (> n 3) (string=? (substring f (- n 3) n) ".sc"))))
          (map (lambda (f) (string-append dir "/" f)) (directory-list dir))))

;; An environment variable's name, wherever it is written: a run of
;; capitals and underscores. NOTE: The validity test is what keeps a
;; mis-parse out of the set -- without it this gate read its own source,
;; which contains the literal "(getenv ", and produced a "variable" whose
;; name began with a close parenthesis and a newline.
;; NOTE: DIGITS COUNT AFTER THE FIRST CHARACTER (F89). A declared name such
;; as THEOURGIA_X2 must be findable in the README, or documenting it could
;; never satisfy DOC-3.
(define (upper-token? s)
  (and (>= (string-length s) 4)
       (let ((c0 (string-ref s 0))) (or (char-upper-case? c0) (char=? c0 #\_)))
       (let loop ((i 0))
         (or (>= i (string-length s))
             (and (let ((c (string-ref s i)))
                    (or (char-upper-case? c) (char=? c #\_) (and (> i 0) (char<=? #\0 c #\9))))
                  (loop (+ i 1)))))))

(define (upper-tokens text)
  (let loop ((i 0) (acc '()) (out '()))
    (cond
      ((>= i (string-length text))
       (let ((t (list->string (reverse acc))))
         (if (upper-token? t) (add-unique t out) out)))
      ((let ((c (string-ref text i)))
         (or (char-upper-case? c) (char=? c #\_) (and (pair? acc) (char<=? #\0 c #\9))))
       (loop (+ i 1) (cons (string-ref text i) acc) out))
      (else
       (let ((t (list->string (reverse acc))))
         (loop (+ i 1) '() (if (upper-token? t) (add-unique t out) out)))))))

;; NEVER: A NAME ONLY COUNTS AS A CALL SITE IF `getenv` ASKED FOR IT. Any
;; capitalised word would otherwise qualify, and the row would be about
;; the README's prose rather than about the program.
(define (getenv-names-in text)
  (let ((marker "(getenv \""))
    (let loop ((i 0) (out '()))
      (let ((at (let scan ((j i))
                  (cond ((> (+ j (string-length marker)) (string-length text)) #f)
                        ((string=? (substring text j (+ j (string-length marker))) marker) j)
                        (else (scan (+ j 1)))))))
        (if (not at)
            out
            (let scan ((j (+ at (string-length marker))) (acc '()))
              (cond ((>= j (string-length text)) out)
                    ((char=? (string-ref text j) #\") 
                     (let ((t (list->string (reverse acc))))
                       (loop j (if (upper-token? t) (add-unique t out) out))))
                    (else (scan (+ j 1) (cons (string-ref text j) acc))))))))))

;; NOTE: `THEOURGIA_LIBDIR` IS READ BY NEITHER A `getenv` NOR A LIBRARY --
;; `test/env.sh` and `test/paths.py` read it -- so those two files are
;; read in their own idiom. NEVER: NOT by looking for capitalised words:
;; that version reported `COMPARING`, `DIRECTORIES` and `SOCKET_LIMIT`
;; as environment variables the README had failed to document, because a
;; comment and a constant are also capitals.
(define (shell-env-names text)
  (let loop ((i 0) (out '()))
    (cond
      ((>= i (string-length text)) out)
      ((char=? (string-ref text i) #\$)
       (let scan ((j (if (and (< (+ i 1) (string-length text))
                              (char=? (string-ref text (+ i 1)) #\{))
                         (+ i 2) (+ i 1)))
                  (acc '()))
         (cond ((or (>= j (string-length text))
                    (not (let ((c (string-ref text j)))
                           (or (char-upper-case? c) (char=? c #\_)))))
                (let ((t (list->string (reverse acc))))
                  (loop j (if (upper-token? t) (add-unique t out) out))))
               (else (scan (+ j 1) (cons (string-ref text j) acc))))))
      (else (loop (+ i 1) out)))))

(define (python-env-names text)
  (let ((marker "os.environ"))
    (let loop ((i 0) (out '()))
      (let ((at (let scan ((j i))
                  (cond ((> (+ j (string-length marker)) (string-length text)) #f)
                        ((string=? (substring text j (+ j (string-length marker))) marker) j)
                        (else (scan (+ j 1)))))))
        (if (not at)
            out
            (let ((q (let scan ((j (+ at (string-length marker))))
                       (cond ((>= j (string-length text)) #f)
                             ((or (char=? (string-ref text j) #\')
                                  (char=? (string-ref text j) #\")) j)
                             ((char=? (string-ref text j) #\newline) #f)
                             (else (scan (+ j 1)))))))
              (if (not q)
                  (loop (+ at (string-length marker)) out)
                  (let ((close (string-ref text q)))
                    (let scan ((j (+ q 1)) (acc '()))
                      (cond ((>= j (string-length text)) out)
                            ((char=? (string-ref text j) close)
                             (let ((t (list->string (reverse acc))))
                               (loop j (if (upper-token? t) (add-unique t out) out))))
                            (else (scan (+ j 1) (cons (string-ref text j) acc))))))))))))) 

;; NEVER: THE RUNNER AND THE LAUNCHER DECLARE WHAT THEY READ (F89). They are
;; not Scheme, so no `getenv` finds what they read, and the runner's
;; `${THEOURGIA_RUNNER_NORMALISED:-}` went undocumented while this row was
;; green. Each carries one line `# ENVIRONMENT READ: <names>` and one line
;; `# ENVIRONMENT NAMED, NOT READ: <names>`; DOC-3 takes the first.
;;
;; KEY: DECLARED, NOT PARSED. Two review rounds of this batch read the files'
;; text for expansions, and each round found the next idiom the scanner got
;; wrong -- a comment after code, a write split over two lines, a `#` line
;; inside a double-quoted string. Ending that takes a shell lexer and a perl
;; lexer. The declaration is a person's decision, written once; what stays
;; mechanical is only the TWIN below, a coarse count that cannot be fooled
;; by syntax: every THEOURGIA_* token anywhere in the file, comments and
;; strings included, must be on one of the two lines. The text scanners of
;; those rounds are gone, not kept beside this: two readers of one fact is
;; the shape that failed.
;; NEVER: THE ALPHABET IS ASCII, [A-Z0-9_]. Chez's char-upper-case? and
;; char-numeric? are Unicode: with them "THEOURGIA_RUN" followed by an
;; accented capital read as one undeclared name, and a name after one read
;; as no name at all (review round 3, measured).
(define (ascii-name-char? c)
  (or (char<=? #\A c #\Z) (char<=? #\0 c #\9) (char=? c #\_)))
(define (theourgia-tokens text)
  (let ((n (string-length text)) (prefix "THEOURGIA_"))
    (let loop ((i 0) (out '()))
      (cond
        ((> (+ i (string-length prefix)) n) (reverse out))
        ((and (string=? (substring text i (+ i (string-length prefix))) prefix)
              (or (= i 0) (not (ascii-name-char? (string-ref text (- i 1))))))
         (let scan ((j (+ i (string-length prefix))))
           (if (and (< j n) (ascii-name-char? (string-ref text j)))
               (scan (+ j 1))
               ;; THE PREFIX ALONE IS NOT A NAME: prose says "THEOURGIA_*".
               (loop j (if (> j (+ i (string-length prefix)))
                           (add-unique (substring text i j) out)
                           out)))))
        (else (loop (+ i 1) out))))))
(define (declared-line text label)
  (let ((head (string-append "# " label ":")))
    (let loop ((ls (lines-of text)))
      (cond ((null? ls) #f)
            ((starts-with? (car ls) head)
             (theourgia-tokens (substring (car ls) (string-length head) (string-length (car ls)))))
            (else (loop (cdr ls)))))))
(define (declared-read text) (declared-line text "ENVIRONMENT READ"))
(define (declared-named text) (declared-line text "ENVIRONMENT NAMED, NOT READ"))
(define (undeclared-tokens text)
  (let ((known (append (or (declared-read text) '()) (or (declared-named text) '()))))
    (filter (lambda (t) (not (member t known))) (theourgia-tokens text))))
(define declaring-files '("run-fixtures.sh" "launch.pl"))
(define runner-and-launcher-names
  (apply append (map (lambda (f) (or (declared-read (file-text f)) '())) declaring-files)))

(define call-site-names
  (sorted-strings
    (fold-left (lambda (acc n) (add-unique n acc)) '()
      (append
        (fold-left (lambda (acc f) (append (getenv-names-in (file-text f)) acc)) '()
                   (append (scheme-sources "..") (scheme-sources ".")))
        (shell-env-names (file-text "env.sh"))
        (python-env-names (file-text "paths.py"))
        runner-and-launcher-names))))

;; ONE PLACE COMPUTES WHAT A README LEAVES OUT, so the row and its negative
;; self-test below ask the same function.
(define (env-names-missing-from readme-text)
  (let ((listed (fold-left (lambda (acc n) (if (member n call-site-names) (add-unique n acc) acc))
                           '() (upper-tokens readme-text))))
    (filter (lambda (n) (not (member n listed))) call-site-names)))

(want "DOC-3 the README lists every environment variable the tree reads"
      (env-names-missing-from readme)
      '())

;; F89: THE RUNNER'S NAME IS READ, AND ITS ABSENCE FROM THE README IS RED.
(want "F89-1 DOC-3 reads THEOURGIA_RUNNER_NORMALISED from run-fixtures.sh, as a name the tree reads"
      (list (and (member "THEOURGIA_RUNNER_NORMALISED" runner-and-launcher-names) #t)
            (and (member "THEOURGIA_RUNNER_NORMALISED" call-site-names) #t))
      '(#t #t))
(want "F89-1 TWIN: the runner and the launcher each carry both declared lines"
      (map (lambda (f) (list f (and (declared-read (file-text f)) #t) (and (declared-named (file-text f)) #t)))
           declaring-files)
      (map (lambda (f) (list f #t #t)) declaring-files))
(want "F89-1 TWIN: every THEOURGIA_* token in the runner and the launcher, comments and strings included, is on a declared line"
      (map (lambda (f) (cons f (undeclared-tokens (file-text f)))) declaring-files)
      (map (lambda (f) (list f)) declaring-files))
;; THE INJECTED NAME IS ONE THE RUNNER DOES NOT ALREADY HOLD: a fixed name that
;; someone later declared would make this row red on a correct file (review
;; round 3). The first THEOURGIA_UNDECLARED<n> absent from the file is used,
;; and it must come back alone.
(define runner-text (file-text "run-fixtures.sh"))
(define fresh-name
  (let loop ((k 1))
    (let ((name (string-append "THEOURGIA_UNDECLARED" (number->string k))))
      (if (member name (theourgia-tokens runner-text)) (loop (+ k 1)) name))))
(want "F89-1 TWIN NEGATIVE: a copy of the runner holding one name it did not hold, in a comment, is red naming it"
      ;; ON A LINE OF ITS OWN: a file whose last line has no newline would
      ;; otherwise take the comment into that line (review round 4).
      (undeclared-tokens (string-append runner-text "\n# see " fresh-name " here\n"))
      (list fresh-name))
(want "F89-1 TWIN NEGATIVE: the alphabet is ASCII: an accented capital ends a name, and a name after one is still read"
      ;; A SPECIMEN OF ITS OWN, NOT THE RUNNER: the names in it are in no
      ;; file, so no declaration, renaming or other token of the runner can
      ;; answer for them (review round 4).
      (theourgia-tokens "# THEOURGIA_ASCIIA\x00C9; and \x00C9;THEOURGIA_ASCIIB9 end\n")
      '("THEOURGIA_ASCIIA" "THEOURGIA_ASCIIB9"))
(want "F89-2 a README copy without the THEOURGIA_RUNNER_NORMALISED line is red, naming it"
      (env-names-missing-from
        (let loop ((ls (lines-of readme)) (out '()))
          (cond ((null? ls) (apply string-append (reverse out)))
                ((contains? (car ls) "THEOURGIA_RUNNER_NORMALISED") (loop (cdr ls) out))
                (else (loop (cdr ls) (cons (string-append (car ls) "\n") out))))))
      '("THEOURGIA_RUNNER_NORMALISED"))

;; ---- the instrument -----------------------------------------------------------
;;
;; NEVER: ALL FOUR ROWS ABOVE ARE SATISFIED BY READING NOTHING. An empty
;; README, an unreadable path, a `directory-list` that came back short --
;; each of those makes every comparison above compare nothing with
;; nothing and print `ok`.
(want "DOC-0 the gate read a README, a verb list, options and call sites"
      (list (> (string-length readme) 10000)
            (> (length program-verbs) 30)
            (> (length advertised) 20)
            (> (length call-site-names) 10))
      '(#t #t #t #t))

(display (string-append "rows: " (number->string rows) "\n"
                        (number->string bad) " failures\n"
                        "docs-check complete\n"))
(exit (if (zero? bad) 0 1))
