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
              get-string-all directory-list file-exists?)
        (only (theourgia arguments) parse-arguments)
        (only (theourgia rpc) rpc-verbs write-protocol))

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

(define program-verbs
  (sorted-strings (map symbol->string (append (rpc-verbs) '(eval serve)))))

(want "DOC-1 the README documents exactly the verbs the program answers to"
      readme-heading-verbs program-verbs)

;; ---- the options it advertises ------------------------------------------------
;;
;; NEVER: ONLY THE HEADING LINE AND THE INDENTED USAGE BLOCK COUNT AS AN
;; ADVERTISEMENT. Prose has to be able to NAME an option in order to say
;; something about it -- that it was broken, that it is the same spelling
;; another verb uses -- and a scanner reading the whole section cannot
;; tell the two apart. Measured while writing that README: a paragraph
;; saying "`--writer` does not work on this verb" was read as the verb
;; advertising `--writer`.

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
            => (lambda (v)
                 (loop (cdr ls) v
                       (append (map (lambda (o) (list (string->symbol v) o)) (dashed-tokens l))
                               out))))
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
    '() (append (rpc-verbs) '(eval serve))))

(want "DOC-2 every verb-specific option the parser accepts is in the README"
      unadvertised '())


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
