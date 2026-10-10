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

;; Exporting a writer's working view (F17): `export-md --working` and
;; `export-code --working`.
;;
;; KEY: EVERY ROW RUNS THE REAL COMMAND LINE AS A PROCESS, as
;; eval-working.sc does, and reads the files the export wrote.
;;
;; NEVER: THE VIEW IS THE ONE `eval --working` RUNS ON. Its committed side is
;; pinned to the join of the writer's own drafts' cuts; a commit another
;; writer made after those drafts is not in it. EVAL AGREES reads the same
;; block both ways and compares.
;;
;; NOTE: A DRAFT CANNOT INSERT A BLOCK. `write` answers `unknown-id` for an id
;; the store does not hold, so the view has the committed blocks and no
;; others; W11-export-working uses two draft EDITS, a Markdown section's
;; text and a code block's source.

(import (chezscheme)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) block-hash reduce-applied-cut draft-version)
        (only (theourgia wire) storable-encode sexpr->string-extended)
        (only (theourgia log) writer-directory))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S ROOT (F71); run alone, /tmp.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/exportworking-" pid-text))
(define store (string-append here "/store"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (put! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'truncate))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; NOTE: SINGLE QUOTES, so the fixture never hands a shell anything to
;; expand -- the programs under test must receive the bytes as written.
(define (quoted a)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                             (string->list a)))
                 "'"))

(define (cli . args)
  (apply cli-reading "/dev/null" args))
;; THE SAME, WITH STANDARD INPUT FROM A FILE: `batch` reads its intents
;; there, and an argument in their place is answered with its usage.
(define (cli-reading input . args)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " --wire < " input " > " out " 2>&1"))
    (file-text out)))

;; Every datum the answer holds, or the text itself when it does not read.
(define (data-of text)
  (guard (e (#t (list 'UNREADABLE text)))
    (let ((p (open-string-input-port text)))
      (let loop ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

;; The first list anywhere in `x` headed by `head`, or #f.
(define (find-headed x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (find-headed (car x) head) (find-headed (cdr x) head)))
        ((vector? x) (find-headed (vector->list x) head))
        (else #f)))

;; The id of the block holding `needle`, as `grep` answers it:
;; `(match <id> <line-no> "<text>")`.
(define (block-holding needle)
  (let ((m (find-headed (data-of (cli "grep" needle)) 'match)))
    (and m (pair? (cdr m)) (string? (cadr m)) (cadr m))))

;; THE ANSWER, the last datum printed: stderr is merged into the text, and a
;; line the command line writes there, such as the machine home it
;; announces, can come before it. #f when nothing reads.
(define (answer-of text)
  (let ((d (data-of text)))
    (and (pair? d) (car (reverse d)))))

;; An answer's own clause, read as a datum: `(cut ...)`, `(working #t)`.
(define (clause-of text head)
  (let ((a (answer-of text)))
    (and (pair? a) (memq (car a) '(ok error))
         (assq head (filter pair? (cdr a))))))

;; Every regular file under `dir`, as (relative-path . text), sorted.
(define (tree-of dir)
  (let walk ((rel ""))
    (let ((full (if (string=? rel "") dir (string-append dir "/" rel))))
      (cond ((file-directory? full)
             (apply append
                    (map (lambda (name) (walk (if (string=? rel "") name (string-append rel "/" name))))
                         (list-sort string<? (directory-list full)))))
            ((file-regular? full) (list (cons rel (file-text full))))
            (else '())))))

(define (fresh-dir name)
  (let ((d (string-append here "/" name)))
    (system (string-append "rm -rf " (quoted d) "; mkdir -p " (quoted d)))
    d))

;; The text of every file an export wrote, joined: the rows ask whether a
;; marker is anywhere in the projection, not in which file.
(define (all-text dir)
  (apply string-append (map cdr (tree-of dir))))

;; ---- the store ----------------------------------------------------------------
;;
;; One Markdown document with two sections and one Scheme file of two forms,
;; imported the ordinary way. Every marker is a word that occurs once, so
;; `grep` names exactly the block that holds it.
(system (string-append "rm -rf " here "; mkdir -p " store))
(cli "init")
(define md-src (fresh-dir "md-src"))
(put! (string-append md-src "/notes.md")
      "# Alpha\n\nalphacommitted here.\n\n## Beta\n\nbetacommitted here.\n")
(define code-src (fresh-dir "code-src"))
;; NOTE: ONE FORM PER FILE. Text-mode import-code keeps a file's text as
;; the blocks its splitter makes, and measured on the first run it kept two
;; forms separated by a blank line as ONE block (F17-00 read the same id
;; twice); two files give two blocks whatever the splitter does.
(put! (string-append code-src "/m.sc") "(define (f) 'onecommitted)\n")
(put! (string-append code-src "/n.sc") "(define (g) 'twocommitted)\n")
(define datum-src (fresh-dir "datum-src"))
(put! (string-append datum-src "/exw.sc")
      "(library (exw) (export h) (import (rnrs))\n(define (h) 'datumcommitted))\n")
(define md-import (cli "import-md" md-src))
(define code-import (cli "import-code" code-src))
(define datum-import (cli "import-code" datum-src "--datum"))

(define alpha-id (block-holding "alphacommitted"))
(define beta-id (block-holding "betacommitted"))
(define one-id (block-holding "onecommitted"))
(define two-id (block-holding "twocommitted"))

;; A datum block stores its form, not text, so `grep` does not see it;
;; `whereis` names the block that defines `h`: `(def <id> ...)`.
(define h-id
  (let ((d (find-headed (data-of (cli "whereis" "h")) 'def)))
    (and d (pair? (cdr d)) (string? (cadr d)) (cadr d))))

;; NEVER: THE PREMISE IS CHECKED, NOT ASSUMED. Every row below writes a draft
;; on one of these blocks, or is refused one on the datum block; if two
;; markers landed in one block, or grep named none, the rows would be about
;; something else and could still read green.
(want "F17-00 PREMISE: the five blocks the rows write drafts on, or try to, are five different blocks"
      (let ((ids (list alpha-id beta-id one-id two-id h-id)))
        (if (and (for-all string? ids)
                 (= 5 (length (fold-left (lambda (acc x) (if (member x acc) acc (cons x acc))) '() ids))))
            'five-blocks
            (list 'ids ids 'md-import md-import 'code-import code-import 'datum-import datum-import)))
      'five-blocks)

;; ---- W11-export-working ---------------------------------------------------------
;;
;; NEVER: THE WRITER'S DRAFTS ARE IN ITS WORKING EXPORT AND NOT IN THE PLAIN ONE.
;; w1 has two drafts: Beta's text and f's source.
(define w1-beta (cli "write" "--writer" "w1" beta-id "betadraftone here.\n"))
(define w1-one (cli "write" "--writer" "w1" one-id "(define (f) 'onedraftone)\n"))

(want "F17-01 PREMISE: w1's two drafts were written"
      (list (if (contains? w1-beta "(ok (saved") 'saved (list 'said w1-beta))
            (if (contains? w1-one "(ok (saved") 'saved (list 'said w1-one)))
      '(saved saved))

(define md-work (fresh-dir "md-work"))
(define md-plain (fresh-dir "md-plain"))
(define md-work-answer (cli "export-md" md-work "--working" "--writer" "w1"))
(define md-plain-answer (cli "export-md" md-plain))

(want "W11-export-working export-md --working writes the writer's draft of a section"
      (let ((t (all-text md-work)))
        (list (contains? t "betadraftone") (contains? t "betacommitted") (contains? t "alphacommitted")))
      '(#t #f #t))

(want "W11-export-working TWIN: plain export-md writes the committed section, not the draft"
      (let ((t (all-text md-plain)))
        (list (contains? t "betadraftone") (contains? t "betacommitted")))
      '(#f #t))

(define code-work (fresh-dir "code-work"))
(define code-plain (fresh-dir "code-plain"))
(define code-work-answer (cli "export-code" code-work "--working" "--writer" "w1"))
(define code-plain-answer (cli "export-code" code-plain))

(want "W11-export-working export-code --working writes the writer's draft of a code block"
      (let ((t (all-text code-work)))
        (list (contains? t "onedraftone") (contains? t "onecommitted") (contains? t "twocommitted")))
      '(#t #f #t))

(want "W11-export-working TWIN: plain export-code writes the committed code, not the draft"
      (let ((t (all-text code-plain)))
        (list (contains? t "onedraftone") (contains? t "onecommitted")))
      '(#f #t))

(define code-raw-work (fresh-dir "code-raw-work"))
(want "W11-export-working export-code --raw --working writes the draft too"
      (begin (cli "export-code" code-raw-work "--raw" "--working" "--writer" "w1")
             (let ((t (all-text code-raw-work)))
               (list (contains? t "onedraftone") (contains? t "onecommitted"))))
      '(#t #f))

;; NEVER: THE ANSWER SAYS IT IS A VIEW, AFTER THE EXISTING CLAUSES, AND ONLY
;; ON THIS ROUTE. The plain answers carry neither clause.
(want "W11-export-working the --working answers end with (cut ...) (working #t); the plain ones carry neither"
      (list (let ((d (data-of md-work-answer)))
              (and (pair? d) (let ((a (car d))) (and (pair? a) (list (car a) (car (list-ref a (- (length a) 2))) (list-ref a (- (length a) 1)))))))
            (let ((d (data-of code-work-answer)))
              (and (pair? d) (let ((a (car d))) (and (pair? a) (list (car a) (car (list-ref a (- (length a) 2))) (list-ref a (- (length a) 1)))))))
            (clause-of md-plain-answer 'cut) (clause-of md-plain-answer 'working)
            (clause-of code-plain-answer 'cut) (clause-of code-plain-answer 'working))
      '((ok cut (working #t)) (ok cut (working #t)) #f #f #f #f))

;; ---- EVAL AGREES ---------------------------------------------------------------
;;
;; NEVER: ONE VIEW. `eval --working` reads the block's source and cut; the
;; export wrote the file and named its cut. The two have to be the same
;; string and the same coordinate, or a writer's eval and its files are
;; about two different views.
(define (eval-src id)
  (let* ((answer (cli "eval" "--working" "--writer" "w1"
                      (string-append "(cdr (assq 'src (cdr (assq 'fields (block \"" id "\")))))")))
         (v (find-headed (data-of answer) 'values)))
    (if (and v (pair? (cdr v)) (pair? (cadr v))) (car (cadr v)) (list 'said answer))))

(define (eval-cut)
  (let ((v (find-headed (data-of (cli "eval" "--working" "--writer" "w1" "(+ 1 2)")) 'working-view)))
    (and v (>= (length v) 3) (caddr v))))

(define (as-text x) (if (bytevector? x) (utf8->string x) x))

(want "EVAL AGREES the draft eval --working reads for a code block is in the file export-code --working wrote"
      (let ((src (as-text (eval-src one-id))))
        (list (and (string? src) (contains? src "onedraftone"))
              (and (string? src) (contains? (all-text code-work) src))))
      '(#t #t))

(want "EVAL AGREES the draft eval --working reads for a section is in the file export-md --working wrote"
      (let ((src (as-text (eval-src beta-id))))
        (list (and (string? src) (contains? src "betadraftone"))
              (and (string? src) (contains? (all-text md-work) src))))
      '(#t #t))

(want "EVAL AGREES the export's (cut ...) is the cut eval --working reports for the same writer"
      (let ((c (clause-of code-work-answer 'cut)) (m (clause-of md-work-answer 'cut)) (e (eval-cut)))
        (list (and c (equal? (cadr c) e)) (and m (equal? (cadr m) e))))
      '(#t #t))

;; ---- W11-cut-plus-working, and the pin ------------------------------------------
;;
;; NEVER: THE VIEW IS THE WRITER'S BASELINE PLUS ITS DRAFTS. w3 drafts first;
;; then w2 commits new text for Alpha and for g; then w4 drafts. w4's
;; baseline is after the commit, so its export shows both the commit and
;; its own drafts. w3's baseline is before it, so its export is pinned
;; there -- the same rule `eval --working` follows without `--latest`.
(cli "write" "--writer" "w3" beta-id "betadraftthree here.\n")
(cli "write" "--writer" "w3" one-id "(define (f) 'onedraftthree)\n")
(cli "write" "--writer" "w2" alpha-id "alphapublished here.\n")
(cli "write" "--writer" "w2" two-id "(define (g) 'twopublished)\n")
(define published (cli "commit" "--writer" "w2" alpha-id two-id))

(want "F17-02 PREMISE: w2's commit was applied to the Markdown block"
      (if (and (contains? published "(ok")
               (contains? (cli "read" alpha-id) "alphapublished"))
          'published
          (list 'said published))
      'published)

;; NOTE: A MEASUREMENT OF THE COMMIT PATH, NOT OF F17 (F119). The commit's
;; plan declares the draft's text as a string (working.sc entry-intent),
;; while its record carries the bytes store.sc's resolve makes of it for a
;; text-mode block; the reducer compares the two verbatim, marks the
;; record plan-mismatch and leaves it pending, so the committed state never
;; gets the new text (archive/theourgia-f119-finding-2026-09-27.md). While
;; that stands this row is red, and so is the export-code half of
;; W11-cut-plus-working below, for that reason and not for the view.
(define code-now (fresh-dir "code-now"))
(define code-now-answer (cli "export-code" code-now))
(want "F17-02 CONTROL: plain export-code after w2's commit of a code block writes the committed file"
      (list (if (contains? code-now-answer "(ok") 'ok (list 'said code-now-answer))
            (contains? (all-text code-now) "twopublished"))
      '(ok #t))

(cli "write" "--writer" "w4" beta-id "betadraftfour here.\n")
(cli "write" "--writer" "w4" one-id "(define (f) 'onedraftfour)\n")

(define md-w4 (fresh-dir "md-w4"))
(define code-w4 (fresh-dir "code-w4"))
(cli "export-md" md-w4 "--working" "--writer" "w4")
(cli "export-code" code-w4 "--working" "--writer" "w4")

(want "W11-cut-plus-working export-md --working shows a commit made before the drafts, and the drafts"
      (let ((t (all-text md-w4)))
        (list (contains? t "alphapublished") (contains? t "betadraftfour")
              (contains? t "alphacommitted") (contains? t "betacommitted")))
      '(#t #t #f #f))

(want "W11-cut-plus-working export-code --working shows a commit made before the drafts, and the drafts"
      (let ((t (all-text code-w4)))
        (list (contains? t "twopublished") (contains? t "onedraftfour")
              (contains? t "twocommitted") (contains? t "onecommitted")))
      '(#t #t #f #f))

(define md-w3 (fresh-dir "md-w3"))
(define code-w3 (fresh-dir "code-w3"))
(cli "export-md" md-w3 "--working" "--writer" "w3")
(cli "export-code" code-w3 "--working" "--writer" "w3")

(want "W11-cut-plus-working TWIN (pinned): a commit made after the drafts is not in export-md --working"
      (let ((t (all-text md-w3)))
        (list (contains? t "alphapublished") (contains? t "alphacommitted") (contains? t "betadraftthree")))
      '(#f #t #t))

;; NOTE: WHILE F119 STANDS this row cannot tell pinned from current: the
;; committed code change never lands, so it is absent either way (M1 left it
;; green). The md TWIN above is the one that measures the pin today.
(want "W11-cut-plus-working TWIN (pinned): a commit made after the drafts is not in export-code --working"
      (let ((t (all-text code-w3)))
        (list (contains? t "twopublished") (contains? t "twocommitted") (contains? t "onedraftthree")))
      '(#f #t #t))

;; NEVER: AND THE PLAIN EXPORT IS THE CURRENT COMMITTED STATE, which holds the
;; commit and none of the drafts.
(define md-now (fresh-dir "md-now"))
(cli "export-md" md-now)
(want "W11-cut-plus-working CONTROL: plain export-md after the commit has it and no draft"
      (let ((t (all-text md-now)))
        (list (contains? t "alphapublished") (contains? t "betacommitted")
              (contains? t "betadraftthree") (contains? t "betadraftfour")))
      '(#t #t #f #f))

;; ---- a datum block's draft ------------------------------------------------------
;;
;; NEVER: A DATUM BLOCK TAKES NO DRAFT. A draft is text and a datum block's
;; code is its body, so `write` refuses one by name (store.sc,
;; draft-on-datum-refusal), and the working view of a datum block is its
;; committed body. Before that refusal a draft could be written on one, and
;; its view read it as the body (design 7.5.21): F17-07 keeps that reading
;; for a draft written then.
(define datum-write (cli "write" "--writer" "w5" h-id "(define (h) 'datumdraft)"))
(define datum-work (fresh-dir "datum-work"))
(define datum-plain (fresh-dir "datum-plain"))
(define datum-work-answer (cli "export-code" datum-work "--datum" "--working" "--writer" "w5"))
(cli "export-code" datum-plain "--datum")

(want "F17-06 write on a datum block is refused by name, and export-code --datum --working writes its committed body"
      (let ((t (all-text datum-work)) (a (answer-of datum-write)))
        (list (and (list? a) (>= (length a) 3) (list-head a 3))
              (contains? t "datumdraft") (contains? t "datumcommitted")
              (clause-of datum-work-answer 'working)))
      '((error bad-request draft-on-datum-unsupported) #f #t (working #t)))

(want "F17-06 TWIN: plain export-code --datum writes the committed body"
      (let ((t (all-text datum-plain)))
        (list (contains? t "datumdraft") (contains? t "datumcommitted")))
      '(#f #t))

;; A DRAFT AN EARLIER BUILD WROTE ON THE DATUM BLOCK, at its baseline now
;; (forge-draft.ss): the view is pinned to the draft's cut, where the block
;; is the datum block it is, so the draft is read as its body.
;;
;; NEVER: A DRAFT THAT DOES NOT READ AS ONE FORM IS REFUSED BY NAME, with the
;; block and the reader's reason, and nothing is written.
(include "forge-draft.ss")
(forge-fresh-draft! store "w6" h-id "(define (h) 'broken")
(define datum-broken (fresh-dir "datum-broken"))
(want "F17-07 an unreadable datum draft is refused as working-draft-unreadable, naming the block"
      (let* ((answer (cli "export-code" datum-broken "--datum" "--working" "--writer" "w6"))
             (a (answer-of answer)))
        (list (and (pair? a) (list (car a) (and (pair? (cdr a)) (cadr a))))
              (clause-of answer 'block)
              (and (clause-of answer 'reason) #t)
              (tree-of datum-broken)))
      (list '(error working-draft-unreadable) (list 'block h-id) #t '()))
;; TWIN: such a draft that reads as one form is written as the block's body.
(forge-fresh-draft! store "w8" h-id "(define (h) 'legacydraft)")
(define datum-legacy (fresh-dir "datum-legacy"))
(want "F17-07 TWIN a datum draft written then, that reads, is written as the body under --working"
      (let ((answer (cli "export-code" datum-legacy "--datum" "--working" "--writer" "w8")))
        (list (clause-of answer 'working)
              (contains? (all-text datum-legacy) "legacydraft")
              (contains? (all-text datum-legacy) "datumcommitted")))
      '((working #t) #t #f))

;; ---- NO-DRAFTS TWIN -------------------------------------------------------------
;;
;; PIN: A WRITER WITH NO DRAFTS EXPORTS EXACTLY WHAT THE PLAIN EXPORT WRITES,
;; byte for byte and file for file. Green on the base once the option
;; exists; it pins rule 4 and is not a measurement of the overlay.
(define (same-trees? verb . extra)
  (let ((a (fresh-dir (string-append verb "-nodrafts-working")))
        (b (fresh-dir (string-append verb "-nodrafts-plain"))))
    (apply cli verb a (append extra (list "--working" "--writer" "w9")))
    (apply cli verb b extra)
    (let ((ta (tree-of a)) (tb (tree-of b)))
      (if (and (pair? ta) (equal? ta tb)) 'identical (list 'working ta 'plain tb)))))

(want "NO-DRAFTS TWIN (PIN): export-md --working for a writer with no drafts is byte-identical to export-md"
      (same-trees? "export-md")
      'identical)
(want "NO-DRAFTS TWIN (PIN): export-code --working for a writer with no drafts is byte-identical to export-code"
      (same-trees? "export-code")
      'identical)

;; ---- the refusals are unchanged -------------------------------------------------
;;
;; NEVER: A DIRECTORY THAT IS NOT ONE IS ANSWERED AS THE PLAIN EXPORT ANSWERS IT,
;; before any view is taken.
(want "F17-03 export-md --working to a path that is not a directory answers as the plain export does"
      (let ((missing (string-append here "/no-such-dir")))
        (list (clause-of (cli "export-md" missing "--working" "--writer" "w1") 'reason)
              (clause-of (cli "export-md" missing) 'reason)))
      '((reason not-a-directory) (reason not-a-directory)))

;; NEVER: --writer IS READ ONLY WITH --working. A plain export accepts it and
;; ignores it, as `read` without --working does.
(define md-plain-w1 (fresh-dir "md-plain-w1"))
(want "F17-04 plain export-md with --writer writes the committed state"
      (begin (cli "export-md" md-plain-w1 "--writer" "w1")
             (equal? (tree-of md-plain-w1) (tree-of md-now)))
      #t)

;; ---- the README -----------------------------------------------------------------
(define readme (file-text "../README.md"))
(want "F17-05 the README's usage forms for export-md and export-code carry --working and --writer"
      (list (contains? readme "(export-md <dir> (\"--with-ids\") (\"--working\") (\"--writer\" <name>))")
            (contains? readme "(export-code <dir> (\"--raw\") (\"--datum\") (\"--working\") (\"--writer\" <name>))"))
      '(#t #t))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\nexport-working complete\n" rows bad)
(exit (if (zero? bad) 0 1))
