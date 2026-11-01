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

;; `read --ast`: a block's body as the parsed tree, references resolved.
;;
;; The answer is `(ok (block <id> <entries> (hash <h>) (ast <node> ...))
;; <validity> <receipt>)`: the entries are the plain read's record after its
;; id, the hash is the block's version, and the tree is md->blocks of a doc
;; or section's src with every `(ref <lexeme> <key>)` given a fourth
;; element, the id of the LIVE block it names in the state read, or #f.
;;
;; EXPECTED VALUES ARE WRITTEN OUT. The trees below are literals, not
;; md->blocks called in a row, and every id, hash and cut in them is bound
;; from the setup before the read it is compared with; nothing expected is
;; taken from the answer under test.
;;
;; THREE STORES. The main one holds the blocks most rows read. A second
;; one holds a block that a peer writer moved and edited concurrently, so
;; its position and its src are in conflict. A third has a fresh writer
;; whose event 805, sequence `md`, inserts a block, so its id ends in `.md`.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce seal-state)
        (only (theourgia reduce) state-read block-hash reduce-applied-cut block-id)
        (only (theourgia md) md->blocks blocks->md)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia wire) encode-record)
        (only (theourgia trace) trace-enable!))

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/read-ast-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- helpers ------------------------------------------------------------------------

;; THE WALKER IS LOOKED UP WHEN A ROW CALLS IT, not imported: a tree without
;; it then reds the rows that use it and runs the rest, where an import
;; would stop the whole file before its first row.
(define (ast-with-targets . args)
  (apply (eval 'ast-with-targets (environment '(theourgia reduce))) args))

;; The clause headed `name` among an answer's elements, or #f.
(define (clause answer name)
  (and (pair? answer) (list? answer)
       (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer))))
(define (heads answer) (map (lambda (c) (and (pair? c) (car c))) (cdr answer)))
(define (entry block name) (and (pair? block) (list? block) (assq name (cddr block))))
(define (string-count text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (lex id) (string-append "[[" id "]]"))
(define (ref id target) (list 'ref (lex id) id target))
;; A write's events; a batch answers `(batch (<item> ...) (done <n>))`, one
;; event per intent, and its items' events are read in order.
(define (events-of answer)
  (if (and (pair? answer) (eq? (car answer) 'batch))
      (apply append (map events-of (cadr answer)))
      (cadr (assq 'events (cdr answer)))))
;; The id of the first block an answer's events made.
(define (made answer)
  (let ((e (car (events-of answer))))
    (block-id (car e) (cdr e))))
(define (last-seq answer)
  (cdr (car (reverse (events-of answer)))))
;; Targets erased: every four-element ref back to md.sc's three.
(define (erase x)
  (cond ((and (pair? x) (eq? (car x) 'ref) (list? x) (= (length x) 4)) (list 'ref (cadr x) (caddr x)))
        ((pair? x) (cons (erase (car x)) (erase (cdr x))))
        (else x)))
;; The read-ast lines a thunk's run writes to the trace.
(define (read-ast-lines thunk)
  (let-values (((port take) (open-string-output-port)))
    (trace-enable! #t)
    (parameterize ((current-error-port port)) (thunk))
    (trace-enable! #f)
    (string-count (take) "(trace read-ast ")))

(define (store-at name)
  (let ((s (string-append root "/" name)))
    (lambda args (rpc-dispatch s args "test"))))
(define main-store (string-append root "/s"))
(define run (store-at "s"))
(define (state) (open-and-reduce main-store))
(define (ins! run parent pred fields)
  (made (run 'batch (format "((insert ~s ~s ~s))" parent pred fields))))

;; ---- the main store ---------------------------------------------------------------

(define WR (cadr (assq 'writer (cdr (run 'init)))))
(define A (ins! run 'root #f '((kind . section) (title . "Alpha") (src . "alpha\n"))))
(define K (ins! run 'root #f '((kind . code) (mode . text) (lang . python) (src . "x = 1\n"))))
(define KD (ins! run 'root #f '((kind . code) (mode . datum) (body . (define y 2)))))
(define FT (ins! run 'root #f '((kind . file) (mode . text) (path . "a.py") (lang . python))))
(define FD (ins! run 'root #f '((kind . file) (mode . datum) (path . "library.sc"))))
(define L (ins! run FD #f '((kind . library) (mode . datum) (lang . chez) (title . "L") (name . (lib l)) (imports . ((rnrs))))))
(define D (ins! run 'root #f `((kind . section) (title . "Deleted") (src . ,(string-append (lex A) "\n")))))
(run 'del D)
(define front "---\nt: 1\n---\n")
(define doc-src (string-append "Intro " (lex A) ".\n"))
(define DOC (ins! run 'root #f `((kind . doc) (title . "Doc") (front . ,front) (src . ,doc-src))))
(define src1
  (string-append "Para with *em*, **strong**, `code`, [a link](http://x), and " (lex A) ".\n\n"
                 "- one\n- two\n\n"
                 "```scheme\n(f x)\n```\n\n"
                 "> quoted\n\n"
                 "| a | b |\n|---|---|\n"))
(define S (ins! run DOC #f `((kind . section) (title . "Sec") (level . 2) (heading-src . "## Sec\n") (src . ,src1))))
(define src2
  (string-append "r " (lex A) " [[" A ".md]] [[" A "|shown]] [[ent:" A "]] [[gone]] [[ent:gone]] " (lex D) " " (lex K) "\n\n"
                 "> q " (lex A) "\n\n"
                 "- item " (lex A) "\n\n"
                 "*e " (lex A) "* **s " (lex A) "**\n"))
(define S2 (ins! run DOC S `((kind . section) (title . "Refs") (level . 2) (heading-src . "## Refs\n") (src . ,src2))))
(define src3
  (string-append "```\n" (lex A) "\n```\n\n"
                 "    " (lex A) " indented\n\n"
                 "span `" (lex A) "` and " (lex A) "\n"))
(define S3 (ins! run DOC S2 `((kind . section) (title . "Code") (level . 2) (heading-src . "## Code\n") (src . ,src3))))
(define DS (ins! run DOC S3 '((kind . section) (title . "Gone") (level . 2) (heading-src . "## Gone\n") (src . "gone section\n"))))
(run 'del DS)
;; A BLOCK THAT CANNOT BE HASHED: a field nested past the depth the encoder
;; takes (nesting-depth.sc's middle band).
(define (nest n) (if (= n 0) "x" (string-append "(" (nest (- n 1)) ")")))
(define U (made (run 'batch (string-append "((insert root #f ((kind . section) (title . \"Unhashable\") (src . \"plain\\n\") (depth-probe . "
                                           (nest 59) "))))"))))
(define BV (ins! run 'root #f '((kind . doc) (title . "Bytes") (src . #vu8(255 254 253 10)))))
(define BU (ins! run 'root #f `((kind . doc) (title . "Text bytes") (src . ,(string->utf8 "Hi\n")))))
;; RA-11: a superseded section under a ground, and the superseding one.
;; RA-6: a decision block in a document, which the markdown projection writes.
(define DD (ins! run 'root #f '((kind . doc) (title . "Decisions") (src . ""))))
(define DEC (ins! run DD #f `((kind . decision) (title . "Rule") (level . 2) (heading-src . "## Rule\n") (src . ,(string-append "We rule " (lex A) ".\n")))))
(define G (ins! run 'root #f '((kind . section) (title . "Ground") (src . "ground\n"))))
(define P (ins! run G #f '((kind . section) (title . "Prior") (src . "prior\n"))))
(define N (ins! run 'root #f '((kind . section) (title . "Newer") (src . "newer\n"))))
(run 'link N "supersedes" P)
;; RA-5: a block whose hash is followed through writes.
(define H5 (ins! run 'root #f '((kind . section) (title . "Hashed") (src . "one\n"))))
;; RA-8: X is deleted after the tag and Y created after it; R names both
;; and is not written after the tag. Y's id is the writer's next event
;; after R's src, the tag and Y's insert are counted: it is predicted, and
;; a SETUP row says the prediction held.
(define X (ins! run 'root #f '((kind . section) (title . "Ex") (src . "ex\n"))))
(define QC (ins! run 'root #f '((kind . section) (title . "Changes") (src . "before\n"))))
(define R-answer (run 'batch (format "((insert root #f ~s))" '((kind . section) (title . "Names") (src . "-\n")))))
(define R (made R-answer))
(define Y-predicted (block-id WR (+ (last-seq R-answer) 3)))
(define src-r (string-append "x " (lex X) " y " (lex Y-predicted) "\n"))
(run 'set R "src" src-r)
(define cut-t1 (reduce-applied-cut (state)))
(run 'tag "t1")
(define Y (ins! run 'root #f '((kind . section) (title . "Why") (src . "why\n"))))
(run 'del X)
(run 'set QC "src" "after\n")

(want "SETUP every main-store block exists and Y is the predicted id"
      (in-order (map string? (list A K KD FT FD L D DOC S S2 S3 DS U BV BU DD DEC G P N H5 X QC R Y))
                (equal? Y Y-predicted))
      (list (map (lambda (x) #t) (iota 25)) #t))

;; ---- the conflict store: a peer moved and edited CS concurrently ---------------------

(define runc (store-at "sc"))
(define WC (cadr (assq 'writer (cdr (runc 'init)))))
(define P1 (ins! runc 'root #f '((kind . section) (title . "P1") (src . "p1\n"))))
(define P2 (ins! runc 'root #f '((kind . section) (title . "P2") (src . "p2\n"))))
(define CS-answer (runc 'batch (format "((insert ~s #f ~s))" P1 '((kind . section) (title . "Contested") (src . "c\n")))))
(define CS (made CS-answer))
(define cs-seq (last-seq CS-answer))
(runc 'move CS P2)
(runc 'set CS "src" "ours\n")
(define peer "zzzzzzzz")
(let loop ((rs (list (encode-record 1 1757300000001 "peer" (list (cons WC cs-seq)) `(move ,CS ,P1 5))
                     (encode-record 2 1757300000002 "peer" (list (cons WC cs-seq)) `(set ,CS src "theirs\n"))))
           (k 1))
  (unless (null? rs)
    (log-publish! (string-append root "/sc") peer k (car rs) (segment-sha (car rs)))
    (loop (cdr rs) (+ k 1))))

;; ---- the store whose writer reaches sequence md -----------------------------------------

(define run3 (store-at "s3"))
(define W3 (cadr (assq 'writer (cdr (run3 'init)))))
(define filler '((kind . section) (title . "f") (src . "f\n")))
;; Fillers up to event 804, in batches, so event 805 is the target's insert.
;; A batch that does not advance by its size ends the loop: the SETUP row
;; below then reads the id the insert got.
(define last3
  (let loop ((seq (let ((c (assoc W3 (reduce-applied-cut (open-and-reduce (string-append root "/s3")))))) (if c (cdr c) 0))))
    (if (>= seq 804)
        seq
        (let* ((k (min 100 (- 804 seq)))
               (a (run3 'batch (format "~s" (map (lambda (i) (list 'insert 'root #f filler)) (iota k)))))
               (s (last-seq a)))
          (if (= s (+ seq k)) (loop s) s)))))
(define T805 (ins! run3 'root #f '((kind . section) (title . "Eight-oh-five") (src . "t\n"))))
(define src-rr (string-append "[[" W3 ".md]] [[ent:" W3 ".md]] [[" W3 ".md.md]] [[" W3 "]]\n"))
(define RR (ins! run3 'root #f `((kind . section) (title . "Names md") (src . ,src-rr))))
(want "SETUP the fresh writer's event 805 is the block <writer>.md"
      (list last3 T805)
      (list 804 (string-append W3 ".md")))

;; ---- what each read is compared with, bound before it --------------------------------

(define st (state))
(define c-now (reduce-applied-cut st))
(define (h id) (block-hash st id))
(define (record-entries id) (cdr (state-read st id)))
(define ast-doc `((para "Intro " ,(ref A A) ".")))
(define ast1
  `((para "Para with " (em "em") ", " (strong "strong") ", " (code "code") ", " (link "http://x" "a link") ", and " ,(ref A A) ".")
    (list #f 1 (item (para "one")) (item (para "two")))
    (code "```" "scheme" "(f x)")
    (quote (para "quoted"))
    (raw-md "| a | b |\n|---|---|")))
(define ast2
  `((para "r " ,(ref A A)
          " " (ref ,(string-append "[[" A ".md]]") ,A ,A)
          " " (ref ,(string-append "[[" A "|shown]]") ,A ,A)
          " " (ref ,(string-append "[[ent:" A "]]") ,(string-append "ent:" A) ,A)
          " " (ref "[[gone]]" "gone" #f)
          " " (ref "[[ent:gone]]" "ent:gone" #f)
          " " ,(ref D #f)
          " " ,(ref K K))
    (quote (para "q " ,(ref A A)))
    (list #f 1 (item (para "item " ,(ref A A))))
    (para (em "e " ,(ref A A)) " " (strong "s " ,(ref A A)))))
(define ast3
  `((code "```" "" ,(lex A))
    (raw-md ,(string-append "    " (lex A) " indented"))
    (para "span " (code ,(lex A)) " and " ,(ref A A))))
(define (block-form id ast) (append (list 'block id) (record-entries id) (list (list 'hash (h id)) (cons 'ast ast))))
(define (one-answer id ast) `(ok ,(block-form id ast) (cut ,c-now) (versions ((,id . ,(h id))))))
;; THE SUBTREE WALK LEAVES A DELETED CHILD OUT: read --recursive of DOC lists
;; DOC, S, S2 and S3, not DS. A tombstone is an item only as the root asked
;; for, which RA-6 reads.
(define doc-ids (list DOC S S2 S3))
(define doc-asts (list ast-doc ast1 ast2 ast3))
;; A tombstone's hash is #f: read-version has none for it, whatever
;; block-hash would say of its record.
(define (item-form id ast)
  (if ast
      (block-form id ast)
      (append (list 'block id) (record-entries id)
              (list (list 'hash (and (not (cdr (assq 'deleted (record-entries id)))) (h id))) '(ast #f)))))

;; ---- RA-1 shape ----------------------------------------------------------------------

(want "RA-1 read <section> --ast is the block form, the tree written out, and the receipt"
      (run 'read S "--ast")
      (one-answer S ast1))
(want "RA-1 the section's record entries come in the record's order, the position a pair under the doc"
      (let ((b (cadr (run 'read S "--ast"))))
        (list (map (lambda (e) (and (pair? e) (car e))) (cddr b))
              (let ((p (entry b 'position))) (and p (pair? (cdr p)) (equal? (cadr p) DOC) (number? (cddr p))))))
      '((deleted fields position edges hash ast) #t))
(want "RA-1 read <doc> --ast is the doc's own src tree; the front matter stays a field"
      (run 'read DOC "--ast")
      (one-answer DOC ast-doc))
(want "RA-1 a section whose position is in conflict answers (position conflict 2) in its block form"
      (entry (cadr (runc 'read CS "--ast")) 'position)
      '(position conflict 2))
(want "RA-1 census: one md->blocks and one parse-inline definition in the product files, md.sc's"
;; A fixture runs in test/, so the tree is its parent; test/ itself, which
;; spells the names in its rows, is left out.
      (let* ((files (let walk ((dir "..") (out '()))
                      (fold-left (lambda (acc name)
                                   (let ((p (string-append dir "/" name)))
                                     (cond ((member p '("../test" "../.git")) acc)
                                           ((file-directory? p) (walk p acc))
                                           ((or (string=? (path-extension p) "sc") (string=? (path-extension p) "ss")) (cons p acc))
                                           (else acc))))
                                 out (list-sort string<? (directory-list dir)))))
             (with (lambda (needle) (filter (lambda (f) (> (string-count (call-with-input-file f get-string-all) needle) 0)) files))))
        (list (with "(define (md->blocks") (with "(define (parse-inline")
              (apply + (map (lambda (f) (string-count (call-with-input-file f get-string-all) "(define (md->blocks")) files))
              (apply + (map (lambda (f) (string-count (call-with-input-file f get-string-all) "(define (parse-inline")) files))))
      '(("../md.sc") ("../md.sc") 1 1))

;; ---- RA-2 resolution ----------------------------------------------------------------------

(want "RA-2 [[A]] [[A.md]] [[A|shown]] [[ent:A]] [[gone]] [[ent:gone]] [[D]] [[K]]: A A A A #f #f #f K, and in a quote, a list item, em and strong"
      (cdr (assq 'ast (cddr (cadr (run 'read S2 "--ast")))))
      ast2)
(want "RA-2 the walk fills targets in a constructed link, a list item and a quote"
      (ast-with-targets st `((para (link "u" "a " (ref ,(lex A) ,A)))
                             (list #t 3 (item (para (ref ,(lex A) ,A))))
                             (quote (para (ref "[[gone]]" "gone")))))
      `((para (link "u" "a " ,(ref A A)))
        (list #t 3 (item (para ,(ref A A))))
        (quote (para (ref "[[gone]]" "gone" #f)))))
(want "RA-2 an id ending in .md: [[<id>]] [[ent:<id>]] [[<id>.md]] resolve to it, [[<writer>]] does not"
      (cdr (assq 'ast (cddr (cadr (run3 'read RR "--ast")))))
      `((para (ref ,(string-append "[[" W3 ".md]]") ,W3 ,T805)
              " " (ref ,(string-append "[[ent:" W3 ".md]]") ,(string-append "ent:" W3) ,T805)
              " " (ref ,(string-append "[[" W3 ".md.md]]") ,(string-append W3 ".md") ,T805)
              " " (ref ,(string-append "[[" W3 "]]") ,W3 #f))))

;; ---- RA-3 code regions (a pin on the parser) --------------------------------------------

;; md->blocks itself is the subject here, so this row is green on a tree
;; without --ast: the reference outside each code region is the control.
(want "RA-3 PIN no reference inside a fence, inside indented code (raw-md) or a code span; the same text outside one is a reference"
      (md->blocks src3)
      (erase ast3))
(want "RA-3 the --ast answer's tree for those regions, targets filled outside them only"
      (cdr (assq 'ast (cddr (cadr (run 'read S3 "--ast")))))
      ast3)

;; ---- RA-4 recursive ---------------------------------------------------------------------

(want "RA-4 read <doc> --ast --recursive lists the items read --recursive lists, in its order"
      (list (map (lambda (b) (cadr b)) (cdr (cadr (run 'read DOC "--ast" "--recursive"))))
            (map (lambda (r) (cdr (assq 'id r))) (cdr (cadr (run 'read DOC "--recursive")))))
      (list doc-ids doc-ids))
(want "RA-4 the whole answer: each item's entries, hash and tree; versions the live items' hashes; then the cut"
      (run 'read DOC "--ast" "--recursive")
      `(ok (items ,@(map item-form doc-ids doc-asts))
           (versions ,(map (lambda (id) (cons id (h id))) (list DOC S S2 S3)))
           (cut ,c-now)))

;; ---- RA-5 hash ----------------------------------------------------------------------------

;; Each reading is caught, so a tree without --ast reds the rows below
;; instead of ending the file here.
(define (hash-read id) (caught (cadr (assq 'hash (cddr (cadr (run 'read id "--ast")))))))
(define h5-0 (block-hash (state) H5))
(define h5-read (hash-read H5))
(define h5-plain (run 'read H5))
(define set-src (run 'set H5 "src" "two\n"))
(define h5-1 (hash-read H5))
(define set-title (run 'set H5 "title" "Hashed again"))
(define h5-2 (hash-read H5))
(run 'set A "title" "Alpha moved")
(define h5-3 (hash-read H5))
(want "RA-5 the hash is the version and the versions pair a plain read gives"
      (list (equal? h5-read h5-0) (equal? (clause h5-plain 'version) (list 'version h5-0))
            (equal? (clause h5-plain 'versions) (list 'versions (list (cons H5 h5-0)))))
      '(#t #t #t))
(want "RA-5 the hash is the token a write's state report carries, it changes after set src and set title, and an unrelated write leaves it"
      (list (equal? (assoc H5 (cadr (clause set-src 'state))) (cons H5 h5-1))
            (equal? (assoc H5 (cadr (clause set-title 'state))) (cons H5 h5-2))
            (equal? h5-1 h5-0) (equal? h5-2 h5-1) (equal? h5-3 h5-2))
      '(#t #t #f #f #t))

;; ---- RA-6 kinds ---------------------------------------------------------------------------

(define (ast-of run id) (assq 'ast (cddr (cadr (run 'read id "--ast")))))
(want "RA-6 a code block (text and datum), a library and a file block answer (ast #f)"
      (map (lambda (id) (ast-of run id)) (list K KD L FT FD))
      '((ast #f) (ast #f) (ast #f) (ast #f) (ast #f)))
;; A BLOCK WITH NO TREE STILL CARRIES THE PLAIN READ'S RECORD, the fields
;; the view derives included: a datum block's names and name, a text block's
;; doc, are not in state-read's record. The plain reads are bound before the
;; --ast reads, and the last element shows the view adds something here, so a
;; block form copied from state-read would be red.
(define plain-K (cadr (run 'read K)))
(define plain-KD (cadr (run 'read KD)))
(define (entries-before-hash b)
  (let loop ((xs (if (and (pair? b) (list? b) (pair? (cdr b))) (cddr b) '())) (out '()))
    (if (or (null? xs) (and (pair? (car xs)) (eq? (caar xs) 'hash)))
        (reverse out)
        (loop (cdr xs) (cons (car xs) out)))))
(want "RA-6 a code block's --ast record is the plain read's, view-derived fields included, single and recursive; the view differs from state-read for the datum block"
      (list (entries-before-hash (cadr (run 'read K "--ast")))
            (entries-before-hash (cadr (run 'read KD "--ast")))
            (entries-before-hash (cadr (cadr (run 'read KD "--ast" "--recursive"))))
            (equal? (cdr plain-KD) (cdr (state-read (state) KD))))
      (list (cdr plain-K) (cdr plain-KD) (cdr plain-KD) #f))
(want "RA-6 a decision block, a kind the markdown projection writes, answers its tree; the projection writes its src into the document"
      (list (ast-of run DEC)
            (let ((t (clause (run 'read DD "--recursive" "--md") 'text)))
              (and t (string? (cadr t)) (> (string-count (cadr t) (string-append "We rule " (lex A) ".")) 0))))
      `((ast (para "We rule " ,(ref A A) ".")) #t))
(want "RA-6 a doc whose src is bytes that are not UTF-8 answers (ast #f (note src-not-text)); UTF-8 bytes are parsed"
      (list (ast-of run BV) (ast-of run BU))
      '((ast #f (note src-not-text)) (ast (para "Hi"))))
(want "RA-6 a section whose src is in conflict answers (ast #f (note src-not-text)), its fields printing the conflict form"
      (let ((b (cadr (runc 'read CS "--ast"))))
        (list (assq 'ast (cddr b))
              (let ((s (assq 'src (cdr (entry b 'fields))))) (and s (pair? (cdr s)) (car (cdr s))))))
      '((ast #f (note src-not-text)) conflict))
(want "RA-6 a deleted section with a string src answers (deleted . #t), (hash #f), (ast #f)"
      (let ((b (cadr (run 'read DS "--ast"))))
        (list (entry b 'deleted) (entry b 'hash) (entry b 'ast)
              (let ((s (assq 'src (cdr (entry b 'fields))))) (and s (string? (cdr s))))))
      '((deleted . #t) (hash #f) (ast #f) #t))
(define c6 (reduce-applied-cut (state)))
(want "RA-6 a deleted root read recursively is its one item, (hash #f) (ast #f), and absent from versions, as the plain recursive read"
      (list (run 'read DS "--ast" "--recursive") (clause (run 'read DS "--recursive") 'versions))
      `((ok (items ,(item-form DS #f)) (versions ()) (cut ,c6)) (versions ())))
(want "RA-6 a block whose hash is unavailable answers (hash (unavailable (reason ...))) with its tree"
      (let ((b (cadr (run 'read U "--ast"))))
        (list (let ((e (entry b 'hash))) (and e (pair? (cadr e)) (car (cadr e))))
              (entry b 'ast)))
      '(unavailable (ast (para "plain"))))

;; ---- RA-7 round trip ----------------------------------------------------------------------

(want "RA-7 the answered tree printed as markdown and parsed again is the same tree, targets erased on both sides"
      (map (lambda (id)
             (let ((t (cdr (assq 'ast (cddr (cadr (run 'read id "--ast"))))))) (equal? (md->blocks (blocks->md (erase t))) (erase t))))
           (list DOC S S2 S3))
      '(#t #t #t #t))
(want "RA-7 the node kinds, written out"
      (map car (cdr (assq 'ast (cddr (cadr (run 'read S "--ast"))))))
      '(para list code quote raw-md))

;; ---- RA-8 options -------------------------------------------------------------------------

(want "RA-8 --ast with --md, --working, --working-info or --signature is incompatible-ast-options"
      (map (lambda (o) (run 'read A "--ast" o)) '("--md" "--working" "--working-info" "--signature"))
      (map (lambda (o) '(error bad-request incompatible-ast-options)) (iota 4)))
(want "RA-8 --ast --cut <tag> answers the tree at the cut, and its cut"
      (let ((a (run 'read QC "--ast" "--cut" "t1")) (b (run 'read QC "--ast")))
        (list (cdr (assq 'ast (cddr (cadr a)))) (clause a 'cut) (cdr (assq 'ast (cddr (cadr b)))) (clause b 'cut)))
      (list '((para "before")) (list 'cut cut-t1) '((para "after")) (list 'cut (reduce-applied-cut (state)))))
(want "RA-8 --ast --cut resolves at the cut: X live then and deleted since, Y created since"
      (list (cdr (assq 'ast (cddr (cadr (run 'read R "--ast" "--cut" "t1")))))
            (cdr (assq 'ast (cddr (cadr (run 'read R "--ast"))))))
      (list `((para "x " ,(ref X X) " y " ,(ref Y #f)))
            `((para "x " ,(ref X #f) " y " ,(ref Y Y)))))
(want "RA-8 --recursive --ast is --ast --recursive, an items answer"
      (let ((a (run 'read DOC "--recursive" "--ast")))
        (list (equal? a (run 'read DOC "--ast" "--recursive")) (heads a)))
      '(#t (items versions cut)))
(want "RA-8 an unknown id answers as a plain read does"
      (list (run 'read "nosuch.zz" "--ast") (run 'read "nosuch.zz" "--ast" "--recursive"))
      (list (run 'read "nosuch.zz") (run 'read "nosuch.zz" "--recursive")))
(want "RA-8 --rev on the local route: no-published-state, as --recursive --rev"
      (list (run 'read A "--ast" "--rev") (run 'read DOC "--recursive" "--rev"))
      '((error bad-request (reason no-published-state)) (error bad-request (reason no-published-state))))
(define sealed (seal-state (state) '() (list 7 "tok")))
(define (on-pub . args) (rpc-dispatch main-store args "test" sealed))
(want "RA-8 --rev on a publication appends (rev ...) after the receipt, as --recursive --rev does"
      (let ((a (on-pub 'read A "--ast" "--rev")) (r (on-pub 'read DOC "--recursive" "--rev")))
        (list (car (reverse a)) (equal? (reverse (cdr (reverse a))) (on-pub 'read A "--ast"))
              (car (reverse r)) (equal? (reverse (cdr (reverse r))) (on-pub 'read DOC "--recursive"))))
      (list '(rev 7 (daemon "tok")) #t '(rev 7 (daemon "tok")) #t))
(want "RA-8 the handler's order: --cut --working, --rev --cut, --rev --signature refused before the ast rule"
      (list (run 'read A "--ast" "--cut" "t1" "--working")
            (run 'read A "--ast" "--rev" "--cut" "t1")
            (run 'read A "--ast" "--rev" "--signature"))
      '((error bad-request incompatible-cut-options)
        (error bad-request incompatible-cut-options)
        (error bad-request incompatible-rev-options)))
(want "RA-8 --writer alone is accepted and changes nothing, as for a plain read"
      (let ((a (run 'read S "--ast" "--writer" "someone")))
        (list (equal? a (run 'read S "--ast")) (heads a)))
      '(#t (block cut versions)))

;; ---- RA-9 envelope pins (green on the base) -------------------------------------------------

(define st9 (state))
(define c9 (reduce-applied-cut st9))
(define hA (block-hash st9 A))
(define hS (block-hash st9 S))
(want "RA-9 PIN read <id> answers the record, its version and the receipt, nothing more"
      (run 'read A)
      `(ok ,(state-read st9 A) (version ,hA) (cut ,c9) (versions ((,A . ,hA)))))
(want "RA-9 PIN read <id> --md answers the block's own bytes and the receipt"
      (run 'read S "--md")
      `(ok (text ,(string-append "## Sec\n" src1)) (cut ,c9) (versions ((,S . ,hS)))))
(want "RA-9 PIN read <doc> --recursive answers the records, the versions and the cut"
      (run 'read DOC "--recursive")
      `(ok (items ,@(map (lambda (id) (state-read st9 id)) doc-ids))
           (versions ,(map (lambda (id) (cons id (block-hash st9 id))) (list DOC S S2 S3)))
           (cut ,c9)))

;; ---- RA-10 trace ----------------------------------------------------------------------------

(want "RA-10 one read-ast trace line per --ast request, single or recursive; none for a plain read"
      (list (read-ast-lines (lambda () (run 'read S "--ast")))
            (read-ast-lines (lambda () (run 'read DOC "--ast" "--recursive")))
            (read-ast-lines (lambda () (run 'read S))))
      '(1 1 0))

;; ---- RA-11 validity -------------------------------------------------------------------------

(want "RA-11 a superseded block's --ast read carries the plain read's validity clause, after the block"
      (list (clause (run 'read P "--ast") 'validity) (clause (run 'read P) 'validity)
            (heads (run 'read P "--ast")))
      (list (list 'validity 'superseded (list 'superseded-by N)) (list 'validity 'superseded (list 'superseded-by N))
            '(block validity cut versions)))
(want "RA-11 a valid block's --ast read has no validity clause (the control)"
      (heads (run 'read H5 "--ast"))
      '(block cut versions))
(want "RA-11 the recursive form carries the listed clause"
      (list (clause (run 'read G "--ast" "--recursive") 'validity) (clause (run 'read G "--recursive") 'validity))
      (list (list 'validity (list (list P 'superseded (list 'superseded-by N))))
            (list 'validity (list (list P 'superseded (list 'superseded-by N))))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nread-ast complete\n" bad rows)
(exit (if (= bad 0) 0 1))
