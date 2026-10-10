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

;; `read --recursive`: a document is its sections, not the empty space
;; above the first heading.
;;
;; A file-level block holds almost nothing of its own. Its `src` is the
;; front matter and whatever sits above the first heading, which in most
;; documents is empty -- everything a reader wants is in the sections
;; under it. `read` of such a block therefore answered with an empty body
;; and was, for the one shape people actually have, useless.
;;
;; THE CLAIM THAT MATTERS IS NOT "IT PRINTS SOMETHING". It is that what
;; it prints is the SAME TEXT the exporter writes to the file -- byte for
;; byte, from the same renderer. Two renderers would agree today and
;; drift later, and the drift would surface as a file that round-trips
;; and a `read` that quietly disagrees with it, which is the hardest kind
;; of disagreement to notice. So the rows below compare `read --md
;; --recursive` against the exported file itself rather than against a
;; string written out here.

(import (chezscheme) (theourgia project) (theourgia store) (theourgia reduce)
        (theourgia log) (theourgia ffi) (theourgia md) (theourgia rpc)
        (theourgia wire))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

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
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define root (test-dir "read1"))
(define (put! p text)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 text)))))
(define (slurp p)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
      (if (eof-object? b) "" (utf8->string b)))))

;; diff -r, read as a reading. An empty output means the two trees are
;; the same file for file and byte for byte; anything else is the report.
(define (trees-differ a b)
  (let ((out (string-append root "/diff.txt")))
    (system (string-append "diff -r " a " " b " > " out " 2>&1"))
    (let ((t (slurp out))) (if (string=? t "") 'identical t))))

;; THE FIRST ANSWER'S FIRST n PARTS, OR A SENTINEL FOR EACH MISSING ONE.
;; Indexing straight into the answers is how a row that should print
;; FAIL ends the file with an exception instead: under a mutation that
;; makes the import do nothing, `(car (car a))` is car of the empty
;; list, and a run with no failure count reads like a run nobody made.
(define (parts a n)
  (let loop ((x (if (null? a) '() (car a))) (k n) (out '()))
    (cond ((= k 0) (reverse out))
          ((null? x) (loop '() (- k 1) (cons 'missing out)))
          ((not (pair? x)) (loop '() (- k 1) (cons 'missing out)))
          (else (loop (cdr x) (- k 1) (cons (car x) out))))))

;; IDS ARE RELABELLED BY CREATION ORDER, NEVER SPELLED OUT. The writer
;; name is generated at init, so an expectation carrying a literal
;; prefix says nothing about the rule it claims to check -- it is a coin
;; flip, and one that came up heads would be worse than one that did
;; not. `b1` is the first block this store made, `b2` the second: the
;; order is the property, and it reads.
(define (labeller d)
  (let ((ids (ids-of d)))
    (lambda (x)
      (if (not (string? x))
          x
          (let loop ((xs ids) (n 1))
            (cond ((null? xs) x)
                  ((string=? (car xs) x) (string->symbol (string-append "b" (number->string n))))
                  (else (loop (cdr xs) (+ n 1)))))))))

;; AND THE ROWS ARE SORTED HERE. state-outline orders by the parent's
;; printed form, so whether the `root` row comes first or last depends
;; on how this store's generated writer name happens to sort against
;; the word "root" -- the same expectation passed for one store and
;; failed for the next. Sorting on the labels makes the reading about
;; the tree rather than about the name.
(define (label-rows d rows)
  (let ((f (labeller d)))
    (list-sort (lambda (x y)
                 (string<? (format "~s" x) (format "~s" y)))
               (map (lambda (row) (list (f (car row)) (f (caddr row)))) rows))))

(define (records d) (length (reduce-trace (open-and-reduce d))))
(define (ids-of d) (map cadr (state-datum (open-and-reduce d))))

;; A STORE AND A SOURCE TREE, MADE FRESH. Each case gets its own machine
;; home as well: the registry is per machine, and sharing one across
;; cases makes the second case's first write look like a rollback.
(define case-n 0)
(define (fresh-case! files)
  (set! case-n (+ case-n 1))
  (let* ((dir (string-append root "/c" (number->string case-n)))
         (src (string-append dir "/src"))
         (d (string-append dir "/store")))
    (system (string-append "rm -rf " dir "; mkdir -p " src " " d))
    (putenv "THEOURGIA_HOME" (string-append dir "-home"))
    (store-init! d)
    (for-each (lambda (f)
                (let ((full (string-append src "/" (car f))))
                  (system (string-append "mkdir -p $(dirname " full ")"))
                  (put! full (cdr f))))
              files)
    (cons src d)))


(define ALPHA "# Alpha\nbody of alpha\n\n## Beta\nbody of beta\n\n### Gamma\nbody of gamma\n\n## Delta\nbody of delta\n")
(define t1 (fresh-case! (list (cons "one.md" ALPHA))))
(define src1 (car t1))
(define d1 (cdr t1))
(import-md d1 src1 "tester")

(define (answer . args) (rpc-dispatch d1 (cons 'read args) "tester"))
(define (text-of a)
  (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a))
           (pair? (cadr a)) (eq? (car (cadr a)) 'text))
      (cadr (cadr a))
      (list 'not-text a)))
(define (ids-of-answer a)
  (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a))
           (pair? (cadr a)) (eq? (car (cadr a)) 'items))
      (map (lambda (b) (cdr (assq 'id b))) (cdr (cadr a)))
      (list 'not-items a)))

;; BLOCKS ARE FOUND BY TITLE, NOT BY POSITION IN A LIST. An expectation
;; written as "the second id this store made" says nothing about the
;; tree; it is a guess about an ordering, and a wrong guess reads as a
;; defect in the thing under test. Titles are what the document actually
;; says.
(define (id-titled title)
  (let ((st (open-and-reduce d1)))
    (let loop ((ids (ids-of d1)))
      (cond
        ((null? ids) (assertion-violation 'id-titled "no block with that title" title))
        ((let ((e (assq 'title (cdr (assq 'fields (state-read st (car ids)))))))
           (and e (string=? (cdr e) title)))
         (car ids))
        (else (loop (cdr ids)))))))

(define (id-of-doc)
  (let ((st (open-and-reduce d1)))
    (let loop ((ids (ids-of d1)))
      (cond
        ((null? ids) (assertion-violation 'id-of-doc "no document block" d1))
        ((eq? 'doc (cdr (assq 'kind (cdr (assq 'fields (state-read st (car ids)))))))
         (car ids))
        (else (loop (cdr ids)))))))

(define doc1 (id-of-doc))
(define label1 (labeller d1))

(printf "== the shape that made this necessary ==\n")
;; A document's own bytes are the front matter and whatever sits above
;; the first heading. Here that is nothing at all.
(want "read of a document answers one block whose body is empty"
      (text-of (answer doc1 "--md"))
      "")
;; ONE BLOCK, AND THEN ITS VERSION: the record is the only item of data,
;; and the one clause after it is the version --if-unchanged compares.
(want "and without --md it is still one block"
      (let ((a (answer doc1)))
        (list (length (filter (lambda (x) (not (and (pair? x) (memq (car x) '(version cut versions))))) (cdr a)))
              (and (pair? (cadr a)) (assq 'id (cadr a)) #t)))
      '(1 #t))

(printf "\n== --recursive answers the subtree ==\n")
(want "the document and every section under it, in document order"
      (map label1 (ids-of-answer (answer doc1 "--recursive")))
      '(b1 b2 b3 b4 b5))
;; A LEAF IS ITS OWN SUBTREE, which is what stops the row above from
;; passing for an implementation that answers with every block in the
;; store.
(want "TWIN: a leaf section is one item"
      (map label1 (ids-of-answer (answer (id-titled "Gamma") "--recursive")))
      '(b4))
(want "TWIN: a section with a child under it is two"
      (map label1 (ids-of-answer (answer (id-titled "Beta") "--recursive")))
      '(b3 b4))

(printf "\n== and --md --recursive is the file ==\n")
;; THE EXPORTED FILE IS THE ORACLE, not a string written out here. It is
;; produced by the same renderer, so this row is what holds the two
;; callers together; a second renderer would pass every other row in this
;; file and fail this one.
(define out1 (string-append root "/c1/out"))
(system (string-append "mkdir -p " out1))
(export-md d1 out1)
(want "reading the document recursively gives back the file byte for byte"
      (text-of (answer doc1 "--md" "--recursive"))
      (slurp (string-append out1 "/one.md")))
;; AND IT IS THE SOURCE, TOO -- which says the round trip and the read
;; agree, not merely that the read agrees with whatever the exporter
;; currently does.
(want "which is also the text that was imported"
      (text-of (answer doc1 "--md" "--recursive"))
      ALPHA)

(printf "\n== a section on its own ==\n")
;; A SECTION ASKED FOR ON ITS OWN DOES NOT CARRY THE DOCUMENT'S FRONT
;; MATTER: the front matter belongs to the document.
;; THE TRAILING BLANK LINE BELONGS TO GAMMA. A section's `src` carries
;; the blank line that separated it from whatever came next in the file,
;; so a subtree lifted out of the middle of a document ends with it. That
;; is the block's own bytes and not a stray newline the renderer added:
;; putting it back where it came from reproduces the file exactly, which
;; is what the row above this one measures.
(want "a section and its children, with their headings"
      (text-of (answer (id-titled "Beta") "--md" "--recursive"))
      "## Beta\nbody of beta\n\n### Gamma\nbody of gamma\n\n")
(want "TWIN: the same section without --recursive is its own bytes only"
      (text-of (answer (id-titled "Beta") "--md"))
      "## Beta\nbody of beta\n\n")

(printf "\n== a document is a file, and a file is not inside a file ==\n")
;; The read batch turned this up as a disagreement: read recursively, a
;; document nested under another keeps its front matter; exported, it is
;; folded into its ancestor's file as a section, losing the front matter
;; and taking an empty heading, and it gets no file of its own.
;;
;; BOTH READINGS ARE DEFENSIBLE, WHICH IS THE PROBLEM. A state that means
;; two things is refused at the entrance rather than given two
;; interpretations -- every later rule would otherwise have to choose one
;; of them, and they would not all choose the same. So the rows that used
;; to pin the disagreement now assert that the write never happens.
(define t2 (fresh-case! (list (cons "outer.md" "# Outer\nouter body\n"))))
(define d2 (cdr t2))
(import-md d2 (car t2) "tester")
(define (block-of d pred)
  (let ((st (open-and-reduce d)))
    (let loop ((ids (map cadr (state-datum st))))
      (cond ((null? ids) #f)
            ((pred (cdr (assq 'fields (state-read st (car ids))))) (car ids))
            (else (loop (cdr ids)))))))
(define outer (block-of d2 (lambda (fs) (eq? 'doc (cdr (assq 'kind fs))))))
(define section2 (block-of d2 (lambda (fs) (eq? 'section (cdr (assq 'kind fs))))))
(define (insert-doc! parent)
  (car (with-store-write d2
         (lambda (st v)
           (list (list 'insert parent #f
                       (list (cons 'kind 'doc) (cons 'path "inner.md")
                             (cons 'front "---\nx: y\n---\n") (cons 'src "intro\n")))))
         "tester")))
(want "a document under another document is refused"
      (let ((a (insert-doc! outer))) (list (car a) (cadr a)))
      '(error doc-must-be-top-level))
(want "and under a section too"
      (let ((a (insert-doc! section2))) (list (car a) (cadr a)))
      '(error doc-must-be-top-level))
;; TWIN: at the root it is an ordinary write. Without this the rows above
;; would pass for an implementation that refused every insert of a
;; document anywhere.
(want "TWIN: the same document at the root is written"
      (car (insert-doc! 'root))
      'ok)
;; AND MOVING ONE THERE IS THE SAME REFUSAL, which is not the same code
;; path: a document already at the root can be moved under a section, and
;; that is the other entrance to the state.
(want "moving a document under a section is refused"
      (let* ((inner (block-of d2 (lambda (fs)
                                   (let ((p (assq 'path fs)))
                                     (and p (string=? (cdr p) "inner.md"))))))
             (a (car (with-store-write d2
                       (lambda (st v) (list (list 'move inner section2 #f)))
                       "tester"))))
        (list (car a) (cadr a)))
      '(error doc-must-be-top-level))
;; TWIN: a SECTION may of course be moved under a section.
(want "TWIN: moving a section under a section is not refused"
      (car (car (with-store-write d2
                  (lambda (st v) (list (list 'move section2 'root #f)))
                  "tester")))
      'ok)

;; AND HISTORY THAT ALREADY HOLDS ONE IS REPORTED, NOT REPAIRED. The
;; record exists and says what it says; a reader that silently moved the
;; block would be inventing a history nobody wrote. This one is written
;; straight into the log, past the verb that would refuse it -- which is
;; how it arrives in practice too: from a store written before the rule,
;; or from another store through sync.
;; A FRESH STORE, BECAUSE THE ROWS ABOVE MOVED THINGS. The refusal rows
;; left d2 with its section at the root -- true of that store and
;; nothing to do with this rule, but an expectation written against d2
;; would be reading the previous rows' side effects. Order between rows
;; is a criterion nobody declared.
(define t3 (fresh-case! (list (cons "outer.md" "# Outer\nouter body\n"))))
(define d3 (cdr t3))
(import-md d3 (car t3) "tester")
(define outer3 (block-of d3 (lambda (fs) (eq? 'doc (cdr (assq 'kind fs))))))
(define section3 (block-of d3 (lambda (fs) (eq? 'section (cdr (assq 'kind fs))))))
(define inner
  (let ((a (car (with-store-write d3
                  (lambda (st v)
                    (list (list 'insert 'root #f
                                (list (cons 'kind 'doc) (cons 'path "inner.md")
                                      (cons 'front "---\nx: y\n---\n")
                                      (cons 'src "intro\n")))))
                  "tester"))))
    (car (map car (cadr (assq 'state (cdr a)))))))
(define (append-raw! store writer seq payload)
  (let ((path (string-append store "/writers/" writer "/000001.sexp")))
    (call-with-port (open-file-output-port path (file-options no-fail no-truncate))
      (lambda (o)
        (set-port-position!
          o (bytevector-length (call-with-port (open-file-input-port path)
                                 get-bytevector-all)))
        (put-bytevector o (encode-record seq (+ 1757300000000 seq) "tester" '()
                                         (storable-encode payload)))))))
(want "CONTROL: with the document at the root, nothing is reported"
      (filter (lambda (c) (eq? (car c) 'nested-document)) (store-conflicts d3))
      '())
(want "a record that puts a document under a section is reported"
      (let* ((w (car (list-sort string<? (store-writers d3))))
             (next (+ 1 (length (reduce-trace (open-and-reduce d3))))))
        ;; THE ORD IS A NUMBER. This said `'(0 . 1)`, which no ord ever
        ;; is -- `ord-between` answers an exact rational or a refusal --
        ;; and it went unnoticed because the moved block was the only
        ;; child, so nothing ever compared it. Once the reducer began
        ;; checking position VALUES, the record was refused and this row
        ;; reported nothing, which read as the nested-document rule
        ;; having broken.
        (append-raw! d3 w next (list 'move inner section3 1))
        (filter (lambda (c) (eq? (car c) 'nested-document)) (store-conflicts d3)))
      (list (list 'nested-document inner)))

(printf "\n== the options ==\n")
;; NEITHER OPTION TAKES A VALUE, and both are stripped before the id is
;; looked at -- so the two orders are the same request. An option list
;; whose meaning depends on where a word sits is a component whose
;; meaning depends on its position.
(want "--md --recursive and --recursive --md are the same request"
      (equal? (answer doc1 "--md" "--recursive")
              (answer doc1 "--recursive" "--md"))
      #t)
(want "an unknown id is still an unknown id"
      (let ((a (answer "nosuchblock" "--recursive")))
        (list (car a) (cadr a)))
      '(error unknown-id))
(want "and a word that is not an option is still refused"
      (car (answer doc1 "--deep"))
      'usage)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "read1 complete\n")
