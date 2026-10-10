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

;; H2: a directory and a store, each derivable from the other.
;; A3 (bytes), A3' (a local edit costs one record) and A6 (identity).
(import (chezscheme) (theourgia project) (theourgia store) (theourgia reduce)
        (theourgia log) (theourgia ffi) (theourgia md)
        (only (theourgia code-project) code-safe-path?)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia wire) encode-record storable-encode))

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


(define root (test-dir "md2"))
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

(printf "== A3: a directory goes in and comes back out ==\n")
(define t1
  (fresh-case!
    (list (cons "one.md" "# Alpha\nbody of alpha\n\n## Beta\nbody of beta\n")
          (cons "two.md" "---\ntitle: t\n---\nlead\n# Gamma\ng\n")
          (cons "sub/three.md" "# Delta\nno final newline")
          (cons "four.md" "no headings at all\n"))))
(define src1 (car t1))
(define d1 (cdr t1))
(define import1 (import-md d1 src1 "tester"))
(want "every file imported and nothing was refused"
      (list (for-all (lambda (a) (eq? (car a) 'ok)) import1)
            (length (filter (lambda (b) (eq? 'doc (cdr (assq 'kind (cdr (assq 'fields b))))))
                            (map (lambda (id) (state-read (open-and-reduce d1) id))
                                 (ids-of d1)))))
      (list #t 4))
(define out1 (string-append root "/c1/out"))
(system (string-append "mkdir -p " out1))
(want "exporting gives back the tree byte for byte"
      (begin (export-md d1 out1) (trees-differ src1 out1))
      'identical)

(printf "== A6: importing the same tree again costs nothing ==\n")
(define before-again (records d1))
(define ids-before (ids-of d1))
(want "a second import of an unchanged tree writes no record and keeps every id"
      (let ((answers (import-md d1 src1 "tester")))
        (list (length answers) (- (records d1) before-again) (equal? ids-before (ids-of d1))))
      (list 0 0 #t))

(printf "== A3': one edited section costs one record ==\n")
(define before-edit (records d1))
(put! (string-append src1 "/one.md") "# Alpha\nbody of alpha\n\n## Beta\nEDITED beta\n")
(define edit-answers (import-md d1 src1 "tester"))
(want "only the edited section is written"
      (list (map car edit-answers) (- (records d1) before-edit))
      (list '(ok) 1))
(want "and the export matches the edited source, the other files untouched"
      (begin (system (string-append "rm -rf " out1 "; mkdir -p " out1))
             (export-md d1 out1)
             (trees-differ src1 out1))
      'identical)

(printf "== the recovery form carries identity and leaves no trace ==\n")
(define rec1 (string-append root "/c1/rec"))
(system (string-append "mkdir -p " rec1))
(export-md d1 rec1 #t)
(want "the exported file names each section before its heading"
      (let ((text (slurp (string-append rec1 "/one.md"))))
        (list (> (string-length text) 0)
              (let count ((i 0) (n 0))
                (cond ((> (+ i 16) (string-length text)) n)
                      ((string=? (substring text i (+ i 16)) "<!-- theourgia: ")
                       (count (+ i 16) (+ n 1)))
                      (else (count (+ i 1) n))))))
      (list #t 2))
;; A FRESH STORE FED THE RECOVERY FORM AND EXPORTED PLAIN gives the
;; ORIGINAL bytes: the markers are projection syntax and must not end up
;; in anybody's content.
(define t2 (fresh-case! '()))
(define d2 (cdr t2))
(define rec-answers (import-md d2 rec1 "tester"))
(define out2 (string-append root "/c2/out"))
(system (string-append "mkdir -p " out2))
(want "a store built from the recovery form exports the plain files"
      (begin (export-md d2 out2)
             (list (for-all (lambda (a) (eq? (car a) 'ok)) rec-answers)
                   (trees-differ src1 out2)))
      (list #t 'identical))

(printf "== A6: sections that look alike ==\n")
;; (i) SAME TITLE, DIFFERENT BODIES. The signature tells them apart, so
;; this must succeed and the ids must not move.
(define t3 (fresh-case! (list (cons "f.md" "# T\nalpha\n# T\nbeta\n"))))
(define ids3 (begin (import-md (cdr t3) (car t3) "t") (ids-of (cdr t3))))
(define before3 (records (cdr t3)))
(want "(i) two sections with one title, one of them edited"
      (begin (put! (string-append (car t3) "/f.md") "# T\nalpha EDITED\n# T\nbeta\n")
             (let ((a (import-md (cdr t3) (car t3) "t")))
               (list (map car a) (- (records (cdr t3)) before3) (equal? ids3 (ids-of (cdr t3))))))
      (list '(ok) 1 #t))
;; (ii) TWO IDENTICAL SECTIONS SWAPPED. The file does not change, so
;; there is nothing to observe and nothing to write.
(define t4 (fresh-case! (list (cons "f.md" "# T\nsame\n# T\nsame\n"))))
(define ids4 (begin (import-md (cdr t4) (car t4) "t") (ids-of (cdr t4))))
(define before4 (records (cdr t4)))
(want "(ii) swapping two identical sections is not observable"
      (begin (put! (string-append (car t4) "/f.md") "# T\nsame\n# T\nsame\n")
             (let ((a (import-md (cdr t4) (car t4) "t")))
               (list (length a) (- (records (cdr t4)) before4) (equal? ids4 (ids-of (cdr t4))))))
      (list 0 0 #t))
;; (iii) A THIRD IDENTICAL SECTION. Now the file HAS changed and nothing
;; in it says which stored section each one is. It must refuse, name the
;; candidates, and ask for a marker.
(define t5 (fresh-case! (list (cons "f.md" "# T\nsame\n# T\nsame\n"))))
(define ids5 (begin (import-md (cdr t5) (car t5) "t") (ids-of (cdr t5))))
(define before5 (records (cdr t5)))
(want "(iii) a third identical section is refused as ambiguous, naming both candidates"
      (begin (put! (string-append (car t5) "/f.md") "# T\nsame\n# T\nsame\n# T\nsame\n")
             (let ((a (import-md (cdr t5) (car t5) "t")))
               (append (parts a 4) (list (- (records (cdr t5)) before5)))))
      (list 'error 'ambiguous-identity
            (list 'candidates (list (cadr ids5) (caddr ids5)))
            '(remedy marker)
            0))
;; AND A MARKER NAMING SOMETHING THIS DOCUMENT DOES NOT HAVE IS A
;; DIFFERENT REFUSAL. Both are "we will not guess", and an operator
;; needs to know which: one is fixed by adding a marker, the other by
;; correcting one.
(define t6 (fresh-case! (list (cons "f.md" "# A\nbody\n"))))
(define before6 (begin (import-md (cdr t6) (car t6) "t") (records (cdr t6))))
(want "a marker naming a block this document does not have is a position mismatch"
      (begin (put! (string-append (car t6) "/f.md")
                   "<!-- theourgia: nosuch.9 -->\n# A\nbody\n")
             (let ((a (import-md (cdr t6) (car t6) "t")))
               (append (parts a 3) (list (- (records (cdr t6)) before6)))))
      (list 'error 'position-mismatch '(declared ("nosuch.9")) 0))
;; A SECTION THE FILE GREW IS KEPT. Dropping it read as "nothing to do",
;; and the next export wrote the file back without the new text.
(define t7 (fresh-case! (list (cons "f.md" "# A\nbody\n"))))
(define before7 (begin (import-md (cdr t7) (car t7) "t") (records (cdr t7))))
(want "a genuinely new section is added rather than dropped"
      (begin (put! (string-append (car t7) "/f.md") "# A\nbody\n# B\nother\n")
             (let ((a (import-md (cdr t7) (car t7) "t")))
               (list (map car a) (- (records (cdr t7)) before7)
                     (length (ids-of (cdr t7))))))
      (list '(ok) 1 3))

(printf "== a new section goes where its heading says ==\n")
;; SECTION 2.1 REQUIRES THE PARENT GRAPH TO BE RE-DERIVABLE FROM THE
;; HEADING LEVELS. Hanging every new section off the document broke
;; that: `# A` then a new `## B` came back with B as A's SIBLING, so
;; re-parsing the exported file gave a different tree from the one that
;; was imported.
;; THE PARENT MAY NOT EXIST YET when the intent is built, which is what
;; the batch back-reference is for.
(define (grow! initial then)
  (let* ((t (fresh-case! (list (cons "f.md" initial))))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (put! (string-append src "/f.md") then)
    (let ((answers (import-md d src "t"))
          (out (string-append src "/../out")))
      (system (string-append "rm -rf " out "; mkdir -p " out))
      (export-md d out)
      (list (map car answers)
            (state-outline (open-and-reduce d))
            (string=? (slurp (string-append out "/f.md")) then)
            d))))
(want "a child heading becomes a child, and the export is the source"
      (let* ((r (grow! "# A\nbody\n" "# A\nbody\n## B\nchild\n"))
             (d (cadddr r)))
        (list (car r) (label-rows d (cadr r)) (caddr r)))
      (list '(ok) '((b1 b2) (b2 b3) (root b1)) #t))
(want "a sibling inserted between two others lands between them"
      (let* ((r (grow! "# A\na\n# C\nc\n" "# A\na\n# B\nb\n# C\nc\n"))
             (d (cadddr r))
             (f (labeller d))
             (doc (car (ids-of d))))
        (list (car r)
              (map (lambda (row) (f (caddr row)))
                   (list-sort (lambda (x y) (< (cadr x) (cadr y)))
                              (filter (lambda (row) (equal? (car row) doc)) (cadr r))))
              (caddr r)))
      (list '(ok) '(b2 b4 b3) #t))
(want "a grandchild becomes a grandchild"
      (let* ((r (grow! "# A\na\n## B\nb\n" "# A\na\n## B\nb\n### C\nc\n"))
             (d (cadddr r)))
        (list (car r) (label-rows d (cadr r)) (caddr r)))
      (list '(ok) '((b1 b2) (b2 b3) (b3 b4) (root b1)) #t))

(printf "== section 2.4: a stored heading line stops being valid ==\n")
;; `set title` LANDED AND EXPORT IGNORED IT. The stored bytes were
;; replayed, so the file came back with the OLD heading -- and importing
;; that file wrote a second record putting the title back. The change
;; could not be made to stick, and the store and the tree disagreed
;; forever without either of them looking wrong on its own.
(define t8 (fresh-case! (list (cons "f.md" "# One\nlead\n\n## Two\nbody two\n"))))
(define d8 (cdr t8))
(define src8 (car t8))
(import-md d8 src8 "t")
(define sec8 (caddr (ids-of d8)))
(define out8 (string-append root "/c" (number->string case-n) "/out"))
(want "CONTROL: the section is the one with the second heading"
      (let* ((b (state-read (open-and-reduce d8) sec8))
             (fs (cdr (assq 'fields b))))
        (list (cdr (assq 'title fs)) (cdr (assq 'level fs))))
      (list "Two" 2))
(want "after set title the export carries the new heading and nothing else moves"
      (begin
        (with-store-write d8 (lambda (st v) (list (list 'set sec8 'title "Two Renamed"))) "t")
        (system (string-append "mkdir -p " out8))
        (export-md d8 out8)
        (slurp (string-append out8 "/f.md")))
      "# One\nlead\n\n## Two Renamed\nbody two\n")
;; AND RE-IMPORTING THAT EXPORT COSTS NOTHING. Comparing the file
;; against the stale stored bytes said "the heading changed" and wrote a
;; record for a file nobody had touched.
(want "re-importing the export writes no record"
      (begin
        (system (string-append "cp " out8 "/f.md " src8 "/f.md"))
        (let ((before (records d8)))
          (let ((a (import-md d8 src8 "t")))
            (list (length a) (- (records d8) before)))))
      (list 0 0))
;; THE SAME RULE FOR LEVEL. A heading line is only good while BOTH the
;; level and the title it parses to still match the block.
(want "after set level the export renumbers the hashes"
      (begin
        (with-store-write d8 (lambda (st v) (list (list 'set sec8 'level 3))) "t")
        (system (string-append "rm -rf " out8 "; mkdir -p " out8))
        (export-md d8 out8)
        (slurp (string-append out8 "/f.md")))
      "# One\nlead\n\n### Two Renamed\nbody two\n")
(want "and re-importing that costs nothing either, with the tree unchanged"
      (begin
        (system (string-append "cp " out8 "/f.md " src8 "/f.md"))
        (let ((before (records d8)))
          (let ((a (import-md d8 src8 "t")))
            (list (length a) (- (records d8) before)
                  (label-rows d8 (state-outline (open-and-reduce d8)))))))
      (list 0 0 '((b1 b2) (b2 b3) (root b1))))

(printf "== what is missing from the tree is not deleted on a guess ==\n")
;; A TOMBSTONE IS PERMANENT AND ABSENCE IS AMBIGUOUS: the file may have
;; been deleted, or the directory may be a partial copy, or a sync may
;; have been interrupted. So absence is reported, and acted on only when
;; the caller says to.
(define (gone! setup-files remove)
  (let* ((t (fresh-case! setup-files))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (remove src)
    (let* ((refused (import-md d src "t"))
           (before (records d))
           (allowed (import-md d src "t" #t)))
      (let ((f (labeller d)))
        (list (let ((p (parts refused 4)))
                (if (and (pair? (caddr p)) (eq? (car (caddr p)) 'blocks))
                    (list (car p) (cadr p) (list 'blocks (map f (cadr (caddr p)))) (cadddr p))
                    p))
              (- (records d) before)
              (map (lambda (b) (f (cadr b)))
                   (filter (lambda (b) (not (cadr (assq 'deleted (cddr b)))))
                           (state-datum (open-and-reduce d)))))))))
;; A FILE THAT DISAPPEARED IS REPORTED, NOT REFUSED: the import of the
;; others goes on, the document is left, and the report names it; it is
;; deleted, and reported deleted, only when that is allowed.
(define (gone-file! setup-files remove)
  (let* ((t (fresh-case! setup-files))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (remove src)
    (let* ((before (records d))
           (reported (import-md-report d src "t"))
           (after-report (records d))
           (allowed (import-md-report d src "t" #t)))
      (let ((f (labeller d)))
        (list (cadr reported) (- after-report before) (cadr allowed)
              (map (lambda (b) (f (cadr b)))
                   (filter (lambda (b) (not (cadr (assq 'deleted (cddr b)))))
                           (state-datum (open-and-reduce d)))))))))
(want "a file that disappeared is reported absent and left, writing nothing; allowed, it is deleted and reported deleted"
      (gone-file! (list (cons "a.md" "# A\nbody a\n## A2\nsub\n")
                        (cons "b.md" "# B\nbody b\n"))
                  (lambda (src) (system (string-append "rm " src "/b.md"))))
      (list '(absent (files ("b.md")))
            0
            '(deleted (files ("b.md")))
            '(b1 b2 b3)))
(want "a section that disappeared from a file still there is refused would-delete with the remedy, not tombstoned"
      (gone! (list (cons "a.md" "# A\nbody a\n## A2\nsub\n"))
             (lambda (src) (put! (string-append src "/a.md") "# A\nbody a\n")))
      (list (list 'error 'would-delete '(blocks (b3)) '(remedy allow-delete))
            1
            '(b1 b2)))

(printf "== deleting one section costs exactly one block ==\n")
;; DELETING A SECTION CHANGES ITS NEIGHBOUR'S BYTES. A section's body
;; runs to the start of the next heading, so removing the last section
;; hands the blank line that separated them back to the one before it.
;; Matching on the whole (heading, body) signature therefore could not
;; tell an EDITED section from a DELETED one: the neighbour matched
;; nothing, was reported as about to be tombstoned, and with
;; --allow-delete was tombstoned and rebuilt under a NEW id. One
;; deletion cost two blocks and moved an id nobody had touched.
;; THE BLANK LINES ARE WHAT MAKE THIS BITE. Without them the neighbour's
;; bytes happen not to change and every one of these rows is green for
;; the broken matcher -- which is why the corpus has them.
(define three-sections "# C\nlead\n\n## S1\none\n\n## S2\ntwo\n\n## S3\nthree\n")
(define (delete-one! name then)
  (let* ((t (fresh-case! (list (cons "f.md" three-sections))))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (let ((before (ids-of d))
          (f (labeller d)))
      (put! (string-append src "/f.md") then)
      (let* ((refused (import-md d src "t"))
             (named (if (and (pair? refused) (eq? (car (car refused)) 'error))
                        (map f (cadr (assq 'blocks (cddr (car refused)))))
                        refused)))
        (import-md d src "t" #t)
        (let ((live (map cadr (filter (lambda (b) (not (cadr (assq 'deleted (cddr b)))))
                                      (state-datum (open-and-reduce d))))))
          (list named
                (map f live)
                ;; every surviving block kept the id it had
                (equal? live (filter (lambda (id) (member id live)) before))))))))
(want "deleting the last section names and tombstones only it"
      (delete-one! "last" "# C\nlead\n\n## S1\none\n\n## S2\ntwo\n")
      (list '(b5) '(b1 b2 b3 b4) #t))
(want "deleting the middle section names and tombstones only it"
      (delete-one! "middle" "# C\nlead\n\n## S1\none\n\n## S3\nthree\n")
      (list '(b4) '(b1 b2 b3 b5) #t))
(want "deleting the first section names and tombstones only it"
      (delete-one! "first" "# C\nlead\n\n## S2\ntwo\n\n## S3\nthree\n")
      (list '(b3) '(b1 b2 b4 b5) #t))
;; AND THE NEIGHBOUR'S CHANGED BODY IS STILL WRITTEN. It is an edit, and
;; treating it as one is the whole point -- but it is a real edit and
;; must reach the log.
(want "the neighbour whose bytes moved is updated, not recreated"
      (let* ((t (fresh-case! (list (cons "f.md" three-sections))))
             (src (car t)) (d (cdr t)))
        (import-md d src "t")
        (let ((s2 (list-ref (ids-of d) 3)))
          (put! (string-append src "/f.md") "# C\nlead\n\n## S1\none\n\n## S2\ntwo\n")
          (import-md d src "t" #t)
          (let ((b (state-read (open-and-reduce d) s2)))
            (list (cdr (assq 'title (cdr (assq 'fields b))))
                  (cdr (assq 'src (cdr (assq 'fields b))))))))
      (list "S2" "two\n"))

(printf "== F42: a block the projection cannot place is named, not dropped ==\n")
;; A KIND THAT IS NOT A SYMBOL CANNOT BE COMPARED WITH ONE. Every place that
;; asks what a block is asks with `eq?`, so a kind left as the string
;; "decision" -- which is what the command line used to store -- matches
;; nothing and the block quietly stops being whatever it says it is. The
;; write routes refuse such a value now; a store written before they did
;; still holds them, and this is what export does when it meets one.
;;
;; THE RECORD IS APPENDED TO THE LOG DIRECTLY, on purpose: going through a
;; write route is exactly what is now impossible, and a fixture that could
;; only build this state through a route would have to be deleted the day
;; the route was fixed -- taking the coverage of old stores with it.
(define (text-field-of store id name)
  (let* ((b (state-read (open-and-reduce store) id))
         (fs (and b (assq 'fields b)))
         (e (and fs (assq name (cdr fs)))))
    (and e (cdr e))))

(define (kind-of-block store id)
  (let* ((b (state-read (open-and-reduce store) id))
         (fs (and b (assq 'fields b)))
         (e (and fs (assq 'kind (cdr fs)))))
    (and e (cdr e))))

;; NEVER: THE BLOCK IS CHOSEN BY WHAT IT IS, NOT BY WHERE IT CAME IN THE LIST.
;; `state-outline` hands back the rows of the tree, and the ORDER OF THAT LIST
;; IS NOT PART OF WHAT IT PROMISES: measured over six fresh stores holding the
;; same one file, the doc row came first three times and the section row first
;; the other three -- the writer name is generated, and the row order follows
;; it. Each consumer that needs an order makes one (the `outline` verb walks
;; parent to child, and its output was byte-identical across those runs). An
;; earlier version of the two rows below took `(car …)` and so tested a
;; different block on different days; it went red once in five runs, which is
;; the worst rate a wrong row can have.
(define (block-of-kind store want-kind)
  (let loop ((ids (map caddr (state-outline (open-and-reduce store)))))
    (cond ((null? ids) (list 'NO-BLOCK-OF-KIND want-kind))
          ((eq? want-kind (kind-of-block store (car ids))) (car ids))
          (else (loop (cdr ids))))))

(define (set-kind! store id value)
  (let* ((sess (log-begin store (lambda args 'applied)))
         (v (session-view sess)))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "independent-fixture" '()
                                      (list 'set id 'kind value)))
    (session-commit! sess)
    (log-end! sess)))

(define t9 (fresh-case! (list (cons "nine.md" "# Nine\nbody of nine\n\n## Deeper\nunder the heading\n"))))
(define d9 (cdr t9))
(import-md d9 (car t9) "tester")
(define out9a (string-append root "/c" (number->string case-n) "/out-a"))
(define out9b (string-append root "/c" (number->string case-n) "/out-b"))
(system (string-append "mkdir -p " out9a " " out9b))

(want "F42 CONTROL: with every kind a symbol, export says only how many files it wrote"
      (export-md d9 out9a)
      '(ok (files 1)))

(define section9 (block-of-kind d9 'section))
(define doc9 (block-of-kind d9 'doc))

;; NEVER: AND THE FIXTURE SAYS IT FOUND THEM. `block-of-kind` answers a list
;; when it finds nothing, which would otherwise travel into the rows below as
;; a block id that matches no block -- and a skip list that names nothing is
;; exactly what those rows would then be measuring.
(want "F42 the store really does hold one block of each kind the rows below break"
      (list (string? section9) (string? doc9) (equal? section9 doc9))
      (list #t #t #f))

(set-kind! d9 section9 "section")

(want "F42 the string-form kind really is in the store, and really is a string"
      (let ((k (kind-of-block d9 section9)))
        (list k (symbol? k)))
      (list "section" #f))


(define (holds? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; NEVER: A SECTION WHOSE KIND CANNOT BE READ IS STILL WRITTEN, AND SO IS NOT
;; REPORTED. `md-tree` uses the kind to pick the blocks that become FILES;
;; everything under one is rendered by structure, kind or no kind. The first
;; version of this row asserted the opposite -- it expected the section in the
;; skip list -- and passed, because the code agreed with it. What neither of
;; them had done was open the file: the section's heading and body were in it
;; the whole time. THE ROW NOW READS THE FILE.
(want "F42 a section with an unreadable kind is written into its file, and is not reported"
      (list (export-md d9 out9b)
            (let ((text (slurp (string-append out9b "/nine.md"))))
              (list (and (holds? text "# Nine") 'the-doc-is-there)
                    (and (holds? text "body of nine") 'with-its-body)
                    (and (holds? text "## Deeper") 'and-the-section-whose-kind-is-broken)
                    (and (holds? text "under the heading") 'with-its-body-too))))
      (list '(ok (files 1))
            '(the-doc-is-there with-its-body
              and-the-section-whose-kind-is-broken with-its-body-too)))


;; NEVER: THE ID IS DERIVED THE WAY THE PRODUCT DERIVES IT. Spelling
;; `<writer>.<seq>` here would be this fixture's own copy of the derivation
;; rule, green on the day it was written and silent afterwards.
(define (id-written-by answer)
  (let* ((ok (car answer))
         (ev (car (cadr (assq 'events (cdr ok))))))
    (block-id (car ev) (cdr ev))))

;; NEVER: WHEN IT IS THE DOC'S KIND, THE WHOLE SUBTREE GOES, AND THE ANSWER
;; SAYS HOW MUCH. Nothing is written at all -- `(files 0)` with an `ok` head
;; is what a caller reading only the head would take for success -- and the
;; blocks under it are lost with it. The entry names the cause once and
;; carries its reach, so the caller can add up what it lost without being
;; handed a line per block.
(define t10 (fresh-case! (list (cons "ten.md" "# Ten\nbody\n\n## A\na\n\n## B\nb\n"))))
(define d10 (cdr t10))
(import-md d10 (car t10) "tester")
(define out10 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out10))
(define doc10 (block-of-kind d10 'doc))
(define under10 (length (filter (lambda (id) (not (equal? id doc10)))
                                (map caddr (state-outline (open-and-reduce d10))))))
(set-kind! d10 doc10 "doc")

(want "F42 a doc that cannot be placed takes its whole subtree, and the entry carries the reach"
      (list (export-md d10 out10)
            (length (directory-list out10))
            under10)
      (list (list 'ok (list 'files 0)
                  (list 'skipped (list doc10 'kind-not-a-symbol (list 'subtree under10))))
            0
            3))

;; NEVER: AND AN ENTRY IS NEVER INSIDE ANOTHER ENTRY. A block with no kind
;; sitting under a doc whose kind is broken is unwritten for the doc's
;; reason, not for its own; listing both would count it twice for a caller
;; adding `1 + n` over the entries, which is the one arithmetic this shape
;; exists to support.
(define t13 (fresh-case! (list (cons "thirteen.md" "# Thirteen\nbody\n"))))
(define d13 (cdr t13))
(import-md d13 (car t13) "tester")
(define out13 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out13))
(define doc13 (block-of-kind d13 'doc))
(define kindless13
  (id-written-by
    (with-store-write d13
      (lambda (s v) (list (list 'insert doc13 #f (list (cons 'title "kindless child")))))
      "tester")))
(set-kind! d13 doc13 "doc")

(want "F42 a block inside a named block is counted in its reach, not listed on its own"
      (let* ((answer (export-md d13 out13))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (length sk) (car (car sk)) (equal? (car (car sk)) doc13)
              (cadr (assq 'subtree (cddr (car sk))))
              (assoc kindless13 sk)))
      (list 1 doc13 #t 2 #f))

(define t11 (fresh-case! (list (cons "eleven.md" "# Eleven\nbody of eleven\n"))))
(define d11 (cdr t11))
(import-md d11 (car t11) "tester")
(define out11 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out11))
(define kindless11
  (id-written-by
    (with-store-write d11
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'title "kindless")))))
      "tester")))

;; NEVER: AND THE ROUTE THAT ACCEPTED IT STILL REFUSES AN UNKNOWN KIND. These
;; two rows are the same call with one field changed, and together they say
;; what the write path's rule actually is: a kind it does not know is refused,
;; NO kind is accepted. Without the second one, "the store can hold a block
;; with no kind" reads like the check is simply absent on this route -- and
;; this route is a third one, `insert`, whose payload only becomes a `put`
;; inside `resolve`, after the caller's intent has been left behind.
(want "F42 the same write route refuses a kind the table does not have"
      (let ((answer (with-store-write d11
                      (lambda (s v) (list (list 'insert 'root #f
                                                (list (cons 'kind 'nonsense)
                                                      (cons 'title "refused")))))
                      "tester")))
        (list (car (car answer)) (cadr (car answer))
              (car (caddr (car answer)))))
      (list 'error 'malformed-intent 'kind-not-known))

(want "F42 the kindless block really was accepted by the write path"
      (list (kind-of-block d11 kindless11)
            (if (state-read (open-and-reduce d11) kindless11) 'present 'MISSING))
      (list #f 'present))

(want "F42 a block with no kind is named with its own reason, and its reach is zero"
      (export-md d11 out11)
      (list 'ok (list 'files 1)
            (list 'skipped (list kindless11 'kind-absent (list 'subtree 0)))))




;; NEVER: A KIND THAT IS PRESENT AND #f IS NOT A MISSING KIND. `field` answers
;; #f for both, so a block carrying `(kind . #f)` was reported as having no
;; kind at all -- the field is right there, and its value is simply not a
;; symbol. Only an older store can hold one now (the write path refuses #f),
;; which is exactly why the reason has to be right: it is the only thing the
;; reader has.
(define t14 (fresh-case! (list (cons "fourteen.md" "# Fourteen\nbody\n"))))
(define d14 (cdr t14))
(import-md d14 (car t14) "tester")
(define out14 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out14))
(define doc14 (block-of-kind d14 'doc))
(set-kind! d14 doc14 #f)

(want "F42 a kind that is present and #f is reported as not a symbol, not as absent"
      (list (kind-of-block d14 doc14) (export-md d14 out14))
      (list #f (list 'ok (list 'files 0)
                     (list 'skipped (list doc14 'kind-not-a-symbol (list 'subtree 1))))))


;; NEVER: A DELETED PARENT IS NOT A NAMED PARENT. `state-read` still answers
;; for a deleted block while the outline stops listing it, so an ancestor can
;; have a reason and never appear in the answer. Measured before this was
;; fixed: delete a doc that has a live section under it and the section --
;; which now belongs to no document -- was suppressed in favour of an entry
;; that was never written. `(ok (files 0))`, nothing on disk, two live blocks
;; reported by nobody. The suppression rule now asks whether the ancestor is
;; LISTED, not whether it has a reason.
(define t17 (fresh-case! (list (cons "seventeen.md" "# Top\ntop body\n\n## Under\nunder body\n"))))
(define d17 (cdr t17))
(import-md d17 (car t17) "tester")
(define out17 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out17))
(define doc17 (block-of-kind d17 'doc))
(define section17 (block-of-kind d17 'section))
(define del17 (car (with-store-write d17 (lambda (s v) (list (list 'del doc17))) "tester")))

(want "F42 a deleted document leaves its live blocks named, not hidden behind an entry nobody will read"
      (let* ((answer (export-md d17 out17))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (car del17)
              (and (state-read (open-and-reduce d17) doc17) 'the-deleted-doc-still-reads-back)
              (length sk)
              (car (car sk))
              (cadr (car sk))
              (cadr (assq 'subtree (cddr (car sk))))
              (length (directory-list out17))))
      (list 'ok 'the-deleted-doc-still-reads-back
            1 section17 'not-in-any-document 1 0))


;; THE WRITE ROUTE REFUSES A DOCUMENT BELOW ANOTHER ONE (`doc-must-be-top-level`).
;; A nested document can still arrive from history written before that rule or
;; from another store; what the export does with one is pinned by
;; one-subtree.sc (F85-3): it is written into its ancestor document's file.
(define t19 (fresh-case! (list (cons "nineteen.md" "# Outer\nouter body\n"))))
(define d19 (cdr t19))
(import-md d19 (car t19) "tester")
(define outer19 (block-of-kind d19 'doc))

(want "F42 the write route refuses a document below another document"
      (let ((answer (with-store-write d19
                      (lambda (s v) (list (list 'insert outer19 #f
                                                (list (cons 'kind 'doc)
                                                      (cons 'path "inner.md")
                                                      (cons 'title "Inner")))))
                      "tester")))
        (list (car (car answer)) (cadr (car answer))))
      (list 'error 'doc-must-be-top-level))


;; NEVER: TWO DOCUMENTS WITH ONE PATH ARE NOT TWO FILES. Both used to be
;; written to it, the second replacing the first, and the answer said
;; `(files 2)` while one document's text existed nowhere -- a count that was
;; right about the writes and wrong about the result. The first by ID wins,
;; which is an order that does not depend on which generated writer name
;; sorted first, and the loser is reported like anything else this projection
;; should have written and did not: with the winner named, so the reader can
;; see WHY rather than only that.
;;
;; The discriminating part is the FILE: it must hold the winner's text. An
;; answer that named the right loser while the losing text sat on disk would
;; have the story exactly backwards.
(define t20 (fresh-case! (list (cons "twenty.md" "# First\nfirst body\n"))))
(define d20 (cdr t20))
(import-md d20 (car t20) "tester")
(define out20 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out20))
(define first20 (block-of-kind d20 'doc))
;; NEVER: THE PATH IS READ BEFORE THE WRITE CALL, NOT INSIDE IT. Reading the
;; store from within a `with-store-write` callback re-opens a store whose lock
;; that same process is holding, and the fixture stops forever with no
;; diagnostic -- measured, the first version of this row hung md2 for the
;; full fifteen minutes of its timeout, and the reading looked exactly like a
;; product defect until the same case ran with the read moved out.
(define path20 (text-field-of d20 first20 'path))
(define second20
  (id-written-by
    (with-store-write d20
      (lambda (s v) (list (list 'insert 'root #f
                                (list (cons 'kind 'doc)
                                      (cons 'path path20)
                                      (cons 'title "Second")))))
      "tester")))

(want "F42 two documents claiming one path: both exist, and they do claim the same path"
      (list (equal? (text-field-of d20 first20 'path) (text-field-of d20 second20 'path))
            (text-field-of d20 first20 'path)
            (string<? first20 second20))
      (list #t "twenty.md" #t))

(want "F42 the first by id is written, the other is named with the winner, and the file is the winner's"
      (let* ((answer (export-md d20 out20))
             (sk (cdr (assq 'skipped (cddr answer))))
             (text (slurp (string-append out20 "/twenty.md"))))
        (list (cadr answer)
              (length sk)
              (car (car sk))
              (cadr (car sk))
              (cadr (assq 'with (cddr (car sk))))
              (and (holds? text "first body") 'the-winners-body-is-on-disk)
              (if (holds? text "Second") 'THE-LOSER-OVERWROTE-IT 'and-the-losers-is-not)))
      (list '(files 1) 1 second20 'path-conflict first20
            'the-winners-body-is-on-disk 'and-the-losers-is-not))


;; NEVER: A SECTION IS NOT A DOCUMENT, AND CANNOT LOSE A DOCUMENT'S PATH.
;; `text-field` answers "" for a block with no path, which is most of them, so
;; asking "did your path lose?" of every block would make one document with a
;; missing path the winner of "" and every unwritten section its loser -- a
;; reason wrong about both blocks it names. Here the section carries a real
;; path, the document's own, and it is still not a path conflict: it is simply
;; in no document.
(define t21 (fresh-case! (list (cons "twentyone.md" "# Real\nbody\n"))))
(define d21 (cdr t21))
(import-md d21 (car t21) "tester")
(define out21 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out21))
(define doc21 (block-of-kind d21 'doc))
(define path21 (text-field-of d21 doc21 'path))
(define impostor21
  (id-written-by
    (with-store-write d21
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'section)
                                                       (cons 'title "Impostor")
                                                       (cons 'path path21)))))
      "tester")))

(want "F42 a section carrying a document's path is in no document, not in a path conflict"
      (let* ((answer (export-md d21 out21))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (text-field-of d21 impostor21 'path)
              (cadr answer)
              (length sk) (car (car sk)) (cadr (car sk))))
      (list path21 '(files 1) 1 impostor21 'not-in-any-document))

;; NEVER: WHICH DOCUMENT SURVIVES MUST NOT DEPEND ON THE ROW ORDER. `md-tree`
;; hands its documents back in the order the outline produced them. For two
;; documents at root that order is their `(ord, id)` -- and a `move` changes
;; the ord without touching the id, so the two orders can be made to disagree
;; and then the rule that picks the winner is visible.
;;
;; An earlier version of this row asked the question of eight freshly built
;; stores and hoped the generated writer names would make the orders differ.
;; They never did, and the row SAID SO rather than passing: root documents are
;; siblings, and siblings are sorted. A row that cannot tell the two rules
;; apart should fail, not read as evidence.
(define t22 (fresh-case! (list (cons "pair.md" "# Pair\nbody of the first\n"))))
(define d22 (cdr t22))
(import-md d22 (car t22) "tester")
(define out22 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out22))
(define first22 (block-of-kind d22 'doc))
(define path22 (text-field-of d22 first22 'path))
(define second22
  (id-written-by
    (with-store-write d22
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path path22)
                                                       (cons 'title "Second")))))
      "tester")))
(define moved22
  (car (with-store-write d22
         (lambda (s v) (list (list 'move first22 'root second22)))
         "tester")))

(want "F42 the two orders really do disagree in this store"
      (list (car moved22)
            (equal? (list-sort string<? (list first22 second22)) (list first22 second22))
            (equal? (map car (md-tree (open-and-reduce d22))) (list second22 first22)))
      (list 'ok #t #t))

(want "F42 and the surviving document is the first by id, not the first the rows mention"
      (let* ((answer (export-md d22 out22))
             (sk (cdr (assq 'skipped (cddr answer))))
             (text (slurp (string-append out22 "/" path22))))
        (list (cadr answer)
              (car (car sk))
              (cadr (car sk))
              (cadr (assq 'with (cddr (car sk))))
              (and (holds? text "body of the first") 'the-first-by-id-is-on-disk)))
      (list '(files 1) second22 'path-conflict first22 'the-first-by-id-is-on-disk))


;; A NESTED DOCUMENT WRITTEN THROUGH THE LOG, WITH ITS OWN SECTION, SHARING ITS
;; ANCESTOR'S PATH. The row below reads that the store holds it where it was
;; written; the export writes it into the ancestor's file (one-subtree.sc,
;; F85-3), so the path it carries names no second file.
(define t23 (fresh-case! (list (cons "twentythree.md" "# Outer\nouter body\n"))))
(define d23 (cdr t23))
(import-md d23 (car t23) "tester")
(define outer23 (block-of-kind d23 'doc))
(define path23 (text-field-of d23 outer23 'path))
(define inner23
  (let* ((sess (log-begin d23 (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "independent-fixture" '()
                                      (list 'put (list (cons 'kind 'doc)
                                                       (cons 'path path23)
                                                       (cons 'title "Inner")
                                                       (cons 'parent outer23) (cons 'ord 0)))))
    (session-commit! sess)
    (log-end! sess)
    id))
(define under23
  (let* ((sess (log-begin d23 (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "independent-fixture" '()
                                      (list 'put (list (cons 'kind 'section)
                                                       (cons 'title "Under the inner one")
                                                       (cons 'parent inner23) (cons 'ord 0)))))
    (session-commit! sess)
    (log-end! sess)
    id))

(want "F42 the nested document and its section are really there, under the outer one"
      (let* ((rows (state-outline (open-and-reduce d23)))
             (parent-of (lambda (id)
                          (let ((row (assoc id (map (lambda (r) (list (caddr r) (car r))) rows))))
                            (and row (cadr row))))))
        (list (equal? (text-field-of d23 inner23 'path) path23)
              (parent-of inner23)
              (parent-of under23)))
      (list #t outer23 inner23))

;; NEVER: AND THE REACH COUNTS THE SAME BLOCKS THE REASONS DO. A `code` child
;; belongs to another projection: it is not this verb's to lose, it gets no
;; reason of its own, and counting it in somebody else's reach made `1 + n`
;; claim two blocks were lost where one was.
(define t24 (fresh-case! (list (cons "twentyfour.md" "# Real\nbody\n"))))
(define d24 (cdr t24))
(import-md d24 (car t24) "tester")
(define out24 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out24))
(define stray24
  (id-written-by
    (with-store-write d24
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title "Stray")))))
      "tester")))
(define code24
  (id-written-by
    (with-store-write d24
      (lambda (s v) (list (list 'insert stray24 #f (list (cons 'kind 'code) (cons 'title "fn")))))
      "tester")))

(want "F42 a block of another projection's kind is not counted in anybody's reach"
      (let* ((answer (export-md d24 out24))
             (sk (cdr (assq 'skipped (cddr answer))))
             (claimed (apply + (map (lambda (e) (+ 1 (cadr (assq 'subtree (cddr e))))) sk))))
        (list (kind-of-block d24 code24)
              (length sk) (car (car sk)) (cadr (assq 'subtree (cddr (car sk))))
              claimed))
      (list 'code 1 stray24 0 1))


;; NEVER: A PATH THAT WOULD LEAVE THE TARGET DIRECTORY IS NOT WRITTEN AT ALL.
;; `../x.md` used to be handed to `write-file` as `dir + "/../x.md"`, which
;; puts a file OUTSIDE the directory the caller named, and the answer was
;; `(ok (files 1))` -- a caller asking for an export into a scratch directory
;; got a write into its parent and was told everything went fine. The row
;; below looks in the parent directory, which is the only place the evidence
;; would be.
(define t25 (fresh-case! (list (cons "twentyfive.md" "# Fine\nbody\n"))))
(define d25 (cdr t25))
(import-md d25 (car t25) "tester")
(define case25 (string-append root "/c" (number->string case-n)))
(define out25 (string-append case25 "/out"))
(system (string-append "mkdir -p " out25))
(define escaper25
  (id-written-by
    (with-store-write d25
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path "../escaped.md")
                                                       (cons 'title "Escape")))))
      "tester")))

(want "F42 a path that climbs out of the target directory is refused, named, and writes nothing"
      (let* ((answer (export-md d25 out25))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (cadr answer)
              (length sk) (car (car sk)) (cadr (car sk))
              (cadr (assq 'path (cddr (car sk))))
              (if (file-exists? (string-append case25 "/escaped.md"))
                  'IT-WROTE-OUTSIDE-THE-TARGET-DIRECTORY
                  'and-nothing-was-written-outside)
              (list-sort string<? (directory-entries out25))))
      (list '(files 1) 1 escaper25 'path-not-usable "../escaped.md"
            'and-nothing-was-written-outside '("twentyfive.md")))

;; NEVER: AND A DOCUMENT WITH NO PATH DOES NOT OPEN THE DIRECTORY AS A FILE.
;; `text-field` answers "" for a missing path, and `dir + "/"` is the
;; directory itself; `write-file` opens it with `no-fail`. The row asserts the
;; export answers at all -- which is what it could not do if the open raised --
;; and that the directory is still a directory afterwards.
(define t26 (fresh-case! (list (cons "twentysix.md" "# Fine\nbody\n"))))
(define d26 (cdr t26))
(import-md d26 (car t26) "tester")
(define out26 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out26))
(define pathless26
  (id-written-by
    (with-store-write d26
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'title "No path at all")))))
      "tester")))

(want "F42 a document with no path is named, and the target directory is left a directory"
      (let* ((answer (guard (e (#t (list 'RAISED))) (export-md d26 out26)))
             (sk (and (pair? answer) (eq? (car answer) 'ok)
                      (cdr (assq 'skipped (cddr answer))))))
        (list (and (pair? answer) (car answer))
              (and sk (length sk))
              (and sk (car (car sk)))
              (and sk (cadr (car sk)))
              (and sk (cadr (assq 'path (cddr (car sk)))))
              (if (file-is-directory? out26) 'still-a-directory 'NO-LONGER-A-DIRECTORY)))
      (list 'ok 1 pathless26 'path-not-usable "" 'still-a-directory))

;; NEVER: A PATH WITH A `.` COMPONENT IS REFUSED, NOT REPAIRED. An earlier
;; version of this round normalised `./a.md` into `a.md` and reported the
;; second document as a conflict. The code projection already had a rule for
;; what a projection may write -- `code-safe-path?` -- and it REFUSES such a
;; path rather than repairing it. Two projections disagreeing about what is
;; safe is worse than either answer, so this one asks the same rule.
(define t27 (fresh-case! (list (cons "twentyseven.md" "# First\nbody of the first\n"))))
(define d27 (cdr t27))
(import-md d27 (car t27) "tester")
(define out27 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out27))
(define first27 (block-of-kind d27 'doc))
(define dotted27 (string-append "./" (text-field-of d27 first27 'path)))
(define second27
  (id-written-by
    (with-store-write d27
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path dotted27)
                                                       (cons 'title "Second")))))
      "tester")))

(want "F42 a path with a dot component is refused by the same rule the code projection uses"
      (let* ((answer (export-md d27 out27))
             (sk (cdr (assq 'skipped (cddr answer))))
             (text (slurp (string-append out27 "/twentyseven.md"))))
        (list (code-safe-path? dotted27)
              (cadr answer)
              (length sk) (car (car sk)) (cadr (car sk))
              (cadr (assq 'path (cddr (car sk))))
              (and (holds? text "body of the first") 'the-first-is-on-disk)))
      (list #f '(files 1) 1 second27 'path-not-usable dotted27 'the-first-is-on-disk))

;; NEVER: A NUL IS A TRUNCATION POINT FOR THE SYSTEM AND HAS TO BE ONE FOR US.
;; `"a.md\x0;suffix"` reaches the filesystem as `a.md`, so it and `a.md` are
;; one file while being two strings. The shared rule refuses any NUL.
(define t28 (fresh-case! (list (cons "twentyeight.md" "# Fine\nbody\n"))))
(define d28 (cdr t28))
(import-md d28 (car t28) "tester")
(define out28 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out28))
(define nul28 (string-append (text-field-of d28 (block-of-kind d28 'doc) 'path)
                             (string (integer->char 0)) "suffix"))
(define nulled28
  (id-written-by
    (with-store-write d28
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path nul28)
                                                       (cons 'title "Nulled")))))
      "tester")))

(want "F42 a path carrying a NUL is refused"
      (let* ((answer (export-md d28 out28))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (code-safe-path? nul28)
              (cadr answer)
              (length sk) (car (car sk)) (cadr (car sk))
              (list-sort string<? (directory-entries out28))))
      (list #f '(files 1) 1 nulled28 'path-not-usable '("twentyeight.md")))

;; NEVER: AND TWO SPELLINGS THE FILESYSTEM CALLS ONE FILE ARE ONE FILE. On a
;; case-insensitive volume `a.md` and `A.md` are the same file, and a conflict
;; key compared as text saw two. The key is now the filesystem's own answer,
;; so this row ASKS THE VOLUME rather than assuming: where case is folded the
;; second document is a conflict, and where it is not they are two documents
;; and two files. A case-sensitive volume on a case-insensitive machine is
;; ordinary, and a row that assumed either way would be wrong on somebody's
;; machine rather than measuring.
(define t29 (fresh-case! (list (cons "twentynine.md" "# Lower\nbody of the lower\n"))))
(define d29 (cdr t29))
(import-md d29 (car t29) "tester")
(define out29 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out29))
(define lower29 (block-of-kind d29 'doc))
(define upper29-path (string-upcase (text-field-of d29 lower29 'path)))
(define upper29
  (id-written-by
    (with-store-write d29
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path upper29-path)
                                                       (cons 'title "Upper")))))
      "tester")))

;; NEVER: AND THIS ROW CANNOT SEE HALF OF WHAT IT ASKS, ON THIS MACHINE.
;; Folding case unconditionally -- ignoring what the volume says -- behaves
;; exactly like the correct rule on a volume that folds, so the mutation that
;; does it SURVIVES here: macOS's default volume folds. The same mutation is
;; killed on a case-sensitive volume, where folding would report a conflict
;; between two documents that are genuinely two files, and this suite runs on
;; FreeBSD as well. That is the reading this row cannot produce on the machine
;; it was written on, written down rather than left to look like coverage.
(want "F42 two spellings the volume calls one file are one file, and where it does not they are two"
      (let* ((folds (eq? #f (path-case-sensitive? out29)))
             (answer (export-md d29 out29))
             (sk (if (null? (cddr answer)) '() (cdr (assq 'skipped (cddr answer))))))
        (list (if folds 'the-volume-folds-case 'the-volume-keeps-case)
              (cadr answer)
              (length sk)
              (map cadr sk)))
        (let ((folds (eq? #f (path-case-sensitive? out29))))
          (if folds
              (list 'the-volume-folds-case '(files 1) 1 '(path-conflict))
              (list 'the-volume-keeps-case '(files 2) 0 '()))))

;; NEVER: AND A SYMLINK IS A WAY OUT THAT NO AMOUNT OF READING THE STRING
;; WILL SHOW. `link/out.md` has no `..` in it and is perfectly relative; if
;; `link` is a symlink to somewhere else, writing it puts a file outside the
;; directory the caller named. That is why the key is the filesystem's answer
;; and not the spelling: the containment check is made against the RESOLVED
;; name. The row looks outside, which is where the evidence would be.
(define t30 (fresh-case! (list (cons "thirty.md" "# Fine\nbody\n"))))
(define d30 (cdr t30))
(import-md d30 (car t30) "tester")
(define case30 (string-append root "/c" (number->string case-n)))
(define out30 (string-append case30 "/out"))
(define elsewhere30 (string-append case30 "/elsewhere"))
(system (string-append "mkdir -p " out30 " " elsewhere30))
(system (string-append "ln -s " elsewhere30 " " out30 "/link"))
(define through30
  (id-written-by
    (with-store-write d30
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'doc)
                                                       (cons 'path "link/out.md")
                                                       (cons 'title "Through the link")))))
      "tester")))

(want "F42 the symlink is really there and really points out of the target directory"
      (list (if (file-is-directory? (string-append out30 "/link")) 'the-link-resolves-to-a-directory 'NO-LINK)
            (code-safe-path? "link/out.md")
            (equal? (real-path (string-append out30 "/link")) (real-path elsewhere30)))
      (list 'the-link-resolves-to-a-directory #t #t))

(want "F42 a path leading through a symlink out of the target is refused, named, and writes nothing"
      (let* ((answer (export-md d30 out30))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (cadr answer)
              (length sk) (car (car sk)) (cadr (car sk))
              (cadr (assq 'path (cddr (car sk))))
              (if (file-exists? (string-append elsewhere30 "/out.md"))
                  'IT-WROTE-THROUGH-THE-LINK
                  'and-nothing-was-written-through-it)))
      (list '(files 1) 1 through30 'path-not-usable "link/out.md"
            'and-nothing-was-written-through-it))

;; NEVER: AND THE REPORT IS NOT ALLOWED TO COST MORE THAN THE EXPORT. Every
;; entry used to recompute the whole outline to count its reach, which is
;; invisible in a fixture with two skipped blocks and ruinous in a store with
;; many: measured at 600 unwritten blocks, 806 ms, rising as the cube -- at a
;; few thousand it is seconds. The budget below is generous by more than an
;; order of magnitude against the reading that failed (12 ms now, 806 ms
;; then), so it is about a change of SHAPE and will not flap on a loaded
;; machine.
(define t18 (fresh-case! (list (cons "eighteen.md" "# Eighteen\nbody\n"))))
(define d18 (cdr t18))
(import-md d18 (car t18) "tester")
(define out18 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out18))
(with-store-write d18
  (lambda (s v)
    (let loop ((i 0) (acc '()))
      (if (= i 600)
          (reverse acc)
          (loop (+ i 1)
                (cons (list 'insert 'root #f (list (cons 'title (number->string i)))) acc)))))
  "tester")

(want "F42 six hundred unwritten blocks are reported in a time that says the walk is not repeated"
      (let* ((t0 (current-time 'time-monotonic))
             (answer (export-md d18 out18))
             (t1 (current-time 'time-monotonic))
             (dt (time-difference t1 t0))
             (ms (+ (* 1000 (time-second dt)) (div (time-nanosecond dt) 1000000)))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (length sk)
              (if (< ms 200) 'within-budget (list 'TOO-SLOW ms))))
      (list 600 'within-budget))

;; NEVER: AND WHEN THERE ARE TWO, THE LIST HAS AN ORDER OF ITS OWN. The rows
;; `state-outline` returns follow the generated writer name, so a skip list
;; built in row order is a different list on two stores holding the same
;; tree. The row below asks for the property -- ascending by id -- rather
;; than restating the sort: an answer that merely happened to come out
;; ascending on this store would pass, and the mutation that reverses the
;; comparison is what says the property is held on purpose.
;;
;; Both blocks sit at root, side by side and not one inside the other, which
;; is what makes this a question about ORDER and not about nesting.
(define t12 (fresh-case! (list (cons "twelve.md" "# Twelve\nbody of twelve\n"))))
(define d12 (cdr t12))
(import-md d12 (car t12) "tester")
(define out12 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out12))
(define (kindless-at-root! store)
  (id-written-by
    (with-store-write store
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'title "kindless")))))
      "tester")))
(define kindless12a (kindless-at-root! d12))
(define kindless12b (kindless-at-root! d12))

(want "F42 two unwritable blocks are both named, each with its reach, in ascending id order"
      (let* ((answer (export-md d12 out12))
             (sk (cdr (assq 'skipped (cddr answer)))))
        (list (equal? (map car sk) (list-sort string<? (map car sk)))
              (length sk)
              (assoc kindless12a sk)
              (assoc kindless12b sk)))
      (list #t 2
            (list kindless12a 'kind-absent '(subtree 0))
            (list kindless12b 'kind-absent '(subtree 0))))

;; NEVER: AND THE TOTAL IS SOMETHING THE CALLER CAN ADD UP. That is the whole
;; reason the reach is a number in the entry rather than a line per block:
;; `1 + n` summed over the entries is the count of blocks that went unwritten,
;; each one counted once. This row does that sum and compares it with the
;; blocks the store actually failed to export.

;; NEVER: A BLOCK THAT BELONGS TO ANOTHER PROJECTION IS NOT LOST, AND THE SUM
;; IS ABOUT THIS PROJECTION'S BLOCKS. A `code` block is written by the code
;; projection, not this one; no markdown file contains it and none should.
;; Listing it would make every store with code in it report a page of entries,
;; which is the noise the reach exists to avoid. So the arithmetic below is
;; exact for the blocks this verb owns, and this row is what says the other
;; kind is deliberately outside it rather than accidentally missing.
(define t16 (fresh-case! (list (cons "sixteen.md" "# Sixteen\nbody\n"))))
(define d16 (cdr t16))
(import-md d16 (car t16) "tester")
(define out16 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out16))
(define code16
  (id-written-by
    (with-store-write d16
      (lambda (s v) (list (list 'insert 'root #f (list (cons 'kind 'code)
                                                       (cons 'title "a function")))))
      "tester")))

(want "F42 a block of another projection's kind is not written here and is not reported"
      (let* ((answer (export-md d16 out16))
             (text (slurp (string-append out16 "/sixteen.md"))))
        (list (kind-of-block d16 code16)
              answer
              (length (directory-list out16))
              (if (holds? text "a function") 'THE-CODE-BLOCK-WAS-WRITTEN-HERE 'and-it-is-not-in-the-file)
              (and (holds? text "# Sixteen") 'while-the-document-is)))
      (list 'code '(ok (files 1)) 1 'and-it-is-not-in-the-file 'while-the-document-is))

;; THE SUM IS ASKED OF TWO STORES THAT FAIL DIFFERENTLY. One is a doc whose
;; kind cannot be read; the other is a store whose blocks were moved into a
;; CYCLE, which `state-outline` resolves by relocating them to root -- they
;; belong to no document any more, the one file written is zero bytes, and
;; before the third reason existed the answer to that store was
;; `(ok (files 1))` with nothing to say about three lost blocks. A property
;; asked of one store is a reading; asked of two that fail for unrelated
;; reasons it starts to be a property.
(define (claimed-unwritten store dir)
  (let* ((answer (export-md store dir))
         (sk (if (null? (cddr answer)) '() (cdr (assq 'skipped (cddr answer))))))
    (apply + (map (lambda (e) (+ 1 (cadr (assq 'subtree (cddr e))))) sk))))

(want "F42 one plus the reach, summed over the entries, is the number of blocks not written"
      (let ((claimed (claimed-unwritten d13 out13))
            (all (length (map caddr (state-outline (open-and-reduce d13))))))
        (list claimed all (= claimed all)))
      (list 3 3 #t))

;; The cycle store: four blocks, the doc is written (empty, which is its own
;; question and is recorded for a later round), and the three under it reach
;; no file at all.
(define t15 (fresh-case! (list (cons "fifteen.md" "# A\na\n\n## B\nb\n\n### C\nc\n"))))
(define d15 (cdr t15))
(import-md d15 (car t15) "tester")
(define out15 (string-append root "/c" (number->string case-n) "/out"))
(system (string-append "mkdir -p " out15))
(define rows15 (state-outline (open-and-reduce d15)))
(define child15 (caddr (car (filter (lambda (r) (not (eq? 'root (car r)))) rows15))))
(define grand15 (caddr (car (filter (lambda (r) (equal? (car r) child15)) rows15))))
;; The move comes from ANOTHER WRITER'S PUBLISHED RECORD: one writer's move
;; under its own child is refused at write time, and a record that arrives
;; through publish is not checked that way. It is published with a
;; dependency on the local writer's end, as a mirror's is, and it makes the
;; cycle; the reduction resolves it by relocating the blocks to root. That
;; is the point of this case.
(define move15
  (let* ((cut (reduce-applied-cut (open-and-reduce d15)))
         (bytes (encode-record 1 1757300000001 "peer" (list (car cut))
                               (storable-encode (list 'move child15 grand15 1)))))
    (log-publish! d15 "zzzzzzzz" 1 bytes (segment-sha bytes))))

(want "the store accepts another writer's move that makes a cycle, which is what strands the blocks"
      (list (car move15)
            (length (filter (lambda (r) (eq? 'root (car r)))
                            (state-outline (open-and-reduce d15)))))
      (list 'published 3))

(want "F42 blocks that belong to no document are named too, and the sum still closes"
      (let* ((answer (export-md d15 out15))
             (sk (cdr (assq 'skipped (cddr answer))))
             (claimed (claimed-unwritten d15 out15))
             (all (length (map caddr (state-outline (open-and-reduce d15)))))
             (written (filter (lambda (id) (not (assoc id sk)))
                              (map caddr (state-outline (open-and-reduce d15))))))
        (list (map cadr sk)
              (list-sort string<? (map car sk))
              (map (lambda (e) (cadr (assq 'subtree (cddr e)))) sk)
              claimed all
              (length (directory-list out15))
              (= claimed (- all 1))))
      (list '(not-in-any-document not-in-any-document)
            (list-sort string<? (list child15 grand15))
            '(0 1)
            3 4
            1
            #t))

(printf "== a directory adds to the store; what it lacks is reported ==\n")
;; NEVER: A DIRECTORY OF DOCUMENTS ADDS AND UPDATES. A document of the store
;; whose file the directory does not hold is left alone and named under
;; `absent`; with --allow-delete it is deleted and named under `deleted`,
;; read from the deletions that ran. The results stay the answer's second
;; element, each judged on its own.
(define (live-doc-of d path)
  (let ((state (open-and-reduce d)))
    (find (lambda (id)
            (let ((b (state-read state id)))
              (and b (not (cdr (assq 'deleted b)))
                   (let ((fs (cdr (assq 'fields b))))
                     (and (equal? (cdr (or (assq 'kind fs) '(kind . #f))) 'doc)
                          (equal? (cdr (or (assq 'path fs) '(path . #f))) path))))))
          (map cadr (state-datum state)))))
(define (subtree-hashes d id)
  (let ((state (open-and-reduce d)))
    (map (lambda (b) (block-hash state b)) (cons id (subtree-ids state id)))))
;; -> (src store) holding docs A (a.md) and B (b.md), then a directory
;; that no longer holds a.md and holds a new c.md.
(define (absent-case!)
  (let* ((t (fresh-case! (list (cons "a.md" "# A\nbody a\n## A2\nsub\n") (cons "b.md" "# B\nbody b\n"))))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (system (string-append "rm " src "/a.md"))
    (put! (string-append src "/c.md") "# C\nbody c\n")
    t))
(define (all-ok? results) (and (list? results) (for-all (lambda (r) (and (pair? r) (eq? (car r) 'ok))) results)))

;; V1: the answer as a caller gets it.
(define v1 (absent-case!))
(define v1-a (live-doc-of (cdr v1) "a.md"))
(define v1-b (live-doc-of (cdr v1) "b.md"))
(define v1-hashes (list (subtree-hashes (cdr v1) v1-a) (subtree-hashes (cdr v1) v1-b)))
(define v1-answer (caught (rpc-dispatch (cdr v1) (list 'import-md (car v1)) "t")))
(want "V1 a directory holding B and a new C: import, every result ok, C imported, A and B untouched, and (absent (files (\"a.md\"))) exactly"
      (list (car v1-answer) (all-ok? (cadr v1-answer)) (and (live-doc-of (cdr v1) "c.md") #t)
            (equal? v1-hashes (list (subtree-hashes (cdr v1) v1-a) (subtree-hashes (cdr v1) v1-b)))
            (cddr v1-answer))
      '(import #t #t #t ((absent (files ("a.md"))))))

;; V4: the export after V1 writes the three documents.
(define v4-out (string-append (car v1) "-out"))
(system (string-append "mkdir -p " v4-out))
(want "V4 export-md after V1 writes exactly a.md, b.md and c.md"
      (begin (export-md (cdr v1) v4-out) (list-sort string<? (directory-list v4-out)))
      '("a.md" "b.md" "c.md"))

;; V2: the same, allowed to delete.
(define v2 (absent-case!))
(define v2-a (live-doc-of (cdr v2) "a.md"))
(define v2-b (live-doc-of (cdr v2) "b.md"))
(define v2-b-hashes (subtree-hashes (cdr v2) v2-b))
(define v2-report (caught (import-md-report (cdr v2) (car v2) "t" #t)))
(want "V2 with --allow-delete: C imported, A deleted, B untouched, (deleted (files (\"a.md\"))) and no absent clause"
      (list (all-ok? (car v2-report)) (and (live-doc-of (cdr v2) "c.md") #t) (live-doc-of (cdr v2) "a.md")
            (equal? v2-b-hashes (subtree-hashes (cdr v2) v2-b)) (cadr v2-report))
      '(#t #t #f #t (deleted (files ("a.md")))))

;; V2b: a deletion planned after a file that fails did not run, and is not
;; reported. B becomes ambiguous (a third identical section), A is gone.
(define v2b (fresh-case! (list (cons "a.md" "# A\nbody a\n") (cons "b.md" "# T\nsame\n# T\nsame\n"))))
(import-md (cdr v2b) (car v2b) "t")
(system (string-append "rm " (car v2b) "/a.md"))
(put! (string-append (car v2b) "/b.md") "# T\nsame\n# T\nsame\n# T\nsame\n")
(define v2b-report (caught (import-md-report (cdr v2b) (car v2b) "t" #t)))
(want "V2b with --allow-delete, a file that fails stops the batch: A is not deleted and no deleted clause is reported"
      (list (exists (lambda (r) (and (pair? r) (eq? (car r) 'error))) (car v2b-report))
            (and (live-doc-of (cdr v2b) "a.md") #t)
            (cadr v2b-report))
      '(#t #t #f))

;; V3: CONTROL, nothing absent, no clause.
(define v3 (fresh-case! (list (cons "a.md" "# A\nbody a\n") (cons "b.md" "# B\nbody b\n"))))
(import-md (cdr v3) (car v3) "t")
(want "V3 CONTROL a directory holding every document of the store answers with no absent clause"
      (let ((r (import-md-report (cdr v3) (car v3) "t"))) (list (all-ok? (car r)) (cadr r)))
      '(#t #f))

;; V6 PIN: A DELETED DOCUMENT WHOSE FILE IS STILL THERE KEEPS THE SECTION RULE.
;; It is not absent (it is not live), and its file is present, so a section
;; removed from that file is still refused would-delete, as it always was.
(define v6 (fresh-case! (list (cons "a.md" "# A\nbody a\n## S\nsection\n"))))
(import-md (cdr v6) (car v6) "t")
(define v6-doc (live-doc-of (cdr v6) "a.md"))
(rpc-dispatch (cdr v6) (list 'del v6-doc) "t")
(put! (string-append (car v6) "/a.md") "# A\nbody a\n")
(want "V6 PIN a deleted document whose file is still there: a section removed from the file is refused would-delete"
      (let* ((r (caught (import-md-report (cdr v6) (car v6) "t")))
             (first (and (pair? r) (pair? (car r)) (car (car r)))))
        (and (pair? first) (pair? (cdr first)) (list (car first) (cadr first))))
      '(error would-delete))

(printf "== a heading is a section only when it says so ==\n")
;; NEVER: A COUNT IS NOT AN IDENTITY. With the counts equal, the import
;; paired stored and incoming sections by position with no look at their
;; headings: a section a batch had inserted, the only one left in its
;; document, was rewritten into the file's one heading. Position now needs
;; every pair to agree on its heading; a block that is not a section is
;; matched only by its own rendering and is never missing; a marker counts
;; section by section; a matched section goes where the file puts it; and
;; what a deleted section holds that the file does not describe is moved to
;; its nearest surviving ancestor before it goes.
(define (kids d id)
  (map caddr (filter (lambda (r) (equal? (car r) id)) (state-outline (open-and-reduce d)))))
(define (position-parent d id)
  (let ((b (state-read (open-and-reduce d) id)))
    (and b (let ((p (cdr (assq 'position b)))) (and (pair? p) (car p))))))
(define (live? d id)
  (let ((b (state-read (open-and-reduce d) id)))
    (and b (not (cdr (assq 'deleted b))))))
(define (insert-block! d parent after fields)
  (id-written-by (with-store-write d (lambda (s v) (list (list 'insert parent after fields))) "t")))
;; A block put through the log directly, as history written before the
;; write path's rules would hold it: the only way to have a document below
;; another one.
(define (log-put! d fields)
  (let* ((sess (log-begin d (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "independent-fixture" '()
                                      (list 'put fields)))
    (session-commit! sess)
    (log-end! sess)
    id))
(define (x-section level title src)
  (list (cons 'kind 'section) (cons 'level level) (cons 'title title)
        (cons 'heading-src (string-append (make-string level #\#) " " title "\n"))
        (cons 'src src)))
(define (x-code title src)
  (list (cons 'kind 'code) (cons 'title title) (cons 'src src)))
(define (marker id) (string-append "<!-- theourgia: " id " -->\n"))
(define (first-answer r) (if (and (pair? r) (pair? (car r))) (car r) r))
(define (swap text from to)
  (let ((n (string-length from)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) text)
            ((string=? (substring text i (+ i n)) from)
             (string-append (substring text 0 i) to (substring text (+ i n) m)))
            (else (loop (+ i 1)))))))
;; The document's file as export-md writes it, plain or with ids.
(define (exported src d name with-ids?)
  (let ((out (string-append src "-out")))
    (system (string-append "rm -rf " out "; mkdir -p " out))
    (export-md d out with-ids?)
    (slurp (string-append out "/" name))))
(define (kind-at d id) (text-field-of d id 'kind))

;; -> (src store f g a b c): f.md's b moved under g's document, c inserted
;; under f's, and g.md written as the store now holds it.
(define (x1-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# a\nbody a\n# b\nbody b\n")
                               (cons "g.md" "# g\nbody g\n"))))
         (src (car t)) (d (cdr t)))
    (import-md d src "t")
    (let* ((f (live-doc-of d "f.md")) (g (live-doc-of d "g.md"))
           (a (car (kids d f))) (b (cadr (kids d f))))
      (with-store-write d (lambda (s v) (list (list 'move b g #f))) "t")
      (let ((c (insert-block! d f #f (x-section 1 "c" "body c\n"))))
        (put! (string-append src "/g.md") "# g\nbody g\n# b\nbody b\n")
        (list src d f g a b c)))))

;; X1: the finding.
(want "X1 a section inserted under a document whose file still names a section moved out of it is not taken over: refused would-delete naming it, nothing written, its title and body unchanged"
      (let* ((x (x1-case!)) (d (list-ref x 1)) (c (list-ref x 6)) (before (records d))
             (r (import-md d (car x) "t")))
        (list (equal? (first-answer r)
                      (list 'error 'would-delete (list 'blocks (list c)) '(remedy allow-delete)))
              (- (records d) before) (text-field-of d c 'title) (text-field-of d c 'src)))
      (list #t 0 "c" "body c\n"))
;; X1b: allowed to delete.
(want "X1b with --allow-delete: a kept with nothing written for it, c deleted, the file's b a new section after a, two records"
      (let* ((x (x1-case!)) (d (list-ref x 1)) (f (list-ref x 2)) (a (list-ref x 4)) (c (list-ref x 6))
             (before (records d))
             (r (import-md d (car x) "t" #t))
             (ks (kids d f)))
        (list (all-ok? r) (- (records d) before)
              (map (lambda (id) (text-field-of d id 'title)) ks)
              (equal? (car ks) a) (and (member c ks) #t) (live? d c)
              (kind-at d (cadr ks))))
      (list #t 2 '("a" "b") #t #f #f 'section))

;; X2 PIN: an unchanged file writes nothing.
(want "X2 PIN an unchanged nested file re-imported writes nothing"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n## B\nb\n# C\nc\n## D\nd\n"))))
             (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((before (records d)) (r (import-md d (car t) "t")))
          (list (length r) (- (records d) before))))
      (list 0 0))

;; X3: a heading edited in the file, the counts equal.
(want "X3 a heading edited in the file is a new section and the old one missing: refused would-delete naming the old one, nothing written"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (b (cadr (kids d f))) (before (records d)))
          (put! (string-append (car t) "/f.md") "# A\na\n# B2\nb\n")
          (let ((r (import-md d (car t) "t")))
            (list (equal? (first-answer r)
                          (list 'error 'would-delete (list 'blocks (list b)) '(remedy allow-delete)))
                  (- (records d) before) (text-field-of d b 'title)))))
      (list #t 0 "B"))

;; X4: a code block under the document, titled like the file's one heading.
(want "X4 a heading is not matched to a code block with its title: a new section, the code block's title and src unchanged"
      (let* ((t (fresh-case! (list (cons "f.md" "lead\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md"))
               (k (insert-block! d f #f (x-code "T" "code body\n"))))
          (put! (string-append (car t) "/f.md") "lead\n# T\nsection body\n")
          (let* ((r (import-md d (car t) "t")) (ks (kids d f)))
            (list (all-ok? r) (text-field-of d k 'src) (text-field-of d k 'title) (kind-at d k)
                  (length ks) (equal? (car ks) k)
                  (and (= 2 (length ks)) (kind-at d (cadr ks)))
                  (and (= 2 (length ks)) (text-field-of d (cadr ks) 'src))))))
      (list #t "code body\n" "T" 'code 2 #t 'section "section body\n"))

;; X4b: a code block is never missing.
(define (x4b-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (b (cadr (kids d f)))
           (k (insert-block! d f #f (x-code "Z" "z\n"))))
      (put! (string-append (car t) "/f.md") "# A\na\n")
      (list (car t) d f b k))))
(want "X4b a file with fewer sections: the refusal names only the section, not the code block"
      (let* ((x (x4b-case!)) (d (cadr x)) (b (cadddr x)))
        (equal? (first-answer (import-md d (car x) "t"))
                (list 'error 'would-delete (list 'blocks (list b)) '(remedy allow-delete))))
      #t)
(want "X4b with --allow-delete the section goes and the code block stays live and unchanged under the document"
      (let* ((x (x4b-case!)) (d (cadr x)) (f (caddr x)) (b (cadddr x)) (k (list-ref x 4))
             (r (import-md d (car x) "t" #t)))
        (list (all-ok? r) (live? d b) (live? d k) (equal? (position-parent d k) f) (text-field-of d k 'src)))
      (list #t #f #t #t "z\n"))

;; X5: a marked section whose heading was edited.
(want "X5 a marked section whose heading was edited is the marked block, under its new heading"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f)))
               (text (exported (car t) d "f.md" #t)))
          (put! (string-append (car t) "/f.md") (swap text "# B\n" "# B2\n"))
          (let ((r (import-md d (car t) "t")))
            (list (all-ok? r) (equal? (kids d f) (list a b)) (text-field-of d b 'title) (live? d b)))))
      (list #t #t "B2" #t))
;; X5b: a marker names a block of another kind.
(want "X5b a marked code block whose heading and body changed is updated by its id and stays code"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md"))
               (k (insert-block! d f #f (x-code "K" "k\n")))
               (text (exported (car t) d "f.md" #t)))
          (put! (string-append (car t) "/f.md") (swap text "# K\nk\n" "# K2\nk edited\n"))
          (let ((r (import-md d (car t) "t")))
            (list (all-ok? r) (kind-at d k) (text-field-of d k 'title) (text-field-of d k 'src)
                  (live? d k) (equal? (position-parent d k) f)))))
      (list #t 'code "K2" "k edited\n" #t #t))

;; X6: a partly marked file.
(define (x6-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f))))
      (put! (string-append (car t) "/f.md") (string-append (marker a) "# C\na\n# D\nd\n"))
      (list (car t) d f a b))))
(want "X6 a partly marked file: the marked section is its block, the unmarked heading new, the one left missing and named, nothing written"
      (let* ((x (x6-case!)) (d (cadr x)) (b (list-ref x 4)) (before (records d))
             (r (import-md d (car x) "t")))
        (list (equal? (first-answer r)
                      (list 'error 'would-delete (list 'blocks (list b)) '(remedy allow-delete)))
              (- (records d) before)))
      (list #t 0))
(want "X6 with --allow-delete the marked section takes the heading C, D is new after it, B is deleted"
      (let* ((x (x6-case!)) (d (cadr x)) (f (caddr x)) (a (cadddr x)) (b (list-ref x 4))
             (r (import-md d (car x) "t" #t))
             (ks (kids d f)))
        (list (all-ok? r) (equal? (car ks) a) (text-field-of d a 'title)
              (map (lambda (id) (text-field-of d id 'title)) ks) (live? d b)))
      (list #t #t "C" '("C" "D") #f))
;; X6b: two markers naming one block.
(want "X6b two markers naming one block are refused position-mismatch naming it, nothing written"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (before (records d)))
          (put! (string-append (car t) "/f.md")
                (string-append (marker a) "# A\na\n" (marker a) "# B\nb\n"))
          (let ((r (import-md d (car t) "t")))
            (list (equal? (first-answer r)
                          (list 'error 'position-mismatch (list 'claimed-twice (list a))))
                  (- (records d) before)))))
      (list #t 0))

;; X7: a parent heading renamed in the file.
(define (x7-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\nlead\n## B\nbody\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (car (kids d a))))
      (put! (string-append (car t) "/f.md") "# C\nlead\n## B\nbody\n")
      (list (car t) d f a b))))
(want "X7 with --allow-delete a renamed parent is a new section, its child keeps its id and moves under it, the old one is deleted, and the export is the file"
      (let* ((x (x7-case!)) (d (cadr x)) (f (caddr x)) (a (cadddr x)) (b (list-ref x 4))
             (r (import-md d (car x) "t" #t))
             (ks (kids d f)))
        (list (all-ok? r) (length ks) (and (pair? ks) (not (equal? (car ks) a)))
              (and (pair? ks) (text-field-of d (car ks) 'title))
              (and (pair? ks) (equal? (kids d (car ks)) (list b)))
              (live? d a)
              (exported (car x) d "f.md" #f)))
      (list #t 1 #t "C" #t #f "# C\nlead\n## B\nbody\n"))
(want "X7b without --allow-delete the rename is refused would-delete naming the old parent, nothing written"
      (let* ((x (x7-case!)) (d (cadr x)) (a (cadddr x)) (before (records d))
             (r (import-md d (car x) "t")))
        (list (equal? (first-answer r)
                      (list 'error 'would-delete (list 'blocks (list a)) '(remedy allow-delete)))
              (- (records d) before)))
      (list #t 0))

;; X8: a section moved under another parent, in the file's order.
(want "X8 a section moved in the file under a later parent keeps its id and lands before that parent's own child, in the file's order"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n## B\nb\n# C\nc\n## D\nd\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (ids (ids-of d))
               (a (car (kids d f))) (c (cadr (kids d f)))
               (b (car (kids d a))) (dd (car (kids d c)))
               (then "# A\na\n# C\nc\n## B\nb\n## D\nd\n")
               (before (records d)))
          (put! (string-append (car t) "/f.md") then)
          (let ((r (import-md d (car t) "t")))
            (list (all-ok? r) (- (records d) before) (equal? ids (ids-of d))
                  (equal? (kids d f) (list a c)) (kids d a) (equal? (kids d c) (list b dd))
                  (string=? (exported (car t) d "f.md" #f) then)))))
      (list #t 2 #t #t '() #t #t))

;; X8b: a new section ahead of every section its parent already holds.
(want "X8b a new section before the one section already there puts the two in the file's order, the old one keeping its id"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (then "# N\nn\n# A\na\n"))
          (put! (string-append (car t) "/f.md") then)
          (let* ((r (import-md d (car t) "t")) (ks (kids d f)))
            (list (all-ok? r) (map (lambda (id) (text-field-of d id 'title)) ks)
                  (and (= 2 (length ks)) (equal? (cadr ks) a))
                  (string=? (exported (car t) d "f.md" #f) then)))))
      (list #t '("N" "A") #t #t))

;; X9: what a deleted section holds that the file does not describe.
(define (x9-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\nbody a\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (a (car (kids d f)))
           (k (insert-block! d a #f (x-code "K" "k text\n")))
           (l (insert-block! d k #f (x-code "L" "l text\n"))))
      (put! (string-append (car t) "/f.md") "# C\nbody c\n")
      (list (car t) d f a k l))))
(want "X9 with --allow-delete the code block under a deleted section moves to the document with its child, both live and unchanged, and the export carries them"
      (let* ((x (x9-case!)) (d (cadr x)) (f (caddr x)) (a (cadddr x)) (k (list-ref x 4)) (l (list-ref x 5))
             (r (import-md d (car x) "t" #t))
             (text (exported (car x) d "f.md" #f)))
        (list (all-ok? r) (live? d a)
              (equal? (position-parent d k) f) (equal? (position-parent d l) k)
              (live? d k) (live? d l) (text-field-of d k 'src) (text-field-of d l 'src)
              (map (lambda (id) (text-field-of d id 'title)) (kids d f))
              (and (holds? text "k text") (holds? text "l text"))))
      (list #t #f #t #t #t #t "k text\n" "l text\n" '("C" "K") #t))
(want "X9b without --allow-delete the refusal names the section and, under holds, the root it would strand"
      (let* ((x (x9-case!)) (d (cadr x)) (a (cadddr x)) (k (list-ref x 4)) (before (records d))
             (r (import-md d (car x) "t")))
        (list (equal? (first-answer r)
                      (list 'error 'would-delete (list 'blocks (list a)) '(remedy allow-delete)
                            (list 'holds (list k))))
              (- (records d) before)))
      (list #t 0))
;; X9c: the walk up passes every deleted section.
(want "X9c a code block under two deleted sections goes to the document"
      (let* ((t (fresh-case! (list (cons "f.md" "# P\np\n## A\na\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (p (car (kids d f))) (a (car (kids d p)))
               (k (insert-block! d a #f (x-code "K" "k\n"))))
          (put! (string-append (car t) "/f.md") "# C\nc\n")
          (let ((r (import-md d (car t) "t" #t)))
            (list (all-ok? r) (live? d p) (live? d a) (live? d k) (equal? (position-parent d k) f)))))
      (list #t #f #f #t #t))
;; X9d: a block of another kind between two deleted sections survives.
(want "X9d a code block between two deleted sections goes to the document, and what the inner section held goes under it, intact"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f)))
               (m (insert-block! d a #f (x-code "M" "m\n")))
               (b (insert-block! d m #f (x-section 2 "B" "b\n")))
               (k (insert-block! d b #f (x-code "K" "k\n")))
               (j (insert-block! d k #f (x-code "J" "j\n"))))
          (put! (string-append (car t) "/f.md") "# C\nc\n")
          (let ((r (import-md d (car t) "t" #t)))
            (list (all-ok? r) (live? d a) (live? d b)
                  (equal? (position-parent d m) f) (equal? (position-parent d k) m)
                  (equal? (position-parent d j) k)
                  (live? d m) (live? d k) (live? d j)))))
      (list #t #f #f #t #t #t #t #t #t))
;; X9e, X9g: a nested document under a deleted section.
;; -> (src store f a n s), n.md present when present? is true.
(define (x9e-case! present?)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (a (car (kids d f)))
           (n (log-put! d (list (cons 'kind 'doc) (cons 'path "n.md") (cons 'title "N")
                                (cons 'parent a) (cons 'ord 0))))
           (s (log-put! d (append (x-section 1 "S" "s\n") (list (cons 'parent n) (cons 'ord 0))))))
      (when present? (put! (string-append (car t) "/n.md") "# S\ns\n"))
      (put! (string-append (car t) "/f.md") "# C\nc\n")
      (list (car t) d f a n s))))
(define x9e (caught (x9e-case! #t)))
(want "X9e with --allow-delete a nested document whose file is present moves to root and stays a document"
      (let* ((d (cadr x9e)) (a (cadddr x9e)) (n (list-ref x9e 4))
             (r (import-md d (car x9e) "t" #t)))
        (list (all-ok? r) (live? d a) (position-parent d n) (kind-at d n) (live? d n)))
      (list #t #f 'root 'doc #t))
(want "X9g and its section, unchanged in its own file, is matched there, not missing for the outer file: live, under it"
      (let* ((d (cadr x9e)) (n (list-ref x9e 4)) (s (list-ref x9e 5)))
        (list (live? d s) (equal? (position-parent d s) n) (text-field-of d s 'src)))
      (list #t #t "s\n"))
(want "X9e without --allow-delete the refusal names the section only, and the nested document under holds"
      (let* ((x (x9e-case! #t)) (d (cadr x)) (a (cadddr x)) (n (list-ref x 4)) (before (records d))
             (r (import-md d (car x) "t")))
        (list (equal? (first-answer r)
                      (list 'error 'would-delete (list 'blocks (list a)) '(remedy allow-delete)
                            (list 'holds (list n))))
              (- (records d) before)))
      (list #t 0))
;; X9f PIN: its file absent, the absent-document rule governs it.
(want "X9f PIN a nested document whose file is absent is reported absent, and names nothing under holds"
      (let* ((x (x9e-case! #f)) (d (cadr x))
             (rep (import-md-report d (car x) "t"))
             (first (first-answer (car rep))))
        (list (cadr rep) (and (pair? first) (list? first) (assq 'holds (cddr first)) #t)))
      (list '(absent (files ("n.md"))) #f))
(want "X9f PIN with --allow-delete it is deleted by that rule and not moved first"
      (let* ((x (x9e-case! #f)) (d (cadr x)) (a (cadddr x)) (n (list-ref x 4))
             (rep (import-md-report d (car x) "t" #t)))
        (list (cadr rep) (live? d n) (equal? (position-parent d n) a)))
      (list '(deleted (files ("n.md"))) #f #t))

;; X10: a block the file names is placed by the file, not re-homed.
(want "X10 a code block the file marks under another section goes there, not to the deleted section's ancestor"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f)))
               (k (insert-block! d a #f (x-code "K" "k\n"))))
          (put! (string-append (car t) "/f.md")
                (string-append (marker b) "# B\nb\n" (marker k) "## K\nk\n"))
          (let ((r (import-md d (car t) "t" #t)))
            (list (all-ok? r) (live? d a) (equal? (position-parent d k) b) (live? d k)
                  (kind-at d k) (text-field-of d b 'title)))))
      (list #t #f #t #t 'code "B"))

;; X11: an unmarked export of a document holding blocks of another kind
;; comes back as itself.
(define (round-trips d src name)
  (let loop ((cycle 0) (out '()))
    (if (= cycle 2)
        (reverse out)
        (begin
          (put! (string-append src "/" name) (exported src d name #f))
          (let* ((before (records d)) (r (import-md d src "t")))
            (loop (+ cycle 1) (cons (list (length r) (- (records d) before)) out)))))))
(want "X11 an unmarked export of a document holding a code block with a child, re-imported twice, writes nothing"
      (let* ((t (fresh-case! (list (cons "f.md" "# C\nc\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md"))
               (k (insert-block! d f #f (x-code "K" "k\n")))
               (l (insert-block! d k #f (x-code "L" "l\n"))))
          (round-trips d (car t) "f.md")))
      '((0 0) (0 0)))
(want "X11 and one whose code block has no final newline and is followed by a section writes nothing either"
      (let* ((t (fresh-case! (list (cons "f.md" "lead\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md"))
               (k (insert-block! d f #f (x-code "K" "x")))
               (s (insert-block! d f k (x-section 1 "S" "s\n"))))
          (round-trips d (car t) "f.md")))
      '((0 0) (0 0)))

;; X11 (iii): a block matched by its own rendering is not placed, even
;; where the file shows it somewhere else.
(want "X11 (iii) a file that shows a code block's rendering ahead of the section it follows writes nothing: the block is matched, not moved"
      (let* ((t (fresh-case! (list (cons "f.md" "# C\nc\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (c (car (kids d f)))
               (k (insert-block! d f c (x-code "K" "k\n"))))
          (put! (string-append (car t) "/f.md") "# K\nk\n# C\nc\n")
          (let* ((before (records d)) (r (import-md d (car t) "t")))
            (list (length r) (- (records d) before) (equal? (kids d f) (list c k))))))
      (list 0 0 #t))

;; X12: a nesting the export cannot write. A section moved under another of
;; the same level is written after it at that level; the file is compared
;; with what the store's own export gives, so its untouched export moves
;; nothing, and a new section after it goes where it follows, under the same
;; section, so the export still writes what the file says.
(define (x12-case!)
  (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
    (import-md d (car t) "t")
    (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f))))
      (with-store-write d (lambda (s v) (list (list 'move b a #f))) "t")
      (list (car t) d f a b))))
(want "X12 a section moved under another of the same level stays there when its untouched export is re-imported: no intents"
      (let* ((x (x12-case!)) (d (cadr x)) (a (cadddr x)) (b (list-ref x 4))
             (text (exported (car x) d "f.md" #f)))
        (put! (string-append (car x) "/f.md") text)
        (let* ((before (records d)) (r (import-md d (car x) "t")))
          (list text (length r) (- (records d) before) (equal? (position-parent d b) a))))
      (list "# A\na\n# B\nb\n" 0 0 #t))
(want "X12 (ii) and a new section after it in that file is one insert, after it and under the same section, not refused, the export unchanged"
      (let* ((x (x12-case!)) (d (cadr x)) (f (caddr x)) (a (cadddr x)) (b (list-ref x 4)))
        (put! (string-append (car x) "/f.md") "# A\na\n# B\nb\n# N\nn\n")
        (let* ((before (records d)) (r (import-md d (car x) "t")))
          (list (all-ok? r) (length r) (- (records d) before)
                (equal? (kids d f) (list a))
                (map (lambda (id) (text-field-of d id 'title)) (kids d a))
                (exported (car x) d "f.md" #f))))
      (list #t 1 1 #t '("B" "N") "# A\na\n# B\nb\n# N\nn\n"))
(want "X12 (iii) two sections stored under another of their level, a new one written between them: one insert between them, the export is the file, and its untouched re-import writes nothing"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n# C\nc\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (ks (kids d f))
               (a (car ks)) (b (cadr ks)) (c (caddr ks))
               (then "# A\na\n# B\nb\n# N\nn\n# C\nc\n"))
          (with-store-write d (lambda (s v) (list (list 'move b a #f) (list 'move c a b))) "t")
          (put! (string-append (car t) "/f.md") then)
          (let* ((before (records d)) (r (import-md d (car t) "t"))
                 (written (- (records d) before))
                 (text (exported (car t) d "f.md" #f)))
            (put! (string-append (car t) "/f.md") text)
            (let* ((before2 (records d)) (r2 (import-md d (car t) "t")))
              (list (all-ok? r) (length r) written (string=? text then)
                    (length r2) (- (records d) before2))))))
      (list #t 1 1 #t 0 0))

;; X13: a title the split normalises. A trailing space is written as it is
;; and read back without it; the section's key is the key of what the export
;; writes, so the untouched export matches the section it came from.
(want "X13 a section whose title has a trailing space matches its own untouched export: no intents, the title kept"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))))
          (with-store-write d (lambda (s v) (list (list 'set a 'title "A "))) "t")
          (put! (string-append (car t) "/f.md") (exported (car t) d "f.md" #f))
          (let* ((before (records d)) (r (import-md d (car t) "t")))
            (list (length r) (- (records d) before) (text-field-of d a 'title) (live? d a)))))
      (list 0 0 "A " #t))

;; X14: a section whose written heading does not parse as one heading -- a
;; title with a line break, which only a record from elsewhere can carry --
;; and whose level two writers set at once. Its level in the arrangement is
;; the level the heading is written at, not the conflict.
(define (mirror-publish! d seq deps payload)
  (let ((bytes (encode-record seq (+ 1757300000000 seq) "peer" deps (storable-encode payload))))
    (log-publish! d "zzzzzzzz" seq bytes (segment-sha bytes))))
(want "X14 a fully marked file imports over a section with a two-line title and a level in conflict: every answer ok, the file's title taken"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f)))
               (seen (car (reduce-applied-cut (open-and-reduce d)))))
          (with-store-write d (lambda (s v) (list (list 'set b 'level 2))) "t")
          (mirror-publish! d 1 (list seen) (list 'set b 'title "B\n# extra"))
          (mirror-publish! d 2 (list seen) (list 'set b 'level 3))
          (let ((level (text-field-of d b 'level)) (title (text-field-of d b 'title)))
            (put! (string-append (car t) "/f.md")
                  (string-append (marker a) "# A\na\n" (marker b) "# B\nb\n"))
            (let ((r (import-md d (car t) "t")))
              (list (and (pair? level) (eq? (car level) 'conflict)) title
                    (all-ok? r) (text-field-of d b 'title))))))
      (list #t "B\n# extra" #t "B"))

;; X15: a section stored under another of its level, the other dropped from
;; the file. The export wrote them as siblings, so the file agrees with the
;; export's order, but the section it is stored under is deleted: it is
;; placed where the file puts it, not left under the tombstone.
(want "X15 with --allow-delete a section stored under one the file drops is moved to where the file puts it, live, and the dropped one deleted"
      (let* ((t (fresh-case! (list (cons "f.md" "# A\na\n# B\nb\n")))) (d (cdr t)))
        (import-md d (car t) "t")
        (let* ((f (live-doc-of d "f.md")) (a (car (kids d f))) (b (cadr (kids d f))))
          (with-store-write d (lambda (s v) (list (list 'move b a #f))) "t")
          (put! (string-append (car t) "/f.md") "# B\nb\n")
          (let ((r (import-md d (car t) "t" #t)))
            (list (all-ok? r) (live? d a) (live? d b) (equal? (position-parent d b) f)
                  (exported (car t) d "f.md" #f)))))
      (list #t #f #t #t "# B\nb\n"))

;; NEVER: A FILE THAT IS NOT TEXT IS NOT IMPORTED WITH ITS BYTES CHANGED.
;; Decoded as it was, an invalid byte became U+FFFD and the file went in
;; as a document whose text no file holds. It is skipped and listed, by
;; import-code's rule, and touches nothing.
(define (put-bytes! p bv)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o bv))))
;; "# B\n", one byte that is not UTF-8, "\n"
(define not-utf8 (bytevector 35 32 66 10 255 10))
(want "Markdown files with a byte that is not UTF-8 are skipped and listed in the walk's order, and make no block"
      (let* ((t (fresh-case! (list (cons "a.md" "# A\nbody a\n") (cons "e.md" "# E\nbody e\n"))))
             (src (car t)) (d (cdr t)))
        (system (string-append "mkdir -p " src "/d"))
        (put-bytes! (string-append src "/d/c.md") not-utf8)
        (put-bytes! (string-append src "/b.md") not-utf8)
        (let ((r (import-md-report d src "t")))
          (list (all-ok? (car r)) (cadr r) (caddr r)
                (and (live-doc-of d "a.md") #t) (and (live-doc-of d "e.md") #t)
                (live-doc-of d "b.md") (live-doc-of d "d/c.md"))))
      (list #t #f '(skipped ("b.md" "d/c.md")) #t #t #f #f))
(want "a document whose file stops being UTF-8 is left as it is, not absent and not deleted with --allow-delete"
      (let* ((t (fresh-case! (list (cons "a.md" "# A\nbody a\n") (cons "b.md" "# B\nbody b\n## B2\nsub\n"))))
             (src (car t)) (d (cdr t)))
        (import-md d src "t")
        (let* ((b (live-doc-of d "b.md")) (hashes (subtree-hashes d b)) (before (records d)))
          (put-bytes! (string-append src "/b.md") not-utf8)
          (let ((r (import-md-report d src "t" #t)))
            (list (all-ok? (car r)) (cadr r) (caddr r) (- (records d) before)
                  (equal? hashes (subtree-hashes d b))))))
      (list #t #f '(skipped ("b.md")) 0 #t))
;; A NUL byte is not text either, and the skipped clause comes after the
;; absent or the deleted one, each where it has something to name.
(want "a file holding a NUL is skipped beside an absent document, and beside the deleted one with --allow-delete"
      (let* ((t (fresh-case! (list (cons "a.md" "# A\nbody a\n") (cons "b.md" "# B\nbody b\n"))))
             (src (car t)) (d (cdr t)))
        (import-md d src "t")
        (system (string-append "rm " src "/b.md"))
        ;; "# N\n", a NUL, "\n"
        (put-bytes! (string-append src "/n.md") (bytevector 35 32 78 10 0 10))
        (let* ((reported (import-md-report d src "t"))
               (wire (rpc-dispatch d (list 'import-md src) "t"))
               (allowed (import-md-report d src "t" #t)))
          (list (cdr reported) (and (pair? wire) (list? wire) (list-tail wire 2))
                (cdr allowed) (live-doc-of d "n.md"))))
      (list '((absent (files ("b.md"))) (skipped ("n.md")))
            '((absent (files ("b.md"))) (skipped ("n.md")))
            '((deleted (files ("b.md"))) (skipped ("n.md")))
            #f))
(want "the answer through the dispatcher carries the skipped clause last"
      (let* ((t (fresh-case! (list (cons "a.md" "# A\nbody a\n")))) (src (car t)) (d (cdr t)))
        (put-bytes! (string-append src "/b.md") not-utf8)
        (let ((a (rpc-dispatch d (list 'import-md src) "t")))
          (list (car a) (all-ok? (cadr a)) (cddr a))))
      (list 'import #t '((skipped ("b.md")))))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "md2 complete\n")
