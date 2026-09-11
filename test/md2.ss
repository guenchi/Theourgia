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
        (theourgia log) (theourgia ffi) (theourgia md))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

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
    (putenv "THEOURGIA_HOME" (string-append dir "/home"))
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
(want "a file that disappeared is named, not tombstoned, until it is allowed"
      (gone! (list (cons "a.md" "# A\nbody a\n## A2\nsub\n")
                   (cons "b.md" "# B\nbody b\n"))
             (lambda (src) (system (string-append "rm " src "/b.md"))))
      (list (list 'error 'would-delete '(blocks (b5 b4)) '(remedy allow-delete))
            2
            '(b1 b2 b3)))
(want "a section that disappeared is named the same way"
      (gone! (list (cons "a.md" "# A\nbody a\n## A2\nsub\n"))
             (lambda (src) (put! (string-append src "/a.md") "# A\nbody a\n")))
      (list (list 'error 'would-delete '(blocks (b3)) '(remedy allow-delete))
            1
            '(b1 b2)))

(printf "\n~a failures\n" bad)
(printf "md2 complete\n")
