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

(printf "\n~a failures\n" bad)
(printf "md2 complete\n")
