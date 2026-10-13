#!chezscheme
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

;; changed-files <cut> <cut>: the files whose exported raw bytes or presence
;; differ between two cuts. Each row builds a small store from files on disk
;; (import-code), tags a cut, changes it, tags another, and reads the answer:
;; content, added, removed; nothing for a title, a cancelling pair of src
;; edits or a swap of equal files; the projection's own refusals; a set of
;; paths that cannot form a tree; and the case- and normalisation-ambiguous
;; marks, with the degeneracy reference run in a child under the switch.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-field code-children volume-insensitivity)
        (only (theourgia log) log-publish! segment-sha writer-directory)
        (only (theourgia wire) encode-record storable-encode))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/changed-files-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'changed-files "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a)) (pair? (caddr a)) (eq? (car (caddr a)) 'reason))
         (list 'error (cadr a) (caddr a)))
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
(define (items-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'items))
                         (cdr (cadr a))
                         (list 'NOT-ITEMS a)))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))

(define n 0)
;; A store whose source directory holds FILES ((<path> . <text>) ...), imported
;; once. -> (store . src-dir)
(define (write-file! path text)
  (sh "mkdir -p " (quoted (path-parent path)))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (file-store! files)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")) (src (string-append d "/src")))
    (sh "mkdir -p " (quoted st) " " (quoted src) " " (quoted (string-append d "/home")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (ask st 'init)
    (for-each (lambda (f) (write-file! (string-append src "/" (car f)) (cdr f))) files)
    (unless (eq? 'ok (head-of (ask st 'import-code src))) (assertion-violation 'changed-files "import failed" files))
    (cons st src)))
(define (store-of s) (car s))
(define (src-of s) (cdr s))
;; the file block at PATH
(define (file-at st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state))))
(define (children-at st path) (code-children (open-and-reduce st) (file-at st path)))
(define (cf st a b) (ask st 'changed-files a b))
;; The text of the file at PATH, imported as one block, set to TEXT through
;; the store: its block written and committed. Not a second import of the
;; source directory -- files without markers import as new files at the same
;; paths, and the cut would refuse duplicate-path.
(define (set-text! st path text)
  (let ((kids (children-at st path)))
    (unless (= 1 (length kids)) (assertion-violation 'changed-files "not one block" path kids))
    (ask st 'write (car kids) text "--writer" "w1")
    (let ((c (ask st 'commit "--writer" "w1")))
      (unless (eq? 'ok (head-of c)) (assertion-violation 'changed-files "commit refused" path c)))))
;; A first import of a Scheme file split at its definitions: split-suggest's
;; review copy, imported in place of the file.
(define (split-store! path text)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store"))
         (raw (string-append d "/raw")) (src (string-append d "/src")))
    (sh "mkdir -p " (quoted st) " " (quoted raw) " " (quoted src) " " (quoted (string-append d "/home")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (ask st 'init)
    (write-file! (string-append raw "/" path) text)
    (sh "mkdir -p " (quoted (path-parent (string-append src "/" path))))
    (ask st 'split-suggest (string-append raw "/" path) "--output" (string-append src "/" path))
    (unless (eq? 'ok (head-of (ask st 'import-code src))) (assertion-violation 'changed-files "split import failed" path))
    (cons st src)))

;; ---- CF-1 the trial's two files ----------------------------------------------
(printf "== CF-1..CF-7: what changes a file and what does not ==\n")
(let* ((s (file-store! '(("test/client-program.sc" . "(define a 1)\n") ("test/rows-baseline.txt" . "x 1 2\n")
                         ("README.md" . "# r\n"))))
       (st (store-of s)))
  (ask st 'tag "t0")
  (set-text! st "test/client-program.sc" "(define a 2)\n")
  (set-text! st "test/rows-baseline.txt" "x 1 3\n")
  (ask st 'tag "t1")
  (want "CF-1 the trial's two files are content changes, and nothing else is listed"
        (items-of (cf st "t0" "t1"))
        '((path "test/client-program.sc" content) (path "test/rows-baseline.txt" content)))
  (want "CF-12 the answer names both cuts, as their resolved literals"
        ;; each a non-empty cut of (writer . sequence) pairs, and the two
        ;; differ: two writes lie between the tags
        (let* ((a (cf st "t0" "t1"))
               (cut (lambda (head) (let ((c (clause-of a head))) (and c (cadr c)))))
               (a-cut? (lambda (c) (and (list? c) (pair? c)
                                        (for-all (lambda (p) (and (pair? p) (string? (car p)) (integer? (cdr p)))) c)))))
          (list (a-cut? (cut 'from)) (a-cut? (cut 'to)) (equal? (cut 'from) (cut 'to))))
        '(#t #t #f))
  (want "CF-4 equal cuts answer no item"
        (items-of (cf st "t1" "t1"))
        '()))

;; ---- CF-2 a rename; CF-3 a mode out of text; CF-12 the order ------------------
;; A file's mode is fixed once it has one: set refuses mode-mismatch, so a
;; text file cannot leave the projection that way and c.txt is not listed.
(let* ((s (file-store! '(("b.txt" . "two\n") ("c.txt" . "three\n") ("a.txt" . "one\n"))))
       (st (store-of s)) (b (file-at st "b.txt")) (c (file-at st "c.txt")))
  (ask st 'tag "t0")
  (ask st 'set b "path" "d.txt")
  (let ((mode (ask st 'set c "mode" "datum")))
    (ask st 'tag "t1")
    (want "CF-2 a rename answers removed plus added; CF-3 a text file's mode cannot change, so it is not listed; CF-12 sorted by path"
          (list (head-of mode) (items-of (cf st "t0" "t1")))
          '((error mode-mismatch) ((path "b.txt" removed) (path "d.txt" added))))))

;; ---- CF-5 a title only -------------------------------------------------------
(let* ((s (file-store! '(("a.txt" . "one\n")))) (st (store-of s)))
  (ask st 'tag "t0")
  (ask st 'set (car (children-at st "a.txt")) "title" "renamed")
  (ask st 'tag "t1")
  (want "CF-5 a title-only change answers no item"
        (items-of (cf st "t0" "t1"))
        '()))

;; ---- CF-6 two src edits that cancel -------------------------------------------
(let* ((s (split-store! "m.sc" "(define a 1)\n(define b 2)\n")) (st (store-of s))
       (kids (children-at st "m.sc")))
  (ask st 'tag "t0")
  (ask st 'write (car kids) "(define a 1)\n(define b" "--writer" "w1")
  (ask st 'write (cadr kids) " 2)\n" "--writer" "w1")
  (ask st 'commit "--writer" "w1")
  (ask st 'tag "t1")
  (want "CF-6 SETUP the file is two blocks and both changed"
        (list (length kids) (length (filter (lambda (l) (eq? (car l) 'changed)) (items-of (ask st 'diff "t0" "t1")))))
        '(2 2))
  (want "CF-6 two src changes that cancel across the boundary answer no item"
        (items-of (cf st "t0" "t1"))
        '()))

;; ---- CF-7 a swap of equal files ------------------------------------------------
(let* ((s (file-store! '(("x.txt" . "same\n") ("y.txt" . "same\n") ("p.txt" . "one\n") ("q.txt" . "two\n"))))
       (st (store-of s)) (x (file-at st "x.txt")) (y (file-at st "y.txt"))
       (p (file-at st "p.txt")) (q (file-at st "q.txt")))
  (ask st 'tag "t0")
  (ask st 'batch (format "((set ~s path \"y.txt\") (set ~s path \"x.txt\"))" x y))
  (ask st 'tag "t1")
  (ask st 'batch (format "((set ~s path \"q.txt\") (set ~s path \"p.txt\"))" p q))
  (ask st 'tag "t2")
  (want "CF-7 two equal files swapped answer no item"
        (items-of (cf st "t0" "t1"))
        '())
  (want "CF-7 two different files swapped are two content changes"
        (items-of (cf st "t1" "t2"))
        '((path "p.txt" content) (path "q.txt" content))))

;; ---- CF-8 a contested path; CF-9 an incomplete load ---------------------------
(printf "== CF-8..CF-10: refusals ==\n")
(define M "zzzzzzzz")
(define (mirror! st seq deps payload)
  (let ((bytes (encode-record seq 1757300000001 "peer" deps (storable-encode payload))))
    (car (log-publish! st M seq bytes (segment-sha bytes)))))
(let* ((s (file-store! '(("a.txt" . "one\n")))) (st (store-of s)) (a (file-at st "a.txt")))
  (ask st 'tag "t0")
  (ask st 'set a "path" "y.txt")
  (mirror! st 1 '() (list 'set a 'path "z.txt"))
  (ask st 'tag "t1")
  (want "CF-8 a contested path at one cut refuses with the projection's own name, unsafe-path"
        (head-of (cf st "t0" "t1"))
        '(error projection-invalid (reason unsafe-path)))
  (sh "chmod 000 " (quoted (writer-directory st M)))
  (let* ((answer (cf st "t0" "t0"))
         (exported (ask st 'export-code (string-append root "/cf9-export"))))
    (sh "chmod 700 " (quoted (writer-directory st M)))
    (want "CF-9 an incomplete load refuses as export-code does"
          (list (head-of answer) (head-of exported))
          '((error incomplete-reduction) (error incomplete-reduction)))))

;; ---- CF-10, CF-13 the directory-prefix rule -------------------------------------
(let* ((s (file-store! '(("x/y" . "one\n") ("w.txt" . "two\n") ("ab" . "three\n") ("abc" . "four\n"))))
       (st (store-of s)) (w (file-at st "w.txt")) (ab (file-at st "ab")))
  (ask st 'tag "t0")
  (want "CF-13 ab and abc do not collide"
        (head-of (cf st "t0" "t0"))
        'ok)
  (ask st 'set ab "path" "abc/d")
  (ask st 'tag "t1")
  (want "CF-13 abc and abc/d do: a directory prefix is p followed by /"
        (cf st "t0" "t1")
        '(error projection-invalid (reason path-collision) (paths "abc" "abc/d") (cut to)))
  (ask st 'set ab "path" "ab")
  (ask st 'set w "path" "x")
  (ask st 'tag "t2")
  (want "CF-10 x and x/y at one cut refuse as path-collision, naming the cut"
        (cf st "t2" "t0")
        '(error projection-invalid (reason path-collision) (paths "x" "x/y") (cut from))))

;; ---- CF-11 the marks; CF-11b Strasse ------------------------------------------------
(printf "== CF-11: spellings a volume would merge ==\n")
(define e-acute (string (integer->char #xE9)))
(define e-decomposed (string #\e (integer->char #x301)))
(define (marks st a b) (list (clause-of (cf st a b) 'case-ambiguous) (clause-of (cf st a b) 'normalisation-ambiguous)))
(let* ((names '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven"))
       (s (file-store! (map (lambda (n) (cons (string-append n ".txt") (string-append n "\n"))) names)))
       (st (store-of s))
       (ids (map (lambda (n) (cons n (file-at st (string-append n ".txt")))) names))
       (id (lambda (n) (cdr (assoc n ids))))
       (put! (lambda (n path) (ask st 'set (id n) "path" path)))
       (home! (lambda (n) (put! n (string-append n ".txt")))))
  (put! "one" "A.txt")
  (put! "two" "a.txt")
  (ask st 'tag "case")
  (want "CF-11 A and a at one cut answer normally, with (case-ambiguous ((A a)))"
        (list (head-of (cf st "case" "case")) (marks st "case" "case"))
        '(ok ((case-ambiguous (("A.txt" "a.txt"))) #f)))
  (home! "two")
  (put! "one" "A")
  (put! "three" "a/b")
  (ask st 'tag "dir")
  (want "CF-11 A and a/b: the file A and the directory a"
        (marks st "dir" "dir")
        '((case-ambiguous (("A" "a"))) #f))
  (home! "one")
  (home! "three")
  (put! "four" "D/x")
  (put! "five" "d/y")
  (ask st 'tag "dirs")
  (want "CF-11 D/x and d/y: two spellings of one directory"
        (marks st "dirs" "dirs")
        '((case-ambiguous (("D" "d"))) #f))
  (home! "four")
  (home! "five")
  (put! "six" (string-append "caf" e-acute ".txt"))
  (put! "seven" (string-append "caf" e-decomposed ".txt"))
  (ask st 'tag "nfd")
  (want "CF-11 an NFD/NFC pair answers with (normalisation-ambiguous ...) and no case mark"
        (marks st "nfd" "nfd")
        (list #f (list 'normalisation-ambiguous
                       (list (list-sort string<? (list (string-append "caf" e-acute ".txt")
                                                       (string-append "caf" e-decomposed ".txt")))))))
  (put! "six" (string-append (string (integer->char #xC9)) ".sc"))
  (put! "seven" (string-append e-decomposed ".sc"))
  (put! "eight" (string-append (string (integer->char #x390)) ".txt"))
  (put! "nine" (string-append (string (integer->char #x3AA) (integer->char #x301)) ".txt"))
  (put! "ten" (string-append "Stra" (string (integer->char #xDF)) "e.md"))
  (put! "eleven" "Strasse.md")
  (ask st 'tag "neither")
  (want "CF-11 U+00C9 against e plus U+0301, and U+0390 against U+03AA U+0301, carry neither mark; CF-11b nor do Strasse and its U+00DF spelling"
        (marks st "neither" "neither")
        '(#f #f)))

;; ---- CF-11 the reference under the switch, in a child ----------------------------
;; The child expands with THEOURGIA_DEGENERACY=1, so changed-files materialises
;; both cuts and compares. On a temporary volume that would merge A and a it
;; records (reference-unavailable case) instead; the fixture probes the same
;; directory the child's reference uses (its machine home) and asserts
;; whichever applies.
(define child-home (string-append root "/child-home"))
(sh "mkdir -p " (quoted child-home))
(define-values (probe-case probe-normalisation) (volume-insensitivity child-home))
(define (child-run program)
  (let ((script (string-append root "/child.ss")) (out (string-append root "/child.out")))
    (call-with-output-file script (lambda (p) (for-each (lambda (f) (write f p) (newline p)) program)) 'replace)
    (sh "THEOURGIA_DEGENERACY=1 THEOURGIA_HOME=" (quoted child-home) " scheme --script " (quoted script)
        " > " (quoted out) " 2> " (quoted (string-append root "/child.err")) " < /dev/null")
    (guard (e (#t 'UNREADABLE)) (call-with-input-file out read))))
;; c.txt becomes .c: a name the import's walk would skip, which the export
;; writes and the reference must read.
(let* ((s (file-store! '(("a.txt" . "one\n") ("b.txt" . "two\n") ("c.txt" . "three\n")))) (st (store-of s))
       (b (file-at st "b.txt")) (c (file-at st "c.txt")))
  (ask st 'tag "t0")
  (set-text! st "a.txt" "uno\n")
  (ask st 'set c "path" ".c")
  (ask st 'tag "t1")
  (ask st 'set b "path" "A.txt")
  (ask st 'tag "t2")
  (let ((r (child-run
             `((import (chezscheme) (only (theourgia rpc) rpc-dispatch))
               (define (answer a b) (guard (e (#t (list 'RAISED (if (condition? e) (condition-message e) e))))
                                      (rpc-dispatch ,st (list 'changed-files a b) "test")))
               (write (list (answer "t0" "t1") (answer "t1" "t2")))))))
    (want "CF-11 under the switch the reference compares and agrees where the cuts are unambiguous, a dot-named file included, and on the A/a cut records reference-unavailable exactly when the temporary volume folds case"
          (list (and (pair? r) (pair? (car r)) (items-of (car r)))
                (and (pair? r) (pair? (car r)) (car (car r)))
                (and (pair? r) (pair? (car r)) (assq 'reference-unavailable (cdr (car r))) #t)
                (and (pair? r) (pair? (cdr r)) (pair? (cadr r)) (car (cadr r)))
                (and (pair? r) (pair? (cdr r)) (pair? (cadr r)) (assq 'reference-unavailable (cdr (cadr r)))))
          (list '((path ".c" added) (path "a.txt" content) (path "c.txt" removed))
                'ok #f 'ok (cond ((and probe-case probe-normalisation) '(reference-unavailable both))
                                 (probe-case '(reference-unavailable case))
                                 (else #f))))))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nchanged-files complete\n" rows bad)
(exit (if (zero? bad) 0 1))
