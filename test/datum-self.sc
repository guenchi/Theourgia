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

;; THE DATUM READER ACCEPTS EVERY ONE OF THEOURGIA'S OWN SOURCES (F51).
;;
;; Measured 2026-09-20 on 44 files: 34 imported with `import-code --datum`,
;; 10 refused. Six were refused as `unbalanced` at their last byte: the
;; ledger in source-lex.sc had lost count of the depth somewhere earlier,
;; at a `#\\` followed by a delimiter, whose name was scanned with the escape
;; rule of an atom and swallowed the delimiter. Four were refused as
;; `prefix-limit` part of the way through: the limit counted every quote in
;; the file rather than a run of them.
;;
;; The CENSUS row imports each source on its own and names every refusal. The
;; ROUND TRIP rows compare the forms Chez reads from each export with the
;; forms it reads from the source. F51-3 and F51-4 pin the two causes on files
;; small enough to read.
;;
;; NOTE: THE ROUND TRIP COMPARES FORMS, NOT BYTES.
;; `export-code --datum` reprints: it writes `#!chezscheme`, a projection
;; header, an `@block` marker before every form, and every form through
;; datum-print, and a `;` comment inside a form is dropped at import
;; (datum-project.sc, export-datum). No export can equal its source byte for
;; byte, whatever the reader does, so the byte comparison is printed as a
;; reading and is not asserted.
;;
;; NOTE: A PROGRAM IS EXPORTED AS A PROGRAM. Import records a file whose
;; first form is `(import ...)` as `(shape . program)` on its library block,
;; and export writes it back as its import form and its forms, with no
;; `(library ...)` around them. The round trip therefore compares programs as
;; well as libraries; the programs are still named and pinned, so a new one is seen.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia datum-code) datum-source-read)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-datum state-read))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (want-1 label (caught got) (caught expected))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define tree (string-append script-dir "/.."))

(define scratch
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/datum-self-" (number->string (get-process-id))))
(when (file-exists? scratch)
  (assertion-violation 'datum-self "scratch directory already exists" scratch))
(system (string-append "mkdir -p " scratch))
(putenv "THEOURGIA_HOME" (string-append scratch "/home"))

(define (shell-quote s)
  (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                                 (string->list s)))
                 "'"))
(define (sh . parts) (system (apply string-append parts)))
(define (file-bytes path)
  (call-with-port (open-file-input-port path)
    (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) (make-bytevector 0) b)))))
(define (file-lines path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '()))
                  (let ((l (get-line p)))
                    (if (eof-object? l) (reverse acc) (loop (cons l acc))))))))
(define (ends-with? s e)
  (let ((n (string-length s)) (m (string-length e)))
    (and (>= n m) (string=? (substring s (- n m) n) e))))
(define (starts-with? s p)
  (and (>= (string-length s) (string-length p)) (string=? p (substring s 0 (string-length p)))))
(define (contains-slash? s) (exists (lambda (c) (char=? c #\/)) (string->list s)))

;; ---- the population ---------------------------------------------------------------
;;
;; NEVER: NOT A HAND LIST. The product's own sources are the `.sc` files at the
;; tree's root and under mcp/; test/ is the fixtures, not the product.
;; `git ls-files` answers where the tree is a checkout; a copy made for a
;; suite or a gate has no .git, and there the directories are listed. The row
;; says which it used and how many it found, and fewer than 45 is red: a
;; listing that finds less than the tree is known to hold is broken, not clean.
(define (own-source? rel)
  (and (ends-with? rel ".sc")
       (or (not (contains-slash? rel))
           (and (starts-with? rel "mcp/") (not (contains-slash? (substring rel 4 (string-length rel))))))))

(define listing
  (if (file-exists? (string-append tree "/.git"))
      (let ((out (string-append scratch "/ls-files.txt")))
        (if (= 0 (sh "git -C " (shell-quote tree) " ls-files > " (shell-quote out) " 2>&1"))
            (cons 'git-ls-files (list-sort string<? (filter own-source? (file-lines out))))
            (cons 'git-ls-files-failed '())))
      (cons 'directory-listing
            (list-sort string<?
                       (append (filter own-source? (directory-list tree))
                               (if (file-directory? (string-append tree "/mcp"))
                                   (filter own-source?
                                           (map (lambda (f) (string-append "mcp/" f))
                                                (directory-list (string-append tree "/mcp"))))
                                   '()))))))
(define sources (cdr listing))

(printf "== F51: the population ==\n")
(printf "   (listed by ~a: ~a files)\n" (car listing) (length sources))
(want "F51-0 the population is listed from the tree and holds at least the 45 own sources, mcp/server.sc among them"
      (list (memq (car listing) '(git-ls-files directory-listing))
            (>= (length sources) 45)
            (and (member "mcp/server.sc" sources) #t))
      (list (memq (car listing) '(git-ls-files directory-listing)) #t #t))

;; ---- F51-1: every own source imports -------------------------------------------------
;;
;; Each file on its own, in a fresh store, so one refusal cannot hide another.
(define n 0)
(define (fresh!)
  (set! n (+ n 1))
  (let ((d (string-append scratch "/f" (number->string n))))
    (sh "mkdir -p " (shell-quote (string-append d "/store")) " " (shell-quote (string-append d "/in")))
    d))

(define (answer-head a) (and (pair? a) (car a)))

;; `(rel dir answer)` for every source.
(define imported
  (map (lambda (rel)
         (let* ((d (fresh!))
                (store (string-append d "/store"))
                (in (string-append d "/in")))
           (rpc-dispatch store '(init) "test")
           (sh "mkdir -p " (shell-quote (string-append in "/" (if (starts-with? rel "mcp/") "mcp" "."))))
           (sh "cp " (shell-quote (string-append tree "/" rel)) " " (shell-quote (string-append in "/" rel)))
           (list rel d (rpc-dispatch store (list 'import-code in "--datum") "test"))))
       sources))

(define (refusal-of a)
  (let ((clause (lambda (k) (let ((c (and (list? a) (assq k (filter pair? a))))) (and c (cadr c))))))
    (list (clause 'reason) (clause 'character-offset))))

(printf "== F51-1: every own source imports with import-code --datum ==\n")
(want "F51-1 CENSUS every own source imports with import-code --datum (refused: file, reason, offset)"
      (fold-left (lambda (acc r)
                   (if (eq? (answer-head (caddr r)) 'ok)
                       acc
                       (append acc (list (cons (car r) (refusal-of (caddr r)))))))
                 '() imported)
      '())

;; ---- ROUND TRIP: what was imported comes back -------------------------------------------
(define (forms-of path)
  (guard (e (#t (list 'UNREADABLE (if (message-condition? e) (condition-message e) e))))
    (call-with-input-file path
      (lambda (p) (let loop ((acc '()))
                    (let ((x (read p)))
                      (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))

(define (first-form-head path)
  (let ((fs (forms-of path)))
    (and (pair? fs) (pair? (car fs)) (car (car fs)))))

(define (first-byte-difference a b)
  (let ((n (min (bytevector-length a) (bytevector-length b))))
    (let loop ((i 0))
      (cond ((= i n) (if (= (bytevector-length a) (bytevector-length b)) #f i))
            ((= (bytevector-u8-ref a i) (bytevector-u8-ref b i)) (loop (+ i 1)))
            (else i)))))

;; `(rel kind same? byte-offset)` for every imported source: kind is library or
;; program by the source's first form; same? is the form comparison.
(define round-trips
  (fold-left
    (lambda (acc r)
      (if (not (eq? (answer-head (caddr r)) 'ok))
          acc
          (let* ((rel (car r)) (d (cadr r))
                 (out (string-append d "/out"))
                 (store (string-append d "/store"))
                 (source (string-append tree "/" rel))
                 (exported (string-append out "/" rel))
                 (answer (begin (sh "mkdir -p " (shell-quote out))
                                (rpc-dispatch store (list 'export-code out "--datum") "test")))
                 (kind (if (eq? (first-form-head source) 'library) 'library 'program)))
            (append acc
                    (list (list rel kind
                                (and (eq? (answer-head answer) 'ok)
                                     (file-exists? exported)
                                     (equal? (forms-of source) (forms-of exported)))
                                (and (file-exists? exported)
                                     (first-byte-difference (file-bytes source) (file-bytes exported)))))))))
    '() imported))

(printf "== ROUND TRIP: an imported source exports to the same forms ==\n")
(for-each (lambda (t)
            (printf "   ~a ~a: forms ~a; first differing byte ~a~a\n"
                    (cadr t) (car t) (if (caddr t) "equal" "DIFFER") (cadddr t)
                    ""))
          round-trips)

(want "ROUND TRIP every imported source, library or program, exports to forms equal? to its source's (differing: file)"
      (map car (filter (lambda (t) (not (caddr t))) round-trips))
      '())

;; NEVER: THE PROGRAMS ARE PINNED BY NAME. A source the classification newly
;; calls a program turns this row red, so none joins the set unseen.
(want "PROGRAMS the sources classified as programs are exactly these"
      (map car (filter (lambda (t) (eq? (cadr t) 'program)) round-trips))
      '("core.sc" "eval-runner-exec.sc" "eval-worker.sc" "mcp/server.sc" "theourgia.sc" "theourgiad.sc"))

;; THE EXPORT'S FIRST FORM says which shape it was written as -- `import`
;; for a program, `library` for a library (TWIN: the wrapper is still there).
(define (exported-head t)
  (let ((r (assoc (car t) imported)))
    (and r (first-form-head (string-append (cadr r) "/out/" (car t))))))
(want "PROGRAM SHAPE every program is exported with its import form first and no library wrapper"
      (map (lambda (t) (list (car t) (exported-head t)))
           (filter (lambda (t) (eq? (cadr t) 'program)) round-trips))
      (map (lambda (t) (list (car t) 'import))
           (filter (lambda (t) (eq? (cadr t) 'program)) round-trips)))
(want "PROGRAM SHAPE TWIN every library is still exported inside its (library ...) wrapper"
      (filter (lambda (t) (not (eq? (exported-head t) 'library)))
              (filter (lambda (t) (eq? (cadr t) 'library)) round-trips))
      '())

;; AN EXPORTED PROGRAM COMES BACK IN. It carries a header now, which an
;; unwrapped file could not; imported into the store it came from, it
;; answers ok.
(want "PROGRAM SHAPE PIN: an exported program re-imports into its own store (green too when programs were exported as libraries)"
      (let* ((r (assoc "theourgia.sc" imported))
             (d (cadr r)))
        (answer-head (rpc-dispatch (string-append d "/store") (list 'import-code (string-append d "/out") "--datum") "test")))
      'ok)

;; NEVER: ONLY THE SHAPE THE IMPORT WRITES IS RESET. A library block may
;; carry a `shape` field the import did not write -- a caller can set any
;; field that is not reserved. Re-importing that library's own export must
;; leave it as it is, not set it to `library` because it is not absent.
(want "PROGRAM SHAPE a library's shape field that the import did not write survives a re-import"
      (let* ((r (assoc "text-code.sc" imported))
             (d (cadr r))
             (store (string-append d "/store"))
             (id (let ((st (open-and-reduce store)))
                   (find (lambda (id) (let* ((b (state-read st id)) (f (and b (assq 'fields b))))
                                        (and f (equal? (assq 'kind (cdr f)) '(kind . library)))))
                         (map cadr (state-datum st)))))
             (out (string-append d "/out-legacy")))
        (rpc-dispatch store (list 'set id "shape" "legacy") "test")
        (sh "mkdir -p " (shell-quote out))
        (rpc-dispatch store (list 'export-code out "--datum") "test")
        (rpc-dispatch store (list 'import-code out "--datum") "test")
        (let* ((b (state-read (open-and-reduce store) id))
               (p (and b (assq 'shape (cdr (assq 'fields b))))))
          (and p (cdr p))))
      "legacy")

;; NEVER: A HEADED FILE THAT IS NEITHER A LIBRARY NOR A PROGRAM IS STILL
;; REFUSED. A program's header, over forms with no
;; `(import ...)` first, is answered missing-library-wrapper, as it always was.
(want "PROGRAM SHAPE a headed file that is neither a library nor a program is refused as missing-library-wrapper"
      (let* ((r (assoc "theourgia.sc" imported))
             (d (cadr r))
             (header (let loop ((ls (file-lines (string-append d "/out/theourgia.sc"))))
                       (cond ((null? ls) #f)
                             ((starts-with? (car ls) ";; @file") (car ls))
                             (else (loop (cdr ls))))))
             (bad (string-append d "/bad")))
        (sh "mkdir -p " (shell-quote bad))
        (call-with-output-file (string-append bad "/theourgia.sc")
          (lambda (p) (put-string p (string-append "#!chezscheme\n" (or header ";; no header found") "\n(define x 1)\n")))
          'truncate)
        (refusal-of (rpc-dispatch (string-append d "/store") (list 'import-code bad "--datum") "test")))
      '(missing-library-wrapper #f))

;; ---- F51-3: the prefix limit is on a run ---------------------------------------------
(define (read-answer text)
  (guard (e ((and (pair? e) (eq? (car e) 'error)) e)
            (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
    (length (datum-source-read (string->utf8 text)))))

(define three-hundred-quoted
  (string-append "(list"
                 (apply string-append (map (lambda (i) " 'x") (iota 300)))
                 ")"))
(define run-of-257 (string-append "(list " (make-string 257 #\') "x)"))

(printf "== F51-3: the prefix limit counts a run, not the file ==\n")
(want "F51-3 a form with 300 separate 'x atoms reads as one datum"
      (read-answer three-hundred-quoted)
      1)
(want "F51-3 a run of 257 consecutive quotes is refused as prefix-limit at the run's first quote"
      (read-answer run-of-257)
      '(error bad-source (reason prefix-limit) (character-offset 6)))
(want "F51-3 a run of 257 unquote-splicings (,@) is refused as prefix-limit at the run's first comma"
      (read-answer (string-append "(list " (apply string-append (make-list 257 ",@")) "x)"))
      '(error bad-source (reason prefix-limit) (character-offset 6)))
(want "F51-3 CONTROL a run of 256 is accepted"
      (read-answer (string-append "(list " (make-string 256 #\') "x)"))
      1)

;; NOTE: A FILE OF MANY SEPARATE DISCARDS (code-3, sb4 r2). Each `#;` is its
;; own run, so the prefix limit no longer refuses the file; what remained was
;; the cost of finding where each discarded datum ends, which copied the rest
;; of the text once per discard. The row reads 20,000 separate `#;x` lines
;; before one form; on the base, which counted every prefix in the file, it
;; is refused as prefix-limit. The time is printed as a reading and is not
;; asserted.
(define twenty-thousand-discards
  (string-append (apply string-append (make-list 20000 "#;x\n")) "(define y 1)\n"))
(let* ((t0 (real-time))
       (answer (read-answer twenty-thousand-discards))
       (t1 (real-time)))
  (printf "   (reading: 20,000 separate discards read in ~a ms)\n" (- t1 t0))
  (want "F51-3 a file of 20,000 separate #;x discards before one form reads as one form"
        answer
        1))

;; ---- F51-4: a character literal's name has no escape --------------------------------
(printf "== F51-4: #\\\\ and its neighbours are one character each ==\n")
(define (forms-read text)
  (guard (e ((and (pair? e) (eq? (car e) 'error)) e)
            (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
    (map car (datum-source-read (string->utf8 text)))))
(want "F51-4 (list #\\\\) reads as one datum, the backslash character"
      (forms-read "(list #\\\\)")
      (list (list 'list #\\)))
(want "F51-4 TWIN (list #\\\\ #\\a) reads as the backslash and #\\a, two data (green on the base too: the swallowed space did not move the depth)"
      (forms-read "(list #\\\\ #\\a)")
      (list (list 'list #\\ #\a)))
(want "F51-4 PIN (list #\\;) reads as one datum"
      (forms-read "(list #\\;)")
      (list (list 'list #\;)))
(want "F51-4 PIN (list #\\\") reads as one datum"
      (forms-read "(list #\\\")")
      (list (list 'list #\")))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "rm -rf " (shell-quote scratch)))
(printf "datum-self complete\n")
(exit (if (zero? bad) 0 1))
