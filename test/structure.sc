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

;; WHERE THE PARENTHESES ACTUALLY PUT THINGS.
;;
;; A MISSING CLOSER IS NOT A SYNTAX ERROR. The reader is perfectly happy:
;; the definitions after the short form simply become part of its body.
;; The first thing anyone hears about it is an "unbound identifier"
;; naming something defined much further down -- reported at the place
;; that USES it, never at the place that swallowed it.
;;
;; MEASURED, on daemon.sc, 2026-09-18: one `)` missing at the end of
;; `watch-loop` swallowed twenty-six later definitions, and Chez said
;; `unbound identifier directory-of at line 219` -- a hundred and eighty
;; lines from the cause. The whole file's parentheses still balanced,
;; because a second edit had one closer too many, so the cheapest check
;; anyone would reach for said nothing at all.
;;
;; TWO READINGS, AND THE SECOND IS THE ONE THAT LOCATES IT:
;;   * the depth at end of file says THAT something is wrong;
;;   * the depth at the start of every line that opens a top-level
;;     definition says WHERE -- the first definition not at the expected
;;     depth is the first one that was swallowed, and the form before it
;;     is the one that failed to close.
;;
;; NEVER: IT REFUSES TO JUDGE WHAT IT CANNOT READ. This scanner knows about
;; strings, escapes, line comments and character literals; it does NOT
;; know about `#|` block comments or `#;` datum comments. A file using
;; either is reported as NOT CHECKED, by name and with the reason, and
;; never quietly measured with the wrong rules -- a scanner that is wider
;; than its comment is worse than no scanner, because the place it is
;; wrong is the place nobody looks.
;;
;; NOTE: THIS IS A PORT OF `structure.py` (F9), rule for rule, and it was
;; accepted by reading the same output as the Python on the whole tree and
;; on copies with each kind of defect. Three things are kept as the Python
;; had them because they are what the readings were taken with, not
;; because they are right: a backslash in a string and a `#\` literal skip
;; the characters after them without counting a newline among them; a
;; carriage return, alone or before a newline, ends a line as the Python's
;; text mode made it; and "whitespace" at the start of a line is the
;; Python's `isspace`, which is Unicode's White_Space plus the four
;; separators U+001C to U+001F.
;;
;; NOTE: IT IMPORTS NOTHING BUT `(chezscheme)`, so it runs where no library
;; of this tree can be loaded -- which is the state it exists to diagnose.

(import (chezscheme))

;; ---- reading one file -----------------------------------------------------

(define (py-space? c)
  (or (char-whitespace? c)
      (let ((n (char->integer c))) (and (>= n #x1C) (<= n #x1F)))))

;; NOTE: CARRIAGE RETURNS BECOME NEWLINES, "\r\n" as one, as the Python's
;; universal-newline reading did before its scanner saw the text.
(define (unify-newlines text)
  (let ((n (string-length text)))
    (let loop ((i 0) (acc '()))
      (cond ((= i n) (list->string (reverse acc)))
            ((char=? (string-ref text i) #\return)
             (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline))
                 (loop (+ i 2) (cons #\newline acc))
                 (loop (+ i 1) (cons #\newline acc))))
            (else (loop (+ i 1) (cons (string-ref text i) acc)))))))

;; NEVER: AN UNDECODABLE BYTE RAISES, it is not replaced. A file this gate
;; cannot decode is one it cannot read, and it says so; reading a
;; replacement character in its place would be measuring a different file.
;;
;; NOTE: A LEADING BYTE-ORDER MARK IS KEPT AS A CHARACTER, because the Python
;; kept it: Chez's codec drops it, and a file that begins with one then
;; reads as a library here and as a script there. Measured on a copy: the
;; two gates disagreed about a swallowed definition in such a file until
;; the mark was put back. It is kept for the port's sake and named as a
;; gap, since the reader that loads the file does drop it.
(define (read-text path)
  (let* ((p (open-file-input-port path))
         (bytes (let ((b (get-bytevector-all p))) (close-port p)
                  (if (eof-object? b) (make-bytevector 0) b)))
         (text (bytevector->string bytes
                                   (make-transcoder (utf-8-codec) (eol-style none)
                                                    (error-handling-mode raise))))
         (marked (and (>= (bytevector-length bytes) 3)
                      (= (bytevector-u8-ref bytes 0) #xEF)
                      (= (bytevector-u8-ref bytes 1) #xBB)
                      (= (bytevector-u8-ref bytes 2) #xBF))))
    (unify-newlines
     (if (and marked
              (not (and (> (string-length text) 0)
                        (char=? (string-ref text 0) #\xFEFF))))
         (string-append (string #\xFEFF) text)
         text))))

(define (starts-at? text i word)
  (let ((n (string-length word)))
    (and (<= (+ i n) (string-length text))
         (string=? (substring text i (+ i n)) word))))

;; `(depth opens library? unsupported)`: the depth at end of file, one
;; `(line depth indent)` per `(define` that begins a line, whether a
;; `(library ` begins one, and the reason the file cannot be read, or #f.
(define (scan text)
  (let ((n (string-length text)))
    (let loop ((i 0) (line 1) (depth 0) (in-string #f) (in-comment #f)
               (at-line-start #t) (line-start 0) (opens '()) (library #f))
      (define (done unsupported) (list depth (reverse opens) library unsupported))
      (if (>= i n)
          (done #f)
          (let ((c (string-ref text i)))
            (cond
              ((char=? c #\newline)
               (loop (+ i 1) (+ line 1) depth in-string #f #t (+ i 1) opens library))
              ((and (not in-comment) (not in-string)
                    (or (starts-at? text i "#|") (starts-at? text i "#;")))
               (done (string-append "uses " (substring text i (+ i 2))
                                    ", which this scanner does not read")))
              (else
               ;; NOTE: THE INDENT IS MEASURED FROM THE START OF THE LINE, not
               ;; from wherever the scan happens to be. Reading it off the
               ;; current position makes every first-on-its-line `(define`
               ;; look like column zero, whatever its indent -- and the gate
               ;; then reports every internal definition in the tree as a
               ;; swallowed one. Measured: twenty-four such reports on a tree
               ;; with nothing wrong with it.
               ;;
               ;; NEVER: A LINE THAT STARTS INSIDE A STRING OR A COMMENT OPENS
               ;; NOTHING. A program written into a string literal put
               ;; `(define` at column 0 inside the literal, and this recorded
               ;; it as a top-level definition at the string's depth -- which
               ;; refused a whole suite at preflight (F84).
               (let* ((first (and at-line-start (not (py-space? c))))
                      (counts (and first (not in-string) (not in-comment)))
                      (opens (if (and counts (starts-at? text i "(define"))
                                 (cons (list line depth (- i line-start)) opens)
                                 opens))
                      (library (or library (and counts (starts-at? text i "(library "))))
                      (at-line-start (and at-line-start (not first))))
                 (cond
                   (in-comment
                    (loop (+ i 1) line depth in-string #t at-line-start line-start opens library))
                   (in-string
                    (cond ((char=? c #\\)
                           (loop (+ i 2) line depth #t #f at-line-start line-start opens library))
                          ((char=? c #\")
                           (loop (+ i 1) line depth #f #f at-line-start line-start opens library))
                          (else
                           (loop (+ i 1) line depth #t #f at-line-start line-start opens library))))
                   ((char=? c #\;)
                    (loop (+ i 1) line depth #f #t at-line-start line-start opens library))
                   ((char=? c #\")
                    (loop (+ i 1) line depth #t #f at-line-start line-start opens library))
                   ((and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\\))
                    (loop (+ i 3) line depth #f #f at-line-start line-start opens library))
                   (else
                    (loop (+ i 1) line
                          (cond ((char=? c #\() (+ depth 1))
                                ((char=? c #\)) (- depth 1))
                                (else depth))
                          #f #f at-line-start line-start opens library)))))))))))

(define (ordinal n)
  (cond ((and (>= (mod n 100) 10) (<= (mod n 100) 20)) "th")
        ((= (mod n 10) 1) "st")
        ((= (mod n 10) 2) "nd")
        ((= (mod n 10) 3) "rd")
        (else "th")))

;; `(problems unsupported)` for the text of one file.
(define (check-text text)
  (let* ((r (scan text))
         (depth (car r)) (opens (cadr r)) (library (caddr r)) (unsupported (cadddr r)))
    (if unsupported
        (list '() unsupported)
        (let ((problems
               (if (= depth 0)
                   '()
                   (list (format "parentheses do not balance: ~a ~a at end of file"
                                 (abs depth)
                                 (if (> depth 0) "unclosed" "too many closers")))))
              ;; A library body's definitions sit at depth 1 and indent 2; a
              ;; script's sit at depth 0 and indent 0. Only the ones at the
              ;; file's own shape are judged, so an internal `(define`
              ;; indented further is left alone.
              (want-indent (if library 2 0))
              (want-depth (if library 1 0)))
          (let loop ((opens opens) (nth 0))
            (cond
              ((null? opens) (list problems #f))
              ((not (= (caddr (car opens)) want-indent)) (loop (cdr opens) nth))
              ((= (cadr (car opens)) want-depth) (loop (cdr opens) (+ nth 1)))
              (else
               (let ((nth (+ nth 1)) (line (car (car opens))) (at (cadr (car opens))))
                 (list (append problems
                               (list (format "the ~a~a top-level define, at line ~a, is at depth ~a and not ~a -- the form before it never closed"
                                             nth (ordinal nth) line at want-depth)))
                       #f)))))))))

;; ---- the gate on itself ---------------------------------------------------
;;
;; NEVER: THE GATE PROVES IT CAN STILL GO RED, ON EVERY RUN. "163 files, 0
;; failures" is what a working gate prints and also what a gate that
;; matches nothing prints, and one of the two ways of being wrong here is
;; silent for as long as the tree happens to be sound. Measured: an earlier
;; version of the Python read the indent from the wrong place and reported
;; twenty-four sound files as broken; the opposite mistake would have
;; reported nothing, for ever, and looked exactly like this one does now.
;;
;; The samples are joined from one string per line, so no line of this
;; file begins inside them.
(define (lines . ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))

(define sound
  (lines "(library (probe)"
         "  (export a)"
         "  (import (rnrs))"
         "  (define (a x)"
         "    (+ x 1))"
         "  (define (b y)"
         "    (* y 2)))"))

(define swallowed
  (lines "(library (probe)"
         "  (export a)"
         "  (import (rnrs))"
         "  (define (a x)"
         "    (+ x 1)"
         "  (define (b y)"
         "    (* y 2)))"))

(define (misplaced opens)
  (filter (lambda (o) (and (= (caddr o) 2) (not (= (cadr o) 1)))) opens))

(define (self-check)
  (let* ((s (scan sound)) (w (scan swallowed)))
    (append
     (if (or (cadddr s) (not (= (car s) 0)))
         (list (format "the sound sample does not read as sound (depth ~a)" (car s)))
         '())
     (if (caddr s) '() (list "the sound sample was not recognised as a library"))
     (if (null? (misplaced (cadr s)))
         '()
         (list "the sound sample reports a definition at the wrong depth"))
     (if (null? (misplaced (cadr w)))
         (list "a definition swallowed by a short form was NOT detected -- this gate can no longer fail and says nothing")
         '()))))

;; ---- which files ------------------------------------------------------------

(define (split-path s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s))
           (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\/)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else (loop (+ i 1) start acc)))))

;; The directory holding this script, absolute, with `.` and `..` taken out
;; by their spelling, as the Python's `abspath` did: no link is followed.
(define (directory-of-script)
  (let* ((given (car (command-line)))
         (abs (if (and (> (string-length given) 0) (char=? (string-ref given 0) #\/))
                  given
                  (string-append (current-directory) "/" given)))
         (parts (let loop ((ps (split-path abs)) (acc '()))
                  (cond ((null? ps) (reverse acc))
                        ((member (car ps) '("" ".")) (loop (cdr ps) acc))
                        ((string=? (car ps) "..") (loop (cdr ps) (if (null? acc) acc (cdr acc))))
                        (else (loop (cdr ps) (cons (car ps) acc)))))))
    (let ((dir (if (null? parts) '() (reverse (cdr (reverse parts))))))
      (apply string-append (if (null? dir) (list "/") (map (lambda (p) (string-append "/" p)) dir))))))

(define (last-part path) (car (reverse (split-path path))))

(define (parent-of path)
  (let ((parts (reverse (cdr (reverse (cdr (split-path path)))))))
    (if (null? parts) "/" (apply string-append (map (lambda (p) (string-append "/" p)) parts)))))

(define (ends-with? s e)
  (let ((n (string-length s)) (m (string-length e)))
    (and (>= n m) (string=? (substring s (- n m) n) e))))

;; `(path . shown)` for every source file this gate reads.
;;
;; NOTE: AND THE SUBDIRECTORIES THAT HOLD SOURCES. `mcp/` was outside this
;; list until the shell moved into it, so the one file added that batch was
;; the one file this gate could not see -- a gate that scans "the tree" and
;; means "two directories" is wrong in the place nobody looks. Named rather
;; than walked: a walk would pull in scratch directories and pinned copies
;; that are not this tree's sources.
(define (source-files here parent)
  (apply append
         (map (lambda (d)
                (let ((dir (car d)) (prefix (cdr d)))
                  (if (file-directory? dir)
                      (map (lambda (name) (cons (string-append dir "/" name)
                                                (string-append prefix name)))
                           (sort string<?
                                 (filter (lambda (name) (or (ends-with? name ".sc")
                                                            (ends-with? name ".sls")))
                                         (directory-list dir))))
                      '())))
              (list (cons parent "")
                    (cons here (string-append (last-part here) "/"))
                    (cons (string-append parent "/mcp") "mcp/")))))

(define (reason-of e)
  (cond ((and (message-condition? e) (irritants-condition? e))
         (format "~a ~s" (condition-message e) (condition-irritants e)))
        ((message-condition? e) (condition-message e))
        (else (format "~s" e))))

;; ---- the run ----------------------------------------------------------------

(define (main)
  (let* ((here (directory-of-script))
         (parent (parent-of here))
         (files (source-files here parent)))
    (if (null? files)
        (begin
          (printf "FAIL structure: no Scheme sources found to check\n")
          (printf "1 failures\n")
          (printf "structure complete\n")
          1)
        (let ((failures 0) (skipped '()))
          (for-each (lambda (problem)
                      (printf "FAIL structure gate itself: ~a\n" problem)
                      (set! failures (+ failures 1)))
                    (self-check))
          (for-each
           (lambda (f)
             (let ((result (guard (e (#t (list 'unreadable (reason-of e))))
                             (check-text (read-text (car f))))))
               (cond
                 ((eq? (car result) 'unreadable)
                  (printf "FAIL ~a: could not be read -- ~a\n" (cdr f) (cadr result))
                  (set! failures (+ failures 1)))
                 ((cadr result)
                  (set! skipped (cons (cons (cdr f) (cadr result)) skipped)))
                 (else
                  (for-each (lambda (problem)
                              (printf "FAIL ~a: ~a\n" (cdr f) problem)
                              (set! failures (+ failures 1)))
                            (car result))))))
           files)
          (printf "checked ~a files\n" (- (length files) (length skipped)))
          ;; NEVER: A SKIP IS NAMED, WITH ITS REASON. A count of what ran is
          ;; silent about the file it did not read, and a file this gate
          ;; cannot read is exactly where a broken one would sit unnoticed.
          (if (null? skipped)
              (printf "not checked: none\n")
              (for-each (lambda (s) (printf "NOT CHECKED ~a: ~a\n" (car s) (cdr s)))
                        (reverse skipped)))
          (printf "~a failures\n" failures)
          (printf "structure complete\n")
          (if (> failures 0) 1 0)))))

(exit (main))
