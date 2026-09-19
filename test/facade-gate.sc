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

;; ONE SEAM TO IGROPYR, AND THIS COUNTS IT.
;;
;; The rule is that every use of igropyr goes through a facade of this
;; core's own, so that replacing the dependency -- or vendoring it again,
;; as Z did -- touches those files and no caller. A rule kept in
;; somebody's head is a rule until the first hurry; this reads the tree.
;;
;; NEVER: IT DOES NOT GREP, IT READS EACH FILE AS DATA -- and the walk that
;; does so lives in `import-walk.scm`, beside this file, because
;; `closures.sc` needs the same one. Why it cannot be a grep, and why it
;; is `load`ed rather than `include`d, are written there.
;;
;; WHAT IT WALKS INTO. An import may name a library directly or wrap it
;; in `only` / `except` / `rename` / `prefix`, and `meta-cond` puts whole
;; import lists behind a branch that this host may not take -- an import
;; in the branch that is NOT taken here is still a use of igropyr in this
;; source, so the walk goes into every branch rather than evaluating any.
;;
;; FOUR DIRECTIONS, WHICH IS WHAT MAKES THE SET OF RULES CLOSED:
;;
;;   a name in facades.sexp  -> that root file exists
;;   a name in facades.sexp  -> it really does import igropyr
;;   a root file imports it  -> its name is in facades.sexp
;;   a name in facades.sexp  -> no copied definition left in it  (facades.sc)
;;
;; The first three are here. Two sides -- the list and the tree -- and
;; both directions of each; there is no fifth way for them to disagree.

(import (chezscheme))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
;; BOTH SIDES OF A ROW ARE GUARDED. A `want` written as a procedure
;; evaluates its arguments before the call, so an argument that raises
;; kills the fixture: the row never prints, and what the suite sees is a
;; missing sentinel rather than a red row naming the question that could
;; not be answered. This directory's `run-fixtures.sh` counts fixtures
;; whose `want` is a bare procedure for exactly that reason.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; THE ROOT IS FOUND FROM THIS SCRIPT, not from the current directory:
;; the runner starts every fixture from `test/`.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/cli.sc")) up script-dir)))

(define facade-names
  (call-with-input-file (string-append script-dir "/facades.sexp") read))

(load (string-append script-dir "/import-walk.scm"))

(define (uses-igropyr? name)
  (pair? (imports-of-file 'igropyr (string-append root "/" name))))

;; ---- the scan reaches into subdirectories, and it has to ---------------
;;
;; NEVER: A NON-RECURSIVE SCAN HAS A DOOR IN IT. `source-files` lists one
;; directory; a root library that imports a nested helper, and lets THAT
;; file import igropyr, leaves the set of root importers unchanged. All
;; four directions stay satisfied and the seam is gone. Nothing in this
;; tree does that today -- which is exactly when a hole is cheap to
;; close.
;;
;; NOTE: `test/` IS OUT OF SCOPE, ON PURPOSE. Fixtures are not the library:
;; some of them import igropyr in order to MEASURE it (`q10`, `cli3`),
;; and requiring them to go through a facade would mean measuring the
;; facade instead of the thing. The rule is about what the core ships.
;; NOTE: THE RECURSION CARRIES ITS DIRECTORY. Written as a named `let` over
;; entries alone, the inner call went back into the loop with the
;; SUBDIRECTORY's entries and the OUTER directory's path -- every nested
;; name was joined to the wrong parent, and the run stopped answering.
;; Measured: it had to be killed. A walker that takes the directory as
;; an argument cannot make that mistake.
;; NOTE: THE SAME EXTENSIONS THE SHARED WALKER KNOWS. This listed `.ss`
;; alone while `import-walk.scm` recognises `.ss`, `.sls`, `.sc` and
;; `.scm` -- so a nested `helper.sls` importing igropyr passed the deep
;; scan untouched, which is the hole this scan was added to close. One
;; list, read from the walker's own definition.
(define (source-suffix? name)
  (exists (lambda (suffix)
            (let ((n (string-length name)) (k (string-length suffix)))
              (and (> n k) (string=? suffix (substring name (- n k) n)))))
          source-suffixes))

(define (sources-under dir)
  (let loop ((entries (directory-list dir)) (out '()))
    (cond
      ((null? entries) out)
      ((member (car entries) '("." ".." "test" ".git")) (loop (cdr entries) out))
      (else
       (let ((path (string-append dir "/" (car entries))))
         (cond
           ;; NEVER: SYMLINKS ARE NOT DESCENDED, AND THE REASON IS IN THIS
           ;; DIRECTORY. The checkout carries a self-link -- `theourgia`
           ;; pointing at `.`, so that `(theourgia x)` resolves from the
           ;; tree itself -- and a walk that follows it re-enters the
           ;; same directory for ever. Measured twice: the first version
           ;; had to be killed, the second reported 198 importers for a
           ;; tree with six, which is the same fault with a bound on it.
           ((and (file-directory? path) (not (file-symbolic-link? path)))
            (loop (cdr entries) (append (sources-under path) out)))
           ((source-suffix? (car entries)) (loop (cdr entries) (cons path out)))
           (else (loop (cdr entries) out))))))))

(define scanned (source-files root))
(define importers (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                             (map stem (filter uses-igropyr? scanned))))
(define declared (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                            facade-names))

;; THE INSTRUMENT'S OWN FIRST READING. A walk that found nothing would
;; make the comparison below vacuous, and "no file imports igropyr" is
;; exactly what a broken reader produces.
(want "FG-00 the scan reached the root sources"
      (> (length scanned) 20) #t)
(want "FG-00 and it found imports to compare at all"
      (> (length importers) 0) #t)

(want "FG-01 the files importing igropyr are exactly the declared facades"
      importers declared)

;; KEY: THE SAME RULE, ASKED OF EVERY SOURCE THE CORE SHIPS. Root files are
;; the population above; this one is "any file at all, however deep".
;; With the tree flat they agree -- and the day somebody adds a
;; subdirectory, this is the row that keeps the answer true.
(define deep-importers
  (list-sort
    string<?
    (map (lambda (p) (let* ((cut (let loop ((i (- (string-length p) 1)))
                                   (cond ((< i 0) -1)
                                         ((char=? (string-ref p i) #\/) i)
                                         (else (loop (- i 1))))))
                            (base (substring p (+ cut 1) (string-length p))))
                       base))
         (filter (lambda (p) (pair? (imports-of-file 'igropyr p)))
                 (sources-under root)))))

(want "FG-03 no source outside the declared facades imports igropyr, at any depth"
      (filter (lambda (b) (not (memq (string->symbol (substring b 0 (- (string-length b) 3)))
                                     facade-names)))
              deep-importers)
      '())

;; NOTE: AND THE DEEP SCAN REALLY LOOKED. A walker that found nothing would
;; satisfy the row above by finding no importers at all.
(want "FG-03 TWIN: the deep scan found the facades themselves"
      (length deep-importers) (length facade-names))

(want "FG-02 every declared facade exists as a root source"
      (filter (lambda (n) (not (file-exists? (string-append root "/" (symbol->string n) ".sc"))))
              facade-names)
      '())

(printf "rows: ~a\n~a failures\nfacade-gate complete\n" rows failures)
(exit (if (zero? failures) 0 1))
