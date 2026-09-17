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
;; ⛔ IT DOES NOT GREP, IT READS EACH FILE AS DATA -- and the walk that
;; does so lives in `import-walk.scm`, beside this file, because
;; `closures.ss` needs the same one. Why it cannot be a grep, and why it
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
;;   a name in facades.sexp  -> no copied definition left in it  (facades.ss)
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
    (if (file-exists? (string-append up "/cli.ss")) up script-dir)))

(define facade-names
  (call-with-input-file (string-append script-dir "/facades.sexp") read))

(load (string-append script-dir "/import-walk.scm"))

(define (uses-igropyr? name)
  (pair? (imports-of-file 'igropyr (string-append root "/" name))))

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

(want "FG-02 every declared facade exists as a root source"
      (filter (lambda (n) (not (file-exists? (string-append root "/" (symbol->string n) ".ss"))))
              facade-names)
      '())

(printf "rows: ~a\n~a failures\nfacade-gate complete\n" rows failures)
(exit (if (zero? failures) 0 1))
