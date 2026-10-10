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

;; THE FORWARDING LIBRARIES EXPORT WHAT THE DESIGN SAYS, EXACTLY.
;;
;; `facade-gate.sc` answers "who may touch igropyr". This answers the
;; other half: WHAT those facades hand on. A facade that exports one
;; extra name has widened the seam -- the name is now something a caller
;; can depend on, and replacing the dependency stops being a change to
;; these files alone.
;;
;; NEVER: THE EXPECTED NAMES ARE NOT WRITTEN HERE. They are read from
;; `facade-exports.sexp`, which is the copy of the design's table; a cell
;; that restated the list would be checking its own copy of it. What this
;; file contributes is the READING: the library's own `(export ...)`
;; form, taken from the source as DATA, so a rename or an addition is
;; seen whether or not anything imports it.
;;
;; NOTE: COMPARED IN BOTH DIRECTIONS. "Every declared name is exported" is
;; silent about the extra one, and the extra one is the whole point.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/core.sc")) up script-dir)))

(define declared (call-with-input-file (string-append script-dir "/facade-exports.sexp") read))

(define (forms-of path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((out '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse out) (loop (cons x out))))))))

;; THE LIBRARY'S OWN EXPORT FORM, from the source read as data.
;;
;; NOTE: `(export a b (rename (c d)))` IS LEGAL R6RS, so a renamed export
;; contributes the name it is EXPORTED AS -- the second one. Taking the
;; head of the form would record the internal name and call the seam
;; clean while a different name leaked.
(define (exports-of name)
  (let ((path (string-append root "/" (symbol->string name) ".sc")))
    (if (not (file-exists? path))
        'no-such-library
        (let loop ((fs (forms-of path)))
          (cond
            ((null? fs) 'no-library-form)
            ((and (pair? (car fs)) (eq? 'library (caar fs)))
             (let scan ((body (cddr (car fs))))
               (cond
                 ((null? body) 'no-export-form)
                 ((and (pair? (car body)) (eq? 'export (caar body)))
                  ;; NOTE: A `rename` FORM CARRIES ANY NUMBER OF PAIRS.
                  ;; Taking only the first let a second renamed export --
                  ;; `(rename (a b) (c leaked))` -- escape the comparison
                  ;; entirely, which is the one thing this cell exists to
                  ;; prevent. Reported by codex.
                  (apply append
                    (map (lambda (x)
                           (if (and (pair? x) (eq? 'rename (car x)))
                               (map cadr (cdr x))
                               (list x)))
                         (cdr (car body)))))
                 (else (scan (cdr body))))))
            (else (loop (cdr fs))))))))

(define (sorted xs)
  (if (list? xs) (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) xs) xs))

;; ---- FE-00 the instrument's own first reading ----------------------------
;;
;; A table that parsed to nothing would make every row below vacuous,
;; and "no names declared" is what a broken read produces.

(want "FE-00 the table names four libraries"
      (map car declared) '(sched net proc json))
;; NOTE: NOT THE COUNTS. Writing `(10 10 7)` here put a second copy of the
;; table in the cell -- the design changed `net` from ten names to nine,
;; the table followed, and this row went red about nothing. What it is
;; for is a table that parsed to nothing, so it asks that and no more.
(want "FE-00 and every library declares at least one name"
      (map (lambda (e) (> (length (cdr e)) 0)) declared) '(#t #t #t #t))

;; ---- FE-01..03 each library exports exactly its row ----------------------

(for-each
  (lambda (entry)
    (let* ((name (car entry))
           (want-names (sorted (cdr entry)))
           (got (exports-of name)))
      (want (string-append "FE-01 (theourgia " (symbol->string name) ") exports exactly the declared set")
            (sorted got) want-names)))
  declared)

;; ---- FE-02 the name that must NOT be there ------------------------------
;;
;; KEY: NAMED, BECAUSE IT IS THE ONE SOMEBODY WILL ADD. `conn-peer-ip` is
;; in igropyr and answers #f on a unix socket; a daemon that reached for
;; it would be identifying its clients by a value that is always #f. The
;; row above already refuses it as "one extra name", but a reader who
;; hits THIS row learns why it is not there.

;; NOTE: IT ASSERTS THE READING FIRST. Written as "conn-peer-ip is not
;; among them", this row is GREEN WHEN THE LIBRARY DOES NOT EXIST --
;; `exports-of` answers a symbol, the membership test is false, and a
;; row about a leak passes because there is nothing to leak from.
;; Measured that way before this was fixed. So the row reads a list AND
;; the absence, and says which half failed.
(want "FE-02 net does not hand on conn-peer-ip"
      (let ((got (exports-of 'net)))
        (list (list? got) (and (list? got) (memq 'conn-peer-ip got) #t)))
      '(#t #f))

(printf "rows: ~a\n~a failures\nfacade-exports complete\n" rows bad)
