;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; A SECTION'S HEADING LINE IS ONE RULE FOR READ --MD AND EXPORT-MD (README,
;; read and export-md; md.sc heading-line): the stored heading-src when it
;; parses to the section's level and title, else "#"*level, a space, the title
;; and LF; a level that is not 1 to 6 reads as 1. A section made by insert has
;; no heading-src, and read --md answered its text with no heading at all. A
;; datum block has no markdown, and read --md answers so.
;;
;;   MH-1  read --md of a section made by insert: its heading line
;;   MH-2  CONTROL: export-md of the same store writes the same line
;;   MH-3  three sections, read --md and export-md agree: no level ("# One"),
;;         level 2 ("## Two"), a stored heading-src that parses to the same
;;         pair ("# Three #", kept byte for byte)
;;   MH-7  a decision under the document, its line stored and then retitled: read
;;         --md and export-md both show the new title's line
;;   MH-8  a task the same way
;;   MH-6  the rule itself (md.sc heading-line): a level that is not 1 to 6 --
;;         the text "2", which the write path refuses and only a record from
;;         elsewhere can hold -- reads as 1
;;   MH-4  read --md of a datum block: (error not-markdown (id <id>) (mode datum))
;;   MH-9  read --recursive --md of a datum root: the same refusal
;;   MH-5  a section retitled after its line was stored: read --md and export-md
;;         both show the new title's line, not the stored one
(import (chezscheme) (only (theourgia md) heading-line))
(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/md-heading-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(define home (string-append root "/home"))
(define cli "../core.sc")

;; -> the first datum a command answered, with TEXT on its standard input
(define (run store text . args)
  (call-with-output-file (string-append root "/in") (lambda (p) (display text p)) 'replace)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER THEOURGIA_HOME=" home
            " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " --wire < " root "/in > " root "/out 2> " root "/err"))
  (let ((x (call-with-input-file (string-append root "/out") read)))
    (if (eof-object? x) (list 'no-answer) x)))
(define n-store 0)
;; -> (<store> <block id> <cursor text>): a store holding one block, and the
;; cursor after it
(define (store-with-a-block!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (run d "" "init")
    (let* ((a (run d "" "insert" "--under" "root" "--title" "A"))
           (w (cadr (assq 'cursor (cdr a))))
           (id (car (car (cadr (assq 'state (cdr a)))))))
      (list d id (string-append (car w) ":" (number->string (cdr w)))))))
(define (end-of d)
  (let* ((c (run d "" "check")) (w (cadr (assq 'local-writer (cdr c)))))
    (cadr (assq 'end (cdr (assoc w (cadr (assq 'writers (cdr c)))))))))
;; a batch answer's first new id
(define (batch-id a) (car (car (cadr (assq 'state (cdr (car (cadr a))))))))
(define (new-id a) (car (car (cadr (assq 'state (cdr a))))))
(define (md-text a) (and (pair? a) (eq? (car a) 'ok) (pair? (cadr a)) (eq? (car (cadr a)) 'text) (cadr (cadr a))))
(define (file-text p) (call-with-input-file p get-string-all))
(define (contains? hay needle)
  (let ((n (string-length needle)) (m (string-length hay)))
    (let loop ((i 0)) (and (<= (+ i n) m) (or (string=? (substring hay i (+ i n)) needle) (loop (+ i 1)))))))
(define (starts? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))

(define d (string-append root "/s"))
(run d "" "init")
(define docid (batch-id (run d "((insert root #f ((kind . doc) (title . \"D\") (path . \"d.md\"))))" "batch")))
(define one (new-id (run d "" "insert" "--under" docid "--title" "One")))
(define two (new-id (run d "" "insert" "--under" docid "--title" "Two")))
(define three (new-id (run d "" "insert" "--under" docid "--title" "Three")))
(printf "level 2 and the stored line: ~s\n"
        (run d (format "((set ~s level 2) (set ~s heading-src \"# Three #\\n\"))" two three) "batch"))
(define five (new-id (run d "" "insert" "--under" docid "--title" "Old")))
(run d (format "((set ~s heading-src \"# Old\\n\"))" five) "batch")
(run d "" "set" five "title" "New")
(define (insert-kind kind title)
  (batch-id (run d (format "((insert ~s #f ((kind . ~a) (title . ~s) (heading-src . ~s))))" docid kind title
                           (string-append "# " title "\n"))
                 "batch")))
(define decision (insert-kind "decision" "Old decision"))
(define task (insert-kind "task" "Old task"))
(run d "" "set" decision "title" "New decision")
(run d "" "set" task "title" "New task")
(define out (string-append root "/md"))
(system (string-append "mkdir -p '" out "'"))
(printf "export-md: ~s\n" (run d "" "export-md" out))
(define exported (file-text (string-append out "/d.md")))
(printf "d.md:\n~a" exported)

(want "MH-1 read --md of a section made by insert begins with its heading line, # One"
      (let ((t (md-text (run d "" "read" one "--md")))) (and t (starts? t "# One\n")))
      #t)
(want "MH-2 CONTROL: export-md of the same store writes the line # One"
      (contains? exported "# One\n")
      #t)
(want "MH-3 read --md and export-md give the same heading line for a section with no level, one at level 2, and one whose stored line parses to the same pair"
      (map (lambda (id line)
             (let ((t (md-text (run d "" "read" id "--md"))))
               (list (and t (starts? t line)) (contains? exported line))))
           (list one two three) '("# One\n" "## Two\n" "# Three #\n"))
      '((#t #t) (#t #t) (#t #t)))
(define dt (batch-id (run d "((insert root #f ((kind . code) (mode . datum) (body define x 1))))" "batch")))
(want "MH-4 read --md of a datum block answers not-markdown, naming it and its mode"
      (run d "" "read" dt "--md")
      (list 'error 'not-markdown (list 'id dt) '(mode datum)))
(want "MH-9 read --recursive --md whose root is a datum block answers the same refusal"
      (run d "" "read" dt "--recursive" "--md")
      (list 'error 'not-markdown (list 'id dt) '(mode datum)))

(want "MH-5 a section retitled after its heading line was stored: read --md and export-md both show # New, and neither the stored # Old"
      (let ((t (md-text (run d "" "read" five "--md"))))
        (list (and t (starts? t "# New\n")) (contains? exported "# New\n") (contains? exported "# Old\n")))
      '(#t #t #f))

(want "MH-7 a decision retitled after its heading line was stored: read --md and export-md both show # New decision, and neither the stored # Old decision"
      (let ((t (md-text (run d "" "read" decision "--md"))))
        (list (and t (starts? t "# New decision\n")) (contains? exported "# New decision\n") (contains? exported "# Old decision\n")))
      '(#t #t #f))
(want "MH-8 a task retitled after its heading line was stored: read --md and export-md both show # New task, and neither the stored # Old task"
      (let ((t (md-text (run d "" "read" task "--md"))))
        (list (and t (starts? t "# New task\n")) (contains? exported "# New task\n") (contains? exported "# Old task\n")))
      '(#t #t #f))
(want "MH-6 the rule reads a level that is not 1 to 6 as 1: the text 2, 0, 7 and none give # Two"
      (map (lambda (level) (heading-line "" level "Two")) '("2" 0 7 #f))
      '("# Two\n" "# Two\n" "# Two\n" "# Two\n"))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nmd-heading complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
