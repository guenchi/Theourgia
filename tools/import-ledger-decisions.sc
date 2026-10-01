#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; Import a design ledger's numbered rulings as decision blocks.
;;
;;   scheme --script tools/import-ledger-decisions.sc <ledger.md> <store> [<landed.txt>]
;;
;; A RULING IS ONE LINE OF THE LEDGER'S CHANGE LOG, of the form
;;
;;   - **v297 (2026-09-18)** <the ruling>
;;
;; with an ASCII or a full-width pair of parentheses. Each becomes a block of
;; kind decision under one section, titled with its number and date, its text
;; the ruling as written.
;;
;; WHICH RULINGS HAVE LANDED IS NOT IN THE LINE, so it is not guessed from it.
;; <landed.txt> says so, one ruling per line: `v297 <where it landed>`. For
;; each, a block naming where is added and linked `implements` to the
;; decision, which is how the store says the obligation is discharged.
;;
;; A tool, not a verb: it writes through the same dispatcher as every caller,
;; and its first `commitments --open` afterwards is the reading it exists for.
(import (chezscheme) (theourgia rpc) (only (theourgia ffi) entry-bytes))

;; Read through the core's own file access, as every reader in this tree is.
(define (lines-of path)
  (let ((text (utf8->string (entry-bytes path))))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((= i (string-length text))
             (reverse (if (< start i) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))

(define (index-of s needle from)
  (let ((n (string-length needle)))
    (let loop ((i from))
      (cond ((> (+ i n) (string-length s)) #f)
            ((string=? (substring s i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))

(define (trim s)
  (let* ((n (string-length s))
         (a (let loop ((i 0)) (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i)))
         (b (let loop ((i n)) (if (and (> i a) (char-whitespace? (string-ref s (- i 1)))) (loop (- i 1)) i))))
    (substring s a b)))

;; (number date text) for a ruling line, #f for any other line.
(define (ruling line)
  (and (prefix? line "- **v")
       (let ((close (index-of line "**" 5)))
         (and close
              (let* ((head (substring line 5 close))
                     (open (or (index-of head "(" 0) (index-of head "\xFF08;" 0)))
                     (shut (or (index-of head ")" 0) (index-of head "\xFF09;" 0)))
                     (number (string-append "v" (trim (if open (substring head 0 open) head)))))
                (and (> (string-length number) 1)
                     (string->number (substring number 1 (string-length number)))
                     (list number
                           (if (and open shut (< open shut)) (trim (substring head (+ open 1) shut)) "")
                           (trim (substring line (+ close 2) (string-length line))))))))))

;; ((number . where) ...)
(define (landed-of path)
  (if (not path)
      '()
      (filter values
              (map (lambda (l)
                     (let* ((t (trim l)) (sp (index-of t " " 0)))
                       (and (> (string-length t) 0) (not (prefix? t "#"))
                            (cons (if sp (substring t 0 sp) t) (if sp (trim (substring t sp (string-length t))) "")))))
                   (lines-of path)))))

;; The id an insert answered with: the first string in the answer that names
;; a block.
(define (answered-id answer)
  (let find ((x answer))
    (cond ((and (string? x) (index-of x "." 0)) x)
          ((pair? x) (or (find (car x)) (find (cdr x))))
          (else #f))))

(define (main ledger store landed-path)
  (define (run . args)
    (let ((a (rpc-dispatch store args "import-ledger-decisions")))
      (unless (and (pair? a) (eq? (car a) 'ok))
        (assertion-violation 'import-ledger-decisions "the store refused" args a))
      a))
  (let* ((rulings (filter values (map ruling (lines-of ledger))))
         (landed (landed-of landed-path))
         (section (answered-id (run 'insert "--under" "root" "--title" "Ledger decisions"
                                    "--text" (string-append "Rulings imported from " ledger ".")))))
    (for-each
      (lambda (r)
        (let ((id (answered-id (run 'insert "--under" section
                                    "--title" (if (string=? (cadr r) "") (car r) (string-append (car r) " (" (cadr r) ")"))
                                    "--text" (caddr r)))))
          (run 'set id "kind" "decision")
          (let ((where (assoc (car r) landed)))
            (when where
              (let ((impl (answered-id (run 'insert "--under" id "--title" (string-append "Landed: " (cdr where))))))
                (run 'link impl "implements" id))))))
      rulings)
    (printf "~a rulings imported, ~a marked landed\n"
            (length rulings) (length (filter (lambda (r) (assoc (car r) landed)) rulings)))))

(let ((args (command-line-arguments)))
  (if (not (<= 2 (length args) 3))
      (begin (display "usage: import-ledger-decisions.sc <ledger.md> <store> [<landed.txt>]\n") (exit 2))
      (main (car args) (cadr args) (and (= 3 (length args)) (caddr args)))))
