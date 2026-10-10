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

;; A definition inside a string literal is text, not a definition.
;;
;; structure.sc reports a top-level definition that sits deeper than the
;; top level: the form before it never closed. (It was structure.py until
;; F9; these rows were written against the Python and read the same on the
;; port.) It used to record an opening
;; `(define` at the start of a line before asking whether the scan was
;; inside a string, so a program written as a string -- a child script a
;; fixture hands to another process -- was reported as a swallowed
;; definition, and the whole suite was refused at preflight (F84).
;;
;; The gate is run on a directory this file builds: a copy of structure.sc
;; under <dir>/test, and the samples in <dir>, which is where it looks.

(import (chezscheme))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (with-expected label expected (x) (want-1 label (caught got) (caught x)))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/structure-strings-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'structure-strings "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define (spit! path text)
  (call-with-output-file path (lambda (o) (put-string o text)) 'replace))
(define (slurp path)
  (call-with-input-file path
    (lambda (i) (let loop ((acc '()))
                  (let ((c (read-char i)))
                    (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc))))))))
(define (lines-of text)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length text))
           (reverse (if (> i start) (cons (substring text start i) acc) acc)))
          ((char=? (string-ref text i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
          (else (loop (+ i 1) start acc)))))
(define (starts-with? s p)
  (and (>= (string-length s) (string-length p)) (string=? p (substring s 0 (string-length p)))))
(define (contains? s w)
  (let loop ((i 0))
    (and (<= (+ i (string-length w)) (string-length s))
         (or (string=? w (substring s i (+ i (string-length w)))) (loop (+ i 1))))))

;; THE SAMPLES ARE BUILT FROM PIECES so that this file itself holds no line
;; starting with the opening it describes, which the gate would read.
(define nl (string #\newline))
(define open-define (string-append "(" "define (x) 1)"))
;; The define sits at the start of a line, inside a string, inside a form.
(define in-string
  (string-append "(define program \"" nl open-define nl "\")" nl))
;; CONTROL: the same line, outside any string, inside the same form -- the
;; case the gate exists for.
(define swallowed
  (string-append "(define program" nl open-define nl ")" nl))

;; A string that holds what looks like a library header, then a definition
;; swallowed by an unclosed form OUTSIDE any string. The string must not
;; make the file read as a library: that would move the indent the gate
;; checks from 0 to 2 and hide the swallowed definition.
(define open-library (string-append "(" "library (fake)"))
(define library-in-string
  (string-append "(define text \"before" nl open-library nl "\")" nl
                 "(begin" nl open-define nl ")" nl))
;; An escaped quote inside the string, and a character literal for a quote
;; before it: neither ends or starts a string.
(define escaped
  (string-append "(define q #\\\")" nl
                 "(define text \"a \\\" b" nl open-define nl "\")" nl))

;; `(FAIL-lines-for-the-sample not-checked? ran-to-end?)` of the gate run on
;; a directory holding one sample.
(define (gate-on name text)
  (let* ((dir (string-append root "/" name))
         (test (string-append dir "/test")))
    (system (string-append "mkdir -p " test))
    (system (string-append "cp structure.sc " test "/structure.sc"))
    (spit! (string-append dir "/" name ".sc") text)
    (system (string-append "scheme --script " test "/structure.sc > " dir "/out.txt 2>&1"))
    (let ((out (slurp (string-append dir "/out.txt")))
          (file (string-append name ".sc")))
      (list (filter (lambda (l) (and (starts-with? l "FAIL") (contains? l file)))
                    (lines-of out))
            (and (exists (lambda (l) (and (starts-with? l "NOT CHECKED") (contains? l file)))
                         (lines-of out))
                 #t)
            (and (contains? out "structure complete") #t)))))

(printf "== ST-1: a define inside a string ==\n")
(let ((control (gate-on "swallowed" swallowed))
      (probe (gate-on "instring" in-string)))
  (want "CONTROL ST-1 the same line outside a string is reported, and the gate ran to its end"
        (list (length (car control)) (cadr control) (caddr control))
        '(1 #f #t))
  (want "ST-1 a define at the start of a line inside a string literal is not reported, and the file WAS checked"
        probe
        '(() #f #t)))
(let ((lib (gate-on "libstring" library-in-string))
      (esc (gate-on "escaped" escaped)))
  (want "ST-1 a library header inside a string does not hide a swallowed definition outside it"
        (list (length (car lib)) (cadr lib) (caddr lib))
        '(1 #f #t))
  (want "ST-1 an escaped quote and a quote character literal do not end or start the string"
        esc
        '(() #f #t)))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "rm -rf " root))
(printf "structure-strings complete\n")
