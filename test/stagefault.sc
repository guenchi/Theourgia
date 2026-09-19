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

;; The point of the stage dimension: the SAME fault must fire in the
;; stage it names and nowhere else. Two writes, two stages, one armed.
(import (chezscheme) (theourgia ffi) (theourgia trace))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
;; AN ARGUMENT THIS PROBE CANNOT DO WITHOUT, ASKED FOR BY NAME. Reading
;; `(cadr (command-line))` with no argument raises on `cadr`, and a probe
;; that died that way is indistinguishable from one that crashed -- so a
;; runner classifying scripts by what they did cannot tell "needs an
;; argument" from "broken", and either has to keep a list of names or
;; report the probe as red for ever. One line makes the class decidable
;; from the outcome.
(define (stagefault-argument)
  (let ((a (command-line)))
    (if (null? (cdr a))
        (begin (printf "usage: stagefault.sc <directory>\n") (exit 2))
        (cadr a))))
(define d (stagefault-argument))
(system (string-append "rm -rf " d "; mkdir -p " d))
(define (fresh name)
  (let ((p (string-append d "/" name)))
    (fd-open p '(read-write create append))))
(define line (string->utf8 "0123456789abcdefghijklmnopqrstuvwxyz\n"))
(parameterize ((theourgia-trace? #t))
  (for-each
   (lambda (stage)
     (let ((fd (fresh (symbol->string stage))))
       (printf "stage ~a: " stage)
       (parameterize ((theourgia-stage stage))
         (guard (e ((fs-error? e) (printf "raised ~a\n" (fs-error-errno e))))
           (write-all! fd line)
           (printf "wrote ~a bytes total\n" (file-size (string-append d "/" (symbol->string stage))))))
       (fd-close fd)))
   '(deliver-barrier commit)))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "stagefault complete\n")
