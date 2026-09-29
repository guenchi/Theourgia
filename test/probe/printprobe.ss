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

;; THE DATUM PRINTER, MEASURED PER CHEZ VERSION. ffi.sc's source-datum-print
;; accepts only the Chez versions listed in printer-measured-versions,
;; because a block's text is what Chez's pretty-print makes of it. This
;; program reproduces the core's reader (a #!chezscheme prefix,
;; get-datum/annotations) and source-datum-print's parameters, without the
;; version check; keep both in step with ffi.sc.
;;
;; TO ADMIT A NEW CHEZ: run this under the new version and under one
;; already listed, on the same list of files, each into its own empty
;; directory,
;;
;;     find <trees> -name '*.sc' -o -name '*.ss' > corpus.txt
;;     scheme --script test/probe/printprobe.ss <out-dir> < corpus.txt
;;
;; and compare the two directories file by file (md5 of each <n>.txt). Only
;; when every file is byte-identical is the version added to
;; printer-measured-versions, with the reading cited beside it.
;;
;; For each file named on stdin (one path per line): read every top-level datum the way the
;; core's source reader does (a #!chezscheme prefix, get-datum/annotations), print each with the
;; core's source-datum-print parameters (without its version check), and write one line per
;; file: <n> <datum count> <path>, n the file's zero-based index, plus the printed text to
;; <out>/<n>.txt.
(import (chezscheme))
(define out (cadr (command-line)))
(define (print-datum datum)
  (parameterize ((pretty-line-length 72) (pretty-one-line-limit 72)
                 (pretty-initial-indent 0) (pretty-standard-indent 2)
                 (pretty-maximum-lines #f) (print-length #f) (print-level #f)
                 (print-radix 10) (print-graph #f) (print-gensym #f)
                 (print-unicode #f))
    (call-with-string-output-port (lambda (port) (pretty-print datum port)))))
(define (read-all text)
  (let ((p (open-input-string (string-append "#!chezscheme\n" text)))
        (sfd (source-file-descriptor "datum-input" 0)))
    (let loop ((pos 0) (acc '()))
      (let-values (((a end) (guard (e (#t (values 'unreadable pos))) (get-datum/annotations p sfd pos))))
        (cond ((eq? a 'unreadable) (reverse (cons 'unreadable acc)))
              ((eof-object? a) (reverse acc))
              (else (loop end (cons (annotation-stripped a) acc))))))))
(define (slurp path) (call-with-input-file path get-string-all))
(let loop ((n 0))
  (let ((line (get-line (current-input-port))))
    (unless (eof-object? line)
      (let* ((ds (guard (e (#t '(unreadable-file))) (read-all (slurp line))))
             (txt (apply string-append (map (lambda (d) (if (eq? d 'unreadable) "#<unreadable>\n" (print-datum d))) ds))))
        (call-with-output-file (string-append out "/" (number->string n) ".txt")
          (lambda (p) (put-string p txt)) 'replace)
        (display (string-append (number->string n) " " (number->string (length ds)) " " line)) (newline)
        (loop (+ n 1))))))
