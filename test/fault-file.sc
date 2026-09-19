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

;; The faults that leave a file behind, so the file can be asked what
;; actually happened rather than the error being taken at its word.
(import (chezscheme) (theourgia ffi) (theourgia trace))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
;; A CHILD THAT CANNOT START SAYS SO IN A LINE, NOT A BACKTRACE. The
;; parent passes the directory to work in and reads back what was
;; printed; run with none, taking the cadr of a one-element command
;; line died inside `cadr`, which reads to a parent exactly like the
;; case having failed rather than never having run.
(define dir
  (let ((args (cdr (command-line))))
    (if (null? args)
        (begin (printf "usage: fault-file.sc <directory>\n") (exit 2))
        (car args))))
(system (string-append "rm -rf " dir "; mkdir -p " dir "/reg"))
(define log (string-append dir "/000001.sexp"))
(define reg (string-append dir "/reg/instances.sexp"))
(define line (string->utf8 "0123456789abcdefghijklmnopqrstuvwxyz\n"))
(define fd (fd-open log '(read-write create append)))
(define rfd (fd-open reg '(read-write create append)))
;; A call site must say which stage it is, or a staged fault cannot aim
;; at it -- which is the whole point of the stage dimension. The log
;; layer will declare these; this fixture stands in for it.
(parameterize ((theourgia-trace? #t))
  (guard (e ((fs-error? e)
             (printf "write raised op=~a errno=~a\n" (fs-error-op e) (fs-error-errno e))))
    (parameterize ((theourgia-stage 'commit))
      (printf "write-all! -> ~a\n" (write-all! fd line))))
  (guard (e ((fs-error? e)
             (printf "fsync(log) raised op=~a errno=~a\n" (fs-error-op e) (fs-error-errno e))))
    (fsync! fd log 'commit)
    (printf "fsync(log) returned\n"))
  (guard (e ((fs-error? e)
             (printf "fsync(registry) raised op=~a errno=~a\n" (fs-error-op e) (fs-error-errno e))))
    (fsync! rfd reg 'registry)
    (printf "fsync(registry) returned\n")))
(fd-close fd) (fd-close rfd)
(printf "log holds ~a of ~a bytes\n" (file-size log) (bytevector-length line))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "fault-file complete\n")
