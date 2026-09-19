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

;; A directory flush must be injectable, and must still be told apart
;; from a file flush by its op symbol.
(import (chezscheme) (theourgia ffi) (theourgia trace))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
;; A CHILD THAT CANNOT START SAYS SO IN A LINE, NOT A BACKTRACE.
(define d
  (let ((args (cdr (command-line))))
    (if (null? args)
        (begin (printf "usage: dirflush.sc <directory>\n") (exit 2))
        (car args))))
(system (string-append "rm -rf " d "; mkdir -p " d "/segdir " d "/regdir"))
(parameterize ((theourgia-trace? #t))
  (for-each
   (lambda (sub)
     (printf "fsync-dir! ~a -> ~a\n" sub
             (guard (e ((fs-error? e) (list 'raised (fs-error-op e) (fs-error-errno e))))
               (fsync-dir! (string-append d "/" sub) 'commit)
               'ok)))
   '("segdir" "regdir")))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "dirflush complete\n")
