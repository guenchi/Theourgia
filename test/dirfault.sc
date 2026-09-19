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

;; An atomic replacement flushes a temporary INSIDE the target's
;; directory and then the directory. A path substring naming the
;; directory matches both; dir= is what separates them.
(import (chezscheme) (theourgia log) (theourgia ffi))
;; AN ARGUMENT THIS PROBE CANNOT DO WITHOUT, ASKED FOR BY NAME. Reading
;; `(cadr (command-line))` with no argument raises on `cadr`, and a probe
;; that died that way is indistinguishable from one that crashed -- so a
;; runner classifying scripts by what they did cannot tell "needs an
;; argument" from "broken", and either has to keep a list of names or
;; report the probe as red for ever. One line makes the class decidable
;; from the outcome.
(define (dirfault-argument)
  (let ((a (command-line)))
    (if (null? (cdr a))
        (begin (printf "usage: dirfault.sc <directory>\n") (exit 2))
        (cadr a))))
(define d (dirfault-argument))
(system (string-append "rm -rf " d "; mkdir -p " d))
(printf "~s\n"
        (guard (e ((fs-error? e) (list 'raised (fs-error-op e))))
          (atomic-write! (string-append d "/published.sexp") (string->utf8 "()\n") 'publish)
          'no-fault))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "dirfault complete\n")
