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

(import (chezscheme) (theourgia store) (theourgia request))
(define args (command-line-arguments))
;; NO ARGUMENTS IS A USAGE LINE, NOT A CRASH. See working-fault-child.
(when (< (length args) 3)
  (printf "usage: evidence-index-child <store> <writer> <report-path> [request-id]\n")
  (exit 0))
(putenv "THEOURGIA_HOME" (string-append (car args) "-home"))
(define es (store-evidence (car args) (cons (cadr args) (if (> (length args) 3) (list-ref args 3) "indexed"))))
(call-with-output-file (caddr args)
  (lambda (p) (write (map ev-placement es) p)) 'replace)
(printf "evidence-index-child complete\n")
