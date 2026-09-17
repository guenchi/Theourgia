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

;; Rewrite one AST form inside one named top-level definition, in place.
;;
;; IT LIVES HERE BECAUSE A FIXTURE NEEDS IT. `mcp-probe.py` adds a verb
;; to a throwaway copy of the core and then asks the unmodified MCP shell
;; whether it can see it -- so the tool the fixture runs has to travel
;; with the fixture. It was reached at `<parents[2]>/implementation/`,
;; outside this repository, which made the fixture unrunnable anywhere
;; that directory did not exist.
;;
;; EXACTLY ONE SITE, OR NOTHING. A pattern that matched no site would
;; rewrite nothing and say nothing, and the run that followed would be a
;; reading of unmutated code wearing a mutant's name; a pattern that
;; matched two would move more than the caller described.
(import (chezscheme))
(define args (command-line-arguments))
;; NO ARGUMENTS IS A USAGE LINE, NOT A CRASH. Every script in this
;; directory is run by the suite runner, and this one is a tool rather
;; than a fixture: the usage line is what tells the two apart.
(when (< (length args) 4)
  (printf "usage: mutate-form <file> <definition-name> <form-before> <form-after>\n")
  (exit 0))
(define file (car args))
(define name (string->symbol (cadr args)))
(define before (read (open-input-string (caddr args))))
(define after (read (open-input-string (cadddr args))))
(define form (call-with-input-file file read))
(define count 0)
(define (rewrite x)
  (cond ((equal? x before) (set! count (+ count 1)) after)
        ((pair? x) (cons (rewrite (car x)) (rewrite (cdr x))))
        (else x)))
(define result
  (map (lambda (x)
         (if (and (pair? x) (eq? 'define (car x))
                  (or (eq? name (cadr x))
                      (and (pair? (cadr x)) (eq? name (caadr x)))))
             (rewrite x) x)) form))
(unless (= count 1) (error 'mutation "Expected exactly one AST site" name count))
(call-with-output-file file (lambda (p) (pretty-print result p)) 'replace)
(printf "mutated ~s exactly once\n" name)
