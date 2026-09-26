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

;; THE VERBS A PROGRAM ANSWERS ITSELF, READ FROM ITS DISPATCH TABLE (F64).
;;
;; `core.sc` answers `eval` in its own process and `theourgiad.sc` answers
;; `serve`; neither goes through the dispatcher, so neither is in
;; `rpc-verbs`. Each program dispatches them from one table, `(define
;; own-verbs (list (cons '<verb> <procedure>) ...))`, and this file reads
;; that table as data. `options-gate.sc` and `docs-check.sc` both load it,
;; so the population they census is the program's own and is spelled once.
;;
;; NEVER: IT WAS A LITERAL IN EACH OF THEM, `'(eval serve)`, typed by hand
;; three times over two files. A third verb dispatched by either program
;; joined none of them, was compared against nothing, and turned no row red.
;;
;; NEVER: IT RAISES RATHER THAN ANSWER EMPTY. A program with no `own-verbs`
;; define, a table that names no verb, and an entry of any other shape all
;; raise, naming the file. An empty answer would read as "this program
;; answers nothing itself", which is the one reading a census cannot tell
;; from a reader that has stopped finding the table.
;;
;; NOTE: WHAT IT CANNOT SEE: a verb a program answers by some other route --
;; a `string=?` on the first argument written beside the table rather than
;; an entry in it. The table is the dispatch point by construction; a
;; second one is a change to the program, not something this reader checks.
;;
;; It is loaded, not included, for the reason `import-walk.sc` gives: each
;; caller computes its own directory and hands `load` the path.

;; Every form in the file. A top-level program is a bare `(import ...)`
;; followed by many forms, so the first is not enough.
(define (own-verbs-forms path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((out '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse out) (loop (cons x out))))))))

;; The verbs of the `own-verbs` table among `forms`, in the table's order.
;; `where` names the source in what it raises.
(define (own-verbs-in forms where)
  (let ((defs (filter (lambda (f)
                        (and (pair? f) (eq? (car f) 'define)
                             (pair? (cdr f)) (eq? (cadr f) 'own-verbs)))
                      forms)))
    (cond
      ((null? defs)
       (error 'own-verbs "no (define own-verbs ...) at the top level" where))
      ((pair? (cdr defs))
       (error 'own-verbs "more than one (define own-verbs ...)" where))
      (else
       (let ((value (and (list? (car defs)) (= (length (car defs)) 3) (caddr (car defs)))))
         (unless (and (list? value) (pair? value) (eq? (car value) 'list))
           (error 'own-verbs "own-verbs is not a (list ...) form" where value))
         (let ((verbs (map (lambda (entry)
                             (if (and (list? entry) (= (length entry) 3)
                                      (eq? (car entry) 'cons)
                                      (list? (cadr entry)) (= (length (cadr entry)) 2)
                                      (eq? (car (cadr entry)) 'quote)
                                      (symbol? (cadr (cadr entry))))
                                 (cadr (cadr entry))
                                 (error 'own-verbs
                                        "an entry is not (cons '<verb> <procedure>)"
                                        where entry)))
                           (cdr value))))
           (when (null? verbs)
             (error 'own-verbs "the own-verbs table names no verb" where))
           verbs))))))

(define (own-verbs-of path)
  (own-verbs-in (own-verbs-forms path) path))
