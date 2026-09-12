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

;; The command line. It finds the store and the actor, hands one request
;; to (theourgia rpc), prints the answer and exits.
;;
;; IT HOLDS NO RULE OF ITS OWN. What a verb means, which outcomes count
;; as success, what a refusal is called and how an unknown id is reported
;; are all decided in the rpc library, so the command line and a caller
;; over a socket cannot come to different answers about the same store.
;; They used to: `nearest-ids` was built twice in this file, once for
;; `read` and once for `read --md`, and a change to either would have
;; been a change only half the tree saw.
;;
;; WHAT IS LEFT HERE IS ARGV AND STDOUT. Those are the two things a
;; network caller does not have, and they are the only two things this
;; file knows about.
;;
;; STDOUT IS THE ANSWER AND THE EXIT CODE IS THE VERDICT. An agent reads
;; the S-expression; a shell reads the code. Diagnostics go to stderr and
;; are never part of either.
(import (chezscheme) (theourgia rpc) (theourgia arguments))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; ---- argv ----------------------------------------------------------------

(define (environment-actor)
  (or (let ((e (getenv "THEOURGIA_ACTOR"))) (and e (> (string-length e) 0) e))
      (let ((u (getenv "USER"))) (and u (> (string-length u) 0) u)) "cli"))

(define (read-all-text port)
  (let-values (((out get) (open-string-output-port)))
    (let loop ()
      (let ((c (read-char port)))
        (if (eof-object? c) (get) (begin (put-char out c) (loop)))))))

;; ---- printing ------------------------------------------------------------

;; THREE KINDS OF ANSWER AND THE ANSWER SAYS WHICH. Items are shown one
;; per line, text is shown as it is, and anything else is one datum. The
;; classification is the library's; this only draws it.
(define (print-answer answer)
  (cond
    ((and (pair? answer) (eq? (car answer) 'ok)
          (pair? (cdr answer)) (pair? (cadr answer))
          (eq? (car (cadr answer)) 'text))
     (put-string (current-output-port) (cadr (cadr answer))))
    ((and (pair? answer) (eq? (car answer) 'ok)
          (pair? (cdr answer)) (pair? (cadr answer))
          (eq? (car (cadr answer)) 'items))
     (for-each say (cdr (cadr answer))))
    (else (say answer))))

(define (main argv)
  (when (null? argv)
    (say '(usage (theourgia <verb> ...)))
    (exit 1))
  (let* ((verb (string->symbol (car argv)))
         (nodes (parse-arguments verb (cdr argv)))
         (answer
           (if (and (pair? nodes) (eq? (car nodes) 'error)) nodes
               (let ((store (or (argument-option nodes "--store")
                                (getenv "THEOURGIA_STORE") "."))
                     (actor (or (argument-option nodes "--actor") (environment-actor))))
                 (rpc-dispatch-parsed store verb
                   (argument-stdin verb (argument-remove nodes '("--store" "--actor"))
                     (lambda () (read-all-text (current-input-port)))) actor)))))
    (print-answer answer)
    (exit (if (rpc-ok? answer) 0 1))))

(main (cdr (command-line)))
