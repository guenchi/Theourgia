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
(import (chezscheme) (theourgia rpc))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; ---- argv ----------------------------------------------------------------

;; OPTIONS ARE PULLED OUT FIRST AND THE REST STAY POSITIONAL, so that a
;; value beginning with a dash is still a value.
(define (take-option args name)
  (let loop ((xs args) (kept '()) (found #f))
    (cond
      ((null? xs) (values found (reverse kept)))
      ((and (not found) (string=? (car xs) name) (pair? (cdr xs)))
       (loop (cddr xs) kept (cadr xs)))
      (else (loop (cdr xs) (cons (car xs) kept) found)))))

;; THE ACTOR IS WHOEVER THE CALLER SAYS IT IS, and the environment is
;; asked before a name is invented: a record's actor is evidence, and
;; "cli" is what it says when nobody claimed it.
(define (actor-of args)
  (let-values (((v rest) (take-option args "--actor")))
    (values (or v
                (let ((e (getenv "THEOURGIA_ACTOR")))
                  (and e (> (string-length e) 0) e))
                (let ((u (getenv "USER")))
                  (and u (> (string-length u) 0) u))
                "cli")
            rest)))

(define (store-of args)
  (let-values (((v rest) (take-option args "--store")))
    (values (or v (let ((e (getenv "THEOURGIA_STORE"))) (or e "."))) rest)))

(define (read-all-text port)
  (let-values (((out get) (open-string-output-port)))
    (let loop ()
      (let ((c (read-char port)))
        (if (eof-object? c) (get) (begin (put-char out c) (loop)))))))

;; STDIN IS AN ARGUMENT THAT ARRIVES ANOTHER WAY. A request carries
;; strings, so the two places this command reads from stdin are resolved
;; into strings HERE -- the library never learns that a terminal was
;; involved, and a caller over a socket sends the same request with the
;; same bytes in it.
(define (with-stdin verb args)
  (cond
    ((eq? verb 'batch) (list (read-all-text (current-input-port))))
    ((and (eq? verb 'insert) (member "-" args))
     (let loop ((xs args) (out '()) (after-text #f))
       (cond
         ((null? xs) (reverse out))
         ((and after-text (string=? (car xs) "-"))
          (loop (cdr xs) (cons (read-all-text (current-input-port)) out) #f))
         (else (loop (cdr xs) (cons (car xs) out)
                     (string=? (car xs) "--text"))))))
    (else args)))

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
  (let*-values (((store rest0) (store-of (cdr argv)))
                ((actor args) (actor-of rest0)))
    (let* ((verb (string->symbol (car argv)))
           (answer (rpc-dispatch store (cons verb (with-stdin verb args)) actor)))
      (print-answer answer)
      (exit (if (rpc-ok? answer) 0 1)))))

(main (cdr (command-line)))
