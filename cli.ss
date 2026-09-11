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

;; The command line. It parses arguments, calls the store, and prints one
;; S-expression on stdout. It holds no rule of its own: what an intent
;; means, what deps a record declares and what a refusal is called are
;; all decided in (theourgia store), so the library and the command
;; cannot come to disagree about them.
;;
;; STDOUT IS THE ANSWER AND THE EXIT CODE IS THE VERDICT. An agent reads
;; the S-expression; a shell reads the code. Diagnostics go to stderr and
;; are never part of either.
(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia log))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; THE ONLY EXIT POINTS. A verb answers; it does not decide how to leave.
(define (ok! x) (say x) (exit 0))
(define (fail! x) (say x) (exit 1))

(define (usage form) (fail! (list 'usage form)))

;; NOTHING RAISED REACHES THE TERMINAL. An agent reads stdout and
;; expects an S-expression there; a Chez backtrace is neither an answer
;; nor on stdout, so a caller gets an empty answer, a non-zero code and
;; no way to tell "the store does not exist" from "the tool broke".
;; Every failure this program can reach is translated into a form with
;; a name in it.
(define (describe-log-error e)
  (let ((detail (log-error-detail e)))
    (append (list 'error (log-error-kind e))
            (if (log-error-writer e) (list (list 'writer (log-error-writer e))) '())
            (if (log-error-segment e) (list (list 'segment (log-error-segment e))) '())
            (if (and (pair? detail) (pair? (car detail)))
                (map (lambda (p) (list (car p) (cdr p))) detail)
                '()))))

(define (guarded thunk)
  (guard (e ((log-error? e) (fail! (describe-log-error e)))
            (#t (fail! (list 'error 'internal
                             (list 'condition
                                   (if (message-condition? e)
                                       (condition-message e)
                                       "unexpected failure")))))) 
    (thunk)))

;; A STORE THAT IS NOT THERE IS ITS OWN ANSWER, decided before anything
;; opens it. Letting the load layer discover it works, but it answers
;; with the kind of the file that was missing rather than with the fact
;; the caller can act on: there is no store here.
(define (require-store! store)
  (unless (file-exists? (string-append store "/meta.sexp"))
    (fail! (list 'error 'no-store store))))

;; ---- arguments -----------------------------------------------------------

;; OPTIONS ARE PULLED OUT FIRST AND THE REST STAY POSITIONAL, so that a
;; value beginning with a dash is still a value: `set x note --n` takes
;; --n as the note because the option scan has already consumed the
;; options it knows and stops claiming anything after them.
(define (take-option args name)
  (let loop ((xs args) (out '()) (found #f))
    (cond
      ((null? xs) (values found (reverse out)))
      ((and (string=? (car xs) name) (pair? (cdr xs)))
       (loop (cddr xs) out (cadr xs)))
      ((string=? (car xs) name)
       (usage (list (string->symbol name) '<value>)))
      (else (loop (cdr xs) (cons (car xs) out) found)))))

(define (store-of args)
  (let-values (((v rest) (take-option args "--store")))
    (values (or v (let ((e (getenv "THEOURGIA_STORE"))) (or e "."))) rest)))

;; ---- rendering -----------------------------------------------------------

;; THE OUTLINE IS A TREE, PRINTED AS ONE. state-outline gives flat rows
;; of (parent ord id) already in order; the walk turns the parent links
;; into indentation. Rows whose parent is a block that is not itself
;; placed would be unreachable from the root, so they are listed under
;; their own headings rather than silently dropped.
(define (title-of state id)
  (let* ((b (state-read state id))
         (fs (and b (cdr (assq 'fields b))))
         (e (and fs (assq 'title fs))))
    (if (and e (string? (cdr e))) (cdr e) "")))

(define (outline-text state)
  (let* ((rows (state-outline state))
         (structure (state-structure state))
         (orphans (cdr (assq 'orphans structure)))
         (out (open-output-string)))
    (define (children-of parent)
      (filter (lambda (row) (equal? (car row) parent)) rows))
    (define (walk parent depth)
      (for-each
        (lambda (row)
          (let ((id (caddr row)))
            (put-string out (make-string (* 2 depth) #\space))
            (put-string out "- ")
            (put-string out id)
            (put-string out "  ")
            (put-string out (title-of state id))
            (put-string out "\n")
            (walk id (+ depth 1))))
        (children-of parent)))
    (walk 'root 0)
    (unless (null? orphans)
      (put-string out "orphans:\n")
      (for-each (lambda (id)
                  (put-string out "- ")
                  (put-string out id)
                  (put-string out "  ")
                  (put-string out (title-of state id))
                  (put-string out "\n"))
                orphans))
    (get-output-string out)))

;; ---- verbs ---------------------------------------------------------------

;; EVERY WRITING VERB GOES THROUGH ONE DOOR. The answer shape, the
;; failure shape and the exit code are decided here once rather than in
;; each verb, so a verb cannot invent a third way to report a refusal.
(define (write-one store intent)
  (let ((answers (with-store-write store (lambda (state view) (list intent)))))
    (let ((a (car answers)))
      (if (eq? (car a) 'ok) (ok! a) (fail! a)))))

;; OPTION NAMES TRAVEL AS STRINGS IN A USAGE FORM. "--under" is not a
;; symbol an R6RS reader will accept, and the point of printing the
;; shape is that an agent can read it back -- a form this file could not
;; itself read would be a poor thing to hand out.
(define insert-usage
  '(insert "--under" <id> ["--after" <id>] "--title" <text> ["--text" <text>]))

(define (parse-insert store args)
  (let*-values (((under rest1) (take-option args "--under"))
                ((after rest2) (take-option rest1 "--after"))
                ((title rest3) (take-option rest2 "--title"))
                ((text rest) (take-option rest3 "--text")))
    (unless (null? rest)
      (usage insert-usage))
    (unless title
      (usage insert-usage))
    (write-one store
               (list 'insert
                     (if (or (not under) (string=? under "root")) 'root under)
                     after
                     (append (list (cons 'kind 'section) (cons 'title title))
                             (if text (list (cons 'text text)) '()))))))

(define (parse-set store args)
  (let-values (((expect rest) (take-option args "--if-unchanged")))
    (let ((intent
            (cond
              ((= 3 (length rest))
               (list 'set (car rest) (string->symbol (cadr rest)) (caddr rest)))
              ((= 2 (length rest))
               (list 'set (car rest) (string->symbol (cadr rest))))
              (else (usage '(set <id> <field> <value>))))))
      (write-one store (if expect (list 'expect expect intent) intent)))))

(define (parse-move store args)
  (let-values (((after rest) (take-option args "--after")))
    (unless (= 2 (length rest)) (usage '(move <id> <parent> ["--after" <id>])))
    (write-one store
               (list 'move (car rest)
                     (if (string=? (cadr rest) "root") 'root (cadr rest))
                     after))))

(define (parse-edge store verb args)
  (unless (= 3 (length args))
    (usage (list verb '<from> '<rel> '<to>)))
  (write-one store (list verb (car args) (string->symbol (cadr args)) (caddr args))))

;; A BATCH IS ONE LOCK AND ONE ANSWER PER ITEM. The intents come in as
;; S-expressions on stdin and go to the store in one call, so the lock is
;; held across all of them; the answer is the list the store gives back,
;; which is shorter than the input when an item failed.
(define (read-all-data port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x) (reverse acc) (loop (cons x acc))))))

(define (parse-batch store args)
  (unless (null? args) (usage '(batch)))
  (let ((items (let ((data (read-all-data (current-input-port))))
                 (if (and (= 1 (length data)) (list? (car data))
                          (pair? (car data)) (list? (car (car data))))
                     (car data)
                     data))))
    (let ((answers (with-store-write store (lambda (state view) items))))
      (say (cons 'batch (list answers)))
      (exit (if (for-all (lambda (a) (eq? (car a) 'ok)) answers) 0 1)))))

(define (main argv)
  (when (null? argv)
    (usage '(theourgia <verb> ...)))
  (let-values (((store args) (store-of (cdr argv))))
    (let ((verb (car argv)))
      (cond
        ((string=? verb "init")
         (guarded (lambda ()
                    (let ((a (store-init! store)))
                      (if (eq? (car a) 'ok) (ok! a) (fail! a))))))
        ((string=? verb "insert")
         (require-store! store) (guarded (lambda () (parse-insert store args))))
        ((string=? verb "set")
         (require-store! store) (guarded (lambda () (parse-set store args))))
        ((string=? verb "move")
         (require-store! store) (guarded (lambda () (parse-move store args))))
        ((string=? verb "del")
         (unless (= 1 (length args)) (usage '(del <id>)))
         (require-store! store)
         (guarded (lambda () (write-one store (list 'del (car args))))))
        ((string=? verb "link")
         (require-store! store) (guarded (lambda () (parse-edge store 'link args))))
        ((string=? verb "unlink")
         (require-store! store) (guarded (lambda () (parse-edge store 'unlink args))))
        ((string=? verb "batch")
         (require-store! store) (guarded (lambda () (parse-batch store args))))
        ((string=? verb "outline")
         (unless (null? args) (usage '(outline)))
         (require-store! store)
         (guarded (lambda ()
                    (put-string (current-output-port) (outline-text (open-and-reduce store)))
                    (exit 0))))
        ((string=? verb "read")
         (require-store! store)
         (unless (= 1 (length args)) (usage '(read <id>)))
         (guarded
           (lambda ()
             (let* ((state (open-and-reduce store))
                    (b (state-read state (car args))))
               (if b
                   (ok! (cons 'ok (list b)))
                   (fail! (list 'error 'unknown-id (car args)
                                (list 'nearest (nearest-ids state (car args))))))))))
        (else (fail! (list 'error 'unknown-verb verb)))))))

(main (cdr (command-line)))
