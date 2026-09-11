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
(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia project) (theourgia md))

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

;; WHO IS WRITING, IN THE ORDER THE CALLER CAN OVERRIDE. An explicit
;; --actor beats the environment, the environment beats the account the
;; process is running under, and a caller with none of those is written
;; down as "cli" -- a program, which is what it is.
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

;; ---- rendering -----------------------------------------------------------

;; THE OUTLINE IS A TREE, PRINTED AS ONE. state-outline gives flat rows
;; of (parent ord id) already in order; the walk turns the parent links
;; into indentation. Rows whose parent is a block that is not itself
;; placed would be unreachable from the root, so they are listed under
;; their own headings rather than silently dropped.
;; A DOCUMENT HAS A PATH WHERE A SECTION HAS A TITLE. Showing the title
;; field for both left every document in the outline as a bare id with
;; two spaces after it -- the one line in the listing a reader most
;; needs in order to know which file they are looking at.
(define (title-of state id)
  (let* ((b (state-read state id))
         (fs (and b (cdr (assq 'fields b))))
         (get (lambda (k) (let ((e (and fs (assq k fs))))
                            (and e (string? (cdr e)) (cdr e))))))
    (or (get 'title) (get 'path) "")))

(define (outline-text state . limit)
  (let* ((depth-limit (if (pair? limit) (car limit) #f))
         (rows (state-outline state))
         (structure (state-structure state))
         (orphans (cdr (assq 'orphans structure)))
         (out (open-output-string)))
    (define (children-of parent)
      (filter (lambda (row) (equal? (car row) parent)) rows))
;; A DEPTH LIMIT STOPS THE WALK, it does not filter the output: a
    ;; filtered listing still pays to render everything, and on a corpus
    ;; that is the difference between an outline and a dump.
    (define (walk parent depth)
      (for-each
        (lambda (row)
          (let ((id (caddr row)))
            (when (or (not depth-limit) (< depth depth-limit))
            (put-string out (make-string (* 2 depth) #\space))
            (put-string out "- ")
            (put-string out id)
            (put-string out "  ")
            (put-string out (title-of state id))
            (put-string out "\n")
            (walk id (+ depth 1)))))
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
(define (write-one store actor intent)
  (let ((answers (with-store-write store (lambda (state view) (list intent)) actor)))
    (let ((a (car answers)))
      (if (eq? (car a) 'ok) (ok! a) (fail! a)))))

;; OPTION NAMES TRAVEL AS STRINGS IN A USAGE FORM. "--under" is not a
;; symbol an R6RS reader will accept, and the point of printing the
;; shape is that an agent can read it back -- a form this file could not
;; itself read would be a poor thing to hand out.
(define insert-usage
  '(insert "--under" <id> ["--after" <id>] "--title" <text> ["--text" <text>]))

(define (parse-insert store actor args)
  (let*-values (((under rest1) (take-option args "--under"))
                ((after rest2) (take-option rest1 "--after"))
                ((title rest3) (take-option rest2 "--title"))
                ((text rest) (take-option rest3 "--text")))
    (unless (null? rest)
      (usage insert-usage))
    (unless title
      (usage insert-usage))
    ;; `--text -` READS THE BODY FROM STDIN, because markdown does not
    ;; fit on a command line: newlines, quotes and everything a shell
    ;; would eat are exactly what a section body is made of.
    (let ((body (cond ((not text) #f)
                      ((string=? text "-") (read-all-text (current-input-port)))
                      (else text))))
      (write-one store actor
                 (list 'insert
                       (if (or (not under) (string=? under "root")) 'root under)
                       after
                       (append (list (cons 'kind 'section) (cons 'title title))
                               (if body (list (cons 'src body)) '())))))))

;; A NUMBER FROM THE COMMAND LINE IS CHECKED BY SHAPE BEFORE IT IS
;; CONVERTED. `string->number` implements the whole of Scheme's numeric
;; syntax, and `#e1e99999999` is a request to build an exact integer of
;; ten billion digits: it does not refuse, it allocates until the machine
;; is exhausted, and no check placed after it ever runs because the call
;; does not return. A shape test first -- plain ASCII digits, and few
;; enough of them to name something a store could hold -- makes the
;; conversion bounded work. The same order is already used to read a
;; segment file's name and a markdown list marker.
(define count-digit-limit 18)

(define (count-argument s)
  (and (string? s)
       (> (string-length s) 0)
       (<= (string-length s) count-digit-limit)
       (let loop ((i 0))
         (cond
           ((= i (string-length s)) (string->number s 10))
           ((char<=? #\0 (string-ref s i) #\9) (loop (+ i 1)))
           (else #f)))))

(define (read-all-text port)
  (let-values (((out get) (open-string-output-port)))
    (let loop ()
      (let ((c (read-char port)))
        (if (eof-object? c) (get) (begin (put-char out c) (loop)))))))

(define (parse-set store actor args)
  (let-values (((expect rest) (take-option args "--if-unchanged")))
    (let ((intent
            (cond
              ((= 3 (length rest))
               (list 'set (car rest) (string->symbol (cadr rest)) (caddr rest)))
              ((= 2 (length rest))
               (list 'set (car rest) (string->symbol (cadr rest))))
              (else (usage '(set <id> <field> <value>))))))
      (write-one store actor (if expect (list 'expect expect intent) intent)))))

(define (parse-move store actor args)
  (let-values (((after rest) (take-option args "--after")))
    (unless (= 2 (length rest)) (usage '(move <id> <parent> ["--after" <id>])))
    (write-one store actor
               (list 'move (car rest)
                     (if (string=? (cadr rest) "root") 'root (cadr rest))
                     after))))

(define (parse-edge store actor verb args)
  (unless (= 3 (length args))
    (usage (list verb '<from> '<rel> '<to>)))
  (write-one store actor (list verb (car args) (string->symbol (cadr args)) (caddr args))))

;; A BATCH IS ONE LOCK AND ONE ANSWER PER ITEM. The intents come in as
;; S-expressions on stdin and go to the store in one call, so the lock is
;; held across all of them; the answer is the list the store gives back,
;; which is shorter than the input when an item failed.
(define (read-all-data port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x) (reverse acc) (loop (cons x acc))))))

(define (parse-batch store actor args)
  (unless (null? args) (usage '(batch)))
  (let ((items (let ((data (read-all-data (current-input-port))))
                 (if (and (= 1 (length data)) (list? (car data))
                          (pair? (car data)) (list? (car (car data))))
                     (car data)
                     data))))
    (let ((answers (with-store-write store (lambda (state view) items) actor)))
      (say (cons 'batch (list answers)))
      (exit (if (for-all (lambda (a) (eq? (car a) 'ok)) answers) 0 1)))))

(define (main argv)
  (when (null? argv)
    (usage '(theourgia <verb> ...)))
  (let*-values (((store rest0) (store-of (cdr argv)))
                ((actor args) (actor-of rest0)))
    (let ((verb (car argv)))
      (cond
        ((string=? verb "init")
         (guarded (lambda ()
                    (let ((a (store-init! store)))
                      (if (eq? (car a) 'ok) (ok! a) (fail! a))))))
        ((string=? verb "insert")
         (require-store! store) (guarded (lambda () (parse-insert store actor args))))
        ((string=? verb "set")
         (require-store! store) (guarded (lambda () (parse-set store actor args))))
        ((string=? verb "move")
         (require-store! store) (guarded (lambda () (parse-move store actor args))))
        ((string=? verb "del")
         (unless (= 1 (length args)) (usage '(del <id>)))
         (require-store! store)
         (guarded (lambda () (write-one store actor (list 'del (car args))))))
        ((string=? verb "link")
         (require-store! store) (guarded (lambda () (parse-edge store actor 'link args))))
        ((string=? verb "unlink")
         (require-store! store) (guarded (lambda () (parse-edge store actor 'unlink args))))
;; A DIRECTORY AND THE STORE, EACH MADE FROM THE OTHER. import answers
        ;; with one entry per intent, exactly as batch does -- a refusal
        ;; about identity is an ordinary refusal and reads like one.
        ((string=? verb "import-md")
         (unless (or (= 1 (length args))
                     (and (= 2 (length args)) (string=? (cadr args) "--allow-delete")))
           (usage '(import-md <dir> ["--allow-delete"])))
         (require-store! store)
         (guarded (lambda ()
                    (let ((answers (import-md store (car args) actor
                                              (= 2 (length args)))))
                      (say (cons 'import (list answers)))
                      (exit (if (for-all (lambda (a) (eq? (car a) 'ok)) answers) 0 1))))))
        ((string=? verb "export-md")
         (unless (or (= 1 (length args))
                     (and (= 2 (length args)) (string=? (cadr args) "--with-ids")))
           (usage '(export-md <dir> ["--with-ids"])))
         (require-store! store)
         (guarded (lambda ()
                    (ok! (export-md store (car args) (= 2 (length args)))))))
        ;; A REPORT AND A VERDICT. The report prints either way -- being
        ;; told "damaged" without being told where is not an answer -- and
        ;; the exit code is what a shell reads.
;; THE WAY OUT OF A NAMED CONDITION, and it says which one. On a
        ;; healthy store it refuses and lists what it looked for -- a
        ;; voluntary generation change splits history for nothing.
        ((string=? verb "adopt")
         (unless (null? args) (usage '(adopt)))
         (require-store! store)
         (guarded (lambda ()
                    (let ((a (store-adopt! store)))
                      (if (eq? (car a) 'adopted)
                          (ok! (cons 'ok (cdr a)))
                          (fail! (cons 'error (cdr a))))))))
        ((string=? verb "check")
         (unless (null? args) (usage '(check)))
         (require-store! store)
         (guarded (lambda ()
                    (let ((r (store-check store)))
                      (say r)
                      (exit (if (eq? 'ok (cadr (assq 'verdict (cdr r)))) 0 1))))))
        ((string=? verb "snapshot")
         (unless (null? args) (usage '(snapshot)))
         (require-store! store)
         (guarded (lambda ()
                    (let ((a (store-snapshot! store actor)))
                      (if (eq? (car a) 'written)
                          (ok! (list 'ok (list 'snapshot (cadr a)) (list 'cut (caddr a))))
                          (fail! (cons 'error (cdr a))))))))
        ;; THE LOCAL END OF SYNC. It hands one segment's bytes to the log
        ;; and prints what the log decided; every rule about whether they
        ;; may be installed lives there, so the command and the library
        ;; cannot come to disagree about what "published" means.
        ;;
        ;; IT WAITS RATHER THAN FAILING. Another process publishing, or a
        ;; reader holding the shared lock, makes this block until the
        ;; lock is free -- a sync client that had to distinguish "busy"
        ;; from "refused" would have to re-send to find out which.
        ;;
        ;; THE HASH IS THE SENDER'S DECLARATION, so it is an argument
        ;; when the sender has one. Computing it here from the same bytes
        ;; would check nothing: it would compare a number with itself.
        ;; Given no declaration, the bytes on disk are taken as their own.
        ((string=? verb "publish")
         (unless (or (= 3 (length args)) (= 4 (length args)))
           (usage '(publish <writer> <segment> <file> [<sha256>])))
         (require-store! store)
         (let ((segment (count-argument (cadr args))))
           (unless (and segment (> segment 0))
             (usage '(publish <writer> <segment> <file> [<sha256>])))
           (guarded
             (lambda ()
               (let* ((path (caddr args))
                      (bytes (if (file-exists? path)
                                 (call-with-port (open-file-input-port path)
                                   (lambda (in)
                                     (let ((b (get-bytevector-all in)))
                                       (if (eof-object? b) (make-bytevector 0) b))))
                                 #f)))
                 (if (not bytes)
                     (fail! (list 'error 'no-candidate (list 'path path)))
                     (let* ((sha (if (= 4 (length args))
                                     (cadddr args)
                                     (segment-sha bytes)))
                            (a (log-publish! store (car args) segment bytes sha)))
                       ;; ONE SHAPE FOR A FAILURE: the first word is
                       ;; `error' and the rest is the reason. The log
                       ;; layer's own refusals already lead with `error'
                       ;; where the candidate itself was wrong, and
                       ;; wrapping those again would make a caller strip
                       ;; two different depths to reach the same fact.
                       (if (memq (car a) '(published idempotent repaired extended))
                           (ok! (cons 'ok (list a)))
                           (fail! (cons 'error
                                        (if (eq? (car a) 'error) (cdr a) (list a))))))))))))
        ;; WHAT REFERS TO THIS BLOCK, from link records and from the
        ;; text, each line saying which. A block with nothing pointing at
        ;; it prints nothing and exits zero: that is an answer, not a
        ;; failure.
        ((string=? verb "refs")
         (unless (= 1 (length args)) (usage '(refs <id>)))
         (require-store! store)
         (guarded (lambda ()
                    (let ((a (store-refs store (car args))))
                      (if (eq? (car a) 'ok)
                          (begin
                            (for-each (lambda (r)
                                        (say (list 'ref (list 'from (car r))
                                                   (list 'rel (cadr r))
                                                   (list 'via (caddr r)))))
                                      (cadr a))
                            (exit 0))
                          (fail! a))))))
        ;; A QUERY IS A STRING AND NOTHING ELSE. It is matched, never
        ;; parsed: a token that looks like a number is text like any
        ;; other, so nothing here can be asked to build one.
        ;;
        ;; NO HITS IS AN ANSWER. Empty output with a zero code says the
        ;; store was read and nothing matched; an error would say the
        ;; question could not be asked.
        ((string=? verb "search")
         (unless (= 1 (length args)) (usage '(search <query>)))
         (require-store! store)
         (guarded (lambda ()
                    (for-each (lambda (hit)
                                (say (cons 'hit hit)))
                              (store-search store (car args)))
                    (exit 0))))
        ((string=? verb "batch")
         (require-store! store) (guarded (lambda () (parse-batch store actor args))))
        ((string=? verb "outline")
         (let-values (((depth rest) (take-option args "--depth")))
           (unless (null? rest) (usage '(outline ["--depth" <n>])))
           (when (and depth (not (count-argument depth)))
             (usage '(outline ["--depth" <n>])))
           (require-store! store)
           (guarded (lambda ()
                      (put-string (current-output-port)
                                  (if depth
                                      (outline-text (open-and-reduce store)
                                                    (count-argument depth))
                                      (outline-text (open-and-reduce store))))
                      (exit 0)))))
;; `read --md` GIVES BACK THE SECTION AS IT IS ON DISK, heading and
        ;; all, rather than the block datum. That is what a person or an
        ;; editor wants; the S-expression is what a program wants, and
        ;; both are the same bytes seen two ways.
        ((and (string=? verb "read") (= 2 (length args)) (string=? (cadr args) "--md"))
         (require-store! store)
         (guarded
           (lambda ()
             (let* ((state (open-and-reduce store))
                    (b (state-read state (car args))))
               (if (not b)
                   (fail! (list 'error 'unknown-id (car args)
                                (list 'nearest (nearest-ids state (car args)))))
                   (let* ((fs (cdr (assq 'fields b)))
                          (head (let ((e (assq 'heading-src fs))) (if e (cdr e) "")))
                          (body (let ((e (assq 'src fs))) (if e (cdr e) ""))))
                     (put-string (current-output-port) (string-append head body))
                     (exit 0)))))))
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
