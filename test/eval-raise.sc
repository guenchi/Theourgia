;; Copyright 2018 - 2026 The Theourgia Authors
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

;; What the user's source raises is data, never the answer's shape (F92,
;; F93). An eval answer is (ok ...) or an (error <name> ...) of the worker's
;; or the supervisor's; a raised value travels inside
;; (error eval-exception (kind raised) ...) as (value <v>) when it can be
;; written back, else as (reason unwritable-value) (type <t>). The worker's
;; own refusals (bad-source, eval-context) are not raises of the source and
;; keep their shape.
(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expect)
  (set! rows (+ rows 1))
  (if (equal? got expect)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a -> ~s   WANT ~s\n" label got expect))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/evalraise-" pid-text))
(when (file-exists? here)
  (assertion-violation 'eval-raise "scratch directory already exists" here))
(define store (string-append here "/store"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(system (string-append "mkdir -p " store))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(putenv "THEOURGIA_RUN" (string-append here "/run"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; The source is spelled without quote characters, since it travels through a
;; shell; the answer is the LAST datum on stdout (a fresh home says where it
;; made its home first).
(define (cli-eval source . flags)
  (let ((out (string-append here "/out.txt")) (rc (string-append here "/rc.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc eval '" source "' --store " store " --wire "
              (apply string-append (map (lambda (f) (string-append f " ")) flags))
              "> " out " 2>/dev/null; echo $? > " rc))
    (let ((data (guard (e (#t '()))
                  (call-with-port (open-input-file out)
                    (lambda (p)
                      (let loop ((acc '()))
                        (let ((d (read p)))
                          (if (eof-object? d) (reverse acc) (loop (cons d acc))))))))))
      (cons (string->number (let ((t (file-text rc))) (substring t 0 (max 0 (- (string-length t) 1)))))
            (if (null? data) (list 'no-answer (file-text out)) (car (reverse data)))))))
(define (rc-of r) (car r))
(define (answer-of r) (cdr r))
(define (field a name)
  (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr a)))))
    (and f (pair? (cdr f)) (cadr f))))
(define (head2 a) (and (pair? a) (list? a) (pair? (cdr a)) (list (car a) (cadr a))))

(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                       "scheme --script ../core.sc init --store " store " > /dev/null 2>&1"))

(printf "== ER-A/B: a raised value shaped like an answer is data ==\n")
(want "ER-A a raised improper pair headed error is carried as (value ...) inside eval-exception, exit 1"
      (let ((r (cli-eval "(raise (cons (string->symbol \"error\") (string->symbol \"custom\")))")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'kind) (field (answer-of r) 'value)))
      '(1 (error eval-exception) raised (error . custom)))
(want "ER-B a raised proper list headed error is carried as data too, not forged as the answer"
      (let ((r (cli-eval "(raise (list (string->symbol \"error\") (string->symbol \"custom\") 1))")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'value)))
      '(1 (error eval-exception) (error custom 1)))

(printf "== ER-C: a value that cannot be written is said to be one ==\n")
(want "ER-C a raised list holding a procedure answers eval-exception with (reason unwritable-value) (type procedure), not eval-worker-exit"
      (let ((r (cli-eval "(raise (list (string->symbol \"error\") (lambda () #f)))")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'reason) (field (answer-of r) 'type)))
      '(1 (error eval-exception) unwritable-value procedure))

(printf "== ER-D/E: the worker's own refusals and an ordinary exception keep their shape ==\n")
(want "ER-D two forms in one source is the worker's own refusal, bad-source, not wrapped"
      (let ((r (cli-eval "(+ 1 2) (+ 3 4)")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'reason)))
      '(1 (error bad-source) expected-one-form))
(want "ER-E an ordinary exception inside the evaluation is eval-exception as before"
      (let ((r (cli-eval "(car 1)")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'kind) (field (answer-of r) 'message)))
      '(1 (error eval-exception) raised "Evaluation raised an exception"))
;; A MALFORMED SOURCE RAISES NOTHING, IT IS REFUSED. The reader the worker
;; calls raises its refusal as a list headed error; F92's first cut wrapped
;; that as a raised value (measured on that cut, by hand, after the first F92
;; suite: `(` answered (error eval-exception (kind raised) (value (error
;; bad-source ...)))).
;; The source and the cut are both read by the worker before evaluation;
;; the flags reach a shell, so the cut is quoted there.
(want "ER-G an unbalanced source is the reader's refusal, bad-source unbalanced, not wrapped"
      (let ((r (cli-eval "(")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'reason)))
      '(1 (error bad-source) unbalanced))
(want "ER-H an unbalanced --cut is the reader's refusal, bad-source unbalanced, not wrapped"
      (let ((r (cli-eval "(+ 1 2)" "--cut" "'('")))
        (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'reason)))
      '(1 (error bad-source) unbalanced))
(want "CONTROL ER-0 a plain value answers ok with its values"
      (let ((r (cli-eval "(+ 1 2)")))
        (list (rc-of r) (and (pair? (answer-of r)) (car (answer-of r))) (field (answer-of r) 'values)))
      '(0 ok (3)))

;; WHAT THE WORKER'S OWN STEPS RAISE IS NOT THE SOURCE'S VALUE (F92 review
;; r1, S). The store's load raises `log-error`, a record, for a meta.sexp it
;; cannot parse; on 86db404 the worker answered that with the fixed message,
;; and F92's r1 passed it to raised-answer, which reported a record the
;; source never raised: (reason unwritable-value) (type unsupported). The
;; meta file is put back after the row, so the rows above are unaffected.
(let* ((meta (string-append store "/meta.sexp"))
       (saved (file-text meta)))
  (call-with-output-file meta (lambda (p) (put-string p "(not a meta")) 'truncate)
  (let ((r (cli-eval "(+ 1 2)")))
    (call-with-output-file meta (lambda (p) (put-string p saved)) 'truncate)
    (want "ER-I a store whose meta.sexp does not parse answers the worker's fixed message, not a value the source raised"
          (list (rc-of r) (head2 (answer-of r)) (field (answer-of r) 'kind) (field (answer-of r) 'message)
                (field (answer-of r) 'reason) (field (answer-of r) 'value))
          '(1 (error eval-exception) raised "Evaluation raised an exception" #f #f))))

(system (string-append "chmod -R u+rwx " here " 2>/dev/null; rm -rf " here))
(printf "\n~a failures\nrows: ~a\neval-raise complete\n" bad rows)
