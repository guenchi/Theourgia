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

;; The eval worker's ready budget holds a worker that is slow to exist.
;;
;; The supervisor gives a worker `ready-ms` of wall clock to say `ready`,
;; and answers (error eval-worker-unavailable (reason no-ready)) past it.
;; The worker imports the store and the reducer before it says ready, which
;; from source costs about the old budget on an unloaded machine, so a
;; loaded machine turned a healthy store's evaluation into that error. The
;; rows here start the worker through an interpreter that sleeps before it
;; runs (THEOURGIA_SCHEME names the interpreter the supervisor starts), so a
;; slow start is produced on purpose and not waited for.
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
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; THE SLOW START IS IN-PROCESS. A wrapper that slept in a shell and then
;; exec'd the interpreter would leave the `sleep` child alive when the
;; supervisor kills a worker that never said ready (by pid: there is no
;; group yet), and the runner would read it as LEFT IN GROUP. So the
;; wrapper execs the interpreter at once on a script that sleeps inside
;; the one process and then loads the shipped worker with its arguments.
(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/evalready-" pid-text))
(when (file-exists? here)
  (assertion-violation 'eval-ready "scratch directory already exists" here))
(define store (string-append here "/store"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(system (string-append "mkdir -p " store))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(putenv "THEOURGIA_RUN" (string-append here "/run"))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

;; THE INTERPRETER THE SUPERVISOR STARTS: the one on PATH, found once, so
;; the wrapper below names a real binary and not itself.
(define real-scheme
  (let ((f (string-append here "/which.txt")))
    (system (string-append "command -v scheme > " f))
    (let ((t (file-text f)))
      (substring t 0 (max 0 (- (string-length t) 1))))))

;; A wrapper that becomes the real interpreter at once, on a script that
;; sleeps in-process and then loads the shipped worker with its arguments:
;; the worker is exactly the shipped one, only later.
(define slow-worker
  (let ((f (string-append here "/slow-worker.sc")))
    (call-with-output-file f
      (lambda (p)
        (put-string p (string-append
          ";; argv: --script <this> <seconds> --script <worker> <worker args...>\n"
          "(let* ((a (cdr (command-line)))\n"
          "       (seconds (string->number (car a)))\n"
          "       (worker (caddr a)))\n"
          "  (sleep (make-time 'time-duration 0 seconds))\n"
          "  (command-line (cons worker (cdddr a)))\n"
          "  (load worker))\n")))
      'replace)
    f))
(define (slow-scheme! seconds)
  (let ((f (string-append here "/slow-" (number->string seconds) ".sh")))
    (call-with-output-file f
      (lambda (p)
        (put-string p (string-append "#!/bin/sh\nexec " real-scheme " --script " slow-worker " "
                                     (number->string seconds) " \"$@\"\n")))
      'replace)
    (system (string-append "chmod 700 " f))
    f))

(define (cli-eval source interpreter . flags)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "THEOURGIA_SCHEME=" interpreter " "
              "scheme --script ../core.sc eval '" source "' --store " store " --wire "
              (apply string-append (map (lambda (f) (string-append f " ")) flags))
              "> " out " 2>&1"))
    ;; THE ANSWER IS THE LAST DATUM: with a fresh THEOURGIA_HOME the command
    ;; line first says where it made its home, on the same stream.
    (let ((data (guard (e (#t '()))
                  (call-with-port (open-input-file out)
                    (lambda (p)
                      (let loop ((acc '()))
                        (let ((d (read p)))
                          (if (eof-object? d) (reverse acc) (loop (cons d acc))))))))))
      (if (null? data) (list 'no-answer (file-text out)) (car (reverse data))))))

(define (head-of a) (if (pair? a) (car a) a))
(define (reason-of a)
  (let ((r (and (pair? a) (list? a)
                (find (lambda (x) (and (pair? x) (eq? (car x) 'reason))) (cdr a)))))
    (and r (pair? (cdr r)) (cadr r))))

(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                       "scheme --script ../core.sc init --store " store " > /dev/null 2>&1"))

(define (now-ms) (let ((t (current-time 'time-monotonic)))
                   (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

(printf "== ER-1: a worker that takes a second to exist is still answered ==\n")
(want "CONTROL ER-1 through the interpreter on PATH, named explicitly, the evaluation answers ok"
      (let ((a (cli-eval "(+ 1 2)" real-scheme)))
        (if (eq? (head-of a) 'ok) 'ok a))
      'ok)
(want "ER-1 through a worker that sleeps one second before it exists, the evaluation still answers ok"
      (let ((a (cli-eval "(+ 1 2)" (slow-scheme! 1))))
        (if (eq? (head-of a) 'ok) 'ok a))
      'ok)
;; THE TWIN SLEEPS NINE SECONDS, and the row reads the clock: an answer that
;; arrived before the sleep ended was not waited for. Nine leaves the ready
;; budget four seconds of room for the command line's own start under load.
(want "ER-1 TWIN: a worker that would take nine seconds is answered by name within the budget, not waited for"
      (let* ((t0 (now-ms))
             (a (cli-eval "(+ 1 2)" (slow-scheme! 9)))
             (dt (- (now-ms) t0)))
        (list (head-of a) (and (pair? a) (list? a) (cadr a)) (reason-of a)
              (if (< dt 9000) 'before-the-sleep-ended (list 'waited dt))))
      '(error eval-worker-unavailable no-ready before-the-sleep-ended))

(printf "== ER-2: the evaluation's budget starts when the worker is ready ==\n")
;; A two-second start under a one-second timeout: a deadline taken before
;; ready has expired when the worker arrives, and the answer would be
;; eval-limit for an evaluation that never began.
(want "ER-2 a worker that takes two seconds to exist, with a one-second evaluation timeout, answers ok"
      (let ((a (cli-eval "(+ 1 2)" (slow-scheme! 2) "--timeout-ms" "1000")))
        (if (eq? (head-of a) 'ok) 'ok a))
      'ok)

(printf "== ER-3: the CPU ceiling is added to the start-up's CPU ==\n")
;; The worker is started directly, as the supervisor starts it, with a CPU
;; argument of 2 and nothing on its standard input; its first line is the
;; diag frame naming the ceiling it installed. RLIMIT_CPU counts the whole
;; process, and the imports before the install cost CPU, so the ceiling
;; read must exceed the argument by that start-up, rounded up: at least 3.
(want "ER-3 started with a CPU argument of 2, the worker installs a ceiling of at least 3, its start-up's CPU added"
      (let ((out (string-append here "/worker.txt")))
        (system (string-append
                  "cd .. && CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                  real-scheme " --script eval-worker.sc " store " '()' '' 2 65536 < /dev/null > " out " 2>&1"))
        ;; THE DIAG LINE IS THE FIRST diag DATUM, not the first line: with a
        ;; fresh THEOURGIA_HOME the worker first says where it made its home.
        (let* ((t (file-text out))
               (d (guard (e (#t #f))
                    (call-with-port (open-string-input-port t)
                      (lambda (p)
                        (let loop ()
                          (let ((x (read p)))
                            (cond ((eof-object? x) #f)
                                  ((and (pair? x) (eq? (car x) 'diag)) x)
                                  (else (loop)))))))))
               (n (and (pair? d) (eq? (car d) 'diag) (pair? (cdr d)) (string? (cadr d))
                       (let ((s (cadr d)))
                         (and (> (string-length s) 11) (string=? (substring s 0 11) "rlimit-cpu ")
                              (string->number (substring s 11 (string-length s))))))))
          (cond ((not n) (list 'first-line (substring t 0 (min 80 (string-length t)))))
                ((>= n 3) 'at-least-3)
                (else (list 'ceiling n)))))
      'at-least-3)

(system (string-append "rm -rf " here))
(printf "\n~a failures\nrows: ~a\neval-ready complete\n" bad rows)
