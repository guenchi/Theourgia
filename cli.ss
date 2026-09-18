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
(import (chezscheme) (theourgia rpc) (theourgia arguments) (theourgia render)
        (theourgia sched) (theourgia net)
        (only (theourgia daemon) serve)
        (only (theourgia ffi) exec-argv!))

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
(define (print-answer answer wire?)
  (put-string (current-output-port) ((if wire? render-wire render-human) answer)))
(define (local-launch args)
  (exec-argv! (append (list "python3" (string-append (let ((p (path-parent (car (command-line))))) (if (string=? p "") "." p)) "/local.py")) args)))

;; ---- forwarding ----------------------------------------------------------
;;
;; ⛔ THE SAME LANGUAGE, OVER A SOCKET. A daemon answers with exactly
;; what this file would have produced locally -- it calls the same
;; dispatcher -- so forwarding is transport and nothing else: the request
;; is wrapped in an envelope naming the store it is for, and the answer
;; is printed by the same two procedures that print a local one. ⛔ There
;; is no rule here about what a verb means.
;;
;; ⛔ AND IT IS SCHEME, not a helper process. What used to happen here was
;; an exec into `local.py --forward`: a second implementation of the wire
;; format, in another language, kept in step by hand -- and the envelope
;; it spoke was already not the one the daemon speaks.
;;
;; ⚠️ THIS NEVER RETURNS. `start-scheduler` does not: it ends in a loop
;; that waits for ever, so there is no "after the scheduler" to fall back
;; into. Every path below therefore finishes the job and exits, the local
;; one included.
(define (datum-line? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))

(define (finish answer wire?)
  (print-answer answer wire?)
  (exit (if (rpc-ok? answer) 0 1)))

;; ⛔ NOT REACHING ANYONE IS A REASON TO RUN LOCALLY; LOSING AN ANSWER IS
;; NOT. A socket file with nothing behind it is a daemon that has gone --
;; the request reached nobody, so running it here does the work once. But
;; once bytes have gone out, the request MAY have been carried out, and a
;; caller told "that did not happen" would do it again. ⛔ Something that
;; may have happened must never be reported as not having happened, so
;; that case gets `transport-unknown`, which means "ask me again".
;;
;; ⚠️ AND THE TWO ARE TOLD APART BY THE CONNECT ERRNO, which is the only
;; thing `exchange` gives that distinguishes them: it reports both as
;; `(transport-error <status>)`. Measured against this daemon: a socket
;; path with nothing there and a stale socket file whose daemon was
;; killed. ⚠️ A read failure mid-answer reports a different errno and
;; falls to the `transport-unknown` branch, which is the safe side.
;; -2 ENOENT (nothing at that path), -38 ENOTSOCK (something that is not
;; a socket), -61 ECONNREFUSED (a socket file whose daemon has gone).
;; ⭐ All three measured here, on this platform, against this daemon --
;; the last by killing one with SIGKILL so it could not unlink its own
;; socket. -111 is the same refusal on Linux, where libuv reports that
;; errno instead; it is listed by name rather than left to be discovered
;; by a user whose CLI stopped working.
(define connect-failed-statuses '(-2 -38 -61 -111))

(define (forward-then-exit! socket store actor verb resolved wire?)
  (start-scheduler
    (lambda ()
      (let ((outcome
              (exchange socket
                        (string->utf8
                          (string-append
                            (render-wire (append (list 'request store actor verb)
                                                 (argument-strings resolved)))
                            "\n"))
                        datum-line?
                        30000)))
        (cond
          ((and (pair? outcome) (eq? 'answer (car outcome)))
           (let ((answer (guard (e (#t 'unreadable))
                           (read (open-string-input-port (utf8->string (cadr outcome)))))))
             (if (eq? answer 'unreadable)
                 (finish '(error transport-unknown (reason unreadable-answer)) wire?)
                 (finish answer wire?))))
          ((and (pair? outcome) (eq? 'transport-error (car outcome))
                (memv (cadr outcome) connect-failed-statuses))
           (finish (rpc-dispatch-parsed store verb resolved actor) wire?))
          (else
           (finish (list 'error 'transport-unknown (list 'reason 'lost-answer)) wire?)))))))

;; ---- serve -----------------------------------------------------------------
;;
;; `theourgia serve [<store>] [--socket <path>]`. ⚠️ The store may be
;; given as a positional, because that is how the command has always been
;; spelled, or as `--store`, because that is how every other verb spells
;; it. ⛔ The parsing is `parse-arguments`, the same reader every other
;; verb uses: a second one here would be a second place that knows what
;; `--socket` means.
(define (serve-and-exit! argv)
  (let ((nodes (parse-arguments 'serve (cdr argv))))
    (if (and (pair? nodes) (eq? (car nodes) 'error))
        (begin (say nodes) (exit 1))
        (let* ((positional (argument-positionals nodes))
               (store (or (argument-option nodes "--store")
                          (and (pair? positional) (car positional))
                          (getenv "THEOURGIA_STORE")
                          "."))
               (socket (or (argument-option nodes "--socket")
                           (string-append store "/socket"))))
          ;; Does not return: the daemon runs until it is told to go, or
          ;; until it finds a reason to leave and reports it.
          (serve store socket)
          (exit 0)))))

(define (main argv)
  (when (null? argv)
    (say '(usage (theourgia <verb> ...)))
    (exit 1))
  ;; ⛔ `serve` IS THIS PROGRAM NOW. It used to exec a Python
  ;; implementation of the daemon; the daemon is Scheme, it speaks the
  ;; envelope the forwarding above speaks, and there is no second one to
  ;; keep in step. ⚠️ `eval` still goes out to the helper -- that is its
  ;; own step, and pretending otherwise here would leave a verb that
  ;; silently stopped working.
  (when (string=? (car argv) "serve") (serve-and-exit! argv))
  (when (string=? (car argv) "eval") (local-launch argv))
  (let* ((verb (string->symbol (car argv)))
         (nodes (parse-arguments verb (cdr argv)))
         (answer
           (if (and (pair? nodes) (eq? (car nodes) 'error)) nodes
               (let* ((store (or (argument-option nodes "--store")
                                (getenv "THEOURGIA_STORE") "."))
                     (actor (or (argument-option nodes "--actor") (environment-actor)))
                     (socket-path (or (argument-option nodes "--socket") (string-append store "/socket")))
                     (resolved (argument-stdin verb (argument-remove nodes '("--store" "--actor" "--wire" "--socket"))
                                 (lambda () (read-all-text (current-input-port))))))
                 (when (and (not (equal? (getenv "THEOURGIA_LOCAL") "1")) (file-exists? socket-path))
                   (forward-then-exit! socket-path store actor verb resolved
                                       (argument-option nodes "--wire")))
                 (rpc-dispatch-parsed store verb resolved actor)))))
    (print-answer answer (and (list? nodes) (not (and (pair? nodes) (eq? (car nodes) 'error))) (argument-option nodes "--wire")))
    (exit (if (rpc-ok? answer) 0 1))))

(main (cdr (command-line)))
