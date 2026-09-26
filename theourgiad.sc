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

;; The daemon program. It starts the daemon for one store and does nothing
;; else: `theourgiad.sc serve [<store>] [--socket <path>] [--detach --log
;; <path>]`, the argv `serve` has always taken, the leading `serve`
;; included, so that every launcher changes by file name only.
;;
;; KEY: THREE PROGRAMS, ONE PER ROLE (F46). `theourgia.sc` is the thin client
;; a user runs; `core.sc` answers a request in its own process or forwards
;; it; this one is the daemon. `ps` shows `theourgiad.sc` for every daemon.
;;
;; NEVER: NO OTHER VERB. Any other first word -- or none -- answers
;; `(error bad-request (reason not-a-daemon-verb) (usage ...))`, the serve
;; usage inside the error as `detach-needs-a-log` carries it, and exits 1,
;; before anything is touched. A request for the store goes to
;; `core.sc`, or through the thin client to a daemon started from here.
;;
;; NEVER: `(theourgia daemon)` IS LOADED AFTER THE ARGUMENTS ARE CHECKED,
;; through `(environment ...)`, not imported. An argument error is answered
;; with the bytes `core.sc serve` gave before the split even when that
;; library cannot be loaded or loads differently: its dependencies do work
;; while they expand (igropyr's injection switch reads the environment,
;; prints a banner, and raises on a bad value), and an import would do that
;; work before `main` runs. Measured on the first split, which imported it:
;; under IGROPYR_INJECT=bad, `serve d --socket ""` raised with exit 255
;; where the base refused with exit 2 (F46 review 1, N1).
(import (chezscheme) (theourgia arguments)
        (only (theourgia render) answer-printing!)
        (only (theourgia client) socket-path socket-dir-refusal)
        (only (theourgia ffi) setsid! redirect-stdio! trace-event!
              fs-error? fs-error-errno unreadable-entry? unreadable-entry-errno))

(define (later lib name)
  (eval name (environment lib)))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; NOTE: `--detach` IS FOR A LAUNCHER, NOT FOR A PERSON. It leaves the
;; caller's session and replaces stdio; it does NOT fork. Typed at a
;; prompt it stops there, silently, because the output it would have
;; shown has already been redirected to the log.
(define serve-usage
  '(serve [<store>] ["--socket" <path>]
          ["--detach" "--log" <path> (started-by-a-client-not-by-hand)]))

;; ---- serve -----------------------------------------------------------------
;;
;; `theourgia serve [<store>] [--socket <path>]`. NOTE: The store may be
;; given as a positional, because that is how the command has always been
;; spelled, or as `--store`, because that is how every other verb spells
;; it. NEVER: The parsing is `parse-arguments`, the same reader every other
;; verb uses: a second one here would be a second place that knows what
;; `--socket` means.
;;
;; NEVER: AN OPTION SERVE DOES NOT READ IS REFUSED, BEFORE ANYTHING IS
;; OPENED OR BOUND. The shared parser reads the options every verb shares
;; (`--actor`, `--req`, `--cursor`, `--wire`) and turns a token it does not
;; know into a positional, so `serve d --bogus` used to serve the store
;; `d` and drop the rest. Serve reads exactly the four below; anything
;; else spelled as an option is named back with the usage. A positional
;; after `--` is a literal, as for every verb, and is not an option.
(define serve-options '("--store" "--socket" "--detach" "--log"))

(define (serve-unknown-option nodes)
  (let loop ((ns nodes))
    (cond
      ((null? ns) #f)
      ((eq? (caar ns) 'end) #f)
      ((and (memq (caar ns) '(option flag))
            (not (member (cadar ns) serve-options)))
       (cadar ns))
      ((and (eq? (caar ns) 'pos)
            (let ((s (cadar ns)))
              (and (>= (string-length s) 2) (string=? (substring s 0 2) "--"))))
       (cadar ns))
      (else (loop (cdr ns))))))

(define (serve-and-exit! argv)
  (let ((nodes (parse-arguments 'serve (cdr argv))))
    (when (and (pair? nodes) (eq? (car nodes) 'error))
      (say nodes) (say (list 'usage serve-usage)) (exit 1))
    (let ((unknown (serve-unknown-option nodes)))
      (when unknown
        (say (list 'error 'bad-request '(reason unknown-option)
                   (list 'option unknown) (list 'usage serve-usage)))
        (exit 1)))
    (let* ((positional (argument-positionals nodes))
           (store (or (argument-option nodes "--store")
                      (and (pair? positional) (car positional))
                      (getenv "THEOURGIA_STORE")
                      "."))
           ;; NEVER: THE SHARED RULE, NOT A SECOND ONE. This used to
           ;; default to `<store>/socket`, which bypassed the
           ;; daemon's own function entirely -- so the rule the
           ;; README documented was never the rule that ran.
           (socket (or (argument-option nodes "--socket")
                       (socket-path store))))
      ;; NEVER: AN EMPTY SOCKET PATH IS REFUSED RATHER THAN TRIED. It is
      ;; not a path, and every layer below treats it as one: the
      ;; daemon derives its lock file from it, and for a path with no
      ;; directory in it that lock is created IN THE CURRENT
      ;; DIRECTORY -- an empty path produced a file called `..lock`
      ;; in whatever directory the process happened to be in, which
      ;; is how this was found, in the source tree. The bind then
      ;; fails and the daemon leaves.
      (when (and socket (string=? socket ""))
        (say '(error bad-socket-path (reason empty)))
        (exit 2))
      ;; NEVER: A --socket GIVEN BY HAND IS NOT GIVEN A DIRECTORY. Before
      ;; F15 the daemon made the whole chain and served, so a mistyped
      ;; path became a directory tree nobody asked for. It is refused
      ;; here, before --detach (a refusal after it would go to the log
      ;; instead of to the caller) and before anything is created. The
      ;; default socket's directory is still made by the daemon: it is
      ;; the run directory, the client's own.
      (let ((given (argument-option nodes "--socket")))
        (when (and given (not (string=? given "")))
          (let ((refusal (socket-dir-refusal given)))
            (when refusal
              (say refusal)
              (exit 2)))))
      (when (argument-option nodes "--detach")
        (detach! (argument-option nodes "--log")))
      ;; Does not return: the daemon runs until it is told to go, or
      ;; until it finds a reason to leave and reports it.
      ((later '(theourgia daemon) 'serve) store socket)
      (exit 0))))

;; ---- leaving the caller behind ------------------------------------------
;;
;; KEY: THE ORDER IS THE POINT, and it is: new session, then stdio, then
;; the daemon's own work -- the lock and the socket, which `serve` does
;; next. Taking the lock first would mean a process that then failed to
;; detach had to give it back, and the window in which it held it is one
;; where a second client saw "somebody is already starting" and waited
;; for a daemon that was about to exit.
;;
;; NEVER: AND ONLY UNDER `--detach`. A `serve` run from a terminal keeps its
;; session and its output, because that is how it is read; making this
;; unconditional would take the output away from the one caller who
;; wants it, and would fail for that caller besides (see below).
;;
;; NOTE: `setsid` FAILS WHEN THE CALLER IS ALREADY A PROCESS GROUP LEADER,
;; which is the normal state of a process started from an interactive
;; shell: it answers EPERM. That is NOT swallowed. A process that could
;; not leave its session would die with the terminal that started it,
;; and a daemon that dies when a shell closes is worse than one that
;; never started -- the client waiting for it would have connected once,
;; been answered, and then found it gone.
;;
;; NOTE: THE WINDOW IS REAL AND IS NOT CLOSED HERE. Between the spawn and
;; the `setsid!` below, the child is still in the client's process group,
;; so a ctrl-C aimed at the client takes it too. The consequence is
;; bounded: the client's readiness test is a successful connection, so it
;; simply never becomes ready and reports that the daemon would not
;; start; the lock is a descriptor and closes with the process, so
;; nothing is left holding it. Closing the window needs the spawn itself
;; to set the session, which is `POSIX_SPAWN_SETSID` -- and that did not
;; exist on FreeBSD 15 as of 2026-09-18, when this was measured, which is
;; one of the two platforms this ships to. So the window stays, described,
;; rather than being closed on one platform and not the other.
;;
;; NOTE: THAT LAST SENTENCE IS AN OBSERVATION ABOUT A PLATFORM ON A DATE,
;; not a property of this program, and nothing here re-checks it. A later
;; FreeBSD can grow the flag without anything in this tree noticing. What
;; a row can and does check is the consequence: `test/detach.sc` asserts
;; the child ends up in its own session.
(define (detach-errno e)
  ;; NOTE: TWO SHAPES, BECAUSE THE TWO STEPS FAIL DIFFERENTLY. `setsid!`
  ;; raises an assertion violation carrying the errno as an irritant;
  ;; opening the log raises the file layer's own durable-error, which
  ;; holds it in a field. Reading only the first reported `unknown` for
  ;; every unwritable log directory -- a real case, measured -- while the
  ;; row asserting "it names the errno" still passed, because it was the
  ;; OTHER step that it exercised.
  ;;
  ;; NEVER: Not guessed: a detach that failed for a reason nobody recorded is
  ;; a daemon that will not start and will not say why.
  ;; NOTE: A THIRD SHAPE SINCE F100a. The log's create is a mutating
  ;; primitive and raises durable-error as before; its open, after the
  ;; create, is not, and raises unreadable-entry, whose errno is a field of
  ;; its own (a name such as EACCES where the file layer's is a number). It
  ;; is read before the irritants, which that condition also carries: the
  ;; first irritant there is the path, and the path is not an errno.
  (cond
    ((fs-error? e) (fs-error-errno e))
    ((unreadable-entry? e) (unreadable-entry-errno e))
    ((and (condition? e) (irritants-condition? e) (pair? (condition-irritants e)))
     (car (condition-irritants e)))
    (else 'unknown)))

(define (detach! log-path)
  ;; NEVER: NO LOG, NO DAEMON. A detached daemon with nowhere to write is one
  ;; whose every startup refusal is lost, and the client that started it
  ;; could then only report that it did not come up. Refusing here, while
  ;; the caller's stderr is still attached, is the last moment at which
  ;; anything can be said at all.
  (unless log-path
    ;; NEVER: AND THE USAGE CLAUSE NAMES THE ONE FORM, it does not write a
    ;; second one. This said `(usage (serve "--detach" "--log" <path>))`,
    ;; which is not a fragment of `serve-usage` but a different statement in
    ;; the same notation: `serve-usage` has `--detach` in brackets, meaning
    ;; optional, and that spelling had it bare, meaning required. It read as
    ;; "serve needs --detach --log <path>", which is false.
    ;;
    ;; Two clauses, two jobs: the tag says what went wrong here, and `usage`
    ;; says what the verb accepts. A partial form is the tag doing its work
    ;; in a notation that can say something untrue.
    (say (list 'error 'detach-needs-a-log (list 'usage serve-usage)))
    (exit 71))
  ;; NOTE: TWO STEPS, NAMED SEPARATELY. Both fail into the same exit and the
  ;; same tag, and written as one guard the answer could not say which
  ;; had happened -- "could not leave the session" and "could not open
  ;; the log" need different things done about them, and the caller is
  ;; usually a program.
  ;;
  ;; NOTE: THE `step` IS WHAT TELLS THEM APART, and both can carry an
  ;; errno. An earlier version of this comment said only `setsid` had one,
  ;; because the log was opened through the port layer; it is not any
  ;; more. `redirect-stdio!` opens with `fd-open` and raises `fs-err` with
  ;; `(errno)` on a failed `dup2`, and the handler above pulls that out
  ;; with `fs-error-errno`. What the field says is now the same question
  ;; on both steps: an errno if the failure had one, `unknown` if not.
  (detach-step 'setsid log-path (lambda () (setsid!)))
  (detach-step 'log log-path (lambda () (redirect-stdio! log-path))))

(define (detach-step step log-path thunk)
  (guard (e (#t
             (let ((code (detach-errno e)))
               (trace-event! 'detach-failed code #f)
               (say (list 'error 'detach-failed
                          (list 'step step)
                          (list 'path log-path)
                          (list 'errno code))))
             (exit 71)))
    (thunk)))

;; KEY: THE VERBS THIS PROGRAM ANSWERS, AS ONE TABLE THAT IS THE DISPATCH
;; (F64), in the shape `core.sc` keeps: `(cons '<verb> <procedure>)`, the
;; procedure taking the whole argv and not returning. `test/own-verbs.sc`
;; reads this define as data, so the gates count what `main` dispatches.
(define own-verbs
  (list (cons 'serve serve-and-exit!)))

(define (main argv)
  ;; NEVER: ONCE, BEFORE ANYTHING IS PRINTED, as in `core.sc`: every answer
  ;; this program prints goes through the same printing settings, so its
  ;; refusals are the bytes `core.sc serve` printed before the split.
  (answer-printing!)
  (let ((own (and (pair? argv) (assq (string->symbol (car argv)) own-verbs))))
    (if own
        ((cdr own) argv)
        (begin
          (say (list 'error 'bad-request '(reason not-a-daemon-verb)
                     (list 'usage serve-usage)))
          (exit 1)))))

(main (cdr (command-line)))
