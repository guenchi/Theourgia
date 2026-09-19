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
        ;; ⛔ ONLY WHAT IT USES, AND `link` IS NOT IN IT: see the same
        ;; note in daemon.ss. Forwarding runs one exchange and exits;
        ;; nothing here has a peer to be linked to.
        (only (theourgia render) answer-printing!)
        (only (theourgia client) socket-path answer-field readable-shape? exit-code?
              verb-spelling-error)
        (only (theourgia ffi)
              env-or setsid! redirect-stdio! trace-event!
              fs-error? fs-error-errno)
        (only (theourgia working) working-snapshot working-baseline)
        (only (theourgia store) open-and-reduce))

;; ---- what this program does NOT load until it has to -----------------------
;;
;; ⭐ EVERY `read` USED TO LOAD THE ACTOR SYSTEM. `(theourgia sched)`,
;; `(theourgia net)`, `(theourgia daemon)` and `(theourgia eval-supervise)`
;; were imported here unconditionally, for the benefit of `serve`, `eval`
;; and the forwarding path -- so a plain `theourgia read` paid to load
;; igropyr's scheduler and libuv machinery and then never used them.
;;
;; MEASURED, on this machine, against the same libraries, in BOTH forms
;; this ships in -- because the numbers are very different and only one
;; of them is a user's:
;;
;;     from source   static 623ms   on demand 435ms   -188ms
;;     from .so      static  51ms   on demand  42ms   -9ms
;;
;; ⚠️ MOST OF THAT 188ms IS EXPANSION, which compiled objects do not pay.
;; A user runs objects and saves about 9ms a call -- 18% of startup,
;; worth having, and ⛔ not the 200ms figure that a source-form reading
;; alone would have suggested. The structural point does not depend on
;; either number: a `read` has no business loading the daemon.
;;
;; ⚠️ AND THE COST IS ONE COST, NOT FOUR. `sched` alone is +164ms; `net`,
;; `daemon` and `eval-supervise` each depend on it and add about 36ms
;; between them. So the saving is "does this call need the actor system
;; at all", and nothing finer is worth arranging.
;;
;; ⚠️ `working` AND `store` STAY STATIC because they are already inside
;; `(theourgia rpc)`'s closure: importing them measured 453ms against the
;; base 452ms. Moving them would buy nothing and would say something
;; false about where the cost is.
;;
;; ⛔ WHAT THIS DOES NOT SAVE, said plainly: a call that FORWARDS to a
;; daemon still loads `net`, which still needs `sched` -- 637ms measured,
;; the same as before. The saving is for calls answered in this process.
;; A mechanism that also made forwarding cheap would have to not use the
;; actor system for the transport, which is a different design and not
;; this one.
(define (later lib name)
  (eval name (environment lib)))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; ---- argv ----------------------------------------------------------------

(define (environment-actor)
  (or (env-or "THEOURGIA_ACTOR") (env-or "USER") "cli"))

;; ⛔ THE BOUND WRITER IS AN IDENTITY THIS PROCESS CARRIES, NOT A WORD IN
;; THE COMMAND LINE. The thin client used to splice `--writer <name>` into
;; the arguments before handing them here, which broke every verb whose
;; grammar has no `--writer`: `theourgia init --store X` answered its
;; usage line and exited 1 for as long as an identity was bound. It is
;; read here and passed to the dispatcher as the default, which is the
;; same route the daemon's envelope takes, and the arguments are left
;; exactly as the caller wrote them.
;;
;; ⚠️ UNSET STAYS UNSET, and does not fall back to the actor -- a draft
;; verb with no writer is refused, and a fallback would mean that refusal
;; could never happen.
(define (environment-writer) (env-or "THEOURGIA_WRITER"))

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
;; ---- forwarding ----------------------------------------------------------
;;
;; ⛔ THE SAME LANGUAGE, OVER A SOCKET. A daemon answers with exactly
;; what this file would have produced locally -- it calls the same
;; dispatcher -- so forwarding is transport and nothing else: the request
;; is wrapped in an envelope naming the store it is for, and the answer
;; is printed by the same two procedures that print a local one. ⛔ There
;; is no rule here about what a verb means.
;;
;; ⛔ AND IT IS SCHEME, not a helper process. ⚠️ HISTORY, NOT CURRENT
;; BEHAVIOUR -- and spelled out because a reader took it for a
;; description of what runs today: **until batch E this exec'd into a
;; Python program, `local.py --forward`. That file no longer exists and
;; nothing here starts another process.** It was a second implementation
;; of the wire format, in another language, kept in step by hand, and the
;; envelope it spoke was already not the one the daemon speaks.
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

;; ---- what comes back --------------------------------------------------
;;
;;   (answer (stdout "<bytes>") (stderr "<bytes>") (exit <n>) (origin <who>))
;;
;; ⛔ THE BYTES ARE WRITTEN, NOT RE-RENDERED. The daemon rendered them,
;; in the mode this caller asked for; printing them through a printer
;; again would be a second rendering of something already rendered, and
;; the two would drift.
;;
;; ⛔ AND THE EXIT CODE IS TAKEN, NOT COMPUTED. Whether an answer counts
;; as a success is knowledge about what a verb means -- `check` turns on
;; its verdict, `batch` on every one of its items -- and the server is
;; where that knowledge is. Working it out again here would be a second
;; copy of `rpc-ok?`, and the copy would be the one that got it wrong.
;;
;; ⚠️ THE SHAPE IS CHECKED BEFORE IT IS BELIEVED. Something that is not
;; this shape is not an answer that came out badly, it is a peer that is
;; not the daemon or is not the same version of it, and that is
;; `transport-unknown`: the request may well have been carried out.
;; ⛔ WHAT CAME BACK WAS WRITTEN BY A PEER. `assq` demands a proper list
;; of pairs and raises on anything else, and the guard around the read
;; does not cover this: `(answer . broken)` reads perfectly well and then
;; raised "improperly formed alist" out of here, past the refusal three
;; lines down that exists to answer exactly this.
;;
;; ⛔ THE READER IS `answer-field`, IN THE LIBRARY. This file, the client
;; program and the MCP shell each had their own copy of the lookup, and
;; the same defect was fixed in two of them -- one rule with three
;; suppliers is how the third stayed wrong.
(define (answer-envelope? x)
  (and (answer-field x 'stdout string?)
       (answer-field x 'stderr string?)
       (answer-field x 'exit exit-code?)
       #t))

(define (envelope-field x name) (answer-field x name (lambda (v) #t)))

(define (finish-envelope envelope)
  (put-string (current-output-port) (envelope-field envelope 'stdout))
  (let ((err (envelope-field envelope 'stderr)))
    (unless (string=? err "")
      (put-string (current-error-port) err)))
  (exit (envelope-field envelope 'exit)))

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

(define (forward-then-exit! socket store actor verb resolved wire?)
  ;; ⚠️ THE WRITER AND THE MODE COME FROM THE SAME PARSE the verb's own
  ;; arguments came from. `--writer` stays in `resolved` as well, because
  ;; a per-call writer still overrides the envelope's on the other side;
  ;; what the envelope carries is the identity this PROCESS is bound to,
  ;; which today is only what the command line said.
  ((later '(theourgia sched) 'start-scheduler)
    (lambda ()
      (let ((outcome
              ((later '(theourgia net) 'exchange) socket
                        ;; ⛔ THE ENVELOPE IS PACKED IN ONE PLACE, and this
                        ;; is not it. The MCP shell sends the same one.
                        ;; ⚠️ AND WHERE THIS PROCESS IS. A forwarded request
                        ;; is carried out by a daemon started from some other
                        ;; directory, so a relative path in it would be read
                        ;; there rather than here. Standard input is not sent:
                        ;; this program parses its own arguments and has
                        ;; already put what was piped in where the verb wants
                        ;; it.
                        (request-frame store verb (argument-strings resolved)
                                       (list (cons 'actor actor)
                                             (cons 'writer (argument-option resolved "--writer"))
                                             (cons 'cwd (current-directory))
                                             (cons 'mode (if wire? 'wire 'human))))
                        datum-line?
                        30000)))
        (cond
          ((and (pair? outcome) (eq? 'answer (car outcome)))
           (let ((envelope (guard (e (#t 'unreadable))
                             (let ((text (utf8->string (cadr outcome))))
                               ;; ⛔ ASKED BEFORE THE READER IS HANDED IT:
                               ;; a datum label makes a cycle that every
                               ;; later walk follows forever.
                               (if (readable-shape? text)
                                   (read (open-string-input-port text))
                                   'unreadable)))))
             (if (answer-envelope? envelope)
                 (finish-envelope envelope)
                 (finish '(error transport-unknown (reason unreadable-answer)) wire?))))
          ;; ⛔ THE SAME QUESTION THE MCP SHELL ASKS, ASKED IN ONE PLACE.
          ((transport-unreachable? outcome)
           (finish (rpc-dispatch-parsed store verb resolved actor #f (environment-writer)) wire?))
          (else
           (finish (list 'error 'transport-unknown (list 'reason 'lost-answer)) wire?)))))))

;; ---- eval --------------------------------------------------------------
;;
;; ⛔ THE SUPERVISOR RUNS HERE, IN THIS PROCESS. The child is owned by an
;; adapter, so its ending is a `#(DOWN …)` and there is nothing to reap by
;; hand -- and there is no helper process between this program and the
;; child.
;;
;; ⚠️ THE LIMITS ARE CHECKED BEFORE ANYTHING IS SPAWNED, and a limit out
;; of range is a bad request rather than a clamped run: a caller that
;; asked for something impossible should hear so, not silently get
;; something else.
(define eval-defaults
  '((timeout-ms . 3000) (memory-bytes . 268435456) (output-bytes . 65536)))

(define (bounded name value low high)
  (and value (exact? value) (integer? value) (<= low value high) value))

(define (eval-number nodes name fallback low high)
  (let ((given (argument-option nodes name)))
    (if (not given)
        fallback
        (bounded name (string->number given) low high))))

(define (read-source nodes)
  (let ((positional (argument-positionals nodes)))
    (if (pair? positional)
        (car positional)
        (read-all-text (current-input-port)))))

;; ⛔ THE VIEW IS FIXED BEFORE THE CHILD EXISTS, or it is not a view at
;; all. Without `--working` there is nothing to overlay and the answer is
;; the committed store at that cut. With it, the writer's live drafts are
;; copied out here, under that writer's own lock -- so a commit landing
;; while the evaluation runs cannot change what was evaluated.
;;
;; ⚠️ AND THE LOCK IS HELD ONLY FOR THE COPY. The run itself takes no
;; lock; what crosses into the worker is bytes, not a handle.
;; WHICH COMMITTED STATE THE EVALUATION STANDS ON.
;;
;; ⛔ `--working` IS PINNED BY DEFAULT, and that is the whole of the
;; difference this settles. A writer's working view stands on the state
;; ITS OWN drafts record -- the join of their cuts -- so a commit another
;; writer made after those drafts does not walk into it. Until this batch
;; the tree always evaluated at the CURRENT committed state, which is the
;; opposite: the same unchanged draft could answer differently because
;; somebody else committed in between, and `--latest` (which asks for
;; exactly that) was accepted and read by nothing.
;;
;; ⚠️ A WRITER WITH NO DRAFTS HAS NO BASELINE and gets the current state.
;; There is nothing for it to be pinned to, and an empty cut is not a
;; coordinate.
;;
;; ⛔ AN EXPLICIT `--cut` WINS over both. The caller named a coordinate;
;; nothing here may move it.
(define (eval-cut nodes store)
  (let ((given (argument-option nodes "--cut")))
    (cond
      (given given)
      ((or (not (argument-option nodes "--working"))
           (argument-option nodes "--latest"))
       "")
      (else
       (let ((answer (working-baseline store #f (argument-option nodes "--writer"))))
         (if (and (pair? answer) (eq? 'ok (car answer)) (pair? (caddr answer)))
             (cut->text (caddr answer))
             ;; A writer that cannot be resolved, or one with no drafts:
             ;; the refusal belongs to the view, which is built next and
             ;; carries it, so the cut is simply the current state.
             ""))))))

;; ⚠️ THE SAME SPELLING `--cut` IS READ IN. `parse-cut` accepts
;; `(("writer" . 3))`, which is what `write` produces for the alist a
;; cut is, so the text this makes can be handed back to the CLI by a
;; caller who read it out of an answer.
(define (cut->text c)
  (let-values (((port get) (open-string-output-port)))
    (write c port)
    (get)))

(define (eval-view nodes store cut)
  (if (not (argument-option nodes "--working"))
      (list 'working #f #f '())
      (let* ((state (open-and-reduce store))
             (answer (working-snapshot store state (argument-option nodes "--writer"))))
        (if (and (pair? answer) (eq? 'ok (car answer)))
            (list 'working (cadr answer) cut (caddr answer))
            ;; A writer that cannot be resolved is the core's refusal, and
            ;; it is carried as an empty view so the worker answers it the
            ;; same way any other bad request is answered.
            (list 'working #f cut '())))))

;; ⛔ THE SPELLING IS ADVERTISED WHERE IT IS REFUSED. These two verbs are
;; the CLI's own -- they are not in `rpc-verbs`, so the dispatcher's
;; usage forms say nothing about them -- and until this batch a caller
;; who misspelled an option got a refusal that named no alternative.
;; `options-gate.ss` reads these two forms as data and checks every
;; option in them against `parse-arguments`, in both directions, which
;; is what makes them a claim rather than a comment: the bug that
;; prompted the gate was `eval --timeout-ms` parsing as a positional
;; because the option table had no `eval` entry at all.
;;
;; ⚠️ ONLY VERB-SPECIFIC OPTIONS BELONG HERE. `--store`, `--wire`,
;; `--actor`, `--req`, `--cursor` and `--socket` are accepted for every
;; verb by the common part of the table, and listing them in one usage
;; form would suggest they are special to it.
(define eval-usage
  '(eval ["--cut" <cut>] ["--under" <library>] ["--working"] ["--latest"]
         ["--writer" <name>] ["--timeout-ms" <n>] ["--memory-bytes" <n>]
         ["--output-bytes" <n>] <source>))

;; ⚠️ `--detach` IS FOR A LAUNCHER, NOT FOR A PERSON. It leaves the
;; caller's session and replaces stdio; it does NOT fork. Typed at a
;; prompt it stops there, silently, because the output it would have
;; shown has already been redirected to the log.
(define serve-usage
  '(serve [<store>] ["--socket" <path>]
          ["--detach" "--log" <path> (started-by-a-client-not-by-hand)]))

(define (eval-and-exit! argv)
  (let ((nodes (parse-arguments 'eval (cdr argv))))
    (if (and (pair? nodes) (eq? (car nodes) 'error))
        (finish (list 'error 'bad-request '(reason eval-arguments)
                      (list 'usage eval-usage))
                #f)
        (let ((timeout (eval-number nodes "--timeout-ms" 3000 1 60000))
              (memory (eval-number nodes "--memory-bytes" 268435456 1048576 2147483648))
              (output (eval-number nodes "--output-bytes" 65536 128 1048576))
              (store (or (argument-option nodes "--store") (getenv "THEOURGIA_STORE") "."))
              (cut (eval-cut nodes (or (argument-option nodes "--store")
                                       (getenv "THEOURGIA_STORE") ".")))
              (under (or (argument-option nodes "--under") ""))
              (wire? (argument-option nodes "--wire")))
          (cond
            ;; ⛔ TWO ANSWERS TO ONE QUESTION. `--cut` names a coordinate
            ;; and `--latest` asks for whichever one is current; a rule
            ;; giving one of them precedence would make the other silently
            ;; do nothing, which is the defect this batch just removed.
            ((and (argument-option nodes "--latest") (argument-option nodes "--cut"))
             (finish (list 'error 'bad-request '(reason cut-and-latest)
                           (list 'usage eval-usage))
                     wire?))
            ((not (and timeout memory output))
             (finish (list 'error 'bad-request '(reason eval-arguments)
                           (list 'usage eval-usage))
                     wire?))
            (else
             (let ((source (read-source nodes)))
               (if (> (string-length source) 1048576)
                   (finish '(error bad-source (reason input-limit)) wire?)
                   ((later '(theourgia sched) 'start-scheduler)
                     (lambda ()
                       (finish
                         ((later '(theourgia eval-supervise) 'supervise-eval)
                           (list (cons 'store store) (cons 'cut cut) (cons 'under under)
                                 (cons 'source source)
                                 (cons 'timeout-ms timeout)
                                 (cons 'memory-bytes memory)
                                 (cons 'output-bytes output)
                                 (cons 'view (eval-view nodes store cut))
                                 (cons 'scheme (scheme-binary))
                                 (cons 'worker (beside-this-program "eval-worker.ss"))))
                         wire?)))))))))))

;; ⚠️ THE INTERPRETER THIS PROGRAM IS ITSELF RUNNING UNDER, so a tree
;; started with a particular Chez starts its children with the same one.
(define (scheme-binary)
  (or (getenv "THEOURGIA_SCHEME") "scheme"))

(define (beside-this-program name)
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (string-append (substring argv0 0 cut) "/" name) name)))

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
        (begin (say nodes) (say (list 'usage serve-usage)) (exit 1))
        (let* ((positional (argument-positionals nodes))
               (store (or (argument-option nodes "--store")
                          (and (pair? positional) (car positional))
                          (getenv "THEOURGIA_STORE")
                          "."))
               ;; ⛔ THE SHARED RULE, NOT A SECOND ONE. This used to
               ;; default to `<store>/socket`, which bypassed the
               ;; daemon's own function entirely -- so the rule the
               ;; README documented was never the rule that ran.
               (socket (or (argument-option nodes "--socket")
                           (socket-path store))))
          ;; ⛔ AN EMPTY SOCKET PATH IS REFUSED RATHER THAN TRIED. It is
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
          (when (argument-option nodes "--detach")
            (detach! (argument-option nodes "--log")))
          ;; Does not return: the daemon runs until it is told to go, or
          ;; until it finds a reason to leave and reports it.
          ((later '(theourgia daemon) 'serve) store socket)
          (exit 0)))))

;; ---- leaving the caller behind ------------------------------------------
;;
;; ⭐ THE ORDER IS THE POINT, and it is: new session, then stdio, then
;; the daemon's own work -- the lock and the socket, which `serve` does
;; next. Taking the lock first would mean a process that then failed to
;; detach had to give it back, and the window in which it held it is one
;; where a second client saw "somebody is already starting" and waited
;; for a daemon that was about to exit.
;;
;; ⛔ AND ONLY UNDER `--detach`. A `serve` run from a terminal keeps its
;; session and its output, because that is how it is read; making this
;; unconditional would take the output away from the one caller who
;; wants it, and would fail for that caller besides (see below).
;;
;; ⚠️ `setsid` FAILS WHEN THE CALLER IS ALREADY A PROCESS GROUP LEADER,
;; which is the normal state of a process started from an interactive
;; shell: it answers EPERM. That is NOT swallowed. A process that could
;; not leave its session would die with the terminal that started it,
;; and a daemon that dies when a shell closes is worse than one that
;; never started -- the client waiting for it would have connected once,
;; been answered, and then found it gone.
;;
;; ⚠️ THE WINDOW IS REAL AND IS NOT CLOSED HERE. Between the spawn and
;; the `setsid!` below, the child is still in the client's process group,
;; so a ctrl-C aimed at the client takes it too. The consequence is
;; bounded: the client's readiness test is a successful connection, so it
;; simply never becomes ready and reports that the daemon would not
;; start; the lock is a descriptor and closes with the process, so
;; nothing is left holding it. Closing the window needs the spawn itself
;; to set the session, which is `POSIX_SPAWN_SETSID` -- and that does not
;; exist on FreeBSD 15 (measured 2026-09-18), which is one of the two
;; platforms this ships to. So the window stays, described, rather than
;; being closed on one platform and not the other.
(define (detach-errno e)
  ;; ⚠️ TWO SHAPES, BECAUSE THE TWO STEPS FAIL DIFFERENTLY. `setsid!`
  ;; raises an assertion violation carrying the errno as an irritant;
  ;; opening the log raises the file layer's own durable-error, which
  ;; holds it in a field. Reading only the first reported `unknown` for
  ;; every unwritable log directory -- a real case, measured -- while the
  ;; row asserting "it names the errno" still passed, because it was the
  ;; OTHER step that it exercised.
  ;;
  ;; ⛔ Not guessed: a detach that failed for a reason nobody recorded is
  ;; a daemon that will not start and will not say why.
  (cond
    ((fs-error? e) (fs-error-errno e))
    ((and (condition? e) (irritants-condition? e) (pair? (condition-irritants e)))
     (car (condition-irritants e)))
    (else 'unknown)))

(define (detach! log-path)
  ;; ⛔ NO LOG, NO DAEMON. A detached daemon with nowhere to write is one
  ;; whose every startup refusal is lost, and the client that started it
  ;; could then only report that it did not come up. Refusing here, while
  ;; the caller's stderr is still attached, is the last moment at which
  ;; anything can be said at all.
  (unless log-path
    (say '(error detach-needs-a-log (usage (serve "--detach" "--log" <path>))))
    (exit 71))
  ;; ⚠️ TWO STEPS, NAMED SEPARATELY. Both fail into the same exit and the
  ;; same tag, and written as one guard the answer could not say which
  ;; had happened -- "could not leave the session" and "could not open
  ;; the log" need different things done about them, and the caller is
  ;; usually a program.
  ;;
  ;; ⚠️ AND ONLY THE FIRST HAS AN ERRNO. `setsid` is a syscall and
  ;; reports one; the log is created through the port layer, which has no
  ;; errno to give, so that field is honestly #f there rather than a
  ;; number invented to fill it. The `step` is what tells them apart.
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

(define (main argv)
  ;; ⛔ ONCE, BEFORE ANYTHING IS PRINTED. Every answer this program gives
  ;; -- local, forwarded, wire or human -- goes through `render-wire`,
  ;; and this is where its settings are decided.
  (answer-printing!)
  (when (null? argv)
    (say '(usage (theourgia <verb> ...)))
    (exit 1))
  ;; ⛔ BOTH OF THESE ARE THIS PROGRAM NOW. `serve` used to exec a Python
  ;; daemon and `eval` a Python supervisor; neither exists. There is no
  ;; second implementation of either to keep in step.
  ;;
  ;; ⛔ AND `eval` NEVER GOES THROUGH A DAEMON. It is intercepted here,
  ;; before the forwarding below, so it always runs in this process --
  ;; and a request naming `eval` that reaches a daemon meets a dispatcher
  ;; that has no such verb, which is the same answer any unknown verb
  ;; gets. Those two facts are the whole of E1-6.
  (when (string=? (car argv) "serve") (serve-and-exit! argv))
  (when (string=? (car argv) "eval") (eval-and-exit! argv))
  ;; ⛔ THE SPELLING IS JUDGED FIRST, BEFORE THE ARGUMENTS ARE PARSED, and
  ;; the order follows from a fact already settled rather than from taste:
  ;; the thin client knows no verb's option table -- that is what makes it
  ;; thin -- so it can never answer `missing-option-value` for a
  ;; verb-specific option, while this program can. The ONE order the two
  ;; programs can share is the check that needs no table, and that is this
  ;; one: a spelling is judged by the writer alone.
  ;;
  ;; ⚠️ MEASURED WITH IT AFTER THE PARSE: `show me --store` answered
  ;; `(error bad-request missing-option-value "--store")` here and
  ;; `(error bad-request unknown-verb (spelling "show me"))` from the thin
  ;; client -- two programs, one argv, two answers.
  ;;
  ;; ⚠️ AND `--wire` IS FOUND BY LOOKING, because the parser has not run
  ;; yet. That is what the thin client's own scanner does with this
  ;; option, so the two agree here too.
  ;; ⚠️ MEASURED: it makes no difference to THIS answer today -- an error
  ;; renders the same in both modes, and only an `ok` answer differs. The
  ;; mode is passed anyway because it is the mode the caller asked for,
  ;; and a refusal that ignored it would be right only for as long as
  ;; error rendering happens to match.
  (let ((spelling-error (verb-spelling-error (car argv))))
    (when spelling-error
      (print-answer spelling-error (and (member "--wire" argv) #t))
      (exit 1)))
  (let* ((verb (string->symbol (car argv)))
         (nodes (parse-arguments verb (cdr argv)))
         (answer
           (if (and (pair? nodes) (eq? (car nodes) 'error)) nodes
               (let* ((store (or (argument-option nodes "--store")
                                (getenv "THEOURGIA_STORE") "."))
                     (actor (or (argument-option nodes "--actor") (environment-actor)))
                     (socket-path* (or (argument-option nodes "--socket") (socket-path store)))
                     (resolved (argument-stdin verb (argument-remove nodes '("--store" "--actor" "--wire" "--socket"))
                                 (lambda () (read-all-text (current-input-port))))))
                 (when (and (not (equal? (getenv "THEOURGIA_LOCAL") "1")) (file-exists? socket-path*))
                   (forward-then-exit! socket-path* store actor verb resolved
                                       (argument-option nodes "--wire")))
                 (rpc-dispatch-parsed store verb resolved actor #f (environment-writer))))))
    (print-answer answer (and (list? nodes) (not (and (pair? nodes) (eq? (car nodes) 'error))) (argument-option nodes "--wire")))
    (exit (if (rpc-ok? answer) 0 1))))

(main (cdr (command-line)))
