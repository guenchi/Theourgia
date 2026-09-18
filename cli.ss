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
        (only (theourgia sched) start-scheduler) (theourgia net)
        (only (theourgia daemon) serve)
        (only (theourgia eval-supervise) supervise-eval)
        (only (theourgia working) working-snapshot working-baseline)
        (only (theourgia store) open-and-reduce))

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

(define (forward-then-exit! socket store actor verb resolved wire?)
  (start-scheduler
    (lambda ()
      (let ((outcome
              (exchange socket
                        ;; ⛔ THE ENVELOPE IS PACKED IN ONE PLACE, and this
                        ;; is not it. The MCP shell sends the same one.
                        (request-frame store actor verb (argument-strings resolved))
                        datum-line?
                        30000)))
        (cond
          ((and (pair? outcome) (eq? 'answer (car outcome)))
           (let ((answer (guard (e (#t 'unreadable))
                           (read (open-string-input-port (utf8->string (cadr outcome)))))))
             (if (eq? answer 'unreadable)
                 (finish '(error transport-unknown (reason unreadable-answer)) wire?)
                 (finish answer wire?))))
          ;; ⛔ THE SAME QUESTION THE MCP SHELL ASKS, ASKED IN ONE PLACE.
          ((transport-unreachable? outcome)
           (finish (rpc-dispatch-parsed store verb resolved actor) wire?))
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

(define serve-usage
  '(serve [<store>] ["--socket" <path>]))

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
                   (start-scheduler
                     (lambda ()
                       (finish
                         (supervise-eval
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
