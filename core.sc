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
        ;; NEVER: ONLY WHAT IT USES, AND `link` IS NOT IN IT: see the same
        ;; note in daemon.sc. Forwarding runs one exchange and exits;
        ;; nothing here has a peer to be linked to.
        (only (theourgia render) answer-printing!)
        (only (theourgia client) socket-path answer-field readable-shape? exit-code?
              verb-spelling-error eval-admit!)
        (only (theourgia ffi) env-or entry-type with-mutation-record mutation-record)
        (only (theourgia answers) classify-failure combine-report)
        (only (theourgia working) working-snapshot working-baseline)
        (only (theourgia store) open-and-reduce)
        (only (theourgia log) load-listener-add! load-declaration-set! load-refused?
              merge-unreadable incomplete-clause)
        (only (theourgia incomplete) incomplete-accepted clause->note)
        (only (theourgia languages) language-for-name language-runner))

;; ---- what this program does NOT load until it has to -----------------------
;;
;; KEY: EVERY `read` USED TO LOAD THE ACTOR SYSTEM. `(theourgia sched)`,
;; `(theourgia net)`, `(theourgia daemon)` and `(theourgia eval-supervise)`
;; were imported here unconditionally, for the benefit of `serve`, `eval`
;; and the forwarding path -- so a plain `theourgia read` paid to load
;; igropyr's scheduler and libuv machinery and then never used them.
;;
;; MEASURED AND RECORDED 2026-09-18, on the development machine (Darwin
;; arm64, Chez machine type `tarm64osx`), against the same libraries, in
;; BOTH forms this ships in -- because the numbers are very different and
;; only one of them is a user's:
;;
;;     from source   static 623ms   on demand 435ms   -188ms
;;     from .so      static  51ms   on demand  42ms   -9ms
;;
;; NOTE: MOST OF THAT 188ms IS EXPANSION, which compiled objects do not pay.
;; A user runs objects and saves about 9ms a call -- 18% of startup,
;; worth having, and NEVER: not the 200ms figure that a source-form reading
;; alone would have suggested. The structural point does not depend on
;; either number: a `read` has no business loading the daemon. Since F46 it
;; cannot: `serve` is `theourgiad.sc`'s, and this program no longer reaches
;; `(theourgia daemon)` at all.
;;
;; NOTE: AND THE COST IS ONE COST, NOT FOUR. `sched` alone is +164ms; `net`,
;; `daemon` and `eval-supervise` each depend on it and add about 36ms
;; between them. So the saving is "does this call need the actor system
;; at all", and nothing finer is worth arranging.
;;
;; NOTE: `working` AND `store` STAY STATIC because they are already inside
;; `(theourgia rpc)`'s closure: importing them measured 453ms against the
;; base 452ms. Moving them would buy nothing and would say something
;; false about where the cost is.
;;
;; NEVER: WHAT THIS DOES NOT SAVE, said plainly: a call that FORWARDS to a
;; daemon still loads `net`, which still needs `sched` -- 637ms measured,
;; the same as before. The saving is for calls answered in this process.
;;
;; NOTE: EVERY FIGURE ABOVE IS HISTORY, AS OF 2026-09-18. Nothing
;; re-measures them and no row turns red when they stop being true; they
;; are kept because they are why the imports are arranged this way, not
;; as a present-day claim about what this program costs. The structural
;; statement at the top of this block is the part that does not decay.
;; A mechanism that also made forwarding cheap would have to not use the
;; actor system for the transport, which is a different design and not
;; this one.
(define (later lib name)
  (eval name (environment lib)))

(define (say x) (write x (current-output-port)) (newline (current-output-port)))

;; ---- argv ----------------------------------------------------------------

(define (environment-actor)
  (or (env-or "THEOURGIA_ACTOR") (env-or "USER") "cli"))

;; NEVER: THE BOUND WRITER IS AN IDENTITY THIS PROCESS CARRIES, NOT A WORD IN
;; THE COMMAND LINE. The thin client used to splice `--writer <name>` into
;; the arguments before handing them here, which broke every verb whose
;; grammar has no `--writer`: `theourgia init --store X` answered its
;; usage line and exited 1 for as long as an identity was bound. It is
;; read here and passed to the dispatcher as the default, which is the
;; same route the daemon's envelope takes, and the arguments are left
;; exactly as the caller wrote them.
;;
;; NOTE: UNSET STAYS UNSET, and does not fall back to the actor -- a draft
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
;; NEVER: THE SAME LANGUAGE, OVER A SOCKET. A daemon answers with exactly
;; what this file would have produced locally -- it calls the same
;; dispatcher -- so forwarding is transport and nothing else: the request
;; is wrapped in an envelope naming the store it is for, and the answer
;; is printed by the same two procedures that print a local one. NEVER: There
;; is no rule here about what a verb means.
;;
;; NEVER: AND IT IS SCHEME, not a helper process. NOTE: HISTORY, NOT CURRENT
;; BEHAVIOUR -- and spelled out because a reader took it for a
;; description of what runs today: **until batch E this exec'd into a
;; Python program, `local.py --forward`. That file no longer exists and
;; nothing here starts another process.** It was a second implementation
;; of the wire format, in another language, kept in step by hand, and the
;; envelope it spoke was already not the one the daemon speaks.
;;
;; NOTE: THIS NEVER RETURNS. `start-scheduler` does not: it ends in a loop
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
;; NEVER: THE BYTES ARE WRITTEN, NOT RE-RENDERED. The daemon rendered them,
;; in the mode this caller asked for; printing them through a printer
;; again would be a second rendering of something already rendered, and
;; the two would drift.
;;
;; NEVER: AND THE EXIT CODE IS TAKEN, NOT COMPUTED. Whether an answer counts
;; as a success is knowledge about what a verb means -- `check` turns on
;; its verdict, `batch` on every one of its items -- and the server is
;; where that knowledge is. Working it out again here would be a second
;; copy of `rpc-ok?`, and the copy would be the one that got it wrong.
;;
;; NOTE: THE SHAPE IS CHECKED BEFORE IT IS BELIEVED. Something that is not
;; this shape is not an answer that came out badly, it is a peer that is
;; not the daemon or is not the same version of it, and that is
;; `transport-unknown`: the request may well have been carried out.
;; NEVER: WHAT CAME BACK WAS WRITTEN BY A PEER. `assq` demands a proper list
;; of pairs and raises on anything else, and the guard around the read
;; does not cover this: `(answer . broken)` reads perfectly well and then
;; raised "improperly formed alist" out of here, past the refusal three
;; lines down that exists to answer exactly this.
;;
;; NEVER: THE READER IS `answer-field`, IN THE LIBRARY. This file, the client
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

;; NEVER: NOT REACHING ANYONE IS A REASON TO RUN LOCALLY; LOSING AN ANSWER IS
;; NOT. A socket file with nothing behind it is a daemon that has gone --
;; the request reached nobody, so running it here does the work once. But
;; once bytes have gone out, the request MAY have been carried out, and a
;; caller told "that did not happen" would do it again. NEVER: Something that
;; may have happened must never be reported as not having happened, so
;; that case gets `transport-unknown`, which means "ask me again".
;;
;; NOTE: AND THE TWO ARE TOLD APART BY THE CONNECT ERRNO, which is the only
;; thing `exchange` gives that distinguishes them: it reports both as
;; `(transport-error <status>)`. Measured against this daemon: a socket
;; path with nothing there and a stale socket file whose daemon was
;; killed. NOTE: A read failure mid-answer reports a different errno and
;; falls to the `transport-unknown` branch, which is the safe side.

(define (forward-then-exit! socket store actor verb resolved wire?)
  ;; NOTE: THE WRITER AND THE MODE COME FROM THE SAME PARSE the verb's own
  ;; arguments came from. `--writer` stays in `resolved` as well, because
  ;; a per-call writer still overrides the envelope's on the other side;
  ;; what the envelope carries is the identity this PROCESS is bound to,
  ;; which today is only what the command line said.
  ((later '(theourgia sched) 'start-scheduler)
    (lambda ()
      (let ((outcome
              ((later '(theourgia net) 'exchange) socket
                        ;; NEVER: THE ENVELOPE IS PACKED IN ONE PLACE, and this
                        ;; is not it. The MCP shell sends the same one.
                        ;; NOTE: AND WHERE THIS PROCESS IS. A forwarded request
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
                               ;; NEVER: ASKED BEFORE THE READER IS HANDED IT:
                               ;; a datum label makes a cycle that every
                               ;; later walk follows forever.
                               (if (readable-shape? text)
                                   (read (open-string-input-port text))
                                   'unreadable)))))
             (if (answer-envelope? envelope)
                 (finish-envelope envelope)
                 (finish '(error transport-unknown (reason unreadable-answer)) wire?))))
          ;; NEVER: THE SAME QUESTION THE MCP SHELL ASKS, ASKED IN ONE PLACE.
          ((transport-unreachable? outcome)
           (finish (rpc-dispatch-parsed store verb resolved actor #f (environment-writer)) wire?))
          (else
           (finish (list 'error 'transport-unknown (list 'reason 'lost-answer)) wire?)))))))

;; ---- eval --------------------------------------------------------------
;;
;; NEVER: THE SUPERVISOR RUNS HERE, IN THIS PROCESS. The child is owned by an
;; adapter, so its ending is a `#(DOWN …)` and there is nothing to reap by
;; hand -- and there is no helper process between this program and the
;; child.
;;
;; NOTE: THE LIMITS ARE CHECKED BEFORE ANYTHING IS SPAWNED, and a limit out
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

;; NEVER: THE VIEW IS FIXED BEFORE THE CHILD EXISTS, or it is not a view at
;; all. Without `--working` there is nothing to overlay and the answer is
;; the committed store at that cut. With it, the writer's live drafts are
;; copied out here, under that writer's own lock -- so a commit landing
;; while the evaluation runs cannot change what was evaluated.
;;
;; NOTE: AND THE LOCK IS HELD ONLY FOR THE COPY. The run itself takes no
;; store or draft lock; what crosses into the worker is bytes, not a
;; handle. The one lock an evaluation holds is its admission slot
;; (client.sc eval-admit!), which is neither: it is under the run root, it
;; bounds how many evaluations run at once, and it ends with this process.
;; WHICH COMMITTED STATE THE EVALUATION STANDS ON.
;;
;; NEVER: `--working` IS PINNED BY DEFAULT, and that is the whole of the
;; difference this settles. A writer's working view stands on the state
;; ITS OWN drafts record -- the join of their cuts -- so a commit another
;; writer made after those drafts does not walk into it. Until this batch
;; the tree always evaluated at the CURRENT committed state, which is the
;; opposite: the same unchanged draft could answer differently because
;; somebody else committed in between, and `--latest` (which asks for
;; exactly that) was accepted and read by nothing.
;;
;; NOTE: A WRITER WITH NO DRAFTS HAS NO BASELINE and gets the current state.
;; There is nothing for it to be pinned to, and an empty cut is not a
;; coordinate.
;;
;; NEVER: AN EXPLICIT `--cut` WINS over both. The caller named a coordinate;
;; nothing here may move it.
(define (eval-cut nodes store)
  (let ((given (argument-option nodes "--cut")))
    (cond
      (given given)
      ((or (not (argument-option nodes "--working"))
           (argument-option nodes "--latest"))
       "")
      (else
       (let ((answer (working-baseline store #f (eval-writer nodes))))
         (cond
           ((and (pair? answer) (eq? 'ok (car answer)) (pair? (caddr answer)))
            (cut->text (caddr answer)))
           ;; A REFUSAL IS THE EVAL'S ANSWER (F100b, D19): it was discarded
           ;; here, the view below met the same refusal and discarded it
           ;; too, and the source ran against an empty view -- `(ok ...)`
           ;; for a draft lock nobody could open. The caller answers it
           ;; with `(during cut)`.
           ((and (pair? answer) (eq? 'error (car answer)))
            (list 'refused answer))
           ;; A writer with no drafts: the current state.
           (else "")))))))

;; NOTE: THE SAME SPELLING `--cut` IS READ IN. `parse-cut` accepts
;; `(("writer" . 3))`, which is what `write` produces for the alist a
;; cut is, so the text this makes can be handed back to the CLI by a
;; caller who read it out of an answer.
(define (cut->text c)
  (let-values (((port get) (open-string-output-port)))
    (write c port)
    (get)))

;; THE EVAL ROUTE'S OWN SCOPE (F77c; design review r2, finding 4; code review
;; r1). This process obtains state twice before the worker exists -- the
;; cut's baseline and the working view. The store string it hands those two
;; is a copy only this request holds, carrying eval's DECLARATION (eval
;; carries what its reduction is missing into its answer) and a listener for
;; the NOTES those loads hear. It names no refusal of its own: eval declares,
;; so no load here is refused for being incomplete, and working.sc's
;; `problem` already names an entry it could not read (code review r1: an
;; override here changed a healthy store's answer and added nothing).
;; -> (values key heard-thunk)
(define (eval-scope store)
  (let ((key (string-copy store)) (heard '()))
    (load-listener-add! key
      (lambda (event)
        (unless (load-refused? event)
          (set! heard (merge-unreadable heard event)))))
    (load-declaration-set! key incomplete-accepted)
    (values key (lambda () heard))))

;; ONE incomplete CLAUSE PER ANSWER (code review r1): the notes this process
;; heard join the answer's own clause -- the worker's, when it heard any --
;; rather than adding a second one. The worker's clauses come back to notes
;; through clause->note, the inverse of their spelling, so a cut keeps its
;; kind and its after through the merge.
(define (with-heard-clause answer heard)
  (if (or (null? heard) (not (and (pair? answer) (list? answer))))
      answer
      (let* ((old (find (lambda (c) (and (pair? c) (eq? (car c) 'incomplete))) (cdr answer)))
             (old-notes (if old
                            (filter (lambda (n) n) (map clause->note (cdr old)))
                            '())))
        (append (remp (lambda (c) (eq? c old)) answer)
                (list (incomplete-clause (merge-unreadable old-notes heard)))))))

(define (eval-view nodes store cut)
  (if (not (argument-option nodes "--working"))
      (list 'working #f #f '())
      (let* ((state (open-and-reduce store))
             (answer (working-snapshot store state (eval-writer nodes))))
        (if (and (pair? answer) (eq? 'ok (car answer)))
            (list 'working (cadr answer) cut (caddr answer))
            ;; A REFUSAL IS THE EVAL'S ANSWER (F100b, D19), answered by the
            ;; caller with `(during view)`; it was carried as an empty view
            ;; and the source ran against nothing.
            (list 'refused answer)))))

;; THE MODE AN EVALUATION ANSWERS IN: `--wire` on its command line, else
;; THEOURGIA_WIRE=1 in its environment. The variable exists for a caller
;; that runs this program as a child and must not add a word to the
;; caller's own argv -- the MCP shell, whose child's argv is the tool
;; call's argv and nothing else -- and it is read for `eval` only: every
;; other verb's mode is its own `--wire`, as before. A parse error is
;; answered in the variable's mode, since an argv that did not parse
;; cannot be trusted to say where `--wire` is.
(define (environment-wire?) (equal? (getenv "THEOURGIA_WIRE") "1"))
(define (eval-wire? nodes)
  (or (and (argument-option nodes "--wire") #t) (environment-wire?)))

;; THE WRITER AN EVALUATION'S VIEW IS FOR, BY THE DISPATCHER'S RULE: an
;; explicit `--writer`, else THEOURGIA_WRITER when it is set and not empty,
;; and never the actor. Every place eval asks -- the cut's baseline, the
;; working view, a foreign runner's projection -- asks here, so the two
;; branches cannot come to different writers. With neither there is no
;; writer, and `--working` is refused writer-required, as before.
;;
;; NOTE: A CHANGE TO THE COMMAND LINE, deliberately: `eval --working` with
;; THEOURGIA_WRITER set and no `--writer` used to refuse writer-required,
;; because eval read `--writer` from argv alone, while every other verb
;; took the variable. It now selects that writer, as they do.
(define (eval-writer nodes)
  (or (argument-option nodes "--writer") (environment-writer)))

;; NOTE: `eval-usage` IS rpc.sc'S, imported with the rest of that library.
;; It moved there when the catalogue began to publish eval's entry (route
;; `child`), so the form a caller reads in `describe` and the form these
;; refusals carry are one definition, not two that can drift.

;; A LANGUAGE OTHER THAN SCHEME, or #f. `--lang scheme` is today's evaluation
;; exactly, with all its options; any other name takes the runner path.
(define (foreign-lang nodes)
  (let ((l (argument-option nodes "--lang")))
    (and l (not (string=? l "scheme")) l)))

;; RUNNERS ARE OFF UNLESS THE OPERATOR TURNS THEM ON. eval runs in this
;; process, so the gate is this process's environment, and it is exactly
;; the value "on": a runner reaches whatever its interpreter can reach on
;; this machine, which the Scheme sandbox does not limit.
(define (runners-enabled?)
  (let ((v (getenv "THEOURGIA_RUNNERS")))
    (and v (string=? v "on"))))

;; `eval --lang <l>` FOR A FOREIGN LANGUAGE. Every exit here carries what the
;; process heard, as every eval exit does. The conflicts are refused first:
;; the runner projects the committed state or the writer's working view, so
;; a cut and a library have nothing to name.
(define (eval-foreign-and-exit! nodes lang key heard timeout memory output)
  (let* ((wire? (eval-wire? nodes))
         (done (lambda (a) (finish (with-heard-clause a (heard)) wire?)))
         (entry (language-for-name lang)))
    (cond
      ((or (argument-option nodes "--cut") (argument-option nodes "--latest"))
       (done (list 'error 'bad-request '(reason lang-and-cut)
                         (list 'usage eval-usage))))
      ((argument-option nodes "--under")
       (done (list 'error 'bad-request '(reason lang-and-under)
                         (list 'usage eval-usage))))
      ((not (and timeout memory output))
       (done (list 'error 'bad-request '(reason eval-arguments)
                         (list 'usage eval-usage))))
      ((not (and entry (language-runner entry)))
       (done (list 'error 'bad-request '(reason no-runner) (list 'lang (string->symbol lang))
                         (list 'usage eval-usage))))
      ((not (runners-enabled?))
       (done '(error runners-disabled)))
      ;; THE ADMISSION comes after the last of this branch's own refusals and
      ;; before the source is read, the view taken or the scratch claimed:
      ;; a slot of the run root's pool, held until this process ends.
      ((admit! timeout) => done)
      (else
       (let ((source (read-source nodes)))
         (if (> (string-length source) 1048576)
             (done '(error bad-source (reason input-limit)))
             (let ((before-scheduler (mutation-record)))
               ((later '(theourgia sched) 'start-scheduler)
                 (lambda ()
                   (with-mutation-record
                     (lambda ()
                       (guard (e ((classify-failure e (mutation-record)) => done))
                         (done
                           (combine-report
                             ((later '(theourgia eval-runner) 'run-foreign-eval)
                               key lang source
                               (list (cons 'working? (argument-option nodes "--working"))
                                     (cons 'writer (eval-writer nodes))
                                     (cons 'timeout-ms timeout)
                                     (cons 'memory-bytes memory)
                                     (cons 'output-bytes output)
                                     (cons 'scheme (scheme-binary))
                                     (cons 'launcher (absolute-path (beside-this-program "eval-runner-exec.sc")))))
                             (mutation-record)))))
                     before-scheduler))))))))))

;; A PATH MADE ABSOLUTE against this process's cwd: the launcher is started
;; with its cwd in the projection, where a relative path names nothing.
(define (absolute-path p)
  (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/))
      p
      (string-append (current-directory) "/" p)))

;; THE SLOT THIS PROCESS HOLDS, kept for as long as it runs: the lock is
;; released by the system when the process ends, and nothing here releases
;; it before. -> the refusal, or #f once a slot is held.
(define admission-slot #f)
(define (admit! timeout)
  (let-values (((slot refusal) (eval-admit! timeout)))
    (set! admission-slot slot)
    refusal))

(define (eval-and-exit! argv)
  (let ((nodes (parse-arguments 'eval (cdr argv))))
    (if (and (pair? nodes) (eq? (car nodes) 'error))
        (finish (list 'error 'bad-request '(reason eval-arguments)
                      (list 'usage eval-usage))
                (environment-wire?))
        (let ((timeout (eval-number nodes "--timeout-ms" 3000 1 60000))
              (memory (eval-number nodes "--memory-bytes" 268435456 1048576 2147483648))
              (output (eval-number nodes "--output-bytes" 65536 128 1048576))
              (store (or (argument-option nodes "--store") (getenv "THEOURGIA_STORE") ".")))
         (let-values (((key heard) (eval-scope store)))
         (if (foreign-lang nodes)
             (eval-foreign-and-exit! nodes (foreign-lang nodes) key heard timeout memory output)
         (let ((under (or (argument-option nodes "--under") ""))
               (wire? (eval-wire? nodes)))
          (cond
            ;; NEVER: TWO ANSWERS TO ONE QUESTION. `--cut` names a coordinate
            ;; and `--latest` asks for whichever one is current; a rule
            ;; giving one of them precedence would make the other silently
            ;; do nothing, which is the defect this batch just removed.
            ;;
            ;; THE PRE-ADMISSION REFUSALS COME FIRST, BEFORE ANY LOAD: a
            ;; request refused here does not queue for an evaluation slot,
            ;; and nothing has been read that an answer could carry. They
            ;; used to be checked after the cut's baseline load and carry
            ;; what it heard; the admission reordered them.
            ((and (argument-option nodes "--latest") (argument-option nodes "--cut"))
             (finish (list 'error 'bad-request '(reason cut-and-latest)
                           (list 'usage eval-usage))
                     wire?))
            ((not (and timeout memory output))
             (finish (list 'error 'bad-request '(reason eval-arguments)
                           (list 'usage eval-usage))
                     wire?))
            (else
            ;; THE ADMISSION (client.sc eval-admit!): a slot of the run root's
            ;; pool, held until this process ends, before the cut, the view,
            ;; the projection or the scheduler. A refusal there is the answer.
            (let ((refusal (admit! timeout)))
            (if refusal
                (finish (with-heard-clause refusal (heard)) wire?)
            (let ((cut (eval-cut nodes key)))
          (cond
            ((and (pair? cut) (eq? (car cut) 'refused))
             (finish (with-heard-clause (append (cadr cut) '((during cut))) (heard)) wire?))
            (else
             (let ((source (read-source nodes)))
               (if (> (string-length source) 1048576)
                   (finish (with-heard-clause '(error bad-source (reason input-limit)) (heard)) wire?)
                   ;; THE RECORD CROSSES INTO THE SCHEDULER (F100b point 3):
                   ;; what this process changed before it -- the cut's draft
                   ;; lock, say -- is handed to the boot actor as its
                   ;; scope's initial entries, the boot adds its own (the
                   ;; view's creations), and the worker's answer is
                   ;; combined with the whole of it at `finish` (the
                   ;; aggregation rule, point (b)).
                   (let ((before-scheduler (mutation-record)))
                     ((later '(theourgia sched) 'start-scheduler)
                       (lambda ()
                         (with-mutation-record
                           (lambda ()
                             ;; EVERY EXIT CARRIES WHAT THIS PROCESS HEARD (F77c
                             ;; code review r2): a failure after the baseline
                             ;; consumed a reduction missing a writer says so too.
                             (guard (e ((classify-failure e (mutation-record))
                                        => (lambda (a) (finish (with-heard-clause a (heard)) wire?))))
                               (let ((view (eval-view nodes key cut)))
                                 (cond
                                   ((and (pair? view) (eq? (car view) 'refused))
                                     (finish (with-heard-clause (append (cadr view) '((during view))) (heard)) wire?))
                                   (else
                                     ;; TRIPWIRE, NOT A MEASUREMENT (AG-b, D22): of the
                                     ;; aggregation rule, only the attachment to a named
                                     ;; outcome has a production case here (P3-d,
                                     ;; AG-b-2). The PROMOTION of absent/unreadable/
                                     ;; unwritable to incomplete has none: the boot
                                     ;; creates nothing without --working, and with it
                                     ;; the boot's own load precedes the worker's
                                     ;; (PR-08/09). H2 in test/facade-record.sc carries
                                     ;; that rule.
                                     (finish
                                       ;; The worker's answer, with the notes this
                                       ;; process heard merged into its one clause.
                                       (with-heard-clause
                                       (combine-report
                                         ((later '(theourgia eval-supervise) 'supervise-eval)
                                           (list (cons 'store store) (cons 'cut cut) (cons 'under under)
                                                 (cons 'source source)
                                                 (cons 'timeout-ms timeout)
                                                 (cons 'memory-bytes memory)
                                                 (cons 'output-bytes output)
                                                 (cons 'view view)
                                                 (cons 'scheme (scheme-binary))
                                                 (cons 'worker (beside-this-program "eval-worker.sc"))))
                                         (mutation-record))
                                       (heard))
                                       wire?))))))
                           before-scheduler))))))))))))))))))))

;; NOTE: THE INTERPRETER NAMED BY `THEOURGIA_SCHEME`, or else whatever
;; `scheme` resolves to on PATH. IT IS NOT NECESSARILY THE ONE THIS PROCESS
;; IS RUNNING UNDER: start a tree with a Chez that is not the one on PATH,
;; leave the variable unset, and the children run under a different one.
;;
;; NEVER: THIS CLAIMED THE RUNNING INTERPRETER. It advertised a property the
;; two lines under it do not have, and nothing anywhere compared a parent's
;; interpreter with a child's. `beside-this-program` below is a different
;; question with a different answer, and is not an example of this one: it
;; takes `(car (command-line))`, which under `--script` is THE SCRIPT's path,
;; so it finds a file beside this script and says nothing about which
;; interpreter is reading it.
(define (scheme-binary)
  (or (getenv "THEOURGIA_SCHEME") "scheme"))

(define (beside-this-program name)
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (string-append (substring argv0 0 cut) "/" name) name)))

;; ---- the verbs this program answers itself ------------------------------
;;
;; KEY: ONE TABLE, AND IT IS THE DISPATCH (F64). `main` looks the first
;; argument up here and nowhere else, and `test/own-verbs.sc` reads this
;; define as data for the gates that census every verb the product answers
;; -- so a verb added here is dispatched and counted by the same edit.
;; Before this it was a `string=?` in `main` and a literal `'(eval serve)`
;; in two fixtures, and a third verb dispatched here joined neither.
;;
;; NEVER: EACH ENTRY IS `(cons '<verb> <procedure>)`, the procedure taking the
;; whole argv and not returning. The reader raises on any other shape
;; rather than skip it, so a verb written some other way is a red census,
;; not a missing one.
(define own-verbs
  (list (cons 'eval eval-and-exit!)))

(define (main argv)
  ;; NEVER: ONCE, BEFORE ANYTHING IS PRINTED. Every answer this program gives
  ;; -- local, forwarded, wire or human -- goes through `render-wire`,
  ;; and this is where its settings are decided.
  (answer-printing!)
  (when (null? argv)
    (say '(usage (theourgia <verb> ...)))
    (exit 1))
  ;; THE TRANSLATION POINT (F100b point 3): everything below -- the eval
  ;; path, the socket derivation, the forward, the local dispatch -- runs in
  ;; this process's mutation-record scope, and a filesystem condition that
  ;; leaves it is answered by (theourgia answers)' table with that record,
  ;; on stdout, exit 1; before, it left as an uncaught exception, exit 255.
  (let ((wire-flag (or (and (member "--wire" argv) #t)
                       (and (pair? argv) (string=? (car argv) "eval") (environment-wire?)))))
  (with-mutation-record (lambda ()
  (guard (e ((classify-failure e (mutation-record)) => (lambda (a) (finish a wire-flag))))
  ;; NEVER: `eval` IS THIS PROGRAM. It used to exec a Python supervisor, which
  ;; no longer exists; there is no second implementation to keep in step.
  ;; `serve` is not this program's any more: the daemon is `theourgiad.sc`
  ;; (F46), and `serve` here is a verb like any other the dispatcher does not
  ;; know.
  ;;
  ;; NEVER: AND `eval` NEVER GOES THROUGH A DAEMON. It is intercepted here,
  ;; before the forwarding below, so it always runs in this process --
  ;; and a request naming `eval` that reaches a daemon meets a dispatcher
  ;; that has no such verb, which is the same answer any unknown verb
  ;; gets. Those two facts are the whole of E1-6.
  (let ((own (assq (string->symbol (car argv)) own-verbs)))
    (when own ((cdr own) argv)))
  ;; NEVER: THE SPELLING IS JUDGED FIRST, BEFORE THE ARGUMENTS ARE PARSED.
  ;; The order is CHOSEN so that the two programs share one, and this is
  ;; the check that can be made without a verb's option table.
  ;;
  ;; NOTE: AN EARLIER VERSION OF THIS COMMENT GAVE A REASON THAT IS FALSE.
  ;; It said the thin client cannot reach the option tables and therefore
  ;; can never answer `missing-option-value`. It imports those tables
  ;; deliberately -- see its own import list -- and calls them, and
  ;; `--store`, the option in the example, is common to every verb rather
  ;; than verb-specific. A false reason is worse than none, because the
  ;; next reader treats a written reason as a constraint.
  ;;
  ;; NOTE: WHAT argv MEANS, AND WHICH LAYER OWNS WHICH PART OF IT, IS AN
  ;; OPEN DESIGN QUESTION. Two answers still differ between the programs:
  ;; a transport option before the verb is read as the verb here and
  ;; scanned past there, and the flag search below matches `--wire` where
  ;; it is an option's value. Both are recorded with their readings.
  ;;
  ;; NOTE: MEASURED 2026-09-19, WITH IT AFTER THE PARSE: `show me --store`
  ;; answered `(error bad-request missing-option-value "--store")` here and
  ;; `(error bad-request unknown-verb (spelling "show me"))` from the thin
  ;; client -- two programs, one argv, two answers.
  ;;
  ;; KEY: AND THAT ONE IS TETHERED, unlike the two readings below it.
  ;; `test/cli-forward.sc`'s F-16 runs `'show me' --store` through both
  ;; programs and requires them to answer identically, so moving this check
  ;; back below the parse turns a row red. Its TWIN runs `outline --store`
  ;; and requires `missing-option-value` from both, which is the other half:
  ;; without it, a check at the front that called every malformed command
  ;; line a spelling problem would satisfy the first row.
  ;;
  ;; NOTE: AND `--wire` IS FOUND BY LOOKING, because the parser has not run
  ;; yet. NEVER: THIS DOES NOT AGREE WITH THE THIN CLIENT, and an earlier
  ;; version of this comment claimed it did. Measured 2026-09-19: for
  ;; ("show me" "--store" "--wire") this search answers yes while the thin
  ;; client's scanner answers no, because there "--wire" is the value of
  ;; `--store`; ("show me" "--" "--wire") differs the same way. Which
  ;; spelling of an argument list means what, and which layer decides, is
  ;; the open design question named below. AS OF that date, and not
  ;; re-measured: either scanner can change and this paragraph goes on
  ;; reading correctly, because no row exercises these two argument lists.
  ;; NOTE: MEASURED 2026-09-19: it made no difference to THIS answer -- an
  ;; error rendered the same in both modes, and only an `ok` answer
  ;; differed. The mode is passed anyway because it is the mode the caller
  ;; asked for, and a refusal that ignored it would be right only for as
  ;; long as error rendering happens to match. AS OF that date: nothing
  ;; compares this refusal in both modes, so if error rendering ever stops
  ;; matching, the reason to pass the mode is the one above and not this
  ;; reading.
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
                     (resolved (argument-stdin verb (argument-remove nodes transport-options)
                                 (lambda () (read-all-text (current-input-port))))))
                 (when (and (not (equal? (getenv "THEOURGIA_LOCAL") "1")) (not (eq? (entry-type socket-path*) 'absent)))
                   (forward-then-exit! socket-path* store actor verb resolved
                                       (argument-option nodes "--wire")))
                 (rpc-dispatch-parsed store verb resolved actor #f (environment-writer))))))
    (print-answer answer (and (list? nodes) (not (and (pair? nodes) (eq? (car nodes) 'error))) (argument-option nodes "--wire")))
    (exit (if (rpc-ok? answer) 0 1))))))))

(main (cdr (command-line)))
