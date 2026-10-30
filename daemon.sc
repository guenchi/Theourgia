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

;;; (theourgia daemon) -- `theourgia serve`, in Scheme over igropyr.
;;;
;;; KEY: IT IS THE THIRD CALLER OF `rpc-dispatch`, NOT A SECOND SUPPLIER OF
;;; ANYTHING. The CLI answers a verb by calling that dispatcher; so does
;;; this. No verb, no answer and no error shape exists here that does not
;;; exist there, which is what makes "the daemon's answer is the CLI's
;;; answer, byte for byte" a fact about the code rather than a promise.
;;;
;;; THE PROCESSES: `main` holds the lock and watches the others; `store`
;;; is the ONLY process that opens the store, so the lock is taken in one
;;; place; a `conn` process per connection reads frames and writes
;;; answers; `listener` turns accepted connections into conn processes.
;;;
;;; NEVER: NO `link` ANYWHERE IN HERE. Everything watches with `monitor`: a
;;; link would take this process down with whatever it is linked to, and
;;; a daemon whose store process dies must SAY so and exit on purpose,
;;; not vanish in a cascade. The gate in `test/` refuses a `link` in this
;;; file's transitive closure.
;;;
;;; NOTE: A CONNECTION ENDING IS `#(DOWN pid reason)`, not a message this
;;; code invents: the adapter owns the connection and the runtime reports
;;; its death (§7.6.32 A). Closing one is killing that adapter.

(library (theourgia daemon)
  (export serve write-report-line! store-check-step)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          (rnrs io simple) (rnrs io ports) (rnrs hashtables)
          (only (chezscheme) real-time getenv write newline read void let-values
                char-whitespace? lookahead-char get-char sleep make-time
                register-signal-handler
                irritants-condition? condition-irritants filter raise condition
                make-message-condition make-irritants-condition
                open-string-input-port call-with-string-output-port
                condition? message-condition? condition-message guard exit
                parameterize eval environment char-numeric? string-ci=?
                with-interrupts-disabled make-weak-eq-hashtable)
          ;; NEVER: NAMED ONE BY ONE, AND `link` IS NOT AMONG THEM. A linked
          ;; process that exits abnormally takes its peer down with it
          ;; without asking, which is the one shape this daemon must not
          ;; have: a connection dying must cost that connection. Every
          ;; watch here is a `monitor`, and the way that rule is kept is
          ;; that the name is not in scope to be written -- NEVER: not that
          ;; somebody remembered. `test/daemon-link-gate.sc` reads this
          ;; import as data and refuses a wholesale one.
          (only (theourgia sched)
                start-scheduler spawn receive send self monitor sleep-ms kill)
          (only (theourgia net) listen! stop-listen! listener-open? accept-failure-counts
                conn-read-start! conn-read-stop!
                conn-write! conn-write-observed! conn-open? conn-close! conn-ref-pid)
          (only (theourgia rpc) rpc-dispatch rpc-ok? subscribe-shape-error query-session-provider!)
          (only (theourgia frame) frame-limit parse-frame frame-refusal frame-field request-shape-error
                answer-for answer-envelope)
          (only (theourgia client) socket-path envelope-version resolved-name store-here?
                socket-path-fits? path-too-long silence-window-ms)
          (only (theourgia render) render-wire render-human answer-printing!)
          (only (theourgia working) draft-lock-path)
          (only (theourgia store) store-publish-hook! obtain-state seal-state
                store-withhold-hook!)
          (only (theourgia log) store-state-snapshot snapshot-unreadable-notes
                snapshot-current-segments segment-shrank? segment-version-now log-append-guard!
                segment-version-readable?
                unreadable-entry? unreadable-entry-path unreadable-entry-reason
                unreadable-behind incomplete-clause)
          (only (theourgia reduce) reduce-applied-cut)
          (only (theourgia incomplete) incomplete-accepted)
          (only (theourgia arguments) parse-arguments argument-option transport-option-refusal)
          ;; KEY: THE SAME LEXICAL RULE GUARDS BOTH DIRECTIONS. A request and
          ;; a reply are read by the same reader, and what that reader must
          ;; not be handed is one fact, not two: it lives beside the packer
          ;; in `(theourgia client)` and is asked here rather than copied.
          (only (theourgia client) readable-shape?)
          ;; THE WALK AND ITS SPELLING RULE are the client's, shared with the
          ;; opener that judges its run directory the same way.
          (only (theourgia client) directory-of base-of in-directory digits? folds-case? same-spelling?
                backing-suffix? namespace-spelling namespace-candidate directory-to-make make-judged!
                lock-spelling)
          (only (theourgia trace) trace-event!)
          (only (theourgia ffi) theourgia-fault theourgia-probe-off? hold-point! hold-sleeper-set!
                theourgia-send-buffer process-id wall-clock-ms theourgia-keepalive-ms progress-sink-set!)
          (only (theourgia platform-numbers) platform-number)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia ffi) lock-try-acquire! lock-release! lock-held? file-ensure! file-is-socket?
                current-lock-acquire current-lock-release
                mkdir-p! unlink! file-is-regular? file-is-directory? path-device-inode path-stat
                entry-type unreadable-entry? unreadable-entry-errno
                write-one! with-mutation-record mutation-record theourgia-stage
                entry-name-type read-link symlink! directory-entries rmdir! mkdir-exclusive! read-entry
                real-path path-case-sensitive? fd-open fd-close write-all! fs-error? fs-error-errno
                file-create-exclusive! entry-identity)
          (only (theourgia answers) classify-failure combine-report condition-text))

  ;; ---- where a daemon lives ---------------------------------------------
  ;;
  ;; NEVER: THE RULE IS IN `(theourgia client)` AND THIS FILE IMPORTS IT. It
  ;; used to be written here, and a client that had to find the same
  ;; socket wrote it again -- two rules, and the day they differ a client
  ;; starts a second daemon for a store that already has one. The reason
  ;; the socket does not live beside the store is stated there, with the
  ;; measurement behind the key.
  ;;
  ;; NOTE: IT ANSWERS TWO VALUES AND RAISES FOR A PATH THAT IS NOT THERE.
  ;; Both matter and both bit: used as a single value it raises "returned
  ;; 2 values to single value return context" -- and a raise inside a
  ;; conn process is that process's death, which its adapter reports as
  ;; a closed connection. So the symptom was a client getting EOF and no
  ;; answer, nowhere near the arity mistake that caused it.
  ;;
  ;; ONLY ABSENCE IS #f (D5): a path that is not there answers #f, as
  ;; it always did; one that cannot be read -- a parent this process may
  ;; not search -- raises its unreadable-entry, so the caller's answer
  ;; names it instead of treating it as absent. Anything else raises too.
  (define (device-inode p)
    (guard (e ((and (unreadable-entry? e) (memq (unreadable-entry-errno e) '(ENOENT ENOTDIR))) #f))
      (let-values (((dev ino) (path-device-inode p))) (cons dev ino))))

  (define (lock-path-for socket)
    (let* ((n (string-length socket))
           (slash (let loop ((i (- n 1))) (cond ((< i 0) #f) ((char=? (string-ref socket i) #\/) i) (else (loop (- i 1)))))))
      (if slash
          (string-append (substring socket 0 slash) "/." (substring socket (+ slash 1) n) ".lock")
          (string-append "." socket ".lock"))))

  ;; ---- the frame ---------------------------------------------------------
  ;;
  ;; Its limit and its reader are in (theourgia frame), shared with core.sc's `--frame`.

  ;; NOTE: THE BUDGET IS THE WHOLE FRAME, NOT THE IDLE GAP. A client that
  ;; sends one byte every four seconds is not idle, and a per-gap timer
  ;; would let it hold a connection for ever.
  (define frame-ms 5000)

  ;; ---- the signal -----------------------------------------------------------
  ;;
  ;; NEVER: THE HANDLER ONLY COUNTS. It runs in a signal context, where the
  ;; scheduler's own invariants do not hold: sending a message or
  ;; spawning from here would be doing scheduler work from outside the
  ;; scheduler. Counting is the whole of it, and everything else is done
  ;; by a process that reads the count.
  ;;
  ;; NOTE: AND THE READER POLLS, with `sleep-ms`, NEVER: not a bare `receive`.
  ;; An idle daemon is parked in the event loop with nothing to wake it;
  ;; the poll is what keeps a timer in the loop so the count is noticed.
  ;; Measured: with a poll every 25 ms, a `kill -TERM` sent a second
  ;; after start-up was seen 39 polls later -- the latency is the poll
  ;; interval, not something that swallows the signal.
  ;;
  ;; NOTE: SECOND SIGNAL MEANS STOP NOW. A drain that is taking too long and
  ;; a person who has asked twice are the same request, and both leave
  ;; with 75.
  (define sigterm-count 0)
  (define signal-term (platform-number 'SIGTERM))
  (define drain-poll-ms 25)
  (define drain-budget-ms 5000)

  ;; ---- the idle exit ---------------------------------------------------------
  ;;
  ;; A DAEMON STARTED WITH `--idle-exit <seconds>` LEAVES BY ITSELF after that
  ;; long with no connection registered and nothing running; 0 is never, and
  ;; so is a daemon started without it. THE EXIT IS A DECISION THAT A LATE
  ;; CONNECTION CANCELS. When the clock fires, main records the highest watch
  ;; it has registered (the attempt's watermark) and sends the listener
  ;; `(leave idle)`. The listener unlinks the public link (from then on a
  ;; connect answers ENOENT, nothing sent), keeps taking the connections the
  ;; kernel completed before the unlink for the grace, stops, and acknowledges
  ;; once every adapter it spawned has reported, with the highest watch it
  ;; sent. Main then exits when that is the watermark and nothing is open or
  ;; running, and otherwise tells the listener to bind and publish again.
  ;;
  ;; A CONNECTION THAT ARRIVES DURING A LEAVE IS REGISTERED AT ONCE, so it
  ;; cancels the exit, AND ADMITTED ONLY WHEN THE ATTEMPT IS DECIDED: served
  ;; after the resume, refused `draining` when the resume fails or a drain
  ;; supersedes. One decision in one place, after the attempt; admitted at
  ;; once, its answer would depend on where in the leave it happened to run.
  ;; THE WAIT IS BOUNDED: the rest of the grace, the adapters' reports
  ;; (nothing in an adapter waits), one listen! and one symlink!. The grace is
  ;; capped at 5000 ms where theourgiad reads it, because nothing reaches the
  ;; late request's client meanwhile: grace + rebind + start-up must fit both
  ;; the client's silence window (client.sc, silence-window-ms, 30 s) and
  ;; adapter-idle-ms, the bound before a connection reads.
  (define idle-exit-seconds 0)
  (define idle-grace-default-ms 200)
  (define idle-grace-ms idle-grace-default-ms)
  ;; THE IDLE OPTIONS AS GIVEN, echoed by the `serving` line so an opener can
  ;; read what its daemon was started with; empty for a daemon started
  ;; without them, whose line is the one it always was.
  (define idle-options '())

  ;; THE LISTENER'S CELL: what a resume bound, written by the listener as soon
  ;; as listen! returns and before it runs anything else, completed with the
  ;; link's identity after the publish: (private target private-ident
  ;; link-ident-or-#f), or #f. Main and every exit path read it, so a forced
  ;; exit after a rebind whose `bound` report main has not yet received still
  ;; unlinks the new socket and link, each only while it is the entry made.
  ;; One VM, one listener: a library variable is the cell.
  (define listener-cell #f)

  ;; ---- the store's presence ----------------------------------------------
  ;;
  ;; AN IDLE DAEMON LOOKS AT ITS STORE ONLY HERE. The change probe runs while
  ;; somebody subscribes and a request probes before it is answered, so a
  ;; daemon nobody talks to never looked at the disk again, and one whose
  ;; store was removed served nothing for days. Every 40th poll (about once a
  ;; second) the watcher stats the store's RESOLVED name, the one its socket
  ;; is keyed by (client.sc's resolved-name): a symlink spelling and its
  ;; target share this daemon, so the directory removed is gone and a symlink
  ;; spelling removed is not. The stat follows links. A sample is present,
  ;; absent (ENOENT or ENOTDIR) or failed (any other error); something other
  ;; than a directory at the name is a store replaced, not one gone, and reads
  ;; as present.
  ;;
  ;; NEVER: A FAILED CHECK IS NOT AN ABSENCE. A permission or I/O fault must
  ;; never end a daemon; it resets the count as a present one does. Two
  ;; absences in a row are gone, so an atomic swap of the directory (one
  ;; rename, never an absent name) is not mistaken for it.
  ;;
  ;; NOTE: THE STAT IS ONE SYNCHRONOUS CALL ON THE SCHEDULER'S THREAD, and every
  ;; process waits while it runs (as with flock, below). Requests already make
  ;; such calls; new is one a second on an idle daemon. A SIGTERM is counted by
  ;; the handler meanwhile and read when the stat returns.
  (define store-check-polls 40)
  (define (store-check name)
    (guard (e ((and (unreadable-entry? e) (memq (unreadable-entry-errno e) '(ENOENT ENOTDIR))) 'absent)
              (#t 'failed))
      (path-stat name)
      'present))

  ;; THE DECISION, A PURE STEP so its rule is pinned without a clock: COUNT is
  ;; the absences in a row so far, SENT? whether store-gone was already sent,
  ;; SAMPLE present, absent or failed. -> (values count' send?): count' is
  ;; count+1 on absent and 0 otherwise; send? is #t when count' has reached
  ;; two and nothing was sent before, so it is sent once for the daemon's life.
  (define (store-check-step count sent? sample)
    (let ((count2 (if (eq? sample 'absent) (+ count 1) 0)))
      (values count2 (and (>= count2 2) (not sent?)))))

  (define (watch-for-signals! main-pid store-name)
    (register-signal-handler signal-term
                             (lambda (signo) (set! sigterm-count (+ sigterm-count 1))))
    (spawn
      (lambda ()
        ;; NEVER: STORE-GONE HAS ITS OWN SENT FLAG, independent of `told` and of
        ;; the SIGTERM count, so the `again` rule below is untouched.
        (let poll ((told 0) (wakes 1) (absences 0) (gone-sent #f))
          (sleep-ms drain-poll-ms)
          (let-values (((absences gone-sent)
                        (if (< wakes store-check-polls)
                            (values absences gone-sent)
                            (let ((sample (store-check store-name)))
                              (trace-event! 'store-check sample #f)
                              (let-values (((count send?) (store-check-step absences gone-sent sample)))
                                (when send? (send main-pid (list 'signal 'store-gone)))
                                (values count (or gone-sent send?)))))))
            (let ((wakes (if (< wakes store-check-polls) (+ wakes 1) 1)))
              (cond
                ;; `again` IS SENT ONCE. It used to be sent on every poll
                ;; once the count reached 2 -- a second path to the same exit that
                ;; masked the ready clause's remembered `again` (MD-3, the
                ;; startup-exit design R5). The mailbox is reliable: one send is
                ;; enough, and after it the loop only polls.
                ((and (>= sigterm-count 2) (< told 2))
                 (send main-pid (list 'signal 'again))
                 (poll 2 wakes absences gone-sent))
                ((and (>= sigterm-count 1) (= told 0))
                 (send main-pid (list 'signal 'drain))
                 (poll 1 wakes absences gone-sent))
                (else (poll told wakes absences gone-sent)))))))))

  ;; ---- serve -------------------------------------------------------------

  ;; NEVER: ONE LOCKING STRATEGY FOR THE WHOLE DAEMON, SET ONCE. Inside a
  ;; daemon nothing may park on `flock`: that call runs on the
  ;; scheduler's own thread, so a process waiting there stops every
  ;; other process in the VM -- including the one that would have
  ;; released the lock, and including main.
  ;;
  ;; NOTE: SET, NOT `parameterize`d AROUND THE PROCESSES THAT TAKE LOCKS.
  ;; Measured: Chez parameters are per OS thread and green threads share
  ;; one, so a parameterize in one actor is seen by every actor running
  ;; during its extent and is gone after it -- an actor reaching a lock
  ;; outside that window would get the blocking default, which is the
  ;; freeze this exists to prevent. What keeps the CLI on the default is
  ;; that nothing in the CLI ever sets this.
  ;; The optional second argument is the start's attempt token,
  ;; echoed as the last clause of every startup report; #f when the
  ;; daemon was started by hand without one. The third is the query
  ;; session's size, the fourth the idle window in seconds (0 or #f:
  ;; never), the fifth the idle grace in milliseconds (#f: the default).
  ;; A STORE THAT IS NOT THERE IS REFUSED BY THE PROGRAMS, NOT HERE: the
  ;; client and theourgiad ask store-here? before anything under the run root
  ;; is made, and this procedure, reached only through theourgiad, makes the
  ;; socket's directory and lock before its store process asks again.
  (define (serve store . opts)
    ;; NEVER: ONCE, HERE, BEFORE ANY ANSWER LEAVES. Every conn process in this
    ;; VM prints through `render-wire`, and the settings it needs are per
    ;; OS thread -- so they are set on the way in rather than around each
    ;; write.
    (answer-printing!)
    ;; THE INCARNATION'S TOKEN, made once per start: this process's id and
    ;; its start instant, so two daemons on one store differ in one or the
    ;; other. A subscriber resuming with another token is told the daemon
    ;; restarted; it is compared for equality and nothing else.
    (set! daemon-token (string-append (number->string (process-id)) "-" (number->string (wall-clock-ms))))
    (let ((socket (if (and (pair? opts) (car opts)) (car opts) (socket-path store)))
          (attempt (and (pair? opts) (pair? (cdr opts)) (cadr opts)))
          (size (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts)) (caddr opts)) (caddr opts) session-size-default))
          (idle (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts)) (pair? (cdddr opts)) (cadddr opts)))
          (grace (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts)) (pair? (cdddr opts))
                      (pair? (cddddr opts)) (car (cddddr opts)))))
      (set! session-size size)
      (set! idle-exit-seconds (or idle 0))
      (set! idle-grace-ms (or grace idle-grace-default-ms))
      (set! idle-options
            (append (if idle (list (list 'idle-exit idle)) '())
                    (if grace (list (list 'idle-grace-ms grace)) '())))
      (start-scheduler (lambda () (main store socket attempt)))))

  ;; ---- the query session kept across requests -------------------------------
  ;;
  ;; ONE SESSION FOR THE DAEMON, kept by query.sc's provider (make-session-provider,
  ;; its rules written there) and handed to every query and context request by
  ;; the dispatcher. ONE ACTOR TOUCHES IT: both verbs are published reads of the
  ;; store process (published-read?), which serves one request at a time. Taken
  ;; by eval, so the daemon's own import closure stays what it was, and LAZILY,
  ;; at the first published read: serve's start enters no verb library, as the
  ;; dispatcher enters each on first use, so a daemon that answers only reads of
  ;; the outline never runs the query library. A tree without the procedure
  ;; makes a fresh session per request, as before.
  ;; THE DERIVED GENERATION counts the derived-table writes this daemon has
  ;; executed (supply, clear-derived: they change score's answers without
  ;; moving the log); a session made before one is not reused after it.
  ;; THE TRACE: (trace session <word> <rest>) for each decision, <rest> #f, one
  ;; datum or a list; (trace session served (<verb> <expanded> <new> <size>))
  ;; after each query or context answer.
  (define session-size-default 1000000)
  (define derived-generation 0)
  (define session-provider #f)
  (define session-size session-size-default)
  (define session-installed? #f)
  (define (session-trace! word . rest)
    (trace-event! 'session word (cond ((null? rest) #f) ((null? (cdr rest)) (car rest)) (else rest))))
  (define (install-session-provider!)
    (unless session-installed?
      (set! session-installed? #t)
      (let ((make (guard (e (#t #f)) (eval 'make-session-provider (environment '(theourgia query))))))
        (set! session-provider (and make (make published-state (lambda () derived-generation) session-size session-trace!)))
        (query-session-provider! session-provider))))
  (define (session-served! verb)
    (let ((d (and session-provider (session-provider 'served))))
      (when d (trace-event! 'session 'served (cons verb d)))))

  ;; ---- the startup report ------------------------------------------------
  ;;
  ;; A STARTUP REPORT IS ONE LINE WRITTEN BY ONE write(2):
  ;; a newline, the datum, a newline, handed to ffi's write-one!, which
  ;; never retries a short write. The leading newline ends whatever an
  ;; earlier process left unterminated in the append-mode log, so a reader
  ;; that selects whole lines finds this one whole. A short write is a
  ;; report failure (durable-error, op write). The port that `report` and
  ;; `say` print through is flushed first, so bytes it buffered cannot land
  ;; after this line.
  ;;
  ;; THE STARTUP REPORTS ARE THESE, and every one is written
  ;; here: main's serve-busy, serve-path-occupied and table answers (point
  ;; 9); the store process's failure, combined by main (point 2); and
  ;; theourgiad's translated refusals (point 8). `(serving ...)`,
  ;; `(exiting ...)` and the argument refusals are not startup reports and
  ;; keep `report`/`say`.
  (define (write-report-line! fd datum)
    (flush-output-port (current-output-port))
    ;; INJECTION ONLY (item 7): a start held before its report's one write
    ;; (PID3 stops the daemon here, so no report carries the token). Inside
    ;; the daemon the wait yields (main sets the sleeper). theourgiad's point
    ;; 8 calls this BEFORE THE SCHEDULER EXISTS and keeps the blocking
    ;; default, which is right there: one thread, nothing else to run.
    (hold-point! 'report-write)
    (write-one! fd (string->utf8
                     (string-append "\n"
                                    (call-with-string-output-port (lambda (p) (write datum p)))
                                    "\n"))))

  ;; The attempt token is the report's LAST clause (item 3), #f for a start
  ;; made by hand.
  (define (startup-report! attempt answer)
    (write-report-line! 1 (append answer (list (list 'attempt attempt)))))

  ;; A STARTUP THAT FAILED REPORTS ONCE AND LEAVES WITH 75 (point 9), or with
  ;; CODE when one is given: a refusal no retry clears (a public name spelled
  ;; as a derived name) leaves with 1, as a usage error does. A failure of
  ;; the report's own write (a short write, say) is not answered again: the
  ;; report channel is what failed, so there is nowhere left to say it, and a
  ;; second report under the same attempt is what the client must never
  ;; select (found in review). The exit is the same either way.
  (define (report-and-exit! attempt answer . code)
    (let ((code (if (pair? code) (car code) 75)))
      (guard (e (#t (exit code)))
        (startup-report! attempt answer))
      (exit code)))

  ;; ---- the lock service ---------------------------------------------
  ;;
  ;; NEVER: THE DESCRIPTOR IS OPENED AND CLOSED BY MAIN, NEVER BY THE PROCESS
  ;; THAT USES THE LOCK. A killed actor does not run its dynamic-wind
  ;; unwinds, and an OS descriptor does not go away when an actor does --
  ;; so a process that opened its own would leave, on the two paths
  ;; below, a lock nobody can release:
  ;;
  ;;   * killed after `flock` and before it could register the
  ;;     descriptor -- nobody knows the number, so nobody can close it,
  ;;     and the store stays locked for the life of the daemon;
  ;;   * killed after closing and before deregistering -- the number is
  ;;     free again, the kernel hands it to the next `open` in this same
  ;;     VM, and the tidy-up then closes SOMEBODY ELSE'S descriptor.
  ;;
  ;; Both windows are between two actions in two different processes.
  ;; Main doing the open and the close removes them: its handler for a
  ;; message runs to the end without yielding, so the descriptor and the
  ;; table entry are never out of step.
  ;;
  ;; NEVER: AND MAIN NEVER WAITS. `lock-try-acquire!` is LOCK_NB; the waiting
  ;; -- the backoff, the budget, the refusal -- all happens in the
  ;; process that asked. A main parked on `flock` would stop every
  ;; process in the VM, including the one holding the lock it waits for.
  (define lock-budget-ms 5000)
  (define lock-backoff-cap-ms 200)

  ;; NOTE: UNIQUE BY VALUE, because a reply is matched against the token
  ;; with `equal?`: two requests carrying equal tokens could take each
  ;; other's answers. One counter for the whole VM is enough -- green
  ;; threads do not preempt between the read and the write.
  (define lock-seq 0)
  (define (fresh-lock-token)
    (set! lock-seq (+ lock-seq 1))
    (list 'lk lock-seq))

  ;; NEVER: ONE NUMBER PER REQUEST FOR THE WHOLE DAEMON, taken where the
  ;; request is accepted. A connection's own sequence numbers say which
  ;; frame on that connection; this says which request in this daemon,
  ;; which is what main needs to answer "is anything still running".
  (define ticket-seq 0)
  (define (fresh-ticket)
    (set! ticket-seq (+ ticket-seq 1))
    ticket-seq)

  ;; ---- progress, per request ---------------------------------------------
  ;;
  ;; NEVER: A REQUEST'S PROGRESS IS ITS OWN. The work below the executor
  ;; carries no ticket, so the executor records which ticket it is running
  ;; (process -> ticket) and the ffi's progress! sink advances that ticket's
  ;; count; the connection reads only its own ticket's count. A request
  ;; queued behind another borrows nothing and goes silent after the window,
  ;; as the work it waits behind would have it.
  ;; THE CONNECTION OWNS THE COUNT: it registers its ticket at zero before the
  ;; request is published and removes it on every way out of its wait. A bump
  ;; advances only a registered ticket, so an executor that bumps after its
  ;; connection left changes nothing, and nothing is kept for a connection
  ;; that has gone.
  ;; NEVER: A TABLE OPERATION OUTSIDE AN INTERRUPT REGION: every actor reads
  ;; and writes these two tables, and a lookup a preemption splits while
  ;; another actor grows the table can miss a key that is there (the rule of
  ;; the ffi's yield tables).
  ;; THE COUNT IS THE CONNECTION'S, and so is its removal: progress-owners
  ;; names, for each connection waiting on a version-2 request, the ticket
  ;; it registered. The wind's after-thunk drops both on every way out it
  ;; sees; a killed connection runs no unwinds, so main reclaims what a dead
  ;; one left (progress-reclaim!, from its DOWN clause), and the executor
  ;; goes on undisturbed.
  (define progress-counts (make-eqv-hashtable))
  (define progress-tickets (make-weak-eq-hashtable))
  (define progress-owners (make-eq-hashtable))
  (define (progress-register! ticket)
    (let ((me self))
      (with-interrupts-disabled
        (hashtable-set! progress-counts ticket 0)
        (hashtable-set! progress-owners me ticket))))
  (define (progress-drop! ticket)
    (let ((me self))
      (with-interrupts-disabled
        (hashtable-delete! progress-counts ticket)
        (when (eqv? (hashtable-ref progress-owners me #f) ticket)
          (hashtable-delete! progress-owners me)))))
  ;; WHO, a process that died: the count its connection registered, if any
  ;; is left, is removed and traced; anything else is untouched.
  (define (progress-reclaim! who)
    (let ((ticket (with-interrupts-disabled
                    (let ((t (hashtable-ref progress-owners who #f)))
                      (when t
                        (hashtable-delete! progress-owners who)
                        (hashtable-delete! progress-counts t))
                      t))))
      (when ticket (trace-event! 'progress-reclaimed ticket #f))))
  (define (progress-count ticket)
    (with-interrupts-disabled (hashtable-ref progress-counts ticket #f)))
  (define (progress-bump!)
    (let ((me self))
      (with-interrupts-disabled
        (let ((ticket (hashtable-ref progress-tickets me #f)))
          (when (and ticket (hashtable-contains? progress-counts ticket))
            (hashtable-update! progress-counts ticket (lambda (n) (+ n 1)) 0))))))
  ;; THUNK runs as this executor's work for TICKET: the association is made
  ;; before anything is asked and undone on every way out -- the refusal, the
  ;; answer, a raise.
  ;; NOTE: THE UNDOING IS READ BACK, IN THE SAME REGION AS THE DELETE, and
  ;; traced after it (association-cleared, #t when this executor holds no
  ;; ticket any more). Nothing else can see the removal: a stale entry
  ;; bumps a count already dropped, and the next with-ticket overwrites it.
  ;; ME is held by this closure, so the weak table cannot lose the key
  ;; between the delete and the read.
  (define (with-ticket ticket thunk)
    (let ((me self))
      (dynamic-wind
        (lambda ()
          (with-interrupts-disabled (hashtable-set! progress-tickets me ticket))
          (trace-event! 'executing ticket #f))
        thunk
        (lambda ()
          (let ((cleared (with-interrupts-disabled
                           (hashtable-delete! progress-tickets me)
                           (not (hashtable-contains? progress-tickets me)))))
            (trace-event! 'executing-done ticket #f)
            (trace-event! 'association-cleared ticket cleared))))))
  ;; MAY THIS EXECUTOR BEGIN? may-execute?, and when it may, the two seams a
  ;; row holds an executing request at (INJECTION ONLY): held with the actor
  ;; alive and making no progress, and the whole process blocked.
  (define (executing? main-pid ticket)
    (let ((yes (may-execute? main-pid ticket)))
      (when yes
        (hold-point! 'executor-before-verb)
        (blocking-hold! 'executor-block-before-verb))
      yes))

  ;; THE KEEPALIVE INTERVAL, K: a version-2 connection whose request has
  ;; progressed since its last keepalive writes one more, this often. Shorter
  ;; than the client's window, or a client would give up between two of them;
  ;; a build with injection on may shorten both (THEOURGIA_KEEPALIVE_MS), and
  ;; a K that is not below the window is refused when this library loads.
  (define keepalive-ms
    (let ((k (or (theourgia-keepalive-ms) 5000)))
      (unless (< k silence-window-ms)
        (assertion-violation 'theourgia-daemon
          "THEOURGIA_KEEPALIVE_MS must be below the silence window" k silence-window-ms))
      k))

  ;; NEVER: ASKED BY WHOEVER IS ABOUT TO EXECUTE, IMMEDIATELY BEFORE DOING SO,
  ;; and answered by main. NEVER: Not decided by the connection: a request
  ;; gets exactly one answer, and two deciders is how it comes to get two
  ;; or none. What has begun runs to its end -- it may already have
  ;; changed the store, and a caller told `draining` about work that was
  ;; done would be told something false.
  (define (may-execute? main-pid ticket)
    (eq? 'execute (ask-main main-pid (list 'may-execute ticket) 'may-execute-is)))

  (define (executed! main-pid ticket)
    (send main-pid (list 'finished ticket)))

  ;; The answer an executor gives for work it is not going to start.
  (define draining-answer '(error draining))

  ;; NOTE: NO TIMEOUT ON THE REPLY, ON PURPOSE. Main answers every request
  ;; in the handler that receives it, so the only way no answer comes is
  ;; that main is gone -- and a daemon without main is on its way out of
  ;; the VM anyway. A timeout here could only invent a `busy` that
  ;; nothing measured.
  (define (ask-main main-pid msg reply)
    (let ((tok (fresh-lock-token)))
      (send main-pid (append msg (list self tok)))
      (receive
        (`(,@reply ,@tok ,value) value))))

  (define (make-lock-strategy main-pid)
    (lambda (path mode)
      (let ((deadline (+ (real-time) lock-budget-ms)))
        (let attempt ((wait 5))
          (let ((l (ask-main main-pid (list 'lock-take path mode) 'lock-answer)))
            (cond
              ((not (eq? l 'busy)) l)
              ((>= (real-time) deadline)
               ;; NEVER: RAISED AS AN ANSWER, not as a crash: the conn turns
               ;; it into the client's reply. Something else holding this
               ;; lock is an ordinary state of the world, not a defect
               ;; here.
               (raise (list 'error 'store-busy (list 'path path))))
              (else
               (sleep-ms wait)
               (attempt (min lock-backoff-cap-ms (* wait 2))))))))))

  (define (make-lock-release main-pid)
    (lambda (l)
      (ask-main main-pid (list 'lock-drop l) 'lock-dropped)
      (void)))

  ;; The entries main holds are (owner . lock-handle); a process can hold
  ;; more than one, so removal is by handle and tidy-up is by owner.
  (define (drop-lock l locks)
    (cond ((null? locks) '())
          ((eq? l (cdar locks)) (cdr locks))
          (else (cons (car locks) (drop-lock l (cdr locks))))))

  (define (release-all-of who locks)
    (let loop ((rest locks) (kept '()))
      (cond
        ((null? rest) (reverse kept))
        ((eq? who (caar rest))
         (lock-release! (cdar rest))
         (loop (cdr rest) kept))
        (else (loop (cdr rest) (cons (car rest) kept))))))

  (define (main store socket attempt)
    ;; THE HOLD SEAM WAITS BY YIELDING HERE, SET ONCE: main is
    ;; the scheduler's first actor and no other exists yet. A hold that
    ;; blocked the OS thread would stop every actor, and a second held
    ;; actor could never be reached (A-record). Unarmed, holds never wait.
    (hold-sleeper-set! sleep-ms)
    ;; MAIN'S STARTUP ANSWERS BY THE TABLE: a filesystem
    ;; failure before the store process exists -- the socket directory, the
    ;; lock file, the lock, the probe of what is on the socket path -- is
    ;; written as main's startup report with main's record, and main leaves
    ;; with 75, never the scheduler's panic 70. The record is kept after a
    ;; successful startup: a store process that fails later has its answer
    ;; combined with it (point 2, aggregation (a)).
    ;;
    ;; UNDER THE `report` STAGE, SO A @report FAULT CAN FIRE HERE (item 1).
    ;; NO ACTOR EXISTS WHILE THIS RUNS: the store process is spawned after
    ;; it. That is what makes a parameterize safe here -- under igropyr a
    ;; parameterize is one global cell that a preemption hands to every
    ;; other actor (actor.sc:459-469), and there is no other actor yet.
    (let ((started
           (parameterize ((theourgia-stage 'report))
             (with-mutation-record
               (lambda ()
                 ;; THE ANSWER IS DECIDED INSIDE THE SCOPE AND REPORTED OUTSIDE IT,
                 ;; ONCE (found in review). Reporting from inside the guarded
                 ;; body let a failure after the report -- of the report's own
                 ;; write, or of a lock release -- be classified and reported a
                 ;; second time under the same attempt, carrying the first
                 ;; report's write in its record.
                 (guard (e ((classify-failure e (mutation-record))
                            => (lambda (answer) (list 'refuse answer))))
                   (cond
                     ;; NEVER: A PUBLIC NAME IS NO ENDPOINT'S LOCK NAME, judged first, on
                     ;; the spelling alone, before anything is made or locked: a public
                     ;; `.s.lock` or `s.d.reserve` is the file the endpoint `s` locks, and
                     ;; while it is published `s`'s start opens it through the link, onto
                     ;; a socket (found in review). Refused whether or not that endpoint
                     ;; exists. The other derived shapes are not: the link is published by
                     ;; a create that fails on any entry, so occupancy refuses whichever
                     ;; start comes second; a backing directory is allocated under the next
                     ;; free name, so a public name spelled as one is stepped around. The
                     ;; case rule is the socket directory's, and an unknown rule (a directory
                     ;; not there yet, a system with no answer) is judged folding, so the
                     ;; doubt refuses. Exit 1: no retry clears it.
                     ((lock-spelling (base-of socket)
                                        (not (eq? #t (path-case-sensitive? (directory-of socket)))))
                      => (lambda (d)
                           (list 'refuse (combine-report `(error serve-name-derived (name ,(base-of socket)) (of ,(cadr d))
                                                                 (shape ,(car d)))
                                                         (mutation-record))
                                 1)))
                     ;; THE PRIVATE PATH MUST FIT, BEFORE ANY EFFECT: the daemon binds
                     ;; `<socket>.d/<pid>`, longer than the public path, so the length is
                     ;; judged on that name here, before the directory, the lock file or
                     ;; anything else exists. A starter (serve's argument check, the
                     ;; shell) refuses earlier on the longest suffix, so this is reached
                     ;; only by a caller that skipped it.
                     ((not (socket-path-fits? (private-socket-path socket (process-id))))
                      (list 'refuse (combine-report (path-too-long (private-socket-path socket (process-id)))
                                                    (mutation-record))))
                     (else
                      ;; INJECTION ONLY: held before the reservation check, after the
                      ;; starter's own check of the socket's directory.
                      (hold-point! 'serve-before-reserve-check)
                      (let ((judged (reserved-by-another socket)))
                        (cond
                          ;; A RESERVED NAMESPACE IS SERVED ONLY BY ITS OWN ENDPOINT, judged
                          ;; before this start takes a lock of its own or clears anything: a
                          ;; path inside another endpoint's marked directory would otherwise
                          ;; take its own lock there and clear a live socket.
                          ;; NEVER: BEFORE THE DIRECTORY IS MADE. A start inside a namespace
                          ;; whose link its endpoint's reclaim has just unlinked would make a
                          ;; real `<name>.d` there, and the endpoint's publish would collide
                          ;; (found in review). So a missing directory is made here only when
                          ;; none of the directories it would make is `..` or spelled
                          ;; as a namespace, and the working directory a relative path
                          ;; starts from is there (reserved-by-another, directory-to-make).
                          ((eq? (car judged) 'error)
                           (list 'refuse (combine-report judged (mutation-record))))
                          ;; NEVER: NO IDENTITY WITHOUT A RESOLVED PARENT. Every
                          ;; admission names the parent its one look resolved; one
                          ;; that did not would be refused here, naming the
                          ;; spelling, rather than marked by it.
                          ((not (caddr judged))
                           (list 'refuse (combine-report `(error socket-dir-missing (dir ,(directory-of socket)))
                                                         (mutation-record))))
                          ;; NEVER: MADE UNDER WHAT WAS JUDGED, NEVER BY THE SPELLING. What
                          ;; is made is what the check above answered: the ancestor it
                          ;; resolved and the names it judged, each made exclusively, or
                          ;; nothing when the parent resolved at its one look. A name a peer
                          ;; made meanwhile refuses serve-busy (make-judged!). Everything
                          ;; after this is one level on the socket's spelling (the lock
                          ;; file, the backing directory, the bind, the publish), so a prefix
                          ;; that vanished in between -- an endpoint's link spelled into the
                          ;; path, unlinked by its reclaim -- fails the start with ENOENT and
                          ;; makes nothing (found in review).
                          ((let ((dir (cadr judged)))
                             ;; INJECTION ONLY: held after the reservation check, before
                             ;; the socket's directory is made.
                             (hold-point! 'serve-before-mkdir)
                             (and dir (make-judged! (car dir) (cdr dir))))
                           => (lambda (stale) (list 'refuse (combine-report stale (mutation-record)))))
                          (else
                           ;; NOTE: THE LOCK FILE HAS TO EXIST BEFORE IT CAN BE LOCKED: `flock`
                           ;; wants an open descriptor, so the file is the lock's name, not its
                           ;; content, and it is never removed -- removing it would let the next
                           ;; caller create a NEW file and lock that one instead, which is two
                           ;; daemons each holding "the" lock.
                           (file-ensure! (lock-path-for socket))
                           ;; NEVER: THE ATTEMPT THAT NEVER WAITS. A blocking flock here would park
                           ;; this process on the scheduler's own thread -- and a second daemon
                           ;; is supposed to answer `serve-busy` and leave, not queue for ever
                           ;; behind the first one. Measured: with the blocking form, the second
                           ;; daemon simply hung and its row never returned.
                           ;; #f IS CONTENTION ONLY (D6): lock-try-acquire! answers #f
                           ;; itself when another holds the lock; a lock file that cannot be
                           ;; opened raises unreadable-entry, which goes to the table above
                           ;; instead of reading as "somebody is serving".
                           (let ((lock (lock-try-acquire! (lock-path-for socket) 'exclusive)))
                             ;; THE TWO NAMED OUTCOMES CARRY MAIN'S RECORD (found
                             ;; in review): the lock file this start created is named in them.
                             ;; NEVER: A LOCK THAT IS HELD IS NOT RELEASED BEFORE THE EXIT. The
                             ;; exit closes its descriptor, which releases the flock; a release
                             ;; here could raise after the answer was decided.
                             ;; UNDER THE LOCK, claim-endpoint! judges the public entry and the
                             ;; namespace, reclaims what an earlier daemon of this endpoint left
                             ;; and reserves this daemon's own directory: #f, or its refusal.
                             (cond
                               ((or (not lock) (not (lock-held? lock)))
                                (list 'refuse (combine-report `(error serve-busy (path ,socket)) (mutation-record))))
                               ((claim-endpoint! socket (caddr judged))
                                => (lambda (refusal) (list 'refuse (combine-report refusal (mutation-record)))))
                               (else
                                (list 'ok lock (mutation-record))))))))))))))))
      (when (eq? (car started) 'refuse)
        (apply report-and-exit! attempt (cdr started)))
      (let ((lock (cadr started))
            (main-record (caddr started)))
         (let ((me self))
           ;; NEVER: ONE LOCKING STRATEGY FOR THE WHOLE VM, SET ONCE, HERE --
           ;; here because this is where main's own pid first exists, and
           ;; once because it has to hold for every process that will ever
           ;; take a lock.
           ;;
           ;; NOTE: SET, NOT `parameterize`d. Measured: Chez parameters are
           ;; per OS thread and every green thread shares one, so a
           ;; parameterize in one actor is seen by every actor running
           ;; during its extent and is gone afterwards -- an actor
           ;; reaching a lock outside that window would get the blocking
           ;; default, which is the freeze this exists to prevent.
           ;;
           ;; KEY: AND WHAT KEEPS THE CLI ON THE BLOCKING DEFAULT IS THAT
           ;; NOTHING IN THE CLI EVER SETS THIS. A CLI run has one thing
           ;; to do and no other processes to starve, so waiting is right
           ;; there, and giving up on a budget would be a regression.
           ;;
           ;; NOTE: BOTH HALVES OR NEITHER: whoever opens the descriptor has
           ;; to be the one that closes it.
           (current-lock-acquire (make-lock-strategy me))
           (current-lock-release (make-lock-release me))
           ;; NEVER: SET HERE TOO, AND FOR THE SAME REASON: one publication
           ;; for the whole VM, and only the process that writes ever
           ;; reaches it.
           ;; The write path's snapshot is sampled here, inside the store's
           ;; lock (see publish!).
           (store-publish-hook! (lambda (state) (publish! store state (store-state-snapshot store))))
           ;; THE WORK'S PROGRESS GOES TO THE TICKET ITS EXECUTOR IS RUNNING.
           (progress-sink-set! progress-bump!)
           ;; NOTE: INSTALLED BEFORE ANYTHING CAN BE ACCEPTED, so a signal
           ;; that arrives during the store's first load is not lost --
           ;; the count is read by a process, and the process is here.
           ;; The name the socket is keyed by, resolved once.
           (watch-for-signals! me (resolved-name store))
           (let* ((store-pid (spawn (lambda () (store-loop store me))))
                  ;; A WITHHELD PUBLICATION ASKS FOR A FRESH FOLD: a
                  ;; write whose session was not delivered publishes nothing,
                  ;; and the store process is asked to reload, which is
                  ;; judged like any other load.
                  (_ (store-withhold-hook! (lambda () (send store-pid (list 'reload)))))
                  (smon (monitor store-pid))
                  (st (make-main-state lock socket store store-pid #f main-record attempt)))
             (state-roles-set! st (list (cons store-pid 'store)))
             ;; NEVER: NOTHING IS ACCEPTED UNTIL THE STORE HAS PUBLISHED. The
             ;; listener is spawned by the `ready` clause below, not
             ;; here -- and main goes into its loop first because the
             ;; boot load takes the store's lock THROUGH main, so a main
             ;; that waited here for `ready` would be waiting for a
             ;; message that only it could make possible.
             (watch-loop st))))))

  ;; NEVER: MAIN'S STATE IS ONE VALUE THAT MAIN MUTATES, NOT NINE ARGUMENTS
  ;; IT THREADS. Four of the nine are association lists of the same
  ;; shape, and a loop that passes them positionally is a place where an
  ;; edit puts the writers where the locks go and nothing says so.
  ;; Measured, on the smaller version of this same loop: a five-argument
  ;; call against a seven-argument definition, which Chez reported as a
  ;; warning nobody was reading and which showed up as every row failing
  ;; with a transport error.
  ;;
  ;; NOTE: MUTATION IS SAFE HERE FOR ONE REASON ONLY: main is the only
  ;; process that ever touches this value, and each clause below runs to
  ;; its end before the next `receive`.
  (define roles-slot 5)
  (define handling-slot 6)
  (define bound-slot 7)
  (define locks-slot 8)
  (define writers-slot 9)

  ;; 11 draining-since (#f until a signal), 12 the tickets being executed,
  ;; 13 the conn processes that have been told to drain, 14 main's startup
  ;; record, 15 the start's attempt token, 16 the
  ;; phase, 17 the signal remembered while STARTING (the startup-exit design,
  ;; archive/theourgia-f100b-m2a-startup-exit-design.md v3), 18 the drain's
  ;; reason (drained or store-gone), set once when the drain is entered.
  ;;
  ;; THE PHASE DECIDES WHAT AN EVENT ENDS IN. STARTING runs from the store's
  ;; spawn to its outcome (`ready` or `startup-failed`); SERVING follows
  ;; `ready`; DRAINING is SERVING with draining-since set. A startup failure
  ;; ENDS main in STARTING -- it never returns to the loop, so no ordinary
  ;; exit (drained 0 among them) can follow a reported failure -- and a
  ;; signal during STARTING is remembered, with its kind, until the outcome.
  ;; 19 idle-since (the idle clock's start), 20 the leave (#f, (leaving <w>)
  ;; or resuming), 21 the highest watch sequence registered, 22 whether the
  ;; listener has acknowledged a stop, 23 the connections whose watch arrived
  ;; during a leave and are not yet admitted (newest first).
  (define (make-main-state lock socket store store-pid listener main-record attempt)
    (vector lock socket store store-pid listener '() '() #f '() '() self #f '() '() main-record attempt
            'starting #f #f (real-time) #f 0 #f '()))

  (define (state-lock st) (vector-ref st 0))
  (define (state-socket st) (vector-ref st 1))
  (define (state-store st) (vector-ref st 2))
  (define (state-store-pid st) (vector-ref st 3))
  (define (state-listener st) (vector-ref st 4))
  (define (state-listener-set! st v) (vector-set! st 4 v))
  (define (state-main st) (vector-ref st 10))
  (define (state-draining st) (vector-ref st 11))
  (define (state-draining-set! st v) (vector-set! st 11 v))
  (define (state-running st) (vector-ref st 12))
  (define (state-running-set! st v) (vector-set! st 12 v))
  (define (state-main-record st) (vector-ref st 14))
  (define (state-attempt st) (vector-ref st 15))
  (define (state-phase st) (vector-ref st 16))
  (define (state-phase-set! st v) (vector-set! st 16 v))
  (define (state-remembered st) (vector-ref st 17))
  (define (state-remembered-set! st v) (vector-set! st 17 v))
  (define (state-drain-reason st) (vector-ref st 18))
  (define (state-drain-reason-set! st v) (vector-set! st 18 v))
  (define (state-idle-since st) (vector-ref st 19))
  (define (state-idle-since-set! st v) (vector-set! st 19 v))
  (define (state-leave st) (vector-ref st 20))
  (define (state-leave-set! st v) (vector-set! st 20 v))
  (define (state-watch-high st) (vector-ref st 21))
  (define (state-watch-high-set! st v) (vector-set! st 21 v))
  (define (state-listener-acked st) (vector-ref st 22))
  (define (state-listener-acked-set! st v) (vector-set! st 22 v))
  (define (state-deferred st) (vector-ref st 23))
  (define (state-deferred-set! st v) (vector-set! st 23 v))
  (define (state-roles st) (vector-ref st roles-slot))
  (define (state-roles-set! st v) (vector-set! st roles-slot v))
  (define (state-handling st) (vector-ref st handling-slot))
  (define (state-handling-set! st v) (vector-set! st handling-slot v))
  (define (state-bound st) (vector-ref st bound-slot))
  (define (state-bound-set! st v) (vector-set! st bound-slot v))
  (define (state-locks st) (vector-ref st locks-slot))
  (define (state-locks-set! st v) (vector-set! st locks-slot v))
  (define (state-writers st) (vector-ref st writers-slot))
  (define (state-writers-set! st v) (vector-set! st writers-slot v))

  ;; NEVER: MAIN ONLY WATCHES, AND IT SAYS WHY. A process that dies takes its
  ;; reason with it into a `#(DOWN pid reason)` that nobody else is
  ;; looking at -- and what a client sees is a connection closing with no
  ;; answer, which is the same thing it sees when a peer simply goes
  ;; away. Writing the reason to the trace is what separates those two,
  ;; and it costs one line.
  ;;
  ;; NOTE: A NORMAL ENDING IS NOT AN EVENT. Tracing those would bury the one
  ;; line that matters under one line per connection that ever closed.
  ;; NOTE: THE LOOP HAS A CLOCK, and it is here rather than in a process of
  ;; its own because the thing it has to notice -- a drain that is not
  ;; finishing -- is main's own state. A request parked on something that
  ;; never completes sends no message; without this arm main would wait
  ;; for it for ever, and "exits within five seconds" would be a promise
  ;; kept only by requests that were going to finish anyway.
  (define (watch-loop st)
    (receive
      (after drain-poll-ms
             (when (state-draining st)
               (when (> (- (real-time) (state-draining st)) drain-budget-ms)
                 (report-quietly `(exiting (reason drain-timeout)
                                           (in-flight ,(length (state-running st)))))
                 (report-resume-failed! st)
                 (leave st 75)))
             ;; THE IDLE CLOCK is read on the same tick.
             (when (idle-due? st) (begin-leave! st))
             (watch-loop st))
      ;; The listener says what it bound, so the exit path can tell this
      ;; socket from one somebody else has since put in its place.
      ;; NEVER: `serving` IS REPORTED HERE, WHERE THE SOCKET IS ACTUALLY
      ;; BOUND -- not where the listener was spawned. It used to be
      ;; announced as soon as the listener process existed, which is
      ;; before `listen!` has been called, so a daemon that could not
      ;; bind printed
      ;;
      ;;   (serving (store "/tmp/x") (socket ""))
      ;;   (exiting (reason listener-down))
      ;;
      ;; in that order. The first line is what a caller waits for and
      ;; believes; the second is what happened. Measured with
      ;; `--socket ""`.
      ;; THE STORE PROCESS'S STARTUP FAILURE ENDS MAIN (aggregation
      ;; (a); the startup-exit design). Its answer already carries
      ;; the store's own record (the table ran over it there; the record
      ;; travels with it for the trace); main adds what it had created itself
      ;; before the store existed -- the lock file, say -- and writes the one
      ;; report, then the store-actor-down line itself, then leaves with 75.
      ;; NEVER: IT DOES NOT RETURN TO THE LOOP. It did, and a drain signal
      ;; processed before the store's DOWN ended the start in `drained`, exit
      ;; 0, after its failure had been reported (found in review). The
      ;; store's DOWN that follows is never processed, nor its cleanup; the
      ;; exit closes the descriptors main holds. A report that cannot be
      ;; written is not reported again (found in review). Only STARTING receives
      ;; this: the store sends it before `ready` or not at all. READ, NOT
      ;; MEASURED in M2a: that no loop follows it is seen in the code; the
      ;; rows that force a signal before the outcome come with M2b's
      ;; `store-start` hold stage.
      (`(startup-failed ,answer ,record)
       (trace-event! 'startup-failed record #f)
       (trace-event! 'daemon-down (list 'store 'startup-failed) #f)
       (guard (e (#t (trace-event! 'startup-report-failed #f #f)))
         (startup-report! (state-attempt st) (combine-report answer (state-main-record st))))
       (report-quietly '(exiting (reason store-actor-down)))
       (leave st 75))
      (`(bound ,ident)
       (state-bound-set! st ident)
       (cond
         ;; A RESUME'S REPORT ends the attempt: the deferred connections are
         ;; admitted (by the drain, when one has superseded it) and the clock
         ;; starts again.
         ((eq? (state-leave st) 'resuming)
          (state-leave-set! st #f)
          (state-listener-acked-set! st #f)
          (report-quietly `(idle-resumed (socket ,(state-socket st))))
          (admit-deferred! st (and (state-draining st) #t))
          (state-idle-since-set! st (real-time)))
         (else
          (report-quietly `(serving (store ,(state-store st)) (socket ,(state-socket st)) ,@idle-options))))
       (watch-loop st))
      ;; THE LISTENER'S ACKNOWLEDGEMENT of a stop, sent once every adapter it
      ;; spawned has reported. In a leave it decides the attempt: no watch after
      ;; the watermark and nothing open or running is the exit; anything else
      ;; cancels it, and the listener binds again. In a drain it is what
      ;; finish-if-drained waits for.
      (`(stopped (handed ,h) (refused-accepts ,ra) (callback-raised ,cr) (exhausted ,ex))
       (state-listener-acked-set! st #t)
       (let ((attempt (state-leave st)))
         (cond
           ((state-draining st)
            (when (pair? attempt) (state-leave-set! st #f))
            (finish-if-drained st))
           ((and (pair? attempt) (eq? (car attempt) 'leaving))
            ;; A LISTENER THE RUNTIME RETIRED DURING THE LEAVE IS NO IDLE EXIT,
            ;; whatever the watermark says (R2 (iv)): it closed before the
            ;; grace ended, resetting what the kernel still queued. It takes
            ;; the resume branch, the exhausted listener answers the resume
            ;; resume-failed listener-exhausted, and the daemon drains and
            ;; leaves 75 with that reason (found in review: it left 0).
            (if (and (= h (cadr attempt)) (nothing-open? st) (not ex))
                (begin
                  (report-quietly `(idle-exit (after ,idle-exit-seconds) (refused-accepts ,ra)
                                              (callback-raised ,cr)))
                  (leave st 0))
                ;; RESUMING lasts until main has recorded the new identity: a
                ;; drain meanwhile tells the listener to stop, which it takes
                ;; after its resume, so the exit waits for both.
                (begin
                  (state-leave-set! st 'resuming)
                  (state-listener-acked-set! st #f)
                  (send (state-listener st) (list 'resume)))))
           (else #f)))
       (watch-loop st))
      ;; THE UNLINK FAILED: our link is still there and the listener still
      ;; listens. The attempt is over and the clock starts again.
      (`(idle-cancelled ,reason)
       (report-quietly `(idle-cancelled ,reason))
       (state-leave-set! st #f)
       (admit-deferred! st (and (state-draining st) #t))
       (state-idle-since-set! st (real-time))
       (watch-loop st))
      ;; A RESUME THAT FAILED: the listener holds nothing and has unlinked what
      ;; it bound. The daemon drains with its own reason and leaves 75; the
      ;; deferred connections are admitted draining by the drain.
      (`(resume-failed ,cause)
       (state-leave-set! st #f)
       (state-listener-acked-set! st #t)
       (cond
         ((state-draining st) (finish-if-drained st))
         (else
          (enter-drain! st (list 'resume-failed cause))
          (finish-if-drained st)))
       (watch-loop st))
      ;; NEVER: THE DOOR OPENS HERE AND NOWHERE ELSE. Until the store has a
      ;; value to serve, a connection could only be told to wait or be
      ;; told something untrue -- so there are no connections.
      ;;
      ;; A SIGNAL REMEMBERED DURING STARTING ACTS HERE, WITH ITS KIND (the
      ;; startup-exit design R5): `again` is second-signal, exit 75, and no
      ;; listener; `drain` is a drain entered at once, with no listener either,
      ;; which finishes drained 0 when nothing runs. Neither can reach a
      ;; connection, because there is none.
      (`(ready)
       (state-phase-set! st 'serving)
       (case (state-remembered st)
         ((again)
          (report-quietly `(exiting (reason second-signal)))
          (leave st 75))
         ((drain store-gone)
          (enter-drain! st (if (eq? (state-remembered st) 'store-gone) 'store-gone 'drained))
          (finish-if-drained st)
          (watch-loop st))
         (else
          (let ((listener (spawn (lambda () (listener-loop (state-socket st)
                                                           (state-store-pid st)
                                                           (state-store st)
                                                           (state-main st))))))
            (monitor listener)
            (state-listener-set! st listener)
            (state-roles-set! st (cons (cons listener 'listener) (state-roles st))))
          (watch-loop st))))
      ;; A CONNECTION IS REGISTERED AT ONCE AND ADMITTED BY THIS ANSWER: it
      ;; reads nothing until told. Draining: admitted to be told so. During a
      ;; leave: deferred until the attempt is decided (see the idle exit).
      (`(watch ,pid ,role ,seq)
       (monitor pid)
       (state-roles-set! st (cons (cons pid role) (state-roles st)))
       (when (> seq (state-watch-high st)) (state-watch-high-set! st seq))
       (state-idle-since-set! st (real-time))
       (cond
         ((state-draining st) (send pid (list 'admitted #t)))
         ((state-leave st) (state-deferred-set! st (cons pid (state-deferred st))))
         (else (send pid (list 'admitted #f))))
       (watch-loop st))
      ;; What a conn is busy with, so a crash can name the request that
      ;; was in flight rather than only the process that was serving it.
      (`(handling ,pid ,seq ,verb)
       (state-handling-set! st (cons (cons pid (list seq verb))
                                     (remove-key pid (state-handling st))))
       (watch-loop st))
      ;; NEVER: OPEN AND FLOCK HAPPEN HERE, IN A HANDLER THAT DOES NOT YIELD,
      ;; and with LOCK_NB, so this process never waits for anything.
      (`(lock-take ,path ,mode ,who ,tok)
       (let ((l (guard (e (#t #f)) (lock-try-acquire! path mode))))
         (when l (state-locks-set! st (cons (cons who l) (state-locks st))))
         (send who (list 'lock-answer tok (if l l 'busy))))
       (watch-loop st))
      ;; NEVER: AND SO DO THE CLOSE AND THE FORGETTING, in one handler: there
      ;; is no instant at which the descriptor is shut and the table
      ;; still names it, which is the instant at which a reused number
      ;; would be closed out from under its new owner.
      (`(lock-drop ,l ,who ,tok)
       (state-locks-set! st (drop-lock l (state-locks st)))
       ;; A RELEASE THAT RAISES DOES NOT END MAIN (the startup-exit design
       ;; R3): in STARTING the boot's own store lock comes back here, and a
       ;; raise in main is the scheduler's panic 70. TRIPWIRE, NOT A
       ;; MEASUREMENT: no row makes an unlock or a close fail.
       (guard (e (#t (trace-event! 'lock-drop-failed #f #f))) (lock-release! l))
       (send who (list 'lock-dropped tok (void)))
       (watch-loop st))
      ;; NEVER: ONE PROCESS PER WRITER, MADE ON DEMAND AND NAMED BY THE WRITER,
      ;; so that two connections working as the same writer meet in one
      ;; mailbox. A connection asks once and remembers the answer.
      (`(writer-for ,name ,who ,tok)
       (let* ((known (assoc name (state-writers st)))
              (pid (if known
                       (cdr known)
                       (let ((p (spawn (lambda () (writer-loop (state-store st) name (state-store-pid st))))))
                         (monitor p)
                         (state-writers-set! st (cons (cons name p) (state-writers st)))
                         (state-roles-set! st (cons (cons p (list 'writer name)) (state-roles st)))
                         p))))
         (send who (list 'writer-is tok pid)))
       (watch-loop st))
      ;; NEVER: ONE SIGNAL STARTS THE DRAIN AND MAIN IS THE ONLY PLACE THAT
      ;; KNOWS IT HAS. The listener stops accepting, every connection is
      ;; told to stop reading, and what is already running is allowed to
      ;; finish -- NEVER: nothing is killed. A request that has begun has
      ;; possibly already changed the store, and a caller told "draining"
      ;; about work that was done would be told something false.
      ;;
      ;; NOTE: MAIN DOES NOT KILL THE CONNECTIONS EITHER. A conn process may
      ;; be inside a write; killing it there can strand a write block in
      ;; the runtime (igropyr's own open item). Each conn is TOLD, and
      ;; closes its own connection when its write has completed.
      (`(signal ,which)
       (cond
         ;; STARTING: REMEMBERED, WITH ITS KIND, until the store's outcome (the
         ;; startup-exit design R5, R6). `again` replaces whatever is remembered;
         ;; otherwise the first remembered kind stays (drain or store-gone),
         ;; so a store-gone is not stored as drain and its reason lost.
         ((eq? (state-phase st) 'starting)
          (let ((kind (cond ((eq? which 'again) 'again)
                            ((state-remembered st) (state-remembered st))
                            (else which))))
            (state-remembered-set! st kind)
            (trace-event! 'signal-remembered kind #f))
          (watch-loop st))
         ((eq? which 'again)
          (report-quietly `(exiting (reason second-signal)))
          (leave st 75))
         ((state-draining st) (watch-loop st))
         (else
          (when (eq? which 'store-gone)
            (report-quietly `(store-gone (store ,(state-store st)))))
          (enter-drain! st (if (eq? which 'store-gone) 'store-gone 'drained))
          ;; NOTE: AND BACK INTO THE LOOP. `finish-if-drained` leaves only
          ;; when the drain is already over; on every other path main has
          ;; to keep watching, because the clock that ends a drain that
          ;; cannot finish is an arm of this receive. Measured: without
          ;; this line main returned from its own loop here, and a second
          ;; signal and the watchdog both had nobody left to reach --
          ;; three rows read as "the daemon never exited".
          (finish-if-drained st)
          (watch-loop st))))
      ;; NEVER: THE EXECUTOR ASKS BEFORE IT STARTS, AND MAIN ANSWERS. Not the
      ;; connection: a request has exactly one answerer, and if the conn
      ;; decided as well there would be interleavings where a request is
      ;; answered twice or not at all. What has begun is on main's list
      ;; until its executor says it is done, and that list is the whole
      ;; of "is the drain finished".
      (`(may-execute ,ticket ,who ,tok)
       (cond
         ((state-draining st) (send who (list 'may-execute-is tok 'draining)))
         (else
          ;; NOTE: THE OWNER TRAVELS WITH THE TICKET. An executor that dies
          ;; mid-request sends no `finished` -- a killed actor runs no
          ;; unwinds -- and without knowing whose ticket it was, main
          ;; would go on counting a request nobody is working on as in
          ;; flight, and hold a drain open until the clock ran out.
          (state-running-set! st (cons (cons ticket who) (state-running st)))
          (send who (list 'may-execute-is tok 'execute))))
       (watch-loop st))
      (`(finished ,ticket)
       ;; NOTE: BY `equal?`, not `eq?`: a ticket is a number, and `eq?` on
       ;; numbers is not something this may depend on.
       (state-running-set! st
         (filter (lambda (entry) (not (equal? ticket (car entry)))) (state-running st)))
       (state-idle-since-set! st (real-time))
       (finish-if-drained st)
       (watch-loop st))
      ;; The conn processes report themselves gone; when the last one has
      ;; and nothing is running, the drain is over.
      (`(conn-done ,who)
       (state-roles-set! st (remove-key who (state-roles st)))
       (state-idle-since-set! st (real-time))
       (finish-if-drained st)
       (watch-loop st))
      ;; THE STORE DIED WHILE STARTING WITHOUT SENDING `startup-failed` (a death
      ;; without the message, D15; AG-a in M2b): a terminal clause of its own
      ;; (the startup-exit design R7). It traces daemon-down itself, writes
      ;; the store-actor-down line and leaves 75, and NEVER reaches the
      ;; SERVING cleanup below (release-all-of) -- the exit closes whatever
      ;; main holds.
      (`#(DOWN ,who ,reason)
       (if (and (eq? (state-phase st) 'starting) (eq? who (state-store-pid st)))
           (begin
             (trace-event! 'daemon-down (list 'store (reason-text reason)) #f)
             (report-quietly '(exiting (reason store-actor-down)))
             (leave st 75))
       (let ((role (or (lookup who (state-roles st)) 'unknown)))
         (unless (eq? reason 'normal)
           (trace-event! 'daemon-down
                         (list role (lookup who (state-handling st)) (reason-text reason))
                         #f))
         ;; NEVER: WHATEVER IT WAS HOLDING IS RELEASED HERE AND NOWHERE ELSE.
         ;; A killed actor runs no unwinds, so a descriptor it was using
         ;; would otherwise keep the store locked for as long as this
         ;; daemon lives.
         (state-locks-set! st (release-all-of who (state-locks st)))
         ;; and a connection's progress count, which only it would drop
         (progress-reclaim! who)
         ;; Whatever it had begun is no longer being worked on by anyone.
         (state-running-set! st
           (filter (lambda (entry) (not (eq? who (cdr entry)))) (state-running st)))
         ;; NEVER: AND IT IS FORGOTTEN BEFORE ANYTHING CAN ASK FOR IT AGAIN,
         ;; so the next request for that writer gets a new process
         ;; rather than a pid nobody is listening on.
         (state-writers-set! st (forget-pid who (state-writers st)))
         (cond
           ;; The store and the listener are the daemon; a conn is not,
           ;; and neither is one writer.
           ((or (eq? who (state-store-pid st)) (eq? who (state-listener st)))
            ;; A death while SERVING or DRAINING; report-quietly keeps the exit
            ;; at 75 if the line cannot be written. A startup failure never
            ;; reaches here: its clause ends main (the startup-exit design).
            (report-quietly `(exiting (reason ,(if (eq? who (state-store-pid st))
                                                   'store-actor-down
                                                   'listener-down))))
            (leave st 75))
           (else
            (state-roles-set! st (remove-key who (state-roles st)))
            (state-handling-set! st (remove-key who (state-handling st)))
            (state-idle-since-set! st (real-time))
            ;; NOTE: A CONNECTION THAT DIED IS A CONNECTION THAT IS GONE. It
            ;; sends no `conn-done` -- a killed actor runs no unwinds --
            ;; so the drain's completion condition has to be re-asked
            ;; here as well, or a crash during a drain would be reported
            ;; as a drain that timed out.
            (finish-if-drained st)
            (watch-loop st))))))))

  ;; NEVER: ONE WAY OUT, SO THE SOCKET IS TIDIED ON ALL OF THEM. A daemon
  ;; that leaves its socket behind is one the next client waits on before
  ;; falling back, and every exit path that forgot the unlink would be a
  ;; separate small version of that bug.
  ;; THE EXIT CODE IS DECIDED BEFORE THE TIDYING, AND THE TIDYING CANNOT
  ;; CHANGE IT (found in review). A failed unlink probe or lock
  ;; release raised here, past the exit, into the scheduler's panic 70 --
  ;; for a start that had already said why it was leaving with 75. The exit
  ;; closes the lock's descriptor, which releases the flock anyway.
  ;; TRIPWIRE, NOT A MEASUREMENT: no row makes an unlock or a close fail.
  (define (leave st code)
    (unlink-own-socket! (state-socket st) (state-bound st))
    ;; AND WHAT A RESUME BOUND, when main has not yet recorded it: the cell is
    ;; written before the listener runs anything after the rebind.
    (when (and listener-cell (not (equal? listener-cell (state-bound st))))
      (unlink-own-socket! (state-socket st) listener-cell))
    (guard (e (#t (trace-event! 'leave-tidy-failed #f #f)))
      (lock-release! (state-lock st)))
    (exit code))

  ;; NEVER: FINISHED MEANS NOTHING IS RUNNING AND NO CONNECTION IS LEFT --
  ;; both, because either alone is reached long before the work is over:
  ;; a connection with nothing in flight still holds bytes it has been
  ;; told not to read, and a request still running still has an answer
  ;; owed to somebody.
  ;;
  ;; KEY: AND THE SECOND CONDITION IS WHAT COVERS A CONNECTION-LOCAL READ.
  ;; Those are answered by the connection's own process without asking
  ;; main, so they are not in `state-running` -- main cannot see them at
  ;; all. It does not need to: the connection serving one is registered
  ;; as a conn, and this cannot fire while it is. D-35 holds a read open
  ;; across a drain and reads the order of the two.
  ;;
  ;; NOTE: AND THOSE READS ARE NOT PUT THROUGH THE ADMISSION CHECK EITHER,
  ;; deliberately. A frame already buffered when a drain begins is served
  ;; rather than refused: it changes nothing, and the answer it gets is
  ;; the right one. Refusing a correct answer so that two paths agree
  ;; would be putting the rule ahead of the result.
;; The drain's entry, once: from a first signal while SERVING, or from a
  ;; drain remembered while STARTING when `ready` arrives. REASON is drained or
  ;; store-gone; it is kept from here to the exit line, never overwritten.
  ;; A DRAIN THAT HAS A LISTENER TELLS IT TO STOP AND WAITS FOR ITS ACCOUNT of
  ;; the adapters it spawned (finish-if-drained), so a connection accepted
  ;; before the stop is admitted draining, never left behind. A listener in a
  ;; leave takes the stop when its grace ends. The connections deferred by a
  ;; leave are admitted now, draining, before any is told to drain.
  (define (enter-drain! st reason)
    (unless (state-drain-reason st) (state-drain-reason-set! st reason))
    (state-draining-set! st (real-time))
    (report-quietly `(draining (in-flight ,(length (state-running st)))))
    (when (and (state-listener st) (not (state-listener-acked st)))
      (send (state-listener st) (list 'stop)))
    (admit-deferred! st #t)
    (for-each (lambda (entry)
                (when (eq? 'conn (cdr entry)) (send (car entry) (list 'drain))))
              (state-roles st)))

  ;; THE LISTENER'S ACKNOWLEDGEMENT IS PART OF "DRAINED" when there is a
  ;; listener: until it has accounted for its adapters, a connection may still
  ;; be on its way to main. A drain entered before any listener existed (the
  ;; startup drain) has nothing to wait for. A drain entered because a resume
  ;; failed leaves 75, with its own line.
  (define (finish-if-drained st)
    (when (and (state-draining st)
               (null? (state-running st))
               (not (exists (lambda (entry) (eq? 'conn (cdr entry))) (state-roles st)))
               (or (not (state-listener st)) (state-listener-acked st)))
      (let ((reason (state-drain-reason st)))
        (if (and (pair? reason) (eq? (car reason) 'resume-failed))
            (begin (report-resume-failed! st) (leave st 75))
            (begin
              (report-quietly `(exiting (reason ,(or reason 'drained))))
              (leave st 0))))))

  (define (report-resume-failed! st)
    (let ((reason (state-drain-reason st)))
      (when (and (pair? reason) (eq? (car reason) 'resume-failed))
        (report-quietly `(idle-exit (reason resume-failed ,(cadr reason)))))))

  ;; NOTHING OPEN: no connection registered and no request running. A
  ;; subscription holds its connection, so it is covered.
  (define (nothing-open? st)
    (and (null? (state-running st))
         (not (exists (lambda (entry) (eq? 'conn (cdr entry))) (state-roles st)))))

  ;; THE CLOCK HAS RUN OUT: an idle window was asked for, the daemon serves
  ;; through a bound listener, no drain or leave is under way, nothing is open
  ;; or running, and the window has passed since the last thing that restarted
  ;; it (a watch, a completion, a connection's end, a cancellation).
  (define (idle-due? st)
    (and (> idle-exit-seconds 0)
         (eq? (state-phase st) 'serving)
         (not (state-draining st))
         (not (state-leave st))
         (state-listener st)
         (state-bound st)
         (nothing-open? st)
         (>= (- (real-time) (state-idle-since st)) (* 1000 idle-exit-seconds))))

  ;; THE ATTEMPT BEGINS: its watermark is the highest watch registered.
  (define (begin-leave! st)
    (state-leave-set! st (list 'leaving (state-watch-high st)))
    (state-listener-acked-set! st #f)
    (send (state-listener st) (list 'leave 'idle)))

  ;; The deferred connections are admitted, DRAINING? or not, once.
  (define (admit-deferred! st draining?)
    (for-each (lambda (pid) (send pid (list 'admitted draining?))) (reverse (state-deferred st)))
    (state-deferred-set! st '()))

  (define (forget-pid who alist)
    (let loop ((rest alist) (kept '()))
      (cond ((null? rest) (reverse kept))
            ((eq? who (cdar rest)) (loop (cdr rest) kept))
            (else (loop (cdr rest) (cons (car rest) kept))))))

  ;; NEVER: ONLY IF IT IS STILL OURS. A daemon that leaves without tidying up
  ;; leaves a path that answers nothing, and the next client waits on it
  ;; before falling back -- but one that deletes whatever is at that path
  ;; deletes the entry of whoever took over in the meantime.
  ;; NOTHING WAS BOUND, NOTHING IS PROBED (found in review): a start
  ;; that failed before the bind has no socket of its own to tidy.
  ;; THE PRIVATE FILE IS UNLINKED BY EVERY CONTROLLED EXIT, here and
  ;; synchronously, as well as the link: the
  ;; listener may never have been closed, so the close's own unlink cannot be
  ;; relied on, and when it did run the file is already gone. The two
  ;; attempts are independent: a failure of either is logged
  ;; `(cleanup-failed <path> <errno>)` and neither delays nor changes the
  ;; exit.
  ;; BOUND is (private link-text private-identity link-identity), the
  ;; identities read by lstat when the listener bound and published: each is
  ;; unlinked only while it is still the entry this daemon made (the
  ;; identity primitives below), so a replacement at either name, or a
  ;; namespace link since pointed elsewhere, is left (found in review).
  (define (unlink-own-socket! socket bound)
    (when bound
      (cleanup-step! (car bound)
        (lambda () (unlink-made! (car bound) (caddr bound))))
      ;; A link not yet published has no identity, and nothing to unlink.
      (when (cadddr bound)
        (cleanup-step! socket
          (lambda () (unlink-made! socket (cadddr bound)))))))

  (define (cleanup-step! path thunk)
    (guard (e (#t (report-quietly
                    (list 'cleanup-failed path
                          (cond ((fs-error? e) (fs-error-errno e))
                                ((unreadable-entry? e) (unreadable-entry-errno e))
                                (else 'unknown))))))
      (thunk)))

  (define (lookup k alist)
    (let ((e (assq k alist))) (and e (cdr e))))

  (define (remove-key k alist)
    (filter (lambda (e) (not (eq? (car e) k))) alist))

  ;; A condition carries its message and irritants; anything else is
  ;; written as it is. NEVER: Neither is folded into "unknown".
  (define (reason-text reason)
    (cond
      ((and (condition? reason) (message-condition? reason))
       (list 'condition (condition-message reason)
             (if (irritants-condition? reason) (condition-irritants reason) '())))
      ((condition? reason) (list 'condition 'no-message))
      (else reason)))

  ;; NOTE: TWO NAMES FOR ONE STORE ARE ONE STORE. A client may reach it
  ;; through a symlink; comparing the text alone would refuse a request
  ;; that is for exactly this library.
  ;; NOTE: THE ASKED SPELLING FIRST (D18): let*, so when the asked
  ;; path cannot be read its unreadable-entry is the one that leaves.
  (define (same-store? asked serving)
    (or (string=? asked serving)
        (let* ((a (device-inode asked)) (b (device-inode serving)))
          (and a b (equal? a b)))))


  ;; NEVER: ONLY A SOCKET IS EVER UNLINKED. A regular file or a directory on
  ;; that path is somebody else's, and removing it to make room would be
  ;; this daemon destroying data it does not own.
  ;;
  ;; NOTE: THE TEST IS NOW "IS IT A SOCKET", NOT "IS IT A FILE OR A
  ;; DIRECTORY". Those are not complements: a FIFO is neither, and
  ;; measured -- `file-is-regular?` #f, `file-is-directory?` #f -- so the
  ;; old spelling let a fifo, a device node or a dangling symlink fall
  ;; through to the unlink below. The question this guard exists to ask
  ;; is whether the thing on that path is the kind of object this daemon
  ;; made, and only `stat`'s type bits answer it.
  ;; NOTHING IS SWALLOWED HERE ANY MORE (a startup propagation): a
  ;; path that cannot be asked about raises into main's table rather than
  ;; reading as "not occupied".
  (define (occupied-by-a-non-socket? p)
    (and (device-inode p) (not (file-is-socket? p))))

  ;; NEVER: SAFE BECAUSE OF WHERE IT IS CALLED, and that is the whole of the
  ;; argument. `serve` reaches this only after `lock-try-acquire!` on the
  ;; sibling `.lock` has SUCCEEDED -- an exclusive, non-blocking flock --
  ;; and only after the branch above has established that whatever is
  ;; there is a socket. Holding that lock means no live daemon is serving
  ;; this path, so a socket file left on it is one nobody is listening
  ;; on: stale by definition.
  ;;
  ;; NOTE: IT DOES NOT TRY TO CONNECT, AND DOES NOT NEED TO. A connect test
  ;; would be a second opinion about something the lock already settled,
  ;; and a worse one: a daemon that is alive but not yet accepting would
  ;; refuse the connection and be deleted out from under itself.
  ;;
  ;; NEVER: MOVING THIS CALL BEFORE THE LOCK BREAKS IT. Two daemons starting
  ;; together would each delete the other's socket.
  ;; IDENT is the socket's identity as claim-public! judged it.
  (define (clear-stale-socket! p ident)
    (guard (e (#t #f)) (unlink-made! p ident)))

  ;; ---- the socket's namespace ----------------------------------------------
  ;;
  ;; THE SOCKET IS A PRIVATE FILE BEHIND A PUBLIC LINK. The daemon binds
  ;; `<socket>.d/<pid>` and publishes `<socket>` as a symbolic link to it,
  ;; because closing a listener unlinks the name it was bound with,
  ;; unconditionally: bound at the public path, a stop would delete a
  ;; replacement created there after our own unlink. An unlink of ours at the
  ;; public path is one of three: a start's reclaim of a link, judged by its
  ;; text; exit's unlink of our own link, judged by the identity recorded when
  ;; it was published; and a pre-link daemon's socket, cleared under the
  ;; lifetime lock.
  ;;
  ;; THE NAMESPACE IS A DIRECTORY THE DAEMON MAKES ITSELF, RESERVED BY A MARKER.
  ;; Its real name is `<socket>.d.<pid>` (or `-1`, `-2`, ... when that name is
  ;; taken), made by exclusive creation, so nothing already there is ever
  ;; replaced; `.endpoint` inside it holds the endpoint's identity, the pid and
  ;; the directory's own name; and `<socket>.d` is published as a symbolic
  ;; link to it, which fails on any existing entry. OWNERSHIP IS THE MARKER,
  ;; NEVER THE NAME: a directory spelled like ours whose marker does not name
  ;; this endpoint is somebody else's and is left as it is. A marker outlives
  ;; its daemon: it is the reservation, and the next start of the same
  ;; endpoint reclaims it.
  ;;
  ;; THE ENDPOINT'S IDENTITY is the canonical path of the public link's parent
  ;; as the start's one look resolved it (realpath; the link itself is never
  ;; resolved) joined with the link's base name, so `--socket run/s` from one directory and `--socket s` from
  ;; `run` are one endpoint, as they are one lock. Two identities are compared
  ;; parent by parent and base name by base name, the base name under the
  ;; parent filesystem's own case rule.



  (define (namespace-link socket) (string-append socket ".d"))
  ;; NEVER: THE RESERVATION LOCK'S NAME IS NO LIFETIME LOCK'S. A lifetime lock
  ;; is `.<base>.lock` (lock-path-for), so `<socket>.d.lock` made `.s`'s
  ;; reservation lock `.s.d.lock`, the lifetime lock of the endpoint `s.d` in
  ;; the same directory: while `s.d` served, every start of `.s` waited out
  ;; the reservation budget and was refused serve-busy naming the other
  ;; endpoint's lock (found in review). The names beside a socket end in
  ;; four different characters, case folded or not: `k` (the lifetime lock),
  ;; `e` (this one), `d` (the link) and a digit (a backing directory), so no
  ;; two endpoints' derived names of different kinds are one file. A PUBLIC
  ;; name may still be spelled as a derived one. Where it has a LOCK's shape
  ;; (`.s.lock`, `s.d.reserve`; lock-spelling) the start is refused
  ;; serve-name-derived before anything is made, since a lock is opened
  ;; through whatever is at its name. Where it has a link's (`s.d`), the
  ;; link is published by a create that fails on any entry, so whichever
  ;; comes second is refused by occupancy, and a public `s.d` beside `.s`
  ;; serves (W5x22). Where it has a backing directory's (`s.d.7`), `s` makes
  ;; its backing directory exclusively under the next free name
  ;; (allocate-backing!), so it steps around a public `s.d.7`, and a public
  ;; start at a backing directory that exists is refused by occupancy.
  (define (reservation-lock-path socket) (string-append socket ".d.reserve"))
  (define (private-socket-path socket pid)
    (string-append socket ".d/" (number->string pid)))
  ;; RELATIVE TO THE LINK'S PARENT, as the link resolves it: a target spelled as
  ;; the bind path would resolve to `run/run/s.d/<pid>` for `--socket run/s`.
  (define (public-link-target socket pid)
    (string-append (base-of socket) ".d/" (number->string pid)))

  ;; FOLDING, ONE PLACE: same-spelling? and namespace-spelling, the rule's
  ;; comparison and its one parser, are (theourgia client)'s, beside the walk
  ;; that judges a run directory and a socket's directory alike; these two
  ;; build on them.
  (define (starts-with? text prefix fold?)
    (let ((n (string-length prefix)))
      (and (>= (string-length text) n)
           (same-spelling? (substring text 0 n) prefix fold?))))

  ;; -> #t when TEXT is `<prefix><digits>`.
  (define (prefixed-digits? text prefix fold?)
    (let ((n (string-length prefix)))
      (and (> (string-length text) n)
           (starts-with? text prefix fold?)
           (digits? (substring text n (string-length text))))))



  ;; A PARENT'S SPELLING, LEXICALLY TIDIED before its base name is parsed:
  ;; `//` collapsed and a trailing `/` or `/.` dropped, so `s.d//7` and
  ;; `s.d/./7` name `s.d` as `s.d/7` does (found in review); its base name
  ;; then says which namespace it is in.
  (define (tidy-spelling p)
    (let loop ((p (let collapse ((p p))
                    (let ((n (string-length p)))
                      (let find ((i 0))
                        (cond ((>= (+ i 1) n) p)
                              ((and (char=? (string-ref p i) #\/) (char=? (string-ref p (+ i 1)) #\/))
                               (collapse (string-append (substring p 0 i) (substring p (+ i 1) n))))
                              (else (find (+ i 1)))))))))
      (let ((n (string-length p)))
        (cond ((and (> n 1) (char=? (string-ref p (- n 1)) #\/)) (loop (substring p 0 (- n 1))))
              ((and (> n 2) (string=? (substring p (- n 2) n) "/.")) (loop (substring p 0 (- n 2))))
              (else p)))))

  ;; A backing directory's spelling for the endpoint whose base name is BASE.
  (define (backing-spelling? name base fold?)
    (let ((p (namespace-spelling name fold?)))
      (and p (eq? (cdr p) 'backing) (same-spelling? (car p) base fold?))))


  ;; NEVER: THE IDENTITY IS TAKEN FROM THE ONE LOOK, NOT RESOLVED AGAIN.
  ;; PARENT is the socket's parent as the start's reservation check resolved
  ;; it (reserved-by-another): its one real-path, or the directory the walk
  ;; answered and main made. Resolved a second time here, a prefix through an
  ;; endpoint's link that its reclaim had unlinked failed, the spelling stood
  ;; in, and once the link was republished the start reserved and marked
  ;; `<d>/s.d/../b`; parents compare as bytes, so every later start of
  ;; `<d>/b` was refused (found in review).
  (define (endpoint-identity parent socket)
    (in-directory parent (base-of socket)))

  ;; The parents compare as BYTES: each is real-path's canonical spelling
  ;; (endpoint-identity), and the volume's case rule applies to the base name.
  (define (same-endpoint? a b)
    (let ((pa (directory-of a)) (pb (directory-of b)))
      (and (string=? pa pb)
           (same-spelling? (base-of a) (base-of b) (folds-case? pa)))))

  (define (marker-path dir) (in-directory dir ".endpoint"))

  ;; -> the identity a directory's marker names, or #f when it has none or its
  ;; text is not a marker: a marker that does not name an endpoint is not
  ;; ours. A marker that cannot be read at all raises, into main's table.
  ;; NEVER: ONLY A REGULAR FILE IS READ. A FIFO named `.endpoint` would block
  ;; the open with both locks held (found in review); it is not a marker.
  (define (marker-identity dir)
    (let ((bytes (and (eq? (entry-name-type (marker-path dir)) 'regular)
                      (read-entry (marker-path dir)))))
      (and (bytevector? bytes)
           (let ((d (guard (e (#t #f)) (read (open-string-input-port (utf8->string bytes))))))
             (and (pair? d) (eq? (car d) 'endpoint) (list? (cdr d))
                  (let ((id (assq 'identity (filter pair? (cdr d)))))
                    (and id (pair? (cdr id)) (string? (cadr id)) (cadr id))))))))

  ;; A marker is a regular file: anything else named `.endpoint` reserves
  ;; nothing (found in review).
  (define (marker-present? dir)
    (eq? (entry-name-type (marker-path dir)) 'regular))

  (define (marked-for? dir identity)
    (let ((id (marker-identity dir)))
      (and id (same-endpoint? id identity))))

  ;; ---- (1) IDENTITY, ONE PLACE -------------------------------------------------
  ;;
  ;; Every removal in the namespace goes through these two. IDENT is the
  ;; entry's (device . inode) by lstat, recorded when this start made the
  ;; entry or when it judged one an earlier daemon left; the entry is removed
  ;; only while lstat still reads that identity, so a replacement is never
  ;; removed for the entry it replaced. -> removed; kept (something else is
  ;; there now); gone (nothing is). A removal that fails raises, and the
  ;; caller's guard decides.
  ;; THE MODEL: the namespace `<socket>.d*` is the daemon's own. What it
  ;; defends against is its peers -- other daemons, under the reservation and
  ;; lifetime locks -- and stale residue. A concurrent foreign writer inside
  ;; it is outside the model, bounded by (a) and by the next start's reclaim.
  ;; NOTE: THE ACCEPTED WINDOWS, ALL OF THEM, HERE (the README lists the same
  ;; nine). Each is a path call where no *at call with an identity exists,
  ;; or a step whose failure this code cannot observe; each says its effect
  ;; and what bounds it.
  ;;   (a) CHECK-THEN-ACT: made? and the removal are two calls on a path; an
  ;;       entry swapped between them is removed. Only a writer inside this
  ;;       endpoint's own, lock-protected names can do that. The same holds
  ;;       between any two steps of one start: a directory swapped in after
  ;;       the marker is written is published unmarked; a residue swapped in
  ;;       after it was judged is swept; a public link replaced before its
  ;;       identity is read is recorded and removed at exit; the directory a
  ;;       start resolved and then makes its socket's directory under is
  ;;       taken as it is at the mkdir, so only a foreign rename replaces it
  ;;       in between (one inside a namespace was refused at the start's
  ;;       look); a directory made by hand inside an endpoint's directory
  ;;       admits a start under it. No peer and no residue produces these
  ;;       states, so no re-check is added for them.
  ;;   (b) THE LISTENER'S CLOSE unlinks the bound private name, as closing a
  ;;       bound pipe does, whatever is at that name in the daemon's own
  ;;       directory by then.
  ;;   (c) `.endpoint` SWAPPED between its exclusive create and its open: the
  ;;       marker's text goes into the swapped-in file. Only a writer inside
  ;;       the directory this start has just made.
  ;;   (d) A STALE PUBLICATION at the socket path is reclaimed first, at every
  ;;       start, whatever the start then answers: it was this endpoint's,
  ;;       behind the lifetime lock the start holds.
  ;;   (e) THE MADE DIRECTORY'S IDENTITY is read by path just after its
  ;;       exclusive mkdir: a directory renamed into its place in that
  ;;       instant is recorded as this start's, and a rollback may remove it.
  ;;       Only a writer in the socket's directory within that instant.
  ;;   (f) A FAILURE RIGHT AFTER THE MARKER'S EXCLUSIVE CREATE (its lstat, or
  ;;       the create's own close) leaves an empty marker no log step covers:
  ;;       a directory nobody links stays reserved. A start inside it is
  ;;       refused naming that marker, with the remedy to remove the directory.
  ;;   (g) AN IDENTITY READ FAILING RIGHT AFTER THE PUBLISH leaves the public
  ;;       link outside `bound`, as a kill after the publish would; the next
  ;;       start reclaims it as a stale publication (d).
  ;;   (h) A RESERVATION-LOCK RELEASE THAT FAILS after a completed reservation
  ;;       leaves it published and marked and the start fails, as a daemon dead
  ;;       after its publish; the next start reclaims it.
  ;;   (i) AN IDENTITY READ THAT FAILS RIGHT AFTER THE EXCLUSIVE MKDIR of this
  ;;       daemon's directory (entry-identity raising for an errno other than
  ;;       ENOENT or ENOTDIR) rolls back a log with no made-dir step, so the
  ;;       empty, unmarked, unlinked `<base>.d.<pid>` stays: never entered or
  ;;       swept, the next start allocating a fresh suffix. Logging the
  ;;       directory before its identity is read would hand the rollback a
  ;;       directory without the identity it compares (found in review). The
  ;;       reservation runs under main's `report` stage, so a stat fault aimed
  ;;       at `<base>.d.<pid>` fires at that read; W5x21 pins this state.
  ;; -> absent, kept or made: what lstat reads at PATH against IDENT.
  (define (made? path ident)
    (let ((now (entry-identity path)))
      (cond ((not now) 'absent)
            ((equal? now ident) 'made)
            (else 'kept))))

  (define (unlink-made! path ident)
    (case (made? path ident)
      ((absent) 'gone)
      ((kept) 'kept)
      (else (unlink! path) 'removed)))

  (define (rmdir-made! path ident)
    (case (made? path ident)
      ((absent) 'gone)
      ((kept) 'kept)
      (else (rmdir! path) 'removed)))

  ;; THE RESERVATION LOCK, `<socket>.d.reserve`, beside the lifetime lock: held by
  ;; a reserving daemon from before its mkdir through its marker and its
  ;; publication, and by a start that asks whether a namespace is reserved,
  ;; for the time it takes to read the marker. It is waited for, within the
  ;; start budget, since its holders keep it only for those few steps.
  (define reservation-budget-ms 10000)
  (define (reservation-lock! socket)
    (let ((path (reservation-lock-path socket)))
      (file-ensure! path)
      (let ((deadline (+ (real-time) reservation-budget-ms)))
        (let loop ()
          (let ((lock (lock-try-acquire! path 'exclusive)))
            (cond
              ((and lock (lock-held? lock)) lock)
              ((> (real-time) deadline) #f)
              (else (sleep-ms 20) (loop))))))))



  ;; BEFORE ANY LOCK OF ITS OWN: a requested path whose parent, resolved,
  ;; is spelled `<name>.d` or `<name>.d.<digits>[-<n>]` lies in the candidate
  ;; namespace of the endpoint `<name>`. Its reservation lock is taken and
  ;; the marker read under it: a marker there means the namespace is
  ;; reserved, its daemon running or not. -> (admit DIR PARENT), DIR what a
  ;; start makes for the parent, (ANCESTOR . NAMES) from directory-to-make,
  ;; or #f when the parent resolved, PARENT the parent as this one look
  ;; resolved it (for DIR, the ancestor joined with the names),
  ;; which the endpoint's identity is taken from (endpoint-identity); or the
  ;; refusal.
  ;; NEVER: THE MARKER IS READ ON A PATH RESOLVED BEFORE THE LOCK. The first
  ;; resolution only names the lock; while this start waits on it, the
  ;; endpoint's own daemon may reclaim the directory it named and publish
  ;; another, and a marker read in the old one would be absent and admit a
  ;; start whose takeover then clears that daemon's live private socket
  ;; (found in review). So the parent is resolved again under the lock, and
  ;; a resolution that now names another endpoint starts the check over.
  ;; NEVER: A PARENT THAT DOES NOT RESOLVE IS NOT AN UNMARKED ONE. The
  ;; endpoint's own reclaim unlinks `<name>.d` for a moment; a parent that
  ;; does not resolve is judged by its spelling, and under the lock it
  ;; refuses socket-dir-missing rather than admitting the start (found in
  ;; review).
  (define (reserved-by-another socket)
    (let* ((parent (tidy-spelling (directory-of socket)))
           (first (real-path parent))
           (spelled (or first parent))
           (fold? (folds-case? (directory-of spelled)))
           (owner (namespace-candidate (base-of spelled) fold?)))
      ;; INJECTION ONLY: held after the parent's one resolution, before it is
      ;; judged.
      (hold-point! 'reserve-after-first-resolve)
      (cond
        ;; NEVER: A PARENT THAT DOES NOT RESOLVE IS ADMITTED ONLY WHEN A START
        ;; MAY MAKE IT. Four ways it may not (directory-to-make), each found
        ;; in review. The walk looks at the parent by lstat only; the real-path
        ;; above (`first`) was the one resolution of it.
        ;; - it is something: an alias of a namespace (a link to `<name>.d`)
        ;;   does not resolve while that endpoint's reclaim has the link
        ;;   unlinked, and its own spelling names no namespace, so it would
        ;;   pass with no lock; resolved a second time after that endpoint
        ;;   republished, it would pass too;
        ;; - making it would make a namespace-spelled directory, or one through
        ;;   `..`: the absent names are made after this check, below the ancestor
        ;;   the walk resolved, and `s.d/../b` or `s.d/x/b`, while the endpoint `s`'s
        ;;   reclaim has its link unlinked, would make a real `s.d` there, and
        ;;   that endpoint's publish would refuse from then on;
        ;; - the first ancestor that is there is a namespace's directory (it
        ;;   resolves to a name spelled `<name>.d` or `<name>.d.<digits>`): once
        ;;   `s` republished, `s.d/x/b` would be made inside its new backing;
        ;; - the walk's floor does not resolve: a working directory that was
        ;;   removed.
        ;; What is admitted is a parent with nothing at it that a start may
        ;; make: the default socket's run directory, made after this check
        ;; under the ancestor the walk resolved, by the names it judged.
        ((and (not owner) (not first))
         (let ((dir (directory-to-make parent)))
           (if dir
               `(admit ,dir ,(fold-left in-directory (car dir) (cdr dir)))
               `(error socket-dir-missing (dir ,parent)))))
        ((not owner) `(admit #f ,first))
        (else
           (let* ((candidate (in-directory (directory-of spelled) owner))
                  (lock (begin (hold-point! 'reserve-after-resolve)
                               (reservation-lock! candidate))))
             (if (not lock)
                 `(error serve-busy (path ,(reservation-lock-path candidate)))
                 (let ((resolved (real-path parent)))
                   (cond
                     ((not resolved)
                      (lock-release! lock)
                      `(error socket-dir-missing (dir ,parent)))
                     ;; BYTES: both parents are real-path's canonical spelling of
                     ;; the same lookup, so they compare exactly.
                     ((not (and (equal? (directory-of resolved) (directory-of spelled))
                                (let ((again (namespace-candidate (base-of resolved) fold?)))
                                  (and again (same-spelling? again owner fold?)))))
                      (lock-release! lock)
                      (reserved-by-another socket))
                     (else
                      (let ((marked (marker-present? resolved))
                            (identity (marker-identity resolved)))
                        (lock-release! lock)
                        (if marked
                            `(error serve-path-reserved (endpoint ,(or identity (marker-path resolved)))
                                    (remedy serve-that-endpoint-or-remove-its-directory))
                            `(admit #f ,resolved))))))))))))

  ;; A RESIDUE DIRECTORY OF THIS ENDPOINT, judged as DIR-IDENT with the marker
  ;; MARKER-IDENT: its stale sockets go (the lifetime lock is ours, so nobody
  ;; listens on them), every other entry and the marker stay, and the
  ;; directory is removed only when nothing but the marker is left, the
  ;; marker first. -> swept; clean when there was nothing to remove and
  ;; nothing failed; untouched when a step failed before anything was
  ;; removed (a directory this daemon cannot write fails at its first
  ;; unlink); partial when one failed after, which leaves the directory with
  ;; what is in it -- unmarked, it never blocks a start.
  (define (sweep-residue! dir dir-ident marker-ident)
    (let ((removed #f)
          (marker? (let ((fold? (folds-case? dir)))
                     (lambda (name) (same-spelling? name ".endpoint" fold?)))))
      (guard (e ((or (fs-error? e) (unreadable-entry? e)) (if removed 'partial 'untouched)))
        (for-each (lambda (name)
                    (let* ((p (in-directory dir name))
                           (ident (entry-identity p)))
                      ;; A SOCKET BY lstat: a link in the residue to a live
                      ;; socket elsewhere is a non-socket entry and stays.
                      (when (and (not (marker? name))
                                 (eq? (entry-name-type p) 'other)
                                 (file-is-socket? p)
                                 (eq? (unlink-made! p ident) 'removed))
                        (set! removed #t))))
                  (directory-entries dir))
        (hold-point! 'reclaim-after-sockets)
        (when (let ((left (directory-entries dir)))
                (and (= (length left) 1) (marker? (car left))))
          (when (eq? (unlink-made! (marker-path dir) marker-ident) 'removed)
            (set! removed #t))
          (hold-point! 'reclaim-after-marker)
          (rmdir-made! dir dir-ident))
        (if removed 'swept 'clean))))

  ;; THE PUBLIC ENTRY, judged by lstat: a link spelled `<base>.d/<digits>` is
  ;; this endpoint's publication and is reclaimed whatever its target's state;
  ;; a socket is a pre-link daemon's and is cleared as before; anything else
  ;; is somebody's and refuses. -> #f, or the refusal.
  (define (claim-public! socket)
    (let ((ident (entry-identity socket)))
      (case (entry-name-type socket)
        ((absent) #f)
        ((link)
         (if (and (prefixed-digits? (read-link socket) (string-append (base-of socket) ".d/")
                                    (folds-case? (directory-of socket)))
                  (memq (unlink-made! socket ident) '(removed gone)))
             #f
             `(error serve-path-occupied (path ,socket))))
        (else
         (if (occupied-by-a-non-socket? socket)
             `(error serve-path-occupied (path ,socket))
             (begin (clear-stale-socket! socket ident) #f))))))

  ;; THE NAMESPACE LINK, judged by its target's marker. THE ORDER OF A RECLAIM:
  ;; the link is unlinked FIRST, after its target was seen to be a marked
  ;; directory of this endpoint, and only then the residue swept (by
  ;; sweep-siblings!, which the directory now is), so an interruption leaves
  ;; either a link to a still-marked directory or no link, never an alias
  ;; whose marker is gone. A link whose target is absent is reclaimed by its
  ;; target's spelling. The unlink of a link to a marked directory goes in
  ;; the undo log (DID!), so a later refusal puts it back. -> #f, or the
  ;; refusal.
  (define (claim-namespace! socket identity did!)
    (let* ((ns (namespace-link socket))
           (parent (directory-of socket))
           (ident (entry-identity ns))
           (occupied `(error serve-path-occupied (path ,ns))))
      (case (entry-name-type ns)
        ((absent) #f)
        ((link)
         (let ((target (read-link ns)))
           (if (not (backing-spelling? target (base-of socket) (folds-case? parent)))
               occupied
               (let ((dir (in-directory parent target)))
                 (case (entry-type dir)
                   ((absent)
                    (case (unlink-made! ns ident)
                      ((removed)
                       (did! (list 'unlinked-link ns target dir))
                       (hold-point! 'reclaim-after-link)
                       #f)
                      ((gone) (hold-point! 'reclaim-after-link) #f)
                      (else occupied)))
                   ((directory)
                    (if (marked-for? dir identity)
                        (case (unlink-made! ns ident)
                          ((removed)
                           (did! (list 'unlinked-link ns target dir))
                           (hold-point! 'reclaim-after-link)
                           #f)
                          ((gone) #f)
                          (else occupied))
                        occupied))
                   (else occupied))))))
        (else occupied))))

  ;; The marked directories of this endpoint beside it (a start interrupted
  ;; before its publication, and the one the namespace link named, NAMED,
  ;; once claim-namespace! unlinked the link): swept; an unmarked or foreign
  ;; one is left exactly as it is and does not block the start. A SIBLING
  ;; THAT CANNOT BE SWEPT, or cannot even be asked about (one this daemon may
  ;; not read), is left and does not block the start. The directory the link
  ;; named is the exception when its sweep FAILED having removed nothing
  ;; (untouched): the start is refused naming the link, and the rollback puts
  ;; the link back. A sweep that had nothing to remove (clean) is no failure. It is
  ;; tried FIRST, so that refusal comes before any other sibling is touched
  ;; (found in review). A sweep of that directory that removed something
  ;; goes in the undo log (DID!) as named-touched, so the rollback does not
  ;; put the link back to it. -> #f, or the refusal.
  (define (sweep-siblings! socket identity named did!)
    (let* ((parent (directory-of socket)) (base (base-of socket)) (fold? (folds-case? parent))
           (named? (lambda (n) (and named (same-spelling? n named fold?))))
           (all (directory-entries parent)))
      (let loop ((names (append (filter named? all) (remp named? all))))
        (if (null? names)
            #f
            (let* ((name (car names))
                   (dir (in-directory parent name))
                   (outcome (if (backing-spelling? name base fold?)
                                (guard (e ((or (fs-error? e) (unreadable-entry? e)) 'untouched))
                                  (let ((dir-ident (entry-identity dir))
                                        (marker-ident (entry-identity (marker-path dir))))
                                    (if (and (eq? (entry-name-type dir) 'directory)
                                             (marked-for? dir identity))
                                        (sweep-residue! dir dir-ident marker-ident)
                                        'not-ours)))
                                'not-ours)))
              (when (and (memq outcome '(swept partial)) (named? name))
                (did! '(named-touched)))
              (if (and (eq? outcome 'untouched) (named? name))
                  `(error serve-path-occupied (path ,(namespace-link socket)))
                  (loop (cdr names))))))))

  ;; ALLOCATED BY EXCLUSIVE CREATION, with a fresh suffix on collision: an
  ;; existing directory at the name -- a reused pid's markerless residue, or
  ;; somebody else's -- is never entered. -> the directory's name.
  (define (allocate-backing! socket)
    (let ((parent (directory-of socket))
          (stem (string-append (base-of socket) ".d." (number->string (process-id)))))
      (let loop ((n 0))
        (let ((name (if (= n 0) stem (string-append stem "-" (number->string n)))))
          (if (eq? (mkdir-exclusive! (in-directory parent name)) 'created)
              name
              (loop (+ n 1)))))))

  ;; THE MARKER'S TEXT, written into the file the reservation created
  ;; exclusively: an entry placed at `.endpoint` after the directory was made
  ;; is somebody's, and is never written over. An entry swapped in between
  ;; the create and the open is not caught: fd-open has no O_NOFOLLOW.
  (define (write-marker! dir identity name)
    (let ((fd (fd-open (marker-path dir) '(write))))
      (write-all! fd (string->utf8
                       (call-with-string-output-port
                         (lambda (p)
                           (write `(endpoint (identity ,identity) (pid ,(process-id)) (directory ,name)) p)
                           (newline p)))))
      (fd-close fd)))

  ;; ---- (2) ROLLBACK, ONE PLACE -------------------------------------------------
  ;;
  ;; UNDO is a reservation's log, newest first: (made-marker <path> <ident>),
  ;; (made-dir <path> <ident>), (named-touched), (unlinked-link <link> <text>
  ;; <dir>). Each step
  ;; is undone through the identity primitives, best effort: a step that
  ;; fails is not reported, since the start's own refusal or failure is what
  ;; it says, and an unmarked directory left behind never blocks a start. A
  ;; namespace link this start unlinked is put back, to the same text, unless
  ;; the log says this start's sweep removed something from the directory it
  ;; named (named-touched): decided from what this start did, never by asking
  ;; the directory again, whose answer can be unreadable (found in review). A
  ;; directory whose marker a sweep removed is no longer a reservation, and a
  ;; link to it would refuse every later start.
  ;; NOTE: A KILL BEFORE THE LINK IS PUT BACK leaves no link and a marked
  ;; directory, the state an interrupted reclaim leaves; the next start finds
  ;; the directory as a sibling it cannot sweep, leaves it, and serves from a
  ;; directory of its own.
  (define (roll-back! undo identity)
    (for-each
      (lambda (step)
        (guard (e ((or (fs-error? e) (unreadable-entry? e)) #f))
          (case (car step)
            ((made-marker) (unlink-made! (cadr step) (caddr step)))
            ((made-dir) (rmdir-made! (cadr step) (caddr step)))
            ((unlinked-link)
             (unless (assq 'named-touched undo)
               (symlink! (caddr step) (cadr step)))))))
      undo))

  ;; UNDER THE LIFETIME LOCK: the public entry judged first, which needs that
  ;; lock alone; then, under the reservation lock as well, the namespace link
  ;; and the residue judged and reclaimed, and this daemon's own directory
  ;; made, marked and published. PARENT is the socket's parent as the
  ;; reservation check resolved it. -> #f, or the refusal; on a refusal after
  ;; the directory was made it is removed again.
  (define (claim-endpoint! socket parent)
    (or (claim-public! socket)
        (claim-namespace-and-reserve! socket parent)))

  ;; ---- (3) THE RAISE PATH, ONE PLACE ----------------------------------------
  ;;
  ;; The reservation runs inside one guard: whatever raises in it -- a probe,
  ;; a listing, a step -- rolls the log back and goes on to main's table, and
  ;; a refusal rolls it back too. Nothing of the sequence sits outside it.
  (define (claim-namespace-and-reserve! socket parent)
    ;; INJECTION ONLY: held before the endpoint's identity is taken, after
    ;; the lifetime lock and the public entry.
    (hold-point! 'reserve-before-identity)
    (let ((identity (endpoint-identity parent socket)))
      ;; INJECTION ONLY: held after the identity, before the reservation lock.
      (hold-point! 'reserve-after-identity)
      (let ((lock (reservation-lock! socket)))
        (if (not lock)
            `(error serve-busy (path ,(reservation-lock-path socket)))
            (let* ((undo '())
                   (did! (lambda (step) (set! undo (cons step undo))))
                   (answer
                    (guard (e (#t (roll-back! undo identity) (lock-release! lock) (raise e)))
                      (let ((refusal (reserve! socket identity did! (lambda () undo))))
                        (when refusal (roll-back! undo identity))
                        refusal))))
              ;; NEVER: THE RELEASE IS AFTER THE GUARD. flock's unlock comes before
              ;; the close, so a release that raises may already have let a peer
              ;; in, and a rollback then would dismantle the reservation under
              ;; that peer's eyes (found in review). A release that raises after a
              ;; completed reservation leaves it published and marked, as a daemon
              ;; dead after its publish would, and the next start reclaims it:
              ;; window (h).
              (lock-release! lock)
              answer)))))

  ;; THE STEPS, each logged as it succeeds: the namespace link, the siblings,
  ;; then this daemon's own directory, its marker and its publication.
  ;; NEVER: A DIRECTORY THAT IS NO LONGER THE ONE MADE is not marked: it is
  ;; somebody's, and a marker of ours would make its sockets residue to the
  ;; next start (found in review). Like a taken marker name, it refuses
  ;; naming the directory.
  (define (reserve! socket identity did! undo-now)
    (or (claim-namespace! socket identity did!)
        (sweep-siblings! socket identity
                         (let ((step (assq 'unlinked-link (undo-now)))) (and step (caddr step)))
                         did!)
        (let* ((name (allocate-backing! socket))
               (dir (in-directory (directory-of socket) name))
               (made (entry-identity dir))
               ;; BYTES: "." is the spelling directory-of gives a bare name.
               (spelled (if (string=? (directory-of socket) ".") name dir)))
          (did! (list 'made-dir dir made))
          (hold-point! 'namespace-after-mkdir)
          (cond
            ((not (eq? (made? dir made) 'made))
             `(error serve-path-occupied (path ,spelled)))
            ((not (file-create-exclusive! (marker-path dir)))
             `(error serve-path-occupied (path ,spelled)))
            (else
             (did! (list 'made-marker (marker-path dir) (entry-identity (marker-path dir))))
             (write-marker! dir identity name)
             (hold-point! 'namespace-before-publish)
             (cond
               ((eq? (symlink! name (namespace-link socket)) 'created)
                #f)
               (else
                `(error serve-path-occupied (path ,(namespace-link socket))))))))))

  (define (report x) (write x) (newline))

  ;; A LINE MAIN WRITES CANNOT CHANGE AN EXIT OR END MAIN (found in
  ;; review). Every `(exiting ...)` line is
  ;; written just before `leave`, whose code is already decided, and
  ;; `(serving ...)` and `(draining ...)` are information; a report channel
  ;; that has failed -- stdout closed, a log that cannot be written -- made
  ;; each raise into the scheduler's panic 70 instead. The failure is traced,
  ;; and main goes on. Every line main writes goes through here.
  ;; MEASURED for the exiting line: with stdout closed, the store-actor-down
  ;; line after a failed startup report raises, and without this guard the
  ;; start panics (M2a r3, "P2-startup closed stdout", mutant MC-1). The
  ;; serving and draining lines go through here too; no row measures them.
  (define (report-quietly x)
    (guard (e (#t (trace-event! 'report-failed #f #f)))
      (report x)))

  ;; ---- what has been published --------------------------------------------
  ;;
  ;; NEVER: ONE CELL, WRITTEN ONLY BY THE STORE PROCESS, READ BY EVERYBODY.
  ;; Not a message broadcast to each reader, and NEVER: not a question the
  ;; readers ask the store: both would put the store process back in the
  ;; path of every read, which is the whole thing this arrangement
  ;; removes. Green threads share one OS thread, so a value written here
  ;; is visible to every process in this VM at the next instruction --
  ;; the same property that makes a Chez parameter useless for a
  ;; per-process override is exactly what is wanted here.
  ;;
  ;; NOTE: THE VALUE IS NOT MODIFIED AFTER IT IS PUBLISHED. Each write folds
  ;; into a reduction of its own, so publishing hands over an object
  ;; nothing will touch again; readers are given it as a value they may
  ;; not change (see `reduction-for` in the rpc layer).
  ;;
  ;; NOTE: AND THE SNAPSHOT TRAVELS WITH IT, because "is this still what is
  ;; on the disk" is a question about the instant the value was made, not
  ;; about now.
  ;;
  ;; NEVER: A SNAPSHOT SAMPLED AFTER THE STATE IT IS PUBLISHED WITH. The
  ;; probe compares the published snapshot with the disk, and a snapshot
  ;; that is NEWER than its state hides every change made in between: the
  ;; probe finds the disk equal to it, asks for no fold, and the daemon
  ;; answers the older state until something else changes. Sampled after a
  ;; fold that had released the store's lock, it was exactly that: a
  ;; commit by another process between the fold and the sample was never
  ;; read. So every caller hands publish! the snapshot it sampled, and each
  ;; says why its moment is no later than its state's:
  ;;   - a fold (start-up, refold-and-publish!) samples BEFORE the fold: a
  ;;     commit between the two is in the state and not in the snapshot,
  ;;     so the next probe finds a change and folds once more -- one fold
  ;;     too many, never one too few;
  ;;   - the write path samples inside the store's lock, after the
  ;;     session's own records and before the lock is released: nothing
  ;;     that takes the lock can change the disk in between, and its own
  ;;     records are in both (sampling before them would make every write
  ;;     look like an outside change and cost a fold).
  (define published #f)
  ;; NEVER: A PROBE DURING A PUBLICATION IN PROGRESS ASKS FOR NO FOLD. The
  ;; store process is the only publisher, and while it handles a message --
  ;; a commit that has written its records and not published, a fold that
  ;; has not published -- the disk is ahead of the cell by its own work. A
  ;; probe that read that as an outside change asked for a reload, and the
  ;; store process then folded the same log again: a fold under the lock for
  ;; nothing, and a revision with no change. The yields after blocking I/O
  ;; are what let a probe run inside such a window. So the store process
  ;; marks itself busy around the handling of EVERY message (busy!, in
  ;; store-loop), whether that message can publish or not, and
  ;; probe-for-outside-change! sends no reload while the mark is up.
  ;; Written by the store process only, read by the connections and the
  ;; writer processes in the same VM: one variable, no interrupt guard.
  (define publication-pending #f)
  ;; ONE MESSAGE IS HANDLED BUSY: store-loop runs each clause's body inside
  ;; this, so the mark is up for the dynamic extent of that body and down on
  ;; every exit, the answer, the draining branch and a raise alike. Each
  ;; clause's (loop) stays outside, so the mark is down while the receive
  ;; waits. The start-up publication needs no mark: nothing can read before
  ;; `ready`.
  (define (busy! thunk)
    (dynamic-wind
      (lambda () (set! publication-pending #t))
      thunk
      (lambda () (set! publication-pending #f))))
  (define publish-seq 0)
  (define daemon-token #f)

  ;; THE PUBLICATION'S NAME, (<revision> <token>), is made here once and kept
  ;; in the cell with the state: a read seals the state with it and makes
  ;; nothing (the change stream's read --rev).
  ;; THE CELL HAS THREE READERS, pinned in the census: answer-published,
  ;; and the change stream's two -- this one, whose frame is made before the
  ;; cell holds the new state and so cannot go through answer-published, and
  ;; subscription-answer. Each takes its incomplete clause with
  ;; incomplete-clause over the state's unreadable notes, as a read does.
  ;; THE FRAME IS MADE HERE, the one place every publication passes -- a
  ;; local commit, a reload, a refresh before a read at a cut, the start --
  ;; from the reduction it replaces and the one it installs, while at
  ;; least one subscriber is registered. No caller makes one. Without a
  ;; subscriber there is no frame work at all.
  (define (publish! store state snapshot)
    (let ((old (published-state))
          (seq (+ publish-seq 1)))
      (set! publish-seq seq)
      (if (and old (pair? subscribers))
          (frame! old state seq)
          (set! published-sets #f))
      (set! published (vector state seq snapshot (list seq daemon-token)))
      (note-known-segments! snapshot)
      (trace-event! 'published seq #f)))

  ;; NEVER: A WRITER'S CURRENT SEGMENT GROWN SHORTER UNDER THE DAEMON IS NOT
  ;; WRITTEN ON. Switching a git branch while a daemon serves the store can put
  ;; a shorter segment where the daemon's was; a reload would follow it and
  ;; the next write would build on it, so the worktree's store and the
  ;; branch's fork. By contract the daemon is stopped first; this is the
  ;; guard for when it was not, and it is that one rule: the appending
  ;; writer's current segment, the same path, a different file, shorter.
  ;;
  ;; KNOWN, PER WRITER, IS ITS CURRENT SEGMENT AS PUBLISHED, (writer path .
  ;; version), set in one place, know-segment!. A publication replaces it,
  ;; unless the new version of the same path is a different file shorter
  ;; than the known one, or is no version a size can be read from while the
  ;; known one is: those are kept, so neither a reload nor a read that
  ;; failed makes the swap the known state. A publication whose current
  ;; segment is another path replaces it unchecked. The log asks the guard
  ;; under the store's lock before a write's first append (log.sc,
  ;; log-append-guard!): when the known segment on the disk now is shorter
  ;; and not the same file, the write is refused, naming the segment. The
  ;; refusal LATCHES for the writer, once seen, until the daemon is started
  ;; again: a segment that grows past the known size afterwards is still
  ;; not the file the daemon published.
  (define known-segments '())
  (define replaced-writers '())
  ;; JUDGE-SHRINK? says the new version is a publication's, judged: kept
  ;; out when it is shorter than the known one of the same path, or has no
  ;; size while the known one has. This process's own repair is not judged
  ;; (note-own-repair!): it replaces the known version, whatever it is.
  (define (know-segment! writer path version judge-shrink?)
    (let ((old (assoc writer known-segments)))
      (unless (and old
                   judge-shrink?
                   (or (and (equal? (cadr old) path) (segment-shrank? (cddr old) version))
                       (and (segment-version-readable? (cddr old))
                            (not (segment-version-readable? version)))))
        (set! known-segments
          (cons (cons writer (cons path version))
                (filter (lambda (k) (not (equal? (car k) writer))) known-segments))))))
  (define (note-known-segments! snapshot)
    (for-each (lambda (e) (know-segment! (car e) (cadr e) (cddr e) #t))
              (snapshot-current-segments snapshot)))

  ;; THIS PROCESS'S OWN REPAIR IS KNOWN AS IT LEFT THE SEGMENT. The log calls
  ;; this under the lock right after it cut a torn tail back (log.sc,
  ;; log-append-guard!), with the length it cut to: the segment is then
  ;; shorter than the version published with the residue, and that version
  ;; must not be kept against it. The path's version is known when it can be
  ;; read and is at least that long; otherwise -- a stat that failed, a path
  ;; gone, a shorter file -- the cut length is known, as a version with no
  ;; identity and that size, so the guard still judges a shorter file
  ;; against it, and the next publication replaces it.
  ;; NEVER: THIS IS NOT A DEFENCE AGAINST A FILE PUT AT THE PATH DURING A
  ;; WRITE. The cut acts on a descriptor and everything after it reaches the
  ;; segment by path again, and the store's lock excludes no rename; that
  ;; is a limit of the whole write path, not of this record.
  ;; It says which it knew, (trace repair-noted version|cut-length <length>).
  (define (note-own-repair! store writer path cut-length)
    (let* ((now (segment-version-now path))
           (use-now? (and (segment-version-readable? now)
                          (not (segment-shrank? (list #f #f #f #f #f #f cut-length) now)))))
      (trace-event! 'repair-noted (if use-now? 'version 'cut-length) cut-length)
      (know-segment! writer path (if use-now? now (list #f #f #f #f #f #f cut-length)) #f)))
  (define (segment-replaced store writer)
    (or (let ((latched (assoc writer replaced-writers))) (and latched (cdr latched)))
        (let* ((k (assoc writer known-segments))
               (refusal (and k
                             (segment-shrank? (cddr k) (segment-version-now (cadr k)))
                             (list 'store-replaced (list 'segment (cadr k))))))
          (when refusal
            (set! replaced-writers (cons (cons writer refusal) replaced-writers)))
          refusal)))

  ;; ---- the change stream: subscribers, frames, the ring ---------------------
  ;;
  ;; ALL OF THIS IS THE STORE PROCESS'S: it publishes, so it frames, and it
  ;; registers subscribers, so the ring and the list are only ever touched
  ;; in one process.
  ;; The live stream: while `subscribers` is not empty every publication
  ;; makes a frame, kept in a ring of the last 256 by revision. When the
  ;; last subscriber leaves the ring stays and framing stops; a later
  ;; publication is a hole in it, which a resume across it is told.
  (define subscribers '())
  (define ring (make-eqv-hashtable))
  (define ring-revs '())
  (define ring-size 256)
  ;; The structural sets of the publication in the cell, while it was
  ;; framed: a frame then computes one reduction's sets, not two.
  (define published-sets #f)

  ;; THE FRAME'S LIBRARY IS LOADED WHEN A SUBSCRIPTION IS ACCEPTED, not at
  ;; start: (theourgia stream-frames) is all of the frame computation, and a
  ;; daemon nobody follows never needs it. Asked by name, once.
  (define frame-procedures #f)
  (define (frames-loaded!)
    (unless frame-procedures
      (trace-event! 'library-loaded 'stream-frames #f)
      (let ((env (environment '(theourgia stream-frames))))
        (set! frame-procedures
              (cons (eval 'structural-sets env) (eval 'change-items env))))))
  (define (structural-sets r) ((car frame-procedures) r))
  (define (change-items old new old-sets new-sets) ((cdr frame-procedures) old new old-sets new-sets))

  ;; (frame-time <ms>): the sets and the items of one frame, timed.
  (define (frame! old new seq)
    (let* ((started (real-time))
           (old-sets (or published-sets (structural-sets old)))
           (new-sets (structural-sets new))
           (items (change-items old new old-sets new-sets))
           (_ (trace-event! 'frame-time (- (real-time) started) #f))
           (clause (incomplete-clause (unreadable-behind new)))
           (datum (append (list 'changes (list 'rev seq) (list 'daemon daemon-token)
                                (list 'from-cut (reduce-applied-cut old))
                                (list 'cut (reduce-applied-cut new))
                                (cons 'items items))
                          (if clause (list clause) '())))
           (bytes (string->utf8 (render-wire datum))))
      (set! published-sets new-sets)
      (hashtable-set! ring seq bytes)
      (set! ring-revs (append ring-revs (list seq)))
      (when (> (length ring-revs) ring-size)
        (hashtable-delete! ring (car ring-revs))
        (set! ring-revs (cdr ring-revs)))
      (for-each (lambda (pid) (send pid (list 'frame seq bytes))) subscribers)))

  ;; A NOTICE THAT IS NOT A PUBLICATION -- a reload that failed -- goes to
  ;; every subscriber, with no revision, and is not kept.
  (define (stream-notice! datum)
    (unless (null? subscribers)
      (let ((bytes (string->utf8 (render-wire datum))))
        (for-each (lambda (pid) (send pid (list 'stream-notice bytes))) subscribers))))

  ;; A REVISION IS WRITTEN IN DECIMAL DIGITS AND NOTHING ELSE: string->number
  ;; would also take "#x10" or "1e2", which name numbers no frame carries.
  (define (decimal-digits->number text)
    (and (string? text)
         (> (string-length text) 0)
         (for-all (lambda (c) (char<=? #\0 c #\9)) (string->list text))
         (string->number text 10)))

  (define (drop-subscriber! pid)
    (when (memq pid subscribers)
      (set! subscribers (remp (lambda (p) (eq? p pid)) subscribers))
      (trace-event! 'stream-unregistered #f #f)
      (timer-follow!)))

  ;; ---- outside changes: the timer, while the stream is live ----------------
  ;;
  ;; A COMMIT MADE OUTSIDE THE DAEMON IS SEEN BY A PROBE: while at least one
  ;; subscriber is registered, a timer actor asks the store process to probe
  ;; once a second, and the store compares the store's snapshot with the
  ;; published one (refresh-if-behind!: a reload on a difference, a snapshot
  ;; it cannot take counted as behind). The timer never folds or publishes;
  ;; the store process does both, so a probe never interleaves with its own
  ;; commits. With nobody subscribed there is no timer and no probe.
  ;; THE SCHEDULE IS ABSOLUTE: each due time is the previous one plus 1000 ms,
  ;; so a slow probe does not push the next. AT MOST ONE PROBE IS OUTSTANDING:
  ;; a tick whose predecessor is not yet acknowledged is skipped, not queued
  ;; (a mailbox has no capacity check), and traced as probe-skipped.
  (define probe-ms 1000)
  (define prober #f)
  ;; THEOURGIA_PROBE=off (test-only, an injection build's): no prober.
  (define (timer-follow!)
    (cond
      ((and (pair? subscribers) (not prober) (not (theourgia-probe-off?)))
       (let ((store-pid self))
         (set! prober (spawn (lambda () (probe-timer store-pid))))))
      ((and (null? subscribers) prober)
       (send prober '(stop))
       (set! prober #f))
      (else (void))))
  (define (probe-timer store-pid)
    (let loop ((due (+ (real-time) probe-ms)) (outstanding #f))
      (receive
        (after (max 0 (- due (real-time)))
          (if outstanding
              (begin
                (trace-event! 'probe-skipped (wall-clock-ms) #f)
                (loop (+ due probe-ms) #t))
              (begin
                (send store-pid (list 'probe self))
                (loop (+ due probe-ms) #t))))
        (`(probe-done) (loop due #f))
        (`(stop) (void)))))

  ;; `subscribe changes <rev> [<token>]`, judged where the ring and the
  ;; current revision are. -> (acceptance . replay), the replay a list of
  ;; (revision . bytes) the stream writes before any live frame, or
  ;; (refusal . ()).
  ;;   - rev a non-negative integer, else invalid-revision;
  ;;   - a resume naming another incarnation's token: daemon-restarted,
  ;;     whatever its revision (judged first, see below);
  ;;   - past current, invalid-revision;
  ;;   - 0 is the initial subscription: no token, nothing replayed, the
  ;;     subscriber reads the state at current for itself;
  ;;   - a resume (rev > 0) needs a token, else missing-token, and every
  ;;     revision after rev up to current in the ring, else window.
  (define (subscription-answer from args)
    (let* ((current publish-seq)
           (rev (and (<= 2 (length args)) (decimal-digits->number (cadr args))))
           (token (and (= 3 (length args)) (caddr args)))
           (unavailable (lambda (reason . more)
                          (append (list 'error 'changes-unavailable (list 'reason reason))
                                  more
                                  (list (list 'current current) (list 'daemon daemon-token))))))
      (cond
        ;; A TRANSPORT OPTION IS REFUSED FIRST, BY NAME. A subscription
        ;; is carried past rpc-dispatch, whose refusal every other verb meets,
        ;; so without this `--wire`, `--help` or `-h` after the revision was
        ;; taken as the resume token. These are the request's RAW arguments
        ;; -- the connection already turned away an argument list that does
        ;; not parse (request-shape-error) -- so `--` still makes what follows
        ;; text, as the dispatcher reads it. Asked here and not in
        ;; subscribe-shape-error: the dispatched handler gives that one the
        ;; positionals, with `--` already removed.
        ((let ((nodes (parse-arguments 'subscribe args)))
           (and (list? nodes) (not (and (pair? nodes) (eq? (car nodes) 'error)))
                (transport-option-refusal nodes)))
         => (lambda (refusal) (cons refusal '())))
        ((subscribe-shape-error args) => (lambda (refusal) (cons refusal '())))
        ((not rev) (cons '(error bad-request invalid-revision) '()))
        ;; ANOTHER INCARNATION'S TOKEN IS JUDGED BEFORE ITS REVISION: a
        ;; revision of an earlier daemon does not compare with this one's
        ;; current, and the answer that lets the client recover is that the
        ;; daemon restarted, not that the number is too large.
        ((and (> rev 0) token (not (equal? token daemon-token)))
         (cons (unavailable 'daemon-restarted) '()))
        ((> rev current) (cons '(error bad-request invalid-revision) '()))
        ((and (> rev 0) (not token)) (cons '(error bad-request missing-token) '()))
        ((and (> rev 0)
              (let missing? ((r (+ rev 1)))
                (and (<= r current) (or (not (hashtable-ref ring r #f)) (missing? (+ r 1))))))
         (cons (unavailable 'window (list 'oldest (if (null? ring-revs) (+ current 1) (car ring-revs)))) '()))
        (else
         (frames-loaded!)
         (monitor from)
         (set! subscribers (cons from subscribers))
         (timer-follow!)
         (let ((clause (incomplete-clause (unreadable-behind (published-state)))))
           (cons (append (list 'ok (list 'subscribed rev) (list 'daemon daemon-token) (list 'current current))
                         (if clause (list clause) '()))
                 (let collect ((r (+ rev 1)) (out '()))
                   (if (or (= rev 0) (> r current))
                       (reverse out)
                       (collect (+ r 1) (cons (cons r (hashtable-ref ring r #f)) out))))))))))

  ;; FOLD AGAIN AND PUBLISH WHAT CAME BACK, with the snapshot sampled before
  ;; the fold (see publish!). The one body of `(reload)`, and of every other
  ;; refold this process makes.
  ;; NOTE: THE HOLD SEAM `reload-before-publish` sits between the fold and
  ;; the publication, where the order above matters: a row commits from
  ;; another process while it is held.
  (define (refold-and-publish! store)
    (let* ((snapshot (store-state-snapshot store))
           (state (obtain-state store #f incomplete-accepted)))
      (hold-point! 'reload-before-publish)
      (publish! store state snapshot)))

  ;; Somebody outside changed the store. Fold it again, under the lock, and
  ;; publish what came back.
  ;; A FOLD THAT FAILS keeps the previous publication and says so by
  ;; name: the table's answer for the condition, or its text.
  ;; `reload-raise` makes it fail.
  (define (reload-or-say-why! store)
    (guard (e (#t (let ((why (or (classify-failure e '()) (condition-text e))))
                    (trace-event! 'reload-failed why #f)
                    (stream-notice! (list 'error 'store-unreadable (list 'reason why))))))
      (when (eq? (theourgia-fault) 'reload-raise)
        (raise (make-message-condition "injected reload raise")))
      (refold-and-publish! store)))

  ;; A READ AT A CUT NAMES EVENTS THE PUBLICATION MAY NOT HAVE FOLDED YET: a
  ;; cut taken from a `log` answered a moment ago can name a commit another
  ;; process made since the last fold. Judged against that publication it
  ;; would be refused `not-received` for an event that is on the disk. So
  ;; the store process -- the one that folds -- compares the published
  ;; snapshot with the disk and, when they differ, folds and publishes
  ;; before it answers. Reads at the connection are untouched: they never
  ;; wait for a fold. A fold that fails keeps the previous publication, as
  ;; `(reload)` does, and the cut is judged against it.
  ;; NOTE: THE SNAPSHOT TAKEN HERE IS ONLY COMPARED: the publication keeps
  ;; the one its own fold sampled (see publish!).
  ;; A SNAPSHOT THAT CANNOT BE TAKEN COUNTS AS BEHIND, WHATEVER RAISED, and
  ;; the catch-all below is that rule: the question is only whether the
  ;; publication is still current, and the safe answer to a doubt about it
  ;; is to fold. The fold reads the store under its lock, and it is what
  ;; names an unreadable writer -- in the answer's notes, or as
  ;; reload-failed when it cannot complete. A failure that has passed by the
  ;; time the fold reads leaves no trace here: the fold simply succeeds, and
  ;; what it read is what is answered.
  (define snapshot-fault-pending #t)
  (define (refresh-if-behind! store)
    (let ((now (guard (e (#t #f))
                 ;; INJECTION ONLY: one snapshot that cannot be taken.
                 (when (and snapshot-fault-pending (eq? (theourgia-fault) 'refresh-snapshot-raise))
                   (set! snapshot-fault-pending #f)
                   (trace-event! 'fault 'refresh-snapshot-raise #f)
                   (raise (make-message-condition "injected snapshot raise")))
                 (store-state-snapshot store))))
      (unless (and now (equal? now (published-snapshot)))
        (reload-or-say-why! store))))

  ;; CELL, when given, is the vector its caller already read: a reader that
  ;; needs the state and its name reads the cell once (answer-published).
  (define (published-state . cell)
    (let ((c (if (pair? cell) (car cell) published))) (and c (vector-ref c 0))))
  (define (published-snapshot) (and published (vector-ref published 2)))

  ;; NEVER: THE READ IS ANSWERED FROM WHAT IS PUBLISHED, WHATEVER THIS FINDS.
  ;; Noticing that somebody else has committed is a reason to ask the
  ;; store for a fresh fold, NEVER: not a reason to make this caller wait for
  ;; one: a read that reloaded before answering would take the store's
  ;; lock on the read path and hand back the delay this whole arrangement
  ;; exists to remove. Being one commit behind is the documented
  ;; behaviour; being slow is not.
  ;;
  ;; WHAT IT FOUND IS HANDED TO THE READ IT RUNS FOR. A changed
  ;; snapshot's unreadable markers go, as notes, to the answer about to be
  ;; given from the old publication (ruling Q-r2-1 (b)): a declared read
  ;; says a writer has gone unreadable, an undeclared consumer refuses, and
  ;; nobody waits for the fold. The markers are an EARLY NOTICE, not a
  ;; completeness check -- the snapshot opens and seeks only the current
  ;; segment and reads no bytes -- and an answer from a complete publication
  ;; without one is one commit behind, as documented. -> the notes, '() when
  ;; nothing changed.
  ;; A PROBE THAT FAILS keeps the previous snapshot, as it always did, and
  ;; now says so by name (ruling 4); `probe-raise` makes it fail.
  (define (probe-for-outside-change! store store-pid)
    (let ((was (published-snapshot)))
      (if (not was)
          '()
          (let ((now (guard (e (#t (trace-event! 'probe-failed (condition-text e) #f) was))
                       (when (eq? (theourgia-fault) 'probe-raise)
                         (raise (make-message-condition "injected probe raise")))
                       (store-state-snapshot store))))
            (if (equal? was now)
                '()
                ;; THE MARK IS READ AT THE DECISION, after the snapshot and
                ;; the notes, with no yield between the read and the send.
                ;; While the store process is busy the request is deferred,
                ;; not lost: a commit in progress publishes a snapshot taken
                ;; after its own records, and a fold whose snapshot predates
                ;; a change leaves the next probe to find it (see
                ;; publication-pending).
                (let ((notes (snapshot-unreadable-notes now)))
                  (when store-pid
                    (if publication-pending
                        (trace-event! 'reload-deferred 'publication-pending #f)
                        (send store-pid (list 'reload))))
                  notes))))))

  ;; ONE WAY TO ANSWER FROM THE PUBLISHED STATE (found in design review):
  ;; probe, then hand the publication SEALED with what the probe found. The
  ;; connection's local reads and the writer processes both come here; the
  ;; census row pins (published-state) to this one reader.
  ;; THE CELL IS READ ONCE: the state and its name come from one vector, so
  ;; the name sealed with a state is that state's.
  (define (answer-published store store-pid parsed actor writer piped cwd)
    (let* ((notes (probe-for-outside-change! store store-pid))
           (cell published)
           (state (published-state cell)))
      (answer-for store parsed actor (and state (seal-state state notes (vector-ref cell 3))) writer piped cwd)))

  ;; ---- the store process -------------------------------------------------
  ;;
  ;; NEVER: THE ONLY PROCESS THAT OPENS THE STORE. `open-and-reduce` takes the
  ;; store's flock, and a lock taken inside an actor blocks the whole
  ;; scheduler thread -- so if two processes could take it, one of them
  ;; parked behind the other would freeze everything, including the
  ;; connection that was going to release it.
  ;; NEVER: THE FIRST PUBLICATION HAPPENS BEFORE ANYBODY CAN ASK FOR
  ;; ANYTHING. Main does not open the socket until this has reported
  ;; `ready`, so there is no such thing as a read arriving with nothing
  ;; published -- and therefore no "no value yet" case for a reader to
  ;; get wrong.
  ;;
  ;; NOTE: AND A BOOT THAT CANNOT TAKE THE LOCK SAYS SO IN ITS OWN WORDS.
  ;; Somebody else holding the store for longer than the budget is an
  ;; ordinary state of the world; `store-busy` is what a client would be
  ;; told, so it is what the daemon prints before it goes, rather than
  ;; leaving main to report the death of a process and nothing about why.
  ;; NEVER: "THERE IS NO STORE HERE" IS ITS OWN ANSWER, AND IT IS THE COMMON
  ;; ONE. A daemon asked to serve a path that holds no store failed while
  ;; opening it and reported
  ;;
  ;;   (error store-load-failed (reason raised))
  ;;
  ;; -- `raised` because the condition carried no message, so the field
  ;; meant to say why said only that something had. The local path has
  ;; always answered `(error no-store <path>)` for this, from `no-store?`
  ;; in `rpc.sc`; the two routes disagreed about the most ordinary
  ;; failure there is, and the daemon's version told nobody anything.
  ;; THROUGH THE DOOR: absent is "no store here", as before; a
  ;; meta.sexp whose directory cannot be searched raises instead of
  ;; reading as a missing store. store-here? is (theourgia client)'s, the
  ;; one predicate the client and theourgiad ask before a start as well.

  (define (store-loop store main-pid)
    ;; INJECTION ONLY (item 7): the store held at its entry, before
    ;; store-here?, so a signal can arrive while main is STARTING (the
    ;; startup-exit design R6).
    (hold-point! 'store-start)
    ;; every write this process makes asks it first (see segment-replaced)
    (log-append-guard! segment-replaced note-own-repair!)
    ;; A RAISE BEFORE ANY REPORT (D15), armed by
    ;; `THEOURGIA_FAULT=store-raise-early@report`: a plain condition, outside
    ;; the scope below, so no startup-failed message is sent and main
    ;; answers only store-actor-down. Unarmed it is one comparison.
    (when (eq? (theourgia-fault) 'store-raise-early)
      (raise (make-message-condition "injected store raise early")))
    ;; THE STARTUP IS ONE SCOPE: whether there is a store
    ;; here and its first load. A failure is answered by the table over this
    ;; scope's record, or kept as the named outcome it already is, and SENT
    ;; to main before this process raises -- main combines it with its own
    ;; record and writes the report (aggregation (a)). The scope closes
    ;; before the request loop, so each request opens its own.
    ;;
    ;; NEVER: "THERE IS NO STORE HERE" IS ITS OWN ANSWER, AND IT IS THE COMMON
    ;; ONE, `store-not-found`, as before. AN ENTRY THAT CANNOT BE READ IS
    ;; NAMED AT START-UP TOO: now by the table, with its errno.
    ;; `store-load-failed` stays for a condition the table does not
    ;; classify.
    (let ((failure (with-mutation-record
                     (lambda ()
                       (guard (e (#t (cons e (mutation-record))))
                         (unless (store-here? store)
                           (raise (list 'error 'store-not-found (list 'store store))))
                         ;; THE DAEMON DECLARES: it holds the state and answers
                         ;; nothing from it itself; every consumer of the
                         ;; publication judges it when it unseals.
                         (let ((snapshot (store-state-snapshot store)))
                           (publish! store (obtain-state store #f incomplete-accepted) snapshot))
                         #f)))))
      (when failure
        (let ((e (car failure)) (record (cdr failure)))
          (send main-pid
                (list 'startup-failed
                      (or (classify-failure e record)
                          (combine-report
                            (if (and (pair? e) (eq? 'error (car e)))
                                e
                                (list 'error 'store-load-failed (list 'reason (condition-text e))))
                            record))
                      record))
          (raise e))))
    (send main-pid (list 'ready))
    (let loop ()
      (receive
        ;; Somebody outside changed the store. Fold it again, under the
        ;; lock, and publish what came back.
        ;; A FOLD THAT FAILS keeps the previous publication and says so by
        ;; name (a ruling): the table's answer for the condition, or
        ;; its text. `reload-raise` makes it fail.
        (`(reload)
         (busy! (lambda () (reload-or-say-why! store)))
         (loop))
        (`(request ,from ,seq ,ticket ,main-pid ,parsed ,actor ,writer ,piped ,cwd)
         (busy!
          (lambda ()
            (with-ticket ticket
             (lambda ()
              ;; NEVER: AND A WAY TO MAKE THIS ONE DIE TOO. The store process is
              ;; the daemon: without it nothing can be answered, so main is
              ;; supposed to say so and leave with 75 rather than sit there
              ;; accepting connections it cannot serve. That rule needs a row,
              ;; and a row needs a seam. Armed by
              ;; `THEOURGIA_FAULT=store-raise@conn`; unarmed it is one
              ;; comparison against #f.
              (when (eq? (theourgia-fault) 'store-raise)
                (raise (condition (make-message-condition "injected store raise")
                                  (make-irritants-condition (list seq)))))
              ;; NEVER: #f, NOT THE PUBLISHED VALUE. This process is the one
              ;; that writes, and a write decides what to do from the
              ;; store as it stands under the lock -- answering from a
              ;; fold made before the lock was taken is how two writers
              ;; come to disagree about what was there.
              (if (executing? main-pid ticket)
                  (let ((answer (if (or (cut-read? parsed) (published-read? parsed))
                                    ;; A READ AT A CUT IS JUDGED AGAINST THE
                                    ;; PUBLICATION, made current first when the
                                    ;; disk has moved past it, and reached the one
                                    ;; way the published state is read; so is a
                                    ;; published read (query, context).
                                    (begin
                                      (refresh-if-behind! store)
                                      ;; THE KEPT SESSION'S PROVIDER, installed at the first published read
                                      (when (published-read? parsed) (install-session-provider!))
                                      (answer-published store self parsed actor writer piped cwd))
                                    (answer-for store parsed actor #f writer piped cwd))))
                    ;; A PUBLISHED READ TOLD THE KEPT SESSION'S WORK: the served line,
                    ;; after the verb has answered
                    (when (published-read? parsed) (session-served! (car parsed)))
                    ;; A DERIVED-TABLE WRITE moves the generation the kept session is judged by
                    (when (and (pair? parsed) (memq (car parsed) '(supply clear-derived)))
                      (set! derived-generation (+ derived-generation 1)))
                    (executed! main-pid ticket)
                    (send from (list 'answer seq answer 'core)))
                  (send from (list 'answer seq draining-answer 'transport)))))))
         (loop))
        ;; A SUBSCRIPTION IS REGISTERED HERE, where the ring and the current
        ;; revision are, and its admission ticket is finished at once: a
        ;; stream lives as long as its connection, and holding the ticket
        ;; that long would hold a drain open for it.
        (`(subscribe ,from ,seq ,ticket ,main-pid ,args)
         (busy!
          (lambda ()
            (if (may-execute? main-pid ticket)
                (let ((answer (subscription-answer from args)))
                  (executed! main-pid ticket)
                  (send from (list 'subscription seq (car answer) (cdr answer))))
                (send from (list 'subscription seq draining-answer '())))))
         (loop))
        (`(unsubscribe ,pid) (busy! (lambda () (drop-subscriber! pid))) (loop))
        ;; THE TIMER'S PROBE, traced when it arrives, answered by the same
        ;; comparison a read at a cut makes, then acknowledged.
        (`(probe ,from)
         (busy!
          (lambda ()
            (trace-event! 'probe (wall-clock-ms) #f)
            (refresh-if-behind! store)
            ;; INJECTION ONLY: a slow probe, and a probe held before its
            ;; acknowledgement.
            (when (eq? (theourgia-fault) 'probe-slow) (sleep-ms 300))
            (hold-point! 'probe-before-ack)
            (send from '(probe-done))))
         (loop))
        ;; THE STREAM'S LAST WORD BEFORE A DRAIN ENDS IT: unregistered here,
        ;; and answered after every frame this process already sent it, since
        ;; it handles messages in order.
        (`(stream-end ,pid) (busy! (lambda () (drop-subscriber! pid) (send pid (list 'stream-end)))) (loop))
        (`#(DOWN ,who ,reason) (busy! (lambda () (drop-subscriber! who))) (loop))
        (`(drain) (busy! (lambda () (void))) (loop)))))

  ;; ---- a writer's process -----------------------------------------------
  ;;
  ;; NEVER: ONE PER WRITER, AND ITS DEATH IS NOT THE DAEMON'S. The store
  ;; process is the serialisation point for everything that publishes,
  ;; and losing it means nothing can be answered at all -- so main leaves
  ;; when it dies. A writer's working directory is that writer's alone,
  ;; so a writer process that dies costs exactly that writer's request:
  ;; main closes whatever descriptors it had open for it, forgets it, and
  ;; the next request for that name gets a new process.
  ;;
  ;; NOTE: IT ANSWERS THROUGH THE SAME DISPATCHER AS EVERYTHING ELSE. No
  ;; verb, no answer and no error shape exists on this path that does not
  ;; exist on the other one; what differs is which mailbox serialises it.
  ;; NEVER: A WAY TO DIE HOLDING A LOCK, because that is the one failure the
  ;; table in main exists for and a rule with no row behind it is tested
  ;; for the first time by the first real crash. The seam takes a REAL
  ;; lock through the real strategy -- so main really has registered a
  ;; real descriptor -- and then dies without releasing it, which is what
  ;; a killed actor does: NEVER: no unwind runs, and the descriptor does not
  ;; go away with the process that was using it.
  ;;
  ;; NOTE: IT FIRES ONCE. The row's point is that the NEXT request for the
  ;; same writer is served; a fault that fired on every request would
  ;; measure nothing but the fault.
  ;;
  ;; NOTE: Armed only by `THEOURGIA_FAULT=writer-raise@conn`, which the ffi
  ;; refuses at startup if it is misspelled; unarmed, this is one
  ;; comparison against #f.
  ;; NOTE: AND A SECOND ONE, FOR THE OTHER HALF OF THE RULE. `writer-raise`
  ;; dies HOLDING a descriptor main opened; `writer-raise-late` dies
  ;; after the answer has gone out, holding nothing -- main's table no
  ;; longer names it, and tidying up after it must therefore close
  ;; nothing. The two are different faults because they are different
  ;; claims, and one row cannot be evidence for both.
  ;; NOTE: AND A THIRD, WHICH ARMS A SITUATION RATHER THAN AN EVENT.
  ;; `writer-hold` is one fault because the thing it has to produce is
  ;; one interleaving that no single event describes: ONE writer parked
  ;; inside a lock main opened for it, WHILE ANOTHER writer dies and is
  ;; tidied up. A `THEOURGIA_FAULT` names one fault, so the two halves
  ;; are two clauses of it -- the first writer to arrive parks, and the
  ;; next request from a DIFFERENT writer answers and then dies.
  ;;
  ;; NEVER: AND IT PARKS IN THE DRAFT LOCK, not the store lock. The second
  ;; writer's request has to be served WHILE the first is parked, and it
  ;; needs the store lock to be served -- so a hold on the store lock
  ;; would serialise the two and there would be no interleaving left to
  ;; test. A writer's draft lock is that writer's alone.
  (define writer-fault-pending #t)
  (define writer-hold-name #f)

  (define conn-hold-pending #t)

  (define (park-in-draft-lock store name hold-ms)
    (let ((path (draft-lock-path store name)))
      (mkdir-p! (directory-of path))
      (file-ensure! path)
      ;; NOTE: LONG ENOUGH FOR THE OTHER WRITER TO BE SERVED, TO DIE, AND
      ;; FOR MAIN TO TIDY UP AFTER IT -- and for the row to take its
      ;; reading before this one wakes. A park of a few hundred
      ;; milliseconds is over before that sequence has finished, and the
      ;; row would then be measuring an unparked daemon.
      (let ((l ((current-lock-acquire) path 'exclusive)))
        (sleep-ms hold-ms)
        ((current-lock-release) l))))

  (define (writer-loop store name store-pid)
    (let loop ((served 0))
      (receive
        (`(request ,from ,seq ,ticket ,main-pid ,parsed ,actor ,writer ,piped ,cwd)
         ;; INJECTION ONLY: this writer dies on its SECOND request, so a
         ;; connection that already used it can be watching it when it goes
         ;; (a subscribed connection's writer DOWN).
         (when (and (= served 1) (eq? (theourgia-fault) 'writer-raise-second))
           (raise (condition (make-message-condition "injected writer raise on a second request")
                             (make-irritants-condition (list name seq)))))
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-raise))
           (set! writer-fault-pending #f)
           ((current-lock-acquire) (string-append store "/lock") 'exclusive)
           (raise (condition (make-message-condition "injected writer raise")
                             (make-irritants-condition (list name seq)))))
         ;; NEVER: THE PUBLISHED VALUE, NOT A FRESH FOLD. A note verb reads
         ;; the library to decide what a draft is based on, and doing
         ;; that by opening the log would take the store's lock on a
         ;; path that is supposed to be able to run beside a commit.
         ;; Being one commit behind is the documented behaviour here,
         ;; and the consequences of acting on a stale premise are caught
         ;; where they have to be caught -- by the commit, under the lock.
         (with-ticket ticket
          (lambda ()
           (if (executing? main-pid ticket)
               (begin
                 ;; NEVER: THE PARK IS INSIDE THE EXECUTED REGION, AFTER MAIN HAS
                 ;; SAID YES. NOTE: Measured with it OUTSIDE: a request that
                 ;; parked before asking had, from main's point of view,
                 ;; never begun -- so a drain that started during the park
                 ;; refused it, and the row that wanted "what has begun
                 ;; finishes" read `(error draining)` for the very request
                 ;; it was about. KEY: `may-execute?` returning IS what "begun"
                 ;; means here; whatever a process does before asking is
                 ;; invisible to the only process that decides.
                 ;;
                 ;; NOTE: TWO LENGTHS, BECAUSE TWO ROWS NEED OPPOSITE THINGS:
                 ;; the short park has to end while another writer is being
                 ;; tidied up; the long one has to outlast the whole drain
                 ;; budget, so the only way out is the clock.
                 (when (and (memq (theourgia-fault) '(writer-hold writer-hold-long))
                            (not writer-hold-name))
                   (set! writer-hold-name name)
                   (park-in-draft-lock store name
                                       (if (eq? (theourgia-fault) 'writer-hold-long) 12000 1500)))
                 (let ((answer (answer-published store store-pid parsed actor writer piped cwd)))
                   (executed! main-pid ticket)
                   (send from (list 'answer seq answer 'core))))
               (send from (list 'answer seq draining-answer 'transport)))))
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-raise-late))
           (set! writer-fault-pending #f)
           (raise (condition (make-message-condition "injected writer raise late")
                             (make-irritants-condition (list name seq)))))
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-hold)
                    writer-hold-name (not (equal? name writer-hold-name)))
           (set! writer-fault-pending #f)
           (raise (condition (make-message-condition "injected writer raise beside a holder")
                             (make-irritants-condition (list name seq)))))
         (loop (+ served 1))))))

  ;; ---- the listener ------------------------------------------------------
  ;;
  ;; NOTE: IT DOES NOT START READING. The connection is handed to a conn
  ;; process, and that process makes itself the target -- so the bytes go
  ;; to whoever serves them and never through here.
  (define (listener-loop socket store-pid store main-pid)
    (let ((me self)
          ;; NOTE: THE HANDLE IS KEPT, because stopping needs it: `listen!`
          ;; answers a reference and `stop-listen!` is about that
          ;; reference, not about the path -- the path may by then be
          ;; somebody else's socket.
          (private (private-socket-path socket (process-id)))
          (target (public-link-target socket (process-id)))
          ;; THE WATCHES ARE NUMBERED from the daemon's start; an acknowledgement
          ;; names the highest one sent, so main can tell whether a connection
          ;; arrived after its watermark.
          (seq 0)
          ;; THE ADAPTERS SPAWNED AND NOT YET ACCOUNTED FOR: each is monitored
          ;; when its spawn is announced, before it can report, and leaves this
          ;; list when it reports `accepted` or dies.
          (pending '()))
     ;; A RESUMED LISTENER'S ADAPTERS HOLD AT A STAGE OF THEIR OWN, so a row
     ;; can place a connection on the rebound socket after the late one that
     ;; cancelled the exit has passed the first stage.
     (define (open! stage)
       (listen! private 64 adapter-idle-ms (theourgia-send-buffer) #t
                (lambda () (adapter-before-report! stage))))
     (define (spawned! a) (monitor a) (set! pending (cons a pending)))
     (define (accepted! ref)
       (set! pending (remq (conn-ref-pid ref) pending))
       ;; NOTE: MAIN IS TOLD ABOUT EVERY CONN IT WILL HAVE TO EXPLAIN.
       (let ((conn (spawn (lambda () (conn-loop ref store-pid store main-pid)))))
         (set! seq (+ seq 1))
         (send main-pid (list 'watch conn 'conn seq))))
     (define (gone! who) (set! pending (remq who pending)))
     (let ((lref (begin
                  ;; INJECTION ONLY (item 7): held before the bind (P6-timeout).
                  (hold-point! 'bind)
                  ;; The send buffer is a fixture's seam, #f outside the
                  ;; injection build: the system's own.
                  ;; BOUND AT THE PRIVATE NAME, through the namespace link.
                  (let ((bound (open! 'adapter-before-report)))
                    ;; INJECTION ONLY: the first step of accept-exhaust-first.
                    (arm-accept-fault! 'bind)
                    bound))))
      ;; THE PUBLIC LINK IS PUBLISHED AFTER THE BIND, so a client that finds it
      ;; finds a socket behind it. symlink(2) never replaces: an entry made at
      ;; the public path since the startup cleared it is left there, the
      ;; private socket is unlinked again, and the listener dies, which main
      ;; reports as listener-down.
      ;; The refusal is traced here, so a reader of the trace can tell a
      ;; publish collision from a bind failure; both exit as listener-down.
      ;; A symlink! that raises (the directory not writable, say) is refused
      ;; the same way, its errno the reason, and the private socket is
      ;; unlinked through cleanup-step!, so a failure there is logged, and
      ;; only while it is still the socket bound. Its identity is read before
      ;; the publish, so a failure to read it leaves nothing published.
      (let* ((bound-ident (entry-identity private))
             (published (guard (e ((fs-error? e) (fs-error-errno e))) (symlink! target socket))))
        (unless (eq? published 'created)
          (trace-event! 'publish-refused `(path ,socket) `(reason ,published))
          (cleanup-step! private (lambda () (unlink-made! private bound-ident)))
          (raise `(error serve-path-occupied (path ,socket))))
        ;; BOUND IS THE PRIVATE FILE'S NAME, THE LINK'S TEXT AND BOTH
        ;; IDENTITIES: the exit unlinks each while it is still that entry.
        (send main-pid (list 'bound (list private target bound-ident (entry-identity socket)))))
      (let serving ((lref lref) (link-ident (entry-identity socket)))
        ;; STOPPED: no handle; every adapter accounted for. A resume binds and
        ;; publishes again; a stop asked of a stopped listener is acknowledged
        ;; again, so a drain that follows a failed resume is not left waiting.
        (define (stopped! base exhausted?)
          (let ((now (accept-failure-counts)))
            (send main-pid (list 'stopped (list 'handed seq)
                                 (list 'refused-accepts (- (car now) (car base)))
                                 (list 'callback-raised (- (cdr now) (cdr base)))
                                 (list 'exhausted exhausted?))))
          (let still ()
            (receive
              (`(resume)
               (if exhausted?
                   (begin (send main-pid (list 'resume-failed 'listener-exhausted)) (dead))
                   (resume!)))
              (`(stop) (stopped! (accept-failure-counts) exhausted?))
              (`(spawned ,a) (spawned! a) (still))
              (`(accepted ,ref) (accepted! ref) (still))
              (`#(DOWN ,who ,reason) (gone! who) (still)))))
        ;; NOTHING LEFT TO SERVE: a resume failed. Only a stop is answered.
        (define (dead)
          (receive
            (`(stop)
             (send main-pid (list 'stopped (list 'handed seq) '(refused-accepts 0) '(callback-raised 0)
                                  '(exhausted #t)))
             (dead))
            (`(spawned ,a) (dead))
            (`(accepted ,ref) (dead))
            (`#(DOWN ,who ,reason) (dead))))
        ;; THE STOP, THEN THE ACCOUNT: the handle is closed (libuv's close
        ;; unlinks the private file), every announcement already in the mailbox
        ;; is taken, and the acknowledgement waits until each adapter spawned has
        ;; reported or died. A listener the runtime had already retired is
        ;; listener-exhausted, which no resume can undo.
        (define (stop-and-account! lref base)
          (let ((exhausted? (not (guard (e (#t #f)) (listener-open? lref)))))
            (guard (e (#t (if #f #f))) (stop-listen! lref))
            (let drain-mailbox ()
              (receive
                (after 0 #f)
                (`(spawned ,a) (spawned! a) (drain-mailbox))
                (`(accepted ,ref) (accepted! ref) (drain-mailbox))
                (`#(DOWN ,who ,reason) (gone! who) (drain-mailbox))))
            (let account ()
              (if (null? pending)
                  (stopped! base exhausted?)
                  (receive
                    (`(spawned ,a) (spawned! a) (account))
                    (`(accepted ,ref) (accepted! ref) (account))
                    (`(stop) (account))
                    (`#(DOWN ,who ,reason) (gone! who) (account)))))))
        ;; A RESUME BINDS AND PUBLISHES AGAIN. The cell is written as soon as
        ;; listen! returns, before anything else runs here, so an exit from
        ;; now on unlinks what this resume bound; it is completed with the
        ;; link's identity after the publish. A public path occupied by an
        ;; entry that is not our link is left as it is. Each failure unlinks
        ;; what this resume bound and is reported by its cause.
        (define (resume!)
          (let ((lref (guard (e (#t (send main-pid (list 'resume-failed (list 'bind (errno-of e))))
                                    #f))
                        (open! 'resumed-adapter-before-report))))
            (if (not lref)
                (dead)
                (let ((ident (entry-identity private)))
                  (set! listener-cell (list private target ident #f))
                  (let ((published (guard (e (#t (errno-of e))) (symlink! target socket))))
                    (if (eq? published 'created)
                        (let ((link-ident (entry-identity socket)))
                          (set! listener-cell (list private target ident link-ident))
                          ;; INJECTION ONLY: held after the rebind and the
                          ;; publish, before main is told.
                          (hold-point! 'resume-before-bound)
                          (send main-pid (list 'bound listener-cell))
                          (serving lref link-ident))
                        (begin
                          (guard (e (#t (if #f #f))) (stop-listen! lref))
                          (cleanup-step! private (lambda () (unlink-made! private ident)))
                          (send main-pid (list 'resume-failed
                                               (if (eq? published 'exists)
                                                   'path-occupied
                                                   (list 'link published))))
                          (dead))))))))
        (let loop ()
          (receive
            ;; NEVER: THE DOOR CLOSES FIRST. Everything else about a drain is
            ;; about work already accepted; a listener still accepting
            ;; would keep making more of it. The drain then waits for this
            ;; listener's account of the adapters it spawned (rule (b)), so a
            ;; connection accepted before the stop is admitted and answered,
            ;; never left behind.
            (`(stop)
             ;; INJECTION ONLY: held on a stop, before the handle is closed.
             (hold-point! 'listener-before-stop)
             (stop-and-account! lref (accept-failure-counts)))
            (`(leave idle)
             (arm-accept-fault! 'leave)
             (let ((base (accept-failure-counts)))
               ;; INJECTION ONLY: held before the unlink; the blocking form
               ;; stops the whole VM, so libuv delivers nothing meanwhile.
               (hold-point! 'leave-before-unlink)
               (blocking-hold! 'leave-block-before-unlink)
               (let ((unlinked (guard (e (#t (list 'failed (errno-of e))))
                                 (unlink-made! socket link-ident))))
                 (if (and (pair? unlinked) (eq? (car unlinked) 'failed))
                     ;; A FAILED UNLINK CANCELS THE ATTEMPT: our link is still
                     ;; there, so the daemon keeps listening and main restarts
                     ;; its clock. Kept or gone go on: our link is not there.
                     (begin
                       (send main-pid (list 'idle-cancelled (list 'reason 'unlink-failed (cadr unlinked))))
                       (loop))
                     (begin
                       ;; THE GRACE: a connection the kernel completed before the
                       ;; unlink is delivered by libuv's callbacks while the
                       ;; handle stays open; one not delivered by the end of it is
                       ;; reset at the stop. A stop asked meanwhile waits for the
                       ;; grace's end, so it never resets more than the grace does.
                       ;; It is counted from the unlink.
                       (let ((deadline (+ (real-time) idle-grace-ms)))
                         ;; INJECTION ONLY: held after the unlink, before the
                         ;; grace's callbacks; the blocking form lets the grace
                         ;; pass with nothing delivered.
                         (hold-point! 'leave-after-unlink)
                         (blocking-hold! 'leave-block-after-unlink)
                         (let grace ()
                           (if (>= (real-time) deadline)
                               (stop-and-account! lref base)
                               (receive
                                 (after (max 0 (- deadline (real-time))) (grace))
                                 (`(spawned ,a) (spawned! a) (grace))
                                 (`(accepted ,ref) (accepted! ref) (grace))
                                 (`(stop) (grace))
                                 (`#(DOWN ,who ,reason) (gone! who) (grace)))))))))))
            (`(spawned ,a) (spawned! a) (loop))
            (`(accepted ,ref) (accepted! ref) (loop))
            (`#(DOWN ,who ,reason) (gone! who) (loop))))))))

  ;; HOW LONG AN ACCEPTED ADAPTER WAITS FOR ITS CONNECTION TO START READING.
  ;; A connection waits for main's admission before it reads, and one that
  ;; arrives during a leave waits for the attempt's decision: the rest of the
  ;; grace (at most 5000 ms) and the rebind. Fifteen seconds covers that with
  ;; room; the runtime's default, five, does not.
  (define adapter-idle-ms 15000)

  ;; INJECTION ONLY: an accepted adapter held before it reports, so a row can
  ;; place a connection between the kernel's accept and the listener's account.
  ;; With adapter-dies armed, every adapter after the first then dies, as an
  ;; adapter that never reports: the first connection is served, so a row has
  ;; a watermark above zero before the death it measures.
  (define adapter-births 0)
  (define (adapter-before-report! stage)
    (hold-point! stage)
    (set! adapter-births (+ adapter-births 1))
    (when (and (eq? (theourgia-fault) 'adapter-dies) (> adapter-births 1)) (kill self 'injected)))

  ;; INJECTION ONLY: THE RUNTIME'S ACCEPT FAILURES DURING A LEAVE, armed when the
  ;; leave begins. accept-refused-once makes igropyr's next uv_accept answer
  ;; negative (its refused-accept counter). accept-raise-once makes the next
  ;; accept raise before the connection is published: igropyr owns the
  ;; descriptor by then, so the raise costs that connection and not the
  ;; listener (its callback-raised counter). accept-raise-second retires the
  ;; listener on the second accept: the first accept's re-arm of the handle
  ;; reserve fails, so the second allocates inside the accept window, and
  ;; that allocation raises (igropyr's rung 3: callback-raised and exhausted). The arming library is the runtime's test side,
  ;; which no production unit may import, so it is reached by eval, and only
  ;; when one of these faults is armed; the process must run with
  ;; IGROPYR_INJECT=on for the runtime's points to exist. The environment holds
  ;; only what the forms use: they quote their symbols and one arms two points
  ;; in sequence, so `quote` and `begin` are in it beside the arming library
  ;; (without them the eval raised in the listener and the daemon left as
  ;; listener-down).
  ;; accept-exhaust-first retires the listener on the FIRST accept of a leave,
  ;; so no late connection is served: armed in two steps, AT is where. At the
  ;; first bind the reserve's re-arm is made to fail, so the first accept
  ;; after it (a request before the window) leaves the reserve empty; at the
  ;; leave the in-window allocation is made to raise (igropyr's rung 3). At
  ;; the bind it is the only fault that arms anything; each step once.
  (define accept-fault-armed '())
  (define (arm-accept-fault! at)
    (unless (memq at accept-fault-armed)
      (let ((form (case (theourgia-fault)
                    ((accept-refused-once) '(inject-arm-return! 'accept-refused -1 1))
                    ((accept-raise-once) '(inject-arm-fault! 'accept-before-publish 1))
                    ((accept-raise-second) '(begin (inject-arm-fault! 'accept-reserve-rearm 1)
                                                   (inject-arm-fault! 'accept-reserve-inwindow 1)))
                    ((accept-exhaust-first) (if (eq? at 'bind)
                                                '(inject-arm-fault! 'accept-reserve-rearm 1)
                                                '(inject-arm-fault! 'accept-reserve-inwindow 1)))
                    (else #f))))
        (when (and form (or (eq? at 'leave) (eq? (theourgia-fault) 'accept-exhaust-first)))
          (set! accept-fault-armed (cons at accept-fault-armed))
          (eval form (environment '(only (rnrs base) quote begin) '(igropyr inject-control)))))))

  ;; INJECTION ONLY: a hold that BLOCKS THE OS THREAD instead of yielding, so
  ;; no actor runs and libuv delivers nothing while it holds. The sleeper is
  ;; set for this one hold and put back after it; nothing else can run in
  ;; between, which is what makes the swap safe.
  (define (blocking-hold! stage)
    (hold-sleeper-set! (lambda (ms) (sleep (make-time 'time-duration (* ms 1000000) 0))))
    (hold-point! stage)
    (hold-sleeper-set! sleep-ms))

  ;; -> the errno a filesystem failure carries; for the runtime's own socket
  ;; failures, which raise with libuv's error text and no number (a failed
  ;; bind: `(error 'uv-pipe-bind "permission denied")`), that text; otherwise
  ;; unknown.
  (define (errno-of e)
    (cond ((fs-error? e) (fs-error-errno e))
          ((unreadable-entry? e) (unreadable-entry-errno e))
          ((and (condition? e) (message-condition? e)) (condition-message e))
          (else 'unknown)))

  ;; ---- routing -----------------------------------------------------------
  ;;
  ;; NEVER: ROUTING ASKS THE SAME PARSER THE VERB WILL ASK. A second reader of
  ;; the command line here would be a second supplier of one fact, and on
  ;; the day the two disagreed about which writer a request names, two
  ;; requests for one writer would run in two processes at once and its
  ;; working directory would have no serialisation left at all. So the
  ;; name comes from `parse-arguments`, the call `rpc-dispatch` itself
  ;; makes.
  ;;
  ;; NOTE: WHAT MAY LEAVE THE STORE PROCESS IS DECIDED BY WHAT IT TOUCHES,
  ;; not by whether it writes. These touch one writer's working directory
  ;; and nothing else. NEVER: `commit` is not among them: it publishes, so it
  ;; belongs on the serialised path with the reads.
  (define writer-local-verbs '(write restore discard drafts))

  ;; NEVER: THESE THREE ARE ANSWERED IN THE CONNECTION'S OWN PROCESS, from
  ;; the published value, without opening the log and without taking the
  ;; store's lock. They read and nothing else, so putting them through
  ;; the one process that serialises writing would make every reader wait
  ;; behind a commit for no reason at all.
  ;;
  ;; NEVER: AND THE LIST IS NOT WIDENED BY GUESSING. A verb answered from a
  ;; published fold that in fact touches something else would be doing it
  ;; outside the lock, with nothing to notice. Anything not named here
  ;; goes to the store process: slower, and right.
  ;;
  ;; NOTE: `read --working` IS NOT THIS `read`. It asks about a writer's
  ;; draft, which belongs to that writer's process, so it is excluded
  ;; here and picked up by `writer-for-request`.
  ;;
  ;; `describe` IS ONE OF THEM, and is the one whose answer depends on the
  ;; publication only through the store's template, read from the published
  ;; state it is handed (rpc.sc describe-template). Like the others it is a read
  ;; of the publication: the probe before it may notice an outside change and
  ;; start a reload, and it writes nothing. Sent to the store process it would
  ;; be handed no state and add nothing.
  ;;
  ;; NEVER: `query` AND `context` ARE NOT AMONG THEM, although each reads the
  ;; state only through reduction-for and the keyword hook. A connection
  ;; answers the publication as it stands while a reload the probe asked for
  ;; is still queued, and a consumer that writes outside the daemon and then
  ;; queries through it must see its write: those two are published reads of
  ;; the store process (published-read?, below).
  (define conn-local-read-verbs '(read outline search describe))

  (define (conn-local-read? request)
    (and (pair? request)
         (symbol? (car request))
         (memq (car request) conn-local-read-verbs)
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                (not (argument-option nodes "--working"))
                (not (argument-option nodes "--working-info"))
                ;; A READ AT A CUT REPLAYS THE LOG, which only the store
                ;; process may open.
                (not (argument-option nodes "--cut"))))))

;; A PUBLISHED READ: `query` and `context`, answered by the store process
  ;; from the publication made current first, as a read at a cut is: no replay
  ;; while the disk has not moved past the publication, one fold when it has.
  ;; Each reads the state only through reduction-for (the published object,
  ;; unsealed) and the store's keyword hook (test/daemon-session.sc's census
  ;; walks both files for the names that reach the store). Handed no state, as
  ;; they were before, every request replayed the store from its snapshot.
  ;; -> #t or #f
  (define published-read-verbs '(query context))
  (define (published-read? request)
    (and (pair? request)
         (memq (car request) published-read-verbs)
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                #t))))

  ;; A read, or an export, at a causal cut: both are judged against the
  ;; publication, so an export at a cut serves the state a read at that cut
  ;; would. -> #t or #f
  (define (cut-read? request)
    (and (pair? request)
         (memq (car request) '(read export-code export-md))
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                (argument-option nodes "--cut")
                #t))))

  ;; NEVER: THE ENVELOPE'S WRITER NAMES A WRITER TOO, and it is asked here,
  ;; inside the same gate. While the writer was spliced into the
  ;; arguments this found it there; once it travelled beside the actor
  ;; instead, a draft verb carrying only an envelope writer found no name
  ;; and went to the store process, so one writer's work ran in two
  ;; different processes depending on how the writer had been named and
  ;; nothing serialised the two against each other.
  ;; NOTE: THE DEFAULT IS READ HERE AND NOT AT THE CALLER: the verb gate
  ;; below is what keeps a reader out of the writer's mailbox, and a
  ;; default applied outside this procedure would be a default that
  ;; skipped it -- every verb from a client that names a writer would
  ;; queue behind that writer's commits for no reason.
  (define (writer-for-request request default-writer)
    (and (pair? request)
         (symbol? (car request))
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                (let ((name (or (argument-option nodes "--writer") default-writer)))
                  (and (string? name)
                       (or (memq (car request) writer-local-verbs)
                           ;; A READ AT A CUT GOES TO THE STORE PROCESS whatever
                           ;; else it carries: an option that asks about a
                           ;; writer's draft is refused there, in one place,
                           ;; and never inside a writer process.
                           (and (eq? (car request) 'read)
                                (not (argument-option nodes "--cut"))
                                (or (argument-option nodes "--working")
                                    (argument-option nodes "--working-info"))))
                       name))))))

  ;; The part of a connection that does not change, in one value. A
  ;; connection has things to remember now -- which writer processes it
  ;; has been given -- and threading each new one through three
  ;; procedures as another positional argument is how a call and a
  ;; definition drift apart.
  (define (make-ctx ref store-pid store main-pid)
    (vector ref store-pid store main-pid (vector '()) (vector #f #f)))
  (define (ctx-ref c) (vector-ref c 0))
  (define (ctx-store-pid c) (vector-ref c 1))
  (define (ctx-store c) (vector-ref c 2))
  (define (ctx-main c) (vector-ref c 3))
  (define (ctx-writers c) (vector-ref (vector-ref c 4) 0))
  (define (ctx-writers-set! c v) (vector-set! (vector-ref c 4) 0 v))
  ;; NOTE: SET WHEREVER THE MESSAGE IS RECEIVED, READ WHEREVER A DECISION IS
  ;; MADE. A connection can be told to drain while it is waiting for an
  ;; answer, while it is writing one, or while it is reading -- three
  ;; places -- and what follows differs at each. A flag is the only shape
  ;; that lets all three say the same thing.
  (define (ctx-draining? c) (vector-ref (vector-ref c 5) 0))
  ;; ADMITTED DRAINING: main answered this connection's watch while draining.
  ;; It reads at most one frame, under the same limit and deadline, and
  ;; answers it as a draining connection answers a whole frame; a `drain`
  ;; arriving meanwhile changes nothing.
  (define (ctx-admitted-draining? c) (vector-ref (vector-ref c 5) 1))
  (define (ctx-admit-draining! c) (vector-set! (vector-ref c 5) 1 #t))
  (define (ctx-drain! c)
    (vector-set! (vector-ref c 5) 0 #t)
    (guard (e (#t (if #f #f))) (conn-read-stop! (ctx-ref c))))

  ;; NOTE: ASKED ONCE PER WRITER, NOT ONCE PER REQUEST, and watched by this
  ;; connection as well as by main: the cache is this connection's, and
  ;; main forgetting a process cannot empty a table main cannot see.
  (define (target-for ctx request default-writer)
    (let ((name (writer-for-request request default-writer)))
      (if (not name)
          (ctx-store-pid ctx)
          (let ((known (assoc name (ctx-writers ctx))))
            (if known
                (cdr known)
                (let ((pid (ask-main (ctx-main ctx) (list 'writer-for name) 'writer-is)))
                  (monitor pid)
                  (ctx-writers-set! ctx (cons (cons name pid) (ctx-writers ctx)))
                  pid))))))

  (define (forget-writer! ctx who)
    (ctx-writers-set! ctx (forget-pid who (ctx-writers ctx))))
  ;; THIS CONNECTION CONSUMED A CACHED WRITER'S DOWN ON A PATH THAT IS NOT A
  ;; TERMINAL -- a pending request's wait, an answer already on the wire, or
  ;; idle time between requests -- and says so, by the name the writer was
  ;; cached under or by its pid when the cache has no entry, then forgets it.
  ;; The line means that and nothing more. Every such path comes here, so the
  ;; line does not depend on which wait the DOWN found; the store's death and
  ;; a request's own writer stay each wait's own, and forget-writer! itself
  ;; is not traced, since a terminal branch calls it too.
  (define (unrelated-writer-down! ctx who)
    (let ((e (find (lambda (e) (eq? who (cdr e))) (ctx-writers ctx))))
      (trace-event! 'writer-down-unrelated
                    (if e (car e) (call-with-string-output-port (lambda (p) (write who p))))
                    #f))
    (forget-writer! ctx who))

  ;; ---- a connection ------------------------------------------------------

  (define (conn-loop ref store-pid store main-pid)
    (let ((ctx (make-ctx ref store-pid store main-pid)))
      (monitor store-pid)
      ;; NEVER: MAIN IS TOLD WHEN THIS CONNECTION IS OVER, on every path out of
      ;; the loop below. "Nothing is running and no connection is left" is
      ;; main's completion condition for a drain, and a conn that ended
      ;; without saying so would hold the drain open until the clock ran
      ;; out -- a clean shutdown reported as a timeout.
      (dynamic-wind
        void
        (lambda ()
          ;; NEVER: NOTHING IS READ BEFORE MAIN HAS ADMITTED THIS CONNECTION.
          ;; The connection exists in the kernel and in its adapter before
          ;; main knows of it, and a flag set later orders nothing: a drain
          ;; that began meanwhile must still decide what this connection is
          ;; told. Main answers the watch the listener sent: admitted, or
          ;; admitted draining; during an idle leave the answer waits for the
          ;; attempt's decision.
          (receive
            (`(admitted ,draining?) (when draining? (ctx-admit-draining! ctx))))
      ;; NOTE: THE FIRST FRAME'S CLOCK STARTS ONLY ONCE THE READ HAS STARTED,
      ;; and a read that will not start is a failed connection: it is
      ;; closed, NEVER: not retried and NEVER: not run locally by anyone.
          (let ((started (conn-read-start! ref)))
            (if (eq? started 'ok)
                (frames ctx 0 (make-bytevector 0) (+ (real-time) frame-ms))
                (conn-close! ref))))
        (lambda () (send main-pid (list 'conn-done self))))))

  ;; NEVER: WHAT FOLLOWS A NEWLINE IS THE NEXT FRAME, NOT RUBBISH. One read
  ;; can carry two frames, or a frame and the beginning of another, and
  ;; an implementation that took the bytes before the first newline and
  ;; dropped the rest would lose a request that had already arrived --
  ;; silently, and only under the interleaving where a client sends two
  ;; requests quickly.
  ;;
  ;; NOTE: THE BUDGET IS THE WHOLE FRAME, NOT THE IDLE GAP: a client sending
  ;; one byte every four seconds is never idle, and a per-gap timer would
  ;; let it hold a connection open for ever.
  (define (frames ctx seq buffered deadline)
    (let ((ref (ctx-ref ctx))
          (cut (newline-at buffered)))
      (cond
        ;; NEVER: A FRAME THAT ARRIVED BEFORE THE DRAIN AND HAS NOT BEEN
        ;; STARTED IS REFUSED, NOT RUN. It is a request nobody has begun,
        ;; so `draining` is the true answer -- and it is answered rather
        ;; than dropped, because a client that is told nothing cannot
        ;; tell "refused" from "lost".
        ;; NEVER: THE SIZE IS JUDGED BEFORE ANYTHING ELSE LOOKS AT THE FRAME.
        ;; The "a whole frame is here" branch used to come first, so the
        ;; limit only ever fired on a frame that had NOT been terminated
        ;; -- measured: a request one megabyte over the limit with a
        ;; newline on the end was dispatched and answered `(ok (items))`,
        ;; while the same bytes without the newline were refused. The
        ;; limit held against clients that do not finish their frames,
        ;; which is nobody.
        ;;
        ;; NOTE: IT IS THE FRAME THAT IS MEASURED, NOT THE BUFFER. Several
        ;; small frames may arrive in one read, and their total says
        ;; nothing about any of them.
        ((and cut (> cut frame-limit))
         (frame-refused! 'socket cut)
         (answer-and-close ref '(error bad-request (reason frame-limit))))
        ;; NEVER: AND DRAINING IS A PREMISE, SO IT IS JUDGED AFTER THE FRAME
        ;; IS UNDERSTOOD. Answered here, a malformed frame arriving during
        ;; a drain was told `draining` -- which says "your request was
        ;; fine and we are not taking it now", when in fact it was never a
        ;; request at all. §7.6.50 v249 fixes the order: well-formed, then
        ;; whose store, then premises. The frame is parsed and then
        ;; refused, by `dispatch-frame`, which is told this connection is
        ;; draining.
        ((and cut (or (ctx-draining? ctx) (ctx-admitted-draining? ctx)))
         (let ((line (subbytes buffered 0 cut))
               (rest (subbytes buffered (+ cut 1) (bytevector-length buffered))))
           (dispatch-frame ctx seq line rest #t)))
        ;; NEVER: DRAINING WITH NOTHING WHOLE LEFT: this connection is done.
        ;; Waiting in the receive below would be waiting for bytes that
        ;; cannot arrive -- reads are stopped -- so the connection would
        ;; sit there until its frame budget expired and the drain would
        ;; be held open by a client that had already been served.
        ((ctx-draining? ctx) (conn-close! ref))
        ;; already holding a whole frame: serve it before reading more
        (cut (let ((line (subbytes buffered 0 cut))
                   (rest (subbytes buffered (+ cut 1) (bytevector-length buffered))))
               (dispatch-frame ctx seq line rest #f)))
        ((> (bytevector-length buffered) frame-limit)
         (frame-refused! 'socket (bytevector-length buffered))
         (answer-and-close ref '(error bad-request (reason frame-limit))))
        (else
         (receive
           (after (max 0 (- deadline (real-time))) (conn-close! ref))
           (`(data ,r ,bv)
            (frames ctx seq (append-bytes buffered bv) deadline))
           (`(eof ,r) (conn-close! ref))
           ;; NEVER: NOTHING IN FLIGHT HERE, so there is nothing to wait for:
           ;; this connection is idle and closing it is the whole of the
           ;; drain for it.
           (`(drain)
            (if (ctx-admitted-draining? ctx)
                (frames ctx seq buffered deadline)
                (begin (ctx-drain! ctx) (conn-close! ref))))
           (`(written ,r ,tok ,status) (frames ctx seq buffered deadline))
           (`#(DOWN ,who ,reason)
            ;; NEVER: ONLY THE STORE'S DEATH ENDS THIS CONNECTION. A writer
            ;; process this connection was watching may die while the
            ;; connection sits idle between frames; forgetting it is the
            ;; whole response, and NEVER: the connection must NOT end -- an
            ;; earlier version returned here for any DOWN at all, which
            ;; would have closed a healthy connection because some other
            ;; writer went away.
            (cond
              ((eq? who (ctx-store-pid ctx))
               (answer-and-close ref '(error transport-unknown (reason store-actor-down))))
              (else
               (unrelated-writer-down! ctx who)
               (frames ctx seq buffered deadline)))))))))

  ;; A FRAME REFUSED FOR ITS SIZE IS TRACED, so a reader of the trace can tell
  ;; a request the transport refused from one never sent: before, the refusal
  ;; left no line, and a row could only infer it from the dispatch that did
  ;; not happen. BYTES is the frame's length when its newline arrived, or what
  ;; was buffered without one. The local route's child traces the same event
  ;; with route child (core.sc, --frame).
  (define (frame-refused! route bytes)
    (trace-event! 'frame-refused (list 'route route) (list '(reason frame-limit) (list 'bytes bytes))))

  (define (newline-at bv)
    (let loop ((i 0))
      (cond ((>= i (bytevector-length bv)) #f)
            ((= (bytevector-u8-ref bv i) 10) i)
            (else (loop (+ i 1))))))

  (define (append-bytes a b)
    (let* ((na (bytevector-length a)) (nb (bytevector-length b))
           (out (make-bytevector (+ na nb))))
      (bytevector-copy! a 0 out 0 na)
      (bytevector-copy! b 0 out na nb)
      out))

  (define (subbytes bv from to)
    (let ((out (make-bytevector (- to from))))
      (bytevector-copy! bv from out 0 (- to from))
      out))

  ;; WHERE A FRAME WAS ANSWERED, ONE TRACE LINE PER FRAME:
  ;;   (trace routed <verb> connection|store|writer)
  ;; connection for every refusal made before a request is handed on and for
  ;; the reads this process answers itself; store or writer for the process
  ;; it was sent to. A frame whose verb cannot be read says `unknown`. It
  ;; changes nothing; the rows read it to tell the routes apart.
  ;; NOTE: NO HANDLER: a frame that did not parse is a symbol, and one that
  ;; did is an alist that may lack the field; both are asked, not caught.
  (define (routed! parsed route)
    (trace-event! 'routed
                  (let* ((field (and (pair? parsed) (assq 'request parsed)))
                         (request (and (pair? field) (cdr field))))
                    (if (and (pair? request) (symbol? (car request))) (car request) 'unknown))
                  route))

  (define (dispatch-frame ctx seq line rest draining?)
    (let ((ref (ctx-ref ctx))
          (parsed (parse-frame line)))
      (cond
        ((frame-refusal parsed)
         => (lambda (refusal) (routed! parsed 'connection) (answer-and-close ref refusal)))
        ;; NEVER: WELL-FORMED FIRST, AND THAT INCLUDES THE VERB'S ARGUMENTS
        ;; (§7.6.50's four layers: well-formed, then whose store, then
        ;; premises, then execution). Judged after the store, a request
        ;; whose arguments do not parse was answered `store-mismatch` when
        ;; sent to the wrong daemon -- which tells the caller to go and
        ;; find another daemon for a request no daemon would accept.
        ((request-shape-error (frame-field 'request parsed))
         => (lambda (err) (routed! parsed 'connection) (answer-and-close ref err)))
        ;; NEVER: A REQUEST FOR ANOTHER STORE IS REFUSED, NOT EXECUTED. This
        ;; daemon serves one store; running somebody else's verb against
        ;; it would write to the wrong library, silently.
        ;; THE STORE CHECK CAN FAIL TO READ: a spelling whose
        ;; parent this daemon may not search is neither this store nor
        ;; another, and the transport refuses it with the table's answer
        ;; (no scope: the check changes nothing, so the record is empty).
        ((guard (e ((classify-failure e '()) => (lambda (a) a)))
           (if (same-store? (frame-field 'store parsed) (ctx-store ctx))
               #f
               (list 'error 'transport-store-mismatch
                     (list 'serving (ctx-store ctx))
                     (list 'asked (frame-field 'store parsed)))))
         => (lambda (refusal) (routed! parsed 'connection) (answer-and-close ref refusal)))
        ;; NEVER: WELL-FORMED BEFORE DRAINING, on this side too (§7.6.50's
        ;; four layers). A request whose arguments do not parse used to be
        ;; answered `draining` while the daemon was going -- telling the
        ;; caller to try again somewhere else about a request that no
        ;; daemon would ever accept. What parses is not this file's
        ;; opinion: it asks the parser that owns the option tables, which
        ;; is also the one that will answer it for real a moment later.
        ;; NOTE: HERE, AFTER THE FRAME AND THE STORE HAVE BEEN JUDGED: this
        ;; request is well-formed and for this store, and is being refused
        ;; because the daemon is going.
        (draining? (routed! parsed 'connection) (answer-and-close ref draining-answer))
        (else
         (let ((actor (frame-field 'actor parsed))
               (mode (frame-field 'mode parsed))
               (request (frame-field 'request parsed)))
           (send (ctx-main ctx)
                 (list 'handling self seq (if (pair? request) (car request) request)))
           ;; NEVER: ONE LINE SAYING THIS DAEMON SERVED THIS REQUEST. Without
           ;; it, "the answer came from the daemon" can only be argued
           ;; from the answer's content -- and a local run of the same
           ;; verb produces the same content, which is the whole point of
           ;; the two routes. NOTE: It costs one comparison when tracing is
           ;; off, because `trace-event!` asks that first.
           (trace-event! 'daemon-dispatch
                         (if (pair? request) (car request) request)
                         #f)
           ;; NEVER: A WAY TO MAKE THIS PROCESS DIE, because otherwise nothing
           ;; can arm the one failure the trace rule exists for. A conn
           ;; is deliberately hard to kill from the wire -- a malformed
           ;; frame is answered, not fatal -- so without a seam the rule
           ;; that reports crashes would have no row behind it, and the
           ;; first real crash would be the first test of it.
           ;;
           ;; NOTE: Armed only by `THEOURGIA_FAULT=conn-raise@conn`, which
           ;; the ffi refuses at startup if it is misspelled; unarmed,
           ;; this is one comparison against #f.
           (when (eq? (theourgia-fault) 'conn-raise)
             (raise (condition (make-message-condition "injected conn raise")
                               (make-irritants-condition (list seq)))))
           ;; NOTE: THE ANSWER IS MATCHED AGAINST THIS REQUEST'S OWN
           ;; SEQUENCE NUMBER, so a reply that belongs to some earlier
           ;; exchange cannot be written out as the answer to this one.
           ;; A SUBSCRIPTION turns this connection into a stream for the
           ;; rest of its life: it is registered with the store process and
           ;; never comes back to `frames`.
           (if (eq? (car request) 'subscribe)
               (subscribe-stream ctx seq parsed rest mode)
           (if (conn-local-read? request)
               (begin
                 (routed! parsed 'connection)
                 ;; NEVER: A WAY TO BE INSIDE A CONNECTION-LOCAL READ WHILE
                 ;; SOMETHING ELSE HAPPENS. These reads are answered by
                 ;; this process without asking main, so the claim that a
                 ;; drain cannot finish underneath one rests on the
                 ;; connection still being registered -- and there was no
                 ;; way to hold a read open long enough for a drain to try.
                 ;; NOTE: ONE PARK PER PROCESS, like the writer's: a
                 ;; connection serving a second read while the row takes
                 ;; its reading would park again and the daemon would look
                 ;; hung rather than busy.
                 ;; NOTE: TWO LENGTHS, BECAUSE TWO ROWS NEED OPPOSITE THINGS,
                 ;; as with the writer's park: the short one has to end
                 ;; well inside the drain budget, the long one has to
                 ;; outlast it so that the only way out is the clock.
                 (when (and conn-hold-pending
                            (memq (theourgia-fault) '(conn-hold conn-hold-long)))
                   (set! conn-hold-pending #f)
                   (sleep-ms (if (eq? (theourgia-fault) 'conn-hold-long) 9000 2500)))
                 (write-answer ctx seq
                               (answer-published (ctx-store ctx) (ctx-store-pid ctx) request actor
                                           (frame-field 'writer parsed)
                                           (frame-field 'stdin parsed)
                                           (frame-field 'cwd parsed))
                               rest mode 'core))
           (let* ((target (target-for ctx request (frame-field 'writer parsed)))
                  (ticket (fresh-ticket))
                  ;; KEEPALIVES ONLY FOR A CLIENT THAT READS THEM, which says so
                  ;; by sending version 2; version 1 gets today's bytes.
                  (keepalive? (eqv? 2 (frame-field 'version parsed))))
             (routed! parsed (if (eqv? target (ctx-store-pid ctx)) 'store 'writer))
             ;; NEVER: THE COUNT EXISTS BEFORE THE REQUEST DOES. It is registered
             ;; before the request is published to the executor, so no bump can
             ;; come before it, and dropped on every way out of THIS wait -- not
             ;; of the connection, which goes on to later frames after the
             ;; answer.
             (let ((terminal
                     (dynamic-wind
                       (lambda () (when keepalive? (progress-register! ticket)))
                       (lambda ()
                         (send target (list 'request self seq ticket (ctx-main ctx) request actor
                                            (frame-field 'writer parsed)
                                            (frame-field 'stdin parsed)
                                            (frame-field 'cwd parsed)))
                         (await-terminal ctx seq target ticket keepalive?))
                       (lambda () (when keepalive? (progress-drop! ticket))))))
               (case (car terminal)
                 ((answer) (write-answer ctx seq (cadr terminal) rest mode (caddr terminal)))
                 ((refuse) (answer-and-close ref (cadr terminal)))
                 (else (void))))))))))))

  ;; THE WAIT FOR A REQUEST'S TERMINAL, keepalives included.
  ;; -> (answer <result> <origin>), (refuse <datum>), or (closed) when a
  ;; keepalive still pending at the terminal never completed.
  ;; THE KEEPALIVE, on a version-2 connection: every K, if this request's
  ;; count moved since the last one and none is pending, one byte 32 with a
  ;; fresh token. One write is pending at most, and its completion is matched
  ;; by this connection's ref and that token; a stale completion (another
  ;; connection's, an older write's) is ignored.
  ;; NEVER: THE TERMINAL IS LATCHED ON ARRIVAL. The answer, the store's
  ;; death, or the death of the writer serving this request stops the
  ;; keepalives at once; a keepalive still pending gets terminal-write-ms,
  ;; and when it does not complete in that the connection closes, the worker
  ;; untouched, and nothing more is written.
  ;; NEVER: A KEEPALIVE THAT FAILS -- its write raises, or completes with an
  ;; error -- STOPS THE KEEPALIVES AND NOTHING ELSE. The request runs to its
  ;; end; one trace line says so; the answer's own write then fails as it
  ;; does for a client that left.
  ;; NEVER: THE THREE DEATHS STAY THREE ANSWERS. Losing the store is fatal to
  ;; the daemon; losing the writer serving this request costs this request
  ;; and no other; losing some other writer this connection had cached costs
  ;; nothing at all and must not end the wait. Each is transport-unknown, not
  ;; failed: the request may have been carried out before the process died.
  ;; INJECTION ONLY: conn-killed-pending ends one connection actor, once,
  ;; while its version-2 request is being worked on: igropyr's kill, from
  ;; another actor, so none of the connection's unwinds run. It fires at the
  ;; first keepalive tick that sees this request's count moved, in place of
  ;; that keepalive: a count that moved is the executor's own bump for this
  ;; ticket, so the work has begun; a kill before it could land before the
  ;; executor started.
  (define conn-kill-armed #t)
  (define (await-terminal ctx seq target ticket keepalive?)
    (let ((ref (ctx-ref ctx))
          (me self)
          (live keepalive?)
          (pending #f)
          (issued 0)
          (seen 0)
          ;; INJECTION ONLY: keepalive-late's withheld completion, the token
          ;; it was issued under; finish sends it, once.
          (late-token #f)
          ;; INJECTION ONLY: one keepalive's completion is withheld for a while
          ;; or for ever, reported failed, or shadowed by stale completions;
          ;; or its write raises.
          (fault-once (and (memq (theourgia-fault) '(keepalive-late keepalive-never keepalive-error
                                                     keepalive-stale keepalive-raise))
                           (theourgia-fault))))
      (define (keepalive-failed! why)
        (set! live #f)
        (set! pending #f)
        (trace-event! 'keepalive-failed why #f))
      (define (issue!)
        (set! issued (+ issued 1))
        (let* ((tok (list 'keepalive issued))
               (fault (let ((f fault-once)) (set! fault-once #f) f))
               ;; a completion the fault replaces is written under a token no
               ;; one waits for
               (write-tok (if (memq fault '(keepalive-late keepalive-never keepalive-error))
                              (list 'keepalive-shadowed issued)
                              tok)))
          (when fault (trace-event! 'fault fault #f))
          (if (guard (e (#t #f))
                (when (eq? fault 'keepalive-raise)
                  (raise (make-message-condition "injected keepalive raise")))
                (conn-write! ref (make-bytevector 1 32) write-tok)
                #t)
              (begin
                (set! pending tok)
                (trace-event! 'keepalive issued #f)
                (case fault
                  ((keepalive-late) (set! late-token tok))
                  ((keepalive-error) (send me (list 'written ref tok 1)))
                  ((keepalive-stale)
                   (send me (list 'written (list 'another-connection) tok 0))
                   (send me (list 'written ref (list 'keepalive 0) 0)))
                  (else (void))))
              (keepalive-failed! 'raised))))
      (define (tick!)
        (when (and live (not pending))
          (let ((c (progress-count ticket)))
            (when (and c (> c seen))
              (set! seen c)
              (if (and conn-kill-armed (eq? (theourgia-fault) 'conn-killed-pending))
                  (begin
                    (set! conn-kill-armed #f)
                    (trace-event! 'fault 'conn-killed-pending #f)
                    (spawn (lambda () (kill me 'injected-kill))))
                  (issue!))))))
      (define (completes? r tok) (and pending (eq? r ref) (equal? tok pending)))
      ;; THE TERMINAL WITH A KEEPALIVE PENDING waits for that keepalive's
      ;; completion, under one absolute bound; terminal-pending says the wait
      ;; began and keepalive-completed that the matching completion ended it.
      ;; INJECTION ONLY: keepalive-late's withheld completion is sent here,
      ;; once, 300 ms into the wait, so the terminal always arrives while it
      ;; is pending; no message restarts either clock.
      (define (finish terminal)
        (set! live #f)
        (if (not pending)
            terminal
            (let ((deadline (+ (real-time) terminal-write-ms))
                  (late-at (and late-token (+ (real-time) 300))))
              (trace-event! 'terminal-pending ticket issued)
              (let wait ()
                (receive
                  (after (max 0 (- (if late-at (min late-at deadline) deadline) (real-time)))
                    (if (and late-at (< (real-time) deadline))
                        (let ((tok late-token))
                          (set! late-at #f)
                          (set! late-token #f)
                          (send me (list 'written ref tok 0))
                          (wait))
                        (begin
                          (trace-event! 'keepalive-timeout issued #f)
                          (conn-close! ref)
                          '(closed))))
                  (`(written ,r ,tok ,status)
                   (if (completes? r tok)
                       (begin (set! pending #f)
                              (trace-event! 'keepalive-completed ticket issued)
                              (unless (= status 0) (trace-event! 'keepalive-failed 'status #f))
                              terminal)
                       (wait)))
                  (`(drain) (ctx-drain! ctx) (wait))
                  (`#(DOWN ,who ,reason)
                   (cond
                     ((eq? who (ctx-store-pid ctx)) (void))
                     ((eq? who target) (forget-writer! ctx who))
                     (else (unrelated-writer-down! ctx who)))
                   (wait)))))))
      (let await ((due (+ (real-time) keepalive-ms)))
        (receive
          (after (if live (max 0 (- due (real-time))) 600000)
            (when live (tick!))
            (await (+ due keepalive-ms)))
          ;; NEVER: AN IN-FLIGHT REQUEST IS FINISHED, NOT ABANDONED. It may
          ;; already have changed the store; the drain is remembered and acted
          ;; on once this one has its answer.
          (`(drain) (ctx-drain! ctx) (await due))
          ;; NEVER: THE EXECUTOR SAYS WHICH SIDE SPOKE, because it is the only
          ;; process that knows: a refusal it made before the request began is
          ;; the transport declining, an answer it computed is the core's.
          (`(answer ,@seq ,result ,origin) (finish (list 'answer result origin)))
          (`(written ,r ,tok ,status)
           (when (completes? r tok)
             (set! pending #f)
             (unless (= status 0) (keepalive-failed! 'status)))
           (await due))
          (`#(DOWN ,who ,reason)
           (cond
             ((eq? who (ctx-store-pid ctx))
              (finish '(refuse (error transport-unknown (reason store-actor-down)))))
             ((eq? who target)
              (forget-writer! ctx who)
              (finish '(refuse (error transport-unknown (reason writer-actor-down)))))
             (else
              (unrelated-writer-down! ctx who)
              (await due))))))))

  ;; ---- a subscribed connection: the change stream -----------------------
  ;;
  ;; THE SUBSCRIPTION IS JUDGED BY THE STORE PROCESS, which holds the ring and
  ;; the current revision; this process asks, and an acceptance turns the
  ;; connection into a stream for the rest of its life. A refusal is an
  ;; ordinary answer and the connection goes on as before.
  (define stream-queue-limit 64)
  (define terminal-write-ms 2000)

  ;; NEVER: NO KEEPALIVE ON A SUBSCRIPTION, before its acceptance or after.
  ;; A subscription makes no progress of its own -- the store judges it at
  ;; once when it reaches it, and before that it is only queued -- so a
  ;; keepalive could report nothing but another request's work. Version 2
  ;; buys a subscriber the window on the silence before acceptance, kept by
  ;; the client.
  (define (subscribe-stream ctx seq parsed rest mode)
    (let ((ref (ctx-ref ctx))
          (ticket (fresh-ticket)))
      (routed! parsed 'store)
      (send (ctx-store-pid ctx)
            (list 'subscribe self seq ticket (ctx-main ctx) (cdr (frame-field 'request parsed))))
      (let await ()
        (receive
          ;; A DRAIN WHILE THE STORE IS JUDGING: remembered; an acceptance
          ;; then streams what it accepted and ends with the drain.
          (`(drain) (ctx-drain! ctx) (await))
          (`(subscription ,@seq ,answer ,replay)
           (if (and (pair? answer) (eq? (car answer) 'ok))
               (stream-run ctx answer replay rest)
               (write-answer ctx seq answer rest mode
                             (if (equal? answer draining-answer) 'transport 'core))))
          (`#(DOWN ,who ,reason)
           (cond
             ((eq? who (ctx-store-pid ctx))
              (answer-and-close ref '(error transport-unknown (reason store-actor-down))))
             (else (unrelated-writer-down! ctx who) (await))))))))

  ;; THE STREAM STATE. One write in flight at a time, each with a fresh
  ;; token; only a `(written ...)` naming this connection and that token
  ;; completes it. Order: the acceptance, the replay list the store snapshot
  ;; into the acceptance, then the live queue (at most 64; a live frame the
  ;; replay already wrote is skipped). Teardown is latched: the first cause
  ;; -- lagging, EOF, a failed write, the drain's end, the store's death, the
  ;; connection's -- unsubscribes once, drops the queue, writes the terminal
  ;; if it has one and the transport is usable, and closes; a frame that
  ;; arrives after that is consumed and not written.
  ;;
  ;; THE ACCEPTANCE IS ALWAYS WIRE-RENDERED, whatever the request's mode: the
  ;; lines after it are bare data, and whoever subscribes is a program.
  ;;
  ;; NO DEADLINE WHILE STREAMING: a stream idles legitimately. The only clock
  ;; is the terminal's: the write ahead of it and the terminal itself each
  ;; get terminal-write-ms.
  ;;
  ;; TRACES (THEOURGIA_TRACE): (stream-write <rev>|acceptance|notice|terminal),
  ;; (stream-written <status>), (write-pending conn) when libuv held a write's
  ;; bytes, (stream-queued <n>) at each enqueue, (stream-late <rev>) for a
  ;; frame consumed after closing, (stream-closed <cause>).
  (define (stream-run ctx answer replay rest)
    (let ((ref (ctx-ref ctx))
          (store-pid (ctx-store-pid ctx))
          (adapter (conn-ref-pid (ctx-ref ctx)))
          (pending #f)
          (pending-is #f)
          (tokens 0)
          (replayed 0)
          (queue '())
          (queued 0)
          (closing #f)
          (terminal #f)
          (deadline #f)
          (end-asked #f)
          (ended #f)
          (done #f)
          (input rest)
          (stale-armed (and (memq (theourgia-fault) '(stream-written-ref stream-written-token)) #t))
          (coalesce-armed (eq? (theourgia-fault) 'stream-coalesce-cut))
          (write-raise-armed (eq? (theourgia-fault) 'stream-write-raise)))
      (define (finish!)
        (set! done #t)
        (conn-close! ref))
      (define (latch! cause terminal-datum)
        (unless closing
          (set! closing cause)
          (trace-event! 'stream-closed cause #f)
          (unless (eq? cause 'store-down) (send store-pid (list 'unsubscribe self)))
          (set! replay '())
          (set! queue '())
          (set! queued 0)
          (if terminal-datum
              (begin
                (set! terminal (string->utf8 (render-wire terminal-datum)))
                (when pending (set! deadline (+ (real-time) terminal-write-ms))))
              (finish!))))
      (define (issue! bytes what)
        (set! tokens (+ tokens 1))
        (let ((tok (list 'stream tokens)))
          (set! pending tok)
          (set! pending-is what)
          (trace-event! 'stream-write what #f)
          (let ((held (guard (e (#t 'raised))
                        ;; INJECTION ONLY: one frame's write raises before
                        ;; anything is sent, as a queue failure does.
                        (when (and write-raise-armed (number? what))
                          (set! write-raise-armed #f)
                          (trace-event! 'fault 'stream-write-raise #f)
                          (raise (make-message-condition "injected stream write raise")))
                        (conn-write-observed! ref bytes tok))))
            (cond
              ;; A WRITE THAT RAISES ends the stream. igropyr leaves the
              ;; connection open when nothing was sent and closes it when a
              ;; prefix was: an open one gets the terminal attempted once, a
              ;; closed one none.
              ((eq? held 'raised)
               (set! pending #f)
               (write-failed! what))
              ((> held 0)
               (trace-event! 'write-pending 'conn #f)
               ;; INJECTION ONLY: a stale completion while this write is
               ;; pending -- another connection's, or an older write's.
               (when stale-armed
                 (set! stale-armed #f)
                 (trace-event! 'fault (theourgia-fault) #f)
                 (send self (if (eq? (theourgia-fault) 'stream-written-ref)
                                (list 'written (list 'another-connection) tok 0)
                                (list 'written ref (list 'stream 0) 0)))))))))
      ;; A FAILED WRITE, by a raise or by its completion's status: the
      ;; terminal's own failure ends at once; otherwise a usable transport
      ;; is told why, and a broken one is closed without a word.
      (define (write-failed! what)
        (cond
          ((eq? what 'terminal) (finish!))
          ((guard (e (#t #f)) (conn-open? ref))
           (latch! 'write-failed '(error transport-unknown (reason write-failed))))
          (else (latch! 'write-failed #f))))
      (define (enqueue! rev bytes)
        (if (= queued stream-queue-limit)
            ;; INJECTION ONLY: held at the overflow, before the latch, so a
            ;; row can have the store send frames that arrive after it.
            (begin
              (hold-point! 'stream-overflow)
              (latch! 'lagging (list 'error 'changes-unavailable '(reason lagging)
                                   (list 'current publish-seq) (list 'daemon daemon-token))))
            (begin
              (set! queue (append queue (list (cons rev bytes))))
              (set! queued (+ queued 1))
              (trace-event! 'stream-queued queued #f))))
      ;; INJECTION ONLY: two whole frames and the first half of a third in
      ;; one write, the rest in the next, so a reader can be shown one read
      ;; holding two lines and a tail.
      (define (coalesce-ready?)
        (and coalesce-armed (not closing) (not ended)
             (>= (length queue) 3)
             (number? (car (car queue))) (number? (car (cadr queue))) (number? (car (caddr queue)))
             #t))
      (define (coalesce!)
        (set! coalesce-armed #f)
        (trace-event! 'fault 'stream-coalesce-cut #f)
        (let* ((a (car queue)) (b (cadr queue)) (c (caddr queue))
               (third (cdr c))
               (half (div (bytevector-length third) 2)))
          (set! queue (cons (cons 'tail (subbytes third half (bytevector-length third))) (cdddr queue)))
          (set! queued (- queued 2))
          (issue! (append-bytes (append-bytes (cdr a) (cdr b)) (subbytes third 0 half)) (car a))))
      ;; THE NEXT WRITE, when none is in flight: the terminal once closing,
      ;; else the replay list, else the queue; and the drain's terminal when
      ;; the store has said stream-end and everything before it is written.
      (define (pump!)
        (let again ()
          (unless (or pending done)
            (cond
              (closing
               (if terminal
                   (let ((t terminal))
                     (set! terminal #f)
                     (set! deadline (+ (real-time) terminal-write-ms))
                     (issue! t 'terminal))
                   (finish!)))
              ((pair? replay)
               (let ((e (car replay)))
                 (set! replay (cdr replay))
                 (set! replayed (car e))
                 (issue! (cdr e) (car e))
                 (again)))
              ((and coalesce-armed (not closing) (not ended) (not (coalesce-ready?))) (void))
              ((coalesce-ready?) (coalesce!) (again))
              ((pair? queue)
               (let ((e (car queue)))
                 (set! queue (cdr queue))
                 (set! queued (- queued 1))
                 (cond
                   ;; the coalesced frame's rest goes after a pause, so a
                   ;; reader sees the first write on its own
                   ((eq? (car e) 'tail) (sleep-ms 200) (issue! (cdr e) 'tail))
                   ((and (number? (car e)) (<= (car e) replayed)) (void))
                   (else (issue! (cdr e) (or (car e) 'notice))))
                 (again)))
              (ended
               (latch! 'draining draining-answer)
               (again))
              (else (void))))))
      ;; A FURTHER FRAME FROM THE CLIENT, buffered leftovers included, is
      ;; answered bad-request subscribed through the queue.
      (define (take-requests!)
        (let ((cut (newline-at input)))
          (cond
            (cut
             (set! input (subbytes input (+ cut 1) (bytevector-length input)))
             (enqueue! #f (string->utf8 (render-wire '(error bad-request (reason subscribed)))))
             (unless closing (take-requests!)))
            ((> (bytevector-length input) frame-limit)
             (set! input (make-bytevector 0))
             (enqueue! #f (string->utf8 (render-wire '(error bad-request (reason frame-limit))))))
            (else (void)))))
      (define (ask-end!)
        (unless (or closing end-asked)
          (set! end-asked #t)
          (send store-pid (list 'stream-end self))))
      (monitor adapter)
      (issue! (string->utf8 (datum->string (answer-envelope answer 'wire 'core))) 'acceptance)
      (unless closing (take-requests!))
      (when (ctx-draining? ctx) (ask-end!))
      (let loop ()
        (pump!)
        (unless done
          (receive
            (after (if deadline (max 0 (- deadline (real-time))) 'infinity)
              (trace-event! 'stream-closed 'terminal-timeout #f)
              (finish!))
            (`(frame ,rev ,bytes)
             (if closing (trace-event! 'stream-late rev #f) (enqueue! rev bytes)))
            (`(stream-notice ,bytes)
             (unless closing (enqueue! #f bytes)))
            (`(written ,r ,tok ,status)
             (when (and pending (eq? r ref) (equal? tok pending))
               (set! pending #f)
               (trace-event! 'stream-written status #f)
               (cond
                 ((eq? pending-is 'terminal) (finish!))
                 ((not (= status 0)) (write-failed! pending-is)))))
            (`(data ,r ,bv)
             (unless closing
               (set! input (append-bytes input bv))
               (take-requests!)))
            (`(eof ,r) (latch! 'eof #f))
            (`(drain) (ctx-drain! ctx) (ask-end!))
            (`(stream-end) (set! ended #t))
            (`#(DOWN ,who ,reason)
             (cond
               ((eq? who store-pid)
                (latch! 'store-down '(error transport-unknown (reason store-actor-down))))
               ((eq? who adapter) (latch! 'connection-down #f))
               (else (unrelated-writer-down! ctx who)))))
          (loop)))))

  ;; NOTE: THE CONNECTION'S OWN PROCESS WRITES THE ANSWER, so a client that
  ;; does not read cannot hold up the store process or any other client.
  ;; NEVER: AND THE ORIGIN IS AN ARGUMENT, NOT A CONSTANT. It used to be
  ;; `'core` for everything written here, so a request the writer refused
  ;; while draining -- nothing dispatched, nothing ran -- arrived at the
  ;; client labelled as the core's own answer. `mcp/server.sc` reads this
  ;; field to decide whether a tool call was carried out, and a refusal
  ;; wearing `core` is reported to a caller as a result: the request was
  ;; not done, and the caller was told it was. Measured: D-24's queued
  ;; request answered `(error draining)` with `(origin core)`.
  (define (write-answer ctx seq result rest mode origin)
    (let ((ref (ctx-ref ctx))
          ;; NOTE: `render-wire` ENDS THE LINE ITSELF. This used to add one
          ;; because the old `datum->string` was a bare `write`; keeping
          ;; it after the printers were collapsed put a blank line after
          ;; every answer the daemon gave.
          ;;
          ;; NOTE: AND THE ENVELOPE IS ALWAYS WIRE-PRINTED, whatever the mode
          ;; is. The mode says how the ANSWER INSIDE it is rendered, for a
          ;; person to read; the envelope around it is read by a program
          ;; either way, so rendering that in human form would give the
          ;; client something it could not parse.
          (bytes (string->utf8 (datum->string (answer-envelope result mode origin)))))
      (conn-read-stop! ref)
      (conn-write! ref bytes seq)
      ;; NEVER: ITS OWN COMPLETION ONLY, by this connection's ref and the
      ;; answer's token: a keepalive's late completion is not the answer's.
      (let ((deadline (+ (real-time) 5000)))
      (let await ()
        (receive
          (after (max 0 (- deadline (real-time))) (conn-close! ref))
          (`(drain) (ctx-drain! ctx) (await))
          (`(written ,r ,tok ,status)
           (cond
             ((not (and (eq? r ref) (equal? tok seq))) (await))
             ((not (= status 0)) (conn-close! ref))
             ;; NEVER: THE LEFTOVER BYTES ARE STILL DEALT WITH, EVEN NOW.
             ;; NOTE: Measured with this branch closing here instead: a
             ;; second frame that had arrived in the same write as the
             ;; first was never answered at all -- the connection simply
             ;; ended -- and a client cannot tell that from a daemon that
             ;; died. `frames` answers it `draining` and closes after
             ;; THAT write. NEVER: Reads are NOT restarted: nothing new may
             ;; arrive, only what was already here may be finished.
             ((ctx-draining? ctx)
              (frames ctx (+ seq 1) rest (+ (real-time) frame-ms)))
             (else
              (let ((started (conn-read-start! ref)))
                (if (eq? started 'ok)
                    ;; NOTE: THE LEFTOVER BYTES GO ON, and the clock restarts
                    ;; for the frame they belong to, not for the one just
                    ;; answered.
                    (frames ctx (+ seq 1) rest (+ (real-time) frame-ms))
                    (void))))))
          ;; NEVER: A WRITER DYING WHILE THIS ANSWER IS ON THE WIRE IS NOT
          ;; THIS CONNECTION'S BUSINESS. Only the store's death ends it
          ;; here; anything else is forgotten and the wait goes on.
          (`#(DOWN ,who ,reason)
           (cond
             ((eq? who (ctx-store-pid ctx)) (void))
             (else (unrelated-writer-down! ctx who) (await)))))))))

  ;; NOTE: A REFUSAL IS AN ANSWER AND WEARS THE SAME ENVELOPE. The client
  ;; unpacks one shape and only one; a refusal that arrived bare would be
  ;; the single case it could not read, and it is the case that happens
  ;; when something is already wrong.
  ;;
  ;; NEVER: WIRE, because these are the answers given BEFORE the envelope has
  ;; been read -- a malformed frame has no mode to honour. A client that
  ;; asked for human form and gets a wire-rendered refusal can still read
  ;; it; the alternative is guessing a mode out of a frame that did not
  ;; parse.
  ;; NEVER: ITS OWN COMPLETION ONLY, as write-answer's.
  (define (answer-and-close ref result)
    (conn-write! ref (string->utf8 (datum->string (answer-envelope result 'wire 'transport))) 'last)
    (let ((deadline (+ (real-time) terminal-write-ms)))
      (let wait ()
        (receive
          (after (max 0 (- deadline (real-time))) (conn-close! ref))
          (`(written ,r ,tok ,status)
           (if (and (eq? r ref) (eq? tok 'last)) (conn-close! ref) (wait)))
          (`#(DOWN ,who ,reason) (void))))))

  ;; NEVER: THE SAME PRINTER THE CLI USES. This was a bare `write` that set
  ;; nothing, so the daemon's answers were already spelled differently
  ;; from the command line's for any non-ASCII character -- the two
  ;; routes are supposed to be byte for byte the same answer.
  ;; NOTE: `render-wire` ends the line itself, so the caller no longer adds
  ;; one.
  (define (datum->string x) (render-wire x))
)
