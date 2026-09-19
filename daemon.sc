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
  (export serve)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          (rnrs io simple) (rnrs io ports)
          (only (chezscheme) real-time getenv write newline read void let-values
                file-exists? char-whitespace? lookahead-char get-char
                register-signal-handler
                irritants-condition? condition-irritants filter raise condition
                make-message-condition make-irritants-condition
                open-string-input-port call-with-string-output-port
                condition? message-condition? condition-message guard exit)
          ;; NEVER: NAMED ONE BY ONE, AND `link` IS NOT AMONG THEM. A linked
          ;; process that exits abnormally takes its peer down with it
          ;; without asking, which is the one shape this daemon must not
          ;; have: a connection dying must cost that connection. Every
          ;; watch here is a `monitor`, and the way that rule is kept is
          ;; that the name is not in scope to be written -- NEVER: not that
          ;; somebody remembered. `test/daemon-link-gate.sc` reads this
          ;; import as data and refuses a wholesale one.
          (only (theourgia sched)
                start-scheduler spawn receive send self monitor sleep-ms)
          (only (theourgia net) listen! stop-listen! conn-read-start! conn-read-stop!
                conn-write! conn-close! conn-ref-pid)
          (only (theourgia rpc) rpc-dispatch rpc-ok?)
          (only (theourgia client) socket-path envelope-version)
          (only (theourgia render) render-wire render-human answer-printing!)
          (only (theourgia working) draft-lock-path)
          (only (theourgia store) open-and-reduce store-publish-hook!)
          (only (theourgia log) store-state-snapshot)
          (only (theourgia arguments) parse-arguments argument-option)
          ;; KEY: THE SAME LEXICAL RULE GUARDS BOTH DIRECTIONS. A request and
          ;; a reply are read by the same reader, and what that reader must
          ;; not be handed is one fact, not two: it lives beside the packer
          ;; in `(theourgia client)` and is asked here rather than copied.
          (only (theourgia client) readable-shape?)
          (only (theourgia trace) trace-event!)
          (only (theourgia ffi) theourgia-fault)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia ffi) lock-try-acquire! lock-release! lock-held? file-ensure! file-is-socket?
                current-lock-acquire current-lock-release
                mkdir-p! unlink! file-is-regular? file-is-directory? path-device-inode))

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
  (define (device-inode p)
    (guard (e (#t #f))
      (let-values (((dev ino) (path-device-inode p))) (cons dev ino))))

  (define (lock-path-for socket)
    (let* ((n (string-length socket))
           (slash (let loop ((i (- n 1))) (cond ((< i 0) #f) ((char=? (string-ref socket i) #\/) i) (else (loop (- i 1)))))))
      (if slash
          (string-append (substring socket 0 slash) "/." (substring socket (+ slash 1) n) ".lock")
          (string-append "." socket ".lock"))))

  ;; ---- the frame ---------------------------------------------------------
  ;;
  ;; One request per line, one answer per line. NEVER: THE LIMIT IS CHECKED
  ;; BEFORE ANYTHING IS PARSED: an over-long frame is refused and the
  ;; connection closed, never dispatched.
  (define frame-limit (* 1024 1024))

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
  (define signal-term 15)
  (define drain-poll-ms 25)
  (define drain-budget-ms 5000)

  (define (watch-for-signals! main-pid)
    (register-signal-handler signal-term
                             (lambda (signo) (set! sigterm-count (+ sigterm-count 1))))
    (spawn
      (lambda ()
        (let poll ((told 0))
          (sleep-ms drain-poll-ms)
          (cond
            ((>= sigterm-count 2) (send main-pid (list 'signal 'again)) (poll 2))
            ((and (>= sigterm-count 1) (= told 0))
             (send main-pid (list 'signal 'drain))
             (poll 1))
            (else (poll told)))))))

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
  (define (serve store . opts)
    ;; NEVER: ONCE, HERE, BEFORE ANY ANSWER LEAVES. Every conn process in this
    ;; VM prints through `render-wire`, and the settings it needs are per
    ;; OS thread -- so they are set on the way in rather than around each
    ;; write.
    (answer-printing!)
    (let ((socket (if (pair? opts) (car opts) (socket-path store))))
      (start-scheduler (lambda () (main store socket)))))

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

  (define (main store socket)
    (mkdir-p! (directory-of socket))
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
    (let ((lock (guard (e (#t #f)) (lock-try-acquire! (lock-path-for socket) 'exclusive))))
      (cond
        ((or (not lock) (not (lock-held? lock))) (report `(error serve-busy (path ,socket))) (exit 75))
        ((occupied-by-a-non-socket? socket)
         (report `(error serve-path-occupied (path ,socket)))
         ;; NOTE: MAIN'S OWN LOCK IS TAKEN AND RELEASED DIRECTLY, NEVER: never
         ;; through the service below: the service is a process asking
         ;; main, and main cannot answer itself from inside a handler.
         ;; It is also never in the table -- the table is for the
         ;; descriptors main opens on somebody else's behalf.
         (lock-release! lock)
         (exit 75))
        (else
         (clear-stale-socket! socket)
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
           (store-publish-hook! (lambda (state) (publish! store state)))
           ;; NOTE: INSTALLED BEFORE ANYTHING CAN BE ACCEPTED, so a signal
           ;; that arrives during the store's first load is not lost --
           ;; the count is read by a process, and the process is here.
           (watch-for-signals! me)
           (let* ((store-pid (spawn (lambda () (store-loop store me))))
                  (smon (monitor store-pid))
                  (st (make-main-state lock socket store store-pid #f)))
             (state-roles-set! st (list (cons store-pid 'store)))
             ;; NEVER: NOTHING IS ACCEPTED UNTIL THE STORE HAS PUBLISHED. The
             ;; listener is spawned by the `ready` clause below, not
             ;; here -- and main goes into its loop first because the
             ;; boot load takes the store's lock THROUGH main, so a main
             ;; that waited here for `ready` would be waiting for a
             ;; message that only it could make possible.
             (watch-loop st)))))))

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
  ;; 13 the conn processes that have been told to drain.
  (define (make-main-state lock socket store store-pid listener)
    (vector lock socket store store-pid listener '() '() #f '() '() self #f '() '()))

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
                 (report `(exiting (reason drain-timeout)
                                   (in-flight ,(length (state-running st)))))
                 (leave st 75)))
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
      (`(bound ,ident)
       (state-bound-set! st ident)
       (report `(serving (store ,(state-store st)) (socket ,(state-socket st))))
       (watch-loop st))
      ;; NEVER: THE DOOR OPENS HERE AND NOWHERE ELSE. Until the store has a
      ;; value to serve, a connection could only be told to wait or be
      ;; told something untrue -- so there are no connections.
      (`(ready)
       (let ((listener (spawn (lambda () (listener-loop (state-socket st)
                                                        (state-store-pid st)
                                                        (state-store st)
                                                        (state-main st))))))
         (monitor listener)
         (state-listener-set! st listener)
         (state-roles-set! st (cons (cons listener 'listener) (state-roles st))))
       (watch-loop st))
      (`(watch ,pid ,role)
       (monitor pid)
       (state-roles-set! st (cons (cons pid role) (state-roles st)))
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
       (lock-release! l)
       (send who (list 'lock-dropped tok (void)))
       (watch-loop st))
      ;; NEVER: ONE PROCESS PER WRITER, MADE ON DEMAND AND NAMED BY THE WRITER,
      ;; so that two connections working as the same writer meet in one
      ;; mailbox. A connection asks once and remembers the answer.
      (`(writer-for ,name ,who ,tok)
       (let* ((known (assoc name (state-writers st)))
              (pid (if known
                       (cdr known)
                       (let ((p (spawn (lambda () (writer-loop (state-store st) name)))))
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
         ((eq? which 'again)
          (report `(exiting (reason second-signal)))
          (leave st 75))
         ((state-draining st) (watch-loop st))
         (else
          (state-draining-set! st (real-time))
          (report `(draining (in-flight ,(length (state-running st)))))
          (when (state-listener st) (send (state-listener st) (list 'stop)))
          (for-each (lambda (entry)
                      (when (eq? 'conn (cdr entry)) (send (car entry) (list 'drain))))
                    (state-roles st))
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
       (finish-if-drained st)
       (watch-loop st))
      ;; The conn processes report themselves gone; when the last one has
      ;; and nothing is running, the drain is over.
      (`(conn-done ,who)
       (state-roles-set! st (remove-key who (state-roles st)))
       (finish-if-drained st)
       (watch-loop st))
      (`#(DOWN ,who ,reason)
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
            (report `(exiting (reason ,(if (eq? who (state-store-pid st))
                                           'store-actor-down
                                           'listener-down))))
            (leave st 75))
           (else
            (state-roles-set! st (remove-key who (state-roles st)))
            (state-handling-set! st (remove-key who (state-handling st)))
            ;; NOTE: A CONNECTION THAT DIED IS A CONNECTION THAT IS GONE. It
            ;; sends no `conn-done` -- a killed actor runs no unwinds --
            ;; so the drain's completion condition has to be re-asked
            ;; here as well, or a crash during a drain would be reported
            ;; as a drain that timed out.
            (finish-if-drained st)
            (watch-loop st)))))))

  ;; NEVER: ONE WAY OUT, SO THE SOCKET IS TIDIED ON ALL OF THEM. A daemon
  ;; that leaves its socket behind is one the next client waits on before
  ;; falling back, and every exit path that forgot the unlink would be a
  ;; separate small version of that bug.
  (define (leave st code)
    (unlink-own-socket! (state-socket st) (state-bound st))
    (lock-release! (state-lock st))
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
  (define (finish-if-drained st)
    (when (and (state-draining st)
               (null? (state-running st))
               (not (exists (lambda (entry) (eq? 'conn (cdr entry))) (state-roles st))))
      (report `(exiting (reason drained)))
      (leave st 0)))

  (define (forget-pid who alist)
    (let loop ((rest alist) (kept '()))
      (cond ((null? rest) (reverse kept))
            ((eq? who (cdar rest)) (loop (cdr rest) kept))
            (else (loop (cdr rest) (cons (car rest) kept))))))

  ;; NEVER: ONLY IF IT IS STILL OURS. A daemon that leaves without tidying up
  ;; leaves a socket that answers nothing, and the next client waits on
  ;; it before falling back -- but one that deletes whatever is at that
  ;; path deletes the socket of whoever took over in the meantime. The
  ;; dev/inode recorded at bind is what tells those two apart.
  (define (unlink-own-socket! socket bound)
    (let ((now (device-inode socket)))
      (when (and bound now (equal? bound now))
        (guard (e (#t #f)) (unlink! socket)))))

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
  (define (same-store? asked serving)
    (or (string=? asked serving)
        (let ((a (device-inode asked)) (b (device-inode serving)))
          (and a b (equal? a b)))))

  (define (directory-of p)
    (let loop ((i (- (string-length p) 1)))
      (cond ((< i 0) ".")
            ((char=? (string-ref p i) #\/) (substring p 0 i))
            (else (loop (- i 1))))))

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
  (define (occupied-by-a-non-socket? p)
    (guard (e (#t #f))
      (and (device-inode p) (not (file-is-socket? p)))))

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
  (define (clear-stale-socket! p)
    (when (device-inode p) (guard (e (#t #f)) (unlink! p))))

  (define (report x) (write x) (newline))

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
  (define published #f)
  (define publish-seq 0)

  (define (publish! store state)
    (set! publish-seq (+ publish-seq 1))
    (set! published (vector state publish-seq (store-state-snapshot store)))
    (trace-event! 'published publish-seq #f))

  (define (published-state) (and published (vector-ref published 0)))
  (define (published-snapshot) (and published (vector-ref published 2)))

  ;; NEVER: THE READ IS ANSWERED FROM WHAT IS PUBLISHED, WHATEVER THIS FINDS.
  ;; Noticing that somebody else has committed is a reason to ask the
  ;; store for a fresh fold, NEVER: not a reason to make this caller wait for
  ;; one: a read that reloaded before answering would take the store's
  ;; lock on the read path and hand back the delay this whole arrangement
  ;; exists to remove. Being one commit behind is the documented
  ;; behaviour; being slow is not.
  (define (probe-for-outside-change! store store-pid)
    (let ((was (published-snapshot)))
      (when (and was (not (equal? was (guard (e (#t was)) (store-state-snapshot store)))))
        (send store-pid (list 'reload)))))

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
  (define (store-here? store)
    (file-exists? (string-append store "/meta.sexp")))

  (define (store-loop store main-pid)
    (unless (store-here? store)
      (report (list 'error 'store-not-found (list 'store store)))
      (raise (list 'error 'store-not-found (list 'store store))))
    (let ((failure (guard (e (#t e)) (publish! store (open-and-reduce store)) #f)))
      (when failure
        (report (if (and (pair? failure) (eq? 'error (car failure)))
                    failure
                    (list 'error 'store-load-failed (list 'reason (condition-text failure)))))
        (raise failure)))
    (send main-pid (list 'ready))
    (let loop ()
      (receive
        ;; Somebody outside changed the store. Fold it again, under the
        ;; lock, and publish what came back.
        (`(reload)
         (guard (e (#t (if #f #f))) (publish! store (open-and-reduce store)))
         (loop))
        (`(request ,from ,seq ,ticket ,main-pid ,parsed ,actor ,writer ,piped ,cwd)
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
         (if (may-execute? main-pid ticket)
             (let ((answer (answer-for store parsed actor #f writer piped cwd)))
               (executed! main-pid ticket)
               (send from (list 'answer seq answer 'core)))
             (send from (list 'answer seq draining-answer 'transport)))
         (loop))
        (`(drain) (loop)))))

  ;; NOTE: A RAISED `(error ...)` IS AN ANSWER THAT TOOK THE SHORT WAY OUT.
  ;; The locking strategy raises `(error store-busy ...)` from deep
  ;; inside the store, where no return value could carry it; folding it
  ;; into `internal` would tell the client we broke when the truth is
  ;; that somebody else is holding the lock.
  ;; NEVER: WHAT THE CALLER PIPED IN AND WHERE THE CALLER WAS travel with the
  ;; request. `parse-frame` has always carried both out of the envelope
  ;; and nothing read them: `batch`, whose intents ARE its standard
  ;; input, answered its usage line on this route, and `write <id> -`
  ;; stored the literal "-" and reported `(ok (saved ...))` -- the
  ;; caller's bytes discarded without a word. The rule that says which
  ;; verbs read standard input is `argument-stdin`, in the parser, and it
  ;; is applied by the dispatcher rather than restated here.
  (define (answer-for store parsed actor state writer piped cwd)
    (guard (e ((and (pair? e) (eq? 'error (car e))) e)
              (#t (list 'error 'internal (list 'reason (condition-text e)))))
      (rpc-dispatch store parsed actor state writer
                    (and (string? piped) (lambda () piped))
                    cwd)))

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

  (define (writer-loop store name)
    (let loop ()
      (receive
        (`(request ,from ,seq ,ticket ,main-pid ,parsed ,actor ,writer ,piped ,cwd)
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
         (if (may-execute? main-pid ticket)
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
               (let ((answer (answer-for store parsed actor (published-state) writer piped cwd)))
                 (executed! main-pid ticket)
                 (send from (list 'answer seq answer 'core))))
             (send from (list 'answer seq draining-answer 'transport)))
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-raise-late))
           (set! writer-fault-pending #f)
           (raise (condition (make-message-condition "injected writer raise late")
                             (make-irritants-condition (list name seq)))))
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-hold)
                    writer-hold-name (not (equal? name writer-hold-name)))
           (set! writer-fault-pending #f)
           (raise (condition (make-message-condition "injected writer raise beside a holder")
                             (make-irritants-condition (list name seq)))))
         (loop)))))

  (define (condition-text e)
    (if (and (condition? e) (message-condition? e)) (condition-message e) 'raised))

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
          (lref (listen! socket 64)))
      (send main-pid (list 'bound (device-inode socket)))
      (let loop ()
        (receive
          ;; NEVER: THE DOOR CLOSES FIRST. Everything else about a drain is
          ;; about work already accepted; a listener still accepting
          ;; would keep making more of it.
          (`(stop)
           (guard (e (#t (if #f #f))) (stop-listen! lref))
           (loop))
          (`(accepted ,ref)
           ;; NOTE: MAIN IS TOLD ABOUT EVERY CONN IT WILL HAVE TO EXPLAIN.
           (let ((conn (spawn (lambda () (conn-loop ref store-pid store main-pid)))))
             (send main-pid (list 'watch conn 'conn)))
           (loop))
          (`#(DOWN ,who ,reason) (loop))))))

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
  (define conn-local-read-verbs '(read outline search))

  (define (conn-local-read? request)
    (and (pair? request)
         (symbol? (car request))
         (memq (car request) conn-local-read-verbs)
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                (not (argument-option nodes "--working"))
                (not (argument-option nodes "--working-info"))))))

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
                           (and (eq? (car request) 'read)
                                (or (argument-option nodes "--working")
                                    (argument-option nodes "--working-info"))))
                       name))))))

  ;; The part of a connection that does not change, in one value. A
  ;; connection has things to remember now -- which writer processes it
  ;; has been given -- and threading each new one through three
  ;; procedures as another positional argument is how a call and a
  ;; definition drift apart.
  (define (make-ctx ref store-pid store main-pid)
    (vector ref store-pid store main-pid (vector '()) (vector #f)))
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
         (answer-and-close ref '(error bad-request (reason frame-limit))))
        ;; NEVER: AND DRAINING IS A PREMISE, SO IT IS JUDGED AFTER THE FRAME
        ;; IS UNDERSTOOD. Answered here, a malformed frame arriving during
        ;; a drain was told `draining` -- which says "your request was
        ;; fine and we are not taking it now", when in fact it was never a
        ;; request at all. §7.6.50 v249 fixes the order: well-formed, then
        ;; whose store, then premises. The frame is parsed and then
        ;; refused, by `dispatch-frame`, which is told this connection is
        ;; draining.
        ((and cut (ctx-draining? ctx))
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
           (`(drain) (ctx-drain! ctx) (conn-close! ref))
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
               (forget-writer! ctx who)
               (frames ctx seq buffered deadline)))))))))

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

  ;; The shapes `rpc-dispatch` refuses before it looks at a verb are
  ;; refused here in the same words, so that judging them does not depend
  ;; on the daemon being willing to serve.
  (define (request-shape-error request)
    (guard (e (#t #f))
      (and (pair? request)
           (symbol? (car request))
           (for-all string? (cdr request))
           (let ((nodes (parse-arguments (car request) (cdr request))))
             (and (pair? nodes) (eq? 'error (car nodes)) nodes)))))

  (define (dispatch-frame ctx seq line rest draining?)
    (let ((ref (ctx-ref ctx))
          (parsed (parse-frame line)))
      (cond
        ((symbol? parsed)
         (answer-and-close ref (list 'error 'bad-request (list 'reason parsed))))
        ;; NEVER: WELL-FORMED FIRST, AND THAT INCLUDES THE VERB'S ARGUMENTS
        ;; (§7.6.50's four layers: well-formed, then whose store, then
        ;; premises, then execution). Judged after the store, a request
        ;; whose arguments do not parse was answered `store-mismatch` when
        ;; sent to the wrong daemon -- which tells the caller to go and
        ;; find another daemon for a request no daemon would accept.
        ((request-shape-error (frame-field 'request parsed))
         => (lambda (err) (answer-and-close ref err)))
        ;; NEVER: A REQUEST FOR ANOTHER STORE IS REFUSED, NOT EXECUTED. This
        ;; daemon serves one store; running somebody else's verb against
        ;; it would write to the wrong library, silently.
        ((not (same-store? (frame-field 'store parsed) (ctx-store ctx)))
         (answer-and-close ref (list 'error 'transport-store-mismatch
                                     (list 'serving (ctx-store ctx))
                                     (list 'asked (frame-field 'store parsed)))))
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
        (draining? (answer-and-close ref draining-answer))
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
           (if (conn-local-read? request)
               (begin
                 (probe-for-outside-change! (ctx-store ctx) (ctx-store-pid ctx))
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
                               (answer-for (ctx-store ctx) request actor (published-state)
                                           (frame-field 'writer parsed)
                                           (frame-field 'stdin parsed)
                                           (frame-field 'cwd parsed))
                               rest mode 'core))
           (let ((target (target-for ctx request (frame-field 'writer parsed)))
                 (ticket (fresh-ticket)))
             (send target (list 'request self seq ticket (ctx-main ctx) request actor
                                (frame-field 'writer parsed)
                                (frame-field 'stdin parsed)
                                (frame-field 'cwd parsed)))
             (let await ()
               (receive
                 ;; NEVER: AN IN-FLIGHT REQUEST IS FINISHED, NOT ABANDONED. It
                 ;; may already have changed the store; the drain is
                 ;; remembered and acted on once this one has its answer.
                 (`(drain) (ctx-drain! ctx) (await))
                 ;; NEVER: THE EXECUTOR SAYS WHICH SIDE SPOKE, because it is
                 ;; the only process that knows. A refusal it made before
                 ;; the request began is the transport declining; an
                 ;; answer it computed is the core's. Deciding here
                 ;; instead meant looking at the answer and guessing.
                 (`(answer ,@seq ,result ,origin)
                  (write-answer ctx seq result rest mode origin))
                 (`#(DOWN ,who ,reason)
                  ;; NEVER: THREE DIFFERENT DEATHS, THREE DIFFERENT ANSWERS.
                  ;; Losing the store is fatal to the daemon; losing the
                  ;; writer that was serving this request costs this
                  ;; request and no other; losing some other writer this
                  ;; connection had cached costs nothing at all and NEVER:
                  ;; must not be answered as a failure.
                  ;;
                  ;; NOTE: AND IT IS `transport-unknown`, not `failed`: the
                  ;; request may have been carried out before the process
                  ;; died. NEVER: Something that may have happened must never
                  ;; be reported as not having happened.
                  (cond
                    ((eq? who (ctx-store-pid ctx))
                     (answer-and-close ref '(error transport-unknown (reason store-actor-down))))
                    ((eq? who target)
                     (forget-writer! ctx who)
                     (answer-and-close ref '(error transport-unknown (reason writer-actor-down))))
                    (else
                     (forget-writer! ctx who)
                     (await)))))))))))))

  ;; NEVER: THE ENVELOPE NAMES THE STORE IT IS FOR:
  ;; `(request <store> <actor> <verb> <arg> ...)`.
  ;;
  ;; A client reaches a daemon by deriving the socket path from its
  ;; store, and `--socket P` lets it reach one directly -- so a request
  ;; meant for store B can arrive at a daemon serving store A. Without
  ;; the store in the envelope that request would be EXECUTED against A,
  ;; which is a write to the wrong library that nothing would ever
  ;; report. It is refused instead, and NEVER: not dispatched.
  ;; ---- the envelope, unpacked ---------------------------------------------
  ;;
  ;; NEVER: THE SHAPE IS PACKED IN `(theourgia client)` AND READ HERE, which
  ;; is two libraries because it is two processes. The ORDER is written
  ;; down in one of them; this side names the fields as it takes them
  ;; apart so that a change there shows up as a changed name here rather
  ;; than as a silently shifted meaning.
  ;;
  ;;   (request <version> <store> <actor> <writer> <mode> <cwd> <stdin>
  ;;            <verb> <args> ...)
  ;;
  ;; NOTE: THE VERSION IS CHECKED, NOT SKIPPED. A client built against a
  ;; later envelope must be told its request was not understood, rather
  ;; than have its fields read as though they were these fields -- which
  ;; is what a parser that only looked at positions would do, and would
  ;; do quietly.
  ;;
  ;; NOTE: `writer`, `cwd` AND `stdin` ARE EACH A STRING OR #f. #f is
  ;; "there is none", which is a thing the client says rather than a
  ;; field it leaves out: a frame whose length varies is one this reader
  ;; would have to guess about.
  (define (frame-field name parsed) (cdr (assq name parsed)))

  ;; NOTE: WHITESPACE AFTER THE FRAME IS NOT DATA. A trailing newline or a
  ;; space is how a frame ordinarily ends; only a further TOKEN is a
  ;; second thing on the line.
  (define (nothing-but-space-left? port)
    (let look ()
      (let ((c (lookahead-char port)))
        (cond
          ((eof-object? c) #t)
          ((char-whitespace? c) (get-char port) (look))
          (else #f)))))

  ;; NEVER: BYTES THAT ARE NOT UTF-8 ARE REFUSED, NOT REPAIRED. `utf8->string`
  ;; substitutes U+FFFD for a malformed sequence, so a frame whose actor
  ;; field held invalid bytes was decoded into a DIFFERENT actor and the
  ;; request then ran under it -- measured: an actor containing \xff\xfe
  ;; was accepted and the verb executed. What arrived is not what the
  ;; caller sent, and the caller is never told.
  ;;
  ;; NOTE: THE DECODER IS THE LIBRARY'S OWN, set to raise rather than
  ;; substitute. Writing a UTF-8 validator here would be a second decoder
  ;; to keep in step with the first.
  (define (frame-text bv)
    (bytevector->string bv (make-transcoder (utf-8-codec)
                                            (eol-style none)
                                            (error-handling-mode raise))))

  ;; NEVER: THE TWO WAYS A FRAME IS UNREADABLE ARE NOT THE SAME FACT, and the
  ;; refusal says which. `not-a-datum` is for bytes that will not decode
  ;; or will not read as a datum at all; `malformed-envelope` is for a
  ;; perfectly good datum whose fields are not an envelope, or a frame
  ;; with something after it. Both used to answer `not-a-datum`, which for
  ;; the second is simply untrue -- it IS a datum -- and sends the reader
  ;; looking at their encoding when the problem is their fields.
  (define (parse-frame bv)
    (guard (e (#t 'not-a-datum))
      (let ((text (frame-text bv)))
       (if (not (readable-shape? text))
           'not-a-datum
      (let* ((port (open-string-input-port text))
             (datum (read port))
             ;; NEVER: NOTHING MAY FOLLOW THE FRAME. A second datum on the
             ;; line is not a second request -- requests are separated by
             ;; the newline this reader was handed one of -- so it is
             ;; either a client that packed two things or someone
             ;; appending to a frame. Either way, executing the first and
             ;; discarding the rest is the worst of the options.
             ;;
             ;; NEVER: AND THE QUESTION IS PUT TO THE PORT, NOT TO THE READ.
             ;; Asked as "did the next read return an end-of-file object",
             ;; a frame containing the literal token `#!eof` answered yes
             ;; -- because a datum CAN BE an eof object. Measured: a frame
             ;; ending `... outline) #!eof (x)` executed `outline` and
             ;; discarded the rest, which is exactly what this check
             ;; exists to prevent, defeated by a literal.
             (clean (nothing-but-space-left? port)))
        (if (not (and clean (well-formed-envelope? datum)))
            'malformed-envelope
            (list (cons 'version (list-ref datum 1))
                  (cons 'store (list-ref datum 2))
                  (cons 'actor (list-ref datum 3))
                  (cons 'writer (list-ref datum 4))
                  (cons 'mode (list-ref datum 5))
                  (cons 'cwd (list-ref datum 6))
                  (cons 'stdin (list-ref datum 7))
                  (cons 'request (list-tail datum 8)))))))))

  ;; NEVER: THE ENVELOPE'S WRITER IS NOT SPLICED INTO THE ARGUMENTS. It used
  ;; to be: two tokens pushed to the front of the request unless a string
  ;; search over unparsed argv found `--writer` already there. That search
  ;; could not tell an option from a caller's own text, so a request whose
  ;; arguments contained the literal `--writer` -- after a `--`, or as the
  ;; bytes being written -- suppressed its own default and was refused
  ;; `writer-required`. The accepted cost of a string search turned out to
  ;; be a legitimate write that could not be made at all.
  ;;
  ;; The writer now travels beside the actor, as the identity it is, and
  ;; `rpc-dispatch` applies it in one place to the verbs that take one.
  ;; The arguments reach the parser exactly as the caller wrote them.

  (define (string-or-false? x) (or (not x) (string? x)))

  (define (well-formed-envelope? datum)
    (and (list? datum)
         (>= (length datum) 9)
         (eq? 'request (car datum))
         (equal? (list-ref datum 1) (envelope-version))
         (string? (list-ref datum 2))
         (string? (list-ref datum 3))
         (string-or-false? (list-ref datum 4))
         (memq (list-ref datum 5) '(wire human))
         (string-or-false? (list-ref datum 6))
         (string-or-false? (list-ref datum 7))
         (symbol? (list-ref datum 8))
         ;; NOTE: AND THE ARGUMENTS, which this did not check. The packer
         ;; refuses to build a frame whose arguments are not all strings;
         ;; the reader accepted one. `EV-3` asserts the packer will not
         ;; make what the reader will not read, and the reverse had no
         ;; guarantee at all.
         ;;
         ;; NEVER: IT WAS NOT REACHING ANYTHING DANGEROUS -- measured, the
         ;; dispatcher refuses a non-string argument itself
         ;; (`rpc.sc`, `arguments-not-strings`) and the daemon goes on
         ;; serving. This is the reader being symmetric with the packer,
         ;; not a repair of a crash, and the row that covers it asserts
         ;; the answer that actually comes back rather than a predicted
         ;; one.
         (all-strings? (list-tail datum 9))
         #t))

  (define (all-strings? xs)
    (or (null? xs)
        (and (string? (car xs)) (all-strings? (cdr xs)))))

  ;; ---- the answer envelope ------------------------------------------------
  ;;
  ;;   (answer (stdout "<bytes>") (stderr "<bytes>") (exit <n>) (origin <who>))
  ;;
  ;; KEY: THE SERVER RENDERS AND THE SERVER DECIDES THE EXIT CODE. Both are
  ;; knowledge about what a verb's answer MEANS -- `check` is a failure
  ;; when its verdict is not ok, `batch` when any one of its items is not
  ;; -- and that knowledge lives in `rpc-ok?`, in the core. A client that
  ;; worked it out would need the core, which is the cost the whole split
  ;; exists to avoid, and a client that carried a copy of the rule would
  ;; be a second place for it to be wrong.
  ;;
  ;; So the client unpacks this one shape, writes two byte strings to two
  ;; streams and exits with a number. That is the same work for every
  ;; verb, and is not knowledge about any of them.
  ;;
  ;; NOTE: `stderr` IS EMPTY TODAY, EVERY TIME. The core answers with one
  ;; datum and has nothing to say on a second stream; the field is here
  ;; because the client also runs the server locally for `init`, `serve`
  ;; and `eval`, where a real stderr exists and has to arrive somewhere.
  ;; NEVER: That is a description of today, not a promise -- a row asserts it
  ;; is empty, so the day something starts writing there it is a red row
  ;; and not a silently dropped message.
  ;; NEVER: WHO IS REFUSING IS NOT SOMETHING THE TEXT CAN BE ASKED. A core
  ;; `(error unknown-id ...)` is a successful call whose answer is no; a
  ;; `(error draining ...)` or `(error transport-unknown ...)` is this
  ;; daemon declining to carry the request, or admitting it does not know
  ;; whether it did. They arrive in the same envelope, and a shell that
  ;; had to tell them apart could only inspect the words inside -- or
  ;; treat every non-zero exit as a failure, which would make every core
  ;; refusal look like a broken call.
  ;;
  ;; NOTE: IT CANNOT BE DERIVED FROM THE TAG EITHER: `bad-request` is
  ;; answered both by this file, about a frame, and by the core, about a
  ;; verb's arguments. What separates them is WHERE the answer was made,
  ;; so that is what is recorded -- `transport` by the paths that refuse
  ;; without dispatching, `core` by the two that carry a dispatched
  ;; result.
  ;;
  ;; NOTE: It is a new field at the END of the envelope, and every reader
  ;; here takes fields by name, so a client of the previous shape reads
  ;; this one unchanged.
  (define (answer-envelope result mode origin)
    (list 'answer
          (list 'stdout ((if (eq? mode 'human) render-human render-wire) result))
          (list 'stderr "")
          (list 'exit (if (rpc-ok? result) 0 1))
          (list 'origin origin)))

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
      (let await ()
        (receive
          (after 5000 (conn-close! ref))
          (`(drain) (ctx-drain! ctx) (await))
          (`(written ,r ,tok ,status)
           (cond
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
             (else (forget-writer! ctx who) (await))))))))

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
  (define (answer-and-close ref result)
    (conn-write! ref (string->utf8 (datum->string (answer-envelope result 'wire 'transport))) 'last)
    (receive
      (after 2000 (conn-close! ref))
      (`(written ,r ,tok ,status) (conn-close! ref))
      (`#(DOWN ,who ,reason) (void))))

  ;; NEVER: THE SAME PRINTER THE CLI USES. This was a bare `write` that set
  ;; nothing, so the daemon's answers were already spelled differently
  ;; from the command line's for any non-ASCII character -- the two
  ;; routes are supposed to be byte for byte the same answer.
  ;; NOTE: `render-wire` ends the line itself, so the caller no longer adds
  ;; one.
  (define (datum->string x) (render-wire x))
)
