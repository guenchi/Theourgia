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
;;; ⭐ IT IS THE THIRD CALLER OF `rpc-dispatch`, NOT A SECOND SUPPLIER OF
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
;;; ⛔ NO `link` ANYWHERE IN HERE. Everything watches with `monitor`: a
;;; link would take this process down with whatever it is linked to, and
;;; a daemon whose store process dies must SAY so and exit on purpose,
;;; not vanish in a cascade. The gate in `test/` refuses a `link` in this
;;; file's transitive closure.
;;;
;;; ⚠️ A CONNECTION ENDING IS `#(DOWN pid reason)`, not a message this
;;; code invents: the adapter owns the connection and the runtime reports
;;; its death (§7.6.32 A). Closing one is killing that adapter.

(library (theourgia daemon)
  (export serve socket-path-for run-root)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          (rnrs io simple) (rnrs io ports)
          (only (chezscheme) real-time getenv write newline read void let-values
                irritants-condition? condition-irritants filter raise condition
                make-message-condition make-irritants-condition
                open-string-input-port call-with-string-output-port
                condition? message-condition? condition-message guard exit)
          (theourgia sched)
          (only (theourgia net) listen! stop-listen! conn-read-start! conn-read-stop!
                conn-write! conn-close! conn-ref-pid)
          (only (theourgia rpc) rpc-dispatch)
          (only (theourgia working) draft-lock-path)
          (only (theourgia store) open-and-reduce store-publish-hook!)
          (only (theourgia log) store-state-snapshot)
          (only (theourgia arguments) parse-arguments argument-option)
          (only (theourgia trace) trace-event!)
          (only (theourgia ffi) theourgia-fault)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia ffi) lock-try-acquire! lock-release! lock-held? file-ensure!
                current-lock-acquire current-lock-release
                mkdir-p! unlink! file-is-regular? file-is-directory? path-device-inode))

  ;; ---- where a daemon lives ---------------------------------------------
  ;;
  ;; ⚠️ THE RUN ROOT IS NOT `THEOURGIA_HOME`. A store may sit anywhere and
  ;; be arbitrarily deep, while `sun_path` is 104 bytes on macOS and
  ;; FreeBSD -- so the socket never lives beside the store. Clients derive
  ;; the same path from the same rule, and the store directory keeps no
  ;; pointer file, so copying a store does not carry a daemon with it.
  (define (run-root)
    (or (getenv "THEOURGIA_RUN")
        (string-append (or (getenv "HOME") "/tmp") "/.theourgia/run")))

  ;; The key is the store's resolved path, so two names for one store --
  ;; through a symlink, say -- reach one socket and one lock.
  (define (store-key store)
    (substring (bytevector->hex (sha256 (string->utf8 (resolve store)))) 0 16))

  ;; ⚠️ IT ANSWERS TWO VALUES AND RAISES FOR A PATH THAT IS NOT THERE.
  ;; Both matter and both bit: used as a single value it raises "returned
  ;; 2 values to single value return context" -- and a raise inside a
  ;; conn process is that process's death, which its adapter reports as
  ;; a closed connection. So the symptom was a client getting EOF and no
  ;; answer, nowhere near the arity mistake that caused it.
  (define (device-inode p)
    (guard (e (#t #f))
      (let-values (((dev ino) (path-device-inode p))) (cons dev ino))))

  (define (resolve p)
    (let ((dev (device-inode p)))
      (if dev
          (string-append p "|" (number->string (car dev)) ":" (number->string (cdr dev)))
          p)))

  (define (socket-path-for store)
    (string-append (run-root) "/" (store-key store) "/socket"))

  (define (lock-path-for socket)
    (let* ((n (string-length socket))
           (slash (let loop ((i (- n 1))) (cond ((< i 0) #f) ((char=? (string-ref socket i) #\/) i) (else (loop (- i 1)))))))
      (if slash
          (string-append (substring socket 0 slash) "/." (substring socket (+ slash 1) n) ".lock")
          (string-append "." socket ".lock"))))

  ;; ---- the frame ---------------------------------------------------------
  ;;
  ;; One request per line, one answer per line. ⛔ THE LIMIT IS CHECKED
  ;; BEFORE ANYTHING IS PARSED: an over-long frame is refused and the
  ;; connection closed, never dispatched.
  (define frame-limit (* 1024 1024))

  ;; ⚠️ THE BUDGET IS THE WHOLE FRAME, NOT THE IDLE GAP. A client that
  ;; sends one byte every four seconds is not idle, and a per-gap timer
  ;; would let it hold a connection for ever.
  (define frame-ms 5000)

  ;; ---- serve -------------------------------------------------------------

  ;; ⛔ ONE LOCKING STRATEGY FOR THE WHOLE DAEMON, SET ONCE. Inside a
  ;; daemon nothing may park on `flock`: that call runs on the
  ;; scheduler's own thread, so a process waiting there stops every
  ;; other process in the VM -- including the one that would have
  ;; released the lock, and including main.
  ;;
  ;; ⚠️ SET, NOT `parameterize`d AROUND THE PROCESSES THAT TAKE LOCKS.
  ;; Measured: Chez parameters are per OS thread and green threads share
  ;; one, so a parameterize in one actor is seen by every actor running
  ;; during its extent and is gone after it -- an actor reaching a lock
  ;; outside that window would get the blocking default, which is the
  ;; freeze this exists to prevent. What keeps the CLI on the default is
  ;; that nothing in the CLI ever sets this.
  (define (serve store . opts)
    (let ((socket (if (pair? opts) (car opts) (socket-path-for store))))
      (start-scheduler (lambda () (main store socket)))))

  ;; ---- the lock service ---------------------------------------------
  ;;
  ;; ⛔ THE DESCRIPTOR IS OPENED AND CLOSED BY MAIN, NEVER BY THE PROCESS
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
  ;; ⛔ AND MAIN NEVER WAITS. `lock-try-acquire!` is LOCK_NB; the waiting
  ;; -- the backoff, the budget, the refusal -- all happens in the
  ;; process that asked. A main parked on `flock` would stop every
  ;; process in the VM, including the one holding the lock it waits for.
  (define lock-budget-ms 5000)
  (define lock-backoff-cap-ms 200)

  ;; ⚠️ UNIQUE BY VALUE, because a reply is matched against the token
  ;; with `equal?`: two requests carrying equal tokens could take each
  ;; other's answers. One counter for the whole VM is enough -- green
  ;; threads do not preempt between the read and the write.
  (define lock-seq 0)
  (define (fresh-lock-token)
    (set! lock-seq (+ lock-seq 1))
    (list 'lk lock-seq))

  ;; ⚠️ NO TIMEOUT ON THE REPLY, ON PURPOSE. Main answers every request
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
               ;; ⛔ RAISED AS AN ANSWER, not as a crash: the conn turns
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
    ;; ⚠️ THE LOCK FILE HAS TO EXIST BEFORE IT CAN BE LOCKED: `flock`
    ;; wants an open descriptor, so the file is the lock's name, not its
    ;; content, and it is never removed -- removing it would let the next
    ;; caller create a NEW file and lock that one instead, which is two
    ;; daemons each holding "the" lock.
    (file-ensure! (lock-path-for socket))
    ;; ⛔ THE ATTEMPT THAT NEVER WAITS. A blocking flock here would park
    ;; this process on the scheduler's own thread -- and a second daemon
    ;; is supposed to answer `serve-busy` and leave, not queue for ever
    ;; behind the first one. Measured: with the blocking form, the second
    ;; daemon simply hung and its row never returned.
    (let ((lock (guard (e (#t #f)) (lock-try-acquire! (lock-path-for socket) 'exclusive))))
      (cond
        ((or (not lock) (not (lock-held? lock))) (report `(error serve-busy (path ,socket))) (exit 75))
        ((occupied-by-a-non-socket? socket)
         (report `(error serve-path-occupied (path ,socket)))
         ;; ⚠️ MAIN'S OWN LOCK IS TAKEN AND RELEASED DIRECTLY, ⛔ never
         ;; through the service below: the service is a process asking
         ;; main, and main cannot answer itself from inside a handler.
         ;; It is also never in the table -- the table is for the
         ;; descriptors main opens on somebody else's behalf.
         (lock-release! lock)
         (exit 75))
        (else
         (clear-stale-socket! socket)
         (let ((me self))
           ;; ⛔ ONE LOCKING STRATEGY FOR THE WHOLE VM, SET ONCE, HERE --
           ;; here because this is where main's own pid first exists, and
           ;; once because it has to hold for every process that will ever
           ;; take a lock.
           ;;
           ;; ⚠️ SET, NOT `parameterize`d. Measured: Chez parameters are
           ;; per OS thread and every green thread shares one, so a
           ;; parameterize in one actor is seen by every actor running
           ;; during its extent and is gone afterwards -- an actor
           ;; reaching a lock outside that window would get the blocking
           ;; default, which is the freeze this exists to prevent.
           ;;
           ;; ⭐ AND WHAT KEEPS THE CLI ON THE BLOCKING DEFAULT IS THAT
           ;; NOTHING IN THE CLI EVER SETS THIS. A CLI run has one thing
           ;; to do and no other processes to starve, so waiting is right
           ;; there, and giving up on a budget would be a regression.
           ;;
           ;; ⚠️ BOTH HALVES OR NEITHER: whoever opens the descriptor has
           ;; to be the one that closes it.
           (current-lock-acquire (make-lock-strategy me))
           (current-lock-release (make-lock-release me))
           ;; ⛔ SET HERE TOO, AND FOR THE SAME REASON: one publication
           ;; for the whole VM, and only the process that writes ever
           ;; reaches it.
           (store-publish-hook! (lambda (state) (publish! store state)))
           (let* ((store-pid (spawn (lambda () (store-loop store me))))
                  (smon (monitor store-pid))
                  (st (make-main-state lock socket store store-pid #f)))
             (state-roles-set! st (list (cons store-pid 'store)))
             ;; ⛔ NOTHING IS ACCEPTED UNTIL THE STORE HAS PUBLISHED. The
             ;; listener is spawned by the `ready` clause below, not
             ;; here -- and main goes into its loop first because the
             ;; boot load takes the store's lock THROUGH main, so a main
             ;; that waited here for `ready` would be waiting for a
             ;; message that only it could make possible.
             (watch-loop st)))))))

  ;; ⛔ MAIN'S STATE IS ONE VALUE THAT MAIN MUTATES, NOT NINE ARGUMENTS
  ;; IT THREADS. Four of the nine are association lists of the same
  ;; shape, and a loop that passes them positionally is a place where an
  ;; edit puts the writers where the locks go and nothing says so.
  ;; Measured, on the smaller version of this same loop: a five-argument
  ;; call against a seven-argument definition, which Chez reported as a
  ;; warning nobody was reading and which showed up as every row failing
  ;; with a transport error.
  ;;
  ;; ⚠️ MUTATION IS SAFE HERE FOR ONE REASON ONLY: main is the only
  ;; process that ever touches this value, and each clause below runs to
  ;; its end before the next `receive`.
  (define roles-slot 5)
  (define handling-slot 6)
  (define bound-slot 7)
  (define locks-slot 8)
  (define writers-slot 9)

  (define (make-main-state lock socket store store-pid listener)
    (vector lock socket store store-pid listener '() '() #f '() '() self))

  (define (state-lock st) (vector-ref st 0))
  (define (state-socket st) (vector-ref st 1))
  (define (state-store st) (vector-ref st 2))
  (define (state-store-pid st) (vector-ref st 3))
  (define (state-listener st) (vector-ref st 4))
  (define (state-listener-set! st v) (vector-set! st 4 v))
  (define (state-main st) (vector-ref st 10))
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

  ;; ⛔ MAIN ONLY WATCHES, AND IT SAYS WHY. A process that dies takes its
  ;; reason with it into a `#(DOWN pid reason)` that nobody else is
  ;; looking at -- and what a client sees is a connection closing with no
  ;; answer, which is the same thing it sees when a peer simply goes
  ;; away. Writing the reason to the trace is what separates those two,
  ;; and it costs one line.
  ;;
  ;; ⚠️ A NORMAL ENDING IS NOT AN EVENT. Tracing those would bury the one
  ;; line that matters under one line per connection that ever closed.
  (define (watch-loop st)
    (receive
      ;; The listener says what it bound, so the exit path can tell this
      ;; socket from one somebody else has since put in its place.
      (`(bound ,ident)
       (state-bound-set! st ident)
       (watch-loop st))
      ;; ⛔ THE DOOR OPENS HERE AND NOWHERE ELSE. Until the store has a
      ;; value to serve, a connection could only be told to wait or be
      ;; told something untrue -- so there are no connections.
      (`(ready)
       (let ((listener (spawn (lambda () (listener-loop (state-socket st)
                                                        (state-store-pid st)
                                                        (state-store st)
                                                        (state-main st))))))
         (monitor listener)
         (state-listener-set! st listener)
         (state-roles-set! st (cons (cons listener 'listener) (state-roles st)))
         (report `(serving (store ,(state-store st)) (socket ,(state-socket st)))))
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
      ;; ⛔ OPEN AND FLOCK HAPPEN HERE, IN A HANDLER THAT DOES NOT YIELD,
      ;; and with LOCK_NB, so this process never waits for anything.
      (`(lock-take ,path ,mode ,who ,tok)
       (let ((l (guard (e (#t #f)) (lock-try-acquire! path mode))))
         (when l (state-locks-set! st (cons (cons who l) (state-locks st))))
         (send who (list 'lock-answer tok (if l l 'busy))))
       (watch-loop st))
      ;; ⛔ AND SO DO THE CLOSE AND THE FORGETTING, in one handler: there
      ;; is no instant at which the descriptor is shut and the table
      ;; still names it, which is the instant at which a reused number
      ;; would be closed out from under its new owner.
      (`(lock-drop ,l ,who ,tok)
       (state-locks-set! st (drop-lock l (state-locks st)))
       (lock-release! l)
       (send who (list 'lock-dropped tok (void)))
       (watch-loop st))
      ;; ⛔ ONE PROCESS PER WRITER, MADE ON DEMAND AND NAMED BY THE WRITER,
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
      (`#(DOWN ,who ,reason)
       (let ((role (or (lookup who (state-roles st)) 'unknown)))
         (unless (eq? reason 'normal)
           (trace-event! 'daemon-down
                         (list role (lookup who (state-handling st)) (reason-text reason))
                         #f))
         ;; ⛔ WHATEVER IT WAS HOLDING IS RELEASED HERE AND NOWHERE ELSE.
         ;; A killed actor runs no unwinds, so a descriptor it was using
         ;; would otherwise keep the store locked for as long as this
         ;; daemon lives.
         (state-locks-set! st (release-all-of who (state-locks st)))
         ;; ⛔ AND IT IS FORGOTTEN BEFORE ANYTHING CAN ASK FOR IT AGAIN,
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
            (unlink-own-socket! (state-socket st) (state-bound st))
            (lock-release! (state-lock st))
            (exit 75))
           (else
            (state-roles-set! st (remove-key who (state-roles st)))
            (state-handling-set! st (remove-key who (state-handling st)))
            (watch-loop st)))))))

  (define (forget-pid who alist)
    (let loop ((rest alist) (kept '()))
      (cond ((null? rest) (reverse kept))
            ((eq? who (cdar rest)) (loop (cdr rest) kept))
            (else (loop (cdr rest) (cons (car rest) kept))))))

  ;; ⛔ ONLY IF IT IS STILL OURS. A daemon that leaves without tidying up
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
  ;; written as it is. ⛔ Neither is folded into "unknown".
  (define (reason-text reason)
    (cond
      ((and (condition? reason) (message-condition? reason))
       (list 'condition (condition-message reason)
             (if (irritants-condition? reason) (condition-irritants reason) '())))
      ((condition? reason) (list 'condition 'no-message))
      (else reason)))

  ;; ⚠️ TWO NAMES FOR ONE STORE ARE ONE STORE. A client may reach it
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

  ;; ⛔ ONLY A SOCKET IS EVER UNLINKED. A regular file or a directory on
  ;; that path is somebody else's, and removing it to make room would be
  ;; this daemon destroying data it does not own.
  (define (occupied-by-a-non-socket? p)
    (guard (e (#t #f)) (or (file-is-regular? p) (file-is-directory? p))))

  (define (clear-stale-socket! p)
    (when (device-inode p) (guard (e (#t #f)) (unlink! p))))

  (define (report x) (write x) (newline))

  ;; ---- what has been published --------------------------------------------
  ;;
  ;; ⛔ ONE CELL, WRITTEN ONLY BY THE STORE PROCESS, READ BY EVERYBODY.
  ;; Not a message broadcast to each reader, and ⛔ not a question the
  ;; readers ask the store: both would put the store process back in the
  ;; path of every read, which is the whole thing this arrangement
  ;; removes. Green threads share one OS thread, so a value written here
  ;; is visible to every process in this VM at the next instruction --
  ;; the same property that makes a Chez parameter useless for a
  ;; per-process override is exactly what is wanted here.
  ;;
  ;; ⚠️ THE VALUE IS NOT MODIFIED AFTER IT IS PUBLISHED. Each write folds
  ;; into a reduction of its own, so publishing hands over an object
  ;; nothing will touch again; readers are given it as a value they may
  ;; not change (see `reduction-for` in the rpc layer).
  ;;
  ;; ⚠️ AND THE SNAPSHOT TRAVELS WITH IT, because "is this still what is
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

  ;; ⛔ THE READ IS ANSWERED FROM WHAT IS PUBLISHED, WHATEVER THIS FINDS.
  ;; Noticing that somebody else has committed is a reason to ask the
  ;; store for a fresh fold, ⛔ not a reason to make this caller wait for
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
  ;; ⛔ THE ONLY PROCESS THAT OPENS THE STORE. `open-and-reduce` takes the
  ;; store's flock, and a lock taken inside an actor blocks the whole
  ;; scheduler thread -- so if two processes could take it, one of them
  ;; parked behind the other would freeze everything, including the
  ;; connection that was going to release it.
  ;; ⛔ THE FIRST PUBLICATION HAPPENS BEFORE ANYBODY CAN ASK FOR
  ;; ANYTHING. Main does not open the socket until this has reported
  ;; `ready`, so there is no such thing as a read arriving with nothing
  ;; published -- and therefore no "no value yet" case for a reader to
  ;; get wrong.
  ;;
  ;; ⚠️ AND A BOOT THAT CANNOT TAKE THE LOCK SAYS SO IN ITS OWN WORDS.
  ;; Somebody else holding the store for longer than the budget is an
  ;; ordinary state of the world; `store-busy` is what a client would be
  ;; told, so it is what the daemon prints before it goes, rather than
  ;; leaving main to report the death of a process and nothing about why.
  (define (store-loop store main-pid)
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
        (`(request ,from ,seq ,parsed ,actor)
         ;; ⛔ AND A WAY TO MAKE THIS ONE DIE TOO. The store process is
         ;; the daemon: without it nothing can be answered, so main is
         ;; supposed to say so and leave with 75 rather than sit there
         ;; accepting connections it cannot serve. That rule needs a row,
         ;; and a row needs a seam. Armed by
         ;; `THEOURGIA_FAULT=store-raise@conn`; unarmed it is one
         ;; comparison against #f.
         (when (eq? (theourgia-fault) 'store-raise)
           (raise (condition (make-message-condition "injected store raise")
                             (make-irritants-condition (list seq)))))
         ;; ⛔ #f, NOT THE PUBLISHED VALUE. This process is the one
         ;; that writes, and a write decides what to do from the
         ;; store as it stands under the lock -- answering from a
         ;; fold made before the lock was taken is how two writers
         ;; come to disagree about what was there.
         (send from (list 'answer seq (answer-for store parsed actor #f)))
         (loop))
        (`(drain) (loop)))))

  ;; ⚠️ A RAISED `(error ...)` IS AN ANSWER THAT TOOK THE SHORT WAY OUT.
  ;; The locking strategy raises `(error store-busy ...)` from deep
  ;; inside the store, where no return value could carry it; folding it
  ;; into `internal` would tell the client we broke when the truth is
  ;; that somebody else is holding the lock.
  (define (answer-for store parsed actor state)
    (guard (e ((and (pair? e) (eq? 'error (car e))) e)
              (#t (list 'error 'internal (list 'reason (condition-text e)))))
      (rpc-dispatch store parsed actor state)))

  ;; ---- a writer's process -----------------------------------------------
  ;;
  ;; ⛔ ONE PER WRITER, AND ITS DEATH IS NOT THE DAEMON'S. The store
  ;; process is the serialisation point for everything that publishes,
  ;; and losing it means nothing can be answered at all -- so main leaves
  ;; when it dies. A writer's working directory is that writer's alone,
  ;; so a writer process that dies costs exactly that writer's request:
  ;; main closes whatever descriptors it had open for it, forgets it, and
  ;; the next request for that name gets a new process.
  ;;
  ;; ⚠️ IT ANSWERS THROUGH THE SAME DISPATCHER AS EVERYTHING ELSE. No
  ;; verb, no answer and no error shape exists on this path that does not
  ;; exist on the other one; what differs is which mailbox serialises it.
  ;; ⛔ A WAY TO DIE HOLDING A LOCK, because that is the one failure the
  ;; table in main exists for and a rule with no row behind it is tested
  ;; for the first time by the first real crash. The seam takes a REAL
  ;; lock through the real strategy -- so main really has registered a
  ;; real descriptor -- and then dies without releasing it, which is what
  ;; a killed actor does: ⛔ no unwind runs, and the descriptor does not
  ;; go away with the process that was using it.
  ;;
  ;; ⚠️ IT FIRES ONCE. The row's point is that the NEXT request for the
  ;; same writer is served; a fault that fired on every request would
  ;; measure nothing but the fault.
  ;;
  ;; ⚠️ Armed only by `THEOURGIA_FAULT=writer-raise@conn`, which the ffi
  ;; refuses at startup if it is misspelled; unarmed, this is one
  ;; comparison against #f.
  ;; ⚠️ AND A SECOND ONE, FOR THE OTHER HALF OF THE RULE. `writer-raise`
  ;; dies HOLDING a descriptor main opened; `writer-raise-late` dies
  ;; after the answer has gone out, holding nothing -- main's table no
  ;; longer names it, and tidying up after it must therefore close
  ;; nothing. The two are different faults because they are different
  ;; claims, and one row cannot be evidence for both.
  ;; ⚠️ AND A THIRD, WHICH ARMS A SITUATION RATHER THAN AN EVENT.
  ;; `writer-hold` is one fault because the thing it has to produce is
  ;; one interleaving that no single event describes: ONE writer parked
  ;; inside a lock main opened for it, WHILE ANOTHER writer dies and is
  ;; tidied up. A `THEOURGIA_FAULT` names one fault, so the two halves
  ;; are two clauses of it -- the first writer to arrive parks, and the
  ;; next request from a DIFFERENT writer answers and then dies.
  ;;
  ;; ⛔ AND IT PARKS IN THE DRAFT LOCK, not the store lock. The second
  ;; writer's request has to be served WHILE the first is parked, and it
  ;; needs the store lock to be served -- so a hold on the store lock
  ;; would serialise the two and there would be no interleaving left to
  ;; test. A writer's draft lock is that writer's alone.
  (define writer-fault-pending #t)
  (define writer-hold-name #f)

  (define (park-in-draft-lock store name)
    (let ((path (draft-lock-path store name)))
      (mkdir-p! (directory-of path))
      (file-ensure! path)
      ;; ⚠️ LONG ENOUGH FOR THE OTHER WRITER TO BE SERVED, TO DIE, AND
      ;; FOR MAIN TO TIDY UP AFTER IT -- and for the row to take its
      ;; reading before this one wakes. A park of a few hundred
      ;; milliseconds is over before that sequence has finished, and the
      ;; row would then be measuring an unparked daemon.
      (let ((l ((current-lock-acquire) path 'exclusive)))
        (sleep-ms 1500)
        ((current-lock-release) l))))

  (define (writer-loop store name)
    (let loop ()
      (receive
        (`(request ,from ,seq ,parsed ,actor)
         (when (and writer-fault-pending (eq? (theourgia-fault) 'writer-raise))
           (set! writer-fault-pending #f)
           ((current-lock-acquire) (string-append store "/lock") 'exclusive)
           (raise (condition (make-message-condition "injected writer raise")
                             (make-irritants-condition (list name seq)))))
         (when (and (eq? (theourgia-fault) 'writer-hold) (not writer-hold-name))
           (set! writer-hold-name name)
           (park-in-draft-lock store name))
         ;; ⛔ THE PUBLISHED VALUE, NOT A FRESH FOLD. A note verb reads
         ;; the library to decide what a draft is based on, and doing
         ;; that by opening the log would take the store's lock on a
         ;; path that is supposed to be able to run beside a commit.
         ;; Being one commit behind is the documented behaviour here,
         ;; and the consequences of acting on a stale premise are caught
         ;; where they have to be caught -- by the commit, under the lock.
         (send from (list 'answer seq (answer-for store parsed actor (published-state))))
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
  ;; ⚠️ IT DOES NOT START READING. The connection is handed to a conn
  ;; process, and that process makes itself the target -- so the bytes go
  ;; to whoever serves them and never through here.
  (define (listener-loop socket store-pid store main-pid)
    (let ((me self))
      (listen! socket 64)
      (send main-pid (list 'bound (device-inode socket)))
      (let loop ()
        (receive
          (`(accepted ,ref)
           ;; ⚠️ MAIN IS TOLD ABOUT EVERY CONN IT WILL HAVE TO EXPLAIN.
           (let ((conn (spawn (lambda () (conn-loop ref store-pid store main-pid)))))
             (send main-pid (list 'watch conn 'conn)))
           (loop))
          (`#(DOWN ,who ,reason) (loop))))))

  ;; ---- routing -----------------------------------------------------------
  ;;
  ;; ⛔ ROUTING ASKS THE SAME PARSER THE VERB WILL ASK. A second reader of
  ;; the command line here would be a second supplier of one fact, and on
  ;; the day the two disagreed about which writer a request names, two
  ;; requests for one writer would run in two processes at once and its
  ;; working directory would have no serialisation left at all. So the
  ;; name comes from `parse-arguments`, the call `rpc-dispatch` itself
  ;; makes.
  ;;
  ;; ⚠️ WHAT MAY LEAVE THE STORE PROCESS IS DECIDED BY WHAT IT TOUCHES,
  ;; not by whether it writes. These touch one writer's working directory
  ;; and nothing else. ⛔ `commit` is not among them: it publishes, so it
  ;; belongs on the serialised path with the reads.
  (define writer-local-verbs '(write restore discard drafts))

  ;; ⛔ THESE THREE ARE ANSWERED IN THE CONNECTION'S OWN PROCESS, from
  ;; the published value, without opening the log and without taking the
  ;; store's lock. They read and nothing else, so putting them through
  ;; the one process that serialises writing would make every reader wait
  ;; behind a commit for no reason at all.
  ;;
  ;; ⛔ AND THE LIST IS NOT WIDENED BY GUESSING. A verb answered from a
  ;; published fold that in fact touches something else would be doing it
  ;; outside the lock, with nothing to notice. Anything not named here
  ;; goes to the store process: slower, and right.
  ;;
  ;; ⚠️ `read --working` IS NOT THIS `read`. It asks about a writer's
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

  (define (writer-for-request request)
    (and (pair? request)
         (symbol? (car request))
         (for-all string? (cdr request))
         (let ((nodes (parse-arguments (car request) (cdr request))))
           (and (list? nodes)
                (not (and (pair? nodes) (eq? 'error (car nodes))))
                (let ((name (argument-option nodes "--writer")))
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
    (vector ref store-pid store main-pid (vector '())))
  (define (ctx-ref c) (vector-ref c 0))
  (define (ctx-store-pid c) (vector-ref c 1))
  (define (ctx-store c) (vector-ref c 2))
  (define (ctx-main c) (vector-ref c 3))
  (define (ctx-writers c) (vector-ref (vector-ref c 4) 0))
  (define (ctx-writers-set! c v) (vector-set! (vector-ref c 4) 0 v))

  ;; ⚠️ ASKED ONCE PER WRITER, NOT ONCE PER REQUEST, and watched by this
  ;; connection as well as by main: the cache is this connection's, and
  ;; main forgetting a process cannot empty a table main cannot see.
  (define (target-for ctx request)
    (let ((name (writer-for-request request)))
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
      ;; ⚠️ THE FIRST FRAME'S CLOCK STARTS ONLY ONCE THE READ HAS STARTED,
      ;; and a read that will not start is a failed connection: it is
      ;; closed, ⛔ not retried and ⛔ not run locally by anyone.
      (let ((started (conn-read-start! ref)))
        (if (eq? started 'ok)
            (frames ctx 0 (make-bytevector 0) (+ (real-time) frame-ms))
            (conn-close! ref)))))

  ;; ⛔ WHAT FOLLOWS A NEWLINE IS THE NEXT FRAME, NOT RUBBISH. One read
  ;; can carry two frames, or a frame and the beginning of another, and
  ;; an implementation that took the bytes before the first newline and
  ;; dropped the rest would lose a request that had already arrived --
  ;; silently, and only under the interleaving where a client sends two
  ;; requests quickly.
  ;;
  ;; ⚠️ THE BUDGET IS THE WHOLE FRAME, NOT THE IDLE GAP: a client sending
  ;; one byte every four seconds is never idle, and a per-gap timer would
  ;; let it hold a connection open for ever.
  (define (frames ctx seq buffered deadline)
    (let ((ref (ctx-ref ctx))
          (cut (newline-at buffered)))
      (cond
        ;; already holding a whole frame: serve it before reading more
        (cut (let ((line (subbytes buffered 0 cut))
                   (rest (subbytes buffered (+ cut 1) (bytevector-length buffered))))
               (dispatch-frame ctx seq line rest)))
        ((> (bytevector-length buffered) frame-limit)
         (answer-and-close ref '(error bad-request (reason frame-limit))))
        (else
         (receive
           (after (max 0 (- deadline (real-time))) (conn-close! ref))
           (`(data ,r ,bv)
            (frames ctx seq (append-bytes buffered bv) deadline))
           (`(eof ,r) (conn-close! ref))
           (`(written ,r ,tok ,status) (frames ctx seq buffered deadline))
           (`#(DOWN ,who ,reason)
            ;; ⛔ ONLY THE STORE'S DEATH ENDS THIS CONNECTION. A writer
            ;; process this connection was watching may die while the
            ;; connection sits idle between frames; forgetting it is the
            ;; whole response, and ⛔ the connection must NOT end -- an
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

  (define (dispatch-frame ctx seq line rest)
    (let ((ref (ctx-ref ctx))
          (parsed (parse-frame line)))
      (cond
        ((eq? parsed 'bad) (answer-and-close ref '(error bad-request (reason not-a-datum))))
        ;; ⛔ A REQUEST FOR ANOTHER STORE IS REFUSED, NOT EXECUTED. This
        ;; daemon serves one store; running somebody else's verb against
        ;; it would write to the wrong library, silently.
        ((not (same-store? (car parsed) (ctx-store ctx)))
         (answer-and-close ref (list 'error 'transport-store-mismatch
                                     (list 'serving (ctx-store ctx))
                                     (list 'asked (car parsed)))))
        (else
         (let ((actor (cadr parsed)) (request (caddr parsed)))
           (send (ctx-main ctx)
                 (list 'handling self seq (if (pair? request) (car request) request)))
           ;; ⛔ A WAY TO MAKE THIS PROCESS DIE, because otherwise nothing
           ;; can arm the one failure the trace rule exists for. A conn
           ;; is deliberately hard to kill from the wire -- a malformed
           ;; frame is answered, not fatal -- so without a seam the rule
           ;; that reports crashes would have no row behind it, and the
           ;; first real crash would be the first test of it.
           ;;
           ;; ⚠️ Armed only by `THEOURGIA_FAULT=conn-raise@conn`, which
           ;; the ffi refuses at startup if it is misspelled; unarmed,
           ;; this is one comparison against #f.
           (when (eq? (theourgia-fault) 'conn-raise)
             (raise (condition (make-message-condition "injected conn raise")
                               (make-irritants-condition (list seq)))))
           ;; ⚠️ THE ANSWER IS MATCHED AGAINST THIS REQUEST'S OWN
           ;; SEQUENCE NUMBER, so a reply that belongs to some earlier
           ;; exchange cannot be written out as the answer to this one.
           (if (conn-local-read? request)
               (begin
                 (probe-for-outside-change! (ctx-store ctx) (ctx-store-pid ctx))
                 (write-answer ctx seq
                               (answer-for (ctx-store ctx) request actor (published-state))
                               rest))
           (let ((target (target-for ctx request)))
             (send target (list 'request self seq request actor))
             (let await ()
               (receive
                 (`(answer ,@seq ,result)
                  (write-answer ctx seq result rest))
                 (`#(DOWN ,who ,reason)
                  ;; ⛔ THREE DIFFERENT DEATHS, THREE DIFFERENT ANSWERS.
                  ;; Losing the store is fatal to the daemon; losing the
                  ;; writer that was serving this request costs this
                  ;; request and no other; losing some other writer this
                  ;; connection had cached costs nothing at all and ⛔
                  ;; must not be answered as a failure.
                  ;;
                  ;; ⚠️ AND IT IS `transport-unknown`, not `failed`: the
                  ;; request may have been carried out before the process
                  ;; died. ⛔ Something that may have happened must never
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

  ;; ⛔ THE ENVELOPE NAMES THE STORE IT IS FOR:
  ;; `(request <store> <actor> <verb> <arg> ...)`.
  ;;
  ;; A client reaches a daemon by deriving the socket path from its
  ;; store, and `--socket P` lets it reach one directly -- so a request
  ;; meant for store B can arrive at a daemon serving store A. Without
  ;; the store in the envelope that request would be EXECUTED against A,
  ;; which is a write to the wrong library that nothing would ever
  ;; report. It is refused instead, and ⛔ not dispatched.
  (define (parse-frame bv)
    (guard (e (#t 'bad))
      (let ((datum (read (open-string-input-port (utf8->string bv)))))
        (if (and (pair? datum) (eq? 'request (car datum)) (pair? (cdr datum))
                 (string? (cadr datum)) (pair? (cddr datum))
                 (string? (caddr datum)) (pair? (cdddr datum)))
            (list (cadr datum) (caddr datum) (cdddr datum))
            'bad))))

  ;; ⚠️ THE CONNECTION'S OWN PROCESS WRITES THE ANSWER, so a client that
  ;; does not read cannot hold up the store process or any other client.
  (define (write-answer ctx seq result rest)
    (let ((ref (ctx-ref ctx))
          (bytes (string->utf8 (string-append (datum->string result) "\n"))))
      (conn-read-stop! ref)
      (conn-write! ref bytes seq)
      (let await ()
        (receive
          (after 5000 (conn-close! ref))
          (`(written ,r ,tok ,status)
           (if (= status 0)
               (let ((started (conn-read-start! ref)))
                 (if (eq? started 'ok)
                     ;; ⚠️ THE LEFTOVER BYTES GO ON, and the clock restarts
                     ;; for the frame they belong to, not for the one just
                     ;; answered.
                     (frames ctx (+ seq 1) rest (+ (real-time) frame-ms))
                     (void)))
               (conn-close! ref)))
          ;; ⛔ A WRITER DYING WHILE THIS ANSWER IS ON THE WIRE IS NOT
          ;; THIS CONNECTION'S BUSINESS. Only the store's death ends it
          ;; here; anything else is forgotten and the wait goes on.
          (`#(DOWN ,who ,reason)
           (cond
             ((eq? who (ctx-store-pid ctx)) (void))
             (else (forget-writer! ctx who) (await))))))))

  (define (answer-and-close ref result)
    (conn-write! ref (string->utf8 (string-append (datum->string result) "\n")) 'last)
    (receive
      (after 2000 (conn-close! ref))
      (`(written ,r ,tok ,status) (conn-close! ref))
      (`#(DOWN ,who ,reason) (void))))

  (define (datum->string x)
    (call-with-string-output-port (lambda (port) (write x port))))
)
