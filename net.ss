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

;;; (theourgia net) -- the transport, one adapter process per connection.
;;;
;;; ⭐ TERMINATION IS THE RUNTIME'S `DOWN`, NOT A MESSAGE THIS CODE SENDS.
;;; An adapter owns its connection, so when it dies -- for any reason --
;;; igropyr closes what it owned, clears a dial still in flight and shuts
;;; a success that lands late, and delivers `#(DOWN pid reason)` to every
;;; monitor. Nothing here has to guarantee that notification, because the
;;; runtime already does.
;;;
;;; ⛔ THERE ARE NO GUARDS IN AN ADAPTER'S BODY, and it never "retires".
;;; A raise is a death and a death is the notification; a deliberate end
;;; is `(kill (self) reason)`, which carries the reason to the monitors.
;;; An earlier design guarded every call and had the adapter deliver its
;;; own terminal message, and eight review rounds found the same class of
;;; defect over and over -- all of it downstream of those two decisions.
;;;
;;; ⛔ AND NOTHING KEEPS A REGISTRY. There is no table of live adapters to
;;; fall out of step with the world, which is why a leak here is measured
;;; with the runtime's own counters -- `process-count`, `conn-count` --
;;; and not with a number this library maintains about itself. A table
;;; can be told that something is gone; the runtime cannot.
;;;
;;; WHAT GOES THROUGH AN ADAPTER: only `start` and `stop`. They move the
;;; target and the read state, so they share one channel to keep their
;;; order. Writing and closing are direct calls.

(library (theourgia net)
  (export listen! stop-listen! listener-open? connect!
          conn-read-start! conn-read-stop!
          conn-write! conn-close! exchange
          conn-ref? conn-ref-conn conn-ref-pid listen-ref?)
  (import (rnrs base) (rnrs control) (rnrs records syntactic)
          (rnrs bytevectors) (rnrs lists)
          (only (chezscheme) real-time)
          (only (igropyr tcp)
                pipe-listen! pipe-connect! conn-set-owner! conn-owner
                conn-on-close! listener-token tcp-stop-listen!
                tcp-read-start! tcp-read-stop! tcp-write!)
          (rename (only (igropyr tcp) listener-open?)
                  (listener-open? igropyr-listener-open?))
          (only (igropyr actor)
                spawn send self receive monitor demonitor kill
                process-alive?))

  ;; ⛔ IMMUTABLE, AND BUILT ONCE PER CONNECTION. Consumers compare these
  ;; with `eq?`, so a second record for one connection would be a second
  ;; identity for one thing. A mutable pair would be worse: its cdr could
  ;; be changed while `eq?` still answered yes, and a later verb would
  ;; reach a different process than the caller believed it addressed.
  (define-record-type (conn-ref make-conn-ref conn-ref?)
    (fields (immutable conn conn-ref-conn)
            (immutable pid conn-ref-pid)))

  ;; ⚠️ THE TOKEN IS PART OF A LISTENER'S IDENTITY. A handle's address is
  ;; reused by whatever listens next, so `stop-listen!` given the handle
  ;; alone can close a listener that merely inherited the address. The
  ;; two-argument form is a no-op for an expired token, which is the
  ;; right answer for a stale record.
  (define-record-type (listen-ref make-listen-ref listen-ref?)
    (fields (immutable handle listen-ref-handle)
            (immutable token listen-ref-token)))

  ;; How long an accepted or dialled connection may sit with nobody
  ;; reading it. ⚠️ It bounds OUR consumer's inaction, never the peer's.
  (define default-idle-ms 5000)

  (define (idle-of opts) (if (pair? opts) (car opts) default-idle-ms))

  ;; ---- listening ---------------------------------------------------------
  ;;
  ;; ⭐ ALL OF OWNERSHIP HAPPENS INSIDE THE ACCEPT FRAME. igropyr runs
  ;; this callback in the event loop, where nothing may yield, and all
  ;; three of these are allowed there: `spawn` enqueues and returns,
  ;; `conn-set-owner!` and `conn-on-close!` have their own regions.
  ;;
  ;; ⚠️ THE REF IS PUBLISHED BY THE ADAPTER, not from here -- so by the
  ;; time anyone can name the connection its owner is already the process
  ;; that serves it, whatever order the scheduler picks.
  ;;
  ;; ⚠️ A RAISE IN THIS FRAME IS igropyr's TO CLEAN UP: its accept ladder
  ;; closes the in-flight connection when this callback throws, so a
  ;; failed allocation here leaves no ownerless connection behind.
  (define (listen! path backlog . opts)
    (let ((custodian self)
          (idle (idle-of opts)))
      (let ((h (pipe-listen! path backlog
                 (lambda (c)
                   (let ((a (spawn (lambda () (accepted-adapter c custodian idle)))))
                     (conn-set-owner! c a)
                     (conn-on-close! c (lambda () (send a (list 'closed)))))))))
        (make-listen-ref h (listener-token h)))))

  (define (stop-listen! lref)
    (tcp-stop-listen! (listen-ref-handle lref) (listen-ref-token lref)))

  ;; ⚠️ AN EARLY SIGNAL, NOT A HEALTH VERDICT. It answers what the library
  ;; believes about itself, and a listener that thinks it is accepting
  ;; while it is not is exactly the defect a self-answered question
  ;; cannot see. A supervisor still needs its own probe; this only makes
  ;; the loud case loud sooner.
  (define (listener-open? lref)
    (igropyr-listener-open? (listen-ref-handle lref) (listen-ref-token lref)))

  ;; ---- dialling ----------------------------------------------------------
  ;;
  ;; The adapter dials for itself, so igropyr makes it the owner when the
  ;; connection completes: there is no handover at all. Cancelling is
  ;; killing this pid, in flight or after -- the runtime clears the owner
  ;; and closes a success that arrives late.
  (define (connect! path . opts)
    (let ((custodian self)
          (idle (idle-of opts)))
      (spawn (lambda () (dialling-adapter path custodian idle)))))

  ;; ---- the adapter -------------------------------------------------------

  (define (dialling-adapter path custodian idle)
    (let ((me self)
          (cmon (monitor custodian)))
      ;; ⛔ NO GUARD. A synchronous refusal from the dial is this
      ;; process's death reason, and the condition itself reaches the
      ;; monitors.
      (pipe-connect! path me)
      (receive
        (`#(tcp-connected ,c)
         (let ((ref (make-conn-ref c me)))
           (conn-on-close! c (lambda () (send me (list 'closed))))
           (send custodian (list 'connected me ref))
           (before-start ref c custodian cmon (+ (real-time) idle))))
        (`#(tcp-connect-failed ,s) (kill me (list 'connect-failed s)))
        (`#(DOWN ,who ,r) (kill me (list 'target-down r))))))

  (define (accepted-adapter c custodian idle)
    (let ((me self))
      ;; ⛔ FIRST IT CHECKS THAT THIS CONNECTION IS ITS OWN. Ownership is
      ;; assigned in the accept frame AFTER the spawn; if indexing it
      ;; failed, or anything after the spawn threw, igropyr has already
      ;; closed the connection -- and this process must not go on to
      ;; publish a ref for something it does not own and then sit there
      ;; until the idle deadline.
      (unless (eq? (conn-owner c) me) (kill me 'birth-failed))
      (let ((cmon (monitor custodian))
            (ref (make-conn-ref c me)))
        (send custodian (list 'accepted ref))
        (before-start ref c custodian cmon (+ (real-time) idle)))))

  (define (before-start ref c target tmon deadline)
    (receive
      (after (max 0 (- deadline (real-time))) (kill self 'idle))
      (`(start ,tok ,from) (start-reading ref c target tmon from tok))
      ;; ⛔ `stop` MUST MATCH HERE TOO, although there is nothing to stop.
      ;; Left unmatched it stays in the mailbox and a later `start` is
      ;; taken ahead of it -- the phase would reorder a consumer's stop
      ;; and start, which is the one thing putting them on one channel is
      ;; meant to prevent.
      ;; ⛔ `stop` DOES NOT MOVE THE TARGET. Only `start` does (§D): a
      ;; process that stops a connection it does not own is asking for
      ;; backpressure, not asking to receive the bytes. Written the other
      ;; way this quietly handed the stream to whoever stopped it -- and
      ;; the mutation that should have caught it survived, because every
      ;; row stopped from the process that was already the target, where
      ;; moving the target is a no-op.
      (`(stop ,tok ,from)
       (send from (list 'stopped ref tok (target-kind target from)))
       (before-start ref c target tmon deadline))
      (`(closed) (kill self 'closed))
      (`#(DOWN ,who ,r) (kill self (list 'target-down r)))))

  ;; ⚠️ A STOP DOES NOT GO BACK TO THE IDLE DEADLINE. That bound is about
  ;; a consumer that never started; one that stops for backpressure is
  ;; not idle, and a resume is an ordinary start in this same phase.
  (define (started ref c target tmon)
    (receive
      (`(start ,tok ,from) (start-reading ref c target tmon from tok))
      (`(stop ,tok ,from)
       (tcp-read-stop! c)
       (send from (list 'stopped ref tok (target-kind target from)))
       (started ref c target tmon))
      (`#(tcp-data ,bv) (send target (list 'data ref bv)) (started ref c target tmon))
      ;; EOF IS NOT THE END OF THE CONNECTION: the peer half-closing its
      ;; writing side leaves ours open, and a server that answers after
      ;; the request has finished depends on exactly that.
      (`#(tcp-eof) (send target (list 'eof ref)) (started ref c target tmon))
      (`#(tcp-error ,n) (kill self (list 'error n)))
      (`(closed) (kill self 'closed))
      (`#(DOWN ,who ,r) (kill self (list 'target-down r)))))

  (define (target-kind target from)
    (if (eq? target from) 'same-target 'new-target))

  ;; Moving the target moves the watch with it, so exactly one target is
  ;; watched at a time and a target that dies still takes the adapter --
  ;; and with it the connection.
  (define (retarget-mon target tmon from)
    (cond
      ((eq? target from) tmon)
      (else (when tmon (demonitor tmon)) (monitor from))))

  ;; ⛔ A READ THAT DOES NOT START IS A DEATH. For an open connection
  ;; igropyr counts a negative `uv_read_start`, answers #f, and closes
  ;; nothing and sends nothing -- so an adapter that answered
  ;; `not-reading` and waited would be waiting for a hook that will never
  ;; run. Dying hands the connection back and tells every monitor why.
  (define (start-reading ref c target tmon from tok)
    (let ((kind (target-kind target from))
          (mon (retarget-mon target tmon from)))
      (cond
        ((tcp-read-start! c)
         (send from (list 'reading ref tok kind))
         (started ref c from mon))
        (else (kill self (list 'read-start-refused))))))

  ;; ---- the verbs, as a caller sees them ----------------------------------
  ;;
  ;; ⭐ `start` AND `stop` ARE SYNCHRONOUS, AND THE WATCH IS BOUNDED. The
  ;; caller monitors, sends, then waits for either the reply or the
  ;; adapter's DOWN, so no answer can be lost: a process that dies before
  ;; replying is reported by the runtime instead.
  ;;
  ;; ⚠️ AND THE WATCH IS DROPPED AGAIN UNLESS THIS CALL BECAME THE
  ;; TARGET. `monitor` does not de-duplicate, so a consumer that stops
  ;; and starts once per frame would accumulate one watch -- and one
  ;; future DOWN -- per frame. Only the call that first makes this
  ;; process the target keeps its monitor, as the standing watch.
  (define (conn-read-start! ref)
    (call-verb ref 'start #t))

  (define (conn-read-stop! ref)
    (call-verb ref 'stop #f))

  ;; ⛔ UNIQUE BY VALUE, NOT BY IDENTITY. The reply is matched with `,@`,
  ;; which compares with `equal?` -- so `(list verb)` would make every
  ;; call's token equal to every other call's, and one caller's answer
  ;; could satisfy another's wait. The counter is library-wide and the
  ;; scheduler is single-threaded, so these are unique across processes.
  (define verb-seq 0)

  (define (fresh-token verb)
    (set! verb-seq (+ verb-seq 1))
    (list verb verb-seq))

  (define (call-verb ref verb keep-when-new?)
    (let* ((pid (conn-ref-pid ref))
           (tok (fresh-token verb))
           (mon (monitor pid)))
      (send pid (list verb tok self))
      (await-reply pid mon tok keep-when-new?)))

  ;; ⛔ MATCHED BY VALUE, SO NOTHING ELSE IS TAKEN OUT OF THE MAILBOX.
  ;; Written as "match anything, compare, recurse if it is not mine", the
  ;; comparison happens AFTER igropyr has already removed the message --
  ;; and a caller that is also watching its store or writer process would
  ;; have that process's DOWN swallowed by a stop it happened to sit
  ;; behind. `,@tok` and `,@pid` match only this call's own reply and
  ;; this adapter's own death; everything else stays where it was.
  ;;
  ;; ⚠️ THIS IS THE SAME DEFECT THE OLD `exchange` HAD, in a new place:
  ;; reading the caller's mailbox and keeping what was not yours.
  (define (await-reply pid mon tok keep-when-new?)
    (receive
      (`(reading ,r ,@tok ,kind) (settle mon kind keep-when-new? 'ok))
      (`(stopped ,r ,@tok ,kind) (settle mon kind keep-when-new? 'ok))
      (`(not-reading ,r ,@tok ,kind)
       (settle mon kind keep-when-new? (list 'refused 'not-reading)))
      (`#(DOWN ,@pid ,r) (list 'down r))))

  (define (settle mon kind keep-when-new? answer)
    (when (or (not keep-when-new?) (eq? kind 'same-target))
      (when mon (demonitor mon)))
    answer)

  ;; ---- writing and closing are direct calls ------------------------------
  ;;
  ;; ⛔ THE ADAPTER IS NOT ON THE WRITE PATH. igropyr copies the bytes
  ;; inside this call, so nothing needs snapshotting; an exception stays
  ;; in this caller's own extent, where its guard can see it; and each
  ;; returned call produces exactly one completion, delivered by libuv
  ;; rather than by any code here.
  ;;
  ;; ⚠️ THE COMPLETION CAN ARRIVE AFTER THE ADAPTER'S DOWN. The two
  ;; travel by independent paths, and a write cancelled by a close
  ;; completes with ECANCELED whenever libuv reaches it.
  ;;
  ;; ⚠️ AND A RAISE FROM HERE LEAVES THE CONNECTION USABLE -- EXCEPT
  ;; WHERE A PREFIX IS ALREADY ON THE WIRE. igropyr treats those two
  ;; cases differently ON PURPOSE, and the difference is not an
  ;; inconsistency to report: when `uv_try_write` took nothing (EAGAIN),
  ;; a failure to queue has sent no byte, so the raise reaches this
  ;; caller and the connection is untouched; when it took a PREFIX and
  ;; the remainder cannot be queued, the stream now carries a truncated
  ;; message that nothing downstream could ever resynchronise, so
  ;; igropyr closes the connection and re-raises -- and our adapter
  ;; learns of that close through its hook and dies. Measured while
  ;; looking for a row for the second case: the first is what a
  ;; 48 KiB write to a peer that is not reading actually takes, and the
  ;; injection point refuses to fake a partial count, so that branch is
  ;; reachable only from a real kernel partial write.
  (define (conn-write! ref bv tok)
    (let ((me self))
      (tcp-write! (conn-ref-conn ref) bv
                  (lambda (status) (send me (list 'written ref tok status))))))

  ;; Closing is killing the adapter, and the runtime closes what it
  ;; owned. ⚠️ The same verb cancels a dial that has not completed: there
  ;; is no connection yet, and killing the dialler is what cancels it.
  (define (conn-close! ref)
    (kill-if-alive (conn-ref-pid ref)))

  (define (kill-if-alive pid)
    (when (process-alive? pid) (kill pid 'closed)))

  ;; ---- exchange ----------------------------------------------------------
  ;;
  ;; One question, one answer, in a process of its own so that this
  ;; library never reads the caller's mailbox.
  (define answer-limit (* 32 1024 1024))

  ;; ⛔ WHERE AN ANSWER ENDS IS THE CALLER'S KNOWLEDGE, NOT THIS
  ;; LIBRARY'S. `complete?` is asked of the bytes received so far and
  ;; answers whether they are a whole answer; passing #f means "end at
  ;; eof", which is what a peer that closes after speaking gives you.
  ;;
  ;; ⚠️ WITHOUT IT THIS COULD ONLY TALK TO PEERS THAT CLOSE. Measured
  ;; against the daemon, which is one frame one answer and holds the
  ;; connection open for the next: the eof-only version simply timed out,
  ;; because the answer was complete and nothing said so. A line-framed
  ;; caller passes a `complete?` that looks for its newline; this library
  ;; still knows nothing about lines.
  ;;
  ;; ⚠️ COST, STATED: when `complete?` is given, the bytes so far are
  ;; assembled on every chunk so it can be asked about them -- quadratic
  ;; in the number of chunks. That is the right trade for framed answers,
  ;; which are small and usually arrive in one or two pieces; a caller
  ;; expecting a very large answer should end at eof instead, where
  ;; nothing is assembled until the end.
  (define (exchange path request complete? timeout-ms)
    (unless (bytevector? request)
      (assertion-violation 'exchange "request must be a bytevector" request))
    (let* ((me self)
           (deadline (+ (real-time) timeout-ms))
           (dialer (spawn (lambda () (exchange-dial path request complete? deadline me)))))
      (let wait ()
        (receive
          (after (max 0 (- deadline (real-time)))
                 (kill-if-alive dialer)
                 (list 'transport-error 'timeout))
          (`(exchange-result ,who ,answer)
           (if (eq? who dialer) answer (wait)))
          (`#(DOWN ,who ,r)
           (if (eq? who dialer)
               (list 'transport-error (list 'dialer-died r))
               (wait)))))))

  (define (exchange-dial path request complete? deadline caller)
    (let ((me self))
      (pipe-connect! path me)
      (receive
        (after (max 0 (- deadline (real-time))) (kill me 'idle))
        (`#(tcp-connected ,c)
         (tcp-read-start! c)
         (tcp-write! c request (lambda (status) (send me (list 'written status))))
         (collect c '() 0 complete? deadline caller me))
        (`#(tcp-connect-failed ,s)
         (send caller (list 'exchange-result me (list 'transport-error s)))))))

  (define (collect c acc size complete? deadline caller me)
    (if (>= (real-time) deadline)
        (send caller (list 'exchange-result me (list 'transport-error 'timeout)))
        (receive
          (after (max 0 (- deadline (real-time)))
                 (send caller (list 'exchange-result me (list 'transport-error 'timeout))))
          (`#(tcp-data ,bv)
           (let ((n (+ size (bytevector-length bv))))
             (cond
               ((> n answer-limit)
                (send caller (list 'exchange-result me
                                   (list 'transport-error 'answer-too-large))))
               (else
                (let ((all (cons bv acc)))
                  (let ((whole (and (procedure? complete?) (join (reverse all)))))
                    (if (and whole (complete? whole))
                        ;; ⚠️ THE ANSWER IS WHOLE, SO THIS SIDE IS DONE:
                        ;; the connection is closed here rather than left
                        ;; for a peer that is not going to close it.
                        (send caller (list 'exchange-result me (list 'answer whole)))
                        (collect c all n complete? deadline caller me))))))))
          ;; ⛔ A PREFIX IS NOT AN ANSWER. When the caller said what a
          ;; complete answer looks like, EOF arriving before one is a
          ;; TRANSPORT FAILURE, not a short success: handing back what
          ;; had arrived lets a consumer treat a truncated reply as the
          ;; real one -- and the MCP shell would turn it into a
          ;; successful text result, which is the worst shape a lost
          ;; answer can take.
          ;;
          ;; ⚠️ WITHOUT `complete?` THE RULE IS UNCHANGED: a caller that
          ;; said nothing about completeness is asking to read until the
          ;; peer closes, and EOF is exactly that answer.
          (`#(tcp-eof)
           (let ((whole (join (reverse acc))))
             (send caller
                   (list 'exchange-result me
                         (if (procedure? complete?)
                             (if (complete? whole)
                                 (list 'answer whole)
                                 (list 'transport-error 'incomplete-answer))
                             (list 'answer whole))))))
          (`#(tcp-error ,n)
           (send caller (list 'exchange-result me (list 'transport-error n))))
          (`(written ,status)
           (if (= status 0)
               (collect c acc size complete? deadline caller me)
               (send caller (list 'exchange-result me (list 'transport-error status))))))))

  (define (join bvs)
    (let* ((total (let loop ((xs bvs) (n 0))
                    (if (null? xs) n (loop (cdr xs) (+ n (bytevector-length (car xs)))))))
           (out (make-bytevector total)))
      (let loop ((xs bvs) (at 0))
        (if (null? xs)
            out
            (let ((n (bytevector-length (car xs))))
              (bytevector-copy! (car xs) 0 out at n)
              (loop (cdr xs) (+ at n)))))))
)
