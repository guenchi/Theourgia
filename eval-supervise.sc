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

;;; (theourgia eval-supervise) -- watching one evaluation, in the caller's
;;; own process.
;;;
;;; NEVER: NO HELPER PROCESS BETWEEN US AND THE CHILD. The adapter owns the
;;; child, so the child's end is the adapter's DOWN and nothing has to be
;;; reaped by hand.
;;;
;;; NEVER: THE HANDSHAKE IS A SAFETY ORDERING, NOT A GREETING. Until the
;;; worker's `(ready <pgid>)` has arrived there may be no process group,
;;; so a timeout before it may kill only the direct child; after it, the
;;; whole group. A worker that never says ready is killed by pid and
;;; reported unavailable -- which is also the only safe thing to do,
;;; since at that point it has run no untrusted code and can have no
;;; grandchildren.
;;;
;;; KEY: THE WORKER'S PROTOCOL STREAM IS "READY, THEN ZERO OR MORE
;;; (incomplete ...) NOTES, THEN ONE ANSWER". A note is the whole clause the
;;; worker's loads have heard so far, written when it grows (K14; review r4,
;;; A1-1). This supervisor keeps the last note it has read and appends it
;;; to EVERY answer it returns -- a limit, a worker that exited without an
;;; answer, and the worker's own answer -- so an evaluation stopped after a
;;; load that could not see a writer still says which. The answer is the
;;; last datum that is not a note. With no note nothing is appended, and
;;; every answer is what it was before notes existed. A refusal before
;;; ready (no-ready, spawn-refused) precedes any load and carries none.
;;;
;;; NOTE: THE DEADLINE IS ABSOLUTE AND IT COVERS THE MAILBOX. It is computed
;;; once, from the moment the worker says ready, and every wait below is
;;; `(max 0 (- deadline (now)))` -- a budget recomputed per message is a
;;; budget a chatty child can refill.

(library (theourgia eval-supervise)
  (export supervise-eval)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          ;; NOTE: `utf8->string` COMES FROM `(rnrs bytevectors)`, and this file
          ;; must not define its own. An earlier draft did -- decoding byte
          ;; by byte through `integer->char`, which is Latin-1 -- and it
          ;; would have quietly mangled every non-ASCII character the
          ;; output quota is measured in.
          (only (chezscheme) real-time
                open-string-input-port read guard condition? message-condition?
                condition-message eof-object eof-object?)
          (only (theourgia sched) spawn receive send self sleep-ms monitor)
          (only (theourgia proc) spawn-worker! worker-write! worker-close-stdin!
                worker-kill! worker-close! worker-rss worker-ref-pid)
          (only (theourgia ffi) signal-pid!)
          (only (theourgia render) render-wire))

  ;; NOTE: THE TWO CLOCKS ARE DIFFERENT AND BOTH ARE NEEDED. `ready-ms` is
  ;; how long a worker may take to exist; the caller's timeout is how long
  ;; an evaluation may take once it does, and it STARTS WHEN THE WORKER SAYS
  ;; READY (supervise-eval below). Until this batch the evaluation's
  ;; deadline was taken before the wait for ready, while this comment
  ;; already promised otherwise; with a ready budget longer than the default
  ;; timeout that would have answered eval-limit for an evaluation that had
  ;; never begun. test/eval-ready.sc pins a two-second start under a
  ;; one-second timeout.
  ;; NEVER: A READY BUDGET NEAR THE WORKER'S OWN START-UP. The worker
  ;; imports the store and the reducer before it says ready, and from
  ;; source that import alone measured about 650 ms median on an unloaded
  ;; developer machine; at 500 ms a healthy store answered
  ;; eval-worker-unavailable in about half the runs beside another job,
  ;; and in every run at load average 5. This budget is wall clock, so it
  ;; has to hold the machine's load as well as the worker's work: about
  ;; eight times the unloaded cost. A worker that never says ready holds
  ;; its caller for the whole of it. test/eval-ready.sc pins a worker that
  ;; takes one second to exist.
  (define ready-ms 5000)
  (define rss-interval-ms 50)

  ;; NEVER: SIGKILL, AND TO THE WHOLE GROUP ONCE THERE IS ONE. `worker-kill!`
  ;; takes a SIGNAL NUMBER and signals the child itself; a grandchild the
  ;; evaluation started would survive it. After `ready` the worker has a
  ;; session of its own, so the negative pid reaches every process in it.
  ;;
  ;; NOTE: AND THE ADAPTER IS CLOSED AFTERWARDS (limit-stop). Killing the
  ;; child ends the child; the adapter is what owns the pipes, and leaving it
  ;; alive would leave this process waiting for streams that will never say
  ;; anything again -- so it is closed once what the child wrote before it
  ;; died has been read, within a bound.
  (define sigkill 9)

  (define (signal-group! ref pgid)
    (when pgid (signal-pid! (- pgid) sigkill))
    (worker-kill! ref sigkill))

  ;; ---- the snapshot the worker evaluates against -----------------------------
  ;;
  ;; NEVER: TAKEN BEFORE THE CHILD EXISTS. The committed side is pinned by the
  ;; cut, which loads deterministically; the drafts are pinned by being
  ;; copied here. A commit landing while the evaluation runs changes
  ;; neither, which is what "the view is fixed at spawn" means.
  (define (view-datum writer cut drafts)
    (list 'working writer cut drafts))

  ;; ---- one evaluation --------------------------------------------------------

;; KEY: THIS MAILBOX IS THIS EVALUATION'S OWN. supervise-eval has one caller,
  ;; cli.sc's eval verb (cli.sc:411-423), which starts a fresh scheduler whose
  ;; only actor calls it once, prints the answer and exits; so every worker-*
  ;; message and every DOWN this process receives belongs to this evaluation.
  ;; `collect` has matched every worker-out without comparing its ref since
  ;; before F77a, and the limit drain does the same (review r6, F1). A second
  ;; caller, sharing an actor with other workers or monitors, would need both
  ;; to filter by ref -- which this `receive`, matching literal symbols and
  ;; bindings only, cannot express.
  (define (supervise-eval spec)
    (let ((store (spec-of spec 'store))
          (cut (spec-of spec 'cut))
          (under (spec-of spec 'under))
          (source (spec-of spec 'source))
          (timeout-ms (spec-of spec 'timeout-ms))
          (memory-bytes (spec-of spec 'memory-bytes))
          (output-bytes (spec-of spec 'output-bytes))
          (view (spec-of spec 'view)))
      ;; NOTE: THE CPU CEILING GOES IN argv, because the worker installs it
      ;; before it says ready and ready comes before anything we send.
      ;; It is the wall budget rounded up plus one second: a limit below
      ;; the wall clock would turn every slow-but-legal evaluation into a
      ;; CPU kill, and one far above it would never fire before the
      ;; supervisor's own deadline did.
      (let* ((cpu-seconds (+ 1 (div (+ timeout-ms 999) 1000)))
             (me self)
             (pid (spawn-worker! (spec-of spec 'scheme)
                                 (list "scheme" "--script" (spec-of spec 'worker)
                                       store cut under
                                       (number->string cpu-seconds)
                                       (number->string output-bytes))
                                 '()
                                 me)))
        ;; THE EVALUATION'S DEADLINE IS TAKEN AT READY, not here: what the
        ;; caller's timeout measures is the evaluation, and before ready
        ;; there is none.
        (wait-for-ready pid me
                        (lambda (ref pgid)
                          (run ref pgid me (+ (real-time) timeout-ms) source view
                               timeout-ms memory-bytes output-bytes))))))

  (define (spec-of spec key)
    (let ((e (assq key spec))) (and e (cdr e))))

  ;; NOTE: THE WORKER'S PATH COMES FROM THE CALLER. Resolving it here against
  ;; the working directory would work from the repository root and fail
  ;; everywhere else -- the caller is the one that knows where it was run
  ;; from.

  ;; NEVER: BEFORE READY WE MAY KILL ONLY THE pid. There is no group yet, and
  ;; there is also nothing to escape: the worker has read no source.
  (define (wait-for-ready pid me continue)
    (let ((ready-deadline (+ (real-time) ready-ms)))
      (let wait ((ref #f))
        (receive
          (after (max 0 (- ready-deadline (real-time)))
                 ;; NEVER: BY pid ONLY: there is no group yet, and there is
                 ;; nothing to escape either -- no source has been read.
                 (when ref (worker-kill! ref sigkill) (worker-close! ref))
                 '(error eval-worker-unavailable (reason no-ready)))
          (`(spawned ,r) (wait r))
          (`(worker-out ,r ,stream ,bv)
           (let ((datum (first-datum bv)))
             (if (and (pair? datum) (eq? 'ready (car datum)))
                 (continue r (cadr datum))
                 (wait r))))
          ;; NEVER: A REFUSED SPAWN ARRIVES AS THE ADAPTER'S DOWN, and it is
          ;; the only thing that arrives: no ref, no I/O, nothing to kill.
          (`#(DOWN ,who ,reason)
           (list 'error 'spawn-refused (list 'reason (reason-text reason))))))))

  (define (first-datum bv)
    (guard (e (#t #f)) (read (open-string-input-port (utf8->string bv)))))

  (define (reason-text reason)
    (cond ((and (condition? reason) (message-condition? reason)) (condition-message reason))
          ((pair? reason) reason)
          (else reason)))

  (define (run ref pgid me deadline source view timeout-ms memory-bytes output-bytes)
    (worker-write! ref (string->utf8 (render-wire 'go)) 'go)
    (worker-write! ref (string->utf8 (render-wire view)) 'view)
    (worker-write! ref (string->utf8 (render-wire (list 'source source))) 'source)
    (worker-close-stdin! ref)
    (collect ref pgid deadline "" "" "" 0 (+ (real-time) rss-interval-ms)
             memory-bytes output-bytes timeout-ms #f #f))

  ;; NOTE: THE QUOTA COUNTS DECODED USER BYTES, ACROSS BOTH STREAMS. What
  ;; travels is the framed text, where a backslash is two characters; the
  ;; promise is about what the user printed.
  ;; STDOUT-DONE says the worker's protocol stream has already ended, so a
  ;; limit that fires after it has nothing left to drain (review r7, N2).
  (define (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited stdout-done)
    (cond
      ((>= (real-time) deadline)
       (limit-stop ref pgid stdout-done protocol (limit-answer 'time timeout-ms out)))
      ;; NEVER: THE SAMPLE DEADLINE ADVANCES WHEN A SAMPLE IS TAKEN, NOT WHEN
      ;; THE POLL TICKS. Pushing it forward from the `after` branch --
      ;; which is what this did -- moves it 50ms into the future every
      ;; 25ms, so on a worker that says nothing it is never due and the
      ;; memory budget is never read at all. Such a worker ran to the
      ;; TIME limit instead, an answer that names the wrong resource and
      ;; arrives after the whole clock rather than at 50ms.
      ((>= (real-time) next-rss)
       (let ((rss (worker-rss ref)))
         (if (and rss (> rss memory-bytes))
             (limit-stop ref pgid stdout-done protocol (limit-answer 'memory memory-bytes out))
             (collect ref pgid deadline protocol out err used
                      (+ (real-time) rss-interval-ms)
                      memory-bytes output-bytes timeout-ms exited stdout-done))))
      (else
       (receive
         (after (min 25 (max 0 (- deadline (real-time))))
                (collect ref pgid deadline protocol out err used next-rss
                         memory-bytes output-bytes timeout-ms exited stdout-done))
         (`(worker-out ,r ,stream ,bv)
          (if (eq? stream 'stdout)
              (collect ref pgid deadline (string-append protocol (utf8->string bv))
                       out err used next-rss memory-bytes output-bytes timeout-ms exited stdout-done)
              (let-values (((o e n) (absorb-frames (utf8->string bv))))
                (let ((used (+ used n)))
                  (if (> used output-bytes)
                      (limit-stop ref pgid stdout-done protocol
                                  (limit-answer 'output output-bytes (string-append out o)))
                      (collect ref pgid deadline protocol
                               (string-append out o) (string-append err e) used
                               next-rss memory-bytes output-bytes timeout-ms exited stdout-done))))))
         (`(worker-eof ,r ,stream)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited
                   (or stdout-done (eq? stream 'stdout))))
         (`(worker-error ,r ,stream ,n)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited stdout-done))
         (`(worker-exit ,r ,status ,signal)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms (list status signal) stdout-done))
         ;; NEVER: THE ADAPTER'S DOWN IS THE END, and only then is the whole
         ;; story in: the exit AND both streams have been seen.
         (`#(DOWN ,who ,reason)
          (finish protocol out err exited))))))

;; NEVER: A LIMIT ANSWERS FROM THE WHOLE PROTOCOL STREAM, NOT FROM WHAT
  ;; HAPPENED TO HAVE BEEN RECEIVED WHEN IT FIRED (review r5). A note the
  ;; worker had already written could still be in this mailbox or in the
  ;; pipe; answering at once dropped it, and the limit answer did not name a
  ;; writer the evaluation's view could not see. So the group is signalled
  ;; but the adapter is not yet closed: the worker is dead and writes
  ;; nothing more, and its stdout is read until it ends (the stdout eof, or
  ;; the adapter's DOWN), for at most `drain-ms` of wall clock -- which is
  ;; what the drain can add to a limit answer's time. Only then is the
  ;; adapter closed and the answer built.
  ;;
  ;; NEVER: THE USER'S FIELDS ARE WHAT THEY WERE WHEN THE LIMIT FIRED. The
  ;; answer's (stdout ...) was taken then and is not added to: an output
  ;; limit must not carry bytes past its quota, and every limit answer's
  ;; user fields stay as they were before the drain existed. Frames on the
  ;; user's stream that arrive during the drain are read and discarded.
  (define drain-ms 500)

  ;; NEVER: A DRAIN THAT RAN OUT OF TIME DOES NOT PASS FOR ONE THAT READ THE
  ;; STREAM TO ITS END (review r6, F2). When the bound ends it before stdout's
  ;; eof or the adapter's DOWN, a note the worker wrote may still be unread,
  ;; so the answer may not name everything its view could not see; it then
  ;; ends with (protocol-cut (after-ms <bound>)). That is a clause of its own,
  ;; not an entry of (incomplete ...): incomplete lists writers the view could
  ;; not see, and its readers take its entries by shape; a stream cut short is
  ;; a different fact about the answer itself. A drain that reached the end
  ;; adds nothing.
  (define (limit-stop ref pgid stdout-done protocol answer)
    (signal-group! ref pgid)
    (let* ((drained (if stdout-done
                        (cons protocol #f)
                        (drain-protocol protocol (+ (real-time) drain-ms))))
           (whole (car drained))
           (cut? (cdr drained)))
      (worker-close! ref)
      (let ((a (with-clause answer whole)))
        (if cut?
            (append a (list (list 'protocol-cut (list 'after-ms drain-ms))))
            a))))

  ;; NEVER: THE BOUND IS CHECKED ON EVERY TURN, NOT LEFT TO `after` (review r6,
  ;; F3). This receive looks at its timeout only when no queued message
  ;; matches, so a mailbox full of the worker's frames kept the drain reading
  ;; past the bound (measured: 2582 ms against 500). Checked here first, at
  ;; most one message is handled after the bound has passed.
  ;; Answers (protocol . cut?): what was read, and whether the bound ended it.
  (define (drain-protocol protocol until)
    (let loop ((p protocol))
      (if (>= (real-time) until)
          (cons p #t)
          (receive
            (after (max 0 (- until (real-time))) (cons p #t))
            (`(worker-out ,r ,stream ,bv)
             (loop (if (eq? stream 'stdout) (string-append p (utf8->string bv)) p)))
            (`(worker-eof ,r ,stream)
             (if (eq? stream 'stdout) (cons p #f) (loop p)))
            (`(worker-error ,r ,stream ,n) (loop p))
            (`(worker-exit ,r ,status ,signal) (loop p))
            (`#(DOWN ,who ,reason) (cons p #f))))))

  (define (limit-answer resource limit out)
    (list 'error 'eval-limit (list 'resource resource) (list 'limit limit)
          (list 'stdout out)))

  ;; NEVER: A PROTOCOL LINE THAT NEVER FINISHED IS NOT AN ANSWER. The worker
  ;; writes its result with a terminating newline; anything else means the
  ;; child died with the answer half-written, and reporting the fragment
  ;; as a result would hand back something nobody produced.
  (define (finish protocol out err exited)
    (let ((datum (and (ends-with-newline? protocol) (protocol-answer protocol))))
      (with-clause
        (cond
          ((not datum)
           (list 'error 'eval-worker-exit
                 (list 'status (if (pair? exited) (car exited) 'unknown))))
          ((and (pair? datum) (eq? 'ok (car datum)))
           (append datum (list (list 'stdout out) (list 'stderr err))))
          (else datum))
        protocol)))

  ;; THE COMPLETE LINES OF THE PROTOCOL STREAM, AS DATUMS, in order. A line
  ;; that cannot be read back -- the worker wrote a value no reader accepts,
  ;; or died mid-line -- ends the list there: what came before it, notes
  ;; included, is still what the worker said.
  (define (protocol-datums text)
    (let ((port (open-string-input-port (complete-lines text))))
      (let loop ((acc '()))
        (let ((d (guard (e (#t (eof-object))) (read port))))
          (if (eof-object? d) (reverse acc) (loop (cons d acc)))))))

  (define (complete-lines text)
    (let loop ((i (string-length text)))
      (cond ((= i 0) "")
            ((char=? (string-ref text (- i 1)) #\newline) (substring text 0 i))
            (else (loop (- i 1))))))

  (define (note? d) (and (pair? d) (eq? 'incomplete (car d))))

  ;; The answer is the last datum that is neither a note nor the handshake;
  ;; #f when there is none. `(ready <pgid>)` is consumed by wait-for-ready
  ;; and the worker says nothing more until the source has been sent, so it
  ;; does not reach this text; it is named here so that no chunking of the
  ;; stream could make it an answer.
  (define (protocol-answer text)
    (let loop ((ds (protocol-datums text)) (answer #f))
      (cond ((null? ds) answer)
            ((note? (car ds)) (loop (cdr ds) answer))
            ((and (pair? (car ds)) (eq? 'ready (car (car ds)))) (loop (cdr ds) answer))
            (else (loop (cdr ds) (car ds))))))

  ;; The last note, which is the whole of what the worker heard; #f when none.
  (define (protocol-clause text)
    (let loop ((ds (protocol-datums text)) (clause #f))
      (cond ((null? ds) clause)
            ((note? (car ds)) (loop (cdr ds) (car ds)))
            (else (loop (cdr ds) clause)))))

  ;; NEVER: THE CLAUSE IS NOT LEFT OUT BECAUSE OF THE SHAPE OF THE ANSWER. An
  ;; answer that is not a proper list -- a raised `(error . x)` passed
  ;; through -- cannot have it appended, so it is wrapped as a raised value
  ;; and the clause follows. With no clause the answer is returned as it is.
  (define (with-clause answer protocol)
    (let ((clause (protocol-clause protocol)))
      (cond ((not clause) answer)
            ((list? answer) (append answer (list clause)))
            (else (list 'error 'eval-exception '(kind raised) (list 'value answer) clause)))))

  (define (ends-with-newline? text)
    (let ((n (string-length text)))
      (and (> n 0) (char=? (string-ref text (- n 1)) #\newline))))

  ;; NOTE: FRAMES ARRIVE IN PIECES AND IN ANY MIXTURE. Each complete line is
  ;; one datum; a partial tail is kept for the next chunk. NEVER: The count
  ;; returned is the DECODED length, which is what the quota is about.
  (define (absorb-frames text)
    (let loop ((rest text) (out "") (err "") (n 0))
      (let ((cut (newline-at rest)))
        (if (not cut)
            (values out err n)
            (let* ((line (substring rest 0 cut))
                   (tail (substring rest (+ cut 1) (string-length rest)))
                   (datum (guard (e (#t #f)) (read (open-string-input-port line)))))
              (cond
                ((and (pair? datum) (eq? 'out (car datum)) (string? (cadr datum)))
                 (loop tail (string-append out (cadr datum)) err
                       (+ n (string-length (cadr datum)))))
                ((and (pair? datum) (eq? 'err (car datum)) (string? (cadr datum)))
                 (loop tail out (string-append err (cadr datum))
                       (+ n (string-length (cadr datum)))))
                (else (loop tail out err n))))))))

  (define (newline-at text)
    (let loop ((i 0))
      (cond ((>= i (string-length text)) #f)
            ((char=? (string-ref text i) #\newline) i)
            (else (loop (+ i 1))))))
)
