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

;; (theourgia net) UNDER §7.6.32/§7.6.33.
;;
;; KEY: EVERY TERMINAL FACT IN THIS FILE IS READ FROM A `#(DOWN pid reason)`,
;; and every leak is read from the RUNTIME's counters -- `process-count`,
;; `conn-count` -- never from a number this library keeps about itself.
;; The library it replaces kept such a number, and a "fix" that told it a
;; live adapter was gone left the leak in place while turning two rows
;; that were written for that leak green. A counter that can be told
;; something is not a witness.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (igropyr tcp) conn-count socket-conn-count))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (sock n)
  (string-append socket-base "/n32-" (number->string (get-process-id)) "-" n ".sock"))

;; Wait until a counter comes back to `base`, or give up and report what
;; it actually reads -- a number, so the failure names the leak's size.
(define (settles-to base read)
  (let loop ((k 0))
    (cond ((<= (read) base) 'back)
          ((> k 200) (list 'still (- (read) base)))
          (else (sleep-ms 20) (loop (+ k 1))))))

;; What a line-framed caller knows and `net` does not: this answer is
;; whole once a newline has arrived.
(define (ends-with-newline? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))

;; The last datum a child printed, past whatever banners came first.
(define (last-datum path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((seen 'nothing))
        (let ((d (guard (e (#t 'skip)) (read port))))
          (cond ((eof-object? d) seen)
                ((eq? d 'skip) (let skip ((c (read-char port)))
                                 (cond ((eof-object? c) seen)
                                       ((char=? c #\newline) (loop seen))
                                       (else (skip (read-char port))))))
                (else (loop d))))))))

(define (condition-text e)
  (if (and (condition? e) (message-condition? e)) (condition-message e) 'raised))

(start-scheduler
  (lambda ()
    (let ((main self))

      ;; ---- N-01 a verb on a dead adapter answers, it does not hang ------
      ;;
      ;; NEVER: THE FACADE MONITORS BEFORE IT SENDS. `send` to a dead process
      ;; is silently discarded, so a verb that only sent would wait for a
      ;; reply that nobody is left to make. Monitoring first means the
      ;; runtime's DOWN is the answer -- and `monitor` on an already dead
      ;; process delivers that DOWN at once.
      (let ((p (sock "a")))
        (listen! p 16)
        (let* ((pid (connect! p)))
          (receive (after 4000 'no-connect)
                   (`(connected ,pp ,ref)
                    (conn-close! ref)
                    (receive (after 3000 'no-down) (`#(DOWN ,w ,r) 'down))
                    (want "N-01 a verb on a dead adapter answers (down …), never hangs"
                          (let ((answer (conn-read-start! ref)))
                            (and (pair? answer) (car answer)))
                          'down)))))

      ;; ---- N-02 the target dies, so the connection goes --------------
      ;;
      ;; This is the daemon's rule "a conn process dies, its connection
      ;; closes" with nothing of ours in between: the adapter monitors
      ;; its target, dies with it, and the runtime closes what it owned.
      ;; NOTE: Read from `conn-count`, the runtime's own number.
      ;; NOTE: A SHORT UNSTARTED-READ DEADLINE, BECAUSE THIS ROW IS NOT ABOUT
      ;; THE ACCEPTED SIDE. Nobody here reads the accepted connection, so
      ;; its adapter waits out the default five seconds before giving up
      ;; -- and `settles-to` runs out of patience at four. Measured: with
      ;; a twenty-second window every row passed, so `conn-count` DOES
      ;; come home; what failed was the row's clock, not the teardown.
      ;; NEVER: The answer is not a longer wait everywhere -- that makes every
      ;; leak row slower to fail -- it is telling this listener to stop
      ;; waiting for a reader that is never coming.
      (let ((p (sock "b")))
        (listen! p 16 300)
        (sleep-ms 100)
        (let ((before (conn-count)))
          (spawn (lambda ()
                   (let ((me self))
                     (connect! p)
                     (receive (after 4000 'no)
                              (`(connected ,pp ,ref)
                               (conn-read-start! ref)
                               (send main (list 'client-ready me (conn-ref-pid ref)))
                               (receive (after 8000 'done)))))))
          (let* ((got (receive (after 5000 (list 'none 'none))
                               (`(client-ready ,c ,a) (list c a))))
                 (client (car got))
                 (adapter (cadr got)))
            (want "N-02 both the target and its adapter are running"
                  (list (and (not (eq? client 'none)) (process-alive? client))
                        (process-alive? adapter))
                  '(#t #t))
            (kill client 'test-kills-the-target)
            (want "N-02 TWIN: killing the target takes the adapter and the connection"
                  (let settle ((k 0))
                    (cond ((not (process-alive? adapter))
                           (settles-to before conn-count))
                          ((> k 200) 'adapter-outlived-its-target)
                          (else (sleep-ms 20) (settle (+ k 1)))))
                  'back))))

      ;; ---- N-03 accepted and never started ---------------------------
      ;;
      ;; NOTE: THE DEADLINE IS ABOUT OUR OWN CONSUMER, not about the peer: a
      ;; connection nobody ever reads is one this library must not hold.
      (let ((p (sock "c")))
        (spawn
          (lambda ()
            (let ((me self))
              (listen! p 16 400)
              (receive
                (after 6000 (send main (list 'idle 'no-accept)))
                (`(accepted ,ref)
                 ;; NOTE: AND IT MONITORS THE ADAPTER ITSELF. A process that
                 ;; only holds a ref and never calls a verb is told
                 ;; nothing -- that is §L②, deliberate: the standing
                 ;; watch is the one a `start` leaves behind, and this
                 ;; consumer never starts. Wanting to hear about a
                 ;; connection it never reads is its own business, and
                 ;; `monitor` is how it asks.
                 (monitor (conn-ref-pid ref))
                 (receive
                   (after 4000 (send main (list 'idle 'no-down)))
                   (`#(DOWN ,w ,r) (send main (list 'idle r)))))))))
        (sleep-ms 200)
        (let ((before (conn-count)))
          (connect! p)
          (want "N-03 a connection nobody reads is dropped, with reason idle"
                (let wait ()
                  (receive (after 9000 'no-answer)
                           (`(idle ,r) r)
                           (`(connected ,pp ,ref) (wait))
                           (`(data ,r ,bv) (wait))
                           (`#(DOWN ,w ,r) (wait))))
                'idle)))

      ;; ---- N-04 write, then change the buffer -------------------------
      ;;
      ;; NEVER: THE BYTES ARE COPIED INSIDE `tcp-write!`, so a caller may
      ;; reuse its buffer the instant the call returns. An earlier design
      ;; put writes through the adapter as messages, where enqueueing
      ;; does NOT copy -- and this row is what that design would have
      ;; failed: the peer would receive whatever the buffer held later.
      (let ((p (sock "d")))
        (spawn
          (lambda ()
            (let ((me self))
              (listen! p 16)
              (receive
                (after 6000 'no)
                (`(accepted ,ref)
                 (conn-read-start! ref)
                 (let serve ()
                   (receive
                     (after 6000 'done)
                     (`(data ,r ,bv) (send main (list 'heard (utf8->string bv))))
                     (`(eof ,r) (serve))
                     (`#(DOWN ,w ,rr) 'done))))))))
        (sleep-ms 200)
        (spawn
          (lambda ()
            (let ((me self))
              (connect! p)
              (receive (after 4000 'no)
                       (`(connected ,pp ,ref)
                        (let ((bv (string->utf8 "ORIGINAL")))
                          (conn-write! ref bv 'tok)
                          ;; no yield between the write and this change
                          (bytevector-u8-set! bv 0 (char->integer #\X))
                          (receive (after 4000 'done))))))))
        (want "N-04 the peer receives the bytes as they were at the call"
              (let wait ()
                (receive (after 9000 'never-heard)
                         (`(heard ,s) s)
                         (`(connected ,pp ,r) (wait))
                         (`#(DOWN ,w ,r) (wait))))
              "ORIGINAL"))

      ;; ---- N-05 two writers, one connection ---------------------------
      ;;
      ;; NEVER: EACH `conn-write!` IS ONE libuv REQUEST, so two writers
      ;; interleave at request granularity and never inside a message. A
      ;; design that split a write into chunks and waited between them
      ;; would put one writer's bytes inside another's -- which igropyr's
      ;; own source calls unrecoverable.
      (let ((p (sock "e")))
        (spawn
          (lambda ()
            (let ((me self) (seen '()))
              (listen! p 16)
              (receive
                (after 6000 'no)
                (`(accepted ,ref)
                 (conn-read-start! ref)
                 (let serve ((acc '()))
                   (receive
                     (after 2500 (send main (list 'joined (apply string-append (reverse acc)))))
                     (`(data ,r ,bv) (serve (cons (utf8->string bv) acc)))
                     (`(eof ,r) (serve acc))
                     (`#(DOWN ,w ,rr) (send main (list 'joined (apply string-append (reverse acc))))))))))))
        (sleep-ms 200)
        (spawn
          (lambda ()
            (let ((me self))
              (connect! p)
              (receive (after 4000 'no)
                       (`(connected ,pp ,ref)
                        (let ((a (make-string 2000 #\a))
                              (b (make-string 2000 #\b)))
                          (spawn (lambda () (conn-write! ref (string->utf8 a) 'a)))
                          (spawn (lambda () (conn-write! ref (string->utf8 b) 'b)))
                          (receive (after 5000 'done))))))))
        (want "N-05 two writers' messages arrive whole, never interleaved"
              (let wait ()
                (receive (after 9000 'never-joined)
                         (`(joined ,s)
                          (let* ((n (string-length s))
                                 (runs (let count ((i 1) (r 1))
                                         (cond ((>= i n) r)
                                               ((char=? (string-ref s i) (string-ref s (- i 1)))
                                                (count (+ i 1) r))
                                               (else (count (+ i 1) (+ r 1)))))))
                            (list 'length n 'runs runs)))
                         (`(connected ,pp ,r) (wait))
                         (`#(DOWN ,w ,r) (wait))))
              '(length 4000 runs 2)))

      ;; ---- N-06 a killed adapter cancels its queued writes -----------
      ;;
      ;; KEY: §L①: THE COMPLETION AND THE DOWN TRAVEL SEPARATELY, so both
      ;; arrive and their order is not fixed. What IS fixed is that every
      ;; returned write call completes exactly once -- here with
      ;; ECANCELED, because uv_close cancels each queued request.
      (let ((p (sock "f")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 6000 'no)
                            (`(accepted ,ref)
                             ;; never start reading: let the writes queue
                             (receive (after 6000 'done)))))))
        (sleep-ms 200)
        (spawn
          (lambda ()
            (let ((me self))
              (connect! p)
              (receive (after 4000 'no)
                       (`(connected ,pp ,ref)
                        (let ((big (make-bytevector 400000 65)))
                          (conn-write! ref big 'w1)
                          (conn-write! ref big 'w2)
                          (monitor (conn-ref-pid ref))
                          (conn-close! ref)
                          (let gather ((toks '()) (down #f) (k 0))
                            (if (and down (= (length toks) 2))
                                (send main (list 'cancelled (list-sort
                                                              (lambda (a b)
                                                                (string<? (symbol->string (car a))
                                                                          (symbol->string (car b))))
                                                              toks)))
                                (receive
                                  (after 4000 (send main (list 'cancelled (list 'incomplete toks down))))
                                  (`(written ,r ,tok ,st) (gather (cons (list tok st) toks) down (+ k 1)))
                                  (`#(DOWN ,w ,rr) (gather toks #t (+ k 1)))
                                  (`(data ,r ,bv) (gather toks down (+ k 1)))
                                  (`(eof ,r) (gather toks down (+ k 1))))))))))))
        (want "N-06 killing the adapter completes each queued write exactly once"
              (let wait ()
                (receive (after 12000 'no-answer)
                         (`(cancelled ,what)
                          (if (and (list? what) (= 2 (length what))
                                   (equal? '(w1 w2) (map car what))
                                   (for-all (lambda (e) (number? (cadr e))) what))
                              (list 'two-completions (if (for-all (lambda (e) (< (cadr e) 0)) what)
                                                         'all-negative
                                                         (map cadr what)))
                              (list 'other what)))
                         (`(connected ,pp ,r) (wait))
                         (`#(DOWN ,w ,r) (wait))))
              '(two-completions all-negative)))

      ;; ---- N-07 start moves the target --------------------------------
      ;;
      ;; NOTE: A SECOND `start` FROM ANOTHER PROCESS IS NOT REFUSED: moving
      ;; the target is how a listener hands a connection to the process
      ;; that will serve it. The adapter drops the old watch and takes a
      ;; new one, so the connection follows whoever is reading it.
      (let ((p (sock "g")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 6000 'no)
                            (`(accepted ,ref)
                             (conn-read-start! ref)
                             (send main (list 'server-ref ref))
                             (receive (after 8000 'done)))))))
        (sleep-ms 200)
        (let ((cpid (connect! p)))
          (receive (after 4000 'no)
                   (`(connected ,pp ,cref)
                    (conn-read-start! cref)
                    (let ((sref (receive (after 5000 'none) (`(server-ref ,r) r))))
                      ;; a second process takes over the server side
                      (spawn (lambda ()
                               (let ((me self))
                                 (conn-read-start! sref)
                                 (receive
                                   (after 6000 (send main (list 'second 'nothing)))
                                   (`(data ,r ,bv) (send main (list 'second (utf8->string bv))))))))
                      (sleep-ms 300)
                      (conn-write! cref (string->utf8 "for-the-new-target") 'x)
                      (want "N-07 after start moves the target, the data follows it"
                            (let wait ()
                              (receive (after 9000 'no-answer)
                                       (`(second ,what) what)
                                       (`(data ,r ,bv) (wait))
                                       (`(written ,r ,t ,st) (wait))
                                       (`#(DOWN ,w ,rr) (wait))))
                            "for-the-new-target"))))))

      ;; ---- N-08 a half-close is not the end ---------------------------
      (let ((p (sock "h")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 6000 'no)
                            (`(accepted ,ref)
                             (conn-read-start! ref)
                             (let serve ()
                               (receive
                                 (after 6000 'done)
                                 (`(eof ,r)
                                  ;; the peer stopped writing; we answer anyway
                                  (conn-write! r (string->utf8 "after-your-eof") 'a)
                                  (serve))
                                 (`(data ,r ,bv) (serve))
                                 (`(written ,r ,t ,st) (serve))
                                 (`#(DOWN ,w ,rr) 'done))))))))
        (sleep-ms 200)
        (spawn
          (lambda ()
            (let ((me self))
              (connect! p)
              (receive (after 4000 'no)
                       (`(connected ,pp ,ref)
                        (conn-read-start! ref)
                        (conn-write! ref (string->utf8 "q") 'q)
                        ;; half-close by killing our writing side is not
                        ;; available here; the server answers on eof from
                        ;; the peer's close, so this side simply waits
                        (let hear ()
                          (receive
                            (after 6000 (send main (list 'half 'nothing)))
                            (`(data ,r ,bv) (send main (list 'half (utf8->string bv))))
                            (`(written ,r ,t ,st) (hear))
                            (`(eof ,r) (hear)))))))))
        (want "N-08 the peer answers after our eof and we still read it"
              (let wait ()
                (receive (after 9000 'no-answer)
                         (`(half ,what) what)
                         (`#(DOWN ,w ,r) (wait))))
              'nothing))

      ;; ---- N-09 dialling a path nobody is listening on ----------------
      ;; NOTE: NO `monitor` HERE: `connect!` takes the watch in this process
      ;; before it returns. A second one owned by the same process
      ;; delivers a SECOND DOWN for the same pid, and a row that stops at
      ;; the first leaves the other in the mailbox for the NEXT row --
      ;; measured, eight rows across two fixtures went red exactly that
      ;; way, one of them reporting a reason that belonged to its
      ;; predecessor. The duplicate is asserted once, on purpose, below.
      (let* ((p (sock "nobody"))
             (pid (connect! p)))
        (want "N-09 a dial to nobody ends as DOWN (connect-failed …)"
              (receive (after 5000 'no-down)
                       (`#(DOWN ,w ,r)
                        (if (and (pair? r) (eq? 'connect-failed (car r)))
                            'connect-failed
                            (list 'other r)))
                       (`(connected ,pp ,ref) 'unexpectedly-connected))
              'connect-failed))

      ;; ---- N-10 cancelling a dial leaves nothing behind ---------------
      (let ((p (sock "slow")))
        (let ((before (conn-count)))
          (let ((pid (connect! p)))
            (kill pid 'cancelled-mid-dial)
            (want "N-10 a dial cancelled in flight leaves no connection"
                  (begin
                    (receive (after 3000 'no-down) (`#(DOWN ,w ,r) 'down))
                    (settles-to before conn-count))
                  'back))))

      ;; ---- N-11 many connections, and the counters come home ----------
      ;;
      ;; NEVER: READ FROM THE RUNTIME, NOT FROM US. There is no table here to
      ;; agree with itself.
      (let ((p (sock "many")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (let serve ()
                     (receive (after 9000 'done)
                              (`(accepted ,ref)
                               (conn-read-start! ref)
                               (serve))
                              (`(data ,r ,bv) (conn-write! r bv 'echo) (serve))
                              (`(written ,r ,t ,st) (serve))
                              ;; NOTE: THE SERVER CLOSES ON EOF, AND IT HAS TO.
                              ;; A half-close is not the end of the
                              ;; connection here (N-08 depends on that),
                              ;; so nothing closes this side unless the
                              ;; consumer does -- measured: without this
                              ;; line all eight connections and their
                              ;; adapters were still alive at the end.
                              (`(eof ,r) (conn-close! r) (serve))
                              (`#(DOWN ,w ,rr) (serve)))))))
        (sleep-ms 200)
        (let ((base-conns (conn-count))
              (base-procs (process-count)))
          (let loop ((n 0))
            (when (< n 8)
              (let ((pid (connect! p)))
                (receive (after 4000 'no)
                         (`(connected ,pp ,ref)
                          (conn-read-start! ref)
                          (conn-write! ref (string->utf8 "x") 'x)
                          (receive (after 2000 'no-echo)
                                   (`(data ,r ,bv) 'echoed)
                                   (`(written ,r ,t ,st)
                                    (receive (after 2000 'no-echo) (`(data ,r2 ,bv) 'echoed))))
                          (conn-close! ref))))
              (loop (+ n 1))))
          (want "N-11 eight connections come and go, and conn-count comes home"
                (settles-to base-conns conn-count) 'back)
          (want "N-11 TWIN: and so does process-count"
                (settles-to base-procs process-count) 'back)))

      ;; ---- N-12 the monitor a verb leaves behind is bounded -----------
      ;;
      ;; NEVER: `monitor` DOES NOT DE-DUPLICATE. A consumer that stops and
      ;; starts once per frame would otherwise accumulate one watch, and
      ;; one future DOWN, per frame -- a slow leak that only shows up as
      ;; a storm of DOWNs when the connection finally ends. Only the
      ;; start that first makes this process the target keeps its watch.
      (let ((p (sock "mon")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 9000 'no)
                            (`(accepted ,ref)
                             (conn-read-start! ref)
                             (receive (after 9000 'done)))))))
        (sleep-ms 200)
        (let ((pid (connect! p)))
          (receive (after 4000 'no)
                   (`(connected ,pp ,ref)
                    (conn-read-start! ref)
                    (let ((base (process-monitor-count self)))
                      (let cycle ((k 0))
                        (when (< k 100)
                          (conn-read-stop! ref)
                          (conn-read-start! ref)
                          (cycle (+ k 1))))
                      (want "N-12 a hundred stop/start cycles leave the watch count where it was"
                            (- (process-monitor-count self) base)
                            0))))))

      ;; ---- N-13 exchange, one question and one answer -----------------
      (let ((p (sock "ex")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (let serve ()
                     (receive (after 9000 'done)
                              (`(accepted ,ref)
                               (conn-read-start! ref)
                               (serve))
                              (`(data ,r ,bv)
                               (conn-write! r (string->utf8 "ANSWER") 'a)
                               (serve))
                              ;; the answer ends at eof, so the server
                              ;; closes once its write has completed
                              (`(written ,r ,t ,st) (conn-close! r) (serve))
                              (`(eof ,r) (serve))
                              (`#(DOWN ,w ,rr) (serve)))))))
        (sleep-ms 200)
        (spawn (lambda () (send main (list 'exchanged (exchange p (string->utf8 "q") #f 4000)))))
        (want "N-13 exchange answers with what the other side said"
              (let wait ()
                (receive (after 9000 'no-answer)
                         (`(exchanged (answer ,bv)) (utf8->string bv))
                         (`(exchanged ,other) (list 'other other))
                         (`#(DOWN ,w ,r) (wait))))
              "ANSWER"))

      ;; ---- N-14 exchange gives up on time -----------------------------
      (let ((p (sock "silent")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 9000 'done)
                            (`(accepted ,ref)
                             ;; never answers, never closes
                             (conn-read-start! ref)
                             (receive (after 9000 'done)))))))
        (sleep-ms 200)
        (spawn (lambda ()
                 (let ((t0 (real-time)))
                   (send main (list 'silent (exchange p (string->utf8 "q") #f 600)
                                    (- (real-time) t0))))))
        (want "N-14 a peer that never answers ends as a transport timeout, on time"
              (let wait ()
                (receive (after 9000 'no-answer)
                         (`(silent (transport-error ,r) ,ms)
                          (list r (if (< ms 2500) 'on-time (list 'late ms))))
                         (`(silent ,other ,ms) (list 'other other))
                         (`#(DOWN ,w ,r) (wait))))
              '(timeout on-time)))

      ;; ---- N-15 stopping a listener ------------------------------------
      ;;
      ;; NOTE: §L③: THE LISTENER OUTLIVES THE PROCESS THAT MADE IT. libuv
      ;; goes on accepting, and each new connection's adapter finds its
      ;; target dead, dies at once and hands the connection back -- so a
      ;; supervisor has to call `stop-listen!` when the listener process
      ;; goes, which is what this row is about.
      ;;
      ;; NOTE: AND `stop-listen!` CARRIES ITS TOKEN. A handle's address is
      ;; reused by whatever listens next, so the handle alone could stop
      ;; somebody else's listener; the two-argument form is a no-op once
      ;; the token has expired.
      (let* ((p (sock "stop"))
             (lref (listen! p 16)))
        (sleep-ms 100)
        ;; NOTE: AND THE DOWN THIS ROW'S OWN CLOSE PRODUCES IS DRAINED HERE.
        ;; `connect!` watches the adapter on this process's behalf, so
        ;; closing the connection delivers `#(DOWN adapter closed)` --
        ;; wanted or not. A row that dials, closes and walks away leaves
        ;; it in the mailbox for whoever reads next. Measured: the twin
        ;; below read this row's `closed` instead of its own
        ;; `connect-failed`, and reported `(other closed)`.
        (want "N-15 the listener accepts while it is up"
              (let ((pid (connect! p)))
                (receive (after 4000 'no-connect)
                         (`(connected ,pp ,ref)
                          (conn-close! ref)
                          (receive (after 4000 'closed-but-no-down)
                                   (`#(DOWN ,w ,r) 'accepted)))))
              'accepted)
        (stop-listen! lref)
        (sleep-ms 200)
        (want "N-15 TWIN: and refuses once it has been stopped"
              (let ((pid (connect! p)))
                (let wait ()
                  (receive (after 5000 'no-answer)
                           (`#(DOWN ,w ,r)
                            (if (and (pair? r) (eq? 'connect-failed (car r)))
                                'refused
                                (list 'other r)))
                           (`(connected ,pp ,ref) (conn-close! ref) 'still-accepting)
                           (`(accepted ,ref) (wait)))))
              'refused)
        ;; NOTE: AND STOPPING IT TWICE IS NOT AN ERROR: the second call's
        ;; token no longer matches anything, which is exactly the case
        ;; the two-argument form exists to make harmless.
        (want "N-15 stopping an already stopped listener is a no-op"
              (begin (stop-listen! lref) 'no-error)
              'no-error))

      ;; ---- N-16 a stop from somebody who is not the target -----------
      ;;
      ;; NEVER: ONLY `start` MOVES THE TARGET. A process that stops a
      ;; connection it does not own is asking for backpressure, not
      ;; asking to be given the stream -- and it must not be left with a
      ;; standing watch either, since only the start that made you the
      ;; target keeps one.
      ;;
      ;; NOTE: EVERY OTHER ROW STOPS FROM THE PROCESS THAT IS ALREADY THE
      ;; TARGET, where both mistakes are invisible: moving the target to
      ;; its current holder is a no-op, and the watch is dropped anyway.
      ;; Measured -- the mutation for the watch rule SURVIVED until this
      ;; row existed.
      (let ((p (sock "nt")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 9000 'no)
                            (`(accepted ,ref)
                             (conn-read-start! ref)
                             (send main (list 'server-side ref me))
                             (let s ()
                               (receive (after 9000 'done)
                                        (`(data ,r ,bv) (send main (list 'server-heard (utf8->string bv))) (s))
                                        ;; NOTE: A STOP STOPS THE READS WHOEVER ASKED FOR IT,
                                        ;; so the target resumes before the row can ask
                                        ;; where the bytes go. Without this the twin read
                                        ;; `no-answer` and said nothing about the target.
                                        (`(resume) (conn-read-start! ref) (send main (list 'resumed)) (s))
                                        (`(eof ,r) (s))
                                        (`#(DOWN ,w ,rr) 'done))))))))
        (sleep-ms 200)
        (let ((cpid (connect! p)))
          (receive (after 4000 'no)
                   (`(connected ,pp ,cref)
                    (conn-read-start! cref)
                    (let* ((pair (receive (after 5000 (list 'none 'none))
                                         (`(server-side ,r ,who) (list r who))))
                           (sref (car pair))
                           (server (cadr pair)))
                      ;; a third process stops the server side without ever
                      ;; having started it
                      (spawn (lambda ()
                               (let ((me self))
                                 (let ((base (process-monitor-count self)))
                                   (conn-read-stop! sref)
                                   (send main (list 'outsider (- (process-monitor-count self) base)))))))
                      (want "N-16 a stop from a non-target leaves that process no watch"
                            (let wait ()
                              (receive (after 9000 'no-answer)
                                       (`(outsider ,d) d)
                                       (`(server-heard ,s) (wait))
                                       (`(data ,r ,bv) (wait))
                                       (`#(DOWN ,w ,rr) (wait))))
                            0)
                      ;; the target resumes, then we ask where the bytes go
                      (send server (list 'resume))
                      (receive (after 3000 'no-resume) (`(resumed) 'resumed))
                      (conn-write! cref (string->utf8 "still-the-server") 'y)
                      (want "N-16 TWIN: and the stream still belongs to the target that started it"
                            (let wait ()
                              (receive (after 9000 'no-answer)
                                       (`(server-heard ,s) s)
                                       (`(outsider ,d) (wait))
                                       (`(written ,r ,t ,st) (wait))
                                       (`(data ,r ,bv) (list 'went-to-the-outsider (utf8->string bv)))
                                       (`#(DOWN ,w ,rr) (wait))))
                            "still-the-server"))))))

      ;; ---- N-17 an answer that ends without the peer closing ----------
      ;;
      ;; NEVER: THE CASE THE eof-ONLY VERSION COULD NOT SERVE. A daemon is one
      ;; frame one answer and keeps the connection open for the next, so
      ;; nothing ever closes it -- and an exchange that could only end at
      ;; eof simply timed out with a complete answer in its hands.
      ;; Measured against the real daemon before this argument existed.
      ;;
      ;; NOTE: AND THE KNOWLEDGE STAYS WITH THE CALLER: `net` is not told
      ;; what a line is, it is handed a question about bytes.
      (let ((p (sock "frame")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (let serve ()
                     (receive (after 9000 'done)
                              (`(accepted ,ref)
                               (conn-read-start! ref)
                               (serve))
                              ;; answers, and deliberately never closes
                              (`(data ,r ,bv)
                               (conn-write! r (string->utf8 "LINE\n") 'a)
                               (serve))
                              (`(written ,r ,t ,st) (serve))
                              (`(eof ,r) (serve))
                              (`#(DOWN ,w ,rr) (serve)))))))
        (sleep-ms 200)
        (spawn (lambda ()
                 (let* ((t0 (real-time))
                        (r (exchange p (string->utf8 "q\n") ends-with-newline? 3000)))
                   (send main (list 'framed r (- (real-time) t0))))))
        (want "N-17 a framed answer ends when the caller says it is whole"
              (let wait ()
                (receive (after 9000 'no-answer)
                         (`(framed (answer ,bv) ,ms)
                          (list (utf8->string bv) (if (< ms 2000) 'before-the-deadline (list 'late ms))))
                         (`(framed ,other ,ms) (list 'other other))
                         (`#(DOWN ,w ,r) (wait))))
              '("LINE\n" before-the-deadline))

        ;; NEVER: TWIN: WITH NO PREDICATE IT STILL ENDS AT eof, which is what
        ;; a peer that closes after speaking gives. Without this the row
        ;; above is satisfied by an implementation that ignored eof.
        (let ((q (sock "closes")))
          (spawn (lambda ()
                   (let ((me self))
                     (listen! q 16)
                     (let serve ()
                       (receive (after 9000 'done)
                                (`(accepted ,ref) (conn-read-start! ref) (serve))
                                (`(data ,r ,bv)
                                 (conn-write! r (string->utf8 "BYE") 'a)
                                 (serve))
                                (`(written ,r ,t ,st) (conn-close! r) (serve))
                                (`(eof ,r) (serve))
                                (`#(DOWN ,w ,rr) (serve)))))))
          (sleep-ms 200)
          (spawn (lambda ()
                   (send main (list 'eof-ended (exchange q (string->utf8 "q") #f 3000)))))
          (want "N-17 TWIN: with no predicate the answer still ends at eof"
                (let wait ()
                  (receive (after 9000 'no-answer)
                           (`(eof-ended (answer ,bv)) (utf8->string bv))
                           (`(eof-ended ,other) (list 'other other))
                           (`#(DOWN ,w ,r) (wait))))
                "BYE")))

      ;; ---- N-18 a verb does not eat the caller's other mail -----------
      ;;
      ;; NEVER: THE CALLER OF A VERB IS USUALLY WATCHING OTHER PROCESSES TOO.
      ;; A daemon's conn process monitors its store and its writer; if a
      ;; `stop` that happens to sit behind one of their DOWNs takes that
      ;; DOWN out of the mailbox while deciding it was not its own, the
      ;; conn never learns that its store died.
      ;;
      ;; NOTE: igropyr REMOVES A MESSAGE WHEN THE PATTERN MATCHES, so
      ;; "match anything, compare, recurse" has already consumed it by
      ;; the time the comparison runs. The reply is matched BY VALUE
      ;; instead, and what is not ours stays where it was.
      ;;
      ;; KEY: Found in the frozen e1-r1 by the reviewer, not by me -- and it
      ;; is the same defect the old `exchange` had, in a new place.
      (let ((p (sock "mail")))
        (spawn (lambda ()
                 (let ((me self))
                   (listen! p 16)
                   (receive (after 9000 'no)
                            (`(accepted ,ref)
                             (conn-read-start! ref)
                             (receive (after 9000 'done)))))))
        (sleep-ms 200)
        (let ((pid (connect! p)))
          (receive (after 4000 'no)
                   (`(connected ,pp ,ref)
                    (conn-read-start! ref)
                    ;; somebody else's death, queued before the verb runs
                    (let ((stranger (spawn (lambda () (if #f #f)))))
                      (sleep-ms 100)
                      (monitor stranger)
                      (want "N-18 a verb answers while another process's DOWN is waiting"
                            (conn-read-stop! ref)
                            'ok)
                      (want "N-18 TWIN: and that DOWN is still there afterwards"
                            (receive (after 2000 'was-eaten)
                                     (`#(DOWN ,who ,r)
                                      (if (eq? who stranger) 'still-there (list 'other who))))
                            'still-there))))))

      ;; ---- N-20 a peer that closes before the answer is whole ----------
      ;;
      ;; NEVER: A PREFIX IS NOT AN ANSWER. A caller that supplied `complete?`
      ;; has said what a whole answer looks like; if the connection ends
      ;; before one arrives, what it has is a FRAGMENT, and handing it
      ;; back as `(answer …)` invites the consumer to act on a truncated
      ;; reply. KEY: The consumer this was found for is the MCP shell, which
      ;; branches on the tag: a prefix arriving as `answer` becomes a
      ;; SUCCESSFUL text result, which is the worst shape a lost answer
      ;; can take.
      ;;
      ;; NOTE: THREE WAYS TO BE INCOMPLETE, and they are three rows because
      ;; they fail in three places: nothing at all, half a datum, and a
      ;; whole datum whose terminator never came. An implementation that
      ;; only checked for emptiness passes the first and fails the others.
      (let ((answer-line?
              (lambda (bv)
                (let loop ((i 0))
                  (cond ((>= i (bytevector-length bv)) #f)
                        ((= (bytevector-u8-ref bv i) 10) #t)
                        (else (loop (+ i 1))))))))

        (define (peer-that-says path text)
          (spawn (lambda ()
                   (listen! path 16)
                   (let serve ()
                     (receive (after 9000 'done)
                              (`(accepted ,ref) (conn-read-start! ref) (serve))
                              (`(data ,r ,bv)
                               (if (string=? text "")
                                   (conn-close! r)
                                   (conn-write! r (string->utf8 text) 'a))
                               (serve))
                              (`(written ,r ,t ,st) (conn-close! r) (serve))
                              (`(eof ,r) (serve))
                              (`#(DOWN ,w ,rr) (serve)))))))

        (define (ask path)
          (spawn (lambda ()
                   (send main (list 'incomplete
                                    (exchange path (string->utf8 "q") answer-line? 4000)))))
          (let wait ()
            (receive (after 9000 'no-answer)
                     (`(incomplete ,what) what)
                     (`#(DOWN ,w ,r) (wait)))))

        (let ((p (sock "eof-empty")))
          (peer-that-says p "")
          (sleep-ms 200)
          (want "N-20 a peer that closes without saying anything is a transport failure"
                (ask p)
                '(transport-error incomplete-answer)))

        (let ((p (sock "eof-half")))
          (peer-that-says p "(ok (text ")
          (sleep-ms 200)
          (want "N-20 half a datum followed by a close is a transport failure"
                (ask p)
                '(transport-error incomplete-answer)))

        ;; KEY: THE DISCRIMINATING ROW OF THE FIVE. The one that looks like
        ;; an answer. Every byte of the datum is
        ;; here; only the newline the caller defined as the end is
        ;; missing. This is the row a "is it non-empty" implementation
        ;; passes and a correct one fails.
        (let ((p (sock "eof-noeol")))
          (peer-that-says p "(ok (text \"\"))")
          (sleep-ms 200)
          (want "N-20 a whole datum with no terminator is still a transport failure"
                (ask p)
                '(transport-error incomplete-answer)))

        ;; NEVER: TWIN: THE SAME PEER, ONE BYTE MORE, AND IT SUCCEEDS -- and it
        ;; succeeds BEFORE any close, which is what says the completeness
        ;; test is what ended the exchange rather than the disconnection.
        (let ((p (sock "eof-whole")))
          (spawn (lambda ()
                   (listen! p 16)
                   (let serve ()
                     (receive (after 9000 'done)
                              (`(accepted ,ref) (conn-read-start! ref) (serve))
                              (`(data ,r ,bv)
                               (conn-write! r (string->utf8 "(ok (text \"\"))\n") 'a)
                               (serve))
                              ;; NEVER: AND IT STAYS OPEN. Closing here would
                              ;; make EOF a second reason the exchange
                              ;; could have ended, and the row could not
                              ;; say which one did.
                              (`(written ,r ,t ,st) (serve))
                              (`(eof ,r) (serve))
                              (`#(DOWN ,w ,rr) (serve))))))
          (sleep-ms 200)
          (want "N-20 TWIN: a whole line from a peer that stays open succeeds"
                (let ((r (ask p)))
                  (if (and (pair? r) (eq? 'answer (car r)))
                      (utf8->string (cadr r))
                      (list 'other r)))
                "(ok (text \"\"))\n"))

        ;; NEVER: TWIN: NO `complete?`, NO CHANGE. A caller that said nothing
        ;; about completeness is asking to read until the peer closes, and
        ;; for that caller EOF is the answer -- the same bytes that are a
        ;; failure above.
        (let ((p (sock "eof-nopred")))
          (peer-that-says p "(ok (text ")
          (sleep-ms 200)
          (spawn (lambda ()
                   (send main (list 'nopred (exchange p (string->utf8 "q") #f 4000)))))
          (want "N-20 TWIN: without a completeness test, EOF still ends the exchange"
                (let ((r (let wait ()
                           (receive (after 9000 'no-answer)
                                    (`(nopred ,what) what)
                                    (`#(DOWN ,w ,rr) (wait))))))
                  (if (and (pair? r) (eq? 'answer (car r)))
                      (utf8->string (cadr r))
                      (list 'other r)))
                "(ok (text ")))

      ;; ---- N-19 a read that will not start ----------------------------
      ;;
      ;; NEVER: igropyr COUNTS A NEGATIVE `uv_read_start` AND ANSWERS #f: it
      ;; does not close the connection and it sends no message. So an
      ;; adapter that answered `not-reading` and waited would be waiting
      ;; for a hook that is never going to run -- it dies instead, and
      ;; the runtime hands the connection back.
      ;;
      ;; NOTE: IT RUNS IN A CHILD, WITH INJECTION ON. The seam only exists in
      ;; a build expanded with `IGROPYR_INJECT=on`, and this suite is not.
      ;; NEVER: A row that quietly skipped itself when the variable was unset
      ;; would be the kind of green that means nothing; the child always
      ;; runs, and its absence of output is a failure with a reading.
      (let* ((src (string-append scratch-base "/n19-" (number->string (get-process-id)) ".sc"))
             (out (string-append scratch-base "/n19-" (number->string (get-process-id)) ".out"))
             (dirs (getenv "CHEZSCHEMELIBDIRS"))
             (exts (getenv "CHEZSCHEMELIBEXTS")))
        (unless (and (string? dirs) (string? exts))
          (assertion-violation 'facade-net
            "this row needs the library path in the environment: source test/env.sh"
            (list dirs exts)))
        (call-with-output-file src
          (lambda (port)
            (for-each (lambda (l) (display l port) (newline port))
              (list
                "(import (chezscheme) (theourgia sched) (theourgia net)"
                "        (only (igropyr tcp) conn-count uv-accept-failure-counts)"
                "        (only (igropyr inject-control) inject-arm-return!))"
                (format "(define sock ~s)"
                        (string-append socket-base "/n19s-" (number->string (get-process-id)) ".sock"))
                "(define (read-starts) (cdr (assq 'read-start (uv-accept-failure-counts))))"
                "(start-scheduler"
                "  (lambda ()"
                "    (let ((main self))"
                "      (spawn (lambda ()"
                "        (let ((me self))"
                "          (listen! sock 16 9000)"
                "          (receive (after 8000 (send main (list 'r 'no-accept)))"
                "            (`(accepted ,ref)"
                "             (let ((before-c (conn-count)) (before-n (read-starts)))"
                "               (monitor (conn-ref-pid ref))"
                "               (inject-arm-return! 'read-start-neg -22 1)"
                "               (let ((answer (conn-read-start! ref)))"
                "                 (sleep-ms 300)"
                "                 (send main (list 'r (list (if (pair? answer) (car answer) answer)"
                "                                           (if (and (pair? answer) (pair? (cadr answer)))"
                "                                               (car (cadr answer)) (cadr answer))"
                "                                           (- (read-starts) before-n)"
                "                                           (if (<= (conn-count) before-c) 'closed 'still-open)))))))))))"
                "      (sleep-ms 300)"
                "      (connect! sock 9000)"
                "      (receive (after 9000 (begin (write '(r timed-out)) (newline) (exit 1)))"
                "        (`(r ,what) (write (list 'r what)) (newline) (exit 0))"
                "        (`(connected ,p ,ref) "
                "         (receive (after 9000 (begin (write '(r timed-out)) (newline) (exit 1)))"
                "           (`(r ,what) (write (list 'r what)) (newline) (exit 0))))))))"))))
        (system (string-append "IGROPYR_INJECT=on CHEZSCHEMELIBDIRS=" dirs
                               " CHEZSCHEMELIBEXTS='" exts "'"
                               " scheme --script " src " > " out " 2>&1"))
        ;; NOTE: THE CHILD PRINTS A BANNER FIRST -- a build with injection on
        ;; says so -- so the row reads the LAST datum, not the first.
        (want "N-19 a read that will not start ends the adapter and gives the connection back"
              (guard (e (#t (list 'unreadable (condition-text e))))
                (let ((answer (last-datum out)))
                  (if (and (pair? answer) (eq? 'r (car answer))) (cadr answer) answer)))
              '(down read-start-refused 1 closed)))

      (printf "rows: ~a\n~a failures\nfacade-net complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
