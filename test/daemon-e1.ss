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

;; `theourgia serve` -- the daemon, over a real unix socket.
;;
;; ⭐ EVERY ROW HERE TALKS TO A DAEMON IN ANOTHER OS PROCESS, over the
;; socket, exactly as a client does. A row that called the daemon's own
;; procedures in this process would be testing the library, not the
;; thing a client meets.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia ffi) lock-try-acquire! lock-release!)
        (only (theourgia working) draft-lock-path))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define pid-text (number->string (get-process-id)))
(define store (string-append "/tmp/dmn-store-" pid-text))
(define socket (string-append "/tmp/dmn-" pid-text ".sock"))
(define runner (string-append "/tmp/dmn-run-" pid-text ".ss"))
(define log (string-append "/tmp/dmn-log-" pid-text ".txt"))

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))

;; ⚠️ THE DAEMON IS A SEPARATE OS PROCESS, started the way a user starts
;; it. Running it inside this scheduler would share a scheduler with its
;; clients, and every timing row would be measuring the wrong thing.
(define (start-daemon!)
  (system (string-append "rm -rf " store " " socket "; mkdir -p " store))
  (call-with-output-file runner
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
              (string-append "(rpc-dispatch \"" store "\" '(init) \"tester\")")
              (string-append "(serve \"" store "\" \"" socket "\")")))))
  (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                         " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                         " scheme --script " runner " > " log " 2>&1 &"))
  ;; wait for the socket to appear rather than sleeping a guess
  (let wait ((k 0))
    (cond ((file-exists? socket) 'up)
          ((> k 100) 'never-came-up)
          (else (sleep-ms 50) (wait (+ k 1))))))

(define (stop-daemon!)
  (system (string-append "pkill -f " runner " 2>/dev/null")))

;; ⚠️ COUNTED PREFIXES ARE A TRAP: `(substring text 0 30)` against a
;; thirty-one character tag compares the wrong thing and the row reports
;; the answer as "other" while the answer was right. Measured, on the
;; store-mismatch row.
(define (starts-with? text prefix)
  (let ((n (string-length prefix)))
    (and (>= (string-length text) n)
         (string=? (substring text 0 n) prefix))))

;; ⛔ AN EMPTY FILE READS AS #!eof, NOT AS "". Measured: the lock rows
;; look at a holder's log the instant it is created, `get-string-all`
;; handed back the eof object, and `string-length` inside the first
;; comparison took the WHOLE fixture down with a boot panic -- after
;; thirteen rows had already passed. A missing file is not a failure
;; here; it is "nothing has been written yet".
(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((text (call-with-input-file path get-string-all)))
        (if (string? text) text ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; Starts a daemon of its own, asks it one question, and answers with
;; (trace-text . what-the-client-got). `fault` arms our conn seam; #f is
;; the same build unarmed.
;;
;; ⚠️ BOTH RUNS USE `THEOURGIA_INJECT=on`, so the twin is the same
;; expansion branch as the armed one -- otherwise it would be testing a
;; different build and could not say anything about this one.
;; A fault spec with the punctuation taken out, for use in a filename.
(define (safe-name text)
  (list->string
    (map (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\-))
         (string->list text))))

(define (start-tagged-daemon! suffix fault)
  ;; ⚠️ THE TAG CARRIES THE FAULT, so two armed runs do not collide on
  ;; one runner file -- `call-with-output-file` refuses an existing one,
  ;; and that refusal killed the whole fixture rather than one row.
  (let* ((tag (string-append pid-text "-" suffix))
         (st (string-append "/tmp/dmn-c" tag))
         (sk (string-append "/tmp/dmn-c" tag ".sock"))
         (rn (string-append "/tmp/dmn-c" tag ".ss"))
         (lg (string-append "/tmp/dmn-c" tag ".log")))
    (system (string-append "rm -rf " st " " sk "; mkdir -p " st))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
                (string-append "(rpc-dispatch \"" st "\" '(init) \"tester\")")
                (string-append "(serve \"" st "\" \"" sk "\")")))))
    ;; ⛔ TRACING IS TURNED ON BY THE ENVIRONMENT, NOT BY A CALL. The
    ;; platform layer calls `trace-enable!` from THEOURGIA_TRACE when it
    ;; LOADS, so a `(trace-enable! #t)` written before the libraries are
    ;; loaded is overwritten by that initialisation -- measured: the flag
    ;; read #t in the runner and #f inside the scheduler, and every trace
    ;; call then wrote nothing while returning perfectly normally.
    ;;
    ;; ⚠️ EVERY RUN USES `THEOURGIA_INJECT=on`, armed or not, so a twin is
    ;; the same expansion branch as the row it is a twin of -- otherwise
    ;; it would be testing a different build and could say nothing about
    ;; this one.
    (system (string-append "THEOURGIA_TRACE=1 THEOURGIA_INJECT=on "
                           (if fault (string-append "THEOURGIA_FAULT=" fault " ") "")
                           "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script " rn " > " lg " 2>&1 &"))
    (let up ((k 0))
      (cond ((file-exists? sk) 'up)
            ((> k 120) 'never) (else (sleep-ms 50) (up (+ k 1)))))
    (list st sk rn lg)))

(define (tagged-store d) (car d))
(define (tagged-socket d) (cadr d))
(define (tagged-log d) (file-text (cadddr d)))
(define (stop-tagged-daemon! d)
  (system (string-append "pkill -f " (caddr d) " 2>/dev/null")))

;; One request to a daemon started above, with the answer's text and the
;; time it took. ⚠️ The time includes connecting, because that is what a
;; client waits: a row that timed only the dispatch would be measuring
;; something no client can observe.
(define (ask-tagged d text ms)
  (let* ((t0 (real-time))
         (r (exchange (tagged-socket d)
                      (string->utf8 (string-append "(request \"" (tagged-store d) "\" \"tester\" " text ")\n"))
                      line-complete? ms)))
    (list (- (real-time) t0)
          (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
              (utf8->string (cadr r))
              'no-answer))))

;; Starts a daemon of its own, asks it one question, and answers with
;; (trace-text . what-the-client-got). `fault` arms our conn seam; #f is
;; the same build unarmed.
;; ⚠️ THE VERB IS THE CALLER'S, because which process serves it is now
;; part of what a row is asking about. `outline` is answered by the
;; connection's own process from the published value and never reaches
;; the store -- so a row that has to make the STORE process do something
;; must send something the store process serves. Measured: with `outline`
;; hard-coded here, the row that arms a raise inside the store process
;; armed it and then never went near it, and read as "the daemon did not
;; die" -- which is true, and about nothing.
(define (run-daemon-with fault request-text)
  (let* ((d (start-tagged-daemon! (if fault (safe-name fault) "quiet") fault))
         (r (exchange (tagged-socket d)
                      (string->utf8 (string-append "(request \"" (tagged-store d) "\" \"tester\" "
                                                   request-text ")\n"))
                      line-complete? 3000))
         (got (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
                  (utf8->string (cadr r))
                  'no-answer)))
    (sleep-ms 600)
    (let ((still-there (file-exists? (tagged-socket d))))
      (stop-tagged-daemon! d)
      (list (tagged-log d) got
            (if still-there 'socket-still-there 'socket-gone)))))

;; ⚠️ THE LOCK IS HELD BY ANOTHER OS PROCESS, because that is the only
;; thing the daemon can actually meet: a holder inside this scheduler
;; would be competing with the rows for the same green threads, and a
;; holder inside the daemon would not be somebody else at all.
(define (hold-lock! path secs tag)
  (let ((rn (string-append "/tmp/dmn-hold-" tag ".ss"))
        (lg (string-append "/tmp/dmn-hold-" tag ".log")))
    (system (string-append "rm -f " rn " " lg))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (only (theourgia ffi) lock-acquire! lock-release!))"
                (string-append "(define l (lock-acquire! \"" path "\" 'exclusive))")
                "(display \"HELD\\n\") (flush-output-port (current-output-port))"
                (string-append "(sleep (make-time 'time-duration 0 " (number->string secs) "))")
                "(lock-release! l)"))))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script " rn " > " lg " 2>&1 &"))
    ;; ⛔ WAIT FOR "HELD", NOT FOR A GUESS. A row that fired its requests
    ;; before the holder had the lock would measure an uncontended
    ;; daemon and pass no matter what the locking strategy did.
    (let up ((k 0))
      (cond ((contains? (file-text lg) "HELD") (list 'held rn))
            ((> k 200) (list 'never-held rn))
            (else (sleep-ms 25) (up (+ k 1)))))))

;; Asks the daemon for this store, and reports how long the answer took.
(define (timed-ask sock text ms)
  (let* ((t0 (real-time))
         (r (exchange sock (string->utf8 text) line-complete? ms)))
    (list (- (real-time) t0)
          (if (and (pair? r) (eq? 'answer (car r)))
              (utf8->string (cadr r))
              (list 'transport r)))))

;; Two frames on ONE connection, the second sent only after the first has
;; been answered. ⚠️ `exchange` opens a connection per call, and "the same
;; connection" is part of what read-your-writes claims, so a row about it
;; cannot be built out of two exchanges.
(define (two-step sock first-text second-text)
  (let ((asker self))
    (spawn
      (lambda ()
        (connect! sock)
        (receive
          (after 4000 (send asker (list 'two-step 'no-connect)))
          (`(connected ,p ,ref)
           (conn-read-start! ref)
           (conn-write! ref (string->utf8 first-text) 'one)
           (let hear-one ((acc ""))
             (receive
               (after 8000 (send asker (list 'two-step (list 'first-timed-out acc))))
               (`(written ,r ,t ,st) (hear-one acc))
               (`(data ,r ,bv)
                (let ((text (string-append acc (utf8->string bv))))
                  (if (not (contains? text "\n"))
                      (hear-one text)
                      (begin
                        (conn-write! ref (string->utf8 second-text) 'two)
                        (let hear-two ((acc2 ""))
                          (receive
                            (after 8000 (send asker (list 'two-step (list 'second-timed-out text acc2))))
                            (`(written ,r2 ,t2 ,st2) (hear-two acc2))
                            (`(data ,r2 ,bv2)
                             (let ((text2 (string-append acc2 (utf8->string bv2))))
                               (if (contains? text2 "\n")
                                   (send asker (list 'two-step (list 'both text text2)))
                                   (hear-two text2))))
                            (`(eof ,r2) (send asker (list 'two-step (list 'eof-second text acc2))))
                            (`#(DOWN ,w2 ,r2) (send asker (list 'two-step (list 'down-second text acc2))))))))))
               (`(eof ,r) (send asker (list 'two-step (list 'eof-first acc))))
               (`#(DOWN ,w ,r) (send asker (list 'two-step (list 'down-first acc))))))))))
    (let wait ()
      (receive (after 20000 'no-answer)
               (`(two-step ,what) what)
               (`#(DOWN ,w ,r) (wait))))))

;; Commits to a store the way a CLI does -- another OS process, taking
;; the store's lock itself, with no daemon involved.
(define (commit-from-outside! store title tag)
  (let ((rn (string-append "/tmp/dmn-out-" tag ".ss"))
        (lg (string-append "/tmp/dmn-out-" tag ".log")))
    (system (string-append "rm -f " rn " " lg))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia rpc))"
                (string-append "(write (rpc-dispatch \"" store "\" '(insert \"--title\" \""
                               title "\") \"outsider\")) (newline)")))))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script " rn " > " lg " 2>&1"))
    (file-text lg)))

;; Waits, with a bound, for the daemon to say it has published again.
;; ⛔ THE ROW WAITS FOR THE DAEMON TO SAY SO, ⛔ not for the outside
;; commit's lock to be released. Those are different instants: the
;; reload is asked for by a read that has already been answered, and it
;; happens when the store process gets to it. Asserting anything about
;; the first instant would be asserting a race.
(define (wait-for-publication d revision ms)
  (let ((marker (string-append "(trace published " (number->string revision))))
    (let wait ((k 0))
      (cond ((contains? (tagged-log d) marker) 'published)
            ((> k (div ms 50)) (list 'never-published revision))
            (else (sleep-ms 50) (wait (+ k 1)))))))

(start-scheduler
  (lambda ()
    (let ((main self))
      (want "D-01 the daemon comes up and creates its socket" (start-daemon!) 'up)

      ;; ---- D-01 TWIN: the build said nothing ---------------------------
      ;;
      ;; ⛔ A COMPILE WARNING FROM THE CHILD IS EVIDENCE, AND IT WAS
      ;; BURIED. An edit once left a five-argument call against a
      ;; seven-argument definition; Chez said so -- "possible incorrect
      ;; argument count in call (watch-loop ...)" -- and the daemon then
      ;; panicked at startup. What the rows showed was `no-connect` and
      ;; `transport-error -61` on eleven of twelve rows, which reads like
      ;; a transport problem and sent me looking in the wrong library.
      ;; The warning was in the log the whole time.
      (want "D-01 TWIN: the daemon's build produced no warnings"
            (let ((text (file-text log)))
              (if (contains? text "Warning") (list 'warned text) 'quiet))
            'quiet)

      ;; ---- D-02 one frame, one answer ---------------------------------
      (want "D-02 a request is answered, and the answer is the CLI's"
            (let ((r (exchange socket (string->utf8 (string-append "(request \"" store "\" \"tester\" outline)\n"))
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (utf8->string (cadr r))
                  (list 'transport r)))
            "(ok (text \"\"))\n")

      ;; ---- D-03 two frames in one write --------------------------------
      ;;
      ;; ⛔ ONE READ CAN CARRY TWO FRAMES. An implementation that took the
      ;; bytes before the first newline and dropped the rest would lose a
      ;; request that had already arrived -- silently, and only when a
      ;; client sends two quickly. Measured on the first version of this
      ;; daemon: it did exactly that.
      (spawn
        (lambda ()
          (let ((me self))
            (connect! socket)
            (receive
              (after 4000 (send main (list 'two 'no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               (conn-write! ref (string->utf8
                                  (string-append "(request \"" store "\" \"tester\" outline)\n"
                                                 "(request \"" store "\" \"tester\" outline)\n"))
                            'both)
               (let gather ((seen 0) (acc (make-bytevector 0)))
                 (receive
                   (after 5000 (send main (list 'two (list 'only seen))))
                   (`(data ,r ,bv)
                    (let* ((n (+ (bytevector-length acc) (bytevector-length bv)))
                           (all (let ((o (make-bytevector n)))
                                  (bytevector-copy! acc 0 o 0 (bytevector-length acc))
                                  (bytevector-copy! bv 0 o (bytevector-length acc) (bytevector-length bv))
                                  o))
                           (lines (let count ((i 0) (k 0))
                                    (cond ((>= i (bytevector-length all)) k)
                                          ((= (bytevector-u8-ref all i) 10) (count (+ i 1) (+ k 1)))
                                          (else (count (+ i 1) k))))))
                      (if (>= lines 2)
                          (send main (list 'two 'both-answered))
                          (gather lines all))))
                   (`(written ,r ,t ,st) (gather seen acc))
                   (`(eof ,r) (send main (list 'two (list 'eof-after seen))))
                   (`#(DOWN ,w ,why) (send main (list 'two (list 'down why)))))))))))
      (want "D-03 two frames in one write get two answers"
            (let wait ()
              (receive (after 9000 'no-answer)
                       (`(two ,what) what)
                       (`#(DOWN ,w ,r) (wait))))
            'both-answered)

      ;; ---- D-04 a frame past the limit ---------------------------------
      ;;
      ;; ⛔ REFUSED BEFORE IT IS PARSED, and the connection goes with it:
      ;; an over-long frame is not dispatched at all.
      (spawn
        (lambda ()
          (let ((me self))
            (connect! socket)
            (receive
              (after 4000 (send main (list 'big 'no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               ;; 1 MiB + a bit, with no newline in it
               (let ((chunk (make-bytevector 65536 65)))
                 (let push ((k 0))
                   (when (< k 17) (conn-write! ref chunk (list 'c k)) (push (+ k 1)))))
               (let hear ()
                 (receive
                   (after 8000 (send main (list 'big 'nothing)))
                   (`(data ,r ,bv)
                    (send main (list 'big (list 'answered (utf8->string bv)))))
                   (`(written ,r ,t ,st) (hear))
                   (`(eof ,r) (send main (list 'big 'closed-without-answer)))
                   (`#(DOWN ,w ,why) (send main (list 'big 'closed-without-answer))))))))))
      (want "D-04 a frame past the limit is refused, not dispatched"
            (let wait ()
              (receive (after 12000 'no-answer)
                       (`(big (answered ,text))
                        (if (starts-with? text "(error bad-request")
                            'refused
                            (list 'other text)))
                       (`(big ,other) other)
                       (`#(DOWN ,w ,r) (wait))))
            'refused)

      ;; ---- D-05 a second daemon on the same store ---------------------
      ;;
      ;; ⛔ THE LOCK IS THE ONLY ARBITER, and it is the kernel's: no pid
      ;; file, no age check. A second daemon says so and leaves with 75,
      ;; so a client that finds no answer falls back to running locally.
      (let* ((runner2 (string-append "/tmp/dmn-run2-" pid-text ".ss"))
             (log2 (string-append "/tmp/dmn-log2-" pid-text ".txt")))
        (call-with-output-file runner2
          (lambda (port)
            (for-each (lambda (l) (display l port) (newline port))
              (list "(import (chezscheme) (theourgia daemon))"
                    (string-append "(serve \"" store "\" \"" socket "\")")))))
        (let ((rc (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                                         " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                                         " scheme --script " runner2 " > " log2 " 2>&1"))))
          (want "D-05 a second daemon on the same store refuses and exits 75"
                (list (if (= rc 75) 'exit-75 (list 'exit rc))
                      (let ((text (file-text log2)))
                        (if (starts-with? text "(error serve-busy")
                            'said-busy
                            (list 'said text))))
                '(exit-75 said-busy))))

      ;; ---- D-06 something that is not a socket on the path ------------
      ;;
      ;; ⛔ A REGULAR FILE THERE IS SOMEBODY ELSE'S. Removing it to make
      ;; room would be this daemon destroying data it does not own, so it
      ;; refuses -- and the row checks the file is still there afterwards.
      (let* ((occupied (string-append "/tmp/dmn-occupied-" pid-text))
             (runner3 (string-append "/tmp/dmn-run3-" pid-text ".ss"))
             (log3 (string-append "/tmp/dmn-log3-" pid-text ".txt")))
        (system (string-append "printf 'not a socket' > " occupied))
        (call-with-output-file runner3
          (lambda (port)
            (for-each (lambda (l) (display l port) (newline port))
              (list "(import (chezscheme) (theourgia daemon))"
                    (string-append "(serve \"" store "\" \"" occupied "\")")))))
        (let ((rc (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                                         " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                                         " scheme --script " runner3 " > " log3 " 2>&1"))))
          (want "D-06 a non-socket on the path is refused, and left alone"
                (list (if (= rc 75) 'exit-75 (list 'exit rc))
                      (if (file-exists? occupied) 'still-there 'DESTROYED)
                      (let ((text (file-text log3)))
                        (if (starts-with? text "(error serve-path-occupied")
                            'said-occupied
                            (list 'said text))))
                '(exit-75 still-there said-occupied))))

      ;; ---- D-07 a frame that never finishes ---------------------------
      ;;
      ;; ⚠️ THE BUDGET IS THE WHOLE FRAME, NOT THE GAP BETWEEN BYTES. A
      ;; client sending one byte every second is never idle, so a per-gap
      ;; timer would let it hold a connection for ever.
      (spawn
        (lambda ()
          (let ((me self) (t0 (real-time)))
            (connect! socket)
            (receive
              (after 4000 (send main (list 'trickle 'no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               (monitor (conn-ref-pid ref))
               (let drip ((k 0))
                 (when (< k 12)
                   (conn-write! ref (string->utf8 "x") (list 'd k))
                   (sleep-ms 700)
                   (drip (+ k 1))))
               (let hear ()
                 (receive
                   (after 12000 (send main (list 'trickle 'never-closed)))
                   (`(eof ,r) (send main (list 'trickle (list 'closed (- (real-time) t0)))))
                   (`#(DOWN ,w ,why) (send main (list 'trickle (list 'closed (- (real-time) t0)))))
                   (`(data ,r ,bv) (hear))
                   (`(written ,r ,t ,st) (hear)))))))))
      (want "D-07 a frame that never finishes is closed on the frame's budget"
            (let wait ()
              (receive (after 20000 'no-answer)
                       (`(trickle (closed ,ms))
                        (if (and (> ms 4000) (< ms 9000)) 'closed-on-budget (list 'at ms)))
                       (`(trickle ,other) other)
                       (`#(DOWN ,w ,r) (wait))))
            'closed-on-budget)

      ;; ---- D-08 a request meant for another store ---------------------
      ;;
      ;; ⛔ REFUSED, NOT EXECUTED. A client reaches a daemon by deriving
      ;; the socket from its store, and `--socket P` lets it reach one
      ;; directly -- so a request for store B can arrive at a daemon
      ;; serving store A. Without the store in the envelope it would be
      ;; run against A: a write to the wrong library that nothing reports.
      (want "D-08 a request naming another store is refused, not run"
            (let ((r (exchange socket
                               (string->utf8 "(request \"/tmp/some-other-store\" \"tester\" outline)\n")
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (let ((text (utf8->string (cadr r))))
                    (if (starts-with? text "(error transport-store-mismatch")
                        'refused
                        (list 'other text)))
                  (list 'transport r)))
            'refused)

      ;; ⚠️ AND THE ONE THAT NAMES THIS STORE STILL WORKS -- without this
      ;; twin, a daemon that refused every request would pass the row
      ;; above.
      (want "D-08 TWIN: and a request naming this store is still served"
            (let ((r (exchange socket
                               (string->utf8 (string-append "(request \"" store "\" \"tester\" outline)\n"))
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (utf8->string (cadr r))
                  (list 'transport r)))
            "(ok (text \"\"))\n")

      ;; ---- D-09 a conn process that dies says why ---------------------
      ;;
      ;; ⛔ "EOF AND NO ANSWER" IS WHAT A CLIENT SEES WHETHER THE PEER
      ;; SIMPLY LEFT OR SOMETHING CRASHED. The reason travels in a
      ;; `#(DOWN pid reason)` nobody else reads, so main writes it to the
      ;; trace and the two stop looking alike. Measured the hard way: an
      ;; arity mistake in a conn process appeared only as a client
      ;; getting EOF, three rounds away from its cause.
      ;;
      ;; ⚠️ THE SEAM IS OURS, and it had to be. Four attempts through
      ;; igropyr's write seams produced nothing, for a structural reason:
      ;; an answer this size goes out whole inside `try_write`, so the
      ;; queued path where those faults live is never reached. The
      ;; process that must die is ours, so the way to make it die is too.
      (let ((crashed (run-daemon-with "conn-raise@conn" "outline")))
        (want "D-09 a conn process that dies writes its reason to the trace"
              (list (cadr crashed)
                    (if (contains? (car crashed) "injected conn raise") 'named-the-reason
                        (list 'trace-said (car crashed))))
              '(no-answer named-the-reason)))

      ;; ⛔ TWIN: THE SAME BUILD, NOT ARMED, ANSWERS NORMALLY AND WRITES
      ;; NOTHING. Without it the row above is satisfied by a daemon that
      ;; crashed for some other reason, or that traces every ending.
      (let ((quiet (run-daemon-with #f "outline")))
        (want "D-09 TWIN: unarmed, the same build answers and traces nothing"
              (list (cadr quiet)
                    (if (contains? (car quiet) "daemon-down") 'traced-anyway 'quiet)
                    (caddr quiet))
              '("(ok (text \"\"))\n" quiet socket-still-there)))

      ;; ---- D-10 the store process dies -------------------------------
      ;;
      ;; ⛔ WITHOUT THE STORE THERE IS NOTHING TO ANSWER WITH, so the
      ;; daemon says so and leaves with 75 -- it does not sit there
      ;; accepting connections it cannot serve. A client that finds no
      ;; socket next time runs locally, which is the whole point of
      ;; leaving rather than lingering.
      ;;
      ;; ⚠️ THE IN-FLIGHT REQUEST IS ANSWERED `transport-unknown`, not
      ;; `failed`: the request may have been executed before the process
      ;; died. ⛔ Something that may have happened must never be reported
      ;; as not having happened.
      (let ((dead-store (run-daemon-with "store-raise@conn" "insert \"--title\" \"STORE-RAISE-PROBE\"")))
        (want "D-10 a store that dies takes the daemon with it, and says why"
              (list (caddr dead-store)
                    (if (contains? (car dead-store) "injected store raise")
                        'named-the-reason
                        (list 'trace-said (car dead-store)))
                    (if (contains? (car dead-store) "store-actor-down")
                        'said-store-down
                        'no-exit-line))
              '(socket-gone named-the-reason said-store-down)))

      ;; ---- D-11 / D-12 somebody else is holding the store lock --------
      ;;
      ;; ⛔ NOTHING INSIDE A DAEMON MAY PARK ON `flock`. That call runs on
      ;; the scheduler's own thread, so a process waiting there stops
      ;; every other process in the VM -- including the one that would
      ;; have released the lock, and including main. The daemon therefore
      ;; sets one locking strategy for itself: try, yield, try again, and
      ;; give up on a budget.
      ;;
      ;; ⚠️ THE TIMES ARE THE POINT, not the answers. An implementation
      ;; that never takes the lock at all answers the first row's request
      ;; at once, and one that never retries answers the second row
      ;; `store-busy` at once -- both with exactly the text these rows
      ;; would otherwise accept. The clock is what separates them.
      (let ((contended
             (lambda (secs tag timeout)
               (let ((held (hold-lock! (string-append store "/lock") secs tag)))
                 ;; A request that never reaches the store: it is answered
                 ;; by the connection's own process, so it is the reading
                 ;; for "the rest of the daemon kept running".
                 (spawn
                   (lambda ()
                     (let ((r (timed-ask socket "(request \"/tmp/some-other-store\" \"tester\" outline)\n" 4000)))
                       (send main (list 'other (car r) (cadr r))))))
                 ;; ⛔ A WRITE, NOT A READ. Since reads are answered from
                 ;; the published value in the connection's own process,
                 ;; a read takes no lock at all and would be answered at
                 ;; once however long somebody else held it -- which is
                 ;; the daemon working, and no evidence about locking.
                 (let ((a (timed-ask socket
                                     (string-append "(request \"" store "\" \"tester\" insert \"--title\" \""
                                                    tag "\")\n")
                                     timeout)))
                   (let wait ()
                     (receive (after 15000 (list (car held) a 'no-other))
                       (`(other ,oms ,otext) (list (car held) a (list oms otext)))
                       (`#(DOWN ,w ,r) (wait))))))))
            (served-while-held
             (lambda (o)
               (cond ((not (pair? o)) o)
                     ((>= (car o) 1500) (list 'other-at (car o)))
                     ((and (string? (cadr o))
                           (starts-with? (cadr o) "(error transport-store-mismatch"))
                      'answered-while-held)
                     (else (list 'other-said (cadr o)))))))

        ;; A hold shorter than the budget: the request waits it out and
        ;; is then served normally.
        (let ((r (contended 3 (string-append pid-text "-short") 15000)))
          (want "D-11 a request waits out a lock somebody else holds, and the daemon keeps serving"
                (list (car r)
                      (if (> (car (cadr r)) 2000)
                          'waited-for-the-lock
                          (list 'answered-at (car (cadr r))))
                      (let ((text (cadr (cadr r))))
                        (if (and (string? text) (starts-with? text "(ok"))
                            'and-then-served
                            (list 'said text)))
                      (served-while-held (caddr r)))
                '(held waited-for-the-lock and-then-served answered-while-held)))

        ;; A hold longer than the budget: this request is refused, and
        ;; ⛔ refused as an answer -- `store-busy` is an ordinary state of
        ;; the world, not a crash and not `internal`.
        (let ((r (contended 6 (string-append pid-text "-long") 12000)))
          (want "D-12 a lock held past the budget answers store-busy, and only that request"
                (list (car r)
                      (let ((ms (car (cadr r))))
                        (if (and (> ms 4500) (< ms 7000))
                            'gave-up-on-the-budget
                            (list 'gave-up-at ms)))
                      (let ((text (cadr (cadr r))))
                        (if (and (string? text) (starts-with? text "(error store-busy"))
                            'said-store-busy
                            (list 'said text)))
                      (served-while-held (caddr r)))
                '(held gave-up-on-the-budget said-store-busy answered-while-held))))

      ;; ---- D-12 TWIN: the CLI, on the same lock, still waits ----------
      ;;
      ;; ⭐ THE STRATEGY IS THE DAEMON'S, NOT THE LOCKING LAYER'S. The
      ;; reason the CLI is unchanged is not that it was excluded: it is
      ;; that NOBODY IN THE CLI EVER SETS THAT PARAMETER, so it still
      ;; reads its default, which is the blocking `flock`. A CLI run has
      ;; one thing to do and no other processes to starve, so waiting is
      ;; the right answer there and giving up at five seconds would be a
      ;; regression.
      ;;
      ;; ⚠️ THE HOLD IS LONGER THAN THE DAEMON'S BUDGET ON PURPOSE. That
      ;; is the whole discrimination: at the same instant on the same
      ;; lock the daemon has already answered `store-busy` and the CLI
      ;; has not answered at all.
      (let* ((tstore (string-append "/tmp/dmn-cli-" pid-text))
             (rn (string-append "/tmp/dmn-cli-" pid-text ".ss"))
             (lg (string-append "/tmp/dmn-cli-" pid-text ".log"))
             (env (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                                 " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' ")))
        (system (string-append "rm -rf " tstore " " rn " " lg "; mkdir -p " tstore))
        (call-with-output-file rn
          (lambda (port)
            (for-each (lambda (l) (display l port) (newline port))
              (list "(import (chezscheme) (theourgia rpc))"
                    ;; ⛔ INIT BEFORE THE HOLDER TAKES THE LOCK: creating
                    ;; the store takes it too, and a store whose lock file
                    ;; does not exist yet cannot be held by anybody.
                    (string-append "(when (equal? \"init\" (car (command-line-arguments)))"
                                   " (rpc-dispatch \"" tstore "\" '(init) \"tester\") (exit 0))")
                    "(let* ((t0 (real-time))"
                    (string-append "       (r (rpc-dispatch \"" tstore "\" '(outline) \"tester\"))")
                    "       (ms (- (real-time) t0)))"
                    "  (write (list 'cli ms (if (and (pair? r) (eq? 'ok (car r))) 'ok r)))"
                    "  (newline))"))))
        (system (string-append env "scheme --script " rn " init > " lg " 2>&1"))
        (hold-lock! (string-append tstore "/lock") 8 (string-append pid-text "-cli"))
        (system (string-append env "scheme --script " rn " ask > " lg " 2>&1"))
        (want "D-12 TWIN: the CLI waits on the same lock past that budget, because nothing in it sets the strategy"
              (let ((said (guard (e (#t 'unreadable)) (call-with-input-file lg read))))
                (if (and (pair? said) (eq? 'cli (car said)))
                    (list (if (> (cadr said) 5500) 'still-waiting-past-the-budget (list 'answered-at (cadr said)))
                          (caddr said))
                    (list 'said (file-text lg))))
              '(still-waiting-past-the-budget ok)))

      ;; ---- D-13 a connection that ends in the middle of a frame -------
      ;;
      ;; ⛔ A FRAME IS FINISHED BY ITS NEWLINE, NOT BY THE CONNECTION
      ;; ENDING. The bytes below are a complete datum and a legal write;
      ;; only the newline is missing. An implementation that flushed what
      ;; it had when the peer went away would RUN that write -- and the
      ;; client, having already gone, would never learn that it had
      ;; happened. ⛔ A request nobody can be told the answer to must not
      ;; be executed.
      ;;
      ;; ⚠️ THE ROW WAITS FOR THE WRITE TO BE ACKNOWLEDGED BEFORE CLOSING,
      ;; because a row whose bytes never left the client would pass
      ;; against any daemon at all.
      ;;
      ;; ⚠️ AND IT WAITS OUT THE FRAME BUDGET BEFORE LOOKING. Flushing on
      ;; EOF and flushing when the frame's clock runs out are two
      ;; different mistakes; a row that looked immediately would only
      ;; catch the first.
      (spawn
        (lambda ()
          (let ((me self))
            (connect! socket)
            (receive
              (after 4000 (send main (list 'partial 'no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               (conn-write! ref (string->utf8
                                  (string-append "(request \"" store "\" \"tester\" insert \"--title\" \"MIDFRAME-CANARY\")"))
                            'nonewline)
               (receive
                 (after 4000 (send main (list 'partial 'never-written)))
                 (`(written ,r ,t ,st)
                  (conn-close! ref)
                  (send main (list 'partial (list 'sent st))))))))))
      (want "D-13 a frame with no newline is dropped when the connection ends, not run"
            (let ((sent (let wait ()
                          (receive (after 9000 'no-ack)
                                   (`(partial ,what) what)
                                   (`#(DOWN ,w ,r) (wait))))))
              (if (not (and (pair? sent) (eq? 'sent (car sent))))
                  (list 'never-reached-the-daemon sent)
                  (begin
                    (sleep-ms 6500)
                    (let ((r (exchange socket
                                       (string->utf8 (string-append "(request \"" store "\" \"tester\" outline)\n"))
                                       line-complete? 6000)))
                      (if (and (pair? r) (eq? 'answer (car r)))
                          (if (contains? (utf8->string (cadr r)) "MIDFRAME-CANARY")
                              (list 'IT-RAN (utf8->string (cadr r)))
                              'not-run)
                          (list 'transport r))))))
            'not-run)

      ;; ⛔ TWIN: THE SAME BYTES WITH THE NEWLINE DO RUN. Without it the
      ;; row above is satisfied by a daemon that refuses every insert, by
      ;; one whose outline never shows anything, and by a canary that was
      ;; never a legal request in the first place.
      (want "D-13 TWIN: the same request, newline and all, is run and shows up"
            (let ((r (exchange socket
                               (string->utf8 (string-append "(request \"" store "\" \"tester\" insert \"--title\" \"WHOLE-FRAME-CANARY\")\n"))
                               line-complete? 6000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (let ((said (utf8->string (cadr r))))
                    (if (starts-with? said "(ok")
                        (let ((o (exchange socket
                                           (string->utf8 (string-append "(request \"" store "\" \"tester\" outline)\n"))
                                           line-complete? 6000)))
                          (if (and (pair? o) (eq? 'answer (car o)))
                              (if (contains? (utf8->string (cadr o)) "WHOLE-FRAME-CANARY")
                                  'ran-and-shows
                                  (list 'not-in-outline (utf8->string (cadr o))))
                              (list 'transport o)))
                        (list 'insert-said said)))
                  (list 'transport r)))
            'ran-and-shows)

      ;; ---- D-14 a writer process killed while holding a lock ----------
      ;;
      ;; ⛔ A KILLED ACTOR RUNS NO UNWINDS, AND AN OS DESCRIPTOR DOES NOT
      ;; DIE WITH IT. That is the whole reason the descriptors are main's:
      ;; a process that opened its own and was then killed would leave the
      ;; store locked for as long as this daemon lives, and every later
      ;; request would answer `store-busy` for a holder that no longer
      ;; exists.
      ;;
      ;; ⚠️ THE SECOND REQUEST IS THE READING, AND ITS CLOCK IS THE
      ;; DISCRIMINATION. "It answered" is satisfied by a daemon that
      ;; waited out the whole five-second budget and got the lock only
      ;; because the kernel dropped it when the process exited; what the
      ;; rule claims is that main closed it, which is immediate.
      ;;
      ;; ⚠️ AND IT IS `drafts`, a verb that really takes the store lock
      ;; (`working-list` opens and reduces the store, which takes it): a
      ;; verb that needed no lock would answer at once whether or not the
      ;; descriptor leaked.
      (let* ((d (start-tagged-daemon! "wdown" "writer-raise@conn"))
             (first (ask-tagged d "drafts \"--writer\" \"w1\"" 8000))
             (second (ask-tagged d "drafts \"--writer\" \"w1\"" 8000))
             (trace (tagged-log d))
             (alive (file-exists? (tagged-socket d))))
        (stop-tagged-daemon! d)
        (want "D-14 a writer killed holding a lock leaves none behind, and the next request for it is served"
              (list (if (and (string? (cadr first))
                             (starts-with? (cadr first) "(error transport-unknown")
                             (contains? (cadr first) "writer-actor-down"))
                        'that-writers-request-failed
                        (list 'first-said (cadr first)))
                    (if (and (string? (cadr second)) (starts-with? (cadr second) "(ok"))
                        'served-again
                        (list 'second-said (cadr second)))
                    (if (< (car second) 100)
                        'at-once
                        (list 'second-took (car second)))
                    (if (contains? trace "injected writer raise") 'named-the-reason 'no-reason)
                    (if alive 'daemon-still-up 'DAEMON-GONE))
              '(that-writers-request-failed served-again at-once named-the-reason daemon-still-up)))

      ;; ⛔ TWIN: THE SAME BUILD, NOT ARMED. Without it the row above is
      ;; satisfied by a daemon on which `drafts --writer w1` answers `ok`
      ;; the second time because it answers `ok` every time and the first
      ;; answer was a coincidence of some other failure.
      (let* ((d (start-tagged-daemon! "wquiet" #f))
             (first (ask-tagged d "drafts \"--writer\" \"w1\"" 8000))
             (trace (tagged-log d)))
        (stop-tagged-daemon! d)
        (want "D-14 TWIN: unarmed, the same writer's first request is served and nothing dies"
              (list (if (and (string? (cadr first)) (starts-with? (cadr first) "(ok"))
                        'served
                        (list 'said (cadr first)))
                    (if (contains? trace "daemon-down") 'traced-a-death 'quiet))
              '(served quiet)))

      ;; ---- D-15 a writer that dies after it has let go ----------------
      ;;
      ;; ⛔ THE TIDY-UP MUST CLOSE NOTHING. The dangerous shape is a
      ;; descriptor closed by its user and only afterwards struck from the
      ;; table: in between, the number is free, this same VM can be given
      ;; it by the next `open`, and the tidy-up then closes a descriptor
      ;; belonging to somebody else. Main doing both in one handler is
      ;; what removes that window.
      ;;
      ;; ⚠️ WHAT THIS ROW CAN AND CANNOT SEE. It shows that a writer
      ;; dying with nothing registered costs nothing: the store is not
      ;; left locked and another writer is served. It does ⛔ NOT catch a
      ;; tidy-up that closed a live descriptor belonging to a DIFFERENT
      ;; writer, because arranging for that writer to be holding one at
      ;; the instant of the death needs a second seam that makes it sit
      ;; inside a locked operation, and no such seam exists. The claim is
      ;; carried by the structure -- main holds the lock HANDLE, whose own
      ;; `held` flag makes a second release a no-op, and the handle is
      ;; dropped from the table and released in one handler that does not
      ;; yield -- and is written here so that the gap is a decision on the
      ;; record rather than a row that looks like cover.
      (let* ((d (start-tagged-daemon! "wlate" "writer-raise-late@conn"))
             (first (ask-tagged d "drafts \"--writer\" \"w1\"" 8000))
             (other (ask-tagged d "drafts \"--writer\" \"w2\"" 8000))
             (read-back (ask-tagged d "outline" 8000))
             (trace (tagged-log d))
             (alive (file-exists? (tagged-socket d))))
        (stop-tagged-daemon! d)
        (want "D-15 a writer that dies after letting go leaves nothing locked and nobody else short"
              (list (if (and (string? (cadr first)) (starts-with? (cadr first) "(ok"))
                        'answered-before-dying
                        (list 'first-said (cadr first)))
                    (if (and (string? (cadr other)) (starts-with? (cadr other) "(ok"))
                        'another-writer-served
                        (list 'other-said (cadr other)))
                    (if (and (string? (cadr read-back)) (starts-with? (cadr read-back) "(ok")
                             (< (car read-back) 100))
                        'store-not-left-locked
                        (list 'read-back (car read-back) (cadr read-back)))
                    (if (contains? trace "injected writer raise late") 'named-the-reason 'no-reason)
                    (if alive 'daemon-still-up 'DAEMON-GONE))
              '(answered-before-dying another-writer-served store-not-left-locked
                named-the-reason daemon-still-up)))

      ;; ---- D-16 one writer tidied up while another is inside a lock ---
      ;;
      ;; ⛔ TIDYING UP AFTER ONE PROCESS MUST NOT TOUCH ANOTHER'S
      ;; DESCRIPTOR. This is the whole reason main holds lock HANDLES and
      ;; releases them BY OWNER: a tidy-up that went by anything less
      ;; specific -- a bare descriptor number it had written down, or
      ;; simply everything it was holding -- would, at this instant, shut
      ;; a lock a living process is inside. Nothing would report it. The
      ;; next thing to happen would be two writers in one working
      ;; directory at once.
      ;;
      ;; ⚠️ THE READING IS TAKEN AFTER THE DEATH AND BEFORE THE WAKING.
      ;; Asking whether the lock is held before the other writer died
      ;; would measure nothing: of course it is held. The row waits for
      ;; the second writer to answer and die, gives main time to tidy up,
      ;; and only then tries the lock -- so a `#f` here is evidence about
      ;; the tidy-up and not about the holder.
      ;;
      ;; ⚠️ AND THE PARK IS IN THE DRAFT LOCK, NOT THE STORE LOCK: the
      ;; other writer has to be SERVED while this one is parked, and
      ;; being served needs the store lock. Parking there would serialise
      ;; the two and leave no interleaving to look at.
      (let* ((d (start-tagged-daemon! "whold" "writer-hold@conn"))
             (draft (draft-lock-path (tagged-store d) "w1")))
        (spawn
          (lambda ()
            (send main (list 'held-answer (ask-tagged d "drafts \"--writer\" \"w1\"" 12000)))))
        (sleep-ms 150)
        (let* ((beside (ask-tagged d "drafts \"--writer\" \"w2\"" 8000)))
          (sleep-ms 200)
          (let* ((taken (guard (e (#t 'raised)) (lock-try-acquire! draft 'exclusive)))
                 (during (cond ((eq? taken 'raised) 'lock-file-not-there)
                               ((not taken) 'still-held-by-its-owner)
                               (else (lock-release! taken) 'TAKEN-FROM-UNDER-IT)))
                 (a (let wait ()
                      (receive (after 15000 'no-answer)
                               (`(held-answer ,r) r)
                               (`#(DOWN ,w ,r) (wait)))))
                 (afterwards (guard (e (#t 'raised)) (lock-try-acquire! draft 'exclusive)))
                 (freed (cond ((eq? afterwards 'raised) 'lock-file-not-there)
                              ((not afterwards) 'STILL-LOCKED-AFTERWARDS)
                              (else (lock-release! afterwards) 'released-by-its-owner)))
                 (trace (tagged-log d))
                 (alive (file-exists? (tagged-socket d))))
            (stop-tagged-daemon! d)
            (want "D-16 a writer tidied up beside a parked one leaves the parked one's lock alone"
                  (list (if (and (pair? beside) (string? (cadr beside))
                                 (starts-with? (cadr beside) "(ok"))
                            'the-other-was-served
                            (list 'beside-said (cadr beside)))
                        (if (contains? trace "injected writer raise beside a holder")
                            'and-then-died
                            'no-death-traced)
                        during
                        (if (and (pair? a) (string? (cadr a)) (starts-with? (cadr a) "(ok")
                                 (> (car a) 900))
                            'parked-then-answered
                            (list 'parked-said a))
                        freed
                        (if alive 'daemon-still-up 'DAEMON-GONE))
                  '(the-other-was-served and-then-died still-held-by-its-owner
                    parked-then-answered released-by-its-owner daemon-still-up)))))

      ;; ---- D-17 a client sees its own write ---------------------------
      ;;
      ;; ⛔ THE VALUE IS PUBLISHED BEFORE THE ANSWER GOES BACK, and this
      ;; is the whole of why that ordering matters. The answer to a write
      ;; is the client's evidence that the write happened; if publication
      ;; came after it, a client could be told "done" and then be handed
      ;; a value that does not contain it -- by the same daemon, on the
      ;; same connection, one frame later.
      ;;
      ;; ⚠️ ON ONE CONNECTION, AND THE SECOND FRAME IS SENT ONLY AFTER
      ;; THE FIRST IS ANSWERED. Two exchanges would be two connections
      ;; and a different claim.
      ;;
      ;; ⚠️ AND THE READ IS SERVED BY A DIFFERENT PROCESS FROM THE WRITE
      ;; -- the write by the store process, the read by the connection's
      ;; own, out of the published value. That is what makes this a
      ;; question at all.
      (want "D-17 a read on the same connection sees the write it follows"
            (let ((answers (two-step socket
                                     (string-append "(request \"" store "\" \"tester\" insert \"--title\" \"RYW-CANARY\")\n")
                                     (string-append "(request \"" store "\" \"tester\" outline)\n"))))
              (if (and (pair? answers) (eq? 'both (car answers)))
                  (list (if (starts-with? (cadr answers) "(ok") 'written (list 'write-said (cadr answers)))
                        (if (contains? (caddr answers) "RYW-CANARY") 'and-read-back 'NOT-IN-THE-READ))
                  (list 'transport answers)))
            '(written and-read-back))

      ;; ---- D-18 somebody else commits ---------------------------------
      ;;
      ;; ⛔ THE DAEMON DOES NOT HOLD THE STORE, so another process can
      ;; commit to it at any time. A reader that never noticed would go
      ;; on answering from a fold made before that commit for as long as
      ;; the daemon lived. What notices is a cheap stat comparison on the
      ;; read path; what acts on it is the store process, later.
      ;;
      ;; ⛔ THE ROW WAITS FOR THE DAEMON TO SAY IT HAS PUBLISHED AGAIN,
      ;; ⛔ NOT for the outside commit to finish. Those are different
      ;; instants, and only the first one is a fact about this daemon.
      (let* ((d (start-tagged-daemon! "extern" #f))
             (before (ask-tagged d "outline" 8000))
             (outside (commit-from-outside! (tagged-store d) "OUTSIDE-CANARY"
                                            (string-append pid-text "-extern")))
             (triggering (ask-tagged d "outline" 8000))
             (waited (wait-for-publication d 2 2000))
             (after-reload (ask-tagged d "outline" 8000))
             (alive (file-exists? (tagged-socket d))))
        (stop-tagged-daemon! d)
        (want "D-18 an outside commit is picked up, and the daemon says when"
              (list (if (and (string? (cadr before)) (starts-with? (cadr before) "(ok"))
                        'served-before
                        (list 'before-said (cadr before)))
                    (if (contains? outside "(ok") 'outside-committed (list 'outside-said outside))
                    waited
                    (if (and (string? (cadr after-reload)) (contains? (cadr after-reload) "OUTSIDE-CANARY"))
                        'and-then-visible
                        (list 'still-said (cadr after-reload)))
                    (if alive 'daemon-still-up 'DAEMON-GONE))
              '(served-before outside-committed published and-then-visible daemon-still-up))

        ;; ⛔ TWIN: THE READ THAT NOTICED IS STILL ANSWERED FROM THE OLD
        ;; VALUE. Without this row, "reads are answered from what is
        ;; published" is satisfied by a daemon that reloads under the
        ;; store's lock before answering every read -- which is the
        ;; behaviour this design exists to avoid, and which would pass
        ;; the row above with room to spare.
        ;;
        ;; ⚠️ IT IS A READING OF THE ANSWER ALREADY TAKEN ABOVE, not a
        ;; second request: asking again would be asking after the reload.
        (want "D-18 TWIN: the read that triggered the reload still answered the value it had"
              (if (and (string? (cadr triggering)) (starts-with? (cadr triggering) "(ok"))
                  (if (contains? (cadr triggering) "OUTSIDE-CANARY")
                      'RELOADED-BEFORE-ANSWERING
                      'answered-the-published-value)
                  (list 'said (cadr triggering)))
              'answered-the-published-value))

      (stop-daemon!)
      ;; ⛔ THIS RUN CLEANS UP AFTER ITSELF, BY ITS OWN PID. Every row
      ;; here that starts a daemon leaves a store, a socket, a runner and
      ;; a log behind, and they accumulated: a thousand two hundred of
      ;; them after one day's work. That is not tidiness -- the fixtures
      ;; in this directory name their roots after the process id, ids get
      ;; reused, and a root that is already there is refused. Measured:
      ;; `behind` failed a whole suite run with "Use a fresh test root"
      ;; against a directory left by a run three hours earlier that had
      ;; happened to get the same pid.
      ;;
      ;; ⚠️ BY PID PREFIX, so it removes this run's files and ⛔ nothing
      ;; belonging to a run going on beside it.
      (system (string-append "rm -rf /tmp/dmn-*" pid-text "* /tmp/dmn-store-" pid-text
                             " /tmp/dmn-run*-" pid-text ".ss /tmp/dmn-log*-" pid-text ".txt"
                             " /tmp/dmn-occupied-" pid-text " /tmp/.dmn-*" pid-text "*.lock"
                             " 2>/dev/null"))
      (printf "rows: ~a\n~a failures\ndaemon-e1 complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
