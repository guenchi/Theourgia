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
        (only (theourgia rpc) request-frame)
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
;; ⭐ THE FIRST ANSWER OUT OF A RUN OF THEM. Rows that read a connection
;; until it closes get every answer that arrived, concatenated; several
;; of them ask what the FIRST one was. Reading the first envelope and
;; taking its `stdout` keeps that question exact -- a substring search
;; over the whole accumulation would be answered by any of the answers,
;; which is a different and weaker question.
;;
;; ⚠️ TEXT THAT IS NOT AN ENVELOPE COMES BACK UNCHANGED, so a row looking
;; at something else still sees it, and a malformed reply appears as
;; itself rather than as "".
(define (first-answer-text acc)
  (if (not (string? acc))
      acc
      (let* ((port (open-string-input-port acc))
             (datum (guard (e (#t #f)) (read port))))
        (if (and (pair? datum) (eq? 'answer (car datum)))
            (let ((hit (assq 'stdout (cdr datum))))
              (if (and (pair? hit) (pair? (cdr hit)) (string? (cadr hit)))
                  (cadr hit)
                  acc))
            acc))))

;; ⛔ WHICH SIDE SPOKE, out of the same envelope. A refusal the daemon
;; makes and an answer the core computed arrive in one shape, and the
;; only thing that tells them apart is this field -- so a row about a
;; refusal has to read it, or it is a row about the text alone.
(define (first-answer-origin acc)
  (if (not (string? acc))
      acc
      (let* ((port (open-string-input-port acc))
             (datum (guard (e (#t #f)) (read port))))
        (if (and (pair? datum) (eq? 'answer (car datum)))
            (let ((hit (assq 'origin (cdr datum))))
              (if (and (pair? hit) (pair? (cdr hit)))
                  (cadr hit)
                  'no-origin-field))
            'not-an-answer))))

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
         (lg (string-append "/tmp/dmn-c" tag ".log"))
         ;; ⚠️ THE DAEMON WRITES ITS OWN PID AND THE SHELL WRITES ITS
         ;; EXIT CODE. A row about signals needs to send one to THIS
         ;; daemon -- `pkill -f` would hit any other run's -- and a row
         ;; about shutting down needs the code it left with, which is
         ;; gone by the time anything can ask about it.
         (pf (string-append "/tmp/dmn-c" tag ".pid"))
         (rc (string-append "/tmp/dmn-c" tag ".rc")))
    (system (string-append "rm -rf " st " " sk " " pf " " rc "; mkdir -p " st))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
                ;; ⚠️ `'truncate`, BECAUSE A RUNNER MAY BE STARTED TWICE.
                ;; `call-with-output-file` refuses an existing file, and
                ;; the takeover row starts this same runner a second
                ;; time -- measured: the second daemon died on the pid
                ;; file before it reached `serve`, and the row read as
                ;; "the new daemon does not answer", which is a
                ;; statement about the daemon and was about this line.
                (string-append "(call-with-output-file \"" pf
                               "\" (lambda (p) (write (get-process-id) p)) 'truncate)")
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
                           " sh -c 'scheme --script " rn " > " lg " 2>&1; echo $? > " rc "' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) 'up)
            ((> k 120) 'never) (else (sleep-ms 50) (up (+ k 1)))))
    (list st sk rn lg pf rc)))

(define (tagged-pid d)
  (let ((text (file-text (list-ref d 4))))
    (and (> (string-length text) 0)
         (string->number (substring text 0 (string-length text))))))

(define (signal-tagged! d name)
  (let ((pid (tagged-pid d)))
    (and pid
         (begin (system (string-append "kill -" name " " (number->string pid)))
                'sent))))

;; ⛔ WAITS FOR THE EXIT CODE, WITH A BOUND, and says which of the two
;; things happened. "It did not exit" and "it exited with 0" are
;; different answers and a row that cannot tell them apart is not a row
;; about shutting down.
(define (tagged-exit d ms)
  (let wait ((k 0))
    (let ((text (file-text (list-ref d 5))))
      (cond ((> (string-length text) 0)
             (list 'exited (string->number
                             (substring text 0 (- (string-length text) 1)))))
            ((> k (div ms 25)) (list 'still-running-after ms))
            (else (sleep-ms 25) (wait (+ k 1)))))))

(define (tagged-store d) (car d))
(define (tagged-socket d) (cadr d))
(define (tagged-log d) (file-text (cadddr d)))
(define (stop-tagged-daemon! d)
  (system (string-append "pkill -f " (caddr d) " 2>/dev/null")))

;; One request to a daemon started above, with the answer's text and the
;; time it took. ⚠️ The time includes connecting, because that is what a
;; client waits: a row that timed only the dispatch would be measuring
;; something no client can observe.
;; ⛔ THE ENVELOPE IS SPELLED IN ONE PLACE IN THIS FILE. The rows below
;; send deliberately malformed VERBS and argument lists, which
;; `request-frame` cannot build and should not -- it refuses them, which
;; is correct of it and useless here. So the frame is written out, and
;; written out once: two helpers each with their own copy is two places
;; to miss when the shape changes, which is what happened when the
;; version field was added.
;;
;;   (request <version> <store> <actor> <writer> <mode> <cwd> <stdin> ...)
;;
;; ⚠️ `#f wire #f #f` IS "no writer, answer me in wire form, no cwd, no
;; stdin" -- each a value, none omitted.
;; ⛔ THE ROWS BELOW ARE ABOUT THE CORE'S ANSWER, and the core's answer
;; is the text inside the answer envelope's `stdout`. Unwrapping it here
;; keeps every one of them asking exactly what it asked before the
;; envelope existed -- the alternative was rewriting two dozen expected
;; strings, which would have turned a transport change into a rewrite of
;; what each row believes.
;;
;; ⚠️ WHAT CANNOT BE UNWRAPPED COMES BACK AS IT ARRIVED. A malformed or
;; unexpected reply then shows up as itself in the failure message,
;; rather than as an empty string that reads like "the daemon said
;; nothing".
;;
;; ⚠️ AND THIS IS NOT USED BY `ask-until-eof`, which counts answers and
;; looks for the terminator: those rows are about the raw bytes on the
;; connection, and unwrapping would erase the thing they measure.
(define (answer-text bv)
  (let* ((text (if (string? bv) bv (utf8->string bv)))
         (datum (guard (e (#t #f)) (read (open-string-input-port text)))))
    (if (and (pair? datum) (eq? 'answer (car datum)))
        (let ((hit (assq 'stdout (cdr datum))))
          (if (and (pair? hit) (pair? (cdr hit)) (string? (cadr hit)))
              (cadr hit)
              text))
        text)))

(define (envelope-around store text)
  (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f " text ")\n"))

;; The same envelope with a writer in it. The writer's field is the one
;; after the actor, and these rows need it filled where every other row
;; here leaves it #f: which process serves a request depends on it.
(define (envelope-around-as store who text)
  (string-append "(request 1 \"" store "\" \"tester\" \"" who "\" wire #f #f " text ")\n"))

(define (ask-tagged-as d who text ms)
  (let ((r (exchange (tagged-socket d)
                     (string->utf8 (envelope-around-as (tagged-store d) who text))
                     line-complete? ms)))
    (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
        (answer-text (cadr r))
        'no-answer)))

(define (ask-tagged d text ms)
  (let* ((t0 (real-time))
         (r (exchange (tagged-socket d)
                      (string->utf8 (envelope-around (tagged-store d) text))
                      line-complete? ms)))
    (list (- (real-time) t0)
          (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
              (answer-text (cadr r))
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
                      (string->utf8 (envelope-around (tagged-store d) request-text))
                      line-complete? 3000))
         (got (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
                  (answer-text (cadr r))
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
              (answer-text (cadr r))
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

;; Sends whatever it is given on ONE connection and reads to EOF, so the
;; row can say how many answers arrived. ⛔ `exchange` stops at the first
;; complete line, which cannot tell "one answer" from "one answer and
;; then another": counting is the whole question for a rule that says a
;; request gets exactly one answerer.
(define (ask-until-eof sock text ms)
  (let ((asker self))
    (spawn
      (lambda ()
        (connect! sock)
        (receive
          (after 4000 (send asker (list 'until-eof 'no-connect 0)))
          (`(connected ,p ,ref)
           (conn-read-start! ref)
           (conn-write! ref (string->utf8 text) 'all)
           (let hear ((acc ""))
             (receive
               (after ms (send asker (list 'until-eof (list 'timed-out acc)
                                           (count-lines acc))))
               (`(written ,r ,t ,st) (hear acc))
               (`(data ,r ,bv) (hear (string-append acc (utf8->string bv))))
               (`(eof ,r) (send asker (list 'until-eof acc (count-lines acc))))
               (`#(DOWN ,w ,why) (send asker (list 'until-eof acc (count-lines acc))))))))))
    (let wait ()
      (receive (after (+ ms 6000) (list 'no-answer 0))
               (`(until-eof ,text ,n) (list text n))
               (`#(DOWN ,w ,r) (wait))))))

(define (count-lines text)
  (let loop ((i 0) (n 0))
    (cond ((>= i (string-length text)) n)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

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
            (let ((r (exchange socket (string->utf8 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (answer-text (cadr r))
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
                                  (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"
                                                 "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))
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
                        (if (starts-with? (first-answer-text text) "(error bad-request")
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
                               (string->utf8 "(request 1 \"/tmp/some-other-store\" \"tester\" #f wire #f #f outline)\n")
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (let ((text (answer-text (cadr r))))
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
                               (string->utf8 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))
                               line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (answer-text (cadr r))
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
                     (let ((r (timed-ask socket "(request 1 \"/tmp/some-other-store\" \"tester\" #f wire #f #f outline)\n" 4000)))
                       (send main (list 'other (car r) (cadr r))))))
                 ;; ⛔ A WRITE, NOT A READ. Since reads are answered from
                 ;; the published value in the connection's own process,
                 ;; a read takes no lock at all and would be answered at
                 ;; once however long somebody else held it -- which is
                 ;; the daemon working, and no evidence about locking.
                 (let ((a (timed-ask socket
                                     (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \""
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
                        (if (and (string? text) (starts-with? (first-answer-text text) "(ok"))
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
                        (if (and (string? text) (starts-with? (first-answer-text text) "(error store-busy"))
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
                                  (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \"MIDFRAME-CANARY\")"))
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
                                       (string->utf8 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))
                                       line-complete? 6000)))
                      (if (and (pair? r) (eq? 'answer (car r)))
                          (if (contains? (answer-text (cadr r)) "MIDFRAME-CANARY")
                              (list 'IT-RAN (answer-text (cadr r)))
                              'not-run)
                          (list 'transport r))))))
            'not-run)

      ;; ⛔ TWIN: THE SAME BYTES WITH THE NEWLINE DO RUN. Without it the
      ;; row above is satisfied by a daemon that refuses every insert, by
      ;; one whose outline never shows anything, and by a canary that was
      ;; never a legal request in the first place.
      (want "D-13 TWIN: the same request, newline and all, is run and shows up"
            (let ((r (exchange socket
                               (string->utf8 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \"WHOLE-FRAME-CANARY\")\n"))
                               line-complete? 6000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (let ((said (answer-text (cadr r))))
                    (if (starts-with? (first-answer-text said) "(ok")
                        (let ((o (exchange socket
                                           (string->utf8 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))
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
                    (if (and (string? (cadr second))
                             (starts-with? (first-answer-text (cadr second)) "(ok"))
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
              (list (if (and (string? (cadr first))
                             (starts-with? (first-answer-text (cadr first)) "(ok"))
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
              (list (if (and (string? (cadr first))
                             (starts-with? (first-answer-text (cadr first)) "(ok"))
                        'answered-before-dying
                        (list 'first-said (cadr first)))
                    (if (and (string? (cadr other))
                             (starts-with? (first-answer-text (cadr other)) "(ok"))
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
                                     (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \"RYW-CANARY\")\n")
                                     (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n"))))
              (if (and (pair? answers) (eq? 'both (car answers)))
                  (list (if (starts-with? (first-answer-text (cadr answers)) "(ok")
                            'written (list 'write-said (cadr answers)))
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

      ;; ---- D-19 one SIGTERM, a clean drain ----------------------------
      ;;
      ;; ⛔ WHAT IS RUNNING FINISHES; WHAT HAS NOT STARTED IS REFUSED;
      ;; THE SOCKET IS TIDIED. A daemon that dropped the request it was
      ;; in the middle of would leave a client that had been told
      ;; nothing about work that may well have been done -- and a daemon
      ;; that left its socket behind makes the next client wait on a
      ;; path with nobody on it before it falls back.
      ;;
      ;; ⚠️ A CLEAN DRAIN LEAVES WITH 0. 75 is what a second signal and a
      ;; drain that ran out of time leave with; a row that accepted any
      ;; exit code would not tell those apart.
      (let* ((d (start-tagged-daemon! "drain" #f))
             (before (ask-tagged d "outline" 8000))
             (sent (signal-tagged! d "TERM"))
             (code (tagged-exit d 8000))
             (socket-after (file-exists? (tagged-socket d)))
             (later (ask-tagged d "outline" 3000)))
        (want "D-19 one SIGTERM drains and the daemon leaves with 0"
              (list (if (and (string? (cadr before)) (starts-with? (cadr before) "(ok"))
                        'served-before
                        (list 'before-said (cadr before)))
                    sent
                    code
                    (if socket-after 'SOCKET-LEFT-BEHIND 'socket-tidied)
                    (if (eq? 'no-answer (cadr later)) 'refused-afterwards
                        (list 'answered-afterwards (cadr later))))
              '(served-before sent (exited 0) socket-tidied refused-afterwards)))

      ;; ---- D-20 a second signal does not wait -------------------------
      ;;
      ;; ⛔ 75, AND WITHOUT WAITING OUT THE BUDGET. Asking twice is asking
      ;; to stop now; a daemon that finished its five-second drain anyway
      ;; and then reported 75 would be doing the opposite of what it was
      ;; asked while printing the right number.
      ;;
      ;; ⚠️ THE SEAM IS A REQUEST THAT WILL NOT COME BACK -- the writer
      ;; parked inside its draft lock -- because otherwise the drain is
      ;; over before a second signal could mean anything.
      (let* ((d (start-tagged-daemon! "twice" "writer-hold@conn")))
        (spawn (lambda () (ask-tagged d "drafts \"--writer\" \"w1\"" 12000)))
        (sleep-ms 200)
        (let ((t0 (real-time)))
          (signal-tagged! d "TERM")
          (sleep-ms 150)
          (signal-tagged! d "TERM")
          (let* ((code (tagged-exit d 8000))
                 (ms (- (real-time) t0)))
            (stop-tagged-daemon! d)
            (want "D-20 a second SIGTERM leaves with 75 without waiting out the budget"
                  (list code (if (< ms 3000) 'without-waiting (list 'took ms)))
                  '((exited 75) without-waiting)))))

      ;; ---- D-21 the watchdog -----------------------------------------
      ;;
      ;; ⛔ A DRAIN THAT CANNOT FINISH STILL ENDS. The seam parks a
      ;; request inside a lock for longer than the whole budget, so the
      ;; only way out is the clock. ⚠️ Without a clock main would wait for
      ;; a message that is never coming, and "it exits within five
      ;; seconds" would be a promise kept only by requests that were
      ;; going to finish anyway.
      (let* ((d (start-tagged-daemon! "watchdog" "writer-hold-long@conn")))
        (spawn (lambda () (ask-tagged d "drafts \"--writer\" \"w1\"" 20000)))
        (sleep-ms 200)
        (let ((t0 (real-time)))
          (signal-tagged! d "TERM")
          (let* ((code (tagged-exit d 12000))
                 (ms (- (real-time) t0)))
            (stop-tagged-daemon! d)
            (want "D-21 a drain that cannot finish exits 75 on the budget"
                  (list code
                        (if (and (> ms 4500) (< ms 8000)) 'on-the-budget (list 'at ms)))
                  '((exited 75) on-the-budget)))))

      ;; ---- D-22 a frame that arrives during a drain -------------------
      ;;
      ;; ⛔ CLOSED, ⛔ NOT DISPATCHED. The frame below is a WRITE, so a
      ;; daemon that ran it would be changing the store after it had been
      ;; told to stop -- and the client, whose connection is closing,
      ;; would never learn that it had.
      ;; ---- D-22b what a MALFORMED frame is told during a drain --------
      ;;
      ;; ⛔ DRAINING IS A PREMISE AND IS JUDGED LAST. Answered before the
      ;; frame was parsed, a malformed frame that was already buffered
      ;; when a drain began was told `draining` -- which says "your
      ;; request was fine and we are not taking it now", when it was never
      ;; a request at all. §7.6.50 v249 fixes the order: well-formed, then
      ;; whose store, then premises.
      ;;
      ;; ⚠️ THE SECOND FRAME IS SENT IN THE SAME WRITE AS THE FIRST, which
      ;; is the only way to reach the branch this row is about. Written as
      ;; a fresh connection opened during the drain, it answered
      ;; `connection-closed` every time: a new connection during a drain
      ;; is closed before it is read, which is a different path and is
      ;; what D-22 measures.
      (let* ((d (start-tagged-daemon! "drainbad" "writer-hold@conn"))
             (pair (string-append
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\")\n"
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f read 42)\n")))
        (spawn (lambda () (send main (list 'drainbad (ask-until-eof (tagged-socket d) pair 15000)))))
        (sleep-ms 300)
        (signal-tagged! d "TERM")
        (let* ((answers (let wait ()
                          (receive (after 20000 'no-answer)
                                   (`(drainbad ,a) a)
                                   (`,other (wait)))))
               (text (if (and (pair? answers) (string? (car answers))) (car answers) "")))
          (tagged-exit d 12000)
          (stop-tagged-daemon! d)
          ;; The first frame is served; the second was buffered when the
          ;; drain began and is the one this row is about.
          (want "D-22b a malformed frame buffered at a drain is told it was malformed"
                (cond
                  ((contains? text "bad-request") 'told-it-was-malformed)
                  ((contains? text "(error draining") 'CALLED-IT-DRAINING)
                  (else (list 'said text)))
                'told-it-was-malformed)))

      (let* ((d (start-tagged-daemon! "idleconn" "writer-hold-long@conn")))
        (spawn (lambda () (ask-tagged d "drafts \"--writer\" \"w1\"" 20000)))
        (sleep-ms 200)
        (signal-tagged! d "TERM")
        (sleep-ms 300)
        (let* ((during (ask-tagged d "insert \"--title\" \"AFTER-DRAIN-CANARY\"" 4000))
               (code (tagged-exit d 12000)))
          (stop-tagged-daemon! d)
          (want "D-22 a request sent during a drain is refused or dropped, never run"
                (list (cond ((eq? 'no-answer (cadr during)) 'connection-closed)
                            ((and (string? (cadr during))
                                  (starts-with? (cadr during) "(error draining"))
                             'answered-draining)
                            (else (list 'said (cadr during))))
                      ;; ⛔ ASKED OF THE STORE, NOT OF THE LOG. Whether a
                      ;; write ran is a fact about what is on the disk;
                      ;; the daemon's own output is where it would say so
                      ;; if it chose to, which is not the same question.
                      (let ((found (string-append "/tmp/dmn-found-" pid-text ".txt")))
                        (system (string-append "grep -rl AFTER-DRAIN-CANARY "
                                               (tagged-store d) " > " found " 2>/dev/null"))
                        (if (> (string-length (file-text found)) 0) 'IT-RAN 'not-run))
                      code)
                '(connection-closed not-run (exited 75)))))

      ;; ---- D-23 a frame already in the buffer when the drain starts ---
      ;;
      ;; ⛔ ANSWERED `draining`, ⛔ NOT RUN AND ⛔ NOT DROPPED. Both frames
      ;; below arrive in ONE write, so the second is sitting in this
      ;; connection's buffer, parsed by nobody, while the first is being
      ;; served. A daemon that ran it would be working after it was told
      ;; to stop; one that simply closed would leave a client unable to
      ;; tell a refusal from a lost connection.
      ;;
      ;; ⚠️ THE ROW COUNTS THE ANSWERS. "The second was refused" and "the
      ;; second was refused twice" are different facts, and a reader that
      ;; stopped at the first newline could not tell them apart -- which
      ;; is the whole of what "one request, one answerer" claims.
      (let* ((d (start-tagged-daemon! "queued" "writer-hold@conn"))
             (both (string-append
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\")\n"
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w2\")\n")))
        (spawn (lambda () (send main (list 'queued (ask-until-eof (tagged-socket d) both 15000)))))
        (sleep-ms 300)
        (signal-tagged! d "TERM")
        (let* ((answers (let wait ()
                          (receive (after 20000 'no-answer)
                                   (`(queued ,what) what)
                                   (`#(DOWN ,w ,r) (wait)))))
               (code (tagged-exit d 12000)))
          (stop-tagged-daemon! d)
          (want "D-23 a frame buffered when the drain begins is answered draining, once"
                (list (if (and (pair? answers) (string? (car answers))
                               (starts-with? (first-answer-text (car answers)) "(ok"))
                          'first-served
                          (list 'first-said answers))
                      (if (and (pair? answers) (string? (car answers))
                               (contains? (car answers) "(error draining"))
                          'second-refused
                          'NO-DRAINING-ANSWER)
                      (if (and (pair? answers) (= 2 (cadr answers)))
                          'exactly-two-answers
                          (list 'answers (and (pair? answers) (cadr answers))))
                      code)
                '(first-served second-refused exactly-two-answers (exited 0)))))

      ;; ---- D-33 the limit measures a frame, not the buffer ------------
      ;;
      ;; ⛔ `daemon.ss` SAYS "IT IS THE FRAME THAT IS MEASURED, NOT THE
      ;; BUFFER" and nothing asked it. Several frames arrive in one read
      ;; whenever a client writes them together, and a ceiling applied to
      ;; what has accumulated would refuse requests that are each
      ;; perfectly ordinary -- the failure appearing only under load,
      ;; which is where it is hardest to read.
      ;;
      ;; ⚠️ TWO FRAMES OF 700 KB: each is well under the megabyte limit
      ;; and together they are well over it. Both must be answered.
      (let* ((d (start-tagged-daemon! "buffered" #f))
             (big (lambda ()
                    (let* ((head (string-append "(request 1 \"" (tagged-store d) "\" \""))
                           (tail "\" #f wire #f #f outline)")
                           (pad (- 700000 (string-length head) (string-length tail))))
                      (string-append head (make-string pad #\a) tail))))
             (both (string-append (big) "\n" (big) "\n"))
             (answers (ask-until-eof (tagged-socket d) both 20000)))
        (stop-tagged-daemon! d)
        (want "D-33 two ordinary frames arriving together are both served"
              (list (if (and (pair? answers) (= 2 (cadr answers)))
                        'two-answers
                        (list 'answers (and (pair? answers) (cadr answers))))
                    (if (and (pair? answers) (string? (car answers))
                             (not (contains? (car answers) "frame-limit")))
                        'neither-refused
                        'REFUSED-FOR-SIZE))
              '(two-answers neither-refused)))

      ;; ---- D-35 a drain does not finish underneath a local read -------
      ;;
      ;; ⛔ MAIN CANNOT SEE THIS REQUEST AT ALL. `outline` is answered by
      ;; the connection's own process, which never asks `may-execute?`
      ;; and never appears in `state-running` -- so "nothing is running"
      ;; is TRUE while this read is half done. What stops the daemon
      ;; leaving underneath it is the other half of the condition: the
      ;; connection is still registered.
      ;;
      ;; ⚠️ THE SEAM IS WHY THIS IS MEASURABLE. A local read answers in
      ;; tens of milliseconds, so without a park the drain and the read
      ;; cannot be ordered by any clock -- the row would have been green
      ;; whichever way round they really happened. `conn-hold` parks
      ;; inside the read for longer than the sequence being measured.
      ;;
      ;; ⭐ AND THE READING IS AN ORDER, NOT A DURATION: the answer must
      ;; arrive, and the exit must come after it. A row that only asked
      ;; "did it exit 0" passes on a daemon that left before answering.
      (let* ((d (start-tagged-daemon! "conndrain" "conn-hold@conn"))
             (answer #f))
        (spawn (lambda ()
                 (send main (list 'r (ask-until-eof
                                       (tagged-socket d)
                                       (string-append "(request 1 \"" (tagged-store d)
                                                      "\" \"tester\" #f wire #f #f outline)\n")
                                       20000)))))
        (sleep-ms 400)
        (signal-tagged! d "TERM")
        (let gather ()
          (receive
            (after 25000
              (want "D-35 a drain does not finish while a connection-local read is in flight"
                    'no-answer 'never))
            (`(r ,what)
             (let ((code (tagged-exit d 12000)))
               (stop-tagged-daemon! d)
               ;; ⛔ THE READING IS THAT THE ANSWER ARRIVED AT ALL. A
               ;; daemon that declared the drain over while this read was
               ;; parked would have left, taking the connection with it,
               ;; and this client would have got EOF and no answer -- so
               ;; "it was answered" IS "it did not leave underneath it".
               ;; ⚠️ ⛔ AND NOT A COMPARISON OF TWO CLOCKS: the exit can
               ;; only be waited for after the answer has been received,
               ;; so "the exit came later" would be true however the two
               ;; really fell out. A guard whose reference value is read
               ;; after the thing it guards is not a guard.
               (want "D-35 a drain does not finish while a connection-local read is in flight"
                     (list (if (and (string? (car what))
                                    (starts-with? (first-answer-text (car what)) "(ok"))
                               'the-read-was-answered
                               (list 'said (car what)))
                           code)
                     '(the-read-was-answered (exited 0)))))
            (`#(DOWN ,w ,y) (gather)))))

      ;; ⛔ TWIN: AND THAT READING CAN GO RED. The row above is worth
      ;; nothing unless an unanswered read is something this fixture can
      ;; actually see, and the watchdog is what makes it visible: a park
      ;; LONGER than the whole drain budget leaves the clock as the only
      ;; way out, main exits 75, and the read that was parked gets
      ;; nothing. ⭐ Same seam, same sequence, one number different.
      (let* ((d (start-tagged-daemon! "conndrainlong" "conn-hold-long@conn")))
        (spawn (lambda ()
                 (send main (list 'r (ask-until-eof
                                       (tagged-socket d)
                                       (string-append "(request 1 \"" (tagged-store d)
                                                      "\" \"tester\" #f wire #f #f outline)\n")
                                       20000)))))
        (sleep-ms 400)
        (signal-tagged! d "TERM")
        (let ((code (tagged-exit d 15000)))
          (let gather ()
            (receive
              (after 25000
                (want "D-35 TWIN: a park that outlasts the budget ends on the clock, unanswered"
                      'no-report 'never))
              (`(r ,what)
               (stop-tagged-daemon! d)
               (want "D-35 TWIN: a park that outlasts the budget ends on the clock, unanswered"
                     (list (if (and (string? (car what))
                                    (starts-with? (first-answer-text (car what)) "(ok"))
                               'STILL-ANSWERED
                               'the-read-got-nothing)
                           code)
                     '(the-read-got-nothing (exited 75))))
              (`#(DOWN ,w ,y) (gather))))))

      ;; ---- D-34 the writer that routes is the one the request names ---
      ;;
      ;; ⛔ ONE WRITER'S WORK IN ONE PROCESS, HOWEVER THE WRITER WAS
      ;; NAMED. A writer has a process of its own so that its drafts are
      ;; serialised somewhere; two ways of naming the same writer that
      ;; end in two different processes serialise nothing, and the two
      ;; are indistinguishable from the client's side -- the answers are
      ;; the same until the day two of them interleave.
      ;;
      ;; ⚠️ THE INSTRUMENT IS A FAULT THAT ONLY THE WRITER PROCESS HAS.
      ;; `writer-raise` is consulted inside `writer-loop` and nowhere
      ;; else, so an armed build answers one way if the request reached
      ;; that process and another way if it was served by the store --
      ;; ⭐ which is the question, and it is not a question about timing.
      ;;
      ;; ⚠️ AND `drafts` IS A WRITER-LOCAL VERB WITH NO `--writer` IN ITS
      ;; ARGUMENTS: the name is in the envelope only. That is the shape
      ;; that stopped routing when the writer left the argument list.
      (let* ((d (start-tagged-daemon! "wenv" "writer-raise@conn"))
             (said (ask-tagged-as d "w1" "drafts" 8000))
             (trace (tagged-log d)))
        (stop-tagged-daemon! d)
        (want "D-34 a draft verb whose writer is named only by the envelope reaches that writer's process"
              (list (if (and (string? said)
                             (contains? said "writer-actor-down"))
                        'the-writers-process-took-it
                        (list 'said said))
                    (if (contains? trace "injected writer raise")
                        'named-the-reason
                        'NO-SUCH-REASON))
              '(the-writers-process-took-it named-the-reason)))

      ;; ⛔ TWIN: THE ENVELOPE'S WRITER MUST NOT WIDEN THE GATE. Only
      ;; draft verbs belong to a writer's process; everything else is the
      ;; store's. A default applied outside that gate routes every verb
      ;; from a client that happens to name a writer into that writer's
      ;; mailbox, where it queues behind commits for no reason -- and the
      ;; row above is satisfied by it, because it too ends in the
      ;; writer's process.
      ;;
      ;; ⚠️ SAME ARMED BUILD, SAME ENVELOPE, A VERB THE STORE SERVES.
      ;; If `describe` reaches the writer it raises, and this reads it.
      (let* ((d (start-tagged-daemon! "wenvstore" "writer-raise@conn"))
             (said (ask-tagged-as d "w1" "describe" 8000))
             (trace (tagged-log d)))
        (stop-tagged-daemon! d)
        (want "D-34 TWIN: a verb the store serves is not diverted by the envelope's writer"
              (list (if (and (string? said) (starts-with? (first-answer-text said) "(ok"))
                        'served
                        (list 'said said))
                    (if (contains? trace "injected writer raise")
                        'RAISED-IN-A-WRITER
                        'no-writer-was-used))
              '(served no-writer-was-used)))

      ;; ---- D-32 well-formed is judged before draining -----------------
      ;;
      ;; ⛔ FOUR LAYERS, AND THE DAEMON IS NOT EXEMPT FROM THEM (§7.6.50).
      ;; `draining` says "this daemon is going, ask another one" -- an
      ;; answer that invites the caller to try again. A request whose
      ;; arguments do not parse will be refused by every daemon there
      ;; will ever be, so saying `draining` to it sends the caller round a
      ;; loop that cannot end differently.
      ;;
      ;; ⚠️ THE SECOND FRAME IS THE ONE UNDER TEST, and it is malformed in
      ;; a way the PARSER owns -- a repeated `--writer`, which `drafts`
      ;; does not take twice. D-23 above is this row's twin: the same
      ;; harness, a second frame that parses, and `draining` is then the
      ;; right answer to it.
      (let* ((d (start-tagged-daemon! "wellformed" "writer-hold@conn"))
             (both (string-append
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\")\n"
                     "(request 1 \"" (tagged-store d) "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\" \"--writer\" \"w2\")\n")))
        (spawn (lambda () (send main (list 'wellformed (ask-until-eof (tagged-socket d) both 15000)))))
        (sleep-ms 300)
        (signal-tagged! d "TERM")
        (let* ((answers (let wait ()
                          (receive (after 20000 'no-answer)
                                   (`(wellformed ,what) what)
                                   (`#(DOWN ,w ,r) (wait)))))
               (code (tagged-exit d 12000)))
          (stop-tagged-daemon! d)
          (want "D-32 a request that does not parse is told so, not told to try again"
                (list (if (and (pair? answers) (string? (car answers))
                               (contains? (car answers) "duplicate-option"))
                          'said-what-was-wrong
                          (list 'said answers))
                      (if (and (pair? answers) (string? (car answers))
                               (contains? (car answers) "(error draining"))
                          'CALLED-IT-DRAINING
                          'not-draining))
                '(said-what-was-wrong not-draining))))

      ;; ---- D-24 a request queued behind one in a writer ---------------
      ;;
      ;; ⛔ THE EXECUTOR DECIDES, NOT THE CONNECTION. A is parked inside
      ;; the writer's lock; B is in the same writer's mailbox behind it.
      ;; The drain begins while both exist. When A finishes, B has still
      ;; not started -- so B is the case the rule is about, and the
      ;; process that answers it is the writer, which is the only one
      ;; that knows B had not begun.
      ;;
      ;; ⚠️ A IS ALLOWED TO FINISH. It may already have changed something;
      ;; a daemon that refused it after it had begun would be reporting
      ;; work that was done as work that was not.
      (let* ((d (start-tagged-daemon! "wqueued" "writer-hold@conn")))
        (spawn (lambda ()
                 (send main (list 'a (ask-until-eof
                                       (tagged-socket d)
                                       (string-append "(request 1 \"" (tagged-store d)
                                                      "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\")\n")
                                       15000)))))
        (sleep-ms 150)
        (spawn (lambda ()
                 (send main (list 'b (ask-until-eof
                                       (tagged-socket d)
                                       (string-append "(request 1 \"" (tagged-store d)
                                                      "\" \"tester\" #f wire #f #f drafts \"--writer\" \"w1\")\n")
                                       15000)))))
        (sleep-ms 200)
        (signal-tagged! d "TERM")
        (let gather ((a #f) (b #f))
          (if (and a b)
              (let ((code (tagged-exit d 12000)))
                (stop-tagged-daemon! d)
                (want "D-24 a request queued in a writer is refused by the writer, exactly once"
                      (list (if (and (string? (car a))
                                     (starts-with? (first-answer-text (car a)) "(ok"))
                                'the-one-that-started-finished
                                (list 'a-said a))
                            (if (and (string? (car b))
                                     (starts-with? (first-answer-text (car b)) "(error draining"))
                                'the-queued-one-was-refused
                                (list 'b-said b))
                            (if (= 1 (cadr b)) 'exactly-one-answer (list 'answers (cadr b)))
                            ;; ⛔ AND IT IS THE TRANSPORT SPEAKING, NOT THE
                            ;; CORE. Nothing dispatched this request: the
                            ;; writer refused it before it began. An
                            ;; envelope that calls it the core's makes a
                            ;; caller read "the store answered, and this is
                            ;; its answer" -- which is how a refusal became
                            ;; a tool result reported as carried out.
                            (list 'origin (first-answer-origin (car b)))
                            ;; ⛔ AND THE OTHER HALF, IN THE SAME ROW. A is
                            ;; the request that DID run, and its answer is
                            ;; the core's. Without this, "the transport
                            ;; spoke" is satisfied by a build that says so
                            ;; about every answer it writes -- which would
                            ;; turn every ordinary result into something a
                            ;; caller is told was not carried out. The two
                            ;; requests differ in exactly one thing: whether
                            ;; anything dispatched them.
                            (list 'a-origin (first-answer-origin (car a)))
                            code)
                      '(the-one-that-started-finished the-queued-one-was-refused
                        exactly-one-answer (origin transport) (a-origin core)
                        (exited 0))))
              (receive (after 25000 (want "D-24 a request queued in a writer is refused by the writer, exactly once"
                                          (list 'no-answer a b) 'never))
                       (`(a ,what) (gather what b))
                       (`(b ,what) (gather a what))
                       (`#(DOWN ,w ,r) (gather a b))))))

      ;; ---- D-25 a read while somebody else holds the store ------------
      ;;
      ;; ⭐ FIFTY MILLISECONDS, AND IT IS ONLY TRUE BECAUSE READS STOPPED
      ;; TOUCHING THE LOCK. Before the published value existed, a read
      ;; during somebody else's exclusive hold waited for that holder --
      ;; measured on this very fixture at 2950 ms. The promise is not
      ;; that the daemon is fast; it is that a reader asks nobody.
      ;;
      ;; ⚠️ THE TWIN IS A WRITE IN THE SAME WINDOW. Without it, a daemon
      ;; that ignored the store lock altogether would pass this row --
      ;; and would be answering reads quickly by being wrong.
      (let* ((tag (string-append pid-text "-fifty"))
             (held (hold-lock! (string-append store "/lock") 2 tag)))
        (spawn
          (lambda ()
            (send main (list 'slow-write
                             (timed-ask socket
                                        (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \""
                                                       tag "\")\n")
                                        12000)))))
        (let* ((r (timed-ask socket
                             (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n")
                             8000))
               (w (let wait ()
                    (receive (after 20000 'no-answer)
                             (`(slow-write ,what) what)
                             (`#(DOWN ,x ,y) (wait))))))
          (want "D-25 a read is answered in 50 ms while somebody else holds the store"
                (list (car held)
                      (if (< (car r) 50) 'at-once (list 'read-took (car r)))
                      (if (and (string? (cadr r)) (starts-with? (cadr r) "(ok"))
                          'and-answered
                          (list 'read-said (cadr r)))
                      ;; the twin: the write in the same window waited
                      (if (and (pair? w) (> (car w) 1000))
                          'the-write-waited
                          (list 'write-took (and (pair? w) (car w)))))
                '(held at-once and-answered the-write-waited))))

      ;; ---- D-26 a hold longer than the budget -------------------------
      ;;
      ;; ⛔ ONE `store-busy`, ON THE BUDGET, AND ⛔ ONLY FOR THE WRITE. The
      ;; reader in the same window is not waiting for anything and must
      ;; not be made to: that is the whole difference the published value
      ;; buys, and a row that only looked at the write would not see it.
      (let* ((tag (string-append pid-text "-toolong"))
             (held (hold-lock! (string-append store "/lock") 7 tag)))
        (spawn
          (lambda ()
            (send main (list 'refused
                             (ask-until-eof socket
                                            (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \""
                                                           tag "\")\n")
                                            12000)))))
        (sleep-ms 200)
        (let* ((r (timed-ask socket
                             (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)\n")
                             8000))
               (w (let wait ()
                    (receive (after 25000 'no-answer)
                             (`(refused ,what) what)
                             (`#(DOWN ,x ,y) (wait))))))
          (want "D-26 a hold past the budget refuses the write once and leaves the read alone"
                (list (car held)
                      (if (and (pair? w) (string? (car w))
                               (starts-with? (first-answer-text (car w)) "(error store-busy"))
                          'the-write-was-refused
                          (list 'write-said w))
                      (if (and (pair? w) (= 1 (cadr w)))
                          'exactly-once
                          (list 'answers (and (pair? w) (cadr w))))
                      (if (< (car r) 50) 'the-read-was-at-once (list 'read-took (car r))))
                '(held the-write-was-refused exactly-once the-read-was-at-once))))

      ;; ---- D-27 the envelope a forwarding caller actually sends --------
      ;;
      ;; ⛔ ONE FRAME, ONE ANSWER, AND THE CONNECTION STAYS. The bytes here
      ;; are not written out by this row -- they are whatever
      ;; `request-frame` produces, which is what the command line and the
      ;; MCP shell both send. A row that spelled the envelope itself would
      ;; be checking its own copy.
      ;;
      ;; ⚠️ MEASURED ON THE VERSION THAT PACKED IT AT THE CALL SITE:
      ;; `render-wire` already ends with a newline and the caller appended
      ;; a second, so every forwarded request carried an EMPTY FRAME
      ;; behind it. The daemon answered both --
      ;; `(ok (text ""))\n(error bad-request (reason not-a-datum))\n` --
      ;; and closed. Nobody saw it: that caller exits after the first
      ;; answer. ⭐ The reading that separates the two is "how many
      ;; answers, and did the peer close", ⛔ not "was the answer right".
      ;; ⚠️ READING TO EOF IS THE POINT, AND SO IS NOT REACHING IT. A
      ;; well-formed single frame leaves the connection open, so this
      ;; helper times out -- and the timeout is the evidence. The failing
      ;; shape is the opposite: a string means EOF arrived, which is what
      ;; the extra empty frame used to cause.
      (let* ((answers (ask-until-eof socket
                                     (utf8->string (request-frame store 'outline '()
                                                                   (list (cons 'actor "tester"))))
                                     2500))
             (text (cond ((not (pair? answers)) "")
                         ((and (pair? (car answers)) (eq? 'timed-out (caar answers)))
                          (cadr (car answers)))
                         ((string? (car answers)) (car answers))
                         (else "")))
             (closed? (and (pair? answers) (string? (car answers)))))
        (want "D-27 what the shared packer sends is one frame: one answer, connection kept"
              (list (if (and (pair? answers) (= 1 (cadr answers)))
                        'exactly-one-answer
                        (list 'answers (and (pair? answers) (cadr answers))))
                    (if (starts-with? (first-answer-text text) "(ok")
                        'and-it-is-the-answer (list 'said text))
                    (if closed? 'PEER-CLOSED 'connection-kept))
              '(exactly-one-answer and-it-is-the-answer connection-kept)))

      ;; ---- D-28 one frame, two writes ---------------------------------
      ;;
      ;; ⛔ A FRAME IS NOT A WRITE. D-03 is the other direction -- two
      ;; frames arriving in one write; this is one frame split across
      ;; two, ⚠️ with the cut INSIDE a UTF-8 sequence, which is where a
      ;; reader that decoded as it went would break.
      (spawn
        (lambda ()
          (let ((me self))
            (connect! socket)
            (receive
              (after 4000 (send main (list 'split 'no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               (let* ((whole (string->utf8
                               (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f read \"\x6C49;\x5B57;\")\n")))
                      (cut 30)
                      (head (let ((o (make-bytevector cut)))
                              (bytevector-copy! whole 0 o 0 cut) o))
                      (tail (let ((o (make-bytevector (- (bytevector-length whole) cut))))
                              (bytevector-copy! whole cut o 0 (- (bytevector-length whole) cut)) o)))
                 (conn-write! ref head 'one)
                 (sleep-ms 150)
                 (conn-write! ref tail 'two))
               (let hear ((acc ""))
                 (receive
                   (after 6000 (send main (list 'split (list 'timed-out acc))))
                   (`(data ,r ,bv) (send main (list 'split (utf8->string bv))))
                   (`(written ,r ,t ,st) (hear acc))
                   (`(eof ,r) (send main (list 'split 'eof)))
                   (`#(DOWN ,w ,y) (send main (list 'split 'down))))))))))
      (want "D-28 a frame split across two writes is reassembled, cut inside a UTF-8 sequence"
            (let ((said (let wait ()
                          (receive (after 12000 'no-answer)
                                   (`(split ,what) what)
                                   (`#(DOWN ,w ,r) (wait))))))
              (if (and (string? said) (contains? said "unknown-id"))
                  'reassembled-and-dispatched
                  (list 'said said)))
            'reassembled-and-dispatched)

      ;; ---- D-29 a legal datum that is not a request -------------------
      ;;
      ;; ⛔ WELL-FORMED IS NOT THE SAME AS MEANINGFUL. `(hello)` reads
      ;; perfectly; it is simply not the envelope, and it is refused by
      ;; shape rather than by parse failure. ⚠️ D-13's frame was
      ;; unterminated and D-04's was too long -- this one is neither.
      (want "D-29 a datum that reads but is not a request is refused by shape"
            (let ((r (exchange socket (string->utf8 "(hello)\n") line-complete? 4000)))
              (if (and (pair? r) (eq? 'answer (car r)))
                  (if (starts-with? (answer-text (cadr r)) "(error bad-request")
                      'refused-by-shape
                      (list 'said (answer-text (cadr r))))
                  (list 'transport r)))
            'refused-by-shape)

      ;; ---- D-30 two clients writing at once ---------------------------
      ;;
      ;; ⛔ BOTH COMMIT AND THEIR ANSWERS ARE DIFFERENT. The store process
      ;; serialises them, so neither is lost and neither is answered with
      ;; the other's receipt -- a daemon that shared one answer between
      ;; concurrent writers would pass a row that only counted successes.
      (let ((a-title (string-append "D30-A-" pid-text))
            (b-title (string-append "D30-B-" pid-text)))
        (spawn (lambda ()
                 (send main (list 'w1 (timed-ask socket
                                                 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \""
                                                                a-title "\")\n") 12000)))))
        (spawn (lambda ()
                 (send main (list 'w2 (timed-ask socket
                                                 (string-append "(request 1 \"" store "\" \"tester\" #f wire #f #f insert \"--title\" \""
                                                                b-title "\")\n") 12000)))))
        (let gather ((one #f) (two #f))
          (if (and one two)
              (want "D-30 two concurrent writes both commit, with answers of their own"
                    (list (if (starts-with? (cadr one) "(ok") 'first-committed (list 'said (cadr one)))
                          (if (starts-with? (cadr two) "(ok") 'second-committed (list 'said (cadr two)))
                          (if (string=? (cadr one) (cadr two)) 'SAME-RECEIPT 'different-receipts))
                    '(first-committed second-committed different-receipts))
              (receive (after 20000 (want "D-30 two concurrent writes both commit, with answers of their own"
                                          (list 'no-answer one two) 'never))
                       (`(w1 ,x) (gather x two))
                       (`(w2 ,x) (gather one x))
                       (`#(DOWN ,w ,r) (gather one two))))))

      ;; ---- D-31 taking over a socket a killed daemon left behind ------
      ;;
      ;; ⛔ THE NEXT DAEMON TAKES OVER, and the evidence is that it
      ;; answers -- ⚠️ not that the file is there, which it was before it
      ;; started. A daemon that refused because something was already at
      ;; the path would leave a store unusable until somebody tidied up
      ;; by hand.
      (let* ((d (start-tagged-daemon! "takeover" #f))
             (before (ask-tagged d "outline" 8000)))
        (system (string-append "pkill -9 -f " (caddr d) " 2>/dev/null"))
        (sleep-ms 500)
        (let ((left-behind (file-exists? (tagged-socket d))))
          (system (string-append "THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                                 " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                                 " scheme --script " (caddr d) " > " (cadddr d) ".2 2>&1 &"))
          (let up ((k 0))
            (cond ((and (file-exists? (tagged-socket d))
                        (let ((r (ask-tagged d "outline" 3000)))
                          (and (string? (cadr r)) (starts-with? (cadr r) "(ok"))))
                   'up)
                  ((> k 60) 'never)
                  (else (sleep-ms 100) (up (+ k 1)))))
          (let ((after (ask-tagged d "outline" 8000)))
            (system (string-append "pkill -f " (caddr d) " 2>/dev/null"))
            (want "D-31 a daemon takes over the socket a killed one left behind"
                  (list (if (starts-with? (cadr before) "(ok") 'served-before (list 'said (cadr before)))
                        (if left-behind 'socket-was-left 'NO-SOCKET-TO-TAKE-OVER)
                        (if (and (string? (cadr after)) (starts-with? (cadr after) "(ok"))
                            'and-the-new-one-answers
                            (list 'after-said (cadr after))))
                  '(served-before socket-was-left and-the-new-one-answers)))))

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
      ;; ⛔ THE PROCESSES GO FIRST, AND THEY WERE NOT GOING AT ALL. This
      ;; tidy-up removed the FILES and left the daemons that were using
      ;; them running -- found as an orphaned `scheme --script
      ;; /tmp/dmn-run-NNNN.ss`, parent 1, five minutes into a suite run
      ;; that had nothing to do with it, competing for the machine with
      ;; whatever was actually being measured.
      ;;
      ;; ⚠️ BEFORE the `rm`, so nothing is still writing the files being
      ;; removed; and by the same pid prefix, so a run going on beside
      ;; this one is not touched.
      (system (string-append "pkill -f 'dmn-run.*" pid-text "' 2>/dev/null"))
      (system (string-append "pkill -f 'serve /tmp/dmn-store-" pid-text "' 2>/dev/null"))
      (system "sleep 1")
      (system (string-append "rm -rf /tmp/dmn-*" pid-text "* /tmp/dmn-store-" pid-text
                             " /tmp/dmn-run*-" pid-text ".ss /tmp/dmn-log*-" pid-text ".txt"
                             " /tmp/dmn-occupied-" pid-text " /tmp/.dmn-*" pid-text "*.lock"
                             " 2>/dev/null"))
      ;; ⛔ AND IT IS ASSERTED, not assumed. "I issued a kill" is not the
      ;; same claim as "nothing is left running", and it is the second
      ;; one the next run depends on.
      (let ((left (string-append "/tmp/dmn-left-" pid-text ".txt")))
        (system (string-append "pgrep -f 'dmn-run.*" pid-text "' | wc -l | tr -d ' ' > " left))
        (let ((n (guard (e (#t "?"))
                   (let ((t (call-with-input-file left get-string-all)))
                     (if (and (string? t) (> (string-length t) 0))
                         (substring t 0 (- (string-length t) 1))
                         "?")))))
          (system (string-append "rm -f " left))
          (want "D-teardown no daemon this run started is still alive" n "0")))
      (printf "rows: ~a\n~a failures\ndaemon-e1 complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
