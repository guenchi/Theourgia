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

;; THE CHANGE STREAM: a daemon's publications as frames a subscriber reads.
;;
;; A daemon numbers its publications: revision 1 is the state it starts
;; with, and each later publication -- a local commit, a reload, a refresh
;; before a read at a cut -- is the next number. `subscribe changes <rev>`
;; keeps the connection open, and every publication after it arrives on
;; that connection as one line:
;;
;;   (changes (rev n) (daemon token) (from-cut c) (cut c) (items ...))
;;
;; the difference between publication n-1 and publication n, computed by
;; comparing the two reductions. `read --rev` says which publication an
;; answer was read from, so a client can tell which frames its copy has
;; already seen.
;;
;; AN ADDED OR REMOVED BLOCK CARRIES NO FIELD OR POSITION ITEMS in its
;; frame -- a consumer reads a new block whole -- and structural items still
;; apply to it (a block made with no position is (added id) and (conflict id
;; unplaced)).
;;
;; EVERY SUBSCRIBER HERE IS A PROCESS HOLDING ITS OWN SOCKET. It connects,
;; sends the subscription, and keeps every line it reads, in order; a row
;; asks it for its lines and asserts on them. The answers to everything
;; else are asked on connections of their own.
;;
;; A DAEMON THAT DOES NOT COME UP IS NOT A READING. Each daemon is started
;; and its socket waited for; when the socket never appears the row that
;; started it says so and every row after it is meaningless, so the
;; fixture prints NOT A READING and stops rather than go on asking a
;; daemon that is not there.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia log) log-publish! segment-sha writer-directory segment-file-name)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce seal-state)
        (only (theourgia reduce) state-structure reduction-facts)
        (only (theourgia stream-frames) structural-sets change-items)
        (only (theourgia render) render-human render-wire)
        (only (theourgia trace) trace-enabled? trace-enable! trace-event!))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

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
(define (count-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))
;; THE LOAD A BOUNDED WAIT IS UNDER, as a factor of at least 1 that its base
;; bound is multiplied by: the larger of 1 + load1/cores (the machine's) and
;; a measured step ratio -- a fixed unit of work timed now against the same
;; unit timed when this file started. The load average lags a burst by a
;; minute; the step does not. Capped at 8, so a wait that will never end
;; still ends.
;; NOTE: A WAIT THAT GIVES OUT SAYS SO, with how long it waited, its bound
;; and what it saw (wait-gave-out!): the row's own FAIL line shows only the
;; value, and "no frame" under load and "no frame" from a defect read alike.
(define (shell-number command)
  (let ((f (string-append scratch-base "/cs-" pid-text "-number")))
    (system (string-append "(" command ") > '" f "' 2>/dev/null"))
    (let* ((t (file-text f))
           (n (guard (e (#t #f)) (read (open-string-input-port t)))))
      (and (number? n) n))))
(define cores (max 1 (or (shell-number "sysctl -n hw.ncpu || nproc") 1)))
;; NOTE: THE FALLBACK IS INSIDE THE PIPE: `a | awk || b` runs b only when awk
;; fails, and awk does not fail on empty input. sysctl prints "{ 1.2 3.4 5.6 }"
;; (macOS, FreeBSD), /proc/loadavg "1.2 3.4 5.6 ..." (Linux).
(define (load1)
  (or (shell-number "{ sysctl -n vm.loadavg 2>/dev/null || cat /proc/loadavg; } | tr -d '{}' | awk '{print $1}'") 0))
;; NOTE: THE STEP IS TENS OF MILLISECONDS, AND THE BASE IS A MEDIAN OF THREE:
;; a step of a few milliseconds read a ratio of 4 on an idle machine, the
;; clock's millisecond rounding being most of what it measured.
(define (step-ms)
  (let ((t0 (real-time)))
    (let loop ((i 0) (a 0)) (when (< i 20000000) (loop (+ i 1) (fxlogxor a i))))
    (max 1 (- (real-time) t0))))
(define base-step (let ((ms (list-sort < (list (step-ms) (step-ms) (step-ms))))) (cadr ms)))
(define (load-factor)
  (min 8 (max 1 (+ 1 (/ (load1) cores)) (/ (step-ms) base-step))))
;; A base bound in ms, scaled by FACTOR, as the exact integer the waits take.
(define (scaled ms factor) (exact (ceiling (* ms factor))))
(define (wait-gave-out! what waited bound factor observed)
  (printf "WAIT GAVE OUT: ~a after ~a ms (bound ~a ms, load factor ~,2f, load1 ~a on ~a cores): saw ~s~%"
          what waited (exact (round bound)) (inexact factor) (load1) cores observed))
;; A DAEMON'S LOG, ONE LINE AT A TIME. Every line a daemon writes is
;; one datum, and the reader takes each line on its own: a line that is not
;; exactly one datum is left out, and costs only itself. The reader this
;; replaces read the whole text as one stream and stopped at the first error;
;; a trace line torn by another actor's flush then either ended the reading
;; or opened a list that took in every line until its own close came --
;; the retention row read the last publication as 227 of 260 that way, with
;; all 260 framed. torn-trace-lines says when a log holds such a line.
;; -> the index of the newline ending the line that starts at I, or the length.
(define (line-end text i)
  (let ((n (string-length text)))
    (let find ((k i)) (cond ((>= k n) n) ((char=? (string-ref text k) #\newline) k) (else (find (+ k 1)))))))
;; -> the one datum LINE holds, as a one-element list, or #f.
(define (line-datum line)
  (guard (e (#t #f))
    (let* ((p (open-string-input-port line)) (x (read p)))
      (and (not (eof-object? x)) (eof-object? (read p)) (list x)))))
(define (log-data text)
  (let ((n (string-length text)))
    (let loop ((i 0) (out '()))
      (if (>= i n)
          (reverse out)
          (let* ((j (line-end text i)) (one (line-datum (substring text i j))))
            (loop (+ j 1) (if one (cons (car one) out) out)))))))
;; THE TORN LINES OF A LOG: a "(trace " that does not begin its line, and a
;; line beginning "(trace " that is not exactly one datum.
;; -> (count first-offset first-text), the text being that line and the next.
(define (torn-trace-lines text)
  (let ((n (string-length text)) (needle "(trace "))
    (let loop ((i 0) (count 0) (first #f))
      (if (>= i n)
          (list count (and first (car first)) (and first (cdr first)))
          (let* ((j (line-end text i))
                 (line (substring text i j))
                 (torn (or (and (> (string-length line) 1) (contains? (substring line 1 (string-length line)) needle))
                           (and (>= (string-length line) (string-length needle))
                                (string=? (substring line 0 (string-length needle)) needle)
                                (not (line-datum line))))))
            (loop (+ j 1) (if torn (+ count 1) count)
                  (or first (and torn (cons i (substring text i (if (< j n) (line-end text (+ j 1)) n)))))))))))
;; THE NUMBERS AFTER "(trace <name> " IN THE TEXT, wherever they stand, read
;; without the reader: a witness that a torn line cannot hide.
(define (raw-trace-numbers text name)
  (let* ((needle (string-append "(trace " (symbol->string name) " ")) (k (string-length needle)) (n (string-length text)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i k) n) (reverse out))
            ((string=? (substring text i (+ i k)) needle)
             (let digits ((j (+ i k)))
               (if (and (< j n) (char-numeric? (string-ref text j)))
                   (digits (+ j 1))
                   (loop j (if (> j (+ i k)) (cons (string->number (substring text (+ i k) j)) out) out)))))
            (else (loop (+ i 1) out))))))
;; Printed, not asserted: a torn line is evidence that later runs remove.
(define (note-torn! label text)
  (let ((t (torn-trace-lines text)))
    (when (> (car t) 0)
      (printf "NOTE ~a: ~a torn trace line(s), the first at byte ~a: ~s~%" label (car t) (cadr t) (caddr t)))))

;; ---- daemons ------------------------------------------------------------------
;;
;; -> (store socket runner log pidfile rcfile) once the socket exists. ENV is
;; prepended to the command (a fault, a hold, a send buffer); every daemon
;; runs the injection build with its trace on, so a row can read what it did.
(define daemon-count 0)
(define all-stores '())
;; the cost store's reduction before its three timed sets (for the shortcut row)
(define cost-before #f)
;; NEVER: NO TIMER UNLESS THE ROW IS ABOUT THE TIMER. A daemon's once-a-second
;; probe folds an outside change on its own schedule, and a read's queued
;; reload then refolds it into an empty frame: every row reading frames by
;; revision, count or line raced it. So the daemons here start with
;; THEOURGIA_PROBE=off (test-only) and a change folds only when a row asks
;; for it; the rows about the timer start theirs with start-timer-daemon!,
;; the variable unset, the prober as in use.
(define (start-daemon! tag env) (start-daemon* tag (string-append "THEOURGIA_PROBE=off " env)))
;; THE VARIABLE UNSET, not inherited: this fixture may itself run with it set.
(define (start-timer-daemon! tag env) (start-daemon* tag (string-append "env -u THEOURGIA_PROBE " env)))
(define (start-daemon* tag env)
  (set! daemon-count (+ daemon-count 1))
  (let* ((t (string-append pid-text "-" (number->string daemon-count) "-" tag))
         (st (string-append scratch-base "/cs-" t))
         (sk (string-append socket-base "/cs-" t ".sock"))
         (rn (string-append scratch-base "/cs-" t ".sc"))
         (lg (string-append scratch-base "/cs-" t ".log"))
         (pf (string-append scratch-base "/cs-" t ".pid"))
         (rc (string-append scratch-base "/cs-" t ".rc")))
    (system (string-append "rm -rf " st " " sk " " pf " " rc "; mkdir -p " st))
    (set! all-stores (cons st all-stores))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
                (string-append "(call-with-output-file \"" pf
                               "\" (lambda (p) (write (get-process-id) p)) 'truncate)")
                (string-append "(rpc-dispatch \"" st "\" '(init) \"tester\")")
                (string-append "(serve \"" st "\" \"" sk "\")")))))
    (system (string-append "env THEOURGIA_TRACE=1 THEOURGIA_INJECT=on " env " "
                           "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " sh -c 'scheme --script " rn " > " lg " 2>&1; echo $? > " rc "' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg pf rc))
            ((> k 400)
             (printf "NOT A READING: the daemon ~a never created its socket in 20 s~%~a~%" t (file-text lg))
             (exit 2))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (d-store d) (list-ref d 0))
(define (d-socket d) (list-ref d 1))
(define (d-log d) (file-text (list-ref d 3)))
(define (d-pid d)
  (let ((text (file-text (list-ref d 4))))
    (and (> (string-length text) 0) (string->number text))))
(define (stop-daemon! d)
  (system (string-append "pkill -f " (list-ref d 2) " 2>/dev/null")))
;; Sends SIGTERM, the drain, and waits for the exit code.
(define (drain-daemon! d)
  (let ((pid (d-pid d)))
    (when pid (system (string-append "kill -TERM " (number->string pid))))
    (let wait ((k 0))
      (let ((text (file-text (list-ref d 5))))
        (cond ((> (string-length text) 0) 'exited)
              ((> k 200) 'still-running)
              (else (sleep-ms 50) (wait (+ k 1))))))))

;; ---- asking -------------------------------------------------------------------

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))
(define (quote-arg s) (format "~s" s))
(define (envelope store verb args)
  (string-append "(request 1 " (quote-arg store) " \"tester\" #f wire #f #f " (symbol->string verb)
                 (apply string-append (map (lambda (a) (string-append " " (quote-arg a))) args))
                 ")\n"))
;; The answer's stdout as a datum, or (transport ...) when none came.
(define (unwrap-answer text)
  (let ((datum (guard (e (#t #f)) (read (open-string-input-port text)))))
    (if (and (pair? datum) (eq? 'answer (car datum)))
        (let ((hit (assq 'stdout (cdr datum))))
          (if (and (pair? hit) (string? (cadr hit)))
              (let ((inner (guard (e (#t #f)) (read (open-string-input-port (cadr hit))))))
                (or inner (cadr hit)))
              datum))
        (list 'not-an-answer text))))
(define (ask d verb . args)
  (let ((r (exchange (d-socket d) (string->utf8 (envelope (d-store d) verb args)) line-complete? 15000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (unwrap-answer (utf8->string (cadr r)))
        (list 'transport r))))
(define (new-id a)
  (let ((ev (and (pair? a) (assq 'events (cdr a)))))
    (and ev (let ((e (car (cadr ev)))) (string-append (car e) "." (number->string (cdr e)))))))
(define (insert! d title) (new-id (ask d 'insert "--title" title)))

;; ---- subscribers --------------------------------------------------------------
;;
;; A process that connects, sends `subscribe changes <rev> [<token>]`, and
;; keeps every line it reads, in order. Messages it answers:
;;   (lines <from>)   -> (lines <list of strings>) to <from>
;;   (write <text>)   writes TEXT on the same connection
;;   (pause)/(resume) stops and restarts reading (the socket fills)
;;   (close)          closes the connection and ends
;; It ends by itself on EOF, keeping what it read, and still answers (lines).
;; OPTS: 'paused (reading starts only on (resume)), a string (bytes sent in
;; the same write after the subscription, which the stream must answer too),
;; and (before <text>) (a request sent ahead of the subscription on the same
;; connection, whose answer is then the first line).
(define (spawn-subscriber! d args . opts)
  (let ((text (string-append (let ((b (find (lambda (o) (and (pair? o) (eq? (car o) 'before))) opts))) (if b (cadr b) ""))
                             (envelope (d-store d) 'subscribe args)
                             (apply string-append (filter string? opts))))
        (paused? (memq 'paused opts)))
    (spawn
      (lambda ()
        ;; A PAUSED READER HAS NOT STARTED READING, and a connection that has
        ;; not started is closed by its own adapter at net.sc's idle deadline
        ;; (5 s): a row holding a replay longer than that would read its own
        ;; reader closing. Paused, the deadline is a minute.
        (if paused? (connect! (d-socket d) 60000) (connect! (d-socket d)))
        (receive
          (after 4000 (let idle () (receive (`(lines ,from) (send from (list 'lines '(no-connect))) (idle)))))
          (`(connected ,p ,ref)
           (unless paused? (conn-read-start! ref))
           (conn-write! ref (string->utf8 text) 'subscribe)
           (let loop ((acc "") (done '()) (open? #t))
             (receive
               (`(written ,r ,t ,st) (loop acc done open?))
               (`(data ,r ,bv)
                (let split ((s (string-append acc (utf8->string bv))) (done done))
                  (let ((nl (let find ((i 0)) (cond ((>= i (string-length s)) #f)
                                                    ((char=? (string-ref s i) #\newline) i)
                                                    (else (find (+ i 1)))))))
                    (if nl
                        (split (substring s (+ nl 1) (string-length s)) (cons (substring s 0 nl) done))
                        (loop s done open?)))))
               (`(eof ,r) (loop acc (if (> (string-length acc) 0) (cons (string-append acc "<partial>") done) done) #f))
               (`#(DOWN ,w ,why) (loop acc done #f))
               (`(lines ,from) (send from (list 'lines (reverse (if open? done (cons "<eof>" done))))) (loop acc done open?))
               (`(write ,t) (when open? (conn-write! ref (string->utf8 t) 'extra)) (loop acc done open?))
               (`(pause) (conn-read-stop! ref) (loop acc done open?))
               (`(resume) (conn-read-start! ref) (loop acc done open?))
               (`(close) (conn-close! ref))))))))))
(define (sub-lines sub)
  (send sub (list 'lines self))
  (receive (after 3000 '(no-answer-from-subscriber))
           (`(lines ,ls) ls)))
;; Waits until the subscriber holds at least N lines, or MS pass.
(define (await-lines sub n ms)
  (let wait ((k 0))
    (let ((ls (sub-lines sub)))
      (cond ((>= (length (filter (lambda (l) (not (equal? l "<eof>"))) ls)) n) ls)
            ((> k (div ms 50)) ls)
            (else (sleep-ms 50) (wait (+ k 1)))))))
;; The acceptance is the one enveloped line; every later line is a bare datum.
(define (acceptance-of lines)
  (if (and (pair? lines) (string? (car lines)) (not (equal? (car lines) "<eof>")))
      (unwrap-answer (car lines))
      'no-acceptance))
(define (frames-of lines)
  (map (lambda (l) (guard (e (#t (list 'unreadable l))) (read (open-string-input-port l))))
       (filter (lambda (l) (not (equal? l "<eof>"))) (if (pair? lines) (cdr lines) '()))))
;; A CLAUSE IS A LIST AFTER THE TAG; an error's second element is a symbol,
;; so the lookup passes over what is not a pair.
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum)
                (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr datum)))))
    (and c (cdr c))))
(define (frame-rev f) (let ((c (clause 'rev f))) (and c (car c))))
;; A frame's items as a sorted list of their printed forms, so a row
;; compares sets without caring about their order.
(define (item-set f)
  (let ((c (clause 'items f)))
    (if c (list-sort string<? (map (lambda (i) (format "~s" i)) c)) 'no-items)))
(define (expected-set . items) (list-sort string<? (map (lambda (i) (format "~s" i)) items)))
(define (token-of acceptance) (let ((c (clause 'daemon acceptance))) (and c (car c))))
(define (current-of acceptance) (let ((c (clause 'current acceptance))) (and c (car c))))

;; ---- records of another writer --------------------------------------------------
;;
;; A row that needs two writers, or a record the command line cannot make
;; (a mirrored move to an exact place, a concurrent write), publishes that
;; writer's record into the daemon's store and asks a read on another
;; connection: the read's probe sees the disk changed and asks the store
;; process for a reload, which publishes.
(define (mirror! d writer seq deps payload)
  (let ((bytes (encode-record seq (+ 1789000000000 seq) "peer" deps (storable-encode payload))))
    (log-publish! (d-store d) writer seq bytes (segment-sha bytes))))
(define (mirror-as! d writer seq deps actor payload)
  (let ((bytes (encode-record seq (+ 1789000000000 seq) actor deps (storable-encode payload))))
    (log-publish! (d-store d) writer seq bytes (segment-sha bytes))))
;; NEVER: A POKE IS A REQUEST, NOT A FOLD. The read probes and, ONLY when the
;; disk differs from the published state and the store process is not busy
;; with a message, sends the store process a reload before it answers (none
;; for an unchanged or absent snapshot, a probe that fails, or a busy store
;; process); the answer is from the published state, and whether that reload
;; has run by the time it arrives is not known. So when poke! returns a
;; reload is queued, ahead of anything sent to the store process after it,
;; if the disk differed and the store process was idle. A row that needs the fold's result waits for it by a
;; condition (frame!, frame-settled, said-frame-after, await-lines,
;; await-trace, a hold's file); a row that needs only the request queued may
;; rely on the order. A reload queued behind another still refolds the same
;; log, so an EMPTY frame can follow a change's frame: a row reads a change's
;; frame by what it says, not by its position. But a read made while the
;; store process is handling a message -- a publication may be in progress --
;; queues nothing (daemon.sc, publication-pending): the next read, or the
;; timer's next tick, asks again. So a row that needs a fold waits for the
;; previous one before it pokes.
(define (poke! d) (ask d 'outline))
;; One segment of WRITER holding N new blocks: one reload, one frame with N
;; (added ...) items, which is how a row makes a frame big enough to fill a
;; socket's buffers.
(define (mirror-many! d writer n)
  (let ((bytes (string->utf8
                 (apply string-append
                        (let loop ((k 1) (out '()))
                          (if (> k n) (reverse out)
                              (loop (+ k 1)
                                    (cons (utf8->string
                                            (encode-record k (+ 1789000000000 k) "peer" '()
                                                           (storable-encode
                                                             (list 'put (list (cons 'kind 'section)
                                                                              (cons 'title (string-append "bulk " (number->string k)))
                                                                              (cons 'parent 'root) (cons 'ord k))))))
                                          out))))))))
    (log-publish! (d-store d) writer 1 bytes (segment-sha bytes))))
;; Publishes big frames, one per reload, until the daemon traces a write it
;; could not finish -- `(write-pending conn)` -- or LIMIT publications pass.
;; -> the number made, or (never-pending k).
(define (fill-until-pending! d limit)
  (let loop ((k 1))
    (cond ((contains? (d-log d) "(trace write-pending") (- k 1))
          ((> k limit) (list 'never-pending (- k 1)))
          ;; a writer id is 8 base36 characters: bulk0001, bulk0002, ...
          ;; ONE PUBLICATION PER WRITER: the next is written only once this
          ;; one's reload has published, so no reload folds two of them and
          ;; the number made is the number of frames.
          ;; A PUBLICATION NOT SEEN ENDS THE FILL with why, not a next writer:
          ;; the callers read a number made, and this is not one.
          (else (let* ((before (or (last-published d) 0))
                       (factor (load-factor)) (bound (scaled 10000 factor)) (t0 (real-time)))
                  (mirror-many! d (string-append "bulk" (let ((t (number->string k))) (string-append (make-string (- 4 (string-length t)) #\0) t))) 300)
                  (poke! d)
                  (let wait ()
                    (cond ((> (or (last-published d) 0) before) (loop (+ k 1)))
                          ((> (- (real-time) t0) bound)
                           (wait-gave-out! "fill-until-pending's publication" (- (real-time) t0) bound factor k)
                           (list 'publication-not-seen k))
                          (else (sleep-ms 50) (wait)))))))))
;; Stops a daemon and starts another on the SAME store and socket.
(define (restart-daemon! d env)
  (stop-daemon! d)
  (let wait ((k 0)) (unless (or (> (string-length (file-text (list-ref d 5))) 0) (> k 100)) (sleep-ms 50) (wait (+ k 1))))
  (system (string-append "rm -f " (d-socket d) " " (list-ref d 4) " " (list-ref d 5)))
  (system (string-append "THEOURGIA_TRACE=1 THEOURGIA_INJECT=on THEOURGIA_PROBE=off " env " "
                         "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                         " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                         " sh -c 'scheme --script " (list-ref d 2) " > " (list-ref d 3) " 2>&1; echo $? > " (list-ref d 5) "' &"))
  (let up ((k 0))
    (cond ((file-exists? (d-socket d)) d)
          ((> k 400) (printf "NOT A READING: the restarted daemon never created its socket~%") (exit 2))
          (else (sleep-ms 50) (up (+ k 1))))))
;; The revisions of a subscriber's frames, in the order they arrived.
(define (revs-of sub) (map frame-rev (frames-of (sub-lines sub))))
;; The applied cut a frame names, as the deps of a record that covers it.
(define (frame-cut f) (let ((c (clause 'cut f))) (and c (car c))))
;; The last revision the daemon's trace says it published.
(define (last-published d)
  (let loop ((ds (log-data (d-log d))) (n #f))
    (cond ((null? ds) n)
          ((and (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)) (eq? (cadar ds) 'published))
           (loop (cdr ds) (caddar ds)))
          (else (loop (cdr ds) n)))))
;; THE FRAME OF THE PUBLICATION AN ACTION MADE, BY THE ACTION'S OWN RECORD:
;; MARK is that record, (<writer> . <seq>), and the action's publication is
;; the first frame past AFTER whose cut covers it. While anyone is
;; subscribed the daemon probes once a second, and a reload queued behind
;; another refolds the same log: such a reload publishes an EMPTY frame
;; (kept, sent, counted like any other), and it can land before the
;; action's own. (A read during a publication in progress defers its
;; request rather than queue one, so a read is no longer how such a reload
;; is made; the timer's tick, or an outside change with no record, still
;; makes one.) So of the frames past AFTER up to the covering one, the
;; one with items is the action's; none with items, the covering one; two
;; with items is not one action's frame, and says so.
;; NEVER: NOT "THE LAST REVISION HELD STILL FOR 300 MS". That was the rule,
;; and under load it took a stale empty frame for the action's when the
;; action's own reload took longer than 300 ms to publish -- the row then
;; read (), and nothing said why. A condition does not depend on how fast
;; the machine is; the bound only ends a wait that will not be answered.
(define (covers? f mark)
  (let ((c (frame-cut f)))
    (and (list? c) (let ((e (assoc (car mark) c))) (and e (>= (cdr e) (cdr mark)))))))
(define (frame-settled d sub after mark)
  (let* ((factor (load-factor)) (bound (scaled 10000 factor)) (t0 (real-time)))
    (let look ()
      (let* ((fs (filter (lambda (f) (let ((r (frame-rev f))) (and r (> r after))))
                         (frames-of (sub-lines sub))))
             (hit (find (lambda (f) (covers? f mark)) fs)))
        (cond (hit
               (let* ((upto (filter (lambda (f) (<= (frame-rev f) (frame-rev hit))) fs))
                      (said (filter (lambda (f) (not (equal? (clause 'items f) '()))) upto)))
                 (cond ((null? said) hit)
                       ((null? (cdr said)) (car said))
                       (else (list 'several-frames-with-items (map frame-rev said))))))
              ((> (- (real-time) t0) bound)
               (let ((seen (list 'wanted mark 'past after 'last-published (last-published d)
                                 'frames (map (lambda (f) (list (frame-rev f) (frame-cut f))) fs))))
                 (wait-gave-out! "frame-settled" (- (real-time) t0) bound factor seen)
                 (list 'no-frame seen)))
              (else (sleep-ms 50) (look)))))))
;; The record a request's answer says it wrote, (<writer> . <seq>).
(define (mark-of answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (pair? (cadr ev)) (car (cadr ev)))))
;; WAITS until the daemon's log holds at least N lines containing NEEDLE, or
;; MS pass; the rows then read the count themselves.
(define (await-trace d needle n ms)
  (let wait ((k 0))
    (unless (or (>= (count-of (d-log d) needle) n) (> k (div ms 50)))
      (sleep-ms 50) (wait (+ k 1)))))
;; THE NUMBERS A TRACE CARRIES: for each (trace <name> <n> ...) line of the
;; daemon's log, <n>, in order.
(define (trace-values d name)
  (let loop ((ds (log-data (d-log d))) (out '()))
    (cond ((null? ds) (reverse out))
          ((and (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)) (eq? (cadar ds) name) (pair? (cddar ds)))
           (loop (cdr ds) (cons (caddar ds) out)))
          (else (loop (cdr ds) out)))))
(define (rev-clause a) (clause 'rev a))
(define (clause-names a) (if (pair? a) (map (lambda (c) (if (pair? c) (car c) c)) (cdr a)) '()))

;; THE CONSUMER'S APPLICATION RULE BY REVISION, the same as the client
;; fixture's. held: alist object -> revision. -> (decision . held'), decision
;; one of apply, skip, reread.
(define (consume held object rev)
  (let ((h (cdr (or (assoc object held) (cons object #f)))))
    (cond ((not (and h rev)) (cons 'reread held))
          ((<= rev h) (cons 'skip held))
          ((= rev (+ h 1)) (cons 'apply (cons (cons object rev) (remp (lambda (e) (equal? (car e) object)) held))))
          (else (cons 'reread held)))))

;; THE FRAME'S COMPARISON WITHOUT ITS SHORTCUTS, the reference a row compares
;; the product's with: stream-frames' change-items as it was before the
;; equal? skips, copied here and kept here, so the reference is not the
;; code under test. Its accessors are the facts' vector slots.
(define (fact-blocks facts) (map (lambda (v) (cons (vector-ref v 0) v)) (car facts)))
(define (blk-tomb v) (vector-ref v 1))
(define (blk-fields v) (vector-ref v 2))
(define (blk-position v) (vector-ref v 3))
(define (reference-change-items old new old-sets new-sets)
  (if (eq? old new)
      '()
      (let* ((of (reduction-facts old)) (nf (reduction-facts new))
             (old-blocks (fact-blocks of)) (new-blocks (fact-blocks nf))
             (old-links (cadr of)) (new-links (cadr nf))
             (ot (make-hashtable string-hash string=?))
            (nt (make-hashtable string-hash string=?))
            (out '()))
        (define (emit! item) (set! out (cons item out)))
        (define (live b) (and b (not (blk-tomb b)) b))
        (define (same-set? a b)
          (and (= (length a) (length b))
               (for-all (lambda (x) (member x b)) a)))
        (define (settled-of cs part)
          (and (= 1 (length cs)) (part (car (car cs)))))
        (define (crossing! id what o n differ?)
          (let ((so (length o)) (sn (length n)))
            (cond ((and (<= so 1) (> sn 1)) (emit! (list 'conflict id what)))
                  ((and (> so 1) (<= sn 1)) (emit! (list 'resolved id what)))
                  ((and (> so 1) (> sn 1) differ?) (emit! (list 'conflict id what))))))
        (define (compare-live! id ob nb)
          (let ((ofs (blk-fields ob)) (nfs (blk-fields nb)))
            (for-each
              (lambda (f)
                (let* ((o (let ((e (assq f ofs))) (if e (cdr e) '())))
                       (n (let ((e (assq f nfs))) (if e (cdr e) '())))
                       (differ? (not (same-set? o n))))
                  (when differ? (emit! (list 'changed id f)))
                  (crossing! id f o n differ?)))
              (let union ((fs (map car ofs)) (acc '()))
                (cond ((null? fs)
                       (append (reverse acc) (filter (lambda (f) (not (memq f acc))) (map car nfs))))
                      ((memq (car fs) acc) (union (cdr fs) acc))
                      (else (union (cdr fs) (cons (car fs) acc)))))))
          (let* ((o (blk-position ob)) (n (blk-position nb)) (differ? (not (same-set? o n))))
            (when differ?
              (emit! (list 'changed id 'position))
              (unless (equal? (settled-of o car) (settled-of n car)) (emit! (list 'changed id 'parent)))
              (unless (equal? (settled-of o cdr) (settled-of n cdr)) (emit! (list 'changed id 'ord))))
            (crossing! id 'position o n differ?)))
        (for-each (lambda (e) (hashtable-set! ot (car e) (cdr e))) old-blocks)
        (for-each (lambda (e) (hashtable-set! nt (car e) (cdr e))) new-blocks)
        (for-each
          (lambda (id)
            (let ((ob (live (hashtable-ref ot id #f))) (nb (live (hashtable-ref nt id #f))))
              (cond ((and ob (not nb)) (emit! (list 'removed id)))
                    ((and nb (not ob)) (emit! (list 'added id)))
                    ((and ob nb) (compare-live! id ob nb)))))
          (append (map car old-blocks)
                  (filter (lambda (id) (not (hashtable-ref ot id #f))) (map car new-blocks))))
        (for-each
          (lambda (key kind)
            (let ((o (cdr (assq key old-sets))) (n (cdr (assq key new-sets))))
              (for-each (lambda (id) (unless (member id o) (emit! (list 'conflict id kind)))) n)
              (for-each (lambda (id) (unless (member id n) (emit! (list 'resolved id kind)))) o)))
          '(conflicts orphans unplaced nested-documents)
          '(cycle orphan unplaced nested))
        (let ((ol (make-hashtable equal-hash equal?)) (nl (make-hashtable equal-hash equal?)))
          (for-each (lambda (l) (hashtable-set! ol l #t)) old-links)
          (for-each (lambda (l) (hashtable-set! nl l #t)) new-links)
          (for-each (lambda (l)
                      (unless (hashtable-ref ol l #f)
                        (emit! (list 'edge-added (car l) (cadr l) (caddr l) (cons 'event (cadddr l))))))
                    new-links)
          (for-each (lambda (l)
                      (unless (hashtable-ref nl l #f)
                        (emit! (list 'edge-removed (car l) (cadr l) (caddr l)))))
                    old-links))
        (reverse out))))

;; Waits for the subscriber's next frame after the N frames it already has.
(define (next-frame sub n)
  (let* ((ls (await-lines sub (+ n 2) 6000))
         (fs (frames-of ls)))
    (if (> (length fs) n) (list-ref fs n) (list 'no-frame (length fs) ls))))
;; THE FIRST FRAME PAST REV AFTER THAT SAYS SOMETHING (has items), waiting
;; until one arrives or the bound passes: an empty refold frame before or
;; after it is passed over, where a position would count it.
(define (said-frame-after sub after)
  (let* ((factor (load-factor)) (bound (scaled 10000 factor)) (t0 (real-time)))
    (let wait ()
      (let* ((fs (frames-of (sub-lines sub)))
             (hit (find (lambda (f) (let ((r (frame-rev f)))
                                      (and r (> r after) (pair? (clause 'items f)))))
                        fs)))
        (cond (hit hit)
              ((> (- (real-time) t0) bound)
               (let ((seen (list 'past after 'frames (map frame-rev fs))))
                 (wait-gave-out! "said-frame-after" (- (real-time) t0) bound factor seen)
                 (list 'no-frame seen)))
              (else (sleep-ms 50) (wait)))))))

;; ---- TL: a trace line is written whole -------------------------------------
;;
;; THE FLOOD: a child program in which K actors each write M trace lines,
;; every one preempted somewhere inside its event -- each actor sets the timer
;; to a few ticks just before calling trace-event!, so the interrupt lands in
;; the middle of building or writing the line. Its stderr goes to a file, and
;; the row reads that file line by line. Uses only trace-event!, which every
;; version has, so the same program reads the code before the line sink,
;; where seven writes to the shared port per line tore lines: 610 of 4000
;; lines torn in each of five probe runs, and none after.
(define flood-actors 8)
(define flood-events 500)
(define flood-ticks 40)
(define (run-flood! tag)
  (let* ((dir (string-append scratch-base "/cs-" pid-text "-" tag))
         (prog (string-append dir "/flood.sc"))
         (err (string-append dir "/flood.err"))
         (out (string-append dir "/flood.out")))
    (system (string-append "rm -rf '" dir "'; mkdir -p '" dir "'"))
    (call-with-output-file prog
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia sched) (only (theourgia ffi) trace-event!))"
                "(start-scheduler"
                "  (lambda ()"
                "    (let ((me self))"
                (format "      (do ((i 0 (+ i 1))) ((= i ~a))" flood-actors)
                "        (let ((name (string->symbol (string-append \"a\" (number->string i)))))"
                "          (spawn (lambda ()"
                (format "                   (do ((j 0 (+ j 1))) ((= j ~a))" flood-events)
                (format "                     (set-timer (+ 1 (mod (+ (* 7 j) (* 13 (string-length (symbol->string name)))) ~a)))" flood-ticks)
                "                     (trace-event! 'flood name j))"
                "                   (send me '(done))))))"
                (format "      (let wait ((n 0)) (unless (= n ~a) (receive (`(done) (wait (+ n 1))))))" flood-actors)
                "      (flush-output-port (current-error-port))"
                "      (display \"flood complete\") (newline)"
                "      (exit 0))))")))
      'truncate)
    ;; TRACING IS TURNED ON BY THE ENVIRONMENT, not by a call: loading the
    ;; platform layer sets the switch from THEOURGIA_TRACE and overwrites a
    ;; call made before it (ffi.sc, trace-switch-installed). The first probe
    ;; of this row called trace-enable! and wrote no line at all.
    (system (string-append "env THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script '" prog "' > '" out "' 2> '" err "' < /dev/null"))
    (let* ((text (file-text err))
           (lines (let loop ((i 0) (acc '()))
                    (if (>= i (string-length text)) (reverse acc)
                        (let ((j (line-end text i))) (loop (+ j 1) (cons (substring text i j) acc))))))
           (whole (filter (lambda (l) (let ((d (line-datum l)))
                                        (and d (pair? (car d)) (= 4 (length (car d)))
                                             (eq? (caar d) 'trace) (eq? (cadar d) 'flood)
                                             (symbol? (caddr (car d))) (integer? (cadddr (car d)))))) lines)))
      (list (contains? (file-text out) "flood complete") (length whole) (car (torn-trace-lines text))
            (and (> (car (torn-trace-lines text)) 0) (caddr (torn-trace-lines text)))))))

(start-scheduler
  (lambda ()
    ;; ==== TL: a trace line is written whole ====
    (printf "~%== TL: a trace line is written whole ==~%")
    ;; ONE STRING PER EVENT, TO THE SINK: a counting sink stands in for the
    ;; installed one while one event is traced, with interrupts disabled so
    ;; no other actor's event can reach it, and both are put back. The sink
    ;; is looked up by name so that the code before the line sink, which has
    ;; none, reads as `no-line-sink` and the rows after it still run.
    (let* ((env (guard (e (#t #f)) (environment '(theourgia trace))))
           (get (and env (guard (e (#t #f)) (eval 'trace-line-sink env))))
           (put (and env (guard (e (#t #f)) (eval 'trace-line-sink-set! env)))))
      (want "TL-1 trace-event! hands the sink the port and the whole line, once per event"
            (if (not (and get put))
                'no-line-sink
                (with-interrupts-disabled
                  (let ((was-on (trace-enabled?)) (was (get)) (calls '()))
                    (put (lambda (p line) (set! calls (cons (list (eq? p (current-error-port)) line) calls))))
                    (trace-enable! #t)
                    (trace-event! 'sink-probe "a b" 76)
                    (trace-enable! was-on)
                    (put was)
                    (reverse calls))))
            '((#t "(trace sink-probe a b 76)\n"))))
    (want "TL-2 a flood of trace lines from actors preempted inside their events: every line whole, none torn"
          (run-flood! "flood")
          (list #t (* flood-actors flood-events) 0 #f))

    ;; ==== ACCEPT: the subscription's acceptance and its refusals ====
    (printf "~%== ACCEPT: subscribe changes <rev> ==~%")
    (let* ((d (start-daemon! "f1" ""))
           (s0 (spawn-subscriber! d '("changes" "0")))
           (ls (await-lines s0 1 5000))
           (acc (acceptance-of ls))
           (token (token-of acc)))
      (want "ACCEPT-1 after start-up, subscribe changes 0 is accepted at current 1, naming the daemon"
            (list (and (pair? acc) (car acc)) (clause 'subscribed acc) (string? token) (clause 'current acc))
            (list 'ok '(0) #t '(1)))
      (sleep-ms 500)
      (want "ACCEPT-2 and replays nothing: no line follows the acceptance"
            (length (filter (lambda (l) (not (equal? l "<eof>"))) (sub-lines s0)))
            1)
      (want "ACCEPT-3 a revision that is not a number, a negative one and one past current are invalid"
            (map (lambda (rev) (let ((a (acceptance-of (await-lines (spawn-subscriber! d (list "changes" rev)) 1 5000))))
                                 (and (pair? a) (list (car a) (cadr a) (caddr a)))))
                 '("x" "-1" "2"))
            '((error bad-request invalid-revision) (error bad-request invalid-revision) (error bad-request invalid-revision)))
      ;; A TRANSPORT OPTION ON THE STREAM'S OWN ROUTE: the daemon hands
      ;; a subscription to the store past the dispatcher's refusal, so before
      ;; this refusal `--help` after revision 0 was a resume token and was
      ;; accepted.
      ;; Each is refused by name with the dispatcher's datum. Any other answer
      ;; is read as its head, so a red shows what was answered instead.
      (want "SUB-T1 a transport option after the revision is refused by name, as the dispatched routes refuse it"
            (map (lambda (args) (let ((a (acceptance-of (await-lines (spawn-subscriber! d args) 1 5000))))
                                  (if (and (list? a) (>= (length a) 3) (eq? (car a) 'error))
                                      (list (car a) (cadr a) (caddr a) (clause 'option a) (clause 'belongs-to a))
                                      (list 'answered (if (pair? a) (car a) a) (and (pair? a) (pair? (cdr a)) (cadr a))))))
                 '(("changes" "0" "--wire") ("changes" "0" "--help") ("changes" "0" "-h") ("changes" "0" "--store" "elsewhere")
                   ("changes" "0" "--actor" "someone") ("changes" "0" "--socket" "x.sock") ("changes" "0" "-h" "--wire")))
            '((error bad-request transport-option-in-rpc ("--wire") (shell))
              (error bad-request transport-option-in-rpc ("--help") (shell))
              (error bad-request transport-option-in-rpc ("-h") (shell))
              (error bad-request transport-option-in-rpc ("--store") (shell))
              (error bad-request transport-option-in-rpc ("--actor") (shell))
              (error bad-request transport-option-in-rpc ("--socket") (shell))
              (error bad-request transport-option-in-rpc ("-h") (shell))))
      ;; TWIN: after `--` the same spellings are text, on both routes. On the
      ;; stream's route a fourth argument is the usage, as before; on the
      ;; dispatched route, which hands the handler positionals with `--`
      ;; removed, `--help` is a third argument and the answer needs-daemon.
      (want "SUB-T2 after --, a transport spelling is text: the stream route answers the usage, the dispatched route needs-daemon"
            (list (let ((a (acceptance-of (await-lines (spawn-subscriber! d '("changes" "0" "--" "--help")) 1 5000))))
                    (and (pair? a) (list (car a) (and (pair? (cdr a)) (car (cadr a))))))
                  (let ((a (rpc-dispatch (d-store d) '(subscribe "changes" "0" "--" "--help") "tester")))
                    (and (pair? a) (list (car a) (cadr a) (clause 'reason a)))))
            '((usage subscribe) (error bad-request (needs-daemon))))
      (insert! d "one")
      (want "ACCEPT-4 a resume (rev > 0) without the token is refused missing-token"
            (let ((a (acceptance-of (await-lines (spawn-subscriber! d '("changes" "2")) 1 5000))))
              (and (pair? a) (list (car a) (cadr a) (caddr a))))
            '(error bad-request missing-token))
      (want "ACCEPT-5 a token that is not this daemon's is refused daemon-restarted, with current and the daemon's token"
            (let ((a (acceptance-of (await-lines (spawn-subscriber! d '("changes" "2" "0-0")) 1 5000))))
              (list (and (pair? a) (car a)) (and (pair? a) (cadr a)) (clause 'reason a) (clause 'current a)
                    (equal? (token-of a) token)))
            (list 'error 'changes-unavailable '(daemon-restarted) '(2) #t))
      (want "ACCEPT-6 on the local route, with no daemon, subscribe answers needs-daemon"
            (let ((a (rpc-dispatch (d-store d) '(subscribe "changes" "0") "tester")))
              (and (pair? a) (list (car a) (cadr a) (clause 'reason a))))
            '(error bad-request (needs-daemon)))
      ;; THE TOKEN (the TOKEN rows): a string pid-start, and a second daemon on the
      ;; same store, started after this one stops, answers another.
      (want "TOKEN-1 the token is a string of the form <pid>-<start>"
            (and (string? token)
                 (let ((dash (let find ((i 0)) (cond ((>= i (string-length token)) #f)
                                                     ((char=? (string-ref token i) #\-) i)
                                                     (else (find (+ i 1)))))))
                   (and dash
                        (equal? (string->number (substring token 0 dash)) (d-pid d))
                        (integer? (string->number (substring token (+ dash 1) (string-length token)))))))
            #t)
      (stop-daemon! d))

    ;; ==== ON DEMAND: the frame library is loaded by a subscription ====
    ;; A daemon that commits with nobody subscribed has not loaded (theourgia
    ;; stream-frames); the first accepted subscription loads it, once.
    (let* ((d (start-daemon! "lazy" "")))
      (insert! d "with nobody following")
      (let ((before (count-of (d-log d) "(trace library-loaded stream-frames")))
        (await-lines (spawn-subscriber! d '("changes" "0")) 1 5000)
        (await-lines (spawn-subscriber! d '("changes" "0")) 1 5000)
        (insert! d "with two following")
        (want "ON-DEMAND a daemon loads the frame library at the first accepted subscription, and only once"
              (list before (count-of (d-log d) "(trace library-loaded stream-frames"))
              '(0 1)))
      (stop-daemon! d))
    ;; A process that loads what core.sc loads and answers a verb has loaded
    ;; neither stream library; asking for each loads it (the second reading is
    ;; what shows the first could have said yes).
    (let* ((child (string-append scratch-base "/cs-" pid-text "-ondemand.sc"))
           (store (string-append scratch-base "/cs-" pid-text "-ondemand-store"))
           (out (string-append child ".out")))
      (system (string-append "rm -rf " store "; mkdir -p " store))
      (call-with-output-file child
        (lambda (p)
          (for-each (lambda (l) (display l p) (newline p))
            (list "(import (chezscheme) (theourgia rpc) (theourgia arguments) (theourgia render) (theourgia client))"
                  "(define (loaded? n) (and (member (list 'theourgia n) (library-list)) #t))"
                  (string-append "(rpc-dispatch \"" store "\" '(init) \"tester\")")
                  (string-append "(rpc-dispatch \"" store "\" '(no-such-verb) \"tester\")")
                  "(define before (list (loaded? 'stream-frames) (loaded? 'stream-client)))"
                  "(environment '(theourgia stream-frames))"
                  "(environment '(theourgia stream-client))"
                  "(write (list before (list (loaded? 'stream-frames) (loaded? 'stream-client))))")))
        'truncate)
      (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                             " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                             " scheme --script " child " > " out " 2>&1"))
      (want "ON-DEMAND a start that answers a verb has loaded neither stream library; asking for each loads it"
            (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out))))
            '((#f #f) (#t #t))))

    ;; ==== FRAME: what a frame says, operation by operation ====
    ;;
    ;; One subscriber, one operation at a time, and the frame each makes.
    ;; THE EXACT ITEM SET IS ASSERTED, not a member of it: a frame that also
    ;; named an unchanged block would be as wrong as one that missed this one.
    (printf "~%== FRAME: one frame per publication, its exact items ==~%")
    (let* ((d (start-daemon! "f2" ""))
           (sub (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines sub 1 5000)))
           (token (token-of acc))
           (seen (current-of acc))
           ;; the frame of the publication the action answered A made (frame-settled)
           (frame! (lambda (a) (let ((f (frame-settled d sub seen (mark-of a)))) (when (frame-rev f) (set! seen (frame-rev f))) f))))
      (let* ((a (ask d 'insert "--title" "A"))
             (id (new-id a))
             (f (frame! a)))
        (want "FRAME-01 insert: the request is answered, and exactly one frame arrives, rev 2, (added id) and none of the new block's fields"
              (list (car a) (frame-rev f) (equal? (clause 'daemon f) (list token)) (item-set f))
              (list 'ok 2 #t (expected-set (list 'added id))))
        (let ((read-cut (clause 'cut (ask d 'read id))))
          (want "FRAME-02 and the frame's cut is the publication's: the cut a read of it answers"
                (list (and read-cut #t) (equal? (clause 'cut f) read-cut) (equal? (clause 'from-cut f) (clause 'cut f)))
                '(#t #t #f)))
        (let ((g (frame! (ask d 'set id "src" "first"))))
          (want "FRAME-03 set src -> (changed id src), and its from-cut is the previous frame's cut"
                (list (item-set g) (equal? (clause 'from-cut g) (clause 'cut f)))
                (list (expected-set (list 'changed id 'src)) #t)))
        (want "FRAME-04 set src to the SAME value -> (changed id src): the candidate set differs by its event"
              (item-set (frame! (ask d 'set id "src" "first"))) (expected-set (list 'changed id 'src)))
        (want "FRAME-05 set front -> (changed id front)"
              (item-set (frame! (ask d 'set id "front" "f: 1"))) (expected-set (list 'changed id 'front)))
        (want "FRAME-06 set kind -> (changed id kind)"
              (item-set (frame! (ask d 'set id "kind" "doc"))) (expected-set (list 'changed id 'kind)))
        (let* ((ab (ask d 'insert "--title" "B")) (b (new-id ab)))
          (frame! ab)
          (want "FRAME-07 del -> (removed id), and nothing else"
                (item-set (frame! (ask d 'del b))) (expected-set (list 'removed b))))
        (want "FRAME-08 revisions are dense: the frames so far are 2 .. the last read, in order, an empty one included"
              (map frame-rev (frames-of (sub-lines sub)))
              (let loop ((k seen) (out '())) (if (< k 2) out (loop (- k 1) (cons k out))))))
      (stop-daemon! d))

    ;; A REFOLD OF THE SAME LOG IS A REVISION WITH NO ITEMS. An outside commit
    ;; and a read: the reload publishes it. Then an outside change that
    ;; carries no record -- a touch of the writers directory, whose times
    ;; are in the snapshot -- and a read: the store process reloads, folds
    ;; the same log, and its frame is sent, empty. The block deleted earlier
    ;; is named in neither. The touch waits for the first frame, since a
    ;; read during a fold asks for nothing.
    (let* ((d (start-daemon! "f2r" ""))
           (gone (insert! d "gone"))
           (_ (ask d 'del gone))
           (sub (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines sub 1 5000))
           (outside (mirror! d "mirrorrf" 1 '() '(put ((kind . section) (title . "outside") (parent . root) (ord . 9))))))
      (poke! d)
      (await-lines sub 2 8000)
      (system (string-append "touch " (d-store d) "/writers"))
      (poke! d)
      (let* ((ls (await-lines sub 3 8000)) (fs (frames-of ls)))
        (want "FRAME-09 a reload publishes the outside record, and a reload of the same log publishes an EMPTY frame"
              (list (>= (length fs) 2) (and (pair? fs) (item-set (car fs)))
                    (and (> (length fs) 1) (for-all (lambda (f) (equal? (item-set f) '())) (cdr fs))))
              (list #t (expected-set (list 'added "mirrorrf.1")) #t))
        (want "FRAME-10 and the deleted block is named in neither"
              (contains? (apply string-append ls) gone)
              #f))
      (stop-daemon! d))

    ;; ---- FRAME, records of other writers: positions, conflicts, edges, structure ----
    ;;
    ;; Each record here is published as another writer's segment, and a read
    ;; on another connection makes the store process reload it. DEPS name
    ;; what a record has seen: the cut of the last frame covers everything
    ;; published so far (a supersession); '() covers nothing (concurrent with
    ;; every candidate already there).
    ;; SETTLED means exactly one position candidate: a settled parent and a
    ;; settled ord change when the candidate set goes to or from one.
    (let* ((d (start-daemon! "f2m" ""))
           (sub (spawn-subscriber! d '("changes" "0")))
           (seen (current-of (acceptance-of (await-lines sub 1 5000))))
           (last-cut '()) (seqs '())
           ;; the frame of the publication record MARK made (frame-settled)
           (frame! (lambda (mark)
                     (let ((f (frame-settled d sub seen mark)))
                       (when (frame-rev f) (set! seen (frame-rev f)))
                       (let ((c (frame-cut f))) (when c (set! last-cut c)))
                       f)))
           ;; the next segment number of writer W
           (seq! (lambda (w) (let* ((e (assoc w seqs)) (k (if e (+ (cdr e) 1) 1)))
                               (set! seqs (cons (cons w k) (remp (lambda (x) (equal? (car x) w)) seqs)))
                               k)))
           ;; publishes W's record covering DEPS, reloads, and returns the frame
           (record! (lambda (w deps payload)
                      (let ((k (seq! w)))
                        (mirror! d w k deps payload)
                        (poke! d)
                        (cons (string-append w "." (number->string k)) (frame! (cons w k))))))
           (put (lambda (title parent ord . kind)
                  (list 'put (append (list (cons 'kind (if (pair? kind) (car kind) 'section)) (cons 'title title))
                                     (if parent (list (cons 'parent parent) (cons 'ord ord)) '()))))))
      ;; THE POSITION TRANSITION TABLE: every combination of the two parts.
      (let* ((pa (car (record! "mirrorpa" last-cut (put "parent a" 'root 1))))
             (pb (car (record! "mirrorpb" last-cut (put "parent b" 'root 2))))
             (x (car (record! "mirrorpx" last-cut (put "x" 'root 0)))))
        (want "FRAME-11 a re-move to the same place (a new event) -> (changed id position) alone"
              (item-set (cdr (record! "mirrorpx" last-cut (list 'move x 'root 0))))
              (expected-set (list 'changed x 'position)))
        (want "FRAME-12 same parent, another ord -> position and ord, not parent"
              (item-set (cdr (record! "mirrorpx" last-cut (list 'move x 'root 1))))
              (expected-set (list 'changed x 'position) (list 'changed x 'ord)))
        (want "FRAME-13 another parent, the same ord -> position and parent, not ord"
              (item-set (cdr (record! "mirrorpx" last-cut (list 'move x pa 1))))
              (expected-set (list 'changed x 'position) (list 'changed x 'parent)))
        (want "FRAME-14 another parent and another ord -> all three"
              (item-set (cdr (record! "mirrorpx" last-cut (list 'move x pb 5))))
              (expected-set (list 'changed x 'position) (list 'changed x 'parent) (list 'changed x 'ord)))
        ;; two concurrent candidates: one writer's move that covers nothing
        (want "FRAME-15 a concurrent second position -> the position changes and is in conflict, and the block is unplaced"
              (item-set (cdr (record! "mirrorpy" '() (list 'move x pa 7))))
              (expected-set (list 'changed x 'position) (list 'changed x 'parent) (list 'changed x 'ord)
                            (list 'conflict x 'position) (list 'conflict x 'unplaced)))
        (want "FRAME-16 one writer replacing its own candidate keeps two (2 -> 2): changed and in conflict again"
              (item-set (cdr (record! "mirrorpy" (list (cons "mirrorpy" 1)) (list 'move x pa 8))))
              (expected-set (list 'changed x 'position) (list 'conflict x 'position)))
        (want "FRAME-17 a move covering both settles it: changed, and both conflicts resolved"
              (item-set (cdr (record! "mirrorpz" last-cut (list 'move x 'root 0))))
              (expected-set (list 'changed x 'position) (list 'changed x 'parent) (list 'changed x 'ord)
                            (list 'resolved x 'position) (list 'resolved x 'unplaced))))
      ;; FIELDS: a concurrent value, a third one, a supersession; and two at once.
      (let* ((y (car (record! "mirrorfa" last-cut (append (put "y" 'root 3) '()))))
             (_ (record! "mirrorfa" last-cut (list 'set y 'src "one"))))
        (want "FRAME-18 a genuinely concurrent record on the same field -> changed and in conflict"
              (item-set (cdr (record! "mirrorfb" (list (cons "mirrorfa" 1)) (list 'set y 'src "two"))))
              (expected-set (list 'changed y 'src) (list 'conflict y 'src)))
        (want "FRAME-19 a third concurrent value: the set changes and stays in conflict -> changed AND conflict"
              (item-set (cdr (record! "mirrorfc" (list (cons "mirrorfa" 1)) (list 'set y 'src "three"))))
              (expected-set (list 'changed y 'src) (list 'conflict y 'src)))
        (want "FRAME-20 a later supersession -> changed and resolved"
              (item-set (cdr (record! "mirrorfa" last-cut (list 'set y 'src "settled"))))
              (expected-set (list 'changed y 'src) (list 'resolved y 'src)))
        ;; NO FRONT, THEN TWO CONCURRENT FRONTS IN ONE FOLD: 0 -> 2.
        ;;
        ;; NEVER: ONE FOLD BY CAUSALITY, NOT BY TIMING. The two fronts are two
        ;; writers' records, two publications, and while a subscriber is
        ;; registered the daemon probes once a second: a probe between them
        ;; folded the first alone, and the row read 1 -> 2 under its 0 -> 2
        ;; name. So both name a third record among what they have seen, and
        ;; that record is published last: until it arrives neither can be
        ;; applied (pending), whatever folds; its one publication releases
        ;; both in the fold that applies it. Neither has seen the other, so
        ;; they stay concurrent.
        (let* ((k1 (seq! "mirrorga")) (k2 (seq! "mirrorgb")) (kr (seq! "mirrorgr"))
               (release (cons "mirrorgr" kr)) (before last-cut)
               (from-cut-of (lambda (f) (let ((c (clause 'from-cut f))) (and c (car c)))))
               (in? (lambda (cut mark) (and (list? cut) (let ((e (assoc (car mark) cut))) (and e (>= (cdr e) (cdr mark)) #t))))))
          (mirror! d "mirrorga" k1 (cons release before) (list 'set y 'front "a: 1"))
          (mirror! d "mirrorgb" k2 (cons release before) (list 'set y 'front "b: 2"))
          (mirror! d "mirrorgr" kr before (list 'set y 'title "released"))
          (poke! d)
          (let ((f (frame! release)))
            (want "FRAME-21 a block with no front acquiring two concurrent fronts in one fold -> changed AND conflict (and the release's own title)"
                  (item-set f)
                  (expected-set (list 'changed y 'front) (list 'conflict y 'front) (list 'changed y 'title)))
            ;; THE CONSTRUCTION, READ: the frame that applied the release
            ;; starts before either front and ends after both.
            (want "FRAME-22 CONSTRUCTION: both fronts arrived in one fold -- the frame's from-cut holds neither, its cut holds both"
                  (list (in? (from-cut-of f) (cons "mirrorga" k1)) (in? (from-cut-of f) (cons "mirrorgb" k2))
                        (in? (frame-cut f) (cons "mirrorga" k1)) (in? (frame-cut f) (cons "mirrorgb" k2)))
                  '(#f #f #t #t))
            ;; CONTROL: THE OLD SHAPE, the first record's own frame awaited
            ;; before the second is written -- two folds, which the reading
            ;; above tells apart: the second's frame starts after the first.
            (let* ((ka (seq! "mirrorha")) (kb (seq! "mirrorhb"))
                   (fa (begin (mirror! d "mirrorha" ka last-cut (list 'set y 'keywords "h: a")) (poke! d)
                              (frame! (cons "mirrorha" ka))))
                   (fb (begin (mirror! d "mirrorhb" kb last-cut (list 'set y 'keywords "h: b")) (poke! d)
                              (frame! (cons "mirrorhb" kb)))))
              (want "FRAME-23 CONTROL: written with a fold between, the two arrive in two folds -- the first's frame lacks the second, the second's starts after the first"
                    (list (in? (frame-cut fa) (cons "mirrorha" ka)) (in? (frame-cut fa) (cons "mirrorhb" kb))
                          (in? (from-cut-of fb) (cons "mirrorha" ka)) (in? (from-cut-of fb) (cons "mirrorhb" kb)))
                    '(#t #f #t #f))))))
      ;; EDGES: a link, a second link with the same endpoints from another
      ;; event, and an unlink that has seen only the first.
      (let* ((p (car (record! "mirrorea" last-cut (put "p" 'root 4))))
             (q (car (record! "mirrorea" last-cut (put "q" 'root 5))))
             (e1 (record! "mirrorea" last-cut (list 'link p 'relates q)))
             (cut-after-e1 last-cut)
             (e2 (record! "mirroreb" '() (list 'link p 'relates q))))
        (want "FRAME-24 link -> (edge-added from rel to (event w . s))"
              (item-set (cdr e1))
              (expected-set (list 'edge-added p 'relates q (cons 'event (cons "mirrorea" 3)))))
        (want "FRAME-25 a second link, same endpoints and relation, another event -> edge-added again, for that event"
              (item-set (cdr e2))
              (expected-set (list 'edge-added p 'relates q (cons 'event (cons "mirroreb" 1)))))
        (want "FRAME-26 an unlink that has seen only the first -> one edge-removed, and the second edge stays"
              (item-set (cdr (record! "mirrorec" cut-after-e1 (list 'unlink p 'relates q))))
              (expected-set (list 'edge-removed p 'relates q))))
      ;; STRUCTURE: a cycle and its undoing; an orphan two ways and its
      ;; rescue; a block with no position at all; a document under a document.
      (let* ((a (car (record! "mirrorsa" last-cut (put "a" 'root 3))))
             (b (car (record! "mirrorsa" last-cut (put "b" 'root 4)))))
        (want "FRAME-27 a under b: position and parent change, no cycle yet"
              (item-set (cdr (record! "mirrorsa" last-cut (list 'move a b 3))))
              (expected-set (list 'changed a 'position) (list 'changed a 'parent)))
        (want "FRAME-28 b under a completes a cycle -> both members in conflict, cycle"
              (item-set (cdr (record! "mirrorsa" last-cut (list 'move b a 4))))
              (expected-set (list 'changed b 'position) (list 'changed b 'parent)
                            (list 'conflict a 'cycle) (list 'conflict b 'cycle)))
        (let* ((r (record! "mirrorsf" last-cut (put "feeder" a 0))) (feeder (car r)))
          (want "FRAME-29 a block put under a cycle member is added and is not on the cycle"
                (item-set (cdr r))
                (expected-set (list 'added feeder)))
          ;; THE TWO COMPUTATIONS WHILE THE CYCLE AND ITS FEEDER EXIST, not
          ;; only on the store left at the end (the TWIN rows).
          (want "TWIN-1 with a cycle and a block leading into it, the comparator's sets equal state-structure's"
                (let ((r (open-and-reduce (d-store d)))) (equal? (structural-sets r) (state-structure r)))
                #t))
        (want "FRAME-30 undoing it -> resolved for both"
              (item-set (cdr (record! "mirrorsa" last-cut (list 'move b 'root 4))))
              (expected-set (list 'changed b 'position) (list 'changed b 'parent)
                            (list 'resolved a 'cycle) (list 'resolved b 'cycle))))
      ;; A FEEDER OLDER THAN THE CYCLE: blocks are walked oldest first, so the
      ;; walk that finds this cycle starts at the feeder, and only the two
      ;; members may be marked.
      (let* ((old (car (record! "mirrorsg" last-cut (put "older feeder" 'root 13))))
             (c (car (record! "mirrorsg" last-cut (put "c" 'root 14))))
             (e (car (record! "mirrorsg" last-cut (put "e" 'root 15)))))
        (record! "mirrorsg" last-cut (list 'move old c 0))
        (record! "mirrorsg" last-cut (list 'move c e 0))
        (want "FRAME-31 a cycle found from an older feeder: only its two members are in conflict"
              (item-set (cdr (record! "mirrorsg" last-cut (list 'move e c 0))))
              (expected-set (list 'changed e 'position) (list 'changed e 'parent) (list 'changed e 'ord)
                            (list 'conflict c 'cycle) (list 'conflict e 'cycle)))
        (want "TWIN-2 with that cycle and its older feeder, the comparator's sets equal state-structure's, and the feeder is not a member"
              (let* ((r (open-and-reduce (d-store d))) (sets (structural-sets r)))
                (list (equal? sets (state-structure r)) (and (member old (cdr (assq 'conflicts sets))) #t)))
              '(#t #f))
        (record! "mirrorsg" last-cut (list 'move e 'root 15)))
      (let* ((parent (car (record! "mirrorob" last-cut (put "parent" 'root 6))))
             (child (car (record! "mirrorob" last-cut (put "child" parent 0)))))
        (want "FRAME-32 deleting a parent whose child survives -> removed, and the child an orphan"
              (item-set (cdr (record! "mirrorob" last-cut (list 'del parent))))
              (expected-set (list 'removed parent) (list 'conflict child 'orphan)))
        (want "FRAME-33 the orphaned child moved to root -> position and parent change, the orphan resolved"
              (item-set (cdr (record! "mirrorob" last-cut (list 'move child 'root 0))))
              (expected-set (list 'changed child 'position) (list 'changed child 'parent) (list 'resolved child 'orphan))))
      (let ((w (car (record! "mirrorow" last-cut (put "wanderer" 'root 7)))))
        (want "FRAME-34 a superseding move to a parent the reduction does not hold -> an orphan"
              (item-set (cdr (record! "mirrorow" last-cut (list 'move w "ghost.1" 7))))
              (expected-set (list 'changed w 'position) (list 'changed w 'parent) (list 'conflict w 'orphan))))
      (want "FRAME-35 a block made by a set with no position candidate -> added, and unplaced"
            (item-set (cdr (record! "mirrorun" last-cut (list 'set "missing.1" 'src "x"))))
            (expected-set (list 'added "missing.1") (list 'conflict "missing.1" 'unplaced)))
      (want "FRAME-36 its first move -> position, parent and ord change, unplaced resolved"
            (item-set (cdr (record! "mirrorun" last-cut (list 'move "missing.1" 'root 0))))
            (expected-set (list 'changed "missing.1" 'position) (list 'changed "missing.1" 'parent)
                          (list 'changed "missing.1" 'ord) (list 'resolved "missing.1" 'unplaced)))
      (let* ((d1 (car (record! "mirrornd" last-cut (put "doc one" 'root 8 'doc))))
             (d2 (car (record! "mirrornd" last-cut (put "doc two" 'root 9 'doc)))))
        (want "FRAME-37 a document moved under a document -> nested"
              (item-set (cdr (record! "mirrornd" last-cut (list 'move d2 d1 9))))
              (expected-set (list 'changed d2 'position) (list 'changed d2 'parent) (list 'conflict d2 'nested)))
        (want "FRAME-38 undone -> resolved"
              (item-set (cdr (record! "mirrornd" last-cut (list 'move d2 'root 9))))
              (expected-set (list 'changed d2 'position) (list 'changed d2 'parent) (list 'resolved d2 'nested))))
      (stop-daemon! d))

    ;; ==== FRAME behind a stale empty frame: the frame is the action's own ====
    ;;
    ;; THE RACE, CONSTRUCTED BY SIZE. A reload refolds the whole log, so on a
    ;; big store every reload is slow, a redundant one too. Action 1 is one
    ;; record and a read. Then an outside change with no record (a touch of
    ;; the writers directory) and a read: a reload of the same log, which
    ;; publishes an EMPTY frame after action 1's. Action 2 is one record and
    ;; one read, made once that empty frame is out (a read during a fold
    ;; asks for nothing), so the stale empty frame lies past action 1's
    ;; revision and before action 2's own, which comes one slow reload
    ;; later. A wait that takes "the last
    ;; revision, once it has held still for 300 ms" takes the empty frame for
    ;; action 2's whenever a reload takes longer than that -- which is what
    ;; load did in the suite, and what size does here on purpose.
    ;; NOTE: THE STORE GROWS UNTIL ONE RELOAD TAKES OVER 600 MS (twice that
    ;; 300 ms), doubling the bulk up to five times. The reload's time is
    ;; PRINTED, not asserted: how long a reload takes is the machine's, not
    ;; this row's. That the stale empty frame was made, and lies between the
    ;; two actions' frames, IS asserted (FRAME-39).
    ;; NOTE: THE OLD WAIT'S RED IS NOT A ROW HERE. It is read by a fixture
    ;; mutant that puts the old rule back in frame-settled and runs this same
    ;; construction: action 2's row then reads (), the gates' red. A copy of
    ;; the old rule kept in this file to witness it was a second instrument
    ;; for what the mutant reads directly, and drew findings round after round.
    ;; NOTE: THE READS HERE ARE OF ONE BLOCK, not poke!'s outline: an outline
    ;; of thousands of blocks takes seconds to answer, so a second outline
    ;; came only after the first reload had published, and queued nothing
    ;; (measured: the second read was routed two seconds after the first).
    ;; NOTE: AND ACTION 2'S FOLD IS HELD (reload-before-publish) for a second
    ;; before it publishes, so the stale empty frame stays the last revision
    ;; for that long whatever the machine: the size alone made the old wait's
    ;; red depend on one reload outlasting the wait's own polls (it survived
    ;; one run in three). The release exists from the start, so every other
    ;; fold passes the hold; it is removed just before action 2. The waits are
    ;; on the stream's lines: a read during the held fold asks for nothing.
    (printf "~%== FRAME: a slow reload behind a stale empty frame ==~%")
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-slow"))
           (_ (begin (system (string-append "rm -f " release " " release ".held"))
                     (system (string-append "touch " release))))
           (d (start-daemon! "f2s" (string-append "THEOURGIA_HOLD='reload-before-publish:" release "'")))
           (nudge! (lambda () (ask d 'read "mirrorb1.1")))
           (timed-reload
             (lambda ()
               (let ((p0 (last-published d)) (t0 (real-time)))
                 (nudge!)
                 (let wait ((k 0))
                   (let ((p (last-published d)))
                     (cond ((and p (or (not p0) (> p p0))) (- (real-time) t0))
                           ((> (- (real-time) t0) (scaled 60000 (load-factor))) (list 'never-published p0))
                           (else (sleep-ms 20) (wait (+ k 1)))))))))
           (reload-ms
             (let grow ((n 4000) (k 1))
               (mirror-many! d (string-append "mirrorb" (number->string k)) n)
               (let ((t (timed-reload)))
                 (if (or (not (number? t)) (> t 600) (>= k 5)) t (grow (* n 2) (+ k 1))))))
           (sub (spawn-subscriber! d '("changes" "0")))
           (seen (current-of (acceptance-of (await-lines sub 1 (scaled 5000 (load-factor))))))
           (f1 (begin (mirror! d "mirrorsa" 1 '() '(put ((kind . section) (title . "first") (parent . root) (ord . 1))))
                      (nudge!)
                      (frame-settled d sub seen (cons "mirrorsa" 1))))
           (r1 (frame-rev f1))
           ;; THE STALE EMPTY FRAME IS OBSERVED, NOT ASSUMED: a frame past r1
           ;; with no items, on the stream's lines, before action 2 starts.
           ;; Without it the rows below would read action 2 against nothing
           ;; and pass; so its absence is bound and FRAME-39 reads it.
           (empty-seen
             (begin (system (string-append "touch " (d-store d) "/writers"))
                    (nudge!)
                    (let ((bound (scaled 60000 (load-factor))) (t0 (real-time)))
                      (let wait ()
                        (cond ((and r1 (exists (lambda (f) (let ((r (frame-rev f)))
                                                             (and r (> r r1) (equal? (clause 'items f) '()))))
                                               (frames-of (sub-lines sub))))
                               #t)
                              ((> (- (real-time) t0) bound)
                               (printf "PREREQUISITE NOT MET: no empty frame past revision ~s within ~s ms after the touch and its read; the stale frame FRAME-39 to FRAME-41 need was not constructed, so their reading is not one of frame-settled~%" r1 bound)
                               #f)
                              (else (sleep-ms 20) (wait)))))))
           (f2 (begin (system (string-append "rm -f " release " " release ".held"))
                      (mirror! d "mirrorsb" 1 '() '(put ((kind . section) (title . "second") (parent . root) (ord . 2))))
                      (nudge!)
                      (spawn (lambda ()
                               (let wait ((k 0))
                                 (unless (or (file-exists? (string-append release ".held")) (> k 400))
                                   (sleep-ms 50) (wait (+ k 1))))
                               (sleep-ms 1000)
                               (system (string-append "touch " release))))
                      (frame-settled d sub (or r1 seen) (cons "mirrorsb" 1))))
           (r2 (frame-rev f2))
           (between (filter (lambda (f) (let ((r (frame-rev f))) (and r r1 r2 (> r r1) (< r r2))))
                            (frames-of (sub-lines sub)))))
      (printf "the slow reload: a reload took ~s ms; frames between the two actions' frames: ~s, of them empty: ~s~%"
              reload-ms (length between) (length (filter (lambda (f) (equal? (clause 'items f) '())) between)))
      (want "FRAME-39 SLOW: the stale empty frame was seen before action 2, and between the two actions' frames there is at least one frame, every one empty"
            (list empty-seen (pair? between) (for-all (lambda (f) (equal? (clause 'items f) '())) between))
            '(#t #t #t))
      (want "FRAME-40 action 1's frame is its own: (added mirrorsa.1)"
            (item-set f1) (expected-set (list 'added "mirrorsa.1")))
      (want "FRAME-41 action 2's frame is its own, not the stale empty one before it: (added mirrorsb.1)"
            (item-set f2) (expected-set (list 'added "mirrorsb.1")))
      (unless (and (number? reload-ms) (> reload-ms 600))
        (printf "NOTE: the reload took ~s ms, not over 600 ms; the hold on action 2's fold constructs the race whatever the reload took~%" reload-ms))
      (stop-daemon! d))

    ;; A RETRACTION: two writers' records claim one single identity, the
    ;; second reverses the first, and the first's block is taken back with
    ;; no new event applied. The frame names what changed back.
    (let* ((d (start-daemon! "f2x" ""))
           (sub (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines sub 1 5000))
           (single (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0))))
;; EACH FRAME IS LOOKED FOR PAST THE REVISION PUBLISHED BEFORE ITS OWN
      ;; WRITE, never past an earlier frame that may not have been found.
      (let ((r1 (or (last-published d) 0)))
       (mirror-as! d "aaa00000" 1 '() single '(put ((kind . section) (title . "claimed") (parent . root) (ord . 10))))
       (poke! d)
      (let ((f1 (said-frame-after sub r1)) (r2 #f))
        (set! r2 (or (last-published d) 0))
        (mirror-as! d "bbb00000" 1 '() single '(put ((kind . section) (title . "rival") (parent . root) (ord . 11))))
        (poke! d)
        (let ((f2 (said-frame-after sub r2)))
          (want "FRAME-42 a single identity's record is applied -> (added its block)"
                (item-set f1) (expected-set (list 'added "aaa00000.1")))
          (want "FRAME-43 a rival claim reverses it: the block is taken back with no new event -> (removed it) alone"
                (item-set f2) (expected-set (list 'removed "aaa00000.1"))))))
      ;; AN OLDER CANDIDATE COMES BACK: a block's src is set by a claimed
      ;; record, and a rival claim takes that record back; the block's
      ;; earlier value is its value again, with no new event.
      (let* ((held-rev #f)
             (single2 (list "test" (cons "other111" "t") 'single "fp" #f (cons "other111" 0)))
             ;; every publication so far has been read above
             (before-plain (or (last-published d) 0)))
        (mirror! d "plainzzz" 1 '() '(put ((kind . section) (title . "keeps") (parent . root) (ord . 12) (src . "original"))))
        (poke! d)
        (said-frame-after sub before-plain)
        (set! held-rev (let ((r (rev-clause (ask d 'read "plainzzz.1" "--rev")))) (and r (car r))))
        (mirror-as! d "ccc00000" 1 (list (cons "plainzzz" 1)) single2 '(set "plainzzz.1" src "claimed"))
        (poke! d)
        (let* ((f3 (said-frame-after sub (or held-rev 0)))
               (r4 (or (last-published d) 0)))
          (mirror-as! d "ddd00000" 1 (list (cons "plainzzz" 1)) single2 '(set "plainzzz.1" src "rival"))
          (poke! d)
          (let ((f4 (said-frame-after sub r4)))
            (want "FRAME-44 a claimed record sets the field -> (changed id src)"
                  (item-set f3) (expected-set (list 'changed "plainzzz.1" 'src)))
            (want "FRAME-45 the rival claim takes it back: the old value returns with no new event -> (changed id src)"
                  (item-set f4) (expected-set (list 'changed "plainzzz.1" 'src)))
            ;; THE APPLICATION RULE IS BY REVISION (the APPLY row): the cut moves
            ;; backwards here, and a consumer holding the read's rev still
            ;; applies both frames, in order.
            (let* ((d3 (consume (list (cons "plainzzz.1" held-rev)) "plainzzz.1" (frame-rev f3)))
                   (d4 (consume (cdr d3) "plainzzz.1" (frame-rev f4))))
              (want "APPLY-1 the retraction moves the applied cut backwards, and a consumer holding its read's rev applies both frames in order"
                    (list (and (assoc "ccc00000" (or (frame-cut f3) '())) #t) (and (assoc "ccc00000" (or (frame-cut f4) '())) #t)
                          (car d3) (car d4))
                    '(#t #f apply apply))))))
      (stop-daemon! d))

    ;; ==== PUBLISHER: every publisher frames ====
    (printf "~%== PUBLISHER: a local commit, a reload, a refresh before a read at a cut ==~%")
    (let* ((d (start-daemon! "f11" ""))
           (id (insert! d "the block"))
           (sub (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines sub 1 5000)))
           (base (current-of acc)))
      (insert! d "local")
      (let ((f (said-frame-after sub base)))
        (want "PUBLISHER-1 a local commit makes exactly one frame, the next revision"
              (frame-rev f) (+ base 1)))
      (mirror! d "mirrorpp" 1 '() '(put ((kind . section) (title . "outside") (parent . root) (ord . 20))))
      (poke! d)
      (let ((f (said-frame-after sub (+ base 1))))
        (want "PUBLISHER-2 a reload asked by another connection's read after an outside commit: one frame, the next revision"
              (list (frame-rev f) (item-set f)) (list (+ base 2) (expected-set (list 'added "mirrorpp.1")))))
      (mirror! d "mirrorpq" 1 '() '(put ((kind . section) (title . "outside two") (parent . root) (ord . 21))))
      (let ((at (ask d 'read id "--cut" (format "~s" (list (cons "mirrorpq" 1))))))
        (let ((f (said-frame-after sub (+ base 2))))
          (want "PUBLISHER-3 a read at a cut that the publication has not folded: the store process refreshes, one frame"
                (list (frame-rev f) (item-set f)) (list (+ base 3) (expected-set (list 'added "mirrorpq.1"))))))
      (sleep-ms 400)
;; THE CLAIM, EXACTLY: one frame with items per change -- the local commit,
      ;; the outside commit, the read at a cut -- and every other frame empty
      ;; (a queued reload's refold of the same log carries no change).
      (want "PUBLISHER-4 and no publisher made a second frame for the same publication: three frames with items, every other frame empty"
            (let ((fs (frames-of (sub-lines sub))))
              (list (length (filter (lambda (f) (pair? (clause 'items f))) fs))
                    (for-all (lambda (f) (or (pair? (clause 'items f)) (equal? (clause 'items f) '()))) fs)))
            '(3 #t))
      (want "REV-11 read --rev --cut is refused incompatible-cut-options"
            (ask d 'read id "--rev" "--cut" (format "~s" (list (cons "mirrorpq" 1))))
            '(error bad-request incompatible-cut-options))
      (let ((a (ask d 'read id "--rev")))
        (want "REV-12 read --rev carries the receipts, then the rev clause last"
              (let ((names (clause-names a)))
                (list (car a) (and (memq 'cut names) #t) (and (memq 'versions names) #t)
                      (and (pair? names) (car (reverse names)))))
              '(ok #t #t rev)))
      (stop-daemon! d))

    ;; ==== REV: the rev travels with the state ====
    (printf "~%== REV: read --rev names the publication its answer was built from ==~%")
    (let* ((d (start-daemon! "f13" ""))
           (id (insert! d "rev block"))
           (kid (new-id (ask d 'insert "--under" id "--title" "kid")))
           (writer "w-rev"))
      (ask d 'write id "draft text" "--writer" writer)
      ;; THE PUBLICATION A READ WAS BUILT FROM is the one current when it was
      ;; asked: a read that sees an outside change asks for a reload and
      ;; answers from the cell as it is (the draft written above changes the
      ;; snapshot). So the daemon is first let settle -- no publication for
      ;; half a second, after a read that probes -- and then each read is
      ;; compared with the last publication traced before it.
      (let* ((rev-of (lambda (a) (let ((r (rev-clause a))) (and r (car r)))))
             (settle! (lambda ()
                        (ask d 'read id)
                        (let wait ((last (last-published d)) (k 0))
                          (sleep-ms 500)
                          (let ((now (last-published d)))
                            (unless (or (equal? now last) (> k 20)) (wait now (+ k 1)))))))
             (answered-at (lambda args
                            (settle!)
                            (let* ((p (last-published d)) (a (apply ask d 'read args)))
                              (list (rev-of a) p)))))
        (want "REV-01 read, read --md, read --recursive and a writer's read --working each carry the revision current when it was asked"
              (map (lambda (x) (and (car x) (equal? (car x) (cadr x))))
                   (list (answered-at id "--rev")
                         (answered-at id "--md" "--rev")
                         (answered-at id "--recursive" "--rev")
                         (answered-at id "--working" "--rev" "--writer" writer)))
              '(#t #t #t #t))
        (want "REV-02 and the clause names the daemon by the token its subscriptions answer: (rev n (daemon token))"
              (let ((r (rev-clause (ask d 'read id "--rev")))
                    (token (token-of (acceptance-of (await-lines (spawn-subscriber! d '("changes" "0")) 1 5000)))))
                (list (and r (pair? (cdr r)) (pair? (cadr r)) (car (cadr r)))
                      (and r (pair? (cdr r)) (pair? (cadr r)) (string? token) (equal? (cdr (cadr r)) (list token)))))
              '(daemon #t))
        (let ((n (last-published d)))
          (mirror! d "mirrorrv" 1 '() '(put ((kind . section) (title . "outside") (parent . root) (ord . 30))))
          (poke! d)
          (let wait ((k 0)) (unless (or (> (or (last-published d) 0) n) (> k 100)) (sleep-ms 50) (wait (+ k 1))))
          (want "REV-03 after an outside commit and its reload, the next read carries a later number, the one current when it was asked"
                (let ((x (answered-at id "--rev"))) (list (and (car x) (> (car x) n)) (equal? (car x) (cadr x))))
                '(#t #t))))
      (want "REV-04 --rev with --working-info, and with --signature, is refused incompatible-rev-options"
            (list (ask d 'read id "--rev" "--working-info" "--writer" writer)
                  (ask d 'read id "--rev" "--signature"))
            '((error bad-request incompatible-rev-options) (error bad-request incompatible-rev-options)))
      (want "REV-05 a plain read on a daemon carries no rev clause"
            (rev-clause (ask d 'read id)) #f)
      (want "REV-06 a read with no id answers the usage form, which shows --rev"
            (contains? (format "~s" (ask d 'read)) "--rev") #t)
      ;; THE LIBRARY'S ENTRY, called by the row itself: a handler handed no
      ;; named state has no rev to give.
      (let ((r (open-and-reduce (d-store d))))
        (want "REV-07 handed a bare reduction, and a seal with no name, read --rev answers no-published-state"
              (list (rpc-dispatch (d-store d) (list 'read id "--rev") "tester" r)
                    (rpc-dispatch (d-store d) (list 'read id "--rev") "tester" (seal-state r '())))
              '((error bad-request (reason no-published-state)) (error bad-request (reason no-published-state))))
        (want "REV-08 and on the local route, with no daemon, read --rev answers no-published-state"
              (rpc-dispatch (d-store d) (list 'read id "--rev") "tester")
              '(error bad-request (reason no-published-state))))
      ;; THE HUMAN RENDERING: an answer that carries a rev is printed whole.
      (let ((with (ask d 'read id "--md" "--rev")) (without (ask d 'read id "--md")))
        (want "REV-09 in human mode a read --md --rev prints the whole answer, clause included"
              (equal? (render-human with) (render-wire with)) #t)
        (want "REV-10 and without --rev it prints the text alone, as before"
              (equal? (render-human without) (cadr (cadr without))) #t))
      (stop-daemon! d))

    ;; ==== INDEP: several subscribers, one connection's misbehaviour, a blocked write ====
    (printf "~%== INDEP: subscribers are independent; a pending write is finished, not lost ==~%")
    (let* ((d (start-daemon! "f3" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc))
           (base (current-of acc)))
      (insert! d "r")
      (await-lines a 2 5000)
      (let* ((b (spawn-subscriber! d (list "changes" (number->string base) token)))
             (bacc (acceptance-of (await-lines b 2 5000)))
             ;; an INITIAL subscription while frames are retained replays none of them
             (c (spawn-subscriber! d '("changes" "0")))
             (cacc (acceptance-of (await-lines c 1 5000))))
        (sleep-ms 400)
        (want "INDEP-01 an initial subscription at a later revision, with frames retained, replays nothing"
              (list (current-of cacc) (length (frames-of (sub-lines c)))) (list (+ base 1) 0))
        (insert! d "r+1")
        (await-lines a 3 5000) (await-lines b 3 5000)
        (want "INDEP-02 an initial subscriber and a resuming one each get their own acceptance, replay and live frames"
              (list (current-of bacc) (revs-of a) (revs-of b))
              (list (+ base 1) (list (+ base 1) (+ base 2)) (list (+ base 1) (+ base 2))))
        (send a '(close))
        (let wait ((k 0)) (unless (or (contains? (d-log d) "(trace stream-closed") (> k 100)) (sleep-ms 50) (wait (+ k 1))))
        (insert! d "after a left")
        (await-lines b 4 5000)
        (want "INDEP-03 one subscriber's connection closing ends its stream, and the other keeps receiving"
              (list (contains? (d-log d) "(trace stream-closed eof") (revs-of b)) (list #t (list (+ base 1) (+ base 2) (+ base 3))))
        (send b (list 'write (envelope (d-store d) 'read (list "x"))))
        (let ((ls (await-lines b 5 5000)))
          (want "INDEP-04 a subscribed connection that sends a request is answered bad-request subscribed, in the stream"
                (and (>= (length ls) 5) (guard (e (#t (list-ref ls 4))) (read (open-string-input-port (list-ref ls 4)))))
                '(error bad-request (reason subscribed))))
        (insert! d "still streaming")
        (want "INDEP-05 and it still receives the next frame"
              (car (reverse (revs-of (begin (await-lines b 6 5000) b)))) (+ base 4)))
      ;; BUFFERED LEFTOVERS: a request sent in the same write as the subscription
      (let* ((e (spawn-subscriber! d '("changes" "0") (envelope (d-store d) 'read (list "x"))))
             (ls (await-lines e 2 5000)))
        (want "INDEP-06 a request that arrived with the subscription is answered bad-request subscribed after the acceptance"
              (and (>= (length ls) 2) (guard (x (#t (cadr ls))) (read (open-string-input-port (cadr ls)))))
              '(error bad-request (reason subscribed))))
      (stop-daemon! d))

    ;; THE TRANSITION: a write the daemon could not finish, observed, then more
    ;; publications; when the reader resumes, everything arrives once, in order.
    (let* ((d (start-daemon! "f3t" "THEOURGIA_SEND_BUFFER=16384"))
           (s (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines s 1 5000)))
      (send s '(pause))
      (let ((made (fill-until-pending! d 40)))
        (want "INDEP-07 the paused reader's socket fills: the daemon traces (write-pending conn)"
              (integer? made) #t)
        (insert! d "after the pending write")
        (send s '(resume))
        (let* ((expect (+ (if (integer? made) made 0) 1))
               (ls (await-lines s (+ expect 1) 15000))
               (revs (map frame-rev (frames-of ls))))
          (want "INDEP-08 when the reader resumes the frames arrive complete, in order, none twice, none missing"
                (list (length revs) (equal? revs (let loop ((k expect) (out '())) (if (= k 0) out (loop (- k 1) (cons (+ k 1) out))))))
                (list expect #t))))
      (stop-daemon! d))

    ;; STALE COMPLETIONS: with a write pending, a notice with the wrong
    ;; reference (and, on a second daemon, one with an old token) neither
    ;; completes the write nor ends the stream. THE PENDING WRITE STAYS
    ;; PENDING: while the reader is still paused, nothing after the fault's
    ;; trace says a write completed or another began -- a stream that took
    ;; the notice would say one or the other at once. Resumed, every frame
    ;; arrives.
    (for-each
      (lambda (fault)
        (let* ((d (start-daemon! (string-append "f3" fault) (string-append "THEOURGIA_SEND_BUFFER=16384 THEOURGIA_FAULT=" fault "@conn")))
               (s (spawn-subscriber! d '("changes" "0")))
               (_ (await-lines s 1 5000)))
          (send s '(pause))
          (let* ((made (fill-until-pending! d 40))
                 (_ (sleep-ms 500))
                 (paused-log (d-log d))
                 (after-fault (let find ((ds (log-data paused-log)) (seen #f) (out '()))
                                (cond ((null? ds) (if seen (reverse out) 'no-fault-trace))
                                      ((and (not seen) (pair? (car ds)) (eq? (caar ds) 'trace) (equal? (cdar ds) (list 'fault (string->symbol fault) #f)))
                                       (find (cdr ds) #t out))
                                      ((and seen (pair? (car ds)) (eq? (caar ds) 'trace) (memq (cadar ds) '(stream-write stream-written)))
                                       (find (cdr ds) seen (cons (cadar ds) out)))
                                      (else (find (cdr ds) seen out))))))
            (want (string-append "INDEP-09 " fault ": with the reader paused, after the notice no write completes and none begins")
                  (list (integer? made) after-fault) (list #t '()))
            (send s '(resume))
            (let* ((expect (if (integer? made) made 0))
                   (_ (await-lines s (+ expect 1) 15000))
                   (_ (insert! d "one more"))
                   (ls (await-lines s (+ expect 2) 8000))
                   (revs (map frame-rev (frames-of ls))))
              (want (string-append "INDEP-10 " fault ": resumed, the stream carries every frame and the next, in order")
                    (list (length revs) (equal? revs (let loop ((k (+ expect 1)) (out '())) (if (= k 0) out (loop (- k 1) (cons (+ k 1) out))))))
                    (list (+ expect 1) #t))))
          (stop-daemon! d)))
      '("stream-written-ref" "stream-written-token"))

    ;; ==== ENDING: drain, restart, overflow, the actors' deaths ====
    (printf "~%== ENDING: the stream's last frame says why it ended ==~%")
    ;; A PUBLICATION IN FLIGHT AT THE DRAIN, with the stream busy: its reader
    ;; is paused and a write pending, so the frame and the drain's stream-end
    ;; both wait in the stream's mailbox, and the frame must come first.
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-drain"))
           (_ (system (string-append "rm -f " release " " release ".held")))
           (d (start-daemon! "f6" (string-append "THEOURGIA_SEND_BUFFER=16384 THEOURGIA_HOLD='reload-before-publish:" release "'")))
           (s (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines s 1 5000)))
      ;; the first reload passes the hold, and the hold is re-armed
      (mirror! d "mirrordp" 1 '() '(put ((kind . section) (title . "prime") (parent . root) (ord . 39))))
      (poke! d)
      (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
      (system (string-append "touch " release))
      (sleep-ms 500)
      ;; the fill's reloads pass while the release exists; the hold is armed
      ;; again only for the publication in flight
      (send s '(pause))
      (let ((made (fill-until-pending! d 40)))
        (system (string-append "rm -f " release " " release ".held"))
        (mirror! d "mirrordr" 1 '() '(put ((kind . section) (title . "in flight") (parent . root) (ord . 40))))
        (poke! d)
        (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
        (let ((pid (d-pid d))) (when pid (system (string-append "kill -TERM " (number->string pid)))))
        (sleep-ms 200)
        (system (string-append "touch " release))
        (sleep-ms 300)
        (send s '(resume))
        (let* ((ls (let ((counted (await-lines s (+ (if (integer? made) made 0) 4) 12000)))
                     ;; AND THE CLOSE, a separate event after the terminal
                     (let wait ((k 0) (ls counted))
                       (if (or (member "<eof>" ls) (> k 100)) ls (begin (sleep-ms 50) (wait (+ k 1) (sub-lines s)))))))
               (fs (frames-of ls))
               (tail (reverse fs)))
          ;; BY CONTENT: the last frame with items before the terminal is the
          ;; publication in flight; an empty refold may sit between them.
          (want "ENDING-01 with a write blocked, a publication in flight at the drain is written before (error draining), and the close follows"
                (list (integer? made)
                      (let ((said (find (lambda (f) (and (frame-rev f) (pair? (clause 'items f)))) (if (pair? tail) (cdr tail) '()))))
                        (and said (item-set said)))
                      (and (pair? tail) (car tail))
                      (and (pair? ls) (car (reverse ls))))
                (list #t (expected-set (list 'added "mirrordr.1")) '(error draining) "<eof>")))))
    ;; A DRAIN WHILE AN ACCEPTED REPLAY LIST IS UNWRITTEN: five large frames
    ;; are retained, and a resuming subscriber, paused from its first byte
    ;; with its socket's buffer pressed, cannot take them all -- the daemon's
    ;; write blocks with replay entries still in its list; the drain; then
    ;; the reader resumes and reads every retained frame, then the terminal.
    (let* ((d (start-daemon! "f6p" "THEOURGIA_SEND_BUFFER=16384"))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc))
           (base (current-of acc)))
      ;; EACH WRITER'S FRAME BY CONTENT, past the revision before its write:
      ;; an empty refold may follow any of them.
      (for-each (lambda (w) (let ((r (or (last-published d) 0))) (mirror-many! d w 300) (poke! d) (said-frame-after a r)))
                '("keptbig1" "keptbig2" "keptbig3" "keptbig4" "keptbig5"))
      (let ((b (spawn-subscriber! d (list "changes" (number->string base) token) 'paused)))
        (let wait ((k 0)) (unless (or (contains? (d-log d) "(trace write-pending") (> k 100)) (sleep-ms 50) (wait (+ k 1))))
        (want "ENDING-02 the resuming reader's replay blocks: the daemon traces (write-pending conn) before the drain"
              (contains? (d-log d) "(trace write-pending") #t)
        (let ((pid (d-pid d))) (when pid (system (string-append "kill -TERM " (number->string pid)))))
        (sleep-ms 300)
        (send b '(resume))
        ;; UNTIL THE CLOSE, not until a count: the retained frames may include
        ;; empty refolds, so the reader is read to its end.
        (let* ((ls (let* ((factor (load-factor)) (bound (scaled 15000 factor)) (t0 (real-time)))
                     (let wait ()
                       (let ((ls (sub-lines b)))
                         (cond ((member "<eof>" ls) ls)
                               ((> (- (real-time) t0) bound)
                                (wait-gave-out! "the replay's close" (- (real-time) t0) bound factor (length ls))
                                ls)
                               (else (sleep-ms 50) (wait)))))))
               (fs (frames-of ls)))
          ;; NO TIMER (THEOURGIA_PROBE=off), SO NO REFOLD: the five writers'
          ;; frames are exactly the retained ones, base + 1 to base + 5.
          (want "ENDING-03 a drain with a replay list unwritten: every retained frame, then (error draining), then the close"
                (list (map frame-rev (filter (lambda (f) (eq? (car f) 'changes)) fs))
                      (and (pair? fs) (car (reverse fs)))
                      (and (pair? ls) (car (reverse ls))))
                (list (list (+ base 1) (+ base 2) (+ base 3) (+ base 4) (+ base 5)) '(error draining) "<eof>")))))
    (let* ((d (start-daemon! "f6r" ""))
           (s (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines s 1 5000)))
           (old (token-of acc)))
      (insert! d "before the restart")
      (restart-daemon! d "")
      (let ((a (acceptance-of (await-lines (spawn-subscriber! d (list "changes" "2" old)) 1 5000))))
        (want "ENDING-04 a resume against the restarted daemon with the old token answers daemon-restarted, naming the new token"
              (list (and (pair? a) (car a)) (clause 'reason a) (and (string? (token-of a)) (not (equal? (token-of a) old))))
              (list 'error '(daemon-restarted) #t))
        (want "TOKEN-2 two daemons started one after the other on one store answer different tokens"
              (and (string? (token-of a)) (not (equal? (token-of a) old))) #t))
      (stop-daemon! d))
    ;; THE QUEUE'S BOUNDARY: with a write blocked, 64 queued frames are kept,
    ;; and the 65th enqueue ends the stream with lagging. The daemon traces the
    ;; queue's length at each enqueue, so the row knows where it is. The
    ;; stream is held at the overflow while two more publications are made,
    ;; so their frames reach it after the latch: they are consumed, not
    ;; written.
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-overflow"))
           (_ (system (string-append "rm -f " release " " release ".held")))
           (d (start-daemon! "f6o" (string-append "THEOURGIA_SEND_BUFFER=16384 THEOURGIA_HOLD='stream-overflow:" release "'")))
           (s (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines s 1 5000)))
           (token (token-of acc)))
      (send s '(pause))
      (let* ((made (fill-until-pending! d 40))
             (_ (let loop ((k 0))
                  (when (and (< k 200) (not (contains? (d-log d) "(trace stream-queued 64")))
                    (insert! d (string-append "q" (number->string k))) (loop (+ k 1)))))
             (at-64 (list (contains? (d-log d) "(trace stream-queued 64") (contains? (d-log d) "(trace stream-closed lagging"))))
        (want "ENDING-05 with a write blocked, 64 queued frames are kept: the queue reaches 64 and the stream has not ended"
              (list (integer? made) at-64) (list #t '(#t #f)))
        (insert! d "the 65th")
        (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
        (insert! d "late one") (insert! d "late two")
        (system (string-append "touch " release))
        (send s '(resume))
        (let* ((_ (let wait ((k 0)) (unless (or (member "<eof>" (sub-lines s)) (> k 300)) (sleep-ms 50) (wait (+ k 1)))))
               (ls (sub-lines s))
               (fs (frames-of ls))
               (terminal (find (lambda (f) (and (pair? f) (eq? (car f) 'error))) fs))
               (log (d-log d)))
          (want "ENDING-06 the 65th enqueue: the terminal frame is lagging, with current and this daemon's token"
                (list (and terminal (cadr terminal)) (clause 'reason terminal) (and (clause 'current terminal) #t) (equal? (token-of terminal) token))
                (list 'changes-unavailable '(lagging) #t #t))
          (want "ENDING-07 the frames the store sent before the unsubscription reached it are consumed, not written; nothing follows the terminal; the connection closes"
                (list (count-of log "(trace stream-late")
                      (length (let after ((fs fs)) (cond ((null? fs) '())
                                                         ((and (pair? (car fs)) (eq? (caar fs) 'error)) (cdr fs))
                                                         (else (after (cdr fs))))))
                      (car (reverse ls)))
                (list 2 0 "<eof>"))))
      (stop-daemon! d))
    ;; THE STORE PROCESS DIES: main leaves with 75 at once and does not wait
    ;; for terminal writes, so the stream's
    ;; transport-unknown terminal is attempted and may not arrive. What is
    ;; decided: the daemon exits 75, the subscriber's connection ends, and a
    ;; terminal line, if one arrived, is exactly that one.
    (let* ((d (start-daemon! "f6s" "THEOURGIA_FAULT=store-raise@conn"))
           (s (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines s 1 5000)))
      (ask d 'insert "--title" "raises the store")
      (let* ((_ (let wait ((k 0)) (unless (or (member "<eof>" (sub-lines s)) (> k 160)) (sleep-ms 50) (wait (+ k 1)))))
             (ls (sub-lines s)) (fs (frames-of ls)))
        (want "ENDING-08 the store process dying: the daemon exits 75, the stream ends, and any terminal is transport-unknown store-actor-down"
              (list (let ((t (file-text (list-ref d 5)))) (and (> (string-length t) 0) (read (open-string-input-port t))))
                    (and (pair? ls) (car (reverse ls)))
                    (for-all (lambda (f) (equal? f '(error transport-unknown (reason store-actor-down)))) fs))
              (list 75 "<eof>" #t)))
      (stop-daemon! d))
    ;; A WRITER PROCESS DIES: nothing for the stream. The subscribing
    ;; connection used the writer first, so it is watching it when it dies:
    ;; the writer dies on its SECOND request (writer-raise-second), made by
    ;; another connection while the stream is open.
    (let* ((d (start-daemon! "f6w" "THEOURGIA_FAULT=writer-raise-second@conn"))
           (id (insert! d "w"))
           (s (spawn-subscriber! d '("changes" "0")
                                 (list 'before (envelope (d-store d) 'write (list id "first draft" "--writer" "dying")))))
           (ls (await-lines s 2 5000)))
      (ask d 'write id "second draft" "--writer" "dying")
      (let wait ((k 0)) (unless (or (contains? (d-log d) "(trace daemon-down") (> k 100)) (sleep-ms 50) (wait (+ k 1))))
      (insert! d "after the writer died")
      (let ((ls (await-lines s 3 6000)))
        (want "ENDING-09 a writer the stream's connection was watching dies: the stream goes on and the next frame arrives"
              (list (contains? (d-log d) "(trace daemon-down")
                    (and (>= (length ls) 3) (guard (e (#t #f)) (car (read (open-string-input-port (list-ref ls 2)))))))
              '(#t changes)))
      (stop-daemon! d))
    ;; A WRITE THAT FAILS: the subscriber's connection is closed while the
    ;; daemon's write to it is blocked; the stream ends without a terminal,
    ;; and the daemon goes on serving.
    (let* ((d (start-daemon! "f6x" "THEOURGIA_SEND_BUFFER=16384"))
           (s (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines s 1 5000)))
      (send s '(pause))
      (let ((made (fill-until-pending! d 40)))
        (send s '(close))
        (let wait ((k 0)) (unless (or (contains? (d-log d) "(trace stream-closed") (> k 200)) (sleep-ms 50) (wait (+ k 1))))
        (want "ENDING-10 the subscriber gone while a write is blocked: the stream ends, and the daemon still answers"
              (list (integer? made) (contains? (d-log d) "(trace stream-closed") (car (ask d 'outline)))
              (list #t #t 'ok)))
      (stop-daemon! d))

    ;; A WRITE THAT RAISES ON A USABLE CONNECTION: the frame's write raises
    ;; before anything is sent (stream-write-raise), the connection stays
    ;; open, and the stream ends with the terminal attempted once.
    (let* ((d (start-daemon! "f6r2" "THEOURGIA_FAULT=stream-write-raise@conn"))
           (s (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines s 1 5000)))
      (insert! d "its write raises")
      (let* ((_ (let wait ((k 0)) (unless (or (member "<eof>" (sub-lines s)) (> k 160)) (sleep-ms 50) (wait (+ k 1)))))
             (ls (sub-lines s)) (fs (frames-of ls)))
        (want "ENDING-11 a write that raises with the connection still open: the terminal transport-unknown write-failed, no frame, then the close"
              (list (contains? (d-log d) "(trace fault stream-write-raise") fs (and (pair? ls) (car (reverse ls))))
              (list #t '((error transport-unknown (reason write-failed))) "<eof>")))
      (stop-daemon! d))

    ;; A RELOAD THAT FAILS keeps the previous publication; subscribers are
    ;; told, with the reason the trace carries, and the stream goes on.
    (let* ((d (start-daemon! "f6f" "THEOURGIA_FAULT=reload-raise@conn"))
           (s (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines s 1 5000)))
           (before (current-of acc)))
      (mirror! d "mirrorrr" 1 '() '(put ((kind . section) (title . "unfolded") (parent . root) (ord . 70))))
      (poke! d)
      ;; BY POSITION, AND RIGHT: the first line after the acceptance is the
      ;; failed reload's notice; a second notice may follow it, never precede.
      (let* ((ls (await-lines s 2 6000)) (f (and (> (length ls) 1) (car (frames-of ls))))
             (traced (let find ((ds (log-data (d-log d))))
                       (cond ((null? ds) 'no-trace)
                             ((and (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)) (eq? (cadar ds) 'reload-failed))
                              (caddar ds))
                             (else (find (cdr ds)))))))
        (want "ENDING-12 a reload that fails sends (error store-unreadable (reason <the trace's reason>)) with no revision"
              ;; THE TRACE PRINTS ITS FIELDS WITH display, so the reason is
              ;; compared as that line spells it.
              (list (and (pair? f) (car f)) (and (pair? f) (cadr f)) (not (eq? traced 'no-trace))
                    (let ((r (clause 'reason f)))
                      (and (pair? r) (contains? (d-log d) (string-append "(trace reload-failed " (format "~a" (car r)) " #f)"))))
                    (frame-rev f))
              (list 'error 'store-unreadable #t #t #f))
        (want "ENDING-13 and the publication stayed: the last revision published is still the one before"
              (last-published d) before))
      (insert! d "after the failed reload")
      ;; BY CONTENT: a timer probe may add a second notice before it.
      (let ((f (said-frame-after s before)))
        (want "ENDING-14 and the stream goes on: the next local commit's frame is the next revision"
              (and (pair? f) (list (car f) (frame-rev f)))
              (list 'changes (+ before 1))))
      (stop-daemon! d))

    ;; ==== LACKS: incomplete and withheld publications ====
    (printf "~%== LACKS: a frame says what its publication lacks ==~%")
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-early"))
           (_ (system (string-append "rm -f " release " " release ".held")))
           (d (start-daemon! "f10" (string-append "THEOURGIA_HOLD='reload-before-publish:" release "'")))
           (id (insert! d "local block"))
           (_ (begin (mirror! d "mirrorin" 1 '() '(put ((kind . section) (title . "kept") (parent . root) (ord . 50))))
                     (mirror! d "mirrorin" 2 '() '(put ((kind . section) (title . "lost") (parent . root) (ord . 51))))))
           (current (string-append (writer-directory (d-store d) "mirrorin") "/" (segment-file-name 2))))
      ;; the first reload passes the hold, and the hold is re-armed
      (poke! d)
      (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
      (system (string-append "touch " release))
      (let wait ((k 0)) (unless (or (contains? (d-log d) "(trace published 3") (> k 200)) (sleep-ms 50) (wait (+ k 1))))
      (system (string-append "rm -f " release " " release ".held"))
      (let* ((sub (spawn-subscriber! d '("changes" "0")))
             (acc (acceptance-of (await-lines sub 1 5000)))
             (old (current-of acc)))
        ;; THE EARLY NOTICE: the current segment cannot be opened; the probe
        ;; sees it, the read answers from the old publication with the
        ;; clause, and the store process is held before publishing the next.
        (system (string-append "chmod 000 " current))
        (let ((early (ask d 'read id "--rev")))
          (want "LACKS-1 a read --rev after the damage answers the OLD revision, and says incomplete"
                (list (and (rev-clause early) (car (rev-clause early))) (and (clause 'incomplete early) #t))
                (list old #t)))
        (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
        (system (string-append "touch " release))
        (let* ((f (said-frame-after sub old))
               (later (ask d 'read id "--rev")))
          (want "LACKS-2 the next revision's frame names the unread writer's block removed and carries (incomplete ...)"
                (list (frame-rev f) (item-set f) (and (clause 'incomplete f) #t))
                (list (+ old 1) (expected-set (list 'removed "mirrorin.2")) #t))
          ;; THE CELL'S THREE READERS SAY ONE THING: the frame (made in
          ;; publish!), the acceptance (subscription-answer) and a read
          ;; (answer-published), on the same publication, carry one clause.
          (let ((acc2 (acceptance-of (await-lines (spawn-subscriber! d '("changes" "0")) 1 5000))))
            (want "LACKS-3 on one publication the frame's, the acceptance's and read's incomplete clauses are present and equal"
                  (list (and (rev-clause later) (car (rev-clause later))) (current-of acc2)
                        (and (clause 'incomplete f) #t)
                        (equal? (clause 'incomplete f) (clause 'incomplete later))
                        (equal? (clause 'incomplete acc2) (clause 'incomplete f)))
                  (list (frame-rev f) (frame-rev f) #t #t #t))))
        (system (string-append "chmod 600 " current))
        (poke! d)
        ;; past the damage's frame, old + 1 (read above)
        (let ((f (said-frame-after sub (+ old 1))))
          (want "LACKS-4 the segment readable again and a reload: the block is added back, and no clause"
                (list (item-set f) (clause 'incomplete f))
                (list (expected-set (list 'added "mirrorin.2")) #f))))
      (stop-daemon! d))

    ;; A WITHHELD PUBLICATION: a session whose load is damaged while it is
    ;; held after its delivery barrier writes, and the gate withholds its
    ;; publication. No revision, no frame; the reload it asks for makes one.
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-barrier"))
           (_ (system (string-append "rm -f " release " " release ".held")))
           ;; THE START'S OWN LOAD PASSES THE BARRIER TOO, before the socket
           ;; exists: a releaser lets it through, and the hold is armed again.
           (_ (system (string-append "sh -c 'k=0; while [ ! -e " release ".held ] && [ $k -lt 600 ]; do sleep 0.05; k=$((k+1)); done; "
                                     "touch " release "; sleep 0.5; rm -f " release " " release ".held' &")))
           (d (start-daemon! "f10w" (string-append "THEOURGIA_HOLD='after-barrier:" release "'")))
           (_ (let wait ((k 0)) (unless (or (not (file-exists? release)) (> k 100)) (sleep-ms 50) (wait (+ k 1)))))
           (_ (begin (mirror! d "mirrorwd" 1 '() '(put ((kind . section) (title . "old") (parent . root) (ord . 60))))
                     (mirror! d "mirrorwd" 2 '() '(put ((kind . section) (title . "new") (parent . root) (ord . 61))))))
           (older (string-append (writer-directory (d-store d) "mirrorwd") "/" (segment-file-name 1)))
           (sub (spawn-subscriber! d '("changes" "0")))
           (_ (await-lines sub 1 5000))
           (asker self))
      (spawn (lambda () (send asker (list 'insert-answer (ask d 'insert "--title" "withheld")))))
      (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 400)) (sleep-ms 50) (wait (+ k 1))))
      (let* ((bv (call-with-port (open-file-input-port older) get-bytevector-all))
             (i (quotient (bytevector-length bv) 2)))
        (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
        (call-with-port (open-file-output-port older (file-options no-fail)) (lambda (p) (put-bytevector p bv)))
        (system (string-append "touch " release))
        (receive (after 30000 'no-insert-answer) (`(insert-answer ,a) a))
        (sleep-ms 500)
        ;; THE BOUNDARY: withholding asks for a reload at once, so a
        ;; publication soon follows; what the withheld session itself may not
        ;; do is publish. The first publication after the withheld trace is
        ;; the reload's: a NEW load runs first, and it passes the barrier's
        ;; hold again before anything is published.
        (want "LACKS-5 the withheld session published nothing itself: after its trace, a new load passes the barrier before the next publication"
              (let find ((ds (log-data (d-log d))) (state 'before))
                (cond ((null? ds) state)
                      ((not (and (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)))) (find (cdr ds) state))
                      ((and (eq? state 'before) (eq? (cadar ds) 'publish-withheld)) (find (cdr ds) 'withheld))
                      ((and (eq? state 'withheld) (eq? (cadar ds) 'hold) (eq? (caddar ds) 'after-barrier)) (find (cdr ds) 'reloading))
                      ((and (eq? state 'withheld) (eq? (cadar ds) 'published)) 'published-without-a-new-load)
                      ((and (eq? state 'reloading) (eq? (cadar ds) 'published)) 'reload-published)
                      (else (find (cdr ds) state))))
              'reload-published)
        (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
        (call-with-port (open-file-output-port older (file-options no-fail)) (lambda (p) (put-bytevector p bv)))
        (poke! d)
        ;; THE ROW'S STATE, AWAITED, not a sleep and not this poke's reload: the
        ;; repair may change no snapshot the probe compares, so the poke may
        ;; queue nothing. The wait is for the claim's condition -- a
        ;; publication after the withheld trace and a frame for each -- or the
        ;; bound, and the row then reads what is there.
        (let* ((published-after
                 (lambda ()
                   (let find ((ds (log-data (d-log d))) (seen #f) (n 0))
                     (cond ((null? ds) (and seen n))
                           ((and (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)) (eq? (cadar ds) 'publish-withheld))
                            (find (cdr ds) #t n))
                           ((and seen (pair? (car ds)) (eq? (caar ds) 'trace) (pair? (cdar ds)) (eq? (cadar ds) 'published))
                            (find (cdr ds) seen (+ n 1)))
                           (else (find (cdr ds) seen n))))))
               (_ (let wait ((k 0))
                    (let ((n (published-after)))
                      (unless (or (and n (>= n 1) (= n (length (frames-of (sub-lines sub))))) (> k 100))
                        (sleep-ms 50) (wait (+ k 1))))))
               (after (published-after)))
          (want "LACKS-6 the session's publication is withheld: traced, and after it every published revision has exactly one frame"
                (list (and after #t) (and after (>= after 1)) (and after (= after (length (frames-of (sub-lines sub))))))
                '(#t #t #t))))
      (stop-daemon! d))

    ;; ==== RETAIN: the window: what a resume can still be given ====
    (printf "~%== RETAIN: retention, the window, the gap, eviction ==~%")
    ;; W1: a resume two revisions back gets exactly the two retained frames.
    (let* ((d (start-daemon! "w1" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc)) (base (current-of acc)))
      (insert! d "w1 one") (insert! d "w1 two") (insert! d "w1 three")
      (await-lines a 4 6000)
      (let* ((b (spawn-subscriber! d (list "changes" (number->string (+ base 1)) token))))
        (await-lines b 3 6000)
        (insert! d "w1 live")
        (await-lines b 4 6000)
        (want "RETAIN-1 a resume at current-2 receives the two retained frames in order, then live ones"
              (revs-of b) (list (+ base 2) (+ base 3) (+ base 4))))
      (stop-daemon! d))
    ;; W2: 258 publications with a subscriber throughout; the ring keeps 256.
    ;; The sets travel on separate connections: the publications are what
    ;; the row counts, and one connection would only make them faster.
    (let* ((d (start-daemon! "w2" ""))
           (id (insert! d "w2 block"))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc)) (base (current-of acc)))
      (let loop ((k 1)) (when (<= k 258) (ask d 'set id "src" (number->string k)) (loop (+ k 1))))
      (let wait ((k 0)) (unless (or (equal? (last-published d) (+ base 258)) (> k 200)) (sleep-ms 50) (wait (+ k 1))))
      (let* ((oldest (+ base 3)) (current (+ base 258))
             (b (spawn-subscriber! d (list "changes" (number->string (- oldest 1)) token)))
             (ls (await-lines b 257 30000)))
        ;; FOUR WITNESSES OF ONE FACT, so a red names the one that
        ;; fell short: the trace as the log reader reads it; the trace's
        ;; text, read without the reader; the number of published lines in
        ;; that text, one per publication since the start; and the last
        ;; revision b was replayed. The first short alone is the reader; the
        ;; second and third, lines lost before the file; all four, the
        ;; publications.
        (let* ((text (d-log d)) (raw (raw-trace-numbers text 'published)) (revs (revs-of b)))
          (note-torn! "retention W2" text)
          (want "PUB-4 258 publications counted four ways: the read trace, its text, its lines, the replayed frames"
                (list (last-published d) (and (pair? raw) (apply max raw)) (length raw) (and (pair? revs) (car (last-pair revs))))
                (list current current current current)))
        (want "RETAIN-2 a resume at oldest-1 is accepted and replays all 256 retained frames, in order, without lagging"
              (list (and (pair? (acceptance-of ls)) (car (acceptance-of ls)))
                    (equal? (revs-of b) (let loop ((r current) (out '())) (if (< r oldest) out (loop (- r 1) (cons r out)))))
                    (exists (lambda (f) (and (pair? f) (eq? (car f) 'error))) (frames-of ls)))
              '(ok #t #f))
        (let ((c (acceptance-of (await-lines (spawn-subscriber! d (list "changes" (number->string (- oldest 2)) token)) 1 5000))))
          (want "RETAIN-3 a resume at oldest-2 answers window with oldest and current"
                (list (and (pair? c) (cadr c)) (clause 'reason c) (clause 'oldest c) (clause 'current c) (equal? (token-of c) token))
                (list 'changes-unavailable '(window) (list oldest) (list current) #t))))
      (stop-daemon! d))
    ;; W3: the gap: nobody subscribed, one publication made unframed. The
    ;; store's own unregistration is waited for, not a sleep.
    (let* ((d (start-daemon! "w3" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc)) (base (current-of acc)))
      (insert! d "w3 framed")
      (await-lines a 2 5000)
      (send a '(close))
      (await-trace d "(trace stream-unregistered" 1 5000)
      (insert! d "w3 unframed")
      (let ((c (acceptance-of (await-lines (spawn-subscriber! d (list "changes" (number->string (+ base 1)) token)) 1 5000))))
        (want "RETAIN-4 after the last subscriber leaves and one more publication, a resume from the last framed revision answers window"
              (list (and (pair? c) (cadr c)) (clause 'reason c))
              (list 'changes-unavailable '(window))))
      (stop-daemon! d))
    ;; W4: framing stopped and restarted leaves a hole the endpoints do not show.
    (let* ((d (start-daemon! "w4" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc)) (base (current-of acc)))
      (insert! d "w4 two") (insert! d "w4 three")
      (await-lines a 3 5000)
      (send a '(close))
      (await-trace d "(trace stream-unregistered" 1 5000)
      (insert! d "w4 four, unframed")
      (let ((b (spawn-subscriber! d '("changes" "0"))))
        (await-lines b 1 5000)
        (insert! d "w4 five, framed")
        (await-lines b 2 5000)
        (let ((c (acceptance-of (await-lines (spawn-subscriber! d (list "changes" (number->string (+ base 2)) token)) 1 5000))))
          (want "RETAIN-5 restarted framing: a resume across the unframed revision answers window, oldest still the first retained"
                (list (and (pair? c) (cadr c)) (clause 'reason c) (clause 'oldest c) (clause 'current c))
                (list 'changes-unavailable '(window) (list (+ base 1)) (list (+ base 4))))))
      (stop-daemon! d))
    ;; W5 and W6: EVICTION while a replay is held. The ring is full (256
    ;; frames, oldest = base+3); a second reader resumes from oldest-1 paused
    ;; from its first byte with its send buffer pressed, so its replay of 256
    ;; frames blocks -- the write-pending traces counted after it subscribed
    ;; are its own, since the first reader reads promptly. Then LIVE
    ;; publications EVICT that many of the oldest frames from the ring: the
    ;; replay list it was accepted with must still hold them.
    (for-each
      (lambda (live)
        (let* ((d (start-daemon! (string-append "w5-" (number->string live)) "THEOURGIA_SEND_BUFFER=16384"))
               (id (insert! d "w5 block"))
               (a (spawn-subscriber! d '("changes" "0")))
               (acc (acceptance-of (await-lines a 1 5000)))
               (token (token-of acc)) (base (current-of acc)))
          (let loop ((k 1)) (when (<= k 258) (ask d 'set id "src" (number->string k)) (loop (+ k 1))))
          (await-lines a 259 30000)
          (let* ((oldest (+ base 3)) (current (+ base 258))
                 (pending-before (count-of (d-log d) "(trace write-pending"))
                 (b (spawn-subscriber! d (list "changes" (number->string (- oldest 1)) token) 'paused)))
            (await-trace d "(trace write-pending" (+ pending-before 1) 5000)
            (let ((held (> (count-of (d-log d) "(trace write-pending") pending-before)))
              (let loop ((k 0)) (when (< k live) (ask d 'set id "src" (string-append "live " (number->string k))) (loop (+ k 1))))
              (send b '(resume))
              (let* ((_ (let wait ((k 0)) (unless (or (>= (length (sub-lines b)) (+ 1 256 live)) (member "<eof>" (sub-lines b)) (> k 400)) (sleep-ms 50) (wait (+ k 1)))))
                     (_ (sleep-ms 1000))
                     (fs (frames-of (sub-lines b)))
                     (revs (map frame-rev (filter (lambda (f) (and (pair? f) (eq? (car f) 'changes))) fs))))
                (if (< live 64)
                    (want (string-append "RETAIN-6 eviction: " (number->string live) " publications evict retained frames while the replay is held, and every accepted frame still arrives, in order, before the live ones")
                          (list held
                                (equal? revs (let loop ((r (+ current live)) (out '())) (if (< r oldest) out (loop (- r 1) (cons r out)))))
                                (exists (lambda (f) (and (pair? f) (eq? (car f) 'error))) fs))
                          '(#t #t #f))
                    ;; THE TWIN: the replay is dropped at the latch, so what came
                    ;; before the terminal is a strict prefix of it, consecutive
                    ;; from oldest, and nothing follows the terminal.
                    (want (string-append "RETAIN-7 eviction's overflow twin: " (number->string live) " publications end the stream lagging; before the terminal only a consecutive prefix of the replay, after it nothing")
                          (let* ((before (let take ((fs fs) (out '())) (cond ((null? fs) (reverse out)) ((and (pair? (car fs)) (eq? (caar fs) 'error)) (reverse out)) (else (take (cdr fs) (cons (frame-rev (car fs)) out))))))
                                 (after (let skip ((fs fs)) (cond ((null? fs) '()) ((and (pair? (car fs)) (eq? (caar fs) 'error)) (cdr fs)) (else (skip (cdr fs))))))
                                 (t (find (lambda (f) (and (pair? f) (eq? (car f) 'error))) fs)))
                            (list held
                                  (and t (clause 'reason t))
                                  (< (length before) 256)
                                  (equal? before (let loop ((r (+ oldest (length before) -1)) (out '())) (if (< r oldest) out (loop (- r 1) (cons r out)))))
                                  after
                                  (car (reverse (sub-lines b)))))
                          '(#t (lagging) #t #t () "<eof>"))))))
          (stop-daemon! d)))
      '(40 65))

    ;; ==== PROBE and the timer: outside changes reach a subscriber ====
    (printf "~%== PROBE: the timer probes while someone follows, and only then ==~%")
    ;; P1-P3. The bounds: no probe in 5 s with nobody subscribed (the timer
    ;; ticks every second, so a running one would show five); an outside
    ;; commit within 2 s (one tick to see it, one to spare); no probe in 3 s once the last subscriber is unregistered.
    (let* ((d (start-timer-daemon! "p1" "")))
      (sleep-ms 5000)
      (want "PROBE-01 with no subscriber, no probe in five seconds"
            (count-of (d-log d) "(trace probe ") 0)
      (let* ((a (spawn-subscriber! d '("changes" "0")))
             (_ (await-lines a 1 5000))
             (_ (await-trace d "(trace probe " 1 3000)))
        (mirror! d "outsideq" 1 '() '(put ((kind . section) (title . "made outside") (parent . root) (ord . 80))))
        (let* ((t0 (real-time))
               (ls (await-lines a 2 4000)) (dt (- (real-time) t0)) (f (and (> (length ls) 1) (car (frames-of ls)))))
          (printf "   outside commit to frame: ~s ms~%" dt)
          (want "PROBE-02 a commit made outside the daemon reaches a subscriber within two seconds, with no request made"
                (list (and f (item-set f)) (<= dt 2000))
                (list (expected-set (list 'added "outsideq.1")) #t)))
        (send a '(close))
        (await-trace d "(trace stream-unregistered" 1 5000)
        (let ((n (count-of (d-log d) "(trace probe ")))
          (sleep-ms 3000)
          (want "PROBE-03 after the last subscriber is unregistered, no probe in three seconds"
                (- (count-of (d-log d) "(trace probe ") n) 0)))
      (stop-daemon! d))
    ;; POFF: THEOURGIA_PROBE=off, the setting every other row's daemon runs
    ;; under: with a subscriber live, no probe in three seconds (a running
    ;; timer would show two or three). The rows above, started with the
    ;; variable unset, read the timer as in use.
    (let* ((d (start-daemon! "poff" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000))))
      (sleep-ms 3000)
      (want "PROBE-04 POFF with the probe off and a subscriber live -- accepted and still open -- no probe in three seconds"
            (list (and (pair? acc) (car acc)) (and (member "<eof>" (sub-lines a)) #t) (count-of (d-log d) "(trace probe "))
            '(ok #f 0))
      (stop-daemon! d))
    ;; P6: the timer follows the subscriber count: one of two leaving keeps it
    ;; running, the last leaving stops it, a new subscriber starts it again.
    (let* ((d (start-timer-daemon! "p6" ""))
           (a (spawn-subscriber! d '("changes" "0")))
           (b (spawn-subscriber! d '("changes" "0"))))
      (await-lines a 1 5000) (await-lines b 1 5000)
      (send a '(close))
      (await-trace d "(trace stream-unregistered" 1 5000)
      (let ((n1 (count-of (d-log d) "(trace probe ")))
        (sleep-ms 2500)
        (let ((n2 (count-of (d-log d) "(trace probe ")))
          (send b '(close))
          (await-trace d "(trace stream-unregistered" 2 5000)
          (let ((n3 (count-of (d-log d) "(trace probe ")))
            (sleep-ms 2500)
            (let ((n4 (count-of (d-log d) "(trace probe ")))
              (await-lines (spawn-subscriber! d '("changes" "0")) 1 5000)
              (sleep-ms 2500)
              (want "PROBE-05 the timer follows the subscribers: one of two leaving keeps it, the last stops it, a new one starts it again"
                    (list (>= (- n2 n1) 1) (- n4 n3) (>= (- (count-of (d-log d) "(trace probe ") n4) 1))
                    '(#t 0 #t))))))
      (stop-daemon! d))
    ;; P7: A SNAPSHOT THAT CANNOT BE TAKEN COUNTS AS BEHIND: with the timer's
    ;; snapshot failing once, the probe reloads, and a frame arrives with no
    ;; commit at all.
    (let* ((d (start-timer-daemon! "p7" "THEOURGIA_FAULT=refresh-snapshot-raise@conn"))
           (a (spawn-subscriber! d '("changes" "0"))))
      (await-lines a 1 5000)
      (let* ((ls (await-lines a 2 4000)) (f (and (> (length ls) 1) (car (frames-of ls)))))
        (want "PROBE-06 a probe whose snapshot cannot be taken reloads: a frame arrives with no commit made"
              (list (contains? (d-log d) "(trace fault refresh-snapshot-raise") (and f (car f)) (and f (item-set f)))
              (list #t 'changes '())))
      (stop-daemon! d))
    ;; AN EMPTY FRAME IS A FRAME: kept in the ring like any other. The probe's
    ;; reload of the same state (a snapshot it could not take) publishes an
    ;; empty frame at base+1, a commit makes base+2, and a resume from base
    ;; is accepted and replays both -- a withheld empty frame would be a hole
    ;; and the resume would be refused window for nothing.
    (let* ((d (start-timer-daemon! "pe" "THEOURGIA_FAULT=refresh-snapshot-raise@conn"))
           (a (spawn-subscriber! d '("changes" "0")))
           (acc (acceptance-of (await-lines a 1 5000)))
           (token (token-of acc)) (base (current-of acc)))
      (await-lines a 2 4000)
      (insert! d "after the empty one")
      (await-lines a 3 4000)
      (let* ((b (spawn-subscriber! d (list "changes" (number->string base) token)))
             (ls (await-lines b 3 5000)) (fs (frames-of ls)))
        (want "PROBE-07 a resume across a revision whose frame was empty is accepted and replays the empty frame, then the next"
              (list (let ((c (acceptance-of ls))) (and (pair? c) (car c)))
                    (map frame-rev fs) (and (pair? fs) (item-set (car fs))))
              (list 'ok (list (+ base 1) (+ base 2)) '())))
      (stop-daemon! d))
    ;; P4: ONE PROBE OUTSTANDING. The store process traces a probe when it
    ;; receives it and is then held before acknowledging, about 3.5 s: during
    ;; the hold exactly one probe and at least two skipped ticks (3.5 s at
    ;; one a second leaves a margin of one). A commit asked during the hold
    ;; is answered only after the release: the probe and the commit are the
    ;; same actor's.
    (let* ((release (string-append scratch-base "/cs-" pid-text "-hold-probe"))
           (_ (system (string-append "rm -f " release " " release ".held")))
           (d (start-timer-daemon! "p4" (string-append "THEOURGIA_HOLD='probe-before-ack:" release "'")))
           (a (spawn-subscriber! d '("changes" "0")))
           (asker self))
      (await-lines a 1 5000)
      (let wait ((k 0)) (unless (or (file-exists? (string-append release ".held")) (> k 100)) (sleep-ms 50) (wait (+ k 1))))
      (let ((entered (file-exists? (string-append release ".held"))))
        (spawn (lambda () (send asker (list 'held-commit (ask d 'insert "--title" "during the held probe") (real-time)))))
        (sleep-ms 3500)
        (let ((probes (count-of (d-log d) "(trace probe ")) (skipped (count-of (d-log d) "(trace probe-skipped "))
              (released-at (real-time)))
          (system (string-append "touch " release))
          (let ((answered (receive (after 8000 #f) (`(held-commit ,a ,t) t))))
            (sleep-ms 1600)
            (want "PROBE-08 with an acknowledgement late, the timer skips its ticks: the hold entered, one probe during it, at least two skipped, then probes again"
                  (list entered probes (>= skipped 2) (> (count-of (d-log d) "(trace probe ") probes))
                  '(#t 1 #t #t))
            (want "PROBE-09 a commit asked during the held probe is answered only after the release: the probe and the commit are one actor's"
                  (and answered (>= answered released-at))
                  #t))))
      (stop-daemon! d))
    ;; P5: THE SCHEDULE IS ABSOLUTE. With every probe taking 300 ms before its
    ;; acknowledgement, six consecutive probes keep a mean interval between
    ;; 850 and 1150 ms: an absolute schedule gives 1000, a schedule counted
    ;; from the acknowledgement gives about 1300, a faster timer less than
    ;; 850; 150 ms either side is the margin, half the probe's own 300.
    (let* ((d (start-timer-daemon! "p5" "THEOURGIA_FAULT=probe-slow@conn"))
           (a (spawn-subscriber! d '("changes" "0"))))
      (await-lines a 1 5000)
      (await-trace d "(trace probe " 7 12000)
      (let* ((instants (trace-values d 'probe))
             (six (and (>= (length instants) 7) (list-head (cdr instants) 6)))
             (mean (and six (/ (- (car (reverse six)) (car six)) 5))))
        (printf "   probe instants (ms): ~s~%" instants)
        (want "PROBE-10 with each probe taking 300 ms, six consecutive probes keep a mean interval within 850..1150 ms (the schedule is absolute, 1000 ms)"
              (and mean (<= 850 mean 1150))
              #t))
      (stop-daemon! d))

    ;; ==== TWIN: the comparator's structural sets are state-structure's ====
    (printf "~%== TWIN: two implementations of one rule, on every store above ==~%")
    (want "TWIN-3 on every store the rows built, the comparator's four structural sets equal state-structure's"
          (filter (lambda (x) x)
                  (map (lambda (st)
                         (guard (e (#t (list st 'RAISED (if (message-condition? e) (condition-message e) e))))
                           (let ((r (open-and-reduce st)))
                             (if (equal? (structural-sets r) (state-structure r)) #f (list st 'DIFFER)))))
                       all-stores))
          '())

    ;; ==== COST: the cost of a frame, measured ====
    ;; THE COST STORE: 5000 blocks, three edges each, 1 KiB bodies, written
    ;; as one segment of another writer and folded once; then one subscriber
    ;; and three publications, each frame's time read from the daemon's
    ;; frame-time trace and bounded at 20 ms.
    (printf "~%== COST: the per-publication frame time on the cost store ==~%")
    (let* ((d (start-daemon! "cost" ""))
           (body (make-string 1024 #\b))
           ;; a block's id carries its record's sequence in base 36, as
           ;; reduce.sc's block-id spells it
           (b36 (lambda (k)
                  (let loop ((k k) (out '()))
                    (if (< k 36)
                        (list->string (cons (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" k) out))
                        (loop (div k 36) (cons (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" (mod k 36)) out))))))
           (n 5000)
           (records
             (let loop ((k 1) (seq 1) (out '()))
               (if (> k n)
                   (reverse out)
                   (let* ((put (encode-record seq (+ 1789000000000 seq) "peer" '()
                                              (storable-encode
                                                (list 'put (list (cons 'kind 'section)
                                                                 (cons 'title (string-append "cost " (number->string k)))
                                                                 (cons 'parent 'root) (cons 'ord k) (cons 'src body))))))
                          (links (let edge ((e 1) (out '()))
                                   (if (> e 3) (reverse out)
                                       (edge (+ e 1)
                                             (cons (encode-record (+ seq e) (+ 1789000000000 seq e) "peer" '()
                                                                  (storable-encode
                                                                    (list 'link (string-append "costwrtr." (b36 seq))
                                                                          'relates (string-append "costwrtr." (b36 (+ 1 (* 4 (mod (+ k e) n))))))))
                                                   out))))))
                     (loop (+ k 1) (+ seq 4) (append (reverse (cons put links)) out))))))
           (bytes (string->utf8 (apply string-append (map utf8->string records)))))
      (log-publish! (d-store d) "costwrtr" 1 bytes (segment-sha bytes))
      ;; NO WAIT NEEDED: the reload is queued before the subscription is sent,
      ;; and the store process answers both in order, so the subscription is
      ;; accepted after the cost store's publication and the frames below are
      ;; the three sets'.
      (poke! d)
      (let* ((a (spawn-subscriber! d '("changes" "0")))
             (st (d-store d)))
        (await-lines a 1 30000)
        ;; the reduction before the three sets, folded here from the log, for
        ;; the shortcut row below (the fixture's time, not the daemon's)
        (set! cost-before (open-and-reduce st))
        ;; the publications change one of the 5000, so it stays the cost store
        ;; NEVER: ONE SET, THEN ITS OWN FRAME, THEN THE NEXT SET, each with its own
        ;; wait. On a gate under load this row read two frame-time lines
        ;; for three sets, each frame well inside its bound, and green twice
        ;; alone. The sets are answered one at a time, and each publication is
        ;; either framed before its answer or, withheld, has its reload queued
        ;; before the answer and so before the next set: they do not share a
        ;; frame. What the
        ;; reading does not say is which wait gave out -- a set's answer (15 s)
        ;; or the one 30 s wait for all three frames. So each set now waits up
        ;; to 30 s for its own frame-time line before the next is sent, and the
        ;; row reads, per set, the answer's head and the frames it made: a set
        ;; that failed, timed out, or made no frame says which.
        (let* ((per-set
                 (let loop ((k 1) (out '()))
                   (if (> k 3)
                       (reverse out)
                       (let* ((before (length (trace-values d 'frame-time)))
                              (answer (ask d 'set "costwrtr.1" "src" (string-append "cost run " (number->string k)))))
                         (await-trace d "(trace frame-time " (+ before 1) 30000)
                         (loop (+ k 1)
                               (cons (list (if (pair? answer) (car answer) answer)
                                           (- (length (trace-values d 'frame-time)) before))
                                     out))))))
               (_ (await-lines a 4 30000))
               (times (trace-values d 'frame-time)))
          (printf "   frame times (ms), three publications on the cost store: ~s~%" times)
          ;; A TORN TRACE LINE IS SAID HERE TOO: the per-set wait counts
          ;; the text and the count reads the lines, and a red under load
          ;; once read two lines for three sets.
          (note-torn! "frame cost" (d-log d))
          (want "COST-1 three frames on the cost store, one for each set, each computed in at most 20 ms"
                (list per-set (length times) (for-all (lambda (t) (and (number? t) (<= t 20))) times))
                '(((ok 1) (ok 1) (ok 1)) 3 #t)))
        ;; THE SHORTCUTS CHANGE NO ITEM: on the cost store, the product's
        ;; change-items equals the reference (the comparison without them)
        ;; item for item, across the three sets (one block's field differs,
        ;; the links are equal) and across one more writer's link (the links
        ;; differ, every block is equal), and across one link REPLACED by
        ;; another in one reload: the two lists are as long as each other and
        ;; differ (a skip judged by length would say nothing there). The
        ;; reductions are folded here from the store's log: the first before
        ;; the sets, the others after the frames above were timed.
        (let* ((r1 (open-and-reduce st))
               (_ (mirror! d "costlink" 1 '() (list 'link "costwrtr.1" 'relates "costwrtr.5")))
               (_ (poke! d))
               (_ (await-lines a 5 30000))
               (r2 (open-and-reduce st))
               ;; costwrtr.l is block 21 (21 = 1 mod 4); costwrtr.1's own
               ;; edges go to 9, d and h
               (_ (mirror! d "costlink" 2 '(("costlink" . 1)) (list 'unlink "costwrtr.1" 'relates "costwrtr.5")))
               (_ (mirror! d "costlink" 3 '(("costlink" . 2)) (list 'link "costwrtr.1" 'relates "costwrtr.l")))
               (r3 (open-and-reduce st))
               (r0 cost-before)
               (pair (lambda (o n)
                       (let ((os (structural-sets o)) (ns (structural-sets n)))
                         (let ((p (list-sort string<? (map (lambda (i) (format "~s" i)) (change-items o n os ns))))
                               (q (list-sort string<? (map (lambda (i) (format "~s" i)) (reference-change-items o n os ns)))))
                           (list (equal? p q) p))))))
          (let ((sets (pair r0 r1)) (link (pair r1 r2)) (swap (pair r2 r3))
                (links-of (lambda (r) (length (cadr (reduction-facts r))))))
            (want "COST-2 on the cost store the frame with the shortcuts equals the full comparison, item for item: across the sets, across a link, across a link replaced"
                  (list (car sets) (cadr sets)
                        (car link) (and (exists (lambda (i) (contains? i "edge-added")) (cadr link)) #t)
                        (car swap) (= (links-of r2) (links-of r3))
                        (and (exists (lambda (i) (contains? i "edge-added")) (cadr swap))
                             (exists (lambda (i) (contains? i "edge-removed")) (cadr swap)) #t))
                  (list #t '("(changed \"costwrtr.1\" src)") #t #t #t #t #t)))))
      (stop-daemon! d))

    (printf "rows: ~a~%~a failures~%change-stream complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
