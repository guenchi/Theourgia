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

;; THE IDLE EXIT: a daemon started with `--idle-exit <seconds>` leaves by
;; itself after that long with no connection open and nothing running, and a
;; connection that arrives while it is leaving cancels the exit.
;;
;; The leave, in order: the listener unlinks the public link (a connect after
;; that answers ENOENT, nothing sent), keeps taking the connections the kernel
;; completed before the unlink for a grace (`--idle-grace-ms`), stops, and
;; acknowledges once every adapter it spawned has reported. Main exits when no
;; connection arrived after the clock fired, and otherwise tells the listener
;; to bind and publish again. A connection that arrives during a leave is
;; registered at once (so it cancels the exit) and admitted only when the
;; attempt is decided: served after the resume, refused `draining` when the
;; resume fails or a drain supersedes. Each row starts its daemon by hand and
;; arms the hold stages it needs; every client is a separate process.

(import (chezscheme)
        (only (theourgia ffi) entry-name-type list-entries real-path sun-path-max))

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

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS: files under
;; THEOURGIA_TEST_ROOT, socket paths under THEOURGIA_TEST_SOCK, which is
;; short enough for one. Run alone, without them, a path falls back to /tmp.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define pid-text (number->string (get-process-id)))
(define here (string-append scratch-base "/dil-" pid-text))
(define sock-here (string-append socket-base "/dil-" pid-text))
(define tree (string-append (current-directory) "/.."))
(define store (string-append here "/store"))

(define (sh cmd) (system cmd))
(define (sh-out cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? t) "" t)))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

;; The data a program printed, lists only: a stray word on stderr is not an
;; answer.
(define (datums text)
  (let ((port (open-input-string text)))
    (let loop ((out '()))
      (let ((d (guard (e (#t (eof-object))) (read port))))
        (cond ((eof-object? d) (reverse out))
              ((pair? d) (loop (cons d out)))
              (else (loop out)))))))

(define (text-has? text sub)
  (let ((n (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) sub) #t)
            (else (loop (+ i 1)))))))

(define env-text
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "))

(sh (string-append "rm -rf " here " " sock-here "; mkdir -p " here " " sock-here))
(sh (string-append env-text "THEOURGIA_LOCAL=1 scheme --script " tree "/core.sc init --store " store
                   " --wire > /dev/null 2>&1 < /dev/null"))
(define store-name (real-path store))

;; ---- starting, stopping, reading the log ---------------------------------------
;;
;; EVERY PROCESS A ROW STARTS IS REGISTERED, so that the teardown can stop it
;; whatever the row did or raised, with the instant it started.
;;
;; NEVER: A SIGNAL TO A PID THAT WAS NOT VERIFIED. A pid is reused once its
;; process is gone, so a row that signals a daemon which already exited (one
;; refused at load, say) could reach an unrelated process. signal! sends only
;; when the pid answers kill -0 AND its start instant (ps lstart) is the one
;; recorded here when it was registered; otherwise it sends nothing and the
;; output says the process was already gone. A process gone before it was
;; registered has no recorded instant and is never signalled.
(define started '())
(define identities '())
(define (pid-identity pid)
  (let ((t (sh-out (format "ps -o lstart= -p ~a 2>/dev/null" pid))))
    (and (> (string-length t) 0) t)))
(define (started! pid)
  (when pid
    (set! started (cons pid started))
    (set! identities (cons (cons pid (pid-identity pid)) identities)))
  pid)
(define (signal! pid sig)
  (let ((known (and pid (assv pid identities))))
    (cond
      ((and known (cdr known) (alive? pid) (equal? (cdr known) (pid-identity pid)))
       (sh (format "kill -~a ~a 2>/dev/null" sig pid))
       'sent)
      (else
       (printf "NOTE ~a to ~a not sent: already gone\n" sig pid)
       'already-gone))))

(define (pid-in path)
  (let loop ((i 0))
    (if (and (< i 100) (string=? "" (file-text path))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
  (let ((t (file-text path)))
    (string->number (substring t 0 (max 0 (- (string-length t) 1))))))

;; (launch! tag dir args env) -> pid: `serve` of the store in the background
;; from DIR at the socket `s`, its output in <here>/<tag>.out with `(rc
;; <code>)` appended when it exits. ARGS follow the socket (the idle options);
;; ENV is environment text (a hold, a fault). The pid is the daemon's own.
(define (launch! tag dir args env)
  (let ((out (string-append here "/" tag ".out"))
        (pidf (string-append here "/" tag ".pid")))
    (sh (string-append "rm -f " out " " pidf))
    (sh (string-append "( cd " dir " || exit 1; " env-text env "scheme --script " tree "/theourgiad.sc serve "
                       store " --socket s" args " > " out " 2>&1 < /dev/null & echo $! > " pidf
                       "; wait $!; echo \"(rc $?)\" >> " out " ) > /dev/null 2>&1 &"))
    (started! (pid-in pidf))))

(define (log-of tag) (datums (file-text (string-append here "/" tag ".out"))))
(define (line-of tag head) (find (lambda (d) (eq? (car d) head)) (log-of tag)))
(define (rc-of tag) (let ((rc (line-of tag 'rc))) (and rc (cadr rc))))

;; -> serving, the first error the start wrote, or timeout (fifteen seconds).
(define (await tag)
  (let loop ((i 0))
    (cond
      ((line-of tag 'serving) 'serving)
      ((line-of tag 'error) => (lambda (e) e))
      ((>= i 300) 'timeout)
      (else (sh "sleep 0.05") (loop (+ i 1))))))

(define (alive? pid) (and pid (zero? (system (format "kill -0 ~a 2>/dev/null" pid)))))

;; -> #t once PID is gone, #f if it is still there after SECONDS.
(define (gone-within? pid seconds)
  (let loop ((i 0))
    (cond ((not (alive? pid)) #t)
          ((>= i (* seconds 20)) #f)
          (else (sh "sleep 0.05") (loop (+ i 1))))))

(define (stop! pid) (signal! pid "TERM") (gone-within? pid 15))
(define (kill! pid) (signal! pid "KILL") (gone-within? pid 15))
(define (pause secs) (sh (format "sleep ~a" secs)))

(define (kind p) (guard (e (#t 'unreadable)) (entry-name-type p)))
(define (names dir)
  (let ((l (guard (e (#t 'unreadable)) (list-entries dir))))
    (if (list? l) (list-sort string<? l) l)))

;; -> whether the lock file LOCK can be taken now (nobody holds it).
(define (lock-free? lock)
  (zero? (sh (string-append "python3 -c \"import fcntl,sys; f=open(sys.argv[1]); "
                            "fcntl.flock(f, fcntl.LOCK_EX|fcntl.LOCK_NB)\" " lock " > /dev/null 2>&1"))))

;; The hold seam: (hold-env (stage rel) ...) arms it; (held? rel) waits for the
;; acknowledgement, (release! rel) lets the process go on.
(define (hold-env . pairs)
  (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='"
                 (let join ((ps pairs))
                   (cond ((null? ps) "")
                         ((null? (cdr ps)) (string-append (caar ps) ":" (cadar ps)))
                         (else (string-append (caar ps) ":" (cadar ps) ";" (join (cdr ps))))))
                 "' "))
(define (held? rel)
  (let loop ((i 0))
    (cond ((file-exists? (string-append rel ".held")) #t)
          ((>= i 400) #f)
          (else (sh "sleep 0.05") (loop (+ i 1))))))
(define (release! rel) (sh (string-append "touch " rel)))

(define (fresh-dir name)
  (let ((d (string-append sock-here "/" name)))
    (sh (string-append "rm -rf " d "; mkdir -p " d))
    d))

;; ---- clients, each a process of its own ------------------------------------------
;;
;; A RAW CLIENT, so a row can say when it connected, when it wrote and what came
;; back, apart from the command line's own retries. (client! tag socket frame
;; go) runs it in the background; it writes to <here>/<tag>.cl one datum per
;; event: (connect-failed <errno>), (connected), (sent), (write-failed
;; <errno>), and then (answer <seconds from the write to the answer's newline>) or (read-failed
;; <errno>), the bytes read in <here>/<tag>.cl.ans. FRAME is a request's verb and arguments, or #f for a
;; connection that writes nothing and closes when GO appears. GO, when given,
;; is a file the client waits for before it writes (or closes).
(define client-py (string-append here "/client.py"))
(call-with-output-file client-py
  (lambda (p)
    (for-each (lambda (l) (put-string p l) (newline p))
      '("import socket, sys, time, os"
        "path, frame, out, go = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]"
        "o = open(out, 'w')"
        "def say(x):"
        "    o.write(x + '\\n'); o.flush()"
        "s = socket.socket(socket.AF_UNIX); s.settimeout(40)"
        "try:"
        "    s.connect(path)"
        "except OSError as e:"
        "    say('(connect-failed %d)' % e.errno); sys.exit(0)"
        "say('(connected)')"
        "if go != '-':"
        "    while not os.path.exists(go): time.sleep(0.02)"
        "if frame == '-':"
        "    s.close(); say('(closed)'); sys.exit(0)"
        "t0 = time.time()"
        "try:"
        "    s.sendall(open(frame, 'rb').read())"
        "except OSError as e:"
        "    say('(write-failed %d)' % e.errno); sys.exit(0)"
        "say('(sent)')"
        "got = b''"
        "try:"
        "    while True:"
        "        b = s.recv(65536)"
        "        if not b: break"
        "        got += b"
        "        if b'\\n' in got: break"
        "except OSError as e:"
        "    open(out + '.ans', 'wb').write(got)"
        "    say('(read-failed %d)' % (e.errno or 0)); sys.exit(0)"
        "open(out + '.ans', 'wb').write(got)"
        "say('(answer %.3f)' % (time.time() - t0))")))
  'replace)

(define frame-n 0)
;; -> the path of a file holding one request frame for the store: VERB and its
;; string arguments, from the tester actor.
(define (frame-file verb . args)
  (set! frame-n (+ frame-n 1))
  (let ((f (string-append here "/frame" (number->string frame-n))))
    (call-with-output-file f
      (lambda (p)
        (write `(request 1 ,store-name "tester" #f wire #f #f ,verb ,@args) p)
        (newline p))
      'replace)
    f))

(define (client! tag socket frame go)
  (sh (string-append "rm -f " here "/" tag ".cl " here "/" tag ".cl.ans"))
  (sh (string-append "python3 " client-py " " socket " " (or frame "-") " " here "/" tag ".cl "
                     (or go "-") " > /dev/null 2>&1 &")))
(define (client-log tag) (datums (file-text (string-append here "/" tag ".cl"))))
;; -> the client's first event of kind HEAD, waited for up to SECONDS, or #f.
(define (client-event tag head seconds)
  (let loop ((i 0))
    (cond ((find (lambda (d) (eq? (car d) head)) (client-log tag)) => (lambda (d) d))
          ((>= i (* seconds 20)) #f)
          (else (sh "sleep 0.05") (loop (+ i 1))))))
;; -> how the client's exchange ended: (answer ok|draining|<head>), (read-failed
;; <errno>), (write-failed <errno>), (connect-failed <errno>), or still-open
;; after SECONDS.
(define (client-end tag seconds)
  (let loop ((i 0))
    (let ((end (find (lambda (d) (memq (car d) '(answer read-failed write-failed connect-failed closed)))
                     (client-log tag))))
      (cond
        ((and end (eq? (car end) 'answer))
         (list 'answer (answer-head (file-text (string-append here "/" tag ".cl.ans")))))
        (end (head-of-end end))
        ((>= i (* seconds 20)) 'still-open)
        (else (sh "sleep 0.05") (loop (+ i 1)))))))
(define (head-of-end end) (if (pair? (cdr end)) (list (car end) (cadr end)) (list (car end))))
;; The head of an enveloped answer: ok, draining, (<error kind> <its first
;; clause>) for another refusal, empty, or the text itself.
(define (index-of text sub)
  (let ((n (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) sub) i)
            (else (loop (+ i 1)))))))
(define (answer-head text)
  (cond ((string=? text "") 'empty)
        ((text-has? text "(error draining)") 'draining)
        ((index-of text "(error ")
         => (lambda (i)
              (let ((d (guard (e (#t #f)) (read (open-input-string (substring text i (string-length text)))))))
                (if (and (pair? d) (pair? (cdr d)))
                    (list (cadr d) (and (pair? (cddr d)) (car (cddr d))))
                    text))))
        ((text-has? text "(ok") 'ok)
        (else text)))
(define (answer-seconds tag)
  (let ((a (find (lambda (d) (eq? (car d) 'answer)) (client-log tag))))
    (and a (cadr a))))

;; A plain `outline` through the command line; -> the answer's head.
(define (ask socket)
  (let ((d (datums (sh-out (string-append "cd " here " && " env-text "scheme --script " tree
                                          "/theourgia.sc outline --wire --store " store
                                          " --socket " socket " < /dev/null 2>&1")))))
    (if (pair? d) (car (car d)) 'nothing)))

;; -> whether something accepts a connection at SOCKET right now.
(define (listening? socket)
  (zero? (sh (string-append "python3 -c \"import socket,sys; s=socket.socket(socket.AF_UNIX); s.settimeout(2);"
                            " s.connect(sys.argv[1])\" " socket " > /dev/null 2>&1"))))

;; NOTHING THIS RUN STARTED MAY OUTLIVE IT, and that is counted, not assumed.
(define (left-running)
  (let ((t (sh-out (string-append "pgrep -f '[d]il-" pid-text "/' | wc -l"))))
    (guard (e (#t t)) (read (open-input-string t)))))
(define (sweep!)
  (for-each (lambda (pid) (if (alive? pid) (kill! pid))) started)
  (sh (string-append "pkill -KILL -f '[d]il-" pid-text "/' 2>/dev/null")))
(define (teardown!)
  ;; WHAT IS LEFT IS NAMED, and the logs under the socket directory are kept
  ;; under the scratch directory, before the socket directory is removed.
  (when (> (let ((n (left-running))) (if (number? n) n 1)) 0)
    (sh (string-append "pgrep -fl '[d]il-" pid-text "/' | sed 's/^/LEFT /'")))
  (sh (string-append "mkdir -p " here "/sock-logs; (cd " sock-here " 2>/dev/null && find . -name '*.log' -print | cpio -pdm "
                     here "/sock-logs 2>/dev/null)"))
  (want "teardown: no process this run started is still running" (left-running) 0)
  (sweep!)
  (sh (string-append "chmod -R u+w " sock-here " 2>/dev/null; rm -rf " sock-here)))

;; A ROW THAT RAISES OUTSIDE ITS CHECK ENDS THE RUN, through the teardown.
(base-exception-handler
  (let ((default (base-exception-handler)))
    (lambda (c)
      (if (warning? c)
          (default c)
          (begin
            (set! bad (+ bad 1))
            (printf "FAIL a row raised outside its check: ")
            (display-condition c)
            (newline)
            (teardown!)
            (printf "rows: ~a\n~a failures\n" rows bad)
            (exit 1))))))

;; The idle options every R row uses: a two-second window and the default
;; grace written out.
(define idle-args " --idle-exit 2 --idle-grace-ms 200")
(define (idle-exit-line tag) (line-of tag 'idle-exit))

;; ---- CONTROL: the product loads and serves -------------------------------------------
(let* ((d (fresh-dir "c0"))
       (pid (launch! "c0" d "" ""))
       (said (await "c0"))
       (got (list said (and (eq? said 'serving) (ask (string-append d "/s"))))))
  (stop! pid)
  (want "CONTROL the product loads, and a daemon starts and answers before any scenario" got '(serving ok))
  (unless (equal? got '(serving ok))
    (teardown!)
    (printf "rows: ~a\n~a failures\n" rows bad)
    (exit 1)))

;; ---- CONTROL: signal!'s identity guard -----------------------------------------
;; THE GUARD HAS A ROW OF ITS OWN. A process is registered with its start
;; instant; while the instant recorded for it differs from the one ps reads,
;; signal! sends nothing, says the process was already gone, and the process
;; is still there; under its true instant the signal is sent and the process
;; goes. The process is a perl sleep whose command line names this run's
;; scratch directory, so the teardown's census and sweep reach it, and it
;; ends by itself after thirty seconds. Red on a signal! that checks only
;; kill -0: its first signal would be sent and end the process.
(let* ((pidf (string-append here "/g0.pid"))
       (pid (begin
              (sh (string-append "rm -f " pidf))
              (sh (string-append "perl -e 'sleep 30' " here "/g0.sleep < /dev/null > /dev/null 2>&1 & echo $! > " pidf))
              (started! (pid-in pidf))))
       (true-id (let ((k (assv pid identities))) (and k (cdr k)))))
  (set! identities (cons (cons pid (string-append (or true-id "") " (altered)")) identities))
  (let* ((wrong (signal! pid "TERM"))
         (still (alive? pid)))
    (set! identities (cdr identities))
    (let* ((right (signal! pid "TERM"))
           (gone (gone-within? pid 15)))
      (want "CONTROL signal! refuses a pid whose recorded start differs, then signals it under its true one"
            (list (string? true-id) wrong still right gone)
            '(#t already-gone #t sent #t)))))

;; ---- W3b: the grace's cap ----------------------------------------------------------
;;
;; A DEFERRED REQUEST WAITS FOR THE GRACE, so the grace is bounded where it is
;; read: above 5000 ms the start is refused before the lock.
(let ((d (fresh-dir "b")))
  (launch! "b1" d " --idle-exit 2 --idle-grace-ms 5001" "")
  (let loop ((i 0)) (if (and (< i 300) (not (rc-of "b1"))) (begin (pause 0.05) (loop (+ i 1)))))
  (want "W3b --idle-grace-ms 5001: refused too-large with its max, exit 1, before the lock"
        (list (line-of "b1" 'error) (rc-of "b1") (kind (string-append d "/.s.lock")))
        '((error bad-request (reason too-large) (option "--idle-grace-ms") (value "5001") (max 5000)) 1 absent))
  (let* ((pid (launch! "b2" d " --idle-exit 2 --idle-grace-ms 5000" ""))
         (said (await "b2")))
    (want "W3b --idle-grace-ms 5000: accepted" said 'serving)
    (stop! pid)))

;; ---- W4, W3b: no idle exit unless asked for ------------------------------------------
(let* ((d (fresh-dir "w4"))
       (pid (launch! "w4" d "" ""))
       (said (await "w4")))
  (pause 3.5)
  (want "W4 serve by hand, without --idle-exit: alive after the window, and answering"
        (list said (alive? pid) (ask (string-append d "/s")))
        '(serving #t ok))
  (stop! pid))

(let* ((d (fresh-dir "w0"))
       (pid (launch! "w0" d " --idle-exit 0" ""))
       (said (await "w0")))
  (pause 3.5)
  (want "W3b --idle-exit 0 is never: alive after the window"
        (list said (alive? pid) (ask (string-append d "/s")))
        '(serving #t ok))
  (stop! pid))

;; ---- W5: the window -----------------------------------------------------------------
;;
;; WHAT AN IDLE EXIT LEAVES: no public link, no private socket, the lock free,
;; and the marked namespace (the reservation stays until the next start).
(let* ((d (fresh-dir "w5"))
       (pid (launch! "w5" d idle-args ""))
       (said (await "w5"))
       (pt (number->string pid)))
  (want "W5 nobody connects: the daemon exits within the window and a few seconds"
        (list said (gone-within? pid 8))
        '(serving #t))
  (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5"))) (begin (pause 0.05) (loop (+ i 1)))))
  (want "W5 the exit is 0, logged with the window and both runtime counters at zero"
        (list (rc-of "w5") (idle-exit-line "w5"))
        '(0 (idle-exit (after 2) (refused-accepts 0) (callback-raised 0))))
  (want "W5 after it: no link, no private socket, the lock free, the marker kept"
        (list (kind (string-append d "/s")) (names (string-append d "/s.d." pt))
              (lock-free? (string-append d "/.s.lock")))
        '(absent (".endpoint") #t)))

(let* ((d (fresh-dir "w5k"))
       (pid (launch! "w5k" d idle-args ""))
       (said (await "w5k")))
  (let loop ((i 0))
    (when (< i 5) (ask (string-append d "/s")) (pause 1) (loop (+ i 1))))
  (want "W5 a connection every second inside the window keeps the daemon"
        (list said (alive? pid))
        '(serving #t))
  (want "W5 within one window after the connections stop it has exited"
        (gone-within? pid 6)
        #t))

(let* ((d (fresh-dir "w5s"))
       (pid (launch! "w5s" d idle-args ""))
       (said (await "w5s"))
       (sub-out (string-append here "/w5s-sub.out"))
       (_ (sh (string-append "( cd " here " && " env-text "exec scheme --script " tree
                             "/theourgia.sc subscribe changes 0 --store " store " --socket " d "/s"
                             " > " sub-out " 2>&1 < /dev/null & echo $! > " here "/w5s-sub.pid )")))
       (sub (started! (pid-in (string-append here "/w5s-sub.pid")))))
  (pause 4)
  (want "W5 an open subscription keeps the daemon past the window"
        (list said (alive? sub) (alive? pid))
        '(serving #t #t))
  (kill! sub)
  (want "W5 within one window after the subscription closes it has exited"
        (gone-within? pid 6)
        #t))

;; ---- W5j: a request running, a connection open ----------------------------------------
;;
;; conn-hold-long parks the first connection-local read for nine seconds.
(let* ((d (fresh-dir "w5j"))
       (pid (launch! "w5j" d idle-args "THEOURGIA_INJECT=on THEOURGIA_FAULT=conn-hold-long@conn "))
       (said (await "w5j")))
  (client! "j1" (string-append d "/s") (frame-file 'outline) #f)
  (pause 4)
  (want "W5j a request held running beyond the window: no unlink and no exit while it runs"
        (list said (alive? pid) (kind (string-append d "/s")))
        '(serving #t link))
  (want "W5j the held request is answered"
        (client-end "j1" 15)
        '(answer ok))
  (pause 1)
  (want "W5j a second after the answer the daemon is still there (a full window after the completion)"
        (alive? pid)
        #t)
  (want "W5j and it exits within the window after that"
        (gone-within? pid 6)
        #t))

(let* ((d (fresh-dir "w5o"))
       (go (string-append here "/w5o.go"))
       (pid (launch! "w5o" d idle-args ""))
       (said (await "w5o")))
  (client! "j2" (string-append d "/s") #f go)
  (pause 4)
  (want "W5j a connection held open without a request keeps the daemon past the window"
        (list said (client-event "j2" 'connected 2) (alive? pid))
        '(serving (connected) #t))
  (release! go)
  (pause 1)
  (want "W5j a second after it closes the daemon is still there"
        (alive? pid)
        #t)
  (want "W5j and it exits within the window after that"
        (gone-within? pid 6)
        #t))

;; ---- W5k: the unlink fails -------------------------------------------------------------
;;
;; The socket's directory is made unwritable while the listener is held before
;; its unlink: the attempt is cancelled and the daemon keeps serving; the
;; directory restored, the next window exits.
(let* ((d (fresh-dir "w5u"))
       (rel (string-append here "/w5u.rel"))
       (pid (launch! "w5u" d idle-args (hold-env (list "leave-before-unlink" rel))))
       (said (await "w5u")))
  (want "W5k the clock fires and the listener is held before its unlink"
        (list said (held? rel))
        '(serving #t))
  (sh (string-append "chmod 555 " d))
  (release! rel)
  (pause 1)
  (want "W5k the unlink refused: the attempt cancelled, logged with the errno, the link kept, the daemon answers"
        (list (line-of "w5u" 'idle-cancelled) (kind (string-append d "/s")) (alive? pid)
              (ask (string-append d "/s")))
        '((idle-cancelled (reason unlink-failed 13)) link #t ok))
  (sh (string-append "chmod 755 " d))
  (want "W5k the directory restored, a later window exits normally"
        (list (gone-within? pid 8) (and (idle-exit-line "w5u") #t))
        '(#t #t)))

;; ---- W5b: the boundary at the path ----------------------------------------------------
;;
;; Held before the unlink, a client connects and writes; held after it, a
;; second client finds nothing there. The first is registered at once, which
;; cancels the exit, and admitted only after the resume: an ordinary answer,
;; within the grace and a margin of the listener's release.
(let* ((d (fresh-dir "w5b"))
       (r1 (string-append here "/w5b.r1"))
       (r2 (string-append here "/w5b.r2"))
       (pid (launch! "w5b" d idle-args (hold-env (list "leave-before-unlink" r1) (list "leave-after-unlink" r2))))
       (said (await "w5b")))
  (want "W5b the clock fires and the listener is held before its unlink"
        (list said (held? r1))
        '(serving #t))
  (client! "b1" (string-append d "/s") (frame-file 'outline) #f)
  (want "W5b a client connects and writes before the unlink"
        (list (client-event "b1" 'connected 5) (client-event "b1" 'sent 5))
        '((connected) (sent)))
  (pause 0.5)
  (release! r1)
  (want "W5b the listener unlinked the link and is held after it"
        (list (held? r2) (kind (string-append d "/s")))
        '(#t absent))
  (client! "b2" (string-append d "/s") (frame-file 'outline) #f)
  (want "W5b a client connecting after the unlink finds nothing there, before writing a byte"
        (client-end "b2" 5)
        '(connect-failed 2))
  (pause 0.2)
  (release! r2)
  (pause 1.5)
  (want "W5b the late connection alone cancelled the exit: same daemon, link back, resumed, no idle-exit"
        (list (alive? pid) (kind (string-append d "/s"))
              (and (line-of "w5b" 'idle-resumed) #t) (idle-exit-line "w5b"))
        '(#t link #t #f))
  (want "W5b the client that connected before the unlink gets an ordinary answer, never draining"
        (client-end "b1" 10)
        '(answer ok))
  (want "W5b its answer arrived within the grace and two seconds of the listener's release"
        (let ((s (answer-seconds "b1"))) (and s (< s 4.5)))
        #t)
  (want "W5b the daemon still answers afterwards"
        (ask (string-append d "/s"))
        'ok)
  (stop! pid))

;; ---- W5e: the boundary at the adapter ------------------------------------------------
;;
;; An accepted adapter held before its report is not a registered connection,
;; so the clock fires; the listener's acknowledgement waits for it; released,
;; it reports, and the exit is cancelled.
(let* ((d (fresh-dir "w5e"))
       (rel (string-append here "/w5e.rel"))
       (pid (launch! "w5e" d idle-args (hold-env (list "adapter-before-report" rel))))
       (said (await "w5e")))
  (client! "e1" (string-append d "/s") (frame-file 'outline) #f)
  (want "W5e a client connects and its adapter is held before its report"
        (list said (client-event "e1" 'connected 5) (held? rel))
        '(serving (connected) #t))
  (pause 3.5)
  (want "W5e the clock fired meanwhile: the link is unlinked and the daemon waits"
        (list (kind (string-append d "/s")) (alive? pid) (idle-exit-line "w5e"))
        '(absent #t #f))
  (release! rel)
  (want "W5e released, the adapter reports, the exit is cancelled and the request answered"
        (client-end "e1" 10)
        '(answer ok))
  (pause 0.5)
  (want "W5e the daemon is still there with its link back"
        (list (alive? pid) (kind (string-append d "/s")) (and (line-of "w5e" 'idle-resumed) #t))
        '(#t link #t))
  (stop! pid))

;; ---- W5f: the late connection is a subscription ----------------------------------------
(let* ((d (fresh-dir "w5f"))
       (rel (string-append here "/w5f.rel"))
       (pid (launch! "w5f" d idle-args (hold-env (list "leave-before-unlink" rel))))
       (said (await "w5f"))
       (sub-out (string-append here "/w5f-sub.out")))
  (want "W5f the clock fires and the listener is held before its unlink"
        (list said (held? rel))
        '(serving #t))
  (sh (string-append "( cd " here " && " env-text "exec scheme --script " tree
                     "/theourgia.sc subscribe changes 0 --store " store " --socket " d "/s"
                     " > " sub-out " 2>&1 < /dev/null & echo $! > " here "/w5f-sub.pid )"))
  (let ((sub (started! (pid-in (string-append here "/w5f-sub.pid")))))
    (pause 0.5)
    (release! rel)
    (pause 3.5)
    (want "W5f the subscription cancelled the exit and holds the clock past the window"
          (list (alive? pid) (alive? sub) (idle-exit-line "w5f"))
          '(#t #t #f))
    (want "W5f a request during it is answered by the same daemon"
          (list (ask (string-append d "/s")) (alive? pid))
          '(ok #t))
    (kill! sub)
    (stop! pid)))

;; ---- W5n: the resume fails on a replacement --------------------------------------------
;;
;; A plain file put at the public path after the unlink: the late connection
;; cancels the exit, the resume finds the path occupied, the late request is
;; answered draining, and the daemon exits 75 leaving the replacement.
(let* ((d (fresh-dir "w5n"))
       (r1 (string-append here "/w5n.r1"))
       (r2 (string-append here "/w5n.r2"))
       (pid (launch! "w5n" d idle-args (hold-env (list "leave-before-unlink" r1) (list "leave-after-unlink" r2))))
       (said (await "w5n"))
       (pt (number->string pid)))
  (want "W5n the listener is held before its unlink"
        (list said (held? r1))
        '(serving #t))
  (client! "n1" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "n1" 'sent 5)
  (pause 0.5)
  (release! r1)
  (held? r2)
  (sh (string-append "echo replacement > " d "/s"))
  (release! r2)
  (want "W5n the late request is answered draining, not dropped"
        (client-end "n1" 10)
        '(answer draining))
  (want "W5n the daemon exits 75, logged resume-failed path-occupied"
        (list (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5n"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5n"))
              (idle-exit-line "w5n"))
        '(#t 75 (idle-exit (reason resume-failed path-occupied))))
  (want "W5n the replacement is untouched, the lock free, no socket left in the namespace"
        (list (file-text (string-append d "/s")) (lock-free? (string-append d "/.s.lock"))
              (names (string-append d "/s.d." pt)))
        '("replacement\n" #t (".endpoint"))))

;; ---- W5l5: a replacement made during the grace survives the stop ------------------------
(let* ((d (fresh-dir "w5r"))
       (rel (string-append here "/w5r.rel"))
       (pid (launch! "w5r" d idle-args (hold-env (list "leave-after-unlink" rel))))
       (said (await "w5r")))
  (want "W5l5 the listener unlinked its link and is held in the grace"
        (list said (held? rel) (kind (string-append d "/s")))
        '(serving #t absent))
  (sh (string-append "echo replacement > " d "/s"))
  (release! rel)
  (want "W5l5 the daemon exits by the idle exit (no late connection)"
        (list (gone-within? pid 8) (and (idle-exit-line "w5r") #t))
        '(#t #t))
  (want "W5l5 the replacement is still there after the stop and the exit, the lock free"
        (list (file-text (string-append d "/s")) (lock-free? (string-append d "/.s.lock")))
        '("replacement\n" #t)))

;; ---- W5o's resume half: a relative socket ------------------------------------------------
(let* ((d (fresh-dir "w5v"))
       (_ (sh (string-append "mkdir -p " d "/run")))
       (r1 (string-append here "/w5v.r1"))
       (out "w5v")
       (pidf (string-append here "/w5v.pid")))
  (sh (string-append "rm -f " here "/w5v.out " pidf))
  (sh (string-append "( cd " d " || exit 1; " env-text (hold-env (list "leave-before-unlink" r1))
                     "scheme --script " tree "/theourgiad.sc serve " store " --socket run/s" idle-args
                     " > " here "/" out ".out 2>&1 < /dev/null & echo $! > " pidf
                     "; wait $!; echo \"(rc $?)\" >> " here "/" out ".out ) > /dev/null 2>&1 &"))
  (let* ((pid (started! (pid-in pidf)))
         (said (await out))
         (pt (number->string pid)))
    (held? r1)
    (client! "v1" (string-append d "/run/s") (frame-file 'outline) #f)
    (client-event "v1" 'sent 5)
    (pause 0.5)
    (release! r1)
    (want "W5o a cancellation's resume republishes the same relative target, and the late request is served"
          (list said (client-end "v1" 10)
                (let ((t (sh-out (string-append "readlink " d "/run/s"))))
                  (and (> (string-length t) 0) (substring t 0 (- (string-length t) 1)))))
          (list 'serving '(answer ok) (string-append "s.d/" pt)))
    (stop! pid)
    (want "W5o the controlled exit after the resume removes the link and the private socket"
          (list (kind (string-append d "/run/s")) (names (string-append d "/run/s.d." pt)))
          '(absent (".endpoint")))))

;; ---- W4b: the drain awaits the listener's acknowledgement --------------------------------
;;
;; No idle exit: an accepted adapter held before its report, then SIGTERM. The
;; connection is admitted draining and answered `draining`; the daemon exits 0
;; after the listener has accounted for it.
(let* ((d (fresh-dir "w4b"))
       (rel (string-append here "/w4b.rel"))
       (pid (launch! "w4b" d "" (hold-env (list "adapter-before-report" rel))))
       (said (await "w4b")))
  (client! "a1" (string-append d "/s") (frame-file 'outline) #f)
  (want "W4b a client connects and writes, its adapter held before its report"
        (list said (client-event "a1" 'sent 5) (held? rel))
        '(serving (sent) #t))
  (signal! pid "TERM")
  (pause 1)
  (want "W4b SIGTERM: the daemon waits for the listener's account of the held adapter"
        (alive? pid)
        #t)
  (release! rel)
  (want "W4b the connection is admitted draining and its request answered draining"
        (client-end "a1" 10)
        '(answer draining))
  (want "W4b the daemon exits 0 afterwards"
        (list (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w4b"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w4b")))
        '(#t 0)))

;; ---- W5h (ii), (iii): the handshake in a plain drain --------------------------------------
;;
;; Moved out of the leave (inside one no connection is admitted before the
;; attempt is decided). A connection admitted before the drain with its frame
;; buffered: a connection-local read is served, a write refused draining by
;; may-execute; admitted before the drain and awaiting data: closed without an
;; envelope, as today.
(let* ((d (fresh-dir "w5h"))
       (pid (launch! "w5h" d "" "THEOURGIA_INJECT=on THEOURGIA_FAULT=conn-hold@conn "))
       (said (await "w5h"))
       (go (string-append here "/w5h.go")))
  (client! "h1" (string-append d "/s") (frame-file 'outline) #f)
  (client! "h3" (string-append d "/s") (frame-file 'outline) go)
  (want "W5h (ii) a read admitted and buffered, held running; (iii) a connection awaiting data"
        (list said (client-event "h1" 'sent 5) (client-event "h3" 'connected 5))
        '(serving (sent) (connected)))
  (pause 0.5)
  (signal! pid "TERM")
  (pause 0.5)
  (release! go)
  (want "W5h (ii) the buffered connection-local read admitted before the drain is served"
        (client-end "h1" 15)
        '(answer ok))
  ;; A peer that writes into a connection the daemon has closed sees either an
  ;; empty read or a failed write, by the kernel's timing; both are "no envelope".
  (want "W5h (iii) the connection admitted before the drain and awaiting data is closed without an envelope"
        (let ((e (client-end "h3" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(write-failed read-failed))))
              'no-envelope
              e))
        'no-envelope)
  (gone-within? pid 15))

;; ---- W5l: an exit while resuming -------------------------------------------------------
;;
;; A late connection cancels the exit; the listener rebinds and publishes, and
;; its report to main is held. A SIGTERM then: the daemon exits only after
;; recording the new identity, so the rebound link and socket are gone after.
(define (cancel-and-hold-report! tag d env-extra)
  (let* ((r1 (string-append here "/" tag ".r1"))
         (r2 (string-append here "/" tag ".r2"))
         (pid (launch! tag d idle-args
                       (string-append (hold-env (list "leave-before-unlink" r1) (list "resume-before-bound" r2))
                                      env-extra)))
         (said (await tag)))
    (held? r1)
    (client! (string-append tag "c") (string-append d "/s") (frame-file 'outline) #f)
    (client-event (string-append tag "c") 'sent 5)
    (pause 0.5)
    (release! r1)
    (list pid said (held? r2) r2)))

(let* ((d (fresh-dir "w5l"))
       (h (cancel-and-hold-report! "w5l" d ""))
       (pid (car h)) (pt (number->string pid)))
  (want "W5l the resume rebound and published; its report to main is held"
        (list (cadr h) (caddr h) (kind (string-append d "/s")))
        '(serving #t link))
  (signal! pid "TERM")
  (pause 1)
  (want "W5l SIGTERM while resuming: the daemon waits for the report"
        (alive? pid)
        #t)
  (release! (cadddr h))
  (want "W5l after the report it exits; the rebound link and socket are gone and the lock free"
        (list (gone-within? pid 10) (kind (string-append d "/s")) (names (string-append d "/s.d." pt))
              (lock-free? (string-append d "/.s.lock")))
        '(#t absent (".endpoint") #t)))

;; W5l2: forced exits with the report still held. The drain's budget runs out:
;; exit 75, and the rebound link and the private file recorded in the cell are
;; gone (leave unlinks them, not the listener's close).
(let* ((d (fresh-dir "w5m"))
       (h (cancel-and-hold-report! "w5m" d "THEOURGIA_HOLD_MS=20000 "))
       (pid (car h)) (pt (number->string pid)))
  (signal! pid "TERM")
  (want "W5l2 the drain's budget runs out with the report held: exit 75"
        (list (gone-within? pid 12)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5m"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5m")))
        '(#t 75))
  (want "W5l2 the rebound link and the recorded private file are gone, the lock free"
        (list (kind (string-append d "/s")) (names (string-append d "/s.d." pt))
              (lock-free? (string-append d "/.s.lock")))
        '(absent (".endpoint") #t))
  (release! (cadddr h)))

(let* ((d (fresh-dir "w5q"))
       (h (cancel-and-hold-report! "w5q" d ""))
       (pid (car h)) (pt (number->string pid)))
  (signal! pid "TERM")
  (pause 0.3)
  (signal! pid "TERM")
  (want "W5l2 a second SIGTERM with the report held: exit 75 at once"
        (list (gone-within? pid 5)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5q"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5q")))
        '(#t 75))
  (want "W5l2 and the rebound link and private file are gone, the lock free"
        (list (kind (string-append d "/s")) (names (string-append d "/s.d." pt))
              (lock-free? (string-append d "/.s.lock")))
        '(absent (".endpoint") #t))
  (release! (cadddr h)))

;; W5l4: during the same window another process replaces the path with a plain
;; file, then the forced exit: the replacement is untouched.
(let* ((d (fresh-dir "w5w"))
       (h (cancel-and-hold-report! "w5w" d ""))
       (pid (car h)))
  (sh (string-append "rm -f " d "/s; echo replacement > " d "/s"))
  (signal! pid "TERM")
  (pause 0.3)
  (signal! pid "TERM")
  (gone-within? pid 5)
  (want "W5l4 a replacement made while resuming is untouched by the forced exit, the lock free"
        (list (file-text (string-append d "/s")) (lock-free? (string-append d "/.s.lock")))
        '("replacement\n" #t))
  (release! (cadddr h)))

;; W5l2's second half: the listener held on its stop, a second signal before it
;; has processed it: exit 75 at once, link and private file gone.
(let* ((d (fresh-dir "w5x"))
       (rel (string-append here "/w5x.rel"))
       (pid (launch! "w5x" d "" (hold-env (list "listener-before-stop" rel))))
       (said (await "w5x"))
       (pt (number->string pid)))
  (signal! pid "TERM")
  (held? rel)
  (signal! pid "TERM")
  (want "W5l2 a second signal while the listener is held on its stop: exit 75, link and private file gone"
        (list said (gone-within? pid 5)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5x"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5x"))
              (kind (string-append d "/s")) (names (string-append d "/s.d." pt)))
        '(serving #t 75 absent (".endpoint")))
  (release! rel))

;; ---- W5t: resume failures by cause ----------------------------------------------------------
;;
;; A late connection cancels the exit while the resume cannot happen: (a) the
;; namespace's backing directory made unwritable (the bind fails), (b) the
;; link's directory made unwritable (the link cannot be published). The bind's
;; failure is the runtime's, which carries libuv's text and no errno. Each time
;; the late request is answered draining and the daemon exits 75 logged with
;; the cause; no socket of the failed resume is left.
(define (resume-fails tag break! mend!)
  (let* ((d (fresh-dir tag))
         (r1 (string-append here "/" tag ".r1"))
         (r2 (string-append here "/" tag ".r2"))
         (pid (launch! tag d idle-args (hold-env (list "leave-before-unlink" r1) (list "leave-after-unlink" r2))))
         (said (await tag))
         (pt (number->string pid)))
    (held? r1)
    (client! (string-append tag "c") (string-append d "/s") (frame-file 'outline) #f)
    (client-event (string-append tag "c") 'sent 5)
    (pause 0.5)
    (release! r1)
    (held? r2)
    (break! d pt)
    (release! r2)
    (let* ((end (client-end (string-append tag "c") 10))
           (gone (gone-within? pid 10)))
      (let loop ((i 0)) (if (and (< i 100) (not (rc-of tag))) (begin (pause 0.05) (loop (+ i 1)))))
      (mend! d pt)
      (list said end gone (rc-of tag) (idle-exit-line tag)
            (names (string-append d "/s.d." pt)) (lock-free? (string-append d "/.s.lock"))))))

;; THE DIRECTORY IS MADE UNWRITABLE AFTER THE UNLINK, so the listener's close
;; cannot remove the private socket either: the resume's bind meets that file
;; (EADDRINUSE), and that one socket, the first bind's, stays beside the
;; marker until the next start; the failed resume left nothing of its own.
(want "W5t (a) the backing directory unwritable: draining, exit 75, resume-failed bind, only the first socket left"
      (let ((got (resume-fails "w5ta"
                               (lambda (d pt) (sh (string-append "chmod 555 " d "/s.d." pt)))
                               (lambda (d pt) (sh (string-append "chmod 755 " d "/s.d." pt))))))
        (list (list-head got 5)
              (let ((n (list-ref got 5)))
                (if (list? n) (map (lambda (x) (if (string->number x) 'a-socket x)) n) n))
              (list-ref got 6)))
      (list '(serving (answer draining) #t 75 (idle-exit (reason resume-failed (bind "address already in use"))))
            '(".endpoint" a-socket) #t))
(want "W5t (b) the link's directory unwritable: draining, exit 75, resume-failed link, nothing left"
      (resume-fails "w5tb"
                    (lambda (d pt) (sh (string-append "chmod 555 " d)))
                    (lambda (d pt) (sh (string-append "chmod 755 " d))))
      '(serving (answer draining) #t 75 (idle-exit (reason resume-failed (link 13))) (".endpoint") #t))

;; ---- W5e2: an adapter that dies before its report ------------------------------------------
;;
;; One earlier served connection (the watermark is 1); then, during the leave,
;; a late connection whose adapter dies before its report (adapter-dies fires
;; from the second adapter on): it adds no watch, the acknowledgement's handed
;; equals the watermark, and the daemon exits; the peer that wrote gets no
;; answer.
(let* ((d (fresh-dir "w5d"))
       (rel (string-append here "/w5d.rel"))
       (pid (launch! "w5d" d idle-args
                     (string-append (hold-env (list "leave-before-unlink" rel))
                                    "THEOURGIA_FAULT=adapter-dies@conn ")))
       (said (await "w5d")))
  (want "W5e2 the first connection is served (the watermark becomes 1)"
        (list said (ask (string-append d "/s")))
        '(serving ok))
  (held? rel)
  (client! "d1" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "d1" 'sent 5)
  (pause 0.5)
  (release! rel)
  (want "W5e2 the late adapter dies before its report: its peer, which wrote, gets no answer"
        (let ((e (client-end "d1" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer)
  (want "W5e2 the daemon exits by the idle exit: the dead adapter added no watch"
        (list (gone-within? pid 8) (idle-exit-line "w5d") (line-of "w5d" 'idle-resumed))
        '(#t (idle-exit (after 2) (refused-accepts 0) (callback-raised 0)) #f)))

;; ---- W5h (i): a signal during a leave, each refusal its own --------------------------------------
;;
;; Late connections during the leave, then SIGTERM while the listener is held
;; after its unlink: the daemon drains and exits 0 and never resumes. A valid
;; request is answered draining; a malformed frame, bad arguments and a request
;; for another store each get their own refusal; an oversized frame the limit.
(let* ((d (fresh-dir "w5i"))
       (r1 (string-append here "/w5i.r1"))
       (r2 (string-append here "/w5i.r2"))
       (pid (launch! "w5i" d idle-args (hold-env (list "leave-before-unlink" r1) (list "leave-after-unlink" r2))))
       (said (await "w5i"))
       (big (string-append here "/big-frame")))
  (call-with-output-file big
    (lambda (p) (put-string p "(request 1 \"") (put-string p (make-string (* 1100 1024) #\a)) (put-string p "\")\n"))
    'replace)
  (let ((bad-frame (string-append here "/bad-frame")))
    (call-with-output-file bad-frame (lambda (p) (put-string p "(request 1 \"x\"\n")) 'replace)
    (held? r1)
    (client! "i1" (string-append d "/s") (frame-file 'outline) #f)
    (client! "i2" (string-append d "/s") bad-frame #f)
    ;; ARGUMENTS THE PARSER REFUSES, as daemon-e1's D-32 and envelope's EV-16:
    ;; `drafts` does not take --writer twice. An unknown option is not that:
    ;; parse-arguments reads it as a positional, and the verb refuses it later.
    (client! "i3" (string-append d "/s") (frame-file 'drafts "--writer" "w1" "--writer" "w2") #f)
    (client! "i5" (string-append d "/s") big #f)
    (let ((other (string-append here "/other-frame")))
      (call-with-output-file other
        (lambda (p) (write `(request 1 ,(string-append here "/elsewhere") "tester" #f wire #f #f outline) p) (newline p))
        'replace)
      (client! "i4" (string-append d "/s") other #f))
    (pause 0.8)
    (release! r1)
    (held? r2)
    (signal! pid "TERM")
    (pause 0.5)
    (release! r2)
    (want "W5h (i) admitted while draining: a valid request is answered draining"
          (list said (client-end "i1" 10))
          '(serving (answer draining)))
    (want "W5h (i) a malformed frame gets its own refusal, not draining"
          (client-end "i2" 10)
          '(answer (bad-request (reason not-a-datum))))
    (want "W5h (i) a request with bad arguments gets its own refusal"
          (let ((e (client-end "i3" 10))) (if (and (pair? e) (pair? (cdr e)) (pair? (cadr e))) (list 'answer (car (cadr e))) e))
          '(answer bad-request))
    (want "W5h (i) a request for another store gets the store mismatch"
          (let ((e (client-end "i4" 10))) (if (and (pair? e) (pair? (cdr e)) (pair? (cadr e))) (list 'answer (car (cadr e))) e))
          '(answer transport-store-mismatch))
    ;; The client may still be writing when the refusal closes the connection:
    ;; the refusal read, or the write cut off (EPIPE), are the same outcome.
    (want "W5h (i) an oversized frame is refused by the size limit"
          (let ((e (client-end "i5" 10)))
            (if (or (equal? e '(answer (bad-request (reason frame-limit)))) (equal? e '(write-failed 32)))
                'refused-for-size e))
          'refused-for-size)
    (want "W5h the daemon drains and exits 0, never resuming: no link reappears"
          (list (gone-within? pid 10)
                (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5i"))) (begin (pause 0.05) (loop (+ i 1)))))
                       (rc-of "w5i"))
                (kind (string-append d "/s")) (and (line-of "w5i" 'idle-resumed) #t))
          '(#t 0 absent #f))))

;; ---- W5h2: forced exits during a leave ------------------------------------------------------------
;;
;; An adapter held past the drain's budget: the daemon exits 75 without the
;; acknowledgement; the held peer that completed its send gets a transport
;; error or nothing, never an answer.
(let* ((d (fresh-dir "w5j2"))
       (rel (string-append here "/w5j2.rel"))
       (pid (launch! "w5j2" d idle-args
                     (string-append (hold-env (list "adapter-before-report" rel)) "THEOURGIA_HOLD_MS=20000 ")))
       (said (await "w5j2")))
  (client! "k1" (string-append d "/s") (frame-file 'outline) #f)
  (held? rel)
  (pause 3)
  (signal! pid "TERM")
  (want "W5h2 an adapter held past the drain's budget: exit 75 without the acknowledgement"
        (list said (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5j2"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5j2")))
        '(serving #t 75))
  (want "W5h2 the held peer that completed its send gets no answer"
        (let ((e (client-end "k1" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer)
  (release! rel))

;; ---- W5h3, W5g, W5g2: connections libuv has not delivered -----------------------------------------
;;
;; The blocking holds stop the whole VM, so a connection the kernel completes
;; while they hold is not delivered until the listener next yields.
(define (blocked-leave! tag d grace)
  (let* ((r1 (string-append here "/" tag ".r1"))
         (r2 (string-append here "/" tag ".r2"))
         (pid (launch! tag d (string-append " --idle-exit 2 --idle-grace-ms " grace)
                       (hold-env (list "leave-block-before-unlink" r1) (list "leave-block-after-unlink" r2))))
         (said (await tag)))
    (list pid said (held? r1) r1 r2)))

;; W5h3: a SIGTERM between the unlink and the end of a long grace, with a
;; connection completed before the unlink and not yet delivered: delivered in
;; the grace, admitted draining, answered draining; the daemon exits 0.
(let* ((d (fresh-dir "w5g3"))
       (b (blocked-leave! "w5g3" d "1500"))
       (pid (car b)))
  (client! "g3" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "g3" 'sent 5)
  (release! (cadddr b))
  (held? (list-ref b 4))
  (signal! pid "TERM")
  (release! (list-ref b 4))
  (want "W5h3 a connection completed before the unlink, delivered in the grace after a SIGTERM: answered draining"
        (list (cadr b) (caddr b) (client-end "g3" 10))
        '(serving #t (answer draining)))
  (want "W5h3 the daemon exits 0 by the drain's rules"
        (list (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5g3"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5g3")))
        '(#t 0)))

;; W5h3b: a stop already queued when an undelivered connection waits in the
;; backlog. The SIGTERM is sent while the listener is held at the non-blocking
;; leave-after-unlink, so main runs and queues (stop) in its mailbox; then the
;; VM is blocked at leave-block-after-unlink and a connection completes. The
;; public link is already unlinked there, so the client connects by the
;; private name, the daemon's own socket. The grace's receive finds the (stop)
;; before the accept; a stop asked in the grace waits for the grace's end, so
;; the connection is delivered, admitted draining and answered draining.
(let* ((d (fresh-dir "w5g4"))
       (r1 (string-append here "/w5g4.r1"))
       (r2 (string-append here "/w5g4.r2"))
       (pid (launch! "w5g4" d " --idle-exit 2 --idle-grace-ms 1500"
                     (hold-env (list "leave-after-unlink" r1) (list "leave-block-after-unlink" r2))))
       (said (await "w5g4"))
       (pt (number->string pid))
       (private (string-append d "/s.d." pt "/" pt))
       (held1 (held? r1)))
  (signal! pid "TERM")
  (pause 0.5)
  (release! r1)
  (let ((held2 (held? r2)))
    (client! "g4" private (frame-file 'outline) #f)
    (client-event "g4" 'sent 5)
    (release! r2)
    (want "W5h3b a stop queued before a connection completed in the blocked grace: the connection is delivered and answered draining"
          (list said held1 held2 (client-end "g4" 10))
          '(serving #t #t (answer draining)))
    (want "W5h3b the daemon exits 0 by the drain's rules"
          (list (gone-within? pid 10)
                (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5g4"))) (begin (pause 0.05) (loop (+ i 1)))))
                       (rc-of "w5g4")))
          '(#t 0))))

;; W5g: the grace (50 ms) passes with the connection undelivered: it is reset
;; at the stop, its peer, which wrote its frame, gets a transport error; the
;; daemon exits by the idle exit.
(let* ((d (fresh-dir "w5g"))
       (b (blocked-leave! "w5g" d "50"))
       (pid (car b)))
  (client! "g1" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "g1" 'sent 5)
  (release! (cadddr b))
  (held? (list-ref b 4))
  (pause 0.3)
  (release! (list-ref b 4))
  (want "W5g a connection undelivered at the grace's end is reset: its peer gets no answer"
        (let ((e (client-end "g1" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer)
  (want "W5g the daemon exits by the idle exit"
        (list (cadr b) (gone-within? pid 8) (and (idle-exit-line "w5g") #t))
        '(serving #t #t)))

;; W5g2: the same reset arranged before the peer's first write: its write or
;; its read fails; it is never answered.
(let* ((d (fresh-dir "w5g2"))
       (b (blocked-leave! "w5g2" d "50"))
       (pid (car b))
       (go (string-append here "/w5g2.go")))
  (client! "g2" (string-append d "/s") (frame-file 'outline) go)
  (client-event "g2" 'connected 5)
  (release! (cadddr b))
  (held? (list-ref b 4))
  (pause 0.3)
  (release! (list-ref b 4))
  (gone-within? pid 8)
  (release! go)
  (want "W5g2 the peer whose first write follows the reset is never answered"
        (let ((e (client-end "g2" 10)))
          (cond ((and (pair? e) (memq (car e) '(write-failed read-failed))) 'failed)
                ((equal? e '(answer empty)) 'failed)
                (else e)))
        'failed))

;; ---- W5l3: a peer on the rebound socket, held before its report ----------------------------
;;
;; The late connection cancels the exit and is served; a peer then connects to
;; the rebound socket and its adapter is held at the resumed listener's own
;; stage. SIGTERM: the peer's request is answered draining after the
;; listener's stop is acknowledged, and the daemon exits 0. With the hold kept
;; to the drain's timeout instead: the forced exit, 75, and the peer that
;; wrote gets no answer.
(define (rebound-held! tag d extra-env)
  (let* ((r1 (string-append here "/" tag ".r1"))
         (r2 (string-append here "/" tag ".r2"))
         (pid (launch! tag d idle-args
                       (string-append (hold-env (list "leave-before-unlink" r1)
                                                (list "resumed-adapter-before-report" r2))
                                      extra-env)))
         (said (await tag)))
    (held? r1)
    (client! (string-append tag "a") (string-append d "/s") (frame-file 'outline) #f)
    (client-event (string-append tag "a") 'sent 5)
    (pause 0.5)
    (release! r1)
    (let ((late (client-end (string-append tag "a") 10)))
      (client! (string-append tag "b") (string-append d "/s") (frame-file 'outline) #f)
      (list pid said late (client-event (string-append tag "b") 'sent 5) (held? r2) r2))))

(let* ((d (fresh-dir "w5y"))
       (h (rebound-held! "w5y" d ""))
       (pid (car h)))
  (want "W5l3 the late connection is served after the resume; a peer on the rebound socket is held before its report"
        (list (cadr h) (caddr h) (cadddr h) (list-ref h 4))
        '(serving (answer ok) (sent) #t))
  (signal! pid "TERM")
  (pause 1)
  (want "W5l3 SIGTERM: the daemon waits for the resumed listener's account"
        (alive? pid)
        #t)
  (release! (list-ref h 5))
  (want "W5l3 the peer's request is answered draining, never a bare close"
        (client-end "w5yb" 10)
        '(answer draining))
  (want "W5l3 the daemon exits 0 after the stop is acknowledged"
        (list (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5y"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5y")))
        '(#t 0)))

(let* ((d (fresh-dir "w5z"))
       (h (rebound-held! "w5z" d "THEOURGIA_HOLD_MS=20000 "))
       (pid (car h)))
  (signal! pid "TERM")
  (want "W5l3 the hold kept to the drain's timeout: the forced exit, 75"
        (list (gone-within? pid 12)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5z"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5z")))
        '(#t 75))
  (want "W5l3 and the peer that wrote gets no answer"
        (let ((e (client-end "w5zb" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer)
  (release! (list-ref h 5)))

;; ---- W5s, W5t (c): the runtime's accept failures during a leave -------------------------------
;;
;; Armed through the fault seam when the leave begins (the daemon arms the
;; runtime's own points; IGROPYR_INJECT=on makes them exist). A refused
;; accept and a raise before the adapter make no adapter and no watch: the
;; exit proceeds, the idle-exit line carrying the counter. A raise on the
;; second accept, after a first late connection cancelled the exit, retires
;; the listener: the resume fails listener-exhausted.
(define (accept-fault-leave tag fault)
  (let* ((d (fresh-dir tag))
         (r1 (string-append here "/" tag ".r1"))
         (pid (launch! tag d idle-args
                       (string-append (hold-env (list "leave-before-unlink" r1))
                                      "IGROPYR_INJECT=on THEOURGIA_FAULT=" fault "@conn ")))
         (said (await tag)))
    (held? r1)
    (list d pid said r1)))

(let* ((a (accept-fault-leave "w5sa" "accept-refused-once"))
       (d (car a)) (pid (cadr a)))
  (client! "sa" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "sa" 'connected 5)
  (pause 0.5)
  (release! (cadddr a))
  (want "W5s an accept refused by the runtime during a leave: the exit proceeds, refused-accepts 1"
        (list (caddr a) (gone-within? pid 8) (idle-exit-line "w5sa"))
        '(serving #t (idle-exit (after 2) (refused-accepts 1) (callback-raised 0))))
  (want "W5s its peer is never answered"
        (let ((e (client-end "sa" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer))

(let* ((a (accept-fault-leave "w5sb" "accept-raise-once"))
       (d (car a)) (pid (cadr a)))
  (client! "sb" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "sb" 'connected 5)
  (pause 0.5)
  (release! (cadddr a))
  (want "W5s a raise in the accept before the adapter: the exit proceeds, callback-raised 1"
        (list (caddr a) (gone-within? pid 8) (idle-exit-line "w5sb"))
        '(serving #t (idle-exit (after 2) (refused-accepts 0) (callback-raised 1))))
  (want "W5s its peer is never answered"
        (let ((e (client-end "sb" 10)))
          (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
              'no-answer e))
        'no-answer))

(let* ((a (accept-fault-leave "w5tc" "accept-raise-second"))
       (d (car a)) (pid (cadr a)) (pt (number->string pid)))
  (client! "tc1" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "tc1" 'sent 5)
  (pause 0.3)
  (client! "tc2" (string-append d "/s") (frame-file 'outline) #f)
  (client-event "tc2" 'connected 5)
  (pause 0.5)
  (release! (cadddr a))
  (want "W5t (c) the listener retired by the runtime: the late request answered draining"
        (list (caddr a) (client-end "tc1" 10))
        '(serving (answer draining)))
  (want "W5t (c) exit 75, resume-failed listener-exhausted, nothing left, the lock free"
        (list (gone-within? pid 10)
              (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5tc"))) (begin (pause 0.05) (loop (+ i 1)))))
                     (rc-of "w5tc"))
              (idle-exit-line "w5tc")
              (names (string-append d "/s.d." pt)) (lock-free? (string-append d "/.s.lock")))
        '(#t 75 (idle-exit (reason resume-failed listener-exhausted)) (".endpoint") #t)))

;; W5t (d): the runtime retires the listener during a leave with NO late
;; connection served. accept-exhaust-first makes the first accept after the
;; bind (the row's own request, before the window) leave the handle reserve
;; empty, and the first accept of the leave then allocates inside the accept
;; window and raises: the listener is retired (callback-raised, exhausted)
;; with no adapter and no watch. That is a resume failure, not an idle exit:
;; 75, resume-failed listener-exhausted, and the late peer is never answered.
(let* ((d (fresh-dir "w5td"))
       (r1 (string-append here "/w5td.r1"))
       (pid (launch! "w5td" d idle-args
                     (string-append (hold-env (list "leave-before-unlink" r1))
                                    "IGROPYR_INJECT=on THEOURGIA_FAULT=accept-exhaust-first@conn ")))
       (said (await "w5td"))
       (pt (number->string pid)))
  (client! "td0" (string-append d "/s") (frame-file 'outline) #f)
  (let ((before (client-end "td0" 10)))
    (held? r1)
    (client! "td1" (string-append d "/s") (frame-file 'outline) #f)
    (client-event "td1" 'connected 5)
    (pause 0.5)
    (release! r1)
    (want "W5t (d) a request before the window is answered; the late peer of the leave never is"
          (list said before
                (let ((e (client-end "td1" 10)))
                  (if (or (equal? e '(answer empty)) (and (pair? e) (memq (car e) '(read-failed write-failed))))
                      'no-answer e)))
          '(serving (answer ok) no-answer))
    (want "W5t (d) the listener retired with no late connection served: exit 75, resume-failed listener-exhausted, nothing left, the lock free"
          (list (gone-within? pid 10)
                (begin (let loop ((i 0)) (if (and (< i 100) (not (rc-of "w5td"))) (begin (pause 0.05) (loop (+ i 1)))))
                       (rc-of "w5td"))
                (idle-exit-line "w5td")
                (names (string-append d "/s.d." pt)) (lock-free? (string-append d "/.s.lock")))
          '(#t 75 (idle-exit (reason resume-failed listener-exhausted)) (".endpoint") #t))))

;; ---- Q19: the opener makes its run directory through the walk ----------------------------
;;
;; A run root spelled as a namespace (`<d>/s.d`) while the endpoint `s` is
;; between its reclaim's unlink and its republish: the opener refuses
;; socket-dir-missing and makes nothing there. Two openers starting at once on
;; a fresh run root, both held after their connect probe found nobody, so both
;; start a daemon: one serves, the other's child is refused serve-busy.
;; THE LOSER'S OPENER CAN ANSWER READY BEFORE ITS CHILD REPORTS: spawn-and-wait!
;; asks connects? before its child's exit, so it reaches the winner while its
;; own child, still loading on a slow machine, has not reached the lock. A
;; count of the log right after both answered ready was too strong: a loaded
;; runner read 0 twice, and its log held the loser's line afterwards. So each
;; opener also holds its daemon's report (report-write, which a startup
;; refusal reaches and `(serving ...)` does not): the loser waits there while
;; both openers answer ready, and its line is counted after it has exited.
(define (opener-driver! tag run argv-text)
  (let ((drv (string-append here "/" tag "-drive.sc")))
    (call-with-output-file drv
      (lambda (p)
        (for-each (lambda (l) (put-string p l) (newline p))
          (list "(import (chezscheme) (theourgia client))"
                (string-append "(define st \"" store "\")")
                (string-append "(write (list 'driver (ensure-daemon! " argv-text " st (socket-path st))))"))))
      'replace)
    (string-append "THEOURGIA_RUN=" run " " env-text "scheme --script " drv " < /dev/null 2>&1")))
(define (driver-result text)
  (let ((d (find (lambda (d) (eq? (car d) 'driver)) (datums text))))
    (if (and d (pair? (cdr d))) (cadr d) (list 'nothing text))))
;; -> whether the driver writing to OUT has printed its answer.
(define (driven? out)
  (let ((r (driver-result (file-text out))))
    (not (and (pair? r) (eq? (car r) 'nothing)))))

(let* ((d (fresh-dir "q19"))
       (a (launch! "q19a" d "" ""))
       (said (await "q19a")))
  (kill! a)
  (let* ((rel (string-append here "/q19.rel"))
         (a2 (launch! "q19b" d "" (hold-env (list "reclaim-after-link" rel)))))
    (want "Q19 the endpoint's restart is held after its reclaim unlinked s.d"
          (list said (held? rel) (kind (string-append d "/s.d")))
          '(serving #t absent))
    (let ((answer (driver-result (sh-out (opener-driver! "q19" (string-append d "/s.d") "'(\"/usr/bin/true\")")))))
      (want "Q19 an opener whose run root is spelled s.d refuses socket-dir-missing and makes nothing there"
            (list (and (pair? answer) (car answer)) (and (pair? answer) (pair? (cdr answer)) (cadr answer))
                  (kind (string-append d "/s.d")))
            '(error socket-dir-missing absent)))
    (release! rel)
    (want "Q19 released, the endpoint republishes and serves"
          (list (await "q19b") (ask (string-append d "/s")))
          '(serving ok))
    (stop! a2)))

(let* ((d (fresh-dir "q19r"))
       (run (string-append d "/run"))
       (argv (string-append "(list \"scheme\" \"--script\" \"" tree "/theourgiad.sc\" \"serve\" st"
                            " \"--socket\" (socket-path st) \"--detach\" \"--log\" (serve-log-path st))"))
       (cmd (opener-driver! "q19r" run argv))
       (r1 (string-append here "/q19r1.rel"))
       (r2 (string-append here "/q19r2.rel"))
       (w1 (string-append here "/q19w1.rel"))
       (w2 (string-append here "/q19w2.rel"))
       (o1 (string-append here "/q19r1.out"))
       (o2 (string-append here "/q19r2.out"))
       (busy-count (lambda ()
                     (let ((t (sh-out (string-append "cat " run "/*/serve.log 2>/dev/null | grep -c '^(error serve-busy'"))))
                       (guard (e (#t t)) (read (open-input-string t))))))
       (held-markers (lambda () (filter (lambda (w) (file-exists? (string-append w ".held"))) (list w1 w2))))
       ;; THE DAEMONS BY PID, NOT BY COUNT: a count of one is also a dead
       ;; winner beside a loser that has not exited yet.
       (pids (lambda ()
               (let ((p (open-input-string (sh-out (string-append "pgrep -f 'theourgiad.sc serve " store " --socket " run "/'")))))
                 (let loop ((out '()))
                   (let ((x (guard (e (#t (eof-object))) (read p))))
                     (if (eof-object? x) (list-sort < out) (loop (if (integer? x) (cons x out) out))))))))
       (alive? (lambda (pid) (zero? (sh (string-append "kill -0 " (number->string pid) " 2>/dev/null")))))
       (endpoint (lambda ()
                   (let* ((t (sh-out (string-append "ls -d " run "/*/socket 2>/dev/null")))
                          (n (let loop ((i 0)) (if (or (= i (string-length t)) (char=? (string-ref t i) #\newline)) i (loop (+ i 1))))))
                     (substring t 0 n))))
       ;; -> the pid the public socket's link names (`<socket>.d/<pid>`), the
       ;; daemon that published it, or #f.
       (link-pid (lambda ()
                   (let* ((t (sh-out (string-append "readlink " (endpoint))))
                          (digits (let loop ((i (string-length t)) (out '()))
                                    (cond ((= i 0) out)
                                          ((char-numeric? (string-ref t (- i 1))) (loop (- i 1) (cons (string-ref t (- i 1)) out)))
                                          ((null? out) (loop (- i 1) out))
                                          (else out)))))
                     (and (pair? digits) (string->number (list->string digits)))))))
  (sh (string-append "( " (hold-env (list "opener-after-probe" r1) (list "report-write" w1)) cmd " > " o1 " ) & ( "
                     (hold-env (list "opener-after-probe" r2) (list "report-write" w2)) cmd " > " o2 " ) &"))
  (want "Q19 two openers at once: both held after their connect probe found nobody"
        (list (held? r1) (held? r2))
        '(#t #t))
  (release! r1)
  (release! r2)
  (let loop ((i 0))
    (unless (or (>= i 600) (and (driven? o1) (driven? o2)))
      (pause 0.05)
      (loop (+ i 1))))
  (let loop ((i 0))
    (unless (or (>= i 400) (pair? (held-markers)))
      (pause 0.05)
      (loop (+ i 1))))
  (pause 1)
  (let* ((ps (pids))
         (winner (link-pid))
         (loser (and winner (= 2 (length ps)) (memv winner ps) (car (remv winner ps)))))
    (want "Q19 two openers at once on a fresh run root: both ready while the loser's daemon is held before its report: one report hold reached, no serve-busy line yet, two daemons, one of them the endpoint's publisher"
          (list (driver-result (file-text o1)) (driver-result (file-text o2))
                (length (held-markers)) (busy-count) (length ps) (and loser #t))
          '(ready ready 1 0 2 #t))
    (release! w1)
    (release! w2)
    (let loop ((i 0))
      (unless (or (>= i 600) (not loser) (not (alive? loser)))
        (pause 0.05)
        (loop (+ i 1))))
    (want "Q19 released, the loser exits and the winner lives: the log has exactly one serve-busy refusal, the loser's, carrying its attempt token"
          (list (and loser (not (alive? loser))) (and winner (alive? winner)) (busy-count)
                (let ((ds (filter (lambda (x) (and (pair? x) (eq? (car x) 'error) (pair? (cdr x)) (eq? (cadr x) 'serve-busy)))
                                  (datums (sh-out (string-append "cat " run "/*/serve.log 2>/dev/null"))))))
                  (and (= 1 (length ds))
                       (let ((a (assq 'attempt (filter pair? (cddr (car ds))))))
                         (and a (string? (cadr a)) (= 16 (string-length (cadr a))))))))
          '(#t #t 1 #t))
    (want "Q19 and one daemon serves the store there: the winner, still the endpoint's publisher, accepting a connection"
          (list (equal? (pids) (if winner (list winner) '())) (and winner (eqv? (link-pid) winner)) (listening? (endpoint)))
          '(#t #t #t)))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null"))
  (pause 1))

;; ---- the shell (sub-step 3): THEOURGIA_IDLE_EXIT, the shell's starts -------------------
;;
;; A DRIVER RUNS THE SHELL AND A SCRIPT AGAINST IT, so a row can call, wait out a
;; window and call again in one session. Script lines: `send <json>`, `await
;; <id>` (until the answer with that id, forty seconds at most), `sleep
;; <seconds>`, `touch <path>`, `wait <path>` (until it exists), `spawn <command>`
;; (in the background), `run <command>` (to its end), `mark <label>` (records
;; (mark <label> <time>)), `exists <path>`; each answer's arrival is recorded
;; as (got <id> <time>)
;; (records (exists "<path>" #t|#f)). The shell's stdout lines go to
;; <here>/<tag>.mcp, its stderr (with THEOURGIA_TRACE=1, the trace) to
;; <tag>.mcperr, the script's records to <tag>.ev.
(define mcp-driver (string-append here "/mcp-drive.py"))
(call-with-output-file mcp-driver
  (lambda (p)
    (for-each (lambda (l) (put-string p l) (newline p))
      '("import subprocess, sys, time, os, json, threading"
        "cmd, script, out, err, ev = sys.argv[1:6]"
        "o = open(out, 'w'); e = open(ev, 'w')"
        "p = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=open(err, 'w'), text=True)"
        "seen = {}"
        "def reader():"
        "    for line in p.stdout:"
        "        o.write(line); o.flush()"
        "        try:"
        "            d = json.loads(line)"
        "            if 'id' in d:"
        "                seen[d['id']] = True"
        "                e.write('(got %s %.3f)\\n' % (d['id'], time.time())); e.flush()"
        "        except Exception: pass"
        "t = threading.Thread(target=reader); t.start()"
        "for raw in open(script):"
        "    op, _, arg = raw.rstrip('\\n').partition(' ')"
        "    if op == 'send': p.stdin.write(arg + '\\n'); p.stdin.flush()"
        "    elif op == 'await':"
        "        end = time.time() + 40"
        "        while int(arg) not in seen and time.time() < end: time.sleep(0.02)"
        "    elif op == 'sleep': time.sleep(float(arg))"
        "    elif op == 'touch': open(arg, 'a').close()"
        "    elif op == 'spawn': subprocess.Popen(arg, shell=True)"
        "    elif op == 'run': subprocess.call(arg, shell=True)"
        "    elif op == 'mark': e.write('(mark %s %.3f)\\n' % (arg, time.time())); e.flush()"
        "    elif op == 'wait':"
        "        end = time.time() + 30"
        "        while not os.path.exists(arg) and time.time() < end: time.sleep(0.02)"
        "    elif op == 'exists': e.write('(exists \"%s\" %s)\\n' % (arg, '#t' if os.path.lexists(arg) else '#f')); e.flush()"
        "p.stdin.close()"
        "try: p.wait(timeout=30)"
        "except subprocess.TimeoutExpired: p.kill()"
        "t.join(5)")))
  'replace)

(define hello-json
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}")
(define ready-json "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
(define (outline-call id)
  (format "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_outline\",\"arguments\":{\"argv\":[]}}}" id))

;; (shell-run! tag env socket lines) -> the shell's stdout lines. ENV is
;; environment text; SOCKET a --socket for the shell, or #f.
(define (shell-run! tag env socket lines)
  (let ((script (string-append here "/" tag ".script"))
        (out (string-append here "/" tag ".mcp")))
    (call-with-output-file script
      (lambda (p) (for-each (lambda (l) (put-string p l) (newline p))
                            (append (list (string-append "send " hello-json) "await 1"
                                          (string-append "send " ready-json))
                                    lines)))
      'replace)
    (sh (string-append "python3 " mcp-driver " \"" env-text env "THEOURGIA_TRACE=1 exec scheme --script " tree
                       "/mcp/server.sc --store " store (if socket (string-append " --socket " socket) "")
                       "\" " script " " out " " here "/" tag ".mcperr " here "/" tag ".ev"))
    (let ((t (file-text out)))
      (let split ((i 0) (from 0) (acc '()))
        (cond ((= i (string-length t)) (reverse (if (> i from) (cons (substring t from i) acc) acc)))
              ((char=? (string-ref t i) #\newline) (split (+ i 1) (+ i 1) (cons (substring t from i) acc)))
              (else (split (+ i 1) from acc)))))))

;; -> the shell's answer line for ID, or #f.
(define (answer-line lines id)
  (find (lambda (l) (text-has? l (format "\"id\":~a," id))) lines))
;; -> ok when the call's result is not an error and carries an (ok ...) text,
;; refused (with the refusal's kind) when it is the shell's own error result,
;; or the line.
(define (call-outcome lines id)
  (let ((l (answer-line lines id)))
    (cond ((not l) 'no-answer)
          ((and (text-has? l "\"isError\":false") (text-has? l "(ok")) 'ok)
          ((text-has? l "\"isError\":true")
           (let ((i (index-of l "(error ")))
             (if i (let ((d (guard (e (#t #f)) (read (open-input-string (substring l i (string-length l)))))))
                     (if (pair? d) (list 'refused (cadr d)) l))
                 l)))
          (else l))))
;; -> the default socket of the store under the run root RUN.
(define (default-socket run)
  (let ((d (datums (sh-out (string-append "THEOURGIA_RUN=" run " " env-text
                                          "scheme --script /dev/stdin <<'X'\n(import (chezscheme) (theourgia client))\n(write (list (socket-path \""
                                          store "\")))\nX\n")))))
    (if (and (pair? d) (pair? (car d))) (caar d) "")))
(define (dirname p) (let loop ((i (- (string-length p) 1)))
                      (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (serve-log-data run) (datums (file-text (string-append (dirname (default-socket run)) "/serve.log"))))
(define (count-lines data head) (length (filter (lambda (d) (eq? (car d) head)) data)))
(define (trace-count tag word)
  (length (filter (lambda (d) (and (eq? (car d) 'trace) (pair? (cdr d)) (eq? (cadr d) word)))
                  (datums (file-text (string-append here "/" tag ".mcperr"))))))
;; -> the first argument of every (trace WORD <n> ...) the shell wrote, in order.
(define (trace-values tag word)
  (map caddr (filter (lambda (d) (and (eq? (car d) 'trace) (pair? (cdr d)) (eq? (cadr d) word) (pair? (cddr d))))
                     (datums (file-text (string-append here "/" tag ".mcperr"))))))
;; -> how many daemons serve the store under RUN. ONLY THE DAEMONS: a row that
;; starts one by hand runs it under `sh -c`, whose command line names the
;; same words, so the pattern is anchored at the program.
(define (daemons-at run)
  (let ((t (sh-out (string-append "pgrep -f '^([^ ]*/)?scheme --script [^ ]*theourgiad.sc serve " store
                                  " --socket " run "/' | wc -l"))))
    (guard (e (#t t)) (read (open-input-string t)))))
;; -> the same, once a start that lost (it logs serve-busy and exits) has
;; gone: read again every 50 ms while more than one is there, for at most five
;; seconds. One that stays longer is counted.
(define (daemons-settled run)
  (let loop ((i 0))
    (let ((n (daemons-at run)))
      (if (and (number? n) (> n 1) (< i 100)) (begin (pause 0.05) (loop (+ i 1))) n))))

;; W3: the shell's first request starts a daemon with --idle-exit; after the
;; window it is gone, its socket removed, its lock free; the next request
;; starts a new one.
(let* ((run (string-append sock-here "/run-w3"))
       (sock (default-socket run))
       (lines (shell-run! "w3" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 ")
                          #f
                          (list (string-append "send " (outline-call 2)) "await 2" "sleep 4.5"
                                (string-append "exists " sock)
                                (string-append "send " (outline-call 3)) "await 3")))
       (log (serve-log-data run)))
  (want "W3 the shell's two requests are answered"
        (list (call-outcome lines 2) (call-outcome lines 3))
        '(ok ok))
  (want "W3 between them the first daemon left by the idle exit and its socket was gone"
        (list (count-lines log 'idle-exit)
              (filter (lambda (d) (eq? (car d) 'exists)) (datums (file-text (string-append here "/w3.ev")))))
        (list 1 (list (list 'exists sock #f))))
  (want "W3 the second request started a second daemon"
        (count-lines log 'serving)
        2))

;; W3c: the options the shell starts its daemon with, read from its serving line.
(define (serving-options run)
  (let ((l (find (lambda (d) (eq? (car d) 'serving)) (serve-log-data run))))
    (and l (list (assq 'idle-exit (cdr l)) (assq 'idle-grace-ms (cdr l))))))
(let* ((run (string-append sock-here "/run-w3c"))
       (lines (shell-run! "w3c" (string-append "THEOURGIA_RUN=" run " ") #f
                          (list (string-append "send " (outline-call 2)) "await 2"))))
  (want "W3c THEOURGIA_IDLE_EXIT unset: the daemon carries --idle-exit 1800 and --idle-grace-ms 200"
        (list (call-outcome lines 2) (serving-options run))
        '(ok ((idle-exit 1800) (idle-grace-ms 200))))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))
(let* ((run (string-append sock-here "/run-w3c0"))
       (lines (shell-run! "w3c0" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=0 ") #f
                          (list (string-append "send " (outline-call 2)) "await 2" "sleep 3.5"))))
  (want "W3c THEOURGIA_IDLE_EXIT=0: --idle-exit 0, and the daemon is alive past the window"
        (list (call-outcome lines 2) (serving-options run) (daemons-at run))
        '(ok ((idle-exit 0) (idle-grace-ms 200)) 1))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))
(want "W3c a daemon started by serve by hand carries neither"
      (let ((l (line-of "w4" 'serving))) (and l (list (assq 'idle-exit (cdr l)) (assq 'idle-grace-ms (cdr l)))))
      '(#f #f))

;; W3d: a bad THEOURGIA_IDLE_EXIT is the shell's refusal of the call, before any
;; spawn; the shell's stdout carries only JSON-RPC frames.
(for-each
  (lambda (v)
    (let* ((run (string-append sock-here "/run-w3d"))
           (lines (shell-run! "w3d" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=" v " ") #f
                              (list (string-append "send " (outline-call 2)) "await 2"))))
      (want (string-append "W3d THEOURGIA_IDLE_EXIT=" v ": the call is refused bad-environment, no daemon, stdout only JSON-RPC")
            (list (call-outcome lines 2) (daemons-at run) (trace-count "w3d" 'spawn)
                  (for-all (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\{))) lines))
            '((refused bad-environment) 0 0 #t))))
  '("abc" "-1" "1.5"))

;; W3d, digits of another script: char-numeric? holds for any Unicode decimal
;; digit (a reading on Chez Scheme 10.1.0: (char-numeric? #\xFF12) is #t and
;; (string->number "\xFF12;\xFF11;") is #f), while the daemon's count parser
;; takes ASCII digits only, so a full-width spelling passed a char-numeric?
;; check and was refused by the daemon onto the shell's stdout. The shell
;; checks ASCII digits: the spelling is the shell's refusal, before any
;; spawn. It is written with escapes so this file stays ASCII.
(let* ((run (string-append sock-here "/run-w3d2"))
       (lines (shell-run! "w3d2" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=\xFF12;\xFF11; ") #f
                          (list (string-append "send " (outline-call 2)) "await 2"))))
  (want "W3d THEOURGIA_IDLE_EXIT in full-width digits: the call is refused bad-environment, no daemon, stdout only JSON-RPC"
        (list (call-outcome lines 2) (daemons-at run) (trace-count "w3d2" 'spawn)
              (for-all (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\{))) lines))
        '((refused bad-environment) 0 0 #t)))

;; W3e (the shell's half): at a public path with no room for the private name,
;; the shell refuses before any spawn, and its stdout carries only frames.
(let* ((d (fresh-dir "w3e"))
       (room (- (sun-path-max) 13 (string-length (string-append d "/"))))
       (refused (string-append d "/" (make-string room #\p)))
       (lines (shell-run! "w3e" "" refused (list (string-append "send " (outline-call 2)) "await 2"))))
  (want "W3e through the shell: refused socket-path-too-long before any spawn, stdout only JSON-RPC"
        (list (call-outcome lines 2) (trace-count "w3e" 'spawn)
              (for-all (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\{))) lines))
        '((refused socket-path-too-long) 0 #t)))

;; W5c: the shell's request arrives while the old daemon is between its unlink
;; and its exit (held): its starts meet serve-busy and are retried inside one
;; ensure attempt; the request is answered by the new daemon; the old one's
;; log shows its idle exit.
;; THE RELEASE WAITS FOR THE RETRY, NOT FOR A TIME. The old daemon is released
;; once the shell's trace shows a third spawn -- the second request's first
;; start and its retry -- bounded at thirty seconds. A retry follows only a
;; start that met serve-busy, and the old daemon holds the lock until the
;; release, so the retry is a precondition of the release rather than a race
;; against it: a fixed sleep let a machine whose daemon start outlasts the
;; sleep reach the lock after the release, with no retry to count. A shell
;; that does not retry leaves two spawns at the bound, and the row reads red.
(let* ((run (string-append sock-here "/run-w5c"))
       (rel (string-append here "/w5c.rel"))
       (trace-file (string-append here "/w5c.mcperr"))
       (lines (shell-run! "w5c"
                          (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 " (hold-env (list "leave-after-unlink" rel)))
                          #f
                          (list (string-append "send " (outline-call 2)) "await 2"
                                (string-append "wait " rel ".held")
                                (string-append "send " (outline-call 3))
                                (string-append "run i=0; until [ \"$(/usr/bin/grep -c '^(trace spawn ' " trace-file
                                               " 2>/dev/null)\" -ge 3 ] 2>/dev/null || [ $i -ge 1500 ]; do sleep 0.02; i=$((i+1)); done")
                                (string-append "touch " rel) "await 3")))
       (log (serve-log-data run)))
  (want "W5c both requests answered; the second by a new daemon after the old one's idle exit"
        (list (call-outcome lines 2) (call-outcome lines 3) (count-lines log 'idle-exit) (count-lines log 'serving))
        '(ok ok 1 2))
  (want "W5c inside one ensure attempt the start was retried: at least three spawns, one attempt per request"
        (list (>= (trace-count "w5c" 'spawn) 3) (trace-values "w5c" 'ensure-attempt))
        '(#t (1 1)))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))

;; W5c2: the same, with a late connection that cancels the exit: the shell's
;; retry finds the resumed daemon (no second daemon ran).
(let* ((run (string-append sock-here "/run-w5c2"))
       (sock (default-socket run))
       (r1 (string-append here "/w5c2.r1"))
       (r2 (string-append here "/w5c2.r2"))
       (late (string-append "python3 " client-py " " sock " " (frame-file 'outline) " " here "/c2late.cl -"))
       (lines (shell-run! "w5c2"
                          (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 "
                                         (hold-env (list "leave-before-unlink" r1) (list "leave-after-unlink" r2)))
                          #f
                          (list (string-append "send " (outline-call 2)) "await 2"
                                (string-append "wait " r1 ".held")
                                (string-append "spawn " late) "sleep 0.8"
                                (string-append "touch " r1)
                                (string-append "wait " r2 ".held")
                                (string-append "send " (outline-call 3)) "sleep 1"
                                (string-append "touch " r2) "await 3")))
       (log (serve-log-data run)))
  (want "W5c2 the second request is answered by the resumed daemon: one daemon ever, no idle exit"
        (list (call-outcome lines 2) (call-outcome lines 3) (count-lines log 'serving)
              (count-lines log 'idle-resumed) (count-lines log 'idle-exit))
        '(ok ok 1 1 0))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))

;; W5d, W5d2, W5i: the shell's ready probe and its attempts, placed by the shell's
;; two hold stages against a daemon started by hand at the shell's socket.
(define (hand-daemon-command run tag env)
  (let ((sock (default-socket run)))
    (sh (string-append "mkdir -p " (dirname sock)))
    (string-append env-text env "scheme --script " tree "/theourgiad.sc serve " store " --socket " sock
                   " --idle-exit 2 --idle-grace-ms 200 > " here "/" tag ".out 2>&1 < /dev/null")))
(define (ev-of tag) (datums (file-text (string-append here "/" tag ".ev"))))
(define (ev-time tag head key)
  (let ((d (find (lambda (d) (and (eq? (car d) head) (pair? (cdr d)) (equal? (cadr d) key))) (ev-of tag))))
    (and d (caddr d))))

;; W5d: the ready probe lands before the unlink: it cancels the exit, and the
;; request, sent after the resume, is answered by the same daemon.
(let* ((run (string-append sock-here "/run-w5d"))
       (s1 (string-append here "/w5d.s1")) (s2 (string-append here "/w5d.s2"))
       (d1 (string-append here "/w5d.d1"))
       (daemon (hand-daemon-command run "w5d-d1" (hold-env (list "leave-before-unlink" d1))))
       (lines (shell-run! "w5d" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 "
                                               (hold-env (list "shell-before-ensure" s1) (list "shell-after-ready" s2)))
                          #f
                          (list (string-append "send " (outline-call 2))
                                (string-append "wait " s1 ".held")
                                (string-append "spawn " daemon)
                                (string-append "wait " d1 ".held")
                                (string-append "touch " s1)
                                (string-append "wait " s2 ".held") "sleep 0.3"
                                (string-append "touch " d1) "sleep 1.5"
                                (string-append "touch " s2) "await 2")))
       (log (log-of "w5d-d1")))
  (want "W5d the probe cancelled the exit; the request is answered by the same daemon, nothing spawned"
        (list (call-outcome lines 2) (count-lines log 'idle-resumed) (count-lines log 'idle-exit)
              (trace-count "w5d" 'spawn) (trace-count "w5d" 'ensure-attempt) (daemons-settled run))
        '(ok 1 0 0 1 1))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))

;; W5d2: the probe lands before the unlink and the request's connect after it,
;; while the daemon is held: the second ensure attempt runs (its start meets
;; serve-busy and is retried until the resume), the request is answered, and a
;; third attempt never happens.
(let* ((run (string-append sock-here "/run-w5e"))
       (s1 (string-append here "/w5e.s1")) (s2 (string-append here "/w5e.s2"))
       (d1 (string-append here "/w5e.d1")) (d2 (string-append here "/w5e.d2"))
       (daemon (hand-daemon-command run "w5e-d1" (hold-env (list "leave-before-unlink" d1) (list "leave-after-unlink" d2))))
       (lines (shell-run! "w5e" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 "
                                               (hold-env (list "shell-before-ensure" s1) (list "shell-after-ready" s2)))
                          #f
                          (list (string-append "send " (outline-call 2))
                                (string-append "wait " s1 ".held")
                                (string-append "spawn " daemon)
                                (string-append "wait " d1 ".held")
                                (string-append "touch " s1)
                                (string-append "wait " s2 ".held") "sleep 0.3"
                                (string-append "touch " d1)
                                (string-append "wait " d2 ".held")
                                (string-append "touch " s2) "sleep 1"
                                (string-append "touch " d2) "await 2")))
       (log (log-of "w5e-d1")))
  (want "W5d2 the request is answered after a second ensure attempt, never a third"
        (list (call-outcome lines 2) (trace-count "w5e" 'ensure-attempt) (count-lines log 'idle-resumed)
              (daemons-settled run))
        '(ok 2 1 1))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))

;; W5d3: the request's connect finds no daemon after BOTH ready answers: the
;; shell answers connect-failed, as its -32603 not-sent error (the call never
;; went out, so it was not carried out), and never starts a third. Nothing serves at
;; first, so the first attempt starts a daemon; the shell is held after its
;; ready, the fixture kills that daemon, and the connect finds nobody, so the
;; second attempt starts another. The fixture takes the release away when the
;; shell traces that second attempt (a stage waits only while its release
;; file is absent): that line is written after the first hold released and
;; before the second daemon starts, so no fixed sleep decides which pass sees
;; the release. The shell holds again after the second ready; that daemon is
;; killed too, and the connect finds nobody again.
;; -> (not-sent KIND (carried-out X)) when the answer to ID is the shell's
;; -32603 not-sent error, or the line.
(define (not-sent-outcome lines id)
  (let ((l (answer-line lines id)))
    (cond ((not l) 'no-answer)
          ((and (text-has? l "\"code\":-32603") (index-of l "(error "))
           => (lambda (i)
                (let ((d (guard (e (#t #f)) (read (open-input-string (substring l i (string-length l)))))))
                  (if (and (pair? d) (pair? (cdr d))) (list 'not-sent (cadr d) (assq 'carried-out (cddr d))) l))))
          (else l))))
(let* ((run (string-append sock-here "/run-w5d3"))
       (s2 (string-append here "/w5d3.s2"))
       (trace-file (string-append here "/w5d3.mcperr"))
       (kill-daemons (string-append "run pkill -KILL -f 'theourgiad.sc serve " store " --socket " run "/'"))
       (lines (shell-run! "w5d3" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 "
                                                (hold-env (list "shell-after-ready" s2)))
                          #f
                          (list (string-append "send " (outline-call 2))
                                (string-append "wait " s2 ".held")
                                kill-daemons "sleep 0.3"
                                (string-append "touch " s2)
                                (string-append "run i=0; until /usr/bin/grep -q '^(trace ensure-attempt 2 ' " trace-file
                                               " || [ $i -ge 1500 ]; do sleep 0.02; i=$((i+1)); done; rm -f " s2 " " s2 ".held")
                                (string-append "wait " s2 ".held")
                                kill-daemons "sleep 0.3"
                                (string-append "touch " s2) "await 2"))))
  (want "W5d3 no daemon after the second ready either: connect-failed, two ensure attempts, never a third"
        (list (not-sent-outcome lines 2) (trace-count "w5d3" 'ensure-attempt))
        '((not-sent connect-failed (carried-out no)) 2))
  (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null")))

;; W5i: one deadline per request. The first attempt spends about six seconds
;; on serve-busy (the old daemon held after its unlink), then a new daemon is
;; ready; the fixture kills it and holds its lock, so the request's connect
;; finds nobody and the second attempt meets serve-busy until the deadline the
;; first one opened. The shell gives up near that deadline (ten seconds after
;; the first start, two and a half of slack), not ten seconds after the second.
(let* ((run (string-append sock-here "/run-w5i"))
       (sock (default-socket run))
       (lock (string-append (dirname sock) "/.socket.lock"))
       (s2 (string-append here "/w5i.s2"))
       (d1 (string-append here "/w5i.d1"))
       (daemon (hand-daemon-command run "w5i-d1" (string-append (hold-env (list "leave-after-unlink" d1)) "THEOURGIA_HOLD_MS=40000 ")))
       (_ (sh (string-append "( " daemon " ) &")))
       (_ (held? d1))
       (lines (shell-run! "w5i" (string-append "THEOURGIA_RUN=" run " THEOURGIA_IDLE_EXIT=2 "
                                               (hold-env (list "shell-after-ready" s2)))
                          #f
                          (list "mark sent" (string-append "send " (outline-call 2)) "sleep 6"
                                (string-append "touch " d1)
                                (string-append "wait " s2 ".held")
                                (string-append "run pkill -KILL -f 'theourgiad.sc serve " store " --socket " run "/'")
                                (string-append "spawn python3 -c \"import fcntl,time; f=open('" lock "','a'); "
                                               "fcntl.flock(f, fcntl.LOCK_EX); time.sleep(25)\"")
                                "sleep 0.5"
                                (string-append "touch " s2) "await 2"))))
  (want "W5i the request is refused serve-busy after two attempts, never a third"
        (list (call-outcome lines 2) (trace-count "w5i" 'ensure-attempt))
        '((refused serve-start-failed) 2))
  (want "W5i and it gave up at the first attempt's deadline: under 12.5 s from the send"
        (let ((a (ev-time "w5i" 'mark 'sent)) (b (ev-time "w5i" 'got 2)))
          (and a b (< (- b a) 12.5)))
        #t)
  ;; `.` FOR THE PARENTHESIS: pkill reads an extended regex, and an
  ;; unbalanced `(` makes it refuse the pattern and kill nothing.
  (sh (string-append "pkill -KILL -f \"import fcntl,time; f=open.'" lock "'\" 2>/dev/null"))
  ;; A START THE SHELL SPAWNED BEFORE IT GAVE UP may come up now that the lock
  ;; is free, and would serve until its window ends: stopped here, as every
  ;; shell row stops its run's daemons, until none is left.
  (let loop ((i 0))
    (sh (string-append "pkill -TERM -f 'theourgiad.sc serve " store " --socket " run "/' 2>/dev/null"))
    (when (and (< i 100) (let ((n (daemons-at run))) (or (not (number? n)) (> n 0))))
      (pause 0.05)
      (loop (+ i 1)))))

(teardown!)
(printf "rows: ~a\n~a failures\ndaemon-idle complete\n" rows bad)
