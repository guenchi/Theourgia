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

;; THE SOCKET'S NAMESPACE: a private socket behind a public link, in a
;; directory the daemon reserves with a marker.
;;
;; A daemon serving `<socket>` binds `<socket>.d/<pid>` and publishes
;; `<socket>` as a symbolic link to it; `<socket>.d` is itself a link to the
;; daemon's own directory `<socket>.d.<pid>[-<n>]`, which holds `.endpoint`,
;; the marker naming the endpoint. These rows read the layout on disk, what a
;; start reclaims and what it leaves, what it refuses, and the order of a
;; reclaim interrupted at each of its steps. Each row starts its daemons by
;; hand, as an opener does, and stops them itself.

(import (chezscheme)
        (only (theourgia ffi) entry-name-type list-entries sun-path-max real-path
              path-case-sensitive?))

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
(define here (string-append scratch-base "/dns-" pid-text))
(define sock-here (string-append socket-base "/dns-" pid-text))
(define tree (string-append (current-directory) "/.."))
(define store (string-append here "/store"))
(define store2 (string-append here "/store2"))

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

(define (head-of d n)
  (let loop ((d d) (n n) (out '()))
    (if (or (= n 0) (not (pair? d))) (reverse out) (loop (cdr d) (- n 1) (cons (car d) out)))))

(define env-text
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "))

(sh (string-append "rm -rf " here " " sock-here "; mkdir -p " here " " sock-here))
(sh (string-append env-text "THEOURGIA_LOCAL=1 scheme --script " tree "/core.sc init --store " store
                   " --wire > /dev/null 2>&1 < /dev/null"))
(sh (string-append env-text "THEOURGIA_LOCAL=1 scheme --script " tree "/core.sc init --store " store2
                   " --wire > /dev/null 2>&1 < /dev/null"))

;; ---- starting, asking, stopping ---------------------------------------------
;;
;; EVERY PROCESS A ROW STARTS IS REGISTERED, so that the teardown can stop it
;; whatever the row did or raised, with the instant it started.
;;
;; NEVER: A SIGNAL TO A PID THAT WAS NOT VERIFIED. A pid is reused once its
;; process is gone, so a row or the teardown that signals a daemon which
;; already exited could reach an unrelated process. signal! sends only when
;; the pid answers kill -0 AND its start instant (ps lstart) is the one
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

;;
;; (start! tag cwd socket env . args): `serve` in the background from CWD, its
;; output in <here>/<tag>.out; ENV is environment text (a hold, THEOURGIA_RUN).
;; The pid is the daemon's own: the subshell execs scheme.
(define (start! tag cwd socket env . args)
  (let ((out (string-append here "/" tag ".out"))
        (pidf (string-append here "/" tag ".pid")))
    (sh (string-append "rm -f " out " " pidf))
    (sh (string-append "( cd " cwd " && " env-text env " exec scheme --script " tree "/theourgiad.sc serve "
                       (if (and (pair? args) (car args)) (car args) store)
                       (if socket (string-append " --socket " socket) "")
                       " > " out " 2>&1 < /dev/null & echo $! > " pidf " )"))
    (let loop ((i 0))
      (if (and (< i 100) (string=? "" (file-text pidf))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
    (started! (string->number (let ((t (file-text pidf))) (substring t 0 (max 0 (- (string-length t) 1))))))))

;; -> serving, or the first datum the start wrote that is not `serving`, or
;; timeout. Bounded at fifteen seconds.
(define (await tag)
  (let ((out (string-append here "/" tag ".out")))
    (let loop ((i 0))
      (let ((ds (datums (file-text out))))
        (cond
          ((exists (lambda (d) (and (pair? d) (eq? (car d) 'serving))) ds) 'serving)
          ((exists (lambda (d) (and (pair? d) (eq? (car d) 'error))) ds)
           (find (lambda (d) (and (pair? d) (eq? (car d) 'error))) ds))
          ((>= i 300) 'timeout)
          (else (sh "sleep 0.05") (loop (+ i 1))))))))

(define (alive? pid) (and pid (zero? (system (format "kill -0 ~a 2>/dev/null" pid)))))

(define (wait-gone pid)
  (let loop ((i 0))
    (cond ((not (alive? pid)) #t)
          ((>= i 300) #f)
          (else (sh "sleep 0.05") (loop (+ i 1))))))

(define (stop! pid) (signal! pid "TERM") (wait-gone pid))
(define (kill! pid) (signal! pid "KILL") (wait-gone pid))

;; Start a daemon in D after the shell command MAKE; the shell execs the daemon, so the
;; daemon keeps the pid MAKE saw as $$.
(define (start-over! tag d make)
  (let ((out (string-append here "/" tag ".out"))
        (pidf (string-append here "/" tag ".pid")))
    (sh (string-append "( cd " d " && exec sh -c \"" make " && exec env " env-text " scheme --script "
                       tree "/theourgiad.sc serve " store " --socket s\" > " out " 2>&1 < /dev/null & echo $! > "
                       pidf " )"))
    (let loop ((i 0))
      (if (and (< i 100) (string=? "" (file-text pidf))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
    (started! (string->number (let ((t (file-text pidf))) (substring t 0 (max 0 (- (string-length t) 1))))))))

;; (launch! tag dir st socket args env) -> pid: `serve` of store ST in the
;; background from DIR, its output in <here>/<tag>.out, and `(rc <code>)`
;; appended to it when it exits. ENV is environment text, as for start!.
(define (launch! tag dir st socket args env)
  (let ((out (string-append here "/" tag ".out"))
        (pidf (string-append here "/" tag ".pid")))
    (sh (string-append "rm -f " out " " pidf))
    (sh (string-append "( cd " dir " || exit 1; " env-text env "scheme --script " tree "/theourgiad.sc serve "
                       st " --socket " socket args " > " out " 2>&1 < /dev/null & echo $! > " pidf
                       "; wait $!; echo \"(rc $?)\" >> " out " ) > /dev/null 2>&1 &"))
    (let loop ((i 0))
      (if (and (< i 100) (string=? "" (file-text pidf))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
    (started! (string->number (let ((t (file-text pidf)))
                                (substring t 0 (max 0 (- (string-length t) 1))))))))

;; -> the exit code a launch! appended, or #f while there is none.
(define (rc-of tag)
  (let ((rc (find (lambda (d) (eq? (car d) 'rc)) (datums (file-text (string-append here "/" tag ".out"))))))
    (and rc (cadr rc))))

;; -> whether SUB occurs in TEXT.
(define (text-has? text sub)
  (let ((n (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) sub) #t)
            (else (loop (+ i 1)))))))

;; -> the exit code a launch! appended, waited for up to fifteen seconds, or
;; still-running.
(define (rc-wait tag)
  (let loop ((i 0))
    (cond ((rc-of tag) => (lambda (rc) rc))
          ((>= i 300) 'still-running)
          (else (sh "sleep 0.05") (loop (+ i 1))))))

;; -> the first line of TEXT that is not a datum's (a raise's message on
;; stderr, say), cut to 200 characters, or #f.
(define (stray-line text)
  (let loop ((lines (let split ((i 0) (from 0) (out '()))
                      (cond ((= i (string-length text)) (reverse (cons (substring text from i) out)))
                            ((char=? (string-ref text i) #\newline)
                             (split (+ i 1) (+ i 1) (cons (substring text from i) out)))
                            (else (split (+ i 1) from out))))))
    (cond ((null? lines) #f)
          ((and (> (string-length (car lines)) 0) (not (char=? (string-ref (car lines) 0) #\()))
           (substring (car lines) 0 (min 200 (string-length (car lines)))))
          (else (loop (cdr lines))))))

;; (serve-bounded tag dir st socket args) -> (datums rc stray): `serve` of
;; store ST run to its end from DIR, its output in <here>/<tag>.out, STRAY its
;; first line that is not a datum. A start a row expects to be refused that
;; serves instead is stopped after fifteen seconds, and rc reads
;; still-running: the row is red, says what it saw, and the run goes on.
(define (serve-bounded tag dir st socket args)
  (let ((out (string-append here "/" tag ".out")))
    (let ((pid (launch! tag dir st socket args "")))
      (let loop ((i 0))
        (let* ((ds (datums (file-text out)))
               (rc (find (lambda (d) (eq? (car d) 'rc)) ds)))
          (cond
            (rc (list (remp (lambda (d) (eq? (car d) 'rc)) ds) (cadr rc) (stray-line (file-text out))))
            ((>= i 300) (kill! pid) (list ds 'still-running (stray-line (file-text out))))
            (else (sh "sleep 0.05") (loop (+ i 1)))))))))

;; -> the first N items of the first datum a bounded start printed, or
;; (nothing <rc> <stray>) when it printed none: an exit before any answer
;; names the line it left on stderr.
(define (first-said r n)
  (if (pair? (car r)) (head-of (caar r) n) (list 'nothing (cadr r) (caddr r))))

;; NOTHING THIS RUN STARTED MAY OUTLIVE IT, and that is counted, not assumed:
;; every daemon and client these rows run names this run's directory, so a
;; process still matching it after the rows is a leak and reads red. The sweep
;; then stops what the census counted, so a red run does not linger either.
(define (left-running)
  (let ((t (sh-out (string-append "pgrep -f '[d]ns-" pid-text "/' | wc -l"))))
    (guard (e (#t t)) (read (open-input-string t)))))
(define (sweep!)
  (for-each (lambda (pid) (if (alive? pid) (kill! pid))) started)
  (sh (string-append "pkill -KILL -f '[d]ns-" pid-text "/' 2>/dev/null")))
(define (teardown!)
  (want "teardown: no process this run started is still running" (left-running) 0)
  (sweep!)
  (sh (string-append "rm -rf " sock-here)))

;; A ROW THAT RAISES OUTSIDE ITS CHECK ENDS THE RUN, through the teardown: the
;; raise counts as one failure, the census still runs and no daemon is left.
;; The run then ends without its completion line.
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

;; A plain `outline` through SOCKET, from the scratch directory: the answer's
;; head, ok or error.
(define (ask-any socket . st)
  (let ((d (datums (sh-out (string-append "cd " here " && " env-text "scheme --script " tree
                                          "/theourgia.sc outline --wire --store "
                                          (if (pair? st) (car st) store)
                                          " --socket " socket " < /dev/null 2>&1")))))
    (if (pair? d) (car (car d)) 'nothing)))

;; -> whether something accepts a connection at the socket path SOCKET.
(define (listening? socket)
  (zero? (sh (string-append "python3 -c \"import socket,sys; s=socket.socket(socket.AF_UNIX); s.settimeout(2);"
                            " s.connect(sys.argv[1])\" " socket " > /dev/null 2>&1"))))

;; ASKED OF A DAEMON: a plain command answers through a core.sc child when
;; nobody listens, so `ok` alone would not say a daemon is there. -> no-daemon
;; when nothing accepts at SOCKET, otherwise the answer's head.
(define (ask socket . st)
  (if (listening? socket) (apply ask-any socket st) 'no-daemon))

(define (kind p) (guard (e (#t 'unreadable)) (entry-name-type p)))
;; A link's text, read by the system's readlink so that these rows also run on
;; a tree without the daemon's own reader.
(define (link p)
  (and (eq? (kind p) 'link)
       (let ((t (sh-out (string-append "readlink " p))))
         (and (> (string-length t) 0) (substring t 0 (- (string-length t) 1))))))
(define (names dir)
  (let ((l (guard (e (#t 'unreadable)) (list-entries dir))))
    (if (list? l) (list-sort string<? l) l)))

;; The hold seam: (hold-env stage rel) arms it; (held? rel) waits for the
;; acknowledgement, (release! rel) lets the process go on.
(define (hold-env stage rel) (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='" stage ":" rel "' "))
(define (held? rel)
  (let loop ((i 0))
    (cond ((file-exists? (string-append rel ".held")) #t)
          ((>= i 300) #f)
          (else (sh "sleep 0.05") (loop (+ i 1))))))
(define (release! rel) (sh (string-append "touch " rel)))

(define (fresh-dir name)
  (let ((d (string-append sock-here "/" name)))
    (sh (string-append "rm -rf " d "; mkdir -p " d))
    d))

;; A marker as a daemon writes it, for the rows that build a layout by hand.
(define (write-marker! dir identity)
  (sh (string-append "mkdir -p " dir))
  (call-with-output-file (string-append dir "/.endpoint")
    (lambda (p) (write `(endpoint (identity ,identity) (pid 1) (directory "x")) p) (newline p))
    'replace))

(define (identity-of dir base) (string-append (real-path dir) "/" base))

;; -> the backing directories `<base>.d.<digits>[-<n>]` in DIR, the base
;; compared exactly, or ignoring case when FOLD is given.
(define (backings dir base . fold)
  (let ((prefix (string-append base ".d.")))
    (filter (lambda (n) (and (> (string-length n) (string-length prefix))
                             ((if (pair? fold) string-ci=? string=?)
                              (substring n 0 (string-length prefix)) prefix)
                             (for-all (lambda (c) (or (char-numeric? c) (char=? c #\-)))
                                      (string->list (substring n (string-length prefix) (string-length n))))))
            (let ((l (names dir))) (if (list? l) l '())))))

;; ---- CONTROL: the product loads and serves ------------------------------------------
;;
;; Before any scenario, one daemon is started and asked. A product that does
;; not load (an unbound identifier, say) then reads as this one red line
;; naming what the daemon wrote, and the run ends, instead of every later row
;; reading timeout.
(let* ((d (fresh-dir "c0"))
       (pid (start! "c0" d "s" "" store))
       (said (await "c0"))
       (got (if (eq? said 'serving)
                (list said (ask (string-append d "/s")))
                (list said (stray-line (file-text (string-append here "/c0.out")))))))
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
              (let loop ((i 0))
                (if (and (< i 100) (string=? "" (file-text pidf))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
              (started! (string->number (let ((t (file-text pidf))) (substring t 0 (max 0 (- (string-length t) 1))))))))
       (true-id (let ((k (assv pid identities))) (and k (cdr k)))))
  (set! identities (cons (cons pid (string-append (or true-id "") " (altered)")) identities))
  (let* ((wrong (signal! pid "TERM"))
         (still (alive? pid)))
    (set! identities (cdr identities))
    (let* ((right (signal! pid "TERM"))
           (gone (wait-gone pid)))
      (want "CONTROL signal! refuses a pid whose recorded start differs, then signals it under its true one"
            (list (string? true-id) wrong still right gone)
            '(#t already-gone #t sent #t)))))

;; ---- W3b: the two options' values ------------------------------------------------
(define (serve-now dir . args)
  (let ((r (serve-bounded "bn" dir store (string-append dir "/s") (string-append " " (apply string-append args)))))
    (list (first-said r 5) (cadr r) (kind (string-append dir "/.s.lock")))))

(let ((d (fresh-dir "b")))
  (want "W3b --idle-exit -1: refused not-a-count, exit 1, before the lock"
        (serve-now d "--idle-exit -1")
        '((error bad-request (reason not-a-count) (option "--idle-exit") (value "-1")) 1 absent))
  (want "W3b --idle-exit 1.5: refused the same way"
        (serve-now d "--idle-exit 1.5")
        '((error bad-request (reason not-a-count) (option "--idle-exit") (value "1.5")) 1 absent))
  (want "W3b --idle-exit abc: refused the same way"
        (serve-now d "--idle-exit abc")
        '((error bad-request (reason not-a-count) (option "--idle-exit") (value "abc")) 1 absent))
  (want "W3b --idle-grace-ms -5: refused the same way"
        (serve-now d "--idle-grace-ms -5")
        '((error bad-request (reason not-a-count) (option "--idle-grace-ms") (value "-5")) 1 absent))
  (let* ((pid (begin
                (sh (string-append "rm -f " here "/b1.out"))
                (start! "b1" d "s --idle-exit 0 --idle-grace-ms 0" "" store)))
         (said (await "b1")))
    (want "W3b --idle-exit 0 --idle-grace-ms 0: serving, and answers through the link"
          (list said (ask (string-append d "/s")))
          '(serving ok))
    (stop! pid)))

;; ---- W3e: the two length predicates (the daemon's half) -----------------------------
;;
;; A start needs room for `.d/` and ten digits after the public path; a client
;; connecting needs only the public path to fit.
(let* ((d (fresh-dir "e"))
       (room (- (sun-path-max) 13 (string-length (string-append d "/"))))
       (refused-name (make-string room #\p))
       (served-name (make-string (- room 1) #\q))
       (refused (string-append d "/" refused-name))
       (served (string-append d "/" served-name)))
  (let ((r (serve-bounded "e0" d store refused "")))
    (want "W3e a public path with no room for the private suffix: serve refuses socket-path-too-long, exit 2"
          (list (first-said r 2) (cadr r))
          '((error socket-path-too-long) 2))
    (want "W3e and nothing was made: no lock file, no link, no namespace"
          (names d)
          '()))
  ;; Companion of "W3e a public path with no room ...": the same path passes the
  ;; client's predicate; no product change in this item turns it red alone.
  (want "W3e a client's read at that public path is answered (the public predicate passed), not path-too-long"
        (ask-any refused)
        'ok)
  (let* ((pid (start! "e1" d served "" store))
         (said (await "e1")))
    (want "W3e one byte shorter: both predicates pass, the daemon serves through the link"
          (list said (ask served) (and (link served) #t))
          '(serving ok #t))
    (stop! pid))
  ;; THE STARTER'S CHECK, DRIVEN DIRECTLY: ensure-daemon! is what the shell
  ;; calls. Something listening at the public path is reached whatever room is
  ;; left after it; with nothing there the start is refused before a spawn,
  ;; naming as max the longest path that serves.
  (let* ((drv (string-append here "/e-drive.sc"))
         (_ (call-with-output-file drv
              (lambda (p)
                (for-each (lambda (l) (put-string p l) (newline p))
                  (list "(import (chezscheme) (theourgia client))"
                        (string-append "(define sock \"" refused "\")")
                        (string-append "(write (list 'driver (ensure-daemon! (list \"scheme\" \"--script\" \"" tree
                                       "/theourgiad.sc\" \"serve\" \"" store "\" \"--socket\" sock) \""
                                       store "\" sock)))"))))
              'replace))
         ;; -> (<the driver's result> . <how many other data were printed>):
         ;; a daemon spawned without --detach shares the driver's output, so
         ;; its own refusal is told apart from the driver's, and counted.
         (drive (lambda ()
                  (let* ((ds (datums (sh-out (string-append env-text "scheme --script " drv
                                                            " < /dev/null 2>&1"))))
                         (mine (find (lambda (d) (eq? (car d) 'driver)) ds)))
                    (if (and mine (pair? (cdr mine)))
                        (cons (cadr mine) (- (length ds) 1))
                        (cons 'nothing (length ds))))))
         (lpid (begin (sh (string-append "python3 -c \"import socket,sys,time; s=socket.socket(socket.AF_UNIX);"
                                         " s.bind(sys.argv[1]); s.listen(8); time.sleep(60)\" " refused
                                         " > /dev/null 2>&1 & echo $! > " here "/e-listener.pid"))
                      (let loop ((i 0))
                        (if (and (< i 100) (not (eq? (kind refused) 'other)))
                            (begin (sh "sleep 0.05") (loop (+ i 1)))))
                      (started! (string->number
                                  (let ((t (file-text (string-append here "/e-listener.pid"))))
                                    (substring t 0 (max 0 (- (string-length t) 1))))))))
         (answer-there (drive))
         (_ (kill! lpid))
         (_ (sh (string-append "rm -f " refused)))
         (answer-none (drive)))
    (want "W3e ensure-daemon! there with something listening: ready (connect needs the public path only)"
          answer-there
          '(ready . 0))
    (want "W3e and with nothing there: refused before a spawn, its max the length of the path that serves"
          (let ((r (car answer-none)))
            (if (pair? r)
                (list (car r) (cadr r)
                      (let ((m (assq 'max (cddr r))))
                        (and m (= (cadr m) (bytevector-length (string->utf8 served)))))
                      (cdr answer-none))
                answer-none))
          '(error socket-path-too-long #t 0))))

;; ---- W5o: a relative socket path -------------------------------------------------
(let* ((d (fresh-dir "o"))
       (_ (sh (string-append "mkdir -p " d "/run")))
       (pid (start! "o1" d "run/s" "" store))
       (said (await "o1"))
       (pt (number->string pid)))
  (want "W5o --socket run/s: the link run/s reads s.d/<pid>, relative to its own directory"
        (list said (link (string-append d "/run/s")) (link (string-append d "/run/s.d")))
        (list 'serving (string-append "s.d/" pt) (string-append "s.d." pt)))
  (want "W5o a client connects through it from another directory"
        (ask (string-append d "/run/s"))
        'ok)
  (want "W5o the marker names the endpoint by its resolved parent and base name"
        (let ((ds (datums (file-text (string-append d "/run/s.d." pt "/.endpoint")))))
          (and (pair? ds) (assq 'identity (cdar ds))))
        (list 'identity (identity-of (string-append d "/run") "s")))
  (stop! pid)
  (want "W5o a controlled exit removes the link and the private socket, and leaves the marked namespace"
        (list (kind (string-append d "/run/s")) (names (string-append d "/run/s.d." pt))
              (link (string-append d "/run/s.d")))
        (list 'absent '(".endpoint") (string-append "s.d." pt))))

(let* ((d (fresh-dir "or"))
       (pid (start! "or1" d #f "THEOURGIA_RUN=rr " store))
       (said (await "or1"))
       (pt (number->string pid))
       (dirs (let ((l (names (string-append d "/rr")))) (if (list? l) l '())))
       (key-dir (and (pair? dirs) (string-append d "/rr/" (car dirs)))))
  (want "W5o THEOURGIA_RUN relative: the default socket is a link to socket.d/<pid>"
        (list said (and key-dir (link (string-append key-dir "/socket"))))
        (list 'serving (string-append "socket.d/" pt)))
  (want "W5o and a client reaches it through the absolute path"
        (and key-dir (ask (string-append key-dir "/socket")))
        'ok)
  (stop! pid))

;; ---- W5p: a dangling public link ----------------------------------------------------
;;
;; SIGTERM while a request is held running: the listener closes, which
;; removes the private socket, and the drain waits for the request; then
;; SIGKILL. The public link is left pointing at nothing.
;; (hold-a-write! sock rel): an insert through SOCKET, then a write of that
;; block in the background, which a daemon started with the write-after-create
;; hold armed at REL holds running; -> whether the hold was reached.
(define (hold-a-write! sock rel)
  (let* ((inserted (sh-out (string-append "cd " here " && " env-text "scheme --script " tree
                                          "/theourgia.sc insert --wire --store " store " --socket " sock
                                          " --title p --text p < /dev/null")))
         (id (let ((ds (datums inserted)))
               (and (pair? ds) (let ((st (assq 'state (cdar ds)))) (and st (car (caadr st)))))))
         (writer (let ((ds (datums inserted)))
                   (and (pair? ds) (let ((ev (assq 'events (cdar ds)))) (and ev (car (caadr ev))))))))
    (sh (string-append "( cd " here " && " env-text "scheme --script " tree "/theourgia.sc write --wire --store "
                       store " --socket " sock " --writer " (or writer "none") " " (or id "none")
                       " held > /dev/null 2>&1 < /dev/null & )"))
    (held? rel)))

(let* ((d (fresh-dir "p"))
       (rel (string-append here "/p-rel"))
       (pid (start! "p1" d "s" (hold-env "write-after-create" rel) store))
       (said (await "p1"))
       (sock (string-append d "/s")))
  (hold-a-write! sock rel)
  (signal! pid "TERM")
  (sh "sleep 1")
  (kill! pid)
  (want "W5p the public link is left dangling (its private socket closed with the listener)"
        (list said (and (link sock) #t) (guard (e (#t 'raised)) (file-exists? sock)))
        '(serving #t #f))
  (release! rel)
  (let* ((pid2 (start! "p2" d "s" "" store))
         (said2 (await "p2")))
    (want "W5p a new daemon reclaims the dangling link, binds, publishes and serves"
          (list said2 (link sock) (ask sock))
          (list 'serving (string-append "s.d/" (number->string pid2)) 'ok))
    (stop! pid2)))

;; ---- W5p2: the publish after the bind collides --------------------------------------
;;
;; Held at the bind, after the start cleared the public path: an entry made
;; there now is not replaced. The listener's publish is refused and traced,
;; its private socket unlinked, and the daemon exits listener-down, 75.
(let* ((d (fresh-dir "pp"))
       (rel (string-append here "/pp-rel"))
       (pid (launch! "pp" d store "s" "" (string-append "THEOURGIA_TRACE=1 " (hold-env "bind" rel))))
       (reached (held? rel))
       (pt (number->string pid))
       (_ (sh (string-append "echo mine > " d "/s")))
       (_ (release! rel))
       (rc (rc-wait "pp"))
       (text (file-text (string-append here "/pp.out"))))
  (want "W5p2 a publish refused after the bind: traced publish-refused, then listener-down, exit 75"
        (list reached rc (text-has? text "(trace publish-refused (path s) (reason exists))")
              (text-has? text "(exiting (reason listener-down))"))
        '(#t 75 #t #t))
  (want "W5p2 the entry found at the socket path is kept, and no private socket is left"
        (list (kind (string-append d "/s")) (file-text (string-append d "/s")) (names (string-append d "/s.d." pt)))
        '(regular "mine\n" (".endpoint"))))

;; ---- W5p3: the publish after the bind fails outright ----------------------------------
;;
;; The socket's directory made unwritable during the bind hold: symlink(2)
;; fails EACCES. Refused and traced the same way, its errno the reason, and
;; the private socket unlinked.
(let* ((d (fresh-dir "p3"))
       (rel (string-append here "/p3-rel"))
       (pid (launch! "p3" d store "s" "" (string-append "THEOURGIA_TRACE=1 " (hold-env "bind" rel))))
       (reached (held? rel))
       (pt (number->string pid))
       (_ (sh (string-append "chmod 500 " d)))
       (_ (release! rel))
       (rc (rc-wait "p3"))
       (_ (sh (string-append "chmod 700 " d)))
       (text (file-text (string-append here "/p3.out"))))
  (want "W5p3 a publish that fails EACCES after the bind: traced with reason 13, listener-down, exit 75"
        (list reached rc (text-has? text "(trace publish-refused (path s) (reason 13))")
              (text-has? text "(exiting (reason listener-down))"))
        '(#t 75 #t #t))
  (want "W5p3 and the private socket is unlinked: only the marker is left in the namespace"
        (list (names (string-append d "/s.d." pt)) (kind (string-append d "/s")))
        '((".endpoint") absent)))

;; ---- W5m: a residue at the restarting daemon's own pid --------------------------------
;;
;; A daemon SIGKILLed leaves its private socket, its marker and both links. The
;; fixture then makes the next daemon's pid equal the dead one's: the shell
;; that becomes it renames the residue to `s.d.$$`, its private socket to
;; `$$`, rewrites the marker's pid and spelling, and points both links there.
;; Inside it a second stale socket, a plain file `notes`, and `peer`, a link to
;; the live neighbour's socket: by lstat a link, so it is not a socket of the
;; residue. A live neighbour serves `s.123` from the same directory throughout.
(define (stale-socket! p)
  (sh (string-append "python3 -c \"import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])\" " p)))

(define (replaced text old new)
  (let ((n (string-length old)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i n) (string-length text))
             (apply string-append (reverse (cons (substring text i (string-length text)) out))))
            ((string=? (substring text i (+ i n)) old) (loop (+ i n) (cons new out)))
            (else (loop (+ i 1) (cons (string (string-ref text i)) out)))))))

(let* ((d (fresh-dir "m"))
       (n (start! "m0" d "s.123" "" store2))
       (n-said (await "m0"))
       (nt (number->string n))
       (a (start! "m1" d "s" "" store))
       (a-said (await "m1"))
       (at (number->string a))
       (_ (kill! a))
       (residue (string-append d "/s.d." at))
       (_ (stale-socket! (string-append residue "/x")))
       (_ (sh (string-append "echo keep > " residue "/notes")))
       (peer-target (string-append d "/s.123.d/" nt))
       (_ (sh (string-append "ln -s " peer-target " " residue "/peer")))
       (tmpl (string-append here "/m-marker"))
       (_ (call-with-output-file tmpl
            (lambda (p)
              (put-string p (replaced (replaced (file-text (string-append residue "/.endpoint"))
                                                (string-append "(pid " at ")") "(pid PIDX)")
                                      (string-append "\"s.d." at "\"") "\"s.d.PIDX\"")))
            'replace))
       (n-before (ask (string-append d "/s.123") store2))
       (b (start-over! "m2" d (string-append "mv s.d." at " s.d.\\$\\$ && mv s.d.\\$\\$/" at " s.d.\\$\\$/\\$\\$"
                                             " && sed s/PIDX/\\$\\$/g " tmpl " > s.d.\\$\\$/.endpoint"
                                             " && rm s.d s && ln -s s.d.\\$\\$ s.d && ln -s s.d/\\$\\$ s")))
       (b-said (await "m2"))
       (bt (number->string b)))
  (want "W5m the residue at the restarting pid, with stale sockets: the start serves at <socket>.d.<pid>-1"
        (list n-said a-said b-said (link (string-append d "/s.d")) (link (string-append d "/s"))
              (ask (string-append d "/s")))
        (list 'serving 'serving 'serving (string-append "s.d." bt "-1") (string-append "s.d/" bt) 'ok))
  (want "W5m every stale socket in the residue is gone; its marker and notes are kept, and so is the directory"
        (names (string-append d "/s.d." bt))
        '(".endpoint" "notes" "peer"))
  (want "W5m the link in the residue to the neighbour's live socket is kept, and the neighbour answers through it"
        (list (link (string-append d "/s.d." bt "/peer")) (ask (string-append d "/s.d." bt "/peer") store2))
        (list peer-target 'ok))
  (want "W5m the live neighbour <socket>.123 stays served throughout, its own namespace untouched"
        (list n-before (ask (string-append d "/s.123") store2) (link (string-append d "/s.123"))
              (link (string-append d "/s.123.d")))
        (list 'ok 'ok (string-append "s.123.d/" nt) (string-append "s.123.d." nt)))
  (stop! b)
  (stop! n))

(let* ((d (fresh-dir "m3"))
       (_ (sh (string-append "echo x > " d "/s.d")))
       (r (serve-bounded "m3" d store "s" "")))
  ;; The socket path reads absent: nothing was there to reclaim.
  (want "W5m a plain file at <socket>.d: serve-path-occupied naming it, nothing deleted"
        (list (first-said r 3) (kind (string-append d "/s.d")) (file-text (string-append d "/s.d"))
              (backings d "s") (kind (string-append d "/s")))
        (list '(error serve-path-occupied (path "s.d")) 'regular "x\n" '() 'absent)))

(let* ((d (fresh-dir "m4"))
       (_ (stale-socket! (string-append d "/s.d")))
       (r (serve-bounded "m4" d store "s" "")))
  (want "W5m a foreign socket at <socket>.d: the same, the socket kept"
        (list (first-said r 3) (kind (string-append d "/s.d")) (backings d "s") (kind (string-append d "/s")))
        (list '(error serve-path-occupied (path "s.d")) 'other '() 'absent)))

;; ---- W5l6: a forced exit whose private unlink fails ------------------------------------
;;
;; A write held running, the daemon's own directory made unwritable, then two
;; SIGTERMs: the second exits at once with 75. The private socket cannot be
;; unlinked, by the listener's close or by `leave`, and `leave` logs it; the
;; public link, in a writable directory, is unlinked. With the directory
;; writable again the next start sweeps the stale socket and serves.
(let* ((d (fresh-dir "l6"))
       (rel (string-append here "/l6-rel"))
       (pid (launch! "l6" d store "s" "" (hold-env "write-after-create" rel)))
       (said (await "l6"))
       (pt (number->string pid))
       (backing (string-append d "/s.d." pt))
       (reached (hold-a-write! (string-append d "/s") rel)))
  (sh (string-append "chmod 500 " backing))
  (signal! pid "TERM")
  (sh "sleep 0.5")
  (signal! pid "TERM")
  (let* ((gone (let loop ((i 0))
                 (cond ((not (alive? pid)) #t)
                       ((>= i 40) #f)
                       (else (sh "sleep 0.05") (loop (+ i 1))))))
         (_ (let loop ((i 0)) (if (and (< i 40) (not (rc-of "l6"))) (begin (sh "sleep 0.05") (loop (+ i 1))))))
         (ds (datums (file-text (string-append here "/l6.out"))))
         (private-left (kind (string-append backing "/" pt))))
    (sh (string-append "chmod 700 " backing))
    (release! rel)
    (want "W5l6 the second signal exits at once with 75, and leave logs cleanup-failed for the private socket"
          (list said reached gone (rc-of "l6")
                (and (member '(exiting (reason second-signal)) ds) #t)
                (find (lambda (x) (eq? (car x) 'cleanup-failed)) ds))
          (list 'serving #t #t 75 #t (list 'cleanup-failed (string-append "s.d/" pt) 13)))
    (want "W5l6 the public link is gone, the private socket remains"
          (list (kind (string-append d "/s")) private-left)
          '(absent other))
    (let* ((pid2 (start! "l6b" d "s" "" store))
           (said2 (await "l6b")))
      (want "W5l6 the next start sweeps the stale socket and serves"
            (list said2 (kind (string-append backing "/" pt)) (ask (string-append d "/s")))
            '(serving absent ok))
      (stop! pid2))))

;; ---- W5l7: the private name replaced while serving -------------------------------------
;;
;; The daemon's private socket removed and ANOTHER SOCKET put at its name (a
;; different inode, so lstat alone cannot tell): leave's own unlink leaves it,
;; since it is not the socket bound. EVERY DRAIN NOW CLOSES THE LISTENER FIRST
;; and waits for it, and that close unlinks the bound name whatever is there
;; (the README says so), so an ordinary drain never reaches leave with the
;; planted socket in place. The one path where leave's check still decides is
;; a forced exit before the listener has closed: the drain's stop held at
;; listener-before-stop, then the second signal, which exits at once.
(let* ((d (fresh-dir "l7"))
       (rel (string-append here "/l7-rel"))
       (pid (launch! "l7" d store "s" "" (hold-env "listener-before-stop" rel)))
       (said (await "l7"))
       (pt (number->string pid))
       (private (string-append d "/s.d." pt "/" pt)))
  (sh (string-append "rm -f " private))
  (stale-socket! private)
  (let* ((inode (lambda () (sh-out (string-append "python3 -c \"import os,sys; print(os.lstat(sys.argv[1]).st_ino)\" "
                                                   private " 2>/dev/null"))))
         (planted (inode))
         (_ (signal! pid "TERM"))
         (held (held? rel))
         (_ (signal! pid "TERM"))
         (gone (let loop ((i 0))
                 (cond ((not (alive? pid)) #t)
                       ((>= i 40) #f)
                       (else (sh "sleep 0.05") (loop (+ i 1))))))
         (_ (let loop ((i 0)) (if (and (< i 40) (not (rc-of "l7"))) (begin (sh "sleep 0.05") (loop (+ i 1)))))))
    (release! rel)
    (want "W5l7 a forced exit before the listener closed leaves another socket put at its private name, and still unlinks its public link"
          (list said held gone (rc-of "l7") (kind private) (string=? planted (inode)) (kind (string-append d "/s")))
          '(serving #t #t 75 other #t absent))))

;; ---- W5l7b: a forced exit before the listener closed, nothing planted -----------------
;;
;; W5l7's frame with the private socket left as the daemon bound it: the drain's
;; stop held at listener-before-stop, then the second signal, which exits at once.
;; The listener's close never ran, so `leave`'s own unlink is the only one that
;; removes the private socket; it is the socket bound, so it is unlinked.
(let* ((d (fresh-dir "l7b"))
       (rel (string-append here "/l7b-rel"))
       (pid (launch! "l7b" d store "s" "" (hold-env "listener-before-stop" rel)))
       (said (await "l7b"))
       (pt (number->string pid))
       (private (string-append d "/s.d." pt "/" pt)))
  (let* ((before (kind private))
         (_ (signal! pid "TERM"))
         (held (held? rel))
         (_ (signal! pid "TERM"))
         (gone (let loop ((i 0))
                 (cond ((not (alive? pid)) #t)
                       ((>= i 40) #f)
                       (else (sh "sleep 0.05") (loop (+ i 1))))))
         (_ (let loop ((i 0)) (if (and (< i 40) (not (rc-of "l7b"))) (begin (sh "sleep 0.05") (loop (+ i 1)))))))
    (release! rel)
    (want "W5l7b a forced exit before the listener closed, in a writable directory, unlinks its own private socket and its public link"
          (list said held gone (rc-of "l7b") (eq? before 'absent) (kind private) (kind (string-append d "/s")))
          '(serving #t #t 75 #f absent absent))))

;; ---- W5q: the namespace's reservation ------------------------------------------------
(let* ((d (fresh-dir "q"))
       (_ (sh (string-append "mkdir -p " d "/s.d")))
       (other (start! "q0" d (string-append d "/s.d/7") "" store2))
       (other-said (await "q0"))
       (pid (start! "q1" d "s" "" store))
       (said (await "q1")))
  (want "W5q a real <socket>.d with no marker, another daemon bound in it: serve-path-occupied naming it"
        (list other-said (head-of said 3))
        (list 'serving '(error serve-path-occupied (path "s.d"))))
  (want "W5q the other daemon's socket untouched and still served"
        (ask (string-append d "/s.d/7") store2)
        'ok)
  (stop! other))

(let* ((d (fresh-dir "q2"))
       (pid (start! "q2a" d "s" "" store))
       (_ (await "q2a"))
       (_ (kill! pid))
       (old (string-append "s.d." (number->string pid)))
       (left (names (string-append d "/" old)))
       (pid2 (start! "q2b" d "s" "" store))
       (said (await "q2b")))
  (want "W5q the supported layout with a stale socket: the link reclaimed, the residue swept, a fresh directory"
        (list left said (kind (string-append d "/" old)) (link (string-append d "/s.d")) (ask (string-append d "/s")))
        (list (list ".endpoint" (number->string pid)) 'serving 'absent
              (string-append "s.d." (number->string pid2)) 'ok))
  (stop! pid2))

(let* ((d (fresh-dir "q3"))
       (_ (write-marker! (string-append d "/s.d") (identity-of d "s")))
       (pid (start! "q3" d "s" "" store))
       (said (await "q3")))
  (want "W5q a REAL <socket>.d directory holding a matching marker: still refused"
        (list (head-of said 3) (names (string-append d "/s.d")))
        (list '(error serve-path-occupied (path "s.d")) '(".endpoint"))))

(let* ((d (fresh-dir "q4"))
       (_ (write-marker! (string-append d "/s.d.9") (identity-of d "other")))
       (_ (sh (string-append "ln -s s.d.9 " d "/s.d")))
       (pid (start! "q4" d "s" "" store))
       (said (await "q4")))
  (want "W5q a link to a directory whose marker names another endpoint: refused, both left"
        (list (head-of said 3) (link (string-append d "/s.d")) (names (string-append d "/s.d.9")))
        (list '(error serve-path-occupied (path "s.d")) "s.d.9" '(".endpoint"))))

(let* ((d (fresh-dir "q5"))
       (_ (sh (string-append "mkdir -p " d "/s.d.7 && ln -s s.d.7 " d "/s.d")))
       (b (start! "q5b" d (string-append d "/s.d.7/8") "" store2))
       (b-said (await "q5b"))
       (pid (start! "q5" d "s" "" store))
       (said (await "q5")))
  (want "W5q a link to an UNMARKED directory holding a live neighbour: refused, link and target unchanged"
        (list b-said (head-of said 3) (link (string-append d "/s.d")) (kind (string-append d "/s.d.7/.endpoint")))
        (list 'serving '(error serve-path-occupied (path "s.d")) "s.d.7" 'absent))
  (want "W5q the neighbour still served"
        (ask (string-append d "/s.d.7/8") store2)
        'ok)
  (stop! b))

;; ---- W5q6: a marked directory the start cannot sweep ------------------------------
;;
;; A dead daemon's directory, made unwritable, with a file planted in it. When
;; `<socket>.d` names it, the start unlinks the link, fails the directory's
;; first unlink, puts the link back and is refused naming it: nothing is
;; removed. When no link names it, it is a sibling: left, and the start serves.
(define (dead-and-locked tag)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (at (number->string a))
         (_ (kill! a))
         (backing (string-append d "/s.d." at)))
    (sh (string-append "echo planted > " backing "/keep"))
    (list d at backing (file-text (string-append backing "/.endpoint")))))

(let* ((setup (dead-and-locked "q6"))
       (d (car setup)) (at (cadr setup)) (backing (caddr setup)) (marker (cadddr setup))
       (_ (sh (string-append "chmod 500 " backing)))
       (r (serve-bounded "q6" d store "s" ""))
       (after (list (names backing) (file-text (string-append backing "/.endpoint"))
                    (file-text (string-append backing "/keep")))))
  (sh (string-append "chmod 700 " backing))
  ;; The socket path reads absent: the dead daemon's publication there was
  ;; this endpoint's own, reclaimed before the namespace was judged, not
  ;; deleted by the refusal.
  (want "W5q6 the link's directory cannot be swept: refused serve-path-occupied naming the link, the link put back"
        (list (first-said r 3) (cadr r) (link (string-append d "/s.d")) (kind (string-append d "/s")))
        (list '(error serve-path-occupied (path "s.d")) 75 (string-append "s.d." at) 'absent))
  (want "W5q6 and nothing inside it was removed: the stale socket, the marker's text and the planted file"
        after
        (list (list ".endpoint" at "keep") marker "planted\n")))

(let* ((setup (dead-and-locked "q7"))
       (d (car setup)) (at (cadr setup)) (backing (caddr setup))
       (_ (sh (string-append "rm " d "/s.d && chmod 500 " backing)))
       (b (start! "q7b" d "s" "" store))
       (said (await "q7b"))
       (bt (number->string b))
       (answer (list said (link (string-append d "/s.d")) (ask (string-append d "/s")) (names backing))))
  (sh (string-append "chmod 700 " backing))
  (want "W5q7 a marked sibling the start cannot sweep is left, and the start serves from its own directory"
        answer
        (list 'serving (string-append "s.d." bt) 'ok (list ".endpoint" at "keep")))
  (stop! b))

;; ---- W5q8..W5q10: siblings that cannot be read, a FIFO marker, case ----------------
(let* ((d (fresh-dir "q8"))
       (_ (sh (string-append "mkdir " d "/s.d.99 && chmod 000 " d "/s.d.99")))
       (a (start! "q8" d "s" "" store))
       (said (await "q8"))
       (answer (list said (ask (string-append d "/s")))))
  (sh (string-append "chmod 700 " d "/s.d.99"))
  (want "W5q8 a sibling that cannot be read is left, and the start serves"
        (append answer (list (kind (string-append d "/s.d.99"))))
        '(serving ok directory))
  (if (eq? said 'serving) (stop! a) (kill! a)))

(let* ((d (fresh-dir "q9"))
       (_ (sh (string-append "mkdir " d "/s.d.99 && mkfifo " d "/s.d.99/.endpoint")))
       (a (start! "q9" d "s" "" store))
       (said (await "q9"))
       (answer (list said (ask (string-append d "/s")) (kind (string-append d "/s.d.99/.endpoint")))))
  (want "W5q9 a FIFO named .endpoint in a sibling is not read: the start serves within the bound"
        answer
        '(serving ok other))
  (if (eq? said 'serving) (stop! a) (kill! a)))

;; CASE: a link spelled `s.d.N` to a directory spelled `S.d.N`, on a volume that
;; folds case, is the same directory; when it cannot be swept its link is put
;; back. The volume's folding is itself a row, so the case row cannot be
;; skipped in silence on a volume that was expected to fold.
(let* ((d (fresh-dir "qa"))
       (_ (sh (string-append "echo x > " d "/CaseProbe")))
       (inode (lambda (p) (sh-out (string-append "python3 -c \"import os,sys; print(os.stat(sys.argv[1]).st_ino)\" "
                                                 p " 2>/dev/null"))))
       (fold? (and (file-exists? (string-append d "/caseprobe"))
                   (equal? (inode (string-append d "/CaseProbe")) (inode (string-append d "/caseprobe"))))))
  (if (not fold?)
      (printf (string-append "SKIP W5q10 case: ~a does not fold case (CaseProbe and caseprobe are not one"
                             " inode); the row needs a case-insensitive volume\n")
              d)
      (let* ((setup (dead-and-locked "qb"))
             (d2 (car setup)) (at (cadr setup))
             (_ (sh (string-append "mv " d2 "/s.d." at " " d2 "/S.d." at " && chmod 500 " d2 "/S.d." at)))
             (r (serve-bounded "qb" d2 store "s" ""))
             (_ (sh (string-append "chmod 700 " d2 "/S.d." at))))
        (want "W5q10 CONTROL this volume folds case: CaseProbe and caseprobe are one inode" fold? #t)
        (want "W5q10 a link spelled s.d.N to S.d.N that cannot be swept: refused, the link put back as spelled"
              (list (first-said r 3) (cadr r) (link (string-append d2 "/s.d")))
              (list '(error serve-path-occupied (path "s.d")) 75 (string-append "s.d." at))))))

;; THE LINK'S DIRECTORY IS TRIED FIRST: with it unsweepable and twelve marked
;; siblings of the endpoint beside it, the refusal comes before any sibling
;; is touched. The siblings' readdir order is read, so the row says when
;; none came before the link's directory (then it could not tell).
(let* ((setup (dead-and-locked "qd"))
       (d (car setup)) (at (cadr setup)) (backing (caddr setup))
       (sibs (map (lambda (i) (string-append d "/s.d." (number->string (+ 1000 i)))) (iota 12)))
       (_ (for-each (lambda (sib)
                      (sh (string-append "mkdir " sib " && cp " backing "/.endpoint " sib "/.endpoint"))
                      (stale-socket! (string-append sib "/x")))
                    sibs))
       (_ (sh (string-append "chmod 500 " backing)))
       (before (string=? "yes\n"
                         (sh-out (string-append
                                   "python3 -c \"import os,sys; l=os.listdir(sys.argv[1]); i=l.index(sys.argv[2]);"
                                   " print('yes' if any(n.startswith('s.d.10') for n in l[:i]) else 'no')\" "
                                   d " s.d." at))))
       (r (serve-bounded "qd" d store "s" "")))
  (sh (string-append "chmod 700 " backing))
  (want "W5q11 the link's directory is tried first: refused before any of twelve marked siblings is swept"
        (list before (first-said r 3) (map (lambda (sib) (names sib)) sibs))
        (list #t '(error serve-path-occupied (path "s.d")) (map (lambda (sib) '(".endpoint" "x")) sibs))))

;; A PARENT THAT CANNOT BE LISTED after the start unlinked the namespace link:
;; the failure goes to main's table, and the link is put back.
(let* ((setup (dead-and-locked "qe"))
       (d (car setup)) (at (cadr setup)) (backing (caddr setup))
       (_ (sh (string-append "chmod 300 " d)))
       (r (serve-bounded "qe" d store (string-append d "/s") ""))
       (_ (sh (string-append "chmod 700 " d))))
  (want "W5q12 the parent cannot be listed: the start fails, the namespace link put back, its directory intact"
        (list (cadr r) (link (string-append d "/s.d")) (names backing))
        (list 75 (string-append "s.d." at) (list ".endpoint" at "keep"))))

;; ---- W5r: equivalent spellings -------------------------------------------------------
(let* ((d (fresh-dir "r"))
       (_ (sh (string-append "mkdir -p " d "/run")))
       (p1 (start! "r1" d "run/s" "" store))
       (s1 (await "r1"))
       (_ (stop! p1))
       (p2 (start! "r2" (string-append d "/run") "s" "" store))
       (s2 (await "r2"))
       (b2 (backings (string-append d "/run") "s"))
       (_ (stop! p2))
       (p3 (start! "r3" here (string-append d "/run/s") "" store))
       (s3 (await "r3"))
       (b3 (backings (string-append d "/run") "s")))
  (want "W5r run/s from the parent, s from run, then the absolute path: one endpoint, each restart sweeps"
        (list s1 s2 b2 s3 b3)
        (list 'serving 'serving (list (string-append "s.d." (number->string p2)))
              'serving (list (string-append "s.d." (number->string p3)))))
  (stop! p3))

(let* ((d (fresh-dir "rc"))
       (fold? (eq? #f (path-case-sensitive? d))))
  (if (not fold?)
      (printf "SKIP W5r case: ~a is on a case-sensitive volume; the case-insensitive half needs one that folds\n" d)
      (let* ((p1 (start! "rc1" d (string-append d "/S") "" store))
             (s1 (await "rc1"))
             (_ (stop! p1))
             (p2 (start! "rc2" d (string-append d "/s") "" store))
             (s2 (await "rc2"))
             (left (backings d "s" 'fold)))
        (want "W5r a case-insensitive volume: /dir/S then /dir/s are one endpoint, the restart sweeps and serves"
              (list s1 s2 left (ask (string-append d "/s")))
              (list 'serving 'serving (list (string-append "s.d." (number->string p2))) 'ok))
        (stop! p2))))

;; ON A CASE-SENSITIVE VOLUME the two are two endpoints. One is made for the
;; row from a disk image; where hdiutil cannot attach one, the row says so.
(let* ((img (string-append here "/cs.dmg"))
       (mnt (string-append sock-here "/cs")))
  (if (not (zero? (sh (string-append "hdiutil create -quiet -size 8m -fs 'Case-sensitive APFS' -volname cs "
                                     img " > /dev/null 2>&1 && mkdir -p " mnt
                                     " && hdiutil attach -quiet -nobrowse -mountpoint " mnt " " img
                                     " > /dev/null 2>&1"))))
      (printf "SKIP W5r case-sensitive: hdiutil could not make and attach a case-sensitive volume at ~a\n" mnt)
      (let* ((p1 (start! "rs1" mnt (string-append mnt "/S") "" store))
             (s1 (await "rs1"))
             (_ (stop! p1))
             (p2 (start! "rs2" mnt (string-append mnt "/s") "" store))
             (s2 (await "rs2"))
             (upper (backings mnt "S")))
        (want "W5r a case-sensitive volume: /dir/S and /dir/s are two endpoints, S's marked namespace left"
              (list s1 s2 upper (link (string-append mnt "/S.d")))
              (list 'serving 'serving (list (string-append "S.d." (number->string p1)))
                    (string-append "S.d." (number->string p1))))
        (stop! p2)
        ;; A lowercase `s.d.N` whose marker names the endpoint `S`: on this
        ;; volume another endpoint's, so `s`'s start leaves it.
        (let* ((l (string-append mnt "/l"))
               (_ (sh (string-append "mkdir -p " l)))
               (_ (write-marker! (string-append l "/s.d.77") (identity-of l "S")))
               (_ (stale-socket! (string-append l "/s.d.77/x")))
               (p3 (start! "rs3" l "s" "" store))
               (s3 (await "rs3")))
          (want "W5r a case-sensitive volume: a lowercase s.d.N marked for S is not s's, and is left"
                (list s3 (names (string-append l "/s.d.77")))
                '(serving (".endpoint" "x")))
          (if (eq? s3 'serving) (stop! p3) (kill! p3)))
        (sh (string-append "hdiutil detach -quiet " mnt " > /dev/null 2>&1")))))

;; ---- W5u: the reservation interrupted -------------------------------------------------
(let* ((d (fresh-dir "u"))
       (rel (string-append here "/u-rel"))
       (pid (start! "u1" d "s" (hold-env "namespace-before-publish" rel) store))
       (_ (held? rel))
       (_ (kill! pid))
       (residue (string-append "s.d." (number->string pid)))
       (state (list (names (string-append d "/" residue)) (kind (string-append d "/s.d"))))
       (pid2 (start! "u2" d "s" "" store))
       (said (await "u2")))
  (want "W5u killed after its directory and marker, before the link: the next start reclaims it and serves"
        (list state said (kind (string-append d "/" residue)) (ask (string-append d "/s")))
        (list '((".endpoint") absent) 'serving 'absent 'ok))
  (stop! pid2))

;; A marker name taken between the directory's creation and the marker's
;; write: the start is refused naming its directory, and leaves it as found.
(let* ((d (fresh-dir "uw"))
       (rel (string-append here "/uw-rel"))
       (pid (launch! "uw" d store "s" "" (hold-env "namespace-after-mkdir" rel)))
       (reached (held? rel))
       (pt (number->string pid))
       (backing (string-append d "/s.d." pt))
       (_ (sh (string-append "echo foreign > " backing "/.endpoint")))
       (_ (release! rel))
       (rc (rc-wait "uw"))
       (ds (datums (file-text (string-append here "/uw.out")))))
  (want "W5u7 a .endpoint planted after the mkdir: refused serve-path-occupied naming the directory, exit 75"
        (list reached (if (pair? ds) (head-of (car ds) 3) 'nothing) rc)
        (list #t `(error serve-path-occupied (path ,(string-append "s.d." pt))) 75))
  (want "W5u7 the planted marker is not written over, and nothing is published"
        (list (file-text (string-append backing "/.endpoint")) (names backing)
              (kind (string-append d "/s.d")) (kind (string-append d "/s")))
        '("foreign\n" (".endpoint") absent absent)))

;; THE PUBLISH FAILS and the directory is removed again: only while it is the
;; one this start made (W5u8), and also when the publish raises (W5u9) or
;; the marker's create does (W5u10).
(define (held-at-publish tag stage act)
  (let* ((d (fresh-dir tag))
         (rel (string-append here "/" tag "-rel"))
         (pid (launch! tag d store "s" "" (hold-env stage rel)))
         (reached (held? rel))
         (pt (number->string pid)))
    (act d pt)
    (release! rel)
    (let* ((rc (rc-wait tag))
           (ds (datums (file-text (string-append here "/" tag ".out")))))
      (list d pt reached rc (if (pair? ds) (head-of (car ds) 3) 'nothing)))))

(let* ((r (held-at-publish "u8" "namespace-before-publish"
            (lambda (d pt)
              (sh (string-append "cd " d " && mv s.d." pt " moved && mkdir s.d." pt " && echo x > s.d")))))
       (d (car r)) (pt (cadr r)))
  ;; THE REPLACEMENT IS EMPTY, so only the identity check keeps the rollback's
  ;; rmdir from removing it.
  (want "W5u8 an empty directory put in place of the allocated one before the publish is not removed by the rollback"
        (list (caddr r) (cadddr r) (car (cddddr r))
              (kind (string-append d "/s.d." pt)) (names (string-append d "/s.d." pt))
              (names (string-append d "/moved")))
        (list #t 75 '(error serve-path-occupied (path "s.d")) 'directory '() '(".endpoint"))))

(let* ((r (held-at-publish "u9" "namespace-before-publish"
            (lambda (d pt) (sh (string-append "chmod 500 " d)))))
       (d (car r)) (pt (cadr r)))
  (sh (string-append "chmod 700 " d))
  (want "W5u9 a publish that raises: the start fails, its marker removed (its parent is unwritable, so the dir stays)"
        (list (caddr r) (cadddr r) (names (string-append d "/s.d." pt)) (kind (string-append d "/s.d")))
        (list #t 75 '() 'absent)))

(let* ((r (held-at-publish "u10" "namespace-after-mkdir"
            (lambda (d pt) (sh (string-append "chmod 500 " d "/s.d." pt)))))
       (d (car r)) (pt (cadr r)))
  (want "W5u10 a marker whose create raises: the start fails, and the directory it made is removed"
        (list (caddr r) (cadddr r) (kind (string-append d "/s.d." pt)) (kind (string-append d "/s.d")))
        (list #t 75 'absent 'absent)))

(let* ((r (held-at-publish "u11" "namespace-after-mkdir"
            (lambda (d pt)
              (sh (string-append "cd " d " && mv s.d." pt " moved && mkdir s.d." pt))
              (stale-socket! (string-append d "/s.d." pt "/x")))))
       (d (car r)) (pt (cadr r)))
  (want "W5u11 a directory swapped in after the mkdir is not marked: refused naming it, and left as found"
        (list (caddr r) (cadddr r) (car (cddddr r)) (names (string-append d "/s.d." pt))
              (kind (string-append d "/s.d")))
        (list #t 75 `(error serve-path-occupied (path ,(string-append "s.d." pt))) '("x") 'absent)))

(let* ((r (held-at-publish "u12" "namespace-before-publish"
            (lambda (d pt)
              (sh (string-append "cd " d " && rm s.d." pt "/.endpoint && echo foreign > s.d." pt
                                 "/.endpoint && echo x > s.d")))))
       (d (car r)) (pt (cadr r)))
  (want "W5u12 a marker replaced before the publish is not removed by the rollback"
        (list (caddr r) (cadddr r) (car (cddddr r)) (file-text (string-append d "/s.d." pt "/.endpoint")))
        (list #t 75 '(error serve-path-occupied (path "s.d")) "foreign\n")))

(let* ((r (held-at-publish "u13" "namespace-before-publish"
            (lambda (d pt) (sh (string-append "chmod 600 " d)))))
       (d (car r)) (pt (cadr r)))
  (sh (string-append "chmod 700 " d))
  (want "W5u13 a publish that raises in an unsearchable parent: the report names the symlink, not the rollback"
        (list (caddr r) (cadddr r)
              (text-has? (file-text (string-append here "/u13.out")) "(op symlink)"))
        (list #t 75 #t)))

(let* ((d (fresh-dir "u3"))
       (_ (sh (string-append "mkdir -p " d "/s.d.7")))
       (b (start! "u3b" d (string-append d "/s.d.7/8") "" store2))
       (b-said (await "u3b"))
       (a (start! "u3a" d "s" "" store))
       (a-said (await "u3a")))
  (want "W5u a foreign s.d.7/ with a live endpoint 8 beside it: untouched, still served, the start not blocked"
        (list b-said a-said (ask (string-append d "/s.d.7/8") store2) (ask (string-append d "/s")))
        '(serving serving ok ok))
  (stop! b)
  (let* ((b2 (start! "u3c" d (string-append d "/s.d.7/8") "" store2))
         (b2-said (await "u3c")))
    (want "W5u that neighbour restarted while A serves: it takes A's free reservation lock and serves again"
          (list b2-said (ask (string-append d "/s.d.7/8") store2) (alive? a))
          '(serving ok #t))
    (stop! b2))
  (stop! a))

;; THE FIRST PROPOSED NAME IS TAKEN: the shell makes `s.d.$$` and then execs
;; the daemon, which keeps the shell's pid.

(let* ((d (fresh-dir "u4"))
       (pid (start-over! "u4" d "mkdir s.d.\\$\\$ && touch s.d.\\$\\$/keep"))
       (said (await "u4"))
       (pt (number->string pid)))
  (want "W5u a markerless directory at the first proposed name: s.d.<pid>-1 allocated, the occupant untouched"
        (list said (link (string-append d "/s.d")) (names (string-append d "/s.d." pt)) (ask (string-append d "/s")))
        (list 'serving (string-append "s.d." pt "-1") '("keep") 'ok))
  (stop! pid))

(let* ((d (fresh-dir "u5"))
       (_ (write-marker! (string-append here "/fm") "/elsewhere/s"))
       (pid (start-over! "u5" d (string-append "mkdir s.d.\\$\\$ && cp " here "/fm/.endpoint s.d.\\$\\$/.endpoint")))
       (said (await "u5"))
       (pt (number->string pid)))
  (want "W5u a foreign marked directory at that name: the same, the occupant untouched"
        (list said (link (string-append d "/s.d")) (names (string-append d "/s.d." pt)))
        (list 'serving (string-append "s.d." pt "-1") '(".endpoint")))
  (stop! pid))

(let* ((d (fresh-dir "u6"))
       (_ (sh (string-append "mkdir -p " d "/s.d")))
       (pid (start! "u6" d "s" "" store))
       (said (await "u6")))
  (want "W5u an unmarked real directory at <socket>.d: serve-path-occupied"
        (head-of said 3)
        '(error serve-path-occupied (path "s.d"))))

(let* ((d (fresh-dir "u7"))
       (rel (string-append here "/u7-rel"))
       (pid (start! "u7" d "s" (hold-env "namespace-before-publish" rel) store))
       (_ (held? rel))
       (_ (sh (string-append "mkdir " d "/s.d")))
       (_ (release! rel))
       (said (await "u7")))
  (want "W5u the race: a foreign <socket>.d made between the inspection and the publish is not replaced"
        (list (head-of said 3) (kind (string-append d "/s.d")) (names (string-append d "/s.d"))
              (backings d "s"))
        (list '(error serve-path-occupied (path "s.d")) 'directory '() '())))

;; ---- W5w: the reverse collision ----------------------------------------------------
(let* ((d (fresh-dir "w"))
       (a (start! "w1" d "s" "" store))
       (a-said (await "w1"))
       (at (number->string a))
       (via-link (serve-bounded "w2" d store2 (string-append d "/s.d/" at) ""))
       (via-dir (serve-bounded "w3" d store2 (string-append d "/s.d." at "/" at) ""))
       (refusal `(error serve-path-reserved (endpoint ,(identity-of d "s"))
                        (remedy serve-that-endpoint-or-remove-its-directory))))
  ;; B asks for A's own private name, so a takeover before the check would
  ;; reach A's live socket.
  (want "W5w serve --socket <A's namespace>/<A's pid>: refused serve-path-reserved naming A's endpoint"
        (list a-said (first-said via-link 4) (first-said via-dir 4))
        (list 'serving refusal refusal))
  (want "W5w no lock file of its own created, A's socket intact and A still serving"
        (list (names (string-append d "/s.d." at)) (ask (string-append d "/s")))
        (list (list ".endpoint" at) 'ok))
  (stop! a))

;; A `.endpoint` that is not a regular file reserves nothing: a start inside
;; that directory is admitted.
(let* ((d (fresh-dir "w4"))
       (_ (sh (string-append "mkdir -p " d "/s.d.5/.endpoint")))
       (b (start! "w4" d (string-append d "/s.d.5/7") "" store2))
       (said (await "w4")))
  (want "W5w2 a namespace-spelled parent whose .endpoint is a directory is not reserved: the start serves"
        (list said (ask (string-append d "/s.d.5/7") store2))
        '(serving ok))
  (if (eq? said 'serving) (stop! b) (kill! b)))

;; A BACKING DIRECTORY RENAMED `s.D.<pid>` on a folding volume still answers
;; through A's link; a start inside it is in A's namespace all the same.
(let* ((d (fresh-dir "w5"))
       (fold? (let ((probe (string-append d "/CaseProbe")))
                (sh (string-append "echo x > " probe))
                (file-exists? (string-append d "/caseprobe")))))
  (if (not fold?)
      (printf "SKIP W5w3 case: ~a does not fold case (CaseProbe/caseprobe); the row needs a folding volume\n" d)
      (let* ((a (start! "w5a" d "s" "" store))
             (a-said (await "w5a"))
             (at (number->string a))
             (_ (sh (string-append "mv " d "/s.d." at " " d "/s.D." at)))
             (r (serve-bounded "w5b" d store2 (string-append d "/s.d/" at) "")))
        (want "W5w3 a backing directory renamed s.D.<pid>: a start at s.d/<A's pid> is refused, and A still answers"
              (list a-said (first-said r 2) (ask (string-append d "/s")))
              '(serving (error serve-path-reserved) ok))
        (if (alive? a) (stop! a)))))

;; ---- W5x: the interleaving ------------------------------------------------------------
(let* ((d (fresh-dir "x"))
       (rel (string-append here "/x-rel"))
       (a (start! "x1" d "s" (hold-env "namespace-after-mkdir" rel) store))
       (_ (held? rel))
       (at (number->string a))
       (b (start! "x2" d (string-append d "/s.d." at "/7") "" store2))
       (_ (sh "sleep 1.5"))
       (b-waiting (list (alive? b) (file-text (string-append here "/x2.out"))))
       (_ (release! rel))
       (a-said (await "x1"))
       (b-said (await "x2")))
  (want "W5x B waits on A's reservation lock during A's window, and has said nothing"
        b-waiting
        '(#t ""))
  (want "W5x A completes and serves; B then reads the marker and is refused serve-path-reserved"
        (list a-said (head-of b-said 2) (ask (string-append d "/s")))
        '(serving (error serve-path-reserved) ok))
  (kill! a)
  (let* ((b2 (start! "x3" d (string-append d "/s.d." at "/7") "" store2))
         (b2-said (await "x3")))
    (want "W5x A dead, its marker left: B is still refused, with the remedy"
          b2-said
          `(error serve-path-reserved (endpoint ,(identity-of d "s"))
                  (remedy serve-that-endpoint-or-remove-its-directory) (attempt #f))))
  (sh (string-append "rm -rf " d "/s.d." at " " d "/s.d && mkdir " d "/s.d." at))
  (let* ((b3 (start! "x4" d (string-append d "/s.d." at "/7") "" store2))
         (b3-said (await "x4")))
    ;; Companion of "W5x A dead, its marker left ...": with the marker gone the same
    ;; path serves, so that refusal comes from the marker; no mutant turns it red alone.
    (want "W5x the operator removed it and made an ordinary directory there: B serves"
          (list b3-said (ask (string-append d "/s.d." at "/7") store2))
          '(serving ok))
    (stop! b3)))

;; ---- W5x5: the namespace republished while B waits ----------------------------------
;;
;; B resolves A's dead namespace, then is held before the reservation lock.
;; A restarts: it reclaims that directory and publishes a new one. B, let go,
;; reads the marker on its path resolved again under the lock, and is refused.
(let* ((d (fresh-dir "x5"))
       (a (start! "x5a" d "s" "" store))
       (_ (await "x5a"))
       (_ (kill! a))
       (rel (string-append here "/x5-rel"))
       (b (launch! "x5b" d store2 (string-append d "/s.d/7") "" (hold-env "reserve-after-resolve" rel)))
       (reached (held? rel))
       (a2 (start! "x5c" d "s" "" store))
       (a2-said (await "x5c"))
       (_ (release! rel))
       (rc (rc-wait "x5b"))
       (ds (datums (file-text (string-append here "/x5b.out")))))
  (want "W5x5 B, held after resolving, is refused serve-path-reserved by A's new marker, and A answers"
        (list reached a2-said (if (pair? ds) (head-of (car ds) 2) 'nothing) rc (ask (string-append d "/s")))
        '(#t serving (error serve-path-reserved) 75 ok))
  (stop! a2))

;; ---- W5x6: the namespace link gone while B waits ---------------------------------------
;;
;; B resolves A's dead namespace and is held before the lock. A restart is
;; killed after its reclaim unlinked `s.d`, so B, let go, finds its parent
;; resolving to nothing: not an unmarked parent, a missing one.
(let* ((d (fresh-dir "x6"))
       (a (start! "x6a" d "s" "" store))
       (_ (await "x6a"))
       (_ (kill! a))
       (rel (string-append here "/x6-rel"))
       (b (launch! "x6b" d store2 (string-append d "/s.d/7") "" (hold-env "reserve-after-resolve" rel)))
       (b-held (held? rel))
       (rel2 (string-append here "/x6-rel2"))
       (a2 (start! "x6c" d "s" (hold-env "reclaim-after-link" rel2) store))
       (a2-held (held? rel2))
       (_ (kill! a2))
       (_ (release! rel))
       (rc (rc-wait "x6b"))
       (ds (datums (file-text (string-append here "/x6b.out")))))
  (want "W5x6 B's parent no longer resolves under the lock: refused socket-dir-missing, not admitted"
        (list b-held a2-held (kind (string-append d "/s.d")) (if (pair? ds) (head-of (car ds) 3) 'nothing) rc)
        (list #t #t 'absent `(error socket-dir-missing (dir ,(string-append d "/s.d"))) 75)))

;; ---- W5x7: B past the starter's check while the endpoint's reclaim runs -------------
;;
;; B passed theourgiad's check of its directory (`s.d` was there) and is held
;; before the reservation check. A restart is held after its reclaim unlinked
;; `s.d`. B, let go, finds no parent: it must neither make a real `s.d` nor
;; skip the lock; it waits for A's reservation and reads A's new marker.
(let* ((d (fresh-dir "x7"))
       (a (start! "x7a" d "s" "" store))
       (_ (await "x7a"))
       (_ (kill! a))
       (rel (string-append here "/x7-rel"))
       (b (launch! "x7b" d store2 (string-append d "/s.d/7") "" (hold-env "serve-before-reserve-check" rel)))
       (b-held (held? rel))
       (rel2 (string-append here "/x7-rel2"))
       (a2 (start! "x7c" d "s" (hold-env "reclaim-after-link" rel2) store))
       (a2-held (held? rel2))
       (gap (kind (string-append d "/s.d")))
       (_ (release! rel))
       (_ (sh "sleep 1"))
       (_ (release! rel2))
       (a2-said (await "x7c"))
       (rc (rc-wait "x7b"))
       (ds (datums (file-text (string-append here "/x7b.out")))))
  (want "W5x7 B held past the starter's check, its parent gone meanwhile: refused serve-path-reserved, A serves"
        (list b-held a2-held gap a2-said (if (pair? ds) (head-of (car ds) 2) 'nothing) rc
              (kind (string-append d "/s.d")) (ask (string-append d "/s")))
        '(#t #t absent serving (error serve-path-reserved) 75 link ok))
  (if (alive? a2) (stop! a2)))

;; ---- W5x8: the same with the parent spelled `s.d//` and `s.d/.` ----------------------
(define (past-the-check tag tail)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (_ (kill! a))
         (rel (string-append here "/" tag "-rel"))
         (b (launch! (string-append tag "b") d store2 (string-append d tail) ""
                     (hold-env "serve-before-reserve-check" rel)))
         (b-held (held? rel))
         (rel2 (string-append here "/" tag "-rel2"))
         (a2 (start! (string-append tag "c") d "s" (hold-env "reclaim-after-link" rel2) store))
         (a2-held (held? rel2))
         (_ (release! rel))
         (_ (sh "sleep 1"))
         (_ (release! rel2))
         (a2-said (await (string-append tag "c")))
         (rc (rc-wait (string-append tag "b")))
         (ds (datums (file-text (string-append here "/" tag "b.out"))))
         (answer (list b-held a2-held a2-said (if (pair? ds) (head-of (car ds) 2) 'nothing) rc
                       (kind (string-append d "/s.d")))))
    (if (alive? a2) (stop! a2))
    answer))

(want "W5x8 the parent spelled s.d//: refused serve-path-reserved like s.d/"
      (past-the-check "x8" "/s.d//7")
      '(#t #t serving (error serve-path-reserved) 75 link))
(want "W5x8 and spelled s.d/./: the same"
      (past-the-check "x9" "/s.d/./7")
      '(#t #t serving (error serve-path-reserved) 75 link))

;; ---- W5x10: an alias of the namespace, dangling while the reclaim runs ---------------
;;
;; `alias` is a link to `s.d`, made before B starts and never changed. While
;; A's reclaim has `s.d` unlinked, `alias` does not resolve and its own
;; spelling names no namespace: B is refused, not admitted without the lock.
(want "W5x10 B at alias/7, the alias dangling during the reclaim: refused socket-dir-missing, A serves"
      (let* ((d (fresh-dir "xa"))
             (a (start! "xaa" d "s" "" store))
             (_ (await "xaa"))
             (_ (kill! a))
             (_ (sh (string-append "ln -s s.d " d "/alias")))
             (rel (string-append here "/xa-rel"))
             (b (launch! "xab" d store2 (string-append d "/alias/7") ""
                         (hold-env "serve-before-reserve-check" rel)))
             (b-held (held? rel))
             (rel2 (string-append here "/xa-rel2"))
             (a2 (start! "xac" d "s" (hold-env "reclaim-after-link" rel2) store))
             (a2-held (held? rel2))
             (_ (release! rel))
             (rc (rc-wait "xab"))
             (_ (release! rel2))
             (a2-said (await "xac"))
             (ds (datums (file-text (string-append here "/xab.out"))))
             (answer (list b-held a2-held
                           (and (pair? ds)
                                (equal? (head-of (car ds) 3)
                                        `(error socket-dir-missing (dir ,(string-append d "/alias")))))
                           rc a2-said (ask (string-append d "/s")))))
        (if (alive? a2) (stop! a2))
        answer)
      '(#t #t #t 75 serving ok))

;; ---- W5x11: a parent made through the namespace while the reclaim runs ---------------
;;
;; B's parent is spelled through A's link: `s.d/..` or `s.d/x`, both there
;; at the starter's check. While A's reclaim has `s.d` unlinked, the parent
;; does not resolve and is absent; making it would make a real `s.d`, and
;; A's publish would refuse from then on. B is refused, and A serves.
(define (made-through-the-namespace tag tail parent)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (_ (kill! a))
         (_ (sh (string-append "mkdir -p " d "/s.d/x")))
         (socket (string-append d "/s.d/" tail))
         (rel (string-append here "/" tag "-rel"))
         (b (launch! (string-append tag "b") d store2 socket ""
                     (hold-env "serve-before-reserve-check" rel)))
         (b-held (held? rel))
         (rel2 (string-append here "/" tag "-rel2"))
         (a2 (start! (string-append tag "c") d "s" (hold-env "reclaim-after-link" rel2) store))
         (a2-held (held? rel2))
         (_ (release! rel))
         (rc (rc-wait (string-append tag "b")))
         (_ (release! rel2))
         (a2-said (await (string-append tag "c")))
         (ds (datums (file-text (string-append here "/" tag "b.out"))))
         (answer (list b-held a2-held
                       (and (pair? ds)
                            (equal? (head-of (car ds) 3)
                                    `(error socket-dir-missing (dir ,(string-append d "/s.d/" parent)))))
                       rc a2-said (ask (string-append d "/s")))))
    (if (alive? b) (kill! b))
    (if (alive? a2) (stop! a2))
    answer))

(want "W5x11 B at s.d/../b, s.d unlinked by the reclaim: refused socket-dir-missing, no s.d made, A serves"
      (made-through-the-namespace "xb" "../b" "..")
      '(#t #t #t 75 serving ok))
(want "W5x11 and B at s.d/x/b: the same"
      (made-through-the-namespace "xc" "x/b" "x")
      '(#t #t #t 75 serving ok))

;; ---- W5x12: a relative socket under a working directory that was removed -------------
;;
;; The start's working directory is removed before it runs, so the walk's
;; floor `.` does not resolve. Measured on macOS in such a directory:
;; (real-path ".") is #f, and (entry-name-type ".") is `directory` (lstat
;; still finds it). Nothing below it can be made or bound: `s` (parent `.`)
;; is refused socket-dir-missing, exit 75. A deeper spelling such as `x/s`
;; never reaches this check: a removed directory holds nothing, so `x` is
;; absent and serve's own directory check refuses it first, exit 2
;; (measured).
(want "W5x12 a relative socket s under a removed working directory: refused socket-dir-missing"
      (let* ((d (fresh-dir "xd"))
             (b (launch! "xd" (string-append d " && rmdir \"$PWD\"") store2 "s" "" ""))
             (rc (rc-wait "xd"))
             (ds (datums (file-text (string-append here "/xd.out"))))
             (answer (list (and (pair? ds)
                                (equal? (head-of (car ds) 3) '(error socket-dir-missing (dir "."))))
                           rc (file-exists? d))))
        (if (alive? b) (kill! b))
        answer)
      '(#t 75 #f))

;; ---- W5x13, W5x14: B resolves once in A's window and is judged after A republished ----
;;
;; A2 is held before its reservation check, so its pid is known while `s.d`
;; still resolves and B passes serve's own directory check. A2 then unlinks
;; `s.d` and is held; B makes its one resolution then (it fails) and is held;
;; A2 republishes, binds and serves; only then is B judged. A second
;; resolution of B's parent would now succeed: `alias` reaches A2's new
;; backing, and `s.d` is that backing. B is refused, and A2's private socket,
;; its link and its backing are as A2 left them.
(define (judged-after-the-republish tag tail parent setup)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (_ (kill! a))
         (_ (sh (setup d)))
         (rel0 (string-append here "/" tag "-rel0"))
         (rel2 (string-append here "/" tag "-rel2"))
         (a2 (start! (string-append tag "c") d "s"
                     (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='serve-before-reserve-check:" rel0
                                    ";reclaim-after-link:" rel2 "' ")
                     store))
         (a2-held0 (held? rel0))
         (socket (string-append d "/" (tail a2)))
         (rel (string-append here "/" tag "-rel"))
         (rel3 (string-append here "/" tag "-rel3"))
         (b (launch! (string-append tag "b") d store2 socket ""
                     (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='serve-before-reserve-check:" rel
                                    ";reserve-after-first-resolve:" rel3 "' ")))
         (b-held (held? rel))
         (_ (release! rel0))
         (a2-held (held? rel2))
         (_ (release! rel))
         (b-held3 (held? rel3))
         (_ (release! rel2))
         (a2-said (await (string-append tag "c")))
         (_ (release! rel3))
         (rc (rc-wait (string-append tag "b")))
         (ds (datums (file-text (string-append here "/" tag "b.out"))))
         (private (string-append d "/s.d/" (number->string a2)))
         (answer (list a2-held0 b-held a2-held b-held3 a2-said
                       (and (pair? ds)
                            (equal? (head-of (car ds) 3)
                                    `(error socket-dir-missing (dir ,(string-append d "/" parent)))))
                       rc
                       (zero? (sh (string-append "[ -S " private " ] && [ ! -h " private " ]")))
                       (file-exists? (string-append d "/s.d/x"))
                       (ask (string-append d "/s")))))
    (if (alive? b) (kill! b))
    (if (alive? a2) (stop! a2))
    answer))

(want "W5x13 B at alias/<A2's pid>, resolved in A2's window, judged after its republish: refused socket-dir-missing, A2's socket kept"
      (judged-after-the-republish "xf" (lambda (pid) (string-append "alias/" (number->string pid))) "alias"
                                  (lambda (d) (string-append "ln -s s.d " d "/alias")))
      '(#t #t #t #t serving #t 75 #t #f ok))
(want "W5x14 B at s.d/x/b, the same: refused, nothing made inside A2's new backing"
      (judged-after-the-republish "xg" (lambda (pid) "s.d/x/b") "s.d/x"
                                  (lambda (d) (string-append "mkdir -p " d "/s.d/x")))
      '(#t #t #t #t serving #t 75 #t #f ok))

;; ---- W5x15, W5x16: a start makes its directory under what it judged ----------------
;;
;; B's parent is spelled through A's link, and B judges it while `s.d` is
;; there: `s.d/..` resolves at B's one look (W5x15), or `s.d/../new` is absent
;; and its first ancestor that is there, `s.d/..`, resolves (W5x16, the walk).
;; A2's reclaim then unlinks `s.d`, and only then does B make its directory.
;; Made by the spelling, that would make a real `s.d`, and A2's publish would
;; refuse. Made under what B resolved, nothing is made at `s.d`: B fails at its
;; lock file, and A2 publishes and serves.
(define (made-under-the-judged-ancestor tag tail walk?)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (_ (kill! a))
         (_ (if walk? (sh (string-append "mkdir " d "/new"))))
         (socket (string-append d "/s.d/" tail))
         (rel (string-append here "/" tag "-rel"))
         (rel4 (string-append here "/" tag "-rel4"))
         (b (launch! (string-append tag "b") d store2 socket ""
                     (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='"
                                    (if walk?
                                        (string-append "serve-before-reserve-check:" rel
                                                       ";serve-before-mkdir:" rel4)
                                        (string-append "reserve-after-first-resolve:" rel4))
                                    "' ")))
         (b-held (if walk?
                     (and (held? rel)
                          (zero? (sh (string-append "rmdir " d "/new")))
                          (begin (release! rel) (held? rel4)))
                     (held? rel4)))
         (rel2 (string-append here "/" tag "-rel2"))
         (a2 (start! (string-append tag "c") d "s" (hold-env "reclaim-after-link" rel2) store))
         (a2-held (held? rel2))
         (_ (release! rel4))
         (rc (rc-wait (string-append tag "b")))
         (_ (release! rel2))
         (a2-said (await (string-append tag "c")))
         (answer (list b-held a2-held rc (kind (string-append d "/s.d")) a2-said
                       (ask (string-append d "/s")))))
    (if (alive? b) (kill! b))
    (if (alive? a2) (stop! a2))
    answer))

(want "W5x15 B at s.d/../b, resolved while s.d is there, admitted during A2's reclaim: no s.d made, A2 serves"
      (made-under-the-judged-ancestor "xh" "../b" #f)
      '(#t #t 75 link serving ok))
(want "W5x16 B at s.d/../new/b, the walk's ancestor resolved while s.d is there: the same"
      (made-under-the-judged-ancestor "xi" "../new/b" #t)
      '(#t #t 75 link serving ok))

;; ---- W5x17: the walk's case rule is the resolved ancestor's ------------------------
;;
;; B's parent `s.d/../t.D/x` is there for serve's own directory check (as
;; `new` is in W5x16) and absent at the reservation check, and the walk
;; resolves its first ancestor that is there, `s.d/..`, while `s.d` is there.
;; (`t.D` is a name the walk judges, below the parent's base: spelled as the
;; parent's base it is judged by the owner check first.) A2's reclaim then
;; unlinks `s.d` before B reads the case rule. Read through the spelling,
;; that look fails, the rule reads as not folding, `t.D` is not
;; namespace-spelled, and B makes a real `t.D`: on a folding volume the
;; endpoint `t`'s namespace spelling, which `t`'s publish would refuse. Read
;; through the resolved ancestor, `t.D` is refused and nothing is made.
(let* ((d (fresh-dir "xj"))
       (fold? (let ((probe (string-append d "/CaseProbe")))
                (sh (string-append "echo x > " probe))
                (file-exists? (string-append d "/caseprobe")))))
  (if (not fold?)
      (printf "SKIP W5x17 case: ~a does not fold case (CaseProbe/caseprobe); the row needs a folding volume\n" d)
      (let* ((a (start! "xja" d "s" "" store))
             (_ (await "xja"))
             (_ (kill! a))
             (_ (sh (string-append "mkdir -p " d "/t.D/x")))
             (rel0 (string-append here "/xj-rel0"))
             (rel (string-append here "/xj-rel"))
             (b (launch! "xjb" d store2 (string-append d "/s.d/../t.D/x/b") ""
                         (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='"
                                        "serve-before-reserve-check:" rel0
                                        ";walk-after-resolve:" rel "' ")))
             (b-held (and (held? rel0)
                          (zero? (sh (string-append "rmdir " d "/t.D/x " d "/t.D")))
                          (begin (release! rel0) (held? rel))))
             (rel2 (string-append here "/xj-rel2"))
             (a2 (start! "xjc" d "s" (hold-env "reclaim-after-link" rel2) store))
             (a2-held (held? rel2))
             (_ (release! rel))
             (rc (rc-wait "xjb"))
             (_ (release! rel2))
             (a2-said (await "xjc"))
             (answer (list b-held a2-held rc (kind (string-append d "/t.D")) a2-said
                           (ask (string-append d "/s")))))
        (if (alive? b) (kill! b))
        (if (alive? a2) (stop! a2))
        (want "W5x17 B at s.d/../t.D/x/b on a folding volume, its walk's case rule read after A2's reclaim unlinked s.d: nothing made at t.D, A2 serves"
              answer
              '(#t #t 75 absent serving ok)))))

;; ---- W5x18: the endpoint's identity is taken from the one look --------------------
;;
;; B at `s.d/../b` judges its parent while `s.d` is there and takes its
;; lifetime lock and public entry; held before its identity, A2's reclaim
;; unlinks `s.d`; B takes its identity then, and is held again before its
;; reservation lock while A2 republishes and serves. An identity resolved at
;; that step, falling back to the spelling, is `<d>/s.d/../b`, and B's marker
;; names it: same-endpoint? compares parents as bytes, so a later start of
;; `<d>/b` finds b.d marked for another endpoint and is refused (W5x18), and
;; an earlier daemon's residue at `b` is judged not B's and refuses B
;; (W5x18b). Taken from the parent B resolved at its one look, the identity
;; is `<d>/b`: B serves, its marker names `<d>/b`, it reclaims the residue,
;; and a later start of `<d>/b` serves.
(define (identity-from-the-one-look tag residue?)
  (let* ((d (fresh-dir tag))
         (a (start! (string-append tag "a") d "s" "" store))
         (_ (await (string-append tag "a")))
         (_ (kill! a))
         (e (and residue? (start! (string-append tag "e") d "b" "" store2)))
         (_ (when e (await (string-append tag "e")) (kill! e)))
         (rel1 (string-append here "/" tag "-rel1"))
         (rel2 (string-append here "/" tag "-rel2"))
         (rel3 (string-append here "/" tag "-rel3"))
         (b (launch! (string-append tag "b") d store2 (string-append d "/s.d/../b") ""
                     (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='"
                                    "reserve-before-identity:" rel1
                                    ";reserve-after-identity:" rel2 "' ")))
         (b-held1 (held? rel1))
         (a2 (start! (string-append tag "c") d "s" (hold-env "reclaim-after-link" rel3) store))
         (a2-held (held? rel3))
         (_ (release! rel1))
         (b-held2 (held? rel2))
         (_ (release! rel3))
         (a2-said (await (string-append tag "c")))
         (_ (release! rel2))
         (b-said (await (string-append tag "b")))
         (marker (let ((ds (datums (file-text (string-append d "/b.d." (number->string b) "/.endpoint")))))
                   (and (pair? ds) (assq 'identity (cdar ds)))))
         (residue (and e (kind (string-append d "/b.d." (number->string e)))))
         (_ (kill! b))
         (c (start! (string-append tag "f") d "b" "" store2))
         (c-said (await (string-append tag "f")))
         (c-asked (ask (string-append d "/b") store2))
         (answer (append (list b-held1 a2-held b-held2 a2-said b-said marker)
                         (if residue? (list residue) '())
                         (list c-said c-asked))))
    (if (alive? b) (kill! b))
    (if (alive? c) (stop! c))
    (if (alive? a2) (stop! a2))
    (list d answer)))

(let* ((r (identity-from-the-one-look "xk" #f)) (d (car r)))
  (want "W5x18 B at s.d/../b, its identity taken while A2's reclaim had s.d unlinked: B's marker names <d>/b, and a later start of <d>/b serves"
        (cadr r)
        (list #t #t #t 'serving 'serving (list 'identity (identity-of d "b")) 'serving 'ok)))
(let* ((r (identity-from-the-one-look "xl" #t)) (d (car r)))
  (want "W5x18b the same with an earlier daemon's residue at b: B judges it under <d>/b, reclaims it and serves"
        (cadr r)
        (list #t #t #t 'serving 'serving (list 'identity (identity-of d "b")) 'absent 'serving 'ok)))

;; ---- W5x19 .. W5x20b: starts that reach the walk -------------------------------------
;;
;; serve refuses a --socket given by hand whose directory is absent (rc 2,
;; socket-dir-missing) before main runs, so a hand-given start reaches the walk
;; only when its directory goes after that check, as in W5x16. walk-start!
;; runs MAKE in D so the check passes, holds the start before the reservation
;; check, runs UNMAKE, and releases it to judge the directory absent. HOLDS is
;; more hold text (";<stage>:<rel>") and ENV more environment text. -> the pid,
;; or #f when the start never reached the hold.
(define (walk-start! tag d st socket make unmake holds env)
  (let* ((rel (string-append here "/" tag "-relw"))
         (_ (sh (string-append "cd " d " && " make)))
         (pid (launch! tag d st socket ""
                       (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='serve-before-reserve-check:"
                                      rel holds "' " env))))
    (and (held? rel)
         (zero? (sh (string-append "cd " d " && " unmake)))
         (begin (release! rel) pid))))

;; ---- W5x19: the walk's answer is canonical ------------------------------------------
;;
;; A socket spelled `<d>/new/./k/b` with `new` absent: the walk answers the
;; directory to make below the resolved `<d>`, and that answer is the
;; endpoint's identity. The `.` sits between two names: a trailing `/.` on
;; the parent is taken off before the walk (tidy-spelling), so `<d>/new/./b`
;; would not reach it. With the `.` kept, the marker names `<d>/new/./k/b`;
;; killed, the daemon leaves its link and marked directory, and the restart
;; finds `new/k` there, resolves it to `<d>/new/k`, compares the parents as
;; bytes and refuses its own residue. With the `.` dropped, the marker names
;; `<d>/new/k/b` and the restart reclaims the residue and serves. W5x19b is the
;; same through THEOURGIA_RUN=<d>/new/. and the default socket under its key,
;; whose directory serve does not check, so it starts plainly.
;; -> (parent answer), PARENT the directory holding the socket.
(define (canonical-walk tag run?)
  (let* ((d (fresh-dir tag))
         (env (if run? (string-append "THEOURGIA_RUN=" d "/new/. ") ""))
         (socket (and (not run?) (string-append d "/new/./k/b")))
         (base (if run? "socket" "b"))
         (a (if run?
                (start! (string-append tag "a") d #f env store)
                (walk-start! (string-append tag "a") d store socket "mkdir -p new/k" "rmdir new/k new" "" "")))
         (a-said (await (string-append tag "a")))
         (parent (if run?
                     (let ((l (names (string-append d "/new"))))
                       (and (list? l) (pair? l) (string-append d "/new/" (car l))))
                     (string-append d "/new/k")))
         (marker (and parent
                      (let ((ds (datums (file-text (string-append parent "/" base ".d."
                                                                  (number->string a) "/.endpoint")))))
                        (and (pair? ds) (assq 'identity (cdar ds))))))
         (_ (kill! a))
         (b (start! (string-append tag "b") d socket env store))
         (b-said (await (string-append tag "b")))
         (asked (and parent (ask (string-append parent "/" base))))
         (answer (list a-said marker b-said asked)))
    (if (alive? b) (stop! b))
    (list parent answer)))

(let* ((r (canonical-walk "xm" #f)) (parent (car r)))
  (want "W5x19 serve --socket <d>/new/./k/b with new absent: the marker names <d>/new/k/b, and the restart after a kill serves"
        (cadr r)
        (list 'serving (list 'identity (identity-of parent "b")) 'serving 'ok)))
(let* ((r (canonical-walk "xn" #t)) (parent (car r)))
  (want "W5x19b the same through THEOURGIA_RUN=<d>/new/.: the marker names <d>/new/<key>/socket, and the restart serves"
        (cadr r)
        (list 'serving (and parent (list 'identity (identity-of parent "socket"))) 'serving 'ok)))

;; -> the first error a start wrote, without its attempt clause, or #f: the
;; refusal as main reports it, its written clause included.
(define (refusal-said tag)
  (let ((e (find (lambda (x) (and (pair? x) (eq? (car x) 'error)))
                 (datums (file-text (string-append here "/" tag ".out"))))))
    (and e (remp (lambda (c) (and (pair? c) (eq? (car c) 'attempt))) e))))

;; ---- W5x20: a start makes exactly the names it judged ------------------------------
;;
;; On a folding volume B at `<d>/run/b`, `run` absent, is admitted to make
;; `<d>/run` and held before its mkdir; a peer serving another store under
;; THEOURGIA_RUN=<d>/Run makes `Run`. A mkdir that takes the name as there makes B's
;; directory under `Run` and records `<d>/run/b`; the restart resolves
;; `<d>/Run`, compares the parents as bytes and refuses its own residue. An
;; exclusive mkdir of each judged name meets `Run` and refuses serve-busy
;; naming `<d>/run`: the judgement is stale, nothing is removed, and the
;; restart resolves `Run` and serves with the marker naming `<d>/Run/b`.
(let* ((d (fresh-dir "xo"))
       (fold? (let ((probe (string-append d "/CaseProbe")))
                (sh (string-append "echo x > " probe))
                (file-exists? (string-append d "/caseprobe")))))
  (sh (string-append "rm -f " d "/CaseProbe"))
  (if (not fold?)
      (printf "SKIP W5x20 case: ~a does not fold case (CaseProbe/caseprobe); the row needs a folding volume\n" d)
      (let* ((rel (string-append here "/xo-rel"))
             (b (walk-start! "xob" d store2 (string-append d "/run/b") "mkdir run" "rmdir run"
                             (string-append ";serve-before-mkdir:" rel) ""))
             (b-held (and b (held? rel)))
             (p (start! "xop" d #f (string-append "THEOURGIA_RUN=" d "/Run ") store))
             (p-said (await "xop"))
             (_ (release! rel))
             (rc (rc-wait "xob"))
             (_ (if (alive? b) (kill! b)))
             (b-said (refusal-said "xob"))
             (c (start! "xoc" d (string-append d "/run/b") "" store2))
             (c-said (await "xoc"))
             (marker (let ((ds (datums (file-text (string-append d "/Run/b.d." (number->string c) "/.endpoint")))))
                       (and (pair? ds) (assq 'identity (cdar ds)))))
             (asked (ask (string-append d "/run/b") store2))
             (answer (list b-held p-said rc b-said c-said marker asked)))
        (if (alive? c) (stop! c))
        (if (alive? p) (stop! p))
        (want "W5x20 B at <d>/run/b held before its mkdir while a peer makes <d>/Run: B refused serve-busy naming <d>/run, its restart serves under Run"
              answer
              (list #t 'serving 75 (list 'error 'serve-busy (list 'path (string-append (real-path d) "/run")))
                    'serving (list 'identity (identity-of (string-append d "/Run") "b")) 'ok)))))

;; W5x20b, on any volume: the stale judgement at the second name. B at
;; `<d>/new/k/b`, both absent, makes `new`; a mkdir fault answers EEXIST (17
;; on every platform this runs on) at `new/k`, as a peer's `k` would (the
;; fault is one shot, so the backing directory under `new/k` is not hit). B
;; is refused serve-busy naming `<d>/new/k`, and the refusal carries main's
;; record: `new`, which this start made and leaves, is in its written
;; clause. The restart, through the walk as well, resolves `new`, makes `k`
;; and serves.
(let* ((d (fresh-dir "xr"))
       (b (walk-start! "xrb" d store2 (string-append d "/new/k/b") "mkdir -p new/k" "rmdir new/k new" ""
                       "THEOURGIA_FAULT='mkdir-fail@report:file=new/k:errno=17' "))
       (rc (rc-wait "xrb"))
       (_ (if (alive? b) (kill! b)))
       (b-said (refusal-said "xrb"))
       (kinds (list (kind (string-append d "/new")) (kind (string-append d "/new/k"))))
       (c (walk-start! "xrc" d store2 (string-append d "/new/k/b") "mkdir new/k" "rmdir new/k" "" ""))
       (c-said (await "xrc"))
       (asked (ask (string-append d "/new/k/b") store2))
       (answer (append (list rc b-said) kinds (list c-said asked))))
  (if (alive? c) (stop! c))
  (want "W5x20b the second judged name meets EEXIST: B refused serve-busy naming <d>/new/k, written naming the mkdir of <d>/new, which stays; the restart serves"
        answer
        (list 75 (list 'error 'serve-busy (list 'path (string-append (real-path d) "/new/k"))
                       (list 'written (list (list 'mkdir (string-append (real-path d) "/new")))))
              'directory 'absent 'serving 'ok)))

;; ---- W5x21: window (i), an identity read failing right after the mkdir ------------
;;
;; The reservation runs under main's `report` stage, so a stat fault aimed
;; at `s.d.<pid>` fires at the first look at the new backing directory: its
;; identity, read right after the exclusive mkdir. The start fails with 75
;; and leaves the directory empty, unmarked and unlinked; the next start is
;; not stopped by it, serves from its own `s.d.<pid>`, and leaves it there.
;; The daemon's pid is the shell's that execs it, so `$$` spells it.
(let* ((d (fresh-dir "xp"))
       (out (string-append here "/xpa.out"))
       (pidf (string-append here "/xpa.pid"))
       (a (begin
            (sh (string-append "rm -f " out " " pidf))
            (sh (string-append "( cd " d " && exec sh -c \"exec env " env-text
                               "THEOURGIA_INJECT=on THEOURGIA_FAULT=stat-fail@report:file=s.d.\\$\\$ scheme --script "
                               tree "/theourgiad.sc serve " store " --socket s\" > " out " 2>&1 < /dev/null & echo $! > "
                               pidf " )"))
            (let loop ((i 0))
              (if (and (< i 100) (string=? "" (file-text pidf))) (begin (sh "sleep 0.05") (loop (+ i 1)))))
            (started! (string->number (let ((t (file-text pidf))) (substring t 0 (max 0 (- (string-length t) 1))))))))
       (a-said (await "xpa"))
       (_ (wait-gone a))
       (left (string-append d "/s.d." (number->string a)))
       (before (list (kind left) (names left) (kind (string-append d "/s.d")) (kind (string-append d "/s"))))
       (b (start! "xpb" d "s" "" store))
       (b-said (await "xpb"))
       (after (list (equal? (link (string-append d "/s.d")) (string-append "s.d." (number->string b)))
                    (kind left) (names left)))
       (asked (ask (string-append d "/s")))
       (answer (append (list (and (pair? a-said) (car a-said))) before (list b-said) after (list asked))))
  (if (alive? b) (stop! b))
  (want "W5x21 window (i): the identity read fails right after the exclusive mkdir: the start fails, the directory is left empty, unmarked and unlinked, and the next start serves"
        answer
        '(error directory () absent absent serving #t directory () ok)))

;; ---- W5x22: a reservation lock is no endpoint's lifetime lock -------------------------
;;
;; Two endpoints in one directory, `s.d` and `.s`. The lifetime lock of
;; `s.d` is `.s.d.lock`, held for its daemon's life. A reservation lock
;; spelled `<socket>.d.lock` would make `.s`'s the same file: its start would
;; wait out the reservation budget and be refused serve-busy naming the
;; other endpoint's lock, on every try while `s.d` serves. Both serve, and
;; each answers on its own path.
(let* ((d (fresh-dir "xs"))
       (a (start! "xsa" d "s.d" "" store))
       (a-said (await "xsa"))
       (b (start! "xsb" d ".s" "" store2))
       (b-said (await "xsb"))
       (asked (list (ask (string-append d "/s.d")) (ask (string-append d "/.s") store2))))
  (if (alive? b) (stop! b))
  (if (alive? a) (stop! a))
  (want "W5x22 s.d serving, then a start of .s: both serve, and each answers on its own path"
        (list a-said b-said asked)
        '(serving serving (ok ok))))

;; ---- W5y: B first, then A ------------------------------------------------------------
(let* ((d (fresh-dir "y"))
       (a (start! "y1" d "s" "" store))
       (_ (await "y1"))
       (_ (kill! a))
       (b (start! "y2" d (string-append d "/s.d/7") "" store2))
       (b-said (await "y2"))
       (a2 (start! "y3" d "s" "" store))
       (a2-said (await "y3"))
       (b2 (start! "y4" d (string-append d "/s.d/7") "" store2))
       (b2-said (await "y4")))
  (want "W5y A dead and marked: B refused; A restarts and serves; B again refused, A alive"
        (list (head-of b-said 2) a2-said (head-of b2-said 2) (alive? a2) (ask (string-append d "/s")))
        '((error serve-path-reserved) serving (error serve-path-reserved) #t ok))
  (stop! a2))

;; ---- W5z: a reclaim interrupted after each of its steps -------------------------------
;; The link's unlink is claim-namespace!'s; the sockets' and the marker's
;; removal are reached through sweep-siblings!, which sweeps the directory the
;; link named once the link is gone.
(define (interrupted-at stage tag)
  (let* ((d (fresh-dir tag))
         (_ (sh (string-append "mkdir -p " d "/s.d.99 && touch " d "/s.d.99/foreign")))
         (a (start! (string-append tag "1") d "s" "" store))
         (_ (await (string-append tag "1")))
         (_ (kill! a))
         (rel (string-append here "/" tag "-rel"))
         (a2 (start! (string-append tag "2") d "s" (hold-env stage rel) store))
         (reached (held? rel))
         (_ (kill! a2))
         (a3 (start! (string-append tag "3") d "s" "" store))
         (said (await (string-append tag "3")))
         (answer (list reached said (ask (string-append d "/s")) (names (string-append d "/s.d.99")))))
    (stop! a3)
    answer))

(want "W5z interrupted after the link's unlink: the next start recovers and serves, the foreign directory untouched"
      (interrupted-at "reclaim-after-link" "z1")
      '(#t serving ok ("foreign")))
(want "W5z interrupted after the sockets' removal: the same"
      (interrupted-at "reclaim-after-sockets" "z2")
      '(#t serving ok ("foreign")))
(want "W5z interrupted after the marker's removal: the same"
      (interrupted-at "reclaim-after-marker" "z3")
      '(#t serving ok ("foreign")))

(define (dangling-to target tag)
  (let* ((d (fresh-dir tag))
         (_ (sh (string-append "ln -s " target " " d "/s.d")))
         (a (start! tag d "s" "" store))
         (said (await tag))
         (answer (list said (link (string-append d "/s.d")) (ask (string-append d "/s")))))
    (stop! a)
    (list (car answer) (string=? (or (cadr answer) "") (string-append "s.d." (number->string a))) (caddr answer))))

(want "W5z a hand-made <socket>.d link to a vanished s.d.<digits>: reclaimed, the start serves"
      (dangling-to "s.d.12345" "z4")
      '(serving #t ok))
(want "W5z and one to a vanished s.d.<digits>-1: the same"
      (dangling-to "s.d.12345-1" "z5")
      '(serving #t ok))

;; ---- W5z6: a sweep that fails after removing something ------------------------------
;;
;; Held after the marker's removal, a file is planted in the residue: the
;; directory's removal fails. Something was removed, so the link is not put
;; back to a directory that is no longer marked: the directory is left as it
;; is, and the start serves from its own.
(let* ((d (fresh-dir "z6"))
       (a (start! "z6a" d "s" "" store))
       (_ (await "z6a"))
       (at (number->string a))
       (_ (kill! a))
       (rel (string-append here "/z6-rel"))
       (b (start! "z6b" d "s" (hold-env "reclaim-after-marker" rel) store))
       (reached (held? rel))
       (_ (sh (string-append "echo late > " d "/s.d." at "/late")))
       (_ (release! rel))
       (said (await "z6b"))
       (bt (number->string b)))
  (want "W5z6 the residue's removal fails after its marker went: left as it is, the start serves from its own"
        (list reached said (link (string-append d "/s.d")) (names (string-append d "/s.d." at))
              (ask (string-append d "/s")))
        (list #t 'serving (string-append "s.d." bt) '("late") 'ok))
  (if (eq? said 'serving) (stop! b) (kill! b)))

;; ---- W5z8: a refusal after a partial sweep of the link's directory -------------------
;;
;; The link's directory loses its marker and then cannot be removed (a file
;; planted at reclaim-after-marker): partial. Then the start is refused (its
;; own directory swapped at namespace-after-mkdir) while the old directory
;; cannot be read. The link is not put back: this start removed the marker.
(let* ((d (fresh-dir "z8"))
       (a (start! "z8a" d "s" "" store))
       (_ (await "z8a"))
       (at (number->string a))
       (old (string-append d "/s.d." at))
       (_ (kill! a))
       (rel (string-append here "/z8-rel"))
       (rel2 (string-append here "/z8-rel2"))
       (b (launch! "z8b" d store "s" ""
                   (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD='reclaim-after-marker:" rel
                                  ";namespace-after-mkdir:" rel2 "' ")))
       (bt (number->string b))
       (reached (held? rel))
       (_ (sh (string-append "echo late > " old "/late")))
       (_ (release! rel))
       (reached2 (held? rel2))
       (_ (sh (string-append "chmod 000 " old " && cd " d " && mv s.d." bt " moved && mkdir s.d." bt)))
       (_ (release! rel2))
       (rc (rc-wait "z8b"))
       (_ (sh (string-append "chmod 700 " old))))
  (want "W5z8 refused after a partial sweep of the link's directory: the link is not put back to it"
        (list reached reached2 rc (kind (string-append d "/s.d")) (names old))
        '(#t #t 75 absent ("late"))))

;; ---- W5z9: a marker stored as `.ENDPOINT` on a folding volume ---------------------------
(let* ((d (fresh-dir "z9"))
       (fold? (let ((probe (string-append d "/CaseProbe")))
                (sh (string-append "echo x > " probe))
                (file-exists? (string-append d "/caseprobe")))))
  (if (not fold?)
      (printf "SKIP W5z9 case: ~a does not fold case (CaseProbe/caseprobe); the row needs a folding volume\n" d)
      (let* ((a (start! "z9a" d "s" "" store))
             (_ (await "z9a"))
             (at (number->string a))
             (old (string-append d "/s.d." at))
             (_ (kill! a))
             (_ (sh (string-append "mv " old "/.endpoint " old "/.ENDPOINT")))
             (b (start! "z9b" d "s" "" store))
             (said (await "z9b")))
        (want "W5z9 a residue whose marker is stored as .ENDPOINT on a folding volume is swept and removed"
              (list said (kind old) (ask (string-append d "/s")))
              '(serving absent ok))
        (if (eq? said 'serving) (stop! b) (kill! b)))))

;; ---- W5z10, W5z11: the link put back after a forced refusal ---------------------------
;;
;; A start reclaims `s.d` and is then refused (its own directory swapped at
;; namespace-after-mkdir): the link is put back when the start removed nothing
;; from the directory it named -- one holding only its marker and `notes`
;; (W5z10), and one that does not exist (W5z11).
(define (refused-after-reclaim tag d)
  (let* ((rel (string-append here "/" tag "-rel"))
         (b (launch! tag d store "s" "" (hold-env "namespace-after-mkdir" rel)))
         (bt (number->string b))
         (reached (held? rel)))
    (sh (string-append "cd " d " && mv s.d." bt " moved && mkdir s.d." bt))
    (release! rel)
    (list reached (rc-wait tag))))

(let* ((d (fresh-dir "za"))
       (a (start! "zaa" d "s" "" store))
       (_ (await "zaa"))
       (at (number->string a))
       (_ (kill! a))
       (_ (sh (string-append "cd " d " && rm s.d." at "/" at " && echo keep > s.d." at "/notes")))
       (r (refused-after-reclaim "zab" d)))
  (want "W5z10 a refusal after a sweep that removed nothing: the link is put back"
        (list r (link (string-append d "/s.d")) (names (string-append d "/s.d." at)))
        (list '(#t 75) (string-append "s.d." at) '(".endpoint" "notes"))))

(let* ((d (fresh-dir "zb"))
       (_ (sh (string-append "ln -s s.d.12345 " d "/s.d")))
       (r (refused-after-reclaim "zbb" d)))
  (want "W5z11 a refusal after a dangling link's reclaim: the link is put back as it was"
        (list r (link (string-append d "/s.d")))
        '((#t 75) "s.d.12345")))

(teardown!)
(printf "rows: ~a\n~a failures\ndaemon-namespace complete\n" rows bad)
