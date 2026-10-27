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

;; A plain command never starts a daemon.
;;
;; KEY: THE THIN CLIENT, AS A PERSON RUNS IT. With a daemon listening it uses
;; the socket; with none it hands the same request frame to a core.sc child
;; of its own and reads the child's answer the way it reads a daemon's. Every
;; row runs the real `theourgia.sc` as a process.
;;
;; NEVER: EVERY LOCAL ROW ALSO ASKS WHAT WAS STARTED. The data in an answer
;; is the same whichever route made it -- that is the byte-for-byte rule --
;; so a row that read only the answer would pass on a client that started a
;; daemon and asked it. Each local call is bracketed: the count of daemon
;; processes naming this fixture's directory before and after, and whether
;; the store's socket exists afterwards.
;;
;; NOTE: THE ROUTES ARE COMPARED ON ONE STORE STATE. A daemon answers from its
;; last publication and a local answer from the store as it is; the local
;; answers are taken, then a daemon is started on the same store and asked the
;; same things, with no write in between.

(import (chezscheme)
        (only (theourgia client) socket-path))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
;; A RAISE INSIDE A ROW IS THAT ROW FAILING, NOT THE FILE ENDING.
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

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS: files under
;; THEOURGIA_TEST_ROOT, socket paths under THEOURGIA_TEST_SOCK, which is short
;; enough for one. Run alone, without them, a path falls back to /tmp.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/opener-" pid-text))
(define sock-here (string-append socket-base "/opener-" pid-text))
;; THE RUN ROOT IS THIS FIXTURE'S OWN, in this process and in every process it
;; starts, so both compute the same socket path for a store.
(define run-root (string-append sock-here "/run"))
(putenv "THEOURGIA_RUN" run-root)

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

;; THE PROGRAMS BY ABSOLUTE PATH: a row runs the client from directories of
;; its own, and the client finds core.sc beside itself.
(define tree
  (let* ((d (current-directory))
         (cut (let loop ((i (- (string-length d) 1)))
                (cond ((< i 0) #f) ((char=? (string-ref d i) #\/) i) (else (loop (- i 1)))))))
    (substring d 0 cut)))
(define client (string-append tree "/theourgia.sc"))
(define core (string-append tree "/core.sc"))

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

(define (trim s)
  (let loop ((e (string-length s)))
    (if (and (> e 0) (char-whitespace? (string-ref s (- e 1)))) (loop (- e 1)) (substring s 0 e))))

(define (env-prefix extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                 "THEOURGIA_RUN=" run-root " THEOURGIA_ACTOR=tester " extra))

(define (sh cmd) (system cmd))

(define (write-file! path text)
  (when (file-exists? path) (delete-file path))
  (call-with-output-file path (lambda (port) (put-string port text))))

(sh (string-append "rm -rf " here " " sock-here "; mkdir -p " here " " sock-here))

;; ---- running things --------------------------------------------------------

;; ONE CLIENT CALL: -> (rc stdout stderr). `extra` is environment text, `in` a
;; file for standard input or #f, `dir` the directory it runs in or #f.
;; NEVER: A HUNG CLIENT IS A RED ROW, NOT A HUNG FIXTURE. Each call is bounded at
;; 120 s (exit 142 when the alarm ends it): a client that waited for its child
;; to exit before reading it would block on a full pipe for ever.
(define calls 0)
(define (client! extra args in dir)
  (set! calls (+ calls 1))
  (let* ((n (number->string calls))
         (out (string-append here "/call" n ".out"))
         (err (string-append here "/call" n ".err"))
         (rcf (string-append here "/call" n ".rc")))
    (sh (string-append "cd " (or dir here) " && " (env-prefix extra) " perl -e 'alarm shift; exec @ARGV' 120 scheme --script " client " " args
                       " < " (or in "/dev/null") " > " out " 2> " err "; echo $? > " rcf))
    (list (string->number (trim (file-text rcf))) (file-text out) (file-text err))))
(define (rc-of r) (car r))
(define (out-of r) (cadr r))
(define (err-of r) (caddr r))

;; THE SETUP GOES THROUGH core.sc WITH THEOURGIA_LOCAL=1, the debugging path,
;; so it depends on nothing the rows are about. -> the answer, read.
(define (setup! args)
  (let ((out (string-append here "/setup.out")))
    (sh (string-append "cd " here " && " (env-prefix "THEOURGIA_LOCAL=1") " scheme --script " core " "
                       args " --wire < /dev/null > " out " 2>&1"))
    (guard (e (#t (list 'unreadable (file-text out))))
      (with-input-from-file out read))))
(define (setup-in! args in)
  (let ((out (string-append here "/setup.out")))
    (sh (string-append "cd " here " && " (env-prefix "THEOURGIA_LOCAL=1") " scheme --script " core " "
                       args " --wire < " in " > " out " 2>&1"))
    (guard (e (#t (list 'unreadable (file-text out))))
      (with-input-from-file out read))))

;; the id an insert made: (ok (events ...) (state ((<id> . <hash>))) ...)
(define (made-id answer)
  (let ((state (and (pair? answer) (assq 'state (cdr answer)))))
    (and state (pair? (cadr state)) (caar (cadr state)))))

;; DAEMON PROCESSES NAMING THIS FIXTURE'S DIRECTORY: what a client that
;; started one would leave. The fixture's own daemons run as `serve-*.sc`
;; scripts and do not match.
(define (daemon-count)
  (let* ((p (process (string-append "pgrep -f 'theourgiad.*" here "' | wc -l")))
         (n (read (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (integer? n) n 'unreadable)))

;; A LOCAL CALL, BRACKETED: -> (rc stdout stderr (<daemons before> <after> <socket left?>)).
(define (local! store extra args in dir)
  (let* ((before (daemon-count))
         (r (client! extra args in dir))
         (after (daemon-count)))
    (append r (list (list before after (file-exists? (socket-path store)))))))
(define (started-of r) (list-ref r 3))
(define nothing-started '(0 0 #f))

;; ---- background processes ----------------------------------------------------

;; EVERY BACKGROUND PROCESS IS WAITED FOR BY ITS PID after it is told to stop.
;; The pid comes from the shell's own output through a process port.
(define (start-bg! cmd)
  (guard (e (#t #f))
    (let* ((p (process (string-append cmd " < /dev/null & echo $!")))
           (in (car p)))
      (close-port (cadr p))
      (let ((n (read in)))
        (close-port in)
        (and (integer? n) (> n 0) n)))))
(include "pid-state.ss")
(define (wait-gone! pid limit-ms)
  (let wait ((k 0))
    (unless (or (not pid) (> (* k 50) limit-ms) (pid-gone? pid))
      (sh "sleep 0.05")
      (wait (+ k 1)))))
(define (wait-file! path limit-ms)
  (let wait ((k 0))
    (cond ((file-exists? path) #t)
          ((> (* k 50) limit-ms) #f)
          (else (sh "sleep 0.05") (wait (+ k 1))))))

;; A DAEMON THIS FIXTURE STARTS ITSELF, tracing, its log under `here`.
(define (start-daemon! store name)
  (let ((script (string-append here "/serve-" name ".sc"))
        (log (string-append here "/serve-" name ".log")))
    (write-file! script
      (string-append "(import (chezscheme) (theourgia daemon))\n"
                     "(serve \"" store "\" \"" (socket-path store) "\")\n"))
    (let ((pid (start-bg! (string-append (env-prefix "THEOURGIA_TRACE=1") " scheme --script " script
                                         " > " log " 2>&1"))))
      (list pid script log (if (wait-file! (socket-path store) 20000) 'up 'never)))))
(define (daemon-up d) (list-ref d 3))
(define (daemon-log d) (file-text (list-ref d 2)))
(define (stop-daemon! d)
  (sh (string-append "pkill -f " (cadr d) " 2>/dev/null"))
  (wait-gone! (car d) 10000))

;; THE STORE'S LOCK, HELD BY ANOTHER PROCESS for `seconds`; -> its pid once it
;; holds the lock (the marker exists).
(define (hold-lock! store seconds)
  (let ((marker (string-append here "/lock-held-" (number->string seconds))))
    (when (file-exists? marker) (delete-file marker))
    (let ((pid (start-bg! (string-append "perl -e 'use Fcntl qw(:flock); open(my $f, \">>\", \"" store
                                         "/lock\") or exit 3; flock($f, LOCK_EX) or exit 4; open(my $m, \">\", \""
                                         marker "\"); close $m; sleep " (number->string seconds) "'"))))
      (wait-file! marker 10000)
      pid)))

(define (now-ms) (let ((t (current-time))) (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

;; ---- the store ---------------------------------------------------------------

(define S (string-append here "/s"))
(setup! (string-append "init --store " S))
(define B (made-id (setup! (string-append "insert --title ALPHA --store " S))))
(want "SETUP the store and its block exist, made through the debugging path" (and (string? B) #t) #t)
(setup! (string-append "tag t1 --store " S))

;; ---- W1: the local answers, then the daemon's on the same state ----------------

;; Each case: a label and its command line.
(define route-cases
  (list (list 'read (string-append "read " B " --store " S))
        (list 'read-wire (string-append "read " B " --store " S " --wire"))
        (list 'store-first (string-append "--store " S " read " B))
        (list 'repeated (string-append "read " B " --store " S " --store " S))
        (list 'separator (string-append "--store " S " --actor A --wire read -- " B))
        (list 'trailing-store (string-append "read " B " --store"))
        (list 'trailing-actor (string-append "read " B " --actor"))
        (list 'help-after-repeat (string-append "read " B " --store " S " --store " S " --help"))
        (list 'cut (string-append "read " B " --cut t1 --store " S))))

(define local-answers
  (map (lambda (c) (cons (car c) (local! S "" (cadr c) #f #f))) route-cases))
(define (local-of name) (cdr (assq name local-answers)))

(define d1 (start-daemon! S "s"))
(want "SETUP the fixture's daemon on the same store came up" (daemon-up d1) 'up)
(define daemon-answers
  (map (lambda (c) (cons (car c) (client! "" (cadr c) #f #f))) route-cases))
(define (daemon-of name) (cdr (assq name daemon-answers)))

;; W9 and W9b, while it listens: the socket route is still taken.
(define w9 (client! "" (string-append "read " B " --rev --store " S " --wire") #f #f))
(define w9b (client! "" (string-append "adopt --store " S " --wire") #f #f))
(define w9b-count (daemon-count))
(stop-daemon! d1)
(define d1-log (daemon-log d1))

(want "W1 no daemon, a plain read: answered, exit 0, nothing started, no socket"
      (let ((r (local-of 'read)))
        (list (rc-of r) (contains? (out-of r) "ALPHA") (started-of r)))
      (list 0 #t nothing-started))
(want "W1 the local answer equals the daemon's byte for byte, exit code too (same store state)"
      (list (equal? (out-of (local-of 'read)) (out-of (daemon-of 'read)))
            (equal? (out-of (local-of 'read-wire)) (out-of (daemon-of 'read-wire)))
            (= (rc-of (local-of 'read)) (rc-of (daemon-of 'read))))
      '(#t #t #t))
(want "W1 CONTROL: the daemon answered those (its log shows the dispatch)"
      (contains? d1-log "daemon-dispatch read")
      #t)
(want "W1c no daemon, the store option before the verb: answered as the daemon answers it, nothing started"
      (let ((r (local-of 'store-first)))
        (list (rc-of r) (equal? (out-of r) (out-of (daemon-of 'store-first))) (started-of r)))
      (list 0 #t nothing-started))
(want "W1e2 repeated transport options: answered as the socket route answers them, not duplicate-option"
      (let ((r (local-of 'repeated)))
        (list (contains? (out-of r) "duplicate-option") (equal? (out-of r) (out-of (daemon-of 'repeated)))
              (started-of r)))
      (list #f #t nothing-started))
(want "W1e2 options before the verb and a literal after the separator: store S, the wire shape, the block read"
      (let ((r (local-of 'separator)))
        (list (rc-of r) (contains? (out-of r) "ALPHA") (equal? (out-of r) (out-of (daemon-of 'separator)))
              (started-of r)))
      (list 0 #t #t nothing-started))
(want "W1g a trailing option with no value: missing-option-value naming it, as the socket route answers"
      (let ((s (local-of 'trailing-store)) (a (local-of 'trailing-actor)))
        (list (contains? (out-of s) "missing-option-value") (contains? (out-of s) "--store")
              (equal? (out-of s) (out-of (daemon-of 'trailing-store)))
              (contains? (out-of a) "missing-option-value") (contains? (out-of a) "--actor")
              (equal? (out-of a) (out-of (daemon-of 'trailing-actor)))
              (started-of s) (started-of a)))
      (list #t #t #t #t #t #t nothing-started nothing-started))
(want "W1h a help flag that survives the client's help check: refused transport-option-in-rpc as the daemon refuses it"
      (let ((r (local-of 'help-after-repeat)))
        (list (contains? (out-of r) "transport-option-in-rpc") (contains? (out-of r) "usage")
              (equal? (out-of r) (out-of (daemon-of 'help-after-repeat))) (started-of r)))
      (list #t #f #t nothing-started))
(want "W2b no daemon, read --cut <tag>: answered as the daemon answers it, nothing started"
      (let ((r (local-of 'cut)))
        (list (rc-of r) (equal? (out-of r) (out-of (daemon-of 'cut))) (started-of r)))
      (list 0 #t nothing-started))
(want "W9 a daemon listening: a CLI read --rev is answered through it, with its rev and token"
      (list (rc-of w9) (contains? (out-of w9) "(rev ") (contains? (out-of w9) "(daemon \""))
      '(0 #t #t))
(want "W9b a daemon listening: a CLI adopt is answered through it, and no second daemon is started"
      (list (contains? d1-log "daemon-dispatch adopt") w9b-count)
      '(#t 0))

;; ---- W2: the two refusals by route -----------------------------------------------

(define w2-rev (local! S "" (string-append "read " B " --rev --store " S) #f #f))
(define w2-sub (local! S "" (string-append "subscribe changes 0 --store " S) #f #f))
(want "W2 no daemon: read --rev refused no-published-state, nothing started"
      (list (contains? (out-of w2-rev) "no-published-state") (rc-of w2-rev) (started-of w2-rev))
      (list #t 1 nothing-started))
(want "W2 no daemon: subscribe refused needs-daemon, nothing started"
      (list (contains? (out-of w2-sub) "needs-daemon") (rc-of w2-sub) (started-of w2-sub))
      (list #t 1 nothing-started))

;; ---- W1b, W1d: standard input, the environment's writer, the trace ----------------

(define intents (string-append here "/intents.sexp"))
(write-file! intents "((insert root #f ((kind . doc) (title . \"FROM-BATCH\"))))\n")
(define w1b-batch (local! S "" (string-append "batch --store " S " --wire") intents #f))
(define text-in (string-append here "/text.txt"))
(write-file! text-in "piped text body\n")
(define w1b-write (local! S "THEOURGIA_WRITER=wb" (string-append "write " B " - --store " S) text-in #f))
(define outline-after-batch (setup! (string-append "outline --store " S)))
(define drafts-wb (setup! (string-append "drafts --writer wb --store " S)))
(want "W1b no daemon, batch with its intents on standard input: written as given, nothing started"
      (list (rc-of w1b-batch) (contains? (format "~s" outline-after-batch) "FROM-BATCH") (started-of w1b-batch))
      (list 0 #t nothing-started))
(want "W1b no daemon, write <id> - with text on standard input: saved, the draft holds the piped text"
      (list (rc-of w1b-write) (contains? (out-of w1b-write) "saved")
            (contains? (format "~s" (setup! (string-append "read " B " --writer wb --working --store " S)))
                       "piped text body")
            (started-of w1b-write))
      (list 0 #t #t nothing-started))
(define w1d (local! S "THEOURGIA_WRITER=wd THEOURGIA_TRACE=1" (string-append "write " B " from-environment --store " S) #f #f))
(want "W1d no daemon, THEOURGIA_WRITER set, a write with no --writer: the draft is that writer's"
      (list (rc-of w1d) (contains? (format "~s" (setup! (string-append "drafts --writer wd --store " S))) B)
            (started-of w1d))
      (list 0 #t nothing-started))
(want "W1d the client's trace shows the child started with THEOURGIA_LOCAL=1, and the child saw it"
      (list (contains? (err-of w1d) "(trace frame-child ") (contains? (err-of w1d) "THEOURGIA_LOCAL=1")
            (contains? (err-of w1d) "(trace frame-local write 1)"))
      '(#t #t #t))

;; ---- W1f: the actor and the wire shape travel in the frame ---------------------------

(define w1f (local! S "" (string-append "insert --title BY-ACTOR --actor actor-a --wire --store " S) #f #f))
(want "W1f no daemon, insert with --actor and --wire: the record carries the actor, the answer the wire shape"
      (list (rc-of w1f) (and (> (string-length (out-of w1f)) 12) (string=? (substring (out-of w1f) 0 12) "(ok (events "))
            (contains? (format "~s" (setup! (string-append "log --store " S))) "actor-a")
            (started-of w1f))
      (list 0 #t #t nothing-started))

;; ---- W1e: adopt on a copy that needs it ------------------------------------------------

(define S2 (string-append here "/s2"))
(sh (string-append "cp -R " S " " S2))
(define w1e (local! S2 "" (string-append "adopt --store " S2 " --wire") #f #f))
(want "W1e no daemon, adopt on a copy that needs it: the adoption answers as it does, nothing started"
      (list (rc-of w1e) (contains? (out-of w1e) "(ok (from ") (contains? (out-of w1e) "(to ") (started-of w1e))
      (list 0 #t #t nothing-started))

;; ---- W1i: describe ------------------------------------------------------------------

(define T (string-append here "/t"))
(setup! (string-append "init --template project --store " T))
(define w1i-local (local! T "" (string-append "describe --store " T " --wire") #f #f))
(define dt (start-daemon! T "t"))
(define w1i-daemon (client! "" (string-append "describe --store " T " --wire") #f #f))
(stop-daemon! dt)
(define nowhere (string-append here "/no-store-here"))
(define w1i-none (local! nowhere "" (string-append "describe --store " nowhere " --wire") #f #f))
(want "W1i no daemon, describe on a templated store: the local answer, with no template clause"
      (list (rc-of w1i-local) (contains? (out-of w1i-local) "(ok (verbs") (contains? (out-of w1i-local) "(template (roots")
            (started-of w1i-local))
      (list 0 #t #f nothing-started))
(want "W1i CONTROL: the same store's daemon adds the template clause"
      (list (daemon-up dt) (contains? (out-of w1i-daemon) "(template (roots"))
      '(up #t))
(want "W1i describe with no store at all, without THEOURGIA_LOCAL: the catalogue, nothing started"
      (list (rc-of w1i-none) (contains? (out-of w1i-none) "(ok (verbs") (started-of w1i-none)
            (file-exists? nowhere))
      (list 0 #t nothing-started #f))

;; ---- W1j: the envelope's ceiling on the child's answer -----------------------------------

(define big-text (make-string 400000 #\x))
(define body-file (string-append here "/big-body.txt"))
(write-file! body-file big-text)
(define BIG (made-id (setup-in! (string-append "insert --title BIG --text - --store " S) body-file)))
(define limit-env "THEOURGIA_TEST_ANSWER_LIMIT=300 THEOURGIA_TRACE=1")
(define w1j-local (local! S limit-env (string-append "read " BIG " --store " S " --wire") #f #f))
(define dj (start-daemon! S "j"))
(define w1j-daemon (client! limit-env (string-append "read " BIG " --store " S " --wire") #f #f))
(stop-daemon! dj)
(define (trace-datum text tag)
  (let loop ((port (open-input-string text)))
    (let ((d (guard (e (#t (eof-object))) (read port))))
      (cond ((eof-object? d) #f)
            ((and (pair? d) (eq? (car d) 'trace) (pair? (cdr d)) (eq? (cadr d) tag)) d)
            (else (loop port))))))
(want "W1j an answer over the ceiling: refused answer-too-large exactly as the socket route refuses it, exit 75"
      (list (trim (out-of w1j-local)) (rc-of w1j-local) (equal? (out-of w1j-local) (out-of w1j-daemon))
            (rc-of w1j-daemon) (started-of w1j-local))
      (list "(error transport-unknown (reason answer-too-large))" 75 #t 75 nothing-started))
(want "W1j the child exited 0 and was reaped; the client's exit is 75 all the same"
      (let ((d (trace-datum (err-of w1j-local) 'frame-child-exit)))
        (and d (cadddr d)))
      '(exit 0))
(want "W1j the child's output (past a pipe's buffer) was read as it came: held never past the ceiling and one read"
      (let ((d (trace-datum (err-of w1j-local) 'child-answer)))
        (and d (list (> (caddr d) 400000) (<= (cadddr d) (+ 300 65536)))))
      '(#t #t))

;; THE REAL CEILING: a recursive read of a store built for it, about 42 MiB of
;; answer from 42 children of a little under a megabyte each (a frame holds at
;; most one), in a store of its own so no other row loads it. A child carries
;; no kind: a doc must be top-level.
(define S40 (string-append here "/s40"))
(setup! (string-append "init --store " S40))
(define P40 (made-id (setup! (string-append "insert --title PARENT --store " S40))))
(define child-intent (string-append here "/child40.sexp"))
(write-file! child-intent
  (string-append "((insert \"" P40 "\" #f ((title . \"C\") (text . \"" (make-string 1000000 #\z) "\"))))\n"))
(let loop ((i 0)) (when (< i 42) (setup-in! (string-append "batch --store " S40) child-intent) (loop (+ i 1))))
(define w1j-real-start (now-ms))
(define w1j-real (local! S40 "THEOURGIA_TRACE=1" (string-append "read " P40 " --recursive --store " S40 " --wire") #f #f))
(define w1j-real-ms (- (now-ms) w1j-real-start))
(printf "reading: W1j's 42 MiB answer refused in ~a ms\n" w1j-real-ms)
(want "W1j at the real ceiling: a 42 MiB child answer is refused answer-too-large, exit 75, the child exited 0"
      (list (trim (out-of w1j-real)) (rc-of w1j-real)
            (let ((d (trace-datum (err-of w1j-real) 'frame-child-exit))) (and d (cadddr d)))
            (started-of w1j-real))
      (list "(error transport-unknown (reason answer-too-large))" 75 '(exit 0) nothing-started))
(want "W1j at the real ceiling: all of it was read as it came, held never past the ceiling and one read"
      (let ((d (trace-datum (err-of w1j-real) 'child-answer)))
        (and d (list (> (caddr d) (* 40 1024 1024)) (<= (cadddr d) (+ (* 32 1024 1024) 65536)))))
      '(#t #t))

;; ---- W1k: an oversized piped input -----------------------------------------------------

(define (intents-of-size bytes)
  (string-append "((insert root #f ((kind . doc) (title . \"" (make-string bytes #\y) "\"))))\n"))
(define small-over (string-append here "/over-small.sexp"))
(define large-over (string-append here "/over-large.sexp"))
(write-file! small-over (intents-of-size (* 3 512 1024)))
(write-file! large-over (intents-of-size (* 64 1024 1024)))
(define outline-before-k (setup! (string-append "outline --store " S)))
(define w1k-small (local! S "" (string-append "batch --store " S " --wire") small-over #f))
(define w1k-large (local! S "" (string-append "batch --store " S " --wire") large-over #f))
(define w1k-mute (local! S "THEOURGIA_INJECT=on THEOURGIA_FAULT=frame-unanswered@conn THEOURGIA_TRACE=1"
                        (string-append "batch --store " S " --wire") small-over #f))
;; HOW MANY CHILDREN A CALL STARTED: its trace's `(trace frame-child <pid> ...)`
;; lines (the write-stopped and exit lines are other tags).
(define (children-started r)
  (let loop ((port (open-input-string (err-of r))) (n 0))
    (let ((d (guard (e (#t (eof-object))) (read port))))
      (cond ((eof-object? d) n)
            ((and (pair? d) (eq? (car d) 'trace) (pair? (cdr d)) (eq? (cadr d) 'frame-child)) (loop port (+ n 1)))
            (else (loop port n))))))
(define outline-after-k (setup! (string-append "outline --store " S)))
(define dk (start-daemon! S "k"))
(define w1k-socket (client! "" (string-append "batch --store " S " --wire") small-over #f))
(define w1k-socket-large (client! "" (string-append "batch --store " S " --wire") large-over #f))
;; the log's CONTROL: a request this daemon did dispatch is in it
(define w1k-socket-read (client! "" (string-append "read " B " --store " S " --wire") #f #f))
(stop-daemon! dk)
(define dk-log (daemon-log dk))
(define outline-after-ks (setup! (string-append "outline --store " S)))
(want "W1k the local route, past the frame's limit: the child's frame-limit refusal is reported, nothing written"
      (list (trim (out-of w1k-small)) (rc-of w1k-small) (equal? outline-before-k outline-after-k)
            (started-of w1k-small))
      (list "(error bad-request (reason frame-limit))" 1 #t nothing-started))
(want "W1k a 64 MiB input, written in part before the child closed: the refusal is read, no crash on SIGPIPE"
      (list (trim (out-of w1k-large)) (rc-of w1k-large) (started-of w1k-large))
      (list "(error bad-request (reason frame-limit))" 1 nothing-started))
(want "W1k a child that closes without answering: transport-unknown, exit 75, no replay (one child started)"
      (list (trim (out-of w1k-mute)) (rc-of w1k-mute) (equal? outline-before-k outline-after-k)
            (children-started w1k-mute) (started-of w1k-mute))
      (list "(error transport-unknown (reason lost-answer))" 75 #t 1 nothing-started))
;; THE DAEMON TRACES NO LINE OF ITS OWN FOR THE REFUSAL: what its log shows
;; is that the batch was never dispatched, beside the read it did dispatch.
;; The 64 MiB case on the socket is answered as the base tree answers it:
;; the daemon closes after the frame's limit while the client's one blocking
;; write is still under way, the write fails with EPIPE and no progress
;; reported, and the client says write-failed, exit 75 (measured on the base
;; tree, three of three).
(define (socket-refused? r)
  (or (contains? (out-of r) "frame-limit") (contains? (out-of r) "transport-unknown")))
(want "W1k (the socket route, as before) both sizes: refused, the store unchanged"
      (list (socket-refused? w1k-socket)
            (or (socket-refused? w1k-socket-large) (contains? (out-of w1k-socket-large) "(error write-failed"))
            (and (memv (rc-of w1k-socket-large) '(1 75)) #t)
            (equal? outline-before-k outline-after-ks))
      '(#t #t #t #t))
(want "W1k (the socket route) the daemon's log: no batch dispatched; CONTROL, the read it answered is there"
      (list (contains? dk-log "daemon-dispatch batch") (contains? dk-log "daemon-dispatch read")
            (rc-of w1k-socket-read))
      '(#f #t 0))
;; Q6 (b): the refusal itself is traced now, with the route and the size.
(want "W1k (the socket route) the daemon's log carries the refusal: frame-refused, route socket, frame-limit"
      (contains? dk-log "(trace frame-refused (route socket) ((reason frame-limit) (bytes ")
      #t)

;; ---- CLOSE-1: a close of the child's pipe ends that fails --------------------------------

;; how many `(trace <tag> ...)` lines a call's trace holds
(define (trace-count text tag)
  (let loop ((port (open-input-string text)) (n 0))
    (let ((d (guard (e (#t (eof-object))) (read port))))
      (cond ((eof-object? d) n)
            ((and (pair? d) (eq? (car d) 'trace) (pair? (cdr d)) (eq? (cadr d) tag)) (loop port (+ n 1)))
            (else (loop port n))))))
(define close-clean (local! S "THEOURGIA_TRACE=1" (string-append "read " B " --store " S " --wire") #f #f))
(define close-armed (local! S "THEOURGIA_INJECT=on THEOURGIA_FAULT=close-fail@client THEOURGIA_TRACE=1"
                            (string-append "read " B " --store " S " --wire") #f #f))
(want "CLOSE-1 both pipe ends' closes fail: the answer delivered byte for byte, exit 0, the child reaped"
      (list (equal? (out-of close-armed) (out-of close-clean)) (rc-of close-armed)
            (trace-count (err-of close-armed) 'close-failed)
            (let ((d (trace-datum (err-of close-armed) 'frame-child-exit))) (and d (cadddr d)))
            (started-of close-armed))
      (list #t 0 2 '(exit 0) nothing-started))
(want "CLOSE-1 CONTROL: unarmed, the same call notes no failed close and answers"
      (list (rc-of close-clean) (trace-count (err-of close-clean) 'close-failed) (contains? (out-of close-clean) "ALPHA"))
      '(0 0 #t))

;; ---- W1l: a relative path resolves against the client's directory -------------------------

(define client-dir (string-append here "/client-dir"))
(define elsewhere (string-append here "/elsewhere"))
(sh (string-append "mkdir -p " client-dir "/src " elsewhere))
(write-file! (string-append client-dir "/src/one.sc") "(define (one) 1)\n")
;; THE CHILD IS STARTED IN ANOTHER DIRECTORY: THEOURGIA_SCHEME names a wrapper
;; that changes directory before it runs scheme, so a child that read its own
;; directory would look in `elsewhere`.
(define wrapper (string-append here "/scheme-elsewhere"))
(write-file! wrapper (string-append "#!/bin/sh\ncd " elsewhere " || exit 9\nexec scheme \"$@\"\n"))
(sh (string-append "chmod +x " wrapper))
(define w1l (local! S (string-append "THEOURGIA_SCHEME=" wrapper) (string-append "import-code ./src --store " S " --wire")
                    #f client-dir))
(want "W1l no daemon, a relative path argument: resolved against the client's directory, the child's elsewhere"
      (list (rc-of w1l) (contains? (out-of w1l) "(ok ") (started-of w1l))
      (list 0 #t nothing-started))

;; ---- W1d2: a daemon appearing between the decision and the child ----------------------------
;;
;; NOTE: A HOLD AND A FAULT ARE COMPILED IN ONLY UNDER THEOURGIA_INJECT=on (ffi.sc's
;; fault section), so every call that arms one sets it.

(define release (string-append here "/release-d2"))
(define d2-out (string-append here "/d2.out"))
(define d2-err (string-append here "/d2.err"))
(define d2-client
  (start-bg! (string-append "cd " here " && " (env-prefix (string-append "THEOURGIA_WRITER=w2 THEOURGIA_TRACE=1 THEOURGIA_INJECT=on THEOURGIA_HOLD=frame-child-start:" release))
                            " scheme --script " client " write " B " held-then-written --store " S
                            " > " d2-out " 2> " d2-err)))
(define d2-held (wait-file! (string-append release ".held") 20000))
(define d2 (start-daemon! S "d2"))
(write-file! release "")
(wait-gone! d2-client 30000)
(define d2-log (daemon-log d2))
(stop-daemon! d2)
(want "W1d2 the client held after deciding nobody listens, a daemon then started on the store"
      (list d2-held (daemon-up d2))
      '(#t up))
(want "W1d2 the request is answered LOCALLY by the child: the daemon saw none, the answer has no rev"
      (list (contains? (file-text d2-out) "saved") (contains? (file-text d2-out) "(rev ")
            (contains? d2-log "daemon-dispatch"))
      '(#t #f #f))
(want "W1d2 the child held THEOURGIA_LOCAL=1, and the environment's writer is kept"
      (list (contains? (file-text d2-err) "(trace frame-local write 1)")
            (contains? (format "~s" (setup! (string-append "drafts --writer w2 --store " S))) B))
      '(#t #t))

;; ---- W2c, W2d: the store's lock held by another process ----------------------------------

(define c-holder (hold-lock! S 6))
(define c-start (now-ms))
(define w2c (local! S "" (string-append "insert --title AFTER-SIX --store " S " --wire") #f #f))
(define c-took (- (now-ms) c-start))
(wait-gone! c-holder 10000)
(want "W2c no daemon, the lock held for six seconds: a plain insert blocks and succeeds after the release"
      (list (rc-of w2c) (contains? (out-of w2c) "(ok (events") (>= c-took 4000) (started-of w2c))
      (list 0 #t #t nothing-started))
(define dc (start-daemon! S "c"))
(define c2-holder (hold-lock! S 7))
(define c2-start (now-ms))
(define w2c-daemon (client! "" (string-append "insert --title VIA-DAEMON --store " S " --wire") #f #f))
(define c2-took (- (now-ms) c2-start))
(wait-gone! c2-holder 10000)
(stop-daemon! dc)
(want "W2c (through a listening daemon, as before) store-busy at five seconds"
      (list (contains? (out-of w2c-daemon) "store-busy") (< 4000 c2-took 7000))
      '(#t #t))

(define d-holder (hold-lock! S 40))
(define d-start (now-ms))
(define w2d (local! S "THEOURGIA_TRACE=1" (string-append "insert --title AFTER-FORTY --store " S " --wire") #f #f))
(define d-took (- (now-ms) d-start))
(wait-gone! d-holder 10000)
(define w2d-child (let ((d (trace-datum (err-of w2d) 'frame-child))) (and d (caddr d))))
(want "W2d the lock held for forty seconds: the insert succeeds after the release, no transport timeout"
      (list (rc-of w2d) (contains? (out-of w2d) "(ok (events") (contains? (out-of w2d) "timeout") (>= d-took 35000)
            (started-of w2d))
      (list 0 #t #f #t nothing-started))
;; NEVER: "GONE AFTER THE CLIENT EXITS" IS NOT "REAPED BY THE CLIENT": an orphan
;; is reaped by init once its parent exits, so only the client's own wait
;; for the child it started tells the two apart.
(want "W2d the child was reaped by the client: its trace shows the wait's status for the child it started"
      (let ((d (trace-datum (err-of w2d) 'frame-child-exit)))
        (and d (integer? w2d-child) (list (equal? (caddr d) w2d-child) (cadddr d))))
      '(#t (exit 0)))

(define k-holder (hold-lock! S 40))
(define k-err (string-append here "/k.err"))
(define k-client
  (start-bg! (string-append "cd " here " && " (env-prefix "THEOURGIA_TRACE=1") " scheme --script " client
                            " insert --title ALONE --store " S " --wire > " here "/k.out 2> " k-err)))
(sh "sleep 10")
(define k-child (let ((d (trace-datum (file-text k-err) 'frame-child))) (and d (caddr d))))
(sh (string-append "kill -9 " (number->string k-client)))
(wait-gone! k-client 5000)
(wait-gone! k-holder 45000)
(wait-gone! k-child 30000)
(want "W2d the client killed at ten seconds: the child completes the insert alone after the release"
      (list (integer? k-child) (pid-gone? k-child)
            (contains? (format "~s" (setup! (string-append "outline --store " S))) "ALONE"))
      '(#t #t #t))

;; ---- the end ------------------------------------------------------------------------------

(want "END nothing this fixture's clients ran left a daemon behind"
      (list (daemon-count) (file-exists? (socket-path S)))
      '(0 #f))
(printf "rows: ~a\n~a failures\n" rows bad)
(printf "opener-route complete\n")
(sh (string-append "rm -rf " sock-here))
(exit (if (= bad 0) 0 1))
