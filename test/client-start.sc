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

;; Starting the daemon a store has not got yet, and saying why when it
;; will not start.
;;
;; KEY: THE REASON IS THE DAEMON'S, NOT THE CLIENT'S. "It did not come up"
;; is the one sentence a client can always produce and the one that helps
;; least: a socket path occupied by a file, a lock another daemon holds
;; and a store that will not open need three different things done about
;; them. So the daemon writes its refusal to a log the client named, and
;; the client hands that refusal back.
;;
;; NEVER: AND ONLY WHAT THIS START WROTE. The log is appended across every
;; start against a store, so a previous failure is sitting in it. The
;; client records the length before spawning and reads only past it; the
;; TWIN below is the row that fails if it does not, and it fails in the
;; direction that matters -- reporting a plausible, specific, wrong
;; reason for a start that in fact succeeded.
;;
;; NOTE: THE EXIT CODE IS NOT HERE. `75` belongs to the client PROGRAM,
;; which does not exist yet; these rows pin what the library answers, and
;; the program's rows join them when it is written.

(import (chezscheme)
        (theourgia client)
        (theourgia rpc)
        (theourgia ffi))

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

(define here (string-append scratch-base "/cstart-" (number->string (get-process-id))))
(define sock-here (string-append socket-base "/cstart-" (number->string (get-process-id))))
(system (string-append "rm -rf " here " " sock-here "; mkdir -p " here "/store " here "/home " sock-here "/run"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(putenv "THEOURGIA_RUN" (string-append sock-here "/run"))

(define store (string-append here "/store"))
(rpc-dispatch store '(init) "test")
(rpc-dispatch store '(insert "--title" "START-MARKER") "test")

(define sock (socket-path store))
(define log (serve-log-path store))
(define daemon "../theourgiad.sc")

;; NEVER: THE ARGV IS BUILT ONCE AND THE SOCKET IS ITS ONLY VARIABLE, so the
;; rows below differ in the thing they are about and in nothing else.
(define (argv-for socket)
  (list "scheme" "--script" daemon "serve" store
        "--detach" "--log" log "--socket" socket))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

;; NEVER: EVERY DAEMON THIS FILE CAUSED TO EXIST. Naming one store leaves any
;; other the fixture made still running, and the pattern is this run's own
;; directory -- which carries this process's pid, so it cannot reach
;; another run's daemons.
(define (kill-daemon!)
  (system (string-append "pkill -f 'serve " here "' 2>/dev/null"))
  (system "sleep 1"))

;; ---- CS-1 a cold start -----------------------------------------------------

(want "CS-1 a store with no daemon gets one"
      (ensure-daemon! (argv-for sock) store sock)
      'ready)

;; NEVER: "READY" HAS TO MEAN A DAEMON THAT ANSWERS. The readiness test is a
;; successful connection, and a connection succeeds against a socket that
;; is bound but not yet being read; this row is what says the difference
;; did not matter.
(want "CS-1 and it answers, from the store it was started for"
      (let ((r (call! sock (request-frame store 'outline '() (list (cons 'actor "tester"))) 5000)))
        (if (and (pair? r) (eq? 'answer (car r)))
            (if (let* ((text (utf8->string (cadr r))) (n (string-length text)))
                  (let scan ((i 0))
                    (cond ((> (+ i 12) n) #f)
                          ((string=? (substring text i (+ i 12)) "START-MARKER") #t)
                          (else (scan (+ i 1))))))
                'answered
                (list 'said (utf8->string (cadr r))))
            r))
      'answered)

;; ---- CS-2 a start that is not needed ---------------------------------------
;;
;; NOTE: NOT MERELY "IT SAID READY". An implementation that spawned anyway
;; also says ready: the second process loses the daemon's lock and exits,
;; leaving one daemon and a refusal appended to the log. So the row
;; measures the LOG, which is where the difference shows -- measured, a
;; losing start appends `(error serve-busy (path ...))` to it.
;;
;; NOTE: AND IT WAITS BEFORE LOOKING, WHICH IS THE WEAK PART OF THIS ROW.
;; Written without the wait it read the log the instant `ensure-daemon!`
;; returned and stayed GREEN against an implementation that spawned
;; unconditionally: that call returns as soon as the FIRST daemon
;; answers a connection, which is milliseconds, while the loser has yet
;; to start a Chez, take the lock, be refused and write. The wait is
;; four seconds because that is what the measurement needed; a loser
;; slower than that would still pass this row, and the row cannot be
;; made to settle the question by waiting longer.
(define before-second (string-length (file-text log)))
(define second-answer (ensure-daemon! (argv-for sock) store sock))
(system "sleep 4")

(want "CS-2 a store that already has a daemon is ready at once"
      second-answer 'ready)

(want "CS-2 TWIN: and nothing was started to find that out"
      (if (= (string-length (file-text log)) before-second)
          'nothing-was-started
          (list 'THE-LOG-GREW (substring (file-text log) before-second
                                         (string-length (file-text log)))))
      'nothing-was-started)

;; ---- CS-2 by counting instead of by waiting ------------------------------
;;
;; KEY: THE CLIENT ANNOUNCES EVERY PROCESS IT STARTS, at the moment it
;; starts it, so "did it start one?" is a count. The row above measures a
;; real side effect and is kept for that -- but it can only see the side
;; effect once the process it is about has got far enough to produce one,
;; and it cannot be made to settle the question by waiting longer. This
;; one is decided by the time `ensure-daemon!` has returned.
;;
;; NEVER: AND IT IS RUN OUT OF PROCESS, because the trace goes to stderr and
;; a fixture cannot read its own.
(define driver (string-append here "/drive.sc"))
(call-with-output-file driver
  (lambda (port)
    (for-each (lambda (l) (display l port) (newline port))
      (list "(import (chezscheme) (theourgia client) (theourgia rpc))"
            (string-append "(define store \"" store "\")")
            (string-append "(define sock \"" sock "\")")
            (string-append "(define log \"" log "\")")
            "(define argv"
            (string-append "  (list \"scheme\" \"--script\" \"" daemon "\" \"serve\" store")
            "        \"--detach\" \"--log\" log \"--socket\" sock))"
            "(write (ensure-daemon! argv store sock))"
            "(newline)"))))

(define (spawn-events-during-a-run tag)
  (let ((err (string-append here "/" tag ".err")))
    (system (string-append "CHEZSCHEMELIBDIRS='" (getenv "CHEZSCHEMELIBDIRS") "' "
                           "CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
                           "THEOURGIA_HOME=" here "/home "
                           "THEOURGIA_RUN=" sock-here "/run "
                           "THEOURGIA_TRACE=1 scheme --script " driver
                           " > /dev/null 2> " err))
    (let* ((text (file-text err))
           (n (string-length text)))
      (let count ((i 0) (seen 0))
        (cond ((> (+ i 13) n) seen)
              ((string=? (substring text i (+ i 13)) "(trace spawn ")
               (count (+ i 13) (+ seen 1)))
              (else (count (+ i 1) seen)))))))

;; The daemon from CS-1 is still up here.
(want "CS-2 with a daemon already answering, nothing is started"
      (spawn-events-during-a-run "warm")
      0)

(kill-daemon!)

;; NEVER: THE POSITIVE TWIN, without which the row above is passed by a
;; client that never starts anything at all.
(want "CS-2 TWIN: with no daemon, exactly one is started"
      (spawn-events-during-a-run "cold")
      1)

(kill-daemon!)

;; ---- CS-3 the refusal comes back ------------------------------------------

(define blocked (string-append sock-here "/blocked.sock"))
(system (string-append "echo not-a-socket > " blocked))

(want "CS-3 a socket path held by a file is reported in the daemon's own words"
      ;; F100b item 6: relayed as serve-start-failed whose kind is the
      ;; report's, the report's clauses verbatim (main's record, the attempt
      ;; token this start passed -- read here as its shape), then the exit.
      (let ((a (ensure-daemon! (argv-for blocked) store blocked)))
        (if (list? a)
            (map (lambda (c)
                   (if (and (pair? c) (eq? (car c) 'attempt) (pair? (cdr c)) (string? (cadr c))
                            (= 16 (string-length (cadr c))))
                       '(attempt <16-hex>)
                       c))
                 a)
            a))
      (list 'error 'serve-start-failed '(kind serve-path-occupied) (list 'path blocked)
            (list 'written (list (list 'create (string-append sock-here "/.blocked.sock.lock"))))
            '(attempt <16-hex>) '(exit 75)))

;; NEVER: AND THE FILE IS STILL THERE. A daemon that reported the path was
;; occupied and then took it anyway would have destroyed whatever was
;; there, and this row would be the only thing to notice.
(want "CS-3 and the file it refused to replace is untouched"
      (file-text blocked)
      "not-a-socket\n")

(system (string-append "rm -f " blocked))

;; ---- CS-4 TWIN: an older failure is not this one ---------------------------
;;
;; This start SUCCEEDS with errors already in the log, so it says that a
;; log full of old refusals does not by itself make a start look failed.
;;
;; NEVER: IT IS NOT THE ROW THAT CATCHES READING THE WHOLE FILE, although it
;; was written believing it was. Measured: with the offset replaced by 0,
;; this row stays GREEN -- a start that succeeds never reaches the code
;; that reads the log at all, so nothing it does can be wrong. CS-5 is
;; the row that goes red, and the note there says why.
(system (string-append "echo '(error stale-from-an-earlier-start)' >> " log))

(want "CS-4 TWIN: an error already in the log is not reported as this start's"
      (ensure-daemon! (argv-for sock) store sock)
      'ready)

(want "CS-4 TWIN: and the daemon that start produced does answer"
      (let ((r (call! sock (request-frame store 'outline '() (list (cons 'actor "tester"))) 5000)))
        (if (and (pair? r) (eq? 'answer (car r))) 'answered r))
      'answered)

(kill-daemon!)

;; ---- CS-5 nowhere to write ------------------------------------------------
;;
;; The daemon refuses before it serves, so there is no daemon and no
;; socket; and because the refusal could not be written down, the client
;; has nothing to relay and says so in the generic form -- WITH the path,
;; so the reason is still one command away.
;;
;; KEY: AND THIS IS THE ROW THAT CATCHES READING THE WHOLE LOG, which is
;; not where it was expected. The discriminating shape needs the stale
;; error to be the LAST error in the file, and that only happens when the
;; failing start writes nothing -- which is exactly this case. Measured
;; with the offset replaced by 0: this row answers
;; `stale-from-an-earlier-start`, a refusal from a different start
;; entirely, for a daemon that failed for a reason that has nothing to do
;; with it. Every other row in this file stays green under that seed.
(define ro (string-append here "/ro"))
(system (string-append "mkdir -p " ro "; chmod 500 " ro))
(define ro-log (string-append ro "/serve.log"))
(define ro-sock (string-append sock-here "/ro.sock"))

(want "CS-5 a daemon that cannot open its log does not start"
      (let ((answer (ensure-daemon!
                      (list "scheme" "--script" daemon "serve" store
                            "--detach" "--log" ro-log "--socket" ro-sock)
                      store ro-sock)))
        (list (if (and (pair? answer) (eq? 'error (car answer))) (cadr answer) answer)
              (if (file-exists? ro-sock) 'SERVED-ANYWAY 'no-socket)))
      '(serve-start-failed no-socket))


;; ---- CS-6 a close that fails after the answer -----------------------------
;;
;; NEVER: MEASURED DEFECT: the close that ends an exchange sat inside the
;; guard that classifies failures, AFTER a complete answer had been read.
;; A close that raised threw the answer away and reported
;; `transport-error` -- "this may or may not have happened" -- for a
;; request that provably had; the guard clause then closed a second time,
;; and a second failure escaped the guard altogether.
;;
;; NOTE: THE FAILURE IS ARMED, NOT WAITED FOR. `close(2)` on a socket does
;; not fail on demand, so `close-fail@client` makes it, and the client
;; names that step so the fault has somewhere to land.
;;
;; NOTE: BOTH RUNS USE `THEOURGIA_INJECT=on`, armed or not, so the twin is
;; the same build and not a different one.
(define close-driver (string-append here "/close.sc"))
(call-with-output-file close-driver
  (lambda (port)
    (for-each (lambda (l) (display l port) (newline port))
      (list "(import (chezscheme) (theourgia client) (theourgia rpc))"
            (string-append "(define store \"" store "\")")
            (string-append "(define sock \"" sock "\")")
            (string-append "(define log \"" log "\")")
            "(define argv"
            (string-append "  (list \"scheme\" \"--script\" \"" daemon "\" \"serve\" store")
            "        \"--detach\" \"--log\" log \"--socket\" sock))"
            ;; The exchange under test needs something to exchange with,
            ;; and starting it is not the step being measured: the fault
            ;; is scoped to the call, so nothing here can arm it.
            "(ensure-daemon! argv store sock)"
            "(let ((r (call! sock (request-frame store 'outline '() (list (cons 'actor \"tester\"))) 5000)))"
            "  (write (car r)) (newline)"
            ;; the answer's own bytes, so a row can ask whether THIS answer
            ;; survived rather than whether some answer came back
            "  (when (eq? 'answer (car r))"
            "    (write (utf8->string (cadr r))) (newline)))"))))

(define (outcome-under-fault tag fault)
  (let ((out (string-append here "/" tag ".out")))
    (system (string-append "CHEZSCHEMELIBDIRS='" (getenv "CHEZSCHEMELIBDIRS") "' "
                           "CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
                           "THEOURGIA_HOME=" here "/home "
                           "THEOURGIA_RUN=" sock-here "/run "
                           "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 "
                           (if fault (string-append "THEOURGIA_FAULT=" fault " ") "")
                           "scheme --script " close-driver
                           " > " out " 2> " out ".err"))
    (list (file-text out) (file-text (string-append out ".err")))))

;; NEVER: "IT ANSWERED" IS NOT "IT ANSWERED THIS". A build that threw the real
;; reply away and returned some other answer would satisfy a row that only
;; looked for the word; what must survive the failed close is the outline
;; the daemon actually sent, so the driver prints the answer's own text and
;; the row compares it with the answer the same call gives with nothing
;; armed.
(define (outcome-with-close-fault armed?)
  (let ((r (outcome-under-fault (if armed? "close-armed" "close-clear")
                                (and armed? "close-fail@client"))))
    (list (if (contains? (car r) "answer") 'answer (list 'said (car r)))
          (if (contains? (cadr r) "close-failed")
              'close-really-failed
              'no-close-failure-seen))))

(want "CS-6 a close that fails after the answer does not take the answer away"
      (outcome-with-close-fault #t)
      '(answer close-really-failed))

;; NEVER: AND IT IS THE SAME ANSWER. The row above asks only that an answer came
;; back; a build that discarded the reply and produced another one would
;; pass it. The bytes the daemon sent are compared with the bytes the same
;; call produces with nothing armed.
(want "CS-6 and the answer that survives is the one the daemon sent"
      (let ((armed (car (outcome-under-fault "close-armed2" "close-fail@client")))
            (clear (car (outcome-under-fault "close-clear2" #f))))
        (let ((body (lambda (text)
                      (let ((at (let loop ((i 0))
                                  (cond ((>= i (string-length text)) #f)
                                        ((char=? (string-ref text i) #\newline) i)
                                        (else (loop (+ i 1)))))))
                        (and at (substring text at (string-length text)))))))
          (cond ((not (body armed)) (list 'armed-printed-no-answer armed))
                ((equal? (body armed) (body clear)) 'same-bytes)
                (else (list 'armed (body armed) 'clear (body clear))))))
      'same-bytes)

;; NEVER: THE TWIN, or the row above passes on a build where the fault never
;; fired: the same build, the same call, nothing armed. It must also
;; answer -- and its second element is what tells the two runs apart.
(want "CS-6 TWIN: and with nothing armed the same call answers with no close failure"
      (outcome-with-close-fault #f)
      '(answer no-close-failure-seen))


;; ---- CS-9 a write that moved no bytes did not send the request -----------
;;
;; NEVER: EVERY FAILURE AFTER THE CONNECT USED TO BE "IT MAY HAVE HAPPENED".
;; That is the safe answer for a failure that could have left bytes on the
;; wire, and the wrong one for a write that refused before moving any: the
;; request provably never went out, and a caller told "unknown" cannot
;; retry something it was free to retry. What separates them is a COUNT,
;; not an errno -- `write-all!` reports its progress, and zero is zero.
;;
;; NOTE: THE FAILURE IS ARMED. `write-eio-first@client` refuses the first
;; write of the client's own step and lets the rest through, which is the
;; one shape this distinction is about.
(want "CS-9 a first write that fails is reported as not sent"
      (let ((r (outcome-under-fault "wfail" "write-eio-first@client")))
        (cond ((contains? (car r) "not-sent") 'not-sent)
              ((contains? (car r) "transport-error") 'SAID-IT-MIGHT-HAVE-HAPPENED)
              (else (list 'said (car r)))))
      'not-sent)

;; NEVER: THE TWIN: the same build with nothing armed answers. Without it the
;; row above passes on a client that reports `not-sent` for everything.
(want "CS-9 TWIN: and with nothing armed the same call is answered"
      (let ((r (outcome-under-fault "wclear" #f)))
        (if (contains? (car r) "answer") 'answer (list 'said (car r))))
      'answer)

;; NEVER: AND THE OTHER SIDE OF THE COUNT, which is the half that decides
;; whether this is a rule or a slogan. A write that fails AFTER some bytes
;; have gone out may have been carried out, and must stay unknown. With
;; only the row above, a client that answered `not-sent` for every write
;; failure -- the safest-sounding and most dangerous answer, because it
;; invites the caller to retry something that may already have happened --
;; passes.
(want "CS-9 a write that fails after bytes have gone is not reported as not-sent"
      (let ((r (outcome-under-fault "wpartial" "write-eio-after-partial@client")))
        (cond ((contains? (car r) "not-sent") 'CALLED-IT-NOT-SENT)
              ((contains? (car r) "transport-error") 'unknown)
              ((contains? (car r) "answer") 'answered-anyway)
              (else (list 'said (car r)))))
      'unknown)

;; ---- CS-7 the daemon's own words, after non-ASCII in the log --------------
;;
;; NEVER: MEASURED DEFECT: the length taken before the spawn is a count of
;; BYTES, and it was used as a `substring` index, which counts
;; CHARACTERS. With any non-ASCII already in the log the slice began too
;; far in and ate the front of the refusal that start had just written --
;; measured as `rror serve-path-occupied)`, which reads as no datum at
;; all, so the caller got the generic "it would not start" while the real
;; reason sat in the file.
(let* ((noisy-store (string-append here "/noisy"))
       (noisy-log (serve-log-path noisy-store))
       (occupied (string-append sock-here "/taken.sock")))
  (system (string-append "mkdir -p " noisy-store "/.. 2>/dev/null; mkdir -p "
                         (substring noisy-log 0 (let loop ((i (- (string-length noisy-log) 1)))
                                                  (if (char=? (string-ref noisy-log i) #\/) i (loop (- i 1)))))))
  ;; Two characters, four bytes: exactly the gap between the two counts.
  (call-with-output-file noisy-log
    (lambda (port) (display "(note 存)\n" port)))
  (system (string-append "printf x > " occupied))
  (let ((answer (ensure-daemon!
                  (list "scheme" "--script" daemon "serve" noisy-store
                        "--detach" "--log" noisy-log "--socket" occupied)
                  noisy-store occupied)))
    (want "CS-7 a refusal written after non-ASCII in the log is relayed whole"
          (if (and (list? answer) (eq? 'error (car answer)))
              (assq 'kind (cddr answer))
              (list 'said answer))
          '(kind serve-path-occupied))))

;; ---- CS-8 a run root that cannot be written to ----------------------------
;;
;; NEVER: MEASURED DEFECT: the log's directory was made OUTSIDE the guard, so
;; a run root that could not be written raised out of `call!` entirely --
;; past every outcome this library defines, to a caller with no handler
;; for it. It is a start that failed, and is now answered as one.
(let* ((locked-run (string-append sock-here "/locked"))
       (locked-store (string-append here "/locked-store")))
  (system (string-append "mkdir -p " locked-run "; chmod 500 " locked-run))
  (let ((answer (guard (e (#t (list 'RAISED-OUT)))
                  (parameterize ()
                    (let ((old (getenv "THEOURGIA_RUN")))
                      (putenv "THEOURGIA_RUN" locked-run)
                      (let ((r (ensure-daemon!
                                 (list "scheme" "--script" daemon "serve" locked-store)
                                 locked-store (string-append locked-run "/s.sock"))))
                        (putenv "THEOURGIA_RUN" (or old ""))
                        r))))))
    (want "CS-8 a run root that cannot be written is a start that failed, not a raise"
          (cond ((equal? answer '(RAISED-OUT)) 'RAISED-OUT)
                ((and (pair? answer) (eq? 'error (car answer))) 'answered-an-error)
                (else (list 'said answer)))
          'answered-an-error))
  (system (string-append "chmod 700 " locked-run)))


;; ---- the client's import closure, asserted where the client is worked on --
;;
;; NEVER: `closures.sc` HAS THIS ROW, AND IT IS NOT WHERE THE WORK HAPPENS.
;; Anyone changing the client runs these suites; a stray import of the
;; core, the scheduler or the networking library would be caught only by
;; a file they had no reason to run, and only if the whole suite ran. The
;; walk itself is `import-walk.sc`, shared, so this is the same
;; measurement taken in an extra place -- NEVER: not a second implementation
;; of it.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define tree-root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/core.sc")) up script-dir)))
(load (string-append script-dir "/import-walk.sc"))

(define import-graph
  (let loop ((names (source-files tree-root)) (out '()))
    (if (null? names)
        out
        (let* ((path (string-append tree-root "/" (car names)))
               (declared (declared-library-name path)))
          (loop (cdr names)
                (if (and (pair? declared) (eq? 'theourgia (car declared)) (pair? (cdr declared)))
                    (cons (cons (cadr declared)
                                (map cadr (filter (lambda (r) (pair? (cdr r)))
                                                  (imports-of-file 'theourgia path))))
                          out)
                    out))))))

(define (closure-from start)
  (let loop ((todo (list start)) (seen '()))
    (cond
      ((null? todo)
       (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
      ((memq (car todo) seen) (loop (cdr todo) seen))
      (else
       (let ((edges (cond ((assq (car todo) import-graph) => cdr) (else '()))))
         (loop (append edges (cdr todo)) (cons (car todo) seen)))))))

(want "IMPORTS the graph was read at all"
      (if (null? import-graph) 'NOTHING-WAS-READ 'read-the-tree)
      'read-the-tree)

(want "IMPORTS the client reaches neither the core nor the scheduler nor the network"
      (filter (lambda (n) (memq n '(rpc daemon sched net store log working reduce)))
              (closure-from 'client))
      '())

;; (theourgia answers) is in it since F100b item 6: the client's own
;; failures are answered by the table (classify-failure).
;; (theourgia platform-numbers) is in it because ffi.sc imports it: every
;; platform number the FFI uses comes from the running platform's row.
(want "IMPORTS and the client's closure is exactly what it should be"
      (closure-from 'client)
      '(answers client digest ffi incomplete platform-numbers render trace))

;; NEVER: AND THE PROGRAM'S OWN CLOSURE, not only the library's. A person runs
;; `theourgia.sc`; what IT reaches is a separate fact from what the
;; `client` library reaches, and the rows above are about the library.
;; `arguments` is allowed and the reason is written in `closures.sc`: the
;; program asks that table whether a verb reads standard input rather than
;; keeping a second copy of it, and the table reaches nothing else.
;; `answers` is allowed for a reason also written there: the program turns
;; a filesystem failure into the one table answer (F100b point 4).
;; (theourgia platform-numbers) is in it because ffi.sc imports it: every
;; platform number the FFI uses comes from the running platform's row.
(want "IMPORTS and the client program's closure is exactly what it should be"
      (let ((program-imports
              (map cadr (filter (lambda (r) (pair? (cdr r)))
                                (imports-of-file 'theourgia
                                                 (string-append tree-root "/theourgia.sc"))))))
        (let loop ((todo program-imports) (seen '()))
          (cond
            ((null? todo)
             (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
            ((memq (car todo) seen) (loop (cdr todo) seen))
            (else
             (let ((edges (cond ((assq (car todo) import-graph) => cdr) (else '()))))
               (loop (append edges (cdr todo)) (cons (car todo) seen)))))))
      '(answers arguments client digest ffi incomplete platform-numbers render trace))


(system (string-append "chmod 700 " ro))
;; NEVER: AND IT IS TAKEN DOWN AT THE END, not only between rows. Whichever
;; daemon was alive when the last row finished used to survive the run:
;; measured by the suite's leak gate, which counts processes and is the
;; only thing here that was looking.
(kill-daemon!)
(system (string-append "chmod -R u+rwx " sock-here " 2>/dev/null; rm -rf " here " " sock-here))
(printf "rows: ~a\n~a failures\nclient-start complete\n" rows bad)
(exit (if (zero? bad) 0 1))
