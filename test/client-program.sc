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

;; `theourgia` -- the client program.
;;
;; KEY: IT KNOWS TRANSPORT AND NOTHING ELSE, and these rows are about that
;; boundary: it finds the daemon, starts one when there is none, writes
;; back the bytes it was given and exits with the number it was handed.
;; Every row that could be satisfied by the client working something out
;; for itself has a twin that says it did not.

(import (chezscheme))

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

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define here (string-append "/tmp/cprog-" (number->string (get-process-id))))
(define store (string-append here "/store"))

(system (string-append "rm -rf " here "; mkdir -p " store " " here "/home " here "/run"))

;; NOTE: `THEOURGIA_TRACE=1` IS SET FOR EVERY RUN, INCLUDING THE DAEMON'S.
;; The client passes its environment on to the daemon it starts, so this
;; is what makes the daemon record who it served -- and that record is
;; the only thing that can tell the two routes apart, because they answer
;; identically on purpose.
(define (env-prefix extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                 "THEOURGIA_HOME=" here "/home THEOURGIA_RUN=" here "/run "
                 "THEOURGIA_TRACE=1 " extra))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

;; NEVER: AN ANSWER IS NOT ALWAYS THE ANSWER YOU EXPECTED. `assq` raises on
;; anything that is not a proper list of pairs, and `(error no-store ...)`
;; is exactly that shape -- so a row that merely wanted a block id took
;; the whole fixture down when an earlier step had failed, and the failure
;; it reported was the fixture's and not the product's. Measured on the
;; previous build, where `init` refuses.
(define (field-of-answer datum name)
  (let look ((xs (and (pair? datum) (cdr datum))))
    (cond ((not (pair? xs)) #f)
          ((and (pair? (car xs)) (eq? name (caar xs))) (car xs))
          (else (look (cdr xs))))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; NEVER: STDOUT AND STDERR ARE KEPT APART. Half of what is asked below is
;; "did anything other than the answer reach stdout", and a helper that
;; merged the two could not answer it.
(define n-run 0)
(define (run! program extra args)
  (set! n-run (+ n-run 1))
  (let* ((tag (string-append here "/r" (number->string n-run)))
         (out (string-append tag ".out"))
         (err (string-append tag ".err"))
         (rc (system (string-append (env-prefix extra) " scheme --script " program
                                    " " args " > " out " 2> " err))))
    (list rc (file-text out) (file-text err))))

(define (client extra args) (run! "../theourgia.sc" extra args))
(define (server extra args) (run! "../cli.sc" extra args))

(define (rc-of r) (car r))
(define (out-of r) (cadr r))
(define (err-of r) (caddr r))

(define (daemons-alive)
  (let ((f (string-append here "/count.txt")))
    (system (string-append "pgrep -f 'serve " store "' | wc -l | tr -d ' ' > " f))
    (let ((t (file-text f)))
      (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) "?"))))

;; NEVER: EVERY DAEMON THIS FILE CAUSED TO EXIST, not the one store that was
;; thought of when this was written. Rows added later make their own
;; stores -- a bound-identity store, a store for the relative-path case --
;; and a pattern naming one of them leaves the others running: measured,
;; two daemons still alive after a run, found by the suite's leak gate and
;; not by any row here. The pattern is this run's own directory, which
;; every store of this fixture is under, and which carries this process's
;; pid so it cannot match another run's.
(define (kill-daemons!)
  (system (string-append "pkill -f 'serve " here "' 2>/dev/null"))
  (system "sleep 1"))

;; ---- P-1 a store with no daemon --------------------------------------------

(define made (client "" (string-append "init --store " store)))
(want "P-1 init works with no daemon anywhere"
      (if (contains? (out-of made) "(ok (store") 'created (list 'said (out-of made)))
      'created)

;; NEVER: AND IT STARTED NOTHING. `init` is one of the three verbs that run in
;; this process; a client that had tried the socket first would have
;; started a daemon for a store that did not exist yet.
(want "P-1 TWIN: and no daemon was started to do it"
      (daemons-alive)
      "0")

(define first-call
  (client "" (string-append "insert --title PROGRAM-MARKER --store " store " --wire")))

(want "P-2 the first call that needs a daemon starts one and is answered"
      (if (contains? (out-of first-call) "(ok (events") 'answered
          (list 'said (out-of first-call)))
      'answered)

(want "P-2 and exactly one daemon is running"
      (daemons-alive)
      "1")

;; KEY: THE ANSWER IS THE WHOLE OF STDOUT. The daemon this call started
;; inherits the client's streams until it redirects them, and it prints a
;; line about its machine home on the way up -- measured, that line goes
;; to STDERR. If it ever goes to stdout instead, every caller parsing the
;; answer gets a datum it did not ask for, first.
(want "P-2 TWIN: and nothing but the answer reached stdout"
      (let ((text (out-of first-call)))
        (if (and (> (string-length text) 0) (char=? #\( (string-ref text 0))
                 (not (contains? text "machine-home")))
            'only-the-answer
            (list 'stdout text)))
      'only-the-answer)

;; ---- P-3 the exit code is the server's -------------------------------------

(want "P-3 an answer that succeeded exits 0"
      (rc-of (client "" (string-append "outline --store " store " --wire")))
      0)

(define refused (client "" (string-append "read nosuch.1 --store " store " --wire")))

(want "P-3 an answer that was refused exits non-zero"
      (if (zero? (rc-of refused)) (list 'EXITED-ZERO (out-of refused)) 'non-zero)
      'non-zero)

;; NEVER: AND THE REFUSAL IS STILL ON STDOUT, because it is the answer. A
;; client that treated a non-zero code as "something went wrong with me"
;; and wrote the text to stderr would break every caller that reads the
;; answer.
(want "P-3 TWIN: and the refusal is the answer, on stdout"
      (if (contains? (out-of refused) "(error unknown-id") 'on-stdout
          (list 'stdout (out-of refused) 'stderr (err-of refused)))
      'on-stdout)

;; ---- P-4 the same answer as the server gives -------------------------------
;;
;; KEY: BYTE FOR BYTE, BOTH MODES. The client renders nothing: what it
;; writes is what the server rendered. If it ever started rendering, this
;; is the row that says so.
(want "P-4 the client's bytes are the server's, in wire mode"
      (let ((a (out-of (client "" (string-append "outline --store " store " --wire"))))
            (b (out-of (server "" (string-append "outline --store " store " --wire")))))
        (if (string=? a b) 'byte-for-byte (list 'client a 'server b)))
      'byte-for-byte)

(want "P-4 the client's bytes are the server's, in human mode"
      (let ((a (out-of (client "" (string-append "outline --store " store))))
            (b (out-of (server "" (string-append "outline --store " store)))))
        (if (string=? a b) 'byte-for-byte (list 'client a 'server b)))
      'byte-for-byte)

;; ---- P-5 THEOURGIA_LOCAL ---------------------------------------------------
;;
;; NOTE: THE ROW IS "IT DID NOT USE THE DAEMON", not "the answer was right":
;; the two routes give the same answer on purpose, so the answer cannot
;; tell them apart. The daemon says who it served, in its trace.
;; ---- P-4b the same answer for a verb whose payload is not a list of names
;;
;; NEVER: TWO ROUTES AGREEING ON NOTHING IS NOT AGREEMENT. The rows above
;; compare an outline, which this store has. A verb that answered nothing at
;; all through both routes would satisfy any byte-for-byte row perfectly, so
;; this pair asks for a payload first and compares second: the answer must
;; carry an id and a line number before the comparison is worth making.
;;
;; `grep` is the verb chosen because its items were added after the client
;; existed, and because its answer carries a number in a position where
;; `search` carries a score -- the shape the two routes could most easily
;; disagree about without either looking wrong.
(let ((made (client "" (string-append "insert --title SEEKME --text \"a line holding seekme\" --store "
                                      store " --wire"))))
  (want "P-4b CONTROL: the block was written, so there is something to find"
        (if (contains? (out-of made) "(ok") 'written (list 'said (out-of made)))
        'written))

(let ((a (out-of (client "" (string-append "grep seekme --store " store " --wire"))))
      (b (out-of (server "" (string-append "grep seekme --store " store " --wire")))))

  (want "P-4b CONTROL: the payload is not empty, and carries an id and a line number"
        (list (contains? a "(match ")
              (contains? a "(items)"))
        (list #t #f))

  (want "P-4b the client's bytes are the server's for grep, in wire mode"
        (if (string=? a b) 'byte-for-byte (list 'client a 'server b))
        'byte-for-byte))

(let ((a (out-of (client "" (string-append "grep seekme --store " store))))
      (b (out-of (server "" (string-append "grep seekme --store " store)))))
  (want "P-4b and in human mode, where the clauses are dropped and the lines are not"
        (list (if (string=? a b) 'byte-for-byte (list 'client a 'server b))
              (contains? a "(match ")
              (contains? a "scanned"))
        (list 'byte-for-byte #t #f)))

(define dispatch-log (string-append here "/dispatch.txt"))

;; NEVER: THE LOG IS WHERE THE CLIENT PUT IT, which is under the run root and
;; keyed on the store -- NOT beside the fixture. Written as
;; `<here>/serve.log` this counted a file that does not exist: `grep`
;; answered 0 both times, the two readings agreed, and the row asserting
;; "the daemon saw nothing" passed while measuring nothing at all. Its
;; twin is what caught it.
;;
;; NOTE: The key is a hash of the store's resolved path, so the directory is
;; matched rather than computed -- computing it here would be this file
;; keeping its own copy of the client's rule.
(define (dispatches)
  (system (string-append "cat " here "/run/*/serve.log 2>/dev/null"
                         " | grep -c daemon-dispatch > " dispatch-log
                         " 2>/dev/null || echo 0 > " dispatch-log))
  (let ((t (file-text dispatch-log)))
    (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) "?")))

(define before-local (dispatches))
(define local-run
  (client "THEOURGIA_LOCAL=1 " (string-append "outline --store " store " --wire")))

(want "P-5 THEOURGIA_LOCAL answers without the daemon"
      (list (if (contains? (out-of local-run) "PROGRAM-MARKER") 'answered
                (list 'said (out-of local-run)))
            (if (string=? (dispatches) before-local) 'and-the-daemon-saw-nothing
                (list 'dispatches before-local '-> (dispatches))))
      '(answered and-the-daemon-saw-nothing))

;; TWIN: the same request WITHOUT the variable does reach the daemon --
;; without this, the row above is passed by a daemon that never sees
;; anything at all.
(define remote-run (client "" (string-append "outline --store " store " --wire")))

(want "P-5 TWIN: and without it, the daemon does serve the request"
      (if (string=? (dispatches) before-local)
          (list 'STILL (dispatches))
          'the-daemon-served-it)
      'the-daemon-served-it)

(want "P-5 TWIN: and both routes gave the same bytes"
      (if (string=? (out-of local-run) (out-of remote-run)) 'byte-for-byte
          (list 'local (out-of local-run) 'remote (out-of remote-run)))
      'byte-for-byte)

;; ---- P-6 the separator ------------------------------------------------------
;;
;; NEVER: NOTHING AFTER `--` IS AN OPTION, INCLUDING THE FOUR THIS PROGRAM
;; UNDERSTANDS. The separator is how a caller passes a value that looks
;; like an option; a scanner that read past it took the very arguments the
;; caller had protected. Measured before the fix:
;; `search -- --store zzz --store <path>` lost BOTH occurrences and left
;; `search` with no query at all.

(client "" (string-append "insert --title \"has --store inside\" --store " store " --wire"))

(want "P-6 a literal that looks like an option survives the separator"
      (let ((r (client "" (string-append "search --store " store " --wire -- --store"))))
        (if (contains? (out-of r) "has --store inside") 'searched-for-the-literal
            (list 'said (out-of r))))
      'searched-for-the-literal)

;; NEVER: AND IT REALLY WAS THE SEARCH TERM. Without this, the row above is
;; also passed by a client that dropped the argument entirely and matched
;; on something else.
;; NEVER: THIS ASKS ABOUT THE ITEMS, NOT ABOUT THE WHOLE ANSWER. It used to
;; look for the text `(ok (items))` -- the entire answer, spelled out -- and
;; went red the day `search` began carrying `cut`, `scanned` and `coverage`
;; beside its items. Nothing about what this row is for had changed: it
;; wants to know that the search after the separator found nothing.
;;
;; A row that pins a whole answer is red for every addition to it, and the
;; reading it gives names the clause that was added rather than the thing
;; that broke.
(want "P-6 TWIN: a different literal after the separator finds nothing"
      (let ((r (client "" (string-append "search --store " store " --wire -- --nosuchthing"))))
        (if (and (contains? (out-of r) "(ok (items)")
                 (not (contains? (out-of r) "(hit ")))
            'no-hits
            (list 'said (out-of r))))
      'no-hits)

;; NEVER: AND ROUTING BEFORE THE SEPARATOR STILL ROUTES. A scanner that
;; stopped reading options altogether would pass both rows above.
(want "P-6 TWIN: options before the separator still work"
      (let ((r (client "" (string-append "outline --store " store " --wire"))))
        (if (contains? (out-of r) "PROGRAM-MARKER") 'routed (list 'said (out-of r))))
      'routed)

;; ---- P-7 a socket path that cannot fit --------------------------------------
;;
;; NEVER: A PATH THAT WILL NOT FIT IS AN ANSWER, NOT AN EXCEPTION. `sun_path`
;; holds 104 bytes; `sockaddr-un` refuses a longer one, correctly and
;; loudly -- but by raising, and nothing above caught it. A run root long
;; enough ended the program with
;; `Exception in sockaddr-un: ...` on stderr, no answer, and status 255.
;; This program's whole contract is that stdout carries the answer and the
;; exit code is the verdict; a raw condition is neither.
;;
;; NOTE: AND IT IS `socket-path-too-long`, NOT `transport-unknown`. Nothing
;; was sent, so the outcome is not unknown -- it is known, and it is that
;; the request did not happen.
(define long-run
  (string-append here "/"
                 (let build ((n 116) (out "")) (if (zero? n) out (build (- n 1) (string-append out "r"))))))

(define too-long-run
  (run! "../theourgia.sc"
        (string-append "THEOURGIA_RUN=" long-run " ")
        (string-append "outline --store " store " --wire")))

(want "P-7 a socket path over the limit is refused, in an answer"
      (list (if (contains? (out-of too-long-run) "(error socket-path-too-long") 'named
                (list 'stdout (out-of too-long-run)))
            (if (contains? (out-of too-long-run) "transport-unknown") 'CALLED-IT-UNKNOWN 'not-unknown))
      '(named not-unknown))

(want "P-7 and it carries the numbers that explain it"
      (if (and (contains? (out-of too-long-run) "(length ")
               (contains? (out-of too-long-run) "(max 104)"))
          'both-numbers
          (list 'said (out-of too-long-run)))
      'both-numbers)

(want "P-7 and it exits 75 rather than dying"
      (if (= 255 (rc-of too-long-run)) 'DIED (rc-of too-long-run))
      75)

;; ---- P-8 a store that is not there -----------------------------------------
;;
;; NEVER: THE COMMON FAILURE HAS ITS OWN NAME. A daemon asked to serve a path
;; holding no store failed while opening it and reported
;; `(error store-load-failed (reason raised))` -- `raised` because the
;; condition carried no message, so the field meant to say why said only
;; that something had. The local path has always answered `no-store`.
(want "P-8 a store that does not exist is named as that"
      (let ((r (client "" (string-append "outline --store " here "/no-such-store --wire"))))
        (if (contains? (out-of r) "(error store-not-found") 'named
            (list 'said (out-of r))))
      'named)

(want "P-8 TWIN: and a store that does exist is served"
      (let ((r (client "" (string-append "outline --store " store " --wire"))))
        (if (contains? (out-of r) "PROGRAM-MARKER") 'served (list 'said (out-of r))))
      'served)


;; ---- P-12 what the caller piped in reaches the verb that reads it ---------
;;
;; NEVER: MEASURED DEFECT, AND ONE OF THEM WAS SILENT. The envelope has
;; always carried a `stdin` field and the daemon never read it, so on the
;; default route `batch` -- whose intents ARE its standard input -- got
;; none and answered its usage line, and `write <id> -` stored the
;; literal string "-" while answering `(ok (saved ...))`. The second is
;; the worse one: the caller's bytes were discarded and the answer said
;; the write had succeeded.
;;
;; NOTE: THE TWO ROUTES ARE COMPARED BYTE FOR BYTE, on one store, with an
;; intent that fails the same way every time -- so the answer carries no
;; identifier that could differ between the runs for an innocent reason.
;; NEVER: NOTHING HERE RUNS A STDIN-READING VERB WITHOUT GIVING IT STDIN.
;; Two calls used to stand above this for no reason but to keep a counter
;; in step, and they ran `batch` with whatever standard input the SUITE
;; had. Under a terminal -- a run inside `screen` -- that never reaches
;; end of file, so the fixture hung until the runner's alarm killed it at
;; 900 s, having printed 21 of its 41 rows, and the leak gate reported the
;; three processes the hung chain was holding. Measured by the main
;; session; this environment hid it, because here the suite's standard
;; input is not a terminal.
(let* ((intents "((del \"nosuch.1\"))")
       (echo (string-append "printf '%s' '" intents "' | ")))
  ;; The helper cannot pipe, so both runs go through a shell that can.
  (let* ((lout (string-append here "/batch-local.out"))
         (dout (string-append here "/batch-daemon.out"))
         (cmd (lambda (extra out)
                (string-append echo (env-prefix extra) " scheme --script ../theourgia.sc "
                               "batch --store " store " --wire > " out " 2>/dev/null"))))
    (system (cmd "THEOURGIA_LOCAL=1 " lout))
    (system (cmd "" dout))
    (want "P-12 a batch reaches the daemon with its intents"
          (let ((text (file-text dout)))
            (cond ((contains? text "(usage") 'GOT-NO-INTENTS)
                  ((contains? text "(batch") 'a-batch-answer)
                  (else (list 'said text))))
          'a-batch-answer)

    ;; NEVER: AND THE TWO ROUTES ANSWER THE SAME BYTES. "It worked on both"
    ;; would also be true of two builds that disagreed about what the
    ;; intents meant.
    (want "P-12 and the daemon route answers exactly what the local route does"
          (if (string=? (file-text lout) (file-text dout))
              'identical
              (list 'local (file-text lout) 'daemon (file-text dout)))
          'identical)))

;; NEVER: AND THE SILENT ONE. `write <id> -` takes its bytes from standard
;; input; with none arriving, "-" was stored as the text and the answer
;; said the write succeeded.
(let* ((made (server "" (string-append "insert --title P12 --text old --store " store " --wire")))
       (id (let* ((datum (guard (e (#t #f)) (read (open-string-input-port (out-of made)))))
                  (evs (and (pair? datum) (field-of-answer datum 'events))))
             (and (pair? evs) (pair? (cadr evs))
                  (let ((ev (car (cadr evs))))
                    (string-append (car ev) "." (number->string (cdr ev)))))))
       (out (string-append here "/dash.out")))
  (system (string-append "printf 'PIPED-BYTES' | "
                         (env-prefix "THEOURGIA_WRITER=w1 ")
                         " scheme --script ../theourgia.sc write " id " - --store " store
                         " --wire > " out " 2>/dev/null"))
  (want "P-12 a write that takes its bytes from standard input stores those bytes"
        (let ((back (client "THEOURGIA_WRITER=w1 "
                            (string-append "read " id " --working --store " store " --wire"))))
          (cond ((contains? (out-of back) "PIPED-BYTES") 'stored-what-was-piped)
                ((contains? (out-of back) "\"-\"") 'STORED-THE-HYPHEN)
                (else (list 'said (out-of back)))))
        'stored-what-was-piped))

;; ---- P-17 what a fixture inherits from whoever started the suite ---------
;;
;; NEVER: MEASURED, AND THIS ENVIRONMENT HID IT. A verb whose input comes from
;; standard input reads whatever standard input the fixture inherited. Run
;; from a terminal -- inside `screen` -- that never reaches end of file, so
;; the read waits forever: `client-program` hung at 21 of its 41 rows until
;; the runner's alarm killed it at 900 seconds, and the leak gate reported
;; the processes the hung chain was holding. Run where standard input is
;; not a terminal, the same fixture passes, which is why it was green here
;; and red for the main session.
;;
;; NOTE: THE RULE IS THE RUNNER'S: every fixture is given `/dev/null`, and a
;; fixture that wants input hands it over itself. These two rows are the
;; measurement and the rule -- the first shows a terminal really does hang
;; this call, the second shows the runner really does redirect.
(let* ((probe (string-append here "/pty-probe.sh"))
       (timed (lambda (redirect)
                (let ((out (string-append here "/pty" (if redirect "-null" "-tty") ".out")))
                  (call-with-output-file probe
                    (lambda (port)
                      (put-string port
                        (string-append
                          "#!/bin/sh\n"
                          (env-prefix "") " timeout 12 scheme --script ../theourgia.sc "
                          "batch --store " store " --wire"
                          (if redirect " < /dev/null" "") " > /dev/null 2>&1\n"
                          "echo rc=$? \n")))
                    'truncate)
                  (system (string-append "chmod +x " probe))
                  ;; `script` gives the child a pseudo-terminal, which is
                  ;; what the suite inherits when a person runs it.
                  (system (string-append "script -q /dev/null " probe " > " out " 2>&1"))
                  (let ((t (file-text out)))
                    (cond ((contains? t "rc=124") 'hung-until-killed)
                          ((contains? t "rc=") 'answered)
                          (else (list 'said t))))))))
  (want "P-17 under a terminal, a verb that reads standard input waits forever"
        (timed #f)
        'hung-until-killed)

  (want "P-17 TWIN: and with /dev/null on its standard input it answers"
        (timed #t)
        'answered))

;; NEVER: AND THE RUNNER IS WHAT SUPPLIES IT. The rows above are about a shell
;; command; this one is about the suite every fixture is run by, so that
;; "fixtures get /dev/null" cannot quietly stop being true.
(want "P-17 the suite runner gives every fixture /dev/null on standard input"
      (let ((t (file-text "run-fixtures.sh")))
        (if (contains? t "$runner \"$f\" > \"$out/$n.out\" 2>&1 < /dev/null")
            'it-redirects
            'NO-REDIRECT-IN-THE-RUNNER))
      'it-redirects)

;; ---- P-18 the reply's second stream ---------------------------------------
;;
;; NEVER: EVERY OTHER ROW HERE WOULD PASS ON A CLIENT THAT THREW `stderr`
;; AWAY. The field is empty in every answer the core produces today, so a
;; build that never wrote it out looks exactly like one that does -- and
;; the field exists precisely because the client also runs the server
;; locally, where a real second stream exists and has to arrive somewhere.
;;
;; NOTE: A STAND-IN PEER PUTS SOMETHING IN IT, which no real verb does yet,
;; and the row asks that it came out on the client's own stderr and NOT on
;; its stdout -- mixing the two is how a caller that parses the answer
;; starts parsing a diagnostic.
(let* ((esock (string-append here "/stderr.sock"))
       (epeer (string-append here "/stderr.sc")))
  (call-with-output-file epeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              "(define reply (string->utf8 \"(answer (stdout \\\"(ok)\\\") (stderr \\\"DIAGNOSTIC-LINE\\\") (exit 0) (origin core))\n\"))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" esock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              "               (`(data ,r ,bv) (conn-write! r reply 'last) (serve))"
              "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " epeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? esock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (let ((r (client "" (string-append "outline --store " store " --socket " esock " --wire"))))
    (want "P-18 what the server put on its second stream reaches the caller's"
          (list (if (contains? (err-of r) "DIAGNOSTIC-LINE") 'on-stderr 'DROPPED)
                (if (contains? (out-of r) "DIAGNOSTIC-LINE") 'ALSO-ON-STDOUT 'not-on-stdout)
                (if (contains? (out-of r) "(ok") 'answer-on-stdout (list 'said (out-of r))))
          '(on-stderr not-on-stdout answer-on-stdout)))
  (system (string-append "pkill -f " epeer " 2>/dev/null")))

;; ---- P-16 a reply that reads forever -------------------------------------
;;
;; NEVER: MEASURED DEFECT, AND IT HUNG THE CLIENT. Chez's reader accepts datum
;; labels, so `#0=(answer (stdout "x") . #0#)` reads perfectly well and is
;; a CYCLE: every walk over it runs forever. Measured on the previous
;; build, this call never returned -- killed at a 20 second timeout, with
;; no answer and no refusal, from a peer that had only to write one line.
;;
;; NOTE: THE SAME BYTES GO TO BOTH PROGRAMS. Three programs read this reply
;; and each had its own reader; the rule that says what may be handed to
;; `read` now lives once in the library, and these two rows are the two
;; programs this fixture can drive.
(let* ((csock (string-append here "/cyclic.sock"))
       (cpeer (string-append here "/cyclic.sc")))
  (call-with-output-file cpeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              "(define reply (string->utf8 \"#0=(answer (stdout \\\"x\\\") . #0#)\n\"))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" csock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              "               (`(data ,r ,bv) (conn-write! r reply 'last) (serve))"
              "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " cpeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? csock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (let ((answered
          (lambda (program)
            (let ((r (run! program "" (string-append "outline --store " store
                                                     " --socket " csock " --wire"))))
              (list (cond ((contains? (out-of r) "unreadable-answer") 'named-it)
                          (else (list 'said (out-of r))))
                    (if (zero? (rc-of r)) 'EXITED-ZERO 'non-zero))))))
    (want "P-16 a reply that would read forever is refused by the client program"
          (answered "../theourgia.sc")
          '(named-it non-zero))
    (want "P-16 and by the command line, which reads the same reply"
          (answered "../cli.sc")
          '(named-it non-zero)))
  (system (string-append "pkill -f " cpeer " 2>/dev/null")))


;; ---- P-19 an exit status this process cannot leave with -------------------
;;
;; NEVER: MEASURED, AND IT WAS SILENT IN TWO DIRECTIONS. The field was checked
;; with `integer?`, which in Scheme is TRUE of `37.0`: a peer answering
;; that passed validation and the client left with 1 -- a failure, for an
;; answer that said 37. A number past what a status holds was worse:
;; `(exit 4294967337)` leaves with 41, truncated somewhere below us, so
;; the caller reads a status NOBODY chose and nothing anywhere says so.
;;
;; KEY: A NUMBER THIS PROCESS CANNOT RELAY IS A BAD ENVELOPE, not a small
;; problem with a good one. The client says `unreadable-answer` and leaves
;; non-zero, which is what it says about every other envelope it cannot
;; act on, NEVER: rather than inventing a status.
(let* ((esock (string-append here "/exit.sock"))
       (epeer (string-append here "/exitpeer.sc"))
       (peer-saying
         (lambda (field)
           (call-with-output-file epeer
             (lambda (port)
               (for-each (lambda (l) (display l port) (newline port))
                 (list "(import (chezscheme) (theourgia sched) (theourgia net))"
                       (string-append
                         "(define reply (string->utf8 \"(answer (stdout \\\"\\\") "
                         "(stderr \\\"\\\") (exit " field ") (origin core))\n\"))")
                       "(start-scheduler"
                       "  (lambda ()"
                       (string-append "    (listen! \"" esock "\" 16)")
                       "    (let serve ()"
                       "      (receive (after 20000 (exit 0))"
                       "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
                       "               (`(data ,r ,bv) (conn-write! r reply 'last) (serve))"
                       "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
                       "               (`(eof ,r) (serve))"
                       "               (`#(DOWN ,w ,y) (serve))))))"))))
           (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                                  "scheme --script " epeer " > /dev/null 2>&1 &"))
           (let up ((k 0))
             (cond ((file-exists? esock) 'up)
                   ((> k 300) 'never)
                   (else (system "sleep 0.05") (up (+ k 1)))))
           (let ((r (run! "../theourgia.sc" ""
                          (string-append "outline --store " store
                                         " --socket " esock " --wire"))))
             (system (string-append "pkill -f " epeer " 2>/dev/null"))
             (system (string-append "rm -f " esock " " epeer))
             (list (rc-of r) (out-of r))))))

  ;; NEVER: THE TWIN COMES FIRST HERE, because without it the two rows below
  ;; are passed by a client that refuses every exit field there is. A
  ;; status it CAN leave with must still be relayed, and NEVER: not flattened
  ;; to 0 or 1 -- our own daemon writes only those two, so a row using one
  ;; of them would not notice a client that ignored the field entirely.
  (want "P-19 TWIN: a status this process can leave with is relayed exactly"
        (car (peer-saying "37"))
        37)

  (want "P-19 an integral flonum is not a status, and is refused as an envelope"
        (let ((r (peer-saying "37.0")))
          (list (if (= 37 (car r)) 'LEFT-WITH-37
                    (if (zero? (car r)) 'LEFT-WITH-ZERO 'non-zero))
                (if (contains? (cadr r) "unreadable-answer") 'named-it
                    (list 'said (cadr r)))))
        '(non-zero named-it))

  ;; NOTE: THIS ONE IS THE DANGEROUS SHAPE: it does not fail, it SUCCEEDS
  ;; with a number the peer never sent. 4294967337 is 41 once truncated.
  (want "P-19 a number past what a status holds is refused, not truncated"
        (let ((r (peer-saying "4294967337")))
          (list (if (= 41 (car r)) 'TRUNCATED-TO-41 'not-truncated)
                (if (contains? (cadr r) "unreadable-answer") 'named-it
                    (list 'said (cadr r)))))
        '(not-truncated named-it)))

;; ---- P-15 two spellings of one store are one store ------------------------
;;
;; NEVER: MEASURED DEFECT, and it made the daemon unusable from inside its own
;; store. The socket key has always been the RESOLVED path, so `--store .`
;; reaches the daemon serving `/abs/store`; the name inside the envelope
;; was whatever the caller typed, and the daemon compared that against its
;; own. The request therefore arrived at the right daemon and was refused
;; by it as being for a different store:
;;
;;   (error transport-store-mismatch (serving "/abs/store") (asked "."))
;;
;; The default for `--store` is `.`, so this is what a second caller in
;; the store's directory gets for every verb while a daemon is up.
;;
;; NOTE: THE DAEMON IS STARTED BY ITS ABSOLUTE PATH, from somewhere else, so
;; the two spellings really do differ.
(let* ((p15-store (string-append here "/p15-store"))
       (p15-other (string-append here "/p15-other"))
       (inside (lambda (dir spelling tag)
                 (let ((out (string-append here "/p15-" tag ".out")))
                   (system (string-append "cd " dir " && " (env-prefix "")
                                          " scheme --script "
                                          (string-append (current-directory) "/../theourgia.sc")
                                          " outline --store " spelling " --wire > " out " 2>&1"))
                   (file-text out)))))
  (system (string-append "mkdir -p " p15-store " " p15-other))
  (server "" (string-append "init --store " p15-store " --wire"))
  (server "" (string-append "init --store " p15-other " --wire"))
  ;; Started from here, by absolute path.
  (client "" (string-append "outline --store " p15-store " --wire"))

  (want "P-15 a store named by its own directory reaches the daemon serving it"
        (let ((text (inside p15-store "." "dot")))
          (cond ((contains? text "transport-store-mismatch") 'REFUSED-ITS-OWN-STORE)
                ((contains? text "(ok") 'served)
                (else (list 'said text))))
        'served)

  (want "P-15 and so does a relative spelling from the directory above it"
        (let ((text (inside here "p15-store" "rel")))
          (cond ((contains? text "transport-store-mismatch") 'REFUSED)
                ((contains? text "(ok") 'served)
                (else (list 'said text))))
        'served)

  ;; NEVER: THE TWIN, and without it the two rows above are passed by a daemon
  ;; that has stopped checking which store it serves. A request for a
  ;; DIFFERENT store, aimed at this daemon's socket, must still be refused.
  (want "P-15 TWIN: a genuinely different store is still refused, and exits non-zero"
        (let* ((out (string-append here "/p15-other.out"))
               (sock (string-append here "/p15.sock"))
               (rc (begin
                     (system (string-append (env-prefix "") " scheme --script ../cli.sc serve "
                                            p15-store " --socket " sock
                                            " > /dev/null 2>&1 &"))
                     (let up ((k 0))
                       (cond ((file-exists? sock) 'up)
                             ((> k 300) 'never)
                             (else (system "sleep 0.05") (up (+ k 1)))))
                     (system (string-append (env-prefix "") " scheme --script ../theourgia.sc "
                                            "outline --store " p15-other " --socket " sock
                                            " --wire > " out " 2>&1"))))
               (text (file-text out)))
          (system (string-append "pkill -f 'serve " p15-store "' 2>/dev/null"))
          (list (if (contains? text "transport-store-mismatch") 'refused (list 'said text))
                (if (zero? rc) 'EXITED-ZERO 'non-zero)))
        '(refused non-zero))
  (system (string-append "pkill -f 'serve " p15-store "' 2>/dev/null")))

;; ---- P-13 a relative path is the CALLER'S ---------------------------------
;;
;; NEVER: THE DAEMON IS IN WHATEVER DIRECTORY IT WAS STARTED FROM, which is
;; not where the caller is. `import-code src` meant one directory to the
;; person typing it and another to the process acting on it -- measured
;; on the previous build as `(error projection-invalid (reason
;; not-a-directory))` once the daemon had been started from somewhere
;; else.
;;
;; NOTE: THE DAEMON IS DELIBERATELY STARTED FROM ANOTHER DIRECTORY. Started
;; by this very call, it inherits the caller's directory and the two
;; agree by accident -- which is why a first client that happens to be in
;; the right place hides this completely.
(let* ((dir-a (string-append here "/from-a"))
       (dir-b (string-append here "/from-b"))
       (cwd-store (string-append here "/cwd-store"))
       (out (string-append here "/cwd.out")))
  (system (string-append "mkdir -p " dir-a " " dir-b "/src; "
                         "printf 'content that only exists under B\\n' > " dir-b "/src/a.txt"))
  (system (string-append "cd " dir-a " && " (env-prefix "")
                         " scheme --script " (string-append (current-directory) "/../theourgia.sc")
                         " init --store " cwd-store " > /dev/null 2>&1"))
  ;; Warm the daemon up FROM A, so its directory is A and not B.
  (system (string-append "cd " dir-a " && " (env-prefix "")
                         " scheme --script " (string-append (current-directory) "/../theourgia.sc")
                         " outline --store " cwd-store " --wire > /dev/null 2>&1"))
  (system (string-append "cd " dir-b " && " (env-prefix "")
                         " scheme --script " (string-append (current-directory) "/../theourgia.sc")
                         " import-code src --store " cwd-store " --wire > " out " 2>/dev/null"))
  (want "P-13 a relative path is read where the caller is, not where the daemon is"
        (let ((text (file-text out)))
          (cond ((contains? text "not-a-directory") 'LOOKED-WHERE-THE-DAEMON-IS)
                ((contains? text "(ok") 'found-the-callers-directory)
                (else (list 'said text))))
        'found-the-callers-directory)
  (system (string-append "pkill -f 'serve " cwd-store "' 2>/dev/null")))

;; ---- P-11 a bound identity does not change the command line ---------------
;;
;; NEVER: MEASURED DEFECT, found by running the documented first command. The
;; local route used to splice `--writer <name>` in after the verb
;; whenever `THEOURGIA_WRITER` was set. `init` has no such option, so
;; `theourgia init --store X` answered `(usage (init))` and exited 1 --
;; in every spelling, for as long as an identity was bound. The first
;; thing anyone does with this program could not be done.
;;
;; NOTE: IT IS THE SAME RULE AS THE ENVELOPE'S: the writer is an identity
;; the process carries, not a word in the arguments. On this route it
;; reaches the server through the environment it is already in.
(let* ((wstore (string-append here "/bound-store"))
       (bound (client "THEOURGIA_WRITER=w1 " (string-append "init --store " wstore))))
  (want "P-11 init works with an identity bound"
        (list (rc-of bound)
              (if (contains? (out-of bound) "(ok") 'ok (list 'said (out-of bound))))
        '(0 ok))

  ;; NEVER: THE TWIN: the binding must still ARRIVE. A build that fixed the
  ;; row above by ignoring `THEOURGIA_WRITER` altogether would pass it and
  ;; break every draft verb, which is what the binding is for. A draft
  ;; written under the bound identity has to be the draft it reads back.
  (let* ((made (server "" (string-append "insert --title A --text old --store " wstore " --wire")))
         ;; The id is read from the answer's own `events`, the way every
         ;; other caller gets one, rather than found in its text.
         (id (let* ((datum (guard (e (#t #f))
                             (read (open-string-input-port (out-of made)))))
                    (evs (and (pair? datum) (field-of-answer datum 'events))))
               (and (pair? evs) (pair? (cadr evs))
                    (let ((ev (car (cadr evs))))
                      (string-append (car ev) "." (number->string (cdr ev))))))))
    ;; NOTE: A ROW WHOSE SETUP DID NOT HAPPEN SAYS SO. Where the row above is
    ;; red -- a build that refuses `init` with an identity bound -- there
    ;; is no block to write to, and a row that went ahead anyway took the
    ;; whole fixture down with a Scheme exception that named neither.
    (want "P-11 TWIN: and the bound identity is the one the draft belongs to"
          (if (not id)
              'NO-BLOCK-TO-WRITE-TO
              (begin
                (client "THEOURGIA_WRITER=w1 "
                        (string-append "write " id " draft-under-w1 --store " wstore " --wire"))
                (let ((back (client "THEOURGIA_WRITER=w1 "
                                    (string-append "read " id " --working --store "
                                                   wstore " --wire"))))
                  (if (contains? (out-of back) "draft-under-w1") 'read-its-own-draft
                      (list 'said (out-of back))))))
          'read-its-own-draft)))

;; ---- P-9 the exit code is the SERVER'S, not a summary of it ---------------
;;
;; NEVER: P-3 ABOVE CANNOT SAY THIS. It asks that success is 0 and that a
;; refusal is "not 0", and a client that answered 1 for every non-zero
;; code in the envelope would satisfy both -- while quietly destroying
;; the one thing the field is for, which is that the SERVER decides what
;; a verb's answer means. A distinctive code is the only way to tell
;; "relayed" from "reduced to a flag".
;;
;; NOTE: THE PEER IS A STAND-IN, not the daemon: the core never answers 37,
;; which is the point -- no real verb can produce this number by
;; accident, so seeing it proves it came through the envelope.
(let ((codesock (string-append here "/code.sock"))
      (codepeer (string-append here "/code.sc")))
  (call-with-output-file codepeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" codesock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              "               (`(data ,r ,bv)"
              "                 (conn-write! r (string->utf8 \"(answer (stdout \\\"(ok)\\\") (stderr \\\"\\\") (exit 37) (origin core))\\n\") 'last)"
              "                 (serve))"
              "               (`(written ,r ,tok ,status) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " codepeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? codesock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (let ((r (client "" (string-append "outline --store " store " --socket " codesock " --wire"))))
    (want "P-9 the client exits with the code the server put in the envelope"
          (rc-of r)
          37))
  (system (string-append "pkill -f " codepeer " 2>/dev/null")))

;; ---- P-14 a reply that is a datum but not an envelope --------------------
;;
;; NEVER: MEASURED DEFECT: the fields were taken with `assq`, which demands a
;; proper list of pairs and raises on anything else. `(answer . broken)`
;; reads perfectly well as a datum, so it got past the reader and raised
;; "improperly formed alist" out of the program -- around the refusal that
;; exists for exactly this, and out to a caller with no handler. What a
;; peer sends is not this program's to assume.
(let* ((rsock (string-append here "/rot.sock"))
       (rpeer (string-append here "/rot.sc")))
  (call-with-output-file rpeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              "(define reply (string->utf8 \"(answer . broken)\n\"))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" rsock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              "               (`(data ,r ,bv) (conn-write! r reply 'last) (serve))"
              "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " rpeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? rsock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (let ((asked-through
          (lambda (program)
            (let ((r (run! program "" (string-append "outline --store " store
                                                     " --socket " rsock " --wire"))))
              (list (cond ((contains? (out-of r) "unreadable-answer") 'named-it)
                          ((contains? (err-of r) "improperly formed") 'RAISED-OUT)
                          (else (list 'said (out-of r) (err-of r))))
                    (if (zero? (rc-of r)) 'EXITED-ZERO 'non-zero))))))

  (want "P-14 a reply that is not an envelope is refused, not raised"
        (asked-through "../theourgia.sc")
        '(named-it non-zero))

  ;; NEVER: AND THE SAME FOR THE OTHER PROGRAM THAT READS THIS ENVELOPE. Three
  ;; programs read it -- this one, the command line, and the MCP shell --
  ;; and each had its own copy of the field lookup, so the same defect was
  ;; fixed in two of them and left in the third. They now call one reader
  ;; in the library. NOTE: The command line forwards to a daemon only when a
  ;; socket is there to forward to, which `--socket` provides here.
  (want "P-14 and the command line, reading the same envelope, refuses it too"
        (asked-through "../cli.sc")
        '(named-it non-zero)))
  (system (string-append "pkill -f " rpeer " 2>/dev/null")))

;; ---- P-10 a request taken and never answered is not sent twice ------------
;;
;; NEVER: WHAT MUST NOT HAPPEN IS A SECOND DISPATCH. The bytes went out, so
;; the request may already have been carried out; asking again would do
;; it twice, and no answer this client can print is worth that. The rows
;; that exist for this all use daemons that answer, or dials that fail --
;; neither reaches the case.
;;
;; NOTE: THE PEER COUNTS THE FRAMES IT RECEIVES and writes the count where
;; this can read it, because "did it resend" is a fact about what arrived
;; at the other end, not about what the client printed.
(let ((holdsock (string-append here "/hold.sock"))
      (holdpeer (string-append here "/hold.sc"))
      (holdcount (string-append here "/hold.count")))
  (call-with-output-file holdpeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              "(define seen 0)"
              (string-append "(define countfile \"" holdcount "\")")
              "(define (note!)"
              "  (set! seen (+ seen 1))"
              "  (call-with-output-file countfile"
              "    (lambda (p) (write seen p))"
              "    'truncate))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" holdsock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              ;; Read the frame -- so it really arrived -- then close
              ;; without answering: "sent, and the answer was lost".
              "               (`(data ,r ,bv) (note!) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " holdpeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? holdsock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (let* ((r (client "" (string-append "outline --store " store " --socket " holdsock " --wire")))
         (count (let ((t (file-text holdcount)))
                  (if (> (string-length t) 0) (string->number t) 0))))
    (want "P-10 a request whose answer was lost is reported, not repeated"
          (list (if (= count 1) 'sent-once (list 'frames count))
                (if (zero? (rc-of r)) 'EXITED-ZERO 'non-zero))
          '(sent-once non-zero))
    ;; NEVER: AND IT SAYS THE OUTCOME IS UNKNOWN, not that nothing happened.
    ;; The bytes went out; reporting `not-sent` here would be a claim
    ;; this client cannot make.
    (want "P-10 and it says so as an unknown outcome"
          (if (contains? (out-of r) "transport-unknown") 'unknown
              (list 'said (out-of r)))
          'unknown))
  (system (string-append "pkill -f " holdpeer " 2>/dev/null")))


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
    (if (file-exists? (string-append up "/cli.sc")) up script-dir)))
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

(want "IMPORTS and the client's closure is exactly what it should be"
      (closure-from 'client)
      '(client digest ffi render trace))

;; NEVER: AND THE PROGRAM'S OWN CLOSURE, not only the library's. A person runs
;; `theourgia.sc`; what IT reaches is a separate fact from what the
;; `client` library reaches, and the rows above are about the library.
;; `arguments` is allowed and the reason is written in `closures.sc`: the
;; program asks that table whether a verb reads standard input rather than
;; keeping a second copy of it, and the table reaches nothing else.
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
      '(arguments client digest ffi render trace))


(kill-daemons!)
(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\nclient-program complete\n" rows bad)
(exit (if (zero? bad) 0 1))
