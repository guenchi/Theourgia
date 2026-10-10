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

;; The command line, forwarding to a daemon -- in Scheme, with no helper
;; process.
;;
;; KEY: EVERY ROW HERE RUNS THE REAL `core.sc` AS A REAL PROCESS, the way a
;; person does. A row that called its procedures would be testing the
;; library and would say nothing about argv, the environment, or which
;; program ends up doing the work.

(import (chezscheme)
        (only (theourgia client) socket-path))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; NEVER: A RAISE INSIDE A ROW IS THAT ROW FAILING, NOT THE FILE ENDING. A
;; fixture whose thirteenth row raises and takes the file with it reports
;; twelve passes and no failures, and the run's own sentinel check is the
;; only thing standing between that and a green reading.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define pid-text (number->string (get-process-id)))

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

(define here (string-append scratch-base "/cliforward-" pid-text))
(define sock-here (string-append socket-base "/cliforward-" pid-text))
;; NEVER: THE RUN ROOT IS THIS FIXTURE'S OWN. `socket-path` puts a store's
;; socket under `THEOURGIA_RUN`, and unset that is `$HOME/.theourgia/run`
;; -- the real one. Measured before this line existed: every run of this
;; file left one more directory in the user's own run root.
;;
;; NOTE: IT IS SET IN THIS PROCESS *AND* PASSED TO EVERY SUBPROCESS. Set in
;; only one of the two, the fixture and the programs it starts would
;; compute different socket paths for the same store -- which is exactly
;; the defect the rows about the key are about, recreated by the fixture.
(putenv "THEOURGIA_RUN" (string-append sock-here "/run"))

(define store-a (string-append here "/a"))
(define store-b (string-append here "/b"))
;; NEVER: THE PATH COMES FROM THE PRODUCT, NOT FROM THIS FILE. It used to be
;; `(string-append store-a "/socket")` -- the default as it stood when
;; this fixture was written -- so when the rule moved to the run root the
;; daemon listened where nothing looked, and F-02 reported the CLI
;; answering locally. NOTE: Rewriting it as the NEW path would be the same
;; mistake one version later: a cell that restates a rule is checking its
;; own copy of it.
(define socket-a (socket-path store-a))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

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

(define (env-prefix extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                 "THEOURGIA_RUN=" sock-here "/run " extra))

;; Runs a file of Scheme in a process of its own.
(define (run-scheme! extra script args out)
  (system (string-append (env-prefix extra) " scheme --script " script " " args
                         " > " out " 2>&1")))

(define (write-script! path lines)
  (when (file-exists? path) (delete-file path))
  (call-with-output-file path
    (lambda (port) (for-each (lambda (l) (display l port) (newline port)) lines))))

;; ---- the two stores and a daemon -------------------------------------------

(define setup (string-append here "/setup.sc"))
(define serve-b (string-append here "/serve-b.sc"))
(define serve-a (string-append here "/serve-a.sc"))

(system (string-append "rm -rf " here " " sock-here "; mkdir -p " store-a " " store-b " " sock-here))

(write-script! setup
  (list "(import (chezscheme) (theourgia rpc))"
        (string-append "(rpc-dispatch \"" store-a "\" '(init) \"tester\")")
        (string-append "(rpc-dispatch \"" store-b "\" '(init) \"tester\")")
        (string-append "(rpc-dispatch \"" store-a "\" '(insert \"--title\" \"IN-A\") \"tester\")")
        "(display \"set up\\n\")"))
(run-scheme! "" setup "" (string-append here "/setup.log"))

;; KEY: THE DAEMON SERVES B AND LISTENS WHERE A'S CLIENT WILL LOOK. That is
;; what makes "did this answer come from the daemon" a question with a
;; one-word answer: `transport-store-mismatch` is a refusal only a daemon
;; can produce, and NEVER: no local run can produce it, whatever else changes.
(write-script! serve-b
  (list "(import (chezscheme) (theourgia daemon))"
        (string-append "(serve \"" store-b "\" \"" socket-a "\")")))

;; EVERY BACKGROUND PROCESS THIS FIXTURE STARTS IS WAITED FOR BY ITS PID
;; after it is told to stop. NEVER: WAITING FOR THE SOCKET IS NOT WAITING FOR
;; THE PROCESS: a daemon unlinks its socket and then takes a moment to exit,
;; and in that moment it was still in this fixture's process group when the
;; fixture ended (measured by the runner's group census, suite12). The
;; runner now reports such a member as LEFT IN GROUP (launcher design L5).
;; A process that has ended but is not yet reaped (a zombie) counts as gone.
;; A BACKGROUND START WHOSE PID IS READ FROM THE SHELL'S OWN OUTPUT through
;; a process port, not through a file under the scratch root, which a row
;; that damaged it would lose (codex r8 C2). The command's own output goes
;; to its log; only `echo $!` reaches the port. #f means the pid could not
;; be read: that start is registered as unknown, and its stop waits out
;; the whole bound.
(define (start-bg! cmd)
  (guard (e (#t #f))
    (let* ((p (process (string-append cmd " < /dev/null & echo $!")))
           (in (car p)))
      (close-port (cadr p))
      (let ((n (read in)))
        (close-port in)
        (and (integer? n) (> n 0) n)))))
(include "pid-state.ss")
(define (wait-gone! pid)
  (if pid
      (let wait ((k 0))
        (unless (or (> k 100) (pid-gone? pid))
          (system "sleep 0.05")
          (wait (+ k 1))))
      (begin
        (printf "a background start's pid could not be read; waiting out the bound\n")
        (system "sleep 5"))))
;; EVERY DAEMON STARTED IS KEPT, not only the latest: two are started
;; between one stop and the next (codex r7 C2).
(define daemon-pids '())
(define (wait-daemons!)
  (for-each wait-gone! daemon-pids)
  (set! daemon-pids '()))

(define (start-daemon! script log)
  (set! daemon-pids (cons (start-bg! (string-append (env-prefix "") " scheme --script " script " > " log " 2>&1"))
                          daemon-pids))
  (let wait ((k 0))
    (cond ((file-exists? socket-a) 'up)
          ((> k 200) 'never)
          (else (system "sleep 0.05") (wait (+ k 1))))))

(define (stop-daemon! script)
  (system (string-append "pkill -f " script " 2>/dev/null"))
  (let ((answer (let wait ((k 0))
                  (cond ((not (file-exists? socket-a)) 'gone)
                        ((> k 40) 'still-there)
                        (else (system "sleep 0.05") (wait (+ k 1)))))))
    (wait-daemons!)
    answer))

;; ---- a python3 that shouts if anybody calls it ------------------------------
;;
;; NEVER: THE ROW ASKS THE QUESTION DIRECTLY. "There is no python on this
;; path" is not something a grep of the source can settle -- the exec
;; used to be behind a runtime condition -- so the row puts a `python3`
;; of its own first on PATH, one that records having been called. A file
;; that is not there afterwards is the reading.
(define fake-bin (string-append here "/bin"))
(define marker (string-append here "/python-was-called"))

(system (string-append "mkdir -p " fake-bin))
(call-with-output-file (string-append fake-bin "/python3")
  (lambda (port)
    (display "#!/bin/sh\n" port)
    (display (string-append "echo \"$@\" > " marker "\n") port)
    (display "exit 97\n" port)))
(system (string-append "chmod +x " fake-bin "/python3"))

(define (cli! extra args out)
  (system (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH " extra))
                         " scheme --script ../core.sc " args " > " out " 2>&1")))

(define out-1 (string-append here "/out1.txt"))
(define out-2 (string-append here "/out2.txt"))
(define out-3 (string-append here "/out3.txt"))
(define out-4 (string-append here "/out4.txt"))

;; ---- F-01 it forwards ------------------------------------------------------
(define daemon-up (start-daemon! serve-b (string-append here "/serve-b.log")))
(want "F-01 the daemon for the forwarding rows came up" daemon-up 'up)

(cli! "" (string-append "outline --store " store-a) out-1)
(define forwarded (file-text out-1))
(want "F-02 with a socket there, the answer comes from the daemon"
      (if (contains? forwarded "transport-store-mismatch")
          'from-the-daemon
          (list 'said forwarded))
      'from-the-daemon)

;; NEVER: AND NO PYTHON RAN. The answer above proves a daemon was reached; it
;; does not say which program reached it, and for a batch whose point is
;; removing the helper that is the whole question.
(want "F-03 and nothing on that path called python"
      (if (file-exists? marker) (list 'called-python (file-text marker)) 'no-python)
      'no-python)

;; ---- F-04 THEOURGIA_LOCAL=1 does not forward -------------------------------
;;
;; NEVER: THE TWIN. Without it, "it forwarded" is satisfied by a command line
;; that forwards unconditionally -- including when the caller has said
;; not to.
(cli! "THEOURGIA_LOCAL=1" (string-append "outline --store " store-a) out-2)
(define local-answer (file-text out-2))
(want "F-04 THEOURGIA_LOCAL=1 answers from this store, socket or no socket"
      (if (contains? local-answer "transport-store-mismatch")
          (list 'FORWARDED-ANYWAY local-answer)
          (if (contains? local-answer "IN-A") 'answered-locally (list 'said local-answer)))
      'answered-locally)

(stop-daemon! serve-b)

;; ---- F-05 the forwarded answer is the local answer -------------------------
;;
;; NOTE: NOW THE DAEMON SERVES THE SAME STORE, so the two answers are
;; answers to the same question and comparing them means something.
(write-script! serve-a
  (list "(import (chezscheme) (theourgia daemon))"
        (string-append "(serve \"" store-a "\" \"" socket-a "\")")))
(define daemon-a (start-daemon! serve-a (string-append here "/serve-a.log")))
(want "F-05 the daemon for the comparison row came up" daemon-a 'up)

(cli! "" (string-append "outline --store " store-a) out-3)
(define through-daemon (file-text out-3))
(want "F-06 the forwarded answer is byte for byte the local answer"
      (if (string=? through-daemon local-answer)
          'identical
          (list 'differ through-daemon local-answer))
      'identical)

(stop-daemon! serve-a)

;; ---- F-07 a socket nobody is behind ----------------------------------------
;;
;; NEVER: REACHING NOBODY IS A REASON TO RUN LOCALLY. The daemon above was
;; stopped; if it left its socket behind, or if one is left by a daemon
;; that was killed, the command line must still answer -- NEVER: and from
;; this store, not with a transport error.
;; NOTE: THE STALE SOCKET IS MADE THE WAY STALE SOCKETS HAPPEN: a daemon
;; killed with a signal it cannot handle, so it never unlinks the file it
;; bound. NEVER: Not by binding one with another tool -- which would be this
;; fixture asserting what a leftover looks like instead of producing one.
(define daemon-again (start-daemon! serve-a (string-append here "/serve-a2.log")))
(want "F-07 a daemon to kill without letting it tidy up" daemon-again 'up)
(system (string-append "pkill -9 -f " serve-a " 2>/dev/null"))
(wait-daemons!)
(system "sleep 0.5")
(want "F-08 and it did leave its socket behind"
      (if (file-exists? socket-a) 'left-behind 'no-socket-to-test-with)
      'left-behind)
(cli! "" (string-append "outline --store " store-a) out-4)
(define stale-answer (file-text out-4))
(want "F-09 a socket with nobody behind it falls through to this store"
      (if (contains? stale-answer "IN-A")
          'answered-locally
          (list 'said stale-answer))
      'answered-locally)



;; ---- F-13 several verbs, byte for byte -------------------------------------
;;
;; NEVER: ONE VERB AGREEING IS NOT THE CLAIM. The claim is that the socket
;; route does not implement anything of its own, and a route that had
;; its own idea about one verb would agree about the others. Measured
;; across four answers of different shapes: text, items, a check report
;; and a refusal.
(define out-6 (string-append here "/out6.txt"))
(define out-7 (string-append here "/out7.txt"))
(define daemon-again2 (start-daemon! serve-a (string-append here "/serve-a3.log")))
(want "F-13 a daemon for the byte comparison came up" daemon-again2 'up)

(want "F-13 four verbs answer byte for byte as they do locally"
      (map (lambda (argv)
             (cli! "" (string-append argv " --store " store-a " --wire") out-6)
             (let ((forwarded (file-text out-6)))
               (cli! "THEOURGIA_LOCAL=1" (string-append argv " --store " store-a " --wire") out-7)
               (if (string=? forwarded (file-text out-7))
                   'identical
                   (list argv forwarded (file-text out-7)))))
           (list "outline" "refs missing.1" "conflicts" "check"))
      '(identical identical identical identical))

;; ---- F-15 a verb that cannot be printed ------------------------------------
;;
;; NEVER: A CALLER'S VERB CAN BE ANY TEXT, and one they got wrong is exactly
;; the kind the wire writer will not emit: `show me` went out as
;; `show\x20;me` and the reader that had asked the question could not
;; parse the answer to it.
;;
;; KEY: SO IT IS REFUSED IN THE PART BOTH ROUTES SHARE, before anything is
;; printed. Judged only by the reader's guard it would have been
;; `not-a-datum` at a daemon and would never have met a guard at all in a
;; local call -- the same verb, two answers, depending on whether a daemon
;; happened to be running. `daemon.sc` promises these two routes give byte
;; for byte the same answer, and this row is that promise for the case
;; that nearly broke it.
(define out-8 (string-append here "/out8.txt"))
(define out-9 (string-append here "/out9.txt"))
(define daemon-again3 (start-daemon! serve-a (string-append here "/serve-a4.log")))
(want "F-15 a daemon for the unprintable-verb comparison came up" daemon-again3 'up)

(want "F-15 a verb that cannot be printed is refused the same way by both routes"
      (let ()
        (cli! "" (string-append "'show me' --store " store-a " --wire") out-8)
        (cli! "THEOURGIA_LOCAL=1" (string-append "'show me' --store " store-a " --wire") out-9)
        (let ((forwarded (file-text out-8)) (local (file-text out-9)))
          (list (if (string=? forwarded local) 'identical (list forwarded local))
                (if (contains? local "unknown-verb") 'named-the-verb (list 'said local))
                ;; NEVER: AND THE SPELLING COMES BACK AS A STRING. It is the
                ;; text the caller typed, which is the only form of it
                ;; that survives the wire.
                (if (contains? local "\"show me\"") 'spelled-as-a-string 'SPELLING-LOST))))
      '(identical named-the-verb spelled-as-a-string))

;; NEVER: AND A SPELLING MADE ONLY OF ALPHABET CHARACTERS THAT STILL CANNOT BE
;; PRINTED. `1` is letters-and-digits, so a check that only looked at
;; characters passed it -- and `write` emits it as `\x31;`, because Chez
;; escapes a symbol whose spelling would READ AS SOMETHING ELSE. The frame
;; then carried a backslash, the reader's guard refused it, and the two
;; routes answered differently for the same verb:
;;
;;   local     (error unknown-verb (spelling "1") (verbs ...))
;;   forwarded (error bad-request (reason not-a-datum))
;;
;; KEY: THE ROW ABOVE CANNOT CATCH THIS. `show me` holds a space, which is
;; outside the alphabet, so a character test already refused it; this one
;; is inside the alphabet and refused only because the writer says so.
(want "F-15 a spelling the writer escapes is refused the same way by both routes"
      (let ()
        (cli! "" (string-append "1 --store " store-a " --wire") out-8)
        (cli! "THEOURGIA_LOCAL=1" (string-append "1 --store " store-a " --wire") out-9)
        (let ((forwarded (file-text out-8)) (local (file-text out-9)))
          (list (if (string=? forwarded local) 'identical (list forwarded local))
                (if (contains? local "unknown-verb") 'named-the-verb (list 'said local))
                (if (contains? local "not-a-datum") 'REFUSED-AS-BYTES 'refused-as-a-verb))))
      '(identical named-the-verb refused-as-a-verb))

;; NEVER: AND THE TWO PROGRAMS AGREE, not merely the two routes. The spelling
;; is judged BEFORE the arguments are parsed, and the order follows from a
;; fact rather than a preference: the thin client knows no verb's option
;; table -- that is what makes it thin -- so it can never answer
;; `missing-option-value` for a verb-specific option, while `core.sc` can.
;; The one order the two can share is the check that needs no table.
;;
;; NOTE: MEASURED WITH THE CHECK AFTER THE PARSE: one argv, two answers --
;;   core.sc        (error bad-request missing-option-value "--store")
;;   theourgia.sc  (error bad-request unknown-verb (spelling "show me"))
;; KEY: AND THE EARLIER ROUND MISSED IT because it tried `--req`, an option
;; that takes no value, so the parse never failed. The same claim was true
;; all along under an input nobody had tried.
;; NEVER: THE ARGV IS PASSED THROUGH UNTOUCHED, and the first version of this
;; helper did not: it appended `--wire`, so `outline --store` became
;; `outline --store --wire` and `--store` ate the flag as its value. Both
;; programs then agreed on `(error no-store "--wire")` -- a true reading
;; of a command line the row never meant to send.
;; NOTE: AND NOTHING IS LOST BY LEAVING IT OFF HERE: measured, an error
;; renders the same in both modes; only an `ok` answer differs, and every
;; row below is about an error.
(define (both-programs argv)
  (cli! "THEOURGIA_LOCAL=1" argv out-8)
  (let ((by-cli (file-text out-8)))
    (system (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH THEOURGIA_LOCAL=1 "))
                           " scheme --script ../theourgia.sc " argv " > " out-9 " 2>&1"))
    (list by-cli (file-text out-9))))

(want "F-16 an unprintable verb answers the same from both programs, whatever follows it"
      (map (lambda (argv)
             (let ((pair (both-programs argv)))
               (cond ((not (string=? (car pair) (cadr pair))) (cons 'DIFFER pair))
                     ((contains? (car pair) "unknown-verb") 'identical-unknown-verb)
                     (else (list 'said (car pair))))))
           (list "'show me' --store" "1 --store"))
      '(identical-unknown-verb identical-unknown-verb))

;; NEVER: TWIN: AND THE CHECK DOES NOT SWALLOW EVERYTHING ELSE. A spelling that
;; is fine, with the same missing option value, must still be answered as a
;; missing option value -- by both programs. Without this row, moving the
;; spelling check to the front is satisfied by one that calls every
;; malformed command line a spelling problem.
(want "F-16 TWIN: a printable verb with the same missing value is still a missing value, from both"
      (let ((pair (both-programs "outline --store")))
        (cond ((not (string=? (car pair) (cadr pair))) (cons 'DIFFER pair))
              ((contains? (car pair) "missing-option-value") 'identical-missing-value)
              (else (list 'said (car pair)))))
      'identical-missing-value)

;; NEVER: TWIN: A HYPHENATED VERB IS ORDINARY AND MUST STILL WORK. `-` is in
;; the alphabet because five verbs are spelled with it, so a guard that
;; refused it would refuse `split-suggest`, `import-code` and three more --
;; and this row is the reason the alphabet is not just letters.
(want "F-15 TWIN: a hyphenated verb is served identically by both routes"
      (let ()
        (cli! "" (string-append "split-suggest --store " store-a " --wire") out-8)
        (cli! "THEOURGIA_LOCAL=1" (string-append "split-suggest --store " store-a " --wire") out-9)
        (let ((forwarded (file-text out-8)) (local (file-text out-9)))
          (list (if (string=? forwarded local) 'identical (list forwarded local))
                (if (contains? local "bad-request") 'REFUSED-A-REAL-VERB 'not-refused))))
      '(identical not-refused))

(stop-daemon! serve-a)

;; ---- F-14 a peer that takes the request and then goes ----------------------
;;
;; NEVER: CONNECTED AND THEN LOST IS NOT "NOBODY WAS THERE". The request went
;; out; it may have been carried out. NEVER: The command line must NOT run it
;; again -- a caller told "that did not happen" would do it twice -- and
;; NEVER: must not report success either. It says the answer could not be
;; obtained, which is what "ask me again" means here.
;;
;; NOTE: TWO HALVES, AND THE SECOND IS THE ONE THAT MATTERS. "It said
;; transport-unknown" is satisfied by an implementation that also ran the
;; command locally and then threw the answer away; the trace is what says
;; it did not.
(define silent-peer (string-append here "/silent.sc"))
(write-script! silent-peer
  (list "(import (chezscheme) (theourgia sched) (theourgia net))"
        "(start-scheduler"
        "  (lambda ()"
        (string-append "    (listen! \"" socket-a "\" 16)")
        "    (let serve ()"
        "      (receive (after 30000 'done)"
        "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
        ;; NEVER: THE BYTES ARE READ AND THEN THE CONNECTION IS CLOSED. Reading
        ;; first is what makes this "the request arrived", not "the dial
        ;; failed".
        "               (`(data ,r ,bv) (conn-close! r) (serve))"
        "               (`(eof ,r) (serve))"
        "               (`#(DOWN ,w ,y) (serve))))))"))
(system (string-append "rm -f " socket-a))
(define silent-pid (start-bg! (string-append (env-prefix "") " scheme --script " silent-peer
                                            " > " here "/silent.log 2>&1")))
(let wait ((k 0))
  (cond ((file-exists? socket-a) 'up)
        ((> k 200) 'never)
        (else (system "sleep 0.05") (wait (+ k 1)))))

(define out-8 (string-append here "/out8.txt"))
(define trace-8 (string-append here "/trace8.txt"))
(system (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH THEOURGIA_TRACE=1 "))
                       " scheme --script ../core.sc insert --title LOST-ANSWER-CANARY --store " store-a
                       " > " out-8 " 2>" trace-8))
(want "F-14 a request that was taken and then lost is reported as unknown"
      (if (contains? (file-text out-8) "transport-unknown")
          'said-unknown
          (list 'said (file-text out-8)))
      'said-unknown)

(want "F-14 TWIN: and it was not quietly run here instead"
      (list (if (contains? (file-text trace-8) "log-open") 'RAN-IT-LOCALLY 'no-local-open)
            (let ((found (string-append here "/found8.txt")))
              (system (string-append "grep -rl LOST-ANSWER-CANARY " store-a " > " found " 2>/dev/null"))
              (if (> (string-length (file-text found)) 0) 'IT-RAN 'not-in-the-store)))
      '(no-local-open not-in-the-store))

(system (string-append "pkill -f " silent-peer " 2>/dev/null"))
(wait-gone! silent-pid)


;; ---- F-10 `theourgia serve` is this program ---------------------------------
;;
;; NEVER: THE VERB USED TO EXEC A PYTHON DAEMON. It is Scheme now, and the row
;; asks the two questions that matter separately: did a daemon come up
;; that this command line can talk to, and did anything call python to
;; make that happen. Either one alone can be true while the other is
;; false -- a daemon can come up by the old route, and no python can run
;; because nothing came up at all.
(define serve-log (string-append here "/serve-verb.log"))
(when (file-exists? marker) (delete-file marker))
(system (string-append "rm -f " socket-a))
(define serve-verb-pid (start-bg! (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH "))
                                                " scheme --script ../theourgiad.sc serve " store-a
                                                " --socket " socket-a " > " serve-log " 2>&1")))
(define serve-verb-up
  (let wait ((k 0))
    (cond ((file-exists? socket-a) 'up)
          ((> k 200) 'never)
          (else (system "sleep 0.05") (wait (+ k 1))))))
(want "F-10 `theourgia serve` brings up a daemon" serve-verb-up 'up)

(define out-5 (string-append here "/out5.txt"))
(cli! "" (string-append "outline --store " store-a) out-5)
(want "F-11 and a request to it is answered"
      (if (contains? (file-text out-5) "IN-A") 'answered (list 'said (file-text out-5)))
      'answered)

(want "F-12 and no python was called to do any of it"
      (if (file-exists? marker) (list 'called-python (file-text marker)) 'no-python)
      'no-python)

(system (string-append "pkill -f \"theourgiad.sc serve " store-a "\" 2>/dev/null"))
(wait-gone! serve-verb-pid)

(system (string-append "rm -rf " here " " sock-here))
(printf "rows: ~a\n~a failures\ncli-forward complete\n" rows bad)
(exit (if (zero? bad) 0 1))
