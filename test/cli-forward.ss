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
;; ⭐ EVERY ROW HERE RUNS THE REAL `cli.ss` AS A REAL PROCESS, the way a
;; person does. A row that called its procedures would be testing the
;; library and would say nothing about argv, the environment, or which
;; program ends up doing the work.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; ⛔ A RAISE INSIDE A ROW IS THAT ROW FAILING, NOT THE FILE ENDING. A
;; fixture whose thirteenth row raises and takes the file with it reports
;; twelve passes and no failures, and the run's own sentinel check is the
;; only thing standing between that and a green reading.
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
(define here (string-append "/tmp/cliforward-" pid-text))
(define store-a (string-append here "/a"))
(define store-b (string-append here "/b"))
(define socket-a (string-append store-a "/socket"))

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
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' " extra))

;; Runs a file of Scheme in a process of its own.
(define (run-scheme! extra script args out)
  (system (string-append (env-prefix extra) " scheme --script " script " " args
                         " > " out " 2>&1")))

(define (write-script! path lines)
  (when (file-exists? path) (delete-file path))
  (call-with-output-file path
    (lambda (port) (for-each (lambda (l) (display l port) (newline port)) lines))))

;; ---- the two stores and a daemon -------------------------------------------

(define setup (string-append here "/setup.ss"))
(define serve-b (string-append here "/serve-b.ss"))
(define serve-a (string-append here "/serve-a.ss"))

(system (string-append "rm -rf " here "; mkdir -p " store-a " " store-b))

(write-script! setup
  (list "(import (chezscheme) (theourgia rpc))"
        (string-append "(rpc-dispatch \"" store-a "\" '(init) \"tester\")")
        (string-append "(rpc-dispatch \"" store-b "\" '(init) \"tester\")")
        (string-append "(rpc-dispatch \"" store-a "\" '(insert \"--title\" \"IN-A\") \"tester\")")
        "(display \"set up\\n\")"))
(run-scheme! "" setup "" (string-append here "/setup.log"))

;; ⭐ THE DAEMON SERVES B AND LISTENS WHERE A'S CLIENT WILL LOOK. That is
;; what makes "did this answer come from the daemon" a question with a
;; one-word answer: `transport-store-mismatch` is a refusal only a daemon
;; can produce, and ⛔ no local run can produce it, whatever else changes.
(write-script! serve-b
  (list "(import (chezscheme) (theourgia daemon))"
        (string-append "(serve \"" store-b "\" \"" socket-a "\")")))

(define (start-daemon! script log)
  (system (string-append (env-prefix "") " scheme --script " script " > " log " 2>&1 &"))
  (let wait ((k 0))
    (cond ((file-exists? socket-a) 'up)
          ((> k 200) 'never)
          (else (system "sleep 0.05") (wait (+ k 1))))))

(define (stop-daemon! script)
  (system (string-append "pkill -f " script " 2>/dev/null"))
  (let wait ((k 0))
    (cond ((not (file-exists? socket-a)) 'gone)
          ((> k 40) 'still-there)
          (else (system "sleep 0.05") (wait (+ k 1))))))

;; ---- a python3 that shouts if anybody calls it ------------------------------
;;
;; ⛔ THE ROW ASKS THE QUESTION DIRECTLY. "There is no python on this
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
                         " scheme --script ../cli.ss " args " > " out " 2>&1")))

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

;; ⛔ AND NO PYTHON RAN. The answer above proves a daemon was reached; it
;; does not say which program reached it, and for a batch whose point is
;; removing the helper that is the whole question.
(want "F-03 and nothing on that path called python"
      (if (file-exists? marker) (list 'called-python (file-text marker)) 'no-python)
      'no-python)

;; ---- F-04 THEOURGIA_LOCAL=1 does not forward -------------------------------
;;
;; ⛔ THE TWIN. Without it, "it forwarded" is satisfied by a command line
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
;; ⚠️ NOW THE DAEMON SERVES THE SAME STORE, so the two answers are
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
;; ⛔ REACHING NOBODY IS A REASON TO RUN LOCALLY. The daemon above was
;; stopped; if it left its socket behind, or if one is left by a daemon
;; that was killed, the command line must still answer -- ⛔ and from
;; this store, not with a transport error.
;; ⚠️ THE STALE SOCKET IS MADE THE WAY STALE SOCKETS HAPPEN: a daemon
;; killed with a signal it cannot handle, so it never unlinks the file it
;; bound. ⛔ Not by binding one with another tool -- which would be this
;; fixture asserting what a leftover looks like instead of producing one.
(define daemon-again (start-daemon! serve-a (string-append here "/serve-a2.log")))
(want "F-07 a daemon to kill without letting it tidy up" daemon-again 'up)
(system (string-append "pkill -9 -f " serve-a " 2>/dev/null"))
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
;; ⛔ ONE VERB AGREEING IS NOT THE CLAIM. The claim is that the socket
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

(stop-daemon! serve-a)

;; ---- F-14 a peer that takes the request and then goes ----------------------
;;
;; ⛔ CONNECTED AND THEN LOST IS NOT "NOBODY WAS THERE". The request went
;; out; it may have been carried out. ⛔ The command line must NOT run it
;; again -- a caller told "that did not happen" would do it twice -- and
;; ⛔ must not report success either. It says the answer could not be
;; obtained, which is what "ask me again" means here.
;;
;; ⚠️ TWO HALVES, AND THE SECOND IS THE ONE THAT MATTERS. "It said
;; transport-unknown" is satisfied by an implementation that also ran the
;; command locally and then threw the answer away; the trace is what says
;; it did not.
(define silent-peer (string-append here "/silent.ss"))
(write-script! silent-peer
  (list "(import (chezscheme) (theourgia sched) (theourgia net))"
        "(start-scheduler"
        "  (lambda ()"
        (string-append "    (listen! \"" socket-a "\" 16)")
        "    (let serve ()"
        "      (receive (after 30000 'done)"
        "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
        ;; ⛔ THE BYTES ARE READ AND THEN THE CONNECTION IS CLOSED. Reading
        ;; first is what makes this "the request arrived", not "the dial
        ;; failed".
        "               (`(data ,r ,bv) (conn-close! r) (serve))"
        "               (`(eof ,r) (serve))"
        "               (`#(DOWN ,w ,y) (serve))))))"))
(system (string-append "rm -f " socket-a))
(system (string-append (env-prefix "") " scheme --script " silent-peer
                       " > " here "/silent.log 2>&1 &"))
(let wait ((k 0))
  (cond ((file-exists? socket-a) 'up)
        ((> k 200) 'never)
        (else (system "sleep 0.05") (wait (+ k 1)))))

(define out-8 (string-append here "/out8.txt"))
(define trace-8 (string-append here "/trace8.txt"))
(system (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH THEOURGIA_TRACE=1 "))
                       " scheme --script ../cli.ss insert --title LOST-ANSWER-CANARY --store " store-a
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


;; ---- F-10 `theourgia serve` is this program ---------------------------------
;;
;; ⛔ THE VERB USED TO EXEC A PYTHON DAEMON. It is Scheme now, and the row
;; asks the two questions that matter separately: did a daemon come up
;; that this command line can talk to, and did anything call python to
;; make that happen. Either one alone can be true while the other is
;; false -- a daemon can come up by the old route, and no python can run
;; because nothing came up at all.
(define serve-log (string-append here "/serve-verb.log"))
(when (file-exists? marker) (delete-file marker))
(system (string-append "rm -f " socket-a))
(system (string-append (env-prefix (string-append "PATH=" fake-bin ":$PATH "))
                       " scheme --script ../cli.ss serve " store-a
                       " --socket " socket-a " > " serve-log " 2>&1 &"))
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

(system (string-append "pkill -f \"cli.ss serve " store-a "\" 2>/dev/null"))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\ncli-forward complete\n" rows bad)
(exit (if (zero? bad) 0 1))
