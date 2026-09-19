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

;; `serve --detach`: whether the daemon leaves the session that started
;; it, and what it does when it cannot.
;;
;; KEY: A DAEMON THE CLIENT STARTS MUST OUTLIVE THE CLIENT. A client runs
;; for one call; the daemon it starts serves every call after that. If it
;; stayed in the client's session it would be taken down by anything
;; aimed at the client -- a ctrl-C, a shell closing -- and the next
;; client would start another one, which is the behaviour the single
;; daemon per store exists to prevent.
;;
;; NEVER: AND ONLY UNDER `--detach`. A `serve` a person runs from a terminal
;; keeps its session and its output, because that is how it is read. The
;; second row is not decoration: making the detach unconditional would
;; pass the first row and break every fixture that reads a foreground
;; daemon's diagnostics, and nothing else here would notice.
;;
;; NOTE: `setsid` CANNOT SUCCEED TWICE. A process that is already a session
;; leader gets EPERM, and that is the normal state of a process started
;; by certain launchers -- so the third row starts one that way on
;; purpose. The refusal must be loud: a daemon that could not leave the
;; session but served anyway would die with the terminal, after the
;; client had already been answered once and concluded it was there.

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
(define pid-text (number->string (get-process-id)))
(define here (string-append "/tmp/detach-" pid-text))
(define store (string-append here "/store"))
(define cli "../cli.ss")

(define (env-prefix extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                 "THEOURGIA_HOME=" here "/home " extra))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(define (trim text)
  (let loop ((i (string-length text)))
    (cond ((zero? i) "")
          ((memv (string-ref text (- i 1)) '(#\newline #\space #\return)) (loop (- i 1)))
          (else (substring text 0 i)))))

;; THE SESSION ID IS ASKED OF THE SYSTEM, not inferred. `ps -o sess=`
;; answers 0 on macOS, which would have made every row below agree for
;; the wrong reason, so this goes through getsid(2) directly.
(define (sid-of pid-string path)
  (system (string-append
            "python3 -c 'import os,sys;print(os.getsid(int(sys.argv[1])))' "
            pid-string " > " path " 2>/dev/null || echo unknown > " path))
  (trim (file-text path)))

(define (wait-for path)
  (let loop ((i 0))
    (cond ((file-exists? path) #t)
          ((> i 300) #f)
          (else (system "sleep 0.05") (loop (+ i 1))))))

(system (string-append "rm -rf " here "; mkdir -p " store " " here "/home"))
(system (string-append (env-prefix "") " scheme --script " cli
                       " init --store " store " > /dev/null 2>&1"))
;; NOTE: A BLOCK, SO THAT "IT ANSWERED" IS DISTINGUISHABLE FROM "IT SAID
;; NOTHING". `outline` on an empty store answers correctly with empty
;; text, and a row that only looked for a non-empty reply passed against
;; a daemon that was never reached -- which is what it did here, until
;; the same query was run against a foreground daemon and gave the same
;; empty output.
(system (string-append (env-prefix "") " scheme --script " cli
                       " insert --title DETACH-MARKER --store " store
                       " > /dev/null 2>&1"))

;; ---- the fixture's own session, for comparison ---------------------------
(define mine (sid-of pid-text (string-append here "/mine.txt")))

;; ---- D-1 a detached daemon leaves this session ---------------------------
(define dt-sock (string-append here "/dt.sock"))
(define dt-pidfile (string-append here "/dt.pid"))
(system (string-append (env-prefix "") " scheme --script " cli
                       " serve " store " --socket " dt-sock " --detach"
                       " --log " here "/serve.log"
                       " > " here "/dt.log 2>&1 & echo $! > " dt-pidfile))
(wait-for dt-sock)
(define dt-pid (trim (file-text dt-pidfile)))
(define dt-sid (sid-of dt-pid (string-append here "/dtsid.txt")))

;; NEVER: "NOT THE SAME AS MINE" IS ALSO TRUE OF A PROCESS THAT IS NOT
;; THERE. `sid-of` answers "unknown" when getsid fails, which is what it
;; does for a pid that has already exited -- so this row passed, once,
;; against a daemon that had refused to start at all. Whatever it is, it
;; has to be a session.
(want "D-1 a detached daemon is not in the session that started it"
      (cond
        ((string=? dt-sid "unknown") (list 'NO-SUCH-PROCESS dt-pid))
        ((string=? dt-sid mine) (list 'SAME dt-sid))
        (else 'left-it))
      'left-it)

;; KEY: AND IT IS A SESSION LEADER, not merely somewhere else. "Different
;; from mine" would also be true of a process that had been adopted into
;; some third session by accident; leading its own is the property.
(want "D-1 and it leads a session of its own"
      (if (string=? dt-sid dt-pid) 'leads-its-own (list dt-sid 'not dt-pid))
      'leads-its-own)

;; NEVER: THE POINT OF ALL THIS: it still answers once the caller is gone.
;; The rows above are about a number; this one is about the daemon.
(define answer-file (string-append here "/answer.txt"))
(system (string-append (env-prefix "") " scheme --script " cli
                       " outline --store " store " --socket " dt-sock
                       " > " answer-file " 2>&1"))
(want "D-1 and it answers a request, from the right store"
      (if (contains? (file-text answer-file) "DETACH-MARKER")
          'answered
          (list 'said (file-text answer-file)))
      'answered)

(system (string-append "kill " dt-pid " 2>/dev/null; sleep 1"))

;; ---- D-2 TWIN: a foreground serve stays where it was ---------------------
(define fg-sock (string-append here "/fg.sock"))
(define fg-pidfile (string-append here "/fg.pid"))
(system (string-append (env-prefix "") " scheme --script " cli
                       " serve " store " --socket " fg-sock
                       " > " here "/fg.log 2>&1 & echo $! > " fg-pidfile))
(wait-for fg-sock)
(define fg-pid (trim (file-text fg-pidfile)))
(define fg-sid (sid-of fg-pid (string-append here "/fgsid.txt")))

(want "D-2 TWIN: a serve without --detach stays in the caller's session"
      (if (string=? fg-sid mine) 'same-as-the-caller (list fg-sid 'not mine))
      'same-as-the-caller)

(system (string-append "kill " fg-pid " 2>/dev/null; sleep 1"))

;; ---- D-3 a detach that cannot happen is loud -----------------------------
;;
;; The launcher makes ITSELF a session leader and then execs the CLI, so
;; the process that runs `--detach` is already leading a session and
;; `setsid` answers EPERM. NOTE: `setsid(1)` is not on macOS, which is why
;; this is done with os.setsid in the launcher rather than a command.
(define leader (string-append here "/leader.py"))
(call-with-output-file leader
  (lambda (port)
    (for-each (lambda (l) (display l port) (newline port))
      (list "import os, sys"
            "os.setsid()"
            "os.execvp(sys.argv[1], sys.argv[1:])"))))

(define d3-out (string-append here "/d3.txt"))
(define d3-rc
  (system (string-append (env-prefix "THEOURGIA_TRACE=1 ")
                         " python3 " leader " scheme --script " cli
                         " serve " store " --socket " here "/d3.sock --detach"
                         " --log " here "/d3-serve.log"
                         " > " d3-out " 2>&1")))

(want "D-3 a detach that cannot be done exits non-zero"
      (if (zero? d3-rc) (list 'EXITED-ZERO (file-text d3-out)) 'non-zero)
      'non-zero)

;; NEVER: AND IT SAYS WHY. A non-zero exit on its own is satisfied by any
;; crash at all, including one that never reached the detach.
(want "D-3 and it names the failure"
      (if (contains? (file-text d3-out) "detach-failed") 'named
          (list 'said (file-text d3-out)))
      'named)

;; NOTE: THE ERRNO IS THE PART THAT IS ACTIONABLE, and it is the part most
;; easily lost: `setsid!` raises an assertion carrying it as an irritant,
;; and a handler that caught the condition without reading the irritant
;; would report `unknown` here and still pass the row above.
;; NEVER: THE ROW ASKS FOR THE NUMBER, NOT FOR THE ABSENCE OF A WORD. Written
;; as "the output does not say unknown" it passed while the detach was
;; not running at all -- an unrelated crash says neither "unknown" nor an
;; errno, and satisfied it. It has to see the errno itself.
(want "D-3 and it carries the errno rather than 'unknown'"
      (let ((text (file-text d3-out)))
        (cond
          ((contains? text "unknown") 'LOST-THE-ERRNO)
          ((contains? text "(errno ") 'carried)
          (else (list 'no-errno-at-all text))))
      'carried)

;; NEVER: AND IT DID NOT GO ON TO SERVE. The refusal would be pointless if
;; the process carried on and bound the socket anyway.
(want "D-3 and no socket was left behind"
      (if (file-exists? (string-append here "/d3.sock")) 'SERVED-ANYWAY 'did-not-serve)
      'did-not-serve)

;; ---- D-4 detaching with stdio already closed -------------------------------
;;
;; NEVER: A DESCRIPTOR OPENED WHILE 0, 1 OR 2 IS CLOSED CAN LAND ON ONE OF
;; THEM, and a later copy onto that number then changes what the other
;; variable refers to. The first version of `redirect-stdio!` was
;; justified by reasoning about which arrangements are safe; a reviewer
;; reported the overwrite twice against that reasoning. Both descriptors
;; are now moved above 2 before any copying, which removes the question
;; rather than answering it -- and this row is the arrangement that was
;; reported.
;;
;; NOTE: THE CHILD IS STARTED WITH STDIN CLOSED, which a shell will not do
;; for you: `<&-` is not portable enough to rely on here, so the fork and
;; the close are done explicitly.
(define closer (string-append here "/closed-stdin.py"))
(call-with-output-file closer
  (lambda (port)
    (for-each (lambda (l) (display l port) (newline port))
      (list "import os, sys, time"
            "pid = os.fork()"
            "if pid == 0:"
            "    os.close(0)"
            "    os.execvp(sys.argv[1], sys.argv[1:])"
            "    os._exit(127)"
            "time.sleep(4)"))))

(define d4-sock (string-append here "/d4.sock"))
(define d4-log (string-append here "/d4-serve.log"))

(system (string-append (env-prefix "") " python3 " closer
                       " scheme --script " cli " serve " store
                       " --socket " d4-sock " --detach --log " d4-log
                       " > /dev/null 2>&1"))

(want "D-4 a daemon detached with stdin closed still binds its socket"
      (if (file-exists? d4-sock) 'bound (list 'no-socket (file-text d4-log)))
      'bound)

;; NEVER: AND ITS LOG IS ITS OWN. If the log descriptor had been overwritten,
;; this is where it would show: the file would be empty or hold something
;; that is not the daemon's line.
(want "D-4 and its own first line reached the log it was given"
      (if (contains? (file-text d4-log) "(serving (store") 'its-own-line
          (list 'log (file-text d4-log)))
      'its-own-line)

;; NEVER: AND IT ANSWERS. Binding and logging would both be true of a daemon
;; that then did nothing.
(define d4-answer (string-append here "/d4-answer.txt"))
(system (string-append (env-prefix "") " scheme --script " cli
                       " outline --store " store " --socket " d4-sock
                       " > " d4-answer " 2>/dev/null"))

(want "D-4 and it serves a request"
      (if (contains? (file-text d4-answer) "DETACH-MARKER") 'answered
          (list 'said (file-text d4-answer)))
      'answered)

(system (string-append "pkill -f 'serve " store "' 2>/dev/null; sleep 1"))
(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\ndetach complete\n" rows bad)
(exit (if (zero? bad) 0 1))
