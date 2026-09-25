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

(define here (string-append scratch-base "/detach-" pid-text))
(define sock-here (string-append socket-base "/detach-" pid-text))
(define store (string-append here "/store"))
(define cli "../core.sc")
;; The daemon program since F46: `serve` is its verb and no other program's.
(define daemon "../theourgiad.sc")

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
;; answered 0 on macOS when this was written (2026-09-19), which would
;; have made every row below agree for the wrong reason, so this goes
;; through getsid(2) directly.
;;
;; NOTE: THIS KIND OF OBSERVATION IS SAFE TO LEAVE AS HISTORY, and it is
;; worth saying why, because the same shape appears twice more in this
;; file: the note beside the launcher saying `setsid(1)` is not on macOS,
;; and the one beside the child's stdio saying `<&-` is not portable enough
;; to rely on. All three explain why a tool was NOT used.
;;
;; NEVER: AND THEY ARE NAMED, NOT NUMBERED. This cited two line numbers, and
;; they were wrong by the time the round ended -- wrong because of edits made
;; to this same file, in the same sitting, by the person who wrote them. A
;; line number in a comment is a fact about a file that the comment's own
;; neighbours can change.
;; A dated reading that justifies a REJECTION cannot decay dangerously:
;; if the platform changes, nothing in this file was resting on the old
;; behaviour. It is the readings that justify RELIANCE that need a row,
;; because those are the ones a change can quietly falsify.
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

(system (string-append "rm -rf " here " " sock-here "; mkdir -p " store " " here "/home " sock-here))
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
(define dt-sock (string-append sock-here "/dt.sock"))
(define dt-pidfile (string-append here "/dt.pid"))
(system (string-append (env-prefix "") " scheme --script " daemon
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
(define fg-sock (string-append sock-here "/fg.sock"))
(define fg-pidfile (string-append here "/fg.pid"))
(system (string-append (env-prefix "") " scheme --script " daemon
                       " serve " store " --socket " fg-sock
                       " > " here "/fg.log 2>&1 & echo $! > " fg-pidfile))
(wait-for fg-sock)
(define fg-pid (trim (file-text fg-pidfile)))
(define fg-sid (sid-of fg-pid (string-append here "/fgsid.txt")))

(want "D-2 TWIN: a serve without --detach stays in the caller's session"
      (if (string=? fg-sid mine) 'same-as-the-caller (list fg-sid 'not mine))
      'same-as-the-caller)

(system (string-append "kill " fg-pid " 2>/dev/null; sleep 1"))

;; ---- D-2b `--detach` with no `--log` refuses, and says what serve takes --
;;
;; NEVER: AND THE FORM IT SAYS IS THE ONE FORM. This refusal used to write a
;; usage form of its own -- `(usage (serve "--detach" "--log" <path>))` --
;; which is not a fragment of `serve-usage` but a different statement in the
;; same notation: `serve-usage` brackets `--detach`, meaning optional, and
;; that spelling did not, meaning required. It read as "serve needs
;; --detach --log <path>", which is false.
;;
;; The options gate found it as a second spelling of one verb's usage form.
;; Nothing here had asked what this refusal says, so nothing would have
;; noticed the false one either: this row is the missing half.
(define nolog-out (string-append here "/nolog.txt"))
(define nolog-code
  (system (string-append (env-prefix "") " scheme --script " daemon
                         " serve " store " --socket " sock-here "/nolog.sock --detach"
                         " > " nolog-out " 2>&1")))
;; NEVER: THE FIRST DATUM ON THAT STREAM IS NOT THE ANSWER. The CLI prints
;; `(theourgia machine-home ...)` when it has to create the home directory,
;; so a reader taking the first form read the notice and reported it as the
;; refusal. Every datum is read and the one that is an `error` is the answer.
(define nolog-data
  (guard (e (#t '()))
    (with-input-from-file nolog-out
      (lambda ()
        (let loop ((acc '()))
          (let ((x (read)))
            (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
(define nolog-answer
  (let look ((xs nolog-data))
    (cond ((null? xs) (list 'no-answer-in (length nolog-data)))
          ((and (pair? (car xs)) (eq? (car (car xs)) 'error)) (car xs))
          (else (look (cdr xs))))))

;; NEVER: `(and (pair? nolog-answer) ...)` WAS AN INVARIANT, NOT A CHECK.
;; `nolog-answer` is either an `error` form or the `(no-answer-in n)` this
;; file builds, and both are pairs -- so those guards were true whatever
;; happened and the row read as if it had asked something it had not. The
;; clauses below take the parts directly and let a wrong shape show as a
;; wrong reading.
;;
;; NEVER: AND THE SENTENCE ABOVE WAS TRUE OF NOTHING FOR A ROUND. It said
;; the guards had been taken out while four of them were still there, one
;; in each row below. A comment that describes a change nobody made is
;; worse than no comment: the next reader believes the question was asked.
;; The readers are now named, so the rows below cannot quietly grow a
;; guard back without this file saying so in one place.
;;
;; NOTE: AND THEY ARE THE SAME READERS THE WITNESS ROW USES. A witness that
;; fed a bad shape to its own copy of these expressions would answer the
;; same whether the rows were right or only the copy was.
(define (answer-head a) (car a))

(define (answer-second a)
  (if (pair? (cdr a)) (cadr a) 'no-second-element))

(define (usage-clause-of a)
  (exists (lambda (x) (and (pair? x) (eq? (car x) 'usage) x)) (cdr a)))
;;
;; AND HOW MANY ERROR FORMS THE STREAM CARRIED. One is the answer; more than
;; one means the run said something this row is not looking at, and taking
;; the first would hide it.
(define nolog-error-count
  (let loop ((xs nolog-data) (n 0))
    (cond ((null? xs) n)
          ((and (pair? (car xs)) (eq? (car (car xs)) 'error)) (loop (cdr xs) (+ n 1)))
          (else (loop (cdr xs) n)))))

(want "D-2b --detach without --log is refused, and the answer names the error"
      (list (if (= 0 nolog-code) 'EXIT-ZERO 'refused)
            nolog-error-count
            (answer-head nolog-answer)
            (answer-second nolog-answer))
      (list 'refused 1 'error 'detach-needs-a-log))

;; WITNESS: AND A SHAPE THAT IS NOT AN ANSWER SHOWS AS A WRONG READING.
;; This is the row that makes the removal above mean something. With the
;; guards in place, a broken answer and the `(no-answer-in n)` this file
;; builds came back through the same tidy path, so the row could not tell
;; "the program said this" from "the reader could not take it apart".
;;
;; NOTE: IT USES THE READERS THE ROW ABOVE USES, not a copy of them. A
;; witness with its own copy of these expressions would answer the same
;; whether the row was right or only the copy was, which is the shape this
;; batch keeps finding.
;;
;; NOTE: THE RAISE IS ASKED FOR BY ITS TAG, NOT BY ITS TEXT. Which words
;; an implementation puts in the condition are not this file's business,
;; and pinning them would make the row red on a different Chez.
;; NEVER: AND IT EXERCISES ALL THREE READERS. The first version called two of
;; them and left `usage-clause-of` -- the one the two rows below depend on --
;; untouched, so a guard growing back in THAT reader would have gone unseen by
;; the row whose whole purpose is to see it.
(want "D-2b WITNESS: the readers do not tidy away a shape that is not an answer"
      (list (car (caught (answer-head 'not-an-answer)))
            (car (caught (answer-second 'not-an-answer)))
            (car (caught (usage-clause-of 'not-an-answer)))
            (answer-head '(error detach-needs-a-log))
            (answer-second '(error detach-needs-a-log))
            (answer-second '(error))
            (usage-clause-of '(error detach-needs-a-log (usage (serve))))
            (usage-clause-of '(error detach-needs-a-log)))
      (list 'RAISED 'RAISED 'RAISED
            'error 'detach-needs-a-log 'no-second-element
            '(usage (serve)) #f))

;; AND THE USAGE CLAUSE IS THE WHOLE FORM -- compared against the one this
;; program keeps, read out of `theourgiad.sc`.
;;
;; NEVER: THE FIRST VERSION OF THIS ROW WAS TITLED "is serve's own form" AND
;; DID NOT COMPARE IT. It checked that the clause's head was `serve` and that
;; `--detach` was bracketed, which is a check on the SHAPE. The question this
;; row exists for is whether that refusal tells the caller what the verb
;; really accepts, and a shape check cannot answer it: a form with the right
;; head and the right bracketing and a missing option would pass.
(define serve-usage-in-source
  (let ((data (guard (e (#t '()))
                (with-input-from-file daemon
                  (lambda ()
                    (let loop ((acc '()))
                      (let ((x (read)))
                        (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
    (let look ((xs data))
      (cond
        ((null? xs) #f)
        ((and (pair? (car xs)) (eq? (car (car xs)) 'define)
              (pair? (cdr (car xs))) (eq? (cadr (car xs)) 'serve-usage)
              (pair? (cddr (car xs))) (pair? (caddr (car xs)))
              (eq? (car (caddr (car xs))) 'quote))
         (cadr (caddr (car xs))))
        (else (look (cdr xs)))))))

;; CONTROL: the form was found in the source at all. Without this, a failure
;; to read it would make the comparison below compare `#f` with `#f` on some
;; future day when the refusal also stopped carrying one.
(want "D-2b CONTROL: serve-usage was read out of theourgiad.sc and is a serve form"
      (list (and (pair? serve-usage-in-source) #t)
            (and (pair? serve-usage-in-source) (car serve-usage-in-source)))
      (list #t 'serve))

(want "D-2b TWIN: the usage clause it carries IS serve-usage, compared whole"
      (let ((u (usage-clause-of nolog-answer)))
        (list (and u #t)
              (and u (equal? (cadr u) serve-usage-in-source))))
      (list #t #t))

;; AND THE NOTATION, ASKED SEPARATELY. The row above would also pass if both
;; sides were wrong in the same way; this one says what the notation means
;; and is the row that fails when a partial form is written.
;;
;; NEVER: THESE TWO PREDICATES ARE COMPLEMENTS ONLY IF THE OPTION APPEARS
;; ONCE. A form naming `--detach` twice -- bare in one place and bracketed in
;; another -- would answer true to both, and the pair would say nothing. The
;; premise is asserted rather than assumed.
(define (count-occurrences form)
  (let loop ((xs form) (bare 0) (bracketed 0))
    (cond
      ((null? xs) (list bare bracketed))
      ((equal? (car xs) "--detach") (loop (cdr xs) (+ bare 1) bracketed))
      ((and (pair? (car xs)) (equal? (car (car xs)) "--detach"))
       (loop (cdr xs) bare (+ bracketed 1)))
      (else (loop (cdr xs) bare bracketed)))))

(want "D-2b and the notation says --detach is optional, exactly once"
      (let ((u (usage-clause-of nolog-answer)))
        (if u (count-occurrences (cadr u)) 'no-usage-clause))
      (list 0 1))


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
                         " python3 " leader " scheme --script " daemon
                         " serve " store " --socket " sock-here "/d3.sock --detach"
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
      (if (file-exists? (string-append sock-here "/d3.sock")) 'SERVED-ANYWAY 'did-not-serve)
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

(define d4-sock (string-append sock-here "/d4.sock"))
(define d4-log (string-append here "/d4-serve.log"))

(system (string-append (env-prefix "") " python3 " closer
                       " scheme --script " daemon " serve " store
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
(system (string-append "rm -rf " here " " sock-here))
(printf "rows: ~a\n~a failures\ndetach complete\n" rows bad)
(exit (if (zero? bad) 0 1))
