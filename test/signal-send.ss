;; Copyright 2018 - 2026 guenchi
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

;; A SIGNAL ONLY TO A PROCESS THIS RUN REGISTERED, AND ONLY WHILE IT IS STILL
;; THAT PROCESS. One definition, included by every fixture that signals a pid
;; it read from a pid file or from $! (daemon-idle, daemon-namespace, log15,
;; launch-self, runner-self, cli2, f0-ondemand, append-prefix, change-stream,
;; client-socket, daemon-e1, daemon-socket, detach, eval-lang, f3-wire-utf8,
;; facade-proc, fold-failure, liveness, liveness-streams, mcp-shell,
;; opener-route, ownership, smoke-shortwrite-nb, startup-ratio) or found by ps
;; (change-stream-client, entry-split, read-at-cut, store-in-git,
;; unreadable-acknowledge-routes, unreadable-routes).
;;
;; NEVER: A SIGNAL TO A PID THAT WAS NOT VERIFIED. A pid is reused once its
;; process is gone, so a send to a process that already exited can reach an
;; unrelated one. signal! sends only when the pid was registered by started!,
;; answers kill -0 AND its start instant (ps lstart) is the one recorded when
;; it was registered; otherwise it sends nothing and prints
;; "NOTE <sig> to <pid> not sent: already gone" on the output. A pid never
;; registered, or gone before it was registered (no recorded instant), is
;; never signalled.
;;
;; REGISTER WHERE THE PROCESS STARTS: started! right after the call that
;; spawned it returns, or at the first read of a pid file the process wrote
;; itself. A registration just before the send would record whatever holds
;; the pid at that moment, and the check would then pass for it.
;;
;; IT DEPENDS ON NOTHING THE INCLUDING FILE DEFINES: only system, process and
;; format, because the fixtures' own shell helpers differ in shape ((sh cmd)
;; in some, (sh . parts) in others). The including file defines none of
;; started, identities, pid-identity, started!, alive?, signal!, pid-of-file,
;; the lookups below (word-after?, serves-store?, command-processes,
;; command-pids, serve-processes, serve-pids, serve-lines), started-at!,
;; register!, signal-processes!, signal-loop-text or a name starting
;; signal-send- itself; it
;; may set! identities, as the CONTROL rows on the guard do.
;;
;; A pid is an integer above 1: kill -0 of 0 or -1 asks about a whole group or
;; every process, so anything else is neither registered nor alive.
;;
;; STATED LIMIT: lstart counts whole seconds, so a pid reused within the
;; second its first process started reads as the same process.
(define started '())
(define identities '())
;; -> the start instant ps prints for PID, trimmed of blanks and the newline
;; so that it compares equal with the same field read from a listing, or #f.
(define (pid-identity pid)
  (let* ((p (process (format "ps -o lstart= -p ~a 2>/dev/null" pid)))
         (t (get-string-all (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (and (string? t)
         (let ((instant (signal-send-trim t)))
           (and (> (string-length instant) 0) instant)))))
(define (alive? pid)
  (and (integer? pid) (> pid 1) (zero? (system (format "kill -0 ~a 2>/dev/null" pid)))))
(define (started! pid)
  (when (and (integer? pid) (> pid 1))
    (set! started (cons pid started))
    (set! identities (cons (cons pid (pid-identity pid)) identities)))
  pid)
(define (signal! pid sig)
  (let ((known (and pid (assv pid identities))))
    (cond
      ((and known (cdr known) (alive? pid) (equal? (cdr known) (pid-identity pid)))
       (system (format "kill -~a ~a 2>/dev/null" sig pid))
       'sent)
      (else
       (printf "NOTE ~a to ~a not sent: already gone\n" sig pid)
       'already-gone))))

;; (pid-of-file path) -> the integer a pid file holds, or #f when the file is
;; absent, empty or holds something else.
(define (pid-of-file path)
  (guard (e (#t #f))
    (let ((d (call-with-input-file path read)))
      (and (integer? d) d))))

;; THE PROCESSES PS LISTS, AND REGISTRATION FROM THE SAME LISTING. One
;; listing, `ps -ax -o pid= -o lstart= -o command=`, read whole: each line is
;; a pid, its start instant (lstart, a field of 24 characters) and its
;; command text. A lookup answers, for each line whose command text a
;; predicate accepts, the pair (pid . instant), and has no side effect, so the
;; polls and the counts use it as it is. A lookup whose processes are then
;; signalled registers them with the instant read in that same listing
;; (register!, started-at!), so a pid that ends and is reused between the
;; listing and a later `ps -o lstart= -p` is not registered as the newcomer.
;; signal-processes! registers and sends in one call, for a send that follows
;; its lookup at once; a list held and sent later is registered where it is
;; found.
;;
;; KEY: EXACTLY THE STORE. serves-store? takes the word after `serve`, or
;; after `serve --store` (the client's spelling of a store that begins with
;; `-`), and asks that it equal the store: a store whose path has this one's
;; as a prefix (/x/s against /x/s1) does not match, and the program's form
;; (theourgiad.sc, theourgiad.so, any path) does not matter.
(define (signal-send-trim text)
  (let loop ((a 0) (b (string-length text)))
    (cond ((and (< a b) (char-whitespace? (string-ref text a))) (loop (+ a 1) b))
          ((and (< a b) (char-whitespace? (string-ref text (- b 1)))) (loop a (- b 1)))
          (else (substring text a b)))))
(define (signal-send-split text break?)
  (let loop ((i 0) (start #f) (acc '()))
    (cond ((= i (string-length text))
           (reverse (if start (cons (substring text start i) acc) acc)))
          ((break? (string-ref text i))
           (loop (+ i 1) #f (if start (cons (substring text start i) acc) acc)))
          (else (loop (+ i 1) (or start i) acc)))))
;; (word-after? text first then) -> #t when a word of TEXT equal to FIRST is
;; followed at once by a word equal to THEN.
(define (word-after? text first then)
  (let loop ((ws (signal-send-split text char-whitespace?)))
    (cond ((or (null? ws) (null? (cdr ws))) #f)
          ((and (string=? (car ws) first) (string=? (cadr ws) then)) #t)
          (else (loop (cdr ws))))))
(define (serves-store? text store)
  (let loop ((ws (signal-send-split text char-whitespace?)))
    (cond ((or (null? ws) (null? (cdr ws))) #f)
          ((and (string=? (car ws) "serve")
                (or (string=? (cadr ws) store)
                    (and (string=? (cadr ws) "--store") (pair? (cddr ws)) (string=? (caddr ws) store))))
           #t)
          (else (loop (cdr ws))))))
;; -> ((pid instant command) ...), one per line of the listing.
(define (signal-send-listing)
  (let* ((p (process "ps -ax -o pid= -o lstart= -o command="))
         (t (get-string-all (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (fold-right
      (lambda (line acc)
        (let* ((n (string-length line))
               (a (let skip ((i 0)) (if (and (< i n) (char=? (string-ref line i) #\space)) (skip (+ i 1)) i)))
               (b (let digits ((i a)) (if (and (< i n) (char-numeric? (string-ref line i))) (digits (+ i 1)) i)))
               (c (let skip ((i b)) (if (and (< i n) (char=? (string-ref line i) #\space)) (skip (+ i 1)) i))))
          (if (and (> b a) (> c b) (<= (+ c 24) n))
              (cons (list (string->number (substring line a b))
                          (signal-send-trim (substring line c (+ c 24)))
                          (signal-send-trim (substring line (+ c 24) n)))
                    acc)
              acc)))
      '()
      (signal-send-split (if (eof-object? t) "" t) (lambda (c) (char=? c #\newline))))))
(define (command-processes has-line?)
  (map (lambda (e) (cons (car e) (cadr e)))
       (filter (lambda (e) (has-line? (caddr e))) (signal-send-listing))))
(define (command-pids has-line?) (map car (command-processes has-line?)))
(define (serve-processes store) (command-processes (lambda (text) (serves-store? text store))))
(define (serve-pids store) (map car (serve-processes store)))
(define (serve-lines store)
  (map caddr (filter (lambda (e) (serves-store? (caddr e) store)) (signal-send-listing))))
;; (started-at! pid instant) records the pair as started! does, with the
;; instant the caller read; (register! processes) does it for each pair and
;; answers the list.
(define (started-at! pid instant)
  (when (and (integer? pid) (> pid 1) (string? instant) (> (string-length instant) 0))
    (set! started (cons pid started))
    (set! identities (cons (cons pid instant) identities)))
  pid)
(define (register! processes)
  (for-each (lambda (p) (started-at! (car p) (cdr p))) processes)
  processes)
;; -> the list of signal!'s answers, one per process.
(define (signal-processes! processes sig)
  (map (lambda (p) (signal! (car p) sig)) (register! processes)))

;; A PROGRAM THAT SIGNALS ITSELF ON A CLOCK, CHECKED IN ITS OWN SHELL TEXT.
;; (signal-loop-text sig rounds period) -> the text of one Scheme form, for a
;; generated program: it starts a background shell loop that sends SIG to the
;; program up to ROUNDS times, PERIOD seconds apart. The program cannot call
;; signal! from inside that loop, so the loop makes the same check itself: the
;; program's start instant (ps lstart) is read once, before the loop is forked,
;; and each round sends only while the pid still has that instant. A mismatch
;; ends the loop, so a pid reused after the program exits is never signalled.
;;
;; NEVER: AN EMPTY CAPTURE IS NO IDENTITY. A ps that printed nothing when the
;; instant was read leaves it empty, and a later empty reading would then
;; compare equal and send, as signal! refuses a pid with no recorded instant.
;; The loop runs only while the captured instant is not empty (codex c1 F1).
;;
;; NEVER: $$ IS NOT THE PID. The loop runs in the shell that system starts, and
;; $$ there is that shell; the target is the program, whose pid the form reads
;; with get-process-id and splices into the text.
;;
;; KEY: THE CHECK AND THE SEND ON ONE LINE. The comparison and the kill are one
;; condition of the while, so there is no send without the check.
(define (signal-loop-text sig rounds period)
  (let ((template
         (string-append
           "s=$(ps -o lstart= -p ~a); ( i=0; while [ $i -lt " (format "~a" rounds) " ]"
           " && [ -n \"$s\" ] && [ \"$(ps -o lstart= -p ~a)\" = \"$s\" ] && kill -" (format "~a" sig) " ~a 2>/dev/null;"
           " do sleep " (format "~a" period) "; i=$((i+1)); done ) > /dev/null 2>&1 &")))
    (format "(let ((pid (get-process-id))) (system (format ~s pid pid pid)))" template)))
