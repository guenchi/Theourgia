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

;;; theourgia -- the client.
;;;
;;; KEY: IT KNOWS TRANSPORT AND NOTHING ELSE. It finds the store's daemon,
;;; sends what it was given, writes back what it gets and exits with the
;;; number it was handed. It does not know what any verb means, which
;;; options any verb takes, or whether an answer is a success.
;;;
;;; NEVER: AND THAT IS WHY IT IS FAST. `(theourgia client)` reaches five
;;; libraries -- itself, ffi, digest, render and trace -- and none of the
;;; core, the scheduler, the socket machinery or the daemon. A client
;;; that had to load them would pay for the server it is trying to talk
;;; to, on every call, which is the whole reason for the split.
;;;
;;; NEVER: THE ARGUMENTS ARE NOT PARSED. They are scanned for the handful of
;;; options that say WHERE to send the request -- and everything else is
;;; passed through in the order it arrived, byte for byte. A client that
;;; parsed properly would need every verb's option table, and that table
;;; is knowledge about verbs.

(import (chezscheme)
        (only (theourgia client)
              socket-path serve-log-path request-frame
              call! ensure-daemon! answer-field readable-shape? exit-code?
              verb-spelling-error)
        ;; NOTE: THE ARGUMENT TABLES, AND NOTHING ELSE. `(theourgia arguments)`
        ;; is a pure library with no dependencies of its own: it reaches
        ;; neither the dispatcher, nor the scheduler, nor the network. It
        ;; is imported so that "does this verb read standard input" can be
        ;; asked of the tables that answer it everywhere else.
        (only (theourgia arguments) parse-arguments argument-wants-stdin?)
        ;; THE TABLE AND THE RECORD (F100b point 4). (theourgia client)
        ;; already brings ffi into this program's closure; (theourgia
        ;; answers) is one small library that does no filesystem work.
        (only (theourgia ffi) with-mutation-record mutation-record)
        (only (theourgia answers) classify-failure))

;; ---- what the client itself understands ---------------------------------
;;
;; NEVER: FOUR OPTIONS AND THREE VERBS. That is the whole of this program's
;; knowledge, and both lists are written once, here.
;;
;; NOTE: THE OPTIONS ARE THE ONES THAT SAY WHERE A REQUEST GOES. `--writer`
;; is deliberately NOT among them: it says who a single call is for,
;; which is the server's business, and it travels through untouched.
(define where-options '("--store" "--actor" "--socket"))

;; NEVER: THESE THREE RUN HERE, NOT OVER A SOCKET. `init` has no daemon to
;; talk to yet -- it is what creates the store. `serve` IS the daemon.
;; `eval` runs a child process under a supervisor that has to be this
;; process's child, and a daemon cannot hand that back down a socket.
(define local-verbs '(init serve eval))

(define (local-by-name? verb) (memq verb local-verbs))

;; ---- scanning, not parsing ----------------------------------------------
;;
;; NOTE: A KNOWN OPTION'S VALUE IS CONSUMED, which is what keeps the verb
;; findable: in `theourgia --store /x outline`, `/x` is not the verb, and
;; the only way to know that is to know `--store` takes a value. Options
;; this program does not know are left alone WITH their values, so an
;; unknown option cannot swallow the verb -- the verb is already behind
;; it by then, because the verb comes first among the things that are not
;; `where` options.
(define (scan argv)
  (let loop ((xs argv) (verb #f) (args '()) (store #f) (actor #f)
             (socket #f) (wire? #f) (literal? #f))
    (cond
      ((null? xs)
       (list verb (reverse args) store actor socket wire?))
      ;; NEVER: NOTHING AFTER `--` IS AN OPTION, INCLUDING THESE. The
      ;; separator is how a caller passes a value that looks like an
      ;; option, and a scanner that reads past it takes the very
      ;; arguments the caller protected. Measured before this existed:
      ;; `search -- --store zzz --store <path>` lost BOTH occurrences and
      ;; `search` was left with no query at all.
      ;;
      ;; NOTE: THE SEPARATOR ITSELF IS KEPT, not dropped: the server's parser
      ;; is the one that decides what it means, and a client that removed
      ;; it would change the command it was asked to send.
      (literal?
       (loop (cdr xs) verb (cons (car xs) args) store actor socket wire? #t))
      ((string=? (car xs) "--")
       (if verb
           (loop (cdr xs) verb (cons (car xs) args) store actor socket wire? #t)
           (loop (cdr xs) verb args store actor socket wire? #t)))
      ((and (member (car xs) where-options) (pair? (cdr xs)))
       (let ((name (car xs)) (value (cadr xs)))
         (loop (cddr xs) verb args
               (if (string=? name "--store") value store)
               (if (string=? name "--actor") value actor)
               (if (string=? name "--socket") value socket)
               wire? #f)))
      ((string=? (car xs) "--wire")
       (loop (cdr xs) verb args store actor socket #t #f))
      ((not verb)
       (loop (cdr xs) (string->symbol (car xs)) args store actor socket wire? #f))
      (else
       (loop (cdr xs) verb (cons (car xs) args) store actor socket wire? #f)))))

(define (env-value name)
  (let ((v (getenv name))) (and v (> (string-length v) 0) v)))

;; NOTE: AN UNSET WRITER STAYS UNSET and does not fall back to the actor.
;; Two agents that were each given only an actor would otherwise share a
;; draft space the moment their actor names matched -- which is the
;; failure the writer exists to prevent, reintroduced as a convenience.
;; A caller who wants one name passes it twice.
(define (bound-writer) (env-value "THEOURGIA_WRITER"))

(define (bound-actor actor)
  (or actor (env-value "THEOURGIA_ACTOR") (env-value "USER") "cli"))

(define (bound-store store)
  (or store (env-value "THEOURGIA_STORE") "."))

;; ---- running the server here --------------------------------------------

(define (beside-this-program name)
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (string-append (substring argv0 0 cut) "/" name) name)))

;; NOTE: THE INTERPRETER THIS PROGRAM IS RUNNING UNDER, so a tree started
;; with a particular Chez starts its server with the same one.
(define (scheme-binary) (or (getenv "THEOURGIA_SCHEME") "scheme"))

;; NEVER: THE ARGUMENTS ARE HANDED ON UNCHANGED. This program did not parse
;; them and must not re-spell them: the server's own parser is the one
;; that decides what they mean, and anything reassembled here would be a
;; second opinion about a command line that is already correct.
;;
;; NEVER: AND THE BOUND WRITER IS NOT ADDED TO THEM. It used to be spliced in
;; after the verb, for want of an envelope on this path, and that broke
;; the verbs whose grammar has no `--writer`: with an identity bound,
;; `init` -- which is how a store comes to exist at all -- answered its
;; usage line and exited 1, in every spelling. The same splice also
;; suppressed itself whenever the caller's own text contained `--writer`.
;;
;; The writer travels in the environment instead, where this process
;; already found it, and `core.sc` reads it as the dispatcher's default.
;; `exec` keeps the environment, so there is nothing to pass on.
;;
;; KEY: THE PROGRAM IS CHOSEN BY ROLE (F46). `serve` is the daemon, and the
;; daemon is `theourgiad.sc`; every other request answered here -- `init`,
;; `eval`, anything under THEOURGIA_LOCAL -- is `core.sc`'s. One program for
;; all of them would send `init` to the daemon, which refuses every verb but
;; `serve`.
(define (run-server-here! argv)
  (exec-server! (cons (scheme-binary)
                      (cons "--script"
                            (cons (beside-this-program
                                    (if (and (pair? argv) (string=? (car argv) "serve"))
                                        "theourgiad.sc"
                                        "core.sc"))
                                  argv)))))

(define (exec-server! args)
  (let* ((width (foreign-sizeof 'void*))
         (argv (foreign-alloc (* width (+ 1 (length args)))))
         (cells (map (lambda (arg)
                       (let* ((b (string->utf8 arg))
                              (n (bytevector-length b))
                              (p (foreign-alloc (+ n 1))))
                         (do ((i 0 (+ i 1))) ((= i n))
                           (foreign-set! 'unsigned-8 p i (bytevector-u8-ref b i)))
                         (foreign-set! 'unsigned-8 p n 0)
                         p))
                     args)))
    (do ((ps cells (cdr ps)) (i 0 (+ i 1))) ((null? ps))
      (foreign-set! 'void* argv (* i width) (car ps)))
    (foreign-set! 'void* argv (* (length args) width) 0)
    ((foreign-procedure "execvp" (string void*) int) (car args) argv)
    ;; Only reached if the exec failed.
    (put-string (current-error-port) "(error server-unavailable)\n")
    (exit 71)))

;; ---- the answer ---------------------------------------------------------
;;
;;   (answer (stdout "<bytes>") (stderr "<bytes>") (exit <n>) (origin <who>))
;;
;; NEVER: THE BYTES ARE WRITTEN, NOT RE-RENDERED, and the code is TAKEN, not
;; computed. Both are the server's decisions; working either out here
;; would need the rule that says what a verb's answer means, and that
;; rule is in the core this program does not load.
;; NEVER: THE ANSWER IS SOMETHING A PEER SENT, SO ITS SHAPE IS NOT A GIVEN.
;; `assq` demands a proper list of pairs and raises on anything else, so
;; `(answer . broken)` -- a datum that reads perfectly well -- used to
;; raise "improperly formed alist" straight past the refusal below, which
;; exists to answer exactly this. NEVER: THE READER IS `answer-field`, IN THE
;; LIBRARY, and every program that reads this envelope calls that one:
;; three copies of the lookup is how two of them came to be fixed and the
;; third did not.
(define (envelope-field envelope name ok?) (answer-field envelope name ok?))

(define (deliver! envelope)
  (let ((out (envelope-field envelope 'stdout string?))
        (err (envelope-field envelope 'stderr string?))
        (code (envelope-field envelope 'exit exit-code?)))
    (if (and out err code)
        (begin
          (put-string (current-output-port) out)
          (unless (string=? err "") (put-string (current-error-port) err))
          (flush-output-port (current-output-port))
          (exit code))
        ;; NEVER: NOT AN ANSWER THAT CAME OUT BADLY -- a peer that is not the
        ;; daemon, or not this version of it. The request may already
        ;; have been carried out, so this is never reported as "nothing
        ;; happened".
        (refuse '(error transport-unknown (reason unreadable-answer))))))

(define (refuse answer)
  (put-string (current-output-port) (string-append (format "~s" answer) "\n"))
  (exit 75))

;; NEVER: THE REPLY IS LOOKED AT BEFORE THE READER IS HANDED IT. A datum label
;; makes a cycle -- `#0=(answer (x 1) . #0#)` reads perfectly well and then
;; every walk over it runs forever -- and a numeric literal builds its value
;; before anything can judge it. Both are refusals now, by the same rule the
;; daemon applies to a request.
(define (read-envelope bytes)
  (guard (e (#t #f))
    (let ((text (utf8->string bytes)))
      (and (readable-shape? text)
           (read (open-string-input-port text))))))

;; ---- one call -----------------------------------------------------------

;; NEVER: ONLY WHEN THE VERB READS IT, AND THE VERB'S OWN RULE SAYS SO. Read
;; whenever standard input merely happens not to be a terminal, this sits
;; forever on a pipe that nobody closes -- measured, and it would do it
;; for every verb, including the ones that read nothing.
;;
;; NOTE: THIS PARSES IN ORDER TO ASK, AND STILL SENDS THE ARGUMENTS
;; UNCHANGED. The parse is thrown away; it exists because "does this verb
;; take its input from stdin" is a question about the arguments -- `write
;; <id> -` does and `write <id> text` does not -- and asking the parser
;; that owns the tables is the only way to ask it without keeping a
;; second copy of them here.
(define (piped-input verb args)
  (let ((nodes (guard (e (#t #f)) (parse-arguments verb args))))
    ;; NOTE: AN EMPTY NODE LIST IS A PARSE, NOT A FAILURE. `batch` takes its
    ;; intents from standard input and nothing else, so its arguments are
    ;; legitimately empty -- and a test for "did we get a pair" rejected
    ;; exactly the verb this exists for.
    (and (list? nodes)
         (not (and (pair? nodes) (eq? 'error (car nodes))))
         (argument-wants-stdin? verb nodes)
         (guard (e (#t #f))
           (let-values (((out get) (open-string-output-port)))
             (let loop ()
               (let ((c (read-char (current-input-port))))
                 (if (eof-object? c) (get) (begin (put-char out c) (loop))))))))))

(define (send-once socket frame)
  (call! socket frame 30000))

(define (main argv)
  ;; THE TRANSLATION POINT (F100b point 4): the whole run -- the socket
  ;; derivation, the frame, the start of a daemon -- in this process's
  ;; mutation-record scope; a filesystem condition that leaves it is the
  ;; table's answer on stdout with exit 75 (`refuse`), where it was an
  ;; uncaught exception, exit 255.
  (with-mutation-record (lambda ()
  (guard (e ((classify-failure e (mutation-record)) => refuse))
  (let* ((scanned (scan argv))
         (verb (car scanned))
         (args (cadr scanned))
         (store (bound-store (caddr scanned)))
         (actor (bound-actor (cadddr scanned)))
         (given-socket (list-ref scanned 4))
         (wire? (list-ref scanned 5)))
    (when (not verb)
      (put-string (current-output-port) "(error no-verb)\n")
      (exit 2))
    ;; NEVER: ASKED BEFORE THE FRAME IS BUILT. A verb this program cannot
    ;; print is one no daemon would accept, and sending it anyway gets
    ;; `not-a-datum` from the reader's guard at the far end -- a different
    ;; answer from the one the same verb gets when it is run here.
    ;; NOTE: THE REFUSAL IS WORDED IN `(theourgia client)`, which this
    ;; program already imports: one place words it, every route asks it.
    (let ((spelling-error (verb-spelling-error (symbol->string verb))))
      (when spelling-error
        (put-string (current-output-port)
                    (string-append (format "~s" spelling-error) "\n"))
        (exit 1)))
    ;; NEVER: THE LOCAL ROUTE IS BY NAME, AND THE NAMES ARE THE THREE ABOVE.
    ;; `THEOURGIA_LOCAL` sends everything that way, which is a debugging
    ;; path and is documented as one.
    (when (or (local-by-name? verb) (env-value "THEOURGIA_LOCAL"))
      (run-server-here! argv))
    (let* ((socket (or given-socket (socket-path store)))
           (frame (request-frame store verb args
                                 (list (cons 'actor actor)
                                       (cons 'writer (bound-writer))
                                       ;; NEVER: WHAT WAS PIPED IN AND WHERE WE ARE.
                                       ;; The server is another process, in
                                       ;; another directory, with no access to
                                       ;; this one's standard input -- so a
                                       ;; `batch` had no intents and a
                                       ;; `write <id> -` stored the literal
                                       ;; "-" while answering `(ok (saved ...))`.
                                       ;; Both facts belong to the caller and
                                       ;; travel with the request.
                                       (cons 'stdin (piped-input verb args))
                                       (cons 'cwd (current-directory))
                                       (cons 'mode (if wire? 'wire 'human)))))
           (first (send-once socket frame)))
      (settle first
              ;; NEVER: NOBODY THERE IS A REASON TO START ONE; A LOST ANSWER IS
              ;; NOT. Nothing was read, so the request reached nobody and
              ;; doing it once now does it once. Once bytes have gone out,
              ;; the request MAY have been carried out.
              (lambda ()
                (let ((started (ensure-daemon! (server-argv store socket) store socket)))
                  (if (eq? started 'ready)
                      (settle (send-once socket frame) #f)
                      (refuse started)))))))))))

;; KEY: ONE CLASSIFICATION, WHICHEVER SEND PRODUCED THE OUTCOME. The second
;; send used to fold every non-answer into `transport-unknown`, so a
;; connect that failed after the daemon reported itself ready -- which
;; establishes that nothing was sent, exactly as it does on the first
;; send -- was reported as a request that may have been carried out. An
;; outcome means the same thing whichever attempt produced it.
;;
;; `start` is what to do about nobody listening, and #f is the second
;; time: a daemon that has just announced itself ready and is then not
;; there has still taken nothing from us, so that is `not-sent` too --
;; NEVER: and never another start, which is how one request becomes two.
(define (settle outcome start)
  (cond
    ((eq? 'answer (car outcome)) (deliver! (read-envelope (cadr outcome))))
    ;; NEVER: NOT SENT IS RELAYED AS ITSELF. The library already worked out
    ;; why and said so in a form a caller can read; wrapping it in
    ;; `transport-unknown` would replace a known outcome with an unknown
    ;; one.
    ((eq? 'not-sent (car outcome)) (refuse (cadr outcome)))
    ((eq? 'no-daemon (car outcome))
     (if start
         (start)
         (refuse (list 'error 'connect-failed
                       (list 'errno (cadr outcome))
                       '(carried-out no)))))
    (else
     (refuse (list 'error 'transport-unknown (list 'reason (cadr outcome)))))))

;; NOTE: THE SOCKET IS PASSED TO THE DAEMON THIS CLIENT STARTS, so a caller
;; who named one gets a daemon on that one rather than on the computed
;; path. And the LOG path is decided here too: the client has to read
;; that file afterwards to say why a start failed, so it is the one that
;; names it.
;;
;; NEVER: A STORE THAT STARTS WITH "-" GOES AS `--store <store>`. The daemon
;; refuses a token spelled like an option that it does not read (F94), and a
;; store is passed verbatim, so a store named `--x` given as a positional was
;; refused where the client meant a store; the value after `--store` is taken
;; whatever its spelling. Every other store keeps the positional, so its
;; daemon's command line is the one it always was (`serve <store> ...`),
;; which is what anything that finds a daemon by its command line matches.
(define (server-argv store socket)
  (append (list (scheme-binary) "--script" (beside-this-program "theourgiad.sc") "serve")
          (if (and (> (string-length store) 0) (char=? (string-ref store 0) #\-))
              (list "--store" store)
              (list store))
          (list "--socket" socket
                "--detach" "--log" (serve-log-path store))))

(main (cdr (command-line)))
